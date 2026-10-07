//! What the server knows: open documents, the workspace's project
//! graph, and the last successful check of every root being edited.
//!
//! A root is a project's root file, or an open file no project
//! contains. Checks are lazy: a change marks its roots dirty and
//! [`ServerState::flush`] checks them when the server is next idle, so
//! a burst of edits costs one check.

use crate::{
    diagnostics::{error_leaf_message, error_location},
    position::{
        PositionEncoding, char_col_to_position_in_text, position_to_char_col_in_text,
    },
    text::{extent, zero_based},
    uri::{path_to_uri, uri_to_path},
    workspace::{WorkspaceModel, detect_package_scope, scan},
};
use ahash::{AHashMap, AHashSet};
use arcstr::ArcStr;
use graphix_compiler::{
    env::Env,
    expr::{BufferOverrides, Source},
    ide::Ide,
};
use lsp_types::{Diagnostic, DiagnosticSeverity, Position, Range, Uri};
use std::{
    path::{Path, PathBuf},
    sync::Arc,
};

pub struct Document {
    /// The URI the client names the document by: its diagnostics publish
    /// under it.
    pub uri: Uri,
    pub version: i32,
    pub text: String,
}

/// A successful check of one root: the environment it left and every
/// IDE side-channel it filled. Bind ids are minted per check, so they
/// mean something only within one `Checked`.
pub struct Checked {
    pub env: Env,
    pub ide: Ide,
}

/// Owns the graphix runtime and type-checks for the server. The server
/// loop is synchronous; the backend drives any async runtime it needs.
pub trait LspBackend: Send + Sync + 'static {
    /// The environment with the stdlib loaded and nothing else.
    fn env(&self) -> Env;
    /// Type-check the project rooted at `root`, every file reachable via
    /// `mod foo;` included, open buffers first. `package` compiles the
    /// root as the body of `mod <package>`.
    fn typecheck_project(
        &self,
        root: &Path,
        package: Option<ArcStr>,
    ) -> anyhow::Result<Checked>;
    /// The open-buffer map the backend's resolvers read, which the
    /// server keeps equal to its open documents.
    fn buffer_overrides(&self) -> BufferOverrides;
}

/// One file's diagnostics to publish; an empty list clears the file.
pub struct Publish {
    pub uri: Uri,
    pub version: Option<i32>,
    pub diagnostics: Vec<Diagnostic>,
}

pub type Diagnostics = Vec<Publish>;

/// What one root's checks left.
#[derive(Default)]
struct Root {
    /// The last successful check. A failed check leaves it standing:
    /// queries over a buffer that does not compile answer from it.
    checked: Option<Checked>,
    /// The warnings of the last successful check, by file.
    warnings: AHashMap<PathBuf, Vec<Diagnostic>>,
    /// The last check's error, when it failed.
    error: Option<(PathBuf, Diagnostic)>,
}

impl Root {
    fn files(&self) -> impl Iterator<Item = &PathBuf> {
        self.warnings.keys().chain(self.error.iter().map(|(f, _)| f))
    }
}

pub struct ServerState {
    pub(crate) base_env: Env,
    /// The open documents, by path.
    pub(crate) documents: AHashMap<PathBuf, Document>,
    backend: Arc<dyn LspBackend>,
    workspace_roots: Vec<PathBuf>,
    pub(crate) workspace: WorkspaceModel,
    /// Every root an open document is checked under.
    roots: AHashMap<PathBuf, Root>,
    /// The files whose published list is not empty.
    published: AHashSet<PathBuf>,
    dirty: AHashSet<PathBuf>,
    /// `workspace/symbol` searches this document's project.
    pub(crate) last_active: Option<Uri>,
    pub(crate) snippet_support: bool,
    pub(crate) position_encoding: PositionEncoding,
}

impl ServerState {
    pub fn new(
        backend: Arc<dyn LspBackend>,
        workspace_roots: Vec<PathBuf>,
        snippet_support: bool,
        position_encoding: PositionEncoding,
    ) -> Self {
        Self {
            base_env: backend.env(),
            documents: AHashMap::default(),
            backend,
            workspace: scan(&workspace_roots, &WorkspaceModel::default()),
            workspace_roots,
            roots: AHashMap::default(),
            published: AHashSet::default(),
            dirty: AHashSet::default(),
            last_active: None,
            snippet_support,
            position_encoding,
        }
    }

    /// The open document `uri` names.
    pub(crate) fn document(&self, uri: &Uri) -> Option<&Document> {
        self.documents.get(&uri_to_path(uri)?)
    }

    /// The roots a file is checked under: every project containing it,
    /// else the file itself. An interface is checked with its
    /// implementation, and is never a root of its own.
    fn roots_of(&self, path: &Path) -> Vec<PathBuf> {
        if path.extension().is_some_and(|e| e == "gxi") {
            let gx = path.with_extension("gx");
            let known = self.documents.contains_key(&gx)
                || self.workspace.file_to_projects.contains_key(&gx)
                || gx.exists();
            return if known { self.roots_of(&gx) } else { vec![] };
        }
        match self.workspace.file_to_projects.get(path) {
            Some(idxs) if !idxs.is_empty() => {
                idxs.iter().map(|i| self.workspace.projects[*i].root.clone()).collect()
            }
            Some(_) | None => vec![path.to_path_buf()],
        }
    }

    /// Every root an open document is checked under.
    fn live(&self) -> AHashSet<PathBuf> {
        self.documents.keys().flat_map(|p| self.roots_of(p)).collect()
    }

    pub fn set_document(&mut self, uri: Uri, text: String, version: i32) {
        self.last_active = Some(uri.clone());
        let Some(path) = uri_to_path(&uri) else { return };
        let buffer = ArcStr::from(text.as_str());
        self.backend.buffer_overrides().lock().insert(path.clone(), buffer);
        self.dirty.extend(self.roots_of(&path));
        self.documents.insert(path, Document { uri, version, text });
    }

    /// Stop tracking a document: the roots it kept are checked again or,
    /// when nothing open needs them, retired.
    pub fn close_document(&mut self, uri: &Uri) -> Diagnostics {
        let Some(path) = uri_to_path(uri) else { return vec![] };
        self.documents.remove(&path);
        self.backend.buffer_overrides().lock().remove(&path);
        let live = self.live();
        for root in self.roots_of(&path) {
            if live.contains(&root) {
                self.dirty.insert(root);
            }
        }
        self.retire()
    }

    /// Disk changed: the project graph may have, and every edited root
    /// may read the file. A root that stopped being one is retired.
    pub fn saved(&mut self) -> Diagnostics {
        self.workspace = scan(&self.workspace_roots, &self.workspace);
        self.dirty.extend(self.live());
        self.retire()
    }

    /// Forget every root no open document is checked under, and publish
    /// what that leaves on its files.
    fn retire(&mut self) -> Diagnostics {
        let live = self.live();
        self.dirty.retain(|r| live.contains(r));
        let gone: Vec<PathBuf> =
            self.roots.keys().filter(|r| !live.contains(*r)).cloned().collect();
        let mut files = AHashSet::default();
        for root in gone {
            if let Some(r) = self.roots.remove(&root) {
                files.extend(r.files().cloned());
            }
        }
        self.publish_files(files)
    }

    /// Check every dirty root.
    pub fn flush(&mut self) -> Diagnostics {
        let mut roots: Vec<PathBuf> = self.dirty.drain().collect();
        roots.sort();
        let mut files = AHashSet::default();
        for root in roots {
            files.extend(self.check(&root));
        }
        self.publish_files(files)
    }

    /// Check `root`, and the files its diagnostics change on. What stands
    /// on its files afterwards is the warnings of its last successful
    /// check and, when this one failed, the error: a buffer that stops
    /// compiling keeps its warnings.
    fn check(&mut self, root: &Path) -> AHashSet<PathBuf> {
        let package = detect_package_scope(root);
        let result = self.backend.typecheck_project(root, package);
        let mut files: AHashSet<PathBuf> = AHashSet::default();
        if let Some(r) = self.roots.get(root) {
            files.extend(r.files().cloned());
        }
        let error = result.as_ref().err().map(|e| self.diagnostic(e, root));
        let warnings = result.as_ref().ok().map(|c| self.warned(c));
        let r = self.roots.entry(root.to_path_buf()).or_default();
        if let (Ok(checked), Some(warnings)) = (result, warnings) {
            r.checked = Some(checked);
            r.warnings = warnings;
        }
        r.error = error;
        files.extend(r.files().cloned());
        files
    }

    /// Publish each of `files`: every root's diagnostics on it, errors
    /// first; a file nothing has anything on is cleared once.
    fn publish_files(&mut self, files: AHashSet<PathBuf>) -> Diagnostics {
        let mut roots: Vec<&PathBuf> = self.roots.keys().collect();
        roots.sort();
        let mut out = Vec::new();
        let mut files: Vec<PathBuf> = files.into_iter().collect();
        files.sort();
        for file in files {
            let mut diagnostics = Vec::new();
            for root in &roots {
                let r = &self.roots[*root];
                let error = r.error.iter().filter(|(f, _)| *f == file).map(|(_, e)| e);
                let warnings = r.warnings.get(&file).into_iter().flatten();
                // roots that share the file find the same things on it
                for d in error.chain(warnings) {
                    if !diagnostics.contains(d) {
                        diagnostics.push(d.clone());
                    }
                }
            }
            if diagnostics.is_empty() && !self.published.contains(&file) {
                continue;
            }
            let doc = self.documents.get(&file);
            let Some(uri) = doc.map(|d| d.uri.clone()).or_else(|| path_to_uri(&file))
            else {
                log::info!(
                    "no URI for {}: its diagnostics are not published",
                    file.display()
                );
                continue;
            };
            match diagnostics.is_empty() {
                true => self.published.remove(&file),
                false => self.published.insert(file.clone()),
            };
            out.push(Publish { uri, version: doc.map(|d| d.version), diagnostics });
        }
        out
    }

    /// The warnings of a check, by file.
    fn warned(&self, checked: &Checked) -> AHashMap<PathBuf, Vec<Diagnostic>> {
        let mut out: AHashMap<PathBuf, Vec<Diagnostic>> = AHashMap::default();
        for w in checked.ide.warnings.iter() {
            let Source::File(path) = &w.ori.source else { continue };
            let (start, end) = (zero_based(w.pos), zero_based(w.end));
            let end = if end > start { end } else { extent(&w.ori.text, start) };
            out.entry(path.clone()).or_default().push(Diagnostic {
                range: Range {
                    start: self.encode(&w.ori.text, start),
                    end: self.encode(&w.ori.text, end),
                },
                severity: Some(DiagnosticSeverity::WARNING),
                source: Some("graphix".to_string()),
                message: w.message.to_string(),
                ..Default::default()
            });
        }
        out
    }

    /// The diagnostic for a failed check, on the file the error names,
    /// else on the root, its range measured on the text the check read
    /// (the open buffer, else the disk).
    fn diagnostic(&self, err: &anyhow::Error, root: &Path) -> (PathBuf, Diagnostic) {
        let loc = error_location(err);
        let path = loc.file.unwrap_or_else(|| root.to_path_buf());
        let at = loc.position.unwrap_or_default();
        let range = |text: &str| Range {
            start: self.encode(text, at),
            end: self.encode(text, loc.end.unwrap_or_else(|| extent(text, at))),
        };
        let range = match self.documents.get(&path) {
            Some(doc) => range(&doc.text),
            None => match std::fs::read_to_string(&path) {
                Ok(text) => range(&text),
                Err(_) => Range { start: at, end: at },
            },
        };
        let diag = Diagnostic {
            range,
            severity: Some(DiagnosticSeverity::ERROR),
            source: Some("graphix".to_string()),
            message: error_leaf_message(err),
            ..Default::default()
        };
        (path, diag)
    }

    /// The check that answers queries about `path`.
    pub(crate) fn checked_for(&self, path: &Path) -> Option<&Checked> {
        self.roots_of(path).iter().find_map(|root| self.roots.get(root)?.checked.as_ref())
    }

    /// An LSP position as (line, char column); unchanged when the
    /// document or the line is unknown.
    pub(crate) fn decode(&self, uri: &Uri, position: Position) -> Position {
        let col = self.document(uri).and_then(|doc| {
            position_to_char_col_in_text(&doc.text, position, self.position_encoding)
        });
        match col {
            None => position,
            Some(c) => Position { line: position.line, character: c as u32 },
        }
    }

    /// A (line, char column) in `text` as an LSP position.
    pub(crate) fn encode(&self, text: &str, at: Position) -> Position {
        char_col_to_position_in_text(
            text,
            at.line,
            at.character as usize,
            self.position_encoding,
        )
    }
}
