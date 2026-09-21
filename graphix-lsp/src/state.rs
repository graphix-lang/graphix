//! What the server knows: open documents, the workspace's project
//! graph, and the last successful check of every root being edited.
//!
//! A root is a project's root file, or an open file no project
//! contains. Checks are lazy: a change marks its roots dirty and
//! [`ServerState::flush`] checks them when the server is next idle, so
//! a burst of edits costs one check.

use crate::{
    diagnostics::{error_leaf_message, error_location},
    position::{PositionEncoding, char_col_to_position_in_text, position_to_char_col},
    text::extent,
    uri::{path_to_uri, uri_to_path},
    workspace::{WorkspaceModel, scan},
};
use ahash::{AHashMap, AHashSet};
use arcstr::ArcStr;
use graphix_compiler::{env::Env, expr::BufferOverrides, ide::Ide};
use lsp_types::{Diagnostic, DiagnosticSeverity, Position, Range, Uri};
use std::{
    path::{Path, PathBuf},
    sync::Arc,
};

pub struct Document {
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

/// Diagnostics to publish, per file; an empty list clears the file.
pub type Diagnostics = Vec<(Uri, Vec<Diagnostic>)>;

pub struct ServerState {
    pub(crate) base_env: Env,
    pub(crate) documents: AHashMap<Uri, Document>,
    backend: Arc<dyn LspBackend>,
    workspace_roots: Vec<PathBuf>,
    pub(crate) workspace: WorkspaceModel,
    /// The last successful check of each root. A failed check leaves
    /// the previous one standing: queries over a buffer that does not
    /// compile answer from it.
    checked: AHashMap<PathBuf, Checked>,
    /// The files the last check of each root left diagnostics on.
    diagnosed: AHashMap<PathBuf, AHashSet<Uri>>,
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
            workspace: scan(&workspace_roots),
            workspace_roots,
            checked: AHashMap::default(),
            diagnosed: AHashMap::default(),
            dirty: AHashSet::default(),
            last_active: None,
            snippet_support,
            position_encoding,
        }
    }

    /// The roots a file is checked under: every project containing it,
    /// else the file itself.
    fn roots_of(&self, path: &Path) -> Vec<PathBuf> {
        match self.workspace.file_to_projects.get(path) {
            Some(idxs) if !idxs.is_empty() => {
                idxs.iter().map(|i| self.workspace.projects[*i].root.clone()).collect()
            }
            Some(_) | None => vec![path.to_path_buf()],
        }
    }

    /// True when an open document is checked under `root`.
    fn is_edited(&self, root: &Path) -> bool {
        self.documents.keys().filter_map(uri_to_path).any(|p| {
            p == root
                || self
                    .workspace
                    .projects
                    .iter()
                    .any(|proj| proj.root == root && proj.files.contains(&p))
        })
    }

    pub fn set_document(&mut self, uri: Uri, text: String, version: i32) {
        self.last_active = Some(uri.clone());
        if let Some(path) = uri_to_path(&uri) {
            let buffer = ArcStr::from(text.as_str());
            self.backend.buffer_overrides().lock().insert(path.clone(), buffer);
            self.dirty.extend(self.roots_of(&path));
        }
        self.documents.insert(uri, Document { version, text });
    }

    /// Stop tracking a document; a root nothing edits any more is
    /// forgotten and its diagnostics cleared.
    pub fn close_document(&mut self, uri: &Uri) -> Diagnostics {
        self.documents.remove(uri);
        let Some(path) = uri_to_path(uri) else { return vec![] };
        self.backend.buffer_overrides().lock().remove(&path);
        let mut cleared = vec![];
        for root in self.roots_of(&path) {
            if self.is_edited(&root) {
                self.dirty.insert(root);
            } else {
                self.dirty.remove(&root);
                self.checked.remove(&root);
                let files = self.diagnosed.remove(&root).unwrap_or_default();
                cleared.extend(files.into_iter().map(|uri| (uri, vec![])));
            }
        }
        cleared
    }

    /// Disk changed: the project graph may have, and every edited root
    /// may read the file.
    pub fn saved(&mut self) {
        self.workspace = scan(&self.workspace_roots);
        let open: Vec<PathBuf> = self.documents.keys().filter_map(uri_to_path).collect();
        for path in open {
            self.dirty.extend(self.roots_of(&path));
        }
    }

    /// Check every dirty root.
    pub fn flush(&mut self) -> Diagnostics {
        let mut roots: Vec<PathBuf> = self.dirty.drain().collect();
        roots.sort();
        roots.iter().flat_map(|root| self.check(root)).collect()
    }

    fn check(&mut self, root: &Path) -> Diagnostics {
        let package = self
            .workspace
            .projects
            .iter()
            .find(|p| p.root == root)
            .and_then(|p| p.package_scope.clone());
        let mut out: Diagnostics = vec![];
        match self.backend.typecheck_project(root, package) {
            Ok(checked) => {
                self.checked.insert(root.to_path_buf(), checked);
            }
            Err(e) => out.push(self.diagnostic(&e, root)),
        }
        let now: AHashSet<Uri> = out.iter().map(|(uri, _)| uri.clone()).collect();
        let before = self.diagnosed.insert(root.to_path_buf(), now).unwrap_or_default();
        let recovered = before.into_iter().filter(|u| out.iter().all(|(o, _)| o != u));
        let recovered: Diagnostics = recovered.map(|uri| (uri, vec![])).collect();
        out.extend(recovered);
        out
    }

    /// The diagnostic for a failed check, on the file the error names,
    /// else on the root.
    fn diagnostic(&self, err: &anyhow::Error, root: &Path) -> (Uri, Vec<Diagnostic>) {
        let loc = error_location(err);
        let path = loc.file.unwrap_or_else(|| root.to_path_buf());
        let uri = path_to_uri(&path)
            .or_else(|| path_to_uri(root))
            .expect("a checked root is an absolute path");
        let at = loc.position.unwrap_or_default();
        let range = |text: &str| Range {
            start: self.encode(text, at),
            end: self.encode(text, extent(text, at)),
        };
        let range = match self.documents.get(&uri) {
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
        (uri, vec![diag])
    }

    /// The check that answers queries about `path`.
    pub(crate) fn checked_for(&self, path: &Path) -> Option<&Checked> {
        self.roots_of(path).iter().find_map(|root| self.checked.get(root))
    }

    /// An LSP position as (line, char column); unchanged when the
    /// document or the line is unknown.
    pub(crate) fn decode(&self, uri: &Uri, position: Position) -> Position {
        let line = self
            .documents
            .get(uri)
            .and_then(|doc| doc.text.lines().nth(position.line as usize));
        match line {
            None => position,
            Some(line) => Position {
                line: position.line,
                character: position_to_char_col(line, position, self.position_encoding)
                    as u32,
            },
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
