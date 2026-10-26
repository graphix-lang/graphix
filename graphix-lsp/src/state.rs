//! What the server knows: open documents, the workspace's project
//! graph, and the last successful check of every root being edited.
//!
//! A root is a project's root file, or an open file no project
//! contains. Checks are lazy: a change marks its roots dirty and
//! [`ServerState::flush`] checks them when the server is next idle, so
//! a burst of edits costs one check.

use crate::{
    diagnostics::{error_leaf_message, error_location},
    position::{LineIndex, LineIndexes, PositionEncoding},
    text::{extent, zero_based},
    uri::{path_to_uri, uri_to_path},
    workspace::{WorkspaceModel, detect_package_scope, scan},
};
use ahash::{AHashMap, AHashSet};
use arcstr::ArcStr;
use graphix_compiler::{
    env::Env,
    expr::{BufferOverrides, Origin, Source, parser},
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
    pub text: ArcStr,
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
    /// The files whose published list is not empty, and the URI it went
    /// out under, which its clear must use.
    published: AHashMap<PathBuf, Uri>,
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
            published: AHashMap::default(),
            dirty: AHashSet::default(),
            last_active: None,
            snippet_support,
            position_encoding,
        }
    }

    /// The URI the client knows `path` by: its document's, else the one
    /// its diagnostics went out under, else its own spelling.
    pub(crate) fn uri_of(&self, path: &Path) -> Option<Uri> {
        match self.documents.get(path) {
            Some(doc) => Some(doc.uri.clone()),
            None => self.published.get(path).cloned().or_else(|| path_to_uri(path)),
        }
    }

    /// The open document `uri` names.
    pub(crate) fn document(&self, uri: &Uri) -> Option<&Document> {
        self.documents.get(&uri_to_path(uri)?)
    }

    /// The roots a file is checked under: every project containing it,
    /// else the file itself. An interface is checked with its
    /// implementation, and is a root of its own only before it has one.
    fn roots_of(&self, path: &Path) -> Vec<PathBuf> {
        if path.extension().is_some_and(|e| e == "gxi") {
            let gx = path.with_extension("gx");
            let known = self.documents.contains_key(&gx)
                || self.workspace.file_to_projects.contains_key(&gx)
                || gx.exists();
            return if known { self.roots_of(&gx) } else { vec![path.to_path_buf()] };
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

    /// Open or change a document: its roots are checked again, and the
    /// roots it moved another file off are retired.
    pub fn set_document(&mut self, uri: Uri, text: String, version: i32) -> Diagnostics {
        self.last_active = Some(uri.clone());
        let Some(path) = uri_to_path(&uri) else { return vec![] };
        let text = ArcStr::from(text);
        self.backend.buffer_overrides().lock().insert(path.clone(), text.clone());
        self.dirty.extend(self.roots_of(&path));
        self.documents.insert(path, Document { uri, version, text });
        self.retire()
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

    /// Disk changed: the project graph may have, and a root reads what
    /// changed unless the file is open (a check reads its buffer). A root
    /// whose project gained or lost a file, or holds a file changed on
    /// disk, is checked again; one in no project reads what the scan does
    /// not see, so it always is. A root that stopped being one is retired.
    pub fn saved(&mut self) -> Diagnostics {
        let workspace = scan(&self.workspace_roots, &self.workspace);
        let old = std::mem::replace(&mut self.workspace, workspace);
        let new = &self.workspace;
        let changed = |p: &PathBuf| {
            !self.documents.contains_key(p)
                && match (old.files.get(p), new.files.get(p)) {
                    (Some(a), Some(b)) => a.modified != b.modified,
                    (a, b) => a.is_some() != b.is_some(),
                }
        };
        fn files<'w>(
            w: &'w WorkspaceModel,
            root: &Path,
        ) -> Option<&'w AHashSet<PathBuf>> {
            w.projects.iter().find(|p| p.root == root).map(|p| &p.files)
        }
        let stale: Vec<PathBuf> = self
            .live()
            .into_iter()
            .filter(|root| {
                let (before, after) = (files(&old, root), files(new, root));
                !self.roots.contains_key(root)
                    || after.is_none()
                    || before != after
                    || after.into_iter().flatten().any(&changed)
            })
            .collect();
        self.dirty.extend(stale);
        self.retire()
    }

    /// Forget every root no open document is checked under, and publish
    /// what that leaves on its files; a root that is live and was never
    /// checked is checked.
    fn retire(&mut self) -> Diagnostics {
        let live = self.live();
        self.dirty.retain(|r| live.contains(r));
        self.dirty.extend(live.iter().filter(|r| !self.roots.contains_key(*r)).cloned());
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
        let result = match root.extension().is_some_and(|e| e == "gxi") {
            // an interface with no implementation yet: only its syntax
            true => self.parse_interface(root).map(|()| None),
            false => {
                self.backend.typecheck_project(root, detect_package_scope(root)).map(Some)
            }
        };
        let mut files: AHashSet<PathBuf> = AHashSet::default();
        if let Some(r) = self.roots.get(root) {
            files.extend(r.files().cloned());
        }
        let error = result.as_ref().err().map(|e| self.diagnostic(e, root));
        let warnings = result
            .as_ref()
            .ok()
            .map(|c| c.as_ref().map(|c| self.warned(c)).unwrap_or_default());
        let r = self.roots.entry(root.to_path_buf()).or_default();
        if let (Ok(checked), Some(warnings)) = (result, warnings) {
            r.checked = checked;
            r.warnings = warnings;
        }
        r.error = error;
        files.extend(r.files().cloned());
        files
    }

    /// Parse the interface `path`, its buffer when it is open.
    fn parse_interface(&self, path: &Path) -> anyhow::Result<()> {
        let text = match self.documents.get(path) {
            Some(doc) => doc.text.clone(),
            None => ArcStr::from(std::fs::read_to_string(path)?),
        };
        let ori = Origin { parent: None, source: Source::File(path.to_path_buf()), text };
        parser::parse_sig(ori).map(|_| ())
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
            if diagnostics.is_empty() && !self.published.contains_key(&file) {
                continue;
            }
            let Some(uri) = self.uri_of(&file) else {
                log::info!(
                    "no URI for {}: its diagnostics are not published",
                    file.display()
                );
                continue;
            };
            match diagnostics.is_empty() {
                true => self.published.remove(&file),
                false => self.published.insert(file.clone(), uri.clone()),
            };
            let version = self.documents.get(&file).map(|d| d.version);
            out.push(Publish { uri, version, diagnostics });
        }
        out
    }

    /// The warnings of a check, by file.
    fn warned(&self, checked: &Checked) -> AHashMap<PathBuf, Vec<Diagnostic>> {
        let mut out: AHashMap<PathBuf, Vec<Diagnostic>> = AHashMap::default();
        let mut texts = LineIndexes::default();
        for w in checked.ide.warnings.iter() {
            let Source::File(path) = &w.ori.source else { continue };
            let (start, end) = (zero_based(w.pos), zero_based(w.end));
            let end = if end > start { end } else { extent(&w.ori.text, start) };
            let lines = texts.of(&w.ori.text);
            out.entry(path.clone()).or_default().push(Diagnostic {
                range: Range {
                    start: self.encode(lines, start),
                    end: self.encode(lines, end),
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
        let range = |text: &ArcStr| {
            let lines = LineIndex::new(text);
            Range {
                start: self.encode(&lines, at),
                end: self.encode(&lines, loc.end.unwrap_or_else(|| extent(text, at))),
            }
        };
        let range = match self.documents.get(&path) {
            Some(doc) => range(&doc.text),
            None => match std::fs::read_to_string(&path) {
                Ok(text) => range(&ArcStr::from(text)),
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

    /// The check that answers queries about `path`: a root's whose last
    /// check succeeded, else the last successful check of any.
    // XCR claude for claude: [bug/medium] This takes the first root that has any
    // Checked. That can be a root whose last check failed and so predates the edit,
    // while another root of the same file has just checked the current buffer. Example:
    // with util.gx shared by two tools and only one tool broken by an edit, hover on
    // util's `f` shows its old type, and a newly added `g` has no hover or references.
    // Prefer a root with `error.is_none()`, and fall back to a stale check only when no
    // current one exists. probe:
    // design/review-2026-10-26/repro/x-lsp-probe-stale-shared-root.py (x-lsp-probe-02)
    // 2026-10-26 claude: checked_for takes the check of a root whose last check
    // succeeded, and a stale one only when no root of the file has a current check.
    // Pin: graphix-shell tests/lsp a_shared_file_answers_from_a_current_check (the
    // probe's two tools, one broken by the edit).
    pub(crate) fn checked_for(&self, path: &Path) -> Option<&Checked> {
        let roots = self.roots_of(path);
        let roots = || roots.iter().filter_map(|root| self.roots.get(root));
        let current =
            roots().find(|r| r.error.is_none()).and_then(|r| r.checked.as_ref());
        current.or_else(|| roots().find_map(|r| r.checked.as_ref()))
    }

    /// An LSP position as (line, char column); unchanged when the
    /// document or the line is unknown.
    pub(crate) fn decode(&self, uri: &Uri, position: Position) -> Position {
        let col = self.document(uri).and_then(|doc| {
            LineIndex::new(&doc.text).char_col(position, self.position_encoding)
        });
        match col {
            None => position,
            Some(c) => Position { line: position.line, character: c as u32 },
        }
    }

    /// A (line, char column) in the text `lines` indexes as an LSP
    /// position.
    // XCR claude for claude: [perf/medium] Every call rescans `text` from the top
    // (`lines(text).nth(line)`), and document_symbols, workspace_symbols, references
    // and warned call it per item, so those requests are O(items x file size).
    // document_symbols' `typ` also scans all of ide.binds per symbol. On 2000 vs 4000
    // one-line lets, documentSymbol takes 0.22 vs 0.84 s and references 0.09 vs 0.37 s,
    // more than the check itself (0.25 s). Fix: build each text's line-start offsets
    // once per request (per ArcStr for references), have the position conversions index
    // into them, and key this file's binds by (pos, name) once for `typ`. probe:
    // design/review-2026-10-26/repro/lsp-quadratic-positions.py (lsp-02)
    // 2026-10-26 claude: Position conversions go through position::LineIndex (each
    // line's range found once per text); a request builds one per text (documentSymbol,
    // workspace/symbol per file, a Query's LineIndexes per Origin text, warned per
    // warning text), and documentSymbol keys the file's binds by (pos, name) once. The
    // probe now gives documentSymbol 0.06/0.09 s, workspace/symbol 0.05/0.10 s,
    // references 0.01/0.02 s at N=2000/4000 (was 0.36/1.47, 0.19/0.79, 0.15/0.68 on
    // this host). No timing pin; LineIndex's conversions are pinned by
    // position::tests::lines_break_as_lsp_does.
    pub(crate) fn encode(&self, lines: &LineIndex, at: Position) -> Position {
        lines.position(at.line, at.character as usize, self.position_encoding)
    }
}
