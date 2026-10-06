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
    /// The warnings of each root's last successful check, by file.
    warnings: AHashMap<PathBuf, AHashMap<Uri, Vec<Diagnostic>>>,
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
            workspace: scan(&workspace_roots, &WorkspaceModel::default()),
            workspace_roots,
            checked: AHashMap::default(),
            warnings: AHashMap::default(),
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
            // CR claude for eric: [bug] A `.gxi` that no project contains becomes its
            // own root here, and `check` hands it to `RootFile::load`, which parses the
            // interface as a program. A valid interface then gets a parse error ("`///`
            // is a doc comment, legal only in a .gxi interface file"), and hover and
            // definition in it return nothing. Every interface is outside every project
            // when the client names no workspace (single-file mode), and a new
            // interface is outside until a save rescans the disk; after that save the
            // error never clears, because nothing retires the old root.
            // `detect_package_scope` (workspace.rs:229) tests only the `mod` stem, so a
            // package's `mod.gxi` fails the same way. A `.gxi` should be checked under
            // the roots of its `.gx` sibling (the sibling itself when it stands alone)
            // and never be a root of its own. probe:
            // design/review-2026-10-05/repro/lsp-05.py (lsp-05)
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
                self.warnings.remove(&root);
                let files = self.diagnosed.remove(&root).unwrap_or_default();
                cleared.extend(files.into_iter().map(|uri| (uri, vec![])));
            }
        }
        cleared
    }

    /// Disk changed: the project graph may have, and every edited root
    /// may read the file.
    pub fn saved(&mut self) {
        // CR claude for eric: [bug] This rescan can turn a root into a module, for
        // example when a save adds `mod helper;` to main.gx. Nothing then drops the old
        // root's `checked`, `warnings` and `diagnosed` entries, because
        // `close_document` only visits the roots `roots_of` names now. So the
        // diagnostics its last check published stay on its files for the life of the
        // server, through edits and after every file is closed, and its Env and Ide
        // stay in memory. After a rescan and on close, retire every root that is no
        // longer a root of an open document: publish empty lists for its diagnosed
        // files (`saved` has to return them) and drop its entries. Keeping all per-root
        // state in one map would make that a single remove. probe:
        // design/review-2026-10-05/repro/lsp-04.py (helper.gx keeps "`super` goes above
        // the package root" after main.gx gains `mod helper;` and checks clean).
        // (lsp-04)
        self.workspace = scan(&self.workspace_roots, &self.workspace);
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

    /// Check `root`. What stands on its files afterwards is the warnings
    /// of its last successful check and, when this one failed, the
    /// error: a buffer that stops compiling keeps its warnings.
    fn check(&mut self, root: &Path) -> Diagnostics {
        let package = detect_package_scope(root);
        let error = match self.backend.typecheck_project(root, package) {
            Ok(checked) => {
                self.warnings.insert(root.to_path_buf(), self.warned(&checked));
                self.checked.insert(root.to_path_buf(), checked);
                None
            }
            Err(e) => Some(self.diagnostic(&e, root)),
        };
        // CR claude for eric: [bug] A publish replaces the client's whole list for a
        // file, but `now` holds only this root's diagnostics. A file two projects share
        // (gui/icon.gx is in 51) therefore shows whichever root published last.
        // `cleared`, here and in close_document (line 155), empties a file another root
        // still fails on. Observed: tool_b's warning on util.gx replaces tool_a's
        // error. A parse error typed into tool_a.gx, or closing tool_b.gx, leaves
        // util.gx clean while the other project still does not compile. Publish, per
        // file, the union over every root that covers it, and clear a file only when no
        // root has anything on it. probe: design/review-2026-10-05/repro/lsp-03.py
        // (`python3 design/review-2026-10-05/repro/lsp-03.py <graphix>`) (lsp-03)
        let mut now = self.warnings.get(root).cloned().unwrap_or_default();
        if let Some((uri, error)) = error {
            now.entry(uri).or_default().insert(0, error);
        }
        let files = now.keys().cloned().collect();
        let before = self.diagnosed.insert(root.to_path_buf(), files).unwrap_or_default();
        let cleared = before.into_iter().filter(|uri| !now.contains_key(uri));
        let cleared: Diagnostics = cleared.map(|uri| (uri, vec![])).collect();
        now.into_iter().chain(cleared).collect()
    }

    /// The warnings of a check, by file.
    fn warned(&self, checked: &Checked) -> AHashMap<Uri, Vec<Diagnostic>> {
        let mut out: AHashMap<Uri, Vec<Diagnostic>> = AHashMap::default();
        for w in checked.ide.warnings.iter() {
            let Source::File(path) = &w.ori.source else { continue };
            let Some(uri) = path_to_uri(path) else { continue };
            let (start, end) = (zero_based(w.pos), zero_based(w.end));
            let end = if end > start { end } else { extent(&w.ori.text, start) };
            out.entry(uri).or_default().push(Diagnostic {
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
    /// else on the root.
    fn diagnostic(&self, err: &anyhow::Error, root: &Path) -> (Uri, Diagnostic) {
        let loc = error_location(err);
        let path = loc.file.unwrap_or_else(|| root.to_path_buf());
        // CR claude for eric: [bug] path_to_uri also returns None for absolute paths.
        // So this expect kills the server (main-thread panic, exit 101) on the first
        // failed check whose error file or root path holds [ ] ^ | \ or is not UTF-8.
        // PATH_ENCODE (uri.rs:13) is the WHATWG path set, but lsp_types 0.97's Uri is
        // fluent-uri, which refuses those characters in a path, and a non-UTF-8 path
        // has no URI at all. The same None makes warned (line 207) drop such a file's
        // warnings without a word. probe: design/review-2026-10-05/repro/x-panics-03.py
        // (a type error in <ws>/[x]/main.gx; a^b, a|b, a\b and a \xff.gx root the
        // same). (x-panics-03)
        let uri = path_to_uri(&path)
            .or_else(|| path_to_uri(root))
            .expect("a checked root is an absolute path");
        let at = loc.position.unwrap_or_default();
        let range = |text: &str| Range {
            start: self.encode(text, at),
            end: self.encode(text, loc.end.unwrap_or_else(|| extent(text, at))),
        };
        // CR claude for eric: [bug] `documents` is keyed by the URI string the client
        // sent, but this `uri` is rebuilt by `path_to_uri`. That function leaves `( ) !
        // $ & ' * + , ; = : @` unencoded, while VS Code (ide/editors/vscode)
        // percent-encodes them. For a file under e.g. `proj (copy)/` the lookup misses,
        // so the error range is encoded against the file on disk rather than the
        // unsaved buffer the check read. An error on a line the disk lacks lands at
        // (3,0)-(3,0), a parse error's underline is measured on the disk line, and
        // UTF-16 columns are counted over the disk's characters. The same miss drops
        // the version in `publish` (server.rs:141), makes `workspace_symbols` parse the
        // disk (symbols.rs:131), and puts every diagnostic under a URI the client never
        // sent; encode against the erring expression's `ori.text` as `warned` does, key
        // documents by path and publish under the client's URI. probe:
        // design/review-2026-10-05/repro/lsp-06.py (lsp-06)
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
        (uri, diag)
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
