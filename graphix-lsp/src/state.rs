use crate::diagnostics::error_to_diagnostics;
use arcstr::ArcStr;
use graphix_compiler::{
    env::Env,
    expr::{ModPath, Source},
    typ::Type,
    ReferenceSite, SourcePosition,
};
use lsp_types::Uri;
use std::{collections::HashMap, path::{Path, PathBuf}, str::FromStr, sync::Arc};

/// An open document tracked by the language server.
pub struct Document {
    pub version: i32,
    pub text: String,
    /// The compiler environment as it would be after type-checking
    /// this document. `Some` after a successful check, `None` after a
    /// failed one (so the env doesn't drift away from the user's
    /// current source).
    pub env: Option<Env>,
    /// Resolved name reference sites observed during the most
    /// recent successful check, used to answer
    /// `textDocument/references` and prepareRename.
    pub references: Vec<ReferenceSite>,
}

impl Document {
    pub fn new(text: String, version: i32) -> Self {
        Self { version, text, env: None, references: Vec::new() }
    }
}

/// Result of a backend typecheck. Mirrors `graphix_rt::CheckResult`
/// but doesn't depend on graphix-rt directly.
pub struct TypecheckResult {
    pub env: Env,
    pub references: Vec<ReferenceSite>,
}

/// Backend that owns the graphix runtime / environment and can
/// type-check documents on behalf of the LSP.
///
/// The LSP loop is synchronous; the backend is responsible for
/// driving any async runtime needed to talk to graphix-rt.
pub trait LspBackend: Send + Sync + 'static {
    /// Get a current snapshot of the base compiler environment with
    /// the stdlib (and any project modules) loaded. This is the env
    /// used as a fallback when a document has no successful check
    /// result yet.
    fn env(&self) -> Env;
    /// Type-check the given document text. On success, return the
    /// env as it would be after compiling the document plus the
    /// list of name reference sites the compiler observed.
    fn typecheck(
        &self,
        source: Source,
        text: ArcStr,
    ) -> anyhow::Result<TypecheckResult>;
    /// Type-check a project from disk, reading `root` and walking
    /// every `mod foo;` declaration via a `Files(<root>.parent())`
    /// resolver. Used by the workspace-wide recheck path to find
    /// cross-file errors and references. Reads files from disk —
    /// unsaved active-doc edits are not seen here (use `typecheck`
    /// for that).
    fn typecheck_project(&self, root: &Path) -> anyhow::Result<TypecheckResult>;
}

/// One project's last-known typecheck result, keyed by the project
/// index in `ServerState.workspace.projects`. `None` while we
/// haven't run the project compile yet (or it failed).
#[derive(Debug, Clone)]
pub struct ProjectResult {
    pub env: Env,
    pub references: Vec<ReferenceSite>,
}

/// Core language intelligence state.
///
/// Holds the compiler environment, open documents, and the workspace
/// project graph. Drives completion, hover, go-to-definition,
/// references, and diagnostics.
pub struct ServerState {
    pub env: Env,
    pub documents: HashMap<Uri, Document>,
    pub backend: Arc<dyn LspBackend>,
    /// Filesystem roots the editor told us about (root_uri /
    /// workspaceFolders). Used to scan for `.gx`/`.gxi` files.
    pub workspace_roots: Vec<PathBuf>,
    /// Last scan of the workspace. Rebuilt on workspace changes.
    pub workspace: crate::workspace::WorkspaceModel,
    /// Last typecheck result per project, parallel to
    /// `workspace.projects`. `None` while a project hasn't been
    /// checked yet or its last check errored.
    pub project_results: Vec<Option<ProjectResult>>,
}

impl ServerState {
    pub fn new(backend: Arc<dyn LspBackend>) -> Self {
        let env = backend.env();
        Self {
            env,
            documents: HashMap::new(),
            backend,
            workspace_roots: Vec::new(),
            workspace: Default::default(),
            project_results: Vec::new(),
        }
    }

    /// Tell the state about the editor's workspace folders. Triggers
    /// an initial scan and project compile.
    pub fn set_workspace_roots(&mut self, roots: Vec<PathBuf>) {
        self.workspace_roots = roots;
        self.recheck_workspace();
    }

    /// Re-scan the workspace and re-typecheck every project from
    /// disk. Throws away the previous project state and replaces it
    /// wholesale.
    pub fn recheck_workspace(&mut self) {
        if self.workspace_roots.is_empty() {
            self.workspace = Default::default();
            self.project_results.clear();
            return;
        }
        self.workspace = crate::workspace::scan(&self.workspace_roots);
        let mut results: Vec<Option<ProjectResult>> =
            Vec::with_capacity(self.workspace.projects.len());
        for project in &self.workspace.projects {
            let r = self.backend.typecheck_project(&project.root);
            match r {
                Ok(TypecheckResult { env, references }) => {
                    results.push(Some(ProjectResult { env, references }));
                }
                Err(_) => {
                    // The error is already routed through the
                    // diagnostics path elsewhere; for queries we just
                    // mark the project as having no result this cycle.
                    results.push(None);
                }
            }
        }
        self.project_results = results;
    }

    /// Indices of projects that contain the file at `uri`.
    fn projects_containing<'a>(&'a self, uri: &Uri) -> impl Iterator<Item = usize> + 'a {
        let path = uri_to_path(uri);
        self.workspace
            .file_to_projects
            .get(&path.unwrap_or_default())
            .into_iter()
            .flat_map(|v| v.iter().copied())
    }

    /// Track a newly opened document.
    pub fn open_document(&mut self, uri: Uri, text: String, version: i32) {
        self.documents.insert(uri, Document::new(text, version));
    }

    /// Update the text of an already-opened document.
    pub fn update_document(&mut self, uri: &Uri, text: String, version: i32) {
        if let Some(doc) = self.documents.get_mut(uri) {
            doc.text = text;
            doc.version = version;
        } else {
            self.documents.insert(uri.clone(), Document::new(text, version));
        }
    }

    /// Stop tracking a document.
    pub fn close_document(&mut self, uri: &Uri) {
        self.documents.remove(uri);
    }

    /// Parse and type-check the document and return any diagnostics.
    /// On success, capture the post-check env and reference sites on
    /// the document so completions, hover, and references see user
    /// bindings.
    pub fn check_document(&mut self, uri: &Uri) -> Vec<lsp_types::Diagnostic> {
        let Some(doc) = self.documents.get(uri) else {
            return Vec::new();
        };
        let source = uri_to_source(uri);
        let text = ArcStr::from(doc.text.as_str());
        match self.backend.typecheck(source, text.clone()) {
            Ok(TypecheckResult { env, references }) => {
                if let Some(d) = self.documents.get_mut(uri) {
                    d.env = Some(env);
                    d.references = references;
                }
                Vec::new()
            }
            Err(e) => {
                if let Some(d) = self.documents.get_mut(uri) {
                    d.env = None;
                    d.references.clear();
                }
                error_to_diagnostics(&e, &text)
            }
        }
    }

    /// Pick the most-specific env we have for `uri`. Order:
    ///   1. The document's own post-check env (live, single-file).
    ///   2. Any project containing the file — its env covers more
    ///      ground (cross-file imports), even if the data is from
    ///      the most recent disk-based project recheck.
    ///   3. The backend's base env (stdlib only).
    fn env_for<'a>(&'a self, uri: &Uri) -> &'a Env {
        if let Some(env) = self.documents.get(uri).and_then(|d| d.env.as_ref()) {
            return env;
        }
        for idx in self.projects_containing(uri) {
            if let Some(Some(r)) = self.project_results.get(idx) {
                return &r.env;
            }
        }
        &self.env
    }

    /// Return completion items at the given position.
    pub fn completions(
        &self,
        uri: &Uri,
        position: lsp_types::Position,
    ) -> Vec<lsp_types::CompletionItem> {
        let Some(doc) = self.documents.get(uri) else {
            return Vec::new();
        };
        let prefix = token_before_cursor(&doc.text, position).unwrap_or_default();
        let scope = ModPath::root();
        let part = modpath_from_typed(&prefix);
        let env = self.env_for(uri);
        let mut items = Vec::new();
        for (name, bind_id) in env.lookup_matching(&scope, &part) {
            if let Some(bind) = env.by_id.get(&bind_id) {
                let kind = if matches!(&bind.typ, Type::Fn(_)) {
                    lsp_types::CompletionItemKind::FUNCTION
                } else {
                    lsp_types::CompletionItemKind::VARIABLE
                };
                items.push(lsp_types::CompletionItem {
                    label: name.to_string(),
                    kind: Some(kind),
                    detail: Some(format!("{}", bind.typ)),
                    documentation: bind
                        .doc
                        .as_ref()
                        .map(|d| lsp_types::Documentation::String(d.to_string())),
                    ..Default::default()
                });
            }
        }
        for module in env.lookup_matching_modules(&scope, &part) {
            items.push(lsp_types::CompletionItem {
                label: module.to_string(),
                kind: Some(lsp_types::CompletionItemKind::MODULE),
                ..Default::default()
            });
        }
        items
    }

    /// Return hover information for the symbol at the given position.
    pub fn hover(
        &self,
        uri: &Uri,
        position: lsp_types::Position,
    ) -> Option<lsp_types::Hover> {
        let doc = self.documents.get(uri)?;
        let word = get_word_at_position(&doc.text, position)?;
        let scope = ModPath::root();
        let name: ModPath = word.split("::").collect();
        let env = self.env_for(uri);

        if let Some((_, bind)) = env.lookup_bind(&scope, &name) {
            let mut contents = format!("```graphix\n{}: {}\n```", word, bind.typ);
            if let Some(doc) = &bind.doc {
                contents.push_str("\n\n");
                contents.push_str(doc);
            }
            return Some(lsp_types::Hover {
                contents: lsp_types::HoverContents::Markup(lsp_types::MarkupContent {
                    kind: lsp_types::MarkupKind::Markdown,
                    value: contents,
                }),
                range: None,
            });
        }

        if let Some(typedef) = env.lookup_typedef(&scope, &name) {
            let mut contents = format!("```graphix\ntype {} = {}\n```", word, typedef.typ);
            if let Some(doc) = &typedef.doc {
                contents.push_str("\n\n");
                contents.push_str(doc);
            }
            return Some(lsp_types::Hover {
                contents: lsp_types::HoverContents::Markup(lsp_types::MarkupContent {
                    kind: lsp_types::MarkupKind::Markdown,
                    value: contents,
                }),
                range: None,
            });
        }

        None
    }

    /// Return all known reference sites for the symbol under the
    /// cursor. Resolves the cursor to a `BindId` two ways: by
    /// matching against a recorded reference site that contains the
    /// position, or — if the cursor is on the binding name itself —
    /// by name lookup in the document's env. Optionally include the
    /// declaration site so editors can show "1 definition + N
    /// references".
    pub fn references(
        &self,
        uri: &Uri,
        position: lsp_types::Position,
        include_declaration: bool,
    ) -> Vec<lsp_types::Location> {
        let Some(doc) = self.documents.get(uri) else {
            return Vec::new();
        };
        // We use (name, scope) as the cross-source key because BindIds
        // are minted fresh per compile — the same identifier resolves
        // to a different `BindId` in the active-doc check vs each
        // project compile.
        let scope = ModPath::root();
        let name = self
            .name_at(uri, position)
            .or_else(|| {
                get_word_at_position(&doc.text, position)
                    .map(|w| w.split("::").collect::<ModPath>())
            });
        let Some(name) = name else { return Vec::new() };
        let mut locs: Vec<lsp_types::Location> = Vec::new();
        // Active doc: doc-local bindings (let foo = …) only the live
        // check sees them.
        self.collect_refs_from(
            doc.env.as_ref(),
            &doc.references,
            &scope,
            &name,
            include_declaration,
            uri,
            &mut locs,
        );
        // Each containing project contributes its cross-file
        // references for the same name.
        for idx in self.projects_containing(uri) {
            if let Some(Some(r)) = self.project_results.get(idx) {
                self.collect_refs_from(
                    Some(&r.env),
                    &r.references,
                    &scope,
                    &name,
                    include_declaration,
                    uri,
                    &mut locs,
                );
            }
        }
        locs.sort_by_key(|l| {
            (
                l.uri.as_str().to_string(),
                l.range.start.line,
                l.range.start.character,
            )
        });
        locs.dedup_by(|a, b| a.uri == b.uri && a.range == b.range);
        locs
    }

    fn collect_refs_from(
        &self,
        env: Option<&Env>,
        references: &[ReferenceSite],
        scope: &ModPath,
        name: &ModPath,
        include_declaration: bool,
        requesting_uri: &Uri,
        out: &mut Vec<lsp_types::Location>,
    ) {
        let Some(env) = env else { return };
        let Some((_, bind)) = env.lookup_bind(scope, name) else { return };
        let bind_id = bind.id;
        for r in references {
            if r.bind_id != bind_id {
                continue;
            }
            if let Some(loc) = ref_to_location(requesting_uri, &r.ori, r.pos) {
                out.push(loc);
            }
        }
        if include_declaration {
            if let Some(loc) = ref_to_location(requesting_uri, &bind.ori, bind.pos) {
                out.push(loc);
            }
        }
    }

    /// If a recorded reference site in the active document covers
    /// `position`, return the name from that site so we can
    /// distinguish shadowed identifiers in cross-source lookup.
    fn name_at(
        &self,
        uri: &Uri,
        position: lsp_types::Position,
    ) -> Option<ModPath> {
        let doc = self.documents.get(uri)?;
        for r in &doc.references {
            if position_in_ref(position, &doc.text, r) {
                return Some(r.name.clone());
            }
        }
        None
    }

    /// Return the document's top-level user-defined bindings. We
    /// identify "user bindings" by matching the bind's recorded
    /// `Origin.text` against the live document text — stdlib and
    /// synthetic bindings have a different (or default) origin.
    pub fn document_symbols(&self, uri: &Uri) -> Vec<lsp_types::DocumentSymbol> {
        let Some(doc) = self.documents.get(uri) else {
            return Vec::new();
        };
        let Some(env) = doc.env.as_ref() else {
            return Vec::new();
        };
        let mut symbols = Vec::new();
        for (_scope, binds) in &env.binds {
            for (name, bind_id) in binds {
                let Some(bind) = env.by_id.get(bind_id) else {
                    continue;
                };
                if &*bind.ori.text != doc.text.as_str() {
                    continue;
                }
                let line = bind.pos.line.saturating_sub(1).max(0) as u32;
                let col = bind.pos.column.saturating_sub(1).max(0) as u32;
                let pos = lsp_types::Position { line, character: col };
                let range = lsp_types::Range { start: pos, end: pos };
                let kind = if matches!(&bind.typ, Type::Fn(_)) {
                    lsp_types::SymbolKind::FUNCTION
                } else {
                    lsp_types::SymbolKind::VARIABLE
                };
                #[allow(deprecated)]
                symbols.push(lsp_types::DocumentSymbol {
                    name: name.to_string(),
                    detail: Some(format!("{}", bind.typ)),
                    kind,
                    tags: None,
                    deprecated: None,
                    range,
                    selection_range: range,
                    children: None,
                });
            }
        }
        symbols.sort_by_key(|s| (s.range.start.line, s.range.start.character));
        symbols
    }

    /// Return the definition location for the symbol at the given
    /// position. Falls back to None when the symbol came from a
    /// non-file source (synthetic bind, netidx module, REPL input).
    pub fn definition(
        &self,
        uri: &Uri,
        position: lsp_types::Position,
    ) -> Option<lsp_types::Location> {
        let doc = self.documents.get(uri)?;
        let word = get_word_at_position(&doc.text, position)?;
        let scope = ModPath::root();
        let name: ModPath = word.split("::").collect();
        let env = self.env_for(uri);
        let (_, bind) = env.lookup_bind(&scope, &name)?;
        let target_uri = match &bind.ori.source {
            Source::File(p) => path_to_uri(p)?,
            // Bindings whose origin is the live document text (the LSP
            // always feeds Source::Internal) point back at the document
            // the user is in.
            Source::Internal(_) | Source::Unspecified => uri.clone(),
            Source::Netidx(_) => return None,
        };
        let line = bind.pos.line.saturating_sub(1).max(0) as u32;
        let column = bind.pos.column.saturating_sub(1).max(0) as u32;
        let pos = lsp_types::Position { line, character: column };
        Some(lsp_types::Location {
            uri: target_uri,
            range: lsp_types::Range { start: pos, end: pos },
        })
    }
}

fn uri_to_source(uri: &Uri) -> Source {
    if let Some(path) = uri_to_path(uri) {
        Source::File(path)
    } else {
        Source::Unspecified
    }
}

fn uri_to_path(uri: &Uri) -> Option<PathBuf> {
    let s = uri.as_str();
    if s.starts_with("file://") {
        Some(PathBuf::from(&s[7..]))
    } else {
        None
    }
}

fn path_to_uri(path: &PathBuf) -> Option<Uri> {
    let uri_str = format!("file://{}", path.display());
    Uri::from_str(&uri_str).ok()
}

/// Check whether a 0-indexed LSP position falls inside a reference
/// site's textual span. We use the printed length of the name as a
/// rough span — `array::map` covers 10 characters from `pos`.
fn position_in_ref(pos: lsp_types::Position, _text: &str, r: &ReferenceSite) -> bool {
    let line0 = r.pos.line.saturating_sub(1).max(0) as u32;
    if pos.line != line0 {
        return false;
    }
    let col0 = r.pos.column.saturating_sub(1).max(0) as u32;
    let len = r.name.to_string().chars().count() as u32;
    pos.character >= col0 && pos.character <= col0 + len
}

/// Map a (Origin, SourcePosition) pair to an LSP Location. For
/// in-document origins (everything the LSP feeds is `Source::Internal`),
/// fall back to the requesting URI.
fn ref_to_location(
    requesting_uri: &Uri,
    ori: &graphix_compiler::expr::Origin,
    pos: SourcePosition,
) -> Option<lsp_types::Location> {
    let target_uri = match &ori.source {
        Source::File(p) => path_to_uri(p)?,
        Source::Internal(_) | Source::Unspecified => requesting_uri.clone(),
        Source::Netidx(_) => return None,
    };
    let line = pos.line.saturating_sub(1).max(0) as u32;
    let column = pos.column.saturating_sub(1).max(0) as u32;
    let p = lsp_types::Position { line, character: column };
    Some(lsp_types::Location {
        uri: target_uri,
        range: lsp_types::Range { start: p, end: p },
    })
}

fn is_id_char(c: char) -> bool {
    c.is_alphanumeric() || c == '_'
}

/// True when `chars[i]` and `chars[i+1]` form a `::` segment separator.
fn is_pathsep(chars: &[char], i: usize) -> bool {
    chars.get(i).copied() == Some(':') && chars.get(i + 1).copied() == Some(':')
}

/// Return the full path-like token at `position`, including any `::`
/// separators (e.g. `array::map`).
fn get_word_at_position(text: &str, position: lsp_types::Position) -> Option<String> {
    let lines: Vec<&str> = text.lines().collect();
    let line = lines.get(position.line as usize)?;
    let chars: Vec<char> = line.chars().collect();
    let col = (position.character as usize).min(chars.len());
    let mut start = col;
    let mut end = col;
    while start > 0 {
        if is_id_char(chars[start - 1]) {
            start -= 1;
        } else if start >= 2 && is_pathsep(&chars, start - 2) {
            start -= 2;
        } else {
            break;
        }
    }
    while end < chars.len() {
        if is_id_char(chars[end]) {
            end += 1;
        } else if is_pathsep(&chars, end) {
            end += 2;
        } else {
            break;
        }
    }
    if start == end {
        return None;
    }
    Some(chars[start..end].iter().collect())
}

/// Return the prefix of a path-like token immediately to the left of the
/// cursor, used as the completion query. Empty if the cursor isn't right
/// after an identifier or `::`.
fn token_before_cursor(text: &str, position: lsp_types::Position) -> Option<String> {
    let lines: Vec<&str> = text.lines().collect();
    let line = lines.get(position.line as usize)?;
    let chars: Vec<char> = line.chars().collect();
    let col = (position.character as usize).min(chars.len());
    let mut start = col;
    while start > 0 {
        if is_id_char(chars[start - 1]) {
            start -= 1;
        } else if start >= 2 && is_pathsep(&chars, start - 2) {
            start -= 2;
        } else {
            break;
        }
    }
    if start == col {
        Some(String::new())
    } else {
        Some(chars[start..col].iter().collect())
    }
}

/// Convert what the user has typed (e.g. `array::ma` or `array::`) into
/// a `ModPath` suitable for `lookup_matching`. A trailing `::` is preserved
/// as an empty basename so completion lists the module's contents.
fn modpath_from_typed(s: &str) -> ModPath {
    if s.is_empty() {
        return ModPath::root();
    }
    let parts: Vec<&str> = s.split("::").collect();
    parts.into_iter().collect()
}
