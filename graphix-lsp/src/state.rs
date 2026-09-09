use crate::{
    diagnostics::{error_leaf_message, error_location},
    position::PositionEncoding,
    workspace::scan,
};
use arcstr::ArcStr;
use graphix_compiler::{
    BindId, SourcePosition,
    env::{Bind, Env},
    expr::{BufferOverrides, Expr, ModPath, Origin, Source},
    ide::{
        Ide, ModuleInternalView, ModuleRefSite, ReferenceSite, ScopeMapEntry,
        SigImplLink, TypeRefSite,
    },
    typ::{FnArgKind, FnType, Type},
};
use lsp_types::Uri;
use poolshark::local::LPooled;
use std::{
    collections::{HashMap, HashSet},
    path::{Path, PathBuf},
    str::FromStr,
    sync::Arc,
};

/// An open document tracked by the language server.
pub struct Document {
    pub version: i32,
    pub text: String,
    /// The compiler environment after type-checking this document;
    /// `None` after a failed check.
    pub env: Option<Env>,
    /// Every IDE side-channel from the most recent successful check.
    pub ide: Ide,
}

impl Document {
    pub fn new(text: String, version: i32) -> Self {
        Self { version, text, env: None, ide: Ide::new() }
    }
}

/// Result of a backend typecheck; mirrors `graphix_rt::CheckResult`
/// without depending on graphix-rt.
pub struct TypecheckResult {
    pub env: Env,
    /// Every IDE side-channel populated during the check.
    pub ide: Ide,
}

/// Backend that owns the graphix runtime and type-checks documents
/// for the LSP. The LSP loop is synchronous; the backend drives any
/// async runtime it needs.
pub trait LspBackend: Send + Sync + 'static {
    /// A snapshot of the base compiler environment with the stdlib
    /// loaded; the fallback when a document has no successful check.
    fn env(&self) -> Env;
    /// Type-check the project rooted at `root`, covering every file
    /// reachable via `mod foo;`. Open buffers participate through the
    /// shared `BufferOverride`. Pass `root = file_path` for a stray
    /// document. `initial_scope` compiles the source as the body of
    /// `mod <scope> { ... }`.
    fn typecheck_project(
        &self,
        root: &Path,
        initial_scope: Option<ArcStr>,
    ) -> anyhow::Result<TypecheckResult>;
    /// The shared open-buffer override map, held by the backend's
    /// resolver chain and mutated by the server on every document event.
    fn buffer_overrides(&self) -> BufferOverrides;
}

/// One project's last-known typecheck result, keyed by its index in
/// `ServerState.workspace.projects`.
pub struct ProjectResult {
    pub env: Env,
    /// Every IDE side-channel from this project's last check.
    pub ide: Ide,
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
    /// Filesystem roots the editor told us about.
    pub workspace_roots: Vec<PathBuf>,
    /// Last scan of the workspace. Rebuilt on workspace changes.
    pub workspace: crate::workspace::WorkspaceModel,
    /// Last typecheck result per project, parallel to `workspace.projects`.
    pub project_results: Vec<Option<ProjectResult>>,
    /// URIs given non-empty diagnostics by the last project recheck, so
    /// recovered files get empty diagnostics next cycle.
    pub last_project_diag_uris: HashSet<Uri>,
    /// The same for the per-document `check_document` cycle; separate so
    /// the two cycles do not stomp each other's stale sets.
    pub last_check_diag_uris: HashSet<Uri>,
    /// The most recent doc the editor told us about; `workspace/symbol`
    /// scopes its search to this doc's project.
    pub last_active_uri: Option<Uri>,
    /// Whether the client advertised `completionItem.snippetSupport`.
    pub snippet_support: bool,
    /// Position encoding negotiated with the client.
    pub position_encoding: PositionEncoding,
}

impl ServerState {
    pub fn new(
        backend: Arc<dyn LspBackend>,
        snippet_support: bool,
        position_encoding: PositionEncoding,
    ) -> Self {
        let env = backend.env();
        Self {
            env,
            documents: HashMap::new(),
            backend,
            workspace_roots: Vec::new(),
            workspace: Default::default(),
            project_results: Vec::new(),
            last_project_diag_uris: HashSet::new(),
            last_check_diag_uris: HashSet::new(),
            last_active_uri: None,
            snippet_support,
            position_encoding,
        }
    }

    /// Record the editor's workspace folders, scan and compile, and
    /// return the per-file diagnostics to publish.
    pub fn set_workspace_roots(
        &mut self,
        roots: Vec<PathBuf>,
    ) -> HashMap<Uri, Vec<lsp_types::Diagnostic>> {
        self.workspace_roots = roots;
        self.recheck_workspace()
    }

    /// Re-scan the workspace and re-typecheck every project from disk,
    /// replacing the previous project state. The returned map covers
    /// errored projects and previously-erroring files that now compile
    /// (with an empty list, so callers can clear stale squiggles).
    pub fn recheck_workspace(&mut self) -> HashMap<Uri, Vec<lsp_types::Diagnostic>> {
        if self.workspace_roots.is_empty() {
            self.workspace = Default::default();
            self.project_results.clear();
            // Clear any stale diagnostics from the previous cycle.
            let mut out = HashMap::new();
            for uri in self.last_project_diag_uris.drain() {
                out.insert(uri, Vec::new());
            }
            return out;
        }
        self.workspace = scan(&self.workspace_roots);
        let mut results: Vec<Option<ProjectResult>> =
            Vec::with_capacity(self.workspace.projects.len());
        let mut diags_by_uri: HashMap<Uri, Vec<lsp_types::Diagnostic>> = HashMap::new();
        for project in &self.workspace.projects {
            let r = self
                .backend
                .typecheck_project(&project.root, project.package_scope.clone());
            match r {
                Ok(TypecheckResult { env, ide }) => {
                    results.push(Some(ProjectResult { env, ide }));
                }
                Err(e) => {
                    let (uri, diag) = self.project_error_to_diagnostic(&e, &project.root);
                    diags_by_uri.entry(uri).or_default().push(diag);
                    results.push(None);
                }
            }
        }
        self.project_results = results;
        // Every URI with diagnostics this cycle plus every URI that had
        // them last cycle and no longer does.
        let mut out: HashMap<Uri, Vec<lsp_types::Diagnostic>> = HashMap::new();
        for (uri, diags) in diags_by_uri {
            out.insert(uri, diags);
        }
        for uri in self.last_project_diag_uris.iter() {
            out.entry(uri.clone()).or_default();
        }
        self.last_project_diag_uris =
            out.iter().filter(|(_, v)| !v.is_empty()).map(|(k, _)| k.clone()).collect();
        out
    }

    /// Translate an incoming LSP position into one whose `character` is
    /// a char count; unchanged when there is no document text to consult.
    fn normalize_position(
        &self,
        uri: &Uri,
        position: lsp_types::Position,
    ) -> lsp_types::Position {
        if matches!(self.position_encoding, PositionEncoding::Utf32) {
            return position;
        }
        let Some(doc) = self.documents.get(uri) else {
            return position;
        };
        let Some(line_text) = doc.text.lines().nth(position.line as usize) else {
            return position;
        };
        let character = crate::position::position_to_char_col(
            line_text,
            position,
            self.position_encoding,
        ) as u32;
        lsp_types::Position { line: position.line, character }
    }

    /// Build an LSP position from a compiler (line, column), encoding
    /// `column` per the negotiated encoding. `text` is the target file's
    /// source, not necessarily the requesting document's.
    fn lsp_position_from_char_col(
        &self,
        text: &str,
        line: u32,
        char_col: usize,
    ) -> lsp_types::Position {
        crate::position::char_col_to_position_in_text(
            text,
            line,
            char_col,
            self.position_encoding,
        )
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

    /// Update an open document's text, mirroring it into the shared
    /// `BufferOverride` map.
    pub fn update_document(&mut self, uri: &Uri, text: String, version: i32) {
        let arc_text = ArcStr::from(text.as_str());
        if let Some(doc) = self.documents.get_mut(uri) {
            doc.text = text;
            doc.version = version;
        } else {
            self.documents.insert(uri.clone(), Document::new(text, version));
        }
        if let Some(path) = uri_to_path(uri) {
            self.backend.buffer_overrides().lock().insert(path, arc_text);
        }
    }

    /// Stop tracking a document and drop its buffer override.
    pub fn close_document(&mut self, uri: &Uri) {
        self.documents.remove(uri);
        if let Some(path) = uri_to_path(uri) {
            self.backend.buffer_overrides().lock().remove(&path);
        }
    }

    /// A project root for `uri`, or the file itself for a stray doc.
    fn project_root_for(&self, uri: &Uri) -> Option<PathBuf> {
        if let Some(idx) = self.projects_containing(uri).next() {
            return Some(self.workspace.projects[idx].root.clone());
        }
        uri_to_path(uri)
    }

    /// The `package_scope` of the project rooted at `root`, if known.
    fn package_scope_for(&self, root: &Path) -> Option<ArcStr> {
        self.workspace
            .projects
            .iter()
            .find(|p| p.root == root)
            .and_then(|p| p.package_scope.clone())
    }

    /// Type-check the document's project (or the stray document itself)
    /// and return the diagnostics; per-doc fields are populated from the
    /// result.
    pub fn check_document(
        &mut self,
        uri: &Uri,
    ) -> HashMap<Uri, Vec<lsp_types::Diagnostic>> {
        self.last_active_uri = Some(uri.clone());
        // `did_open` lands here without a prior `update_document`.
        if let Some(doc) = self.documents.get(uri) {
            if let Some(path) = uri_to_path(uri) {
                self.backend
                    .buffer_overrides()
                    .lock()
                    .insert(path, ArcStr::from(doc.text.as_str()));
            }
        }
        let mut out: HashMap<Uri, Vec<lsp_types::Diagnostic>> = HashMap::new();
        // Always include the active URI so callers can clear stale squiggles.
        out.insert(uri.clone(), Vec::new());
        if !self.documents.contains_key(uri) {
            return out;
        }
        let Some(root) = self.project_root_for(uri) else {
            return out;
        };
        let initial_scope = self.package_scope_for(&root);
        match self.backend.typecheck_project(&root, initial_scope) {
            Ok(TypecheckResult { env, ide }) => {
                if let Some(d) = self.documents.get_mut(uri) {
                    d.env = Some(env);
                    d.ide = ide;
                }
            }
            Err(e) => {
                if let Some(d) = self.documents.get_mut(uri) {
                    d.env = None;
                    d.ide = Ide::new();
                }
                // Attribute the failure to the file the error chain names;
                // it may be a different module of the same project.
                let (target_uri, diag) = self.project_error_to_diagnostic(&e, &root);
                out.entry(target_uri.clone()).or_default().push(diag);
            }
        }
        for stale in self.last_check_diag_uris.iter() {
            out.entry(stale.clone()).or_default();
        }
        self.last_check_diag_uris =
            out.iter().filter(|(_, v)| !v.is_empty()).map(|(k, _)| k.clone()).collect();
        out
    }

    /// The lexical scope at `position` in `uri` from the compiler-emitted
    /// scope map: the entry with the greatest `pos` ≤ cursor in the same
    /// file, active doc first, then any project. Root scope if none.
    pub fn scope_at(&self, uri: &Uri, position: lsp_types::Position) -> ModPath {
        let position = self.normalize_position(uri, position);
        self.scope_at_char(uri, position)
    }

    /// `scope_at` for a position already through `normalize_position`.
    fn scope_at_char(&self, uri: &Uri, position: lsp_types::Position) -> ModPath {
        let mut best: Option<ScopeMapEntry> = None;
        let mut consider = |e: &ScopeMapEntry| {
            if !origin_matches_uri(&e.ori, uri) {
                return;
            }
            if !pos_le(e.pos, position) {
                return;
            }
            match &best {
                None => best = Some(e.clone()),
                Some(b) if pos_lt(b.pos, e.pos) => best = Some(e.clone()),
                _ => (),
            }
        };
        if let Some(doc) = self.documents.get(uri) {
            for e in doc.ide.scope_map.iter() {
                consider(e);
            }
        }
        for r in &self.project_results {
            if let Some(r) = r {
                for e in r.ide.scope_map.iter() {
                    consider(e);
                }
            }
        }
        best.map(|e| e.scope.lexical).unwrap_or_else(ModPath::root)
    }

    /// The most specific env for `uri`: the document's own post-check
    /// env, then any containing project's, then the backend's base env.
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
        let position = self.normalize_position(uri, position);
        let Some(doc) = self.documents.get(uri) else {
            return Vec::new();
        };
        let scope = self.scope_at_char(uri, position);
        let env = self.env_for(uri);
        let mut items = Vec::new();
        // Inside `#…` the user is naming a labeled arg: offer only the
        // callee's labels, with a text_edit replacing the typed `#…`.
        if let Some(label_ctx) = label_prefix(&doc.text, position) {
            if let Some(callee) = call_context(&doc.text, position) {
                let basename = callee.rsplit("::").next().unwrap_or(&callee).to_string();
                let callee_path = modpath_from_typed(&callee);
                for (name, bind) in lookup_matching_via_by_id(env, &scope, &callee_path) {
                    if name.as_str() != basename {
                        continue;
                    }
                    if let Type::Fn(fnt) = &bind.typ {
                        push_labeled_arg_completions(
                            &mut items,
                            fnt,
                            Some(label_ctx.range),
                        );
                    }
                    break;
                }
            }
            return items;
        }
        let prefix = token_before_cursor(&doc.text, position).unwrap_or_default();
        // The scope map makes locally-scoped names visible in completion.
        let part = modpath_from_typed(&prefix);
        let matched = lookup_matching_via_by_id(env, &scope, &part);
        // Inside an open call's argument list, prepend the callee's labeled args.
        if let Some(callee) = call_context(&doc.text, position) {
            let basename = callee.rsplit("::").next().unwrap_or(&callee).to_string();
            let callee_path = modpath_from_typed(&callee);
            for (name, bind) in lookup_matching_via_by_id(env, &scope, &callee_path) {
                if name.as_str() != basename {
                    continue;
                }
                if let Type::Fn(fnt) = &bind.typ {
                    push_labeled_arg_completions(&mut items, fnt, None);
                }
                break;
            }
        }
        // A name that is both a function (via `use foo;`) and its module
        // shows the function; the module stays reachable by qualified path.
        let mut binding_labels: HashSet<String> = HashSet::new();
        for (name, bind) in matched {
            let (kind, snippet) = match &bind.typ {
                Type::Fn(fnt) => (
                    lsp_types::CompletionItemKind::FUNCTION,
                    self.snippet_support.then(|| fn_snippet(name.as_str(), fnt)),
                ),
                _ => (lsp_types::CompletionItemKind::VARIABLE, None),
            };
            let label = name.to_string();
            binding_labels.insert(label.clone());
            let (insert_text, insert_text_format) = match snippet {
                Some(s) => (Some(s), Some(lsp_types::InsertTextFormat::SNIPPET)),
                None => (None, None),
            };
            items.push(lsp_types::CompletionItem {
                label,
                kind: Some(kind),
                detail: Some(format_bind_type(&bind.typ)),
                documentation: bind
                    .doc
                    .as_ref()
                    .map(|d| lsp_types::Documentation::String(d.to_string())),
                insert_text,
                insert_text_format,
                ..Default::default()
            });
        }
        for module in env.lookup_matching_modules(&scope, &part) {
            let label = module.to_string();
            if binding_labels.contains(&label) {
                continue;
            }
            items.push(lsp_types::CompletionItem {
                label,
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
        let position = self.normalize_position(uri, position);
        let doc = self.documents.get(uri)?;
        let env = self.env_for(uri);

        // A recorded reference site is the only path for bindings not
        // reachable by name from the cursor's scope (lambda parameters,
        // nested lets, interpolation variables).
        for r in doc.ide.references.iter() {
            if position_in_ref(position, r) {
                if let Some(bind) = bind_for_id(env, r.bind_id) {
                    return Some(bind_hover(&r.name.to_string(), bind));
                }
            }
        }

        // Declarations are not in `doc.ide.references`; scan ide_binds for
        // a Bind whose `pos` covers the cursor.
        if let Some(bind) = bind_at_decl(env, uri, position) {
            return Some(bind_hover(bind.name.as_str(), bind));
        }

        let word = get_word_at_position(&doc.text, position)?;
        // `position` is already char-encoded.
        let scope = self.scope_at_char(uri, position);
        let name: ModPath = word.split("::").collect();

        if let Some((_, bind)) = env.lookup_bind(&scope, &name).ok().flatten() {
            return Some(bind_hover(&word, bind));
        }

        if let Some(typedef) = env.lookup_typedef(&scope, &name).ok().flatten() {
            let mut contents =
                format!("```graphix\ntype {} = {}\n```", word, typedef.typ);
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

    /// All known reference sites for the symbol under the cursor,
    /// resolved via a covering reference site or by name lookup when the
    /// cursor is on the binding name itself. `include_declaration` adds
    /// the declaration site.
    pub fn references(
        &self,
        uri: &Uri,
        position: lsp_types::Position,
        include_declaration: bool,
    ) -> Vec<lsp_types::Location> {
        let position = self.normalize_position(uri, position);
        let Some(doc) = self.documents.get(uri) else {
            return Vec::new();
        };
        // (name, scope) is the cross-source key: BindIds are minted fresh
        // per compile.
        let scope = ModPath::root();
        let name = self.name_at(uri, position).or_else(|| {
            get_word_at_position(&doc.text, position)
                .map(|w| w.split("::").collect::<ModPath>())
        });
        let Some(name) = name else { return Vec::new() };
        let mut locs: Vec<lsp_types::Location> = Vec::new();
        // Doc-local bindings are seen only by the live check.
        self.collect_refs_from(
            doc.env.as_ref(),
            &doc.ide.references,
            &doc.ide.sig_links,
            &doc.ide.module_internals,
            &scope,
            &name,
            include_declaration,
            uri,
            &mut locs,
        );
        for idx in self.projects_containing(uri) {
            if let Some(Some(r)) = self.project_results.get(idx) {
                self.collect_refs_from(
                    Some(&r.env),
                    &r.ide.references,
                    &r.ide.sig_links,
                    &r.ide.module_internals,
                    &scope,
                    &name,
                    include_declaration,
                    uri,
                    &mut locs,
                );
            }
        }
        // A module name also collects every `use foo;` / `mod foo;` site
        // resolving to the same canonical path.
        if let Some(canonical) = self.canonical_module_at(uri, position) {
            self.collect_module_refs(&canonical, uri, &mut locs);
        }
        // A type name collects every site resolving to the same canonical
        // (scope, name).
        if let Some((canonical_scope, type_name)) =
            self.canonical_typedef_at(uri, position)
        {
            self.collect_type_refs(&canonical_scope, &type_name, uri, &mut locs);
            if include_declaration {
                if let Some(loc) =
                    self.typedef_decl_location(&canonical_scope, &type_name, uri)
                {
                    locs.push(loc);
                }
            }
        }
        locs.sort_by(|a, b| {
            a.uri
                .as_str()
                .cmp(b.uri.as_str())
                .then(a.range.start.line.cmp(&b.range.start.line))
                .then(a.range.start.character.cmp(&b.range.start.character))
        });
        locs.dedup_by(|a, b| a.uri == b.uri && a.range == b.range);
        locs
    }

    fn collect_refs_from(
        &self,
        env: Option<&Env>,
        references: &[ReferenceSite],
        sig_links: &[SigImplLink],
        module_internals: &[ModuleInternalView],
        scope: &ModPath,
        name: &ModPath,
        include_declaration: bool,
        requesting_uri: &Uri,
        out: &mut Vec<lsp_types::Location>,
    ) {
        let Some(env) = env else { return };
        let Some((_, bind)) = env.lookup_bind(scope, name).ok().flatten() else { return };
        let starter_id = bind.id;
        // Sig val proxies live in the external env and impl bindings in
        // the per-module internal env; union the linked pair so the result
        // does not depend on which side was clicked.
        let mut ids: Vec<BindId> = Vec::with_capacity(2);
        ids.push(starter_id);
        for l in sig_links {
            if l.sig_id == starter_id && !ids.contains(&l.impl_id) {
                ids.push(l.impl_id);
            }
            if l.impl_id == starter_id && !ids.contains(&l.sig_id) {
                ids.push(l.sig_id);
            }
        }
        for r in references {
            if !ids.contains(&r.bind_id) {
                continue;
            }
            if let Some(loc) = self.ref_to_location(requesting_uri, &r.ori, r.pos) {
                out.push(loc);
            }
        }
        if include_declaration {
            if let Some(loc) = self.ref_to_location(requesting_uri, &bind.ori, bind.pos) {
                out.push(loc);
            }
            // The impl declaration is the unioned id that is not `starter_id`.
            for id in ids.iter().skip(1) {
                if let Some(impl_bind) =
                    module_internals.iter().find_map(|v| v.env.by_id.get(id))
                {
                    if let Some(loc) = self.ref_to_location(
                        requesting_uri,
                        &impl_bind.ori,
                        impl_bind.pos,
                    ) {
                        out.push(loc);
                    }
                }
            }
        }
    }

    /// The name at a recorded reference site covering `position`, to
    /// distinguish shadowed identifiers in cross-source lookup.
    fn name_at(&self, uri: &Uri, position: lsp_types::Position) -> Option<ModPath> {
        let doc = self.documents.get(uri)?;
        for r in doc.ide.references.iter() {
            if position_in_ref(position, r) {
                return Some(r.name.clone());
            }
        }
        None
    }

    /// The declaration site recorded on the reference site under the
    /// cursor; works for bindings since removed from the env. On a
    /// `use foo;` / `mod foo;` site, the canonical module path.
    fn canonical_module_at(
        &self,
        uri: &Uri,
        position: lsp_types::Position,
    ) -> Option<ModPath> {
        let doc = self.documents.get(uri)?;
        for m in doc.ide.module_references.iter() {
            if position_in_module_ref(position, m) {
                return Some(m.canonical.clone());
            }
        }
        None
    }

    /// Locations of every module reference whose canonical path matches,
    /// across the active doc and every project.
    fn collect_module_refs(
        &self,
        canonical: &ModPath,
        requesting_uri: &Uri,
        out: &mut Vec<lsp_types::Location>,
    ) {
        let push = |m: &ModuleRefSite, out: &mut Vec<lsp_types::Location>| {
            if let Some(loc) = self.ref_to_location(requesting_uri, &m.ori, m.pos) {
                out.push(loc);
            }
        };
        if let Some(doc) = self.documents.get(requesting_uri) {
            for m in doc.ide.module_references.iter() {
                if &m.canonical == canonical {
                    push(m, out);
                }
            }
        }
        for r in &self.project_results {
            if let Some(r) = r {
                for m in r.ide.module_references.iter() {
                    if &m.canonical == canonical {
                        push(m, out);
                    }
                }
            }
        }
    }

    /// The canonical (scope, name) of the typedef the TypeRefSite under
    /// the cursor resolved to.
    fn canonical_typedef_at(
        &self,
        uri: &Uri,
        position: lsp_types::Position,
    ) -> Option<(ModPath, ModPath)> {
        let doc = self.documents.get(uri)?;
        for t in doc.ide.type_refs.iter() {
            if position_in_type_ref(position, t) {
                return Some((t.canonical_scope.clone(), t.name.clone()));
            }
        }
        for r in &self.project_results {
            if let Some(r) = r {
                for t in r.ide.type_refs.iter() {
                    if origin_matches_uri(&t.ori, uri)
                        && position_in_type_ref(position, t)
                    {
                        return Some((t.canonical_scope.clone(), t.name.clone()));
                    }
                }
            }
        }
        None
    }

    fn collect_type_refs(
        &self,
        canonical_scope: &ModPath,
        name: &ModPath,
        requesting_uri: &Uri,
        out: &mut Vec<lsp_types::Location>,
    ) {
        let push = |t: &TypeRefSite, out: &mut Vec<lsp_types::Location>| {
            if let Some(loc) = self.ref_to_location(requesting_uri, &t.ori, t.pos) {
                out.push(loc);
            }
        };
        if let Some(doc) = self.documents.get(requesting_uri) {
            for t in doc.ide.type_refs.iter() {
                if &t.canonical_scope == canonical_scope && &t.name == name {
                    push(t, out);
                }
            }
        }
        for r in &self.project_results {
            if let Some(r) = r {
                for t in r.ide.type_refs.iter() {
                    if &t.canonical_scope == canonical_scope && &t.name == name {
                        push(t, out);
                    }
                }
            }
        }
    }

    /// The typedef declaration site for a canonical (scope, name); any
    /// matching TypeRefSite carries the same def_pos/def_ori.
    fn typedef_decl_location(
        &self,
        canonical_scope: &ModPath,
        name: &ModPath,
        requesting_uri: &Uri,
    ) -> Option<lsp_types::Location> {
        let take =
            |t: &TypeRefSite| self.ref_to_location(requesting_uri, &t.def_ori, t.def_pos);
        if let Some(doc) = self.documents.get(requesting_uri) {
            for t in doc.ide.type_refs.iter() {
                if &t.canonical_scope == canonical_scope && &t.name == name {
                    return take(t);
                }
            }
        }
        for r in &self.project_results {
            if let Some(r) = r {
                for t in r.ide.type_refs.iter() {
                    if &t.canonical_scope == canonical_scope && &t.name == name {
                        return take(t);
                    }
                }
            }
        }
        None
    }

    /// The typedef declaration location for the type reference under the
    /// cursor.
    fn type_definition_at(
        &self,
        uri: &Uri,
        position: lsp_types::Position,
    ) -> Option<lsp_types::Location> {
        let doc = self.documents.get(uri)?;
        for t in doc.ide.type_refs.iter() {
            if position_in_type_ref(position, t) {
                return self.ref_to_location(uri, &t.def_ori, t.def_pos);
            }
        }
        for r in &self.project_results {
            if let Some(r) = r {
                for t in r.ide.type_refs.iter() {
                    // the use site lives in this URI
                    if origin_matches_uri(&t.ori, uri)
                        && position_in_type_ref(position, t)
                    {
                        return self.ref_to_location(uri, &t.def_ori, t.def_pos);
                    }
                }
            }
        }
        None
    }

    /// On a sig `val foo: T;` site in a `.gxi`, the location of the
    /// implementation bind in the paired `.gx` via `sig_links`.
    fn sig_to_impl_definition(
        &self,
        uri: &Uri,
        position: lsp_types::Position,
    ) -> Option<lsp_types::Location> {
        let env = self.env_for(uri);
        let bind = bind_at_decl(env, uri, position)?;
        let sig_id = bind.id;
        // Only sig val proxies are `Bind`s in `ide_binds`, so a sig_link
        // with this id means a sig val site.
        let impl_id = self.sig_link_impl_for(uri, sig_id)?;
        let impl_bind = bind_for_id(env, impl_id).or_else(|| {
            // The impl bind lives in the module's internal env.
            self.module_internals_for(uri)
                .into_iter()
                .find_map(|view| view.env.by_id.get(&impl_id))
        })?;
        let target_uri = match &impl_bind.ori.source {
            Source::File(p) => path_to_uri(p)?,
            Source::Internal(_) | Source::Unspecified => self
                .find_uri_for_internal_origin(uri, &impl_bind.ori)
                .unwrap_or_else(|| uri.clone()),
            Source::Netidx(_) => return None,
        };
        let line = impl_bind.pos.line.saturating_sub(1).max(0) as u32;
        let char_col = impl_bind.pos.column.saturating_sub(1).max(0) as usize;
        let pos = self.lsp_position_from_char_col(&impl_bind.ori.text, line, char_col);
        Some(lsp_types::Location {
            uri: target_uri,
            range: lsp_types::Range { start: pos, end: pos },
        })
    }

    /// The impl bind id for a sig bind id, from the active doc's
    /// `sig_links` and every project's.
    fn sig_link_impl_for(&self, uri: &Uri, sig_id: BindId) -> Option<BindId> {
        if let Some(doc) = self.documents.get(uri) {
            for l in doc.ide.sig_links.iter() {
                if l.sig_id == sig_id {
                    return Some(l.impl_id);
                }
            }
        }
        for idx in self.projects_containing(uri) {
            if let Some(Some(r)) = self.project_results.get(idx) {
                for l in r.ide.sig_links.iter() {
                    if l.sig_id == sig_id {
                        return Some(l.impl_id);
                    }
                }
            }
        }
        None
    }

    /// Per-module internal-view env snapshots for every project
    /// containing `uri`, plus the active doc's.
    fn module_internals_for<'a>(&'a self, uri: &Uri) -> Vec<&'a ModuleInternalView> {
        let mut out = Vec::new();
        if let Some(doc) = self.documents.get(uri) {
            for v in doc.ide.module_internals.iter() {
                out.push(v);
            }
        }
        for idx in self.projects_containing(uri) {
            if let Some(Some(r)) = self.project_results.get(idx) {
                for v in r.ide.module_internals.iter() {
                    out.push(v);
                }
            }
        }
        out
    }

    fn def_site_at_position(
        &self,
        uri: &Uri,
        position: lsp_types::Position,
    ) -> Option<(SourcePosition, Origin)> {
        let doc = self.documents.get(uri)?;
        for r in doc.ide.references.iter() {
            if position_in_ref(position, r) {
                return Some((r.def_pos, Origin::clone(&r.def_ori)));
            }
        }
        None
    }

    /// On a `use foo;` or `mod foo;` site, the file the module body lives
    /// in (from this site's `def_ori`, or any `mod` decl with the same
    /// canonical path).
    fn module_definition_at(
        &self,
        uri: &Uri,
        position: lsp_types::Position,
    ) -> Option<lsp_types::Location> {
        let doc = self.documents.get(uri)?;
        let target = doc
            .ide
            .module_references
            .iter()
            .find(|m| position_in_module_ref(position, m))?;
        if let Some(ori) = target.def_ori.as_ref() {
            return self.module_origin_to_location(uri, ori);
        }
        // Another module ref with the same canonical path, likely the
        // `mod foo;` declaration in the project's main file.
        let canonical = &target.canonical;
        let mut search: LPooled<Vec<&ModuleRefSite>> = LPooled::take();
        search.extend(doc.ide.module_references.iter());
        for r in &self.project_results {
            if let Some(r) = r {
                search.extend(r.ide.module_references.iter());
            }
        }
        search
            .iter()
            .copied()
            .find(|m| &m.canonical == canonical && m.def_ori.is_some())
            .and_then(|m| {
                self.module_origin_to_location(uri, m.def_ori.as_ref().unwrap())
            })
    }

    /// The document's top-level user-defined bindings: those whose
    /// recorded `Origin.text` matches the live document text.
    pub fn document_symbols(&self, uri: &Uri) -> Vec<lsp_types::DocumentSymbol> {
        let Some(doc) = self.documents.get(uri) else {
            return Vec::new();
        };
        // `.gxi` items never land in `env.binds`; parse the buffer as a sig.
        if uri_is_gxi(uri) {
            return self.gxi_document_symbols(&doc.text);
        }
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
                let char_col = bind.pos.column.saturating_sub(1).max(0) as usize;
                let pos = self.lsp_position_from_char_col(&doc.text, line, char_col);
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

    /// Symbols for a `.gxi` interface file. Parses the text as a Sig
    /// and emits one entry per `val`/`type`/`mod`/`use` item.
    fn gxi_document_symbols(&self, text: &str) -> Vec<lsp_types::DocumentSymbol> {
        use graphix_compiler::expr::{Origin, SigKind, Source, parser};
        let ori = Origin {
            parent: None,
            source: Source::Unspecified,
            text: ArcStr::from(text),
        };
        let Ok(sig) = parser::parse_sig(ori) else {
            return Vec::new();
        };
        let mut symbols = Vec::new();
        for si in sig.items.iter() {
            let (name, kind, detail) = match &si.kind {
                SigKind::Bind(b) => {
                    let kind = if matches!(b.typ, Type::Fn(_)) {
                        lsp_types::SymbolKind::FUNCTION
                    } else {
                        lsp_types::SymbolKind::VARIABLE
                    };
                    (b.name.to_string(), kind, Some(format!("{}", b.typ)))
                }
                SigKind::TypeDef(t) => {
                    let detail = format!("{t}");
                    (
                        t.name.to_string(),
                        lsp_types::SymbolKind::TYPE_PARAMETER,
                        Some(detail),
                    )
                }
                SigKind::Module(name) => {
                    (name.to_string(), lsp_types::SymbolKind::MODULE, None)
                }
                SigKind::Trait(t) => {
                    (t.name.to_string(), lsp_types::SymbolKind::INTERFACE, None)
                }
                SigKind::Impl(i) => (
                    format!("impl {} for {}", i.trait_name, i.target),
                    lsp_types::SymbolKind::OBJECT,
                    None,
                ),
                SigKind::Use { names, .. } => {
                    let mut label = String::from("use ");
                    for (i, n) in names.iter().enumerate() {
                        if i > 0 {
                            label.push_str(", ");
                        }
                        label.push_str(&n.to_string());
                    }
                    (label, lsp_types::SymbolKind::NAMESPACE, None)
                }
            };
            let line = si.pos.line.saturating_sub(1).max(0) as u32;
            let char_col = si.pos.column.saturating_sub(1).max(0) as usize;
            let pos = self.lsp_position_from_char_col(text, line, char_col);
            let range = lsp_types::Range { start: pos, end: pos };
            #[allow(deprecated)]
            symbols.push(lsp_types::DocumentSymbol {
                name,
                detail,
                kind,
                tags: None,
                deprecated: None,
                range,
                selection_range: range,
                children: None,
            });
        }
        symbols.sort_by_key(|s| (s.range.start.line, s.range.start.character));
        symbols
    }

    /// Workspace-wide symbol search, scoped to the project containing
    /// the most recently active document (every project if unknown),
    /// filtered by case-insensitive substring.
    pub fn workspace_symbols(&self, query: &str) -> Vec<lsp_types::SymbolInformation> {
        use graphix_compiler::expr::{Origin, SigKind, Source, parser};
        let needle = query.to_ascii_lowercase();
        let matches = |name: &str| -> bool {
            needle.is_empty() || name.to_ascii_lowercase().contains(&needle)
        };
        let active_idx = self
            .last_active_uri
            .as_ref()
            .and_then(|u| self.projects_containing(u).next());
        log::debug!(
            "workspace_symbols query={query:?} active={:?} projects={} files={} active_idx={:?}",
            self.last_active_uri.as_ref().map(|u| u.as_str()),
            self.workspace.projects.len(),
            self.workspace.files.len(),
            active_idx,
        );
        let scoped_files: Vec<&PathBuf> = match active_idx {
            Some(idx) => {
                let mut v: Vec<&PathBuf> =
                    self.workspace.projects[idx].files.iter().collect();
                v.sort();
                v
            }
            None => {
                let mut v: Vec<&PathBuf> = self.workspace.files.keys().collect();
                v.sort();
                v
            }
        };
        let mut out = Vec::new();
        for path in scoped_files {
            let Some(uri) = path_to_uri(path) else {
                continue;
            };
            // Open-buffer text first so unsaved edits are searchable.
            let text: ArcStr = match self.documents.get(&uri) {
                Some(d) => ArcStr::from(d.text.as_str()),
                None => match std::fs::read_to_string(path) {
                    Ok(s) => ArcStr::from(s),
                    Err(_) => continue,
                },
            };
            let is_gxi = path
                .extension()
                .and_then(|s| s.to_str())
                .map(|e| e == "gxi")
                .unwrap_or(false);
            let ori = Origin {
                parent: None,
                source: Source::File(path.clone()),
                text: text.clone(),
            };
            let push =
                |name: String,
                 kind: lsp_types::SymbolKind,
                 line: i32,
                 col: i32,
                 out: &mut Vec<lsp_types::SymbolInformation>| {
                    if !matches(&name) {
                        return;
                    }
                    let line = line.saturating_sub(1).max(0) as u32;
                    let char_col = col.saturating_sub(1).max(0) as usize;
                    let pos = self.lsp_position_from_char_col(&text, line, char_col);
                    let range = lsp_types::Range { start: pos, end: pos };
                    #[allow(deprecated)]
                    out.push(lsp_types::SymbolInformation {
                        name,
                        kind,
                        tags: None,
                        deprecated: None,
                        location: lsp_types::Location { uri: uri.clone(), range },
                        container_name: None,
                    });
                };
            if is_gxi {
                let Ok(sig) = parser::parse_sig(ori) else {
                    continue;
                };
                for si in sig.items.iter() {
                    let (name, kind) = match &si.kind {
                        SigKind::Bind(b) => {
                            let kind = if matches!(b.typ, Type::Fn(_)) {
                                lsp_types::SymbolKind::FUNCTION
                            } else {
                                lsp_types::SymbolKind::VARIABLE
                            };
                            (b.name.to_string(), kind)
                        }
                        SigKind::TypeDef(t) => {
                            (t.name.to_string(), lsp_types::SymbolKind::TYPE_PARAMETER)
                        }
                        SigKind::Module(name) => {
                            (name.to_string(), lsp_types::SymbolKind::MODULE)
                        }
                        SigKind::Trait(t) => {
                            (t.name.to_string(), lsp_types::SymbolKind::INTERFACE)
                        }
                        SigKind::Use { .. } | SigKind::Impl(_) => continue,
                    };
                    push(name, kind, si.pos.line, si.pos.column, &mut out);
                }
            } else {
                let Ok(exprs) = parser::parse(ori) else {
                    continue;
                };
                for e in exprs.iter() {
                    self.walk_gx_for_workspace_symbols(
                        e, &uri, &text, &matches, &mut out,
                    );
                }
            }
        }
        out.sort_by(|a, b| a.name.cmp(&b.name));
        out
    }

    /// Walk a top-level `.gx` expression and emit a workspace symbol
    /// for every binding-shaped item (`let`, `type`, `mod`).
    fn walk_gx_for_workspace_symbols(
        &self,
        e: &Expr,
        uri: &Uri,
        text: &str,
        matches: &impl Fn(&str) -> bool,
        out: &mut Vec<lsp_types::SymbolInformation>,
    ) {
        use graphix_compiler::expr::{ExprKind, StructurePattern};
        let mut push =
            |name: String, kind: lsp_types::SymbolKind, pos: SourcePosition| {
                if !matches(&name) {
                    return;
                }
                let line = pos.line.saturating_sub(1).max(0) as u32;
                let char_col = pos.column.saturating_sub(1).max(0) as usize;
                let p = self.lsp_position_from_char_col(text, line, char_col);
                let range = lsp_types::Range { start: p, end: p };
                #[allow(deprecated)]
                out.push(lsp_types::SymbolInformation {
                    name,
                    kind,
                    tags: None,
                    deprecated: None,
                    location: lsp_types::Location { uri: uri.clone(), range },
                    container_name: None,
                });
            };
        match &e.kind {
            ExprKind::Bind(b) => {
                if let StructurePattern::Bind(name) = &b.pattern {
                    let kind = match &b.value.kind {
                        ExprKind::Lambda(_) => lsp_types::SymbolKind::FUNCTION,
                        _ => lsp_types::SymbolKind::VARIABLE,
                    };
                    push(name.to_string(), kind, e.pos);
                }
            }
            ExprKind::TypeDef(td) => {
                push(td.name.to_string(), lsp_types::SymbolKind::TYPE_PARAMETER, e.pos);
            }
            ExprKind::Module { name, .. } => {
                push(name.to_string(), lsp_types::SymbolKind::MODULE, e.pos);
            }
            _ => (),
        }
    }

    /// The definition location for the symbol at `position`; `None` when
    /// the symbol came from a non-file source.
    pub fn definition(
        &self,
        uri: &Uri,
        position: lsp_types::Position,
    ) -> Option<lsp_types::Location> {
        let position = self.normalize_position(uri, position);
        // Sig val site → implementation site.
        if let Some(loc) = self.sig_to_impl_definition(uri, position) {
            return Some(loc);
        }
        // A ReferenceSite carries the declaration site, including lambda
        // parameters `by_id` no longer remembers.
        if let Some((def_pos, def_ori)) = self.def_site_at_position(uri, position) {
            return self.ref_to_location(uri, &def_ori, def_pos);
        }
        if let Some(loc) = self.module_definition_at(uri, position) {
            return Some(loc);
        }
        if let Some(loc) = self.type_definition_at(uri, position) {
            return Some(loc);
        }
        // The cursor directly on the binding name has no ReferenceSite.
        let doc = self.documents.get(uri)?;
        let word = get_word_at_position(&doc.text, position)?;
        let scope = ModPath::root();
        let name: ModPath = word.split("::").collect();
        let env = self.env_for(uri);
        if let Some((_, bind)) = env.lookup_bind(&scope, &name).ok().flatten() {
            let target_uri = match &bind.ori.source {
                Source::File(p) => path_to_uri(p)?,
                Source::Internal(_) | Source::Unspecified => self
                    .find_uri_for_internal_origin(uri, &bind.ori)
                    .unwrap_or_else(|| uri.clone()),
                Source::Netidx(_) => return None,
            };
            let line = bind.pos.line.saturating_sub(1).max(0) as u32;
            let char_col = bind.pos.column.saturating_sub(1).max(0) as usize;
            let pos = self.lsp_position_from_char_col(&bind.ori.text, line, char_col);
            return Some(lsp_types::Location {
                uri: target_uri,
                range: lsp_types::Range { start: pos, end: pos },
            });
        }
        // The cursor directly on a typedef name has no ReferenceSite.
        let typedef = env.lookup_typedef(&scope, &name).ok().flatten()?;
        let target_uri = match &typedef.ori.source {
            Source::File(p) => path_to_uri(p)?,
            Source::Internal(_) | Source::Unspecified => self
                .find_uri_for_internal_origin(uri, &typedef.ori)
                .unwrap_or_else(|| uri.clone()),
            Source::Netidx(_) => return None,
        };
        let line = typedef.pos.line.saturating_sub(1).max(0) as u32;
        let char_col = typedef.pos.column.saturating_sub(1).max(0) as usize;
        let pos = self.lsp_position_from_char_col(&typedef.ori.text, line, char_col);
        Some(lsp_types::Location {
            uri: target_uri,
            range: lsp_types::Range { start: pos, end: pos },
        })
    }
}

fn uri_to_path(uri: &Uri) -> Option<PathBuf> {
    crate::uri::uri_to_path(uri)
}

fn uri_is_gxi(uri: &Uri) -> bool {
    uri.as_str().ends_with(".gxi")
}

fn path_to_uri(path: &Path) -> Option<Uri> {
    crate::uri::path_to_uri(path)
}

/// Turn a project compile error into a (uri, diagnostic) pair attributed
/// to the originating file, else the project root.
impl ServerState {
    fn project_error_to_diagnostic(
        &self,
        err: &anyhow::Error,
        project_root: &Path,
    ) -> (Uri, lsp_types::Diagnostic) {
        let loc = error_location(err);
        let target_path = loc.file.unwrap_or_else(|| project_root.to_path_buf());
        let uri = path_to_uri(&target_path).unwrap_or_else(|| {
            // Fall back to the project root rather than drop the diagnostic.
            path_to_uri(project_root)
                .or_else(|| Uri::from_str("file:///").ok())
                .expect("file:/// is a valid URI")
        });
        let char_pos = loc.position.unwrap_or_default();
        // Translate the char-based position using the target file's text:
        // the open document if tracked, else disk; on a read failure the
        // unencoded position (identical for ASCII sources).
        let pos = match self.documents.get(&uri) {
            Some(doc) => self.lsp_position_from_char_col(
                &doc.text,
                char_pos.line,
                char_pos.character as usize,
            ),
            None => match std::fs::read_to_string(&target_path) {
                Ok(text) => self.lsp_position_from_char_col(
                    &text,
                    char_pos.line,
                    char_pos.character as usize,
                ),
                Err(_) => char_pos,
            },
        };
        let diag = lsp_types::Diagnostic {
            range: lsp_types::Range { start: pos, end: pos },
            severity: Some(lsp_types::DiagnosticSeverity::ERROR),
            source: Some("graphix".to_string()),
            message: error_leaf_message(err),
            ..Default::default()
        };
        (uri, diag)
    }
}

/// Char count of `name` as `Display` renders it (`array::map` → 10),
/// without allocating.
fn modpath_display_chars(name: &ModPath) -> u32 {
    use netidx::path::Path as NPath;
    use std::borrow::Borrow;
    let s: &str = name.borrow();
    let levels = NPath::levels(s);
    let parts: usize = NPath::parts(s).map(|p| p.chars().count()).sum();
    (parts + 2 * levels.saturating_sub(1)) as u32
}

/// Whether a 0-indexed LSP position falls inside a reference site's
/// span, taken as the printed length of the name.
fn position_in_ref(pos: lsp_types::Position, r: &ReferenceSite) -> bool {
    span_covers(pos, r.pos, modpath_display_chars(&r.name))
}

/// The same for a module reference; `pos` is at the `mod`/`use` keyword.
fn position_in_module_ref(pos: lsp_types::Position, m: &ModuleRefSite) -> bool {
    // pessimistic on `use   foo;`
    span_covers(pos, m.pos, 4 + modpath_display_chars(&m.name))
}

/// Type references record the position of the type name itself.
fn position_in_type_ref(pos: lsp_types::Position, t: &TypeRefSite) -> bool {
    span_covers(pos, t.pos, modpath_display_chars(&t.name))
}

/// True if the Origin's source path matches the requesting URI.
/// Internal/Unspecified always match; Netidx never does.
fn origin_matches_uri(ori: &Origin, uri: &Uri) -> bool {
    match &ori.source {
        Source::File(p) => match path_to_uri(p) {
            Some(u) => &u == uri,
            None => false,
        },
        // The active document and every VFS stdlib module are both
        // `Source::Internal`; only the document has `parent = None`.
        Source::Internal(_) | Source::Unspecified => ori.parent.is_none(),
        Source::Netidx(_) => false,
    }
}

/// True if `a` (1-indexed compiler pos) is ≤ `b` (0-indexed LSP pos).
fn pos_le(a: SourcePosition, b: lsp_types::Position) -> bool {
    let a_line = a.line.saturating_sub(1).max(0) as u32;
    if a_line < b.line {
        return true;
    }
    if a_line > b.line {
        return false;
    }
    let a_col = a.column.saturating_sub(1).max(0) as u32;
    a_col <= b.character
}

/// Strict less-than between two 1-indexed compiler positions.
fn pos_lt(a: SourcePosition, b: SourcePosition) -> bool {
    if a.line < b.line {
        return true;
    }
    if a.line > b.line {
        return false;
    }
    a.column < b.column
}

fn span_covers(pos: lsp_types::Position, start: SourcePosition, len: u32) -> bool {
    let line0 = start.line.saturating_sub(1).max(0) as u32;
    if pos.line != line0 {
        return false;
    }
    let col0 = start.column.saturating_sub(1).max(0) as u32;
    pos.character >= col0 && pos.character <= col0 + len
}

impl ServerState {
    /// Recover the file URI of an `Internal`/`Unspecified` origin by
    /// content match against the `.gx`/`.gxi` sibling of the requesting
    /// URI (the VFS gives both stdlib files the same `Internal` name).
    fn find_uri_for_internal_origin(
        &self,
        requesting_uri: &Uri,
        ori: &Origin,
    ) -> Option<Uri> {
        let want = ori.text.as_str();
        if let Some(doc) = self.documents.get(requesting_uri) {
            if doc.text.as_str() == want {
                return Some(requesting_uri.clone());
            }
        }
        let path = uri_to_path(requesting_uri)?;
        let sibling = match path.extension().and_then(|s| s.to_str()) {
            Some("gx") => path.with_extension("gxi"),
            Some("gxi") => path.with_extension("gx"),
            _ => return None,
        };
        let sibling_uri = path_to_uri(&sibling)?;
        if let Some(doc) = self.documents.get(&sibling_uri) {
            if doc.text.as_str() == want {
                return Some(sibling_uri);
            }
        }
        if let Ok(disk) = std::fs::read_to_string(&sibling) {
            if disk == want {
                return Some(sibling_uri);
            }
        }
        None
    }

    /// Map a module reference's `def_ori` to a Location at the file's start.
    fn module_origin_to_location(
        &self,
        requesting_uri: &Uri,
        ori: &Origin,
    ) -> Option<lsp_types::Location> {
        let target_uri = match &ori.source {
            Source::File(p) => path_to_uri(p)?,
            Source::Internal(_) | Source::Unspecified => self
                .find_uri_for_internal_origin(requesting_uri, ori)
                .unwrap_or_else(|| requesting_uri.clone()),
            Source::Netidx(_) => return None,
        };
        let pos = lsp_types::Position { line: 0, character: 0 };
        Some(lsp_types::Location {
            uri: target_uri,
            range: lsp_types::Range { start: pos, end: pos },
        })
    }

    /// Map an (Origin, SourcePosition) to an LSP Location; in-document
    /// origins fall back to the requesting URI. The column is encoded
    /// against the target file's text.
    fn ref_to_location(
        &self,
        requesting_uri: &Uri,
        ori: &Origin,
        pos: SourcePosition,
    ) -> Option<lsp_types::Location> {
        let target_uri = match &ori.source {
            Source::File(p) => path_to_uri(p)?,
            Source::Internal(_) | Source::Unspecified => self
                .find_uri_for_internal_origin(requesting_uri, ori)
                .unwrap_or_else(|| requesting_uri.clone()),
            Source::Netidx(_) => return None,
        };
        let line = pos.line.saturating_sub(1).max(0) as u32;
        let char_col = pos.column.saturating_sub(1).max(0) as usize;
        // The open document text if tracked, else the text the compiler saw.
        let target_text: &str =
            self.documents.get(&target_uri).map(|d| d.text.as_str()).unwrap_or(&ori.text);
        let p = self.lsp_position_from_char_col(target_text, line, char_col);
        Some(lsp_types::Location {
            uri: target_uri,
            range: lsp_types::Range { start: p, end: p },
        })
    }
}

/// Like `Env::lookup_matching` over `env.ide_binds`, so short-lived
/// bindings (lambda params, torn-down scopes) stay visible. A bind is
/// visible from `cursor_scope` if its scope is that scope, an ancestor,
/// or reachable via `use`.
fn lookup_matching_via_by_id(
    env: &Env,
    cursor_scope: &ModPath,
    part: &ModPath,
) -> Vec<(compact_str::CompactString, Bind)> {
    let mut out = Vec::new();
    let mut seen: HashSet<compact_str::CompactString> = HashSet::new();
    for (name, bind_id) in env.lookup_matching(cursor_scope, part) {
        if !seen.insert(name.clone()) {
            continue;
        }
        if let Some(bind) = env.by_id.get(&bind_id) {
            out.push((name, bind.clone()));
        }
    }
    out
}

fn is_id_char(c: char) -> bool {
    c.is_alphanumeric() || c == '_'
}

/// An LSP snippet body for a function completion: a `${N:placeholder}`
/// per required arg (named from `FnArgType::name`, else `a0`, `a1`, …
/// or the label), labeled args with defaults skipped, `$0` after the parens.
fn fn_snippet(name: &str, fnt: &FnType) -> String {
    use std::fmt::Write;
    let mut body = String::new();
    body.push_str(name);
    body.push('(');
    let mut idx = 1u32;
    let mut positional = 0u32;
    let mut first = true;
    for arg in fnt.args.iter() {
        match &arg.kind {
            FnArgKind::Labeled { has_default: true, .. } => continue,
            FnArgKind::Labeled { name: label, has_default: false } => {
                if !first {
                    body.push_str(", ");
                }
                let _ = write!(body, "#{label}: ${{{idx}:{label}}}");
                idx += 1;
                first = false;
            }
            FnArgKind::Positional { name } => {
                if !first {
                    body.push_str(", ");
                }
                match name.as_deref() {
                    Some(n) => {
                        let _ = write!(body, "${{{idx}:{n}}}");
                    }
                    None => {
                        let _ = write!(body, "${{{idx}:a{positional}}}");
                    }
                }
                idx += 1;
                positional += 1;
                first = false;
            }
        }
    }
    body.push_str(")$0");
    body
}

/// The callee path (`foo`, `array::map`) if the cursor is inside an open
/// `(`'s argument list; `None` inside `[`/`{` or past a statement
/// boundary. String literals are not parsed.
fn call_context(text: &str, position: lsp_types::Position) -> Option<String> {
    let mut chars: LPooled<Vec<char>> = LPooled::take();
    chars.extend(text.chars());
    // `position.character` is a char count here.
    let mut offset = 0usize;
    let mut line = 0u32;
    let mut col = 0u32;
    while offset < chars.len() {
        if line == position.line && col == position.character {
            break;
        }
        let c = chars[offset];
        if c == '\n' {
            line += 1;
            col = 0;
        } else {
            col += 1;
        }
        offset += 1;
    }
    let mut depth = 0i32;
    let mut i = offset;
    while i > 0 {
        i -= 1;
        let c = chars[i];
        match c {
            ')' | ']' | '}' => depth += 1,
            '(' => {
                if depth == 0 {
                    // the enclosing open-paren
                    let mut j = i;
                    while j > 0 && chars[j - 1].is_whitespace() {
                        j -= 1;
                    }
                    let mut start = j;
                    while start > 0 {
                        if is_id_char(chars[start - 1]) {
                            start -= 1;
                        } else if start >= 2 && is_pathsep(&chars, start - 2) {
                            start -= 2;
                        } else {
                            break;
                        }
                    }
                    if start == j {
                        return None;
                    }
                    return Some(chars[start..j].iter().collect());
                }
                depth -= 1;
            }
            '[' | '{' => {
                if depth == 0 {
                    return None;
                }
                depth -= 1;
            }
            ';' if depth == 0 => return None,
            _ => {}
        }
    }
    None
}

/// Append a `#label` completion item per labeled arg of `fnt`.
///
/// `replace` is the range accepting a completion replaces (the typed
/// `#…`); `None` inserts at the cursor.
fn push_labeled_arg_completions(
    items: &mut Vec<lsp_types::CompletionItem>,
    fnt: &FnType,
    replace: Option<lsp_types::Range>,
) {
    for arg in fnt.args.iter() {
        let Some(label) = arg.label() else {
            continue;
        };
        let label_text = format!("#{label}");
        let insert = format!("#{label}: ");
        let text_edit = replace.map(|range| {
            lsp_types::CompletionTextEdit::Edit(lsp_types::TextEdit {
                range,
                new_text: insert.clone(),
            })
        });
        items.push(lsp_types::CompletionItem {
            label: label_text,
            kind: Some(lsp_types::CompletionItemKind::FIELD),
            detail: Some(format!("{}", arg.typ.clone().resolve_tvars())),
            insert_text: text_edit.is_none().then_some(insert),
            text_edit,
            ..Default::default()
        });
    }
}

/// The `#`-prefixed token being typed and the range a label completion
/// replaces.
struct LabelCtx {
    range: lsp_types::Range,
}

/// The range of the `#…` token under the cursor, if any.
fn label_prefix(text: &str, position: lsp_types::Position) -> Option<LabelCtx> {
    let line = text.lines().nth(position.line as usize)?;
    let mut chars: LPooled<Vec<char>> = LPooled::take();
    chars.extend(line.chars());
    let col = (position.character as usize).min(chars.len());
    let mut start = col;
    while start > 0 && is_id_char(chars[start - 1]) {
        start -= 1;
    }
    if start == 0 || chars[start - 1] != '#' {
        return None;
    }
    // `?#…` is only valid in fn type signatures
    if start >= 2 && chars[start - 2] == '?' {
        return None;
    }
    let hash_col = (start - 1) as u32;
    Some(LabelCtx {
        range: lsp_types::Range {
            start: lsp_types::Position { line: position.line, character: hash_col },
            end: position,
        },
    })
}

/// The hover payload for a binding: a graphix code fence with
/// `name: type`, then any doc comment.
fn bind_hover(name: &str, bind: &Bind) -> lsp_types::Hover {
    let mut contents =
        format!("```graphix\n{}: {}\n```", name, format_bind_type(&bind.typ));
    if let Some(d) = &bind.doc {
        contents.push_str("\n\n");
        contents.push_str(d);
    }
    lsp_types::Hover {
        contents: lsp_types::HoverContents::Markup(lsp_types::MarkupContent {
            kind: lsp_types::MarkupKind::Markdown,
            value: contents,
        }),
        range: None,
    }
}

/// A binding whose declaration position covers the cursor (parameters
/// and let names are in `ide_binds`, not `references`), filtered by URI.
fn bind_at_decl<'a>(
    env: &'a Env,
    uri: &Uri,
    position: lsp_types::Position,
) -> Option<&'a Bind> {
    for (_, defs) in &env.ide_binds {
        for (_, b) in defs {
            if !origin_matches_uri(&b.ori, uri) {
                continue;
            }
            if span_covers(position, b.pos, b.name.len() as u32) {
                return Some(b);
            }
        }
    }
    None
}

/// Look up a bind by id, falling back to a scan of `ide_binds`, which
/// keeps lambda parameters after `env.by_id` has dropped them.
fn bind_for_id(env: &Env, id: BindId) -> Option<&Bind> {
    if let Some(b) = env.by_id.get(&id) {
        return Some(b);
    }
    for (_, defs) in &env.ide_binds {
        for (_, b) in defs {
            if b.id == id {
                return Some(b);
            }
        }
    }
    None
}

/// Format a binding's type for display. `replace_auto_constrained`
/// (Fn types) folds constraint-table tvars into the surface first;
/// `resolve_tvars` empties the constraint table, so it must run second.
fn format_bind_type(typ: &Type) -> String {
    use triomphe::Arc;
    let folded = match typ {
        Type::Fn(ft) => Type::Fn(Arc::new(ft.replace_auto_constrained())),
        t => t.clone(),
    };
    format!("{}", folded.resolve_tvars())
}

/// True when `chars[i]` and `chars[i+1]` form a `::` segment separator.
fn is_pathsep(chars: &[char], i: usize) -> bool {
    chars.get(i).copied() == Some(':') && chars.get(i + 1).copied() == Some(':')
}

/// Return the full path-like token at `position`, including any `::`
/// separators (e.g. `array::map`).
fn get_word_at_position(text: &str, position: lsp_types::Position) -> Option<String> {
    let line = text.lines().nth(position.line as usize)?;
    let mut chars: LPooled<Vec<char>> = LPooled::take();
    chars.extend(line.chars());
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
    let line = text.lines().nth(position.line as usize)?;
    let mut chars: LPooled<Vec<char>> = LPooled::take();
    chars.extend(line.chars());
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

/// Convert the typed prefix (`array::ma`, `array::`) into a `ModPath` for
/// `lookup_matching`; a trailing `::` becomes an empty basename.
fn modpath_from_typed(s: &str) -> ModPath {
    if s.is_empty() {
        return ModPath::root();
    }
    let parts: Vec<&str> = s.split("::").collect();
    parts.into_iter().collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use Origin;
    use ahash::AHashMap;
    use arcstr::literal;
    use std::str::FromStr;
    use triomphe::Arc;

    fn ori(source: Source, parent: Option<Arc<Origin>>) -> Origin {
        Origin { parent, source, text: literal!("") }
    }

    /// The active document is `Source::Internal` with no parent; VFS
    /// stdlib modules are `Internal` with a parent and must not match.
    #[test]
    fn origin_matches_uri_internal_active_doc_only() {
        let uri = Uri::from_str("file:///tmp/active.gx").unwrap();
        let active = ori(Source::Internal(literal!("let x = 1")), None);
        assert!(origin_matches_uri(&active, &uri));

        let parent = Arc::new(ori(Source::Internal(literal!("loader")), None));
        let vfs_child = ori(Source::Internal(literal!("array")), Some(parent));
        assert!(!origin_matches_uri(&vfs_child, &uri));
    }

    /// File-loaded modules match only when the path round-trips to the URI.
    #[test]
    fn origin_matches_uri_file_paths() {
        let uri = Uri::from_str("file:///tmp/active.gx").unwrap();
        let active_file = ori(Source::File(PathBuf::from("/tmp/active.gx")), None);
        assert!(origin_matches_uri(&active_file, &uri));

        let other_file = ori(Source::File(PathBuf::from("/tmp/other.gx")), None);
        assert!(!origin_matches_uri(&other_file, &uri));

        // the same physical file, however it was reached
        let parent = Arc::new(ori(Source::Internal(literal!("loader")), None));
        let active_via_parent =
            ori(Source::File(PathBuf::from("/tmp/active.gx")), Some(parent));
        assert!(origin_matches_uri(&active_via_parent, &uri));
    }

    /// Netidx-sourced origins never match a file URI.
    #[test]
    fn origin_matches_uri_netidx_never_matches() {
        let uri = Uri::from_str("file:///tmp/active.gx").unwrap();
        let n = ori(Source::Netidx(netidx::path::Path::from("/foo")), None);
        assert!(!origin_matches_uri(&n, &uri));
    }

    /// `Source::Unspecified` follows the same parent rule as `Internal`.
    #[test]
    fn origin_matches_uri_unspecified_follows_parent_rule() {
        let uri = Uri::from_str("file:///tmp/active.gx").unwrap();
        let top = ori(Source::Unspecified, None);
        assert!(origin_matches_uri(&top, &uri));

        let parent = Arc::new(ori(Source::Unspecified, None));
        let child = ori(Source::Unspecified, Some(parent));
        assert!(!origin_matches_uri(&child, &uri));
    }

    /// Stand-in `LspBackend` for tests that need no typecheck.
    struct StubBackend {
        overrides: BufferOverrides,
    }
    impl StubBackend {
        fn new() -> std::sync::Arc<Self> {
            std::sync::Arc::new(Self {
                overrides: triomphe::Arc::new(parking_lot::Mutex::new(
                    AHashMap::default(),
                )),
            })
        }
    }
    impl LspBackend for StubBackend {
        fn env(&self) -> Env {
            Env::default()
        }

        fn typecheck_project(
            &self,
            _root: &std::path::Path,
            _initial_scope: Option<ArcStr>,
        ) -> anyhow::Result<TypecheckResult> {
            anyhow::bail!("stub backend can't typecheck")
        }

        fn buffer_overrides(&self) -> BufferOverrides {
            self.overrides.clone()
        }
    }

    /// `workspace_symbols` surfaces every top-level `let`/`type`/`mod`
    /// from a `.gx` and `val`/`type`/`mod` from a `.gxi`, scoped to the
    /// active document's project.
    #[test]
    fn workspace_symbols_returns_project_files() {
        let dir = tempfile::tempdir().unwrap();
        let root = dir.path();
        let main_gx = root.join("main.gx");
        let lib_gxi = root.join("lib.gxi");
        let lib_gx = root.join("lib.gx");
        std::fs::write(&main_gx, "mod lib;\nlet helper = |x: i64| -> i64 x + 1;\n")
            .unwrap();
        std::fs::write(&lib_gxi, "val widgetize: fn(n: i64) -> i64;\n").unwrap();
        std::fs::write(&lib_gx, "let widgetize = |n: i64| -> i64 n;\n").unwrap();

        let backend: std::sync::Arc<dyn LspBackend> = StubBackend::new();
        let mut state = ServerState::new(backend, false, PositionEncoding::Utf16);
        state.workspace_roots = vec![root.to_path_buf()];
        state.workspace = scan(&state.workspace_roots);
        // `main.gx` as the active doc picks its project.
        state.last_active_uri = path_to_uri(&main_gx).map(|u| u);

        // Empty query returns everything.
        let all = state.workspace_symbols("");
        let names: Vec<&str> = all.iter().map(|s| s.name.as_str()).collect();
        assert!(names.contains(&"helper"), "missing helper, got {names:?}");
        assert!(names.contains(&"lib"), "missing mod lib, got {names:?}");
        assert!(names.contains(&"widgetize"), "missing widgetize, got {names:?}");

        // Filtered query.
        let some = state.workspace_symbols("widget");
        let names: Vec<&str> = some.iter().map(|s| s.name.as_str()).collect();
        assert!(names.contains(&"widgetize"));
        assert!(!names.contains(&"helper"));
    }
}
