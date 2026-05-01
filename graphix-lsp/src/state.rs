use crate::diagnostics::{error_leaf_message, error_location, error_to_diagnostics};
use arcstr::ArcStr;
use graphix_compiler::{
    env::Env,
    expr::{ModPath, Source},
    typ::{FnArgKind, FnType, Type},
    ModuleRefSite, ReferenceSite, ScopeMapEntry, SourcePosition, TypeRefSite,
    MODULE_REF_SITE_POOL, REFERENCE_SITE_POOL, SCOPE_MAP_ENTRY_POOL, TYPE_REF_SITE_POOL,
};
use lsp_types::Uri;
use poolshark::{global::GPooled, local::LPooled};
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
    /// The compiler environment as it would be after type-checking
    /// this document. `Some` after a successful check, `None` after a
    /// failed one (so the env doesn't drift away from the user's
    /// current source).
    pub env: Option<Env>,
    /// Resolved name reference sites observed during the most
    /// recent successful check, used to answer
    /// `textDocument/references` and prepareRename.
    pub references: GPooled<Vec<ReferenceSite>>,
    /// Module reference sites (`use foo;`, `mod foo;`).
    pub module_references: GPooled<Vec<ModuleRefSite>>,
    /// Type-name reference sites (uses of `Foo` in type positions).
    pub type_references: GPooled<Vec<TypeRefSite>>,
    /// Per-Expr scope map from the latest compile. Used to answer
    /// cursor → scope queries.
    pub scope_map: GPooled<Vec<ScopeMapEntry>>,
}

impl Document {
    pub fn new(text: String, version: i32) -> Self {
        Self {
            version,
            text,
            env: None,
            references: REFERENCE_SITE_POOL.take(),
            module_references: MODULE_REF_SITE_POOL.take(),
            type_references: TYPE_REF_SITE_POOL.take(),
            scope_map: SCOPE_MAP_ENTRY_POOL.take(),
        }
    }
}

/// Result of a backend typecheck. Mirrors `graphix_rt::CheckResult`
/// but doesn't depend on graphix-rt directly.
pub struct TypecheckResult {
    pub env: Env,
    pub references: GPooled<Vec<ReferenceSite>>,
    pub module_references: GPooled<Vec<ModuleRefSite>>,
    pub type_references: GPooled<Vec<TypeRefSite>>,
    pub scope_map: GPooled<Vec<ScopeMapEntry>>,
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
pub struct ProjectResult {
    pub env: Env,
    pub references: GPooled<Vec<ReferenceSite>>,
    pub module_references: GPooled<Vec<ModuleRefSite>>,
    pub type_references: GPooled<Vec<TypeRefSite>>,
    pub scope_map: GPooled<Vec<ScopeMapEntry>>,
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
    /// URIs we published non-empty diagnostics for in the last
    /// project recheck. Used to publish empty diagnostics on the
    /// next cycle for files that recovered, so editors don't show
    /// stale red squiggles.
    pub last_project_diag_uris: HashSet<Uri>,
    /// Whether the client advertised `completionItem.snippetSupport`.
    /// Drives whether function completions emit a snippet body that
    /// expands `name(${1}, ${2})$0` on accept.
    pub snippet_support: bool,
}

impl ServerState {
    pub fn new(backend: Arc<dyn LspBackend>, snippet_support: bool) -> Self {
        let env = backend.env();
        Self {
            env,
            documents: HashMap::new(),
            backend,
            workspace_roots: Vec::new(),
            workspace: Default::default(),
            project_results: Vec::new(),
            last_project_diag_uris: HashSet::new(),
            snippet_support,
        }
    }

    /// Tell the state about the editor's workspace folders. Triggers
    /// an initial scan and project compile, returning the
    /// per-file diagnostics so the caller can publish them.
    pub fn set_workspace_roots(
        &mut self,
        roots: Vec<PathBuf>,
    ) -> HashMap<Uri, Vec<lsp_types::Diagnostic>> {
        self.workspace_roots = roots;
        self.recheck_workspace()
    }

    /// Re-scan the workspace and re-typecheck every project from
    /// disk. Throws away the previous project state and replaces it
    /// wholesale.
    ///
    /// Returns a (uri → diagnostics) map covering both errored
    /// projects (with the diagnostic attributed to the file the
    /// error originated in) and previously-erroring files that now
    /// compile cleanly (returned with an empty diagnostic list so
    /// callers can clear stale squiggles).
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
        self.workspace = crate::workspace::scan(&self.workspace_roots);
        let mut results: Vec<Option<ProjectResult>> =
            Vec::with_capacity(self.workspace.projects.len());
        let mut diags_by_uri: HashMap<Uri, Vec<lsp_types::Diagnostic>> =
            HashMap::new();
        for project in &self.workspace.projects {
            let r = self.backend.typecheck_project(&project.root);
            match r {
                Ok(TypecheckResult {
                    env,
                    references,
                    module_references,
                    type_references,
                    scope_map,
                }) => {
                    results.push(Some(ProjectResult {
                        env,
                        references,
                        module_references,
                        type_references,
                        scope_map,
                    }));
                }
                Err(e) => {
                    let (uri, diag) =
                        project_error_to_diagnostic(&e, &project.root);
                    diags_by_uri.entry(uri).or_default().push(diag);
                    results.push(None);
                }
            }
        }
        self.project_results = results;
        // Compose the publish set: every URI we have diagnostics for
        // this cycle plus every URI that had diagnostics last cycle but
        // doesn't anymore (so we can publish empty to clear).
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
            Ok(TypecheckResult {
                env,
                references,
                module_references,
                type_references,
                scope_map,
            }) => {
                if let Some(d) = self.documents.get_mut(uri) {
                    d.env = Some(env);
                    d.references = references;
                    d.module_references = module_references;
                    d.type_references = type_references;
                    d.scope_map = scope_map;
                }
                Vec::new()
            }
            Err(e) => {
                if let Some(d) = self.documents.get_mut(uri) {
                    d.env = None;
                    d.references = REFERENCE_SITE_POOL.take();
                    d.module_references = MODULE_REF_SITE_POOL.take();
                    d.type_references = TYPE_REF_SITE_POOL.take();
                    d.scope_map = SCOPE_MAP_ENTRY_POOL.take();
                }
                error_to_diagnostics(&e, &text)
            }
        }
    }

    /// Find the lexical scope at `position` in `uri` using the
    /// compiler-emitted scope map. Picks the entry with the greatest
    /// `pos` ≤ cursor in the same file as the requesting URI. Walks
    /// the active doc first (most-fresh data), then any project's
    /// scope map. Returns root scope if nothing matches — that's
    /// the natural default at top of file or in unparsed regions.
    pub fn scope_at(&self, uri: &Uri, position: lsp_types::Position) -> ModPath {
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
            for e in doc.scope_map.iter() {
                consider(e);
            }
        }
        for r in &self.project_results {
            if let Some(r) = r {
                for e in r.scope_map.iter() {
                    consider(e);
                }
            }
        }
        best.map(|e| e.scope.lexical).unwrap_or_else(ModPath::root)
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
        let scope = self.scope_at(uri, position);
        let env = self.env_for(uri);
        let mut items = Vec::new();
        // Special-case: cursor is inside `#…` — the user is naming a
        // labeled arg, not picking from the global namespace. Emit only
        // the callee's labeled args (filtered by what's been typed) and
        // wire up a text_edit so accepting the completion replaces the
        // typed `#…` instead of appending to it.
        if let Some(label_ctx) = label_prefix(&doc.text, position) {
            if let Some(callee) = call_context(&doc.text, position) {
                let basename =
                    callee.rsplit("::").next().unwrap_or(&callee).to_string();
                let callee_path = modpath_from_typed(&callee);
                for (name, bind) in lookup_matching_via_by_id(env, &scope, &callee_path)
                {
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
        // Use the compiler-emitted scope map to find what scope the
        // cursor is in. This makes lambda parameters, let bindings,
        // and other locally-scoped names visible in completion.
        let part = modpath_from_typed(&prefix);
        let matched = lookup_matching_via_by_id(env, &scope, &part);
        // If the cursor sits inside an open call's argument list, prepend
        // the callee's labeled args as completion items so users discover
        // optional named params.
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
        // Track binding labels so we can hide a same-named module entry
        // below — when a name is both a function (via `use foo;`) and
        // the module it came from, the function form is more useful in
        // a completion popup. The module is still reachable via
        // qualified-path completion (`column::Column`).
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
        // Module completions still use the env's `modules` set.
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
        let doc = self.documents.get(uri)?;
        let env = self.env_for(uri);

        // First, try resolving via a recorded reference site at the
        // cursor. This is the only path that works for bindings that
        // aren't reachable by name from the cursor's scope — lambda
        // parameters, lets inside nested blocks, and the variable
        // tokens inside string interpolations all show up here.
        for r in doc.references.iter() {
            if position_in_ref(position, r) {
                if let Some(bind) = bind_for_id(env, r.bind_id) {
                    return Some(bind_hover(&r.name.to_string(), bind));
                }
            }
        }

        // Cursor on a binding's declaration (lambda param, let name,
        // etc.). Declarations don't appear in `doc.references`, so
        // scan ide_binds for a Bind whose `pos` covers the cursor.
        if let Some(bind) = bind_at_decl(env, uri, position) {
            return Some(bind_hover(bind.name.as_str(), bind));
        }

        let word = get_word_at_position(&doc.text, position)?;
        // Use the cursor's lexical scope so locally-bound names
        // (let inside a function, etc.) are resolved correctly.
        let scope = self.scope_at(uri, position);
        let name: ModPath = word.split("::").collect();

        if let Some((_, bind)) = env.lookup_bind(&scope, &name) {
            return Some(bind_hover(&word, bind));
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
        // Module references: if the cursor is on a module name, also
        // collect every `use foo;` / `mod foo;` site that resolved to
        // the same canonical path.
        if let Some(canonical) = self.canonical_module_at(uri, position) {
            self.collect_module_refs(&canonical, uri, &mut locs);
        }
        // Type references: if the cursor is on a type name, find
        // every site that resolved to the same canonical (scope, name).
        // The compiler captures both lookup_ref derefs and the walk
        // of typedef bodies, so we can match precisely.
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
        for r in doc.references.iter() {
            if position_in_ref(position, r) {
                return Some(r.name.clone());
            }
        }
        None
    }

    /// If the cursor sits on a recorded reference site, return the
    /// declaration site (`def_pos`, `def_ori`) recorded on it. The
    /// ReferenceSite captures this at resolution time, so this
    /// works even for bindings that have since been removed from
    /// the env (e.g. lambda parameters tied to a single callsite).
    /// If the cursor is on a `use foo;` / `mod foo;` site, return
    /// the canonical module path it resolved to.
    fn canonical_module_at(
        &self,
        uri: &Uri,
        position: lsp_types::Position,
    ) -> Option<ModPath> {
        let doc = self.documents.get(uri)?;
        for m in doc.module_references.iter() {
            if position_in_module_ref(position, m) {
                return Some(m.canonical.clone());
            }
        }
        None
    }

    /// Walk active doc + every project's module_references, yielding
    /// locations for every entry whose canonical path matches.
    fn collect_module_refs(
        &self,
        canonical: &ModPath,
        requesting_uri: &Uri,
        out: &mut Vec<lsp_types::Location>,
    ) {
        let push = |m: &ModuleRefSite, out: &mut Vec<lsp_types::Location>| {
            if let Some(loc) = ref_to_location(requesting_uri, &m.ori, m.pos) {
                out.push(loc);
            }
        };
        if let Some(doc) = self.documents.get(requesting_uri) {
            for m in doc.module_references.iter() {
                if &m.canonical == canonical {
                    push(m, out);
                }
            }
        }
        for r in &self.project_results {
            if let Some(r) = r {
                for m in r.module_references.iter() {
                    if &m.canonical == canonical {
                        push(m, out);
                    }
                }
            }
        }
    }

    /// If the cursor is on a recorded TypeRefSite, return the
    /// canonical (scope, name) of the typedef it resolved to.
    fn canonical_typedef_at(
        &self,
        uri: &Uri,
        position: lsp_types::Position,
    ) -> Option<(ModPath, ModPath)> {
        let doc = self.documents.get(uri)?;
        for t in doc.type_references.iter() {
            if position_in_type_ref(position, t) {
                return Some((t.canonical_scope.clone(), t.name.clone()));
            }
        }
        for r in &self.project_results {
            if let Some(r) = r {
                for t in r.type_references.iter() {
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
            if let Some(loc) = ref_to_location(requesting_uri, &t.ori, t.pos) {
                out.push(loc);
            }
        };
        if let Some(doc) = self.documents.get(requesting_uri) {
            for t in doc.type_references.iter() {
                if &t.canonical_scope == canonical_scope && &t.name == name {
                    push(t, out);
                }
            }
        }
        for r in &self.project_results {
            if let Some(r) = r {
                for t in r.type_references.iter() {
                    if &t.canonical_scope == canonical_scope && &t.name == name {
                        push(t, out);
                    }
                }
            }
        }
    }

    /// Look up the typedef declaration site for a given canonical
    /// (scope, name). Walks active doc and projects; any TypeRefSite
    /// with matching canonical info has the same def_pos/def_ori, so
    /// any match works.
    fn typedef_decl_location(
        &self,
        canonical_scope: &ModPath,
        name: &ModPath,
        requesting_uri: &Uri,
    ) -> Option<lsp_types::Location> {
        let take = |t: &TypeRefSite| ref_to_location(requesting_uri, &t.def_ori, t.def_pos);
        if let Some(doc) = self.documents.get(requesting_uri) {
            for t in doc.type_references.iter() {
                if &t.canonical_scope == canonical_scope && &t.name == name {
                    return take(t);
                }
            }
        }
        for r in &self.project_results {
            if let Some(r) = r {
                for t in r.type_references.iter() {
                    if &t.canonical_scope == canonical_scope && &t.name == name {
                        return take(t);
                    }
                }
            }
        }
        None
    }

    /// If the cursor is on a recorded type-reference site, return
    /// the typedef's declaration location.
    fn type_definition_at(
        &self,
        uri: &Uri,
        position: lsp_types::Position,
    ) -> Option<lsp_types::Location> {
        let doc = self.documents.get(uri)?;
        for t in doc.type_references.iter() {
            if position_in_type_ref(position, t) {
                return ref_to_location(uri, &t.def_ori, t.def_pos);
            }
        }
        // Also try any project's type_references (cross-file).
        for r in &self.project_results {
            if let Some(r) = r {
                for t in r.type_references.iter() {
                    // Match by file ori source equality on the use side
                    // (the use site lives in this URI).
                    if origin_matches_uri(&t.ori, uri)
                        && position_in_type_ref(position, t)
                    {
                        return ref_to_location(uri, &t.def_ori, t.def_pos);
                    }
                }
            }
        }
        None
    }

    fn def_site_at_position(
        &self,
        uri: &Uri,
        position: lsp_types::Position,
    ) -> Option<(SourcePosition, graphix_compiler::expr::Origin)> {
        let doc = self.documents.get(uri)?;
        for r in doc.references.iter() {
            if position_in_ref(position, r) {
                return Some((r.def_pos, graphix_compiler::expr::Origin::clone(&r.def_ori)));
            }
        }
        None
    }

    /// If the cursor is on a `use foo;` or `mod foo;` site, return
    /// the file the module body lives in (for `mod` decls the
    /// resolver gives us a file directly; for `use` we look across
    /// the workspace for any `mod` decl with the same canonical
    /// path).
    fn module_definition_at(
        &self,
        uri: &Uri,
        position: lsp_types::Position,
    ) -> Option<lsp_types::Location> {
        let doc = self.documents.get(uri)?;
        let target = doc
            .module_references
            .iter()
            .find(|m| position_in_module_ref(position, m))?;
        // Prefer this site's own def_ori if the resolver attached one.
        if let Some(ori) = target.def_ori.as_ref() {
            return module_origin_to_location(uri, ori);
        }
        // Otherwise look at any other module ref with the same
        // canonical path that DOES have a def_ori — likely the
        // `mod foo;` declaration in the project's main file.
        let canonical = &target.canonical;
        let mut search: LPooled<Vec<&ModuleRefSite>> = LPooled::take();
        search.extend(doc.module_references.iter());
        for r in &self.project_results {
            if let Some(r) = r {
                search.extend(r.module_references.iter());
            }
        }
        search
            .iter()
            .copied()
            .find(|m| &m.canonical == canonical && m.def_ori.is_some())
            .and_then(|m| {
                module_origin_to_location(uri, m.def_ori.as_ref().unwrap())
            })
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
        // First try the references list — each ReferenceSite carries
        // the bind's declaration site. This works for all bindings,
        // including lambda parameters that the env's by_id map no
        // longer remembers (their callsite was deleted post-typecheck).
        if let Some((def_pos, def_ori)) =
            self.def_site_at_position(uri, position)
        {
            return ref_to_location(uri, &def_ori, def_pos);
        }
        // Try module references — `use foo;` and `mod foo;` sites
        // carry the file the module body lives in.
        if let Some(loc) = self.module_definition_at(uri, position) {
            return Some(loc);
        }
        // Try type references — `Foo` in `let x: Foo` resolves to a
        // typedef; jump to its declaration.
        if let Some(loc) = self.type_definition_at(uri, position) {
            return Some(loc);
        }
        // Fall back to env lookup — this catches the cursor sitting
        // directly on the binding name (where there's no
        // ReferenceSite for it but the bind is reachable via name).
        let doc = self.documents.get(uri)?;
        let word = get_word_at_position(&doc.text, position)?;
        let scope = ModPath::root();
        let name: ModPath = word.split("::").collect();
        let env = self.env_for(uri);
        if let Some((_, bind)) = env.lookup_bind(&scope, &name) {
            let target_uri = match &bind.ori.source {
                Source::File(p) => path_to_uri(p)?,
                Source::Internal(_) | Source::Unspecified => uri.clone(),
                Source::Netidx(_) => return None,
            };
            let line = bind.pos.line.saturating_sub(1).max(0) as u32;
            let column = bind.pos.column.saturating_sub(1).max(0) as u32;
            let pos = lsp_types::Position { line, character: column };
            return Some(lsp_types::Location {
                uri: target_uri,
                range: lsp_types::Range { start: pos, end: pos },
            });
        }
        // Last fallback: cursor on a typedef name with no recorded
        // ReferenceSite (e.g. cursor right on `Foo` in
        // `type Foo = …` itself).
        let typedef = env.lookup_typedef(&scope, &name)?;
        let target_uri = match &typedef.ori.source {
            Source::File(p) => path_to_uri(p)?,
            Source::Internal(_) | Source::Unspecified => uri.clone(),
            Source::Netidx(_) => return None,
        };
        let line = typedef.pos.line.saturating_sub(1).max(0) as u32;
        let column = typedef.pos.column.saturating_sub(1).max(0) as u32;
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

/// Turn a project compile error into a (uri, diagnostic) pair. The
/// uri is the file the error originated in (extracted from the
/// chain) — falling back to the project root if attribution failed.
fn project_error_to_diagnostic(
    err: &anyhow::Error,
    project_root: &Path,
) -> (Uri, lsp_types::Diagnostic) {
    let loc = error_location(err);
    let target_path = loc.file.unwrap_or_else(|| project_root.to_path_buf());
    let uri = path_to_uri(&target_path).unwrap_or_else(|| {
        // Path → URI conversion shouldn't fail for absolute paths,
        // but if it does we fall back to the project root URI rather
        // than dropping the diagnostic on the floor.
        path_to_uri(&project_root.to_path_buf())
            .or_else(|| Uri::from_str("file:///").ok())
            .expect("file:/// is a valid URI")
    });
    let pos = loc.position.unwrap_or_default();
    let diag = lsp_types::Diagnostic {
        range: lsp_types::Range { start: pos, end: pos },
        severity: Some(lsp_types::DiagnosticSeverity::ERROR),
        source: Some("graphix".to_string()),
        message: error_leaf_message(err),
        ..Default::default()
    };
    (uri, diag)
}

/// Char count of `name` as it would be rendered by `Display` (e.g.
/// `array::map` → 10), computed without allocating a String.
fn modpath_display_chars(name: &ModPath) -> u32 {
    use netidx::path::Path as NPath;
    use std::borrow::Borrow;
    let s: &str = name.borrow();
    let levels = NPath::levels(s);
    let parts: usize = NPath::parts(s).map(|p| p.chars().count()).sum();
    (parts + 2 * levels.saturating_sub(1)) as u32
}

/// Check whether a 0-indexed LSP position falls inside a reference
/// site's textual span. We use the printed length of the name as a
/// rough span — `array::map` covers 10 characters from `pos`.
fn position_in_ref(pos: lsp_types::Position, r: &ReferenceSite) -> bool {
    span_covers(pos, r.pos, modpath_display_chars(&r.name))
}

/// Same idea for a module reference. The pos points at the `mod` or
/// `use` keyword, so we extend the span to cover the keyword + name.
fn position_in_module_ref(pos: lsp_types::Position, m: &ModuleRefSite) -> bool {
    // The keyword is "use" or "mod" (3 chars) + 1 space + name length.
    // Falls down on `use   foo;` (multi-space) — pessimistic, not wrong.
    span_covers(pos, m.pos, 4 + modpath_display_chars(&m.name))
}

/// Type references record the position of the type name itself.
fn position_in_type_ref(pos: lsp_types::Position, t: &TypeRefSite) -> bool {
    span_covers(pos, t.pos, modpath_display_chars(&t.name))
}

/// True if the given Origin's source path matches the requesting URI.
/// Internal/Unspecified always match (they're the active doc's own
/// content); Netidx never does.
fn origin_matches_uri(ori: &graphix_compiler::expr::Origin, uri: &Uri) -> bool {
    match &ori.source {
        Source::File(p) => match path_to_uri(p) {
            Some(u) => &u == uri,
            None => false,
        },
        // The LSP feeds the active document as `Source::Internal(text)`,
        // and the VFS resolver wraps every stdlib module in
        // `Source::Internal(name)` too. Both look the same on `source`
        // alone, so we use `parent` to discriminate: only the document
        // the LSP passed to `check_with_resolvers` has `parent = None`.
        // VFS- (and File-) loaded children all carry `Some(parent_ori)`.
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

/// Map a module reference's `def_ori` (file the module body was
/// loaded from) to an LSP Location pointing at the file's start.
fn module_origin_to_location(
    requesting_uri: &Uri,
    ori: &graphix_compiler::expr::Origin,
) -> Option<lsp_types::Location> {
    let target_uri = match &ori.source {
        Source::File(p) => path_to_uri(p)?,
        Source::Internal(_) | Source::Unspecified => requesting_uri.clone(),
        Source::Netidx(_) => return None,
    };
    let pos = lsp_types::Position { line: 0, character: 0 };
    Some(lsp_types::Location {
        uri: target_uri,
        range: lsp_types::Range { start: pos, end: pos },
    })
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

/// Like `Env::lookup_matching` but walks `env.by_id` directly so
/// that bindings whose lexical entries were dropped at scope
/// teardown (e.g. lambda parameters) are still visible to IDE
/// queries. A bind is "visible" from `cursor_scope` if its scope is
/// `cursor_scope` itself or any ancestor.
/// Like `Env::lookup_matching` but walks `env.ide_binds` so
/// short-lived bindings (lambda params, let bindings inside
/// scopes that get torn down) are still visible to IDE queries.
/// A bind is visible from `cursor_scope` if the bind's scope is
/// `cursor_scope` itself, an ancestor, or a sibling-via-`use`.
///
/// Goes through `env.lookup_matching` which walks the scope's `used`
/// list via `find_visible` — that's how names brought in by `use foo;`
/// become reachable. Walking `ide_binds` directly (as we used to)
/// missed those.
fn lookup_matching_via_by_id(
    env: &Env,
    cursor_scope: &ModPath,
    part: &ModPath,
) -> Vec<(compact_str::CompactString, graphix_compiler::env::Bind)> {
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

/// Build an LSP snippet body for a function completion. Each required
/// arg becomes a `${N:placeholder}` tab-stop. The placeholder is the
/// arg's source-level name (`FnArgType::name`) when present; otherwise
/// it falls back to `a0`, `a1`, … for positional args and the label
/// name for labeled args. Labeled args with defaults are skipped —
/// they're offered separately when the cursor is inside the call. The
/// final `$0` lands the cursor outside the parens after the user tabs
/// through the placeholders.
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

/// If the cursor sits inside an open `(`'s argument list, return the
/// callee path (e.g. `foo`, `array::map`). Returns `None` if the cursor
/// isn't inside a call, or the enclosing bracket is `[`/`{`, or the
/// scan hits a statement boundary first.
///
/// String literals are not parsed — a stray `(` inside a string can
/// produce a false positive in pathological cases, but the common case
/// (calls in code) is handled correctly.
fn call_context(text: &str, position: lsp_types::Position) -> Option<String> {
    let mut chars: LPooled<Vec<char>> = LPooled::take();
    chars.extend(text.chars());
    // Translate (line, character) into a byte/char offset in the flat
    // char vector. We treat character as a UTF-16 code unit count
    // (default LSP encoding) but for ASCII-heavy graphix source this
    // is equivalent to char count.
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
                    // Found the enclosing open-paren. Walk back over
                    // whitespace, then read the callee path.
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
/// When `replace` is `Some`, accepting a completion replaces that range
/// in the document — used when the user is mid-`#…` and we want to
/// substitute the typed `#` rather than insert another one. When
/// `None`, accepting just inserts at the cursor (used when labeled args
/// are offered passively alongside other completions).
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

/// Cursor-context returned by `label_prefix` — the `#`-prefixed token
/// the user is currently typing, plus the editable range that should
/// be replaced when a label completion is accepted.
struct LabelCtx {
    range: lsp_types::Range,
}

/// If the cursor sits inside a `#…` token (right after a `#`, or inside
/// the identifier following one), return the range of `#…` so a
/// `text_edit` can replace it cleanly. Returns `None` if the cursor
/// isn't in a label-completion position.
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
    // Don't trigger on `?#…` — that's only valid in fn type signatures
    // and an unrelated edit position.
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

/// Build the hover popup payload for a binding. Markdown body is a
/// graphix code fence with `name: type`, then any doc comment.
fn bind_hover(name: &str, bind: &graphix_compiler::env::Bind) -> lsp_types::Hover {
    let mut contents = format!("```graphix\n{}: {}\n```", name, format_bind_type(&bind.typ));
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

/// Find a binding whose declaration position covers the cursor.
/// Catches hovers on function parameters and let-binding names —
/// declarations don't appear in `doc.references`, but they're recorded
/// in `ide_binds` along with `(pos, ori, name)`. Filtering by the
/// active URI keeps multi-file workspace checks from cross-talking.
fn bind_at_decl<'a>(
    env: &'a Env,
    uri: &Uri,
    position: lsp_types::Position,
) -> Option<&'a graphix_compiler::env::Bind> {
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

/// Look up a bind by id, falling back to a linear scan of `ide_binds`.
///
/// `env.by_id` loses lambda parameters once their parent callsite is
/// dropped — but `unbind_variable` doesn't touch `ide_binds`, so the
/// IDE-only mirror still has the type and doc. ide_binds is keyed by
/// (scope, name) not by BindId, so we have to scan; n is small enough
/// at IDE speeds that this is fine.
fn bind_for_id(
    env: &Env,
    id: graphix_compiler::BindId,
) -> Option<&graphix_compiler::env::Bind> {
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

/// Format a binding's type for display (completion detail, hover, etc.).
///
/// Two passes happen here, and the order matters. `replace_auto_constrained`
/// (Fn types only) folds auto-named TVars from the function's constraint
/// table into the surface — this catches polymorphic functions whose
/// concrete arg types only appear as constraints, never written into the
/// TVar's storage. `resolve_tvars` then walks the whole type and
/// dereferences any TVar whose RwLock has been written by unification —
/// this catches non-function bindings (`let tbl = …`) and any inner TVars
/// the first pass left behind. resolve_tvars empties the constraint
/// table, so it must run second.
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
