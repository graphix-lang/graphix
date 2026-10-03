//! IDE/LSP side-channels: write-only sinks the compiler fills while
//! `Env.ide` holds one (an LSP-style check), drained at the compile
//! boundary into the check result; a few recorders run under any
//! [`IdeMode::Lsp`]. Nothing here is read by the compiler itself.
//! [`Ide`] owns all of them, shared via `Env.ide`.

use crate::{
    BindId, SourcePosition,
    env::{Bind, Env},
    expr::{ModPath, Origin, WrittenPath},
    typ::Type,
};
use ahash::AHashSet;
use arcstr::ArcStr;
use compact_str::CompactString;
use parking_lot::Mutex;
use poolshark::global::{GPooled, Pool};
use std::sync::LazyLock;
use triomphe::Arc;

/// A name occurrence the compiler resolved to a `BindId`. `def_pos` and
/// `def_ori` snapshot the declaration site, because some bindings
/// (lambda parameters) leave the env before tooling asks.
#[derive(Debug, Clone)]
pub struct ReferenceSite {
    pub pos: SourcePosition,
    pub ori: Arc<Origin>,
    pub name: ModPath,
    pub bind_id: BindId,
    pub def_pos: SourcePosition,
    pub def_ori: Arc<Origin>,
}

/// A `mod foo;` declaration, at the name, or one item of a `use`, at
/// the statement. For `mod foo;`, `def_ori` is the file the body was
/// loaded from.
#[derive(Debug, Clone)]
pub struct ModuleRefSite {
    pub pos: SourcePosition,
    pub ori: Arc<Origin>,
    /// Module name as the user wrote it (might be relative).
    pub name: ModPath,
    /// Absolute module path the compiler resolved this reference to.
    pub canonical: ModPath,
    /// Origin of the module's body; `None` for `use` sites.
    pub def_ori: Option<Arc<Origin>>,
    /// A `use` item: where each segment of `name` stands (a group
    /// shares its prefix's). `None` for `mod`.
    pub segments: Option<WrittenPath>,
}

/// The compiler compiled the `Expr` spanning `[pos, end)` in `scope`:
/// the module the expression stands in, not one it opens.
#[derive(Debug, Clone)]
pub struct ScopeMapEntry {
    pub pos: SourcePosition,
    pub end: SourcePosition,
    pub ori: Arc<Origin>,
    pub scope: ModPath,
}

/// A type-name occurrence (`Foo` in `let x: Foo`); `def_pos`/`def_ori`
/// point at the typedef.
#[derive(Debug, Clone)]
pub struct TypeRefSite {
    pub pos: SourcePosition,
    pub ori: Arc<Origin>,
    /// The name as written in source (e.g. `Result`, `array::Foo`).
    pub name: ModPath,
    /// Canonical scope of the typedef the reference resolved to.
    pub canonical_scope: ModPath,
    pub def_pos: SourcePosition,
    pub def_ori: Arc<Origin>,
}

/// A field selected from a struct (`s.f`): where the field's name
/// stands and what the field is.
#[derive(Debug, Clone)]
pub struct FieldRefSite {
    pub pos: SourcePosition,
    pub ori: Arc<Origin>,
    pub name: ArcStr,
    pub typ: Type,
}

/// Something the check accepted and the author should still hear
/// about, over the text `[pos, end)`.
#[derive(Debug, Clone)]
pub struct Warning {
    pub pos: SourcePosition,
    pub end: SourcePosition,
    pub ori: Arc<Origin>,
    pub message: ArcStr,
}

/// Links a `.gxi` `val foo: T` declaration to its `let foo = …`
/// implementation in the paired `.gx`.
#[derive(Debug, Clone)]
pub struct SigImplLink {
    pub scope: ModPath,
    pub name: CompactString,
    pub sig_id: BindId,
    pub impl_id: BindId,
}

/// Per-module snapshot of the impl-side env, where implementation
/// bindings shadow sig proxies.
#[derive(Debug, Clone)]
pub struct ModuleInternalView {
    pub scope: ModPath,
    pub env: Env,
}

/// One checked expression's type: the node's written span and its type
/// snapshot (`Type::resolve_tvars`), so nothing unified after the check
/// moves it. Recorded only when a check asks for types.
#[derive(Debug, Clone)]
pub struct ExprTypeSite {
    pub ori: Arc<Origin>,
    pub pos: SourcePosition,
    /// `None` for an expression the parser did not write.
    pub end: Option<SourcePosition>,
    pub typ: Type,
    /// The node's own type is a cell its uses decided (a `let` over ⊥),
    /// `typ` what the cell resolved to.
    pub cell: bool,
}

/// Whether a compile serves an editor, and the sink it records into.
#[derive(Debug, Clone, Default)]
pub enum IdeMode {
    #[default]
    Off,
    /// An editor's runtime: fusion off, unknown builtins warn. The sink
    /// is `Some` during a check, and every compile within it drains into
    /// the one buffer the clones share.
    Lsp(Option<Arc<Mutex<Ide>>>),
}

impl IdeMode {
    pub fn is_lsp(&self) -> bool {
        matches!(self, Self::Lsp(_))
    }

    /// A compile task's mode: a sink of its own, which [`Self::join`]
    /// appends to this one's in order, whatever order the tasks ran in.
    pub(crate) fn fork(&self) -> Self {
        match self {
            Self::Lsp(Some(_)) => Self::Lsp(Some(Arc::new(Mutex::new(Ide::new())))),
            mode => mode.clone(),
        }
    }

    pub(crate) fn join(&self, fork: Self) {
        if let (Some(sink), Self::Lsp(Some(task))) = (self.sink(), fork) {
            sink.lock().append(&mut task.lock())
        }
    }

    pub fn sink(&self) -> Option<&Arc<Mutex<Ide>>> {
        match self {
            Self::Lsp(sink) => sink.as_ref(),
            Self::Off => None,
        }
    }
}

/// Every IDE/LSP side-channel accumulated during a compile. Installed
/// into `Env.ide` only under an LSP-style check.
#[derive(Debug)]
pub struct Ide {
    /// Every binding the check created, in order, including the
    /// short-lived ones (lambda parameters, block lets, pattern binds)
    /// the env has dropped by the time tooling asks.
    pub binds: GPooled<Vec<Bind>>,
    /// Resolved name references (`textDocument/references`,
    /// `textDocument/definition`).
    pub references: GPooled<Vec<ReferenceSite>>,
    /// Module references — `use foo;` and `mod foo;` mentions.
    pub module_references: GPooled<Vec<ModuleRefSite>>,
    /// Per-compile scope map answering `cursor → scope` queries.
    pub scope_map: GPooled<Vec<ScopeMapEntry>>,
    /// Type-name references in type positions, one per site.
    pub type_refs: GPooled<Vec<TypeRefSite>>,
    /// The sites in `type_refs`, by origin identity and position: a
    /// site is expanded at every check of its type.
    type_ref_sites: GPooled<AHashSet<(usize, i32, i32)>>,
    /// Struct field selections.
    pub field_refs: GPooled<Vec<FieldRefSite>>,
    /// Warnings, in place of the stderr lines a run prints.
    pub warnings: GPooled<Vec<Warning>>,
    /// `val`-sig ↔ `let`-impl bind links.
    pub sig_links: GPooled<Vec<SigImplLink>>,
    /// Per-module impl-side env snapshots.
    pub module_internals: GPooled<Vec<ModuleInternalView>>,
    /// Every checked node's type outside lambda bodies, when the check
    /// asked for them (`record_expr_types`).
    pub expr_types: GPooled<Vec<ExprTypeSite>>,
}

impl Ide {
    /// Fresh, empty sinks from the pools.
    pub fn new() -> Self {
        static BIND_POOL: LazyLock<Pool<Vec<Bind>>> =
            LazyLock::new(|| Pool::new(64, 65536));
        static REFERENCE_SITE_POOL: LazyLock<Pool<Vec<ReferenceSite>>> =
            LazyLock::new(|| Pool::new(64, 65536));
        static MODULE_REF_SITE_POOL: LazyLock<Pool<Vec<ModuleRefSite>>> =
            LazyLock::new(|| Pool::new(64, 65536));
        static SCOPE_MAP_ENTRY_POOL: LazyLock<Pool<Vec<ScopeMapEntry>>> =
            LazyLock::new(|| Pool::new(64, 65536));
        static TYPE_REF_SITE_POOL: LazyLock<Pool<Vec<TypeRefSite>>> =
            LazyLock::new(|| Pool::new(64, 65536));
        static TYPE_REF_SEEN_POOL: LazyLock<Pool<AHashSet<(usize, i32, i32)>>> =
            LazyLock::new(|| Pool::new(64, 65536));
        static FIELD_REF_SITE_POOL: LazyLock<Pool<Vec<FieldRefSite>>> =
            LazyLock::new(|| Pool::new(64, 65536));
        static WARNING_POOL: LazyLock<Pool<Vec<Warning>>> =
            LazyLock::new(|| Pool::new(64, 4096));
        static SIG_LINK_POOL: LazyLock<Pool<Vec<SigImplLink>>> =
            LazyLock::new(|| Pool::new(32, 4096));
        static MODULE_INTERNAL_VIEW_POOL: LazyLock<Pool<Vec<ModuleInternalView>>> =
            LazyLock::new(|| Pool::new(32, 4096));
        static EXPR_TYPE_SITE_POOL: LazyLock<Pool<Vec<ExprTypeSite>>> =
            LazyLock::new(|| Pool::new(32, 65536));
        Self {
            binds: BIND_POOL.take(),
            references: REFERENCE_SITE_POOL.take(),
            module_references: MODULE_REF_SITE_POOL.take(),
            scope_map: SCOPE_MAP_ENTRY_POOL.take(),
            type_refs: TYPE_REF_SITE_POOL.take(),
            type_ref_sites: TYPE_REF_SEEN_POOL.take(),
            field_refs: FIELD_REF_SITE_POOL.take(),
            warnings: WARNING_POOL.take(),
            sig_links: SIG_LINK_POOL.take(),
            module_internals: MODULE_INTERNAL_VIEW_POOL.take(),
            expr_types: EXPR_TYPE_SITE_POOL.take(),
        }
    }

    /// Move every record of `other` after this one's.
    fn append(&mut self, other: &mut Ide) {
        let Self {
            binds,
            references,
            module_references,
            scope_map,
            type_refs,
            type_ref_sites: _,
            field_refs,
            warnings,
            sig_links,
            module_internals,
            expr_types,
        } = other;
        self.binds.append(binds);
        self.references.append(references);
        self.module_references.append(module_references);
        self.scope_map.append(scope_map);
        for site in type_refs.drain(..) {
            self.push_type_ref(site)
        }
        self.field_refs.append(field_refs);
        self.warnings.append(warnings);
        self.sig_links.append(sig_links);
        self.module_internals.append(module_internals);
        self.expr_types.append(expr_types);
    }

    pub(crate) fn push_type_ref(&mut self, site: TypeRefSite) {
        let key = (Arc::as_ptr(&site.ori) as usize, site.pos.line, site.pos.column);
        if self.type_ref_sites.insert(key) {
            self.type_refs.push(site)
        }
    }
}

impl Default for Ide {
    fn default() -> Self {
        Self::new()
    }
}
