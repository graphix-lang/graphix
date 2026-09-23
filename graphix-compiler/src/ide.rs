// CR claude for eric: [readability] The sinks fill whenever `Env.ide` is Some
// (every `Env::push_*`, `bind_variable`, `warn`); `lsp_mode` gates only some
// recorders. The doc names the wrong knob (see the lsp_mode CR in env.rs).
//! IDE/LSP side-channels: write-only sinks the compiler fills when
//! [`crate::env::Env::lsp_mode`] is set, drained at the compile
//! boundary into the check result. Nothing here is read by the
//! compiler itself. [`Ide`] owns all of them, shared via `Env.ide`.

// CR claude for eric: [style] `expr::Origin` (9 uses) and `expr::ModPath` (6)
// are spelled through the module; import them.
use crate::{
    BindId, Scope, SourcePosition,
    env::{Bind, Env},
    expr,
    typ::Type,
};
use arcstr::ArcStr;
use compact_str::CompactString;
use poolshark::global::{GPooled, Pool};
use std::sync::LazyLock;
use triomphe::Arc;

/// A name occurrence the compiler resolved to a `BindId`. `def_pos` and
/// `def_ori` snapshot the declaration site, because some bindings
/// (lambda parameters) leave the env before tooling asks.
#[derive(Debug, Clone)]
pub struct ReferenceSite {
    pub pos: SourcePosition,
    pub ori: Arc<expr::Origin>,
    pub name: expr::ModPath,
    pub bind_id: BindId,
    pub def_pos: SourcePosition,
    pub def_ori: Arc<expr::Origin>,
}

/// A `mod foo;` declaration, at the name, or one item of a `use`, at
/// the statement. For `mod foo;`, `def_ori` is the file the body was
/// loaded from.
#[derive(Debug, Clone)]
pub struct ModuleRefSite {
    pub pos: SourcePosition,
    pub ori: Arc<expr::Origin>,
    /// Module name as the user wrote it (might be relative).
    pub name: expr::ModPath,
    /// Absolute module path the compiler resolved this reference to.
    pub canonical: expr::ModPath,
    /// Origin of the module's body; `None` for `use` sites.
    pub def_ori: Option<Arc<expr::Origin>>,
    /// A `use` item: where each segment of `name` stands (a group
    /// shares its prefix's). `None` for `mod`.
    pub segments: Option<expr::WrittenPath>,
}

/// The compiler compiled the `Expr` spanning `[pos, end)` in `scope`:
/// the scope the expression stands in, not one it opens.
#[derive(Debug, Clone)]
pub struct ScopeMapEntry {
    pub pos: SourcePosition,
    pub end: SourcePosition,
    pub ori: Arc<expr::Origin>,
    pub scope: Scope,
}

/// A type-name occurrence (`Foo` in `let x: Foo`); `def_pos`/`def_ori`
/// point at the typedef.
#[derive(Debug, Clone)]
pub struct TypeRefSite {
    pub pos: SourcePosition,
    pub ori: Arc<expr::Origin>,
    /// The name as written in source (e.g. `Result`, `array::Foo`).
    pub name: expr::ModPath,
    /// Canonical scope of the typedef the reference resolved to.
    pub canonical_scope: expr::ModPath,
    pub def_pos: SourcePosition,
    pub def_ori: Arc<expr::Origin>,
}

/// A field selected from a struct (`s.f`): where the field's name
/// stands and what the field is.
#[derive(Debug, Clone)]
pub struct FieldRefSite {
    pub pos: SourcePosition,
    pub ori: Arc<expr::Origin>,
    pub name: ArcStr,
    pub typ: Type,
}

/// Something the check accepted and the author should still hear
/// about, over the text `[pos, end)`.
#[derive(Debug, Clone)]
pub struct Warning {
    pub pos: SourcePosition,
    pub end: SourcePosition,
    pub ori: Arc<expr::Origin>,
    pub message: ArcStr,
}

/// Links a `.gxi` `val foo: T` declaration to its `let foo = …`
/// implementation in the paired `.gx`.
#[derive(Debug, Clone)]
pub struct SigImplLink {
    pub scope: expr::ModPath,
    pub name: CompactString,
    pub sig_id: BindId,
    pub impl_id: BindId,
}

/// Per-module snapshot of the impl-side env, where implementation
/// bindings shadow sig proxies.
#[derive(Debug, Clone)]
pub struct ModuleInternalView {
    pub scope: expr::ModPath,
    pub env: Env,
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
    /// Type-name references in type positions.
    pub type_refs: GPooled<Vec<TypeRefSite>>,
    /// Struct field selections.
    pub field_refs: GPooled<Vec<FieldRefSite>>,
    /// Warnings, in place of the stderr lines a run prints.
    pub warnings: GPooled<Vec<Warning>>,
    /// `val`-sig ↔ `let`-impl bind links.
    pub sig_links: GPooled<Vec<SigImplLink>>,
    /// Per-module impl-side env snapshots.
    pub module_internals: GPooled<Vec<ModuleInternalView>>,
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
        static FIELD_REF_SITE_POOL: LazyLock<Pool<Vec<FieldRefSite>>> =
            LazyLock::new(|| Pool::new(64, 65536));
        static WARNING_POOL: LazyLock<Pool<Vec<Warning>>> =
            LazyLock::new(|| Pool::new(64, 4096));
        static SIG_LINK_POOL: LazyLock<Pool<Vec<SigImplLink>>> =
            LazyLock::new(|| Pool::new(32, 4096));
        static MODULE_INTERNAL_VIEW_POOL: LazyLock<Pool<Vec<ModuleInternalView>>> =
            LazyLock::new(|| Pool::new(32, 4096));
        Self {
            binds: BIND_POOL.take(),
            references: REFERENCE_SITE_POOL.take(),
            module_references: MODULE_REF_SITE_POOL.take(),
            scope_map: SCOPE_MAP_ENTRY_POOL.take(),
            type_refs: TYPE_REF_SITE_POOL.take(),
            field_refs: FIELD_REF_SITE_POOL.take(),
            warnings: WARNING_POOL.take(),
            sig_links: SIG_LINK_POOL.take(),
            module_internals: MODULE_INTERNAL_VIEW_POOL.take(),
        }
    }
}

impl Default for Ide {
    fn default() -> Self {
        Self::new()
    }
}
