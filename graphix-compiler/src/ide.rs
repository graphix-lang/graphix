//! IDE/LSP side-channels: write-only sinks the compiler fills when
//! [`crate::env::Env::lsp_mode`] is set, drained at the compile
//! boundary into the check result. Nothing here is read by the
//! compiler itself. [`Ide`] owns all of them, shared via `Env.ide`.

use crate::{BindId, Scope, SourcePosition, env::Env, expr};
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

/// A module reference (`use foo;` or `mod foo;`). For `mod foo;`,
/// `def_ori` is the file the body was loaded from.
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
}

/// The compiler descended into an `Expr` at `(pos, ori)` while in
/// `scope`. `cursor → scope` is the entry with the greatest `pos` ≤
/// the cursor in the same file.
#[derive(Debug, Clone)]
pub struct ScopeMapEntry {
    pub pos: SourcePosition,
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
    /// Resolved name references (`textDocument/references`,
    /// `textDocument/definition`).
    pub references: GPooled<Vec<ReferenceSite>>,
    /// Module references — `use foo;` and `mod foo;` mentions.
    pub module_references: GPooled<Vec<ModuleRefSite>>,
    /// Per-compile scope map answering `cursor → scope` queries.
    pub scope_map: GPooled<Vec<ScopeMapEntry>>,
    /// Type-name references in type positions.
    pub type_refs: GPooled<Vec<TypeRefSite>>,
    /// `val`-sig ↔ `let`-impl bind links.
    pub sig_links: GPooled<Vec<SigImplLink>>,
    /// Per-module impl-side env snapshots.
    pub module_internals: GPooled<Vec<ModuleInternalView>>,
}

impl Ide {
    /// Fresh, empty sinks from the pools.
    pub fn new() -> Self {
        static REFERENCE_SITE_POOL: LazyLock<Pool<Vec<ReferenceSite>>> =
            LazyLock::new(|| Pool::new(64, 65536));
        static MODULE_REF_SITE_POOL: LazyLock<Pool<Vec<ModuleRefSite>>> =
            LazyLock::new(|| Pool::new(64, 65536));
        static SCOPE_MAP_ENTRY_POOL: LazyLock<Pool<Vec<ScopeMapEntry>>> =
            LazyLock::new(|| Pool::new(64, 65536));
        static TYPE_REF_SITE_POOL: LazyLock<Pool<Vec<TypeRefSite>>> =
            LazyLock::new(|| Pool::new(64, 65536));
        static SIG_LINK_POOL: LazyLock<Pool<Vec<SigImplLink>>> =
            LazyLock::new(|| Pool::new(32, 4096));
        static MODULE_INTERNAL_VIEW_POOL: LazyLock<Pool<Vec<ModuleInternalView>>> =
            LazyLock::new(|| Pool::new(32, 4096));
        Self {
            references: REFERENCE_SITE_POOL.take(),
            module_references: MODULE_REF_SITE_POOL.take(),
            scope_map: SCOPE_MAP_ENTRY_POOL.take(),
            type_refs: TYPE_REF_SITE_POOL.take(),
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
