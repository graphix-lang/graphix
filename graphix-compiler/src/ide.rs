//! IDE/LSP tooling side-channels.
//!
//! Everything in this module exists so editor tooling (go-to-definition,
//! find-references, hover, cursor → scope) can be answered without
//! re-implementing the compiler's name and type resolution. None of it
//! is consulted by the compiler proper — the site collections are pure
//! write-only sinks during a compile, populated only when
//! [`crate::env::Env::lsp_mode`] is set and drained at the compile
//! boundary into the runtime's check result.
//!
//! The single [`Ide`] struct owns all six side-channels. It's shared via
//! `Env.ide` (an `Option<Arc<Mutex<Ide>>>`): pushes come from two kinds
//! of site — those that hold `&mut ExecCtx` and those that hold only
//! `&Env` — and routing both through the env's shared sink keeps a
//! single home for every IDE table while letting reentrant or concurrent
//! compiles within one check drain into the same buffer.

use crate::{env::Env, expr, BindId, Scope, SourcePosition};
use compact_str::CompactString;
use poolshark::global::{GPooled, Pool};
use std::sync::LazyLock;
use triomphe::Arc;

/// A textual occurrence of a name at a specific source position that
/// the compiler resolved to a particular `BindId`. Populated as a side
/// effect of compilation so IDE tooling can answer
/// `textDocument/references` and `textDocument/definition` without
/// re-implementing name resolution.
///
/// `def_pos` and `def_ori` mirror the bind's declaration site at
/// resolution time. They're captured here because some bindings
/// (notably lambda parameters) are unbound from the env when the
/// callsite that created them is dropped — but their declaration
/// site is still meaningful to the user.
#[derive(Debug, Clone)]
pub struct ReferenceSite {
    pub pos: SourcePosition,
    pub ori: Arc<expr::Origin>,
    pub name: expr::ModPath,
    pub bind_id: BindId,
    pub def_pos: SourcePosition,
    pub def_ori: Arc<expr::Origin>,
}

/// A textual occurrence of a module reference (either `use foo;` or
/// `mod foo;`). For the `mod foo;` case `def_ori` points at the file
/// the module's body was loaded from — that's the natural target for
/// go-to-definition on a module name.
#[derive(Debug, Clone)]
pub struct ModuleRefSite {
    pub pos: SourcePosition,
    pub ori: Arc<expr::Origin>,
    /// Module name as the user wrote it (might be relative).
    pub name: expr::ModPath,
    /// Absolute module path the compiler resolved this reference to.
    pub canonical: expr::ModPath,
    /// Origin of the module's body (the file it was loaded from)
    /// when this site is itself a declaration that pulled the
    /// module in. `None` for plain `use` sites.
    pub def_ori: Option<Arc<expr::Origin>>,
}

/// One entry in the per-compile scope map: the compiler descended
/// into an `Expr` at this `(pos, ori)` while in this `scope`. IDE
/// tooling answers `cursor → scope` by finding the entry with the
/// greatest `pos` ≤ the cursor in the same file.
#[derive(Debug, Clone)]
pub struct ScopeMapEntry {
    pub pos: SourcePosition,
    pub ori: Arc<expr::Origin>,
    pub scope: Scope,
}

/// A textual occurrence of a type reference (e.g. `Foo` in `let x: Foo`).
/// Captured by the compiler when a `Type::Ref` carrying parse-time
/// position info gets dereferenced. `def_pos`/`def_ori` point at the
/// `type Foo = …` declaration site so go-to-def on a type name lands
/// on the typedef.
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

/// Maps a `val foo: T` declaration in a `.gxi` interface to its
/// `let foo = …` implementation site in the paired `.gx`. Populated by
/// `check_sig` whenever it matches a sig proxy bind to its impl bind.
/// Used by IDE tooling to (a) goto-def from a sig val site to the impl,
/// and (b) union find-references results across both `BindId`s.
/// Only populated when `env.lsp_mode` is set.
#[derive(Debug, Clone)]
pub struct SigImplLink {
    pub scope: expr::ModPath,
    pub name: CompactString,
    pub sig_id: BindId,
    pub impl_id: BindId,
}

/// Per-module snapshot of the *internal* env (the impl's view, where
/// implementation bindings shadow sig proxies). The CheckResult's
/// top-level `env` is the *external* view across the project; this
/// per-module entry lets IDE queries on names inside a module reach
/// the impl bind metadata. Only populated when `env.lsp_mode` is set.
#[derive(Debug, Clone)]
pub struct ModuleInternalView {
    pub scope: expr::ModPath,
    pub env: Env,
}

/// Every IDE/LSP side-channel accumulated during a compile, in one
/// place. `Some(_)`-installed into `Env.ide` only when running under an
/// LSP-style check; non-LSP compiles leave `Env.ide` as `None` and pay
/// nothing at the push sites.
///
/// The first three tables are pushed from sites that hold `&mut
/// ExecCtx` (via `Env::push_reference` / `push_module_reference` /
/// `push_scope_map_entry`); the last three from sites that hold only
/// `&Env` (via `push_type_ref` / `push_sig_link` /
/// `push_module_internal_view`). All six share the one sink so the
/// runtime's `check` swaps them in and out as a single unit.
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
    /// Fresh, empty sinks pulled from the named pools. Sized generously
    /// since the LSP recompiles on every keystroke and the reference /
    /// scope tables can grow into the tens of thousands of entries on
    /// large modules.
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
