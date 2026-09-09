# The module system: Rust-2018-style names

Status: built 2026-08-22
Pins: `stdlib/graphix-tests/src/lang/modules.rs` (`finding1_sig_alias`,
`finding1_private_type_in_body`, `finding1_imported_body_annotation`,
`finding1_private_type_union_member`, `use_in_value_position_is_compile_error`,
`declaration_in_value_position_is_compile_error`),
`graphix-compiler/src/expr/parser/test.rs` (`use_groups`, `use_new_grammar`,
`keyword_rooted_typath`), `graphix-compiler/src/env.rs` (`mod_root_strips_marked`).

## The invariant

> Each module's namespace is an explicit table — name → resolved
> target, private items included — and that table is the only thing
> ANY resolution consults: immediate, deferred, instance-time, or
> consumer-side.

Resolution is a pure function of `(module, name)`. The table is never
stripped or truncated for the module's own use; the interface (`.gxi`)
gates only lookups that CROSS into the module from outside.

### Why

Before this, name resolution was a function of ambient context:
`use` was OCaml `open` (a search-path injection), and a bare name from
scope `S` walked every enclosing scope to the root, at each level also
searching every module that level had opened. Three consequences, all
bugs the admin-TUI port hit:

1. **Resolution was time- and place-sensitive.** Anything resolving a
   name LATER (per-callsite instance elaboration, `TypeRef::lookup_ref`
   at consumer touch) or from a DIFFERENT scope (interface types read
   by consumers) had to reconstruct the def-site's implicit
   environment, and each reconstruction site was a place to get it
   wrong: a gxi type field spelled through a use-alias broke at
   consumer touch; a module-private type in a public lambda's body
   broke at instance elaboration; a use-imported bare type name in a
   body annotation broke the same way. The def site accepted spellings
   the use site could not resolve.
2. **Position-dependent visibility.** A submodule saw its parent's
   bindings only if the `mod` declaration came after them —
   declaration ORDER was a visibility rule.
3. **Ambient ambiguity.** From inside `netidx_admin::tui`, the path
   `tui::overlay::Layer` meant the root package only because no
   ancestor happened to have a nested `tui`.

The change was made while there was no installed base.

## Surface language

### Paths

Rust 2018 rules, in expression AND type positions (`super::helper(x)`,
`let p: package::tui::Pump = …`). A path's leading segment is one of:

- a **package name** (the package prelude): `tui::line`, `array::len`;
- **`package`** — the current package's root (Rust's `crate`);
- **`self`** — the current module (also names the module itself in a
  group: `use tui::overlay::{self, layer}`);
- **`super`** — the parent module, chainable.

`self`, `super`, `package` and `pub` are fully reserved: full
reservation keeps the path grammar LL-simple and the diagnostics honest
("`package` is a reserved word"), and the no-installed-base window made
the breakage free.

### `use`

`use` imports NAMES into the declaring scope; it is not an open.

```graphix
use tui::line;                       // one name
use tui::{line, span, style};        // group
use tui::overlay::{self, layer};     // the module and an item
use sys::{net, time};                // nesting
use tui::line as tline;              // rename
use tui::*;                          // glob
use super::{answer, identicon};      // relative
use package::tui::pump;              // package-relative
```

An imported name covers every declaration kind that shares it (value,
type, module). Globs are legal and style-discouraged; the one blessed
glob in exemplar code is the widget-module idiom `use gui::text::{self,
*}` — a module and its main function sharing a name is what the
spelling is FOR. `use` is legal inside any block or lambda body,
importing into that lexical scope only. It compiles to a static env
mutation plus a `Nop` node; there is no reactive `Use` node that
un-imports on delete. A `use` in a `.gxi` is a private import shared
with the impl, not a re-export. `pub use` parses and refuses
("re-exports (`pub use`) are not yet supported") so the grammar does
not move twice when re-exports arrive.

### `mod`

`mod foo;` declares the submodule and introduces the name `foo` — a
declaration, not an import. A `mod` may appear anywhere in the file and
the submodule sees NOTHING of the parent implicitly; it writes `use
super::…` for what it needs. A module's BODY still executes in graph
order; only name visibility is order-independent.

### Preludes — the only implicit names

1. **The core prelude**: the root items of `core`'s interface plus the
   primitive type names, an implicit glob of `/core` in every module,
   shadowable by declarations and imports.
2. **The package prelude**: every registered package's NAME is a path
   root anywhere (`Env.package_roots`, populated by `defpackage!`'s
   generated `register`). Only the name is implicit, never the items.

### Shadowing and collisions

Precedence, first hit wins, kind-filtered: own declaration → explicit
import → glob → package prelude → core prelude.

- A `use` colliding with an existing same-scope declaration errors at
  the import; a declaration AFTER an import shadows it silently (`let`
  re-binding is idiomatic Graphix and own-first precedence makes it
  well-defined).
- Two explicit imports of one name: error (rename one). The REPL
  compiles with `CFlag::ReplaceImports` so a re-`use` shadows, like a
  `let` re-binding; file modules keep the error.
- Two globs providing one name: allowed at import, **error at first
  use** naming both sources (Rust's rule — glob pairs are common and
  mostly disjoint). This immediately caught two real latent collisions
  the old walk resolved silently by search order: `window` (the gui
  widget vs `array::window`) and `Table` (`gui::data_table` vs
  `sys::net`).
- An explicit import covers only the kinds its target has; a kind miss
  falls through to globs. Load-bearing for `{self, *}`: the module name
  is the explicit entry, the same-named val arrives by glob.
- Path roots consult module-kind entries only, so `let array = …` never
  breaks `array::len` (Rust's locals-don't-block-paths rule). The
  earlier ruling that an item named like a package is refused was
  dropped for this: `tui::list` is a stdlib module named like the
  `list` package, and examples `use tui::list`. Importing a package
  under its own name is a no-op.

Expression-level lexical scoping is untouched: blocks, lambda params,
select-arm binds and local `let`s chain lexically, and the chain stops
at the module root instead of continuing into ancestor modules.

### Declarations are statement-position-only

`use`, static `mod`, `type`, `trait` and `impl` refuse in value
position (a `let` RHS, a call argument, a block's value slot, a select
arm body). They are ⊥-typed with a phantom value channel, and a
value-position one let a connect route runtime values through a ⊥
binding. A dynamic `mod` stays an expression with a real
`[error, null]` value.

### Interfaces

Unchanged in role: `use`, `type` and `mod` in the `.gxi` apply to the
`.gx` automatically (requiring duplication is ceremony). A private
item is visible to the defining module and its descendants, uniformly
for values and types — a private type in a public lambda's body is the
module's own business when the body is instance-elaborated elsewhere.

## Resolution

### The table

`Env.names: Map<ModPath, ScopeNames>`, where a `ScopeNames` is
`imports: name → ImportEntry { scope, name, chain }` plus the scope's
glob source list in declaration order. Scope paths are globally
unique, so `names` is a per-context registry of every module's and
block's import table, and it is EXEMPT from `restore_lexical_env` —
never rolled back by the module privacy swap. That exemption IS the
invariant: `ctx.env.names[def_scope]` equals a captured
`f.env.names[def_scope]` by construction, which is what lets instance
elaboration, `TypeRef::lookup_ref` and the interface bridging walks all
resolve through the DEFINING module's table whenever they run. The
places that used to rely on `Use::delete` scrub explicitly: a dynamic
module recompile clears `names` under its scope; `unbind_scope_subtree`
scrubs it for the LSP.

### Marked block components

Generated block scope components carry a `#` prefix (`#do123`, `#fn7`,
`#sel9`); identifiers cannot start with `#`, so no module can collide.
`mod_root(scope)` strips trailing `#`-components. This beats testing
membership in `env.modules` because it works in EVERY env with zero
lookups — the modules set is env-relative and a private-env snapshot
need not contain the module's own path.

### The algorithm

A bare name from scope `S`, walking `S`, `dirname(S)`, … to
`mod_root(S)`: own declarations (kind-specific) → explicit imports (one
redirect, kind-checked at the target) → globs (all sources checked,
two distinct hits is the ambiguity error). Past the module root: the
core prelude, then the package prelude (module-kind lookups only).

Qualified paths resolve the leading segment module-kind-only through
the same chain, then descend `modules` with privacy by presence in the
consulted env's view; interior imports of the modules walked are not
consulted (plain `use` is private), except through `self::`/`super::`
roots, which are inside the privacy boundary. The package prelude gates
at the DESCENT: `package_roots` membership alone answers the leading
segment, so a sandboxed dynamic-module env may keep `/sys/net` without
`/sys`.

Keyword roots resolve structurally. `self::x` looks up at
`mod_root(S)`. `package::x` looks up at the package root — `/pkg` when
`S`'s first component is a registered package, else `/` (in a loaded
script this is the root program scope, not the file's block: file-top
items are reachable only by counted `super`s — a documented wart).
`super` is SCOPE-relative, not module-relative: one `super` from module
`M` anchors at `dirname(M)` and resolves along that anchor's chain (an
entry carries `chain: true`; a `super::*` glob expands to one source
per chain level at use time). This is what makes script files work: a
loaded file's top level is the `#do` block under root, and a
submodule's `use super::x` must reach the file's top-level lets, which
live at that block scope. Climbing above `/` or a registered package's
root is an error. Check mode (statements at root) and load mode (the
`#do` wrapper) agree.

### Headers

Before compiling a block's children, `compile_block_children`
pre-registers the block's `mod` NAMES (`predeclared_mods` keeps the
duplicate-module guard honest), and `bind_sig` pre-registers sig `mod`
items the same way, so declaration order carries no visibility meaning
and a sibling submodule wins over a same-named package (`list::List`
inside tui is the submodule). Values are NOT pre-registered: forward
value references remain errors, because body evaluation order is
semantics in a dataflow language.

### Import compilation

Every MODULE segment of a use path validates eagerly (headers make
forward `mod`s visible). The terminal name may not exist yet
(`use self::sub::x` may legitimately precede `mod sub;` in body order):
the entry is stored and re-checked when the enclosing top-level
statement finishes compiling, so a typo'd import still errors at the
right position. Glob items validate the module path only. Renames key
the entry under the `as` name; `use m` alone stores a module-kind entry.
Use segments accept type names (an uppercase interior segment refuses
at resolution), which `use super::{Client, Response}` needs.

A dynamic module's `source` expression compiles in the ENCLOSING scope:
it is loader-side code, and compiling it under the module's own scope
made `let src = …; mod foo dynamic { … source src }` inexpressible
(inside a block no spelling reaches the block-local; `super::` names
module items only). The sig still binds under the module.

### Diagnostics

The table knows exactly what is in scope, so an unresolved name gets a
real error, an ambiguous glob names both sources, and `resolve_pure`
logs structural resolution errors before mapping them to `None` so an
ambiguity cannot masquerade as "undefined type".

## What was deleted

The ancestor-module visibility walk; `use`-as-open; the `used`
search-path map and its deferred re-resolution of written use names;
positional `mod` visibility and the gxi ordering rule; the reactive
`Use` node; the "use paths are absolute" fiction (root was just the
last ancestor tried).

Not touched: the node graph, the JIT, module SOURCE loading
(`ModuleResolver`/GRAPHIX_MODPATH — file resolution is orthogonal to
name resolution), the dynamic catch scope.

Not built: `pub use` re-exports.
