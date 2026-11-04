# The module system: Rust-2018-style names

Status: built 2026-08-22
Pins: `stdlib/graphix-tests/src/lang/modules.rs` (`sig_type_through_use_alias`,
`private_type_in_public_body`, `imported_type_in_body`,
`private_type_union_member`, `use_in_value_position_is_compile_error`,
`declaration_in_value_position_is_compile_error`),
`graphix-types/src/expr/parser/test.rs` (`use_groups`, `use_new_grammar`,
`keyword_rooted_typath`), `graphix-types/src/env.rs` (`mod_root_strips_marked`).

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

The parser retains comments on use-tree entries in `UseComments`, outside
syntax equality and hashing. Each comment records its prefix depth,
whether it stands above an entry, ends its line, or sits below a group,
and its line (`Comment`, which says whether a blank line stands above it
in its block).
A comment on a single path follows its imported name; a group comment
follows its prefix. `UseItem::sorted` moves this metadata to the first
canonical leaf of that prefix. If grouping joins several ending comments
at one prefix, they become leading lines there. The formatter retains
braces around a commented group even when it contains only one name, and
its ornament check verifies the normalized comment records on reparse.
Pins: `comments_in_use_trees` in `expr/format.rs`.

### `mod`

`mod foo;` declares the submodule and introduces the name `foo` — a
declaration, not an import. A `mod` may appear anywhere in the file and
the submodule sees NOTHING of the parent implicitly; it writes `use
super::…` for what it needs. A module's BODY still executes in graph
order, and only module and type names are visible whatever their order:
values are not pre-registered, so a value must be bound before the
`mod` whose body imports it (`mod c; let top = 1;` with `c.gx` doing
`use super::top` is refused, "top not defined").

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
  use** naming both sources, unless both reach the same item (Rust's rule — glob pairs are common and
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
binding. A `let` refuses in value position too, a block's value slot
excepted: it has no value, and its name would bind in the enclosing
block, past the expression that holds it. A dynamic `mod` stays an expression with a real
`[error, null]` value.

### Interfaces

Unchanged in role: `use`, `type`, `trait` and `mod` in the `.gxi` apply
to the `.gx` automatically (requiring duplication is ceremony), spliced
into its body (`resolver::add_interface_modules`): each after the body's
last declaration of every interface item listed before it, whatever
order the body declares them in, ahead of the body when it declares
none of them, and in interface order among themselves; a type, or a
trait with no default body, reads no value and goes ahead of the body.
One listed after a `val` the body does not declare goes after the body,
which the interface's check refuses for the missing item, unless the
compile fails first: the spliced item, or a body statement reading it,
that reads the missing name is refused as not defined.
The `.gx` may not define a type or trait its interface
defines: only a hidden type (`type T;`) takes a definition there, an
abstract with the declared parameters (`module.rs::check_redefinition`).
The reverse does not hold: a name the `.gxi` writes resolves through
what the `.gxi` imports and declares, the prelude and other modules'
paths, never through the `.gx`'s imports or its own types, so an
interface means one thing whatever its implementation imports. The
interface check resolves each item's names in that view
(`module.rs::interface_env`, `check_interface_names`) and refuses one it
does not give; a script's interface alike. A private
item is visible to the defining module and its descendants, uniformly
for values and types — a private type in a public lambda's body is the
module's own business when the body is instance-elaborated elsewhere.

At run time a `val` and the binding implementing it are one variable
(`module.rs::Proxy`, joined in `StaticCallees`). A write to either lands
on both as the cycle starts, before any node runs
(`deliver_pair_writes`), so every reader of either sees it that cycle
wherever it runs; writes to both in one cycle are two writes to one
variable, the body's landing first. What one of them produces within a
cycle, the body's `let` or a reload's new value, the module node moves
to the other as a binding delivers its own (`deliver_production`:
stored, in the cycle's event, and a fire schedules the roots that read
it and have not run yet): a `let` bound to `a::v` in a REPL input
after the module's, or an instance of one of the body's functions
called from one, sees it. A reader that ran earlier in the cycle (a
statement before the `mod`, an earlier input), through a function
value, misses it as it misses any `let`'s production (CR
root-order-production-01). Each is a `<-` target where the other is
(`CompileCtx::join_interface_pair`), so a woken `let` keeps a write
made through the `val`, and a seq's plan counts the pair one variable
(`design/dependency_summaries.md` §3).

### A `val`'s type variables

A `val` is a scheme its uses copy: every val whose type holds a variable
is generalized (`Env::poly_binds`), and a read of a function-typed val
and a call through a function type a val holds instantiate the
variables standing inside function types. A variable outside every
function type (`val v: Array<'a>`) no use copies, so a
write at one type and a read at another would share one cell: the
module's check refuses such a val, "declare the val at one type"
(`module.rs::check_val_variables` over the val's binding, a variable
counted whatever a use has bound it to, so a sibling's check decides
nothing here; `Type::variable_outside_functions`: a typedef as its
expansion, an abstract type's parameters and an unresolved name's
arguments outside, a typedef met inside itself over grown parameters
counting its arguments outside). A dynamic module's `sig` is held to it
at the program's check, a script's interface at `check_script_sig`.

`Type::sig_matches` pairs each open implementation cell with the
declaration's type at its place (`SigMatch::paired`). A cell no use of
the implementation copies (a written `let`'s, an outer binding's) is one
type: paired with a type holding a variable a use picks it is refused,
naming the way out (the outer binding's type, the val at one type, a
quantified annotation on the `let`, or, for an implementation that is no
lambda, a lambda, or a literal of lambdas for a val that is no function;
the binding named is the nearest the implementation's lambda reaches
through the lambdas it captures that holds no lambda,
`module::Holder::of`); paired with a type holding none, a
concrete type or a whole quantified function type, the declaration
decides it (`module.rs::hold_paired`), so `val e: B<i64>` over `let e =
B(1)` builds `e` at `B<i64>`. A trait method's body, an impl's and a
default, is held the same way (`traits.rs::implements`). A method that
is a lambda has the cells its definition owns copied by each call, but
an impl's `let` is annotated with the method's signature, which claims
them at the top level; so its cell is one type only where a binding it
captures reaches it. A method whose value is no lambda (`(f, 1).0`, a
select, a call's result) keeps its value's types, which no call copies,
and its cells are judged by level, as a val's implementation's are.

What is written through a val lands on the implementation, the pair
being one variable, and every use takes the implementation's generic
variables at its own types; so a val something writes must fit its
implementation: a copy of the val's type contained in the
implementation's, the implementation's variables rigid, none taking a
bound its own bounds do not imply (`CompileCtx::check_val_written`).
Within one compile a `<-` or `&mut` among a block's statements compiles
before the module checks, so the module's check asks it of a val that
is a `<-` target; a later compile's write or `&mut` through a val
already joined asks it as it marks the target
(`CompileCtx::mark_connect_target`). One in a lambda body compiles at
its definition's check, and in a sibling module that check may run
beside the val's module's own: a write there through a sibling's val
(`Env::in_sibling`) is not judged as it marks the target, and once a
block's statements are checked each of its static modules, and the
static modules in them, asks it again of every val a write reached
(`Module::check_written_vals`), so the verdict and its message do not
depend on which check ran first. The val as a
whole is held, not each value written: `m::f <- |x| x` over `val f:
fn(x: i64) -> i64` and a generic implementation is refused too.

A val generic in a variable, wherever its function types stand (a
struct, an array, a typedef's arguments) and whether it writes its own
quantifiers (`fn<'a: ..>`) or not, is read as a copy
(`Type::scheme_read`, which copies a typedef application whose arguments
hold a cell a copy takes, `Type::copied_outside_functions`, as it copies
a function type), and holds a write through it as any generalized
binding does: the value written must fit a copy of the val's type with
its variables rigid, held by cell (two function types the copy took
apart name their variables alike), a variable that stands only in a
quantifier's bound included, and carry none of them out
(`Connect::typecheck0_with`). `CompileCtx::monomorphic`, which makes that
copy, instantiates a typedef's arguments as one, since they stand for
what its body holds. A writable reference to a val is refused where the
val is generic (`CompileCtx::generalized_earlier`, which counts a
declared signature's own quantifiers, its scheme's, and skips a rank-2
type's, `CellFact::FormalQuantifier`), and one to a place of it where
the place's type holds such a cell (`crate::holds_generic`, at the
reference's check). A name that aliases a generalized binding and is
written (`let g = m::c; g <- ..`) holds an instance of its own, the
value restriction (`Bind::forwards`).

## Resolution

### The table

`Env.names: Map<ModPath, ScopeNames>`, where a `ScopeNames` is
`imports: name → ImportEntry { scope, name, keyword_anchored }` plus the scope's
glob source list in declaration order. Scope paths are globally
unique, so `names` is a per-context registry of every module's and
block's import table, and it is EXEMPT from `Env::swap_lexical` (the
context's `with_restored`) — never swapped out by the module privacy
swap. That exemption IS the
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
package prelude (module-kind lookups only), then the core prelude.

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
`S`'s first component is a registered package, else `/` (the program
is the package). A script's top level is the root in every mode (the
check, the LSP, the REPL and the run all compile it there), so
`package::m` reaches a script's file-top `mod m` from any depth, and a
script's `mod` named like a package is a duplicate.
`super` is SCOPE-relative, not module-relative: one `super` from module
`M` anchors at `dirname(M)` and resolves along that anchor's chain (an
entry carries `keyword_anchored: true`; a `super::*` glob expands to one source
per chain level at use time). This is what makes script files work: a
submodule's `use super::x` must reach the file's top-level lets, which
live at the root. Climbing above `/` or a registered package's root is
an error.

### Headers

Before compiling a block's children, `compile_block_children`
pre-registers the block's `mod` NAMES (refusing a duplicate there; each
`mod` statement then compiles with `predeclared` set, so its own
duplicate guard does not trip), and `bind_sig` pre-registers sig `mod`
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
