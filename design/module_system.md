# The module system: from open to use

Status: **PROPOSED** — ruled in outline (Eric, 2026-08-22: transition
from the OCaml-style open system to a Rust-style use system, all leans
in the design conversation accepted including `use x as y`); this doc
is the detailed design for review before build. Open points for Eric
are marked **OPEN** inline and collected at the end.

## Motivation

Name resolution today is a function of ambient context. The engine is
`Env::find_visible` (env.rs): a reference `name` from scope `S` walks
every enclosing scope from innermost to root, and at each level also
searches every module path that level has `use`d — `use` is OCaml
`open`, a search-path injection, and the "paths are absolute" rule is
a fiction (root is just the last ancestor tried; `array::len` works
bare because the walk reaches `/`). Three consequences, all bugs or
walls we have hit:

1. **Resolution is time- and place-sensitive.** Anything that
   resolves a name LATER (per-callsite instance elaboration,
   `TypeRef::lookup_ref` at consumer touch) or from a DIFFERENT scope
   (interface types read by consumers) must reconstruct the def-site's
   implicit environment — and each reconstruction site is a place to
   get it wrong. The admin-TUI campaign's finding 1 (2026-08-21) is
   three faces of exactly this: a gxi type field spelled through a
   use-alias broke at consumer touch; a module-private type in a
   public lambda's body broke at instance elaboration; a use-imported
   bare type name in a body annotation broke the same way. The
   def-site accepted spellings the use-site could not resolve. The
   `used` map doesn't even resolve its own entries at use time —
   `use_in_scope` validates and then stores the WRITTEN relative
   name, re-resolved ambiently on every lookup.
2. **Position-dependent visibility.** A submodule sees its parent's
   bindings only if the `mod` declaration comes after them in the
   parent — declaration ORDER is a visibility rule. Reordering
   declarations changes meaning; the TUI module had to be declared
   last in netidx_admin's gxi for exactly this reason.
3. **Ambient ambiguity.** From inside `netidx_admin::tui`, the path
   `tui::overlay::Layer` means the root package only because no
   ancestor happens to have a nested `tui` — the ancestor walk would
   prefer one silently if it appeared. Every unqualified name's
   meaning depends on the whole ancestor chain's contents.

The window for this change is now: there is no installed base, and
the cost only grows. (Same argument that ruled the dead-arm change.)

## The core invariant

> **Each module's namespace is materialized once, at module compile,
> into an explicit table — name → resolved target, private items
> included — and that table is the only thing ANY resolution
> consults: immediate, deferred, instance-time, or consumer-side.**

Resolution becomes a pure function of `(module, name)`. The table is
never stripped or truncated for the module's own use; the interface
(`.gxi`) gates only lookups that CROSS into the module from outside.
This is what fixes finding 1 structurally rather than patch by patch:
instance elaboration, `TypeRef::lookup_ref`, and the privatize/bridge
walks all resolve through the defining module's table, which exists
independently of when or where they run. (The env-independent-TypeRef
carried cell remains as a cache; the table is what makes its
lazy-seeding rules stop being load-bearing for correctness.)

## Surface language

### Path grammar

Rust 2018 rules, exactly — they are battle-tested and any Rust user
already knows them. A path's leading segment is one of:

- a **package name** (from the package prelude, below): `tui::line`,
  `netidx_admin::connect`
- **`package`** — the current package's root (Rust's `crate`):
  `package::tui::pump`
- **`self`** — the current module: `self::helper` (also usable to
  name the module itself in a group: `use tui::overlay::{self, layer}`)
- **`super`** — the parent module, chainable:
  `use super::{Question, QuestionId}`, `use super::super::x`

The same grammar applies in **expression and type positions**, not
just `use`: `super::helper(x)`, `let p: package::tui::Pump = …`.

### `use` declarations

`use` imports NAMES into the declaring scope — it is no longer an
open. Forms:

```graphix
use tui::line;                       // one name
use tui::{line, span, style};        // group (exists today, kept)
use tui::overlay::{self, layer};     // self in group (exists, kept)
use sys::{net, time};                // nesting (exists, kept)
use tui::line as tline;              // NEW: rename
use tui::*;                          // NEW: wildcard (glob) import
use super::{answer, identicon};      // NEW: relative
use package::tui::pump;              // NEW: package-relative
```

An imported name covers every declaration kind that shares it (value,
type, module) — Rust's all-namespaces import. `use tui` alone imports
only the module name `tui` (a no-op at top level given the package
prelude, meaningful for nested modules).

Globs stay legal and style-discouraged (test modules, prelude-shaped
packages); the migration relies on them.

### `mod` declarations

`mod foo;` declares the submodule and introduces the name `foo` in
the declaring module — declaration, not import, exactly as today. What
dies is POSITION: a `mod` may appear anywhere in the file, and the
submodule sees nothing of the parent implicitly — it writes
`use super::…` for what it needs. The gxi ordering rule ("a submodule
can reference parent bindings only if `mod` comes after them") is
deleted.

Note `mod` remains a reactive-graph statement in one respect: a
module's BODY still executes in graph order. Only name VISIBILITY
becomes order-independent.

### Preludes — the only implicit names

1. **The core prelude**: the root items of `core`'s interface
   (`print`, `println`, `cast`, `error`, `filter`, `once`, `never`,
   `seq`, `Any`, `Result`, …) plus primitive type names. Implicitly
   imported into every module; shadowable by explicit declarations
   and imports (Rust's std-prelude rule).
2. **The package prelude**: every registered package's NAME is usable
   as a path root anywhere — `array::len`, `str::join` keep working
   bare-qualified with no `use` (Rust's extern prelude). Only the
   name is implicit; the package's items are not.

Everything else in scope arrived by an explicit `use`, an explicit
declaration, or `mod`.

**Package-name shadowing (OPEN, recommendation):** a module item or
import named the same as a registered package is a compile error
("item shadows the package `tui`"). This avoids needing Rust's `::x`
disambiguator syntax entirely. Package sets are small and user-chosen,
so collisions are rare and the loud error is cheap; if it ever bites,
adding a `::`-rooted absolute form later is compatible. Alternative:
allow shadowing and add `::tui` now.

### Shadowing and collisions

Within one module namespace, per name-kind:

- an explicit declaration and an explicit import of the same name:
  **error** at the import;
- two explicit imports of the same name: **error** (rename one);
- explicit (declaration or import) beats **glob**: allowed, the
  explicit wins;
- two globs providing the same name: allowed at import; **error at
  first USE** of the ambiguous name, naming both sources (Rust's
  rule — glob pairs are common and mostly-disjoint).

Expression-level lexical scoping is UNTOUCHED: blocks, lambda params,
select-arm binds, and local `let`s chain lexically as today — the
chain stops at the module root instead of continuing into ancestor
modules. Locals shadow module items and imports as they do now.

### `use` inside blocks

`use` remains legal inside any block/lambda body, importing into that
lexical scope only (Rust allows this too; the shell depends on
incremental uses). But `use` stops being a live graph node: it
becomes a compile-time declaration compiled to NoOp — the current
`Use::update`/`delete`/`stop_use_in_scope` machinery (a REACTIVE
import that un-imports when the node is deleted) is deleted. The
shell's per-expression env update handles REPL redefinition the same
way it handles `let` shadowing.

### Interfaces (`.gxi`)

Unchanged in role: the interface declares the public API; items not
in it are private. Kept conventions:

- `use`, `type`, and `mod` in the `.gxi` apply to the `.gx`
  automatically (the interface is part of the module; requiring
  duplication is ceremony). A `use` in the gxi is NOT a re-export —
  it is a private import shared with the impl, exactly as today.
- Privacy scope: a private item is visible to the defining module
  and its descendant modules (Rust's default-privacy rule; today's
  behavior for values, now uniform for types too — face 2 of
  finding 1 was privatization making a private type invisible to the
  module's own instance-elaborated bodies).

**Re-exports (reserved, phase 2):** the spelling `pub use path::name;`
in a `.gxi` re-exports the target as if declared here. Not built in
this arc; the parser reserves the form (parse + "not yet supported"
error) so the grammar doesn't shift twice.

### Keywords

`super` and `package` become reserved words everywhere; `self`
becomes reserved everywhere (it is already contextual in use groups).
Full reservation is chosen over contextual because it keeps the path
grammar LL-simple and the diagnostics honest ("`package` is a
reserved word"), and the no-installed-base window makes the breakage
free. `bytes`-style adjudication applies if the roundtrip hunt finds
a collision; none is expected (`self`/`super`/`package` are not
plausible field names in wire mirrors the way `duration` was —
**OPEN** to veto if you disagree, this is the one lean in this doc I
haven't run past you).

## Resolution algorithm

### Building the table

Per module, at module compile, two passes:

1. **Headers**: collect the module's own declarations — `let` names,
   `type` names, `mod` names (gxi + gx, interface items marked
   public) — into the table unresolved-bodied but NAMED. This makes
   item declaration order-independent for resolution (bodies still
   compile and execute in order; forward VALUE references remain
   whatever they are today — this arc does not change body
   evaluation, only naming).
2. **Uses**: resolve each `use` path — leading segment against
   (own table ∪ preludes ∪ `self`/`super`/`package`), subsequent
   segments by walking the named modules' tables, PUBLIC view when
   the walk crosses out of the current package or into a sibling —
   and enter the imported names. Glob entries record their source
   module for lazy expansion + ambiguity detection. Renames enter
   under the `as` name.

Cycles (`a` uses `b` uses `a`) are legal at module granularity —
tables are name-level, and a cycle only errors if resolving a
specific NAME recurses into itself (report the chain).

### Consulting the table

Every name lookup is: walk the expression-lexical chain (block
scopes, params — unchanged, module-bounded), then the module table,
then the preludes. `find_visible`'s ancestor-module walk and the
`used` search-path mechanism are deleted. Qualified paths resolve the
leading segment the same way, then walk module tables with privacy
enforced at each crossing.

Deferred consumers unify on the same call: `TypeRef::lookup_ref`
resolves `(defining module, name)` through the table (TypeRefs
already carry their scope); per-callsite instance elaboration
resolves the lambda body's names through the DEFINING module's table,
not the caller's env; the privatize/bridge walks read the same table
with the public/private bit doing the work the registry-copy
truncation does today. The three finding-1 faces become regression
fixtures that go red→green in this phase.

Dynamic modules (`ModuleKind::Dynamic`, hot-reload) build their table
per load from the loaded source's uses; their declared sig gates the
public view as it does today.

### Diagnostics

The table knows exactly what is in scope, so unresolved names finally
get real errors: "no `line` in scope — `tui::line` exists; add
`use tui::line`" (candidate search over package roots the module
already touches, then all packages). Ambiguous-glob errors name both
sources. This is also the infrastructure half of the campaign's
finding 3 (reserved-word/merged-expectation diagnostics are a parser
concern, but "undefined type X in scope Y" messages route through
here and stop lying about scope).

## What dies (inventory)

- The ancestor-module visibility walk (module-crossing part of
  `find_visible`).
- `use`-as-open: `use m` no longer makes `m`'s items bare.
- The `used: Map<ModPath, Arc<Vec<ModPath>>>` search-path map and
  deferred re-resolution of written use names.
- Positional `mod` visibility and the gxi ordering rule.
- The reactive `Use` node (`update`/`delete`/`stop_use_in_scope`).
- The "use paths are absolute" rule (superseded by leading-segment
  rules; the accidental relative-ambient resolution dies with it).
- The sig-vs-impl whole-list use dedup noted in 226ad10d (grouping
  imperfection) — subsumed by table entry semantics.

## Implementation plan

- **P1 — grammar** (compatible, lands on main): parser + printer +
  roundtrip generator + tree-sitter for `as` renames, `*`, and
  `self`/`super`/`package` leading segments in use trees AND
  expression/type paths; `pub use` parsed-and-refused. Old semantics
  untouched. The printer's regrouping learns the new segment kinds.
- **P2 — the table** (branch `module-system`): build per-module
  tables; swap `lookup_bind`/`lookup_typedef`/`canonical_modpath`
  onto table+lexical-chain resolution; preludes; privacy at
  crossings; `Use` becomes a static declaration; delete the open
  machinery.
- **P3 — deferred unification**: `TypeRef::lookup_ref`, instance
  elaboration, privatize/bridge walks consult tables. Finding-1
  regression fixtures (all three faces, as graphix-tests fixtures
  with a two-module VFS layout) red→green here.
- **P4 — migration** (same branch, lands with P2/P3): mechanical
  rewrite first — top-level `use m` → `use m::*` is
  semantics-preserving under the new rules, nested `use a::b` →
  `use a::b::{self, *}` — then compile-error-driven fixup for the
  dead implicit-parent references (`use super::…`), which doubles as
  the first field test of the new diagnostics. Hand-tighten the
  stdlib and book examples to explicit lists (they are the exemplar
  code); leave mechanical globs in test fixtures where tightening
  adds nothing. Touches: every stdlib `.gx`/`.gxi`, book modules
  chapter + examples, graphix-tests inline programs, the fuzzer's
  generator vocabulary, browser.gx, soak-dash.gx, and (netidx repo)
  the admin package.
- **P5 — gates** (the arc-end factorized run): full workspace suites,
  FUSEAUDIT sweep, overnight roundtrip on the new grammar, fuzz soak
  on the migrated generator, GUI-wedge `--check` timing (the table
  should be faster than the walk — verify, don't assume), admin
  package suite in the netidx repo.

Not touched: the node graph, the JIT, module SOURCE loading
(`ModuleResolver`/GRAPHIX_MODPATH — file resolution is orthogonal to
name resolution), the dynamic catch scope, `graphix-ast-pack` (blobs
are regenerated by the in-tree compiler, no skew by construction).

## Open questions (collected)

1. Package-name shadowing: refuse (recommended) vs allow + `::x`
   absolute paths.
2. Keyword reservation: `self`/`super`/`package` fully reserved
   (recommended) vs contextual-in-paths only.
3. Re-export spelling `pub use` in `.gxi`, parsed-and-refused this
   arc — confirm the spelling so the grammar doesn't move twice.
4. Migration style for graphix-tests fixtures: mechanical globs
   (recommended — 2000+ fixtures, tightening is noise) vs explicit
   lists everywhere.
