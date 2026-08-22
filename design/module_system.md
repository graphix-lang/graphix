# The module system: from open to use

Status: **RULED** (Eric, 2026-08-22 — all leans accepted including
`use x as y`; the four open points answered inline: shadowing
restriction accepted, full keyword reservation, `pub use` spelling
confirmed, mechanical globs for test fixtures with user-facing
examples tightened explicitly). **P1 BUILT** same day: the full
grammar on main — `UseItem` AST (path + rename, keywords/glob as
reserved segments), `self`/`super`/`package` roots in use trees AND
expression/type paths, `as` renames, `*` globs, `pub use`
parse-and-refuse, `self`/`super`/`package`/`pub` fully reserved;
printer regroups renames/globs (never absorbing `*` into a prefix);
roundtrip generator + ts-compat exercise the space. Old semantics
bridged: `use m::*` maps onto open (enabling pre-migration on main),
plain module paths unchanged, renames and keyword roots refuse with
a P2 pointer. P2 (the table) next, on branch `module-system`.

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

Eric: Yep, I'm ok with this restriction for now

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

Eric: Fine with me.

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

## P2 concrete design (implementation record)

Decisions made while building the table, recorded for review. Each is
cheap to revise; flagged **[call]** where it is a judgment the ruling
did not explicitly make.

### Marked block-scope components

Scope paths gain a structural module/block distinction: generated
block components get a `#` prefix (`#do123`, `#fn7`, `#sel9`,
`#ca4`, `#c4` — identifiers cannot start with `#`, so no module can
collide). `mod_root(scope)` = strip trailing `#`-components. This
beats the alternative (testing membership in `env.modules`) because
it works in EVERY env with zero lookups — the modules set is
env-relative and the private-env snapshot doesn't even contain the
module's own path today. Five format sites change; nothing in the
tree pattern-matches the old prefixes (verified by inventory).

### The table is a global registry, not lexical state

`Env.used` is replaced by `Env.names: Map<ModPath, ScopeNames>` where

```rust
struct ScopeNames {
    imports: Map<CompactString, ImportEntry>, // name → target
    globs: Arc<Vec<ModPath>>,                 // source modules, decl order
}
struct ImportEntry { scope: ModPath, name: CompactString } // canonical target
```

**[call]** `names` is EXEMPT from `restore_lexical_env{,_mut}` — kept
from `self` like `by_id`, never rolled back by the module privacy
swap. Scope paths are globally unique, so the map is a per-ctx
registry of every module/block's import table: this IS the "table
materialized once, never stripped" invariant, and it is what makes
deferred resolution (P3) work — `ctx.env.names[def_scope]` equals the
captured `f.env.names[def_scope]` by construction. Mutation cases
that previously relied on `Use::delete` get explicit scrubs: dynamic
module recompile clears `names` under its scope before re-compiling;
`unbind_scope_subtree` scrubs it (LSP); the REPL compiles with an
import-replaces-import flag so re-`use` at the same scope shadows
instead of erroring (file modules keep the two-imports error).

### Resolution algorithm (as implemented)

Bare name from scope S, walking S, dirname(S), …, mod_root(S):
own decls (kind-specific: binds/typedefs/modules) → explicit imports
(one redirect, kind-checked at target) → globs (all sources checked;
two distinct hits = ambiguity error naming both). After mod_root:
core prelude (an implicit glob of `/core` — root_module_source drops
`use core`) → package prelude (module-kind lookups only).

Qualified paths: the leading segment resolves module-KIND-ONLY
through the same chain — **[call]** value and type binds are
invisible to path roots, exactly Rust's locals-don't-block-paths
rule, so `let array = …` never breaks `array::len`. A consequence:
the ruled package-shadowing refusal narrows to items that actually
enter the module namespace as MODULES (`mod array;` or a module
import named `array`); value/type items named like packages are
kind-disjoint and legal. Descent walks `modules` with privacy by
presence (the consulted env's view); interior imports are not
consulted (plain `use` is private — `pub use` re-exports are phase
2), except through `self::`/`super::` roots, which are inside the
privacy boundary.

Keyword roots resolve structurally: `self::x` → lookup at
mod_root(S); `package::x` → lookup at package_root(S) = `/pkg` when
S's first component is a registered package, else `/`. **[call]**
`super` is scope-relative, not module-relative: one `super` from
module M resolves at the SCOPE dirname(M) (chain-walked to its own
mod_root, no preludes), k supers iterate dirname∘mod_root. This is
what makes script files work — a loaded file's top level is the
`#do` block under root, and a submodule's `use super::x` must reach
the file's top-level lets, which live at that block scope. Guard:
`super` that would climb above package_root(S) is an error.
`package::` from a script resolves at `/` (root program scope), NOT
the file block — file-top items are reachable via counted `super`s
only; documented wart.

`Env.package_roots: Set<ArcStr>` is populated by `defpackage!`'s
generated `register` (threading the embedder-local `root_mods` into
the env at last).

### Import compilation

`Use` compiles to a static env mutation + a `Nop` node; the reactive
`Use` node (struct, `NodeView::Use`, `stop_use_in_scope`) is deleted.
Each item resolves every MODULE segment eagerly (headers
pre-registration below makes forward `mod` names visible), then:
**[call]** the FINAL name's existence is checked eagerly when the
target already has it, else the entry is stored deferred and the
error surfaces at first use ("x is not defined — imported from m,
which has no x"). Necessary because a `use self::sub::x` may
legitimately precede `mod sub;` in body order. Glob items validate
the module path only. Renames key the entry under the `as` name;
`use m` alone stores a module-kind entry {parent(m), m}.

### Headers pass (narrow)

Before compiling a module body's statements, one AST scan
pre-registers `mod` NAMES into `env.modules` (both the sig-bearing
and sig-less arms, into the compiling env — fixing today's
sig-less-inserts-after asymmetry and the mid-compile resolution
horizon: `list::List` inside tui resolves the sibling submodule, not
the list package, because own decls precede the package prelude).
Values are NOT pre-registered: forward value references remain
errors — body evaluation order is semantics in a dataflow language,
so the gxi ordering rule dies only for types/modules/visibility, not
for value initialization order.

### Migration order on the branch (each step buildable)

1. Marked components (green, semantics-neutral).
2. Mechanical use-rewrite, valid under BOTH semantics: every old
   (absolute, module-targeting) use item `a::…::m` becomes
   `a::…::m::{self, *}` (single-segment package opens just
   `pkg::*` — the self-import would collide with the package
   prelude). Under the old bridge `{self, *}` = open twice ≡ open;
   under the new rules `self` restores qualified access and `*`
   restores bare access. Green before the flip.
3. The flip: names table + new lookup core + keyword roots + preludes
   + static Use + delete `used`/`find_visible`'s walk. Fix forward
   (missing `use super::…`, etc.) until green.
4. Book examples migrated + hand-tightened; fuzzer generator vocab.

## P2/P4 as-built deviations (branch `module-system`, 2026-08-22)

The flip is BUILT and the whole workspace is green on it (compiler
152, graphix-tests 2028, gui 163, tui 69, fuzz 57, lsp 29, examples
+ shell suites; vendor tests excluded until the netidx release).
Deltas from the plan above, each cheap to revise:

1. **The package-shadow refusal is DROPPED** — superseded by
   precedence: own decl → explicit import → glob → package prelude →
   core prelude, first hit wins, kind-filtered (path roots consult
   only module-kind entries, so `let array = …` never breaks
   `array::len` — Rust's locals-don't-block-paths rule). The refusal
   proved untenable against the tree's own API surface: `tui::list`
   is a stdlib module named like the list package, and examples
   `use tui::list`. Importing a package under its own name is a
   no-op (the prelude already provides it). **[REVISES the answered
   open point 1 — flagged for review.]**
2. **Decl/import collision is asymmetric**: a `use` colliding with
   an existing same-scope declaration errors at the import; a
   declaration AFTER an import shadows it silently — `let`
   re-binding is idiomatic graphix and own-first precedence makes it
   well-defined. The REPL compiles with `CFlag::ReplaceImports`
   (re-`use` shadows, like `let` re-binding).
3. **Kind fallthrough**: an explicit import covers only the kinds
   its target has; a kind-miss falls through to globs. Load-bearing
   for the widget-module pattern (`use gui::text::{self, *}`: the
   module name is the explicit entry, the same-named val arrives by
   glob).
4. **`super` is scope-relative with a root-aware guard**: one
   `super` from module M anchors at the SCOPE dirname(M) — which in
   a loaded script is the file's top-level block, so `use super::x`
   reaches script-level `let`s — and resolves along that anchor's
   chain. A depth-1 user module's parent is the root scope (the
   program IS the package); climbing above `/` or a registered
   package's root errors. Check mode (statements at root) and load
   mode (the `#do` wrapper) agree.
5. **Headers passes**: `compile_block_children` pre-registers a
   block's `mod` NAMES before compiling children (declaration order
   carries no visibility meaning; `predeclared_mods` keeps the
   duplicate-module guard honest), and `bind_sig` pre-registers sig
   `mod` items the same way. The private-env snapshot additionally
   gets the module's own path inserted (it predates `bind_sig`, and
   submodules resolve package-rooted paths through it).
6. **The package prelude gates at the DESCENT, not the root**:
   `package_roots` membership alone answers the leading segment — a
   sandboxed dynamic-module env may keep `/sys/net` without `/sys`.
7. **Use segments accept type names** (values/modules are lowercase,
   types uppercase; an uppercase interior refuses at resolution),
   ditto `as` targets. P1's `fname`-only segments couldn't spell
   `use super::{Client, Response}` — found by the stdlib itself.
8. **Deferred-existence imports**: every module segment of a use
   path validates eagerly (headers make forward `mod`s visible); the
   terminal name may not exist yet (`use self::sub::x` before
   `mod sub;`) — the entry is stored and re-checked when the
   enclosing top-level statement finishes compiling, so a typo'd
   import still errors, at the right position.
9. **`Env.names` is the table** (`imports: name → ImportEntry
   {scope, name, chain}` + glob source list per scope), a global
   registry exempt from the lexical swap exactly as designed;
   `super`-anchored entries carry `chain: true` (the anchor may be a
   block level whose items live across its chain), and a
   `super::*` glob expands to one source per chain level at use
   time. Glob ambiguity (two globs providing one name) errors at
   first use naming both sources — it immediately caught two REAL
   latent collisions the old walk resolved silently by search
   order: `window` (gui widget vs array::window) and `Table`
   (gui::data_table vs sys::net) in the data-table examples.
10. **Finding 1 was load-bearing inside the stdlib**: gui's own
    `mod.gxi` referenced `Palette`/`StyleSheet` bare with no import
    — under open semantics those refs resolved through whatever the
    CONSUMER happened to open. Now spelled
    `use self::style::{Palette, StyleSheet};` at the def site.
    `resolve_pure` logs structural resolution errors before mapping
    them to `None`, so an ambiguity can't masquerade as "undefined
    type".
11. **Migration mechanics**: stdlib submodules got a mechanical
    `use super::*;` (they were already ordered for the old implicit
    rule; explicit tightening is the arc-end pass), inline test
    fixtures the `{self, *}` spellings, and the fuzz harness
    prepends `use super::*; ` (same line — subject positions
    preserved) to `/test.gx` so generated subjects keep reading
    schedule-injected inputs across the module boundary. The arc-end
    tightening pass (2026-08-22) then replaced every mechanical glob
    in the stdlib and the examples with the explicit list of names
    actually referenced; the widget-module `{self, *}` idiom is kept
    (a module and its main function sharing a name is what the
    spelling is FOR) and is documented in the book's Use chapter.
12. **Keyword roots parse in TYPE position** (arc-end fix): `typath`
    initially lacked `path_root`, so `-> super::m0::T` was a parse
    error while the doc promised expression/type-position parity.
    `path_root()` now leads `typath`; resolution needed nothing.
    Covered by `keyword_rooted_typath` (parser test), keyword leads
    in the roundtrip generators (`path_lead`), and the tree-sitter
    `type_path` rule.
13. **A dynamic module's `source` expression compiles in the
    ENCLOSING scope** (arc-end fix, semantics call flagged for Eric).
    The flip left the whole `mod foo dynamic { … }` block compiling
    under `foo`, which made `let src = …; mod foo dynamic { … source
    src }` inexpressible — inside a block no spelling reaches the
    block-local (`super::` names module items only), a real
    capability regression the fuzzer's reactive dynamic-module arm
    caught (0/9 dead arm). The source expression is LOADER-side code,
    so it now compiles against the enclosing scope (the sig still
    binds under `foo`; sandbox items were already reachable through
    the package prelude). Reactive gen-check: 95.5% → 100%.

## Open questions (collected)

1. Package-name shadowing: refuse (recommended) vs allow + `::x`
   absolute paths.

Eric: Ok with this

2. Keyword reservation: `self`/`super`/`package` fully reserved
   (recommended) vs contextual-in-paths only.

Eric: Ok with this
  
3. Re-export spelling `pub use` in `.gxi`, parsed-and-refused this
   arc — confirm the spelling so the grammar doesn't move twice.

Eric: Ok with this
  
4. Migration style for graphix-tests fixtures: mechanical globs
   (recommended — 2000+ fixtures, tightening is noise) vs explicit
   lists everywhere.

Eric: Ok with this for test, but the examples are use facing and should
be tightened explicitly.
