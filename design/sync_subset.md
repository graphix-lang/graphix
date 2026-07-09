# The synchronous subset — repatriating control from Rust

Status: **design sketch** (2026-07-09, Eric + Claude). Nothing here is
built. The near-term per-instance-identity fix for lifted connect
targets (`KernelSig::lifted`, state-buffer identity words) is landed
separately and is compatible with — but not dependent on — this
design.

## The observation that seeds this

Rust is a fine synchronous subset for **computation** and a bad one
for **control**.

The test is what crosses the FFI boundary. When a builtin receives
graphix *values* and returns graphix *values* (`str::sub`,
`buffer::decode`, `math::clamp`), the `BuiltIn` trait is an honest
foreign function interface and Rust is the right tool — mature
optimizer, real loops, no ceremony. Nothing about those builtins ever
felt hacky, and nothing in this design proposes moving them.

When a builtin receives graphix *code* — `array::map(a, |x| ...)` —
it stops being a leaf. The Rust side now holds a loop whose body is a
graphix computation, and to run it it must become a mini-interpreter:
slot tables, per-element dispatch, update routing, cache priming,
runtime instantiation of node subgraphs. MapQ/FoldQ re-implement a
slice of the evaluator, in a second place, with their own identity
machinery. That machinery is `clone_rebind`.

## The indictment of clone_rebind

`clone_rebind` is runtime instantiation-by-copying: deep-clone a live
node tree, re-minting `BindId`s through a `RebindMap`, re-deriving
wake roots, scope resolution, and captured-name bindings as you go. It
exists so per-slot instantiation can skip re-typechecking and share
compiled kernel `Arc`s — a performance optimization, and it is used by
exactly one client: MapQ/FoldQ per-slot instantiation (every other
`clone_rebind` in the tree is interior recursion within those clones).

The empirical record: **six** independent bug classes to date are
clone-identity bugs — `clone-scope-resolution`, `clone-wake-root`,
`capture-name-alias`, `rebind-recompile`,
`lambda-instantiation-binding`, and finding 35 (per-slot lift
aliasing, where a lifted target's `Bind` was spliced into the shared
kernel and the clone had no node left to re-mint from). Each is the
same disease: cloning a live tree forces re-derivation of identity
relationships that fresh compilation gets right by construction.

The language already has one honest instantiation mechanism: lambda
binding. `LambdaDef.init` compiles a body fresh per bind — fresh
`BindId`s, fresh wake registration, correct scope resolution, no
remap, no minting-authority puzzles, no identity baked anywhere.
"Quote + force" is already in the language, spelled `|args|` and
application.

## The design

Build a synchronous sub-language inside graphix:

- **Loops and mutation.** Ordinary `for`/`while`-class iteration and
  mutable locals, legal only inside sync contexts. Termination is the
  programmer's problem exactly as it is in Rust; the runtime keeps the
  interrupt/runaway guards the JIT scaffolds already have (the
  cooperative interrupt poll, the `array::init` length guard) as
  language-level equivalents.
- **The boundary type.** A sync context may RETURN an async
  computation to the reactive superset as a first-class value —
  `Async<'b>` (name TBD), which is in essence a `LambdaDef`: a typed,
  compiled-but-uninstantiated body plus captures. Constructing one is
  quoting; the reactive layer instantiating it is forcing, and forcing
  IS lambda instantiation — the existing, identity-correct path.
- **HOFs become library code.** `array::map` with a pure callback is a
  sync loop — no slots, no template, no instantiation at all. With an
  impure callback, the loop runs the sync part inline and returns one
  `Async` per element; the reactive superset instantiates each through
  lambda binding.

What remains in Rust afterward is the honest reactive primitive
MapQ/FoldQ were hiding: **dynamic children** — a combineLatest over N
live subgraphs that grows and shrinks with a collection. That is
runtime work, it is small, and it carries no compiler smuggled inside.

## Why this kills the bug classes rather than the bugs

- One loop semantics. Today the node-walk's MapQ slot machinery and
  the JIT's scaffold loops are two implementations of iteration that
  the differential fuzzer must hold together (empty-input shortcuts,
  slot over-fire, per-slot latch state, firing inheritance — a large
  slice of the findings corpus). A language-level loop has one
  semantics that both evaluators share, the same way `a[i]`
  bounds-checking goes through one shared helper today.
- No live-tree cloning. Per-element instantiation is lambda
  application; the six clone-identity bug classes become structurally
  impossible.
- The boundary is typed. The EFFECT/STATELESS consts, the emit
  contracts, the per-slot firing rules — invariants currently enforced
  by convention on control-carrying builtins — become type-system
  facts about what may appear in a sync context and what an `Async`
  may capture.

## Cost accounting

- Pure callbacks: instantiate nothing (today: one template clone per
  slot even when fully fused). Strictly cheaper.
- Async elements: one fresh body-compile per slot at creation instead
  of one tree clone. Heavier per slot, one-time, and dwarfed by
  whatever async work the slot exists to perform. If profiling ever
  says otherwise, a def-level compiled-body cache is the honest
  version of the template optimization (share the compiled artifact,
  instantiate only identity) — which is precisely the shape the
  per-instance-identity fix already gives kernels: shared code,
  identity in per-instance state.
- The fusion story converges rather than expands: the sync subset is
  what the fusion scaffold already compiles (`emit_map_loop` IS this
  sub-language, machine-generated). Surfacing it as syntax is parser
  and typing work, not a new backend — and it hands the NODE-WALK
  loops too, closing the evaluator asymmetry from the canonical side.

## Open questions

- Syntax and typing of sync contexts: a block form? a function
  coloring (`sync fn`)? How does purity interact with `Async`-mention?
- Mutation semantics at the boundary: writes from sync code to
  reactive variables are deliveries (next cycle), per the 2026-07-09
  cached-follows-deliveries ruling; mutable SYNC locals never escape.
- `Async` typing: capture rules (by value at quote time?), and whether
  an `Async` is a Value (it must be, to ride collections) — the
  LambdaDef Value wrapping already does this.
- Migration: MapQ/FoldQ remain until the subset lands; the
  per-instance-identity fix keeps clone_rebind correct in the interim
  and dies with it.
