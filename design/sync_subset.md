# The synchronous subset — repatriating control from Rust

Status: **design sketch, v2** (2026-07-09, Eric + Claude). Nothing here
is built. The near-term per-instance-identity fix for lifted connect
targets (`KernelSig::lifted`, state-buffer identity words) is landed
separately and is compatible with — but not dependent on — this
design. v2 supersedes the v1 sketch: `sync` moved from a function
coloring to a block expression, effects left the surface type system
entirely, and mutable data structures resolved to *mut places* rather
than a parallel type universe.

## The observation that seeds this

Rust is a fine synchronous subset for **computation** and a bad one
for **control**.

The test is what crosses the FFI boundary. When a builtin receives
graphix *values* and returns graphix *values* (`str::sub`,
`buffer::decode`, `math::clamp`), the `BuiltIn` trait is an honest
foreign function interface and Rust is the right tool. Nothing about
those builtins ever felt hacky, and nothing here proposes moving them.

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
aliasing). Each is the same disease: cloning a live tree forces
re-derivation of identity relationships that fresh compilation gets
right by construction.

The language already has one honest instantiation mechanism: lambda
binding. `LambdaDef.init` compiles a body fresh per bind — fresh
`BindId`s, fresh wake registration, correct scope resolution, no
remap, no minting-authority puzzles, no identity baked anywhere.
"Quote + force" is already in the language, spelled `|args|` and
application.

## The design (v2): `sync` is a block, effects are invisible

The running example — `array::map` written in the language:

```graphix
val map: fn(a: Array<'a>, f: fn('a) -> 'b) -> Array<'b>;

let map = |a: Array<'a>, f: fn('a) -> 'b| -> Array<'b> sync {
    let mut tmp = [];
    for v in a {
        array::push(tmp, f(v))
    }
    tmp
};
```

Three commitments, each an ergonomics decision with semantic teeth:

1. **`sync { … }` is a block expression, not a function color.** The
   fn type is the ordinary `fn(…) -> …` — `.gxi` signatures are
   unchanged from today, HOF types don't fork, and users write `sync`
   only when they want loops and mutation, the way they write `try`
   when they want a catch.

2. **Effects never appear in the type system.** The compiler already
   infers them (`analysis::infer_effects`, the M6 HOF effect join);
   they stay compile-time facts. No effect variables, no second
   inference lattice at the surface. Diagnostics must compensate: an
   error arising from an inferred effect names the effect's SOURCE
   ("`f` is async because it uses `sys::time::timer` at …").

3. **`sync` promises sequential SEMANTICS, not synchronous
   execution.** The block means "evaluate in written order; mutation
   allowed." The compiler chooses the elaboration per call site:
   - `f` sync → the whole block compiles to one kernel. This is
     today's scaffold loop, verbatim — `emit_map_loop` already emits
     exactly the body above (`buf_new → push → finalize`).
   - `f` async → each `f(v)` instantiates a subgraph (lambda binding,
     the identity-correct path) and its result is an *eventual value*;
     `tmp` collects eventual values; the array that escapes the block
     fills in as elements arrive. This is exactly MapQ's semantics
     today — and a fold's accumulator chain over eventual values is
     exactly FoldQ's slot chain. The sequential source is the single
     specification both elaborations implement; the impure-HOF split
     becomes a typed, compiler-chosen elaboration instead of a fusion
     heuristic.

### Re-evaluation, taint, and the elaboration ladder

The v2 staging rule ("eventual values are data, not control") was too
strong — `filter` breaks it immediately:

```graphix
let filter = |a: Array<'a>, f: fn('a) -> bool| -> Array<'a> sync {
    let mut res = [];
    for v in a {
        select f(v) {
            true => array::push(res, v),
            false => ()
        }
    }
    res
}
```

The `select` scrutinizes `f(v)` — control on an eventual value — and
async filter must work. The repair is better than the rule:

**The async-elaborated block is a reactive node; it RE-EVALUATES as
slot values land, and pending values are TAINTED.** First evaluation:
the loop runs (control up to the `f(v)` calls depends only on sync
values), each `f(v)` instantiates a slot, each pending result is a
#219-tainted value; the select consumes taint, `res` taints, the block
produces nothing this cycle. Deliveries land → the node re-runs →
selects evaluate on real bools → the output fires. A later `f(v)`
update re-runs the block again — exactly MapQ's living-slots behavior.
No staging analysis: evaluate under the existing taint algebra, let
taint gate the output.

What survives of the rule is weaker and more precise: **the set of
async call sites executed must not depend on eventual values** (else
the call set is unstable across re-runs). Filter and fold pass (every
call runs unconditionally; results are only consumed). What trips it —
`for v in a { if f(v) { g(v) } }`, both async — falls to rung 3 below.

**The elaboration ladder** (every rung already exists today in some
form):

1. **All-sync** → one kernel: today's scaffold loop.
2. **Async, stable call set** → per-call-site slots + block
   re-evaluation under taint: today's MapQ/FoldQ slot-and-finish
   model, compiler-generated.
3. **Control-dependent async** → per-element fallback: the loop body
   becomes a lambda instantiated per iteration, its internal
   sequencing handled reactively inside the slot — precisely today's
   per-slot model, demoted from the only case to the degenerate case.

**Slot identity across re-runs**: `f(v)` on re-evaluation must find
its EXISTING slot, not re-instantiate; when `a` changes the slot set
diffs (grow/shrink). Keyed by (call site × iteration index) in a
table — the identity question clone_rebind answered with tree surgery,
answered with a map.

**Mut places under re-evaluation** are naturally safe: per-run
scratch, reinitialized each run — the block stays pure modulo its own
locals.

### Mutation: mut PLACES, not mut types

`let mut tmp = []` does not introduce a `Vec`. It introduces a mutable
*binding* — a place — holding an ordinary `Array<'b>`. Mutating ops on
a mut place (`array::push(tmp, x)`) mean `tmp = array::push(tmp, x)`
semantically, executed in place when the runtime can prove sole
ownership (refcount-1 steal — ValArray and the chunkmap are refcounted
persistent structures, so this is classic COW). Behind the curtain a
mut Array place keeps builder slack so repeated pushes aren't O(n²),
materializing on read/escape — the scaffold's `buf_new → push →
finalize`, promoted from emission detail to the meaning of a mut
place.

Consequences:

- **`Vec` lives nowhere.** No `vec::` module, no `to_array` — the
  freeze is the place's ESCAPE (return, capture, assignment out), and
  what escapes is a plain `Array<'b>`. One type universe.
- **Aliasing collapses**: places aren't values; you can read one
  (materializes a value) or update it, never alias it. Escape analysis
  is lexical. No affine type system needed.
- **User-defined mutable structures** split along the leaf/control
  line: mutation *ergonomics* over existing types comes free (mut
  struct places with field updates, mut Map places — every persistent
  type gets in-place update in sync blocks). Novel *representations*
  (ring buffers, specialized tables) stay where representation
  innovation already lives: Rust leaf builtins behind an abstract
  type. `buffer::` is the shipped proof — a user-facing mutable sync
  structure as Rust ops over an abstract type. Computation, not
  control: the sanctioned side of the boundary.
- **Coherence with refs and the cached ruling**: the reactive layer
  already has mutable places — ref cells, `*r <- v`. `let mut` is
  their sync-local cousin with the opposite visibility rule, and the
  rules compose into one story: WITHIN a sync block, mutation is
  immediate (sequential semantics); anything crossing OUT of sync into
  reactive land is a delivery, visible next cycle (the 2026-07-09
  cached-follows-deliveries ruling).

### What remains in Rust

The honest reactive primitive MapQ/FoldQ were hiding: **dynamic
children** — a combineLatest over N live subgraphs that grows and
shrinks with a collection, now an internal target of the async
elaboration rather than a user-visible builtin. Plus every leaf
builtin, unchanged. No user-facing quote/force type is needed in this
formulation — the eventual-value machinery is elaboration-internal
(internally it is a `LambdaDef`, as ever).

## Why this kills bug classes rather than bugs

- One loop semantics. Today the node-walk's MapQ slot machinery and
  the JIT's scaffold loops are two implementations of iteration the
  differential fuzzer must hold together (empty-input shortcuts, slot
  over-fire, per-slot latch state, firing inheritance — a large slice
  of the findings corpus). A language-level loop has one semantics
  both evaluators share — and the node-walk finally gets loops,
  closing the evaluator asymmetry from the canonical side.
- No live-tree cloning. Per-element instantiation is lambda
  application; the six clone-identity bug classes become structurally
  impossible.
- The boundary is checked. EFFECT/STATELESS consts, the emit
  contracts, per-slot firing rules — convention-enforced invariants on
  control-carrying builtins — become compiler-checked facts about sync
  blocks and the staging rule.

## Cost accounting

- Pure callbacks: instantiate nothing (today: one template clone per
  slot even when fully fused). Strictly cheaper.
- Async elements: one fresh body-compile per slot at creation instead
  of one tree clone — heavier per slot, one-time, dwarfed by the async
  work the slot exists for. If profiling ever objects, a def-level
  compiled-body cache is the honest template optimization: shared
  code, identity in per-instance state — precisely the shape the
  per-instance-identity fix already gives kernels.
- Bottom semantics come free: the sync value domain is `['b, ⊥]` with
  the #219 taint algebra — the JIT's existing rules transfer
  wholesale. (The pending tail-arg-stale-cache ruling is this question
  in tail-loop clothing; one answer should cover both.)
- Divergence: `for` over a collection terminates; `while` doesn't have
  to. Inherit the JIT's position (an infinite pure loop hangs the
  cycle — correct) and its mechanism (the cooperative interrupt poll).

## Worm ledger (v1 worms → v2 resolutions)

| v1 worm | v2 resolution |
| --- | --- |
| Function coloring (`sync fn`) | Gone from types: `sync` is a block; effects inferred, never written. |
| Calling reactive `f` from sync | Re-evaluation under taint + the elaboration ladder; only the CALL SET must be eventual-independent (rung 3 catches the rest). |
| `Vec` + freeze API | Mut PLACES over persistent types; freeze = escape; no `vec::`. |
| Aliasing / affine types | Places aren't values; no aliasing exists to restrict. |
| Bottom mid-loop | #219 taint algebra, unchanged. |
| Surface effect variables | Avoided entirely (inference-only was v1's recommendation; v2 makes it structural). |

## Open questions

- `<-` inside a sync block: a connect would re-fire per re-evaluation.
  Forbid in v1 of the feature, or accept ordinary connect semantics
  (fires when its RHS fires, which now includes re-runs)?
- Re-evaluation cost: today's MapQ re-runs only `finish`; a naive
  block re-run re-loops everything. Fine for hundreds of elements;
  for 10^5 the incremental version is self-adjusting-computation
  territory — out of scope, noted.
- Call-set stability checking: syntactic (async calls not under
  eventual-dependent control) or flow-based (eventual-ness as a
  compile-time taint on places too — laundering through a mut place
  then branching)?
- Mut-place builder representations per type (Array slack buffer, Map
  COW handle, String rope?) and the materialize-on-read cost model.
- `for` syntax and iteration protocol (arrays, maps, ranges; does
  user code ever iterate an abstract type?).
- Diagnostics quality for inferred effects — the error must carry the
  effect-source chain or the invisibility becomes user-hostile.
- Migration: MapQ/FoldQ remain until the subset lands; the
  per-instance-identity fix keeps clone_rebind correct in the interim
  and dies with it.
