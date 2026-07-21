# The synchronous subset — repatriating control from Rust

> **SUPERSEDED (2026-07-13):** The `sync` language experiment was removed.
> Reactive collection HOFs are compiler-owned Nodes; see
> `design/collection_intrinsics.md`. The text below is retained as design
> history.

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

### Mutation: mut VARIABLES, persistent data, in-place by proof

All data structures stay exactly as they are — persistent, immutable
values. The only mutable thing is a *binding*:

```graphix
let filter = |a: Array<'a>, f: fn('a) -> bool| -> Array<'a> sync {
    let mut res = [];
    for v in a {
        select f(v) {
            true => res = array::push(res, v),
            false => ()
        }
    }
    res
}
```

`res = array::push(res, v)` is ordinary rebinding — the language has
sequential shadowing today; the only new semantics a `for` loop adds
is the rebind carrying ACROSS iterations. Hence the one-line
desugaring: **a `for` loop with mut vars IS a fold, the mut vars its
accumulator tuple.** The JIT already compiles exactly this — the
scaffold's fold loop carries the accumulator as a CLIF block
parameter; "in-kernel mutable variables" is what block params are.
Rung-2 elaboration inherits for free: loop-carried mut vars under
async are fold's acc chain (FoldQ's slot chain).

**The accumulator idiom is `List`, and array-building is a scoped
optimization — not a language promise.** ValArray is an Arc-wrapped
SLICE: it cannot grow or shrink, so there is no in-place push,
uniqueness or not. The honest semantic story: `array::push` copies
(as it does today), and the idiomatic O(n) accumulator is cons onto a
`List` and reverse (or `list::to_array`) at the end — that is what
List is for. Separately, the emitter may recognize the build-loop
PATTERN — a loop-carried `res = array::push(res, v)` whose only
in-loop uses are push/len — and keep `res` in builder form (a growable
scratch, `buf_new → push → finalize at escape`, verbatim what
`emit_map_loop` emits for the stdlib HOFs today), never materializing
a ValArray until escape. Any other use of `res` mid-loop materializes
and the copies are real. An optimization of a specific shape, kept out
of the semantics.

Consequences: no mut containers, no in-place ops with special
semantics, no `vec::`, no freeze API, no affine types. One new binder
(`let mut`), one new expression form (assignment to a mut local, sync
blocks only).

- **User-defined mutable structures** split along the leaf/control
  line: mutation ergonomics over existing types comes free (mut locals
  of struct/Map type rebound with functional update). Novel
  REPRESENTATIONS (ring buffers, specialized tables) stay where
  representation innovation already lives: Rust leaf builtins behind
  an abstract type — `buffer::` is the shipped proof. Computation, not
  control: the sanctioned side of the boundary.
- **Coherence with refs and the cached ruling**: reactive land already
  has mutable places — ref cells, `*r <- v`. `let mut` is the
  sync-local cousin with the opposite visibility rule, and the rules
  compose: WITHIN a sync block, rebinding is immediate (sequential
  semantics); anything crossing OUT into reactive land is a delivery,
  visible next cycle (the 2026-07-09 cached-follows-deliveries
  ruling).

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
| `Vec` + freeze API | Mut VARIABLES + persistent data; `for` = fold; List is the accumulator idiom; build-loop builder as a pattern-scoped emission optimization; no `vec::`, no freeze. |
| Aliasing / affine types | Rebinding, not aliasing; uniqueness is an optimization condition, not a type. |
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
- How far the build-loop pattern recognition goes (push-only is the
  stdlib-HOF shape; push+len is easy; anything more is probably not
  worth it given List).
- `for` syntax and iteration protocol (arrays, maps, ranges; does
  user code ever iterate an abstract type?).
- Diagnostics quality for inferred effects — the error must carry the
  effect-source chain or the invisibility becomes user-hostile.
- Migration: MapQ/FoldQ remain until the subset lands; the
  per-instance-identity fix keeps clone_rebind correct in the interim
  and dies with it.

## Prototype status (sync-subset-proto branch, 2026-07-09)

P0–P3 are BUILT and committed; the desugar prototype validates the
core bet — sequential semantics with zero new evaluation machinery.

- **P0 syntax** (2497ff7b + 3e97d0a1): `sync { }`, `let mut`, `x = e`,
  `for p in it { }`; tight-brace rule for MapRef; proptest round-trip.
- **P1 desugar** (f7795efb): functionalization in
  `expr/sync_desugar.rs` — assignment = shadowing, `for` = fold over
  the body's assigned set, arm-assigns hoist to tuple yields. No new
  node types; both evaluators inherit the semantics from one spec.
- **P2 fusion** (a236e65f): pure sync blocks compile to ONE kernel —
  ten `#[native]` fixtures (zero node-walk residue) over tuple,
  struct, array, string, and mixed accumulators. The enabling work
  was general vocabulary: `emit_fold_loop` grew composite/string
  accumulator carries (owned loop-carried acc with clone-borrowed /
  drop-replaced discipline), FoldQ freezes from the RESOLVED acc type,
  and `union_identical` derefs bound tvars against bare types.
- **P3 async** (6e9e383e): an async call anywhere in the body (arm,
  nested loop, iter, beside sync assigns) makes the desugared fold
  callback impure and rides MapQ/FoldQ per-slot machinery unchanged —
  rung 2 was free, differentially verified.

**P4 is gated on per-callsite elaboration.** An in-language
`array::map` written as a sync block is CORRECT today at multiple
instantiations (verified differentially), but compiles zero kernels:
the fold callback's call to the lambda PARAM `f` cannot statically
resolve in a once-compiled generic body (missed-fusion item 1). The
design's answer — the compiler elaborates a sync fn per call site
(sync arg → one kernel, async arg → per-element instantiation) — is
the piece that makes in-language HOFs match MapQ's native loops, and
it retires clone_rebind/MapQ/FoldQ when it lands. Needs a design
session before code.

**Pending rulings encountered by the prototype:**
- Unannotated `let mut res = []` in a MULTI-mut loop: the no-assign
  arm's element tvar is unbound when the arm-tuple union forms →
  `union.0` is a compile error. Annotation is the workaround.
  (fuzz/pending-ruling/sync_multimut_union_tuple_ref.md)
- Assignment in tail position is rejected (dead rebind) — currently
  via the compiler backstop; a dedicated error would read better.

## P4 design: per-callsite elaboration via param→lambda binding (2026-07-10)

The blocker was: a call to a lambda PARAM (`f(v)` inside `let map2 =
|a, f| sync { … f(v) … }`) can't statically resolve in a once-compiled
generic body, so in-language HOFs are correct but compile zero
kernels.

The insight from the jul09c queuefn investigation: the compiler
already elaborates per callsite — GXLambda instances are built PER
CALLSITE with the site-resolved signature (#18), `try_static_resolve`
already computes `fn_arg_targets` (fn-typed args that resolve to known
lambdas — it's how MapQ pre-materializes callbacks), and the #203
cascade already drives each bound instance's body through typecheck1
so nested callsites resolve. The ONLY missing link: the instance's
arg-pattern BindId for a statically-known fn arg is never entered in
`ctx.bind_to_lambda`, so the body's `f(v)` fnode Ref finds nothing.

P4 mechanism (small, because everything else exists):

1. In `resolve_static`, after `setup_bind` builds the per-site
   instance: for each statically-known fn-typed arg, look up the
   instance's arg-pattern BindId (`GXLambda::args[i].single_bind_id()`)
   and insert `bind_to_lambda[param_bind_id] = lambda_value` for the
   duration of the instance's typecheck1 cascade.
2. The cascade's body walk then statically resolves `f(v)` like any
   lambda binding — per-instance BindIds are fresh per callsite, so
   there is NO cross-site contamination by construction.
3. Fusion needs no new code: it reads resolved CallSites. A sync
   callback → the desugared fold's callback body inlines into the
   scaffold loop (one kernel per HOF callsite). An async callback →
   the effect lattice classifies the call impure → the sync block's
   impure-fold path (P3).

Then the stdlib HOFs rewrite in-language (`map`/`filter`/… as sync
blocks over `for`), and MapQ/FoldQ/clone_rebind retire — the jul09c
soak alone produced two clone_rebind-adjacent crashes (findings 35,
38); the per-slot cloning of analysis-built trees is the standing
hazard this ends.

### P4 layer 2 status (2026-07-10, in progress)

Layer 1 (semantics) is COMMITTED (8630436f): the param knot,
per-callsite resolution of fn-typed args, rigid cells never written
(bind-suppression — also fixed the `x + i64:1` monomorphization
quirk), and nested-gate fact scoping. `inlang_map` pins the semantics.

Layer 2 (the fusion elaboration, option (a)): `GXLambda` now has an
`Apply::fuse` override running region fusion over the per-site
instance body (reached via `CallSite::fuse` → `apply.fuse`). VERIFIED
reached; the body's region attempt currently fails with
"emit_clif: builtin call site not discovered" for the sites inside
the fold — the per-REGION builtin-site discovery pass does not cover
instance-body regions the way it covers top-level regions (suspected
ExprId-keyed map mismatch between the region's own nodes and the
analysis-pred bodies FoldQ hands to discovery — the same seam class
as the queuefn clone finding, corpus #38). Next: read the discovery
walker (`builtin_apply_sites` construction in try_fuse's analysis)
and either extend its descent or re-key. `GXDBG_P4=1` prints the
per-instance fusion outcome and the new failure entries.

## P4 final scope (Eric's ruling, 2026-07-10): eliminate FoldQ too

The finished product is the FULL repatriation — no MapQ, no FoldQ, no
clone_rebind anywhere. Never-until-complete is the loop's emission
contract (a loop whose body produced no value for some element — a
bottom, or an async value not yet arrived — emits nothing this cycle;
matches the old fold contract and the #219 loop-carried taint model).

Architecture (one new invention, everything else is deletion):

- **`For` node** (ExprKind::ForFold, desugar-internal): the sync-block
  desugar keeps its assigned-set analysis but emits ForFold { iter,
  init, acc_pattern, elem_pattern, body } instead of an array::fold
  Apply. Compiled to a real Update node:
  - Node-walk, SYNC body: sequential in-place loop — bind acc+elem,
    re-run the ONE body tree per element (the GXLambda tail_loop
    precedent). Sequential semantics are the semantics: a stateful
    Sync builtin (count) in the body is SHARED across iterations —
    that's what a loop means; the per-slot-counter behavior dies with
    the machinery that produced it.
  - Node-walk, ASYNC body: per-INDEX body instantiation — compile the
    body expr fresh per element index on demand (the NORMAL compile
    path; instances cached by index, never cloned), re-evaluate the
    whole chain from init whenever any instance fires, emit only when
    every element produced (never-until-complete).
  - JIT: For::emit_clif drives emit_fold_loop with the P2 FoldAcc
    machinery directly. Async bodies de-fuse naturally.
- **stdlib**: map/filter/flat_map/filter_map/find/find_map/fold
  written in graphix (sync blocks over for) in array.gx; existing .gxi
  signatures unchanged. init/group/iter/sort/len/push/concat stay
  Rust computation leaves. map::/list:: HOFs keep Rust impls this
  pass (for-over-Map needs type-directed desugar — later).
- **DELETE**: MapQ, FoldQ, MapFn, FoldFn, every per-HOF impl, analysis
  preds, per-slot templates, fused_template, the map/filter/find/
  flat_map scaffolds (emit_fold_loop + shared helpers remain), and the
  ENTIRE clone_rebind method tree — nothing clones compiled nodes;
  every instantiation is LambdaDef::init per (site, index).
- Tree stays red mid-refactor by design; verification at the end:
  suite + GRAPHIX_FUSE_AUDIT sweep + bench parity + fresh soak.
