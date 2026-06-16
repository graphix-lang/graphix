# Fusion / lowering split — fusion as a pure analysis pass (PROPOSED)

> Status: **proposed, not built.** Captured 2026-06-15 from a design discussion
> (Eric + Claude). The driver is *legibility* at least as much as performance —
> see "Why this matters" at the bottom.

## The problem

Today fusion and lowering are welded together in `fusion::try_fuse`. One call
does two conceptually different jobs at once:

1. **Analysis (fusion):** decide what becomes a kernel — find the maximal sync
   region, collect its free-var inputs/feeders, derive the `KernelSig`, resolve
   static call sites, work out HOF-callback structure, locate the async
   boundaries.
2. **Lowering:** actually emit CLIF (recurse `emit_clif` from the region root)
   and hand it to cranelift.

The weld is explicit and deliberate — the design slogan is *"is it fusable IS
the compile attempt"* (`design/distributed_jit.md`). Kernel membership is
decided BY trying to compile: if `emit_clif` succeeds for every node in the
region it fuses; if anything bails, the region node-walks. That collapse bought
a real thing — it killed the bug class where a separate "can this fuse?"
predicate drifts from "does this actually lower?" (the predicate kept saying
*yes* where the lowerer then choked).

The cost is legibility: you can't read the fusion *decision* without also
reading the *codegen*, and you can't inspect "what would fuse" without invoking
cranelift. Two concerns are tangled in one function.

## The proposed shape

Split into two passes with a **data seam** between them.

- **Fusion = a pure analysis pass.** Walk the typed node graph and *color* it:
  give each fusable node a `KernelId`, and build a per-kernel **descriptor**
  stored in `ctx`, keyed by `KernelId`. The descriptor carries everything
  lowering needs and that's expensive to work out: member nodes, inputs/feeders,
  the `KernelSig`, static-call resolutions, callback structure, boundaries.
  This pass does **not** call cranelift. Its output is *data* — the fusion plan.

- **Lowering = a consumer.** For each `KernelId`, walk its member nodes, emit
  each via `emit_clif`, and wire the signature from the descriptor. Lowering's
  one job is cranelift emission; it does no analysis, because the analysis is
  already sitting in the descriptor.

**The node graph stays the IR.** The descriptor is *annotation on the existing
graph*, not a re-encoding of the computation into a new vocabulary. Each node
keeps its full-expressiveness `emit_clif`. This is the key difference from GIR
(deleted; see `design/distributed_jit.md`): GIR re-encoded operations into a
closed op-set and died of the vocabulary tax. Coloring + descriptors give you
the IR's *separation* without the IR's *parallel-language* cost — because the
nodes already are the language. It's the analysis-pass-annotates /
codegen-pass-consumes split most compilers use, with the twist of keeping the
node graph as the IR rather than lowering to a second one.

## The hard part — the relocated weld

Splitting reintroduces the drift the weld prevented: fusion colors node X into
kernel K, then lowering can't emit X — the plan promises a kernel the lowerer
can't build. Handling, in order of how much it covers:

- **Most boundaries are structural.** A timer / subscribe / `connect` is async,
  and a node knows that without compiling. The coloring reads it off the node.
- **Most emit-failures are type-determinable.** `try_fuse`'s existing
  pre-cranelift gates — identity rejection, representable-return via
  `freeze_concrete` / `abi_kind` — already decide a large fraction without
  touching the backend.
- **Strategy:** lift every `emit_clif` bail condition into a fusion-time gate,
  so the coloring is accurate by construction. The residual tail (a node that
  passes every gate and *still* won't emit) gets a **coarse fallback**: that
  kernel node-walks.
- **Drift guard:** the differential oracle (node-walk vs jit) + FuseExpect. An
  ungated bail shows up as a lowering failure and the oracle catches it. The
  thing that "kept being wrong" historically is made safe this time by gating
  *deliberately* and letting the existing test net scream on any miss.

The bound to keep in mind: the separation is clean exactly to the degree the
coloring is honest about emittability. Where it isn't, you eat coarse fallback —
a bounded, well-guarded cost, not the unbounded drift of an unguarded predicate.

## The wrinkle — fusion isn't purely compile-time

The impure-HOF per-slot path fuses *at runtime*: `MapQ::update` runs
`fusion::jit_node` on a `clone_rebind`'d template body (the path the
`splice_child` deletion rerouted, 2026-06-15). So "a pass before lowering" is
really "a pass before *each* lowering," sometimes mid-execution. The descriptor
model still holds (color the template, build its descriptor, lower it), but the
analysis pass and the descriptor type must be invocable from `update()`, not
only `compile()`. Draw the box around "the fusion pass" with that in mind.

## Payoff

- Lowering does one legible job; analysis and codegen no longer interleave.
- The fusion plan is inspectable data — debugging, FuseExpect / `FusionStats`
  become direct reads of the plan; structural-hash kernel caching becomes
  possible; a second backend becomes possible.
- Each concern is testable in isolation.
- Above all: **understandable.** Read the fusion pass and you know the plan;
  read lowering and you know the emission. They don't bleed into each other.

## Bonus: it's also faster (no speculative emission)

Today fusability *is* "did it emit," so `try_fuse` emits a region's CLIF
speculatively: it emits the whole subtree, and if the region turns out to
contain an async node it bails, throws the emission away, and the recursion
re-emits the smaller sub-regions it then tries. A sync computation with a buried
async cut above it gets emitted more than once — roughly O(nodes × speculation
depth) on the *mixed* regions. Wholly-sync regions already emit once, so they're
unaffected; it's specifically the regions that don't fuse whole that bleed.

The analysis pass decides the kernel boundaries *without emitting* — structural
(async nodes) plus the type gates `try_fuse` already runs — so lowering emits
each final kernel exactly once. One O(nodes) coloring walk replaces the
trial-and-error re-emission.

Honest scope: the discarded work today is mostly *emission*, not full
compilation — an `emit_clif` bail aborts the build before cranelift turns it into
machine code, so it's the cheaper half that's wasted. Real, but emission-real.

The point worth keeping: this is the same weld charging twice. The speculative
re-emission and the analysis/codegen tangle have one root cause, which is why a
single split buys both — *cleaner and faster from the same move*, which almost
never happens.

## Why this matters (the real driver)

Eric reviews this code now rather than writing it — Claude writes it. So
legibility isn't a nicety; it's the condition under which Eric stays the
architect of his own project. A function that does analysis and codegen at once
is hard to follow; two passes with a data seam between them are not. When the
choice is "clever and dense" vs "two honest passes you can read," this is the
project where you pick the second. (See the working-relationship note in
`~/.claude` memory: `feedback_clarity_as_participation`.)
