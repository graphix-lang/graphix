# Whole-graph fusion via sync/async effect tracking

A plan for extending the per-lambda fusion + KIR + JIT pipeline
(`design/fusion_architecture.md`) to fuse arbitrary sync subgraphs of
the dataflow, not just lambda bodies. Captures the design we converged
on in conversation; open items called out as such. Last updated
2026-05-05.

## Motivation

Per-lambda fusion has hit its ceiling. From the bench data in
`fusion_architecture.md`:

- KIR JIT is 50-70× over the node graph on mandelbrot, but **only 5-15%
  over the interpreter** on most workloads. Per-call overhead dominates
  inner-loop work.
- Vectorization-shaped benches (`sum_of_squares`, `dot product`) show
  no meaningful JIT/interp gap because `array::init` / `array::fold`
  per-element dispatch eats the kernel's win.
- Mandelbrot's 49× speedup is misleading: it's mostly because the
  per-pixel callback was an interpreted GXLambda before fusion. The
  inner iterate loop is a small fraction of total time; array
  machinery dominates the rest.

The bottleneck is **dispatch granularity**. The runtime orchestrates
every node: each `array::init` element runs through `Apply::update`,
arg marshalling, `KirNode::update`, `KirNode::evaluate`, with `Value`
boxing on every input/output. The fused kernel is fast; the path to
and from it is not.

Whole-graph fusion absorbs the surrounding dataflow into the kernel.
A sync chain like `array::map(|x| x + 1, arr)` compiles to one kernel
that contains both the `map` loop *and* the per-element body. The
runtime only mediates at genuine cycle boundaries — async-effect
builtins (timer, subscribe, IO) and unstable-binding reads.

This is the path to closing the array-dispatch ceiling and reaching
AOT-equivalent perf in JIT.

## Effect classification

### Async function effect

A function is **async** if any output it produces can appear on a
cycle later than the cycle the trigger arrived on. Concretely, async
builtins are the ones that introduce a cycle boundary:

- **Time-driven**: `sys::time::timer` (one-shot or repeating)
- **Network**: `sys::net::subscribe`, `sys::net::publish`,
  `http::*` request/response, RPC calls
- **IO**: `sys::io::read`, `write`, `read_exact`, `write_exact`,
  `flush`; `sys::fs::*` async operations; filesystem watchers
  (`sys::fs::watch::*`)
- **Buffering / scheduling**: `queue`, `throttle`
- **GUI/TUI callbacks**: any widget callback fires on the cycle the
  user interacts, which is necessarily a later cycle than where the
  widget was constructed

### Sync function effect

A function is **sync** if every output it produces is on the same
cycle as the input that triggered it (or it produces no output for
that input). Sync includes everything that isn't on the async list:

- **Pure**: arithmetic, comparison, logic, `cast<T>`, error
  construction/handling (`error`, `is_err`, `filter_err`, `?`, `$`)
- **Sampling / control flow**: `~` (sample), `select` (regardless of
  arm sleep — sleep determines *which* arm runs, not *when* it
  produces)
- **Stateful sync**: `count`, `sum`, `mean`, `min`, `max`, `product`,
  `hold`, `take`, `skip`, `uniq`, `all`, `and`, `or`, `once`,
  `filter`. State across cycles becomes a kernel-local register.
- **Data structures**: all `array::*`, `str::*`, `re::*`, `map::*`
  operations over current values
- **References**: `&x` (creation), `*r` (deref), `*r <- v` (write
  through ref) — see references section for nuance

The state-holding sync ops (count, sum, hold, etc.) are the
interesting case: they maintain state across cycles, but their
input → output relationship is same-cycle. The state lives in the
kernel as a register the kernel updates on each invocation.

### `<-` is sync; reads of unstable bindings are async

The connect operator itself is sync — `x <- expr` evaluates `expr`
and schedules the write all in current-cycle work. The async edge
lives at the *read* of any unstable binding.

A binding is **unstable** if any `<-` somewhere in the program
targets it. This set is already tracked in the codebase as
`ctx.unstable_bindings`, populated by `fusion::scan_connect_targets`.

Reading an unstable binding produces an **async-tagged value**
because the value observed is "previous-cycle x". Observable
demonstration: `{ let x = 0; x <- x + 1; x }` evaluates to `0` on
cycle 0 and `1` on cycle 1. The block's return value is cycle-shifted
relative to a trigger, so reads of x must be tagged async.

This placement (async on the read, not on the write) is deliberate:

1. It aligns with the existing `unstable_bindings` infrastructure.
2. It lets `<-` writes appear as the *final operation of a sync
   kernel* — read stable inputs, compute, schedule write, all
   current-cycle.
3. Async-ness propagates through *value reads* — the natural
   composition through expressions and function calls.

### Async tag propagation

Async is a value-level property, propagated through every operation:

- A read of an unstable binding produces an async-tagged value.
- An async-effect builtin call site produces async-tagged values.
- Any operation whose input is async-tagged produces async-tagged
  output (transitive).

The tag is internal to the compiler. User-facing types do not surface
it — printed types stay simple. The propagation is automatic during
effect inference (M6 below).

### Function effect inference

A function's classification has two parts: an **intrinsic effect**
(determined by the function itself) and a **call-site effect**
(determined per call by joining the intrinsic effect with the
effects of fn-typed arguments at that site).

#### Intrinsic effect

A function is **intrinsically async** if its body, treating fn-typed
parameter calls as having no effect contribution, contains any of:

- An async-effect builtin call (timer, subscribe, IO, queue, etc.).
- A user-function call where the callee is intrinsically async.

Otherwise the function is **intrinsically sync**.

Reading async-tagged values inside the body does *not* make the
function intrinsically async. Reads produce async-tagged values used
internally and (potentially) propagated to the return value, but the
function's own work timing is unchanged. Standard effect-system
framing — a sync function can operate on effectful values without
itself being an effect-introducer.

Example: a closure that does `counter <- counter + 1` is intrinsically
sync. It returns null/unit, has no async-effect builtin call, and `<-`
is itself sync. The rhs reads `counter` (async-tagged because counter
is unstable), but that read produces an async value used only
internally — it never propagates to the closure's return.

#### Call-site effect (HOFs)

The rule (positive form): **a call site is sync iff the callee is
intrinsically sync AND every fn-typed argument passed at this site is
sync.** Otherwise async.

Equivalently as a join in the {sync, async} lattice (`sync ⊔ sync =
sync`, everything else is async):

```
callsite_effect = callee_intrinsic ⊔ ⨆(fn_arg_effects at this site)
```

This formula handles HOFs like `array::map(f, arr)` without
introducing first-class effect polymorphism in the user-visible type
system. Effects are compile-time only, inferred and propagated
automatically.

**Resolution mechanism**: peek at the resolved callee for each fn-
typed argument using the existing `apply_site_hint` machinery (which
already threads concrete FnTypes from `CallSite::typecheck` — see
`fusion_architecture.md:347-373`). Extend it to thread the callee's
effect alongside its type.

If the call site is sync, the HOF body inlines into the surrounding
fused kernel along with the resolved fn args. If async, fall back to
dispatch-style execution via `KirOp::DynCall` or equivalent.

#### Simple rule vs tight rule

**Simple rule** (use this): every fn-typed argument contributes its
effect to the call-site effect, regardless of whether the HOF body
actually calls it.

**Tight rule**: only fn-typed args that are called (directly, or
transitively passed to another callee that calls them) contribute.

These are functionally equivalent for any realistic HOF — HOFs that
take a fn arg and never call it are vanishingly rare and harmlessly
over-classified as async-leaning under the simple rule. Use the
simple rule; the tight rule isn't worth the analysis complexity.

#### Function values from unstable bindings

If a fn-typed argument's value comes from a read of an unstable
binding (or transitively derives from one), the value passed in is
async-tagged. We cannot statically know which actual function will
be invoked at runtime — the runtime might have any function in that
slot.

Conservative classification: such call sites are **async**, dispatched
via runtime indirection (`KirOp::DynCall`-style). Aligns with how
M4g v1's stability gate already handles unstable callees.

#### Composition

The call-site formula composes correctly through nested cases:

- **Nested HOFs**: `array::map(|arr| array::map(f, arr), arrs)` is
  sync iff `f` is sync. The inner closure's call-site effect tracks
  `f`; the outer `map`'s call-site effect tracks the inner closure.
- **Closures capturing functions**: `let helper = || f(42)` is sync
  iff `f` is sync at helper's definition site. If `f` is unstable in
  helper's scope, helper is async (helper's body reads `f` as an
  async-tagged fn value).
- **HOFs that return functions**: `let make_adder = |n| |x| x + n`
  has two distinct effects — make_adder's intrinsic effect (sync)
  and the returned closure's intrinsic effect (also sync). Calling
  make_adder uses the outer effect; calling the result uses the
  inner.

#### Self-recursion and mutual recursion

Standard fixed-point inference. Initialize every function's intrinsic
effect as sync; for each function, walk the body checking for async
edges (consulting recursive callees at their current state). Mark
async if any edge is found. Iterate until convergence.

Effects only flow from sync to async, never the reverse, so the
fixed point converges in O(N) passes for a program with N user
functions.

### Worked examples

```graphix
let f = |x| x + 1                        // sync (pure)
let g = |x| {
    sys::time::timer(duration:1.s, false);
    x
}                                          // async (timer call)

let stable = 42                            // stable binding
let unstable = 0                           // becomes unstable due to <- below
unstable <- input ~ unstable + 1

f(stable)                                  // sync output
f(unstable)                                // async-tagged value (sync fn, async input)
g(stable)                                  // async output (async fn)

let h = |fn, x| fn(x)                      // effect-polymorphic — resolved per site
h(f, stable)                               // sync at this site
h(g, stable)                               // async at this site

let increment = || unstable <- unstable + 1  // sync — returns null,
                                             // <- is sync, reads of
                                             // unstable produce
                                             // async values used
                                             // only internally
```

## Fusion model

The dataflow graph is partitioned into **sync subgraphs** separated by
**async edges**. Each maximal sync subgraph compiles to one fused KIR
kernel. The runtime orchestrates kernels through async edges using its
existing event-batch mechanism.

### Where async edges live

1. **Async-effect builtin call sites** — `timer(...)`, `subscribe(...)`,
   `read(...)`, `queue(...)`, etc. The builtin's output is the start of
   a new sync subgraph downstream.
2. **Reads of unstable bindings** — each such read fires when the
   binding updates, which is whichever cycle the binding's `<-` write
   landed on (one cycle after the write).
3. **Calls to user-defined async functions** — same as builtin calls;
   the call site is the boundary.

Everything else can be absorbed into the surrounding kernel.

### Kernel structure

A fused kernel:

- Has inputs: values flowing in from upstream async edges (async
  builtins firing, unstable binding updates) plus stable bindings and
  function args.
- Has internal state: registers for stateful sync ops (count, sum,
  hold, mean), captured environment refs.
- Has outputs: values flowing to downstream consumers + side-channel
  `<-` writes scheduling cycle-K+1 updates to unstable bindings.
- Runs synchronously on each invocation (one cycle of work).

The runtime sees a fused kernel as a single `Apply<R, E>` — the same
interface `KirNode` already exposes. No fundamentally new runtime
machinery; just bigger fused chunks.

### Cross-kernel calls

**Default: inline aggressively.** Within a sync subgraph, fuse every
sync operation into a single kernel. Across kernels, prefer inlining
the callee's KIR into the caller's kernel rather than emitting a
cross-kernel call. The KIR is dramatically faster than node-graph
dispatch per-op; the worst-case fused-and-inlined kernel is still
faster than the equivalent node graph hopping through `Apply::update`
+ HashMap variable bindings + `Value` boxing.

Separate kernels (using the existing `KirOp::Call` machinery from
M4d v2/v3) only when forced:

- **Mutual recursion** — A and B call each other; one of them must
  be a separate kernel to break the cycle.
- **Self-recursion** — `let rec f = ...` where `f` calls itself.
  The recursion compiles as one kernel with cross-kernel call to
  itself.
- **AOT-shared library code** (only relevant if AOT survives) —
  shared utility kernels deduplicated across an emitted package.

Cold-start JIT compile latency for very large kernels is addressed
by the existing async-JIT worker path: interpret while compiling,
swap in when ready. Steady-state perf always favors more fusion.

### References

References (`&x`, `*r`, `*r <- v`) propagate the unstable property
through the type. A reference to an unstable binding carries that
unstable-ness; dereferencing produces an async-tagged value.

When a reference is a function parameter, the static analysis can't
always tell which binding it targets. Be conservative: treat any
function-parameter reference as potentially unstable. Loses precision
in code that threads refs through many layers; address in v2 with a
flow-sensitive analysis if it matters in practice.

### Multi-arm select

`select { arm1 => ..., arm2 => ..., ... }` produces an output from
whichever arm matches. Effect of the whole select is the *union* of
arm effects: sync iff every arm is sync, async if any arm is async.

Static analysis can't generally tell which arm runs, so we take the
worst case. This is conservative but accurate.

## Implementation plan

The plan extends existing infrastructure rather than replacing it.
Each milestone is independently testable and shippable. Order
chosen to keep benchmarks running at every step.

### M5 — Builtin effect classification

**What**: Add an `EffectKind` enum (`Sync` / `Async`) to the
`BuiltIn` trait or a sidecar registry. Annotate every builtin in the
codebase with its effect.

**Concretely**:
- Add `const EFFECT: EffectKind` to `BuiltIn` trait, or a sidecar
  function returning the kind.
- Walk every `BuiltIn` impl across the stdlib packages, classify, and
  annotate.
- Document the rules for non-obvious cases (`hold`, `count`, `sum`,
  etc.) in comments.
- `defpackage!` macro extension to ergonomically pass the effect.

**Open**: Does a builtin ever need different effects per call site
(e.g., based on input types)? My current read is no — a builtin's
async-ness is intrinsic. Verify during the audit.

**Estimated effort**: 2-3 days.

### M6 — Effect inference for user-defined code

**What**: Bidirectional effect inference over `Expr`. Fixed-point
analysis that determines each function's effect and tags every value
expression with sync/async.

**Concretely**:
- Add `effect: Cell<EffectKind>` (or equivalent) to `LambdaDef`.
- Add an `is_async: bool` annotation on each `ExprId` (sidecar map
  on `ctx`, populated during typecheck or a dedicated pass).
- Implement a fixed-point traversal:
  1. Initialize every function effect = sync.
  2. For each function body, walk top-down:
     - Builtin call site → check `EffectKind`; if async, mark.
     - Read of binding → check `unstable_bindings`; if unstable,
       value is async-tagged.
     - Function-typed param call → use resolved callee at call site
       (existing `apply_site_hint` mechanism).
     - User function call → recurse to find that function's effect.
  3. If any change, re-run.
- HOF call sites: store the effect of the resolved callee on the
  Apply expression. Already half-done since `apply_site_hint`
  threads the resolved FnType.

**Estimated effort**: ~1 week.

### M7 — Extend KIR coverage

**What**: Add KIR ops for sync builtins not yet lowered. Priority by
benchmark impact.

**Priority order**:
1. `array::map`, `array::filter`, `array::fold`, `array::flatten`,
   `array::iter` — the array dispatch bottleneck. Absorbing these
   into kernels closes the vectorization gap.
2. `count`, `sum`, `hold`, `take`, `skip`, `mean` — common stateful
   sync ops. State becomes a kernel register.
3. `str::*` over current strings — common in UI code.
4. `select` with arbitrary patterns. May already be supported in
   most cases; audit.
5. Reference ops (`&`, `*`, `*r <- v`) — already partially in.
6. `map::*` and `re::*` — lower priority, smaller bench impact.

**Open**: Some sync ops have complex state (e.g., `array::window`).
KIR registers may not be expressive enough; may need richer state
slots. Address per-op as we hit them.

**Estimated effort**: 2-3 weeks (depending on coverage breadth).

### M8 — Whole-graph fusion analyzer

**What**: Replace the per-lambda fusion entry with a top-level
analyzer that finds maximal sync subgraphs and builds one KIR kernel
per subgraph.

**Concretely**:
- New entry: `fusion::analyze_program(expr) -> Vec<FusedSubgraph>`.
- For each top-level expression, walk the AST identifying async-edge
  positions (async builtin calls, reads of unstable bindings, calls
  to async user functions).
- Each maximal sync region between async edges is a subgraph.
- For each subgraph, run the existing `build_kir_kernel` to produce
  a `KirKernel`.
- Wire each kernel into the runtime as a single `Apply<R, E>`.

**Existing reuse**:
- `build_kir_kernel` for KIR construction.
- Lazy resolution / `apply_site_hint` for call-site type/effect.
- JIT compilation pipeline (cranelift, shared module, async worker).

**Estimated effort**: 3-4 weeks.

### M9 — Runtime integration

**What**: Wire fused kernels into the runtime such that each sync
subgraph is one node, async builtins remain individual nodes, and
unstable bindings mediate cycle transitions via the existing variable
machinery.

**Concretely**:
- The compiler produces a graph of `KirNode` (fused kernels) and
  individual nodes for async-effect builtins, connected via runtime
  variable bindings.
- The runtime's existing event-batch loop drives this unchanged.
- Per-kernel state lifecycle: each kernel owns its registers; when
  the surrounding lambda is dropped, the kernel and its registers
  are dropped.

**Estimated effort**: 1-2 weeks.

### Total: ~2-3 months

Sequencing notes:
- M5 + M6 are independent and unblock M8.
- M7 can run in parallel with M5/M6.
- M8 depends on all of the above.
- M9 is implementation work after M8.

## Open design questions

### References as parameters

Conservative reference handling (every param ref treated as
potentially unstable) loses precision. Consider:

```graphix
let helper = |r: &i64| *r + 1            // is *r async or sync?
helper(&stable_var)                       // sync
helper(&unstable_var)                     // async
```

Without flow-sensitive analysis, we treat both as async, losing the
sync win at the first call. Per-call-site specialization (similar to
the HOF effect peek) could close this; complexity vs gain unclear.
Defer to v2.

### Debugging and error attribution

Errors thrown from deep inside a fused kernel currently lose source
mapping (per `fusion_architecture.md`). Maintain a KIR-op → ExprId
map; emit ExprId in error messages so users can locate the source.
Already partial; finish during M7.

### `unstable_bindings` set completeness

Currently populated by `scan_connect_targets` only at certain
compile entry points (per `fusion_architecture.md:415-419`). Verify
all entry points run the scan before whole-graph fusion runs.

### AOT: drop if it gets in the way

The existing fusion doc keeps AOT pending re-evaluation. Whole-graph
fusion shifts the calculus: if M5–M9 land and the JIT path reaches
or beats AOT on real benches (which the success criteria below
target), AOT's remaining justification is single-binary distribution
without a JIT engine — a deployment concern, not a perf one.

**Position**: AOT is no longer load-bearing for performance.
Maintain it through M5–M7 in case it stays useful, but drop it if
it becomes friction. Specifically, if extending KIR coverage in M7
requires substantial new work in the Rust-source emitter
(`fusion::kir_to_rust_kernel`) and the JIT path is on track to
match AOT performance, deleting the ~1500 lines of AOT-specific
code (`kir_to_rust_kernel`, `rewrite_program`, `emit_package`,
`render_lib_rs`/`cargo_toml`, the standalone-build pipeline) is the
right call. The single-binary distribution case can be revisited
later as a separate effort if it matters.

### Interaction with stability gate (M4g v1)

Today's stability gate (`unstable_bindings` blocks `KirOp::Call`)
implements a coarse version of what whole-graph fusion does
properly. The gate becomes redundant once M8 lands — async-tagged
values flowing into a callee make the call site async, no separate
stability check needed. Remove during M8.

## Success criteria

1. **Vectorization-shaped benches** (`vectorize_dot`,
   `sum_of_squares`) show clear JIT-over-interpreter wins. Today they
   tie because array dispatch dominates; after M8 the inner loop
   should dominate, and JIT/AOT advantages become visible.
2. **Mandelbrot fully-fused** (JIT path) reaches AOT-equivalent
   performance — currently ~14% behind, mostly array dispatch
   overhead.
3. **Top-level reactive programs** (UI event handlers, stream
   processors) show meaningful speedups from collapsing chains of
   simple node ops into single fused kernels.
4. No regression on existing fusion benches (mandelbrot, sum_tail,
   fold_squared).

## Files and key types

- `graphix-compiler/src/effects.rs` (new) — `EffectKind`, effect
  inference pass
- `graphix-compiler/src/fusion.rs` — extended with whole-graph
  analyzer
- `graphix-compiler/src/kernel_ir.rs` — new sync ops
- `graphix-compiler/src/kir_interp.rs`, `kir_jit.rs` — lower new ops
- `graphix-compiler/src/lib.rs` — `BuiltIn` trait extension for
  `EffectKind`
- All `stdlib/graphix-package-*/src/lib.rs` — annotate builtins
- `graphix-compiler/src/typ/` — sidecar effect annotation on
  expressions

## Status (May 2026)

Milestones M5–M9 from the implementation plan are all complete.
Past the original plan, several follow-up milestones landed under
the M8.4 banner — most notably *maximal sync subgraph splitting*,
where an Async sub-expression doesn't split a region, it becomes
a kernel input fed by a separately-compiled feeder Node
(`RegionInputSource::Lifted`). The fusion pipeline today carves
regions whose internal Sync ops absorb into a single KIR kernel,
JIT-compiles via cranelift (region kernels included, not just
lambda kernels), and feeds Async edges through the runtime
variable system.

The dispatch granularity bottleneck the design called out is
substantially closed: per-element array operations inline into
the surrounding kernel via `emit_array_map` / `emit_array_fold`,
and anonymous lambdas register the same way named ones do.
Predictable-perf cliffs that emerged in practice (the user's
explicit reasoning, captured under [[feedback-predictable-fusion]]):
each one fixed in turn — JIT string support, string locals on
both backends, anonymous-lambda fusion, composite-element
producer ops, BindId-keyed unstable bindings, FusedRegion JIT
path, module-kernel async lifting.

Remaining cliffs identified by the post-landings audit (May 2026):
composite-element arrays (`array::map(arr_of_tuples, ...)`),
missing `emit_expr` arms for `StructWith`/`Array`-literal/etc.,
composite variant payloads, composite-pattern select destructuring,
free-var string region inputs. These are real work but not
load-bearing on any current correctness story — each one is a
specific KirType-widening or new emit_expr arm following the
patterns already established. See `CLAUDE.md`'s "Post-landings
audit fixes" entry for the catalog.

JIT-side memory safety has been audited three times. Two
bug-finding audit rounds (Dec 2025 pending-path bugs +
May 2026 string/tailcall leaks + nullable panic + select
soundness) and the rest of the surface — refcount discipline,
two-register Value ABI, owned/borrowed classification,
KernelStrings shared-module lifetime, scope-exit drop discipline
across composites/variants/nullables/strings — has been verified
clean.

## See also

- `design/fusion_architecture.md` — Per-lambda fusion + KIR + JIT
  pipeline (foundation this builds on)
- `design/queue_fn.md` — queue_fn feature, parked
- `CLAUDE.md` "Recent Changes" — chronological catalog of every
  May 2026 fusion/JIT landing with file:line references
