# Transient recursion — O(depth) memory for non-tail recursion in the node-walk

## The problem (soak jul08d follow-up, 2026-07-08)

A recursive back-edge cannot be statically pre-bound at compile time (the
#203 cut leaves it `DynamicUnbound` — resolving it eagerly would recurse
compilation forever). So at runtime, the first dispatch of each recursive
call site runs `CallSite::setup_bind`: a fresh compile of the whole lambda
body, a `typecheck0` pass, and `analyze_bound_callee` over the new subtree.
The fresh body contains its own unbound recursive call sites, which bind
when *they* first dispatch.

Because a body with two recursive call sites (fib) binds a distinct callee
instance per site, the instance tree unfolds to the shape of the **full
dynamic call tree** — one compiled body instance per dynamic call, retained
forever as graph structure. Measured: fib(28) = 1.03M instances ≈ 9.3KB
each ≈ **9.6GB peak RSS**; fib(33) extrapolates to ~80GB — the canonical
evaluator could not complete recursion trees the JIT finishes in
milliseconds, and it was memory that killed it, not time. (This also
explains node-walk recursion's speed: it runs the compiler + typechecker
once per dynamic call.)

The retention is not gratuitous in general: a lambda body can hold async
state (timers, subscriptions), and the persistent unfold is exactly the
reactive model — those instances must live. It is pure waste only when the
instance is a **pure activation record**.

## The mechanism

Three pieces, all runtime-side (`node/callsite.rs`, `node/lambda.rs`):

1. **Recursion detection** — `ExecCtx::active_lambdas`, a multiset of
   `LambdaId`s whose `GXLambda::update` is currently on the Rust call
   stack (pushed after the depth guard, popped on exit). A `bind()` whose
   def is already active is a recursive unfold — the enclosing body is an
   activation of the same lambda. This catches mutual recursion too, and
   costs two map ops per lambda dispatch.

2. **The gate** — `transient_body_ok`: the instance may be deleted on
   return only if nothing observable lives in it. Requirements, checked
   transitively through every already-bound callee body (the bind-time
   #203 cascade has already resolved statically-known inner sites):

   - `LambdaDef::intrinsic_effect == Sync` (checked first, before the
     walk). This excludes async bodies and — because `analysis.rs`
     classifies them Async — `~`, `any`, `try/catch`, and fn-typed
     parameter calls (dynamic callees).
   - No builtin call sites. A builtin `Apply` may hold per-instance state
     (`count`, `sum`, `min`, `max`, `all`, `skip`, …) that accumulates
     across calls under the retained unfold; there is no
     stateless-vs-stateful declaration on `BuiltIn` to consult, so ALL
     builtins refuse. (A `STATELESS` marker const could widen the gate
     later — `str::len`-style pure builtins are observably safe.)
   - No `connect` (plain or deref): the write target and its next-cycle
     delivery live inside the instance.
   - No `&x`: a reference to an instance-local binding can escape the
     instance's lifetime.
   - An UNBOUND inner call site is fine only when it provably targets a
     stable, known user lambda (`bind_to_lambda`/`cached`, not in
     `unstable_bindings`) — that is exactly a recursive back-edge, whose
     target is an ancestor of this walk and whose own unfolds gate
     themselves at their own bind. Anything dynamic refuses.

3. **Park / re-bind** — a transient binding
   (`Callee::DynamicBound { transient: true, .. }`) never survives the
   `CallSite::update` that created it: after the dispatch returns, the
   instance's external refs (its captures, minus the fnode's own target)
   are collected, the site takes over their runtime `ref_var`
   registrations (so capture events keep flowing to this top while the
   instance is gone — `Ref::delete` unregisters, and a capture only
   reachable through a lazily-bound inner lambda has no other registrar),
   the instance is deleted, and the site becomes
   `Callee::TransientParked { def, ext_refs }`. The def value is stashed
   because the fnode Ref delivers the lambda only once.

   A parked site re-binds on a GENUINE call: an arg fired this cycle, an
   init-forced view (an enclosing fresh bind's priming dispatch, an arm
   wake), or an `ext_refs` capture present in `event.variables` — the
   retained instance this replaces was reactively live to its captures
   (the JIT twin: captures are kernel inputs). Quiet cycles stay parked,
   matching the retained twin's produce-nothing passive re-poll. The
   re-bind is the ordinary `bind()` path — same compile, same cached-arg
   priming — so a re-bound call is indistinguishable from a first call.

Peak memory becomes O(active depth), which the 256 call-depth guard caps
at a few MB. The unwind is natural stack discipline: each instance's
transient children have already parked by the time it parks.

## Costs and non-costs

- One-shot recursion: **unchanged** — every unfolded node already paid
  compile-per-call; transient just frees it on return. fib(28) measured
  9.6GB → ~130MB peak, same wall time.
- Re-fired recursion (a reactive program re-firing a deep non-tail
  recursion every cycle): re-compiles per call where the retained tree
  re-walked warm nodes. This workload is already ~10^5x off the JIT;
  correctness of the fallback (not dying) wins. If it ever matters, the
  follow-up is a per-`LambdaDef` instance pool: park into a freelist,
  re-bind by rebinding formals + dispatching with `event.init = true` —
  precisely the tail-loop re-entry contract generalized to non-tail.
- Runtime lazy binds never fuse (`InitFn` builds a plain `GXLambda`;
  fusion runs at compile time only), so deleting and recompiling a
  transient instance can never discard spliced kernels.

## What stays retained (deliberately)

- The **entry instance** of any recursion (bound when the def was not yet
  active) — one per call site, today's behavior.
- Non-Sync recursive bodies (async state = legitimate reactive
  structure).
- Bodies with builtins/connect/ByRef — semantics-preserving refusal. The
  retained-unfold behavior for stateful-sync builtins in recursion
  (per-call-tree-path `count` state accumulating across fires) is pinned
  by `rec_transient_stateful_retained` in
  `stdlib/graphix-tests/src/lang/functions.rs`; whether that is the
  *intended* language semantics (vs fresh-per-call) is an open question —
  note the JIT currently de-fuses those shapes, so the node-walk is the
  only live semantics for them.

## The CallSite arg-cache leak (found by the same investigation)

`CallSite::delete` deleted its arg nodes but never removed
`ctx.cached[arg.id]` — one leaked `Value` per arg per deleted site (and
`setup_bind`'s default-arg cleanup likewise leaked the old default's
entry per re-bind). Invisible while instances were never deleted; once
transient parking deleted instances per call it became one cached entry
per dynamic call (measured: `ctx.cached` at 132k entries after 131k
binds; flat ~1k after the fix). Both sites now remove the cached entry.
