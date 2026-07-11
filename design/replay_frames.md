# reset_replay and evaluation frames

> Built 2026-07-11 (Eric approved the ruling that morning; it settles
> what `fuzz/soak-jul10c`'s class-2 residuals and the old tail-arg
> stale-cache pending ruling were both waiting on). This documents the
> semantics as landed — the mechanism, the classification rule, and
> the two channels that make the interpreter agree with the kernel.
>
> **SUPERSEDED IN PART (v2, same day):** the single-channel
> approximations in this doc — the sticky `ctx.frame_bottom` bit and
> the frame-gated invariant-arg re-delivery — lasted one hour of
> soaking before jul10e found both seams (a discarded-statement error
> bottoming a whole iteration; a re-delivered const call arg firing a
> const-body fold per iteration). Eric's ruling: the interp carries
> per-VALUE fired/taint flags via `TagValue` — see "v2: TagValue as
> the interpreter currency" below. The frame mechanism (private
> variables maps, per-iteration reset_replay, parent-owned seeds) is
> unchanged; only the flag transport is replaced.

## The problem

The node-walk keeps REPLAY MEMORY everywhere: `Cached` operand
wrappers, select's cached scrutinee (and its arm selection), builtin
arg slots (`CachedVals`), a call site's published arg values in
`rt.cached`. All of it exists so REACTIVE evaluation can combine one
fresh input with a quiet other (combineLatest).

Sequential evaluation re-runs the same shared node tree many times
per cycle — a `For` sync-loop iteration, a tail-loop jump. Replay
memory then leaks one iteration's sub-results into the next whenever
the next iteration's producer bottoms: `filter(a, |x| 10/x > 2)` over
zeros KEPT the errored elements (the select re-polled its stale
selection with fresh arm-body inputs); `fold(.., |v0,_| v0 % v0)`
emitted where 0 % 0 must bottom. The kernel never had the disease —
its temps are SSA, recomputed per iteration, with a taint bit for
bottoms.

## The mechanism

Three pieces, all landed:

1. **`reset_replay`** — a REQUIRED method on `Update` and `Apply`
   (no default: the replay-vs-semantic classification is a per-node
   decision the compiler must force). Clears replay caches, never
   semantic state, never wake registrations. It is NOT sleep: sleep
   is arm-rewake RESTART semantics (`once`'s flag and `count`'s tally
   reset on sleep; they survive reset_replay).

2. **Evaluation frames** — the For sync loop and the tail loop's
   re-entered passes run the body against a PRIVATE variables map
   (`mem::swap` of `event.variables`): externals seeded from
   `rt.cached` (delivery-fresh per cached-into-Rt), acc/elem (or the
   jump's rebound formals) inserted fired. A body publish — a shadow
   bind, a callsite arg, a callee formal — dies with the frame, so
   nothing needs to be enumerated for removal (the enumeration
   approach broke twice before). `ctx.frame_depth` marks frame
   execution; per-iteration `reset_replay` runs between frames.

3. **`ctx.frame_bottom`** — the interpreter's taint bit. The single
   `Option<Value>` channel conflates GENUINE bottom (a swallowed
   error: unchecked arith, handler-less `?`, `$`) with legitimate
   quiet (a stale input bridged by a value cache). Error sites mark
   the flag (a no-op outside frames); the frame driver treats the
   iteration as bottomed even when a downstream cache produced a
   value. Sticky, like the kernel's taint.

## The two channels (why some caches PERSIST)

The kernel separates VALUE from FIRING: a value can be present in a
slot while its disc says STALE. The interpreter's corpus-pinned
equivalents:

- **Builtin arg slots (`CachedVals`) persist across frames.** They
  are the value channel — the kernel's own per-arg last-value slots
  do exactly this. A const-result feeder (`f(v)` with a constant
  body) fires once ever; its slot value is what lets
  `push(res, f(v))` keep emitting per fired `res`
  (hof_const_body_prev_len). The fold dual (`|a,b| 100`) stays quiet
  because its value rides the DIRECT chain, which is the firing
  channel (hof-lift-firing pin). Value-level staleness from an
  in-frame ERROR is caught by `frame_bottom`, not by clearing slots.

- **Closed (refs-free) `Cached` subtrees keep their cache** — a
  constant expression's value is identical in every frame and cannot
  re-fire without an init view; the cache IS the kernel's immediate.
  Same rule for a call site's closed args: their `rt.cached` entries
  survive reset, and inside a frame the call site re-delivers them
  into the private map (the DynCall side-channel twin; gated on
  `frame_depth` — in reactive land that re-delivery re-ran effectful
  callees every cycle).

- **Select ties selection to the scrutinee cache**: a frame-varying
  scrutinee clears both (the filter bug); a closed scrutinee keeps
  both (a `select 1 { 1 => x, _ => y }` body keeps emitting per
  fired x — the coarse-firing pins).

## Init priming

No per-iteration init view (it made const-callback folds fire —
constants must be value-present, not firing). Instead the loop's
FIRST ITERATING run is init-forced (`For::primed`) so constants and
defaults materialize into their frame-surviving caches — the loop
mirror of a call site's first-dispatch priming. The tail loop keeps
its pre-existing per-jump init (a tail call is a CALL; its emission
is the return value, not a slots-word).

## What this settled

- The jul10c replay-leak class: 17 of 20 artifacts now agree
  (d06/d36/g89 are other classes — async queue/group machinery and
  pacing).
- The old tail-arg stale-cache pending ruling (fuzz/pending-ruling/):
  the tail frame + per-jump reset is the fix.
- The double-emission class (dynamic-module programs on the async
  per-index path) is NOT addressed — `update_async` is untouched.

## Costs and invariants

- Iteration cost: one `reset_replay` walk + frame map rebuild per
  iteration. The interp loop was already O(body) per iteration.
- `sleep` and `reset_replay` are independent contracts; neither
  calls the other. Every new node must implement both — the compiler
  enforces it.
- reset_replay must NEVER touch wake registrations (`ref_var`/
  `unref_var`) — that is sleep's business.

## v2: TagValue as the interpreter currency (2026-07-11, Eric's call)

The kernel's disc word carries two reserved bits with documented
semantics (`fusion/emit.rs`): **STALE** (bit 61 — "did not fire this
cycle; the payload is a cached prior value") propagated by AND-reduce
(a result fires iff ANY consumed operand fired) and forced fresh at
exactly two consumers (the kernel output and a `set_var` write); and
**TAINT** (bit 62 — "MAY be a bottom") propagated by OR and forced at
outputs and destructuring consumers, with the invariant TAINT ⟹
STALE. The v1 interp approximated both channels with frame-scoped
scalars — a sticky `frame_bottom` bool (taint without a value to ride
on) and fired-map re-delivery (staleness with no way to say so) — and
jul10e broke each within the hour:

- `fuzz/div_000000`: `{let b = 1/0; fold([5,7],b,…); fold([5,7],0,…)}`
  as a fold body — the error feeds only a DISCARDED statement; the
  kernel's per-value taint never reaches the taken output path (emits
  7s), the per-frame bit bottoms everything (emits nothing).
- `reactive/div_000000/000001` (isolated: fold body `f(271,0)*0 + k`
  with const args): re-delivered invariant call args are
  indistinguishable from fired ones in `event.variables`, so the call
  executes and "fires" per iteration — interp 6 events, kernel 1. The
  interp was also self-inconsistent: a bare-const body stayed quiet
  while a const-CALL body fired.

The fix is to stop translating: the interp adopts `TagValue` — the
16-byte tag-carrying Value already used at the JIT↔runtime boundary
(`fusion/emit_helpers.rs`), whose tag byte IS the kernel's disc tag
region — as its value currency:

- `Update::update` returns `Option<TagValue>`: `None` = no production
  (silence, as today); `Some(fired v)` = today's `Some`;
  `Some(stale v)` = value-channel refresh (parent caches it, nothing
  fires); `Some(tainted v)` = a bottom placeholder flowing (tainted ⟹
  stale, constructor-enforced).
- `Event.variables: IntMap<BindId, TagValue>` — a frame re-delivery
  or seed inserts STALE entries; ordinary deliveries are fired. The
  kernel marshal reads param STALE bits off the entry instead of
  re-deriving them from map presence.
- `Cached.cached: Option<TagValue>` — a cached taint persists with the
  value (the kernel's slot bit) and ORs into any result that consumes
  it; `Cached::update` reports fired-ness, not mere production, so
  stale refreshes stop counting as fire triggers.
- Propagation in ops mirrors CLIF: result = tainted if any CONSUMED
  operand tainted (checked before attempting the op — no synthetic
  error logs off placeholders), else fired if any consumed operand
  fired, else stale.
- `Apply::update` (the ~69 stdlib builtins) STAYS `Option<Value>`:
  a builtin cannot know fired-ness — its `CachedArgs` wrapper does
  (a stale arg production updates slots without triggering eval; a
  tainted slot short-circuits eval to a tainted placeholder, the
  conservative DynCall rule). `GXLambda`/`BuiltInLambda`/fused-Kernel
  surface their body's tag through an `Apply` accessor with a
  fired default.
- Force points in the interp: loop drivers (a tainted body result =
  sticky bottom, the sequential break; a stale one contributes no
  firing — body-driven firing becomes consumption-accurate), `connect`
  /`?` variable writes (skip non-fired RHS — `set_var_typed`'s twin),
  and the runtime delivery boundary. Select forces taint at the
  scrutinee (the destructuring-consumer rule).
- Error sites (unchecked arith, handler-less `?`, `$`) inside frames
  (`frame_depth > 0`) produce `Some(tainted Null)` instead of `None`
  and skip their log line; outside frames they keep today's None+log
  (reactive semantics unchanged — stale and taint productions cannot
  originate outside frames).
- DELETED: `ctx.frame_bottom`/`mark_frame_bottom`/`take_frame_bottom`
  and the frame-gated invariant-arg re-delivery in `CallSite::update`.
- `rt.cached` stays clean `Value`: cross-cycle stores hold settled
  values only (the write gates are fired-only), staleness is a
  delivery-relative property, and cross-cycle taint must not persist.

`TagValue` moves to a central module (re-exported where the JIT
helpers import it), gaining the STALE tag constant beside `TAINT_TAG`,
`fired()`/`tainted()` predicates, and the three constructors that make
the invariant unrepresentable-to-violate. Its masking discipline
(`value()`/`with_value` are the only ways back to `Value`) is what
keeps a tagged disc from ever being read as a `Value` discriminant —
the exact UB class it was built to stop at the JIT boundary.

### Implementation truths (as landed)

- **Ops FORWARD stale productions** (recompute and emit a stale
  result), they don't absorb them at the first `Cached`: absorption
  breaks in-frame value chains (an arg expression over stale seeds
  must still marshal a value to its callee, as the kernel recomputes
  everything per invocation). Outside frames stale productions cannot
  originate, so reactive behavior is bit-identical.
- **Fired vs stale delivery is "fresh tree vs re-run frame"**: a NEW
  node tree's first dispatch (a lazy bind's priming, a select arm
  wake, a fresh async loop instance, the runtime's init view) delivers
  FIRED — everything is new to it. Only RE-RUN frames of the SAME tree
  (For iterations, tail-loop jumps) seed stale — and even there, an
  external that genuinely fired this cycle keeps its real fired entry
  for the whole pass (the kernel's per-invocation param disc).
- **Loop drivers**: body-result tag drives firing (`fired_any`);
  tainted body result = sticky bottom (never-until-complete); a
  complete quiet pass emits the acc STALE (value channel); empty
  source emits the init FIRED (the top gate already proved an input
  fired). The first ITERATING run's forced init view counts as fired.
- **`CachedArgs` (Sync builtins)**: per-slot tags + a `last_result`
  slot. Fired trigger → eval; stale refresh → re-surface
  `last_result` tagged stale WITHOUT re-running eval (safe for
  stateful-sync builtins — `count` doesn't tick); any tainted slot →
  skip eval, produce the tainted placeholder (conservative DynCall).
- **`Apply::out_tag`**: `Apply::update` stays `Option<Value>` (a
  builtin can't know fired-ness); the tag rides a side accessor read
  by `CallSite` immediately after `update`. Overridden by `GXLambda`
  (body tag), `BuiltInLambda` (delegates — the reset_replay trap
  again), `CachedArgs`.
- **Force points in code**: `Connect`/`ConnectDeref`/`ByRef`/`Sample`
  gate `set_var` on fired; the runtime delivery boundary
  (graphix-rt `do_cycle`) drops non-fired top productions; `Select`
  and `Bind`/`GXLambda` formal marshal poison-and-skip destructuring
  on taint; loop drivers force taint to bottom.
- **Kernel seams**: `Kernel::update` reads feeder production tags
  (tainted → TAINT placeholder slot; stale → slot refresh without
  running). A RUN only surfaces FIRED outputs (the JIT return gate
  forces stale/taint before the decode) — but the kernel carries a
  RESULT slot (`last_result`, the `CachedArgs::last_result` twin): a
  poll that delivered only stale productions re-surfaces the cached
  result tagged STALE. Sound because regions are pure by construction
  (effects de-fuse): stale inputs = unchanged values = the cached
  result IS what a re-run would compute. This is what lets a
  partially-fused subtree inside a node-walked loop advance the acc
  chain on the value channel (jit-mode frame-runs of unfusable loops
  around fused kernels — p7-class shapes).
- **Tail-loop result tag**: the tail loop's re-entered passes run
  under a forced init view (body constants must re-fire per jump),
  which poisons the body's own tag FIRED. When the loop actually
  re-entered, the result tag is DERIVED FROM THE ENTRY instead —
  fired iff a formal delivery triggered, the dispatch ran under a
  real init view, or a captured input triggered — the kernel's
  rebind-and-jump result-disc derivation. A first pass that never
  jumped keeps its organic tag.
- Known refinement flagged during the sweep: select's
  `bind_event` delivers scrutinee FIELDS fired even when the
  scrutinee production was stale — threading the scrutinee tag
  through would be stricter; guards re-run selection on any
  production (value-channel guard changes still re-dispatch).
