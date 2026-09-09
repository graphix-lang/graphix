# Strict fusion: pure computation only

Status: built 2026-09-01
Pins: `stdlib/graphix-tests/src/lang/errors.rs` (`catch_array_index_fused`, `checked_div0`), `stdlib/graphix-tests/src/lib_tests/native.rs`, `graphix-fuzz/findings/default-arg-birth-sep2026/`, `graphix-fuzz/findings/dyncall-tagblind-print-aug2026/`
Supersedes: impure_hof_fusion, replay_frames, pure_dataflow_plan

## The rule

Fusion admits pure computation only. A subtree fuses iff every node in
it emits CLIF and every builtin it reaches carries a fast fn; a kernel
is a pure function of its inputs to (result, deliveries), with exactly
one kind of cross-invocation memory: the firing boundary. Everything
else — a builtin without a fast fn (stateful, effectful, seam-gated),
`connect`, async producers — refuses emission and node-walks,
transitively through callees. The user model is one sentence: *pure
code over fastcall builtins fuses*; `#[native]` asserts it at a source
location and turns every cliff into a compile error.

Eric's ruling: "Complexity needs to pay rent, and this whack of
complexity can't afford to live in our compiler." The machinery that
went was deleted outright rather than kept behind a hatch — it is all
in git.

## Why

Every soak class for two months lived at one boundary: fused code with
interior cross-invocation state. Site identity (per-site inner Applies,
site words, per-slot chains), the stale/taint mask delivery protocol,
the restart and stateful-reach gates, selection-memory words, the
quiet-frame trio, wake hints and the birth view all existed to make a
stateful kernel interior mimic the interp. The default-arg-birth
finding showed the mimicry could reproduce the interp's bugs so
faithfully that BOTH engines broke identically and the differential
oracle went blind — the machinery had stopped being checkable.

Measured before the flip (release build, whole findings corpus): all
464 pins agreed under strict; 94% of kernels survived (821/877); 401 of
464 programs fused identically, and the losers were overwhelmingly the
stateful-fusion bug witnesses the corpus over-represents by
construction. The bench corpus was flat or faster (`composite_seams`
~37 vs ~42ms) except `tail_sum` (+6.5%, call periphery — the loop
kernel itself was identical), and it needed no restructuring: hot code
was already written in the strict idiom — pure folds in fused position,
`array::window`/`count` at `<-` and control-flow seams fusion never
crossed. That is the strongest form of the "the user can restructure"
argument.

## The admission predicate

A builtin's one classification is `const EFFECT: Effect` with
`Effect::{Async, Sync, Stateless(Option<FastCall>)}` (`effects.rs`).
The five knobs it replaced (`EFFECT`/`STATELESS`/`SLEEP_RESTARTS`/
`FASTCALL`/`FASTCALL_TYPED`) had to be set together and a fast fn was
easy to forget after declaring Sync + stateless; now the invalid
combinations are unrepresentable and declaring a builtin stateless puts
the fast-fn question in the author's face. The `Option` keeps the
tail-loop collapse fact for the stateless builtins that can never be
fast fns — effects (`print`/`log`/`exit`/`now`) and the
partial-delivery producers (opt's short-circuits, `divide`,
`filter_err`), whose result depends on which args arrived while a fast
fn sees every arg present. Marking those `Sync` would have made a tail
loop reaching `log` retain an activation per iteration.

`FastCall::Plain(fn(&[Value]) -> Option<Value>)` is called through
`graphix_fastcall`: the site stores the args' (disc, payload) pairs in
a stack slot, the trampoline views it as `&[Value]`, and the args'
discs decide the tag — a tainted arg bottoms the call without invoking
the fn, all-stale args make the result stale, `None` is this cycle's
bottom. `FastCall::Typed(fn(&Env, &Type, &[Value]) -> Option<Value>)`
is for a result directed by its return type (`str::parse`'s `'b`): the
site bakes its resolved `CallSite::typ()` beside the pointer and
`graphix_typedcall` runs it under the kernel's env loan (`KERNEL_ENV`).
The non-inline `cast<T>(x)` is the same dispatch with `cast_typed`, the
interp's exact `cast_value`. The interp runs the same fn through
`fast_eval`/`fast_eval_typed` — one implementation per builtin.

A labeled default the call left unwritten is marshaled from the
CallSite's own compiled default node (`MarshalArg::Default` →
`CallSite::arg_named`), so `sort(a)`, `escape(s)` and
`hbs::render(t, d)` fuse in their common spelling; the trampoline reads
the buffer AS the args, so an unwritten label would otherwise be a
hole. A configuration a fast fn compiles from its args — a regex, an
escape table, a template registry — lives in a bounded thread-local
`FastMemo` keyed by the configuring values: a cache, never state (a
miss rebuilds from the key; one memo per thread serves every site; it
clears whole when full). The per-instance memos this replaced were the
reason those builtins could not be declared stateless, and they carried
a partial production (a bad pattern erred before the subject arrived)
that was an accident of the code shape.

The stdlib is converted maximally (~110 fast fns across core, array,
map, list, str, re, hbs, sys::time, sys paths, the json/toml/pack
writers). What remains is out by rule, not by gap: the partial-delivery
producers, the stateful family (`count`/`sum`/`min`/`max`/`mean`/
`product`/`uniq`/`once`/`take`/`skip`/`hold`/`window`/`group`/`and`/
`or`), the lambda-taking HOFs, effects, and the json/toml/pack readers
(async by design). `bench_mandelbrot_iterate` stays a builtin call on
purpose — it is the bench's un-fused comparison point.

## The `?` delivery queue

A handler-ful `?` fuses. Eric: "`?` is central to array access … the
fact that it fuses with no catch and doesn't with catch is even worse
for predictable performance." The delivery is an effect, but a pure
function of the kernel's inputs, so it is emitted statelessly: a
failing handler-ful `?` calls `graphix_qop_raise(site, disc, payload)`
— `site` an interned `QopSite` (handler, own top, spec) — which clones
the error onto the invocation's `QOP_RAISES` queue (a scoped
thread-local like `KERNEL_ABORT`/`KERNEL_ENV`, saved and restored
around nested invocations reached through the value hooks).
`Kernel::update` drains the queue after the wrapper returns, in push
(= execution) order, through `node::error::deliver_error`, the handler
path factored out of `Qop::update` so both engines run one function:
same-top Vacant-insert / `set_var` on an occupied entry, cross-top
`set_var`, in-frame `frame_outbox` parking. Delivery keys on a FRESH
error (not tainted, not stale), exactly `Qop::update`'s fired-only
rule. The value side is untouched: the failing `?` is the tainted
placeholder that continues.

## What a kernel keeps

Only what decides FIRING: per-param STALE and TAINT discs, prev-length
words (exact HOF resize detection), first-call words (a callee's init
view on its first call ever), and the per-call-site blocks /
per-slot anchor chains / per-activation block trees that give those
words per-slot and per-activation multiplicity
(`kernel_instance_state.md`), plus the shrink reclaim of unreached
activations. No replay caches (`Kernel::reset_replay` is a no-op —
every word a kernel keeps is semantic), no selection memory, no inner
`Apply`s or `Node`s (`Kernel` is not generic). The runtime loans an
invocation exactly four things through scoped thread-locals:
`KERNEL_ABORT` (the interrupt / stack-budget / bottom-abort channel),
`KERNEL_ENV`, `QOP_RAISES` and the core-trait value hooks. Wire slot 0
bit 2 is the kernel's own `slept` bit, so a wake delivers standing args
STALE and a stateless re-eval's STALE result (`wake_catchup.md`).

## What was deleted, and why

- **The DynCall dispatcher** (`graphix_dyncall`, `DispatcherState`,
  `DynCallSlot`, the site-id mint, the pooled arg marshal, the region-
  wide slot table, `CastApply`/`QopDeliverApply`): an inner `Apply`
  per site inside a kernel was the stateful interior in its entirety;
  the one admission predicate replaces the dispatcher and every gate
  that guarded it.
- **Site identity and wake hints** (`emit_dyncall_site_word`,
  `claim_slot_site_words`, the key-0 bucket, `WAKE_HINT`/
  `DISPATCH_WAKE`): existed only to give inner Applies per-site
  multiplicity and sleep routing.
- **The interior gates** (`sleep_restarts`/`stateless` site info,
  `arm_depth`, `saw_*_reach`, `has_{restart,stateful}_reach`, the P7
  deferred checks): classifiers over a kind of kernel that no longer
  exists.
- **Selection memory** (`SelWord` claims, `record`/`woke`/`eff_init`,
  `init_override`/`wake_override`): a fused select claims nothing; its
  firing is organic (`organic_firing.md`).
- **Arm-lift** (`collect_lifted_connect_targets`, `arm_region`,
  `KernelSig::lifted`, the minted per-instance BindIds,
  `emit_connect_node`, `graphix_set_var`): `Connect::emit_clif` refuses
  — connect is an effect.
- **Replay words** (`replay_state_words`, the `SiteLayout`/`SelfBlock`
  replay lists, honor headers, `reset_self_block_tree`,
  `drop_replay_values`): with the DynCall result caches and the rides
  gone they had no caller.
- **The permissive hatch** (`GRAPHIX_PERMISSIVE_FUSE`): two fusion
  regimes would have been two semantics to soak; the deletion happened
  before the soak on Eric's call, because git holds the old tree.
- **The mid-kernel `?` write through the dispatcher**
  (`emit_qop_deliver`, `FnSource::QopDeliver`): replaced by the delivery
  queue above.

What survives regardless is the interp semantics built alongside
(wake catch-up, the birth rule, the sys::net level effects) — language
semantics, engine-independent.

## Alternatives ruled out

- **Keeping stateful kernels behind a stricter gate** (fuse stateful
  interiors only when no arm sleeps, or only outside frames): every
  such gate was tried in some form and each produced its own soak
  class; the gates were the complexity, not the cure.
- **Removing the JIT entirely** (the pure-dataflow arc): built and
  gated on a branch, but the node-walk alone cannot make hot pure
  loops fast, and predictable performance is a core value. Fusion
  stays, narrowed to the regime where its rent is paid; fusion is an
  embedder switch (UIs run with it off).
- **Admitting stateful builtins as fast fns with per-site memory:** a
  fast fn that remembers is a DynCall slot by another name — the same
  multiplicity questions return.
