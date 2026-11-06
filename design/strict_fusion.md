# Strict fusion: pure computation only

Status: built 2026-09-01
Pins: `stdlib/graphix-tests/src/lang/errors.rs` (`catch_array_index_fused`, `checked_div0`), `stdlib/graphix-tests/src/lib_tests/native.rs`, `graphix-fuzz/findings/default-arg-birth-sep2026/`, `graphix-fuzz/findings/dyncall-tagblind-print-aug2026/`
Supersedes: impure_hof_fusion, replay_frames, pure_dataflow_plan

## The rule

Fusion admits pure computation only. A subtree fuses iff every node in
it emits CLIF and every builtin it reaches carries a fast fn; a kernel
is a pure function of its inputs to (result, deliveries), with exactly
one kind of cross-invocation memory: the firing boundary. Everything
else — a builtin without a fast fn (stateful, seam-gated), `connect`,
async producers — refuses emission and node-walks, transitively through
callees. The user model is one sentence: *pure code over fastcall
builtins fuses*; `#[native]` asserts it at a source location and turns
every cliff into a compile error.

The one effect a kernel holds is an `Effect::Io` call (`dbg`, `log`,
`print`, `println`), so debugging a kernel does not change it. Its
`IoCall` is a fast fn for the call's value and the index of its trigger
argument. The call site reads every other argument as stale, so the
value fires when the trigger does, and when the trigger fires untainted
`graphix_iocall` queues the site and the arguments beside the run's `?`
raises (`emit_helpers::Deferred`); after the run the wrapper performs
each through the builtin's `BuiltIn::perform`, in execution order, a
forked loop's chunks in slot order. A kernel derives its selection
fresh each run, so under a select arm a call that performs (an `Io`
builtin's, or a function's that reaches one) fuses only when each
trigger fires at every entry to the arm: an `Io` builtin's trigger fires
with the scrutinee or one of the arm's binds (a read of one, or a part
that reads one: a nested select through its scrutinee only, never a
function's result), a performing function is called with arguments that
each do and performs only on its own formals, recursively
(`analysis::io_follows`), and no arm has a guard
(`lowering::arm_io_blocker`). Elsewhere a fire the
arm missed asleep, which the node-walk re-delivers at entry, or a
constant, which fires at the arm's first entry only, would print on one
engine only. A tail recursion that prints by its formals is one native loop.

Swallowed-failure diagnostics (`$` and `?` without a covering handler)
follow a fired error operand inside a kernel. They carry neither a
report-count nor an at-least-once guarantee across the two engines, and
remain eligible for fusion. Prologue guards dominate the selection chain
and may report even when an earlier arm claims the value; their bind masks
still prevent unreached binds from becoming values. A constant error in
an arm first entered after the kernel's birth may never fire there and
never report. Native tail depths may share one report.

`stdlib/graphix-tests/tests/fusion_diagnostics.rs` checks values, report
counts, and retained native calls in all four modes:

| Swallowed failure | Node-walk reports | Fused reports |
| --- | ---: | ---: |
| Unreached guard over four firing inputs | 0 | 4 |
| Constant error in a late arm | 1 | 0 |
| Constant error over six tail depths | 6 | 1 |

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
`Effect::{Async, Sync, Stateless(Option<FastCall>), Io(Option<IoCall>)}`
(`effects.rs`). The five knobs it replaced (`EFFECT`/`STATELESS`/
`SLEEP_RESTARTS`/`FASTCALL`/`FASTCALL_TYPED`) had to be set together and
a fast fn was easy to forget after declaring Sync + stateless; now the
invalid combinations are unrepresentable and declaring a builtin
stateless puts the fast-fn question in the author's face. The `Option`
keeps the stateless fact (the JIT's native tail loop, a sleep-free arm)
for a pure builtin without a direct entry: a raw `Apply` that answers
before all its args arrive, or a data operation whose cost warrants
keeping its last result when its own arguments stand stale. The latter
are `str::parse`, `hbs::render`, the json/toml/pack writers,
`array::sort`, `list::sort`, `array::dedup` and every `re::*` builtin.
They use `CachedArgs` with `Effect::Stateless(None)`.
An effect is never `Stateless`: an arm holding only stateless calls
skips sleep and is slept at its re-entry, where a `Stateless` call
recomputes its value by running `eval` again, which for an effect would
act again on a fire no one made. Effects are `Effect::Io`: an arm
holding one sleeps, and its wake recomputes the value through
`EvalCached::quiet`, which performs nothing (`exit` has no `IoCall`).
`now`, which keeps its last reading, is `Sync`.

`FastCall::Plain(fn(&[Value]) -> Option<Value>)` is called through
`graphix_fastcall`: the site stores the args' (disc, payload) pairs in
a stack slot, the trampoline views it as `&[Value]`, and the args'
discs decide the tag — a tainted arg bottoms the call without invoking
the fn, all-stale args make the result stale, `None` is this cycle's
bottom. `FastCall::Typed(fn(&Env, &FnType, &[Value]) -> Option<Value>)`
is directed by the resolved function type (`str::sprintf`, for example): the
site bakes its resolved `CallSite::typ()` beside the pointer and
`graphix_typedcall` runs it under the kernel's env loan (`KERNEL_ENV`).
The non-inline `cast<T>(x)` is the same dispatch with `cast_typed`, the
interp's exact `cast_from` (the site interns the `(target, source)`
pair: the source's static type tells an array from a list). The interp runs the same fn through
`fast_eval`/`fast_eval_typed` — one implementation per builtin.

A labeled default the call left unwritten is marshaled from the
CallSite's own compiled default node (`MarshalArg::Default` →
`CallSite::arg_named`), so `str::escape(s)` fuses with its default
configuration; the trampoline reads the buffer AS the args, so an unwritten label would otherwise be a
hole. A configuration a fast fn compiles from its args — a regex, an
escape table, a template registry — lives in a bounded thread-local
`FastMemo` keyed by the configuring values: a cache, never state (a
miss rebuilds from the key; one memo per thread serves every site; it
clears whole when full). The per-instance memos this replaced were the
reason those builtins could not be declared stateless, and they carried
a partial production (a bad pattern erred before the subject arrived)
that was an accident of the code shape.

Fast calls cover core, array, map, list, str, sys::time and sys paths,
except the cached data operations above. Other exclusions are opt's
short-circuits and `filter_err`, which hold their last answer, the stateful family (`count`/`uniq`/`once`/`take`/`skip`/
`hold`/`window`/`group`), the lambda-taking HOFs, effects, and the
json/toml/pack readers (async by design). The pure aggregates (`sum`/
`min`/`max`/`mean`/`product`/`and`/`or`) and the pure option builtins
have fast calls.

## The `?` delivery queue

A handler-ful `?` fuses. Eric: "`?` is central to array access … the
fact that it fuses with no catch and doesn't with catch is even worse
for predictable performance." The delivery is an effect, but a pure
function of the kernel's inputs, so it is emitted statelessly: a
failing handler-ful `?` calls `graphix_qop_raise(site, disc, payload)`
— `site` an interned `QopSite` (handler, own top, spec) — which clones
the error onto the invocation's deferred queue (`emit_helpers::Deferred`,
which holds the queued Io calls too; a scoped thread-local like
`KERNEL_ABORT`/`KERNEL_ENV`, saved and restored around nested
invocations reached through the value hooks).
`FusedKernel::update` drains the queue after the wrapper returns, in push
(= execution) order, through `node::error::deliver_error`, the handler
path factored out of `Qop::update` so both engines run one function:
same-top Vacant-insert / `set_var` on an occupied entry, cross-top
`set_var`. Delivery keys on a FRESH
error (not tainted, not stale), exactly `Qop::update`'s fired-only
rule. The value side is untouched: the failing `?` is the tainted
placeholder that continues.

The delivery's TRIGGER must be computable by a memoryless kernel. A
handler-ful `?` under a select arm raises on an edge the arm may owe
to the select: a constant fires at the arm's first entry only (a wake
is a resume), so a raise whose error derives from one (`error(`E)?`,
`cast<i64>("x")?`) fires when the arm is first ENTERED, and an input
fire no selected arm saw is
re-delivered FIRED to the arm that reads it at its wake
(design/wake_catchup.md). A kernel derives its selection fresh every
run and keeps neither memory, so `lowering::arm_raise_blocker` refuses
every region with a handler-ful `?` reachable under a select arm,
whatever its error derives from, callee bodies and nested selects
included. A bottom-typed arm that can raise runs its
body for the delivery instead of the bare `never()` placeholder, and a
`?` or `$` over an inner that is always what it strips has its own
emission (`emit_qop_always_bad`: raise when fresh, yield a bottom). The
alternative that would let these fuse — a per-select last-arm word,
the same class as a first-call word — is selection memory, not taken.

## What a kernel keeps

Only what decides FIRING: per-param STALE and TAINT discs, prev-length
words (exact HOF resize detection), first-call words (a callee's init
view on its first call ever), and the per-call-site blocks /
per-slot anchor chains / per-activation block trees that give those
words per-slot and per-activation multiplicity
(`kernel_instance_state.md`), plus the shrink reclaim of unreached
activations. No replay caches (every word a kernel keeps is semantic),
no selection memory, no inner
`Apply`s or `Node`s beyond its input feeders. The runtime loans an
invocation exactly these through scoped thread-locals:
`KERNEL_ABORT` (the interrupt / stack-budget / bottom-abort channel),
`KERNEL_PANIC`, `KERNEL_ENV`, the deferred queue, the self-block counters
(`SELF_BLOCK_GEN` and its kin), the core-trait value hooks and the fork
mode (`par_loop::PAR_LOAN`, the invoking context's `ParLoan`). Wire slot 0
bit 1 is the wake view (the kernel's own `slept` bit or the arm's), so a wake delivers standing args
STALE and a stateless re-eval's STALE result (`wake_catchup.md`).

## What a kernel does not hold

A kernel holds no inner `Apply`, no selection memory and no replay
words: each was a stateful interior, and strict fusion admits none. A
fused select claims nothing; its firing is organic
(`organic_firing.md`). A connect is an effect, so `Connect::emit_clif`
refuses it. A mid-kernel `?` delivers through the queue above. The
machinery that held those, and the permissive hatch beside it, are in
`git log`.

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
  stays, narrowed to the regime where its rent is paid. The shell fuses
  every program, UIs included, unless `--no-fusion` is given
  (`CFlag::FusionDisabled`).
- **Admitting stateful builtins as fast fns with per-site memory:** a
  fast fn that remembers is a DynCall slot by another name — the same
  multiplicity questions return.

## The rules in brief

- **Strict fusion** (`design/strict_fusion.md`): fusion admits pure
  computation only, and the `Effect::Io` calls gated on their trigger's
  fire. A builtin fuses iff its `Effect::Stateless` carries a `FastCall`
  or its `Effect::Io` an `IoCall`; one that performs under a select arm
  fuses there only when its trigger fires at every entry (it fires with
  the scrutinee or the arm's binds, through a nested select's scrutinee
  only and never a function's result; a performing callee performs only
  on its formals, `analysis::io_follows`; and no arm has a guard,
  `lowering::arm_io_blocker`), and a tail recursion is one native loop
  only when its Io and its handler-ful raises follow its formals
  (`lowering::site_memory`: a constant-derived raise is an
  activation's at its birth); `?` fuses with or without a covering catch (a handler-ful
  raise is queued and delivered after the run through the same path
  `Qop` uses), except a handler-ful `?` reachable under a select arm,
  callee bodies included: its raise may be owed to a fire the arm
  missed asleep or to a constant at its first entry, and a kernel remembers
  neither, so it node-walks (`lowering::arm_raise_blocker`), as does a
  guard whose handler-ful raise or performing call does not fire with
  its arm's binds (`lowering::unfollowed_guard`): a kernel runs its
  prologue guards even where unreached, with those binds bottom; the
  node-walk runs only guards the chain reaches; only a
  `never(..)` arm is a
  standing bottom (its args are still emitted and consumed), any other
  bottom-typed arm body runs; everything else — stateful/effectful builtins, `connect`,
  `~`, `Any`, `Catch` — node-walks, transitively. A kernel's only
  cross-invocation memory is the firing boundary (prev-length words,
  first-call words, per-site/per-activation blocks; in a loop each is
  the slot's, never shared by its iterations) and its loops' cost sites,
  which decide nothing a program sees; no replay caches, no selection
  memory. The runtime loans a kernel exactly `KERNEL_ABORT`,
  `KERNEL_PANIC`, `KERNEL_ENV`, the deferred raises and Io calls
  (`emit_helpers::Deferred`), the self-block counters (`SELF_BLOCK_GEN`
  and its kin), the core-trait value hooks and the fork
  mode (`par_loop::ParLoan`): a map-family loop at a kernel's top level
  runs as chunks of its slots, in order or forked
  (`design/parallel_eval.md` §10).
  `#[native]` asserts zero node-walk residue at a source location and is
  THE advertised performance model (an argument a fused call is fed by
  the node-walk is residue; bound outside the annotation it is an
  input). A registry attribute inside a definition body stays pending until
  an instance is built, including a dynamic call or a later REPL input;
  the definition alone does not require fusion to dispatch it.
  `#[sync]`/`#[async]`/`#[pure]` assert analysis facts
  (`#[pure]`: nothing that makes an arm sleep but `dbg`, which warns),
  and
  `#[tail_recursive]` analysis's gate plus, for every instance a compile
  or a run-time bind builds (a later REPL input's included), the loop
  fusion built: no call to it left in the node-walk, a dynamic call's
  included, but one dispatching an instance whose body is the loop's
  kernel (`analysis::check_tail_loops`; a bind it covers is fused until
  its failure is reported, once; unchecked where fusion does not run). A
  run-time bind fuses a collection slot's instance (its prototype's
  kernels) and one an assertion it reaches covers, no other (a function
  value called with no attribute in reach node-walks); it does so as its
  runtime does, whatever its site's flags (the package root's have fusion
  off, a site a collection or a builtin builds has none): a runtime whose
  flags disable fusion disables its context's (`FusionCtx::disable`).

Coverage today: scalar arithmetic/comparison/logic/casts, producers and
accessors, string interpolation of every part kind, `?`/`$`, a bare `null`
wherever a value stands, the eight array HOFs as native loops (nesting
included; filter_map's unwraps a `` `Keep `` result and skips a null),
structural select destructuring with scalar and variant
payload binds, owned binds of a nullable's payload, of a slice's rest,
head or whole (`[x, tail..]`, `all@ [_, ..]`) and of non-scalar elements,
tag tests and binds over a primitive union, every pattern at an
element, a payload or a list head as at the top (a binding or-pattern
included), a tuple, struct or slice pattern over a union holding one
member it may pass, scalar literals over an option, a result
or a union, `never()` arms as bottom
productions of the merge shape, or-patterns, list patterns, type tests
that hold the scrutinee's type, a written tuple, struct or array test
over a composite whose type it does not hold, any union of encoded members as one
value (`` [`A, `B, null] ``, a list beside an array) with its written
type tests, a union naming union typedefs as the flat union they name, tail loops
over any kernel param
kind, every fast-fn builtin and non-inline cast, cross-kernel lambda
calls, trait default bodies, a self-recursive instance's body, its
self-calls calling its own kernel, wherever an instance's body fuses
(`fusion::fuse_instance_body`: a compiled instance whose call does not
fuse as a region, and the loop a collection's callback, or a function
value an assertion covers, binds at run time), a collection callback's
call of the function whose body runs it inline being that function's
self-call, each loop slot rooting its own activation, and so a callback
that is that function's bare name (`array::map(xs, t)` in `t`), whose
slots make the call, the collection's own instance's included where the
function is its callback (slot-root chains, `design/kernel_instance_state.md`). A subtree that does not fuse whole
descends: every part that calls a function (a lambda call, a collection
operation) is tried as a region of its own, any other part only
descends (`fusion::fuse_parts`); a lambda call whose argument does not
fuse takes that argument as a node-walked feeder of its kernel
(`fusion::try_fuse_feeding_args`); a `let` bound to a lambda literal
emits nothing in a kernel, which calls it statically; a collection that
does not fuse whole fuses its prototype's callback instance, and each
slot's instance takes those kernels at its bind (`fusion/share.rs`:
by attempt ordinal, where root, inputs, return, callees and raises
agree). An attribute on a
node absorbed into a larger kernel still has its target checked
(`Attribute::check_target`). `FusionStats.failed` is a blocker profile,
not a gap count.
