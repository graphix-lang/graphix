# Strict fusion — retreat to pure computation?

Status: RULED by Eric 2026-09-01 — "Complexity needs to pay rent, and
your report makes it pretty clear that this whack of complexity can't
afford to live in our compiler. Strict fusion is the way forward." —
and BUILT the same day: the default flipped (`fa08136a`), then the
stateful-kernel machinery was deleted outright rather than kept
behind a hatch (Eric: "I don't think we should wait for the soak to
delete, we can always pull things out of the git history"). See
"The deletion" below for what went. The question, in Eric's words: "Are we trying to fuse too much? How much
simpler would both our lives be if we defused everything but stateless
non-async code that contains only fastcall builtins? … the user can
still get performance by structuring their code to arrange pure
computation together … the code we did fuse would probably be faster
because it would strip out all the mechanisms we're using to deal
with the above."

## The case

Every soak class since July has lived at one boundary: fused code
with interior cross-invocation state. Site identity (per-site inner
Applies, site words, per-slot chains, per-activation block trees and
their reclaim), the stale/taint mask delivery protocol, the
SLEEP_RESTARTS and has_stateful_reach gates, selection-memory words,
the quiet-frame trio, wire bit 2 / wake hints / the birth view — all
of it exists to make stateful kernel interiors mimic the interp, and
the default-arg-birth finding showed the mimicry can reproduce the
interp's bugs so faithfully that BOTH engines break identically and
the differential oracle goes blind.

Under a pure+fastcall-only regime, kernels are pure functions of
(inputs, discs) with exactly one kind of cross-invocation memory: the
firing boundary (output discs, taint propagation, one prev-length
word per HOF loop for resize detection). The user model collapses to
one sentence — "pure code over fastcall builtins fuses" — and
`#[native]` marks every cliff.

## The prototype

`GRAPHIX_STRICT_FUSE=1` (dbgenv; zero default-path change): refuses
emission of (a) any DynCall-path builtin dispatch — everything that
is not a direct FASTCALL, which covers every stateful, effectful,
seam-gated, and defaulted-label site in one predicate; (b) fused
`connect`; (c) handler-ful `?`. Lambda callees are covered
transitively (their bodies hit the same gates). Refusal = node-walk =
the canonical semantics by construction.

Known prototype over-refusals a real implementation would admit: the
`cast<>`/qop pseudo-sites (pure, compiler-internal, `fastcall: None`
only as an artifact). The numbers below are therefore a FLOOR on what
strict keeps.

## The numbers (2026-09-01, washu-chan, release build, quiet box)

**Semantic safety**: all 464 findings-corpus pins agree under strict.
The one flag raised was `dyncall-tagblind-print-aug2026/04`, whose
`#[native]` on an effectful loop correctly became a compile error —
the cliff signal working as designed.

**Coverage census** (per-program fused-kernel counts, both modes,
whole findings corpus): 94% of kernels survive (821/877); 401 of 464
programs fuse identically; 12 lose all fusion, 51 lose some — and
the losers are overwhelmingly the stateful-fusion BUG WITNESSES
(dyncall-stale-arg, sleep-restart-gate, tagblind-print,
recursive-activation-blocks, …), which this corpus over-represents
by construction.

**Bench corpus** (bench/run.sh, then variance-checked with repeated
runs — the min-of-N harness was noise-skewed on first pass):

| bench | verdict |
|---|---|
| fold_sum, fold_floatmath, filter_fold, map_fold, list_fold_sum, mandelbrot, leibniz_pi, symbolic, stream_stats | flat |
| list_map_fold, netidx_stream | flat (runs overlap; strict's best faster) |
| composite_seams | strict consistently FASTER (~37 vs ~42ms) |
| tail_sum | **+6.5%** (31.75ms stable vs ~33.8ms) — the only real regression |

tail_sum's loop kernel is IDENTICAL under strict (same back-edged
CLIF); the delta is call periphery (the fused result-statement region
vs an interp call site entering the loop kernel), which admitting the
pure cast pseudo-site would likely recover — no user modification
involved. The stream benches were the predicted risk and came out
flat because their stateful pieces (array::window in a `<-` RHS,
count in control flow) already sit at seams fusion never crosses:
**the bench corpus already writes hot code in the strict idiom
without having been told to** — pure folds in fused position,
stateful/async machinery at the boundaries. That is the strongest
form of the "user can restructure" argument: no restructuring was
needed even once.

## What a real strict regime would delete (sketch)

DynCall site identity end to end (slots' instances/recipes/site
blocks, SelfBlock trees + reclaim, SiteAnchor chains,
slot_state_sites), the dyncall stale/taint side-channel and the
dispatcher's inner-Apply path, the P7/stateful/key-0 gates
(replaced by the one admission predicate), selection-memory words +
arm-wake init overrides + wake hints + wire bit 2 + the birth view,
kernel replay words (audit — likely reduces to prev-len only),
arm-lift machinery in kernels. Kept: the firing/taint boundary
discs, the ABI, prev-len words, the scaffold loops, tail collapse,
native recursion (now stateless), cross-kernel calls.

What survives regardless: the entire interp semantics built this
arc (wake catch-up, the birth rule, sys::net reconcile) — that is
language semantics, engine-independent, and all 464 pins hold under
both regimes.

## The deletion (2026-09-01, the commit after the flip)

Deleted, in one cut, with the workspace gate green after it:

- The `GRAPHIX_PERMISSIVE_FUSE` hatch (`graphix_strict_fuse`): the
  three strict gates became the only path.
- The DynCall dispatcher end to end: `graphix_dyncall`,
  `dispatch_typed`, `DispatcherState`, `DynDispatchHandle`,
  `DYN_DISPATCH_HANDLE`, `DynCallSlot` (+ `SiteInstance`,
  `SlotRecipe`, the site-id mint), `Kernel::dyn_slots`, `FnParam` /
  `FnSource` / `BuiltinSlot` / `KernelSig::fn_params`, the region-wide
  combined slot table and the DynCall `base` half of the JIT cache
  key, `CastApply`, `QopDeliverApply`, `pre_bind_*`/`pre_init_*`, the
  pooled-buffer arg marshal (`value_buf_stack` keeps its constructor /
  HOF-result role).
- Site identity: `emit_dyncall_site_word`, `claim_slot_site_words`
  (the per-slot identity pairs), the key-0 bucket and its mask
  special cases, the `WAKE_HINT`/`DISPATCH_WAKE`/`dyncall_wake` wake
  hint, `DispatcherState::woke`.
- The interior gates: `sleep_restarts`/`stateless` on the site info,
  `arm_depth`/`value_arm_depth`, `saw_*_reach`,
  `self_backedge_in_*`, `KernelSig::has_{restart,stateful}_reach` and
  the callee-side / deferred checks (P7 and its wake-catch-up
  widening). Their replacement is the one admission predicate at
  discovery: a builtin registers a `FastFn` or it node-walks.
- Selection memory: `sel_state`/`SelWord` claims in
  `emit_select_node`, the select ids in `slot_state_sites`, the
  `record`/`woke`/`eff_init` machinery of `emit_select_value_arm`,
  `LowerCtx::{init_override, wake_override}`, and the fused select's
  `woke` word. A fused select claims nothing; its firing is organic.
- Arm-lift: `collect_lifted_connect_targets`, `FusionCtx::arm_region`,
  `KernelSig::lifted`, the reserved head state words, the minted
  per-instance BindIds, `emit_let_node`'s seed-select, `is_lifted` /
  `lifted_state_off`, `emit_connect_node`, `graphix_set_var` /
  `set_var_typed`. `Connect::emit_clif` refuses ("connect is an
  effect").
- Handler-ful `?`: `emit_qop_deliver`, `try_register_qop_deliver`,
  `FnSource::QopDeliver` (the mid-kernel write through the
  dispatcher). Replaced the same evening by the DELIVERY QUEUE —
  see below.

Kept, renamed where the old name lied: the abort flag
(`KERNEL_ABORT`, `graphix_abort_set`/`graphix_abort_peek` — the
interrupt/depth-trip/bottom-abort channel), the fastcall trampoline
(`graphix_fastcall` over the shared `fast_dispatch` core) plus the
new `graphix_castcall` (the Cast pseudo-site: `cast_value` against
the site's interned destination `Type`, resolving type names through
`KERNEL_ENV` — the kernel's env, loaned for the wrapper call like the
core-trait value hooks), wire slot 0 bit 2 (the kernel's own `slept`
bit now actually sets it — it was write-never before), prev-length
words, first-call words, the per-call-site blocks / anchor chains /
per-activation trees that carry them, and the shrink reclaim.

The replay-word plumbing went in the commit after: with the DynCall
result caches and the rides already gone, `claim_state_word_replay*`
had no caller, so `replay_state_words`/`replay_value_pairs`, the
`SiteLayout`/`SelfBlock` replay lists, the honor headers (and the
`site_desc` high half that published them), `reset_self_block_tree`,
`Kernel::drop_replay_values` and the `allow_replay_state` plumbing
all followed. `Kernel::reset_replay` is a no-op: every word a kernel
keeps is semantic.

`Kernel` is no longer generic: it holds no `Apply`s and no `Node`s.

## The `?` delivery queue (same evening)

Eric: "`?` is central to array access … the fact that it fuses with
no catch and doesn't with catch is actually even worse for
predictable performance." The handler-ful `?` was the one refusal
in the deletion that was not about state: the delivery is an
effect, but a pure function of the kernel's inputs. So it came back
stateless. A failing handler-ful `?` calls `graphix_qop_raise(site,
disc, payload)` — `site` an interned `QopSite` (handler, own top,
spec), the error CLONED onto the invocation's `QOP_RAISES` queue
(a scoped thread-local like `KERNEL_ABORT`/`KERNEL_ENV`, saved and
restored around nested invocations reached through the value
hooks). `Kernel::update` drains the queue after the wrapper returns,
in push (= execution = node-update) order, and runs
`node::error::deliver_error` for each — the handler path factored
OUT of `Qop::update` so both engines call one function: same-top
Vacant-insert / `set_var` on an occupied entry, cross-top `set_var`,
in-frame `frame_outbox` parking. Delivery keys on a FRESH error
(`is_fresh`: not tainted, not stale), exactly `Qop::update`'s
fired-only rule. The value side is untouched (the failing `?` was
already the tainted placeholder that continues). Pinned by
`lang/errors.rs` `catch_array_index_fused` (a `#[native]`-free Jit
fixture: the handler counts the fused region's raise) and the
`checked_div0` flip back to Jit.

## The three follow-on calls (Eric, same day)

- **Pure selects FUSE** — kept as measured.
- **Grow the FASTCALL set as big as we can**: sweep every remaining
  non-fastcall Sync builtin. Known conversion targets: `re::` (global
  pattern cache keyed by the pattern string), `str::parse` (the
  init-time cast type moves to a per-call argument or a keyed cache),
  `sort` (audit the unread-body note), `escape`/`unescape` (global
  cache keyed by the config value). Stays out on semantics: the
  partial-delivery producers (opt::or/and/contains/or_default/
  ok_or/zip, core::divide) — a fastcall sees all args present, and
  their short-circuit-on-partial IS the semantics.
- **`#[native]` is THE advertised performance model** — the book
  documents: pure code over fastcall builtins fuses; `#[native]`
  asserts it and errors on the cliff; stateful/effectful/async code
  runs on the reactive interpreter, structured at the seams.

## The fastcall growth sweep (2026-09-02)

A census of every `EFFECT = Sync` builtin without a fast fn found 57
(the macro-generated ones included). Two compiler gaps blocked the
common spellings before any builtin could convert:

- **A labeled DEFAULT the call left unwritten** node-walked the site
  (the trampoline reads the buffer AS the args, so an unwritten label
  was a hole — json::write_str's `#pretty`, 2026-08-30). The CallSite
  compiles every unwritten default as its own arg node
  (`Arg::is_default`, typechecked per site), so discovery now marshals
  that node like a written one (`MarshalArg::Default(name)` →
  `CallSite::arg_named`). `sort(a)`, `escape(s)`, `hbs::render(t, d)`
  fuse as written.
- **A result DIRECTED by the return type** (`str::parse`'s `'b`) had
  no way to reach the fn. `FASTCALL_TYPED` is a
  `fn(&Env, &Type, &[Value])`: discovery bakes the site's resolved
  `CallSite::typ()` beside the fn pointer and `graphix_typedcall`
  runs it under the kernel's env loan — the same loan the Cast
  pseudo-site already used, which is now the same dispatch
  (`SiteDispatch::Typed(cast_typed, target)`; `graphix_castcall` is
  gone). The interp twin passes `typecheck1`'s `resolved.rtype`
  through `fast_eval_typed`, so both engines run one fn on one type.

The conversions: `re::{is_match, find, captures, split, splitn}`,
`str::{parse, escape, unescape, split, rsplit, splitn, rsplitn}`,
`array::sort`, `list::{concat, flatten, sort, unzip}` (one
`sort_values` in core replaces two copies), `core::error`,
`buffer::encode`, `hbs::render`. A fn that COMPILES a configuration
from its args (a regex, an escape table, a Handlebars registry) keeps
it in a bounded thread-local `FastMemo` keyed by the configuring
values — a cache, never state: a miss rebuilds from the key, one memo
per thread serves every site, and it clears whole when full. The
per-instance memos these replaced were the reason the builtins were
not stateless-declarable as fast fns; they also carried a partial
production (a bad pattern erred before the subject arrived) that was
an accident of the code shape, not a contract, and is gone.

What remains is out by RULE, not by gap: the partial-delivery
producers (opt's short-circuits, `divide`'s reset, `filter_err`'s
ride on a non-error), the stateful family (`count`/`sum`/`min`/
`max`/`mean`/`product`/`uniq`/`once`/`take`/`skip`/`hold`/`window`/
`group`/`and`/`or`), the lambda-taking HOFs (`filter`, opt's
callback forms), effects (`print`/`dbg`/`log`/`exit`/`now`/`rand`,
`buffer::decode`'s ref writes, the http handle constructors) and the
json/toml/pack readers (async by design). `bench_mandelbrot_iterate`
stays a builtin call on purpose — it is the bench's un-fused
comparison point.

## Transition plan (as executed)

1. Flip the default (`fa08136a`), admit Cast, re-annotate the fixture
   corpus (FuseExpect + `#[native]` pins on shapes that now
   node-walk), full gates. 2. The sep01b strict-default soak round
   started on the five remote boxes. 3. The deletion (above) — done in
   one cut instead of the staged plan, on Eric's call, with the
   replay-word cut as the commit after. 4. The fastcall growth sweep
   (above, 2026-09-02); the book chapter rides behind.
