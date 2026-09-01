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
  `FnSource::QopDeliver`; `Qop::emit_clif` refuses when a handler is
  installed.

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

## Transition plan (as executed)

1. Flip the default (`fa08136a`), admit Cast, re-annotate the fixture
   corpus (FuseExpect + `#[native]` pins on shapes that now
   node-walk), full gates. 2. The sep01b strict-default soak round
   started on the five remote boxes. 3. The deletion (above) — done in
   one cut instead of the staged plan, on Eric's call, with the
   replay-word cut as the commit after. 4. The fastcall growth sweep
   and the book chapter ride behind.
