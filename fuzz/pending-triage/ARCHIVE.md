# pending-triage — ARCHIVE of closed rounds

> The per-class triage records, verbatim, moved out of README.md on
> 2026-08-30. Every round here is CLOSED; README.md holds the protocol,
> the open items and the one-line ledger. These records are the
> mechanism write-ups — read the matching one when a class recurs.


## The aug18a arc (2026-08-20) — 3 fixed, 3 open

The fleet round on campaign aug18a (hz0/aieka/katana/ryouko, campaign
binary 1b1778b3, the organic-firing P4 soak): 12 divergences, 6
classes. Fixed and committed same day:

1. **framed-formal-seed** (c4fa9407) — a framed dispatch seeds its
   quiet args every pass; the fold-in-rec-arm that never published.
2. **trip-poison-extent** (6df2ec60) — one shared depth-trip poison
   bit with pop-to-zero extent; the kernel rode across trips (and
   refused legal rides past the root).
3. **connect-instance-identity** (e05a6c8b) — connect-target liveness
   in dead-elim, per-instance minted lifted ids, loop/rec/arm lift
   gates; the write-only-let spinner family.

Open, each parked here with mechanism located and a design question:

4. **class 4 — CLOSED** (2026-08-20): THE SHRINK-TO-ZERO RULE —
   always-executed loop-EXIT re-ensures (TruncRec records propagating
   outward per frame) truncate every in-loop chain when its level
   shrinks; no prewalk needed. Pins:
   findings/slot-shrink-truncate-aug2026/ (three faces: DynCall
   pairs, nested levels, callee site blocks).
5. **class 5 — CLOSED** (2026-08-20): ruled AND built same day —
   THE BOTTOM-OUT RULE (design/activation_state.md; ruled with
   state-multiplicity=activation-multiplicity). The finding INVERTED
   (the kernel's tail refusal was right; the interp's ride
   re-emission face is deleted, the kernel's value-position folds
   and undetermined-guard chain match). Pins moved to
   findings/bottom-out-aug2026/. Open follow-ons live in the design
   doc: the mid-loop guard-bottom residue (the back-edge bucket
   AUDITED CLOSED 2026-08-20 — covered by 003fa7d6's per-activation
   trees; degrade doors unreachable and now loud).
6. **class 6 — CLOSED** (2026-08-20): NOT a fusion leak — both modes
   flapped identically in isolation; `constrain_known` (+2 sibling
   walks) drained a name-keyed AHashMap in per-process hash order.
   Fixed by (name, TVarId) sorts; pin:
   findings/constrain-order-diag-aug2026/.

## The aug20a round (2026-08-22) — 5 findings, 1 class, CLOSED

Campaign aug20a (hz0/aieka/katana/ryouko on ad091e65, the
activation-state soak): 5 divergences — ryouko 4, hz0 1 — pulled at
the module-system redeploy (aug22a); all five reproduced on merged
main (bdd013b0). Triaged and fixed 2026-08-22; pins moved to
`graphix-fuzz/findings/quiet-frame-init-view-aug2026/` (00–04 the
campaign witnesses, 05–07 the two further faces found while
isolating).

**Shape:** an `array::iter`-driven binding `m` read ONLY by the guard
of a structure-failed arm, inside a `let rec` tail chain. Every `m`
delivery re-derives the chain (quiet framed passes in the interp), and
on each pass the `0 =>` arm is re-woken after sleeping on the n≠0
pass — loop plumbing, not a trigger. The interp emits once; the JIT
re-emitted per delivery.

**Root cause — NOT the guard fold** (the suspect above was wrong; the
consulted-guard chain is fine): a re-derivation inside a QUIET FRAME
(`frame_depth > 0 && !frame_init`) is not an init view — the interp's
Constant/Ref/Bind/lambda-priming sites all gate on `frame_init` there
— but three kernel mechanisms manufactured one anyway:

1. `DynCallSlot::sleep` reset `fired`, so every post-wake dispatch
   was a FIRST dispatch — forced `event.init`, every arg delivered
   fired, STALE mask ignored — and the arm-body DynCall fired on
   constant args the interp delivers stale. The interp's
   `CallSite::sleep` keeps `first_update`: a re-woken site is resumed,
   not re-primed (sleep is pause); only a site's first-ever dispatch
   is the `bound` init-view dispatch, and THAT one keeps its forced
   view at any frame depth (43e6af90's FIRED seeds — pins
   frame-formal-init-view-aug2026, which a frame-gated first dispatch
   broke on the first try). (Faces 00–04.)
2. A fused select's selection-changed word (`woke`) granted the
   re-selected arm an init view on every NATIVE tail-loop iteration
   (05), and in a callee kernel that cannot know statically it runs
   per iteration (06).
3. The same word inside a fused sub-region of an INTERP frame (07).

**Fix:** `DynCallSlot::sleep` no longer resets `fired`; wire slot 0
gains bit 1, THE QUIET FLAG — set by the wrapper from the interp
frame, by a tail-loop body for itself when `!init`, inherited by
callees through the context word — under which becoming-selected
grants no init view (a first-ever call/dispatch still does, as the
interp's `bound` dispatch does). See CLAUDE.md "Fusion / JIT
subsystem" (the QUIET FLAG entry).


## The aug22c round (2026-08-24) — 11 findings, 5 classes, CLOSED

Campaign aug22c (hz0/aieka/katana/ryouko on e9791a6a, the quiet-frame
soak): ~2 days, 11 divergences — hz0 7, aieka 2, ryouko 2, katana 0 —
pulled at the traits redeploy (aug24a). All 11 reproduced on merged main
(f60bbf2d); none was traits-related. Every one came from the
corpus-mutation source (~236M subjects); generate (~260M) and reactive
(~281M) found nothing. Triaged and fixed 2026-08-24, all in the compiler
(four mechanisms, one of them a typechecker unsoundness). Pins:

- **A + B — one class, `nested-bind-stmt-dead-elim-aug2026`** (7
  witnesses): a `let` bound INSIDE a statement's subtree — an array or
  struct literal, a variant payload, a select scrutinee — and later used
  as a connect target (A: `x <- x` lost its spin in the JIT; B: the tail
  read `x` and the JIT published nothing). `emit_block_node`'s
  dead-statement elimination ran its later-reader scan only when the
  statement WAS a `Bind` and treated every other statement as unread, so
  the literal was eliminated whole and the connect target's seed with
  it. A statement binds whatever its subtree binds: the scan now runs
  over every statement's `Refs`.
- **C — `labeled-callback-param-aug2026`** (2): TWO bugs. The
  typechecker accepted `array::map(xs, |#foo: i64 = 42| foo)` — a
  callback with NO positional parameter — because `FnType::contains`
  computed "first positional index" as the LAST labeled index when no
  positional followed and zipped `#foo` against the declared `x: 'a`
  (`FnType::first_positional` now). And for the legal
  `|#foo = 42, x| foo` the inline loop emitter bound the element to
  parameter INDEX 0, labeled or not (the JIT read `foo` as the element);
  `callback_param` refuses callbacks with labeled parameters — there is
  no inline binding for a labeled default — so they interpret.
- **D — `dyncall-value-return-stale-aug2026`** (1 + 2 faces): the
  README's first reading was wrong (not a cadence question — the interp's
  1 is the SlotFlags rule). An in-loop DynCall in a CALLEE kernel is an
  unclaimed key-0 site: it delivers its args fired by design and restores
  the honest plane by folding the real arg discs' STALE into the RESULT
  tag — and the Value-shape return branch adopted the dispatcher's disc
  raw, skipping the fold. A constant-arg `bytes`-returning builtin (and
  a non-scalar cast, which lowers to the same DynCall with a `[T, Error]`
  Value return) therefore fired on every invocation of the callee.
- **E — typechecker, fixtures in `lang/select.rs`** (1): the README's
  first reading was wrong here too — the program is ILL-TYPED, and the
  interp was the engine reporting it. A select's type is the union of
  its arm types and a free `'b` in one arm stays free (`str::parse` has
  no concrete result — the literal-`i64`-arm twin was always rejected).
  When the sibling arm's `i64` arrives through a bound tvar
  (`array::iter`'s instantiation), the instance check
  (`setup_static_bind`'s return write-back) compared the union
  `['b, 'a]` against a `resolve_tvars` copy of itself, and the Set×Set
  residue arm let the bound member "cover" the copy's FREE member by
  binding it (`'b' := i64`), so the prototype typed by accident while
  the per-slot callback instance settled `'b := Bottom` and bottomed at
  eval (the runtime tc1 failure is swallowed at `log::trace`). A free
  rhs member now goes to the residue and aliases the bare lhs member.
  Consequence: three shapes that compiled by accident are rejected
  consistently now; annotate the result (`let v: i64 = select ..`).

## The aug24a round (2026-08-24) — 4 findings, 2 classes, CLOSED

Campaign aug24a (hz0/aieka/katana/ryouko on c9c1e7cb — the traits merge
plus the aug22c park) ran ~4 hours before this redeploy: 4 divergences —
hz0 2, katana 1, ryouko 1, aieka 0 — all four from corpus mutation.
Pulled with the new `graphix-fuzz/fleet.sh pull`. Both classes closed the
same day; the raw witnesses are removed and the pins carry the record.

- **F — already fixed** (ryouko): the aug22c class D mechanism found
  again, independently, on the pre-fix binary — `map::insert` folding a
  MAP accumulator through `list::fold` inside a `let rec` callee body
  (interp 1 production, JIT 5). It AGREES on `3450a07b` and is pinned as
  `findings/dyncall-value-return-stale-aug2026/03_map_acc_in_callee_loop.gx`,
  the first face of that class whose Value-shape return is a Map.
- **G — `typedef-cell-mode-parity-aug2026`** (hz0 x2, katana): COMPILE-MODE
  SKEW, a second mechanism in the family of
  `fusion-mutates-tvars-aug2026` — but oracle-visible, because both modes
  reject with DIFFERENT text (`List<'a: bool>` under `--no-fusion`,
  `List<'a: unbound>` with fusion). `Env::seed_typedef_refs` — the eager
  pass that fills every typedef's carried resolution cell — ran inside
  `if ctx.fusion.enabled`, so the STDLIB compiled with filled cells in one
  mode and empty cells in the other, and a ref whose cell is filled takes
  a different path through `contains` than one whose cell is empty
  (`ref_id` keys identity off the cell, `lookup_ref` resolves through it).
  The failed unification bound the call site's `'a` in one mode only. The
  verdict agreed here; the INFERENCE CHANNEL did not, which is the part
  that could have differed on a program that compiles. A user-local
  typedef of the same shape always agreed — the skew needs a typedef
  compiled in an earlier `compile()` call, i.e. the stdlib.
  Fixed by seeding in both modes: the pass is a typecheck-time fact
  ("every name's final target is registered exactly here"), not a fusion
  one, so `seed_typedef_refs()` moved above the fusion gate. The
  `graphix-shell` `check_mode_parity` test now iterates over both witness
  families.

## The aug25a flood (2026-08-26) — 82 divergences, TWO classes, both fixed

The first fleet round over the traits/P2 merge (campaign binaries at
6e62fff3). 81 of 82 findings are one class; neither is new to the
campaign's delta.

- **A — `set-eq-drops-cell-link-aug2026`** (the flood; every box):
  `list::fold(list::map(l, |x| <fn value>), i64:0, |acc, x| acc + x)`
  ACCEPTED by the typechecker (`acc + <fn>`), array twin and let-bound
  twin both refused; the engines then diverged downstream. The
  DIVERGENCE first appears at 1c64ba3d (the genn-Ref lookup let the
  region fuse), but the ACCEPTANCE hole predates the campaign.
  Mechanism, confirmed by twin bind traces (string element vs fn
  element): pre-unification binds the map call's return cell to
  fold's `List<'a>` EXPANSION; a callback returning a FUNCTION is
  generalized at its def gate (`unbind_tvars` — rtype cell back to
  None, fn kept as a constraint), so map's `'b` aliases a still-open
  cell; the rtype write-back then compared
  ``[`Cons('a, ..), `Nil] == [`Cons('b, ..), `Nil]`` with `Type::eq`,
  whose TVar arm calls two distinct unbound cells equal, and the
  whole-set fast arm's by-NAME `alias_tvars` merged nothing — `'a`
  and `'b` never met, verdict true, zero commits. A concrete element
  stays BOUND (`Some(string)`), fails the eq, and takes the
  committing walk — the entire fn-specificity. Fixed by holding
  `contains`' equality fast paths (whole-set arm, member pre-pass,
  residue reflexive check, `ref_id` param dedup) to
  `union_identical` — the documented union-collapse rule ("a collapse
  on `TVar::eq` drops the discarded cell's future binding") applied
  to its other consumer. Pins:
  `findings/set-eq-drops-cell-link-aug2026/` + graphix-tests
  `list_map_fn_element_fold_rejected`. Found with the new
  `CHK-CONTAINS` verdict prints: a passing check with zero interior
  events between its operands and its verdict is a fast path that
  committed nothing.
- **B — `arith-widened-cell-aug2026`** (ryouko, aieka; fixed 64fbdaf3):
  `filter_err({let x = f64:0. * f64:0.; x})` — the consumer's
  parameter type (`['a, Error<'e>]`) widened the multiply's result
  cell, the let classified two words wide, the arith emitted a bare
  F64, and `bind_local` PANICKED cranelift — the runtime thread
  aborted and the oracle read `CompileErr("runtime did not respond")`.
  A panic, not a wedge; any narrow scalar triggers it. Arith now
  widens to the representation its own type declares
  (`widen_to_declared_repr`). Pins:
  `findings/arith-widened-cell-aug2026/`.

## The aug24b + aug25a residue (2026-08-27) — 3 findings, 3 classes, CLOSED

What was left in the directory after the aug25a record: aug24b's one
divergence (hz0) and 107 aug25a files (the README's 82 plus what the
fleet kept pulling on the old binaries before it was stopped). All 107
re-run through the fixed tree: 105 AGREE (classes A and B above); the
two that still diverged, both ryouko, were new classes.

- **hold-async-clock-aug2026** (aug24b hz0 divergence_000000; oracle
  fix, no engine change): `hold(#clock, array::iter([100, 200, 300]))`
  with `clock = sys::io::stderr(null) ~ 1`. Interp settled 300, 200,
  200 across three runs; the JIT 100 every time — `hold` emits
  whichever element was held when the clock LANDS, and an async
  clock's arrival cycle is a race. Deterministic clocks (`i64:1`,
  `never()`, `once(..)`) agree on both engines. `hold(` joins the
  oracle's fire-count-sensitive list: a `sys::` program naming it is
  Excluded instead of compared at final values.
- **C — bound-cell-cycle-accepts-aug2026** (ryouko divergence_000006;
  fixed in `contains`): `src <- [i64:0, src]` under an INFERRED
  `Array<'a: Array<'b>>` typed, where the annotated twin refuses. The
  connect's check is `Array<'a> ⊇ Array<'e>` with `'e := [i64,
  Array<'a>]` — two BOUND cells, `'a` reachable from `'e` — and the
  TVar×TVar arm's cycle refusal answered TRUE and marked both cells
  for the terminal settle, which never consults a bound cell. An i64
  then reached a slot typed `Array<i64>`; the JIT read it as an array
  and counted 4 fires of the nested map to the interp's 2. Both bound
  cells now walk their bindings like any two bound cells (the occurs
  check at every bind keeps the walk finite; a pair memo answers a
  revisit coinductively), and the general walk says `Array<'b> ⊇
  i64` — false. Pins: `findings/bound-cell-cycle-accepts-aug2026/`,
  graphix-tests `connect_self_nesting_*`.
- **D — init-over-limit-aug2026/02** (ryouko divergence_000027; fixed
  in `emit_init_loop`): `fold(init(iter([0, MAX]), |i| i),
  count(iter([1, 2])), ..)` — on the over-limit cycle the interp
  bottoms the fold (`[1:1]`), the JIT emitted the fired init
  (`[1:1 2:2]`). The kernel's over-limit path forced `TAINT | STALE`
  into the count's disc, so the init region published a STANDING
  bottom (`Tag(96)`, never published by the bind — the store kept the
  previous `[]`) where the interp's MapQ publishes a FRESH one
  (`Tag(64)`); the fold kernel was then invoked with the source
  `present, not fired` and folded a quiet empty array under a fired
  init. Force only TAINT: validity (`exact_stale`, the slot tables)
  keys on TAINT, and the count's own fired-bit makes the bottom fresh.
  A constant init hid this on both engines by the fired-bit alone.
  Pins: `findings/init-over-limit-aug2026/02_*`, graphix-tests
  `fold_over_oversize_init_bottoms`.

Same day, from the typemorph lane rather than a campaign: the
block-wrap μ class (8 corpus flips) closed by making the μ-collapse
look through binding cells — see CLAUDE.md's `let rec` bullet.

## The aug27a round (2026-08-28) — 6 divergences, 4 classes; 5 fixed, 1 non-bug, CLOSED

Campaign aug27a (hz0/aieka/katana/ryouko/mazikeen on the pre-unified-ride
binary) pulled at the aug28a redeploy — the redeploy that put THE UNIFIED
RIDE (select-on-bottom, 7d36a526) on the fleet. 6 divergences: hz0 3,
aieka 1, katana 1, ryouko 1, mazikeen 0. **None is the select-on-bottom
class; none was introduced by the merge.** Triaged 2026-08-28 (all six
well-typed under `--check`, so every one is a genuine adjudication).
Raw witnesses parked on disk under `aug27a/` (untracked).

- **hz0/000000 + hz0/000002 — FIXED (9e3bae1a)**: ONE class, the
  tail-rebind-by-name bug. A recursive kernel (`ap`/`fold_go`) forwards
  an fn formal whose callback CAPTURES an outer binding spelled like one
  of the callee's own formals — `|x| n` capturing an outer `n` beside
  `ap`'s formal `n`; `|a, x| a + acc + 1` capturing an outer `acc`
  beside `fold_go`'s formal `acc`. The capture threads in as an extra
  kernel input with the SAME BASENAME as the formal, and
  `emit_tail_rebind_jump` resolved its target slot with `lookup_name`
  (back-to-front, so the later-bound CAPTURE won). The loop wrote each
  iteration's update into the capture and the real formal never
  advanced: an infinite loop when the formal is the loop bound
  (hz0/000000, jit Timeout vs interp 0); a dropped accumulator otherwise
  (hz0/000002, jit 0 vs interp 10). `fn_formal_two_callbacks` passed
  because its callbacks don't capture. Fix: resolve the rebind slot
  BindId-first (`KernelParam.bind_id` names the formal exactly). Pins:
  graphix-tests `fn_formal_capture_collides_{bound,acc}`.
- **hz0/000001 — NON-BUG (async artifact)**: `array::group(sys::tcp::
  listen(..))` — interp `[]` vs jit `[TcpListener]`. AGREES on re-check;
  a `sys::tcp` quiescence race that slipped past the `sys::` divergence
  exclusion. Follow-up: confirm the exclusion covers `array::group` over
  a `sys::` stream.

- **aieka/000000 — FIXED (82e4fbfa)**: NOT a typechecker static/dynamic
  ruling after all — a soundness bug in `use`. `let tag = use array::*`
  put a `use` in VALUE position, which compiled to a `Bottom`-typed `Nop`
  that unified with any type (I could bind one variable as both `i64` and
  `string`). The `tag <- e.0` connect routed in the error-payload struct
  at runtime while the downstream `select tag {[init.., x] => x * 100}`
  narrowed the unconstrained type to `Array<i64>`; the fused arm-body
  kernel read a struct where it compiled a scalar (interp garbage-array,
  jit ABI-bottom). Fix (Eric's call, reject-at-typecheck): `use` and
  static `mod` are declarations, not expressions — legal only in
  statement position. A `never()`-typed twin already rejected; only the
  `use`-as-value form stayed unconstrained. Pins: graphix-tests
  `use_in_value_position_is_compile_error`,
  `use_value_soundness_witness_rejected`.

- **katana/000000 — FIXED (d4f046d8)**: NOT an inference interaction —
  a lambda-instantiation truncation. `{let x = f64:0.; {let a =
  array::init(3, |#foo: i64 = 42, x| x); array::fold(a, 0, |acc, x| x)}}`
  gave interp `f64:0.` / jit `i64:0` (both wrong, should be 2). A
  collection callback is instantiated against the HOF's DECLARED type
  (`fn(x: 'a) -> 'b`, 1 positional param); a user lambda with a labeled
  default before the positional (`|#foo = 42, x|`) has 2 patterns, and
  `new_with_body` zipped them against the 1-param type and SILENTLY
  truncated — keeping `foo` (defaulted) and dropping the positional `x`.
  The element was never delivered; the body's `x` fell through to the
  outer `let x`. The general user-HOF path was already correct (full
  arity); only the collection's synthetic dispatch hit the narrow type.
  Fix: a narrow instance signature bails so the Dynamic dispatch retries
  with the full `def_typ`. Pins: graphix-tests
  `labeled_callback_{outer_shadow,default_used}`.

- **ryouko/000000 — FIXED (bfda0913)**: NOT a firing adjudication — a
  recursive-type soundness hole. `list::find(Cons(0, Cons(3, once)), |x|
  true)` (`once` a builtin Fn value in a Cons tail) was ACCEPTED at depth
  >= 2 while depth 1 and an explicit `: List` annotation both rejected;
  the two engines then diverged over the unsound value (interp `[]` / jit
  `[0]`). `contains`' cycle memo keyed every non-Ref RHS to one `None`,
  so the outer `List<'a> >= Cons(i64, Cons(i64, Fn))` and the inner
  `List<'a> >= Cons(i64, Fn)` collided and the deep `Fn` was never
  checked. Fix: content-typed non-Refs get a distinct memo id (finite
  RHS never needed the memo); Any/primitives keep `None`. Pins:
  graphix-tests `recursive_fn_tail_rejected_at_every_depth`,
  `recursive_list_find`.

All six aug27a divergences resolved: 5 real classes fixed (A/B/C/D/E),
1 async non-bug. The round is CLOSED.

## The aug28a round (2026-08-28) — 1 divergence, NEW class, pulled at the aug28b redeploy

Campaign aug28a (the unified-ride binary, pre aug27a fixes) pulled before
the aug28b redeploy. 1 divergence (aieka); re-checked on the aug27a-fixed
binary and it STILL reproduces — a NEW class, none of A–E.

- **aieka/000000 — reactive fold-in-guard / array::group divergence**
  (well-typed, re-verified 2026-08-28 on the fixed binary): `{let x =
  array::iter([1,2,3,4]); let m = x / MAX; let rec f = |n| select n {0 =>
  select array::fold(["a","bb","ccc"], 0, |acc, s| str::len(s)) {0 if m
  == 0 => 1, _ => 2}, _ => f(n-1)}; array::group(f(3), |n, _| n >= 3)}` —
  interp `[]` vs jit `[2:[2,2,2]]`. A reactive program: `array::iter`
  streams, `m` derives from it, and a `let rec` whose base case is a
  guarded select over an `array::fold` result feeds `array::group`. The
  interp emits no event; the jit emits a grouped `[2,2,2]`. DEFERRED —
  needs its own triage (fold-in-guard reactivity, or an array::iter
  fire-count/streaming-timing question). Parked under `aug28a/`.

  **FIXED 2026-08-30 — the third mechanism of the QUIET-FLAG class
  (`findings/quiet-frame-init-view-aug2026/08`).** Not fold-in-guard
  reactivity at all: `f(3)` fires once on both engines (`count(f(3))`
  = 1, bare `f(3)` traces `[0:2]`), and the guard is never consulted.
  What differed was how many values `array::group` RECEIVED — one per
  framed pass on the JIT. The call `f(3)` stays on the node-walk (a
  builtin's argument), so `f`'s tail loop runs in interp FRAMES with
  the base-case arm `select array::fold(..) {..}` as a fused ARM
  region; the fold's per-element `str::len(s)` sites claim their
  identity through the per-slot chain (`emit_dyncall_site_word`), and
  that chain was registered `reset: true` — `Kernel::reset_replay`
  (once per pass, f(3)→f(0)) FREED it. Every pass minted three fresh
  site ids (`GXDBG_DYNC`: 1,2,3 / 4,5,6 / 7,8,9 …), every fresh
  `SiteInstance`'s first dispatch forced the init view, the fold fired,
  the scrutinee delivery fired the arm region, `group` pushed:
  interp `[0:[2]]` vs JIT `[0:[2] 1:[2] 2:[2] 3:[2] 4:[2]]` with a
  `|n, x| x == 2` predicate. The design comment said "chain leaves
  reset on frames → fresh ids, matching the interp's transient
  re-derivation"; the interp's `FoldQ::reset_replay` keeps each slot's
  CallSite (identity + `first_update`) and clears only caches, so a
  framed pass re-dispatches RESUMED sites with honest stale args. Fix:
  slot chains are semantic per-position state — `SiteAnchor.reset`
  and `Kernel::free_reset_chains` deleted (it was the only reset-kind
  producer), `claim_slot_cache_words` → `claim_slot_site_words`. The
  same mechanism leaked a `SiteInstance` per slot per pass (ids never
  retired) — gone with it. Residual (pre-existing, not built): a
  resize that truncates slots frees their chain leaves but never
  deletes the orphaned `SiteInstance`s, so an oscillating-length
  source grows `DynCallSlot.instances` slowly. Gates: regress 450/0
  (the pin included), selfcheck OK, graphix-compiler 161/0,
  graphix-tests 2278/0; witness + campaign original AGREE.

## The aug28b round (2026-08-29) — 5 divergences, 3 classes, all fixed

Campaign aug28b (hz0/aieka/katana/ryouko/mazikeen/washu-chan on the
aug27a-fixed binary, f3e5543c) pulled ~1 day in: 5 divergences — aieka 3,
hz0 1, ryouko 1 (aieka/000002 ≡ ryouko/000000, one class two boxes).
katana/mazikeen/washu-chan 0. Triaged 2026-08-29; three classes, all
well-typed under `--check` (genuine adjudications), all fixed same day.

- **Class 1 — SUPERSEDED by the bottom-ride deletion** (hz0/000000,
  aieka/000001; de-fuse ecdf127f then REVERTED). The de-fuse below was the
  right fix under the unified ride; Eric then reconsidered the ride itself
  (2026-08-29) and DELETED it (`6991e2ad`/`7a564fa9`): a bottom scrutinee
  now bottoms the select on both engines, so class-1 bottoms out at the
  base case (both `[]`) and the tail loop fuses — no de-fuse needed. Pins
  renamed to `findings/tail-select-bottom-out-aug2026`. The original
  triage, kept for the record:
  THE UNIFIED RIDE in a tail-loop spine.
  `select select n {m if m <= 0 => 1 % m, m => f(m)} {0 => acc, _ =>
  ap(f, n-1, acc)}` — the spine holds its `_` selection across n=3,2,1,
  so at n=0 (`1 % 0` bottoms) the bottom scrutinee RUNS the held arm (the
  recursive jump) and the loop continues to n=-1, where `1 % -1 = 0`
  matches the base and emits acc=0. The interp does this (`[0]`); the
  kernel tail spine keeps no per-iteration selection and bottomed out
  (`[]`). Eric ruled (2026-08-29) the interp is right — the unified ride
  applies uniformly, "crazy in, crazy out," no special case. A scrutinee
  only rides if it can bottom MID-loop: a bare formal rides its previous
  value (never bottoms), a loop-invariant capture is constant (bottoms
  from iteration 0 with no held selection, or never). So a scrutinee
  built solely from Ref/Const/comparison/boolean never rides and stays
  fused (the 120-185x family selects on the loop variable — untouched);
  anything else de-fuses to the node-walk (`emit_select_node_tail` +
  `scrut_cannot_ride`). Open follow-on: Eric ruled a tail loop should
  RESET its selection per dispatch on the interp too (the cross-dispatch
  ride sub-case). With the de-fuse, ride-capable selects run node-walk in
  both modes, so this is NOT needed for interp/jit agreement, only for
  semantic purity — and a broad reset (clearing every select's selection
  on a framed re-trigger) REGRESSED `kernel-frame-init-const-fire`, whose
  value-position nested select `select (v2 /? 42) {..}` became a spurious
  re-selection fire where it must stay quiet (stale scrutinee). A correct
  reset must target only ride-capable/spine selects; deferred pending a
  narrower design.
- **Class 2 — `skipped-fn-arg-effect-aug2026`** (aieka/000000; fixed
  7c1e7e14): a `<-` spinner inside a DISCARDED fn-typed argument was
  dropped by the JIT. `f = |a, b| true` ignores `a`, whose value is
  Fn-typed (`str::len`), so `a` is a loop-invariant fn formal that leaves
  the kernel sig (skipped_args); `emit_lambda_call_node` skipped emitting
  the arg node on the premise it is pure. When the arg is a block with a
  connect, skipping it dropped the effect: node-walk spun (capped) where
  the kernel quiesced. An effectful skipped arg now de-fuses (effects
  de-fuse, never silently skip); pure ones still fuse.
- **Class 3 — `framed-arg-stale-formal-aug2026`** (aieka/000002 ≡
  ryouko/000000; fixed 15d386a6): a NODE-WALK bug (the JIT was right). A
  tail-loop fold re-triggered by a fresh seed folded to 100 instead of 55
  in epoch 1: the loop-control `i` decremented right, but the `i` passed
  to the callback `f(acc, i)` was the stale entry-formal 10 every step.
  Inside a frame the store holds the pre-frame value (R3), a stale DynCall
  arg was never published to the cycle-scoped overlay, and the callee
  reads the distinct arg id (not the formal the overlay read-through
  covers). A stale non-bottom arg in a frame now publishes its current
  frame-overlay value onto the arg id. The aug13i stale-entry-formal
  shape, through an fn-formal call.

Gates after all three: regress 445/0, selfcheck OK, graphix-tests 2276/0,
FuseExpect audit 0 mismatches. aug28a still open (unrelated).

### Closeout (2026-08-29)

The class-1 "Open follow-on" (cross-dispatch selection reset) is RESOLVED,
not deferred: the bottom-ride deletion made a bottoming scrutinee bottom
the select before any held selection is consulted, so no reset is needed
anywhere. `ctx.reset_selection` — the core-trait comparator seam that
cleared a reused site's held selection per dispatch — was likewise
confirmed dead and REMOVED (`d4ebb79e`); `core_bottom_key_rule` stays
green. Final gates: regress 449/0, selfcheck OK (1787/0 flaky),
graphix-tests 2276/0.

At the aug29a redeploy the stale aug28b binary was pulled one last time: 5
divergences (aieka 3, hz0 1, ryouko 1), and ALL FIVE re-check AGREE on the
fixed tree — every one a re-discovery of a class this arc fixed, nothing
novel. The raw pull is discarded (nothing to triage); the pins live in
`findings/`. aug28a remains the only open item.

## The aug29a round (2026-08-30) — 1 divergence, environmental, CLOSED

Campaign aug29a (hz0/aieka/katana/ryouko/mazikeen on the ride-deletion
tree `d4ebb79e`; washu-chan ran it overnight too, 0 findings, and was
rebooted for updates before the pull) pulled after ~17 hours / ~310M
subjects at the aug30a redeploy: ONE divergence, ryouko/000000.

- **ryouko/000000 — asymmetric timeout, NOT a bug** (written 16:49:57,
  ~15 minutes into the campaign). `count(500000, 0)` — a stateless
  tail loop — in statement position beside an array literal: interp
  Timeout at the 10s lane budget AND at the 80s retry with <5s of CPU
  burned, JIT a value. The discriminator (Eric's 2026-08-17 ruling:
  seconds of burn = honest slowness, ~0 burn = wedge) recorded it
  CORRECTLY as wedge-shaped. It is not a wedge: `check` AGREEs 30/30 on
  HEAD locally (0.5s total, both engines + stdlib compile) and **5/5 on
  ryouko itself, on the aug29a binary, under the live aug30a load
  (load average 246 on 32 cores): 1.1s wall / 0.85s CPU each**. A
  sub-second program that did not finish in 90s and burned nothing is
  a box-wide stall — the timing fits campaign start, when 256 workers
  cold-compile the stdlib at once (ryouko carries a 62G zram swap and
  had 5.6G of it in use at inspection). No journal OOM/hung-task line
  was readable. Disposition: one-off, parked on disk under `aug29a/`
  (untracked), no pin — the class is "the harness can't tell a stall
  from a wedge after the fact". FOLLOW-UP (harness, not built): the
  finding header should carry the retry's own evidence — CPU burned /
  window, `/proc/loadavg`, `MemAvailable` at the retry — so a future
  triage reads stall-vs-wedge off the artifact instead of
  reconstructing it on the box a day later. `Divergence` has six
  constructors and a cross-process report path, so it is a small
  arc of its own, not a tweak.

## The aug31c round (2026-08-31) — 1 finding, 1 class, CLOSED

Campaign aug31c (all six boxes — washu-chan's first night on the
fleet — on the native-List tree): one divergence, pulled 2026-08-31,
reproduced verbatim on HEAD.

**fold-midchain-fired** — the fused fold derived its firing bit from
the ACC CARRY alone (`result_is_firing`, ba524ee8 #9, 2026-07-05): the
last body evaluation's STALE. A mid-chain slot consuming a fired init
through an `_ => acc` arm while a LATER slot takes a constant arm
leaves the final carry stale — FoldQ fires (`any_trig`: any slot
PRODUCTION triggers, position-independent), the kernel stayed quiet.
Root cause is ruling skew: ba524ee8 matched the pre-dense
consumption-chain node-walk; the organic-tags rework re-ruled the
interp to per-slot productions (dense_delivery.md, the twochannel
class) and the kernel's fold was never revisited — `emit_fold_loop`
was the ONLY loop not folding body discs into `SlotFlags`. Fix: each
body evaluation's STALE folds into the slots word
(`SlotFlags::fold_stale`); the carry stays an ADDITIONAL firing source
(`result_also_fires` — it alone covers the empty-source fold under a
fired init, where there are no body evaluations); TAINT stays off the
flags (consumption decides, the 2026-08-13 option-A ruling — an
acc-ignoring callback recovers). Pins:
`findings/fold-midchain-fired-aug2026/` (00 minimal, 01 the campaign
witness) + the `run!` cadence fixture `array_fold_midchain_fire`
(lib_tests/array.rs — a `<-` counter turns swallowed re-fires into a
value difference `run!` can see).

Reduction note: the campaign witness's guard-select/group front end
was pure observer — bare select+guard, group alone, and a
passthrough-callback fold all AGREE; the divergence needs an
acc-consuming slot BEFORE a const-arm slot (source `[1, 2]` diverges,
`[2, 1]` agrees).
