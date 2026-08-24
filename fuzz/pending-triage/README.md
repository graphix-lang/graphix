# pending-triage

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
