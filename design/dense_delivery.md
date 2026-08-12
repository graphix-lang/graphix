# Dense delivery: TagValue everywhere, caching eliminated

Status: APPROVED (Eric, 2026-08-11) — implementation in progress on the
`dense-delivery` branch. This document is the semantic authority for the
redesign; the phased migration plan lives with the branch.

## Why

The sparse interpreter currency — `Update::update -> Option<TagValue>`, with
consumer operand caches riding on `None` — grew three defects no patch could
fix:

1. **Two bottoms.** `None` ("no production") and `TagValue::tainted` ("a
   production whose value is bottom") are both ⊥ to a reader of the type, but
   they have different reactive consequences (ride vs poison). Builtin authors
   confronted `Option<TagValue>` without the compiler-internal context to tell
   them apart.
2. **A second caching channel.** Each node decides what to cache; the STALE
   tag added a *delivery-side* refresh channel on top of the consumer caches,
   and the two had to agree by discipline rather than construction.
3. **Tag-blindness.** The tags were advisory. Every one of the 81 raw-`Apply`
   stdlib builtins read `Some(_)` as "fired" — `once` burned its one-shot on a
   stale re-surfacing, `count` over-counted, `print` duplicated, `now`
   resampled, `rand` redrew — live bugs in the plain node-walk, invisible to
   the value-trace oracle.

### Why not remove the tag instead

Three attempts, three deaths, one structural reason:

- v1 `frame_bottom` bool + fired re-delivery — dead in one hour of jul10e
  soaking.
- The `GXLambda::last_result` value side channel — could not distinguish
  None-as-bottom from None-as-quiet; t1/t4/t6 regressed in an hour.
- The 2026-08-08 dependence-scoped frame invalidation (post-mortem in commit
  `2a35dab6`): the refills were removable, but the tail spine's
  becoming-selected path "still needs to say 'emitted, but not an event' — a
  second bit by definition — and deriving firing from dependence instead
  cannot replace it: a parent asking 'did anything I depend on fire' overrides
  a child that has already decided to be quiet, which reproduces the bug it
  was meant to fix."

The bit must exist, and it must travel WITH the value (productions flow
through event maps, frames, and the tail-call stash, where a side channel
separates from its value in time — `1d1a9999` is the commit that had to teach
the stash this). **Do not attempt removal again.** Since the tag cannot go,
it goes everywhere, honestly.

The retained fire-gate witness (`fire_gate_missing_fire_aug08d.gx`, bisected
2026-08-08) is the empirical capstone: "delivering a non-fired arg as ABSENCE
is itself unsound, independently of whether the call is made." Absence is not
a sound encoding of either quiet or bottom. Dense delivery is the
counterfactual that witness demanded.

## The model

Every awake node delivers every cycle. There is no `None`.

### The four states

`Tag` becomes two orthogonal bits — **fired** ("this is an event") × **bottom**
("there is no usable value") — giving four states:

| state | meaning |
|---|---|
| `Fired(v)` | an event carrying a value |
| `Stale(v)` | present, not an event — the value channel |
| `FreshBottom` | the computation failed/produced-nothing THIS cycle (1/0 just happened) — an event with no value |
| `StaleBottom` | a standing bottom, nothing new (includes the phantom "never produced") |

Both bits propagate by OR over consumed inputs — one join rule
(`Tag::join`), replacing "taint ORs, stale AND-reduces, TAINT⟹STALE". The
old invariant is dropped: a fresh bottom is fired+bottom, which is exactly
the bare-TAINT disc the CLIF emitters already mint at div0, `?`-errors, and
missing params — the orthogonal algebra names what the kernel already does.

Force points: an output becomes an event iff fired ∧ ¬bottom. The runtime
boundary (`gx.rs` do_cycle) remains the sole dense→event filter. `?`/`$` and
unchecked-arith log on **FreshBottom consumption only**, at every depth
(Eric's ruling: no depth-0 exemption — an n-iteration loop over a fired error
logs n times; standing bottoms never log).

### The signature

```rust
fn update<'a>(&'a mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> &'a TagValue
```

Borrowed production, no Option. Every *computing* node owns one production
slot — the resident IS the return slot, initialized to a bottom-tagged
phantom (`never()`, never-produced, and pre-first-value async sources are all
this one state). Quiet cycle = downgrade the slot's tag to Stale in place and
return the borrow; triggered inputs = recompute into the slot. *Delegating*
nodes (Block→last child, Bind, Module) forward the child's borrow — zero
copies down delegation chains. `Apply` has the same shape (+ `from`);
`Apply::out_tag` is deleted — the tag rides the returned production.

There is no additional clone cost versus sparse: clones occur only at genuine
store points (store writes, composite builds, the FFI staging copy), all of
which the sparse design also paid, and delegation chains now clone strictly
less.

### TagView — the one consumption API

```rust
pub enum TagView<'a> {
    Fired(&'a TagValue),
    Stale(&'a TagValue),
    FreshBottom,
    StaleBottom,
}
impl TagValue { pub fn view(&self) -> TagView<'_> { ... } }
```

Exhaustive matches are the rule, compiler and stdlib alike. The payload refs
are `&TagValue` (the tag rides the disc's upper byte, so no untagged `Value`
exists at any address to lend); value access goes through the masking APIs
(`with_value`/`value_cloned`), preserving the mask-by-construction boundary
guarantee. Bottom variants carry nothing — the placeholder is never a usable
value. The boolean accessors (`is_fired`/`is_tainted`/raw tag bits) are
demoted to the JIT boundary (disc packing in `fusion/`), scoped pub(crate).

### The persistent store

`event.variables` becomes `IntMap<BindId, (TagValue, cycle_stamp)>`,
replacing both the per-cycle map and `rt.cached` (which is deleted).
Producers write on genuine production only — including bottom: the store
keeps at-rest bottom, and does NOT retain pre-bottom values (an undesignated
ride). Readers interpret: stamp==now → the entry's tag; stamp≠now → Stale
(StaleBottom for bottom entries); absent → phantom. Frames, select arm binds,
and DynCall arg side-channels become scoped OVERLAYS (read-through, private
writes shadow, dropped at pass end; writes are private and notifies deferred
to escape — `frame_outbox` is the template and the one channel that outlives
a drop).

### The three laws

**R1 — recompute.** A node's resident equals its function over its inputs'
current values after every update; the tag plane is derived by the join. As
an optimization a node may skip recompute and Stale-downgrade in place when
no input delivered a triggering tag (Fired|FreshBottom) — valid ONLY at frame
depth 0, where the law *"Stale ⇒ payload unchanged since last recompute"*
holds. Frames violate that law by ruling (tail-jump STALE chains carry
advancing values), so framed passes recompute unconditionally — exactly the
kernel. This law is global and unenforced: debug-assert it under a test flag;
the fallback is unconditional recompute (CPU, not correctness).

**R2 — read-side init.** Every FIRED backfill in the old tree (per-top init,
arm-wake refill, fresh-bind primes, new-slot callbacks, default refs,
dynamic-module prime, DynCall first dispatch) collapses into one rule at the
store read: a reader under an init view interprets a standing entry as Fired;
otherwise as Stale. `event.init`/`frame_init` survive as per-reader novelty
flags only.

**R3 — bottom propagates.** Consuming a bottom bottoms the production —
uniformly, including builtin argument seams: a bottomed arg makes the wrapper
produce bottom for the invocation without calling the builtin. "Taint never
reaches builtins" (2026-07-19/20) evolves into "bottom never reaches
authors". There are no seam registers and no consumer-side rides. The
legitimate rides are DESIGNATED semantic memory owned by the riding node:
the select's scrutinee resident (the aug06ghz0 ruling — the standing
selection lives on against ITS memory), `~`'s held arg, tail-rebind formals.
In dataflow terms these are the language's `pre`/`fby` — explicit delay
state — rather than ambient cache behavior.

### The design north star (Eric, 2026-08-11)

"Try to live with a clean dataflow semantics before we pollute it with hacks
to make it look more normal." The value plane becomes classic dataflow —
every edge carries a value every cycle, a total valuation, the spreadsheet
law (R1) — and graphix's event-driven character is reduced from an
architecture to an annotation: the fired bit, which survives because firing
is observable in the language (effects, `count`, `~`, strict select).

## What this preserves (fixed points)

The rulings are reproduction targets; only the mechanism changes: the strict
select rule (2026-08-06), the scrutinee ride (aug06ghz0 — now the select's
resident; tail-position selects do not ride, preserving the depth-trip
unwind), sleep-is-pause (2026-07-31), body-driven firing / organic tags
(replay-frames v3), the depth-trip settled bottom (2026-07-23 — now uniform
across depths and engines), the genuine-call gate (the passive re-poll wedge
protection), selection memory (SelCell/SelSnap), `Sample.triggered` debt
(now Fired-only — the post-jul23e depth-0 behavior made uniform),
`FoldSlot.held`, builtin semantic state, and the async builtins' documented
arm-rewake RESTART latches.

Collection slot values and `StructWith.current` get ONE rule: they are
residents — semantic uniformly, surviving sleep AND frames (safe because
absence is unrepresentable, so the sparse leak class they were cleared for
cannot occur). `reset_replay` shrinks toward nothing.

## What this deletes

Consumer operand caches (`Cached` + ~24 field sites, `CachedVals`,
`CachedArgs.last_result` as correctness), the `produced && determined`
composite idiom, all FIRED backfills and refills (R2), `seed_externals`, the
frame stale backfill, the tail-arg `Option<TagValue>` stash (None dies; a
bottom entry means ride), ALL depth-0 absence conversions (the
callsite/kernel stale-taint filters, the taint→None dispatch exit, the
log+None error minting), PRIME-then-REPLAY (no caches to fill — bind fresh,
seed SelSnap, run one ordinary pass; sound because `transient_body_ok`
restricts transient bodies to STATELESS builtins), FoldQ's next-acc-removal
(absence as a signal), `gate_tainted_args`, `Apply::out_tag`, `rt.cached`,
and the kernel's presence/fired reconstruction (`args`/`fired_this_cycle`/
`param_fired` — the feeder tag IS the flag) plus the DynCall taint/stale
masks (tags ride in-band, per representable_bottom's region-width
constraint).

Supersedes where applicable: `design/replay_frames.md` Ruling A.2 ("STALE and
TAINTED never escape frame depth 0") is repealed — density is exactly that
repeal, with the jul10h-000007 protection moving from the depth filter into
tag-aware builtins. The DynCall fire gate (`c038c091`, branch fire-gate-min)
is superseded unlanded — it was the sparse answer ("don't deliver, don't
call") to the problem dense dissolves ("always deliver, tagged").

## Rulings recorded 2026-08-11 (with the plan approval)

1. **Bottom propagates** at builtin arg seams — no seam registers. The
   2026-07-19/20 eval-decides ruling, the jul30a re-woken-arm ride pin, and
   `array::window`'s []-on-absent-val pin become ruled deltas.
2. **Log everywhere** — FreshBottom-gated logging at every depth; frames now
   log; standing bottoms never log.
3. **Fresh-at-instantiation** for cross-module callee reads of module state —
   resolves the module-state-callee-reactivity pending ruling in the
   direction R2 implies.
4. **Fire gate superseded** — fire-gate-min never lands; its witness becomes
   a flip-phase adjudication item expected to flip to agreement.
5. **TagView is the API** — no custom builtin-facing arg enum; exhaustive
   matches everywhere; boolean accessors demoted to the JIT boundary.
6. Kernel marshal uses honest tags: FreshBottom stages bare-TAINT,
   StaleBottom/phantom stage TAINT|STALE — a standing bottom stops firing
   loop/select machinery (a conscious change; corpus re-verify SlotFlags
   folds, woke-forced-FIRED, set_var gates).

## The ruled-delta list

Observable changes ruled intended IN ADVANCE (encoded as red→green fixtures
before the flip; the flip-phase divergence enumeration must map every
divergence to this list — anything off-list stops the line):

1. Tag-blind builtin fixes: once/count/take/skip/queue/hold/iterq stop
   consuming stale ticks; print/dbg/log stop duplicating; now/rand stop
   resampling; exit stops exiting on stale.
2. HOF stale-laundering fixed: callback subgraphs of opt HOFs, core::filter,
   array::group, net::publish/publish_rpc, http::serve, queuefn see honest
   tags instead of unconditional fired.
3. `fire_gate_missing_fire_aug08d` flips to agreement.
4. `missing_fire_epoch3_aug08e` agrees (kernel depth-trip becomes a delivered
   FreshBottom instead of a whole-kernel abort).
5. jul10h-000007's protection moves into array::group — verify re-pinned.
6. Log cadence: standing bottoms never log; each Fired error logs once per
   consumption.
7. Bind bottom persists in the store — fresh-reader resurrection of
   pre-bottom values dies.
8. `~` debt tightening: Fired-only, uniform across depths.
9. once/take/skip/count/hold/uniq revert to Sync (exact per-arg firing
   restores the pre-F2-flip classification) — fusion coverage changes.
10. str_split/rsplit/splitn/rsplitn gain EFFECT=Sync (landed pre-branch —
    their fixtures' storage-law attribution was wrong; the missing const was
    the real blocker).
11. Internal pacing changes are invisible at the GXEvent boundary; any
    trace-visible delta beyond this list indicates a mis-gated consumer.
12. Guard stale re-deliveries no longer force re-matches.
13. Bottom-propagates deltas: builtins bottom on any bottomed arg — the
    jul30a ride pin, array::window's []-on-absent-val pin, and the
    max(fired, bottomed) program class change; fixtures re-blessed.
14. In-frame error logging appears (frames were silent).

## Open items carried into implementation

- The R1 Stale⇒unchanged law: unenforced; debug-assert + recompute fallback.
- `tail_position` scoping of the no-ride rule is an analysis artifact (both
  engines agree; the select.rs:356-362 residual notes carry over unwitnessed).
- The overlay notify contract ("writes private, notifies deferred-to-escape")
  needs exhaustive multi-top tracing.
- Borrow pressure points: select emits through its own resident (a clone per
  emission — identical to today's `cached.clone()`); kernel staging copies
  per param (identical to today's marshal). Prototype-first on select /
  the tail stash / FoldQ before any mass conversion.
- `event.init`'s residual role is decided narrowly at the flip.

## As-built: the P4 builtin seam (2026-08-11)

The author-facing seam is `TagValue::view()` plus three TRANSITIONAL
helpers in graphix-package-core (they die with the P6 adapter
deletion), and the family gate is one field, `ExecCtx::dense_seam`
(default `false`; the 5b flip turns it `true`):

- `seam_tick(tv, ctx.dense_seam)` — the EVENT decision. Closed: fired
  or stale ticks (the sparse `to_option()` consumption, bit-exact).
  Open: fired only. Bottoms never tick in either mode — they cannot
  reach a builtin seam pre-flip (the CallSite taint gate), so the
  bottom arms are unreachable backstops, quiet per the `CachedArgs`
  precedent rather than replaying sparse tick-with-placeholder.
- `seam_value(tv)` — the value-plane read (config/latch args): fired
  or stale, ungated (dense and sparse agree).
- `seam_publish_tag(tv, ctx.dense_seam)` — the HOF republish tag.
  Closed: FIRED (the sparse laundering). Open: the honest arrival tag.

One finding from the conversion: every `update_diff`/`triggers()`-gated
decision (throttle, timer, the escape template) was ALREADY stale-quiet
— `triggers()` excludes STALE — so those sites take `tv.is_fired()`
with NO gate; the tag-blind hazard lives only in the `to_option()`/
`is_absent()` seams. Migrated behind the gate: once/take/skip/count/
uniq/queue/hold ticks, print/println/dbg/log effects, now/rand/exit,
array::group (the jul10h-000007 protection moved in-builtin) and the
iter/iterq families, filter/opt-HOF/queuefn laundering. Own-field
refactors (ungated, pure): throttle's `last_v` emission source (its
`CachedVals` deleted), the timer family per the same rule.

## As-built: the P5a reorder (2026-08-11, Eric approved)

The planned pre-flip consumer-cache deletion was REFUTED by the code:
`Cached::reset_replay` is not passive storage — it implements the
frame contract (clear on frame entry unless the subtree is closed, the
`invariant` OnceLock), which child residents do not reproduce, so a
pre-flip deletion rides pre-frame values where the cache correctly
forgot them. Deletion therefore moves AFTER the 5b flip (P5b′), where
every awake child delivers `Stale(resident)` every cycle and the slots
become pure pass-through mirrors — trivially deletable under an
∅-diff gate against the post-flip baseline. The `produced &&
determined` idiom moves with it.

What landed as P5a (5a-lite): `CachedVals::update_full`'s summary fold
is `Tag::join` (provably identical through the `from_raw` clamp), and
the Q1 BOTTOM-PROPAGATES seam lives in the wrappers behind
`dense_seam`: `CachedVals::any_bottom` (poisoned-at-rest or
never-delivered slot) makes `CachedArgs`/`CachedArgsAsync` bottom the
invocation without calling `eval` — `TagValue::bottom_null(triggers)`
mints honest FreshBottom/StaleBottom bits that read as the sparse
TAINT until the 5b clamp removal. The entire ~254-builtin EvalCached
family becomes dense-correct the moment the gate opens, with no
per-builtin work.

## As-built: P5b, THE FLIP (2026-08-12)

The interp is dense; the kernel stays sparse behind boundary adapters
until 5c. The full rule list lives in commit 0c18f15c (store
authoritative + read_var seam, overlays, prime/backfill/save-restore
deletions, honest algebra, stale-born constants, the select flow
driver, collection slot fills, fold last-slot poison scope, Fired-only
Sample debt, Q2 log cadence). Three kernel-side 5b→5c adapters carry
the still-sparse kernel across the flip:

- **Feeder poll (input)**: only a TRIGGERING bottom drops the retained
  slot and fires; a STANDING bottom is the dense ride channel — no-op,
  the kernel must not re-fire per quiet cycle of a bottomed feeder.
- **DynCall dispatch (output)**: a PRESENT BOTTOM production from the
  inner Apply (the Q1 wrapper's `bottom_null`) is the sparse
  protocol's "no value" — return None so the call site takes
  DYNCALL_PENDING and mints the typed #219 taint placeholder. This
  was the 5b SEGV: passing the bottom through as a value handed the
  CLIF call site `Value::Null` with the tag stripped, and the typed
  adapter adopted Null's UNINITIALIZED payload word as an owned
  ArcStr pointer (masked_outer_call_cache_ride, crash location
  str::replace, corruption origin two dispatches upstream —
  GXDBG_DYNC found it in one run).
- **DynCall delivery (tombstone)**: a taint-masked slot delivers an
  explicit ABSENT tombstone into the overlay instead of no write. The
  arg Refs are SHARED across site instances and a dense Ref RIDES its
  resident on a read_var miss — another site's last delivery, the
  site-identity rule violated through the dense read model (the same
  program's follow-on divergence once the SEGV was fixed: the outer
  site's masked arg rode the inner site's "xyz"). With an entry
  present for every slot every dispatch, the shared residents are
  never consulted; per-site ride state stays where it belongs, in the
  per-site CachedArgs slots. Dies with the masks at 5c.

Net effect of the adapter pair on the dyncall seam: bottom in →
bottom out, coherently — the dyncall-partial-args window fixtures
flipped from mapped desyncs to AGREE ahead of 5c.

## Reorder note: P5b′ follows 5c (2026-08-12)

The P5b′-before-5c placement assumed "every awake child delivers
Stale(resident) every cycle" — true for interp children post-flip,
FALSE at the kernel boundary until 5c: `FusedKernel::update` still
returns ABSENT on quiet cycles (the sparse output filter), and the
consumer caches are what ride history across those absences wherever
a fused region sits under a Cached consumer (a select's fused
scrutinee, `~`'s fused arg). Deleting them first would require an
absence shim per consumer — complexity, not the planned
simplification. After 5c kills absent everywhere, the deletion is the
trivial mirror-removal the plan described. Same resequencing class
Eric approved at P5a (deletion follows the flip that makes it
trivial).

## As-built: 5c — the kernel flip (2026-08-12)

The kernel boundary is honest; the deliberate 5b engine desync is
CLOSED (corpus sweep: 22 divergences at 5b close → ∅; the one-sided
adjudication key retires). What landed, per seam:

**Seam C (output).** The CLIF return gate (`emit_force` on a
not-fresh disc → pending) is DELETED with its `gate_stale_at_return`
plumbing: every kernel returns its result's honest TAINT/STALE tag
in-band on the disc, and `Kernel::update` decodes the production —
Fired/Stale carry the value into the resident, bottoms free the
placeholder and produce the shared FreshBottom/StaleBottom. A quiet
poll rides the resident (the R1 skip); `TagValue::absent()` is GONE
from the kernel output. Pending is reserved for genuine aborts and
SPLIT BY CAUSE: a depth trip is a delivered FreshBottom
(`peek_depth_trip` — `FusedKernel` still takes the flag for the
diagnostic; missing_fire_epoch3_aug08e fixed), an interrupt rides.
`builder.rs`'s depth-0 fired-only filter (replay_frames Ruling A.2)
is repealed — `FusedKernel` forwards the honest production.

**Seam A (input).** Feeder staging packs straight from each
production's tag: value+STALE for quiet rides, bare TAINT for a
triggering bottom, TAINT|STALE for a standing one (the ruled
marshaled-param choice — a standing bottom must not fire loop/select
machinery). `Kernel::args` (the retained per-arg slots) is DELETED:
dense feeders deliver every cycle and the R2 store read is the
wake/arm-replay memory. The unresolvable-Binding early-out rides
instead of vanishing.

**Seam B (DynCall).** The masks stay — they ARE the honest per-arg
tag channel (taint×stale = the four states; the in-band constraint
was about kernel param discs, already satisfied). The DELIVERY is
honest: a taint-masked slot delivers FreshBottom/StaleBottom and the
wrapper's Q1 arm bottoms the invocation (the 5b tombstone and the
pre-dense ride-own-history semantics are the re-blessed
dyncall-partial-args delta). The RETURN is the production WHOLE
(`Option<TagValue>`; `dispatch_typed` transmutes the tagged words):
the call site adopts the in-band tag — stale resurfaces stop reading
as fires (the dyncall-stale-arg class closed) — and the per-arg
neutral-disc result folds die (the wrapper already joined args into
its production tag). The placeholder path keys on pend|taint|mismatch
and preserves the return's STALE. `default_external_refs` priming is
deleted (default trees' Refs read the store, R2).

**The taint-cache scoping (the big Q1 rule).** The interior-bottom
"prior success degrades to STALE + cached value" site caches
implemented the PRE-DENSE consumer contract, repealed by the flip.
They are now scoped to the interp's two VALUE-DRIVEN ride contexts
(`in_ride_scope` = `loop_depth > 0 || guard_depth > 0`): loop bodies
(per-slot values are designated memory, fork 7) and select guard
interiors (guard truth reads cached values, which survive under a
poisoned tag). Everywhere else — dyncall results, qop results, the
div node, the select merge — BOTTOM PROPAGATES. The select scrutinee
ride (`emit_scrut_ride`) is untouched designated memory. The four
aug07 storage-law fixtures now FUSE (the refusals and the ASPIREd
value residents are both obsolete).

**The framed tail-loop cluster (interp).** Five 5b-deletion holes
re-expressed under R1/R2/R3: `prev_looped` survives quiet polls;
the framed first pass seeds its frame with the formals' per-cycle
truth (standing → QUIET — a fresh() upgrade over-fired
ignored-capture; the kernel stages retained params STALE); selects
RE-MATCH on any scrutinee value view inside frames (framed passes
are the kernel's value-driven re-derivation; stale jump plumbing
must advance the loop); arith recomputes unconditionally in frames
(the R1 skip is depth-0-only; Q2 logging stays trig-gated); frames
never write the store (R3 — the store keeps ENTRY values for the
framed seed), with the tail stash consuming arg PRODUCTIONS directly
(stale plumbing is never published; the old read_var stash had been
reading store contamination by accident).

**Fresh-instance seeding (interp).** A fresh bind's ids (callsite
arg ids, instance formal pattern ids) have no store history, and
quiet productions never publish — the kernel's
every-param-per-invocation delivery is restored on the VALUE channel:
bind-time `store_insert_standing` seeds at depth 0 (STORE-only — an
overlay entry would shadow the R2 init-view upgrade), overlay-only
inside frames. A becoming-selected wake with a non-triggering
scrutinee production binds FIRED (the arm's init view — under dense
`arg_prod` is never None, so the old unwrap_or(FIRED) had gone dead).

**Also fixed en route (interp regressions from 5b):** FoldQ's firing
seeded from source/init DELIVERIES instead of slot productions (a
const-body fold re-emitted per source tick against the organic-tags
ruling — the twochannel class); this had been mis-adjudicated at 5b
as an expected delta (gates-are-not-the-fuzzer: pattern-matching the
pin name is not deriving the expected behavior).

**Deleted adapters:** the 5b feeder-poll input adapter, the dispatch
output adapter and tombstone, `gate_stale_at_return`/`emit_force`/
`is_not_fresh`, the neutral-disc folds, `default_external_refs`,
`Kernel::args`. `Tag::clamp_sparse` is zero-caller (removed at the
close). `fusion/` contains no `absent`/`is_absent` reads.

## P5b′ re-scoped: cache deletion is a designation pass, not mechanics

The 5c taint-cache arc proved the premise wrong a second time (the
first was the P5a reorder): "every consumer cache is a pure mirror
post-flip" holds only for TAG-DRIVEN consumers. The VALUE-DRIVEN
consumers — the select scrutinee ride (`arg.cached`), guard truth
tests (operand caches read under a poisoned tag), collection slot
values (fork 7) — hold DESIGNATED ride memory in exactly the way the
kernel's `in_ride_scope` caches now do, and Cached's
value-kept-under-poison discipline is what implements it. A blanket
interp-side deletion would re-create the 5c-4 regressions in mirror
image. P5b′ is therefore a DESIGNATION pass: classify each of the ~24
`Cached` sites as mirror (delete — op operands outside guards,
statement/merge positions, the wrapper arg slots whose Q1 arm already
decides) or designated memory (keep, renamed for what it is); the
`produced && determined` idiom and `rt.cached`'s mirror die with the
mirrors. Same gate as planned: ∅-diff against the post-5c baseline.

## P5b′ designation inventory (2026-08-12)

The classification, from reading every site. The test that separates
the classes: is `cached` ever READ under a poisoned tag (value-driven
ride — designated), or only on the non-bottom path where it equals the
child's current dense production (mirror)?

**DESIGNATED — keep, renamed `Cached` → `Held`:**

- `Select.arg` (select.rs) — the scrutinee ride (aug06ghz0): `bottomed
  && arg.cached.is_some()` IS the ride condition; binds/matches read
  the history under poison. Frame discipline unchanged (reset_replay
  clears — frame state never survives).
- `PatternNode.guard` (pattern.rs) — guard truth memory: `is_match`
  takes `&self` and CANNOT consume the guard node's production; the
  held truth is read at re-match time, including under a poisoned
  guard. Architecturally forced designated memory (the kernel's
  guard-scope cache twin).
- `Sample.arg` (mod.rs) — the `~` hold: declared semantic
  (reset_replay deliberately skips the clear; "sample the latest" IS
  the contract).

**MIRROR — deleted, replaced by direct production reads:**

- op.rs arith/cmp/bool lhs/rhs; StringInterpolate.args;
  ConnectDeref.rhs (the retarget-write reads the production's stale
  value — same value); TypeCast's absent arm; the dead `update_args!`
  macro (zero users).
- data.rs Struct/Tuple/Variant element slices; StructWith.n AND
  StructWith.current (the source production carries the array every
  cycle — Arc clone, no held copy needed).
- array.rs ArrayRef.source/i, ArraySlice.source/start/end, Array.n;
  map.rs Map.keys/vals, MapRef.source/key.
- collection.rs MapQ/FoldQ `source`/`init` — consumed at delivery
  only; the genuine cross-cycle memory (`self.current`, `self.init`)
  already lives in separate fields and stays.
- select.rs arm bodies (`Vec<(PatternNode, Cached)>` → plain `Node`):
  every emit reads the cache immediately after the arm's update, on
  the non-bottom path.
- `rt.cached` (the value-half mirror map): `Rt::cached()` readers (the
  static-resolution fallbacks, module/kernel primes, buffer::decode)
  convert to store reads. The one semantic delta is RULED (delta 7):
  `cached` retained pre-bottom values where the store keeps the
  standing bottom — fresh-reader resurrection of pre-bottom values
  dies, which is what delta 7 ordered at 5b; these readers were the
  residual.
- `TagValue::absent()` / `ABSENT_BIT`: exactly ONE producer remained —
  `Connect` (a value-less ⊥ statement; its sibling `ConnectDeref`
  already returned `phantom_ref`). Connect → `phantom_ref`, the ABSENT
  machinery and its 6 consumer checks die. Absence becomes
  unrepresentable BEFORE the mirror deletion so `Cached::update`'s
  Option collapses honestly.

**NEITHER (stays, re-documented):** `CachedVals`/`CachedArgs`/
`CachedArgsAsync`. Post-flip the arg slots are a STAGING BUFFER (the
kernel's marshal twin) — overwritten from every dense delivery, and
the Q1 any_bottom arm means `eval` never reads a slot whose production
was bottom, so the keep-value-under-taint discipline is dead weight on
the EvalCached path. It is NOT dead for the `update_diff` raw-Apply
users (str::escape, timer, net::write...), which read slots outside
the Q1 gate — that is the P6 seam, untouched here. The design's
"wrapper arg slots die" is thus re-scoped: the CACHE role died at 5b
(Q1); the buffer role is the marshal and stays.

**The join normalization (rides along with the deletion).** The
scalar ops, StringInterpolate, ArrayRef/Slice/MapRef, and TypeCast
minted `FRESH_BOTTOM` whenever ANY delivery arrived while an operand's
at-rest tag was tainted — including all-quiet cycles where every
consumed production was StaleBottom. data.rs composites (and Any)
already derived bottomness from PRODUCTION tags (`produced |=
t.triggers()`; quiet → ride), which is `Tag::join` and what the
kernel's CLIF propagate rules do (taint ORs, stale AND-reduces; the
ruled marshaled-param choice — a standing bottom must not fire
loop/select machinery). The mirror rewrite normalizes every node to
the join. This is what makes the deletion CLEAN: under the join, a
phantom child and a bottomed-with-history child converge (both
StaleBottom on quiet cycles), so the `determined` bit (the caches' "has
ever produced" residue) carries no information. Verified unobservable
at every force point pre-change: `?`/`$` pass bottoms through and log
only fired ERROR VALUES (the source logs are input-trig-gated);
Connect/set_var/gx-emit gate on `is_fired`; select's flow driver is
`!bottomed`-gated; Q1 wrappers bottom the invocation either way. The
gates (suite/sweep/regress/captures) are the check.

## As-built: P5b′ (2026-08-12)

Four commits (`99c787ab..`): absence death, the mirror deletion, the
rt.cached death, the 5c-orphan sweep. What the code now says:

- **The dense node template.** `node::gather` (join element production
  tags + collect values — one clone where the cache fill paid one),
  `dense_gate!` (the uniform recompute gate `trig ∨ bottom-resident
  refill ∨ frames` — R1 — plus the bottom join: FreshBottom iff a
  delivery triggered, standing bottoms ride), and `read_prod!` (one
  child's production into the join accumulators). Every former mirror
  site is an instance of this template; the recompute gate is now
  UNIFORM across the families (previously arith had the full gate,
  cmp/bool recomputed on any delivery, composites on triggers only —
  all observably equivalent at force points, now one rule).
- **`Held`** (né `Cached`) survives at exactly the three designated
  sites: the select scrutinee, pattern guards, `~`'s arg.
  `Held::update` returns a bare `Tag` (absence is unrepresentable).
  `StructWith.current`/`current_tag` died with the mirrors (the source
  production carries the array every cycle).
- **The select emit path** reads arm PRODUCTIONS (`arm_prod!`): the
  quiet/same-arm emissions take the join rule (a standing-bottom arm
  rides the select's resident instead of re-minting FreshBottom); a
  BECOMING-SELECTED wake onto a bottomed arm still emits FreshBottom
  unconditionally — the selection change IS the event (strict select).
- **`Rt::store_value`** is the one cross-cycle value read (store value
  half; bottom ⇒ None — ruled delta 7's residual carve-outs closed:
  the static-resolution fallbacks, dynamic-bind/module seeds, kernel
  fn-param primes, buffer::decode, shell Ref snapshots no longer
  resurrect pre-bottom values). `Rt::cached`/`cached_insert`/
  `cached_remove`/`GRAPHIX_STORE_ASSERT` are gone; publishers write
  `store_insert(id, TagValue::fired(v))` / `store_remove`.
- **Absence is unrepresentable**: `Connect` returns the phantom (its
  sibling `ConnectDeref` already did), `TagValue::absent`/`is_absent`/
  `ABSENT_BIT`/`to_option` deleted, `Update::update`'s doc states the
  dense contract plainly.
- **`CachedVals` stays** as the builtin arg STAGING BUFFER (the
  kernel's marshal twin) per the designation section — the P6 seam
  owns its raw-Apply/`update_diff` users.

Open note for Eric (pre-existing, unchanged by this pass, now visible
in one place): the empty-composite constant arms handle frames
explicitly, and the uniform gate recomputes composites in frames — but
`Variant`'s ZERO-PAYLOAD arm still lacks the frame-init handling its
Struct/Tuple/Array/Map empty twins have (the pre-existing family
inconsistency the plan flagged; no witness, left as-is for ∅-diff).

**P5b′ gates (2026-08-12, all GREEN):** workspace suite 2433/0 across
66 binaries (one db_subscribe_on_remove contention flake under full
parallel load, 3/3 solo green); regress 303 programs 0 regressions;
fusecheck 303 programs 0 mismatches — 5 gains surfaced and were
VERIFIED as 5c's (a scratch worktree build of the fuzz binary AT
091de13a reports the identical 5; the manifest predated the kernel
flip's storage-law un-refusals; blessed in da2e436e); detcheck 503
programs (303 corpus + 200 generated) 0 flaps; leakcheck 3 witnesses
0 leaks; paired stdout captures vs the blessed 5c baseline ∅ in both
modes — every diff in the documented noise set (self-timed/
timestamp-seeded bench, rand/now nondeterminism, netidx pacing, the
two free-running soak counters prefix-identical at differing lengths).
P5b′ is CLOSED.

## As-built: P6 — the stdlib long tail (2026-08-12, 4c8e9b44)

**The update_diff seam is gone.** `seam_arg` (package-core) is the
per-arg dense read for raw-Apply builtins with designated own state:
update the node, return `(value, fired)` — the value channel is `None`
for bottoms, and bottoms never tick (Q1 applied at raw seams). The
seven `update_diff` users read productions directly: net::write,
net::subscribe, net::call, net::list/list_table, net::publish,
net::publish_rpc, http::serve. `CachedVals::update_diff`, the
`arity1!/arity2!` extractors, and every migrated builtin's
`args: CachedVals` mirror are deleted; `sleep`/`reset_replay` keep
only designated state. http::serve's quiet path returns the honest
`out.ride()` (the deferred 5b item — its phantom_ref placeholder dies).

**Q1-normalization deltas** (bottom cases that were UNREACHABLE
pre-flip — the CallSite taint gate silenced them — and went live at 5b
with sparse-era arm semantics; all normalized to "a bottom delivery is
no event, designated state rides"):
- net::subscribe's `(None, true)` arm UNSUBSCRIBED on a FreshBottom
  path with no history — dead code pre-flip, live-and-unruled after.
  Now: no event; the standing subscription rides (the language way to
  stop a subscription remains arm deselection/sleep).
- net::call re-CALLED the rpc with held args on a FreshBottom
  delivery; net::write re-wrote the previous value; net::publish
  re-published/updated; publish_rpc and http::serve tore down and
  republished proc/listener from held values. All now fired-gated.
- net::write's resubscribe re-write reads the value production's
  stale channel: a value bottomed at rest is NOT re-written to a new
  path (delta-7-consistent — no resurrection of pre-bottom values).

**The adapter is deleted; the sparse view is unrepresentable.**
`ExecCtx::dense_seam` (constant `true` since the flip) is gone;
`seam_tick(tv)` is Fired-only with no gate parameter;
`seam_publish_tag` is deleted (the honest republish tag is
`tv.tag()`); the `CachedArgs`/`CachedArgsAsync` Q1 bottom arms are
ungated. `seam_tick`/`seam_value`/`seam_arg` remain as the durable
authoring vocabulary — no sparse arm survives to select.

**buffer::decode verified verbatim:** the wrapper runs `eval` only on
the fired join (stale → retag STALE without eval; bottom → the Q1 arm
without eval), so its `set_var` effects cannot re-run on stale
refreshes or bottoms. Residual audit: every raw production read in
stdlib flows through `seam_arg`/`seam_tick`/`seam_value` or the
`CachedVals` staging buffer; the one bare pump (`Never`) discards by
design.

**Flagged pre-existing (NOT changed — for Eric):** net::write drops
its old `Dval` on a path SWITCH (and on the invalid-path teardown)
without calling `NetState::unsubscribe`, unlike `delete`/`sleep` which
do — the old path's wake registration for the write's BindId appears
to linger until delete. Pre-dates dense; preserved verbatim.

**P6 gates (2026-08-12, all GREEN):** workspace suite 2433/0 across
66 binaries; regress 303 programs 0 regressions; paired stdout
captures vs the blessed 5c baseline ∅ modulo the documented noise set
in both modes. Three off-list capture diffs surfaced and were
adjudicated, none behavioral: audit__03 and
empty-scaffold-depth-charge__01 had EMPTY blessed nofusion files (the
4-way-parallel blessing run's contention flake — the pinned 5c binary
run solo prints exactly what P6 prints; blessed files repaired with
the verified content), and select-guard-shortcircuit__02 is inherent
stdout ORDER nondeterminism (two tops' prints interleave; the same
binary produces different orders run-to-run, line MULTISETS identical
in both modes; added to the noise record). P6 is CLOSED. Remaining
after P6: P7 Sync flips, P8 corpus re-adjudication + docs, P9 soak.
