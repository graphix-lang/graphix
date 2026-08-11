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
