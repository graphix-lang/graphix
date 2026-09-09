# Dense delivery

Status: built 2026-08-13
Pins: `stdlib/graphix-tests/src/lang/dense_deltas.rs` (one fixture per ruled delta below); `stdlib/graphix-tests/src/lib_tests/bottom.rs`; `graphix-fuzz/findings/dyncall-tagblind-print-aug2026/`, `module-state-callee-reactivity-aug2026/`, `dyncall-stale-arg-fired-aug2026/`, `sleep-restart-gate-aug2026/`, `sleep-preserves-caches-jul2026/`
Supersedes: `replay_frames.md` (frames survive as overlays; the `reset_replay` replay-vs-semantic classification survives), `pure_dataflow_plan.md` part B (sparse delivery, withdrawn)

## The model

Every awake node delivers every cycle. There is no "no production".

### The four states

`Tag` is two orthogonal bits — **fired** ("this is an event") ×
**bottom** ("there is no usable value"):

| state | meaning |
|---|---|
| `Fired(v)` | an event carrying a value |
| `Stale(v)` | present, not an event — the value channel |
| `FreshBottom` | the computation produced nothing THIS cycle (1/0 just happened) — an event with no value |
| `StaleBottom` | a standing bottom, nothing new; includes the phantom "never produced" |

Both bits propagate by OR over consumed inputs — one join rule,
`Tag::join`. A fresh bottom is fired+bottom, which is exactly the bare
`TAINT` disc the CLIF emitters mint at div0, `?`-errors and missing
params; a standing bottom is `TAINT|STALE`.

Force points: an output becomes an event iff fired ∧ ¬bottom. The
runtime boundary (`do_cycle` in graphix-rt) is the sole dense→event
filter. `?`/`$` and unchecked arithmetic log on FreshBottom consumption
only, at every depth (no depth-0 exemption: an n-iteration loop over a
fired error logs n times; standing bottoms never log).

### The signature

```rust
fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> &TagValue
```

A borrowed production, no `Option`. Every computing node owns one
production slot — the RESIDENT is the return slot, initialised to the
bottom-tagged phantom (`TagValue::phantom()`; `never()`, never-produced
and pre-first-value async sources are all this one state). A quiet
cycle downgrades the slot's tag to Stale in place (`resident.ride()`);
triggered inputs recompute into it. Delegating nodes (Block → last
child, Bind, Module) forward the child's borrow. `Apply::update` has the
same shape plus `from`. There is no clone cost over the sparse form:
clones occur only at genuine store points (store writes, composite
builds, the FFI staging copy).

### TagView — the one consumption API

```rust
pub enum TagView<'a> { Fired(&'a TagValue), Stale(&'a TagValue), FreshBottom, StaleBottom }
impl TagValue { pub fn view(&self) -> TagView<'_> }
```

Exhaustive matches are the rule, compiler and stdlib alike. The payload
refs are `&TagValue` because the tag rides the disc's upper byte — no
untagged `Value` exists at any address to lend; value access goes
through the masking APIs (`with_value`/`value_cloned`). Bottom variants
carry nothing. The raw bit accessors are the JIT boundary's
(`pub(crate)`, disc packing in `fusion/`).

### The persistent store

`Rt::store()` is `IntMap<BindId, (TagValue, cycle)>`: the production
and cycle stamp of every bound variable's last delivery, maintained at
delivery so a primed read can never observe a value ahead of the
delivery stream. Producers write on genuine production only —
including bottom: the store keeps an at-rest bottom and does NOT retain
the pre-bottom value (`Rt::store_value` answers `None` for a bottomed
bind). A reader interprets `stamp == cycle()` as delivered this cycle
(the entry's tag), an older stamp as Stale (StaleBottom for a bottom
entry), absence as the phantom. `store_insert_standing` stamps an entry
as an earlier cycle — value-channel maintenance that is deliberately not
a delivery (`ByRef`'s seed).

`event.variables` is the INNERMOST OVERLAY, not the store: same-cycle
transient deliveries (select arm binds, call-site formal publishes) at
depth 0, or a framed pass's private writes; `event.frames` holds the
enclosing overlays. Reads fall through the frame stack to the store
(`node::read_var`). Frame writes are private and never reach the store
(the store keeps ENTRY values for the framed seed); notifies are
deferred to escape (`frame_outbox` is the one channel that outlives a
frame drop).

### The three laws

**R1 — recompute.** A node's resident equals its function over its
inputs' current values after every update; the tag plane is the join.
A node may skip the recompute and Stale-downgrade in place when no
consumed input triggered (Fired|FreshBottom), valid only where "Stale ⇒
payload unchanged since the last recompute" holds: frame depth 0, an
awake node. Framed passes recompute unconditionally (tail-jump STALE
chains carry advancing values by ruling — exactly the kernel), and so
does the first update after a sleep (`design/wake_catchup.md`). The
uniform gate is `dense_gate!` (`node/mod.rs`): bottom in ⇒ bottom out
(FreshBottom iff a delivery triggered); otherwise ride unless `trig ∨
bottom-resident ∨ frame_depth > 0 ∨ woke`. `read_prod!`/`node::gather`
are the per-child join accumulators every computing node uses.

**R2 — read-side init.** There are no FIRED backfills. One rule at the
store read: a reader under an init view (`event.init`, or `dispatch_init`
inside a frame) interprets a standing entry as Fired; otherwise as
Stale. A wake (`event.wake_init`) is NOT genuine init — standing entries
read Stale under it.

**R3 — bottom propagates.** Consuming a bottom bottoms the production,
uniformly, including builtin argument seams: a bottomed argument makes
the wrapper produce bottom for the invocation without calling `eval`
(`CachedVals::any_bottom` → `TagValue::bottom_null(triggering)`).
Bottom never reaches builtin authors. There are no seam registers and
no consumer-side rides. The legitimate rides are DESIGNATED semantic
memory owned by the riding node — `Held` (`node/mod.rs`) at exactly
three sites: the select scrutinee (binds and re-matches read the held
value; a bottom scrutinee still bottoms the select — `design/
activation_state.md`), a pattern guard's truth (`is_match` takes
`&self` and cannot consume the guard's production), and `~`'s argument
("sample the latest" is the contract; `~!` is the strict form — a
trigger that finds the RHS bottom produces bottom and banks nothing).
In dataflow terms these are the language's `pre`/`fby`, not ambient
cache behaviour. `Held::reset_replay` clears the value between frames
unless the subtree references no bindings (a closed expression is
identical in every frame and cannot re-produce without an init view).

### Frames and `reset_replay`

A framed pass (a tail-loop iteration, a per-activation dispatch) runs
against a private overlay pushed by `Event::enter_frame`. `Update::
reset_replay` is called between frames and clears REPLAY memory only
("the last value I saw") while preserving SEMANTIC state (`count`'s
tally, `once`'s flag, a select's selection, an accumulated queue). It
is required with no default impl: the replay-vs-semantic classification
is a per-node decision the compiler must force. `Kernel::reset_replay`
is a no-op — a kernel carries no replay caches.

### The north star

"Try to live with a clean dataflow semantics before we pollute it with
hacks to make it look more normal" (Eric, 2026-08-11). The value plane
is classic dataflow — every edge carries a value every cycle, a total
valuation, the spreadsheet law R1 — and the event-driven character is
reduced from an architecture to one annotation, the fired bit, which
survives because firing is observable in the language (effects,
`count`, `~`, select).

## Why dense, and why not remove the tag

The sparse currency — `Option<TagValue>` with consumer operand caches
riding on `None` — had three defects no patch could fix:

1. **Two bottoms.** `None` ("no production") and a tainted production
   are both ⊥ to a reader of the type but had different reactive
   consequences (ride vs poison), and builtin authors met
   `Option<TagValue>` without the context to tell them apart.
2. **A second caching channel.** Each node decided what to cache, and
   the STALE tag added a delivery-side refresh channel on top; the two
   agreed by discipline, not construction.
3. **Tag-blindness.** The tags were advisory: every raw-`Apply` builtin
   read `Some(_)` as "fired" — `once` burned its shot on a stale
   re-surfacing, `count` over-counted, `print` duplicated, `now`
   resampled, `rand` redrew — live bugs invisible to the value-trace
   oracle.

Removing the fired bit instead was tried three times and died each
time for one structural reason: the tail spine's becoming-selected path
must say "emitted, but not an event", which is a second bit by
definition, and deriving firing from dependence cannot replace it — a
parent asking "did anything I depend on fire" overrides a child that
has already decided to be quiet. The bit must exist and must travel
WITH the value, because productions flow through event maps, frames and
the tail-call stash, where a side channel separates from its value in
time. Delivering a non-fired argument as ABSENCE is unsound
independently of whether the call is made (the fire-gate witness):
absence encodes neither quiet nor bottom. So the tag goes everywhere,
honestly.

Sparse delivery was proposed again in 2026-09 (the pure-dataflow plan's
part B, a bool/current currency) and withdrawn: sparse delivery died on
the JIT and the engine did not get simpler.

## Builtin seams

- `CachedVals`/`CachedArgs`/`CachedArgsAsync` (package-core): the
  argument slots are a STAGING BUFFER — the kernel's marshal twin —
  overwritten from every delivery; the wrapper runs `eval` on the
  fired join, retags STALE without `eval` on a stale join, and bottoms
  the invocation without `eval` when any slot is bottom. A builtin
  whose `Effect` is `Stateless` re-runs `eval` from the refreshed slots
  on the first update after a sleep, result STALE.
- Raw `Apply` builtins with designated own state read productions
  through `seam_arg` (update the node, return `(value, fired)`; a
  bottom's value is `None` and never ticks), `seam_tick` (the event
  decision: fired only) and `seam_value` (the value-plane read: fired
  or stale). Every raw production read in stdlib flows through these
  or the staging buffer. Tag-blindness is unwritable under this
  contract.
- The restart builtins (`once`/`take`/`skip`/`uniq`/`hold`/`count`)
  clear their latches in their own `sleep()`. They are `Sync`, never
  `Stateless`, so they never fuse and no kernel needs an interior
  arm-sleep initiator.
- Async builtins clear their output to the phantom on sleep
  (`design/async_sleep_outputs.md`).

## The kernel boundary

- **Input.** Feeder staging packs straight from each production's tag:
  value for a fired param, value+`STALE` for a quiet ride, bare `TAINT`
  for a triggering bottom, `TAINT|STALE` for a standing one — a
  standing bottom must not fire loop or select machinery. Wire slot 0 is
  a context word: bit 0 init, bit 1 quiet frame, bit 2 wake.
- **Output.** Every kernel returns its result's honest TAINT/STALE tag
  in-band on the disc; `Kernel::update` decodes it — Fired/Stale carry
  the value into the resident, bottoms produce the shared
  FreshBottom/StaleBottom. A quiet poll rides the resident (R1).
  Pending is reserved for genuine aborts (the interrupt, the stack
  budget).
- **`tval::value_words`** is the one sanctioned `Value` → `[u64; 2]`
  read, every byte defined: dataless variants → 0, narrow scalars
  widened by `pack_value_to_u64`'s conventions, pointer payloads read
  through `MaybeUninit`. A `transmute` of a `Value` whose payload lane
  is padding types undef as `u64`, which LLVM treats as poison; at
  opt-level 3 with fat LTO a partially-undef branch merge can fold a
  FreshBottom delivery into a clean-FIRED placeholder, invisible in
  debug builds.

## The ruled deltas

Observable consequences ruled intended in advance; the numbers are
what `dense_deltas.rs` cites.

1. Tag-blind builtin fixes: once/count/take/skip/queue/hold/iterq stop
   consuming stale ticks; print/dbg/log stop duplicating; now/rand stop
   resampling; exit stops exiting on stale.
2. HOF stale-laundering fixed: callback subgraphs of the opt HOFs,
   `core::filter`, `array::group`, `net::publish`/`publish_rpc`,
   `http::serve` and `queuefn` see honest tags instead of unconditional
   fired.
3. A non-fired argument is delivered stale, never as absence.
4. Retired: the kernel depth trip it observed is gone (depth is bounded
   by memory, `design/recursive_activations.md`).
5. `array::group`'s protection against a re-surfacing stale group lives
   in the builtin.
6. Log cadence: standing bottoms never log; each fired error logs once
   per consumption.
7. A bind's bottom persists in the store — fresh-reader resurrection of
   a pre-bottom value dies.
8. `~`'s trigger debt is Fired-only, uniform across depths.
9. once/take/skip/count/hold/uniq are `Sync` (exact per-arg firing
   restores the classification).
10. str `split`/`rsplit`/`splitn`/`rsplitn` are `Sync`.
11. Internal pacing changes are invisible at the GXEvent boundary; any
    trace-visible delta beyond this list indicates a mis-gated
    consumer.
12. Guard stale re-deliveries do not force re-matches.
13. Builtins bottom on any bottomed argument (`array::window` over an
    absent value, `max(fired, bottomed)`).
14. In-frame error logging appears (frames used to be silent).
