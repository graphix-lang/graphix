# Per-kernel-instance state (the firing-exactness wire slot)

Status: BUILT (2026-07-03, same day as proposed). Motivated by the two
firing residuals the trace oracle confirmed at fuzzer-v2 phase-2.3
calibration; both are fixed and promoted to `findings/firing-jul2026/`
(the trace-strength regress gate), with `run!` fixtures
(`guarded_select_selection_memory`, `hof_const_body_prev_len`) pinning
the count-observable forms. Two v1 scope notes vs the proposal below:
claims are ROOT-BODY only (`BodyEmitter::allow_state` — a callee is
reached from many call sites whose claims would alias; the per-callsite
sub-buffer composition described below remains future work), and the
select rule was corrected against the observed node-walk: the scrutinee
term STAYS (a scrutinee fire re-emits even with a const arm — verified
2026-07-03); selection memory refines only the GUARD term.

**Extended 2026-07-15 with PER-SLOT state tables** (soak-jul14b fuzz
divergence 000009; Eric's ruling: guarded selects can't de-fuse —
"they're the only control flow in the language" — and the firing rule
is "an arm fires once when it becomes selected, and if its body
dependencies naturally cause it to fire"; his design sketch: MapQ
keeps a slot per element, so the kernel needs "a similar structure to
store the selected arm"). See the section at the end.

## The problem

A fused kernel is a pure function of its inputs — deliberately. But two
node-walk firing rules are NOT functions of the current cycle's inputs;
they compare against the previous invocation:

1. **HOF resize detection** (`firing_000007`). `MapQ::update` emits iff
   `resized ∨ any slot pred emitted` (plus an unconditional emit when
   the source fires while EMPTY). `resized` compares the source array's
   length against the previous cycle's. The kernel's stateless
   approximation — "source fired ∨ any slot body fired" — over-fires on
   a same-length source event whose slot bodies are all quiet (a map
   with a CONST callback body re-emits per source fire where the
   node-walk emits once). Suppressing the source-fired term without
   length memory would UNDER-fire instead: a shrink with an unchanged
   prefix emits in the node-walk purely because of the resize — wrong
   values, worse than duplicate fires.

2. **Guarded-select selection memory** (`firing_000005`). The node-walk
   emits on a guard-only event only when the SELECTION (which arm is
   taken) actually changes. The kernel's guard-feeder STALE fold fires
   whenever a guard feeder fires — one duplicate emission per
   guard-only cycle with an unchanged selection, and the init
   off-by-one the finding pins (interp 4 / jit 5).

Both need one word of memory that survives across kernel invocations but
belongs to the kernel instance. Each fused region gets fresh memory, just as
each node instance owns its own state.

## The design

**One new leading wire slot.** The kernel ABI's cycle-context prefix
(`kernel_abi::INIT_WIRE_SLOTS`, currently 1: the `event.init` flag)
grows to 2: slot 1 carries `state: *mut u64` — a pointer to a small
zero-initialized buffer owned by the caller, or null when the kernel
claimed no state. Every kernel carries the slot (uniform ABI, same as
the init flag); the pack sites and the wrapper→body forwarding already
have a single source of truth to extend.

**Claiming words.** During emission, a site that needs memory claims
the next index from a counter on the lower ctx and emits loads/stores
at `state + 8*idx`. The final count lands in `KernelSig.state_words`.
Claim sites are ordinary emission code — no new registry, no per-op
vocabulary (this is a CONTEXT slot, not an IR).

**Ownership.** The spliced node (the kernel-callable's invocation site,
where the slots are packed) allocates `Box<[u64]>` zeroed when
`state_words > 0` and passes the pointer on every call. Every runtime Kernel
owns its buffer; the immutable compiled artifact remains shared.

**Zero means "no previous".** Every consumer stores `value + 1` so the
zeroed initial state reads as "no previous observation", and init
semantics fall out without touching the init flag:

- *HOF prev-length word*: stores `len + 1`. `resized := word != len+1`
  (so the first arrival — word 0 — counts as resized, which is also
  what makes "source fires while EMPTY" emit). Exact firing rule:
  `emit := resized ∨ any_slot_fired ∨ (source_fired ∧ len == 0)`.
  The word updates on every kernel run that evaluates the HOF,
  regardless of downstream taint — mirroring `MapQ`'s internal state,
  which advances whether or not anything consumes the result.
- *Select prev-arm word*: stores `arm_index + 1`. On a guard-only fire
  (no scrutinee/arm-input fire), `emit := arm_index+1 != word`. The
  first selection (word 0) always emits.

**What this is NOT.** Not a general mutable-state channel for user
programs (`<-` remains the only cross-cycle mutation, via the lift),
and not a second bottom/taint channel — the state words carry firing
bookkeeping only, invisible to value semantics. A kernel with
`state_words == 0` is bit-for-bit the pure function it is today.

## Ripple points

- `fusion/kernel_abi.rs`: `INIT_WIRE_SLOTS` 1→2 (rename or document
  slot 1); `KernelSig.state_words`.
- `fusion/kernel.rs`: pack site pushes the state pointer; the spliced
  node owns the buffer.
- `fusion/emit.rs`: wrapper→body forwarding of the new slot (the same
  loops that forward the init flag); a `claim_state_word(cx) -> idx`
  helper; the guarded-select consumer.
- `fusion/scaffold.rs` + `stdlib/graphix-package-array`: `SlotFlags::
  apply` grows the exact rule (needs the source len and fired bit —
  both already in hand at the apply site).
- Findings `firing_000005`/`firing_000007` flip to FIXED and promote
  into `graphix-fuzz/findings/` (the trace-strength regress gate).
- `run!` fixtures: const-body map over a reactive source (emit count),
  shrink-with-unchanged-prefix (emit REQUIRED), guard-only unchanged
  selection (quiet), guard-only changed selection (emits).

## Testing

The trace oracle is the referee: both findings' raw shapes (no `count`
instrumentation) must flip from ExtraFire divergence to agreement, the
graphix-tests differential suite must stay green (the shrink under-fire
hazard is a VALUE bug the `run!` fixtures would catch), and a
reactive-generation soak (fuzzer-v2 phase 3) exercises the mechanism
broadly once injection schedules land.

## Per-slot state tables (2026-07-15)

A guarded select inside a scaffold loop is one select PER SLOT in the
node-walk (each MapQ/FoldQ slot's subgraph owns a Select instance with
its own selection memory), so the static one-word claim is refused
there and the guard term fell back to the unrefined feeder fold — one
duplicate emission per guard-only cycle with an unchanged selection
(soak-jul14b divergence 000009). The fix gives loop-body selects one
word per slot without any ABI change:

**The table lives BEHIND an ordinary claimed state word.** The loop
emitters' PREHEADER runs at `loop_depth == 0`, where `claim_state_word`
is legal — so the loop claims one static word per guarded-select site
in its body and hands it to a new runtime helper,
`graphix_slot_state_table(word, len, valid) -> *mut u64`. The word owns
a boxed `Vec<u64>` (lazily created on first call); the helper resizes
it to the loop length with PREFIX RETENTION — shrink truncates, regrow
re-creates fresh zeroed slots — exactly the interpreted MapQ slot
lifecycle, so retained slots keep their recorded selection and fresh
slots read 0 ("no previous"). A TAINTED source skips the logical
resize (the node-walk saw no event), growing only as an in-bounds
guard — mirroring `SlotFlags::apply`'s prev-len word rule.

**Wiring.** `guarded_select_sites(body)` (emit.rs) prewalks the loop's
body tree for guarded-select `ExprId`s — the walk sees exactly the
tree the loop emits inline (a nested collection HOF's callback body is
behind its own lambda def, unreachable). Each collection op passes the
sites into its scaffold emitter; the emitter's preheader calls
`BodyCx::open_slot_tables` (claim + helper call per site, one
`SlotTableFrame { depth, idx_var, tables }` pushed always — empty when
claims are refused) and `close_slot_tables` pops after body emission.
`emit_select_node`, on a refused static claim, consults the top frame
via `BodyCx::slot_select_word(spec.id)`: an `ExprId` match AND
`loop_depth == frame.depth` yields the address `table + i*8`, which
the arm consumer (`emit_select_value_arm`) uses for the same
compare-and-record it does at root level — including the selection-
changed ARM INIT VIEW, so consts/seeds in a newly-selected arm
re-deliver per slot.

**Ownership.** The claimed words are recorded on
`WrappedKernel::slot_table_words` (threaded like
`replay_state_words`); the runtime `Kernel`'s `Drop` frees the boxed
Vecs. Semantic state: `sleep`/`reset_replay` never touch them (same
choice as the static select word).

**Arbitrary nesting depth (same day — Eric's review: "each loop that
has a select in it needs its own set of slots").** The word-owns-a-Vec
trick RECURSES: a directory table's entry is itself an owning word for
the next level. A select at depth D gets one static ANCHOR word (a
directory word is per-INSTANCE — its per-slot content lives in the
heap structure — so `claim_state_word_loop_invariant`'s in-loop
exemption applies), and its loop's preheader emits the chain: each
enclosing frame contributes one directory ensure (sized by that
frame's `len`, resize gated by that frame's source taint, indexed by
that frame's current ordinal — the frame stack carries `len`/
`src_disc`/`idx_var`), ending in the leaf table of selection words.
`graphix_slot_state_table` takes `own_levels` (0 = leaf; k = entries
own k−1-level subtrees): truncation at any level frees the dropped
subtrees (`free_slot_chain`, shared with `Kernel::drop`, registration
is `(word, own_levels)`), regrow re-creates fresh — the MapQ
prefix-retention lifecycle applied per level, ragged inner lengths
for free. The chain is emitted at the nested preheader, which runs
once per enclosing iteration — ensure calls follow the loop
structure's natural cost. The node-walk twin is the interpreted slot
TREE (outer slot i owns an inner MapQ instance which owns per-slot
selects) with u64s in place of subgraphs.

**Remaining residual: CALLEE bodies only** (`state_enabled == false` —
a guarded select in a cross-kernel callee keeps the unrefined guard
term: duplicate fire, never a wrong value). Fixing it is the
per-callsite sub-buffer composition already noted above: the CALLER
claims the anchor in its own space (static word, or an entry in its
loop's slot chain) and passes the address through the cross-kernel
ABI — an ABI touch the loop fix avoided, and the reason it's a
separate change.

**The arm-lift consumer stays static-only:** a lifted connect target's
identity is per INSTANCE (state-word BindIds), so a per-slot word
can't reproduce the re-seed; `has_arm_lift` in a loop still `Err`s the
kernel (de-fuse), unchanged.

Pinned by `run!` fixtures `guarded_select_in_loop_selection_memory`
(guard-only unchanged selection is quiet), `guarded_select_per_slot_
independence` (two slots with DIFFERENT stable selections stay quiet —
a shared word would thrash), `guarded_select_slot_table_resize`
(prefix retention + fresh-slot first-selection fire), the nested
quartet `guarded_select_nested_loop_selection_memory` /
`_nested_per_pair_independence` (per-(i,j) memory — flat sharing
would thrash) / `_nested_ragged_resize` (directory grow + ragged
inner lens) / `_triple_nested` (depth 3), and the promoted findings
`findings/select-slot-memory-jul2026/` (02 is the nested shape).
