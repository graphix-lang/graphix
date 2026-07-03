# Per-kernel-instance state (the firing-exactness wire slot)

Status: PROPOSED (2026-07-03). Motivated by the two firing residuals the
trace oracle confirmed at fuzzer-v2 phase-2.3 calibration
(`fuzz/triage-fuzzer-v2/firing_000005`, `firing_000007`). Not yet built.

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

Both need one word of memory that survives across kernel invocations
but belongs to the kernel INSTANCE — a per-slot `clone_rebind` clone
must get fresh memory, exactly as each node-walk slot owns its own
`MapQ`/select state.

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
`state_words > 0` and passes the pointer on every call. `clone_rebind`
allocates a fresh zeroed buffer per clone — per-slot instances get
per-slot memory for free, matching the node-walk's per-slot async
state. The template's buffer is never shared.

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
  node owns the buffer; `clone_rebind` allocates fresh.
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
