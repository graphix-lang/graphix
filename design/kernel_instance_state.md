# Per-kernel-instance state (the firing-exactness wire slots)

Status: built 2026-07-03 (per-slot chains 2026-07-15, per-call-site
blocks 2026-07-16, per-activation trees 2026-08); reduced to firing
bookkeeping only by strict fusion 2026-09-01
Pins: `lang/functions.rs` `hof_const_body_prev_len`,
`lang/select.rs` `guarded_select_selection_memory`,
`guarded_select_in_loop_selection_memory`,
`lib_tests/lift.rs` `fused_recursion_sheds_unreached_blocks`,
`findings/firing-jul2026/`, `findings/select-slot-memory-jul2026/`,
`findings/recursive-activation-blocks-aug2026/`

## The problem

A fused kernel is a pure function of its inputs — deliberately. But two
node-walk FIRING rules are not functions of the current cycle's inputs;
they compare against the previous invocation:

1. **HOF resize detection.** `MapQ::update` emits iff `resized ∨ any
   slot fired` (plus an unconditional emit when the source fires while
   EMPTY), and `resized` compares the source length against the
   previous cycle's. A stateless approximation — "source fired ∨ any
   slot body fired" — over-fires on a same-length source event whose
   slot bodies are all quiet (a map with a constant callback body
   re-emits per source fire where the node-walk emits once).
   Suppressing the source-fired term without length memory would
   UNDER-fire instead: a shrink with an unchanged prefix emits in the
   node-walk purely because of the resize — wrong values, worse than
   duplicate fires.
2. **A callee's first-call init view.** The node-walk primes an
   instance's first dispatch with a forced init view (`first_update`),
   so a late first call — a fold callback whose loop had zero elements
   until its source grew — still fires its constants and cached reads
   once. One compiled callee body has no "first call" unless something
   remembers it.

Both need one word of memory that survives across kernel invocations
and belongs to the kernel instance — never a value, never a selection.
Under strict fusion (`strict_fusion.md`) this is the ONLY
cross-invocation memory a kernel has: a select claims no word (a
selection is derived fresh from the present scrutinee every
invocation, organic firing), there is no replay cache, no DynCall site
identity, no arm-lift re-seed. A kernel with `state_words == 0` and no
site layout is bit-for-bit the pure function of its inputs.

## Zero means "no previous"

Every buffer described below is zero-initialized and every consumer
stores `value + 1`, so a fresh buffer reads as "no previous
observation" and init semantics fall out of the zeroing without
touching the init flag:

- *prev-length word*: stores `len + 1`; `resized := word != len + 1`
  (the first arrival counts as resized, which is also what makes
  "source fires while empty" emit). The exact rule is `fires :=
  resized ∨ any_slot_fired`, folded into the loop's STALE bit by
  `SlotFlags` (`emit/scaffold.rs`). A TAINTED source skips the logical
  resize — the node-walk saw no event — and the word is untouched.
- *first-call word*: a cross-kernel call site forces the callee's init
  flag when its word reads 0, records, and never again (the word is
  shared across loop iterations at one site, exactly like the shared
  instance). With no word available the callee sees the plain kernel
  init flag.

## The three identity coordinates

The node-walk gives a HOF node an instance per region instance, per
collection slot (an inner MapQ per outer slot), and per call site
(every CallSite owns its own Apply). A prev-length word must have the
same multiplicity or it aliases: two slots with different lengths
sharing one word would thrash. The storage is one coordinate at a time,
and a new emission context only has to say which coordinates it adds.

### Region instance: wire slot 1

The kernel ABI's leading cycle-context words (`CTX_WIRE_SLOTS`,
`kernel_abi.rs`) carry in slot 1 `state: *mut u64`, a pointer to a
zeroed `Box<[u64]>` owned by the runtime `Kernel` node (null when the
kernel claimed none). A root-body emission site claims the next index
from a counter (`BodyCx::claim_state_word`) and emits loads/stores at
`state + 8*idx`; the final count is `KernelSig.state_words`. Claims are
ordinary emission code — a context slot, not an IR. Only the region
parent's ROOT body may claim: a callee is reached from arbitrarily many
call sites whose claims would alias, and `claim_state_word` answers
`None` there (and inside a scaffold loop, where one static word cannot
hold per-slot memory). A caller that gets `None` MUST emit its
stateless approximation.

### Loop ordinal: per-slot chains behind one word

A nested collection loop inside a scaffold loop is one HOF instance PER
OUTER SLOT in the node-walk, so its prev-length word needs one word per
ordinal. The table lives behind an ordinary claimed word: the enclosing
loop's PREHEADER (which runs at `loop_depth == 0`, where a static claim
is legal) claims one word per nested-loop site in its body and hands it
to `graphix_slot_state_table(word, len, valid, own_levels, leaf)`. The
word owns a boxed `Vec<u64>`, resized to the loop length with PREFIX
RETENTION — shrink truncates, regrow re-creates fresh zeroed slots —
the interpreted MapQ slot lifecycle. A tainted source skips the logical
resize and grows only as an in-bounds guard.

The trick RECURSES for arbitrary nesting: a directory table's entry is
itself an owning word for the next level. A site at depth D gets one
static anchor word (`claim_state_word_loop_invariant`: a directory word
is per-instance, its per-slot content lives in the heap structure), and
each enclosing frame contributes one directory ensure sized by that
frame's `len`, gated by that frame's source taint, indexed by that
frame's current ordinal; the chain ends in a leaf table with one word
per slot. Truncation at any level frees the dropped subtrees
(`free_slot_chain`, shared with `Kernel::drop`); regrow re-creates
fresh — ragged inner lengths for free. The chain is emitted at the
nested preheader, once per enclosing iteration, so ensure calls follow
the loop structure's natural cost. `BodyCx::open_slot_tables` pushes a
`SlotTableFrame { depth, idx_var, tables }` (always, possibly empty);
`slot_select_word(site)` answers `table + i*8` when the site is emitted
at exactly the frame's depth; `close_slot_tables` pops after body
emission. The claimed anchors are recorded on
`WrappedKernel::slot_table_words` so `Kernel::drop` frees them;
`sleep` never touches them (a slot chain is per-position semantic
state and survives pause).

### Call site: wire slot 2

One compiled callee body is shared across call sites where the
node-walk instantiates per site. Slot 2 carries the per-call-site block
pointer, uniform on every kernel signature:

- **The callee declares its layout.** A callee body claims from the
  site channel: prev-length words at its root level take words directly
  (`claim_site_word`), and its loop chains ANCHOR in the block
  (`claim_site_anchor`). The count, anchors and the words rooting
  per-activation trees are the kernel's `SiteLayout`, recorded at
  definition. `to_define` is defined in REVERSE (deepest callees first,
  parent last) so callers read their callees' layouts; a still-missing
  layout IS the recursive back-edge discriminator and the call passes 0.
- **The caller supplies the storage** (`emit_site_block`). At a root
  call site: a contiguous run in the caller's own space — instance
  words in a parent (the callee's anchors translate into the parent's
  `slot_table_words`), site words in a callee (anchors translate into
  ITS layout — the composition recurses through callee-of-callee). At
  an in-loop call site: one block per slot coordinate, the leaf of an
  owning chain over all open frames with `words` stride per slot — a
  plain leaf when the callee has no anchors, else a `SiteLeaf`-described
  block leaf (`graphix_slot_state_blocks`) whose in-block anchors the
  resize helper and `free_slot_chain` walk recursively, so a callee with
  a nested loop called from inside a loop frees exactly.
- **Null-guards everywhere the base can be 0**: the wrapper packs 0 for
  region parents, and recursive back-edges pass 0 (a fresh transient
  activation in the node-walk — for a single-shot activation fresh
  memory ≡ no memory). A consumer whose base is null branches to the
  stateless approximation (`SelWord::Guarded`); a callee loop's chain
  branches around its ensure calls.

A region parent has no kernel caller, so the runtime `Kernel` supplies
its own `site` block when the compiled body claimed site words.

### Activation: per-activation block trees

A self-call cannot use a statically carved block: activations at
different depths are distinct instances with distinct histories, and a
depth-indexed chain would alias them across cycles. Each self-call site
owns a root word in the CALLER's block; `graphix_site_child_block(word,
desc)` allocates the callee's block on first use (sized from the
callee's `KernelSig::site_block_words`, because a self-call's size is its own
body's, unknown while that body is still emitting) and retains it —
one `SelfBlock` per activation, self-similar since only self-calls take
this path (mutual recursion de-fuses at the static call edge). Callee
kernels define in topological order over the recorded call edges; a
callee defined after its caller would run below a recursion with no
interior memory.

**Shrink = delete**, the JIT twin of the interp's activation delete
(`recursive_activations.md` §2): each `Kernel` invocation bumps a
generation and every reached activation block is stamped with it; after
the run, if the reach count fell below the live tree size, the
`state`/`site` `SelfBlock` trees are walked and every subtree not
stamped current is freed, its source word nulled so `Kernel::drop`
never double-frees. The walk is gated on the count (a stable or growing
recursion pays only the counter) and written in safe Rust rather than
emitted CLIF — the reclaim is transparent to the differential, so the
pointer walk lives where ASAN can see it. Both tree walks
(`free_self_block_tree` and the reclaim) are explicit worklists, per
the stack discipline: they recurse one frame per activation otherwise,
and depth is unbounded.

## What this is not

Not a mutable-state channel for user programs (`<-` is the only
cross-cycle mutation, and a connect node-walks), not a second
bottom/taint channel, and not selection memory. The words carry firing
bookkeeping only, invisible to value semantics; `Kernel::reset_replay`
is a no-op because there is nothing to reset. The former slot 3 (a
derivation-changed bit) died with the organic-firing ruling — firing
needs no recursion machinery — and the selection words, DynCall
identity words and arm-lift re-seed died with strict fusion.
