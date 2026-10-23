# JIT memory

Status: eviction and relocation built 2026-10-22 (f-jit-04)
Pins: `graphix-rt/tests/jit_retention.rs`,
`graphix-shell/tests/jit_arena_rotation.rs`

A runtime's JIT holds what it compiled for as long as something can
still run it or ask for it again. This document says what that is, when
each part is let go, and how a generation's code space comes back.

## What the JIT holds

- **Generations.** One cranelift `JITModule` and one arena
  (`emit/arena.rs`: reserved, committed a page run per allocation). A
  record tree installs into the current generation. Its code lives as
  long as the `CodeOwner` lives, and every kernel entry installed there
  holds it.
- **Records.** A compiled function's bytes, relocations by symbolic
  target and constants by recipe (`emit/record.rs`). A record is
  self-contained: it can be installed into any generation.
- **Emission caches.** The lambda kernels' signatures by
  `KernelCacheKey`, their bodies by `KernelKey` (one build per kernel,
  whichever regions call it), and `Names` (each id's signature).

## Eviction (built)

A deleted definition queues its `LambdaId` (`FusionCtx::forget_def`).
After each full link, `FusionCtx::link` forgets the following:

- the definition's kernels and their cached bodies. A live body that
  calls one keeps the callee's code through its own record tree;
- the evicted bodies' records;
- `Names`, down to the cached bodies' ids.

A region's parent record lives only through its batch. Each link also
sweeps from the generation's `loaded` map every record that nothing
else holds. `FusionStats::retained` counts what is left.

The memory eviction cannot reach is the generation's own:

- each link burns at least one arena page, because a finalized page is
  never written again;
- cranelift keeps a declaration and bookkeeping for every function the
  module ever defined.

That is about 6 KB per compile-and-drop. It comes back only when the
module drops. Today a module drops when its arena is exhausted and every
kernel installed in it has dropped, so one surviving kernel pins it for
good.

## Relocation

A kernel's entry becomes movable. A generation that is mostly dead
retires, and its surviving kernels are reinstalled into the current
generation from their records. Then nothing holds the old `CodeOwner`,
and the whole module drops: code, arena and cranelift's bookkeeping.

### When it is safe

A relocation runs at the start of a cycle, before any node updates
(`GX::do_cycle` calls `FusionCtx::relocate_kernels` first). Nothing runs then:

- kernels run only inside a cycle's update pass;
- the evaluation pool's parts of a cycle join before the cycle ends;
- a compile from the handle is processed between cycles, and a
  compile inside a cycle (a runtime bind) only marks.

Every code address a kernel takes at run time is transient. A chunk's
address passes to the loop driver and a spill thunk's to
`graphix_grow_stack`, both within one run. The entry is read again at
every invocation (`FusedKernel`'s update). State buffers, site blocks
and slot tables are data and are untouched.

Invariant: no JIT code runs outside a cycle's update pass. A path that
broke it would make relocation unsound. One such path would be a value
hook run from outside a cycle.

### The entry

`KernelEntry` (`emit/jit.rs`) holds the wrapper's code pointer (an
`AtomicPtr`, null until the pass links), the `CodeOwner` of the
generation it lives in, and the wrapper's record, which is what a
relocation installs.

- A `WrappedKernel` holds a `std::sync::Arc<KernelEntry>`, which is
  shared by its clones (collection slots take the prototype's).
- The hot path is one `Acquire` load of the pointer.
- The `Jit` keeps a `std::sync::Weak` to every entry it set, pruned of
  dead ones at each link. That is how a relocation finds the
  survivors.

### The mark

A relocation is marked when either holds:

- a generation retired. Arena exhaustion retires the current
  generation mid-link and installs the batch in a fresh one
  (`Jit::try_install`); the old one's survivors move at the next cycle
  start instead of pinning it.
- the current generation is mostly dead: its committed arena is at
  least 4MB and its live code (the code bytes of its `loaded` records,
  kept up to date at load and sweep) is under a quarter of that. This
  is checked after each link's sweep (`Jit::forget`).

The quarter keeps the cost amortized. A relocation copies the live
bytes, and at most a quarter of what was committed is live, so every
committed byte pays for a bounded copy. The 4MB floor keeps a small
program from ever relocating.

### The move

`Jit::relocate`, when the mark is set:

1. If the current generation is mostly dead, retire it and start a
   fresh one of the configured size.
2. Collect every live entry whose `code` is not the current
   generation's.
3. Install their wrappers' record trees into the current generation in
   `LINK_BATCH` batches through `Jit::install`, which already grows a
   fresh arena on exhaustion. `loaded` deduplicates callees that two
   wrappers share.
4. For each entry, store the new `fn_ptr` (`Release`) and replace
   `code`. The old `CodeOwner` drops with its last entry, and the
   module is freed (`free_memory`).

If an install fails, its entries keep their old code, which their
`code` still holds. A warning is logged and nothing panics: the old
code stays valid.

`FusionStats` counts relocations and the kernels moved, and snapshots
the committed arena. `GRAPHIX_JIT_RELOCATE=every` retires the current
generation and moves every live kernel at every cycle start, for
stress runs.

### Tests

- `jit_retention.rs` (`live_kernels_leave_dead_generations`) keeps a
  fused program answering through 2500 compiles of others. It asserts
  that relocations happened, that the committed arena peaks under 8MB
  (about 10MB unrelocated), and that the program's value is right
  throughout.
- `jit_arena_rotation.rs`: after an exhaustion, the old generation's
  survivors move at the next cycle and the total is still right; and a
  multi-cycle program, a loop's slot state included, answers the same
  with every kernel moved every cycle.
- `GRAPHIX_JIT_RELOCATE=every` is a stress mode for the whole `run!`
  suite and for a soak lane: every kernel moves every cycle, so any
  state or address that depends on where the code lives diverges.
