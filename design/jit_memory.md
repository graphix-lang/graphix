# JIT memory

Status: eviction and relocation built 2026-10-22 (f-jit-04)
Pins: `graphix-rt/tests/rt/jit_retention.rs`,
`graphix-shell/tests/shell/jit_arena_rotation.rs`

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
module drops, which relocation (below) makes happen while some of its
kernels survive.

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

- a generation retired. A failed install drops the current
  generation mid-link, never installing into it again; on arena
  exhaustion the batch installs in a fresh one, twice the size each
  time a fresh one cannot hold it, up to the 2 GiB colocated-call range
  (`Jit::install`). Any other failure, an OS refusal to commit pages
  included, returns its own error, and the next install reserves a fresh
  generation. The old one's survivors move at the next cycle start
  instead of pinning it.
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

1. If the current generation is mostly dead, retire it; the install
   reserves a fresh one of the configured size.
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

## The rules in brief

- **JIT pipeline and memory** (`fusion/emit/jit.rs`): emission names
  everything by ids of its own (`jit::Names`) and defines nothing.
  Disjoint subtrees fuse in compile tasks (`fusion::fuse_each`: the
  parts of a node that call a function); a task emits into its own
  `Emission` over a frozen copy of its parent's names and caches, and
  its join renumbers its functions after the parent's in join order, a
  lambda kernel the parent already has replacing the task's, so the
  decisions and the output are the serial walk's (`GRAPHIX_FUSE_SERIAL`
  is the A/B). A region that fails forgets the kernel signatures it
  cached with its bodies. Only the context's root emission links:
  every `LINK_BATCH` regions a batch starts compiling on threads of its
  own while emission goes on (`FusionCtx::link_batch`), and installs at
  the next batch or at the pass's link (`FusionCtx::link`), which
  compiles the rest; records are built in emission order and a batch's
  wrappers install with one finalize; a kernel has its entry only after
  its pass links. Emission accepted everything a link compiles, so a
  link that fails (a verifier error included) is a JIT bug and panics.
  A function past `jit::LARGE_FUNCTION` instructions compiles with
  Cranelift's single-pass register allocator: linear compile time,
  worse code, one region still.
  One JITModule + 256MB
  arena per generation (reserved, committed as code installs, on every
  host: `emit/arena.rs`), built on the first fusion (a fusion-off context
  never builds one); an install that fails retires the generation, and
  on exhaustion the link reinstalls its records in a fresh one, twice the
size while a fresh one cannot hold the batch; a
  generation's code is freed when it and every kernel installed into it
  have dropped (each `WrappedKernel`'s `KernelEntry` holds its code). A
  deleted definition's kernels and bodies, and the records no kernel
  reaches, are forgotten at the next link
  (`FusionCtx::link`); only a cached body's ids stay named. A link that
  retired a generation, or left the current one mostly dead (4MB
  committed, under a quarter live), marks a relocation: at the next
  cycle's start (`GX::do_cycle`, `FusionCtx::relocate_kernels`) every
  live kernel outside the current generation is reinstalled there from
  its record and its entry repointed, so the old module drops. No JIT
  code runs outside a cycle's update pass, and a kernel reads its entry
  at each call (`design/jit_memory.md`). Pins:
  `graphix-rt/tests/rt/jit_retention.rs`, `jit_arena_rotation.rs`. Pin:
  `graphix-shell/tests/shell/jit_arena_rotation.rs`. A two-word value never
  crosses the helper seam by value: a pointer in, an out-pointer back,
  through `HelperRefs::call` alone (`design/helper_abi_portability.md`),
  so fusion runs on every host Cranelift supports, Windows included.
  Kernel ABI (`KernelSig::abi_params`): the context words, then a
  (disc, payload) pair per param in source order (formals, then
  captures); recursive types, abstract types, primitive
  unions (a union naming primitive typedefs, `Number`, is one) and the
  primitives with no register form (varints, decimal,
  error) are opaque 2-word values (`design/unified_value_abi.md`).
