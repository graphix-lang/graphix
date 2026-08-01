# TVar UAF crash — jul31baieka generate lane (2026-08-01)

One-shot SIGSEGV, unreproduced in 810 retries. Parked with artifacts;
an ASAN lane hunts it in subsequent rounds.

## Facts

- Child SIGSEGV (SEGV_MAPERR, core captured) on aieka, campaign
  jul31baieka (binary a412a0b3), generate lane, ~5h into the round.
- Crash site: `triomphe::Arc<typ::tvar::TVarInner>::drop_slow`,
  faulting instruction `testb $0x1,(%rdi)` with **rdi = 0x5** — the
  TVarInner's interior (its ArcStr name's pointer word) was garbage
  when the refcount hit zero. The allocation was scribbled or
  reused-while-referenced; the drop is just where it surfaced.
- Drop context: a Bind/Block/Bind/Block/Select node tree being
  dropped inside `GX::process_input_batch` under `do_cycle`
  (block_in_place) — the dynamic-module recompile path drops old
  compiled bodies exactly there, and the subject program carries FIVE
  `mod dN dynamic` modules plus abstract types and heavy churn.
- Thread dump: fusion::intern::gc_loop existed but was asleep in
  nanosleep; the interner is safe code (ArcStr clones, refcount-gated
  retain) — unlikely culprit. All other threads parked.
- Bisect hammer: 400 runs on a412a0b3 + 400 on 73de0cab (pre-lpool,
  pre-sleep-fix, pre-mu-fix), 8-way parallel on the crashing box:
  ZERO crashes on both. No evidence implicating the 2026-07-31
  commits; the race needs campaign-level load/timing.

## Suspects (unproven)

The Rust-side interner and pools are safe code; the raw-memory
writers are the JIT's: per-instance state words, slot-table chains
(SiteAnchor/SiteLeaf recursive free via Kernel::drop /
WrappedKernel::slot_table_words), and the DynCall marshal buffer
(`*mut LPooled<Vec<Value>>` through graphix_dyncall). A stale kernel
writing through freed state after a dynamic-module recompile/eviction
would corrupt whatever reallocated there. Note ASAN cannot see JIT
writes (uninstrumented code) — it covers only the Rust-side
hypothesis class (double-drop, pool misuse, drop-after-free).

## Artifacts

- aieka:~/tmp/crash-jul31b/{core,graphix-fuzz} — 1.2G core + the
  exact campaign binary (gdb-able in place).
- This dir: the subject program (multi-module file-v1 format).
- ASAN binary: ryouko ~/tmp/target-asan (nightly, -Zbuild-std).

## Next steps when it re-fires

1. gdb the new core: confirm same shape (TVarInner drop, garbage
   interior). Diff the drop context.
2. If ASAN catches it: the report's alloc/free stacks are the answer.
3. If it stays JIT-suspect: GRAPHIX_DBG_KERNELS + audit the state-word
   free path against dynamic-module kernel eviction ordering.
