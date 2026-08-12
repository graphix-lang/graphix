# SEGV: dyncall-site-identity/00_masked_outer_call_cache_ride under check (5b)

Reproduce: `graphix-fuzz check
findings/dyncall-site-identity-jul2026/00_masked_outer_call_cache_ride.gx`
on the dense-delivery branch (0c18f15c) — SIGSEGV in
`str::replace` ← `ReplaceEv::eval` ← `CachedArgs::update` ←
`DynCallSlot::dispatch` ← `graphix_dyncall` (JIT frame): a corrupt
String Value reaches the inner builtin's arg slot through the kernel
dispatch marshal. Standalone runs (both modes) do NOT crash — only the
multi-epoch check oracle. Pre-dense main (same subject, same oracle):
AGREE, no crash — the corruption is dense-branch-introduced (P1..5b),
i.e. the flipped interp's feeder productions interacting with the
still-sparse kernel staging/marshal (the stale-mask value path is the
prime suspect: a stale-masked slot's delivered Value may be an
uninitialized/garbage staging temp that pre-flip absence-masking never
read). The 5b feeder-poll adapter (standing-bottom rides don't
re-fire) did NOT fix it. THE 5C BLOCKER: root-cause before or as part
of the kernel flip; a memory-safety crash cannot ride as an expected
desync.
