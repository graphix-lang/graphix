# pending-triage

## The aug18a arc (2026-08-20) — 3 fixed, 3 open

The fleet round on campaign aug18a (hz0/aieka/katana/ryouko, campaign
binary 1b1778b3, the organic-firing P4 soak): 12 divergences, 6
classes. Fixed and committed same day:

1. **framed-formal-seed** (c4fa9407) — a framed dispatch seeds its
   quiet args every pass; the fold-in-rec-arm that never published.
2. **trip-poison-extent** (6df2ec60) — one shared depth-trip poison
   bit with pop-to-zero extent; the kernel rode across trips (and
   refused legal rides past the root).
3. **connect-instance-identity** (e05a6c8b) — connect-target liveness
   in dead-elim, per-instance minted lifted ids, loop/rec/arm lift
   gates; the write-only-let spinner family.

Open, each parked here with mechanism located and a design question:

4. **class4_slot_state_survives_empty.gx** — kernel per-slot state
   survives a shrink-to-zero resize (the loop-body-emitted chain
   ensures never run on an empty generation). Fix = prewalk-based
   preheader ensures.
5. **class5_tail_entry_ride.gx** — RULED 2026-08-20 and INVERTED
   (design/activation_state.md): the kernel's `[1,1,1]` is the ruled
   trace — held state never determines output bottomness (bottom in,
   bottom out), and state multiplicity = activation multiplicity
   (tail loop = ONE activation). Fix = interp amendment, not kernel
   storage; probe twins class5_probe_{native,tail}_depth1.gx are the
   red fixtures. Not yet built.
6. **class6_compile_err_detail.gx** — mode-divergent tvar-bound
   rendering in a typecheck error (a fusion-phase stdlib-compile walk
   binds a shared cell). Diagnostic-only.
