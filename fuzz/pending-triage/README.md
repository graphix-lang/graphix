# pending-triage

Findings pulled off the fleet land here, one untracked directory per
campaign (`fleet.sh pull <camp>` → `<camp>/<box>/divergence_NNNNNN.gx`).
The raw witnesses are working material and are NOT committed; what is
committed is this file (the record), `ARCHIVE.md` (the per-class
write-ups of closed rounds), and the pins in `graphix-fuzz/findings/`.

## Protocol

1. `graphix --check` the witness before adjudicating anything — an
   ill-typed program makes two correct engines look like they disagree.
2. Re-`check` on HEAD; a campaign binary can predate every fix in the
   tree. Reduce to the smallest shape that still diverges.
3. Adjudicate against the INTENDED semantics (the design docs), never
   by trusting either engine; a divergence is at least as likely a JIT
   bug as a node-walk bug. A new semantics question is a ruling request
   to Eric, not a fix.
4. Fix, pin in `graphix-fuzz/findings/<class>-<mon><year>/` (red→green:
   the witness must diverge before the fix), run the gates (`cargo test`
   at the root; release `graphix-fuzz regress` + `selfcheck`), record the
   round below, then redeploy the fleet on the fixed tree and delete the
   raw pull.

## Open

Nothing. `fuzz/pending-ruling/` does not exist (no rulings outstanding).

## Follow-ups (not findings)

- Harness: an asymmetric-timeout finding should carry the retry's own
  evidence — CPU burned / window, `/proc/loadavg`, `MemAvailable` — so a
  campaign-start stall (aug29a) is decidable from the artifact.
- Kernel: a resize that truncates slots frees their chain leaves but
  never deletes the orphaned `DynCallSlot` instances (slow growth under
  an oscillating-length source).

## Ledger (closed rounds)

| campaign | pulled | findings | classes | disposition |
|---|---|---|---|---|
| aug18a | 2026-08-20 | 12 | 6 | all fixed same day — framed-formal-seed `c4fa9407`, trip-poison-extent `6df2ec60`, connect-instance-identity `e05a6c8b`, the shrink-to-zero rule (`slot-shrink-truncate-aug2026`), the bottom-out ruling (`bottom-out-aug2026`), constrain-order (`constrain-order-diag-aug2026`) |
| aug20a | 2026-08-22 | 5 | 1 | the quiet-frame init view: `DynCallSlot::sleep` keeps `fired`, wire slot 0 gains the QUIET bit (`quiet-frame-init-view-aug2026/00–07`) |
| aug22c | 2026-08-24 | 11 | 5 | `nested-bind-stmt-dead-elim`, `labeled-callback-param`, `dyncall-value-return-stale` (+2 faces), the free-union-member typing rule (`lang/select.rs`) |
| aug24a | 2026-08-24 | 4 | 2 | `typedef-cell-mode-parity` (`Env::seed_typedef_refs` in both modes, `check_mode_parity`); one class-D re-find on a stale binary |
| aug25a flood | 2026-08-26 | 82 | 2 | `set-eq-drops-cell-link` (81 of 82), `arith-widened-cell` `64fbdaf3` |
| aug24b + aug25a residue | 2026-08-27 | 3 | 3 | `hold-async-clock` (oracle exclusion, not a bug), `bound-cell-cycle-accepts` (`contains`), `init-over-limit/02` |
| aug27a | 2026-08-28 | 6 | 4 | tail-rebind-by-name `9e3bae1a` (×2), `82e4fbfa`, `d4f046d8`, `bfda0913`; one async-artifact non-bug |
| aug28a | 2026-08-28 | 1 | 1 | in-loop DynCall site identity freed per evaluation frame — `quiet-frame-init-view-aug2026/08`, `e96206de` (2026-08-30); the shape then FUSES via identity-keyed instantiation `3bd9a9a9` |
| aug28b | 2026-08-29 | 5 | 3 | tail-select bottom-out (superseded by the ride deletion `6991e2ad`), `skipped-fn-arg-effect` `7c1e7e14`, `framed-arg-stale-formal` `15d386a6` |
| aug29a | 2026-08-30 | 1 | 0 | a campaign-start box stall on ryouko (AGREE 5/5 on the same box and binary under load) — not a bug |
| aug30a | 2026-08-30 | 0 | — | 2.5 h on the recursion-shrink tree |
| aug30b | 2026-08-30 | 0 | — | ~3 h on the site-identity fix |
| aug31c | 2026-08-31 | 1 | 1 | fold firing is per-slot ∨ the acc carry (`fold-midchain-fired-aug2026`) |
| aug31d | 2026-08-31 | 0 | — | ~6 h on the aug31d tree (fold per-slot fix + or-patterns P1/P2) |
| aug31e | 2026-08-31 | 1 | 1 | the ⊥-typed connect target (`bottom-connect-target-aug2026`): `type`/`trait`/`impl` join `use`/static `mod` as statement-position-only (82e4fbfa's family), and contains' (Bottom, TVar) arm derefs a bound cell instead of answering true (it also clobbered the binding under InitTVars). washu-chan's aug31c pull was a stale copy of the already-fixed fold finding |
| aug31e (late pull, at the aug31f deploy) | 2026-09-01 | 2 | 0 | both AGREE at HEAD: ryouko's was the already-triaged ⊥-connect witness; hz0's (an interp missed-emission on a fold-HOF + qop + const-scrutinee-select shape) is the ride family wake catch-up closes — adopted as `wake-catchup-sep2026/07` to hold the fix |
| aug31f round 1 (ryouko, ~1h in) | 2026-09-01 | 2 | 1 | 00: asymmetric interp timeout on 500k tail recursion — AGREEs on an idle box (the loaded-ryouko class, see Follow-ups). 01: THE DEFAULT-ARG BIRTH CLASS (`default-arg-birth-sep2026`) — a labeled default reaches a fresh callee only as a standing read, and 9b2e7231 stopped upgrading those under an arm's wake view, so an instance born at a becoming-selected dispatch never configured its fired-gated seam (str::escape emitted nothing, forever); BOTH engines broke together at those commits (metamorphic blind spot) and the interp resurfaced alone after wake catch-up. Three fixes: interp bound-dispatch seeds defaults FIRED; DynCallSlot first dispatch is a BIRTH view (wake_init cleared, marshalled tags untouched); escape's config memo survives sleep. Worktree bisection (e40af2be ✓ → a4f69e8e ✗ both engines) |
| sep01a | 2026-09-01 | 0 | — | ~3h on the locality + birth-rule tree (ee64db56/cbd2df98), all six boxes, permissive-default binaries; closed clean and superseded same day by the strict-fusion flip (fa08136a) — sep01b is the strict-default soak, five remotes |
