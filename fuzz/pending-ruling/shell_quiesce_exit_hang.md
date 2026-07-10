# Shell quiesce-exit hang + release SEGV (open, needs fresh-context bisect)

## Symptoms (2026-07-10 ~02:00)

- On the CURRENT sync-subset-proto debug build, the graphix SHELL
  prints the correct value and never exits — for EVERY value-position
  program tried, including `{let x = 1; x + 1}` (cr6). GXDBG_P4_OFF=1
  (instance-fusion override disabled) does NOT fix it.
- jul10a fuzz finding crash_000001: the RELEASE build SEGVs (signal
  11, harness exit 139) on a str::split_escaped + map + nested local
  lambdas + tuple-arg program. The debug build hangs (value correct)
  instead. Not yet clear whether the SEGV and the hang share a root.
- The fuzz harness itself quiesces fine (campaigns run at full
  throughput; run! fixtures pass 2280/0) — the failure is specific to
  the SHELL's exit path (or an environmental stdin/tty interaction).

## Conflicting observations (why a careful bisect is needed)

- cr1 (the crash-shape program) HUNG at merge-base 39174bb2 with a
  freshly-built binary — evidence of a PRE-EXISTING hang.
- Yesterday (P1-era build) `h_string_fold.gx` and friends exited 0
  via the same shell + Bash-tool environment; tonight fcl.gx exited 0
  at ~23:47 on the bind-suppression build. Both contradict
  "pre-existing for all programs".
- The trivial cr6 was never tested at base with a valid binary (one
  attempt was invalidated by a blocked checkout).

## Next session

1. Bisect cr6 (25s test) across: 39174bb2, e0f601be, 5634fbdc,
   d20bc153, 3a1caa40, 8630436f, 807d5be2, HEAD. Tree is fully
   committed — clean checkouts, NO stash (a no-op stash + pop grabbed
   a foreign stash tonight and had to be surgically restored).
2. If cr6 exits at base: bisect forward to the breaking commit.
   If it hangs at base: investigate the shell's exit condition
   (stdin/tty detection?) and why yesterday's runs exited.
3. Separately reproduce crash_000001's SEGV under gdb on the release
   binary (core was dumped) — a SEGV is never acceptable regardless of
   the hang question.
4. GRAPHIX_DBG_VARS on cr6 shows normal NOTIFY_SET traffic then
   silence — no obviously-leaked wake interest in the tail; diff the
   full trace against a build that exits.

## Second SEGV instance (jul10b fuzz crash_000000, override-off build)

`array::map(["a", str::len, "ccc"], |s| str::len({let inner = …;
let middle = |x| inner(x) + inner(x + 1); let outer = |x| middle(x)
- middle(x - 1); outer(…)}))` — a first-class BUILTIN value in the
array literal plus deeply-nested local lambda calls. exit=139
reproducible. Confirms the SEGV class does not depend on the
instance-fusion override (default-off in this build) — consistent
with the pre-existing classification from the cr1 baseline test.
Common factor across both crashes: NESTED LOCAL LAMBDA CALL CHAINS
inside an array::map callback (per-slot machinery + local-lambda
resolution). gdb the core in the morning.

## Open jul10b divergence (morning triage)

generate/divergence_000000: PACING — interp fires (0, 3600) AND
(1, 3600) in one epoch, jit fires only (0, 3600). Guarded select over
i64:1 with array::find in an untaken arm. Deterministic. Suspects:
select arm-wake/guard replay classes (the jul08l/n family) or a new
rigid-era interaction. Not yet classified.

## Third open jul10b item (morning triage)

fuzz/divergence_000001: deterministic JIT no-emit. Shape: fold +
`let rec lp = |n, a| select n { 0 => a, _ => (5, 6) }` — a "rec"
lambda whose non-base arm returns a TUPLE (no recursion despite let
rec), so the return type is the union [i64, (i64, i64)]; body then
does `lp(500, 0) * 0` (tuple arith → bottom in interp) + try/catch.
interp yields an Array; jit emits nothing. Suspect: recursive-callee
kernel freeze over a mixed scalar/composite union return, or the
tuple-arith bottom path diverging at the region gate. Same program
FAMILY as jul09c 000003/jul10a 000003 mutants (generator lineage).
