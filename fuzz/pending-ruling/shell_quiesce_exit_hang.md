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
