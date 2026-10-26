#!/bin/bash
# Title: a subject's sys::exit kills the fuzz harness process once the run's
#        ExitRequest receiver is dropped (abandoned construction, fusecheck's
#        `let Started { ctx, .. }`), with the subject's own exit status.
# Command: bash design/review-2026-10-26/repro/fuzz-lib-b-exit-kills-harness.sh
# Expected: check-one writes a verdict file ("agree", or a divergence) and exits 0;
#           a subject never decides the harness's exit status.
# Observed: with a 1 ms per-run budget the first construction is abandoned at
#           the deadline, finishes on its own task, runs `sys::exit(3)`,
#           ExitRequest::request sees the receiver gone and calls
#           std::process::exit(3): "check-one exit=3", no verdict file. The
#           campaign parent then reads "exit status: 3" as a CRASH (died()).
#           With a 3000 ms budget the same program agrees and exits 0.
set -u
W=$(mktemp -d)
printf 'sys::exit(3);\n42\n' > "$W/exit3.gx"
F=${GRAPHIX_FUZZ:-/home/eric/tmp/review-2026-10-26/bin/graphix-fuzz}
timeout -s KILL 170 "$F" check-one "$W/v1" 1 < "$W/exit3.gx"
echo "check-one exit=$?"
cat "$W/v1" 2>/dev/null || echo "(no verdict file)"
timeout -s KILL 170 "$F" check-one "$W/v2" 3000 < "$W/exit3.gx"
echo "check-one (3000 ms) exit=$?"; cat "$W/v2"
rm -rf "$W"
