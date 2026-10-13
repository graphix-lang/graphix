#!/usr/bin/env bash
# x-rerun-8-13: a task the runtime aborts on purpose (after_idle's restart,
# a Latest call superseded, an arm's sleep) is logged at ERROR as
# "a runtime task ended without replying: task N was cancelled"
# (graphix-rt/src/gx.rs:242 `joined` logs every JoinError, a cancellation
# included). An ordinary after_idle program logs one per re-arm.
# command: GRAPHIX=/path/to/graphix bash x-rerun-8-13.sh
# expected: nothing at ERROR (a cancellation is the runtime's own doing)
# observed: one ERROR line per re-arm, interleaved with the program's output
set -u
G=${GRAPHIX:-graphix}
d=$(mktemp -d); trap 'rm -rf "$d"' EXIT
cat > "$d/p.gx" <<'GX'
let clk = sys::time::timer(duration:10.ms, 10);
let n = 0;
n <- clk ~ n + 1;
sys::exit(sys::time::after_idle(duration:150.ms, n ~ 0));
n * 2
GX
mkdir "$d/log"
RUST_LOG=warn timeout -s KILL 60 "$G" --no-cache --log-dir "$d/log" "$d/p.gx" 2>&1 | grep -c 'ended without replying'
