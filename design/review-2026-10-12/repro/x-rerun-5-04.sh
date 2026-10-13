#!/usr/bin/env bash
# x-rerun-5-04 (variant of tests-lang-a-02): the fixture
# block_issues_together (stdlib/graphix-tests/src/lang/seq.rs:553) races two
# 30 ms after_idle calls against a 50 ms one-shot timer; a 20-50 ms stall of
# the runtime thread (a loaded CI box) lands both in one cycle and flips the
# block case from "done" to "late". This runs the fixture's block program,
# plus an ARMED/RESULT print, once untouched and twice frozen with SIGSTOP
# for 50 ms from ~15 ms after init.
# command: GRAPHIX=/path/to/graphix bash design/review-2026-10-12/repro/x-rerun-5-04.sh
# expected (what the test asserts): RESULT done on every run
# observed (HEAD 7324207f, debug build): done, then late, late
set -u
GRAPHIX=${GRAPHIX:-graphix}
DIR=$(mktemp -d); trap 'rm -rf "$DIR"' EXIT
cat > "$DIR/block.gx" <<'GX'
println("ARMED");
let result = {
  let f = |v| sys::time::after_idle(duration:30.ms, v);
  let a = never<i64>();
  let b = never<i64>();
  seq { { a <- f(1); b <- f(2) } };
  let late = sys::time::timer(duration:50.ms, false);
  let first = once(any(late ~ "late", (a + b) ~ "done"));
  sys::time::after_idle(duration:100.ms, first)
};
println("RESULT [result]");
sys::exit(sys::time::timer(duration:0.6s, false) ~ 0)
GX
run() {
  local prog=$1 delay=$2 stall=$3 pid="" p line
  coproc G { exec timeout -s KILL 60 "$GRAPHIX" --no-cache "$prog" 2>&1; }
  for _ in $(seq 1 300); do
    for p in $(pgrep -f -- "$prog"); do
      [[ "$(basename "$(readlink "/proc/$p/exe" 2>/dev/null)")" == graphix ]] && pid=$p
    done
    [ -n "$pid" ] && break
    sleep 0.02
  done
  while IFS= read -r line <&"${G[0]}"; do
    case "$line" in
      ARMED*)
        if [ "$stall" != 0 ] && [ -n "$pid" ]; then
          sleep "$delay"; kill -STOP "$pid"; sleep "$stall"; kill -CONT "$pid"
        fi ;;
      RESULT*) echo "  $(basename "$prog" .gx) delay=$delay freeze=$stall: $line" ;;
    esac
  done
  wait
}
run "$DIR/block.gx" 0 0
run "$DIR/block.gx" 0.015 0.05
run "$DIR/block.gx" 0.015 0.05
