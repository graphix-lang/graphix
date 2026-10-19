#!/usr/bin/env bash
# x-rerun-6 (x-rerun-5-06's fix widened the margin; the race remains): the
# fixture block_issues_together (stdlib/graphix-tests/src/lang/seq.rs) still
# orders "done" against "late" by wall clock, now 200 ms after_idle calls
# against a 300 ms timer. This runs the fixture's block case at the new
# timings, once untouched and twice with the runtime frozen (SIGSTOP) for
# 200 ms from 150 ms after init, as a loaded debug test run can stall it.
# command: GRAPHIX=/path/to/graphix bash x-rerun-6-block-issues-together-stall.sh
# expected (what the test asserts): RESULT done on every run
# observed (HEAD 9445a1d21, debug build): done, then late, late
GRAPHIX=${GRAPHIX:-graphix}
DIR=$(mktemp -d); trap 'rm -rf "$DIR"' EXIT
cat > "$DIR/block.gx" <<'GX'
println("ARMED");
let result = {
  let f = |v| sys::time::after_idle(duration:200.ms, v);
  let a = never<i64>();
  let b = never<i64>();
  seq { { a <- f(1); b <- f(2) } };
  let late = sys::time::timer(duration:300.ms, false);
  let first = once(any(late ~ "late", (a + b) ~ "done"));
  sys::time::after_idle(duration:100.ms, first)
};
println("RESULT [result]");
sys::exit(sys::time::timer(duration:1.2s, false) ~ 0)
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
run "$DIR/block.gx" 0.15 0.2
run "$DIR/block.gx" 0.15 0.2
