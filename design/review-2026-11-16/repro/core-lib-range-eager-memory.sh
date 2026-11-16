#!/bin/bash
# Title: range queues every element as a runtime write up front: memory is
#        proportional to j - i for as long as the range is being delivered.
# Command: bash design/review-2026-11-16/repro/core-lib-range-eager-memory.sh
# Expected: a range costs O(1) while it delivers one element a cycle (as
#           iterq's cursor does), so peak RSS stays near the idle program's.
# Observed: idle program peak RSS ~74 MB; range(0, 16000000) ~1.1 GB
#           (about 65 bytes an element), for the whole delivery.
G=${G:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(mktemp -d)
cat > "$D/range.gx" <<'GX'
let r = range(0, 16000000);
let c = count(r);
sys::exit(sys::time::timer(duration:3.s, false) ~ 0);
never()
GX
cat > "$D/idle.gx" <<'GX'
let c = 1;
sys::exit(sys::time::timer(duration:3.s, false) ~ 0);
never()
GX
peak() {
  "$G" --no-cache "$1" > /dev/null 2>&1 &
  local P=$! max=0 r pid
  while kill -0 $P 2>/dev/null; do
    pid=$(for q in $(pgrep -x graphix); do grep -qa -- "$1" /proc/$q/cmdline 2>/dev/null && echo $q; done | head -1)
    r=$([ -n "$pid" ] && awk '/VmRSS/{print $2}' /proc/$pid/status 2>/dev/null)
    [ -n "$r" ] && [ "$r" -gt "$max" ] && max=$r
    sleep 0.2
  done
  wait $P
  echo "$(basename "$1"): peak RSS ${max} kB"
}
timeout -s KILL 60 bash -c "$(declare -f peak); G=$G; peak $D/idle.gx; peak $D/range.gx"
rm -rf "$D"
