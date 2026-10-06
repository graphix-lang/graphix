#!/bin/bash
# rt-01: abandoned async work is never cancelled and its result leaks in the
# store (seq steps, select arms, timers).
#
# Each program prints its own VmRSS every 2 s and exits at 11 s.
#   seq        a seq triggered every 10 ms whose step reads a 100 KB file:
#              the passed step sleeps, CachedArgsAsync::sleep remints its
#              reply id and leaves the old id's 100 KB string in the store
#   seq_ctl    the same read every 10 ms outside any seq or arm
#   arm        the same read in a select arm toggled every 10 ms (each wake
#              re-reads; each sleep strands the result)
#   tlong      20 `timer(duration:3600.s, true)` in an arm toggled every ms:
#              each sleep releases the id but the runtime task keeps
#              sleeping for an hour (GXRt keeps no handle, Rt has no cancel)
#   tshort100  100 `timer(duration:30.ms, true)` in the same arm: each
#              abandoned task completes 30 ms later and push_var_event
#              stores its fire under the dead id (gx.rs:455)
#   arm_ctl    the toggled arm with no timers
#
# command: timeout -s KILL 170 bash design/review-2026-10-05/repro/rt-01.sh <graphix>
#          (runs `<graphix> --no-cache` on each program)
#
# expected: every program's VmRSS stays flat like its control's.
# observed (HEAD c722befe, debug build; VmRSS MB at 2 / 4 / 6 / 8 / 10 s):
#   seq        84 103 122 140 158  (~101 KB per run, ~900 runs)
#   seq_ctl    64  65  66  66  66
#   arm        73  83  92 101 110  (~104 KB per read, 450 reads)
#   tlong      67  71  75  79  82  (~420 B per abandoned timer)
#   tshort100  74  80  90  95  99
#   arm_ctl    63  65  66  67  68
#   Over 40 s (VmRSS every 5 s) arm_ctl levels off (73 75 76 79 79 79 80 80)
#   while tshort100 keeps climbing (81 92 99 108 116 122 130 139, ~70 B per
#   abandoned fire). The seq program with --no-fusion: 82 101 119 137 156.
#   With the step `let n = str::len(sys::fs::read_all(path)$)` (the let holds
#   an int) growth is still ~100 KB per run: read_all's delivery, not the let.
G=${1:-graphix}
D=$(mktemp -d)
trap 'rm -rf "$D"' EXIT
head -c 102400 /dev/zero | tr '\0' 'x' > "$D/blob.txt"

TAIL='let tick = sys::time::timer(duration:2.s, true);
let rss = re::find(#pat: r"VmRSS:\s+\d+ kB", sys::fs::read_all(tick ~ "/proc/self/status")$)$;
sys::exit(sys::time::timer(duration:11.s, false) ~ 0);'

cat > "$D/seq.gx" <<EOF
let fast = sys::time::timer(duration:10.ms, true);
let r = seq fast {
  let s = sys::fs::read_all("$D/blob.txt")\$;
  str::len(s)
};
let runs = 0;
runs <- r ~ runs + 1;
$TAIL
"[rss ~ runs] runs: [rss]"
EOF

cat > "$D/seq_ctl.gx" <<EOF
let fast = sys::time::timer(duration:10.ms, true);
let r = str::len(sys::fs::read_all(fast ~ "$D/blob.txt")\$);
let runs = 0;
runs <- r ~ runs + 1;
$TAIL
"[rss ~ runs] runs: [rss]"
EOF

cat > "$D/arm.gx" <<EOF
let n = 0;
n <- sys::time::timer(duration:10.ms, true) ~ n + 1;
let x = select n % 2 {
  0 => str::len(sys::fs::read_all("$D/blob.txt")\$),
  _ => 0
};
let reads = 0;
reads <- select x { 0 => never(), _ => x ~ reads + 1 };
$TAIL
"[rss ~ reads] reads: [rss]"
EOF

timers() { # $1 = duration, $2 = how many
  printf 'let n = 0;\nn <- sys::time::timer(duration:1.ms, true) ~ n + 1;\nlet x = select n %% 2 {\n  0 => {\n'
  for i in $(seq 1 "$2"); do printf '    let t%d = sys::time::timer(duration:%s, true);\n' "$i" "$1"; done
  printf '    n\n  },\n  _ => 0\n};\n%s\n"[rss ~ n] ticks: [rss]"\n' "$TAIL"
}
timers 3600.s 20 > "$D/tlong.gx"
timers 30.ms 100 > "$D/tshort100.gx"
timers 30.ms 0 > "$D/arm_ctl.gx"

for p in seq seq_ctl arm tlong tshort100 arm_ctl; do
  echo "== $p"
  timeout -s KILL 25 "$G" --no-cache "$D/$p.gx"
done
