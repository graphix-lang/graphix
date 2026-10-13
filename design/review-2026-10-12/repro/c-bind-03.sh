#!/bin/sh
# c-bind-03: wake tracker watches only a destructuring let's representative
# (the first id, Env::facet_of), so a `<-` to one sibling while the reading
# arm sleeps is a phantom catch-up (write to the representative `a`, the arm
# reads `b`) or a lost one (write to `b`, never observed).
#
# Run: sh design/review-2026-10-12/repro/c-bind-03.sh <path-to-graphix>
#   (runs: timeout -s KILL 30 <graphix> --no-cache <prog>.gx)
# Both engines (add --no-fusion to compare) print the same.
#
# phantom, `let (a, b)`:    observed 0 1 1 2 2 3 1 2 2 2   expected 0 1 1 2 2 3 1 1 1
#   (expected = the same program with `let a = 0; let b = 0` or `let (b, a)`)
# lost, `let (a, b)`:       observed 0 1 1 2 3 1 1 1 [1]   expected 0 1 1 2 3 1 2 2 2
#   (the count of trailing repeats varies with timer jitter)
#   (expected = the same program with `let a = 0; let b = 0` or `let (b, a)`)
GX=${1:-graphix}
D=$(mktemp -d)
cat > "$D/phantom.gx" <<'EOF'
let clock = sys::time::timer(duration:50.ms, true);
let n = 0;
n <- clock ~ n + 1;
let (a, b) = (0, 0);
a <- select n { 2 => 7, _ => never() };
let c = ((n < 2) || (n >= 4)) && (a >= 0);
sys::exit(select n { 7 => 0, _ => never() });
let out = select c {
  true => {
    let cnt = 0;
    cnt <- b ~ cnt + 1;
    cnt
  },
  false => n
};
out
EOF
cat > "$D/lost.gx" <<'EOF'
let clock = sys::time::timer(duration:50.ms, true);
let n = 0;
n <- clock ~ n + 1;
let (a, b) = (0, 0);
b <- select n { 2 => 7, _ => never() };
let c = (n < 2) || (n >= 4);
sys::exit(select n { 7 => 0, _ => never() });
let out = select c {
  true => {
    let cnt = 0;
    cnt <- b ~ cnt + 1;
    cnt
  },
  false => n
};
out
EOF
for p in phantom lost; do
  printf '%s: ' "$p"
  timeout -s KILL 30 "$GX" --no-cache "$D/$p.gx" 2>&1 | tr '\n' ' '
  echo
done
rm -rf "$D"
