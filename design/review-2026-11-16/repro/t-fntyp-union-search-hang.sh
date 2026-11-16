#!/bin/bash
# Title: a val whose signature and implementation hold three six-member
#        unions of open variables that cannot pair hangs the check
#        (SigPairs::search tries 720^3 member assignments)
# Command: bash design/review-2026-11-16/repro/t-fntyp-union-search-hang.sh
# Expected: a refusal in well under a second ("type variable usage
#           mismatch: expected 'g, got 'a"), as the two-union form gives.
# Observed: two unions: refused after ~0.6 s (0.08 s for a module without
#           the unions); three unions: no answer in 170 s (killed, exit 137).
G=${GRAPHIX:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(mktemp -d)
trap 'rm -rf "$D"' EXIT
U="[Array<'a>, Array<'b>, Array<'c>, Array<'d>, Array<'e>, Array<'f>]"
A="'a, 'b, 'c, 'd, 'e, 'f"
mk() { # dir, sig params, impl params
  mkdir -p "$D/$1"
  printf "type U<$A> = $U;\ntype V<$A> = $U;\nval f: fn(w: 'g, $2) -> i64;\n" > "$D/$1/foo.gxi"
  printf "let f = |w: 'p1, $3| 0\n" > "$D/$1/foo.gx"
  printf 'mod foo;\n1\n' > "$D/$1/main.gx"
}
mk two "x: U<$A>, y: U<$A>" \
  "x: V<'p1, 'p2, 'p3, 'p4, 'p5, 'p6>, y: V<'p2, 'q2, 'q3, 'q4, 'q5, 'q6>"
mk three "x: U<$A>, y: U<$A>, z: U<$A>" \
  "x: V<'p1, 'p2, 'p3, 'p4, 'p5, 'p6>, y: V<'p2, 'q2, 'q3, 'q4, 'q5, 'q6>, z: V<'p3, 'r2, 'r3, 'r4, 'r5, 'r6>"
for d in two three; do
  echo "--- $d unions"
  ( cd "$D/$d" && time (timeout -s KILL ${T:-170} "$G" --no-cache --check main.gx 2>&1 | tail -1 | cut -c1-200; echo "exit ${PIPESTATUS[0]}") )
done
