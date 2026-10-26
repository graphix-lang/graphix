#!/bin/bash
# t-print: a select (or seq) head that fits flat but for its ` {` overflows the width
# Command: bash design/review-2026-10-26/repro/t-print-select-head-overflow.sh
# Expected: every printed line is at most 40 columns (the scrutinee laid out over lines
#           once `select <scrutinee> {` no longer fits).
# Observed: `select ffffffffffffffffffffffffff(1, 2) {` (41 columns) and
#           `seq fffffffffffffffffffffffffffff(1, 2) {` (41 columns) at --width 40.
G=${G:-/home/eric/tmp/review-2026-10-26/bin/graphix}
d=$(mktemp -d)
cat > $d/sel.gx <<'GX'
let ffffffffffffffffffffffffff = |x, y| x + y;
select ffffffffffffffffffffffffff(1, 2) {
    3 => 4,
    _ => 5
}
GX
cat > $d/seq.gx <<'GX'
let fffffffffffffffffffffffffffff = |x, y| x + y;
seq fffffffffffffffffffffffffffff(1, 2) {
    3;
    4
}
GX
for f in $d/sel.gx $d/seq.gx; do
  $G fmt --width 40 --stdout $f | awk '{ printf "%3d: %s\n", length($0), $0 }' | grep -E '^ *4[1-9]:'
done
rm -rf $d
