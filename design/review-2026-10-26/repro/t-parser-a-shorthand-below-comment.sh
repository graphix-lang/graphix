#!/bin/sh
# Title: an own-line comment below a shorthand struct field or labeled-arg shorthand
#        (the list's last item) parses as the comment ending that item's line
# Command: sh design/review-2026-10-26/repro/t-parser-a-shorthand-below-comment.sh
# Expected: fmt keeps "// below ..." on its own line under `b` / `#a`, as it does
#           under `b: 2`
# Observed: fmt prints `b // below shorthand` and `#a // below labeled shorthand`
B=${B:-/home/eric/tmp/review-2026-10-26/bin/graphix}
D=$(mktemp -d)
cat > $D/p.gx <<'GX'
let a = 1;
let b = 2;
let h = |#a, #b = 3| a + b;
let q = {
    a: 1,
    b: 2
    // below full
};
let r = h(
    #b: 1,
    #a
    // below labeled shorthand
);
let w = {
    a,
    b
    // below shorthand
};
(q, r, w)
GX
timeout -s KILL 30 $B fmt --stdout $D/p.gx
rm -rf $D
