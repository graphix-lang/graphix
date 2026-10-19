#!/bin/sh
# t-print: a decorated expression in an index, slice, map key or struct-with source is
# written by the flat Display: its comment/attribute lands at column 0 mid-layout and the
# block it decorates is squeezed onto one line (with a trailing space after `{`).
# command: sh t-print-decorated-suffix.sh
# expected: the block laid out over lines, the comment above `let i` at its indent
# observed:
#     a[{ 
# // c
# let i = 0; i }];
G=${G:-/home/eric/tmp/review-2026-10-19/bin/graphix}
d=$(mktemp -d)
cat > $d/a.gx <<'GX'
let a = [1, 2];
let m = {"k" => 1};
let s = {a: 1};
let v = a[{
    // c
    let i = 0;
    i
}];
let w = m{{
    #[sync]
    let k = "k";
    k
}};
let n = {
    {
        // c
        let k = 1;
        k
    } => 2
};
let t = { ({
    // c
    let z = s;
    z
}) with a: 2 };
v
GX
$G fmt --stdout $d/a.gx
rm -rf $d
