#!/bin/bash
# small-pkgs-02: xls::read pads the used range to A1; a 2 KB xlsx exhausts memory.
# Usage: small-pkgs-02.sh <graphix binary>
#   (run from the repo root; generated files go to a fresh mktemp dir)
# Builds three one-cell workbooks with x-panics-02-make-xlsx.py and prints
# array::len(xls::read(b, "S")$) for each.
# Expected: each read costs memory proportional to the input (a ~2 KB file).
# Observed (debug build, 6 GB cap):
#   C3        -> 3
#   XFD2000   -> 2000   (32.8M nulls built from a 2 KB file, ~1.5 s)
#   XFD100000 -> OOM-killed at the 6 GB cap in ~7 s (1.6e9 nulls, ~26 GB wanted)
set -u
G=${1:?graphix binary}
D=$(mktemp -d)
MK="$(dirname "$0")/x-panics-02-make-xlsx.py"
for cell in C3 XFD2000 XFD100000; do
    timeout -s KILL 20 python3 -I "$MK" "$D/$cell.xlsx" "$cell"
    cat > "$D/$cell.gx" <<GX
let b = sys::fs::read_all_bin("$D/$cell.xlsx")\$;
let n = array::len(xls::read(b, "S")\$);
sys::exit(sys::time::after_idle(duration:500.ms, n ~ 0));
n
GX
    echo "== $cell ($(stat -c %s "$D/$cell.xlsx") bytes)"
    timeout -s KILL 90 "$G" --no-cache "$D/$cell.gx"
    echo "exit $?"
done
rm -rf "$D"
