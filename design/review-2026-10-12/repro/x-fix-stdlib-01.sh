#!/bin/sh
# x-fix-stdlib-01: xls::read pads to A1 densely: a 2 KB xlsx with one far cell OOM-kills the runtime
# command: sh x-fix-stdlib-01.sh <graphix binary>
#   (writes small.xlsx, one cell at C3, and bomb.xlsx, one cell at XFD100000, via
#    x-fix-stdlib-06.py next to this file, then reads both with xls::read)
# expected: small prints, then the bomb is refused with an XlsErr (or read without padding)
# observed (6 GB probe cap): 'small: [[null, null, null], [null, null, null], [null, null, 1]]'
#   then '[probe] OOM-KILLED', exit 251: parse_sheet allocates 100000 rows x 16384 nulls
set -e
G=${1:-graphix}
HERE=$(cd "$(dirname "$0")" && pwd)
D=$(mktemp -d)
python3 -I "$HERE/x-fix-stdlib-06.py" C3 "$D/small.xlsx"
python3 -I "$HERE/x-fix-stdlib-06.py" XFD100000 "$D/bomb.xlsx"
cat > "$D/p.gx" <<GX
let p = "$D/";
let small = xls::read(sys::fs::read_all_bin("[p]small.xlsx")\$, "S");
println("small: [small]");
let big = xls::read(sys::fs::read_all_bin("[p]bomb.xlsx")\$, "S");
println("big rows: [array::len(big\$)]");
sys::exit(sys::time::after_idle(duration:20.s, 0))
GX
ls -l "$D"/*.xlsx
timeout -s KILL 120 "$G" --no-cache "$D/p.gx"
