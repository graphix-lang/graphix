#!/bin/sh
# x-panics-02: xls::read pads the used range out to A1, so a 2 KB workbook
# whose only cell is XFD1048576 builds 1048576 rows x 16384 nulls (17e9
# Values) and the process is OOM-killed.
#
# usage: sh x-panics-02.sh /path/to/graphix
# expected: "rows 1048576"-style answer bounded in memory, or an XlsErr
# observed: the run is killed by the memory cap within seconds; a C3-only
#           workbook ("small") answers "rows 3" at once.
set -e
G=${1:-graphix}
D=$(mktemp -d)
HERE=$(cd "$(dirname "$0")" && pwd)
python3 -I "$HERE/x-panics-02-make-xlsx.py" "$D/small.xlsx" C3
python3 -I "$HERE/x-panics-02-make-xlsx.py" "$D/corner.xlsx" XFD1048576
for f in small corner; do
  cat > "$D/$f.gx" <<GX
let r = xls::read(sys::fs::read_all_bin("$D/$f.xlsx")\$, "S");
sys::exit(sys::time::after_idle(duration:200.ms, r ~ 0));
select r { error as e => "error [e]", rows => "rows [array::len(rows)]" }
GX
  echo "-- $f.xlsx ($(wc -c < "$D/$f.xlsx") bytes)"
  timeout -s KILL 60 "$G" --no-cache "$D/$f.gx" 2>&1 | tail -2 || true
done
