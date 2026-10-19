#!/bin/sh
# check_ods's cell bound is reset by a <table:table/> inside a cell, so a 10 KB ods file OOM-kills the runtime
# Run: sh design/review-2026-10-19/repro/small-pkgs-ods-nested-table.sh
# Expected: plain and nested both answer error:`XlsErr("an ods table spans over 16777216 cells")
# Observed: plain is refused; nested (2000 rows) answers ["S"] after calamine built 32.7M cells;
#   nested (8000 rows, a 10 KB file) is OOM-killed by the 6 GB probe cap.
set -e
D=$(mktemp -d)
HERE=$(cd "$(dirname "$0")" && pwd)
GX=/home/eric/tmp/review-2026-10-19/bin/graphix
for spec in "plain 2000" "nested 2000 nested" "nested8k 8000 nested"; do
  set -- $spec
  timeout -s KILL 30 python3 -I "$HERE/small-pkgs-ods-nested-table-mkods.py" "$D/$1.ods" $2 $3
  cat > "$D/$1.gx" <<GX
let data = sys::fs::read_all_bin("$D/$1.ods")\$;
let r = xls::sheets(data);
sys::exit(sys::time::after_idle(duration:300.ms, r ~ 0));
r
GX
  echo "== $1 ($(wc -c < "$D/$1.ods") bytes)"
  timeout -s KILL 150 $GX --no-cache "$D/$1.gx" || echo "exit $?"
done
rm -rf "$D"
