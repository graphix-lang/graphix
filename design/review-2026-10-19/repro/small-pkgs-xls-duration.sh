#!/bin/sh
# xls::read turns an xlsx duration cell ([h]:mm:ss) into a datetime in 1900
# Run: sh design/review-2026-10-19/repro/small-pkgs-xls-duration.sh
# Expected: [[36.h, 2025-10-13 12:00:00 UTC]]  (A1 holds 1.5 days formatted [h]:mm:ss)
# Observed: [[1900-01-01 12:00:00 UTC, 2025-10-13 12:00:00 UTC]]
set -e
D=$(mktemp -d)
HERE=$(cd "$(dirname "$0")" && pwd)
timeout -s KILL 20 python3 -I "$HERE/small-pkgs-xls-duration-mkxlsx.py" "$D/dur.xlsx"
cat > "$D/x.gx" <<GX
let data = sys::fs::read_all_bin("$D/dur.xlsx")\$;
let r = xls::read(data, "S");
sys::exit(sys::time::after_idle(duration:300.ms, r ~ 0));
r
GX
timeout -s KILL 60 /home/eric/tmp/review-2026-10-19/bin/graphix --no-cache "$D/x.gx"
rm -rf "$D"
