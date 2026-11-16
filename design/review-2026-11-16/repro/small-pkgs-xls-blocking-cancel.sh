#!/bin/bash
# xls::read runs its parse on a blocking thread but keeps EvalCachedAsync::CANCEL_SAFE
# at its default (true): an arm sleep drops the future, the parse keeps running, and the
# site's next call starts another at once, so parses whose replies land nowhere pile up.
# command: bash small-pkgs-xls-blocking-cancel.sh
# expected: an arm toggled every 150 ms over a 5M-cell xlsx (one parse ~1.4 s, ~200 MB
#   peak) holds one parse at a time: peak RSS near the single-parse figure.
# observed (two runs): single parse peak RSS 207760-292484 kB; toggled 900064-972072 kB (3-5 parses
#   running at once), and no reply ever printed.
set -e
D=$(mktemp -d /home/eric/tmp/review-2026-11-16/work/small-pkgs/xlsc.XXXX)
G=/home/eric/tmp/review-2026-11-16/bin/graphix
REAL=/home/eric/tmp/review-2026-11-16/bin/real/graphix
cat > $D/mk.py <<'PY'
import sys, zipfile
out, rows, cols = sys.argv[1], int(sys.argv[2]), int(sys.argv[3])
def col(c):
    s = ""; c += 1
    while c:
        c, r = divmod(c - 1, 26); s = chr(65 + r) + s
    return s
ct = '<?xml version="1.0" encoding="UTF-8"?><Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types"><Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/><Default Extension="xml" ContentType="application/xml"/><Override PartName="/xl/workbook.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml"/><Override PartName="/xl/worksheets/sheet1.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml"/></Types>'
rels = '<?xml version="1.0" encoding="UTF-8"?><Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships"><Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument" Target="xl/workbook.xml"/></Relationships>'
wb = '<?xml version="1.0" encoding="UTF-8"?><workbook xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"><sheets><sheet name="S" sheetId="1" r:id="rId1"/></sheets></workbook>'
wbr = '<?xml version="1.0" encoding="UTF-8"?><Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships"><Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet" Target="worksheets/sheet1.xml"/></Relationships>'
with zipfile.ZipFile(out, "w", zipfile.ZIP_DEFLATED) as z:
    for k, v in [("[Content_Types].xml", ct), ("_rels/.rels", rels), ("xl/workbook.xml", wb), ("xl/_rels/workbook.xml.rels", wbr)]:
        z.writestr(k, v)
    with z.open("xl/worksheets/sheet1.xml", "w") as f:
        f.write(b'<?xml version="1.0" encoding="UTF-8"?><worksheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main"><sheetData>')
        for r in range(rows):
            f.write(("".join(['<row r="%d">' % (r + 1)] + ['<c r="%s%d"><v>%d</v></c>' % (col(c), r + 1, r * cols + c) for c in range(cols)] + ['</row>'])).encode())
        f.write(b'</sheetData></worksheet>')
PY
timeout -s KILL 170 python3 $D/mk.py $D/big.xlsx 100000 50
cat > $D/one.gx <<GX
let b = sys::fs::read_all_bin("$D/big.xlsx")\$;
let r = xls::read(b, "S");
println(select r { error as e => "err", v => "one parse: [array::len(v)] rows" });
sys::exit(sys::time::after_idle(duration:3000.ms, 0));
0
GX
cat > $D/toggle.gx <<GX
let b = sys::fs::read_all_bin("$D/big.xlsx")\$;
let clock = sys::time::timer(duration:150.ms, true);
let n = 0;
n <- clock ~ n + 1;
let r = select n { k if k % 2 == 0 => xls::read(k ~ b, "S"), _ => never() };
println(select r { error as e => "err", v => "[n] got [array::len(v)]" });
sys::exit(select n { k if k > 26 => k ~ 0, _ => never() });
0
GX
peak() {
  timeout -s KILL 60 $G --no-cache "$1" &
  W=$!; pk=0
  for i in $(seq 1 300); do
    sleep 0.1
    P=""; for q in $(pgrep -f "real/graphix --no-cache $1"); do [ "$(readlink /proc/$q/exe)" = $REAL ] && P=$q; done
    [ -z "$P" ] && { kill -0 $W 2>/dev/null || break; continue; }
    R=$(awk '/VmRSS/{print $2}' /proc/$P/status 2>/dev/null || true)
    [ -n "$R" ] && [ "$R" -gt "$pk" ] && pk=$R
  done
  wait $W || true
  echo "$(basename $1): peak RSS ${pk} kB"
}
peak $D/one.gx
peak $D/toggle.gx
rm -r $D
