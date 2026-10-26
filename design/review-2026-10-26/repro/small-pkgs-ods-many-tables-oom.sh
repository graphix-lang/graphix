#!/bin/sh
# small-pkgs: an 814-byte ods of 16 tables, each under check_ods' MAX_CELLS, OOM-kills the runtime
# Run: sh design/review-2026-10-26/repro/small-pkgs-ods-many-tables-oom.sh
# Expected: an `XlsErr` ("an ods table spans over 16777216 cells" or a workbook-wide bound):
#   calamine builds every table of the workbook when it opens, so the bound must be on their sum.
# Observed: "[probe] OOM-KILLED: this run exceeded its 6G memory cap" (exit 251); with one table
#   (mkods_multi.py one.ods 1 4000) xls::sheets answers ["T0"].
set -e
D=${TMPDIR:-/tmp}/small-pkgs-ods-tables; mkdir -p "$D"
cat > "$D/mkods_multi.py" <<'PY'
# mkods_multi.py OUT NTABLES N : an ods of NTABLES tables, each a value at A1 and
# one at row N+1, column N+1 (an N+1 by N+1 used area, under check_ods' MAX_CELLS).
import zipfile, sys
out, ntables, n = sys.argv[1], int(sys.argv[2]), int(sys.argv[3])
one = '<table:table-cell office:value-type="float" office:value="1"><text:p>1</text:p></table:table-cell>'
table = ('<table:table table:name="T%d"><table:table-row>' + one + '</table:table-row>'
         '<table:table-row table:number-rows-repeated="%d"><table:table-cell table:number-columns-repeated="%d"/>'
         + one + '</table:table-row></table:table>')
tables = ''.join(table % (i, n, n) for i in range(ntables))
content = ('<?xml version="1.0" encoding="UTF-8"?><office:document-content xmlns:office="urn:oasis:names:tc:opendocument:xmlns:office:1.0" '
  'xmlns:table="urn:oasis:names:tc:opendocument:xmlns:table:1.0" xmlns:text="urn:oasis:names:tc:opendocument:xmlns:text:1.0">'
  '<office:body><office:spreadsheet>' + tables + '</office:spreadsheet></office:body></office:document-content>')
with zipfile.ZipFile(out, 'w', zipfile.ZIP_DEFLATED) as z:
    z.writestr('mimetype', 'application/vnd.oasis.opendocument.spreadsheet')
    z.writestr('META-INF/manifest.xml', '<?xml version="1.0" encoding="UTF-8"?><manifest:manifest xmlns:manifest="urn:oasis:names:tc:opendocument:xmlns:manifest:1.0"/>')
    z.writestr('content.xml', content)
PY
python3 -I "$D/mkods_multi.py" "$D/multi.ods" 16 4000
cat > "$D/x.gx" <<GX
let data = sys::fs::read_all_bin("$D/multi.ods")\$;
let s = xls::sheets(data);
sys::exit(sys::time::after_idle(duration:300.ms, s ~ 0));
s
GX
timeout -s KILL 150 /home/eric/tmp/review-2026-10-26/bin/graphix --no-cache "$D/x.gx"
