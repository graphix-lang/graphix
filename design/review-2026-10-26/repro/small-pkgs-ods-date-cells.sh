#!/bin/sh
# small-pkgs: xls::read gives an ods date cell holding a date alone, or fractional seconds, as a string
# Run: sh design/review-2026-10-26/repro/small-pkgs-ods-date-cells.sh
# Expected: [[2020-01-01 00:00:00 UTC, 2020-01-01 10:00:00 UTC, 2020-01-01 10:00:00.500 UTC]]
#   (every office:value-type="date" cell a datetime, as an xlsx date cell is)
# Observed: [["2020-01-01", 2020-01-01 10:00:00 UTC, "2020-01-01T10:00:00.5"]]
set -e
D=${TMPDIR:-/tmp}/small-pkgs-ods-dates; mkdir -p "$D"
cat > "$D/mkods.py" <<'PY'
import zipfile, sys
out = sys.argv[1]
cells = ''.join('<table:table-cell office:value-type="date" office:date-value="%s"><text:p>x</text:p></table:table-cell>' % v for v in sys.argv[2:])
content = ('<?xml version="1.0" encoding="UTF-8"?><office:document-content xmlns:office="urn:oasis:names:tc:opendocument:xmlns:office:1.0" '
  'xmlns:table="urn:oasis:names:tc:opendocument:xmlns:table:1.0" xmlns:text="urn:oasis:names:tc:opendocument:xmlns:text:1.0">'
  '<office:body><office:spreadsheet><table:table table:name="S"><table:table-row>' + cells +
  '</table:table-row></table:table></office:spreadsheet></office:body></office:document-content>')
with zipfile.ZipFile(out, 'w', zipfile.ZIP_STORED) as z:
    z.writestr('mimetype', 'application/vnd.oasis.opendocument.spreadsheet')
    z.writestr('META-INF/manifest.xml', '<?xml version="1.0" encoding="UTF-8"?><manifest:manifest xmlns:manifest="urn:oasis:names:tc:opendocument:xmlns:manifest:1.0"/>')
    z.writestr('content.xml', content)
PY
python3 -I "$D/mkods.py" "$D/dates.ods" 2020-01-01 2020-01-01T10:00:00 2020-01-01T10:00:00.5
cat > "$D/x.gx" <<GX
let data = sys::fs::read_all_bin("$D/dates.ods")\$;
sys::exit(sys::time::after_idle(duration:300.ms, 0));
xls::read(data, "S")
GX
timeout -s KILL 60 /home/eric/tmp/review-2026-10-26/bin/graphix --no-cache "$D/x.gx"
