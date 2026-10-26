#!/bin/sh
# small-pkgs: a 1 KB xlsx whose sharedStrings.xml declares uniqueCount="100000000000" aborts the process
# Run: sh design/review-2026-10-26/repro/small-pkgs-xlsx-sst-abort.sh
# Expected: an `XlsErr` value (or the sheet [["hi"]], the count being only a hint).
# Observed: "memory allocation of 2400000000000 bytes failed", exit 134 (SIGABRT): calamine's
#   Xlsx::read_shared_strings calls `self.strings.reserve(uniqueCount)` on the attribute's
#   value when the workbook opens; an allocation failure aborts, which no catch_unwind or
#   spawn_blocking JoinError contains. With uniqueCount="1" the same file reads [["hi"]].
set -e
D=${TMPDIR:-/tmp}/small-pkgs-xlsx-sst; mkdir -p "$D"
cat > "$D/mkxlsx.py" <<'PY'
# mkxlsx.py OUT UNIQUECOUNT : a one-sheet xlsx whose sharedStrings.xml declares
# uniqueCount=UNIQUECOUNT (and holds one string).
import zipfile, sys
out, n = sys.argv[1], sys.argv[2]
files = {
 '[Content_Types].xml': '<?xml version="1.0" encoding="UTF-8"?><Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types"><Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/><Default Extension="xml" ContentType="application/xml"/><Override PartName="/xl/workbook.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml"/><Override PartName="/xl/worksheets/sheet1.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml"/><Override PartName="/xl/sharedStrings.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sharedStrings+xml"/></Types>',
 '_rels/.rels': '<?xml version="1.0" encoding="UTF-8"?><Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships"><Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument" Target="xl/workbook.xml"/></Relationships>',
 'xl/workbook.xml': '<?xml version="1.0" encoding="UTF-8"?><workbook xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"><sheets><sheet name="Sheet1" sheetId="1" r:id="rId1"/></sheets></workbook>',
 'xl/_rels/workbook.xml.rels': '<?xml version="1.0" encoding="UTF-8"?><Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships"><Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet" Target="worksheets/sheet1.xml"/><Relationship Id="rId2" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/sharedStrings" Target="sharedStrings.xml"/></Relationships>',
 'xl/sharedStrings.xml': '<?xml version="1.0" encoding="UTF-8"?><sst xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main" count="1" uniqueCount="%s"><si><t>hi</t></si></sst>' % n,
 'xl/worksheets/sheet1.xml': '<?xml version="1.0" encoding="UTF-8"?><worksheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main"><sheetData><row r="1"><c r="A1" t="s"><v>0</v></c></row></sheetData></worksheet>',
}
with zipfile.ZipFile(out, 'w', zipfile.ZIP_DEFLATED) as z:
    for k, v in files.items(): z.writestr(k, v)
PY
python3 -I "$D/mkxlsx.py" "$D/big.xlsx" 100000000000
cat > "$D/x.gx" <<GX
let data = sys::fs::read_all_bin("$D/big.xlsx")\$;
let s = xls::read(data, "Sheet1");
sys::exit(sys::time::after_idle(duration:300.ms, s ~ 0));
s
GX
timeout -s KILL 60 /home/eric/tmp/review-2026-10-26/bin/graphix --no-cache "$D/x.gx"
