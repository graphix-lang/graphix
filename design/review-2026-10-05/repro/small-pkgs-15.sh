#!/usr/bin/env bash
# small-pkgs-15: xls::read drops leading blank rows and columns without
# reporting the offset.
#
# parse_sheet (stdlib/graphix-package-xls/src/lib.rs:55-64) returns
# range.rows() of calamine's worksheet_range and drops range.start().
# calamine builds that range over the non-empty cells only (xlsx/xls/xlsb:
# Range::from_sparse with the default HeaderRow::FirstNonEmptyRow; ods:
# get_range's row_min/col_min), so rows[0][0] is the first used row and
# the first used column of the whole sheet, not A1, and nothing returns
# where it starts.
#
# The script builds two minimal xlsx files: "margin" holds B3=1, C3=2,
# B4=3; "margin+note" is the same plus "note" in A6.
#
# command: GRAPHIX=/path/to/graphix bash design/review-2026-10-05/repro/small-pkgs-15.sh
#
# expected ("Read a sheet by name as a 2D array of rows", xls mod.gxi):
#   B3 at rows[2][1] in both files (or the start offset returned beside
#   the rows); typing into an empty cell does not move the other cells.
# observed (HEAD c722befe, debug build; the same with --no-fusion; the
# line order varies, the reads are async):
#   margin:      [[1, 2], [3, null]]
#   margin+note: [[null, 1, 2], [null, 3, null], [null, null, null], ["note", null, null]]
#   B3 is rows[0][0] in the first and rows[0][1] in the second: the note
#   in A6 shifted every column of the table.
set -u
GRAPHIX=${GRAPHIX:-graphix}
dir=$(mktemp -d)
trap 'rm -rf "$dir"' EXIT
export XDG_CACHE_HOME="$dir/cache"

python3 - "$dir" <<'EOF'
import sys, zipfile

CT = """<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">
<Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/>
<Default Extension="xml" ContentType="application/xml"/>
<Override PartName="/xl/workbook.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml"/>
<Override PartName="/xl/worksheets/sheet1.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml"/>
</Types>"""
RELS = """<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">
<Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument" Target="xl/workbook.xml"/>
</Relationships>"""
WB = """<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<workbook xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships">
<sheets><sheet name="S" sheetId="1" r:id="rId1"/></sheets>
</workbook>"""
WBRELS = """<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">
<Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet" Target="worksheets/sheet1.xml"/>
</Relationships>"""


def write(path, dim, rows):
    sheet = (
        '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n'
        '<worksheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main">'
        f'<dimension ref="{dim}"/><sheetData>{rows}</sheetData></worksheet>'
    )
    with zipfile.ZipFile(path, "w", zipfile.ZIP_DEFLATED) as z:
        z.writestr("[Content_Types].xml", CT)
        z.writestr("_rels/.rels", RELS)
        z.writestr("xl/workbook.xml", WB)
        z.writestr("xl/_rels/workbook.xml.rels", WBRELS)
        z.writestr("xl/worksheets/sheet1.xml", sheet)


d = sys.argv[1]
table = (
    '<row r="3"><c r="B3"><v>1</v></c><c r="C3"><v>2</v></c></row>'
    '<row r="4"><c r="B4"><v>3</v></c></row>'
)
note = '<row r="6"><c r="A6" t="inlineStr"><is><t>note</t></is></c></row>'
write(f"{d}/margin.xlsx", "B3:C4", table)
write(f"{d}/margin_note.xlsx", "A3:C6", table + note)
EOF

cat > "$dir/read.gx" <<EOF
let a = xls::read(sys::fs::read_all_bin("$dir/margin.xlsx")\$, "S")\$;
let b = xls::read(sys::fs::read_all_bin("$dir/margin_note.xlsx")\$, "S")\$;
println("margin:      [a]");
println("margin+note: [b]");
sys::exit(sys::time::after_idle(duration:200.ms, (a, b) ~ 0))
EOF

timeout -s KILL 60 "$GRAPHIX" --no-cache "$dir/read.gx"
