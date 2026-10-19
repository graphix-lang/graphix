# Writes an ods workbook (argv[1]) of one sheet "S" holding argv[2] rows, each 16383 empty
# columns and a value in the last one: the shape xls's check_ods refuses (its pin
# ods_refuses_many_far_rows). With argv[3] == "nested", the valued cell also holds an empty
# <table:table/>, which calamine skips with the cell's content but check_ods takes for a new
# table and resets its counts, so the bound never trips.
import sys, zipfile
rows, nested = int(sys.argv[2]), len(sys.argv) > 3 and sys.argv[3] == "nested"
inner = "<table:table/>" if nested else "<text:p>1</text:p>"
row = ('<table:table-row><table:table-cell table:number-columns-repeated="16383"/>'
       '<table:table-cell office:value-type="float" office:value="1">' + inner +
       '</table:table-cell></table:table-row>')
content = ('<?xml version="1.0" encoding="UTF-8"?>'
  '<office:document-content xmlns:office="urn:oasis:names:tc:opendocument:xmlns:office:1.0" '
  'xmlns:table="urn:oasis:names:tc:opendocument:xmlns:table:1.0" '
  'xmlns:text="urn:oasis:names:tc:opendocument:xmlns:text:1.0"><office:body><office:spreadsheet>'
  '<table:table table:name="S">' + row * rows + '</table:table></office:spreadsheet></office:body></office:document-content>')
with zipfile.ZipFile(sys.argv[1], "w", zipfile.ZIP_DEFLATED) as z:
    z.writestr(zipfile.ZipInfo("mimetype"), "application/vnd.oasis.opendocument.spreadsheet")
    z.writestr("META-INF/manifest.xml", '<?xml version="1.0" encoding="UTF-8"?><manifest:manifest xmlns:manifest="urn:oasis:names:tc:opendocument:xmlns:manifest:1.0"/>')
    z.writestr("content.xml", content)
