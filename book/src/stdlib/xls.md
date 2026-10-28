# xls

The `xls` module reads spreadsheet files in xlsx, xls, ods, and xlsb
formats (via calamine). A sheet is returned as a 2D array of `Cell`s, so
a select over a cell needs an arm for each of its seven kinds and no more.

```graphix
{{#include ../../../stdlib/graphix-package-xls/src/graphix/mod.gxi}}
```

## Memory when opening xls files

Both `xls::sheets` and `xls::read` open every sheet of a BIFF `.xls`
workbook as a dense rectangle over its used cells. A small sparse file can
therefore require much more memory than its file size. Graphix's cell limit
is checked after this allocation and does not bound the cost of opening
`.xls`. An allocation failure can terminate the process instead of returning
`XlsErr`.

Read only trusted `.xls` files in the Graphix process, or use a separate
process with an enforced memory limit for untrusted files. Graphix has a
preflight size check for `.ods`; that check does not cover `.xls`.

## Example

```graphix

let data = sys::fs::read_all_bin("report.xlsx")?;
let names = xls::sheets(data)?;
let rows = xls::read(data, names[0]$)?;
```
