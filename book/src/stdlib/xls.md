# xls

The `xls` module reads spreadsheet files in xlsx, xls, ods, and xlsb
formats (via calamine). A sheet is returned as a 2D array of `Cell`s, so
a select over a cell needs an arm for each of its seven kinds and no more.

```graphix
{{#include ../../../stdlib/graphix-package-xls/src/graphix/mod.gxi}}
```

## Example

```graphix

let data = sys::fs::read_all_bin("report.xlsx")?;
let names = xls::sheets(data)?;
let rows = xls::read(data, names[0]$)?;
```
