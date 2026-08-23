# xls

The `xls` module reads spreadsheet files in xlsx, xls, ods, and xlsb
formats (via calamine). Data is returned as a 2D array of primitive
values.

```graphix
{{#include ../../../stdlib/graphix-package-xls/src/graphix/mod.gxi}}
```

## Example

```graphix

let data = sys::fs::read_all_bin("report.xlsx")?;
let names = xls::sheets(data)?;
let rows = xls::read(data, names[0]$)?;
```
