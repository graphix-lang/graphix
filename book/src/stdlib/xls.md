# xls

The `xls` module reads spreadsheet files in xlsx, xls, ods, and xlsb
formats (via calamine). Data is returned as a 2D array of primitive
values.

```graphix
use sys::io::Stream;

/// List sheet names in a workbook.
val sheets: fn(input: [bytes, Stream<'a>]) -> Result<Array<string>, [`XlsErr(string), `IOErr(string)]>;

/// Read a sheet by name as a 2D array of rows.
val read: fn(input: [bytes, Stream<'a>], sheet: string) -> Result<Array<Array<PrimNoErr>>, [`XlsErr(string), `IOErr(string)]>;
```

## Example

```graphix

let data = sys::fs::read_all_bin("report.xlsx")?;
let names = xls::sheets(data)?;
let rows = xls::read(data, names[0]$)?;
```
