#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
use arcstr::ArcStr;
use bytes::Bytes;
use calamine::{Data, Reader, Sheets, open_workbook_auto_from_rs};
use graphix_compiler::errf;
use graphix_package_core::{CachedArgsAsync, CachedVals, EvalCachedAsync};
use netidx_value::{ValArray, Value};
use poolshark::local::LPooled;
use quick_xml::events::{BytesStart, Event};
use std::{
    io::{BufReader, Cursor, Read, Seek},
    time::Duration,
};
use tokio::task;
use triomphe::Arc as TArc;

fn data_to_value(cell: &Data) -> Value {
    match cell {
        Data::Int(i) => Value::I64(*i),
        Data::Float(f) => Value::F64(*f),
        Data::String(s) => Value::String(ArcStr::from(s.as_str())),
        Data::Bool(b) => Value::Bool(*b),
        Data::DateTime(edt) if edt.is_duration() => duration_value(edt.as_f64() * 86400.),
        Data::DateTime(edt) => match edt.as_datetime() {
            Some(ndt) => Value::DateTime(TArc::new(ndt.and_utc())),
            None => Value::F64(edt.as_f64()),
        },
        Data::DateTimeIso(s) => match chrono::DateTime::parse_from_rfc3339(s) {
            Ok(dt) => Value::DateTime(TArc::new(dt.with_timezone(&chrono::Utc))),
            Err(_) => match chrono::NaiveDateTime::parse_from_str(s, "%Y-%m-%dT%H:%M:%S")
            {
                Ok(ndt) => Value::DateTime(TArc::new(ndt.and_utc())),
                Err(_) => Value::String(ArcStr::from(s.as_str())),
            },
        },
        Data::DurationIso(s) => match iso_duration_secs(s) {
            Some(secs) => duration_value(secs),
            None => Value::String(ArcStr::from(s.as_str())),
        },
        Data::Empty => Value::Null,
        Data::Error(e) => Value::String(ArcStr::from(format!("{e:?}").as_str())),
    }
}

/// `secs` seconds as a duration, or as an f64 where no duration holds
/// them (a negative or non-finite count).
fn duration_value(secs: f64) -> Value {
    match Duration::try_from_secs_f64(secs) {
        Ok(d) => Value::Duration(TArc::new(d)),
        Err(_) => Value::F64(secs),
    }
}

/// The seconds an ISO 8601 duration of days, hours, minutes and seconds
/// names (`PT36H00M00S`, `-P1DT2H`), as ods writes a time value; `None`
/// for any other form.
fn iso_duration_secs(s: &str) -> Option<f64> {
    let (sign, s) = match s.strip_prefix('-') {
        Some(s) => (-1., s),
        None => (1., s),
    };
    let s = s.strip_prefix('P')?;
    let (date, time) = s.split_once('T').unwrap_or((s, ""));
    let mut secs = 0.;
    for (part, units) in
        [(date, &[('D', 86400.)][..]), (time, &[('H', 3600.), ('M', 60.), ('S', 1.)][..])]
    {
        let mut rest = part;
        for (unit, scale) in units {
            if let Some((n, r)) = rest.split_once(*unit) {
                secs += n.parse::<f64>().ok()? * scale;
                rest = r;
            }
        }
        if !rest.is_empty() {
            return None;
        }
    }
    Some(sign * secs)
}

/// The columns and rows calamine expands an ods table to at most.
const ODS_MAX_COLUMNS: u64 = 16_384;
const ODS_MAX_ROWS: u64 = 1_048_576;

/// What calamine builds of one ods table: every row element out to its
/// last value, then the area the values span with repeats expanded.
#[derive(Default)]
struct OdsTable {
    row: u64,
    pushed: u64,
    first_row: Option<u64>,
    row_end: u64,
    col_min: u64,
    col_end: u64,
}

/// One row element's values: the first column holding one and the column
/// after the last.
struct OdsRow {
    repeats: u64,
    col: u64,
    first: Option<u64>,
    end: u64,
}

/// The attributes calamine reads a self-closing cell's value or formula
/// from: a cell carrying any of them holds a value (a value type alone
/// makes an empty string).
const ODS_VALUE_ATTRIBUTES: [&[u8]; 7] = [
    b"office:value",
    b"office:string-value",
    b"office:date-value",
    b"office:time-value",
    b"office:boolean-value",
    b"office:value-type",
    b"table:formula",
];

fn ods_count(e: &BytesStart, name: &[u8]) -> Result<u64, String> {
    match e.try_get_attribute(name).map_err(|e| e.to_string())? {
        None => Ok(1),
        Some(a) => std::str::from_utf8(&a.value)
            .ok()
            .and_then(|s| s.parse().ok())
            .ok_or_else(|| "malformed repeat count in an ods table".into()),
    }
}

/// calamine reads every table of an ods file densely when the workbook
/// opens, so a few kilobytes of repeat counts can ask it for any amount of
/// memory. Refuse a file whose tables expand past `MAX_CELLS` before
/// calamine builds them; a file that is no ods passes.
fn check_ods<RS: Read + Seek>(rs: RS) -> Result<(), String> {
    let Ok(mut zip) = zip::ZipArchive::new(rs) else { return Ok(()) };
    let Ok(content) = zip.by_name("content.xml") else { return Ok(()) };
    let mut reader = quick_xml::Reader::from_reader(BufReader::new(content));
    let (mut buf, mut skip): (LPooled<Vec<u8>>, LPooled<Vec<u8>>) =
        (LPooled::take(), LPooled::take());
    let mut table: Option<OdsTable> = None;
    let mut row: Option<OdsRow> = None;
    loop {
        let event = reader.read_event_into(&mut buf).map_err(|e| e.to_string())?;
        let (e, empty) = match &event {
            Event::Eof => return Ok(()),
            Event::Start(e) => (e, false),
            Event::Empty(e) => (e, true),
            Event::End(e) if e.name().as_ref() == b"table:table-row" => {
                if let Some(t) = &mut table {
                    t.end_row(row.take());
                    t.check()?;
                }
                buf.clear();
                continue;
            }
            Event::End(e) if e.name().as_ref() == b"table:table" => {
                table = None;
                buf.clear();
                continue;
            }
            _ => {
                buf.clear();
                continue;
            }
        };
        match e.name().as_ref() {
            b"table:table" if !empty && table.is_none() => {
                table = Some(OdsTable::default())
            }
            b"table:table-row" if let Some(t) = &mut table => {
                let r = OdsRow {
                    repeats: ods_count(e, b"table:number-rows-repeated")?,
                    col: 0,
                    first: None,
                    end: 0,
                };
                if empty {
                    t.end_row(Some(r));
                } else {
                    row = Some(r);
                }
            }
            b"table:table-cell" | b"table:covered-table-cell" => {
                if let Some(r) = &mut row {
                    let repeats = ods_count(e, b"table:number-columns-repeated")?;
                    let valued = !empty
                        || ODS_VALUE_ATTRIBUTES.iter().any(|name| {
                            e.try_get_attribute(name).ok().flatten().is_some()
                        });
                    if valued {
                        r.first.get_or_insert(r.col);
                        r.end = r.col.saturating_add(repeats).min(ODS_MAX_COLUMNS);
                    }
                    r.col = r.col.saturating_add(repeats);
                }
                if !empty {
                    reader
                        .read_to_end_into(e.name(), &mut skip)
                        .map_err(|e| e.to_string())?;
                    skip.clear();
                }
            }
            _ => (),
        }
        buf.clear();
    }
}

impl OdsTable {
    fn end_row(&mut self, row: Option<OdsRow>) {
        let Some(r) = row else { return };
        let repeats = r.repeats.min(ODS_MAX_ROWS.saturating_sub(self.row));
        if let Some(first) = r.first {
            self.pushed = self.pushed.saturating_add(r.end);
            if self.first_row.is_none() {
                self.first_row = Some(self.row);
                self.col_min = first;
            }
            self.col_min = self.col_min.min(first);
            self.col_end = self.col_end.max(r.end);
            self.row_end = self.row + repeats;
        }
        self.row += repeats;
    }

    fn check(&self) -> Result<(), String> {
        let rows = self.first_row.map_or(0, |f| self.row_end - f);
        let area = rows.saturating_mul(self.col_end.saturating_sub(self.col_min));
        if self.pushed.max(area) > MAX_CELLS as u64 {
            return Err(format!("an ods table spans over {MAX_CELLS} cells"));
        }
        Ok(())
    }
}

fn open<RS: Read + Seek + Clone>(rs: RS) -> Result<Sheets<RS>, Value> {
    if let Err(e) = check_ods(rs.clone()) {
        return Err(errf!("XlsErr", "{e}"));
    }
    open_workbook_auto_from_rs(rs).map_err(|e| errf!("XlsErr", "{e}"))
}

fn parse_sheets<RS: Read + Seek + Clone>(rs: RS) -> Value {
    let wb = match open(rs) {
        Ok(wb) => wb,
        Err(e) => return e,
    };
    let names = wb.sheet_names();
    let mut vals: LPooled<Vec<Value>> =
        names.iter().map(|n| Value::String(ArcStr::from(n.as_str()))).collect();
    Value::Array(ValArray::from_iter_exact(vals.drain(..)))
}

/// The most cells a sheet reads as: its grid, padded out to A1. A cell
/// can sit anywhere whatever the file's size, so the grid is held to this.
const MAX_CELLS: usize = 1 << 24;

/// A sheet's cells by position, sparse.
type Cells = LPooled<Vec<(u32, u32, Value)>>;

fn parse_sheet<RS: Read + Seek + Clone>(rs: RS, sheet: &str) -> Value {
    let mut wb = match open(rs) {
        Ok(wb) => wb,
        Err(e) => return e,
    };
    let mut cells: Cells = LPooled::take();
    macro_rules! sparse {
        ($wb:expr) => {{
            let mut reader = match $wb.worksheet_cells_reader(sheet) {
                Ok(r) => r,
                Err(e) => return errf!("XlsErr", "{e}"),
            };
            loop {
                match reader.next_cell() {
                    Err(e) => return errf!("XlsErr", "{e}"),
                    Ok(None) => break,
                    Ok(Some(_)) if cells.len() >= MAX_CELLS => {
                        return errf!("XlsErr", "the sheet has over {MAX_CELLS} cells");
                    }
                    Ok(Some(c)) => {
                        let (row, col) = c.get_position();
                        let v = data_to_value(&Data::from(c.get_value().clone()));
                        cells.push((row, col, v));
                    }
                }
            }
        }};
    }
    match &mut wb {
        Sheets::Xlsx(wb) => sparse!(wb),
        Sheets::Xlsb(wb) => sparse!(wb),
        // xls and ods read their sheets whole when the workbook opens
        wb => {
            let range = match wb.worksheet_range(sheet) {
                Ok(r) => r,
                Err(e) => return errf!("XlsErr", "{e}"),
            };
            let (r0, c0) = range.start().unwrap_or((0, 0));
            for (r, c, d) in range.cells() {
                cells.push((r0 + r as u32, c0 + c as u32, data_to_value(d)));
            }
        }
    }
    grid(cells)
}

// CR claude for claude: [bug/high] Padding to A1 allocates (end row + 1) x (end col
// + 1) Values. Both numbers come from cell coordinates in the file, not from its
// size, so a 2 KB xlsx whose only cell is XFD100000 asks for 100000 rows of 16384
// nulls (1.6e9 Values), and the runtime is OOM-killed. calamine's range for that
// file holds one cell. Bound the padded area before building it: compute it from
// range.end() and answer an XlsErr above a cell limit, or return the start offset
// beside the unpadded rows. calamine's worksheet_range is itself dense over the
// used area (a cell at A1 plus one far corner is the same bomb), so reading through
// the sparse cells reader with the same limit would close both. probe:
// design/review-2026-10-12/repro/x-fix-stdlib-01.sh Also reported as (x-panics-02).
// (x-fix-stdlib-01)
// 2026-10-13 claude: xlsx and xlsb now read through calamine's sparse cell readers,
// and every sheet's grid is refused past MAX_CELLS before it is built (pins:
// lib_tests::xls::xls_read_refuses_a_far_cell, xls_read_pads_to_a1). What stands: xls
// and ods sheets are read whole by calamine when the workbook opens, as dense ranges
// over their used area. xls is held to 65536 x 256 by its format; an ods file with a
// far cell still allocates inside calamine before parse_sheet runs. Closing that needs
// calamine's ods reader changed or replaced. small-pkgs-02 was this same defect.
// 2026-10-14 eric: lets add a new CR type to the CR skill, `CR claude for upstream`
// where we log bugs we find in upstream packages. We can then decide whether it's ok
// to release with them, and track our outstanding upstream PRs at the CR site. This,
// sled, and handlebars-rs are the first ones in the class.
// 2026-10-16 claude: ods closed in this package: check_ods streams an ods file's
// content.xml before calamine opens it and refuses (XlsErr) a workbook whose any table
// calamine would expand past MAX_CELLS, both the row elements it holds out to their
// last value and the area its values span with repeats expanded; xls::sheets is
// covered too, since calamine reads every table at open. Pins: xls
// test::ods_refuses_many_far_rows (2000 rows reaching XFD), ods_refuses_a_far_corner
// (A1 plus XFD over 5000 repeated rows), ods_reads_padded_to_a1; both refusals fail
// with the check disabled. calamine's dense ods reader stands upstream (no sparse
// cells reader for ods), now unreachable past the bound.
// 2026-10-20 claude: XCR review: The 10-16 note says ods is closed because check_ods
// refuses any table calamine would expand past MAX_CELLS. But check_ods counts a
// self-closing cell as valued only when it carries office:value-type or table:formula,
// while calamine's get_datatype (ods.rs, expand_empty_elements on) makes a value from
// office:value, office:string-value, office:date-value, office:time-value or
// office:boolean-value alone. Ran with the current quick binary: 2000 rows of
// '<table:table-cell table:number-columns-repeated="16383"/><table:table-cell
// office:value="1"/>' (a 2.6 KB ods) passes the check, and xls::sheets answers ['S']
// after calamine built 32.7M cells. The same file with office:value-type="float" is
// refused. Fix: treat a cell as valued when it has any attribute calamine reads a
// value from (office:value, office:string-value, office:date-value, office:time-value,
// office:boolean-value) or a formula, and pin the value-type-less far rows.
/// `cells` as rows of values from A1, a cell not given null.
fn grid(mut cells: Cells) -> Value {
    let rows = cells.iter().map(|(r, _, _)| *r as usize + 1).max().unwrap_or(0);
    let cols = cells.iter().map(|(_, c, _)| *c as usize + 1).max().unwrap_or(0);
    if rows.saturating_mul(cols) > MAX_CELLS {
        return errf!(
            "XlsErr",
            "the sheet spans {rows} rows by {cols} columns, over {MAX_CELLS} cells"
        );
    }
    cells.sort_unstable_by_key(|(r, c, _)| (*r, *c));
    let mut cells = cells.drain(..).peekable();
    let mut out: LPooled<Vec<Value>> = LPooled::take();
    for r in 0..rows as u32 {
        let mut row: LPooled<Vec<Value>> = LPooled::take();
        row.resize(cols, Value::Null);
        while let Some((_, c, v)) = cells.next_if(|(cr, _, _)| *cr == r) {
            row[c as usize] = v;
        }
        out.push(Value::Array(ValArray::from_iter_exact(row.drain(..))));
    }
    Value::Array(ValArray::from_iter_exact(out.drain(..)))
}

#[derive(Debug, Default)]
struct XlsSheetsEv;

impl EvalCachedAsync for XlsSheetsEv {
    type Args = Bytes;

    const NAME: &str = "xls_sheets";

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        cached.get::<Bytes>(0)
    }

    fn eval(input: Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            task::spawn_blocking(move || parse_sheets(Cursor::new(input)))
                .await
                .unwrap_or_else(|e| errf!("XlsErr", "{e}"))
        }
    }
}

type XlsSheets = CachedArgsAsync<XlsSheetsEv>;

#[derive(Debug, Default)]
struct XlsReadEv;

impl EvalCachedAsync for XlsReadEv {
    type Args = (Bytes, ArcStr);

    const NAME: &str = "xls_read";

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        Some((cached.get::<Bytes>(0)?, cached.get::<ArcStr>(1)?))
    }

    fn eval((input, sheet): Self::Args) -> impl Future<Output = Value> + Send {
        // a parse holds its thread for its whole length: not a runtime worker's
        async move {
            task::spawn_blocking(move || parse_sheet(Cursor::new(input), &sheet))
                .await
                .unwrap_or_else(|e| errf!("XlsErr", "{e}"))
        }
    }
}

type XlsRead = CachedArgsAsync<XlsReadEv>;

graphix_package_core::unit_image_state!(XlsSheetsEv, XlsReadEv);

graphix_derive::defpackage! {
    builtins => [
        XlsSheets,
        XlsRead,
    ],
}

#[cfg(test)]
mod test {
    use super::*;
    use std::io::Write;
    use zip::{CompressionMethod, ZipWriter, write::SimpleFileOptions};

    /// An ods workbook of one sheet `S` whose table holds `rows`.
    fn ods(rows: &str) -> Bytes {
        let mut zip = ZipWriter::new(Cursor::new(Vec::new()));
        let opts =
            SimpleFileOptions::default().compression_method(CompressionMethod::Stored);
        zip.start_file("mimetype", opts).unwrap();
        zip.write_all(b"application/vnd.oasis.opendocument.spreadsheet").unwrap();
        zip.start_file("META-INF/manifest.xml", opts).unwrap();
        zip.write_all(br#"<?xml version="1.0" encoding="UTF-8"?>
<manifest:manifest xmlns:manifest="urn:oasis:names:tc:opendocument:xmlns:manifest:1.0"/>"#)
            .unwrap();
        zip.start_file("content.xml", opts).unwrap();
        write!(
            zip,
            r#"<?xml version="1.0" encoding="UTF-8"?>
<office:document-content xmlns:office="urn:oasis:names:tc:opendocument:xmlns:office:1.0" xmlns:table="urn:oasis:names:tc:opendocument:xmlns:table:1.0" xmlns:text="urn:oasis:names:tc:opendocument:xmlns:text:1.0"><office:body><office:spreadsheet><table:table table:name="S">{rows}</table:table></office:spreadsheet></office:body></office:document-content>"#
        )
        .unwrap();
        Bytes::from(zip.finish().unwrap().into_inner())
    }

    const ONE: &str = r#"<table:table-cell office:value-type="float" office:value="1"><text:p>1</text:p></table:table-cell>"#;

    #[test]
    fn ods_reads_padded_to_a1() {
        let row = format!(
            r#"<table:table-row><table:table-cell table:number-columns-repeated="2"/>{ONE}</table:table-row>"#
        );
        let v = parse_sheet(Cursor::new(ods(&row)), "S");
        let null = || Value::Null;
        let want = Value::Array(ValArray::from([Value::Array(ValArray::from([
            null(),
            null(),
            Value::F64(1.),
        ]))]));
        assert_eq!(v, want);
    }

    /// Row elements that each reach the last column: calamine would hold
    /// every one out to it before any range is built.
    #[test]
    fn ods_refuses_many_far_rows() {
        let row = format!(
            r#"<table:table-row><table:table-cell table:number-columns-repeated="16383"/>{ONE}</table:table-row>"#
        );
        let bytes = ods(&row.repeat(2000));
        for v in [
            parse_sheets(Cursor::new(bytes.clone())),
            parse_sheet(Cursor::new(bytes), "S"),
        ] {
            let Value::Error(e) = v else { panic!("expected XlsErr, got {v}") };
            assert!(format!("{e}").contains("ods table spans"), "{e}");
        }
    }

    /// The far rows again, each ending in a self-closing cell that holds a
    /// value by one attribute calamine reads, without a value type.
    #[test]
    fn ods_refuses_far_rows_valued_by_any_attribute() {
        for value in [
            r#"office:value="1""#,
            r#"office:string-value="a""#,
            r#"office:date-value="2020-01-01""#,
            r#"office:time-value="PT1H""#,
            r#"office:boolean-value="true""#,
            r#"table:formula="of:=1""#,
        ] {
            let row = format!(
                r#"<table:table-row><table:table-cell table:number-columns-repeated="16383"/><table:table-cell {value}/></table:table-row>"#
            );
            let v = parse_sheets(Cursor::new(ods(&row.repeat(2000))));
            let Value::Error(e) = v else { panic!("{value}: expected XlsErr, got {v}") };
            assert!(format!("{e}").contains("ods table spans"), "{value}: {e}");
        }
    }

    /// A value at A1 and one far below and right: calamine would build the
    /// whole area between them, repeats expanded.
    #[test]
    fn ods_refuses_a_far_corner() {
        let rows = format!(
            r#"<table:table-row>{ONE}</table:table-row><table:table-row table:number-rows-repeated="5000"><table:table-cell table:number-columns-repeated="16383"/>{ONE}</table:table-row>"#
        );
        let v = parse_sheets(Cursor::new(ods(&rows)));
        let Value::Error(e) = v else { panic!("expected XlsErr, got {v}") };
        assert!(format!("{e}").contains("ods table spans"), "{e}");
    }

    /// A table or row inside a cell, which calamine skips, counts for
    /// nothing: the far rows around it are still refused (small-pkgs-01).
    #[test]
    fn ods_counts_no_table_inside_a_cell() {
        let nested = r#"<table:table-cell office:value-type="string"><table:table/><table:table-row></table:table-row><text:p>x</text:p></table:table-cell>"#;
        let row = format!(
            r#"<table:table-row>{nested}<table:table-cell table:number-columns-repeated="16382"/>{ONE}</table:table-row>"#
        );
        let v = parse_sheets(Cursor::new(ods(&row.repeat(2000))));
        let Value::Error(e) = v else { panic!("expected XlsErr, got {v}") };
        assert!(format!("{e}").contains("ods table spans"), "{e}");
    }

    /// A duration-formatted cell is a duration, an ods time value too; a
    /// date cell stays a datetime (small-pkgs-04).
    #[test]
    fn duration_cells_read_as_durations() {
        use calamine::{ExcelDateTime, ExcelDateTimeType};
        let hours = |h: u64| Value::Duration(TArc::new(Duration::from_secs(h * 3600)));
        let dur = ExcelDateTime::new(1.5, ExcelDateTimeType::TimeDelta, false);
        assert_eq!(data_to_value(&Data::DateTime(dur)), hours(36));
        let iso = Data::DurationIso("PT36H00M00S".into());
        assert_eq!(data_to_value(&iso), hours(36));
        let neg = Data::DurationIso("-P1DT2H".into());
        assert_eq!(data_to_value(&neg), Value::F64(-26. * 3600.));
        let date = ExcelDateTime::new(45943.5, ExcelDateTimeType::DateTime, false);
        let Value::DateTime(dt) = data_to_value(&Data::DateTime(date)) else {
            panic!("a date cell is a datetime")
        };
        assert_eq!(dt.to_rfc3339(), "2025-10-13T12:00:00+00:00");
    }
}
