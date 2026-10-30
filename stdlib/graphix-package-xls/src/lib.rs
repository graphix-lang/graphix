#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
use arcstr::ArcStr;
use bytes::Bytes;
use calamine::{Data, Reader, Sheets, open_workbook_auto_from_rs};
use chrono::{DateTime, NaiveDate, NaiveDateTime, NaiveTime, Utc};
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
use zip::ZipArchive;

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
        Data::DateTimeIso(s) => match iso_datetime(s) {
            Some(dt) => Value::DateTime(TArc::new(dt)),
            None => Value::String(ArcStr::from(s.as_str())),
        },
        Data::DurationIso(s) => match iso_duration_secs(s) {
            Some(secs) => duration_value(secs),
            None => Value::String(ArcStr::from(s.as_str())),
        },
        Data::Empty => Value::Null,
        Data::Error(e) => Value::String(ArcStr::from(format!("{e:?}").as_str())),
    }
}

/// An ISO 8601 date-time cell as an instant: with an offset, else local
/// time read as UTC (fractional seconds optional), else a date alone at
/// midnight UTC.
fn iso_datetime(s: &str) -> Option<DateTime<Utc>> {
    DateTime::parse_from_rfc3339(s)
        .map(|dt| dt.with_timezone(&Utc))
        .or_else(|_| {
            NaiveDateTime::parse_from_str(s, "%Y-%m-%dT%H:%M:%S%.f").map(|t| t.and_utc())
        })
        .or_else(|_| {
            NaiveDate::parse_from_str(s, "%Y-%m-%d")
                .map(|d| d.and_time(NaiveTime::MIN).and_utc())
        })
        .ok()
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
/// memory. Refuse a file whose tables together expand past `MAX_CELLS`
/// before calamine builds them; a zip that is no ods passes.
fn check_ods<RS: Read + Seek>(zip: &mut ZipArchive<RS>) -> Result<(), String> {
    let Ok(content) = zip.by_name("content.xml") else { return Ok(()) };
    let mut reader = quick_xml::Reader::from_reader(BufReader::new(content));
    let (mut buf, mut skip): (LPooled<Vec<u8>>, LPooled<Vec<u8>>) =
        (LPooled::take(), LPooled::take());
    let mut table: Option<OdsTable> = None;
    let mut row: Option<OdsRow> = None;
    let mut done: u64 = 0;
    loop {
        let event = reader.read_event_into(&mut buf).map_err(|e| e.to_string())?;
        let (e, empty) = match &event {
            Event::Eof => return Ok(()),
            Event::Start(e) => (e, false),
            Event::Empty(e) => (e, true),
            Event::End(e) if e.name().as_ref() == b"table:table-row" => {
                if let Some(t) = &mut table {
                    t.end_row(row.take());
                    if done.saturating_add(t.cells()) > MAX_CELLS as u64 {
                        return Err(format!(
                            "the ods tables span over {MAX_CELLS} cells"
                        ));
                    }
                }
                buf.clear();
                continue;
            }
            Event::End(e) if e.name().as_ref() == b"table:table" => {
                if let Some(t) = table.take() {
                    done = done.saturating_add(t.cells());
                }
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

    /// The cells calamine holds for the table at most: its rows as pushed,
    /// or the range built from them.
    fn cells(&self) -> u64 {
        let rows = self.first_row.map_or(0, |f| self.row_end - f);
        let area = rows.saturating_mul(self.col_end.saturating_sub(self.col_min));
        self.pushed.max(area)
    }
}

/// Whether a zip entry is the one calamine reads as `path`, which it looks
/// up ignoring ascii case and the separator.
fn is_zip_path(name: &str, path: &str) -> bool {
    name.len() == path.len()
        && name
            .bytes()
            .zip(path.bytes())
            .all(|(n, p)| n.eq_ignore_ascii_case(&p) || (n == b'\\' && p == b'/'))
}

/// The `uniqueCount` calamine reads off an `sst` element: the first
/// attribute so named, written as digits, and none past a malformed one.
fn sst_unique_count(e: &BytesStart) -> Option<u64> {
    for a in e.attributes() {
        let a = a.ok()?;
        if a.key.as_ref() == b"uniqueCount" {
            return std::str::from_utf8(&a.value).ok()?.parse().ok();
        }
    }
    None
}

/// calamine's xlsx open reserves room for as many shared strings as the
/// `sst` element declares, and builds a string for each `si` element it
/// meets, before any cell bound applies; an allocation that cannot be met
/// aborts the process. It meets them nested in a string or another element
/// and past an end of the sst element inside a string, so every `si`
/// element after the sst element's start counts, up to where the reader
/// fails: refuse more of them than `MAX_CELLS`, and a count past them. A
/// zip without the part passes.
fn check_shared_strings<RS: Read + Seek>(zip: &mut ZipArchive<RS>) -> Result<(), String> {
    'parts: for i in 0..zip.len() {
        let Ok(part) = zip.by_index(i) else { continue };
        if !is_zip_path(part.name(), "xl/sharedStrings.xml") {
            continue;
        }
        let mut reader = quick_xml::Reader::from_reader(BufReader::new(part));
        let config = reader.config_mut();
        config.check_end_names = false;
        config.expand_empty_elements = true;
        let mut buf: LPooled<Vec<u8>> = LPooled::take();
        let declared = loop {
            match reader.read_event_into(&mut buf) {
                Ok(Event::Start(e)) if e.local_name().as_ref() == b"sst" => {
                    break sst_unique_count(&e);
                }
                Ok(Event::Eof) | Err(_) => continue 'parts,
                Ok(_) => buf.clear(),
            }
        };
        let mut held: u64 = 0;
        loop {
            buf.clear();
            match reader.read_event_into(&mut buf) {
                Ok(Event::Start(e)) if e.local_name().as_ref() == b"si" => {
                    held += 1;
                    if held > MAX_CELLS as u64 {
                        return Err(format!(
                            "the shared strings part holds over {MAX_CELLS} strings"
                        ));
                    }
                }
                Ok(Event::Eof) | Err(_) => break,
                Ok(_) => (),
            }
        }
        if let Some(declared) = declared
            && declared > held
        {
            return Err(format!(
                "the shared strings part declares {declared} strings and holds {held}"
            ));
        }
    }
    Ok(())
}

fn open<RS: Read + Seek + Clone>(rs: RS) -> Result<Sheets<RS>, Value> {
    if let Ok(mut zip) = ZipArchive::new(rs.clone())
        && let Err(e) = check_ods(&mut zip).and_then(|()| check_shared_strings(&mut zip))
    {
        return Err(errf!("XlsErr", "{e}"));
    }
    // XCR claude for claude: [bug/high] calamine's xlsx open reserves the strings vector
    // to sharedStrings.xml's uniqueCount attribute, unchecked. A 1 KB xlsx declaring
    // uniqueCount="100000000000" makes xls::read fail to allocate 2.4 TB and abort the
    // process; spawn_blocking cannot contain an abort. Guard xlsx as check_ods guards
    // ods: stream xl/sharedStrings.xml first and refuse a uniqueCount larger than the
    // entry's uncompressed size could hold (or past a bound tied to MAX_CELLS). Add the
    // calamine entry to UPSTREAM.md. probe:
    // design/review-2026-10-26/repro/small-pkgs-xlsx-sst-abort.sh (small-pkgs-02)
    // 2026-10-26 claude: open now runs check_shared_strings beside check_ods over one
    // ZipArchive: every entry calamine would take for xl/sharedStrings.xml (ascii case
    // and backslash ignored, as build_zip_path_cache does) is streamed with calamine's
    // reader config, the first sst element's uniqueCount read as calamine's
    // get_attribute does, and a count past the bytes the part holds is refused.
    // UPSTREAM.md has the calamine entry. Pins: graphix-package-xls tests
    // xlsx_refuses_an_overstated_string_count (aborts with the check undone, for each
    // path spelling) and xlsx_reads_shared_strings.
    // 2026-10-27 claude: The bound is still attacker-scalable. check_shared_strings
    // accepts uniqueCount up to the part's uncompressed byte count, and calamine's
    // `self.strings.reserve(n)` (xlsx/mod.rs) takes 24 bytes per declared string. A
    // deflate bomb of whitespace inside sharedStrings.xml, about 3 MB of xlsx expanding
    // to 3 GB, therefore passes the check and asks calamine to reserve ~72 GB, which
    // still aborts the process on a normal host. The whitespace itself would cost
    // calamine nothing. The check already streams the whole part, so count the `<si>`
    // elements there and refuse a declared count above that number; the reservation is
    // then bounded by strings calamine builds anyway. Alternatively, cap the count at a
    // fixed bound tied to MAX_CELLS. Pin a small, highly compressed part whose byte
    // count exceeds its declared count but whose `<si>` count does not.
    // 2026-10-27 claude: check_shared_strings now counts the part's si elements as it
    // streams it and refuses a declared uniqueCount above that count, so whitespace
    // padding no longer raises the bound (the reservation calamine makes is at most the
    // strings the part holds). Pin: test::xlsx_refuses_a_count_its_padding_would_cover
    // (1000 declared, one string, 4 KB of spaces; the byte bound let it through).
    // 2026-10-28 claude: Review (2026-10-28): check_shared_strings counts every <si>
    // start, including <si/> after </sst> (which calamine never reads, its loop ends at
    // End(sst)) and nested ones, so <si/> padding (5 bytes each, ~1000:1 deflate) still
    // raises the bound: uniqueCount=1000 with one string and 999 <si/> after </sst>
    // passes and xls::read returns [["hi"]]; scaled to ~30 MB it would reserve over 100
    // GB. Count only top-level si before </sst>, or cap at MAX_CELLS. probe
    // ~/tmp/xcr-review-2026-10-28/5/x/ (mk.py, after.xlsx, r.gx).
    // xlsx_refuses_a_count_its_padding_would_cover covers whitespace only. UPSTREAM.md's
    // xlsx entry still describes the old byte check.
    // 2026-10-29 claude: check_shared_strings counts only the sst element's own si
    // children and stops at its end, where calamine stops, so si elements past </sst> or
    // nested in one no longer raise the bound; and it refuses a part holding more than
    // MAX_CELLS strings, since calamine builds every string the sst holds at open,
    // before any cell bound (<si/> padding inside the sst costs calamine as much as the
    // reservation did). Pin: test::xlsx_counts_only_the_strings_calamine_reads (1000
    // declared, one string, 999 <si/> after </sst>, or 998 nested in one: both passed
    // the old count). The MAX_CELLS cap is unpinned (16M strings). UPSTREAM.md updated.
    // 2026-10-29 claude: Review (2026-10-29, cr29 R2): check_shared_strings skipped any
    // part whose sst has no parseable uniqueCount (let Some(declared) = declared else {
    // continue }), so the MAX_CELLS cap did not apply there while calamine still builds
    // every si: ~/tmp/cr29/scratch/review/r2/none20m.xlsx (147 KB, 20M <si/>, no
    // uniqueCount, from mkxlsx.py) reads and needs over 300 MB; a few MB would grow a
    // Vec of ~24 GB. The MAX_CELLS cap was unpinned. Also found while fixing: the
    // depth-0 count stopped short of calamine, which builds a string for each si its
    // loop meets, nested in an si or in another element, and past an </sst> inside an si
    // (probe: a sheet cell indexing past the depth-0 count reads); with uniqueCount="1"
    // each shape passed the old check and calamine built 16.7M strings.
    // 2026-10-29 claude: check_shared_strings counts every si element after the sst
    // element's start, at any depth and to where the reader fails (an upper bound of the
    // strings calamine builds: its loop meets si nested in an si or another element and
    // past an </sst> inside an si), whether or not a count is declared or parses; it
    // refuses more than MAX_CELLS of them as it counts, and a declared count past them.
    // Pin: test::xlsx_refuses_more_strings_than_the_cell_bound (MAX_CELLS+1 strings with
    // no count, an unparseable count, and uniqueCount=1 nested in an si, wrapped in
    // another element, or after an </sst> inside an si); each of its five cases fails on
    // the old check (four read the sheet after calamine built 16.7M strings, the fifth
    // fails only after building them). test::xlsx_counts_only_the_strings_calamine_reads
    // is gone: its premise was wrong (calamine reads 999 strings from its nested case),
    // and si past the true </sst> now count, which lets a declared count reserve at most
    // the si elements the part holds, under the cap. UPSTREAM.md updated.
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

    #[test]
    fn xls_sparse_cells_span_the_full_bounding_box() {
        let bytes = Bytes::from_static(include_bytes!("../tests/data/sparse.xls"));
        assert_eq!(bytes.len(), 5632);
        let mut workbook = open(Cursor::new(bytes)).unwrap();
        assert_eq!(workbook.sheet_names(), &["S0"]);
        let range = workbook.worksheet_range("S0").unwrap();
        assert_eq!(range.get_size(), (256, 256));
        assert_eq!(range.cells().count(), 65536);
        assert_eq!(range.used_cells().count(), 2);
        assert_eq!(range.get_value((0, 0)), Some(&Data::Float(1.0)));
        assert_eq!(range.get_value((255, 255)), Some(&Data::Float(2.0)));
    }

    /// A zip of `files`, each a name and its contents, stored.
    fn zip_of(files: &[(&str, &str)]) -> Bytes {
        let mut zip = ZipWriter::new(Cursor::new(Vec::new()));
        let opts =
            SimpleFileOptions::default().compression_method(CompressionMethod::Stored);
        for (name, contents) in files {
            zip.start_file(*name, opts).unwrap();
            zip.write_all(contents.as_bytes()).unwrap();
        }
        Bytes::from(zip.finish().unwrap().into_inner())
    }

    /// An ods workbook whose spreadsheet holds `tables`.
    fn ods_of(tables: &str) -> Bytes {
        let content = format!(
            r#"<?xml version="1.0" encoding="UTF-8"?>
<office:document-content xmlns:office="urn:oasis:names:tc:opendocument:xmlns:office:1.0" xmlns:table="urn:oasis:names:tc:opendocument:xmlns:table:1.0" xmlns:text="urn:oasis:names:tc:opendocument:xmlns:text:1.0"><office:body><office:spreadsheet>{tables}</office:spreadsheet></office:body></office:document-content>"#
        );
        zip_of(&[
            ("mimetype", "application/vnd.oasis.opendocument.spreadsheet"),
            (
                "META-INF/manifest.xml",
                r#"<?xml version="1.0" encoding="UTF-8"?>
<manifest:manifest xmlns:manifest="urn:oasis:names:tc:opendocument:xmlns:manifest:1.0"/>"#,
            ),
            ("content.xml", &content),
        ])
    }

    /// An ods workbook of one sheet `S` whose table holds `rows`.
    fn ods(rows: &str) -> Bytes {
        ods_of(&format!(r#"<table:table table:name="S">{rows}</table:table>"#))
    }

    /// An xlsx workbook of one sheet `S` whose A1 is the shared string
    /// `hi`, at `path` in a table that declares `unique` strings.
    fn xlsx(path: &str, unique: &str) -> Bytes {
        xlsx_padded(path, Some(unique), "")
    }

    /// [`xlsx`] whose table declares `unique` strings, or no count, and
    /// holds `inside` after its one string.
    fn xlsx_padded(path: &str, unique: Option<&str>, inside: &str) -> Bytes {
        let unique = unique.map_or(String::new(), |u| format!(r#" uniqueCount="{u}""#));
        let sst = format!(
            r#"<?xml version="1.0" encoding="UTF-8"?><sst xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main" count="1"{unique}><si><t>hi</t></si>{inside}</sst>"#
        );
        zip_of(&[
            (
                "[Content_Types].xml",
                r#"<?xml version="1.0" encoding="UTF-8"?><Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types"><Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/><Default Extension="xml" ContentType="application/xml"/><Override PartName="/xl/workbook.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml"/><Override PartName="/xl/worksheets/sheet1.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml"/><Override PartName="/xl/sharedStrings.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sharedStrings+xml"/></Types>"#,
            ),
            (
                "_rels/.rels",
                r#"<?xml version="1.0" encoding="UTF-8"?><Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships"><Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument" Target="xl/workbook.xml"/></Relationships>"#,
            ),
            (
                "xl/workbook.xml",
                r#"<?xml version="1.0" encoding="UTF-8"?><workbook xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"><sheets><sheet name="S" sheetId="1" r:id="rId1"/></sheets></workbook>"#,
            ),
            (
                "xl/_rels/workbook.xml.rels",
                r#"<?xml version="1.0" encoding="UTF-8"?><Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships"><Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet" Target="worksheets/sheet1.xml"/><Relationship Id="rId2" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/sharedStrings" Target="sharedStrings.xml"/></Relationships>"#,
            ),
            (path, &sst),
            (
                "xl/worksheets/sheet1.xml",
                r#"<?xml version="1.0" encoding="UTF-8"?><worksheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main"><sheetData><row r="1"><c r="A1" t="s"><v>0</v></c></row></sheetData></worksheet>"#,
            ),
        ])
    }

    /// An ods date cell reads as a datetime whether it holds a date
    /// alone, a date-time, or fractional seconds.
    #[test]
    fn ods_date_cells_read_as_datetimes() {
        let cells: String = ["2020-01-01", "2020-01-01T10:00:00", "2020-01-01T10:00:00.5"]
            .iter()
            .map(|d| {
                format!(
                    r#"<table:table-cell office:value-type="date" office:date-value="{d}"><text:p>x</text:p></table:table-cell>"#
                )
            })
            .collect();
        let v = parse_sheet(
            Cursor::new(ods(&format!("<table:table-row>{cells}</table:table-row>"))),
            "S",
        );
        let at = |h, ms| {
            let d = NaiveDate::from_ymd_opt(2020, 1, 1).unwrap();
            Value::DateTime(TArc::new(
                d.and_hms_milli_opt(h, 0, 0, ms).unwrap().and_utc(),
            ))
        };
        let row = ValArray::from([at(0, 0), at(10, 0), at(10, 500)]);
        assert_eq!(v, Value::Array(ValArray::from([Value::Array(row)])));
    }

    #[test]
    fn xlsx_reads_shared_strings() {
        let v = parse_sheet(Cursor::new(xlsx("xl/sharedStrings.xml", "1")), "S");
        let hi = Value::String(arcstr::literal!("hi"));
        assert_eq!(v, Value::Array(ValArray::from([Value::Array(ValArray::from([hi]))])));
    }

    /// A shared strings table declaring more strings than its bytes could
    /// hold, under every name calamine finds it by: calamine would reserve
    /// room for them all and abort.
    #[test]
    fn xlsx_refuses_an_overstated_string_count() {
        for path in
            ["xl/sharedStrings.xml", "XL/SHAREDSTRINGS.XML", "xl\\sharedStrings.xml"]
        {
            let bytes = xlsx(path, "100000000000");
            for v in [
                parse_sheets(Cursor::new(bytes.clone())),
                parse_sheet(Cursor::new(bytes.clone()), "S"),
            ] {
                let Value::Error(e) = v else {
                    panic!("{path}: expected XlsErr, got {v}")
                };
                assert!(format!("{e}").contains("declares 100000000000 strings"), "{e}");
            }
        }
    }

    /// A count past the table's strings is refused however many bytes pad
    /// the table: whitespace inflates the bytes, not the strings.
    #[test]
    fn xlsx_refuses_a_count_its_padding_would_cover() {
        let v = parse_sheet(
            Cursor::new(xlsx_padded(
                "xl/sharedStrings.xml",
                Some("1000"),
                &" ".repeat(4096),
            )),
            "S",
        );
        let Value::Error(e) = v else { panic!("expected XlsErr, got {v}") };
        assert!(format!("{e}").contains("declares 1000 strings and holds 1"), "{e}");
    }

    /// More strings than `MAX_CELLS` are refused whether the table declares
    /// a count or not, and wherever they sit: calamine builds one for each
    /// string element it meets, nested in a string or another element, and
    /// past an end of the sst element inside a string.
    #[test]
    fn xlsx_refuses_more_strings_than_the_cell_bound() {
        let many = "<si/>".repeat(MAX_CELLS);
        for (unique, inside) in [
            (None, many.clone()),
            (Some("many"), many.clone()),
            (Some("1"), format!("<si>{many}</si>")),
            (Some("1"), format!("<x>{many}</x>")),
            (Some("1"), format!("<si></sst></si>{many}")),
        ] {
            let case = format!("{unique:?} {}..", &inside[..15]);
            let bytes = xlsx_padded("xl/sharedStrings.xml", unique, &inside);
            let v = parse_sheets(Cursor::new(bytes));
            let Value::Error(e) = v else { panic!("{case}: expected XlsErr, got {v}") };
            let e = format!("{e}");
            assert!(
                e.contains(&format!("holds over {MAX_CELLS} strings")),
                "{case}: {e}"
            );
        }
    }

    /// Two tables each under the bound whose sum is over it: calamine
    /// holds every table of the workbook at once.
    #[test]
    fn ods_bounds_the_tables_together() {
        let table = |name: &str| {
            format!(
                r#"<table:table table:name="{name}"><table:table-row>{ONE}</table:table-row><table:table-row table:number-rows-repeated="2999"><table:table-cell table:number-columns-repeated="2999"/>{ONE}</table:table-row></table:table>"#
            )
        };
        let one = ods_of(&table("A"));
        let Value::Array(names) = parse_sheets(Cursor::new(one)) else {
            panic!("one table is under the bound")
        };
        assert_eq!(&names[..], &[Value::String(arcstr::literal!("A"))]);
        let two = ods_of(&format!("{}{}", table("A"), table("B")));
        let v = parse_sheets(Cursor::new(two));
        let Value::Error(e) = v else { panic!("expected XlsErr, got {v}") };
        assert!(format!("{e}").contains("ods tables span"), "{e}");
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
            assert!(format!("{e}").contains("ods tables span"), "{e}");
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
            assert!(format!("{e}").contains("ods tables span"), "{value}: {e}");
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
        assert!(format!("{e}").contains("ods tables span"), "{e}");
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
        assert!(format!("{e}").contains("ods tables span"), "{e}");
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
