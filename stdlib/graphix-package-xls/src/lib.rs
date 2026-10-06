#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
use arcstr::ArcStr;
use bytes::Bytes;
use calamine::{Data, Reader, open_workbook_auto_from_rs};
use graphix_compiler::errf;
use graphix_package_core::{CachedArgsAsync, CachedVals, EvalCachedAsync};
use netidx_value::{ValArray, Value};
use poolshark::local::LPooled;
use std::io::Cursor;
use triomphe::Arc as TArc;

fn data_to_value(cell: &Data) -> Value {
    match cell {
        Data::Int(i) => Value::I64(*i),
        Data::Float(f) => Value::F64(*f),
        Data::String(s) => Value::String(ArcStr::from(s.as_str())),
        Data::Bool(b) => Value::Bool(*b),
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
        Data::DurationIso(s) => Value::String(ArcStr::from(s.as_str())),
        Data::Empty => Value::Null,
        Data::Error(e) => Value::String(ArcStr::from(format!("{e:?}").as_str())),
    }
}

fn parse_sheets<RS: std::io::Read + std::io::Seek + Clone>(rs: RS) -> Value {
    let wb = match open_workbook_auto_from_rs(rs) {
        Ok(wb) => wb,
        Err(e) => return errf!("XlsErr", "{e}"),
    };
    let names = wb.sheet_names();
    let mut vals: LPooled<Vec<Value>> =
        names.iter().map(|n| Value::String(ArcStr::from(n.as_str()))).collect();
    Value::Array(ValArray::from_iter_exact(vals.drain(..)))
}

fn parse_sheet<RS: std::io::Read + std::io::Seek + Clone>(rs: RS, sheet: &str) -> Value {
    let mut wb = match open_workbook_auto_from_rs(rs) {
        Ok(wb) => wb,
        Err(e) => return errf!("XlsErr", "{e}"),
    };
    let range = match wb.worksheet_range(sheet) {
        Ok(r) => r,
        Err(e) => return errf!("XlsErr", "{e}"),
    };
    let mut rows: LPooled<Vec<Value>> = LPooled::take();
    // CR claude for claude: [bug] calamine's worksheet_range covers only the non-empty
    // cells (Range::from_sparse for xlsx/xls/xlsb, get_range for ods), and
    // range.start() is dropped here. So rows[0][0] is the first used row and the first
    // used column of the whole sheet, not A1, and the caller cannot learn the offset. A
    // sheet with B3=1, C3=2, B4=3 reads as [[1, 2], [3, null]]. Adding "note" in A6
    // makes it [[null, 1, 2], [null, 3, null], [null, null, null], ["note", null,
    // null]], so an edit to an unrelated empty cell shifts every column. Pad with
    // start.0 empty rows and start.1 leading nulls (or return the offset), and say
    // which in mod.gxi. probe: design/review-2026-10-05/repro/small-pkgs-15.sh
    // (small-pkgs-15)
    for row in range.rows() {
        let mut cells: LPooled<Vec<Value>> = row.iter().map(data_to_value).collect();
        rows.push(Value::Array(ValArray::from_iter_exact(cells.drain(..))));
    }
    Value::Array(ValArray::from_iter_exact(rows.drain(..)))
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
        async move { parse_sheets(Cursor::new(input)) }
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
        async move { parse_sheet(Cursor::new(input), &sheet) }
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
