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

// ── Cell conversion ──────────────────────────────────────────

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

// ── Shared parsing core ──────────────────────────────────────

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
    for row in range.rows() {
        let mut cells: LPooled<Vec<Value>> = row.iter().map(data_to_value).collect();
        rows.push(Value::Array(ValArray::from_iter_exact(cells.drain(..))));
    }
    Value::Array(ValArray::from_iter_exact(rows.drain(..)))
}

// ── XlsSheets (async) ───────────────────────────────────────

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

// ── XlsRead (async) ─────────────────────────────────────────

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

// ── Package registration ─────────────────────────────────────

graphix_derive::defpackage! {
    builtins => [
        XlsSheets,
        XlsRead,
    ],
}
