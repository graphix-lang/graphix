#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
use arcstr::ArcStr;
use bytes::Bytes;
use calamine::{open_workbook_auto_from_rs, Data, Reader};
use graphix_compiler::errf;
use graphix_package_core::{CachedArgsAsync, CachedVals, EvalCachedAsync};
use graphix_package_sys::{get_stream, StreamKind};
use netidx_value::{ValArray, Value};
use poolshark::local::LPooled;
use std::io::Cursor;
use std::sync::Arc;
use tokio::{io::AsyncReadExt, sync::Mutex};
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

// ── ReadInput ────────────────────────────────────────────────

#[derive(Debug)]
enum ReadInput {
    Bytes(Bytes),
    Stream(Arc<Mutex<Option<StreamKind>>>),
}

// ── XlsSheets (async) ───────────────────────────────────────

#[derive(Debug, Default)]
struct XlsSheetsEv;

impl EvalCachedAsync for XlsSheetsEv {
    const NAME: &str = "xls_sheets";
    const NEEDS_CALLSITE: bool = false;
    type Args = ReadInput;

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        let v = cached.0.first()?.as_ref()?;
        match v {
            Value::Bytes(b) => Some(ReadInput::Bytes((**b).clone())),
            Value::Abstract(_) => Some(ReadInput::Stream(get_stream(cached, 0)?)),
            _ => None,
        }
    }

    fn eval(input: Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            match input {
                ReadInput::Bytes(b) => {
                    let cursor = Cursor::new(b);
                    parse_sheets(cursor)
                }
                ReadInput::Stream(stream) => {
                    let mut guard = stream.lock().await;
                    let s = match guard.as_mut() {
                        Some(s) => s,
                        None => return errf!("IOErr", "stream unavailable"),
                    };
                    let mut buf = Vec::new();
                    if let Err(e) = s.read_to_end(&mut buf).await {
                        return errf!("IOErr", "read failed: {e}");
                    }
                    parse_sheets(Cursor::new(buf))
                }
            }
        }
    }
}

type XlsSheets = CachedArgsAsync<XlsSheetsEv>;

// ── XlsRead (async) ─────────────────────────────────────────

#[derive(Debug, Default)]
struct XlsReadEv;

impl EvalCachedAsync for XlsReadEv {
    const NAME: &str = "xls_read";
    const NEEDS_CALLSITE: bool = false;
    type Args = (ReadInput, ArcStr);

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        let v = cached.0.first()?.as_ref()?;
        let input = match v {
            Value::Bytes(b) => ReadInput::Bytes((**b).clone()),
            Value::Abstract(_) => ReadInput::Stream(get_stream(cached, 0)?),
            _ => return None,
        };
        let sheet = cached.get::<ArcStr>(1)?;
        Some((input, sheet))
    }

    fn eval((input, sheet): Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            match input {
                ReadInput::Bytes(b) => {
                    let cursor = Cursor::new(b);
                    parse_sheet(cursor, &sheet)
                }
                ReadInput::Stream(stream) => {
                    let mut guard = stream.lock().await;
                    let s = match guard.as_mut() {
                        Some(s) => s,
                        None => return errf!("IOErr", "stream unavailable"),
                    };
                    let mut buf = Vec::new();
                    if let Err(e) = s.read_to_end(&mut buf).await {
                        return errf!("IOErr", "read failed: {e}");
                    }
                    parse_sheet(Cursor::new(buf), &sheet)
                }
            }
        }
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
