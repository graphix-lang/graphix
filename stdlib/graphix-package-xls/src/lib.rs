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

/// The most cells a sheet reads as: its grid, padded out to A1. A cell
/// can sit anywhere whatever the file's size, so the grid is held to this.
const MAX_CELLS: usize = 1 << 24;

/// A sheet's cells by position, sparse.
type Cells = LPooled<Vec<(u32, u32, Value)>>;

fn parse_sheet<RS: std::io::Read + std::io::Seek + Clone>(rs: RS, sheet: &str) -> Value {
    let mut wb = match open_workbook_auto_from_rs(rs) {
        Ok(wb) => wb,
        Err(e) => return errf!("XlsErr", "{e}"),
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
        // CR claude for claude: [perf/low] parse_sheet runs inside the async block, on
        // whichever tokio worker the JoinSet task lands on. Inflating and parsing a
        // multi-MB workbook holds that worker for the whole parse, stalling the netidx
        // IO and timers queued there. Fix: run it under `tokio::task::spawn_blocking`
        // and turn a JoinError into an XlsErr, as graphix-package-sqlite does. Do the
        // same for parse_sheets in XlsSheetsEv::eval. (small-pkgs-13)
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
