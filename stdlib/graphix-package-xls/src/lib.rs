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
    // the range covers only the used cells: pad it out to A1
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
    // CR claude for claude: [bug/high] Padding the used range out to A1 turns a tiny
    // file into an unbounded allocation. A 1497-byte xlsx whose only cell is XFD100000
    // makes this build 100000 rows of 16384 nulls, about 1.6e9 Values, each leading row
    // as its own ValArray, and the run is OOM-killed at 6 GB. The input is untrusted,
    // so the result size must be bounded by something tied to the input. Building the
    // blank row once and cloning it does not fix this alone. calamine's Range is itself
    // dense over the used range: a file with cells at A1 and XFD200000 OOMs inside
    // `worksheet_range` with no padding at all. The fix needs a size check before the
    // dense range is built. Either read the sheet's dimensions or cells through
    // calamine's sparse cell reader and refuse with `XlsErr` past a cap, or return the
    // cells sparsely or with the range origin rather than as a padded grid (a breaking
    // change; pin it in the xls lib tests). probe:
    // design/review-2026-10-12/repro/small-pkgs-02.sh (small-pkgs-02)
    let (r0, c0) = range.start().map_or((0, 0), |(r, c)| (r as usize, c as usize));
    let width = c0 + range.width();
    let mut rows: LPooled<Vec<Value>> = LPooled::take();
    for _ in 0..r0 {
        rows.push(Value::Array(ValArray::from_iter_exact(std::iter::repeat_n(
            Value::Null,
            width,
        ))));
    }
    for row in range.rows() {
        let mut cells: LPooled<Vec<Value>> = std::iter::repeat_n(Value::Null, c0)
            .chain(row.iter().map(data_to_value))
            .collect();
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
