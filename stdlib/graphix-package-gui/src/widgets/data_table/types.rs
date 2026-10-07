//! Pure data types, parsers, and small helpers for the data table.

use super::{
    CELL_H_PADDING, MAX_SPARKLINE_POINTS, MIN_COL_WIDTH, RESIZE_HANDLE_WIDTH,
    ROW_NAME_KEY, VALUE_COL_KEY,
};
use ahash::{AHashMap, AHashSet};
use arcstr::ArcStr;
use compact_str::{CompactString, format_compact};
use graphix_rt::{Callable, GXExt, Ref};
use log::warn;
use netidx::{path::Path, publisher::Value};
use netidx_derive::FromValue;
use poolshark::local::LPooled;
use std::{
    borrow::Cow,
    collections::VecDeque,
    time::{Duration, Instant},
};

pub(super) use crate::widgets::measure_text;

/// Compute the column width needed for a cell's text content.
pub(super) fn col_text_width(name: &str) -> f32 {
    measure_text(name, 13.0, iced_core::Font::DEFAULT)
        + CELL_H_PADDING
        + RESIZE_HANDLE_WIDTH
}

/// Compute the column width needed for a header's text (bold, size 14).
pub(super) fn col_header_width(name: &str) -> f32 {
    let bold = iced_core::Font {
        weight: iced_core::font::Weight::Bold,
        ..iced_core::Font::DEFAULT
    };
    measure_text(name, 14.0, bold) + CELL_H_PADDING + RESIZE_HANDLE_WIDTH
}

pub(super) fn col_min_width(name: &str, max_w: f32) -> f32 {
    col_text_width(name).max(MIN_COL_WIDTH).min(max_w)
}

/// Truncate text to fit within a pixel width, appending "..." if
/// needed. Borrows the input when no truncation is required.
pub(crate) fn truncate_to_width(text: &str, max_px: f32) -> Cow<'_, str> {
    let avail = max_px - CELL_H_PADDING - RESIZE_HANDLE_WIDTH;
    if avail <= 0.0 || text.is_empty() {
        return Cow::Borrowed("");
    }
    let measure = |s: &str| measure_text(s, 13.0, iced_core::Font::DEFAULT);
    if measure(text) <= avail {
        return Cow::Borrowed(text);
    }
    let target = avail - measure("...");
    if target <= 0.0 {
        return Cow::Borrowed("...");
    }
    let mut ends: LPooled<Vec<usize>> = LPooled::take();
    ends.extend(text.char_indices().map(|(i, c)| i + c.len_utf8()));
    let fit = ends.partition_point(|&e| measure(&text[..e]) <= target);
    match fit {
        0 => Cow::Borrowed("..."),
        n => Cow::Owned(format!("{}...", &text[..ends[n - 1]])),
    }
}

/// Layout metrics written on each layout pass.
#[derive(Clone, Copy)]
pub(super) struct ViewportMetrics {
    pub(super) viewport_width: f32,
    pub(super) viewport_height: f32,
    pub(super) rows_in_view: usize,
    pub(super) cols_in_view: usize,
    pub(super) needs_subscription_reconcile: bool,
}

impl Default for ViewportMetrics {
    fn default() -> Self {
        Self {
            viewport_width: 1024.0,
            viewport_height: 0.0,
            rows_in_view: super::DEFAULT_VISIBLE_ROWS,
            cols_in_view: super::DEFAULT_VISIBLE_COLS,
            needs_subscription_reconcile: false,
        }
    }
}

/// State for an active column resize drag.
pub(super) struct ResizeDrag {
    pub(super) col_name: ArcStr,
    /// Last cursor x; only deltas between samples are used, so the
    /// coordinate frame does not matter. `None` before the first move.
    pub(super) last_x: Option<f32>,
    pub(super) current_width: f32,
}

#[derive(Clone, FromValue)]
pub(super) struct SortBy {
    pub(super) column: ArcStr,
    pub(super) direction: SortDirection,
}

#[derive(Clone, PartialEq, FromValue)]
pub(super) enum SortDirection {
    Ascending,
    Descending,
}

#[derive(Clone)]
pub(super) enum ColumnType {
    /// Plain text, optionally editable (spreadsheet-style).
    Text,
    /// Boolean toggle.
    Toggle,
    /// Dropdown selection from a fixed list.
    Combo { choices: Vec<(ArcStr, ArcStr)> },
    /// Numeric spinner with range and step.
    Spin { min: f64, max: f64, increment: f64 },
    /// Progress bar (read-only).
    Progress,
    /// Clickable button showing cell value.
    Button,
    /// Mini line chart of recent values. `min`/`max` fix the y-axis;
    /// an unset end auto-scales to the union of the column's rows.
    Sparkline { history_seconds: f64, min: Option<f64>, max: Option<f64> },
}

/// Parsed column spec from one entry of the columns array. A bare
/// string inflates to a `Text` column with a `` `Netidx `` source and
/// no callback.
pub(super) struct ColumnSpec {
    pub(super) name: ArcStr,
    pub(super) typ: ColumnType,
    pub(super) display_name: Option<ArcStr>,
    /// `source` ref bind id; `0` for bare-string columns.
    pub(super) source_bid: u64,
    /// Width ref bind id.
    pub(super) width_bid: u64,
    /// `on_resize` ref bind id.
    pub(super) on_resize_bid: u64,
    pub(super) callback_value: Option<Value>,
}

pub(super) fn parse_sort_by(v: &Value) -> LPooled<Vec<SortBy>> {
    v.clone().cast_to().unwrap_or_default()
}

pub(super) fn parse_selection(v: &Value) -> LPooled<AHashSet<ArcStr>> {
    let items = match v.clone().cast_to::<Vec<Value>>() {
        Ok(items) => items,
        Err(_) => return LPooled::take(),
    };
    items
        .into_iter()
        .filter_map(|v| match v {
            Value::String(s) => Some(s),
            _ => None,
        })
        .collect()
}

fn parse_column_type(v: Value) -> (ColumnType, Option<Value>) {
    #[derive(FromValue)]
    struct Choice {
        id: ArcStr,
        label: ArcStr,
    }
    #[derive(FromValue)]
    enum Repr {
        Text { on_edit: Option<Value> },
        Toggle { on_edit: Option<Value> },
        Combo { choices: Vec<Choice>, on_edit: Option<Value> },
        Spin { min: f64, max: f64, increment: f64, on_edit: Option<Value> },
        Progress,
        Button { on_click: Option<Value> },
        Sparkline { history_seconds: f64, min: Option<f64>, max: Option<f64> },
    }
    match v.cast_to::<Repr>() {
        Err(_) => (ColumnType::Text, None),
        Ok(Repr::Text { on_edit }) => (ColumnType::Text, on_edit),
        Ok(Repr::Toggle { on_edit }) => (ColumnType::Toggle, on_edit),
        Ok(Repr::Combo { choices, on_edit }) => {
            let choices =
                choices.into_iter().map(|Choice { id, label }| (id, label)).collect();
            (ColumnType::Combo { choices }, on_edit)
        }
        Ok(Repr::Spin { min, max, increment, on_edit }) => {
            (ColumnType::Spin { min, max, increment }, on_edit)
        }
        Ok(Repr::Progress) => (ColumnType::Progress, None),
        Ok(Repr::Button { on_click }) => (ColumnType::Button, on_click),
        Ok(Repr::Sparkline { history_seconds, min, max }) => {
            let history_seconds = if history_seconds.is_finite() && history_seconds > 0.0
            {
                history_seconds
            } else {
                60.0
            };
            (ColumnType::Sparkline { history_seconds, min, max }, None)
        }
    }
}

/// Strip null bytes so a user column name cannot collide with the
/// `ROW_NAME_SENTINEL_KEY` / `VALUE_COL_KEY` sentinels.
fn sanitize_col_name(raw: ArcStr) -> ArcStr {
    if raw.contains('\0') {
        let cleaned: CompactString = raw.chars().filter(|ch| *ch != '\0').collect();
        cleaned.as_str().into()
    } else {
        raw
    }
}

/// Parse one entry of the columns array; bare strings inflate to a
/// default `Text` column with a `` `Netidx `` source.
fn parse_column_entry(v: Value) -> Option<ColumnSpec> {
    if let Value::String(name) = v {
        return Some(ColumnSpec {
            name: sanitize_col_name(name),
            typ: ColumnType::Text,
            display_name: None,
            source_bid: 0,
            width_bid: 0,
            on_resize_bid: 0,
            callback_value: None,
        });
    }
    #[derive(FromValue)]
    struct Fields {
        name: ArcStr,
        typ: Value,
        display_name: Option<ArcStr>,
        source: u64,
        on_resize: u64,
        width: u64,
    }
    let Fields { name, typ, display_name, source, on_resize, width } =
        v.cast_to().ok()?;
    let (typ, callback_value) = parse_column_type(typ);
    Some(ColumnSpec {
        name: sanitize_col_name(name),
        typ,
        display_name,
        source_bid: source,
        width_bid: width,
        on_resize_bid: on_resize,
        callback_value,
    })
}

/// Parse the columns array of a `Table` value, in user order. For a
/// duplicate name the first occurrence's position and the last spec
/// win, so appended overrides work.
pub(super) fn parse_table_columns(v: &Value) -> LPooled<Vec<ColumnSpec>> {
    let mut raw = match v.clone().cast_to::<LPooled<Vec<Value>>>() {
        Ok(r) => r,
        Err(_) => return LPooled::take(),
    };
    let mut out: LPooled<Vec<ColumnSpec>> = LPooled::take();
    let mut idx: LPooled<AHashMap<ArcStr, usize>> = LPooled::take();
    for item in raw.drain(..) {
        let Some(spec) = parse_column_entry(item) else { continue };
        match idx.get(&spec.name) {
            Some(&i) => out[i] = spec,
            None => {
                idx.insert(spec.name.clone(), out.len());
                out.push(spec);
            }
        }
    }
    out
}

pub(super) fn numeric_key(s: &str) -> Option<f64> {
    s.parse::<f64>().ok()
}

/// The basename of a row path, or the whole path when it has no
/// separator.
pub(super) fn row_basename(p: &Path) -> &str {
    Path::basename(p).unwrap_or(&**p)
}

/// A column's parsed `source` ref. `Netidx` subscribes every absolute
/// row to `<row_path>/<col_name>` and shows the fallback for pending,
/// unsubscribed and virtual cells; `Static` shows the fallback for
/// every cell.
pub(super) enum Source {
    Netidx(Fallback),
    Static(Fallback),
}

/// Where a cell's fallback value comes from.
pub(super) enum Fallback {
    None,
    Uniform(Value),
    PerRow(LPooled<AHashMap<ArcStr, Value>>),
}

impl Fallback {
    fn from_value(v: Value) -> Self {
        match v {
            Value::Null => Fallback::None,
            v @ Value::Map(_) => match v.cast_to::<LPooled<AHashMap<ArcStr, Value>>>() {
                Ok(m) => Fallback::PerRow(m),
                Err(e) => {
                    warn!("source Map had non-string keys: {e}");
                    Fallback::None
                }
            },
            v => Fallback::Uniform(v),
        }
    }

    /// The fallback for `row_name` (a row basename).
    pub(super) fn lookup(&self, row_name: &str) -> Option<&Value> {
        match self {
            Fallback::None => None,
            Fallback::Uniform(v) => Some(v),
            Fallback::PerRow(m) => m.get(row_name),
        }
    }
}

impl Source {
    fn parse(v: Option<&Value>) -> Self {
        match v {
            None | Some(Value::Null) => Source::Netidx(Fallback::None),
            Some(v) => match v.clone().cast_to::<(ArcStr, Value)>() {
                Ok((tag, payload)) if tag.as_str() == "Netidx" => {
                    Source::Netidx(Fallback::from_value(payload))
                }
                _ => Source::Static(Fallback::from_value(v.clone())),
            },
        }
    }

    pub(super) fn is_netidx(&self) -> bool {
        matches!(self, Source::Netidx(_))
    }

    /// The fallback for a cell with no live subscription value.
    pub(super) fn lookup(&self, row_name: &str) -> Option<&Value> {
        match self {
            Source::Netidx(f) | Source::Static(f) => f.lookup(row_name),
        }
    }
}

/// A `source` column ref paired with its pre-parsed lookup cache.
pub(super) struct SourceEntry<X: GXExt> {
    pub(super) r: Ref<X>,
    pub(super) parsed: Source,
}

impl<X: GXExt> SourceEntry<X> {
    pub(super) fn new(r: Ref<X>) -> Self {
        let parsed = Source::parse(r.last.as_ref());
        Self { r, parsed }
    }

    pub(super) fn refresh_from_last(&mut self) {
        self.parsed = Source::parse(self.r.last.as_ref());
    }
}

/// Per-column state: the parsed spec plus the compiled callables and
/// refs, filled in by `apply_table`.
pub(super) struct ColumnState<X: GXExt> {
    pub(super) spec: ColumnSpec,
    pub(super) callback: Option<Callable<X>>,
    pub(super) source: Option<SourceEntry<X>>,
    pub(super) width_ref: Option<Ref<X>>,
    pub(super) ref_width: Option<f32>,
    pub(super) on_resize_ref: Option<Ref<X>>,
    pub(super) on_resize: Option<Callable<X>>,
}

impl<X: GXExt> ColumnState<X> {
    pub(super) fn new(spec: ColumnSpec) -> Self {
        Self {
            spec,
            callback: None,
            source: None,
            width_ref: None,
            ref_width: None,
            on_resize_ref: None,
            on_resize: None,
        }
    }

    /// True when this column subscribes to netidx; true until the
    /// source ref has a value.
    pub(super) fn is_subscribed(&self) -> bool {
        match self.source.as_ref() {
            Some(s) => s.parsed.is_netidx(),
            None => true,
        }
    }
}

pub(super) struct NakedValue<'a>(pub(super) &'a Value);
impl std::fmt::Display for NakedValue<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt_naked(f)
    }
}

/// A value as a cell shows it: strings bare (Combo matches raw ids),
/// null empty.
pub(super) fn format_value(v: &Value) -> ArcStr {
    match v {
        Value::Null => ArcStr::new(),
        Value::String(s) => s.clone(),
        _ => format_compact!("{}", NakedValue(v)).as_str().into(),
    }
}

/// Parse an edit buffer as a graphix value, falling back to a bare
/// string so `hello` needs no quotes.
pub(super) fn parse_or_quote(s: &str) -> Value {
    netidx::protocol::value_parser::parse_value(s)
        .unwrap_or_else(|_| Value::String(ArcStr::from(s)))
}

/// The path a cell stands for: the row's own for the row-name cell and a
/// Value-mode cell, else `<row>/<col>`. Selections and callbacks name
/// cells by it.
pub(super) fn cell_path(row: &Path, col: &ArcStr) -> ArcStr {
    if col == &ROW_NAME_KEY || col == &VALUE_COL_KEY {
        row.clone().into()
    } else {
        format_compact!("{}/{}", &**row, col.as_str()).as_str().into()
    }
}

/// Whether `sel` is `cell_path(row, col)`, without allocating.
pub(super) fn is_cell_path(sel: &str, row: &str, col: &ArcStr) -> bool {
    if col == &ROW_NAME_KEY || col == &VALUE_COL_KEY {
        return sel == row;
    }
    let n = row.len();
    sel.len() == n + 1 + col.len()
        && sel.as_bytes().get(n) == Some(&b'/')
        && sel.starts_with(row)
        && &sel[n + 1..] == col.as_str()
}

/// Halve a sparkline history: of each run of four points keep the lowest
/// and the highest, in time order, so every peak and valley survives.
pub(crate) fn decimate_sparkline(history: &mut VecDeque<(Instant, f64)>) {
    let mut points: LPooled<Vec<(Instant, f64)>> = LPooled::take();
    points.extend(history.drain(..));
    for run in points.chunks(4) {
        if run.len() < 4 {
            history.extend(run.iter().copied());
            continue;
        }
        let lo = (0..4).min_by(|&a, &b| run[a].1.total_cmp(&run[b].1)).unwrap();
        let hi = (0..4).max_by(|&a, &b| run[a].1.total_cmp(&run[b].1)).unwrap();
        let (a, b) = if lo == hi { (0, 3) } else { (lo.min(hi), lo.max(hi)) };
        history.push_back(run[a]);
        history.push_back(run[b]);
    }
}

/// The oldest instant a history `history_seconds` long keeps at `now`;
/// `None` when the window reaches past what `Instant` can say: keep all.
fn sparkline_cutoff(now: Instant, history_seconds: f64) -> Option<Instant> {
    now.checked_sub(Duration::try_from_secs_f64(history_seconds).ok()?)
}

/// Drop the points older than the window.
pub(super) fn age_sparkline(
    history: &mut VecDeque<(Instant, f64)>,
    now: Instant,
    history_seconds: f64,
) {
    if let Some(cutoff) = sparkline_cutoff(now, history_seconds) {
        while history.front().is_some_and(|(t, _)| *t < cutoff) {
            history.pop_front();
        }
    }
}

/// Record a point at `now`, age the window and keep the history at most
/// `MAX_SPARKLINE_POINTS` long.
pub(super) fn push_sparkline_point(
    history: &mut VecDeque<(Instant, f64)>,
    now: Instant,
    v: f64,
    history_seconds: f64,
) {
    history.push_back((now, v));
    age_sparkline(history, now, history_seconds);
    if history.len() > MAX_SPARKLINE_POINTS {
        decimate_sparkline(history);
    }
}

pub(super) fn value_to_f64(v: &Value) -> Option<f64> {
    match v {
        Value::F64(f) => Some(*f),
        Value::F32(f) => Some(*f as f64),
        Value::I64(i) => Some(*i as f64),
        Value::U64(i) => Some(*i as f64),
        Value::I32(i) => Some(*i as f64),
        Value::U32(i) => Some(*i as f64),
        _ => v.clone().cast_to::<f64>().ok(),
    }
}
