//! Pure data types, parsers, and small helpers for the data table.
//!
//! Nothing in here depends on `DataTableW` state; the widget methods
//! live in the sibling modules. Keeping these in one file avoids
//! sprinkling `parse_*` helpers across every impl block that calls them.

use super::{Renderer, CELL_H_PADDING, MIN_COL_WIDTH, RESIZE_HANDLE_WIDTH};
use arcstr::ArcStr;
use compact_str::{format_compact, CompactString};
use fxhash::{FxHashMap, FxHashSet};
use graphix_rt::{Callable, GXExt, Ref};
use iced_core::text::Paragraph as _;
use log::warn;
use netidx::{path::Path, publisher::Value};
use netidx_derive::FromValue;
use std::{borrow::Cow, collections::VecDeque, time::Instant};

pub(super) type Paragraph = <Renderer as iced_core::text::Renderer>::Paragraph;

/// Measure the actual rendered width of text at the given font size.
pub(super) fn measure_text(text: &str, size: f32, font: iced_core::Font) -> f32 {
    let para = Paragraph::with_text(iced_core::Text {
        content: text.into(),
        bounds: iced_core::Size::new(f32::INFINITY, f32::INFINITY),
        size: iced_core::Pixels(size),
        line_height: iced_core::text::LineHeight::default(),
        font,
        align_x: iced_core::alignment::Horizontal::Left.into(),
        align_y: iced_core::alignment::Vertical::Top,
        shaping: iced_core::text::Shaping::Advanced,
        wrapping: iced_core::text::Wrapping::None,
    });
    para.min_bounds().width
}

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

/// Truncate text to fit within a pixel width, appending "..." if needed.
/// Uses actual text measurement for accuracy. Returns the input as a
/// borrow when no truncation is required — the common case for short
/// numeric cells — so no allocation happens in that path.
pub(super) fn truncate_to_width(text: &str, max_px: f32) -> Cow<'_, str> {
    let avail = max_px - CELL_H_PADDING - RESIZE_HANDLE_WIDTH;
    if avail <= 0.0 || text.is_empty() {
        return Cow::Borrowed("");
    }
    let full_w = measure_text(text, 13.0, iced_core::Font::DEFAULT);
    if full_w <= avail {
        return Cow::Borrowed(text);
    }
    // Binary search for the longest prefix that fits with "..."
    let ellipsis_w = measure_text("...", 13.0, iced_core::Font::DEFAULT);
    let target = avail - ellipsis_w;
    if target <= 0.0 {
        return Cow::Borrowed("...");
    }
    let mut lo = 0usize;
    let mut hi = text.len();
    while lo < hi {
        let mid = (lo + hi + 1) / 2;
        // Snap to char boundary
        let mid = if mid >= text.len() {
            text.len()
        } else {
            let mut m = mid;
            while m > 0 && !text.is_char_boundary(m) {
                m -= 1;
            }
            m
        };
        if mid == 0 {
            break;
        }
        let w = measure_text(&text[..mid], 13.0, iced_core::Font::DEFAULT);
        if w <= target {
            lo = mid;
            if lo == hi {
                break;
            }
        } else {
            hi = mid - 1;
            // Snap hi to char boundary
            while hi > 0 && !text.is_char_boundary(hi) {
                hi -= 1;
            }
        }
    }
    if lo == 0 {
        Cow::Borrowed("...")
    } else {
        while lo > 0 && !text.is_char_boundary(lo) {
            lo -= 1;
        }
        Cow::Owned(format!("{}...", &text[..lo]))
    }
}

/// Viewport-derived layout metrics. Written by the `responsive`
/// closure on each layout pass, read by keyboard nav, scroll handling,
/// subscription updates. `dirty` drives the deferred
/// `update_subscriptions()` hook in `before_view`.
#[derive(Clone, Copy)]
pub(super) struct ViewportMetrics {
    pub(super) viewport_width: f32,
    pub(super) viewport_height: f32,
    pub(super) rows_in_view: usize,
    pub(super) cols_in_view: usize,
    pub(super) dirty: bool,
}

impl Default for ViewportMetrics {
    fn default() -> Self {
        Self {
            viewport_width: 1024.0,
            viewport_height: 0.0,
            rows_in_view: super::DEFAULT_VISIBLE_ROWS,
            cols_in_view: super::DEFAULT_VISIBLE_COLS,
            dirty: false,
        }
    }
}

/// State for an active column resize drag.
pub(super) struct ResizeDrag {
    pub(super) col_name: ArcStr,
    /// Last cursor x in whatever coordinate system ColumnResizeMove
    /// uses (widget-local, per MouseArea::on_move). We track deltas
    /// between successive moves so the drag is robust regardless of
    /// whether the cursor position is window-absolute or local —
    /// all that matters is that consecutive samples use the same
    /// frame. `None` until the first move sample arrives.
    pub(super) last_x: Option<f32>,
    pub(super) current_width: f32,
}

// ── Sort / column types ────────────────────────────────────────────

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
    /// Mini line chart accumulating recent values. `min`/`max`, when
    /// `Some`, fix the y-axis bounds for every cell in the column;
    /// otherwise the column auto-scales to the union of all rows'
    /// values (so cells in the same column can be compared visually).
    Sparkline { history_seconds: f64, min: Option<f64>, max: Option<f64> },
}

/// Parsed column spec from one entry of the graphix
/// `Array<[string, ColumnSpec]>` columns array. A bare-string entry
/// (`Value::String`) inflates to a `ColumnSpec` with `typ = Text`,
/// `source` reffing `` `Netidx ``, and no callback — the equivalent of
/// the netidx-published cell behavior the column would have had under
/// the old `column_types` API.
pub(super) struct ColumnSpec {
    pub(super) name: ArcStr,
    pub(super) typ: ColumnType,
    pub(super) display_name: Option<ArcStr>,
    /// Raw `source` ref bind ID — compiled separately into the
    /// `ColumnState::source` entry. `0` for bare-string columns,
    /// which inflate to a synthetic `&\`Netidx` ref with no bid.
    pub(super) source_bid: u64,
    /// Raw width ref bind ID — compiled separately into the
    /// `ColumnState::width_ref` slot.
    pub(super) width_bid: u64,
    /// Raw on_resize ref bind ID. The .gxi types the field as
    /// `&[fn(f64) -> Any, null]`, so the runtime value is a u64 bid
    /// pointing at the callable (or null).
    pub(super) on_resize_bid: u64,
    pub(super) callback_value: Option<Value>,
}

// ── Parsing ────────────────────────────────────────────────────────

pub(super) fn parse_sort_by(v: &Value) -> Vec<SortBy> {
    v.clone().cast_to().unwrap_or_default()
}

pub(super) fn parse_selection(v: &Value) -> FxHashSet<ArcStr> {
    let items = match v.clone().cast_to::<Vec<Value>>() {
        Ok(items) => items,
        Err(_) => return FxHashSet::default(),
    };
    items
        .into_iter()
        .filter_map(|v| match v {
            Value::String(s) => Some(s),
            _ => None,
        })
        .collect()
}

fn parse_column_type(v: &Value) -> (ColumnType, Option<Value>) {
    // Bare variants arrive as Value::String("Tag")
    if let Value::String(tag) = v {
        return match tag.as_str() {
            "Progress" => (ColumnType::Progress, None),
            _ => (ColumnType::Text, None),
        };
    }
    let (tag, payload) = match v.clone().cast_to::<(ArcStr, Value)>() {
        Ok(t) => t,
        Err(_) => return (ColumnType::Text, None),
    };
    match tag.as_str() {
        "Text" => (ColumnType::Text, extract_struct1_callback(&payload)),
        "Toggle" => (ColumnType::Toggle, extract_struct1_callback(&payload)),
        "Combo" => {
            // struct { choices, on_edit } — alphabetical: choices, on_edit
            match payload.cast_to::<[(ArcStr, Value); 2]>() {
                Ok([(_, choices_val), (_, on_edit)]) => {
                    let choices = parse_combo_choices(&choices_val);
                    (ColumnType::Combo { choices }, non_null(on_edit))
                }
                Err(_) => (ColumnType::Combo { choices: vec![] }, None),
            }
        }
        "Spin" => {
            // struct { increment, max, min, on_edit } — 4 fields alphabetical
            match payload.cast_to::<[(ArcStr, Value); 4]>() {
                Ok([(_, inc_v), (_, max_v), (_, min_v), (_, on_edit)]) => {
                    let inc = inc_v.cast_to::<f64>().unwrap_or(1.0);
                    let max = max_v.cast_to::<f64>().unwrap_or(100.0);
                    let min = min_v.cast_to::<f64>().unwrap_or(0.0);
                    (ColumnType::Spin { min, max, increment: inc }, non_null(on_edit))
                }
                Err(_) => {
                    (ColumnType::Spin { min: 0.0, max: 100.0, increment: 1.0 }, None)
                }
            }
        }
        "Progress" => (ColumnType::Progress, None),
        "Button" => (ColumnType::Button, extract_struct1_callback(&payload)),
        "Sparkline" => {
            // struct { history_seconds, max, min } — 3 fields alphabetical.
            // `min`/`max` are `[f64, null]` so cast_to::<f64>() returns
            // Err for `null` — that's how we get None.
            let (hs, max_o, min_o) = match payload.cast_to::<[(ArcStr, Value); 3]>() {
                Ok([(_, hs_v), (_, max_v), (_, min_v)]) => {
                    let hs_raw = hs_v.cast_to::<f64>().unwrap_or(60.0);
                    let hs = if hs_raw.is_finite() && hs_raw > 0.0 { hs_raw } else { 60.0 };
                    let max_o = max_v.cast_to::<f64>().ok();
                    let min_o = min_v.cast_to::<f64>().ok();
                    (hs, max_o, min_o)
                }
                Err(_) => (60.0, None, None),
            };
            (ColumnType::Sparkline { history_seconds: hs, min: min_o, max: max_o }, None)
        }
        _ => (ColumnType::Text, None),
    }
}

/// Extract the lone field of a one-field struct (e.g. `Text({on_edit})`),
/// returning `None` for `null` so callers don't compile callbacks for
/// the absent case.
fn extract_struct1_callback(payload: &Value) -> Option<Value> {
    match payload.clone().cast_to::<[(ArcStr, Value); 1]>() {
        Ok([(_, v)]) => non_null(v),
        Err(_) => None,
    }
}

fn non_null(v: Value) -> Option<Value> {
    match v {
        Value::Null => None,
        v => Some(v),
    }
}

fn parse_combo_choices(v: &Value) -> Vec<(ArcStr, ArcStr)> {
    let items = v.clone().cast_to::<Vec<Value>>().unwrap_or_default();
    let mut choices = Vec::with_capacity(items.len());
    for item in items {
        // struct { id, label } — alphabetical
        if let Ok([(_, id_val), (_, label_val)]) = item.cast_to::<[(ArcStr, Value); 2]>()
        {
            let id = match id_val {
                Value::String(s) => s,
                _ => ArcStr::new(),
            };
            let label = match label_val {
                Value::String(s) => s,
                _ => ArcStr::new(),
            };
            choices.push((id, label));
        }
    }
    choices
}

pub(super) fn value_to_display(v: &Value) -> ArcStr {
    match v {
        Value::Null => ArcStr::new(),
        Value::String(s) => s.clone(),
        _ => format_compact!("{}", NakedValue(v)).as_str().into(),
    }
}

/// Strip null bytes from a column name so user-supplied input can't
/// collide with the `ROW_NAME_KEY` / `VALUE_COL_KEY` sentinels (both
/// of which carry leading `\0`).
fn sanitize_col_name(raw: ArcStr) -> ArcStr {
    if raw.contains('\0') {
        let cleaned: CompactString = raw.chars().filter(|ch| *ch != '\0').collect();
        cleaned.as_str().into()
    } else {
        raw
    }
}

/// Parse one entry of the columns array. Bare strings inflate to a
/// default Text column with `` `Netidx `` source. Structs cast to the
/// 6-field ColumnSpec shape (name, typ, display_name, source,
/// on_resize, width — alphabetical).
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
    // Struct fields alphabetical: display_name, name, on_resize,
    // source, typ, width.
    let [(_, dn), (_, name_v), (_, or), (_, src), (_, tv), (_, w)] =
        v.cast_to::<[(ArcStr, Value); 6]>().ok()?;
    let name = match name_v {
        Value::String(s) => sanitize_col_name(s),
        _ => return None,
    };
    let display_name = match dn {
        Value::String(s) => Some(s),
        _ => None,
    };
    let source_bid = src.cast_to::<u64>().unwrap_or(0);
    let on_resize_bid = or.cast_to::<u64>().unwrap_or(0);
    let width_bid = w.cast_to::<u64>().unwrap_or(0);
    let (typ, callback_value) = parse_column_type(&tv);
    Some(ColumnSpec {
        name,
        typ,
        display_name,
        source_bid,
        width_bid,
        on_resize_bid,
        callback_value,
    })
}

/// Parse the columns array of a `Table` value. Returns the specs in
/// the user-supplied order; duplicate names are kept in their first
/// position (later duplicates are dropped silently — a column key is
/// the dedup primitive).
pub(super) fn parse_table_columns(v: &Value) -> Vec<ColumnSpec> {
    let raw = match v.clone().cast_to::<Vec<Value>>() {
        Ok(r) => r,
        Err(_) => return Vec::new(),
    };
    let mut out: Vec<ColumnSpec> = Vec::with_capacity(raw.len());
    let mut seen: FxHashSet<ArcStr> = FxHashSet::default();
    for item in raw {
        let Some(spec) = parse_column_entry(item) else { continue };
        if seen.insert(spec.name.clone()) {
            out.push(spec);
        }
    }
    out
}

// ── Filter / sort ──────────────────────────────────────────────────

pub(super) fn numeric_key(s: &str) -> Option<f64> {
    s.parse::<f64>().ok()
}

/// Return the basename of a row path as `&str`, falling back to the
/// full path string when it has no separator. Zero-allocation — the
/// result borrows from the path's backing `ArcStr`.
pub(super) fn row_basename(p: &Path) -> &str {
    Path::basename(p).unwrap_or(&**p)
}

/// Pre-parsed cache for a column's `source` ref. Built once when the
/// ref updates so the per-cell display path can look up by `&str`
/// without rebuilding a `Value::String(ArcStr)` key per call.
/// `PerRow` uses `FxHashMap<ArcStr, Value>` because `ArcStr: Borrow<str>`
/// enables the cheap lookup.
///
/// `Netidx` means the column subscribes to `<row_path>/<col_name>` for
/// every row; cell values come from netidx, not from the source ref
/// itself. The other variants are pure display data with no
/// subscription.
pub(super) enum Source {
    Netidx,
    Uniform(Value),
    PerRow(FxHashMap<ArcStr, Value>),
}

impl Source {
    fn parse(v: Option<&Value>) -> Self {
        match v {
            // The .gxi defaults the source ref to `&\`Netidx`, so a
            // missing/null value here means the column never had a
            // source attached (bare-string column with synthesized
            // default). Treat as Netidx.
            None | Some(Value::Null) => Source::Netidx,
            Some(Value::String(s)) if s.as_str() == "Netidx" => Source::Netidx,
            Some(v @ Value::Map(_)) => {
                // `source` is typed `&[\`Netidx, string, Map<string, Any>]`
                // in the .gxi, so a `Value::Map` here is guaranteed to
                // have string keys.
                match v.clone().cast_to::<FxHashMap<ArcStr, Value>>() {
                    Ok(per_row) => Source::PerRow(per_row),
                    Err(e) => {
                        warn!("source Map had non-string keys: {e}");
                        Source::Netidx
                    }
                }
            }
            Some(v) => Source::Uniform(v.clone()),
        }
    }

    /// True when this source drives a netidx subscription. The
    /// subscription path is `<row_path>/<col_name>` per row;
    /// non-Netidx sources skip subscription and render from their
    /// stored value.
    pub(super) fn is_netidx(&self) -> bool {
        matches!(self, Source::Netidx)
    }

    /// Per-row stored value for non-Netidx sources, or `None` for
    /// Netidx (cells come from subscriptions instead).
    pub(super) fn lookup(&self, row_name: &str) -> Option<&Value> {
        match self {
            Source::Netidx => None,
            Source::Uniform(v) => Some(v),
            Source::PerRow(m) => m.get(row_name),
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

/// Per-column state. One entry per column in the table's `columns`
/// array, in display order. Always carries a `ColumnSpec` (bare
/// strings inflate to a default Text + `` `Netidx `` source spec
/// during parsing); compiled callables and refs are populated as
/// `apply_table` resolves the per-column bind ids.
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
    /// Build a fresh `ColumnState` from a parsed spec. Compiled
    /// callables/refs start `None` and are filled in by
    /// `apply_column_recompile`.
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

    /// True when this column subscribes to netidx — derived from the
    /// source ref's parsed value when known, defaulting to true (the
    /// `` `Netidx `` variant is what bare-string columns inflate to).
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

pub(super) fn format_value(v: &Value) -> ArcStr {
    // Strings are displayed bare (no surrounding quotes). fmt_naked
    // quotes strings for parser round-tripping, but spreadsheet cells
    // look much better without the quotes, and Combo's value lookup
    // matches raw ids. Returns ArcStr because cell values are stored
    // long-term (in `cells.grid`) and read many times per frame —
    // refcount-bump clones beat per-read String allocs.
    match v {
        Value::Null => ArcStr::new(),
        Value::String(s) => s.clone(),
        _ => format_compact!("{}", NakedValue(v)).as_str().into(),
    }
}

/// Parse the user-typed edit buffer as a graphix value. If parsing
/// succeeds we commit the typed value (e.g. `42` → `i64`, `true` →
/// `bool`, `duration:1.s` → `Duration`). If parsing fails we commit
/// the buffer as a bare string, so users can type `hello` without
/// needing to wrap it in quotes.
pub(super) fn parse_or_quote(s: &str) -> Value {
    netidx::protocol::value_parser::parse_value(s)
        .unwrap_or_else(|_| Value::String(ArcStr::from(s)))
}

/// Check whether `sel` equals the cell path `<row>/<col>` without
/// allocating a temporary string. Used by selection-position lookups
/// in keyboard nav and cell-selection rendering.
pub(super) fn cell_path_matches(sel: &ArcStr, row: &str, col: &str) -> bool {
    let s = sel.as_str();
    let n = row.len();
    s.len() == n + 1 + col.len()
        && s.as_bytes().get(n) == Some(&b'/')
        && s.starts_with(row)
        && &s[n + 1..] == col
}

/// Decimate a sparkline history by merging adjacent pairs.
/// For each pair, keeps the point with greater absolute deviation
/// from the pair's mean value, preserving peaks and valleys.
/// Halves the number of points.
pub(crate) fn decimate_sparkline(history: &mut VecDeque<(Instant, f64)>) {
    let points: Vec<(Instant, f64)> = history.drain(..).collect();
    let mut i = 0;
    while i < points.len() {
        if i + 1 < points.len() {
            let a = &points[i];
            let b = &points[i + 1];
            let mean = (a.1 + b.1) / 2.0;
            let da = (a.1 - mean).abs();
            let db = (b.1 - mean).abs();
            // Keep the point with greater deviation, use midpoint time.
            let mid_t = a.0 + (b.0 - a.0) / 2;
            if da >= db {
                history.push_back((mid_t, a.1));
            } else {
                history.push_back((mid_t, b.1));
            }
            i += 2;
        } else {
            history.push_back(points[i]);
            i += 1;
        }
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
