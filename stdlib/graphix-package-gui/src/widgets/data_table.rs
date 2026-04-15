//! Netidx data table widget.
//!
//! Subscribes to a netidx path, discovers rows and columns via the
//! resolver, subscribes to individual cells, and renders a live
//! scrollable table with virtualized scrolling.

use super::{GuiW, GuiWidget, IcedElement, Message, Renderer};
use crate::theme::GraphixTheme;
use anyhow::{Context, Result};
use arcstr::ArcStr;
use fxhash::{FxHashMap, FxHashSet};
use graphix_compiler::expr::ExprId;
use graphix_rt::{Callable, GXExt, GXHandle, Ref, TRef};
use iced_core::text::Paragraph as _;
use iced_widget as widget;
use log::warn;
use netidx::{
    path::Path,
    protocol::valarray::ValArray,
    publisher::Value,
    subscriber::{Dval, Event, Subscriber, UpdatesFlags},
};
use parking_lot::Mutex;
use std::{
    collections::VecDeque,
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc,
    },
    time::Instant,
};

const ROW_HEIGHT_ESTIMATE: f32 = 22.0;
const ROW_HEIGHT_CONTROLS: f32 = 30.0;
const ROW_BUFFER: usize = 50;
const MIN_COL_WIDTH: f32 = 80.0;
const DEFAULT_MAX_COL_WIDTH: f32 = 300.0;
const DEFAULT_VISIBLE_ROWS: usize = 30;
const DEFAULT_VISIBLE_COLS: usize = 20;
/// Max points kept per sparkline. When exceeded, adjacent pairs are
/// merged (keeping the value with greater absolute deviation from the
/// mean) to halve the count. This bounds memory at ~8KB per sparkline.
const MAX_SPARKLINE_POINTS: usize = 512;

/// Horizontal padding inside each cell (left + right total: [3, 5] = 10px).
const CELL_H_PADDING: f32 = 10.0;
/// Width of the resize handle inside header cells.
const RESIZE_HANDLE_WIDTH: f32 = 5.0;
/// Internal key for the synthesized row-name column (leftmost when
/// `show_row_name` is true). Contains a leading null byte so it can
/// never collide with a real netidx column name, even one literally
/// called "name". All width caches, ref/user width maps, scroll
/// targets, and cell-path routing use this key for the row-name
/// column — `"name"` is only ever the displayed header label.
const ROW_NAME_KEY: &str = "\0__rowname__";
/// Header label displayed for the synthesized row-name column.
const ROW_NAME_LABEL: &str = "name";

type Paragraph = <Renderer as iced_core::text::Renderer>::Paragraph;

/// Measure the actual rendered width of text at the given font size.
fn measure_text(text: &str, size: f32, font: iced_core::Font) -> f32 {
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
fn col_text_width(name: &str) -> f32 {
    measure_text(name, 13.0, iced_core::Font::DEFAULT)
        + CELL_H_PADDING + RESIZE_HANDLE_WIDTH
}

/// Compute the column width needed for a header's text (bold, size 14).
fn col_header_width(name: &str) -> f32 {
    let bold = iced_core::Font {
        weight: iced_core::font::Weight::Bold,
        ..iced_core::Font::DEFAULT
    };
    measure_text(name, 14.0, bold)
        + CELL_H_PADDING + RESIZE_HANDLE_WIDTH
}

fn col_min_width(name: &str, max_w: f32) -> f32 {
    col_text_width(name).max(MIN_COL_WIDTH).min(max_w)
}

/// Truncate text to fit within a pixel width, appending "..." if needed.
/// Uses actual text measurement for accuracy.
fn truncate_to_width(text: &str, max_px: f32) -> String {
    let avail = max_px - CELL_H_PADDING - RESIZE_HANDLE_WIDTH;
    if avail <= 0.0 || text.is_empty() {
        return String::new();
    }
    let full_w = measure_text(text, 13.0, iced_core::Font::DEFAULT);
    if full_w <= avail {
        return text.to_string();
    }
    // Binary search for the longest prefix that fits with "..."
    let ellipsis_w = measure_text("...", 13.0, iced_core::Font::DEFAULT);
    let target = avail - ellipsis_w;
    if target <= 0.0 {
        return "...".to_string();
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
            while m > 0 && !text.is_char_boundary(m) { m -= 1; }
            m
        };
        if mid == 0 { break; }
        let w = measure_text(&text[..mid], 13.0, iced_core::Font::DEFAULT);
        if w <= target {
            lo = mid;
            if lo == hi { break; }
        } else {
            hi = mid - 1;
            // Snap hi to char boundary
            while hi > 0 && !text.is_char_boundary(hi) { hi -= 1; }
        }
    }
    if lo == 0 {
        "...".to_string()
    } else {
        while lo > 0 && !text.is_char_boundary(lo) { lo -= 1; }
        format!("{}...", &text[..lo])
    }
}

/// State for an active column resize drag.
struct ResizeDrag {
    col_name: String,
    /// Last cursor x in whatever coordinate system ColumnResizeMove
    /// uses (widget-local, per MouseArea::on_move). We track deltas
    /// between successive moves so the drag is robust regardless of
    /// whether the cursor position is window-absolute or local —
    /// all that matters is that consecutive samples use the same
    /// frame. `None` until the first move sample arrives.
    last_x: Option<f32>,
    current_width: f32,
}

// ── Sort / column types ────────────────────────────────────────────

#[derive(Clone)]
struct SortBy {
    column: String,
    ascending: bool,
}


#[derive(Clone)]
enum ColumnType {
    /// Plain text, optionally editable (spreadsheet-style).
    Text,
    /// Boolean toggle.
    Toggle,
    /// Dropdown selection from a fixed list.
    Combo { choices: Vec<(String, String)> },
    /// Numeric spinner with range and step.
    Spin { min: f64, max: f64, increment: f64 },
    /// Progress bar (read-only).
    Progress,
    /// Clickable button showing cell value.
    Button,
    /// Mini line chart accumulating recent values.
    Sparkline { history_seconds: f64 },
}

/// Parsed column spec from the graphix Map<string, ColumnSpec>.
struct ColumnSpec {
    typ: ColumnType,
    display_name: Option<String>,
    /// Raw default_value ref bind ID — compiled separately into default_value_refs.
    default_value_bid: u64,
    /// Raw width ref bind ID — compiled separately into width_refs.
    width_bid: u64,
    /// Raw on_resize ref bind ID — compiled separately. The .gxi types
    /// the field as `&[fn(f64) -> Any, null]`, so the runtime value is
    /// a u64 bid pointing at the callable (or null).
    on_resize_bid: u64,
    callback_value: Option<Value>,
}

// ── Parsing ────────────────────────────────────────────────────────

fn parse_sort_by(v: &Value) -> Vec<SortBy> {
    let items = v.clone().cast_to::<Vec<Value>>().unwrap_or_default();
    items
        .into_iter()
        .filter_map(|item| {
            // SortBy struct: { column, direction } — alphabetical
            let [(_, col_v), (_, dir_v)] =
                item.cast_to::<[(ArcStr, Value); 2]>().ok()?;
            let column = col_v.cast_to::<String>().ok()?;
            let ascending = match &dir_v {
                Value::String(s) => s.as_str() == "Ascending",
                _ => dir_v
                    .clone()
                    .cast_to::<(ArcStr, Value)>()
                    .map(|(tag, _)| tag.as_str() == "Ascending")
                    .unwrap_or(true),
            };
            Some(SortBy { column, ascending })
        })
        .collect()
}

fn parse_selection(v: &Value) -> FxHashSet<String> {
    v.clone()
        .cast_to::<Vec<String>>()
        .unwrap_or_default()
        .into_iter()
        .collect()
}

fn parse_column_type(v: &Value) -> (ColumnType, Option<Value>) {
    // Bare variants arrive as Value::String("Tag")
    if let Value::String(tag) = v {
        return match tag.as_str() {
            "Progress" => (ColumnType::Progress, Option::None),
            _ => (ColumnType::Text, Option::None),
        };
    }
    let (tag, payload) = match v.clone().cast_to::<(ArcStr, Value)>() {
        Ok(t) => t,
        Err(_) => return (ColumnType::Text, Option::None),
    };
    match tag.as_str() {
        "Text" => {
            // struct { on_edit: [fn|null] } — field: on_edit
            let cb = extract_callback_field(&payload, 1, 0);
            (ColumnType::Text, cb)
        }
        "Toggle" => {
            let cb = extract_callback_field(&payload, 1, 0);
            (ColumnType::Toggle, cb)
        }
        "Combo" => {
            // struct { choices, on_edit } — alphabetical: choices, on_edit
            let choices = match payload.clone().cast_to::<[(ArcStr, Value); 2]>() {
                Ok([(_, choices_val), (_, _on_edit)]) => {
                    parse_combo_choices(&choices_val)
                }
                Err(_) => vec![],
            };
            let cb = match payload.cast_to::<[(ArcStr, Value); 2]>() {
                Ok([(_, _), (_, on_edit)]) => non_null(on_edit),
                Err(_) => Option::None,
            };
            (ColumnType::Combo { choices }, cb)
        }
        "Spin" => {
            // struct { increment, max, min, on_edit } — 4 fields alphabetical
            let (min, max, inc) =
                match payload.clone().cast_to::<[(ArcStr, Value); 4]>() {
                    Ok([(_, inc_v), (_, max_v), (_, min_v), (_, _)]) => {
                        let inc = inc_v.cast_to::<f64>().unwrap_or(1.0);
                        let max = max_v.cast_to::<f64>().unwrap_or(100.0);
                        let min = min_v.cast_to::<f64>().unwrap_or(0.0);
                        (min, max, inc)
                    }
                    Err(_) => (0.0, 100.0, 1.0),
                };
            let cb = match payload.cast_to::<[(ArcStr, Value); 4]>() {
                Ok([(_, _), (_, _), (_, _), (_, on_edit)]) => non_null(on_edit),
                Err(_) => Option::None,
            };
            (ColumnType::Spin { min, max, increment: inc }, cb)
        }
        "Progress" => (ColumnType::Progress, Option::None),
        "Button" => {
            // struct { on_click } — 1 field
            let cb = extract_callback_field(&payload, 1, 0);
            (ColumnType::Button, cb)
        }
        "Sparkline" => {
            // struct { history_seconds } — 1 field
            let hs = match payload.cast_to::<[(ArcStr, Value); 1]>() {
                Ok([(_, v)]) => v.cast_to::<f64>().unwrap_or(60.0),
                Err(_) => 60.0,
            };
            (ColumnType::Sparkline { history_seconds: hs }, Option::None)
        }
        _ => (ColumnType::Text, Option::None),
    }
}

fn extract_callback_field(payload: &Value, n_fields: usize, idx: usize) -> Option<Value> {
    if n_fields == 1 {
        match payload.clone().cast_to::<[(ArcStr, Value); 1]>() {
            Ok(fields) => non_null(fields[idx].1.clone()),
            Err(_) => Option::None,
        }
    } else {
        Option::None
    }
}

fn non_null(v: Value) -> Option<Value> {
    match v {
        Value::Null => Option::None,
        v => Some(v),
    }
}

fn parse_combo_choices(v: &Value) -> Vec<(String, String)> {
    let items = v.clone().cast_to::<Vec<Value>>().unwrap_or_default();
    let mut choices = Vec::new();
    for item in &items {
        // struct { id, label } — alphabetical
        if let Ok([(_, id_val), (_, label_val)]) =
            item.clone().cast_to::<[(ArcStr, Value); 2]>()
        {
            let id = id_val.cast_to::<String>().unwrap_or_default();
            let label = label_val.cast_to::<String>().unwrap_or_default();
            choices.push((id, label));
        }
    }
    choices
}

fn value_to_display(v: &Value) -> String {
    match v {
        Value::Null => String::new(),
        Value::String(s) => s.to_string(),
        _ => NakedValue(v).to_string(),
    }
}

fn parse_column_specs(v: &Value) -> FxHashMap<String, ColumnSpec> {
    let pairs = match v.clone().cast_to::<Vec<(String, Value)>>() {
        Ok(p) => p,
        Err(_) => return FxHashMap::default(),
    };
    let mut map = FxHashMap::default();
    for (mut name, spec_val) in pairs {
        // Strip null bytes from column keys so a user-supplied column
        // spec can't collide with the `ROW_NAME_KEY` sentinel.
        if name.contains('\0') {
            name.retain(|ch| ch != '\0');
        }
        // ColumnSpec struct: { default_value, display_name, on_resize, typ, width } — alphabetical
        let (dv_bid, display_name, on_resize_bid, typ_val, width_bid) =
            match spec_val.cast_to::<[(ArcStr, Value); 5]>() {
                Ok([(_, dv), (_, dn), (_, or), (_, tv), (_, w)]) => {
                    let bid = dv.cast_to::<u64>().unwrap_or(0);
                    let display_name = match dn {
                        Value::Null => None,
                        Value::String(s) => Some(s.to_string()),
                        _ => None,
                    };
                    let on_resize_bid = or.cast_to::<u64>().unwrap_or(0);
                    let width_bid = w.cast_to::<u64>().unwrap_or(0);
                    (bid, display_name, on_resize_bid, tv, width_bid)
                }
                Err(_) => (0, None, 0, Value::Null, 0),
            };
        let (typ, callback_value) = parse_column_type(&typ_val);
        map.insert(name, ColumnSpec {
            typ, display_name, default_value_bid: dv_bid,
            width_bid, on_resize_bid, callback_value,
        });
    }
    map
}

// ── Filter / sort ──────────────────────────────────────────────────

fn numeric_key(s: &str) -> Option<f64> {
    s.parse::<f64>().ok()
}

// ── Shared cell data ───────────────────────────────────────────────

struct SharedCells {
    grid: Mutex<Vec<Vec<String>>>,
    /// Sparkline history: (row_path, col_name) → timestamped values.
    /// Keyed by path strings so history survives row reordering.
    sparklines: Mutex<FxHashMap<(String, String), VecDeque<(Instant, f64)>>>,
    dirty: AtomicBool,
}

#[derive(Clone, Copy, PartialEq)]
enum DisplayMode { Table, Value }

// ── Widget ─────────────────────────────────────────────────────────

pub(crate) struct DataTableW<X: GXExt> {
    gx: GXHandle<X>,
    subscriber: Subscriber,
    rt: tokio::runtime::Handle,
    table_ref: Ref<X>,
    show_row_name: TRef<X, bool>,
    sort_by_ref: Ref<X>,
    column_types_ref: Ref<X>,
    selection_ref: Ref<X>,
    sort_by: Vec<SortBy>,
    column_types: FxHashMap<String, ColumnSpec>,
    /// Set of selected row paths, controlled by graphix
    selection: FxHashSet<String>,
    on_activate_ref: Ref<X>,
    on_activate: Option<Callable<X>>,
    on_select_ref: Ref<X>,
    on_select: Option<Callable<X>>,
    on_header_click_ref: Ref<X>,
    on_header_click: Option<Callable<X>>,
    on_update_ref: Ref<X>,
    on_update: Option<Callable<X>>,
    /// Compiled callables for per-column on_edit/on_click
    col_callbacks: FxHashMap<String, Callable<X>>,
    /// Compiled refs for per-column default_value. The ref's `.last`
    /// holds the Value directly (uniform, `Value::Map`, or `Value::Null`);
    /// cells look up per-row entries from the stored Map on demand.
    default_value_refs: FxHashMap<String, Ref<X>>,
    /// Compiled refs for per-column width
    width_refs: FxHashMap<String, Ref<X>>,
    /// Ref-controlled widths (from width refs)
    ref_widths: FxHashMap<String, f32>,
    /// User-controlled widths (free resize or auto-sized on first load)
    user_widths: Mutex<FxHashMap<String, f32>>,
    /// Compiled on_resize callables per column
    on_resize_callbacks: FxHashMap<String, Callable<X>>,
    /// Compiled refs for per-column on_resize. The .gxi types on_resize
    /// as a `&` field, so the inner callable can change at runtime; we
    /// recompile the callable when the ref's value updates.
    on_resize_refs: FxHashMap<String, Ref<X>>,
    /// Active resize drag state
    resize_drag: Option<ResizeDrag>,
    /// Last resize handle click: (col_meta_idx, timestamp) for double-click detection
    last_resize_click: Option<(usize, Instant)>,
    mode: DisplayMode,
    col_names: Vec<String>,
    /// Names of columns that have no source in the table's `columns`
    /// array — they exist only as virtual columns whose values come
    /// from `default_value`. Populated in `apply_table`. Used by the
    /// default_value update path to know which cells to refresh
    /// regardless of whether the row has subscriptions for its
    /// non-virtual columns.
    virtual_cols: FxHashSet<String>,
    row_paths: Vec<Path>,
    /// Row name (basename) for each row in `row_paths`, as `ArcStr` so
    /// lookups that need the name as a key (e.g. Map keys for per-row
    /// defaults) don't allocate on every access. Kept in sync with
    /// `row_paths` — same length, same order.
    row_names: Vec<ArcStr>,
    cells: Arc<SharedCells>,
    row_subs: Vec<Option<Vec<Dval>>>,
    sub_start: usize,
    sub_end: usize,
    first_row: usize,
    first_col: usize,
    rows_in_view: usize,
    cols_in_view: usize,
    /// Last known viewport width in pixels (for column fit calculations).
    viewport_width: f32,
    /// Cached actual column widths from the last view() call, keyed by col_name.
    cached_col_widths: Mutex<FxHashMap<String, f32>>,
    /// Set when keyboard nav changes first_row/first_col. Suppresses
    /// the next handle_scroll from overwriting the keyboard-driven position.
    keyboard_scroll_override: bool,
    /// Which cell is being edited: (row_idx, col_name)
    editing: Option<(usize, String)>,
    /// Text buffer for the cell being edited
    edit_buffer: String,
    /// Sort column subscriptions: one Vec<Dval> per distinct column in
    /// `sort_by`, kept alive so their background tasks can feed
    /// `sort_col_values`.
    sort_col_subs: FxHashMap<String, Vec<Dval>>,
    /// Sort column values: column → row_path → display string,
    /// populated by sort subscriptions.
    sort_col_values: Arc<Mutex<FxHashMap<String, FxHashMap<String, String>>>>,
    /// Flag set by sort column subscription tasks when new data arrives.
    sort_col_dirty: Arc<AtomicBool>,
    needs_resolve: bool,
}

impl<X: GXExt> DataTableW<X> {
    pub(crate) async fn compile(gx: GXHandle<X>, source: Value) -> Result<GuiW<X>> {
        // Fields alphabetical: column_types, on_activate, on_header_click,
        // on_select, on_update, selection, show_row_name, sort_by, table
        let [
            (_, column_types_id),
            (_, on_activate_id),
            (_, on_header_click_id),
            (_, on_select_id),
            (_, on_update_id),
            (_, selection_id),
            (_, show_row_name_id),
            (_, sort_by_id),
            (_, table_id),
        ] = source
            .cast_to::<[(ArcStr, u64); 9]>()
            .context("data_table flds")?;
        let (
            column_types_ref,
            on_activate_ref,
            on_header_click_ref,
            on_select_ref,
            on_update_ref,
            selection_ref,
            show_row_name_ref,
            sort_by_ref,
            table_ref,
        ) = tokio::try_join!(
            gx.compile_ref(column_types_id),
            gx.compile_ref(on_activate_id),
            gx.compile_ref(on_header_click_id),
            gx.compile_ref(on_select_id),
            gx.compile_ref(on_update_id),
            gx.compile_ref(selection_id),
            gx.compile_ref(show_row_name_id),
            gx.compile_ref(sort_by_id),
            gx.compile_ref(table_id),
        )?;
        let on_activate = compile_callable_opt(&gx, &on_activate_ref).await?;
        let on_select = compile_callable_opt(&gx, &on_select_ref).await?;
        let on_header_click = compile_callable_opt(&gx, &on_header_click_ref).await?;
        let on_update = compile_callable_opt(&gx, &on_update_ref).await?;
        let sort_by = sort_by_ref.last.as_ref()
            .map(parse_sort_by).unwrap_or_default();
        let column_types_parsed = match column_types_ref.last.as_ref() {
            Some(Value::Null) | None => FxHashMap::default(),
            Some(v) => parse_column_specs(v),
        };
        let selection = selection_ref.last.as_ref()
            .map(parse_selection).unwrap_or_default();
        // Compile per-column callbacks, default_value refs, width refs, on_resize
        let mut col_callbacks = FxHashMap::default();
        let mut default_value_refs = FxHashMap::default();
        let mut width_refs = FxHashMap::default();
        let mut ref_widths = FxHashMap::default();
        let mut on_resize_callbacks = FxHashMap::default();
        let mut on_resize_refs = FxHashMap::default();
        for (name, spec) in &column_types_parsed {
            if let Some(cb_val) = &spec.callback_value {
                if let Ok(callable) = gx.compile_callable(cb_val.clone()).await {
                    col_callbacks.insert(name.clone(), callable);
                }
            }
            if spec.default_value_bid != 0 {
                if let Ok(r) = gx.compile_ref(spec.default_value_bid).await {
                    default_value_refs.insert(name.clone(), r);
                }
            }
            if spec.width_bid != 0 {
                if let Ok(r) = gx.compile_ref(spec.width_bid).await {
                    if let Some(w) = r.last.as_ref().and_then(|v| v.clone().cast_to::<f64>().ok()) {
                        ref_widths.insert(name.clone(), w as f32);
                    }
                    width_refs.insert(name.clone(), r);
                }
            }
            if spec.on_resize_bid != 0 {
                if let Ok(r) = gx.compile_ref(spec.on_resize_bid).await {
                    if let Ok(Some(c)) = compile_callable_opt(&gx, &r).await {
                        on_resize_callbacks.insert(name.clone(), c);
                    }
                    on_resize_refs.insert(name.clone(), r);
                }
            }
        }
        let subscriber = gx.subscriber();
        let rt = tokio::runtime::Handle::current();
        let show_row_name =
            TRef::new(show_row_name_ref).context("data_table tref show_row_name")?;
        let cells = Arc::new(SharedCells {
            grid: Mutex::new(vec![]),
            sparklines: Mutex::new(FxHashMap::default()),
            dirty: AtomicBool::new(false),
        });
        let mut w = Self {
            gx, subscriber, rt,
            table_ref, show_row_name,
            sort_by_ref, column_types_ref, selection_ref,
            sort_by, column_types: column_types_parsed, selection,
            on_activate_ref, on_activate,
            on_select_ref, on_select,
            on_header_click_ref, on_header_click,
            on_update_ref, on_update,
            col_callbacks,
            default_value_refs,
            width_refs,
            ref_widths,
            user_widths: Mutex::new(FxHashMap::default()),
            on_resize_callbacks,
            on_resize_refs,
            resize_drag: None,
            last_resize_click: None,
            mode: DisplayMode::Table,
            col_names: vec![],
            virtual_cols: FxHashSet::default(),
            row_paths: vec![], row_names: vec![],
            cells, row_subs: vec![],
            sub_start: 0, sub_end: 0,
            first_row: 0, first_col: 0,
            rows_in_view: DEFAULT_VISIBLE_ROWS,
            cols_in_view: DEFAULT_VISIBLE_COLS,
            viewport_width: 1024.0,
            cached_col_widths: Mutex::new(FxHashMap::default()),
            keyboard_scroll_override: false,
            editing: None, edit_buffer: String::new(),
            sort_col_subs: FxHashMap::default(),
            sort_col_values: Arc::new(Mutex::new(FxHashMap::default())),
            sort_col_dirty: Arc::new(AtomicBool::new(false)),
            needs_resolve: false,
        };
        w.apply_table();
        w.update_subscriptions();
        Ok(Box::new(w))
    }

    /// Parse the table ref value and rebuild the table structure.
    /// No resolver call — the user passes the table directly.
    fn apply_table(&mut self) {
        self.row_subs.clear();
        self.sub_start = 0;
        self.sub_end = 0;
        self.col_names.clear();
        self.row_paths.clear();
        self.row_names.clear();
        // Re-read selection from graphix ref (don't clear user selection)
        self.selection = self.selection_ref.last.as_ref()
            .map(parse_selection).unwrap_or_default();
        self.editing = None;
        self.first_row = 0;
        self.first_col = 0;
        let table_val = match self.table_ref.last.as_ref() {
            Some(v) if *v != Value::Null => v.clone(),
            _ => {
                self.needs_resolve = false;
                return;
            }
        };
        // Table is { rows: Array<string>, columns: Array<string> }
        // Fields alphabetical: columns, rows.
        //
        // Null bytes in incoming column names are stripped to prevent
        // collisions with `ROW_NAME_KEY` (which has a leading `\0`)
        // and similar internal sentinel keys used by width caches.
        let (raw_cols, raw_rows) = match table_val.cast_to::<[(ArcStr, Value); 2]>() {
            Ok([(_, cols_val), (_, rows_val)]) => {
                let mut cols: Vec<String> = cols_val
                    .cast_to::<Vec<String>>()
                    .unwrap_or_default();
                for c in cols.iter_mut() {
                    if c.contains('\0') {
                        c.retain(|ch| ch != '\0');
                    }
                }
                let rows: Vec<String> = rows_val.cast_to::<Vec<String>>()
                    .unwrap_or_default();
                (cols, rows)
            }
            Err(_) => {
                warn!("failed to parse table value");
                self.needs_resolve = false;
                return;
            }
        };
        // Convert row strings to Paths (for absolute netidx paths)
        // or keep as-is for virtual rows
        let raw_row_paths: Vec<Path> = raw_rows.into_iter()
            .map(|s| Path::from(s))
            .collect();
        self.col_names = raw_cols;
        self.row_paths = raw_row_paths;
        self.row_names = self.row_paths.iter()
            .map(|p| ArcStr::from(Path::basename(p).unwrap_or(&**p)))
            .collect();
        // Add virtual columns and record which columns are virtual.
        // A column is virtual when it's defined in `column_types` but
        // doesn't appear in the table's `columns` array — its value
        // comes solely from `default_value`, never from a netidx
        // subscription.
        self.virtual_cols.clear();
        for (name, _spec) in &self.column_types {
            if self.has_default(name) && !self.col_names.contains(name) {
                self.col_names.push(name.clone());
                self.virtual_cols.insert(name.clone());
            }
        }
        self.mode = if self.col_names.is_empty() && !self.row_paths.is_empty() {
            DisplayMode::Value
        } else {
            DisplayMode::Table
        };
        // Subscribe to every distinct column mentioned in `sort_by`, so
        // the row comparator has live cell values for each sort key.
        self.sort_col_subs.clear();
        self.sort_col_values.lock().clear();
        self.sort_col_dirty.store(false, Ordering::Relaxed);
        let sort_cols: FxHashSet<String> =
            self.sort_by.iter().map(|s| s.column.clone()).collect();
        for sort_col in sort_cols {
            let mut subs: Vec<Dval> = Vec::new();
            self.sort_col_values
                .lock()
                .entry(sort_col.clone())
                .or_default();
            for row_path in &self.row_paths {
                if !Path::is_absolute(row_path) {
                    continue;
                }
                let cell_path = row_path.append(&sort_col);
                let row_key = row_path.to_string();
                let (tx, mut rx) = futures::channel::mpsc::channel(2);
                let dval = self.subscriber.subscribe(cell_path);
                dval.updates(UpdatesFlags::BEGIN_WITH_LAST, tx);
                subs.push(dval);
                let values = self.sort_col_values.clone();
                let dirty = self.sort_col_dirty.clone();
                let col_name = sort_col.clone();
                self.rt.spawn(async move {
                    use futures::StreamExt;
                    while let Some(mut batch) = rx.next().await {
                        for (_, event) in batch.drain(..) {
                            if let Event::Update(v) = event {
                                values
                                    .lock()
                                    .entry(col_name.clone())
                                    .or_default()
                                    .insert(row_key.clone(), format_value(&v));
                                dirty.store(true, Ordering::Relaxed);
                            }
                        }
                    }
                });
            }
            self.sort_col_subs.insert(sort_col, subs);
        }
        let n_rows = self.row_paths.len();
        let n_cols = match self.mode {
            DisplayMode::Table => self.col_names.len(),
            DisplayMode::Value => 1,
        };
        // Initialize grid with default values
        let mut grid = Vec::with_capacity(n_rows);
        for row_name in &self.row_names {
            let mut row = Vec::with_capacity(n_cols);
            if self.mode == DisplayMode::Value {
                // Value mode: 1 cell per row (subscribed to row path directly)
                row.push(String::new());
            } else {
                for col_name in &self.col_names {
                    row.push(self.default_for(col_name, row_name));
                }
            }
            grid.push(row);
        }
        *self.cells.grid.lock() = grid;
        // Don't clear sparkline history — push_defaults_to_sparklines adds
        // new points. Full clear only happens when row_paths/col_names change
        // (which they do during apply_table), so stale (row, col) keys in the
        // sparkline map just won't be rendered.
        self.push_defaults_to_sparklines();
        self.cells.dirty.store(false, Ordering::Relaxed);
        self.row_subs = (0..n_rows).map(|_| None).collect();
        // Apply column sort using default values (for virtual columns/rows).
        // Subscribed sort column data will trigger resort_by_column later.
        if !self.sort_by.is_empty() {
            self.resort_by_column();
        }
        self.needs_resolve = false;
    }

    fn display_row_range(&self) -> (usize, usize) {
        let n = self.row_paths.len();
        if n == 0 { return (0, 0); }
        let start = self.first_row.min(n.saturating_sub(1));
        let end = (start + self.rows_in_view).min(n);
        (start, end)
    }

    fn subscription_row_range(&self) -> (usize, usize) {
        let n = self.row_paths.len();
        let (ds, de) = self.display_row_range();
        (ds.saturating_sub(ROW_BUFFER), (de + ROW_BUFFER).min(n))
    }

    fn display_col_range(&self) -> (usize, usize) {
        let total = self.total_data_cols();
        if total == 0 { return (0, 0); }
        let start = self.first_col.min(total.saturating_sub(1));
        let end = (start + self.cols_in_view).min(total);
        (start, end)
    }

    fn total_data_cols(&self) -> usize {
        match self.mode {
            DisplayMode::Table => self.col_names.len(),
            DisplayMode::Value => 1,
        }
    }

    fn update_subscriptions(&mut self) {
        let (new_start, new_end) = self.subscription_row_range();
        if new_start == self.sub_start && new_end == self.sub_end { return; }
        for i in self.sub_start..self.sub_end {
            if (i < new_start || i >= new_end) && i < self.row_subs.len() {
                self.row_subs[i] = None;
            }
        }
        for i in new_start..new_end {
            if i < self.row_subs.len() && self.row_subs[i].is_none() {
                self.subscribe_row(i);
            }
        }
        self.sub_start = new_start;
        self.sub_end = new_end;
    }

    fn subscribe_row(&mut self, row_idx: usize) {
        let row_path = &self.row_paths[row_idx];
        // Only subscribe if the row is an absolute netidx path
        if !Path::is_absolute(row_path) {
            return;
        }
        let cells = self.cells.clone();
        let on_update_call = self.on_update.as_ref()
            .map(|c| (self.gx.clone(), c.id()));
        match self.mode {
            DisplayMode::Table => {
                let mut dvals = Vec::with_capacity(self.col_names.len());
                for (col_idx, col_name) in self.col_names.iter().enumerate() {
                    let is_sparkline = matches!(
                        self.col_type_for(col_name),
                        ColumnType::Sparkline { .. }
                    );
                    let history_secs = match self.col_type_for(col_name) {
                        ColumnType::Sparkline { history_seconds } => *history_seconds,
                        _ => 0.0,
                    };
                    let sparkline_key = (row_path.to_string(), col_name.to_string());
                    let cell_path = row_path.append(col_name);
                    let cell_path_arc = ArcStr::from(&*cell_path);
                    let (tx, mut rx) = futures::channel::mpsc::channel(2);
                    let dval = self.subscriber.subscribe(cell_path);
                    dval.updates(UpdatesFlags::BEGIN_WITH_LAST, tx);
                    dvals.push(dval);
                    let cells = cells.clone();
                    let on_update_call = on_update_call.clone();
                    self.rt.spawn(async move {
                        use futures::StreamExt;
                        while let Some(mut batch) = rx.next().await {
                            let mut grid = cells.grid.lock();
                            for (_, event) in batch.drain(..) {
                                if let Event::Update(v) = event {
                                    if row_idx < grid.len() && col_idx < grid[row_idx].len() {
                                        grid[row_idx][col_idx] = format_value(&v);
                                        cells.dirty.store(true, Ordering::Relaxed);
                                    }
                                    // Accumulate sparkline history
                                    if is_sparkline {
                                        if let Some(f) = value_to_f64(&v) {
                                            let now = Instant::now();
                                            let mut sp = cells.sparklines.lock();
                                            let history = sp
                                                .entry(sparkline_key.clone())
                                                .or_insert_with(VecDeque::new);
                                            history.push_back((now, f));
                                            // Trim entries older than history window
                                            let cutoff = now - std::time::Duration::from_secs_f64(history_secs);
                                            while history.front().map(|(t, _)| *t < cutoff).unwrap_or(false) {
                                                history.pop_front();
                                            }
                                            // Decimate when exceeding max points.
                                            // Merge adjacent pairs, keeping the value
                                            // with greater absolute deviation from the
                                            // local mean. This preserves peaks/valleys.
                                            if history.len() > MAX_SPARKLINE_POINTS {
                                                decimate_sparkline(history);
                                            }
                                        }
                                    }
                                    if let Some((ref gx, cb_id)) = on_update_call {
                                        let _ = gx.call(cb_id, ValArray::from_iter([
                                            Value::String(cell_path_arc.clone()),
                                            v,
                                        ]));
                                    }
                                }
                            }
                        }
                    });
                }
                self.row_subs[row_idx] = Some(dvals);
            }
            DisplayMode::Value => {
                let (tx, mut rx) = futures::channel::mpsc::channel(2);
                let dval = self.subscriber.subscribe(row_path.clone());
                let row_path_arc = ArcStr::from(&**row_path);
                dval.updates(UpdatesFlags::BEGIN_WITH_LAST, tx);
                let cells = cells.clone();
                self.rt.spawn(async move {
                    use futures::StreamExt;
                    while let Some(mut batch) = rx.next().await {
                        let mut grid = cells.grid.lock();
                        for (_, event) in batch.drain(..) {
                            if let Event::Update(v) = event {
                                if row_idx < grid.len() && !grid[row_idx].is_empty() {
                                    grid[row_idx][0] = format_value(&v);
                                    cells.dirty.store(true, Ordering::Relaxed);
                                }
                                if let Some((ref gx, cb_id)) = on_update_call {
                                    let _ = gx.call(cb_id, ValArray::from_iter([
                                        Value::String(row_path_arc.clone()),
                                        v,
                                    ]));
                                }
                            }
                        }
                    }
                });
                self.row_subs[row_idx] = Some(vec![dval]);
            }
        }
    }

    /// Get the sort value for a row in the sort column.
    /// Checks subscribed values first, then falls back to default_value
    /// from column_types (for virtual columns/rows).
    /// Push current default values into sparkline histories for all
    /// sparkline columns. Called when column_types updates reactively
    /// to record the new values as data points.
    fn push_defaults_to_sparklines(&self) {
        if self.mode != DisplayMode::Table {
            return;
        }
        let now = Instant::now();
        let mut sp = self.cells.sparklines.lock();
        for (_col_idx, col_name) in self.col_names.iter().enumerate() {
            if !matches!(self.col_type_for(col_name), ColumnType::Sparkline { .. }) {
                continue;
            }
            let history_secs = match self.col_type_for(col_name) {
                ColumnType::Sparkline { history_seconds } => *history_seconds,
                _ => unreachable!(),
            };
            for (row_idx, row_path) in self.row_paths.iter().enumerate() {
                let row_name = &self.row_names[row_idx];
                let val_str = self.default_for(col_name, row_name);
                if val_str.is_empty() { continue; }
                if let Ok(f) = val_str.parse::<f64>() {
                    let key = (row_path.to_string(), col_name.to_string());
                    let history = sp
                        .entry(key)
                        .or_insert_with(VecDeque::new);
                    history.push_back((now, f));
                    // Trim old entries
                    let cutoff = now - std::time::Duration::from_secs_f64(history_secs);
                    while history.front().map(|(t, _)| *t < cutoff).unwrap_or(false) {
                        history.pop_front();
                    }
                    if history.len() > MAX_SPARKLINE_POINTS {
                        decimate_sparkline(history);
                    }
                }
            }
        }
    }

    fn sort_value_for(&self, row_idx: usize, sort_col: &str) -> String {
        let row_path = &self.row_paths[row_idx];
        // Check subscribed sort column values (column → row → value)
        let vals = self.sort_col_values.lock();
        if let Some(col_map) = vals.get(sort_col) {
            if let Some(v) = col_map.get(&**row_path) {
                return v.clone();
            }
        }
        drop(vals);
        // Fall back to default value
        self.default_for(sort_col, &self.row_names[row_idx])
    }

    /// Re-sort row_paths by sort column values and rebuild the cell grid.
    /// Called when sort_col_dirty is set, meaning new sort data arrived,
    /// or during apply_table when sort_by is non-empty.
    fn resort_by_column(&mut self) {
        if self.sort_by.is_empty() {
            return;
        }
        let n = self.row_paths.len();
        // Precompute sort keys: keys[row][sort_by_index] = display string
        let keys: Vec<Vec<String>> = (0..n)
            .map(|i| {
                self.sort_by
                    .iter()
                    .map(|sb| self.sort_value_for(i, &sb.column))
                    .collect()
            })
            .collect();
        let mut indices: Vec<usize> = (0..n).collect();
        indices.sort_by(|&a, &b| {
            for (idx, sb) in self.sort_by.iter().enumerate() {
                let va = keys[a][idx].as_str();
                let vb = keys[b][idx].as_str();
                let cmp = match (numeric_key(va), numeric_key(vb)) {
                    (Some(na), Some(nb)) => na
                        .partial_cmp(&nb)
                        .unwrap_or(std::cmp::Ordering::Equal),
                    _ => va.cmp(vb),
                };
                if cmp != std::cmp::Ordering::Equal {
                    return if sb.ascending { cmp } else { cmp.reverse() };
                }
            }
            std::cmp::Ordering::Equal
        });
        // Reorder row_paths and row_names in lockstep
        let old_paths = self.row_paths.clone();
        let old_names = self.row_names.clone();
        for (new_i, &old_i) in indices.iter().enumerate() {
            self.row_paths[new_i] = old_paths[old_i].clone();
            self.row_names[new_i] = old_names[old_i].clone();
        }
        // Rebuild the grid for the new row order
        let n_cols = match self.mode {
            DisplayMode::Table => self.col_names.len(),
            DisplayMode::Value => 1,
        };
        let mut grid = Vec::with_capacity(self.row_paths.len());
        for row_name in &self.row_names {
            let mut row = Vec::with_capacity(n_cols);
            if self.mode == DisplayMode::Value {
                row.push(String::new());
            } else {
                for col_name in &self.col_names {
                    row.push(self.default_for(col_name, row_name));
                }
            }
            grid.push(row);
        }
        *self.cells.grid.lock() = grid;
        // Clear existing row subscriptions so virtualization re-subscribes
        // in the new order
        for sub in self.row_subs.iter_mut() {
            *sub = None;
        }
        self.sub_start = 0;
        self.sub_end = 0;
        self.update_subscriptions();
    }

    fn fire_on_select(&self, row_idx: usize, col_name: &str) {
        if let Some(callable) = &self.on_select {
            if let Some(row_path) = self.row_paths.get(row_idx) {
                // Send the full cell path: row_path/col_name
                let cell_path = if col_name == ROW_NAME_KEY {
                    ArcStr::from(&**row_path)
                } else {
                    ArcStr::from(format!("{}/{}", &**row_path, col_name))
                };
                let pv = Value::String(cell_path);
                let _ = self.gx.call(callable.id(), ValArray::from_iter([pv]));
            }
        }
    }

    /// Get the default value string for a cell, reading from the column's
    /// stored default_value ref. A `Value::Map` is treated as a per-row
    /// table (key = row basename); anything else is a uniform value.
    fn default_for(&self, col_name: &str, row_name: &ArcStr) -> String {
        let Some(r) = self.default_value_refs.get(col_name) else {
            return String::new();
        };
        match r.last.as_ref() {
            None | Some(Value::Null) => String::new(),
            Some(Value::Map(m)) => {
                let key = Value::String(row_name.clone());
                m.get(&key).map(value_to_display).unwrap_or_default()
            }
            Some(v) => value_to_display(v),
        }
    }

    /// Check if a column has any non-null default value.
    fn has_default(&self, col_name: &str) -> bool {
        matches!(
            self.default_value_refs.get(col_name).and_then(|r| r.last.as_ref()),
            Some(v) if !matches!(v, Value::Null)
        )
    }

    /// Returns the effective column width if explicitly set (by ref or user drag).
    /// None means auto-size from content.
    fn effective_col_width(&self, col_name: &str) -> Option<f32> {
        // 1. User-set width (from drag resize)
        if let Some(w) = self.user_widths.lock().get(col_name) {
            return Some(*w);
        }
        // 2. Ref-controlled width
        self.ref_widths.get(col_name).copied()
    }

    /// Compute how many data columns fit from first_col given actual cached widths
    /// and the viewport width.
    fn actual_visible_cols(&self, from_col: usize, vp_width: f32) -> usize {
        let show_name = self.show_row_name.t.unwrap_or(true);
        let cache = self.cached_col_widths.lock();
        let mut used = if show_name {
            cache.get(ROW_NAME_KEY).copied().unwrap_or(MIN_COL_WIDTH)
        } else {
            0.0
        };
        let mut count = 0;
        for i in from_col..self.col_names.len() {
            let w = cache.get(&self.col_names[i])
                .copied()
                .unwrap_or(MIN_COL_WIDTH);
            if used + w > vp_width && count > 0 {
                break;
            }
            used += w;
            count += 1;
        }
        count.max(1)
    }

    /// Scroll viewport to ensure cell at (row_idx, col_name) is visible.
    fn scroll_to_cell(&mut self, row: usize, col_name: &str) {
        let mut changed = false;
        // Row scroll
        if row < self.first_row {
            self.first_row = row;
            changed = true;
        } else if row >= self.first_row + self.rows_in_view {
            self.first_row = row.saturating_sub(self.rows_in_view.saturating_sub(1));
            changed = true;
        }
        // Column scroll using actual cached widths (skip when
        // scrolling to the synthesized row-name column, which is
        // always at the viewport's left edge).
        if col_name != ROW_NAME_KEY {
            if let Some(ci) = self.col_names.iter().position(|n| n == col_name) {
                if ci < self.first_col {
                    self.first_col = ci;
                    changed = true;
                } else {
                    // Use actual widths to check if the column fits in viewport
                    let vis = self.actual_visible_cols(self.first_col, self.viewport_width);
                    if ci >= self.first_col + vis {
                        // Scroll right so ci is the last visible column
                        self.first_col = ci.saturating_sub(
                            self.actual_visible_cols(
                                ci.saturating_sub(self.cols_in_view),
                                self.viewport_width,
                            ).saturating_sub(1)
                        );
                        changed = true;
                    }
                }
            }
        }
        if changed {
            self.keyboard_scroll_override = true;
            self.update_subscriptions();
        }
    }

    /// Ensure at least one selected cell is visible in the viewport.
    fn ensure_selection_visible(&mut self) {
        if self.selection.is_empty() { return; }
        let show_name = self.show_row_name.t.unwrap_or(true);
        for sel_path in self.selection.clone() {
            for (ri, rp) in self.row_paths.iter().enumerate() {
                if show_name && sel_path == &**rp {
                    self.scroll_to_cell(ri, ROW_NAME_KEY);
                    return;
                }
                let prefix = format!("{}/", &**rp);
                if let Some(col_name) = sel_path.strip_prefix(&prefix) {
                    self.scroll_to_cell(ri, col_name);
                    return;
                }
            }
        }
    }

    /// Auto-fit all columns to their max content width (no cap).
    /// Scans ALL rows, not just visible ones.
    fn auto_fit_all_columns(&mut self) {
        let grid = self.cells.grid.lock();
        let show_name = self.show_row_name.t.unwrap_or(true);
        let mut widths = self.user_widths.lock();
        if show_name {
            let mut w = col_header_width(ROW_NAME_LABEL).max(MIN_COL_WIDTH);
            for p in &self.row_paths {
                let name = Path::basename(p).unwrap_or("");
                w = w.max(col_text_width(name).max(MIN_COL_WIDTH));
            }
            widths.insert(ROW_NAME_KEY.into(), w);
        }
        match self.mode {
            DisplayMode::Table => {
                for (col_idx, col_name) in self.col_names.iter().enumerate() {
                    // Skip columns with fixed ref width and no on_resize
                    let is_fixed = self.ref_widths.contains_key(col_name)
                        && !self.on_resize_callbacks.contains_key(col_name);
                    if is_fixed { continue; }
                    let display = self.column_types.get(col_name)
                        .and_then(|s| s.display_name.as_deref())
                        .unwrap_or(col_name);
                    let mut w = col_header_width(display).max(MIN_COL_WIDTH);
                    for row in grid.iter() {
                        if col_idx < row.len() {
                            w = w.max(col_text_width(&row[col_idx]).max(MIN_COL_WIDTH));
                        }
                    }
                    widths.insert(col_name.clone(), w);
                }
            }
            DisplayMode::Value => {
                let mut w = col_header_width("value").max(MIN_COL_WIDTH);
                for row in grid.iter() {
                    if let Some(v) = row.first() {
                        w = w.max(col_text_width(v).max(MIN_COL_WIDTH));
                    }
                }
                widths.insert("value".into(), w);
            }
        }
    }

    fn col_type_for(&self, col_name: &str) -> &ColumnType {
        self.column_types.get(col_name)
            .map(|s| &s.typ)
            .unwrap_or(&ColumnType::Text)
    }

}

async fn compile_callable_opt<X: GXExt>(
    gx: &GXHandle<X>,
    r: &Ref<X>,
) -> Result<Option<Callable<X>>> {
    match r.last.as_ref() {
        Some(Value::Null) | None => Ok(None),
        Some(v) => Ok(Some(gx.compile_callable(v.clone()).await?)),
    }
}

struct NakedValue<'a>(&'a Value);
impl std::fmt::Display for NakedValue<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt_naked(f)
    }
}
fn format_value(v: &Value) -> String {
    // Strings are displayed bare (no surrounding quotes). fmt_naked
    // quotes strings for parser round-tripping, but spreadsheet cells
    // look much better without the quotes, and Combo's value lookup
    // matches raw ids.
    match v {
        Value::Null => String::new(),
        Value::String(s) => s.to_string(),
        _ => NakedValue(v).to_string(),
    }
}

/// Parse the user-typed edit buffer as a graphix value. If parsing
/// succeeds we commit the typed value (e.g. `42` → `i64`, `true` →
/// `bool`, `duration:1.s` → `Duration`). If parsing fails we commit
/// the buffer as a bare string, so users can type `hello` without
/// needing to wrap it in quotes.
fn parse_or_quote(s: &str) -> Value {
    netidx::protocol::value_parser::parse_value(s)
        .unwrap_or_else(|_| Value::String(ArcStr::from(s)))
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

fn value_to_f64(v: &Value) -> Option<f64> {
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

type Col<'a> = widget::Column<'a, Message, GraphixTheme, Renderer>;
type Row<'a> = widget::Row<'a, Message, GraphixTheme, Renderer>;

/// Canvas program that draws a sparkline polyline.
struct SparklineCanvas {
    /// (time_offset_secs, value) pairs
    points: Vec<(f64, f64)>,
}

impl<Message> widget::canvas::Program<Message, GraphixTheme, Renderer> for SparklineCanvas {
    type State = ();

    fn draw(
        &self,
        _state: &Self::State,
        renderer: &Renderer,
        _theme: &GraphixTheme,
        bounds: iced_core::Rectangle,
        _cursor: iced_core::mouse::Cursor,
    ) -> Vec<widget::canvas::Geometry<Renderer>> {
        use widget::canvas::{Frame, Path, Stroke};
        let mut frame = Frame::new(renderer, bounds.size());
        if self.points.len() < 2 {
            return vec![frame.into_geometry()];
        }
        // Find value range
        let (mut min_v, mut max_v) = (f64::MAX, f64::MIN);
        let (min_t, max_t) = (self.points.first().unwrap().0, self.points.last().unwrap().0);
        for &(_, v) in &self.points {
            if v < min_v { min_v = v; }
            if v > max_v { max_v = v; }
        }
        if (max_v - min_v).abs() < 1e-12 {
            min_v -= 1.0;
            max_v += 1.0;
        }
        let t_range = (max_t - min_t).max(1e-12);
        let v_range = max_v - min_v;
        let w = bounds.width;
        let h = bounds.height;
        // Decimate: if more points than pixels, sample
        let max_points = w as usize;
        let step = if self.points.len() > max_points {
            self.points.len() / max_points
        } else {
            1
        };
        // Build path
        let path = Path::new(|builder| {
            let mut first = true;
            for (i, &(t, v)) in self.points.iter().enumerate() {
                if i % step != 0 && i != self.points.len() - 1 {
                    continue;
                }
                let x = ((t - min_t) / t_range) as f32 * w;
                let y = h - ((v - min_v) / v_range) as f32 * h;
                if first {
                    builder.move_to(iced_core::Point::new(x, y));
                    first = false;
                } else {
                    builder.line_to(iced_core::Point::new(x, y));
                }
            }
        });
        frame.stroke(
            &path,
            Stroke::default()
                .with_color(iced_core::Color::from_rgb(0.3, 0.7, 1.0))
                .with_width(1.5),
        );
        vec![frame.into_geometry()]
    }
}

impl<X: GXExt> GuiWidget<X> for DataTableW<X> {
    fn handle_update(&mut self, rt: &tokio::runtime::Handle, id: ExprId, v: &Value) -> Result<bool> {
        let mut changed = false;
        if id == self.table_ref.id {
            self.table_ref.last = Some(v.clone());
            self.needs_resolve = true;
            changed = true;
        }
        if id == self.sort_by_ref.id {
            self.sort_by_ref.last = Some(v.clone());
            self.sort_by = parse_sort_by(v);
            self.needs_resolve = true;
            changed = true;
        }
        if id == self.column_types_ref.id {
            self.column_types_ref.last = Some(v.clone());
            match v {
                Value::Null => {
                    self.column_types.clear();
                    self.col_callbacks.clear();
                    self.on_resize_callbacks.clear();
                    self.on_resize_refs.clear();
                    self.default_value_refs.clear();
                }
                v => {
                    self.column_types = parse_column_specs(v);
                    self.col_callbacks.clear();
                    self.on_resize_callbacks.clear();
                    self.on_resize_refs.clear();
                    self.default_value_refs.clear();
                    for (name, spec) in &self.column_types {
                        if let Some(cb_val) = &spec.callback_value {
                            if let Ok(callable) = rt.block_on(self.gx.compile_callable(cb_val.clone())) {
                                self.col_callbacks.insert(name.clone(), callable);
                            }
                        }
                        if spec.on_resize_bid != 0 {
                            if let Ok(r) = rt.block_on(self.gx.compile_ref(spec.on_resize_bid)) {
                                if let Ok(Some(c)) = rt.block_on(
                                    compile_callable_opt(&self.gx, &r),
                                ) {
                                    self.on_resize_callbacks.insert(name.clone(), c);
                                }
                                self.on_resize_refs.insert(name.clone(), r);
                            }
                        }
                        if spec.width_bid != 0 {
                            if let Ok(r) = rt.block_on(self.gx.compile_ref(spec.width_bid)) {
                                if let Some(w) = r.last.as_ref().and_then(|rv| rv.clone().cast_to::<f64>().ok()) {
                                    self.ref_widths.insert(name.clone(), w as f32);
                                }
                                self.width_refs.insert(name.clone(), r);
                            }
                        }
                        if spec.default_value_bid != 0 {
                            if let Ok(r) = rt.block_on(self.gx.compile_ref(spec.default_value_bid)) {
                                self.default_value_refs.insert(name.clone(), r);
                            }
                        }
                    }
                }
            }
            self.needs_resolve = true;
            changed = true;
        }
        if id == self.selection_ref.id {
            self.selection_ref.last = Some(v.clone());
            self.selection = parse_selection(v);
            self.ensure_selection_visible();
            changed = true;
        }
        if self.needs_resolve {
            self.apply_table();
            self.update_subscriptions();
            changed = true;
        }
        changed |= self.show_row_name.update(id, v).context("show_row_name")?.is_some();
        macro_rules! update_cb {
            ($ref:ident, $field:ident) => {
                if id == self.$ref.id {
                    self.$ref.last = Some(v.clone());
                    self.$field = rt.block_on(compile_callable_opt(&self.gx, &self.$ref))?;
                }
            }
        }
        update_cb!(on_activate_ref, on_activate);
        update_cb!(on_select_ref, on_select);
        update_cb!(on_header_click_ref, on_header_click);
        update_cb!(on_update_ref, on_update);
        if self.cells.dirty.swap(false, Ordering::Relaxed) { changed = true; }
        // Check default_value ref updates (independent of column_types structure).
        // Find which column (if any) this update belongs to.
        let dv_col = self.default_value_refs.iter_mut()
            .find(|(_, r)| id == r.id)
            .map(|(name, r)| {
                r.last = Some(v.clone());
                name.clone()
            });
        if let Some(col_name) = dv_col {
            // Update grid cells with new default values. default_for
            // reads from the ref's .last we just wrote, so
            // Map/uniform/null are all handled uniformly — whenever
            // the graphix-side map literal re-evaluates (because any
            // bound name inside it changed), this handler propagates
            // the new values to the grid.
            //
            // For non-virtual columns the cell is normally populated
            // by a netidx subscription; we only let the default fill
            // it in if there's no active subscription on this row.
            // For virtual columns (no source in the table's `columns`
            // array) the default IS the value, so we always overwrite
            // — without this exception, subscribed rows whose other
            // columns are subscribed would never see updates to the
            // virtual column's default.
            let is_virtual = self.virtual_cols.contains(&col_name);
            let mut grid = self.cells.grid.lock();
            if let Some(col_idx) = self.col_names.iter().position(|n| *n == col_name) {
                for (row_idx, row_path) in self.row_paths.iter().enumerate() {
                    let unsubscribed = !Path::is_absolute(row_path)
                        || self.row_subs.get(row_idx).map(|s| s.is_none()).unwrap_or(true);
                    if is_virtual || unsubscribed {
                        let row_name = &self.row_names[row_idx];
                        if row_idx < grid.len() && col_idx < grid[row_idx].len() {
                            grid[row_idx][col_idx] = self.default_for(&col_name, row_name);
                        }
                    }
                }
            }
            drop(grid);
            self.push_defaults_to_sparklines();
            changed = true;
        }
        // Check width ref updates
        let width_col = self.width_refs.iter_mut()
            .find(|(_, r)| id == r.id)
            .map(|(name, r)| { r.last = Some(v.clone()); name.clone() });
        if let Some(col_name) = width_col {
            match v.clone().cast_to::<f64>() {
                Ok(w) => { self.ref_widths.insert(col_name, w as f32); }
                Err(_) => { self.ref_widths.remove(&col_name); }
            }
            changed = true;
        }
        // Check on_resize ref updates: the function inside the ref may
        // be swapped, set to null, or initialized from null.
        let on_resize_col = self.on_resize_refs.iter_mut()
            .find(|(_, r)| id == r.id)
            .map(|(name, r)| { r.last = Some(v.clone()); name.clone() });
        if let Some(col_name) = on_resize_col {
            match v {
                Value::Null => { self.on_resize_callbacks.remove(&col_name); }
                v => {
                    match rt.block_on(self.gx.compile_callable(v.clone())) {
                        Ok(c) => { self.on_resize_callbacks.insert(col_name, c); }
                        Err(_) => { self.on_resize_callbacks.remove(&col_name); }
                    }
                }
            }
            changed = true;
        }
        // Check if sort column data arrived and needs re-sort
        if self.sort_col_dirty.swap(false, Ordering::Relaxed) {
            self.resort_by_column();
            changed = true;
        }
        self.update_subscriptions();
        Ok(changed)
    }

    #[cfg(test)]
    fn data_table_snapshot(&self) -> Option<super::DataTableSnapshot> {
        let mut sel: Vec<String> = self.selection.iter().cloned().collect();
        sel.sort();
        Some(super::DataTableSnapshot {
            col_names: self.col_names.clone(),
            row_basenames: self.row_paths.iter()
                .map(|p| Path::basename(p).unwrap_or(&**p).to_string())
                .collect(),
            grid: self.cells.grid.lock().clone(),
            is_value_mode: self.mode == DisplayMode::Value,
            selection: sel,
        })
    }

    #[cfg(test)]
    fn as_any(&self) -> &dyn std::any::Any { self }

    fn handle_table_key(&mut self, action: &super::TableKeyAction) -> bool {
        use super::TableKeyAction;
        let n_rows = self.row_paths.len();
        if n_rows == 0 { return false; }
        let show_name = self.show_row_name.t.unwrap_or(true);
        let name_offset = if show_name { 1 } else { 0 };
        // Build the FULL column list (not just visible) for keyboard nav.
        // Keyboard nav needs to move beyond the visible range and scroll.
        let mut display_cols: Vec<String> = Vec::new();
        if show_name {
            display_cols.push(ROW_NAME_KEY.to_string());
        }
        match self.mode {
            DisplayMode::Table => {
                for name in &self.col_names {
                    display_cols.push(name.clone());
                }
            }
            DisplayMode::Value => {
                display_cols.push("value".to_string());
            }
        }
        let n_display_cols = display_cols.len();
        if n_display_cols == 0 { return false; }
        // Find current position from the selection set.
        // Selection contains cell paths like "row_path/col_name" or "row_path" for name col.
        let (cur_row, cur_col) = self.selection.iter()
            .find_map(|sel_path| {
                for (ri, rp) in self.row_paths.iter().enumerate() {
                    // Check "row_path/col_name" format
                    for (ci, col_name) in display_cols.iter().enumerate() {
                        let cell_path = if col_name == ROW_NAME_KEY {
                            rp.to_string()
                        } else {
                            format!("{}/{}", &**rp, col_name)
                        };
                        if *sel_path == cell_path {
                            return Some((ri, ci));
                        }
                    }
                }
                None
            })
            .unwrap_or((0, name_offset));
        match action {
            TableKeyAction::Up | TableKeyAction::Down
            | TableKeyAction::Left | TableKeyAction::Right => {
                // Clamp c to data columns only (skip name column)
                let min_col = name_offset;
                let (mut r, mut c) = (cur_row, cur_col.max(min_col));
                match action {
                    TableKeyAction::Up => { r = r.saturating_sub(1); }
                    TableKeyAction::Down => { r = (r + 1).min(n_rows - 1); }
                    TableKeyAction::Left => {
                        if c > min_col { c -= 1; }
                        else if r > 0 { r -= 1; c = n_display_cols - 1; }
                    }
                    TableKeyAction::Right => {
                        if c + 1 < n_display_cols { c += 1; }
                        else if r + 1 < n_rows { r += 1; c = min_col; }
                    }
                    _ => unreachable!(),
                }
                let col_name = &display_cols[c];
                self.fire_on_select(r, col_name);
                // Scroll to keep the target cell visible (optimistic,
                // don't wait for graphix round-trip)
                self.scroll_to_cell(r, col_name);
                true
            }
            TableKeyAction::Enter => {
                if let Some(callable) = &self.on_activate {
                    if let Some(path) = self.row_paths.get(cur_row) {
                        let pv = Value::String(ArcStr::from(&**path));
                        let _ = self.gx.call(callable.id(), ValArray::from_iter([pv]));
                    }
                }
                true
            }
            TableKeyAction::Space => {
                if cur_col >= name_offset {
                    let col_name = &display_cols[cur_col];
                    if self.col_callbacks.contains_key(col_name) {
                        self.handle_cell_edit(cur_row, col_name.clone());
                    }
                }
                true
            }
            TableKeyAction::Escape => {
                self.handle_cell_edit_cancel();
                true
            }
        }
    }

    fn handle_cell_edit(&mut self, row: usize, col: String) -> bool {
        self.editing = Some((row, col.clone()));
        // Initialize edit buffer with current cell value
        let col_idx = self.col_names.iter().position(|n| *n == col);
        if let Some(ci) = col_idx {
            let grid = self.cells.grid.lock();
            if row < grid.len() && ci < grid[row].len() {
                self.edit_buffer = grid[row][ci].clone();
            } else {
                self.edit_buffer.clear();
            }
        } else {
            self.edit_buffer.clear();
        }
        true
    }

    fn handle_cell_edit_input(&mut self, text: String) -> bool {
        self.edit_buffer = text;
        true
    }

    fn handle_cell_edit_submit(&mut self) -> bool {
        if let Some((row, ref col)) = self.editing {
            if let Some(callable) = self.col_callbacks.get(col) {
                if let Some(path) = self.row_paths.get(row) {
                    let cell_path = path.append(col);
                    let v = parse_or_quote(&self.edit_buffer);
                    let _ = self.gx.call(
                        callable.id(),
                        ValArray::from_iter([
                            Value::String(ArcStr::from(&*cell_path)),
                            v,
                        ]),
                    );
                }
            }
        }
        self.editing = None;
        self.edit_buffer.clear();
        true
    }

    fn handle_cell_edit_cancel(&mut self) -> bool {
        self.editing = None;
        self.edit_buffer.clear();
        true
    }

    fn handle_viewport_resize(&mut self, vp_w: f32, vp_h: f32) -> bool {
        let rows_in_view = ((vp_h / ROW_HEIGHT_ESTIMATE).ceil() as usize).max(1);
        let name_cols = if self.show_row_name.t.unwrap_or(true) { 1 } else { 0 };
        let cols_in_view = ((vp_w / MIN_COL_WIDTH).ceil() as usize)
            .saturating_sub(name_cols).max(1);
        if self.rows_in_view == rows_in_view && self.cols_in_view == cols_in_view {
            return false;
        }
        self.rows_in_view = rows_in_view;
        self.cols_in_view = cols_in_view;
        self.viewport_width = vp_w;
        self.update_subscriptions();
        true
    }

    fn handle_cell_click(&mut self, row: usize, col: String) -> bool {
        // Name column click fires on_activate. Accept the display
        // label `"name"` as a synonym for the row-name col so tests
        // and external callers don't need to know the internal
        // sentinel key.
        if col == ROW_NAME_KEY || col == ROW_NAME_LABEL {
            if let Some(callable) = &self.on_activate {
                if let Some(path) = self.row_paths.get(row) {
                    let pv = Value::String(ArcStr::from(&**path));
                    let _ = self.gx.call(callable.id(), ValArray::from_iter([pv]));
                    return true;
                }
            }
        }
        // Any cell click fires on_select with the cell path
        self.fire_on_select(row, &col);
        true
    }

    fn handle_scroll(&mut self, ox: f32, oy: f32, vp_w: f32, vp_h: f32) -> bool {
        if self.keyboard_scroll_override {
            // Check if this is a real user scroll (position actually changed
            // from what we'd expect) or just the overlay re-asserting.
            let expected_ox = self.first_col as f32 * MIN_COL_WIDTH;
            let expected_oy = self.first_row as f32 * ROW_HEIGHT_ESTIMATE;
            let real_scroll = (ox - expected_ox).abs() > MIN_COL_WIDTH * 0.5
                || (oy - expected_oy).abs() > ROW_HEIGHT_ESTIMATE * 0.5;
            if !real_scroll {
                // Just the overlay re-asserting — keep keyboard position
                return false;
            }
            // Real user scroll — clear override and process normally
            self.keyboard_scroll_override = false;
        }
        let n_rows = self.row_paths.len();
        let n_cols = self.total_data_cols();
        let new_first_row = ((oy / ROW_HEIGHT_ESTIMATE).round() as usize).min(n_rows.saturating_sub(1));
        let new_first_col = ((ox / MIN_COL_WIDTH).round() as usize).min(n_cols.saturating_sub(1));
        let rows_in_view = ((vp_h / ROW_HEIGHT_ESTIMATE).ceil() as usize).max(1);
        let name_cols = if self.show_row_name.t.unwrap_or(true) { 1 } else { 0 };
        let cols_in_view = ((vp_w / MIN_COL_WIDTH).ceil() as usize)
            .saturating_sub(name_cols).max(1);
        let changed = self.first_row != new_first_row
            || self.first_col != new_first_col
            || self.rows_in_view != rows_in_view
            || self.cols_in_view != cols_in_view;
        if !changed { return false; }
        let row_changed = self.first_row != new_first_row || self.rows_in_view != rows_in_view;
        self.first_row = new_first_row;
        self.first_col = new_first_col;
        self.rows_in_view = rows_in_view;
        self.cols_in_view = cols_in_view;
        self.viewport_width = vp_w;
        if row_changed { self.update_subscriptions(); }
        true
    }

    fn handle_column_resize_start(&mut self, col_meta_idx: usize, _cursor_x: f32) -> bool {
        // Double-click detection: if same handle clicked within 400ms, auto-fit
        let now = Instant::now();
        let is_double = self.last_resize_click
            .map(|(idx, t)| idx == col_meta_idx && now.duration_since(t).as_millis() < 400)
            .unwrap_or(false);
        self.last_resize_click = Some((col_meta_idx, now));
        if is_double {
            self.auto_fit_all_columns();
            return true;
        }
        let show_name = self.show_row_name.t.unwrap_or(true);
        let name = if show_name && col_meta_idx == 0 {
            ROW_NAME_KEY.to_string()
        } else {
            let data_idx = if show_name { col_meta_idx - 1 } else { col_meta_idx };
            let (vis_start, _vis_end) = self.display_col_range();
            let abs_idx = vis_start + data_idx;
            if abs_idx < self.col_names.len() {
                self.col_names[abs_idx].clone()
            } else {
                return false;
            }
        };
        let current_w = self.effective_col_width(&name)
            .or_else(|| self.cached_col_widths.lock().get(&name).copied())
            .unwrap_or(DEFAULT_MAX_COL_WIDTH);
        self.resize_drag = Some(ResizeDrag {
            col_name: name,
            last_x: None,
            current_width: current_w,
        });
        true
    }

    fn handle_mouse_move_resize(&mut self, cursor_x: f32) -> Option<(super::CallableId, f64)> {
        let drag = self.resize_drag.as_mut()?;
        // First sample seeds last_x; no width change yet.
        let last = match drag.last_x {
            Some(v) => v,
            None => {
                drag.last_x = Some(cursor_x);
                return None;
            }
        };
        let delta = cursor_x - last;
        drag.last_x = Some(cursor_x);
        drag.current_width = (drag.current_width + delta).max(MIN_COL_WIDTH);
        let col_name = drag.col_name.clone();
        let new_width = drag.current_width;
        self.user_widths.lock().insert(col_name.clone(), new_width);
        self.on_resize_callbacks.get(&col_name)
            .map(|c| (c.id(), new_width as f64))
    }

    fn handle_column_resize_end(&mut self) -> bool {
        self.resize_drag.take().is_some()
    }

    fn is_column_resizing(&self) -> bool {
        self.resize_drag.is_some()
    }

    fn view(&self) -> IcedElement<'_> {
        if self.row_paths.is_empty() {
            let msg = if self.table_ref.last.is_some() {
                "No data in table".to_string()
            } else {
                "No table specified".to_string()
            };
            return widget::text(msg).into();
        }
        let num_rows = self.row_paths.len();
        let show_row_name = self.show_row_name.t.unwrap_or(true);
        let on_header_click = self.on_header_click.as_ref().map(|c| c.id());
        let bold = iced_core::Font {
            weight: iced_core::font::Weight::Bold,
            ..iced_core::Font::DEFAULT
        };
        let (vis_row_start, vis_row_end) = self.display_row_range();
        let (vis_col_start, vis_col_end) = self.display_col_range();
        // Snapshot cell data (need it for column width computation too)
        let grid_snapshot: Vec<Vec<String>> = {
            let grid = self.cells.grid.lock();
            (vis_row_start..vis_row_end)
                .map(|i| if i < grid.len() { grid[i].clone() } else { vec![] })
                .collect()
        };
        let name_col_offset = if show_row_name { 1 } else { 0 };
        // Column metadata with widths.
        // If effective_col_width returns Some, use it directly.
        // Otherwise auto-size from content (up to DEFAULT_MAX_COL_WIDTH)
        // and lock the result into user_widths.
        let mut col_meta: Vec<(String, f32)> = Vec::new();
        if show_row_name {
            let w = match self.effective_col_width(ROW_NAME_KEY) {
                Some(w) => w,
                None => {
                    let max_w = DEFAULT_MAX_COL_WIDTH;
                    let mut w = col_min_width(ROW_NAME_LABEL, max_w);
                    for row_idx in vis_row_start..vis_row_end {
                        if let Some(p) = self.row_paths.get(row_idx) {
                            let name = Path::basename(p).unwrap_or("");
                            w = w.max(col_min_width(name, max_w));
                        }
                    }
                    w
                }
            };
            col_meta.push((ROW_NAME_KEY.into(), w));
        }
        match self.mode {
            DisplayMode::Table => {
                for i in vis_col_start..vis_col_end {
                    let name = &self.col_names[i];
                    let w = match self.effective_col_width(name) {
                        Some(w) => w,
                        None => {
                            let max_w = DEFAULT_MAX_COL_WIDTH;
                            let display = self.column_types.get(name)
                                .and_then(|s| s.display_name.as_deref())
                                .unwrap_or(name);
                            // Header text (bold, 14pt) sets minimum
                            let mut w = col_header_width(display)
                                .max(MIN_COL_WIDTH).min(max_w);
                            // Cell content may be wider
                            for row in &grid_snapshot {
                                if i < row.len() {
                                    w = w.max(col_min_width(&row[i], max_w));
                                }
                            }
                            w
                        }
                    };
                    col_meta.push((name.clone(), w));
                }
            }
            DisplayMode::Value => {
                let w = match self.effective_col_width("value") {
                    Some(w) => w,
                    None => {
                        let max_w = DEFAULT_MAX_COL_WIDTH;
                        let mut w = col_min_width("value", max_w);
                        for row in &grid_snapshot {
                            if let Some(v) = row.first() {
                                w = w.max(col_min_width(v, max_w));
                            }
                        }
                        w
                    }
                };
                col_meta.push(("value".into(), w));
            }
        }
        // Cache column widths for keyboard nav viewport calculations
        {
            let mut cache = self.cached_col_widths.lock();
            cache.clear();
            for (name, w) in &col_meta {
                cache.insert(name.clone(), *w);
            }
        }
        // Header row — styled identically to data cells (same padding, borders)
        let mut header_row = Row::new().spacing(0);
        for (ci, (name, w)) in col_meta.iter().enumerate() {
            let is_data_col = ci >= name_col_offset;
            let header_text = if is_data_col {
                self.column_types.get(name)
                    .and_then(|s| s.display_name.clone())
                    .unwrap_or_else(|| name.clone())
            } else {
                // Synthesized row-name column: render the user-facing
                // label, not the internal sentinel key.
                ROW_NAME_LABEL.to_string()
            };
            let is_fixed = self.ref_widths.contains_key(name)
                && !self.on_resize_callbacks.contains_key(name);
            let text_el: IcedElement<'_> = if is_data_col {
                if let Some(cid) = on_header_click {
                    widget::Button::<'_, Message, GraphixTheme, Renderer>::new(
                        widget::text(header_text).size(14).font(bold)
                            .wrapping(iced_core::text::Wrapping::None),
                    ).on_press(Message::Call(cid, ValArray::from_iter([
                        Value::String(ArcStr::from(name.as_str())),
                    ]))).into()
                } else {
                    widget::text(header_text).size(14).font(bold)
                        .wrapping(iced_core::text::Wrapping::None).into()
                }
            } else {
                widget::text(header_text).size(14).font(bold)
                    .wrapping(iced_core::text::Wrapping::None).into()
            };
            // Use same styling as wrap_cell: same width, padding, border
            let inner: IcedElement<'_> = if !is_fixed {
                let handle: IcedElement<'_> =
                    widget::MouseArea::<'_, Message, GraphixTheme, Renderer>::new(
                        widget::container(widget::Space::new())
                            .width(RESIZE_HANDLE_WIDTH)
                            .height(iced_core::Length::Fill)
                    )
                    .interaction(iced_core::mouse::Interaction::ResizingColumn)
                    .on_press(Message::ColumnResizeStart(ci))
                    .into();
                Row::new()
                    .push(text_el)
                    .push(iced_widget::Space::new().width(iced_core::Length::Fill))
                    .push(handle)
                    .spacing(0)
                    .into()
            } else {
                text_el
            };
            let cell: IcedElement<'_> = widget::container(inner)
                .width(*w)
                .height(iced_core::Length::Shrink)
                .padding(iced_core::Padding::from([3, 5]))
                .style(|theme: &GraphixTheme| {
                    let p = theme.palette();
                    widget::container::Style {
                        border: iced_core::Border {
                            color: iced_core::Color { a: 0.15, ..p.text },
                            width: 0.5,
                            radius: 0.0.into(),
                        },
                        ..Default::default()
                    }
                }).into();
            header_row = header_row.push(cell);
        }
        // Data rows
        let _has_on_select = self.on_select.is_some();
        let row_h = self.row_height();
        let mut body = Col::new()
            .spacing(0)
            .width(iced_core::Length::Shrink)
            .height(iced_core::Length::Shrink);
        for (vi, row_idx) in (vis_row_start..vis_row_end).enumerate() {
            let _ = row_idx; // used below in cell rendering
            let mut row_w = Row::new().spacing(0);
            for (ci, (col_name, w)) in col_meta.iter().enumerate() {
                let is_name_col = ci == 0 && show_row_name;
                let cell_el: IcedElement<'_> = if is_name_col {
                    let name = self.row_paths.get(row_idx)
                        .and_then(|p| Path::basename(p))
                        .unwrap_or("")
                        .to_string();
                    let is_sel = self.row_paths.get(row_idx)
                        .map(|p| {
                            let cell_path = format!("{}", &**p);
                            self.selection.contains(&cell_path)
                        })
                        .unwrap_or(false);
                    let inner: IcedElement<'_> = widget::text(truncate_to_width(&name, *w))
                        .size(13)
                        .wrapping(iced_core::text::Wrapping::None)
                        .into();
                    self.wrap_cell(inner, col_name, row_idx, *w, row_h, is_sel)
                } else {
                    let data_col = vis_col_start + ci - name_col_offset;
                    let text = if vi < grid_snapshot.len() && data_col < grid_snapshot[vi].len() {
                        grid_snapshot[vi][data_col].clone()
                    } else {
                        String::new()
                    };
                    let col_type = if self.mode == DisplayMode::Table && data_col < self.col_names.len() {
                        self.col_type_for(&self.col_names[data_col])
                    } else {
                        &ColumnType::Text
                    };
                    self.render_cell(col_name, col_type, text, row_idx, *w, row_h, false)
                };
                row_w = row_w.push(cell_el);
            }
            let row_el: IcedElement<'_> = row_w.into();
            body = body.push(row_el);
        }
        let grid_area: IcedElement<'_> = Col::new()
            .push(header_row)
            .push(body)
            .width(iced_core::Length::Shrink)
            .height(iced_core::Length::Fill)
            .into();
        let need_vscroll = num_rows > self.rows_in_view;
        let total_data_cols = self.total_data_cols();
        // Check if all columns fit by summing actual widths
        let total_col_width: f32 = col_meta.iter().map(|(_, w)| *w).sum();
        let need_hscroll = total_data_cols > self.cols_in_view
            || total_col_width > self.viewport_width;
        if !need_vscroll && !need_hscroll {
            return self.wrap_keyboard(self.wrap_resize_drag(grid_area));
        }
        let virtual_height = if need_vscroll {
            (num_rows + self.rows_in_view) as f32 * ROW_HEIGHT_ESTIMATE
        } else { 1.0 };
        let virtual_width = if need_hscroll {
            (total_data_cols + self.cols_in_view) as f32 * MIN_COL_WIDTH
        } else { 1.0 };
        let virtual_content: IcedElement<'_> = widget::Space::new()
            .width(virtual_width).height(virtual_height).into();
        let direction = match (need_vscroll, need_hscroll) {
            (true, true) => widget::scrollable::Direction::Both {
                vertical: widget::scrollable::Scrollbar::default(),
                horizontal: widget::scrollable::Scrollbar::default(),
            },
            (true, false) => widget::scrollable::Direction::Vertical(
                widget::scrollable::Scrollbar::default(),
            ),
            (false, true) => widget::scrollable::Direction::Horizontal(
                widget::scrollable::Scrollbar::default(),
            ),
            (false, false) => unreachable!(),
        };
        let scroll_overlay: IcedElement<'_> =
            widget::Scrollable::<'_, Message, GraphixTheme, Renderer>::new(virtual_content)
                .direction(direction)
                .on_scroll(|vp| {
                    let abs = vp.absolute_offset();
                    let bounds = vp.bounds();
                    Message::Scroll(abs.x, abs.y, bounds.width, bounds.height)
                })
                .width(iced_core::Length::Fill)
                .height(iced_core::Length::Fill)
                .into();
        let result: IcedElement<'_> = widget::Stack::<'_, Message, GraphixTheme, Renderer>::new()
            .push(grid_area)
            .push(scroll_overlay)
            .width(iced_core::Length::Fill)
            .height(iced_core::Length::Fill)
            .into();
        self.wrap_keyboard(self.wrap_resize_drag(result))
    }
}


/// Keyboard area wrapper.
impl<X: GXExt> DataTableW<X> {
    /// Wrap the table's rendered view in a top-level MouseArea that
    /// emits `ColumnResizeMove` on every cursor move and
    /// `ColumnResizeEnd` on left-button release. The event loop's
    /// drain arm filters those against `is_column_resizing`, so there
    /// is no per-frame cost for regular hovering — just one extra
    /// message per mouse-move event while the cursor is over the
    /// table. Drag state changes (start/move/end) all go through the
    /// same message drain cycle, which is what fixes the timing bug
    /// where `CursorMoved` events arriving in `window_event` saw a
    /// stale `is_column_resizing()` result.
    fn wrap_resize_drag<'a>(&'a self, content: IcedElement<'a>) -> IcedElement<'a> {
        widget::MouseArea::<'_, Message, GraphixTheme, Renderer>::new(content)
            .on_move(|pt| Message::ColumnResizeMove(pt.x))
            .on_release(Message::ColumnResizeEnd)
            .into()
    }

    fn wrap_keyboard<'a>(&'a self, content: IcedElement<'a>) -> IcedElement<'a> {
        use crate::widgets::iced_keyboard_area::KeyboardArea;
        use iced_core::keyboard;

        KeyboardArea::new(content)
            .on_key_press(|event| {
                match event {
                    keyboard::Event::KeyPressed { key, .. } => {
                        use iced_core::keyboard::Key;
                        match key {
                            Key::Named(keyboard::key::Named::ArrowUp) =>
                                Message::TableKey(super::TableKeyAction::Up),
                            Key::Named(keyboard::key::Named::ArrowDown) =>
                                Message::TableKey(super::TableKeyAction::Down),
                            Key::Named(keyboard::key::Named::ArrowLeft) =>
                                Message::TableKey(super::TableKeyAction::Left),
                            Key::Named(keyboard::key::Named::ArrowRight) =>
                                Message::TableKey(super::TableKeyAction::Right),
                            Key::Named(keyboard::key::Named::Enter) =>
                                Message::TableKey(super::TableKeyAction::Enter),
                            Key::Named(keyboard::key::Named::Space) =>
                                Message::TableKey(super::TableKeyAction::Space),
                            Key::Named(keyboard::key::Named::Escape) =>
                                Message::TableKey(super::TableKeyAction::Escape),
                            _ => Message::Nop,
                        }
                    }
                    _ => Message::Nop,
                }
            })
            .into()
    }
}

/// Cell rendering by column type.
impl<X: GXExt> DataTableW<X> {
    /// Row height used by all data-row cells in this view pass.
    /// Tall-control columns (Combo, Spin, Toggle) force every row to
    /// the taller `ROW_HEIGHT_CONTROLS`; otherwise every row is
    /// `ROW_HEIGHT_ESTIMATE`. A single height for every cell in every
    /// row is what keeps the cell borders aligned — iced's Row lays
    /// children out at their individual natural heights when they are
    /// `Length::Shrink`, which produces ragged borders.
    fn row_height(&self) -> f32 {
        let tall = self.col_names.iter().any(|n| {
            match self.column_types.get(n).map(|s| &s.typ) {
                Some(ColumnType::Combo { .. })
                | Some(ColumnType::Spin { .. })
                | Some(ColumnType::Toggle) => true,
                _ => false,
            }
        });
        if tall { ROW_HEIGHT_CONTROLS } else { ROW_HEIGHT_ESTIMATE }
    }

    /// Wrap a cell's inner content with spreadsheet-style container:
    /// thin border, flat background, click-to-select.
    fn wrap_cell<'a>(
        &'a self,
        inner: IcedElement<'a>,
        col_name: &str,
        row_idx: usize,
        w: f32,
        row_h: f32,
        is_selected: bool,
    ) -> IcedElement<'a> {
        let col_for_msg = col_name.to_string();
        let styled: IcedElement<'a> = widget::container(inner)
            .width(w)
            .height(iced_core::Length::Fixed(row_h))
            .padding(iced_core::Padding::from([3, 5]))
            .style(move |theme: &GraphixTheme| {
                let p = theme.palette();
                widget::container::Style {
                    background: if is_selected {
                        Some(iced_core::Background::Color(
                            iced_core::Color { a: 0.25, ..p.primary },
                        ))
                    } else {
                        Option::None
                    },
                    border: iced_core::Border {
                        color: iced_core::Color { a: 0.15, ..p.text },
                        width: 0.5,
                        radius: 0.0.into(),
                    },
                    ..Default::default()
                }
            }).into();
        // Wrap in mouse_area for click-to-select.
        // Container constrains the mouse_area to not expand.
        widget::container(
            widget::MouseArea::<'_, Message, GraphixTheme, Renderer>::new(styled)
                .on_press(Message::CellClick(row_idx, col_for_msg)),
        )
        .width(iced_core::Length::Shrink)
        .height(iced_core::Length::Shrink)
        .into()
    }

    fn render_cell<'a>(
        &'a self,
        col_name: &str,
        col_type: &ColumnType,
        text: String,
        row_idx: usize,
        w: f32,
        row_h: f32,
        _can_select: bool,
    ) -> IcedElement<'a> {
        let cell_path = self.row_paths.get(row_idx)
            .map(|p| p.append(col_name));
        let callback_id = self.col_callbacks.get(col_name).map(|c| c.id());
        let is_selected = self.row_paths.get(row_idx)
            .map(|p| {
                let cell_path = format!("{}/{}", &**p, col_name);
                self.selection.contains(&cell_path)
            })
            .unwrap_or(false);
        let inner: IcedElement<'a> = match col_type {
            ColumnType::Text => {
                let is_editing = self.editing.as_ref()
                    .map(|(r, c)| *r == row_idx && c == col_name)
                    .unwrap_or(false);
                if is_editing {
                    widget::TextInput::<'_, Message, GraphixTheme, Renderer>::new(
                        "", &self.edit_buffer,
                    )
                    .on_input(Message::CellEditInput)
                    .on_submit(Message::CellEditSubmit)
                    .size(13)
                    .padding(2)
                    .into()
                } else if is_selected && callback_id.is_some() {
                    // Selected editable cell: click again to edit
                    let col_for_msg = col_name.to_string();
                    widget::Button::<'_, Message, GraphixTheme, Renderer>::new(
                        widget::text(truncate_to_width(&text, w)).size(13)
                            .wrapping(iced_core::text::Wrapping::None),
                    ).on_press(Message::CellEdit(row_idx, col_for_msg)).into()
                } else {
                    widget::text(truncate_to_width(&text, w)).size(13)
                        .wrapping(iced_core::text::Wrapping::None)
                        .into()
                }
            }
            ColumnType::Toggle => {
                let checked = &*text == "true" || &*text == "1";
                let mut tog = widget::Toggler::new(checked);
                if let (Some(cid), Some(cp)) = (callback_id, &cell_path) {
                    let path_val = Value::String(ArcStr::from(&**cp));
                    tog = tog.on_toggle(move |new_val| {
                        Message::Call(cid, ValArray::from_iter([
                            path_val.clone(),
                            Value::Bool(new_val),
                        ]))
                    });
                }
                tog.into()
            }
            ColumnType::Combo { choices } => {
                let options: Vec<String> = choices.iter().map(|(_, label)| label.clone()).collect();
                let selected = choices.iter()
                    .find(|(id, _)| *id == text)
                    .map(|(_, label)| label.clone());
                widget::PickList::<'_, String, Vec<String>, String, Message, GraphixTheme, Renderer>::new(
                    options, selected, {
                        let id_map_owned: FxHashMap<String, String> = choices.iter()
                            .map(|(id, label)| (label.clone(), id.clone())).collect();
                        let cell_path = cell_path.clone();
                        move |label: String| {
                            if let (Some(cid), Some(cp)) = (callback_id, &cell_path) {
                                let id = id_map_owned.get(&label).cloned().unwrap_or(label);
                                Message::Call(cid, ValArray::from_iter([
                                    Value::String(ArcStr::from(&**cp)),
                                    Value::String(ArcStr::from(id)),
                                ]))
                            } else {
                                Message::Nop
                            }
                        }
                    },
                ).text_size(13).into()
            }
            ColumnType::Spin { min, max, increment } => {
                let val: f64 = text.parse().unwrap_or(*min);
                if let (Some(cid), Some(cp)) = (callback_id, &cell_path) {
                    let path_val = Value::String(ArcStr::from(&**cp));
                    let dec_val = (val - increment).max(*min);
                    let inc_val = (val + increment).min(*max);
                    let path_dec = path_val.clone();
                    let path_inc = path_val;
                    let cid_dec = cid;
                    let cid_inc = cid;
                    let minus: IcedElement<'_> =
                        widget::Button::<'_, Message, GraphixTheme, Renderer>::new(
                            widget::text("−").size(11),
                        )
                        .padding(iced_core::Padding::from([0, 4]))
                        .on_press(Message::Call(
                            cid_dec,
                            ValArray::from_iter([path_dec, Value::F64(dec_val)]),
                        ))
                        .into();
                    let plus: IcedElement<'_> =
                        widget::Button::<'_, Message, GraphixTheme, Renderer>::new(
                            widget::text("+").size(11),
                        )
                        .padding(iced_core::Padding::from([0, 4]))
                        .on_press(Message::Call(
                            cid_inc,
                            ValArray::from_iter([path_inc, Value::F64(inc_val)]),
                        ))
                        .into();
                    let label: IcedElement<'_> = widget::text(text).size(13).into();
                    Row::new()
                        .push(minus)
                        .push(label)
                        .push(plus)
                        .spacing(2)
                        .align_y(iced_core::Alignment::Center)
                        .into()
                } else {
                    widget::text(text).size(13).into()
                }
            }
            ColumnType::Progress => {
                let val: f32 = text.parse().unwrap_or(0.0);
                widget::ProgressBar::<'_, GraphixTheme>::new(0.0..=1.0, val.clamp(0.0, 1.0))
                    .girth(16).into()
            }
            ColumnType::Button => {
                let val_str = text.clone();
                let mut btn = widget::Button::<'_, Message, GraphixTheme, Renderer>::new(
                    widget::text(text).size(13),
                );
                if let (Some(cid), Some(cp)) = (callback_id, &cell_path) {
                    let path_val = Value::String(ArcStr::from(&**cp));
                    let val = Value::String(ArcStr::from(val_str));
                    btn = btn.on_press(Message::Call(cid, ValArray::from_iter([path_val, val])));
                }
                btn.into()
            }
            ColumnType::Sparkline { .. } => {
                let sparklines = self.cells.sparklines.lock();
                let key = self.row_paths.get(row_idx)
                    .map(|p| (p.to_string(), col_name.to_string()));
                let history = key.as_ref().and_then(|k| sparklines.get(k));
                let points: Vec<(f64, f64)> = match history {
                    Some(h) if h.len() >= 2 => {
                        let first_t = h.front().unwrap().0;
                        h.iter().map(|(t, v)| {
                            (t.duration_since(first_t).as_secs_f64(), *v)
                        }).collect()
                    }
                    _ => vec![],
                };
                drop(sparklines);
                if points.is_empty() {
                    widget::text(text).size(13).into()
                } else {
                    let sparkline = SparklineCanvas { points };
                    widget::Canvas::new(sparkline)
                        .width(iced_core::Length::Fill)
                        .height(20.0)
                        .into()
                }
            }
        };
        self.wrap_cell(inner, col_name, row_idx, w, row_h, is_selected)
    }
}

#[cfg(test)]
impl<X: GXExt> DataTableW<X> {
    /// The width currently dictated by the column's `width` ref (the
    /// graphix-controlled width), if any. Independent of user drags or
    /// auto-sizing.
    pub fn dt_ref_width(&self, col: &str) -> Option<f32> {
        self.ref_widths.get(col).copied()
    }

    /// Number of points currently retained in the sparkline history for
    /// the cell at (row_basename, col). Returns None if the row or column
    /// is not a sparkline cell.
    pub fn dt_sparkline_len(&self, row: &str, col: &str) -> Option<usize> {
        let row_path = self.row_paths.iter()
            .find(|p| Path::basename(p).unwrap_or(&***p) == row)?;
        let key = (row_path.to_string(), col.to_string());
        self.cells.sparklines.lock().get(&key).map(|h| h.len())
    }

    /// Snapshot of the values in the sparkline history for the cell at
    /// (row_basename, col), in chronological order.
    pub fn dt_sparkline_values(&self, row: &str, col: &str) -> Option<Vec<f64>> {
        let row_path = self.row_paths.iter()
            .find(|p| Path::basename(p).unwrap_or(&***p) == row)?;
        let key = (row_path.to_string(), col.to_string());
        self.cells.sparklines.lock().get(&key)
            .map(|h| h.iter().map(|(_, v)| *v).collect())
    }

    /// Direct injection of a sparkline data point. Bypasses netidx
    /// publishing so decimation can be exercised deterministically with
    /// thousands of points. Triggers the same `decimate_sparkline` path
    /// invoked by the runtime when MAX_SPARKLINE_POINTS is exceeded.
    pub fn dt_push_sparkline(
        &self,
        row: &str,
        col: &str,
        when: Instant,
        v: f64,
    ) {
        let row_path = match self.row_paths.iter()
            .find(|p| Path::basename(p).unwrap_or(&***p) == row)
        {
            Some(p) => p.clone(),
            None => return,
        };
        let key = (row_path.to_string(), col.to_string());
        let mut sp = self.cells.sparklines.lock();
        let history = sp.entry(key).or_insert_with(VecDeque::new);
        history.push_back((when, v));
        if history.len() > MAX_SPARKLINE_POINTS {
            decimate_sparkline(history);
        }
    }

    /// CallableId of the per-column on_edit/on_click callback for `col`,
    /// if one was registered. Tests use this with `gx.call(...)` to
    /// dispatch edit/click events without going through pixel-layout.
    pub fn dt_col_callback_id(&self, col: &str) -> Option<super::CallableId> {
        self.col_callbacks.get(col).map(|c| c.id())
    }

    /// CallableId of the on_resize callback for `col`, if one was
    /// registered. Tests use this together with the resize handler
    /// methods to verify that column-resize drags fire the callback.
    pub fn dt_on_resize_callback_id(&self, col: &str) -> Option<super::CallableId> {
        self.on_resize_callbacks.get(col).map(|c| c.id())
    }

    /// Index of `col` in the col_meta vector built by `view()` — i.e.,
    /// the value `handle_column_resize_start` expects. Returns None if
    /// the column is not currently visible.
    pub fn dt_meta_col_idx(&self, col: &str) -> Option<usize> {
        let show_name = self.show_row_name.t.unwrap_or(true);
        // Test-facing convenience: treat the bare string "name" as the
        // synthesized row-name column so tests don't need to know the
        // internal `ROW_NAME_KEY` sentinel.
        if col == "name" || col == ROW_NAME_KEY {
            return if show_name { Some(0) } else { None };
        }
        let (vis_start, vis_end) = self.display_col_range();
        let pos = self.col_names.iter().position(|n| n == col)?;
        if pos < vis_start || pos >= vis_end {
            return None;
        }
        let offset = if show_name { 1 } else { 0 };
        Some(offset + (pos - vis_start))
    }

    /// Pixel bounds of the cell at (row_idx, col), computed from
    /// `cached_col_widths` populated by the most recent `view()`. Tests
    /// must call `view()` once before this to populate the cache. Returns
    /// None if the column is not visible or the cache is empty.
    pub fn dt_cell_bounds(
        &self,
        row_idx: usize,
        col: &str,
    ) -> Option<iced_core::Rectangle> {
        let cache = self.cached_col_widths.lock();
        if cache.is_empty() {
            return None;
        }
        let show_name = self.show_row_name.t.unwrap_or(true);
        let mut x = 0.0_f32;
        let w;
        let is_row_name_col = col == "name" || col == ROW_NAME_KEY;
        if is_row_name_col && show_name {
            w = cache.get(ROW_NAME_KEY).copied()?;
        } else {
            if show_name {
                x += cache.get(ROW_NAME_KEY).copied()?;
            }
            let (vis_start, vis_end) = self.display_col_range();
            let pos = self.col_names.iter().position(|n| n == col)?;
            if pos < vis_start || pos >= vis_end {
                return None;
            }
            for ci in vis_start..pos {
                x += cache.get(&self.col_names[ci]).copied()?;
            }
            w = cache.get(col).copied()?;
        }
        // Header cell is one ROW_HEIGHT_ESTIMATE plus container padding (3+3).
        let header_h = ROW_HEIGHT_ESTIMATE + 6.0;
        let y = header_h + row_idx as f32 * ROW_HEIGHT_ESTIMATE;
        Some(iced_core::Rectangle {
            x,
            y,
            width: w,
            height: ROW_HEIGHT_ESTIMATE,
        })
    }

    /// Pixel center of the cell at (row_idx, col). See `dt_cell_bounds`.
    pub fn dt_cell_center(
        &self,
        row_idx: usize,
        col: &str,
    ) -> Option<iced_core::Point> {
        self.dt_cell_bounds(row_idx, col).map(|r| r.center())
    }

    /// The width currently set in `user_widths` (from drag resize or
    /// auto-fit), if any. Independent of ref-controlled widths.
    pub fn dt_user_width(&self, col: &str) -> Option<f32> {
        self.user_widths.lock().get(col).copied()
    }
}
