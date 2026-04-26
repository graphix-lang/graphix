//! Data table widget.
//!
//! Takes a `sys::net::Table` — rows, columns, and an optional base
//! path — and renders a live scrollable table with virtualized row
//! and column viewports. Rows whose path is absolute drive netidx
//! subscriptions; rows whose path is not absolute are virtual and
//! only display per-column default values. Columns are configured via
//! the `column_types` map, which selects per-column editor widgets
//! (text, toggle, combo, spin, progress, button, sparkline) and
//! supplies default values, widths, and resize callbacks. Sort order,
//! selection, header clicks, cell activation, and cell edits are all
//! driven by graphix refs and callables.
//!
//! Split across several sibling modules, each owning one slice of the
//! widget's state machine:
//! - `types`         — pure values, parsers, small helpers
//! - `subscriptions` — `SharedCells`, dispatch task, sub reconcile
//! - `layout`        — viewport geometry, widths, scroll math
//! - `events`        — keyboard/mouse/scroll handling
//! - `render`        — `view()` body and per-cell rendering
//! - `test_access`   — test-only inspection helpers

use super::{GuiW, GuiWidget, IcedElement, Message, Renderer};
use anyhow::{Context, Result};
use arcstr::{literal, ArcStr};
use compact_str::CompactString;
use futures::channel::mpsc;
use fxhash::{FxBuildHasher, FxHashMap};
use graphix_compiler::expr::ExprId;
use graphix_rt::{Callable, GXExt, GXHandle, Ref, TRef};
use indexmap::IndexMap;
use netidx::{
    path::Path,
    publisher::Value,
    subscriber::{Event, SubId, Subscriber},
};
use parking_lot::Mutex;
use poolshark::global::GPooled;
use std::{
    sync::{atomic::Ordering, Arc},
    time::Instant,
};

mod events;
mod layout;
mod render;
mod subscriptions;
mod types;

#[cfg(test)]
mod test_access;

use subscriptions::{spawn_dispatch_task, SharedCells};
use types::{
    parse_column_specs, parse_selection, parse_sort_by, ColumnState, ResizeDrag, SortBy,
};

#[cfg(test)]
pub(crate) use types::decimate_sparkline;

/// `IndexMap` with the project-standard fx hasher. Holds every known
/// column, both displayed (raw netidx columns and virtual columns
/// with a non-null default) and spec-only (configured but not yet
/// displayed because their `default_value` ref currently resolves to
/// null and the column has no source in the table data). Netidx
/// tables can reach millions of columns, so the `fxhash` hasher
/// matters and the indexed iteration order doubles as the display
/// order — the first `displayed_count` entries are the displayed
/// columns in display order.
pub(super) type FxIndexMap<K, V> = IndexMap<K, V, FxBuildHasher>;

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
///
/// `ROW_NAME_KEY_ARC` is the static `ArcStr` form used as a map key
/// or `Message` payload — clones are refcount bumps with no alloc.
/// `ROW_NAME_KEY` is the same bytes as a `&'static str` for `==`
/// checks against incoming column-name strings.
const ROW_NAME_KEY: &str = "\0__rowname__";
static ROW_NAME_KEY_ARC: ArcStr = literal!("\0__rowname__");
/// Header label displayed for the synthesized row-name column.
const ROW_NAME_LABEL: &str = "name";

/// Sentinel column key used for `DisplayMode::Value` rows where the cell
/// has no real column name. Leading `\0` keeps it from colliding with
/// any user-supplied column (apply_table strips `\0` from incoming
/// column names).
const VALUE_COL_KEY: ArcStr = literal!("\0value");

/// Channel slack for the shared subscription dispatch queue. Every
/// cell subscription and every sort-column subscription pushes into
/// this one channel, so the slack needs to absorb bursts from large
/// tables without stalling the publisher side.
const SUB_CHANNEL_SLACK: usize = 64;

#[derive(Clone, Copy, PartialEq)]
pub(super) enum DisplayMode {
    Table,
    Value,
}

pub(crate) struct DataTableW<X: GXExt> {
    gx: GXHandle<X>,
    subscriber: Subscriber,
    table_ref: Ref<X>,
    show_row_name: TRef<X, bool>,
    sort_by_ref: Ref<X>,
    column_types_ref: Ref<X>,
    selection_ref: Ref<X>,
    sort_by: Vec<SortBy>,
    /// Set of selected cell paths (`row_path` for the row-name column,
    /// `row_path/col_name` otherwise), controlled by graphix.
    selection: fxhash::FxHashSet<ArcStr>,
    on_activate_ref: Ref<X>,
    on_activate: Option<Callable<X>>,
    on_select_ref: Ref<X>,
    on_select: Option<Callable<X>>,
    on_header_click_ref: Ref<X>,
    on_header_click: Option<Callable<X>>,
    on_update_ref: Ref<X>,
    on_update: Option<Callable<X>>,
    /// All known columns. Indexed iteration order is the display
    /// order: the first `displayed_count` entries are the displayed
    /// columns, in display order; remaining entries are spec-only
    /// (configured via `column_types` but not currently rendered).
    /// Spec-only entries stay in the map so their `default_value`,
    /// `width`, and `on_resize` refs remain alive — their updates can
    /// promote the column into the displayed prefix without losing
    /// callable identity.
    columns: FxIndexMap<ArcStr, ColumnState<X>>,
    /// Number of leading entries in `columns` that are currently
    /// displayed. Maintained as an invariant by `apply_table`,
    /// `apply_column_types_diff`, and `reconcile_virtual_cols`.
    displayed_count: usize,
    /// User-controlled widths (free resize or auto-sized on first
    /// load). Held in a `Mutex` so render-path code (which only has
    /// `&self`) can read and write through it. Lifetime is decoupled
    /// from the column entry — a column re-added after deletion
    /// reuses the user's stored drag preference.
    user_widths: Mutex<FxHashMap<ArcStr, f32>>,
    /// Active resize drag state
    resize_drag: Option<ResizeDrag>,
    /// Last resize handle click: (col_meta_idx, timestamp) for double-click detection
    last_resize_click: Option<(usize, Instant)>,
    mode: DisplayMode,
    row_paths: Vec<Path>,
    cells: Arc<SharedCells<X>>,
    first_row: usize,
    first_col: usize,
    /// Viewport-derived layout metrics. Written during layout by the
    /// `responsive` wrapper around `view()`; read everywhere else.
    /// Interior-mutable because iced requires the widget to be `Sync`.
    viewport_metrics: Mutex<types::ViewportMetrics>,
    /// Cached actual column widths from the last view() call, keyed by col_name.
    cached_col_widths: Mutex<FxHashMap<ArcStr, f32>>,
    /// Set when keyboard nav changes first_row/first_col. Suppresses
    /// the next handle_scroll from overwriting the keyboard-driven position.
    keyboard_scroll_override: bool,
    /// Which cell is being edited: (row_path, col_name). Keyed by path
    /// rather than index so the edit stays attached to the right row
    /// under scroll or sort changes.
    editing: Option<(Path, ArcStr)>,
    /// Text buffer for the cell being edited
    edit_buffer: CompactString,
    /// Sender side of the shared subscription update channel. Cloned
    /// into every `Dval::updates` call so one background task
    /// processes every cell and sort-column update.
    update_tx: mpsc::Sender<GPooled<Vec<(SubId, Event)>>>,
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

/// Tag for which per-column ref the `handle_update` ref-id sweep
/// matched. `default_value` carries the pre/post `is_present()` flags
/// because the null↔non-null transition is what governs whether
/// `reconcile_virtual_cols` needs to run.
enum ColumnRefKind {
    DefaultValue { was_present: bool, now_present: bool },
    Width,
    OnResize,
}

impl<X: GXExt> DataTableW<X> {
    /// Iterator over the displayed prefix of `columns`, in display
    /// order. Use this anywhere the previous code iterated `col_names`
    /// — the position in the resulting iterator IS the column's
    /// display index.
    pub(in crate::widgets::data_table) fn displayed_columns(
        &self,
    ) -> impl ExactSizeIterator<Item = (&ArcStr, &ColumnState<X>)> {
        self.columns.as_slice()[..self.displayed_count].iter().map(|(k, v)| (k, v))
    }

    /// `(name, state)` of the displayed column at `idx`, or `None` if
    /// `idx` is out of the displayed range. The non-displayed
    /// (spec-only) tail of `columns` is intentionally invisible
    /// through this accessor.
    pub(in crate::widgets::data_table) fn displayed_column_at(
        &self,
        idx: usize,
    ) -> Option<(&ArcStr, &ColumnState<X>)> {
        if idx >= self.displayed_count {
            return None;
        }
        self.columns.get_index(idx)
    }

    /// Display-order index of `name`, or `None` if `name` isn't a
    /// displayed column. Spec-only entries (sitting at positions
    /// ≥ `displayed_count`) return `None` to keep callers from using
    /// their position as a display index.
    pub(in crate::widgets::data_table) fn displayed_index_of(
        &self,
        name: &str,
    ) -> Option<usize> {
        self.columns.get_index_of(name).filter(|&i| i < self.displayed_count)
    }

    pub(crate) async fn compile(gx: GXHandle<X>, source: Value) -> Result<GuiW<X>> {
        // Fields alphabetical: column_types, on_activate, on_header_click,
        // on_select, on_update, selection, show_row_name, sort_by, table
        let [(_, column_types_id), (_, on_activate_id), (_, on_header_click_id), (_, on_select_id), (_, on_update_id), (_, selection_id), (_, show_row_name_id), (_, sort_by_id), (_, table_id)] =
            source.cast_to::<[(ArcStr, u64); 9]>().context("data_table flds")?;
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
        let sort_by = sort_by_ref.last.as_ref().map(parse_sort_by).unwrap_or_default();
        let column_types_parsed = match column_types_ref.last.as_ref() {
            Some(Value::Null) | None => fxhash::FxHashMap::default(),
            Some(v) => parse_column_specs(v),
        };
        let selection =
            selection_ref.last.as_ref().map(parse_selection).unwrap_or_default();
        let subscriber = gx.subscriber();
        let rt = tokio::runtime::Handle::current();
        let show_row_name =
            TRef::new(show_row_name_ref).context("data_table tref show_row_name")?;
        let initial_on_update = on_update.as_ref().map(|c| c.id());
        let cells = Arc::new(SharedCells::new(gx.clone()));
        cells.inner.lock().on_update = initial_on_update;
        let (update_tx, update_rx) = mpsc::channel(SUB_CHANNEL_SLACK);
        spawn_dispatch_task(&rt, &cells, update_rx);
        // Start with empty column_types so apply_column_types_update
        // handles initial compilation via the same diff path the
        // reactive update uses.
        let mut w = Self {
            gx,
            subscriber,
            table_ref,
            show_row_name,
            sort_by_ref,
            column_types_ref,
            selection_ref,
            sort_by,
            selection,
            on_activate_ref,
            on_activate,
            on_select_ref,
            on_select,
            on_header_click_ref,
            on_header_click,
            on_update_ref,
            on_update,
            columns: FxIndexMap::default(),
            displayed_count: 0,
            user_widths: Mutex::new(FxHashMap::default()),
            resize_drag: None,
            last_resize_click: None,
            mode: DisplayMode::Table,
            row_paths: vec![],
            cells,
            first_row: 0,
            first_col: 0,
            viewport_metrics: Mutex::new(types::ViewportMetrics::default()),
            cached_col_widths: Mutex::new(FxHashMap::default()),
            keyboard_scroll_override: false,
            editing: None,
            edit_buffer: CompactString::new(""),
            update_tx,
        };
        let (_, recompile) = w.apply_column_types_diff(column_types_parsed);
        w.apply_column_recompile(recompile).await?;
        w.apply_table();
        w.update_subscriptions();
        Ok(Box::new(w))
    }
}

impl<X: GXExt> GuiWidget<X> for DataTableW<X> {
    /// Consume any deferred work that arrived from background tasks
    /// (sort-column subscription updates) before the next render. This
    /// is the path that propagates "the sort column has new data" into
    /// an actual reorder when no graphix ref update happens to fire
    /// `handle_update`.
    fn before_view(&mut self) -> bool {
        let mut changed = false;
        if self.cells.sort_col_dirty.swap(false, Ordering::Relaxed) {
            self.resort_by_column();
            // Re-sort permuted `row_paths`, so the visible window now
            // contains different rows. Re-evaluate subs so the new
            // visible rows are subscribed and the ones that scrolled
            // out are dropped.
            self.update_subscriptions();
            changed = true;
        }
        // The `responsive` wrapper around `view()` updates
        // `viewport_metrics` during layout and sets `dirty` when the
        // visible row count changed. It can't call
        // `update_subscriptions()` itself (only has `&self`), so we
        // pick up the pending work here, one cycle later.
        let viewport_dirty = {
            let mut m = self.viewport_metrics.lock();
            let d = m.dirty;
            m.dirty = false;
            d
        };
        if viewport_dirty {
            self.update_subscriptions();
            changed = true;
        }
        if self.cells.dirty.swap(false, Ordering::Relaxed) {
            changed = true;
        }
        changed
    }

    fn handle_update(
        &mut self,
        rt: &tokio::runtime::Handle,
        id: ExprId,
        v: &Value,
    ) -> Result<bool> {
        let mut changed = false;
        let mut needs_resolve = false;
        if id == self.table_ref.id {
            self.table_ref.last = Some(v.clone());
            needs_resolve = true;
            changed = true;
        }
        if id == self.sort_by_ref.id {
            self.sort_by_ref.last = Some(v.clone());
            self.sort_by = parse_sort_by(v);
            needs_resolve = true;
            changed = true;
        }
        if id == self.column_types_ref.id {
            self.column_types_ref.last = Some(v.clone());
            let new_types = match v {
                Value::Null => fxhash::FxHashMap::default(),
                v => parse_column_specs(v),
            };
            let (structural, recompile) = self.apply_column_types_diff(new_types);
            // Only enter the runtime if something actually needs
            // compiling. `display_name`-only updates (and anything
            // else that leaves every callable and bid alone) stay on
            // the sync path so tests driven from a current-thread
            // tokio task don't trip the nested-runtime panic.
            if !recompile.is_empty() {
                rt.block_on(self.apply_column_recompile(recompile))?;
            }
            if structural {
                needs_resolve = true;
            }
            changed = true;
        }
        if id == self.selection_ref.id {
            self.selection_ref.last = Some(v.clone());
            self.selection = parse_selection(v);
            self.ensure_selection_visible();
            changed = true;
        }
        if needs_resolve {
            self.apply_table();
            self.update_subscriptions();
            changed = true;
        }
        changed |= self.show_row_name.update(id, v).context("show_row_name")?.is_some();
        macro_rules! update_cb {
            ($ref:ident, $field:ident) => {
                if id == self.$ref.id {
                    self.$ref.last = Some(v.clone());
                    self.$field =
                        rt.block_on(compile_callable_opt(&self.gx, &self.$ref))?;
                }
            };
        }
        update_cb!(on_activate_ref, on_activate);
        update_cb!(on_select_ref, on_select);
        update_cb!(on_header_click_ref, on_header_click);
        if id == self.on_update_ref.id {
            self.on_update_ref.last = Some(v.clone());
            let new_cb =
                rt.block_on(compile_callable_opt(&self.gx, &self.on_update_ref))?;
            // Publish the new id to the dispatch task before we drop
            // the old Callable — if we swapped first, the task could
            // briefly see an already-deleted id between drop and the
            // shared-state write.
            self.cells.inner.lock().on_update = new_cb.as_ref().map(|c| c.id());
            self.on_update = new_cb;
        }
        if self.cells.dirty.swap(false, Ordering::Relaxed) {
            changed = true;
        }
        // Check per-column ref updates (default_value, width, on_resize).
        // One pass over `columns` finds which column (if any) owns the
        // updated ref id and which kind of ref it is — vs. one scan
        // per ref kind in the previous map-per-kind layout.
        let column_ref_hit = self.columns.iter_mut().find_map(|(name, c)| {
            if let Some(e) = c.default_value.as_mut() {
                if e.r.id == id {
                    let was_present = e.parsed.is_present();
                    e.r.last = Some(v.clone());
                    e.refresh_from_last();
                    let now_present = e.parsed.is_present();
                    return Some((
                        name.clone(),
                        ColumnRefKind::DefaultValue { was_present, now_present },
                    ));
                }
            }
            if let Some(r) = c.width_ref.as_mut() {
                if r.id == id {
                    r.last = Some(v.clone());
                    c.ref_width = v.clone().cast_to::<f64>().ok().map(|w| w as f32);
                    return Some((name.clone(), ColumnRefKind::Width));
                }
            }
            if let Some(r) = c.on_resize_ref.as_mut() {
                if r.id == id {
                    r.last = Some(v.clone());
                    return Some((name.clone(), ColumnRefKind::OnResize));
                }
            }
            None
        });
        if let Some((col_name, kind)) = column_ref_hit {
            match kind {
                ColumnRefKind::DefaultValue { was_present, now_present } => {
                    if was_present != now_present {
                        self.reconcile_virtual_cols();
                    }
                    // Default values are read at render time via
                    // `default_for`, which consults the column's
                    // `default_value` entry. The ref's `.last` was
                    // refreshed above, so the next view() pass sees
                    // the new value automatically.
                    self.push_defaults_to_sparklines();
                }
                ColumnRefKind::Width => {}
                ColumnRefKind::OnResize => {
                    let new_cb = match v {
                        Value::Null => None,
                        v => rt.block_on(self.gx.compile_callable(v.clone())).ok(),
                    };
                    if let Some(c) = self.columns.get_mut(&col_name) {
                        c.on_resize = new_cb;
                    }
                }
            }
            changed = true;
        }
        // Check if sort column data arrived and needs re-sort
        if self.cells.sort_col_dirty.swap(false, Ordering::Relaxed) {
            self.resort_by_column();
            changed = true;
        }
        self.update_subscriptions();
        Ok(changed)
    }

    #[cfg(test)]
    fn data_table_snapshot(&self) -> Option<super::DataTableSnapshot> {
        let mut sel: Vec<String> = self.selection.iter().map(|s| s.to_string()).collect();
        sel.sort();
        // Materialize the cell index into the row-major Vec<Vec<String>>
        // the snapshot uses. Cells with a live subscription read from
        // `inner.formatted_for` (lazy ArcStr cache, populated on first
        // read after each value change); cells without one fall back
        // to the column's `default_for`. Snapshot stays String-keyed
        // so existing test assertions keep working.
        let mut inner = self.cells.inner.lock();
        let grid: Vec<Vec<String>> = match self.mode {
            DisplayMode::Table => self
                .row_paths
                .iter()
                .map(|row_path| {
                    self.displayed_columns()
                        .map(|(cn, _)| {
                            let key = (row_path.clone(), cn.clone());
                            let id = inner.cells.get(&key).copied();
                            id.and_then(|id| inner.formatted_for(id))
                                .map(|s| s.to_string())
                                .unwrap_or_else(|| {
                                    self.default_for(cn, types::row_basename(row_path))
                                        .to_string()
                                })
                        })
                        .collect()
                })
                .collect(),
            DisplayMode::Value => self
                .row_paths
                .iter()
                .map(|row_path| {
                    let key = (row_path.clone(), VALUE_COL_KEY);
                    let id = inner.cells.get(&key).copied();
                    let v = id
                        .and_then(|id| inner.formatted_for(id))
                        .map(|s| s.to_string())
                        .unwrap_or_default();
                    vec![v]
                })
                .collect(),
        };
        drop(inner);
        Some(super::DataTableSnapshot {
            col_names: self.displayed_columns().map(|(s, _)| s.to_string()).collect(),
            row_basenames: self
                .row_paths
                .iter()
                .map(|p| Path::basename(p).unwrap_or(&**p).to_string())
                .collect(),
            grid,
            is_value_mode: self.mode == DisplayMode::Value,
            selection: sel,
        })
    }

    #[cfg(test)]
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }

    #[cfg(test)]
    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }

    fn on_message(
        &mut self,
        msg: &super::Message,
        shell: &mut super::MessageShell,
    ) -> bool {
        use super::Message;
        use netidx::protocol::valarray::ValArray;
        match msg {
            Message::CellClick(row, col) => self.handle_cell_click(*row, col.clone()),
            Message::CellEdit(row, col) => self.handle_cell_edit(*row, col.clone()),
            Message::CellEditInput(text) => self.handle_cell_edit_input(text.clone()),
            Message::CellEditSubmit => self.handle_cell_edit_submit(),
            Message::CellEditCancel => self.handle_cell_edit_cancel(),
            Message::TableKey(action) => self.handle_table_key(action),
            Message::Scroll(v, h, vp_w, vp_h) => self.handle_scroll(*v, *h, *vp_w, *vp_h),
            Message::ColumnResizeStart(ci) => {
                self.handle_column_resize_start(*ci, shell.cursor_position.x)
            }
            Message::ColumnResizeMove(x) => {
                // Only a widget actively dragging consumes ColumnResizeMove;
                // otherwise the cursor is just passing over the table.
                if !self.is_column_resizing() {
                    return false;
                }
                if let Some((cid, w)) = self.handle_mouse_move_resize(*x) {
                    shell.publish(Message::Call(
                        cid,
                        ValArray::from_iter([Value::F64(w)]),
                    ));
                }
                true
            }
            Message::ColumnResizeEnd => self.handle_column_resize_end(),
            // Variants `data_table` does not handle — enumerated
            // exhaustively so a new `Message` variant forces a
            // deliberate decision here rather than silently dropping.
            Message::Nop | Message::Call(..) | Message::EditorAction(..) => false,
        }
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
            return iced_widget::text(msg).into();
        }
        // Wrap in `responsive` so the widget receives its actual
        // allocated size at layout time. This is what lets the
        // horizontal scrollbar appear dynamically when the user shrinks
        // the window below content width: we derive `need_hscroll` and
        // the rendered row/column counts from `size` directly, and
        // refresh the cached viewport metrics as a side effect so
        // off-layout consumers (keyboard nav, subscription updates)
        // also see the latest values.
        iced_widget::responsive(|size| self.render_with_size(size))
            .width(iced_core::Length::Fill)
            .height(iced_core::Length::Fill)
            .into()
    }
}
