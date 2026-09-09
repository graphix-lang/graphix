//! Data table widget: a live scrollable table over a `sys::net::Table`
//! with virtualized row and column viewports. Absolute row paths drive
//! netidx subscriptions; other rows are virtual and show per-column
//! defaults. Columns select an editor widget and supply defaults,
//! widths and resize callbacks; sort order, selection, header clicks,
//! activation and edits are driven by graphix refs and callables.

use super::{GuiW, GuiWidget, IcedElement, Message, Renderer};
use ahash::{AHashMap, AHashSet, AHasher};
use anyhow::{Context, Result};
use arcstr::{ArcStr, literal};
use compact_str::CompactString;
use futures::channel::mpsc;
use graphix_compiler::expr::ExprId;
use graphix_rt::{Callable, GXExt, GXHandle, Ref, TRef};
use indexmap::IndexMap;
use netidx::{
    path::Path,
    publisher::Value,
    subscriber::{Event, SubId, Subscriber},
};
use parking_lot::Mutex;
use poolshark::{global::GPooled, local::LPooled};
use std::{
    hash::BuildHasherDefault,
    sync::{Arc, atomic::Ordering},
    time::Instant,
};

mod events;
mod layout;
mod render;
mod subscriptions;
mod types;

#[cfg(test)]
mod test_access;

use subscriptions::{SharedCells, spawn_dispatch_task};
use types::{ColumnState, ResizeDrag, SortBy, parse_selection, parse_sort_by};

#[cfg(test)]
pub(crate) use types::decimate_sparkline;

/// `IndexMap` with the ahash hasher; iteration order is display order.
pub(super) type AIndexMap<K, V> = IndexMap<K, V, BuildHasherDefault<AHasher>>;

const ROW_HEIGHT_ESTIMATE: f32 = 22.0;
const ROW_HEIGHT_CONTROLS: f32 = 30.0;
const ROW_BUFFER: usize = 50;
const MIN_COL_WIDTH: f32 = 80.0;
const DEFAULT_MAX_COL_WIDTH: f32 = 300.0;
const DEFAULT_VISIBLE_ROWS: usize = 30;
const DEFAULT_VISIBLE_COLS: usize = 20;
/// Max points kept per sparkline; `decimate_sparkline` halves the
/// count when exceeded.
const MAX_SPARKLINE_POINTS: usize = 512;

/// Horizontal padding inside each cell, left + right.
const CELL_H_PADDING: f32 = 10.0;
/// Width of the resize handle inside header cells.
const RESIZE_HANDLE_WIDTH: f32 = 5.0;
/// Internal key for the synthesized row-name column. The leading null
/// byte keeps it from colliding with any real column name; `"name"` is
/// only the displayed label.
const ROW_NAME_SENTINEL_KEY: &str = "\0__rowname__";
static ROW_NAME_SENTINEL_KEY_ARC: ArcStr = literal!("\0__rowname__");
/// Header label displayed for the synthesized row-name column.
const ROW_NAME_HEADER_LABEL: &str = "name";

/// Column key for `DisplayMode::Value` cells, which have no real column
/// name. The leading `\0` cannot appear in a user column name.
const VALUE_COL_KEY: ArcStr = literal!("\0value");

/// Slack of the shared subscription dispatch channel.
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
    selection_ref: Ref<X>,
    sort_by: LPooled<Vec<SortBy>>,
    /// Set of selected cell paths (`row_path` for the row-name column,
    /// `row_path/col_name` otherwise), controlled by graphix.
    selection: LPooled<AHashSet<ArcStr>>,
    on_activate_ref: Ref<X>,
    on_activate: Option<Callable<X>>,
    on_select_ref: Ref<X>,
    on_select: Option<Callable<X>>,
    on_header_click_ref: Ref<X>,
    on_header_click: Option<Callable<X>>,
    on_update_ref: Ref<X>,
    on_update: Option<Callable<X>>,
    /// All columns in display order.
    columns: AIndexMap<ArcStr, ColumnState<X>>,
    /// User-controlled widths, kept across column removal so a
    /// re-added column keeps its width. Locked because the render
    /// path has only `&self`.
    user_widths: Mutex<AHashMap<ArcStr, f32>>,
    resize_drag: Option<ResizeDrag>,
    /// Last resize-handle click, for double-click detection.
    last_resize_click: Option<(usize, Instant)>,
    mode: DisplayMode,
    row_paths: Vec<Path>,
    cells: Arc<SharedCells<X>>,
    first_row: usize,
    first_col: usize,
    /// Layout metrics written by the `responsive` wrapper in `view()`.
    viewport_metrics: Mutex<types::ViewportMetrics>,
    /// Column widths from the last `view()`.
    cached_col_widths: Mutex<AHashMap<ArcStr, f32>>,
    /// Set by keyboard navigation so the next `handle_scroll` does not
    /// overwrite its position.
    ignore_overlay_reassert: bool,
    /// The cell being edited, keyed by path so it survives scroll and
    /// sort changes.
    editing: Option<(Path, ArcStr)>,
    edit_buffer: CompactString,
    /// Sender side of the shared subscription update channel.
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

/// Which per-column ref an update matched. A `Source` flip between
/// netidx and stored mode changes the subscription topology.
enum ColumnRefKind {
    Source { was_netidx: bool, now_netidx: bool },
    Width,
    OnResize,
}

impl<X: GXExt> DataTableW<X> {
    /// All columns in display order.
    pub(in crate::widgets::data_table) fn displayed_columns(
        &self,
    ) -> impl ExactSizeIterator<Item = (&ArcStr, &ColumnState<X>)> {
        self.columns.iter()
    }

    /// The column at display index `idx`.
    pub(in crate::widgets::data_table) fn displayed_column_at(
        &self,
        idx: usize,
    ) -> Option<(&ArcStr, &ColumnState<X>)> {
        self.columns.get_index(idx)
    }

    /// Display-order index of `name`.
    pub(in crate::widgets::data_table) fn displayed_index_of(
        &self,
        name: &str,
    ) -> Option<usize> {
        self.columns.get_index_of(name)
    }

    pub(in crate::widgets::data_table) fn displayed_count(&self) -> usize {
        self.columns.len()
    }

    pub(crate) async fn compile(gx: GXHandle<X>, source: Value) -> Result<GuiW<X>> {
        // Fields alphabetical: on_activate, on_header_click, on_select,
        // on_update, selection, show_row_name, sort_by, table
        let [
            (_, on_activate_id),
            (_, on_header_click_id),
            (_, on_select_id),
            (_, on_update_id),
            (_, selection_id),
            (_, show_row_name_id),
            (_, sort_by_id),
            (_, table_id),
        ] = source.cast_to::<[(ArcStr, u64); 8]>().context("data_table flds")?;
        let (
            on_activate_ref,
            on_header_click_ref,
            on_select_ref,
            on_update_ref,
            selection_ref,
            show_row_name_ref,
            sort_by_ref,
            table_ref,
        ) = tokio::try_join!(
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
        let selection =
            selection_ref.last.as_ref().map(parse_selection).unwrap_or_default();
        let subscriber = gx
            .with_ctx(|ctx| {
                graphix_package_sys::netstate::NetState::get(ctx).subscriber(ctx)
            })
            .await??;
        let rt = tokio::runtime::Handle::current();
        let show_row_name =
            TRef::new(show_row_name_ref).context("data_table tref show_row_name")?;
        let initial_on_update = on_update.as_ref().map(|c| c.id());
        let cells = Arc::new(SharedCells::new(gx.clone()));
        cells.inner.lock().on_update = initial_on_update;
        let (update_tx, update_rx) = mpsc::channel(SUB_CHANNEL_SLACK);
        spawn_dispatch_task(&rt, &cells, update_rx);
        let mut w = Self {
            gx,
            subscriber,
            table_ref,
            show_row_name,
            sort_by_ref,
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
            columns: AIndexMap::default(),
            user_widths: Mutex::new(AHashMap::default()),
            resize_drag: None,
            last_resize_click: None,
            mode: DisplayMode::Table,
            row_paths: vec![],
            cells,
            first_row: 0,
            first_col: 0,
            viewport_metrics: Mutex::new(types::ViewportMetrics::default()),
            cached_col_widths: Mutex::new(AHashMap::default()),
            ignore_overlay_reassert: false,
            editing: None,
            edit_buffer: CompactString::new(""),
            update_tx,
        };
        let pending = w.apply_table_sync();
        if !pending.is_empty() {
            w.compile_pending_columns(pending).await?;
        }
        w.push_defaults_to_sparklines();
        if !w.sort_by.is_empty() {
            w.resort_by_column();
        }
        w.update_subscriptions();
        Ok(Box::new(w))
    }
}

impl<X: GXExt> GuiWidget<X> for DataTableW<X> {
    /// Apply work deferred from background tasks and layout (sort
    /// updates, viewport changes) before the next render.
    fn before_view(&mut self) -> bool {
        let mut changed = false;
        if self.cells.sort_col_dirty.swap(false, Ordering::Relaxed) {
            self.resort_by_column();
            self.update_subscriptions();
            changed = true;
        }
        // Layout has only `&self`, so viewport changes are picked up here.
        let viewport_dirty = {
            let mut m = self.viewport_metrics.lock();
            let d = m.needs_subscription_reconcile;
            m.needs_subscription_reconcile = false;
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
            let old_cols: AHashSet<ArcStr> =
                self.sort_by.iter().map(|s| s.column.clone()).collect();
            self.sort_by = parse_sort_by(v);
            self.apply_sort_by_change(&old_cols);
            if !self.sort_by.is_empty() {
                self.resort_by_column();
            }
            changed = true;
        }
        if id == self.selection_ref.id {
            self.selection_ref.last = Some(v.clone());
            self.selection = parse_selection(v);
            self.ensure_selection_visible();
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
            // The dispatch task must see the new id before the old
            // Callable is dropped.
            self.cells.inner.lock().on_update = new_cb.as_ref().map(|c| c.id());
            self.on_update = new_cb;
        }
        if self.cells.dirty.swap(false, Ordering::Relaxed) {
            changed = true;
        }
        let column_ref_hit = self.columns.iter_mut().find_map(|(name, c)| {
            if let Some(e) = c.source.as_mut() {
                if e.r.id == id {
                    let was_netidx = e.parsed.is_netidx();
                    e.r.last = Some(v.clone());
                    e.refresh_from_last();
                    let now_netidx = e.parsed.is_netidx();
                    return Some((
                        name.clone(),
                        ColumnRefKind::Source { was_netidx, now_netidx },
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
                ColumnRefKind::Source { was_netidx, now_netidx } => {
                    if was_netidx != now_netidx {
                        needs_resolve = true;
                    }
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
        if self.cells.sort_col_dirty.swap(false, Ordering::Relaxed) {
            self.resort_by_column();
            changed = true;
        }
        if needs_resolve {
            let pending = self.apply_table_sync();
            if !pending.is_empty() {
                rt.block_on(self.compile_pending_columns(pending))?;
            }
            self.push_defaults_to_sparklines();
            if !self.sort_by.is_empty() {
                self.resort_by_column();
            }
        }
        self.update_subscriptions();
        Ok(changed)
    }

    #[cfg(test)]
    fn data_table_snapshot(&self) -> Option<super::DataTableSnapshot> {
        let mut sel: Vec<String> = self.selection.iter().map(|s| s.to_string()).collect();
        sel.sort();
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
        // `responsive` gives the widget its allocated size at layout
        // time; the viewport metrics are refreshed as a side effect.
        iced_widget::responsive(|size| self.render_with_size(size))
            .width(iced_core::Length::Fill)
            .height(iced_core::Length::Fill)
            .into()
    }
}
