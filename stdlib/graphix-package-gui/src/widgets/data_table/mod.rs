//! Data table widget: a live scrollable table over a `sys::net::Table`
//! with virtualized row and column viewports. Absolute row paths drive
//! netidx subscriptions; other rows are virtual and show per-column
//! defaults. Columns select an editor widget and supply defaults,
//! widths and resize callbacks; sort order, selection, header clicks,
//! activation and edits are driven by graphix refs and callables.

use super::{
    GuiW, GuiWidget, Handler, IcedElement, Message, Renderer, TableId, TableMsg, WidgetOp,
};
use ahash::{AHashMap, AHashSet, AHasher};
use anyhow::{Context, Result};
use arcstr::{ArcStr, literal};
use compact_str::CompactString;
use futures::channel::mpsc;
use graphix_compiler::expr::ExprId;
use graphix_rt::{GXExt, GXHandle, Ref, TRef};
use indexmap::IndexMap;
use log::warn;
use netidx::{
    path::Path,
    publisher::Value,
    subscriber::{Event, SubId, Subscriber},
};
use netidx_derive::FromValue;
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
pub(crate) use types::{decimate_sparkline, truncate_to_width};

/// `IndexMap` with the ahash hasher; iteration order is display order.
pub(super) type AIndexMap<K, V> = IndexMap<K, V, BuildHasherDefault<AHasher>>;

const ROW_HEIGHT_ESTIMATE: f32 = 22.0;
/// The header row's height: its text (14 px at iced's 1.3 line height)
/// and padding fit.
const HEADER_HEIGHT: f32 = 28.0;
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
/// Key of the synthesized row-name column. A user column name cannot
/// hold `\0`, so no real column collides with it.
pub(crate) const ROW_NAME_KEY: ArcStr = literal!("\0__rowname__");
const ROW_NAME_HEADER_LABEL: &str = "name";

/// Key of the one column of `DisplayMode::Value`, whose cell is the row
/// path's own value.
pub(crate) const VALUE_COL_KEY: ArcStr = literal!("\0value");
const VALUE_HEADER_LABEL: &str = "value";

/// Slack of the shared subscription dispatch channel.
const SUB_CHANNEL_SLACK: usize = 64;

#[derive(Clone, Copy, PartialEq)]
pub(super) enum DisplayMode {
    Table,
    Value,
}

pub(crate) struct DataTableW<X: GXExt> {
    /// What this table's messages are addressed to.
    id: TableId,
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
    on_activate: Handler<X>,
    on_select: Handler<X>,
    on_header_click: Handler<X>,
    on_update: Handler<X>,
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
    /// The table's rows in its own order.
    table_rows: Vec<Path>,
    /// The rows in display order: `table_rows` sorted by `sort_by`.
    row_paths: Vec<Path>,
    cells: Arc<SharedCells<X>>,
    first_row: usize,
    first_col: usize,
    /// Layout metrics written by the `responsive` wrapper in `view()`.
    viewport_metrics: Mutex<types::ViewportMetrics>,
    /// Column widths from the last `view()`.
    cached_col_widths: Mutex<AHashMap<ArcStr, f32>>,
    /// The scroll overlay, which `scroll_dirty` moves to the view the
    /// widget moved itself to.
    overlay_id: iced_core::widget::Id,
    scroll_dirty: bool,
    /// The cell editor, which `focus_editor` focuses when an edit starts.
    editor_id: iced_core::widget::Id,
    focus_editor: bool,
    /// The cell being edited, keyed by path so it survives scroll and
    /// sort changes.
    editing: Option<(Path, ArcStr)>,
    edit_buffer: CompactString,
    /// Sender side of the shared subscription update channel.
    update_tx: mpsc::Sender<GPooled<Vec<(SubId, Event)>>>,
}

impl<X: GXExt> DataTableW<X> {
    /// Subscribe what the sort reads, sort, then subscribe the window the
    /// sorted rows show.
    fn reconcile(&mut self) {
        self.update_subscriptions();
        self.sort_rows();
        self.update_subscriptions();
    }

    pub(crate) async fn compile(gx: GXHandle<X>, source: Value) -> Result<GuiW<X>> {
        // Fields alphabetical: on_activate, on_header_click, on_select,
        // on_update, selection, show_row_name, sort_by, table
        #[derive(FromValue)]
        struct Fields {
            on_activate: u64,
            on_header_click: u64,
            on_select: u64,
            on_update: u64,
            selection: u64,
            show_row_name: u64,
            sort_by: u64,
            table: u64,
        }
        let Fields {
            on_activate: on_activate_id,
            on_header_click: on_header_click_id,
            on_select: on_select_id,
            on_update: on_update_id,
            selection: selection_id,
            show_row_name: show_row_name_id,
            sort_by: sort_by_id,
            table: table_id,
        } = source.cast_to().context("data_table flds")?;
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
        let on_activate = Handler::compile(&gx, on_activate_ref).await?;
        let on_select = Handler::compile(&gx, on_select_ref).await?;
        let on_header_click = Handler::compile(&gx, on_header_click_ref).await?;
        let on_update = Handler::compile(&gx, on_update_ref).await?;
        let sort_by = sort_by_ref.last.as_ref().map(parse_sort_by).unwrap_or_default();
        let selection =
            selection_ref.last.as_ref().map(parse_selection).unwrap_or_default();
        let subscriber = gx
            .with_ctx(|ctx| {
                let ctx = &mut ctx.view();
                graphix_package_sys::netstate::NetState::get(ctx).subscriber(ctx)
            })
            .await??;
        let rt = tokio::runtime::Handle::current();
        let show_row_name =
            TRef::new(show_row_name_ref).context("data_table tref show_row_name")?;
        let cells = Arc::new(SharedCells::new(gx.clone()));
        cells.inner.lock().on_update = on_update.id();
        let (update_tx, update_rx) = mpsc::channel(SUB_CHANNEL_SLACK);
        spawn_dispatch_task(&rt, &cells, update_rx);
        let mut w = Self {
            id: TableId::new(),
            gx,
            subscriber,
            table_ref,
            show_row_name,
            sort_by_ref,
            selection_ref,
            sort_by,
            selection,
            on_activate,
            on_select,
            on_header_click,
            on_update,
            columns: AIndexMap::default(),
            user_widths: Mutex::new(AHashMap::default()),
            resize_drag: None,
            last_resize_click: None,
            mode: DisplayMode::Table,
            table_rows: vec![],
            row_paths: vec![],
            cells,
            first_row: 0,
            first_col: 0,
            viewport_metrics: Mutex::new(types::ViewportMetrics::default()),
            cached_col_widths: Mutex::new(AHashMap::default()),
            overlay_id: iced_core::widget::Id::unique(),
            scroll_dirty: false,
            editor_id: iced_core::widget::Id::unique(),
            focus_editor: false,
            editing: None,
            edit_buffer: CompactString::new(""),
            update_tx,
        };
        let pending = w.apply_table_sync();
        if !pending.is_empty() {
            w.compile_pending_columns(pending).await;
        }
        w.push_defaults_to_sparklines();
        w.reconcile();
        Ok(Box::new(w))
    }
}

impl<X: GXExt> GuiWidget<X> for DataTableW<X> {
    /// Apply work deferred from background tasks and layout (sort
    /// updates, viewport changes) before the next render.
    fn before_view(&mut self) -> bool {
        let mut changed = false;
        if self.cells.sort_col_dirty.swap(false, Ordering::Relaxed) {
            self.sort_rows();
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
        let mut reconcile = false;
        let mut source_fired = false;
        if id == self.table_ref.id {
            self.table_ref.last = Some(v.clone());
            needs_resolve = true;
        }
        if id == self.sort_by_ref.id {
            self.sort_by_ref.last = Some(v.clone());
            self.sort_by = parse_sort_by(v);
            reconcile = true;
        }
        if id == self.selection_ref.id {
            self.selection_ref.last = Some(v.clone());
            self.selection = parse_selection(v);
            self.ensure_selection_visible();
            changed = true;
        }
        changed |= self.show_row_name.update(id, v).context("show_row_name")?.is_some();
        self.on_activate.update(rt, &self.gx, id, v).context("on_activate")?;
        self.on_select.update(rt, &self.gx, id, v).context("on_select")?;
        self.on_header_click.update(rt, &self.gx, id, v).context("on_header_click")?;
        let retired = self.on_update.update(rt, &self.gx, id, v).context("on_update")?;
        if retired.is_some() || id == self.on_update.r.id {
            self.cells.inner.lock().on_update = self.on_update.id();
        }
        drop(retired);
        for c in self.columns.values_mut() {
            if let Some(e) = c.source.as_mut()
                && e.r.id == id
            {
                let was_netidx = e.parsed.is_netidx();
                e.r.last = Some(v.clone());
                e.refresh_from_last();
                reconcile |= was_netidx != e.parsed.is_netidx();
                source_fired = true;
                changed = true;
            }
            if let Some(r) = c.width_ref.as_mut()
                && r.id == id
            {
                r.last = Some(v.clone());
                c.ref_width = v.clone().cast_to::<f64>().ok().map(|w| w as f32);
                changed = true;
            }
            if let Some(r) = c.on_resize_ref.as_mut()
                && r.id == id
            {
                r.last = Some(v.clone());
                if let Err(e) =
                    super::update_callable_blocking(rt, &self.gx, &mut c.on_resize, v)
                {
                    warn!("data_table column {}: on_resize: {e:#}", c.spec.name);
                    c.on_resize = None;
                }
            }
        }
        if needs_resolve {
            let pending = self.apply_table_sync();
            if !pending.is_empty() {
                rt.block_on(self.compile_pending_columns(pending));
            }
            reconcile = true;
        }
        if reconcile || source_fired {
            self.push_defaults_to_sparklines();
        }
        if reconcile {
            self.reconcile();
            changed = true;
        }
        changed |= self.cells.dirty.swap(false, Ordering::Relaxed);
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
                    self.columns
                        .iter()
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
            col_names: self.columns.iter().map(|(s, _)| s.to_string()).collect(),
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
        use netidx::protocol::valarray::ValArray;
        let Message::Table(id, msg) = msg else { return false };
        if *id != self.id {
            return false;
        }
        match msg {
            TableMsg::CellClick(row, col) => self.handle_cell_click(*row, col.clone()),
            TableMsg::CellEdit(row, col) => self.handle_cell_edit(*row, col.clone()),
            TableMsg::CellEditInput(text) => self.handle_cell_edit_input(text.clone()),
            TableMsg::CellEditSubmit => self.handle_cell_edit_submit(),
            TableMsg::CellEditCancel => self.handle_cell_edit_cancel(),
            TableMsg::Key(action) => self.handle_table_key(action),
            TableMsg::Scroll(v, h, vp_w, vp_h) => {
                self.handle_scroll(*v, *h, *vp_w, *vp_h)
            }
            TableMsg::ColumnResizeStart(ci) => self.handle_column_resize_start(*ci),
            TableMsg::ColumnResizeMove(x) => {
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
            TableMsg::ColumnResizeEnd => self.handle_column_resize_end(),
        }
    }

    fn is_column_resizing(&self) -> bool {
        self.resize_drag.is_some()
    }

    fn take_ops(&mut self, out: &mut Vec<WidgetOp>) {
        use iced_core::widget::operation::scrollable::AbsoluteOffset;
        if std::mem::take(&mut self.scroll_dirty) {
            let x = self.offset_at_col(self.first_col);
            let y = self.first_row as f32 * self.row_height();
            out.push(WidgetOp::ScrollTo(
                self.overlay_id.clone(),
                AbsoluteOffset { x: Some(x), y: Some(y) },
            ));
        }
        if std::mem::take(&mut self.focus_editor) {
            out.push(WidgetOp::Focus(self.editor_id.clone()));
        }
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
