//! Input handling for `DataTableW`: keyboard navigation, cell editing,
//! clicks, scroll, and column resize drags.

use super::{
    DataTableW, DisplayMode, HEADER_HEIGHT, MIN_COL_WIDTH, ROW_NAME_KEY, VALUE_COL_KEY,
    types::{ResizeDrag, ViewportMetrics, cell_path, is_cell_path, parse_or_quote},
};
use crate::widgets::TableKeyAction;
use arcstr::ArcStr;
use compact_str::CompactString;
use graphix_rt::{CallableId, GXExt};
use netidx::{protocol::valarray::ValArray, publisher::Value};
use poolshark::local::LPooled;
use std::time::Instant;

impl<X: GXExt> DataTableW<X> {
    pub(super) fn fire_on_select(&self, row_idx: usize, col_name: &ArcStr) {
        if let (Some(cid), Some(row)) = (self.on_select.id(), self.row_paths.get(row_idx))
        {
            let pv = Value::String(cell_path(row, col_name));
            let _ = self.gx.call(cid, ValArray::from_iter([pv]));
        }
    }

    /// The columns keyboard navigation moves over, in display order. A
    /// Value-mode cell is its row's path, as the row-name cell is, so
    /// there the value column alone is navigable.
    fn navigable_columns(&self) -> LPooled<Vec<ArcStr>> {
        let mut cols: LPooled<Vec<ArcStr>> = LPooled::take();
        match self.mode {
            DisplayMode::Table => {
                if self.show_row_name.t.unwrap_or(true) {
                    cols.push(ROW_NAME_KEY);
                }
                cols.extend(self.columns.keys().cloned());
            }
            DisplayMode::Value => cols.push(VALUE_COL_KEY),
        }
        cols
    }

    pub(crate) fn handle_table_key(&mut self, action: &TableKeyAction) -> bool {
        let n_rows = self.row_paths.len();
        let cols = self.navigable_columns();
        if n_rows == 0 || cols.is_empty() {
            return false;
        }
        let first_data_col = match cols.first() {
            Some(c) if c == &ROW_NAME_KEY => 1.min(cols.len() - 1),
            _ => 0,
        };
        let (cur_row, cur_col) = self
            .selection
            .iter()
            .find_map(|sel| {
                self.row_paths.iter().enumerate().find_map(|(ri, rp)| {
                    let ci = cols.iter().position(|c| is_cell_path(sel, rp, c))?;
                    Some((ri, ci))
                })
            })
            .unwrap_or((0, first_data_col));
        match action {
            TableKeyAction::Up
            | TableKeyAction::Down
            | TableKeyAction::Left
            | TableKeyAction::Right => {
                let (mut r, mut c) = (cur_row, cur_col.max(first_data_col));
                match action {
                    TableKeyAction::Up => r = r.saturating_sub(1),
                    TableKeyAction::Down => r = (r + 1).min(n_rows - 1),
                    TableKeyAction::Left if c > first_data_col => c -= 1,
                    TableKeyAction::Left if r > 0 => {
                        r -= 1;
                        c = cols.len() - 1;
                    }
                    TableKeyAction::Right if c + 1 < cols.len() => c += 1,
                    TableKeyAction::Right if r + 1 < n_rows => {
                        r += 1;
                        c = first_data_col;
                    }
                    _ => (),
                }
                self.fire_on_select(r, &cols[c]);
                self.scroll_to_cell(r, &cols[c]);
                true
            }
            TableKeyAction::Enter => {
                if let (Some(cid), Some(path)) =
                    (self.on_activate.id(), self.row_paths.get(cur_row))
                {
                    let pv = Value::String(ArcStr::from(&**path));
                    let _ = self.gx.call(cid, ValArray::from_iter([pv]));
                }
                true
            }
            TableKeyAction::Space => {
                let col = &cols[cur_col];
                if self.columns.get(col).is_some_and(|c| c.callback.is_some()) {
                    self.handle_cell_edit(cur_row, col.clone());
                }
                true
            }
            TableKeyAction::Escape => self.handle_cell_edit_cancel(),
        }
    }

    /// Open the editor on a cell, holding its current text, and focus it.
    pub(crate) fn handle_cell_edit(&mut self, row: usize, col: ArcStr) -> bool {
        let Some(row_path) = self.row_paths.get(row).cloned() else { return false };
        self.edit_buffer = match self.columns.contains_key(&col) {
            false => CompactString::new(""),
            true => {
                let mut inner = self.cells.inner.lock();
                let id = inner.cells.get(&(row_path.clone(), col.clone())).copied();
                match id.and_then(|id| inner.formatted_for(id)) {
                    Some(s) => s.as_str().into(),
                    None => {
                        drop(inner);
                        self.default_for(&col, super::types::row_basename(&row_path))
                            .as_str()
                            .into()
                    }
                }
            }
        };
        self.editing = Some((row_path, col));
        self.focus_editor = true;
        true
    }

    pub(crate) fn handle_cell_edit_input(&mut self, text: CompactString) -> bool {
        self.edit_buffer = text;
        true
    }

    pub(crate) fn handle_cell_edit_submit(&mut self) -> bool {
        if let Some((row_path, col)) = &self.editing
            && let Some(cid) = self.columns.get(col).and_then(|c| c.callback.as_ref())
        {
            let v = parse_or_quote(&self.edit_buffer);
            let path = Value::String(ArcStr::from(&*row_path.append(col)));
            let _ = self.gx.call(cid.id(), ValArray::from_iter([path, v]));
        }
        self.handle_cell_edit_cancel()
    }

    pub(crate) fn handle_cell_edit_cancel(&mut self) -> bool {
        let was_editing = self.editing.take().is_some();
        self.edit_buffer.clear();
        was_editing
    }

    /// A click on the row-name cell activates the row when there is an
    /// `on_activate`; any other click selects the cell.
    pub(crate) fn handle_cell_click(&mut self, row: usize, col: ArcStr) -> bool {
        if col == ROW_NAME_KEY
            && let (Some(cid), Some(row_path)) =
                (self.on_activate.id(), self.row_paths.get(row))
        {
            let pv = Value::String(row_path.clone().into());
            let _ = self.gx.call(cid, ValArray::from_iter([pv]));
            return true;
        }
        self.fire_on_select(row, &col);
        true
    }

    /// The overlay scrolled to `(ox, oy)` in a `vp_w` x `vp_h` viewport.
    pub(crate) fn handle_scroll(
        &mut self,
        ox: f32,
        oy: f32,
        vp_w: f32,
        vp_h: f32,
    ) -> bool {
        let row_h = self.row_height();
        let rows_in_view =
            (((vp_h - HEADER_HEIGHT).max(0.0) / row_h).ceil() as usize).max(1);
        let name_cols = if self.show_row_name.t.unwrap_or(true) { 1 } else { 0 };
        let cols_in_view =
            ((vp_w / MIN_COL_WIDTH).ceil() as usize).saturating_sub(name_cols).max(1);
        let prev = *self.viewport_metrics.lock();
        let metrics_changed = prev.viewport_width != vp_w
            || prev.viewport_height != vp_h
            || prev.rows_in_view != rows_in_view
            || prev.cols_in_view != cols_in_view;
        if metrics_changed {
            *self.viewport_metrics.lock() = ViewportMetrics {
                viewport_width: vp_w,
                viewport_height: vp_h,
                rows_in_view,
                cols_in_view,
                needs_subscription_reconcile: false,
            };
        }
        let n_rows = self.row_paths.len();
        let n_cols = self.total_data_cols();
        let max_oy = (n_rows as f32 * row_h + HEADER_HEIGHT - vp_h).max(0.0);
        let new_first_row = if oy >= max_oy - 0.5 {
            n_rows.saturating_sub(self.whole_rows_in_view())
        } else {
            ((oy / row_h).round() as usize).min(n_rows.saturating_sub(1))
        };
        // At the right end the snap could clip the last column; the
        // columns that fit whole are shown instead.
        let max_ox = (self.virtual_content_width() - vp_w).max(0.0);
        let snap_col = self.col_at_offset(ox).min(n_cols.saturating_sub(1));
        let new_first_col = if ox >= max_ox - 0.5 {
            snap_col.max(self.min_first_col_for_fit(vp_w)).min(n_cols.saturating_sub(1))
        } else {
            snap_col
        };
        let row_changed =
            self.first_row != new_first_row || prev.rows_in_view != rows_in_view;
        let pos_changed = row_changed || self.first_col != new_first_col;
        self.first_row = new_first_row;
        self.first_col = new_first_col;
        if row_changed {
            self.update_subscriptions();
        }
        metrics_changed || pos_changed
    }

    /// The header cell at `col_meta_idx` (the row-name column first when
    /// shown) was pressed on its resize handle; a second press within
    /// 400 ms auto-fits every column.
    pub(crate) fn handle_column_resize_start(&mut self, col_meta_idx: usize) -> bool {
        let now = Instant::now();
        let is_double = self.last_resize_click.is_some_and(|(idx, t)| {
            idx == col_meta_idx && now.duration_since(t).as_millis() < 400
        });
        self.last_resize_click = Some((col_meta_idx, now));
        if is_double {
            self.auto_fit_all_columns();
            return true;
        }
        let show_name = self.show_row_name.t.unwrap_or(true);
        let name: ArcStr = match (show_name, col_meta_idx) {
            (true, 0) => ROW_NAME_KEY,
            (_, i) => {
                let data_idx = i - show_name as usize;
                match self.mode {
                    DisplayMode::Value if data_idx == 0 => VALUE_COL_KEY,
                    DisplayMode::Value => return false,
                    DisplayMode::Table => {
                        let (vis_start, _) = self.display_col_range();
                        match self.columns.get_index(vis_start + data_idx) {
                            Some((name, _)) => name.clone(),
                            None => return false,
                        }
                    }
                }
            }
        };
        let current_width = self.column_canonical_width(&name);
        self.resize_drag =
            Some(ResizeDrag { col_name: name, last_x: None, current_width });
        true
    }

    /// Follow the drag to `cursor_x`. A column with a width ref takes its
    /// width from the program, so the drag only asks its `on_resize`;
    /// any other column takes the drag's width itself.
    pub(crate) fn handle_mouse_move_resize(
        &mut self,
        cursor_x: f32,
    ) -> Option<(CallableId, f64)> {
        let drag = self.resize_drag.as_mut()?;
        let last = drag.last_x.replace(cursor_x)?;
        drag.current_width = (drag.current_width + cursor_x - last).max(MIN_COL_WIDTH);
        let (name, w) = (drag.col_name.clone(), drag.current_width);
        let col = self.columns.get(&name);
        if col.is_none_or(|c| c.ref_width.is_none()) {
            self.user_widths.lock().insert(name, w);
        }
        col.and_then(|c| c.on_resize.as_ref()).map(|c| (c.id(), w as f64))
    }

    pub(crate) fn handle_column_resize_end(&mut self) -> bool {
        self.resize_drag.take().is_some()
    }
}
