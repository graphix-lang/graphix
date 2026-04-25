//! User-input handling on `DataTableW`: keyboard nav, cell edit
//! lifecycle, clicks, scroll, and column resize drags. Dispatched to
//! by the `on_message` arm of the `GuiWidget` trait impl.

use super::types::{cell_path_matches, parse_or_quote, ResizeDrag, ViewportMetrics};
use super::{
    DataTableW, DisplayMode, DEFAULT_MAX_COL_WIDTH, MIN_COL_WIDTH, ROW_HEIGHT_ESTIMATE,
    ROW_NAME_KEY, ROW_NAME_KEY_ARC, ROW_NAME_LABEL,
};
use arcstr::{literal, ArcStr};
use compact_str::CompactString;
use graphix_rt::GXExt;
use netidx::{protocol::valarray::ValArray, publisher::Value};
use poolshark::local::LPooled;
use std::time::Instant;

impl<X: GXExt> DataTableW<X> {
    pub(super) fn fire_on_select(&self, row_idx: usize, col_name: &str) {
        if let Some(callable) = &self.on_select {
            if let Some(row_path) = self.row_paths.get(row_idx) {
                // Send the full cell path: row_path/col_name
                let cell_path: ArcStr = if col_name == ROW_NAME_KEY {
                    row_path.clone().into()
                } else {
                    compact_str::format_compact!(
                        "{}/{}",
                        row_path.as_ref() as &str,
                        col_name
                    )
                    .as_str()
                    .into()
                };
                let pv = Value::String(cell_path);
                let _ = self.gx.call(callable.id(), ValArray::from_iter([pv]));
            }
        }
    }

    pub(crate) fn handle_table_key(
        &mut self,
        action: &crate::widgets::TableKeyAction,
    ) -> bool {
        use crate::widgets::TableKeyAction;
        let n_rows = self.row_paths.len();
        if n_rows == 0 {
            return false;
        }
        let show_name = self.show_row_name.t.unwrap_or(true);
        let name_offset = if show_name { 1 } else { 0 };
        // Build the FULL column list (not just visible) for keyboard
        // nav. Pooled scratch — keyboard events are bursty (key repeat)
        // and we don't want a fresh Vec per arrow press.
        let mut display_cols: LPooled<Vec<ArcStr>> = LPooled::take();
        if show_name {
            display_cols.push(ROW_NAME_KEY_ARC.clone());
        }
        match self.mode {
            DisplayMode::Table => {
                display_cols.extend(self.col_names.iter().cloned());
            }
            DisplayMode::Value => {
                display_cols.push(literal!("value"));
            }
        }
        let n_display_cols = display_cols.len();
        if n_display_cols == 0 {
            return false;
        }
        // Find current position from the selection set.
        // Selection contains cell paths like "row_path/col_name" or "row_path" for name col.
        let (cur_row, cur_col) = self
            .selection
            .iter()
            .find_map(|sel_path| {
                for (ri, rp) in self.row_paths.iter().enumerate() {
                    // Check "row_path/col_name" format
                    for (ci, col_name) in display_cols.iter().enumerate() {
                        let matches = if col_name == &ROW_NAME_KEY_ARC {
                            sel_path.as_str() == rp.as_ref()
                        } else {
                            cell_path_matches(sel_path, rp.as_ref(), col_name)
                        };
                        if matches {
                            return Some((ri, ci));
                        }
                    }
                }
                None
            })
            .unwrap_or((0, name_offset));
        match action {
            TableKeyAction::Up
            | TableKeyAction::Down
            | TableKeyAction::Left
            | TableKeyAction::Right => {
                // Clamp c to data columns only (skip name column)
                let min_col = name_offset;
                let (mut r, mut c) = (cur_row, cur_col.max(min_col));
                match action {
                    TableKeyAction::Up => {
                        r = r.saturating_sub(1);
                    }
                    TableKeyAction::Down => {
                        r = (r + 1).min(n_rows - 1);
                    }
                    TableKeyAction::Left => {
                        if c > min_col {
                            c -= 1;
                        } else if r > 0 {
                            r -= 1;
                            c = n_display_cols - 1;
                        }
                    }
                    TableKeyAction::Right => {
                        if c + 1 < n_display_cols {
                            c += 1;
                        } else if r + 1 < n_rows {
                            r += 1;
                            c = min_col;
                        }
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

    pub(crate) fn handle_cell_edit(&mut self, row: usize, col: ArcStr) -> bool {
        // Initialize edit buffer with current cell value
        let col_in_table = self.col_names.contains(&col);
        let row_path = match self.row_paths.get(row) {
            Some(p) => p.clone(),
            None => {
                self.edit_buffer.clear();
                return true;
            }
        };
        if col_in_table {
            let key = (row_path.clone(), col.clone());
            let mut inner = self.cells.inner.lock();
            let id = inner.cells.get(&key).copied();
            self.edit_buffer = id
                .and_then(|id| inner.formatted_for(id))
                .map(|s| s.as_str().into())
                .unwrap_or_else(|| CompactString::new(""));
        } else {
            self.edit_buffer.clear();
        }
        self.editing = Some((row_path, col));
        true
    }

    pub(crate) fn handle_cell_edit_input(&mut self, text: CompactString) -> bool {
        self.edit_buffer = text;
        true
    }

    pub(crate) fn handle_cell_edit_submit(&mut self) -> bool {
        if let Some((ref row_path, ref col)) = self.editing {
            if let Some(callable) = self.col_callbacks.get(col) {
                let cell_path = row_path.append(col);
                let v = parse_or_quote(&self.edit_buffer);
                let _ = self.gx.call(
                    callable.id(),
                    ValArray::from_iter([Value::String(ArcStr::from(&*cell_path)), v]),
                );
            }
        }
        self.editing = None;
        self.edit_buffer.clear();
        true
    }

    pub(crate) fn handle_cell_edit_cancel(&mut self) -> bool {
        self.editing = None;
        self.edit_buffer.clear();
        true
    }

    pub(crate) fn handle_cell_click(&mut self, row: usize, col: ArcStr) -> bool {
        // Name column click fires on_activate. Accept the display
        // label `"name"` as a synonym for the row-name col so tests
        // and external callers don't need to know the internal
        // sentinel key.
        if col.as_str() == ROW_NAME_KEY || col.as_str() == ROW_NAME_LABEL {
            if let Some(callable) = &self.on_activate {
                if let Some(row_path) = self.row_paths.get(row) {
                    let pv = Value::String(row_path.clone().into());
                    let _ = self.gx.call(callable.id(), ValArray::from_iter([pv]));
                    return true;
                }
            }
        }
        // Any cell click fires on_select with the cell path
        self.fire_on_select(row, &col);
        true
    }

    pub(crate) fn handle_scroll(
        &mut self,
        ox: f32,
        oy: f32,
        vp_w: f32,
        vp_h: f32,
    ) -> bool {
        let row_h = self.row_height();
        let header_h = ROW_HEIGHT_ESTIMATE;
        let body_h = (vp_h - header_h).max(0.0);
        let rows_in_view = ((body_h / row_h).ceil() as usize).max(1);
        let name_cols = if self.show_row_name.t.unwrap_or(true) { 1 } else { 0 };
        let cols_in_view =
            ((vp_w / MIN_COL_WIDTH).ceil() as usize).saturating_sub(name_cols).max(1);
        // Viewport metrics update unconditionally so that a pure resize
        // notification (no offset change, or offset matching the
        // keyboard-override position) still refreshes the cached
        // viewport width / row count. Gating this behind the override
        // check would drop resize updates.
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
                dirty: false,
            };
        }
        if self.keyboard_scroll_override {
            // Check if this is a real user scroll (position actually changed
            // from what we'd expect) or just the overlay re-asserting.
            // Column threshold uses half the current column's width so
            // wide columns don't mis-detect the overlay as a real scroll.
            let expected_ox = self.offset_at_col(self.first_col);
            let expected_oy = self.first_row as f32 * row_h;
            let col_thresh = {
                let cache = self.cached_col_widths.lock();
                self.col_names
                    .get_index(self.first_col)
                    .and_then(|n| cache.get(n).copied())
                    .unwrap_or(MIN_COL_WIDTH)
                    * 0.5
            };
            let real_scroll = (ox - expected_ox).abs() > col_thresh
                || (oy - expected_oy).abs() > row_h * 0.5;
            if !real_scroll {
                // Overlay is re-asserting — keep keyboard-driven
                // first_row/first_col, but metric changes above still
                // take effect. Re-subscribe if the visible row window
                // grew/shrank so newly-visible rows get subs.
                if metrics_changed && prev.rows_in_view != rows_in_view {
                    self.update_subscriptions();
                }
                return metrics_changed;
            }
            // Real user scroll — clear override and process normally
            self.keyboard_scroll_override = false;
        }
        let n_rows = self.row_paths.len();
        let n_cols = self.total_data_cols();
        let new_first_row = ((oy / row_h).round() as usize).min(n_rows.saturating_sub(1));
        // At scroll-end, snap-to-midpoint inside `col_at_offset` can land
        // first_col BELOW the fit threshold — the rendered suffix then
        // overflows on the right and the last column is clipped. Detect
        // "ox ≈ ox_max" and force first_col to `min_first_col_for_fit`
        // so the suffix lands exactly inside the viewport. We can't do
        // this clamp unconditionally: at low ox, min_fit can be > 0
        // (because the table is wider than the viewport), and applying
        // it would hide the leftmost columns.
        let virtual_width = self.virtual_content_width();
        let max_ox = (virtual_width - vp_w).max(0.0);
        let snap_col = self.col_at_offset(ox).min(n_cols.saturating_sub(1));
        let new_first_col = if ox >= max_ox - 0.5 {
            snap_col.max(self.min_first_col_for_fit(vp_w)).min(n_cols.saturating_sub(1))
        } else {
            snap_col
        };
        let pos_changed =
            self.first_row != new_first_row || self.first_col != new_first_col;
        if !metrics_changed && !pos_changed {
            return false;
        }
        let row_changed =
            self.first_row != new_first_row || prev.rows_in_view != rows_in_view;
        self.first_row = new_first_row;
        self.first_col = new_first_col;
        if row_changed {
            self.update_subscriptions();
        }
        true
    }

    pub(crate) fn handle_column_resize_start(
        &mut self,
        col_meta_idx: usize,
        _cursor_x: f32,
    ) -> bool {
        // Double-click detection: if same handle clicked within 400ms, auto-fit
        let now = Instant::now();
        let is_double = self
            .last_resize_click
            .map(|(idx, t)| {
                idx == col_meta_idx && now.duration_since(t).as_millis() < 400
            })
            .unwrap_or(false);
        self.last_resize_click = Some((col_meta_idx, now));
        if is_double {
            self.auto_fit_all_columns();
            return true;
        }
        let show_name = self.show_row_name.t.unwrap_or(true);
        let name: ArcStr = if show_name && col_meta_idx == 0 {
            ROW_NAME_KEY_ARC.clone()
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
        let current_w = self
            .effective_col_width(&name)
            .or_else(|| self.cached_col_widths.lock().get(&name).copied())
            .unwrap_or(DEFAULT_MAX_COL_WIDTH);
        self.resize_drag =
            Some(ResizeDrag { col_name: name, last_x: None, current_width: current_w });
        true
    }

    pub(crate) fn handle_mouse_move_resize(
        &mut self,
        cursor_x: f32,
    ) -> Option<(graphix_rt::CallableId, f64)> {
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
        self.on_resize_callbacks.get(&col_name).map(|c| (c.id(), new_width as f64))
    }

    pub(crate) fn handle_column_resize_end(&mut self) -> bool {
        self.resize_drag.take().is_some()
    }
}
