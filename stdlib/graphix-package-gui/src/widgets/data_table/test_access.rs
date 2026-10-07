//! Test-only accessors on `DataTableW` for `GuiTestHarness::dt()`.

use super::{
    DataTableW, HEADER_HEIGHT, ROW_NAME_KEY,
    types::{ColumnType, push_sparkline_point},
};
use arcstr::ArcStr;
use graphix_rt::GXExt;
use netidx::path::Path;
use poolshark::local::LPooled;
use std::time::Instant;

impl<X: GXExt> DataTableW<X> {
    /// The width set by the column's `width` ref, if any.
    pub fn dt_ref_width(&self, col: &str) -> Option<f32> {
        self.columns.get(col).and_then(|c| c.ref_width)
    }

    /// The viewport metrics from the most recent layout.
    pub fn dt_viewport_metrics(&self) -> (f32, f32, usize, usize) {
        let m = self.viewport_metrics.lock();
        (m.viewport_width, m.viewport_height, m.rows_in_view, m.cols_in_view)
    }

    /// Points retained in the sparkline history at (row_basename, col);
    /// `None` when it is not a sparkline cell.
    pub fn dt_sparkline_len(&self, row: &str, col: &str) -> Option<usize> {
        let key = self.sparkline_key_for(row, col)?;
        self.cells.inner.lock().sparklines.get(&key).map(|h| h.len())
    }

    /// Snapshot of the values in the sparkline history for the cell at
    /// (row_basename, col), in chronological order.
    pub fn dt_sparkline_values(&self, row: &str, col: &str) -> Option<Vec<f64>> {
        let key = self.sparkline_key_for(row, col)?;
        self.cells
            .inner
            .lock()
            .sparklines
            .get(&key)
            .map(|h| h.iter().map(|(_, v)| *v).collect())
    }

    /// Record a sparkline point as a live update would, at `when`.
    pub fn dt_push_sparkline(&self, row: &str, col: &str, when: Instant, v: f64) {
        let Some(key) = self.sparkline_key_for(row, col) else { return };
        let Some(ColumnType::Sparkline { history_seconds, .. }) =
            self.columns.get(col).map(|c| &c.spec.typ)
        else {
            return;
        };
        let mut inner = self.cells.inner.lock();
        let history = inner.sparklines.entry(key).or_insert_with(LPooled::take);
        push_sparkline_point(history, when, v, *history_seconds);
    }

    fn sparkline_key_for(&self, row: &str, col: &str) -> Option<(Path, ArcStr)> {
        let row_path = self
            .row_paths
            .iter()
            .find(|p| Path::basename(*p).unwrap_or(&***p) == row)?
            .clone();
        let col_arc = self
            .columns
            .iter()
            .find(|(n, _)| n.as_str() == col)
            .map(|(n, _)| n.clone())
            .unwrap_or_else(|| ArcStr::from(col));
        Some((row_path, col_arc))
    }

    /// Index of `col` in the visible column metadata, as
    /// `handle_column_resize_start` expects; `None` when not visible.
    pub fn dt_meta_col_idx(&self, col: &str) -> Option<usize> {
        let show_name = self.show_row_name.t.unwrap_or(true);
        if col == ROW_NAME_KEY {
            return if show_name { Some(0) } else { None };
        }
        let (vis_start, vis_end) = self.display_col_range();
        let pos = self.columns.get_index_of(col)?;
        if pos < vis_start || pos >= vis_end {
            return None;
        }
        let offset = if show_name { 1 } else { 0 };
        Some(offset + (pos - vis_start))
    }

    /// Pixel bounds of the cell at (row_idx, col). Requires a prior
    /// `view()` to have populated the width cache.
    pub fn dt_cell_bounds(
        &self,
        row_idx: usize,
        col: &str,
    ) -> Option<iced_core::Rectangle> {
        let (vis_start, vis_end) = self.display_col_range();
        let cache = self.cached_col_widths.lock();
        if cache.is_empty() {
            return None;
        }
        let show_name = self.show_row_name.t.unwrap_or(true);
        let mut x = 0.0_f32;
        let w;
        if col == ROW_NAME_KEY && show_name {
            w = cache.get(&ROW_NAME_KEY).copied()?;
        } else {
            if show_name {
                x += cache.get(&ROW_NAME_KEY).copied()?;
            }
            let pos = self.columns.get_index_of(col)?;
            if pos < vis_start || pos >= vis_end {
                return None;
            }
            for ci in vis_start..pos {
                let (name, _) = self.columns.get_index(ci)?;
                x += cache.get(name).copied()?;
            }
            w = cache.get(col).copied()?;
        }
        let row_h = self.row_height();
        let y = HEADER_HEIGHT + row_idx as f32 * row_h;
        Some(iced_core::Rectangle { x, y, width: w, height: row_h })
    }

    /// The user width (drag or auto-fit), if any.
    pub fn dt_user_width(&self, col: &str) -> Option<f32> {
        self.user_widths.lock().get(col).copied()
    }

    /// Set a cached column width without a render pass. Returns the
    /// previous value.
    pub fn dt_set_cached_width(&self, col: &str, w: f32) -> Option<f32> {
        self.cached_col_widths.lock().insert(ArcStr::from(col), w)
    }

    /// Public wrapper over `col_at_offset` for tests.
    pub fn col_at_offset_for_test(&self, ox: f32) -> usize {
        self.col_at_offset(ox)
    }

    /// Sort indicator suffix (e.g. `" ▲"`, `" ▼₂"`) for `col`, or
    /// `None` if it is not in `sort_by`.
    pub fn dt_sort_indicator(&self, col: &str) -> Option<String> {
        self.build_sort_indicators().get(col).map(|s| s.to_string())
    }

    /// The first row and data column drawn.
    pub fn dt_first_cell(&self) -> (usize, usize) {
        (self.first_row, self.first_col)
    }

    /// The cell being edited, as (row path, column).
    pub fn dt_editing(&self) -> Option<(String, String)> {
        self.editing.as_ref().map(|(r, c)| (r.to_string(), c.to_string()))
    }

    /// How many netidx subscriptions the table holds.
    pub fn dt_subscription_count(&self) -> usize {
        self.cells.inner.lock().dvals.len()
    }

    pub fn dt_is_resizing(&self) -> bool {
        self.resize_drag.is_some()
    }

    /// How many sparkline histories the table keeps.
    pub fn dt_sparkline_count(&self) -> usize {
        self.cells.inner.lock().sparklines.len()
    }

    /// Restamp every sparkline point at `t`, as if recorded then.
    pub fn dt_age_out_after(&self, t: Instant) {
        for h in self.cells.inner.lock().sparklines.values_mut() {
            h.iter_mut().for_each(|p| p.0 = t);
        }
    }
}
