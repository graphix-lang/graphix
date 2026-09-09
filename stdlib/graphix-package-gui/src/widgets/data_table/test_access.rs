//! Test-only accessors on `DataTableW` for `GuiTestHarness::dt()`.

use super::{
    DataTableW, DisplayMode, MAX_SPARKLINE_POINTS, ROW_HEIGHT_ESTIMATE, ROW_NAME_KEY,
    VALUE_COL_KEY,
    types::{decimate_sparkline, row_basename},
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

    /// Inject a sparkline point directly, bypassing netidx; decimates
    /// like the runtime path.
    pub fn dt_push_sparkline(&self, row: &str, col: &str, when: Instant, v: f64) {
        let key = match self.sparkline_key_for(row, col) {
            Some(k) => k,
            None => return,
        };
        let mut inner = self.cells.inner.lock();
        let history = inner.sparklines.entry(key).or_insert_with(LPooled::take);
        history.push_back((when, v));
        if history.len() > MAX_SPARKLINE_POINTS {
            decimate_sparkline(history);
        }
    }

    fn sparkline_key_for(&self, row: &str, col: &str) -> Option<(Path, ArcStr)> {
        let row_path = self
            .row_paths
            .iter()
            .find(|p| Path::basename(*p).unwrap_or(&***p) == row)?
            .clone();
        let col_arc = self
            .displayed_columns()
            .find(|(n, _)| n.as_str() == col)
            .map(|(n, _)| n.clone())
            .unwrap_or_else(|| ArcStr::from(col));
        Some((row_path, col_arc))
    }

    /// Index of `col` in the visible column metadata, as
    /// `handle_column_resize_start` expects; `None` when not visible.
    pub fn dt_meta_col_idx(&self, col: &str) -> Option<usize> {
        let show_name = self.show_row_name.t.unwrap_or(true);
        if col == "name" || col == ROW_NAME_KEY {
            return if show_name { Some(0) } else { None };
        }
        let (vis_start, vis_end) = self.display_col_range();
        let pos = self.displayed_index_of(col)?;
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
            let pos = self.displayed_index_of(col)?;
            if pos < vis_start || pos >= vis_end {
                return None;
            }
            for ci in vis_start..pos {
                let (name, _) = self.displayed_column_at(ci)?;
                x += cache.get(name).copied()?;
            }
            w = cache.get(col).copied()?;
        }
        // Header height includes the container padding.
        let header_h = ROW_HEIGHT_ESTIMATE + 6.0;
        let y = header_h + row_idx as f32 * ROW_HEIGHT_ESTIMATE;
        Some(iced_core::Rectangle { x, y, width: w, height: ROW_HEIGHT_ESTIMATE })
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

    #[allow(dead_code)]
    pub fn dt_snapshot_value_at(&self, row_idx: usize, col_idx: usize) -> Option<String> {
        let row_path = self.row_paths.get(row_idx)?;
        match self.mode {
            DisplayMode::Table => {
                let (col, _) = self.displayed_column_at(col_idx)?;
                let mut inner = self.cells.inner.lock();
                let key = (row_path.clone(), col.clone());
                let id = inner.cells.get(&key).copied();
                let v = id
                    .and_then(|id| inner.formatted_for(id))
                    .map(|s| s.to_string())
                    .unwrap_or_else(|| {
                        self.default_for(col, row_basename(row_path)).to_string()
                    });
                Some(v)
            }
            DisplayMode::Value if col_idx == 0 => {
                let mut inner = self.cells.inner.lock();
                let key = (row_path.clone(), VALUE_COL_KEY);
                let id = inner.cells.get(&key).copied();
                Some(
                    id.and_then(|id| inner.formatted_for(id))
                        .map(|s| s.to_string())
                        .unwrap_or_default(),
                )
            }
            DisplayMode::Value => None,
        }
    }
}
