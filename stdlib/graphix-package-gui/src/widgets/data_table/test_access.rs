//! Test-only accessors on `DataTableW`. Exposes internal state so
//! `GuiTestHarness::dt()` helpers can inspect widths, sparkline
//! histories, and the grid snapshot without doing a full render pass.

use super::types::{decimate_sparkline, row_basename};
use super::{DataTableW, DisplayMode, MAX_SPARKLINE_POINTS, ROW_HEIGHT_ESTIMATE, ROW_NAME_KEY, VALUE_COL_KEY};
use arcstr::ArcStr;
use graphix_rt::GXExt;
use netidx::path::Path;
use poolshark::local::LPooled;
use std::time::Instant;

impl<X: GXExt> DataTableW<X> {
    /// The width currently dictated by the column's `width` ref (the
    /// graphix-controlled width), if any. Independent of user drags or
    /// auto-sizing.
    pub fn dt_ref_width(&self, col: &str) -> Option<f32> {
        self.columns.get(col).and_then(|c| c.ref_width)
    }

    /// Snapshot of the viewport metrics written by the responsive
    /// closure during the most recent layout. Tests assert these to
    /// verify that resize-only events propagate — the horizontal-
    /// scrollbar-on-shrink bug surfaced here before the `responsive`
    /// refactor because this state never updated when content fit
    /// initially.
    pub fn dt_viewport_metrics(&self) -> (f32, f32, usize, usize) {
        let m = self.viewport_metrics.lock();
        (m.viewport_width, m.viewport_height, m.rows_in_view, m.cols_in_view)
    }

    /// Number of points currently retained in the sparkline history for
    /// the cell at (row_basename, col). Returns None if the row or column
    /// is not a sparkline cell.
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

    /// Direct injection of a sparkline data point. Bypasses netidx
    /// publishing so decimation can be exercised deterministically with
    /// thousands of points. Triggers the same `decimate_sparkline` path
    /// invoked by the runtime when MAX_SPARKLINE_POINTS is exceeded.
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
        let pos = self.displayed_index_of(col)?;
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
        // Header cell is one ROW_HEIGHT_ESTIMATE plus container padding (3+3).
        let header_h = ROW_HEIGHT_ESTIMATE + 6.0;
        let y = header_h + row_idx as f32 * ROW_HEIGHT_ESTIMATE;
        Some(iced_core::Rectangle { x, y, width: w, height: ROW_HEIGHT_ESTIMATE })
    }

    /// The width currently set in `user_widths` (from drag resize or
    /// auto-fit), if any. Independent of ref-controlled widths.
    pub fn dt_user_width(&self, col: &str) -> Option<f32> {
        self.user_widths.lock().get(col).copied()
    }

    /// Number of distinct rows with at least one active netidx
    /// subscription. Tests use this to verify that `column_types`
    /// updates don't tear down live row subs unnecessarily.
    pub fn dt_row_sub_count(&self) -> usize {
        let inner = self.cells.inner.lock();
        let mut seen: std::collections::HashSet<&netidx::path::Path> =
            std::collections::HashSet::new();
        for (path, _col) in inner.cells.keys() {
            seen.insert(path);
        }
        seen.len()
    }

    /// Manually populate the cached column widths so scroll-math
    /// tests can predict `first_col` without waiting for a render
    /// pass to measure text. Returns the previous value if any.
    pub fn dt_set_cached_width(&self, col: &str, w: f32) -> Option<f32> {
        self.cached_col_widths.lock().insert(ArcStr::from(col), w)
    }

    /// Public wrapper over `col_at_offset` for tests.
    pub fn col_at_offset_for_test(&self, ox: f32) -> usize {
        self.col_at_offset(ox)
    }

    /// Public wrapper over `min_first_col_for_fit` for tests.
    pub fn min_first_col_for_fit_for_test(&self, vp_width: f32) -> usize {
        self.min_first_col_for_fit(vp_width)
    }

    /// Public wrapper over `virtual_content_width` for tests.
    pub fn virtual_content_width_for_test(&self) -> f32 {
        self.virtual_content_width()
    }

    /// Drive `handle_scroll` directly. Mirrors the `Message::Scroll`
    /// path the responsive scrollable would emit but skips the iced
    /// layout pass — tests can pin viewport size and offset and verify
    /// the resulting `first_col` / `first_row` choice. Returns the
    /// boolean from `handle_scroll`.
    pub fn handle_scroll_for_test(
        &mut self,
        ox: f32,
        oy: f32,
        vp_w: f32,
        vp_h: f32,
    ) -> bool {
        self.handle_scroll(ox, oy, vp_w, vp_h)
    }

    /// Public read of `first_col` for tests.
    pub fn first_col_for_test(&self) -> usize {
        self.first_col
    }

    /// Sort indicator suffix (e.g. `" ▲"`, `" ▼₂"`) for `col`, or
    /// `None` if the column isn't currently in `sort_by`. Tests use
    /// this to check that header arrows and subscript priorities
    /// track the `sort_by` ref without having to rummage through the
    /// iced tree for the actual rendered text.
    pub fn dt_sort_indicator(&self, col: &str) -> Option<String> {
        self.build_sort_indicators().get(col).map(|s| s.to_string())
    }

    /// Look up a single cell's display value, going through the same
    /// formatted-cache + default fallback the snapshot uses. Returns
    /// `None` if the indices are out of range.
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
