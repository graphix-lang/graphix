//! Layout geometry for `DataTableW`: visible ranges, column widths,
//! scroll math, and row height.

use super::{
    DataTableW, DisplayMode, MIN_COL_WIDTH, ROW_BUFFER, ROW_HEIGHT_CONTROLS,
    ROW_HEIGHT_ESTIMATE, ROW_NAME_HEADER_LABEL, ROW_NAME_SENTINEL_KEY,
    ROW_NAME_SENTINEL_KEY_ARC, VALUE_COL_KEY,
    types::{ColumnType, col_header_width, col_text_width, row_basename, value_to_f64},
};
use arcstr::ArcStr;
use graphix_rt::GXExt;
use netidx::{path::Path, publisher::Value};
use poolshark::local::LPooled;

impl<X: GXExt> DataTableW<X> {
    pub(super) fn display_row_range(&self) -> (usize, usize) {
        let n = self.row_paths.len();
        if n == 0 {
            return (0, 0);
        }
        let start = self.first_row.min(n.saturating_sub(1));
        let rows_in_view = self.viewport_metrics.lock().rows_in_view;
        let end = (start + rows_in_view).min(n);
        (start, end)
    }

    pub(super) fn subscription_row_range(&self) -> (usize, usize) {
        let n = self.row_paths.len();
        let (ds, de) = self.display_row_range();
        (ds.saturating_sub(ROW_BUFFER), (de + ROW_BUFFER).min(n))
    }

    pub(super) fn display_col_range(&self) -> (usize, usize) {
        let total = self.total_data_cols();
        if total == 0 {
            return (0, 0);
        }
        let start = self.first_col.min(total.saturating_sub(1));
        let cols_in_view = self.viewport_metrics.lock().cols_in_view;
        let end = (start + cols_in_view).min(total);
        (start, end)
    }

    pub(super) fn total_data_cols(&self) -> usize {
        match self.mode {
            DisplayMode::Table => self.displayed_count(),
            DisplayMode::Value => 1,
        }
    }

    /// Pixel width of the synthesized row-name column (0 when hidden).
    pub(super) fn name_col_width(&self) -> f32 {
        if !self.show_row_name.t.unwrap_or(true) {
            return 0.0;
        }
        self.cached_col_widths
            .lock()
            .get(ROW_NAME_SENTINEL_KEY)
            .copied()
            .unwrap_or(MIN_COL_WIDTH)
    }

    /// Data-column index whose left boundary is nearest the virtual
    /// scroll offset, accounting for the name column.
    pub(super) fn col_at_offset(&self, ox: f32) -> usize {
        let name_col_w = self.name_col_width();
        let effective_ox = (ox - name_col_w).max(0.0);
        let mut acc = 0.0;
        for (i, (name, _)) in self.displayed_columns().enumerate() {
            let w = self.column_canonical_width(name);
            let next = acc + w;
            if effective_ox < (acc + next) / 2.0 {
                return i;
            }
            acc = next;
        }
        self.displayed_count().saturating_sub(1)
    }

    /// Inverse of `col_at_offset`: the virtual scroll offset of
    /// `first_col = ci`.
    pub(super) fn offset_at_col(&self, ci: usize) -> f32 {
        let name_col_w = self.name_col_width();
        name_col_w
            + self
                .displayed_columns()
                .take(ci)
                .map(|(n, _)| self.column_canonical_width(n))
                .sum::<f32>()
    }

    /// String shown for a cell with no live subscription value.
    pub(super) fn default_for(&self, col_name: &str, row_name: &str) -> ArcStr {
        let Some(c) = self.columns.get(col_name) else { return ArcStr::new() };
        let Some(entry) = c.source.as_ref() else { return ArcStr::new() };
        entry
            .parsed
            .lookup(row_name)
            .map(super::types::value_to_display)
            .unwrap_or_default()
    }

    /// `default_for` as an `f64`, converted from the raw value.
    pub(super) fn default_value_f64_for(
        &self,
        col_name: &str,
        row_name: &str,
    ) -> Option<f64> {
        self.columns
            .get(col_name)
            .and_then(|c| c.source.as_ref())
            .and_then(|e| e.parsed.lookup(row_name))
            .and_then(value_to_f64)
    }

    /// Current raw `Value` for a cell: the live subscription value,
    /// else the column's default for this row.
    pub(super) fn raw_value_for(
        &self,
        row_path: &Path,
        col_name: &ArcStr,
    ) -> Option<Value> {
        let inner = self.cells.inner.lock();
        let key = (row_path.clone(), col_name.clone());
        if let Some(id) = inner.cells.get(&key).copied() {
            if let Some(v) = inner.values.get(&id) {
                return Some(v.clone());
            }
        }
        drop(inner);
        self.columns
            .get(col_name.as_str())
            .and_then(|c| c.source.as_ref())
            .and_then(|e| e.parsed.lookup(super::types::row_basename(row_path)))
            .cloned()
    }

    /// The column width if set by user drag or ref; `None` means
    /// auto-size from content.
    pub(super) fn explicit_col_width(&self, col_name: &str) -> Option<f32> {
        if let Some(w) = self.user_widths.lock().get(col_name) {
            return Some(*w);
        }
        self.columns.get(col_name).and_then(|c| c.ref_width)
    }

    /// Canonical width for `col_name`: `explicit_col_width`, else the
    /// last rendered width, else `MIN_COL_WIDTH`. All scroll math must
    /// agree on this number.
    pub(super) fn column_canonical_width(&self, col_name: &str) -> f32 {
        if let Some(w) = self.explicit_col_width(col_name) {
            return w;
        }
        self.cached_col_widths.lock().get(col_name).copied().unwrap_or(MIN_COL_WIDTH)
    }

    /// Smallest `first_col` such that the columns from it to the end
    /// (plus the name column) fit in `vp_width`.
    pub(super) fn min_first_col_for_fit(&self, vp_width: f32) -> usize {
        let total = self.displayed_count();
        if total == 0 {
            return 0;
        }
        let avail = (vp_width - self.name_col_width()).max(0.0);
        let mut acc = 0.0;
        for k in (0..total).rev() {
            let (name, _) = self.displayed_column_at(k).unwrap();
            let w = self.column_canonical_width(name);
            if acc + w > avail {
                return k + 1;
            }
            acc += w;
        }
        0
    }

    /// Total virtual content width: the name column plus every data
    /// column at its canonical width.
    pub(super) fn virtual_content_width(&self) -> f32 {
        let name_col_w = self.name_col_width();
        let data_cols_w: f32 = match self.mode {
            DisplayMode::Table => self
                .displayed_columns()
                .map(|(n, _)| self.column_canonical_width(n))
                .sum(),
            DisplayMode::Value => self.column_canonical_width("value"),
        };
        name_col_w + data_cols_w
    }

    /// How many data columns fit from `first_col` at their cached widths.
    pub(super) fn actual_visible_cols(&self, from_col: usize, vp_width: f32) -> usize {
        let mut used = self.name_col_width();
        let mut count = 0;
        for i in from_col..self.displayed_count() {
            let (name, _) = self.displayed_column_at(i).unwrap();
            let w = self.column_canonical_width(name);
            if used + w > vp_width && count > 0 {
                break;
            }
            used += w;
            count += 1;
        }
        count.max(1)
    }

    /// Scroll so the cell at (row_idx, col_name) is visible.
    pub(super) fn scroll_to_cell(&mut self, row: usize, col_name: &str) {
        let metrics = *self.viewport_metrics.lock();
        let mut changed = false;
        if row < self.first_row {
            self.first_row = row;
            changed = true;
        } else if row >= self.first_row + metrics.rows_in_view {
            self.first_row = row.saturating_sub(metrics.rows_in_view.saturating_sub(1));
            changed = true;
        }
        if col_name != ROW_NAME_SENTINEL_KEY {
            if let Some(ci) = self.displayed_index_of(col_name) {
                if ci < self.first_col {
                    self.first_col = ci;
                    changed = true;
                } else {
                    let vis =
                        self.actual_visible_cols(self.first_col, metrics.viewport_width);
                    if ci >= self.first_col + vis {
                        self.first_col = ci.saturating_sub(
                            self.actual_visible_cols(
                                ci.saturating_sub(metrics.cols_in_view),
                                metrics.viewport_width,
                            )
                            .saturating_sub(1),
                        );
                        changed = true;
                    }
                }
            }
        }
        if changed {
            self.ignore_overlay_reassert = true;
            self.update_subscriptions();
        }
    }

    /// Ensure at least one selected cell is visible in the viewport.
    pub(super) fn ensure_selection_visible(&mut self) {
        if self.selection.is_empty() {
            return;
        }
        let show_name = self.show_row_name.t.unwrap_or(true);
        let mut selection: LPooled<Vec<ArcStr>> = LPooled::take();
        selection.extend(self.selection.iter().cloned());
        // A selected path is a row (the name column) or `<row>/<col>`.
        let mut target: Option<(usize, ArcStr)> = None;
        'outer: for sel_path in selection.iter() {
            for (ri, row_path) in self.row_paths.iter().enumerate() {
                let row_str: &str = row_path;
                if show_name && sel_path.as_str() == row_str {
                    target = Some((ri, ROW_NAME_SENTINEL_KEY_ARC.clone()));
                    break 'outer;
                }
                if let Some(rest) = sel_path.as_str().strip_prefix(row_str) {
                    if let Some(col) = rest.strip_prefix('/') {
                        let col_arc = self
                            .displayed_columns()
                            .find(|(n, _)| n.as_str() == col)
                            .map(|(n, _)| n.clone())
                            .unwrap_or_else(|| ArcStr::from(col));
                        target = Some((ri, col_arc));
                        break 'outer;
                    }
                }
            }
        }
        if let Some((ri, col)) = target {
            self.scroll_to_cell(ri, &col);
        }
    }

    /// Auto-fit every column to its widest content over all rows.
    pub(super) fn auto_fit_all_columns(&mut self) {
        let show_name = self.show_row_name.t.unwrap_or(true);
        let mut inner = self.cells.inner.lock();
        let mut widths = self.user_widths.lock();
        if show_name {
            let mut w = col_header_width(ROW_NAME_HEADER_LABEL).max(MIN_COL_WIDTH);
            for p in &self.row_paths {
                let name = Path::basename(p).unwrap_or("");
                w = w.max(col_text_width(name).max(MIN_COL_WIDTH));
            }
            widths.insert(ROW_NAME_SENTINEL_KEY.into(), w);
        }
        match self.mode {
            DisplayMode::Table => {
                let cols: LPooled<Vec<ArcStr>> =
                    self.displayed_columns().map(|(name, _)| name.clone()).collect();
                for col_name in cols.iter() {
                    let entry = self.columns.get(col_name);
                    let is_fixed = entry
                        .map(|c| c.ref_width.is_some() && c.on_resize.is_none())
                        .unwrap_or(false);
                    if is_fixed {
                        continue;
                    }
                    let display = entry
                        .and_then(|c| c.spec.display_name.as_deref())
                        .unwrap_or(col_name);
                    let mut w = col_header_width(display).max(MIN_COL_WIDTH);
                    for row_path in self.row_paths.iter() {
                        let key = (row_path.clone(), col_name.clone());
                        let id = inner.cells.get(&key).copied();
                        let text =
                            id.and_then(|id| inner.formatted_for(id)).unwrap_or_else(
                                || self.default_for(col_name, row_basename(row_path)),
                            );
                        w = w.max(col_text_width(&text).max(MIN_COL_WIDTH));
                    }
                    widths.insert(col_name.clone(), w);
                }
            }
            DisplayMode::Value => {
                let mut w = col_header_width("value").max(MIN_COL_WIDTH);
                for row_path in self.row_paths.iter() {
                    let key = (row_path.clone(), VALUE_COL_KEY);
                    let id = inner.cells.get(&key).copied();
                    if let Some(text) = id.and_then(|id| inner.formatted_for(id)) {
                        w = w.max(col_text_width(&text).max(MIN_COL_WIDTH));
                    }
                }
                widths.insert("value".into(), w);
            }
        }
    }

    pub(super) fn col_type_for(&self, col_name: &str) -> &ColumnType {
        self.columns.get(col_name).map(|c| &c.spec.typ).unwrap_or(&ColumnType::Text)
    }

    /// The one row height for every cell this pass; a control column
    /// makes every row `ROW_HEIGHT_CONTROLS`. One height keeps the cell
    /// borders aligned.
    pub(super) fn row_height(&self) -> f32 {
        let tall = self.displayed_columns().any(|(_, c)| {
            matches!(
                c.spec.typ,
                ColumnType::Combo { .. } | ColumnType::Spin { .. } | ColumnType::Toggle
            )
        });
        if tall { ROW_HEIGHT_CONTROLS } else { ROW_HEIGHT_ESTIMATE }
    }
}
