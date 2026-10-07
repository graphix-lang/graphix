//! Layout geometry for `DataTableW`: visible ranges, column widths,
//! scroll math, and row height.

use super::{
    DataTableW, DisplayMode, HEADER_HEIGHT, MIN_COL_WIDTH, ROW_BUFFER,
    ROW_HEIGHT_CONTROLS, ROW_HEIGHT_ESTIMATE, ROW_NAME_HEADER_LABEL, ROW_NAME_KEY,
    VALUE_COL_KEY, VALUE_HEADER_LABEL,
    types::{
        ColumnType, col_header_width, col_text_width, format_value, is_cell_path,
        row_basename, value_to_f64,
    },
};
use arcstr::ArcStr;
use graphix_rt::GXExt;
use netidx::{path::Path, protocol::valarray::ValArray, publisher::Value};
use poolshark::local::LPooled;

impl<X: GXExt> DataTableW<X> {
    /// The rows drawn: from `first_row`, as many as reach into the body,
    /// the clipped last one included.
    pub(super) fn display_row_range(&self) -> (usize, usize) {
        let n = self.row_paths.len();
        if n == 0 {
            return (0, 0);
        }
        let start = self.first_row.min(n - 1);
        let rows_in_view = self.viewport_metrics.lock().rows_in_view;
        (start, (start + rows_in_view).min(n))
    }

    pub(super) fn subscription_row_range(&self) -> (usize, usize) {
        let n = self.row_paths.len();
        let (ds, de) = self.display_row_range();
        (ds.saturating_sub(ROW_BUFFER), (de + ROW_BUFFER).min(n))
    }

    /// How many rows the body shows whole, at least one.
    pub(super) fn whole_rows_in_view(&self) -> usize {
        let m = *self.viewport_metrics.lock();
        if m.viewport_height <= 0.0 {
            return m.rows_in_view;
        }
        (((m.viewport_height - HEADER_HEIGHT) / self.row_height()).floor() as usize)
            .max(1)
    }

    /// The data columns drawn: from `first_col`, as many as reach into the
    /// viewport at their widths, the clipped last one included.
    pub(super) fn display_col_range(&self) -> (usize, usize) {
        let total = self.total_data_cols();
        if total == 0 {
            return (0, 0);
        }
        let start = self.first_col.min(total - 1);
        let avail = self.viewport_metrics.lock().viewport_width - self.name_col_width();
        let mut used = 0.0;
        let mut end = start;
        while end < total && (end == start || used < avail) {
            used += self.data_col_width(end);
            end += 1;
        }
        (start, end)
    }

    pub(super) fn total_data_cols(&self) -> usize {
        match self.mode {
            DisplayMode::Table => self.columns.len(),
            DisplayMode::Value => 1,
        }
    }

    /// Canonical width of the data column at display index `ci`.
    fn data_col_width(&self, ci: usize) -> f32 {
        match self.mode {
            DisplayMode::Value => self.column_canonical_width(&VALUE_COL_KEY),
            DisplayMode::Table => match self.columns.get_index(ci) {
                Some((name, _)) => self.column_canonical_width(name),
                None => MIN_COL_WIDTH,
            },
        }
    }

    /// Whether the data column `ci` is drawn whole in the viewport.
    fn col_fully_visible(&self, ci: usize) -> bool {
        let avail = self.viewport_metrics.lock().viewport_width - self.name_col_width();
        ci >= self.first_col
            && (self.first_col..=ci).map(|i| self.data_col_width(i)).sum::<f32>() <= avail
    }

    /// Pixel width of the synthesized row-name column (0 when hidden).
    pub(super) fn name_col_width(&self) -> f32 {
        if !self.show_row_name.t.unwrap_or(true) {
            return 0.0;
        }
        self.column_canonical_width(&ROW_NAME_KEY)
    }

    /// The data column whose left edge is nearest the scroll offset `ox`.
    /// The row-name column is pinned, so offsets count data columns only.
    pub(super) fn col_at_offset(&self, ox: f32) -> usize {
        let n = self.total_data_cols();
        let mut acc = 0.0;
        for i in 0..n {
            let next = acc + self.data_col_width(i);
            if ox < (acc + next) / 2.0 {
                return i;
            }
            acc = next;
        }
        n.saturating_sub(1)
    }

    /// Inverse of `col_at_offset`: the scroll offset of `first_col = ci`.
    pub(super) fn offset_at_col(&self, ci: usize) -> f32 {
        (0..ci).map(|i| self.data_col_width(i)).sum()
    }

    /// String shown for a cell with no live subscription value.
    pub(super) fn default_for(&self, col_name: &str, row_name: &str) -> ArcStr {
        self.columns
            .get(col_name)
            .and_then(|c| c.source.as_ref())
            .and_then(|e| e.parsed.lookup(row_name))
            .map(format_value)
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
        if let Some(v) = self.cells.inner.lock().live_value(row_path, col_name) {
            return Some(v.clone());
        }
        self.columns
            .get(col_name.as_str())
            .and_then(|c| c.source.as_ref())
            .and_then(|e| e.parsed.lookup(row_basename(row_path)))
            .cloned()
    }

    /// The column's set width: its width ref's, which the program owns,
    /// else the user's (a drag, an auto-fit); `None` sizes it to content.
    pub(super) fn explicit_col_width(&self, col_name: &str) -> Option<f32> {
        match self.columns.get(col_name).and_then(|c| c.ref_width) {
            Some(w) => Some(w),
            None => self.user_widths.lock().get(col_name).copied(),
        }
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
        let avail = (vp_width - self.name_col_width()).max(0.0);
        let mut acc = 0.0;
        for k in (0..self.total_data_cols()).rev() {
            acc += self.data_col_width(k);
            if acc > avail {
                return k + 1;
            }
        }
        0
    }

    /// Total virtual content width: the name column plus every data
    /// column at its canonical width.
    pub(super) fn virtual_content_width(&self) -> f32 {
        self.name_col_width() + self.offset_at_col(self.total_data_cols())
    }

    /// Scroll so the cell at (row, col) is drawn whole, and move the
    /// overlay to match.
    pub(super) fn scroll_to_cell(&mut self, row: usize, col_name: &ArcStr) {
        let (first, whole) = (self.first_row, self.whole_rows_in_view());
        if row < self.first_row {
            self.first_row = row;
        } else if row >= self.first_row + whole {
            self.first_row = row + 1 - whole;
        }
        let first_col = self.first_col;
        if let Some(ci) = self.columns.get_index_of(col_name.as_str()) {
            if ci < self.first_col {
                self.first_col = ci;
            } else if !self.col_fully_visible(ci) {
                let avail =
                    self.viewport_metrics.lock().viewport_width - self.name_col_width();
                let mut k = ci;
                let mut used = self.data_col_width(ci);
                while k > 0 && used + self.data_col_width(k - 1) <= avail {
                    k -= 1;
                    used += self.data_col_width(k);
                }
                self.first_col = k;
            }
        }
        if (first, first_col) != (self.first_row, self.first_col) {
            self.scroll_dirty = true;
            self.update_subscriptions();
        }
    }

    /// Scroll to a selected cell unless one is already drawn whole.
    pub(super) fn ensure_selection_visible(&mut self) {
        if self.selection.is_empty() {
            return;
        }
        let cols = self.navigable_columns_with_name();
        let rows = self.first_row..self.first_row + self.whole_rows_in_view();
        let mut target: Option<(usize, ArcStr)> = None;
        for sel in self.selection.iter() {
            for (ri, rp) in self.row_paths.iter().enumerate() {
                if !sel.starts_with(&**rp) {
                    continue;
                }
                for c in cols.iter().filter(|c| is_cell_path(sel, rp, c)) {
                    let col_visible = match self.columns.get_index_of(c.as_str()) {
                        Some(ci) => self.col_fully_visible(ci),
                        None => true,
                    };
                    if rows.contains(&ri) && col_visible {
                        return;
                    }
                    target.get_or_insert_with(|| (ri, c.clone()));
                }
            }
        }
        if let Some((ri, col)) = target {
            self.scroll_to_cell(ri, &col);
        }
    }

    /// Every column a selection can name, the row-name column included.
    fn navigable_columns_with_name(&self) -> LPooled<Vec<ArcStr>> {
        let mut cols: LPooled<Vec<ArcStr>> = LPooled::take();
        cols.push(ROW_NAME_KEY);
        match self.mode {
            DisplayMode::Table => cols.extend(self.columns.keys().cloned()),
            DisplayMode::Value => cols.push(VALUE_COL_KEY),
        }
        cols
    }

    /// Auto-fit every column to its widest content over all rows. A
    /// column with a width ref is the program's: it is asked through its
    /// `on_resize`, and one without is left alone.
    pub(super) fn auto_fit_all_columns(&mut self) {
        let mut fits: LPooled<Vec<(ArcStr, f32)>> = LPooled::take();
        {
            let mut inner = self.cells.inner.lock();
            let mut widest = |label: &str, col: &ArcStr| {
                let mut w = col_header_width(label).max(MIN_COL_WIDTH);
                for rp in self.row_paths.iter() {
                    let text = if col == &ROW_NAME_KEY {
                        ArcStr::from(row_basename(rp))
                    } else {
                        let id = inner.cells.get(&(rp.clone(), col.clone())).copied();
                        id.and_then(|id| inner.formatted_for(id))
                            .unwrap_or_else(|| self.default_for(col, row_basename(rp)))
                    };
                    w = w.max(col_text_width(&text));
                }
                w
            };
            if self.show_row_name.t.unwrap_or(true) {
                fits.push((ROW_NAME_KEY, widest(ROW_NAME_HEADER_LABEL, &ROW_NAME_KEY)));
            }
            match self.mode {
                DisplayMode::Table => {
                    for (name, c) in self.columns.iter() {
                        let label = c.spec.display_name.as_deref().unwrap_or(name);
                        fits.push((name.clone(), widest(label, name)));
                    }
                }
                DisplayMode::Value => {
                    fits.push((VALUE_COL_KEY, widest(VALUE_HEADER_LABEL, &VALUE_COL_KEY)))
                }
            }
        }
        let mut user_widths = self.user_widths.lock();
        for (name, w) in fits.drain(..) {
            match self.columns.get(&name) {
                Some(c) if c.ref_width.is_some() => {
                    if let Some(f) = &c.on_resize {
                        let _ = self
                            .gx
                            .call(f.id(), ValArray::from_iter([Value::F64(w as f64)]));
                    }
                }
                _ => {
                    user_widths.insert(name, w);
                }
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
        let tall = self.columns.values().any(|c| {
            matches!(
                c.spec.typ,
                ColumnType::Combo { .. } | ColumnType::Spin { .. } | ColumnType::Toggle
            )
        });
        if tall { ROW_HEIGHT_CONTROLS } else { ROW_HEIGHT_ESTIMATE }
    }
}
