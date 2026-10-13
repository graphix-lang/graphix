//! Layout geometry for `DataTableW`: visible ranges, column widths,
//! scroll math, and row height.

use super::{
    DataTableW, DisplayMode, HEADER_HEIGHT, MIN_COL_WIDTH, ROW_BUFFER,
    ROW_HEIGHT_CONTROLS, ROW_HEIGHT_ESTIMATE, ROW_NAME_HEADER_LABEL, ROW_NAME_KEY,
    VALUE_COL_KEY, VALUE_HEADER_LABEL,
    subscriptions::SharedCellsInner,
    types::{
        ColumnType, col_header_width, col_text_width, format_value, row_basename,
        value_to_f64,
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
        if !self.show_row_name() {
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

    /// The displayed cell text, using a held subscription lock.
    pub(super) fn cell_text(
        &self,
        inner: &mut SharedCellsInner,
        row: &Path,
        col: &ArcStr,
    ) -> ArcStr {
        let col = if self.mode == DisplayMode::Value { &VALUE_COL_KEY } else { col };
        let id = inner.cells.get(&(row.clone(), col.clone())).copied();
        if let Some(text) = id.and_then(|id| inner.formatted_for(id)) {
            return text;
        }
        match self.mode {
            DisplayMode::Value => ArcStr::new(),
            DisplayMode::Table => self
                .columns
                .get(col)
                .and_then(|c| c.source.as_ref())
                .and_then(|e| e.parsed.lookup(row_basename(row)))
                .map(format_value)
                .unwrap_or_default(),
        }
    }

    /// The column fallback as an `f64`, converted from its raw value.
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
        self.raw_cell_value(&self.cells.inner.lock(), row_path, col_name).cloned()
    }

    pub(super) fn raw_cell_value<'a>(
        &'a self,
        inner: &'a SharedCellsInner,
        row_path: &Path,
        col_name: &ArcStr,
    ) -> Option<&'a Value> {
        inner.live_value(row_path, col_name).or_else(|| {
            self.columns
                .get(col_name)
                .and_then(|c| c.source.as_ref())
                .and_then(|e| e.parsed.lookup(row_basename(row_path)))
        })
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
    // XCR claude for claude: [perf/low] This scans every row for every selected path on
    // each selection update. is_selected scans the whole selection for every drawn cell
    // on every frame, and handle_table_key scans selection × rows on every key. A
    // select-all over a 20k-row table makes each update about 4e8 checks and each frame
    // 20k checks per cell. The selection is an AHashSet, whose iteration order is
    // randomly seeded, so with several selected cells both this scroll target and the
    // keyboard's current cell are arbitrary. Fix: index the selection by row path (row
    // → selected columns) in parse_selection, find rows through a path → index map
    // built in sort_rows, and choose the lowest (row, column) deterministically.
    // (gui-datatable-10)
    // 2026-10-13 codex: selection_indexes_exact_cells pins indexed membership and row reordering; selection_keeps_program_order pins the chosen order.
    // Program order follows tests-gui-a-01 rather than lowest display index. The
    // multiple_selections_use_program_order live pin needs resolver sockets.
    pub(super) fn ensure_selection_visible(&mut self) {
        let rows = self.first_row..self.first_row + self.whole_rows_in_view();
        let mut target: Option<(usize, ArcStr)> = None;
        for (ri, col) in self.selection.cells() {
            let col_visible = match self.columns.get_index_of(col.as_str()) {
                Some(ci) => self.col_fully_visible(ci),
                None => true,
            };
            if rows.contains(&ri) && col_visible {
                return;
            }
            target.get_or_insert_with(|| (ri, col.clone()));
        }
        if let Some((ri, col)) = target {
            self.scroll_to_cell(ri, &col);
        }
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
                        self.cell_text(&mut inner, rp, col)
                    };
                    w = w.max(col_text_width(&text));
                }
                w
            };
            if self.show_row_name() {
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
