//! Layout geometry helpers on `DataTableW`: visible ranges, column
//! widths, scroll math, and row-height selection. Everything here is
//! pure state math — no rendering, no subscription side effects.

use super::types::{
    col_header_width, col_text_width, row_basename, value_to_f64, ColumnType,
};
use super::{
    DataTableW, DisplayMode, MIN_COL_WIDTH, ROW_BUFFER, ROW_HEIGHT_CONTROLS,
    ROW_HEIGHT_ESTIMATE, ROW_NAME_KEY, ROW_NAME_KEY_ARC, ROW_NAME_LABEL, VALUE_COL_KEY,
};
use arcstr::ArcStr;
use graphix_rt::GXExt;
use netidx::path::Path;
use netidx::publisher::Value;
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
        self.cached_col_widths.lock().get(ROW_NAME_KEY).copied().unwrap_or(MIN_COL_WIDTH)
    }

    /// Data-column index (into the displayed prefix of `columns`)
    /// whose prefix-sum boundary is nearest the given virtual scroll
    /// offset. Snap-to-column: partial column visibility at the left
    /// edge is rounded to whichever boundary is closer. Accounts for
    /// the fixed name column at the start of the virtual content.
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

    /// Inverse of `col_at_offset`: virtual scroll offset corresponding
    /// to `first_col = ci`. Used to detect whether a scrollbar event
    /// reflects a real user drag or just the overlay re-asserting the
    /// keyboard-driven position.
    pub(super) fn offset_at_col(&self, ci: usize) -> f32 {
        let name_col_w = self.name_col_width();
        name_col_w
            + self
                .displayed_columns()
                .take(ci)
                .map(|(n, _)| self.column_canonical_width(n))
                .sum::<f32>()
    }

    /// String shown for a cell that has no live subscription value
    /// (either because the column is non-Netidx-sourced, the row is
    /// virtual, or the subscription is pending / went
    /// `Unsubscribed`). The lookup runs against the source's
    /// fallback — uniform value for every row, or per-row map keyed
    /// by row basename.
    pub(super) fn default_for(&self, col_name: &str, row_name: &str) -> ArcStr {
        let Some(c) = self.columns.get(col_name) else { return ArcStr::new() };
        let Some(entry) = c.source.as_ref() else { return ArcStr::new() };
        entry
            .parsed
            .lookup(row_name)
            .map(super::types::value_to_display)
            .unwrap_or_default()
    }

    /// Like `default_for`, but pulls the underlying Value (not its
    /// display string) and converts it to f64 directly. Used by
    /// sparkline default seeding to avoid a format-then-reparse round
    /// trip that would lose precision.
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

    /// Current raw `Value` for a cell: the live subscription value if
    /// one exists, else the column's default value for this row. The
    /// value-passing callbacks (Button `on_click`) need the typed
    /// payload, not its formatted display string.
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

    /// Returns the effective column width if explicitly set (by ref or user drag).
    /// None means auto-size from content.
    pub(super) fn effective_col_width(&self, col_name: &str) -> Option<f32> {
        // 1. User-set width (from drag resize)
        if let Some(w) = self.user_widths.lock().get(col_name) {
            return Some(*w);
        }
        // 2. Ref-controlled width
        self.columns.get(col_name).and_then(|c| c.ref_width)
    }

    /// Canonical width for `col_name`: `effective_col_width` (ref or user
    /// drag) wins because those are known up-front and authoritative;
    /// otherwise the last value cached during a render pass; otherwise
    /// `MIN_COL_WIDTH` as a last resort. The scroll math (virtual_width,
    /// `col_at_offset`, `offset_at_col`, `actual_visible_cols`,
    /// `min_first_col_for_fit`) all need to agree on this number — if
    /// virtual_width thinks a ref-controlled column is 80px wide because
    /// it hasn't been rendered yet, iced caps the scrollbar short and the
    /// rightmost columns are unreachable until something forces them to
    /// render (e.g. widening the window).
    pub(super) fn column_canonical_width(&self, col_name: &str) -> f32 {
        if let Some(w) = self.effective_col_width(col_name) {
            return w;
        }
        self.cached_col_widths
            .lock()
            .get(col_name)
            .copied()
            .unwrap_or(MIN_COL_WIDTH)
    }

    /// Smallest `first_col` such that `cols[first_col..total]` (plus the
    /// name column when shown) fit in `vp_width`. Below this, the
    /// rendered grid overflows and the rightmost column gets clipped.
    /// At or above this, the user is at scroll-end and the suffix fully
    /// fits — going higher would just expose empty space.
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

    /// Total virtual content width: name column + every data column at
    /// its canonical width. This is the value `render` hands to iced's
    /// `Scrollable` as the width of the `Space`-backed virtual content,
    /// and must agree with `col_at_offset` / `offset_at_col` so that
    /// scroll positions and column indices round-trip cleanly.
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

    /// Compute how many data columns fit from first_col given actual cached widths
    /// and the viewport width.
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

    /// Scroll viewport to ensure cell at (row_idx, col_name) is visible.
    pub(super) fn scroll_to_cell(&mut self, row: usize, col_name: &str) {
        let metrics = *self.viewport_metrics.lock();
        let mut changed = false;
        // Row scroll
        if row < self.first_row {
            self.first_row = row;
            changed = true;
        } else if row >= self.first_row + metrics.rows_in_view {
            self.first_row = row.saturating_sub(metrics.rows_in_view.saturating_sub(1));
            changed = true;
        }
        // Column scroll using actual cached widths (skip when
        // scrolling to the synthesized row-name column, which is
        // always at the viewport's left edge).
        if col_name != ROW_NAME_KEY {
            if let Some(ci) = self.displayed_index_of(col_name) {
                if ci < self.first_col {
                    self.first_col = ci;
                    changed = true;
                } else {
                    // Use actual widths to check if the column fits in viewport
                    let vis =
                        self.actual_visible_cols(self.first_col, metrics.viewport_width);
                    if ci >= self.first_col + vis {
                        // Scroll right so ci is the last visible column
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
            self.keyboard_scroll_override = true;
            self.update_subscriptions();
        }
    }

    /// Ensure at least one selected cell is visible in the viewport.
    pub(super) fn ensure_selection_visible(&mut self) {
        if self.selection.is_empty() {
            return;
        }
        let show_name = self.show_row_name.t.unwrap_or(true);
        // Clone into a pooled scratch buffer — `scroll_to_cell`
        // mutates `self`, so we can't hold a borrow on `self.selection`
        // while iterating. Refcount clone is cheap; the pool avoids
        // allocating a fresh Vec on every selection change.
        let mut selection: LPooled<Vec<ArcStr>> = LPooled::take();
        selection.extend(self.selection.iter().cloned());
        // For each selected path, look for a row whose path is the
        // prefix (row alone → name column; row + "/" + col → that
        // col). String matching on the shared path avoids building
        // per-row `<row>/` prefix strings.
        let mut target: Option<(usize, ArcStr)> = None;
        'outer: for sel_path in selection.iter() {
            for (ri, row_path) in self.row_paths.iter().enumerate() {
                let row_str: &str = row_path;
                if show_name && sel_path.as_str() == row_str {
                    target = Some((ri, ROW_NAME_KEY_ARC.clone()));
                    break 'outer;
                }
                if let Some(rest) = sel_path.as_str().strip_prefix(row_str) {
                    if let Some(col) = rest.strip_prefix('/') {
                        // Match against the existing ArcStr in
                        // `columns` so scroll_to_cell can compare by
                        // identity.
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

    /// Auto-fit all columns to their max content width (no cap).
    /// Scans ALL rows, not just visible ones.
    pub(super) fn auto_fit_all_columns(&mut self) {
        let show_name = self.show_row_name.t.unwrap_or(true);
        let mut inner = self.cells.inner.lock();
        let mut widths = self.user_widths.lock();
        if show_name {
            let mut w = col_header_width(ROW_NAME_LABEL).max(MIN_COL_WIDTH);
            for p in &self.row_paths {
                let name = Path::basename(p).unwrap_or("");
                w = w.max(col_text_width(name).max(MIN_COL_WIDTH));
            }
            widths.insert(ROW_NAME_KEY.into(), w);
        }
        match self.mode {
            DisplayMode::Table => {
                // Snapshot the displayed columns so the per-cell loop
                // doesn't fight a reborrow of `self.columns`.
                let cols: LPooled<Vec<ArcStr>> =
                    self.displayed_columns().map(|(name, _)| name.clone()).collect();
                for col_name in cols.iter() {
                    // Skip columns with fixed ref width and no on_resize
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
                        let text = id.and_then(|id| inner.formatted_for(id)).unwrap_or_else(
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

    /// Row height used by all data-row cells in this view pass.
    /// Tall-control columns (Combo, Spin, Toggle) force every row to
    /// the taller `ROW_HEIGHT_CONTROLS`; otherwise every row is
    /// `ROW_HEIGHT_ESTIMATE`. A single height for every cell in every
    /// row is what keeps the cell borders aligned — iced's Row lays
    /// children out at their individual natural heights when they are
    /// `Length::Shrink`, which produces ragged borders.
    pub(super) fn row_height(&self) -> f32 {
        let tall = self.displayed_columns().any(|(_, c)| {
            matches!(
                c.spec.typ,
                ColumnType::Combo { .. } | ColumnType::Spin { .. } | ColumnType::Toggle
            )
        });
        if tall {
            ROW_HEIGHT_CONTROLS
        } else {
            ROW_HEIGHT_ESTIMATE
        }
    }
}
