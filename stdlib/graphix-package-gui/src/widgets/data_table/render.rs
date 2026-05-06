//! Rendering path for `DataTableW`: the `render_with_size` body,
//! per-cell rendering, wrapper layers (keyboard, resize drag), and the
//! sparkline canvas program.

use super::types::{
    cell_path_matches, col_header_width, col_min_width, row_basename, truncate_to_width,
    ColumnType, SortDirection,
};
use super::{
    DataTableW, DisplayMode, IcedElement, Message, Renderer, DEFAULT_MAX_COL_WIDTH,
    MIN_COL_WIDTH, RESIZE_HANDLE_WIDTH, ROW_HEIGHT_ESTIMATE, ROW_NAME_KEY,
    ROW_NAME_KEY_ARC, ROW_NAME_LABEL, VALUE_COL_KEY,
};
use crate::theme::GraphixTheme;
use arcstr::{literal, ArcStr};
use compact_str::{format_compact, CompactString};
use fxhash::FxHashMap;
use graphix_rt::GXExt;
use iced_widget as widget;
use netidx::{path::Path, protocol::valarray::ValArray, publisher::Value};
use poolshark::local::LPooled;

type Col<'a> = widget::Column<'a, Message, GraphixTheme, Renderer>;
type Row<'a> = widget::Row<'a, Message, GraphixTheme, Renderer>;

/// Unicode subscript digit (U+2080..U+2089) for 1–9, used to annotate
/// sort-priority in the header indicator. Returns `""` outside the
/// supported range — nine columns of tie-breaking is more than any
/// reasonable spreadsheet workflow, and a missing digit is clearer
/// than a wrong one if it somehow gets hit.
fn subscript_digit(n: usize) -> &'static str {
    match n {
        1 => "₁",
        2 => "₂",
        3 => "₃",
        4 => "₄",
        5 => "₅",
        6 => "₆",
        7 => "₇",
        8 => "₈",
        9 => "₉",
        _ => "",
    }
}

/// Y-axis bounds for a sparkline cell. Both ends are pre-resolved
/// during view(): the column-wide union of all rows' values for axes
/// the caller didn't fix, and the user-supplied `min`/`max` from the
/// `Sparkline` column type for the ones they did.
#[derive(Clone, Copy)]
pub(super) struct SparkBounds {
    min: f64,
    max: f64,
}

/// Canvas program that draws a sparkline polyline.
struct SparklineCanvas {
    /// (time_offset_secs, value) pairs
    points: Vec<(f64, f64)>,
    /// Y-axis bounds resolved by the caller (column-wide auto-scale or
    /// user-fixed via the column type's `min`/`max`).
    bounds: SparkBounds,
}

impl<Message> widget::canvas::Program<Message, GraphixTheme, Renderer>
    for SparklineCanvas
{
    type State = ();

    fn draw(
        &self,
        _state: &Self::State,
        renderer: &Renderer,
        _theme: &GraphixTheme,
        bounds: iced_core::Rectangle,
        _cursor: iced_core::mouse::Cursor,
    ) -> Vec<widget::canvas::Geometry<Renderer>> {
        use widget::canvas::{Frame, Path, Stroke};
        let mut frame = Frame::new(renderer, bounds.size());
        if self.points.len() < 2 {
            return vec![frame.into_geometry()];
        }
        let (mut min_v, mut max_v) = (self.bounds.min, self.bounds.max);
        let (min_t, max_t) =
            (self.points.first().unwrap().0, self.points.last().unwrap().0);
        if (max_v - min_v).abs() < 1e-12 {
            min_v -= 1.0;
            max_v += 1.0;
        }
        let t_range = (max_t - min_t).max(1e-12);
        let v_range = max_v - min_v;
        let w = bounds.width;
        let h = bounds.height;
        // Decimate: if more points than pixels, sample.
        let max_points = (w as usize).max(1);
        let step = if self.points.len() > max_points {
            self.points.len() / max_points
        } else {
            1
        };
        // Build path
        let path = Path::new(|builder| {
            let mut first = true;
            for (i, &(t, v)) in self.points.iter().enumerate() {
                if i % step != 0 && i != self.points.len() - 1 {
                    continue;
                }
                let x = ((t - min_t) / t_range) as f32 * w;
                let y = h - ((v - min_v) / v_range) as f32 * h;
                if first {
                    builder.move_to(iced_core::Point::new(x, y));
                    first = false;
                } else {
                    builder.line_to(iced_core::Point::new(x, y));
                }
            }
        });
        frame.stroke(
            &path,
            Stroke::default()
                .with_color(iced_core::Color::from_rgb(0.3, 0.7, 1.0))
                .with_width(1.5),
        );
        vec![frame.into_geometry()]
    }
}

impl<X: GXExt> DataTableW<X> {
    /// Build the per-column header sort indicator suffix. A column not
    /// in `sort_by` gets no entry. A column in `sort_by` gets a leading
    /// space and an arrow: ▲ for ascending, ▼ for descending. When
    /// there's more than one sort column, each indicator also gets a
    /// unicode subscript digit (₁ ₂ ₃…) showing its 1-based priority
    /// so the user can tell primary from tie-breakers.
    pub(super) fn build_sort_indicators(
        &self,
    ) -> LPooled<FxHashMap<ArcStr, CompactString>> {
        let multi = self.sort_by.len() > 1;
        self.sort_by
            .iter()
            .enumerate()
            .map(|(idx, sb)| {
                let arrow = match sb.direction {
                    SortDirection::Ascending => "▲",
                    SortDirection::Descending => "▼",
                };
                let s = if multi {
                    let sub = subscript_digit(idx + 1);
                    format_compact!(" {arrow}{sub}")
                } else {
                    format_compact!(" {arrow}")
                };
                (sb.column.clone(), s)
            })
            .collect()
    }

    pub(super) fn render_with_size(&self, size: iced_core::Size) -> IcedElement<'_> {
        // Update viewport metrics from the actual layout size. The
        // `dirty` flag is consumed in `before_view` to trigger a
        // deferred `update_subscriptions()` on resize — we can't call
        // it from here because the closure only holds `&self`.
        {
            let row_h = self.row_height();
            let header_h = ROW_HEIGHT_ESTIMATE;
            let body_h = (size.height - header_h).max(0.0);
            let rows_in_view = ((body_h / row_h).ceil() as usize).max(1);
            let name_cols = if self.show_row_name.t.unwrap_or(true) { 1 } else { 0 };
            let cols_in_view = ((size.width / MIN_COL_WIDTH).ceil() as usize)
                .saturating_sub(name_cols)
                .max(1);
            let mut m = self.viewport_metrics.lock();
            let changed = m.viewport_width != size.width
                || m.viewport_height != size.height
                || m.rows_in_view != rows_in_view
                || m.cols_in_view != cols_in_view;
            if changed {
                *m = super::types::ViewportMetrics {
                    viewport_width: size.width,
                    viewport_height: size.height,
                    rows_in_view,
                    cols_in_view,
                    dirty: m.dirty || m.rows_in_view != rows_in_view,
                };
            }
        }
        let num_rows = self.row_paths.len();
        let show_row_name = self.show_row_name.t.unwrap_or(true);
        let on_header_click = self.on_header_click.as_ref().map(|c| c.id());
        let bold = iced_core::Font {
            weight: iced_core::font::Weight::Bold,
            ..iced_core::Font::DEFAULT
        };
        let (vis_row_start, vis_row_end) = self.display_row_range();
        let (vis_col_start, vis_col_end) = self.display_col_range();
        // Snapshot cell data (need it for column width computation too).
        // Materialize from the identity-keyed grid into a Vec<Vec<ArcStr>>
        // sized [visible_rows][all_cols]; falling back to per-cell defaults
        // for entries no subscription has filled in yet. ArcStr clones
        // are refcount bumps — the grid's stored values are reused
        // across width measurement, header rendering, and per-cell
        // rendering without re-allocating.
        let grid_snapshot: LPooled<Vec<LPooled<Vec<ArcStr>>>> = {
            let mut inner = self.cells.inner.lock();
            (vis_row_start..vis_row_end)
                .map(|i| match self.row_paths.get(i) {
                    None => LPooled::take(),
                    Some(row_path) => match self.mode {
                        DisplayMode::Table => self
                            .displayed_columns()
                            .map(|(cn, _)| {
                                let key = (row_path.clone(), cn.clone());
                                let id = inner.cells.get(&key).copied();
                                id.and_then(|id| inner.formatted_for(id)).unwrap_or_else(
                                    || self.default_for(cn, row_basename(row_path)),
                                )
                            })
                            .collect(),
                        DisplayMode::Value => {
                            let key = (row_path.clone(), VALUE_COL_KEY);
                            let id = inner.cells.get(&key).copied();
                            let v = id
                                .and_then(|id| inner.formatted_for(id))
                                .unwrap_or_default();
                            let mut res: LPooled<Vec<ArcStr>> = LPooled::take();
                            res.push(v);
                            res
                        }
                    },
                })
                .collect()
        };
        let name_col_offset = if show_row_name { 1 } else { 0 };
        let sort_indicators: LPooled<FxHashMap<ArcStr, CompactString>> =
            self.build_sort_indicators();
        // Column metadata with widths.
        // If effective_col_width returns Some, use it directly.
        // Otherwise auto-size from content (up to DEFAULT_MAX_COL_WIDTH)
        // and lock the result into user_widths.
        let mut col_meta: LPooled<Vec<(ArcStr, f32)>> = LPooled::take();
        if show_row_name {
            let w = match self.effective_col_width(ROW_NAME_KEY) {
                Some(w) => w,
                None => {
                    let max_w = DEFAULT_MAX_COL_WIDTH;
                    let mut w = col_min_width(ROW_NAME_LABEL, max_w);
                    for row_idx in vis_row_start..vis_row_end {
                        if let Some(p) = self.row_paths.get(row_idx) {
                            let name = Path::basename(p).unwrap_or("");
                            w = w.max(col_min_width(name, max_w));
                        }
                    }
                    w
                }
            };
            col_meta.push((ROW_NAME_KEY_ARC.clone(), w));
        }
        match self.mode {
            DisplayMode::Table => {
                for i in vis_col_start..vis_col_end {
                    let (name, state) = match self.displayed_column_at(i) {
                        Some(p) => p,
                        None => break,
                    };
                    let w = match self.effective_col_width(name) {
                        Some(w) => w,
                        None => {
                            let max_w = DEFAULT_MAX_COL_WIDTH;
                            let display = state
                                .spec
                                .display_name
                                .as_deref()
                                .unwrap_or(name.as_str());
                            // Include the sort indicator (if any) in
                            // header-width measurement so the arrow
                            // can't be clipped or push the text against
                            // the resize handle when auto-sizing.
                            let header: CompactString = match sort_indicators.get(name) {
                                Some(ind) => format_compact!("{display}{ind}"),
                                None => display.into(),
                            };
                            // Header text (bold, 14pt) sets minimum
                            let mut w =
                                col_header_width(&header).max(MIN_COL_WIDTH).min(max_w);
                            // Cell content may be wider
                            for row in &*grid_snapshot {
                                if i < row.len() {
                                    w = w.max(col_min_width(&row[i], max_w));
                                }
                            }
                            w
                        }
                    };
                    col_meta.push((name.clone(), w));
                }
            }
            DisplayMode::Value => {
                let w = match self.effective_col_width("value") {
                    Some(w) => w,
                    None => {
                        let max_w = DEFAULT_MAX_COL_WIDTH;
                        let mut w = col_min_width("value", max_w);
                        for row in &*grid_snapshot {
                            if let Some(v) = row.first() {
                                w = w.max(col_min_width(v, max_w));
                            }
                        }
                        w
                    }
                };
                col_meta.push((literal!("value"), w));
            }
        }
        // Cache column widths for keyboard nav viewport calculations.
        // Accumulated across frames (not cleared here) so columns that
        // have scrolled out of view retain their last-known widths —
        // virtual_width below needs an estimate of the *entire*
        // content's width to get the horizontal scrollbar range right,
        // not just the currently-visible slice. `apply_table` clears
        // the cache when the column set changes.
        {
            let mut cache = self.cached_col_widths.lock();
            for (name, w) in &*col_meta {
                cache.insert(name.clone(), *w);
            }
        }
        // Header row — styled identically to data cells (same padding, borders)
        let mut header_row = Row::new().spacing(0);
        for (ci, (name, w)) in col_meta.iter().enumerate() {
            let is_data_col = ci >= name_col_offset;
            let entry = self.columns.get(name);
            let header_text: ArcStr = if is_data_col {
                let base = entry
                    .and_then(|c| c.spec.display_name.clone())
                    .unwrap_or_else(|| name.clone());
                match sort_indicators.get(name) {
                    Some(ind) => format_compact!("{base}{ind}").as_str().into(),
                    None => base,
                }
            } else {
                // Synthesized row-name column: render the user-facing
                // label, not the internal sentinel key.
                literal!("name")
            };
            let is_fixed = entry
                .map(|c| c.ref_width.is_some() && c.on_resize.is_none())
                .unwrap_or(false);
            // Render the header as plain text; if there's a header-click
            // callback wrap it in a `MouseArea` so the label stays flush
            // with the cell (a `Button` would add its own background,
            // border, and padding, making the header row visibly taller
            // than the row-name header cell). `Interaction::Pointer`
            // gives the user a hover cue that the header is clickable.
            let plain_text: IcedElement<'_> = widget::text(header_text.to_string())
                .size(14)
                .font(bold)
                .wrapping(iced_core::text::Wrapping::None)
                .into();
            let text_el: IcedElement<'_> = if is_data_col {
                if let Some(cid) = on_header_click {
                    widget::MouseArea::<'_, Message, GraphixTheme, Renderer>::new(
                        plain_text,
                    )
                    .interaction(iced_core::mouse::Interaction::Pointer)
                    .on_press(Message::Call(
                        cid,
                        ValArray::from_iter([Value::String(name.clone())]),
                    ))
                    .into()
                } else {
                    plain_text
                }
            } else {
                plain_text
            };
            // Use same styling as wrap_cell: same width, padding, border
            let inner: IcedElement<'_> = if !is_fixed {
                let handle: IcedElement<'_> =
                    widget::MouseArea::<'_, Message, GraphixTheme, Renderer>::new(
                        widget::container(widget::Space::new())
                            .width(RESIZE_HANDLE_WIDTH)
                            .height(iced_core::Length::Fill),
                    )
                    .interaction(iced_core::mouse::Interaction::ResizingColumn)
                    .on_press(Message::ColumnResizeStart(ci))
                    .into();
                Row::new()
                    .push(text_el)
                    .push(iced_widget::Space::new().width(iced_core::Length::Fill))
                    .push(handle)
                    .spacing(0)
                    .into()
            } else {
                text_el
            };
            let cell: IcedElement<'_> = widget::container(inner)
                .width(*w)
                .height(iced_core::Length::Shrink)
                .padding(iced_core::Padding::from([3, 5]))
                .style(|theme: &GraphixTheme| {
                    let p = theme.palette();
                    widget::container::Style {
                        border: iced_core::Border {
                            color: iced_core::Color { a: 0.15, ..p.text },
                            width: 0.5,
                            radius: 0.0.into(),
                        },
                        ..Default::default()
                    }
                })
                .into();
            header_row = header_row.push(cell);
        }
        // Data rows
        let _has_on_select = self.on_select.is_some();
        let row_h = self.row_height();
        // Resolve sparkline y-axis bounds once per column. Default
        // behavior: union of every row's points so cells in the same
        // column are visually comparable. The column type's `min`/`max`
        // override either end when set.
        let spark_bounds_by_col: FxHashMap<ArcStr, SparkBounds> =
            self.compute_sparkline_bounds();
        let mut body = Col::new()
            .spacing(0)
            .width(iced_core::Length::Shrink)
            .height(iced_core::Length::Shrink);
        for (vi, row_idx) in (vis_row_start..vis_row_end).enumerate() {
            let _ = row_idx; // used below in cell rendering
            let mut row_w = Row::new().spacing(0);
            for (ci, (col_name, w)) in col_meta.iter().enumerate() {
                let is_name_col = ci == 0 && show_row_name;
                let cell_el: IcedElement<'_> = if is_name_col {
                    let name =
                        self.row_paths.get(row_idx).map(row_basename).unwrap_or("");
                    // Name column's selection key is the row path
                    // itself, not `<row>/<col>`.
                    let is_sel = self
                        .row_paths
                        .get(row_idx)
                        .map(|row_path| {
                            self.selection
                                .iter()
                                .any(|sel| sel.as_str() == row_path.as_ref() as &str)
                        })
                        .unwrap_or(false);
                    let inner: IcedElement<'_> =
                        widget::text(truncate_to_width(name, *w))
                            .size(13)
                            .wrapping(iced_core::text::Wrapping::None)
                            .into();
                    self.wrap_cell(inner, col_name, row_idx, *w, row_h, is_sel)
                } else {
                    let data_col = vis_col_start + ci - name_col_offset;
                    let text: ArcStr = if vi < grid_snapshot.len()
                        && data_col < grid_snapshot[vi].len()
                    {
                        grid_snapshot[vi][data_col].clone()
                    } else {
                        ArcStr::new()
                    };
                    let col_type = if self.mode == DisplayMode::Table {
                        match self.displayed_column_at(data_col) {
                            Some((name, _)) => self.col_type_for(name),
                            None => &ColumnType::Text,
                        }
                    } else {
                        &ColumnType::Text
                    };
                    let sb = spark_bounds_by_col.get(col_name).copied();
                    self.render_cell(
                        col_name, col_type, text, row_idx, *w, row_h, false, sb,
                    )
                };
                row_w = row_w.push(cell_el);
            }
            let row_el: IcedElement<'_> = row_w.into();
            body = body.push(row_el);
        }
        let grid_area: IcedElement<'_> = Col::new()
            .push(header_row)
            .push(body)
            .width(iced_core::Length::Shrink)
            .height(iced_core::Length::Fill)
            .into();
        let row_h = self.row_height();
        let header_h = ROW_HEIGHT_ESTIMATE;
        // Virtual content size drives iced's scrollable range and its
        // auto-hide: a bar shows iff `content_bounds > bounds` in that
        // axis (`vendor/iced_widget/src/scrollable.rs:1949-1955`).
        //
        // Critical: this must reflect the **entire** content extent,
        // not just the currently-rendered slice. `col_meta` holds only
        // the visible column window, so summing it is wrong — once the
        // user scrolls right, the off-screen left columns are not in
        // `col_meta` and the sum shrinks, which iced then reads as
        // "content fits" and hides the scrollbar mid-scroll. Instead,
        // we sum across all `col_names`, falling back to
        // `MIN_COL_WIDTH` for columns whose widths haven't been
        // measured yet because they've never been rendered.
        // `cached_col_widths` persists across frames for this purpose.
        let virtual_width = self.virtual_content_width();
        let virtual_height = num_rows as f32 * row_h + header_h;
        let virtual_content: IcedElement<'_> =
            widget::Space::new().width(virtual_width).height(virtual_height).into();
        let scroll_overlay: IcedElement<'_> =
            widget::Scrollable::<'_, Message, GraphixTheme, Renderer>::new(
                virtual_content,
            )
            .direction(widget::scrollable::Direction::Both {
                vertical: widget::scrollable::Scrollbar::default(),
                horizontal: widget::scrollable::Scrollbar::default(),
            })
            .on_scroll(|vp| {
                let abs = vp.absolute_offset();
                let bounds = vp.bounds();
                Message::Scroll(abs.x, abs.y, bounds.width, bounds.height)
            })
            .width(iced_core::Length::Fill)
            .height(iced_core::Length::Fill)
            .into();
        let stacked: IcedElement<'_> =
            widget::Stack::<'_, Message, GraphixTheme, Renderer>::new()
                .push(grid_area)
                .push(scroll_overlay)
                .width(iced_core::Length::Fill)
                .height(iced_core::Length::Fill)
                .into();
        // Clip the final output so a partial last row (rendered because
        // `rows_in_view` is `ceil`'d for the "more below" affordance)
        // doesn't bleed past the table's allocated bounds into widgets
        // below. `container::clip(true)` sets iced's scissor rect
        // during draw only — layout, hit-testing, focus traversal and
        // iced overlay dropdowns (pick_list, tooltips) are unaffected.
        let clipped: IcedElement<'_> = widget::container(stacked)
            .width(iced_core::Length::Fill)
            .height(iced_core::Length::Fill)
            .clip(true)
            .into();
        self.wrap_keyboard(self.wrap_resize_drag(clipped))
    }

    /// Wrap the table's rendered view in a top-level MouseArea that
    /// emits `ColumnResizeMove` on every cursor move and
    /// `ColumnResizeEnd` on left-button release. The event loop's
    /// drain arm filters those against `is_column_resizing`, so there
    /// is no per-frame cost for regular hovering — just one extra
    /// message per mouse-move event while the cursor is over the
    /// table. Drag state changes (start/move/end) all go through the
    /// same message drain cycle, which is what fixes the timing bug
    /// where `CursorMoved` events arriving in `window_event` saw a
    /// stale `is_column_resizing()` result.
    fn wrap_resize_drag<'a>(&'a self, content: IcedElement<'a>) -> IcedElement<'a> {
        widget::MouseArea::<'_, Message, GraphixTheme, Renderer>::new(content)
            .on_move(|pt| Message::ColumnResizeMove(pt.x))
            .on_release(Message::ColumnResizeEnd)
            .into()
    }

    fn wrap_keyboard<'a>(&'a self, content: IcedElement<'a>) -> IcedElement<'a> {
        use crate::widgets::iced_keyboard_area::KeyboardArea;
        use crate::widgets::TableKeyAction;
        use iced_core::keyboard;

        KeyboardArea::new(content)
            .on_key_press(|event| match event {
                keyboard::Event::KeyPressed { key, .. } => {
                    use iced_core::keyboard::Key;
                    match key {
                        Key::Named(keyboard::key::Named::ArrowUp) => {
                            Message::TableKey(TableKeyAction::Up)
                        }
                        Key::Named(keyboard::key::Named::ArrowDown) => {
                            Message::TableKey(TableKeyAction::Down)
                        }
                        Key::Named(keyboard::key::Named::ArrowLeft) => {
                            Message::TableKey(TableKeyAction::Left)
                        }
                        Key::Named(keyboard::key::Named::ArrowRight) => {
                            Message::TableKey(TableKeyAction::Right)
                        }
                        Key::Named(keyboard::key::Named::Enter) => {
                            Message::TableKey(TableKeyAction::Enter)
                        }
                        Key::Named(keyboard::key::Named::Space) => {
                            Message::TableKey(TableKeyAction::Space)
                        }
                        Key::Named(keyboard::key::Named::Escape) => {
                            Message::TableKey(TableKeyAction::Escape)
                        }
                        _ => Message::Nop,
                    }
                }
                _ => Message::Nop,
            })
            .into()
    }

    /// Wrap a cell's inner content with spreadsheet-style container:
    /// thin border, flat background, click-to-select.
    fn wrap_cell<'a>(
        &'a self,
        inner: IcedElement<'a>,
        col_name: &ArcStr,
        row_idx: usize,
        w: f32,
        row_h: f32,
        is_selected: bool,
    ) -> IcedElement<'a> {
        let col_for_msg = col_name.clone();
        let styled: IcedElement<'a> = widget::container(inner)
            .width(w)
            .height(iced_core::Length::Fixed(row_h))
            .padding(iced_core::Padding::from([3, 5]))
            .style(move |theme: &GraphixTheme| {
                let p = theme.palette();
                widget::container::Style {
                    background: if is_selected {
                        Some(iced_core::Background::Color(iced_core::Color {
                            a: 0.25,
                            ..p.primary
                        }))
                    } else {
                        Option::None
                    },
                    border: iced_core::Border {
                        color: iced_core::Color { a: 0.15, ..p.text },
                        width: 0.5,
                        radius: 0.0.into(),
                    },
                    ..Default::default()
                }
            })
            .into();
        // Wrap in mouse_area for click-to-select.
        // Container constrains the mouse_area to not expand.
        widget::container(
            widget::MouseArea::<'_, Message, GraphixTheme, Renderer>::new(styled)
                .on_press(Message::CellClick(row_idx, col_for_msg)),
        )
        .width(iced_core::Length::Shrink)
        .height(iced_core::Length::Shrink)
        .into()
    }

    /// Compute the y-axis bounds for every sparkline column, by row
    /// union, so cells share a scale. Returns one entry per
    /// `Sparkline`-typed column. The column type's `min`/`max` clamp
    /// either end when set; otherwise the bound comes from the union
    /// of every row's recorded points.
    fn compute_sparkline_bounds(&self) -> FxHashMap<ArcStr, SparkBounds> {
        let mut out = FxHashMap::default();
        // Collect column-wide value ranges by walking the sparkline
        // history in one lock.
        let mut ranges: FxHashMap<ArcStr, (f64, f64)> = FxHashMap::default();
        {
            let inner = self.cells.inner.lock();
            for ((_row, col), history) in inner.sparklines.iter() {
                let entry = ranges.entry(col.clone()).or_insert((f64::MAX, f64::MIN));
                for (_t, v) in history.iter() {
                    if *v < entry.0 {
                        entry.0 = *v;
                    }
                    if *v > entry.1 {
                        entry.1 = *v;
                    }
                }
            }
        }
        for (col_name, state) in self.columns.iter() {
            let (fmin, fmax) = match &state.spec.typ {
                ColumnType::Sparkline { min, max, .. } => (min, max),
                _ => continue,
            };
            // Auto-scale fallback: use the column's union range if
            // we've seen any points; otherwise an arbitrary unit
            // window the canvas's degenerate-range guard widens.
            let (auto_min, auto_max) = ranges
                .get(col_name)
                .copied()
                .filter(|&(lo, hi)| lo <= hi)
                .unwrap_or((0.0, 1.0));
            out.insert(
                col_name.clone(),
                SparkBounds {
                    min: fmin.unwrap_or(auto_min),
                    max: fmax.unwrap_or(auto_max),
                },
            );
        }
        out
    }

    fn render_cell<'a>(
        &'a self,
        col_name: &ArcStr,
        col_type: &ColumnType,
        text: ArcStr,
        row_idx: usize,
        w: f32,
        row_h: f32,
        _can_select: bool,
        spark_bounds: Option<SparkBounds>,
    ) -> IcedElement<'a> {
        let cell_path = self.row_paths.get(row_idx).map(|p| p.append(col_name));
        let callback_id = self
            .columns
            .get(col_name.as_str())
            .and_then(|c| c.callback.as_ref())
            .map(|c| c.id());
        let is_selected = self
            .row_paths
            .get(row_idx)
            .map(|row_path| {
                self.selection.iter().any(|sel| {
                    cell_path_matches(sel, row_path.as_ref() as &str, col_name)
                })
            })
            .unwrap_or(false);
        let inner: IcedElement<'a> = match col_type {
            ColumnType::Text => {
                let is_editing = self
                    .editing
                    .as_ref()
                    .and_then(|(p, c)| self.row_paths.get(row_idx).map(|rp| (p, c, rp)))
                    .map(|(p, c, rp)| p == rp && c == col_name)
                    .unwrap_or(false);
                if is_editing {
                    widget::TextInput::<'_, Message, GraphixTheme, Renderer>::new(
                        "",
                        self.edit_buffer.as_str(),
                    )
                    .on_input(|s: String| Message::CellEditInput(s.into()))
                    .on_submit(Message::CellEditSubmit)
                    .size(13)
                    .padding(2)
                    .into()
                } else if is_selected && callback_id.is_some() {
                    // Selected editable cell: click again to edit
                    let col_for_msg = col_name.clone();
                    widget::Button::<'_, Message, GraphixTheme, Renderer>::new(
                        widget::text(truncate_to_width(&text, w).into_owned())
                            .size(13)
                            .wrapping(iced_core::text::Wrapping::None),
                    )
                    .on_press(Message::CellEdit(row_idx, col_for_msg))
                    .into()
                } else {
                    widget::text(truncate_to_width(&text, w).into_owned())
                        .size(13)
                        .wrapping(iced_core::text::Wrapping::None)
                        .into()
                }
            }
            ColumnType::Toggle => {
                let checked = text.as_str() == "true" || text.as_str() == "1";
                let mut tog = widget::Toggler::new(checked);
                if let (Some(cid), Some(cp)) = (callback_id, &cell_path) {
                    let path_val = Value::String(ArcStr::from(&**cp));
                    tog = tog.on_toggle(move |new_val| {
                        Message::Call(
                            cid,
                            ValArray::from_iter([path_val.clone(), Value::Bool(new_val)]),
                        )
                    });
                }
                tog.into()
            }
            ColumnType::Combo { choices } => {
                let options: Vec<String> =
                    choices.iter().map(|(_, label)| label.to_string()).collect();
                let selected = choices
                    .iter()
                    .find(|(id, _)| id.as_str() == text.as_str())
                    .map(|(_, label)| label.to_string());
                widget::PickList::<
                    '_,
                    String,
                    Vec<String>,
                    String,
                    Message,
                    GraphixTheme,
                    Renderer,
                >::new(options, selected, {
                    let id_map_owned: FxHashMap<String, ArcStr> = choices
                        .iter()
                        .map(|(id, label)| (label.to_string(), id.clone()))
                        .collect();
                    let cell_path = cell_path.clone();
                    move |label: String| {
                        if let (Some(cid), Some(cp)) = (callback_id, &cell_path) {
                            let id = id_map_owned
                                .get(&label)
                                .cloned()
                                .unwrap_or_else(|| ArcStr::from(label));
                            Message::Call(
                                cid,
                                ValArray::from_iter([
                                    Value::String(ArcStr::from(&**cp)),
                                    Value::String(id),
                                ]),
                            )
                        } else {
                            Message::Nop
                        }
                    }
                })
                .text_size(13)
                .into()
            }
            ColumnType::Spin { min, max, increment } => {
                let val: f64 = text.parse().unwrap_or(*min);
                if let (Some(cid), Some(cp)) = (callback_id, &cell_path) {
                    let path_val = Value::String(ArcStr::from(&**cp));
                    let dec_val = (val - increment).max(*min);
                    let inc_val = (val + increment).min(*max);
                    let path_dec = path_val.clone();
                    let path_inc = path_val;
                    let cid_dec = cid;
                    let cid_inc = cid;
                    let minus: IcedElement<'_> =
                        widget::Button::<'_, Message, GraphixTheme, Renderer>::new(
                            widget::text("−").size(11),
                        )
                        .padding(iced_core::Padding::from([0, 4]))
                        .on_press(Message::Call(
                            cid_dec,
                            ValArray::from_iter([path_dec, Value::F64(dec_val)]),
                        ))
                        .into();
                    let plus: IcedElement<'_> =
                        widget::Button::<'_, Message, GraphixTheme, Renderer>::new(
                            widget::text("+").size(11),
                        )
                        .padding(iced_core::Padding::from([0, 4]))
                        .on_press(Message::Call(
                            cid_inc,
                            ValArray::from_iter([path_inc, Value::F64(inc_val)]),
                        ))
                        .into();
                    let label: IcedElement<'_> =
                        widget::text(text.to_string()).size(13).into();
                    Row::new()
                        .push(minus)
                        .push(label)
                        .push(plus)
                        .spacing(2)
                        .align_y(iced_core::Alignment::Center)
                        .into()
                } else {
                    widget::text(text.to_string()).size(13).into()
                }
            }
            ColumnType::Progress => {
                let val: f32 = text.parse().unwrap_or(0.0);
                widget::ProgressBar::<'_, GraphixTheme>::new(
                    0.0..=1.0,
                    val.clamp(0.0, 1.0),
                )
                .girth(16)
                .into()
            }
            ColumnType::Button => {
                let mut btn = widget::Button::<'_, Message, GraphixTheme, Renderer>::new(
                    widget::text(text.to_string()).size(13),
                );
                if let (Some(cid), Some(cp)) = (callback_id, &cell_path) {
                    let path_val = Value::String(ArcStr::from(&**cp));
                    let val = self
                        .row_paths
                        .get(row_idx)
                        .and_then(|rp| self.raw_value_for(rp, col_name))
                        .unwrap_or(Value::Null);
                    btn = btn.on_press(Message::Call(
                        cid,
                        ValArray::from_iter([path_val, val]),
                    ));
                }
                btn.into()
            }
            ColumnType::Sparkline { .. } => {
                let inner = self.cells.inner.lock();
                let key = self
                    .row_paths
                    .get(row_idx)
                    .map(|row_path| (row_path.clone(), col_name.clone()));
                let history = key.as_ref().and_then(|k| inner.sparklines.get(k));
                let points: Vec<(f64, f64)> = match history {
                    Some(h) if h.len() >= 2 => {
                        let first_t = h.front().unwrap().0;
                        h.iter()
                            .map(|(t, v)| (t.duration_since(first_t).as_secs_f64(), *v))
                            .collect()
                    }
                    _ => vec![],
                };
                drop(inner);
                if points.is_empty() {
                    widget::text(text.to_string()).size(13).into()
                } else {
                    let bounds =
                        spark_bounds.unwrap_or(SparkBounds { min: 0.0, max: 1.0 });
                    let sparkline = SparklineCanvas { points, bounds };
                    widget::Canvas::new(sparkline)
                        .width(iced_core::Length::Fill)
                        .height(20.0)
                        .into()
                }
            }
        };
        self.wrap_cell(inner, col_name, row_idx, w, row_h, is_selected)
    }
}
