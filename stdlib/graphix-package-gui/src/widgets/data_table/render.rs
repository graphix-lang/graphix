//! Rendering for `DataTableW`: `render_with_size`, per-cell rendering,
//! wrapper layers, and the sparkline canvas program.

use super::{
    DEFAULT_MAX_COL_WIDTH, DataTableW, DisplayMode, HEADER_HEIGHT, IcedElement,
    MIN_COL_WIDTH, Message, RESIZE_HANDLE_WIDTH, ROW_NAME_HEADER_LABEL, ROW_NAME_KEY,
    Renderer, TableMsg, VALUE_COL_KEY, VALUE_HEADER_LABEL,
    types::{
        Choice, ColumnType, SortDirection, ViewportMetrics, age_sparkline, cell_path,
        col_header_width, col_min_width, row_basename, sparkline_cutoff,
        truncate_to_width,
    },
};
use crate::{
    theme::GraphixTheme,
    widgets::{TableKeyAction, iced_keyboard_area::KeyboardArea},
};
use ahash::AHashMap;
use arcstr::ArcStr;
use compact_str::{CompactString, format_compact};
use graphix_rt::GXExt;
use iced_core::keyboard::{Key, key::Named};
use iced_widget as widget;
use netidx::{protocol::valarray::ValArray, publisher::Value};
use poolshark::local::LPooled;
use std::{
    collections::VecDeque,
    time::{Duration, Instant},
};

type Col<'a> = widget::Column<'a, Message, GraphixTheme, Renderer>;
type Row<'a> = widget::Row<'a, Message, GraphixTheme, Renderer>;

/// Unicode subscript digit for 1–9, `""` outside that range.
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

/// Resolved y-axis bounds for a sparkline cell.
#[derive(Clone, Copy)]
pub(super) struct SparkBounds {
    min: f64,
    max: f64,
}

/// Canvas program that draws a sparkline polyline.
struct SparklineCanvas {
    /// Every sample had aged out when it was built: its picture is final.
    still: bool,
    history_seconds: f64,
    points: LPooled<Vec<(f64, f64)>>,
    bounds: SparkBounds,
}

impl SparklineCanvas {
    fn new(
        history: &VecDeque<(Instant, f64)>,
        now: Instant,
        history_seconds: f64,
        bounds: SparkBounds,
    ) -> Self {
        let mut points: LPooled<Vec<_>> = history
            .iter()
            .map(|&(t, v)| {
                let age = now.saturating_duration_since(t).as_secs_f64();
                ((1.0 - age / history_seconds).clamp(0.0, 1.0), v)
            })
            .collect();
        if let Some(&(_, v)) = points.last() {
            points.push((1.0, v));
        }
        Self {
            points,
            bounds,
            history_seconds,
            still: history.back().is_none_or(|&(t, _)| {
                sparkline_cutoff(now, history_seconds).is_some_and(|cutoff| t <= cutoff)
            }),
        }
    }

    fn path(&self, size: iced_core::Size) -> widget::canvas::Path {
        use widget::canvas::Path;
        let (mut min_v, mut max_v) = (self.bounds.min, self.bounds.max);
        if (max_v - min_v).abs() < 1e-12 {
            min_v -= 1.0;
            max_v += 1.0;
        }
        let v_range = max_v - min_v;
        let w = size.width;
        let h = size.height;
        Path::new(|builder| {
            let mut first = true;
            for &(t, v) in self.points.iter() {
                if !t.is_finite() || !v.is_finite() {
                    first = true;
                    continue;
                }
                let x = t as f32 * w;
                let y = h - ((v - min_v) / v_range) as f32 * h;
                if !x.is_finite() || !y.is_finite() {
                    first = true;
                    continue;
                }
                if first {
                    builder.move_to(iced_core::Point::new(x, y));
                    first = false;
                } else {
                    builder.line_to(iced_core::Point::new(x, y));
                }
            }
        })
    }
}

impl<Message> widget::canvas::Program<Message, GraphixTheme, Renderer>
    for SparklineCanvas
{
    type State = ();

    fn update(
        &self,
        _state: &mut Self::State,
        event: &iced_core::Event,
        bounds: iced_core::Rectangle,
        _cursor: iced_core::mouse::Cursor,
    ) -> Option<widget::Action<Message>> {
        let iced_core::Event::Window(iced_core::window::Event::RedrawRequested(now)) =
            event
        else {
            return None;
        };
        if self.still || bounds.width <= 0.0 || !bounds.width.is_finite() {
            return None;
        }
        let seconds = (self.history_seconds / bounds.width as f64).max(1.0 / 60.0);
        let period = Duration::try_from_secs_f64(seconds).ok()?;
        Some(widget::Action::request_redraw_at(now.checked_add(period)?))
    }

    fn draw(
        &self,
        _state: &Self::State,
        renderer: &Renderer,
        _theme: &GraphixTheme,
        bounds: iced_core::Rectangle,
        _cursor: iced_core::mouse::Cursor,
    ) -> Vec<widget::canvas::Geometry<Renderer>> {
        use widget::canvas::{Frame, Stroke};
        let mut frame = Frame::new(renderer, bounds.size());
        if self.points.len() < 2 {
            return vec![frame.into_geometry()];
        }
        let path = self.path(bounds.size());
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
    /// A message addressed to this table.
    fn msg(&self, m: TableMsg) -> Message {
        Message::Table(self.id, m)
    }

    /// Header sort-indicator suffix per sorted column: an arrow, plus a
    /// subscript priority digit when there is more than one sort column.
    pub(super) fn build_sort_indicators(
        &self,
    ) -> LPooled<AHashMap<ArcStr, CompactString>> {
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

    pub(super) fn visible_grid(&self) -> LPooled<Vec<LPooled<Vec<ArcStr>>>> {
        let (vis_row_start, vis_row_end) = self.display_row_range();
        let (vis_col_start, vis_col_end) = self.display_col_range();
        {
            let mut inner = self.cells.inner.lock();
            (vis_row_start..vis_row_end)
                .map(|i| match self.row_paths.get(i) {
                    None => LPooled::take(),
                    Some(row_path) => match self.mode {
                        DisplayMode::Table => self
                            .columns
                            .get_range(vis_col_start..vis_col_end)
                            .unwrap()
                            .iter()
                            .map(|(cn, _)| self.cell_text(&mut inner, row_path, cn))
                            .collect(),
                        DisplayMode::Value => {
                            let mut res: LPooled<Vec<ArcStr>> = LPooled::take();
                            res.push(self.cell_text(
                                &mut inner,
                                row_path,
                                &VALUE_COL_KEY,
                            ));
                            res
                        }
                    },
                })
                .collect()
        }
    }

    pub(super) fn render_with_size(&self, size: iced_core::Size) -> IcedElement<'_> {
        {
            let mut next = ViewportMetrics::new(size, self.row_height());
            let mut metrics = self.viewport_metrics.lock();
            next.needs_subscription_reconcile = metrics.needs_subscription_reconcile
                || metrics.rows_in_view != next.rows_in_view;
            *metrics = next;
        }
        let num_rows = self.row_paths.len();
        let show_row_name = self.show_row_name();
        let on_header_click = self.on_header_click.id();
        let bold = iced_core::Font {
            weight: iced_core::font::Weight::Bold,
            ..iced_core::Font::DEFAULT
        };
        let (vis_row_start, vis_row_end) = self.display_row_range();
        let (vis_col_start, vis_col_end) = self.display_col_range();
        if (vis_col_start, vis_col_end) != self.subscription_columns {
            self.viewport_metrics.lock().needs_subscription_reconcile = true;
        }
        let grid_snapshot = self.visible_grid();
        let name_col_offset = if show_row_name { 1 } else { 0 };
        let sort_indicators: LPooled<AHashMap<ArcStr, CompactString>> =
            self.build_sort_indicators();
        let mut col_meta: LPooled<Vec<(ArcStr, f32)>> = LPooled::take();
        if show_row_name {
            let w = match self.explicit_col_width(&ROW_NAME_KEY) {
                Some(w) => w,
                None => {
                    let max_w = DEFAULT_MAX_COL_WIDTH;
                    let mut w = col_min_width(ROW_NAME_HEADER_LABEL, max_w);
                    for p in &self.row_paths[vis_row_start..vis_row_end] {
                        w = w.max(col_min_width(row_basename(p), max_w));
                    }
                    w
                }
            };
            col_meta.push((ROW_NAME_KEY, w));
        }
        match self.mode {
            DisplayMode::Table => {
                for i in vis_col_start..vis_col_end {
                    let (name, state) = match self.columns.get_index(i) {
                        Some(p) => p,
                        None => break,
                    };
                    let w = match self.explicit_col_width(name) {
                        Some(w) => w,
                        None => {
                            let max_w = DEFAULT_MAX_COL_WIDTH;
                            let display = state
                                .spec
                                .display_name
                                .as_deref()
                                .unwrap_or(name.as_str());
                            let header: CompactString = match sort_indicators.get(name) {
                                Some(ind) => format_compact!("{display}{ind}"),
                                None => display.into(),
                            };
                            let mut w =
                                col_header_width(&header).max(MIN_COL_WIDTH).min(max_w);
                            for row in &*grid_snapshot {
                                if let Some(text) = row.get(i - vis_col_start) {
                                    w = w.max(col_min_width(text, max_w));
                                }
                            }
                            w
                        }
                    };
                    col_meta.push((name.clone(), w));
                }
            }
            DisplayMode::Value => {
                let w = match self.explicit_col_width(&VALUE_COL_KEY) {
                    Some(w) => w,
                    None => {
                        let max_w = DEFAULT_MAX_COL_WIDTH;
                        let mut w = col_min_width(VALUE_HEADER_LABEL, max_w);
                        for row in &*grid_snapshot {
                            if let Some(v) = row.first() {
                                w = w.max(col_min_width(v, max_w));
                            }
                        }
                        w
                    }
                };
                col_meta.push((VALUE_COL_KEY, w));
            }
        }
        // Accumulated across frames: `virtual_content_width` needs the
        // widths of columns that have scrolled out of view.
        {
            let mut cache = self.cached_col_widths.lock();
            for (name, w) in &*col_meta {
                cache.insert(name.clone(), *w);
            }
        }
        let mut header_row = Row::new().spacing(0);
        for (ci, (name, w)) in col_meta.iter().enumerate() {
            let is_data_col = self.mode == DisplayMode::Table && ci >= name_col_offset;
            let entry = self.columns.get(name);
            let base = match entry {
                _ if name == &ROW_NAME_KEY => ROW_NAME_HEADER_LABEL,
                _ if name == &VALUE_COL_KEY => VALUE_HEADER_LABEL,
                Some(c) => c.spec.display_name.as_deref().unwrap_or(name),
                None => name,
            };
            let header_text = match sort_indicators.get(name) {
                Some(ind) => format_compact!("{base}{ind}"),
                None => base.into(),
            };
            let is_fixed = entry
                .map(|c| c.ref_width.is_some() && c.on_resize.is_none())
                .unwrap_or(false);
            let plain_text: IcedElement<'_> =
                widget::text(header_text.as_str().to_owned())
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
            let inner: IcedElement<'_> = if !is_fixed {
                let handle: IcedElement<'_> =
                    widget::MouseArea::<'_, Message, GraphixTheme, Renderer>::new(
                        widget::container(widget::Space::new())
                            .width(RESIZE_HANDLE_WIDTH)
                            .height(iced_core::Length::Fill),
                    )
                    .interaction(iced_core::mouse::Interaction::ResizingColumn)
                    .on_press(self.msg(TableMsg::ColumnResizeStart(name.clone())))
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
                .height(HEADER_HEIGHT)
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
        let row_h = self.row_height();
        let now = Instant::now();
        let spark_bounds_by_col = self.compute_sparkline_bounds(now);
        let mut body = Col::new()
            .spacing(0)
            .width(iced_core::Length::Shrink)
            .height(iced_core::Length::Shrink);
        for (vi, row_idx) in (vis_row_start..vis_row_end).enumerate() {
            let mut row_w = Row::new().spacing(0);
            for (ci, (col_name, w)) in col_meta.iter().enumerate() {
                let is_name_col = ci == 0 && show_row_name;
                let cell_el: IcedElement<'_> = if is_name_col {
                    let name =
                        self.row_paths.get(row_idx).map(row_basename).unwrap_or("");
                    let is_sel = self.is_selected(row_idx, &ROW_NAME_KEY);
                    let inner: IcedElement<'_> =
                        widget::text(truncate_to_width(name, *w))
                            .size(13)
                            .wrapping(iced_core::text::Wrapping::None)
                            .into();
                    self.wrap_cell(inner, col_name, row_idx, *w, row_h, is_sel)
                } else {
                    let data_col = vis_col_start + ci - name_col_offset;
                    let text = grid_snapshot
                        .get(vi)
                        .and_then(|row| row.get(data_col - vis_col_start))
                        .cloned()
                        .unwrap_or_default();
                    let col_type = if self.mode == DisplayMode::Table {
                        match self.columns.get_index(data_col) {
                            Some((name, _)) => self.col_type_for(name),
                            None => &ColumnType::Text,
                        }
                    } else {
                        &ColumnType::Text
                    };
                    let sb = spark_bounds_by_col.get(col_name).copied();
                    self.render_cell(
                        col_name, col_type, text, row_idx, *w, row_h, sb, now,
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
        // iced shows a scrollbar iff content exceeds bounds, so the
        // virtual size must cover the entire content, not the visible
        // slice.
        let virtual_width = self.virtual_content_width();
        let virtual_height = num_rows as f32 * row_h + HEADER_HEIGHT;
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
            .id(self.overlay_id.clone())
            .on_scroll(|vp| {
                let abs = vp.absolute_offset();
                let bounds = vp.bounds();
                self.msg(TableMsg::Scroll(abs.x, abs.y, bounds.width, bounds.height))
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
        // Clip so the partial last row does not bleed past the table's
        // bounds; the scissor applies to draw only, not overlays.
        let clipped: IcedElement<'_> = widget::container(stacked)
            .width(iced_core::Length::Fill)
            .height(iced_core::Length::Fill)
            .clip(true)
            .into();
        self.wrap_keyboard(clipped)
    }

    /// The table's keys, and the pointer a column resize follows: every
    /// move and the release that ends it, wherever the cursor is. A press
    /// and release in one batch start and end the drag in order. While a cell is edited the keys are the editor's, but
    /// for Escape, which cancels the edit.
    fn wrap_keyboard<'a>(&'a self, content: IcedElement<'a>) -> IcedElement<'a> {
        let editing = self.editing.is_some();
        KeyboardArea::new(content)
            .keys_first()
            .on_pointer(
                |x| self.msg(TableMsg::ColumnResizeMove(x)),
                self.msg(TableMsg::ColumnResizeEnd),
            )
            .on_key_press(move |key, _, _, _| {
                let action = match key {
                    Key::Named(Named::Escape) => TableKeyAction::Escape,
                    _ if editing => return None,
                    Key::Named(Named::ArrowUp) => TableKeyAction::Up,
                    Key::Named(Named::ArrowDown) => TableKeyAction::Down,
                    Key::Named(Named::ArrowLeft) => TableKeyAction::Left,
                    Key::Named(Named::ArrowRight) => TableKeyAction::Right,
                    Key::Named(Named::Enter) => TableKeyAction::Enter,
                    Key::Named(Named::Space) => TableKeyAction::Space,
                    _ => return None,
                };
                Some(self.msg(TableMsg::Key(action)))
            })
            .into()
    }

    /// Whether the cell at `(row_idx, col)` is selected.
    fn is_selected(&self, row_idx: usize, col: &ArcStr) -> bool {
        self.row_paths.get(row_idx).is_some_and(|rp| self.selection.contains(rp, col))
    }

    /// Wrap a cell's content in a bordered, click-to-select container.
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
        widget::container(
            widget::MouseArea::<'_, Message, GraphixTheme, Renderer>::new(styled)
                .on_press(self.msg(TableMsg::CellClick(row_idx, col_for_msg))),
        )
        .width(iced_core::Length::Shrink)
        .height(iced_core::Length::Shrink)
        .into()
    }

    /// Y-axis bounds per sparkline column: the column type's `min`/`max`
    /// where set, else the union of every row's points in the window.
    /// Points older than the window are dropped here, so a row no update
    /// reaches still ages.
    fn compute_sparkline_bounds(
        &self,
        now: Instant,
    ) -> LPooled<AHashMap<ArcStr, SparkBounds>> {
        let mut out: LPooled<AHashMap<ArcStr, SparkBounds>> = LPooled::take();
        let mut ranges: LPooled<AHashMap<ArcStr, (f64, f64)>> = LPooled::take();
        {
            let mut inner = self.cells.inner.lock();
            for ((_row, col), history) in inner.sparklines.iter_mut() {
                if let Some(ColumnType::Sparkline { history_seconds, .. }) =
                    self.columns.get(col).map(|c| &c.spec.typ)
                {
                    age_sparkline(history, now, *history_seconds);
                }
                let entry = ranges.entry(col.clone()).or_insert((f64::MAX, f64::MIN));
                for (_t, v) in history.iter().filter(|(_, v)| v.is_finite()) {
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
        col_type: &'a ColumnType,
        text: ArcStr,
        row_idx: usize,
        w: f32,
        row_h: f32,
        spark_bounds: Option<SparkBounds>,
        now: Instant,
    ) -> IcedElement<'a> {
        let callback_id = self
            .columns
            .get(col_name.as_str())
            .and_then(|c| c.callback.as_ref())
            .map(|c| c.id());
        let cell_path = callback_id
            .and_then(|_| self.row_paths.get(row_idx).map(|p| cell_path(p, col_name)));
        let is_selected = self.is_selected(row_idx, col_name);
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
                    .id(self.editor_id.clone())
                    .on_input(|s: String| self.msg(TableMsg::CellEditInput(s.into())))
                    .on_submit(self.msg(TableMsg::CellEditSubmit))
                    .size(13)
                    .padding(2)
                    .into()
                } else if is_selected && callback_id.is_some() {
                    let col_for_msg = col_name.clone();
                    widget::Button::<'_, Message, GraphixTheme, Renderer>::new(
                        widget::text(truncate_to_width(&text, w).into_owned())
                            .size(13)
                            .wrapping(iced_core::text::Wrapping::None),
                    )
                    .on_press(self.msg(TableMsg::CellEdit(row_idx, col_for_msg)))
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
                    let path_val = Value::String(cp.clone());
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
                let selected = choices.iter().find(|c| c.id.as_str() == text.as_str());
                let cell_path = cell_path.clone();
                widget::PickList::<
                    '_,
                    Choice,
                    &[Choice],
                    &Choice,
                    Message,
                    GraphixTheme,
                    Renderer,
                >::new(&choices[..], selected, move |choice: Choice| {
                    match (callback_id, &cell_path) {
                        (Some(cid), Some(cp)) => Message::Call(
                            cid,
                            ValArray::from_iter([
                                Value::String(cp.clone()),
                                Value::String(choice.id),
                            ]),
                        ),
                        _ => Message::Nop,
                    }
                })
                .text_size(13)
                .into()
            }
            ColumnType::Spin { min, max, increment } => {
                let val: f64 = text.parse().unwrap_or(*min);
                if let (Some(cid), Some(cp)) = (callback_id, &cell_path) {
                    let path_val = Value::String(cp.clone());
                    let dec_val = (val - increment).max(*min).min(*max);
                    let inc_val = (val + increment).max(*min).min(*max);
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
                    let path_val = Value::String(cp.clone());
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
            ColumnType::Sparkline { history_seconds, .. } => {
                let inner = self.cells.inner.lock();
                let key = self
                    .row_paths
                    .get(row_idx)
                    .map(|row_path| (row_path.clone(), col_name.clone()));
                let history = key.as_ref().and_then(|k| inner.sparklines.get(k));
                // XCR claude for claude: [bug/low] The sparkline's x-axis is the cell's
                // own data span (first point to last change), not the rolling window:
                // SparklineCanvas::path maps [min_t, max_t] onto the whole width. A
                // value that changed 10 minutes ago is drawn as if it changed now, and
                // in one column a row with 10 s of history and one with 900 s are both
                // stretched full width, though the y-axis is shared so cells can be
                // compared. Map t over [now - history_seconds, now] and draw the last
                // value as a flat segment out to now. Found by reading; not run.
                // (gui-datatable-03)
                // 2026-10-27 claude: Eric ruled (2026-10-27, cr-questions round 2,
                // q50, A sparkline's x axis): yes, rolling window (the recommendation:
                // the rolling window.)
                // 2026-10-28 codex: Sparkline points now use the rolling
                // history_seconds window and one timestamp per table frame; the final
                // value extends flat to now, including a single unchanged sample.
                // Aging keeps the last value at/before the window start. Scratch
                // points use LPooled. Pins sparkline_path_uses_the_rolling_window and
                // sparkline_aging_keeps_the_value_at_the_window_start both failed
                // before the fix and pass after;
                // sparkline_timestamps_share_a_window_and_extend_to_now additionally
                // checks distinct histories, timestamp normalization, a lone old
                // sample and the terminal segment. cargo test -p graphix-package-gui
                // --offline sparkline_tests: 4 passed. Updated the book and the
                // existing integration expectation for retained history. Full GUI
                // suite follows; GPU rendering and netidx-dependent table integration
                // cannot run in this sandbox.
                // 2026-10-28 codex: Nonempty visible SparklineCanvas programs now
                // request iced RedrawRequest::At on each redraw, at history_seconds /
                // pixel width (capped at 60 Hz). The existing window deadline wakes the
                // normal frame path, which rebuilds and lays out the table with a fresh
                // timestamp even without data events. Empty and zero-width canvases
                // request no timer. quiet_sparklines_request_pixel_timed_redraws failed
                // before the update method was added, then passed: repeated idle
                // deadlines scale with both history duration and width, successive
                // rebuilt samples move one pixel, and the held final value still
                // extends to now. cargo test -p graphix-package-gui --offline --lib
                // sparkline_tests -- --nocapture: 5 passed. Updated the book. CPU
                // scheduling and geometry are verified; an actual window/GPU timer
                // rendering remains for coordinator review. Full GUI lib gate follows
                // after the test-strengthening work.
                // 2026-10-28 codex: Second review: restored the unmerged f128727 redraw
                // scheduler, then stopped it when the newest sample is at or before the
                // current history cutoff. SparklineCanvas stores the newest sample's
                // timestamp and uses the same sparkline_cutoff helper as history aging
                // on every RedrawRequested event, so an existing canvas stops even
                // without being rebuilt. Pins
                // expired_sparklines_stop_requesting_redraws and
                // sparkline_redraw_scheduling_cost both failed before this fix and pass
                // afterward. They cover exact expiration, fractional-second/nanosecond
                // rounding, later events on the same canvas, a retained flat line, and
                // resumption after a new sample. CPU measurement in the dev profile,
                // isolated after the full suites finished: 10,000 canvases, 100 updates
                // each, 10-second history and 100-pixel width. At sample age 10s, one
                // million update calls requested one million redraws before (8.412581
                // ms) and zero after (8.755766 ms). At age 0s, both requested one
                // million; before 8.141288 ms, after 11.022088 ms. These measure
                // scheduling calls, not actual window/GPU redraw cost. cargo test -p
                // graphix-package-gui --offline --lib sparkline_tests -- --nocapture:
                // before 5 passed/2 failed, after 7 passed. The isolated
                // sparkline_redraw_scheduling_cost filter failed before and passed
                // after. Final cargo test -p graphix-package-gui --offline --lib: 92
                // passed, 258 failed, 1 ignored; 78 failures under
                // test::data_table_test:: are resolver/socket failures and 180 cannot
                // initialize a GPU adapter. cargo test -p graphix-tests --offline
                // --lib: 11015 passed, 447 failed, 3 ignored. Network-dependent failure
                // prefixes: lang::async_restart::seq_network,
                // lang::interfaces::abstract_type_dynamic_module, lang::modules::,
                // lang::traits::, lib_tests::core::queue, lib_tests::http::,
                // lib_tests::json::json_stream_, lib_tests::net::,
                // lib_tests::pack::pack_stream_tcp, lib_tests::tcp::, lib_tests::tls::,
                // lib_tests::toml::toml_stream_tcp and lib_tests::typecheck::. Book
                // updated/rebuilt; cargo fmt --all completed. Actual window timers, GPU
                // rendering and socket-dependent integration remain unverified here.
                // 2026-10-28 claude: Review (2026-10-28) found expiry judged at the
                // RedrawRequested instant, after the canvas's picture was built, so a
                // frame drawn just before expiry could stay on screen, and an update
                // cost two Duration conversions. Fixed on merge: SparklineCanvas::new
                // decides once whether its picture is final (still), from its own now;
                // expired_sparklines_stop_requesting_redraws pins the redraw after a
                // pre-expiry picture.
                let bounds = spark_bounds.unwrap_or(SparkBounds { min: 0.0, max: 1.0 });
                let sparkline = history
                    .filter(|h| !h.is_empty())
                    .map(|h| SparklineCanvas::new(h, now, *history_seconds, bounds));
                drop(inner);
                if let Some(sparkline) = sparkline {
                    widget::Canvas::new(sparkline)
                        .width(iced_core::Length::Fill)
                        .height(20.0)
                        .into()
                } else {
                    widget::text(text.to_string()).size(13).into()
                }
            }
        };
        self.wrap_cell(inner, col_name, row_idx, w, row_h, is_selected)
    }
}

#[cfg(test)]
mod sparkline_tests {
    use super::*;
    use widget::canvas::path::lyon_path::Event;

    fn redraw_at(canvas: &SparklineCanvas, now: Instant) -> Option<widget::Action<()>> {
        use iced_core::{Event, Rectangle, Size, mouse::Cursor, window};
        use widget::canvas::Program;
        <SparklineCanvas as Program<(), GraphixTheme, Renderer>>::update(
            canvas,
            &mut (),
            &Event::Window(window::Event::RedrawRequested(now)),
            Rectangle::with_size(Size::new(100.0, 20.0)),
            Cursor::Unavailable,
        )
    }

    #[test]
    fn expired_sparklines_stop_requesting_redraws() {
        let now = Instant::now();
        let bounds = SparkBounds { min: 0.0, max: 1.0 };
        let mut history = [(now - Duration::from_secs(2), 0.25), (now, 0.75)].into();
        for seconds in [10.0, 0.1234567895, 1.4e-9] {
            let cutoff = now + Duration::from_secs_f64(seconds);
            let built = |at| SparklineCanvas::new(&history, at, seconds, bounds);
            let before = built(cutoff - Duration::from_nanos(1));
            assert!(redraw_at(&before, cutoff - Duration::from_nanos(1)).is_some());
            // a picture drawn before the expiry is redrawn once more, after it
            assert!(redraw_at(&before, cutoff + Duration::from_millis(1)).is_some());
            assert!(redraw_at(&built(cutoff), cutoff).is_none());
            let late = cutoff + Duration::from_secs(1);
            assert!(redraw_at(&built(late), late).is_none());
        }
        let mut frame_time = now + Duration::from_secs(10);
        for _ in 0..3 {
            age_sparkline(&mut history, frame_time, 10.0);
            let canvas = SparklineCanvas::new(&history, frame_time, 10.0, bounds);
            assert_eq!(&*canvas.points, &[(0.0, 0.75), (1.0, 0.75)]);
            assert!(redraw_at(&canvas, frame_time).is_none());
            frame_time += Duration::from_secs(1);
        }
        history.push_back((frame_time, 0.5));
        let canvas = SparklineCanvas::new(&history, frame_time, 10.0, bounds);
        assert!(redraw_at(&canvas, frame_time).is_some());
    }

    #[test]
    fn sparkline_redraw_scheduling_cost() {
        use std::hint::black_box;
        let now = Instant::now();
        let bounds = SparkBounds { min: 0.0, max: 1.0 };
        let history = [(now - Duration::from_secs(2), 0.25), (now, 0.75)].into();
        let mut counts = [0; 2];
        for (count, age) in counts.iter_mut().zip([0, 10]) {
            let time = now + Duration::from_secs(age);
            let canvases: LPooled<Vec<_>> = (0..10_000)
                .map(|_| SparklineCanvas::new(&history, time, 10.0, bounds))
                .collect();
            let start = Instant::now();
            for _ in 0..100 {
                for canvas in canvases.iter() {
                    *count += black_box(redraw_at(black_box(canvas), black_box(time)))
                        .is_some() as usize;
                }
            }
            eprintln!(
                "10,000 sparklines, 100 updates each, sample age {age}s: {:?}, {count} redraw requests",
                start.elapsed()
            );
        }
        assert_eq!(counts, [1_000_000, 0]);
    }

    #[test]
    fn quiet_sparklines_request_pixel_timed_redraws() {
        use iced_core::{Event as IcedEvent, Rectangle, mouse::Cursor, window};
        use std::time::Duration;
        use widget::canvas::Program;

        let now = Instant::now();
        let history = [(now - Duration::from_secs(2), 0.25), (now, 0.75)].into();
        let bounds = SparkBounds { min: 0.0, max: 1.0 };
        let canvas = SparklineCanvas::new(&history, now, 10.0, bounds);
        for (width, seconds, expected_ms) in
            [(100.0, 10.0, 100), (200.0, 10.0, 50), (100.0, 20.0, 200)]
        {
            let mut frame_time = now;
            for _ in 0..3 {
                let canvas = SparklineCanvas::new(&history, frame_time, seconds, bounds);
                let action =
                    <SparklineCanvas as Program<(), GraphixTheme, Renderer>>::update(
                        &canvas,
                        &mut (),
                        &IcedEvent::Window(window::Event::RedrawRequested(frame_time)),
                        Rectangle::with_size(iced_core::Size::new(width, 20.0)),
                        Cursor::Unavailable,
                    )
                    .expect(
                        "nonempty history must keep requesting frames on an idle table",
                    );
                let (message, redraw, status) = action.into_inner();
                assert!(message.is_none());
                assert_eq!(status, iced_core::event::Status::Ignored);
                let window::RedrawRequest::At(next) = redraw else {
                    panic!("{redraw:?}")
                };
                assert_eq!(next - frame_time, Duration::from_millis(expected_ms));
                let next_canvas = SparklineCanvas::new(&history, next, seconds, bounds);
                let dx = (canvas.points[1].0 - next_canvas.points[1].0) * width as f64;
                assert!((dx - 1.0).abs() < 1e-10);
                assert_eq!(next_canvas.points.last(), Some(&(1.0, 0.75)));
                frame_time = next;
            }
        }
        for (history, width) in [(VecDeque::new(), 100.0), (history, 0.0)] {
            let canvas = SparklineCanvas::new(&history, now, 10.0, bounds);
            assert!(
                <SparklineCanvas as Program<(), GraphixTheme, Renderer>>::update(
                    &canvas,
                    &mut (),
                    &IcedEvent::Window(window::Event::RedrawRequested(now)),
                    Rectangle::with_size(iced_core::Size::new(width, 20.0)),
                    Cursor::Unavailable,
                )
                .is_none()
            );
        }
        assert!(
            <SparklineCanvas as Program<(), GraphixTheme, Renderer>>::update(
                &canvas,
                &mut (),
                &IcedEvent::Mouse(iced_core::mouse::Event::CursorLeft),
                Rectangle::with_size(iced_core::Size::new(100.0, 20.0)),
                Cursor::Unavailable,
            )
            .is_none()
        );
    }

    #[test]
    fn sparkline_timestamps_share_a_window_and_extend_to_now() {
        use std::time::Duration;
        let now = Instant::now();
        let bounds = SparkBounds { min: 0.0, max: 1.0 };
        for ages in [[8, 6], [4, 2]] {
            let history = ages
                .into_iter()
                .map(|age| (now - Duration::from_secs(age), 1.0))
                .collect();
            let canvas = SparklineCanvas::new(&history, now, 10.0, bounds);
            for (point, age) in canvas.points.iter().zip(ages) {
                assert!((point.0 - (10 - age) as f64 / 10.0).abs() < 1e-12);
            }
            assert_eq!(canvas.points.last(), Some(&(1.0, 1.0)));
        }
        let history = [(now - Duration::from_secs(20), 0.5)].into();
        let canvas = SparklineCanvas::new(&history, now, 10.0, bounds);
        assert_eq!(&*canvas.points, &[(0.0, 0.5), (1.0, 0.5)]);
    }

    #[test]
    fn sparkline_path_uses_the_rolling_window() {
        let canvas = SparklineCanvas {
            still: true,
            history_seconds: 10.0,
            points: [(0.2, 0.0), (0.4, 1.0), (1.0, 1.0)].into_iter().collect(),
            bounds: SparkBounds { min: 0.0, max: 1.0 },
        };
        let path = canvas.path(iced_core::Size::new(100.0, 20.0));
        let lines: LPooled<Vec<_>> = path
            .raw()
            .iter()
            .filter_map(|e| match e {
                Event::Line { from, to } => Some(((from.x, from.y), (to.x, to.y))),
                _ => None,
            })
            .collect();
        assert_eq!(&*lines, &[((20.0, 20.0), (40.0, 0.0)), ((40.0, 0.0), (100.0, 0.0))]);
    }

    #[test]
    fn sparkline_aging_keeps_the_value_at_the_window_start() {
        use std::{collections::VecDeque, time::Duration};
        let now = Instant::now();
        let mut history: VecDeque<_> = [(20, 1.0), (15, 2.0), (5, 3.0)]
            .map(|(age, v)| (now - Duration::from_secs(age), v))
            .into();
        age_sparkline(&mut history, now, 10.0);
        assert_eq!(history.len(), 2);
        assert_eq!(history[0].1, 2.0);
        age_sparkline(&mut history, now + Duration::from_secs(20), 10.0);
        assert_eq!(history.len(), 1);
        assert_eq!(history[0].1, 3.0);
    }

    #[test]
    fn sparkline_paths_skip_nonfinite_samples() {
        let canvas = SparklineCanvas {
            still: true,
            history_seconds: 10.0,
            points: vec![
                (f64::NAN, 0.0),
                (0.0, 0.0),
                (1.0, 1.0),
                (2.0, f64::NAN),
                (3.0, 0.0),
                (4.0, 1.0),
                (5.0, f64::INFINITY),
            ]
            .into_iter()
            .collect(),
            bounds: SparkBounds { min: 0.0, max: 1.0 },
        };
        let path = canvas.path(iced_core::Size::new(100.0, 20.0));
        let mut lines = 0;
        for event in path.raw().iter() {
            match event {
                Event::Begin { at } => assert!(at.x.is_finite() && at.y.is_finite()),
                Event::Line { from, to } => {
                    assert!(from.x.is_finite() && from.y.is_finite());
                    assert!(to.x.is_finite() && to.y.is_finite());
                    lines += 1;
                }
                Event::End { .. } => (),
                _ => panic!("unexpected sparkline segment"),
            }
        }
        assert_eq!(lines, 2);
    }
}
