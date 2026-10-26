use super::{
    ChartW,
    clip::{View, clip_area, clip_polyline},
    dataset::{ChartMode, DatasetEntry, XYKind, first_pie, pie_slices, pie_start_angle},
    interact::{ChartState, PlotInfo, ViewState, draw_tooltip},
    plotters_backend::{IcedBackend, estimate_text},
    ranges::*,
    types::*,
};
use crate::widgets::Renderer;
use arcstr::ArcStr;
use graphix_rt::GXExt;
use iced_core::mouse;
use iced_widget::canvas as iced_canvas;
use log::error;
use plotters::{
    chart::{ChartBuilder, ChartContext, SeriesAnno},
    coord::Shift,
    coord::{
        CoordTranslate,
        cartesian::Cartesian2d,
        ranged1d::{
            AsRangedCoord, DefaultFormatting, DiscreteRanged, KeyPointHint, Ranged,
            SegmentValue, ValueFormatter,
        },
        types::{RangedCoordf64, RangedCoordusize},
    },
    drawing::{DrawingArea, DrawingAreaErrorKind},
    element::{
        CandleStick, Circle, Drawable, IntoDynElement, PathElement, Pie, PointCollection,
        Polygon, Rectangle,
    },
    prelude::{
        AreaSeries, DashedLineSeries, DrawingBackend, Histogram, IntoDrawingArea,
        IntoSegmentedCoord, LineSeries, SeriesLabelPosition,
    },
    style::{
        BLACK, Color as PlotColor, IntoFont, RGBAColor, RGBColor, ShapeStyle, TextStyle,
        WHITE,
    },
};
use plotters_backend::{BackendCoord, DrawingErrorKind};
use std::ops::Range;

const PALETTE: [RGBColor; 8] = [
    RGBColor(31, 119, 180),
    RGBColor(255, 127, 14),
    RGBColor(44, 160, 44),
    RGBColor(214, 39, 40),
    RGBColor(148, 103, 189),
    RGBColor(140, 86, 75),
    RGBColor(227, 119, 194),
    RGBColor(127, 127, 127),
];

const DEFAULT_GAIN: RGBColor = RGBColor(44, 160, 44);
const DEFAULT_LOSS: RGBColor = RGBColor(214, 39, 40);

/// The most ticks or grid lines an axis is asked for.
const MAX_TICKS: i64 = 1000;

#[derive(Clone, Copy)]
struct CategoryRange(usize);

impl Ranged for CategoryRange {
    type FormatOption = DefaultFormatting;
    type ValueType = usize;

    fn map(&self, value: &usize, limit: (i32, i32)) -> i32 {
        if self.0 <= 1 {
            limit.0
        } else {
            RangedCoordusize::from(self.range()).map(value, limit)
        }
    }

    fn key_points<H: KeyPointHint>(&self, hint: H) -> Vec<usize> {
        RangedCoordusize::from(self.range()).key_points(hint)
    }

    fn range(&self) -> Range<usize> {
        0..self.0.saturating_sub(1)
    }
}

impl DiscreteRanged for CategoryRange {
    fn size(&self) -> usize {
        self.0
    }

    fn index_of(&self, value: &usize) -> Option<usize> {
        (*value < self.0).then_some(*value)
    }

    fn from_index(&self, index: usize) -> Option<usize> {
        self.index_of(&index)
    }
}

fn palette_color(chart_style: Option<&ChartStyleV>, i: usize) -> RGBAColor {
    match chart_style.and_then(|s| s.palette.as_deref()).filter(|p| !p.is_empty()) {
        Some(p) => p[i % p.len()].to_plotters(),
        None => PALETTE[i % PALETTE.len()].to_rgba(),
    }
}

fn series_color(
    chart_style: Option<&ChartStyleV>,
    explicit: Option<ChartColor>,
    i: usize,
) -> RGBAColor {
    match explicit {
        Some(c) => c.to_plotters(),
        None => palette_color(chart_style, i),
    }
}

fn text_style(size: f64, color: Option<ChartColor>) -> TextStyle<'static> {
    let mut style = TextStyle::from(("sans-serif", size).into_font());
    if let Some(c) = color {
        style.color = c.to_plotters().to_backend_color();
    }
    style
}

/// A tick or grid-line count as plotters takes it: at least `min`, at
/// most `MAX_TICKS`.
fn ticks(n: i64, min: usize) -> usize {
    n.clamp(min as i64, MAX_TICKS) as usize
}

/// A size in pixels as plotters takes it: a positive size rounds to at
/// least one pixel, anything else is none.
pub(crate) fn pixels(size: f64) -> u32 {
    if size > 0.0 { (size.round() as u32).max(1) } else { 0 }
}

fn stroke(style: &SeriesStyleV) -> u32 {
    pixels(style.stroke_width.unwrap_or(2.0))
}

/// A lone point has no segment to show it, so it gets a marker by default.
pub(crate) fn marker_size(point_size: Option<f64>, len: usize) -> u32 {
    match point_size {
        Some(p) => pixels(p),
        None if len == 1 => 3,
        None => 0,
    }
}

/// A legend entry that is a short line.
fn line_mark(s: ShapeStyle) -> impl Fn(BackendCoord) -> PathElement<BackendCoord> {
    move |(x, y)| PathElement::new([(x, y), (x + 20, y)], s)
}

/// A legend entry that is a dot.
fn dot_mark(r: u32, s: ShapeStyle) -> impl Fn(BackendCoord) -> Circle<BackendCoord, u32> {
    move |(x, y)| Circle::new((x, y), r, s)
}

/// A legend entry that is a small block.
fn block_mark(s: ShapeStyle) -> impl Fn(BackendCoord) -> Rectangle<BackendCoord> {
    move |(x, y)| Rectangle::new([(x, y - 5), (x + 20, y + 5)], s)
}

/// Log a series that failed to draw; give one that drew its legend entry.
fn annotate<'a, DB: DrawingBackend + 'a, E: IntoDynElement<'a, DB, BackendCoord>>(
    drawn: Result<&mut SeriesAnno<'a, DB>, DrawingAreaErrorKind<DB::ErrorType>>,
    label: Option<&str>,
    mark: impl Fn(BackendCoord) -> E + 'a,
    what: &str,
) {
    match drawn {
        Err(e) => error!("chart draw {what}: {e:?}"),
        Ok(ann) => {
            if let Some(l) = label {
                ann.label(l).legend(mark);
            }
        }
    }
}

/// Draw the series legend when a dataset has a label.
fn draw_legend<'a, DB: DrawingBackend + 'a, CT: CoordTranslate, X: GXExt>(
    chart: &mut ChartContext<'a, DB, CT>,
    w: &ChartW<X>,
    cs: Option<&ChartStyleV>,
    label_sz: f64,
) {
    if !w.datasets.iter().any(|ds| ds.mode() == Some(w.mode) && ds.label().is_some()) {
        return;
    }
    let ls = cs.and_then(|s| s.legend.as_ref());
    let bg =
        ls.and_then(|s| s.background).map_or(WHITE.to_rgba(), ChartColor::to_plotters);
    let border =
        ls.and_then(|s| s.border).map_or(BLACK.to_rgba(), ChartColor::to_plotters);
    let mut labels = chart.configure_series_labels();
    labels
        .position(
            cs.and_then(|s| s.legend_position.as_ref())
                .map_or(SeriesLabelPosition::UpperLeft, |p| p.0.clone()),
        )
        .margin(15)
        .background_style(bg.mix(0.8))
        .border_style(border)
        .label_font(text_style(
            ls.and_then(|s| s.label_size).map_or(label_sz, |s| s.0 as f64),
            ls.and_then(|s| s.label_color),
        ));
    if let Err(e) = labels.draw() {
        error!("chart series labels draw: {e:?}");
    }
}

/// Size the axis label areas from the mesh style, else to fit the y tick
/// labels over `y` and the axis descriptions; `x_pad` is the x area's
/// padding with and without a description.
fn label_areas<DB: DrawingBackend>(
    builder: &mut ChartBuilder<'_, '_, DB>,
    y: (f64, f64),
    descs: (bool, bool),
    mesh: Option<&MeshStyleV>,
    label_sz: f64,
    x_pad: (u32, u32),
) {
    let (_, tick_h) = estimate_text("0", label_sz);
    let fmt = y_ticks(y, mesh);
    let (lo, hi) = (fmt.format(y.0), fmt.format(y.1));
    let (tick_w, _) =
        estimate_text(if lo.len() > hi.len() { &lo } else { &hi }, label_sz);
    let auto_y = if descs.1 { tick_w + tick_h + 15 } else { tick_w + 8 };
    let auto_x = if descs.0 { tick_h * 2 + x_pad.0 } else { tick_h + x_pad.1 };
    let size = |s: Option<f64>, auto: u32| s.map_or(auto, |s| s as u32);
    builder.x_label_area_size(size(mesh.and_then(|m| m.x_label_area_size), auto_x));
    builder.y_label_area_size(size(mesh.and_then(|m| m.y_label_area_size), auto_y));
}

/// How the y axis over `y` writes its ticks.
fn y_ticks(y: (f64, f64), mesh: Option<&MeshStyleV>) -> TickFormat {
    TickFormat::new(y, mesh.and_then(|m| m.y_labels).map_or(10, |n| ticks(n, 1)))
}

/// How a numeric x axis over `x` writes its ticks.
fn x_ticks(x: (f64, f64), mesh: Option<&MeshStyleV>) -> TickFormat {
    TickFormat::new(x, mesh.and_then(|m| m.x_labels).map_or(10, |n| ticks(n, 1)))
}

/// What draw records for update: the plot rectangle and its ranges.
fn plot_info(
    (px, py): (Range<i32>, Range<i32>),
    x: (f64, f64),
    y: (f64, f64),
) -> PlotInfo {
    PlotInfo {
        rect: iced_core::Rectangle {
            x: px.start as f32,
            y: py.start as f32,
            width: (px.end - px.start) as f32,
            height: (py.end - py.start) as f32,
        },
        x_range: x,
        y_range: y,
    }
}

/// Set up the mesh of a 2D chart; an x axis that cannot take zero grid
/// lines (datetime, categories) gets `x_min_lines`.
fn configure_mesh<DB, C>(
    chart: &mut ChartContext<'_, DB, Cartesian2d<C, RangedCoordf64>>,
    x_label: Option<&str>,
    y_label: Option<&str>,
    mesh_style: Option<&MeshStyleV>,
    x_min_lines: usize,
    x_formatter: Option<&dyn Fn(&C::ValueType) -> String>,
    y: (f64, f64),
) -> bool
where
    DB: DrawingBackend,
    C: Ranged + ValueFormatter<C::ValueType>,
{
    let y_fmt = y_ticks(y, mesh_style);
    let y_formatter = |v: &f64| y_fmt.format(*v);
    let mut mesh_cfg = chart.configure_mesh();
    mesh_cfg.y_label_formatter(&y_formatter);
    if let Some(formatter) = x_formatter {
        mesh_cfg.x_label_formatter(formatter);
    }
    if let Some(xl) = x_label {
        mesh_cfg.x_desc(xl);
    }
    if let Some(yl) = y_label {
        mesh_cfg.y_desc(yl);
    }
    if let Some(ms) = mesh_style {
        if ms.show_x_grid == Some(false) {
            mesh_cfg.disable_x_mesh();
        }
        if ms.show_y_grid == Some(false) {
            mesh_cfg.disable_y_mesh();
        }
        if let Some(c) = ms.grid_color {
            mesh_cfg.light_line_style(c.to_plotters());
        }
        if let Some(c) = ms.bold_line_color {
            mesh_cfg.bold_line_style(c.to_plotters());
        }
        if let Some(c) = ms.axis_color {
            mesh_cfg.axis_style(c.to_plotters());
        }
        if ms.label_size.is_some() || ms.label_color.is_some() {
            let style =
                text_style(ms.label_size.map_or(12.0, |s| s.0 as f64), ms.label_color);
            mesh_cfg.label_style(style.clone());
            mesh_cfg.axis_desc_style(style);
        }
        if let Some(n) = ms.x_labels {
            mesh_cfg.x_labels(ticks(n, 1));
        }
        if let Some(n) = ms.y_labels {
            mesh_cfg.y_labels(ticks(n, 1));
        }
        if let Some(n) = ms.x_light_lines {
            mesh_cfg.x_max_light_lines(ticks(n, x_min_lines));
        }
        if let Some(n) = ms.y_light_lines {
            mesh_cfg.y_max_light_lines(ticks(n, 0));
        }
    }
    if let Err(e) = mesh_cfg.draw() {
        error!("chart mesh draw: {e:?}");
        return false;
    }
    true
}

/// Draw the XY, candlestick and error-bar series of a 2D chart, each cut
/// to `view` in data space; `to_x` maps a data x to the axis'.
fn draw_xy<'a, DB, C, X>(
    chart: &mut ChartContext<'a, DB, Cartesian2d<C, RangedCoordf64>>,
    w: &ChartW<X>,
    cs: Option<&ChartStyleV>,
    view: View,
    to_x: impl Fn(f64) -> C::ValueType,
) where
    DB: DrawingBackend + 'a,
    C: Ranged,
    C::ValueType: Clone + 'static,
    X: GXExt,
{
    let at = |(x, y): (f64, f64)| (to_x(x), y);
    for (i, ds) in w.datasets.iter().enumerate() {
        if ds.mode() != Some(w.mode) {
            continue;
        }
        match ds {
            DatasetEntry::XY { kind, data, style } => {
                let Some(d) = data.t.as_ref() else { continue };
                let color = series_color(cs, style.color, i);
                let line = ShapeStyle::from(color).stroke_width(stroke(style));
                let fill = ShapeStyle::from(color).filled();
                let label = style.label.as_deref();
                let lines = |chart: &mut _, label: Option<&str>| {
                    let runs = clip_polyline(&d.pts, view);
                    let none: [(f64, f64); 0] = [];
                    let mut label = label;
                    if runs.is_empty() {
                        annotate(
                            ChartContext::draw_series(
                                chart,
                                LineSeries::new(none.into_iter().map(at), line),
                            ),
                            label,
                            line_mark(line),
                            "line",
                        );
                    }
                    for run in runs.iter() {
                        let pts = run.iter().copied().map(at);
                        let drawn = match kind {
                            XYKind::Dashed { dash, gap } => ChartContext::draw_series(
                                chart,
                                DashedLineSeries::new(
                                    pts,
                                    pixels(*dash).max(1),
                                    pixels(*gap),
                                    line,
                                ),
                            ),
                            _ => ChartContext::draw_series(
                                chart,
                                LineSeries::new(pts, line),
                            ),
                        };
                        annotate(drawn, label.take(), line_mark(line), "line");
                    }
                };
                let markers = |chart: &mut _, ps: u32, label: Option<&str>| {
                    let dots = d
                        .pts
                        .iter()
                        .copied()
                        .filter(|p| ps > 0 && view.contains(*p))
                        .map(|p| Circle::new(at(p), ps, fill));
                    annotate(
                        ChartContext::draw_series(chart, dots),
                        label,
                        dot_mark(ps, fill),
                        "markers",
                    );
                };
                match kind {
                    XYKind::Line | XYKind::Dashed { .. } => {
                        lines(chart, label);
                        markers(chart, marker_size(style.point_size, d.pts.len()), None);
                    }
                    XYKind::Scatter => {
                        markers(chart, pixels(style.point_size.unwrap_or(3.0)), label)
                    }
                    XYKind::Area => {
                        let area = clip_area(&d.pts, view);
                        let fill = ShapeStyle::from(color.mix(0.3)).filled();
                        let none: &[(f64, f64)] = &[];
                        let mut label = label;
                        let outlines = area.iter().map(|r| &r[..]);
                        for outline in outlines.chain(area.is_empty().then_some(none)) {
                            let series = AreaSeries::new(
                                outline.iter().copied().map(at),
                                view.clamp_y(0.0),
                                fill,
                            );
                            annotate(
                                chart.draw_series(series),
                                label.take(),
                                line_mark(line),
                                "area",
                            );
                        }
                        lines(chart, None);
                        markers(chart, marker_size(style.point_size, d.pts.len()), None);
                    }
                }
            }
            DatasetEntry::Candlestick { data, style } => {
                let Some(d) = data.t.as_ref() else { continue };
                let pick = |c: Option<ChartColor>, or: RGBColor| {
                    ShapeStyle::from(c.map_or(or.to_rgba(), ChartColor::to_plotters))
                        .filled()
                };
                let (gain, loss) = (
                    pick(style.gain_color, DEFAULT_GAIN),
                    pick(style.loss_color, DEFAULT_LOSS),
                );
                let bw = pixels(style.bar_width.unwrap_or(5.0));
                let y = |v: f64| view.clamp_y(v);
                let candles =
                    d.pts.iter().filter(|p| view.contains_x(p.x) && p.is_finite()).map(
                        |p| {
                            let color = if p.open < p.close { gain } else { loss };
                            CandleStick::new(
                                to_x(p.x),
                                y(p.open),
                                y(p.high),
                                y(p.low),
                                y(p.close),
                                color,
                                color,
                                bw,
                            )
                        },
                    );
                annotate(
                    chart.draw_series(candles),
                    style.label.as_deref(),
                    block_mark(gain),
                    "candlestick",
                );
            }
            DatasetEntry::ErrorBar { data, style } => {
                let Some(d) = data.t.as_ref() else { continue };
                let line = ShapeStyle::from(series_color(cs, style.color, i))
                    .stroke_width(stroke(style));
                let caps = style.point_size.map_or(stroke(style), pixels);
                let (lo, hi) = view.y;
                let bars = d
                    .pts
                    .iter()
                    .filter(|p| view.contains_x(p.x) && p.is_finite())
                    .filter(|p| p.max >= lo && p.min <= hi)
                    .map(|p| ClippedErrorBar {
                        pts: [
                            (to_x(p.x), view.clamp_y(p.min)),
                            (to_x(p.x), view.clamp_y(p.avg)),
                            (to_x(p.x), view.clamp_y(p.max)),
                        ],
                        ends: (lo <= p.min, p.max <= hi),
                        avg: lo <= p.avg && p.avg <= hi,
                        style: line,
                        width: caps,
                    });
                annotate(
                    chart.draw_series(bars),
                    style.label.as_deref(),
                    line_mark(line),
                    "errorbar",
                );
            }
            DatasetEntry::Bar { .. }
            | DatasetEntry::Pie { .. }
            | DatasetEntry::Scatter3D { .. }
            | DatasetEntry::Line3D { .. }
            | DatasetEntry::Surface { .. } => {}
        }
    }
}

/// A vertical error bar cut to a view: its whisker inside it, a cap only
/// on an end that is the bar's own, the average only when it is in view.
struct ClippedErrorBar<C> {
    /// min, avg and max, moved into the view
    pts: [(C, f64); 3],
    /// whether min and max are in the view
    ends: (bool, bool),
    avg: bool,
    style: ShapeStyle,
    width: u32,
}

impl<'a, C: 'a> PointCollection<'a, (C, f64)> for &'a ClippedErrorBar<C> {
    type Point = &'a (C, f64);
    type IntoIter = &'a [(C, f64)];

    fn point_iter(self) -> &'a [(C, f64)] {
        &self.pts
    }
}

impl<C, DB: DrawingBackend> Drawable<DB> for ClippedErrorBar<C> {
    fn draw<I: Iterator<Item = BackendCoord>>(
        &self,
        mut points: I,
        backend: &mut DB,
        _: (u32, u32),
    ) -> Result<(), DrawingErrorKind<DB::ErrorType>> {
        let (Some(min), Some(avg), Some(max)) =
            (points.next(), points.next(), points.next())
        else {
            return Ok(());
        };
        let half = (self.width / 2) as i32;
        backend.draw_line(min, max, &self.style)?;
        for (end, at) in [(self.ends.0, min), (self.ends.1, max)] {
            if end {
                backend.draw_line(
                    (at.0 - half, at.1),
                    (at.0 + half, at.1),
                    &self.style,
                )?;
            }
        }
        if self.avg {
            backend.draw_circle(avg, self.width / 2, &self.style, self.style.filled)?;
        }
        Ok(())
    }
}

/// The y range a 2D chart takes when its view sets none: the program's,
/// else `auto`.
fn y_range<X: GXExt>(w: &ChartW<X>, view: &ViewState, auto: (f64, f64)) -> (f64, f64) {
    view.y
        .and_then(checked_range)
        .or_else(|| user_range(w.drawn.y_range.t.as_ref()))
        .unwrap_or(auto)
}

/// A range the program set, when plotters can draw over it.
fn user_range(r: Option<&OptAxisRange>) -> Option<(f64, f64)> {
    r.and_then(|r| r.0.as_ref()).and_then(|r| checked_range((r.min, r.max)))
}

/// The axis descriptions the program set.
fn axis_labels<X: GXExt>(w: &ChartW<X>) -> (Option<&str>, Option<&str>) {
    (label(&w.drawn.x_label.t), label(&w.drawn.y_label.t))
}

fn label(l: &Option<Option<ArcStr>>) -> Option<&str> {
    l.as_ref().and_then(|o| o.as_deref())
}

/// The tick text size of a chart's mesh.
fn label_size(cs: Option<&ChartStyleV>) -> f64 {
    cs.and_then(|s| s.mesh.as_ref())
        .and_then(|m| m.label_size)
        .map_or(12.0, |s| s.0 as f64)
}

/// Draw a numeric or time-series chart through `view`.
fn draw_xy_chart<DB: DrawingBackend, X: GXExt>(
    builder: &mut ChartBuilder<'_, '_, DB>,
    w: &ChartW<X>,
    cs: Option<&ChartStyleV>,
    view: &ViewState,
) -> bool {
    let time = w.mode == ChartMode::TimeSeries;
    let (auto_x, auto_y) = compute_ranges(&w.datasets, time);
    let user_x = w
        .drawn
        .x_range
        .t
        .as_ref()
        .and_then(|r| r.0.as_ref())
        .filter(|r| r.time == time)
        .and_then(|r| checked_range((r.min, r.max)));
    let x = view.x.and_then(checked_range).or(user_x).unwrap_or(auto_x);
    let y = y_range(w, view, auto_y);
    let plot = View { x: if time { time_range(x) } else { x }, y };
    let mesh = cs.and_then(|s| s.mesh.as_ref());
    let (x_label, y_label) = axis_labels(w);
    let descs = (x_label.is_some(), y_label.is_some());
    let pad = if time { (20, 12) } else { (15, 8) };
    label_areas(builder, y, descs, mesh, label_size(cs), pad);
    match time {
        true => draw_xy_on(
            builder,
            w,
            cs,
            plot,
            ms_datetime(plot.x.0)..ms_datetime(plot.x.1),
            ms_datetime,
            1,
            None,
        ),
        false => {
            let fmt = x_ticks(plot.x, mesh);
            let formatter = |v: &f64| fmt.format(*v);
            draw_xy_on(
                builder,
                w,
                cs,
                plot,
                plot.x.0..plot.x.1,
                |x| x,
                0,
                Some(&formatter),
            )
        }
    }
}

fn draw_xy_on<DB, R, X>(
    builder: &mut ChartBuilder<'_, '_, DB>,
    w: &ChartW<X>,
    cs: Option<&ChartStyleV>,
    view: View,
    x_range: R,
    to_x: impl Fn(f64) -> R::Value,
    x_min_lines: usize,
    x_formatter: Option<&dyn Fn(&R::Value) -> String>,
) -> bool
where
    DB: DrawingBackend,
    R: AsRangedCoord,
    R::CoordDescType: ValueFormatter<R::Value>,
    R::Value: Clone + 'static,
    X: GXExt,
{
    let mut chart = match builder.build_cartesian_2d(x_range, view.y.0..view.y.1) {
        Ok(c) => c,
        Err(e) => {
            error!("chart build: {e:?}");
            return false;
        }
    };
    w.plot_info.set(Some(plot_info(
        chart.plotting_area().get_pixel_range(),
        view.x,
        view.y,
    )));
    let (x_label, y_label) = axis_labels(w);
    let mesh_style = cs.and_then(|s| s.mesh.as_ref());
    if !configure_mesh(
        &mut chart,
        x_label,
        y_label,
        mesh_style,
        x_min_lines,
        x_formatter,
        view.y,
    ) {
        return false;
    }
    draw_xy(&mut chart, w, cs, view, to_x);
    draw_legend(&mut chart, w, cs, label_size(cs));
    true
}

impl<X: GXExt> iced_canvas::Program<crate::widgets::Message, crate::theme::GraphixTheme>
    for ChartW<X>
{
    type State = ChartState;

    fn update(
        &self,
        state: &mut Self::State,
        event: &iced_core::event::Event,
        bounds: iced_core::Rectangle,
        cursor: mouse::Cursor,
    ) -> Option<iced_widget::Action<crate::widgets::Message>> {
        state.handle_event(self, event, bounds, cursor)
    }

    fn mouse_interaction(
        &self,
        state: &Self::State,
        bounds: iced_core::Rectangle,
        cursor: mouse::Cursor,
    ) -> mouse::Interaction {
        state.mouse_interaction(self.mode, bounds, cursor)
    }

    fn draw(
        &self,
        state: &Self::State,
        renderer: &Renderer,
        _theme: &crate::theme::GraphixTheme,
        bounds: iced_core::Rectangle,
        cursor: mouse::Cursor,
    ) -> Vec<iced_canvas::Geometry<Renderer>> {
        let view = state.view_for(self);
        if self.cached_view.replace(Some(view)) != Some(view) {
            self.cache.clear();
        }
        let chart_geom = self.cache.draw(renderer, bounds.size(), |frame| {
            self.plot_info.set(None);
            let w = frame.width() as u32;
            let h = frame.height() as u32;
            if w == 0 || h == 0 || self.mode == ChartMode::Empty {
                return;
            }
            let backend = IcedBackend::new(frame, w, h);
            let root = backend.into_drawing_area();
            let cs = self.drawn.style.t.as_ref().and_then(|s| s.0.as_ref());
            let bg = cs
                .and_then(|s| s.background)
                .map_or(WHITE.to_rgba(), ChartColor::to_plotters);
            if let Err(e) = root.fill(&bg) {
                error!("chart fill: {e:?}");
                return;
            }
            let title = self.drawn.title.t.as_ref().and_then(|o| o.as_deref());
            let title_style = text_style(
                cs.and_then(|s| s.title_size).map_or(16.0, |s| s.0 as f64),
                cs.and_then(|s| s.title_color),
            );
            let drawn = match self.mode {
                ChartMode::Pie => match title.map(|t| root.titled(t, title_style)) {
                    None => draw_pie_chart(&root, self, cs),
                    Some(Ok(area)) => draw_pie_chart(&area, self, cs),
                    Some(Err(e)) => {
                        error!("chart title: {e:?}");
                        false
                    }
                },
                mode => {
                    let mut builder = ChartBuilder::on(&root);
                    builder.margin(cs.and_then(|s| s.margin).map_or(10, pixels));
                    if let Some(t) = title {
                        builder.caption(t, title_style);
                    }
                    match mode {
                        ChartMode::Numeric | ChartMode::TimeSeries => {
                            draw_xy_chart(&mut builder, self, cs, &view)
                        }
                        ChartMode::Bar => draw_bar_chart(&mut builder, self, cs, &view),
                        ChartMode::ThreeD => draw_3d_chart(&mut builder, self, cs, &view),
                        ChartMode::Pie | ChartMode::Empty => false,
                    }
                }
            };
            if drawn && let Err(e) = root.present() {
                error!("chart present: {e:?}");
            }
        });
        let mut result = vec![chart_geom];
        if let Some(snap) = state.tooltip(self, bounds, cursor) {
            let overlay = iced_canvas::Cache::new();
            let geom = overlay.draw(renderer, bounds.size(), |frame| {
                draw_tooltip(frame, &snap, bounds.size());
            });
            result.push(geom);
        }
        result
    }
}

/// Draw a bar chart through `view`.
fn draw_bar_chart<DB: DrawingBackend, X: GXExt>(
    builder: &mut ChartBuilder<'_, '_, DB>,
    w: &ChartW<X>,
    cs: Option<&ChartStyleV>,
    view: &ViewState,
) -> bool {
    let categories = w.bar_categories();
    let mut lo = 0.0f64;
    let mut hi = 0.0f64;
    for v in w.bars.series.iter().flat_map(|series| series.iter().flatten()) {
        if v.is_finite() {
            lo = lo.min(*v);
            hi = hi.max(*v);
        }
    }
    let y = y_range(w, view, pad_range(lo, hi));
    let mesh_style = cs.and_then(|s| s.mesh.as_ref());
    let (x_label, y_label) = axis_labels(w);
    let descs = (x_label.is_some(), y_label.is_some());
    label_areas(builder, y, descs, mesh_style, label_size(cs), (15, 8));
    let mut chart = match builder
        .build_cartesian_2d(CategoryRange(categories.len()).into_segmented(), y.0..y.1)
    {
        Ok(c) => c,
        Err(e) => {
            error!("chart build: {e:?}");
            return false;
        }
    };
    w.plot_info.set(Some(plot_info(
        chart.plotting_area().get_pixel_range(),
        (0.0, categories.len() as f64),
        y,
    )));
    let category_label = |index: &SegmentValue<usize>| match index {
        SegmentValue::Exact(i) | SegmentValue::CenterOf(i) => {
            categories.get(*i).map(|s| s.to_string()).unwrap_or_default()
        }
        SegmentValue::Last => String::new(),
    };
    if !configure_mesh(
        &mut chart,
        x_label,
        y_label,
        mesh_style,
        1,
        Some(&category_label),
        y,
    ) {
        return false;
    }
    let view = View { x: (0.0, categories.len() as f64), y };
    for (i, ds) in w.datasets.iter().enumerate() {
        let DatasetEntry::Bar { style, .. } = ds else { continue };
        let fill = ShapeStyle::from(series_color(cs, style.color, i)).filled();
        // CR claude for eric: [bug/low] Each bar series is a separate Histogram over
        // the same category segments, so series overlap full-width at a shared
        // category, and a later series hides every earlier one it is at least as tall
        // as. nearest gives every series at a slot the same distance and keeps the
        // first, so the tooltip names the earliest, possibly hidden, series. Needs a
        // decision on the multi-series layout: grouped (each series takes 1/n of the
        // slot, nearest picks by sub-slot) or stacked. Found by reading; not run.
        // (gui-chart-05)
        let hist = Histogram::vertical(&chart)
            .style(fill)
            .margin(pixels(style.margin.unwrap_or(5.0)))
            .baseline(view.clamp_y(0.0))
            .data((0..categories.len()).filter_map(|slot| {
                let v = w.bars.value(i, slot).filter(|v| v.is_finite())?;
                Some((slot, view.clamp_y(v)))
            }));
        annotate(
            chart.draw_series(hist),
            style.label.as_deref(),
            block_mark(fill),
            "bar",
        );
    }
    draw_legend(&mut chart, w, cs, label_size(cs));
    true
}

/// Draw a pie chart centred in `area`.
fn draw_pie_chart<DB: DrawingBackend, X: GXExt>(
    area: &DrawingArea<DB, Shift>,
    w: &ChartW<X>,
    cs: Option<&ChartStyleV>,
) -> bool {
    let Some((_, pie_data, pie_style)) = first_pie(&w.datasets) else { return false };
    // a title taller than the chart leaves an area of negative height,
    // which plotters reports wrapped
    let (aw, ah) = area.dim_in_pixel();
    let (Ok(aw), Ok(ah)) = (i32::try_from(aw), i32::try_from(ah)) else { return false };
    let (labels, sizes): (Vec<&str>, Vec<f64>) = pie_slices(pie_data).unzip();
    if aw == 0 || ah == 0 || sizes.is_empty() {
        return false;
    }
    let center = (aw / 2, ah / 2);
    let radius = (aw.min(ah) as f64 * 0.35).max(10.0);
    // plotters' pie takes opaque colors
    let colors: Vec<RGBColor> = (0..sizes.len())
        .map(|i| match pie_style.colors.as_deref() {
            Some(cs) if !cs.is_empty() => cs[i % cs.len()].to_plotters(),
            _ => palette_color(cs, i),
        })
        .map(|c| RGBColor(c.0, c.1, c.2))
        .collect();
    let base = area.get_base_pixel();
    w.plot_info.set(Some(PlotInfo {
        rect: iced_core::Rectangle {
            x: (base.0 + center.0) as f32 - radius as f32,
            y: (base.1 + center.1) as f32 - radius as f32,
            width: radius as f32 * 2.0,
            height: radius as f32 * 2.0,
        },
        x_range: (0.0, 1.0),
        y_range: (0.0, 1.0),
    }));
    let label_sz = label_size(cs);
    let mut pie = Pie::new(&center, &radius, &sizes, &colors, &labels);
    pie.label_style(("sans-serif", label_sz).into_font());
    pie.start_angle(pie_start_angle(pie_style));
    if let Some(hole) = pie_style.donut.filter(|d| d.is_finite()) {
        pie.donut_hole(hole.clamp(0.0, 1.0) * radius);
    }
    if pie_style.show_percentages == Some(true) {
        pie.percentages(("sans-serif", label_sz * 0.9).into_font());
    }
    if let Some(offset) = pie_style.label_offset.filter(|o| o.is_finite()) {
        pie.label_offset(radius * offset / 100.0);
    }
    if let Err(e) = area.draw(&pie) {
        error!("chart draw pie: {e:?}");
    }
    true
}

/// Draw a 3D chart turned and scaled by `view`.
fn draw_3d_chart<DB: DrawingBackend, X: GXExt>(
    builder: &mut ChartBuilder<'_, '_, DB>,
    w: &ChartW<X>,
    cs: Option<&ChartStyleV>,
    view: &ViewState,
) -> bool {
    let (auto_x, auto_y, auto_z) = compute_3d_ranges(&w.datasets);
    let x = w
        .drawn
        .x_range
        .t
        .as_ref()
        .and_then(|r| r.0.as_ref())
        .filter(|r| !r.time)
        .and_then(|r| checked_range((r.min, r.max)))
        .unwrap_or(auto_x);
    let y = user_range(w.drawn.y_range.t.as_ref()).unwrap_or(auto_y);
    let (z_min, z_max) = user_range(w.drawn.z_range.t.as_ref()).unwrap_or(auto_z);
    let mesh_style = cs.and_then(|s| s.mesh.as_ref());
    let size = |s: Option<f64>| s.map_or(30, |s| s as u32);
    builder.x_label_area_size(size(mesh_style.and_then(|m| m.x_label_area_size)));
    builder.y_label_area_size(size(mesh_style.and_then(|m| m.y_label_area_size)));
    let mut chart = match builder.build_cartesian_3d(x.0..x.1, z_min..z_max, y.0..y.1) {
        Ok(c) => c,
        Err(e) => {
            error!("chart build: {e:?}");
            return false;
        }
    };
    let proj = w.drawn.projection.t.as_ref().and_then(|o| o.0.as_ref());
    chart.with_projection(|mut pb| {
        if let Some(p) = proj {
            pb.yaw = p.yaw.unwrap_or(pb.yaw);
            pb.pitch = p.pitch.unwrap_or(pb.pitch);
            pb.scale = p.scale.unwrap_or(pb.scale);
        }
        pb.yaw += view.yaw;
        pb.pitch += view.pitch;
        pb.scale *= view.scale;
        pb.into_matrix()
    });
    {
        let mut axes = chart.configure_axes();
        if let Some(ms) = mesh_style {
            if ms.label_size.is_some() || ms.label_color.is_some() {
                axes.label_style(text_style(
                    ms.label_size.map_or(12.0, |s| s.0 as f64),
                    ms.label_color,
                ));
            }
            if let Some(c) = ms.grid_color {
                axes.light_grid_style(c.to_plotters());
            }
            if let Some(c) = ms.bold_line_color {
                axes.bold_grid_style(c.to_plotters());
            }
            // plotters' 3D y is up: the chart's z
            if let Some(n) = ms.x_labels {
                axes.x_labels(ticks(n, 1));
            }
            if let Some(n) = ms.y_labels {
                axes.z_labels(ticks(n, 1));
            }
            if let Some(n) = ms.z_labels {
                axes.y_labels(ticks(n, 1));
            }
            if let Some(n) = ms.x_light_lines {
                axes.x_max_light_lines(ticks(n, 0));
            }
            if let Some(n) = ms.y_light_lines {
                axes.z_max_light_lines(ticks(n, 0));
            }
            if let Some(n) = ms.z_light_lines {
                axes.y_max_light_lines(ticks(n, 0));
            }
        }
        let (x_label, y_label) = axis_labels(w);
        let z_label = w.drawn.z_label.t.as_ref().and_then(|o| o.as_deref());
        let (x_fn, y_fn, z_fn) =
            (axis_format(x_label), axis_format(z_label), axis_format(y_label));
        axes.x_formatter(&x_fn);
        axes.y_formatter(&y_fn);
        axes.z_formatter(&z_fn);
        if let Err(e) = axes.draw() {
            error!("chart 3d axes draw: {e:?}");
        }
    }
    let contains = |p: &(f64, f64, f64)| {
        x.0 <= p.0
            && p.0 <= x.1
            && y.0 <= p.1
            && p.1 <= y.1
            && z_min <= p.2
            && p.2 <= z_max
    };
    let up = |&(x, y, z): &(f64, f64, f64)| (x, z, y);
    for (i, ds) in w.datasets.iter().enumerate() {
        match ds {
            DatasetEntry::Scatter3D { data, style } => {
                let Some(pts) = data.t.as_ref() else { continue };
                let fill = ShapeStyle::from(series_color(cs, style.color, i)).filled();
                let ps = pixels(style.point_size.unwrap_or(3.0));
                let dots = pts
                    .0
                    .iter()
                    .filter(|p| contains(p))
                    .map(|p| Circle::new(up(p), ps, fill));
                annotate(
                    chart.draw_series(dots),
                    style.label.as_deref(),
                    dot_mark(ps, fill),
                    "scatter3d",
                );
            }
            DatasetEntry::Line3D { data, style } => {
                let Some(pts) = data.t.as_ref() else { continue };
                let color = series_color(cs, style.color, i);
                let line = ShapeStyle::from(color).stroke_width(stroke(style));
                let fill = ShapeStyle::from(color).filled();
                let mut label = style.label.as_deref();
                for run in pts.0.split(|p| !contains(p)) {
                    annotate(
                        chart.draw_series(LineSeries::new(run.iter().map(up), line)),
                        label.take(),
                        line_mark(line),
                        "line3d",
                    );
                }
                let ps = marker_size(style.point_size, pts.0.len());
                if ps > 0 {
                    let dots = pts
                        .0
                        .iter()
                        .filter(|p| contains(p))
                        .map(|p| Circle::new(up(p), ps, fill));
                    annotate(
                        chart.draw_series(dots),
                        None,
                        dot_mark(ps, fill),
                        "line3d markers",
                    );
                }
            }
            DatasetEntry::Surface { data, style } => {
                let Some(grid) = data.t.as_ref() else { continue };
                let color = series_color(cs, style.color, i);
                let by_z = style.color_by_z.unwrap_or(false);
                let flat = color.mix(0.6).filled();
                let shade = |z: f64| match by_z {
                    false => flat,
                    true => {
                        let t = match z_max > z_min {
                            true => ((z - z_min) / (z_max - z_min)).clamp(0.0, 1.0),
                            false => 0.5,
                        };
                        let (r, g, b) = hsl_to_rgb((1.0 - t) * 240.0, 0.8, 0.5);
                        RGBColor(r, g, b).mix(0.6).filled()
                    }
                };
                // each cell is the quad of its four grid points
                let cells = grid.0.windows(2).flat_map(|rows| {
                    let (a, b) = (&rows[0], &rows[1]);
                    let n = a.len().min(b.len());
                    (1..n).map(move |j| [a[j - 1], a[j], b[j], b[j - 1]])
                });
                let quads = cells.filter(|q| q.iter().all(contains)).map(|q| {
                    let z = q.iter().map(|p| p.2).sum::<f64>() / 4.0;
                    Polygon::new(q.iter().map(up).collect::<Vec<_>>(), shade(z))
                });
                annotate(
                    chart.draw_series(quads),
                    style.label.as_deref(),
                    block_mark(ShapeStyle::from(color).filled()),
                    "surface",
                );
            }
            _ => {}
        }
    }
    draw_legend(&mut chart, w, cs, label_size(cs));
    true
}

/// A 3D axis' tick text: the value, after the axis' label when it has one.
// CR claude for claude: [bug/low] 3D ticks are written with one fixed decimal, so a
// surface whose z spans 0..0.01 labels every tick 0.0, and Unix-ms values print as
// 13-digit integers. The 2D value axes moved to TickFormat for exactly this; the 3D
// axes did not. Fix: in draw_3d_chart, build a TickFormat per axis from its range and
// the mesh's label count, and have axis_format write `{l}: {fmt.format(*v)}`. Found by
// reading; not run. (gui-chart-03)
fn axis_format(label: Option<&str>) -> impl Fn(&f64) -> String + '_ {
    move |v| match label {
        Some(l) => format!("{l}: {v:.1}"),
        None => format!("{v:.1}"),
    }
}

/// Convert HSL to RGB (hue in degrees 0..360, s and l in 0..1).
fn hsl_to_rgb(h: f64, s: f64, l: f64) -> (u8, u8, u8) {
    let c = (1.0 - (2.0 * l - 1.0).abs()) * s;
    let h2 = h / 60.0;
    let x = c * (1.0 - (h2 % 2.0 - 1.0).abs());
    let (r1, g1, b1) = if h2 < 1.0 {
        (c, x, 0.0)
    } else if h2 < 2.0 {
        (x, c, 0.0)
    } else if h2 < 3.0 {
        (0.0, c, x)
    } else if h2 < 4.0 {
        (0.0, x, c)
    } else if h2 < 5.0 {
        (x, 0.0, c)
    } else {
        (c, 0.0, x)
    };
    let m = l - c / 2.0;
    (((r1 + m) * 255.0) as u8, ((g1 + m) * 255.0) as u8, ((b1 + m) * 255.0) as u8)
}
