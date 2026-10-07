use super::{
    ChartW,
    clip::{View, clip_area, clip_polyline},
    dataset::{
        ChartMode, DatasetEntry, XYKind, bar_categories, bar_value, pie_slices,
        pie_start_angle,
    },
    interact::{ChartState, PlotInfo, draw_tooltip},
    plotters_backend::{IcedBackend, estimate_text},
    ranges::*,
    types::*,
};
use crate::widgets::Renderer;
use graphix_rt::GXExt;
use iced_core::mouse;
use iced_widget::canvas as iced_canvas;
use log::error;
use plotters::{
    chart::{ChartBuilder, ChartContext, SeriesAnno},
    coord::CoordTranslate,
    drawing::DrawingAreaErrorKind,
    element::{
        CandleStick, Circle, ErrorBar, IntoDynElement, PathElement, Pie, Polygon,
        Rectangle,
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
use plotters_backend::BackendCoord;
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

fn stroke(style: &SeriesStyleV) -> u32 {
    style.stroke_width.unwrap_or(2.0) as u32
}

/// A lone point has no segment to show it, so it gets a marker by default.
pub(crate) fn marker_size(point_size: Option<f64>, len: usize) -> u32 {
    match point_size {
        Some(p) => p as u32,
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
    if !w.datasets.iter().any(|ds| ds.label().is_some()) {
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
            ls.and_then(|s| s.label_size).unwrap_or(label_sz),
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
    let prec = tick_precision(y.1 - y.0);
    let (lo, hi) = (format!("{:.prec$}", y.0), format!("{:.prec$}", y.1));
    let (tick_w, _) =
        estimate_text(if lo.len() > hi.len() { &lo } else { &hi }, label_sz);
    let auto_y = if descs.1 { tick_w + tick_h + 15 } else { tick_w + 8 };
    let auto_x = if descs.0 { tick_h * 2 + x_pad.0 } else { tick_h + x_pad.1 };
    let size = |s: Option<f64>, auto: u32| s.map_or(auto, |s| s as u32);
    builder.x_label_area_size(size(mesh.and_then(|m| m.x_label_area_size), auto_x));
    builder.y_label_area_size(size(mesh.and_then(|m| m.y_label_area_size), auto_y));
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
/// lines (datetime, categories) gets `$x_min_lines`.
macro_rules! configure_mesh {
    ($chart:expr, $x_label:expr, $y_label:expr, $mesh_style:expr, $x_min_lines:expr) => {{
        let mut mesh_cfg = $chart.configure_mesh();
        if let Some(xl) = $x_label {
            mesh_cfg.x_desc(xl);
        }
        if let Some(yl) = $y_label {
            mesh_cfg.y_desc(yl);
        }
        if let Some(ms) = $mesh_style {
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
                let style = text_style(ms.label_size.unwrap_or(12.0), ms.label_color);
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
                mesh_cfg.x_max_light_lines(ticks(n, $x_min_lines));
            }
            if let Some(n) = ms.y_light_lines {
                mesh_cfg.y_max_light_lines(ticks(n, 0));
            }
        }
        if let Err(e) = mesh_cfg.draw() {
            error!("chart mesh draw: {e:?}");
            return;
        }
    }};
}

/// Draw the XY, candlestick and error-bar series of a 2D chart, each cut
/// to `$view` in data space; `$to_x` maps a data x to the axis'.
macro_rules! draw_xy_body {
    ($chart:expr, $w:expr, $cs:expr, $view:expr, $to_x:expr) => {{
        let view: View = $view;
        let cs: Option<&ChartStyleV> = $cs;
        let at = |(x, y): (f64, f64)| ($to_x(x), y);
        for (i, ds) in $w.datasets.iter().enumerate() {
            if ds.mode() != Some($w.mode) {
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
                                XYKind::Dashed { dash, gap } => {
                                    ChartContext::draw_series(
                                        chart,
                                        DashedLineSeries::new(
                                            pts,
                                            (*dash as u32).max(1),
                                            *gap as u32,
                                            line,
                                        ),
                                    )
                                }
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
                            lines(&mut $chart, label);
                            markers(
                                &mut $chart,
                                marker_size(style.point_size, d.pts.len()),
                                None,
                            );
                        }
                        XYKind::Scatter => markers(
                            &mut $chart,
                            style.point_size.unwrap_or(3.0) as u32,
                            label,
                        ),
                        XYKind::Area => {
                            let area = clip_area(&d.pts, view);
                            let series = AreaSeries::new(
                                area.iter().copied().map(at),
                                view.clamp_y(0.0),
                                ShapeStyle::from(color.mix(0.3)).filled(),
                            );
                            annotate(
                                $chart.draw_series(series),
                                label,
                                line_mark(line),
                                "area",
                            );
                            lines(&mut $chart, None);
                            markers(
                                &mut $chart,
                                marker_size(style.point_size, d.pts.len()),
                                None,
                            );
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
                    let bw = style.bar_width.unwrap_or(5.0) as u32;
                    let y = |v: f64| view.clamp_y(v);
                    let candles =
                        d.pts.iter().filter(|p| view.contains_x(p.x)).map(|p| {
                            CandleStick::new(
                                $to_x(p.x),
                                y(p.open),
                                y(p.high),
                                y(p.low),
                                y(p.close),
                                gain,
                                loss,
                                bw,
                            )
                        });
                    annotate(
                        $chart.draw_series(candles),
                        style.label.as_deref(),
                        block_mark(gain),
                        "candlestick",
                    );
                }
                DatasetEntry::ErrorBar { data, style } => {
                    let Some(d) = data.t.as_ref() else { continue };
                    let line = ShapeStyle::from(series_color(cs, style.color, i))
                        .stroke_width(stroke(style));
                    let caps = style.point_size.map_or(stroke(style), |p| p as u32);
                    let y = |v: f64| view.clamp_y(v);
                    let bars = d.pts.iter().filter(|p| view.contains_x(p.x)).map(|p| {
                        ErrorBar::new_vertical(
                            $to_x(p.x),
                            y(p.min),
                            y(p.avg),
                            y(p.max),
                            line,
                            caps,
                        )
                    });
                    annotate(
                        $chart.draw_series(bars),
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
    }};
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
        _cursor: mouse::Cursor,
    ) -> Vec<iced_canvas::Geometry<Renderer>> {
        if self.dirty.get() {
            state.cache.clear();
            self.dirty.set(false);
        }
        let view = state.view_for(self);
        let chart_geom = state.cache.draw(renderer, bounds.size(), |frame| {
            state.plot_info.set(None);
            let w = frame.width() as u32;
            let h = frame.height() as u32;
            if w == 0 || h == 0 || self.mode == ChartMode::Empty {
                return;
            }
            let backend = IcedBackend::new(frame, w, h);
            let root = backend.into_drawing_area();
            let chart_style = self.style.t.as_ref().and_then(|s| s.0.as_ref());
            let bg = chart_style
                .and_then(|s| s.background)
                .map_or(WHITE.to_rgba(), ChartColor::to_plotters);
            if let Err(e) = root.fill(&bg) {
                error!("chart fill: {e:?}");
                return;
            }
            let title = self.title.t.as_ref().and_then(|o| o.as_deref());
            let x_label = self.x_label.t.as_ref().and_then(|o| o.as_deref());
            let y_label = self.y_label.t.as_ref().and_then(|o| o.as_deref());
            let margin = chart_style.and_then(|s| s.margin).unwrap_or(10.0);
            let title_size = chart_style.and_then(|s| s.title_size).unwrap_or(16.0);
            let mesh_style = chart_style.and_then(|s| s.mesh.as_ref());
            let label_sz = mesh_style.and_then(|ms| ms.label_size).unwrap_or(12.0);
            let descs = (x_label.is_some(), y_label.is_some());
            let mut builder = ChartBuilder::on(&root);
            builder.margin(margin as u32);
            if let Some(t) = title {
                let title_color = chart_style.and_then(|s| s.title_color);
                builder.caption(t, text_style(title_size, title_color));
            }
            let user = |r: Option<&OptAxisRange>| {
                r.and_then(|r| r.0.as_ref()).and_then(|r| checked_range((r.min, r.max)))
            };
            let user_y = user(self.y_range.t.as_ref());
            let user_x = |time: bool| {
                self.x_range
                    .t
                    .as_ref()
                    .and_then(|r| r.0.as_ref())
                    .filter(|r| r.time == time)
                    .and_then(|r| checked_range((r.min, r.max)))
            };
            match self.mode {
                ChartMode::Numeric | ChartMode::TimeSeries => {
                    let time = self.mode == ChartMode::TimeSeries;
                    let (auto_x, auto_y) = compute_ranges(&self.datasets, time);
                    let x =
                        view.x.and_then(checked_range).or(user_x(time)).unwrap_or(auto_x);
                    let y = view.y.and_then(checked_range).or(user_y).unwrap_or(auto_y);
                    let plot = View { x: if time { time_range(x) } else { x }, y };
                    let pad = if time { (20, 12) } else { (15, 8) };
                    label_areas(&mut builder, y, descs, mesh_style, label_sz, pad);
                    macro_rules! xy_chart {
                        ($x_range:expr, $to_x:expr, $x_min_lines:expr) => {{
                            let mut chart =
                                match builder.build_cartesian_2d($x_range, y.0..y.1) {
                                    Ok(c) => c,
                                    Err(e) => return error!("chart build: {e:?}"),
                                };
                            state.plot_info.set(Some(plot_info(
                                chart.plotting_area().get_pixel_range(),
                                plot.x,
                                y,
                            )));
                            configure_mesh!(
                                chart,
                                x_label,
                                y_label,
                                mesh_style,
                                $x_min_lines
                            );
                            draw_xy_body!(chart, self, chart_style, plot, $to_x);
                            draw_legend(&mut chart, self, chart_style, label_sz);
                        }};
                    }
                    match time {
                        true => xy_chart!(
                            ms_datetime(plot.x.0)..ms_datetime(plot.x.1),
                            ms_datetime,
                            1
                        ),
                        false => xy_chart!(plot.x.0..plot.x.1, |x: f64| x, 0),
                    }
                }
                ChartMode::Bar => {
                    let categories = bar_categories(&self.datasets);
                    let mut lo = 0.0f64;
                    let mut hi = 0.0f64;
                    for ds in self.datasets.iter() {
                        if let DatasetEntry::Bar { data: d, .. } = ds
                            && let Some(bd) = d.t.as_ref()
                        {
                            for v in categories.iter().filter_map(|c| bar_value(bd, c)) {
                                if v.is_finite() {
                                    lo = lo.min(v);
                                    hi = hi.max(v);
                                }
                            }
                        }
                    }
                    let y = view
                        .y
                        .and_then(checked_range)
                        .or(user_y)
                        .unwrap_or_else(|| pad_range(lo, hi));
                    label_areas(&mut builder, y, descs, mesh_style, label_sz, (15, 8));
                    let mut chart = match builder.build_cartesian_2d(
                        categories.as_slice().into_segmented(),
                        y.0..y.1,
                    ) {
                        Ok(c) => c,
                        Err(e) => return error!("chart build: {e:?}"),
                    };
                    state.plot_info.set(Some(plot_info(
                        chart.plotting_area().get_pixel_range(),
                        (0.0, categories.len() as f64),
                        y,
                    )));
                    configure_mesh!(chart, x_label, y_label, mesh_style, 1);
                    let view = View { x: (0.0, categories.len() as f64), y };
                    for (i, ds) in self.datasets.iter().enumerate() {
                        let DatasetEntry::Bar { data, style } = ds else { continue };
                        let Some(bd) = data.t.as_ref() else { continue };
                        let fill =
                            ShapeStyle::from(series_color(chart_style, style.color, i))
                                .filled();
                        let hist = Histogram::vertical(&chart)
                            .style(fill)
                            .margin(style.margin.unwrap_or(5.0) as u32)
                            .baseline(view.clamp_y(0.0))
                            .data(categories.iter().filter_map(|c| {
                                let v = bar_value(bd, c).filter(|v| v.is_finite())?;
                                Some((c, view.clamp_y(v)))
                            }));
                        annotate(
                            chart.draw_series(hist),
                            style.label.as_deref(),
                            block_mark(fill),
                            "bar",
                        );
                    }
                    draw_legend(&mut chart, self, chart_style, label_sz);
                }
                ChartMode::Pie => {
                    let Some((pie_data, pie_style)) =
                        self.datasets.iter().find_map(|ds| match ds {
                            DatasetEntry::Pie { data, style } => {
                                data.t.as_ref().map(|d| (d, style))
                            }
                            _ => None,
                        })
                    else {
                        return;
                    };
                    let title_h = match title {
                        Some(t) => estimate_text(t, title_size).1 + margin as u32,
                        None => 0,
                    };
                    let avail_h = h.saturating_sub(title_h);
                    let (labels, sizes): (Vec<&str>, Vec<f64>) =
                        pie_slices(pie_data).unzip();
                    if avail_h == 0 || sizes.is_empty() {
                        return;
                    }
                    let center = ((w / 2) as i32, (title_h + avail_h / 2) as i32);
                    let radius = (w.min(avail_h) as f64 * 0.35).max(10.0);
                    // plotters' pie takes opaque colors
                    let colors: Vec<RGBColor> = (0..sizes.len())
                        .map(|i| match pie_style.colors.as_deref() {
                            Some(cs) if !cs.is_empty() => cs[i % cs.len()].to_plotters(),
                            _ => palette_color(chart_style, i),
                        })
                        .map(|c| RGBColor(c.0, c.1, c.2))
                        .collect();
                    state.plot_info.set(Some(PlotInfo {
                        rect: iced_core::Rectangle {
                            x: center.0 as f32 - radius as f32,
                            y: center.1 as f32 - radius as f32,
                            width: radius as f32 * 2.0,
                            height: radius as f32 * 2.0,
                        },
                        x_range: (0.0, 1.0),
                        y_range: (0.0, 1.0),
                    }));
                    let mut pie = Pie::new(&center, &radius, &sizes, &colors, &labels);
                    pie.label_style(("sans-serif", label_sz).into_font());
                    pie.start_angle(pie_start_angle(pie_style));
                    if let Some(hole) = pie_style.donut.filter(|d| d.is_finite()) {
                        pie.donut_hole(hole.clamp(0.0, 1.0) * radius);
                    }
                    if pie_style.show_percentages == Some(true) {
                        pie.percentages(("sans-serif", label_sz * 0.9).into_font());
                    }
                    if let Some(offset) = pie_style.label_offset.filter(|o| o.is_finite())
                    {
                        pie.label_offset(radius * offset / 100.0);
                    }
                    if let Err(e) = root.draw(&pie) {
                        error!("chart draw pie: {e:?}");
                    }
                }
                ChartMode::ThreeD => {
                    let (auto_x, auto_y, auto_z) = compute_3d_ranges(&self.datasets);
                    let x = user_x(false).unwrap_or(auto_x);
                    let y = user_y.unwrap_or(auto_y);
                    let (z_min, z_max) = user(self.z_range.t.as_ref()).unwrap_or(auto_z);
                    let size = |s: Option<f64>| s.map_or(30, |s| s as u32);
                    builder.x_label_area_size(size(
                        mesh_style.and_then(|m| m.x_label_area_size),
                    ));
                    builder.y_label_area_size(size(
                        mesh_style.and_then(|m| m.y_label_area_size),
                    ));
                    let mut chart = match builder.build_cartesian_3d(
                        x.0..x.1,
                        z_min..z_max,
                        y.0..y.1,
                    ) {
                        Ok(c) => c,
                        Err(e) => return error!("chart build: {e:?}"),
                    };
                    let proj = self.projection.t.as_ref().and_then(|o| o.0.as_ref());
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
                                    ms.label_size.unwrap_or(12.0),
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
                        let z_label = self.z_label.t.as_ref().and_then(|o| o.as_deref());
                        let (x_fn, y_fn, z_fn) = (
                            axis_format(x_label),
                            axis_format(z_label),
                            axis_format(y_label),
                        );
                        axes.x_formatter(&x_fn);
                        axes.y_formatter(&y_fn);
                        axes.z_formatter(&z_fn);
                        if let Err(e) = axes.draw() {
                            error!("chart 3d axes draw: {e:?}");
                        }
                    }
                    let up = |&(x, y, z): &(f64, f64, f64)| (x, z, y);
                    for (i, ds) in self.datasets.iter().enumerate() {
                        match ds {
                            DatasetEntry::Scatter3D { data, style } => {
                                let Some(pts) = data.t.as_ref() else { continue };
                                let fill = ShapeStyle::from(series_color(
                                    chart_style,
                                    style.color,
                                    i,
                                ))
                                .filled();
                                let ps = style.point_size.unwrap_or(3.0) as u32;
                                let dots =
                                    pts.0.iter().map(|p| Circle::new(up(p), ps, fill));
                                annotate(
                                    chart.draw_series(dots),
                                    style.label.as_deref(),
                                    dot_mark(ps, fill),
                                    "scatter3d",
                                );
                            }
                            DatasetEntry::Line3D { data, style } => {
                                let Some(pts) = data.t.as_ref() else { continue };
                                let color = series_color(chart_style, style.color, i);
                                let line =
                                    ShapeStyle::from(color).stroke_width(stroke(style));
                                let fill = ShapeStyle::from(color).filled();
                                annotate(
                                    chart.draw_series(LineSeries::new(
                                        pts.0.iter().map(up),
                                        line,
                                    )),
                                    style.label.as_deref(),
                                    line_mark(line),
                                    "line3d",
                                );
                                let ps = marker_size(style.point_size, pts.0.len());
                                if ps > 0 {
                                    let dots = pts
                                        .0
                                        .iter()
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
                                let color = series_color(chart_style, style.color, i);
                                let by_z = style.color_by_z.unwrap_or(false);
                                let flat = color.mix(0.6).filled();
                                let shade = |z: f64| match by_z {
                                    false => flat,
                                    true => {
                                        let t = match z_max > z_min {
                                            true => ((z - z_min) / (z_max - z_min))
                                                .clamp(0.0, 1.0),
                                            false => 0.5,
                                        };
                                        let (r, g, b) =
                                            hsl_to_rgb((1.0 - t) * 240.0, 0.8, 0.5);
                                        RGBColor(r, g, b).mix(0.6).filled()
                                    }
                                };
                                // each cell is the quad of its four grid points
                                let cells = grid.0.windows(2).flat_map(|rows| {
                                    let (a, b) = (&rows[0], &rows[1]);
                                    let n = a.len().min(b.len());
                                    (1..n).map(move |j| [a[j - 1], a[j], b[j], b[j - 1]])
                                });
                                let quads = cells.map(|q| {
                                    let z = q.iter().map(|p| p.2).sum::<f64>() / 4.0;
                                    Polygon::new(
                                        q.iter().map(up).collect::<Vec<_>>(),
                                        shade(z),
                                    )
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
                    draw_legend(&mut chart, self, chart_style, label_sz);
                }
                ChartMode::Empty => return,
            }
            if let Err(e) = root.present() {
                error!("chart present: {e:?}");
            }
        });
        let mut result = vec![chart_geom];
        if let Some(snap) = state.snap_point.as_ref().filter(|_| state.owns(self)) {
            let overlay = iced_canvas::Cache::new();
            let geom = overlay.draw(renderer, bounds.size(), |frame| {
                draw_tooltip(frame, snap, bounds.size());
            });
            result.push(geom);
        }
        result
    }
}

/// A 3D axis' tick text: the value, after the axis' label when it has one.
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
