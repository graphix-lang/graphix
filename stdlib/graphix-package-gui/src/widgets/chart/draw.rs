use super::{
    ChartW,
    dataset::{ChartMode, DatasetEntry, XYKind, chart_mode},
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
    chart::ChartBuilder,
    element::{CandleStick, ErrorBar, PathElement, Pie},
    prelude::{
        AreaSeries, Circle, DashedLineSeries, Histogram, IntoDrawingArea,
        IntoSegmentedCoord, LineSeries, SeriesLabelPosition, SurfaceSeries,
    },
    style::{
        BLACK, Color as PlotColor, IntoFont, RGBColor, ShapeStyle, TextStyle, WHITE,
    },
};

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

fn palette_color(chart_style: Option<&ChartStyleV>, i: usize) -> RGBColor {
    match chart_style.and_then(|s| s.palette.as_deref()).filter(|p| !p.is_empty()) {
        Some(p) => ChartColor::to_plotters_rgb(p[i % p.len()]),
        None => PALETTE[i % PALETTE.len()],
    }
}

fn series_color(
    chart_style: Option<&ChartStyleV>,
    explicit: Option<ChartColor>,
    i: usize,
) -> RGBColor {
    match explicit {
        Some(c) => ChartColor::to_plotters_rgb(c),
        None => palette_color(chart_style, i),
    }
}

fn text_style(size: f64, color: Option<ChartColor>) -> TextStyle<'static> {
    let mut style = TextStyle::from(("sans-serif", size).into_font());
    if let Some(c) = color {
        style.color = ChartColor::to_plotters_rgb(c).to_backend_color();
    }
    style
}

/// Draw the series legend if any dataset has a label.
macro_rules! draw_legend {
    ($chart:expr, $self:expr, $chart_style:expr, $label_sz:expr) => {{
        let has_labels = $self.datasets.iter().any(|ds| ds.label().is_some());
        if has_labels {
            let cs: Option<&ChartStyleV> = $chart_style;
            let legend_pos = cs
                .and_then(|s| s.legend_position.as_ref())
                .map(|p| p.0.clone())
                .unwrap_or(SeriesLabelPosition::UpperLeft);
            let ls = cs.and_then(|s| s.legend.as_ref());
            let legend_bg = ls
                .and_then(|s| s.background)
                .map(ChartColor::to_plotters_rgb)
                .unwrap_or(WHITE);
            let legend_border = ls
                .and_then(|s| s.border)
                .map(ChartColor::to_plotters_rgb)
                .unwrap_or(BLACK);
            let legend_font_sz = ls.and_then(|s| s.label_size).unwrap_or($label_sz);
            let mut labels = $chart.configure_series_labels();
            labels.position(legend_pos);
            labels.margin(15);
            labels.background_style(legend_bg.mix(0.8));
            labels.border_style(legend_border);
            labels.label_font(text_style(legend_font_sz, ls.and_then(|s| s.label_color)));
            if let Err(e) = labels.draw() {
                error!("chart series labels draw: {e:?}");
            }
        }
    }};
}

const DEFAULT_GAIN: RGBColor = RGBColor(44, 160, 44);
const DEFAULT_LOSS: RGBColor = RGBColor(214, 39, 40);

/// A lone point has no segment to show it, so it gets a marker by default.
pub(crate) fn marker_size(point_size: Option<f64>, len: usize) -> u32 {
    match point_size {
        Some(p) => p as u32,
        None if len == 1 => 3,
        None => 0,
    }
}

macro_rules! draw_markers {
    ($chart:expr, $pts:expr, $ps:expr, $style:expr, $what:literal) => {
        if $ps > 0 {
            let series = $pts.map(|c| Circle::new(c, $ps, $style));
            if let Err(e) = $chart.draw_series(series) {
                error!("chart draw {} markers: {e:?}", $what);
            }
        }
    };
}

/// Draw series data onto a chart context, parameterized by x coordinate type.
macro_rules! draw_chart_body {
    ($chart:expr, $self:expr, $chart_style:expr, $xy_variant:path,
     $ohlc_variant:path, $eb_variant:path, $label_sz:expr) => {{
        let chart_style: Option<&ChartStyleV> = $chart_style;
        // CR claude for claude: [bug] Every sample is handed to plotters whatever the
        // visible range, and plotters clamps each mapped point into the plot rect
        // (Rect::truncate). So with an x_range/y_range narrower than the data, or after
        // any pan or zoom, out-of-view scatter points are drawn on the border, and line
        // and area segments bend toward the clamped corner. A point about 2^31 px
        // outside the view overflows plotters' i32 `limit.0 + offset`. In debug builds
        // that is a panic (one -1e7 sample under y_range {0, 1}, or about 160 wheel
        // zoom steps); in release the add wraps and the point is drawn on the opposite
        // edge. Clip each series to the view in data space before drawing: split
        // segments at the edge, and drop out-of-view markers, candles and error bars.
        // Also bound how far handle_scroll can zoom in. probe:
        // design/review-2026-10-05/repro/gui-chart-02.rs (gui-chart-02)
        for (i, ds) in $self.datasets.iter().enumerate() {
            match ds {
                DatasetEntry::XY { kind, data, style } => {
                    let pts = match data.t.as_ref() {
                        Some($xy_variant(p)) => p,
                        _ => continue,
                    };
                    let color = series_color(chart_style, style.color, i);
                    let sw = style.stroke_width.unwrap_or(2.0) as u32;
                    let line_style = ShapeStyle::from(color).stroke_width(sw);
                    let fill_style = ShapeStyle::from(color).filled();
                    let label = style.label.as_deref();

                    match kind {
                        XYKind::Line => {
                            let ps = marker_size(style.point_size, pts.len());
                            let series = LineSeries::new(pts.iter().copied(), line_style);
                            match $chart.draw_series(series) {
                                Ok(ann) => {
                                    if let Some(l) = label {
                                        ann.label(l).legend(move |(x, y)| {
                                            PathElement::new(
                                                [(x, y), (x + 20, y)],
                                                line_style,
                                            )
                                        });
                                    }
                                }
                                Err(e) => error!("chart draw line: {e:?}"),
                            }
                            draw_markers!(
                                $chart,
                                pts.iter().copied(),
                                ps,
                                fill_style,
                                "line"
                            );
                        }
                        XYKind::Scatter => {
                            let ps = style.point_size.unwrap_or(3.0) as u32;
                            let series = pts
                                .iter()
                                .map(|&(x, y)| Circle::new((x, y), ps, fill_style));
                            match $chart.draw_series(series) {
                                Ok(ann) => {
                                    if let Some(l) = label {
                                        ann.label(l).legend(move |(x, y)| {
                                            Circle::new((x, y), ps, fill_style)
                                        });
                                    }
                                }
                                Err(e) => error!("chart draw scatter: {e:?}"),
                            }
                        }
                        XYKind::Area => {
                            let ps = marker_size(style.point_size, pts.len());
                            let area_fill = color.mix(0.3);
                            let series = AreaSeries::new(
                                pts.iter().copied(),
                                0.0,
                                ShapeStyle::from(area_fill).filled(),
                            )
                            .border_style(line_style);
                            match $chart.draw_series(series) {
                                Ok(ann) => {
                                    if let Some(l) = label {
                                        ann.label(l).legend(move |(x, y)| {
                                            PathElement::new(
                                                [(x, y), (x + 20, y)],
                                                line_style,
                                            )
                                        });
                                    }
                                }
                                Err(e) => error!("chart draw area: {e:?}"),
                            }
                            draw_markers!(
                                $chart,
                                pts.iter().copied(),
                                ps,
                                fill_style,
                                "area"
                            );
                        }
                    }
                }

                DatasetEntry::DashedLine { data, dash, gap, style } => {
                    let pts = match data.t.as_ref() {
                        Some($xy_variant(p)) => p,
                        _ => continue,
                    };
                    let color = series_color(chart_style, style.color, i);
                    let sw = style.stroke_width.unwrap_or(2.0) as u32;
                    let ps = marker_size(style.point_size, pts.len());
                    let line_style = ShapeStyle::from(color).stroke_width(sw);
                    let fill_style = ShapeStyle::from(color).filled();
                    let label = style.label.as_deref();

                    let series = DashedLineSeries::new(
                        pts.iter().copied(),
                        *dash as u32,
                        *gap as u32,
                        line_style,
                    );
                    match $chart.draw_series(series) {
                        Ok(ann) => {
                            if let Some(l) = label {
                                ann.label(l).legend(move |(x, y)| {
                                    PathElement::new([(x, y), (x + 20, y)], line_style)
                                });
                            }
                        }
                        Err(e) => error!("chart draw dashed: {e:?}"),
                    }
                    draw_markers!($chart, pts.iter().copied(), ps, fill_style, "dashed");
                }

                // Rendered by their own ChartMode paths.
                DatasetEntry::Bar { .. }
                | DatasetEntry::Pie { .. }
                | DatasetEntry::Scatter3D { .. }
                | DatasetEntry::Line3D { .. }
                | DatasetEntry::Surface { .. } => {}

                DatasetEntry::Candlestick { data, style } => {
                    let gain = style
                        .gain_color
                        .map(ChartColor::to_plotters_rgb)
                        .unwrap_or(DEFAULT_GAIN);
                    let loss = style
                        .loss_color
                        .map(ChartColor::to_plotters_rgb)
                        .unwrap_or(DEFAULT_LOSS);
                    let bw = style.bar_width.unwrap_or(5.0) as u32;
                    let label = style.label.as_deref();

                    match data.t.as_ref() {
                        Some($ohlc_variant(pts)) => {
                            let series = pts.iter().map(|pt| {
                                CandleStick::new(
                                    pt.x,
                                    pt.open,
                                    pt.high,
                                    pt.low,
                                    pt.close,
                                    ShapeStyle::from(gain).filled(),
                                    ShapeStyle::from(loss).filled(),
                                    bw,
                                )
                            });
                            match $chart.draw_series(series) {
                                Ok(ann) => {
                                    if let Some(l) = label {
                                        let gain_style = ShapeStyle::from(gain).filled();
                                        ann.label(l).legend(move |(x, y)| {
                                            plotters::element::Rectangle::new(
                                                [(x, y - 5), (x + 20, y + 5)],
                                                gain_style,
                                            )
                                        });
                                    }
                                }
                                Err(e) => error!("chart draw candlestick: {e:?}"),
                            }
                        }
                        _ => continue,
                    }
                }

                DatasetEntry::ErrorBar { data, style } => {
                    let color = series_color(chart_style, style.color, i);
                    let sw = style.stroke_width.unwrap_or(2.0) as u32;
                    let line_style = ShapeStyle::from(color).stroke_width(sw);
                    let label = style.label.as_deref();

                    match data.t.as_ref() {
                        Some($eb_variant(pts)) => {
                            let series = pts.iter().map(|pt| {
                                ErrorBar::new_vertical(
                                    pt.x, pt.min, pt.avg, pt.max, line_style, sw,
                                )
                            });
                            match $chart.draw_series(series) {
                                Ok(ann) => {
                                    if let Some(l) = label {
                                        ann.label(l).legend(move |(x, y)| {
                                            PathElement::new(
                                                [(x, y), (x + 20, y)],
                                                line_style,
                                            )
                                        });
                                    }
                                }
                                Err(e) => error!("chart draw errorbar: {e:?}"),
                            }
                        }
                        _ => continue,
                    }
                }
            }
        }

        draw_legend!($chart, $self, chart_style, $label_sz);
    }};
}

/// Set up the mesh on a chart context. Shared between numeric and datetime modes.
macro_rules! configure_mesh {
    ($chart:expr, $x_label:expr, $y_label:expr, $mesh_style:expr) => {{
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
                let pc = ChartColor::to_plotters_rgb(c);
                mesh_cfg.light_line_style(pc);
            }
            if let Some(c) = ms.bold_line_color {
                let pc = ChartColor::to_plotters_rgb(c);
                mesh_cfg.bold_line_style(pc);
            }
            if let Some(c) = ms.axis_color {
                let pc = ChartColor::to_plotters_rgb(c);
                mesh_cfg.axis_style(pc);
            }
            if ms.label_size.is_some() || ms.label_color.is_some() {
                let style = text_style(ms.label_size.unwrap_or(12.0), ms.label_color);
                mesh_cfg.label_style(style.clone());
                mesh_cfg.axis_desc_style(style);
            }
            // CR claude for claude: [bug] These mesh counts are unchecked i64 values cast
            // to usize, here and in the 3D axes at 865-882. Two values cause a panic on
            // the first draw. `x_light_lines: 0` (the book calls it "bold lines only")
            // or `x_labels: 0` panics any time-series chart: plotters' datetime key
            // points get a 0 hint, which overflows a pow in debug and divides by zero
            // in release. The same values panic a one-category bar chart in
            // `step_by(0)`. A negative 2D count panics on overflow in debug and
            // allocates without bound in release, and a negative 3D label count loops
            // forever. The GUI draws on the main thread, so the whole program dies.
            // Probe: design/review-2026-10-05/repro/gui-chart-04.rs (an integration
            // test; the command is in its header). (gui-chart-04)
            if let Some(n) = ms.x_labels {
                mesh_cfg.x_labels(n as usize);
            }
            if let Some(n) = ms.y_labels {
                mesh_cfg.y_labels(n as usize);
            }
            if let Some(n) = ms.x_light_lines {
                mesh_cfg.x_max_light_lines(n as usize);
            }
            if let Some(n) = ms.y_light_lines {
                mesh_cfg.y_max_light_lines(n as usize);
            }
        }
        if let Err(e) = mesh_cfg.draw() {
            error!("chart mesh draw: {e:?}");
            return;
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
        let mode = chart_mode(&self.datasets);
        state.mouse_interaction(mode, bounds, cursor)
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

        let chart_geom = state.cache.draw(renderer, bounds.size(), |frame| {
            let w = frame.width() as u32;
            let h = frame.height() as u32;
            if w == 0 || h == 0 {
                return;
            }

            let mode = chart_mode(&self.datasets);
            if mode == ChartMode::Empty {
                return;
            }

            let backend = IcedBackend::new(frame, w, h);
            let root = backend.into_drawing_area();

            let chart_style = self.style.t.as_ref().and_then(|s| s.0.as_ref());
            let bg = chart_style
                .and_then(|s| s.background)
                .map(ChartColor::to_plotters_rgb)
                .unwrap_or(WHITE);
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

            let mut builder = ChartBuilder::on(&root);
            builder.margin(margin as u32);
            if let Some(t) = title {
                let title_color = chart_style.and_then(|s| s.title_color);
                builder.caption(t, text_style(title_size, title_color));
            }

            let y_range_opt = self.y_range.t.as_ref().and_then(|r| r.0.as_ref());

            match mode {
                ChartMode::Numeric => {
                    let (auto_x, auto_y) = compute_ranges(&self.datasets);
                    let base_x = match self.x_range.t.as_ref().and_then(|r| r.0.as_ref())
                    {
                        Some(XAxisRange::Numeric { min, max }) => (*min, *max),
                        _ => auto_x,
                    };
                    let base_y = match y_range_opt {
                        Some(r) => (r.min, r.max),
                        None => auto_y,
                    };
                    let (x_min, x_max) = state.x_view.unwrap_or(base_x);
                    let (y_min, y_max) = state.y_view.unwrap_or(base_y);

                    // CR claude for claude: [structure] The Numeric, TimeSeries and Bar
                    // arms repeat the label-area sizing (480-500, 558-578, 646-666; the
                    // copies differ only in the x padding constants) and the PlotInfo
                    // construction (508-518, 586-596, 676-686). The draw_series match
                    // that adds a label and a legend is written out eleven times, with
                    // one of three legend shapes. DatasetEntry::DashedLine is XY plus a
                    // dash and a gap, so every match on DatasetEntry in mod.rs,
                    // ranges.rs, dataset.rs and interact.rs needs an `XY | DashedLine`
                    // arm, and draw.rs repeats the line arm for it. compile_datasets'
                    // eleven arms also repeat the same compile_ref/TRef::new/context
                    // statement. One label-area helper, one PlotInfo helper, one legend
                    // helper, an XYKind::Dashed { dash, gap } and one data-compiling
                    // helper would remove the copies. (gui-chart-13)
                    let (_, tick_h) = estimate_text("0", label_sz as f64);
                    let prec = tick_precision(y_max - y_min);
                    let y_min_s = format!("{y_min:.prec$}");
                    let y_max_s = format!("{y_max:.prec$}");
                    let widest =
                        if y_min_s.len() > y_max_s.len() { &y_min_s } else { &y_max_s };
                    let (tick_w, _) = estimate_text(widest, label_sz as f64);
                    let auto_y_area =
                        if y_label.is_some() { tick_w + tick_h + 15 } else { tick_w + 8 };
                    let auto_x_area =
                        if x_label.is_some() { tick_h * 2 + 15 } else { tick_h + 8 };
                    let x_area = mesh_style
                        .and_then(|ms| ms.x_label_area_size)
                        .map(|s| s as u32)
                        .unwrap_or(auto_x_area);
                    let y_area = mesh_style
                        .and_then(|ms| ms.y_label_area_size)
                        .map(|s| s as u32)
                        .unwrap_or(auto_y_area);
                    builder.x_label_area_size(x_area);
                    builder.y_label_area_size(y_area);

                    let mut chart =
                        match builder.build_cartesian_2d(x_min..x_max, y_min..y_max) {
                            Ok(c) => c,
                            Err(_) => return,
                        };

                    let (px, py) = chart.plotting_area().get_pixel_range();
                    state.plot_info.set(Some(PlotInfo {
                        rect: iced_core::Rectangle {
                            x: px.start as f32,
                            y: py.start as f32,
                            width: (px.end - px.start) as f32,
                            height: (py.end - py.start) as f32,
                        },
                        x_range: (x_min, x_max),
                        y_range: (y_min, y_max),
                    }));

                    configure_mesh!(chart, x_label, y_label, mesh_style);
                    draw_chart_body!(
                        chart,
                        self,
                        chart_style,
                        XYData::Numeric,
                        OHLCData::Numeric,
                        EBData::Numeric,
                        label_sz
                    );
                }

                ChartMode::TimeSeries => {
                    let (auto_x, auto_y) = compute_time_ranges(&self.datasets);
                    let base_x_dt =
                        match self.x_range.t.as_ref().and_then(|r| r.0.as_ref()) {
                            Some(XAxisRange::DateTime { min, max }) => (*min, *max),
                            _ => auto_x,
                        };
                    let base_y = match y_range_opt {
                        Some(r) => (r.min, r.max),
                        None => auto_y,
                    };

                    let base_x_ms = (
                        base_x_dt.0.timestamp_millis() as f64,
                        base_x_dt.1.timestamp_millis() as f64,
                    );
                    let effective_x_ms = state.x_view.unwrap_or(base_x_ms);
                    let (y_min, y_max) = state.y_view.unwrap_or(base_y);

                    let x_min =
                        chrono::DateTime::from_timestamp_millis(effective_x_ms.0 as i64)
                            .unwrap_or(base_x_dt.0);
                    let x_max =
                        chrono::DateTime::from_timestamp_millis(effective_x_ms.1 as i64)
                            .unwrap_or(base_x_dt.1);

                    let (_, tick_h) = estimate_text("0", label_sz as f64);
                    let prec = tick_precision(y_max - y_min);
                    let y_min_s = format!("{y_min:.prec$}");
                    let y_max_s = format!("{y_max:.prec$}");
                    let widest =
                        if y_min_s.len() > y_max_s.len() { &y_min_s } else { &y_max_s };
                    let (tick_w, _) = estimate_text(widest, label_sz as f64);
                    let auto_y_area =
                        if y_label.is_some() { tick_w + tick_h + 15 } else { tick_w + 8 };
                    let auto_x_area =
                        if x_label.is_some() { tick_h * 2 + 20 } else { tick_h + 12 };
                    let x_area = mesh_style
                        .and_then(|ms| ms.x_label_area_size)
                        .map(|s| s as u32)
                        .unwrap_or(auto_x_area);
                    let y_area = mesh_style
                        .and_then(|ms| ms.y_label_area_size)
                        .map(|s| s as u32)
                        .unwrap_or(auto_y_area);
                    builder.x_label_area_size(x_area);
                    builder.y_label_area_size(y_area);

                    let mut chart =
                        match builder.build_cartesian_2d(x_min..x_max, y_min..y_max) {
                            Ok(c) => c,
                            Err(_) => return,
                        };

                    let (px, py) = chart.plotting_area().get_pixel_range();
                    state.plot_info.set(Some(PlotInfo {
                        rect: iced_core::Rectangle {
                            x: px.start as f32,
                            y: py.start as f32,
                            width: (px.end - px.start) as f32,
                            height: (py.end - py.start) as f32,
                        },
                        x_range: effective_x_ms,
                        y_range: (y_min, y_max),
                    }));

                    configure_mesh!(chart, x_label, y_label, mesh_style);
                    draw_chart_body!(
                        chart,
                        self,
                        chart_style,
                        XYData::DateTime,
                        OHLCData::DateTime,
                        EBData::DateTime,
                        label_sz
                    );
                }

                ChartMode::Bar => {
                    let mut categories: Vec<String> = Vec::new();
                    let mut y_min = f64::INFINITY;
                    let mut y_max = f64::NEG_INFINITY;
                    for ds in self.datasets.iter() {
                        if let DatasetEntry::Bar { data, .. } = ds {
                            if let Some(bd) = data.t.as_ref() {
                                for (cat, val) in bd.0.iter() {
                                    if !categories.iter().any(|c| c == cat) {
                                        categories.push(cat.clone());
                                    }
                                    if *val < y_min {
                                        y_min = *val;
                                    }
                                    if *val > y_max {
                                        y_max = *val;
                                    }
                                }
                            }
                        }
                    }
                    if categories.is_empty() {
                        return;
                    }
                    if y_min > 0.0 {
                        y_min = 0.0;
                    }
                    if y_max < 0.0 {
                        y_max = 0.0;
                    }
                    let base_y = match y_range_opt {
                        Some(r) => (r.min, r.max),
                        None => pad_range(y_min, y_max),
                    };
                    let (y_min, y_max) = state.y_view.unwrap_or(base_y);

                    let (_, tick_h) = estimate_text("0", label_sz as f64);
                    let prec = tick_precision(y_max - y_min);
                    let y_min_s = format!("{y_min:.prec$}");
                    let y_max_s = format!("{y_max:.prec$}");
                    let widest =
                        if y_min_s.len() > y_max_s.len() { &y_min_s } else { &y_max_s };
                    let (tick_w, _) = estimate_text(widest, label_sz as f64);
                    let auto_y_area =
                        if y_label.is_some() { tick_w + tick_h + 15 } else { tick_w + 8 };
                    let auto_x_area =
                        if x_label.is_some() { tick_h * 2 + 15 } else { tick_h + 8 };
                    let x_area = mesh_style
                        .and_then(|ms| ms.x_label_area_size)
                        .map(|s| s as u32)
                        .unwrap_or(auto_x_area);
                    let y_area = mesh_style
                        .and_then(|ms| ms.y_label_area_size)
                        .map(|s| s as u32)
                        .unwrap_or(auto_y_area);
                    builder.x_label_area_size(x_area);
                    builder.y_label_area_size(y_area);

                    let mut chart = match builder.build_cartesian_2d(
                        categories.as_slice().into_segmented(),
                        y_min..y_max,
                    ) {
                        Ok(c) => c,
                        Err(_) => return,
                    };

                    let (px, py) = chart.plotting_area().get_pixel_range();
                    state.plot_info.set(Some(PlotInfo {
                        rect: iced_core::Rectangle {
                            x: px.start as f32,
                            y: py.start as f32,
                            width: (px.end - px.start) as f32,
                            height: (py.end - py.start) as f32,
                        },
                        x_range: (0.0, categories.len() as f64),
                        y_range: (y_min, y_max),
                    }));

                    configure_mesh!(chart, x_label, y_label, mesh_style);

                    for (i, ds) in self.datasets.iter().enumerate() {
                        if let DatasetEntry::Bar { data, style } = ds {
                            if let Some(bd) = data.t.as_ref() {
                                let color = series_color(chart_style, style.color, i);
                                let fill_style = ShapeStyle::from(color).filled();
                                let margin_px = style.margin.unwrap_or(5.0) as u32;
                                let hist = Histogram::vertical(&chart)
                                    .style(fill_style)
                                    .margin(margin_px)
                                    .data(bd.0.iter().map(|(cat, val)| (cat, *val)));
                                match chart.draw_series(hist) {
                                    Ok(ann) => {
                                        if let Some(l) = style.label.as_deref() {
                                            ann.label(l).legend(move |(x, y)| {
                                                plotters::element::Rectangle::new(
                                                    [(x, y - 5), (x + 20, y + 5)],
                                                    fill_style,
                                                )
                                            });
                                        }
                                    }
                                    Err(e) => error!("chart draw bar: {e:?}"),
                                }
                            }
                        }
                    }

                    draw_legend!(chart, self, chart_style, label_sz);
                }

                ChartMode::Pie => {
                    let (pie_data, pie_style) =
                        match self.datasets.iter().find_map(|ds| {
                            if let DatasetEntry::Pie { data, style } = ds {
                                data.t.as_ref().map(|d| (d, style))
                            } else {
                                None
                            }
                        }) {
                            Some(v) => v,
                            None => return,
                        };

                    let title_h = if title.is_some() {
                        let (_, th) =
                            estimate_text(title.unwrap_or(""), title_size as f64);
                        th + margin as u32
                    } else {
                        0
                    };

                    let center_x = (w / 2) as i32;
                    let center_y = ((h + title_h) / 2) as i32;
                    // CR claude for claude: [bug] title_h is the estimated title height
                    // plus the margin (29 px at the defaults), and `h - title_h` is
                    // u32. A titled pie chart shorter than that (#height:
                    // &`Fixed(20.0), or a window resized small) panics a debug build
                    // here with 'attempt to subtract with overflow'. A release build
                    // wraps instead, takes the radius from the width and puts the
                    // centre below the frame. Use h.saturating_sub(title_h) and skip
                    // the pie when nothing is left. probe:
                    // design/review-2026-10-05/repro/gui-chart-15.rs (gui-chart-15)
                    let radius = (w.min(h - title_h) as f64 * 0.35).max(10.0);

                    let pie_labels: Vec<String> =
                        pie_data.0.iter().map(|(l, _)| l.clone()).collect();
                    // CR claude for claude: [bug] These values reach plotters' Pie
                    // unchecked, and Pie::draw loops `while offset_theta <=
                    // theta_final` with theta_final = slice / total * 2π + offset. When
                    // the values sum to zero and the first nonzero one is positive,
                    // theta_final is inf, so the draw never returns and pushes points
                    // until the process runs out of memory, on the GUI's main thread. A
                    // total near zero ([1.0, -0.999999]) takes about 1.8e9 iterations
                    // (~15 GB), and a non-finite #start_angle hangs the same loop with
                    // ordinary data. The tooltip already skips total <= 0
                    // (interact.rs:481); the draw needs the same guard over finite
                    // non-negative slices, and a finite start angle taken mod 360.
                    // probe: design/review-2026-10-05/repro/gui-chart-03.rs, which
                    // draws chart(&[pie(&[("in", 100.0), ("out", -100.0)])])
                    // headlessly. (gui-chart-03)
                    let sizes: Vec<f64> = pie_data.0.iter().map(|(_, v)| *v).collect();
                    // CR claude for claude: [bug] A #colors array shorter than the data
                    // goes to plotters' Pie unchanged. Pie::draw returns LengthMismatch
                    // at the first slice without a colour (logged as "chart draw pie"),
                    // so only the slices before it are drawn: #colors: [red] over three
                    // slices paints one 60-degree wedge, and #colors: [] paints
                    // nothing. Give every slice a colour: cycle the given ones as the
                    // palette path does, or fall back to the palette when the array is
                    // empty. probe: design/review-2026-10-05/repro/gui-chart-16.rs
                    // (gui-chart-16)
                    let colors: Vec<RGBColor> = match &pie_style.colors {
                        Some(cs) => {
                            cs.iter().map(|c| ChartColor::to_plotters_rgb(*c)).collect()
                        }
                        None => (0..sizes.len())
                            .map(|i| palette_color(chart_style, i))
                            .collect(),
                    };
                    let label_strs: Vec<&str> =
                        pie_labels.iter().map(|s| s.as_str()).collect();

                    state.plot_info.set(Some(PlotInfo {
                        rect: iced_core::Rectangle {
                            x: center_x as f32 - radius as f32,
                            y: center_y as f32 - radius as f32,
                            width: radius as f32 * 2.0,
                            height: radius as f32 * 2.0,
                        },
                        x_range: (0.0, 1.0),
                        y_range: (0.0, 1.0),
                    }));

                    let center = (center_x, center_y);
                    let mut pie =
                        Pie::new(&center, &radius, &sizes, &colors, &label_strs);
                    pie.label_style(("sans-serif", label_sz).into_font());
                    if let Some(angle) = pie_style.start_angle {
                        pie.start_angle(angle);
                    }
                    // CR claude for claude: [bug] The book (chart.md, PieStyle) documents
                    // `donut` as the inner radius as a fraction of the outer radius
                    // (0.0-1.0). Plotters' `donut_hole` takes a hole radius in pixels
                    // and ignores anything not strictly between 0 and `radius`. So
                    // `#donut: 0.5` draws a full pie, the whole documented range gives
                    // at most a 1 px hole, and `#donut: 52.0` is a 52 px hole whatever
                    // size the pie is drawn at. Pass `hole.clamp(0.0, 1.0) * radius`
                    // instead. `label_offset` below has the same problem: the book
                    // calls it a percentage, but plotters adds it to the radius in
                    // pixels (`#label_offset: 50.0` moves the labels 50 px at every
                    // size), so scale it by the radius or document pixels. probe:
                    // design/review-2026-10-05/repro/gui-chart-11.rs (copy to
                    // stdlib/graphix-package-gui/tests/review_gui_chart_11.rs; cargo
                    // test -p graphix-package-gui --test review_gui_chart_11 --
                    // --nocapture) (gui-chart-11)
                    if let Some(hole) = pie_style.donut {
                        pie.donut_hole(hole);
                    }
                    if pie_style.show_percentages == Some(true) {
                        pie.percentages(("sans-serif", label_sz * 0.9).into_font());
                    }
                    if let Some(offset) = pie_style.label_offset {
                        pie.label_offset(offset);
                    }
                    if let Err(e) = root.draw(&pie) {
                        error!("chart draw pie: {e:?}");
                    }
                }

                ChartMode::ThreeD => {
                    let (auto_x, auto_y, auto_z) = compute_3d_ranges(&self.datasets);
                    let (x_min, x_max) =
                        match self.x_range.t.as_ref().and_then(|r| r.0.as_ref()) {
                            Some(XAxisRange::Numeric { min, max }) => (*min, *max),
                            _ => auto_x,
                        };
                    let (y_min, y_max) = match y_range_opt {
                        Some(r) => (r.min, r.max),
                        None => auto_y,
                    };
                    let z_range_opt = self.z_range.t.as_ref().and_then(|r| r.0.as_ref());
                    let (z_min, z_max) = match z_range_opt {
                        Some(r) => (r.min, r.max),
                        None => auto_z,
                    };

                    let x_area = mesh_style
                        .and_then(|ms| ms.x_label_area_size)
                        .map(|s| s as u32)
                        .unwrap_or(30);
                    let y_area = mesh_style
                        .and_then(|ms| ms.y_label_area_size)
                        .map(|s| s as u32)
                        .unwrap_or(30);
                    builder.x_label_area_size(x_area);
                    builder.y_label_area_size(y_area);

                    let mut chart = match builder.build_cartesian_3d(
                        x_min..x_max,
                        z_min..z_max,
                        y_min..y_max,
                    ) {
                        Ok(c) => c,
                        Err(_) => return,
                    };

                    let proj = self.projection.t.as_ref().and_then(|o| o.0.as_ref());
                    let yaw_offset = state.yaw_offset;
                    let pitch_offset = state.pitch_offset;
                    let scale_factor = state.scale_factor;
                    chart.with_projection(|mut pb| {
                        if let Some(p) = proj {
                            if let Some(yaw) = p.yaw {
                                pb.yaw = yaw;
                            }
                            if let Some(pitch) = p.pitch {
                                pb.pitch = pitch;
                            }
                            if let Some(scale) = p.scale {
                                pb.scale = scale;
                            }
                        }
                        pb.yaw += yaw_offset;
                        pb.pitch += pitch_offset;
                        pb.scale *= scale_factor;
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
                                axes.light_grid_style(ChartColor::to_plotters_rgb(c));
                            }
                            if let Some(c) = ms.bold_line_color {
                                axes.bold_grid_style(ChartColor::to_plotters_rgb(c));
                            }
                            if let Some(n) = ms.x_labels {
                                axes.x_labels(n as usize);
                            }
                            if let Some(n) = ms.y_labels {
                                axes.z_labels(n as usize);
                            }
                            if let Some(n) = ms.z_labels {
                                axes.y_labels(n as usize);
                            }
                            if let Some(n) = ms.x_light_lines {
                                axes.x_max_light_lines(n as usize);
                            }
                            if let Some(n) = ms.y_light_lines {
                                axes.z_max_light_lines(n as usize);
                            }
                            if let Some(n) = ms.z_light_lines {
                                axes.y_max_light_lines(n as usize);
                            }
                        }
                        let x_pfx = x_label.map(|l| format!("{l}: "));
                        let y_pfx = y_label.map(|l| format!("{l}: "));
                        let z_label_str =
                            self.z_label.t.as_ref().and_then(|o| o.as_deref());
                        let z_pfx = z_label_str.map(|l| format!("{l}: "));
                        let x_fn = |x: &f64| match &x_pfx {
                            Some(pfx) => format!("{pfx}{x:.1}"),
                            None => format!("{x:.1}"),
                        };
                        let y_fn = |y: &f64| match &z_pfx {
                            Some(pfx) => format!("{pfx}{y:.1}"),
                            None => format!("{y:.1}"),
                        };
                        let z_fn = |z: &f64| match &y_pfx {
                            Some(pfx) => format!("{pfx}{z:.1}"),
                            None => format!("{z:.1}"),
                        };
                        axes.x_formatter(&x_fn);
                        axes.y_formatter(&y_fn);
                        axes.z_formatter(&z_fn);
                        if let Err(e) = axes.draw() {
                            error!("chart 3d axes draw: {e:?}");
                        }
                    }

                    for (i, ds) in self.datasets.iter().enumerate() {
                        match ds {
                            DatasetEntry::Scatter3D { data, style } => {
                                if let Some(pts) = data.t.as_ref() {
                                    let color = series_color(chart_style, style.color, i);
                                    let ps = style.point_size.unwrap_or(3.0) as u32;
                                    let fill_style = ShapeStyle::from(color).filled();
                                    let series = pts.0.iter().map(|&(x, y, z)| {
                                        Circle::new((x, z, y), ps, fill_style)
                                    });
                                    match chart.draw_series(series) {
                                        Ok(ann) => {
                                            if let Some(l) = style.label.as_deref() {
                                                ann.label(l).legend(move |(x, y)| {
                                                    Circle::new((x, y), ps, fill_style)
                                                });
                                            }
                                        }
                                        Err(e) => error!("chart draw scatter3d: {e:?}"),
                                    }
                                }
                            }
                            DatasetEntry::Line3D { data, style } => {
                                if let Some(pts) = data.t.as_ref() {
                                    let color = series_color(chart_style, style.color, i);
                                    let sw = style.stroke_width.unwrap_or(2.0) as u32;
                                    let ps = marker_size(style.point_size, pts.0.len());
                                    let line_style =
                                        ShapeStyle::from(color).stroke_width(sw);
                                    let fill_style = ShapeStyle::from(color).filled();
                                    let series = LineSeries::new(
                                        pts.0.iter().map(|&(x, y, z)| (x, z, y)),
                                        line_style,
                                    );
                                    match chart.draw_series(series) {
                                        Ok(ann) => {
                                            if let Some(l) = style.label.as_deref() {
                                                ann.label(l).legend(move |(x, y)| {
                                                    PathElement::new(
                                                        [(x, y), (x + 20, y)],
                                                        line_style,
                                                    )
                                                });
                                            }
                                        }
                                        Err(e) => {
                                            error!("chart draw line3d: {e:?}")
                                        }
                                    }
                                    draw_markers!(
                                        chart,
                                        pts.0.iter().map(|&(x, y, z)| (x, z, y)),
                                        ps,
                                        fill_style,
                                        "line3d"
                                    );
                                }
                            }
                            DatasetEntry::Surface { data, style } => {
                                if let Some(grid) = data.t.as_ref() {
                                    if grid.0.is_empty() || grid.0[0].is_empty() {
                                        continue;
                                    }
                                    let color = series_color(chart_style, style.color, i);
                                    let color_by_z = style.color_by_z.unwrap_or(false);

                                    let x_vals: Vec<f64> = grid
                                        .0
                                        .iter()
                                        .filter(|row| !row.is_empty())
                                        .map(|row| row[0].0)
                                        .collect();
                                    let y_vals: Vec<f64> =
                                        grid.0[0].iter().map(|pt| pt.1).collect();

                                    // SurfaceSeries::xoz calls back with the exact x/y
                                    // values supplied, so binary search finds the index.
                                    let ncols = y_vals.len();
                                    let z_grid: Vec<f64> = grid
                                        .0
                                        .iter()
                                        .filter(|row| !row.is_empty())
                                        .flat_map(|row| row.iter().map(|&(_, _, z)| z))
                                        .collect();
                                    // CR claude for claude: [bug] z_lookup finds a
                                    // point's row and column by binary search over
                                    // x_vals (each row's first x) and y_vals (row 0's
                                    // ys). That is only right when both axes ascend,
                                    // and nothing sorts the grid; the book documents no
                                    // order. On a descending axis of three or more
                                    // values the search misses and unwrap_or(0) reads
                                    // row 0 (column 0), so every row is drawn with the
                                    // first row's z. Rows at x = 2, 1, 0 with z = 5, 9,
                                    // 1 draw a flat plane at z = 5, and the book's
                                    // chart_3d paraboloid with its rows reversed draws
                                    // the x = 4 row extruded along x. The same rebuild
                                    // also ignores every point's own x/y past row[0]
                                    // and row 0, and shifts the flat index on ragged
                                    // rows; building each cell's polygon straight from
                                    // grid[i][j], grid[i][j+1], grid[i+1][j+1],
                                    // grid[i+1][j] avoids recovering indices from
                                    // values. probe:
                                    // design/review-2026-10-05/repro/gui-chart-08.rs
                                    // (screenshots of the descending grids are
                                    // pixel-identical to the first-row-z prediction).
                                    // (gui-chart-08)
                                    let z_lookup = |x: f64, y: f64| -> f64 {
                                        let ri = x_vals
                                            .binary_search_by(|v| {
                                                v.partial_cmp(&x)
                                                    .unwrap_or(std::cmp::Ordering::Equal)
                                            })
                                            .unwrap_or(0);
                                        let ci = y_vals
                                            .binary_search_by(|v| {
                                                v.partial_cmp(&y)
                                                    .unwrap_or(std::cmp::Ordering::Equal)
                                            })
                                            .unwrap_or(0);
                                        z_grid
                                            .get(ri * ncols + ci)
                                            .copied()
                                            .unwrap_or(0.0)
                                    };
                                    if color_by_z {
                                        let z_color = |z: &f64| {
                                            let t = if z_max > z_min {
                                                (z - z_min) / (z_max - z_min)
                                            } else {
                                                0.5
                                            };
                                            let hue = (1.0 - t) * 240.0;
                                            let (r, g, b) = hsl_to_rgb(hue, 0.8, 0.5);
                                            RGBColor(r, g, b).mix(0.6).filled()
                                        };
                                        let series = SurfaceSeries::xoz(
                                            x_vals.iter().copied(),
                                            y_vals.iter().copied(),
                                            |x, y| z_lookup(x, y),
                                        )
                                        .style_func(&z_color);
                                        match chart.draw_series(series) {
                                            Ok(ann) => {
                                                if let Some(l) = style.label.as_deref() {
                                                    let fill =
                                                        ShapeStyle::from(color).filled();
                                                    ann.label(l).legend(move |(x, y)| {
                                                        plotters::element::Rectangle::new(
                                                            [(x, y - 5), (x + 20, y + 5)],
                                                            fill,
                                                        )
                                                    });
                                                }
                                            }
                                            Err(e) => error!("chart draw surface: {e:?}"),
                                        }
                                    } else {
                                        let fill_style = color.mix(0.6).filled();
                                        let series = SurfaceSeries::xoz(
                                            x_vals.iter().copied(),
                                            y_vals.iter().copied(),
                                            |x, y| z_lookup(x, y),
                                        )
                                        .style(fill_style);
                                        match chart.draw_series(series) {
                                            Ok(ann) => {
                                                if let Some(l) = style.label.as_deref() {
                                                    ann.label(l).legend(move |(x, y)| {
                                                        plotters::element::Rectangle::new(
                                                            [(x, y - 5), (x + 20, y + 5)],
                                                            fill_style,
                                                        )
                                                    });
                                                }
                                            }
                                            Err(e) => error!("chart draw surface: {e:?}"),
                                        }
                                    }
                                }
                            }
                            _ => {}
                        }
                    }

                    draw_legend!(chart, self, chart_style, label_sz);
                }

                ChartMode::Empty => unreachable!(),
            }

            if let Err(e) = root.present() {
                error!("chart present: {e:?}");
            }
        });

        let mut result = vec![chart_geom];
        if let Some(snap) = &state.snap_point {
            let overlay = iced_canvas::Cache::new();
            let geom = overlay.draw(renderer, bounds.size(), |frame| {
                draw_tooltip(frame, snap, bounds.size());
            });
            result.push(geom);
        }
        result
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
