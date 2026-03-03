use super::dataset::{chart_mode, ChartMode, DatasetEntry, XYKind};
use super::interact::{draw_tooltip, ChartState, PlotInfo};
use super::plotters_backend::{estimate_text, IcedBackend};
use super::ranges::*;
use super::types::*;
use super::ChartW;
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
        Color as PlotColor, IntoFont, RGBColor, ShapeStyle, TextStyle, BLACK, WHITE,
    },
};

// ── Color palette & helpers ─────────────────────────────────────────

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

fn iced_to_plotters(c: iced_core::Color) -> RGBColor {
    let [r, g, b, _] = c.into_rgba8();
    RGBColor(r, g, b)
}

// ── Drawing macros ──────────────────────────────────────────────────

/// Draw series data onto a chart context. The macro is parameterized by
/// coordinate type to avoid duplicating the ~200-line series loop for
/// numeric vs. datetime x-axes.
macro_rules! draw_chart_body {
    ($chart:expr, $self:expr, $xy_variant:path, $ohlc_variant:path,
     $eb_variant:path, $label_sz:expr) => {{
        // Draw each dataset
        for (i, ds) in $self.datasets.iter().enumerate() {
            match ds {
                DatasetEntry::XY { kind, data, style } => {
                    let pts = match data.t.as_ref() {
                        Some($xy_variant(p)) => p,
                        _ => continue,
                    };
                    let color = style
                        .color
                        .map(iced_to_plotters)
                        .unwrap_or(PALETTE[i % PALETTE.len()]);
                    let sw = style.stroke_width.unwrap_or(2.0) as u32;
                    let ps = style.point_size.unwrap_or(3.0) as u32;
                    let line_style = ShapeStyle::from(color).stroke_width(sw);
                    let fill_style = ShapeStyle::from(color).filled();
                    let label = style.label.as_deref();

                    match kind {
                        XYKind::Line => {
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
                        }
                        XYKind::Scatter => {
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
                        }
                    }
                }

                DatasetEntry::DashedLine { data, dash, gap, style } => {
                    let pts = match data.t.as_ref() {
                        Some($xy_variant(p)) => p,
                        _ => continue,
                    };
                    let color = style
                        .color
                        .map(iced_to_plotters)
                        .unwrap_or(PALETTE[i % PALETTE.len()]);
                    let sw = style.stroke_width.unwrap_or(2.0) as u32;
                    let line_style = ShapeStyle::from(color).stroke_width(sw);
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
                }

                // These dataset types are rendered in their own ChartMode paths
                DatasetEntry::Bar { .. }
                | DatasetEntry::Pie { .. }
                | DatasetEntry::Scatter3D { .. }
                | DatasetEntry::Line3D { .. }
                | DatasetEntry::Surface { .. } => {}

                DatasetEntry::Candlestick { data, style } => {
                    let gain =
                        style.gain_color.map(iced_to_plotters).unwrap_or(DEFAULT_GAIN);
                    let loss =
                        style.loss_color.map(iced_to_plotters).unwrap_or(DEFAULT_LOSS);
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
                    let color = style
                        .color
                        .map(iced_to_plotters)
                        .unwrap_or(PALETTE[i % PALETTE.len()]);
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

        // Legend
        let has_labels = $self.datasets.iter().any(|ds| ds.label().is_some());
        if has_labels {
            let legend_pos = $self
                .legend_position
                .t
                .as_ref()
                .and_then(|o| o.0.as_ref())
                .map(|p| p.0.clone())
                .unwrap_or(SeriesLabelPosition::UpperLeft);
            let ls = $self.legend_style.t.as_ref().and_then(|o| o.0.as_ref());
            let legend_bg =
                ls.and_then(|s| s.background).map(iced_to_plotters).unwrap_or(WHITE);
            let legend_border =
                ls.and_then(|s| s.border).map(iced_to_plotters).unwrap_or(BLACK);
            let legend_font_sz = ls.and_then(|s| s.label_size).unwrap_or($label_sz);
            let mut labels = $chart.configure_series_labels();
            labels.position(legend_pos);
            labels.margin(15);
            labels.background_style(legend_bg.mix(0.8));
            labels.border_style(legend_border);
            let mut style = TextStyle::from(("sans-serif", legend_font_sz).into_font());
            if let Some(lc) = ls.and_then(|s| s.label_color) {
                style.color = iced_to_plotters(lc).to_backend_color();
            }
            labels.label_font(style);
            if let Err(e) = labels.draw() {
                error!("chart series labels draw: {e:?}");
            }
        }
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
                let pc = iced_to_plotters(c);
                mesh_cfg.light_line_style(pc);
            }
            if let Some(c) = ms.axis_color {
                let pc = iced_to_plotters(c);
                mesh_cfg.axis_style(pc);
            }
            if ms.label_size.is_some() || ms.label_color.is_some() {
                let s = ms.label_size.unwrap_or(12.0);
                if let Some(lc) = ms.label_color {
                    let mut style = TextStyle::from(("sans-serif", s).into_font());
                    style.color = iced_to_plotters(lc).to_backend_color();
                    mesh_cfg.label_style(style.clone());
                    mesh_cfg.axis_desc_style(style);
                } else {
                    mesh_cfg.label_style(("sans-serif", s).into_font());
                }
            }
            if let Some(n) = ms.x_labels {
                mesh_cfg.x_labels(n as usize);
            }
            if let Some(n) = ms.y_labels {
                mesh_cfg.y_labels(n as usize);
            }
        }
        if let Err(e) = mesh_cfg.draw() {
            error!("chart mesh draw: {e:?}");
            return;
        }
    }};
}

// ── Program impl ────────────────────────────────────────────────────

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
        // Check dirty flag from data updates
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

            // Background color
            let bg = self
                .background
                .t
                .as_ref()
                .and_then(|o| o.0)
                .map(iced_to_plotters)
                .unwrap_or(WHITE);
            if let Err(e) = root.fill(&bg) {
                error!("chart fill: {e:?}");
                return;
            }

            let title = self.title.t.as_ref().and_then(|o| o.as_deref());
            let x_label = self.x_label.t.as_ref().and_then(|o| o.as_deref());
            let y_label = self.y_label.t.as_ref().and_then(|o| o.as_deref());
            let margin = self.margin.t.as_ref().and_then(|o| o.0).unwrap_or(10.0);
            let title_size = self.title_size.t.as_ref().and_then(|o| o.0).unwrap_or(16.0);
            let mesh_style = self.mesh.t.as_ref().and_then(|m| m.0.as_ref());
            let label_sz = mesh_style.and_then(|ms| ms.label_size).unwrap_or(12.0);

            let mut builder = ChartBuilder::on(&root);
            builder.margin(margin as u32);
            if let Some(t) = title {
                let font = ("sans-serif", title_size).into_font();
                if let Some(tc) = self.title_color.t.as_ref().and_then(|o| o.0) {
                    let mut style = TextStyle::from(font);
                    style.color = iced_to_plotters(tc).to_backend_color();
                    builder.caption(t, style);
                } else {
                    builder.caption(t, font);
                }
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
                    // Apply zoom/pan overrides
                    let (x_min, x_max) = state.x_view.unwrap_or(base_x);
                    let (y_min, y_max) = state.y_view.unwrap_or(base_y);

                    // Compute label areas
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

                    // Capture plot area for interactivity
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

                    // For zoom/pan, work in millis
                    let base_x_ms = (
                        base_x_dt.0.timestamp_millis() as f64,
                        base_x_dt.1.timestamp_millis() as f64,
                    );
                    let effective_x_ms = state.x_view.unwrap_or(base_x_ms);
                    let (y_min, y_max) = state.y_view.unwrap_or(base_y);

                    // Convert back to DateTime
                    let x_min =
                        chrono::DateTime::from_timestamp_millis(effective_x_ms.0 as i64)
                            .unwrap_or(base_x_dt.0);
                    let x_max =
                        chrono::DateTime::from_timestamp_millis(effective_x_ms.1 as i64)
                            .unwrap_or(base_x_dt.1);

                    // Compute label areas — datetime ticks are wider
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

                    // Capture plot area for interactivity (store x as millis)
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
                        XYData::DateTime,
                        OHLCData::DateTime,
                        EBData::DateTime,
                        label_sz
                    );
                }

                ChartMode::Bar => {
                    // Collect unique categories and compute y-range
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
                    // Extend y-range to include 0
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

                    // Compute label areas
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

                    // Capture plot area for interactivity
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

                    // Draw each bar dataset
                    for (i, ds) in self.datasets.iter().enumerate() {
                        if let DatasetEntry::Bar { data, style } = ds {
                            if let Some(bd) = data.t.as_ref() {
                                let color = style
                                    .color
                                    .map(iced_to_plotters)
                                    .unwrap_or(PALETTE[i % PALETTE.len()]);
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

                    // Legend
                    let has_labels = self.datasets.iter().any(|ds| ds.label().is_some());
                    if has_labels {
                        let legend_pos = self
                            .legend_position
                            .t
                            .as_ref()
                            .and_then(|o| o.0.as_ref())
                            .map(|p| p.0.clone())
                            .unwrap_or(SeriesLabelPosition::UpperLeft);
                        let ls = self.legend_style.t.as_ref().and_then(|o| o.0.as_ref());
                        let legend_bg = ls
                            .and_then(|s| s.background)
                            .map(iced_to_plotters)
                            .unwrap_or(WHITE);
                        let legend_border = ls
                            .and_then(|s| s.border)
                            .map(iced_to_plotters)
                            .unwrap_or(BLACK);
                        let legend_font_sz =
                            ls.and_then(|s| s.label_size).unwrap_or(label_sz);
                        let mut labels = chart.configure_series_labels();
                        labels.position(legend_pos);
                        labels.margin(15);
                        labels.background_style(legend_bg.mix(0.8));
                        labels.border_style(legend_border);
                        let mut style =
                            TextStyle::from(("sans-serif", legend_font_sz).into_font());
                        if let Some(lc) = ls.and_then(|s| s.label_color) {
                            style.color = iced_to_plotters(lc).to_backend_color();
                        }
                        labels.label_font(style);
                        if let Err(e) = labels.draw() {
                            error!("chart series labels draw: {e:?}");
                        }
                    }
                }

                ChartMode::Pie => {
                    // Pie is drawn directly on the DrawingArea, no ChartBuilder
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

                    // Account for title height
                    let title_h = if title.is_some() {
                        let (_, th) =
                            estimate_text(title.unwrap_or(""), title_size as f64);
                        th + margin as u32
                    } else {
                        0
                    };

                    let center_x = (w / 2) as i32;
                    let center_y = ((h + title_h) / 2) as i32;
                    let radius = (w.min(h - title_h) as f64 * 0.35).max(10.0);

                    let pie_labels: Vec<String> =
                        pie_data.0.iter().map(|(l, _)| l.clone()).collect();
                    let sizes: Vec<f64> = pie_data.0.iter().map(|(_, v)| *v).collect();
                    let colors: Vec<RGBColor> = match &pie_style.colors {
                        Some(cs) => cs.iter().map(|c| iced_to_plotters(*c)).collect(),
                        None => {
                            (0..sizes.len()).map(|i| PALETTE[i % PALETTE.len()]).collect()
                        }
                    };
                    let label_strs: Vec<&str> =
                        pie_labels.iter().map(|s| s.as_str()).collect();

                    // Store plot info for pie hover
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

                    // Apply projection with interactive offsets
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

                    // Configure axes
                    {
                        let mut axes = chart.configure_axes();
                        if mesh_style.and_then(|ms| ms.label_size).is_some()
                            || mesh_style.and_then(|ms| ms.label_color).is_some()
                        {
                            let s =
                                mesh_style.and_then(|ms| ms.label_size).unwrap_or(12.0);
                            if let Some(lc) = mesh_style.and_then(|ms| ms.label_color) {
                                let mut style =
                                    TextStyle::from(("sans-serif", s).into_font());
                                style.color = iced_to_plotters(lc).to_backend_color();
                                axes.label_style(style);
                            } else {
                                axes.label_style(("sans-serif", s).into_font());
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

                    // Draw each 3D dataset
                    for (i, ds) in self.datasets.iter().enumerate() {
                        match ds {
                            DatasetEntry::Scatter3D { data, style } => {
                                if let Some(pts) = data.t.as_ref() {
                                    let color = style
                                        .color
                                        .map(iced_to_plotters)
                                        .unwrap_or(PALETTE[i % PALETTE.len()]);
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
                                    let color = style
                                        .color
                                        .map(iced_to_plotters)
                                        .unwrap_or(PALETTE[i % PALETTE.len()]);
                                    let sw = style.stroke_width.unwrap_or(2.0) as u32;
                                    let line_style =
                                        ShapeStyle::from(color).stroke_width(sw);
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
                                }
                            }
                            DatasetEntry::Surface { data, style } => {
                                if let Some(grid) = data.t.as_ref() {
                                    if grid.0.is_empty() || grid.0[0].is_empty() {
                                        continue;
                                    }
                                    let color = style
                                        .color
                                        .map(iced_to_plotters)
                                        .unwrap_or(PALETTE[i % PALETTE.len()]);
                                    let color_by_z = style.color_by_z.unwrap_or(false);

                                    let x_vals: Vec<f64> =
                                        grid.0.iter().map(|row| row[0].0).collect();
                                    let y_vals: Vec<f64> =
                                        grid.0[0].iter().map(|pt| pt.1).collect();

                                    let z_lookup = |x: f64, y: f64| -> f64 {
                                        for row in grid.0.iter() {
                                            for &(gx, gy, gz) in row.iter() {
                                                if (gx - x).abs() < 1e-10
                                                    && (gy - y).abs() < 1e-10
                                                {
                                                    return gz;
                                                }
                                            }
                                        }
                                        0.0
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

                    // Legend
                    let has_labels = self.datasets.iter().any(|ds| ds.label().is_some());
                    if has_labels {
                        let legend_pos = self
                            .legend_position
                            .t
                            .as_ref()
                            .and_then(|o| o.0.as_ref())
                            .map(|p| p.0.clone())
                            .unwrap_or(SeriesLabelPosition::UpperLeft);
                        let ls = self.legend_style.t.as_ref().and_then(|o| o.0.as_ref());
                        let legend_bg = ls
                            .and_then(|s| s.background)
                            .map(iced_to_plotters)
                            .unwrap_or(WHITE);
                        let legend_border = ls
                            .and_then(|s| s.border)
                            .map(iced_to_plotters)
                            .unwrap_or(BLACK);
                        let legend_font_sz =
                            ls.and_then(|s| s.label_size).unwrap_or(label_sz);
                        let mut labels = chart.configure_series_labels();
                        labels.position(legend_pos);
                        labels.margin(15);
                        labels.background_style(legend_bg.mix(0.8));
                        labels.border_style(legend_border);
                        let mut style =
                            TextStyle::from(("sans-serif", legend_font_sz).into_font());
                        if let Some(lc) = ls.and_then(|s| s.label_color) {
                            style.color = iced_to_plotters(lc).to_backend_color();
                        }
                        labels.label_font(style);
                        if let Err(e) = labels.draw() {
                            error!("chart series labels draw: {e:?}");
                        }
                    }
                }

                ChartMode::Empty => unreachable!(),
            }

            if let Err(e) = root.present() {
                error!("chart present: {e:?}");
            }
        });

        // Draw tooltip overlay (uncached, redrawn each frame)
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
