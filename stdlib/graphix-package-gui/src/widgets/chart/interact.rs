use super::{
    ChartW,
    dataset::{
        ChartMode, DatasetEntry, bar_categories, bar_value, pie_slices, pie_start_angle,
    },
    ranges::checked_range,
    types::*,
};
use crate::widgets::{ChartId, Renderer};
use compact_str::{CompactString, format_compact};
use graphix_rt::GXExt;
use iced_core::{Point, Rectangle, mouse};
use iced_widget::canvas as iced_canvas;
use std::{
    cell::Cell,
    time::{Duration, Instant},
};

/// Snap threshold in pixels — how close the cursor must be to a data point.
const SNAP_THRESHOLD: f32 = 20.0;
/// Zoom factor per scroll line.
const ZOOM_FACTOR: f64 = 1.1;
/// Two clicks this close in time and place are a double-click.
const DOUBLE_CLICK: Duration = Duration::from_millis(400);
/// A press released within this many pixels of where it went down is a
/// click, not a drag.
const CLICK_SLOP: f32 = 4.0;

/// Plot area info captured during draw() for use by update().
#[derive(Clone, Copy, Debug)]
pub struct PlotInfo {
    pub rect: Rectangle,
    pub x_range: (f64, f64),
    pub y_range: (f64, f64),
}

/// A snapped data point for tooltip display.
#[derive(Clone, Debug)]
pub struct SnapPoint {
    pub pixel: Point,
    pub label: CompactString,
    pub value: CompactString,
}

/// A drag in progress: where it started and the view it started from.
struct Drag {
    origin: Point,
    x_view: Option<(f64, f64)>,
    y_view: Option<(f64, f64)>,
    yaw: f64,
    pitch: f64,
}

/// Interactive chart state, held as `Program::State`. iced keeps it by
/// position in its tree, so the view holds only for the chart and mode
/// in `owner`.
pub struct ChartState {
    pub cache: iced_canvas::Cache<Renderer>,
    owner: Option<(ChartId, ChartMode)>,
    pub x_view: Option<(f64, f64)>,
    pub y_view: Option<(f64, f64)>,
    pub yaw_offset: f64,
    pub pitch_offset: f64,
    pub scale_factor: f64,
    pub plot_info: Cell<Option<PlotInfo>>,
    pub snap_point: Option<SnapPoint>,
    drag: Option<Drag>,
    press: Option<Point>,
    last_click: Option<(Point, Instant)>,
}

impl Default for ChartState {
    fn default() -> Self {
        Self {
            cache: iced_canvas::Cache::new(),
            owner: None,
            x_view: None,
            y_view: None,
            yaw_offset: 0.0,
            pitch_offset: 0.0,
            scale_factor: 1.0,
            plot_info: Cell::new(None),
            snap_point: None,
            drag: None,
            press: None,
            last_click: None,
        }
    }
}

/// The view a chart draws through.
pub struct ViewState {
    pub x: Option<(f64, f64)>,
    pub y: Option<(f64, f64)>,
    pub yaw: f64,
    pub pitch: f64,
    pub scale: f64,
}

impl ChartState {
    /// Whether this state's view and snap were taken on `chart` in its
    /// current mode.
    pub(crate) fn owns<X: GXExt>(&self, chart: &ChartW<X>) -> bool {
        self.owner == Some((chart.id, chart.mode))
    }

    /// The view to draw `chart` through: the state's, when it owns it,
    /// else the chart's own.
    pub(crate) fn view_for<X: GXExt>(&self, chart: &ChartW<X>) -> ViewState {
        match self.owns(chart) {
            true => ViewState {
                x: self.x_view,
                y: self.y_view,
                yaw: self.yaw_offset,
                pitch: self.pitch_offset,
                scale: self.scale_factor,
            },
            false => ViewState { x: None, y: None, yaw: 0.0, pitch: 0.0, scale: 1.0 },
        }
    }

    fn reset_view(&mut self) {
        self.x_view = None;
        self.y_view = None;
        self.yaw_offset = 0.0;
        self.pitch_offset = 0.0;
        self.scale_factor = 1.0;
        self.cache.clear();
    }

    /// Handle a mouse event. Returns an optional Action.
    pub(crate) fn handle_event<X: GXExt>(
        &mut self,
        chart: &ChartW<X>,
        event: &iced_core::event::Event,
        bounds: Rectangle,
        cursor: mouse::Cursor,
    ) -> Option<iced_widget::Action<crate::widgets::Message>> {
        use iced_core::{event::Event, mouse::Event as ME};
        use iced_widget::Action;
        let mode = chart.mode;
        if self.owner != Some((chart.id, mode)) {
            self.owner = Some((chart.id, mode));
            self.reset_view();
            self.snap_point = None;
            self.drag = None;
            self.press = None;
            self.last_click = None;
        }
        match event {
            Event::Mouse(ME::CursorMoved { position }) => {
                let local = Point::new(position.x - bounds.x, position.y - bounds.y);
                if let Some(d) = &self.drag {
                    let (dx, dy) = (local.x - d.origin.x, local.y - d.origin.y);
                    self.handle_drag(mode, dx, dy);
                    self.cache.clear();
                    return Some(Action::request_redraw().and_capture());
                }
                if let Some(info) = self.plot_info.get()
                    && mode != ChartMode::ThreeD
                {
                    self.snap_point = find_nearest_point(chart, &info, local);
                }
                Some(Action::request_redraw())
            }
            Event::Mouse(ME::WheelScrolled { delta }) => {
                if matches!(mode, ChartMode::Pie | ChartMode::Empty) {
                    return None;
                }
                let pos = cursor.position_in(bounds)?;
                let lines = match delta {
                    mouse::ScrollDelta::Lines { y, .. } => *y,
                    mouse::ScrollDelta::Pixels { y, .. } => *y / 28.0,
                } as f64;
                if lines.abs() < 0.001 {
                    return None;
                }
                if mode == ChartMode::ThreeD {
                    self.scale_factor =
                        (self.scale_factor * ZOOM_FACTOR.powf(lines)).clamp(0.1, 10.0);
                } else {
                    let info = self.plot_info.get()?;
                    self.handle_zoom(mode, &info, pos, ZOOM_FACTOR.powf(-lines));
                }
                self.cache.clear();
                Some(Action::capture())
            }
            Event::Mouse(ME::ButtonPressed(mouse::Button::Left)) => {
                let pos = cursor.position_in(bounds)?;
                self.press = Some(pos);
                self.drag = Some(Drag {
                    origin: pos,
                    x_view: self
                        .x_view
                        .or_else(|| self.plot_info.get().map(|i| i.x_range)),
                    y_view: self
                        .y_view
                        .or_else(|| self.plot_info.get().map(|i| i.y_range)),
                    yaw: self.yaw_offset,
                    pitch: self.pitch_offset,
                });
                Some(Action::capture())
            }
            Event::Mouse(ME::ButtonReleased(mouse::Button::Left)) => {
                let dragged = self.drag.take().is_some();
                if let (Some(down), Some(up)) =
                    (self.press.take(), cursor.position_in(bounds))
                    && down.distance(up) <= CLICK_SLOP
                {
                    let now = Instant::now();
                    match self.last_click.take() {
                        Some((at, t))
                            if now.duration_since(t) < DOUBLE_CLICK
                                && at.distance(up) <= CLICK_SLOP =>
                        {
                            self.reset_view()
                        }
                        _ => self.last_click = Some((up, now)),
                    }
                }
                dragged.then(|| Action::request_redraw().and_capture())
            }
            Event::Mouse(ME::CursorLeft) => {
                self.snap_point = None;
                Some(Action::request_redraw())
            }
            _ => None,
        }
    }

    /// Pan by the cursor's travel `(dx, dy)` since the drag began; a bar
    /// chart's categories stay put, a 3D chart turns.
    fn handle_drag(&mut self, mode: ChartMode, dx: f32, dy: f32) {
        let Some(d) = &self.drag else { return };
        match mode {
            ChartMode::ThreeD => {
                self.yaw_offset = d.yaw - dx as f64 * 0.01;
                self.pitch_offset = d.pitch + dy as f64 * 0.01;
            }
            ChartMode::Pie | ChartMode::Empty => {}
            ChartMode::Numeric | ChartMode::TimeSeries | ChartMode::Bar => {
                let Some(info) = self.plot_info.get() else { return };
                if info.rect.width <= 0.0 || info.rect.height <= 0.0 {
                    return;
                }
                let shift = |r: (f64, f64), by: f64| checked_range((r.0 + by, r.1 + by));
                if mode != ChartMode::Bar {
                    let x = d.x_view.unwrap_or(info.x_range);
                    let by = -(dx / info.rect.width) as f64 * (x.1 - x.0);
                    self.x_view = shift(x, by).or(self.x_view);
                }
                let y = d.y_view.unwrap_or(info.y_range);
                let by = (dy / info.rect.height) as f64 * (y.1 - y.0);
                self.y_view = shift(y, by).or(self.y_view);
            }
        }
    }

    /// Zoom about the cursor by `factor` (below 1 zooms in); a bar chart
    /// zooms its values only. A zoom past what f64 resolves is not taken.
    fn handle_zoom(
        &mut self,
        mode: ChartMode,
        info: &PlotInfo,
        cursor: Point,
        factor: f64,
    ) {
        if info.rect.width <= 0.0 || info.rect.height <= 0.0 {
            return;
        }
        let zoom = |r: (f64, f64), t: f64| {
            let at = r.0 + t * (r.1 - r.0);
            let span = (r.1 - r.0) * factor;
            checked_range((at - t * span, at + (1.0 - t) * span))
                .filter(|z| z.1 - z.0 > 1e-9 * (z.0.abs() + z.1.abs()))
        };
        if mode != ChartMode::Bar {
            let t = ((cursor.x - info.rect.x) / info.rect.width).clamp(0.0, 1.0) as f64;
            self.x_view = zoom(self.x_view.unwrap_or(info.x_range), t).or(self.x_view);
        }
        let t = ((cursor.y - info.rect.y) / info.rect.height).clamp(0.0, 1.0) as f64;
        self.y_view = zoom(self.y_view.unwrap_or(info.y_range), 1.0 - t).or(self.y_view);
    }

    /// Return the appropriate mouse cursor for the current state.
    pub fn mouse_interaction(
        &self,
        mode: ChartMode,
        bounds: Rectangle,
        cursor: mouse::Cursor,
    ) -> mouse::Interaction {
        if !cursor.is_over(bounds) {
            return mouse::Interaction::default();
        }
        if self.drag.is_some() {
            return mouse::Interaction::Grabbing;
        }
        match mode {
            ChartMode::ThreeD => mouse::Interaction::Grab,
            ChartMode::Pie | ChartMode::Empty => mouse::Interaction::default(),
            ChartMode::Numeric | ChartMode::TimeSeries | ChartMode::Bar => {
                mouse::Interaction::Crosshair
            }
        }
    }
}

/// Convert pixel coordinates to data coordinates.
fn pixel_to_data(pixel: Point, info: &PlotInfo) -> Option<(f64, f64)> {
    let t_x = (pixel.x - info.rect.x) / info.rect.width;
    let t_y = (pixel.y - info.rect.y) / info.rect.height;
    if !(0.0..=1.0).contains(&t_x) || !(0.0..=1.0).contains(&t_y) {
        return None;
    }
    let x = info.x_range.0 + t_x as f64 * (info.x_range.1 - info.x_range.0);
    let y = info.y_range.1 - t_y as f64 * (info.y_range.1 - info.y_range.0);
    Some((x, y))
}

/// Convert data coordinates to pixel coordinates.
fn data_to_pixel(x: f64, y: f64, info: &PlotInfo) -> Point {
    let t_x = (x - info.x_range.0) / (info.x_range.1 - info.x_range.0);
    let t_y = (info.y_range.1 - y) / (info.y_range.1 - info.y_range.0);
    Point::new(
        info.rect.x + t_x as f32 * info.rect.width,
        info.rect.y + t_y as f32 * info.rect.height,
    )
}

/// The point the cursor is nearest so far: its distance and pixel, and
/// which dataset and which of its points.
#[derive(Clone, Copy)]
struct Hit {
    dist: f32,
    pixel: Point,
    ds: usize,
    at: usize,
}

/// Find the nearest data point to the cursor across the datasets the
/// chart draws, formatting only the one found.
fn find_nearest_point<X: GXExt>(
    chart: &ChartW<X>,
    info: &PlotInfo,
    cursor: Point,
) -> Option<SnapPoint> {
    if !info.rect.contains(cursor) {
        return None;
    }
    let mode = chart.mode;
    let mut best: Option<Hit> = None;
    let mut offer = |hit: Hit| {
        if best.is_none_or(|b| hit.dist < b.dist) {
            best = Some(hit)
        }
    };
    let mut near = |ds: usize, at: usize, x: f64, y: f64| {
        let pixel = data_to_pixel(x, y, info);
        let dist = pixel.distance(cursor);
        if dist < SNAP_THRESHOLD {
            offer(Hit { dist, pixel, ds, at })
        }
    };
    let slot = match mode {
        ChartMode::Bar => pixel_to_data(cursor, info)
            .filter(|(x, _)| *x >= 0.0)
            .map(|(x, _)| x.floor() as usize),
        _ => None,
    };
    let categories = bar_categories(&chart.datasets);
    let mut bar: Option<Hit> = None;
    let mut pie: Option<Hit> = None;
    for (i, ds) in chart.datasets.iter().enumerate() {
        if ds.mode() != Some(mode) {
            continue;
        }
        match ds {
            DatasetEntry::XY { data, .. } => {
                for (j, &(x, y)) in data.t.iter().flat_map(|d| d.pts.iter()).enumerate() {
                    near(i, j, x, y)
                }
            }
            DatasetEntry::Candlestick { data, .. } => {
                for (j, p) in data.t.iter().flat_map(|d| d.pts.iter()).enumerate() {
                    near(i, j, p.x, p.close)
                }
            }
            DatasetEntry::ErrorBar { data, .. } => {
                for (j, p) in data.t.iter().flat_map(|d| d.pts.iter()).enumerate() {
                    near(i, j, p.x, p.avg)
                }
            }
            DatasetEntry::Bar { data, .. } => {
                // The whole vertical strip of a slot is a hit; the anchor is
                // the top-center of this series' bar there.
                let (Some(bd), Some(at)) = (data.t.as_ref(), slot) else { continue };
                let Some(v) = categories.get(at).and_then(|c| bar_value(bd, c)) else {
                    continue;
                };
                let pixel = data_to_pixel(at as f64 + 0.5, v, info);
                let dist = (cursor.x - pixel.x).abs();
                if bar.is_none_or(|b| dist < b.dist) {
                    bar = Some(Hit { dist, pixel, ds: i, at })
                }
            }
            DatasetEntry::Pie { data, style } => {
                let Some(bd) = data.t.as_ref() else { continue };
                let total: f64 = pie_slices(bd).map(|(_, v)| v).sum();
                if !(total > 0.0 && total.is_finite()) {
                    continue;
                }
                let (cx, cy) = (info.rect.center_x(), info.rect.center_y());
                let radius = info.rect.width.min(info.rect.height) / 2.0;
                let (dx, dy) = (cursor.x - cx, cursor.y - cy);
                // Any cursor angle inside a slice selects it, whatever the radius.
                let start = pie_start_angle(style);
                let angle =
                    ((dy.atan2(dx) as f64).to_degrees() - start).rem_euclid(360.0);
                let mut from = 0.0;
                for (at, (_, v)) in pie_slices(bd).enumerate() {
                    let sweep = v / total * 360.0;
                    if angle < from + sweep {
                        let mid = (from + sweep / 2.0 + start).to_radians();
                        let r = (radius * 0.5) as f64;
                        let pixel = Point::new(
                            cx + (mid.cos() * r) as f32,
                            cy + (mid.sin() * r) as f32,
                        );
                        pie = Some(Hit { dist: 0.0, pixel, ds: i, at });
                        break;
                    }
                    from += sweep;
                }
            }
            DatasetEntry::Scatter3D { .. }
            | DatasetEntry::Line3D { .. }
            | DatasetEntry::Surface { .. } => {}
        }
    }
    let hit = best.or(bar).or(pie)?;
    let ds = &chart.datasets[hit.ds];
    let series = || match ds.label() {
        Some(l) => CompactString::from(l),
        None => format_compact!("Series {}", hit.ds + 1),
    };
    let time = |x: f64| ms_datetime(x);
    let (label, value) = match ds {
        DatasetEntry::XY { data, .. } => {
            let d = data.t.as_ref()?;
            let (x, y) = d.pts[hit.at];
            let v = match d.time {
                true => format_compact!("({}, {y:.4})", time(x)),
                false => format_compact!("({x:.4}, {y:.4})"),
            };
            (series(), v)
        }
        DatasetEntry::Candlestick { data, .. } => {
            let d = data.t.as_ref()?;
            let p = d.pts[hit.at];
            let ohlc = format_compact!(
                "O:{:.2} H:{:.2} L:{:.2} C:{:.2}",
                p.open,
                p.high,
                p.low,
                p.close
            );
            let v = match d.time {
                true => format_compact!("{}: {ohlc}", time(p.x)),
                false => ohlc,
            };
            (series(), v)
        }
        DatasetEntry::ErrorBar { data, .. } => {
            let d = data.t.as_ref()?;
            let p = d.pts[hit.at];
            let eb = format_compact!("avg:{:.2} [{:.2}, {:.2}]", p.avg, p.min, p.max);
            let v = match d.time {
                true => format_compact!("{}: {eb}", time(p.x)),
                false => eb,
            };
            (series(), v)
        }
        DatasetEntry::Bar { data, style } => {
            let cat = categories.get(hit.at)?;
            let v = bar_value(data.t.as_ref()?, cat)?;
            let label = style.label.as_deref().unwrap_or(cat);
            (CompactString::from(label), format_compact!("{cat}: {v:.2}"))
        }
        DatasetEntry::Pie { data, .. } => {
            let bd = data.t.as_ref()?;
            let total: f64 = pie_slices(bd).map(|(_, v)| v).sum();
            let (cat, v) = pie_slices(bd).nth(hit.at)?;
            let pct = v / total * 100.0;
            (CompactString::from(cat), format_compact!("{v:.2} ({pct:.1}%)"))
        }
        DatasetEntry::Scatter3D { .. }
        | DatasetEntry::Line3D { .. }
        | DatasetEntry::Surface { .. } => return None,
    };
    Some(SnapPoint { pixel: hit.pixel, label, value })
}

/// Draw the tooltip overlay onto a frame.
pub fn draw_tooltip(
    frame: &mut iced_widget::canvas::Frame<Renderer>,
    snap: &SnapPoint,
    bounds_size: iced_core::Size,
) {
    use iced_core::{Color, Size};
    use iced_widget::canvas::{Path, Stroke};

    let highlight = Path::circle(snap.pixel, 5.0);
    frame.fill(&highlight, Color::from_rgba8(255, 100, 100, 0.78));
    frame.stroke(&highlight, Stroke::default().with_color(Color::WHITE).with_width(1.5));

    let text = format!("{}: {}", snap.label, snap.value);
    let font_size = 12.0_f32;
    let text_w = text.len() as f32 * font_size * 0.6 + 16.0;
    let text_h = font_size + 12.0;
    let pad = 8.0_f32;

    let mut tx = snap.pixel.x + 12.0;
    let mut ty = snap.pixel.y - text_h - 8.0;

    if tx + text_w > bounds_size.width {
        tx = snap.pixel.x - text_w - 12.0;
    }
    if ty < 0.0 {
        ty = snap.pixel.y + 12.0;
    }
    if tx < 0.0 {
        tx = pad;
    }

    let bg_rect = Path::rectangle(Point::new(tx, ty), Size::new(text_w, text_h));
    frame.fill(&bg_rect, Color::from_rgba8(40, 40, 50, 0.9));
    frame.stroke(
        &bg_rect,
        Stroke::default()
            .with_color(Color::from_rgba8(120, 120, 140, 0.78))
            .with_width(1.0),
    );

    frame.fill_text(iced_widget::canvas::Text {
        content: text,
        position: Point::new(tx + pad, ty + pad / 2.0),
        color: Color::from_rgba8(240, 240, 240, 1.0),
        size: font_size.into(),
        ..iced_widget::canvas::Text::default()
    });
}
