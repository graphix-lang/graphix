use super::dataset::{chart_mode, ChartMode, DatasetEntry};
use super::types::*;
use crate::widgets::Renderer;
use graphix_rt::GXExt;
use iced_core::{mouse, Point, Rectangle};
use iced_widget::canvas as iced_canvas;
use std::cell::Cell;

/// Snap threshold in pixels — how close the cursor must be to a data point.
const SNAP_THRESHOLD: f32 = 20.0;
/// Zoom factor per scroll line.
const ZOOM_FACTOR: f64 = 1.1;
/// Double-click threshold in milliseconds.
const DOUBLE_CLICK_MS: u128 = 400;

/// Plot area info captured during draw() for use by update().
#[derive(Clone, Copy, Debug)]
pub(crate) struct PlotInfo {
    pub rect: Rectangle,
    pub x_range: (f64, f64),
    pub y_range: (f64, f64),
}

/// A snapped data point for tooltip display.
#[derive(Clone, Debug)]
pub(crate) struct SnapPoint {
    pub pixel: Point,
    pub label: String,
    pub value: String,
}

/// Interactive chart state, held as `Program::State`.
pub(crate) struct ChartState {
    pub cache: iced_canvas::Cache<Renderer>,
    // cursor position (canvas-relative)
    pub cursor: Option<Point>,
    // zoom/pan — overrides to base axis ranges
    pub x_view: Option<(f64, f64)>,
    pub y_view: Option<(f64, f64)>,
    // drag state for pan
    pub drag_origin: Option<Point>,
    drag_x_view: Option<(f64, f64)>,
    drag_y_view: Option<(f64, f64)>,
    // 3D rotation drag
    drag_yaw: Option<f64>,
    drag_pitch: Option<f64>,
    // 3D interactive rotation offsets
    pub yaw_offset: f64,
    pub pitch_offset: f64,
    pub scale_factor: f64,
    // plot area info set during draw() via Cell
    pub plot_info: Cell<Option<PlotInfo>>,
    // nearest point for tooltip
    pub snap_point: Option<SnapPoint>,
    // double-click detection
    last_click: Option<std::time::Instant>,
}

impl Default for ChartState {
    fn default() -> Self {
        Self {
            cache: iced_canvas::Cache::new(),
            cursor: None,
            x_view: None,
            y_view: None,
            drag_origin: None,
            drag_x_view: None,
            drag_y_view: None,
            drag_yaw: None,
            drag_pitch: None,
            yaw_offset: 0.0,
            pitch_offset: 0.0,
            scale_factor: 1.0,
            plot_info: Cell::new(None),
            snap_point: None,
            last_click: None,
        }
    }
}

impl ChartState {
    /// Handle a mouse event. Returns an optional Action.
    pub(super) fn handle_event<X: GXExt>(
        &mut self,
        chart: &super::ChartW<X>,
        event: &iced_core::event::Event,
        bounds: Rectangle,
        cursor: mouse::Cursor,
    ) -> Option<iced_widget::Action<crate::widgets::Message>> {
        use iced_core::event::Event;
        use iced_core::mouse::Event as ME;
        use iced_widget::Action;

        let mode = chart_mode(&chart.datasets);

        match event {
            Event::Mouse(ME::CursorMoved { position }) => {
                let local = Point::new(position.x - bounds.x, position.y - bounds.y);
                self.cursor = Some(local);

                if let Some(origin) = self.drag_origin {
                    let dx = local.x - origin.x;
                    let dy = local.y - origin.y;
                    self.handle_drag(mode, dx, dy);
                    self.cache.clear();
                    return Some(Action::request_redraw().and_capture());
                }

                // Find nearest data point for tooltip
                if let Some(info) = self.plot_info.get() {
                    if mode != ChartMode::ThreeD {
                        self.snap_point =
                            find_nearest_point(&chart.datasets, &info, local, mode);
                    }
                }
                Some(Action::request_redraw())
            }

            Event::Mouse(ME::WheelScrolled { delta }) => {
                let pos = match cursor.position_in(bounds) {
                    Some(p) => p,
                    None => return None,
                };
                let lines = match delta {
                    mouse::ScrollDelta::Lines { y, .. } => *y,
                    mouse::ScrollDelta::Pixels { y, .. } => *y / 28.0,
                };
                if lines.abs() < 0.001 {
                    return None;
                }
                let info = self.plot_info.get()?;
                self.handle_scroll(mode, &info, pos, lines);
                self.cache.clear();
                Some(Action::capture())
            }

            Event::Mouse(ME::ButtonPressed(mouse::Button::Left)) => {
                let pos = match cursor.position_in(bounds) {
                    Some(p) => p,
                    None => return None,
                };
                // Check for double-click
                let now = std::time::Instant::now();
                if let Some(last) = self.last_click {
                    if now.duration_since(last).as_millis() < DOUBLE_CLICK_MS {
                        // Reset zoom/pan
                        self.x_view = None;
                        self.y_view = None;
                        self.yaw_offset = 0.0;
                        self.pitch_offset = 0.0;
                        self.scale_factor = 1.0;
                        self.cache.clear();
                        self.last_click = None;
                        return Some(Action::capture());
                    }
                }
                self.last_click = Some(now);
                self.drag_origin = Some(pos);
                self.drag_x_view =
                    self.x_view.or_else(|| self.plot_info.get().map(|i| i.x_range));
                self.drag_y_view =
                    self.y_view.or_else(|| self.plot_info.get().map(|i| i.y_range));
                self.drag_yaw = Some(self.yaw_offset);
                self.drag_pitch = Some(self.pitch_offset);
                Some(Action::capture())
            }

            Event::Mouse(ME::ButtonReleased(mouse::Button::Left)) => {
                if self.drag_origin.is_some() {
                    self.drag_origin = None;
                    self.drag_x_view = None;
                    self.drag_y_view = None;
                    self.drag_yaw = None;
                    self.drag_pitch = None;
                    return Some(Action::capture());
                }
                None
            }

            Event::Mouse(ME::CursorLeft) => {
                self.cursor = None;
                self.snap_point = None;
                Some(Action::request_redraw())
            }

            _ => None,
        }
    }

    fn handle_drag(&mut self, mode: ChartMode, dx: f32, dy: f32) {
        match mode {
            ChartMode::ThreeD => {
                // Drag rotates yaw/pitch
                if let (Some(base_yaw), Some(base_pitch)) =
                    (self.drag_yaw, self.drag_pitch)
                {
                    self.yaw_offset = base_yaw + (dx as f64) * 0.01;
                    self.pitch_offset = base_pitch + (dy as f64) * 0.01;
                }
            }
            ChartMode::Bar => {
                // Bar charts: drag only pans Y axis
                if let Some(info) = self.plot_info.get() {
                    let y_range = self.drag_y_view.unwrap_or(info.y_range);
                    let y_span = y_range.1 - y_range.0;
                    let dy_data = (dy as f64 / info.rect.height as f64) * y_span;
                    self.y_view = Some((y_range.0 + dy_data, y_range.1 + dy_data));
                }
            }
            ChartMode::Pie | ChartMode::Empty => {}
            _ => {
                // Numeric / TimeSeries: drag pans both axes
                if let Some(info) = self.plot_info.get() {
                    let x_range = self.drag_x_view.unwrap_or(info.x_range);
                    let y_range = self.drag_y_view.unwrap_or(info.y_range);
                    let x_span = x_range.1 - x_range.0;
                    let y_span = y_range.1 - y_range.0;
                    let dx_data = -(dx as f64 / info.rect.width as f64) * x_span;
                    let dy_data = (dy as f64 / info.rect.height as f64) * y_span;
                    self.x_view = Some((x_range.0 + dx_data, x_range.1 + dx_data));
                    self.y_view = Some((y_range.0 + dy_data, y_range.1 + dy_data));
                }
            }
        }
    }

    fn handle_scroll(
        &mut self,
        mode: ChartMode,
        info: &PlotInfo,
        cursor: Point,
        lines: f32,
    ) {
        let factor = if lines > 0.0 { 1.0 / ZOOM_FACTOR } else { ZOOM_FACTOR };

        match mode {
            ChartMode::ThreeD => {
                // Scroll zooms scale
                self.scale_factor *=
                    if lines > 0.0 { ZOOM_FACTOR } else { 1.0 / ZOOM_FACTOR };
                self.scale_factor = self.scale_factor.clamp(0.1, 10.0);
            }
            ChartMode::Bar => {
                // Bar: zoom Y only, centered on cursor Y
                let y_range = self.y_view.unwrap_or(info.y_range);
                let t_y = (cursor.y - info.rect.y) / info.rect.height;
                let data_y = y_range.1 - t_y as f64 * (y_range.1 - y_range.0);
                let new_span = (y_range.1 - y_range.0) * factor;
                let t_y_f = t_y as f64;
                self.y_view =
                    Some((data_y - (1.0 - t_y_f) * new_span, data_y + t_y_f * new_span));
            }
            ChartMode::Pie | ChartMode::Empty => {}
            _ => {
                // Zoom both axes centered on cursor
                let x_range = self.x_view.unwrap_or(info.x_range);
                let y_range = self.y_view.unwrap_or(info.y_range);

                let t_x =
                    ((cursor.x - info.rect.x) / info.rect.width).clamp(0.0, 1.0) as f64;
                let t_y =
                    ((cursor.y - info.rect.y) / info.rect.height).clamp(0.0, 1.0) as f64;

                let data_x = x_range.0 + t_x * (x_range.1 - x_range.0);
                let data_y = y_range.1 - t_y * (y_range.1 - y_range.0);

                let x_span = (x_range.1 - x_range.0) * factor;
                let y_span = (y_range.1 - y_range.0) * factor;

                self.x_view =
                    Some((data_x - t_x * x_span, data_x + (1.0 - t_x) * x_span));
                self.y_view =
                    Some((data_y - (1.0 - t_y) * y_span, data_y + t_y * y_span));
            }
        }
    }

    /// Return the appropriate mouse cursor for the current state.
    pub(super) fn mouse_interaction(
        &self,
        mode: ChartMode,
        bounds: Rectangle,
        cursor: mouse::Cursor,
    ) -> mouse::Interaction {
        let _pos = match cursor.position_in(bounds) {
            Some(p) => p,
            None => return mouse::Interaction::default(),
        };
        if self.drag_origin.is_some() {
            return mouse::Interaction::Grabbing;
        }
        match mode {
            ChartMode::ThreeD => mouse::Interaction::Grab,
            ChartMode::Pie | ChartMode::Empty => mouse::Interaction::default(),
            _ => mouse::Interaction::Crosshair,
        }
    }
}

/// Convert pixel coordinates to data coordinates.
fn pixel_to_data(pixel: Point, info: &PlotInfo) -> Option<(f64, f64)> {
    let t_x = (pixel.x - info.rect.x) / info.rect.width;
    let t_y = (pixel.y - info.rect.y) / info.rect.height;
    if t_x < 0.0 || t_x > 1.0 || t_y < 0.0 || t_y > 1.0 {
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

/// Try to improve the current best snap with a candidate point.
fn try_snap(
    best: &mut Option<(f32, SnapPoint)>,
    cursor: Point,
    px: Point,
    label: &str,
    value: String,
) {
    let dist = ((px.x - cursor.x).powi(2) + (px.y - cursor.y).powi(2)).sqrt();
    if dist < SNAP_THRESHOLD && best.as_ref().map_or(true, |(d, _)| dist < *d) {
        *best = Some((dist, SnapPoint { pixel: px, label: label.to_string(), value }));
    }
}

/// Find the nearest data point to the cursor across all datasets.
fn find_nearest_point<X: GXExt>(
    datasets: &[DatasetEntry<X>],
    info: &PlotInfo,
    cursor: Point,
    mode: ChartMode,
) -> Option<SnapPoint> {
    // Only snap within the plot area
    if cursor.x < info.rect.x
        || cursor.x > info.rect.x + info.rect.width
        || cursor.y < info.rect.y
        || cursor.y > info.rect.y + info.rect.height
    {
        return None;
    }

    let mut best: Option<(f32, SnapPoint)> = None;

    for (i, ds) in datasets.iter().enumerate() {
        let default_label = format!("Series {}", i + 1);
        let series_label = ds.label().unwrap_or(&default_label);
        match ds {
            DatasetEntry::XY { data, .. } | DatasetEntry::DashedLine { data, .. } => {
                if let Some(d) = data.t.as_ref() {
                    match d {
                        XYData::Numeric(pts) if mode == ChartMode::Numeric => {
                            for &(x, y) in pts.iter() {
                                let px = data_to_pixel(x, y, info);
                                try_snap(
                                    &mut best,
                                    cursor,
                                    px,
                                    series_label,
                                    format!("({x:.4}, {y:.4})"),
                                );
                            }
                        }
                        XYData::DateTime(pts) if mode == ChartMode::TimeSeries => {
                            for &(dt, y) in pts.iter() {
                                let x = dt.timestamp_millis() as f64;
                                let px = data_to_pixel(x, y, info);
                                try_snap(
                                    &mut best,
                                    cursor,
                                    px,
                                    series_label,
                                    format!("({dt}, {y:.4})"),
                                );
                            }
                        }
                        _ => {}
                    }
                }
            }
            DatasetEntry::Candlestick { data, .. } => {
                if let Some(d) = data.t.as_ref() {
                    match d {
                        OHLCData::Numeric(pts) => {
                            for pt in pts.iter() {
                                let px = data_to_pixel(pt.x, pt.close, info);
                                try_snap(
                                    &mut best,
                                    cursor,
                                    px,
                                    series_label,
                                    format!(
                                        "O:{:.2} H:{:.2} L:{:.2} C:{:.2}",
                                        pt.open, pt.high, pt.low, pt.close
                                    ),
                                );
                            }
                        }
                        OHLCData::DateTime(pts) => {
                            for pt in pts.iter() {
                                let x = pt.x.timestamp_millis() as f64;
                                let px = data_to_pixel(x, pt.close, info);
                                try_snap(
                                    &mut best,
                                    cursor,
                                    px,
                                    series_label,
                                    format!(
                                        "{}: O:{:.2} H:{:.2} L:{:.2} C:{:.2}",
                                        pt.x, pt.open, pt.high, pt.low, pt.close
                                    ),
                                );
                            }
                        }
                    }
                }
            }
            DatasetEntry::ErrorBar { data, .. } => {
                if let Some(d) = data.t.as_ref() {
                    match d {
                        EBData::Numeric(pts) => {
                            for pt in pts.iter() {
                                let px = data_to_pixel(pt.x, pt.avg, info);
                                try_snap(
                                    &mut best,
                                    cursor,
                                    px,
                                    series_label,
                                    format!(
                                        "avg:{:.2} [{:.2}, {:.2}]",
                                        pt.avg, pt.min, pt.max
                                    ),
                                );
                            }
                        }
                        EBData::DateTime(pts) => {
                            for pt in pts.iter() {
                                let x = pt.x.timestamp_millis() as f64;
                                let px = data_to_pixel(x, pt.avg, info);
                                try_snap(
                                    &mut best,
                                    cursor,
                                    px,
                                    series_label,
                                    format!(
                                        "{}: avg:{:.2} [{:.2}, {:.2}]",
                                        pt.x, pt.avg, pt.min, pt.max
                                    ),
                                );
                            }
                        }
                    }
                }
            }
            DatasetEntry::Bar { data, style } => {
                if let Some(bd) = data.t.as_ref() {
                    if pixel_to_data(cursor, info).is_some() {
                        for (cat, val) in bd.0.iter() {
                            let label = style.label.as_deref().unwrap_or(cat.as_str());
                            let py = data_to_pixel(0.0, *val, info);
                            let dist = (cursor.y - py.y).abs();
                            if dist < SNAP_THRESHOLD * 2.0
                                && best.as_ref().map_or(true, |(d, _)| dist < *d)
                            {
                                best = Some((
                                    dist,
                                    SnapPoint {
                                        pixel: Point::new(cursor.x, py.y),
                                        label: label.to_string(),
                                        value: format!("{cat}: {val:.2}"),
                                    },
                                ));
                            }
                        }
                    }
                }
            }
            DatasetEntry::Pie { data, style } => {
                if let Some(bd) = data.t.as_ref() {
                    let total: f64 = bd.0.iter().map(|(_, v)| *v).sum();
                    if total <= 0.0 {
                        continue;
                    }
                    let cx = info.rect.x + info.rect.width / 2.0;
                    let cy = info.rect.y + info.rect.height / 2.0;
                    let radius = (info.rect.width.min(info.rect.height) * 0.35).max(10.0);
                    let dx = cursor.x - cx;
                    let dy = cursor.y - cy;
                    let dist = (dx * dx + dy * dy).sqrt();
                    if dist > radius {
                        continue;
                    }
                    let start = style.start_angle.unwrap_or(0.0);
                    let angle = ((dy.atan2(dx) as f64).to_degrees() - start).rem_euclid(360.0);
                    let mut cumulative = 0.0;
                    for (cat, val) in bd.0.iter() {
                        let slice_angle = (*val / total) * 360.0;
                        if angle >= cumulative && angle < cumulative + slice_angle {
                            let pct = (*val / total) * 100.0;
                            best = Some((
                                0.0,
                                SnapPoint {
                                    pixel: cursor,
                                    label: cat.clone(),
                                    value: format!("{val:.2} ({pct:.1}%)"),
                                },
                            ));
                            break;
                        }
                        cumulative += slice_angle;
                    }
                }
            }
            // No tooltip for 3D datasets
            DatasetEntry::Scatter3D { .. }
            | DatasetEntry::Line3D { .. }
            | DatasetEntry::Surface { .. } => {}
        }
    }

    best.map(|(_, sp)| sp)
}

/// Draw the tooltip overlay onto a frame.
pub(super) fn draw_tooltip(
    frame: &mut iced_widget::canvas::Frame<Renderer>,
    snap: &SnapPoint,
    bounds_size: iced_core::Size,
) {
    use iced_core::{Color, Size};
    use iced_widget::canvas::{Path, Stroke};

    // Highlight circle at snap point
    let highlight = Path::circle(snap.pixel, 5.0);
    frame.fill(&highlight, Color::from_rgba8(255, 100, 100, 0.78));
    frame.stroke(&highlight, Stroke::default().with_color(Color::WHITE).with_width(1.5));

    // Tooltip text
    let text = format!("{}: {}", snap.label, snap.value);
    let font_size = 12.0_f32;
    let text_w = text.len() as f32 * font_size * 0.6 + 16.0;
    let text_h = font_size + 12.0;
    let pad = 8.0_f32;

    // Position tooltip near snap point, offset so it doesn't obscure the point
    let mut tx = snap.pixel.x + 12.0;
    let mut ty = snap.pixel.y - text_h - 8.0;

    // Keep tooltip on-screen
    if tx + text_w > bounds_size.width {
        tx = snap.pixel.x - text_w - 12.0;
    }
    if ty < 0.0 {
        ty = snap.pixel.y + 12.0;
    }
    if tx < 0.0 {
        tx = pad;
    }

    // Background
    let bg_rect = Path::rectangle(Point::new(tx, ty), Size::new(text_w, text_h));
    frame.fill(&bg_rect, Color::from_rgba8(40, 40, 50, 0.9));
    frame.stroke(
        &bg_rect,
        Stroke::default()
            .with_color(Color::from_rgba8(120, 120, 140, 0.78))
            .with_width(1.0),
    );

    // Text
    frame.fill_text(iced_widget::canvas::Text {
        content: text,
        position: Point::new(tx + pad, ty + pad / 2.0),
        color: Color::from_rgba8(240, 240, 240, 1.0),
        size: font_size.into(),
        ..iced_widget::canvas::Text::default()
    });
}
