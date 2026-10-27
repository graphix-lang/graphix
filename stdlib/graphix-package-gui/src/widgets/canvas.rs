use super::{GuiW, GuiWidget, IcedElement, Renderer};
use crate::types::{ColorV, Finite, LengthV, SizeV, TextSizeV};
use anyhow::{Context, Result};
use compact_str::{CompactString, format_compact};
use graphix_compiler::expr::ExprId;
use graphix_rt::{GXExt, GXHandle};
use iced_core::{Point, Rectangle, mouse};
use netidx::{
    protocol::valarray::ValArray,
    publisher::{FromValue, Value},
};
use netidx_derive::FromValue;

use iced_widget::canvas as iced_canvas;
use std::cell::Cell;

mod path;
use path::CheckedPath;

#[derive(Clone, Copy, Debug)]
pub(crate) struct PointV(pub(crate) Point);

impl FromValue for PointV {
    fn from_value(v: Value) -> Result<Self> {
        #[derive(FromValue)]
        #[value(shape = "graphix_compiler::rep::GraphixShape")]
        struct Fields {
            x: Finite,
            y: Finite,
        }
        let Fields { x, y } = v.cast_to()?;
        Ok(Self(Point::new(x.0, y.0)))
    }
}

#[derive(Clone, Copy, Debug, FromValue)]
#[value(shape = "graphix_compiler::rep::GraphixShape")]
pub(crate) struct StrokeV {
    color: ColorV,
    width: Finite,
}

#[derive(Clone, Debug, FromValue)]
#[value(shape = "graphix_compiler::rep::GraphixShape")]
pub(crate) enum CanvasShape {
    Line {
        from: PointV,
        to: PointV,
        color: ColorV,
        width: Finite,
    },
    Circle {
        center: PointV,
        radius: Finite,
        fill: Option<ColorV>,
        stroke: Option<StrokeV>,
    },
    Rect {
        top_left: PointV,
        size: SizeV,
        fill: Option<ColorV>,
        stroke: Option<StrokeV>,
    },
    RoundedRect {
        top_left: PointV,
        size: SizeV,
        radius: Finite,
        fill: Option<ColorV>,
        stroke: Option<StrokeV>,
    },
    Arc {
        center: PointV,
        radius: Finite,
        start_angle: Finite,
        end_angle: Finite,
        stroke: StrokeV,
    },
    Ellipse {
        center: PointV,
        radii: PointV,
        rotation: Finite,
        start_angle: Finite,
        end_angle: Finite,
        fill: Option<ColorV>,
        stroke: Option<StrokeV>,
    },
    BezierCurve {
        from: PointV,
        control_a: PointV,
        control_b: PointV,
        to: PointV,
        color: ColorV,
        width: Finite,
    },
    QuadraticCurve {
        from: PointV,
        control: PointV,
        to: PointV,
        color: ColorV,
        width: Finite,
    },
    Text {
        content: String,
        position: PointV,
        color: ColorV,
        size: TextSizeV,
    },
    Path {
        segments: Vec<PathSegment>,
        fill: Option<ColorV>,
        stroke: Option<StrokeV>,
    },
}

#[derive(Clone, Debug, FromValue)]
#[value(shape = "graphix_compiler::rep::GraphixShape")]
pub(crate) enum PathSegment {
    MoveTo(PointV),
    LineTo(PointV),
    BezierTo { control_a: PointV, control_b: PointV, to: PointV },
    QuadraticTo { control: PointV, to: PointV },
    ArcTo { a: PointV, b: PointV, radius: Finite },
    Close,
}

const MAX_EXTENT: f64 = 1_000_000.0;

fn extent_fits(points: &[PointV], margin: f64) -> bool {
    points.iter().all(|p| {
        (p.0.x as f64).abs() + margin <= MAX_EXTENT
            && (p.0.y as f64).abs() + margin <= MAX_EXTENT
    })
}

impl CanvasShape {
    fn fits(&self) -> bool {
        let stroke =
            |s: &Option<StrokeV>| s.as_ref().map_or(0.0, |s| s.width.0.abs() as f64);
        match self {
            Self::Line { from, to, width, .. } => {
                extent_fits(&[*from, *to], width.0.abs() as f64)
            }
            Self::Circle { center, radius, stroke: s, .. } => {
                extent_fits(&[*center], radius.0.abs() as f64 + stroke(s))
            }
            Self::Arc { center, radius, stroke, .. } => extent_fits(
                &[*center],
                radius.0.abs() as f64 + stroke.width.0.abs() as f64,
            ),
            Self::Rect { top_left, size, stroke: s, .. }
            | Self::RoundedRect { top_left, size, stroke: s, .. } => extent_fits(
                &[*top_left],
                size.0.width.abs().max(size.0.height.abs()) as f64 + stroke(s),
            ),
            Self::Ellipse { center, radii, stroke: s, .. } => extent_fits(
                &[*center],
                (radii.0.x as f64).hypot(radii.0.y as f64) + stroke(s),
            ),
            Self::BezierCurve { from, control_a, control_b, to, width, .. } => {
                extent_fits(&[*from, *control_a, *control_b, *to], width.0.abs() as f64)
            }
            Self::QuadraticCurve { from, control, to, width, .. } => {
                extent_fits(&[*from, *control, *to], width.0.abs() as f64)
            }
            Self::Text { position, size, content, .. } => extent_fits(
                &[*position],
                size.0.abs() as f64 * content.chars().count().max(1) as f64,
            ),
            Self::Path { segments, stroke: s, .. } => segments.iter().all(|s0| {
                let margin = stroke(s);
                match s0 {
                    PathSegment::MoveTo(p) | PathSegment::LineTo(p) => {
                        extent_fits(&[*p], margin)
                    }
                    PathSegment::BezierTo { control_a, control_b, to } => {
                        extent_fits(&[*control_a, *control_b, *to], margin)
                    }
                    PathSegment::QuadraticTo { control, to } => {
                        extent_fits(&[*control, *to], margin)
                    }
                    PathSegment::ArcTo { a, b, radius } => {
                        extent_fits(&[*a, *b], margin + radius.0.abs() as f64)
                    }
                    PathSegment::Close => margin <= MAX_EXTENT,
                }
            }),
        }
    }
}

/// The shapes of a canvas update; one that does not read (a non-finite
/// field) is left out, as a chart leaves out a non-finite point, and the
/// rest draw.
#[derive(Clone, Debug)]
pub(crate) struct ShapeVec(pub Vec<CanvasShape>);

impl ShapeVec {
    /// The shapes of `a` that read, and, when any did not, a report of
    /// how many were left out and why the first was.
    pub(crate) fn read(a: &ValArray) -> (Self, Option<CompactString>) {
        let mut shapes = Vec::with_capacity(a.len());
        let mut skipped = None;
        for s in a.iter() {
            match CanvasShape::from_value(s.clone()) {
                Ok(s) => shapes.push(s),
                Err(e) => skipped.get_or_insert((0, e)).0 += 1,
            }
        }
        let report = skipped.map(|(n, e)| {
            format_compact!("canvas: {n} shape(s) skipped, the first: {e:#}")
        });
        (Self(shapes), report)
    }
}

impl FromValue for ShapeVec {
    fn from_value(v: Value) -> Result<Self> {
        let Value::Array(a) = v else {
            anyhow::bail!("expected an array of shapes, got {v}")
        };
        let (shapes, report) = Self::read(&a);
        if let Some(report) = report {
            log::error!(target: graphix_compiler::FAILURE_TARGET, "{report}");
        }
        Ok(shapes)
    }
}

graphix_rt::props! {
    struct Props {
        background: Option<ColorV>,
        height: LengthV,
        shapes: ShapeVec,
        width: LengthV,
    }
}

pub(crate) struct CanvasW<X: GXExt> {
    p: Props<X>,
    cache: iced_canvas::Cache<Renderer>,
    invalid_geometry: Cell<bool>,
}

impl<X: GXExt> CanvasW<X> {
    #[cfg(test)]
    pub(crate) fn shape_count(&self) -> Option<usize> {
        self.shapes().map(<[CanvasShape]>::len)
    }

    #[cfg(test)]
    pub(crate) fn has_invalid_geometry(&self) -> bool {
        self.invalid_geometry.get()
    }

    #[cfg(test)]
    pub(crate) fn shapes(&self) -> Option<&[CanvasShape]> {
        self.p.shapes.t.as_ref().map(|shapes| shapes.0.as_slice())
    }

    pub(crate) async fn compile(gx: GXHandle<X>, source: Value) -> Result<GuiW<X>> {
        let p = Props::compile(&gx, &source).await.context("canvas")?;
        Ok(GuiW::new(Self {
            p,
            cache: iced_canvas::Cache::new(),
            invalid_geometry: Cell::new(false),
        }))
    }
}

impl<X: GXExt> GuiWidget<X> for CanvasW<X> {
    #[cfg(test)]
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }

    fn handle_update(
        &mut self,
        _rt: &tokio::runtime::Handle,
        id: ExprId,
        v: &Value,
    ) -> Result<bool> {
        let changed = self.p.update(id, v).context("canvas")?;
        if id == self.p.shapes.r.id {
            self.invalid_geometry.set(false);
        }
        if id == self.p.shapes.r.id || id == self.p.background.r.id {
            self.cache.clear();
        }
        Ok(changed)
    }

    fn view(&self) -> IcedElement<'_> {
        let mut c = iced_canvas::Canvas::new(self);
        if let Some(w) = self.p.width.t.as_ref() {
            c = c.width(w.0);
        }
        if let Some(h) = self.p.height.t.as_ref() {
            c = c.height(h.0);
        }
        c.into()
    }
}

impl<X: GXExt> iced_canvas::Program<super::Message, crate::theme::GraphixTheme>
    for CanvasW<X>
{
    type State = ();

    fn draw(
        &self,
        _state: &Self::State,
        renderer: &Renderer,
        _theme: &crate::theme::GraphixTheme,
        bounds: Rectangle,
        _cursor: mouse::Cursor,
    ) -> Vec<iced_canvas::Geometry<Renderer>> {
        let geom = self.cache.draw(renderer, bounds.size(), |frame| {
            if let Some(Some(bg)) = self.p.background.t.as_ref() {
                frame.fill_rectangle(Point::ORIGIN, frame.size(), bg.0);
            }
            if let Some(shapes) = self.p.shapes.t.as_ref() {
                for shape in &shapes.0 {
                    if draw_shape(frame, shape).is_none() && !self.invalid_geometry.replace(true) {
                        log::error!(target: graphix_compiler::FAILURE_TARGET, "canvas skipped invalid geometry (extent limit: {MAX_EXTENT} logical pixels)");
                    }
                }
            }
        });
        vec![geom]
    }
}

// CR claude for claude: [bug/medium] A shape is tessellated whole, with no clipping to
// the canvas, at a fixed tolerance, so its cost grows with its size, not with what is
// visible. A filled circle of radius 1e12 in a 300x200 canvas takes about 1 s and 1 GB
// per draw (1e8: 10 ms, 1e10: 94 ms), and each 100x of radius costs 10x. A radius of
// 1e14 or more (any finite value passes Finite) freezes the GUI thread or exhausts
// memory. Arc, Ellipse, RoundedRect, the Bezier curves and Path curves behave the same.
// Bound the cost by the canvas: either refuse a shape whose extent passes a fixed bound
// and log it, as a non-finite shape is skipped, or clip the geometry to the frame
// before tessellating. probe:
// design/review-2026-10-26/repro/gui-widgets-a-canvas-huge-radius.rs (gui-widgets-a-01)
// 2026-10-27 claude: Eric ruled (2026-10-27, cr-questions round 2, q48, Canvas shapes
// much larger than the canvas): yes, refuse past a bound (the recommendation: refuse
// past a bound (simple, predictable); clipping later if needed.)
fn draw_shape(
    frame: &mut iced_widget::canvas::Frame<Renderer>,
    shape: &CanvasShape,
) -> Option<()> {
    use iced_widget::canvas::Stroke;

    fn stroke(s: &StrokeV) -> Stroke<'static> {
        Stroke::default().with_color(s.color.0).with_width(s.width.0)
    }
    if !shape.fits() {
        return None;
    }
    match shape {
        // XCR claude for claude: [bug/low] A finite line far past the canvas draws
        // nothing. A `Line` from x=-1e30 to x=1e30 at y 10..20 leaves the test
        // harness's 200x200 frame black and is not flagged as invalid geometry, while
        // the same line at x=±1e12 draws across rows 14 and 15. Found by the pixel
        // probes added for (tests-gui-a-01): canvas_huge_line_keeps_finite_endpoints
        // (±3e38) can only assert that the line is not skipped. Fix: clip a stroke's
        // segments, in f64, to a box far past any canvas before tessellating
        // (CheckedPath), or flag such geometry as invalid; which, and for which shapes
        // (lines, or every path), is to decide. (canvas-huge-line-01)
        // 2026-10-27 codex: Applied Eric's question-48 ruling: draw_shape refuses
        // every shape outside a conservative extent of 1,000,000 logical pixels and
        // logs through the existing invalid-geometry path. This is over 60 16K screen
        // widths from the origin, far beyond a useful canvas drawing, while keeping
        // curve tessellation far below the measured 1e12-radius allocation. Checks
        // include stroke widths, curve controls, circle/ellipse radii, rectangle
        // sizes, text position/size/content length and Path segments; CheckedPath also
        // rejects derived points outside the bound. Pin
        // widgets::canvas::path::tests::huge_finite_line_is_rejected reproduces the
        // finite +/-1e30 line and failed before the fix, then passed.
        // widgets::canvas::tests::every_shape_has_a_bounded_extent covers all shape
        // kinds; four widgets::canvas unit tests pass. Updated
        // canvas_huge_line_is_skipped for the new policy, but did not run GPU/runtime
        // harness tests in the socket-restricted sandbox. No image encoding changed.
        CanvasShape::Line { from, to, color, width } => {
            let path = CheckedPath::build(|b| {
                b.move_to(from.0);
                b.line_to(to.0);
            })?;
            frame
                .stroke(&path, Stroke::default().with_color(color.0).with_width(width.0));
        }
        CanvasShape::Circle { center, radius, fill, stroke: s } => {
            let path = CheckedPath::build(|b| b.circle(center.0, radius.0))?;
            if let Some(c) = fill {
                frame.fill(&path, c.0);
            }
            if let Some(s) = s {
                frame.stroke(&path, stroke(s));
            }
        }
        CanvasShape::Rect { top_left, size, fill, stroke: s } => {
            if !CheckedPath::rectangle_fits(top_left.0, size.0) {
                return None;
            }
            if let Some(c) = fill {
                frame.fill_rectangle(top_left.0, size.0, c.0);
            }
            if let Some(s) = s {
                let path = CheckedPath::build(|b| b.rectangle(top_left.0, size.0))?;
                frame.stroke(&path, stroke(s));
            }
        }
        CanvasShape::RoundedRect { top_left, size, radius, fill, stroke: s } => {
            let path = CheckedPath::build(|b| {
                b.rounded_rectangle(top_left.0, size.0, radius.0)
            })?;
            if let Some(c) = fill {
                frame.fill(&path, c.0);
            }
            if let Some(s) = s {
                frame.stroke(&path, stroke(s));
            }
        }
        CanvasShape::Arc { center, radius, start_angle, end_angle, stroke: s } => {
            let path = CheckedPath::build(|b| {
                b.arc(iced_widget::canvas::path::Arc {
                    center: center.0,
                    radius: radius.0,
                    start_angle: iced_core::Radians(start_angle.0),
                    end_angle: iced_core::Radians(end_angle.0),
                });
            })?;
            frame.stroke(&path, stroke(s));
        }
        CanvasShape::Ellipse {
            center,
            radii,
            rotation,
            start_angle,
            end_angle,
            fill,
            stroke: s,
        } => {
            let path = CheckedPath::build(|b| {
                b.ellipse(iced_widget::canvas::path::arc::Elliptical {
                    center: center.0,
                    radii: iced_core::Vector::new(radii.0.x, radii.0.y),
                    rotation: iced_core::Radians(rotation.0),
                    start_angle: iced_core::Radians(start_angle.0),
                    end_angle: iced_core::Radians(end_angle.0),
                });
            })?;
            if let Some(c) = fill {
                frame.fill(&path, c.0);
            }
            if let Some(s) = s {
                frame.stroke(&path, stroke(s));
            }
        }
        CanvasShape::BezierCurve { from, control_a, control_b, to, color, width } => {
            let path = CheckedPath::build(|b| {
                b.move_to(from.0);
                b.bezier_curve_to(control_a.0, control_b.0, to.0);
            })?;
            frame
                .stroke(&path, Stroke::default().with_color(color.0).with_width(width.0));
        }
        CanvasShape::QuadraticCurve { from, control, to, color, width } => {
            let path = CheckedPath::build(|b| {
                b.move_to(from.0);
                b.quadratic_curve_to(control.0, to.0);
            })?;
            frame
                .stroke(&path, Stroke::default().with_color(color.0).with_width(width.0));
        }
        CanvasShape::Text { content, position, color, size } => {
            frame.fill_text(iced_widget::canvas::Text {
                content: content.clone(),
                position: position.0,
                color: color.0,
                size: size.0.into(),
                ..iced_widget::canvas::Text::default()
            });
        }
        CanvasShape::Path { segments, fill, stroke: s } => {
            let path = CheckedPath::build(|b| {
                for seg in segments {
                    match seg {
                        PathSegment::MoveTo(p) => b.move_to(p.0),
                        PathSegment::LineTo(p) => b.line_to(p.0),
                        PathSegment::BezierTo { control_a, control_b, to } => {
                            b.bezier_curve_to(control_a.0, control_b.0, to.0);
                        }
                        PathSegment::QuadraticTo { control, to } => {
                            b.quadratic_curve_to(control.0, to.0);
                        }
                        PathSegment::ArcTo { a, b: bp, radius } => {
                            b.arc_to(a.0, bp.0, radius.0);
                        }
                        PathSegment::Close => b.close(),
                    }
                }
            })?;
            if let Some(c) = fill {
                frame.fill(&path, c.0);
            }
            if let Some(s) = s {
                frame.stroke(&path, stroke(s));
            }
        }
    }
    Some(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use iced_core::{Color, Size};

    #[test]
    fn every_shape_has_a_bounded_extent() {
        for (x, fits) in [(100.0, true), (1e30, false)] {
            let p = PointV(Point::new(x, x));
            let origin = PointV(Point::ORIGIN);
            let color = ColorV(Color::WHITE);
            let width = Finite(2.0);
            let radius = Finite(x);
            let stroke = StrokeV { color, width };
            let size = SizeV(Size::new(x, x));
            for shape in [
                CanvasShape::Line { from: p, to: origin, color, width },
                CanvasShape::Circle {
                    center: origin,
                    radius,
                    fill: Some(color),
                    stroke: None,
                },
                CanvasShape::Rect {
                    top_left: origin,
                    size,
                    fill: Some(color),
                    stroke: None,
                },
                CanvasShape::RoundedRect {
                    top_left: origin,
                    size,
                    radius: Finite(10.0),
                    fill: Some(color),
                    stroke: None,
                },
                CanvasShape::Arc {
                    center: origin,
                    radius,
                    start_angle: Finite(0.0),
                    end_angle: Finite(1.0),
                    stroke,
                },
                CanvasShape::Ellipse {
                    center: origin,
                    radii: p,
                    rotation: Finite(1.0),
                    start_angle: Finite(0.0),
                    end_angle: Finite(1.0),
                    fill: Some(color),
                    stroke: None,
                },
                CanvasShape::BezierCurve {
                    from: origin,
                    control_a: p,
                    control_b: origin,
                    to: origin,
                    color,
                    width,
                },
                CanvasShape::QuadraticCurve {
                    from: origin,
                    control: p,
                    to: origin,
                    color,
                    width,
                },
                CanvasShape::Text {
                    content: "text".into(),
                    position: p,
                    color,
                    size: TextSizeV(16.0),
                },
                CanvasShape::Path {
                    segments: vec![
                        PathSegment::MoveTo(origin),
                        PathSegment::ArcTo { a: p, b: origin, radius },
                    ],
                    fill: Some(color),
                    stroke: None,
                },
                CanvasShape::Line { from: origin, to: origin, color, width: radius },
                CanvasShape::Text {
                    content: "text".into(),
                    position: origin,
                    color,
                    size: TextSizeV(x),
                },
            ] {
                assert_eq!(shape.fits(), fits, "{shape:?}");
            }
        }
    }
}
