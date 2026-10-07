use super::{GuiW, GuiWidget, IcedElement, Renderer};
use crate::types::{ColorV, Finite, LengthV, SizeV, TextSizeV};
use anyhow::{Context, Result};
use graphix_compiler::expr::ExprId;
use graphix_rt::{GXExt, GXHandle};
use iced_core::{Point, Rectangle, mouse};
use netidx::publisher::{FromValue, Value};
use netidx_derive::FromValue;

use iced_widget::canvas as iced_canvas;

#[derive(Clone, Copy, Debug)]
pub(crate) struct PointV(pub(crate) Point);

impl FromValue for PointV {
    fn from_value(v: Value) -> Result<Self> {
        #[derive(FromValue)]
        struct Fields {
            x: Finite,
            y: Finite,
        }
        let Fields { x, y } = v.cast_to()?;
        Ok(Self(Point::new(x.0, y.0)))
    }
}

#[derive(Clone, Copy, Debug, FromValue)]
pub(crate) struct StrokeV {
    color: ColorV,
    width: Finite,
}

#[derive(Clone, Debug, FromValue)]
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
pub(crate) enum PathSegment {
    MoveTo(PointV),
    LineTo(PointV),
    BezierTo { control_a: PointV, control_b: PointV, to: PointV },
    QuadraticTo { control: PointV, to: PointV },
    ArcTo { a: PointV, b: PointV, radius: Finite },
    Close,
}

#[derive(Clone, Debug, FromValue)]
pub(crate) struct ShapeVec(pub Vec<CanvasShape>);

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
}

impl<X: GXExt> CanvasW<X> {
    pub(crate) async fn compile(gx: GXHandle<X>, source: Value) -> Result<GuiW<X>> {
        let p = Props::compile(&gx, &source).await.context("canvas")?;
        Ok(Box::new(Self { p, cache: iced_canvas::Cache::new() }))
    }
}

impl<X: GXExt> GuiWidget<X> for CanvasW<X> {
    fn handle_update(
        &mut self,
        _rt: &tokio::runtime::Handle,
        id: ExprId,
        v: &Value,
    ) -> Result<bool> {
        let changed = self.p.update(id, v).context("canvas")?;
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
                    draw_shape(frame, shape);
                }
            }
        });
        vec![geom]
    }
}

fn draw_shape(frame: &mut iced_widget::canvas::Frame<Renderer>, shape: &CanvasShape) {
    use iced_widget::canvas::{Path, Stroke};

    fn stroke(s: &StrokeV) -> Stroke<'static> {
        Stroke::default().with_color(s.color.0).with_width(s.width.0)
    }
    match shape {
        CanvasShape::Line { from, to, color, width } => {
            let path = Path::line(from.0, to.0);
            frame
                .stroke(&path, Stroke::default().with_color(color.0).with_width(width.0));
        }
        CanvasShape::Circle { center, radius, fill, stroke: s } => {
            let path = Path::circle(center.0, radius.0);
            if let Some(c) = fill {
                frame.fill(&path, c.0);
            }
            if let Some(s) = s {
                frame.stroke(&path, stroke(s));
            }
        }
        CanvasShape::Rect { top_left, size, fill, stroke: s } => {
            if let Some(c) = fill {
                frame.fill_rectangle(top_left.0, size.0, c.0);
            }
            if let Some(s) = s {
                let path = Path::rectangle(top_left.0, size.0);
                frame.stroke(&path, stroke(s));
            }
        }
        CanvasShape::RoundedRect { top_left, size, radius, fill, stroke: s } => {
            let border_radius = iced_core::border::Radius::from(radius.0);
            let path = Path::rounded_rectangle(top_left.0, size.0, border_radius);
            if let Some(c) = fill {
                frame.fill(&path, c.0);
            }
            if let Some(s) = s {
                frame.stroke(&path, stroke(s));
            }
        }
        CanvasShape::Arc { center, radius, start_angle, end_angle, stroke: s } => {
            let path = Path::new(|b| {
                b.arc(iced_widget::canvas::path::Arc {
                    center: center.0,
                    radius: radius.0,
                    start_angle: iced_core::Radians(start_angle.0),
                    end_angle: iced_core::Radians(end_angle.0),
                });
            });
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
            let path = Path::new(|b| {
                b.ellipse(iced_widget::canvas::path::arc::Elliptical {
                    center: center.0,
                    radii: iced_core::Vector::new(radii.0.x, radii.0.y),
                    rotation: iced_core::Radians(rotation.0),
                    start_angle: iced_core::Radians(start_angle.0),
                    end_angle: iced_core::Radians(end_angle.0),
                });
            });
            if let Some(c) = fill {
                frame.fill(&path, c.0);
            }
            if let Some(s) = s {
                frame.stroke(&path, stroke(s));
            }
        }
        CanvasShape::BezierCurve { from, control_a, control_b, to, color, width } => {
            let path = Path::new(|b| {
                b.move_to(from.0);
                b.bezier_curve_to(control_a.0, control_b.0, to.0);
            });
            frame
                .stroke(&path, Stroke::default().with_color(color.0).with_width(width.0));
        }
        CanvasShape::QuadraticCurve { from, control, to, color, width } => {
            let path = Path::new(|b| {
                b.move_to(from.0);
                b.quadratic_curve_to(control.0, to.0);
            });
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
            let path = Path::new(|b| {
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
            });
            if let Some(c) = fill {
                frame.fill(&path, c.0);
            }
            if let Some(s) = s {
                frame.stroke(&path, stroke(s));
            }
        }
    }
}
