use super::{GuiW, GuiWidget, IcedElement, Renderer};
use crate::types::{ColorV, LengthV};
use anyhow::{bail, Context, Result};
use arcstr::ArcStr;
use graphix_compiler::expr::ExprId;
use graphix_rt::{GXExt, GXHandle, TRef};
use iced_core::{mouse, Color, Point, Rectangle, Size};
use netidx::publisher::{FromValue, Value};
use smallvec::SmallVec;
use tokio::try_join;

// Use full paths to avoid ambiguity with our module name
use iced_widget::canvas as iced_canvas;

#[derive(Clone, Debug)]
pub(crate) enum CanvasShape {
    Line {
        from: Point,
        to: Point,
        color: Color,
        width: f32,
    },
    Circle {
        center: Point,
        radius: f32,
        fill: Option<Color>,
        stroke: Option<(Color, f32)>,
    },
    Rect {
        top_left: Point,
        size: Size,
        fill: Option<Color>,
        stroke: Option<(Color, f32)>,
    },
    RoundedRect {
        top_left: Point,
        size: Size,
        radius: f32,
        fill: Option<Color>,
        stroke: Option<(Color, f32)>,
    },
    Arc {
        center: Point,
        radius: f32,
        start_angle: f32,
        end_angle: f32,
        stroke: (Color, f32),
    },
    Ellipse {
        center: Point,
        radii: Point,
        rotation: f32,
        start_angle: f32,
        end_angle: f32,
        fill: Option<Color>,
        stroke: Option<(Color, f32)>,
    },
    BezierCurve {
        from: Point,
        control_a: Point,
        control_b: Point,
        to: Point,
        color: Color,
        width: f32,
    },
    QuadraticCurve {
        from: Point,
        control: Point,
        to: Point,
        color: Color,
        width: f32,
    },
    Text {
        content: String,
        position: Point,
        color: Color,
        size: f32,
    },
    Path {
        segments: Vec<PathSegment>,
        fill: Option<Color>,
        stroke: Option<(Color, f32)>,
    },
}

#[derive(Clone, Debug)]
pub(crate) enum PathSegment {
    MoveTo(Point),
    LineTo(Point),
    BezierTo { control_a: Point, control_b: Point, to: Point },
    QuadraticTo { control: Point, to: Point },
    ArcTo { a: Point, b: Point, radius: f32 },
    Close,
}

impl FromValue for CanvasShape {
    fn from_value(v: Value) -> Result<Self> {
        let (tag, val) = v.cast_to::<(ArcStr, Value)>()?;
        match &*tag {
            "Line" => {
                let [(_, color), (_, from), (_, to), (_, width)] =
                    val.cast_to::<[(ArcStr, Value); 4]>()?;
                let ColorV(color) = ColorV::from_value(color)?;
                let (fx, fy) = parse_point(from)?;
                let (tx, ty) = parse_point(to)?;
                let width = width.cast_to::<f64>()? as f32;
                Ok(CanvasShape::Line {
                    from: Point::new(fx, fy),
                    to: Point::new(tx, ty),
                    color,
                    width,
                })
            }
            "Circle" => {
                let [(_, center), (_, fill), (_, radius), (_, stroke)] =
                    val.cast_to::<[(ArcStr, Value); 4]>()?;
                let (cx, cy) = parse_point(center)?;
                let radius = radius.cast_to::<f64>()? as f32;
                let fill = parse_opt_color(fill)?;
                let stroke = parse_opt_stroke(stroke)?;
                Ok(CanvasShape::Circle {
                    center: Point::new(cx, cy),
                    radius,
                    fill,
                    stroke,
                })
            }
            "Rect" => {
                let [(_, fill), (_, size), (_, stroke), (_, top_left)] =
                    val.cast_to::<[(ArcStr, Value); 4]>()?;
                let (x, y) = parse_point(top_left)?;
                let [(_, h), (_, w)] = size.cast_to::<[(ArcStr, f64); 2]>()?;
                let fill = parse_opt_color(fill)?;
                let stroke = parse_opt_stroke(stroke)?;
                Ok(CanvasShape::Rect {
                    top_left: Point::new(x, y),
                    size: Size::new(w as f32, h as f32),
                    fill,
                    stroke,
                })
            }
            "RoundedRect" => {
                let [(_, fill), (_, radius), (_, size), (_, stroke), (_, top_left)] =
                    val.cast_to::<[(ArcStr, Value); 5]>()?;
                let (x, y) = parse_point(top_left)?;
                let [(_, h), (_, w)] = size.cast_to::<[(ArcStr, f64); 2]>()?;
                let radius = radius.cast_to::<f64>()? as f32;
                let fill = parse_opt_color(fill)?;
                let stroke = parse_opt_stroke(stroke)?;
                Ok(CanvasShape::RoundedRect {
                    top_left: Point::new(x, y),
                    size: Size::new(w as f32, h as f32),
                    radius,
                    fill,
                    stroke,
                })
            }
            "Arc" => {
                let [(_, center), (_, end_angle), (_, radius), (_, start_angle), (_, stroke)] =
                    val.cast_to::<[(ArcStr, Value); 5]>()?;
                let (cx, cy) = parse_point(center)?;
                let radius = radius.cast_to::<f64>()? as f32;
                let start_angle = start_angle.cast_to::<f64>()? as f32;
                let end_angle = end_angle.cast_to::<f64>()? as f32;
                let [(_, color), (_, width)] =
                    stroke.cast_to::<[(ArcStr, Value); 2]>()?;
                let ColorV(color) = ColorV::from_value(color)?;
                let width = width.cast_to::<f64>()? as f32;
                Ok(CanvasShape::Arc {
                    center: Point::new(cx, cy),
                    radius,
                    start_angle,
                    end_angle,
                    stroke: (color, width),
                })
            }
            "Ellipse" => {
                let [(_, center), (_, end_angle), (_, fill), (_, radii), (_, rotation), (_, start_angle), (_, stroke)] =
                    val.cast_to::<[(ArcStr, Value); 7]>()?;
                let (cx, cy) = parse_point(center)?;
                let (rx, ry) = parse_point(radii)?;
                let rotation = rotation.cast_to::<f64>()? as f32;
                let start_angle = start_angle.cast_to::<f64>()? as f32;
                let end_angle = end_angle.cast_to::<f64>()? as f32;
                let fill = parse_opt_color(fill)?;
                let stroke = parse_opt_stroke(stroke)?;
                Ok(CanvasShape::Ellipse {
                    center: Point::new(cx, cy),
                    radii: Point::new(rx, ry),
                    rotation,
                    start_angle,
                    end_angle,
                    fill,
                    stroke,
                })
            }
            "BezierCurve" => {
                let [(_, color), (_, control_a), (_, control_b), (_, from), (_, to), (_, width)] =
                    val.cast_to::<[(ArcStr, Value); 6]>()?;
                let ColorV(color) = ColorV::from_value(color)?;
                let (fx, fy) = parse_point(from)?;
                let (ax, ay) = parse_point(control_a)?;
                let (bx, by) = parse_point(control_b)?;
                let (tx, ty) = parse_point(to)?;
                let width = width.cast_to::<f64>()? as f32;
                Ok(CanvasShape::BezierCurve {
                    from: Point::new(fx, fy),
                    control_a: Point::new(ax, ay),
                    control_b: Point::new(bx, by),
                    to: Point::new(tx, ty),
                    color,
                    width,
                })
            }
            "QuadraticCurve" => {
                let [(_, color), (_, control), (_, from), (_, to), (_, width)] =
                    val.cast_to::<[(ArcStr, Value); 5]>()?;
                let ColorV(color) = ColorV::from_value(color)?;
                let (fx, fy) = parse_point(from)?;
                let (cx, cy) = parse_point(control)?;
                let (tx, ty) = parse_point(to)?;
                let width = width.cast_to::<f64>()? as f32;
                Ok(CanvasShape::QuadraticCurve {
                    from: Point::new(fx, fy),
                    control: Point::new(cx, cy),
                    to: Point::new(tx, ty),
                    color,
                    width,
                })
            }
            "Text" => {
                let [(_, color), (_, content), (_, position), (_, size)] =
                    val.cast_to::<[(ArcStr, Value); 4]>()?;
                let ColorV(color) = ColorV::from_value(color)?;
                let content = content.cast_to::<String>()?;
                let (px, py) = parse_point(position)?;
                let size = size.cast_to::<f64>()? as f32;
                Ok(CanvasShape::Text {
                    content,
                    position: Point::new(px, py),
                    color,
                    size,
                })
            }
            "Path" => {
                let [(_, fill), (_, segments), (_, stroke)] =
                    val.cast_to::<[(ArcStr, Value); 3]>()?;
                let fill = parse_opt_color(fill)?;
                let stroke = parse_opt_stroke(stroke)?;
                let seg_items = segments.cast_to::<Vec<Value>>()?;
                let segments = seg_items
                    .into_iter()
                    .map(PathSegment::from_value)
                    .collect::<Result<_>>()?;
                Ok(CanvasShape::Path { segments, fill, stroke })
            }
            s => bail!("invalid canvas shape tag: {s}"),
        }
    }
}

impl FromValue for PathSegment {
    fn from_value(v: Value) -> Result<Self> {
        let (tag, val) = v.cast_to::<(ArcStr, Value)>()?;
        match &*tag {
            "MoveTo" => {
                let (x, y) = parse_point(val)?;
                Ok(PathSegment::MoveTo(Point::new(x, y)))
            }
            "LineTo" => {
                let (x, y) = parse_point(val)?;
                Ok(PathSegment::LineTo(Point::new(x, y)))
            }
            "BezierTo" => {
                let [(_, control_a), (_, control_b), (_, to)] =
                    val.cast_to::<[(ArcStr, Value); 3]>()?;
                let (ax, ay) = parse_point(control_a)?;
                let (bx, by) = parse_point(control_b)?;
                let (tx, ty) = parse_point(to)?;
                Ok(PathSegment::BezierTo {
                    control_a: Point::new(ax, ay),
                    control_b: Point::new(bx, by),
                    to: Point::new(tx, ty),
                })
            }
            "QuadraticTo" => {
                let [(_, control), (_, to)] = val.cast_to::<[(ArcStr, Value); 2]>()?;
                let (cx, cy) = parse_point(control)?;
                let (tx, ty) = parse_point(to)?;
                Ok(PathSegment::QuadraticTo {
                    control: Point::new(cx, cy),
                    to: Point::new(tx, ty),
                })
            }
            "ArcTo" => {
                let [(_, a), (_, b), (_, radius)] =
                    val.cast_to::<[(ArcStr, Value); 3]>()?;
                let (ax, ay) = parse_point(a)?;
                let (bx, by) = parse_point(b)?;
                let radius = radius.cast_to::<f64>()? as f32;
                Ok(PathSegment::ArcTo {
                    a: Point::new(ax, ay),
                    b: Point::new(bx, by),
                    radius,
                })
            }
            "Close" => Ok(PathSegment::Close),
            s => bail!("invalid path segment tag: {s}"),
        }
    }
}

fn parse_point(v: Value) -> Result<(f32, f32)> {
    let [(_, x), (_, y)] = v.cast_to::<[(ArcStr, f64); 2]>()?;
    Ok((x as f32, y as f32))
}

fn parse_opt_color(v: Value) -> Result<Option<Color>> {
    if v == Value::Null {
        Ok(None)
    } else {
        Ok(Some(ColorV::from_value(v)?.0))
    }
}

fn parse_opt_stroke(v: Value) -> Result<Option<(Color, f32)>> {
    if v == Value::Null {
        Ok(None)
    } else {
        let [(_, color), (_, width)] = v.cast_to::<[(ArcStr, Value); 2]>()?;
        let ColorV(c) = ColorV::from_value(color)?;
        let w = width.cast_to::<f64>()? as f32;
        Ok(Some((c, w)))
    }
}

/// Newtype for Vec<CanvasShape> to satisfy orphan rules.
#[derive(Clone, Debug)]
pub(crate) struct ShapeVec(pub Vec<CanvasShape>);

impl FromValue for ShapeVec {
    fn from_value(v: Value) -> Result<Self> {
        let items = v.cast_to::<SmallVec<[Value; 8]>>()?;
        let shapes: Vec<CanvasShape> =
            items.into_iter().map(CanvasShape::from_value).collect::<Result<_>>()?;
        Ok(Self(shapes))
    }
}

pub(crate) struct CanvasW<X: GXExt> {
    shapes: TRef<X, ShapeVec>,
    width: TRef<X, LengthV>,
    height: TRef<X, LengthV>,
    background: TRef<X, Option<ColorV>>,
    cache: iced_canvas::Cache<Renderer>,
}

impl<X: GXExt> CanvasW<X> {
    pub(crate) async fn compile(gx: GXHandle<X>, source: Value) -> Result<GuiW<X>> {
        let [(_, background), (_, height), (_, shapes), (_, width)] =
            source.cast_to::<[(ArcStr, u64); 4]>().context("canvas flds")?;
        let (background, height, shapes, width) = try_join! {
            gx.compile_ref(background),
            gx.compile_ref(height),
            gx.compile_ref(shapes),
            gx.compile_ref(width),
        }?;
        Ok(Box::new(Self {
            shapes: TRef::new(shapes).context("canvas tref shapes")?,
            width: TRef::new(width).context("canvas tref width")?,
            height: TRef::new(height).context("canvas tref height")?,
            background: TRef::new(background).context("canvas tref background")?,
            cache: iced_canvas::Cache::new(),
        }))
    }
}

impl<X: GXExt> GuiWidget<X> for CanvasW<X> {
    fn handle_update(
        &mut self,
        _rt: &tokio::runtime::Handle,
        id: ExprId,
        v: &Value,
    ) -> Result<bool> {
        let mut changed = false;
        if self.shapes.update(id, v).context("canvas update shapes")?.is_some() {
            self.cache.clear();
            changed = true;
        }
        if self.background.update(id, v).context("canvas update background")?.is_some() {
            self.cache.clear();
            changed = true;
        }
        changed |= self.width.update(id, v).context("canvas update width")?.is_some();
        changed |= self.height.update(id, v).context("canvas update height")?.is_some();
        Ok(changed)
    }

    fn view(&self) -> IcedElement<'_> {
        let mut c = iced_canvas::Canvas::new(self);
        if let Some(w) = self.width.t.as_ref() {
            c = c.width(w.0);
        }
        if let Some(h) = self.height.t.as_ref() {
            c = c.height(h.0);
        }
        c.into()
    }
}

impl<X: GXExt> iced_canvas::Program<super::Message> for CanvasW<X> {
    type State = ();

    fn draw(
        &self,
        _state: &Self::State,
        renderer: &Renderer,
        _theme: &iced_core::Theme,
        bounds: Rectangle,
        _cursor: mouse::Cursor,
    ) -> Vec<iced_canvas::Geometry<Renderer>> {
        let geom = self.cache.draw(renderer, bounds.size(), |frame| {
            if let Some(Some(bg)) = self.background.t.as_ref() {
                frame.fill_rectangle(Point::ORIGIN, frame.size(), bg.0);
            }
            if let Some(shapes) = self.shapes.t.as_ref() {
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

    match shape {
        CanvasShape::Line { from, to, color, width } => {
            let path = Path::line(*from, *to);
            frame.stroke(&path, Stroke::default().with_color(*color).with_width(*width));
        }
        CanvasShape::Circle { center, radius, fill, stroke } => {
            let path = Path::circle(*center, *radius);
            if let Some(c) = fill {
                frame.fill(&path, *c);
            }
            if let Some((c, w)) = stroke {
                frame.stroke(&path, Stroke::default().with_color(*c).with_width(*w));
            }
        }
        CanvasShape::Rect { top_left, size, fill, stroke } => {
            if let Some(c) = fill {
                frame.fill_rectangle(*top_left, *size, *c);
            }
            if let Some((c, w)) = stroke {
                let path = Path::rectangle(*top_left, *size);
                frame.stroke(&path, Stroke::default().with_color(*c).with_width(*w));
            }
        }
        CanvasShape::RoundedRect { top_left, size, radius, fill, stroke } => {
            let border_radius = iced_core::border::Radius::from(*radius);
            let path = Path::rounded_rectangle(*top_left, *size, border_radius);
            if let Some(c) = fill {
                frame.fill(&path, *c);
            }
            if let Some((c, w)) = stroke {
                frame.stroke(&path, Stroke::default().with_color(*c).with_width(*w));
            }
        }
        CanvasShape::Arc { center, radius, start_angle, end_angle, stroke } => {
            let path = Path::new(|b| {
                b.arc(iced_widget::canvas::path::Arc {
                    center: *center,
                    radius: *radius,
                    start_angle: iced_core::Radians(*start_angle),
                    end_angle: iced_core::Radians(*end_angle),
                });
            });
            let (c, w) = stroke;
            frame.stroke(&path, Stroke::default().with_color(*c).with_width(*w));
        }
        CanvasShape::Ellipse {
            center,
            radii,
            rotation,
            start_angle,
            end_angle,
            fill,
            stroke,
        } => {
            let path = Path::new(|b| {
                b.ellipse(iced_widget::canvas::path::arc::Elliptical {
                    center: *center,
                    radii: iced_core::Vector::new(radii.x, radii.y),
                    rotation: iced_core::Radians(*rotation),
                    start_angle: iced_core::Radians(*start_angle),
                    end_angle: iced_core::Radians(*end_angle),
                });
            });
            if let Some(c) = fill {
                frame.fill(&path, *c);
            }
            if let Some((c, w)) = stroke {
                frame.stroke(&path, Stroke::default().with_color(*c).with_width(*w));
            }
        }
        CanvasShape::BezierCurve { from, control_a, control_b, to, color, width } => {
            let path = Path::new(|b| {
                b.move_to(*from);
                b.bezier_curve_to(*control_a, *control_b, *to);
            });
            frame.stroke(&path, Stroke::default().with_color(*color).with_width(*width));
        }
        CanvasShape::QuadraticCurve { from, control, to, color, width } => {
            let path = Path::new(|b| {
                b.move_to(*from);
                b.quadratic_curve_to(*control, *to);
            });
            frame.stroke(&path, Stroke::default().with_color(*color).with_width(*width));
        }
        CanvasShape::Text { content, position, color, size } => {
            frame.fill_text(iced_widget::canvas::Text {
                content: content.clone(),
                position: *position,
                color: *color,
                size: (*size).into(),
                ..iced_widget::canvas::Text::default()
            });
        }
        CanvasShape::Path { segments, fill, stroke } => {
            let path = Path::new(|b| {
                for seg in segments {
                    match seg {
                        PathSegment::MoveTo(p) => b.move_to(*p),
                        PathSegment::LineTo(p) => b.line_to(*p),
                        PathSegment::BezierTo { control_a, control_b, to } => {
                            b.bezier_curve_to(*control_a, *control_b, *to);
                        }
                        PathSegment::QuadraticTo { control, to } => {
                            b.quadratic_curve_to(*control, *to);
                        }
                        PathSegment::ArcTo { a, b: bp, radius } => {
                            b.arc_to(*a, *bp, *radius);
                        }
                        PathSegment::Close => b.close(),
                    }
                }
            });
            if let Some(c) = fill {
                frame.fill(&path, *c);
            }
            if let Some((c, w)) = stroke {
                frame.stroke(&path, Stroke::default().with_color(*c).with_width(*w));
            }
        }
    }
}
