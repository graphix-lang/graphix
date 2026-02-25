use crate::{
    types::{ColorV, LengthV},
    GuiW, GuiWidget, IcedElement, Renderer,
};
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
    Text {
        content: String,
        position: Point,
        color: Color,
        size: f32,
    },
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
            s => bail!("invalid canvas shape tag: {s}"),
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
        let shapes: Vec<CanvasShape> = items
            .into_iter()
            .map(CanvasShape::from_value)
            .collect::<Result<_>>()?;
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

impl<X: GXExt> iced_canvas::Program<crate::Message> for CanvasW<X> {
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
                frame.fill_rectangle(
                    Point::ORIGIN,
                    frame.size(),
                    bg.0,
                );
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

fn draw_shape(
    frame: &mut iced_widget::canvas::Frame<Renderer>,
    shape: &CanvasShape,
) {
    use iced_widget::canvas::{Path, Stroke};

    match shape {
        CanvasShape::Line { from, to, color, width } => {
            let path = Path::line(*from, *to);
            frame.stroke(
                &path,
                Stroke::default().with_color(*color).with_width(*width),
            );
        }
        CanvasShape::Circle { center, radius, fill, stroke } => {
            let path = Path::circle(*center, *radius);
            if let Some(c) = fill {
                frame.fill(&path, *c);
            }
            if let Some((c, w)) = stroke {
                frame.stroke(
                    &path,
                    Stroke::default().with_color(*c).with_width(*w),
                );
            }
        }
        CanvasShape::Rect { top_left, size, fill, stroke } => {
            if let Some(c) = fill {
                frame.fill_rectangle(*top_left, *size, *c);
            }
            if let Some((c, w)) = stroke {
                let path = Path::rectangle(*top_left, *size);
                frame.stroke(
                    &path,
                    Stroke::default().with_color(*c).with_width(*w),
                );
            }
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
    }
}
