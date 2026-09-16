use super::{BoundsV, ColorV, LineV, MarkerV, TuiW, TuiWidget};
use anyhow::{Context, Result};
use async_trait::async_trait;
use crossterm::event::Event;
use futures::future::try_join_all;
use graphix_compiler::expr::ExprId;
use graphix_rt::{GXExt, GXHandle, Ref, TRef};
use netidx::publisher::{FromValue, Value};
use netidx_derive::FromValue;
use ratatui::{
    Frame,
    layout::Rect,
    widgets::canvas::{
        Canvas, Circle, Context as CanvasContext, Line, Points, Rectangle,
    },
};
use smallvec::SmallVec;
use tokio::try_join;

#[derive(Clone)]
struct CanvasLineV(Line);

impl FromValue for CanvasLineV {
    fn from_value(v: Value) -> Result<Self> {
        #[derive(FromValue)]
        struct Fields {
            color: ColorV,
            x1: f64,
            x2: f64,
            y1: f64,
            y2: f64,
        }
        let Fields { color, x1, x2, y1, y2 } = v.cast_to()?;
        Ok(Self(Line { x1, y1, x2, y2, color: color.0 }))
    }
}

#[derive(Clone)]
struct CanvasCircleV(Circle);

impl FromValue for CanvasCircleV {
    fn from_value(v: Value) -> Result<Self> {
        #[derive(FromValue)]
        struct Fields {
            color: ColorV,
            radius: f64,
            x: f64,
            y: f64,
        }
        let Fields { color, radius, x, y } = v.cast_to()?;
        Ok(Self(Circle { x, y, radius, color: color.0 }))
    }
}

#[derive(Clone)]
struct CanvasRectangleV(Rectangle);

impl FromValue for CanvasRectangleV {
    fn from_value(v: Value) -> Result<Self> {
        #[derive(FromValue)]
        struct Fields {
            color: ColorV,
            height: f64,
            width: f64,
            x: f64,
            y: f64,
        }
        let Fields { color, height, width, x, y } = v.cast_to()?;
        Ok(Self(Rectangle { x, y, width, height, color: color.0 }))
    }
}

#[derive(Clone, FromValue)]
struct CanvasPointsV {
    color: ColorV,
    coords: Option<Vec<(f64, f64)>>,
}

#[derive(Clone, FromValue)]
struct CanvasLabelV {
    line: LineV,
    x: f64,
    y: f64,
}

#[derive(Clone, FromValue)]
enum ShapeV {
    Line(CanvasLineV),
    Circle(CanvasCircleV),
    Rectangle(CanvasRectangleV),
    Points(CanvasPointsV),
    Label(CanvasLabelV),
}

impl ShapeV {
    fn draw(&self, ctx: &mut CanvasContext) {
        match self {
            ShapeV::Line(s) => {
                ctx.draw(&s.0);
            }
            ShapeV::Circle(s) => {
                ctx.draw(&s.0);
            }
            ShapeV::Rectangle(s) => {
                ctx.draw(&s.0);
            }
            ShapeV::Points(s) => {
                let coords = s.coords.as_deref().unwrap_or_default();
                let points = Points { coords, color: s.color.0 };
                ctx.draw(&points);
            }
            ShapeV::Label(s) => {
                ctx.print(s.x, s.y, s.line.0.clone());
            }
        }
    }
}

struct ShapeRef<X: GXExt> {
    r: Ref<X>,
    shape: Option<ShapeV>,
}

impl<X: GXExt> ShapeRef<X> {
    fn update(&mut self, id: ExprId, v: &Value) -> Result<()> {
        if self.r.id == id {
            self.shape = Some(v.clone().cast_to::<ShapeV>()?);
        }
        Ok(())
    }
}

pub(super) struct CanvasW<X: GXExt> {
    gx: GXHandle<X>,
    shapes_ref: Ref<X>,
    shapes: Vec<ShapeRef<X>>,
    background_color: TRef<X, Option<ColorV>>,
    marker: TRef<X, Option<MarkerV>>,
    x_bounds: TRef<X, BoundsV>,
    y_bounds: TRef<X, BoundsV>,
}

impl<X: GXExt> CanvasW<X> {
    pub(super) async fn compile(gx: GXHandle<X>, v: Value) -> Result<TuiW> {
        #[derive(FromValue)]
        struct Fields {
            background_color: u64,
            marker: u64,
            shapes: u64,
            x_bounds: u64,
            y_bounds: u64,
        }
        let Fields { background_color, marker, shapes, x_bounds, y_bounds } =
            v.cast_to()?;
        let (background_color, marker, shapes_ref, x_bounds, y_bounds) = try_join! {
            gx.compile_ref(background_color),
            gx.compile_ref(marker),
            gx.compile_ref(shapes),
            gx.compile_ref(x_bounds),
            gx.compile_ref(y_bounds)
        }?;
        let mut t = Self {
            gx: gx.clone(),
            shapes_ref,
            shapes: Vec::new(),
            background_color: TRef::new(background_color)?,
            marker: TRef::new(marker)?,
            x_bounds: TRef::new(x_bounds)?,
            y_bounds: TRef::new(y_bounds)?,
        };
        if let Some(v) = t.shapes_ref.last.take() {
            t.set_shapes(v).await?;
        }
        Ok(Box::new(t))
    }

    async fn set_shapes(&mut self, v: Value) -> Result<()> {
        let ids = v.cast_to::<SmallVec<[u64; 8]>>()?;
        let refs = try_join_all(ids.into_iter().map(|id| {
            let gx = self.gx.clone();
            async move {
                let mut r = gx.compile_ref(id).await?;
                let shape = match r.last.take() {
                    Some(v) => Some(v.cast_to::<ShapeV>()?),
                    None => None,
                };
                Ok::<ShapeRef<X>, anyhow::Error>(ShapeRef { r, shape })
            }
        }))
        .await?;
        self.shapes = refs;
        Ok(())
    }
}

#[async_trait]
impl<X: GXExt> TuiWidget for CanvasW<X> {
    async fn handle_event(&mut self, _e: Event, _v: Value) -> Result<()> {
        Ok(())
    }

    async fn handle_update(&mut self, id: ExprId, v: Value) -> Result<()> {
        self.background_color.update(id, &v).context("canvas background update")?;
        self.marker.update(id, &v).context("canvas marker update")?;
        self.x_bounds.update(id, &v).context("canvas x_bounds update")?;
        self.y_bounds.update(id, &v).context("canvas y_bounds update")?;
        if self.shapes_ref.id == id {
            self.set_shapes(v.clone()).await?;
        }
        for s in &mut self.shapes {
            s.update(id, &v)?;
        }
        Ok(())
    }

    fn draw(&mut self, frame: &mut Frame, rect: Rect) -> Result<()> {
        let x_bounds = self.x_bounds.t.map_or([0.0, 0.0], |b| [b.min, b.max]);
        let y_bounds = self.y_bounds.t.map_or([0.0, 0.0], |b| [b.min, b.max]);
        let bg = self.background_color.t.as_ref().and_then(|c| c.as_ref()).cloned();
        let marker = self.marker.t.as_ref().and_then(|m| m.as_ref()).cloned();
        let shapes = &self.shapes;
        let mut canvas = Canvas::default().x_bounds(x_bounds).y_bounds(y_bounds);
        if let Some(c) = bg {
            canvas = canvas.background_color(c.0);
        }
        if let Some(m) = marker {
            canvas = canvas.marker(m.0);
        }
        canvas = canvas.paint(|ctx| {
            for s in shapes {
                if let Some(shape) = &s.shape {
                    shape.draw(ctx);
                }
            }
        });
        frame.render_widget(canvas, rect);
        Ok(())
    }
}
