use super::{BoundsV, ColorV, LineV, MarkerV, TuiW, TuiWidget};
use anyhow::{Context, Result};
use async_trait::async_trait;
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

/// The finite points: ratatui paints a NaN at the canvas edge.
#[derive(Clone)]
struct CanvasPointsV {
    color: ColorV,
    coords: Vec<(f64, f64)>,
}

impl FromValue for CanvasPointsV {
    fn from_value(v: Value) -> Result<Self> {
        #[derive(FromValue)]
        struct Fields {
            color: ColorV,
            coords: Option<Vec<(f64, f64)>>,
        }
        let Fields { color, coords } = v.cast_to()?;
        let mut coords = coords.unwrap_or_default();
        coords.retain(|(x, y)| x.is_finite() && y.is_finite());
        Ok(Self { color, coords })
    }
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
    /// Whether the shape's coordinates are numbers ratatui can place.
    fn finite(&self) -> bool {
        let all = |cs: &[f64]| cs.iter().all(|c| c.is_finite());
        match self {
            ShapeV::Line(CanvasLineV(l)) => all(&[l.x1, l.y1, l.x2, l.y2]),
            ShapeV::Circle(CanvasCircleV(c)) => all(&[c.x, c.y, c.radius]),
            ShapeV::Rectangle(CanvasRectangleV(r)) => all(&[r.x, r.y, r.width, r.height]),
            ShapeV::Label(l) => all(&[l.x, l.y]),
            ShapeV::Points(_) => true,
        }
    }

    fn draw(&self, ctx: &mut CanvasContext) {
        match self {
            _ if !self.finite() => {}
            ShapeV::Line(s) => ctx.draw(&s.0),
            ShapeV::Circle(s) => ctx.draw(&s.0),
            ShapeV::Rectangle(s) => ctx.draw(&s.0),
            ShapeV::Points(s) => {
                ctx.draw(&Points { coords: &s.coords, color: s.color.0 });
            }
            ShapeV::Label(s) => {
                ctx.print(s.x, s.y, s.line.0.clone());
            }
        }
    }
}

graphix_rt::props! {
    struct Props {
        background_color: Option<ColorV>,
        marker: Option<MarkerV>,
        x_bounds: BoundsV,
        y_bounds: BoundsV,
    }
}

pub(super) struct CanvasW<X: GXExt> {
    gx: GXHandle<X>,
    p: Props<X>,
    shapes_ref: Ref<X>,
    shapes: Vec<TRef<X, ShapeV>>,
}

impl<X: GXExt> CanvasW<X> {
    pub(super) async fn compile(gx: GXHandle<X>, v: Value) -> Result<TuiW> {
        #[derive(FromValue)]
        struct Fields {
            shapes: u64,
        }
        let p = Props::compile(&gx, &v).await.context("canvas")?;
        let Fields { shapes } = v.cast_to()?;
        let shapes_ref = gx.compile_ref(shapes).await?;
        let mut t = Self { gx, p, shapes_ref, shapes: Vec::new() };
        if let Some(v) = t.shapes_ref.last.take() {
            t.set_shapes(v).await?;
        }
        Ok(Box::new(t))
    }

    async fn set_shapes(&mut self, v: Value) -> Result<()> {
        let ids = v.cast_to::<SmallVec<[u64; 8]>>()?;
        let refs =
            try_join_all(ids.into_iter().map(|id| self.gx.compile_ref(id))).await?;
        self.shapes = refs.into_iter().map(TRef::new).collect::<Result<_>>()?;
        Ok(())
    }
}

#[async_trait]
impl<X: GXExt> TuiWidget for CanvasW<X> {
    async fn handle_update(&mut self, id: ExprId, v: Value) -> Result<()> {
        self.p.update(id, &v).context("canvas")?;
        if self.shapes_ref.id == id {
            self.set_shapes(v.clone()).await?;
        }
        for s in &mut self.shapes {
            s.update(id, &v).context("canvas shape")?;
        }
        Ok(())
    }

    fn draw(&mut self, frame: &mut Frame, rect: Rect) -> Result<()> {
        let p = &self.p;
        let bounds = |b: Option<BoundsV>| b.map_or([0.0, 0.0], |b| [b.min, b.max]);
        let mut canvas = Canvas::default()
            .x_bounds(bounds(p.x_bounds.t))
            .y_bounds(bounds(p.y_bounds.t));
        if let Some(Some(c)) = p.background_color.t {
            canvas = canvas.background_color(c.0);
        }
        if let Some(Some(m)) = p.marker.t {
            canvas = canvas.marker(m.0);
        }
        let shapes = &self.shapes;
        canvas = canvas.paint(|ctx| {
            for shape in shapes.iter().filter_map(|s| s.t.as_ref()) {
                shape.draw(ctx);
            }
        });
        frame.render_widget(canvas, rect);
        Ok(())
    }
}
