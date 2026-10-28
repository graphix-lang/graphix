use super::{
    BoundsV, ColorV, LineV, MarkerV, PointV, TuiW, TuiWidget, clip::clip_line, reconcile,
};
use anyhow::{Context, Result};
use async_trait::async_trait;
use graphix_compiler::{BindId, expr::ExprId};
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

#[derive(Clone)]
struct CanvasLineV(Line);

impl FromValue for CanvasLineV {
    fn from_value(v: Value) -> Result<Self> {
        #[derive(FromValue)]
        #[value(shape = "graphix_compiler::rep::GraphixShape")]
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
        #[value(shape = "graphix_compiler::rep::GraphixShape")]
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
        #[value(shape = "graphix_compiler::rep::GraphixShape")]
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
        #[value(shape = "graphix_compiler::rep::GraphixShape")]
        struct Fields {
            color: ColorV,
            coords: Option<Vec<PointV>>,
        }
        let Fields { color, coords } = v.cast_to()?;
        let coords = coords
            .unwrap_or_default()
            .into_iter()
            .filter_map(|PointV(x, y)| (x.is_finite() && y.is_finite()).then_some((x, y)))
            .collect();
        Ok(Self { color, coords })
    }
}

#[derive(Clone, FromValue)]
#[value(shape = "graphix_compiler::rep::GraphixShape")]
struct CanvasLabelV {
    line: LineV,
    x: f64,
    y: f64,
}

#[derive(Clone, FromValue)]
#[value(shape = "graphix_compiler::rep::GraphixShape")]
enum ShapeV {
    Line(CanvasLineV),
    Circle(CanvasCircleV),
    Rectangle(CanvasRectangleV),
    Points(CanvasPointsV),
    Label(CanvasLabelV),
}

impl ShapeV {
    /// Whether the shape's coordinates are numbers ratatui can place.
    // XCR claude for claude: [bug/medium] This check, and DatasetW's breaks in chart.rs,
    // guards only x2 - x1 and y2 - y1. ratatui's clipper (line-clipping
    // cohen_sutherland) still computes (x_min - x1) * dy / dx, which overflows. Its
    // inf/inf NaN counts as inside and lands at column 0, so `Line{x1: -1e300, y1:
    // 1e300, x2: 5, y2: 5}` over 0..10 bounds is drawn from the bottom-left corner
    // instead of the top-left. A segment whose difference overflows is dropped even
    // when it crosses the window: the row y = 5 from x = -1e308 to 1e308 draws nothing.
    // The pins overflowing_canvas_geometry_is_empty and overflowing_chart_line_is_empty
    // assert that empty screen for a diagonal through the window. Decide: clip in
    // Graphix with a parametric clip whose intermediates stay inside the segment and
    // give ratatui only the inside part (draws these lines, flips both pins), or keep
    // 'draws nothing' and extend this test and the breaks to the clipper's products.
    // Either way, add an UPSTREAM.md entry. probe:
    // design/review-2026-10-26/repro/tui-widgets-canvas-line-clip.sh (tui-widgets-02)
    // 2026-10-26 claude: 2026-10-26 lane7: an_overflowing_pair_breaks_the_line now pins
    // only that each finite segment's cells are drawn (compared with a chart of that
    // segment alone), which holds under either option. Its render today also shows the
    // clipper's garbage: (4,4)->(-1e308,-1e308) and (1e308,1e308)->(6,6) draw a V from
    // the top-left corner through (4.5,5), lines no data names. Breaking where (dx *
    // dy) overflows would stop it, since the clipper's products are bounded by
    // |dx|*|dy|.
    // 2026-10-27 claude: Eric ruled (2026-10-27, cr-questions round 2, q53, TUI lines
    // that overflow the clipper): yes, clip and add upstream entry (the recommendation:
    // clip in Graphix; add an UPSTREAM.md entry.)
    // 2026-10-28 codex: Reworked clipping: original-endpoint edge intersections use
    // power-of-two scaling and a compensated determinant. Canvas lines and chart lines
    // draw their visible portions; connected chart pieces share one polyline/dataset,
    // with breaks only at disconnected visible runs. Pins:
    // clip::tests::asymmetric_segments_keep_their_slope failed on the supplied old
    // clipper (0.634413...,0);
    // canvas::tests::enormous_canvas_lines_match_their_visible_segments failed with the
    // original canvas path; connected_braille_segments_share_a_layer,
    // clipped_lines_break_only_at_disconnected_runs and large_line_uses_one_layer all
    // failed when forced back to one dataset per segment. They pass after fixing. The
    // 2000-point 200x50 Braille CPU render measured 235 us raw versus 268 us including
    // clipping (100 renders, dev profile; no allocation measurement). cargo test -p
    // graphix-package-tui --offline --lib: 151 passed, 8 failed, all in
    // test::input_test::a_browser_* and test::examples_test::every_example_renders,
    // with sandbox resolver/socket failures. Both updated overflow integration pins
    // passed. Book rebuilt and UPSTREAM entry added. GPU/socket-capable review remains
    // external.
    // 2026-10-28 codex: Second review: restored the unmerged rework from bbd7afa,
    // retaining clip.rs unchanged. DatasetW::build now returns one Dataset, for every
    // series. LineData clips to a finite box padded by one axis span and connects
    // disconnected pieces along its perimeter, outside the window; enormous axes and all
    // series scale down by powers of two to bound ratatui's products. Restored the true
    // build/name-width and text_room comments. Reproduced both new pins against the
    // rejected runs implementation: reentering_line_preserves_braille_dots lost cell
    // (1,0); reentering_line_uses_one_layer found 478 layers. Both pass now, and the
    // final pins exercise DatasetW::build directly (its single-Dataset return type
    // enforces the layer count). Added perimeter_joins_never_enter_the_window,
    // enormous_axes_keep_clipper_products_finite and
    // enormous_chart_axes_preserve_each_graph_type. The canvas enormous-line pin also
    // failed with the original draw/finite code restored and passed with clipping. Dev
    // CPU timings, 100 renders at 200x50 Braille: 2000 points (i/200,
    // 5+8*sin(150*i/200)), rejected runs 25.279512 ms, raw 419.261 us; fixed including
    // clip/build 447.882 us, raw 419.347 us. Canvas, 1999 segments from the same points:
    // original 375.678 us, fixed 416.312 us. cargo test -p graphix-package-tui --offline
    // --lib chart::tests -- --nocapture: 8 passed; canvas::tests: 3 passed;
    // enormous_chart_axes: 1 passed. Final cargo test -p graphix-package-tui --offline
    // --lib: 157 passed, 8 socket-dependent failures: test::input_test::a_browser_* and
    // test::examples_test::every_example_renders (browser_basic, browser_commands,
    // browser_navigation, paragraph_log_viewer, sparkline_rolling), with resolver
    // Operation not permitted errors. UPSTREAM.md and book updated; mdbook build and
    // cargo fmt --all completed. No terminal/socket-capable run or allocation
    // measurements; the requested graphix-tests lib gate is running and its results will
    // be in the final report.
    // 2026-10-28 claude: Review (2026-10-28) found a line along a window edge lost or
    // moved by rounding in clip::intersection, and an unclipped chart 30-80% slower than
    // main (every draw re-clipped and copied). Fixed on merge: a segment along a
    // constant coordinate clips exactly (pin
    // clip::tests::lines_along_a_constant_coordinate_clip_exactly, fails without), and a
    // series inside the padded box draws from its own data (LineData::raw). The commit's
    // 25 ms -> 0.45 ms was against the rejected attempt; against main the re-entering
    // case was 7-14% slower before the fast path.
    fn finite(&self) -> bool {
        let all = |cs: &[f64]| cs.iter().all(|c| c.is_finite());
        match self {
            ShapeV::Line(CanvasLineV(l)) => all(&[l.x1, l.y1, l.x2, l.y2]),
            ShapeV::Circle(CanvasCircleV(c)) => all(&[
                c.x,
                c.y,
                c.radius,
                c.x - c.radius,
                c.x + c.radius,
                c.y - c.radius,
                c.y + c.radius,
                2.0 * c.radius,
            ]),
            ShapeV::Rectangle(CanvasRectangleV(r)) => {
                all(&[r.x, r.y, r.width, r.height, r.x + r.width, r.y + r.height])
            }
            ShapeV::Label(l) => all(&[l.x, l.y]),
            ShapeV::Points(_) => true,
        }
    }

    fn draw(&self, ctx: &mut CanvasContext, x: [f64; 2], y: [f64; 2]) {
        match self {
            _ if !self.finite() => {}
            ShapeV::Line(CanvasLineV(l)) => {
                if let Some([(x1, y1), (x2, y2)]) =
                    clip_line((l.x1, l.y1), (l.x2, l.y2), x, y)
                {
                    ctx.draw(&Line { x1, y1, x2, y2, color: l.color });
                }
            }
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
    shapes: Vec<(Value, TRef<X, ShapeV>)>,
}

impl<X: GXExt> CanvasW<X> {
    pub(super) async fn compile(gx: GXHandle<X>, v: Value) -> Result<TuiW> {
        let p = Props::compile(&gx, &v).await.context("canvas")?;
        let shapes_ref = gx.compile_field(&v, "shapes").await?;
        let mut t = Self { gx, p, shapes_ref, shapes: Vec::new() };
        if let Some(v) = t.shapes_ref.last.take() {
            t.set_shapes(v).await?;
        }
        Ok(TuiW::new(t))
    }

    async fn set_shapes(&mut self, v: Value) -> Result<()> {
        reconcile(&mut self.shapes, v.cast_to()?, |id| {
            let gx = self.gx.clone();
            async move { TRef::new(gx.compile_ref(id.cast_to::<BindId>()?).await?) }
        })
        .await
    }
}

#[async_trait]
impl<X: GXExt> TuiWidget for CanvasW<X> {
    fn handle_update<'a>(
        &'a mut self,
        id: ExprId,
        v: &'a Value,
    ) -> Result<crate::PendingUpdate<'a>> {
        self.p.update(id, v).context("canvas")?;
        if self.shapes_ref.id == id {
            return Ok(Some(Box::pin(self.set_shapes(v.clone()))));
        }
        for (_, s) in &mut self.shapes {
            s.update(id, v).context("canvas shape")?;
        }
        Ok(None)
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
            for shape in shapes.iter().filter_map(|(_, s)| s.t.as_ref()) {
                shape.draw(ctx, bounds(p.x_bounds.t), bounds(p.y_bounds.t));
            }
        });
        frame.render_widget(canvas, rect);
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ratatui::style::Color;

    #[test]
    fn enormous_canvas_lines_match_their_visible_segments() {
        use ratatui::{buffer::Buffer, symbols::Marker, widgets::Widget};
        let render = |a: (f64, f64), b: (f64, f64)| {
            let shape = ShapeV::Line(CanvasLineV(Line {
                x1: a.0,
                y1: a.1,
                x2: b.0,
                y2: b.1,
                color: Color::Red,
            }));
            let rect = Rect::new(0, 0, 11, 11);
            let mut buffer = Buffer::empty(rect);
            Canvas::default()
                .marker(Marker::Braille)
                .x_bounds([0.0, 10.0])
                .y_bounds([0.0, 10.0])
                .paint(|ctx| shape.draw(ctx, [0.0, 10.0], [0.0, 10.0]))
                .render(rect, &mut buffer);
            buffer
        };
        for (a, b, c, d) in [
            ((-1e300, 1e300), (5.0, 5.0), (0.0, 10.0), (5.0, 5.0)),
            ((-1e308, 5.0), (1e308, 5.0), (0.0, 5.0), (10.0, 5.0)),
            ((-1e300, -0.7e300), (3e300, 2.1e300), (0.0, 0.0), (10.0, 7.0)),
        ] {
            assert_eq!(render(a, b), render(c, d), "{a:?} -> {b:?}");
        }
    }

    #[test]
    fn canvas_line_render_cost() {
        use poolshark::local::LPooled;
        use ratatui::{buffer::Buffer, symbols::Marker, widgets::Widget};
        use std::{hint::black_box, time::Instant};
        let point = |i| {
            let x = i as f64 / 200.0;
            (x, 5.0 + 8.0 * (150.0 * x).sin())
        };
        let shapes: LPooled<Vec<_>> = (0..1999)
            .map(|i| {
                let (x1, y1) = point(i);
                let (x2, y2) = point(i + 1);
                ShapeV::Line(CanvasLineV(Line { x1, y1, x2, y2, color: Color::Red }))
            })
            .collect();
        let rect = Rect::new(0, 0, 200, 50);
        let start = Instant::now();
        for _ in 0..100 {
            let mut buffer = Buffer::empty(rect);
            Canvas::default()
                .marker(Marker::Braille)
                .x_bounds([0.0, 10.0])
                .y_bounds([0.0, 10.0])
                .paint(|ctx| {
                    for shape in shapes.iter() {
                        shape.draw(ctx, [0.0, 10.0], [0.0, 10.0]);
                    }
                })
                .render(rect, &mut buffer);
            black_box(buffer);
        }
        eprintln!(
            "canvas k=150, 1999 segments, 200x50 Braille, 100 renders: {:?}",
            start.elapsed() / 100
        );
    }

    #[test]
    fn finite_shapes_require_finite_extents() {
        let circle = |x, radius| {
            ShapeV::Circle(CanvasCircleV(Circle { x, y: 0.0, radius, color: Color::Red }))
        };
        let rectangle = |x, width| {
            ShapeV::Rectangle(CanvasRectangleV(Rectangle {
                x,
                y: 0.0,
                width,
                height: 1.0,
                color: Color::Red,
            }))
        };
        assert!(circle(0.0, 1.0).finite());
        assert!(!circle(1e308, 1e308).finite());
        assert!(!circle(-1e308, 1e308).finite());
        assert!(rectangle(0.0, 1.0).finite());
        assert!(!rectangle(1e308, 1e308).finite());
    }
}
