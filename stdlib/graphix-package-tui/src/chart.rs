use super::{
    AlignmentV, BoundsV, LineV, MarkerV, PointV, StyleV, TuiW, TuiWidget,
    clip::clip_line, into_borrowed_line_within, layout::ConstraintV, reconcile,
    tag_newtype,
};
use anyhow::{Context, Result, anyhow};
use async_trait::async_trait;
use graphix_compiler::{expr::ExprId, rep};
use graphix_rt::{GXExt, GXHandle, Ref};
use netidx::publisher::{FromValue, Value};
use netidx_derive::FromValue;
use poolshark::local::LPooled;
use ratatui::{
    Frame,
    layout::Rect,
    widgets::{Axis, Chart, Dataset, GraphType, LegendPosition},
};

tag_newtype!(#[derive(Clone, Copy)] GraphTypeV(GraphType), "graph type", {
    "Scatter" => GraphType::Scatter,
    "Line" => GraphType::Line,
    "Bar" => GraphType::Bar,
});

tag_newtype!(#[derive(Clone, Copy)] LegendPositionV(LegendPosition), "legend position", {
    "Top" => LegendPosition::Top,
    "TopRight" => LegendPosition::TopRight,
    "TopLeft" => LegendPosition::TopLeft,
    "Left" => LegendPosition::Left,
    "Right" => LegendPosition::Right,
    "Bottom" => LegendPosition::Bottom,
    "BottomRight" => LegendPosition::BottomRight,
    "BottomLeft" => LegendPosition::BottomLeft,
});

#[derive(Clone)]
struct AxisV {
    axis: Axis<'static>,
    bounds: [f64; 2],
    labels: Vec<LineV>,
    title: Option<LineV>,
}

impl FromValue for AxisV {
    fn from_value(v: Value) -> Result<Self> {
        #[derive(FromValue)]
        #[value(shape = "graphix_compiler::rep::GraphixShape")]
        struct Fields {
            bounds: BoundsV,
            labels: Option<Vec<LineV>>,
            labels_alignment: Option<AlignmentV>,
            style: Option<StyleV>,
            title: Option<LineV>,
        }
        let Fields { bounds, labels, labels_alignment, style, title } = v.cast_to()?;
        let mut axis = Axis::default();
        if let Some(al) = labels_alignment {
            axis = axis.labels_alignment(al.0);
        }
        if let Some(st) = style {
            axis = axis.style(st.0);
        }
        Ok(Self {
            axis,
            bounds: [bounds.min, bounds.max],
            labels: labels.unwrap_or_default(),
            title,
        })
    }
}

impl AxisV {
    /// The axis with at most `room` of its labels, its text cut to `width`.
    fn build(&self, room: u16, width: u16) -> Axis<'_> {
        let count = self.labels.len().min(room as usize);
        let labels = (0..count)
            .map(|i| {
                let index =
                    if count < 2 { 0 } else { i * (self.labels.len() - 1) / (count - 1) };
                into_borrowed_line_within(&self.labels[index].0, width)
            })
            .collect::<Vec<_>>();
        let axis = self.axis.clone().bounds(self.bounds).labels(labels);
        match &self.title {
            Some(LineV(t)) => axis.title(into_borrowed_line_within(t, width)),
            None => axis,
        }
    }
}

#[derive(Clone, Copy, FromValue)]
#[value(shape = "graphix_compiler::rep::GraphixShape")]
struct HLConstraintsV {
    height: ConstraintV,
    width: ConstraintV,
}

graphix_rt::props! {
    struct DatasetProps {
        graph_type: Option<GraphTypeV>,
        marker: Option<MarkerV>,
        name: Option<LineV>,
        style: Option<StyleV>,
    }
}

#[derive(Clone, Copy)]
struct PlotBounds {
    x: [f64; 2],
    y: [f64; 2],
    scale: (f64, f64),
}

impl PlotBounds {
    fn new(x: [f64; 2], y: [f64; 2]) -> Self {
        let scale = |bounds: [f64; 2]| {
            let max = bounds[0].abs().max(bounds[1].abs());
            if max > 1e100 { f64::from_bits(max.to_bits() & (0x7ff << 52)) } else { 1.0 }
        };
        let scale = (scale(x), scale(y));
        Self { x: x.map(|v| v / scale.0), y: y.map(|v| v / scale.1), scale }
    }

    fn point(&self, p: (f64, f64)) -> (f64, f64) {
        (p.0 / self.scale.0, p.1 / self.scale.1)
    }
}

#[derive(Default)]
struct LineData {
    points: LPooled<Vec<(f64, f64)>>,
    /// The series needs no clipping: it is drawn from its own data.
    raw: bool,
}

impl LineData {
    fn clip(&mut self, data: &[(f64, f64)], bounds: PlotBounds) {
        self.points.clear();
        let pad = |[min, max]: [f64; 2]| [min - (max - min), max + (max - min)];
        let (x, y) = (pad(bounds.x), pad(bounds.y));
        self.raw = bounds.scale == (1.0, 1.0)
            && data
                .iter()
                .all(|p| p.0 >= x[0] && p.0 <= x[1] && p.1 >= y[0] && p.1 <= y[1]);
        if self.raw {
            return;
        }
        for pair in data.windows(2) {
            let Some([a, b]) =
                clip_line(bounds.point(pair[0]), bounds.point(pair[1]), x, y)
            else {
                continue;
            };
            if let Some(&last) = self.points.last() {
                if last != a {
                    self.join(last, a, x, y);
                    self.points.push(a);
                }
            } else {
                self.points.push(a);
            }
            self.points.push(b);
        }
        if let [p] = data {
            let p = bounds.point(*p);
            if p.0 >= bounds.x[0]
                && p.0 <= bounds.x[1]
                && p.1 >= bounds.y[0]
                && p.1 <= bounds.y[1]
            {
                self.points.push(p);
            }
        }
    }

    /// What the series draws: its own data, or the clipped points.
    fn drawn<'a>(&'a self, data: &'a [(f64, f64)]) -> &'a [(f64, f64)] {
        if self.raw { data } else { self.points.as_slice() }
    }

    // Disconnected endpoints lie on this box; each join stays on its perimeter,
    // strictly outside the axis bounds, so ratatui discards every join segment.
    fn join(&mut self, from: (f64, f64), to: (f64, f64), x: [f64; 2], y: [f64; 2]) {
        let edge = |p: (f64, f64)| {
            if p.1 == y[0] {
                0
            } else if p.0 == x[1] {
                1
            } else if p.1 == y[1] {
                2
            } else {
                debug_assert_eq!(p.0, x[0]);
                3
            }
        };
        let corners = [(x[1], y[0]), (x[1], y[1]), (x[0], y[1]), (x[0], y[0])];
        let mut side = edge(from);
        let end = edge(to);
        while side != end {
            self.points.push(corners[side]);
            side = (side + 1) % 4;
        }
    }
}

struct DatasetW<X: GXExt> {
    p: DatasetProps<X>,
    data_ref: Ref<X>,
    data: Vec<(f64, f64)>,
    line: LineData,
}

impl<X: GXExt> DatasetW<X> {
    async fn compile(gx: &GXHandle<X>, v: Value) -> Result<Self> {
        let p = DatasetProps::compile(gx, &v).await.context("dataset")?;
        let mut t = Self {
            p,
            data_ref: gx.compile_field(&v, "data").await?,
            data: vec![],
            line: LineData::default(),
        };
        if let Some(v) = t.data_ref.last.take() {
            t.set_data(&v)?;
        }
        Ok(t)
    }

    /// The finite points: ratatui paints a NaN at the canvas edge.
    fn set_data(&mut self, v: &Value) -> Result<()> {
        self.data.clear();
        let points = rep::array_elems(v).ok_or_else(|| {
            anyhow!("invalid dataset {}", graphix_compiler::typ::Naked(v))
        })?;
        for p in points.iter() {
            let PointV(x, y) = p.clone().cast_to().context("invalid dataset pair")?;
            if x.is_finite() && y.is_finite() {
                self.data.push((x, y));
            }
        }
        Ok(())
    }

    fn update(&mut self, id: ExprId, v: &Value) -> Result<()> {
        self.p.update(id, v).context("dataset")?;
        if id == self.data_ref.id {
            self.set_data(v)?;
        }
        Ok(())
    }

    fn is_line(&self) -> bool {
        matches!(self.p.graph_type.t.flatten(), Some(GraphTypeV(GraphType::Line)))
    }

    /// One dataset per series; its name is cut to `width`.
    fn build(&self, width: u16, bounds: PlotBounds) -> Dataset<'_> {
        let data = if self.is_line() {
            self.line.drawn(&self.data)
        } else if bounds.scale != (1.0, 1.0) {
            self.line.points.as_slice()
        } else {
            self.data.as_slice()
        };
        let p = &self.p;
        let mut ds = Dataset::default().data(data);
        if let Some(Some(LineV(l))) = &p.name.t {
            ds = ds.name(into_borrowed_line_within(l, width));
        }
        if let Some(Some(m)) = p.marker.t {
            ds = ds.marker(m.0);
        }
        if let Some(Some(g)) = p.graph_type.t {
            ds = ds.graph_type(g.0);
        }
        if let Some(Some(s)) = p.style.t {
            ds = ds.style(s.0);
        }
        ds
    }
}

graphix_rt::props! {
    struct Props {
        hidden_legend_constraints: Option<HLConstraintsV>,
        legend_position: Option<LegendPositionV>,
        style: Option<StyleV>,
        x_axis: Option<AxisV>,
        y_axis: Option<AxisV>,
    }
}

pub(super) struct ChartW<X: GXExt> {
    gx: GXHandle<X>,
    p: Props<X>,
    datasets_ref: Ref<X>,
    datasets: Vec<(Value, DatasetW<X>)>,
}

impl<X: GXExt> ChartW<X> {
    pub(super) async fn compile(gx: GXHandle<X>, v: Value) -> Result<TuiW> {
        let p = Props::compile(&gx, &v).await.context("chart")?;
        let datasets_ref = gx.compile_field(&v, "datasets").await?;
        let mut t = Self { gx, p, datasets_ref, datasets: vec![] };
        if let Some(v) = t.datasets_ref.last.take() {
            t.set_datasets(v).await?;
        }
        Ok(TuiW::new(t))
    }

    async fn set_datasets(&mut self, v: Value) -> Result<()> {
        reconcile(&mut self.datasets, v.cast_to()?, |d| DatasetW::compile(&self.gx, d))
            .await
    }
}

#[async_trait]
impl<X: GXExt> TuiWidget for ChartW<X> {
    fn handle_update<'a>(
        &'a mut self,
        id: ExprId,
        v: &'a Value,
    ) -> Result<crate::PendingUpdate<'a>> {
        self.p.update(id, v).context("chart")?;
        if self.datasets_ref.id == id {
            return Ok(Some(Box::pin(self.set_datasets(v.clone()))));
        }
        for (_, d) in &mut self.datasets {
            d.update(id, v)?;
        }
        Ok(None)
    }

    fn draw(&mut self, frame: &mut Frame, rect: Rect) -> Result<()> {
        let p = &self.p;
        let bounds = |axis: &Option<Option<AxisV>>| {
            axis.as_ref().and_then(|a| a.as_ref()).map_or([0.0, 0.0], |a| a.bounds)
        };
        let bounds = PlotBounds::new(bounds(&p.x_axis.t), bounds(&p.y_axis.t));
        for (_, d) in &mut self.datasets {
            if d.is_line() {
                d.line.clip(&d.data, bounds);
            } else if bounds.scale != (1.0, 1.0) {
                d.line.points.clear();
                d.line.points.extend(d.data.iter().map(|&p| bounds.point(p)));
            }
        }
        // the legend adds its border to its widest name
        let text_room = rect.width.saturating_sub(2);
        let mut chart = Chart::new(
            self.datasets.iter().map(|(_, d)| d.build(text_room, bounds)).collect(),
        );
        if let Some(Some(h)) = &p.hidden_legend_constraints.t {
            chart = chart.hidden_legend_constraints((h.width.0, h.height.0));
        }
        // XCR claude for claude: [bug/low] A null legend_position leaves ratatui's default
        // (TopRight), so no Graphix value can hide the legend of a chart whose datasets
        // are named: `#legend_position: &null` still draws the box. Decide what null
        // means. If it should hide, pass
        // `legend_position(p.legend_position.t.flatten().map(|lp| lp.0))` and make
        // chart.gx's default `TopRight. Otherwise document in chart.gxi how a legend is
        // hidden. probe:
        // design/review-2026-10-26/repro/tui-widgets-chart-legend-null.sh
        // (tui-widgets-06)
        // 2026-10-27 claude: Eric ruled (2026-10-27, cr-questions round 2, q54, A TUI
        // chart's null legend position): yes, null hides (the recommendation: null
        // hides.)
        // 2026-10-27 qwen: null hides the legend, as ruled. ChartW::draw now always calls
        // ratatui's legend_position with the resolved value, so a null (or absent)
        // legend_position is legend_position(None) and no legend box is drawn; a chart
        // with named datasets that wants one must say where. chart.gx defaults
        // #legend_position to `TopRight so a chart that never mentions the legend still
        // shows it there, and chart.gxi's /// doc for val chart now says that null hides
        // the legend and names the default. Tests in
        // stdlib/graphix-package-tui/src/test/widgets_test.rs render a chart with one
        // dataset named cpu over a 40x10 viewport:
        // a_null_legend_position_hides_the_legend (#legend_position: &null asserts cpu is
        // not drawn -- it failed before the chart.rs fix, with ratatui's default legend
        // box drawn at the top right) and a_legend_position_shows_the_legend (&`TopRight
        // asserts it is drawn). A third test,
        // a_chart_that_omits_the_legend_position_shows_the_legend, pins the new chart.gx
        // default for a chart that passes no legend_position; it fails if that default
        // goes back to null. cargo test -p graphix-package-tui: 144 passed, 8 failed. The
        // 8 (the seven input_test::a_browser_* tests and
        // examples_test::every_example_renders on browser_basic, browser_commands,
        // browser_navigation, paragraph_log_viewer, sparkline_rolling -- "draws nothing")
        // fail the same way on an unmodified checkout in this sandbox: they need a netidx
        // listing and there is no network here.
        chart = chart.legend_position(p.legend_position.t.flatten().map(|lp| lp.0));
        if let Some(Some(s)) = &p.style.t {
            chart = chart.style(s.0);
        }
        if let Some(Some(a)) = &p.x_axis.t {
            chart = chart.x_axis(a.build(rect.width, text_room).bounds(bounds.x));
        }
        if let Some(Some(a)) = &p.y_axis.t {
            chart = chart.y_axis(
                a.build(
                    (rect.height as u32).min(
                        u16::MAX as u32 / rect.height.saturating_sub(1).max(1) as u32 + 1,
                    ) as u16,
                    text_room,
                )
                .bounds(bounds.y),
            );
        }
        frame.render_widget(chart, rect);
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ratatui::{buffer::Buffer, text::Line, widgets::Widget};

    fn render_lines<'a>(
        runs: impl Iterator<Item = &'a [(f64, f64)]>,
        rect: Rect,
    ) -> Buffer {
        use ratatui::symbols::Marker;
        let datasets = runs
            .map(|data| {
                Dataset::default()
                    .data(data)
                    .graph_type(GraphType::Line)
                    .marker(Marker::Braille)
            })
            .collect();
        let mut buffer = Buffer::empty(rect);
        Chart::new(datasets)
            .x_axis(Axis::default().bounds([0.0, 10.0]))
            .y_axis(Axis::default().bounds([0.0, 10.0]))
            .render(rect, &mut buffer);
        buffer
    }

    #[test]
    fn connected_braille_segments_share_a_layer() {
        let data = [(2.0, 1.0), (2.3, 9.0), (2.6, 1.0), (8.0, 5.0), (2.0, 5.2)];
        let mut clipped = LineData::default();
        clipped.clip(&data, PlotBounds::new([0.0, 10.0], [0.0, 10.0]));
        let rect = Rect::new(0, 0, 11, 11);
        let expected = render_lines(std::iter::once(data.as_slice()), rect);
        assert_eq!(render_lines(std::iter::once(clipped.drawn(&data)), rect), expected);
    }

    #[test]
    fn disconnected_runs_join_outside_the_window() {
        let data = [(-1e308, 5.0), (5.0, 5.0), (30.0, 5.0), (30.0, 8.0), (5.0, 8.0)];
        let mut clipped = LineData::default();
        clipped.clip(&data, PlotBounds::new([0.0, 10.0], [0.0, 10.0]));
        let expected = [(-10.0, 5.0), (5.0, 5.0), (20.0, 5.0), (20.0, 8.0), (5.0, 8.0)];
        assert_eq!(&*clipped.points, &expected);
        let rect = Rect::new(0, 0, 11, 11);
        assert_eq!(
            render_lines(std::iter::once(clipped.points.as_slice()), rect),
            render_lines(std::iter::once(expected.as_slice()), rect)
        );
    }

    #[test]
    fn large_line_uses_one_layer() {
        use std::{hint::black_box, time::Instant};
        let points: LPooled<Vec<_>> = (0..2000)
            .map(|i| {
                let x = i as f64 / 200.0;
                (x, 5.0 + x.sin() * 4.0)
            })
            .collect();
        let mut clipped = LineData::default();
        clipped.clip(&points, PlotBounds::new([0.0, 10.0], [0.0, 10.0]));
        assert_eq!(clipped.drawn(&points).len(), points.len());
        let rect = Rect::new(0, 0, 200, 50);
        let start = Instant::now();
        for _ in 0..100 {
            black_box(render_lines(std::iter::once(points.as_slice()), rect));
        }
        let baseline = start.elapsed() / 100;
        let start = Instant::now();
        for _ in 0..100 {
            clipped.clip(&points, PlotBounds::new([0.0, 10.0], [0.0, 10.0]));
            black_box(render_lines(std::iter::once(clipped.drawn(&points)), rect));
        }
        eprintln!(
            "2000 points, 200x50: raw {baseline:?}, clipped {:?}",
            start.elapsed() / 100
        );
    }

    async fn line_series()
    -> Result<(graphix_package_core::testing::Fixture, DatasetW<graphix_rt::NoExt>)> {
        use graphix_package_core::testing::Fixture;
        let (f, value) = Fixture::new(
            "use tui::chart::dataset; let result = dataset(#graph_type: &`Line, #marker: &`Braille, &[(0.75, 9.6), (1.05, 30.0), (1.35, 9.6)])",
            &[&graphix_package_core::P, &graphix_package_array::P, &graphix_package_str::P, &graphix_package_sys::P, &crate::P],
            |_| {},
        ).await?;
        let series = DatasetW::compile(&f.gx, value).await?;
        Ok((f, series))
    }

    #[tokio::test]
    async fn reentering_line_preserves_braille_dots() -> Result<()> {
        let (_f, mut series) = line_series().await?;
        let bounds = PlotBounds::new([0.0, 10.0], [0.0, 10.0]);
        series.line.clip(&series.data, bounds);
        let rect = Rect::new(0, 0, 11, 11);
        let expected = render_lines(std::iter::once(series.data.as_slice()), rect);
        let mut actual = Buffer::empty(rect);
        let dataset: Dataset<'_> = series.build(rect.width, bounds);
        Chart::new(vec![dataset])
            .x_axis(Axis::default().bounds(bounds.x))
            .y_axis(Axis::default().bounds(bounds.y))
            .render(rect, &mut actual);
        assert_eq!(actual[(1, 0)], expected[(1, 0)]);
        assert_eq!(actual, expected);
        Ok(())
    }

    #[tokio::test]
    async fn reentering_line_uses_one_layer() -> Result<()> {
        use std::{hint::black_box, time::Instant};
        let (_f, mut series) = line_series().await?;
        series.data = (0..2000)
            .map(|i| {
                let x = i as f64 / 200.0;
                (x, 5.0 + (150.0 * x).sin() * 8.0)
            })
            .collect();
        let bounds = PlotBounds::new([0.0, 10.0], [0.0, 10.0]);
        let rect = Rect::new(0, 0, 200, 50);
        let start = Instant::now();
        for _ in 0..100 {
            black_box(render_lines(std::iter::once(series.data.as_slice()), rect));
        }
        let raw = start.elapsed() / 100;
        let start = Instant::now();
        for _ in 0..100 {
            series.line.clip(&series.data, bounds);
            let dataset: Dataset<'_> = series.build(rect.width, bounds);
            let mut buffer = Buffer::empty(rect);
            Chart::new(vec![dataset])
                .x_axis(Axis::default().bounds(bounds.x))
                .y_axis(Axis::default().bounds(bounds.y))
                .render(rect, &mut buffer);
            black_box(buffer);
        }
        eprintln!(
            "k=150, 2000 points, 200x50 Braille, 100 renders: raw {raw:?}, clipped {:?}",
            start.elapsed() / 100
        );
        assert!(series.line.points.iter().all(|&(x, y)|
            (-10.0..=20.0).contains(&x) && (-10.0..=20.0).contains(&y)));
        Ok(())
    }

    #[test]
    fn perimeter_joins_never_enter_the_window() {
        let outside = [(5.0, -40.0), (40.0, 5.0), (5.0, 40.0), (-40.0, 5.0)];
        let rect = Rect::new(0, 0, 11, 11);
        for a in outside {
            for b in outside {
                let data = [(5.0, 5.0), a, b, (5.0, 5.0)];
                let mut clipped = LineData::default();
                clipped.clip(&data, PlotBounds::new([0.0, 10.0], [0.0, 10.0]));
                assert_eq!(
                    render_lines(std::iter::once(clipped.drawn(&data)), rect),
                    render_lines(std::iter::once(data.as_slice()), rect),
                    "{a:?} -> {b:?}"
                );
            }
        }
    }

    #[test]
    fn enormous_axes_keep_clipper_products_finite() {
        let bounds = PlotBounds::new([0.0, 1e300], [0.0, 1e300]);
        let data = [(1e299, 1e299), (2e299, 1e308), (1e308, 1e308), (9e299, 9e299)];
        let mut clipped = LineData::default();
        clipped.clip(&data, bounds);
        assert!(!clipped.points.is_empty());
        for p in clipped.points.windows(2) {
            let dx = p[1].0 - p[0].0;
            let dy = p[1].1 - p[0].1;
            for x in bounds.x {
                assert!(((x - p[0].0) * dy).is_finite());
            }
            for y in bounds.y {
                assert!(((y - p[0].1) * dx).is_finite());
            }
        }
    }

    #[test]
    fn x_labels_do_not_truncate_their_count() {
        let mut labels = vec![LineV(Line::from("m")); 65_538];
        labels[0] = LineV(Line::from("f"));
        labels[65_537] = LineV(Line::from("z"));
        let axis =
            AxisV { axis: Axis::default(), bounds: [0.0, 1.0], labels, title: None };
        let rect = Rect::new(0, 0, 80, 10);
        let mut buffer = Buffer::empty(rect);
        Chart::default()
            .x_axis(axis.build(rect.width, rect.width))
            .render(rect, &mut buffer);
        assert_eq!(buffer[(0, 9)].symbol(), "f");
        Chart::default().x_axis(axis.build(2, rect.width)).render(rect, &mut buffer);
        assert_eq!(buffer[(79, 9)].symbol(), "z");
    }
}
