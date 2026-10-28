use super::tag_newtype;
use super::{
    AlignmentV, BoundsV, LineV, MarkerV, PointV, StyleV, TuiW, TuiWidget,
    into_borrowed_line_within, layout::ConstraintV, reconcile,
};
use anyhow::{Context, Result, anyhow};
use async_trait::async_trait;
use graphix_compiler::{expr::ExprId, rep};
use graphix_rt::{GXExt, GXHandle, Ref};
use netidx::publisher::{FromValue, Value};
use netidx_derive::FromValue;
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
        let mut axis = Axis::default().bounds([bounds.min, bounds.max]);
        if let Some(al) = labels_alignment {
            axis = axis.labels_alignment(al.0);
        }
        if let Some(st) = style {
            axis = axis.style(st.0);
        }
        Ok(Self { axis, labels: labels.unwrap_or_default(), title })
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
        let axis = self.axis.clone().labels(labels);
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

struct DatasetW<X: GXExt> {
    p: DatasetProps<X>,
    data_ref: Ref<X>,
    data: Vec<(f64, f64)>,
}

impl<X: GXExt> DatasetW<X> {
    async fn compile(gx: &GXHandle<X>, v: Value) -> Result<Self> {
        let p = DatasetProps::compile(gx, &v).await.context("dataset")?;
        let mut t =
            Self { p, data_ref: gx.compile_field(&v, "data").await?, data: vec![] };
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

    /// The datasets this series draws as: one, or for a line one per
    /// segment between its breaks, the first carrying the name. A line
    /// breaks before each point it cannot be joined to from the one before
    /// (their difference overflows). The name is cut to `width`.
    fn build(&self, width: u16) -> impl Iterator<Item = Dataset<'_>> {
        let connected =
            matches!(self.p.graph_type.t.flatten(), Some(GraphTypeV(GraphType::Line)));
        let joined: &[(f64, f64)] = if connected { &self.data } else { &[] };
        let breaks = joined
            .windows(2)
            .enumerate()
            .filter(|(_, p)| {
                !(p[1].0 - p[0].0).is_finite() || !(p[1].1 - p[0].1).is_finite()
            })
            .map(|(i, _)| i + 1);
        let starts = std::iter::once(0).chain(breaks.clone());
        let ends = breaks.chain(std::iter::once(self.data.len()));
        starts
            .zip(ends)
            .enumerate()
            .map(move |(i, (s, e))| self.segment(&self.data[s..e], i == 0, width))
    }

    fn segment<'a>(
        &'a self,
        data: &'a [(f64, f64)],
        named: bool,
        width: u16,
    ) -> Dataset<'a> {
        let p = &self.p;
        let mut ds = Dataset::default().data(data);
        if named && let Some(Some(LineV(l))) = &p.name.t {
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
        // the legend adds its border to its widest name
        let text_room = rect.width.saturating_sub(2);
        let mut chart = Chart::new(
            self.datasets.iter().flat_map(|(_, d)| d.build(text_room)).collect(),
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
            chart = chart.x_axis(a.build(rect.width, text_room));
        }
        if let Some(Some(a)) = &p.y_axis.t {
            chart = chart.y_axis(a.build(
                (rect.height as u32).min(
                    u16::MAX as u32 / rect.height.saturating_sub(1).max(1) as u32 + 1,
                ) as u16,
                text_room,
            ));
        }
        frame.render_widget(chart, rect);
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ratatui::{buffer::Buffer, text::Line, widgets::Widget};

    #[test]
    fn x_labels_do_not_truncate_their_count() {
        let mut labels = vec![LineV(Line::from("m")); 65_538];
        labels[0] = LineV(Line::from("f"));
        labels[65_537] = LineV(Line::from("z"));
        let axis =
            AxisV { axis: Axis::default().bounds([0.0, 1.0]), labels, title: None };
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
