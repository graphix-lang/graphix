use super::{
    AlignmentV, BoundsV, LineV, MarkerV, PointV, StyleV, TuiW, TuiWidget,
    into_borrowed_line, layout::ConstraintV, reconcile,
};
use anyhow::{Context, Result, anyhow, bail};
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

#[derive(Clone, Copy)]
struct GraphTypeV(GraphType);

impl FromValue for GraphTypeV {
    fn from_value(v: Value) -> Result<Self> {
        let g = match rep::bare_tag(&v)? {
            "Scatter" => GraphType::Scatter,
            "Line" => GraphType::Line,
            "Bar" => GraphType::Bar,
            s => bail!("invalid graphtype {s}"),
        };
        Ok(Self(g))
    }
}

#[derive(Clone, Copy)]
struct LegendPositionV(LegendPosition);

impl FromValue for LegendPositionV {
    fn from_value(v: Value) -> Result<Self> {
        let p = match rep::bare_tag(&v)? {
            "Top" => LegendPosition::Top,
            "TopRight" => LegendPosition::TopRight,
            "TopLeft" => LegendPosition::TopLeft,
            "Left" => LegendPosition::Left,
            "Right" => LegendPosition::Right,
            "Bottom" => LegendPosition::Bottom,
            "BottomRight" => LegendPosition::BottomRight,
            "BottomLeft" => LegendPosition::BottomLeft,
            s => bail!("invalid legend position {s}"),
        };
        Ok(Self(p))
    }
}

#[derive(Clone)]
struct AxisV {
    axis: Axis<'static>,
    labels: Vec<LineV>,
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
        if let Some(LineV(t)) = title {
            axis = axis.title(t);
        }
        Ok(Self { axis, labels: labels.unwrap_or_default() })
    }
}

impl AxisV {
    fn build(&self, room: u16) -> Axis<'_> {
        let count = self.labels.len().min(room as usize);
        let labels = (0..count)
            .map(|i| {
                let index =
                    if count < 2 { 0 } else { i * (self.labels.len() - 1) / (count - 1) };
                into_borrowed_line(&self.labels[index].0)
            })
            .collect::<Vec<_>>();
        self.axis.clone().labels(labels)
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
    /// Where a line breaks: each index starts a segment the point before
    /// it cannot be joined to (their difference overflows).
    breaks: Vec<usize>,
}

impl<X: GXExt> DatasetW<X> {
    async fn compile(gx: &GXHandle<X>, v: Value) -> Result<Self> {
        let p = DatasetProps::compile(gx, &v).await.context("dataset")?;
        let mut t = Self {
            p,
            data_ref: gx.compile_field(&v, "data").await?,
            data: vec![],
            breaks: vec![],
        };
        if let Some(v) = t.data_ref.last.take() {
            t.set_data(&v)?;
        }
        Ok(t)
    }

    /// The finite points: ratatui paints a NaN at the canvas edge.
    fn set_data(&mut self, v: &Value) -> Result<()> {
        // CR claude for claude: [risk/low] An early return below (array_elems failing
        // or a pair that does not cast) leaves `data` empty or partial while `breaks`
        // still indexes the old data. The display loop only logs an update error and
        // keeps drawing, so DatasetW::build's `self.data[s..e]` then panics. Clear
        // `breaks` together with `data` here, or build both into locals and assign them
        // only on success. Found by reading; not run. (tui-widgets-07)
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
        self.breaks.clear();
        for (i, pair) in self.data.windows(2).enumerate() {
            if !(pair[1].0 - pair[0].0).is_finite()
                || !(pair[1].1 - pair[0].1).is_finite()
            {
                self.breaks.push(i + 1);
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
    /// segment between its breaks, the first carrying the name.
    fn build(&self) -> impl Iterator<Item = Dataset<'_>> {
        let connected =
            matches!(self.p.graph_type.t.flatten(), Some(GraphTypeV(GraphType::Line)));
        let breaks: &[usize] = if connected { &self.breaks } else { &[] };
        let starts = std::iter::once(0).chain(breaks.iter().copied());
        let ends = breaks.iter().copied().chain(std::iter::once(self.data.len()));
        starts
            .zip(ends)
            .enumerate()
            .map(|(i, (s, e))| self.segment(&self.data[s..e], i == 0))
    }

    fn segment<'a>(&'a self, data: &'a [(f64, f64)], named: bool) -> Dataset<'a> {
        let p = &self.p;
        let mut ds = Dataset::default().data(data);
        if named && let Some(Some(LineV(l))) = &p.name.t {
            ds = ds.name(into_borrowed_line(l));
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
        let mut chart =
            Chart::new(self.datasets.iter().flat_map(|(_, d)| d.build()).collect());
        if let Some(Some(h)) = &p.hidden_legend_constraints.t {
            chart = chart.hidden_legend_constraints((h.width.0, h.height.0));
        }
        // CR claude for eric: [bug/low] A null legend_position leaves ratatui's default
        // (TopRight), so no Graphix value can hide the legend of a chart whose datasets
        // are named: `#legend_position: &null` still draws the box. Decide what null
        // means. If it should hide, pass
        // `legend_position(p.legend_position.t.flatten().map(|lp| lp.0))` and make
        // chart.gx's default `TopRight. Otherwise document in chart.gxi how a legend is
        // hidden. probe:
        // design/review-2026-10-26/repro/tui-widgets-chart-legend-null.sh
        // (tui-widgets-06)
        if let Some(Some(lp)) = p.legend_position.t {
            chart = chart.legend_position(Some(lp.0));
        }
        if let Some(Some(s)) = &p.style.t {
            chart = chart.style(s.0);
        }
        if let Some(Some(a)) = &p.x_axis.t {
            chart = chart.x_axis(a.build(rect.width));
        }
        // CR claude for claude: [bug/low] Label counts are bounded before ratatui sees
        // them, but text widths are not. ratatui adds widths in u16 (`w + 1` for an
        // axis title, `inner_width + 2` for the legend, `area.x + label_size + margin`
        // for a horizontal bar label in barchart.rs), so a 65535-wide y title, a
        // 65534-wide dataset name or a 65535-wide bar label panics the display under
        // overflow checks (dev, soak) and wraps in release. Cut titles, dataset names
        // and bar labels to the rect's width when building them (AxisV::build,
        // DatasetW::segment, BarProps::build), and add an UPSTREAM.md entry. probe:
        // design/review-2026-10-26/repro/tui-widgets-wide-text-overflow.sh
        // (tui-widgets-05)
        if let Some(Some(a)) = &p.y_axis.t {
            chart =
                chart.y_axis(a.build((rect.height as u32).min(
                    u16::MAX as u32 / rect.height.saturating_sub(1).max(1) as u32 + 1,
                ) as u16));
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
        let axis = AxisV { axis: Axis::default().bounds([0.0, 1.0]), labels };
        let rect = Rect::new(0, 0, 80, 10);
        let mut buffer = Buffer::empty(rect);
        Chart::default().x_axis(axis.build(rect.width)).render(rect, &mut buffer);
        assert_eq!(buffer[(0, 9)].symbol(), "f");
        Chart::default().x_axis(axis.build(2)).render(rect, &mut buffer);
        assert_eq!(buffer[(79, 9)].symbol(), "z");
    }
}
