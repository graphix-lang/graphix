use super::{
    AlignmentV, BoundsV, LineV, MarkerV, StyleV, TuiW, TuiWidget, into_borrowed_line,
    layout::ConstraintV,
};
use anyhow::{Context, Result, bail};
use arcstr::ArcStr;
use async_trait::async_trait;
use futures::future::try_join_all;
use graphix_compiler::expr::ExprId;
use graphix_rt::{GXExt, GXHandle, Ref};
use netidx::publisher::{FromValue, Value};
use netidx_derive::FromValue;
use ratatui::{
    Frame,
    layout::Rect,
    widgets::{Axis, Chart, Dataset, GraphType, LegendPosition},
};
use smallvec::SmallVec;

#[derive(Clone, Copy)]
struct GraphTypeV(GraphType);

impl FromValue for GraphTypeV {
    fn from_value(v: Value) -> Result<Self> {
        let g = match &*v.cast_to::<ArcStr>()? {
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
        let p = match &*v.cast_to::<ArcStr>()? {
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
struct AxisV(Axis<'static>);

impl FromValue for AxisV {
    fn from_value(v: Value) -> Result<Self> {
        #[derive(FromValue)]
        struct Fields {
            bounds: BoundsV,
            labels: Option<Vec<LineV>>,
            labels_alignment: Option<AlignmentV>,
            style: Option<StyleV>,
            title: Option<LineV>,
        }
        let Fields { bounds, labels, labels_alignment, style, title } = v.cast_to()?;
        let mut axis = Axis::default().bounds([bounds.min, bounds.max]);
        if let Some(lbls) = labels {
            let lbls = lbls.into_iter().map(|l| l.0).collect::<Vec<_>>();
            axis = axis.labels(lbls);
        }
        if let Some(al) = labels_alignment {
            axis = axis.labels_alignment(al.0);
        }
        if let Some(st) = style {
            axis = axis.style(st.0);
        }
        if let Some(LineV(t)) = title {
            axis = axis.title(t);
        }
        Ok(Self(axis))
    }
}

#[derive(Clone, Copy, FromValue)]
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
        #[derive(FromValue)]
        struct Fields {
            data: u64,
        }
        let p = DatasetProps::compile(gx, &v).await.context("dataset")?;
        let Fields { data } = v.cast_to()?;
        let mut t = Self { p, data_ref: gx.compile_ref(data).await?, data: vec![] };
        if let Some(v) = t.data_ref.last.take() {
            t.set_data(&v)?;
        }
        Ok(t)
    }

    /// The finite points: ratatui paints a NaN at the canvas edge.
    fn set_data(&mut self, v: &Value) -> Result<()> {
        self.data.clear();
        match v {
            Value::Array(a) => {
                for v in a {
                    let (x, y) = v
                        .clone()
                        .cast_to::<(f64, f64)>()
                        .context("invalid dataset pair")?;
                    if x.is_finite() && y.is_finite() {
                        self.data.push((x, y));
                    }
                }
            }
            v => bail!("invalid dataset {v}"),
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

    fn build(&self) -> Dataset<'_> {
        let p = &self.p;
        let mut ds = Dataset::default().data(&self.data);
        if let Some(Some(LineV(l))) = &p.name.t {
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
    datasets: Vec<DatasetW<X>>,
}

impl<X: GXExt> ChartW<X> {
    pub(super) async fn compile(gx: GXHandle<X>, v: Value) -> Result<TuiW> {
        #[derive(FromValue)]
        struct Fields {
            datasets: u64,
        }
        let p = Props::compile(&gx, &v).await.context("chart")?;
        let Fields { datasets } = v.cast_to()?;
        let datasets_ref = gx.compile_ref(datasets).await?;
        let mut t = Self { gx, p, datasets_ref, datasets: vec![] };
        if let Some(v) = t.datasets_ref.last.take() {
            t.set_datasets(v).await?;
        }
        Ok(Box::new(t))
    }

    async fn set_datasets(&mut self, v: Value) -> Result<()> {
        let ds = v.cast_to::<SmallVec<[Value; 8]>>()?;
        self.datasets =
            try_join_all(ds.into_iter().map(|d| DatasetW::compile(&self.gx, d))).await?;
        Ok(())
    }
}

#[async_trait]
impl<X: GXExt> TuiWidget for ChartW<X> {
    async fn handle_update(&mut self, id: ExprId, v: Value) -> Result<()> {
        self.p.update(id, &v).context("chart")?;
        if self.datasets_ref.id == id {
            self.set_datasets(v.clone()).await?;
        }
        for d in &mut self.datasets {
            d.update(id, &v)?;
        }
        Ok(())
    }

    fn draw(&mut self, frame: &mut Frame, rect: Rect) -> Result<()> {
        let p = &self.p;
        let mut chart = Chart::new(self.datasets.iter().map(|d| d.build()).collect());
        if let Some(Some(h)) = &p.hidden_legend_constraints.t {
            chart = chart.hidden_legend_constraints((h.width.0, h.height.0));
        }
        if let Some(Some(lp)) = p.legend_position.t {
            chart = chart.legend_position(Some(lp.0));
        }
        if let Some(Some(s)) = &p.style.t {
            chart = chart.style(s.0);
        }
        if let Some(Some(a)) = &p.x_axis.t {
            chart = chart.x_axis(a.0.clone());
        }
        if let Some(Some(a)) = &p.y_axis.t {
            chart = chart.y_axis(a.0.clone());
        }
        frame.render_widget(chart, rect);
        Ok(())
    }
}
