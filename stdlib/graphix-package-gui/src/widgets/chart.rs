use super::{plotters_backend::IcedBackend, GuiW, GuiWidget, IcedElement, Renderer};
use crate::types::{ColorV, LengthV};
use anyhow::{bail, Context, Result};
use arcstr::ArcStr;
use graphix_compiler::expr::ExprId;
use graphix_rt::{GXExt, GXHandle, TRef};
use iced_core::mouse;
use iced_widget::canvas as iced_canvas;
use log::error;
use netidx::publisher::{FromValue, Value};
use plotters::{
    chart::ChartBuilder,
    element::PathElement,
    prelude::{AreaSeries, Circle, IntoDrawingArea, LineSeries},
    style::{Color as PlotColor, IntoFont, RGBColor, ShapeStyle, BLACK, WHITE},
};
use poolshark::local::LPooled;
use tokio::try_join;

#[derive(Clone, Debug)]
pub(crate) enum ChartType {
    Line,
    Scatter,
    Bar,
    Area,
}

impl FromValue for ChartType {
    fn from_value(v: Value) -> Result<Self> {
        match &*v.cast_to::<ArcStr>()? {
            "Line" => Ok(ChartType::Line),
            "Scatter" => Ok(ChartType::Scatter),
            "Bar" => Ok(ChartType::Bar),
            "Area" => Ok(ChartType::Area),
            s => bail!("invalid chart type: {s}"),
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) struct Dataset {
    // CR estokes: should the data be a ref, so the user can update the graph on
    // the fly easily, e.g. by building a realtime data set with the window
    // function?
    pub(crate) data: LPooled<Vec<(f64, f64)>>,
    pub(crate) chart_type: ChartType,
    pub(crate) color: Option<iced_core::Color>,
    pub(crate) label: Option<String>,
}

impl FromValue for Dataset {
    fn from_value(v: Value) -> Result<Self> {
        let [(_, chart_type), (_, color), (_, data), (_, label)] =
            v.cast_to::<[(ArcStr, Value); 4]>()?;
        let chart_type = ChartType::from_value(chart_type)?;
        let color =
            if color == Value::Null { None } else { Some(ColorV::from_value(color)?.0) };
        let data: LPooled<Vec<(f64, f64)>> = match data {
            Value::Array(a) => a
                .iter()
                .map(|v| v.clone().cast_to::<(f64, f64)>())
                .collect::<Result<_>>()?,
            _ => bail!("chart dataset: expected array for data"),
        };
        let label =
            if label == Value::Null { None } else { Some(label.cast_to::<String>()?) };
        Ok(Dataset { data, chart_type, color, label })
    }
}

#[derive(Clone, Debug)]
pub(crate) struct AxisRange {
    pub(crate) min: f64,
    pub(crate) max: f64,
}

impl FromValue for AxisRange {
    fn from_value(v: Value) -> Result<Self> {
        let [(_, max), (_, min)] = v.cast_to::<[(ArcStr, f64); 2]>()?;
        Ok(AxisRange { min, max })
    }
}

/// Newtype for Vec<Dataset> to satisfy orphan rules.
#[derive(Clone, Debug)]
pub(crate) struct DatasetVec(pub Vec<Dataset>);

impl FromValue for DatasetVec {
    fn from_value(v: Value) -> Result<Self> {
        let items = v.cast_to::<Vec<Value>>()?;
        let ds: Vec<Dataset> =
            items.into_iter().map(Dataset::from_value).collect::<Result<_>>()?;
        Ok(Self(ds))
    }
}

/// Newtype for Option<AxisRange> to satisfy orphan rules.
#[derive(Clone, Debug)]
pub(crate) struct OptAxisRange(pub Option<AxisRange>);

impl FromValue for OptAxisRange {
    fn from_value(v: Value) -> Result<Self> {
        if v == Value::Null {
            Ok(Self(None))
        } else {
            Ok(Self(Some(AxisRange::from_value(v)?)))
        }
    }
}

pub(crate) struct ChartW<X: GXExt> {
    datasets: TRef<X, DatasetVec>,
    title: TRef<X, Option<String>>,
    x_label: TRef<X, Option<String>>,
    y_label: TRef<X, Option<String>>,
    x_range: TRef<X, OptAxisRange>,
    y_range: TRef<X, OptAxisRange>,
    width: TRef<X, LengthV>,
    height: TRef<X, LengthV>,
    cache: iced_canvas::Cache<Renderer>,
}

impl<X: GXExt> ChartW<X> {
    pub(crate) async fn compile(gx: GXHandle<X>, source: Value) -> Result<GuiW<X>> {
        let [(_, datasets), (_, height), (_, title), (_, width), (_, x_label), (_, x_range), (_, y_label), (_, y_range)] =
            source.cast_to::<[(ArcStr, u64); 8]>().context("chart flds")?;
        let (datasets, height, title, width, x_label, x_range, y_label, y_range) = try_join! {
            gx.compile_ref(datasets),
            gx.compile_ref(height),
            gx.compile_ref(title),
            gx.compile_ref(width),
            gx.compile_ref(x_label),
            gx.compile_ref(x_range),
            gx.compile_ref(y_label),
            gx.compile_ref(y_range),
        }?;
        Ok(Box::new(Self {
            datasets: TRef::new(datasets).context("chart tref datasets")?,
            title: TRef::new(title).context("chart tref title")?,
            x_label: TRef::new(x_label).context("chart tref x_label")?,
            y_label: TRef::new(y_label).context("chart tref y_label")?,
            x_range: TRef::new(x_range).context("chart tref x_range")?,
            y_range: TRef::new(y_range).context("chart tref y_range")?,
            width: TRef::new(width).context("chart tref width")?,
            height: TRef::new(height).context("chart tref height")?,
            cache: iced_canvas::Cache::new(),
        }))
    }
}

impl<X: GXExt> GuiWidget<X> for ChartW<X> {
    fn handle_update(
        &mut self,
        _rt: &tokio::runtime::Handle,
        id: ExprId,
        v: &Value,
    ) -> Result<bool> {
        let mut changed = false;
        macro_rules! up {
            ($f:ident) => {
                if self
                    .$f
                    .update(id, v)
                    .context(concat!("chart update ", stringify!($f)))?
                    .is_some()
                {
                    self.cache.clear();
                    changed = true;
                }
            };
        }
        up!(datasets);
        up!(title);
        up!(x_label);
        up!(y_label);
        up!(x_range);
        up!(y_range);
        changed |= self.width.update(id, v).context("chart update width")?.is_some();
        changed |= self.height.update(id, v).context("chart update height")?.is_some();
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

/// Default colors for datasets when no color is specified.
const PALETTE: [RGBColor; 8] = [
    RGBColor(31, 119, 180),
    RGBColor(255, 127, 14),
    RGBColor(44, 160, 44),
    RGBColor(214, 39, 40),
    RGBColor(148, 103, 189),
    RGBColor(140, 86, 75),
    RGBColor(227, 119, 194),
    RGBColor(127, 127, 127),
];

fn iced_to_plotters(c: iced_core::Color) -> RGBColor {
    let [r, g, b, _] = c.into_rgba8();
    RGBColor(r, g, b)
}

impl<X: GXExt> iced_canvas::Program<super::Message> for ChartW<X> {
    type State = ();

    fn draw(
        &self,
        _state: &Self::State,
        renderer: &Renderer,
        _theme: &iced_core::Theme,
        bounds: iced_core::Rectangle,
        _cursor: mouse::Cursor,
    ) -> Vec<iced_canvas::Geometry<Renderer>> {
        let geom = self.cache.draw(renderer, bounds.size(), |frame| {
            let w = frame.width() as u32;
            let h = frame.height() as u32;
            if w == 0 || h == 0 {
                return;
            }
            let datasets = match self.datasets.t.as_ref() {
                Some(ds) if !ds.0.is_empty() => &ds.0,
                _ => return,
            };

            let (x_min, x_max) = match self.x_range.t.as_ref().and_then(|r| r.0.as_ref())
            {
                Some(r) => (r.min, r.max),
                None => auto_range(datasets, |p| p.0),
            };
            let (y_min, y_max) = match self.y_range.t.as_ref().and_then(|r| r.0.as_ref())
            {
                Some(r) => (r.min, r.max),
                None => auto_range(datasets, |p| p.1),
            };

            let backend = IcedBackend::new(frame, w, h);
            let root = backend.into_drawing_area();
            if let Err(e) = root.fill(&WHITE) {
                error!("chart fill: {e:?}");
                return;
            }

            let title = self.title.t.as_ref().and_then(|o| o.as_deref());
            let x_label = self.x_label.t.as_ref().and_then(|o| o.as_deref());
            let y_label = self.y_label.t.as_ref().and_then(|o| o.as_deref());

            let mut builder = ChartBuilder::on(&root);
            builder.margin(10);
            if let Some(t) = title {
                builder.caption(t, ("sans-serif", 16).into_font());
            }
            if x_label.is_some() || y_label.is_some() {
                builder.x_label_area_size(30);
                builder.y_label_area_size(50);
            }

            let mut chart = match builder.build_cartesian_2d(x_min..x_max, y_min..y_max) {
                Ok(c) => c,
                Err(_) => return,
            };

            let mut mesh = chart.configure_mesh();
            if let Some(xl) = x_label {
                mesh.x_desc(xl);
            }
            if let Some(yl) = y_label {
                mesh.y_desc(yl);
            }
            if let Err(e) = mesh.draw() {
                error!("chart mesh draw: {e:?}");
                return;
            }

            for (i, ds) in datasets.iter().enumerate() {
                let color =
                    ds.color.map(iced_to_plotters).unwrap_or(PALETTE[i % PALETTE.len()]);
                let label = ds.label.as_deref();
                let line_style = ShapeStyle::from(color).stroke_width(2);
                let fill_style = ShapeStyle::from(color).filled();

                match ds.chart_type {
                    ChartType::Line => {
                        let series = LineSeries::new(ds.data.iter().copied(), line_style);
                        match chart.draw_series(series) {
                            Ok(ann) => {
                                if let Some(l) = label {
                                    ann.label(l).legend(move |(x, y)| {
                                        PathElement::new(
                                            [(x, y), (x + 20, y)],
                                            line_style,
                                        )
                                    });
                                }
                            }
                            Err(e) => error!("chart draw line series: {e:?}"),
                        }
                    }
                    ChartType::Scatter => {
                        let series = ds
                            .data
                            .iter()
                            .map(|&(x, y)| Circle::new((x, y), 3, fill_style));
                        match chart.draw_series(series) {
                            Ok(ann) => {
                                if let Some(l) = label {
                                    ann.label(l).legend(move |(x, y)| {
                                        Circle::new((x, y), 3, fill_style)
                                    });
                                }
                            }
                            Err(e) => error!("chart draw scatter series: {e:?}"),
                        }
                    }
                    ChartType::Bar => {
                        let bar_width = if ds.data.len() > 1 {
                            let dx = (x_max - x_min) / ds.data.len() as f64;
                            dx * 0.8
                        } else {
                            1.0
                        };
                        let series = ds.data.iter().map(|&(x, y)| {
                            plotters::element::Rectangle::new(
                                [(x - bar_width / 2.0, 0.0), (x + bar_width / 2.0, y)],
                                fill_style,
                            )
                        });
                        match chart.draw_series(series) {
                            Ok(ann) => {
                                if let Some(l) = label {
                                    ann.label(l).legend(move |(x, y)| {
                                        plotters::element::Rectangle::new(
                                            [(x, y - 5), (x + 20, y + 5)],
                                            fill_style,
                                        )
                                    });
                                }
                            }
                            Err(e) => error!("chart draw bar series: {e:?}"),
                        }
                    }
                    ChartType::Area => {
                        let area_fill = color.mix(0.3);
                        let series = AreaSeries::new(
                            ds.data.iter().copied(),
                            0.0,
                            ShapeStyle::from(area_fill).filled(),
                        )
                        .border_style(line_style);
                        match chart.draw_series(series) {
                            Ok(ann) => {
                                if let Some(l) = label {
                                    ann.label(l).legend(move |(x, y)| {
                                        PathElement::new(
                                            [(x, y), (x + 20, y)],
                                            line_style,
                                        )
                                    });
                                }
                            }
                            Err(e) => error!("chart draw area series: {e:?}"),
                        }
                    }
                }
            }

            if datasets.iter().any(|ds| ds.label.is_some()) {
                if let Err(e) = chart
                    .configure_series_labels()
                    .background_style(WHITE.mix(0.8))
                    .border_style(BLACK)
                    .draw()
                {
                    error!("chart series labels draw: {e:?}");
                }
            }

            if let Err(e) = root.present() {
                error!("chart present: {e:?}");
            }
        });
        vec![geom]
    }
}

pub(crate) fn auto_range(
    datasets: &[Dataset],
    f: impl Fn(&(f64, f64)) -> f64,
) -> (f64, f64) {
    let mut min = f64::INFINITY;
    let mut max = f64::NEG_INFINITY;
    for ds in datasets {
        for pt in ds.data.iter() {
            let v = f(pt);
            if v < min {
                min = v;
            }
            if v > max {
                max = v;
            }
        }
    }
    if min == max {
        min -= 1.0;
        max += 1.0;
    }
    let pad = (max - min) * 0.05;
    (min - pad, max + pad)
}
