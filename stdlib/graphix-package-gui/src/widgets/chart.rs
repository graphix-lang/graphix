use super::{
    plotters_backend::{estimate_text, IcedBackend},
    GuiW, GuiWidget, IcedElement, Renderer,
};
use crate::types::{ColorV, LengthV};
use anyhow::{bail, Context, Result};
use arcstr::ArcStr;
use graphix_compiler::expr::ExprId;
use graphix_rt::{GXExt, GXHandle, Ref, TRef};
use iced_core::mouse;
use iced_widget::canvas as iced_canvas;
use log::error;
use netidx::publisher::{FromValue, Value};
use plotters::{
    chart::ChartBuilder,
    element::{CandleStick, ErrorBar, PathElement},
    prelude::{
        AreaSeries, Circle, DashedLineSeries, IntoDrawingArea, LineSeries,
        SeriesLabelPosition,
    },
    style::{Color as PlotColor, IntoFont, RGBColor, ShapeStyle, TextStyle, BLACK, WHITE},
};
use poolshark::local::LPooled;
use tokio::try_join;

// ── Data point types ────────────────────────────────────────────────

/// XY data points (shared by Line, Scatter, Bar, Area, DashedLine).
pub(crate) struct XYPoints(pub LPooled<Vec<(f64, f64)>>);

impl FromValue for XYPoints {
    fn from_value(v: Value) -> Result<Self> {
        match v {
            Value::Array(a) => Ok(Self(
                a.iter()
                    .map(|v| v.clone().cast_to::<(f64, f64)>())
                    .collect::<Result<_>>()?,
            )),
            _ => bail!("chart dataset data: expected array"),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub(crate) struct OHLCPoint {
    pub x: f64,
    pub open: f64,
    pub high: f64,
    pub low: f64,
    pub close: f64,
}

impl FromValue for OHLCPoint {
    fn from_value(v: Value) -> Result<Self> {
        let [(_, close), (_, high), (_, low), (_, open), (_, x)] =
            v.cast_to::<[(ArcStr, f64); 5]>()?;
        Ok(Self { x, open, high, low, close })
    }
}

pub(crate) struct OHLCPoints(pub LPooled<Vec<OHLCPoint>>);

impl FromValue for OHLCPoints {
    fn from_value(v: Value) -> Result<Self> {
        match v {
            Value::Array(a) => Ok(Self(
                a.iter()
                    .map(|v| OHLCPoint::from_value(v.clone()))
                    .collect::<Result<_>>()?,
            )),
            _ => bail!("chart ohlc data: expected array"),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub(crate) struct EBPoint {
    pub x: f64,
    pub min: f64,
    pub avg: f64,
    pub max: f64,
}

impl FromValue for EBPoint {
    fn from_value(v: Value) -> Result<Self> {
        let [(_, avg), (_, max), (_, min), (_, x)] = v.cast_to::<[(ArcStr, f64); 4]>()?;
        Ok(Self { x, min, avg, max })
    }
}

pub(crate) struct EBPoints(pub LPooled<Vec<EBPoint>>);

impl FromValue for EBPoints {
    fn from_value(v: Value) -> Result<Self> {
        match v {
            Value::Array(a) => Ok(Self(
                a.iter()
                    .map(|v| EBPoint::from_value(v.clone()))
                    .collect::<Result<_>>()?,
            )),
            _ => bail!("chart error bar data: expected array"),
        }
    }
}

// ── Style types ─────────────────────────────────────────────────────

struct SeriesStyleV {
    color: Option<iced_core::Color>,
    label: Option<String>,
    stroke_width: Option<f64>,
    point_size: Option<f64>,
}

impl FromValue for SeriesStyleV {
    fn from_value(v: Value) -> Result<Self> {
        let [(_, color), (_, label), (_, point_size), (_, stroke_width)] =
            v.cast_to::<[(ArcStr, Value); 4]>()?;
        Ok(Self {
            color: if color == Value::Null {
                None
            } else {
                Some(ColorV::from_value(color)?.0)
            },
            label: if label == Value::Null {
                None
            } else {
                Some(label.cast_to::<String>()?)
            },
            stroke_width: if stroke_width == Value::Null {
                None
            } else {
                Some(stroke_width.cast_to::<f64>()?)
            },
            point_size: if point_size == Value::Null {
                None
            } else {
                Some(point_size.cast_to::<f64>()?)
            },
        })
    }
}

struct CandlestickStyleV {
    gain_color: Option<iced_core::Color>,
    loss_color: Option<iced_core::Color>,
    bar_width: Option<f64>,
    label: Option<String>,
}

impl FromValue for CandlestickStyleV {
    fn from_value(v: Value) -> Result<Self> {
        let [(_, bar_width), (_, gain_color), (_, label), (_, loss_color)] =
            v.cast_to::<[(ArcStr, Value); 4]>()?;
        Ok(Self {
            gain_color: if gain_color == Value::Null {
                None
            } else {
                Some(ColorV::from_value(gain_color)?.0)
            },
            loss_color: if loss_color == Value::Null {
                None
            } else {
                Some(ColorV::from_value(loss_color)?.0)
            },
            bar_width: if bar_width == Value::Null {
                None
            } else {
                Some(bar_width.cast_to::<f64>()?)
            },
            label: if label == Value::Null {
                None
            } else {
                Some(label.cast_to::<String>()?)
            },
        })
    }
}

// ── Mesh style ──────────────────────────────────────────────────────

struct MeshStyleV {
    show_x_grid: Option<bool>,
    show_y_grid: Option<bool>,
    grid_color: Option<iced_core::Color>,
    axis_color: Option<iced_core::Color>,
    label_color: Option<iced_core::Color>,
    label_size: Option<f64>,
    x_label_area_size: Option<f64>,
    x_labels: Option<i64>,
    y_label_area_size: Option<f64>,
    y_labels: Option<i64>,
}

impl FromValue for MeshStyleV {
    fn from_value(v: Value) -> Result<Self> {
        let [(_, axis_color), (_, grid_color), (_, label_color), (_, label_size), (_, show_x_grid), (_, show_y_grid), (_, x_label_area_size), (_, x_labels), (_, y_label_area_size), (_, y_labels)] =
            v.cast_to::<[(ArcStr, Value); 10]>()?;
        Ok(Self {
            show_x_grid: if show_x_grid == Value::Null {
                None
            } else {
                Some(show_x_grid.cast_to::<bool>()?)
            },
            show_y_grid: if show_y_grid == Value::Null {
                None
            } else {
                Some(show_y_grid.cast_to::<bool>()?)
            },
            grid_color: if grid_color == Value::Null {
                None
            } else {
                Some(ColorV::from_value(grid_color)?.0)
            },
            axis_color: if axis_color == Value::Null {
                None
            } else {
                Some(ColorV::from_value(axis_color)?.0)
            },
            label_color: if label_color == Value::Null {
                None
            } else {
                Some(ColorV::from_value(label_color)?.0)
            },
            label_size: if label_size == Value::Null {
                None
            } else {
                Some(label_size.cast_to::<f64>()?)
            },
            x_label_area_size: if x_label_area_size == Value::Null {
                None
            } else {
                Some(x_label_area_size.cast_to::<f64>()?)
            },
            x_labels: if x_labels == Value::Null {
                None
            } else {
                Some(x_labels.cast_to::<i64>()?)
            },
            y_label_area_size: if y_label_area_size == Value::Null {
                None
            } else {
                Some(y_label_area_size.cast_to::<f64>()?)
            },
            y_labels: if y_labels == Value::Null {
                None
            } else {
                Some(y_labels.cast_to::<i64>()?)
            },
        })
    }
}

/// Newtype for Option<MeshStyleV> to satisfy orphan rules.
struct OptMeshStyle(Option<MeshStyleV>);

impl FromValue for OptMeshStyle {
    fn from_value(v: Value) -> Result<Self> {
        if v == Value::Null {
            Ok(Self(None))
        } else {
            Ok(Self(Some(MeshStyleV::from_value(v)?)))
        }
    }
}

// ── Legend style ────────────────────────────────────────────────────

struct LegendStyleV {
    background: Option<iced_core::Color>,
    border: Option<iced_core::Color>,
    label_color: Option<iced_core::Color>,
    label_size: Option<f64>,
}

impl FromValue for LegendStyleV {
    fn from_value(v: Value) -> Result<Self> {
        let [(_, background), (_, border), (_, label_color), (_, label_size)] =
            v.cast_to::<[(ArcStr, Value); 4]>()?;
        Ok(Self {
            background: if background == Value::Null {
                None
            } else {
                Some(ColorV::from_value(background)?.0)
            },
            border: if border == Value::Null {
                None
            } else {
                Some(ColorV::from_value(border)?.0)
            },
            label_color: if label_color == Value::Null {
                None
            } else {
                Some(ColorV::from_value(label_color)?.0)
            },
            label_size: if label_size == Value::Null {
                None
            } else {
                Some(label_size.cast_to::<f64>()?)
            },
        })
    }
}

/// Newtype for Option<LegendStyleV> to satisfy orphan rules.
struct OptLegendStyle(Option<LegendStyleV>);

impl FromValue for OptLegendStyle {
    fn from_value(v: Value) -> Result<Self> {
        if v == Value::Null {
            Ok(Self(None))
        } else {
            Ok(Self(Some(LegendStyleV::from_value(v)?)))
        }
    }
}

// ── Legend position ─────────────────────────────────────────────────

#[derive(Clone)]
struct LegendPositionV(SeriesLabelPosition);

impl FromValue for LegendPositionV {
    fn from_value(v: Value) -> Result<Self> {
        match &*v.cast_to::<ArcStr>()? {
            "UpperLeft" => Ok(Self(SeriesLabelPosition::UpperLeft)),
            "UpperRight" => Ok(Self(SeriesLabelPosition::UpperRight)),
            "LowerLeft" => Ok(Self(SeriesLabelPosition::LowerLeft)),
            "LowerRight" => Ok(Self(SeriesLabelPosition::LowerRight)),
            "MiddleLeft" => Ok(Self(SeriesLabelPosition::MiddleLeft)),
            "MiddleRight" => Ok(Self(SeriesLabelPosition::MiddleRight)),
            "UpperMiddle" => Ok(Self(SeriesLabelPosition::UpperMiddle)),
            "LowerMiddle" => Ok(Self(SeriesLabelPosition::LowerMiddle)),
            s => bail!("invalid legend position: {s}"),
        }
    }
}

/// Newtype for Option<LegendPositionV> to satisfy orphan rules.
struct OptLegendPosition(Option<LegendPositionV>);

impl FromValue for OptLegendPosition {
    fn from_value(v: Value) -> Result<Self> {
        if v == Value::Null {
            Ok(Self(None))
        } else {
            Ok(Self(Some(LegendPositionV::from_value(v)?)))
        }
    }
}

// ── Optional f64 newtype ────────────────────────────────────────────

struct OptF64(Option<f64>);

impl FromValue for OptF64 {
    fn from_value(v: Value) -> Result<Self> {
        if v == Value::Null {
            Ok(Self(None))
        } else {
            Ok(Self(Some(v.cast_to::<f64>()?)))
        }
    }
}

// ── Optional Color newtype ──────────────────────────────────────────

struct OptColor(Option<iced_core::Color>);

impl FromValue for OptColor {
    fn from_value(v: Value) -> Result<Self> {
        if v == Value::Null {
            Ok(Self(None))
        } else {
            Ok(Self(Some(ColorV::from_value(v)?.0)))
        }
    }
}

// ── Axis range ──────────────────────────────────────────────────────

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

// ── Dataset types ───────────────────────────────────────────────────

#[derive(Clone, Copy)]
enum XYKind {
    Line,
    Scatter,
    Bar,
    Area,
}

/// A compiled dataset with live reactive data refs.
enum DatasetEntry<X: GXExt> {
    XY { kind: XYKind, data: TRef<X, XYPoints>, style: SeriesStyleV },
    DashedLine { data: TRef<X, XYPoints>, dash: f64, gap: f64, style: SeriesStyleV },
    Candlestick { data: TRef<X, OHLCPoints>, style: CandlestickStyleV },
    ErrorBar { data: TRef<X, EBPoints>, style: SeriesStyleV },
}

impl<X: GXExt> DatasetEntry<X> {
    fn label(&self) -> Option<&str> {
        match self {
            Self::XY { style, .. }
            | Self::DashedLine { style, .. }
            | Self::ErrorBar { style, .. } => style.label.as_deref(),
            Self::Candlestick { style, .. } => style.label.as_deref(),
        }
    }
}

/// Dataset metadata parsed from the datasets array value before ref compilation.
enum DatasetMeta {
    XY { kind: XYKind, data_id: u64, style: SeriesStyleV },
    DashedLine { data_id: u64, dash: f64, gap: f64, style: SeriesStyleV },
    Candlestick { data_id: u64, style: CandlestickStyleV },
    ErrorBar { data_id: u64, style: SeriesStyleV },
}

impl FromValue for DatasetMeta {
    fn from_value(v: Value) -> Result<Self> {
        let (tag, inner) = v.cast_to::<(ArcStr, Value)>()?;
        match &*tag {
            "Line" => {
                let [(_, data), (_, style)] = inner.cast_to::<[(ArcStr, Value); 2]>()?;
                let data_id = data.cast_to::<u64>()?;
                let style = SeriesStyleV::from_value(style)?;
                Ok(Self::XY { kind: XYKind::Line, data_id, style })
            }
            "Scatter" => {
                let [(_, data), (_, style)] = inner.cast_to::<[(ArcStr, Value); 2]>()?;
                let data_id = data.cast_to::<u64>()?;
                let style = SeriesStyleV::from_value(style)?;
                Ok(Self::XY { kind: XYKind::Scatter, data_id, style })
            }
            "Bar" => {
                let [(_, data), (_, style)] = inner.cast_to::<[(ArcStr, Value); 2]>()?;
                let data_id = data.cast_to::<u64>()?;
                let style = SeriesStyleV::from_value(style)?;
                Ok(Self::XY { kind: XYKind::Bar, data_id, style })
            }
            "Area" => {
                let [(_, data), (_, style)] = inner.cast_to::<[(ArcStr, Value); 2]>()?;
                let data_id = data.cast_to::<u64>()?;
                let style = SeriesStyleV::from_value(style)?;
                Ok(Self::XY { kind: XYKind::Area, data_id, style })
            }
            "DashedLine" => {
                let [(_, dash), (_, data), (_, gap), (_, style)] =
                    inner.cast_to::<[(ArcStr, Value); 4]>()?;
                let data_id = data.cast_to::<u64>()?;
                let dash = dash.cast_to::<f64>()?;
                let gap = gap.cast_to::<f64>()?;
                let style = SeriesStyleV::from_value(style)?;
                Ok(Self::DashedLine { data_id, dash, gap, style })
            }
            "Candlestick" => {
                let [(_, data), (_, style)] = inner.cast_to::<[(ArcStr, Value); 2]>()?;
                let data_id = data.cast_to::<u64>()?;
                let style = CandlestickStyleV::from_value(style)?;
                Ok(Self::Candlestick { data_id, style })
            }
            "ErrorBar" => {
                let [(_, data), (_, style)] = inner.cast_to::<[(ArcStr, Value); 2]>()?;
                let data_id = data.cast_to::<u64>()?;
                let style = SeriesStyleV::from_value(style)?;
                Ok(Self::ErrorBar { data_id, style })
            }
            s => bail!("invalid dataset variant: {s}"),
        }
    }
}

/// Compile dataset metadata into live entries with data refs.
async fn compile_datasets<X: GXExt>(
    gx: &GXHandle<X>,
    v: Value,
) -> Result<Vec<DatasetEntry<X>>> {
    let metas: Vec<DatasetMeta> = v
        .cast_to::<Vec<Value>>()?
        .into_iter()
        .map(DatasetMeta::from_value)
        .collect::<Result<_>>()?;
    let mut entries = Vec::with_capacity(metas.len());
    for meta in metas {
        match meta {
            DatasetMeta::XY { kind, data_id, style } => {
                let data_ref = gx.compile_ref(data_id).await?;
                let data = TRef::new(data_ref).context("chart xy data")?;
                entries.push(DatasetEntry::XY { kind, data, style });
            }
            DatasetMeta::DashedLine { data_id, dash, gap, style } => {
                let data_ref = gx.compile_ref(data_id).await?;
                let data = TRef::new(data_ref).context("chart dashed data")?;
                entries.push(DatasetEntry::DashedLine { data, dash, gap, style });
            }
            DatasetMeta::Candlestick { data_id, style } => {
                let data_ref = gx.compile_ref(data_id).await?;
                let data = TRef::new(data_ref).context("chart ohlc data")?;
                entries.push(DatasetEntry::Candlestick { data, style });
            }
            DatasetMeta::ErrorBar { data_id, style } => {
                let data_ref = gx.compile_ref(data_id).await?;
                let data = TRef::new(data_ref).context("chart errorbar data")?;
                entries.push(DatasetEntry::ErrorBar { data, style });
            }
        }
    }
    Ok(entries)
}

// ── ChartW ──────────────────────────────────────────────────────────

pub(crate) struct ChartW<X: GXExt> {
    gx: GXHandle<X>,
    datasets_ref: Ref<X>,
    datasets: Vec<DatasetEntry<X>>,
    title: TRef<X, Option<String>>,
    x_label: TRef<X, Option<String>>,
    y_label: TRef<X, Option<String>>,
    x_range: TRef<X, OptAxisRange>,
    y_range: TRef<X, OptAxisRange>,
    width: TRef<X, LengthV>,
    height: TRef<X, LengthV>,
    background: TRef<X, OptColor>,
    margin: TRef<X, OptF64>,
    title_color: TRef<X, OptColor>,
    title_size: TRef<X, OptF64>,
    legend_position: TRef<X, OptLegendPosition>,
    legend_style: TRef<X, OptLegendStyle>,
    mesh: TRef<X, OptMeshStyle>,
    cache: iced_canvas::Cache<Renderer>,
}

impl<X: GXExt> ChartW<X> {
    pub(crate) async fn compile(gx: GXHandle<X>, source: Value) -> Result<GuiW<X>> {
        let [(_, background), (_, datasets), (_, height), (_, legend_position), (_, legend_style), (_, margin), (_, mesh), (_, title), (_, title_color), (_, title_size), (_, width), (_, x_label), (_, x_range), (_, y_label), (_, y_range)] =
            source.cast_to::<[(ArcStr, u64); 15]>().context("chart flds")?;
        let (
            background_ref,
            datasets_ref,
            height_ref,
            legend_position_ref,
            legend_style_ref,
            margin_ref,
            mesh_ref,
            title_ref,
            title_color_ref,
            title_size_ref,
            width_ref,
            x_label_ref,
            x_range_ref,
            y_label_ref,
            y_range_ref,
        ) = try_join! {
            gx.compile_ref(background),
            gx.compile_ref(datasets),
            gx.compile_ref(height),
            gx.compile_ref(legend_position),
            gx.compile_ref(legend_style),
            gx.compile_ref(margin),
            gx.compile_ref(mesh),
            gx.compile_ref(title),
            gx.compile_ref(title_color),
            gx.compile_ref(title_size),
            gx.compile_ref(width),
            gx.compile_ref(x_label),
            gx.compile_ref(x_range),
            gx.compile_ref(y_label),
            gx.compile_ref(y_range),
        }?;
        let entries = match datasets_ref.last.as_ref() {
            Some(v) => compile_datasets(&gx, v.clone()).await?,
            None => vec![],
        };
        Ok(Box::new(Self {
            gx: gx.clone(),
            datasets_ref,
            datasets: entries,
            title: TRef::new(title_ref).context("chart tref title")?,
            x_label: TRef::new(x_label_ref).context("chart tref x_label")?,
            y_label: TRef::new(y_label_ref).context("chart tref y_label")?,
            x_range: TRef::new(x_range_ref).context("chart tref x_range")?,
            y_range: TRef::new(y_range_ref).context("chart tref y_range")?,
            width: TRef::new(width_ref).context("chart tref width")?,
            height: TRef::new(height_ref).context("chart tref height")?,
            background: TRef::new(background_ref).context("chart tref background")?,
            margin: TRef::new(margin_ref).context("chart tref margin")?,
            title_color: TRef::new(title_color_ref).context("chart tref title_color")?,
            title_size: TRef::new(title_size_ref).context("chart tref title_size")?,
            legend_position: TRef::new(legend_position_ref)
                .context("chart tref legend_position")?,
            legend_style: TRef::new(legend_style_ref)
                .context("chart tref legend_style")?,
            mesh: TRef::new(mesh_ref).context("chart tref mesh")?,
            cache: iced_canvas::Cache::new(),
        }))
    }
}

impl<X: GXExt> GuiWidget<X> for ChartW<X> {
    fn handle_update(
        &mut self,
        rt: &tokio::runtime::Handle,
        id: ExprId,
        v: &Value,
    ) -> Result<bool> {
        let mut changed = false;
        if id == self.datasets_ref.id {
            self.datasets_ref.last = Some(v.clone());
            self.datasets = rt
                .block_on(compile_datasets(&self.gx, v.clone()))
                .context("chart datasets recompile")?;
            self.cache.clear();
            changed = true;
        }
        for ds in &mut self.datasets {
            let updated = match ds {
                DatasetEntry::XY { data, .. } | DatasetEntry::DashedLine { data, .. } => {
                    data.update(id, v).context("chart update xy data")?.is_some()
                }
                DatasetEntry::Candlestick { data, .. } => {
                    data.update(id, v).context("chart update ohlc data")?.is_some()
                }
                DatasetEntry::ErrorBar { data, .. } => {
                    data.update(id, v).context("chart update errorbar data")?.is_some()
                }
            };
            if updated {
                self.cache.clear();
                changed = true;
            }
        }
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
        up!(title);
        up!(x_label);
        up!(y_label);
        up!(x_range);
        up!(y_range);
        up!(background);
        up!(margin);
        up!(title_color);
        up!(title_size);
        up!(legend_position);
        up!(legend_style);
        up!(mesh);
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

// ── Color palette & helpers ─────────────────────────────────────────

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

const DEFAULT_GAIN: RGBColor = RGBColor(44, 160, 44);
const DEFAULT_LOSS: RGBColor = RGBColor(214, 39, 40);

fn iced_to_plotters(c: iced_core::Color) -> RGBColor {
    let [r, g, b, _] = c.into_rgba8();
    RGBColor(r, g, b)
}

// ── Range computation ───────────────────────────────────────────────

/// Compute axis ranges across all dataset entries.
fn compute_ranges<X: GXExt>(datasets: &[DatasetEntry<X>]) -> ((f64, f64), (f64, f64)) {
    let mut x_min = f64::INFINITY;
    let mut x_max = f64::NEG_INFINITY;
    let mut y_min = f64::INFINITY;
    let mut y_max = f64::NEG_INFINITY;

    macro_rules! extend {
        ($x:expr, $ylo:expr, $yhi:expr) => {
            if $x < x_min {
                x_min = $x;
            }
            if $x > x_max {
                x_max = $x;
            }
            if $ylo < y_min {
                y_min = $ylo;
            }
            if $yhi > y_max {
                y_max = $yhi;
            }
        };
    }

    for ds in datasets {
        match ds {
            DatasetEntry::XY { data, .. } | DatasetEntry::DashedLine { data, .. } => {
                if let Some(pts) = data.t.as_ref() {
                    for &(x, y) in pts.0.iter() {
                        extend!(x, y, y);
                    }
                }
            }
            DatasetEntry::Candlestick { data, .. } => {
                if let Some(pts) = data.t.as_ref() {
                    for pt in pts.0.iter() {
                        extend!(pt.x, pt.low, pt.high);
                    }
                }
            }
            DatasetEntry::ErrorBar { data, .. } => {
                if let Some(pts) = data.t.as_ref() {
                    for pt in pts.0.iter() {
                        extend!(pt.x, pt.min, pt.max);
                    }
                }
            }
        }
    }

    (pad_range(x_min, x_max), pad_range(y_min, y_max))
}

pub(crate) fn pad_range(min: f64, max: f64) -> (f64, f64) {
    if min > max {
        return (-1.0, 1.0);
    }
    let (min, max) = if min == max { (min - 1.0, max + 1.0) } else { (min, max) };
    let pad = (max - min) * 0.05;
    (min - pad, max + pad)
}

// ── Drawing ─────────────────────────────────────────────────────────

impl<X: GXExt> iced_canvas::Program<super::Message, crate::theme::GraphixTheme>
    for ChartW<X>
{
    type State = ();

    fn draw(
        &self,
        _state: &Self::State,
        renderer: &Renderer,
        _theme: &crate::theme::GraphixTheme,
        bounds: iced_core::Rectangle,
        _cursor: mouse::Cursor,
    ) -> Vec<iced_canvas::Geometry<Renderer>> {
        let geom = self.cache.draw(renderer, bounds.size(), |frame| {
            let w = frame.width() as u32;
            let h = frame.height() as u32;
            if w == 0 || h == 0 {
                return;
            }

            let has_data = self.datasets.iter().any(|ds| match ds {
                DatasetEntry::XY { data, .. } | DatasetEntry::DashedLine { data, .. } => {
                    data.t.as_ref().is_some_and(|d| !d.0.is_empty())
                }
                DatasetEntry::Candlestick { data, .. } => {
                    data.t.as_ref().is_some_and(|d| !d.0.is_empty())
                }
                DatasetEntry::ErrorBar { data, .. } => {
                    data.t.as_ref().is_some_and(|d| !d.0.is_empty())
                }
            });
            if self.datasets.is_empty() || !has_data {
                return;
            }

            let (auto_x, auto_y) = compute_ranges(&self.datasets);

            let (x_min, x_max) = match self.x_range.t.as_ref().and_then(|r| r.0.as_ref())
            {
                Some(r) => (r.min, r.max),
                None => auto_x,
            };
            let (y_min, y_max) = match self.y_range.t.as_ref().and_then(|r| r.0.as_ref())
            {
                Some(r) => (r.min, r.max),
                None => auto_y,
            };

            let backend = IcedBackend::new(frame, w, h);
            let root = backend.into_drawing_area();

            // Background color
            let bg = self
                .background
                .t
                .as_ref()
                .and_then(|o| o.0)
                .map(iced_to_plotters)
                .unwrap_or(WHITE);
            if let Err(e) = root.fill(&bg) {
                error!("chart fill: {e:?}");
                return;
            }

            let title = self.title.t.as_ref().and_then(|o| o.as_deref());
            let x_label = self.x_label.t.as_ref().and_then(|o| o.as_deref());
            let y_label = self.y_label.t.as_ref().and_then(|o| o.as_deref());
            let margin = self.margin.t.as_ref().and_then(|o| o.0).unwrap_or(10.0);
            let title_size = self.title_size.t.as_ref().and_then(|o| o.0).unwrap_or(16.0);
            let mesh_style = self.mesh.t.as_ref().and_then(|m| m.0.as_ref());
            let label_sz = mesh_style.and_then(|ms| ms.label_size).unwrap_or(12.0);

            let mut builder = ChartBuilder::on(&root);
            builder.margin(margin as u32);
            if let Some(t) = title {
                let font = ("sans-serif", title_size).into_font();
                if let Some(tc) = self.title_color.t.as_ref().and_then(|o| o.0) {
                    let mut style = TextStyle::from(font);
                    style.color = iced_to_plotters(tc).to_backend_color();
                    builder.caption(t, style);
                } else {
                    builder.caption(t, font);
                }
            }
            // Compute label areas from actual tick content.
            let (_, tick_h) = estimate_text("0", label_sz as f64);
            let y_min_s = format!("{y_min}");
            let y_max_s = format!("{y_max}");
            let widest =
                if y_min_s.len() > y_max_s.len() { &y_min_s } else { &y_max_s };
            let (tick_w, _) = estimate_text(widest, label_sz as f64);
            let auto_y_area = if y_label.is_some() {
                tick_w + tick_h + 15
            } else {
                tick_w + 8
            };
            let auto_x_area = if x_label.is_some() {
                tick_h * 2 + 15
            } else {
                tick_h + 8
            };
            let x_area = mesh_style
                .and_then(|ms| ms.x_label_area_size)
                .map(|s| s as u32)
                .unwrap_or(auto_x_area);
            let y_area = mesh_style
                .and_then(|ms| ms.y_label_area_size)
                .map(|s| s as u32)
                .unwrap_or(auto_y_area);
            builder.x_label_area_size(x_area);
            builder.y_label_area_size(y_area);

            let mut chart = match builder.build_cartesian_2d(x_min..x_max, y_min..y_max) {
                Ok(c) => c,
                Err(_) => return,
            };

            // Mesh configuration
            let mut mesh_cfg = chart.configure_mesh();
            if let Some(xl) = x_label {
                mesh_cfg.x_desc(xl);
            }
            if let Some(yl) = y_label {
                mesh_cfg.y_desc(yl);
            }
            if let Some(ms) = mesh_style {
                if ms.show_x_grid == Some(false) {
                    mesh_cfg.disable_x_mesh();
                }
                if ms.show_y_grid == Some(false) {
                    mesh_cfg.disable_y_mesh();
                }
                if let Some(c) = ms.grid_color {
                    let pc = iced_to_plotters(c);
                    mesh_cfg.light_line_style(pc);
                }
                if let Some(c) = ms.axis_color {
                    let pc = iced_to_plotters(c);
                    mesh_cfg.axis_style(pc);
                }
                if ms.label_size.is_some() || ms.label_color.is_some() {
                    let s = ms.label_size.unwrap_or(12.0);
                    if let Some(lc) = ms.label_color {
                        let mut style =
                            TextStyle::from(("sans-serif", s).into_font());
                        style.color = iced_to_plotters(lc).to_backend_color();
                        mesh_cfg.label_style(style.clone());
                        mesh_cfg.axis_desc_style(style);
                    } else {
                        mesh_cfg.label_style(("sans-serif", s).into_font());
                    }
                }
                if let Some(n) = ms.x_labels {
                    mesh_cfg.x_labels(n as usize);
                }
                if let Some(n) = ms.y_labels {
                    mesh_cfg.y_labels(n as usize);
                }
            }
            if let Err(e) = mesh_cfg.draw() {
                error!("chart mesh draw: {e:?}");
                return;
            }

            // Draw each dataset
            for (i, ds) in self.datasets.iter().enumerate() {
                match ds {
                    DatasetEntry::XY { kind, data, style } => {
                        let pts = match data.t.as_ref() {
                            Some(d) => &d.0,
                            None => continue,
                        };
                        let color = style
                            .color
                            .map(iced_to_plotters)
                            .unwrap_or(PALETTE[i % PALETTE.len()]);
                        let sw = style.stroke_width.unwrap_or(2.0) as u32;
                        let ps = style.point_size.unwrap_or(3.0) as u32;
                        let line_style = ShapeStyle::from(color).stroke_width(sw);
                        let fill_style = ShapeStyle::from(color).filled();
                        let label = style.label.as_deref();

                        match kind {
                            XYKind::Line => {
                                let series =
                                    LineSeries::new(pts.iter().copied(), line_style);
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
                                    Err(e) => error!("chart draw line: {e:?}"),
                                }
                            }
                            XYKind::Scatter => {
                                let series = pts
                                    .iter()
                                    .map(|&(x, y)| Circle::new((x, y), ps, fill_style));
                                match chart.draw_series(series) {
                                    Ok(ann) => {
                                        if let Some(l) = label {
                                            ann.label(l).legend(move |(x, y)| {
                                                Circle::new((x, y), ps, fill_style)
                                            });
                                        }
                                    }
                                    Err(e) => error!("chart draw scatter: {e:?}"),
                                }
                            }
                            XYKind::Bar => {
                                let bar_width = if pts.len() > 1 {
                                    let dx = (x_max - x_min) / pts.len() as f64;
                                    dx * 0.8
                                } else {
                                    1.0
                                };
                                let series = pts.iter().map(|&(x, y)| {
                                    plotters::element::Rectangle::new(
                                        [
                                            (x - bar_width / 2.0, 0.0),
                                            (x + bar_width / 2.0, y),
                                        ],
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
                                    Err(e) => error!("chart draw bar: {e:?}"),
                                }
                            }
                            XYKind::Area => {
                                let area_fill = color.mix(0.3);
                                let series = AreaSeries::new(
                                    pts.iter().copied(),
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
                                    Err(e) => error!("chart draw area: {e:?}"),
                                }
                            }
                        }
                    }

                    DatasetEntry::DashedLine { data, dash, gap, style } => {
                        let pts = match data.t.as_ref() {
                            Some(d) => &d.0,
                            None => continue,
                        };
                        let color = style
                            .color
                            .map(iced_to_plotters)
                            .unwrap_or(PALETTE[i % PALETTE.len()]);
                        let sw = style.stroke_width.unwrap_or(2.0) as u32;
                        let line_style = ShapeStyle::from(color).stroke_width(sw);
                        let label = style.label.as_deref();

                        let series = DashedLineSeries::new(
                            pts.iter().copied(),
                            *dash as u32,
                            *gap as u32,
                            line_style,
                        );
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
                            Err(e) => error!("chart draw dashed: {e:?}"),
                        }
                    }

                    DatasetEntry::Candlestick { data, style } => {
                        let pts = match data.t.as_ref() {
                            Some(d) => &d.0,
                            None => continue,
                        };
                        let gain = style
                            .gain_color
                            .map(iced_to_plotters)
                            .unwrap_or(DEFAULT_GAIN);
                        let loss = style
                            .loss_color
                            .map(iced_to_plotters)
                            .unwrap_or(DEFAULT_LOSS);
                        let bw = style.bar_width.unwrap_or(5.0) as u32;
                        let label = style.label.as_deref();

                        let series = pts.iter().map(|pt| {
                            CandleStick::new(
                                pt.x,
                                pt.open,
                                pt.high,
                                pt.low,
                                pt.close,
                                ShapeStyle::from(gain).filled(),
                                ShapeStyle::from(loss).filled(),
                                bw,
                            )
                        });
                        match chart.draw_series(series) {
                            Ok(ann) => {
                                if let Some(l) = label {
                                    let gain_style = ShapeStyle::from(gain).filled();
                                    ann.label(l).legend(move |(x, y)| {
                                        plotters::element::Rectangle::new(
                                            [(x, y - 5), (x + 20, y + 5)],
                                            gain_style,
                                        )
                                    });
                                }
                            }
                            Err(e) => error!("chart draw candlestick: {e:?}"),
                        }
                    }

                    DatasetEntry::ErrorBar { data, style } => {
                        let pts = match data.t.as_ref() {
                            Some(d) => &d.0,
                            None => continue,
                        };
                        let color = style
                            .color
                            .map(iced_to_plotters)
                            .unwrap_or(PALETTE[i % PALETTE.len()]);
                        let sw = style.stroke_width.unwrap_or(2.0) as u32;
                        let line_style = ShapeStyle::from(color).stroke_width(sw);
                        let label = style.label.as_deref();

                        let series = pts.iter().map(|pt| {
                            ErrorBar::new_vertical(
                                pt.x, pt.min, pt.avg, pt.max, line_style, sw,
                            )
                        });
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
                            Err(e) => error!("chart draw errorbar: {e:?}"),
                        }
                    }
                }
            }

            // Legend
            let has_labels = self.datasets.iter().any(|ds| ds.label().is_some());
            if has_labels {
                let legend_pos = self
                    .legend_position
                    .t
                    .as_ref()
                    .and_then(|o| o.0.as_ref())
                    .map(|p| p.0.clone())
                    .unwrap_or(SeriesLabelPosition::UpperLeft);
                let ls = self.legend_style.t.as_ref().and_then(|o| o.0.as_ref());
                let legend_bg =
                    ls.and_then(|s| s.background).map(iced_to_plotters).unwrap_or(WHITE);
                let legend_border =
                    ls.and_then(|s| s.border).map(iced_to_plotters).unwrap_or(BLACK);
                let legend_font_sz =
                    ls.and_then(|s| s.label_size).unwrap_or(label_sz);
                let mut labels = chart.configure_series_labels();
                labels.position(legend_pos);
                labels.margin(15);
                labels.background_style(legend_bg.mix(0.8));
                labels.border_style(legend_border);
                let mut style =
                    TextStyle::from(("sans-serif", legend_font_sz).into_font());
                if let Some(lc) = ls.and_then(|s| s.label_color) {
                    style.color = iced_to_plotters(lc).to_backend_color();
                }
                labels.label_font(style);
                if let Err(e) = labels.draw() {
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
