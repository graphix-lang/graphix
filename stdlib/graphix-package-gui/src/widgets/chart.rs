use super::{
    plotters_backend::{estimate_text, IcedBackend},
    GuiW, GuiWidget, IcedElement, Renderer,
};
use crate::types::{ColorV, LengthV};
use anyhow::{bail, Context, Result};
use arcstr::ArcStr;
use chrono::{DateTime, TimeDelta, Utc};
use graphix_compiler::expr::ExprId;
use graphix_rt::{GXExt, GXHandle, Ref, TRef};
use iced_core::mouse;
use iced_widget::canvas as iced_canvas;
use log::error;
use netidx::publisher::{FromValue, Value};
use plotters::{
    chart::ChartBuilder,
    element::{CandleStick, ErrorBar, PathElement, Pie},
    prelude::{
        AreaSeries, Circle, DashedLineSeries, Histogram, IntoDrawingArea,
        IntoSegmentedCoord, LineSeries, SeriesLabelPosition, SurfaceSeries,
    },
    style::{
        Color as PlotColor, IntoFont, RGBColor, ShapeStyle, TextStyle, BLACK, WHITE,
    },
};
use poolshark::local::LPooled;
use tokio::try_join;

// ── Data point types ────────────────────────────────────────────────

/// XY data: either numeric (f64, f64) or time-series (DateTime<Utc>, f64).
pub(crate) enum XYData {
    Numeric(LPooled<Vec<(f64, f64)>>),
    DateTime(LPooled<Vec<(DateTime<Utc>, f64)>>),
}

impl FromValue for XYData {
    fn from_value(v: Value) -> Result<Self> {
        let a = match v {
            Value::Array(a) => a,
            _ => bail!("chart dataset data: expected array"),
        };
        if a.is_empty() {
            return Ok(Self::Numeric(LPooled::take()));
        }
        // Check first element's x value variant directly.
        // Don't use cast_to: netidx casts any number to DateTime (as Unix timestamp).
        let is_datetime = matches!(&a[0], Value::Array(tup) if !tup.is_empty() && matches!(&tup[0], Value::DateTime(_)));
        if is_datetime {
            Ok(Self::DateTime(
                a.iter()
                    .map(|v| v.clone().cast_to::<(DateTime<Utc>, f64)>())
                    .collect::<Result<_>>()?,
            ))
        } else {
            Ok(Self::Numeric(
                a.iter()
                    .map(|v| v.clone().cast_to::<(f64, f64)>())
                    .collect::<Result<_>>()?,
            ))
        }
    }
}

/// Bar chart data: categorical (String) x-axis, numeric y-axis.
pub(crate) struct BarData(LPooled<Vec<(String, f64)>>);

impl FromValue for BarData {
    fn from_value(v: Value) -> Result<Self> {
        let a = match v {
            Value::Array(a) => a,
            _ => bail!("chart bar data: expected array"),
        };
        Ok(Self(
            a.iter()
                .map(|v| v.clone().cast_to::<(String, f64)>())
                .collect::<Result<_>>()?,
        ))
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

#[derive(Clone, Copy, Debug)]
pub(crate) struct TimeOHLCPoint {
    pub x: DateTime<Utc>,
    pub open: f64,
    pub high: f64,
    pub low: f64,
    pub close: f64,
}

impl FromValue for TimeOHLCPoint {
    fn from_value(v: Value) -> Result<Self> {
        let [(_, close), (_, high), (_, low), (_, open), (_, x)] =
            v.cast_to::<[(ArcStr, Value); 5]>()?;
        Ok(Self {
            x: x.cast_to::<DateTime<Utc>>()?,
            open: open.cast_to::<f64>()?,
            high: high.cast_to::<f64>()?,
            low: low.cast_to::<f64>()?,
            close: close.cast_to::<f64>()?,
        })
    }
}

/// OHLC data: either numeric or time-series x-axis.
pub(crate) enum OHLCData {
    Numeric(LPooled<Vec<OHLCPoint>>),
    DateTime(LPooled<Vec<TimeOHLCPoint>>),
}

impl FromValue for OHLCData {
    fn from_value(v: Value) -> Result<Self> {
        let a = match v {
            Value::Array(a) => a,
            _ => bail!("chart ohlc data: expected array"),
        };
        if a.is_empty() {
            return Ok(Self::Numeric(LPooled::take()));
        }
        // Check first element's x field type
        let first_fields = a[0].clone().cast_to::<[(ArcStr, Value); 5]>()?;
        // Fields are sorted alphabetically: close, high, low, open, x
        let x_val = &first_fields[4].1;
        if matches!(x_val, Value::DateTime(_)) {
            Ok(Self::DateTime(
                a.iter()
                    .map(|v| TimeOHLCPoint::from_value(v.clone()))
                    .collect::<Result<_>>()?,
            ))
        } else {
            Ok(Self::Numeric(
                a.iter()
                    .map(|v| OHLCPoint::from_value(v.clone()))
                    .collect::<Result<_>>()?,
            ))
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

#[derive(Clone, Copy, Debug)]
pub(crate) struct TimeEBPoint {
    pub x: DateTime<Utc>,
    pub min: f64,
    pub avg: f64,
    pub max: f64,
}

impl FromValue for TimeEBPoint {
    fn from_value(v: Value) -> Result<Self> {
        let [(_, avg), (_, max), (_, min), (_, x)] =
            v.cast_to::<[(ArcStr, Value); 4]>()?;
        Ok(Self {
            x: x.cast_to::<DateTime<Utc>>()?,
            min: min.cast_to::<f64>()?,
            avg: avg.cast_to::<f64>()?,
            max: max.cast_to::<f64>()?,
        })
    }
}

/// Error bar data: either numeric or time-series x-axis.
pub(crate) enum EBData {
    Numeric(LPooled<Vec<EBPoint>>),
    DateTime(LPooled<Vec<TimeEBPoint>>),
}

impl FromValue for EBData {
    fn from_value(v: Value) -> Result<Self> {
        let a = match v {
            Value::Array(a) => a,
            _ => bail!("chart error bar data: expected array"),
        };
        if a.is_empty() {
            return Ok(Self::Numeric(LPooled::take()));
        }
        // Check first element's x field type
        let first_fields = a[0].clone().cast_to::<[(ArcStr, Value); 4]>()?;
        // Fields sorted alphabetically: avg, max, min, x
        let x_val = &first_fields[3].1;
        if matches!(x_val, Value::DateTime(_)) {
            Ok(Self::DateTime(
                a.iter()
                    .map(|v| TimeEBPoint::from_value(v.clone()))
                    .collect::<Result<_>>()?,
            ))
        } else {
            Ok(Self::Numeric(
                a.iter()
                    .map(|v| EBPoint::from_value(v.clone()))
                    .collect::<Result<_>>()?,
            ))
        }
    }
}

/// 3D point data: Array<(f64, f64, f64)>.
pub(crate) struct XYZData(LPooled<Vec<(f64, f64, f64)>>);

impl FromValue for XYZData {
    fn from_value(v: Value) -> Result<Self> {
        let a = match v {
            Value::Array(a) => a,
            _ => bail!("chart xyz data: expected array"),
        };
        Ok(Self(
            a.iter()
                .map(|v| v.clone().cast_to::<(f64, f64, f64)>())
                .collect::<Result<_>>()?,
        ))
    }
}

/// Surface data: Array<Array<(f64, f64, f64)>> — a grid of 3D points.
pub(crate) struct SurfaceData(Vec<Vec<(f64, f64, f64)>>);

impl FromValue for SurfaceData {
    fn from_value(v: Value) -> Result<Self> {
        let a = match v {
            Value::Array(a) => a,
            _ => bail!("chart surface data: expected array of arrays"),
        };
        let mut rows = Vec::with_capacity(a.len());
        for row_v in a.iter() {
            let row_a = match row_v {
                Value::Array(a) => a,
                _ => bail!("chart surface data: expected inner array"),
            };
            let row: Vec<(f64, f64, f64)> = row_a
                .iter()
                .map(|v| v.clone().cast_to::<(f64, f64, f64)>())
                .collect::<Result<_>>()?;
            rows.push(row);
        }
        Ok(Self(rows))
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

struct BarStyleV {
    color: Option<iced_core::Color>,
    label: Option<String>,
    margin: Option<f64>,
}

impl FromValue for BarStyleV {
    fn from_value(v: Value) -> Result<Self> {
        let [(_, color), (_, label), (_, margin)] =
            v.cast_to::<[(ArcStr, Value); 3]>()?;
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
            margin: if margin == Value::Null {
                None
            } else {
                Some(margin.cast_to::<f64>()?)
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

struct PieStyleV {
    colors: Option<Vec<iced_core::Color>>,
    donut: Option<f64>,
    label_offset: Option<f64>,
    show_percentages: Option<bool>,
    start_angle: Option<f64>,
}

impl FromValue for PieStyleV {
    fn from_value(v: Value) -> Result<Self> {
        // Fields sorted alphabetically: colors, donut, label_offset, show_percentages, start_angle
        let [(_, colors), (_, donut), (_, label_offset), (_, show_percentages), (_, start_angle)] =
            v.cast_to::<[(ArcStr, Value); 5]>()?;
        Ok(Self {
            colors: if colors == Value::Null {
                None
            } else {
                let arr = match colors {
                    Value::Array(a) => a,
                    _ => bail!("pie colors: expected array"),
                };
                Some(
                    arr.iter()
                        .map(|v| Ok(ColorV::from_value(v.clone())?.0))
                        .collect::<Result<_>>()?,
                )
            },
            donut: if donut == Value::Null {
                None
            } else {
                Some(donut.cast_to::<f64>()?)
            },
            label_offset: if label_offset == Value::Null {
                None
            } else {
                Some(label_offset.cast_to::<f64>()?)
            },
            show_percentages: if show_percentages == Value::Null {
                None
            } else {
                Some(show_percentages.cast_to::<bool>()?)
            },
            start_angle: if start_angle == Value::Null {
                None
            } else {
                Some(start_angle.cast_to::<f64>()?)
            },
        })
    }
}

struct SurfaceStyleV {
    color: Option<iced_core::Color>,
    color_by_z: Option<bool>,
    label: Option<String>,
}

impl FromValue for SurfaceStyleV {
    fn from_value(v: Value) -> Result<Self> {
        // Fields sorted alphabetically: color, color_by_z, label
        let [(_, color), (_, color_by_z), (_, label)] =
            v.cast_to::<[(ArcStr, Value); 3]>()?;
        Ok(Self {
            color: if color == Value::Null {
                None
            } else {
                Some(ColorV::from_value(color)?.0)
            },
            color_by_z: if color_by_z == Value::Null {
                None
            } else {
                Some(color_by_z.cast_to::<bool>()?)
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

// ── Projection3D ───────────────────────────────────────────────────

struct Projection3DV {
    pitch: Option<f64>,
    scale: Option<f64>,
    yaw: Option<f64>,
}

impl FromValue for Projection3DV {
    fn from_value(v: Value) -> Result<Self> {
        // Fields sorted alphabetically: pitch, scale, yaw
        let [(_, pitch), (_, scale), (_, yaw)] = v.cast_to::<[(ArcStr, Value); 3]>()?;
        Ok(Self {
            pitch: if pitch == Value::Null { None } else { Some(pitch.cast_to::<f64>()?) },
            scale: if scale == Value::Null { None } else { Some(scale.cast_to::<f64>()?) },
            yaw: if yaw == Value::Null { None } else { Some(yaw.cast_to::<f64>()?) },
        })
    }
}

struct OptProjection3D(Option<Projection3DV>);

impl FromValue for OptProjection3D {
    fn from_value(v: Value) -> Result<Self> {
        if v == Value::Null {
            Ok(Self(None))
        } else {
            Ok(Self(Some(Projection3DV::from_value(v)?)))
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
    Area,
}

/// A compiled dataset with live reactive data refs.
enum DatasetEntry<X: GXExt> {
    XY { kind: XYKind, data: TRef<X, XYData>, style: SeriesStyleV },
    DashedLine { data: TRef<X, XYData>, dash: f64, gap: f64, style: SeriesStyleV },
    Bar { data: TRef<X, BarData>, style: BarStyleV },
    Candlestick { data: TRef<X, OHLCData>, style: CandlestickStyleV },
    ErrorBar { data: TRef<X, EBData>, style: SeriesStyleV },
    Pie { data: TRef<X, BarData>, style: PieStyleV },
    Scatter3D { data: TRef<X, XYZData>, style: SeriesStyleV },
    Line3D { data: TRef<X, XYZData>, style: SeriesStyleV },
    Surface { data: TRef<X, SurfaceData>, style: SurfaceStyleV },
}

impl<X: GXExt> DatasetEntry<X> {
    fn label(&self) -> Option<&str> {
        match self {
            Self::XY { style, .. }
            | Self::DashedLine { style, .. }
            | Self::ErrorBar { style, .. }
            | Self::Scatter3D { style, .. }
            | Self::Line3D { style, .. } => style.label.as_deref(),
            Self::Bar { style, .. } => style.label.as_deref(),
            Self::Candlestick { style, .. } => style.label.as_deref(),
            Self::Pie { .. } => None,
            Self::Surface { style, .. } => style.label.as_deref(),
        }
    }
}

/// Dataset metadata parsed from the datasets array value before ref compilation.
enum DatasetMeta {
    XY { kind: XYKind, data_id: u64, style: SeriesStyleV },
    DashedLine { data_id: u64, dash: f64, gap: f64, style: SeriesStyleV },
    Bar { data_id: u64, style: BarStyleV },
    Candlestick { data_id: u64, style: CandlestickStyleV },
    ErrorBar { data_id: u64, style: SeriesStyleV },
    Pie { data_id: u64, style: PieStyleV },
    Scatter3D { data_id: u64, style: SeriesStyleV },
    Line3D { data_id: u64, style: SeriesStyleV },
    Surface { data_id: u64, style: SurfaceStyleV },
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
                let style = BarStyleV::from_value(style)?;
                Ok(Self::Bar { data_id, style })
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
            "Pie" => {
                let [(_, data), (_, style)] = inner.cast_to::<[(ArcStr, Value); 2]>()?;
                let data_id = data.cast_to::<u64>()?;
                let style = PieStyleV::from_value(style)?;
                Ok(Self::Pie { data_id, style })
            }
            "Scatter3D" => {
                let [(_, data), (_, style)] = inner.cast_to::<[(ArcStr, Value); 2]>()?;
                let data_id = data.cast_to::<u64>()?;
                let style = SeriesStyleV::from_value(style)?;
                Ok(Self::Scatter3D { data_id, style })
            }
            "Line3D" => {
                let [(_, data), (_, style)] = inner.cast_to::<[(ArcStr, Value); 2]>()?;
                let data_id = data.cast_to::<u64>()?;
                let style = SeriesStyleV::from_value(style)?;
                Ok(Self::Line3D { data_id, style })
            }
            "Surface" => {
                let [(_, data), (_, style)] = inner.cast_to::<[(ArcStr, Value); 2]>()?;
                let data_id = data.cast_to::<u64>()?;
                let style = SurfaceStyleV::from_value(style)?;
                Ok(Self::Surface { data_id, style })
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
    // CR estokes: use an LPooled<Vec> here
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
            DatasetMeta::Bar { data_id, style } => {
                let data_ref = gx.compile_ref(data_id).await?;
                let data = TRef::new(data_ref).context("chart bar data")?;
                entries.push(DatasetEntry::Bar { data, style });
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
            DatasetMeta::Pie { data_id, style } => {
                let data_ref = gx.compile_ref(data_id).await?;
                let data = TRef::new(data_ref).context("chart pie data")?;
                entries.push(DatasetEntry::Pie { data, style });
            }
            DatasetMeta::Scatter3D { data_id, style } => {
                let data_ref = gx.compile_ref(data_id).await?;
                let data = TRef::new(data_ref).context("chart scatter3d data")?;
                entries.push(DatasetEntry::Scatter3D { data, style });
            }
            DatasetMeta::Line3D { data_id, style } => {
                let data_ref = gx.compile_ref(data_id).await?;
                let data = TRef::new(data_ref).context("chart line3d data")?;
                entries.push(DatasetEntry::Line3D { data, style });
            }
            DatasetMeta::Surface { data_id, style } => {
                let data_ref = gx.compile_ref(data_id).await?;
                let data = TRef::new(data_ref).context("chart surface data")?;
                entries.push(DatasetEntry::Surface { data, style });
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
    z_label: TRef<X, Option<String>>,
    x_range: TRef<X, OptXAxisRange>,
    y_range: TRef<X, OptAxisRange>,
    z_range: TRef<X, OptAxisRange>,
    projection: TRef<X, OptProjection3D>,
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
        let [(_, background), (_, datasets), (_, height), (_, legend_position), (_, legend_style), (_, margin), (_, mesh), (_, projection), (_, title), (_, title_color), (_, title_size), (_, width), (_, x_label), (_, x_range), (_, y_label), (_, y_range), (_, z_label), (_, z_range)] =
            source.cast_to::<[(ArcStr, u64); 18]>().context("chart flds")?;
        let (
            background_ref,
            datasets_ref,
            height_ref,
            legend_position_ref,
            legend_style_ref,
            margin_ref,
            mesh_ref,
            projection_ref,
            title_ref,
            title_color_ref,
            title_size_ref,
            width_ref,
            x_label_ref,
            x_range_ref,
            y_label_ref,
            y_range_ref,
            z_label_ref,
            z_range_ref,
        ) = try_join! {
            gx.compile_ref(background),
            gx.compile_ref(datasets),
            gx.compile_ref(height),
            gx.compile_ref(legend_position),
            gx.compile_ref(legend_style),
            gx.compile_ref(margin),
            gx.compile_ref(mesh),
            gx.compile_ref(projection),
            gx.compile_ref(title),
            gx.compile_ref(title_color),
            gx.compile_ref(title_size),
            gx.compile_ref(width),
            gx.compile_ref(x_label),
            gx.compile_ref(x_range),
            gx.compile_ref(y_label),
            gx.compile_ref(y_range),
            gx.compile_ref(z_label),
            gx.compile_ref(z_range),
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
            z_label: TRef::new(z_label_ref).context("chart tref z_label")?,
            x_range: TRef::new(x_range_ref).context("chart tref x_range")?,
            y_range: TRef::new(y_range_ref).context("chart tref y_range")?,
            z_range: TRef::new(z_range_ref).context("chart tref z_range")?,
            projection: TRef::new(projection_ref).context("chart tref projection")?,
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
                DatasetEntry::Bar { data, .. } => {
                    data.update(id, v).context("chart update bar data")?.is_some()
                }
                DatasetEntry::Candlestick { data, .. } => {
                    data.update(id, v).context("chart update ohlc data")?.is_some()
                }
                DatasetEntry::ErrorBar { data, .. } => {
                    data.update(id, v).context("chart update errorbar data")?.is_some()
                }
                DatasetEntry::Pie { data, .. } => {
                    data.update(id, v).context("chart update pie data")?.is_some()
                }
                DatasetEntry::Scatter3D { data, .. }
                | DatasetEntry::Line3D { data, .. } => {
                    data.update(id, v).context("chart update 3d data")?.is_some()
                }
                DatasetEntry::Surface { data, .. } => {
                    data.update(id, v).context("chart update surface data")?.is_some()
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
        up!(z_label);
        up!(x_range);
        up!(y_range);
        up!(z_range);
        up!(projection);
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

// ── Chart mode detection ────────────────────────────────────────────

#[derive(Clone, Copy, PartialEq, Eq)]
enum ChartMode {
    Numeric,
    TimeSeries,
    Bar,
    Pie,
    ThreeD,
    Empty,
}

fn chart_mode<X: GXExt>(datasets: &[DatasetEntry<X>]) -> ChartMode {
    let mut has_bar = false;
    let mut has_pie = false;
    let mut has_3d = false;
    let mut has_other = false;
    for ds in datasets {
        match ds {
            DatasetEntry::XY { data, .. } | DatasetEntry::DashedLine { data, .. } => {
                if let Some(d) = data.t.as_ref() {
                    match d {
                        XYData::DateTime(v) if !v.is_empty() => has_other = true,
                        XYData::Numeric(v) if !v.is_empty() => has_other = true,
                        _ => {}
                    }
                }
            }
            DatasetEntry::Bar { data, .. } => {
                if let Some(d) = data.t.as_ref() {
                    if !d.0.is_empty() {
                        has_bar = true;
                    }
                }
            }
            DatasetEntry::Candlestick { data, .. } => {
                if let Some(d) = data.t.as_ref() {
                    match d {
                        OHLCData::DateTime(v) if !v.is_empty() => has_other = true,
                        OHLCData::Numeric(v) if !v.is_empty() => has_other = true,
                        _ => {}
                    }
                }
            }
            DatasetEntry::ErrorBar { data, .. } => {
                if let Some(d) = data.t.as_ref() {
                    match d {
                        EBData::DateTime(v) if !v.is_empty() => has_other = true,
                        EBData::Numeric(v) if !v.is_empty() => has_other = true,
                        _ => {}
                    }
                }
            }
            DatasetEntry::Pie { data, .. } => {
                if let Some(d) = data.t.as_ref() {
                    if !d.0.is_empty() {
                        has_pie = true;
                    }
                }
            }
            DatasetEntry::Scatter3D { data, .. } | DatasetEntry::Line3D { data, .. } => {
                if let Some(d) = data.t.as_ref() {
                    if !d.0.is_empty() {
                        has_3d = true;
                    }
                }
            }
            DatasetEntry::Surface { data, .. } => {
                if let Some(d) = data.t.as_ref() {
                    if !d.0.is_empty() {
                        has_3d = true;
                    }
                }
            }
        }
    }
    let mode_count =
        has_bar as u8 + has_pie as u8 + has_3d as u8 + has_other as u8;
    if mode_count > 1 {
        error!("chart: cannot mix bar, pie, 3D, and XY/timeseries datasets");
        return ChartMode::Empty;
    }
    if has_pie {
        return ChartMode::Pie;
    }
    if has_bar {
        return ChartMode::Bar;
    }
    if has_3d {
        return ChartMode::ThreeD;
    }
    // Determine numeric vs timeseries from first non-empty dataset
    for ds in datasets {
        match ds {
            DatasetEntry::XY { data, .. } | DatasetEntry::DashedLine { data, .. } => {
                if let Some(d) = data.t.as_ref() {
                    match d {
                        XYData::DateTime(v) if !v.is_empty() => {
                            return ChartMode::TimeSeries
                        }
                        XYData::Numeric(v) if !v.is_empty() => return ChartMode::Numeric,
                        _ => {}
                    }
                }
            }
            DatasetEntry::Bar { .. }
            | DatasetEntry::Pie { .. }
            | DatasetEntry::Scatter3D { .. }
            | DatasetEntry::Line3D { .. }
            | DatasetEntry::Surface { .. } => {}
            DatasetEntry::Candlestick { data, .. } => {
                if let Some(d) = data.t.as_ref() {
                    match d {
                        OHLCData::DateTime(v) if !v.is_empty() => {
                            return ChartMode::TimeSeries;
                        }
                        OHLCData::Numeric(v) if !v.is_empty() => {
                            return ChartMode::Numeric
                        }
                        _ => {}
                    }
                }
            }
            DatasetEntry::ErrorBar { data, .. } => {
                if let Some(d) = data.t.as_ref() {
                    match d {
                        EBData::DateTime(v) if !v.is_empty() => {
                            return ChartMode::TimeSeries;
                        }
                        EBData::Numeric(v) if !v.is_empty() => return ChartMode::Numeric,
                        _ => {}
                    }
                }
            }
        }
    }
    ChartMode::Empty
}

// ── Range computation ───────────────────────────────────────────────

/// Compute numeric axis ranges across all dataset entries.
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
                if let Some(XYData::Numeric(pts)) = data.t.as_ref() {
                    for &(x, y) in pts.iter() {
                        extend!(x, y, y);
                    }
                }
            }
            DatasetEntry::Bar { .. }
            | DatasetEntry::Pie { .. }
            | DatasetEntry::Scatter3D { .. }
            | DatasetEntry::Line3D { .. }
            | DatasetEntry::Surface { .. } => {}
            DatasetEntry::Candlestick { data, .. } => {
                if let Some(OHLCData::Numeric(pts)) = data.t.as_ref() {
                    for pt in pts.iter() {
                        extend!(pt.x, pt.low, pt.high);
                    }
                }
            }
            DatasetEntry::ErrorBar { data, .. } => {
                if let Some(EBData::Numeric(pts)) = data.t.as_ref() {
                    for pt in pts.iter() {
                        extend!(pt.x, pt.min, pt.max);
                    }
                }
            }
        }
    }

    (pad_range(x_min, x_max), pad_range(y_min, y_max))
}

/// Compute datetime x-axis and numeric y-axis ranges across all dataset entries.
fn compute_time_ranges<X: GXExt>(
    datasets: &[DatasetEntry<X>],
) -> ((DateTime<Utc>, DateTime<Utc>), (f64, f64)) {
    let mut x_min = DateTime::<Utc>::MAX_UTC;
    let mut x_max = DateTime::<Utc>::MIN_UTC;
    let mut y_min = f64::INFINITY;
    let mut y_max = f64::NEG_INFINITY;

    macro_rules! extend_y {
        ($ylo:expr, $yhi:expr) => {
            if $ylo < y_min {
                y_min = $ylo;
            }
            if $yhi > y_max {
                y_max = $yhi;
            }
        };
    }

    macro_rules! extend_x {
        ($x:expr) => {
            if $x < x_min {
                x_min = $x;
            }
            if $x > x_max {
                x_max = $x;
            }
        };
    }

    for ds in datasets {
        match ds {
            DatasetEntry::XY { data, .. } | DatasetEntry::DashedLine { data, .. } => {
                if let Some(XYData::DateTime(pts)) = data.t.as_ref() {
                    for &(x, y) in pts.iter() {
                        extend_x!(x);
                        extend_y!(y, y);
                    }
                }
            }
            DatasetEntry::Bar { .. }
            | DatasetEntry::Pie { .. }
            | DatasetEntry::Scatter3D { .. }
            | DatasetEntry::Line3D { .. }
            | DatasetEntry::Surface { .. } => {}
            DatasetEntry::Candlestick { data, .. } => {
                if let Some(OHLCData::DateTime(pts)) = data.t.as_ref() {
                    for pt in pts.iter() {
                        extend_x!(pt.x);
                        extend_y!(pt.low, pt.high);
                    }
                }
            }
            DatasetEntry::ErrorBar { data, .. } => {
                if let Some(EBData::DateTime(pts)) = data.t.as_ref() {
                    for pt in pts.iter() {
                        extend_x!(pt.x);
                        extend_y!(pt.min, pt.max);
                    }
                }
            }
        }
    }

    (pad_time_range(x_min, x_max), pad_range(y_min, y_max))
}

/// Compute 3D axis ranges across all 3D dataset entries.
fn compute_3d_ranges<X: GXExt>(
    datasets: &[DatasetEntry<X>],
) -> ((f64, f64), (f64, f64), (f64, f64)) {
    let mut x_min = f64::INFINITY;
    let mut x_max = f64::NEG_INFINITY;
    let mut y_min = f64::INFINITY;
    let mut y_max = f64::NEG_INFINITY;
    let mut z_min = f64::INFINITY;
    let mut z_max = f64::NEG_INFINITY;

    macro_rules! extend3 {
        ($x:expr, $y:expr, $z:expr) => {
            if $x < x_min { x_min = $x; }
            if $x > x_max { x_max = $x; }
            if $y < y_min { y_min = $y; }
            if $y > y_max { y_max = $y; }
            if $z < z_min { z_min = $z; }
            if $z > z_max { z_max = $z; }
        };
    }

    for ds in datasets {
        match ds {
            DatasetEntry::Scatter3D { data, .. }
            | DatasetEntry::Line3D { data, .. } => {
                if let Some(pts) = data.t.as_ref() {
                    for &(x, y, z) in pts.0.iter() {
                        extend3!(x, y, z);
                    }
                }
            }
            DatasetEntry::Surface { data, .. } => {
                if let Some(grid) = data.t.as_ref() {
                    for row in grid.0.iter() {
                        for &(x, y, z) in row.iter() {
                            extend3!(x, y, z);
                        }
                    }
                }
            }
            _ => {}
        }
    }

    (pad_range(x_min, x_max), pad_range(y_min, y_max), pad_range(z_min, z_max))
}

pub(crate) fn pad_range(min: f64, max: f64) -> (f64, f64) {
    if min > max {
        return (-1.0, 1.0);
    }
    let (min, max) = if min == max { (min - 1.0, max + 1.0) } else { (min, max) };
    let pad = (max - min) * 0.05;
    (min - pad, max + pad)
}

/// Estimate the number of decimal places needed for tick labels
/// given the axis range. Plotters generates ~10 ticks, so the step
/// is roughly range/10. We need enough precision to distinguish ticks.
fn tick_precision(range: f64) -> usize {
    let step = range / 10.0;
    if step >= 1.0 {
        1
    } else if step >= 0.1 {
        2
    } else if step >= 0.01 {
        3
    } else {
        4
    }
}

fn pad_time_range(
    min: DateTime<Utc>,
    max: DateTime<Utc>,
) -> (DateTime<Utc>, DateTime<Utc>) {
    if min > max {
        let now = Utc::now();
        return (now - TimeDelta::hours(1), now + TimeDelta::hours(1));
    }
    if min == max {
        return (min - TimeDelta::hours(1), max + TimeDelta::hours(1));
    }
    let span = max - min;
    let pad = span * 5 / 100;
    (min - pad, max + pad)
}

// ── X-axis range ───────────────────────────────────────────────────

/// Parsed x-axis range: either numeric or datetime.
enum XAxisRange {
    Numeric { min: f64, max: f64 },
    DateTime { min: DateTime<Utc>, max: DateTime<Utc> },
}

/// Optional x-axis range from graphix value.
struct OptXAxisRange(Option<XAxisRange>);

impl FromValue for OptXAxisRange {
    fn from_value(v: Value) -> Result<Self> {
        if v == Value::Null {
            return Ok(Self(None));
        }
        // Try numeric first
        if let Ok([(_, max), (_, min)]) = v.clone().cast_to::<[(ArcStr, f64); 2]>() {
            return Ok(Self(Some(XAxisRange::Numeric { min, max })));
        }
        // Try datetime
        let [(_, max), (_, min)] = v.cast_to::<[(ArcStr, Value); 2]>()?;
        Ok(Self(Some(XAxisRange::DateTime {
            min: min.cast_to::<DateTime<Utc>>()?,
            max: max.cast_to::<DateTime<Utc>>()?,
        })))
    }
}

// ── Drawing ─────────────────────────────────────────────────────────

/// Draw series data onto a chart context. The macro is parameterized by
/// coordinate type to avoid duplicating the ~200-line series loop for
/// numeric vs. datetime x-axes.
macro_rules! draw_chart_body {
    ($chart:expr, $self:expr, $xy_variant:path, $ohlc_variant:path,
     $eb_variant:path, $label_sz:expr) => {{
        // Draw each dataset
        for (i, ds) in $self.datasets.iter().enumerate() {
            match ds {
                DatasetEntry::XY { kind, data, style } => {
                    let pts = match data.t.as_ref() {
                        Some($xy_variant(p)) => p,
                        _ => continue,
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
                            let series = LineSeries::new(pts.iter().copied(), line_style);
                            match $chart.draw_series(series) {
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
                            match $chart.draw_series(series) {
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
                        XYKind::Area => {
                            let area_fill = color.mix(0.3);
                            let series = AreaSeries::new(
                                pts.iter().copied(),
                                0.0,
                                ShapeStyle::from(area_fill).filled(),
                            )
                            .border_style(line_style);
                            match $chart.draw_series(series) {
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
                        Some($xy_variant(p)) => p,
                        _ => continue,
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
                    match $chart.draw_series(series) {
                        Ok(ann) => {
                            if let Some(l) = label {
                                ann.label(l).legend(move |(x, y)| {
                                    PathElement::new([(x, y), (x + 20, y)], line_style)
                                });
                            }
                        }
                        Err(e) => error!("chart draw dashed: {e:?}"),
                    }
                }

                // These dataset types are rendered in their own ChartMode paths
                DatasetEntry::Bar { .. }
                | DatasetEntry::Pie { .. }
                | DatasetEntry::Scatter3D { .. }
                | DatasetEntry::Line3D { .. }
                | DatasetEntry::Surface { .. } => {}

                DatasetEntry::Candlestick { data, style } => {
                    let gain =
                        style.gain_color.map(iced_to_plotters).unwrap_or(DEFAULT_GAIN);
                    let loss =
                        style.loss_color.map(iced_to_plotters).unwrap_or(DEFAULT_LOSS);
                    let bw = style.bar_width.unwrap_or(5.0) as u32;
                    let label = style.label.as_deref();

                    match data.t.as_ref() {
                        Some($ohlc_variant(pts)) => {
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
                            match $chart.draw_series(series) {
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
                        _ => continue,
                    }
                }

                DatasetEntry::ErrorBar { data, style } => {
                    let color = style
                        .color
                        .map(iced_to_plotters)
                        .unwrap_or(PALETTE[i % PALETTE.len()]);
                    let sw = style.stroke_width.unwrap_or(2.0) as u32;
                    let line_style = ShapeStyle::from(color).stroke_width(sw);
                    let label = style.label.as_deref();

                    match data.t.as_ref() {
                        Some($eb_variant(pts)) => {
                            let series = pts.iter().map(|pt| {
                                ErrorBar::new_vertical(
                                    pt.x, pt.min, pt.avg, pt.max, line_style, sw,
                                )
                            });
                            match $chart.draw_series(series) {
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
                        _ => continue,
                    }
                }
            }
        }

        // Legend
        let has_labels = $self.datasets.iter().any(|ds| ds.label().is_some());
        if has_labels {
            let legend_pos = $self
                .legend_position
                .t
                .as_ref()
                .and_then(|o| o.0.as_ref())
                .map(|p| p.0.clone())
                .unwrap_or(SeriesLabelPosition::UpperLeft);
            let ls = $self.legend_style.t.as_ref().and_then(|o| o.0.as_ref());
            let legend_bg =
                ls.and_then(|s| s.background).map(iced_to_plotters).unwrap_or(WHITE);
            let legend_border =
                ls.and_then(|s| s.border).map(iced_to_plotters).unwrap_or(BLACK);
            let legend_font_sz = ls.and_then(|s| s.label_size).unwrap_or($label_sz);
            let mut labels = $chart.configure_series_labels();
            labels.position(legend_pos);
            labels.margin(15);
            labels.background_style(legend_bg.mix(0.8));
            labels.border_style(legend_border);
            let mut style = TextStyle::from(("sans-serif", legend_font_sz).into_font());
            if let Some(lc) = ls.and_then(|s| s.label_color) {
                style.color = iced_to_plotters(lc).to_backend_color();
            }
            labels.label_font(style);
            if let Err(e) = labels.draw() {
                error!("chart series labels draw: {e:?}");
            }
        }
    }};
}

/// Set up the mesh on a chart context. Shared between numeric and datetime modes.
macro_rules! configure_mesh {
    ($chart:expr, $x_label:expr, $y_label:expr, $mesh_style:expr) => {{
        let mut mesh_cfg = $chart.configure_mesh();
        if let Some(xl) = $x_label {
            mesh_cfg.x_desc(xl);
        }
        if let Some(yl) = $y_label {
            mesh_cfg.y_desc(yl);
        }
        if let Some(ms) = $mesh_style {
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
                    let mut style = TextStyle::from(("sans-serif", s).into_font());
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
    }};
}

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

            let mode = chart_mode(&self.datasets);
            if mode == ChartMode::Empty {
                return;
            }

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

            let y_range_opt = self.y_range.t.as_ref().and_then(|r| r.0.as_ref());

            match mode {
                ChartMode::Numeric => {
                    let (auto_x, auto_y) = compute_ranges(&self.datasets);
                    let (x_min, x_max) =
                        match self.x_range.t.as_ref().and_then(|r| r.0.as_ref()) {
                            Some(XAxisRange::Numeric { min, max }) => (*min, *max),
                            _ => auto_x,
                        };
                    let (y_min, y_max) = match y_range_opt {
                        Some(r) => (r.min, r.max),
                        None => auto_y,
                    };

                    // Compute label areas
                    let (_, tick_h) = estimate_text("0", label_sz as f64);
                    let prec = tick_precision(y_max - y_min);
                    let y_min_s = format!("{y_min:.prec$}");
                    let y_max_s = format!("{y_max:.prec$}");
                    let widest =
                        if y_min_s.len() > y_max_s.len() { &y_min_s } else { &y_max_s };
                    let (tick_w, _) = estimate_text(widest, label_sz as f64);
                    let auto_y_area =
                        if y_label.is_some() { tick_w + tick_h + 15 } else { tick_w + 8 };
                    let auto_x_area =
                        if x_label.is_some() { tick_h * 2 + 15 } else { tick_h + 8 };
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

                    let mut chart =
                        match builder.build_cartesian_2d(x_min..x_max, y_min..y_max) {
                            Ok(c) => c,
                            Err(_) => return,
                        };
                    configure_mesh!(chart, x_label, y_label, mesh_style);
                    draw_chart_body!(
                        chart,
                        self,
                        XYData::Numeric,
                        OHLCData::Numeric,
                        EBData::Numeric,
                        label_sz
                    );
                }

                ChartMode::TimeSeries => {
                    let (auto_x, auto_y) = compute_time_ranges(&self.datasets);
                    let (x_min, x_max) =
                        match self.x_range.t.as_ref().and_then(|r| r.0.as_ref()) {
                            Some(XAxisRange::DateTime { min, max }) => (*min, *max),
                            _ => auto_x,
                        };
                    let (y_min, y_max) = match y_range_opt {
                        Some(r) => (r.min, r.max),
                        None => auto_y,
                    };

                    // Compute label areas — datetime ticks are wider
                    let (_, tick_h) = estimate_text("0", label_sz as f64);
                    let prec = tick_precision(y_max - y_min);
                    let y_min_s = format!("{y_min:.prec$}");
                    let y_max_s = format!("{y_max:.prec$}");
                    let widest =
                        if y_min_s.len() > y_max_s.len() { &y_min_s } else { &y_max_s };
                    let (tick_w, _) = estimate_text(widest, label_sz as f64);
                    let auto_y_area =
                        if y_label.is_some() { tick_w + tick_h + 15 } else { tick_w + 8 };
                    let auto_x_area =
                        if x_label.is_some() { tick_h * 2 + 20 } else { tick_h + 12 };
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

                    let mut chart =
                        match builder.build_cartesian_2d(x_min..x_max, y_min..y_max) {
                            Ok(c) => c,
                            Err(_) => return,
                        };
                    configure_mesh!(chart, x_label, y_label, mesh_style);
                    draw_chart_body!(
                        chart,
                        self,
                        XYData::DateTime,
                        OHLCData::DateTime,
                        EBData::DateTime,
                        label_sz
                    );
                }

                ChartMode::Bar => {
                    // Collect unique categories (preserving first-appearance order)
                    // and compute y-range across all bar datasets.
                    let mut categories: Vec<String> = Vec::new();
                    let mut y_min = f64::INFINITY;
                    let mut y_max = f64::NEG_INFINITY;
                    for ds in self.datasets.iter() {
                        if let DatasetEntry::Bar { data, .. } = ds {
                            if let Some(bd) = data.t.as_ref() {
                                for (cat, val) in bd.0.iter() {
                                    if !categories.iter().any(|c| c == cat) {
                                        categories.push(cat.clone());
                                    }
                                    if *val < y_min {
                                        y_min = *val;
                                    }
                                    if *val > y_max {
                                        y_max = *val;
                                    }
                                }
                            }
                        }
                    }
                    if categories.is_empty() {
                        return;
                    }
                    // Extend y-range to include 0 (bars should start from 0)
                    if y_min > 0.0 {
                        y_min = 0.0;
                    }
                    if y_max < 0.0 {
                        y_max = 0.0;
                    }
                    let (y_min, y_max) = match y_range_opt {
                        Some(r) => (r.min, r.max),
                        None => pad_range(y_min, y_max),
                    };

                    // Compute label areas
                    let (_, tick_h) = estimate_text("0", label_sz as f64);
                    let prec = tick_precision(y_max - y_min);
                    let y_min_s = format!("{y_min:.prec$}");
                    let y_max_s = format!("{y_max:.prec$}");
                    let widest = if y_min_s.len() > y_max_s.len() {
                        &y_min_s
                    } else {
                        &y_max_s
                    };
                    let (tick_w, _) = estimate_text(widest, label_sz as f64);
                    let auto_y_area =
                        if y_label.is_some() { tick_w + tick_h + 15 } else { tick_w + 8 };
                    let auto_x_area =
                        if x_label.is_some() { tick_h * 2 + 15 } else { tick_h + 8 };
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

                    let mut chart = match builder.build_cartesian_2d(
                        categories.as_slice().into_segmented(),
                        y_min..y_max,
                    ) {
                        Ok(c) => c,
                        Err(_) => return,
                    };
                    configure_mesh!(chart, x_label, y_label, mesh_style);

                    // Draw each bar dataset
                    for (i, ds) in self.datasets.iter().enumerate() {
                        if let DatasetEntry::Bar { data, style } = ds {
                            if let Some(bd) = data.t.as_ref() {
                                let color = style
                                    .color
                                    .map(iced_to_plotters)
                                    .unwrap_or(PALETTE[i % PALETTE.len()]);
                                let fill_style = ShapeStyle::from(color).filled();
                                let margin_px =
                                    style.margin.unwrap_or(5.0) as u32;
                                let hist = Histogram::vertical(&chart)
                                    .style(fill_style)
                                    .margin(margin_px)
                                    .data(
                                        bd.0.iter().map(|(cat, val)| (cat, *val)),
                                    );
                                match chart.draw_series(hist) {
                                    Ok(ann) => {
                                        if let Some(l) = style.label.as_deref() {
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
                        }
                    }

                    // Legend
                    let has_labels =
                        self.datasets.iter().any(|ds| ds.label().is_some());
                    if has_labels {
                        let legend_pos = self
                            .legend_position
                            .t
                            .as_ref()
                            .and_then(|o| o.0.as_ref())
                            .map(|p| p.0.clone())
                            .unwrap_or(SeriesLabelPosition::UpperLeft);
                        let ls =
                            self.legend_style.t.as_ref().and_then(|o| o.0.as_ref());
                        let legend_bg = ls
                            .and_then(|s| s.background)
                            .map(iced_to_plotters)
                            .unwrap_or(WHITE);
                        let legend_border = ls
                            .and_then(|s| s.border)
                            .map(iced_to_plotters)
                            .unwrap_or(BLACK);
                        let legend_font_sz =
                            ls.and_then(|s| s.label_size).unwrap_or(label_sz);
                        let mut labels = chart.configure_series_labels();
                        labels.position(legend_pos);
                        labels.margin(15);
                        labels.background_style(legend_bg.mix(0.8));
                        labels.border_style(legend_border);
                        let mut style = TextStyle::from(
                            ("sans-serif", legend_font_sz).into_font(),
                        );
                        if let Some(lc) = ls.and_then(|s| s.label_color) {
                            style.color = iced_to_plotters(lc).to_backend_color();
                        }
                        labels.label_font(style);
                        if let Err(e) = labels.draw() {
                            error!("chart series labels draw: {e:?}");
                        }
                    }
                }

                ChartMode::Pie => {
                    // Pie is drawn directly on the DrawingArea, no ChartBuilder
                    // Find the first Pie dataset
                    let (pie_data, pie_style) = match self.datasets.iter().find_map(|ds| {
                        if let DatasetEntry::Pie { data, style } = ds {
                            data.t.as_ref().map(|d| (d, style))
                        } else {
                            None
                        }
                    }) {
                        Some(v) => v,
                        None => return,
                    };

                    // Account for title height
                    let title_h = if title.is_some() {
                        let (_, th) = estimate_text(title.unwrap_or(""), title_size as f64);
                        th + margin as u32
                    } else {
                        0
                    };

                    let center_x = (w / 2) as i32;
                    let center_y = ((h + title_h) / 2) as i32;
                    let radius = (w.min(h - title_h) as f64 * 0.35).max(10.0);

                    let labels: Vec<String> =
                        pie_data.0.iter().map(|(l, _)| l.clone()).collect();
                    let sizes: Vec<f64> =
                        pie_data.0.iter().map(|(_, v)| *v).collect();
                    let colors: Vec<RGBColor> = match &pie_style.colors {
                        Some(cs) => cs.iter().map(|c| iced_to_plotters(*c)).collect(),
                        None => (0..sizes.len())
                            .map(|i| PALETTE[i % PALETTE.len()])
                            .collect(),
                    };
                    let label_strs: Vec<&str> =
                        labels.iter().map(|s| s.as_str()).collect();

                    let center = (center_x, center_y);
                    let mut pie = Pie::new(
                        &center,
                        &radius,
                        &sizes,
                        &colors,
                        &label_strs,
                    );
                    pie.label_style(
                        ("sans-serif", label_sz).into_font(),
                    );
                    if let Some(angle) = pie_style.start_angle {
                        pie.start_angle(angle);
                    }
                    if let Some(hole) = pie_style.donut {
                        pie.donut_hole(hole);
                    }
                    if pie_style.show_percentages == Some(true) {
                        pie.percentages(
                            ("sans-serif", label_sz * 0.9).into_font(),
                        );
                    }
                    if let Some(offset) = pie_style.label_offset {
                        pie.label_offset(offset);
                    }
                    if let Err(e) = root.draw(&pie) {
                        error!("chart draw pie: {e:?}");
                    }
                }

                ChartMode::ThreeD => {
                    let (auto_x, auto_y, auto_z) =
                        compute_3d_ranges(&self.datasets);
                    let (x_min, x_max) =
                        match self.x_range.t.as_ref().and_then(|r| r.0.as_ref()) {
                            Some(XAxisRange::Numeric { min, max }) => (*min, *max),
                            _ => auto_x,
                        };
                    let (y_min, y_max) = match y_range_opt {
                        Some(r) => (r.min, r.max),
                        None => auto_y,
                    };
                    let z_range_opt =
                        self.z_range.t.as_ref().and_then(|r| r.0.as_ref());
                    let (z_min, z_max) = match z_range_opt {
                        Some(r) => (r.min, r.max),
                        None => auto_z,
                    };

                    let x_area = mesh_style
                        .and_then(|ms| ms.x_label_area_size)
                        .map(|s| s as u32)
                        .unwrap_or(30);
                    let y_area = mesh_style
                        .and_then(|ms| ms.y_label_area_size)
                        .map(|s| s as u32)
                        .unwrap_or(30);
                    builder.x_label_area_size(x_area);
                    builder.y_label_area_size(y_area);

                    let mut chart = match builder.build_cartesian_3d(
                        x_min..x_max,
                        z_min..z_max,
                        y_min..y_max,
                    ) {
                        Ok(c) => c,
                        Err(_) => return,
                    };

                    // Apply projection
                    let proj = self
                        .projection
                        .t
                        .as_ref()
                        .and_then(|o| o.0.as_ref());
                    chart.with_projection(|mut pb| {
                        if let Some(p) = proj {
                            if let Some(yaw) = p.yaw {
                                pb.yaw = yaw;
                            }
                            if let Some(pitch) = p.pitch {
                                pb.pitch = pitch;
                            }
                            if let Some(scale) = p.scale {
                                pb.scale = scale;
                            }
                        }
                        pb.into_matrix()
                    });

                    // Configure axes
                    {
                        let mut axes = chart.configure_axes();
                        if mesh_style.and_then(|ms| ms.label_size).is_some()
                            || mesh_style.and_then(|ms| ms.label_color).is_some()
                        {
                            let s = mesh_style
                                .and_then(|ms| ms.label_size)
                                .unwrap_or(12.0);
                            if let Some(lc) =
                                mesh_style.and_then(|ms| ms.label_color)
                            {
                                let mut style =
                                    TextStyle::from(("sans-serif", s).into_font());
                                style.color =
                                    iced_to_plotters(lc).to_backend_color();
                                axes.label_style(style);
                            } else {
                                axes.label_style(("sans-serif", s).into_font());
                            }
                        }
                        // 3D axes use x_formatter/y_formatter/z_formatter
                        // with label prefix when axis labels are set.
                        let x_pfx = x_label.map(|l| format!("{l}: "));
                        let y_pfx = y_label.map(|l| format!("{l}: "));
                        let z_label_str = self.z_label.t.as_ref().and_then(|o| o.as_deref());
                        let z_pfx = z_label_str.map(|l| format!("{l}: "));
                        let x_fn = |x: &f64| match &x_pfx {
                            Some(pfx) => format!("{pfx}{x:.1}"),
                            None => format!("{x:.1}"),
                        };
                        let y_fn = |y: &f64| match &z_pfx {
                            Some(pfx) => format!("{pfx}{y:.1}"),
                            None => format!("{y:.1}"),
                        };
                        let z_fn = |z: &f64| match &y_pfx {
                            Some(pfx) => format!("{pfx}{z:.1}"),
                            None => format!("{z:.1}"),
                        };
                        axes.x_formatter(&x_fn);
                        axes.y_formatter(&y_fn);
                        axes.z_formatter(&z_fn);
                        if let Err(e) = axes.draw() {
                            error!("chart 3d axes draw: {e:?}");
                        }
                    }

                    // Draw each 3D dataset
                    for (i, ds) in self.datasets.iter().enumerate() {
                        match ds {
                            DatasetEntry::Scatter3D { data, style } => {
                                if let Some(pts) = data.t.as_ref() {
                                    let color = style
                                        .color
                                        .map(iced_to_plotters)
                                        .unwrap_or(PALETTE[i % PALETTE.len()]);
                                    let ps =
                                        style.point_size.unwrap_or(3.0) as u32;
                                    let fill_style =
                                        ShapeStyle::from(color).filled();
                                    let series = pts.0.iter().map(|&(x, y, z)| {
                                        Circle::new((x, z, y), ps, fill_style)
                                    });
                                    match chart.draw_series(series) {
                                        Ok(ann) => {
                                            if let Some(l) =
                                                style.label.as_deref()
                                            {
                                                ann.label(l).legend(
                                                    move |(x, y)| {
                                                        Circle::new(
                                                            (x, y),
                                                            ps,
                                                            fill_style,
                                                        )
                                                    },
                                                );
                                            }
                                        }
                                        Err(e) => error!(
                                            "chart draw scatter3d: {e:?}"
                                        ),
                                    }
                                }
                            }
                            DatasetEntry::Line3D { data, style } => {
                                if let Some(pts) = data.t.as_ref() {
                                    let color = style
                                        .color
                                        .map(iced_to_plotters)
                                        .unwrap_or(PALETTE[i % PALETTE.len()]);
                                    let sw = style
                                        .stroke_width
                                        .unwrap_or(2.0)
                                        as u32;
                                    let line_style =
                                        ShapeStyle::from(color).stroke_width(sw);
                                    let series = LineSeries::new(
                                        pts.0
                                            .iter()
                                            .map(|&(x, y, z)| (x, z, y)),
                                        line_style,
                                    );
                                    match chart.draw_series(series) {
                                        Ok(ann) => {
                                            if let Some(l) =
                                                style.label.as_deref()
                                            {
                                                ann.label(l).legend(
                                                    move |(x, y)| {
                                                        PathElement::new(
                                                            [
                                                                (x, y),
                                                                (x + 20, y),
                                                            ],
                                                            line_style,
                                                        )
                                                    },
                                                );
                                            }
                                        }
                                        Err(e) => {
                                            error!("chart draw line3d: {e:?}")
                                        }
                                    }
                                }
                            }
                            DatasetEntry::Surface { data, style } => {
                                if let Some(grid) = data.t.as_ref() {
                                    if grid.0.is_empty()
                                        || grid.0[0].is_empty()
                                    {
                                        continue;
                                    }
                                    let color = style
                                        .color
                                        .map(iced_to_plotters)
                                        .unwrap_or(PALETTE[i % PALETTE.len()]);
                                    let color_by_z =
                                        style.color_by_z.unwrap_or(false);

                                    // Extract unique x and y values from the grid
                                    let x_vals: Vec<f64> = grid
                                        .0
                                        .iter()
                                        .map(|row| row[0].0)
                                        .collect();
                                    let y_vals: Vec<f64> = grid.0[0]
                                        .iter()
                                        .map(|pt| pt.1)
                                        .collect();

                                    let z_lookup = |x: f64, y: f64| -> f64 {
                                        for row in grid.0.iter() {
                                            for &(gx, gy, gz) in row.iter() {
                                                if (gx - x).abs() < 1e-10
                                                    && (gy - y).abs() < 1e-10
                                                {
                                                    return gz;
                                                }
                                            }
                                        }
                                        0.0
                                    };
                                    if color_by_z {
                                        let z_color = |z: &f64| {
                                            // Map z to a hue gradient (blue→red)
                                            let t = if z_max > z_min {
                                                (z - z_min) / (z_max - z_min)
                                            } else {
                                                0.5
                                            };
                                            let hue = (1.0 - t) * 240.0;
                                            let (r, g, b) =
                                                hsl_to_rgb(hue, 0.8, 0.5);
                                            RGBColor(r, g, b)
                                                .mix(0.6)
                                                .filled()
                                        };
                                        let series = SurfaceSeries::xoz(
                                            x_vals.iter().copied(),
                                            y_vals.iter().copied(),
                                            |x, y| z_lookup(x, y),
                                        )
                                        .style_func(&z_color);
                                        match chart.draw_series(series) {
                                            Ok(ann) => {
                                                if let Some(l) =
                                                    style.label.as_deref()
                                                {
                                                    let fill =
                                                        ShapeStyle::from(color)
                                                            .filled();
                                                    ann.label(l).legend(
                                                        move |(x, y)| {
                                                            plotters::element::Rectangle::new(
                                                                [(x, y - 5), (x + 20, y + 5)],
                                                                fill,
                                                            )
                                                        },
                                                    );
                                                }
                                            }
                                            Err(e) => error!(
                                                "chart draw surface: {e:?}"
                                            ),
                                        }
                                    } else {
                                        let fill_style = color.mix(0.6).filled();
                                        let series = SurfaceSeries::xoz(
                                            x_vals.iter().copied(),
                                            y_vals.iter().copied(),
                                            |x, y| z_lookup(x, y),
                                        )
                                        .style(fill_style);
                                        match chart.draw_series(series) {
                                            Ok(ann) => {
                                                if let Some(l) =
                                                    style.label.as_deref()
                                                {
                                                    ann.label(l).legend(
                                                        move |(x, y)| {
                                                            plotters::element::Rectangle::new(
                                                                [(x, y - 5), (x + 20, y + 5)],
                                                                fill_style,
                                                            )
                                                        },
                                                    );
                                                }
                                            }
                                            Err(e) => error!(
                                                "chart draw surface: {e:?}"
                                            ),
                                        }
                                    }
                                }
                            }
                            _ => {}
                        }
                    }

                    // Legend
                    let has_labels =
                        self.datasets.iter().any(|ds| ds.label().is_some());
                    if has_labels {
                        let legend_pos = self
                            .legend_position
                            .t
                            .as_ref()
                            .and_then(|o| o.0.as_ref())
                            .map(|p| p.0.clone())
                            .unwrap_or(SeriesLabelPosition::UpperLeft);
                        let ls =
                            self.legend_style.t.as_ref().and_then(|o| o.0.as_ref());
                        let legend_bg = ls
                            .and_then(|s| s.background)
                            .map(iced_to_plotters)
                            .unwrap_or(WHITE);
                        let legend_border = ls
                            .and_then(|s| s.border)
                            .map(iced_to_plotters)
                            .unwrap_or(BLACK);
                        let legend_font_sz =
                            ls.and_then(|s| s.label_size).unwrap_or(label_sz);
                        let mut labels = chart.configure_series_labels();
                        labels.position(legend_pos);
                        labels.margin(15);
                        labels.background_style(legend_bg.mix(0.8));
                        labels.border_style(legend_border);
                        let mut style = TextStyle::from(
                            ("sans-serif", legend_font_sz).into_font(),
                        );
                        if let Some(lc) = ls.and_then(|s| s.label_color) {
                            style.color = iced_to_plotters(lc).to_backend_color();
                        }
                        labels.label_font(style);
                        if let Err(e) = labels.draw() {
                            error!("chart series labels draw: {e:?}");
                        }
                    }
                }

                ChartMode::Empty => unreachable!(),
            }

            if let Err(e) = root.present() {
                error!("chart present: {e:?}");
            }
        });
        vec![geom]
    }
}

/// Convert HSL to RGB (hue in degrees 0..360, s and l in 0..1).
fn hsl_to_rgb(h: f64, s: f64, l: f64) -> (u8, u8, u8) {
    let c = (1.0 - (2.0 * l - 1.0).abs()) * s;
    let h2 = h / 60.0;
    let x = c * (1.0 - (h2 % 2.0 - 1.0).abs());
    let (r1, g1, b1) = if h2 < 1.0 {
        (c, x, 0.0)
    } else if h2 < 2.0 {
        (x, c, 0.0)
    } else if h2 < 3.0 {
        (0.0, c, x)
    } else if h2 < 4.0 {
        (0.0, x, c)
    } else if h2 < 5.0 {
        (x, 0.0, c)
    } else {
        (c, 0.0, x)
    };
    let m = l - c / 2.0;
    (
        ((r1 + m) * 255.0) as u8,
        ((g1 + m) * 255.0) as u8,
        ((b1 + m) * 255.0) as u8,
    )
}
