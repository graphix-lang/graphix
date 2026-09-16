use crate::types::ColorV;
use anyhow::{Result, bail};
use arcstr::ArcStr;
use chrono::{DateTime, Utc};
use netidx::publisher::{FromValue, Value};
use netidx_derive::FromValue;
use plotters::prelude::SeriesLabelPosition;
use poolshark::local::LPooled;

/// A simple RGBA color that does not depend on iced_core.
#[derive(Clone, Copy, Debug)]
pub struct ChartColor(pub f32, pub f32, pub f32, pub f32);

impl ChartColor {
    pub fn to_plotters_rgb(self) -> plotters::style::RGBColor {
        plotters::style::RGBColor(
            (self.0 * 255.0) as u8,
            (self.1 * 255.0) as u8,
            (self.2 * 255.0) as u8,
        )
    }
}

impl From<iced_core::Color> for ChartColor {
    fn from(c: iced_core::Color) -> Self {
        Self(c.r, c.g, c.b, c.a)
    }
}

impl FromValue for ChartColor {
    fn from_value(v: Value) -> Result<Self> {
        Ok(ColorV::from_value(v)?.0.into())
    }
}

impl From<ChartColor> for iced_core::Color {
    fn from(c: ChartColor) -> Self {
        iced_core::Color::from_rgba(c.0, c.1, c.2, c.3)
    }
}

/// XY data: either numeric (f64, f64) or time-series (DateTime<Utc>, f64).
pub enum XYData {
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
        // Not cast_to: netidx casts any number to DateTime.
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
pub struct BarData(pub LPooled<Vec<(String, f64)>>);

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

#[derive(Clone, Copy, Debug, FromValue)]
pub struct OHLCPoint {
    pub x: f64,
    pub open: f64,
    pub high: f64,
    pub low: f64,
    pub close: f64,
}

#[derive(Clone, Copy, Debug, FromValue)]
pub struct TimeOHLCPoint {
    pub x: DateTime<Utc>,
    pub open: f64,
    pub high: f64,
    pub low: f64,
    pub close: f64,
}

fn datetime_x(point: &Value) -> Result<bool> {
    #[derive(FromValue)]
    struct Fields {
        x: Value,
    }
    let Fields { x } = point.clone().cast_to()?;
    Ok(matches!(x, Value::DateTime(_)))
}

/// OHLC data: either numeric or time-series x-axis.
pub enum OHLCData {
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
        if datetime_x(&a[0])? {
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

#[derive(Clone, Copy, Debug, FromValue)]
pub struct EBPoint {
    pub x: f64,
    pub min: f64,
    pub avg: f64,
    pub max: f64,
}

#[derive(Clone, Copy, Debug, FromValue)]
pub struct TimeEBPoint {
    pub x: DateTime<Utc>,
    pub min: f64,
    pub avg: f64,
    pub max: f64,
}

/// Error bar data: either numeric or time-series x-axis.
pub enum EBData {
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
        if datetime_x(&a[0])? {
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
pub struct XYZData(pub LPooled<Vec<(f64, f64, f64)>>);

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
pub struct SurfaceData(pub Vec<Vec<(f64, f64, f64)>>);

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

#[derive(FromValue)]
pub struct SeriesStyleV {
    pub color: Option<ChartColor>,
    pub label: Option<String>,
    pub stroke_width: Option<f64>,
    pub point_size: Option<f64>,
}

#[derive(FromValue)]
pub struct BarStyleV {
    pub color: Option<ChartColor>,
    pub label: Option<String>,
    pub margin: Option<f64>,
}

#[derive(FromValue)]
pub struct CandlestickStyleV {
    pub gain_color: Option<ChartColor>,
    pub loss_color: Option<ChartColor>,
    pub bar_width: Option<f64>,
    pub label: Option<String>,
}

#[derive(FromValue)]
pub struct PieStyleV {
    pub colors: Option<Vec<ChartColor>>,
    pub donut: Option<f64>,
    pub label_offset: Option<f64>,
    pub show_percentages: Option<bool>,
    pub start_angle: Option<f64>,
}

#[derive(FromValue)]
pub struct SurfaceStyleV {
    pub color: Option<ChartColor>,
    pub color_by_z: Option<bool>,
    pub label: Option<String>,
}

#[derive(FromValue)]
pub struct MeshStyleV {
    pub show_x_grid: Option<bool>,
    pub show_y_grid: Option<bool>,
    pub grid_color: Option<ChartColor>,
    pub bold_line_color: Option<ChartColor>,
    pub axis_color: Option<ChartColor>,
    pub label_color: Option<ChartColor>,
    pub label_size: Option<f64>,
    pub x_label_area_size: Option<f64>,
    pub x_labels: Option<i64>,
    pub x_light_lines: Option<i64>,
    pub y_label_area_size: Option<f64>,
    pub y_labels: Option<i64>,
    pub y_light_lines: Option<i64>,
    pub z_labels: Option<i64>,
    pub z_light_lines: Option<i64>,
}

#[derive(FromValue)]
pub struct LegendStyleV {
    pub background: Option<ChartColor>,
    pub border: Option<ChartColor>,
    pub label_color: Option<ChartColor>,
    pub label_size: Option<f64>,
}

#[derive(Clone)]
pub struct LegendPositionV(pub SeriesLabelPosition);

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

#[derive(FromValue)]
pub struct ChartStyleV {
    pub background: Option<ChartColor>,
    pub margin: Option<f64>,
    pub title_size: Option<f64>,
    pub title_color: Option<ChartColor>,
    pub palette: Option<Vec<ChartColor>>,
    pub legend_position: Option<LegendPositionV>,
    pub legend: Option<LegendStyleV>,
    pub mesh: Option<MeshStyleV>,
}

/// Newtype for Option<ChartStyleV> to satisfy orphan rules.
#[derive(FromValue)]
pub struct OptChartStyle(pub Option<ChartStyleV>);

#[derive(FromValue)]
pub struct Projection3DV {
    pub pitch: Option<f64>,
    pub scale: Option<f64>,
    pub yaw: Option<f64>,
}

#[derive(FromValue)]
pub struct OptProjection3D(pub Option<Projection3DV>);

#[derive(Clone, Debug, FromValue)]
pub struct AxisRange {
    pub min: f64,
    pub max: f64,
}

/// Newtype for Option<AxisRange> to satisfy orphan rules.
#[derive(Clone, Debug, FromValue)]
pub struct OptAxisRange(pub Option<AxisRange>);

/// Parsed x-axis range: either numeric or datetime.
pub enum XAxisRange {
    Numeric { min: f64, max: f64 },
    DateTime { min: DateTime<Utc>, max: DateTime<Utc> },
}

/// Optional x-axis range from graphix value.
pub struct OptXAxisRange(pub Option<XAxisRange>);

impl FromValue for OptXAxisRange {
    fn from_value(v: Value) -> Result<Self> {
        if v == Value::Null {
            return Ok(Self(None));
        }
        if let Ok(AxisRange { min, max }) = v.clone().cast_to() {
            return Ok(Self(Some(XAxisRange::Numeric { min, max })));
        }
        #[derive(FromValue)]
        struct Fields {
            min: DateTime<Utc>,
            max: DateTime<Utc>,
        }
        let Fields { min, max } = v.cast_to()?;
        Ok(Self(Some(XAxisRange::DateTime { min, max })))
    }
}
