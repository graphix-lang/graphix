use crate::types::{ColorV, TextSizeV};
use anyhow::{Result, bail};
use arcstr::ArcStr;
use chrono::{DateTime, Utc};
use graphix_compiler::rep;
use netidx::publisher::{FromValue, Value};
use netidx_derive::FromValue;
use plotters::prelude::SeriesLabelPosition;
use poolshark::local::LPooled;

/// A simple RGBA color that does not depend on iced_core.
#[derive(Clone, Copy, Debug)]
pub struct ChartColor(pub f32, pub f32, pub f32, pub f32);

impl ChartColor {
    pub fn to_plotters(self) -> plotters::style::RGBAColor {
        let c = |v: f32| (v.clamp(0.0, 1.0) * 255.0).round() as u8;
        plotters::style::RGBAColor(
            c(self.0),
            c(self.1),
            c(self.2),
            self.3.clamp(0.0, 1.0) as f64,
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

/// A datetime as a time series' x: ms since the epoch.
pub fn datetime_ms(d: &DateTime<Utc>) -> f64 {
    d.timestamp_micros() as f64 / 1000.0
}

/// The datetime a time series' x stands for, clamped to chrono's range.
pub fn ms_datetime(ms: f64) -> DateTime<Utc> {
    let lo = DateTime::<Utc>::MIN_UTC.timestamp_micros();
    let hi = DateTime::<Utc>::MAX_UTC.timestamp_micros();
    let us = if ms.is_nan() { 0 } else { ((ms * 1000.0) as i64).clamp(lo, hi) };
    DateTime::from_timestamp_micros(us).unwrap_or_default()
}

/// Whether an array of points is a time series: its first point's x is a
/// datetime. Not a cast: netidx casts any number to a datetime.
fn is_time_series(a: &[Value], x_of: impl Fn(&Value) -> Option<&Value>) -> bool {
    a.first().and_then(x_of).is_some_and(|x| matches!(x, Value::DateTime(_)))
}

/// A Graphix tuple `(x, y)`.
#[derive(FromValue)]
#[value(shape = "graphix_compiler::rep::GraphixShape")]
struct Pair<X>(X, f64);

/// A Graphix tuple `(x, y, z)`.
#[derive(FromValue)]
#[value(shape = "graphix_compiler::rep::GraphixShape")]
struct Triple(f64, f64, f64);

fn array(v: Value, what: &str) -> Result<netidx::protocol::valarray::ValArray> {
    match v {
        Value::Array(a) => Ok(a),
        _ => bail!("chart {what} data: expected array"),
    }
}

/// XY data; x is in ms since the epoch when `time`.
pub struct XYData {
    pub time: bool,
    pub pts: LPooled<Vec<(f64, f64)>>,
}

impl FromValue for XYData {
    fn from_value(v: Value) -> Result<Self> {
        let a = array(v, "xy")?;
        let time = is_time_series(&a, |p| rep::tuple_elems(p).and_then(|t| t.first()));
        let pts = a
            .iter()
            .map(|v| match time {
                true => v
                    .clone()
                    .cast_to::<Pair<DateTime<Utc>>>()
                    .map(|Pair(x, y)| (datetime_ms(&x), y)),
                false => v.clone().cast_to::<Pair<f64>>().map(|Pair(x, y)| (x, y)),
            })
            .collect::<Result<_>>()?;
        Ok(Self { time, pts })
    }
}

/// Bar chart data: categorical x-axis, numeric y-axis.
pub struct BarData(pub LPooled<Vec<(ArcStr, f64)>>);

impl FromValue for BarData {
    fn from_value(v: Value) -> Result<Self> {
        let a = match v {
            Value::Array(a) => a,
            _ => bail!("chart bar data: expected array"),
        };
        Ok(Self(
            a.iter()
                .map(|v| v.clone().cast_to::<Pair<ArcStr>>().map(|Pair(l, y)| (l, y)))
                .collect::<Result<_>>()?,
        ))
    }
}

/// One candle; x is in ms since the epoch in a time series.
#[derive(Clone, Copy, Debug)]
pub struct OHLCPoint {
    pub x: f64,
    pub open: f64,
    pub high: f64,
    pub low: f64,
    pub close: f64,
}

impl OHLCPoint {
    pub fn is_finite(&self) -> bool {
        [self.x, self.open, self.high, self.low, self.close].iter().all(|v| v.is_finite())
    }
}

/// One error bar; x is in ms since the epoch in a time series.
#[derive(Clone, Copy, Debug)]
pub struct EBPoint {
    pub x: f64,
    pub min: f64,
    pub avg: f64,
    pub max: f64,
}

impl EBPoint {
    pub fn is_finite(&self) -> bool {
        [self.x, self.min, self.avg, self.max].iter().all(|v| v.is_finite())
    }
}

/// Points whose x is a number or, in a time series, a datetime.
pub struct Series<P> {
    pub time: bool,
    pub pts: LPooled<Vec<P>>,
}

pub type OHLCData = Series<OHLCPoint>;
pub type EBData = Series<EBPoint>;

/// A struct point's x as a time series' or a number's.
fn point_x(time: bool, x: Value) -> Result<f64> {
    match time {
        true => Ok(datetime_ms(&x.cast_to::<DateTime<Utc>>()?)),
        false => x.cast_to::<f64>(),
    }
}

fn struct_x(p: &Value) -> Option<&Value> {
    rep::struct_fields(p).and_then(|f| rep::field_named(f, "x")).map(|(_, x)| x)
}

impl FromValue for OHLCData {
    fn from_value(v: Value) -> Result<Self> {
        #[derive(FromValue)]
        #[value(shape = "graphix_compiler::rep::GraphixShape")]
        struct P {
            x: Value,
            open: f64,
            high: f64,
            low: f64,
            close: f64,
        }
        let a = array(v, "ohlc")?;
        let time = is_time_series(&a, struct_x);
        let pts = a
            .iter()
            .map(|v| {
                let P { x, open, high, low, close } = v.clone().cast_to()?;
                Ok(OHLCPoint { x: point_x(time, x)?, open, high, low, close })
            })
            .collect::<Result<_>>()?;
        Ok(Self { time, pts })
    }
}

impl FromValue for EBData {
    fn from_value(v: Value) -> Result<Self> {
        #[derive(FromValue)]
        #[value(shape = "graphix_compiler::rep::GraphixShape")]
        struct P {
            x: Value,
            min: f64,
            avg: f64,
            max: f64,
        }
        let a = array(v, "error bar")?;
        let time = is_time_series(&a, struct_x);
        let pts = a
            .iter()
            .map(|v| {
                let P { x, min, avg, max } = v.clone().cast_to()?;
                Ok(EBPoint { x: point_x(time, x)?, min, avg, max })
            })
            .collect::<Result<_>>()?;
        Ok(Self { time, pts })
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
                .map(|v| v.clone().cast_to::<Triple>().map(|Triple(x, y, z)| (x, y, z)))
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
                .map(|v| v.clone().cast_to::<Triple>().map(|Triple(x, y, z)| (x, y, z)))
                .collect::<Result<_>>()?;
            rows.push(row);
        }
        Ok(Self(rows))
    }
}

#[derive(FromValue)]
#[value(shape = "graphix_compiler::rep::GraphixShape")]
pub struct SeriesStyleV {
    pub color: Option<ChartColor>,
    // CR claude for claude: [style/low] The style labels here, in BarStyleV,
    // CandlestickStyleV and SurfaceStyleV, are `Option<String>`: each style refire
    // copies the Value's ArcStr into a fresh String. Use `Option<ArcStr>`. SurfaceData
    // below is a plain `Vec<Vec<..>>` while every other data type is LPooled; pool it
    // as `LPooled<Vec<LPooled<Vec<..>>>>`. Found by reading; not run. (gui-chart-10)
    pub label: Option<String>,
    pub stroke_width: Option<f64>,
    pub point_size: Option<f64>,
}

#[derive(FromValue)]
#[value(shape = "graphix_compiler::rep::GraphixShape")]
pub struct BarStyleV {
    pub color: Option<ChartColor>,
    pub label: Option<String>,
    pub margin: Option<f64>,
}

#[derive(FromValue)]
#[value(shape = "graphix_compiler::rep::GraphixShape")]
pub struct CandlestickStyleV {
    pub gain_color: Option<ChartColor>,
    pub loss_color: Option<ChartColor>,
    pub bar_width: Option<f64>,
    pub label: Option<String>,
}

#[derive(FromValue)]
#[value(shape = "graphix_compiler::rep::GraphixShape")]
pub struct PieStyleV {
    pub colors: Option<Vec<ChartColor>>,
    pub donut: Option<f64>,
    pub label_offset: Option<f64>,
    pub show_percentages: Option<bool>,
    pub start_angle: Option<f64>,
}

#[derive(FromValue)]
#[value(shape = "graphix_compiler::rep::GraphixShape")]
pub struct SurfaceStyleV {
    pub color: Option<ChartColor>,
    pub color_by_z: Option<bool>,
    pub label: Option<String>,
}

#[derive(FromValue)]
#[value(shape = "graphix_compiler::rep::GraphixShape")]
pub struct MeshStyleV {
    pub show_x_grid: Option<bool>,
    pub show_y_grid: Option<bool>,
    pub grid_color: Option<ChartColor>,
    pub bold_line_color: Option<ChartColor>,
    pub axis_color: Option<ChartColor>,
    pub label_color: Option<ChartColor>,
    pub label_size: Option<TextSizeV>,
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
#[value(shape = "graphix_compiler::rep::GraphixShape")]
pub struct LegendStyleV {
    pub background: Option<ChartColor>,
    pub border: Option<ChartColor>,
    pub label_color: Option<ChartColor>,
    pub label_size: Option<TextSizeV>,
}

#[derive(Clone)]
pub struct LegendPositionV(pub SeriesLabelPosition);

impl FromValue for LegendPositionV {
    fn from_value(v: Value) -> Result<Self> {
        match rep::bare_tag(&v)? {
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
#[value(shape = "graphix_compiler::rep::GraphixShape")]
pub struct ChartStyleV {
    pub background: Option<ChartColor>,
    pub margin: Option<f64>,
    pub title_size: Option<TextSizeV>,
    pub title_color: Option<ChartColor>,
    pub palette: Option<Vec<ChartColor>>,
    pub legend_position: Option<LegendPositionV>,
    pub legend: Option<LegendStyleV>,
    pub mesh: Option<MeshStyleV>,
}

/// Newtype for Option<ChartStyleV> to satisfy orphan rules.
#[derive(FromValue)]
#[value(shape = "graphix_compiler::rep::GraphixShape")]
pub struct OptChartStyle(pub Option<ChartStyleV>);

#[derive(FromValue)]
#[value(shape = "graphix_compiler::rep::GraphixShape")]
pub struct Projection3DV {
    pub pitch: Option<f64>,
    pub scale: Option<f64>,
    pub yaw: Option<f64>,
}

#[derive(FromValue)]
#[value(shape = "graphix_compiler::rep::GraphixShape")]
pub struct OptProjection3D(pub Option<Projection3DV>);

#[derive(Clone, Debug, FromValue)]
#[value(shape = "graphix_compiler::rep::GraphixShape")]
pub struct AxisRange {
    pub min: f64,
    pub max: f64,
}

/// Newtype for Option<AxisRange> to satisfy orphan rules.
#[derive(Clone, Debug, FromValue)]
#[value(shape = "graphix_compiler::rep::GraphixShape")]
pub struct OptAxisRange(pub Option<AxisRange>);

/// The x-axis range; in ms since the epoch when `time`.
pub struct XAxisRange {
    pub time: bool,
    pub min: f64,
    pub max: f64,
}

/// Optional x-axis range from graphix value.
pub struct OptXAxisRange(pub Option<XAxisRange>);

impl FromValue for OptXAxisRange {
    fn from_value(v: Value) -> Result<Self> {
        #[derive(FromValue)]
        #[value(shape = "graphix_compiler::rep::GraphixShape")]
        struct Fields {
            min: Value,
            max: Value,
        }
        if v == Value::Null {
            return Ok(Self(None));
        }
        let Fields { min, max } = v.cast_to()?;
        let time = matches!(min, Value::DateTime(_));
        Ok(Self(Some(XAxisRange {
            time,
            min: point_x(time, min)?,
            max: point_x(time, max)?,
        })))
    }
}
