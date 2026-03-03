use crate::types::ColorV;
use anyhow::{bail, Result};
use arcstr::ArcStr;
use chrono::{DateTime, Utc};
use netidx::publisher::{FromValue, Value};
use plotters::prelude::SeriesLabelPosition;
use poolshark::local::LPooled;

// ── Data point types ────────────────────────────────────────────────

/// XY data: either numeric (f64, f64) or time-series (DateTime<Utc>, f64).
pub(super) enum XYData {
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
pub(super) struct BarData(pub LPooled<Vec<(String, f64)>>);

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
pub(super) struct OHLCPoint {
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
pub(super) struct TimeOHLCPoint {
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
pub(super) enum OHLCData {
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
pub(super) struct EBPoint {
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
pub(super) struct TimeEBPoint {
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
pub(super) enum EBData {
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
pub(super) struct XYZData(pub LPooled<Vec<(f64, f64, f64)>>);

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
pub(super) struct SurfaceData(pub Vec<Vec<(f64, f64, f64)>>);

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

pub(super) struct SeriesStyleV {
    pub color: Option<iced_core::Color>,
    pub label: Option<String>,
    pub stroke_width: Option<f64>,
    pub point_size: Option<f64>,
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

pub(super) struct BarStyleV {
    pub color: Option<iced_core::Color>,
    pub label: Option<String>,
    pub margin: Option<f64>,
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

pub(super) struct CandlestickStyleV {
    pub gain_color: Option<iced_core::Color>,
    pub loss_color: Option<iced_core::Color>,
    pub bar_width: Option<f64>,
    pub label: Option<String>,
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

pub(super) struct PieStyleV {
    pub colors: Option<Vec<iced_core::Color>>,
    pub donut: Option<f64>,
    pub label_offset: Option<f64>,
    pub show_percentages: Option<bool>,
    pub start_angle: Option<f64>,
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

pub(super) struct SurfaceStyleV {
    pub color: Option<iced_core::Color>,
    pub color_by_z: Option<bool>,
    pub label: Option<String>,
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

pub(super) struct MeshStyleV {
    pub show_x_grid: Option<bool>,
    pub show_y_grid: Option<bool>,
    pub grid_color: Option<iced_core::Color>,
    pub axis_color: Option<iced_core::Color>,
    pub label_color: Option<iced_core::Color>,
    pub label_size: Option<f64>,
    pub x_label_area_size: Option<f64>,
    pub x_labels: Option<i64>,
    pub y_label_area_size: Option<f64>,
    pub y_labels: Option<i64>,
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
pub(super) struct OptMeshStyle(pub Option<MeshStyleV>);

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

pub(super) struct LegendStyleV {
    pub background: Option<iced_core::Color>,
    pub border: Option<iced_core::Color>,
    pub label_color: Option<iced_core::Color>,
    pub label_size: Option<f64>,
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
pub(super) struct OptLegendStyle(pub Option<LegendStyleV>);

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
pub(super) struct LegendPositionV(pub SeriesLabelPosition);

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
pub(super) struct OptLegendPosition(pub Option<LegendPositionV>);

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

pub(super) struct OptF64(pub Option<f64>);

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

pub(super) struct OptColor(pub Option<iced_core::Color>);

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

pub(super) struct Projection3DV {
    pub pitch: Option<f64>,
    pub scale: Option<f64>,
    pub yaw: Option<f64>,
}

impl FromValue for Projection3DV {
    fn from_value(v: Value) -> Result<Self> {
        // Fields sorted alphabetically: pitch, scale, yaw
        let [(_, pitch), (_, scale), (_, yaw)] = v.cast_to::<[(ArcStr, Value); 3]>()?;
        Ok(Self {
            pitch: if pitch == Value::Null {
                None
            } else {
                Some(pitch.cast_to::<f64>()?)
            },
            scale: if scale == Value::Null {
                None
            } else {
                Some(scale.cast_to::<f64>()?)
            },
            yaw: if yaw == Value::Null { None } else { Some(yaw.cast_to::<f64>()?) },
        })
    }
}

pub(super) struct OptProjection3D(pub Option<Projection3DV>);

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
pub(super) struct AxisRange {
    pub min: f64,
    pub max: f64,
}

impl FromValue for AxisRange {
    fn from_value(v: Value) -> Result<Self> {
        let [(_, max), (_, min)] = v.cast_to::<[(ArcStr, f64); 2]>()?;
        Ok(AxisRange { min, max })
    }
}

/// Newtype for Option<AxisRange> to satisfy orphan rules.
#[derive(Clone, Debug)]
pub(super) struct OptAxisRange(pub Option<AxisRange>);

impl FromValue for OptAxisRange {
    fn from_value(v: Value) -> Result<Self> {
        if v == Value::Null {
            Ok(Self(None))
        } else {
            Ok(Self(Some(AxisRange::from_value(v)?)))
        }
    }
}

// ── X-axis range ───────────────────────────────────────────────────

/// Parsed x-axis range: either numeric or datetime.
pub(super) enum XAxisRange {
    Numeric { min: f64, max: f64 },
    DateTime { min: DateTime<Utc>, max: DateTime<Utc> },
}

/// Optional x-axis range from graphix value.
pub(super) struct OptXAxisRange(pub Option<XAxisRange>);

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
