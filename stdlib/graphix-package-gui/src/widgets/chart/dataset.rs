use super::types::*;
use anyhow::{bail, Context, Result};
use arcstr::ArcStr;
use graphix_rt::{GXExt, GXHandle, TRef};
use log::error;
use netidx::publisher::{FromValue, Value};

// ── Dataset types ───────────────────────────────────────────────────

#[derive(Clone, Copy)]
pub(super) enum XYKind {
    Line,
    Scatter,
    Area,
}

/// A compiled dataset with live reactive data refs.
pub(super) enum DatasetEntry<X: GXExt> {
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
    pub(super) fn label(&self) -> Option<&str> {
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
pub(super) async fn compile_datasets<X: GXExt>(
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

// ── Chart mode detection ────────────────────────────────────────────

#[derive(Clone, Copy, PartialEq, Eq)]
pub(super) enum ChartMode {
    Numeric,
    TimeSeries,
    Bar,
    Pie,
    ThreeD,
    Empty,
}

pub(super) fn chart_mode<X: GXExt>(datasets: &[DatasetEntry<X>]) -> ChartMode {
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
    let mode_count = has_bar as u8 + has_pie as u8 + has_3d as u8 + has_other as u8;
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
