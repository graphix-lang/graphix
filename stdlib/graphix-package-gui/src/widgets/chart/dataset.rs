use super::types::*;
use anyhow::{Context, Result};
use graphix_rt::{GXExt, GXHandle, TRef};
use log::error;
use netidx::publisher::Value;
use netidx_derive::FromValue;
use poolshark::local::LPooled;

#[derive(Clone, Copy)]
pub enum XYKind {
    Line,
    Scatter,
    Area,
}

/// A compiled dataset with live reactive data refs.
pub enum DatasetEntry<X: GXExt> {
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
    pub fn label(&self) -> Option<&str> {
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
#[derive(FromValue)]
enum DatasetMeta {
    Line { data: u64, style: SeriesStyleV },
    Scatter { data: u64, style: SeriesStyleV },
    Bar { data: u64, style: BarStyleV },
    Area { data: u64, style: SeriesStyleV },
    DashedLine { data: u64, dash: f64, gap: f64, style: SeriesStyleV },
    Candlestick { data: u64, style: CandlestickStyleV },
    ErrorBar { data: u64, style: SeriesStyleV },
    Pie { data: u64, style: PieStyleV },
    Scatter3D { data: u64, style: SeriesStyleV },
    Line3D { data: u64, style: SeriesStyleV },
    Surface { data: u64, style: SurfaceStyleV },
}

/// Compile dataset metadata into live entries with data refs.
pub async fn compile_datasets<X: GXExt>(
    gx: &GXHandle<X>,
    v: Value,
) -> Result<LPooled<Vec<DatasetEntry<X>>>> {
    let mut metas = v.cast_to::<LPooled<Vec<DatasetMeta>>>()?;
    let mut entries: LPooled<Vec<DatasetEntry<X>>> = LPooled::take();
    entries.reserve(metas.len());
    for meta in metas.drain(..) {
        let entry = match meta {
            DatasetMeta::Line { data, style } => {
                let data =
                    TRef::new(gx.compile_ref(data).await?).context("chart xy data")?;
                DatasetEntry::XY { kind: XYKind::Line, data, style }
            }
            DatasetMeta::Scatter { data, style } => {
                let data =
                    TRef::new(gx.compile_ref(data).await?).context("chart xy data")?;
                DatasetEntry::XY { kind: XYKind::Scatter, data, style }
            }
            DatasetMeta::Area { data, style } => {
                let data =
                    TRef::new(gx.compile_ref(data).await?).context("chart xy data")?;
                DatasetEntry::XY { kind: XYKind::Area, data, style }
            }
            DatasetMeta::DashedLine { data, dash, gap, style } => {
                let data = TRef::new(gx.compile_ref(data).await?)
                    .context("chart dashed data")?;
                DatasetEntry::DashedLine { data, dash, gap, style }
            }
            DatasetMeta::Bar { data, style } => {
                let data =
                    TRef::new(gx.compile_ref(data).await?).context("chart bar data")?;
                DatasetEntry::Bar { data, style }
            }
            DatasetMeta::Candlestick { data, style } => {
                let data =
                    TRef::new(gx.compile_ref(data).await?).context("chart ohlc data")?;
                DatasetEntry::Candlestick { data, style }
            }
            DatasetMeta::ErrorBar { data, style } => {
                let data = TRef::new(gx.compile_ref(data).await?)
                    .context("chart errorbar data")?;
                DatasetEntry::ErrorBar { data, style }
            }
            DatasetMeta::Pie { data, style } => {
                let data =
                    TRef::new(gx.compile_ref(data).await?).context("chart pie data")?;
                DatasetEntry::Pie { data, style }
            }
            DatasetMeta::Scatter3D { data, style } => {
                let data = TRef::new(gx.compile_ref(data).await?)
                    .context("chart scatter3d data")?;
                DatasetEntry::Scatter3D { data, style }
            }
            DatasetMeta::Line3D { data, style } => {
                let data = TRef::new(gx.compile_ref(data).await?)
                    .context("chart line3d data")?;
                DatasetEntry::Line3D { data, style }
            }
            DatasetMeta::Surface { data, style } => {
                let data = TRef::new(gx.compile_ref(data).await?)
                    .context("chart surface data")?;
                DatasetEntry::Surface { data, style }
            }
        };
        entries.push(entry);
    }
    Ok(entries)
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum ChartMode {
    Numeric,
    TimeSeries,
    Bar,
    Pie,
    ThreeD,
    Empty,
}

pub fn chart_mode<X: GXExt>(datasets: &[DatasetEntry<X>]) -> ChartMode {
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
    for ds in datasets {
        match ds {
            DatasetEntry::XY { data, .. } | DatasetEntry::DashedLine { data, .. } => {
                if let Some(d) = data.t.as_ref() {
                    match d {
                        XYData::DateTime(v) if !v.is_empty() => {
                            return ChartMode::TimeSeries;
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
                            return ChartMode::Numeric;
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
