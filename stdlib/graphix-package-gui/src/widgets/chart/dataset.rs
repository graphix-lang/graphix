use super::types::*;
use anyhow::{Context, Result};
use graphix_rt::{GXExt, GXHandle, TRef};
use netidx::publisher::{FromValue, Value};
use netidx_derive::FromValue;
use poolshark::local::LPooled;

#[derive(Clone, Copy)]
pub enum XYKind {
    Line,
    Scatter,
    Area,
    Dashed { dash: f64, gap: f64 },
}

/// A compiled dataset with live reactive data refs.
pub enum DatasetEntry<X: GXExt> {
    XY { kind: XYKind, data: TRef<X, XYData>, style: SeriesStyleV },
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
            | Self::ErrorBar { style, .. }
            | Self::Scatter3D { style, .. }
            | Self::Line3D { style, .. } => style.label.as_deref(),
            Self::Bar { style, .. } => style.label.as_deref(),
            Self::Candlestick { style, .. } => style.label.as_deref(),
            Self::Pie { .. } => None,
            Self::Surface { style, .. } => style.label.as_deref(),
        }
    }

    /// The chart mode this dataset's data asks for; `None` while it has
    /// none.
    pub fn mode(&self) -> Option<ChartMode> {
        fn xy(time: bool) -> ChartMode {
            if time { ChartMode::TimeSeries } else { ChartMode::Numeric }
        }
        match self {
            Self::XY { data, .. } => {
                data.t.as_ref().filter(|d| !d.pts.is_empty()).map(|d| xy(d.time))
            }
            Self::Candlestick { data, .. } => {
                data.t.as_ref().filter(|d| !d.pts.is_empty()).map(|d| xy(d.time))
            }
            Self::ErrorBar { data, .. } => {
                data.t.as_ref().filter(|d| !d.pts.is_empty()).map(|d| xy(d.time))
            }
            Self::Bar { data, .. } => {
                data.t.as_ref().filter(|d| !d.0.is_empty()).map(|_| ChartMode::Bar)
            }
            Self::Pie { data, .. } => {
                data.t.as_ref().filter(|d| !d.0.is_empty()).map(|_| ChartMode::Pie)
            }
            Self::Scatter3D { data, .. } | Self::Line3D { data, .. } => {
                data.t.as_ref().filter(|d| !d.0.is_empty()).map(|_| ChartMode::ThreeD)
            }
            Self::Surface { data, .. } => {
                data.t.as_ref().filter(|d| !d.0.is_empty()).map(|_| ChartMode::ThreeD)
            }
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
    async fn data<X: GXExt, T: FromValue>(
        gx: &GXHandle<X>,
        id: u64,
        what: &'static str,
    ) -> Result<TRef<X, T>> {
        TRef::new(gx.compile_ref(id).await?).context(what)
    }
    let mut metas = v.cast_to::<LPooled<Vec<DatasetMeta>>>()?;
    let mut entries: LPooled<Vec<DatasetEntry<X>>> = LPooled::take();
    for meta in metas.drain(..) {
        let xy = |kind, data, style| DatasetEntry::XY { kind, data, style };
        entries.push(match meta {
            DatasetMeta::Line { data: d, style } => {
                xy(XYKind::Line, data(gx, d, "chart line data").await?, style)
            }
            DatasetMeta::Scatter { data: d, style } => {
                xy(XYKind::Scatter, data(gx, d, "chart scatter data").await?, style)
            }
            DatasetMeta::Area { data: d, style } => {
                xy(XYKind::Area, data(gx, d, "chart area data").await?, style)
            }
            DatasetMeta::DashedLine { data: d, dash, gap, style } => xy(
                XYKind::Dashed { dash, gap },
                data(gx, d, "chart dashed data").await?,
                style,
            ),
            DatasetMeta::Bar { data: d, style } => {
                DatasetEntry::Bar { data: data(gx, d, "chart bar data").await?, style }
            }
            DatasetMeta::Candlestick { data: d, style } => DatasetEntry::Candlestick {
                data: data(gx, d, "chart ohlc data").await?,
                style,
            },
            DatasetMeta::ErrorBar { data: d, style } => DatasetEntry::ErrorBar {
                data: data(gx, d, "chart errorbar data").await?,
                style,
            },
            DatasetMeta::Pie { data: d, style } => {
                DatasetEntry::Pie { data: data(gx, d, "chart pie data").await?, style }
            }
            DatasetMeta::Scatter3D { data: d, style } => DatasetEntry::Scatter3D {
                data: data(gx, d, "chart scatter3d data").await?,
                style,
            },
            DatasetMeta::Line3D { data: d, style } => DatasetEntry::Line3D {
                data: data(gx, d, "chart line3d data").await?,
                style,
            },
            DatasetMeta::Surface { data: d, style } => DatasetEntry::Surface {
                data: data(gx, d, "chart surface data").await?,
                style,
            },
        });
    }
    Ok(entries)
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum ChartMode {
    Numeric,
    TimeSeries,
    Bar,
    Pie,
    ThreeD,
    Empty,
}

impl ChartMode {
    /// The one mode every dataset with data asks for: `Empty` when none
    /// has data, and `Err` naming two that disagree.
    pub fn of<X: GXExt>(
        datasets: &[DatasetEntry<X>],
    ) -> std::result::Result<ChartMode, (ChartMode, ChartMode)> {
        let mut modes = datasets.iter().filter_map(|ds| ds.mode());
        let Some(first) = modes.next() else { return Ok(ChartMode::Empty) };
        match modes.find(|m| *m != first) {
            None => Ok(first),
            Some(other) => Err((first, other)),
        }
    }
}

/// The bar chart's category slots: every bar series' categories in
/// first-seen order, each once.
pub fn bar_categories<X: GXExt>(datasets: &[DatasetEntry<X>]) -> LPooled<Vec<String>> {
    let mut cats: LPooled<Vec<String>> = LPooled::take();
    for ds in datasets {
        if let DatasetEntry::Bar { data, .. } = ds
            && let Some(bd) = data.t.as_ref()
        {
            for (cat, _) in bd.0.iter() {
                if !cats.contains(cat) {
                    cats.push(cat.clone());
                }
            }
        }
    }
    cats
}

/// A bar series' height at `cat`: the sum of its entries there, as the
/// histogram draws it; `None` where it has none.
pub fn bar_value(bd: &BarData, cat: &str) -> Option<f64> {
    bd.0.iter().filter(|(c, _)| c == cat).map(|(_, v)| *v).reduce(|a, b| a + b)
}

/// The slices a pie draws: finite and positive.
pub fn pie_slices(bd: &BarData) -> impl Iterator<Item = (&str, f64)> {
    bd.0.iter().filter(|(_, v)| v.is_finite() && *v > 0.0).map(|(l, v)| (l.as_str(), *v))
}

/// A pie's start angle in degrees, in [0, 360); a non-finite one is 0.
pub fn pie_start_angle(style: &PieStyleV) -> f64 {
    style.start_angle.filter(|a| a.is_finite()).map_or(0.0, |a| a.rem_euclid(360.0))
}
