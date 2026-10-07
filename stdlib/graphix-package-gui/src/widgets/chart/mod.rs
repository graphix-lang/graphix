mod clip;
pub mod dataset;
mod draw;
pub mod interact;
mod plotters_backend;
pub mod ranges;
pub mod types;

use crate::{
    types::LengthV,
    widgets::{ChartId, GuiW, GuiWidget, IcedElement},
};
use anyhow::{Context, Result};
use arcstr::ArcStr;
use futures::try_join;
use graphix_compiler::expr::ExprId;
use graphix_rt::{GXExt, GXHandle, Ref};
use iced_widget::canvas as iced_canvas;
use log::error;
use netidx::publisher::Value;
use poolshark::local::LPooled;
use std::cell::Cell;

pub use dataset::*;
#[cfg(test)]
pub(crate) use draw::marker_size;
pub use interact::{ChartState, PlotInfo, SnapPoint};
pub use ranges::*;
pub use types::*;

graphix_rt::props! {
    struct Drawn {
        projection: OptProjection3D,
        style: OptChartStyle,
        title: Option<ArcStr>,
        x_label: Option<ArcStr>,
        x_range: OptXAxisRange,
        y_label: Option<ArcStr>,
        y_range: OptAxisRange,
        z_label: Option<ArcStr>,
        z_range: OptAxisRange,
    }
}

graphix_rt::props! {
    struct Size {
        height: LengthV,
        width: LengthV,
    }
}

pub(crate) struct ChartW<X: GXExt> {
    gx: GXHandle<X>,
    datasets_ref: Ref<X>,
    datasets: LPooled<Vec<DatasetEntry<X>>>,
    /// What the plot is drawn from: a change redraws it.
    drawn: Drawn<X>,
    size: Size<X>,
    /// Which chart a `ChartState`'s view was taken on.
    id: ChartId,
    /// The mode the data asks for, `Empty` when datasets disagree.
    mode: ChartMode,
    /// The disagreement last reported, so it is reported once.
    conflict: Option<(ChartMode, ChartMode)>,
    /// Set to true when data changes; draw() clears the cache and resets.
    dirty: Cell<bool>,
}

impl<X: GXExt> ChartW<X> {
    pub(crate) async fn compile(gx: GXHandle<X>, source: Value) -> Result<GuiW<X>> {
        let (datasets_ref, drawn, size) = try_join!(
            gx.compile_field(&source, "datasets"),
            Drawn::compile(&gx, &source),
            Size::compile(&gx, &source),
        )
        .context("chart")?;
        let entries = match datasets_ref.last.as_ref() {
            Some(v) => compile_datasets(&gx, v.clone()).await?,
            None => LPooled::take(),
        };
        let mut chart = Self {
            gx: gx.clone(),
            datasets_ref,
            datasets: entries,
            drawn,
            size,
            id: ChartId::new(),
            mode: ChartMode::Empty,
            conflict: None,
            dirty: Cell::new(true),
        };
        chart.refresh_mode();
        Ok(Box::new(chart))
    }

    #[cfg(test)]
    pub(crate) fn datasets(&self) -> &[DatasetEntry<X>] {
        &self.datasets
    }

    #[cfg(test)]
    pub(crate) fn mode(&self) -> ChartMode {
        self.mode
    }

    /// Recompute the mode from the data, reporting a new disagreement.
    fn refresh_mode(&mut self) {
        match ChartMode::of(&self.datasets) {
            Ok(mode) => {
                self.mode = mode;
                self.conflict = None;
            }
            Err(pair) => {
                if self.conflict != Some(pair) {
                    error!("chart: cannot mix {:?} and {:?} datasets", pair.0, pair.1);
                }
                self.mode = ChartMode::Empty;
                self.conflict = Some(pair);
            }
        }
    }
}

impl<X: GXExt> GuiWidget<X> for ChartW<X> {
    #[cfg(test)]
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }

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
            self.dirty.set(true);
            changed = true;
        }
        for ds in self.datasets.iter_mut() {
            let updated = match ds {
                DatasetEntry::XY { data, .. } => data.update(id, v)?.is_some(),
                DatasetEntry::Bar { data, .. } | DatasetEntry::Pie { data, .. } => {
                    data.update(id, v)?.is_some()
                }
                DatasetEntry::Candlestick { data, .. } => data.update(id, v)?.is_some(),
                DatasetEntry::ErrorBar { data, .. } => data.update(id, v)?.is_some(),
                DatasetEntry::Scatter3D { data, .. }
                | DatasetEntry::Line3D { data, .. } => data.update(id, v)?.is_some(),
                DatasetEntry::Surface { data, .. } => data.update(id, v)?.is_some(),
            };
            if updated {
                self.dirty.set(true);
                changed = true;
            }
        }
        if changed {
            self.refresh_mode();
        }
        if self.drawn.update(id, v).context("chart")? {
            self.dirty.set(true);
            changed = true;
        }
        changed |= self.size.update(id, v).context("chart size")?;
        Ok(changed)
    }

    fn view(&self) -> IcedElement<'_> {
        let mut c = iced_canvas::Canvas::new(self);
        if let Some(w) = self.size.width.t.as_ref() {
            c = c.width(w.0);
        }
        if let Some(h) = self.size.height.t.as_ref() {
            c = c.height(h.0);
        }
        c.into()
    }
}
