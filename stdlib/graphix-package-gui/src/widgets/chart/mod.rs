mod dataset;
mod draw;
mod interact;
mod plotters_backend;
mod ranges;
mod types;

use crate::types::LengthV;
use crate::widgets::{GuiW, GuiWidget, IcedElement};
use anyhow::{Context, Result};
use arcstr::ArcStr;
use dataset::{compile_datasets, DatasetEntry};
use graphix_compiler::expr::ExprId;
use graphix_rt::{GXExt, GXHandle, Ref, TRef};
use iced_widget::canvas as iced_canvas;
use netidx::publisher::Value;
use poolshark::local::LPooled;
use std::cell::Cell;
use tokio::try_join;
use types::*;

#[cfg(test)]
pub(crate) use ranges::pad_range;

// ── ChartW ──────────────────────────────────────────────────────────

pub(crate) struct ChartW<X: GXExt> {
    gx: GXHandle<X>,
    datasets_ref: Ref<X>,
    datasets: LPooled<Vec<DatasetEntry<X>>>,
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
    /// Set to true when data changes; draw() clears the cache and resets.
    dirty: Cell<bool>,
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
            None => LPooled::take(),
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
            dirty: Cell::new(false),
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
            self.dirty.set(true);
            changed = true;
        }
        for ds in self.datasets.iter_mut() {
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
                self.dirty.set(true);
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
                    self.dirty.set(true);
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
