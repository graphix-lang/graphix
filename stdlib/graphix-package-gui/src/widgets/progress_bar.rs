use super::{GuiW, GuiWidget, IcedElement};
use crate::types::LengthV;
use anyhow::{Context, Result};
use arcstr::ArcStr;
use graphix_compiler::expr::ExprId;
use graphix_rt::{GXExt, GXHandle, TRef};
use iced_widget as widget;
use netidx::publisher::Value;
use tokio::try_join;

pub(crate) struct ProgressBarW<X: GXExt> {
    value: TRef<X, f64>,
    min: TRef<X, f64>,
    max: TRef<X, f64>,
    width: TRef<X, LengthV>,
    height: TRef<X, Option<f64>>,
}

impl<X: GXExt> ProgressBarW<X> {
    pub(crate) async fn compile(gx: GXHandle<X>, source: Value) -> Result<GuiW<X>> {
        let [(_, height), (_, max), (_, min), (_, value), (_, width)] =
            source.cast_to::<[(ArcStr, u64); 5]>().context("progress_bar flds")?;
        let (height, max, min, value, width) = try_join! {
            gx.compile_ref(height),
            gx.compile_ref(max),
            gx.compile_ref(min),
            gx.compile_ref(value),
            gx.compile_ref(width),
        }?;
        Ok(Box::new(Self {
            value: TRef::new(value).context("progress_bar tref value")?,
            min: TRef::new(min).context("progress_bar tref min")?,
            max: TRef::new(max).context("progress_bar tref max")?,
            width: TRef::new(width).context("progress_bar tref width")?,
            height: TRef::new(height).context("progress_bar tref height")?,
        }))
    }
}

impl<X: GXExt> GuiWidget<X> for ProgressBarW<X> {
    fn handle_update(
        &mut self,
        _rt: &tokio::runtime::Handle,
        id: ExprId,
        v: &Value,
    ) -> Result<bool> {
        let mut changed = false;
        changed |=
            self.value.update(id, v).context("progress_bar update value")?.is_some();
        changed |= self.min.update(id, v).context("progress_bar update min")?.is_some();
        changed |= self.max.update(id, v).context("progress_bar update max")?.is_some();
        changed |=
            self.width.update(id, v).context("progress_bar update width")?.is_some();
        changed |=
            self.height.update(id, v).context("progress_bar update height")?.is_some();
        Ok(changed)
    }

    fn view(&self) -> IcedElement<'_> {
        let val = self.value.t.unwrap_or(0.0) as f32;
        let min = self.min.t.unwrap_or(0.0) as f32;
        let max = self.max.t.unwrap_or(100.0) as f32;
        let mut pb = widget::ProgressBar::new(min..=max, val);
        if let Some(w) = self.width.t.as_ref() {
            pb = pb.length(w.0);
        }
        if let Some(Some(h)) = self.height.t {
            pb = pb.girth(h as f32);
        }
        pb.into()
    }
}
