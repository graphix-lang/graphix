use super::{GuiW, GuiWidget, IcedElement};
use crate::types::LengthV;
use anyhow::{Context, Result};
use graphix_compiler::expr::ExprId;
use graphix_rt::{GXExt, GXHandle};
use iced_widget as widget;
use netidx::publisher::Value;

graphix_rt::props! {
    struct Props {
        height: Option<f64>,
        max: f64,
        min: f64,
        value: f64,
        width: LengthV,
    }
}

pub(crate) struct ProgressBarW<X: GXExt>(Props<X>);

impl<X: GXExt> ProgressBarW<X> {
    pub(crate) async fn compile(gx: GXHandle<X>, source: Value) -> Result<GuiW<X>> {
        Ok(Box::new(Self(Props::compile(&gx, &source).await.context("progress_bar")?)))
    }
}

impl<X: GXExt> GuiWidget<X> for ProgressBarW<X> {
    fn handle_update(
        &mut self,
        _rt: &tokio::runtime::Handle,
        id: ExprId,
        v: &Value,
    ) -> Result<bool> {
        self.0.update(id, v).context("progress_bar")
    }

    fn view(&self) -> IcedElement<'_> {
        let p = &self.0;
        let val = p.value.t.unwrap_or(0.0) as f32;
        let min = p.min.t.unwrap_or(0.0) as f32;
        let max = p.max.t.unwrap_or(100.0) as f32;
        // iced clamps with f32::clamp, which panics on an empty or NaN range
        let (min, max, val) = match min <= max {
            true => (min, max, val),
            false => (0.0, 1.0, 0.0),
        };
        let mut pb = widget::ProgressBar::new(min..=max, val);
        if let Some(w) = p.width.t.as_ref() {
            pb = pb.length(w.0);
        }
        if let Some(Some(h)) = p.height.t {
            pb = pb.girth(h as f32);
        }
        pb.into()
    }
}
