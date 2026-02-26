use super::{GuiW, GuiWidget, IcedElement, Message};
use crate::types::LengthV;
use anyhow::{Context, Result};
use arcstr::ArcStr;
use graphix_compiler::{expr::ExprId, BindId};
use graphix_rt::{Callable, GXExt, GXHandle, Ref, TRef};
use iced_widget as widget;
use netidx::publisher::Value;
use tokio::try_join;

pub(crate) struct VerticalSliderW<X: GXExt> {
    gx: GXHandle<X>,
    value: TRef<X, f64>,
    target_bid: Option<BindId>,
    min: TRef<X, f64>,
    max: TRef<X, f64>,
    step: TRef<X, Option<f64>>,
    on_release: Ref<X>,
    on_release_callable: Option<Callable<X>>,
    width: TRef<X, Option<f64>>,
    height: TRef<X, LengthV>,
}

impl<X: GXExt> VerticalSliderW<X> {
    pub(crate) async fn compile(gx: GXHandle<X>, source: Value) -> Result<GuiW<X>> {
        let [(_, height), (_, max), (_, min), (_, on_release), (_, step), (_, value), (_, width)] =
            source.cast_to::<[(ArcStr, u64); 7]>().context("vertical_slider flds")?;
        let (height, max, min, on_release, step, value, width) = try_join! {
            gx.compile_ref(height),
            gx.compile_ref(max),
            gx.compile_ref(min),
            gx.compile_ref(on_release),
            gx.compile_ref(step),
            gx.compile_ref(value),
            gx.compile_ref(width),
        }?;
        let target_bid = value.target_bid;
        let callable = match on_release.last.as_ref() {
            Some(v) => Some(
                gx.compile_callable(v.clone())
                    .await
                    .context("vertical_slider on_release callable")?,
            ),
            None => None,
        };
        Ok(Box::new(Self {
            gx: gx.clone(),
            value: TRef::new(value).context("vertical_slider tref value")?,
            target_bid,
            min: TRef::new(min).context("vertical_slider tref min")?,
            max: TRef::new(max).context("vertical_slider tref max")?,
            step: TRef::new(step).context("vertical_slider tref step")?,
            on_release,
            on_release_callable: callable,
            width: TRef::new(width).context("vertical_slider tref width")?,
            height: TRef::new(height).context("vertical_slider tref height")?,
        }))
    }
}

impl<X: GXExt> GuiWidget<X> for VerticalSliderW<X> {
    fn handle_update(
        &mut self,
        rt: &tokio::runtime::Handle,
        id: ExprId,
        v: &Value,
    ) -> Result<bool> {
        let mut changed = false;
        changed |= self.value.update(id, v).context("vslider update value")?.is_some();
        changed |= self.min.update(id, v).context("vslider update min")?.is_some();
        changed |= self.max.update(id, v).context("vslider update max")?.is_some();
        changed |= self.step.update(id, v).context("vslider update step")?.is_some();
        changed |= self.width.update(id, v).context("vslider update width")?.is_some();
        changed |= self.height.update(id, v).context("vslider update height")?.is_some();
        if id == self.on_release.id {
            self.on_release.last = Some(v.clone());
            self.on_release_callable = Some(
                rt.block_on(self.gx.compile_callable(v.clone()))
                    .context("vslider on_release callable recompile")?,
            );
        }
        Ok(changed)
    }

    fn view(&self) -> IcedElement<'_> {
        let val = self.value.t.unwrap_or(0.0) as f32;
        let min = self.min.t.unwrap_or(0.0) as f32;
        let max = self.max.t.unwrap_or(100.0) as f32;
        let range = min..=max;
        let mut sl = widget::VerticalSlider::new(range, val, |v| {
            match self.target_bid {
                Some(bid) => Message::Set(bid, Value::F64(v as f64)),
                None => Message::Nop,
            }
        });
        if let Some(Some(step)) = self.step.t {
            sl = sl.step(step as f32);
        }
        if let Some(callable) = &self.on_release_callable {
            sl = sl.on_release(Message::Call(callable.id()));
        }
        if let Some(Some(w)) = self.width.t {
            sl = sl.width(w as f32);
        }
        if let Some(h) = self.height.t.as_ref() {
            sl = sl.height(h.0);
        }
        sl.into()
    }
}
