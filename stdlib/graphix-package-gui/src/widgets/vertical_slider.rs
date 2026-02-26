use super::{GuiW, GuiWidget, IcedElement, Message};
use crate::types::LengthV;
use anyhow::{Context, Result};
use arcstr::ArcStr;
use graphix_compiler::expr::ExprId;
use graphix_rt::{Callable, GXExt, GXHandle, Ref, TRef};
use iced_widget as widget;
use netidx::{protocol::valarray::ValArray, publisher::Value};
use tokio::try_join;

pub(crate) struct VerticalSliderW<X: GXExt> {
    gx: GXHandle<X>,
    value: TRef<X, f64>,
    min: TRef<X, f64>,
    max: TRef<X, f64>,
    step: TRef<X, Option<f64>>,
    on_change: Ref<X>,
    on_change_callable: Option<Callable<X>>,
    on_release: Ref<X>,
    on_release_callable: Option<Callable<X>>,
    width: TRef<X, Option<f64>>,
    height: TRef<X, LengthV>,
}

impl<X: GXExt> VerticalSliderW<X> {
    pub(crate) async fn compile(gx: GXHandle<X>, source: Value) -> Result<GuiW<X>> {
        let [(_, height), (_, max), (_, min), (_, on_change), (_, on_release), (_, step), (_, value), (_, width)] =
            source.cast_to::<[(ArcStr, u64); 8]>().context("vertical_slider flds")?;
        let (height, max, min, on_change, on_release, step, value, width) = try_join! {
            gx.compile_ref(height),
            gx.compile_ref(max),
            gx.compile_ref(min),
            gx.compile_ref(on_change),
            gx.compile_ref(on_release),
            gx.compile_ref(step),
            gx.compile_ref(value),
            gx.compile_ref(width),
        }?;
        let on_change_callable = match on_change.last.as_ref() {
            Some(v) => Some(
                gx.compile_callable(v.clone())
                    .await
                    .context("vertical_slider on_change callable")?,
            ),
            None => None,
        };
        let on_release_callable = match on_release.last.as_ref() {
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
            min: TRef::new(min).context("vertical_slider tref min")?,
            max: TRef::new(max).context("vertical_slider tref max")?,
            step: TRef::new(step).context("vertical_slider tref step")?,
            on_change,
            on_change_callable,
            on_release,
            on_release_callable,
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
        if id == self.on_change.id {
            self.on_change.last = Some(v.clone());
            self.on_change_callable = Some(
                rt.block_on(self.gx.compile_callable(v.clone()))
                    .context("vslider on_change callable recompile")?,
            );
        }
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
        let on_change_id = self.on_change_callable.as_ref().map(|c| c.id());
        let mut sl =
            widget::VerticalSlider::new(range, val, move |v| match on_change_id {
                Some(id) => {
                    Message::Call(id, ValArray::from_iter([Value::F64(v as f64)]))
                }
                None => Message::Nop,
            });
        if let Some(Some(step)) = self.step.t {
            sl = sl.step(step as f32);
        }
        if let Some(callable) = &self.on_release_callable {
            sl = sl.on_release(Message::Call(
                callable.id(),
                ValArray::from_iter([Value::Null]),
            ));
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
