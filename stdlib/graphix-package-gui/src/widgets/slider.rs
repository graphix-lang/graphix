use super::{GuiW, GuiWidget, IcedElement, Message};
use crate::types::LengthV;
use anyhow::{Context, Result};
use arcstr::ArcStr;
use graphix_compiler::expr::ExprId;
use graphix_rt::{Callable, GXExt, GXHandle, Ref, TRef};
use iced_widget as widget;
use netidx::{protocol::valarray::ValArray, publisher::Value};
use tokio::try_join;

pub(crate) struct SliderW<X: GXExt> {
    gx: GXHandle<X>,
    disabled: TRef<X, bool>,
    value: TRef<X, f64>,
    min: TRef<X, f64>,
    max: TRef<X, f64>,
    step: TRef<X, Option<f64>>,
    on_change: Ref<X>,
    on_change_callable: Option<Callable<X>>,
    on_release: Ref<X>,
    on_release_callable: Option<Callable<X>>,
    width: TRef<X, LengthV>,
    height: TRef<X, Option<f64>>,
}

impl<X: GXExt> SliderW<X> {
    pub(crate) async fn compile(gx: GXHandle<X>, source: Value) -> Result<GuiW<X>> {
        let [(_, disabled), (_, height), (_, max), (_, min), (_, on_change), (_, on_release), (_, step), (_, value), (_, width)] =
            source.cast_to::<[(ArcStr, u64); 9]>().context("slider flds")?;
        let (disabled, height, max, min, on_change, on_release, step, value, width) = try_join! {
            gx.compile_ref(disabled),
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
                    .context("slider on_change callable")?,
            ),
            None => None,
        };
        let on_release_callable = match on_release.last.as_ref() {
            Some(v) => Some(
                gx.compile_callable(v.clone())
                    .await
                    .context("slider on_release callable")?,
            ),
            None => None,
        };
        Ok(Box::new(Self {
            gx: gx.clone(),
            disabled: TRef::new(disabled).context("slider tref disabled")?,
            value: TRef::new(value).context("slider tref value")?,
            min: TRef::new(min).context("slider tref min")?,
            max: TRef::new(max).context("slider tref max")?,
            step: TRef::new(step).context("slider tref step")?,
            on_change,
            on_change_callable,
            on_release,
            on_release_callable,
            width: TRef::new(width).context("slider tref width")?,
            height: TRef::new(height).context("slider tref height")?,
        }))
    }
}

impl<X: GXExt> GuiWidget<X> for SliderW<X> {
    fn handle_update(
        &mut self,
        rt: &tokio::runtime::Handle,
        id: ExprId,
        v: &Value,
    ) -> Result<bool> {
        let mut changed = false;
        changed |=
            self.disabled.update(id, v).context("slider update disabled")?.is_some();
        changed |= self.value.update(id, v).context("slider update value")?.is_some();
        changed |= self.min.update(id, v).context("slider update min")?.is_some();
        changed |= self.max.update(id, v).context("slider update max")?.is_some();
        changed |= self.step.update(id, v).context("slider update step")?.is_some();
        changed |= self.width.update(id, v).context("slider update width")?.is_some();
        changed |= self.height.update(id, v).context("slider update height")?.is_some();
        if id == self.on_change.id {
            self.on_change.last = Some(v.clone());
            self.on_change_callable = Some(
                rt.block_on(self.gx.compile_callable(v.clone()))
                    .context("slider on_change callable recompile")?,
            );
        }
        if id == self.on_release.id {
            self.on_release.last = Some(v.clone());
            self.on_release_callable = Some(
                rt.block_on(self.gx.compile_callable(v.clone()))
                    .context("slider on_release callable recompile")?,
            );
        }
        Ok(changed)
    }

    fn view(&self) -> IcedElement<'_> {
        let val = self.value.t.unwrap_or(0.0) as f32;
        let min = self.min.t.unwrap_or(0.0) as f32;
        let max = self.max.t.unwrap_or(100.0) as f32;
        let range = min..=max;
        let disabled = self.disabled.t.unwrap_or(false);
        let on_change_id = if disabled {
            None
        } else {
            self.on_change_callable.as_ref().map(|c| c.id())
        };
        let mut sl = widget::Slider::new(range, val, move |v| match on_change_id {
            Some(id) => Message::Call(id, ValArray::from_iter([Value::F64(v as f64)])),
            None => Message::Nop,
        });
        if let Some(Some(step)) = self.step.t {
            sl = sl.step(step as f32);
        }
        if !disabled {
            if let Some(callable) = &self.on_release_callable {
                sl = sl.on_release(Message::Call(
                    callable.id(),
                    ValArray::from_iter([Value::Null]),
                ));
            }
        }
        if let Some(w) = self.width.t.as_ref() {
            sl = sl.width(w.0);
        }
        if let Some(Some(h)) = self.height.t {
            sl = sl.height(h as f32);
        }
        sl.into()
    }
}
