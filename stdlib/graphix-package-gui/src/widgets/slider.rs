use super::{GuiW, GuiWidget, Handler, IcedElement, Message};
use crate::types::LengthV;
use anyhow::{Context, Result};
use graphix_compiler::expr::ExprId;
use graphix_rt::{GXExt, GXHandle, TRef};
use iced_widget as widget;
use netidx::{protocol::valarray::ValArray, publisher::Value};
use netidx_derive::FromValue;
use tokio::try_join;

/// Helper: map a dimension kind tag to its TRef inner type.
macro_rules! slider_dim_type {
    (length) => { LengthV };
    (scalar) => { Option<f64> };
}

/// Helper: apply a dimension value to the iced slider widget.
macro_rules! slider_dim_set {
    (length, $self:ident, $sl:ident, $dim:ident) => {
        if let Some(v) = $self.$dim.t.as_ref() {
            $sl = $sl.$dim(v.0);
        }
    };
    (scalar, $self:ident, $sl:ident, $dim:ident) => {
        if let Some(Some(v)) = $self.$dim.t {
            $sl = $sl.$dim(v as f32);
        }
    };
}

/// Generate a horizontal or vertical slider widget. Dims are passed in
/// alphabetical order with a kind tag: `length` (primary axis) or
/// `scalar` (cross axis).
macro_rules! slider_widget {
    ($name:ident, $label:literal, $Widget:ident,
     $dim1:ident: $kind1:tt, $dim2:ident: $kind2:tt) => {
        pub(crate) struct $name<X: GXExt> {
            gx: GXHandle<X>,
            disabled: TRef<X, bool>,
            value: TRef<X, f64>,
            min: TRef<X, f64>,
            max: TRef<X, f64>,
            step: TRef<X, Option<f64>>,
            on_change: Handler<X>,
            on_release: Handler<X>,
            $dim1: TRef<X, slider_dim_type!($kind1)>,
            $dim2: TRef<X, slider_dim_type!($kind2)>,
        }

        impl<X: GXExt> $name<X> {
            pub(crate) async fn compile(
                gx: GXHandle<X>,
                source: Value,
            ) -> Result<GuiW<X>> {
                #[derive(FromValue)]
                struct Fields {
                    disabled: u64,
                    $dim1: u64,
                    max: u64,
                    min: u64,
                    on_change: u64,
                    on_release: u64,
                    step: u64,
                    value: u64,
                    $dim2: u64,
                }
                let Fields {
                    disabled,
                    $dim1,
                    max,
                    min,
                    on_change,
                    on_release,
                    step,
                    value,
                    $dim2,
                } = source.cast_to().context(concat!($label, " flds"))?;
                let (
                    disabled,
                    $dim1,
                    max,
                    min,
                    on_change,
                    on_release,
                    step,
                    value,
                    $dim2,
                ) = try_join! {
                    gx.compile_ref(disabled),
                    gx.compile_ref($dim1),
                    gx.compile_ref(max),
                    gx.compile_ref(min),
                    gx.compile_ref(on_change),
                    gx.compile_ref(on_release),
                    gx.compile_ref(step),
                    gx.compile_ref(value),
                    gx.compile_ref($dim2),
                }?;
                let on_change = Handler::compile(&gx, on_change)
                    .await
                    .context(concat!($label, " on_change"))?;
                let on_release = Handler::compile(&gx, on_release)
                    .await
                    .context(concat!($label, " on_release"))?;
                Ok(Box::new(Self {
                    gx: gx.clone(),
                    disabled: TRef::new(disabled)
                        .context(concat!($label, " tref disabled"))?,
                    value: TRef::new(value).context(concat!($label, " tref value"))?,
                    min: TRef::new(min).context(concat!($label, " tref min"))?,
                    max: TRef::new(max).context(concat!($label, " tref max"))?,
                    step: TRef::new(step).context(concat!($label, " tref step"))?,
                    on_change,
                    on_release,
                    $dim1: TRef::new($dim1).context(concat!(
                        $label,
                        " tref ",
                        stringify!($dim1)
                    ))?,
                    $dim2: TRef::new($dim2).context(concat!(
                        $label,
                        " tref ",
                        stringify!($dim2)
                    ))?,
                }))
            }
        }

        impl<X: GXExt> GuiWidget<X> for $name<X> {
            fn handle_update(
                &mut self,
                rt: &tokio::runtime::Handle,
                id: ExprId,
                v: &Value,
            ) -> Result<bool> {
                let mut changed = false;
                changed |= self
                    .disabled
                    .update(id, v)
                    .context(concat!($label, " update disabled"))?
                    .is_some();
                changed |= self
                    .value
                    .update(id, v)
                    .context(concat!($label, " update value"))?
                    .is_some();
                changed |= self
                    .min
                    .update(id, v)
                    .context(concat!($label, " update min"))?
                    .is_some();
                changed |= self
                    .max
                    .update(id, v)
                    .context(concat!($label, " update max"))?
                    .is_some();
                changed |= self
                    .step
                    .update(id, v)
                    .context(concat!($label, " update step"))?
                    .is_some();
                changed |= self
                    .$dim1
                    .update(id, v)
                    .context(concat!($label, " update ", stringify!($dim1)))?
                    .is_some();
                changed |= self
                    .$dim2
                    .update(id, v)
                    .context(concat!($label, " update ", stringify!($dim2)))?
                    .is_some();
                self.on_change
                    .update(rt, &self.gx, id, v)
                    .context(concat!($label, " on_change"))?;
                self.on_release
                    .update(rt, &self.gx, id, v)
                    .context(concat!($label, " on_release"))?;
                Ok(changed)
            }

            fn view(&self) -> IcedElement<'_> {
                let val = self.value.t.unwrap_or(0.0);
                let min = self.min.t.unwrap_or(0.0);
                let max = self.max.t.unwrap_or(100.0);
                let disabled = self.disabled.t.unwrap_or(false);
                let on_change_id = if disabled { None } else { self.on_change.id() };
                let mut sl =
                    widget::$Widget::new(min..=max, val, move |v| match on_change_id {
                        Some(id) => {
                            Message::Call(id, ValArray::from_iter([Value::F64(v)]))
                        }
                        None => Message::Nop,
                    });
                // no usable step is continuous: a millionth of the range
                let step = match self.step.t {
                    Some(Some(s)) if s.is_finite() && s > 0.0 => s,
                    _ => ((max - min) / 1e6).abs().max(f64::MIN_POSITIVE),
                };
                sl = sl.step(step);
                if let Some(id) = self.on_release.id().filter(|_| !disabled) {
                    sl = sl.on_release(Message::Call(
                        id,
                        ValArray::from_iter([Value::Null]),
                    ));
                }
                slider_dim_set!($kind1, self, sl, $dim1);
                slider_dim_set!($kind2, self, sl, $dim2);
                sl.into()
            }
        }
    };
}

slider_widget!(SliderW, "slider", Slider, height: scalar, width: length);
slider_widget!(VerticalSliderW, "vslider", VerticalSlider, height: length, width: scalar);
