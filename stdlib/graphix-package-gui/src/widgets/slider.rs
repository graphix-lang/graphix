use super::{GuiW, GuiWidget, Handler, IcedElement, Message};
use crate::types::LengthV;
use anyhow::{Context, Result};
use futures::try_join;
use graphix_compiler::expr::ExprId;
use graphix_rt::{GXExt, GXHandle};
use iced_widget as widget;
use netidx::{protocol::valarray::ValArray, publisher::Value};

/// Helper: map a dimension kind tag to its TRef inner type.
macro_rules! slider_dim_type {
    (length) => { LengthV };
    (scalar) => { Option<f64> };
}

/// Helper: apply a dimension value to the iced slider widget.
macro_rules! slider_dim_set {
    (length, $self:ident, $sl:ident, $dim:ident) => {
        if let Some(v) = $self.p.$dim.t.as_ref() {
            $sl = $sl.$dim(v.0);
        }
    };
    (scalar, $self:ident, $sl:ident, $dim:ident) => {
        if let Some(Some(v)) = $self.p.$dim.t {
            $sl = $sl.$dim(v as f32);
        }
    };
}

/// Generate a horizontal or vertical slider widget. Dims are passed in
/// alphabetical order with a kind tag: `length` (primary axis) or
/// `scalar` (cross axis).
macro_rules! slider_widget {
    ($name:ident, $props:ident, $label:literal, $Widget:ident,
     $dim1:ident: $kind1:tt, $dim2:ident: $kind2:tt) => {
        graphix_rt::props! {
            struct $props {
                disabled: bool,
                max: f64,
                min: f64,
                step: Option<f64>,
                value: f64,
                $dim1: slider_dim_type!($kind1),
                $dim2: slider_dim_type!($kind2),
            }
        }

        pub(crate) struct $name<X: GXExt> {
            gx: GXHandle<X>,
            p: $props<X>,
            on_change: Handler<X>,
            on_release: Handler<X>,
        }

        impl<X: GXExt> $name<X> {
            pub(crate) async fn compile(
                gx: GXHandle<X>,
                source: Value,
            ) -> Result<GuiW<X>> {
                let (p, on_change, on_release) = try_join!(
                    $props::compile(&gx, &source),
                    Handler::field(&gx, &source, "on_change"),
                    Handler::field(&gx, &source, "on_release"),
                )
                .context($label)?;
                Ok(Box::new(Self { gx, p, on_change, on_release }))
            }
        }

        impl<X: GXExt> GuiWidget<X> for $name<X> {
            fn handle_update(
                &mut self,
                rt: &tokio::runtime::Handle,
                id: ExprId,
                v: &Value,
            ) -> Result<bool> {
                let changed = self.p.update(id, v).context($label)?;
                self.on_change
                    .update(rt, &self.gx, id, v)
                    .context(concat!($label, " on_change"))?;
                self.on_release
                    .update(rt, &self.gx, id, v)
                    .context(concat!($label, " on_release"))?;
                Ok(changed)
            }

            fn view(&self) -> IcedElement<'_> {
                let val = self.p.value.t.unwrap_or(0.0);
                let min = self.p.min.t.unwrap_or(0.0);
                let max = self.p.max.t.unwrap_or(100.0);
                let disabled = self.p.disabled.t.unwrap_or(false);
                let on_change_id = if disabled { None } else { self.on_change.id() };
                let mut sl =
                    widget::$Widget::new(min..=max, val, move |v| match on_change_id {
                        Some(id) => {
                            Message::Call(id, ValArray::from_iter([Value::F64(v)]))
                        }
                        None => Message::Nop,
                    });
                // no usable step is continuous: a millionth of the range
                let step = match self.p.step.t {
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

slider_widget!(SliderW, SliderProps, "slider", Slider, height: scalar, width: length);
slider_widget!(VerticalSliderW, VerticalSliderProps, "vslider", VerticalSlider, height: length, width: scalar);
