use super::{GuiW, GuiWidget, IcedElement, Message};
use crate::types::LengthV;
use anyhow::{Context, Result};
use arcstr::ArcStr;
use graphix_compiler::expr::ExprId;
use graphix_rt::{Callable, GXExt, GXHandle, Ref, TRef};
use iced_widget as widget;
use netidx::{protocol::valarray::ValArray, publisher::Value};
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

/// Generate a horizontal or vertical slider widget. The two dims (height,
/// width) are passed in alphabetical order together with a kind tag
/// (`length` for the primary axis, `scalar` for the cross axis) that
/// controls the field type and how the value is applied to the iced widget.
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
            on_change: Ref<X>,
            on_change_callable: Option<Callable<X>>,
            on_release: Ref<X>,
            on_release_callable: Option<Callable<X>>,
            $dim1: TRef<X, slider_dim_type!($kind1)>,
            $dim2: TRef<X, slider_dim_type!($kind2)>,
        }

        impl<X: GXExt> $name<X> {
            pub(crate) async fn compile(gx: GXHandle<X>, source: Value) -> Result<GuiW<X>> {
                let [(_, disabled), (_, $dim1), (_, max), (_, min), (_, on_change), (_, on_release), (_, step), (_, value), (_, $dim2)] =
                    source.cast_to::<[(ArcStr, u64); 9]>().context(concat!($label, " flds"))?;
                let (disabled, $dim1, max, min, on_change, on_release, step, value, $dim2) = try_join! {
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
                let on_change_callable =
                    compile_callable!(gx, on_change, concat!($label, " on_change"));
                let on_release_callable =
                    compile_callable!(gx, on_release, concat!($label, " on_release"));
                Ok(Box::new(Self {
                    gx: gx.clone(),
                    disabled: TRef::new(disabled).context(concat!($label, " tref disabled"))?,
                    value: TRef::new(value).context(concat!($label, " tref value"))?,
                    min: TRef::new(min).context(concat!($label, " tref min"))?,
                    max: TRef::new(max).context(concat!($label, " tref max"))?,
                    step: TRef::new(step).context(concat!($label, " tref step"))?,
                    on_change,
                    on_change_callable,
                    on_release,
                    on_release_callable,
                    $dim1: TRef::new($dim1).context(concat!($label, " tref ", stringify!($dim1)))?,
                    $dim2: TRef::new($dim2).context(concat!($label, " tref ", stringify!($dim2)))?,
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
                changed |= self.disabled.update(id, v).context(concat!($label, " update disabled"))?.is_some();
                changed |= self.value.update(id, v).context(concat!($label, " update value"))?.is_some();
                changed |= self.min.update(id, v).context(concat!($label, " update min"))?.is_some();
                changed |= self.max.update(id, v).context(concat!($label, " update max"))?.is_some();
                changed |= self.step.update(id, v).context(concat!($label, " update step"))?.is_some();
                changed |= self.$dim1.update(id, v).context(concat!($label, " update ", stringify!($dim1)))?.is_some();
                changed |= self.$dim2.update(id, v).context(concat!($label, " update ", stringify!($dim2)))?.is_some();
                update_callable!(self, rt, id, v, on_change, on_change_callable, concat!($label, " on_change recompile"));
                update_callable!(self, rt, id, v, on_release, on_release_callable, concat!($label, " on_release recompile"));
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
                let mut sl = widget::$Widget::new(range, val, move |v| match on_change_id {
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
                slider_dim_set!($kind1, self, sl, $dim1);
                slider_dim_set!($kind2, self, sl, $dim2);
                sl.into()
            }
        }
    };
}

// Slider: primary axis is width (LengthV), cross axis is height (scalar).
// Dims listed in alphabetical order: height first, width second.
slider_widget!(SliderW, "slider", Slider, height: scalar, width: length);
// VerticalSlider: primary axis is height (LengthV), cross axis is width (scalar).
slider_widget!(VerticalSliderW, "vslider", VerticalSlider, height: length, width: scalar);
