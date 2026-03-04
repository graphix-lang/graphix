use super::{GuiW, GuiWidget, IcedElement, Message};
use crate::types::LengthV;
use anyhow::{Context, Result};
use arcstr::ArcStr;
use graphix_compiler::expr::ExprId;
use graphix_rt::{Callable, GXExt, GXHandle, Ref, TRef};
use iced_widget as widget;
use netidx::{protocol::valarray::ValArray, publisher::Value};
use tokio::try_join;

/// Generate the struct, compile(), and handle_update helper for a boolean
/// toggle widget (Checkbox or Toggler). The `$state` ident is the name
/// of the boolean field (e.g. `is_checked` or `is_toggled`). The trait
/// impl with view() is written separately for each widget.
macro_rules! toggle_widget {
    ($name:ident, $label:literal, $state:ident) => {
        pub(crate) struct $name<X: GXExt> {
            gx: GXHandle<X>,
            disabled: TRef<X, bool>,
            $state: TRef<X, bool>,
            label: TRef<X, String>,
            on_toggle: Ref<X>,
            on_toggle_callable: Option<Callable<X>>,
            width: TRef<X, LengthV>,
            size: TRef<X, Option<f64>>,
            spacing: TRef<X, Option<f64>>,
        }

        impl<X: GXExt> $name<X> {
            pub(crate) async fn compile(gx: GXHandle<X>, source: Value) -> Result<GuiW<X>> {
                let [(_, disabled), (_, $state), (_, label), (_, on_toggle), (_, size), (_, spacing), (_, width)] =
                    source.cast_to::<[(ArcStr, u64); 7]>().context(concat!($label, " flds"))?;
                let (disabled, $state, label, on_toggle, size, spacing, width) = try_join! {
                    gx.compile_ref(disabled),
                    gx.compile_ref($state),
                    gx.compile_ref(label),
                    gx.compile_ref(on_toggle),
                    gx.compile_ref(size),
                    gx.compile_ref(spacing),
                    gx.compile_ref(width),
                }?;
                let callable =
                    compile_callable!(gx, on_toggle, concat!($label, " on_toggle"));
                Ok(Box::new(Self {
                    gx: gx.clone(),
                    disabled: TRef::new(disabled).context(concat!($label, " tref disabled"))?,
                    $state: TRef::new($state).context(concat!($label, " tref ", stringify!($state)))?,
                    label: TRef::new(label).context(concat!($label, " tref label"))?,
                    on_toggle,
                    on_toggle_callable: callable,
                    width: TRef::new(width).context(concat!($label, " tref width"))?,
                    size: TRef::new(size).context(concat!($label, " tref size"))?,
                    spacing: TRef::new(spacing).context(concat!($label, " tref spacing"))?,
                }))
            }

            fn do_update(
                &mut self,
                rt: &tokio::runtime::Handle,
                id: ExprId,
                v: &Value,
            ) -> Result<bool> {
                let mut changed = false;
                changed |= self.disabled.update(id, v).context(concat!($label, " update disabled"))?.is_some();
                changed |= self.$state.update(id, v).context(concat!($label, " update ", stringify!($state)))?.is_some();
                changed |= self.label.update(id, v).context(concat!($label, " update label"))?.is_some();
                changed |= self.width.update(id, v).context(concat!($label, " update width"))?.is_some();
                changed |= self.size.update(id, v).context(concat!($label, " update size"))?.is_some();
                changed |= self.spacing.update(id, v).context(concat!($label, " update spacing"))?.is_some();
                update_callable!(self, rt, id, v, on_toggle, on_toggle_callable, concat!($label, " on_toggle recompile"));
                Ok(changed)
            }
        }
    };
}

toggle_widget!(CheckboxW, "checkbox", is_checked);

impl<X: GXExt> GuiWidget<X> for CheckboxW<X> {
    fn handle_update(
        &mut self,
        rt: &tokio::runtime::Handle,
        id: ExprId,
        v: &Value,
    ) -> Result<bool> {
        self.do_update(rt, id, v)
    }

    fn view(&self) -> IcedElement<'_> {
        let label = self.label.t.as_deref().unwrap_or("");
        let checked = self.is_checked.t.unwrap_or(false);
        let mut cb = widget::Checkbox::new(checked).label(label);
        if !self.disabled.t.unwrap_or(false) {
            if let Some(callable) = &self.on_toggle_callable {
                let id = callable.id();
                cb = cb.on_toggle(move |b| {
                    Message::Call(id, ValArray::from_iter([Value::from(b)]))
                });
            }
        }
        if let Some(w) = self.width.t.as_ref() {
            cb = cb.width(w.0);
        }
        if let Some(Some(sz)) = self.size.t {
            cb = cb.size(sz as f32);
        }
        if let Some(Some(sp)) = self.spacing.t {
            cb = cb.spacing(sp as f32);
        }
        cb.into()
    }
}

toggle_widget!(TogglerW, "toggler", is_toggled);

impl<X: GXExt> GuiWidget<X> for TogglerW<X> {
    fn handle_update(
        &mut self,
        rt: &tokio::runtime::Handle,
        id: ExprId,
        v: &Value,
    ) -> Result<bool> {
        self.do_update(rt, id, v)
    }

    fn view(&self) -> IcedElement<'_> {
        let label = self.label.t.as_deref().unwrap_or("");
        let toggled = self.is_toggled.t.unwrap_or(false);
        let mut tg = widget::Toggler::new(toggled);
        if !label.is_empty() {
            tg = tg.label(label);
        }
        if !self.disabled.t.unwrap_or(false) {
            if let Some(callable) = &self.on_toggle_callable {
                let id = callable.id();
                tg = tg.on_toggle(move |b| {
                    Message::Call(id, ValArray::from_iter([Value::from(b)]))
                });
            }
        }
        if let Some(w) = self.width.t.as_ref() {
            tg = tg.width(w.0);
        }
        if let Some(Some(sz)) = self.size.t {
            tg = tg.size(sz as f32);
        }
        if let Some(Some(sp)) = self.spacing.t {
            tg = tg.spacing(sp as f32);
        }
        tg.into()
    }
}
