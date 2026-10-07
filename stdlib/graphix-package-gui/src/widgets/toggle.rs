use super::{
    Echoes, GuiW, GuiWidget, Handler, IcedElement, Message, MessageShell, call_arg,
};
use crate::types::{LengthV, TextSizeV};
use anyhow::{Context, Result};
use arcstr::ArcStr;
use graphix_compiler::expr::ExprId;
use graphix_rt::{GXExt, GXHandle, TRef};
use iced_widget as widget;
use netidx::{protocol::valarray::ValArray, publisher::Value};
use netidx_derive::FromValue;
use tokio::try_join;

/// Generate the struct, compile(), and handle_update helper for a boolean
/// toggle widget; `$state` names the boolean field.
macro_rules! toggle_widget {
    ($name:ident, $label:literal, $state:ident) => {
        pub(crate) struct $name<X: GXExt> {
            gx: GXHandle<X>,
            disabled: TRef<X, bool>,
            $state: TRef<X, bool>,
            label: TRef<X, ArcStr>,
            on_toggle: Handler<X>,
            /// What on_toggle sent that the state has not echoed yet.
            echoes: Echoes<bool>,
            width: TRef<X, LengthV>,
            size: TRef<X, Option<TextSizeV>>,
            spacing: TRef<X, Option<f64>>,
        }

        impl<X: GXExt> $name<X> {
            pub(crate) async fn compile(gx: GXHandle<X>, source: Value) -> Result<GuiW<X>> {
                #[derive(FromValue)]
                struct Fields {
                    disabled: u64,
                    $state: u64,
                    label: u64,
                    on_toggle: u64,
                    size: u64,
                    spacing: u64,
                    width: u64,
                }
                let Fields { disabled, $state, label, on_toggle, size, spacing, width } =
                    source.cast_to().context(concat!($label, " flds"))?;
                let (disabled, $state, label, on_toggle, size, spacing, width) = try_join! {
                    gx.compile_ref(disabled),
                    gx.compile_ref($state),
                    gx.compile_ref(label),
                    gx.compile_ref(on_toggle),
                    gx.compile_ref(size),
                    gx.compile_ref(spacing),
                    gx.compile_ref(width),
                }?;
                let on_toggle = Handler::compile(&gx, on_toggle)
                    .await
                    .context(concat!($label, " on_toggle"))?;
                Ok(Box::new(Self {
                    gx: gx.clone(),
                    disabled: TRef::new(disabled).context(concat!($label, " tref disabled"))?,
                    $state: TRef::new($state).context(concat!($label, " tref ", stringify!($state)))?,
                    label: TRef::new(label).context(concat!($label, " tref label"))?,
                    on_toggle,
                    echoes: Echoes::new(),
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
                if let Some(s) = self.$state.update(id, v).context(concat!($label, " update ", stringify!($state)))? {
                    self.echoes.delivered(s);
                    changed = true;
                }
                changed |= self.label.update(id, v).context(concat!($label, " update label"))?.is_some();
                changed |= self.width.update(id, v).context(concat!($label, " update width"))?.is_some();
                changed |= self.size.update(id, v).context(concat!($label, " update size"))?.is_some();
                changed |= self.spacing.update(id, v).context(concat!($label, " update spacing"))?.is_some();
                self.on_toggle.update(rt, &self.gx, id, v).context(concat!($label, " on_toggle"))?;
                Ok(changed)
            }

            /// The state shown: the newest toggle not echoed yet, else the
            /// program's.
            fn shown(&self) -> bool {
                *self.echoes.shown(self.$state.t.as_ref()).unwrap_or(&false)
            }

            fn note_toggle(&mut self, msg: &Message) -> bool {
                match call_arg(msg, self.on_toggle.id()) {
                    Some(Value::Bool(b)) => {
                        self.echoes.sent(*b);
                        true
                    }
                    _ => false,
                }
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

    fn on_message(&mut self, msg: &Message, _shell: &mut MessageShell) -> bool {
        self.note_toggle(msg)
    }

    fn view(&self) -> IcedElement<'_> {
        let label = self.label.t.as_deref().unwrap_or("");
        let checked = self.shown();
        let mut cb = widget::Checkbox::new(checked).label(label);
        if !self.disabled.t.unwrap_or(false) {
            if let Some(callable) = &self.on_toggle.f {
                let id = callable.id();
                cb = cb.on_toggle(move |b| {
                    Message::Call(id, ValArray::from_iter([Value::from(b)]))
                });
            }
        }
        if let Some(w) = self.width.t.as_ref() {
            cb = cb.width(w.0);
        }
        if let Some(Some(TextSizeV(sz))) = self.size.t {
            cb = cb.size(sz);
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

    fn on_message(&mut self, msg: &Message, _shell: &mut MessageShell) -> bool {
        self.note_toggle(msg)
    }

    fn view(&self) -> IcedElement<'_> {
        let label = self.label.t.as_deref().unwrap_or("");
        let toggled = self.shown();
        let mut tg = widget::Toggler::new(toggled);
        if !label.is_empty() {
            tg = tg.label(label);
        }
        if !self.disabled.t.unwrap_or(false) {
            if let Some(callable) = &self.on_toggle.f {
                let id = callable.id();
                tg = tg.on_toggle(move |b| {
                    Message::Call(id, ValArray::from_iter([Value::from(b)]))
                });
            }
        }
        if let Some(w) = self.width.t.as_ref() {
            tg = tg.width(w.0);
        }
        if let Some(Some(TextSizeV(sz))) = self.size.t {
            tg = tg.size(sz);
        }
        if let Some(Some(sp)) = self.spacing.t {
            tg = tg.spacing(sp as f32);
        }
        tg.into()
    }
}
