use super::{
    Echoes, GuiW, GuiWidget, Handler, IcedElement, Message, MessageShell, call_arg,
};
use crate::types::{LengthV, TextSizeV};
use anyhow::{Context, Result};
use arcstr::ArcStr;
use futures::try_join;
use graphix_compiler::expr::ExprId;
use graphix_rt::{GXExt, GXHandle, TRef};
use iced_widget as widget;
use netidx::{protocol::valarray::ValArray, publisher::Value};

graphix_rt::props! {
    struct Props {
        disabled: bool,
        label: ArcStr,
        size: Option<TextSizeV>,
        spacing: Option<f64>,
        width: LengthV,
    }
}

/// Generate the struct, compile(), and handle_update helper for a boolean
/// toggle widget; `$state` names the boolean field.
macro_rules! toggle_widget {
    ($name:ident, $label:literal, $state:ident) => {
        pub(crate) struct $name<X: GXExt> {
            gx: GXHandle<X>,
            p: Props<X>,
            $state: TRef<X, bool>,
            on_toggle: Handler<X>,
            /// What on_toggle sent that the state has not echoed yet.
            echoes: Echoes<bool>,
        }

        impl<X: GXExt> $name<X> {
            pub(crate) async fn compile(
                gx: GXHandle<X>,
                source: Value,
            ) -> Result<GuiW<X>> {
                let (p, $state, on_toggle) = try_join!(
                    Props::compile(&gx, &source),
                    gx.compile_field(&source, stringify!($state)),
                    Handler::field(&gx, &source, "on_toggle"),
                )
                .context($label)?;
                let $state = TRef::new($state).context(concat!(
                    $label,
                    " ",
                    stringify!($state)
                ))?;
                Ok(Box::new(Self { gx, p, $state, on_toggle, echoes: Echoes::new() }))
            }

            fn do_update(
                &mut self,
                rt: &tokio::runtime::Handle,
                id: ExprId,
                v: &Value,
            ) -> Result<bool> {
                let mut changed = self.p.update(id, v).context($label)?;
                if let Some(s) = self.$state.update(id, v).context(concat!(
                    $label,
                    " ",
                    stringify!($state)
                ))? {
                    self.echoes.delivered(s);
                    changed = true;
                }
                self.on_toggle
                    .update(rt, &self.gx, id, v)
                    .context(concat!($label, " on_toggle"))?;
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
        let label = self.p.label.t.as_deref().unwrap_or("");
        let checked = self.shown();
        let mut cb = widget::Checkbox::new(checked).label(label);
        if !self.p.disabled.t.unwrap_or(false) {
            if let Some(callable) = &self.on_toggle.f {
                let id = callable.id();
                cb = cb.on_toggle(move |b| {
                    Message::Call(id, ValArray::from_iter([Value::from(b)]))
                });
            }
        }
        if let Some(w) = self.p.width.t.as_ref() {
            cb = cb.width(w.0);
        }
        if let Some(Some(TextSizeV(sz))) = self.p.size.t {
            cb = cb.size(sz);
        }
        if let Some(Some(sp)) = self.p.spacing.t {
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
        let label = self.p.label.t.as_deref().unwrap_or("");
        let toggled = self.shown();
        let mut tg = widget::Toggler::new(toggled);
        if !label.is_empty() {
            tg = tg.label(label);
        }
        if !self.p.disabled.t.unwrap_or(false) {
            if let Some(callable) = &self.on_toggle.f {
                let id = callable.id();
                tg = tg.on_toggle(move |b| {
                    Message::Call(id, ValArray::from_iter([Value::from(b)]))
                });
            }
        }
        if let Some(w) = self.p.width.t.as_ref() {
            tg = tg.width(w.0);
        }
        if let Some(Some(TextSizeV(sz))) = self.p.size.t {
            tg = tg.size(sz);
        }
        if let Some(Some(sp)) = self.p.spacing.t {
            tg = tg.spacing(sp as f32);
        }
        tg.into()
    }
}
