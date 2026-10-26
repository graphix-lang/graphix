use super::{Echoes, GuiW, GuiWidget, Handler, IcedElement, Message, MessageShell};
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

/// Generate a boolean toggle widget; `$state` names the boolean field and
/// `$new` builds the iced widget from the state shown, `$shown`, and the
/// label, `$text`.
macro_rules! toggle_widget {
    ($name:ident, $label:literal, $state:ident, |$shown:ident, $text:ident| $new:expr) => {
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
                Ok(GuiW::new(Self { gx, p, $state, on_toggle, echoes: Echoes::new() }))
            }
        }

        impl<X: GXExt> GuiWidget<X> for $name<X> {
            fn handle_update(
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
                    .update(rt, &self.gx, id, v, &mut changed)
                    .context(concat!($label, " on_toggle"))?;
                Ok(self.echoes.update(&self.gx, id, &self.on_toggle) || changed)
            }

            fn idle(&mut self) -> bool {
                self.echoes.idle()
            }

            fn on_message(&mut self, msg: &Message, shell: &mut MessageShell) -> bool {
                self.echoes.on_call(msg, &self.on_toggle, shell, |v| match v {
                    Value::Bool(b) => Some(*b),
                    _ => None,
                })
            }

            fn view(&self) -> IcedElement<'_> {
                let $text = self.p.label.t.as_deref().unwrap_or("");
                let $shown = *self.echoes.shown(self.$state.t.as_ref()).unwrap_or(&false);
                let mut w = $new;
                if !self.p.disabled.t.unwrap_or(false) {
                    if let Some(callable) = &self.on_toggle.f {
                        let id = callable.id();
                        w = w.on_toggle(move |b| {
                            Message::Call(id, ValArray::from_iter([Value::from(b)]))
                        });
                    }
                }
                if let Some(width) = self.p.width.t.as_ref() {
                    w = w.width(width.0);
                }
                if let Some(Some(TextSizeV(sz))) = self.p.size.t {
                    w = w.size(sz);
                }
                if let Some(Some(sp)) = self.p.spacing.t {
                    w = w.spacing(sp as f32);
                }
                w.into()
            }
        }
    };
}

toggle_widget!(CheckboxW, "checkbox", is_checked, |checked, label| {
    widget::Checkbox::new(checked).label(label)
});

toggle_widget!(TogglerW, "toggler", is_toggled, |toggled, label| {
    let tg = widget::Toggler::new(toggled);
    if label.is_empty() { tg } else { tg.label(label) }
});
