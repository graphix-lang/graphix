use super::{
    Echoes, GuiW, GuiWidget, Handler, IcedElement, Message, MessageShell, call_arg,
};
use crate::types::{FontV, LengthV, PaddingV, TextSizeV};
use anyhow::{Context, Result};
use arcstr::ArcStr;
use futures::try_join;
use graphix_compiler::expr::ExprId;
use graphix_rt::{GXExt, GXHandle};
use iced_widget as widget;
use netidx::{protocol::valarray::ValArray, publisher::Value};

graphix_rt::props! {
    struct Props {
        disabled: bool,
        font: Option<FontV>,
        is_secure: bool,
        padding: PaddingV,
        placeholder: ArcStr,
        size: Option<TextSizeV>,
        value: ArcStr,
        width: LengthV,
    }
}

pub(crate) struct TextInputW<X: GXExt> {
    gx: GXHandle<X>,
    p: Props<X>,
    on_input: Handler<X>,
    on_submit: Handler<X>,
    /// What on_input sent that the value has not echoed yet.
    echoes: Echoes<ArcStr>,
}

impl<X: GXExt> TextInputW<X> {
    pub(crate) async fn compile(gx: GXHandle<X>, source: Value) -> Result<GuiW<X>> {
        let (p, on_input, on_submit) = try_join!(
            Props::compile(&gx, &source),
            Handler::field(&gx, &source, "on_input"),
            Handler::field(&gx, &source, "on_submit"),
        )
        .context("text_input")?;
        Ok(Box::new(Self { gx, p, on_input, on_submit, echoes: Echoes::new() }))
    }
}

impl<X: GXExt> GuiWidget<X> for TextInputW<X> {
    fn handle_update(
        &mut self,
        rt: &tokio::runtime::Handle,
        id: ExprId,
        v: &Value,
    ) -> Result<bool> {
        let changed = self.p.update(id, v).context("text_input")?;
        if id == self.p.value.r.id
            && let Some(value) = &self.p.value.t
        {
            self.echoes.delivered(value);
        }
        self.on_input
            .update(rt, &self.gx, id, v)
            .context("text_input on_input recompile")?;
        self.on_submit
            .update(rt, &self.gx, id, v)
            .context("text_input on_submit recompile")?;
        Ok(changed)
    }

    fn view(&self) -> IcedElement<'_> {
        let val = self.echoes.shown(self.p.value.t.as_ref()).map_or("", |v| v.as_str());
        let placeholder = self.p.placeholder.t.as_deref().unwrap_or("");
        let mut ti = widget::TextInput::new(placeholder, val);
        if !self.p.disabled.t.unwrap_or(false) {
            if let Some(callable) = &self.on_input.f {
                let id = callable.id();
                ti = ti.on_input(move |s| {
                    Message::Call(id, ValArray::from_iter([Value::String(s.into())]))
                });
            }
            if let Some(callable) = &self.on_submit.f {
                ti = ti.on_submit(Message::Call(
                    callable.id(),
                    ValArray::from_iter([Value::Null]),
                ));
            }
        }
        if self.p.is_secure.t == Some(true) {
            ti = ti.secure(true);
        }
        if let Some(w) = self.p.width.t.as_ref() {
            ti = ti.width(w.0);
        }
        if let Some(p) = self.p.padding.t.as_ref() {
            ti = ti.padding(p.0);
        }
        if let Some(Some(TextSizeV(sz))) = self.p.size.t {
            ti = ti.size(sz);
        }
        if let Some(Some(f)) = self.p.font.t.as_ref() {
            ti = ti.font(f.0);
        }
        ti.into()
    }

    fn on_message(&mut self, msg: &Message, _shell: &mut MessageShell) -> bool {
        match call_arg(msg, self.on_input.id()) {
            Some(Value::String(s)) => {
                self.echoes.sent(s.clone());
                true
            }
            _ => false,
        }
    }
}
