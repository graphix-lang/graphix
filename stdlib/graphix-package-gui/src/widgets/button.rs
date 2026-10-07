use super::{Child, GuiW, GuiWidget, Handler, IcedElement, Message};
use crate::types::{LengthV, PaddingV};
use anyhow::{Context, Result};
use futures::try_join;
use graphix_compiler::expr::ExprId;
use graphix_rt::{GXExt, GXHandle};
use iced_widget as widget;
use netidx::{protocol::valarray::ValArray, publisher::Value};

graphix_rt::props! {
    struct Props {
        disabled: bool,
        height: LengthV,
        padding: PaddingV,
        width: LengthV,
    }
}

pub(crate) struct ButtonW<X: GXExt> {
    gx: GXHandle<X>,
    p: Props<X>,
    on_press: Handler<X>,
    child: Child<X>,
}

impl<X: GXExt> ButtonW<X> {
    pub(crate) async fn compile(gx: GXHandle<X>, source: Value) -> Result<GuiW<X>> {
        let (p, on_press, child) = try_join!(
            Props::compile(&gx, &source),
            Handler::field(&gx, &source, "on_press"),
            Child::field(&gx, &source, "child"),
        )
        .context("button")?;
        Ok(Box::new(Self { gx, p, on_press, child }))
    }
}

impl<X: GXExt> GuiWidget<X> for ButtonW<X> {
    fn for_each_child_mut(&mut self, f: &mut dyn FnMut(&mut GuiW<X>)) {
        f(&mut self.child.w);
    }

    fn for_each_child(&self, f: &mut dyn FnMut(&GuiW<X>)) {
        f(&self.child.w);
    }

    fn handle_update(
        &mut self,
        rt: &tokio::runtime::Handle,
        id: ExprId,
        v: &Value,
    ) -> Result<bool> {
        let mut changed = self.p.update(id, v).context("button")?;
        self.on_press.update(rt, &self.gx, id, v).context("button on_press")?;
        changed |= self.child.update(rt, &self.gx, id, v).context("button child")?;
        Ok(changed)
    }

    fn view(&self) -> IcedElement<'_> {
        let mut btn = widget::Button::new(self.child.w.view());
        if !self.p.disabled.t.unwrap_or(false) {
            if let Some(callable) = &self.on_press.f {
                btn = btn.on_press(Message::Call(
                    callable.id(),
                    ValArray::from_iter([Value::Null]),
                ));
            }
        }
        if let Some(w) = self.p.width.t.as_ref() {
            btn = btn.width(w.0);
        }
        if let Some(h) = self.p.height.t.as_ref() {
            btn = btn.height(h.0);
        }
        if let Some(p) = self.p.padding.t.as_ref() {
            btn = btn.padding(p.0);
        }
        btn.into()
    }
}
