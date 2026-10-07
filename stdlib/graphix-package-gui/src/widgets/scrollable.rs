use super::{Child, GuiW, GuiWidget, Handler, IcedElement, Message};
use crate::types::{LengthV, ScrollDirectionV};
use anyhow::{Context, Result};
use futures::try_join;
use graphix_compiler::expr::ExprId;
use graphix_rt::{GXExt, GXHandle};
use iced_widget as widget;
use netidx::{protocol::valarray::ValArray, publisher::Value};

graphix_rt::props! {
    struct Props {
        direction: ScrollDirectionV,
        height: LengthV,
        width: LengthV,
    }
}

pub(crate) struct ScrollableW<X: GXExt> {
    gx: GXHandle<X>,
    p: Props<X>,
    child: Child<X>,
    on_scroll: Handler<X>,
}

impl<X: GXExt> ScrollableW<X> {
    pub(crate) async fn compile(gx: GXHandle<X>, source: Value) -> Result<GuiW<X>> {
        let (p, child, on_scroll) = try_join!(
            Props::compile(&gx, &source),
            Child::field(&gx, &source, "child"),
            Handler::field(&gx, &source, "on_scroll"),
        )
        .context("scrollable")?;
        Ok(Box::new(Self { gx, p, child, on_scroll }))
    }
}

impl<X: GXExt> GuiWidget<X> for ScrollableW<X> {
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
        let mut changed = self.p.update(id, v).context("scrollable")?;
        changed |= self
            .child
            .update(rt, &self.gx, id, v)
            .context("scrollable child recompile")?;
        self.on_scroll
            .update(rt, &self.gx, id, v)
            .context("scrollable on_scroll recompile")?;
        Ok(changed)
    }

    fn view(&self) -> IcedElement<'_> {
        let mut sc = widget::Scrollable::new(self.child.w.view());
        if let Some(dir) = self.p.direction.t.as_ref() {
            sc = sc.direction(dir.0);
        }
        if let Some(w) = self.p.width.t.as_ref() {
            sc = sc.width(w.0);
        }
        if let Some(h) = self.p.height.t.as_ref() {
            sc = sc.height(h.0);
        }
        if let Some(c) = &self.on_scroll.f {
            let id = c.id();
            sc = sc.on_scroll(move |viewport| {
                let off = viewport.absolute_offset();
                let offset_val: Value = [
                    (arcstr::literal!("x"), Value::F64(off.x as f64)),
                    (arcstr::literal!("y"), Value::F64(off.y as f64)),
                ]
                .into();
                Message::Call(id, ValArray::from_iter([offset_val]))
            });
        }
        sc.into()
    }
}
