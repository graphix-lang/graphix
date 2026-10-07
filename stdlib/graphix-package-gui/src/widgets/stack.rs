use super::{Children, GuiW, GuiWidget, IcedElement};
use crate::types::LengthV;
use anyhow::{Context, Result};
use futures::try_join;
use graphix_compiler::expr::ExprId;
use graphix_rt::{GXExt, GXHandle};
use iced_widget as widget;
use netidx::publisher::Value;

graphix_rt::props! {
    struct Props {
        height: LengthV,
        width: LengthV,
    }
}

pub(crate) struct StackW<X: GXExt> {
    gx: GXHandle<X>,
    p: Props<X>,
    children: Children<X>,
}

impl<X: GXExt> StackW<X> {
    pub(crate) async fn compile(gx: GXHandle<X>, source: Value) -> Result<GuiW<X>> {
        let (p, children) = try_join!(
            Props::compile(&gx, &source),
            Children::field(&gx, &source, "children"),
        )
        .context("stack")?;
        Ok(Box::new(Self { gx, p, children }))
    }
}

impl<X: GXExt> GuiWidget<X> for StackW<X> {
    fn for_each_child_mut(&mut self, f: &mut dyn FnMut(&mut GuiW<X>)) {
        self.children.ws.iter_mut().for_each(f)
    }

    fn for_each_child(&self, f: &mut dyn FnMut(&GuiW<X>)) {
        self.children.ws.iter().for_each(f)
    }

    fn handle_update(
        &mut self,
        rt: &tokio::runtime::Handle,
        id: ExprId,
        v: &Value,
    ) -> Result<bool> {
        let mut changed = self.p.update(id, v).context("stack")?;
        changed |= self.children.update(rt, &self.gx, id, v).context("stack children")?;
        Ok(changed)
    }

    fn view(&self) -> IcedElement<'_> {
        let mut s = widget::Stack::new();
        if let Some(w) = self.p.width.t.as_ref() {
            s = s.width(w.0);
        }
        if let Some(h) = self.p.height.t.as_ref() {
            s = s.height(h.0);
        }
        for child in &self.children.ws {
            s = s.push(child.view());
        }
        s.into()
    }
}
