use super::{Child, GuiW, GuiWidget, IcedElement};
use crate::types::{HAlignV, LengthV, PaddingV, VAlignV};
use anyhow::{Context, Result};
use futures::try_join;
use graphix_compiler::expr::ExprId;
use graphix_rt::{GXExt, GXHandle};
use iced_widget as widget;
use netidx::publisher::Value;

graphix_rt::props! {
    struct Props {
        halign: HAlignV,
        height: LengthV,
        padding: PaddingV,
        valign: VAlignV,
        width: LengthV,
    }
}

pub(crate) struct ContainerW<X: GXExt> {
    gx: GXHandle<X>,
    p: Props<X>,
    child: Child<X>,
}

impl<X: GXExt> ContainerW<X> {
    pub(crate) async fn compile(gx: GXHandle<X>, source: Value) -> Result<GuiW<X>> {
        let (p, child) =
            try_join!(Props::compile(&gx, &source), Child::field(&gx, &source, "child"),)
                .context("container")?;
        Ok(Box::new(Self { gx, p, child }))
    }
}

impl<X: GXExt> GuiWidget<X> for ContainerW<X> {
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
        let mut changed = self.p.update(id, v).context("container")?;
        changed |= self
            .child
            .update(rt, &self.gx, id, v)
            .context("container child recompile")?;
        Ok(changed)
    }

    fn view(&self) -> IcedElement<'_> {
        let mut c = widget::Container::new(self.child.w.view());
        if let Some(p) = self.p.padding.t.as_ref() {
            c = c.padding(p.0);
        }
        if let Some(w) = self.p.width.t.as_ref() {
            c = c.width(w.0);
        }
        if let Some(h) = self.p.height.t.as_ref() {
            c = c.height(h.0);
        }
        if let Some(a) = self.p.halign.t.as_ref() {
            c = c.align_x(a.0);
        }
        if let Some(a) = self.p.valign.t.as_ref() {
            c = c.align_y(a.0);
        }
        c.into()
    }
}
