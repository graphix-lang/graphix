use super::{Child, GuiW, GuiWidget, IcedElement};
use crate::types::TooltipPositionV;
use anyhow::{Context, Result};
use futures::try_join;
use graphix_compiler::expr::ExprId;
use graphix_rt::{GXExt, GXHandle};
use iced_widget as widget;
use netidx::publisher::Value;

graphix_rt::props! {
    struct Props {
        gap: Option<f64>,
        position: TooltipPositionV,
    }
}

pub(crate) struct TooltipW<X: GXExt> {
    gx: GXHandle<X>,
    p: Props<X>,
    child: Child<X>,
    tip: Child<X>,
}

impl<X: GXExt> TooltipW<X> {
    pub(crate) async fn compile(gx: GXHandle<X>, source: Value) -> Result<GuiW<X>> {
        let (p, child, tip) = try_join!(
            Props::compile(&gx, &source),
            Child::field(&gx, &source, "child"),
            Child::field(&gx, &source, "tip"),
        )
        .context("tooltip")?;
        Ok(Box::new(Self { gx, p, child, tip }))
    }
}

impl<X: GXExt> GuiWidget<X> for TooltipW<X> {
    fn for_each_child_mut(&mut self, f: &mut dyn FnMut(&mut GuiW<X>)) {
        f(&mut self.child.w);
        f(&mut self.tip.w);
    }

    fn for_each_child(&self, f: &mut dyn FnMut(&GuiW<X>)) {
        f(&self.child.w);
        f(&self.tip.w);
    }

    fn handle_update(
        &mut self,
        rt: &tokio::runtime::Handle,
        id: ExprId,
        v: &Value,
    ) -> Result<bool> {
        let mut changed = self.p.update(id, v).context("tooltip")?;
        changed |=
            self.child.update(rt, &self.gx, id, v).context("tooltip child recompile")?;
        changed |=
            self.tip.update(rt, &self.gx, id, v).context("tooltip tip recompile")?;
        Ok(changed)
    }

    fn view(&self) -> IcedElement<'_> {
        let pos = self
            .p
            .position
            .t
            .as_ref()
            .map(|p| p.0)
            .unwrap_or(widget::tooltip::Position::Bottom);
        let mut tt = widget::Tooltip::new(self.child.w.view(), self.tip.w.view(), pos);
        if let Some(Some(g)) = self.p.gap.t {
            tt = tt.gap(g as f32);
        }
        tt.into()
    }
}
