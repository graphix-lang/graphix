use super::{Child, GuiW, GuiWidget, IcedElement};
use crate::types::TooltipPositionV;
use anyhow::{Context, Result};
use graphix_compiler::expr::ExprId;
use graphix_rt::{GXExt, GXHandle, TRef};
use iced_widget as widget;
use netidx::publisher::Value;
use netidx_derive::FromValue;
use tokio::try_join;

pub(crate) struct TooltipW<X: GXExt> {
    gx: GXHandle<X>,
    child: Child<X>,
    tip: Child<X>,
    position: TRef<X, TooltipPositionV>,
    gap: TRef<X, Option<f64>>,
}

impl<X: GXExt> TooltipW<X> {
    pub(crate) async fn compile(gx: GXHandle<X>, source: Value) -> Result<GuiW<X>> {
        #[derive(FromValue)]
        struct Fields {
            child: u64,
            gap: u64,
            position: u64,
            tip: u64,
        }
        let Fields { child, gap, position, tip } =
            source.cast_to().context("tooltip flds")?;
        let (child_ref, gap, position, tip_ref) = try_join! {
            gx.compile_ref(child),
            gx.compile_ref(gap),
            gx.compile_ref(position),
            gx.compile_ref(tip),
        }?;
        let child = Child::compile(&gx, child_ref).await.context("tooltip child")?;
        let tip = Child::compile(&gx, tip_ref).await.context("tooltip tip")?;
        Ok(Box::new(Self {
            gx: gx.clone(),
            child,
            tip,
            position: TRef::new(position).context("tooltip tref position")?,
            gap: TRef::new(gap).context("tooltip tref gap")?,
        }))
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
        let mut changed = false;
        changed |=
            self.position.update(id, v).context("tooltip update position")?.is_some();
        changed |= self.gap.update(id, v).context("tooltip update gap")?.is_some();
        changed |=
            self.child.update(rt, &self.gx, id, v).context("tooltip child recompile")?;
        changed |=
            self.tip.update(rt, &self.gx, id, v).context("tooltip tip recompile")?;
        Ok(changed)
    }

    fn view(&self) -> IcedElement<'_> {
        let pos = self
            .position
            .t
            .as_ref()
            .map(|p| p.0)
            .unwrap_or(widget::tooltip::Position::Bottom);
        let mut tt = widget::Tooltip::new(self.child.w.view(), self.tip.w.view(), pos);
        if let Some(Some(g)) = self.gap.t {
            tt = tt.gap(g as f32);
        }
        tt.into()
    }
}
