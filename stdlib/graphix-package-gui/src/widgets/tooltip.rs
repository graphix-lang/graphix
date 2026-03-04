use super::{compile, GuiW, GuiWidget, IcedElement};
use crate::types::TooltipPositionV;
use anyhow::{Context, Result};
use arcstr::ArcStr;
use graphix_compiler::expr::ExprId;
use graphix_rt::{CallableId, GXExt, GXHandle, Ref, TRef};
use iced_widget as widget;
use netidx::publisher::Value;
use tokio::try_join;

pub(crate) struct TooltipW<X: GXExt> {
    gx: GXHandle<X>,
    child_ref: Ref<X>,
    child: GuiW<X>,
    tip_ref: Ref<X>,
    tip: GuiW<X>,
    position: TRef<X, TooltipPositionV>,
    gap: TRef<X, Option<f64>>,
}

impl<X: GXExt> TooltipW<X> {
    pub(crate) async fn compile(gx: GXHandle<X>, source: Value) -> Result<GuiW<X>> {
        let [(_, child), (_, gap), (_, position), (_, tip)] =
            source.cast_to::<[(ArcStr, u64); 4]>().context("tooltip flds")?;
        let (child_ref, gap, position, tip_ref) = try_join! {
            gx.compile_ref(child),
            gx.compile_ref(gap),
            gx.compile_ref(position),
            gx.compile_ref(tip),
        }?;
        let compiled_child = compile_child!(gx, child_ref, "tooltip child");
        let compiled_tip = compile_child!(gx, tip_ref, "tooltip tip");
        Ok(Box::new(Self {
            gx: gx.clone(),
            child_ref,
            child: compiled_child,
            tip_ref,
            tip: compiled_tip,
            position: TRef::new(position).context("tooltip tref position")?,
            gap: TRef::new(gap).context("tooltip tref gap")?,
        }))
    }
}

impl<X: GXExt> GuiWidget<X> for TooltipW<X> {
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
        update_child!(self, rt, id, v, changed, child_ref, child, "tooltip child recompile");
        update_child!(self, rt, id, v, changed, tip_ref, tip, "tooltip tip recompile");
        Ok(changed)
    }

    fn editor_action(
        &mut self,
        id: ExprId,
        action: &iced_widget::text_editor::Action,
    ) -> Option<(CallableId, Value)> {
        if let some @ Some(_) = self.child.editor_action(id, action) {
            return some;
        }
        self.tip.editor_action(id, action)
    }

    fn view(&self) -> IcedElement<'_> {
        let pos = self
            .position
            .t
            .as_ref()
            .map(|p| p.0)
            .unwrap_or(widget::tooltip::Position::Bottom);
        let mut tt = widget::Tooltip::new(self.child.view(), self.tip.view(), pos);
        if let Some(Some(g)) = self.gap.t {
            tt = tt.gap(g as f32);
        }
        tt.into()
    }
}
