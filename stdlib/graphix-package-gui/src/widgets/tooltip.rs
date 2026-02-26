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
        let compiled_child: GuiW<X> = match child_ref.last.as_ref() {
            None => Box::new(super::EmptyW),
            Some(v) => compile(gx.clone(), v.clone()).await.context("tooltip child")?,
        };
        let compiled_tip: GuiW<X> = match tip_ref.last.as_ref() {
            None => Box::new(super::EmptyW),
            Some(v) => compile(gx.clone(), v.clone()).await.context("tooltip tip")?,
        };
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
        if id == self.child_ref.id {
            self.child_ref.last = Some(v.clone());
            self.child = rt
                .block_on(compile(self.gx.clone(), v.clone()))
                .context("tooltip child recompile")?;
            changed = true;
        }
        if id == self.tip_ref.id {
            self.tip_ref.last = Some(v.clone());
            self.tip = rt
                .block_on(compile(self.gx.clone(), v.clone()))
                .context("tooltip tip recompile")?;
            changed = true;
        }
        changed |= self.child.handle_update(rt, id, v)?;
        changed |= self.tip.handle_update(rt, id, v)?;
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
