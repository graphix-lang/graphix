use crate::{
    compile,
    types::LengthV,
    GuiW, GuiWidget, IcedElement,
};
use anyhow::{Context, Result};
use arcstr::ArcStr;
use graphix_compiler::{expr::ExprId, BindId};
use graphix_rt::{GXExt, GXHandle, Ref, TRef};
use iced_widget as widget;
use netidx::publisher::Value;
use tokio::try_join;

use crate::types::ScrollDirectionV;

pub(crate) struct ScrollableW<X: GXExt> {
    gx: GXHandle<X>,
    child_ref: Ref<X>,
    child: GuiW<X>,
    direction: TRef<X, ScrollDirectionV>,
    width: TRef<X, LengthV>,
    height: TRef<X, LengthV>,
}

impl<X: GXExt> ScrollableW<X> {
    pub(crate) async fn compile(gx: GXHandle<X>, source: Value) -> Result<GuiW<X>> {
        let [(_, child), (_, direction), (_, height), (_, width)] =
            source.cast_to::<[(ArcStr, u64); 4]>().context("scrollable flds")?;
        let (child_ref, direction, height, width) = try_join! {
            gx.compile_ref(child),
            gx.compile_ref(direction),
            gx.compile_ref(height),
            gx.compile_ref(width),
        }?;
        let compiled_child: GuiW<X> = match child_ref.last.as_ref() {
            None => Box::new(crate::EmptyW),
            Some(v) => compile(gx.clone(), v.clone()).await.context("scrollable child")?,
        };
        Ok(Box::new(Self {
            gx: gx.clone(),
            child_ref,
            child: compiled_child,
            direction: TRef::new(direction).context("scrollable tref direction")?,
            width: TRef::new(width).context("scrollable tref width")?,
            height: TRef::new(height).context("scrollable tref height")?,
        }))
    }
}

impl<X: GXExt> GuiWidget<X> for ScrollableW<X> {
    fn handle_update(
        &mut self,
        rt: &tokio::runtime::Handle,
        id: ExprId,
        v: &Value,
    ) -> Result<bool> {
        let mut changed = false;
        changed |= self.direction.update(id, v).context("scrollable update direction")?.is_some();
        changed |= self.width.update(id, v).context("scrollable update width")?.is_some();
        changed |= self.height.update(id, v).context("scrollable update height")?.is_some();
        if id == self.child_ref.id {
            self.child_ref.last = Some(v.clone());
            self.child = rt.block_on(compile(self.gx.clone(), v.clone()))
                .context("scrollable child recompile")?;
            changed = true;
        }
        changed |= self.child.handle_update(rt, id, v)?;
        Ok(changed)
    }

    fn editor_action(
        &mut self,
        id: ExprId,
        action: &iced_widget::text_editor::Action,
    ) -> Option<(BindId, Value)> {
        self.child.editor_action(id, action)
    }

    fn view(&self) -> IcedElement<'_> {
        let mut sc = widget::Scrollable::new(self.child.view());
        if let Some(dir) = self.direction.t.as_ref() {
            sc = sc.direction(dir.0);
        }
        if let Some(w) = self.width.t.as_ref() {
            sc = sc.width(w.0);
        }
        if let Some(h) = self.height.t.as_ref() {
            sc = sc.height(h.0);
        }
        sc.into()
    }
}
