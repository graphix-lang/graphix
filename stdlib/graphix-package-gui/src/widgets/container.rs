use super::{compile, GuiW, GuiWidget, IcedElement};
use crate::types::{HAlignV, LengthV, PaddingV, VAlignV};
use anyhow::{Context, Result};
use arcstr::ArcStr;
use graphix_compiler::expr::ExprId;
use graphix_rt::{CallableId, GXExt, GXHandle, Ref, TRef};
use iced_widget as widget;
use netidx::publisher::Value;
use tokio::try_join;

pub(crate) struct ContainerW<X: GXExt> {
    gx: GXHandle<X>,
    padding: TRef<X, PaddingV>,
    width: TRef<X, LengthV>,
    height: TRef<X, LengthV>,
    halign: TRef<X, HAlignV>,
    valign: TRef<X, VAlignV>,
    child_ref: Ref<X>,
    child: GuiW<X>,
}

impl<X: GXExt> ContainerW<X> {
    pub(crate) async fn compile(gx: GXHandle<X>, source: Value) -> Result<GuiW<X>> {
        let [(_, child), (_, halign), (_, height), (_, padding), (_, valign), (_, width)] =
            source.cast_to::<[(ArcStr, u64); 6]>().context("container flds")?;
        let (child_ref, halign, height, padding, valign, width) = try_join! {
            gx.compile_ref(child),
            gx.compile_ref(halign),
            gx.compile_ref(height),
            gx.compile_ref(padding),
            gx.compile_ref(valign),
            gx.compile_ref(width),
        }?;
        let compiled_child: GuiW<X> = match child_ref.last.as_ref() {
            None => Box::new(super::EmptyW),
            Some(v) => compile(gx.clone(), v.clone()).await.context("container child")?,
        };
        Ok(Box::new(Self {
            gx: gx.clone(),
            padding: TRef::new(padding).context("container tref padding")?,
            width: TRef::new(width).context("container tref width")?,
            height: TRef::new(height).context("container tref height")?,
            halign: TRef::new(halign).context("container tref halign")?,
            valign: TRef::new(valign).context("container tref valign")?,
            child_ref,
            child: compiled_child,
        }))
    }
}

impl<X: GXExt> GuiWidget<X> for ContainerW<X> {
    fn handle_update(
        &mut self,
        rt: &tokio::runtime::Handle,
        id: ExprId,
        v: &Value,
    ) -> Result<bool> {
        let mut changed = false;
        changed |=
            self.padding.update(id, v).context("container update padding")?.is_some();
        changed |= self.width.update(id, v).context("container update width")?.is_some();
        changed |=
            self.height.update(id, v).context("container update height")?.is_some();
        changed |=
            self.halign.update(id, v).context("container update halign")?.is_some();
        changed |=
            self.valign.update(id, v).context("container update valign")?.is_some();
        if id == self.child_ref.id {
            self.child_ref.last = Some(v.clone());
            self.child = rt
                .block_on(compile(self.gx.clone(), v.clone()))
                .context("container child recompile")?;
            changed = true;
        }
        changed |= self.child.handle_update(rt, id, v)?;
        Ok(changed)
    }

    fn editor_action(
        &mut self,
        id: ExprId,
        action: &iced_widget::text_editor::Action,
    ) -> Option<(CallableId, Value)> {
        self.child.editor_action(id, action)
    }

    fn view(&self) -> IcedElement<'_> {
        let mut c = widget::Container::new(self.child.view());
        if let Some(p) = self.padding.t.as_ref() {
            c = c.padding(p.0);
        }
        if let Some(w) = self.width.t.as_ref() {
            c = c.width(w.0);
        }
        if let Some(h) = self.height.t.as_ref() {
            c = c.height(h.0);
        }
        if let Some(a) = self.halign.t.as_ref() {
            c = c.align_x(a.0);
        }
        if let Some(a) = self.valign.t.as_ref() {
            c = c.align_y(a.0);
        }
        c.into()
    }
}
