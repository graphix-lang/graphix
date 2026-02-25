use crate::{
    compile_children,
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

pub(crate) struct StackW<X: GXExt> {
    gx: GXHandle<X>,
    children_ref: Ref<X>,
    children: Vec<GuiW<X>>,
    width: TRef<X, LengthV>,
    height: TRef<X, LengthV>,
}

impl<X: GXExt> StackW<X> {
    pub(crate) async fn compile(gx: GXHandle<X>, source: Value) -> Result<GuiW<X>> {
        let [(_, children), (_, height), (_, width)] =
            source.cast_to::<[(ArcStr, u64); 3]>().context("stack flds")?;
        let (children_ref, height, width) = try_join! {
            gx.compile_ref(children),
            gx.compile_ref(height),
            gx.compile_ref(width),
        }?;
        let compiled_children = match children_ref.last.as_ref() {
            None => vec![],
            Some(v) => compile_children(gx.clone(), v.clone()).await
                .context("stack children")?,
        };
        Ok(Box::new(Self {
            gx: gx.clone(),
            children_ref,
            children: compiled_children,
            width: TRef::new(width).context("stack tref width")?,
            height: TRef::new(height).context("stack tref height")?,
        }))
    }
}

impl<X: GXExt> GuiWidget<X> for StackW<X> {
    fn handle_update(
        &mut self,
        rt: &tokio::runtime::Handle,
        id: ExprId,
        v: &Value,
    ) -> Result<bool> {
        let mut changed = false;
        changed |= self.width.update(id, v).context("stack update width")?.is_some();
        changed |= self.height.update(id, v).context("stack update height")?.is_some();
        if id == self.children_ref.id {
            self.children_ref.last = Some(v.clone());
            self.children = rt.block_on(
                compile_children(self.gx.clone(), v.clone())
            ).context("stack children recompile")?;
            changed = true;
        }
        for child in &mut self.children {
            changed |= child.handle_update(rt, id, v)?;
        }
        Ok(changed)
    }

    fn editor_action(
        &mut self,
        id: ExprId,
        action: &iced_widget::text_editor::Action,
    ) -> Option<(BindId, Value)> {
        for child in &mut self.children {
            if let some @ Some(_) = child.editor_action(id, action) {
                return some;
            }
        }
        None
    }

    fn view(&self) -> IcedElement<'_> {
        let mut s = widget::Stack::new();
        if let Some(w) = self.width.t.as_ref() {
            s = s.width(w.0);
        }
        if let Some(h) = self.height.t.as_ref() {
            s = s.height(h.0);
        }
        for child in &self.children {
            s = s.push(child.view());
        }
        s.into()
    }
}
