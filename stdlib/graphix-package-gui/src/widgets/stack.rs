use super::{Children, GuiW, GuiWidget, IcedElement};
use crate::types::LengthV;
use anyhow::{Context, Result};
use graphix_compiler::expr::ExprId;
use graphix_rt::{GXExt, GXHandle, TRef};
use iced_widget as widget;
use netidx::publisher::Value;
use netidx_derive::FromValue;
use tokio::try_join;

pub(crate) struct StackW<X: GXExt> {
    gx: GXHandle<X>,
    children: Children<X>,
    width: TRef<X, LengthV>,
    height: TRef<X, LengthV>,
}

impl<X: GXExt> StackW<X> {
    pub(crate) async fn compile(gx: GXHandle<X>, source: Value) -> Result<GuiW<X>> {
        #[derive(FromValue)]
        struct Fields {
            children: u64,
            height: u64,
            width: u64,
        }
        let Fields { children, height, width } =
            source.cast_to().context("stack flds")?;
        let (children_ref, height, width) = try_join! {
            gx.compile_ref(children),
            gx.compile_ref(height),
            gx.compile_ref(width),
        }?;
        let children =
            Children::compile(&gx, children_ref).await.context("stack children")?;
        Ok(Box::new(Self {
            gx: gx.clone(),
            children,
            width: TRef::new(width).context("stack tref width")?,
            height: TRef::new(height).context("stack tref height")?,
        }))
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
        let mut changed = false;
        changed |= self.width.update(id, v).context("stack update width")?.is_some();
        changed |= self.height.update(id, v).context("stack update height")?.is_some();
        changed |= self.children.update(rt, &self.gx, id, v).context("stack children")?;
        Ok(changed)
    }

    fn view(&self) -> IcedElement<'_> {
        let mut s = widget::Stack::new();
        if let Some(w) = self.width.t.as_ref() {
            s = s.width(w.0);
        }
        if let Some(h) = self.height.t.as_ref() {
            s = s.height(h.0);
        }
        for child in &self.children.ws {
            s = s.push(child.view());
        }
        s.into()
    }
}
