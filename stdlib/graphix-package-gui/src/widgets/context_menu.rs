use super::{
    Child, GuiW, GuiWidget, IcedElement, context_menu_widget::OwnedContextMenu,
    menu_bar::MenuItems,
};
use anyhow::{Context, Result};
use futures::try_join;
use graphix_compiler::expr::ExprId;
use graphix_rt::{GXExt, GXHandle};
use netidx::publisher::Value;

pub(crate) struct ContextMenuW<X: GXExt> {
    gx: GXHandle<X>,
    child: Child<X>,
    items: MenuItems<X>,
}

impl<X: GXExt> ContextMenuW<X> {
    pub(crate) async fn compile(gx: GXHandle<X>, source: Value) -> Result<GuiW<X>> {
        let (child, items) = try_join!(Child::field(&gx, &source, "child"), async {
            MenuItems::compile(&gx, gx.compile_field(&source, "items").await?).await
        },)
        .context("context_menu")?;
        Ok(Box::new(Self { gx: gx.clone(), child, items }))
    }
}

impl<X: GXExt> GuiWidget<X> for ContextMenuW<X> {
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
        let mut changed = false;
        changed |= self
            .child
            .update(rt, &self.gx, id, v)
            .context("context_menu child recompile")?;
        changed |=
            self.items.update(rt, &self.gx, id, v).context("context_menu items")?;
        Ok(changed)
    }

    fn view(&self) -> IcedElement<'_> {
        OwnedContextMenu::new(self.child.w.view(), self.items.descs()).into()
    }
}
