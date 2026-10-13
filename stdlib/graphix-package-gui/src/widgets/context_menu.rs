use super::{
    Child, GuiW, GuiWidget, IcedElement, context_menu_widget::OwnedContextMenu,
    menu_bar::MenuItems,
};
use anyhow::{Context, Result};
use graphix_compiler::expr::ExprId;
use graphix_rt::{GXExt, GXHandle};
use netidx::publisher::Value;
use netidx_derive::FromValue;
use tokio::try_join;

pub(crate) struct ContextMenuW<X: GXExt> {
    gx: GXHandle<X>,
    child: Child<X>,
    items: MenuItems<X>,
}

impl<X: GXExt> ContextMenuW<X> {
    pub(crate) async fn compile(gx: GXHandle<X>, source: Value) -> Result<GuiW<X>> {
        // CR claude for claude: [structure/low] This casts the source to a local
        // `Fields` struct and calls `compile_ref` itself, where every other widget uses
        // `Child::field` / `gx.compile_field`, which also supply the shared field-name
        // error context. It then awaits `Child::compile` and `MenuItems::compile` one
        // after the other, so each context menu waits for two round trips in sequence
        // instead of overlapping them as the other widgets do. Replace both with
        // `try_join!(Child::field(&gx, &source, "child"), async {
        // MenuItems::compile(&gx, gx.compile_field(&source, "items").await?).await })`
        // and delete `Fields`. (gui-widgets-a-10)
        #[derive(FromValue)]
        struct Fields {
            child: u64,
            items: u64,
        }
        let Fields { child, items } = source.cast_to().context("context_menu flds")?;
        let (child_ref, items_ref) = try_join! {
            gx.compile_ref(child),
            gx.compile_ref(items),
        }?;
        let child = Child::compile(&gx, child_ref).await.context("context_menu child")?;
        let items =
            MenuItems::compile(&gx, items_ref).await.context("context_menu items")?;
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
