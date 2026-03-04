use super::context_menu_widget::OwnedContextMenu;
use super::menu_bar::{compile_menu_items, menu_item_desc, MenuItemKind};
use super::{compile, GuiW, GuiWidget, IcedElement};
use anyhow::{Context, Result};
use arcstr::ArcStr;
use graphix_compiler::expr::ExprId;
use graphix_rt::{CallableId, GXExt, GXHandle, Ref};
use netidx::publisher::Value;
use tokio::try_join;

pub(crate) struct ContextMenuW<X: GXExt> {
    gx: GXHandle<X>,
    child_ref: Ref<X>,
    child: GuiW<X>,
    items_ref: Ref<X>,
    items: Vec<MenuItemKind<X>>,
}

impl<X: GXExt> ContextMenuW<X> {
    pub(crate) async fn compile(gx: GXHandle<X>, source: Value) -> Result<GuiW<X>> {
        let [(_, child), (_, items)] =
            source.cast_to::<[(ArcStr, u64); 2]>().context("context_menu flds")?;
        let (child_ref, items_ref) = try_join! {
            gx.compile_ref(child),
            gx.compile_ref(items),
        }?;
        let compiled_child = compile_child!(gx, child_ref, "context_menu child");
        let compiled_items = match items_ref.last.as_ref() {
            Some(v) => {
                compile_menu_items(&gx, v.clone()).await.context("context_menu items")?
            }
            None => vec![],
        };
        Ok(Box::new(Self {
            gx: gx.clone(),
            child_ref,
            child: compiled_child,
            items_ref,
            items: compiled_items,
        }))
    }
}

impl<X: GXExt> GuiWidget<X> for ContextMenuW<X> {
    fn handle_update(
        &mut self,
        rt: &tokio::runtime::Handle,
        id: ExprId,
        v: &Value,
    ) -> Result<bool> {
        let mut changed = false;
        update_child!(
            self, rt, id, v, changed, child_ref, child,
            "context_menu child recompile"
        );
        if id == self.items_ref.id {
            self.items_ref.last = Some(v.clone());
            self.items = rt
                .block_on(compile_menu_items(&self.gx, v.clone()))
                .context("context_menu items recompile")?;
            changed = true;
        }
        for item in &mut self.items {
            match item {
                MenuItemKind::Action {
                    label,
                    shortcut,
                    on_click,
                    on_click_callable,
                    disabled,
                } => {
                    changed |= label
                        .update(id, v)
                        .context("context_menu item update label")?
                        .is_some();
                    changed |= shortcut
                        .update(id, v)
                        .context("context_menu item update shortcut")?
                        .is_some();
                    changed |= disabled
                        .update(id, v)
                        .context("context_menu item update disabled")?
                        .is_some();
                    if id == on_click.id {
                        on_click.last = Some(v.clone());
                        *on_click_callable = Some(
                            rt.block_on(self.gx.compile_callable(v.clone()))
                                .context("context_menu item on_click recompile")?,
                        );
                    }
                }
                MenuItemKind::Divider => {}
            }
        }
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
        let items = self.items.iter().map(menu_item_desc).collect();
        OwnedContextMenu::new(self.child.view(), items).into()
    }
}
