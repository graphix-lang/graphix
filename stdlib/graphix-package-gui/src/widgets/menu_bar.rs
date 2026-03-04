use super::menu_bar_widget::{MenuGroupDesc, MenuItemDesc, OwnedMenuBar};
use super::{GuiW, IcedElement};
use crate::types::{LengthV, ShortcutV};
use anyhow::{bail, Context, Result};
use arcstr::ArcStr;
use graphix_compiler::expr::ExprId;
use graphix_rt::{Callable, GXExt, GXHandle, Ref, TRef};
use iced_core::Length;
use netidx::publisher::Value;
use smallvec::SmallVec;
use tokio::try_join;

enum MenuItemKind<X: GXExt> {
    Action {
        label: TRef<X, String>,
        shortcut: TRef<X, Option<ShortcutV>>,
        on_click: Ref<X>,
        on_click_callable: Option<Callable<X>>,
        disabled: TRef<X, bool>,
    },
    Divider,
}

struct CompiledMenuGroup<X: GXExt> {
    label: TRef<X, String>,
    items_ref: Ref<X>,
    items: Vec<MenuItemKind<X>>,
}

pub(crate) struct MenuBarW<X: GXExt> {
    gx: GXHandle<X>,
    menus_ref: Ref<X>,
    menus: Vec<CompiledMenuGroup<X>>,
    width: TRef<X, LengthV>,
}

async fn compile_menu_item<X: GXExt>(
    gx: &GXHandle<X>,
    v: Value,
) -> Result<MenuItemKind<X>> {
    // `Divider has no payload, so it encodes as a bare string tag
    if let Value::String(tag) = &v {
        return match tag.as_str() {
            "Divider" => Ok(MenuItemKind::Divider),
            s => bail!("invalid menu item variant: {s}"),
        };
    }
    let (tag, inner) = v.cast_to::<(ArcStr, Value)>().context("menu item tag")?;
    match &*tag {
        "Action" => {
            let [(_, disabled_id), (_, label_id), (_, on_click_id), (_, shortcut_id)] =
                inner.cast_to::<[(ArcStr, u64); 4]>().context("menu action flds")?;
            let (disabled, label, on_click, shortcut) = try_join! {
                gx.compile_ref(disabled_id),
                gx.compile_ref(label_id),
                gx.compile_ref(on_click_id),
                gx.compile_ref(shortcut_id),
            }?;
            let on_click_callable = match on_click.last.as_ref() {
                Some(v) => Some(
                    gx.compile_callable(v.clone())
                        .await
                        .context("menu action on_click")?,
                ),
                None => None,
            };
            Ok(MenuItemKind::Action {
                label: TRef::new(label).context("menu action tref label")?,
                shortcut: TRef::new(shortcut).context("menu action tref shortcut")?,
                on_click,
                on_click_callable,
                disabled: TRef::new(disabled).context("menu action tref disabled")?,
            })
        }
        s => bail!("invalid menu item variant: {s}"),
    }
}

async fn compile_menu_items<X: GXExt>(
    gx: &GXHandle<X>,
    v: Value,
) -> Result<Vec<MenuItemKind<X>>> {
    let items = v.cast_to::<SmallVec<[Value; 8]>>()?;
    let mut result = Vec::with_capacity(items.len());
    for item in items {
        result.push(compile_menu_item(gx, item).await?);
    }
    Ok(result)
}

async fn compile_menu_group<X: GXExt>(
    gx: &GXHandle<X>,
    v: Value,
) -> Result<CompiledMenuGroup<X>> {
    let [(_, items_id), (_, label_id)] =
        v.cast_to::<[(ArcStr, u64); 2]>().context("menu group flds")?;
    let (items_ref, label) =
        try_join! { gx.compile_ref(items_id), gx.compile_ref(label_id), }?;
    let items = match items_ref.last.as_ref() {
        Some(v) => compile_menu_items(gx, v.clone()).await.context("menu group items")?,
        None => vec![],
    };
    Ok(CompiledMenuGroup {
        label: TRef::new(label).context("menu group tref label")?,
        items_ref,
        items,
    })
}

async fn compile_menus<X: GXExt>(
    gx: &GXHandle<X>,
    v: Value,
) -> Result<Vec<CompiledMenuGroup<X>>> {
    let groups = v.cast_to::<SmallVec<[Value; 8]>>()?;
    let mut result = Vec::with_capacity(groups.len());
    for group in groups {
        result.push(compile_menu_group(gx, group).await?);
    }
    Ok(result)
}

impl<X: GXExt> MenuBarW<X> {
    pub(crate) async fn compile(gx: GXHandle<X>, source: Value) -> Result<GuiW<X>> {
        let [(_, menus_id), (_, width_id)] =
            source.cast_to::<[(ArcStr, u64); 2]>().context("menu_bar flds")?;
        let (menus_ref, width) =
            try_join! { gx.compile_ref(menus_id), gx.compile_ref(width_id), }?;
        let menus = match menus_ref.last.as_ref() {
            Some(v) => compile_menus(&gx, v.clone()).await.context("menu_bar menus")?,
            None => vec![],
        };
        Ok(Box::new(Self {
            gx: gx.clone(),
            menus_ref,
            menus,
            width: TRef::new(width).context("menu_bar tref width")?,
        }))
    }
}

impl<X: GXExt> super::GuiWidget<X> for MenuBarW<X> {
    fn handle_update(
        &mut self,
        rt: &tokio::runtime::Handle,
        id: ExprId,
        v: &Value,
    ) -> Result<bool> {
        let mut changed = false;
        changed |= self.width.update(id, v).context("menu_bar update width")?.is_some();
        if id == self.menus_ref.id {
            self.menus_ref.last = Some(v.clone());
            self.menus = rt
                .block_on(compile_menus(&self.gx, v.clone()))
                .context("menu_bar menus recompile")?;
            changed = true;
        }
        for group in &mut self.menus {
            changed |=
                group.label.update(id, v).context("menu group update label")?.is_some();
            if id == group.items_ref.id {
                group.items_ref.last = Some(v.clone());
                group.items = rt
                    .block_on(compile_menu_items(&self.gx, v.clone()))
                    .context("menu group items recompile")?;
                changed = true;
            }
            for item in &mut group.items {
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
                            .context("menu item update label")?
                            .is_some();
                        changed |= shortcut
                            .update(id, v)
                            .context("menu item update shortcut")?
                            .is_some();
                        changed |= disabled
                            .update(id, v)
                            .context("menu item update disabled")?
                            .is_some();
                        if id == on_click.id {
                            on_click.last = Some(v.clone());
                            *on_click_callable = Some(
                                rt.block_on(self.gx.compile_callable(v.clone()))
                                    .context("menu item on_click recompile")?,
                            );
                        }
                    }
                    MenuItemKind::Divider => {}
                }
            }
        }
        Ok(changed)
    }

    fn view(&self) -> IcedElement<'_> {
        let descs: Vec<MenuGroupDesc> = self
            .menus
            .iter()
            .map(|group| MenuGroupDesc {
                label: group.label.t.as_deref().unwrap_or("").to_string(),
                items: group
                    .items
                    .iter()
                    .map(|item| match item {
                        MenuItemKind::Action {
                            label,
                            shortcut,
                            on_click_callable,
                            disabled,
                            ..
                        } => MenuItemDesc::Action {
                            label: label.t.as_deref().unwrap_or("").to_string(),
                            shortcut: shortcut
                                .t
                                .as_ref()
                                .and_then(|o| o.clone()),
                            callable_id: on_click_callable.as_ref().map(|c| c.id()),
                            disabled: disabled.t.unwrap_or(false),
                        },
                        MenuItemKind::Divider => MenuItemDesc::Divider,
                    })
                    .collect(),
            })
            .collect();
        let width = self.width.t.as_ref().map(|w| w.0).unwrap_or(Length::Shrink);
        OwnedMenuBar { descs, width }.into()
    }
}
