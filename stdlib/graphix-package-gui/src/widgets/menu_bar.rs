use super::{
    GuiW, Handler, IcedElement,
    menu_bar_widget::{MenuGroupDesc, MenuItemDesc, OwnedMenuBar},
    reconcile,
};
use crate::types::{LengthV, ShortcutV};
use anyhow::{Context, Result};
use arcstr::ArcStr;
use graphix_compiler::expr::ExprId;
use graphix_rt::{GXExt, GXHandle, Ref, TRef};
use iced_core::Length;
use netidx::publisher::Value;
use netidx_derive::FromValue;
use tokio::try_join;

pub(crate) enum MenuItemKind<X: GXExt> {
    Action {
        label: TRef<X, ArcStr>,
        shortcut: TRef<X, Option<ShortcutV>>,
        on_click: Handler<X>,
        disabled: TRef<X, bool>,
    },
    Divider,
}

impl<X: GXExt> MenuItemKind<X> {
    async fn compile(gx: &GXHandle<X>, v: Value) -> Result<Self> {
        #[derive(FromValue)]
        enum Repr {
            Divider,
            Action { disabled: u64, label: u64, on_click: u64, shortcut: u64 },
        }
        match v.cast_to::<Repr>().context("menu item")? {
            Repr::Divider => Ok(Self::Divider),
            Repr::Action { disabled, label, on_click, shortcut } => {
                let (disabled, label, on_click, shortcut) = try_join! {
                    gx.compile_ref(disabled),
                    gx.compile_ref(label),
                    gx.compile_ref(on_click),
                    gx.compile_ref(shortcut),
                }?;
                Ok(Self::Action {
                    label: TRef::new(label).context("menu action tref label")?,
                    shortcut: TRef::new(shortcut).context("menu action tref shortcut")?,
                    on_click: Handler::compile(gx, on_click)
                        .await
                        .context("menu action on_click")?,
                    disabled: TRef::new(disabled).context("menu action tref disabled")?,
                })
            }
        }
    }

    fn update(
        &mut self,
        rt: &tokio::runtime::Handle,
        gx: &GXHandle<X>,
        id: ExprId,
        v: &Value,
    ) -> Result<bool> {
        let Self::Action { label, shortcut, on_click, disabled } = self else {
            return Ok(false);
        };
        on_click.update(rt, gx, id, v).context("menu item on_click")?;
        Ok(label.update(id, v).context("menu item label")?.is_some()
            | shortcut.update(id, v).context("menu item shortcut")?.is_some()
            | disabled.update(id, v).context("menu item disabled")?.is_some())
    }

    fn desc(&self) -> MenuItemDesc<'_> {
        match self {
            Self::Action { label, shortcut, on_click, disabled } => {
                MenuItemDesc::Action {
                    label: label.t.as_deref().unwrap_or(""),
                    shortcut: shortcut.t.as_ref().and_then(|s| s.as_ref()),
                    callable_id: on_click.id(),
                    disabled: disabled.t.unwrap_or(false),
                }
            }
            Self::Divider => MenuItemDesc::Divider,
        }
    }
}

/// A menu's items property: its ref, and each item beside the value it
/// compiled from, so a new list keeps the items it still holds.
pub(crate) struct MenuItems<X: GXExt> {
    r: Ref<X>,
    items: Vec<(Value, MenuItemKind<X>)>,
}

impl<X: GXExt> MenuItems<X> {
    pub(crate) async fn compile(gx: &GXHandle<X>, r: Ref<X>) -> Result<Self> {
        let items = match r.last.as_ref() {
            None => vec![],
            Some(v) => v.clone().cast_to::<Vec<Value>>()?,
        };
        let items = reconcile([], items, |i| MenuItemKind::compile(gx, i)).await?;
        Ok(Self { r, items })
    }

    pub(crate) fn update(
        &mut self,
        rt: &tokio::runtime::Handle,
        gx: &GXHandle<X>,
        id: ExprId,
        v: &Value,
    ) -> Result<bool> {
        let mut changed = false;
        if id == self.r.id && self.r.last.as_ref() != Some(v) {
            self.r.last = Some(v.clone());
            let items = v.clone().cast_to::<Vec<Value>>()?;
            let old = self.items.drain(..);
            self.items =
                rt.block_on(reconcile(old, items, |i| MenuItemKind::compile(gx, i)))?;
            changed = true;
        }
        for (_, item) in &mut self.items {
            changed |= item.update(rt, gx, id, v)?;
        }
        Ok(changed)
    }

    pub(crate) fn descs(&self) -> Vec<MenuItemDesc<'_>> {
        self.items.iter().map(|(_, i)| i.desc()).collect()
    }
}

struct CompiledMenuGroup<X: GXExt> {
    label: TRef<X, ArcStr>,
    items: MenuItems<X>,
}

impl<X: GXExt> CompiledMenuGroup<X> {
    async fn compile(gx: &GXHandle<X>, v: Value) -> Result<Self> {
        #[derive(FromValue)]
        struct Fields {
            items: u64,
            label: u64,
        }
        let Fields { items, label } = v.cast_to().context("menu group flds")?;
        let (items, label) = try_join! { gx.compile_ref(items), gx.compile_ref(label) }?;
        Ok(Self {
            label: TRef::new(label).context("menu group tref label")?,
            items: MenuItems::compile(gx, items).await.context("menu group items")?,
        })
    }
}

pub(crate) struct MenuBarW<X: GXExt> {
    gx: GXHandle<X>,
    menus_ref: Ref<X>,
    /// Each menu beside the value it compiled from.
    menus: Vec<(Value, CompiledMenuGroup<X>)>,
    width: TRef<X, LengthV>,
}

impl<X: GXExt> MenuBarW<X> {
    pub(crate) async fn compile(gx: GXHandle<X>, source: Value) -> Result<GuiW<X>> {
        #[derive(FromValue)]
        struct Fields {
            menus: u64,
            width: u64,
        }
        let Fields { menus, width } = source.cast_to().context("menu_bar flds")?;
        let (menus_ref, width) =
            try_join! { gx.compile_ref(menus), gx.compile_ref(width) }?;
        let menus = match menus_ref.last.as_ref() {
            None => vec![],
            Some(v) => v.clone().cast_to::<Vec<Value>>()?,
        };
        let menus = reconcile([], menus, |g| CompiledMenuGroup::compile(&gx, g))
            .await
            .context("menu_bar menus")?;
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
        if id == self.menus_ref.id && self.menus_ref.last.as_ref() != Some(v) {
            self.menus_ref.last = Some(v.clone());
            let menus = v.clone().cast_to::<Vec<Value>>()?;
            let (gx, old) = (&self.gx, self.menus.drain(..));
            self.menus = rt
                .block_on(reconcile(old, menus, |g| CompiledMenuGroup::compile(gx, g)))
                .context("menu_bar menus")?;
            changed = true;
        }
        for (_, group) in &mut self.menus {
            changed |= group.label.update(id, v).context("menu group label")?.is_some();
            changed |= group.items.update(rt, &self.gx, id, v)?;
        }
        Ok(changed)
    }

    fn view(&self) -> IcedElement<'_> {
        let descs = self
            .menus
            .iter()
            .map(|(_, g)| MenuGroupDesc {
                label: g.label.t.as_deref().unwrap_or(""),
                items: g.items.descs(),
            })
            .collect();
        let width = self.width.t.as_ref().map_or(Length::Shrink, |w| w.0);
        OwnedMenuBar { descs, width }.into()
    }
}
