use super::{
    Children, GuiW, Handler, IcedElement, Item,
    menu_bar_widget::{MenuGroupDesc, MenuItemDesc, OwnedMenuBar},
};
use crate::types::{LengthV, ShortcutV};
use anyhow::{Context, Result};
use arcstr::ArcStr;
use futures::try_join;
use graphix_compiler::expr::ExprId;
use graphix_rt::{GXExt, GXHandle, TRef};
use iced_core::Length;
use netidx::publisher::Value;
use netidx_derive::FromValue;

graphix_rt::props! {
    pub(crate) struct ActionProps {
        disabled: bool,
        label: ArcStr,
        shortcut: Option<ShortcutV>,
    }
}

pub(crate) enum MenuItemKind<X: GXExt> {
    Action { p: ActionProps<X>, on_click: Handler<X> },
    Divider,
}

impl<X: GXExt> Item<X> for MenuItemKind<X> {
    async fn compile(gx: &GXHandle<X>, v: Value) -> Result<Self> {
        #[derive(FromValue)]
        #[value(shape = "graphix_compiler::rep::GraphixShape")]
        enum Repr {
            Divider,
            Action(Value),
        }
        match v.cast_to::<Repr>().context("menu item")? {
            Repr::Divider => Ok(Self::Divider),
            Repr::Action(v) => {
                let (p, on_click) = try_join!(
                    ActionProps::compile(gx, &v),
                    Handler::field(gx, &v, "on_click"),
                )
                .context("menu action")?;
                Ok(Self::Action { p, on_click })
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
        let Self::Action { p, on_click } = self else {
            return Ok(false);
        };
        let mut changed = p.update(id, v).context("menu item")?;
        on_click.update(rt, gx, id, v, &mut changed).context("menu item on_click")?;
        Ok(changed)
    }
}

impl<X: GXExt> MenuItemKind<X> {
    fn desc(&self) -> MenuItemDesc<'_> {
        match self {
            Self::Action { p, on_click } => MenuItemDesc::Action {
                label: p.label.t.as_deref().unwrap_or(""),
                shortcut: p.shortcut.t.as_ref().and_then(|s| s.as_ref()),
                callable_id: on_click.id(),
                disabled: p.disabled.t.unwrap_or(false),
            },
            Self::Divider => MenuItemDesc::Divider,
        }
    }
}

/// A menu's items property.
pub(crate) type MenuItems<X> = Children<X, MenuItemKind<X>>;

impl<X: GXExt> MenuItems<X> {
    pub(crate) fn descs(&self) -> Vec<MenuItemDesc<'_>> {
        self.ws.iter().map(|(_, i)| i.desc()).collect()
    }
}

struct CompiledMenuGroup<X: GXExt> {
    label: TRef<X, ArcStr>,
    items: MenuItems<X>,
}

impl<X: GXExt> Item<X> for CompiledMenuGroup<X> {
    async fn compile(gx: &GXHandle<X>, v: Value) -> Result<Self> {
        let (items, label) =
            try_join!(gx.compile_field(&v, "items"), gx.compile_field(&v, "label"))
                .context("menu group")?;
        Ok(Self {
            label: TRef::new(label).context("menu group tref label")?,
            items: MenuItems::compile(gx, items).await.context("menu group items")?,
        })
    }

    fn update(
        &mut self,
        rt: &tokio::runtime::Handle,
        gx: &GXHandle<X>,
        id: ExprId,
        v: &Value,
    ) -> Result<bool> {
        let changed = self.label.update(id, v).context("menu group label")?.is_some();
        Ok(self.items.update(rt, gx, id, v)? || changed)
    }
}

pub(crate) struct MenuBarW<X: GXExt> {
    gx: GXHandle<X>,
    menus: Children<X, CompiledMenuGroup<X>>,
    width: TRef<X, LengthV>,
}

impl<X: GXExt> MenuBarW<X> {
    pub(crate) async fn compile(gx: GXHandle<X>, source: Value) -> Result<GuiW<X>> {
        let (menus, width) = try_join!(
            Children::field(&gx, &source, "menus"),
            gx.compile_field(&source, "width"),
        )
        .context("menu_bar")?;
        Ok(GuiW::new(Self {
            gx: gx.clone(),
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
        let changed =
            self.width.update(id, v).context("menu_bar update width")?.is_some();
        Ok(self.menus.update(rt, &self.gx, id, v).context("menu_bar menus")? || changed)
    }

    fn view(&self) -> IcedElement<'_> {
        let descs = self
            .menus
            .ws
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
