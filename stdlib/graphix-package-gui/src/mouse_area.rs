use crate::{
    compile,
    GuiW, GuiWidget, IcedElement, Message,
};
use anyhow::{Context, Result};
use arcstr::ArcStr;
use graphix_compiler::{expr::ExprId, BindId};
use graphix_rt::{Callable, GXExt, GXHandle, Ref};
use iced_widget as widget;
use netidx::publisher::Value;
use tokio::try_join;

pub(crate) struct MouseAreaW<X: GXExt> {
    gx: GXHandle<X>,
    child_ref: Ref<X>,
    child: GuiW<X>,
    on_press: Ref<X>,
    on_press_callable: Option<Callable<X>>,
    on_release: Ref<X>,
    on_release_callable: Option<Callable<X>>,
    on_enter: Ref<X>,
    on_enter_callable: Option<Callable<X>>,
    on_exit: Ref<X>,
    on_exit_callable: Option<Callable<X>>,
}

impl<X: GXExt> MouseAreaW<X> {
    pub(crate) async fn compile(gx: GXHandle<X>, source: Value) -> Result<GuiW<X>> {
        let [(_, child), (_, on_enter), (_, on_exit), (_, on_press), (_, on_release)] =
            source.cast_to::<[(ArcStr, u64); 5]>().context("mouse_area flds")?;
        let (child_ref, on_enter, on_exit, on_press, on_release) = try_join! {
            gx.compile_ref(child),
            gx.compile_ref(on_enter),
            gx.compile_ref(on_exit),
            gx.compile_ref(on_press),
            gx.compile_ref(on_release),
        }?;
        let compiled_child: GuiW<X> = match child_ref.last.as_ref() {
            None => Box::new(crate::EmptyW),
            Some(v) => compile(gx.clone(), v.clone()).await.context("mouse_area child")?,
        };
        let on_press_callable = match on_press.last.as_ref() {
            Some(v) => Some(gx.compile_callable(v.clone()).await.context("mouse_area on_press")?),
            None => None,
        };
        let on_release_callable = match on_release.last.as_ref() {
            Some(v) => Some(gx.compile_callable(v.clone()).await.context("mouse_area on_release")?),
            None => None,
        };
        let on_enter_callable = match on_enter.last.as_ref() {
            Some(v) => Some(gx.compile_callable(v.clone()).await.context("mouse_area on_enter")?),
            None => None,
        };
        let on_exit_callable = match on_exit.last.as_ref() {
            Some(v) => Some(gx.compile_callable(v.clone()).await.context("mouse_area on_exit")?),
            None => None,
        };
        Ok(Box::new(Self {
            gx: gx.clone(),
            child_ref,
            child: compiled_child,
            on_press,
            on_press_callable,
            on_release,
            on_release_callable,
            on_enter,
            on_enter_callable,
            on_exit,
            on_exit_callable,
        }))
    }
}

impl<X: GXExt> GuiWidget<X> for MouseAreaW<X> {
    fn handle_update(
        &mut self,
        rt: &tokio::runtime::Handle,
        id: ExprId,
        v: &Value,
    ) -> Result<bool> {
        let mut changed = false;
        if id == self.child_ref.id {
            self.child_ref.last = Some(v.clone());
            self.child = rt.block_on(compile(self.gx.clone(), v.clone()))
                .context("mouse_area child recompile")?;
            changed = true;
        }
        changed |= self.child.handle_update(rt, id, v)?;
        macro_rules! update_callable {
            ($field:ident, $callable:ident, $label:literal) => {
                if id == self.$field.id {
                    self.$field.last = Some(v.clone());
                    self.$callable = Some(
                        rt.block_on(self.gx.compile_callable(v.clone()))
                            .context(concat!("mouse_area ", $label, " recompile"))?,
                    );
                }
            };
        }
        update_callable!(on_press, on_press_callable, "on_press");
        update_callable!(on_release, on_release_callable, "on_release");
        update_callable!(on_enter, on_enter_callable, "on_enter");
        update_callable!(on_exit, on_exit_callable, "on_exit");
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
        let mut ma = widget::MouseArea::new(self.child.view());
        if let Some(c) = &self.on_press_callable {
            ma = ma.on_press(Message::Call(c.id()));
        }
        if let Some(c) = &self.on_release_callable {
            ma = ma.on_release(Message::Call(c.id()));
        }
        if let Some(c) = &self.on_enter_callable {
            ma = ma.on_enter(Message::Call(c.id()));
        }
        if let Some(c) = &self.on_exit_callable {
            ma = ma.on_exit(Message::Call(c.id()));
        }
        ma.into()
    }
}
