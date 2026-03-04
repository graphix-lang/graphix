use super::{compile, GuiW, GuiWidget, IcedElement, Message};
use anyhow::{Context, Result};
use arcstr::ArcStr;
use graphix_compiler::expr::ExprId;
use graphix_rt::{Callable, CallableId, GXExt, GXHandle, Ref};
use iced_widget as widget;
use netidx::{protocol::valarray::ValArray, publisher::Value};
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
    on_move: Ref<X>,
    on_move_callable: Option<Callable<X>>,
}

impl<X: GXExt> MouseAreaW<X> {
    pub(crate) async fn compile(gx: GXHandle<X>, source: Value) -> Result<GuiW<X>> {
        let [(_, child), (_, on_enter), (_, on_exit), (_, on_move), (_, on_press), (_, on_release)] =
            source.cast_to::<[(ArcStr, u64); 6]>().context("mouse_area flds")?;
        let (child_ref, on_enter, on_exit, on_move, on_press, on_release) = try_join! {
            gx.compile_ref(child),
            gx.compile_ref(on_enter),
            gx.compile_ref(on_exit),
            gx.compile_ref(on_move),
            gx.compile_ref(on_press),
            gx.compile_ref(on_release),
        }?;
        let compiled_child = compile_child!(gx, child_ref, "mouse_area child");
        let on_press_callable = compile_callable!(gx, on_press, "mouse_area on_press");
        let on_release_callable =
            compile_callable!(gx, on_release, "mouse_area on_release");
        let on_enter_callable = compile_callable!(gx, on_enter, "mouse_area on_enter");
        let on_exit_callable = compile_callable!(gx, on_exit, "mouse_area on_exit");
        let on_move_callable = compile_callable!(gx, on_move, "mouse_area on_move");
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
            on_move,
            on_move_callable,
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
        update_child!(self, rt, id, v, changed, child_ref, child, "mouse_area child recompile");
        update_callable!(self, rt, id, v, on_press, on_press_callable, "mouse_area on_press recompile");
        update_callable!(self, rt, id, v, on_release, on_release_callable, "mouse_area on_release recompile");
        update_callable!(self, rt, id, v, on_enter, on_enter_callable, "mouse_area on_enter recompile");
        update_callable!(self, rt, id, v, on_exit, on_exit_callable, "mouse_area on_exit recompile");
        update_callable!(self, rt, id, v, on_move, on_move_callable, "mouse_area on_move recompile");
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
        let mut ma = widget::MouseArea::new(self.child.view());
        if let Some(c) = &self.on_press_callable {
            ma = ma.on_press(Message::Call(c.id(), ValArray::from_iter([Value::Null])));
        }
        if let Some(c) = &self.on_release_callable {
            ma = ma.on_release(Message::Call(c.id(), ValArray::from_iter([Value::Null])));
        }
        if let Some(c) = &self.on_enter_callable {
            ma = ma.on_enter(Message::Call(c.id(), ValArray::from_iter([Value::Null])));
        }
        if let Some(c) = &self.on_exit_callable {
            ma = ma.on_exit(Message::Call(c.id(), ValArray::from_iter([Value::Null])));
        }
        if let Some(c) = &self.on_move_callable {
            let id = c.id();
            ma = ma.on_move(move |point| {
                let point_val: Value = [
                    (arcstr::literal!("x"), Value::F64(point.x as f64)),
                    (arcstr::literal!("y"), Value::F64(point.y as f64)),
                ]
                .into();
                Message::Call(id, ValArray::from_iter([point_val]))
            });
        }
        ma.into()
    }
}
