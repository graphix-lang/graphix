use super::{Child, GuiW, GuiWidget, Handler, IcedElement, Message};
use anyhow::{Context, Result};
use graphix_compiler::expr::ExprId;
use graphix_rt::{GXExt, GXHandle};
use iced_widget as widget;
use netidx::{protocol::valarray::ValArray, publisher::Value};
use netidx_derive::FromValue;
use tokio::try_join;

fn mouse_button_value(button: &str) -> Value {
    Value::String(button.into())
}

pub(crate) struct MouseAreaW<X: GXExt> {
    gx: GXHandle<X>,
    child: Child<X>,
    on_press: Handler<X>,
    on_release: Handler<X>,
    on_enter: Handler<X>,
    on_exit: Handler<X>,
    on_move: Handler<X>,
}

impl<X: GXExt> MouseAreaW<X> {
    pub(crate) async fn compile(gx: GXHandle<X>, source: Value) -> Result<GuiW<X>> {
        #[derive(FromValue)]
        struct Fields {
            child: u64,
            on_enter: u64,
            on_exit: u64,
            on_move: u64,
            on_press: u64,
            on_release: u64,
        }
        let Fields { child, on_enter, on_exit, on_move, on_press, on_release } =
            source.cast_to().context("mouse_area flds")?;
        let (child_ref, on_enter, on_exit, on_move, on_press, on_release) = try_join! {
            gx.compile_ref(child),
            gx.compile_ref(on_enter),
            gx.compile_ref(on_exit),
            gx.compile_ref(on_move),
            gx.compile_ref(on_press),
            gx.compile_ref(on_release),
        }?;
        let child = Child::compile(&gx, child_ref).await.context("mouse_area child")?;
        let on_press =
            Handler::compile(&gx, on_press).await.context("mouse_area on_press")?;
        let on_release =
            Handler::compile(&gx, on_release).await.context("mouse_area on_release")?;
        let on_enter =
            Handler::compile(&gx, on_enter).await.context("mouse_area on_enter")?;
        let on_exit =
            Handler::compile(&gx, on_exit).await.context("mouse_area on_exit")?;
        let on_move =
            Handler::compile(&gx, on_move).await.context("mouse_area on_move")?;
        Ok(Box::new(Self {
            gx: gx.clone(),
            child,
            on_press,
            on_release,
            on_enter,
            on_exit,
            on_move,
        }))
    }
}

impl<X: GXExt> GuiWidget<X> for MouseAreaW<X> {
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
            .context("mouse_area child recompile")?;
        self.on_press
            .update(rt, &self.gx, id, v)
            .context("mouse_area on_press recompile")?;
        self.on_release
            .update(rt, &self.gx, id, v)
            .context("mouse_area on_release recompile")?;
        self.on_enter
            .update(rt, &self.gx, id, v)
            .context("mouse_area on_enter recompile")?;
        self.on_exit
            .update(rt, &self.gx, id, v)
            .context("mouse_area on_exit recompile")?;
        self.on_move
            .update(rt, &self.gx, id, v)
            .context("mouse_area on_move recompile")?;
        Ok(changed)
    }

    fn view(&self) -> IcedElement<'_> {
        let mut ma = widget::MouseArea::new(self.child.w.view());
        if let Some(c) = &self.on_press.f {
            let id = c.id();
            ma = ma.on_press(Message::Call(
                id,
                ValArray::from_iter([mouse_button_value("Left")]),
            ));
            ma = ma.on_right_press(Message::Call(
                id,
                ValArray::from_iter([mouse_button_value("Right")]),
            ));
            ma = ma.on_middle_press(Message::Call(
                id,
                ValArray::from_iter([mouse_button_value("Middle")]),
            ));
        }
        if let Some(c) = &self.on_release.f {
            let id = c.id();
            ma = ma.on_release(Message::Call(
                id,
                ValArray::from_iter([mouse_button_value("Left")]),
            ));
            ma = ma.on_right_release(Message::Call(
                id,
                ValArray::from_iter([mouse_button_value("Right")]),
            ));
            ma = ma.on_middle_release(Message::Call(
                id,
                ValArray::from_iter([mouse_button_value("Middle")]),
            ));
        }
        if let Some(c) = &self.on_enter.f {
            ma = ma.on_enter(Message::Call(c.id(), ValArray::from_iter([Value::Null])));
        }
        if let Some(c) = &self.on_exit.f {
            ma = ma.on_exit(Message::Call(c.id(), ValArray::from_iter([Value::Null])));
        }
        if let Some(c) = &self.on_move.f {
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
