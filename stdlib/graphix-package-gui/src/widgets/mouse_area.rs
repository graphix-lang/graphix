use super::{Child, GuiW, GuiWidget, Handler, IcedElement, Message};
use anyhow::{Context, Result};
use futures::try_join;
use graphix_compiler::expr::ExprId;
use graphix_rt::{GXExt, GXHandle};
use iced_widget as widget;
use netidx::{protocol::valarray::ValArray, publisher::Value};

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
        let (child, on_press, on_release, on_enter, on_exit, on_move) = try_join!(
            Child::field(&gx, &source, "child"),
            Handler::field(&gx, &source, "on_press"),
            Handler::field(&gx, &source, "on_release"),
            Handler::field(&gx, &source, "on_enter"),
            Handler::field(&gx, &source, "on_exit"),
            Handler::field(&gx, &source, "on_move"),
        )
        .context("mouse_area")?;
        Ok(Box::new(Self { gx, child, on_press, on_release, on_enter, on_exit, on_move }))
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
