use super::{
    GuiW, GuiWidget, IcedElement, Message, compile, iced_keyboard_area::KeyboardArea,
};
use anyhow::{Context, Result};
use arcstr::literal;
use compact_str::format_compact;
use graphix_compiler::expr::ExprId;
use graphix_rt::{Callable, GXExt, GXHandle, Ref};
use iced_core::keyboard;
use netidx::{protocol::valarray::ValArray, publisher::Value};
use netidx_derive::FromValue;
use tokio::try_join;

pub(crate) struct KeyboardAreaW<X: GXExt> {
    gx: GXHandle<X>,
    child_ref: Ref<X>,
    child: GuiW<X>,
    on_key_press: Ref<X>,
    on_key_press_callable: Option<Callable<X>>,
    on_key_release: Ref<X>,
    on_key_release_callable: Option<Callable<X>>,
}

impl<X: GXExt> KeyboardAreaW<X> {
    pub(crate) async fn compile(gx: GXHandle<X>, source: Value) -> Result<GuiW<X>> {
        #[derive(FromValue)]
        struct Fields {
            child: u64,
            on_key_press: u64,
            on_key_release: u64,
        }
        let Fields { child, on_key_press, on_key_release } =
            source.cast_to().context("keyboard_area flds")?;
        let (child_ref, on_key_press, on_key_release) = try_join! {
            gx.compile_ref(child),
            gx.compile_ref(on_key_press),
            gx.compile_ref(on_key_release),
        }?;
        let compiled_child = compile_child!(gx, child_ref, "keyboard_area child");
        let on_key_press_callable =
            compile_callable!(gx, on_key_press, "keyboard_area on_key_press");
        let on_key_release_callable =
            compile_callable!(gx, on_key_release, "keyboard_area on_key_release");
        Ok(Box::new(Self {
            gx: gx.clone(),
            child_ref,
            child: compiled_child,
            on_key_press,
            on_key_press_callable,
            on_key_release,
            on_key_release_callable,
        }))
    }
}

/// The `KeyEvent` a key's callback is called with. A named key is spelled
/// as iced names it (`Enter`, `ArrowUp`): keyboard_area.md documents those.
pub(crate) fn key_event_to_value(
    key: &keyboard::Key,
    modifiers: keyboard::Modifiers,
    text: Option<&str>,
    repeat: bool,
) -> Value {
    let key = match key.as_ref() {
        keyboard::Key::Character(c) => Value::String(c.into()),
        keyboard::Key::Named(named) => {
            Value::String(format_compact!("{named:?}").as_str().into())
        }
        keyboard::Key::Unidentified => Value::String(literal!("Unidentified")),
    };
    let mods: Value = [
        (literal!("alt"), Value::Bool(modifiers.alt())),
        (literal!("ctrl"), Value::Bool(modifiers.control())),
        (literal!("logo"), Value::Bool(modifiers.logo())),
        (literal!("shift"), Value::Bool(modifiers.shift())),
    ]
    .into();
    [
        (literal!("key"), key),
        (literal!("modifiers"), mods),
        (literal!("repeat"), Value::Bool(repeat)),
        (literal!("text"), Value::String(text.unwrap_or("").into())),
    ]
    .into()
}

impl<X: GXExt> GuiWidget<X> for KeyboardAreaW<X> {
    fn for_each_child_mut(&mut self, f: &mut dyn FnMut(&mut GuiW<X>)) {
        f(&mut self.child);
    }

    fn for_each_child(&self, f: &mut dyn FnMut(&GuiW<X>)) {
        f(&self.child);
    }

    fn handle_update(
        &mut self,
        rt: &tokio::runtime::Handle,
        id: ExprId,
        v: &Value,
    ) -> Result<bool> {
        let mut changed = false;
        update_child!(
            self,
            rt,
            id,
            v,
            changed,
            child_ref,
            child,
            "keyboard_area child recompile"
        );
        update_callable!(
            self,
            rt,
            id,
            v,
            on_key_press,
            on_key_press_callable,
            "keyboard_area on_key_press recompile"
        );
        update_callable!(
            self,
            rt,
            id,
            v,
            on_key_release,
            on_key_release_callable,
            "keyboard_area on_key_release recompile"
        );
        Ok(changed)
    }

    fn view(&self) -> IcedElement<'_> {
        let mut ka = KeyboardArea::new(self.child.view());
        if let Some(c) = &self.on_key_press_callable {
            let id = c.id();
            ka = ka.on_key_press(move |key, mods, text, repeat| {
                let ev = key_event_to_value(key, mods, text, repeat);
                Some(Message::Call(id, ValArray::from_iter([ev])))
            });
        }
        if let Some(c) = &self.on_key_release_callable {
            let id = c.id();
            ka = ka.on_key_release(move |key, mods, text, repeat| {
                let ev = key_event_to_value(key, mods, text, repeat);
                Some(Message::Call(id, ValArray::from_iter([ev])))
            });
        }
        ka.into()
    }
}
