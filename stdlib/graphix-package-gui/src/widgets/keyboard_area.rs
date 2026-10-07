use super::{
    GuiW, GuiWidget, IcedElement, Message, compile, iced_keyboard_area::KeyboardArea,
};
use anyhow::{Context, Result};
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

/// Convert an iced keyboard event to a graphix Value struct:
/// `{key: string, modifiers: {shift: bool, ctrl: bool, alt: bool, logo: bool}, text: string, repeat: bool}`
// CR claude for claude: [structure] KeyboardArea hands its callbacks the whole
// keyboard::Event, so this function has to rule out ModifiersChanged with
// unreachable!(), even though iced_keyboard_area.rs:139-149 calls it only for
// KeyPressed and KeyReleased. Passing key, modifiers, text and repeat from those two
// arms makes the panic impossible to reach. Named keys are spelled with
// format!("{named:?}"), which makes iced's derived Debug output the key-name contract
// that keyboard_area.md documents ("Enter", "ArrowUp"), and it allocates a String and
// then an ArcStr for every key. Use format_compact!(..).as_str().into(), and pin a few
// names in a test so a rename in iced cannot change them silently. (gui-widgets-a-14)
fn key_event_to_value(event: &keyboard::Event) -> Value {
    let (key, modifiers, text, repeat) = match event {
        keyboard::Event::KeyPressed { key, modifiers, text, repeat, .. } => {
            (key, modifiers, text.as_ref().map(|s| s.as_str()), *repeat)
        }
        keyboard::Event::KeyReleased { key, modifiers, .. } => {
            (key, modifiers, None, false)
        }
        keyboard::Event::ModifiersChanged(_) => unreachable!(),
    };
    let key_str: Value = match key.as_ref() {
        keyboard::Key::Character(c) => Value::String(c.into()),
        keyboard::Key::Named(named) => Value::String(format!("{named:?}").into()),
        keyboard::Key::Unidentified => Value::String("Unidentified".into()),
    };
    let mods_val: Value = [
        (arcstr::literal!("alt"), Value::Bool(modifiers.alt())),
        (arcstr::literal!("ctrl"), Value::Bool(modifiers.control())),
        (arcstr::literal!("logo"), Value::Bool(modifiers.logo())),
        (arcstr::literal!("shift"), Value::Bool(modifiers.shift())),
    ]
    .into();
    let text_val = Value::String(text.unwrap_or("").into());
    [
        (arcstr::literal!("key"), key_str),
        (arcstr::literal!("modifiers"), mods_val),
        (arcstr::literal!("repeat"), Value::Bool(repeat)),
        (arcstr::literal!("text"), text_val),
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
            ka = ka.on_key_press(move |event| {
                Message::Call(id, ValArray::from_iter([key_event_to_value(event)]))
            });
        }
        if let Some(c) = &self.on_key_release_callable {
            let id = c.id();
            ka = ka.on_key_release(move |event| {
                Message::Call(id, ValArray::from_iter([key_event_to_value(event)]))
            });
        }
        ka.into()
    }
}
