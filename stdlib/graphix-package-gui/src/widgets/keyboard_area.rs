use super::{
    compile, iced_keyboard_area::KeyboardArea, GuiW, GuiWidget, IcedElement, Message,
};
use anyhow::{Context, Result};
use arcstr::ArcStr;
use graphix_compiler::expr::ExprId;
use graphix_rt::{Callable, CallableId, GXExt, GXHandle, Ref};
use iced_core::keyboard;
use netidx::{protocol::valarray::ValArray, publisher::Value};
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
        let [(_, child), (_, on_key_press), (_, on_key_release)] =
            source.cast_to::<[(ArcStr, u64); 3]>().context("keyboard_area flds")?;
        let (child_ref, on_key_press, on_key_release) = try_join! {
            gx.compile_ref(child),
            gx.compile_ref(on_key_press),
            gx.compile_ref(on_key_release),
        }?;
        let compiled_child: GuiW<X> = match child_ref.last.as_ref() {
            None => Box::new(super::EmptyW),
            Some(v) => {
                compile(gx.clone(), v.clone()).await.context("keyboard_area child")?
            }
        };
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
    fn handle_update(
        &mut self,
        rt: &tokio::runtime::Handle,
        id: ExprId,
        v: &Value,
    ) -> Result<bool> {
        let mut changed = false;
        if id == self.child_ref.id {
            self.child_ref.last = Some(v.clone());
            self.child = rt
                .block_on(compile(self.gx.clone(), v.clone()))
                .context("keyboard_area child recompile")?;
            changed = true;
        }
        changed |= self.child.handle_update(rt, id, v)?;
        update_callable!(
            self,
            rt,
            id,
            v,
            on_key_press,
            on_key_press_callable,
            "keyboard_area on_key_press"
        );
        update_callable!(
            self,
            rt,
            id,
            v,
            on_key_release,
            on_key_release_callable,
            "keyboard_area on_key_release"
        );
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
