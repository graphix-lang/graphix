//! The event loop's frame, drawn: a widget with its callback draws with an
//! enabled status, because the frame delivers the redraw iced widgets take
//! their status from.

use super::{GuiTestHarness, headless_gpu};
use crate::{frame::frame, theme::GraphixTheme, widgets::Renderer};
use ahash::AHashMap;
use anyhow::Result;
use iced_core::{Color, Event, Point, Size, clipboard, mouse};
use iced_runtime::user_interface::Cache;
use iced_wgpu::graphics::Viewport;

const W: u32 = 400;
const H: u32 = 120;

fn button(disabled: bool) -> String {
    format!(
        "use gui::{{button::button, column::column, text::text}};\n\
         let disabled = {disabled};\n\
         let result = column(#padding: &`All(30.0), &[button(\n\
             #on_press: |c| println(c ~ \"clicked\"),\n\
             #width: &`Fixed(100.0),\n\
             #height: &`Fixed(40.0),\n\
             #disabled: &disabled,\n\
             &text(&\"\")\n\
         )])"
    )
}

/// The dominant colour inside the button after one frame.
fn draw(
    h: &mut GuiTestHarness,
    r: &mut Renderer,
    cache: &mut Cache,
    events: &[Event],
    at: Point,
) -> [u8; 3] {
    let theme = GraphixTheme { inner: iced_core::Theme::Dark, overrides: None };
    let mut messages = Vec::new();
    frame(
        &mut h.widget,
        Size::new(W as f32, H as f32),
        cache,
        r,
        events,
        mouse::Cursor::Available(at),
        &mut clipboard::Null,
        &mut messages,
        &theme,
    );
    let rgba =
        r.screenshot(&Viewport::with_physical_size(Size::new(W, H), 1.0), Color::BLACK);
    let mut counts: AHashMap<[u8; 3], usize> = AHashMap::default();
    for y in 35..65 {
        for x in 35..125 {
            let i = ((y * W + x) * 4) as usize;
            *counts.entry([rgba[i], rgba[i + 1], rgba[i + 2]]).or_default() += 1;
        }
    }
    counts.into_iter().max_by_key(|(_, n)| *n).map(|(c, _)| c).unwrap()
}

#[tokio::test(flavor = "current_thread")]
async fn an_enabled_button_draws_enabled_and_hovers() -> Result<()> {
    let mut r = headless_gpu().await.create_renderer();
    let mut on = GuiTestHarness::new(&button(false)).await?;
    let mut off = GuiTestHarness::new(&button(true)).await?;
    let mut cache = Cache::default();
    let idle = draw(&mut on, &mut r, &mut cache, &[], Point::ORIGIN);
    let over = Point::new(80.0, 50.0);
    let moved = [Event::Mouse(mouse::Event::CursorMoved { position: over })];
    let hovered = draw(&mut on, &mut r, &mut cache, &moved, over);
    let disabled = draw(&mut off, &mut r, &mut Cache::default(), &[], Point::ORIGIN);
    assert_ne!(idle, disabled, "an enabled button drew as disabled");
    assert_ne!(idle, hovered, "the button under the cursor drew as idle");
    Ok(())
}

/// iced's text inputs, scrollables and sliders keep their own modifier
/// state, which only a ModifiersChanged event sets.
#[test]
fn modifiers_reach_the_widgets() {
    use winit::{event::WindowEvent, keyboard::ModifiersState};
    let m = winit::event::Modifiers::from(ModifiersState::CONTROL);
    let evs =
        crate::convert::window_event(&WindowEvent::ModifiersChanged(m), 1.0, m.state());
    assert!(matches!(
        &evs[..],
        [Event::Keyboard(iced_core::keyboard::Event::ModifiersChanged(m))] if m.control()
    ));
}

#[test]
fn touch_phases_reach_the_widgets_at_logical_positions() {
    use iced_core::touch::{Event as TouchEvent, Finger};
    use winit::{
        dpi::PhysicalPosition,
        event::{DeviceId, Touch, TouchPhase, WindowEvent},
        keyboard::ModifiersState,
    };
    let id = Finger(42);
    let position = Point::new(25.0, 60.0);
    for (phase, expected) in [
        (TouchPhase::Started, TouchEvent::FingerPressed { id, position }),
        (TouchPhase::Moved, TouchEvent::FingerMoved { id, position }),
        (TouchPhase::Ended, TouchEvent::FingerLifted { id, position }),
        (TouchPhase::Cancelled, TouchEvent::FingerLost { id, position }),
    ] {
        let event = WindowEvent::Touch(Touch {
            device_id: DeviceId::dummy(),
            phase,
            location: PhysicalPosition::new(50.0, 120.0),
            force: None,
            id: id.0,
        });
        let events = crate::convert::window_event(&event, 2.0, ModifiersState::empty());
        assert_eq!(&events[..], &[Event::Touch(expected)], "{phase:?}");
    }
}

async fn assert_deferred_focus(context_menu: bool) -> Result<()> {
    use super::InteractionHarness;
    use crate::widgets::{GuiWidget, IcedElement, Message, WidgetOp};
    use graphix_compiler::expr::ExprId;
    use graphix_rt::NoExt;
    use iced_core::widget::Id;
    use netidx::publisher::Value;

    struct FocusInput {
        id: Id,
        prepared: bool,
        context_menu: bool,
    }

    impl GuiWidget<NoExt> for FocusInput {
        fn handle_update(
            &mut self,
            _: &tokio::runtime::Handle,
            _: ExprId,
            _: &Value,
        ) -> Result<bool> {
            Ok(false)
        }

        fn before_view(&mut self) -> bool {
            self.prepared = true;
            true
        }

        fn take_ops(&mut self, out: &mut Vec<WidgetOp>) {
            if std::mem::take(&mut self.prepared) {
                out.push(WidgetOp::Focus(self.id.clone()));
            }
        }

        fn view(&self) -> IcedElement<'_> {
            let input = iced_widget::text_input("", "")
                .id(self.id.clone())
                .on_input(|_| Message::Nop)
                .into();
            if self.context_menu {
                crate::widgets::context_menu_widget::OwnedContextMenu::new(
                    input,
                    Vec::new(),
                )
                .into()
            } else {
                input
            }
        }
    }

    let mut h = InteractionHarness::new(
        r#"use gui::text::text; let result = text(&"placeholder")"#,
    )
    .await?;
    h.inner.widget =
        Box::new(FocusInput { id: Id::unique(), prepared: false, context_menu });
    assert!(matches!(h.type_text("a").as_slice(), [Message::Nop]));
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn deferred_focus_applies_before_keyboard_input() -> Result<()> {
    assert_deferred_focus(false).await
}

#[tokio::test(flavor = "current_thread")]
async fn context_menu_forwards_widget_operations() -> Result<()> {
    assert_deferred_focus(true).await
}

#[tokio::test(flavor = "current_thread")]
async fn captured_menu_events_publish_nothing() {
    use crate::widgets::menu_bar_widget::{MenuItemDesc, MenuOverlay};
    use graphix_rt::CallableId;
    use iced_core::{Layout, Overlay, Shell};

    let renderer = headless_gpu().await.create_renderer();
    let items = [MenuItemDesc::Action {
        label: "action",
        shortcut: None,
        callable_id: Some(CallableId::new()),
        disabled: false,
    }];
    let mut open = true;
    let mut menu =
        MenuOverlay { items: &items, position: Point::ORIGIN, open: &mut open };
    let node = menu.layout(&renderer, Size::new(400.0, 300.0));
    let mut messages = Vec::new();
    let mut shell = Shell::new(&mut messages);
    shell.capture_event();
    menu.update(
        &Event::Mouse(mouse::Event::ButtonPressed(mouse::Button::Left)),
        Layout::new(&node),
        mouse::Cursor::Available(Point::new(20.0, 15.0)),
        &renderer,
        &mut clipboard::Null,
        &mut shell,
    );
    assert!(messages.is_empty());
    assert!(open);
}
