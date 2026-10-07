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
    h.widget.before_view();
    frame(
        h.widget.view(),
        Size::new(W as f32, H as f32),
        cache,
        r,
        &mut Vec::new(),
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
