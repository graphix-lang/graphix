//! One frame of a window's interface, run the way iced's own shell runs
//! one. The event loop and the test harnesses share it, so a departure
//! from iced's protocol shows in the tests.

use crate::{
    theme::{GraphixTheme, with_view_theme},
    widgets::{GuiW, Message, Renderer, WidgetOp},
};
use graphix_rt::{GXExt, GXHandle};
use iced_core::{Event, Size, clipboard::Clipboard, mouse, renderer::Style, window};
use iced_runtime::user_interface::{Cache, State, UserInterface};
use poolshark::local::LPooled;
use std::{collections::VecDeque, time::Instant};

/// Prepare and build the widgets under the window's theme, apply their operations,
/// deliver input and redraw events, then draw. Published messages land in `messages`.
/// Returns the redraw state, or `Outdated` when an update requires rebuilding.
pub fn frame<X: GXExt>(
    widget: &mut GuiW<X>,
    viewport: Size,
    cache: &mut Cache,
    renderer: &mut Renderer,
    events: &[Event],
    cursor: mouse::Cursor,
    clipboard: &mut dyn Clipboard,
    messages: &mut Vec<Message>,
    theme: &GraphixTheme,
) -> State {
    widget.before_view();
    let mut ops = LPooled::take();
    widget.take_ops(&mut ops);
    with_view_theme(theme, || {
        let mut ui = UserInterface::build(
            widget.view(),
            viewport,
            std::mem::take(cache),
            renderer,
        );
        for op in ops.drain(..) {
            use iced_core::widget::operation::{focusable, scrollable};
            match op {
                WidgetOp::ScrollTo(id, offset) => {
                    ui.operate(renderer, &mut scrollable::scroll_to::<()>(id, offset))
                }
                WidgetOp::Focus(id) => {
                    ui.operate(renderer, &mut focusable::focus::<()>(id))
                }
            }
        }
        let (on_events, _) = ui.update(events, cursor, renderer, clipboard, messages);
        let redraw = [Event::Window(window::Event::RedrawRequested(Instant::now()))];
        let (on_redraw, _) = ui.update(&redraw, cursor, renderer, clipboard, messages);
        let style = Style { text_color: theme.palette().text };
        ui.draw(renderer, theme, &style, cursor);
        *cache = ui.into_cache();
        match on_events {
            State::Outdated => State::Outdated,
            State::Updated { .. } => on_redraw,
        }
    })
}

/// Apply what widgets published, in order: every message goes to
/// `deliver`, which hands it to the widgets and queues what they publish
/// in turn, and a call then goes to the runtime.
pub fn apply_messages<X: GXExt>(
    gx: &GXHandle<X>,
    messages: impl IntoIterator<Item = Message>,
    mut deliver: impl FnMut(&Message, &mut VecDeque<Message>),
) {
    let mut pending: VecDeque<Message> = messages.into_iter().collect();
    while let Some(msg) = pending.pop_front() {
        match msg {
            Message::Nop => {}
            Message::Call(..) => {
                deliver(&msg, &mut pending);
                if let Message::Call(id, args) = msg
                    && let Err(e) = gx.call(id, args)
                {
                    log::error!("failed to call: {e:?}");
                }
            }
            other => deliver(&other, &mut pending),
        }
    }
}
