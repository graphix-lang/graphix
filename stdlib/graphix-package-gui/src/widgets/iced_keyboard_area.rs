use iced_core::{
    Clipboard, Element, Event, Length, Rectangle, Shell, Size, Vector, Widget, keyboard,
    layout::{self, Layout},
    mouse, overlay, renderer,
    widget::{
        Operation,
        tree::{self, Tree},
    },
};

use super::{Message, Renderer};

/// A container that takes keyboard events while focused: a left click
/// inside it focuses it, one outside unfocuses it. Focus operations pass
/// it by, so focusing a text input inside it keeps it focused.
pub(crate) struct KeyboardArea<'a> {
    content: Element<'a, Message, crate::theme::GraphixTheme, Renderer>,
    on_key_press: Option<KeyFn<'a>>,
    on_key_release: Option<KeyFn<'a>>,
    on_pointer: Option<Pointer<'a>>,
    keys_first: bool,
}

/// A key's handler: the key, its modifiers, the text it typed and whether
/// it repeats. `None` leaves the key to whatever encloses the area.
type KeyFn<'a> = Box<
    dyn Fn(&keyboard::Key, keyboard::Modifiers, Option<&str>, bool) -> Option<Message>
        + 'a,
>;

/// A drag's hooks: every left press and cursor move (x within the area)
/// and every left release, wherever the cursor is.
struct Pointer<'a> {
    on_move: Box<dyn Fn(f32) -> Message + 'a>,
    on_release: Message,
}

#[derive(Default)]
struct State {
    is_focused: bool,
}

impl<'a> KeyboardArea<'a> {
    pub(crate) fn new(
        content: impl Into<Element<'a, Message, crate::theme::GraphixTheme, Renderer>>,
    ) -> Self {
        Self {
            content: content.into(),
            on_key_press: None,
            on_key_release: None,
            on_pointer: None,
            keys_first: false,
        }
    }

    #[must_use]
    pub(crate) fn on_key_press(
        mut self,
        f: impl Fn(&keyboard::Key, keyboard::Modifiers, Option<&str>, bool) -> Option<Message>
        + 'a,
    ) -> Self {
        self.on_key_press = Some(Box::new(f));
        self
    }

    #[must_use]
    pub(crate) fn on_key_release(
        mut self,
        f: impl Fn(&keyboard::Key, keyboard::Modifiers, Option<&str>, bool) -> Option<Message>
        + 'a,
    ) -> Self {
        self.on_key_release = Some(Box::new(f));
        self
    }

    /// Offer keys to the handlers before the content, which then sees only
    /// the keys they leave.
    #[must_use]
    pub(crate) fn keys_first(mut self) -> Self {
        self.keys_first = true;
        self
    }

    /// The message a handler makes of `event`, if it takes it.
    fn key_message(&self, event: &Event) -> Option<Message> {
        match event {
            Event::Keyboard(keyboard::Event::KeyPressed {
                key,
                modifiers,
                text,
                repeat,
                ..
            }) => self.on_key_press.as_ref().and_then(|f| {
                f(key, *modifiers, text.as_ref().map(|t| t.as_str()), *repeat)
            }),
            Event::Keyboard(keyboard::Event::KeyReleased { key, modifiers, .. }) => {
                self.on_key_release.as_ref().and_then(|f| f(key, *modifiers, None, false))
            }
            _ => None,
        }
    }

    /// Follow a drag: the press that starts it, every cursor move and the
    /// left release that ends it, even outside the area's bounds.
    #[must_use]
    pub(crate) fn on_pointer(
        mut self,
        on_move: impl Fn(f32) -> Message + 'a,
        on_release: Message,
    ) -> Self {
        self.on_pointer = Some(Pointer { on_move: Box::new(on_move), on_release });
        self
    }
}

impl Widget<Message, crate::theme::GraphixTheme, Renderer> for KeyboardArea<'_> {
    fn tag(&self) -> tree::Tag {
        tree::Tag::of::<State>()
    }

    fn state(&self) -> tree::State {
        tree::State::new(State::default())
    }

    fn children(&self) -> Vec<Tree> {
        vec![Tree::new(&self.content)]
    }

    fn diff(&self, tree: &mut Tree) {
        tree.diff_children(std::slice::from_ref(&self.content));
    }

    fn size(&self) -> Size<Length> {
        self.content.as_widget().size()
    }

    fn layout(
        &mut self,
        tree: &mut Tree,
        renderer: &Renderer,
        limits: &layout::Limits,
    ) -> layout::Node {
        self.content.as_widget_mut().layout(&mut tree.children[0], renderer, limits)
    }

    fn operate(
        &mut self,
        tree: &mut Tree,
        layout: Layout<'_>,
        renderer: &Renderer,
        operation: &mut dyn Operation,
    ) {
        self.content.as_widget_mut().operate(
            &mut tree.children[0],
            layout,
            renderer,
            operation,
        );
    }

    fn update(
        &mut self,
        tree: &mut Tree,
        event: &Event,
        layout: Layout<'_>,
        cursor: mouse::Cursor,
        renderer: &Renderer,
        clipboard: &mut dyn Clipboard,
        shell: &mut Shell<'_, Message>,
        viewport: &Rectangle,
    ) {
        let state: &mut State = tree.state.downcast_mut();
        if let Event::Mouse(mouse::Event::ButtonPressed(mouse::Button::Left)) = event {
            if cursor.is_over(layout.bounds()) {
                state.is_focused = true;
            } else {
                state.is_focused = false;
            }
        }
        if self.keys_first
            && state.is_focused
            && let Some(msg) = self.key_message(event)
        {
            shell.publish(msg);
            shell.capture_event();
            return;
        }

        self.content.as_widget_mut().update(
            &mut tree.children[0],
            event,
            layout,
            cursor,
            renderer,
            clipboard,
            shell,
            viewport,
        );

        if let Some(p) = &self.on_pointer {
            match event {
                Event::Mouse(mouse::Event::CursorMoved { position }) => {
                    shell.publish((p.on_move)(position.x - layout.bounds().x))
                }
                Event::Mouse(mouse::Event::ButtonPressed(mouse::Button::Left)) => {
                    if let Some(position) = cursor.position() {
                        shell.publish((p.on_move)(position.x - layout.bounds().x))
                    }
                }
                Event::Mouse(mouse::Event::ButtonReleased(mouse::Button::Left)) => {
                    shell.publish(p.on_release.clone())
                }
                _ => (),
            }
        }

        if shell.is_event_captured() {
            return;
        }

        if !self.keys_first
            && state.is_focused
            && let Some(msg) = self.key_message(event)
        {
            shell.publish(msg);
            shell.capture_event();
        }
    }

    fn mouse_interaction(
        &self,
        tree: &Tree,
        layout: Layout<'_>,
        cursor: mouse::Cursor,
        viewport: &Rectangle,
        renderer: &Renderer,
    ) -> mouse::Interaction {
        self.content.as_widget().mouse_interaction(
            &tree.children[0],
            layout,
            cursor,
            viewport,
            renderer,
        )
    }

    fn draw(
        &self,
        tree: &Tree,
        renderer: &mut Renderer,
        theme: &crate::theme::GraphixTheme,
        style: &renderer::Style,
        layout: Layout<'_>,
        cursor: mouse::Cursor,
        viewport: &Rectangle,
    ) {
        self.content.as_widget().draw(
            &tree.children[0],
            renderer,
            theme,
            style,
            layout,
            cursor,
            viewport,
        );
    }

    fn overlay<'b>(
        &'b mut self,
        tree: &'b mut Tree,
        layout: Layout<'b>,
        renderer: &Renderer,
        viewport: &Rectangle,
        translation: Vector,
    ) -> Option<overlay::Element<'b, Message, crate::theme::GraphixTheme, Renderer>> {
        self.content.as_widget_mut().overlay(
            &mut tree.children[0],
            layout,
            renderer,
            viewport,
            translation,
        )
    }
}

impl<'a> From<KeyboardArea<'a>>
    for Element<'a, Message, crate::theme::GraphixTheme, Renderer>
{
    fn from(area: KeyboardArea<'a>) -> Self {
        Element::new(area)
    }
}
