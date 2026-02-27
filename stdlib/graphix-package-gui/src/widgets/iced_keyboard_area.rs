use iced_core::{
    keyboard,
    layout::{self, Layout},
    mouse,
    overlay,
    renderer,
    widget::{
        tree::{self, Tree},
        Operation,
    },
    Clipboard, Element, Event, Length, Rectangle, Shell, Size, Vector, Widget,
};

use super::{Message, Renderer};

/// A container that captures keyboard events when focused.
///
/// Gains focus on mouse click inside bounds, loses focus on click
/// outside. Participates in tab-order focus traversal.
pub(crate) struct KeyboardArea<'a> {
    content: Element<'a, Message, iced_core::Theme, Renderer>,
    on_key_press: Option<Box<dyn Fn(&keyboard::Event) -> Message + 'a>>,
    on_key_release: Option<Box<dyn Fn(&keyboard::Event) -> Message + 'a>>,
}

#[derive(Default)]
struct State {
    is_focused: bool,
}

impl<'a> KeyboardArea<'a> {
    pub(crate) fn new(
        content: impl Into<Element<'a, Message, iced_core::Theme, Renderer>>,
    ) -> Self {
        Self {
            content: content.into(),
            on_key_press: None,
            on_key_release: None,
        }
    }

    #[must_use]
    pub(crate) fn on_key_press(
        mut self,
        f: impl Fn(&keyboard::Event) -> Message + 'a,
    ) -> Self {
        self.on_key_press = Some(Box::new(f));
        self
    }

    #[must_use]
    pub(crate) fn on_key_release(
        mut self,
        f: impl Fn(&keyboard::Event) -> Message + 'a,
    ) -> Self {
        self.on_key_release = Some(Box::new(f));
        self
    }
}

impl Widget<Message, iced_core::Theme, Renderer> for KeyboardArea<'_> {
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
        self.content
            .as_widget_mut()
            .layout(&mut tree.children[0], renderer, limits)
    }

    fn operate(
        &mut self,
        tree: &mut Tree,
        layout: Layout<'_>,
        renderer: &Renderer,
        operation: &mut dyn Operation,
    ) {
        let state: &mut State = tree.state.downcast_mut();
        operation.focusable(None, layout.bounds(), state);
        self.content
            .as_widget_mut()
            .operate(&mut tree.children[0], layout, renderer, operation);
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

        if shell.is_event_captured() {
            return;
        }

        let state: &mut State = tree.state.downcast_mut();

        match event {
            Event::Mouse(mouse::Event::ButtonPressed(mouse::Button::Left)) => {
                if cursor.is_over(layout.bounds()) {
                    state.is_focused = true;
                } else {
                    state.is_focused = false;
                }
            }
            Event::Keyboard(kb_event) if state.is_focused => match kb_event {
                keyboard::Event::KeyPressed { .. } => {
                    if let Some(f) = &self.on_key_press {
                        shell.publish(f(kb_event));
                        shell.capture_event();
                    }
                }
                keyboard::Event::KeyReleased { .. } => {
                    if let Some(f) = &self.on_key_release {
                        shell.publish(f(kb_event));
                        shell.capture_event();
                    }
                }
                keyboard::Event::ModifiersChanged(_) => {}
            },
            _ => {}
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
        theme: &iced_core::Theme,
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
    ) -> Option<overlay::Element<'b, Message, iced_core::Theme, Renderer>> {
        self.content.as_widget_mut().overlay(
            &mut tree.children[0],
            layout,
            renderer,
            viewport,
            translation,
        )
    }
}

impl iced_core::widget::operation::focusable::Focusable for State {
    fn is_focused(&self) -> bool {
        self.is_focused
    }

    fn focus(&mut self) {
        self.is_focused = true;
    }

    fn unfocus(&mut self) {
        self.is_focused = false;
    }
}

impl<'a> From<KeyboardArea<'a>>
    for Element<'a, Message, iced_core::Theme, Renderer>
{
    fn from(area: KeyboardArea<'a>) -> Self {
        Element::new(area)
    }
}
