use iced_core::{
    Clipboard, Element, Event, Layout, Length, Point, Rectangle, Shell, Size, Vector,
    Widget, keyboard, layout, mouse, overlay, renderer, touch, widget,
};

use super::{
    Message, Renderer,
    menu_bar_widget::{MenuItemDesc, MenuOverlay},
};
use crate::theme::GraphixTheme;

#[derive(Default)]
struct State {
    open: bool,
    position: Point,
}

/// An iced widget that wraps a child and shows a dropdown menu on
/// right-click (context menu). Uses `MenuOverlay` for rendering.
pub(crate) struct OwnedContextMenu<'a> {
    child: Element<'a, Message, GraphixTheme, Renderer>,
    items: Vec<MenuItemDesc<'a>>,
}

impl<'a> OwnedContextMenu<'a> {
    pub fn new(
        child: Element<'a, Message, GraphixTheme, Renderer>,
        items: Vec<MenuItemDesc<'a>>,
    ) -> Self {
        Self { child, items }
    }
}

impl<'a> Widget<Message, GraphixTheme, Renderer> for OwnedContextMenu<'a> {
    fn tag(&self) -> widget::tree::Tag {
        widget::tree::Tag::of::<State>()
    }

    fn state(&self) -> widget::tree::State {
        widget::tree::State::new(State::default())
    }

    fn children(&self) -> Vec<widget::Tree> {
        vec![widget::Tree::new(&self.child)]
    }

    fn diff(&self, tree: &mut widget::Tree) {
        tree.diff_children(std::slice::from_ref(&self.child));
    }

    fn size(&self) -> Size<Length> {
        self.child.as_widget().size()
    }

    fn layout(
        &mut self,
        tree: &mut widget::Tree,
        renderer: &Renderer,
        limits: &layout::Limits,
    ) -> layout::Node {
        self.child.as_widget_mut().layout(&mut tree.children[0], renderer, limits)
    }

    fn draw(
        &self,
        tree: &widget::Tree,
        renderer: &mut Renderer,
        theme: &GraphixTheme,
        style: &renderer::Style,
        layout: Layout<'_>,
        cursor: mouse::Cursor,
        viewport: &Rectangle,
    ) {
        self.child.as_widget().draw(
            &tree.children[0],
            renderer,
            theme,
            style,
            layout,
            cursor,
            viewport,
        );
    }

    fn update(
        &mut self,
        tree: &mut widget::Tree,
        event: &Event,
        layout: Layout<'_>,
        cursor: mouse::Cursor,
        renderer: &Renderer,
        clipboard: &mut dyn Clipboard,
        shell: &mut Shell<'_, Message>,
        viewport: &Rectangle,
    ) {
        self.child.as_widget_mut().update(
            &mut tree.children[0],
            event,
            layout,
            cursor,
            renderer,
            clipboard,
            shell,
            viewport,
        );
        let state = tree.state.downcast_mut::<State>();
        match event {
            // a menu inside this one may have taken the click
            Event::Mouse(mouse::Event::ButtonPressed(mouse::Button::Right))
                if !shell.is_event_captured() && !self.items.is_empty() =>
            {
                if let Some(pos) = cursor.position_over(layout.bounds()) {
                    state.open = true;
                    state.position = pos;
                    shell.capture_event();
                }
            }
            Event::Mouse(mouse::Event::ButtonPressed(mouse::Button::Left)) => {
                if state.open {
                    state.open = false;
                }
            }
            Event::Touch(touch::Event::FingerPressed { .. }) => {
                if state.open {
                    state.open = false;
                }
            }
            Event::Keyboard(keyboard::Event::KeyPressed {
                key: keyboard::Key::Named(keyboard::key::Named::Escape),
                ..
            }) => {
                if state.open {
                    state.open = false;
                    shell.capture_event();
                }
            }
            _ => {}
        }
    }

    fn overlay<'b>(
        &'b mut self,
        tree: &'b mut widget::Tree,
        layout: Layout<'b>,
        renderer: &Renderer,
        viewport: &Rectangle,
        translation: Vector,
    ) -> Option<overlay::Element<'b, Message, GraphixTheme, Renderer>> {
        let child_overlay = self.child.as_widget_mut().overlay(
            &mut tree.children[0],
            layout,
            renderer,
            viewport,
            translation,
        );
        let state = tree.state.downcast_mut::<State>();
        if child_overlay.is_some() {
            // a child's dropdown covers the menu; it does not wait under it
            state.open = false;
            return child_overlay;
        }
        if !state.open || self.items.is_empty() {
            return None;
        }
        Some(overlay::Element::new(Box::new(MenuOverlay {
            items: &self.items,
            position: state.position + translation,
            open: &mut state.open,
        })))
    }

    fn mouse_interaction(
        &self,
        tree: &widget::Tree,
        layout: Layout<'_>,
        cursor: mouse::Cursor,
        viewport: &Rectangle,
        renderer: &Renderer,
    ) -> mouse::Interaction {
        self.child.as_widget().mouse_interaction(
            &tree.children[0],
            layout,
            cursor,
            viewport,
            renderer,
        )
    }
}

impl<'a> From<OwnedContextMenu<'a>> for Element<'a, Message, GraphixTheme, Renderer> {
    fn from(w: OwnedContextMenu<'a>) -> Self {
        Self::new(w)
    }
}
