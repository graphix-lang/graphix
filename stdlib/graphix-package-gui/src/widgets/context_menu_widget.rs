use iced_core::{
    keyboard, layout, mouse, overlay, renderer, touch, widget, Clipboard, Element, Event,
    Layout, Length, Point, Rectangle, Shell, Size, Vector, Widget,
};

use super::menu_bar_widget::{MenuGroupDesc, MenuItemDesc, MenuOverlay};
use super::{Message, Renderer};
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
    desc: MenuGroupDesc,
}

impl<'a> OwnedContextMenu<'a> {
    pub fn new(
        child: Element<'a, Message, GraphixTheme, Renderer>,
        items: Vec<MenuItemDesc>,
    ) -> Self {
        Self {
            child,
            desc: MenuGroupDesc { label: String::new(), items },
        }
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
        self.child.as_widget_mut().layout(
            &mut tree.children[0],
            renderer,
            limits,
        )
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
        // Forward events to child first
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
            Event::Mouse(mouse::Event::ButtonPressed(mouse::Button::Right)) => {
                if cursor.is_over(layout.bounds()) {
                    if let Some(pos) = cursor.position() {
                        state.open = true;
                        state.position = pos;
                        shell.capture_event();
                    }
                }
            }
            Event::Mouse(mouse::Event::ButtonPressed(mouse::Button::Left)) => {
                if state.open {
                    // Close on left-click outside the overlay
                    // (clicks on overlay items are handled by MenuOverlay)
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
        // First check for child overlays
        let child_overlay = self.child.as_widget_mut().overlay(
            &mut tree.children[0],
            layout,
            renderer,
            viewport,
            translation,
        );
        if child_overlay.is_some() {
            return child_overlay;
        }
        let state = tree.state.downcast_mut::<State>();
        if !state.open || self.desc.items.is_empty() {
            return None;
        }
        Some(overlay::Element::new(Box::new(MenuOverlay {
            menu: &self.desc,
            position: state.position,
            open: Some(&mut state.open),
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

impl<'a> From<OwnedContextMenu<'a>>
    for Element<'a, Message, GraphixTheme, Renderer>
{
    fn from(w: OwnedContextMenu<'a>) -> Self {
        Self::new(w)
    }
}
