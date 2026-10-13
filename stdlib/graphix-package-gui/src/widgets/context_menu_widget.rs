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

// CR claude for claude: [bug/medium] This impl does not override `operate`, and iced's
// default `Widget::operate` does nothing. Every Operation therefore stops at the
// context menu and never reaches the child. That includes the frame's
// `WidgetOp::ScrollTo` and `WidgetOp::Focus`, iced's focus traversal and text-input
// operations. One effect: a data_table wrapped in `menu::context_menu` does not scroll
// to follow its selection, and its cell editor never gets focus. Fix: forward it the
// way `KeyboardArea` does, `self.child.as_widget_mut().operate(&mut tree.children[0],
// layout, renderer, operation)`. Probe:
// design/review-2026-10-12/repro/gui-widgets-a-05.sh (test
// operations_reach_context_menu_child in gui-widgets-a.rs: it finds 1 focusable, should
// find 2). (gui-widgets-a-05)
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
            // CR claude for claude: [bug/high] The menu closes only on a left press, a
            // finger press or Escape. A right press on another row opens that row's
            // menu and leaves this one open. iced's `overlay::Group::update` gives
            // every event to every sibling overlay, whether or not one has captured it.
            // `MenuOverlay::update` never checks `shell.is_event_captured()`. So with
            // two menus open, one Ctrl+D runs the matching action in both menus, and
            // one left click where they overlap runs an item in each (e.g. A's Copy and
            // B's Delete). Fix: close the menu on any button press that lands outside
            // its overlay, including a right press elsewhere, and make
            // `MenuOverlay::update` return early when the event is already captured.
            // Probe: design/review-2026-10-12/repro/gui-widgets-a-03.sh (tests
            // second_context_menu_closes_the_first, one_shortcut_one_action in
            // gui-widgets-a.rs). (gui-widgets-a-03)
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
        // CR claude for claude: [bug/medium] Any child overlay closes the menu here,
        // not only a dropdown. iced's Tooltip (graphix's tooltip sets no delay) returns
        // an overlay for as long as the cursor hovers it, and a right-click always
        // happens while hovering. So a context menu around a tooltip'd widget closes in
        // the same frame it opens and can never be used. Fix: when the menu is open,
        // keep it open and return both overlays as an overlay::Group, child first and
        // then the MenuOverlay. Alternatively, close the menu only when a child overlay
        // appears after the menu opened (record it in State). probe:
        // design/review-2026-10-12/repro/gui-widgets-a-04.rs
        // (context_menu_over_tooltip_opens picks nothing; the control test without the
        // tooltip picks the item). (gui-widgets-a-04)
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
