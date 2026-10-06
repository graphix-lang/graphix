use iced_core::{
    Clipboard, Element, Event, Layout, Length, Point, Rectangle, Shell, Size, Vector,
    Widget, keyboard, layout, mouse, overlay, renderer, touch, widget,
};

use super::{
    Message, Renderer,
    menu_bar_widget::{MenuGroupDesc, MenuItemDesc, MenuOverlay},
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
    desc: MenuGroupDesc,
}

impl<'a> OwnedContextMenu<'a> {
    pub fn new(
        child: Element<'a, Message, GraphixTheme, Renderer>,
        items: Vec<MenuItemDesc>,
    ) -> Self {
        Self { child, desc: MenuGroupDesc { label: String::new(), items } }
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
        // CR claude for eric: [bug] This sets `open` on every right-click over the
        // child, even when this menu cannot show. That happens in three cases: an inner
        // context_menu already captured the click (there is no
        // `shell.is_event_captured()` check here, unlike iced_keyboard_area.rs:133), a
        // child's pick_list dropdown covers this menu, or `items` is empty. The flag
        // survives, so the menu pops up unprompted at the old right-click position once
        // the cause goes away: right after an inner menu item is chosen, after a
        // dropdown option is picked, or when the items arrive. The next click there
        // then chooses its first item. A capture check covers only the nested case,
        // because pick_list ignores right-clicks. probe:
        // design/review-2026-10-05/repro/gui-widgets-a-08.rs
        // (`nested_inner_item_reopens_outer`: choose Rename, and the outer "New folder"
        // menu is then open at the same spot). (gui-widgets-a-08)
        let state = tree.state.downcast_mut::<State>();
        // CR claude for eric: [doc-drift] menu.md says a shortcut triggers its action
        // globally within the window, and book/src/examples/gui/context_menu.gx shows
        // Ctrl+C and Ctrl+V. This match has no shortcut arm, though. Only
        // MenuOverlay::update matches shortcuts, and that overlay exists only while the
        // menu is open (OwnedMenuBar::update handles them globally). So a context
        // menu's shortcuts do nothing until the menu is opened with a right-click.
        // Either match the item shortcuts here (deciding which of several per-row menus
        // owns a key) or document that context-menu shortcuts work only while the menu
        // is open. probe: design/review-2026-10-05/repro/gui-widgets-a-12.rs (closed
        // menu: Ctrl+C publishes nothing; menu::bar and an opened menu: the Call).
        // (gui-widgets-a-12)
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
        if child_overlay.is_some() {
            return child_overlay;
        }
        let state = tree.state.downcast_mut::<State>();
        if !state.open || self.desc.items.is_empty() {
            return None;
        }
        Some(overlay::Element::new(Box::new(MenuOverlay {
            menu: &self.desc,
            // CR claude for eric: [bug] overlay() drops `translation`. Inside a
            // scrollable, `state.position` is in content coordinates, because iced
            // gives children the cursor plus the scroll offset and expects overlays to
            // add `translation`, as pick_list and tooltip do. So the menu opens at the
            // cursor plus the scroll offset: scrolled 1000px, it is laid out 900px
            // below a 200px window and cannot be used. OwnedMenuBar::overlay
            // (menu_bar_widget.rs:519) drops it the same way. MenuOverlay::layout also
            // ignores its `bounds`, so a menu opened near the bottom or right edge runs
            // off the window; iced's own menu flips above and stays within the bounds.
            // Probe: design/review-2026-10-05/repro/gui-widgets-a-05.rs (copy it to
            // stdlib/graphix-package-gui/tests/ and run cargo test --test
            // review_gui_widgets_a_05); iced's pick_list in the same scrolled list is
            // the passing control. (gui-widgets-a-05)
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

impl<'a> From<OwnedContextMenu<'a>> for Element<'a, Message, GraphixTheme, Renderer> {
    fn from(w: OwnedContextMenu<'a>) -> Self {
        Self::new(w)
    }
}
