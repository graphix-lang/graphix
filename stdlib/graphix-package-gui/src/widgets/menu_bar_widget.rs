use iced_core::{
    Clipboard, Element, Event, Layout, Length, Padding, Point, Rectangle, Shell, Size,
    Vector, Widget, alignment, keyboard, layout, mouse, overlay, renderer, touch, widget,
};

use super::{Message, Renderer, measure_text};
use crate::{theme::GraphixTheme, types::ShortcutV};
use graphix_rt::CallableId;
use netidx::{protocol::valarray::ValArray, publisher::Value};

/// A menu item as the menu widgets draw it, borrowed from its widget.
pub(crate) enum MenuItemDesc<'a> {
    Action {
        label: &'a str,
        shortcut: Option<&'a ShortcutV>,
        callable_id: Option<CallableId>,
        disabled: bool,
    },
    Divider,
}

/// A top-level menu: its label and items.
pub(crate) struct MenuGroupDesc<'a> {
    pub label: &'a str,
    pub items: Vec<MenuItemDesc<'a>>,
}

/// The enabled action whose shortcut `key` and `modifiers` press.
pub(crate) fn shortcut_action<'a>(
    items: impl IntoIterator<Item = &'a MenuItemDesc<'a>>,
    key: &keyboard::Key,
    modifiers: keyboard::Modifiers,
) -> Option<CallableId> {
    items.into_iter().find_map(|item| match item {
        MenuItemDesc::Action {
            shortcut: Some(sc),
            callable_id: Some(id),
            disabled: false,
            ..
        } if *key == sc.key && modifiers == sc.modifiers => Some(*id),
        _ => None,
    })
}

fn call(id: CallableId) -> Message {
    Message::Call(id, ValArray::from_iter([Value::Null]))
}

#[derive(Default)]
pub(crate) struct State {
    pub open_menu: Option<usize>,
    /// `true` while the dropdown overlay is visible; the overlay flips
    /// it to `false` on item click and `update` clears `open_menu`.
    pub menu_visible: bool,
}

/// A dropdown menu at `position` in window coordinates, kept inside the
/// window; `open` is set to `false` when an item is chosen.
pub(crate) struct MenuOverlay<'a, 'b> {
    pub items: &'b [MenuItemDesc<'a>],
    pub position: Point,
    pub open: &'b mut bool,
}

const ITEM_PADDING: Padding = Padding { top: 6.0, right: 20.0, bottom: 6.0, left: 20.0 };
const DIVIDER_HEIGHT: f32 = 9.0;
const MIN_ITEM_WIDTH: f32 = 180.0;

impl overlay::Overlay<Message, GraphixTheme, Renderer> for MenuOverlay<'_, '_> {
    fn layout(&mut self, renderer: &Renderer, bounds: Size) -> layout::Node {
        let text_size = <Renderer as iced_core::text::Renderer>::default_size(renderer).0;
        let measure = |s: &str| measure_text(s, text_size, iced_core::Font::DEFAULT);
        let mut max_width: f32 = MIN_ITEM_WIDTH;
        let mut total_height: f32 = 0.0;
        let mut child_sizes = Vec::with_capacity(self.items.len());
        for item in self.items {
            match item {
                MenuItemDesc::Action { label, shortcut, .. } => {
                    let gap = text_size * 1.5;
                    let item_w = measure(label)
                        + shortcut.map_or(0.0, |sc| gap + measure(&sc.display))
                        + ITEM_PADDING.left
                        + ITEM_PADDING.right;
                    let item_h = text_size + ITEM_PADDING.top + ITEM_PADDING.bottom;
                    max_width = max_width.max(item_w);
                    child_sizes.push(item_h);
                    total_height += item_h;
                }
                MenuItemDesc::Divider => {
                    child_sizes.push(DIVIDER_HEIGHT);
                    total_height += DIVIDER_HEIGHT;
                }
            }
        }
        let mut y = 0.0f32;
        let nodes: Vec<_> = child_sizes
            .into_iter()
            .map(|h| {
                let node = layout::Node::new(Size::new(max_width, h))
                    .move_to(Point::new(0.0, y));
                y += h;
                node
            })
            .collect();
        // inside the window: shifted left at the right edge, above the
        // point at the bottom when it fits there
        let (p, size) = (self.position, Size::new(max_width, total_height));
        let x = p.x.min(bounds.width - size.width).max(0.0);
        let y = match p.y + size.height > bounds.height && p.y >= size.height {
            true => p.y - size.height,
            false => p.y.min(bounds.height - size.height).max(0.0),
        };
        layout::Node::with_children(size, nodes).move_to(Point::new(x, y))
    }

    fn mouse_interaction(
        &self,
        layout: Layout<'_>,
        cursor: mouse::Cursor,
        _renderer: &Renderer,
    ) -> mouse::Interaction {
        match cursor.is_over(layout.bounds()) {
            true => mouse::Interaction::Pointer,
            false => mouse::Interaction::None,
        }
    }

    fn draw(
        &self,
        renderer: &mut Renderer,
        theme: &GraphixTheme,
        _style: &renderer::Style,
        layout: Layout<'_>,
        cursor: mouse::Cursor,
    ) {
        let palette = theme.palette();
        let bounds = layout.bounds();
        <Renderer as renderer::Renderer>::fill_quad(
            renderer,
            renderer::Quad {
                bounds: Rectangle { x: bounds.x + 2.0, y: bounds.y + 2.0, ..bounds },
                border: Default::default(),
                shadow: Default::default(),
                snap: true,
            },
            iced_core::Color::from_rgba(0.0, 0.0, 0.0, 0.3),
        );
        <Renderer as renderer::Renderer>::fill_quad(
            renderer,
            renderer::Quad {
                bounds,
                border: iced_core::Border {
                    color: iced_core::Color::from_rgba(0.5, 0.5, 0.5, 0.3),
                    width: 1.0,
                    radius: 4.0.into(),
                },
                shadow: Default::default(),
                snap: true,
            },
            palette.background,
        );
        let text_size = <Renderer as iced_core::text::Renderer>::default_size(renderer);
        for (item, child_layout) in self.items.iter().zip(layout.children()) {
            let item_bounds = child_layout.bounds();
            match item {
                MenuItemDesc::Action { label, shortcut, disabled, .. } => {
                    let is_hovered = !disabled && cursor.is_over(item_bounds);
                    if is_hovered {
                        <Renderer as renderer::Renderer>::fill_quad(
                            renderer,
                            renderer::Quad {
                                bounds: item_bounds,
                                border: Default::default(),
                                shadow: Default::default(),
                                snap: true,
                            },
                            iced_core::Color::from_rgba(
                                palette.primary.r,
                                palette.primary.g,
                                palette.primary.b,
                                0.25,
                            ),
                        );
                    }
                    let text_color = if *disabled {
                        iced_core::Color::from_rgba(
                            palette.text.r,
                            palette.text.g,
                            palette.text.b,
                            0.4,
                        )
                    } else {
                        palette.text
                    };
                    let text_bounds = Size::new(
                        item_bounds.width - ITEM_PADDING.left - ITEM_PADDING.right,
                        item_bounds.height,
                    );
                    <Renderer as iced_core::text::Renderer>::fill_text(
                        renderer,
                        iced_core::Text {
                            content: (*label).into(),
                            bounds: text_bounds,
                            size: text_size,
                            line_height: iced_core::text::LineHeight::default(),
                            font: iced_core::Font::DEFAULT,
                            align_x: alignment::Horizontal::Left.into(),
                            align_y: alignment::Vertical::Center,
                            shaping: iced_core::text::Shaping::Advanced,
                            wrapping: iced_core::text::Wrapping::None,
                        },
                        Point::new(
                            item_bounds.x + ITEM_PADDING.left,
                            item_bounds.center_y(),
                        ),
                        text_color,
                        item_bounds,
                    );
                    if let Some(sc) = shortcut {
                        let dimmed = iced_core::Color::from_rgba(
                            text_color.r,
                            text_color.g,
                            text_color.b,
                            text_color.a * 0.5,
                        );
                        <Renderer as iced_core::text::Renderer>::fill_text(
                            renderer,
                            iced_core::Text {
                                content: sc.display.as_str().into(),
                                bounds: text_bounds,
                                size: text_size,
                                line_height: iced_core::text::LineHeight::default(),
                                font: iced_core::Font::DEFAULT,
                                align_x: alignment::Horizontal::Right.into(),
                                align_y: alignment::Vertical::Center,
                                shaping: iced_core::text::Shaping::Advanced,
                                wrapping: iced_core::text::Wrapping::None,
                            },
                            Point::new(
                                item_bounds.x + item_bounds.width - ITEM_PADDING.right,
                                item_bounds.center_y(),
                            ),
                            dimmed,
                            item_bounds,
                        );
                    }
                }
                MenuItemDesc::Divider => {
                    let y = item_bounds.center_y();
                    let divider_color = iced_core::Color::from_rgba(
                        palette.text.r,
                        palette.text.g,
                        palette.text.b,
                        0.15,
                    );
                    <Renderer as renderer::Renderer>::fill_quad(
                        renderer,
                        renderer::Quad {
                            bounds: Rectangle {
                                x: item_bounds.x + 8.0,
                                y: y - 0.5,
                                width: item_bounds.width - 16.0,
                                height: 1.0,
                            },
                            border: Default::default(),
                            shadow: Default::default(),
                            snap: true,
                        },
                        divider_color,
                    );
                }
            }
        }
    }

    fn update(
        &mut self,
        event: &Event,
        layout: Layout<'_>,
        cursor: mouse::Cursor,
        _renderer: &Renderer,
        _clipboard: &mut dyn Clipboard,
        shell: &mut Shell<'_, Message>,
    ) {
        match event {
            Event::Keyboard(keyboard::Event::KeyPressed { key, modifiers, .. }) => {
                if let Some(id) = shortcut_action(self.items, key, *modifiers) {
                    *self.open = false;
                    shell.publish(call(id));
                    shell.capture_event();
                }
            }
            Event::Mouse(mouse::Event::ButtonPressed(_))
            | Event::Touch(touch::Event::FingerPressed { .. })
                if cursor.is_over(layout.bounds()) =>
            {
                let chosen =
                    self.items.iter().zip(layout.children()).find_map(|(item, l)| {
                        match item {
                            MenuItemDesc::Action {
                                callable_id: Some(id),
                                disabled: false,
                                ..
                            } if cursor.is_over(l.bounds()) => Some(*id),
                            _ => None,
                        }
                    });
                if let Some(id) = chosen
                    && matches!(
                        event,
                        Event::Mouse(mouse::Event::ButtonPressed(mouse::Button::Left))
                            | Event::Touch(_)
                    )
                {
                    *self.open = false;
                    shell.publish(call(id));
                }
                shell.capture_event();
            }
            Event::Mouse(mouse::Event::WheelScrolled { .. })
                if cursor.is_over(layout.bounds()) =>
            {
                shell.capture_event()
            }
            _ => {}
        }
    }
}

/// The owning widget that renders the menu bar and manages the overlay.
pub(crate) struct OwnedMenuBar<'a> {
    pub descs: Vec<MenuGroupDesc<'a>>,
    pub width: Length,
}

impl Widget<Message, GraphixTheme, Renderer> for OwnedMenuBar<'_> {
    fn tag(&self) -> widget::tree::Tag {
        widget::tree::Tag::of::<State>()
    }

    fn state(&self) -> widget::tree::State {
        widget::tree::State::new(State::default())
    }

    fn size(&self) -> Size<Length> {
        Size::new(self.width, Length::Shrink)
    }

    fn layout(
        &mut self,
        _tree: &mut widget::Tree,
        renderer: &Renderer,
        limits: &layout::Limits,
    ) -> layout::Node {
        let text_size = <Renderer as iced_core::text::Renderer>::default_size(renderer).0;
        let padding = Padding::new(8.0);
        let mut total_width: f32 = 0.0;
        let mut max_height: f32 = 0.0;
        let mut children = Vec::with_capacity(self.descs.len());
        for menu in &self.descs {
            let label_w = measure_text(menu.label, text_size, iced_core::Font::DEFAULT);
            let padded_w = label_w + padding.left + padding.right;
            let padded_h = text_size + padding.top + padding.bottom;
            children.push(
                layout::Node::new(Size::new(padded_w, padded_h))
                    .move_to(Point::new(total_width, 0.0)),
            );
            total_width += padded_w;
            max_height = max_height.max(padded_h);
        }
        for child in &mut children {
            let s = child.size();
            *child = layout::Node::new(Size::new(s.width, max_height))
                .move_to(child.bounds().position());
        }
        let size = limits.resolve(
            self.width,
            Length::Shrink,
            Size::new(total_width, max_height),
        );
        layout::Node::with_children(size, children)
    }

    fn draw(
        &self,
        tree: &widget::Tree,
        renderer: &mut Renderer,
        theme: &GraphixTheme,
        _style: &renderer::Style,
        layout: Layout<'_>,
        cursor: mouse::Cursor,
        _viewport: &Rectangle,
    ) {
        let state = tree.state.downcast_ref::<State>();
        let palette = theme.palette();
        let bar_bg = iced_core::Color::from_rgba(
            palette.background.r * 0.9,
            palette.background.g * 0.9,
            palette.background.b * 0.9,
            1.0,
        );
        <Renderer as renderer::Renderer>::fill_quad(
            renderer,
            renderer::Quad {
                bounds: layout.bounds(),
                border: Default::default(),
                shadow: Default::default(),
                snap: true,
            },
            bar_bg,
        );
        let text_size = <Renderer as iced_core::text::Renderer>::default_size(renderer);
        for (i, (menu, child_layout)) in
            self.descs.iter().zip(layout.children()).enumerate()
        {
            let bounds = child_layout.bounds();
            let is_open = state.open_menu == Some(i);
            let is_hovered = cursor.is_over(bounds);
            if is_open || is_hovered {
                let highlight = iced_core::Color::from_rgba(
                    palette.primary.r,
                    palette.primary.g,
                    palette.primary.b,
                    if is_open { 0.3 } else { 0.15 },
                );
                <Renderer as renderer::Renderer>::fill_quad(
                    renderer,
                    renderer::Quad {
                        bounds,
                        border: Default::default(),
                        shadow: Default::default(),
                        snap: true,
                    },
                    highlight,
                );
            }
            <Renderer as iced_core::text::Renderer>::fill_text(
                renderer,
                iced_core::Text {
                    content: menu.label.into(),
                    bounds: Size::new(bounds.width, bounds.height),
                    size: text_size,
                    line_height: iced_core::text::LineHeight::default(),
                    font: iced_core::Font::DEFAULT,
                    align_x: alignment::Horizontal::Center.into(),
                    align_y: alignment::Vertical::Center,
                    shaping: iced_core::text::Shaping::Basic,
                    wrapping: iced_core::text::Wrapping::None,
                },
                bounds.center(),
                palette.text,
                bounds,
            );
        }
    }

    fn update(
        &mut self,
        tree: &mut widget::Tree,
        event: &Event,
        layout: Layout<'_>,
        cursor: mouse::Cursor,
        _renderer: &Renderer,
        _clipboard: &mut dyn Clipboard,
        shell: &mut Shell<'_, Message>,
        _viewport: &Rectangle,
    ) {
        let state = tree.state.downcast_mut::<State>();
        if state.open_menu.is_some() && !state.menu_visible {
            state.open_menu = None;
        }
        match event {
            Event::Mouse(mouse::Event::ButtonPressed(mouse::Button::Left))
            | Event::Touch(touch::Event::FingerPressed { .. }) => {
                for (i, child_layout) in layout.children().enumerate() {
                    if cursor.is_over(child_layout.bounds()) {
                        if state.open_menu == Some(i) {
                            state.open_menu = None;
                            state.menu_visible = false;
                        } else {
                            state.open_menu = Some(i);
                            state.menu_visible = true;
                        }
                        shell.capture_event();
                        return;
                    }
                }
                if state.open_menu.is_some() {
                    state.open_menu = None;
                    state.menu_visible = false;
                    shell.capture_event();
                }
            }
            Event::Mouse(mouse::Event::CursorMoved { .. }) => {
                if state.open_menu.is_some() {
                    for (i, child_layout) in layout.children().enumerate() {
                        if cursor.is_over(child_layout.bounds())
                            && state.open_menu != Some(i)
                        {
                            state.open_menu = Some(i);
                            state.menu_visible = true;
                            shell.capture_event();
                            return;
                        }
                    }
                }
            }
            Event::Keyboard(keyboard::Event::KeyPressed {
                key: keyboard::Key::Named(keyboard::key::Named::Escape),
                ..
            }) => {
                if state.open_menu.is_some() {
                    state.open_menu = None;
                    state.menu_visible = false;
                    shell.capture_event();
                }
            }
            Event::Keyboard(keyboard::Event::KeyPressed { key, modifiers, .. }) => {
                let items = self.descs.iter().flat_map(|m| m.items.iter());
                if let Some(id) = shortcut_action(items, key, *modifiers) {
                    state.open_menu = None;
                    state.menu_visible = false;
                    shell.publish(call(id));
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
        _renderer: &Renderer,
        _viewport: &Rectangle,
        translation: Vector,
    ) -> Option<overlay::Element<'b, Message, GraphixTheme, Renderer>> {
        let state = tree.state.downcast_mut::<State>();
        let idx = state.open_menu?;
        let menu = self.descs.get(idx)?;
        let label = layout.children().nth(idx)?.bounds();
        let position = Point::new(label.x, label.y + label.height) + translation;
        Some(overlay::Element::new(Box::new(MenuOverlay {
            items: &menu.items,
            position,
            open: &mut state.menu_visible,
        })))
    }
}

impl<'a> From<OwnedMenuBar<'a>> for Element<'a, Message, GraphixTheme, Renderer> {
    fn from(w: OwnedMenuBar<'a>) -> Self {
        Self::new(w)
    }
}
