use iced_core::{
    alignment, keyboard, layout, mouse, overlay, renderer, touch, widget,
    Clipboard, Element, Event, Layout, Length, Padding, Point, Rectangle, Shell, Size,
    Vector, Widget,
};

use super::{Message, Renderer};
use crate::theme::GraphixTheme;
use crate::types::ShortcutV;
use graphix_rt::CallableId;
use netidx::{protocol::valarray::ValArray, publisher::Value};

/// Description of a menu item for the custom menu bar widget.
pub(crate) enum MenuItemDesc {
    Action {
        label: String,
        shortcut: Option<ShortcutV>,
        callable_id: Option<CallableId>,
        disabled: bool,
    },
    Divider,
}

/// Description of a menu group (top-level menu label + items).
pub(crate) struct MenuGroupDesc {
    pub label: String,
    pub items: Vec<MenuItemDesc>,
}

#[derive(Default)]
pub(crate) struct State {
    pub open_menu: Option<usize>,
}

/// Overlay that renders the dropdown menu below a menu bar label.
/// When `open` is `Some`, the overlay sets it to `false` when an item
/// is clicked (used by context menus). Menu bar passes `None` since
/// it manages open state in its own `update()`.
pub(crate) struct MenuOverlay<'a> {
    pub menu: &'a MenuGroupDesc,
    pub position: Point,
    pub open: Option<&'a mut bool>,
}

const ITEM_PADDING: Padding = Padding {
    top: 6.0,
    right: 20.0,
    bottom: 6.0,
    left: 20.0,
};
const DIVIDER_HEIGHT: f32 = 9.0;
const MIN_ITEM_WIDTH: f32 = 180.0;

impl overlay::Overlay<Message, GraphixTheme, Renderer> for MenuOverlay<'_> {
    fn layout(&mut self, renderer: &Renderer, _bounds: Size) -> layout::Node {
        let text_size = <Renderer as iced_core::text::Renderer>::default_size(renderer).0;
        let mut max_width: f32 = MIN_ITEM_WIDTH;
        let mut total_height: f32 = 0.0;
        let mut child_sizes = Vec::with_capacity(self.menu.items.len());
        for item in &self.menu.items {
            match item {
                MenuItemDesc::Action { label, shortcut, .. } => {
                    let display_len = match shortcut {
                        Some(sc) => label.len() + 3 + sc.display.len(),
                        None => label.len(),
                    };
                    let item_w = text_size * display_len as f32 * 0.6
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
        layout::Node::with_children(Size::new(max_width, total_height), nodes)
            .move_to(self.position)
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
        // Drop shadow
        <Renderer as renderer::Renderer>::fill_quad(
            renderer,
            renderer::Quad {
                bounds: Rectangle {
                    x: bounds.x + 2.0,
                    y: bounds.y + 2.0,
                    ..bounds
                },
                border: Default::default(),
                shadow: Default::default(),
                snap: true,
            },
            iced_core::Color::from_rgba(0.0, 0.0, 0.0, 0.3),
        );
        // Background
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
        // Items
        let text_size = <Renderer as iced_core::text::Renderer>::default_size(renderer);
        for (item, child_layout) in self.menu.items.iter().zip(layout.children()) {
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
                        item_bounds.width
                            - ITEM_PADDING.left
                            - ITEM_PADDING.right,
                        item_bounds.height,
                    );
                    <Renderer as iced_core::text::Renderer>::fill_text(
                        renderer,
                        iced_core::Text {
                            content: label.as_str().into(),
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
                                line_height:
                                    iced_core::text::LineHeight::default(),
                                font: iced_core::Font::DEFAULT,
                                align_x: alignment::Horizontal::Right.into(),
                                align_y: alignment::Vertical::Center,
                                shaping: iced_core::text::Shaping::Advanced,
                                wrapping: iced_core::text::Wrapping::None,
                            },
                            Point::new(
                                item_bounds.x + item_bounds.width
                                    - ITEM_PADDING.right,
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
            Event::Keyboard(keyboard::Event::KeyPressed {
                key, modifiers, ..
            }) => {
                for item in &self.menu.items {
                    if let MenuItemDesc::Action {
                        shortcut: Some(sc),
                        callable_id: Some(id),
                        disabled: false,
                        ..
                    } = item
                    {
                        if *key == sc.key && *modifiers == sc.modifiers {
                            shell.publish(Message::Call(
                                *id,
                                ValArray::from_iter([Value::Null]),
                            ));
                            shell.capture_event();
                            return;
                        }
                    }
                }
            }
            Event::Mouse(mouse::Event::ButtonPressed(mouse::Button::Left))
            | Event::Touch(touch::Event::FingerPressed { .. }) => {
                for (item, child_layout) in
                    self.menu.items.iter().zip(layout.children())
                {
                    if cursor.is_over(child_layout.bounds()) {
                        if let MenuItemDesc::Action {
                            callable_id: Some(id),
                            disabled: false,
                            ..
                        } = item
                        {
                            if let Some(open) = self.open.as_deref_mut() {
                                *open = false;
                            }
                            shell.publish(Message::Call(
                                *id,
                                ValArray::from_iter([Value::Null]),
                            ));
                            shell.capture_event();
                            return;
                        }
                    }
                }
            }
            _ => {}
        }
    }
}

/// The owning widget that renders the menu bar and manages the overlay.
pub(crate) struct OwnedMenuBar {
    pub descs: Vec<MenuGroupDesc>,
    pub width: Length,
}

impl Widget<Message, GraphixTheme, Renderer> for OwnedMenuBar {
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
        let limits = limits.width(self.width);
        let max = limits.max();
        let text_size = <Renderer as iced_core::text::Renderer>::default_size(renderer).0;
        let padding = Padding::new(8.0);
        let mut total_width: f32 = 0.0;
        let mut max_height: f32 = 0.0;
        let mut children = Vec::with_capacity(self.descs.len());
        for menu in &self.descs {
            let label_w = text_size * menu.label.len() as f32 * 0.6;
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
        let bar_width =
            if self.width == Length::Fill { max.width } else { total_width };
        layout::Node::with_children(Size::new(bar_width, max_height), children)
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
                    content: menu.label.as_str().into(),
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
        match event {
            Event::Mouse(mouse::Event::ButtonPressed(mouse::Button::Left))
            | Event::Touch(touch::Event::FingerPressed { .. }) => {
                for (i, child_layout) in layout.children().enumerate() {
                    if cursor.is_over(child_layout.bounds()) {
                        if state.open_menu == Some(i) {
                            state.open_menu = None;
                        } else {
                            state.open_menu = Some(i);
                        }
                        shell.capture_event();
                        return;
                    }
                }
                if state.open_menu.is_some() {
                    state.open_menu = None;
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
                    shell.capture_event();
                }
            }
            Event::Keyboard(keyboard::Event::KeyPressed {
                key, modifiers, ..
            }) => {
                for menu in &self.descs {
                    for item in &menu.items {
                        if let MenuItemDesc::Action {
                            shortcut: Some(sc),
                            callable_id: Some(id),
                            disabled: false,
                            ..
                        } = item
                        {
                            if *key == sc.key && *modifiers == sc.modifiers {
                                state.open_menu = None;
                                shell.publish(Message::Call(
                                    *id,
                                    ValArray::from_iter([Value::Null]),
                                ));
                                shell.capture_event();
                                return;
                            }
                        }
                    }
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
        _translation: Vector,
    ) -> Option<overlay::Element<'b, Message, GraphixTheme, Renderer>> {
        let state = tree.state.downcast_ref::<State>();
        let idx = state.open_menu?;
        if idx >= self.descs.len() {
            return None;
        }
        let label_bounds = layout.children().nth(idx)?.bounds();
        let position =
            Point::new(label_bounds.x, label_bounds.y + label_bounds.height);
        Some(overlay::Element::new(Box::new(MenuOverlay {
            menu: &self.descs[idx],
            position,
            open: None,
        })))
    }
}

impl From<OwnedMenuBar>
    for Element<'_, Message, GraphixTheme, Renderer>
{
    fn from(w: OwnedMenuBar) -> Self {
        Self::new(w)
    }
}
