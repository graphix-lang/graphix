use iced_core::{border::Border, Color, Shadow};
use iced_widget::{
    button, checkbox, combo_box, container, overlay::menu, pick_list, progress_bar, radio,
    rule, scrollable, slider, svg, text_editor, text_input, toggler,
};
use triomphe::Arc;

/// Wrapper around `iced_core::Theme` that supports per-widget style overrides.
///
/// When `overrides` is `None`, all Catalog impls delegate directly to the
/// inner theme — behavior is identical to using `iced_core::Theme` directly.
/// When `overrides` is `Some`, each widget checks for a user-specified style
/// before falling back to the inner theme's built-in Catalog.
#[derive(Clone, Debug)]
pub(crate) struct GraphixTheme {
    pub inner: iced_core::Theme,
    pub overrides: Option<Arc<StyleOverrides>>,
}

impl GraphixTheme {
    pub fn palette(&self) -> iced_core::theme::palette::Palette {
        self.inner.palette()
    }
}

#[derive(Clone, Debug)]
pub(crate) struct StyleOverrides {
    pub button: Option<ButtonSpec>,
    pub checkbox: Option<CheckboxSpec>,
    pub container: Option<ContainerSpec>,
    pub menu: Option<MenuSpec>,
    pub pick_list: Option<PickListSpec>,
    pub progress_bar: Option<ProgressBarSpec>,
    pub radio: Option<RadioSpec>,
    pub rule: Option<RuleSpec>,
    pub scrollable: Option<ScrollableSpec>,
    pub slider: Option<SliderSpec>,
    pub text_editor: Option<TextEditorSpec>,
    pub text_input: Option<TextInputSpec>,
    pub toggler: Option<TogglerSpec>,
}

// --- Spec structs ---

#[derive(Clone, Copy, Debug)]
pub(crate) struct ButtonSpec {
    pub background: Color,
    pub border_color: Color,
    pub border_radius: f32,
    pub border_width: f32,
    pub text_color: Color,
}

#[derive(Clone, Copy, Debug)]
pub(crate) struct CheckboxSpec {
    pub accent: Color,
    pub background: Color,
    pub border_color: Color,
    pub border_radius: f32,
    pub border_width: f32,
    pub icon_color: Color,
    pub text_color: Color,
}

#[derive(Clone, Copy, Debug)]
pub(crate) struct TextInputSpec {
    pub background: Color,
    pub border_color: Color,
    pub border_radius: f32,
    pub border_width: f32,
    pub icon_color: Color,
    pub placeholder_color: Color,
    pub selection_color: Color,
    pub value_color: Color,
}

#[derive(Clone, Copy, Debug)]
pub(crate) struct TogglerSpec {
    pub background: Color,
    pub background_border_color: Color,
    pub border_radius: f32,
    pub foreground: Color,
    pub foreground_border_color: Color,
    pub text_color: Color,
}

#[derive(Clone, Copy, Debug)]
pub(crate) struct SliderSpec {
    pub handle_border_color: Color,
    pub handle_border_width: f32,
    pub handle_color: Color,
    pub handle_radius: f32,
    pub rail_color: Color,
    pub rail_fill_color: Color,
    pub rail_width: f32,
}

#[derive(Clone, Copy, Debug)]
pub(crate) struct RadioSpec {
    pub background: Color,
    pub border_color: Color,
    pub border_width: f32,
    pub dot_color: Color,
    pub text_color: Color,
}

#[derive(Clone, Copy, Debug)]
pub(crate) struct PickListSpec {
    pub background: Color,
    pub border_color: Color,
    pub border_radius: f32,
    pub border_width: f32,
    pub handle_color: Color,
    pub placeholder_color: Color,
    pub text_color: Color,
}

#[derive(Clone, Copy, Debug)]
pub(crate) struct TextEditorSpec {
    pub background: Color,
    pub border_color: Color,
    pub border_radius: f32,
    pub border_width: f32,
    pub placeholder_color: Color,
    pub selection_color: Color,
    pub value_color: Color,
}

#[derive(Clone, Copy, Debug)]
pub(crate) struct ContainerSpec {
    pub background: Color,
    pub border_color: Color,
    pub border_radius: f32,
    pub border_width: f32,
    pub text_color: Color,
}

#[derive(Clone, Copy, Debug)]
pub(crate) struct ScrollableSpec {
    pub background: Color,
    pub border_color: Color,
    pub border_radius: f32,
    pub border_width: f32,
    pub scroller_color: Color,
}

#[derive(Clone, Copy, Debug)]
pub(crate) struct ProgressBarSpec {
    pub background: Color,
    pub bar_color: Color,
    pub border_radius: f32,
}

#[derive(Clone, Copy, Debug)]
pub(crate) struct RuleSpec {
    pub color: Color,
    pub radius: f32,
    pub width: f32,
}

#[derive(Clone, Copy, Debug)]
pub(crate) struct MenuSpec {
    pub background: Color,
    pub border_color: Color,
    pub border_radius: f32,
    pub border_width: f32,
    pub selected_background: Color,
    pub selected_text_color: Color,
    pub text_color: Color,
}

// --- Color adjustment helpers ---

fn hover_adjust(color: Color, is_dark: bool) -> Color {
    if is_dark {
        Color::from_rgba(
            (color.r + 0.15).min(1.0),
            (color.g + 0.15).min(1.0),
            (color.b + 0.15).min(1.0),
            color.a,
        )
    } else {
        Color::from_rgba(
            (color.r - 0.10).max(0.0),
            (color.g - 0.10).max(0.0),
            (color.b - 0.10).max(0.0),
            color.a,
        )
    }
}

fn dim(color: Color) -> Color {
    Color::from_rgba(color.r, color.g, color.b, color.a * 0.5)
}

// --- Resolve methods ---

impl ButtonSpec {
    fn resolve(&self, is_dark: bool, status: button::Status) -> button::Style {
        let base = button::Style {
            background: Some(self.background.into()),
            text_color: self.text_color,
            border: Border {
                color: self.border_color,
                width: self.border_width,
                radius: self.border_radius.into(),
            },
            shadow: Shadow::default(),
            snap: false,
        };
        match status {
            button::Status::Active | button::Status::Pressed => base,
            button::Status::Hovered => button::Style {
                background: Some(hover_adjust(self.background, is_dark).into()),
                ..base
            },
            button::Status::Disabled => button::Style {
                background: Some(dim(self.background).into()),
                text_color: dim(self.text_color),
                ..base
            },
        }
    }
}

impl CheckboxSpec {
    fn resolve(&self, is_dark: bool, status: checkbox::Status) -> checkbox::Style {
        let (bg, is_hovered) = match status {
            checkbox::Status::Active { is_checked } => {
                (if is_checked { self.accent } else { self.background }, false)
            }
            checkbox::Status::Hovered { is_checked } => {
                (if is_checked { self.accent } else { self.background }, true)
            }
            checkbox::Status::Disabled { is_checked } => {
                let c = if is_checked { self.accent } else { self.background };
                return checkbox::Style {
                    background: dim(c).into(),
                    icon_color: dim(self.icon_color),
                    border: Border {
                        color: dim(self.border_color),
                        width: self.border_width,
                        radius: self.border_radius.into(),
                    },
                    text_color: Some(dim(self.text_color)),
                };
            }
        };
        let bg = if is_hovered { hover_adjust(bg, is_dark) } else { bg };
        checkbox::Style {
            background: bg.into(),
            icon_color: self.icon_color,
            border: Border {
                color: self.border_color,
                width: self.border_width,
                radius: self.border_radius.into(),
            },
            text_color: Some(self.text_color),
        }
    }
}

impl TextInputSpec {
    fn resolve(&self, is_dark: bool, status: text_input::Status) -> text_input::Style {
        let bg = match status {
            text_input::Status::Active => self.background,
            text_input::Status::Hovered | text_input::Status::Focused { .. } => {
                hover_adjust(self.background, is_dark)
            }
            text_input::Status::Disabled => dim(self.background),
        };
        text_input::Style {
            background: bg.into(),
            border: Border {
                color: self.border_color,
                width: self.border_width,
                radius: self.border_radius.into(),
            },
            icon: self.icon_color,
            placeholder: self.placeholder_color,
            value: self.value_color,
            selection: self.selection_color,
        }
    }
}

impl TogglerSpec {
    fn resolve(&self, is_dark: bool, status: toggler::Status) -> toggler::Style {
        let is_hovered = matches!(status, toggler::Status::Hovered { .. });
        let is_disabled = matches!(status, toggler::Status::Disabled { .. });
        let bg = if is_disabled {
            dim(self.background)
        } else if is_hovered {
            hover_adjust(self.background, is_dark)
        } else {
            self.background
        };
        toggler::Style {
            background: bg.into(),
            background_border_width: 1.0,
            background_border_color: if is_disabled {
                dim(self.background_border_color)
            } else {
                self.background_border_color
            },
            foreground: if is_disabled { dim(self.foreground) } else { self.foreground }.into(),
            foreground_border_width: 1.0,
            foreground_border_color: if is_disabled {
                dim(self.foreground_border_color)
            } else {
                self.foreground_border_color
            },
            text_color: Some(if is_disabled { dim(self.text_color) } else { self.text_color }),
            border_radius: Some(self.border_radius.into()),
            padding_ratio: 0.1,
        }
    }
}

impl SliderSpec {
    fn resolve(&self, is_dark: bool, status: slider::Status) -> slider::Style {
        let is_hovered = matches!(status, slider::Status::Hovered);
        let handle_bg = if is_hovered {
            hover_adjust(self.handle_color, is_dark)
        } else {
            self.handle_color
        };
        slider::Style {
            rail: slider::Rail {
                backgrounds: (self.rail_fill_color.into(), self.rail_color.into()),
                width: self.rail_width,
                border: Border::default(),
            },
            handle: slider::Handle {
                shape: slider::HandleShape::Circle { radius: self.handle_radius },
                background: handle_bg.into(),
                border_width: self.handle_border_width,
                border_color: self.handle_border_color,
            },
        }
    }
}

impl RadioSpec {
    fn resolve(&self, is_dark: bool, status: radio::Status) -> radio::Style {
        let is_hovered = matches!(status, radio::Status::Hovered { .. });
        let bg = if is_hovered {
            hover_adjust(self.background, is_dark)
        } else {
            self.background
        };
        radio::Style {
            background: bg.into(),
            dot_color: self.dot_color,
            border_width: self.border_width,
            border_color: self.border_color,
            text_color: Some(self.text_color),
        }
    }
}

impl PickListSpec {
    fn resolve(&self, is_dark: bool, status: pick_list::Status) -> pick_list::Style {
        let is_hovered = matches!(
            status,
            pick_list::Status::Hovered | pick_list::Status::Opened { .. }
        );
        let bg = if is_hovered {
            hover_adjust(self.background, is_dark)
        } else {
            self.background
        };
        pick_list::Style {
            text_color: self.text_color,
            placeholder_color: self.placeholder_color,
            handle_color: self.handle_color,
            background: bg.into(),
            border: Border {
                color: self.border_color,
                width: self.border_width,
                radius: self.border_radius.into(),
            },
        }
    }
}

impl TextEditorSpec {
    fn resolve(&self, is_dark: bool, status: text_editor::Status) -> text_editor::Style {
        let bg = match status {
            text_editor::Status::Active => self.background,
            text_editor::Status::Hovered | text_editor::Status::Focused { .. } => {
                hover_adjust(self.background, is_dark)
            }
            text_editor::Status::Disabled => dim(self.background),
        };
        text_editor::Style {
            background: bg.into(),
            border: Border {
                color: self.border_color,
                width: self.border_width,
                radius: self.border_radius.into(),
            },
            placeholder: self.placeholder_color,
            value: self.value_color,
            selection: self.selection_color,
        }
    }
}

impl ContainerSpec {
    fn resolve(&self) -> container::Style {
        container::Style {
            text_color: Some(self.text_color),
            background: Some(self.background.into()),
            border: Border {
                color: self.border_color,
                width: self.border_width,
                radius: self.border_radius.into(),
            },
            shadow: Shadow::default(),
            snap: false,
        }
    }
}

impl ScrollableSpec {
    fn resolve(&self, is_dark: bool, status: scrollable::Status) -> scrollable::Style {
        let is_hovered = matches!(status, scrollable::Status::Hovered { .. });
        let scroller_bg = if is_hovered {
            hover_adjust(self.scroller_color, is_dark)
        } else {
            self.scroller_color
        };
        let rail = scrollable::Rail {
            background: Some(self.background.into()),
            border: Border {
                color: self.border_color,
                width: self.border_width,
                radius: self.border_radius.into(),
            },
            scroller: scrollable::Scroller {
                background: scroller_bg.into(),
                border: Border::default(),
            },
        };
        scrollable::Style {
            container: container::Style::default(),
            vertical_rail: rail,
            horizontal_rail: rail,
            gap: None,
            auto_scroll: scrollable::AutoScroll {
                background: Color::TRANSPARENT.into(),
                border: Border::default(),
                shadow: Shadow::default(),
                icon: Color::TRANSPARENT,
            },
        }
    }
}

impl ProgressBarSpec {
    fn resolve(&self) -> progress_bar::Style {
        progress_bar::Style {
            background: self.background.into(),
            bar: self.bar_color.into(),
            border: Border {
                radius: self.border_radius.into(),
                ..Border::default()
            },
        }
    }
}

impl RuleSpec {
    fn resolve(&self) -> rule::Style {
        rule::Style {
            color: self.color,
            radius: self.radius.into(),
            fill_mode: rule::FillMode::Percent(self.width),
            snap: false,
        }
    }
}

impl MenuSpec {
    fn resolve(&self) -> menu::Style {
        menu::Style {
            background: self.background.into(),
            border: Border {
                color: self.border_color,
                width: self.border_width,
                radius: self.border_radius.into(),
            },
            text_color: self.text_color,
            selected_text_color: self.selected_text_color,
            selected_background: self.selected_background.into(),
            shadow: Shadow::default(),
        }
    }
}

// --- Catalog trait implementations ---

// Macros to reduce boilerplate for the common Catalog patterns.

macro_rules! impl_catalog_with_status {
    ($module:ident, $field:ident, $fallback:expr) => {
        impl $module::Catalog for GraphixTheme {
            type Class<'a> = $module::StyleFn<'a, Self>;

            fn default<'a>() -> Self::Class<'a> {
                Box::new(|theme, status| {
                    if let Some(spec) =
                        theme.overrides.as_ref().and_then(|o| o.$field.as_ref())
                    {
                        spec.resolve(theme.inner.extended_palette().is_dark, status)
                    } else {
                        #[allow(clippy::redundant_closure_call)]
                        ($fallback)(&theme.inner, status)
                    }
                })
            }

            fn style(&self, class: &Self::Class<'_>, status: $module::Status) -> $module::Style {
                class(self, status)
            }
        }
    };
}

macro_rules! impl_catalog_no_status {
    ($module:ident, $field:ident, $fallback:expr) => {
        impl $module::Catalog for GraphixTheme {
            type Class<'a> = $module::StyleFn<'a, Self>;

            fn default<'a>() -> Self::Class<'a> {
                Box::new(|theme| {
                    if let Some(spec) =
                        theme.overrides.as_ref().and_then(|o| o.$field.as_ref())
                    {
                        spec.resolve()
                    } else {
                        #[allow(clippy::redundant_closure_call)]
                        ($fallback)(&theme.inner)
                    }
                })
            }

            fn style(&self, class: &Self::Class<'_>) -> $module::Style {
                class(self)
            }
        }
    };
}

impl_catalog_with_status!(button, button, button::primary);
impl_catalog_with_status!(checkbox, checkbox, checkbox::primary);
impl_catalog_with_status!(text_input, text_input, text_input::default);
impl_catalog_with_status!(toggler, toggler, toggler::default);
impl_catalog_with_status!(slider, slider, slider::default);
impl_catalog_with_status!(radio, radio, radio::default);
// pick_list: needs fully qualified paths due to supertrait ambiguity
impl pick_list::Catalog for GraphixTheme {
    type Class<'a> = pick_list::StyleFn<'a, Self>;

    fn default<'a>() -> <Self as pick_list::Catalog>::Class<'a> {
        Box::new(|theme, status| {
            if let Some(spec) = theme.overrides.as_ref().and_then(|o| o.pick_list.as_ref()) {
                spec.resolve(theme.inner.extended_palette().is_dark, status)
            } else {
                pick_list::default(&theme.inner, status)
            }
        })
    }

    fn style(
        &self,
        class: &<Self as pick_list::Catalog>::Class<'_>,
        status: pick_list::Status,
    ) -> pick_list::Style {
        class(self, status)
    }
}
impl_catalog_with_status!(text_editor, text_editor, text_editor::default);

// Scrollable has status but its resolve takes is_dark
impl scrollable::Catalog for GraphixTheme {
    type Class<'a> = scrollable::StyleFn<'a, Self>;

    fn default<'a>() -> Self::Class<'a> {
        Box::new(|theme, status| {
            if let Some(spec) = theme.overrides.as_ref().and_then(|o| o.scrollable.as_ref()) {
                spec.resolve(theme.inner.extended_palette().is_dark, status)
            } else {
                scrollable::default(&theme.inner, status)
            }
        })
    }

    fn style(&self, class: &Self::Class<'_>, status: scrollable::Status) -> scrollable::Style {
        class(self, status)
    }
}

impl_catalog_no_status!(container, container, container::transparent);
impl_catalog_no_status!(progress_bar, progress_bar, progress_bar::primary);
impl_catalog_no_status!(rule, rule, rule::default);

// Menu: needs fully qualified paths due to supertrait ambiguity with scrollable
impl menu::Catalog for GraphixTheme {
    type Class<'a> = menu::StyleFn<'a, Self>;

    fn default<'a>() -> <Self as menu::Catalog>::Class<'a> {
        Box::new(|theme| {
            if let Some(spec) = theme.overrides.as_ref().and_then(|o| o.menu.as_ref()) {
                spec.resolve()
            } else {
                menu::default(&theme.inner)
            }
        })
    }

    fn style(&self, class: &<Self as menu::Catalog>::Class<'_>) -> menu::Style {
        class(self)
    }
}

// ComboBox: supertrait of text_input + menu; empty impl uses defaults
impl combo_box::Catalog for GraphixTheme {}

// Delegate-only: text
impl iced_core::widget::text::Catalog for GraphixTheme {
    type Class<'a> = iced_core::widget::text::StyleFn<'a, Self>;

    fn default<'a>() -> Self::Class<'a> {
        Box::new(|_theme| iced_core::widget::text::Style::default())
    }

    fn style(&self, class: &Self::Class<'_>) -> iced_core::widget::text::Style {
        class(self)
    }
}

// Delegate-only: svg
impl svg::Catalog for GraphixTheme {
    type Class<'a> = svg::StyleFn<'a, Self>;

    fn default<'a>() -> Self::Class<'a> {
        Box::new(|_theme, _status| svg::Style::default())
    }

    fn style(&self, class: &Self::Class<'_>, status: svg::Status) -> svg::Style {
        class(self, status)
    }
}

// theme::Base — required supertrait for text_editor::Catalog
impl iced_core::theme::Base for GraphixTheme {
    fn default(preference: iced_core::theme::Mode) -> Self {
        GraphixTheme {
            inner: iced_core::theme::Base::default(preference),
            overrides: None,
        }
    }

    fn mode(&self) -> iced_core::theme::Mode {
        self.inner.mode()
    }

    fn base(&self) -> iced_core::theme::Style {
        self.inner.base()
    }

    fn palette(&self) -> Option<iced_core::theme::palette::Palette> {
        iced_core::theme::Base::palette(&self.inner)
    }

    fn name(&self) -> &str {
        self.inner.name()
    }
}
