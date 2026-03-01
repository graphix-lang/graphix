use iced_core::Color;
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
    pub background: Option<Color>,
    pub border_color: Option<Color>,
    pub border_radius: Option<f32>,
    pub border_width: Option<f32>,
    pub text_color: Option<Color>,
}

#[derive(Clone, Copy, Debug)]
pub(crate) struct CheckboxSpec {
    pub accent: Option<Color>,
    pub background: Option<Color>,
    pub border_color: Option<Color>,
    pub border_radius: Option<f32>,
    pub border_width: Option<f32>,
    pub icon_color: Option<Color>,
    pub text_color: Option<Color>,
}

#[derive(Clone, Copy, Debug)]
pub(crate) struct TextInputSpec {
    pub background: Option<Color>,
    pub border_color: Option<Color>,
    pub border_radius: Option<f32>,
    pub border_width: Option<f32>,
    pub icon_color: Option<Color>,
    pub placeholder_color: Option<Color>,
    pub selection_color: Option<Color>,
    pub value_color: Option<Color>,
}

#[derive(Clone, Copy, Debug)]
pub(crate) struct TogglerSpec {
    pub background: Option<Color>,
    pub background_border_color: Option<Color>,
    pub border_radius: Option<f32>,
    pub foreground: Option<Color>,
    pub foreground_border_color: Option<Color>,
    pub text_color: Option<Color>,
}

#[derive(Clone, Copy, Debug)]
pub(crate) struct SliderSpec {
    pub handle_border_color: Option<Color>,
    pub handle_border_width: Option<f32>,
    pub handle_color: Option<Color>,
    pub handle_radius: Option<f32>,
    pub rail_color: Option<Color>,
    pub rail_fill_color: Option<Color>,
    pub rail_width: Option<f32>,
}

#[derive(Clone, Copy, Debug)]
pub(crate) struct RadioSpec {
    pub background: Option<Color>,
    pub border_color: Option<Color>,
    pub border_width: Option<f32>,
    pub dot_color: Option<Color>,
    pub text_color: Option<Color>,
}

#[derive(Clone, Copy, Debug)]
pub(crate) struct PickListSpec {
    pub background: Option<Color>,
    pub border_color: Option<Color>,
    pub border_radius: Option<f32>,
    pub border_width: Option<f32>,
    pub handle_color: Option<Color>,
    pub placeholder_color: Option<Color>,
    pub text_color: Option<Color>,
}

#[derive(Clone, Copy, Debug)]
pub(crate) struct TextEditorSpec {
    pub background: Option<Color>,
    pub border_color: Option<Color>,
    pub border_radius: Option<f32>,
    pub border_width: Option<f32>,
    pub placeholder_color: Option<Color>,
    pub selection_color: Option<Color>,
    pub value_color: Option<Color>,
}

#[derive(Clone, Copy, Debug)]
pub(crate) struct ContainerSpec {
    pub background: Option<Color>,
    pub border_color: Option<Color>,
    pub border_radius: Option<f32>,
    pub border_width: Option<f32>,
    pub text_color: Option<Color>,
}

#[derive(Clone, Copy, Debug)]
pub(crate) struct ScrollableSpec {
    pub background: Option<Color>,
    pub border_color: Option<Color>,
    pub border_radius: Option<f32>,
    pub border_width: Option<f32>,
    pub scroller_color: Option<Color>,
}

#[derive(Clone, Copy, Debug)]
pub(crate) struct ProgressBarSpec {
    pub background: Option<Color>,
    pub bar_color: Option<Color>,
    pub border_radius: Option<f32>,
}

#[derive(Clone, Copy, Debug)]
pub(crate) struct RuleSpec {
    pub color: Option<Color>,
    pub radius: Option<f32>,
    pub width: Option<f32>,
}

#[derive(Clone, Copy, Debug)]
pub(crate) struct MenuSpec {
    pub background: Option<Color>,
    pub border_color: Option<Color>,
    pub border_radius: Option<f32>,
    pub border_width: Option<f32>,
    pub selected_background: Option<Color>,
    pub selected_text_color: Option<Color>,
    pub text_color: Option<Color>,
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

// --- Resolve methods (overlay pattern) ---
//
// Each resolve starts from iced's complete default style for the given
// theme + status, then selectively overrides only user-specified fields.

impl ButtonSpec {
    fn resolve(&self, theme: &iced_core::Theme, status: button::Status) -> button::Style {
        let is_dark = theme.extended_palette().is_dark;
        let mut s = button::primary(theme, status);
        if let Some(bg) = self.background {
            let bg = match status {
                button::Status::Hovered => hover_adjust(bg, is_dark),
                button::Status::Disabled => dim(bg),
                _ => bg,
            };
            s.background = Some(bg.into());
        }
        if let Some(tc) = self.text_color {
            s.text_color =
                if matches!(status, button::Status::Disabled) { dim(tc) } else { tc };
        }
        if let Some(bc) = self.border_color {
            s.border.color = bc;
        }
        if let Some(bw) = self.border_width {
            s.border.width = bw;
        }
        if let Some(br) = self.border_radius {
            s.border.radius = br.into();
        }
        s
    }
}

impl CheckboxSpec {
    fn resolve(&self, theme: &iced_core::Theme, status: checkbox::Status) -> checkbox::Style {
        let is_dark = theme.extended_palette().is_dark;
        let mut s = checkbox::primary(theme, status);
        let is_disabled = matches!(status, checkbox::Status::Disabled { .. });
        let is_hovered = matches!(status, checkbox::Status::Hovered { .. });
        let is_checked = match status {
            checkbox::Status::Active { is_checked }
            | checkbox::Status::Hovered { is_checked }
            | checkbox::Status::Disabled { is_checked } => is_checked,
        };
        if let Some(accent) = self.accent {
            if let Some(bg) = self.background {
                let c = if is_checked { accent } else { bg };
                let c = if is_disabled {
                    dim(c)
                } else if is_hovered {
                    hover_adjust(c, is_dark)
                } else {
                    c
                };
                s.background = c.into();
            } else {
                // only accent specified
                if is_checked {
                    let c = if is_disabled {
                        dim(accent)
                    } else if is_hovered {
                        hover_adjust(accent, is_dark)
                    } else {
                        accent
                    };
                    s.background = c.into();
                }
            }
        } else if let Some(bg) = self.background {
            if !is_checked {
                let c = if is_disabled {
                    dim(bg)
                } else if is_hovered {
                    hover_adjust(bg, is_dark)
                } else {
                    bg
                };
                s.background = c.into();
            }
        }
        if let Some(ic) = self.icon_color {
            s.icon_color = if is_disabled { dim(ic) } else { ic };
        }
        if let Some(tc) = self.text_color {
            s.text_color = Some(if is_disabled { dim(tc) } else { tc });
        }
        if let Some(bc) = self.border_color {
            s.border.color = if is_disabled { dim(bc) } else { bc };
        }
        if let Some(bw) = self.border_width {
            s.border.width = bw;
        }
        if let Some(br) = self.border_radius {
            s.border.radius = br.into();
        }
        s
    }
}

impl TextInputSpec {
    fn resolve(
        &self,
        theme: &iced_core::Theme,
        status: text_input::Status,
    ) -> text_input::Style {
        let is_dark = theme.extended_palette().is_dark;
        let mut s = text_input::default(theme, status);
        if let Some(bg) = self.background {
            s.background = match status {
                text_input::Status::Active => bg,
                text_input::Status::Hovered | text_input::Status::Focused { .. } => {
                    hover_adjust(bg, is_dark)
                }
                text_input::Status::Disabled => dim(bg),
            }
            .into();
        }
        if let Some(bc) = self.border_color {
            s.border.color = bc;
        }
        if let Some(bw) = self.border_width {
            s.border.width = bw;
        }
        if let Some(br) = self.border_radius {
            s.border.radius = br.into();
        }
        if let Some(ic) = self.icon_color {
            s.icon = ic;
        }
        if let Some(pc) = self.placeholder_color {
            s.placeholder = pc;
        }
        if let Some(vc) = self.value_color {
            s.value = vc;
        }
        if let Some(sc) = self.selection_color {
            s.selection = sc;
        }
        s
    }
}

impl TogglerSpec {
    fn resolve(&self, theme: &iced_core::Theme, status: toggler::Status) -> toggler::Style {
        let is_dark = theme.extended_palette().is_dark;
        let mut s = toggler::default(theme, status);
        let is_hovered = matches!(status, toggler::Status::Hovered { .. });
        let is_disabled = matches!(status, toggler::Status::Disabled { .. });
        if let Some(bg) = self.background {
            s.background = if is_disabled {
                dim(bg)
            } else if is_hovered {
                hover_adjust(bg, is_dark)
            } else {
                bg
            }
            .into();
        }
        if let Some(bbc) = self.background_border_color {
            s.background_border_color = if is_disabled { dim(bbc) } else { bbc };
        }
        if let Some(fg) = self.foreground {
            s.foreground = if is_disabled { dim(fg) } else { fg }.into();
        }
        if let Some(fbc) = self.foreground_border_color {
            s.foreground_border_color = if is_disabled { dim(fbc) } else { fbc };
        }
        if let Some(tc) = self.text_color {
            s.text_color = Some(if is_disabled { dim(tc) } else { tc });
        }
        if let Some(br) = self.border_radius {
            s.border_radius = Some(br.into());
        }
        s
    }
}

impl SliderSpec {
    fn resolve(&self, theme: &iced_core::Theme, status: slider::Status) -> slider::Style {
        let is_dark = theme.extended_palette().is_dark;
        let mut s = slider::default(theme, status);
        let is_hovered = matches!(status, slider::Status::Hovered);
        if let Some(rfc) = self.rail_fill_color {
            s.rail.backgrounds.0 = rfc.into();
        }
        if let Some(rc) = self.rail_color {
            s.rail.backgrounds.1 = rc.into();
        }
        if let Some(rw) = self.rail_width {
            s.rail.width = rw;
        }
        if let Some(hc) = self.handle_color {
            let hc = if is_hovered { hover_adjust(hc, is_dark) } else { hc };
            s.handle.background = hc.into();
        }
        if let Some(hr) = self.handle_radius {
            s.handle.shape = slider::HandleShape::Circle { radius: hr };
        }
        if let Some(hbw) = self.handle_border_width {
            s.handle.border_width = hbw;
        }
        if let Some(hbc) = self.handle_border_color {
            s.handle.border_color = hbc;
        }
        s
    }
}

impl RadioSpec {
    fn resolve(&self, theme: &iced_core::Theme, status: radio::Status) -> radio::Style {
        let is_dark = theme.extended_palette().is_dark;
        let mut s = radio::default(theme, status);
        let is_hovered = matches!(status, radio::Status::Hovered { .. });
        if let Some(bg) = self.background {
            s.background =
                if is_hovered { hover_adjust(bg, is_dark) } else { bg }.into();
        }
        if let Some(dc) = self.dot_color {
            s.dot_color = dc;
        }
        if let Some(bw) = self.border_width {
            s.border_width = bw;
        }
        if let Some(bc) = self.border_color {
            s.border_color = bc;
        }
        if let Some(tc) = self.text_color {
            s.text_color = Some(tc);
        }
        s
    }
}

impl PickListSpec {
    fn resolve(&self, theme: &iced_core::Theme, status: pick_list::Status) -> pick_list::Style {
        let is_dark = theme.extended_palette().is_dark;
        let mut s = pick_list::default(theme, status);
        let is_hovered = matches!(
            status,
            pick_list::Status::Hovered | pick_list::Status::Opened { .. }
        );
        if let Some(bg) = self.background {
            s.background =
                if is_hovered { hover_adjust(bg, is_dark) } else { bg }.into();
        }
        if let Some(tc) = self.text_color {
            s.text_color = tc;
        }
        if let Some(pc) = self.placeholder_color {
            s.placeholder_color = pc;
        }
        if let Some(hc) = self.handle_color {
            s.handle_color = hc;
        }
        if let Some(bc) = self.border_color {
            s.border.color = bc;
        }
        if let Some(bw) = self.border_width {
            s.border.width = bw;
        }
        if let Some(br) = self.border_radius {
            s.border.radius = br.into();
        }
        s
    }
}

impl TextEditorSpec {
    fn resolve(
        &self,
        theme: &iced_core::Theme,
        status: text_editor::Status,
    ) -> text_editor::Style {
        let is_dark = theme.extended_palette().is_dark;
        let mut s = text_editor::default(theme, status);
        if let Some(bg) = self.background {
            s.background = match status {
                text_editor::Status::Active => bg,
                text_editor::Status::Hovered | text_editor::Status::Focused { .. } => {
                    hover_adjust(bg, is_dark)
                }
                text_editor::Status::Disabled => dim(bg),
            }
            .into();
        }
        if let Some(bc) = self.border_color {
            s.border.color = bc;
        }
        if let Some(bw) = self.border_width {
            s.border.width = bw;
        }
        if let Some(br) = self.border_radius {
            s.border.radius = br.into();
        }
        if let Some(pc) = self.placeholder_color {
            s.placeholder = pc;
        }
        if let Some(vc) = self.value_color {
            s.value = vc;
        }
        if let Some(sc) = self.selection_color {
            s.selection = sc;
        }
        s
    }
}

impl ContainerSpec {
    fn resolve(&self, theme: &iced_core::Theme) -> container::Style {
        let mut s = container::transparent(theme);
        if let Some(bg) = self.background {
            s.background = Some(bg.into());
        }
        if let Some(tc) = self.text_color {
            s.text_color = Some(tc);
        }
        if let Some(bc) = self.border_color {
            s.border.color = bc;
        }
        if let Some(bw) = self.border_width {
            s.border.width = bw;
        }
        if let Some(br) = self.border_radius {
            s.border.radius = br.into();
        }
        s
    }
}

impl ScrollableSpec {
    fn resolve(
        &self,
        theme: &iced_core::Theme,
        status: scrollable::Status,
    ) -> scrollable::Style {
        let is_dark = theme.extended_palette().is_dark;
        let mut s = scrollable::default(theme, status);
        // Apply to both rails symmetrically
        for rail in [&mut s.vertical_rail, &mut s.horizontal_rail] {
            if let Some(bg) = self.background {
                rail.background = Some(bg.into());
            }
            if let Some(bc) = self.border_color {
                rail.border.color = bc;
            }
            if let Some(bw) = self.border_width {
                rail.border.width = bw;
            }
            if let Some(br) = self.border_radius {
                rail.border.radius = br.into();
            }
            if let Some(sc) = self.scroller_color {
                let is_hovered = matches!(status, scrollable::Status::Hovered { .. });
                let sc = if is_hovered { hover_adjust(sc, is_dark) } else { sc };
                rail.scroller.background = sc.into();
            }
        }
        s
    }
}

impl ProgressBarSpec {
    fn resolve(&self, theme: &iced_core::Theme) -> progress_bar::Style {
        let mut s = progress_bar::primary(theme);
        if let Some(bg) = self.background {
            s.background = bg.into();
        }
        if let Some(bar) = self.bar_color {
            s.bar = bar.into();
        }
        if let Some(br) = self.border_radius {
            s.border.radius = br.into();
        }
        s
    }
}

impl RuleSpec {
    fn resolve(&self, theme: &iced_core::Theme) -> rule::Style {
        let mut s = rule::default(theme);
        if let Some(c) = self.color {
            s.color = c;
        }
        if let Some(r) = self.radius {
            s.radius = r.into();
        }
        if let Some(w) = self.width {
            s.fill_mode = rule::FillMode::Percent(w);
        }
        s
    }
}

impl MenuSpec {
    fn resolve(&self, theme: &iced_core::Theme) -> menu::Style {
        let mut s = menu::default(theme);
        if let Some(bg) = self.background {
            s.background = bg.into();
        }
        if let Some(bc) = self.border_color {
            s.border.color = bc;
        }
        if let Some(bw) = self.border_width {
            s.border.width = bw;
        }
        if let Some(br) = self.border_radius {
            s.border.radius = br.into();
        }
        if let Some(tc) = self.text_color {
            s.text_color = tc;
        }
        if let Some(stc) = self.selected_text_color {
            s.selected_text_color = stc;
        }
        if let Some(sb) = self.selected_background {
            s.selected_background = sb.into();
        }
        s
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
                        spec.resolve(&theme.inner, status)
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
                        spec.resolve(&theme.inner)
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
                spec.resolve(&theme.inner, status)
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

impl scrollable::Catalog for GraphixTheme {
    type Class<'a> = scrollable::StyleFn<'a, Self>;

    fn default<'a>() -> Self::Class<'a> {
        Box::new(|theme, status| {
            if let Some(spec) = theme.overrides.as_ref().and_then(|o| o.scrollable.as_ref()) {
                spec.resolve(&theme.inner, status)
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
                spec.resolve(&theme.inner)
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
