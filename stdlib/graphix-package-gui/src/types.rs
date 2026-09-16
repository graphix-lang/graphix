use crate::theme::{GraphixTheme, StyleOverrides};
use anyhow::{Context, Result, anyhow, bail};
use arcstr::ArcStr;
use graphix_compiler::abstract_value::payload as abstract_payload;
use iced_core::{
    Color, ContentFit, Font, Length, Padding, Size,
    alignment::{Horizontal, Vertical},
    font::{Family, Style, Weight},
};
use iced_widget::{scrollable, tooltip};
use netidx::publisher::{FromValue, Value};
use netidx_derive::{FromValue, IntoValue};
use smallvec::SmallVec;
use std::{
    collections::HashSet,
    sync::{LazyLock, Mutex},
};
use triomphe::Arc;

static FONT_NAMES: LazyLock<Mutex<HashSet<&'static str>>> =
    LazyLock::new(Default::default);

#[derive(Clone, Copy, Debug)]
pub struct LengthV(pub Length);

impl FromValue for LengthV {
    fn from_value(v: Value) -> Result<Self> {
        #[derive(FromValue)]
        enum Repr {
            Fill,
            Shrink,
            FillPortion(u16),
            Fixed(f32),
        }
        Ok(Self(match v.cast_to::<Repr>()? {
            Repr::Fill => Length::Fill,
            Repr::Shrink => Length::Shrink,
            Repr::FillPortion(n) => Length::FillPortion(n),
            Repr::Fixed(n) => Length::Fixed(n),
        }))
    }
}

#[derive(Clone, Copy, Debug)]
pub struct PaddingV(pub Padding);

impl FromValue for PaddingV {
    fn from_value(v: Value) -> Result<Self> {
        #[derive(FromValue)]
        enum Repr {
            All(f32),
            Axis { x: f32, y: f32 },
            Each { top: f32, right: f32, bottom: f32, left: f32 },
        }
        Ok(Self(match v.cast_to::<Repr>()? {
            Repr::All(n) => Padding::new(n),
            Repr::Axis { x, y } => Padding::from([y, x]),
            Repr::Each { top, right, bottom, left } => {
                Padding { top, right, bottom, left }
            }
        }))
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct SizeV(pub Size);

impl FromValue for SizeV {
    fn from_value(v: Value) -> Result<Self> {
        #[derive(FromValue)]
        struct Fields {
            width: f32,
            height: f32,
        }
        let Fields { width, height } = v.cast_to()?;
        Ok(Self(Size::new(width, height)))
    }
}

impl From<SizeV> for Value {
    fn from(SizeV(s): SizeV) -> Value {
        #[derive(IntoValue)]
        struct Fields {
            width: f64,
            height: f64,
        }
        Fields { width: s.width as f64, height: s.height as f64 }.into()
    }
}

#[derive(Clone, Copy, Debug)]
pub struct ColorV(pub Color);

impl FromValue for ColorV {
    fn from_value(v: Value) -> Result<Self> {
        #[derive(FromValue)]
        struct Fields {
            r: f32,
            g: f32,
            b: f32,
            a: f32,
        }
        let Fields { r, g, b, a } = abstract_payload(&v)
            .ok_or_else(|| anyhow!("expected a Color, got {v}"))?
            .clone()
            .cast_to()?;
        if !(0.0..=1.0).contains(&r)
            || !(0.0..=1.0).contains(&g)
            || !(0.0..=1.0).contains(&b)
            || !(0.0..=1.0).contains(&a)
        {
            bail!("color components must be in [0, 1], got r={r} g={g} b={b} a={a}");
        }
        Ok(Self(Color::from_rgba(r, g, b, a)))
    }
}

#[derive(Clone, Copy, Debug)]
pub struct HAlignV(pub Horizontal);

impl FromValue for HAlignV {
    fn from_value(v: Value) -> Result<Self> {
        match &*v.cast_to::<ArcStr>()? {
            "Left" => Ok(Self(Horizontal::Left)),
            "Center" => Ok(Self(Horizontal::Center)),
            "Right" => Ok(Self(Horizontal::Right)),
            s => bail!("invalid halign {s}"),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct VAlignV(pub Vertical);

impl FromValue for VAlignV {
    fn from_value(v: Value) -> Result<Self> {
        match &*v.cast_to::<ArcStr>()? {
            "Top" => Ok(Self(Vertical::Top)),
            "Center" => Ok(Self(Vertical::Center)),
            "Bottom" => Ok(Self(Vertical::Bottom)),
            s => bail!("invalid valign {s}"),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct FontV(pub Font);

impl FromValue for FontV {
    fn from_value(v: Value) -> Result<Self> {
        #[derive(FromValue)]
        struct Fields {
            family: Value,
            weight: ArcStr,
            style: ArcStr,
        }
        let Fields { family, weight, style } = v.cast_to()?;
        let family = match family {
            Value::String(s) => match &*s {
                "SansSerif" => Family::SansSerif,
                "Serif" => Family::Serif,
                "Monospace" => Family::Monospace,
                s => bail!("invalid font family {s}"),
            },
            v => match v.cast_to::<(ArcStr, Value)>()? {
                (s, v) if &*s == "Name" => {
                    let name = v.cast_to::<ArcStr>()?;
                    let mut cache = FONT_NAMES.lock().unwrap();
                    let interned = match cache.get(name.as_str()) {
                        Some(&s) => s,
                        None => {
                            let leaked: &'static str =
                                Box::leak(name.to_string().into_boxed_str());
                            cache.insert(leaked);
                            leaked
                        }
                    };
                    Family::Name(interned)
                }
                (s, _) => bail!("invalid font family {s}"),
            },
        };
        let weight = match &*weight {
            "Thin" => Weight::Thin,
            "ExtraLight" => Weight::ExtraLight,
            "Light" => Weight::Light,
            "Normal" => Weight::Normal,
            "Medium" => Weight::Medium,
            "SemiBold" => Weight::Semibold,
            "Bold" => Weight::Bold,
            "ExtraBold" => Weight::ExtraBold,
            "Black" => Weight::Black,
            s => bail!("invalid font weight {s}"),
        };
        let style = match &*style {
            "Normal" => Style::Normal,
            "Italic" => Style::Italic,
            "Oblique" => Style::Oblique,
            s => bail!("invalid font style {s}"),
        };
        Ok(Self(Font { family, weight, style, ..Font::DEFAULT }))
    }
}

#[derive(Clone, Copy, Debug)]
pub struct PaletteV(pub iced_core::theme::palette::Palette);

impl FromValue for PaletteV {
    fn from_value(v: Value) -> Result<Self> {
        #[derive(FromValue)]
        struct Fields {
            background: ColorV,
            text: ColorV,
            primary: ColorV,
            success: ColorV,
            warning: ColorV,
            danger: ColorV,
        }
        let Fields { background, text, primary, success, warning, danger } =
            v.cast_to()?;
        Ok(Self(iced_core::theme::palette::Palette {
            background: background.0,
            text: text.0,
            primary: primary.0,
            success: success.0,
            warning: warning.0,
            danger: danger.0,
        }))
    }
}

#[derive(Clone, Debug)]
pub struct ThemeV(pub GraphixTheme);

impl FromValue for ThemeV {
    fn from_value(v: Value) -> Result<Self> {
        use iced_core::Theme;
        match v {
            Value::String(s) => {
                let inner = match &*s {
                    "Light" => Theme::Light,
                    "Dark" => Theme::Dark,
                    "Dracula" => Theme::Dracula,
                    "Nord" => Theme::Nord,
                    "SolarizedLight" => Theme::SolarizedLight,
                    "SolarizedDark" => Theme::SolarizedDark,
                    "GruvboxLight" => Theme::GruvboxLight,
                    "GruvboxDark" => Theme::GruvboxDark,
                    "CatppuccinLatte" => Theme::CatppuccinLatte,
                    "CatppuccinFrappe" => Theme::CatppuccinFrappe,
                    "CatppuccinMacchiato" => Theme::CatppuccinMacchiato,
                    "CatppuccinMocha" => Theme::CatppuccinMocha,
                    "TokyoNight" => Theme::TokyoNight,
                    "TokyoNightStorm" => Theme::TokyoNightStorm,
                    "TokyoNightLight" => Theme::TokyoNightLight,
                    "KanagawaWave" => Theme::KanagawaWave,
                    "KanagawaDragon" => Theme::KanagawaDragon,
                    "KanagawaLotus" => Theme::KanagawaLotus,
                    "Moonfly" => Theme::Moonfly,
                    "Nightfly" => Theme::Nightfly,
                    "Oxocarbon" => Theme::Oxocarbon,
                    "Ferra" => Theme::Ferra,
                    s => bail!("invalid theme {s}"),
                };
                Ok(Self(GraphixTheme { inner, overrides: None }))
            }
            v => match v.cast_to::<(ArcStr, Value)>()? {
                (s, v) if &*s == "CustomPalette" => {
                    let palette = PaletteV::from_value(v)?;
                    Ok(Self(GraphixTheme {
                        inner: Theme::custom("Custom", palette.0),
                        overrides: None,
                    }))
                }
                (s, v) if &*s == "Custom" => {
                    #[derive(FromValue)]
                    struct Fields {
                        palette: PaletteV,
                    }
                    let Fields { palette } = v.clone().cast_to()?;
                    let overrides = v.cast_to::<StyleOverrides>()?;
                    Ok(Self(GraphixTheme {
                        inner: Theme::custom("Custom", palette.0),
                        overrides: Some(Arc::new(overrides)),
                    }))
                }
                (s, _) => bail!("invalid theme {s}"),
            },
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct ScrollDirectionV(pub scrollable::Direction);

impl FromValue for ScrollDirectionV {
    fn from_value(v: Value) -> Result<Self> {
        match &*v.cast_to::<ArcStr>()? {
            "Vertical" => Ok(Self(scrollable::Direction::Vertical(
                scrollable::Scrollbar::default(),
            ))),
            "Horizontal" => Ok(Self(scrollable::Direction::Horizontal(
                scrollable::Scrollbar::default(),
            ))),
            "Both" => Ok(Self(scrollable::Direction::Both {
                vertical: scrollable::Scrollbar::default(),
                horizontal: scrollable::Scrollbar::default(),
            })),
            s => bail!("invalid scroll direction {s}"),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct TooltipPositionV(pub tooltip::Position);

impl FromValue for TooltipPositionV {
    fn from_value(v: Value) -> Result<Self> {
        match &*v.cast_to::<ArcStr>()? {
            "Top" => Ok(Self(tooltip::Position::Top)),
            "Bottom" => Ok(Self(tooltip::Position::Bottom)),
            "Left" => Ok(Self(tooltip::Position::Left)),
            "Right" => Ok(Self(tooltip::Position::Right)),
            "FollowCursor" => Ok(Self(tooltip::Position::FollowCursor)),
            s => bail!("invalid tooltip position {s}"),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct ContentFitV(pub ContentFit);

impl FromValue for ContentFitV {
    fn from_value(v: Value) -> Result<Self> {
        match &*v.cast_to::<ArcStr>()? {
            "Fill" => Ok(Self(ContentFit::Fill)),
            "Contain" => Ok(Self(ContentFit::Contain)),
            "Cover" => Ok(Self(ContentFit::Cover)),
            "None" => Ok(Self(ContentFit::None)),
            "ScaleDown" => Ok(Self(ContentFit::ScaleDown)),
            s => bail!("invalid content fit {s}"),
        }
    }
}

/// Image source: file path, raw encoded bytes, inline SVG, or decoded RGBA pixels.
#[derive(Clone, Debug)]
pub enum ImageSourceV {
    Path(String),
    Bytes(iced_core::Bytes),
    Svg(String),
    Rgba { width: u32, height: u32, pixels: iced_core::Bytes },
}

impl ImageSourceV {
    pub fn is_svg(&self) -> bool {
        match self {
            Self::Path(p) => p.ends_with(".svg") || p.ends_with(".svgz"),
            Self::Svg(_) => true,
            _ => false,
        }
    }

    pub fn to_handle(&self) -> iced_core::image::Handle {
        match self {
            Self::Path(p) => iced_core::image::Handle::from_path(p),
            Self::Bytes(b) => iced_core::image::Handle::from_bytes(b.clone()),
            Self::Svg(_) => iced_core::image::Handle::from_path(""),
            Self::Rgba { width, height, pixels } => {
                iced_core::image::Handle::from_rgba(*width, *height, pixels.clone())
            }
        }
    }

    pub fn to_svg_handle(&self) -> iced_core::svg::Handle {
        match self {
            Self::Path(p) => iced_core::svg::Handle::from_path(p),
            Self::Bytes(b) => iced_core::svg::Handle::from_memory(b.to_vec()),
            Self::Svg(s) => iced_core::svg::Handle::from_memory(s.as_bytes().to_vec()),
            Self::Rgba { .. } => iced_core::svg::Handle::from_path(""),
        }
    }

    pub fn decode_icon(&self) -> Result<Option<winit::window::Icon>> {
        match self {
            Self::Path(p) if p.is_empty() => Ok(None),
            Self::Path(p) if self.is_svg() => {
                let data = std::fs::read(p)?;
                decode_svg_icon(&data)
            }
            Self::Path(p) => {
                let img = ::image::open(p)?.into_rgba8();
                let (w, h) = img.dimensions();
                Ok(Some(winit::window::Icon::from_rgba(img.into_raw(), w, h)?))
            }
            Self::Bytes(b) if b.is_empty() => Ok(None),
            Self::Bytes(b) => {
                let img = ::image::load_from_memory(b)?.into_rgba8();
                let (w, h) = img.dimensions();
                Ok(Some(winit::window::Icon::from_rgba(img.into_raw(), w, h)?))
            }
            Self::Svg(s) if s.is_empty() => Ok(None),
            Self::Svg(s) => decode_svg_icon(s.as_bytes()),
            Self::Rgba { width, height, pixels } => {
                if pixels.is_empty() {
                    return Ok(None);
                }
                Ok(Some(winit::window::Icon::from_rgba(
                    pixels.to_vec(),
                    *width,
                    *height,
                )?))
            }
        }
    }
}

fn decode_svg_icon(data: &[u8]) -> Result<Option<winit::window::Icon>> {
    let tree = resvg::usvg::Tree::from_data(data, &Default::default())?;
    let size = 32;
    let svg_size = tree.size();
    let sx = size as f32 / svg_size.width();
    let sy = size as f32 / svg_size.height();
    let scale = sx.min(sy);
    let mut pixmap = resvg::tiny_skia::Pixmap::new(size, size)
        .context("failed to allocate pixmap for SVG icon")?;
    let transform = resvg::tiny_skia::Transform::from_scale(scale, scale);
    resvg::render(&tree, transform, &mut pixmap.as_mut());
    Ok(Some(winit::window::Icon::from_rgba(pixmap.data().to_vec(), size, size)?))
}

impl FromValue for ImageSourceV {
    fn from_value(v: Value) -> Result<Self> {
        match v {
            Value::String(s) => Ok(Self::Path(s.to_string())),
            Value::Bytes(b) => Ok(Self::Bytes((*b).clone())),
            v => {
                let (tag, val) = v.cast_to::<(ArcStr, Value)>()?;
                match &*tag {
                    "Bytes" => match val {
                        Value::Bytes(b) => Ok(Self::Bytes((*b).clone())),
                        _ => bail!("ImageSource Bytes: expected bytes value"),
                    },
                    "Svg" => Ok(Self::Svg(val.cast_to::<String>()?)),
                    "Rgba" => {
                        #[derive(FromValue)]
                        struct Fields {
                            width: u32,
                            height: u32,
                            pixels: iced_core::Bytes,
                        }
                        let Fields { width, height, pixels } = val.cast_to()?;
                        Ok(Self::Rgba { width, height, pixels })
                    }
                    s => bail!("invalid ImageSource variant: {s}"),
                }
            }
        }
    }
}

#[derive(Clone, Copy, Debug, FromValue)]
pub enum GridColumnsV {
    Fixed(usize),
    Fluid(f32),
}

#[derive(Clone, Copy, Debug)]
pub struct GridSizingV(pub iced_widget::grid::Sizing);

impl FromValue for GridSizingV {
    fn from_value(v: Value) -> Result<Self> {
        use iced_widget::grid::Sizing;
        #[derive(FromValue)]
        enum Repr {
            AspectRatio(f32),
            EvenlyDistribute(LengthV),
        }
        Ok(Self(match v.cast_to::<Repr>()? {
            Repr::AspectRatio(r) => Sizing::AspectRatio(r),
            Repr::EvenlyDistribute(l) => Sizing::EvenlyDistribute(l.0),
        }))
    }
}

/// Parsed shortcut from the Graphix `Shortcut` struct.
#[derive(Clone, Debug)]
pub struct ShortcutV {
    pub display: String,
    pub key: iced_core::keyboard::Key,
    pub modifiers: iced_core::keyboard::Modifiers,
}

impl FromValue for ShortcutV {
    fn from_value(v: Value) -> Result<Self> {
        #[derive(FromValue)]
        struct Fields {
            alt: bool,
            ctrl: bool,
            key: ArcStr,
            logo: bool,
            shift: bool,
        }
        let Fields { alt, ctrl, key, logo, shift } = abstract_payload(&v)
            .ok_or_else(|| anyhow!("expected a Shortcut, got {v}"))?
            .clone()
            .cast_to()?;
        let mut display = String::new();
        if ctrl {
            display.push_str("Ctrl+");
        }
        if alt {
            display.push_str("Alt+");
        }
        if shift {
            display.push_str("Shift+");
        }
        if logo {
            display.push_str("Super+");
        }
        display.push_str(&key.to_uppercase());
        let mut modifiers = iced_core::keyboard::Modifiers::empty();
        if ctrl {
            modifiers |= iced_core::keyboard::Modifiers::CTRL;
        }
        if alt {
            modifiers |= iced_core::keyboard::Modifiers::ALT;
        }
        if shift {
            modifiers |= iced_core::keyboard::Modifiers::SHIFT;
        }
        if logo {
            modifiers |= iced_core::keyboard::Modifiers::LOGO;
        }
        let iced_key = iced_core::keyboard::Key::Character(key.to_lowercase().into());
        Ok(Self { display, key: iced_key, modifiers })
    }
}

/// Newtype for `Vec<String>` to satisfy orphan rules.
#[derive(Clone, Debug)]
pub struct StringVec(pub Vec<String>);

impl FromValue for StringVec {
    fn from_value(v: Value) -> Result<Self> {
        let items = v.cast_to::<SmallVec<[Value; 8]>>()?;
        let v: Vec<String> =
            items.into_iter().map(|v| v.cast_to::<String>()).collect::<Result<_>>()?;
        Ok(Self(v))
    }
}
