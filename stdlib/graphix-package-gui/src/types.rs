use crate::theme::{GraphixTheme, StyleOverrides};
use anyhow::{Context, Result, anyhow, bail};
use arcstr::ArcStr;
use graphix_compiler::{abstract_value::payload as abstract_payload, rep};
use iced_core::{
    Color, ContentFit, Font, Length, Padding, Size,
    alignment::{Horizontal, Vertical},
    font::{Family, Style, Weight},
};
use iced_widget::{scrollable, tooltip};
use netidx::publisher::{FromValue, Value};
use netidx_derive::{FromValue, IntoValue};
use parking_lot::Mutex;
use smallvec::SmallVec;
use std::{collections::HashSet, sync::LazyLock};
use triomphe::Arc;

// iced requires static family names; each distinct name is allocated once and never freed.
static FONT_NAMES: LazyLock<Mutex<HashSet<&'static str>>> =
    LazyLock::new(Default::default);

#[derive(Clone, Copy, Debug)]
pub struct LengthV(pub Length);

impl FromValue for LengthV {
    fn from_value(v: Value) -> Result<Self> {
        #[derive(FromValue)]
        #[value(shape = "graphix_compiler::rep::GraphixShape")]
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

/// A text size in pixels: finite, positive and at most 4096 as an f32.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct TextSizeV(pub f32);

impl FromValue for TextSizeV {
    fn from_value(v: Value) -> Result<Self> {
        let f = v.cast_to::<f64>()? as f32;
        if f.is_finite() && f > 0.0 && f <= 4096.0 {
            Ok(Self(f))
        } else {
            bail!("a text size must be finite, positive and at most 4096 pixels, got {f}")
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct PaddingV(pub Padding);

impl FromValue for PaddingV {
    fn from_value(v: Value) -> Result<Self> {
        #[derive(FromValue)]
        #[value(shape = "graphix_compiler::rep::GraphixShape")]
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
        #[value(shape = "graphix_compiler::rep::GraphixShape")]
        struct Fields {
            width: Finite,
            height: Finite,
        }
        let Fields { width, height } = v.cast_to()?;
        Ok(Self(Size::new(width.0, height.0)))
    }
}

// XCR claude for claude: [bug/medium] A window size is only checked for finiteness.
// winit's X11 create_window converts the physical size to u16 with
// `try_into().unwrap()`, so `size: {width: 70000.0, ..}` panics the event loop on X11;
// zero and negative sizes also reach winit. Refuse a dimension that is not positive or
// exceeds a bound (e.g. 16384), so it becomes a logged error like NaN. Found by reading
// winit-0.30.13 x11/window.rs; not run (no display). Program:
// design/review-2026-10-19/repro/gui-core-x11-window-size-panic.gx (gui-core-04)
// 2026-10-19 claude: The window's size is now its own type, WindowSizeV, whose read
// refuses an edge that is not positive or is past 8192 logical pixels (room for a scale
// factor of 7 within X11's 16 bits); a canvas Rect's SizeV keeps taking any finite
// size. The bound 8192 is my choice. Pin:
// types::tests::window_sizes_fit_the_window_system.
/// A window's size: positive, and no larger than every window system
/// takes.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct WindowSizeV(pub Size);

/// The largest window edge in logical pixels: X11 takes a window's size
/// in 16 bits of physical pixels, which this leaves room to scale into.
const MAX_WINDOW_EDGE: f32 = 8192.;

impl FromValue for WindowSizeV {
    fn from_value(v: Value) -> Result<Self> {
        let SizeV(size) = v.cast_to()?;
        for edge in [size.width, size.height] {
            if !(edge > 0. && edge <= MAX_WINDOW_EDGE) {
                bail!(
                    "a window edge must be positive and at most {MAX_WINDOW_EDGE}, not {edge}"
                )
            }
        }
        Ok(Self(size))
    }
}

impl From<WindowSizeV> for Value {
    fn from(WindowSizeV(s): WindowSizeV) -> Value {
        SizeV(s).into()
    }
}

/// An f32 that is a number: NaN and the infinities are refused, which
/// geometry would otherwise carry to an assert or a failed tessellation.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Finite(pub f32);

impl FromValue for Finite {
    fn from_value(v: Value) -> Result<Self> {
        let f = v.cast_to::<f64>()? as f32;
        if f.is_finite() {
            Ok(Self(f))
        } else {
            bail!("expected a finite number, got {f}")
        }
    }
}

impl From<SizeV> for Value {
    fn from(SizeV(s): SizeV) -> Value {
        #[derive(IntoValue)]
        #[value(shape = "graphix_compiler::rep::GraphixShape")]
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
        #[value(shape = "graphix_compiler::rep::GraphixShape")]
        struct Fields {
            r: f32,
            g: f32,
            b: f32,
            a: f32,
        }
        let Fields { r, g, b, a } = abstract_payload(&v)
            .ok_or_else(|| {
                anyhow!("expected a Color, got {}", graphix_compiler::typ::Naked(&v))
            })?
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
        match rep::bare_tag(&v)? {
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
        match rep::bare_tag(&v)? {
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
        #[value(shape = "graphix_compiler::rep::GraphixShape")]
        struct Fields {
            family: Value,
            weight: Value,
            style: Value,
        }
        let Fields { family, weight, style } = v.cast_to()?;
        let family = match rep::variant(&family) {
            Some((s, [])) => match s.as_str() {
                "SansSerif" => Family::SansSerif,
                "Serif" => Family::Serif,
                "Monospace" => Family::Monospace,
                s => bail!("invalid font family {s}"),
            },
            Some((s, [name])) => match s.as_str() {
                "Name" => {
                    let name = name.clone().cast_to::<ArcStr>()?;
                    let mut cache = FONT_NAMES.lock();
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
                s => bail!("invalid font family {s}"),
            },
            _ => bail!("invalid font family {family}"),
        };
        let weight = match rep::bare_tag(&weight)? {
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
        let style = match rep::bare_tag(&style)? {
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
        #[value(shape = "graphix_compiler::rep::GraphixShape")]
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
        match rep::variant(&v) {
            Some((s, [])) => {
                let inner = match s.as_str() {
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
            Some((s, [v])) => match s.as_str() {
                "CustomPalette" => {
                    let palette = PaletteV::from_value(v.clone())?;
                    Ok(Self(GraphixTheme {
                        inner: Theme::custom("Custom", palette.0),
                        overrides: None,
                    }))
                }
                "Custom" => {
                    #[derive(FromValue)]
                    #[value(shape = "graphix_compiler::rep::GraphixShape")]
                    struct Fields {
                        palette: PaletteV,
                    }
                    let Fields { palette } = v.clone().cast_to()?;
                    let overrides = v.clone().cast_to::<StyleOverrides>()?;
                    Ok(Self(GraphixTheme {
                        inner: Theme::custom("Custom", palette.0),
                        overrides: Some(Arc::new(overrides)),
                    }))
                }
                s => bail!("invalid theme {s}"),
            },
            _ => bail!("invalid theme {}", graphix_compiler::typ::Naked(&v)),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct ScrollDirectionV(pub scrollable::Direction);

impl FromValue for ScrollDirectionV {
    fn from_value(v: Value) -> Result<Self> {
        match rep::bare_tag(&v)? {
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
        match rep::bare_tag(&v)? {
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
        match rep::bare_tag(&v)? {
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

    /// Whether `other` is the same image: the same path or text, or
    /// bytes at the same place, else equal bytes.
    pub fn same_as(&self, other: &Self) -> bool {
        let bytes = |a: &iced_core::Bytes, b: &iced_core::Bytes| {
            (a.as_ptr() == b.as_ptr() && a.len() == b.len()) || a == b
        };
        match (self, other) {
            (Self::Path(a), Self::Path(b)) | (Self::Svg(a), Self::Svg(b)) => a == b,
            (Self::Bytes(a), Self::Bytes(b)) => bytes(a, b),
            (
                Self::Rgba { width: w0, height: h0, pixels: a },
                Self::Rgba { width: w1, height: h1, pixels: b },
            ) => w0 == w1 && h0 == h1 && bytes(a, b),
            _ => false,
        }
    }

    pub fn decode_icon(&self) -> Result<Option<winit::window::Icon>> {
        match self {
            Self::Path(p) if p.is_empty() => Ok(None),
            Self::Path(p) if self.is_svg() => {
                let data = std::fs::read(p)?;
                decode_svg_icon(&data)
            }
            Self::Path(p) => raster_icon(::image::open(p)?.into_rgba8()),
            Self::Bytes(b) if b.is_empty() => Ok(None),
            Self::Bytes(b) => raster_icon(::image::load_from_memory(b)?.into_rgba8()),
            Self::Svg(s) if s.is_empty() => Ok(None),
            Self::Svg(s) => decode_svg_icon(s.as_bytes()),
            Self::Rgba { width, height, pixels } => {
                if pixels.is_empty() {
                    return Ok(None);
                }
                let img = ::image::RgbaImage::from_raw(*width, *height, pixels.to_vec())
                    .context("an RGBA icon's pixels do not fill its size")?;
                raster_icon(img)
            }
        }
    }
}

/// An RGBA image's pixels are four bytes for every one of its width x
/// height pixels.
pub(crate) fn check_rgba(width: u32, height: u32, len: usize) -> Result<()> {
    match (width as u64).checked_mul(height as u64).and_then(|n| n.checked_mul(4)) {
        Some(want) if want == len as u64 => Ok(()),
        Some(want) => {
            bail!("an RGBA image of {width} x {height} needs {want} bytes, not {len}")
        }
        None => bail!("an RGBA image of {width} x {height} is too large"),
    }
}

/// The largest icon edge handed to the window system: X11 sends an icon
/// in one request, which a much larger one overflows.
const MAX_ICON_EDGE: u32 = 256;

/// `img` shrunk, keeping its shape, to fit `MAX_ICON_EDGE`.
fn fit_icon(img: ::image::RgbaImage) -> ::image::RgbaImage {
    let (w, h) = img.dimensions();
    let edge = w.max(h);
    if edge <= MAX_ICON_EDGE {
        return img;
    }
    let fit = |n: u32| ((n as u64 * MAX_ICON_EDGE as u64 / edge as u64) as u32).max(1);
    ::image::imageops::thumbnail(&img, fit(w), fit(h))
}

fn raster_icon(img: ::image::RgbaImage) -> Result<Option<winit::window::Icon>> {
    let img = fit_icon(img);
    let (w, h) = img.dimensions();
    Ok(Some(winit::window::Icon::from_rgba(img.into_raw(), w, h)?))
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
                let Some((tag, [val])) = rep::variant(&v) else {
                    bail!("invalid ImageSource {}", graphix_compiler::typ::Naked(&v))
                };
                match tag.as_str() {
                    "Bytes" => match val {
                        Value::Bytes(b) => Ok(Self::Bytes((**b).clone())),
                        _ => bail!("ImageSource Bytes: expected bytes value"),
                    },
                    "Svg" => Ok(Self::Svg(val.clone().cast_to::<String>()?)),
                    "Rgba" => {
                        #[derive(FromValue)]
                        #[value(shape = "graphix_compiler::rep::GraphixShape")]
                        struct Fields {
                            width: u32,
                            height: u32,
                            pixels: iced_core::Bytes,
                        }
                        let Fields { width, height, pixels } = val.clone().cast_to()?;
                        check_rgba(width, height, pixels.len())?;
                        Ok(Self::Rgba { width, height, pixels })
                    }
                    s => bail!("invalid ImageSource variant: {s}"),
                }
            }
        }
    }
}

#[derive(Clone, Copy, Debug, FromValue)]
#[value(shape = "graphix_compiler::rep::GraphixShape")]
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
        #[value(shape = "graphix_compiler::rep::GraphixShape")]
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
        #[value(shape = "graphix_compiler::rep::GraphixShape")]
        struct Fields {
            alt: bool,
            ctrl: bool,
            key: ArcStr,
            logo: bool,
            shift: bool,
        }
        let Fields { alt, ctrl, key, logo, shift } = abstract_payload(&v)
            .ok_or_else(|| {
                anyhow!("expected a Shortcut, got {}", graphix_compiler::typ::Naked(&v))
            })?
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rgba_sizes_past_u64_are_refused() {
        assert!(check_rgba(u32::MAX, u32::MAX, 0).is_err());
        assert!(check_rgba(1 << 31, 1 << 31, 0).is_err());
        assert!(check_rgba(2, 3, 24).is_ok());
    }

    #[test]
    fn window_sizes_fit_the_window_system() {
        let size = |w, h| Value::from(SizeV(Size::new(w, h))).cast_to::<WindowSizeV>();
        for (w, h) in [(70000., 10.), (10., 0.), (-5., 10.), (10., 8193.)] {
            assert!(size(w, h).is_err(), "{w} x {h}");
        }
        assert_eq!(size(800., 8192.).unwrap(), WindowSizeV(Size::new(800., 8192.)));
    }

    #[test]
    fn icons_shrink_to_fit() {
        let dims = |w, h| fit_icon(::image::RgbaImage::new(w, h)).dimensions();
        assert_eq!(dims(1024, 512), (256, 128));
        assert_eq!(dims(4000, 1), (256, 1));
        assert_eq!(dims(100, 60), (100, 60));
    }

    #[test]
    fn text_sizes_fit_the_rasterizer() {
        for size in [100_000.0, 4096.5, 0.0, -1.0, f64::INFINITY, f64::NAN] {
            assert!(TextSizeV::from_value(Value::F64(size)).is_err(), "{size}");
        }
        for size in [0.5, 16.0, 4096.0] {
            assert_eq!(TextSizeV::from_value(Value::F64(size)).unwrap().0, size as f32);
        }
    }
}
