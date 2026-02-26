use anyhow::{bail, Result};
use arcstr::ArcStr;
use iced_core::{
    alignment::{Horizontal, Vertical},
    font::{Family, Style, Weight},
    Color, ContentFit, Font, Length, Padding, Size,
};
use iced_widget::{scrollable, tooltip};
use netidx::publisher::{FromValue, Value};
use smallvec::SmallVec;
use std::{
    collections::HashSet,
    sync::{LazyLock, Mutex},
};

static FONT_NAMES: LazyLock<Mutex<HashSet<&'static str>>> =
    LazyLock::new(Default::default);

#[derive(Clone, Copy, Debug)]
pub(crate) struct LengthV(pub Length);

impl FromValue for LengthV {
    fn from_value(v: Value) -> Result<Self> {
        match v {
            Value::String(s) => match &*s {
                "Fill" => Ok(Self(Length::Fill)),
                "Shrink" => Ok(Self(Length::Shrink)),
                s => bail!("invalid length {s}"),
            },
            v => match v.cast_to::<(ArcStr, Value)>()? {
                (s, v) if &*s == "FillPortion" => {
                    let n = v.cast_to::<u16>()?;
                    Ok(Self(Length::FillPortion(n)))
                }
                (s, v) if &*s == "Fixed" => {
                    let n = v.cast_to::<f64>()? as f32;
                    Ok(Self(Length::Fixed(n)))
                }
                (s, _) => bail!("invalid length {s}"),
            },
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub(crate) struct PaddingV(pub Padding);

impl FromValue for PaddingV {
    fn from_value(v: Value) -> Result<Self> {
        match v.cast_to::<(ArcStr, Value)>()? {
            (s, v) if &*s == "All" => {
                let n = v.cast_to::<f64>()? as f32;
                Ok(Self(Padding::new(n)))
            }
            (s, v) if &*s == "Axis" => {
                let (y, x) = v.cast_to::<(f64, f64)>()?;
                Ok(Self(Padding::from([y as f32, x as f32])))
            }
            (s, v) if &*s == "Each" => {
                let (top, right, bottom, left) = v.cast_to::<(f64, f64, f64, f64)>()?;
                Ok(Self(Padding {
                    top: top as f32,
                    right: right as f32,
                    bottom: bottom as f32,
                    left: left as f32,
                }))
            }
            (s, _) => bail!("invalid padding {s}"),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub(crate) struct SizeV(pub Size);

impl FromValue for SizeV {
    fn from_value(v: Value) -> Result<Self> {
        let [(_, height), (_, width)] = v.cast_to::<[(ArcStr, f64); 2]>()?;
        Ok(Self(Size::new(width as f32, height as f32)))
    }
}

impl From<SizeV> for Value {
    fn from(v: SizeV) -> Value {
        use arcstr::literal;
        [(literal!("height"), v.0.height as f64), (literal!("width"), v.0.width as f64)]
            .into()
    }
}

#[derive(Clone, Copy, Debug)]
pub(crate) struct ColorV(pub Color);

impl FromValue for ColorV {
    fn from_value(v: Value) -> Result<Self> {
        let [(_, a), (_, b), (_, g), (_, r)] = v.cast_to::<[(ArcStr, f64); 4]>()?;
        Ok(Self(Color::from_rgba(r as f32, g as f32, b as f32, a as f32)))
    }
}

#[derive(Clone, Copy, Debug)]
pub(crate) struct HAlignV(pub Horizontal);

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
pub(crate) struct VAlignV(pub Vertical);

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
pub(crate) struct FontV(pub Font);

impl FromValue for FontV {
    fn from_value(v: Value) -> Result<Self> {
        let [(_, family), (_, style), (_, weight)] =
            v.cast_to::<[(ArcStr, Value); 3]>()?;
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
        let weight = match &*weight.cast_to::<ArcStr>()? {
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
        let style = match &*style.cast_to::<ArcStr>()? {
            "Normal" => Style::Normal,
            "Italic" => Style::Italic,
            "Oblique" => Style::Oblique,
            s => bail!("invalid font style {s}"),
        };
        Ok(Self(Font { family, weight, style, ..Font::DEFAULT }))
    }
}

#[derive(Clone, Debug)]
pub(crate) struct ThemeV(pub iced_core::Theme);

impl FromValue for ThemeV {
    fn from_value(v: Value) -> Result<Self> {
        use iced_core::Theme;
        match &*v.cast_to::<ArcStr>()? {
            "Light" => Ok(Self(Theme::Light)),
            "Dark" => Ok(Self(Theme::Dark)),
            "Dracula" => Ok(Self(Theme::Dracula)),
            "Nord" => Ok(Self(Theme::Nord)),
            "SolarizedLight" => Ok(Self(Theme::SolarizedLight)),
            "SolarizedDark" => Ok(Self(Theme::SolarizedDark)),
            "GruvboxLight" => Ok(Self(Theme::GruvboxLight)),
            "GruvboxDark" => Ok(Self(Theme::GruvboxDark)),
            "CatppuccinLatte" => Ok(Self(Theme::CatppuccinLatte)),
            "CatppuccinFrappe" => Ok(Self(Theme::CatppuccinFrappe)),
            "CatppuccinMacchiato" => Ok(Self(Theme::CatppuccinMacchiato)),
            "CatppuccinMocha" => Ok(Self(Theme::CatppuccinMocha)),
            "TokyoNight" => Ok(Self(Theme::TokyoNight)),
            "TokyoNightStorm" => Ok(Self(Theme::TokyoNightStorm)),
            "TokyoNightLight" => Ok(Self(Theme::TokyoNightLight)),
            "KanagawaWave" => Ok(Self(Theme::KanagawaWave)),
            "KanagawaDragon" => Ok(Self(Theme::KanagawaDragon)),
            "KanagawaLotus" => Ok(Self(Theme::KanagawaLotus)),
            "Moonfly" => Ok(Self(Theme::Moonfly)),
            "Nightfly" => Ok(Self(Theme::Nightfly)),
            "Oxocarbon" => Ok(Self(Theme::Oxocarbon)),
            "Ferra" => Ok(Self(Theme::Ferra)),
            s => bail!("invalid theme {s}"),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub(crate) struct ScrollDirectionV(pub scrollable::Direction);

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
pub(crate) struct TooltipPositionV(pub tooltip::Position);

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
pub(crate) struct ContentFitV(pub ContentFit);

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

/// Image source: file path, raw encoded bytes, or decoded RGBA pixels.
#[derive(Clone, Debug)]
pub(crate) enum ImageSourceV {
    Path(String),
    Bytes(iced_core::Bytes),
    Rgba { width: u32, height: u32, pixels: iced_core::Bytes },
}

impl ImageSourceV {
    pub(crate) fn to_handle(&self) -> iced_core::image::Handle {
        match self {
            Self::Path(p) => iced_core::image::Handle::from_path(p),
            Self::Bytes(b) => iced_core::image::Handle::from_bytes(b.clone()),
            Self::Rgba { width, height, pixels } => {
                iced_core::image::Handle::from_rgba(*width, *height, pixels.clone())
            }
        }
    }
}

impl FromValue for ImageSourceV {
    fn from_value(v: Value) -> Result<Self> {
        match v {
            // Bare string → file path (backward compat)
            Value::String(s) => Ok(Self::Path(s.to_string())),
            // Bare bytes → encoded image data
            Value::Bytes(b) => Ok(Self::Bytes(iced_core::Bytes::copy_from_slice(&b))),
            // Variant tag
            v => {
                let (tag, val) = v.cast_to::<(ArcStr, Value)>()?;
                match &*tag {
                    "Path" => Ok(Self::Path(val.cast_to::<String>()?)),
                    "Bytes" => match val {
                        Value::Bytes(b) => {
                            Ok(Self::Bytes(iced_core::Bytes::copy_from_slice(&b)))
                        }
                        _ => bail!("ImageSource Bytes: expected bytes value"),
                    },
                    "Rgba" => {
                        let [(_, height), (_, pixels), (_, width)] =
                            val.cast_to::<[(ArcStr, Value); 3]>()?;
                        let width = width.cast_to::<u32>()?;
                        let height = height.cast_to::<u32>()?;
                        let pixels = match pixels {
                            Value::Bytes(b) => iced_core::Bytes::copy_from_slice(&b),
                            _ => bail!("ImageSource Rgba: expected bytes for pixels"),
                        };
                        Ok(Self::Rgba { width, height, pixels })
                    }
                    s => bail!("invalid ImageSource variant: {s}"),
                }
            }
        }
    }
}

/// Newtype for `Vec<String>` to satisfy orphan rules.
#[derive(Clone, Debug)]
pub(crate) struct StringVec(pub Vec<String>);

impl FromValue for StringVec {
    fn from_value(v: Value) -> Result<Self> {
        let items = v.cast_to::<SmallVec<[Value; 8]>>()?;
        let v: Vec<String> =
            items.into_iter().map(|v| v.cast_to::<String>()).collect::<Result<_>>()?;
        Ok(Self(v))
    }
}
