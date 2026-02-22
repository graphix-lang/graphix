use anyhow::{bail, Result};
use arcstr::ArcStr;
use iced_core::{
    alignment::{Horizontal, Vertical},
    font::{Family, Style, Weight},
    Color, Font, Length, Padding, Size,
};
use netidx::publisher::{FromValue, Value};

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
                let (top, right, bottom, left) =
                    v.cast_to::<(f64, f64, f64, f64)>()?;
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

impl Into<Value> for SizeV {
    fn into(self) -> Value {
        use arcstr::literal;
        [(literal!("height"), self.0.height as f64), (literal!("width"), self.0.width as f64)]
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
        let family = match &*family.cast_to::<ArcStr>()? {
            "SansSerif" => Family::SansSerif,
            "Serif" => Family::Serif,
            "Monospace" => Family::Monospace,
            s => bail!("invalid font family {s}"),
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
