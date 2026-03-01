use crate::theme::{
    ButtonSpec, CheckboxSpec, ContainerSpec, GraphixTheme, MenuSpec, PickListSpec,
    ProgressBarSpec, RadioSpec, RuleSpec, ScrollableSpec, SliderSpec, StyleOverrides,
    TextEditorSpec, TextInputSpec, TogglerSpec,
};
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
use triomphe::Arc;

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

#[derive(Clone, Copy, Debug)]
pub(crate) struct PaletteV(pub iced_core::theme::palette::Palette);

impl FromValue for PaletteV {
    fn from_value(v: Value) -> Result<Self> {
        let [(_, bg), (_, danger), (_, primary), (_, success), (_, text), (_, warning)] =
            v.cast_to::<[(ArcStr, Value); 6]>()?;
        let bg = ColorV::from_value(bg)?;
        let text = ColorV::from_value(text)?;
        let primary = ColorV::from_value(primary)?;
        let success = ColorV::from_value(success)?;
        let warning = ColorV::from_value(warning)?;
        let danger = ColorV::from_value(danger)?;
        Ok(Self(iced_core::theme::palette::Palette {
            background: bg.0,
            text: text.0,
            primary: primary.0,
            success: success.0,
            warning: warning.0,
            danger: danger.0,
        }))
    }
}

#[derive(Clone, Debug)]
pub(crate) struct ThemeV(pub GraphixTheme);

pub(crate) fn parse_opt_color(v: Value) -> Result<Option<Color>> {
    if v == Value::Null {
        Ok(None)
    } else {
        Ok(Some(ColorV::from_value(v)?.0))
    }
}

fn parse_opt_f32(v: Value) -> Result<Option<f32>> {
    if v == Value::Null {
        Ok(None)
    } else {
        Ok(Some(v.cast_to::<f64>()? as f32))
    }
}

fn parse_opt_spec<T>(v: Value, f: impl FnOnce(Value) -> Result<T>) -> Result<Option<T>> {
    if v == Value::Null {
        Ok(None)
    } else {
        Ok(Some(f(v)?))
    }
}

fn parse_button_spec(v: Value) -> Result<ButtonSpec> {
    let [(_, bg), (_, bc), (_, br), (_, bw), (_, tc)] =
        v.cast_to::<[(ArcStr, Value); 5]>()?;
    Ok(ButtonSpec {
        background: parse_opt_color(bg)?,
        border_color: parse_opt_color(bc)?,
        border_radius: parse_opt_f32(br)?,
        border_width: parse_opt_f32(bw)?,
        text_color: parse_opt_color(tc)?,
    })
}

fn parse_checkbox_spec(v: Value) -> Result<CheckboxSpec> {
    let [(_, accent), (_, bg), (_, bc), (_, br), (_, bw), (_, ic), (_, tc)] =
        v.cast_to::<[(ArcStr, Value); 7]>()?;
    Ok(CheckboxSpec {
        accent: parse_opt_color(accent)?,
        background: parse_opt_color(bg)?,
        border_color: parse_opt_color(bc)?,
        border_radius: parse_opt_f32(br)?,
        border_width: parse_opt_f32(bw)?,
        icon_color: parse_opt_color(ic)?,
        text_color: parse_opt_color(tc)?,
    })
}

fn parse_container_spec(v: Value) -> Result<ContainerSpec> {
    let [(_, bg), (_, bc), (_, br), (_, bw), (_, tc)] =
        v.cast_to::<[(ArcStr, Value); 5]>()?;
    Ok(ContainerSpec {
        background: parse_opt_color(bg)?,
        border_color: parse_opt_color(bc)?,
        border_radius: parse_opt_f32(br)?,
        border_width: parse_opt_f32(bw)?,
        text_color: parse_opt_color(tc)?,
    })
}

fn parse_menu_spec(v: Value) -> Result<MenuSpec> {
    let [(_, bg), (_, bc), (_, br), (_, bw), (_, sb), (_, stc), (_, tc)] =
        v.cast_to::<[(ArcStr, Value); 7]>()?;
    Ok(MenuSpec {
        background: parse_opt_color(bg)?,
        border_color: parse_opt_color(bc)?,
        border_radius: parse_opt_f32(br)?,
        border_width: parse_opt_f32(bw)?,
        selected_background: parse_opt_color(sb)?,
        selected_text_color: parse_opt_color(stc)?,
        text_color: parse_opt_color(tc)?,
    })
}

fn parse_pick_list_spec(v: Value) -> Result<PickListSpec> {
    let [(_, bg), (_, bc), (_, br), (_, bw), (_, hc), (_, pc), (_, tc)] =
        v.cast_to::<[(ArcStr, Value); 7]>()?;
    Ok(PickListSpec {
        background: parse_opt_color(bg)?,
        border_color: parse_opt_color(bc)?,
        border_radius: parse_opt_f32(br)?,
        border_width: parse_opt_f32(bw)?,
        handle_color: parse_opt_color(hc)?,
        placeholder_color: parse_opt_color(pc)?,
        text_color: parse_opt_color(tc)?,
    })
}

fn parse_progress_bar_spec(v: Value) -> Result<ProgressBarSpec> {
    let [(_, bg), (_, bar), (_, br)] = v.cast_to::<[(ArcStr, Value); 3]>()?;
    Ok(ProgressBarSpec {
        background: parse_opt_color(bg)?,
        bar_color: parse_opt_color(bar)?,
        border_radius: parse_opt_f32(br)?,
    })
}

fn parse_radio_spec(v: Value) -> Result<RadioSpec> {
    let [(_, bg), (_, bc), (_, bw), (_, dc), (_, tc)] =
        v.cast_to::<[(ArcStr, Value); 5]>()?;
    Ok(RadioSpec {
        background: parse_opt_color(bg)?,
        border_color: parse_opt_color(bc)?,
        border_width: parse_opt_f32(bw)?,
        dot_color: parse_opt_color(dc)?,
        text_color: parse_opt_color(tc)?,
    })
}

fn parse_rule_spec(v: Value) -> Result<RuleSpec> {
    let [(_, color), (_, radius), (_, width)] = v.cast_to::<[(ArcStr, Value); 3]>()?;
    Ok(RuleSpec {
        color: parse_opt_color(color)?,
        radius: parse_opt_f32(radius)?,
        width: parse_opt_f32(width)?,
    })
}

fn parse_scrollable_spec(v: Value) -> Result<ScrollableSpec> {
    let [(_, bg), (_, bc), (_, br), (_, bw), (_, sc)] =
        v.cast_to::<[(ArcStr, Value); 5]>()?;
    Ok(ScrollableSpec {
        background: parse_opt_color(bg)?,
        border_color: parse_opt_color(bc)?,
        border_radius: parse_opt_f32(br)?,
        border_width: parse_opt_f32(bw)?,
        scroller_color: parse_opt_color(sc)?,
    })
}

fn parse_slider_spec(v: Value) -> Result<SliderSpec> {
    let [(_, hbc), (_, hbw), (_, hc), (_, hr), (_, rc), (_, rfc), (_, rw)] =
        v.cast_to::<[(ArcStr, Value); 7]>()?;
    Ok(SliderSpec {
        handle_border_color: parse_opt_color(hbc)?,
        handle_border_width: parse_opt_f32(hbw)?,
        handle_color: parse_opt_color(hc)?,
        handle_radius: parse_opt_f32(hr)?,
        rail_color: parse_opt_color(rc)?,
        rail_fill_color: parse_opt_color(rfc)?,
        rail_width: parse_opt_f32(rw)?,
    })
}

fn parse_text_editor_spec(v: Value) -> Result<TextEditorSpec> {
    let [(_, bg), (_, bc), (_, br), (_, bw), (_, pc), (_, sc), (_, vc)] =
        v.cast_to::<[(ArcStr, Value); 7]>()?;
    Ok(TextEditorSpec {
        background: parse_opt_color(bg)?,
        border_color: parse_opt_color(bc)?,
        border_radius: parse_opt_f32(br)?,
        border_width: parse_opt_f32(bw)?,
        placeholder_color: parse_opt_color(pc)?,
        selection_color: parse_opt_color(sc)?,
        value_color: parse_opt_color(vc)?,
    })
}

fn parse_text_input_spec(v: Value) -> Result<TextInputSpec> {
    let [(_, bg), (_, bc), (_, br), (_, bw), (_, ic), (_, pc), (_, sc), (_, vc)] =
        v.cast_to::<[(ArcStr, Value); 8]>()?;
    Ok(TextInputSpec {
        background: parse_opt_color(bg)?,
        border_color: parse_opt_color(bc)?,
        border_radius: parse_opt_f32(br)?,
        border_width: parse_opt_f32(bw)?,
        icon_color: parse_opt_color(ic)?,
        placeholder_color: parse_opt_color(pc)?,
        selection_color: parse_opt_color(sc)?,
        value_color: parse_opt_color(vc)?,
    })
}

fn parse_toggler_spec(v: Value) -> Result<TogglerSpec> {
    let [(_, bg), (_, bbc), (_, br), (_, fg), (_, fbc), (_, tc)] =
        v.cast_to::<[(ArcStr, Value); 6]>()?;
    Ok(TogglerSpec {
        background: parse_opt_color(bg)?,
        background_border_color: parse_opt_color(bbc)?,
        border_radius: parse_opt_f32(br)?,
        foreground: parse_opt_color(fg)?,
        foreground_border_color: parse_opt_color(fbc)?,
        text_color: parse_opt_color(tc)?,
    })
}

fn parse_stylesheet(
    v: Value,
) -> Result<(iced_core::theme::palette::Palette, StyleOverrides)> {
    let [(_, button), (_, checkbox), (_, container), (_, menu), (_, palette), (_, pick_list), (_, progress_bar), (_, radio), (_, rule), (_, scrollable), (_, slider), (_, text_editor), (_, text_input), (_, toggler)] =
        v.cast_to::<[(ArcStr, Value); 14]>()?;
    let palette = PaletteV::from_value(palette)?;
    Ok((
        palette.0,
        StyleOverrides {
            button: parse_opt_spec(button, parse_button_spec)?,
            checkbox: parse_opt_spec(checkbox, parse_checkbox_spec)?,
            container: parse_opt_spec(container, parse_container_spec)?,
            menu: parse_opt_spec(menu, parse_menu_spec)?,
            pick_list: parse_opt_spec(pick_list, parse_pick_list_spec)?,
            progress_bar: parse_opt_spec(progress_bar, parse_progress_bar_spec)?,
            radio: parse_opt_spec(radio, parse_radio_spec)?,
            rule: parse_opt_spec(rule, parse_rule_spec)?,
            scrollable: parse_opt_spec(scrollable, parse_scrollable_spec)?,
            slider: parse_opt_spec(slider, parse_slider_spec)?,
            text_editor: parse_opt_spec(text_editor, parse_text_editor_spec)?,
            text_input: parse_opt_spec(text_input, parse_text_input_spec)?,
            toggler: parse_opt_spec(toggler, parse_toggler_spec)?,
        },
    ))
}

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
                    let (palette, overrides) = parse_stylesheet(v)?;
                    Ok(Self(GraphixTheme {
                        inner: Theme::custom("Custom", palette),
                        overrides: Some(Arc::new(overrides)),
                    }))
                }
                (s, _) => bail!("invalid theme {s}"),
            },
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
            Value::Bytes(b) => Ok(Self::Bytes((*b).clone())),
            // Variant tag
            v => {
                let (tag, val) = v.cast_to::<(ArcStr, Value)>()?;
                match &*tag {
                    "Bytes" => match val {
                        Value::Bytes(b) => Ok(Self::Bytes((*b).clone())),
                        _ => bail!("ImageSource Bytes: expected bytes value"),
                    },
                    "Rgba" => {
                        let [(_, height), (_, pixels), (_, width)] =
                            val.cast_to::<[(ArcStr, Value); 3]>()?;
                        let width = width.cast_to::<u32>()?;
                        let height = height.cast_to::<u32>()?;
                        let pixels = match pixels {
                            Value::Bytes(b) => (*b).clone(),
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
