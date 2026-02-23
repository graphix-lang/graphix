//! Convert winit events to iced events.
//!
//! Based on iced_winit/src/conversion.rs but adapted for our event loop.

use iced_core::{keyboard, mouse, window, Event, Point, Size};
use smallvec::SmallVec;
use winit::event::{ElementState, MouseButton, WindowEvent};
use winit::keyboard::{Key, NamedKey, NativeKeyCode};
use winit::platform::modifier_supplement::KeyEventExtModifierSupplement;
use keyboard::key::NativeCode;

/// Convert a winit WindowEvent to zero or more iced Events.
pub(crate) fn window_event(
    event: &WindowEvent,
    scale_factor: f64,
    modifiers: winit::keyboard::ModifiersState,
) -> SmallVec<[Event; 1]> {
    let mut events = SmallVec::new();
    match event {
        WindowEvent::Resized(size) => {
            let logical = size.to_logical::<f32>(scale_factor);
            events.push(Event::Window(window::Event::Resized(Size::new(
                logical.width,
                logical.height,
            ))));
        }
        WindowEvent::CursorMoved { position, .. } => {
            let logical = position.to_logical::<f32>(scale_factor);
            events.push(Event::Mouse(mouse::Event::CursorMoved {
                position: Point::new(logical.x, logical.y),
            }));
        }
        WindowEvent::CursorEntered { .. } => {
            events.push(Event::Mouse(mouse::Event::CursorEntered));
        }
        WindowEvent::CursorLeft { .. } => {
            events.push(Event::Mouse(mouse::Event::CursorLeft));
        }
        WindowEvent::MouseInput { state, button, .. } => {
            if let Some(btn) = mouse_button(*button) {
                let event = match state {
                    ElementState::Pressed => mouse::Event::ButtonPressed(btn),
                    ElementState::Released => mouse::Event::ButtonReleased(btn),
                };
                events.push(Event::Mouse(event));
            }
        }
        WindowEvent::MouseWheel { delta, .. } => {
            let delta = match delta {
                winit::event::MouseScrollDelta::LineDelta(x, y) => {
                    mouse::ScrollDelta::Lines { x: *x, y: *y }
                }
                winit::event::MouseScrollDelta::PixelDelta(pos) => {
                    let logical = pos.to_logical::<f32>(scale_factor);
                    mouse::ScrollDelta::Pixels { x: logical.x, y: logical.y }
                }
            };
            events.push(Event::Mouse(mouse::Event::WheelScrolled { delta }));
        }
        WindowEvent::KeyboardInput { is_synthetic: true, .. } => {}
        WindowEvent::KeyboardInput { event, .. } => {
            let key = convert_key(event.key_without_modifiers());
            let modified_key = convert_key(event.logical_key.clone());
            let mods = convert_modifiers(modifiers);
            let text = event
                .text_with_all_modifiers()
                .map(|s| s.to_string().into())
                .filter(|s: &iced_core::SmolStr| {
                    !s.as_str().chars().any(|c| {
                        matches!(c, '\u{E000}'..='\u{F8FF}'
                            | '\u{F0000}'..='\u{FFFFD}'
                            | '\u{100000}'..='\u{10FFFD}')
                    })
                });
            let ev = match event.state {
                ElementState::Pressed => keyboard::Event::KeyPressed {
                    key,
                    modified_key,
                    modifiers: mods,
                    location: convert_key_location(event.location),
                    text,
                    physical_key: physical_key(event.physical_key),
                    repeat: event.repeat,
                },
                ElementState::Released => keyboard::Event::KeyReleased {
                    key,
                    modified_key,
                    modifiers: mods,
                    location: convert_key_location(event.location),
                    physical_key: physical_key(event.physical_key),
                },
            };
            events.push(Event::Keyboard(ev));
        }
        WindowEvent::Focused(focused) => {
            events.push(Event::Window(if *focused {
                window::Event::Focused
            } else {
                window::Event::Unfocused
            }));
        }
        WindowEvent::CloseRequested => {
            events.push(Event::Window(window::Event::CloseRequested));
        }
        _ => {}
    }
    events
}

fn mouse_button(button: MouseButton) -> Option<mouse::Button> {
    match button {
        MouseButton::Left => Some(mouse::Button::Left),
        MouseButton::Right => Some(mouse::Button::Right),
        MouseButton::Middle => Some(mouse::Button::Middle),
        MouseButton::Other(n) => Some(mouse::Button::Other(n)),
        _ => None,
    }
}

fn convert_key(key: Key) -> keyboard::Key {
    match key {
        Key::Character(c) => keyboard::Key::Character(c.as_str().to_string().into()),
        Key::Named(named) => match convert_named_key(named) {
            Some(n) => keyboard::Key::Named(n),
            None => keyboard::Key::Unidentified,
        },
        Key::Unidentified(_) | Key::Dead(_) => keyboard::Key::Unidentified,
    }
}

fn convert_named_key(key: NamedKey) -> Option<keyboard::key::Named> {
    use keyboard::key::Named;
    Some(match key {
        NamedKey::Alt => Named::Alt,
        NamedKey::AltGraph => Named::AltGraph,
        NamedKey::CapsLock => Named::CapsLock,
        NamedKey::Control => Named::Control,
        NamedKey::Fn => Named::Fn,
        NamedKey::FnLock => Named::FnLock,
        NamedKey::NumLock => Named::NumLock,
        NamedKey::ScrollLock => Named::ScrollLock,
        NamedKey::Shift => Named::Shift,
        NamedKey::Symbol => Named::Symbol,
        NamedKey::SymbolLock => Named::SymbolLock,
        NamedKey::Meta => Named::Meta,
        NamedKey::Hyper => Named::Hyper,
        NamedKey::Super => Named::Super,
        NamedKey::Enter => Named::Enter,
        NamedKey::Tab => Named::Tab,
        NamedKey::Space => Named::Space,
        NamedKey::ArrowDown => Named::ArrowDown,
        NamedKey::ArrowLeft => Named::ArrowLeft,
        NamedKey::ArrowRight => Named::ArrowRight,
        NamedKey::ArrowUp => Named::ArrowUp,
        NamedKey::End => Named::End,
        NamedKey::Home => Named::Home,
        NamedKey::PageDown => Named::PageDown,
        NamedKey::PageUp => Named::PageUp,
        NamedKey::Backspace => Named::Backspace,
        NamedKey::Clear => Named::Clear,
        NamedKey::Copy => Named::Copy,
        NamedKey::CrSel => Named::CrSel,
        NamedKey::Cut => Named::Cut,
        NamedKey::Delete => Named::Delete,
        NamedKey::EraseEof => Named::EraseEof,
        NamedKey::ExSel => Named::ExSel,
        NamedKey::Insert => Named::Insert,
        NamedKey::Paste => Named::Paste,
        NamedKey::Redo => Named::Redo,
        NamedKey::Undo => Named::Undo,
        NamedKey::Accept => Named::Accept,
        NamedKey::Again => Named::Again,
        NamedKey::Attn => Named::Attn,
        NamedKey::Cancel => Named::Cancel,
        NamedKey::ContextMenu => Named::ContextMenu,
        NamedKey::Escape => Named::Escape,
        NamedKey::Execute => Named::Execute,
        NamedKey::Find => Named::Find,
        NamedKey::Help => Named::Help,
        NamedKey::Pause => Named::Pause,
        NamedKey::Play => Named::Play,
        NamedKey::Props => Named::Props,
        NamedKey::Select => Named::Select,
        NamedKey::ZoomIn => Named::ZoomIn,
        NamedKey::ZoomOut => Named::ZoomOut,
        NamedKey::BrightnessDown => Named::BrightnessDown,
        NamedKey::BrightnessUp => Named::BrightnessUp,
        NamedKey::Eject => Named::Eject,
        NamedKey::LogOff => Named::LogOff,
        NamedKey::Power => Named::Power,
        NamedKey::PowerOff => Named::PowerOff,
        NamedKey::PrintScreen => Named::PrintScreen,
        NamedKey::Hibernate => Named::Hibernate,
        NamedKey::Standby => Named::Standby,
        NamedKey::WakeUp => Named::WakeUp,
        NamedKey::AllCandidates => Named::AllCandidates,
        NamedKey::Alphanumeric => Named::Alphanumeric,
        NamedKey::CodeInput => Named::CodeInput,
        NamedKey::Compose => Named::Compose,
        NamedKey::Convert => Named::Convert,
        NamedKey::FinalMode => Named::FinalMode,
        NamedKey::GroupFirst => Named::GroupFirst,
        NamedKey::GroupLast => Named::GroupLast,
        NamedKey::GroupNext => Named::GroupNext,
        NamedKey::GroupPrevious => Named::GroupPrevious,
        NamedKey::ModeChange => Named::ModeChange,
        NamedKey::NextCandidate => Named::NextCandidate,
        NamedKey::NonConvert => Named::NonConvert,
        NamedKey::PreviousCandidate => Named::PreviousCandidate,
        NamedKey::Process => Named::Process,
        NamedKey::SingleCandidate => Named::SingleCandidate,
        NamedKey::HangulMode => Named::HangulMode,
        NamedKey::HanjaMode => Named::HanjaMode,
        NamedKey::JunjaMode => Named::JunjaMode,
        NamedKey::Eisu => Named::Eisu,
        NamedKey::Hankaku => Named::Hankaku,
        NamedKey::Hiragana => Named::Hiragana,
        NamedKey::HiraganaKatakana => Named::HiraganaKatakana,
        NamedKey::KanaMode => Named::KanaMode,
        NamedKey::KanjiMode => Named::KanjiMode,
        NamedKey::Katakana => Named::Katakana,
        NamedKey::Romaji => Named::Romaji,
        NamedKey::Zenkaku => Named::Zenkaku,
        NamedKey::ZenkakuHankaku => Named::ZenkakuHankaku,
        NamedKey::Soft1 => Named::Soft1,
        NamedKey::Soft2 => Named::Soft2,
        NamedKey::Soft3 => Named::Soft3,
        NamedKey::Soft4 => Named::Soft4,
        NamedKey::ChannelDown => Named::ChannelDown,
        NamedKey::ChannelUp => Named::ChannelUp,
        NamedKey::Close => Named::Close,
        NamedKey::MailForward => Named::MailForward,
        NamedKey::MailReply => Named::MailReply,
        NamedKey::MailSend => Named::MailSend,
        NamedKey::MediaClose => Named::MediaClose,
        NamedKey::MediaFastForward => Named::MediaFastForward,
        NamedKey::MediaPause => Named::MediaPause,
        NamedKey::MediaPlay => Named::MediaPlay,
        NamedKey::MediaPlayPause => Named::MediaPlayPause,
        NamedKey::MediaRecord => Named::MediaRecord,
        NamedKey::MediaRewind => Named::MediaRewind,
        NamedKey::MediaStop => Named::MediaStop,
        NamedKey::MediaTrackNext => Named::MediaTrackNext,
        NamedKey::MediaTrackPrevious => Named::MediaTrackPrevious,
        NamedKey::New => Named::New,
        NamedKey::Open => Named::Open,
        NamedKey::Print => Named::Print,
        NamedKey::Save => Named::Save,
        NamedKey::SpellCheck => Named::SpellCheck,
        NamedKey::Key11 => Named::Key11,
        NamedKey::Key12 => Named::Key12,
        NamedKey::AudioBalanceLeft => Named::AudioBalanceLeft,
        NamedKey::AudioBalanceRight => Named::AudioBalanceRight,
        NamedKey::AudioBassBoostDown => Named::AudioBassBoostDown,
        NamedKey::AudioBassBoostToggle => Named::AudioBassBoostToggle,
        NamedKey::AudioBassBoostUp => Named::AudioBassBoostUp,
        NamedKey::AudioFaderFront => Named::AudioFaderFront,
        NamedKey::AudioFaderRear => Named::AudioFaderRear,
        NamedKey::AudioSurroundModeNext => Named::AudioSurroundModeNext,
        NamedKey::AudioTrebleDown => Named::AudioTrebleDown,
        NamedKey::AudioTrebleUp => Named::AudioTrebleUp,
        NamedKey::AudioVolumeDown => Named::AudioVolumeDown,
        NamedKey::AudioVolumeUp => Named::AudioVolumeUp,
        NamedKey::AudioVolumeMute => Named::AudioVolumeMute,
        NamedKey::MicrophoneToggle => Named::MicrophoneToggle,
        NamedKey::MicrophoneVolumeDown => Named::MicrophoneVolumeDown,
        NamedKey::MicrophoneVolumeUp => Named::MicrophoneVolumeUp,
        NamedKey::MicrophoneVolumeMute => Named::MicrophoneVolumeMute,
        NamedKey::SpeechCorrectionList => Named::SpeechCorrectionList,
        NamedKey::SpeechInputToggle => Named::SpeechInputToggle,
        NamedKey::LaunchApplication1 => Named::LaunchApplication1,
        NamedKey::LaunchApplication2 => Named::LaunchApplication2,
        NamedKey::LaunchCalendar => Named::LaunchCalendar,
        NamedKey::LaunchContacts => Named::LaunchContacts,
        NamedKey::LaunchMail => Named::LaunchMail,
        NamedKey::LaunchMediaPlayer => Named::LaunchMediaPlayer,
        NamedKey::LaunchMusicPlayer => Named::LaunchMusicPlayer,
        NamedKey::LaunchPhone => Named::LaunchPhone,
        NamedKey::LaunchScreenSaver => Named::LaunchScreenSaver,
        NamedKey::LaunchSpreadsheet => Named::LaunchSpreadsheet,
        NamedKey::LaunchWebBrowser => Named::LaunchWebBrowser,
        NamedKey::LaunchWebCam => Named::LaunchWebCam,
        NamedKey::LaunchWordProcessor => Named::LaunchWordProcessor,
        NamedKey::BrowserBack => Named::BrowserBack,
        NamedKey::BrowserFavorites => Named::BrowserFavorites,
        NamedKey::BrowserForward => Named::BrowserForward,
        NamedKey::BrowserHome => Named::BrowserHome,
        NamedKey::BrowserRefresh => Named::BrowserRefresh,
        NamedKey::BrowserSearch => Named::BrowserSearch,
        NamedKey::BrowserStop => Named::BrowserStop,
        NamedKey::AppSwitch => Named::AppSwitch,
        NamedKey::Call => Named::Call,
        NamedKey::Camera => Named::Camera,
        NamedKey::CameraFocus => Named::CameraFocus,
        NamedKey::EndCall => Named::EndCall,
        NamedKey::GoBack => Named::GoBack,
        NamedKey::GoHome => Named::GoHome,
        NamedKey::HeadsetHook => Named::HeadsetHook,
        NamedKey::LastNumberRedial => Named::LastNumberRedial,
        NamedKey::Notification => Named::Notification,
        NamedKey::MannerMode => Named::MannerMode,
        NamedKey::VoiceDial => Named::VoiceDial,
        NamedKey::TV => Named::TV,
        NamedKey::TV3DMode => Named::TV3DMode,
        NamedKey::TVAntennaCable => Named::TVAntennaCable,
        NamedKey::TVAudioDescription => Named::TVAudioDescription,
        NamedKey::TVAudioDescriptionMixDown => Named::TVAudioDescriptionMixDown,
        NamedKey::TVAudioDescriptionMixUp => Named::TVAudioDescriptionMixUp,
        NamedKey::TVContentsMenu => Named::TVContentsMenu,
        NamedKey::TVDataService => Named::TVDataService,
        NamedKey::TVInput => Named::TVInput,
        NamedKey::TVInputComponent1 => Named::TVInputComponent1,
        NamedKey::TVInputComponent2 => Named::TVInputComponent2,
        NamedKey::TVInputComposite1 => Named::TVInputComposite1,
        NamedKey::TVInputComposite2 => Named::TVInputComposite2,
        NamedKey::TVInputHDMI1 => Named::TVInputHDMI1,
        NamedKey::TVInputHDMI2 => Named::TVInputHDMI2,
        NamedKey::TVInputHDMI3 => Named::TVInputHDMI3,
        NamedKey::TVInputHDMI4 => Named::TVInputHDMI4,
        NamedKey::TVInputVGA1 => Named::TVInputVGA1,
        NamedKey::TVMediaContext => Named::TVMediaContext,
        NamedKey::TVNetwork => Named::TVNetwork,
        NamedKey::TVNumberEntry => Named::TVNumberEntry,
        NamedKey::TVPower => Named::TVPower,
        NamedKey::TVRadioService => Named::TVRadioService,
        NamedKey::TVSatellite => Named::TVSatellite,
        NamedKey::TVSatelliteBS => Named::TVSatelliteBS,
        NamedKey::TVSatelliteCS => Named::TVSatelliteCS,
        NamedKey::TVSatelliteToggle => Named::TVSatelliteToggle,
        NamedKey::TVTerrestrialAnalog => Named::TVTerrestrialAnalog,
        NamedKey::TVTerrestrialDigital => Named::TVTerrestrialDigital,
        NamedKey::TVTimer => Named::TVTimer,
        NamedKey::AVRInput => Named::AVRInput,
        NamedKey::AVRPower => Named::AVRPower,
        NamedKey::ColorF0Red => Named::ColorF0Red,
        NamedKey::ColorF1Green => Named::ColorF1Green,
        NamedKey::ColorF2Yellow => Named::ColorF2Yellow,
        NamedKey::ColorF3Blue => Named::ColorF3Blue,
        NamedKey::ColorF4Grey => Named::ColorF4Grey,
        NamedKey::ColorF5Brown => Named::ColorF5Brown,
        NamedKey::ClosedCaptionToggle => Named::ClosedCaptionToggle,
        NamedKey::Dimmer => Named::Dimmer,
        NamedKey::DisplaySwap => Named::DisplaySwap,
        NamedKey::DVR => Named::DVR,
        NamedKey::Exit => Named::Exit,
        NamedKey::FavoriteClear0 => Named::FavoriteClear0,
        NamedKey::FavoriteClear1 => Named::FavoriteClear1,
        NamedKey::FavoriteClear2 => Named::FavoriteClear2,
        NamedKey::FavoriteClear3 => Named::FavoriteClear3,
        NamedKey::FavoriteRecall0 => Named::FavoriteRecall0,
        NamedKey::FavoriteRecall1 => Named::FavoriteRecall1,
        NamedKey::FavoriteRecall2 => Named::FavoriteRecall2,
        NamedKey::FavoriteRecall3 => Named::FavoriteRecall3,
        NamedKey::FavoriteStore0 => Named::FavoriteStore0,
        NamedKey::FavoriteStore1 => Named::FavoriteStore1,
        NamedKey::FavoriteStore2 => Named::FavoriteStore2,
        NamedKey::FavoriteStore3 => Named::FavoriteStore3,
        NamedKey::Guide => Named::Guide,
        NamedKey::GuideNextDay => Named::GuideNextDay,
        NamedKey::GuidePreviousDay => Named::GuidePreviousDay,
        NamedKey::Info => Named::Info,
        NamedKey::InstantReplay => Named::InstantReplay,
        NamedKey::Link => Named::Link,
        NamedKey::ListProgram => Named::ListProgram,
        NamedKey::LiveContent => Named::LiveContent,
        NamedKey::Lock => Named::Lock,
        NamedKey::MediaApps => Named::MediaApps,
        NamedKey::MediaAudioTrack => Named::MediaAudioTrack,
        NamedKey::MediaLast => Named::MediaLast,
        NamedKey::MediaSkipBackward => Named::MediaSkipBackward,
        NamedKey::MediaSkipForward => Named::MediaSkipForward,
        NamedKey::MediaStepBackward => Named::MediaStepBackward,
        NamedKey::MediaStepForward => Named::MediaStepForward,
        NamedKey::MediaTopMenu => Named::MediaTopMenu,
        NamedKey::NavigateIn => Named::NavigateIn,
        NamedKey::NavigateNext => Named::NavigateNext,
        NamedKey::NavigateOut => Named::NavigateOut,
        NamedKey::NavigatePrevious => Named::NavigatePrevious,
        NamedKey::NextFavoriteChannel => Named::NextFavoriteChannel,
        NamedKey::NextUserProfile => Named::NextUserProfile,
        NamedKey::OnDemand => Named::OnDemand,
        NamedKey::Pairing => Named::Pairing,
        NamedKey::PinPDown => Named::PinPDown,
        NamedKey::PinPMove => Named::PinPMove,
        NamedKey::PinPToggle => Named::PinPToggle,
        NamedKey::PinPUp => Named::PinPUp,
        NamedKey::PlaySpeedDown => Named::PlaySpeedDown,
        NamedKey::PlaySpeedReset => Named::PlaySpeedReset,
        NamedKey::PlaySpeedUp => Named::PlaySpeedUp,
        NamedKey::RandomToggle => Named::RandomToggle,
        NamedKey::RcLowBattery => Named::RcLowBattery,
        NamedKey::RecordSpeedNext => Named::RecordSpeedNext,
        NamedKey::RfBypass => Named::RfBypass,
        NamedKey::ScanChannelsToggle => Named::ScanChannelsToggle,
        NamedKey::ScreenModeNext => Named::ScreenModeNext,
        NamedKey::Settings => Named::Settings,
        NamedKey::SplitScreenToggle => Named::SplitScreenToggle,
        NamedKey::STBInput => Named::STBInput,
        NamedKey::STBPower => Named::STBPower,
        NamedKey::Subtitle => Named::Subtitle,
        NamedKey::Teletext => Named::Teletext,
        NamedKey::VideoModeNext => Named::VideoModeNext,
        NamedKey::Wink => Named::Wink,
        NamedKey::ZoomToggle => Named::ZoomToggle,
        NamedKey::F1 => Named::F1,
        NamedKey::F2 => Named::F2,
        NamedKey::F3 => Named::F3,
        NamedKey::F4 => Named::F4,
        NamedKey::F5 => Named::F5,
        NamedKey::F6 => Named::F6,
        NamedKey::F7 => Named::F7,
        NamedKey::F8 => Named::F8,
        NamedKey::F9 => Named::F9,
        NamedKey::F10 => Named::F10,
        NamedKey::F11 => Named::F11,
        NamedKey::F12 => Named::F12,
        NamedKey::F13 => Named::F13,
        NamedKey::F14 => Named::F14,
        NamedKey::F15 => Named::F15,
        NamedKey::F16 => Named::F16,
        NamedKey::F17 => Named::F17,
        NamedKey::F18 => Named::F18,
        NamedKey::F19 => Named::F19,
        NamedKey::F20 => Named::F20,
        NamedKey::F21 => Named::F21,
        NamedKey::F22 => Named::F22,
        NamedKey::F23 => Named::F23,
        NamedKey::F24 => Named::F24,
        NamedKey::F25 => Named::F25,
        NamedKey::F26 => Named::F26,
        NamedKey::F27 => Named::F27,
        NamedKey::F28 => Named::F28,
        NamedKey::F29 => Named::F29,
        NamedKey::F30 => Named::F30,
        NamedKey::F31 => Named::F31,
        NamedKey::F32 => Named::F32,
        NamedKey::F33 => Named::F33,
        NamedKey::F34 => Named::F34,
        NamedKey::F35 => Named::F35,
        _ => return None,
    })
}

fn convert_modifiers(mods: winit::keyboard::ModifiersState) -> keyboard::Modifiers {
    let mut result = keyboard::Modifiers::empty();
    if mods.shift_key() {
        result |= keyboard::Modifiers::SHIFT;
    }
    if mods.control_key() {
        result |= keyboard::Modifiers::CTRL;
    }
    if mods.alt_key() {
        result |= keyboard::Modifiers::ALT;
    }
    if mods.super_key() {
        result |= keyboard::Modifiers::LOGO;
    }
    result
}

fn convert_key_location(
    loc: winit::keyboard::KeyLocation,
) -> keyboard::Location {
    match loc {
        winit::keyboard::KeyLocation::Standard => keyboard::Location::Standard,
        winit::keyboard::KeyLocation::Left => keyboard::Location::Left,
        winit::keyboard::KeyLocation::Right => keyboard::Location::Right,
        winit::keyboard::KeyLocation::Numpad => keyboard::Location::Numpad,
    }
}

fn physical_key(
    key: winit::keyboard::PhysicalKey,
) -> keyboard::key::Physical {
    match key {
        winit::keyboard::PhysicalKey::Code(code) => match convert_key_code(code) {
            Some(c) => keyboard::key::Physical::Code(c),
            None => keyboard::key::Physical::Unidentified(NativeCode::Unidentified),
        },
        winit::keyboard::PhysicalKey::Unidentified(code) => {
            keyboard::key::Physical::Unidentified(match code {
                NativeKeyCode::Unidentified => NativeCode::Unidentified,
                NativeKeyCode::Android(n) => NativeCode::Android(n),
                NativeKeyCode::MacOS(n) => NativeCode::MacOS(n),
                NativeKeyCode::Windows(n) => NativeCode::Windows(n),
                NativeKeyCode::Xkb(n) => NativeCode::Xkb(n),
            })
        }
    }
}

fn convert_key_code(code: winit::keyboard::KeyCode) -> Option<keyboard::key::Code> {
    use keyboard::key::Code;
    use winit::keyboard::KeyCode;
    Some(match code {
        KeyCode::Backquote => Code::Backquote,
        KeyCode::Backslash => Code::Backslash,
        KeyCode::BracketLeft => Code::BracketLeft,
        KeyCode::BracketRight => Code::BracketRight,
        KeyCode::Comma => Code::Comma,
        KeyCode::Digit0 => Code::Digit0,
        KeyCode::Digit1 => Code::Digit1,
        KeyCode::Digit2 => Code::Digit2,
        KeyCode::Digit3 => Code::Digit3,
        KeyCode::Digit4 => Code::Digit4,
        KeyCode::Digit5 => Code::Digit5,
        KeyCode::Digit6 => Code::Digit6,
        KeyCode::Digit7 => Code::Digit7,
        KeyCode::Digit8 => Code::Digit8,
        KeyCode::Digit9 => Code::Digit9,
        KeyCode::Equal => Code::Equal,
        KeyCode::IntlBackslash => Code::IntlBackslash,
        KeyCode::IntlRo => Code::IntlRo,
        KeyCode::IntlYen => Code::IntlYen,
        KeyCode::KeyA => Code::KeyA,
        KeyCode::KeyB => Code::KeyB,
        KeyCode::KeyC => Code::KeyC,
        KeyCode::KeyD => Code::KeyD,
        KeyCode::KeyE => Code::KeyE,
        KeyCode::KeyF => Code::KeyF,
        KeyCode::KeyG => Code::KeyG,
        KeyCode::KeyH => Code::KeyH,
        KeyCode::KeyI => Code::KeyI,
        KeyCode::KeyJ => Code::KeyJ,
        KeyCode::KeyK => Code::KeyK,
        KeyCode::KeyL => Code::KeyL,
        KeyCode::KeyM => Code::KeyM,
        KeyCode::KeyN => Code::KeyN,
        KeyCode::KeyO => Code::KeyO,
        KeyCode::KeyP => Code::KeyP,
        KeyCode::KeyQ => Code::KeyQ,
        KeyCode::KeyR => Code::KeyR,
        KeyCode::KeyS => Code::KeyS,
        KeyCode::KeyT => Code::KeyT,
        KeyCode::KeyU => Code::KeyU,
        KeyCode::KeyV => Code::KeyV,
        KeyCode::KeyW => Code::KeyW,
        KeyCode::KeyX => Code::KeyX,
        KeyCode::KeyY => Code::KeyY,
        KeyCode::KeyZ => Code::KeyZ,
        KeyCode::Minus => Code::Minus,
        KeyCode::Period => Code::Period,
        KeyCode::Quote => Code::Quote,
        KeyCode::Semicolon => Code::Semicolon,
        KeyCode::Slash => Code::Slash,
        KeyCode::AltLeft => Code::AltLeft,
        KeyCode::AltRight => Code::AltRight,
        KeyCode::Backspace => Code::Backspace,
        KeyCode::CapsLock => Code::CapsLock,
        KeyCode::ContextMenu => Code::ContextMenu,
        KeyCode::ControlLeft => Code::ControlLeft,
        KeyCode::ControlRight => Code::ControlRight,
        KeyCode::Enter => Code::Enter,
        KeyCode::SuperLeft => Code::SuperLeft,
        KeyCode::SuperRight => Code::SuperRight,
        KeyCode::ShiftLeft => Code::ShiftLeft,
        KeyCode::ShiftRight => Code::ShiftRight,
        KeyCode::Space => Code::Space,
        KeyCode::Tab => Code::Tab,
        KeyCode::Convert => Code::Convert,
        KeyCode::KanaMode => Code::KanaMode,
        KeyCode::Lang1 => Code::Lang1,
        KeyCode::Lang2 => Code::Lang2,
        KeyCode::Lang3 => Code::Lang3,
        KeyCode::Lang4 => Code::Lang4,
        KeyCode::Lang5 => Code::Lang5,
        KeyCode::NonConvert => Code::NonConvert,
        KeyCode::Delete => Code::Delete,
        KeyCode::End => Code::End,
        KeyCode::Help => Code::Help,
        KeyCode::Home => Code::Home,
        KeyCode::Insert => Code::Insert,
        KeyCode::PageDown => Code::PageDown,
        KeyCode::PageUp => Code::PageUp,
        KeyCode::ArrowDown => Code::ArrowDown,
        KeyCode::ArrowLeft => Code::ArrowLeft,
        KeyCode::ArrowRight => Code::ArrowRight,
        KeyCode::ArrowUp => Code::ArrowUp,
        KeyCode::NumLock => Code::NumLock,
        KeyCode::Numpad0 => Code::Numpad0,
        KeyCode::Numpad1 => Code::Numpad1,
        KeyCode::Numpad2 => Code::Numpad2,
        KeyCode::Numpad3 => Code::Numpad3,
        KeyCode::Numpad4 => Code::Numpad4,
        KeyCode::Numpad5 => Code::Numpad5,
        KeyCode::Numpad6 => Code::Numpad6,
        KeyCode::Numpad7 => Code::Numpad7,
        KeyCode::Numpad8 => Code::Numpad8,
        KeyCode::Numpad9 => Code::Numpad9,
        KeyCode::NumpadAdd => Code::NumpadAdd,
        KeyCode::NumpadBackspace => Code::NumpadBackspace,
        KeyCode::NumpadClear => Code::NumpadClear,
        KeyCode::NumpadClearEntry => Code::NumpadClearEntry,
        KeyCode::NumpadComma => Code::NumpadComma,
        KeyCode::NumpadDecimal => Code::NumpadDecimal,
        KeyCode::NumpadDivide => Code::NumpadDivide,
        KeyCode::NumpadEnter => Code::NumpadEnter,
        KeyCode::NumpadEqual => Code::NumpadEqual,
        KeyCode::NumpadHash => Code::NumpadHash,
        KeyCode::NumpadMemoryAdd => Code::NumpadMemoryAdd,
        KeyCode::NumpadMemoryClear => Code::NumpadMemoryClear,
        KeyCode::NumpadMemoryRecall => Code::NumpadMemoryRecall,
        KeyCode::NumpadMemoryStore => Code::NumpadMemoryStore,
        KeyCode::NumpadMemorySubtract => Code::NumpadMemorySubtract,
        KeyCode::NumpadMultiply => Code::NumpadMultiply,
        KeyCode::NumpadParenLeft => Code::NumpadParenLeft,
        KeyCode::NumpadParenRight => Code::NumpadParenRight,
        KeyCode::NumpadStar => Code::NumpadStar,
        KeyCode::NumpadSubtract => Code::NumpadSubtract,
        KeyCode::Escape => Code::Escape,
        KeyCode::Fn => Code::Fn,
        KeyCode::FnLock => Code::FnLock,
        KeyCode::PrintScreen => Code::PrintScreen,
        KeyCode::ScrollLock => Code::ScrollLock,
        KeyCode::Pause => Code::Pause,
        KeyCode::BrowserBack => Code::BrowserBack,
        KeyCode::BrowserFavorites => Code::BrowserFavorites,
        KeyCode::BrowserForward => Code::BrowserForward,
        KeyCode::BrowserHome => Code::BrowserHome,
        KeyCode::BrowserRefresh => Code::BrowserRefresh,
        KeyCode::BrowserSearch => Code::BrowserSearch,
        KeyCode::BrowserStop => Code::BrowserStop,
        KeyCode::Eject => Code::Eject,
        KeyCode::LaunchApp1 => Code::LaunchApp1,
        KeyCode::LaunchApp2 => Code::LaunchApp2,
        KeyCode::LaunchMail => Code::LaunchMail,
        KeyCode::MediaPlayPause => Code::MediaPlayPause,
        KeyCode::MediaSelect => Code::MediaSelect,
        KeyCode::MediaStop => Code::MediaStop,
        KeyCode::MediaTrackNext => Code::MediaTrackNext,
        KeyCode::MediaTrackPrevious => Code::MediaTrackPrevious,
        KeyCode::Power => Code::Power,
        KeyCode::Sleep => Code::Sleep,
        KeyCode::AudioVolumeDown => Code::AudioVolumeDown,
        KeyCode::AudioVolumeMute => Code::AudioVolumeMute,
        KeyCode::AudioVolumeUp => Code::AudioVolumeUp,
        KeyCode::WakeUp => Code::WakeUp,
        KeyCode::Meta => Code::Meta,
        KeyCode::Hyper => Code::Hyper,
        KeyCode::Turbo => Code::Turbo,
        KeyCode::Abort => Code::Abort,
        KeyCode::Resume => Code::Resume,
        KeyCode::Suspend => Code::Suspend,
        KeyCode::Again => Code::Again,
        KeyCode::Copy => Code::Copy,
        KeyCode::Cut => Code::Cut,
        KeyCode::Find => Code::Find,
        KeyCode::Open => Code::Open,
        KeyCode::Paste => Code::Paste,
        KeyCode::Props => Code::Props,
        KeyCode::Select => Code::Select,
        KeyCode::Undo => Code::Undo,
        KeyCode::Hiragana => Code::Hiragana,
        KeyCode::Katakana => Code::Katakana,
        KeyCode::F1 => Code::F1,
        KeyCode::F2 => Code::F2,
        KeyCode::F3 => Code::F3,
        KeyCode::F4 => Code::F4,
        KeyCode::F5 => Code::F5,
        KeyCode::F6 => Code::F6,
        KeyCode::F7 => Code::F7,
        KeyCode::F8 => Code::F8,
        KeyCode::F9 => Code::F9,
        KeyCode::F10 => Code::F10,
        KeyCode::F11 => Code::F11,
        KeyCode::F12 => Code::F12,
        KeyCode::F13 => Code::F13,
        KeyCode::F14 => Code::F14,
        KeyCode::F15 => Code::F15,
        KeyCode::F16 => Code::F16,
        KeyCode::F17 => Code::F17,
        KeyCode::F18 => Code::F18,
        KeyCode::F19 => Code::F19,
        KeyCode::F20 => Code::F20,
        KeyCode::F21 => Code::F21,
        KeyCode::F22 => Code::F22,
        KeyCode::F23 => Code::F23,
        KeyCode::F24 => Code::F24,
        KeyCode::F25 => Code::F25,
        KeyCode::F26 => Code::F26,
        KeyCode::F27 => Code::F27,
        KeyCode::F28 => Code::F28,
        KeyCode::F29 => Code::F29,
        KeyCode::F30 => Code::F30,
        KeyCode::F31 => Code::F31,
        KeyCode::F32 => Code::F32,
        KeyCode::F33 => Code::F33,
        KeyCode::F34 => Code::F34,
        KeyCode::F35 => Code::F35,
        _ => return None,
    })
}
