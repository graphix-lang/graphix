//! Convert winit events to iced events.
//!
//! Based on iced_winit/src/conversion.rs but adapted for our event loop.

use iced_core::{keyboard, mouse, window, Event, Point, Size};
use winit::event::{ElementState, MouseButton, WindowEvent};
use winit::keyboard::{Key, NamedKey, NativeKeyCode};
use keyboard::key::NativeCode;

/// Convert a winit WindowEvent to zero or more iced Events.
pub(crate) fn window_event(
    event: &WindowEvent,
    scale_factor: f64,
    modifiers: winit::keyboard::ModifiersState,
) -> Vec<Event> {
    let mut events = Vec::new();
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
        WindowEvent::KeyboardInput { event, .. } => {
            if let Some(key) = key_code(&event.logical_key) {
                let mods = convert_modifiers(modifiers);
                let ev = match event.state {
                    ElementState::Pressed => keyboard::Event::KeyPressed {
                        key,
                        modified_key: key_code(&event.logical_key)
                            .unwrap_or(keyboard::Key::Unidentified),
                        modifiers: mods,
                        location: convert_key_location(event.location),
                        text: event.text.as_ref().map(|s| s.to_string().into()),
                        physical_key: physical_key(event.physical_key),
                        repeat: event.repeat,
                    },
                    ElementState::Released => keyboard::Event::KeyReleased {
                        key,
                        modified_key: key_code(&event.logical_key)
                            .unwrap_or(keyboard::Key::Unidentified),
                        modifiers: mods,
                        location: convert_key_location(event.location),
                        physical_key: physical_key(event.physical_key),
                    },
                };
                events.push(Event::Keyboard(ev));
            }
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

fn key_code(key: &Key) -> Option<keyboard::Key> {
    match key {
        Key::Named(named) => Some(keyboard::Key::Named(convert_named_key(*named))),
        Key::Character(c) => {
            Some(keyboard::Key::Character(c.as_str().to_string().into()))
        }
        Key::Unidentified(_) | Key::Dead(_) => None,
    }
}

fn convert_named_key(key: NamedKey) -> keyboard::key::Named {
    use keyboard::key::Named;
    match key {
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
        NamedKey::Delete => Named::Delete,
        NamedKey::Insert => Named::Insert,
        NamedKey::Escape => Named::Escape,
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
        _ => Named::Space, // fallback
    }
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
            None => keyboard::key::Physical::Unidentified(NativeCode::Xkb(0)),
        },
        winit::keyboard::PhysicalKey::Unidentified(code) => {
            keyboard::key::Physical::Unidentified(match code {
                NativeKeyCode::Android(n) => NativeCode::Android(n),
                NativeKeyCode::MacOS(n) => NativeCode::MacOS(n),
                NativeKeyCode::Windows(n) => NativeCode::Windows(n),
                NativeKeyCode::Xkb(n) => NativeCode::Xkb(n),
                _ => NativeCode::Xkb(0),
            })
        }
    }
}

fn convert_key_code(code: winit::keyboard::KeyCode) -> Option<keyboard::key::Code> {
    use keyboard::key::Code;
    use winit::keyboard::KeyCode;
    Some(match code {
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
        KeyCode::Space => Code::Space,
        KeyCode::Enter => Code::Enter,
        KeyCode::Tab => Code::Tab,
        KeyCode::Escape => Code::Escape,
        _ => return None,
    })
}
