mod control_test;
mod examples_test;
mod input_test;
mod panic_test;
mod render_test;
mod terminal_test;
mod widgets_test;

use crossterm::event::{Event, KeyCode, KeyEvent};

fn key(code: KeyCode) -> Event {
    Event::Key(KeyEvent::from(code))
}

fn ch(c: char) -> Event {
    key(KeyCode::Char(c))
}
