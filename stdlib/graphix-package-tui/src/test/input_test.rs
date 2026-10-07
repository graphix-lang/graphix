//! Input routing: the input handler's one-event-at-a-time protocol under
//! bursts, handler swaps and unanswered events, and the form, line editor
//! and browser built on it.

use crate::testing::TuiTestHarness;
use anyhow::Result;
use crossterm::event::{Event, KeyCode, KeyEvent, KeyModifiers};
use netidx::{protocol::valarray::ValArray, publisher::Value};
use std::time::Duration;

fn key(code: KeyCode) -> Event {
    Event::Key(KeyEvent::from(code))
}

fn ch(c: char) -> Event {
    key(KeyCode::Char(c))
}

fn strings(v: &[&str]) -> Value {
    Value::Array(ValArray::from_iter(v.iter().map(|s| Value::from(s.to_string()))))
}

/// A handler whose guard reads state its own arm writes answers again
/// when that state lands; the second answer must not be taken for the
/// next event's.
#[tokio::test]
async fn a_guard_refire_answers_nothing() -> Result<()> {
    let mut h = TuiTestHarness::new(
        r#"
use tui::input_handler::{self, *};
use tui::text::text;
let sel = 1;
let inner_hits = 0;
let inner = |e: Event| -> [`Stop, `Continue] on_press(e, |k| {
  inner_hits <- (k ~ inner_hits) + 1;
  `Stop
});
let outer = |e: Event| -> [`Stop, `Continue] on_press(e, |k| select k.code {
  kk@ `Up if sel > 0 => { sel <- (kk ~ sel) - 1; `Stop },
  _ => `Continue
});
let result = input_handler(#handle: &outer, &input_handler(#handle: &inner, &text(&"x")))
"#,
    )
    .await?;
    h.watch("test::sel").await?;
    h.watch("test::inner_hits").await?;
    h.dispatch_events([key(KeyCode::Up), ch('x'), ch('y')]).await?;
    assert_eq!(h.get_watched("test::sel"), Some(&Value::I64(0)));
    assert_eq!(h.get_watched("test::inner_hits"), Some(&Value::I64(2)));
    Ok(())
}

/// A handler replaced while a call is out goes on taking events.
#[tokio::test]
async fn a_handler_swapped_mid_call_keeps_working() -> Result<()> {
    let mut h = TuiTestHarness::new(
        r#"
use tui::input_handler::{self, *};
use tui::text::text;
let mode: [`A, `B] = `A;
let count = 0;
let fa = |e: Event| -> [`Stop, `Continue] on_press(e, |k| select k.code {
  kk@ `Char("m") => { mode <- kk ~ `B; `Stop },
  kk => { count <- (kk ~ count) + 1; `Stop }
});
let fb = |e: Event| -> [`Stop, `Continue] on_press(e, |k| {
  count <- (k ~ count) + 10;
  `Stop
});
let handle = select mode { `A => fa, `B => fb };
let result = input_handler(#handle: &handle, &text(&"x"))
"#,
    )
    .await?;
    h.watch("test::count").await?;
    h.dispatch_events([ch('m'), ch('a')]).await?;
    let before = h.get_watched("test::count").cloned();
    let before = match before {
        Some(Value::I64(n)) => n,
        v => anyhow::bail!("count is {v:?}"),
    };
    h.dispatch_event(ch('b')).await?;
    assert_eq!(h.get_watched("test::count"), Some(&Value::I64(before + 10)));
    Ok(())
}

/// An event the handler does not answer is dropped, and the next one is
/// handled.
#[tokio::test]
async fn an_unanswered_event_costs_one_event() -> Result<()> {
    let mut h = TuiTestHarness::new(
        r#"
use tui::input_handler::{self, *};
use tui::text::text;
let count = 0;
let handle = |e: Event| -> [`Stop, `Continue] on_press(e, |k| select k.code {
  `Char("x") => never(),
  kk => { count <- (kk ~ count) + 1; `Stop }
});
let result = input_handler(#handle: &handle, &text(&"x"))
"#,
    )
    .await?;
    h.watch("test::count").await?;
    h.dispatch_events([ch('x'), ch('a'), ch('b')]).await?;
    assert_eq!(h.get_watched("test::count"), Some(&Value::I64(2)));
    Ok(())
}

const FORM: &str = r#"
use tui::input_handler::{self, *};
use tui::form::{self, *};
let fields: Array<form::Field> = [
  { label: "one", value: "" },
  { label: "two", value: "" }
];
let submitted: Array<string> = [];
let cancelled = false;
let f = form(
  #title: "f",
  #on_submit: |vs| { submitted <- vs; null },
  #on_cancel: |e| { cancelled <- e ~ true; null },
  fields
);
let reseed = |x: Any| { fields <- x ~ [
  { label: "one", value: "" },
  { label: "two", value: "" }
]; null };
let result = input_handler(#handle: &f.handle, &f.view)
"#;

#[tokio::test]
async fn a_form_edits_moves_submits_and_cancels() -> Result<()> {
    let mut h = TuiTestHarness::new(FORM).await?;
    h.watch("test::submitted").await?;
    h.watch("test::cancelled").await?;
    h.dispatch_events([
        ch('a'),
        ch('b'),
        key(KeyCode::Tab),
        ch('c'),
        key(KeyCode::Enter),
    ])
    .await?;
    assert_eq!(h.get_watched("test::submitted"), Some(&strings(&["ab", "c"])));
    h.dispatch_events([key(KeyCode::BackTab), ch('d'), key(KeyCode::Enter)]).await?;
    assert_eq!(h.get_watched("test::submitted"), Some(&strings(&["abd", "c"])));
    h.dispatch_event(key(KeyCode::Esc)).await?;
    assert_eq!(h.get_watched("test::cancelled"), Some(&Value::Bool(true)));
    Ok(())
}

/// A new `fields` delivery starts the form over on its first field, even
/// after a key moved the focus.
#[tokio::test]
async fn a_reseeded_form_starts_on_the_first_field() -> Result<()> {
    let mut h = TuiTestHarness::new(FORM).await?;
    h.watch("test::submitted").await?;
    let reseed = h.compile_named_callable("test::reseed").await?;
    h.dispatch_event(key(KeyCode::Tab)).await?;
    h.call_callback(reseed, ValArray::from_iter([Value::Null])).await?;
    h.dispatch_events([ch('x'), key(KeyCode::Enter)]).await?;
    assert_eq!(h.get_watched("test::submitted"), Some(&strings(&["x", ""])));
    Ok(())
}

/// A form with no fields answers every key: its handler never stalls.
#[tokio::test]
async fn an_empty_form_answers_keys() -> Result<()> {
    let mut h = TuiTestHarness::new(
        r#"
use tui::input_handler::{self, *};
use tui::form::{self, *};
let fields: Array<form::Field> = [];
let cancelled = false;
let f = form(
  #title: "f",
  #on_submit: |vs| null,
  #on_cancel: |e| { cancelled <- e ~ true; null },
  fields
);
let result = input_handler(#handle: &f.handle, &f.view)
"#,
    )
    .await?;
    h.watch("test::cancelled").await?;
    h.dispatch_events([ch('a'), key(KeyCode::Tab), key(KeyCode::Esc)]).await?;
    assert_eq!(h.get_watched("test::cancelled"), Some(&Value::Bool(true)));
    Ok(())
}

const EDITOR: &str = r#"
use tui::input_handler::{self, *};
use tui::line_edit::{self, *};
use tui::text::text;
let ed = line_edit::state("héé");
let v = ed.value;
let cur = ed.cursor;
let continued = 0;
let handle = |e: Event| -> [`Stop, `Continue] select line_edit::handle(&mut ed, e) {
  r@ `Continue => { continued <- (r ~ continued) + 1; r },
  r => r
};
let result = input_handler(#handle: &handle, &text(&[line_edit::view(&ed)]))
"#;

/// The cursor counts characters, not bytes.
#[tokio::test]
async fn line_edit_counts_characters() -> Result<()> {
    let mut h = TuiTestHarness::new(EDITOR).await?;
    h.watch("test::v").await?;
    h.watch("test::cur").await?;
    assert_eq!(h.get_watched("test::cur"), Some(&Value::I64(3)));
    h.dispatch_event(key(KeyCode::Backspace)).await?;
    assert_eq!(h.get_watched("test::v"), Some(&Value::from("hé")));
    assert_eq!(h.get_watched("test::cur"), Some(&Value::I64(2)));
    h.dispatch_events([key(KeyCode::Left), ch('ü')]).await?;
    assert_eq!(h.get_watched("test::v"), Some(&Value::from("hüé")));
    h.assert_lines(&["hüé "])?;
    Ok(())
}

/// A chord is not typing: Ctrl+U and Alt+B leave the text alone and
/// continue, Shift and AltGr (Control+Alt) type.
#[tokio::test]
async fn line_edit_leaves_chords_to_the_caller() -> Result<()> {
    let mut h = TuiTestHarness::new(EDITOR).await?;
    h.watch("test::v").await?;
    h.watch("test::continued").await?;
    let chord = |c, m| Event::Key(KeyEvent::new(KeyCode::Char(c), m));
    h.dispatch_events([
        chord('u', KeyModifiers::CONTROL),
        chord('b', KeyModifiers::ALT),
        chord('X', KeyModifiers::SHIFT),
        chord('@', KeyModifiers::CONTROL | KeyModifiers::ALT),
    ])
    .await?;
    assert_eq!(h.get_watched("test::v"), Some(&Value::from("hééX@")));
    assert_eq!(h.get_watched("test::continued"), Some(&Value::I64(2)));
    Ok(())
}

/// A paste inserts its text at the cursor, line breaks as spaces.
#[tokio::test]
async fn line_edit_takes_a_paste() -> Result<()> {
    let mut h = TuiTestHarness::new(EDITOR).await?;
    h.watch("test::v").await?;
    h.watch("test::cur").await?;
    h.dispatch_event(Event::Paste("a\nb".into())).await?;
    assert_eq!(h.get_watched("test::v"), Some(&Value::from("hééa b")));
    assert_eq!(h.get_watched("test::cur"), Some(&Value::I64(6)));
    Ok(())
}

#[tokio::test]
async fn line_edit_masks_each_character_once() -> Result<()> {
    let mut h = TuiTestHarness::new(
        r#"
use tui::line_edit::{self, *};
use tui::text::text;
let ed = line_edit::state("héé");
let result = text(&[line_edit::view(#mask: "*", #focused: false, &ed)])
"#,
    )
    .await?;
    h.assert_lines(&["***"])
}

/// The browser reports the selected path whether or not the caller
/// asks for the selected row.
#[tokio::test(flavor = "multi_thread")]
async fn a_browser_reports_its_selected_path() -> Result<()> {
    let mut h = TuiTestHarness::with_viewport(
        r#"
use tui::browser::browser;
let p0 = sys::net::publish("/local/browse/r0", 0);
let p1 = sys::net::publish("/local/browse/r1", 1);
let path = sys::time::after_idle(duration:200.ms, "/local/browse");
let selected_path: string = never();
let result = browser(
  #size: {width: 40, height: 10},
  #selected_path: &mut selected_path,
  path
)
"#,
        40,
        10,
    )
    .await?;
    h.watch("test::selected_path").await?;
    let deadline = tokio::time::Instant::now() + Duration::from_secs(10);
    while h.get_watched("test::selected_path") != Some(&Value::from("/local/browse/r0")) {
        if tokio::time::Instant::now() > deadline {
            let at = h.get_watched("test::selected_path").cloned();
            anyhow::bail!(
                "selected_path is {at:?}; render:\n{}",
                h.render_lines()?.join("\n")
            )
        }
        h.next_update(Duration::from_millis(500)).await?;
    }
    Ok(())
}
