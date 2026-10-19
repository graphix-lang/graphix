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
  `Char("x") => `Stop,
  _ => `Continue
});
let result = input_handler(#handle: &outer, &input_handler(#handle: &inner, &text(&"x")))
"#,
    )
    .await?;
    h.watch("test::sel").await?;
    h.watch("test::inner_hits").await?;
    h.dispatch_events([key(KeyCode::Up), ch('x')]).await?;
    assert_eq!(h.get_watched("test::sel"), Some(&Value::I64(0)));
    assert_eq!(h.get_watched("test::inner_hits"), Some(&Value::I64(0)));
    h.dispatch_event(ch('y')).await?;
    assert_eq!(h.get_watched("test::inner_hits"), Some(&Value::I64(1)));
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
    // CR claude for claude: [test-gap/low] [test-gap] `before` accepts any count, so an
    // 'a' that the swap from fa to fb loses (count 0) passes. That lost event is the
    // failure this test is named for. The outcome is deterministic (the call and the
    // delete travel on one channel, and delete_callable still answers pending calls),
    // so pin the one value: 1 if fa answers 'a', 10 if fb does. Found by reading; not
    // run. (tests-tui-07)
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
    // CR claude for claude: [test-gap/low] [test-gap] This test cannot see a stall. An
    // event its handler leaves unanswered is answered Null by the runtime and dropped
    // (InputHandlerW::answered), so Esc reaches on_cancel even if 'a' and Tab produced
    // nothing. Wrap f.handle with a `Continue` counter, as
    // a_form_without_cancel_continues_escape does, and assert that 'a' and Tab each
    // answered `Continue`. Found by reading; not run. (tests-tui-04)
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
    for pasted in ["a\nb", "a\r\nb", "a\rb"] {
        let mut h = TuiTestHarness::new(EDITOR).await?;
        h.watch("test::v").await?;
        h.watch("test::cur").await?;
        h.dispatch_event(Event::Paste(pasted.into())).await?;
        assert_eq!(h.get_watched("test::v"), Some(&Value::from("hééa b")));
        assert_eq!(h.get_watched("test::cur"), Some(&Value::I64(6)));
    }
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
let selected_path: [string, null] = never();
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
    // CR claude for claude: [structure/low] [structure] This deadline-poll loop is
    // written out four times: here, twice more in this file, and in control_test. Add
    // `TuiTestHarness::wait_for(timeout, pred)` in testing.rs and use it at all four
    // sites. In the same spirit, the `key`/`ch` builders are defined five times across
    // the test files (they belong in test/mod.rs), widgets_test's per-test
    // crossterm/netidx `use`s belong at the top of the file, and examples_test's
    // REGISTER restates the unused crate::TEST_REGISTER. Found by reading; not run.
    // (tests-tui-10)
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

/// Navigation never reports a row of the listing it leaves: the selection
/// starts over when the new listing arrives, and an empty listing reports
/// null.
#[tokio::test(flavor = "multi_thread")]
async fn a_browser_selection_follows_the_listing() -> Result<()> {
    let mut h = TuiTestHarness::with_viewport(
        r#"
use tui::browser::{MoveCursor, browser};
let pa = array::map([0, 1, 2, 3], |i| sys::net::publish("/local/nav/a/a[i]", i));
let pb = array::map([0, 1], |i| sys::net::publish("/local/nav/b/b[i]", i));
let clock = sys::time::timer(duration:300.ms, true);
let n = 0;
n <- clock ~ n + 1;
let cursor: MoveCursor = never();
cursor <- select n { 2 => n ~ `Down(2), _ => never() };
let path = "/local/nav/a";
path <- select n { 4 => n ~ "/local/nav/b", 6 => n ~ "/local/nav/a/a3", _ => never() };
let sel: [string, null] = never();
let seen: Array<[string, null]> = [];
seen <- sel ~ array::push(seen, sel);
let result = browser(#cursor, #selected_row: &mut sel, #size: {width: 40, height: 8}, path)
"#,
        40,
        8,
    )
    .await?;
    h.watch("test::seen").await?;
    let deadline = tokio::time::Instant::now() + Duration::from_secs(10);
    loop {
        if let Some(Value::Array(a)) = h.get_watched("test::seen")
            && a.last() == Some(&Value::Null)
        {
            let mut seen: Vec<Value> = a.iter().cloned().collect();
            seen.dedup();
            let want = ["/local/nav/a/a0", "/local/nav/a/a2", "/local/nav/b/b0"]
                .map(Value::from)
                .into_iter()
                .chain([Value::Null])
                .collect::<Vec<_>>();
            assert_eq!(seen, want);
            return Ok(());
        }
        if tokio::time::Instant::now() > deadline {
            anyhow::bail!("seen {:?}", h.get_watched("test::seen"))
        }
        h.next_update(Duration::from_millis(500)).await?;
    }
}

/// A scroll by one row resubscribes only the row that enters: no row that
/// stays on screen blanks to #SUB, so no frame shows more than one.
#[tokio::test(flavor = "multi_thread")]
async fn a_browser_scroll_keeps_the_rows_that_stay() -> Result<()> {
    let mut h = TuiTestHarness::with_viewport(
        r##"
use tui::{Tui, browser::{MoveCursor, browser}};
let pubs = array::init(12, |k| sys::net::publish("/local/ring/r[k]", k));
let path = sys::time::after_idle(duration:200.ms, "/local/ring");
let cur: MoveCursor = never();
let b: Tui = browser(#rate: duration:10.ms, #size: {width: 40, height: 4}, #cursor: cur, path);
let progress = select b {
  `Table(t) => {
    let rows = *t.rows;
    (array::len(rows), array::len(array::filter(rows, |r| str::contains(#part: "#SUB", "[(*r).cells]"))))
  },
  _ => never()
};
let loaded = false;
loaded <- select (loaded, progress) { (false, (3, 0)) => true, _ => never() };
let i = 0;
let tick = select loaded { true => sys::time::timer(duration:100.ms, 10), false => never() };
i <- tick ~ i + 1;
cur <- select i { x if x >= 1 && x <= 6 => i ~ `Down(1), _ => never() };
let after_load = select loaded { true => progress.1, false => never() };
let worst = 0;
worst <- after_load ~ max(worst, after_load);
let answer = select i { 10 => worst, _ => never() };
let result = b
"##,
        40,
        4,
    )
    .await?;
    h.watch("test::answer").await?;
    let deadline = tokio::time::Instant::now() + Duration::from_secs(10);
    while !matches!(h.get_watched("test::answer"), Some(Value::I64(_))) {
        if tokio::time::Instant::now() > deadline {
            anyhow::bail!("no result; render:\n{}", h.render_lines()?.join("\n"))
        }
        h.next_update(Duration::from_millis(500)).await?;
    }
    assert_eq!(h.get_watched("test::answer"), Some(&Value::I64(1)));
    Ok(())
}

#[tokio::test]
async fn a_form_without_cancel_continues_escape() -> Result<()> {
    let mut h = TuiTestHarness::new(
        r#"
use tui::{form::form, input_handler::{Event, input_handler}};
let continued = 0;
let f = form(#title: "form", #on_submit: |_| null, []);
let handle = |e: Event| select f.handle(e) {
    r@ `Continue => { continued <- (r ~ continued) + 1; r },
    r => r
};
let result = input_handler(#handle: &handle, &f.view)
"#,
    )
    .await?;
    h.watch("test::continued").await?;
    h.dispatch_events([key(KeyCode::Esc), key(KeyCode::Esc)]).await?;
    assert_eq!(h.get_watched("test::continued"), Some(&Value::I64(2)));
    Ok(())
}

#[tokio::test]
async fn line_edit_clamps_external_cursors() -> Result<()> {
    use compact_str::format_compact;
    for (cursor, event, want, pos) in [
        (-1, ch('x'), "xé", 1),
        (i64::MAX, ch('x'), "éx", 2),
        (-1, Event::Paste("x".into()), "xé", 1),
        (i64::MAX, Event::Paste("x".into()), "éx", 2),
        (-1, key(KeyCode::Backspace), "é", 0),
        (i64::MAX, key(KeyCode::Backspace), "", 0),
    ] {
        let code = format_compact!(
            r#"
use tui::{{line_edit, input_handler::{{Event, input_handler}}, text::text}};
let ed: line_edit::State = {{value: "é", cursor: {cursor}}};
let value = ed.value;
let cursor = ed.cursor;
let handle = |e: Event| line_edit::handle(&mut ed, e);
let result = input_handler(#handle: &handle, &text(&[line_edit::view(&ed)]))
"#
        );
        let mut h = TuiTestHarness::new(&code).await?;
        h.watch("test::value").await?;
        h.watch("test::cursor").await?;
        h.dispatch_event(event).await?;
        assert_eq!(h.get_watched("test::value"), Some(&Value::from(want)));
        assert_eq!(h.get_watched("test::cursor"), Some(&Value::I64(pos)));
    }
    Ok(())
}

#[tokio::test]
async fn line_edit_highlights_the_whole_mask_at_the_cursor() -> Result<()> {
    use ratatui::style::Modifier;
    let mut h = TuiTestHarness::new(
        r#"
use tui::{line_edit, text::text};
let ed: line_edit::State = {value: "abc", cursor: 1};
let result = text(&[line_edit::view(#mask: "**", &ed)])
"#,
    )
    .await?;
    h.assert_lines(&["******"])?;
    let buffer = h.render()?;
    for x in 0..6 {
        assert_eq!(
            buffer[(x, 0)].modifier.contains(Modifier::REVERSED),
            (2..4).contains(&x),
            "cell {x}"
        );
    }
    Ok(())
}

#[tokio::test]
async fn line_edit_types_moves_and_deletes() -> Result<()> {
    let mut h = TuiTestHarness::new(EDITOR).await?;
    h.watch("test::v").await?;
    h.watch("test::cur").await?;
    h.dispatch_events((0..3).map(|_| key(KeyCode::Backspace))).await?;
    for c in ['h', 'i', '!'] {
        h.dispatch_event(ch(c)).await?;
    }
    assert_eq!(h.get_watched("test::v"), Some(&Value::from("hi!")));
    assert_eq!(h.get_watched("test::cur"), Some(&Value::I64(3)));
    h.dispatch_events([key(KeyCode::Left), key(KeyCode::Backspace)]).await?;
    assert_eq!(h.get_watched("test::v"), Some(&Value::from("h!")));
    assert_eq!(h.get_watched("test::cur"), Some(&Value::I64(1)));
    h.dispatch_events([key(KeyCode::Home), ch('X')]).await?;
    assert_eq!(h.get_watched("test::v"), Some(&Value::from("Xh!")));
    assert_eq!(h.get_watched("test::cur"), Some(&Value::I64(1)));
    h.dispatch_events([key(KeyCode::End), key(KeyCode::Delete)]).await?;
    assert_eq!(h.get_watched("test::v"), Some(&Value::from("Xh!")));
    assert_eq!(h.get_watched("test::cur"), Some(&Value::I64(3)));
    h.assert_lines(&["Xh!"])
}
