//! What widgets draw as their inputs change: values that arrive late,
//! go away or come back, and the display's own inputs.

use crate::testing::TuiTestHarness;
use anyhow::Result;
use crossterm::event::{Event, KeyCode, KeyEvent};
use netidx::{protocol::valarray::ValArray, publisher::Value};

fn null() -> ValArray {
    ValArray::from_iter([Value::Null])
}

/// The selection shows once the items arrive, and follows them as they
/// shrink and grow back.
#[tokio::test]
async fn a_list_selects_items_that_arrive_late() -> Result<()> {
    let mut h = TuiTestHarness::new(
        r#"
use tui::*;
use tui::list::list;
let items: Array<Line> = [];
let fill = |x: Any| { items <- x ~ [line("A"), line("B"), line("C")]; null };
let shrink = |x: Any| { items <- x ~ [line("A")]; null };
let result = list(#highlight_symbol: &">>", #selected: &1, &items)
"#,
    )
    .await?;
    h.assert_lines(&[])?;
    let fill = h.compile_named_callable("test::fill").await?;
    let shrink = h.compile_named_callable("test::shrink").await?;
    h.call_callback(fill, null()).await?;
    h.assert_lines(&["  A", ">>B", "  C"])?;
    h.call_callback(shrink, null()).await?;
    h.assert_lines(&[">>A"])?;
    h.call_callback(fill, null()).await?;
    h.assert_lines(&["  A", ">>B", "  C"])
}

#[tokio::test]
async fn a_table_selection_set_to_null_deselects() -> Result<()> {
    let mut h = TuiTestHarness::new(
        r#"
use tui::*;
use tui::table::{self, *};
let sel: [i64, null] = 0;
let clear = |x: Any| { sel <- x ~ null; null };
let r1 = row([cell(line("a"))]);
let r2 = row([cell(line("b"))]);
let result = table(#highlight_symbol: &">>", #selected: &sel, &[&r1, &r2])
"#,
    )
    .await?;
    h.assert_lines(&[">>a", "  b"])?;
    let clear = h.compile_named_callable("test::clear").await?;
    h.call_callback(clear, null()).await?;
    h.assert_lines(&["a", "b"])
}

/// Values below one draw, and are scaled against each other.
#[tokio::test]
async fn a_sparkline_draws_fractions() -> Result<()> {
    let mut h = TuiTestHarness::with_viewport(
        r#"
use tui::sparkline::sparkline;
let result = sparkline(&[0.2, 0.5, 1.0, 0.4, 0.0 / 0.0])
"#,
        5,
        2,
    )
    .await?;
    h.assert_lines(&["  █  ", "▃██▆ "])
}

/// More bars than the chart has room for, and values past what ratatui's
/// u64 arithmetic held, draw what fits.
#[tokio::test]
async fn a_bar_chart_draws_the_bars_that_fit() -> Result<()> {
    let mut h = TuiTestHarness::with_viewport(
        r#"
use tui::*;
use tui::barchart::{self, *};
let bars = array::init(40000, |i| bar(&(i + 1)));
let result = bar_chart(#bar_gap: &0, &[bar_group(bars), bar_group([bar(&1)])])
"#,
        8,
        2,
    )
    .await?;
    h.assert_lines(&["    ▂▄▆█", "▂▄▆45678"])
}

#[tokio::test]
async fn a_bar_chart_scales_the_largest_values() -> Result<()> {
    let mut h = TuiTestHarness::with_viewport(
        r#"
use tui::*;
use tui::barchart::{self, *};
let big = 9223372036854775807;
let result = bar_chart(#bar_gap: &1, &[bar_group([bar(&1), bar(&big)])])
"#,
        3,
        3,
    )
    .await?;
    h.assert_lines(&["  █", "  █", "  █"])
}

#[tokio::test]
async fn a_paragraph_scrolls_unwrapped_text_sideways() -> Result<()> {
    let mut h = TuiTestHarness::with_viewport(
        r#"
use tui::paragraph::paragraph;
let result = paragraph(#wrap: &false, #scroll: &{x: 2, y: 0}, &"abcdefgh")
"#,
        4,
        1,
    )
    .await?;
    h.assert_lines(&["cdef"])
}

/// An offset counts content lines, past any screen size.
#[tokio::test]
async fn a_paragraph_scrolls_past_a_thousand_lines() -> Result<()> {
    let mut h = TuiTestHarness::with_viewport(
        r#"
use tui::*;
use tui::paragraph::paragraph;
let lines = array::init(2000, |i| line("L[i]"));
let result = paragraph(#scroll: &{x: 0, y: 1500}, &lines)
"#,
        6,
        2,
    )
    .await?;
    h.assert_lines(&["L1500", "L1501"])
}

/// The per-axis margins win over `#margin`, whichever is written.
#[tokio::test]
async fn layout_margins_apply_per_axis_last() -> Result<()> {
    let mut h = TuiTestHarness::with_viewport(
        r#"
use tui::layout::{child, layout};
use tui::text::text;
let result = layout(
  #horizontal_margin: &3,
  #margin: &1,
  #vertical_margin: &2,
  &[child(#constraint: `Min(1), text(&"x"))]
)
"#,
        8,
        5,
    )
    .await?;
    h.assert_lines(&["", "", "   x"])
}

/// A scrollbar with no content length draws no bar.
#[tokio::test]
async fn a_scrollbar_without_a_length_draws_no_bar() -> Result<()> {
    let mut h = TuiTestHarness::with_viewport(
        r#"
use tui::scrollbar::scrollbar;
use tui::text::text;
let result = scrollbar(#position: &0, &text(&"body"))
"#,
        6,
        2,
    )
    .await?;
    h.assert_lines(&["body"])
}

/// A calendar draws nothing until it has a date.
#[tokio::test]
async fn a_calendar_waits_for_its_date() -> Result<()> {
    let mut h = TuiTestHarness::new(
        r#"
use tui::calendar::{self, *};
let d: calendar::Date = never();
let result = calendar(&d)
"#,
    )
    .await?;
    h.assert_lines(&[])
}

/// One y label is drawn by nothing, not a division by zero.
#[tokio::test]
async fn a_chart_with_one_y_label_draws() -> Result<()> {
    let mut h = TuiTestHarness::with_viewport(
        r#"
use tui::*;
use tui::chart::{self, *};
let ds = dataset(&[(0.0, 0.0), (1.0, 1.0)]);
let result = chart(
  #x_axis: &axis({min: 0.0, max: 1.0}),
  #y_axis: &axis(#labels: [line("y")], {min: 0.0, max: 1.0}),
  &[ds]
)
"#,
        4,
        2,
    )
    .await?;
    h.assert_lines(&[" │ •", " │• "])
}

/// The root re-firing builds a new tree.
#[tokio::test]
async fn a_new_root_rebuilds_the_tree() -> Result<()> {
    let mut h = TuiTestHarness::new(
        r#"
use tui::text::text;
let screen = 0;
let next = |x: Any| { screen <- x ~ 1; null };
let result = select screen { 0 => text(&"zero"), _ => text(&"one") }
"#,
    )
    .await?;
    h.assert_lines(&["zero"])?;
    let next = h.compile_named_callable("test::next").await?;
    h.call_callback(next, null()).await?;
    h.assert_lines(&["one"])
}

/// The program sees the display's size and every event, and Ctrl-C
/// stops the display instead of reaching it.
#[tokio::test]
async fn the_program_sees_size_and_events() -> Result<()> {
    let mut h = TuiTestHarness::with_viewport(
        r#"
use tui::text::text;
let n = 0;
n <- tui::event ~ n + 1;
let result = text(&"[tui::size.width]x[tui::size.height] [n]")
"#,
        12,
        1,
    )
    .await?;
    h.drain().await?;
    h.assert_lines(&["12x1 0"])?;
    let key = |c| Event::Key(KeyEvent::from(KeyCode::Char(c)));
    h.dispatch_events([key('a'), key('b')]).await?;
    h.assert_lines(&["12x1 2"])?;
    h.dispatch_event(Event::Key(KeyEvent::new(
        KeyCode::Char('c'),
        crossterm::event::KeyModifiers::CONTROL,
    )))
    .await?;
    assert!(h.stopped());
    h.dispatch_event(key('d')).await?;
    h.assert_lines(&["12x1 2"])
}
