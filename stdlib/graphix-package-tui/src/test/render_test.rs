//! What widgets draw as their inputs change: values that arrive late,
//! go away or come back, and the display's own inputs.

use super::ch;
use crate::testing::TuiTestHarness;
use anyhow::Result;
use crossterm::event::{Event, KeyCode, KeyEvent, KeyModifiers};
use graphix_package::Ended;
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

/// A row that has not arrived holds its slot, so the selection, an index
/// into the program's rows, names the same row before and after.
#[tokio::test]
async fn a_table_keeps_the_slot_of_a_row_that_arrives_late() -> Result<()> {
    let mut h = TuiTestHarness::new(
        r#"
use tui::*;
use tui::table::{self, *};
let r0 = never();
let fill = |x: Any| { r0 <- x ~ row([cell(line("zero"))]); null };
let r1 = row([cell(line("one"))]);
let r2 = row([cell(line("two"))]);
let result = table(#highlight_symbol: &">>", #selected: &1, &[&r0, &r1, &r2])
"#,
    )
    .await?;
    h.assert_lines(&["", ">>one", "  two"])?;
    let fill = h.compile_named_callable("test::fill").await?;
    h.call_callback(fill, null()).await?;
    h.assert_lines(&["  zero", ">>one", "  two"])
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

#[tokio::test]
async fn a_paragraph_scrolls_past_u16_lines() -> Result<()> {
    let mut h = TuiTestHarness::with_viewport(
        r#"
use tui::paragraph::paragraph;
let text = str::join(#sep: "\n", array::init(70000, |i| "L[i]"));
let result = paragraph(#wrap: &false, #scroll: &{x: 0, y: 69998}, &text)
"#,
        8,
        3,
    )
    .await?;
    h.assert_lines(&["L69998", "L69999"])
}

/// Wrapped, an offset counts the rows the lines wrap to.
#[tokio::test]
async fn a_wrapped_paragraph_scrolls_past_u16_rows() -> Result<()> {
    let mut h = TuiTestHarness::with_viewport(
        r#"
use tui::paragraph::paragraph;
let text = str::join(#sep: "\n", array::init(40000, |i| "[i + 100000] x"));
let result = paragraph(#scroll: &{x: 0, y: 70001}, &text)
"#,
        6,
        2,
    )
    .await?;
    h.assert_lines(&["x", "135001"])
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

/// The program sees the display's size and every event, and a stop key
/// stops the display instead of reaching it: Ctrl-C leaves it, Ctrl-\
/// (Ctrl-4 as a terminal reports its control code) interrupts.
#[tokio::test]
async fn the_program_sees_size_and_events() -> Result<()> {
    for (key, stop) in
        [('c', Ended::Left), ('\\', Ended::Interrupted), ('4', Ended::Interrupted)]
    {
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
        h.dispatch_events([ch('a'), ch('b')]).await?;
        h.assert_lines(&["12x1 2"])?;
        h.dispatch_event(Event::Key(KeyEvent::new(
            KeyCode::Char(key),
            KeyModifiers::CONTROL,
        )))
        .await?;
        h.assert_lines(&["12x1 2"])?;
        assert_eq!(h.stopped(), Some(stop), "{key:?}");
    }
    Ok(())
}

#[tokio::test]
async fn size_outputs_require_writable_references() -> Result<()> {
    use compact_str::format_compact;
    for body in [
        "tui::block::block(#size: OUTPUT, &w)",
        "tui::scrollbar::scrollbar(#size: OUTPUT, &w)",
        "tui::tabs::tabs(#size: OUTPUT, &[(tui::line(\"tab\"), w)])",
        "tui::layout::layout(&[tui::layout::child(#size: OUTPUT, #constraint: `Fill(1), w)])",
        "tui::overlay::overlay(#layers: &[tui::overlay::layer(#size: OUTPUT, w)], w)",
    ] {
        for (n, output) in [
            ("null = null", "&n"),
            ("null = null", "&mut n"),
            ("tui::Size = never()", "&n"),
        ] {
            let code = format_compact!(
                "let n: {n}; let w = tui::text::text(&\"hello\"); let result = {}",
                body.replace("OUTPUT", output)
            );
            match TuiTestHarness::new(&code).await {
                Ok(_) => panic!("invalid size output accepted: {code}"),
                Err(e) => {
                    assert!(format_compact!("{e:#}").contains("&mut Size"), "{e:#}")
                }
            }
        }
        let code = format_compact!(
            "let n: tui::Size = never(); let w = tui::text::text(&\"hello\"); let result = {}",
            body.replace("OUTPUT", "&mut n")
        );
        let mut h = TuiTestHarness::new(&code).await?;
        h.watch("test::n").await?;
        h.render()?;
        h.drain().await?;
        assert!(matches!(h.get_watched("test::n"), Some(Value::Array(_))));
    }
    Ok(())
}

#[tokio::test]
async fn a_zero_area_is_reported_on_the_first_draw() -> Result<()> {
    let mut h = TuiTestHarness::with_viewport(
        r#"
use tui::{Size, block::block, text::text};
let size: Size = never();
let result = block(#border: &`All, #size: &mut size, &text(&"body"))
"#,
        2,
        2,
    )
    .await?;
    h.watch("test::size").await?;
    h.render()?;
    h.drain().await?;
    assert_eq!(h.get_watched("test::size"), Some(&crate::SizeV::new(0, 0).into()));
    Ok(())
}

#[tokio::test]
async fn unchanged_widgets_keep_their_size_reports() -> Result<()> {
    use compact_str::format_compact;
    for body in [
        "refired",
        "block(&refired)",
        "tui::layout::layout(&(tick ~ [tui::layout::child(#constraint: `Fill(1), stable)]))",
        "tui::overlay::overlay(#layers: &(tick ~ [tui::overlay::layer(stable)]), text(&\"base\"))",
        "tui::tabs::tabs(&[(tui::line(\"[tick]\"), stable)])",
    ] {
        let code = format_compact!(
            r#"
use tui::{{Size, block::block, text::text}};
let tick = 0;
let pulse = |n: i64| {{ tick <- n; null }};
let size: Size = never();
let reports = count(size);
let stable = block(#size: &mut size, &text(&"body"));
let refired = tick ~ stable;
let result = {body}
"#
        );
        let mut h = TuiTestHarness::new(&code).await?;
        h.watch("test::reports").await?;
        let pulse = h.compile_named_callable("test::pulse").await?;
        h.render()?;
        h.drain().await?;
        assert_eq!(h.get_watched("test::reports"), Some(&Value::I64(1)), "{body}");
        for n in 1..=3 {
            h.call_callback(pulse, ValArray::from_iter([Value::I64(n)])).await?;
            h.render()?;
            h.drain().await?;
            assert_eq!(h.get_watched("test::reports"), Some(&Value::I64(1)), "{body}");
        }
    }
    Ok(())
}

#[tokio::test]
async fn resizing_updates_the_backend_and_widget_layout() -> Result<()> {
    use ratatui::layout::Rect;
    let mut h = TuiTestHarness::new(
        r#"
use tui::{block::block, text::text};
let result = block(#border: &`All, &text(&"[tui::size.width]x[tui::size.height]"))
"#,
    )
    .await?;
    for (width, height) in [(80, 20), (12, 4)] {
        h.dispatch_event(Event::Resize(width, height)).await?;
        let buffer = h.render()?;
        assert_eq!(buffer.area, Rect::new(0, 0, width, height));
        assert_eq!(buffer[(width - 1, height - 1)].symbol(), "┘");
        let expected = compact_str::format_compact!("{width}x{height}");
        for (x, c) in expected.char_indices() {
            assert_eq!(
                buffer[(x as u16 + 1, 1)].symbol(),
                &expected[x..x + c.len_utf8()]
            );
        }
    }
    Ok(())
}
