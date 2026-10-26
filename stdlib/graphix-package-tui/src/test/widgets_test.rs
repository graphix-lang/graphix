//! Smoke tests for every widget the TUI package exposes: each compiles
//! a minimal graphix expression producing the widget and renders it
//! through `TuiTestHarness`, so a ratatui panic on default inputs
//! surfaces here, and each pins what it draws. Forms, the line editor
//! and the browser are in input_test.

use super::ch;
use crate::testing::TuiTestHarness;
use anyhow::Result;
use crossterm::event::{Event, KeyCode, KeyEvent, KeyEventKind, KeyModifiers};
use netidx::publisher::Value;
use ratatui::{
    buffer::Cell,
    style::{Color, Modifier, Style},
};

fn keyed(code: KeyCode, kind: KeyEventKind) -> Event {
    Event::Key(KeyEvent::new_with_kind(code, KeyModifiers::NONE, kind))
}

#[tokio::test]
async fn text_compiles_and_renders() -> Result<()> {
    let mut h = TuiTestHarness::new(
        "use tui::*;\nuse tui::text::{self, *};\nlet result = text(&\"hello\")",
    )
    .await?;
    h.assert_lines(&["hello"])?;
    Ok(())
}

#[tokio::test]
async fn paragraph_compiles_and_renders() -> Result<()> {
    let mut h = TuiTestHarness::new(
        "use tui::*;\nuse tui::paragraph::{self, *};\nlet result = paragraph(&\"first line\")",
    )
    .await?;
    h.assert_lines(&["first line"])?;
    Ok(())
}

#[tokio::test]
async fn block_compiles_and_renders() -> Result<()> {
    let mut h = TuiTestHarness::new(
        r#"
use tui::*;
use tui::block::{self, *};
use tui::paragraph::{self, *};
let inner = paragraph(&"in");
let result = block(#border: &`All, #title: &line("T"), &inner)
"#,
    )
    .await?;
    h.assert_lines(&[
        "┌T─────────────────────────────────────┐",
        "│in                                    │",
        "│                                      │",
        "│                                      │",
        "│                                      │",
        "│                                      │",
        "│                                      │",
        "│                                      │",
        "│                                      │",
        "└──────────────────────────────────────┘",
    ])?;
    Ok(())
}

#[tokio::test]
async fn scrollbar_compiles_and_renders() -> Result<()> {
    let mut h = TuiTestHarness::new(
        r#"
use tui::*;
use tui::scrollbar::{self, *};
use tui::paragraph::{self, *};
let inner = paragraph(&"body");
let result = scrollbar(#position: &0, #content_length: &20, &inner)
"#,
    )
    .await?;
    h.assert_lines(&[
        "body                                   █",
        "                                       █",
        "                                       █",
    ])?;
    Ok(())
}

#[tokio::test]
async fn layout_compiles_and_renders() -> Result<()> {
    let mut h = TuiTestHarness::new(
        r#"
use tui::*;
use tui::layout::{self, *};
use tui::text::{self, *};
let a = text(&"A");
let b = text(&"B");
let result = layout(
    #direction: &`Horizontal,
    &[
        child(#constraint: `Percentage(50), a),
        child(#constraint: `Percentage(50), b)
    ]
)
"#,
    )
    .await?;
    h.assert_lines(&["A                   B"])?;
    Ok(())
}

#[tokio::test]
async fn tabs_compiles_and_renders() -> Result<()> {
    let mut h = TuiTestHarness::new(
        r#"
use tui::*;
use tui::tabs::{self, *};
use tui::paragraph::{self, *};
let one = paragraph(&"one");
let two = paragraph(&"two");
let result = tabs(
    #selected: &0,
    &[(line("A"), one), (line("B"), two)]
)
"#,
    )
    .await?;
    h.assert_lines(&[" A │ B", "one"])?;
    Ok(())
}

#[tokio::test]
async fn barchart_compiles_and_renders() -> Result<()> {
    let mut h = TuiTestHarness::new(
        r#"
use tui::*;
use tui::barchart::{self, *};
let b = bar(#label: &line("Q1"), &42);
let result = bar_chart(&[bar_group(#label: line("G"), [b])])
"#,
    )
    .await?;
    h.assert_lines(&["█", "█", "█", "█", "█", "█", "█", "█", "Q", "G"])?;
    Ok(())
}

#[tokio::test]
async fn chart_compiles_and_renders() -> Result<()> {
    let mut h = TuiTestHarness::new(
        r#"
use tui::*;
use tui::chart::{self, *};
let pts: Array<(f64, f64)> = [(0.0, 0.0), (1.0, 1.0), (2.0, 4.0)];
let ds = dataset(#graph_type: &`Line, #marker: &`Dot, &pts);
let result = chart(
    #x_axis: &axis({min: 0.0, max: 2.0}),
    #y_axis: &axis({min: 0.0, max: 4.0}),
    &[ds]
)
"#,
    )
    .await?;
    h.assert_lines(&[
        "                                      ••",
        "                                   •••",
        "                                 ••",
        "                              •••",
        "                           •••",
        "                         ••",
        "                      •••",
        "                ••••••",
        "      ••••••••••",
        "••••••",
    ])?;
    Ok(())
}

#[tokio::test]
async fn sparkline_compiles_and_renders() -> Result<()> {
    let mut h = TuiTestHarness::new(
        r#"
use tui::*;
use tui::sparkline::{self, *};
let data = [10.0, 25.0, 40.0, 55.0, 70.0];
let result = sparkline(#max: &100.0, &data)
"#,
    )
    .await?;
    h.assert_lines(&[
        "",
        "",
        "",
        "    ▇",
        "   ▃█",
        "   ██",
        "  ▇██",
        " ▄███",
        " ████",
        "▇████",
    ])?;
    Ok(())
}

#[tokio::test]
async fn line_gauge_compiles_and_renders() -> Result<()> {
    let mut h = TuiTestHarness::new(
        "use tui::*;\nuse tui::line_gauge::{self, *};\nlet result = line_gauge(&0.5)",
    )
    .await?;
    h.assert_lines(&[" 50% ───────────────────────────────────"])?;
    Ok(())
}

#[tokio::test]
async fn gauge_in_range_renders() -> Result<()> {
    let mut h = TuiTestHarness::new(
        "use tui::*;\nuse tui::gauge::{self, *};\nlet result = gauge(&0.5)",
    )
    .await?;
    h.assert_lines(&[
        "████████████████████",
        "████████████████████",
        "████████████████████",
        "████████████████████",
        "████████████████████",
        "██████████████████50%",
        "████████████████████",
        "████████████████████",
        "████████████████████",
        "████████████████████",
    ])?;
    Ok(())
}

#[tokio::test]
async fn input_handler_compiles_and_renders() -> Result<()> {
    // `#handle` isn't optional, so supply a callable; the text widget
    // gives it something to render.
    let mut h = TuiTestHarness::new(
        r#"
use tui::*;
use tui::input_handler::{self, *};
use tui::text::{self, *};
let on_event = |e: Event| -> [`Stop, `Continue] select e { _ => `Continue };
let inner = text(&"x");
let result = input_handler(#handle: &on_event, &inner)
"#,
    )
    .await?;
    h.assert_lines(&["x"])?;
    Ok(())
}

#[tokio::test]
async fn list_compiles_and_renders() -> Result<()> {
    let mut h = TuiTestHarness::new(
        r#"
use tui::*;
use tui::list::{self, *};
let items = [line("A"), line("B"), line("C")];
let result = list(#selected: &0, &items)
"#,
    )
    .await?;
    h.assert_lines(&["A", "B", "C"])?;
    Ok(())
}

#[tokio::test]
async fn table_compiles_and_renders() -> Result<()> {
    let mut h = TuiTestHarness::new(
        r#"
use tui::*;
use tui::table::{self, *};
let r1 = row([cell(line("a")), cell(line("1"))]);
let r2 = row([cell(line("b")), cell(line("2"))]);
let result = table(#selected: &0, &[&r1, &r2])
"#,
    )
    .await?;
    h.assert_lines(&["a                    1", "b                    2"])?;
    Ok(())
}

#[tokio::test]
async fn calendar_compiles_and_renders() -> Result<()> {
    let mut h = TuiTestHarness::with_viewport(
        "use tui::*;\nuse tui::calendar::{self, *};\nlet d = date(2024, 5, 15);\nlet result = calendar(&d)",
        24,
        10,
    )
    .await?;
    h.assert_lines(&[
        "           1  2  3  4",
        "  5  6  7  8  9 10 11",
        " 12 13 14 15 16 17 18",
        " 19 20 21 22 23 24 25",
        " 26 27 28 29 30 31",
    ])?;
    Ok(())
}

#[tokio::test]
async fn a_short_calendar_stays_in_its_area() -> Result<()> {
    let mut h = TuiTestHarness::with_viewport(
        r#"
use tui::{style, layout::{child, layout}, text::text, calendar::{calendar, date}};
let cal = calendar(#show_month: &style(), &date(2026, 8, 15));
let result = layout(#direction: &`Vertical, &[
  child(#constraint: `Length(3), cal),
  child(#constraint: `Fill(1), text(&"below"))
])
"#,
        24,
        8,
    )
    .await?;
    h.assert_lines(&[
        "      August 2026",
        "                    1",
        "  2  3  4  5  6  7  8",
        "below",
    ])?;
    Ok(())
}

/// A screen of more than 65535 cells.
#[tokio::test]
async fn a_short_calendar_draws_on_a_large_screen() -> Result<()> {
    let mut h = TuiTestHarness::with_viewport(
        r#"
use tui::{layout::{child, layout}, text::text, calendar::{calendar, date}};
let result = layout(&[
  child(#constraint: `Length(3), calendar(&date(2024, 5, 15))),
  child(#constraint: `Fill(1), text(&"below"))
])
"#,
        300,
        300,
    )
    .await?;
    h.assert_lines(&[
        "           1  2  3  4",
        "  5  6  7  8  9 10 11",
        " 12 13 14 15 16 17 18",
        "below",
    ])
}

#[tokio::test]
async fn a_short_calendar_keeps_its_background() -> Result<()> {
    let mut h = TuiTestHarness::with_viewport(
        r#"
use tui::{style, block::block, calendar::{calendar, date}};
let result = block(#style: &style(#bg: `Blue), &calendar(&date(2024, 5, 15)))
"#,
        24,
        3,
    )
    .await?;
    let buf = h.render()?;
    assert_eq!(buf[(0, 0)].bg, Color::Blue);
    assert_eq!(buf[(23, 2)].bg, Color::Blue);
    Ok(())
}

#[tokio::test]
async fn canvas_compiles_and_renders() -> Result<()> {
    let mut h = TuiTestHarness::new(
        r#"
use tui::*;
use tui::canvas::{self, *};
let l = `Line({color: `Red, x1: 0.0, y1: 0.0, x2: 10.0, y2: 5.0});
let result = canvas(
    #x_bounds: &{min: 0.0, max: 10.0},
    #y_bounds: &{min: 0.0, max: 10.0},
    &[&l]
)
"#,
    )
    .await?;
    h.assert_lines(&[
        "",
        "",
        "",
        "",
        "",
        "                                ⢀⣀⡠⠤⠔⠒⠊⠉",
        "                        ⣀⣀⠤⠤⠔⠒⠊⠉⠁",
        "                ⣀⣀⠤⠤⠒⠒⠉⠉",
        "       ⢀⣀⡠⠤⠔⠒⠒⠉⠉",
        "⣀⡠⠤⠔⠒⠊⠉⠁",
    ])?;
    Ok(())
}

#[tokio::test]
async fn canvas_bounds_too_wide_to_scale_draw_nothing() -> Result<()> {
    let mut h = TuiTestHarness::new(
        r#"
use tui::canvas::canvas;
let p = `Points({color: `Red, coords: [(1e308, 1.0)]});
let l = `Label({line: tui::line("L"), x: 1e308, y: 1.0});
let result = canvas(
    #marker: &`HalfBlock,
    #x_bounds: &{min: 0.0, max: 1e308},
    #y_bounds: &{min: 0.0, max: 2.0},
    &[&p, &l]
)
"#,
    )
    .await?;
    h.assert_lines(&[""])?;
    Ok(())
}

#[tokio::test]
async fn overlay_renders_base_and_layer() -> Result<()> {
    let mut h = TuiTestHarness::new(
        r#"
use tui::*;
use tui::overlay::{self, *};
use tui::paragraph::{self, *};
let base = paragraph(&"BASE CONTENT");
let modal = layer(paragraph(&"MODAL CONTENT"));
let result = overlay(#layers: &[modal], base)
"#,
    )
    .await?;
    h.assert_lines(&["BASE CONTENT", "", "        MODAL CONTENT"])?;
    Ok(())
}

#[tokio::test]
async fn overlay_empty_layers_is_base() -> Result<()> {
    let mut h = TuiTestHarness::new(
        r#"
use tui::*;
use tui::overlay::{self, *};
use tui::paragraph::{self, *};
let layers: Array<overlay::Layer> = [];
let result = overlay(#layers: &layers, paragraph(&"JUST BASE"))
"#,
    )
    .await?;
    h.assert_lines(&["JUST BASE"])?;
    Ok(())
}

#[tokio::test]
async fn overlay_top_layer_captures_input() -> Result<()> {
    let mut h = TuiTestHarness::new(
        r#"
use tui::*;
use tui::input_handler::{self, *};
use tui::overlay::{self, *};
use tui::paragraph::{self, *};
let base_hit = 0;
let layer_hit = 0;
let on_base = |e: Event| -> [`Stop, `Continue] select e {
  k@ `Key(_) => { base_hit <- (k ~ base_hit) + 1; `Stop },
  _ => `Continue
};
let on_layer = |e: Event| -> [`Stop, `Continue] select e {
  k@ `Key(_) => { layer_hit <- (k ~ layer_hit) + 1; `Stop },
  _ => `Continue
};
let base = input_handler(#handle: &on_base, &paragraph(&"base"));
let modal = layer(input_handler(#handle: &on_layer, &paragraph(&"modal")));
let result = overlay(#layers: &[modal], base)
"#,
    )
    .await?;
    h.watch("test::base_hit").await?;
    h.watch("test::layer_hit").await?;
    h.render()?;
    h.dispatch_event(ch('x')).await?;
    h.drain().await?;
    assert_eq!(
        h.get_watched("test::layer_hit"),
        Some(&Value::I64(1)),
        "layer missed the key"
    );
    assert_eq!(
        h.get_watched("test::base_hit"),
        Some(&Value::I64(0)),
        "base saw a captured key"
    );
    Ok(())
}

#[tokio::test]
async fn overlay_no_layer_routes_to_base() -> Result<()> {
    let mut h = TuiTestHarness::new(
        r#"
use tui::*;
use tui::input_handler::{self, *};
use tui::overlay::{self, *};
use tui::paragraph::{self, *};
let base_hit = 0;
let on_base = |e: Event| -> [`Stop, `Continue] select e {
  k@ `Key(_) => { base_hit <- (k ~ base_hit) + 1; `Stop },
  _ => `Continue
};
let layers: Array<overlay::Layer> = [];
let base = input_handler(#handle: &on_base, &paragraph(&"base"));
let result = overlay(#layers: &layers, base)
"#,
    )
    .await?;
    h.watch("test::base_hit").await?;
    h.render()?;
    h.dispatch_event(ch('x')).await?;
    h.drain().await?;
    assert_eq!(
        h.get_watched("test::base_hit"),
        Some(&Value::I64(1)),
        "base missed the key"
    );
    Ok(())
}

#[tokio::test]
async fn on_press_runs_the_callback_on_presses_only() -> Result<()> {
    let mut h = TuiTestHarness::new(
        r#"
use tui::*;
use tui::input_handler::{self, *};
use tui::paragraph::{self, *};
let downs = 0;
let calls = 0;
let handle = |e: Event| on_press(e, |k| {
  calls <- (k ~ calls) + 1;
  select k.code {
    `Down => { downs <- (k ~ downs) + 1; `Stop },
    _ => `Continue
  }
});
let result = input_handler(#handle: &handle, &paragraph(&"base"))
"#,
    )
    .await?;
    h.watch("test::downs").await?;
    h.watch("test::calls").await?;
    h.render()?;
    for e in [
        keyed(KeyCode::Down, KeyEventKind::Press),
        keyed(KeyCode::Down, KeyEventKind::Release),
        keyed(KeyCode::Down, KeyEventKind::Repeat),
        keyed(KeyCode::Down, KeyEventKind::Press),
        Event::Resize(80, 24),
        Event::Paste("x".into()),
        keyed(KeyCode::Char('x'), KeyEventKind::Press),
    ] {
        h.dispatch_event(e).await?;
        h.drain().await?;
    }
    assert_eq!(h.get_watched("test::calls"), Some(&Value::I64(3)), "one call per press");
    assert_eq!(h.get_watched("test::downs"), Some(&Value::I64(2)), "two presses of Down");
    Ok(())
}

#[tokio::test]
async fn a_modal_handler_over_on_press_lets_nothing_through() -> Result<()> {
    let mut h = TuiTestHarness::new(
        r#"
use tui::*;
use tui::input_handler::{self, *};
use tui::paragraph::{self, *};
let leaked = 0;
let closed = 0;
let inner = |e: Event| { leaked <- (e ~ leaked) + 1; e ~ `Stop };
let modal = |e: Event| {
  on_press(e, |k| select k.code {
    `Esc => { closed <- (k ~ closed) + 1; `Stop },
    _ => `Stop
  });
  e ~ `Stop
};
let result = input_handler(#handle: &modal, &input_handler(#handle: &inner, &paragraph(&"base")))
"#,
    )
    .await?;
    h.watch("test::leaked").await?;
    h.watch("test::closed").await?;
    h.render()?;
    for e in [
        keyed(KeyCode::Esc, KeyEventKind::Press),
        keyed(KeyCode::Esc, KeyEventKind::Release),
        keyed(KeyCode::Esc, KeyEventKind::Release),
        Event::Resize(80, 24),
        Event::Resize(81, 24),
        keyed(KeyCode::Char('x'), KeyEventKind::Press),
        keyed(KeyCode::Esc, KeyEventKind::Press),
    ] {
        h.dispatch_event(e).await?;
        h.drain().await?;
    }
    assert_eq!(h.get_watched("test::closed"), Some(&Value::I64(2)), "two presses of Esc");
    assert_eq!(
        h.get_watched("test::leaked"),
        Some(&Value::I64(0)),
        "the child saw an event"
    );
    Ok(())
}

#[tokio::test]
async fn span_styles_keep_rgb_indexed_and_bold() -> Result<()> {
    let mut h = TuiTestHarness::new(r#"
use tui::{line, span, style, text::text};
let result = text(&[line([
  span(#style: style(#fg: `Rgb({r: 12, g: 34, b: 56}), #bg: `Indexed(123), #add_modifier: [`Bold]), "R"),
  span(#style: style(#fg: `Indexed(67)), "I")
])])
"#).await?;
    h.assert_cell(
        0,
        0,
        Cell::new("R").set_style(
            Style::new()
                .fg(Color::Rgb(12, 34, 56))
                .bg(Color::Indexed(123))
                .add_modifier(Modifier::BOLD),
        ),
    )?;
    h.assert_cell(1, 0, Cell::new("I").set_style(Style::new().fg(Color::Indexed(67))))?;
    h.assert_cell(2, 0, &Cell::new(" "))
}

#[tokio::test]
async fn list_styles_highlight_only_the_selected_row() -> Result<()> {
    let mut h = TuiTestHarness::new(
        r#"
use tui::{line, style, list::list};
let result = list(#selected: &1,
  #highlight_style: &style(#fg: `Yellow, #bg: `Blue, #add_modifier: [`Bold]),
  &[line("A"), line("B")])
"#,
    )
    .await?;
    h.assert_cell(0, 0, &Cell::new("A"))?;
    h.assert_cell(
        0,
        1,
        Cell::new("B").set_style(
            Style::new().fg(Color::Yellow).bg(Color::Blue).add_modifier(Modifier::BOLD),
        ),
    )
}

#[tokio::test]
async fn table_styles_highlight_only_the_selected_row() -> Result<()> {
    let mut h = TuiTestHarness::new(
        r#"
use tui::{line, style, table::{cell, row, table}};
let result = table(#selected: &1,
  #row_highlight_style: &style(#fg: `Green, #bg: `Red, #add_modifier: [`Reversed]),
  &[&row([cell(line("A"))]), &row([cell(line("B"))])])
"#,
    )
    .await?;
    h.assert_cell(0, 0, &Cell::new("A"))?;
    h.assert_cell(
        0,
        1,
        Cell::new("B").set_style(
            Style::new().fg(Color::Green).bg(Color::Red).add_modifier(Modifier::REVERSED),
        ),
    )
}
