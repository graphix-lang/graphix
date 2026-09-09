//! Smoke tests for every widget the TUI package exposes: each compiles
//! a minimal graphix expression producing the widget and renders it
//! through `TuiTestHarness`, so a ratatui panic on default inputs
//! surfaces here. Content assertions only where output is stable.

use crate::testing::TuiTestHarness;
use anyhow::Result;

#[tokio::test]
async fn text_compiles_and_renders() -> Result<()> {
    let mut h = TuiTestHarness::new(
        "use tui::*;\nuse tui::text::{self, *};\nlet result = text(&\"hello\")",
    )
    .await?;
    let lines = h.render_lines()?;
    assert!(
        lines[0].starts_with("hello"),
        "expected first line to start with `hello`, got {:?}",
        lines[0],
    );
    Ok(())
}

#[tokio::test]
async fn paragraph_compiles_and_renders() -> Result<()> {
    let mut h = TuiTestHarness::new(
        "use tui::*;\nuse tui::paragraph::{self, *};\nlet result = paragraph(&\"first line\")",
    )
    .await?;
    let lines = h.render_lines()?;
    assert!(
        lines[0].starts_with("first line"),
        "expected `first line` start, got {:?}",
        lines[0],
    );
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
    h.render()?;
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
let result = scrollbar(#position: &0, &inner)
"#,
    )
    .await?;
    h.render()?;
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
    h.render()?;
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
    h.render()?;
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
    h.render()?;
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
    h.render()?;
    Ok(())
}

#[tokio::test]
async fn sparkline_compiles_and_renders() -> Result<()> {
    let mut h = TuiTestHarness::new(
        r#"
use tui::*;
use tui::sparkline::{self, *};
let data = [10.0, 25.0, 40.0, 55.0, 70.0];
let result = sparkline(#max: &100, &data)
"#,
    )
    .await?;
    h.render()?;
    Ok(())
}

#[tokio::test]
async fn line_gauge_compiles_and_renders() -> Result<()> {
    let mut h = TuiTestHarness::new(
        "use tui::*;\nuse tui::line_gauge::{self, *};\nlet result = line_gauge(&0.5)",
    )
    .await?;
    h.render()?;
    Ok(())
}

#[tokio::test]
async fn line_gauge_out_of_range_does_not_panic() -> Result<()> {
    let mut h = TuiTestHarness::new(
        "use tui::*;\nuse tui::line_gauge::{self, *};\nlet result = line_gauge(&-0.3)",
    )
    .await?;
    h.render()?;
    Ok(())
}

#[tokio::test]
async fn gauge_in_range_renders() -> Result<()> {
    let mut h = TuiTestHarness::new(
        "use tui::*;\nuse tui::gauge::{self, *};\nlet result = gauge(&0.5)",
    )
    .await?;
    h.render()?;
    Ok(())
}

#[tokio::test]
async fn gauge_out_of_range_does_not_panic() -> Result<()> {
    // Pins the ratio clamp: ratatui's `Gauge::ratio` asserts on [0, 1].
    let mut h = TuiTestHarness::new(
        "use tui::*;\nuse tui::gauge::{self, *};\nlet result = gauge(&5.0)",
    )
    .await?;
    h.render()?;
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
    h.render()?;
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
    h.render()?;
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
    h.render()?;
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
    h.render()?;
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
    h.render()?;
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
    let lines = h.render_lines()?;
    assert!(lines[0].starts_with("BASE CONTENT"), "base missing: {:?}", lines[0]);
    assert!(
        lines.iter().any(|l| l.contains("MODAL CONTENT")),
        "modal missing: {lines:?}"
    );
    // the modal is centered, so its content must not be on the base's row
    assert!(!lines[0].contains("MODAL"), "modal not centered: {:?}", lines[0]);
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
    let lines = h.render_lines()?;
    assert!(lines[0].starts_with("JUST BASE"), "base missing: {:?}", lines[0]);
    assert!(lines.iter().all(|l| !l.contains("MODAL")));
    Ok(())
}

#[tokio::test]
async fn overlay_top_layer_captures_input() -> Result<()> {
    use crossterm::event::{Event, KeyCode, KeyEvent};
    use netidx::publisher::Value;
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
    h.dispatch_event(Event::Key(KeyEvent::from(KeyCode::Char('x')))).await?;
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
    use crossterm::event::{Event, KeyCode, KeyEvent};
    use netidx::publisher::Value;
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
    h.dispatch_event(Event::Key(KeyEvent::from(KeyCode::Char('x')))).await?;
    h.drain().await?;
    assert_eq!(
        h.get_watched("test::base_hit"),
        Some(&Value::I64(1)),
        "base missed the key"
    );
    Ok(())
}

#[tokio::test]
async fn line_edit_types_moves_and_deletes() -> Result<()> {
    use crossterm::event::{Event, KeyCode, KeyEvent};
    use netidx::publisher::Value;
    let mut h = TuiTestHarness::new(
        r#"
use tui::*;
use tui::input_handler::{self, *};
use tui::line_edit::{self, *};
use tui::text::{self, *};
let ed = line_edit::state("");
let v = ed.value;
let cur = ed.cursor;
let handle = |e: Event| -> [`Stop, `Continue] line_edit::handle(&ed, e);
let result = input_handler(#handle: &handle, &text(&[line_edit::view(&ed)]))
"#,
    )
    .await?;
    h.watch("test::v").await?;
    h.watch("test::cur").await?;
    h.render()?;
    for c in ['h', 'i', '!'] {
        h.dispatch_event(Event::Key(KeyEvent::from(KeyCode::Char(c)))).await?;
    }
    h.drain().await?;
    assert_eq!(h.get_watched("test::v"), Some(&Value::from("hi!")));
    assert_eq!(h.get_watched("test::cur"), Some(&Value::I64(3)));
    // Left over the '!' then backspace deletes the 'i'.
    h.dispatch_event(Event::Key(KeyEvent::from(KeyCode::Left))).await?;
    h.dispatch_event(Event::Key(KeyEvent::from(KeyCode::Backspace))).await?;
    h.drain().await?;
    assert_eq!(h.get_watched("test::v"), Some(&Value::from("h!")));
    assert_eq!(h.get_watched("test::cur"), Some(&Value::I64(1)));
    // Home then a character prepends.
    h.dispatch_event(Event::Key(KeyEvent::from(KeyCode::Home))).await?;
    h.dispatch_event(Event::Key(KeyEvent::from(KeyCode::Char('X')))).await?;
    h.drain().await?;
    assert_eq!(h.get_watched("test::v"), Some(&Value::from("Xh!")));
    assert_eq!(h.get_watched("test::cur"), Some(&Value::I64(1)));
    // End then Delete is a no-op at the boundary.
    h.dispatch_event(Event::Key(KeyEvent::from(KeyCode::End))).await?;
    h.dispatch_event(Event::Key(KeyEvent::from(KeyCode::Delete))).await?;
    h.drain().await?;
    assert_eq!(h.get_watched("test::v"), Some(&Value::from("Xh!")));
    assert_eq!(h.get_watched("test::cur"), Some(&Value::I64(3)));
    Ok(())
}

#[tokio::test]
async fn line_edit_masks_secrets() -> Result<()> {
    let mut h = TuiTestHarness::new(
        r#"
use tui::*;
use tui::line_edit::{self, *};
use tui::text::{self, *};
let ed = line_edit::state("abc");
let result = text(&[line_edit::view(#mask: "*", &ed)])
"#,
    )
    .await?;
    let lines = h.render_lines()?;
    assert!(lines[0].contains("***"), "mask missing: {:?}", lines[0]);
    assert!(!lines[0].contains("abc"), "secret leaked: {:?}", lines[0]);
    Ok(())
}
