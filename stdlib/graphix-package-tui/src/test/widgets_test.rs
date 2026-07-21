//! Smoke tests for every widget the TUI package exposes.
//!
//! Each test compiles a minimal graphix expression that produces the
//! widget, builds the widget tree through `TuiTestHarness`, and
//! renders into a `TestBackend`. The render call exercises the same
//! ratatui code path the live runtime takes — so anything in ratatui
//! that would panic on our default inputs surfaces here.
//!
//! Conventions:
//! - The graphix wrapper is `use tui; use tui::<widget>; let result = ...`.
//! - Smoke tests just assert `render()` returns Ok (no panic, no
//!   widget-side error). Content assertions are reserved for the few
//!   cases where exact output is stable and meaningful.
//! - Panic-input regression tests live alongside the smoke test for
//!   the same widget so the failure mode is documented in one place.

use super::TuiTestHarness;
use anyhow::Result;

// ── text ─────────────────────────────────────────────────────────────

#[tokio::test]
async fn text_compiles_and_renders() -> Result<()> {
    let mut h =
        TuiTestHarness::new("use tui;\nuse tui::text;\nlet result = text(&\"hello\")")
            .await?;
    let lines = h.render_lines()?;
    assert!(
        lines[0].starts_with("hello"),
        "expected first line to start with `hello`, got {:?}",
        lines[0],
    );
    Ok(())
}

// ── paragraph ────────────────────────────────────────────────────────

#[tokio::test]
async fn paragraph_compiles_and_renders() -> Result<()> {
    let mut h = TuiTestHarness::new(
        "use tui;\nuse tui::paragraph;\nlet result = paragraph(&\"first line\")",
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

// ── block ────────────────────────────────────────────────────────────

#[tokio::test]
async fn block_compiles_and_renders() -> Result<()> {
    let mut h = TuiTestHarness::new(
        r#"
use tui;
use tui::block;
use tui::paragraph;
let inner = paragraph(&"in");
let result = block(#border: &`All, #title: &line("T"), &inner)
"#,
    )
    .await?;
    h.render()?;
    Ok(())
}

// ── scrollbar ────────────────────────────────────────────────────────

#[tokio::test]
async fn scrollbar_compiles_and_renders() -> Result<()> {
    let mut h = TuiTestHarness::new(
        r#"
use tui;
use tui::scrollbar;
use tui::paragraph;
let inner = paragraph(&"body");
let result = scrollbar(#position: &0, &inner)
"#,
    )
    .await?;
    h.render()?;
    Ok(())
}

// ── layout ───────────────────────────────────────────────────────────

#[tokio::test]
async fn layout_compiles_and_renders() -> Result<()> {
    let mut h = TuiTestHarness::new(
        r#"
use tui;
use tui::layout;
use tui::text;
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

// ── tabs ─────────────────────────────────────────────────────────────

#[tokio::test]
async fn tabs_compiles_and_renders() -> Result<()> {
    let mut h = TuiTestHarness::new(
        r#"
use tui;
use tui::tabs;
use tui::paragraph;
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

// ── barchart (bar_chart in graphix) ──────────────────────────────────

#[tokio::test]
async fn barchart_compiles_and_renders() -> Result<()> {
    let mut h = TuiTestHarness::new(
        r#"
use tui;
use tui::barchart;
let b = bar(#label: &line("Q1"), &42);
let result = bar_chart(&[bar_group(#label: line("G"), [b])])
"#,
    )
    .await?;
    h.render()?;
    Ok(())
}

// ── chart ────────────────────────────────────────────────────────────

#[tokio::test]
async fn chart_compiles_and_renders() -> Result<()> {
    let mut h = TuiTestHarness::new(
        r#"
use tui;
use tui::chart;
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

// ── sparkline ────────────────────────────────────────────────────────

#[tokio::test]
async fn sparkline_compiles_and_renders() -> Result<()> {
    let mut h = TuiTestHarness::new(
        r#"
use tui;
use tui::sparkline;
let data = [10.0, 25.0, 40.0, 55.0, 70.0];
let result = sparkline(#max: &100, &data)
"#,
    )
    .await?;
    h.render()?;
    Ok(())
}

// ── line_gauge ───────────────────────────────────────────────────────

#[tokio::test]
async fn line_gauge_compiles_and_renders() -> Result<()> {
    let mut h = TuiTestHarness::new(
        "use tui;\nuse tui::line_gauge;\nlet result = line_gauge(&0.5)",
    )
    .await?;
    h.render()?;
    Ok(())
}

#[tokio::test]
async fn line_gauge_out_of_range_does_not_panic() -> Result<()> {
    let mut h = TuiTestHarness::new(
        "use tui;\nuse tui::line_gauge;\nlet result = line_gauge(&-0.3)",
    )
    .await?;
    h.render()?;
    Ok(())
}

// ── gauge ────────────────────────────────────────────────────────────

#[tokio::test]
async fn gauge_in_range_renders() -> Result<()> {
    let mut h =
        TuiTestHarness::new("use tui;\nuse tui::gauge;\nlet result = gauge(&0.5)")
            .await?;
    h.render()?;
    Ok(())
}

#[tokio::test]
async fn gauge_out_of_range_does_not_panic() -> Result<()> {
    // Without the clamp_ratio fix this would panic inside ratatui's
    // Gauge::ratio assert. The harness exists in part to keep this
    // regression test alive.
    let mut h =
        TuiTestHarness::new("use tui;\nuse tui::gauge;\nlet result = gauge(&5.0)")
            .await?;
    h.render()?;
    Ok(())
}

// ── input_handler ────────────────────────────────────────────────────

#[tokio::test]
async fn input_handler_compiles_and_renders() -> Result<()> {
    // input_handler's `#handle` arg isn't optional, so we have to
    // supply a callable. Wrap a simple text widget so it has
    // something to render.
    let mut h = TuiTestHarness::new(
        r#"
use tui;
use tui::input_handler;
use tui::text;
let on_event = |e: Event| -> [`Stop, `Continue] select e { _ => `Continue };
let inner = text(&"x");
let result = input_handler(#handle: &on_event, &inner)
"#,
    )
    .await?;
    h.render()?;
    Ok(())
}

// ── list ─────────────────────────────────────────────────────────────

#[tokio::test]
async fn list_compiles_and_renders() -> Result<()> {
    let mut h = TuiTestHarness::new(
        r#"
use tui;
use tui::list;
let items = [line("A"), line("B"), line("C")];
let result = list(#selected: &0, &items)
"#,
    )
    .await?;
    h.render()?;
    Ok(())
}

// ── table ────────────────────────────────────────────────────────────

#[tokio::test]
async fn table_compiles_and_renders() -> Result<()> {
    let mut h = TuiTestHarness::new(
        r#"
use tui;
use tui::table;
let r1 = row([cell(line("a")), cell(line("1"))]);
let r2 = row([cell(line("b")), cell(line("2"))]);
let result = table(#selected: &0, &[&r1, &r2])
"#,
    )
    .await?;
    h.render()?;
    Ok(())
}

// ── calendar ─────────────────────────────────────────────────────────

#[tokio::test]
async fn calendar_compiles_and_renders() -> Result<()> {
    let mut h = TuiTestHarness::with_viewport(
        "use tui;\nuse tui::calendar;\nlet d = date(2024, 5, 15);\nlet result = calendar(&d)",
        24,
        10,
    )
    .await?;
    h.render()?;
    Ok(())
}

// ── canvas ───────────────────────────────────────────────────────────

#[tokio::test]
async fn canvas_compiles_and_renders() -> Result<()> {
    let mut h = TuiTestHarness::new(
        r#"
use tui;
use tui::canvas;
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
