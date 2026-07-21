//! Panic-surface regression tests.
//!
//! Each test drives a widget with an input that — without a clamp /
//! sanitize wrapper somewhere — would or could trigger an assert in
//! ratatui (or in a downstream cast). The harness renders the widget
//! through a `TestBackend`, which exercises the real ratatui code
//! path: any panic surfaces here as a test failure with the original
//! `panicked at ...` message intact.
//!
//! Tests grouped by widget. A test that PASSES means the input is
//! either safe by construction (ratatui silently ignores it) or
//! we've clamped it on our side. A test that FAILS means a clamp is
//! still missing.

use super::TuiTestHarness;
use anyhow::Result;

// ── gauge / line_gauge ───────────────────────────────────────────────
//
// ratio outside [0, 1] panics in ratatui's Gauge::ratio /
// LineGauge::ratio. Fixed by clamp_ratio in gauge.rs.

#[tokio::test]
async fn gauge_ratio_above_one_does_not_panic() -> Result<()> {
    let mut h =
        TuiTestHarness::new("use tui;\nuse tui::gauge;\nlet result = gauge(&5.0)")
            .await?;
    h.render()?;
    Ok(())
}

#[tokio::test]
async fn gauge_ratio_negative_does_not_panic() -> Result<()> {
    let mut h =
        TuiTestHarness::new("use tui;\nuse tui::gauge;\nlet result = gauge(&-0.3)")
            .await?;
    h.render()?;
    Ok(())
}

#[tokio::test]
async fn gauge_ratio_nan_does_not_panic() -> Result<()> {
    let mut h = TuiTestHarness::new(
        "use tui;\nuse tui::gauge;\nlet result = gauge(&(0.0 / 0.0))",
    )
    .await?;
    h.render()?;
    Ok(())
}

#[tokio::test]
async fn line_gauge_ratio_negative_does_not_panic() -> Result<()> {
    let mut h = TuiTestHarness::new(
        "use tui;\nuse tui::line_gauge;\nlet result = line_gauge(&-0.5)",
    )
    .await?;
    h.render()?;
    Ok(())
}

#[tokio::test]
async fn line_gauge_ratio_above_one_does_not_panic() -> Result<()> {
    let mut h = TuiTestHarness::new(
        "use tui;\nuse tui::line_gauge;\nlet result = line_gauge(&2.5)",
    )
    .await?;
    h.render()?;
    Ok(())
}

// ── barchart ─────────────────────────────────────────────────────────
//
// bar_width / bar_gap / group_gap are u16 in ratatui; we cast from
// i64. max is u64. Negative i64 → underflow on cast → wraps to a huge
// value that may panic when ratatui multiplies it into layout.

#[tokio::test]
async fn barchart_bar_width_negative_does_not_panic() -> Result<()> {
    let mut h = TuiTestHarness::new(
        r#"
use tui;
use tui::barchart;
let b = bar(#label: &line("X"), &10);
let result = bar_chart(#bar_width: &-1, &[bar_group(#label: line("G"), [b])])
"#,
    )
    .await?;
    h.render()?;
    Ok(())
}

#[tokio::test]
async fn barchart_bar_gap_huge_does_not_panic() -> Result<()> {
    let mut h = TuiTestHarness::new(
        r#"
use tui;
use tui::barchart;
let b = bar(#label: &line("X"), &10);
let result = bar_chart(#bar_gap: &999999, &[bar_group(#label: line("G"), [b])])
"#,
    )
    .await?;
    h.render()?;
    Ok(())
}

#[tokio::test]
async fn barchart_max_zero_does_not_panic() -> Result<()> {
    let mut h = TuiTestHarness::new(
        r#"
use tui;
use tui::barchart;
let b = bar(#label: &line("X"), &10);
let result = bar_chart(#max: &0, &[bar_group(#label: line("G"), [b])])
"#,
    )
    .await?;
    h.render()?;
    Ok(())
}

#[tokio::test]
async fn barchart_max_negative_does_not_panic() -> Result<()> {
    let mut h = TuiTestHarness::new(
        r#"
use tui;
use tui::barchart;
let b = bar(#label: &line("X"), &10);
let result = bar_chart(#max: &-50, &[bar_group(#label: line("G"), [b])])
"#,
    )
    .await?;
    h.render()?;
    Ok(())
}

#[tokio::test]
async fn barchart_negative_bar_value_does_not_panic() -> Result<()> {
    let mut h = TuiTestHarness::new(
        r#"
use tui;
use tui::barchart;
let b = bar(#label: &line("X"), &-10);
let result = bar_chart(&[bar_group(#label: line("G"), [b])])
"#,
    )
    .await?;
    h.render()?;
    Ok(())
}

// ── sparkline ────────────────────────────────────────────────────────

#[tokio::test]
async fn sparkline_max_zero_does_not_panic() -> Result<()> {
    let mut h = TuiTestHarness::new(
        r#"
use tui;
use tui::sparkline;
let data = [1.0, 2.0, 3.0];
let result = sparkline(#max: &0, &data)
"#,
    )
    .await?;
    h.render()?;
    Ok(())
}

#[tokio::test]
async fn sparkline_max_negative_does_not_panic() -> Result<()> {
    let mut h = TuiTestHarness::new(
        r#"
use tui;
use tui::sparkline;
let data = [1.0, 2.0, 3.0];
let result = sparkline(#max: &-100, &data)
"#,
    )
    .await?;
    h.render()?;
    Ok(())
}

// ── scrollbar ────────────────────────────────────────────────────────

#[tokio::test]
async fn scrollbar_position_negative_does_not_panic() -> Result<()> {
    let mut h = TuiTestHarness::new(
        r#"
use tui;
use tui::scrollbar;
use tui::paragraph;
let inner = paragraph(&"body");
let result = scrollbar(#position: &-5, &inner)
"#,
    )
    .await?;
    h.render()?;
    Ok(())
}

#[tokio::test]
async fn scrollbar_content_length_negative_does_not_panic() -> Result<()> {
    let mut h = TuiTestHarness::new(
        r#"
use tui;
use tui::scrollbar;
use tui::paragraph;
let inner = paragraph(&"body");
let result = scrollbar(#position: &0, #content_length: &-10, &inner)
"#,
    )
    .await?;
    h.render()?;
    Ok(())
}

// ── tabs ─────────────────────────────────────────────────────────────

#[tokio::test]
async fn tabs_selected_negative_does_not_panic() -> Result<()> {
    let mut h = TuiTestHarness::new(
        r#"
use tui;
use tui::tabs;
use tui::paragraph;
let one = paragraph(&"one");
let result = tabs(#selected: &-3, &[(line("A"), one)])
"#,
    )
    .await?;
    h.render()?;
    Ok(())
}

#[tokio::test]
async fn tabs_selected_out_of_range_does_not_panic() -> Result<()> {
    let mut h = TuiTestHarness::new(
        r#"
use tui;
use tui::tabs;
use tui::paragraph;
let one = paragraph(&"one");
let result = tabs(#selected: &99, &[(line("A"), one)])
"#,
    )
    .await?;
    h.render()?;
    Ok(())
}

// ── list ─────────────────────────────────────────────────────────────

#[tokio::test]
async fn list_selected_negative_does_not_panic() -> Result<()> {
    let mut h = TuiTestHarness::new(
        r#"
use tui;
use tui::list;
let items = [line("A"), line("B")];
let result = list(#selected: &-2, &items)
"#,
    )
    .await?;
    h.render()?;
    Ok(())
}

#[tokio::test]
async fn list_selected_out_of_range_does_not_panic() -> Result<()> {
    let mut h = TuiTestHarness::new(
        r#"
use tui;
use tui::list;
let items = [line("A"), line("B")];
let result = list(#selected: &99, &items)
"#,
    )
    .await?;
    h.render()?;
    Ok(())
}

#[tokio::test]
async fn list_scroll_negative_does_not_panic() -> Result<()> {
    let mut h = TuiTestHarness::new(
        r#"
use tui;
use tui::list;
let items = [line("A"), line("B")];
let result = list(#scroll: &-5, &items)
"#,
    )
    .await?;
    h.render()?;
    Ok(())
}

// ── table ────────────────────────────────────────────────────────────

#[tokio::test]
async fn table_selected_negative_does_not_panic() -> Result<()> {
    let mut h = TuiTestHarness::new(
        r#"
use tui;
use tui::table;
let r1 = row([cell(line("a"))]);
let result = table(#selected: &-1, &[&r1])
"#,
    )
    .await?;
    h.render()?;
    Ok(())
}

#[tokio::test]
async fn table_selected_out_of_range_does_not_panic() -> Result<()> {
    let mut h = TuiTestHarness::new(
        r#"
use tui;
use tui::table;
let r1 = row([cell(line("a"))]);
let result = table(#selected: &50, &[&r1])
"#,
    )
    .await?;
    h.render()?;
    Ok(())
}

// ── chart ────────────────────────────────────────────────────────────
//
// Axis::bounds([min, max]) reversed (min > max), NaN values, and NaN
// dataset coordinates can break chart's internal coordinate math.

#[tokio::test]
async fn chart_axis_bounds_reversed_does_not_panic() -> Result<()> {
    let mut h = TuiTestHarness::new(
        r#"
use tui;
use tui::chart;
let pts: Array<(f64, f64)> = [(0.0, 0.0), (1.0, 1.0)];
let ds = dataset(&pts);
let result = chart(
    #x_axis: &axis({min: 10.0, max: 0.0}),
    #y_axis: &axis({min: 0.0, max: 10.0}),
    &[ds]
)
"#,
    )
    .await?;
    h.render()?;
    Ok(())
}

#[tokio::test]
async fn chart_axis_bounds_nan_does_not_panic() -> Result<()> {
    let mut h = TuiTestHarness::new(
        r#"
use tui;
use tui::chart;
let pts: Array<(f64, f64)> = [(0.0, 0.0), (1.0, 1.0)];
let ds = dataset(&pts);
let nan = 0.0 / 0.0;
let result = chart(
    #x_axis: &axis({min: nan, max: 10.0}),
    #y_axis: &axis({min: 0.0, max: 10.0}),
    &[ds]
)
"#,
    )
    .await?;
    h.render()?;
    Ok(())
}

#[tokio::test]
async fn chart_dataset_nan_point_does_not_panic() -> Result<()> {
    let mut h = TuiTestHarness::new(
        r#"
use tui;
use tui::chart;
let nan = 0.0 / 0.0;
let pts: Array<(f64, f64)> = [(0.0, 0.0), (nan, nan), (1.0, 1.0)];
let ds = dataset(&pts);
let result = chart(
    #x_axis: &axis({min: 0.0, max: 1.0}),
    #y_axis: &axis({min: 0.0, max: 1.0}),
    &[ds]
)
"#,
    )
    .await?;
    h.render()?;
    Ok(())
}

#[tokio::test]
async fn chart_axis_bounds_equal_does_not_panic() -> Result<()> {
    // min == max is a degenerate axis — internal divide-by-zero
    // candidate when computing point positions.
    let mut h = TuiTestHarness::new(
        r#"
use tui;
use tui::chart;
let pts: Array<(f64, f64)> = [(0.0, 0.0), (1.0, 1.0)];
let ds = dataset(&pts);
let result = chart(
    #x_axis: &axis({min: 5.0, max: 5.0}),
    #y_axis: &axis({min: 5.0, max: 5.0}),
    &[ds]
)
"#,
    )
    .await?;
    h.render()?;
    Ok(())
}

// ── canvas ───────────────────────────────────────────────────────────

#[tokio::test]
async fn canvas_bounds_reversed_does_not_panic() -> Result<()> {
    let mut h = TuiTestHarness::new(
        r#"
use tui;
use tui::canvas;
let l = `Line({color: `Red, x1: 0.0, y1: 0.0, x2: 1.0, y2: 1.0});
let result = canvas(
    #x_bounds: &{min: 10.0, max: 0.0},
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
async fn canvas_bounds_equal_does_not_panic() -> Result<()> {
    let mut h = TuiTestHarness::new(
        r#"
use tui;
use tui::canvas;
let l = `Line({color: `Red, x1: 0.0, y1: 0.0, x2: 1.0, y2: 1.0});
let result = canvas(
    #x_bounds: &{min: 5.0, max: 5.0},
    #y_bounds: &{min: 5.0, max: 5.0},
    &[&l]
)
"#,
    )
    .await?;
    h.render()?;
    Ok(())
}

#[tokio::test]
async fn canvas_bounds_nan_does_not_panic() -> Result<()> {
    let mut h = TuiTestHarness::new(
        r#"
use tui;
use tui::canvas;
let l = `Line({color: `Red, x1: 0.0, y1: 0.0, x2: 1.0, y2: 1.0});
let nan = 0.0 / 0.0;
let result = canvas(
    #x_bounds: &{min: nan, max: 10.0},
    #y_bounds: &{min: 0.0, max: 10.0},
    &[&l]
)
"#,
    )
    .await?;
    h.render()?;
    Ok(())
}

// ── paragraph ────────────────────────────────────────────────────────

#[tokio::test]
async fn paragraph_scroll_negative_does_not_panic() -> Result<()> {
    let mut h = TuiTestHarness::new(
        r#"
use tui;
use tui::paragraph;
let result = paragraph(#scroll: &{x: -5, y: -10}, &"body")
"#,
    )
    .await?;
    h.render()?;
    Ok(())
}

#[tokio::test]
async fn paragraph_scroll_huge_does_not_panic() -> Result<()> {
    let mut h = TuiTestHarness::new(
        r#"
use tui;
use tui::paragraph;
let result = paragraph(#scroll: &{x: 99999, y: 99999}, &"body")
"#,
    )
    .await?;
    h.render()?;
    Ok(())
}

// ── calendar ─────────────────────────────────────────────────────────
//
// `date(year, month, day)` accepts arbitrary i64s. Invalid combos
// (month=13, day=31 in February, etc.) might panic in the time crate.

#[tokio::test]
async fn calendar_invalid_month_does_not_panic() -> Result<()> {
    let mut h = TuiTestHarness::with_viewport(
        "use tui;\nuse tui::calendar;\nlet d = date(2024, 13, 1);\nlet result = calendar(&d)",
        24,
        10,
    )
    .await?;
    h.render()?;
    Ok(())
}

#[tokio::test]
async fn calendar_invalid_day_does_not_panic() -> Result<()> {
    let mut h = TuiTestHarness::with_viewport(
        "use tui;\nuse tui::calendar;\nlet d = date(2024, 2, 31);\nlet result = calendar(&d)",
        24,
        10,
    )
    .await?;
    h.render()?;
    Ok(())
}

#[tokio::test]
async fn calendar_negative_year_does_not_panic() -> Result<()> {
    let mut h = TuiTestHarness::with_viewport(
        "use tui;\nuse tui::calendar;\nlet d = date(-1, 1, 1);\nlet result = calendar(&d)",
        24,
        10,
    )
    .await?;
    h.render()?;
    Ok(())
}
