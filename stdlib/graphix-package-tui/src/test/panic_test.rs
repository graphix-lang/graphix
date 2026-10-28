//! Panic-surface regression tests: each drives a widget with an input
//! that, unclamped, would trigger an assert in ratatui or a downstream
//! cast, and pins what the clamped input draws.

use crate::testing::TuiTestHarness;
use anyhow::Result;
use compact_str::format_compact;

// gauge / line_gauge: a ratio outside [0, 1] must be clamped
// (ratatui's Gauge::ratio / LineGauge::ratio assert).

#[tokio::test]
async fn gauge_ratio_above_one_does_not_panic() -> Result<()> {
    let mut h = TuiTestHarness::new(
        "use tui::*;\nuse tui::gauge::{self, *};\nlet result = gauge(&5.0)",
    )
    .await?;
    h.assert_lines(&[
        "████████████████████████████████████████",
        "████████████████████████████████████████",
        "████████████████████████████████████████",
        "████████████████████████████████████████",
        "████████████████████████████████████████",
        "██████████████████100% █████████████████",
        "████████████████████████████████████████",
        "████████████████████████████████████████",
        "████████████████████████████████████████",
        "████████████████████████████████████████",
    ])?;
    Ok(())
}

#[tokio::test]
async fn gauge_ratio_negative_does_not_panic() -> Result<()> {
    let mut h = TuiTestHarness::new(
        "use tui::*;\nuse tui::gauge::{self, *};\nlet result = gauge(&-0.3)",
    )
    .await?;
    h.assert_lines(&["", "", "", "", "", "                   0%"])?;
    Ok(())
}

#[tokio::test]
async fn gauge_ratio_nan_does_not_panic() -> Result<()> {
    let mut h = TuiTestHarness::new(
        "use tui::*;\nuse tui::gauge::{self, *};\nlet result = gauge(&(0.0 / 0.0))",
    )
    .await?;
    h.assert_lines(&["", "", "", "", "", "                   0%"])?;
    Ok(())
}

#[tokio::test]
async fn line_gauge_ratio_negative_does_not_panic() -> Result<()> {
    let mut h = TuiTestHarness::new(
        "use tui::*;\nuse tui::line_gauge::{self, *};\nlet result = line_gauge(&-0.5)",
    )
    .await?;
    h.assert_lines(&["  0% ───────────────────────────────────"])?;
    Ok(())
}

#[tokio::test]
async fn line_gauge_ratio_above_one_does_not_panic() -> Result<()> {
    let mut h = TuiTestHarness::new(
        "use tui::*;\nuse tui::line_gauge::{self, *};\nlet result = line_gauge(&2.5)",
    )
    .await?;
    h.assert_lines(&["100% ───────────────────────────────────"])?;
    Ok(())
}

// barchart: bar_width / bar_gap / group_gap (u16) and max (u64) are
// cast from i64; a negative must clamp, not wrap.

#[tokio::test]
async fn barchart_bar_width_negative_does_not_panic() -> Result<()> {
    let mut h = TuiTestHarness::new(
        r#"
use tui::*;
use tui::barchart::{self, *};
let b = bar(#label: &line("X"), &10);
let result = bar_chart(#bar_width: &-1, &[bar_group(#label: line("G"), [b])])
"#,
    )
    .await?;
    h.assert_lines(&[])?;
    Ok(())
}

#[tokio::test]
async fn barchart_bar_gap_huge_does_not_panic() -> Result<()> {
    let mut h = TuiTestHarness::new(
        r#"
use tui::*;
use tui::barchart::{self, *};
let b = bar(#label: &line("X"), &10);
let result = bar_chart(#bar_gap: &999999, &[bar_group(#label: line("G"), [b])])
"#,
    )
    .await?;
    h.assert_lines(&["█", "█", "█", "█", "█", "█", "█", "█", "X", "G"])?;
    Ok(())
}

#[tokio::test]
async fn barchart_max_zero_does_not_panic() -> Result<()> {
    let mut h = TuiTestHarness::new(
        r#"
use tui::*;
use tui::barchart::{self, *};
let b = bar(#label: &line("X"), &10);
let result = bar_chart(#max: &0, &[bar_group(#label: line("G"), [b])])
"#,
    )
    .await?;
    h.assert_lines(&["█", "█", "█", "█", "█", "█", "█", "█", "X", "G"])?;
    Ok(())
}

#[tokio::test]
async fn barchart_max_negative_does_not_panic() -> Result<()> {
    let mut h = TuiTestHarness::new(
        r#"
use tui::*;
use tui::barchart::{self, *};
let b = bar(#label: &line("X"), &10);
let result = bar_chart(#max: &-50, &[bar_group(#label: line("G"), [b])])
"#,
    )
    .await?;
    h.assert_lines(&["█", "█", "█", "█", "█", "█", "█", "█", "X", "G"])?;
    Ok(())
}

#[tokio::test]
async fn barchart_negative_bar_value_does_not_panic() -> Result<()> {
    let mut h = TuiTestHarness::new(
        r#"
use tui::*;
use tui::barchart::{self, *};
let b = bar(#label: &line("X"), &-10);
let result = bar_chart(&[bar_group(#label: line("G"), [b])])
"#,
    )
    .await?;
    h.assert_lines(&["", "", "", "", "", "", "", "", "X", "G"])?;
    Ok(())
}

#[tokio::test]
async fn barchart_horizontal_wide_text_value_does_not_panic() -> Result<()> {
    let mut h = TuiTestHarness::with_viewport(
        r#"
use tui::barchart::{bar, bar_chart, bar_group};
let b = bar(#text_value: &"éé", &10);
let result = bar_chart(#direction: &`Horizontal, &[bar_group([b])])
"#,
        3,
        1,
    )
    .await?;
    h.assert_lines(&["é██"])?;
    Ok(())
}

#[tokio::test]
async fn sparkline_max_zero_does_not_panic() -> Result<()> {
    let mut h = TuiTestHarness::new(
        r#"
use tui::*;
use tui::sparkline::{self, *};
let data = [1.0, 2.0, 3.0];
let result = sparkline(#max: &0.0, &data)
"#,
    )
    .await?;
    h.assert_lines(&[
        "  █",
        "  █",
        "  █",
        " ▅█",
        " ██",
        " ██",
        "▂██",
        "███",
        "███",
        "███",
    ])?;
    Ok(())
}

#[tokio::test]
async fn sparkline_max_negative_does_not_panic() -> Result<()> {
    let mut h = TuiTestHarness::new(
        r#"
use tui::*;
use tui::sparkline::{self, *};
let data = [1.0, 2.0, 3.0];
let result = sparkline(#max: &-100.0, &data)
"#,
    )
    .await?;
    h.assert_lines(&[
        "  █",
        "  █",
        "  █",
        " ▅█",
        " ██",
        " ██",
        "▂██",
        "███",
        "███",
        "███",
    ])?;
    Ok(())
}

#[tokio::test]
async fn scrollbar_counts_keep_the_thumb_on_track() -> Result<()> {
    use compact_str::format_compact;
    for (length, position, viewport, expected) in [
        (10, -5, 1, "^#.....v"),
        (-10, 0, 1, "        "),
        (i64::MAX, i64::MAX, 1, "^.....#v"),
        (10, 0, i64::MAX, "^######v"),
        (i64::MAX, i64::MAX, i64::MAX, "^...###v"),
    ] {
        let source = format_compact!(
            r##"
use tui::scrollbar::scrollbar;
use tui::text::text;
let result = scrollbar(
  #position: &{position}, #content_length: &{length}, #viewport_length: &{viewport},
  #begin_symbol: &"^", #end_symbol: &"v", #thumb_symbol: &"#", #track_symbol: &".",
  &text(&""))
"##
        );
        let mut h = TuiTestHarness::with_viewport(&source, 1, 8).await?;
        let buffer = h.render()?;
        for y in 0..expected.len() {
            assert_eq!(
                buffer[(0, y as u16)].symbol(),
                &expected[y..y + 1],
                "length={length}, position={position}, viewport={viewport}, row={y}"
            );
        }
    }
    Ok(())
}

#[tokio::test]
async fn tabs_selected_negative_does_not_panic() -> Result<()> {
    let mut h = TuiTestHarness::new(
        r#"
use tui::*;
use tui::tabs::{self, *};
use tui::paragraph::{self, *};
let one = paragraph(&"one");
let result = tabs(#selected: &-3, &[(line("A"), one)])
"#,
    )
    .await?;
    h.assert_lines(&[" A", "one"])?;
    Ok(())
}

#[tokio::test]
async fn tabs_selected_out_of_range_does_not_panic() -> Result<()> {
    let mut h = TuiTestHarness::new(
        r#"
use tui::*;
use tui::tabs::{self, *};
use tui::paragraph::{self, *};
let one = paragraph(&"one");
let result = tabs(#selected: &99, &[(line("A"), one)])
"#,
    )
    .await?;
    h.assert_lines(&[" A", "one"])?;
    Ok(())
}

#[tokio::test]
async fn list_selected_negative_does_not_panic() -> Result<()> {
    let mut h = TuiTestHarness::new(
        r#"
use tui::*;
use tui::list::{self, *};
let items = [line("A"), line("B")];
let result = list(#highlight_symbol: &">>", #selected: &-2, &items)
"#,
    )
    .await?;
    h.assert_lines(&[">>A", "  B"])?;
    Ok(())
}

#[tokio::test]
async fn list_selected_out_of_range_does_not_panic() -> Result<()> {
    let mut h = TuiTestHarness::new(
        r#"
use tui::*;
use tui::list::{self, *};
let items = [line("A"), line("B")];
let result = list(#highlight_symbol: &">>", #selected: &99, &items)
"#,
    )
    .await?;
    h.assert_lines(&["  A", ">>B"])?;
    Ok(())
}

#[tokio::test]
async fn list_scroll_negative_does_not_panic() -> Result<()> {
    let mut h = TuiTestHarness::new(
        r#"
use tui::*;
use tui::list::{self, *};
let items = [line("A"), line("B")];
let result = list(#scroll: &-5, &items)
"#,
    )
    .await?;
    h.assert_lines(&["A", "B"])?;
    Ok(())
}

#[tokio::test]
async fn table_selected_negative_does_not_panic() -> Result<()> {
    let mut h = TuiTestHarness::new(
        r#"
use tui::*;
use tui::table::{self, *};
let r1 = row([cell(line("a"))]);
let result = table(#highlight_symbol: &">>", #selected: &-1, &[&r1])
"#,
    )
    .await?;
    h.assert_lines(&[">>a"])?;
    Ok(())
}

#[tokio::test]
async fn table_selected_out_of_range_does_not_panic() -> Result<()> {
    let mut h = TuiTestHarness::new(
        r#"
use tui::*;
use tui::table::{self, *};
let r1 = row([cell(line("a"))]);
let result = table(#highlight_symbol: &">>", #selected: &50, &[&r1])
"#,
    )
    .await?;
    h.assert_lines(&[">>a"])?;
    Ok(())
}

// chart: reversed bounds and NaN values must not break the
// coordinate math.

#[tokio::test]
async fn chart_axis_bounds_reversed_does_not_panic() -> Result<()> {
    let mut h = TuiTestHarness::new(
        r#"
use tui::*;
use tui::chart::{self, *};
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
    h.assert_lines(&[])?;
    Ok(())
}

#[tokio::test]
async fn chart_axis_bounds_nan_does_not_panic() -> Result<()> {
    let mut h = TuiTestHarness::new(
        r#"
use tui::*;
use tui::chart::{self, *};
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
    h.assert_lines(&[])?;
    Ok(())
}

#[tokio::test]
async fn chart_dataset_nan_point_does_not_panic() -> Result<()> {
    let mut h = TuiTestHarness::new(
        r#"
use tui::*;
use tui::chart::{self, *};
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
    h.assert_lines(&[
        "                                       •",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "•",
    ])?;
    Ok(())
}

#[tokio::test]
async fn chart_axis_bounds_equal_does_not_panic() -> Result<()> {
    let mut h = TuiTestHarness::new(
        r#"
use tui::*;
use tui::chart::{self, *};
let pts: Array<(f64, f64)> = [(5.0, 5.0)];
let ds = dataset(&pts);
let result = chart(
    #x_axis: &axis({min: 5.0, max: 5.0}),
    #y_axis: &axis({min: 5.0, max: 5.0}),
    &[ds]
)
"#,
    )
    .await?;
    h.assert_lines(&[])?;
    Ok(())
}

#[tokio::test]
async fn canvas_bounds_reversed_does_not_panic() -> Result<()> {
    let mut h = TuiTestHarness::new(
        r#"
use tui::*;
use tui::canvas::{self, *};
let l = `Line({color: `Red, x1: 0.0, y1: 0.0, x2: 1.0, y2: 1.0});
let result = canvas(
    #x_bounds: &{min: 10.0, max: 0.0},
    #y_bounds: &{min: 0.0, max: 10.0},
    &[&l]
)
"#,
    )
    .await?;
    h.assert_lines(&[])?;
    Ok(())
}

#[tokio::test]
async fn canvas_bounds_equal_does_not_panic() -> Result<()> {
    let mut h = TuiTestHarness::new(
        r#"
use tui::*;
use tui::canvas::{self, *};
let l = `Line({color: `Red, x1: 5.0, y1: 5.0, x2: 5.0, y2: 5.0});
let result = canvas(
    #x_bounds: &{min: 5.0, max: 5.0},
    #y_bounds: &{min: 5.0, max: 5.0},
    &[&l]
)
"#,
    )
    .await?;
    h.assert_lines(&[])?;
    Ok(())
}

#[tokio::test]
async fn canvas_bounds_nan_does_not_panic() -> Result<()> {
    let mut h = TuiTestHarness::new(
        r#"
use tui::*;
use tui::canvas::{self, *};
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
    h.assert_lines(&[])?;
    Ok(())
}

#[tokio::test]
async fn paragraph_scroll_negative_does_not_panic() -> Result<()> {
    let mut h = TuiTestHarness::new(
        r#"
use tui::*;
use tui::paragraph::{self, *};
let result = paragraph(#scroll: &{x: -5, y: -10}, &"body")
"#,
    )
    .await?;
    h.assert_lines(&["body"])?;
    Ok(())
}

#[tokio::test]
async fn paragraph_scroll_huge_does_not_panic() -> Result<()> {
    let mut h = TuiTestHarness::new(
        r#"
use tui::*;
use tui::paragraph::{self, *};
let result = paragraph(#scroll: &{x: 99999, y: 99999}, &"body")
"#,
    )
    .await?;
    h.assert_lines(&[])?;
    Ok(())
}

// calendar: `date(year, month, day)` accepts arbitrary i64s; invalid
// combinations must clamp rather than panic in `time`.

#[tokio::test]
async fn calendar_invalid_month_does_not_panic() -> Result<()> {
    let mut h = TuiTestHarness::with_viewport(
        "use tui::*;\nuse tui::calendar::{self, *};\nlet d = date(2024, 13, 1);\nlet result = calendar(&d)",
        24,
        10,
    )
    .await?;
    h.assert_lines(&[
        "  1  2  3  4  5  6  7",
        "  8  9 10 11 12 13 14",
        " 15 16 17 18 19 20 21",
        " 22 23 24 25 26 27 28",
        " 29 30 31",
    ])?;
    Ok(())
}

#[tokio::test]
async fn calendar_invalid_day_does_not_panic() -> Result<()> {
    let mut h = TuiTestHarness::with_viewport(
        "use tui::*;\nuse tui::calendar::{self, *};\nlet d = date(2024, 2, 31);\nlet result = calendar(&d)",
        24,
        10,
    )
    .await?;
    h.assert_lines(&[
        "              1  2  3",
        "  4  5  6  7  8  9 10",
        " 11 12 13 14 15 16 17",
        " 18 19 20 21 22 23 24",
        " 25 26 27 28 29",
    ])?;
    Ok(())
}

#[tokio::test]
async fn calendar_negative_year_does_not_panic() -> Result<()> {
    let mut h = TuiTestHarness::with_viewport(
        "use tui::*;\nuse tui::calendar::{self, *};\nlet d = date(-1, 1, 1);\nlet result = calendar(&d)",
        24,
        10,
    )
    .await?;
    h.assert_lines(&[
        "                 1  2",
        "  3  4  5  6  7  8  9",
        " 10 11 12 13 14 15 16",
        " 17 18 19 20 21 22 23",
        " 24 25 26 27 28 29 30",
        " 31",
    ])?;
    Ok(())
}

async fn canvas_line_lines(coords: &str) -> Result<Vec<String>> {
    let code = format_compact!(
        r#"
use tui::canvas::canvas;
let shape = `Line({{color: `Red, {coords}}});
let result = canvas(#marker: &`Block, #x_bounds: &{{min: 0.0, max: 10.0}},
    #y_bounds: &{{min: 0.0, max: 10.0}}, &[&shape])
"#
    );
    TuiTestHarness::new(&code).await?.render_lines()
}

#[tokio::test]
async fn overflowing_canvas_lines_are_clipped() -> Result<()> {
    for (huge, clipped) in [
        (
            "x1: -1e308, y1: -1e308, x2: 1e308, y2: 1e308",
            "x1: 0.0, y1: 0.0, x2: 10.0, y2: 10.0",
        ),
        (
            "x1: -1e300, y1: 1e300, x2: 5.0, y2: 5.0",
            "x1: 0.0, y1: 10.0, x2: 5.0, y2: 5.0",
        ),
        (
            "x1: -1e308, y1: 5.0, x2: 1e308, y2: 5.0",
            "x1: 0.0, y1: 5.0, x2: 10.0, y2: 5.0",
        ),
    ] {
        let expected = canvas_line_lines(clipped).await?;
        assert!(expected.iter().any(|l| !l.trim().is_empty()));
        assert_eq!(canvas_line_lines(huge).await?, expected, "{huge}");
    }
    Ok(())
}

#[tokio::test]
async fn overflowing_chart_lines_are_clipped() -> Result<()> {
    for (huge, clipped) in [
        ("(-1e308, -1e308), (1e308, 1e308)", "(0.0, 0.0), (10.0, 10.0)"),
        ("(-1e300, 1e300), (5.0, 5.0)", "(0.0, 10.0), (5.0, 5.0)"),
        ("(-1e308, 5.0), (1e308, 5.0)", "(0.0, 5.0), (10.0, 5.0)"),
    ] {
        let expected = line_chart_lines(clipped).await?;
        assert!(expected.iter().any(|l| !l.trim().is_empty()));
        assert_eq!(line_chart_lines(huge).await?, expected, "{huge}");
    }
    Ok(())
}

/// The rows a line chart over 0..10 draws for `data`.
async fn line_chart_lines(data: &str) -> Result<Vec<String>> {
    let code = format_compact!(
        r#"
use tui::chart::{{axis, chart, dataset}};
let result = chart(#x_axis: &axis({{min: 0.0, max: 10.0}}),
    #y_axis: &axis({{min: 0.0, max: 10.0}}),
    &[dataset(#graph_type: &`Line, &[{data}])])
"#
    );
    TuiTestHarness::new(&code).await?.render_lines()
}

#[tokio::test]
async fn an_overflowing_pair_keeps_the_finite_segments() -> Result<()> {
    let drawn = line_chart_lines(
        "(1.0, 1.0), (4.0, 4.0), (-1e308, -1e308), (1e308, 1e308), (6.0, 6.0), (9.0, 9.0)",
    )
    .await?;
    for segment in ["(1.0, 1.0), (4.0, 4.0)", "(6.0, 6.0), (9.0, 9.0)"] {
        let alone = line_chart_lines(segment).await?;
        assert!(alone.iter().any(|l| !l.trim().is_empty()), "{segment}: {alone:?}");
        for (row, (a, d)) in alone.iter().zip(&drawn).enumerate() {
            for (col, (a, d)) in a.chars().zip(d.chars()).enumerate() {
                assert!(a == ' ' || a == d, "{segment} at ({col}, {row}): {drawn:?}");
            }
        }
    }
    Ok(())
}

/// Text as wide as ratatui's u16 coordinates draws cut to the screen: an
/// axis title, a legend name and a horizontal bar label.
#[tokio::test]
async fn text_wider_than_u16_draws_cut() -> Result<()> {
    let (t, n) = ("a".repeat(65535), "a".repeat(65534));
    for (code, row) in [
        (
            format!(
                r#"
use tui::{{line, chart::{{axis, chart, dataset}}}};
let result = chart(
    #x_axis: &axis({{min: 0.0, max: 10.0}}),
    #y_axis: &axis(#title: line("{t}"), {{min: 0.0, max: 10.0}}),
    &[dataset(&[(1.0, 1.0)])]
)
"#
            ),
            0,
        ),
        (
            format!(
                r#"
use tui::{{line, chart::{{axis, chart, dataset}}}};
let result = chart(
    #legend_position: &`TopRight,
    #hidden_legend_constraints: &{{width: `Percentage(100), height: `Percentage(100)}},
    #x_axis: &axis({{min: 0.0, max: 10.0}}),
    #y_axis: &axis({{min: 0.0, max: 10.0}}),
    &[dataset(#name: &line("{n}"), &[(1.0, 1.0)])]
)
"#
            ),
            1,
        ),
        (
            format!(
                r#"
use tui::{{line, barchart::{{bar, bar_chart, bar_group}}}};
let result = bar_chart(#direction: &`Horizontal, &[bar_group([bar(#label: &line("{t}"), &3)])])
"#
            ),
            0,
        ),
    ] {
        let mut h = TuiTestHarness::with_viewport(&code, 40, 10).await?;
        let lines = h.render_lines()?;
        assert!(lines[row].contains("aaaaaaaaaa"), "{lines:?}");
    }
    Ok(())
}

#[tokio::test]
async fn chart_samples_labels_to_fit() -> Result<()> {
    for height in [50, 400] {
        let mut h = TuiTestHarness::with_viewport(
            r#"
use tui::{line, chart::{axis, chart}};
let labels = array::init(2000, |i| line("[i]"));
let result = chart(#y_axis: &axis(#labels: labels, {min: 0.0, max: 1.0}), &[])
"#,
            40,
            height,
        )
        .await?;
        let lines = h.render_lines()?;
        assert!(lines.first().unwrap().starts_with("1999"));
        assert!(lines.last().unwrap().starts_with("0"), "{:?}", lines.last());
    }
    Ok(())
}

#[tokio::test]
async fn a_deep_widget_tree_compiles_and_draws() -> Result<()> {
    let mut h = TuiTestHarness::with_viewport(
        r#"
use tui::{block::block, paragraph::paragraph};
let result = array::fold(array::init(1000, |i| i), paragraph(&"x"), |acc, i| block(&acc))
"#,
        4,
        2,
    )
    .await?;
    h.assert_lines(&["x"])?;
    Ok(())
}
