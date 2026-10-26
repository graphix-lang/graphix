use super::GuiTestHarness;
use anyhow::Result;
use compact_str::format_compact;

use crate::{
    theme::GraphixTheme,
    widgets::chart::{ChartMode, ChartState, ChartW, PlotInfo},
};
use graphix_rt::NoExt;
use iced_core::{Point, Rectangle, Size, mouse};
use iced_widget::canvas::Program;
use netidx::{protocol::valarray::ValArray, publisher::Value};

fn chart(h: &GuiTestHarness) -> &ChartW<NoExt> {
    h.root.w.as_any().downcast_ref::<ChartW<NoExt>>().expect("a chart")
}

/// Draw the chart through its program into `state` at `size`, returning
/// the plot area the draw recorded.
async fn draw_in(h: &GuiTestHarness, state: &ChartState, size: Size) -> Option<PlotInfo> {
    let frame = h.frame_state();
    let theme = GraphixTheme { inner: iced_core::Theme::Dark, overrides: None };
    let bounds = Rectangle::new(Point::ORIGIN, size);
    let _ = Program::draw(
        chart(h),
        state,
        &frame.renderer,
        &theme,
        bounds,
        mouse::Cursor::Unavailable,
    );
    chart(h).plot_info()
}

async fn draw(h: &GuiTestHarness) -> Option<PlotInfo> {
    draw_in(h, &ChartState::default(), Size::new(400.0, 300.0)).await
}

fn color_pixels(rgba: &[u8], color: [u8; 3]) -> usize {
    rgba.chunks_exact(4).filter(|px| px[..3] == color).count()
}

async fn chart_harness(args: &str) -> Result<GuiTestHarness> {
    let code = format!(
        "use gui::*;\nuse gui::chart::{{self, *}};\n\
         let day1 = [(datetime:\"2024-01-01T00:00:00Z\", 1.0), (datetime:\"2024-01-02T00:00:00Z\", 2.0)];\n\
         let result = chart({args})"
    );
    GuiTestHarness::new(&code).await
}

#[tokio::test(flavor = "current_thread")]
async fn axis_range_renders() -> Result<()> {
    let h = chart_harness(
        "#x_range: &{min: 0.0, max: 100.0}, \
         #y_range: &{min: -5.0, max: 50.0}, \
         #width: &`Fill, #height: &`Fixed(200.0), \
         &[chart::line(#label: \"test\", &[(0.0, 1.0)])]",
    )
    .await?;
    assert!(draw(&h).await.is_some());
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn empty_line_draws_nothing() -> Result<()> {
    let h = chart_harness(concat!(
        "#width: &`Fill, #height: &`Fixed(200.0), ",
        r#"&[chart::line(#label: "test", &[])]"#,
    ))
    .await?;
    assert_eq!(chart(&h).mode(), ChartMode::Empty);
    assert!(draw(&h).await.is_none());
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn empty_colored_scatter_draws_nothing() -> Result<()> {
    let h = chart_harness(
        "#width: &`Fill, #height: &`Fixed(200.0), \
         &[chart::scatter(#color: color(#r: 0.0, #g: 1.0, #b: 0.0, #a: 1.0)$, &[])]",
    )
    .await?;
    assert_eq!(chart(&h).mode(), ChartMode::Empty);
    assert!(draw(&h).await.is_none());
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn candlestick_renders() -> Result<()> {
    let h = chart_harness(
        "#width: &`Fill, #height: &`Fixed(200.0), \
         &[chart::candlestick(#label: \"OHLC\", \
           &[{x: 1.0, open: 10.0, high: 15.0, low: 8.0, close: 12.0}, \
             {x: 2.0, open: 12.0, high: 14.0, low: 9.0, close: 11.0}])]",
    )
    .await?;
    assert!(draw(&h).await.is_some());
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn error_bar_renders() -> Result<()> {
    let h = chart_harness(
        "#width: &`Fill, #height: &`Fixed(200.0), \
         &[chart::error_bar(#label: \"Confidence\", \
           &[{x: 1.0, min: 3.0, avg: 5.0, max: 7.0}, \
             {x: 2.0, min: 4.0, avg: 6.0, max: 8.0}])]",
    )
    .await?;
    assert!(draw(&h).await.is_some());
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
// CR claude for claude: [test-gap/low] This passes whether the line is dashed, solid or
// not drawn. draw_xy_chart records plot_info before any series is drawn, and a series
// error only reaches annotate's log. No other test looks at a dashed line's pixels.
// Fix: screenshot a magenta dashed_line(#dash: 10.0, #gap: 10.0) and a magenta solid
// line over the same points, as line_pixels does. Assert the dashed count is above zero
// and well below the solid count. Found by reading; not run. (tests-gui-b-03)
async fn dashed_line_renders() -> Result<()> {
    let h = chart_harness(
        "#width: &`Fill, #height: &`Fixed(200.0), \
         &[chart::dashed_line(#dash: 10.0, #gap: 5.0, \
           &[(0.0, 0.0), (5.0, 5.0), (10.0, 2.0)])]",
    )
    .await?;
    assert!(draw(&h).await.is_some());
    Ok(())
}

/// The pixels of a line in a color no other part of the chart uses.
async fn line_pixels(stroke: &str) -> Result<usize> {
    let mut h = chart_harness(&format_compact!(
        "#width: &`Fill, #height: &`Fixed(200.0), \
         &[chart::line(#color: color(#r: 1.0, #g: 0.0, #b: 1.0)$, \
           #stroke_width: {stroke}, &[(0.0, 0.0), (5.0, 5.0)])]"
    ))
    .await?;
    Ok(color_pixels(&h.screenshot(400, 200), [255, 0, 255]))
}

#[tokio::test(flavor = "current_thread")]
async fn series_style_stroke_width() -> Result<()> {
    let (thin, thick, half) =
        (line_pixels("1.0").await?, line_pixels("4.0").await?, line_pixels("0.5").await?);
    assert!(thick > 2 * thin, "stroke 4: {thick}, stroke 1: {thin}");
    assert!(half > 0, "a half-pixel line draws");
    Ok(())
}

/// How many pixels are of a hue whose `strong` channels each exceed every
/// other channel by far: a color nothing in a default chart draws,
/// through the antialiasing and blending a drawn color goes through.
fn pixels_of_hue(rgba: &[u8], strong: [bool; 3]) -> usize {
    rgba.chunks_exact(4)
        .filter(|px| {
            (0..3).filter(|&i| strong[i]).all(|i| {
                (0..3).filter(|&j| !strong[j]).all(|j| px[i] as i16 - px[j] as i16 > 100)
            })
        })
        .count()
}

const MAGENTA: [bool; 3] = [true, false, true];
const CYAN: [bool; 3] = [false, true, true];
const GREEN: [bool; 3] = [false, true, false];

#[tokio::test(flavor = "current_thread")]
async fn background_color() -> Result<()> {
    let mut h = chart_harness(
        "#width: &`Fill, #height: &`Fixed(200.0), \
         #style: &chart_style(#background: color(#r: 1.0, #g: 0.0, #b: 1.0, #a: 1.0)$), \
         &[chart::line(&[(0.0, 0.0), (5.0, 5.0)])]",
    )
    .await?;
    assert!(color_pixels(&h.screenshot(400, 200), [255, 0, 255]) > 40_000);
    assert!(draw(&h).await.is_some());
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn mesh_style() -> Result<()> {
    let mut h = chart_harness(
        "#width: &`Fill, #height: &`Fixed(200.0), \
         #style: &chart_style(#mesh: {show_x_grid: true, show_y_grid: false, \
                   grid_color: null, bold_line_color: color(#r: 0.0, #g: 1.0, #b: 0.0)$, \
                   axis_color: null, \
                   label_color: null, label_size: 12.0, \
                   x_label_area_size: null, x_labels: 5, x_light_lines: 0, \
                   y_label_area_size: null, y_labels: 5, y_light_lines: 2, \
                   z_labels: null, z_light_lines: null}), \
         &[chart::line(&[(0.0, 0.0), (5.0, 5.0)])]",
    )
    .await?;
    assert!(color_pixels(&h.screenshot(400, 200), [0, 255, 0]) > 100);
    assert!(draw(&h).await.is_some());
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn dark_background_label_colors() -> Result<()> {
    let h = chart_harness(
        "#width: &`Fill, #height: &`Fixed(200.0), \
         #title: &\"Dark Chart\", \
         #x_label: &\"X\", #y_label: &\"Y\", \
         #style: &{background: color(#r: 0.1, #g: 0.1, #b: 0.15, #a: 1.0)$, \
                   margin: null, title_size: null, \
                   title_color: color(#r: 0.9, #g: 0.9, #b: 0.9, #a: 1.0)$, \
                   palette: [color(#r: 1.0, #g: 0.5, #b: 0.0, #a: 1.0)$], \
                   legend_position: `LowerRight, legend: null, \
                   mesh: chart::mesh_style( \
                     #grid_color: color(#r: 0.3, #g: 0.3, #b: 0.35, #a: 1.0)$, \
                     #axis_color: color(#r: 0.5, #g: 0.5, #b: 0.55, #a: 1.0)$, \
                     #label_color: color(#r: 0.8, #g: 0.8, #b: 0.8, #a: 1.0)$, \
                     #label_size: 14.0)}, \
         &[chart::line(#label: \"Series\", &[(0.0, 0.0), (5.0, 5.0)])]",
    )
    .await?;
    assert!(draw(&h).await.is_some());
    let mut h = chart_harness(
        "#width: &`Fill, #height: &`Fixed(200.0), \
         #title: &\"Dark Chart\", #x_label: &\"X axis\", \
         #style: &chart_style(\
           #title_color: color(#r: 1.0, #g: 0.0, #b: 1.0)$, \
           #mesh: chart::mesh_style(#label_color: color(#r: 0.0, #g: 1.0, #b: 1.0)$)), \
         &[chart::line(&[(0.0, 0.0), (5.0, 5.0)])]",
    )
    .await?;
    let pixels = h.screenshot(400, 200);
    assert!(pixels_of_hue(&pixels, MAGENTA) > 10, "title color");
    assert!(pixels_of_hue(&pixels, CYAN) > 10, "label color");
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn legend_style() -> Result<()> {
    let h = chart_harness(
        "#width: &`Fill, #height: &`Fixed(200.0), \
         #style: &chart_style(#legend: { \
           background: color(#r: 0.2, #g: 0.2, #b: 0.25, #a: 1.0)$, \
           border: color(#r: 0.5, #g: 0.5, #b: 0.5, #a: 1.0)$, \
           label_color: color(#r: 0.9, #g: 0.9, #b: 0.9, #a: 1.0)$, \
           label_size: null}), \
         &[chart::line(#label: \"Test\", &[(0.0, 0.0), (5.0, 5.0)])]",
    )
    .await?;
    assert!(draw(&h).await.is_some());
    let mut h = chart_harness(
        "#width: &`Fill, #height: &`Fixed(200.0), \
         #style: &chart_style(#legend: { \
           background: color(#r: 0.0, #g: 1.0, #b: 0.0, #a: 1.0)$, \
           border: null, label_color: null, label_size: null}), \
         &[chart::line(#label: \"Test\", &[(0.0, 0.0), (5.0, 5.0)])]",
    )
    .await?;
    let pixels = h.screenshot(400, 200);
    assert!(pixels_of_hue(&pixels, GREEN) > 200, "legend background");
    Ok(())
}

/// A labelled series that draws nothing has no legend: no empty box.
#[tokio::test(flavor = "current_thread")]
async fn a_legend_counts_only_drawn_series() -> Result<()> {
    assert_same_series_pixels(
        r#"line(&[(1.0, 1.0), (2.0, 2.0)]), line(#label: "nothing yet", &[])"#,
        "line(&[(1.0, 1.0), (2.0, 2.0)])",
    )
    .await
}

/// A pie chart shows its title, and sits centred below it.
#[tokio::test(flavor = "current_thread")]
async fn a_pie_shows_its_title() -> Result<()> {
    let mut h = chart_harness(
        r#"#width: &`Fill, #height: &`Fixed(200.0), #title: &"Languages",
           #style: &chart_style(#title_color: color(#r: 1.0, #g: 0.0, #b: 1.0)$),
           &[pie(&[("a", 1.0), ("b", 2.0)])]"#,
    )
    .await?;
    let pixels = h.screenshot(400, 200);
    assert!(pixels_of_hue(&pixels, MAGENTA) > 10, "the title draws");
    let info = chart(&h).plot_info().expect("drawn");
    let below = 200.0 - info.rect.y - info.rect.height;
    assert!(info.rect.y > below, "{info:?}: centred under the title");
    Ok(())
}

/// An error bar draws only what is in view: none for a bar outside it,
/// and no cap at an end the view cuts.
#[tokio::test(flavor = "current_thread")]
async fn error_bars_draw_only_what_is_in_view() -> Result<()> {
    assert_same_series_pixels(
        "line(&[(1.0, 1.0)]), error_bar(&[{x: 2.0, min: 6.0, avg: 7.0, max: 8.0}])",
        "line(&[(1.0, 1.0)])",
    )
    .await?;
    let bar_pixels = async |max: &str| -> Result<usize> {
        let mut h = chart_harness(&format_compact!(
            "#width: &`Fill, #height: &`Fixed(200.0), \
             #x_range: &{{min: 0.0, max: 5.0}}, #y_range: &{{min: 0.0, max: 5.0}}, \
             &[error_bar(#color: color(#r: 1.0, #g: 0.0, #b: 1.0)$, #point_size: 30.0, \
               &[{{x: 2.0, min: 1.0, avg: 2.0, max: {max}}}])]"
        ))
        .await?;
        Ok(color_pixels(&h.screenshot(400, 200), [255, 0, 255]))
    };
    let (cut, capped) = (bar_pixels("9.0").await?, bar_pixels("4.99").await?);
    assert!(cut + 15 < capped, "a cut end draws no cap: {cut} vs {capped}");
    Ok(())
}

/// The tooltip is the point under the cursor in the plot as it is now,
/// not as it was at the cursor's last move.
#[tokio::test(flavor = "multi_thread")]
async fn the_tooltip_follows_new_data_under_a_resting_cursor() -> Result<()> {
    let mut h = GuiTestHarness::new(
        "use gui::*;\nuse gui::chart::{self, *};\n\
         let pts = [(5.0, 5.0)];\n\
         let result = chart(#x_range: &{min: 0.0, max: 10.0}, \
           #y_range: &{min: 0.0, max: 10.0}, &[line(&pts)])",
    )
    .await?;
    let mut state = ChartState::default();
    let size = Size::new(400.0, 300.0);
    let bounds = Rectangle::with_size(size);
    let p = draw_in(&h, &state, size).await.unwrap().rect.center();
    let at = mouse::Cursor::Available(p);
    let moved = iced_core::Event::Mouse(mouse::Event::CursorMoved { position: p });
    state.handle_event(chart(&h), &moved, bounds, at);
    assert_eq!(state.tooltip(chart(&h), bounds, at).unwrap().value, "(5.0000, 5.0000)");
    let pts =
        graphix_package_core::testing::find_bind_id(&h.f.compiled.env, "test::pts")?;
    let moved_pts = Value::Array(ValArray::from_iter([graphix_compiler::rep::tuple([
        Value::F64(5.0),
        Value::F64(5.1),
    ])]));
    h.f.gx.compile_ref(pts).await?.set(moved_pts)?;
    h.drain().await?;
    draw_in(&h, &state, size).await;
    assert_eq!(state.tooltip(chart(&h), bounds, at).unwrap().value, "(5.0000, 5.1000)");
    Ok(())
}

/// A wheel narrows a view of a second of a time series, and of a second
/// of an x in Unix seconds.
#[tokio::test(flavor = "current_thread")]
async fn a_view_zooms_to_under_a_second() -> Result<()> {
    use iced_core::{event::Event, mouse::Event as ME};
    for (data, x0, span) in [
        ("line(&day1)", 1_704_067_200_000.0, 1100.0),
        ("line(&[(1790000000.0, 1.0), (1790000002.0, 2.0)])", 1_790_000_000.0, 1.1),
    ] {
        let h = chart_harness(&format_compact!("&[{data}]")).await?;
        let mut state = ChartState::default();
        let size = Size::new(400.0, 300.0);
        let bounds = Rectangle::with_size(size);
        let p = draw_in(&h, &state, size).await.unwrap().rect.center();
        let at = mouse::Cursor::Available(p);
        state.handle_event(
            chart(&h),
            &Event::Mouse(ME::CursorMoved { position: p }),
            bounds,
            at,
        );
        state.x_view = Some((x0, x0 + span));
        draw_in(&h, &state, size).await;
        let wheel =
            ME::WheelScrolled { delta: mouse::ScrollDelta::Lines { x: 0.0, y: 1.0 } };
        state.handle_event(chart(&h), &Event::Mouse(wheel), bounds, at);
        let (a, b) = state.x_view.unwrap();
        assert!(b - a < span * 0.95, "{data}: {:?}", (a, b));
    }
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn shared_style_with_update() -> Result<()> {
    let code = "use gui::*;\nuse gui::chart::{self, *};\n\
         let deck = chart_style(#title_size: 20.0, #margin: 4.0);\n\
         let big = {deck with title_size: 40.0};\n\
         let result = gui::column::column(&[\
           chart(#title: &\"a\", #style: &deck, &[chart::line(&[(0.0, 0.0), (1.0, 1.0)])]),\
           chart(#title: &\"b\", #style: &big, &[chart::line(&[(0.0, 0.0), (1.0, 1.0)])])])";
    let mut h = GuiTestHarness::new(code).await?;
    h.render()?;
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn mesh_style_3d() -> Result<()> {
    let h = chart_harness(
        "#width: &`Fill, #height: &`Fixed(200.0), \
         #style: &chart_style(#mesh: mesh_style( \
           #grid_color: color(#r: 0.3, #g: 0.3, #b: 0.35, #a: 1.0)$, \
           #bold_line_color: color(#r: 0.6, #g: 0.6, #b: 0.6, #a: 1.0)$, \
           #x_labels: 4, #y_labels: 3, #z_labels: 2, \
           #x_light_lines: 0, #y_light_lines: 1, #z_light_lines: 2)), \
         &[chart::scatter3d(&[(0.0, 0.0, 0.0), (1.0, 2.0, 3.0)])]",
    )
    .await?;
    assert_eq!(chart(&h).mode(), ChartMode::ThreeD);
    let _ = draw(&h).await;
    Ok(())
}

#[test]
fn marker_size_rule() {
    use crate::widgets::chart::marker_size;
    assert_eq!(marker_size(Some(6.0), 4), 6);
    assert_eq!(marker_size(Some(0.0), 1), 0);
    assert_eq!(marker_size(None, 4), 0);
    assert_eq!(marker_size(None, 1), 3);
    assert_eq!(marker_size(Some(0.5), 4), 1);
    assert_eq!(marker_size(Some(2.7), 4), 3);
    assert_eq!(marker_size(Some(f64::NAN), 4), 0);
}

/// Every marker path draws through the program without erroring on a
/// one-point series, whose auto range is degenerate.
#[tokio::test(flavor = "current_thread")]
async fn markers_draw_on_every_series_kind() -> Result<()> {
    let h = chart_harness(
        r#"&[
            line(&[(2.0, 5.0)]),
            line(#point_size: 6.0, &[(1.0, 1.0), (2.0, 3.0), (3.0, 2.0)]),
            dashed_line(#point_size: 4.0, &[(1.0, 4.0), (2.0, 4.5), (3.0, 3.5)]),
            area(&[(3.5, 1.5)])
        ]"#,
    )
    .await?;
    assert!(draw(&h).await.is_some());
    Ok(())
}

/// The ranges the draw takes come from the datasets: x and y of a
/// numeric series, x in ms of a time series, and all three of a 3D one.
#[tokio::test(flavor = "current_thread")]
async fn ranges_cover_the_data() -> Result<()> {
    use crate::widgets::chart::{compute_3d_ranges, compute_ranges, datetime_ms};
    let h = chart_harness("&[line(&[(-3.0, 1.0), (7.0, 20.0)])]").await?;
    let ((x0, x1), (y0, y1)) = compute_ranges(chart(&h).datasets(), false);
    assert!(x0 < -3.0 && x1 > 7.0 && y0 < 1.0 && y1 > 20.0);
    let h = chart_harness(r#"&[line(&day1)]"#).await?;
    let ((x0, x1), _) = compute_ranges(chart(&h).datasets(), true);
    let day = |d: &str| datetime_ms(&d.parse().unwrap());
    assert!(x0 < day("2024-01-01T00:00:00Z") && x1 > day("2024-01-02T00:00:00Z"));
    let h = chart_harness("&[scatter3d(&[(0.0, 1.0, 2.0), (3.0, 4.0, 5.0)])]").await?;
    let ((x0, x1), (y0, y1), (z0, z1)) = compute_3d_ranges(chart(&h).datasets());
    assert!(x0 < 0.0 && x1 > 3.0 && y0 < 1.0 && y1 > 4.0 && z0 < 2.0 && z1 > 5.0);
    Ok(())
}

/// Infinite and NaN samples and ranges draw over finite ranges, without
/// hanging the tick loop or tripping plotters' NaN assert.
#[tokio::test(flavor = "current_thread")]
async fn non_finite_data_draws() -> Result<()> {
    let h = chart_harness(
        "#y_range: &{min: 0.0 / 0.0, max: 1.0}, \
         &[line(&[(0.0, 1.0), (1.0, 1.0 / 0.0), (2.0, 0.0 / 0.0), (3.0, 2.0)])]",
    )
    .await?;
    let info = draw(&h).await.expect("drawn");
    let finite = |r: (f64, f64)| r.0.is_finite() && r.1.is_finite() && r.0 < r.1;
    assert!(finite(info.x_range) && finite(info.y_range), "{info:?}");
    let h = chart_harness("&[bar(&[(\"a\", 1.0 / 0.0), (\"b\", 2.0)])]").await?;
    assert!(draw(&h).await.is_some_and(|i| finite(i.y_range)));
    Ok(())
}

/// Points far outside a narrow view are cut away in data space instead
/// of mapped to pixels, where they overflowed plotters' arithmetic.
#[tokio::test(flavor = "current_thread")]
async fn far_points_outside_the_view_draw() -> Result<()> {
    let h = chart_harness(
        "#y_range: &{min: 0.0, max: 1.0}, &[\
         line(&[(0.0, 0.5), (1.0, -1e7), (2.0, 0.5)]), \
         area(&[(0.0, 0.5), (1.0, 1e12), (2.0, 0.5)]), \
         scatter(&[(1.0, -1e300)]), \
         error_bar(&[{x: 1.0, min: -1e300, avg: 0.5, max: 1e300}])]",
    )
    .await?;
    assert_eq!(draw(&h).await.map(|i| i.y_range), Some((0.0, 1.0)));
    Ok(())
}

/// A degenerate pie draws the usable slices without looping.
#[tokio::test(flavor = "current_thread")]
async fn degenerate_pies_draw_what_they_can() -> Result<()> {
    let h = chart_harness(r#"&[pie(&[("in", 100.0), ("out", -100.0)])]"#).await?;
    assert!(draw(&h).await.is_some());
    let h =
        chart_harness(r#"&[pie(#start_angle: 1.0 / 0.0, &[("a", 1.0), ("b", 2.0)])]"#)
            .await?;
    assert!(draw(&h).await.is_some());
    let h = chart_harness(r#"&[pie(&[("a", 0.0), ("b", 0.0)])]"#).await?;
    assert!(draw(&h).await.is_none());
    let h = chart_harness(r#"#title: &"pie", &[pie(&[("a", 1.0)])]"#).await?;
    let _ = draw_in(&h, &ChartState::default(), Size::new(400.0, 20.0)).await;
    Ok(())
}

/// Mesh counts of zero or less draw on every axis kind.
#[tokio::test(flavor = "current_thread")]
async fn zero_and_negative_mesh_counts_draw() -> Result<()> {
    let style = "#style: &chart_style(#mesh: mesh_style(\
                 #x_labels: 0, #x_light_lines: 0, #y_labels: -3, #y_light_lines: -1, \
                 #z_labels: -2, #z_light_lines: 0))";
    for data in
        [r#"line(&day1)"#, r#"bar(&[("only", 1.0)])"#, "line(&[(0.0, 1.0), (1.0, 2.0)])"]
    {
        let h = chart_harness(&format!("{style}, &[{data}]")).await?;
        assert!(draw(&h).await.is_some(), "{data}");
    }
    let h = chart_harness(&format!(
        "{style}, &[scatter3d(&[(0.0, 0.0, 0.0), (1.0, 1.0, 1.0)])]"
    ))
    .await?;
    let _ = draw(&h).await;
    Ok(())
}

/// A time series at the end of chrono's range pads inside it.
#[tokio::test(flavor = "current_thread")]
async fn a_time_series_at_the_end_of_time_draws() -> Result<()> {
    let h = GuiTestHarness::new(
        r#"use gui::*; use gui::chart::{self, *};
let d = [(datetime:"2026-01-01T00:00:00Z", 1.0), (datetime:"+262142-12-31T23:59:59Z", 2.0)];
let result = chart(&[line(&d)])"#,
    )
    .await?;
    assert!(draw(&h).await.is_some());
    Ok(())
}

/// A datetime `x_range` sets a time series' x range.
#[tokio::test(flavor = "current_thread")]
async fn a_datetime_x_range_is_honoured() -> Result<()> {
    use crate::widgets::chart::datetime_ms;
    let h = GuiTestHarness::new(
        r#"use gui::*; use gui::chart::{self, *};
let d = [(datetime:"2024-01-01T00:00:00Z", 1.0), (datetime:"2024-01-09T00:00:00Z", 2.0)];
let result = chart(
    #x_range: &{min: datetime:"2024-01-05T00:00:00Z", max: datetime:"2024-01-06T00:00:00Z"},
    &[line(&d)]
)"#,
    )
    .await?;
    let ms = |d: &str| datetime_ms(&d.parse().unwrap());
    let x = draw(&h).await.expect("drawn").x_range;
    assert_eq!(x, (ms("2024-01-05T00:00:00Z"), ms("2024-01-06T00:00:00Z")));
    Ok(())
}

/// A view panned on one chart is not the view of a chart drawn after it
/// from the same state.
#[tokio::test(flavor = "current_thread")]
async fn a_view_belongs_to_its_chart() -> Result<()> {
    use iced_core::{event::Event, mouse::Event as ME};
    let numeric = chart_harness("&[line(&[(0.0, 0.0), (10.0, 10.0)])]").await?;
    let time = chart_harness(r#"&[line(&day1)]"#).await?;
    let mut state = ChartState::default();
    let size = Size::new(400.0, 300.0);
    let bounds = Rectangle::new(Point::ORIGIN, size);
    let base = draw_in(&time, &ChartState::default(), size).await.expect("drawn").x_range;
    let _ = draw_in(&numeric, &state, size).await;
    let at = mouse::Cursor::Available(Point::new(200.0, 150.0));
    let c = chart(&numeric);
    let _ = state.handle_event(
        c,
        &Event::Mouse(ME::ButtonPressed(mouse::Button::Left)),
        bounds,
        at,
    );
    let moved = Event::Mouse(ME::CursorMoved { position: Point::new(300.0, 150.0) });
    let _ = state.handle_event(
        c,
        &moved,
        bounds,
        mouse::Cursor::Available(Point::new(300.0, 150.0)),
    );
    assert!(state.x_view.is_some(), "the drag panned");
    assert_eq!(draw_in(&time, &state, size).await.map(|i| i.x_range), Some(base));
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn a_view_belongs_to_its_chart_mode() -> Result<()> {
    use iced_core::{event::Event, mouse::Event as ME};
    let mut h = GuiTestHarness::new(r#"
        use gui::chart::{chart, line};
        let data: [Array<(f64, f64)>, Array<(datetime, f64)>] = [(0.0, 0.0), (10.0, 10.0)];
        let change = |t: Any| data <- t ~ [(datetime:"2024-01-01T00:00:00Z", 1.0),
                                          (datetime:"2024-01-02T00:00:00Z", 2.0)];
        let result = chart(&[line(&data)])
    "#).await?;
    let mut state = ChartState::default();
    let size = Size::new(400.0, 300.0);
    let bounds = Rectangle::new(Point::ORIGIN, size);
    draw_in(&h, &state, size).await.expect("numeric plot");
    let start = Point::new(100.0, 100.0);
    let end = Point::new(200.0, 100.0);
    for (ev, p) in [
        (ME::ButtonPressed(mouse::Button::Left), start),
        (ME::CursorMoved { position: end }, end),
        (ME::ButtonReleased(mouse::Button::Left), end),
    ] {
        state.handle_event(
            chart(&h),
            &Event::Mouse(ev),
            bounds,
            mouse::Cursor::Available(p),
        );
    }
    assert!(state.x_view.is_some());
    let change = h.compile_named_callable("test::change").await?;
    h.call_callback(change, ValArray::from_iter([Value::Null])).await?;
    assert_eq!(chart(&h).mode(), ChartMode::TimeSeries);
    assert_eq!(state.view_for(chart(&h)).x, None);
    assert!(draw_in(&h, &state, size).await.expect("time plot").x_range.0 > 1e12);
    Ok(())
}

/// A press that moves is a drag, two quick clicks in one place reset the
/// view, and two quick clicks apart do not.
#[tokio::test(flavor = "current_thread")]
async fn double_clicks_are_clicks_in_one_place() -> Result<()> {
    use iced_core::{event::Event, mouse::Event as ME};
    let h = chart_harness("&[line(&[(0.0, 0.0), (10.0, 10.0)])]").await?;
    let c = chart(&h);
    let size = Size::new(400.0, 300.0);
    let bounds = Rectangle::new(Point::ORIGIN, size);
    let mut state = ChartState::default();
    let _ = draw_in(&h, &state, size).await;
    let away = Event::Mouse(ME::CursorMoved { position: Point::new(1.0, 1.0) });
    let _ = state.handle_event(c, &away, bounds, mouse::Cursor::Unavailable);
    let click = |state: &mut ChartState, p: Point| {
        let at = mouse::Cursor::Available(p);
        for ev in [
            ME::ButtonPressed(mouse::Button::Left),
            ME::ButtonReleased(mouse::Button::Left),
        ] {
            let _ = state.handle_event(c, &Event::Mouse(ev), bounds, at);
        }
    };
    state.x_view = Some((1.0, 2.0));
    click(&mut state, Point::new(100.0, 100.0));
    click(&mut state, Point::new(250.0, 100.0));
    assert_eq!(state.x_view, Some((1.0, 2.0)), "clicks apart keep the view");
    click(&mut state, Point::new(250.0, 100.0));
    assert_eq!(state.x_view, None, "a double-click resets it");
    state.x_view = Some((1.0, 2.0));
    for _ in 0..2 {
        let start = Point::new(100.0, 100.0);
        let end = Point::new(200.0, 100.0);
        for (ev, p) in [
            (ME::ButtonPressed(mouse::Button::Left), start),
            (ME::CursorMoved { position: end }, end),
            (ME::ButtonReleased(mouse::Button::Left), end),
        ] {
            state.handle_event(c, &Event::Mouse(ev), bounds, mouse::Cursor::Available(p));
        }
        assert!(state.x_view.is_some(), "a drag must not count as a reset click");
    }
    Ok(())
}

/// The bar tooltip reads the hovered slot's category from each series.
#[tokio::test(flavor = "current_thread")]
async fn the_bar_tooltip_reads_its_slot() -> Result<()> {
    use iced_core::{event::Event, mouse::Event as ME};
    let h = chart_harness(
        r#"&[bar(#label: "A", &[("a", 4.0)]), bar(#label: "B", &[("b", 2.0), ("a", 3.0)])]"#,
    )
    .await?;
    let size = Size::new(400.0, 300.0);
    let mut state = ChartState::default();
    let info = draw_in(&h, &state, size).await.expect("drawn");
    let x = info.rect.x + info.rect.width * 0.75;
    let p = Point::new(x, info.rect.y + info.rect.height / 2.0);
    let moved = Event::Mouse(ME::CursorMoved { position: p });
    let bounds = Rectangle::new(Point::ORIGIN, size);
    let at = mouse::Cursor::Available(p);
    let _ = state.handle_event(chart(&h), &moved, bounds, at);
    let snap = state.tooltip(chart(&h), bounds, at).expect("a bar under the cursor");
    assert_eq!((snap.label.as_str(), snap.value.as_str()), ("B", "b: 2.00"));
    Ok(())
}

/// Numeric and datetime series cannot share an axis: the chart draws
/// nothing.
#[tokio::test(flavor = "current_thread")]
async fn numeric_and_datetime_series_do_not_mix() -> Result<()> {
    let h = chart_harness(r#"&[line(&[(0.0, 1.0)]), line(&day1)]"#).await?;
    assert_eq!(chart(&h).mode(), ChartMode::Empty);
    assert!(draw(&h).await.is_none());
    Ok(())
}

/// A surface draws from its grid whatever order its axes run in.
#[tokio::test(flavor = "current_thread")]
// CR claude for claude: [test-gap/medium] This pin cannot fail except by a panic.
// draw_3d_chart never sets plot_info, so draw() returns None for every 3D chart, and
// the result is discarded. The defect it pins (a descending grid drawn with the first
// row's z everywhere, design/review-2026-10-05/repro/gui-chart-08.rs) draws a wrong
// picture without panicking, so the test passes with it back. mesh_style_3d and the 3D
// case of zero_and_negative_mesh_counts_draw have the same problem.
// invalid_3d_points_are_omitted_and_break_lines compares against charts that draw no
// series, so it passes if Line3D, Surface or Scatter3D draw nothing. Fix: fix all three
// ranges and screenshot, as assert_same_series_pixels does. Assert the descending
// surface differs from the same grid with every z set to the first row's z, and from an
// axes-only 3D chart. Add the same 'differs from empty' control to the invalid-3D
// comparisons. Found by reading; not run. (tests-gui-b-02)
async fn a_descending_surface_draws() -> Result<()> {
    let h = chart_harness(
        "&[surface(#color_by_z: true, &[\
           [(2.0, 2.0, 5.0), (2.0, 1.0, 5.0), (2.0, 0.0, 5.0)], \
           [(1.0, 2.0, 9.0), (1.0, 1.0, 9.0)], \
           [(0.0, 2.0, 1.0), (0.0, 1.0, 1.0), (0.0, 0.0, 1.0)]])]",
    )
    .await?;
    let _ = draw(&h).await;
    Ok(())
}

#[test]
fn colors_keep_their_alpha() {
    use crate::widgets::chart::ChartColor;
    let c = ChartColor(1.0, 0.0, 0.0, 0.3).to_plotters();
    assert_eq!((c.0, c.1, c.2), (255, 0, 0));
    assert!((c.3 - 0.3).abs() < 1e-6);
}

#[tokio::test(flavor = "current_thread")]
async fn bar_categories_share_the_dataset_labels() -> Result<()> {
    use crate::widgets::chart::DatasetEntry;
    let h = chart_harness(r#"&[bar(&[("a category name longer than 24 bytes", 1.0)])]"#)
        .await?;
    let chart = chart(&h);
    let categories = chart.bar_categories();
    let DatasetEntry::Bar { data, .. } = &chart.datasets()[0] else { unreachable!() };
    assert!(std::ptr::eq(
        categories[0].as_str(),
        data.t.as_ref().unwrap().0[0].0.as_str()
    ));
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn bar_slots_sum_duplicates_and_follow_data_changes() -> Result<()> {
    let mut h = GuiTestHarness::new(
        r#"
        use gui::chart::{bar, chart};
        let data = [("a", 1.0), ("a", 2.0), ("b", 4.0)];
        let change = |t: Any| data <- t ~ [("c", 7.0), ("c", 8.0)];
        let result = chart(&[bar(&data), bar(&[("b", 10.0), ("c", 20.0)])])
    "#,
    )
    .await?;
    let c = chart(&h);
    assert_eq!(c.bar_categories(), ["a", "b", "c"]);
    assert_eq!(
        [c.bar_value(0, 0), c.bar_value(0, 1), c.bar_value(0, 2)],
        [Some(3.0), Some(4.0), None]
    );
    assert_eq!(
        [c.bar_value(1, 0), c.bar_value(1, 1), c.bar_value(1, 2)],
        [None, Some(10.0), Some(20.0)]
    );
    let change = h.compile_named_callable("test::change").await?;
    h.call_callback(change, ValArray::from_iter([Value::Null])).await?;
    let c = chart(&h);
    assert_eq!(c.bar_categories(), ["c", "b"]);
    assert_eq!([c.bar_value(0, 0), c.bar_value(0, 1)], [Some(15.0), None]);
    assert_eq!([c.bar_value(1, 0), c.bar_value(1, 1)], [Some(20.0), Some(10.0)]);
    Ok(())
}

async fn assert_same_series_pixels(actual: &str, expected: &str) -> Result<()> {
    let args = |data| {
        format_compact!(
            "#width: &`Fill, #height: &`Fixed(200.0), \
         #x_range: &{{min: 0.0, max: 5.0}}, \
         #y_range: &{{min: 0.0, max: 5.0}}, \
         #z_range: &{{min: 0.0, max: 5.0}}, &[{data}]"
        )
    };
    let mut actual = chart_harness(&args(actual)).await?;
    let mut expected = chart_harness(&args(expected)).await?;
    assert!(actual.screenshot(400, 200) == expected.screenshot(400, 200));
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn nonfinite_candles_and_error_bars_are_omitted() -> Result<()> {
    for (constructor, point) in [
        ("candlestick", "{x: 2.0, open: 1.0, high: 4.0, low: 0.0 / 0.0, close: 3.0}"),
        ("error_bar", "{x: 2.0, min: 0.0 / 0.0, avg: 2.0, max: 4.0}"),
    ] {
        assert_same_series_pixels(
            &format_compact!("line(&[(1.0, 1.0)]), {constructor}(&[{point}])"),
            "line(&[(1.0, 1.0)])",
        )
        .await?;
    }
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn clipped_candles_keep_their_gain_color() -> Result<()> {
    let mut h = chart_harness(
        "#width: &`Fill, #height: &`Fixed(200.0), \
         #x_range: &{min: 0.0, max: 5.0}, #y_range: &{min: 0.0, max: 5.0}, \
         &[candlestick(#gain_color: color(#r: 0.0, #g: 1.0, #b: 0.0)$, \
           #loss_color: color(#r: 1.0, #g: 0.0, #b: 1.0)$, #bar_width: 20.0, \
           &[{x: 2.0, open: 6.0, high: 8.0, low: 2.0, close: 7.0}])]",
    )
    .await?;
    let pixels = h.screenshot(400, 200);
    assert!(color_pixels(&pixels, [0, 255, 0]) > 10);
    assert_eq!(color_pixels(&pixels, [255, 0, 255]), 0);
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn invalid_3d_points_are_omitted_and_break_lines() -> Result<()> {
    for invalid in ["(2.0, 0.0 / 0.0, 2.0)", "(20.0, 20.0, 20.0)"] {
        assert_same_series_pixels(
            &format_compact!("scatter3d(&[(1.0, 1.0, 1.0), {invalid}])"),
            "scatter3d(&[(1.0, 1.0, 1.0)])",
        )
        .await?;
        assert_same_series_pixels(
            &format_compact!(
                "line3d(#point_size: 0.0, &[(1.0, 1.0, 1.0), {invalid}, (3.0, 3.0, 3.0)])"
            ),
            "line3d(#point_size: 0.0, &[(1.0, 1.0, 1.0)])",
        )
        .await?;
        assert_same_series_pixels(
            &format_compact!(
                "surface(&[[(1.0, 1.0, 1.0), (2.0, 1.0, 2.0)], [(1.0, 2.0, 2.0), {invalid}]])"
            ),
            "line3d(#point_size: 0.0, &[(1.0, 1.0, 1.0)])",
        ).await?;
    }
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn kept_charts_keep_their_own_cache() -> Result<()> {
    let tall = chart_harness("&[line(&[(1.0, 1000.0), (2.0, 2000.0)])]").await?;
    let short = chart_harness("&[line(&[(1.0, 1.0), (2.0, 2.0)])]").await?;
    let (mut top, bottom) = (ChartState::default(), ChartState::default());
    let size = Size::new(400.0, 300.0);
    let tall_range = draw_in(&tall, &top, size).await.unwrap().y_range;
    let short_range = draw_in(&short, &bottom, size).await.unwrap().y_range;
    assert_eq!(draw_in(&short, &top, size).await.unwrap().y_range, short_range);
    assert_eq!(draw_in(&tall, &bottom, size).await.unwrap().y_range, tall_range);
    let event = iced_core::Event::Mouse(mouse::Event::WheelScrolled {
        delta: mouse::ScrollDelta::Lines { x: 0.0, y: 1.0 },
    });
    top.handle_event(
        chart(&short),
        &event,
        Rectangle::with_size(size),
        mouse::Cursor::Available(Point::new(200.0, 150.0)),
    );
    assert!(top.y_view.unwrap().1 < 10.0);
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn a_cached_chart_discards_a_view_from_its_previous_slot() -> Result<()> {
    let h = chart_harness("&[line(&[(1.0, 1.0), (2.0, 2.0)])]").await?;
    let mut state = ChartState::default();
    let size = Size::new(400.0, 300.0);
    let base = draw_in(&h, &state, size).await.unwrap().y_range;
    state.handle_event(
        chart(&h),
        &iced_core::Event::Mouse(mouse::Event::WheelScrolled {
            delta: mouse::ScrollDelta::Lines { x: 0.0, y: 1.0 },
        }),
        Rectangle::with_size(size),
        mouse::Cursor::Available(Point::new(200.0, 150.0)),
    );
    assert_ne!(draw_in(&h, &state, size).await.unwrap().y_range, base);
    assert_eq!(draw_in(&h, &ChartState::default(), size).await.unwrap().y_range, base);
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn a_single_bar_label_is_centered_in_the_plot() -> Result<()> {
    let mut h = chart_harness(
        r#"#width: &`Fill, #height: &`Fixed(200.0),
        #style: &chart_style(#mesh: mesh_style(
            #label_color: color(#r: 1.0, #g: 0.0, #b: 1.0)$,
            #label_size: 18.0, #x_label_area_size: 30.0, #y_label_area_size: 70.0)),
        &[bar(&[("only", 5.0)])]"#,
    )
    .await?;
    let info =
        draw_in(&h, &ChartState::default(), Size::new(400.0, 200.0)).await.unwrap();
    let pixels = h.screenshot(400, 200);
    let mut xs = pixels.chunks_exact(4).enumerate().filter_map(|(i, px)| {
        (i / 400 > info.rect.y as usize + info.rect.height as usize
            && px[..3] == [255, 0, 255])
        .then_some((i % 400) as f32)
    });
    let first = xs.next().expect("visible category text");
    let (lo, hi) = xs.fold((first, first), |(lo, hi), x| (lo.min(x), hi.max(x)));
    let center = (lo + hi) / 2.0;
    assert!(
        (center - info.rect.center_x()).abs() <= 3.0,
        "label {center}, plot {:?}",
        info.rect
    );
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn pie_drawing_skips_empty_and_nonpositive_datasets() -> Result<()> {
    for empty in ["[]", "[(\"zero\", 0.0), (\"negative\", -1.0)]"] {
        assert_same_series_pixels(
            &format_compact!("pie(&{empty}), pie(&[(\"first\", 1.0)])"),
            "pie(&[(\"first\", 1.0)])",
        )
        .await?;
    }
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn pie_tooltip_uses_the_drawn_dataset() -> Result<()> {
    let h = chart_harness(r#"&[pie(&[("first", 1.0)]), pie(&[("other", 2.0)])]"#).await?;
    let mut state = ChartState::default();
    let size = Size::new(400.0, 300.0);
    let info = draw_in(&h, &state, size).await.unwrap();
    let p = info.rect.center();
    state.handle_event(
        chart(&h),
        &iced_core::Event::Mouse(mouse::Event::CursorMoved { position: p }),
        Rectangle::with_size(size),
        mouse::Cursor::Available(p),
    );
    let snap =
        state.tooltip(chart(&h), Rectangle::with_size(size), mouse::Cursor::Available(p));
    let snap = snap.unwrap();
    assert_eq!(snap.label, "first");
    assert_eq!(snap.value, "1.00 (100.0%)");
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn chart_motion_redraws_only_changed_content() -> Result<()> {
    let h = centered_point_chart().await?;
    let mut state = ChartState::default();
    let size = Size::new(400.0, 300.0);
    let center = draw_in(&h, &state, size).await.unwrap().rect.center();
    let away = Point::new(-10.0, -10.0);
    for (p, redraw) in
        [(away, false), (center, true), (center, false), (away, true), (away, false)]
    {
        let action = state.handle_event(
            chart(&h),
            &iced_core::Event::Mouse(mouse::Event::CursorMoved { position: p }),
            Rectangle::with_size(size),
            mouse::Cursor::Available(p),
        );
        assert_eq!(action.is_some(), redraw);
    }
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn pie_and_empty_charts_do_not_capture_drags() -> Result<()> {
    for data in [r#"&[pie(&[("a", 1.0)])]"#, "&[]"] {
        let h = chart_harness(data).await?;
        let mut state = ChartState::default();
        let size = Size::new(400.0, 300.0);
        draw_in(&h, &state, size).await;
        assert!(
            state
                .handle_event(
                    chart(&h),
                    &iced_core::Event::Mouse(mouse::Event::ButtonPressed(
                        mouse::Button::Left
                    )),
                    Rectangle::with_size(size),
                    mouse::Cursor::Available(Point::new(200.0, 150.0))
                )
                .is_none()
        );
    }
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn unchanged_chart_drag_does_not_redraw() -> Result<()> {
    let h = chart_harness("&[line(&[(1.0, 1.0), (2.0, 2.0)])]").await?;
    let mut state = ChartState::default();
    let size = Size::new(400.0, 300.0);
    draw_in(&h, &state, size).await;
    let at = Point::new(200.0, 150.0);
    state.handle_event(
        chart(&h),
        &iced_core::Event::Mouse(mouse::Event::ButtonPressed(mouse::Button::Left)),
        Rectangle::with_size(size),
        mouse::Cursor::Available(at),
    );
    use iced_core::window::RedrawRequest::{NextFrame, Wait};
    for (p, redraw) in [
        (at, Wait),
        (Point::new(220.0, 150.0), NextFrame),
        (Point::new(220.0, 150.0), Wait),
    ] {
        let action = state
            .handle_event(
                chart(&h),
                &iced_core::Event::Mouse(mouse::Event::CursorMoved { position: p }),
                Rectangle::with_size(size),
                mouse::Cursor::Available(p),
            )
            .unwrap();
        assert_eq!(action.into_inner().1, redraw);
    }
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn chart_hover_uses_translated_cursor() -> Result<()> {
    let h = centered_point_chart().await?;
    let mut state = ChartState::default();
    let size = Size::new(400.0, 300.0);
    let p = draw_in(&h, &state, size).await.unwrap().rect.center();
    let bounds = Rectangle::new(Point::new(30.0, 500.0), size);
    let at = mouse::Cursor::Available(Point::new(p.x + 30.0, p.y + 500.0));
    state.handle_event(
        chart(&h),
        &iced_core::Event::Mouse(mouse::Event::CursorMoved {
            position: Point::new(p.x + 30.0, p.y),
        }),
        bounds,
        at,
    );
    assert!(state.tooltip(chart(&h), bounds, at).unwrap().pixel.distance(p) < 1.0);
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn chart_drag_uses_translated_cursor_outside_bounds() -> Result<()> {
    let h = centered_point_chart().await?;
    let mut state = ChartState::default();
    let size = Size::new(400.0, 300.0);
    let info = draw_in(&h, &state, size).await.unwrap();
    let bounds = Rectangle::new(Point::new(30.0, 500.0), size);
    state.handle_event(
        chart(&h),
        &iced_core::Event::Mouse(mouse::Event::ButtonPressed(mouse::Button::Left)),
        bounds,
        mouse::Cursor::Available(Point::new(230.0, 650.0)),
    );
    state.handle_event(
        chart(&h),
        &iced_core::Event::Mouse(mouse::Event::CursorMoved {
            position: Point::new(480.0, 150.0),
        }),
        bounds,
        mouse::Cursor::Levitating(Point::new(480.0, 650.0)),
    );
    assert_eq!(state.y_view, Some((0.0, 10.0)));
    assert!(
        (state.x_view.unwrap().0 + (250.0 / info.rect.width) as f64 * 10.0).abs() < 1e-6
    );
    Ok(())
}

async fn centered_point_chart() -> Result<GuiTestHarness> {
    chart_harness("#x_range: &{min: 0.0, max: 10.0}, #y_range: &{min: 0.0, max: 10.0}, &[line(&[(5.0, 5.0)])]").await
}

async fn hover_at_fraction(
    args: &str,
    x: f32,
    y: f32,
) -> Result<(PlotInfo, Option<crate::widgets::chart::SnapPoint>)> {
    let h = chart_harness(args).await?;
    let mut state = ChartState::default();
    let size = Size::new(400.0, 300.0);
    let info = draw_in(&h, &state, size).await.unwrap();
    let p =
        Point::new(info.rect.x + x * info.rect.width, info.rect.y + y * info.rect.height);
    state.handle_event(
        chart(&h),
        &iced_core::Event::Mouse(mouse::Event::CursorMoved { position: p }),
        Rectangle::with_size(size),
        mouse::Cursor::Available(p),
    );
    Ok((
        info,
        state.tooltip(chart(&h), Rectangle::with_size(size), mouse::Cursor::Available(p)),
    ))
}

#[tokio::test(flavor = "current_thread")]
async fn bar_hover_anchors_the_visible_value() -> Result<()> {
    let (info, snap) = hover_at_fraction(
        r#"#y_range: &{min: 0.0, max: 1.0}, &[bar(&[("a", 1000.0)])]"#,
        0.5,
        0.5,
    )
    .await?;
    let snap = snap.unwrap();
    assert_eq!(snap.pixel.y, info.rect.y);
    assert_eq!(snap.value, "a: 1000.00");
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn nonfinite_bars_have_no_tooltip() -> Result<()> {
    let (_, snap) =
        hover_at_fraction(r#"&[bar(&[("a", 0.0 / 0.0), ("b", 1.0)])]"#, 0.25, 0.5)
            .await?;
    assert!(snap.is_none());
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn chart_hover_ignores_points_not_drawn() -> Result<()> {
    for (data, y) in [
        ("scatter(&[(5.0, 10.01)])", 0.001),
        (
            "candlestick(&[{x: 5.0, open: 11.0, high: 12.0, low: 10.01, close: 10.01}])",
            0.001,
        ),
        ("error_bar(&[{x: 5.0, min: 10.01, avg: 10.01, max: 11.0}])", 0.001),
        (
            "candlestick(&[{x: 5.0, open: 4.0, high: 6.0, low: 0.0 / 0.0, close: 5.0}])",
            0.5,
        ),
        ("error_bar(&[{x: 5.0, min: 0.0 / 0.0, avg: 5.0, max: 6.0}])", 0.5),
    ] {
        let (_, snap) = hover_at_fraction(&format_compact!(
            "#x_range: &{{min: 0.0, max: 10.0}}, #y_range: &{{min: 0.0, max: 10.0}}, &[{data}]"
        ), 0.5, y).await?;
        assert!(snap.is_none(), "{data}");
    }
    Ok(())
}

#[tokio::test(flavor = "multi_thread")]
async fn equal_dataset_events_keep_the_compiled_data() -> Result<()> {
    let mut h = chart_harness("&[line(&[(1.0, 1.0)])]").await?;
    let source = chart(&h).datasets_ref();
    let (id, value) = (source.id, source.last.clone().unwrap());
    let data_id = |h: &GuiTestHarness| match &chart(h).datasets()[0] {
        crate::widgets::chart::DatasetEntry::XY { data, .. } => data.r.id,
        _ => panic!("an XY dataset"),
    };
    let before = data_id(&h);
    assert!(!super::update_widget(
        &mut h.root,
        &h.f.gx,
        &tokio::runtime::Handle::current(),
        id,
        &value
    )?);
    assert_eq!(data_id(&h), before);
    Ok(())
}

#[tokio::test(flavor = "multi_thread")]
async fn a_failed_dataset_recompile_is_retryable() -> Result<()> {
    let mut h = chart_harness("&[line(&[(1.0, 1.0)])]").await?;
    let id = chart(&h).datasets_ref().id;
    for _ in 0..2 {
        assert!(
            super::update_widget(
                &mut h.root,
                &h.f.gx,
                &tokio::runtime::Handle::current(),
                id,
                &Value::Null
            )
            .is_err()
        );
    }
    assert_eq!(chart(&h).datasets().len(), 1);
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn chart_text_sizes_are_validated_before_drawing() -> Result<()> {
    for field in ["title", "mesh", "legend"] {
        for size in ["0.0", "-1.0", "0.0 / 0.0", "1.0 / 0.0", "100000.0", "16.0"] {
            let style = match field {
                "title" => format_compact!("chart_style(#title_size: {size})"),
                "mesh" => {
                    format_compact!("chart_style(#mesh: mesh_style(#label_size: {size}))")
                }
                _ => format_compact!(
                    "chart_style(#legend: legend_style(#label_size: {size}))"
                ),
            };
            let result = chart_harness(&format_compact!(
                "#width: &`Fill, #height: &`Fixed(200.0), #title: &\"chart\", #style: &{style}, &[line(#label: \"series\", &[(1.0, 1.0)])]"
            )).await;
            match result {
                Ok(mut h) => {
                    if size == "0.0" || size == "16.0" {
                        h.screenshot(400, 200);
                    }
                    assert_eq!(size, "16.0", "accepted {field} size {size}");
                }
                Err(e) => {
                    assert_ne!(size, "16.0");
                    assert!(format_compact!("{e:#}").contains("text size"), "{e:#}");
                }
            }
        }
    }
    Ok(())
}
