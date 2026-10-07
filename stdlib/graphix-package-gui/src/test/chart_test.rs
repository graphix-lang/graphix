use super::GuiTestHarness;
use anyhow::Result;

use crate::{
    theme::GraphixTheme,
    widgets::chart::{ChartMode, ChartState, ChartW, PlotInfo},
};
use graphix_rt::NoExt;
use iced_core::{Point, Rectangle, Size, mouse};
use iced_widget::canvas::Program;

fn chart(h: &GuiTestHarness) -> &ChartW<NoExt> {
    h.widget.as_any().downcast_ref::<ChartW<NoExt>>().expect("a chart")
}

/// Draw the chart through its program into `state` at `size`, returning
/// the plot area the draw recorded.
async fn draw_in(h: &GuiTestHarness, state: &ChartState, size: Size) -> Option<PlotInfo> {
    let renderer = super::headless_gpu().await.create_renderer();
    let theme = GraphixTheme { inner: iced_core::Theme::Dark, overrides: None };
    let bounds = Rectangle::new(Point::ORIGIN, size);
    let _ = Program::draw(
        chart(h),
        state,
        &renderer,
        &theme,
        bounds,
        mouse::Cursor::Unavailable,
    );
    state.plot_info.get()
}

async fn draw(h: &GuiTestHarness) -> Option<PlotInfo> {
    draw_in(h, &ChartState::default(), Size::new(400.0, 300.0)).await
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
async fn dataset_meta_renders() -> Result<()> {
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
async fn dataset_meta_with_color() -> Result<()> {
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

#[tokio::test(flavor = "current_thread")]
async fn series_style_stroke_width() -> Result<()> {
    let h = chart_harness(
        "#width: &`Fill, #height: &`Fixed(200.0), \
         &[chart::line(#stroke_width: 4.0, #point_size: 5.0, \
           &[(0.0, 0.0), (5.0, 5.0)])]",
    )
    .await?;
    assert!(draw(&h).await.is_some());
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn background_color() -> Result<()> {
    let h = chart_harness(
        "#width: &`Fill, #height: &`Fixed(200.0), \
         #style: &chart_style(#background: color(#r: 0.1, #g: 0.1, #b: 0.1, #a: 1.0)$), \
         &[chart::line(&[(0.0, 0.0), (5.0, 5.0)])]",
    )
    .await?;
    assert!(draw(&h).await.is_some());
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn mesh_style() -> Result<()> {
    let h = chart_harness(
        "#width: &`Fill, #height: &`Fixed(200.0), \
         #style: &chart_style(#mesh: {show_x_grid: true, show_y_grid: false, \
                   grid_color: null, bold_line_color: color(#r: 0.5, #g: 0.5, #b: 0.5)$, \
                   axis_color: null, \
                   label_color: null, label_size: 12.0, \
                   x_label_area_size: null, x_labels: 5, x_light_lines: 0, \
                   y_label_area_size: null, y_labels: 5, y_light_lines: 2, \
                   z_labels: null, z_light_lines: null}), \
         &[chart::line(&[(0.0, 0.0), (5.0, 5.0)])]",
    )
    .await?;
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
    let h = GuiTestHarness::new(code).await?;
    h.render().await?;
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

/// The geometry cache is `Program::State`, which iced keeps in its
/// widget tree by position, so a chart compiled into a slot another
/// chart drew from inherits that chart's cache. Its first draw must
/// not trust it.
#[tokio::test(flavor = "current_thread")]
async fn fresh_chart_redraws_an_inherited_cache() -> Result<()> {
    use crate::{
        theme::GraphixTheme,
        widgets::chart::{ChartState, ChartW},
    };
    use graphix_rt::NoExt;
    use iced_core::{Point, Rectangle, Size, mouse};
    use iced_widget::canvas::Program;
    let tall = chart_harness(r#"&[line(&[(1.0, 1000.0), (2.0, 2000.0)])]"#).await?;
    let short = chart_harness(r#"&[line(&[(1.0, 1.0), (2.0, 2.0)])]"#).await?;
    let renderer = super::headless_gpu().await.create_renderer();
    let theme = GraphixTheme { inner: iced_core::Theme::Dark, overrides: None };
    let bounds = Rectangle::new(Point::ORIGIN, Size::new(400.0, 300.0));
    let state = ChartState::default();
    let y_max = |w: &crate::widgets::GuiW<NoExt>| {
        let c = w.as_any().downcast_ref::<ChartW<NoExt>>().expect("a chart");
        let _ = Program::draw(
            c,
            &state,
            &renderer,
            &theme,
            bounds,
            mouse::Cursor::Unavailable,
        );
        state.plot_info.get().expect("draw records the plot area").y_range.1
    };
    assert!(y_max(&tall.widget) > 1000.0);
    assert!(y_max(&short.widget) < 100.0);
    Ok(())
}

#[test]
fn marker_size_rule() {
    use crate::widgets::chart::marker_size;
    assert_eq!(marker_size(Some(6.0), 4), 6);
    assert_eq!(marker_size(Some(0.0), 1), 0);
    assert_eq!(marker_size(None, 4), 0);
    assert_eq!(marker_size(None, 1), 3);
}

/// Every marker path draws through the program without erroring on a
/// one-point series, whose auto range is degenerate.
#[tokio::test(flavor = "current_thread")]
async fn markers_draw_on_every_series_kind() -> Result<()> {
    use crate::{
        theme::GraphixTheme,
        widgets::chart::{ChartState, ChartW},
    };
    use graphix_rt::NoExt;
    use iced_core::{Point, Rectangle, Size, mouse};
    use iced_widget::canvas::Program;
    let h = chart_harness(
        r#"&[
            line(&[(2.0, 5.0)]),
            line(#point_size: 6.0, &[(1.0, 1.0), (2.0, 3.0), (3.0, 2.0)]),
            dashed_line(#point_size: 4.0, &[(1.0, 4.0), (2.0, 4.5), (3.0, 3.5)]),
            area(&[(3.5, 1.5)])
        ]"#,
    )
    .await?;
    let renderer = super::headless_gpu().await.create_renderer();
    let theme = GraphixTheme { inner: iced_core::Theme::Dark, overrides: None };
    let bounds = Rectangle::new(Point::ORIGIN, Size::new(400.0, 300.0));
    let state = ChartState::default();
    let c = h.widget.as_any().downcast_ref::<ChartW<NoExt>>().expect("a chart");
    let _ =
        Program::draw(c, &state, &renderer, &theme, bounds, mouse::Cursor::Unavailable);
    assert!(state.plot_info.get().is_some());
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

/// A pie whose slices sum to nothing, or with a non-finite start angle,
/// draws nothing rather than looping forever.
#[tokio::test(flavor = "current_thread")]
async fn degenerate_pies_draw_nothing() -> Result<()> {
    let h = chart_harness(r#"&[pie(&[("in", 100.0), ("out", -100.0)])]"#).await?;
    let _ = draw(&h).await;
    let h =
        chart_harness(r#"&[pie(#start_angle: 1.0 / 0.0, &[("a", 1.0), ("b", 2.0)])]"#)
            .await?;
    assert!(draw(&h).await.is_some());
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
/// from the same state, nor of the same chart once its mode changes.
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
    let _ = state.handle_event(c, &moved, bounds, at);
    assert!(state.x_view.is_some(), "the drag panned");
    assert_eq!(draw_in(&time, &state, size).await.map(|i| i.x_range), Some(base));
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
    let _ = state.handle_event(chart(&h), &moved, bounds, mouse::Cursor::Available(p));
    let snap = state.snap_point.expect("a bar under the cursor");
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
