use super::GuiTestHarness;
use crate::widgets::chart::pad_range;
use anyhow::Result;

fn auto_range<'a>(
    data: impl IntoIterator<Item = &'a [(f64, f64)]>,
    f: impl Fn(&(f64, f64)) -> f64,
) -> (f64, f64) {
    let mut min = f64::INFINITY;
    let mut max = f64::NEG_INFINITY;
    for slice in data {
        for pt in slice {
            let v = f(pt);
            if v < min {
                min = v;
            }
            if v > max {
                max = v;
            }
        }
    }
    pad_range(min, max)
}

async fn chart_harness(args: &str) -> Result<GuiTestHarness> {
    let code = format!(
        "use gui;\nuse gui::chart;\n\
         let result = chart({args})"
    );
    GuiTestHarness::new(&code).await
}

// ── auto_range ──────────────────────────────────────────────────────

#[test]
fn auto_range_normal() {
    let data: &[(f64, f64)] = &[(0.0, 1.0), (5.0, 10.0), (10.0, 3.0)];
    let (xmin, xmax) = auto_range([data], |p| p.0);
    // min=0, max=10, pad=0.5 → (-0.5, 10.5)
    assert!(xmin < 0.0);
    assert!(xmax > 10.0);

    let (ymin, ymax) = auto_range([data], |p| p.1);
    // min=1, max=10, pad=0.45 → (0.55, 10.45)
    assert!(ymin < 1.0);
    assert!(ymax > 10.0);
}

#[test]
fn auto_range_single_point() {
    let data: &[(f64, f64)] = &[(5.0, 5.0)];
    let (xmin, xmax) = auto_range([data], |p| p.0);
    // Single point: 5-1=4, 5+1=6, then pad → < 4 and > 6
    assert!(xmin < 4.0);
    assert!(xmax > 6.0);
}

#[test]
fn auto_range_identical_values() {
    let data: &[(f64, f64)] = &[(3.0, 7.0), (3.0, 7.0), (3.0, 7.0)];
    let (xmin, xmax) = auto_range([data], |p| p.0);
    // All x=3 → expand to (2, 4), pad → < 2 and > 4
    assert!(xmin < 2.0);
    assert!(xmax > 4.0);
}

#[test]
fn auto_range_empty() {
    let empty: &[(f64, f64)] = &[];
    let (xmin, xmax) = auto_range([empty], |p| p.0);
    assert!(xmin.is_finite());
    assert!(xmax.is_finite());
    assert!(xmin < xmax);

    // Also with no slices at all
    let (xmin, xmax) = auto_range(std::iter::empty::<&[(f64, f64)]>(), |p| p.0);
    assert!(xmin.is_finite());
    assert!(xmax.is_finite());
    assert!(xmin < xmax);
}

#[test]
fn auto_range_negative() {
    let data: &[(f64, f64)] = &[(-10.0, -5.0), (-3.0, 2.0)];
    let (xmin, xmax) = auto_range([data], |p| p.0);
    assert!(xmin < -10.0);
    assert!(xmax > -3.0);
}

#[test]
fn auto_range_multiple_datasets() {
    let d1: &[(f64, f64)] = &[(0.0, 0.0), (5.0, 5.0)];
    let d2: &[(f64, f64)] = &[(10.0, 10.0), (20.0, 20.0)];
    let (xmin, xmax) = auto_range([d1, d2], |p| p.0);
    assert!(xmin < 0.0);
    assert!(xmax > 20.0);
}

// ── Chart with new constructor syntax ───────────────────────────────

#[tokio::test(flavor = "current_thread")]
async fn axis_range_renders() -> Result<()> {
    let h = chart_harness(
        "#x_range: &{min: 0.0, max: 100.0}, \
         #y_range: &{min: -5.0, max: 50.0}, \
         #width: &`Fill, #height: &`Fixed(200.0), \
         &[chart::line(#label: \"test\", &[(0.0, 1.0)])]",
    )
    .await?;
    let _ = h.view();
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn dataset_meta_renders() -> Result<()> {
    let h = chart_harness(concat!(
        "#width: &`Fill, #height: &`Fixed(200.0), ",
        r#"&[chart::line(#label: "test", &[])]"#,
    ))
    .await?;
    let _ = h.view();
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn dataset_meta_with_color() -> Result<()> {
    let h = chart_harness(
        "#width: &`Fill, #height: &`Fixed(200.0), \
         &[chart::scatter(#color: {r: 0.0, g: 1.0, b: 0.0, a: 1.0}, &[])]",
    )
    .await?;
    let _ = h.view();
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
    let _ = h.view();
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
    let _ = h.view();
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
    let _ = h.view();
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
    let _ = h.view();
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn background_color() -> Result<()> {
    let h = chart_harness(
        "#width: &`Fill, #height: &`Fixed(200.0), \
         #background: &{r: 0.1, g: 0.1, b: 0.1, a: 1.0}, \
         &[chart::line(&[(0.0, 0.0), (5.0, 5.0)])]",
    )
    .await?;
    let _ = h.view();
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn mesh_style() -> Result<()> {
    let h = chart_harness(
        "#width: &`Fill, #height: &`Fixed(200.0), \
         #mesh: &{show_x_grid: true, show_y_grid: false, \
                   grid_color: null, axis_color: null, \
                   label_color: null, label_size: 12.0, \
                   x_label_area_size: null, x_labels: 5, \
                   y_label_area_size: null, y_labels: 5}, \
         &[chart::line(&[(0.0, 0.0), (5.0, 5.0)])]",
    )
    .await?;
    let _ = h.view();
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn dark_background_label_colors() -> Result<()> {
    let h = chart_harness(
        "#width: &`Fill, #height: &`Fixed(200.0), \
         #title: &\"Dark Chart\", \
         #title_color: &{r: 0.9, g: 0.9, b: 0.9, a: 1.0}, \
         #x_label: &\"X\", #y_label: &\"Y\", \
         #background: &{r: 0.1, g: 0.1, b: 0.15, a: 1.0}, \
         #mesh: &{show_x_grid: true, show_y_grid: true, \
                   grid_color: {r: 0.3, g: 0.3, b: 0.35, a: 1.0}, \
                   axis_color: {r: 0.5, g: 0.5, b: 0.55, a: 1.0}, \
                   label_color: {r: 0.8, g: 0.8, b: 0.8, a: 1.0}, \
                   label_size: 14.0, x_label_area_size: null, x_labels: null, \
                   y_label_area_size: null, y_labels: null}, \
         &[chart::line(#label: \"Series\", &[(0.0, 0.0), (5.0, 5.0)])]",
    )
    .await?;
    let _ = h.view();
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn legend_style() -> Result<()> {
    let h = chart_harness(
        "#width: &`Fill, #height: &`Fixed(200.0), \
         #legend_style: &{ \
           background: {r: 0.2, g: 0.2, b: 0.25, a: 1.0}, \
           border: {r: 0.5, g: 0.5, b: 0.5, a: 1.0}, \
           label_color: {r: 0.9, g: 0.9, b: 0.9, a: 1.0}, \
           label_size: null}, \
         &[chart::line(#label: \"Test\", &[(0.0, 0.0), (5.0, 5.0)])]",
    )
    .await?;
    let _ = h.view();
    Ok(())
}
