use crate::widgets::chart::{auto_range, ChartType};
use super::GuiTestHarness;
use anyhow::Result;
use netidx::publisher::{FromValue, Value};

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

// ── ChartType::from_value ───────────────────────────────────────────

#[test]
fn chart_type_line() -> Result<()> {
    let ct = ChartType::from_value(Value::String("Line".into()))?;
    assert!(matches!(ct, ChartType::Line));
    Ok(())
}

#[test]
fn chart_type_scatter() -> Result<()> {
    let ct = ChartType::from_value(Value::String("Scatter".into()))?;
    assert!(matches!(ct, ChartType::Scatter));
    Ok(())
}

#[test]
fn chart_type_bar() -> Result<()> {
    let ct = ChartType::from_value(Value::String("Bar".into()))?;
    assert!(matches!(ct, ChartType::Bar));
    Ok(())
}

#[test]
fn chart_type_area() -> Result<()> {
    let ct = ChartType::from_value(Value::String("Area".into()))?;
    assert!(matches!(ct, ChartType::Area));
    Ok(())
}

#[test]
fn chart_type_invalid() {
    assert!(ChartType::from_value(Value::String("Pie".into())).is_err());
}

// ── Axis range via chart() ──────────────────────────────────────────

#[tokio::test(flavor = "current_thread")]
async fn axis_range_renders() -> Result<()> {
    let h = chart_harness(
        "#x_range: &{min: 0.0, max: 100.0}, \
         #y_range: &{min: -5.0, max: 50.0}, \
         #width: &`Fill, #height: &`Fixed(200.0), \
         &[{data: &[(0.0, 1.0)], chart_type: `Line, color: null, label: null}]",
    )
    .await?;
    let _ = h.view();
    Ok(())
}

// ── Dataset metadata via chart() ────────────────────────────────────

#[tokio::test(flavor = "current_thread")]
async fn dataset_meta_renders() -> Result<()> {
    let h = chart_harness(concat!(
        "#width: &`Fill, #height: &`Fixed(200.0), ",
        r#"&[{data: &[], chart_type: `Line, color: null, label: "test"}]"#,
    ))
    .await?;
    let _ = h.view();
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn dataset_meta_with_color() -> Result<()> {
    let h = chart_harness(
        "#width: &`Fill, #height: &`Fixed(200.0), \
         &[{data: &[], chart_type: `Scatter, \
            color: {r: 0.0, g: 1.0, b: 0.0, a: 1.0}, label: null}]",
    )
    .await?;
    let _ = h.view();
    Ok(())
}
