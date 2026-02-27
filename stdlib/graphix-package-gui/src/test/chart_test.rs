use crate::widgets::chart::{auto_range, AxisRange, ChartType, DatasetMeta};
use anyhow::Result;
use arcstr::literal;
use netidx::publisher::{FromValue, Value};

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

// ── AxisRange::from_value ───────────────────────────────────────────

#[test]
fn axis_range_parses() -> Result<()> {
    let v: Value = [(literal!("max"), 100.0f64), (literal!("min"), 0.0f64)].into();
    let ar = AxisRange::from_value(v)?;
    assert!((ar.min - 0.0).abs() < 1e-10);
    assert!((ar.max - 100.0).abs() < 1e-10);
    Ok(())
}

// ── DatasetMeta::from_value ─────────────────────────────────────────

/// Build a dataset meta Value from raw parts.
/// `data_id` simulates the bind ID that a ref would produce.
fn dataset_meta_val(data_id: u64, chart_type: &str, color: Value, label: Value) -> Value {
    [
        (literal!("chart_type"), Value::String(chart_type.into())),
        (literal!("color"), color),
        (literal!("data"), Value::U64(data_id)),
        (literal!("label"), label),
    ]
    .into()
}

#[test]
fn dataset_meta_parses() -> Result<()> {
    let v = dataset_meta_val(42, "Line", Value::Null, Value::String("test".into()));
    let dm = DatasetMeta::from_value(v)?;
    assert_eq!(dm.data_id, 42);
    assert!(matches!(dm.chart_type, ChartType::Line));
    assert!(dm.color.is_none());
    assert_eq!(dm.label.as_deref(), Some("test"));
    Ok(())
}

#[test]
fn dataset_meta_with_color() -> Result<()> {
    let color: Value = [
        (literal!("a"), 1.0f64),
        (literal!("b"), 0.0f64),
        (literal!("g"), 1.0f64),
        (literal!("r"), 0.0f64),
    ]
    .into();
    let v = dataset_meta_val(99, "Scatter", color, Value::Null);
    let dm = DatasetMeta::from_value(v)?;
    assert!(dm.color.is_some());
    assert!(matches!(dm.chart_type, ChartType::Scatter));
    assert!(dm.label.is_none());
    Ok(())
}
