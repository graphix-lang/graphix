use crate::widgets::chart::{auto_range, AxisRange, ChartType, Dataset};
use anyhow::Result;
use arcstr::literal;
use netidx::publisher::{FromValue, Value};

// ── auto_range ──────────────────────────────────────────────────────

fn make_dataset(data: &[(f64, f64)]) -> Dataset {
    Dataset::from_value(dataset_val(data, "Line", Value::Null, Value::Null))
        .expect("dataset from_value")
}

#[test]
fn auto_range_normal() {
    let ds = make_dataset(&[(0.0, 1.0), (5.0, 10.0), (10.0, 3.0)]);
    let (xmin, xmax) = auto_range(&[ds.clone()], |p| p.0);
    // min=0, max=10, pad=0.5 → (-0.5, 10.5)
    assert!(xmin < 0.0);
    assert!(xmax > 10.0);

    let (ymin, ymax) = auto_range(&[ds], |p| p.1);
    // min=1, max=10, pad=0.45 → (0.55, 10.45)
    assert!(ymin < 1.0);
    assert!(ymax > 10.0);
}

#[test]
fn auto_range_single_point() {
    let ds = make_dataset(&[(5.0, 5.0)]);
    let (xmin, xmax) = auto_range(&[ds], |p| p.0);
    // Single point: 5-1=4, 5+1=6, then pad → < 4 and > 6
    assert!(xmin < 4.0);
    assert!(xmax > 6.0);
}

#[test]
fn auto_range_identical_values() {
    let ds = make_dataset(&[(3.0, 7.0), (3.0, 7.0), (3.0, 7.0)]);
    let (xmin, xmax) = auto_range(&[ds], |p| p.0);
    // All x=3 → expand to (2, 4), pad → < 2 and > 4
    assert!(xmin < 2.0);
    assert!(xmax > 4.0);
}

#[test]
fn auto_range_negative() {
    let ds = make_dataset(&[(-10.0, -5.0), (-3.0, 2.0)]);
    let (xmin, xmax) = auto_range(&[ds], |p| p.0);
    assert!(xmin < -10.0);
    assert!(xmax > -3.0);
}

#[test]
fn auto_range_multiple_datasets() {
    let ds1 = make_dataset(&[(0.0, 0.0), (5.0, 5.0)]);
    let ds2 = make_dataset(&[(10.0, 10.0), (20.0, 20.0)]);
    let (xmin, xmax) = auto_range(&[ds1, ds2], |p| p.0);
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

// ── Dataset::from_value ─────────────────────────────────────────────

/// Build a dataset Value from raw parts.
fn dataset_val(
    data: &[(f64, f64)],
    chart_type: &str,
    color: Value,
    label: Value,
) -> Value {
    let data_arr: Value = data
        .iter()
        .map(|&(x, y)| -> Value { (x, y).into() })
        .collect::<Vec<Value>>()
        .into();
    [
        (literal!("chart_type"), Value::String(chart_type.into())),
        (literal!("color"), color),
        (literal!("data"), data_arr),
        (literal!("label"), label),
    ]
    .into()
}

#[test]
fn dataset_parses() -> Result<()> {
    let v = dataset_val(
        &[(1.0, 2.0), (3.0, 4.0)],
        "Line",
        Value::Null,
        Value::String("test".into()),
    );
    let ds = Dataset::from_value(v)?;
    assert_eq!(ds.data.len(), 2);
    assert_eq!(ds.data[0], (1.0, 2.0));
    assert!(matches!(ds.chart_type, ChartType::Line));
    assert!(ds.color.is_none());
    assert_eq!(ds.label.as_deref(), Some("test"));
    Ok(())
}

#[test]
fn dataset_with_color() -> Result<()> {
    let color: Value = [
        (literal!("a"), 1.0f64),
        (literal!("b"), 0.0f64),
        (literal!("g"), 1.0f64),
        (literal!("r"), 0.0f64),
    ]
    .into();
    let v = dataset_val(&[(0.0, 0.0)], "Scatter", color, Value::Null);
    let ds = Dataset::from_value(v)?;
    assert!(ds.color.is_some());
    assert!(matches!(ds.chart_type, ChartType::Scatter));
    assert!(ds.label.is_none());
    Ok(())
}
