use crate::widgets::canvas::CanvasShape;
use anyhow::Result;
use arcstr::{literal, ArcStr};
use iced_core::{Point, Size};
use netidx::publisher::{FromValue, Value};

/// Build a Color value as graphix would encode it: sorted fields {a, b, g, r}.
fn color_val(r: f64, g: f64, b: f64, a: f64) -> Value {
    [(literal!("a"), a), (literal!("b"), b), (literal!("g"), g), (literal!("r"), r)]
        .into()
}

/// Build a Point value: sorted fields {x, y}.
fn point_val(x: f64, y: f64) -> Value {
    [(literal!("x"), x), (literal!("y"), y)].into()
}

/// Build a stroke value: sorted fields {color, width}.
fn stroke_val(r: f64, g: f64, b: f64, a: f64, width: f64) -> Value {
    [(literal!("color"), color_val(r, g, b, a)), (literal!("width"), Value::F64(width))]
        .into()
}

/// Build a tagged variant value: (tag, payload).
fn variant(tag: &str, payload: Value) -> Value {
    (ArcStr::from(tag), payload).into()
}

// ── Line ────────────────────────────────────────────────────────────

#[test]
fn line_parses() -> Result<()> {
    let v = variant(
        "Line",
        [
            (literal!("color"), color_val(1.0, 0.0, 0.0, 1.0)),
            (literal!("from"), point_val(0.0, 0.0)),
            (literal!("to"), point_val(100.0, 50.0)),
            (literal!("width"), Value::F64(2.5)),
        ]
        .into(),
    );
    let shape = CanvasShape::from_value(v)?;
    match shape {
        CanvasShape::Line { from, to, color, width } => {
            assert_eq!(from, Point::new(0.0, 0.0));
            assert_eq!(to, Point::new(100.0, 50.0));
            assert!((color.r - 1.0).abs() < 1e-5);
            assert!((color.g).abs() < 1e-5);
            assert!((width - 2.5).abs() < 1e-5);
        }
        other => panic!("expected Line, got {other:?}"),
    }
    Ok(())
}

// ── Circle ──────────────────────────────────────────────────────────

#[test]
fn circle_with_fill_only() -> Result<()> {
    let v = variant(
        "Circle",
        [
            (literal!("center"), point_val(10.0, 20.0)),
            (literal!("fill"), color_val(0.0, 1.0, 0.0, 1.0)),
            (literal!("radius"), Value::F64(25.0)),
            (literal!("stroke"), Value::Null),
        ]
        .into(),
    );
    let shape = CanvasShape::from_value(v)?;
    match shape {
        CanvasShape::Circle { center, radius, fill, stroke } => {
            assert_eq!(center, Point::new(10.0, 20.0));
            assert!((radius - 25.0).abs() < 1e-5);
            assert!(fill.is_some());
            assert!(stroke.is_none());
        }
        other => panic!("expected Circle, got {other:?}"),
    }
    Ok(())
}

#[test]
fn circle_with_stroke_only() -> Result<()> {
    let v = variant(
        "Circle",
        [
            (literal!("center"), point_val(0.0, 0.0)),
            (literal!("fill"), Value::Null),
            (literal!("radius"), Value::F64(10.0)),
            (literal!("stroke"), stroke_val(0.0, 0.0, 1.0, 1.0, 3.0)),
        ]
        .into(),
    );
    let shape = CanvasShape::from_value(v)?;
    match shape {
        CanvasShape::Circle { fill, stroke, .. } => {
            assert!(fill.is_none());
            let (c, w) = stroke.unwrap();
            assert!((c.b - 1.0).abs() < 1e-5);
            assert!((w - 3.0).abs() < 1e-5);
        }
        other => panic!("expected Circle, got {other:?}"),
    }
    Ok(())
}

#[test]
fn circle_with_both() -> Result<()> {
    let v = variant(
        "Circle",
        [
            (literal!("center"), point_val(0.0, 0.0)),
            (literal!("fill"), color_val(1.0, 1.0, 1.0, 1.0)),
            (literal!("radius"), Value::F64(5.0)),
            (literal!("stroke"), stroke_val(0.0, 0.0, 0.0, 1.0, 1.0)),
        ]
        .into(),
    );
    let shape = CanvasShape::from_value(v)?;
    match shape {
        CanvasShape::Circle { fill, stroke, .. } => {
            assert!(fill.is_some());
            assert!(stroke.is_some());
        }
        other => panic!("expected Circle, got {other:?}"),
    }
    Ok(())
}

#[test]
fn circle_with_neither() -> Result<()> {
    let v = variant(
        "Circle",
        [
            (literal!("center"), point_val(0.0, 0.0)),
            (literal!("fill"), Value::Null),
            (literal!("radius"), Value::F64(5.0)),
            (literal!("stroke"), Value::Null),
        ]
        .into(),
    );
    let shape = CanvasShape::from_value(v)?;
    match shape {
        CanvasShape::Circle { fill, stroke, .. } => {
            assert!(fill.is_none());
            assert!(stroke.is_none());
        }
        other => panic!("expected Circle, got {other:?}"),
    }
    Ok(())
}

// ── Rect ────────────────────────────────────────────────────────────

#[test]
fn rect_with_fill() -> Result<()> {
    let v = variant(
        "Rect",
        [
            (literal!("fill"), color_val(0.5, 0.5, 0.5, 1.0)),
            (
                literal!("size"),
                [(literal!("height"), 30.0f64), (literal!("width"), 40.0f64)].into(),
            ),
            (literal!("stroke"), Value::Null),
            (literal!("top_left"), point_val(10.0, 20.0)),
        ]
        .into(),
    );
    let shape = CanvasShape::from_value(v)?;
    match shape {
        CanvasShape::Rect { top_left, size, fill, stroke } => {
            assert_eq!(top_left, Point::new(10.0, 20.0));
            assert_eq!(size, Size::new(40.0, 30.0));
            assert!(fill.is_some());
            assert!(stroke.is_none());
        }
        other => panic!("expected Rect, got {other:?}"),
    }
    Ok(())
}

#[test]
fn rect_with_stroke() -> Result<()> {
    let v = variant(
        "Rect",
        [
            (literal!("fill"), Value::Null),
            (
                literal!("size"),
                [(literal!("height"), 10.0f64), (literal!("width"), 10.0f64)].into(),
            ),
            (literal!("stroke"), stroke_val(1.0, 0.0, 0.0, 1.0, 2.0)),
            (literal!("top_left"), point_val(0.0, 0.0)),
        ]
        .into(),
    );
    let shape = CanvasShape::from_value(v)?;
    match shape {
        CanvasShape::Rect { fill, stroke, .. } => {
            assert!(fill.is_none());
            assert!(stroke.is_some());
        }
        other => panic!("expected Rect, got {other:?}"),
    }
    Ok(())
}

// ── Text ────────────────────────────────────────────────────────────

#[test]
fn text_parses() -> Result<()> {
    let v = variant(
        "Text",
        [
            (literal!("color"), color_val(0.0, 0.0, 0.0, 1.0)),
            (literal!("content"), Value::String("hello".into())),
            (literal!("position"), point_val(5.0, 10.0)),
            (literal!("size"), Value::F64(16.0)),
        ]
        .into(),
    );
    let shape = CanvasShape::from_value(v)?;
    match shape {
        CanvasShape::Text { content, position, color, size } => {
            assert_eq!(content, "hello");
            assert_eq!(position, Point::new(5.0, 10.0));
            assert!((color.a - 1.0).abs() < 1e-5);
            assert!((size - 16.0).abs() < 1e-5);
        }
        other => panic!("expected Text, got {other:?}"),
    }
    Ok(())
}

// ── Error cases ─────────────────────────────────────────────────────

#[test]
fn invalid_tag_errors() {
    let v = variant("Hexagon", Value::Null);
    assert!(CanvasShape::from_value(v).is_err());
}

#[test]
fn malformed_line_errors() {
    // Wrong number of fields
    let v = variant("Line", [(literal!("color"), color_val(1.0, 0.0, 0.0, 1.0))].into());
    assert!(CanvasShape::from_value(v).is_err());
}
