use super::GuiTestHarness;
use crate::widgets::canvas::{CanvasShape, PathSegment};
use anyhow::Result;
use arcstr::ArcStr;
use netidx::publisher::{FromValue, Value};

async fn canvas_harness(shapes_expr: &str) -> Result<GuiTestHarness> {
    let code = format!(
        "use gui::*;\nuse gui::canvas::{{self, *}};\n\
         let result = canvas(#width: &`Fill, #height: &`Fixed(200.0), &[{shapes_expr}])"
    );
    GuiTestHarness::new(&code).await
}

#[tokio::test(flavor = "current_thread")]
async fn line_renders() -> Result<()> {
    let h = canvas_harness(concat!(
        "`Line({from: {x: 0.0, y: 0.0}, to: {x: 100.0, y: 50.0}, ",
        "color: color(#r: 1.0, #g: 0.0, #b: 0.0, #a: 1.0)$, width: 2.5})",
    ))
    .await?;
    let _ = h.view();
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn circle_with_fill_only() -> Result<()> {
    let h = canvas_harness(concat!(
        "`Circle({center: {x: 10.0, y: 20.0}, radius: 25.0, ",
        "fill: color(#r: 0.0, #g: 1.0, #b: 0.0, #a: 1.0)$, stroke: null})",
    ))
    .await?;
    let _ = h.view();
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn circle_with_stroke_only() -> Result<()> {
    let h = canvas_harness(concat!(
        "`Circle({center: {x: 0.0, y: 0.0}, radius: 10.0, ",
        "fill: null, stroke: {color: color(#r: 0.0, #g: 0.0, #b: 1.0, #a: 1.0)$, width: 3.0}})",
    ))
    .await?;
    let _ = h.view();
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn circle_with_both() -> Result<()> {
    let h = canvas_harness(concat!(
        "`Circle({center: {x: 0.0, y: 0.0}, radius: 5.0, ",
        "fill: color(#r: 1.0, #g: 1.0, #b: 1.0, #a: 1.0)$, ",
        "stroke: {color: color(#r: 0.0, #g: 0.0, #b: 0.0, #a: 1.0)$, width: 1.0}})",
    ))
    .await?;
    let _ = h.view();
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn circle_with_neither() -> Result<()> {
    let h = canvas_harness(
        "`Circle({center: {x: 0.0, y: 0.0}, radius: 5.0, fill: null, stroke: null})",
    )
    .await?;
    let _ = h.view();
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn rect_with_fill() -> Result<()> {
    let h = canvas_harness(concat!(
        "`Rect({top_left: {x: 10.0, y: 20.0}, ",
        "size: {width: 40.0, height: 30.0}, ",
        "fill: color(#r: 0.5, #g: 0.5, #b: 0.5, #a: 1.0)$, stroke: null})",
    ))
    .await?;
    let _ = h.view();
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn rect_with_stroke() -> Result<()> {
    let h = canvas_harness(concat!(
        "`Rect({top_left: {x: 0.0, y: 0.0}, ",
        "size: {width: 10.0, height: 10.0}, ",
        "fill: null, stroke: {color: color(#r: 1.0, #g: 0.0, #b: 0.0, #a: 1.0)$, width: 2.0}})",
    ))
    .await?;
    let _ = h.view();
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn text_renders() -> Result<()> {
    let h = canvas_harness(concat!(
        r#"`Text({content: "hello", position: {x: 5.0, y: 10.0}, "#,
        "color: color(#r: 0.0, #g: 0.0, #b: 0.0, #a: 1.0)$, size: 16.0})",
    ))
    .await?;
    let _ = h.view();
    Ok(())
}

#[test]
fn invalid_tag_errors() {
    let v: Value = (ArcStr::from("Hexagon"), Value::Null).into();
    assert!(CanvasShape::from_value(v).is_err());
}

#[test]
fn malformed_line_errors() {
    let payload: Value = [(ArcStr::from("color"), Value::Null)].into();
    let v: Value = (ArcStr::from("Line"), payload).into();
    assert!(CanvasShape::from_value(v).is_err());
}

#[tokio::test(flavor = "current_thread")]
async fn path_and_curves_render() -> Result<()> {
    let h = canvas_harness(concat!(
        "`Path({segments: [`MoveTo({x: 0.0, y: 0.0}), `LineTo({x: 10.0, y: 0.0}), ",
        "`BezierTo({control_a: {x: 1.0, y: 2.0}, control_b: {x: 3.0, y: 4.0}, to: {x: 5.0, y: 6.0}}), ",
        "`QuadraticTo({control: {x: 1.0, y: 1.0}, to: {x: 2.0, y: 2.0}}), ",
        "`ArcTo({a: {x: 0.0, y: 0.0}, b: {x: 1.0, y: 1.0}, radius: 3.0}), `Close], ",
        "fill: null, stroke: {color: color(#r: 0.0, #g: 0.0, #b: 0.0, #a: 1.0)$, width: 1.0}}), ",
        "`RoundedRect({top_left: {x: 0.0, y: 0.0}, size: {width: 4.0, height: 2.0}, radius: 1.0, ",
        "fill: color(#r: 1.0, #g: 0.0, #b: 0.0, #a: 1.0)$, stroke: null}), ",
        "`Arc({center: {x: 0.0, y: 0.0}, radius: 2.0, start_angle: 0.0, end_angle: 1.0, ",
        "stroke: {color: color(#r: 0.0, #g: 1.0, #b: 0.0, #a: 1.0)$, width: 1.0}}), ",
        "`Ellipse({center: {x: 0.0, y: 0.0}, radii: {x: 2.0, y: 1.0}, rotation: 0.0, ",
        "start_angle: 0.0, end_angle: 1.0, fill: null, stroke: null}), ",
        "`BezierCurve({from: {x: 0.0, y: 0.0}, control_a: {x: 1.0, y: 0.0}, ",
        "control_b: {x: 1.0, y: 1.0}, to: {x: 2.0, y: 2.0}, ",
        "color: color(#r: 0.0, #g: 0.0, #b: 1.0, #a: 1.0)$, width: 1.0}), ",
        "`QuadraticCurve({from: {x: 0.0, y: 0.0}, control: {x: 1.0, y: 0.0}, to: {x: 2.0, y: 2.0}, ",
        "color: color(#r: 0.0, #g: 0.0, #b: 1.0, #a: 1.0)$, width: 1.0})",
    ))
    .await?;
    let _ = h.view();
    Ok(())
}

#[test]
fn decoded_fields() -> Result<()> {
    let v: Value = r#"["Rect", [["fill", null], ["size", [["height", 30.0], ["width", 40.0]]], ["stroke", null], ["top_left", [["x", 10.0], ["y", 20.0]]]]]"#
        .parse()?;
    match CanvasShape::from_value(v)? {
        CanvasShape::Rect { top_left, size, fill: None, stroke: None } => {
            assert_eq!((top_left.0.x, top_left.0.y), (10.0, 20.0));
            assert_eq!((size.0.width, size.0.height), (40.0, 30.0));
        }
        s => panic!("expected a Rect, got {s:?}"),
    }
    let v: Value = r#"["Path", [["fill", null], ["segments", [["MoveTo", [["x", 1.0], ["y", 2.0]]], "Close"]], ["stroke", null]]]"#
        .parse()?;
    match CanvasShape::from_value(v)? {
        CanvasShape::Path { segments, fill: None, stroke: None } => match &segments[..] {
            [PathSegment::MoveTo(p), PathSegment::Close] => {
                assert_eq!((p.0.x, p.0.y), (1.0, 2.0))
            }
            s => panic!("unexpected segments {s:?}"),
        },
        s => panic!("expected a Path, got {s:?}"),
    }
    Ok(())
}
