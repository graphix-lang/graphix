use crate::widgets::canvas::CanvasShape;
use super::GuiTestHarness;
use anyhow::Result;
use arcstr::ArcStr;
use netidx::publisher::{FromValue, Value};

async fn canvas_harness(shapes_expr: &str) -> Result<GuiTestHarness> {
    let code = format!(
        "use gui;\nuse gui::canvas;\n\
         let result = canvas(#width: &`Fill, #height: &`Fixed(200.0), &[{shapes_expr}])"
    );
    GuiTestHarness::new(&code).await
}

// ── Line ────────────────────────────────────────────────────────────

// XCR estokes: Instead of writing out the data structure, we should load the
// actual graphix files we are compiling into the gui library, and then call the
// actual functions that will be used. In other words lets test the actual code
// that runs instead of a facimile of it.
//
// This applies to every other test as well.
#[tokio::test(flavor = "current_thread")]
async fn line_renders() -> Result<()> {
    let h = canvas_harness(concat!(
        "`Line({from: {x: 0.0, y: 0.0}, to: {x: 100.0, y: 50.0}, ",
        "color: {r: 1.0, g: 0.0, b: 0.0, a: 1.0}, width: 2.5})",
    ))
    .await?;
    let _ = h.view();
    Ok(())
}

// ── Circle ──────────────────────────────────────────────────────────

#[tokio::test(flavor = "current_thread")]
async fn circle_with_fill_only() -> Result<()> {
    let h = canvas_harness(concat!(
        "`Circle({center: {x: 10.0, y: 20.0}, radius: 25.0, ",
        "fill: {r: 0.0, g: 1.0, b: 0.0, a: 1.0}, stroke: null})",
    ))
    .await?;
    let _ = h.view();
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn circle_with_stroke_only() -> Result<()> {
    let h = canvas_harness(concat!(
        "`Circle({center: {x: 0.0, y: 0.0}, radius: 10.0, ",
        "fill: null, stroke: {color: {r: 0.0, g: 0.0, b: 1.0, a: 1.0}, width: 3.0}})",
    ))
    .await?;
    let _ = h.view();
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn circle_with_both() -> Result<()> {
    let h = canvas_harness(concat!(
        "`Circle({center: {x: 0.0, y: 0.0}, radius: 5.0, ",
        "fill: {r: 1.0, g: 1.0, b: 1.0, a: 1.0}, ",
        "stroke: {color: {r: 0.0, g: 0.0, b: 0.0, a: 1.0}, width: 1.0}})",
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

// ── Rect ────────────────────────────────────────────────────────────

#[tokio::test(flavor = "current_thread")]
async fn rect_with_fill() -> Result<()> {
    let h = canvas_harness(concat!(
        "`Rect({top_left: {x: 10.0, y: 20.0}, ",
        "size: {width: 40.0, height: 30.0}, ",
        "fill: {r: 0.5, g: 0.5, b: 0.5, a: 1.0}, stroke: null})",
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
        "fill: null, stroke: {color: {r: 1.0, g: 0.0, b: 0.0, a: 1.0}, width: 2.0}})",
    ))
    .await?;
    let _ = h.view();
    Ok(())
}

// ── Text ────────────────────────────────────────────────────────────

#[tokio::test(flavor = "current_thread")]
async fn text_renders() -> Result<()> {
    let h = canvas_harness(concat!(
        r#"`Text({content: "hello", position: {x: 5.0, y: 10.0}, "#,
        "color: {r: 0.0, g: 0.0, b: 0.0, a: 1.0}, size: 16.0})",
    ))
    .await?;
    let _ = h.view();
    Ok(())
}

// ── Error cases (from_value unit tests) ─────────────────────────────

#[test]
fn invalid_tag_errors() {
    let v: Value = (ArcStr::from("Hexagon"), Value::Null).into();
    assert!(CanvasShape::from_value(v).is_err());
}

#[test]
fn malformed_line_errors() {
    // Wrong number of fields
    let payload: Value = [(ArcStr::from("color"), Value::Null)].into();
    let v: Value = (ArcStr::from("Line"), payload).into();
    assert!(CanvasShape::from_value(v).is_err());
}
