use super::GuiTestHarness;
use crate::widgets::canvas::{CanvasShape, CanvasW, PathSegment};
use anyhow::Result;
use arcstr::ArcStr;
use compact_str::format_compact;
use graphix_rt::NoExt;
use netidx::publisher::{FromValue, Value};

async fn canvas_harness(
    expected_count: usize,
    shapes_expr: &str,
) -> Result<GuiTestHarness> {
    let code = format_compact!(
        "use gui::{{color, canvas::canvas}};\n\
         let result = canvas(#width: &`Fill, #height: &`Fixed(200.0), &[{shapes_expr}])"
    );
    let h = GuiTestHarness::new(&code).await?;
    let canvas = h.root.w.as_any().downcast_ref::<CanvasW<NoExt>>().expect("a canvas");
    assert_eq!(canvas.shape_count(), Some(expected_count));
    Ok(h)
}

fn invalid_geometry(h: &GuiTestHarness) -> bool {
    h.root.w.as_any().downcast_ref::<CanvasW<NoExt>>().unwrap().has_invalid_geometry()
}

// XCR claude for claude: [test-gap/medium] [test-gap/medium] Every canvas rendering test
// asserts only that no shape was flagged as invalid geometry. That flag is set only
// when draw_shape returns None, and no test here screenshots the canvas, so a draw that
// fills with the wrong colour, misplaces a shape, or draws nothing at all passes every
// test here, including "the others draw" in a_non_finite_shape_is_left_out. Check
// pixels with h.screenshot: e.g. rect_with_fill's grey at (30,35) inside its 10,20
// 40x30 rectangle, and a red pixel on the valid line of a_non_finite_shape_is_left_out.
// Found by reading; not run. (tests-gui-a-01)
// 2026-10-26 claude: Every canvas rendering test now screenshots a 200x200 frame (draws
// in canvas_test.rs) and probes pixels: each shape's colour inside it and black
// outside, circle_with_neither an all-black frame, path_and_curves_render spread so
// each of its six shapes has its own probe, text_renders white text lit in its box
// only. A circle fill dropped or a rect fill shifted 3 px fails circle_with_fill_only,
// circle_with_both and rect_with_fill. The probes found that the ±3e38 line of
// canvas_huge_line_keeps_finite_endpoints draws nothing (a ±1e12 one draws), so that
// test asserts only that it is not skipped; new CR (canvas-huge-line-01) at draw_shape.
// canvas_arc_to_reversal_draws asserts its sliver covers no pixel.
/// The side of the square frame the tests draw.
const SIDE: u32 = 200;

const RED: [u8; 3] = [255, 0, 0];
const GREEN: [u8; 3] = [0, 255, 0];
const BLUE: [u8; 3] = [0, 0, 255];
const WHITE: [u8; 3] = [255, 255, 255];
const BLACK: [u8; 3] = [0, 0, 0];

/// One frame over black, every shape drawn (none skipped as invalid
/// geometry), each `(x, y, rgb)` of `probes` that colour; the frame as RGBA.
fn draws(h: &mut GuiTestHarness, probes: &[(u32, u32, [u8; 3])]) -> Vec<u8> {
    let px = h.screenshot(SIDE, SIDE);
    assert!(!invalid_geometry(h), "a shape was skipped");
    for &(x, y, rgb) in probes {
        let at = &px[((y * SIDE + x) * 4) as usize..][..3];
        let near = at.iter().zip(rgb).all(|(a, b)| a.abs_diff(b) <= 8);
        assert!(near, "pixel ({x}, {y}) is {at:?}, not {rgb:?}");
    }
    px
}

#[tokio::test(flavor = "current_thread")]
async fn line_renders() -> Result<()> {
    let mut h = canvas_harness(
        1,
        concat!(
            "`Line({from: {x: 0.0, y: 0.0}, to: {x: 100.0, y: 50.0}, ",
            "color: color(#r: 1.0, #g: 0.0, #b: 0.0, #a: 1.0)$, width: 2.5})",
        ),
    )
    .await?;
    draws(&mut h, &[(50, 25, RED), (50, 60, BLACK)]);
    Ok(())
}

/// A shape with a non-finite field is left out; the others draw.
#[tokio::test(flavor = "current_thread")]
async fn a_non_finite_shape_is_left_out() -> Result<()> {
    let mut h = canvas_harness(
        1,
        concat!(
            "`Line({from: {x: 0.0, y: 0.0}, to: {x: 100.0, y: 50.0}, ",
            "color: color(#r: 1.0, #g: 0.0, #b: 0.0, #a: 1.0)$, width: 2.5}), ",
            "`Line({from: {x: 0.0, y: 0.0 / 0.0}, to: {x: 100.0, y: 50.0}, ",
            "color: color(#r: 1.0, #g: 0.0, #b: 0.0, #a: 1.0)$, width: 2.5})",
        ),
    )
    .await?;
    draws(&mut h, &[(50, 25, RED)]);
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn circle_with_fill_only() -> Result<()> {
    let mut h = canvas_harness(
        1,
        concat!(
            "`Circle({center: {x: 10.0, y: 20.0}, radius: 25.0, ",
            "fill: color(#r: 0.0, #g: 1.0, #b: 0.0, #a: 1.0)$, stroke: null})",
        ),
    )
    .await?;
    draws(&mut h, &[(10, 20, GREEN), (30, 30, GREEN), (40, 40, BLACK)]);
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn circle_with_stroke_only() -> Result<()> {
    let mut h = canvas_harness(1, concat!(
        "`Circle({center: {x: 0.0, y: 0.0}, radius: 10.0, ",
        "fill: null, stroke: {color: color(#r: 0.0, #g: 0.0, #b: 1.0, #a: 1.0)$, width: 3.0}})",
    ))
    .await?;
    draws(&mut h, &[(7, 7, BLUE), (2, 2, BLACK)]);
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn circle_with_both() -> Result<()> {
    let mut h = canvas_harness(
        1,
        concat!(
            "`Circle({center: {x: 0.0, y: 0.0}, radius: 5.0, ",
            "fill: color(#r: 1.0, #g: 1.0, #b: 1.0, #a: 1.0)$, ",
            "stroke: {color: color(#r: 0.0, #g: 0.0, #b: 0.0, #a: 1.0)$, width: 1.0}})",
        ),
    )
    .await?;
    draws(&mut h, &[(1, 1, WHITE)]);
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn circle_with_neither() -> Result<()> {
    let mut h = canvas_harness(
        1,
        "`Circle({center: {x: 0.0, y: 0.0}, radius: 5.0, fill: null, stroke: null})",
    )
    .await?;
    let px = draws(&mut h, &[]);
    assert!(px.chunks_exact(4).all(|p| p[..3] == BLACK), "nothing is drawn");
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn rect_with_fill() -> Result<()> {
    let mut h = canvas_harness(
        1,
        concat!(
            "`Rect({top_left: {x: 10.0, y: 20.0}, ",
            "size: {width: 40.0, height: 30.0}, ",
            "fill: color(#r: 0.5, #g: 0.5, #b: 0.5, #a: 1.0)$, stroke: null})",
        ),
    )
    .await?;
    let grey = [128, 128, 128];
    draws(&mut h, &[(10, 20, grey), (49, 49, grey), (50, 35, BLACK), (30, 50, BLACK)]);
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn rect_with_stroke() -> Result<()> {
    let mut h = canvas_harness(1, concat!(
        "`Rect({top_left: {x: 0.0, y: 0.0}, ",
        "size: {width: 10.0, height: 10.0}, ",
        "fill: null, stroke: {color: color(#r: 1.0, #g: 0.0, #b: 0.0, #a: 1.0)$, width: 2.0}})",
    ))
    .await?;
    draws(&mut h, &[(10, 5, RED), (5, 5, BLACK)]);
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn text_renders() -> Result<()> {
    let mut h = canvas_harness(
        1,
        concat!(
            r#"`Text({content: "hello", position: {x: 5.0, y: 10.0}, "#,
            "color: color(#r: 1.0, #g: 1.0, #b: 1.0, #a: 1.0)$, size: 16.0})",
        ),
    )
    .await?;
    let px = draws(&mut h, &[]);
    let lit = |x: u32, y: u32| px[((y * SIDE + x) * 4) as usize] > 128;
    let inside = (5..80).flat_map(|x| (5..35).map(move |y| (x, y)));
    assert!(inside.filter(|&(x, y)| lit(x, y)).count() > 20, "the text is drawn");
    assert!(!lit(150, 150) && !lit(5, 100), "only the text is drawn");
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
    let mut h = canvas_harness(6, concat!(
        "`Path({segments: [`MoveTo({x: 10.0, y: 10.0}), `LineTo({x: 60.0, y: 10.0}), ",
        "`BezierTo({control_a: {x: 70.0, y: 20.0}, control_b: {x: 70.0, y: 40.0}, to: {x: 60.0, y: 50.0}}), ",
        "`QuadraticTo({control: {x: 35.0, y: 60.0}, to: {x: 10.0, y: 50.0}}), ",
        "`ArcTo({a: {x: 0.0, y: 30.0}, b: {x: 10.0, y: 10.0}, radius: 5.0}), `Close], ",
        "fill: null, stroke: {color: color(#r: 1.0, #g: 1.0, #b: 1.0, #a: 1.0)$, width: 3.0}}), ",
        "`RoundedRect({top_left: {x: 100.0, y: 10.0}, size: {width: 40.0, height: 20.0}, radius: 4.0, ",
        "fill: color(#r: 1.0, #g: 0.0, #b: 0.0, #a: 1.0)$, stroke: null}), ",
        "`Arc({center: {x: 30.0, y: 120.0}, radius: 20.0, start_angle: 0.0, end_angle: 1.0, ",
        "stroke: {color: color(#r: 0.0, #g: 1.0, #b: 0.0, #a: 1.0)$, width: 3.0}}), ",
        "`Ellipse({center: {x: 120.0, y: 120.0}, radii: {x: 30.0, y: 15.0}, rotation: 0.0, ",
        "start_angle: 0.0, end_angle: 6.3, fill: color(#r: 0.0, #g: 0.0, #b: 1.0, #a: 1.0)$, stroke: null}), ",
        "`BezierCurve({from: {x: 10.0, y: 170.0}, control_a: {x: 60.0, y: 170.0}, ",
        "control_b: {x: 110.0, y: 170.0}, to: {x: 160.0, y: 170.0}, ",
        "color: color(#r: 1.0, #g: 1.0, #b: 0.0, #a: 1.0)$, width: 3.0}), ",
        "`QuadraticCurve({from: {x: 10.0, y: 190.0}, control: {x: 85.0, y: 190.0}, to: {x: 160.0, y: 190.0}, ",
        "color: color(#r: 0.0, #g: 1.0, #b: 1.0, #a: 1.0)$, width: 3.0})",
    ))
.await?;
    draws(
        &mut h,
        &[
            (35, 10, WHITE),
            (35, 30, BLACK),
            (120, 20, RED),
            (47, 129, GREEN),
            (120, 120, BLUE),
            (85, 170, [255, 255, 0]),
            (85, 190, [0, 255, 255]),
        ],
    );
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

/// An `ArcTo` that doubles back bounds a sliver: no pixel, and no shape
/// skipped.
#[tokio::test(flavor = "current_thread")]
async fn canvas_arc_to_reversal_draws() -> Result<()> {
    let mut h = canvas_harness(1,
        "`Path({segments: [`MoveTo({x: 0.0, y: 0.0}), \
         `ArcTo({a: {x: 100.0, y: 0.0}, b: {x: 0.0, y: 0.00001}, radius: 10.0}), `Close], \
         fill: color(#r: 1.0)$, stroke: null})",
    ).await?;
    let px = draws(&mut h, &[]);
    assert!(px.chunks_exact(4).all(|p| p[..3] == BLACK), "the sliver covers no pixel");
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn canvas_overflowing_circle_is_skipped() -> Result<()> {
    let mut h = canvas_harness(1,
        "`Circle({center: {x: 300000000000000000000000000000000000000.0, y: 50.0}, \
         radius: 300000000000000000000000000000000000000.0, fill: color(#g: 1.0)$, stroke: null})",
    ).await?;
    h.render()?;
    assert!(invalid_geometry(&h));
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn canvas_huge_line_keeps_finite_endpoints() -> Result<()> {
    let mut h = canvas_harness(
        1,
        "`Line({from: {x: -300000000000000000000000000000000000000.0, y: 10.0}, \
         to: {x: 300000000000000000000000000000000000000.0, y: 20.0}, \
         color: color(#b: 1.0)$, width: 2.0})",
    )
    .await?;
    draws(&mut h, &[]);
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn canvas_overflowing_filled_rectangle_is_skipped() -> Result<()> {
    let mut h = canvas_harness(
        1,
        "`Rect({top_left: {x: 300000000000000000000000000000000000000.0, y: 0.0}, \
         size: {width: 300000000000000000000000000000000000000.0, height: 10.0}, \
         fill: color(#g: 1.0)$, stroke: null})",
    )
    .await?;
    h.render()?;
    assert!(invalid_geometry(&h));
    Ok(())
}
