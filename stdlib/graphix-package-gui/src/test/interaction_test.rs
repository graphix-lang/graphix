use super::{expect_call, expect_call_with_args, has_message, InteractionHarness};
use crate::widgets::Message;
use anyhow::Result;
use iced_core::{Point, Size};
use netidx::publisher::Value;

/// Standard widget imports for interaction tests.
const IMPORTS: &str = "\
use gui;\n\
use gui::text;\n\
use gui::button;\n\
use gui::checkbox;\n\
use gui::toggler;\n\
use gui::text_input;\n\
use gui::slider;\n\
use gui::radio;\n\
use gui::pick_list;\n\
use gui::text_editor;\n\
use gui::vertical_slider;\n\
use gui::mouse_area";

async fn harness(widget_expr: &str) -> Result<InteractionHarness> {
    let code = format!("{IMPORTS};\nlet result = {widget_expr}");
    InteractionHarness::new(&code).await
}

/// Click near the origin — widgets use Shrink sizing and are laid out
/// at (0,0), so clicking at the viewport center misses them.
const WIDGET_HIT: Point = Point::new(10.0, 10.0);

// ── Button ──────────────────────────────────────────────────────────

#[tokio::test(flavor = "current_thread")]
async fn button_click_produces_call() -> Result<()> {
    let mut h = harness("button(#on_press: |_| null, &text(&\"Click me\"))").await?;
    let msgs = h.click(WIDGET_HIT);
    expect_call(&msgs);
    Ok(())
}

// Note: the graphix `button` function always provides a default
// `on_press: |_| null`, so there is no way to create a button without
// an on_press via the graphix function.

// ── Checkbox ────────────────────────────────────────────────────────

// Note: checkbox/toggler/slider/radio interactions produce Call messages
// via on_toggle/on_change/on_select callbacks. Without a callback, the
// widget is display-only. These tests verify both the no-callback case
// (no panic) and the callback case (produces Call).

#[tokio::test(flavor = "current_thread")]
async fn checkbox_click_no_panic() -> Result<()> {
    let code = format!(
        "{IMPORTS};\n\
         let checked = &false;\n\
         let result = checkbox(#label: &\"Toggle me\", checked)"
    );
    let mut h = InteractionHarness::new(&code).await?;
    let _ = h.view();
    let _ = h.click(WIDGET_HIT);
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn checkbox_toggle_produces_call() -> Result<()> {
    let code = format!(
        "{IMPORTS};\n\
         let result = checkbox(#label: &\"Toggle me\", #on_toggle: |v| null, &false)"
    );
    let mut h = InteractionHarness::new(&code).await?;
    let msgs = h.click(WIDGET_HIT);
    expect_call_with_args(&msgs, |args| {
        matches!(args.iter().next(), Some(Value::Bool(_)))
    });
    Ok(())
}

// ── Toggler ─────────────────────────────────────────────────────────

#[tokio::test(flavor = "current_thread")]
async fn toggler_click_no_panic() -> Result<()> {
    let code = format!(
        "{IMPORTS};\n\
         let toggled = &false;\n\
         let result = toggler(#label: &\"Dark mode\", toggled)"
    );
    let mut h = InteractionHarness::new(&code).await?;
    let _ = h.view();
    let _ = h.click(WIDGET_HIT);
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn toggler_toggle_produces_call() -> Result<()> {
    let code = format!(
        "{IMPORTS};\n\
         let result = toggler(#label: &\"Dark mode\", #on_toggle: |v| null, &false)"
    );
    let mut h = InteractionHarness::new(&code).await?;
    let msgs = h.click(WIDGET_HIT);
    expect_call_with_args(&msgs, |args| {
        matches!(args.iter().next(), Some(Value::Bool(_)))
    });
    Ok(())
}

// ── Slider ──────────────────────────────────────────────────────────

#[tokio::test(flavor = "current_thread")]
async fn slider_click_no_panic() -> Result<()> {
    let code = format!(
        "{IMPORTS};\n\
         let val = &50.0;\n\
         let result = slider(#min: &0.0, #max: &100.0, val)"
    );
    let mut h = InteractionHarness::with_viewport(&code, Size::new(200.0, 22.0)).await?;
    let _ = h.click(Point::new(150.0, 10.0));
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn slider_drag_no_panic() -> Result<()> {
    let code = format!(
        "{IMPORTS};\n\
         let val = &50.0;\n\
         let result = slider(#min: &0.0, #max: &100.0, val)"
    );
    let mut h = InteractionHarness::with_viewport(&code, Size::new(200.0, 22.0)).await?;
    let from = Point::new(100.0, 10.0);
    let _ = h.drag_horizontal(from, 180.0, 5);
    Ok(())
}

// ── VerticalSlider ──────────────────────────────────────────────────

#[tokio::test(flavor = "current_thread")]
async fn vertical_slider_click_no_panic() -> Result<()> {
    let code = format!(
        "{IMPORTS};\n\
         let val = &50.0;\n\
         let result = vertical_slider(#min: &0.0, #max: &100.0, val)"
    );
    let mut h = InteractionHarness::with_viewport(&code, Size::new(22.0, 200.0)).await?;
    let _ = h.click(Point::new(10.0, 50.0));
    Ok(())
}

// ── TextInput ───────────────────────────────────────────────────────

#[tokio::test(flavor = "current_thread")]
async fn text_input_click_and_type_no_panic() -> Result<()> {
    let code = format!(
        "{IMPORTS};\n\
         let val = &\"\";\n\
         let result = text_input(#placeholder: &\"Type here\", val)"
    );
    let mut h = InteractionHarness::new(&code).await?;
    h.click(WIDGET_HIT);
    let _ = h.type_text("abc");
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn text_input_submit_produces_call() -> Result<()> {
    let code = format!(
        "{IMPORTS};\n\
         let val = &\"\";\n\
         let result = text_input(\
             #placeholder: &\"Search\", \
             #on_submit: |_| null, \
             val)"
    );
    let mut h = InteractionHarness::new(&code).await?;
    h.click(WIDGET_HIT);
    h.type_text("query");
    let msgs = h.press_key(iced_core::keyboard::key::Named::Enter);
    assert!(
        has_message(&msgs, |m| matches!(m, Message::Call(_, _))),
        "pressing Enter should produce a Call message"
    );
    Ok(())
}

// ── Radio ───────────────────────────────────────────────────────────

#[tokio::test(flavor = "current_thread")]
async fn radio_click_no_panic() -> Result<()> {
    let code = format!(
        "{IMPORTS};\n\
         let sel = &\"none\";\n\
         let result = radio(#label: &\"Option A\", #selected: sel, &\"option_a\")"
    );
    let mut h = InteractionHarness::new(&code).await?;
    let _ = h.view();
    let _ = h.click(WIDGET_HIT);
    Ok(())
}

// ── PickList ────────────────────────────────────────────────────────

#[tokio::test(flavor = "current_thread")]
async fn pick_list_basic() -> Result<()> {
    let mut h = harness(
        "pick_list(\
            #selected: &\"Red\",\
            #placeholder: &\"Choose...\",\
            &[\"Red\", \"Green\", \"Blue\"])",
    )
    .await?;
    let _ = h.view();
    let _ = h.click(WIDGET_HIT);
    Ok(())
}

// ── MouseArea ───────────────────────────────────────────────────────

#[tokio::test(flavor = "current_thread")]
async fn mouse_area_press_produces_call() -> Result<()> {
    let mut h =
        harness("mouse_area(#on_press: |_| null, &text(&\"Click zone\"))").await?;
    let msgs = h.click(WIDGET_HIT);
    assert!(
        has_message(&msgs, |m| matches!(m, Message::Call(_, _))),
        "mouse_area on_press should produce Call"
    );
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn mouse_area_release_produces_call() -> Result<()> {
    let mut h =
        harness("mouse_area(#on_release: |_| null, &text(&\"Click zone\"))").await?;
    let msgs = h.click(WIDGET_HIT);
    assert!(
        has_message(&msgs, |m| matches!(m, Message::Call(_, _))),
        "mouse_area on_release should produce Call"
    );
    Ok(())
}

// ── TextEditor ──────────────────────────────────────────────────────

#[tokio::test(flavor = "current_thread")]
async fn text_editor_click_and_type_no_panic() -> Result<()> {
    let code = format!(
        "{IMPORTS};\n\
         let val = &\"\";\n\
         let result = text_editor(#placeholder: &\"Edit...\", val)"
    );
    let mut h = InteractionHarness::with_viewport(&code, Size::new(300.0, 100.0)).await?;
    h.click(WIDGET_HIT);
    let _ = h.type_text("hello");
    Ok(())
}
