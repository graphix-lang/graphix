use super::GuiTestHarness;
use anyhow::Result;

/// All widget imports needed for tests.
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
use gui::progress_bar;\n\
use gui::text_editor;\n\
use gui::row;\n\
use gui::column;\n\
use gui::container;\n\
use gui::scrollable;\n\
use gui::space;\n\
use gui::rule;\n\
use gui::stack;\n\
use gui::tooltip;\n\
use gui::vertical_slider;\n\
use gui::combo_box;\n\
use gui::mouse_area";

/// Helper: compile a simple widget expression.
/// Wraps the code in standard imports + `let result = <expr>`.
async fn harness(widget_expr: &str) -> Result<GuiTestHarness> {
    let code = format!("{IMPORTS};\nlet result = {widget_expr}");
    GuiTestHarness::new(&code).await
}

/// Call view() inside a block so the borrow ends before we do anything else.
macro_rules! view {
    ($h:expr) => {{ let _ = $h.view(); }};
}

// ── Text ────────────────────────────────────────────────────────────

#[tokio::test(flavor = "current_thread")]
async fn text_renders() -> Result<()> {
    let h = harness(r#"text(&"hello world")"#).await?;
    view!(h);
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn text_with_styling() -> Result<()> {
    let h = harness(r#"text(#size: &24.0, #width: &`Fill, &"styled")"#).await?;
    view!(h);
    Ok(())
}

// ── Button ──────────────────────────────────────────────────────────

#[tokio::test(flavor = "current_thread")]
async fn button_renders() -> Result<()> {
    let h = harness(r#"button(#on_press: |_| null, &text(&"Click me"))"#).await?;
    view!(h);
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn button_with_callback() -> Result<()> {
    let code = format!(
        "{IMPORTS};\n\
         let count = &0;\n\
         let result = button(\
             #on_press: |_| null,\
             &text(&\"click\"))"
    );
    let h = GuiTestHarness::new(&code).await?;
    view!(h);
    Ok(())
}

// ── Checkbox ────────────────────────────────────────────────────────

#[tokio::test(flavor = "current_thread")]
async fn checkbox_renders() -> Result<()> {
    let h = harness(r#"checkbox(#label: &"Accept", &false)"#).await?;
    view!(h);
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn checkbox_with_reactive_ref() -> Result<()> {
    let code = format!(
        "{IMPORTS};\n\
         let checked = &false;\n\
         let result = checkbox(#label: &\"Toggle me\", checked)"
    );
    let mut h = GuiTestHarness::new(&code).await?;
    view!(h);
    h.drain().await?;
    view!(h);
    Ok(())
}

// ── Toggler ─────────────────────────────────────────────────────────

#[tokio::test(flavor = "current_thread")]
async fn toggler_renders() -> Result<()> {
    let h = harness(r#"toggler(#label: &"Dark mode", &true)"#).await?;
    view!(h);
    Ok(())
}

// ── TextInput ───────────────────────────────────────────────────────

#[tokio::test(flavor = "current_thread")]
async fn text_input_renders() -> Result<()> {
    let h = harness(r#"text_input(#placeholder: &"Type here...", &"initial")"#).await?;
    view!(h);
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn text_input_with_reactive_ref() -> Result<()> {
    let code = format!(
        "{IMPORTS};\n\
         let val = &\"\";\n\
         let result = text_input(#placeholder: &\"Search\", #on_submit: |_| null, val)"
    );
    let mut h = GuiTestHarness::new(&code).await?;
    view!(h);
    h.drain().await?;
    view!(h);
    Ok(())
}

// ── Slider ──────────────────────────────────────────────────────────

#[tokio::test(flavor = "current_thread")]
async fn slider_renders() -> Result<()> {
    let h = harness(r#"slider(#min: &0.0, #max: &100.0, &50.0)"#).await?;
    view!(h);
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn slider_with_step() -> Result<()> {
    let h = harness(r#"slider(#min: &0.0, #max: &100.0, #step: &5.0, &25.0)"#).await?;
    view!(h);
    Ok(())
}

// ── Radio ───────────────────────────────────────────────────────────

#[tokio::test(flavor = "current_thread")]
async fn radio_renders() -> Result<()> {
    let h = harness(
        r#"radio(#label: &"Option A", #selected: &"option_a", &"option_a")"#,
    )
    .await?;
    view!(h);
    Ok(())
}

// ── PickList ────────────────────────────────────────────────────────

#[tokio::test(flavor = "current_thread")]
async fn pick_list_renders() -> Result<()> {
    let h = harness(
        "pick_list(\
            #selected: &\"Red\",\
            #placeholder: &\"Choose...\",\
            &[\"Red\", \"Green\", \"Blue\"])",
    )
    .await?;
    view!(h);
    Ok(())
}

// ── ProgressBar ─────────────────────────────────────────────────────

#[tokio::test(flavor = "current_thread")]
async fn progress_bar_renders() -> Result<()> {
    let h = harness(r#"progress_bar(#min: &0.0, #max: &1.0, &0.5)"#).await?;
    view!(h);
    Ok(())
}

// ── TextEditor ──────────────────────────────────────────────────────

#[tokio::test(flavor = "current_thread")]
async fn text_editor_renders() -> Result<()> {
    let h = harness(r#"text_editor(#placeholder: &"Edit...", &"Hello\nWorld")"#).await?;
    view!(h);
    Ok(())
}

// ── Row / Column ────────────────────────────────────────────────────

#[tokio::test(flavor = "current_thread")]
async fn row_with_children() -> Result<()> {
    let h = harness(
        "row(#spacing: &10.0, &[text(&\"Left\"), text(&\"Right\")])",
    )
    .await?;
    view!(h);
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn column_with_children() -> Result<()> {
    let h = harness(
        "column(#spacing: &10.0, &[text(&\"Top\"), text(&\"Bottom\")])",
    )
    .await?;
    view!(h);
    Ok(())
}

// ── Container ───────────────────────────────────────────────────────

#[tokio::test(flavor = "current_thread")]
async fn container_renders() -> Result<()> {
    let h = harness(
        "container(#halign: &`Center, #valign: &`Center, &text(&\"Centered\"))",
    )
    .await?;
    view!(h);
    Ok(())
}

// ── Scrollable ──────────────────────────────────────────────────────

#[tokio::test(flavor = "current_thread")]
async fn scrollable_renders() -> Result<()> {
    let h = harness(r#"scrollable(&text(&"Scrollable content"))"#).await?;
    view!(h);
    Ok(())
}

// ── Space ───────────────────────────────────────────────────────────

#[tokio::test(flavor = "current_thread")]
async fn space_renders() -> Result<()> {
    let h = harness(r#"space(#width: &`Fill, #height: &`Fixed(20.0))"#).await?;
    view!(h);
    Ok(())
}

// ── Rules ───────────────────────────────────────────────────────────

#[tokio::test(flavor = "current_thread")]
async fn horizontal_rule_renders() -> Result<()> {
    let h = harness(r#"horizontal_rule(#height: &2.0)"#).await?;
    view!(h);
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn vertical_rule_renders() -> Result<()> {
    let h = harness(r#"vertical_rule(#width: &2.0)"#).await?;
    view!(h);
    Ok(())
}

// ── Stack ───────────────────────────────────────────────────────────

#[tokio::test(flavor = "current_thread")]
async fn stack_renders() -> Result<()> {
    let h = harness(
        "stack(&[text(&\"Background\"), text(&\"Foreground\")])",
    )
    .await?;
    view!(h);
    Ok(())
}

// ── Tooltip ─────────────────────────────────────────────────────────

#[tokio::test(flavor = "current_thread")]
async fn tooltip_renders() -> Result<()> {
    let h = harness(
        "tooltip(#tip: &text(&\"Tooltip text\"), #position: &`Top, &text(&\"Hover me\"))",
    )
    .await?;
    view!(h);
    Ok(())
}

// ── VerticalSlider ──────────────────────────────────────────────────

#[tokio::test(flavor = "current_thread")]
async fn vertical_slider_renders() -> Result<()> {
    let h = harness(r#"vertical_slider(#min: &0.0, #max: &100.0, &50.0)"#).await?;
    view!(h);
    Ok(())
}

// ── ComboBox ────────────────────────────────────────────────────────

#[tokio::test(flavor = "current_thread")]
async fn combo_box_renders() -> Result<()> {
    let h = harness(
        "combo_box(\
            #selected: &\"Alpha\",\
            #placeholder: &\"Pick one\",\
            &[\"Alpha\", \"Beta\", \"Gamma\"])",
    )
    .await?;
    view!(h);
    Ok(())
}

// ── MouseArea ───────────────────────────────────────────────────────

#[tokio::test(flavor = "current_thread")]
async fn mouse_area_renders() -> Result<()> {
    let h = harness(
        "mouse_area(#on_press: |_| null, &text(&\"Click zone\"))",
    )
    .await?;
    view!(h);
    Ok(())
}

// ── Reactive ref tests ──────────────────────────────────────────────
// These test that widgets compile correctly with reactive ref values
// and that the initial view renders. Tests reactive value propagation
// through the widget tree by draining initial updates.

#[tokio::test(flavor = "current_thread")]
async fn text_with_reactive_ref() -> Result<()> {
    let code = format!(
        "{IMPORTS};\n\
         let msg = &\"hello\";\n\
         let result = text(msg)"
    );
    let mut h = GuiTestHarness::new(&code).await?;
    view!(h);
    h.drain().await?;
    view!(h);
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn nested_widget_with_reactive_ref() -> Result<()> {
    let code = format!(
        "{IMPORTS};\n\
         let label = &\"initial\";\n\
         let result = container(\
             &column(&[text(label), button(&text(&\"btn\"))]))"
    );
    let mut h = GuiTestHarness::new(&code).await?;
    view!(h);
    h.drain().await?;
    view!(h);
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn slider_with_reactive_ref() -> Result<()> {
    let code = format!(
        "{IMPORTS};\n\
         let val = &25.0;\n\
         let result = slider(#min: &0.0, #max: &100.0, val)"
    );
    let mut h = GuiTestHarness::new(&code).await?;
    view!(h);
    h.drain().await?;
    view!(h);
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn checkbox_with_reactive_value() -> Result<()> {
    let code = format!(
        "{IMPORTS};\n\
         let checked = &false;\n\
         let result = checkbox(#label: &\"Check me\", checked)"
    );
    let mut h = GuiTestHarness::new(&code).await?;
    view!(h);
    h.drain().await?;
    view!(h);
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn toggler_with_reactive_value() -> Result<()> {
    let code = format!(
        "{IMPORTS};\n\
         let toggled = &false;\n\
         let result = toggler(#label: &\"Toggle me\", toggled)"
    );
    let mut h = GuiTestHarness::new(&code).await?;
    view!(h);
    h.drain().await?;
    view!(h);
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn text_input_with_reactive_value() -> Result<()> {
    let code = format!(
        "{IMPORTS};\n\
         let val = &\"\";\n\
         let result = text_input(#placeholder: &\"Type...\", val)"
    );
    let mut h = GuiTestHarness::new(&code).await?;
    view!(h);
    h.drain().await?;
    view!(h);
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn pick_list_with_reactive_selection() -> Result<()> {
    let code = format!(
        "{IMPORTS};\n\
         let sel = &\"Red\";\n\
         let result = pick_list(\
             #selected: sel,\
             #placeholder: &\"Choose\",\
             &[\"Red\", \"Green\", \"Blue\"])"
    );
    let mut h = GuiTestHarness::new(&code).await?;
    view!(h);
    h.drain().await?;
    view!(h);
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn row_with_reactive_children() -> Result<()> {
    let code = format!(
        "{IMPORTS};\n\
         let items = &[text(&\"one\")];\n\
         let result = row(items)"
    );
    let mut h = GuiTestHarness::new(&code).await?;
    view!(h);
    h.drain().await?;
    view!(h);
    Ok(())
}
