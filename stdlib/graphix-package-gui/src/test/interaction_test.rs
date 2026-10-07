use super::{InteractionHarness, expect_call, expect_call_with_args};
use anyhow::Result;
use graphix_rt::NoExt;
use iced_core::{Event, Point, Size, mouse};
use netidx::publisher::Value;

const IMPORTS: &str = "\
use gui::*;\n\
use gui::text::{self, *};\n\
use gui::button::{self, *};\n\
use gui::checkbox::{self, *};\n\
use gui::toggler::{self, *};\n\
use gui::text_input::{self, *};\n\
use gui::slider::{self, *};\n\
use gui::radio::{self, *};\n\
use gui::pick_list::{self, *};\n\
use gui::text_editor::{self, *};\n\
use gui::vertical_slider::{self, *};\n\
use gui::mouse_area::{self, *};\n\
use gui::keyboard_area::{self, *};\n\
use gui::scrollable::{self, *};\n\
use gui::combo_box::{self, *};\n\
use gui::column::{self, *}";

async fn harness(widget_expr: &str) -> Result<InteractionHarness> {
    let code = format!("{IMPORTS};\nlet result = {widget_expr}");
    InteractionHarness::new(&code).await
}

/// Widgets use Shrink sizing and sit at (0,0); the viewport center
/// misses them.
const WIDGET_HIT: Point = Point::new(10.0, 10.0);

#[tokio::test(flavor = "current_thread")]
async fn button_click_produces_call() -> Result<()> {
    let mut h = harness("button(#on_press: |_| null, &text(&\"Click me\"))").await?;
    let msgs = h.click(WIDGET_HIT);
    expect_call(&msgs);
    Ok(())
}

/// A row with fewer or more cells than the table has columns, rows of
/// different lengths and a table with no columns lay out and take input.
#[tokio::test(flavor = "current_thread")]
async fn table_cell_count_mismatch_lays_out() -> Result<()> {
    let cases = [
        "[table_column(&text(&\"A\")), table_column(&text(&\"B\")), table_column(&text(&\"C\"))], \
         [[text(&\"1\"), text(&\"2\")]]",
        "[table_column(&text(&\"Only\"))], [[text(&\"a\"), text(&\"b\"), text(&\"c\")]]",
        "[table_column(&text(&\"A\")), table_column(&text(&\"B\"))], \
         [[], [text(&\"1\")], [text(&\"1\"), text(&\"2\"), text(&\"3\")]]",
        "[], [[text(&\"orphan\")]]",
    ];
    for case in cases {
        let code = format!(
            "{IMPORTS};\nuse gui::table::{{self, *}};\nlet result = table(&{})",
            case.replacen("], [", "], &[", 1)
        );
        let mut h = InteractionHarness::new(&code).await?;
        let _ = h.view();
        let _ = h.click(WIDGET_HIT);
        h.resize(Size::new(40.0, 20.0));
    }
    Ok(())
}

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
    // CR claude for claude: [test-gap] These predicates accept any Bool (here and in
    // toggler_toggle_produces_call), and radio_on_select, text_input_on_input and
    // text_editor_on_edit accept any String, so a wrapper that sends the old value
    // (false, "none", "") instead of the new one (true, "option_a", "a") passes.
    // expect_call_with_args (mod.rs:642) also returns the first match without checking
    // it is the only one, so a callback fired twice per click passes, and
    // on_resize_fires_on_drag (data_table_test.rs:1327) accepts any width above 100
    // where the drag from 100 to 180 must give 180. Assert the exact values, and make
    // expect_call_with_args require exactly one match as expect_call does.
    // (tests-ui.r2-14)
    expect_call_with_args(&msgs, |args| {
        matches!(args.iter().next(), Some(Value::Bool(_)))
    });
    Ok(())
}

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

#[tokio::test(flavor = "current_thread")]
async fn slider_on_change_produces_call() -> Result<()> {
    let code = format!(
        "{IMPORTS};\n\
         let changed = false;\n\
         let result = slider(#min: &0.0, #max: &100.0, \
             #on_change: |v| changed <- v ~ true, &50.0)"
    );
    let mut h = InteractionHarness::with_viewport(&code, Size::new(200.0, 22.0)).await?;
    let initial = h.watch("test::changed").await?;
    assert_eq!(initial, Value::Bool(false));
    let msgs = h.click(Point::new(150.0, 10.0));
    h.dispatch_calls(&msgs).await?;
    assert_eq!(h.get_watched("test::changed"), Some(&Value::Bool(true)));
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn slider_on_release_produces_call() -> Result<()> {
    let code = format!(
        "{IMPORTS};\n\
         let released = false;\n\
         let result = slider(#min: &0.0, #max: &100.0, \
             #on_release: |click| released <- click ~ true, &50.0)"
    );
    let mut h = InteractionHarness::with_viewport(&code, Size::new(200.0, 22.0)).await?;
    let initial = h.watch("test::released").await?;
    assert_eq!(initial, Value::Bool(false));
    let msgs = h.click(Point::new(150.0, 10.0));
    h.dispatch_calls(&msgs).await?;
    assert_eq!(h.get_watched("test::released"), Some(&Value::Bool(true)));
    Ok(())
}

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

#[tokio::test(flavor = "current_thread")]
async fn vertical_slider_on_change_produces_call() -> Result<()> {
    let code = format!(
        "{IMPORTS};\n\
         let changed = false;\n\
         let result = vertical_slider(\
             #min: &0.0, #max: &100.0, #on_change: |v| changed <- v ~ true, &50.0)"
    );
    let mut h = InteractionHarness::with_viewport(&code, Size::new(22.0, 200.0)).await?;
    let initial = h.watch("test::changed").await?;
    assert_eq!(initial, Value::Bool(false));
    let msgs = h.click(Point::new(10.0, 50.0));
    h.dispatch_calls(&msgs).await?;
    assert_eq!(h.get_watched("test::changed"), Some(&Value::Bool(true)));
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn vertical_slider_on_release_produces_call() -> Result<()> {
    let code = format!(
        "{IMPORTS};\n\
         let released = false;\n\
         let result = vertical_slider(\
             #min: &0.0, #max: &100.0, \
             #on_release: |click| released <- click ~ true, &50.0)"
    );
    let mut h = InteractionHarness::with_viewport(&code, Size::new(22.0, 200.0)).await?;
    let initial = h.watch("test::released").await?;
    assert_eq!(initial, Value::Bool(false));
    let msgs = h.click(Point::new(10.0, 50.0));
    h.dispatch_calls(&msgs).await?;
    assert_eq!(h.get_watched("test::released"), Some(&Value::Bool(true)));
    Ok(())
}

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
    expect_call(&msgs);
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn text_input_on_input_produces_call() -> Result<()> {
    let code = format!(
        "{IMPORTS};\n\
         let result = text_input(\
             #placeholder: &\"Type here\", \
             #on_input: |s| null, \
             &\"\")"
    );
    let mut h = InteractionHarness::new(&code).await?;
    h.click(WIDGET_HIT);
    let msgs = h.type_text("a");
    expect_call_with_args(&msgs, |args| {
        matches!(args.iter().next(), Some(Value::String(_)))
    });
    Ok(())
}

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

#[tokio::test(flavor = "current_thread")]
async fn radio_on_select_produces_call() -> Result<()> {
    let code = format!(
        "{IMPORTS};\n\
         let result = radio(\
             #label: &\"Option A\", \
             #selected: &\"none\", \
             #on_select: |v| null, \
             &\"option_a\")"
    );
    let mut h = InteractionHarness::new(&code).await?;
    let msgs = h.click(WIDGET_HIT);
    expect_call_with_args(&msgs, |args| {
        matches!(args.iter().next(), Some(Value::String(_)))
    });
    Ok(())
}

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

#[tokio::test(flavor = "current_thread")]
async fn pick_list_on_select_produces_call() -> Result<()> {
    let code = format!(
        "{IMPORTS};\n\
         let result = pick_list(\
             #selected: &\"Red\", \
             #on_select: |s| null, \
             #placeholder: &\"Choose...\", \
             &[\"Red\", \"Green\", \"Blue\"])"
    );
    // The dropdown is an overlay, which the headless UserInterface does
    // not route clicks to; this pins only that clicking does not panic.
    // CR claude for claude: [test-gap] The comment above is stale, and the test checks
    // nothing its name promises. on_edit_combo_column (data_table_test.rs:946-963)
    // opens the same iced PickList overlay with one click in this harness and selects
    // an option with a second. Making PickListW's on_select closure
    // (pick_list.rs:114-119) always return Nop leaves this test green. Click to open,
    // click the second option at (x, widget bottom + 22 * 1.5) as that test does, and
    // expect_call_with_args for "Green". Do the same in
    // combo_box_on_select_produces_call (:405). (tests-ui-11)
    let mut h = InteractionHarness::with_viewport(&code, Size::new(300.0, 200.0)).await?;
    let _ = h.view();
    let _ = h.click(WIDGET_HIT);
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn mouse_area_press_produces_call() -> Result<()> {
    let code = format!(
        "{IMPORTS};\n\
         let pressed = false;\n\
         let result = mouse_area(\
             #on_press: |click| pressed <- click ~ true, \
             &text(&\"Click zone\"))"
    );
    let mut h = InteractionHarness::new(&code).await?;
    let initial = h.watch("test::pressed").await?;
    assert_eq!(initial, Value::Bool(false));
    let msgs = h.click(WIDGET_HIT);
    h.dispatch_calls(&msgs).await?;
    assert_eq!(h.get_watched("test::pressed"), Some(&Value::Bool(true)));
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn mouse_area_release_produces_call() -> Result<()> {
    let code = format!(
        "{IMPORTS};\n\
         let released = false;\n\
         let result = mouse_area(\
             #on_release: |click| released <- click ~ true, \
             &text(&\"Click zone\"))"
    );
    let mut h = InteractionHarness::new(&code).await?;
    let initial = h.watch("test::released").await?;
    assert_eq!(initial, Value::Bool(false));
    let msgs = h.click(WIDGET_HIT);
    h.dispatch_calls(&msgs).await?;
    assert_eq!(h.get_watched("test::released"), Some(&Value::Bool(true)));
    Ok(())
}

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

#[tokio::test(flavor = "current_thread")]
async fn text_editor_on_edit_produces_callback() -> Result<()> {
    let code = format!(
        "{IMPORTS};\n\
         let result = text_editor(#placeholder: &\"Edit...\", #on_edit: |s| null, &\"\")"
    );
    let mut h = InteractionHarness::with_viewport(&code, Size::new(300.0, 100.0)).await?;
    h.click(WIDGET_HIT);
    let msgs = h.type_text("a");
    let results = h.process_editor_actions(&msgs);
    assert!(
        results.iter().any(|(_, v)| matches!(v, Value::String(_))),
        "text_editor on_edit should produce a String value callback"
    );
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn combo_box_on_select_produces_call() -> Result<()> {
    let code = format!(
        "{IMPORTS};\n\
         let result = combo_box(\
             #selected: &\"Alpha\", \
             #on_select: |s| null, \
             #placeholder: &\"Pick one\", \
             &[\"Alpha\", \"Beta\", \"Gamma\"])"
    );
    // Suggestions are an overlay, as for pick_list.
    let mut h = InteractionHarness::with_viewport(&code, Size::new(300.0, 200.0)).await?;
    let _ = h.view();
    // CR claude for claude: [test-gap] This test clicks and returns Ok without looking at
    // any message, so it cannot fail. It should pick an option (type and press Enter,
    // or click the overlay) and expect the on_select Call. No canvas_test case draws
    // either: view() never calls Program::draw, so draw_shape (canvas.rs:213-338) runs
    // in no test (chart_test.rs:282-310 shows a headless draw, and CanvasW would need
    // as_any). Context menus have only a render test (widgets_test.rs:532), with
    // nothing for right-click, item clicks, shortcuts, disabled items or a scrolled
    // container. As a result, the suite passes despite the canvas panics on NaN
    // coordinates and zero text size, the combo box reset on a same-value options fire,
    // and the dead shortcuts of a closed context menu. (gui-widgets-a-17)
    let _ = h.click(WIDGET_HIT);
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn scrollable_on_scroll_produces_call() -> Result<()> {
    let code = format!(
        "{IMPORTS};\n\
         let result = scrollable(\
             #on_scroll: |pos| null, \
             #height: &`Fixed(50.0), \
             &column(#spacing: &10.0, &[\
                 text(&\"Line 1\"), text(&\"Line 2\"), text(&\"Line 3\"), \
                 text(&\"Line 4\"), text(&\"Line 5\"), text(&\"Line 6\"), \
                 text(&\"Line 7\"), text(&\"Line 8\"), text(&\"Line 9\"), \
                 text(&\"Line 10\")\
             ]))"
    );
    let mut h = InteractionHarness::with_viewport(&code, Size::new(300.0, 50.0)).await?;
    h.move_cursor(Point::new(10.0, 10.0));
    // down: the first frame published the viewport at the top
    let msgs = h.scroll(0.0, -3.0);
    expect_call(&msgs);
    Ok(())
}

// Each mouse_area callback test flips a graphix variable only that
// handler can reach, since `expect_call` cannot tell the slots apart.

#[tokio::test(flavor = "current_thread")]
async fn mouse_area_on_enter_produces_call() -> Result<()> {
    let code = format!(
        "{IMPORTS};\n\
         let entered = false;\n\
         let result = mouse_area(\
             #on_enter: |click| entered <- click ~ true, \
             &text(&\"Zone\"))"
    );
    let mut h = InteractionHarness::new(&code).await?;
    let initial = h.watch("test::entered").await?;
    assert_eq!(initial, Value::Bool(false));
    let msgs = h.move_cursor(WIDGET_HIT);
    h.dispatch_calls(&msgs).await?;
    assert_eq!(h.get_watched("test::entered"), Some(&Value::Bool(true)));
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn mouse_area_on_exit_produces_call() -> Result<()> {
    let code = format!(
        "{IMPORTS};\n\
         let exited = false;\n\
         let result = mouse_area(\
             #on_exit: |click| exited <- click ~ true, \
             &text(&\"Zone\"))"
    );
    let mut h = InteractionHarness::new(&code).await?;
    let initial = h.watch("test::exited").await?;
    assert_eq!(initial, Value::Bool(false));
    h.move_cursor(WIDGET_HIT);
    let msgs = h.move_cursor(Point::new(999.0, 999.0));
    h.dispatch_calls(&msgs).await?;
    assert_eq!(h.get_watched("test::exited"), Some(&Value::Bool(true)));
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn mouse_area_on_move_produces_call() -> Result<()> {
    let code = format!(
        "{IMPORTS};\n\
         let moved = false;\n\
         let result = mouse_area(\
             #on_move: |pos| moved <- pos ~ true, \
             &text(&\"Zone\"))"
    );
    let mut h = InteractionHarness::new(&code).await?;
    let initial = h.watch("test::moved").await?;
    assert_eq!(initial, Value::Bool(false));
    // Only cursor motion after the on_enter arm reaches on_move.
    h.move_cursor(Point::new(5.0, 5.0));
    let msgs = h.move_cursor(WIDGET_HIT);
    h.dispatch_calls(&msgs).await?;
    assert_eq!(h.get_watched("test::moved"), Some(&Value::Bool(true)));
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn keyboard_area_on_key_press_produces_call() -> Result<()> {
    let code = format!(
        "{IMPORTS};\n\
         let pressed = false;\n\
         let result = keyboard_area(\
             #on_key_press: |ev| pressed <- ev ~ true, \
             &text(&\"Type here\"))"
    );
    let mut h = InteractionHarness::new(&code).await?;
    let initial = h.watch("test::pressed").await?;
    assert_eq!(initial, Value::Bool(false));
    h.click(WIDGET_HIT);
    let msgs = h.press_key(iced_core::keyboard::key::Named::Space);
    h.dispatch_calls(&msgs).await?;
    assert_eq!(h.get_watched("test::pressed"), Some(&Value::Bool(true)));
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn keyboard_area_on_key_release_produces_call() -> Result<()> {
    let code = format!(
        "{IMPORTS};\n\
         let released = false;\n\
         let result = keyboard_area(\
             #on_key_release: |ev| released <- ev ~ true, \
             &text(&\"Type here\"))"
    );
    let mut h = InteractionHarness::new(&code).await?;
    let initial = h.watch("test::released").await?;
    assert_eq!(initial, Value::Bool(false));
    h.click(WIDGET_HIT);
    let msgs = h.release_key(iced_core::keyboard::key::Named::Space);
    h.dispatch_calls(&msgs).await?;
    assert_eq!(h.get_watched("test::released"), Some(&Value::Bool(true)));
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn button_press_rotates_seq() -> Result<()> {
    let code = format!(
        "{IMPORTS};\n\
         let charts = [1, 2, 3];\n\
         let result = button(\
             #on_press: |e| seq e {{ charts <- array::rotate(charts) }}, \
             &text(&\">\"))"
    );
    let mut h = InteractionHarness::new(&code).await?;
    let initial = h.watch("test::charts").await?;
    assert_eq!(initial.clone().cast_to::<[i64; 3]>()?, [1, 2, 3]);
    let msgs = h.click(WIDGET_HIT);
    h.dispatch_calls(&msgs).await?;
    let v = h.get_watched("test::charts").unwrap().clone().cast_to::<[i64; 3]>()?;
    assert_eq!(v, [3, 1, 2]);
    let msgs = h.click(WIDGET_HIT);
    h.dispatch_calls(&msgs).await?;
    let v = h.get_watched("test::charts").unwrap().clone().cast_to::<[i64; 3]>()?;
    assert_eq!(v, [2, 3, 1]);
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn button_press_rotates_sampled() -> Result<()> {
    let code = format!(
        "{IMPORTS};\n\
         let charts = [1, 2, 3];\n\
         let result = button(\
             #on_press: |e| charts <- e ~ array::rotate(charts), \
             &text(&\">\"))"
    );
    let mut h = InteractionHarness::new(&code).await?;
    let initial = h.watch("test::charts").await?;
    assert_eq!(initial.clone().cast_to::<[i64; 3]>()?, [1, 2, 3]);
    let msgs = h.click(WIDGET_HIT);
    h.dispatch_calls(&msgs).await?;
    let v = h.get_watched("test::charts").unwrap().clone().cast_to::<[i64; 3]>()?;
    assert_eq!(v, [3, 1, 2]);
    let msgs = h.click(WIDGET_HIT);
    h.dispatch_calls(&msgs).await?;
    let v = h.get_watched("test::charts").unwrap().clone().cast_to::<[i64; 3]>()?;
    assert_eq!(v, [2, 3, 1]);
    Ok(())
}

#[tokio::test(flavor = "multi_thread")]
async fn stack_children_follow_rotation() -> Result<()> {
    let code = format!(
        "{IMPORTS};\n\
         use gui::stack::{{self, *}};\n\
         let slides = [text(&\"A\"), column(&[text(&\"B\")])];\n\
         let result = column(&[\
             button(#on_press: |e| seq e {{ slides <- array::rotate(slides) }}, &text(&\">\")), \
             stack(&slides)])"
    );
    let mut h = InteractionHarness::new(&code).await?;
    let _ = h.watch("test::slides").await?;
    let order = |h: &InteractionHarness| -> Vec<usize> {
        let count = |w: &crate::widgets::GuiW<NoExt>| {
            let mut n = 0;
            w.for_each_child(&mut |_| n += 1);
            n
        };
        let (mut i, mut out) = (0, Vec::new());
        h.inner.widget.for_each_child(&mut |c| {
            if i == 1 {
                c.for_each_child(&mut |slide| out.push(count(slide)));
            }
            i += 1;
        });
        out
    };
    assert_eq!(order(&h), [0, 1]);
    let msgs = h.click(WIDGET_HIT);
    h.dispatch_calls(&msgs).await?;
    assert_eq!(order(&h), [1, 0]);
    Ok(())
}

/// Ctrl+V pastes into a focused text input: the input takes Ctrl from the
/// ModifiersChanged event the loop forwards.
#[tokio::test(flavor = "current_thread")]
async fn text_input_ctrl_v_pastes() -> Result<()> {
    let code = format!(
        "{IMPORTS};\n\
         let result = text_input(#on_input: |s| null, &\"\")"
    );
    let mut h = InteractionHarness::new(&code).await?;
    h.clipboard.0 = Some("pasted".into());
    h.click(WIDGET_HIT);
    let msgs = h.press_ctrl("v");
    expect_call_with_args(
        &msgs,
        |args| matches!(args.iter().next(), Some(Value::String(s)) if &**s == "pasted"),
    );
    Ok(())
}

/// The pointer leaving the window leaves a mouse area too.
#[tokio::test(flavor = "current_thread")]
async fn mouse_area_exits_when_the_cursor_leaves_the_window() -> Result<()> {
    let code = format!(
        "{IMPORTS};\n\
         let result = mouse_area(#on_exit: |c| null, &text(&\"Zone\"))"
    );
    let mut h = InteractionHarness::new(&code).await?;
    h.move_cursor(WIDGET_HIT);
    let msgs = h.process_events(&[Event::Mouse(mouse::Event::CursorLeft)]);
    expect_call(&msgs);
    Ok(())
}
