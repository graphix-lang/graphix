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
use gui::progress_bar::{self, *};\n\
use gui::menu::{self, *};\n\
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
    expect_call_with_args(&msgs, |args| args.first() == Some(&Value::Bool(true)));
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
    expect_call_with_args(&msgs, |args| args.first() == Some(&Value::Bool(true)));
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
    expect_call_with_args(&msgs, |args| args.first() == Some(&Value::from("a")));
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
    expect_call_with_args(&msgs, |args| args.first() == Some(&Value::from("option_a")));
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
    let mut h = InteractionHarness::with_viewport(&code, Size::new(300.0, 200.0)).await?;
    let msgs = pick(&mut h, "Green");
    expect_call_with_args(&msgs, |args| args.first() == Some(&Value::from("Green")));
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
    let values: Vec<_> = results.iter().map(|(_, v)| v.clone()).collect();
    assert_eq!(values, [Value::from("a")], "one on_edit with the new text");
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
    let mut h = InteractionHarness::with_viewport(&code, Size::new(300.0, 200.0)).await?;
    let _ = h.view();
    let _ = h.click(WIDGET_HIT);
    let _ = h.type_text("Gam");
    let msgs = h.press_key(iced_core::keyboard::key::Named::Enter);
    expect_call_with_args(&msgs, |args| args.first() == Some(&Value::from("Gamma")));
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

/// An area with no press handler leaves presses to the area around it.
#[tokio::test(flavor = "current_thread")]
async fn keyboard_area_passes_keys_it_has_no_handler_for() -> Result<()> {
    let code = format!(
        "{IMPORTS};\n\
         let outer = \"\";\n\
         let inner = \"\";\n\
         let result = keyboard_area(\
             #on_key_press: |ev| outer <- ev.key, \
             &keyboard_area(#on_key_release: |ev| inner <- ev.key, &text(&\"Type here\")))"
    );
    let mut h = InteractionHarness::new(&code).await?;
    let _ = h.watch("test::outer").await?;
    let _ = h.watch("test::inner").await?;
    h.click(WIDGET_HIT);
    let mut msgs = h.press_key(iced_core::keyboard::key::Named::Enter);
    msgs.extend(h.release_key(iced_core::keyboard::key::Named::Enter));
    h.dispatch_calls(&msgs).await?;
    assert_eq!(h.get_watched("test::outer"), Some(&Value::String("Enter".into())));
    assert_eq!(h.get_watched("test::inner"), Some(&Value::String("Enter".into())));
    Ok(())
}

/// Named keys are spelled as the book documents them.
#[test]
fn key_names() {
    use crate::widgets::keyboard_area::key_event_to_value;
    use iced_core::keyboard::{Key, Modifiers, key::Named};
    for (key, name) in [
        (Key::Named(Named::Enter), "Enter"),
        (Key::Named(Named::ArrowUp), "ArrowUp"),
        (Key::Named(Named::Escape), "Escape"),
        (Key::Named(Named::Tab), "Tab"),
        (Key::Character("a".into()), "a"),
    ] {
        let v = key_event_to_value(&key, Modifiers::empty(), None, false);
        let key = v.cast_to::<std::collections::HashMap<String, Value>>().unwrap()["key"]
            .clone();
        assert_eq!(key, Value::String(name.into()));
    }
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

/// Open the dropdown at `WIDGET_HIT` and click down its list until a row
/// chooses `option`; the messages of that click.
fn pick(h: &mut InteractionHarness, option: &str) -> Vec<super::Message> {
    let _ = h.view();
    for y in (24..160).step_by(4) {
        let _ = h.click(WIDGET_HIT);
        let msgs = h.click(Point::new(WIDGET_HIT.x, y as f32));
        let chose = msgs.iter().any(|m| {
            matches!(m, super::Message::Call(_, a) if a.first() == Some(&Value::String(option.into())))
        });
        if chose {
            return msgs;
        }
    }
    panic!("no row of the dropdown chose {option}")
}

/// Options delivered again unchanged keep what the user is typing.
#[tokio::test(flavor = "current_thread")]
async fn combo_box_keeps_typing_across_equal_options() -> Result<()> {
    let code = format!(
        "{IMPORTS};\n\
         let opts = [\"apple\", \"banana\", \"cherry\"];\n\
         let result = combo_box(#on_select: |s| null, #placeholder: &\"Pick\", &opts)"
    );
    let mut h = InteractionHarness::with_viewport(&code, Size::new(300.0, 200.0)).await?;
    let _ = h.view();
    let _ = h.click(WIDGET_HIT);
    let _ = h.type_text("ban");
    let opts =
        graphix_package_core::testing::find_bind_id(&h.inner.compiled.env, "test::opts")?;
    let opts_ref = h.inner.gx.compile_ref(opts).await?;
    for _ in 0..3 {
        let v = opts_ref.last.clone().expect("the options");
        h.inner.gx.compile_ref(opts).await?.set(v)?;
        h.drain().await?;
    }
    let msgs = h.press_key(iced_core::keyboard::key::Named::Enter);
    expect_call_with_args(&msgs, |args| args.first() == Some(&Value::from("banana")));
    Ok(())
}

/// Keys typed before the runtime echoes the earlier ones build on what
/// was typed: the input's text is the user's, not the stale value's.
#[tokio::test(flavor = "current_thread")]
async fn text_input_keys_outrun_their_echoes() -> Result<()> {
    let code = format!(
        "{IMPORTS};\n\
         let v = \"\";\n\
         let result = text_input(#on_input: |s| v <- s, &v)"
    );
    let mut h = InteractionHarness::new(&code).await?;
    let _ = h.watch("test::v").await?;
    let _ = h.click(WIDGET_HIT);
    for c in ["a", "b", "c"] {
        let msgs = h.type_text(c);
        h.apply(&msgs);
    }
    h.drain().await?;
    assert_eq!(h.get_watched("test::v"), Some(&Value::from("abc")));
    Ok(())
}

/// Two clicks before the first echo toggle twice.
#[tokio::test(flavor = "current_thread")]
async fn checkbox_clicks_outrun_their_echoes() -> Result<()> {
    let code = format!(
        "{IMPORTS};\n\
         let c = false;\n\
         let result = checkbox(#label: &\"c\", #on_toggle: |b| c <- b, &c)"
    );
    let mut h = InteractionHarness::new(&code).await?;
    let _ = h.watch("test::c").await?;
    for _ in 0..2 {
        let msgs = h.click(WIDGET_HIT);
        h.apply(&msgs);
    }
    h.drain().await?;
    assert_eq!(h.get_watched("test::c"), Some(&Value::Bool(false)));
    Ok(())
}

/// The editor keeps its cursor when its echo comes back, through a struct
/// field that re-fires the whole column.
#[tokio::test(flavor = "current_thread")]
async fn text_editor_keeps_its_cursor_through_echoes() -> Result<()> {
    let code = format!(
        "{IMPORTS};\n\
         let doc = {{ text: \"\", n: 0 }};\n\
         let result = column(&[\
             text_editor(#on_edit: |s| doc <- s ~ {{doc with text: s, n: doc.n + 1}}, &doc.text)\
         ])"
    );
    let mut h = InteractionHarness::with_viewport(&code, Size::new(300.0, 100.0)).await?;
    let _ = h.watch("test::doc").await?;
    let _ = h.click(WIDGET_HIT);
    for c in ["a", "b", "c"] {
        h.live(&[char_key(c)]).await?;
    }
    let text = match h.get_watched("test::doc") {
        Some(Value::Array(fields)) => fields.iter().find_map(|f| match f {
            Value::Array(kv) if kv[0] == Value::from("text") => Some(kv[1].clone()),
            _ => None,
        }),
        _ => None,
    };
    assert_eq!(text, Some(Value::from("abc")));
    Ok(())
}

fn char_key(c: &str) -> Event {
    use iced_core::keyboard;
    let s: iced_core::SmolStr = c.into();
    Event::Keyboard(keyboard::Event::KeyPressed {
        key: keyboard::Key::Character(s.clone()),
        modified_key: keyboard::Key::Character(s.clone()),
        physical_key: keyboard::key::Physical::Unidentified(
            keyboard::key::NativeCode::Unidentified,
        ),
        location: keyboard::Location::Standard,
        modifiers: keyboard::Modifiers::empty(),
        text: Some(s),
        repeat: false,
    })
}

/// A mouse area with only a hover handler takes no clicks: the button
/// under it gets them.
#[tokio::test(flavor = "current_thread")]
async fn a_hover_only_mouse_area_passes_clicks() -> Result<()> {
    let code = format!(
        "{IMPORTS};\n\
         let pressed = false;\n\
         let result = mouse_area(\
             #on_enter: |_| null, \
             &button(#on_press: |c| pressed <- c ~ true, &text(&\"go\")))"
    );
    let mut h = InteractionHarness::new(&code).await?;
    let _ = h.watch("test::pressed").await?;
    let msgs = h.click(WIDGET_HIT);
    h.dispatch_calls(&msgs).await?;
    assert_eq!(h.get_watched("test::pressed"), Some(&Value::Bool(true)));
    Ok(())
}

/// A radio whose value has not arrived selects nothing.
#[tokio::test(flavor = "current_thread")]
async fn a_radio_without_its_value_selects_nothing() -> Result<()> {
    let code = format!(
        "{IMPORTS};\n\
         let v: string = never();\n\
         let result = radio(#label: &\"r\", #on_select: |x| null, &v)"
    );
    let mut h = InteractionHarness::new(&code).await?;
    let msgs = h.click(WIDGET_HIT);
    assert!(!msgs.iter().any(|m| matches!(m, super::Message::Call(..))), "{msgs:?}");
    Ok(())
}

/// Sliders deliver f64 values, a null step is continuous, and a progress
/// bar over an empty range draws.
#[tokio::test(flavor = "current_thread")]
async fn sliders_are_f64_and_continuous() -> Result<()> {
    let value_at = async |slider: &str, x: f32| -> Result<f64> {
        let code = format!("{IMPORTS};\nlet result = {slider}");
        let mut h =
            InteractionHarness::with_viewport(&code, Size::new(300.0, 50.0)).await?;
        let msgs = h.click(Point::new(x, 10.0));
        let id = expect_call(&msgs);
        let _ = id;
        match msgs.iter().find_map(|m| match m {
            super::Message::Call(_, a) => a.first().cloned(),
            _ => None,
        }) {
            Some(Value::F64(v)) => Ok(v),
            v => anyhow::bail!("not an f64: {v:?}"),
        }
    };
    let stepped = value_at(
        "slider(#min: &0.0, #max: &1.0, #step: &0.05, #on_change: |v| null, &0.0)",
        40.0,
    )
    .await?;
    assert!((stepped * 20.0 - (stepped * 20.0).round()).abs() < 1e-12, "{stepped}");
    let free =
        value_at("slider(#min: &0.0, #max: &1.0, #on_change: |v| null, &0.0)", 150.0)
            .await?;
    assert!(free > 0.05 && free < 0.95, "{free}");
    let h = harness("progress_bar(#min: &1.0, #max: &0.0, &0.5)").await?;
    h.inner.render().await
}

/// A context menu opens at a right-click, chooses an item at a click,
/// takes its shortcut while open and none while closed.
#[tokio::test(flavor = "current_thread")]
async fn context_menu_items_and_shortcuts() -> Result<()> {
    let code = format!(
        "{IMPORTS};\n\
         let n = 0;\n\
         let copy = menu::action(\
             #on_click: |c| n <- c ~ n + 1, \
             #shortcut: &menu::shortcut(#ctrl: true, \"c\")$, \
             &\"Copy\");\n\
         let result = menu::context_menu(&[copy], &text(&\"target\"))"
    );
    let mut h = InteractionHarness::with_viewport(&code, Size::new(400.0, 300.0)).await?;
    let _ = h.watch("test::n").await?;
    let msgs = h.press_ctrl("c");
    assert!(
        !msgs.iter().any(|m| matches!(m, super::Message::Call(..))),
        "closed: {msgs:?}"
    );
    right_click(&mut h, WIDGET_HIT);
    let msgs = h.press_ctrl("c");
    h.dispatch_calls(&msgs).await?;
    assert_eq!(h.get_watched("test::n"), Some(&Value::I64(1)), "shortcut while open");
    right_click(&mut h, WIDGET_HIT);
    let msgs = h.click(Point::new(WIDGET_HIT.x + 20.0, WIDGET_HIT.y + 12.0));
    h.dispatch_calls(&msgs).await?;
    assert_eq!(h.get_watched("test::n"), Some(&Value::I64(2)), "item click");
    Ok(())
}

/// A press on an open menu's disabled item does not reach the button
/// under it.
#[tokio::test(flavor = "current_thread")]
async fn an_open_menu_takes_presses_over_it() -> Result<()> {
    let code = format!(
        "{IMPORTS};\n\
         let pressed = false;\n\
         let off = menu::action(#on_click: |_| null, #disabled: &true, &\"Off\");\n\
         let result = menu::context_menu(\
             &[off], \
             &button(#on_press: |c| pressed <- c ~ true, &text(&\"a wide button\")))"
    );
    let mut h = InteractionHarness::with_viewport(&code, Size::new(400.0, 300.0)).await?;
    let _ = h.watch("test::pressed").await?;
    right_click(&mut h, Point::new(5.0, 5.0));
    let msgs = h.click(Point::new(20.0, 15.0));
    h.dispatch_calls(&msgs).await?;
    assert_eq!(h.get_watched("test::pressed"), Some(&Value::Bool(false)));
    Ok(())
}

fn right_click(h: &mut InteractionHarness, at: Point) {
    let _ = h.view();
    let _ = h.move_cursor(at);
    let _ = h.process_events(&[Event::Mouse(mouse::Event::ButtonPressed(
        mouse::Button::Right,
    ))]);
    let _ = h.process_events(&[Event::Mouse(mouse::Event::ButtonReleased(
        mouse::Button::Right,
    ))]);
}

/// A shortcut key is one character, whatever its byte length.
#[tokio::test(flavor = "current_thread")]
async fn a_shortcut_key_is_any_one_character() -> Result<()> {
    let code = r#"{
use gui::menu;
let valid = |k: string| select menu::shortcut(#ctrl: true, k) { error as e => false, _ => true };
[valid("é"), valid("€"), valid("ab")]
}"#;
    let (v, _ctx) =
        graphix_package_core::testing::eval(code, super::TEST_REGISTER).await?;
    assert_eq!(
        v,
        Value::Array([true, true, false].map(Value::Bool).into_iter().collect())
    );
    Ok(())
}
