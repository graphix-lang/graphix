use super::{
    IMPORTS, InteractionHarness, char_key, expect_call, expect_call_with_args,
    interaction_harness as harness,
};
use anyhow::Result;
use compact_str::format_compact;
use graphix_rt::NoExt;
use iced_core::{Event, Point, Size, mouse};
use netidx::{protocol::valarray::ValArray, publisher::Value};

/// Widgets use Shrink sizing and sit at (0,0); the viewport center
/// misses them.
const WIDGET_HIT: Point = Point::new(10.0, 10.0);

#[tokio::test(flavor = "current_thread")]
async fn button_click_produces_call() -> Result<()> {
    let code = r#"
        use gui::{button::button, text::text};
        let calls = 0;
        let result = button(#on_press: |t| calls <- t ~ calls + 1, &text(&"Click"))
    "#;
    let mut h = InteractionHarness::new(code).await?;
    assert_eq!(h.watch("test::calls").await?, Value::I64(0));
    for n in 1..=2 {
        let msgs = h.click(WIDGET_HIT);
        expect_call(&msgs);
        h.dispatch_calls(&msgs).await?;
        assert_eq!(h.get_watched("test::calls"), Some(&Value::I64(n)));
    }
    Ok(())
}

/// A tap presses a button as a click does: a finger moves the pointer.
#[tokio::test(flavor = "current_thread")]
async fn button_tap_produces_call() -> Result<()> {
    use iced_core::touch;
    let code = r#"
        use gui::{button::button, text::text};
        let result = button(#on_press: |t| t, &text(&"Tap"))
    "#;
    let mut h = InteractionHarness::new(code).await?;
    let id = touch::Finger(1);
    let mut msgs = h.process_events(&[Event::Touch(touch::Event::FingerPressed {
        id,
        position: WIDGET_HIT,
    })]);
    msgs.extend(h.process_events(&[Event::Touch(touch::Event::FingerLifted {
        id,
        position: WIDGET_HIT,
    })]));
    assert_eq!(msgs.iter().filter(|m| matches!(m, super::Message::Call(..))).count(), 1);
    Ok(())
}

/// A row with fewer or more cells than the table has columns, rows of
/// different lengths and a table with no columns lay out and take input.
#[tokio::test(flavor = "current_thread")]
async fn table_cell_count_mismatch_lays_out() -> Result<()> {
    let cases = [
        "[table_column(&text(&\"A\")), table_column(&text(&\"B\")), table_column(&text(&\"C\"))], \
         &[[text(&\"1\"), text(&\"2\")]]",
        "[table_column(&text(&\"Only\"))], &[[text(&\"a\"), text(&\"b\"), text(&\"c\")]]",
        "[table_column(&text(&\"A\")), table_column(&text(&\"B\"))], \
         &[[], [text(&\"1\")], [text(&\"1\"), text(&\"2\"), text(&\"3\")]]",
        "[], &[[text(&\"orphan\")]]",
    ];
    for case in cases {
        let code = format_compact!("{IMPORTS};\nlet result = table(&{case})");
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

async fn toggle_dispatches(widget: &str) -> Result<()> {
    let code = format_compact!(
        r#"
        {IMPORTS};
        let state = false;
        let calls = 0;
        let toggle = |v| {{ state <- v; calls <- v ~ calls + 1; null }};
        let result = {widget}(#label: &"Toggle", #on_toggle: toggle, &state)
    "#
    );
    let mut h = InteractionHarness::new(&code).await?;
    assert_eq!(h.watch("test::state").await?, Value::Bool(false));
    assert_eq!(h.watch("test::calls").await?, Value::I64(0));
    for (i, value) in [true, false].into_iter().enumerate() {
        let msgs = h.click(WIDGET_HIT);
        expect_call_with_args(&msgs, |args| args.first() == Some(&Value::Bool(value)));
        h.dispatch_calls(&msgs).await?;
        assert_eq!(h.get_watched("test::state"), Some(&Value::Bool(value)));
        assert_eq!(h.get_watched("test::calls"), Some(&Value::I64(i as i64 + 1)));
    }
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn checkbox_toggle_produces_call() -> Result<()> {
    toggle_dispatches("checkbox").await
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
    toggle_dispatches("toggler").await
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
    let code = r#"
        use gui::text_input::text_input;
        let value = "";
        let calls = 0;
        let input = |s| { value <- s; calls <- s ~ calls + 1; null };
        let result = text_input(#on_input: input, &value)
    "#;
    let mut h = InteractionHarness::new(code).await?;
    assert_eq!(h.watch("test::value").await?, Value::from(""));
    assert_eq!(h.watch("test::calls").await?, Value::I64(0));
    h.click(WIDGET_HIT);
    for (i, (typed, value)) in [("a", "a"), ("b", "ab")].into_iter().enumerate() {
        let msgs = h.type_text(typed);
        expect_call_with_args(&msgs, |args| args.first() == Some(&Value::from(value)));
        h.dispatch_calls(&msgs).await?;
        assert_eq!(h.get_watched("test::value"), Some(&Value::from(value)));
        assert_eq!(h.get_watched("test::calls"), Some(&Value::I64(i as i64 + 1)));
    }
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
async fn radio_dispatches_every_click_without_initial_selection() -> Result<()> {
    let code = r#"
        use gui::{column::column, radio::radio};
        let selected = "none";
        let calls = 0;
        let pick = |v| { selected <- v; calls <- v ~ calls + 1; null };
        let result = column(#spacing: &20.0, &[
            radio(#label: &"A", #size: &20.0, #selected: &selected, #on_select: pick, &"a"),
            radio(#label: &"B", #size: &20.0, #selected: &selected, #on_select: pick, &"b")
        ])
    "#;
    let mut h = InteractionHarness::with_viewport(code, Size::new(300.0, 100.0)).await?;
    h.watch("test::selected").await?;
    h.watch("test::calls").await?;
    assert_eq!(h.get_watched("test::selected"), Some(&Value::from("none")));
    assert_eq!(h.get_watched("test::calls"), Some(&Value::I64(0)));
    for (i, (point, selected)) in [
        (Point::new(10.0, 10.0), "a"),
        (Point::new(10.0, 50.0), "b"),
        (Point::new(10.0, 10.0), "a"),
    ]
    .into_iter()
    .enumerate()
    {
        let msgs = h.click(point);
        expect_call_with_args(&msgs, |args| args.first() == Some(&Value::from(selected)));
        h.dispatch_calls(&msgs).await?;
        assert_eq!(h.get_watched("test::selected"), Some(&Value::from(selected)));
        assert_eq!(h.get_watched("test::calls"), Some(&Value::I64(i as i64 + 1)));
    }
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
    let code = r#"
        use gui::pick_list::pick_list;
        let selected = "Red";
        let calls = 0;
        let pick = |s| { selected <- s; calls <- s ~ calls + 1; null };
        let result = pick_list(#selected: &selected, #on_select: pick,
                               &["Red", "Green", "Blue"])
    "#;
    let mut h = InteractionHarness::with_viewport(code, Size::new(300.0, 200.0)).await?;
    assert_eq!(h.watch("test::selected").await?, Value::from("Red"));
    assert_eq!(h.watch("test::calls").await?, Value::I64(0));
    for (i, selected) in ["Green", "Red"].into_iter().enumerate() {
        let msgs = pick(&mut h, selected);
        expect_call_with_args(&msgs, |args| args.first() == Some(&Value::from(selected)));
        h.dispatch_calls(&msgs).await?;
        assert_eq!(h.get_watched("test::selected"), Some(&Value::from(selected)));
        assert_eq!(h.get_watched("test::calls"), Some(&Value::I64(i as i64 + 1)));
    }
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
    let code = r#"
        use gui::combo_box::combo_box;
        let selected = "Alpha";
        let calls = 0;
        let choose = |s| { selected <- s; calls <- s ~ calls + 1; null };
        let result = combo_box(#selected: &selected, #on_select: choose,
                               &["Alpha", "Beta", "Gamma"])
    "#;
    let mut h = InteractionHarness::with_viewport(code, Size::new(300.0, 200.0)).await?;
    assert_eq!(h.watch("test::selected").await?, Value::from("Alpha"));
    assert_eq!(h.watch("test::calls").await?, Value::I64(0));
    let _ = h.view();
    let _ = h.click(WIDGET_HIT);
    let _ = h.type_text("Gam");
    let msgs = h.press_key(iced_core::keyboard::key::Named::Enter);
    expect_call_with_args(&msgs, |args| args.first() == Some(&Value::from("Gamma")));
    h.dispatch_calls(&msgs).await?;
    assert_eq!(h.get_watched("test::selected"), Some(&Value::from("Gamma")));
    assert_eq!(h.get_watched("test::calls"), Some(&Value::I64(1)));
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

/// A key's text is what it types: none for a key that types a control
/// character.
#[test]
fn key_text_is_printable() {
    use crate::widgets::keyboard_area::key_event_to_value;
    use iced_core::keyboard::{Key, Modifiers, key::Named};
    let enter = Key::Named(Named::Enter);
    for control in ["\r", "\x01", "\x1b", "\x7f"] {
        assert_eq!(
            key_event_to_value(&enter, Modifiers::empty(), Some(control), false),
            key_event_to_value(&enter, Modifiers::empty(), None, false),
            "{control:?}"
        );
    }
    let a = Key::Character("a".into());
    assert_ne!(
        key_event_to_value(&a, Modifiers::empty(), Some("a"), false),
        key_event_to_value(&a, Modifiers::empty(), None, false)
    );
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
        let fields = graphix_compiler::rep::struct_fields(&v).expect("a struct");
        let (_, key) = graphix_compiler::rep::field_named(fields, "key").expect("key");
        assert_eq!(key, &Value::String(name.into()));
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

async fn button_press_rotates(handler: &str) -> Result<()> {
    let code = format_compact!(
        "{IMPORTS};\n\
         let charts = [1, 2, 3];\n\
         let result = button(\
             #on_press: {handler}, \
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
async fn button_press_rotates_seq() -> Result<()> {
    button_press_rotates("|e| seq e { charts <- array::rotate(charts) }").await
}

#[tokio::test(flavor = "current_thread")]
async fn button_press_rotates_sampled() -> Result<()> {
    button_press_rotates("|e| charts <- e ~ array::rotate(charts)").await
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
        h.inner.root.w.for_each_child(&mut |c| {
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
    h.inner.frame_state().clipboard.0 = Some("pasted".into());
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
    let opts = graphix_package_core::testing::find_bind_id(
        &h.inner.f.compiled.env,
        "test::opts",
    )?;
    let opts_ref = h.inner.f.gx.compile_ref(opts).await?;
    for _ in 0..3 {
        let v = opts_ref.last.clone().expect("the options");
        h.inner.f.gx.compile_ref(opts).await?.set(v)?;
        h.drain().await?;
    }
    let msgs = h.press_key(iced_core::keyboard::key::Named::Enter);
    expect_call_with_args(&msgs, |args| args.first() == Some(&Value::from("banana")));
    Ok(())
}

/// New options keep what the user is typing: the filter stands.
#[tokio::test(flavor = "current_thread")]
async fn combo_box_keeps_typing_across_new_options() -> Result<()> {
    let code = format!(
        "{IMPORTS};\n\
         let opts = [\"apple\", \"banana\", \"cherry\"];\n\
         let result = combo_box(#on_select: |s| null, #placeholder: &\"Pick\", &opts)"
    );
    let mut h = InteractionHarness::with_viewport(&code, Size::new(300.0, 200.0)).await?;
    let _ = h.view();
    let _ = h.click(WIDGET_HIT);
    let typed = h.type_text("ban");
    h.apply(&typed);
    let more = ["apple", "banana", "cherry", "date"].map(Value::from);
    h.inner.set_var("test::opts", Value::Array(ValArray::from_iter(more)))?;
    h.drain().await?;
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
    for expected in [true, false] {
        let msgs = h.click(WIDGET_HIT);
        expect_call_with_args(&msgs, |args| args.first() == Some(&Value::Bool(expected)));
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
    let doc = h.get_watched("test::doc").expect("doc");
    let fields = graphix_compiler::rep::struct_fields(doc).expect("a struct");
    let (_, text) = graphix_compiler::rep::field_named(fields, "text").expect("text");
    assert_eq!(text, &Value::from("abc"));
    Ok(())
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
    assert!(matches!(&msgs[..], [super::Message::Nop]), "{msgs:?}");
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
        expect_call(&msgs);
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
    let mut h = harness("progress_bar(#min: &1.0, #max: &0.0, &0.5)").await?;
    h.inner.render()
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

/// A context menu whose items go away closes: when they come back it
/// stays closed until the next right-click.
#[tokio::test(flavor = "multi_thread")]
async fn a_context_menu_without_items_closes() -> Result<()> {
    let code = format!(
        "{IMPORTS};\n\
         let show = true;\n\
         let items = select show {{ \
             true => [menu::action(#on_click: |c| c, &\"Copy\")], false => [] }};\n\
         let result = menu::context_menu(&items, \
             &container(#width: &`Fill, #height: &`Fixed(40.0), &text(&\"target\")))"
    );
    let mut h = InteractionHarness::with_viewport(&code, Size::new(400.0, 300.0)).await?;
    right_click(&mut h, Point::new(20.0, 20.0));
    let _ = h.move_cursor(Point::new(250.0, 280.0));
    for shown in [false, true] {
        h.inner.set_var("test::show", Value::Bool(shown))?;
        h.drain().await?;
        let _ = h.process_events(&[]);
    }
    let msgs = h.click(Point::new(60.0, 30.0));
    assert!(!msgs.iter().any(|m| matches!(m, super::Message::Call(..))), "{msgs:?}");
    Ok(())
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

#[tokio::test(flavor = "current_thread")]
async fn text_input_keeps_focus_across_resize() -> Result<()> {
    let mut h = InteractionHarness::new(
        r#"
        use gui::text_input::text_input;
        let value = "";
        let result = text_input(#on_input: |v| value <- v, &value)
    "#,
    )
    .await?;
    h.watch("test::value").await?;
    h.click(WIDGET_HIT);
    let msgs = h.type_text("a");
    h.dispatch_calls(&msgs).await?;
    h.resize(Size::new(400.0, 80.0));
    let msgs = h.type_text("b");
    expect_call_with_args(&msgs, |args| args.first() == Some(&Value::from("ab")));
    h.dispatch_calls(&msgs).await?;
    assert_eq!(h.get_watched("test::value"), Some(&Value::from("ab")));
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn menu_shortcuts_match_modified_characters() -> Result<()> {
    use iced_core::keyboard::{Event as KeyEvent, Key, Modifiers};

    for context in [false, true] {
        for (shortcut, base, modified, shift) in
            [("?", "/", "?", true), ("€", "e", "€", false), ("n", "n", "N", true)]
        {
            let widget = if context {
                r#"menu::context_menu(&[action], &text(&"target"))"#
            } else {
                r#"menu::bar(&[menu::menu(&"File", &[action])])"#
            };
            let code = format_compact!(
                r#"
                use gui::{{menu, text::text}};
                let calls = 0;
                let action = menu::action(
                    #shortcut: &menu::shortcut(#ctrl: true, #shift: {shift}, "{shortcut}")$,
                    #on_click: |t| calls <- t ~ calls + 1,
                    &"Action");
                let result = {widget}
            "#
            );
            let mut h =
                InteractionHarness::with_viewport(&code, Size::new(400.0, 200.0)).await?;
            assert_eq!(h.watch("test::calls").await?, Value::I64(0));
            if context {
                right_click(&mut h, WIDGET_HIT);
            }
            let mut event = char_key(base);
            let Event::Keyboard(KeyEvent::KeyPressed { modified_key, modifiers, .. }) =
                &mut event
            else {
                unreachable!()
            };
            *modified_key = Key::Character(modified.into());
            *modifiers = Modifiers::CTRL
                | if shift { Modifiers::SHIFT } else { Modifiers::empty() };
            let msgs = h.process_events(&[event]);
            expect_call(&msgs);
            h.dispatch_calls(&msgs).await?;
            assert_eq!(h.get_watched("test::calls"), Some(&Value::I64(1)));
        }
    }
    Ok(())
}

async fn two_context_menus() -> Result<InteractionHarness> {
    let code = r#"
        use gui::{column::column, menu, text::text, container::container};
        let del_a = 0;
        let copy_a = 0;
        let del_b = 0;
        let copy_b = 0;
        let row = |label, on_del, on_copy| menu::context_menu(
            &[menu::action(#shortcut: &menu::shortcut(#ctrl: true, "d")$, #on_click: on_del, &"Delete"),
              menu::action(#on_click: on_copy, &"Copy")],
            &container(#width: &`Fill, #height: &`Fixed(40.0), &text(&label))
        );
        let result = column(&[
            row("A", |c| del_a <- c ~ del_a + 1, |c| copy_a <- c ~ copy_a + 1),
            row("B", |c| del_b <- c ~ del_b + 1, |c| copy_b <- c ~ copy_b + 1)
        ])
    "#;
    let mut h = InteractionHarness::with_viewport(code, Size::new(400.0, 300.0)).await?;
    for var in ["test::del_a", "test::copy_a", "test::del_b", "test::copy_b"] {
        h.watch(var).await?;
    }
    Ok(h)
}

#[tokio::test(flavor = "current_thread")]
async fn opening_a_second_context_menu_closes_the_first() -> Result<()> {
    for shortcut in [false, true] {
        let mut h = two_context_menus().await?;
        right_click(&mut h, Point::new(50.0, 20.0));
        right_click(&mut h, Point::new(if shortcut { 300.0 } else { 20.0 }, 60.0));
        let msgs =
            if shortcut { h.press_ctrl("d") } else { h.click(Point::new(100.0, 70.0)) };
        h.dispatch_calls(&msgs).await?;
        for var in ["test::del_a", "test::copy_a", "test::del_b", "test::copy_b"] {
            assert_eq!(
                h.get_watched(var),
                Some(&Value::I64(i64::from(var == "test::del_b"))),
                "{var}, shortcut={shortcut}"
            );
        }
    }
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn a_menu_bar_heading_toggles_its_menu_closed() -> Result<()> {
    let mut h = InteractionHarness::with_viewport(
        r#"
        use gui::menu;
        let calls = 0;
        let result = menu::bar(&[menu::menu(&"File", &[
            menu::action(#on_click: |t| calls <- t ~ calls + 1, &"Action")
        ])])
    "#,
        Size::new(400.0, 300.0),
    )
    .await?;
    h.watch("test::calls").await?;
    for _ in 0..2 {
        h.click(WIDGET_HIT);
    }
    let msgs = h.click(Point::new(30.0, 50.0));
    h.dispatch_calls(&msgs).await?;
    assert_eq!(h.get_watched("test::calls"), Some(&Value::I64(0)));
    h.click(WIDGET_HIT);
    let msgs = h.click(Point::new(30.0, 50.0));
    h.dispatch_calls(&msgs).await?;
    assert_eq!(h.get_watched("test::calls"), Some(&Value::I64(1)));
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn context_menu_over_tooltip_can_choose_an_item() -> Result<()> {
    for tooltip in [true, false] {
        let child =
            if tooltip { r#"tooltip(#tip: &text(&"tip"), &child)"# } else { "child" };
        let code = format_compact!(
            r#"
            {IMPORTS};
            let calls = 0;
            let child = container(#width: &`Fill, #height: &`Fill, &text(&"target"));
            let result = menu::context_menu(
                &[menu::action(#on_click: |t| calls <- t ~ calls + 1, &"Choose")], &{child}
            )
        "#
        );
        let mut h =
            InteractionHarness::with_viewport(&code, Size::new(400.0, 300.0)).await?;
        h.watch("test::calls").await?;
        right_click(&mut h, Point::new(50.0, 50.0));
        let msgs = h.click(Point::new(80.0, 60.0));
        h.dispatch_calls(&msgs).await?;
        assert_eq!(
            h.get_watched("test::calls"),
            Some(&Value::I64(1)),
            "tooltip={tooltip}"
        );
    }
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn invalid_slider_ranges_are_inert() -> Result<()> {
    for (kind, size, hit) in [
        ("slider", Size::new(200.0, 22.0), Point::new(150.0, 10.0)),
        ("vertical_slider", Size::new(22.0, 200.0), Point::new(10.0, 50.0)),
    ] {
        for (min, max) in [("10.0", "0.0"), ("0.0 / 0.0", "10.0"), ("0.0", "0.0 / 0.0")] {
            let code = format_compact!(
                "{IMPORTS}; let result = {kind}(#min: &({min}), #max: &({max}), #on_change: |v| null, #on_release: |v| null, &5.0)"
            );
            let mut h = InteractionHarness::with_viewport(&code, size).await?;
            let msgs = h.click(hit);
            assert!(
                !msgs.iter().any(|m| matches!(m, super::Message::Call(..))),
                "{kind} {min}..{max}: {msgs:?}"
            );
            h.inner.render()?;
        }
    }
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn mouse_button_names_use_static_storage() -> Result<()> {
    let mut h = harness(
        "mouse_area(#on_press: |v| null, #on_release: |v| null, &text(&\"target\"))",
    )
    .await?;
    h.move_cursor(WIDGET_HIT);
    for button in [mouse::Button::Left, mouse::Button::Right, mouse::Button::Middle] {
        for event in
            [mouse::Event::ButtonPressed(button), mouse::Event::ButtonReleased(button)]
        {
            let msgs = h.process_events(&[Event::Mouse(event)]);
            expect_call(&msgs);
            for m in &msgs {
                if let super::Message::Call(_, args) = m {
                    let Some((name, [])) = graphix_compiler::rep::variant(&args[0])
                    else {
                        panic!("button name missing")
                    };
                    assert!(arcstr::ArcStr::is_static(name), "{name}");
                }
            }
        }
    }
    Ok(())
}

#[tokio::test(flavor = "multi_thread")]
async fn handler_presence_changes_request_a_frame() -> Result<()> {
    for widget in [
        "button(#on_press: handler, &text(&\"go\"))",
        "text_input(#on_input: handler, &\"\")",
        "checkbox(#on_toggle: handler, &false)",
        "toggler(#on_toggle: handler, &false)",
        "menu::context_menu(&[menu::action(#on_click: handler, &\"go\")], &text(&\"menu\"))",
    ] {
        let code = format_compact!(
            "{IMPORTS}; let enabled = false; let handler = select enabled {{ true => |v: Any| null, false => null }}; let change = |v: bool| enabled <- v; let result = {widget}"
        );
        let mut h = InteractionHarness::new(&code).await?;
        let change = h.inner.compile_named_callable("test::change").await?;
        h.inner.drain().await?;
        for enabled in [true, false] {
            h.inner.f.gx.call(change, ValArray::from_iter([Value::Bool(enabled)]))?;
            assert!(h.inner.drain().await?, "{widget}: enabled={enabled}");
        }
    }
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn text_editor_can_type_after_a_programmatic_clear() -> Result<()> {
    let code = format_compact!(
        "{IMPORTS}; let doc = \"abcdef\"; let result = text_editor(#on_edit: |s| doc <- s ~ \"\", &doc)"
    );
    let mut h = InteractionHarness::with_viewport(&code, Size::new(300.0, 100.0)).await?;
    h.watch("test::doc").await?;
    h.click(WIDGET_HIT);
    let msgs = h.press_key(iced_core::keyboard::key::Named::End);
    h.apply(&msgs);
    h.live(&[char_key("x")]).await?;
    assert_eq!(h.get_watched("test::doc"), Some(&Value::from("")));
    let msgs = h.process_events(&[char_key("y")]);
    let edits = h.process_editor_actions(&msgs);
    assert_eq!(edits.iter().map(|(_, v)| v).collect::<Vec<_>>(), [&Value::from("y")]);
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn rejected_toggles_return_to_the_program_value() -> Result<()> {
    for kind in ["checkbox", "toggler"] {
        let code = format_compact!(
            "{IMPORTS}; let seen = false; let result = {kind}(#on_toggle: |v| seen <- v, &false)"
        );
        let mut h = InteractionHarness::new(&code).await?;
        h.watch("test::seen").await?;
        for _ in 0..2 {
            let msgs = h.click(WIDGET_HIT);
            expect_call_with_args(&msgs, |a| a.first() == Some(&Value::Bool(true)));
            h.dispatch_calls(&msgs).await?;
            assert_eq!(h.get_watched("test::seen"), Some(&Value::Bool(true)));
        }
    }
    Ok(())
}

async fn rejected_text_edit(kind: &str, handler: &str) -> Result<()> {
    let code = format_compact!(
        "{IMPORTS}; let seen = \"\"; let result = {kind}(#{handler}: |s| seen <- s, &\"\")"
    );
    let mut h = InteractionHarness::with_viewport(&code, Size::new(300.0, 100.0)).await?;
    h.watch("test::seen").await?;
    h.click(WIDGET_HIT);
    for key in ["a", "b"] {
        h.live(&[char_key(key)]).await?;
        assert_eq!(h.get_watched("test::seen"), Some(&Value::from(key)));
    }
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn rejected_text_input_returns_to_the_program_value() -> Result<()> {
    rejected_text_edit("text_input", "on_input").await
}

#[tokio::test(flavor = "current_thread")]
async fn rejected_text_editor_returns_to_the_program_value() -> Result<()> {
    rejected_text_edit("text_editor", "on_edit").await
}

#[tokio::test(flavor = "current_thread")]
async fn continuous_slider_keys_and_wheel_move_one_percent() -> Result<()> {
    use iced_core::keyboard::{self, key::Named};
    for (name, size, hit) in [
        ("slider", Size::new(400.0, 22.0), Point::new(149.3, 10.0)),
        ("vertical_slider", Size::new(22.0, 400.0), Point::new(10.0, 250.7)),
    ] {
        let code = format_compact!(
            "{IMPORTS}; let result = {name}(#min: &0.0, #max: &100.0, #on_change: |v| null, &50.0)"
        );
        let mut h = InteractionHarness::with_viewport(&code, size).await?;
        h.move_cursor(hit);
        let value = |msgs: &[super::Message]| -> f64 {
            msgs.iter()
                .find_map(|m| match m {
                    super::Message::Call(_, args) => match args.first() {
                        Some(Value::F64(v)) => Some(*v),
                        _ => None,
                    },
                    _ => None,
                })
                .expect("slider callback")
        };
        assert!((value(&h.press_key(Named::ArrowUp)) - 51.0).abs() < 1e-10, "{name} key");
        h.process_events(&[Event::Keyboard(keyboard::Event::ModifiersChanged(
            keyboard::Modifiers::CTRL,
        ))]);
        assert!((value(&h.scroll(0.0, -1.0)) - 49.0).abs() < 1e-10, "{name} wheel");
        h.process_events(&[Event::Keyboard(keyboard::Event::ModifiersChanged(
            keyboard::Modifiers::empty(),
        ))]);
        assert!((value(&h.click(hit)) - 37.325).abs() < 0.0002, "{name} fine drag");
    }
    Ok(())
}

/// A stepped slider's keys, wheel and drags all land on min + k * step.
#[tokio::test(flavor = "current_thread")]
async fn stepped_slider_keys_wheel_and_drags_share_one_lattice() -> Result<()> {
    use iced_core::keyboard::{self, key::Named};
    for (name, size, hit) in [
        ("slider", Size::new(400.0, 22.0), Point::new(149.3, 10.0)),
        ("vertical_slider", Size::new(22.0, 400.0), Point::new(10.0, 250.7)),
    ] {
        let code = format_compact!(
            "{IMPORTS}; let result = {name}(#min: &0.5, #max: &10.5, #step: &1.0, #on_change: |v| null, &5.5)"
        );
        let mut h = InteractionHarness::with_viewport(&code, size).await?;
        h.move_cursor(hit);
        let value = |msgs: &[super::Message]| -> f64 {
            msgs.iter()
                .find_map(|m| match m {
                    super::Message::Call(_, args) => match args.first() {
                        Some(Value::F64(v)) => Some(*v),
                        _ => None,
                    },
                    _ => None,
                })
                .expect("slider callback")
        };
        assert_eq!(value(&h.press_key(Named::ArrowUp)), 6.5, "{name} key");
        h.process_events(&[Event::Keyboard(keyboard::Event::ModifiersChanged(
            keyboard::Modifiers::CTRL,
        ))]);
        assert_eq!(value(&h.scroll(0.0, -1.0)), 4.5, "{name} wheel");
        h.process_events(&[Event::Keyboard(keyboard::Event::ModifiersChanged(
            keyboard::Modifiers::empty(),
        ))]);
        let dragged = value(&h.click(hit));
        assert_eq!(dragged - dragged.floor(), 0.5, "{name} drag: {dragged}");
    }
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn continuous_slider_extreme_range_clicks_stay_inside() -> Result<()> {
    for (name, size, hit) in [
        ("slider", Size::new(400.0, 22.0), Point::new(100.0, 10.0)),
        ("vertical_slider", Size::new(22.0, 400.0), Point::new(10.0, 300.0)),
    ] {
        let code = format_compact!(
            "{IMPORTS}; let result = {name}(#min: &-1.7e308, #max: &1.7e308, #on_change: |v| null, &0.0)"
        );
        let mut h = InteractionHarness::with_viewport(&code, size).await?;
        let msgs = h.click(hit);
        let value = msgs
            .iter()
            .find_map(|m| match m {
                super::Message::Call(_, args) => match args.first() {
                    Some(Value::F64(v)) => Some(*v),
                    _ => None,
                },
                _ => None,
            })
            .expect("slider callback");
        assert!(
            value.is_finite() && (value / 1.7e308 + 0.5).abs() < 1e-6,
            "{name}: {value}"
        );
    }
    Ok(())
}
