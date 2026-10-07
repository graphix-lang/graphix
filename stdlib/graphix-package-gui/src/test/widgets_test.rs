use super::GuiTestHarness;
use anyhow::Result;
use graphix_package_core::testing;
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
use gui::progress_bar::{self, *};\n\
use gui::text_editor::{self, *};\n\
use gui::row::{self, *};\n\
use gui::column::{self, *};\n\
use gui::container::{self, *};\n\
use gui::scrollable::{self, *};\n\
use gui::space::{self, *};\n\
use gui::rule::{self, *};\n\
use gui::stack::{self, *};\n\
use gui::tooltip::{self, *};\n\
use gui::vertical_slider::{self, *};\n\
use gui::combo_box::{self, *};\n\
use gui::mouse_area::{self, *};\n\
use gui::canvas::{self, *};\n\
use gui::chart::{self, *};\n\
use gui::image::{self, *};\n\
use gui::grid::{self, *};\n\
use gui::qr_code::{self, *};\n\
use gui::markdown::{self, *};\n\
use gui::table::{self, *};\n\
use gui::menu::{self, *}";

/// Compile `let result = <widget_expr>` under the standard imports.
async fn harness(widget_expr: &str) -> Result<GuiTestHarness> {
    let code = format!("{IMPORTS};\nlet result = {widget_expr}");
    GuiTestHarness::new(&code).await
}

/// The widget built from `decls` and `widget` takes the update when the
/// program writes `value` to `var`, and draws after it.
async fn reacts(decls: &str, widget: &str, var: &str, value: Value) -> Result<()> {
    let code = format!("{IMPORTS};\n{decls};\nlet result = {widget}");
    let mut h = GuiTestHarness::new(&code).await?;
    h.drain().await?;
    h.render().await?;
    let bid = testing::find_bind_id(&h.compiled.env, var)?;
    h.gx.compile_ref(bid).await?.set(value)?;
    assert!(h.drain().await?, "writing {var} changes the widget");
    h.render().await
}

/// Build, lay out and draw the tree.
macro_rules! view {
    ($h:expr) => {{
        $h.render().await?;
    }};
}

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

#[tokio::test(flavor = "current_thread")]
async fn button_renders() -> Result<()> {
    let h = harness(r#"button(#on_press: |_| null, &text(&"Click me"))"#).await?;
    view!(h);
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn checkbox_renders() -> Result<()> {
    let h = harness(r#"checkbox(#label: &"Accept", &false)"#).await?;
    view!(h);
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn toggler_renders() -> Result<()> {
    let h = harness(r#"toggler(#label: &"Dark mode", &true)"#).await?;
    view!(h);
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn text_input_renders() -> Result<()> {
    let h = harness(r#"text_input(#placeholder: &"Type here...", &"initial")"#).await?;
    view!(h);
    Ok(())
}

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

#[tokio::test(flavor = "current_thread")]
async fn radio_renders() -> Result<()> {
    let h = harness(r#"radio(#label: &"Option A", #selected: &"option_a", &"option_a")"#)
        .await?;
    view!(h);
    Ok(())
}

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

#[tokio::test(flavor = "current_thread")]
async fn progress_bar_renders() -> Result<()> {
    let h = harness(r#"progress_bar(#min: &0.0, #max: &1.0, &0.5)"#).await?;
    view!(h);
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn text_editor_renders() -> Result<()> {
    let h = harness(r#"text_editor(#placeholder: &"Edit...", &"Hello\nWorld")"#).await?;
    view!(h);
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn row_with_children() -> Result<()> {
    let h = harness("row(#spacing: &10.0, &[text(&\"Left\"), text(&\"Right\")])").await?;
    view!(h);
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn column_with_children() -> Result<()> {
    let h =
        harness("column(#spacing: &10.0, &[text(&\"Top\"), text(&\"Bottom\")])").await?;
    view!(h);
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn container_renders() -> Result<()> {
    let h =
        harness("container(#halign: &`Center, #valign: &`Center, &text(&\"Centered\"))")
            .await?;
    view!(h);
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn scrollable_renders() -> Result<()> {
    let h = harness(r#"scrollable(&text(&"Scrollable content"))"#).await?;
    view!(h);
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn space_renders() -> Result<()> {
    let h = harness(r#"space(#width: &`Fill, #height: &`Fixed(20.0))"#).await?;
    view!(h);
    Ok(())
}

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

#[tokio::test(flavor = "current_thread")]
async fn stack_renders() -> Result<()> {
    let h = harness("stack(&[text(&\"Background\"), text(&\"Foreground\")])").await?;
    view!(h);
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn tooltip_renders() -> Result<()> {
    let h = harness(
        "tooltip(#tip: &text(&\"Tooltip text\"), #position: &`Top, &text(&\"Hover me\"))",
    )
    .await?;
    view!(h);
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn vertical_slider_renders() -> Result<()> {
    let h = harness(r#"vertical_slider(#min: &0.0, #max: &100.0, &50.0)"#).await?;
    view!(h);
    Ok(())
}

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

#[tokio::test(flavor = "current_thread")]
async fn mouse_area_renders() -> Result<()> {
    let h = harness("mouse_area(#on_press: |_| null, &text(&\"Click zone\"))").await?;
    view!(h);
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn text_with_reactive_ref() -> Result<()> {
    reacts(
        r#"let msg = "hello""#,
        r#"text(&msg)"#,
        "test::msg",
        Value::String("bye".into()),
    )
    .await
}

#[tokio::test(flavor = "current_thread")]
async fn nested_widget_with_reactive_ref() -> Result<()> {
    reacts(
        r#"let label = "initial""#,
        r#"container(&column(&[text(&label), button(&text(&"btn"))]))"#,
        "test::label",
        Value::String("next".into()),
    )
    .await
}

#[tokio::test(flavor = "current_thread")]
async fn slider_with_reactive_ref() -> Result<()> {
    reacts(
        r#"let val = 25.0"#,
        r#"slider(#min: &0.0, #max: &100.0, &val)"#,
        "test::val",
        Value::F64(75.0),
    )
    .await
}

#[tokio::test(flavor = "current_thread")]
async fn checkbox_with_reactive_value() -> Result<()> {
    reacts(
        r#"let checked = false"#,
        r#"checkbox(#label: &"Check me", &checked)"#,
        "test::checked",
        Value::Bool(true),
    )
    .await
}

#[tokio::test(flavor = "current_thread")]
async fn toggler_with_reactive_value() -> Result<()> {
    reacts(
        r#"let toggled = false"#,
        r#"toggler(#label: &"Toggle me", &toggled)"#,
        "test::toggled",
        Value::Bool(true),
    )
    .await
}

#[tokio::test(flavor = "current_thread")]
async fn text_input_with_reactive_value() -> Result<()> {
    reacts(
        r#"let val = """#,
        r#"text_input(#placeholder: &"Type...", &val)"#,
        "test::val",
        Value::String("typed".into()),
    )
    .await
}

#[tokio::test(flavor = "current_thread")]
async fn pick_list_with_reactive_selection() -> Result<()> {
    reacts(r#"let sel = "Red""#, r#"pick_list(#selected: &sel, #placeholder: &"Choose", &["Red", "Green", "Blue"])"#, "test::sel", Value::String("Blue".into())).await
}

#[tokio::test(flavor = "multi_thread")]
async fn row_with_reactive_children() -> Result<()> {
    reacts(
        r#"let n = 1;
let items = select n { 1 => [text(&"one")], _ => [text(&"one"), text(&"two")] }"#,
        r#"row(&items)"#,
        "test::n",
        Value::I64(2),
    )
    .await
}

#[tokio::test(flavor = "current_thread")]
async fn image_renders() -> Result<()> {
    let h = harness(r#"image(&"/dev/null")"#).await?;
    view!(h);
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn svg_renders() -> Result<()> {
    let h = harness(r#"image(&`Svg("<svg></svg>"))"#).await?;
    view!(h);
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn grid_renders() -> Result<()> {
    let h = harness(r#"grid(#columns: &`Fixed(2), &[text(&"A"), text(&"B")])"#).await?;
    view!(h);
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn grid_with_params() -> Result<()> {
    let h = harness(
        "grid(\
            #columns: &`Fluid(100.0),\
            #spacing: &10.0,\
            #width: &400.0,\
            #height: &`AspectRatio(0.5),\
            &[text(&\"One\"), text(&\"Two\"), text(&\"Three\")])",
    )
    .await?;
    view!(h);
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn qr_code_renders() -> Result<()> {
    let h = harness(r#"qr_code(&"hello")"#).await?;
    view!(h);
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn qr_code_with_cell_size() -> Result<()> {
    let h = harness(r#"qr_code(#cell_size: &4.0, &"test data")"#).await?;
    view!(h);
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn markdown_renders() -> Result<()> {
    let h = harness(r##"markdown(&"# Hello\n**bold**")"##).await?;
    view!(h);
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn markdown_with_params() -> Result<()> {
    let h = harness(
        "markdown(\
            #on_link: |url| println(url),\
            #text_size: &18.0,\
            #spacing: &10.0,\
            &\"Some *markdown* text\")",
    )
    .await?;
    view!(h);
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn table_renders() -> Result<()> {
    let h = harness(
        "table(\
            &[table_column(&text(&\"Name\")), table_column(&text(&\"Value\"))],\
            &[[text(&\"a\"), text(&\"1\")]])",
    )
    .await?;
    view!(h);
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn table_with_params() -> Result<()> {
    let h = harness(
        "table(\
            #padding: &8.0,\
            #separator: &1.0,\
            &[table_column(#halign: &`Right, &text(&\"Col\"))],\
            &[[text(&\"val\")]])",
    )
    .await?;
    view!(h);
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn menu_bar_renders() -> Result<()> {
    let h = harness(
        "menu::bar(&[\
            menu::menu(&\"File\", &[\
                menu::action(#on_click: |_| null, &\"New\"),\
                menu::divider(),\
                menu::action(&\"Quit\")\
            ])\
        ])",
    )
    .await?;
    view!(h);
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn context_menu_renders() -> Result<()> {
    let h = harness(
        "menu::context_menu(\
            &[menu::action(#on_click: |_| null, &\"Copy\"), menu::divider()],\
            &text(&\"Right-click me\"))",
    )
    .await?;
    view!(h);
    Ok(())
}

/// Sizes text cannot be laid out at, and coordinates that are not
/// numbers, are refused as updates and the last good ones kept.
#[tokio::test(flavor = "current_thread")]
async fn bad_sizes_and_coordinates_are_refused() -> Result<()> {
    for (decls, widget, var, bad) in [
        ("let s = 16.0", r#"text(#size: &s, &"t")"#, "test::s", Value::F64(0.0)),
        ("let s = 16.0", r#"text(#size: &s, &"t")"#, "test::s", Value::F64(-2.0)),
        (
            "let x = 10.0",
            "canvas(#width: &`Fixed(100.0), #height: &`Fixed(100.0), \
             &[`Circle({center: {x, y: 10.0}, radius: 5.0, fill: null, stroke: null})])",
            "test::x",
            Value::F64(f64::NAN),
        ),
    ] {
        let code = format!("{IMPORTS};\n{decls};\nlet result = {widget}");
        let mut h = GuiTestHarness::new(&code).await?;
        h.drain().await?;
        let bid = testing::find_bind_id(&h.compiled.env, var)?;
        h.gx.compile_ref(bid).await?.set(bad.clone())?;
        assert!(h.drain().await.is_err(), "{widget} takes {bad:?}");
        h.render().await?;
    }
    Ok(())
}

#[test]
fn image_sources_compare_by_content() {
    use crate::types::ImageSourceV;
    let a = iced_core::Bytes::from_static(b"png bytes");
    let b = iced_core::Bytes::from(b"png bytes".to_vec());
    assert!(ImageSourceV::Bytes(a.clone()).same_as(&ImageSourceV::Bytes(a.clone())));
    assert!(ImageSourceV::Bytes(a.clone()).same_as(&ImageSourceV::Bytes(b)));
    let c = iced_core::Bytes::from_static(b"other");
    assert!(!ImageSourceV::Bytes(a).same_as(&ImageSourceV::Bytes(c)));
    assert!(
        !ImageSourceV::Path("a.png".into()).same_as(&ImageSourceV::Svg("a.png".into()))
    );
}
