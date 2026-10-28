use super::GuiTestHarness;
use crate::widgets::{Child, Children};
use anyhow::Result;
use graphix_package_core::testing::find_bind_id;
use graphix_rt::{NoExt, Ref};
use netidx::{protocol::valarray::ValArray, publisher::Value};

async fn fixture() -> Result<GuiTestHarness> {
    GuiTestHarness::new(
        r#"
        use gui::text::text;
        let size = 0.0;
        let good = text(#size: &16.0, &"good");
        let bad = text(#size: &size, &"bad");
        let source = [good];
        let proposed = [good, bad];
        let single = good;
        let repair = |t: Any| size <- t ~ 16.0;
        let result = text(&"fixture")
    "#,
    )
    .await
}

async fn named_ref(h: &GuiTestHarness, name: &str) -> Result<Ref<NoExt>> {
    h.f.gx.compile_ref(find_bind_id(&h.f.compiled.env, name)?).await
}

#[tokio::test(flavor = "multi_thread")]
async fn children_survive_a_failed_recompile_and_retry() -> Result<()> {
    let mut h = fixture().await?;
    let source = named_ref(&h, "test::source").await?;
    let proposed = named_ref(&h, "test::proposed").await?.last.clone().unwrap();
    let mut children: Children<NoExt> = Children::compile(&h.f.gx, source).await?;
    let (id, original) = (children.r.id, children.r.last.clone());
    let rt = tokio::runtime::Handle::current();
    for _ in 0..2 {
        assert!(
            tokio::task::block_in_place(|| children.update(&rt, &h.f.gx, id, &proposed))
                .is_err()
        );
        assert_eq!(children.ws.len(), 1);
        assert_eq!(children.r.last, original);
    }
    let repair = h.compile_named_callable("test::repair").await?;
    h.call_callback(repair, ValArray::from_iter([Value::Null])).await?;
    assert!(
        tokio::task::block_in_place(|| children.update(&rt, &h.f.gx, id, &proposed))?
    );
    assert_eq!(children.ws.len(), 2);
    assert_eq!(children.r.last, Some(proposed));
    Ok(())
}

#[tokio::test(flavor = "multi_thread")]
async fn a_single_child_survives_a_failed_recompile_and_retry() -> Result<()> {
    let mut h = fixture().await?;
    let source = named_ref(&h, "test::single").await?;
    let proposed = named_ref(&h, "test::bad").await?.last.clone().unwrap();
    let mut child = Child::compile(&h.f.gx, source).await?;
    let (id, original) = (child.r.id, child.r.last.clone());
    let rt = tokio::runtime::Handle::current();
    for _ in 0..2 {
        assert!(
            tokio::task::block_in_place(|| child.update(&rt, &h.f.gx, id, &proposed))
                .is_err()
        );
        assert_eq!(child.r.last, original);
    }
    let repair = h.compile_named_callable("test::repair").await?;
    h.call_callback(repair, ValArray::from_iter([Value::Null])).await?;
    assert!(tokio::task::block_in_place(|| child.update(&rt, &h.f.gx, id, &proposed))?);
    assert_eq!(child.r.last, Some(proposed));
    Ok(())
}

/// A result that changes rebuilds the harness's root, as a window's
/// content is rebuilt.
#[tokio::test(flavor = "multi_thread")]
async fn a_changed_result_rebuilds_the_root() -> Result<()> {
    use crate::widgets::canvas::CanvasW;
    let mut h = GuiTestHarness::new(
        r#"
        use gui::{color, canvas::canvas};
        let n = 0;
        let line = `Line({from: {x: 0.0, y: 0.0}, to: {x: 1.0, y: 1.0}, color: color(#r: 1.0)$, width: 1.0});
        let result = select n { 0 => canvas(&[]), _ => canvas(&[line]) }
    "#,
    )
    .await?;
    let shapes = |h: &GuiTestHarness| {
        h.root
            .w
            .as_any()
            .downcast_ref::<CanvasW<NoExt>>()
            .expect("a canvas")
            .shape_count()
    };
    assert_eq!(shapes(&h), Some(0));
    h.set_var("test::n", Value::I64(1))?;
    h.drain().await?;
    assert_eq!(shapes(&h), Some(1));
    Ok(())
}

/// A table whose cell changes its widget keeps the cells beside it.
#[tokio::test(flavor = "multi_thread")]
async fn a_changed_cell_keeps_its_row_siblings() -> Result<()> {
    use crate::widgets::{table::TableW, text::TextW};
    let mut h = GuiTestHarness::new(
        r#"
        use gui::{text::text, text_input::text_input, table::{table, table_column}};
        let n = 0;
        let a = text(&"a");
        let b = select n { 0 => text(&"b"), _ => text_input(&"c") };
        let result = table(&[table_column(&text(&"A")), table_column(&text(&"B"))], &[[a, b]])
    "#,
    )
    .await?;
    let first = |h: &GuiTestHarness| {
        let t = h.root.w.as_any().downcast_ref::<TableW<NoExt>>().expect("a table");
        assert_eq!(t.rows.ws.len(), 2);
        let a = t.rows.ws[0].1.as_any().downcast_ref::<TextW<NoExt>>().expect("text");
        a.content_ref()
    };
    let before = first(&h);
    h.set_var("test::n", Value::I64(1))?;
    assert!(h.drain().await?, "the changed cell changes the table");
    assert_eq!(first(&h), before, "the unchanged cell is kept");
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn a_thousand_widgets_receive_their_updates() -> Result<()> {
    use crate::widgets::text::TextW;
    use compact_str::format_compact;
    use poolshark::local::LPooled;
    use std::{fmt::Write, time::Instant};

    let mut code: LPooled<String> = LPooled::take();
    code.push_str("use gui::{text::text, column::column}; let value = \"before\"; let result = column(&[");
    for i in 0..1000 {
        if i > 0 {
            code.push(',');
        }
        write!(code, "text(&\"[value]-{i}\")")?;
    }
    code.push_str("])");
    let mut h = GuiTestHarness::new(&code).await?;
    let mut ids: LPooled<Vec<_>> = LPooled::take();
    h.root.w.for_each_child_mut(&mut |w| {
        ids.push(w.as_any().downcast_ref::<TextW<NoExt>>().unwrap().content_ref());
    });
    assert_eq!(ids.len(), 1000);
    let rt = tokio::runtime::Handle::current();
    let mut times: LPooled<Vec<_>> = LPooled::take();
    for value in
        ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten"]
    {
        let values: LPooled<Vec<_>> = (0..ids.len())
            .map(|i| Value::String(format_compact!("{value}-{i}").as_str().into()))
            .collect();
        let start = Instant::now();
        for (id, value) in ids.iter().zip(values.iter()) {
            assert!(super::update_widget(&mut h.root, &h.f.gx, &rt, *id, value)?);
        }
        times.push(start.elapsed());
        let mut expected = values.iter();
        h.root.w.for_each_child_mut(&mut |w| {
            assert_eq!(
                w.as_any().downcast_ref::<TextW<NoExt>>().unwrap().content(),
                match expected.next().unwrap() {
                    Value::String(s) => Some(s.as_str()),
                    _ => unreachable!(),
                }
            );
        });
    }
    times.sort_unstable();
    eprintln!("1000 updates through 1000 widgets: median {:?}", times[5]);
    h.set_var("test::value", Value::String(arcstr::literal!("runtime")))?;
    h.drain().await?;
    let mut index = 0;
    h.root.w.for_each_child_mut(&mut |w| {
        assert_eq!(
            w.as_any().downcast_ref::<TextW<NoExt>>().unwrap().content(),
            Some(format_compact!("runtime-{index}").as_str())
        );
        index += 1;
    });
    Ok(())
}
