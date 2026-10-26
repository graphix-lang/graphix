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
