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
    h.gx.compile_ref(find_bind_id(&h.compiled.env, name)?).await
}

#[tokio::test(flavor = "multi_thread")]
async fn children_survive_a_failed_recompile_and_retry() -> Result<()> {
    let mut h = fixture().await?;
    let source = named_ref(&h, "test::source").await?;
    let proposed = named_ref(&h, "test::proposed").await?.last.clone().unwrap();
    let mut children = Children::compile(&h.gx, source).await?;
    let (id, original) = (children.r.id, children.r.last.clone());
    let rt = tokio::runtime::Handle::current();
    for _ in 0..2 {
        assert!(
            tokio::task::block_in_place(|| children.update(&rt, &h.gx, id, &proposed))
                .is_err()
        );
        assert_eq!(children.ws.len(), 1);
        assert_eq!(children.r.last, original);
    }
    let repair = h.compile_named_callable("test::repair").await?;
    h.call_callback(repair, ValArray::from_iter([Value::Null])).await?;
    assert!(tokio::task::block_in_place(|| children.update(&rt, &h.gx, id, &proposed))?);
    assert_eq!(children.ws.len(), 2);
    assert_eq!(children.r.last, Some(proposed));
    Ok(())
}

#[tokio::test(flavor = "multi_thread")]
async fn a_single_child_survives_a_failed_recompile_and_retry() -> Result<()> {
    let mut h = fixture().await?;
    let source = named_ref(&h, "test::single").await?;
    let proposed = named_ref(&h, "test::bad").await?.last.clone().unwrap();
    let mut child = Child::compile(&h.gx, source).await?;
    let (id, original) = (child.r.id, child.r.last.clone());
    let rt = tokio::runtime::Handle::current();
    for _ in 0..2 {
        assert!(
            tokio::task::block_in_place(|| child.update(&rt, &h.gx, id, &proposed))
                .is_err()
        );
        assert_eq!(child.r.last, original);
    }
    let repair = h.compile_named_callable("test::repair").await?;
    h.call_callback(repair, ValArray::from_iter([Value::Null])).await?;
    assert!(tokio::task::block_in_place(|| child.update(&rt, &h.gx, id, &proposed))?);
    assert_eq!(child.r.last, Some(proposed));
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn reconciliation_preserves_old_entries_until_success() -> Result<()> {
    use crate::widgets::reconcile;
    let mut old = vec![(Value::I64(1), "first"), (Value::I64(2), "second")];
    let items = || [2, 3, 1].map(Value::I64).into_iter().collect();
    assert!(
        reconcile(&mut old, items(), |_| async { anyhow::bail!("bad item") })
            .await
            .is_err()
    );
    assert_eq!(old, [(Value::I64(1), "first"), (Value::I64(2), "second")]);
    reconcile(&mut old, items(), |v| async move {
        assert_eq!(v, Value::I64(3), "retained entries must not compile again");
        Ok("fresh")
    })
    .await?;
    assert_eq!(
        old,
        [(Value::I64(2), "second"), (Value::I64(3), "fresh"), (Value::I64(1), "first")]
    );
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn reconciliation_compiles_fresh_entries_concurrently() -> Result<()> {
    use crate::widgets::reconcile;
    use std::time::Duration;
    use tokio::sync::Barrier;
    use triomphe::Arc;

    let barrier = Arc::new(Barrier::new(3));
    let mut old = Vec::new();
    let items = [1, 2, 3].map(Value::I64).into_iter().collect();
    tokio::time::timeout(
        Duration::from_secs(5),
        reconcile(&mut old, items, |v| {
            let barrier = barrier.clone();
            async move {
                barrier.wait().await;
                Ok(v)
            }
        }),
    )
    .await??;
    assert_eq!(old.len(), 3);
    Ok(())
}
