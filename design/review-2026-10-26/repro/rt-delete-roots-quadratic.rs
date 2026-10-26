//! rt-delete-roots-quadratic: deleting n root nodes (Refs) costs O(n^2):
//! GX::nodes is an IndexMap and ToGX::Delete uses shift_remove.
//! Run: copy to graphix-rt/tests/review_rt_1.rs, then
//!   flock ~/tmp/cargo.lock cargo test -p graphix-rt --test review_rt_1 deleting -- --nocapture
//! Expected: time roughly linear in n.
//! Observed (dev profile): 2000 -> 5.4 ms, 4000 -> 20.3 ms, 8000 -> 77.9 ms,
//!   16000 -> 320 ms (4x per doubling).
use anyhow::Result;
use arcstr::literal;
use graphix_compiler::{Scope, expr::ModPath};
use graphix_rt::{GXConfig, GXEvent, GXRt, NoExt};
use poolshark::global::GPooled;
use tokio::sync::mpsc;

type Batch = GPooled<Vec<GXEvent>>;

/// Dropping n refs among n roots: each delete shifts the roots after it.
#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn deleting_many_refs() -> Result<()> {
    for n in [2000usize, 4000, 8000, 16000] {
        let ctx = GXRt::<NoExt>::new_state()?;
        let (tx, mut rx) = mpsc::channel::<Batch>(100);
        tokio::spawn(async move { while rx.recv().await.is_some() {} });
        let gx = GXConfig::builder(ctx, tx).build()?.start().await?;
        let _m = gx.compile(literal!("let x = 0")).await?;
        let env = gx.get_env().await?;
        let r = gx.compile_ref_by_name(&env, &Scope::root(), &ModPath::from(["x"])).await?;
        let mut refs = Vec::with_capacity(n);
        for _ in 0..n {
            refs.push(gx.compile_ref(r.bid).await?);
        }
        gx.get_env().await?;
        let st = std::time::Instant::now();
        // oldest first: every delete shifts every later root
        drop(refs);
        gx.get_env().await?;
        let stats = gx.env_stats().await?;
        eprintln!(
            "PROBE delete {n} refs: {:?} (ref_var_total after {})",
            st.elapsed(),
            stats.ref_var_total
        );
    }
    Ok(())
}
