//! A runtime that stops answers every request it was sent, with an error
//! once it is gone: a request sent while its run loop ends is never left
//! waiting forever.

use anyhow::Result;
use graphix_rt::{GXConfig, GXEvent, GXRt, NoExt};
use poolshark::global::GPooled;
use std::time::Duration;
use tokio::{sync::mpsc, task::JoinSet};

type Batch = GPooled<Vec<GXEvent>>;

const CALLERS: usize = 8;
const TRIALS: usize = 3000;

#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn a_request_racing_the_stop_is_answered() -> Result<()> {
    for trial in 0..TRIALS {
        let ctx = GXRt::<NoExt>::new_state()?;
        let (tx, mut rx) = mpsc::channel::<Batch>(100);
        tokio::spawn(async move { while rx.recv().await.is_some() {} });
        let gx = GXConfig::builder(ctx, tx).build()?.start().await?;
        let mut callers = JoinSet::new();
        for _ in 0..CALLERS {
            let gx = gx.clone();
            callers.spawn(async move { while gx.get_env().await.is_ok() {} });
        }
        tokio::task::yield_now().await;
        gx.abort();
        let all = async { while callers.join_next().await.is_some() {} };
        if tokio::time::timeout(Duration::from_secs(30), all).await.is_err() {
            anyhow::bail!(
                "trial {trial}: a request sent as the runtime stopped never answered"
            )
        }
    }
    Ok(())
}
