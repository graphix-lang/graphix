use crate::testing::{self, TestCtx};
use anyhow::Result;
use graphix_rt::GXEvent;
use poolshark::global::GPooled;
use tokio::sync::mpsc;

mod lang;
mod lib;

pub async fn init(sub: mpsc::Sender<GPooled<Vec<GXEvent>>>) -> Result<TestCtx> {
    testing::init(sub, crate::TEST_REGISTER, crate::TEST_ROOT).await
}
