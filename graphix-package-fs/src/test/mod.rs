use anyhow::Result;
use graphix_package_core::testing::{self, TestCtx};
use graphix_rt::GXEvent;
use poolshark::global::GPooled;
pub use testing::escape_path;
use tokio::sync::mpsc;

mod dir_ops;
mod integration;
mod metadata;
mod read;
mod readdir;
mod tempdir;
mod watch;
mod write;

pub async fn init(sub: mpsc::Sender<GPooled<Vec<GXEvent>>>) -> Result<TestCtx> {
    testing::init(sub, crate::TEST_REGISTER, crate::TEST_ROOT).await
}
