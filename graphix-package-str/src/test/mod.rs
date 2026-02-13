use anyhow::Result;
use arcstr::literal;
use graphix_package_core::testing::{self, RegisterFn, TestCtx};
use graphix_rt::GXEvent;
use poolshark::global::GPooled;
use tokio::sync::mpsc;

mod string;

pub const REGISTER: &[RegisterFn] = &[
    graphix_package_core::register_test,
    crate::register_test,
    graphix_package_array::register_test,
];

pub const ROOT: arcstr::ArcStr = literal!(
    "\
mod core;\nuse core;\n\
mod str;\n\
mod array"
);

#[allow(dead_code)]
pub async fn init(sub: mpsc::Sender<GPooled<Vec<GXEvent>>>) -> Result<TestCtx> {
    testing::init(sub, REGISTER, ROOT).await
}
