use anyhow::Result;
use arcstr::literal;
use graphix_compiler::expr::ModuleResolver;
use graphix_package_core::testing::{self, RegisterFn, TestCtx};
use graphix_rt::GXEvent;
use poolshark::global::GPooled;
use tokio::sync::mpsc;

mod fs;

pub const REGISTER: &[RegisterFn] =
    &[graphix_package_core::register_test, crate::register_test];

pub const ROOT: arcstr::ArcStr = literal!(
    "\
mod core;\nuse core;\n\
mod fs"
);

#[allow(dead_code)]
pub async fn init_with_resolvers(
    sub: mpsc::Sender<GPooled<Vec<GXEvent>>>,
    resolvers: Vec<ModuleResolver>,
) -> Result<TestCtx> {
    testing::init_with_resolvers(sub, REGISTER, ROOT, resolvers).await
}

pub async fn init(sub: mpsc::Sender<GPooled<Vec<GXEvent>>>) -> Result<TestCtx> {
    testing::init(sub, REGISTER, ROOT).await
}

pub use testing::escape_path;
