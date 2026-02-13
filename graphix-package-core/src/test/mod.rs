use crate::testing::{self, RegisterFn, TestCtx};
use anyhow::Result;
use arcstr::literal;
use graphix_rt::GXEvent;
use poolshark::global::GPooled;
use tokio::sync::mpsc;

mod lang;
mod lib;

pub const REGISTER: &[RegisterFn] = &[
    crate::register_test,
    graphix_package_array::register_test,
    graphix_package_map::register_test,
    graphix_package_str::register_test,
    graphix_package_time::register_test,
    graphix_package_net::register_test,
    graphix_package_re::register_test,
    graphix_package_rand::register_test,
    graphix_package_fs::register_test,
];

pub const ROOT: arcstr::ArcStr = literal!(
    "\
mod core;\nuse core;\n\
mod array;\n\
mod map;\n\
mod str;\n\
mod time;\n\
mod net;\n\
mod re;\n\
mod rand;\n\
mod fs"
);

pub async fn init(sub: mpsc::Sender<GPooled<Vec<GXEvent>>>) -> Result<TestCtx> {
    testing::init(sub, REGISTER, ROOT).await
}
