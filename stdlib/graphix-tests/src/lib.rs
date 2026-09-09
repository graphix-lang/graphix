// Hosts the language and stdlib integration tests; depends on every
// stdlib package so TEST_REGISTER includes them all.

// Discovered from this crate's [dependencies], which deliberately omit
// bench/gui/tui.
#[cfg(test)]
pub(crate) const TEST_REGISTER: &[&dyn graphix_package::Package<graphix_rt::NoExt>] =
    graphix_package::package_refs!();

#[cfg(test)]
pub(crate) async fn init(
    sub: tokio::sync::mpsc::Sender<poolshark::global::GPooled<Vec<graphix_rt::GXEvent>>>,
) -> anyhow::Result<graphix_package_core::testing::TestCtx> {
    graphix_package_core::testing::init(sub, TEST_REGISTER).await
}

#[cfg(test)]
mod lang;
#[cfg(test)]
mod lib_tests;
