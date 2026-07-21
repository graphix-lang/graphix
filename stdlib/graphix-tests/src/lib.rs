// This crate exists solely to host the language and stdlib integration tests.
// It depends on all stdlib packages so that TEST_REGISTER includes everything,
// breaking the circular dev-dependency that previously existed in
// graphix-package-core.

// Auto-discovered from this crate's [dependencies] (the `graphix-package-*`
// list IS the curation — it deliberately omits bench/gui/tui).
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
