// This crate exists solely to host the language and stdlib integration tests.
// It depends on all stdlib packages so that TEST_REGISTER includes everything,
// breaking the circular dev-dependency that previously existed in
// graphix-package-core.

#[cfg(test)]
pub(crate) const TEST_REGISTER: &[graphix_package_core::testing::RegisterFn] = &[
    <graphix_package_core::P as graphix_package::Package<graphix_rt::NoExt>>::register,
    <graphix_package_array::P as graphix_package::Package<graphix_rt::NoExt>>::register,
    <graphix_package_map::P as graphix_package::Package<graphix_rt::NoExt>>::register,
    <graphix_package_str::P as graphix_package::Package<graphix_rt::NoExt>>::register,
    <graphix_package_time::P as graphix_package::Package<graphix_rt::NoExt>>::register,
    <graphix_package_net::P as graphix_package::Package<graphix_rt::NoExt>>::register,
    <graphix_package_re::P as graphix_package::Package<graphix_rt::NoExt>>::register,
    <graphix_package_rand::P as graphix_package::Package<graphix_rt::NoExt>>::register,
    <graphix_package_fs::P as graphix_package::Package<graphix_rt::NoExt>>::register,
];

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
