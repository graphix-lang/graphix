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
/// One test per `testing::Mode` for each named `async fn(Mode) -> Result<()>`.
macro_rules! modes {
    ($($test:ident),+ $(,)?) => {$(
        mod $test {
            use graphix_package_core::testing::Mode;

            #[tokio::test(flavor = "current_thread")]
            async fn interp() -> anyhow::Result<()> {
                super::$test(Mode::Interp).await
            }

            #[tokio::test(flavor = "current_thread")]
            async fn jit() -> anyhow::Result<()> {
                super::$test(Mode::Jit).await
            }

            #[tokio::test(flavor = "current_thread")]
            async fn par() -> anyhow::Result<()> {
                super::$test(Mode::Par).await
            }

            #[tokio::test(flavor = "current_thread")]
            async fn jit_par() -> anyhow::Result<()> {
                super::$test(Mode::JitPar).await
            }
        }
    )+};
}

#[cfg(test)]
mod lang;
#[cfg(test)]
mod lib_tests;
