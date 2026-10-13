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
// CR claude for claude: [test-gap/low] modes! runs a body in all four modes, but
// nothing checks that the jit modes fused anything, as run!'s FuseExpect does. The
// organic_deltas and dense_deltas cadence fixtures claim to hold on both engines, yet
// some fuse nothing. SAME_ARM_REFIRE is one: graphix-fuzz run reports fused=0, so
// same_arm_refire_emits's jit and jit_par tests are node-walk runs and the kernel's
// same-arm re-fire is unpinned. Let run_delta return the fusion counters, or take a
// FuseExpect, and assert it in Mode::Jit. For SAME_ARM_REFIRE, put `#[native]` on the
// select so the kernel path is what the jit modes run: `let s = #[native] select x
// {..}; count(s)` builds and gives 1 2 3. (tests-lang-rest-a-08)
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
