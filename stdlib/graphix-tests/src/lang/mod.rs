// Language feature tests organized by category

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

mod arrays;
mod async_restart;
mod attributes;
mod basics;
mod byref;
mod collection;
mod datetime;
mod dense_deltas;
mod errors;
mod functions;
mod fusion;
mod image;
mod inference;
mod interfaces;
mod lists;
mod maps;
mod modules;
mod organic_deltas;
mod par_attrs;
mod par_loops;
mod printing;
mod select;
mod seq;
mod seq_abort;
mod seq_calls;
mod seq_errors;
mod seq_let;
mod seq_shadow;
mod seq_steps;
mod seq_try;
mod seqq;
mod traits;
mod tuples_structs;
mod types;
mod variants;
