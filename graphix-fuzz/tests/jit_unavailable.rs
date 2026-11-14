use graphix_fuzz::{Mode, Outcome, Route, cold_warm};
use std::time::Duration;

/// A JIT that cannot be built leaves the program unfused and without an
/// image: the warm outcome is the cold one, not an image bug. The arena
/// size is read once per process, so this test has the process to itself.
#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn an_unbuildable_jit_is_no_image_bug() {
    // SAFETY: nothing else in this process reads the environment yet.
    unsafe { std::env::set_var("GRAPHIX_JIT_ARENA", "1") };
    let code =
        "{ let rec f = |n: i64| -> i64 select n { 0 => 0, n => f(n - 1) }; f(100) }";
    let (cold, warm) =
        cold_warm(code, Mode::Jit, Route::InLanguage, Duration::from_secs(60)).await;
    assert!(matches!(cold, Outcome::Trace(_)), "{cold:?}");
    assert!(cold.agrees_with(&warm), "cold {cold:?}, warm {warm:?}");
}
