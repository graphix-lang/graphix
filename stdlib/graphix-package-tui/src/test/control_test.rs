//! With no display running (every harness is headless) a suspend
//! request has no taker: a rising edge answers with an error at once,
//! and the level's false side asks nothing of a display.

use crate::testing::TuiTestHarness;
use anyhow::Result;
use std::time::{Duration, Instant};

#[tokio::test(flavor = "multi_thread")]
async fn suspend_without_a_display_is_an_error() -> Result<()> {
    let mut h = TuiTestHarness::with_viewport(
        r#"
use tui::paragraph::{self, *};
let idle = tui::suspend(false);
let s = tui::suspend(true);
let status = select (is_err(idle), is_err(s)) {
  (false, true) => "error: [s]",
  (false, false) => "suspended",
  (true, _) => "idle errored: [idle]"
};
let result = paragraph(&status)
"#,
        100,
        10,
    )
    .await?;
    let deadline = Instant::now() + Duration::from_secs(30);
    loop {
        // CR claude for claude: [perf/low] tui::suspend answers through an async task
        // reply, and drain returns at runtime idle without waiting for one, as its doc
        // says. This loop therefore polls the runtime and re-renders back to back until
        // the reply lands, for up to 30 s if it never does. Wait for the reply with
        // `h.next_update(Duration::from_millis(500)).await?`, as input_test's browser
        // tests do. (tests-tui-08)
        h.drain().await?;
        let lines = h.render_lines()?;
        if lines.iter().any(|l| {
            l.starts_with("error: ") && l.contains("no terminal display is running")
            // CR claude for claude: [readability/low] suspend test polls drain() in a tight
            // loop; post-Ctrl-C step tests the harness latch, not the display.
            // control_test's loop calls drain() and render() repeatedly for up to 30 s with
            // no wait. drain() returns as soon as the runtime is idle, so while the async
            // answer is pending this spins on wait_idle round trips. The harness has
            // next_update(timeout) for exactly this, as input_test.rs uses it. Separately,
            // render_test.rs dispatches 'd' after Ctrl-C. TuiTestHarness::deliver drops it
            // through its own `stopped` latch (testing.rs), while the real display keeps
            // calling screen.event after firing stop (lib.rs display loop). That step pins
            // harness behavior the display does not have. Failure: The suspend test burns a
            // core until the answer arrives. The 'd' assertion passes even if the display
            // routed keys after Ctrl-C. Fix: In control_test use
            // `h.next_update(Duration::from_millis(500)).await?` between checks. Drop the
            // post-Ctrl-C 'd' dispatch, or document that the latch models the shell
            // stopping the display. Unverified: the reviewer's report, not checked by a
            // verifier. (tests-tui-11)
        }) {
            return Ok(());
        }
        if lines.iter().any(|l| l.contains("suspended") || l.contains("idle errored")) {
            anyhow::bail!(
                "suspend(true) did not error, or suspend(false) did:\n{}",
                lines.join("\n")
            );
        }
        if Instant::now() > deadline {
            anyhow::bail!("no answer from suspend; last render:\n{}", lines.join("\n"));
        }
    }
}
