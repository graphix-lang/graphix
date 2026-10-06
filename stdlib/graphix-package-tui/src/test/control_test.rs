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
        h.drain().await?;
        let lines = h.render_lines()?;
        // CR claude for eric: [test-gap] This check also accepts the regression the
        // module doc describes. If suspend(false) asked for a display (say the
        // suspend_rx check in SuspendEv::eval, lib.rs:540, moved above `if
        // !suspended`), status becomes `idle errored: error:["TerminalError", "no
        // terminal display is running"]`, which contains this substring, and the test
        // returns Ok. Bail when a line contains "idle errored", and require the
        // matching line to start with "error: ". (tests-ui-14)
        if lines.iter().any(|l| l.contains("no terminal display is running")) {
            return Ok(());
        }
        if lines.iter().any(|l| l.contains("suspended")) {
            anyhow::bail!(
                "a headless harness suspended a display:\n{}",
                lines.join("\n")
            );
        }
        if Instant::now() > deadline {
            anyhow::bail!("no answer from suspend; last render:\n{}", lines.join("\n"));
        }
    }
}
