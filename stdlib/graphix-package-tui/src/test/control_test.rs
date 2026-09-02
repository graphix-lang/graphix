//! The display control a program reaches through libstate: with no
//! terminal display running — every harness is headless — a suspend
//! request has no taker. The runner parks the receiver in the control
//! until a display claims it, so a rising edge answers with an error
//! at once instead of parking the program on an acknowledgement that
//! can never come; the level's false side asks nothing of a display.

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
