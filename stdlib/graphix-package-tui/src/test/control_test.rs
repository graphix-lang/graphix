//! The display control a program reaches through libstate: with no
//! terminal display running — every harness is headless — the
//! suspend request has no taker. The runner parks the receiver in the
//! control until a display claims it, so the call answers with an
//! error at once instead of parking the program on a reply that can
//! never come.

use crate::testing::TuiTestHarness;
use anyhow::Result;
use std::time::{Duration, Instant};

#[tokio::test(flavor = "multi_thread")]
async fn run_in_terminal_without_a_display_is_an_error() -> Result<()> {
    let mut h = TuiTestHarness::with_viewport(
        r#"
use tui::paragraph::{self, *};
let r = tui::run_in_terminal(#args: [], #note: null, "/bin/true");
let status = select is_err(r) {
  true => "error: [r]",
  false => "ran"
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
        if lines.iter().any(|l| l.contains("ran")) {
            anyhow::bail!(
                "a headless harness ran a program in a terminal:\n{}",
                lines.join("\n")
            );
        }
        if Instant::now() > deadline {
            anyhow::bail!(
                "no answer from run_in_terminal; last render:\n{}",
                lines.join("\n")
            );
        }
    }
}
