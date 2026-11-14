//! With no display running (every harness is headless) a suspend
//! request has no taker: a rising edge answers with an error at once,
//! and the level's false side asks nothing of a display.

use crate::testing::TuiTestHarness;
use anyhow::Result;

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
    h.wait_for(|h| {
        let lines = h.render_lines()?;
        if lines.iter().any(|l| l.contains("suspended") || l.contains("idle errored")) {
            anyhow::bail!(
                "suspend(true) did not error, or suspend(false) did:\n{}",
                lines.join("\n")
            );
        }
        Ok(lines.iter().any(|l| {
            l.starts_with("error: ") && l.contains("no terminal display is running")
        }))
    })
    .await
}

/// A wait nothing outstanding can end fails when the runtime settles, not
/// at a wall deadline.
#[tokio::test(flavor = "multi_thread")]
async fn a_wait_the_program_cannot_end_fails_when_it_settles() -> Result<()> {
    let mut h = TuiTestHarness::new(
        r#"
use tui::paragraph::{self, *};
let result = paragraph(&"static")
"#,
    )
    .await?;
    let e = h.wait_for(|_| Ok(false)).await.unwrap_err();
    assert!(format!("{e:#}").contains("settled without an update"), "{e:#}");
    let e = h.await_update().await.unwrap_err();
    assert!(format!("{e:#}").contains("settled without an update"), "{e:#}");
    assert!(!h.next_update(std::time::Duration::from_millis(20)).await?);
    Ok(())
}

#[tokio::test]
async fn suspend_receiver_survives_an_unwinding_display() -> Result<()> {
    use futures::StreamExt;
    let control = crate::TuiControl::default();
    let _ = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        let _receiver = crate::SuspendReceiver::take(&control).unwrap();
        assert!(crate::SuspendReceiver::take(&control).is_err());
        panic!("display failure");
    }));
    let mut receiver = crate::SuspendReceiver::take(&control)?;
    let (ack, done) = tokio::sync::oneshot::channel();
    control.0.suspend_tx.unbounded_send(crate::Suspend { ack })?;
    let request = receiver.rx.as_mut().unwrap().next().await.unwrap();
    let (resume, _) = tokio::sync::oneshot::channel();
    request.ack.send(Ok(resume)).unwrap();
    let _ = done.await??;
    Ok(())
}
