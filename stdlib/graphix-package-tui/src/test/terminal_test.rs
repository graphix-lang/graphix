use poolshark::local::LPooled;

#[test]
fn toggling_mouse_does_not_toggle_focus() {
    let mut output: LPooled<Vec<u8>> = LPooled::take();
    crate::set_mouse_to(&mut *output, true);
    crate::set_mouse_to(&mut *output, false);
    let output = std::str::from_utf8(&output).unwrap();
    assert!(!output.contains("1004"), "{output:?}");
}

#[test]
fn display_modes_enable_focus() -> anyhow::Result<()> {
    let mut output: LPooled<Vec<u8>> = LPooled::take();
    crate::take_terminal_modes(&mut *output)?;
    let output = std::str::from_utf8(&output)?;
    assert!(output.contains("\x1b[?1004h"));
    assert!(!output.contains("1000"));
    Ok(())
}

#[cfg(unix)]
async fn in_child(
    name: &str,
    run: impl FnOnce() -> anyhow::Result<()>,
) -> anyhow::Result<Option<std::process::Output>> {
    use compact_str::format_compact;
    if std::env::var("GRAPHIX_TUI_TEST_CHILD").as_deref() == Ok(name) {
        let limit = libc::rlimit { rlim_cur: 0, rlim_max: 0 };
        // The limit applies only to this isolated test process.
        unsafe { libc::setrlimit(libc::RLIMIT_CORE, &limit) };
        run()?;
        return Ok(None);
    }
    let output = tokio::time::timeout(
        std::time::Duration::from_secs(10),
        tokio::process::Command::new(std::env::current_exe()?)
            .arg("--exact")
            .arg(format_compact!("test::terminal_test::{name}").as_str())
            .args(["--nocapture", "--include-ignored", "--test-threads=8"])
            .env("GRAPHIX_TUI_TEST_CHILD", name)
            .env("RUST_BACKTRACE", "0")
            .kill_on_drop(true)
            .output(),
    )
    .await??;
    Ok(Some(output))
}

#[cfg(unix)]
#[tokio::test]
async fn held_stderr_keeps_a_bounded_head_and_tail() -> anyhow::Result<()> {
    use std::io::Write;
    if let Some(output) = in_child("held_stderr_keeps_a_bounded_head_and_tail", || {
        let held = crate::StderrHeld::hold()?;
        // fd 2 remains owned by this process while it is duplicated.
        let writer =
            unsafe { std::os::fd::BorrowedFd::borrow_raw(2) }.try_clone_to_owned()?;
        let mut stderr = std::io::stderr().lock();
        stderr.write_all(b"HEAD\n")?;
        for _ in 0..4096 {
            stderr.write_all(&[b'x'; 256])?;
        }
        stderr.write_all(b"\nTAIL\n")?;
        drop(stderr);
        drop(held);
        drop(writer);
        Ok(())
    })
    .await?
    {
        assert!(output.status.success(), "{output:?}");
        assert!(output.stderr.len() < 70_000, "{} bytes replayed", output.stderr.len());
        assert!(output.stderr.starts_with(b"HEAD\n"));
        assert!(output.stderr.ends_with(b"\nTAIL\n"));
        assert!(std::str::from_utf8(&output.stderr)?.contains("bytes dropped"));
    }
    Ok(())
}
