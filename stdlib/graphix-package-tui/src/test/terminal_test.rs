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
    terminal: bool,
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
    let mut command = tokio::process::Command::new(std::env::current_exe()?);
    command
        .arg("--exact")
        .arg(format_compact!("test::terminal_test::{name}").as_str())
        .args(["--nocapture", "--include-ignored", "--test-threads=8"])
        .env("GRAPHIX_TUI_TEST_CHILD", name)
        .env("RUST_BACKTRACE", "0")
        .kill_on_drop(true);
    let _master = if terminal {
        use std::os::fd::{AsRawFd, FromRawFd, OwnedFd};
        let (mut master, mut slave) = (-1, -1);
        let size = libc::winsize { ws_row: 10, ws_col: 40, ws_xpixel: 0, ws_ypixel: 0 };
        // openpty initializes both descriptors on success; they become owned below.
        if unsafe {
            libc::openpty(
                &mut master,
                &mut slave,
                std::ptr::null_mut(),
                std::ptr::null(),
                &size,
            )
        } < 0
        {
            return Err(std::io::Error::last_os_error().into());
        }
        let (master, slave) =
            unsafe { (OwnedFd::from_raw_fd(master), OwnedFd::from_raw_fd(slave)) };
        // Keep only stdin in the child; the parent's master keeps the PTY alive.
        if unsafe { libc::fcntl(master.as_raw_fd(), libc::F_SETFD, libc::FD_CLOEXEC) } < 0
        {
            return Err(std::io::Error::last_os_error().into());
        }
        command.stdin(std::process::Stdio::from(slave));
        // Only async-signal-safe syscalls run between fork and exec.
        unsafe {
            command.pre_exec(|| {
                if libc::setsid() < 0 || libc::ioctl(0, libc::TIOCSCTTY, 0) < 0 {
                    return Err(std::io::Error::last_os_error());
                }
                Ok(())
            });
        }
        Some(master)
    } else {
        None
    };
    let output =
        tokio::time::timeout(std::time::Duration::from_secs(10), command.output())
            .await??;
    Ok(Some(output))
}

#[cfg(unix)]
#[tokio::test]
async fn held_stderr_keeps_a_bounded_head_and_tail() -> anyhow::Result<()> {
    use std::io::Write;
    if let Some(output) =
        in_child("held_stderr_keeps_a_bounded_head_and_tail", false, || {
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

#[cfg(unix)]
#[tokio::test]
async fn caught_panics_do_not_leave_the_display_cooked() -> anyhow::Result<()> {
    if let Some(output) =
        in_child("caught_panics_do_not_leave_the_display_cooked", true, || {
            let mut live = crate::Live::take(false)?;
            let _ = std::panic::catch_unwind(|| panic!("caught while displaying"));
            assert!(live.recover(false)?);
            assert!(crossterm::terminal::is_raw_mode_enabled()?);
            drop(live);
            crossterm::terminal::enable_raw_mode()?;
            let _ = std::panic::catch_unwind(|| panic!("caught after displaying"));
            let raw = crossterm::terminal::is_raw_mode_enabled()?;
            crossterm::terminal::disable_raw_mode()?;
            assert!(raw);
            Ok(())
        })
        .await?
    {
        assert!(output.status.success(), "{output:?}");
    }
    Ok(())
}

#[cfg(unix)]
#[tokio::test]
async fn aborting_panics_replay_stderr_and_the_diagnostic() -> anyhow::Result<()> {
    extern "C" fn aborting() {
        panic!("TUI_ABORT_DIAGNOSTIC");
    }
    if let Some(output) =
        in_child("aborting_panics_replay_stderr_and_the_diagnostic", true, || {
            use std::io::Write;
            let _live = crate::Live::take(false)?;
            std::io::stderr().write_all(b"TUI_BEFORE_PANIC\n")?;
            aborting();
            Ok(())
        })
        .await?
    {
        assert!(!output.status.success());
        let stderr = std::str::from_utf8(&output.stderr)?;
        assert!(stderr.contains("TUI_BEFORE_PANIC"), "{stderr}");
        assert!(stderr.contains("TUI_ABORT_DIAGNOSTIC"), "{stderr}");
        assert!(std::str::from_utf8(&output.stdout)?.contains("\x1b[?1049l"));
    }
    Ok(())
}
