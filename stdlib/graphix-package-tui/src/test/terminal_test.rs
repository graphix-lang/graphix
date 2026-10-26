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

/// Run `run` alone in a child process of this test binary. With
/// `terminal`, the child's stdin is a fresh PTY, and the bytes given are
/// typed on it once the child has put it in raw mode.
#[cfg(unix)]
async fn in_child(
    name: &str,
    terminal: Option<&[u8]>,
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
    let master = if terminal.is_some() {
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
    let typed = async {
        let (Some(master), Some(input)) = (&master, terminal) else { return Ok(()) };
        if input.is_empty() {
            return Ok(());
        }
        use std::os::fd::AsRawFd;
        loop {
            // tcgetattr fills the struct on success, which is checked.
            let mut modes: libc::termios = unsafe { std::mem::zeroed() };
            if unsafe { libc::tcgetattr(master.as_raw_fd(), &mut modes) } < 0 {
                return Err(std::io::Error::last_os_error());
            }
            if modes.c_lflag & libc::ICANON == 0 {
                break;
            }
            tokio::time::sleep(std::time::Duration::from_millis(10)).await
        }
        // The buffer outlives the call and its length is passed with it.
        let n = unsafe {
            libc::write(master.as_raw_fd(), input.as_ptr().cast(), input.len())
        };
        if n < 0 { Err(std::io::Error::last_os_error()) } else { Ok(()) }
    };
    let (output, typed) = tokio::time::timeout(
        std::time::Duration::from_secs(10),
        futures::future::join(command.output(), typed),
    )
    .await?;
    typed?;
    Ok(Some(output?))
}

#[cfg(unix)]
#[tokio::test]
async fn held_stderr_keeps_a_bounded_head_and_tail() -> anyhow::Result<()> {
    use std::io::Write;
    if let Some(output) =
        in_child("held_stderr_keeps_a_bounded_head_and_tail", None, || {
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
async fn stderr_inherited_while_held_outlives_the_hold() -> anyhow::Result<()> {
    use std::io::Write;
    if let Some(output) =
        in_child("stderr_inherited_while_held_outlives_the_hold", None, || {
            let held = crate::StderrHeld::hold()?;
            // what a child spawned now inherits as its stderr
            let inherited =
                unsafe { std::os::fd::BorrowedFd::borrow_raw(2) }.try_clone_to_owned()?;
            std::io::stderr().write_all(b"HELD\n")?;
            drop(held);
            std::fs::File::from(inherited).write_all(b"LATE\n")?;
            let held = crate::StderrHeld::hold()?;
            std::io::stderr().write_all(b"AGAIN\n")?;
            drop(held);
            Ok(())
        })
        .await?
    {
        assert!(output.status.success(), "{output:?}");
        assert!(output.stderr.ends_with(b"HELD\nLATE\nAGAIN\n"), "{output:?}");
    }
    Ok(())
}

#[cfg(unix)]
#[tokio::test]
async fn caught_panics_do_not_leave_the_display_cooked() -> anyhow::Result<()> {
    if let Some(output) =
        in_child("caught_panics_do_not_leave_the_display_cooked", Some(b""), || {
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
        // CR claude for claude: [test-gap/low] This is the only check of this test, and
        // it also passes when the child ran nothing. If the name passed to in_child, or
        // the terminal_test module path, stops matching, the child's `--exact` filter
        // selects 0 tests and libtest exits 0. With no input to type, nothing else
        // fails. Assert that the child's stdout reports "1 passed", here or in in_child
        // for every child that exits successfully. Found by reading; not run.
        // (tests-tui-02)
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
        in_child("aborting_panics_replay_stderr_and_the_diagnostic", Some(b""), || {
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

#[cfg(unix)]
#[tokio::test]
async fn ctrl_c_is_read_while_work_waits() -> anyhow::Result<()> {
    if let Some(output) =
        in_child("ctrl_c_is_read_while_work_waits", Some(b"x\x03"), || {
            std::thread::scope(|s| {
                s.spawn(|| {
                    let rt = tokio::runtime::Builder::new_current_thread()
                        .enable_all()
                        .build()?;
                    rt.block_on(async {
                        let mut term = crate::Term::Live(crate::Live::take(false)?);
                        let mut liveness =
                            tokio::time::interval(std::time::Duration::from_secs(1));
                        let mut pending = std::collections::VecDeque::new();
                        let done = term
                            .reading(
                                std::future::pending::<()>(),
                                &mut liveness,
                                &mut pending,
                            )
                            .await?;
                        assert!(done.is_none());
                        assert_eq!(pending.len(), 1, "{pending:?}");
                        Ok(())
                    })
                })
                .join()
                .unwrap()
            })
        })
        .await?
    {
        assert!(output.status.success(), "{output:?}");
    }
    Ok(())
}
