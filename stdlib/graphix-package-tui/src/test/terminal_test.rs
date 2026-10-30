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
/// typed on it once the child has put it in raw mode. A child that
/// exits successfully must have run the test `name`.
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
    let output = output?;
    const RAN_ONE: &[u8] = b"test result: ok. 1 passed";
    if output.status.success()
        && !output.stdout.windows(RAN_ONE.len()).any(|w| w == RAN_ONE)
    {
        anyhow::bail!("the child ran no test named {name}: {output:?}")
    }
    Ok(Some(output))
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
        assert!(output.status.success(), "{output:?}");
    }
    Ok(())
}

#[cfg(unix)]
#[tokio::test]
async fn aborting_panics_give_back_the_terminal() -> anyhow::Result<()> {
    extern "C" fn aborting() {
        panic!("TUI_ABORT_DIAGNOSTIC");
    }
    if let Some(output) =
        in_child("aborting_panics_give_back_the_terminal", Some(b""), || {
            let _live = crate::Live::take(false)?;
            aborting();
            Ok(())
        })
        .await?
    {
        assert!(!output.status.success());
        let stderr = std::str::from_utf8(&output.stderr)?;
        assert!(stderr.contains("TUI_ABORT_DIAGNOSTIC"), "{stderr}");
        assert!(std::str::from_utf8(&output.stdout)?.contains("\x1b[?1049l"));
    }
    Ok(())
}

/// A stop key typed on the terminal is read while the work waits, the
/// keys before it kept: Ctrl-C leaves the display, Ctrl-\'s control
/// code interrupts.
#[cfg(unix)]
async fn stop_key_is_read_while_work_waits(
    name: &str,
    typed: &[u8],
    stop: graphix_package::Ended,
) -> anyhow::Result<()> {
    if let Some(output) = in_child(name, Some(typed), || {
        std::thread::scope(|s| {
            s.spawn(|| {
                let rt =
                    tokio::runtime::Builder::new_current_thread().enable_all().build()?;
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
                    assert_eq!(done, std::ops::ControlFlow::Break(stop));
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

#[cfg(unix)]
#[tokio::test]
async fn ctrl_c_is_read_while_work_waits() -> anyhow::Result<()> {
    stop_key_is_read_while_work_waits(
        "ctrl_c_is_read_while_work_waits",
        b"x\x03",
        graphix_package::Ended::Left,
    )
    .await
}

#[cfg(unix)]
#[tokio::test]
async fn ctrl_backslash_is_read_while_work_waits() -> anyhow::Result<()> {
    stop_key_is_read_while_work_waits(
        "ctrl_backslash_is_read_while_work_waits",
        b"x\x1c",
        graphix_package::Ended::Interrupted,
    )
    .await
}

/// Stderr is never held: what is written to it while the display runs
/// reaches the terminal at once, over the display, before and after a
/// recovery alike.
#[cfg(unix)]
#[tokio::test]
async fn stderr_is_written_through_while_displaying() -> anyhow::Result<()> {
    use std::io::Write;
    if let Some(output) =
        in_child("stderr_is_written_through_while_displaying", Some(b""), || {
            // stderr joins the display's stream, so the order of the two shows
            if unsafe { libc::dup2(1, 2) } < 0 {
                return Err(std::io::Error::last_os_error().into());
            }
            let mut live = crate::Live::take(false)?;
            std::io::stderr().write_all(b"FIRST\n")?;
            let _ = std::panic::catch_unwind(|| panic!("caught while displaying"));
            assert!(live.recover(false)?);
            std::io::stderr().write_all(b"RECOVERED\n")?;
            drop(live);
            Ok(())
        })
        .await?
    {
        assert!(output.status.success(), "{output:?}");
        let find = |hay: &[u8], needle: &[u8]| {
            hay.windows(needle.len()).position(|w| w == needle)
        };
        let mut rest = &output.stdout[..];
        let mut screens: LPooled<Vec<&[u8]>> = LPooled::take();
        while let Some(i) = find(rest, b"\x1b[?1049h") {
            let screen = &rest[i..];
            let end = find(screen, b"\x1b[?1049l").unwrap_or(screen.len());
            screens.push(&screen[..end]);
            rest = &screen[end..];
        }
        assert_eq!(screens.len(), 2, "{output:?}");
        for (screen, written) in screens.iter().zip([&b"FIRST\n"[..], b"RECOVERED\n"]) {
            assert!(
                find(screen, written).is_some(),
                "{:?}",
                String::from_utf8_lossy(screen)
            );
        }
    }
    Ok(())
}
