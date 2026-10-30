//! The REPL's two keys: Ctrl-C gives the prompt back and keeps the
//! session; Ctrl-\, the interrupt, replaces the runtime with a fresh one
//! and says so; Ctrl-D ends the shell. A piped REPL hears both keys as
//! signals (SIGINT, SIGQUIT) and Ctrl-D as the pipe's end. At a terminal
//! the line editor reads them as keys at the prompt, and the terminal
//! sends them as signals while a value shows.
#![cfg(unix)]

use parking_lot::Mutex;
use std::{
    fs::File,
    io::{Read, Write},
    os::{
        fd::{AsRawFd, FromRawFd, OwnedFd},
        unix::process::CommandExt,
    },
    process::{Child, ChildStdin, Command, ExitStatus, Stdio},
    sync::Arc,
    thread,
    time::{Duration, Instant},
};

const RESET: &str = "interrupted: the session was reset";
const BUSY: &str = "ctrl-c stops nothing";
const WEDGE: &str = "{ let rec f = |v: i64| -> i64 f(v + i64:1); f(i64:0) }";
const PATIENCE: Duration = Duration::from_secs(60);

fn collect(mut r: impl Read + Send + 'static) -> Arc<Mutex<Vec<u8>>> {
    let out = Arc::new(Mutex::new(vec![]));
    let o = out.clone();
    thread::spawn(move || {
        let mut buf = [0u8; 4096];
        while let Ok(n) = r.read(&mut buf)
            && n > 0
        {
            o.lock().extend_from_slice(&buf[..n]);
        }
    });
    out
}

fn text(out: &Mutex<Vec<u8>>) -> String {
    String::from_utf8_lossy(&out.lock()).into_owned()
}

fn signal(child: &Child, sig: libc::c_int) {
    unsafe { libc::kill(child.id() as libc::pid_t, sig) };
}

/// How `child` exited, once it does.
fn exited(child: &mut Child, shown: impl Fn() -> String) -> ExitStatus {
    let deadline = Instant::now() + Duration::from_secs(30);
    loop {
        if let Some(status) = child.try_wait().expect("try_wait") {
            // let the readers take what is left
            thread::sleep(Duration::from_millis(100));
            return status;
        }
        if Instant::now() > deadline {
            let _ = child.kill();
            panic!("the shell did not end\n{}", shown())
        }
        thread::sleep(Duration::from_millis(50));
    }
}

/// A REPL running on piped input, its output collected as it comes.
struct Repl {
    child: Child,
    stdin: Option<ChildStdin>,
    stdout: Arc<Mutex<Vec<u8>>>,
    stderr: Arc<Mutex<Vec<u8>>>,
    _dir: tempfile::TempDir,
}

impl Repl {
    fn start() -> Self {
        let dir = tempfile::tempdir().expect("tempdir");
        let mut child = Command::new(env!("CARGO_BIN_EXE_graphix"))
            .args(["--no-netidx", "--no-cache", "--no-init"])
            .env("XDG_CACHE_HOME", dir.path().join("cache"))
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .expect("spawn graphix");
        let stdin = child.stdin.take();
        let stdout = collect(child.stdout.take().unwrap());
        let stderr = collect(child.stderr.take().unwrap());
        Self { child, stdin, stdout, stderr, _dir: dir }
    }

    fn shown(&self) -> String {
        format!("stdout:\n{}\nstderr:\n{}", text(&self.stdout), text(&self.stderr))
    }

    fn line(&mut self, line: &str) {
        let stdin = self.stdin.as_mut().expect("stdin is open");
        writeln!(stdin, "{line}").expect("write a line");
        stdin.flush().expect("flush");
    }

    /// Wait until `out` holds `want` `n` times.
    fn wait_for(&self, out: &Mutex<Vec<u8>>, want: &str, n: usize) {
        let deadline = Instant::now() + PATIENCE;
        while text(out).matches(want).count() < n {
            if Instant::now() > deadline {
                panic!("{want:?} never showed {n} times\n{}", self.shown())
            }
            thread::sleep(Duration::from_millis(20));
        }
    }

    /// Wedge the runtime in a cycle that never ends.
    fn wedge(&mut self) {
        self.line(WEDGE);
        self.wait_for(&self.stdout, "-: i64", 1);
        thread::sleep(Duration::from_millis(500));
    }

    /// Close the input, as Ctrl-D does, and wait for the shell to end.
    fn end(mut self) -> (ExitStatus, String, String) {
        drop(self.stdin.take());
        self.exited()
    }

    fn exited(mut self) -> (ExitStatus, String, String) {
        let (stdout, stderr) = (self.stdout.clone(), self.stderr.clone());
        let status = exited(&mut self.child, || {
            format!("stdout:\n{}\nstderr:\n{}", text(&stdout), text(&stderr))
        });
        (status, text(&self.stdout), text(&self.stderr))
    }
}

/// Ctrl-C at an idle prompt stops nothing: a binding made before it is
/// still there after.
#[test]
fn ctrl_c_at_an_idle_prompt_keeps_the_session() {
    let mut repl = Repl::start();
    repl.line("let x = 41");
    repl.line(r#"let ready = println("before")"#);
    repl.wait_for(&repl.stdout, "before", 1);
    signal(&repl.child, libc::SIGINT);
    repl.line("x + 1");
    repl.wait_for(&repl.stdout, "42", 1);
    let (status, stdout, stderr) = repl.end();
    assert_eq!(status.code(), Some(0), "stdout:\n{stdout}\nstderr:\n{stderr}");
    assert!(!stderr.contains(RESET), "{stderr}");
}

/// The interrupt at an idle prompt resets the session: a binding made
/// before it is gone after, the shell goes on reading, and Ctrl-D still
/// ends it.
#[test]
fn interrupt_at_an_idle_prompt_resets_the_session() {
    let mut repl = Repl::start();
    repl.line("let x = 41");
    repl.line(r#"let ready = println("before")"#);
    repl.wait_for(&repl.stdout, "before", 1);
    signal(&repl.child, libc::SIGQUIT);
    repl.wait_for(&repl.stderr, RESET, 1);
    repl.line("x + 1");
    repl.line(r#"let ready = println("after")"#);
    repl.wait_for(&repl.stdout, "after", 1);
    let (status, stdout, stderr) = repl.end();
    assert_eq!(status.code(), Some(0), "stdout:\n{stdout}\nstderr:\n{stderr}");
    assert!(stderr.contains("x not defined"), "{stderr}");
    assert!(!stdout.contains("42"), "{stdout}");
    assert_eq!(stderr.matches(RESET).count(), 1, "{stderr}");
}

/// The interrupt frees a REPL whose input wedged its runtime: the
/// session is reset and the next input runs.
#[test]
fn interrupt_frees_a_wedged_repl() {
    let mut repl = Repl::start();
    repl.line("let x = 41");
    repl.wedge();
    signal(&repl.child, libc::SIGQUIT);
    repl.wait_for(&repl.stderr, RESET, 1);
    repl.line("x + 1");
    repl.line("sys::exit(5)");
    let (status, stdout, stderr) = repl.exited();
    assert_eq!(status.code(), Some(5), "stdout:\n{stdout}\nstderr:\n{stderr}");
    assert!(stderr.contains("x not defined"), "{stderr}");
    assert!(!stdout.contains("42"), "{stdout}");
}

/// Ctrl-C while an input waits on a busy runtime stops nothing: it says
/// what would, and the interrupt still frees the REPL after it.
#[test]
fn ctrl_c_while_busy_names_the_interrupt() {
    let mut repl = Repl::start();
    repl.line("let x = 41");
    repl.wedge();
    repl.line("x + 1");
    // until the shell has read the line, a Ctrl-C only leaves the display
    let deadline = Instant::now() + PATIENCE;
    while !text(&repl.stderr).contains(BUSY) {
        if Instant::now() > deadline {
            panic!("{BUSY:?} never showed\n{}", repl.shown())
        }
        signal(&repl.child, libc::SIGINT);
        thread::sleep(Duration::from_millis(200));
    }
    assert!(!text(&repl.stdout).contains("42"), "{}", repl.shown());
    assert!(!text(&repl.stderr).contains(RESET), "{}", repl.shown());
    signal(&repl.child, libc::SIGQUIT);
    repl.wait_for(&repl.stderr, RESET, 1);
    repl.line("sys::exit(5)");
    let (status, stdout, stderr) = repl.exited();
    assert_eq!(status.code(), Some(5), "stdout:\n{stdout}\nstderr:\n{stderr}");
    assert_eq!(stderr.matches(RESET).count(), 1, "{stderr}");
}

/// Interrupts a second apart, some landing while an earlier one's reset
/// starts the next runtime, each start again: the REPL goes on working.
#[test]
fn interrupts_during_a_reset_leave_a_working_repl() {
    let mut repl = Repl::start();
    repl.line(r#"let ready = println("before")"#);
    repl.wait_for(&repl.stdout, "before", 1);
    for _ in 0..20 {
        signal(&repl.child, libc::SIGQUIT);
        thread::sleep(Duration::from_millis(50));
    }
    repl.wait_for(&repl.stderr, RESET, 1);
    // the last reset has started its runtime once the count holds
    let mut resets = 0;
    loop {
        thread::sleep(Duration::from_secs(2));
        let now = text(&repl.stderr).matches(RESET).count();
        if now == resets {
            break;
        }
        resets = now;
    }
    repl.line(r#"let ready = println("after")"#);
    repl.wait_for(&repl.stdout, "after", 1);
    let (status, stdout, stderr) = repl.end();
    assert_eq!(status.code(), Some(0), "stdout:\n{stdout}\nstderr:\n{stderr}");
}

/// Ctrl-D ends a REPL that is computing nothing.
#[test]
fn ctrl_d_ends_the_shell() {
    let mut repl = Repl::start();
    repl.line("let x = 41");
    repl.line("x + 1");
    repl.wait_for(&repl.stdout, "42", 1);
    let (status, stdout, stderr) = repl.end();
    assert_eq!(status.code(), Some(0), "stdout:\n{stdout}\nstderr:\n{stderr}");
    assert!(!stderr.contains(RESET), "{stderr}");
}

/// What a terminal shows: the text with its control sequences taken out.
fn plain(bytes: &[u8]) -> String {
    let s = String::from_utf8_lossy(bytes);
    let mut out = String::with_capacity(s.len());
    let mut chars = s.chars().peekable();
    while let Some(c) = chars.next() {
        if c != '\x1b' {
            out.push(c);
            continue;
        }
        match chars.next() {
            // CSI: parameters up to a final byte
            Some('[') => {
                while let Some(c) = chars.next() {
                    if ('@'..='~').contains(&c) {
                        break;
                    }
                }
            }
            // OSC: up to BEL or ST
            Some(']') => {
                while let Some(c) = chars.next() {
                    if c == '\x07' || (c == '\x1b' && chars.next_if_eq(&'\\').is_some()) {
                        break;
                    }
                }
            }
            _ => (),
        }
    }
    out
}

/// A REPL at a terminal: its stdin, stdout and stderr are a PTY that is
/// its controlling terminal, so the keys written to it reach the line
/// editor, or become signals while the terminal is in its usual mode.
struct Tty {
    child: Child,
    master: File,
    out: Arc<Mutex<Vec<u8>>>,
    _dir: tempfile::TempDir,
}

impl Tty {
    fn start() -> Self {
        let dir = tempfile::tempdir().expect("tempdir");
        let (mut master, mut slave) = (-1, -1);
        let size = libc::winsize { ws_row: 24, ws_col: 100, ws_xpixel: 0, ws_ypixel: 0 };
        // openpty initializes both descriptors on success; they become owned below.
        let r = unsafe {
            libc::openpty(
                &mut master,
                &mut slave,
                std::ptr::null_mut(),
                std::ptr::null(),
                &size,
            )
        };
        assert!(r == 0, "openpty: {}", std::io::Error::last_os_error());
        let (master, slave) =
            unsafe { (OwnedFd::from_raw_fd(master), OwnedFd::from_raw_fd(slave)) };
        for fd in [&master, &slave] {
            // only the copies made for the child's stdio cross the exec
            let r =
                unsafe { libc::fcntl(fd.as_raw_fd(), libc::F_SETFD, libc::FD_CLOEXEC) };
            assert!(r == 0, "fcntl: {}", std::io::Error::last_os_error());
        }
        let mut command = Command::new(env!("CARGO_BIN_EXE_graphix"));
        command
            .args(["--no-netidx", "--no-cache", "--no-init"])
            .env("XDG_CACHE_HOME", dir.path().join("cache"))
            .env("TERM", "xterm")
            .stdin(Stdio::from(slave.try_clone().expect("dup")))
            .stdout(Stdio::from(slave.try_clone().expect("dup")))
            .stderr(Stdio::from(slave));
        // Only async-signal-safe syscalls run between fork and exec.
        unsafe {
            command.pre_exec(|| {
                if libc::setsid() < 0 || libc::ioctl(0, libc::TIOCSCTTY, 0) < 0 {
                    return Err(std::io::Error::last_os_error());
                }
                Ok(())
            });
        }
        let child = command.spawn().expect("spawn graphix");
        let master = File::from(master);
        let mut reader = master.try_clone().expect("dup");
        let mut answer = master.try_clone().expect("dup");
        let out = Arc::new(Mutex::new(vec![]));
        let o = out.clone();
        thread::spawn(move || {
            const ASK: &[u8] = b"\x1b[6n";
            let mut buf = [0u8; 4096];
            // reads end in EIO once the child has closed the terminal
            while let Ok(n) = reader.read(&mut buf)
                && n > 0
            {
                let mut o = o.lock();
                let from = o.len().saturating_sub(ASK.len() - 1);
                o.extend_from_slice(&buf[..n]);
                // answer where the cursor is, as a terminal does
                for _ in o[from..].windows(ASK.len()).filter(|w| *w == ASK) {
                    let _ = answer.write_all(b"\x1b[1;1R");
                }
            }
        });
        let tty = Self { child, master, out, _dir: dir };
        tty.wait_for("Welcome");
        tty
    }

    fn shown(&self) -> String {
        plain(&self.out.lock())
    }

    /// Wait until the terminal has shown `want`.
    fn wait_for(&self, want: &str) {
        self.wait_until(&format!("{want:?} shown"), || self.shown().contains(want))
    }

    fn wait_until(&self, what: &str, mut done: impl FnMut() -> bool) {
        let deadline = Instant::now() + PATIENCE;
        while !done() {
            if Instant::now() > deadline {
                panic!("never {what}; the terminal shows:\n{}", self.shown())
            }
            thread::sleep(Duration::from_millis(10));
        }
    }

    /// Whether a reader holds the terminal raw: the line editor at its
    /// prompt, or a TUI.
    fn raw(&self) -> bool {
        // tcgetattr fills the struct on success, which is checked.
        let mut modes: libc::termios = unsafe { std::mem::zeroed() };
        let r = unsafe { libc::tcgetattr(self.master.as_raw_fd(), &mut modes) };
        assert!(r == 0, "tcgetattr: {}", std::io::Error::last_os_error());
        modes.c_lflag & libc::ICANON == 0
    }

    fn wait_raw(&self, raw: bool) {
        self.wait_until(if raw { "raw" } else { "cooked" }, || self.raw() == raw)
    }

    fn keys(&self, keys: &[u8]) {
        (&self.master).write_all(keys).expect("type")
    }

    /// Type `line` at the prompt and wait until what the terminal shows
    /// after it holds one of `shows`, which only the line's evaluation
    /// shows: the prompt raw mode stands for next is then the one after
    /// it. What it showed is the answer.
    fn line(&self, line: &str, shows: &[&str]) -> String {
        self.wait_raw(true);
        let from = self.out.lock().len();
        self.keys(format!("{line}\r").as_bytes());
        let mut seen = String::new();
        self.wait_until(&format!("one of {shows:?} shown"), || {
            seen = plain(&self.out.lock()[from..]);
            shows.iter().any(|s| seen.contains(s))
        });
        seen
    }

    /// Leave the value display with Ctrl-C, which the terminal sends as
    /// SIGINT.
    fn leave_display(&self) {
        self.wait_raw(false);
        self.keys(b"\x03");
    }

    /// How many times the line editor has drawn its prompt.
    fn prompts(&self) -> usize {
        self.shown().matches('〉').count()
    }

    /// Type `junk` at the prompt, then `key`, which ends the line, and
    /// wait for the prompt after it.
    fn abandon_line(&self, junk: &str, key: &[u8]) {
        self.wait_raw(true);
        self.keys(junk.as_bytes());
        self.wait_for(&format!("〉{junk}"));
        thread::sleep(Duration::from_millis(200));
        let prompts = self.prompts();
        self.keys(key);
        self.wait_until("prompted again", || self.prompts() > prompts);
    }

    /// End the shell with Ctrl-D at its prompt.
    fn end(mut self) -> (ExitStatus, String) {
        self.wait_raw(true);
        self.keys(b"\x04");
        let out = self.out.clone();
        let status = exited(&mut self.child, || plain(&out.lock()));
        (status, self.shown())
    }
}

impl Drop for Tty {
    fn drop(&mut self) {
        let _ = self.child.kill();
        let _ = self.child.wait();
    }
}

const BIND: &str = r#"let x = { println("bound"); 41 }"#;

/// Ctrl-C at a value display gives the prompt back with every binding
/// kept.
#[test]
fn ctrl_c_at_a_terminal_display_keeps_the_session() {
    let tty = Tty::start();
    tty.line(BIND, &["bound"]);
    tty.line("x + 1", &["42"]);
    tty.leave_display();
    let shown = tty.line("x + 100", &["141", "not defined"]);
    assert!(shown.contains("141"), "{}", tty.shown());
    tty.leave_display();
    let (status, shown) = tty.end();
    assert!(!shown.contains(RESET), "{shown}");
    assert_eq!(status.code(), Some(0), "{shown}");
}

/// Ctrl-C at an idle prompt abandons the line being typed and keeps the
/// session.
#[test]
fn ctrl_c_at_a_terminal_prompt_keeps_the_session() {
    let tty = Tty::start();
    tty.line(BIND, &["bound"]);
    tty.abandon_line("junk", b"\x03");
    let shown = tty.line("x + 100", &["141", "not defined", "error"]);
    assert!(shown.contains("141"), "{}", tty.shown());
    tty.leave_display();
    let (status, shown) = tty.end();
    assert!(!shown.contains(RESET), "{shown}");
    assert_eq!(status.code(), Some(0), "{shown}");
}

/// Ctrl-\ at an idle prompt abandons the line and resets the session.
#[test]
fn ctrl_backslash_at_a_terminal_prompt_resets_the_session() {
    let tty = Tty::start();
    tty.line(BIND, &["bound"]);
    tty.abandon_line("junk", b"\x1c");
    tty.wait_for(RESET);
    let shown = tty.line("x + 100", &["141", "not defined", "error"]);
    assert!(shown.contains("x not defined"), "{}", tty.shown());
    let (status, shown) = tty.end();
    assert_eq!(shown.matches(RESET).count(), 1, "{shown}");
    assert_eq!(status.code(), Some(0), "{shown}");
}

/// Ctrl-\ at a value display, where the terminal sends it as SIGQUIT,
/// resets the session.
#[test]
fn ctrl_backslash_at_a_terminal_display_resets_the_session() {
    let tty = Tty::start();
    tty.line(BIND, &["bound"]);
    tty.line("x + 1", &["42"]);
    tty.wait_raw(false);
    tty.keys(b"\x1c");
    tty.wait_for(RESET);
    let shown = tty.line("x + 100", &["141", "not defined"]);
    assert!(shown.contains("x not defined"), "{}", tty.shown());
    let (status, shown) = tty.end();
    assert_eq!(status.code(), Some(0), "{shown}");
}

/// A TUI display reads the keys itself: Ctrl-C leaves it with the
/// session kept, Ctrl-\ resets the session.
#[test]
fn tui_display_keys() {
    const LEFT: &[u8] = b"\x1b[?1049l";
    let tty = Tty::start();
    let left = || tty.out.lock().windows(LEFT.len()).filter(|w| *w == LEFT).count();
    tty.line(BIND, &["bound"]);
    tty.line(r#"tui::text::text(&"x=[x]")"#, &["x=41"]);
    tty.keys(b"\x03");
    tty.wait_until("the display gone", || left() == 1);
    let shown = tty.line("x + 100", &["141", "not defined"]);
    assert!(shown.contains("141"), "{}", tty.shown());
    tty.leave_display();
    tty.line(r#"tui::text::text(&"again=[x]")"#, &["again=41"]);
    tty.keys(b"\x1c");
    tty.wait_for(RESET);
    let shown = tty.line("x + 1000", &["1041", "not defined"]);
    assert!(shown.contains("x not defined"), "{}", tty.shown());
    let (status, shown) = tty.end();
    assert_eq!(shown.matches(RESET).count(), 1, "{shown}");
    assert_eq!(status.code(), Some(0), "{shown}");
}

/// A TUI display that ends as it starts gives the prompt back, however
/// often: the line editor's first read after it does not fail.
#[test]
fn tui_displays_ending_at_once_give_the_prompt_back() {
    let tty = Tty::start();
    for _ in 0..6 {
        tty.wait_raw(true);
        let from = tty.out.lock().len();
        tty.keys(b"{ tui::exit(null); tui::text::text(&\"bye\") }\r");
        tty.wait_until("prompted after the display", || {
            let seen = plain(&tty.out.lock()[from..]);
            seen.split_once("-: ").is_some_and(|(_, after)| after.contains('〉'))
        });
    }
    let shown = tty.line("40 + 2", &["42", "error"]);
    assert!(shown.contains("42"), "{}", tty.shown());
    tty.leave_display();
    let (status, shown) = tty.end();
    assert!(!shown.contains("cursor position"), "{shown}");
    assert_eq!(status.code(), Some(0), "{shown}");
}
