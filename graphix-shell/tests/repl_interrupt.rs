//! The REPL's Ctrl-C replaces the runtime with a fresh one, idle or busy,
//! and says so; Ctrl-D ends the shell. The REPL here reads a pipe, so
//! Ctrl-C is a SIGINT and Ctrl-D the pipe's end.
#![cfg(unix)]

use parking_lot::Mutex;
use std::{
    io::{Read, Write},
    process::{Child, ChildStdin, Command, ExitStatus, Stdio},
    sync::Arc,
    thread,
    time::{Duration, Instant},
};

const RESET: &str = "interrupted: the session was reset";

/// A REPL running on piped input, its output collected as it comes.
struct Repl {
    child: Child,
    stdin: Option<ChildStdin>,
    stdout: Arc<Mutex<String>>,
    stderr: Arc<Mutex<String>>,
    _dir: tempfile::TempDir,
}

fn collect(mut r: impl Read + Send + 'static) -> Arc<Mutex<String>> {
    let out = Arc::new(Mutex::new(String::new()));
    let o = out.clone();
    thread::spawn(move || {
        let mut buf = [0u8; 4096];
        while let Ok(n) = r.read(&mut buf)
            && n > 0
        {
            o.lock().push_str(&String::from_utf8_lossy(&buf[..n]));
        }
    });
    out
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

    fn line(&mut self, line: &str) {
        let stdin = self.stdin.as_mut().expect("stdin is open");
        writeln!(stdin, "{line}").expect("write a line");
        stdin.flush().expect("flush");
    }

    /// Wait until `out` holds `want` `n` times.
    fn wait_for(&self, out: &Mutex<String>, want: &str, n: usize) {
        let deadline = Instant::now() + Duration::from_secs(60);
        while out.lock().matches(want).count() < n {
            if Instant::now() > deadline {
                panic!(
                    "{want:?} never showed {n} times\nstdout:\n{}\nstderr:\n{}",
                    self.stdout.lock(),
                    self.stderr.lock()
                )
            }
            thread::sleep(Duration::from_millis(20));
        }
    }

    fn sigint(&self) {
        unsafe { libc::kill(self.child.id() as libc::pid_t, libc::SIGINT) };
    }

    /// Close the input, as Ctrl-D does, and wait for the shell to end.
    fn end(mut self) -> (ExitStatus, String, String) {
        drop(self.stdin.take());
        self.exited()
    }

    fn exited(mut self) -> (ExitStatus, String, String) {
        let deadline = Instant::now() + Duration::from_secs(30);
        loop {
            if let Some(status) = self.child.try_wait().expect("try_wait") {
                // let the readers take what is left
                thread::sleep(Duration::from_millis(100));
                return (status, self.stdout.lock().clone(), self.stderr.lock().clone());
            }
            if Instant::now() > deadline {
                let _ = self.child.kill();
                panic!(
                    "the shell did not end\nstdout:\n{}\nstderr:\n{}",
                    self.stdout.lock(),
                    self.stderr.lock()
                )
            }
            thread::sleep(Duration::from_millis(50));
        }
    }
}

/// Ctrl-C at an idle prompt resets the session: a binding made before it
/// is gone after, the shell goes on reading, and Ctrl-D still ends it.
#[test]
fn ctrl_c_at_an_idle_prompt_resets_the_session() {
    let mut repl = Repl::start();
    repl.line("let x = 41");
    repl.line(r#"let ready = println("before")"#);
    repl.wait_for(&repl.stdout, "before", 1);
    repl.sigint();
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

/// Ctrl-C frees a REPL whose input wedged its runtime: the session is
/// reset and the next input runs.
#[test]
fn ctrl_c_frees_a_wedged_repl() {
    let mut repl = Repl::start();
    repl.line("let x = 41");
    repl.line("{ let rec f = |v: i64| -> i64 f(v + i64:1); f(i64:0) }");
    repl.wait_for(&repl.stdout, "-: i64", 1);
    thread::sleep(Duration::from_millis(500));
    repl.sigint();
    repl.wait_for(&repl.stderr, RESET, 1);
    repl.line("x + 1");
    repl.line("sys::exit(5)");
    let (status, stdout, stderr) = repl.exited();
    assert_eq!(status.code(), Some(5), "stdout:\n{stdout}\nstderr:\n{stderr}");
    assert!(stderr.contains("x not defined"), "{stderr}");
    assert!(!stdout.contains("42"), "{stdout}");
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
