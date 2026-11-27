//! `graphix --debug` and `graphix debug` (`design/debugger.md`): a running
//! program asked from a second process, over its socket.

use crate::common;
use std::{
    fs,
    io::{BufRead, BufReader},
    path::{Path, PathBuf},
    process::{Child, ChildStderr, Command, ExitStatus, Stdio},
};

const PROGRAM: &str = r#"let tick = count(sys::time::timer(duration:1.s, true)$);
let a = [1, 2, 3];
let price = 10;
let qty = a[7]$;
let total = price * qty;
let scale = |x: i64| -> i64 {
    let doubled = x * 2;
    doubled + 1
};
let scaled = scale(price);
let go = false;
sys::exit(select go { true => go ~ 0, false => never() })
"#;

/// A program running with its debugger offered, killed when dropped.
struct Running {
    child: Child,
    socket: PathBuf,
    dir: PathBuf,
    _stderr: BufReader<ChildStderr>,
}

impl Drop for Running {
    fn drop(&mut self) {
        let _ = self.child.kill();
        let _ = self.child.wait();
    }
}

fn graphix(dir: &Path) -> Command {
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_graphix"));
    cmd.env("XDG_CACHE_HOME", dir.join("cache")).env("XDG_RUNTIME_DIR", dir.join("run"));
    cmd
}

impl Running {
    /// Run `program` as `dir/main.gx` with `args` and wait until it
    /// offers its debugger: the line that names the socket.
    fn start(dir: &Path, program: &str, args: &[&str]) -> Self {
        let path = dir.join("main.gx");
        fs::write(&path, program).expect("write the program");
        fs::create_dir_all(dir.join("run")).expect("make the runtime dir");
        let mut cmd = graphix(dir);
        cmd.arg("--no-netidx").args(args).arg(&path);
        cmd.stdin(Stdio::null()).stdout(Stdio::null()).stderr(Stdio::piped());
        let mut child = cmd.spawn().expect("run graphix");
        let mut stderr = BufReader::new(child.stderr.take().expect("its stderr"));
        let mut said = String::new();
        let socket = loop {
            let mut line = String::new();
            let n = stderr.read_line(&mut line).expect("read its stderr");
            assert!(n > 0, "the program ended without offering a debugger:\n{said}");
            match line.trim().strip_prefix("debugger: graphix debug ") {
                Some(socket) => break PathBuf::from(socket),
                None => said.push_str(&line),
            }
        };
        Running { child, socket, dir: dir.to_path_buf(), _stderr: stderr }
    }

    /// `graphix debug` at `target` (the socket where `None`) running
    /// `commands`: whether every one was answered, and what it printed.
    fn debug(&self, target: Option<&str>, commands: &[&str]) -> (bool, String) {
        let mut cmd = graphix(&self.dir);
        cmd.arg("debug");
        match target {
            Some("") => (),
            Some(t) => drop(cmd.arg(t)),
            None => drop(cmd.arg(&self.socket)),
        }
        for c in commands {
            cmd.arg("-c").arg(c);
        }
        let out = common::output(cmd);
        let (stdout, stderr) = (
            String::from_utf8_lossy(&out.stdout).into_owned(),
            String::from_utf8_lossy(&out.stderr).into_owned(),
        );
        assert!(stderr.is_empty() || !out.status.success(), "{stderr}");
        (out.status.success(), stdout + &stderr)
    }

    fn ask(&self, commands: &[&str]) -> String {
        let (answered, out) = self.debug(None, commands);
        assert!(answered, "{commands:?} was refused:\n{out}");
        out
    }

    fn ended(mut self) -> ExitStatus {
        self.child.wait().expect("wait for graphix")
    }
}

#[track_caller]
fn has(out: &str, lines: &[&str]) {
    for want in lines {
        assert!(
            out.lines().any(|l| l.trim_start().starts_with(want)),
            "no line `{want}`:\n{out}"
        );
    }
}

/// A second process asks a running program what a binding holds and why
/// one has no value, evaluates in it, writes to it, and ends it: cold
/// and from the image, in both engines. Inside fused code a place is the
/// block that computes it, its inputs and its output.
#[test]
fn a_second_process_asks_a_running_program() {
    for (label, args, inside) in [
        (
            "jit",
            &[][..],
            &[
                "a fused block at line 10, column 14: i64 = 21",
                "scale(price)",
                "price: i64 = 10",
            ][..],
        ),
        ("interp", &["--no-fusion"][..], &["doubled: i64 = 20"][..]),
    ] {
        let dir = tempfile::tempdir().expect("tempdir");
        for start in ["cold", "warm"] {
            let socket = format!("--debug={}", dir.path().join("d.sock").display());
            let run =
                Running::start(dir.path(), PROGRAM, &[&[socket.as_str()], args].concat());
            assert_eq!(run.socket, dir.path().join("d.sock"));
            let out =
                run.ask(&["print price", "print qty", "why total", "print main.gx:7"]);
            has(
                &out,
                &[
                    "price: i64 = 10",
                    "qty: i64 has no value",
                    "total: i64 has no value",
                    "let total = price * qty",
                    "let qty = a[7]$",
                    "a: Array<i64> = [1, 2, 3]",
                    "everything it reads has a value: the nothing starts in this expression",
                ],
            );
            has(&out, inside);
            let out =
                run.ask(&["price * 3", "let fee = price + 1", "fee", "list main.gx"]);
            has(&out, &["-: i64", "30", "11", "scaled: i64 = 21"]);
            let out = run.ask(&["set price = 100", "print scaled", "print total"]);
            has(&out, &["price <- 100", "scaled: i64 = 201", "total: i64 has no value"]);
            let (answered, out) =
                run.debug(None, &["print nosuch", "set price = \"x\"", "print price"]);
            assert!(!answered, "{label} {start}: {out}");
            has(
                &out,
                &[
                    "error: no binding is named nosuch",
                    "error: \"x\" is not a i64: price is not set",
                    "price: i64 = 100",
                ],
            );
            // the program may end before the answer is written
            let _ = run.debug(None, &["set go = true"]);
            let socket = run.socket.clone();
            let status = run.ended();
            assert!(status.success(), "{label} {start}: {status:?}");
            assert!(!socket.exists(), "{label} {start}: the socket outlived its program");
        }
    }
}

/// `--debug` alone offers a socket under the user's runtime directory,
/// named for the process, that the owner alone may open; `graphix debug`
/// finds it by the process id, and with no target when it is the only one.
#[cfg(unix)]
#[test]
fn the_default_socket_is_the_owners_and_found() {
    use std::os::unix::fs::PermissionsExt;
    let dir = tempfile::tempdir().expect("tempdir");
    let run = Running::start(dir.path(), PROGRAM, &["--no-cache", "--debug"]);
    let pid = run.child.id();
    let socket = dir.path().join("run/graphix").join(format!("debug-{pid}.sock"));
    assert_eq!(run.socket, socket);
    let mode = |p: &Path| fs::metadata(p).expect("stat").permissions().mode() & 0o777;
    assert_eq!((mode(&socket), mode(socket.parent().unwrap())), (0o600, 0o700));
    for target in [format!("{pid}"), String::new()] {
        let (answered, out) = run.debug(Some(&target), &["print price"]);
        assert!(answered, "{target:?}: {out}");
        has(&out, &["price: i64 = 10"]);
    }
    let (answered, out) = run.debug(Some("1"), &["info"]);
    assert!(!answered && out.contains("connecting to"), "{out}");
}
