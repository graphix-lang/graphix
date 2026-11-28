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
        Self::offered(cmd, dir)
    }

    /// Run `cmd`, a program started with its debugger on, and wait until
    /// it offers it. `dir` holds the runtime directory its clients use.
    fn offered(mut cmd: Command, dir: &Path) -> Self {
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

/// Where a test's program offers its debugger: a socket in `dir`, which
/// is the test's own; on Windows a pipe named for it.
fn socket_in(dir: &Path) -> PathBuf {
    match cfg!(windows) {
        false => dir.join("d.sock"),
        true => {
            let name = dir.file_name().unwrap_or_default().to_string_lossy();
            PathBuf::from(format!(r"\\.\pipe\graphix-test-{name}"))
        }
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
    for (label, args, inside, starts) in [
        (
            "jit",
            &[][..],
            &[
                "a fused block at line 10, column 14: i64 = 21",
                "scale(price)",
                "price: i64 = 10",
            ][..],
            "the nothing starts inside fused code: --no-fusion shows where: a[7]$",
        ),
        (
            "interp",
            &["--no-fusion"][..],
            &["doubled: i64 = 20"][..],
            "`$` dropped the error `ArrayIndexError(\"array index out of bounds\"): a[7]$",
        ),
    ] {
        let dir = tempfile::tempdir().expect("tempdir");
        for start in ["cold", "warm"] {
            let socket = socket_in(dir.path());
            let offer = format!("--debug={}", socket.display());
            let run =
                Running::start(dir.path(), PROGRAM, &[&[offer.as_str()], args].concat());
            assert_eq!(run.socket, socket);
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
                    starts,
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
            let status = run.ended();
            assert!(status.success(), "{label} {start}: {status:?}");
            #[cfg(unix)]
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
    // a script waits for a program that may have just been started, at
    // most its limit
    let mut nobody = graphix(dir.path());
    nobody.args(["debug", "1", "--timeout", "0.2", "-c", "info"]);
    let out = common::output(nobody);
    let said = String::from_utf8_lossy(&out.stderr);
    assert!(!out.status.success() && said.contains("connecting to"), "{said}");
}

/// `--debug-wait` holds a program before its first cycle. A session
/// steps it, sees what each step delivered, writes while it is held, and
/// lets it run to a break; `wait` waits for the break to hold it.
#[test]
fn a_program_started_held_is_stepped() {
    const PROGRAM: &str = r#"let input = 0;
let doubled = input * 2;
let go = false;
sys::exit(select go { true => go ~ 0, false => never() })
"#;
    for (label, args) in [("jit", &[][..]), ("interp", &["--no-fusion"][..])] {
        let dir = tempfile::tempdir().expect("tempdir");
        let run =
            Running::start(dir.path(), PROGRAM, &[&["--debug-wait"], args].concat());
        let out = run.ask(&[
            "print input",
            "step",
            "print doubled",
            "set input = 5",
            "print doubled",
            "step",
            "print doubled",
            "break doubled == 14",
            "continue",
            "set input = 6",
            "set input = 7",
            "wait",
            "set input = 8",
            "print doubled",
            "cycles 2",
        ]);
        let lines = out.lines().map(str::trim).collect::<Vec<_>>();
        let cycles = |from: &str| -> Vec<String> {
            let said = lines.iter().filter(|l| l.starts_with(from));
            said.map(|l| l.rsplit_once(": ").map_or("", |(_, d)| d).to_string()).collect()
        };
        assert!(lines[0].starts_with("input: i64 has no value"), "{label}:\n{out}");
        assert_eq!(
            cycles("held after cycle"),
            ["input = 0, doubled = 0, go = false", "input = 5, doubled = 10"],
            "{label}:\n{out}"
        );
        assert_eq!(cycles("break "), ["input = 7, doubled = 14"], "{label}:\n{out}");
        let held = lines.iter().filter(|l| l.contains("the program is held")).count();
        assert_eq!(held, 2, "{label}:\n{out}");
        let doubled = lines.iter().filter(|l| l.starts_with("doubled: i64 = "));
        let doubled = doubled.map(|l| &l[..l.find("    ").unwrap_or(l.len())]);
        assert_eq!(
            doubled.collect::<Vec<_>>(),
            [
                "doubled: i64 = 0",
                "doubled: i64 = 0",
                "doubled: i64 = 10",
                "doubled: i64 = 14"
            ],
            "{label}:\n{out}"
        );
        assert_eq!(
            cycles("cycle "),
            ["input = 6, doubled = 12", "input = 7, doubled = 14"],
            "{label}:\n{out}"
        );
        // the break still holds it: let it run, and end it
        let _ = run.debug(None, &["continue", "set go = true"]);
        let status = run.ended();
        assert!(status.success(), "{label}: {status:?}");
    }
}

/// `graphix debug --run` is one command for what took two: it runs the
/// program, asks, passes on what the program says, and stops it. With
/// `--json` each answer is a line of JSON; a refusal fails the command.
#[test]
fn one_command_runs_a_program_and_asks() {
    let dir = tempfile::tempdir().expect("tempdir");
    let path = dir.path().join("main.gx");
    fs::write(&path, PROGRAM).expect("write the program");
    let mut cmd = graphix(dir.path());
    cmd.args(["--no-netidx", "--no-fusion", "debug", "--json", "--run"]).arg(&path);
    cmd.args(["-c", "print price", "-c", "why total", "-c", "print nosuch"]);
    let out = common::output(cmd);
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert_eq!(out.status.code(), Some(1), "{stdout}");
    let lines = stdout
        .lines()
        .map(|l| serde_json::from_str::<serde_json::Value>(l).expect("a line of JSON"))
        .collect::<Vec<_>>();
    let answer = |cmd: &str| {
        let of = lines.iter().find(|l| l["cmd"] == cmd);
        of.unwrap_or_else(|| panic!("no answer to {cmd}:\n{stdout}"))
    };
    assert_eq!(answer("print price")["ok"]["bindings"]["bindings"][0]["value"], "10");
    assert_eq!(answer("print nosuch")["err"], "no binding is named nosuch");
    let why = &answer("why total")["ok"]["why"]["why"];
    assert_eq!(why["reads"][1]["because"][0]["text"], "a[7]$", "{why}");
    let said = lines.iter().filter(|l| l["program"] == "err");
    assert!(
        said.clone()
            .any(|l| l["line"].as_str().is_some_and(|s| s.contains("ignored error"))),
        "{stdout}"
    );

    let bad = dir.path().join("bad.gx");
    fs::write(&bad, "let x = ;\n").expect("write the program");
    let mut cmd = graphix(dir.path());
    cmd.args(["--no-netidx", "debug", "--run"]).arg(&bad).args(["-c", "print x"]);
    let out = common::output(cmd);
    let (stdout, stderr) =
        (String::from_utf8_lossy(&out.stdout), String::from_utf8_lossy(&out.stderr));
    assert_eq!(out.status.code(), Some(1), "{stdout}{stderr}");
    assert!(stdout.contains("[err] Error: parsing"), "{stdout}{stderr}");
    assert!(
        stderr.contains("the program ended before it offered a debugger"),
        "{stdout}{stderr}"
    );
}

/// Where nobody is there to end it, a wait gives up at its limit and a
/// step that finds no work is ended: the command fails and says so.
#[test]
fn a_script_does_not_wait_for_ever() {
    const PROGRAM: &str = r#"let input = 0;
let go = false;
sys::exit(select go { true => go ~ 0, false => never() })
"#;
    let dir = tempfile::tempdir().expect("tempdir");
    let socket = socket_in(dir.path());
    let offer = format!("--debug={}", socket.display());
    let run = Running::start(dir.path(), PROGRAM, &["--no-cache", offer.as_str()]);
    let limited = |commands: &[&str]| {
        let mut cmd = graphix(dir.path());
        cmd.arg("debug").arg(&socket).args(["--timeout", "0.3"]);
        for c in commands {
            cmd.arg("-c").arg(c);
        }
        let out = common::output(cmd);
        (out.status.code(), String::from_utf8_lossy(&out.stdout).into_owned())
    };
    let (code, out) = limited(&["wait", "print input"]);
    assert_eq!(code, Some(1), "{out}");
    has(&out, &["error: the program is still running after 0.3 s", "input: i64 = 0"]);
    let (code, out) = limited(&["step", "wait"]);
    assert_eq!(code, Some(1), "{out}");
    has(
        &out,
        &[
            "error: no cycle has done the program's work after 0.3 s: the step is ended \
             and the program held",
            "held before cycle",
        ],
    );
    drop(run);
}

/// One command at a time, each its own `graphix debug`, builds on the
/// last: a hold, a break, a binding and the cycle log are the program's,
/// and a follow in one process hears what another does.
#[test]
fn one_command_at_a_time_builds_on_the_last() {
    const PROGRAM: &str = r#"let input = 0;
let doubled = input * 2;
let go = false;
sys::exit(select go { true => go ~ 0, false => never() })
"#;
    let dir = tempfile::tempdir().expect("tempdir");
    let socket = socket_in(dir.path());
    let offer = format!("--debug={}", socket.display());
    let run = Running::start(dir.path(), PROGRAM, &["--no-cache", offer.as_str()]);
    has(&run.ask(&["pause"]), &["held before cycle"]);
    assert!(run.ask(&["info"]).contains(", held;"));
    has(&run.ask(&["set input = 5"]), &["input <- 5    the program is held"]);
    let stepped = run.ask(&["step"]);
    assert!(stepped.trim_end().ends_with(": input = 5, doubled = 10"), "{stepped}");
    has(&run.ask(&["break doubled == 14"]), &["watch 1: doubled: i64 = 10"]);
    assert_eq!(run.ask(&["let extra = doubled + 1"]), "");
    has(&run.ask(&["extra"]), &["-: i64", "11"]);
    has(&run.ask(&["continue"]), &["running"]);
    has(&run.ask(&["set input = 7"]), &["input <- 7", "doubled = 14    watch 1"]);
    has(&run.ask(&["wait"]), &["held before cycle"]);
    has(&run.ask(&["info"]), &["break 1: doubled == 14"]);
    let logged = run.ask(&["cycles 1"]);
    assert!(
        logged.trim_end().ends_with(": input = 7, doubled = 14, extra = 15"),
        "{logged}"
    );
    has(
        &run.ask(&["unwatch 1", "continue", "watch input"]),
        &["running", "watch 2: input"],
    );

    let mut follower = graphix(dir.path());
    follower.arg("debug").arg(&socket).args(["-c", "info", "--follow", "--timeout", "0"]);
    follower.stdin(Stdio::null()).stdout(Stdio::piped()).stderr(Stdio::null());
    let mut follower = follower.spawn().expect("run graphix debug");
    let mut heard = BufReader::new(follower.stdout.take().expect("its stdout")).lines();
    let mut next = || heard.next().expect("the follower's output").expect("a line");
    // its commands are done once it has said the state: it listens now
    while !next().starts_with("watch 2: input") {}
    has(&run.ask(&["set input = 9"]), &["input <- 9"]);
    assert_eq!(next(), "input = 9    watch 2, cycle ".to_string() + &last_cycle(&run));
    let _ = follower.kill();
    let _ = follower.wait();
    let _ = run.debug(None, &["set go = true"]);
    assert!(run.ended().success());
}

/// The cycle that last delivered something, as `cycles 1` shows it.
fn last_cycle(run: &Running) -> String {
    let logged = run.ask(&["cycles 1"]);
    let number = logged.strip_prefix("cycle ").and_then(|l| l.split(':').next());
    number.unwrap_or_else(|| panic!("no cycle in {logged}")).to_string()
}

/// Two programs with the same file name, each run in its own directory,
/// are told apart: with no target `graphix debug` asks the one that runs
/// in the directory it is run in, and from elsewhere lists them by what
/// they are and where; a program's file or its directory names it.
#[test]
fn programs_in_different_directories_are_told_apart() {
    let dir = tempfile::tempdir().expect("tempdir");
    let dir = dir.path().canonicalize().expect("the real tempdir");
    fs::create_dir_all(dir.join("run")).expect("make the runtime dir");
    let start = |name: &str, x: i64| {
        let home = dir.join(name);
        fs::create_dir_all(&home).expect("make the program's directory");
        let program = format!(
            "let x = {x};\nlet go = false;\nsys::exit(select go {{ true => go ~ 0, false => \
             never() }})\n"
        );
        fs::write(home.join("main.gx"), program).expect("write the program");
        let mut cmd = graphix(&dir);
        cmd.current_dir(&home).args([
            "--no-netidx",
            "--no-cache",
            "--debug",
            "./main.gx",
        ]);
        Running::offered(cmd, &dir)
    };
    let (a, b) = (start("a", 1), start("b", 2));
    let ask = |from: &Path, target: Option<&str>| {
        let mut cmd = graphix(&dir);
        cmd.current_dir(from).arg("debug").args(target).args(["-c", "print x"]);
        let out = common::output(cmd);
        let said = [&out.stdout[..], &out.stderr[..]].concat();
        (out.status.success(), String::from_utf8_lossy(&said).into_owned())
    };
    for (home, x) in [("a", 1), ("b", 2)] {
        let from_home = ask(&dir.join(home), None);
        assert!(from_home.0, "{}", from_home.1);
        has(&from_home.1, &[&format!("x: i64 = {x}")]);
        let by_file = ask(&dir, Some(&format!("{home}/main.gx")));
        has(&by_file.1, &[&format!("x: i64 = {x}")]);
        let by_directory = ask(&dir, Some(home));
        has(&by_directory.1, &[&format!("x: i64 = {x}")]);
    }
    let (answered, listed) = ask(&dir, None);
    assert!(!answered, "{listed}");
    let named = |r: &Running, home: &str| {
        format!(
            "graphix debug {}    # main.gx in {}",
            r.child.id(),
            dir.join(home).display()
        )
    };
    has(
        &listed,
        &[
            "Error: several programs offer a debugger; name one:",
            &named(&a, "a"),
            &named(&b, "b"),
        ],
    );
    // nobody runs there: a script waits for a program to start, at most its limit
    let mut nobody = graphix(&dir);
    nobody.current_dir(&dir).args(["debug", "run", "--timeout", "0.2", "-c", "print x"]);
    let out = common::output(nobody);
    let said = String::from_utf8_lossy(&out.stderr);
    assert!(!out.status.success() && said.contains("no program that runs"), "{said}");
}
