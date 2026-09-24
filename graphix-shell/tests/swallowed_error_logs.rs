//! `$` and a handler-less `?` swallow an error, and say so: the
//! node-walk logs the site and the error, and a fused kernel logs the
//! same line through its helper. The logger's module path tells the
//! two apart, so each engine is proven to have produced its own. A
//! collection init count past the element limit bottoms, and says so,
//! in both engines.

use std::{
    fs,
    process::{Command, Stdio},
};

/// A runtime index (rand cannot fold) that is always out of bounds,
/// swallowed once by `$` and once by a `?` no catch covers.
const PROGRAM: &str = r#"
let a = [1, 2, 3];
let i = rand::rand(#start: 10, #end: 20, #clock: 0);
let x = a[i]$;
let y = a[i]?;
sys::exit(sys::time::after_idle(duration:200.ms, 0))
"#;

/// A count past `MAX_ARRAY_INIT_LEN`; `#[native]` proves the kernel ran it.
const OVERSIZE: &str = r#"
let n = 20000000;
let a = #[native] array::init(n, |i| i);
sys::exit(sys::time::after_idle(duration:200.ms, 0))
"#;

/// Run `program` with logging to `dir`; returns (stderr, log text).
fn run(program: &str, no_fusion: bool, label: &str) -> (String, String) {
    let dir =
        std::env::temp_dir().join(format!("gx-swallow-{}-{}", std::process::id(), label));
    let _ = fs::remove_dir_all(&dir);
    fs::create_dir_all(&dir).expect("tempdir");
    let path = dir.join("swallow.gx");
    fs::write(&path, program).expect("write program");
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_graphix"));
    if no_fusion {
        cmd.arg("--no-fusion");
    }
    let out = cmd
        .arg("--no-netidx")
        .arg("--log-dir")
        .arg(&dir)
        .arg(&path)
        .env("RUST_LOG", "warn")
        .stdin(Stdio::null())
        .output()
        .expect("run graphix");
    assert!(
        out.status.success(),
        "{label}: graphix failed: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    let log = fs::read_dir(&dir)
        .expect("read log dir")
        .filter_map(|e| e.ok())
        .filter(|e| e.path().extension().is_some_and(|x| x == "log"))
        .map(|e| fs::read_to_string(e.path()).expect("read log"))
        .collect::<String>();
    let _ = fs::remove_dir_all(&dir);
    (String::from_utf8_lossy(&out.stderr).into_owned(), log)
}

fn assert_diagnostics(label: &str, module: &str, stderr: &str, log: &str) {
    let ignored = format!("WARN [{module}] ignored error in");
    let unhandled = format!("ERROR [{module}] unhandled error in");
    assert!(
        log.contains(&ignored) && log.contains("line: 4, column: 9"),
        "{label}: `$` did not log its swallowed error from {module}:\n{log}"
    );
    assert!(
        log.contains(&unhandled) && log.contains("line: 5, column: 9"),
        "{label}: handler-less `?` did not log its error from {module}:\n{log}"
    );
    assert!(
        stderr.contains("unhandled error in"),
        "{label}: handler-less `?` did not report on stderr:\n{stderr}"
    );
}

#[test]
fn fused_swallowed_errors_are_logged() {
    let (stderr, log) = run(PROGRAM, false, "jit");
    assert_diagnostics("jit", "graphix_compiler::fusion::emit_helpers", &stderr, &log);
}

#[test]
fn node_walk_swallowed_errors_are_logged() {
    let (stderr, log) = run(PROGRAM, true, "interp");
    assert_diagnostics("interp", "graphix_compiler::node::error", &stderr, &log);
}

#[test]
fn init_oversize_is_logged_by_both_engines() {
    for (no_fusion, label) in [(false, "oversize-jit"), (true, "oversize-interp")] {
        let (_, log) = run(OVERSIZE, no_fusion, label);
        assert!(
            log.contains("collection init size 20000000 exceeds"),
            "{label}: the oversize count was not logged:\n{log}"
        );
    }
}
