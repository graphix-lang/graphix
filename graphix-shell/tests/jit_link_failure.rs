//! A fusion pass whose link fails rebuilds the statement without
//! fusion: the program runs node-walked to its value, and says why.
//! `GRAPHIX_FAIL_LINK` (debug builds) fails every link.

use std::{
    fs,
    process::{Command, Stdio},
};

const PROGRAM: &str = "\
let x = sys::time::after_idle(duration:1.ms, 3);
let f = |a| a * a + 1;
let total = f(x) + f(x + 1);
sys::exit(select total { 27 => 0, _ => 1 })
";

#[cfg(debug_assertions)]
#[test]
fn failed_link_rebuilds_unfused() {
    let dir = std::env::temp_dir().join(format!("gx-link-{}", std::process::id()));
    let _ = fs::remove_dir_all(&dir);
    fs::create_dir_all(&dir).expect("tempdir");
    let path = dir.join("link.gx");
    fs::write(&path, PROGRAM).expect("write program");
    let out = Command::new(env!("CARGO_BIN_EXE_graphix"))
        .arg("--no-netidx")
        .arg("--no-cache")
        .arg("--log-dir")
        .arg(&dir)
        .arg(&path)
        .env("RUST_LOG", "warn")
        .env("GRAPHIX_FAIL_LINK", "1")
        .stdin(Stdio::null())
        .output()
        .expect("run graphix");
    let log = fs::read_dir(&dir)
        .expect("read log dir")
        .filter_map(|e| e.ok())
        .filter(|e| e.path().extension().is_some_and(|x| x == "log"))
        .map(|e| fs::read_to_string(e.path()).expect("read log"))
        .collect::<String>();
    let _ = fs::remove_dir_all(&dir);
    assert!(
        out.status.success(),
        "the program did not compute its total: {}\n{log}",
        String::from_utf8_lossy(&out.stderr)
    );
    assert!(
        log.contains("the statement is rebuilt without fusion"),
        "the failed link was not reported:\n{log}"
    );
}
