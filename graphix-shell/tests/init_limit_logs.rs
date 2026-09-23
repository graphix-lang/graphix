//! An `array::init` count over the element limit is bottom, and the
//! node-walk says so once per fired count, not once per cycle it reads
//! the standing count.

use std::{
    fs,
    process::{Command, Stdio},
};

/// A fixed oversize count beside a clock that keeps the graph cycling.
const PROGRAM: &str = r#"
let clock = sys::time::timer(duration:10.ms, true);
let n = 0;
n <- clock ~ n + 1;
let a = array::init(20000000, |i| i);
sys::exit(select n { 5 => 0, _ => never() })
"#;

#[test]
fn oversize_init_logs_once() {
    let dir = std::env::temp_dir().join(format!("gx-init-limit-{}", std::process::id()));
    let _ = fs::remove_dir_all(&dir);
    fs::create_dir_all(&dir).expect("tempdir");
    let path = dir.join("init.gx");
    fs::write(&path, PROGRAM).expect("write program");
    let out = Command::new(env!("CARGO_BIN_EXE_graphix"))
        .arg("--no-fusion")
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
        "graphix failed: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    let log = fs::read_dir(&dir)
        .expect("read log dir")
        .filter_map(|e| e.ok())
        .filter(|e| e.path().extension().is_some_and(|x| x == "log"))
        .map(|e| fs::read_to_string(e.path()).expect("read log"))
        .collect::<String>();
    let _ = fs::remove_dir_all(&dir);
    assert_eq!(log.matches("exceeds the").count(), 1, "{log}");
}
