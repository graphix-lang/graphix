//! A builtin whose `eval` gave no value over arguments that do not
//! change stands bottom: it is not evaluated again, and logs once.

use std::{
    fs,
    process::{Command, Stdio},
};

/// `divide` over args that fire only at init, beside a counter that
/// keeps its statement updating.
const PROGRAM: &str = r#"
let b = 0;
let ep = 0;
ep <- select ep { n if n < 20 => n + 1, _ => never() };
let t = (ep, divide(10, b));
sys::exit(select ep { 20 => 0, _ => never() })
"#;

fn divide_logs(fusion: bool) -> usize {
    let dir = std::env::temp_dir()
        .join(format!("gx-standing-bottom-{}-{fusion}", std::process::id()));
    let _ = fs::remove_dir_all(&dir);
    fs::create_dir_all(&dir).expect("tempdir");
    let path = dir.join("divide.gx");
    fs::write(&path, PROGRAM).expect("write program");
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_graphix"));
    cmd.arg("--no-cache").arg("--no-netidx").arg("--log-dir").arg(&dir);
    if !fusion {
        cmd.arg("--no-fusion");
    }
    let out = cmd
        .arg(&path)
        .env("RUST_LOG", "error")
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
    log.matches("divide").count()
}

#[test]
fn a_standing_divide_by_zero_logs_once() {
    assert_eq!(divide_logs(false), 1, "node-walk");
    assert_eq!(divide_logs(true), 1, "fusion");
}

/// A failing `divide` is reported on stderr, as `/`'s failure is.
#[test]
fn a_failing_divide_is_reported() {
    let dir =
        std::env::temp_dir().join(format!("gx-divide-report-{}", std::process::id()));
    let _ = fs::remove_dir_all(&dir);
    fs::create_dir_all(&dir).expect("tempdir");
    let path = dir.join("divide.gx");
    fs::write(&path, "let z = 0;\nlet r = divide(7, z);\nsys::exit(sys::time::after_idle(duration:100.ms, 0))\n")
        .expect("write program");
    let out = Command::new(env!("CARGO_BIN_EXE_graphix"))
        .arg("--no-cache")
        .arg("--no-netidx")
        .arg(&path)
        .stdin(Stdio::null())
        .output()
        .expect("run graphix");
    let _ = fs::remove_dir_all(&dir);
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(stderr.contains("divide:"), "{stderr}");
}
