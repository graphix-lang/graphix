//! A definition reached only through a callee bound at runtime has its
//! assertion checked when the call binds it; the program already runs,
//! so a failure is reported on stderr and in the log.

use std::{
    fs,
    process::{Command, Stdio},
};

const PROGRAM: &str = r#"
#[sync]
let f = |n: i64| throttle(#rate: duration:0.001s, n);
let b = true;
let g = select b { true => f, false => |n: i64| n };
println(g(1));
sys::exit(sys::time::after_idle(duration:100.ms, 0))
"#;

#[test]
fn bound_callee_assertion_is_checked() {
    let dir =
        std::env::temp_dir().join(format!("gx-bound-assert-{}", std::process::id()));
    let _ = fs::remove_dir_all(&dir);
    fs::create_dir_all(&dir).expect("tempdir");
    let path = dir.join("bound.gx");
    fs::write(&path, PROGRAM).expect("write program");
    let out = Command::new(env!("CARGO_BIN_EXE_graphix"))
        .arg("--no-netidx")
        .arg("--no-cache")
        .arg("--log-dir")
        .arg(&dir)
        .arg(&path)
        .env("RUST_LOG", "error")
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
    let stderr = String::from_utf8_lossy(&out.stderr);
    let msg = "#[sync]: this function is async";
    assert!(stderr.contains(msg), "no assertion failure on stderr:\n{stderr}");
    assert!(log.contains(msg), "no assertion failure in the log:\n{log}");
}
