//! A full JIT arena retires its module and the region's build retries
//! in a fresh one: with a tiny arena the program still fuses and runs
//! to its value, and the rotation says so.

use std::{
    fs,
    process::{Command, Stdio},
};

/// Many small regions, so a tiny arena fills while fusing them.
fn program() -> String {
    let mut s = String::from("let x = sys::time::after_idle(duration:1.ms, 3);\n");
    for i in 0..40 {
        s.push_str(&format!("let r{i} = #[native] (x * {i} + 1);\n"));
    }
    let sum = (0..40).map(|i| format!("r{i}")).collect::<Vec<_>>().join(" + ");
    s.push_str(&format!("let total = {sum};\n"));
    s.push_str("sys::exit(select total { 2380 => 0, _ => 1 })\n");
    s
}

#[test]
fn exhausted_arena_rotates() {
    let dir = std::env::temp_dir().join(format!("gx-arena-{}", std::process::id()));
    let _ = fs::remove_dir_all(&dir);
    fs::create_dir_all(&dir).expect("tempdir");
    let path = dir.join("arena.gx");
    fs::write(&path, program()).expect("write program");
    let out = Command::new(env!("CARGO_BIN_EXE_graphix"))
        .arg("--no-netidx")
        .arg("--no-cache")
        .arg("--log-dir")
        .arg(&dir)
        .arg(&path)
        .env("RUST_LOG", "warn")
        .env("GRAPHIX_JIT_ARENA", "65536")
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
        log.contains("JIT code arena exhausted: retired generation"),
        "a 64KB arena never rotated:\n{log}"
    );
}
