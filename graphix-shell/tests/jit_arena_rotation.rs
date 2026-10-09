//! A full JIT arena retires its module and the link reinstalls in a
//! fresh one: with an arena that holds one link's regions but not two,
//! the program still fuses and runs to its value, and the rotation says
//! so.

use std::{
    fs,
    path::Path,
    process::{Command, Stdio},
};

/// Regions for two links.
const REGIONS: usize = 512;

fn program() -> String {
    let mut s = String::from("let x = sys::time::after_idle(duration:1.ms, 3);\n");
    for i in 0..REGIONS {
        s.push_str(&format!("let r{i} = #[native] (x * {i} + 1);\n"));
    }
    let sum = (0..REGIONS).map(|i| format!("r{i}")).collect::<Vec<_>>().join(" + ");
    let expect: usize = (0..REGIONS).map(|i| 3 * i + 1).sum();
    s.push_str(&format!("let total = {sum};\n"));
    s.push_str(&format!("sys::exit(select total {{ {expect} => 0, _ => 1 }})\n"));
    s
}

/// Run the program with a 192KB arena, caching its image under `dir`:
/// whether it computed its total, its stderr and its log.
fn run(dir: &Path, path: &Path) -> (bool, String, String) {
    let logs = dir.join("log");
    let _ = fs::remove_dir_all(&logs);
    fs::create_dir_all(&logs).expect("log dir");
    let out = Command::new(env!("CARGO_BIN_EXE_graphix"))
        .arg("--no-netidx")
        .arg("--log-dir")
        .arg(&logs)
        .arg(path)
        .env("RUST_LOG", "warn")
        .env("GRAPHIX_JIT_ARENA", "196608")
        .env("XDG_CACHE_HOME", dir.join("cache"))
        .stdin(Stdio::null())
        .output()
        .expect("run graphix");
    let log = fs::read_dir(&logs)
        .expect("read log dir")
        .filter_map(|e| e.ok())
        .filter(|e| e.path().extension().is_some_and(|x| x == "log"))
        .map(|e| fs::read_to_string(e.path()).expect("read log"))
        .collect::<String>();
    (out.status.success(), String::from_utf8_lossy(&out.stderr).into_owned(), log)
}

#[test]
fn exhausted_arena_rotates() {
    let dir = std::env::temp_dir().join(format!("gx-arena-{}", std::process::id()));
    let _ = fs::remove_dir_all(&dir);
    fs::create_dir_all(&dir).expect("tempdir");
    let path = dir.join("arena.gx");
    fs::write(&path, program()).expect("write program");
    let (cold_ok, cold_err, cold_log) = run(&dir, &path);
    let (warm_ok, warm_err, warm_log) = run(&dir, &path);
    let _ = fs::remove_dir_all(&dir);
    assert!(cold_ok, "the program did not compute its total: {cold_err}\n{cold_log}");
    assert!(
        cold_log.contains("JIT code arena exhausted: retired generation"),
        "a 192KB arena never rotated:\n{cold_log}"
    );
    // the warm start installs the restored regions into the same arena
    assert!(warm_ok, "the warm start did not compute its total: {warm_err}\n{warm_log}");
    assert!(
        warm_log.contains("JIT code arena exhausted: retired generation"),
        "the warm start never rotated:\n{warm_log}"
    );
}

/// A JIT that cannot be built leaves the program unfused, never failed.
#[test]
fn unbuildable_jit_runs_unfused() {
    let dir = std::env::temp_dir().join(format!("gx-noarena-{}", std::process::id()));
    let _ = fs::remove_dir_all(&dir);
    fs::create_dir_all(&dir).expect("tempdir");
    let path = dir.join("noarena.gx");
    fs::write(
        &path,
        "let r = (|x: i64| x * 2 + 1)(20);\n\
         sys::exit(select sys::time::after_idle(duration:1.ms, r) { 41 => 0, _ => 1 })\n",
    )
    .expect("write program");
    let out = Command::new(env!("CARGO_BIN_EXE_graphix"))
        .arg("--no-netidx")
        .arg("--no-cache")
        .arg(&path)
        .env("GRAPHIX_JIT_ARENA", "1000000000000000")
        .stdin(Stdio::null())
        .output()
        .expect("run graphix");
    let _ = fs::remove_dir_all(&dir);
    assert!(out.status.success(), "{}", String::from_utf8_lossy(&out.stderr));
}
