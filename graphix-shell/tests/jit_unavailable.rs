//! A host where the JIT cannot be built runs every program unfused, and a
//! `#[native]` there is refused for that reason, never blamed on its
//! position. The arena size is process-wide, so each program runs in a
//! child.

use std::{env, fs, process::Command};

fn run(program: &str, i: usize) -> (String, String) {
    let path =
        env::temp_dir().join(format!("jit_unavailable_{}_{i}.gx", std::process::id()));
    fs::write(&path, program).unwrap();
    let out = Command::new(env!("CARGO_BIN_EXE_graphix"))
        .arg("--no-cache")
        .arg(&path)
        .env("GRAPHIX_JIT_ARENA", "1000000000000000")
        .output()
        .unwrap();
    let _ = fs::remove_file(&path);
    let stdout = String::from_utf8_lossy(&out.stdout).into_owned();
    (stdout, String::from_utf8_lossy(&out.stderr).into_owned())
}

#[test]
fn native_names_the_missing_jit() {
    let exit = "sys::exit(sys::time::after_idle(duration:100.ms, 0));\n";
    let (stdout, _) = run(&format!("{exit}let f = |x: i64| x * 2 + 1;\nf(20)\n"), 0);
    assert_eq!(stdout.lines().last(), Some("41"));
    let (_, stderr) =
        run(&format!("{exit}let f = |x: i64| x * 2 + 1;\n#[native] f(20)\n"), 1);
    assert!(stderr.contains("#[native] cannot hold"), "{stderr}");
    assert!(stderr.contains("fusion is unavailable"), "{stderr}");
}
