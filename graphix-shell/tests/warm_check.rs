//! A check over a restored registration image decides what a cold one
//! does: run each program's `--check` twice under a private cache, the
//! first compiling cold and writing the image, the second restoring it.

use std::{fs, process::Command};

/// One `--check` run: whether it passed, what it said, and its log.
struct Ran {
    passed: bool,
    stderr: String,
    log: String,
}

/// `src` checked twice under a private cache; the first run must write
/// the registration image and the second restore it.
fn check_twice(name: &str, src: &str) -> [Ran; 2] {
    let dir =
        std::env::temp_dir().join(format!("gx-warm-check-{name}-{}", std::process::id()));
    let _ = fs::remove_dir_all(&dir);
    fs::create_dir_all(&dir).expect("tempdir");
    let path = dir.join("prog.gx");
    fs::write(&path, src).expect("write program");
    let run = |n: usize| {
        let logs = dir.join(format!("log{n}"));
        let out = Command::new(env!("CARGO_BIN_EXE_graphix"))
            .arg("--no-netidx")
            .arg("--log-dir")
            .arg(&logs)
            .arg("--check")
            .arg(&path)
            .env("XDG_CACHE_HOME", dir.join("cache"))
            .env("RUST_LOG", "info")
            .output()
            .expect("run graphix");
        let log = fs::read_to_string(logs.join("graphix.log")).unwrap_or_default();
        let stderr = String::from_utf8_lossy(&out.stderr).into_owned();
        Ran { passed: out.status.success(), stderr, log }
    };
    let r = [run(0), run(1)];
    let _ = fs::remove_dir_all(&dir);
    assert!(r[0].log.contains("Registration image written to"), "{}", r[0].log);
    assert!(
        r[1].log.contains("Registration image ") && !r[1].log.contains("written to"),
        "the second check did not restore: {}",
        r[1].log
    );
    r
}

/// An interface `val`'s omitted default narrows the call's cells warm as
/// cold: the restored type carries no lambda ids, the binding names the
/// definition.
#[test]
fn interface_defaults_check_warm() {
    let [cold, warm] = check_twice("refused", "rand::rand(#clock: null) + 1\n");
    assert!(
        !cold.passed && !warm.passed,
        "a warm check accepted what a cold one refused"
    );
    let refusal = |r: &Ran| {
        r.stderr.lines().find(|l| l.contains("cannot compute")).map(str::to_owned)
    };
    assert!(refusal(&cold).is_some(), "{}", cold.stderr);
    assert_eq!(refusal(&cold), refusal(&warm), "{}", warm.stderr);
    let [cold, warm] =
        check_twice("accepted", "let x = never();\nx <- rand::rand(#clock: null);\nx\n");
    assert!(cold.passed && warm.passed, "{}\n{}", cold.stderr, warm.stderr);
}
