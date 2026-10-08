//! A check over a restored registration image decides what a cold one
//! does: run each program's `--check` twice under a private cache, the
//! first compiling cold and writing the image, the second restoring it.

use std::{fs, process::Command};

fn check_twice(name: &str, src: &str) -> [bool; 2] {
    let dir =
        std::env::temp_dir().join(format!("gx-warm-check-{name}-{}", std::process::id()));
    let _ = fs::remove_dir_all(&dir);
    fs::create_dir_all(&dir).expect("tempdir");
    let path = dir.join("prog.gx");
    fs::write(&path, src).expect("write program");
    let run = || {
        Command::new(env!("CARGO_BIN_EXE_graphix"))
            .arg("--no-netidx")
            .arg("--check")
            .arg(&path)
            .env("XDG_CACHE_HOME", dir.join("cache"))
            .output()
            .expect("run graphix")
            .status
            .success()
    };
    let r = [run(), run()];
    let _ = fs::remove_dir_all(&dir);
    r
}

/// An interface `val`'s omitted default narrows the call's cells warm as
/// cold: the restored type carries no lambda ids, the binding names the
/// definition.
#[test]
fn interface_defaults_check_warm() {
    let refused = check_twice("refused", "rand::rand(#clock: null) + 1\n");
    assert_eq!(refused, [false, false], "a warm check accepted what a cold one refused");
    let accepted =
        check_twice("accepted", "let x = never();\nx <- rand::rand(#clock: null);\nx\n");
    assert_eq!(accepted, [true, true]);
}
