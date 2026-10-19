//! `sys::exit` exits with the code it is given where the OS can carry it,
//! and with 1, a failure, where it cannot.

use std::{
    fs,
    process::{Command, Stdio},
};

fn exit_code(code: i64) -> Option<i32> {
    let path = std::env::temp_dir()
        .join(format!("gx-exit-code-{code}-{}.gx", std::process::id()));
    let program = format!("sys::exit(sys::time::after_idle(duration:10.ms, {code}))\n");
    fs::write(&path, program).expect("write program");
    let status = Command::new(env!("CARGO_BIN_EXE_graphix"))
        .args(["--no-netidx", "--no-cache"])
        .arg(&path)
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status()
        .expect("run graphix");
    let _ = fs::remove_file(&path);
    status.code()
}

#[test]
fn exit_codes_the_os_cannot_carry_are_failures() {
    assert_eq!(exit_code(0), Some(0));
    assert_eq!(exit_code(3), Some(3));
    assert_eq!(exit_code(255), Some(255));
    assert_eq!(exit_code(256), Some(1));
    assert_eq!(exit_code(-1), Some(1));
}
