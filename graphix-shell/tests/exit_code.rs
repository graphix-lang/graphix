//! `sys::exit` exits with the code it is given where the OS can carry it,
//! and with 1, a failure, where it cannot.

mod common;
use common::graphix;

fn exit_code(code: i64) -> Option<i32> {
    let dir = tempfile::tempdir().expect("tempdir");
    let program = format!("sys::exit(sys::time::after_idle(duration:10.ms, {code}))\n");
    graphix(dir.path(), &program, &["--no-cache"], &[]).status.code()
}

#[test]
fn exit_codes_the_os_cannot_carry_are_failures() {
    assert_eq!(exit_code(0), Some(0));
    assert_eq!(exit_code(3), Some(3));
    assert_eq!(exit_code(255), Some(255));
    assert_eq!(exit_code(256), Some(1));
    assert_eq!(exit_code(-1), Some(1));
}
