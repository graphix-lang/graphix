//! A script whose root fuses prints its value through the types its
//! typedefs name: the kernel owns them once the region it replaced, which
//! declared them, is gone.

use std::{
    fs,
    process::{Command, Stdio},
};

const PROGRAM: &str = r#"
type C = {col: i64};
type S = {inner: C, n: i64};
let s: S = {inner: {col: 1}, n: 2};
sys::exit(sys::time::after_idle(duration:100.ms, 0));
s
"#;

#[test]
fn fused_root_prints_through_its_typedefs() {
    let dir =
        std::env::temp_dir().join(format!("gx-root-typedef-{}", std::process::id()));
    let _ = fs::remove_dir_all(&dir);
    fs::create_dir_all(&dir).expect("tempdir");
    let path = dir.join("root.gx");
    fs::write(&path, PROGRAM).expect("write program");
    let out = Command::new(env!("CARGO_BIN_EXE_graphix"))
        .arg("--no-netidx")
        .arg("--no-cache")
        .arg(&path)
        .stdin(Stdio::null())
        .output()
        .expect("run graphix");
    let _ = fs::remove_dir_all(&dir);
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert!(stdout.contains("{inner: {col: 1}, n: 2}"), "{stdout}");
}
