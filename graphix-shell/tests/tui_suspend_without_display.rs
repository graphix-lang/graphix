//! A program the shell shows as text has no terminal display: a
//! `tui::suspend` it asks, even in its first cycle, before the shell has
//! chosen how to show it, is refused rather than left waiting.

use std::process::{Command, Stdio};

#[test]
fn suspend_without_display_is_refused() {
    let dir = std::env::temp_dir().join(format!("gx-suspend-{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("tempdir");
    let path = dir.join("suspend.gx");
    std::fs::write(
        &path,
        "let r = tui::suspend(true);\n\
         let refused = sys::time::after_idle(duration:200.ms, r) ~ 0;\n\
         sys::exit(any(refused, sys::time::timer(duration:5.s, false) ~ 1));\n\
         r\n",
    )
    .expect("write program");
    let out = Command::new(env!("CARGO_BIN_EXE_graphix"))
        .arg("--no-netidx")
        .arg("--no-cache")
        .arg(&path)
        .stdin(Stdio::null())
        .output()
        .expect("run graphix");
    let _ = std::fs::remove_dir_all(&dir);
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert!(out.status.success(), "{}", String::from_utf8_lossy(&out.stderr));
    assert!(
        stdout.contains(r#"error:`TerminalError("no terminal display is running")"#),
        "{stdout}"
    );
}
