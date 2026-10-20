//! A program the shell shows as text has no terminal display: a
//! `tui::suspend` it asks, even in its first cycle, before the shell has
//! chosen how to show it, is refused rather than left waiting.

mod common;
use common::graphix;

#[test]
fn suspend_without_display_is_refused() {
    let dir = tempfile::tempdir().expect("tempdir");
    let program = "let r = tui::suspend(true);\n\
         let refused = sys::time::after_idle(duration:200.ms, r) ~ 0;\n\
         sys::exit(any(refused, sys::time::timer(duration:5.s, false) ~ 1));\n\
         r\n";
    let ran = graphix(dir.path(), program, &["--no-cache"], &[]);
    assert!(ran.status.success(), "{}", ran.stderr);
    assert!(
        ran.stdout.contains(r#"error:`TerminalError("no terminal display is running")"#),
        "{}",
        ran.stdout
    );
}
