//! Everything after the script's name is the script's, a graphix option
//! or subcommand name included.

use std::{
    fs,
    process::{Command, Stdio},
};

#[test]
fn arguments_after_the_script_are_the_scripts() {
    let dir = tempfile::tempdir().expect("tempdir");
    let path = dir.path().join("args.gx");
    let program =
        "println(sys::args());\nsys::exit(sys::time::after_idle(duration:10.ms, 0))\n";
    fs::write(&path, program).expect("write program");
    let rest = ["--help", "-n", "3", "fmt", "x", "--check"];
    let out = Command::new(env!("CARGO_BIN_EXE_graphix"))
        .args(["--no-netidx", "--no-cache"])
        .arg(&path)
        .args(rest)
        .stdin(Stdio::null())
        .output()
        .expect("run graphix");
    assert!(out.status.success(), "{}", String::from_utf8_lossy(&out.stderr));
    let quoted = rest.iter().map(|a| format!("\"{a}\"")).collect::<Vec<_>>().join(", ");
    let script = path.canonicalize().expect("canonicalize");
    let expected = format!("[\"{}\", {quoted}]\n", script.display());
    assert_eq!(String::from_utf8_lossy(&out.stdout), expected);
}
