//! Module compile tasks write no type variable an earlier task made: the
//! task audit stays silent on a cold check, interfaces and typedefs in the
//! modules included, so a write elaboration makes stands out.

use std::{
    fs,
    process::{Command, Stdio},
};

#[test]
fn a_cold_check_writes_no_foreign_variable() {
    let dir = std::env::temp_dir().join(format!("gx-task-audit-{}", std::process::id()));
    let _ = fs::remove_dir_all(&dir);
    fs::create_dir_all(&dir).expect("tempdir");
    fs::write(dir.join("m.gxi"), "type P<'a> = ['a, Array<'a>];\nval v: P<i64>;\n")
        .expect("write gxi");
    fs::write(
        dir.join("m.gx"),
        "type P<'a> = ['a, Array<'a>];\ntype Q<'b> = {x: 'b, y: 'b};\nlet q: Q<i64> = {x: 1, y: 2};\nlet v: P<i64> = q.x\n",
    )
    .expect("write gx");
    fs::write(dir.join("main.gx"), "mod m;\nm::v\n").expect("write main");
    let out = Command::new(env!("CARGO_BIN_EXE_graphix"))
        .args(["--no-netidx", "--no-cache", "--check"])
        .arg(dir.join("main.gx"))
        .env("GRAPHIX_TASK_AUDIT", "1")
        .stdin(Stdio::null())
        .output()
        .expect("run graphix");
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(out.status.success(), "{stderr}");
    assert!(!stderr.contains("FOREIGN-WRITE"), "{stderr}");
    let _ = fs::remove_dir_all(&dir);
}
