//! Module compile tasks write no type variable an earlier task made: the
//! task audit stays silent on a cold check, interfaces and typedefs in the
//! modules included, so a write elaboration makes stands out.

mod common;
use common::graphix;
use std::fs;

#[test]
fn a_cold_check_writes_no_foreign_variable() {
    let dir = tempfile::tempdir().expect("tempdir");
    fs::write(
        dir.path().join("m.gxi"),
        "type P<'a> = ['a, Array<'a>];\nval v: P<i64>;\n",
    )
    .expect("write gxi");
    fs::write(
        dir.path().join("m.gx"),
        "type Q<'b> = {x: 'b, y: 'b};\nlet q: Q<i64> = {x: 1, y: 2};\nlet v: P<i64> = q.x\n",
    )
    .expect("write gx");
    let args = ["--no-cache", "--check"];
    let ran =
        graphix(dir.path(), "mod m;\nm::v\n", &args, &[("GRAPHIX_TASK_AUDIT", "1")]);
    assert!(ran.status.success(), "{}", ran.stderr);
    assert!(!ran.stderr.contains("FOREIGN-WRITE"), "{}", ran.stderr);
}
