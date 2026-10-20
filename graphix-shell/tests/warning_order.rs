//! Warnings from modules checked in parallel print in program order, each
//! at its own file's position.

mod common;
use common::graphix;
use std::fs;

#[test]
fn module_warnings_print_in_program_order() {
    let dir = tempfile::tempdir().expect("tempdir");
    let mods = ["a", "b", "c", "d", "e", "f"];
    let mut main = String::new();
    for m in mods {
        let at = |ext: &str| dir.path().join(format!("{m}.{ext}"));
        fs::write(at("gxi"), "val v: i64;\n").expect("write gxi");
        fs::write(at("gx"), format!("let v = error(`E{m})? + 1\n")).expect("write gx");
        main.push_str(&format!("mod {m};\n"));
    }
    main.push_str("a::v + b::v\n");
    for _ in 0..3 {
        let ran = graphix(dir.path(), &main, &["--no-cache", "--check"], &[]);
        let order: Vec<&str> = ran
            .stderr
            .lines()
            .filter(|l| l.starts_with("WARNING at line: 1, column: 9 in file "))
            .filter_map(|l| l.rsplit('/').next())
            .collect();
        assert_eq!(
            order,
            ["a.gx", "b.gx", "c.gx", "d.gx", "e.gx", "f.gx"],
            "{}",
            ran.stderr
        );
    }
}
