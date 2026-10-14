//! Warnings from modules checked in parallel print in program order, each
//! at its own file's position.

use std::{
    fs,
    process::{Command, Stdio},
};

#[test]
fn module_warnings_print_in_program_order() {
    let dir =
        std::env::temp_dir().join(format!("gx-warning-order-{}", std::process::id()));
    let _ = fs::remove_dir_all(&dir);
    fs::create_dir_all(&dir).expect("tempdir");
    let mods = ["a", "b", "c", "d", "e", "f"];
    let mut main = String::new();
    for m in mods {
        fs::write(dir.join(format!("{m}.gxi")), "val v: i64;\n").expect("write gxi");
        fs::write(dir.join(format!("{m}.gx")), format!("let v = error(`E{m})? + 1\n"))
            .expect("write gx");
        main.push_str(&format!("mod {m};\n"));
    }
    main.push_str("a::v + b::v\n");
    fs::write(dir.join("main.gx"), main).expect("write main");
    for _ in 0..3 {
        let out = Command::new(env!("CARGO_BIN_EXE_graphix"))
            .args(["--no-netidx", "--no-cache", "--check"])
            .arg(dir.join("main.gx"))
            .stdin(Stdio::null())
            .output()
            .expect("run graphix");
        let stderr = String::from_utf8_lossy(&out.stderr);
        let order: Vec<&str> = stderr
            .lines()
            .filter(|l| l.starts_with("WARNING at line: 1, column: 9 in file "))
            .filter_map(|l| l.rsplit('/').next())
            .collect();
        assert_eq!(order, ["a.gx", "b.gx", "c.gx", "d.gx", "e.gx", "f.gx"], "{stderr}");
    }
    let _ = fs::remove_dir_all(&dir);
}
