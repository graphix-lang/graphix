//! A script's top level is the root, `/`, in the run as in the check:
//! `self::` and `package::` name its own names, a block inside it is no
//! package of its own, and a module named like a package is a duplicate
//! in both.

use std::{
    fs,
    process::{Command, Stdio},
};

const EXIT: &str = "sys::exit(sys::time::after_idle(duration:100.ms, 0));\n";

/// `--check`'s and the run's outcome for `main` (with `files` beside
/// it): `Ok(stdout)` or `Err(stderr)` for each.
fn verdicts(
    case: &str,
    main: &str,
    files: &[(&str, &str)],
) -> [Result<String, String>; 2] {
    let dir = std::env::temp_dir()
        .join(format!("gx-script-scope-{case}-{}", std::process::id()));
    let _ = fs::remove_dir_all(&dir);
    fs::create_dir_all(&dir).expect("tempdir");
    for (name, text) in files {
        fs::write(dir.join(name), text).expect("write module");
    }
    let path = dir.join("main.gx");
    fs::write(&path, main).expect("write program");
    let go = |check: bool| {
        let mut cmd = Command::new(env!("CARGO_BIN_EXE_graphix"));
        cmd.args(["--no-netidx", "--no-cache"]);
        if check {
            cmd.arg("--check");
        }
        let out = cmd.arg(&path).stdin(Stdio::null()).output().expect("run graphix");
        match out.status.success() {
            true => Ok(String::from_utf8_lossy(&out.stdout).into_owned()),
            false => Err(String::from_utf8_lossy(&out.stderr).into_owned()),
        }
    };
    let r = [go(true), go(false)];
    let _ = fs::remove_dir_all(&dir);
    r
}

fn accepted(case: &str, main: &str, files: &[(&str, &str)], prints: &str) {
    let [check, run] = verdicts(case, main, files);
    check.unwrap_or_else(|e| panic!("{case}: --check refused: {e}"));
    let out = run.unwrap_or_else(|e| panic!("{case}: the run refused: {e}"));
    assert!(out.contains(prints), "{case}: printed {out}");
}

#[test]
fn self_names_the_scripts_top_level() {
    accepted(
        "self-value",
        &format!("let a = 41;\n{EXIT}println(\"[self::a + 1]\")\n"),
        &[],
        "42",
    );
    accepted(
        "self-type",
        &format!("type T = i64;\nlet x: self::T = 41;\n{EXIT}println(\"[x + 1]\")\n"),
        &[],
        "42",
    );
    accepted(
        "self-use",
        &format!("mod m;\nuse self::m::x;\n{EXIT}println(\"[x + 1]\")\n"),
        &[("m.gx", "let x = 41\n")],
        "42",
    );
}

#[test]
fn a_block_is_in_the_scripts_package() {
    accepted(
        "block-impl",
        &format!(
            "type Key = Abstract<i64>;\n\
             let out = {{ impl Eq for Key {{ let eq = |a, b| a.0 % 10 == b.0 % 10 }}; Key(1) == Key(11) }};\n\
             {EXIT}println(\"[out]\")\n"
        ),
        &[],
        "true",
    );
    accepted(
        "block-package",
        &format!(
            "let x = 1;\nlet y = {{ let x = \"s\"; package::x + 1 }};\n{EXIT}println(\"[y]\")\n"
        ),
        &[],
        "2",
    );
}

#[test]
fn a_module_named_like_a_package_is_a_duplicate() {
    let [check, run] = verdicts(
        "dup-str",
        &format!("mod str;\n{EXIT}println(\"[str::x]\")\n"),
        &[("str.gx", "let x = 41\n")],
    );
    for (mode, r) in [("--check", check), ("the run", run)] {
        let e = r.err().unwrap_or_else(|| panic!("{mode} accepted a second `str`"));
        assert!(e.contains("duplicate module definition str"), "{mode}: {e}");
    }
}
