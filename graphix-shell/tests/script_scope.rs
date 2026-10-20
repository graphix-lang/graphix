//! A script's top level is the root, `/`, in the run as in the check:
//! `self::` and `package::` name its own names, a block inside it is no
//! package of its own, and a module named like a package is a duplicate
//! in both.

mod common;
use common::graphix;
use std::fs;

const EXIT: &str = "sys::exit(sys::time::after_idle(duration:100.ms, 0));\n";

/// `--check`'s and the run's outcome for `main` (with `files` beside
/// it): `Ok(stdout)` or `Err(stderr)` for each.
fn verdicts(main: &str, files: &[(&str, &str)]) -> [Result<String, String>; 2] {
    let dir = tempfile::tempdir().expect("tempdir");
    for (name, text) in files {
        fs::write(dir.path().join(name), text).expect("write module");
    }
    let go = |args: &[&str]| {
        let ran = graphix(dir.path(), main, args, &[]);
        match ran.status.success() {
            true => Ok(ran.stdout),
            false => Err(ran.stderr),
        }
    };
    [go(&["--no-cache", "--check"]), go(&["--no-cache"])]
}

fn accepted(case: &str, main: &str, files: &[(&str, &str)], prints: &str) {
    let [check, run] = verdicts(main, files);
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
    accepted(
        "package-sibling",
        &format!("mod m0;\nmod m1;\n{EXIT}println(\"[m1::f(1)]\")\n"),
        &[
            ("m0.gx", "let k0 = 41\n"),
            ("m1.gx", "let f = |x: i64| -> i64 package::m0::k0 + x\n"),
        ],
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
        &format!("mod str;\n{EXIT}println(\"[str::x]\")\n"),
        &[("str.gx", "let x = 41\n")],
    );
    for (mode, r) in [("--check", check), ("the run", run)] {
        let e = r.err().unwrap_or_else(|| panic!("{mode} accepted a second `str`"));
        assert!(e.contains("duplicate module definition str"), "{mode}: {e}");
    }
}

#[test]
fn a_root_duplicate_names_the_top_level() {
    let [check, _] = verdicts("type T = i64;\ntype T = string;\n0\n", &[]);
    let e = check.expect_err("--check accepted a duplicate type");
    assert!(e.contains("T is already defined at the top level"), "{e}");
}
