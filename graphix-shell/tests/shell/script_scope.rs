//! A script's top level is the root, `/`, in the run as in the check:
//! `self::` and `package::` name its own names, a block inside it is no
//! package of its own, and a module named like a package is a duplicate
//! in both.

use super::common;
use common::graphix;
use std::fs;

const EXIT: &str = "sys::exit(sys::time::after_idle(duration:100.ms, 0));\n";

/// `--check`'s and the run's outcome for `main` (with `files` beside
/// it): `Ok(stdout)` or `Err(stderr)` for each.
fn verdicts(main: &str, files: &[(&str, &str)]) -> [Result<String, String>; 2] {
    let dir = tempfile::tempdir().expect("tempdir");
    for (name, text) in files {
        let path = dir.path().join(name);
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent).expect("a dir for the module");
        }
        fs::write(path, text).expect("write module");
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
fn a_module_named_like_a_package_is_refused() {
    let [check, run] = verdicts(
        &format!("mod str;\n{EXIT}println(\"[str::x]\")\n"),
        &[("str.gx", "let x = 41\n")],
    );
    for (mode, r) in [("--check", check), ("the run", run)] {
        let e = r.err().unwrap_or_else(|| panic!("{mode} accepted a second `str`"));
        assert!(e.contains("`str` is the name of a package"), "{mode}: {e}");
    }
}

#[test]
fn a_root_duplicate_names_the_top_level() {
    let [check, _] = verdicts("type T = i64;\ntype T = string;\n0\n", &[]);
    let e = check.expect_err("--check accepted a duplicate type");
    assert!(e.contains("T is already defined at the top level"), "{e}");
}

/// A module that writes a holder name of the script's top level through
/// `package::` makes it written there, so the top level's reads of the
/// other name agree whether they stand before or after `mod m`.
#[test]
fn a_module_writes_a_holder_through_package() {
    let main = |after: bool| {
        let reads = "let z = b.f(\"z\");\n";
        let (before, after) = if after { ("", reads) } else { (reads, "") };
        format!(
            "let s = {{f: |x| x}};\nlet (a, b) = (s, s);\nlet w = b.f(\"w\");\n{before}\
             mod m;\n{after}{EXIT}println(\"[w] [z] [m::n]\")\n"
        )
    };
    let by_path = "package::a <- never<{f: fn(x: i64) -> i64}>();\n\
                   let n = package::a.f(1)\n";
    let by_use = "use package::a;\na <- never<{f: fn(x: i64) -> i64}>();\n\
                  let n = a.f(1)\n";
    for (case, m) in [("path", by_path), ("use", by_use)] {
        for after in [false, true] {
            let case = format!("{case}, reads after the module: {after}");
            accepted(&case, &main(after), &[("m.gx", m)], "w z 1");
        }
    }
}

/// A module below the root whose child writes one of its holder names
/// through `package::` and its path makes it written there, and so does a
/// `self::` write in the holder's own block: the reads of the other name
/// agree wherever they stand.
#[test]
fn a_holder_written_through_self_or_its_package_path() {
    let holder = "let s = {f: |x| x};\nlet (a, b) = (s, s);\nlet w = b.f(\"w\");\n";
    let reads = "let z = b.f(\"z\");\n";
    let write = "never<{f: fn(x: i64) -> i64}>();\n";
    let main = format!("mod p;\n{EXIT}println(p::out)\n");
    let m = format!("package::p::a <- {write}let n = package::p::a.f(1)\n");
    for after in [false, true] {
        let (before, after_) = if after { ("", reads) } else { (reads, "") };
        let p = format!("{holder}{before}mod m;\n{after_}let out = \"[w] [z] [m::n]\"\n");
        let case = format!("package path, reads after the module: {after}");
        accepted(&case, &main, &[("p.gx", &p), ("p/m.gx", &m)], "w z 1");
        let main = format!(
            "{holder}{before}self::a <- {write}{after_}{EXIT}println(\"[w] [z] [a.f(1)]\")\n"
        );
        let case = format!("self, reads after the write: {after}");
        accepted(&case, &main, &[], "w z 1");
    }
}
