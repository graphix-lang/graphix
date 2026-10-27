//! `GRAPHIX_PROFILE_INSTANCES` reports each instance's parent, the
//! instance whose check or elaboration built it; `bench/instances.py`
//! builds its tree from them. The flags are process-wide, so the program
//! runs in a child.

mod common;
use common::graphix;
use std::collections::HashMap;

const PROGRAM: &str = "sys::exit(sys::time::after_idle(duration:100.ms, 0));
let app = |f: fn(y: 'a) -> 'a, x: 'a| f(x);
let outer = |x| app(|y| y * 2, x);
outer(3)
";

/// The field `key` of an INSTANCE line.
fn field<'a>(line: &'a str, key: &str) -> &'a str {
    let at = line.find(&format!(" {key}=")).expect(key) + key.len() + 2;
    line[at..].split(' ').next().unwrap()
}

#[test]
fn an_instance_built_inside_another_names_it_as_parent() {
    let dir = tempfile::tempdir().expect("tempdir");
    let env = [
        ("GRAPHIX_PAR", "off"),
        ("GRAPHIX_PROFILE", "1"),
        ("GRAPHIX_PROFILE_INSTANCES", "1"),
    ];
    let ran = graphix(dir.path(), PROGRAM, &["--no-cache"], &env);
    // the program's instances by id, each with its parent and its position
    let rows: HashMap<&str, (&str, &str)> = ran
        .stderr
        .lines()
        .filter(|l| l.starts_with("INSTANCE ") && l.contains("main.gx"))
        .map(|l| {
            let at = l.split("):").nth(1).unwrap().trim_end_matches('"');
            (field(l, "id"), (field(l, "parent"), at))
        })
        .collect();
    let built_by = |child: &str, parent: &str| {
        rows.values()
            .any(|(p, at)| *at == child && rows.get(p).is_some_and(|p| p.1 == parent))
    };
    let (outer, callback) = ("line: 3, column: 17", "line: 3, column: 25");
    // the callback's instance is built by outer's check
    assert!(built_by(callback, outer), "{rows:#?}");
}
