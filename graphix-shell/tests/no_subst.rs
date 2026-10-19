//! `GRAPHIX_NO_SUBST` is the A/B for instances typed by substitution:
//! each instance checks its body again. Ordinary generic programs must
//! run alike either way, so a divergence the knob shows means something.
//! The knob is process-wide, so each program runs in a child.

mod common;
use common::graphix;

/// Generic programs and the value each prints.
const PROGRAMS: [(&str, &str); 5] = [
    ("let len = |l| select l { [] => 0, [_, ..] => 1 };\nlen([1, 2])\n", "1"),
    (
        "let twice = |f: fn(x: 'a) -> 'a, x: 'a| f(f(x));\n(twice(|x| x + 1, 1), twice(|s| \"[s]!\", \"a\"))\n",
        "(3, \"a!!\")",
    ),
    (
        "let sum = |a| array::fold(a, 0, |acc, x| acc + x);\nsum(array::map([1, 2, 3], |x| x * 2))\n",
        "12",
    ),
    (
        "let app = |f: fn<'b: Any>(x: 'b) -> 'b| f;\nlet g = app(|x| x);\n(g(1), g(\"s\"))\n",
        "(1, \"s\")",
    ),
    (
        "let first = |o: [`Some('a), `None], d: 'a| select o { `Some(x) => x, `None => d };\n\
         (first(`Some(1), 0), first(`None, \"d\"))\n",
        "(1, \"d\")",
    ),
];

fn run(program: &str, no_subst: bool) -> String {
    let dir = tempfile::tempdir().expect("tempdir");
    let exit = "sys::exit(sys::time::after_idle(duration:100.ms, 0));\n";
    let env: &[(&str, &str)] = match no_subst {
        true => &[("GRAPHIX_NO_SUBST", "1")],
        false => &[],
    };
    let ran = graphix(dir.path(), &format!("{exit}{program}"), &["--no-cache"], env);
    ran.stdout.lines().last().unwrap_or("").to_string()
}

#[test]
fn generic_programs_run_alike_without_substitution() {
    for (program, expected) in PROGRAMS.iter() {
        assert_eq!(run(program, false), *expected, "{program}");
        assert_eq!(run(program, true), *expected, "under GRAPHIX_NO_SUBST: {program}");
    }
}
