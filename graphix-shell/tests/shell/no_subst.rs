//! `GRAPHIX_NO_SUBST` is the A/B for instances typed by substitution:
//! each instance checks its body again. Ordinary generic programs must
//! run alike either way, so a divergence the knob shows means something.
//! The knob is process-wide, so each program runs in a child.

use super::common;
use common::graphix;

/// Generic programs and the value each prints.
const PROGRAMS: [(&str, &str); 4] = [
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

/// An instance that checks its body again meets its callee's variable
/// standing above a lower bound, bound to nothing: the argument that
/// bound holds fits it.
#[test]
fn an_instance_checked_again_fits_what_a_lower_bound_holds() {
    let program = "type O<'t> = ['t, null];\n\
         type L<'a> = [`Nil, `Cons('a, L<'a>)];\n\
         type B<'a> = ('a, O<'a>);\n\
         let f = 'a0: Array<L<B<'a1>>>, 'a1: O<O<'a2>>, 'a2: string |x0: 'a0, x1: 'a1, x2: 'a2| x0;\n\
         let h = 'b0: Array<L<B<'b1>>>, 'b1: O<O<'b2>>, 'b2: string |y0: 'b0, y1: 'b1, y2: 'b2| f(y0, y1, y2);\n\
         h([`Nil], \"s\", \"s\")\n";
    assert_eq!(run(program, true), "[`Nil]");
}

/// An instance that checks its body again holds a decided argument where
/// its definition held an undecided cell or a declared variable: one that
/// two undecided members of a union could take waits for the call's other
/// arguments there too.
#[test]
fn an_instance_checked_again_waits_for_the_arguments_that_decide_a_union() {
    let typedef = "type P<'p, 'q> = [Array<'p>, Array<'q>];\n\
         let f = |x: P<'a, 'b>, y: 'a, z: 'b| x;\n";
    let programs = [
        ("let g = |u, v, w| f(u, v, w);\ng([], 1, \"s\")\n", "[]"),
        ("let g = |u, v, w| f(u, v, w);\ng([1], 1, \"s\")\n", "[1]"),
        ("let h = |u: P<'a, 'b>, v: 'a, w: 'b| f(u, v, w);\nh([1], 1, \"s\")\n", "[1]"),
        ("let g = |u| { let (p, q, r) = u; f(p, q, r) };\ng(([], 1, \"s\"))\n", "[]"),
    ];
    for (program, expected) in programs {
        let program = format!("{typedef}{program}");
        assert_eq!(run(&program, false), expected, "{program}");
        assert_eq!(run(&program, true), expected, "under GRAPHIX_NO_SUBST: {program}");
    }
}
