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
/// its definition held an undecided cell or a declared variable, and
/// meets its arguments in the same order: the ones before a union's
/// argument decide its members there too.
#[test]
fn an_instance_checked_again_meets_its_arguments_in_the_same_order() {
    let typedef = "type P<'p, 'q> = [Array<'p>, Array<'q>];\n\
         let f = |y: 'a, z: 'b, x: P<'a, 'b>| x;\n";
    let programs = [
        (
            "let g = |v, w, u| f(v, w, u);\nlet e: Array<i64> = [];\ng(1, \"s\", e)\n",
            "[]",
        ),
        ("let g = |v, w, u| f(v, w, u);\ng(1, \"s\", [1])\n", "[1]"),
        ("let h = |v: 'a, w: 'b, u: P<'a, 'b>| f(v, w, u);\nh(1, \"s\", [1])\n", "[1]"),
        (
            "let g = |u| { let (q, r, p) = u; f(q, r, p) };\n\
             let e: Array<i64> = [];\ng((1, \"s\", e))\n",
            "[]",
        ),
    ];
    for (program, expected) in programs {
        let program = format!("{typedef}{program}");
        assert_eq!(run(&program, false), expected, "{program}");
        assert_eq!(run(&program, true), expected, "under GRAPHIX_NO_SUBST: {program}");
    }
}

/// An instance that checks its body again leaves the defaults of a
/// function it returns to the calls through it, as its definition's check
/// does: a call that passes the label never judges the default.
#[test]
fn an_instance_checked_again_leaves_a_returned_default_to_its_calls() {
    let programs = [
        (
            "let v = |k: i64| |#d = 1, x: i64| cast<string>(d)$;\nv(0)(#d: \"s\", 7)\n",
            "\"s\"",
        ),
        ("let v = |k: i64| |#d = 1, x: i64| cast<string>(d)$;\nv(0)(7)\n", "\"1\""),
        (
            "let v = |k: i64| |j: i64| |#d = 1, x: i64| cast<string>(d)$;\n\
             v(0)(0)(#d: \"s\", 7)\n",
            "\"s\"",
        ),
        (
            "let v = |k: i64| {f: |#d = 1, x: i64| cast<string>(d)$, n: k};\n\
             v(0).f(#d: \"s\", 7)\n",
            "\"s\"",
        ),
        (
            "let v = |k: i64| select k {\n\
               0 => |#d = 1, x: i64| cast<string>(d)$,\n\
               _ => |#d = 2, x: i64| cast<string>(d)$\n\
             };\n\
             let w = v(0);\n\
             w(#d: \"s\", 7)\n",
            "\"s\"",
        ),
    ];
    for (program, expected) in programs {
        assert_eq!(run(program, false), expected, "{program}");
        assert_eq!(run(program, true), expected, "under GRAPHIX_NO_SUBST: {program}");
    }
}

/// An instance that checks its body again types a written test's binds as
/// its definition did: a member the test's structure rules out, the
/// instance's whole scrutinee here, gives them nothing.
#[test]
fn an_instance_checked_again_takes_no_bind_from_a_member_the_structure_rules_out() {
    let typedef = "type T = [(`A, i64), (`B, string)];\n";
    let programs = [
        (
            "let g = 'a: T |x: 'a| select x { T as (`A, h) => `H(h), _ => `N };\n\
             (g((`A, 3)), g((`B, \"abc\")))\n",
            "(`H(3), `N)",
        ),
        (
            "let g = 'a: T |x: 'a| select x { T as (`A, h) => h + 1, _ => 0 };\n\
             (g((`A, 3)), g((`B, \"abc\")))\n",
            "(4, 0)",
        ),
        (
            "let g = 'a: T |x: Array<'a>| select x { Array<T> as [(`A, h), ..] => h + 1, _ => 0 };\n\
             (g([(`A, 3)]), g([(`B, \"abc\")]))\n",
            "(4, 0)",
        ),
        (
            "type U = [(`A, i64, string), (`B, string, bool)];\n\
             let g = 'a: U |x: 'a| select x { U as (`A, h, _) | (`B, _, h) => `H(h), _ => `N };\n\
             (g((`A, 1, \"s\")), g((`B, \"s\", true)))\n",
            "(`H(1), `H(true))",
        ),
    ];
    for (program, expected) in programs {
        let program = format!("{typedef}{program}");
        assert_eq!(run(&program, false), expected, "{program}");
        assert_eq!(run(&program, true), expected, "under GRAPHIX_NO_SUBST: {program}");
    }
}

/// An instance that checks its body again holds a nested definition's
/// variable bounded by an enclosing one, which the instance decided, as
/// its definition does: the variable fits where the enclosing one stands.
#[test]
fn an_instance_checked_again_holds_a_nested_variable_bounded_by_an_enclosing_one() {
    let programs = [
        "let f = 'z: Number |w: 'z| { let g = 'e: 'z |u: 'e| -> 'z u; g(w) };\nf(3)\n",
        "let f = 'z: Number |w: 'z| {\n\
           let g = 'e: 'z |k: fn(x: 'z) -> 'z, u: 'e| k(u);\n\
           g(|x| x, w)\n\
         };\n\
         f(3)\n",
        "let f = 'z: Number |w: 'z| {\n\
           let g = 'b: fn(x: 'z) -> 'z, 'e: 'z |k: 'b, u: 'e| k(u);\n\
           g(|x| x, w)\n\
         };\n\
         f(3)\n",
        "let f = 'z: Number |w: 'z| { let g = 'b: fn(x: 'z) -> 'z |k: 'b| k(w); g(|x| x) };\nf(3)\n",
    ];
    for program in programs {
        assert_eq!(run(program, false), "3", "{program}");
        assert_eq!(run(program, true), "3", "under GRAPHIX_NO_SUBST: {program}");
    }
}
