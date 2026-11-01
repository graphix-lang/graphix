//! A definition reached only through a callee bound at runtime has its
//! assertion checked when the call binds it; the program already runs,
//! so a failure is reported on stderr and in the log.

mod common;
use common::{CACHE_LOG, graphix, program_image};

const PROGRAM: &str = r#"
#[sync]
let f = |n: i64| throttle(#rate: duration:0.001s, n);
let b = true;
let g = select b { true => f, false => |n: i64| n };
println(g(1));
sys::exit(sys::time::after_idle(duration:100.ms, 0))
"#;

const MSG: &str = "#[sync]: this function is async";

#[test]
fn bound_callee_assertion_is_checked() {
    let dir = tempfile::tempdir().expect("tempdir");
    let ran = graphix(dir.path(), PROGRAM, &["--no-cache"], &[("RUST_LOG", "error")]);
    assert!(ran.stderr.contains(MSG), "no assertion failure on stderr:\n{}", ran.stderr);
    assert!(ran.log.contains(MSG), "no assertion failure in the log:\n{}", ran.log);
}

/// A pending assertion travels in the session image: a warm start's
/// run-time bind reports it as the cold start's does, and the program
/// runs on.
#[test]
fn bound_callee_assertion_survives_the_image() {
    let dir = tempfile::tempdir().expect("tempdir");
    let cold = graphix(dir.path(), PROGRAM, &[], &[CACHE_LOG]);
    let warm = graphix(dir.path(), PROGRAM, &[], &[CACHE_LOG]);
    assert_eq!(program_image(&cold), (true, false), "cold:\n{}", cold.log);
    assert_eq!(program_image(&warm), (false, true), "warm:\n{}", warm.log);
    for (run, ran) in [("cold", &cold), ("warm", &warm)] {
        assert!(ran.stderr.contains(MSG), "no assertion failure {run}:\n{}", ran.stderr);
        assert!(
            ran.stdout.lines().any(|l| l == "1"),
            "{run} did not run:\n{}",
            ran.stderr
        );
    }
}

/// A literal a run-time bind's instance defines is judged with the
/// function the bind passes for the parameter it calls: the async one
/// fails the assertion, the pure one does not.
const CAPTURED: &str = r#"
#[sync]
let d = |f: fn(v: i64) -> bool, v: i64| filter(v, |x| f(x));
let e: fn(f: fn(v: i64) -> bool, v: i64) -> i64 = d;
e <- never();
println(e(|x| CALLBACK, 3));
sys::exit(sys::time::after_idle(duration:100.ms, 0))
"#;

#[test]
fn bound_callee_captured_param_call_is_judged() {
    let dir = tempfile::tempdir().expect("tempdir");
    for (callback, fails) in
        [("sys::time::after_idle(duration:10.ms, x) > 0", true), ("x > 0", false)]
    {
        let program = CAPTURED.replace("CALLBACK", callback);
        let ran = graphix(dir.path(), &program, &["--no-cache"], &[]);
        assert_eq!(ran.stderr.contains(MSG), fails, "{callback}:\n{}", ran.stderr);
        assert!(ran.stdout.lines().any(|l| l == "3"), "{callback}:\n{}", ran.stderr);
    }
}

/// A `#[tail_recursive]` covers an instance a run-time bind builds: the
/// loop called through a function value, and a function value whose body
/// calls the loop, are fused at the bind and run the loop; a loop whose
/// body does not fuse runs in the node-walk and is reported, once, as the
/// program runs on. So is an instance a bound body builds over a callback
/// that keeps state, after the definition passed its check.
const BOUND_LOOP: &str = r#"
#[tail_recursive]
let rec loop = |n: i64, acc: i64| -> i64 select n { 0 => acc, _ => loop(n - 1, ACC) };
let b = true;
let g = select b { true => CALLEE, false => |n: i64, acc: i64| -> i64 n + acc };
println(g(4, 0));
sys::exit(sys::time::after_idle(duration:100.ms, 0))
"#;

#[test]
fn bound_callee_tail_loop_is_checked() {
    const LOOP_MSG: &str = "#[tail_recursive]: no loop is built for the call `g(4, 0)`";
    let dir = tempfile::tempdir().expect("tempdir");
    for (acc, callee, reported) in [
        ("acc + n", "loop", false),
        ("acc + n", "|n: i64, acc: i64| -> i64 loop(n, acc)", false),
        ("*(&acc) + n", "loop", true),
    ] {
        let program = BOUND_LOOP.replace("ACC", acc).replace("CALLEE", callee);
        let ran = graphix(dir.path(), &program, &["--no-cache"], &[]);
        match reported {
            true => assert_eq!(ran.stderr.matches(LOOP_MSG).count(), 1, "{}", ran.stderr),
            false => assert!(!ran.stderr.contains("#[tail_recursive]"), "{}", ran.stderr),
        }
        assert!(ran.stdout.lines().any(|l| l == "10"), "{callee}:\n{}", ran.stderr);
    }
}

/// A self-recursive function value, out of tail position, fuses at the
/// bind: its body is one kernel whose self-calls call that kernel, so the
/// `#[native]` it holds passes and the call answers.
const BOUND_SELF: &str = r#"
let rec fib = |n: i64| -> i64 #[native] select n { 0 => 0, 1 => 1, _ => fib(n - 1) + fib(n - 2) };
let b = true;
let g = select b { true => fib, false => |n: i64| -> i64 n };
println(g(20));
sys::exit(sys::time::after_idle(duration:100.ms, 0))
"#;

#[test]
fn bound_self_recursion_fuses_at_the_bind() {
    let dir = tempfile::tempdir().expect("tempdir");
    let ran = graphix(dir.path(), BOUND_SELF, &["--no-cache"], &[]);
    assert!(!ran.stderr.contains("#[native]"), "{}", ran.stderr);
    assert!(ran.stdout.lines().any(|l| l == "6765"), "{}", ran.stderr);
}

const BOUND_STATEFUL: &str = r#"
#[tail_recursive]
let rec loop = |f: fn(x: i64) -> i64, n: i64, acc: i64| -> i64
  select n { 0 => acc, _ => loop(f, n - 1, acc + f(n)) };
let a = loop(|x| x, 3, 0);
let b = true;
let g = select b { true => |n: i64| -> i64 loop(|x| count(x), n, 0), false => |n: i64| -> i64 n };
println((a, g(3)));
sys::exit(sys::time::after_idle(duration:100.ms, 0))
"#;

#[test]
fn bound_callee_stateful_instance_is_reported() {
    const MSG: &str = "#[tail_recursive]: no loop is built for the call \
                       `loop(|x| count(x), n, 0)` (line 7, column 44): the recursion of \
                       its instance is not a stateless tail recursion";
    let dir = tempfile::tempdir().expect("tempdir");
    let ran = graphix(dir.path(), BOUND_STATEFUL, &["--no-cache"], &[]);
    assert_eq!(ran.stderr.matches(MSG).count(), 1, "{}", ran.stderr);
    assert!(ran.stdout.lines().any(|l| l == "(6, 3)"), "{}", ran.stderr);
}

/// Where fusion does not run no loop is checked: a collection's slots,
/// bound as the program runs, call the loop in the node-walk and nothing
/// is reported.
const NO_FUSION: &str = r#"
#[tail_recursive]
let rec tri = |n: i64| -> i64 select n { 0 => 0, _ => tri(n - 1) };
#[tail_recursive]
let rec step = |acc: i64, x: i64| -> i64 select x { 0 => acc, _ => step(acc + x, x - 1) };
println(array::map([1, 2, 3], tri));
println(array::fold([1, 2, 3], 0, step));
sys::exit(sys::time::after_idle(duration:100.ms, 0))
"#;

#[test]
fn no_fusion_checks_no_loop() {
    let dir = tempfile::tempdir().expect("tempdir");
    let ran = graphix(dir.path(), NO_FUSION, &["--no-cache", "--no-fusion"], &[]);
    assert!(!ran.stderr.contains("#[tail_recursive]"), "{}", ran.stderr);
    let out: Vec<&str> = ran.stdout.lines().filter(|l| !l.is_empty()).collect();
    assert_eq!(out, ["[0, 0, 0]", "10"], "{}", ran.stderr);
}

/// A collection handed through a function value a loop whose body does not
/// fuse calls it in the node-walk, from each slot: reported once, naming
/// no call the collection built.
const SLOT_LOOP: &str = r#"
#[tail_recursive]
let rec tri = |n: i64| -> i64 select n { 0 => 0, _ => tri(*(&n) - 1) };
let b = true;
let f = select b { true => tri, false => |n: i64| -> i64 n };
println(array::map([1, 2, 3], f));
sys::exit(sys::time::after_idle(duration:100.ms, 0))
"#;

#[test]
fn bound_slot_tail_loop_names_no_built_call() {
    const MSG: &str = "#[tail_recursive]: no loop is built for a call a collection or a \
                       builtin makes through a function value: it did not fuse";
    let dir = tempfile::tempdir().expect("tempdir");
    let ran = graphix(dir.path(), SLOT_LOOP, &["--no-cache"], &[]);
    assert_eq!(ran.stderr.matches("#[tail_recursive]").count(), 1, "{}", ran.stderr);
    assert!(ran.stderr.contains(MSG), "{}", ran.stderr);
    assert!(ran.stdout.lines().any(|l| l == "[0, 0, 0]"), "{}", ran.stderr);
}

/// A run-time bind fuses as its runtime does, wherever its call site was
/// compiled: a function value calling the loop, called through a function
/// of the package root's (compiled with fusion off) or directly, builds the
/// loop either way.
const BOUND_IN_PACKAGE: &str = r#"
#[tail_recursive]
let rec loop = |n: i64, acc: i64| -> i64 select n { 0 => acc, _ => loop(n - 1, acc + n) };
let b = true;
let f = select b { true => |x: i64| -> i64 loop(x, 0), false => |x: i64| -> i64 x };
println(CALL);
sys::exit(sys::time::after_idle(duration:100.ms, 0))
"#;

#[test]
fn bound_callee_in_package_body_builds_the_loop() {
    let dir = tempfile::tempdir().expect("tempdir");
    for call in ["f(4)", "opt::map(4, f)"] {
        let program = BOUND_IN_PACKAGE.replace("CALL", call);
        let ran = graphix(dir.path(), &program, &["--no-cache"], &[]);
        assert!(!ran.stderr.contains("#[tail_recursive]"), "{call}:\n{}", ran.stderr);
        assert!(ran.stdout.lines().any(|l| l == "10"), "{call}:\n{}", ran.stderr);
    }
}

/// A node-walked recursion binds each level at run time and passes its
/// function parameter on: a level's call through it is judged by the
/// function the first level was handed, whether that level was built by
/// the program's compile or by a call through a function value. A sync
/// callback fails neither `#[sync]` nor `#[pure]` at any depth; an async
/// one handed through the value fails the check, once.
const ACTIVATION: &str = r#"
#[ATTR]
let rec loop = |f: fn(x: i64) -> i64, n: i64, acc: i64| -> i64
  select n { 0 => acc, _ => loop(f, n - 1, acc + f(n)) };
let b = true;
let g = select b { true => loop, false => |f: fn(x: i64) -> i64, n: i64, acc: i64| -> i64 n };
println((loop(|x| x, 3, 0), g(|x| CALLBACK, 3, 0)));
sys::exit(sys::time::after_idle(duration:100.ms, 0))
"#;

#[test]
fn bound_activation_is_judged_by_its_callback() {
    let dir = tempfile::tempdir().expect("tempdir");
    for attr in ["sync", "pure"] {
        let head = format!("#[{attr}]");
        for (callback, out, fails) in [
            ("x * 2", "(6, 12)", false),
            ("sys::time::after_idle(duration:1.ms, x * 2)", "(6, 12)", true),
        ] {
            let program = ACTIVATION.replace("ATTR", attr).replace("CALLBACK", callback);
            for args in [&["--no-cache"][..], &["--no-cache", "--no-fusion"]] {
                let ran = graphix(dir.path(), &program, args, &[]);
                let what = format!("{attr} {callback} {args:?}");
                assert_eq!(
                    ran.stderr.matches(&head).count(),
                    fails as usize,
                    "{what}:\n{}",
                    ran.stderr
                );
                assert!(ran.stdout.lines().any(|l| l == out), "{what}:\n{}", ran.stderr);
            }
        }
    }
}
