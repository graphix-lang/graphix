//! A block's type is one definition whether its program compiled cold
//! or restored from the image cache: a function written above the block's
//! `type`, kept past the activation that declared it and bound at run
//! time, runs the same on a script's second run as on its first.

use super::common;
use common::{CACHE_LOG, graphix, program_image};

/// `body` is a factory `mk` whose function `f` is written above its
/// block's type; the rest keeps `mk(20)`'s `f` after the array that made
/// it shrinks, then calls it at run time with `arg`.
fn outliving(head: &str, body: &str, arg: &str) -> String {
    format!(
        "{head}
let mk = |z: i64| {{
    {body}
    f
}};
let xs = [10, 20];
let fs = array::map(xs, |i| mk(i));
let saved = once(fs);
xs <- saved ~ [];
let gone = select array::len(xs) {{ 0 => xs ~ saved, _ => never() }};
let h = never();
h <- gone;
let g = h[1]$;
println(g({arg}));
sys::exit(sys::time::after_idle(duration:100.ms, 0))
"
    )
}

/// Run `program` cold and then warm, fused and node-walked: each warm run
/// restores the program's image and prints what its cold run printed,
/// `want`.
fn cold_and_warm(program: &str, want: &str) {
    for args in [&[][..], &["--no-fusion"]] {
        let dir = tempfile::tempdir().unwrap();
        let cold = graphix(dir.path(), program, args, &[CACHE_LOG]);
        assert!(cold.status.success(), "{args:?} cold: {}", cold.stderr);
        assert_eq!(program_image(&cold), (true, false), "{}", cold.log);
        assert_eq!(cold.stdout.trim_end(), want, "{args:?} cold: {}", cold.stderr);
        let warm = graphix(dir.path(), program, args, &[CACHE_LOG]);
        assert!(warm.status.success(), "{args:?} warm: {}", warm.stderr);
        assert_eq!(program_image(&warm), (false, true), "{}", warm.log);
        assert_eq!(warm.stdout.trim_end(), want, "{args:?} warm: {}", warm.stderr);
    }
}

#[test]
fn a_type_named_above_its_declaration() {
    let body =
        "let f = |x: i64| -> (L, i64) { let b: L = x + z; (b, cast<L>(\"7\")$ + x) };
    type L = i64;";
    cold_and_warm(&outliving("", body, "2"), "(22, 9)");
}

#[test]
fn a_type_named_above_its_declaration_beside_an_outer_one() {
    let body =
        "let f = |x: i64| -> (L, i64) { let b: L = x + z; (b, cast<L>(\"7\")$ + x) };
    type L = i64;";
    cold_and_warm(&outliving("type L = i64;", body, "2"), "(22, 9)");
}

#[test]
fn a_type_only_the_signature_names() {
    let body = "let f = |x: i64| -> (L, i64) { let b: L = x + z; (b, x) };
    type L = i64;";
    cold_and_warm(&outliving("", body, "2"), "(22, 2)");
}

/// A cast to the block's type reads by its definition, warm as cold.
#[test]
fn a_cast_to_the_type() {
    let body =
        "let f = |x: i64| -> ([L, Error<Any>], i64) (cast<L>(\"[x]\"), x); type L = f64;";
    cold_and_warm(&outliving("", body, "7"), "(7.0, 7)");
}

#[test]
fn an_abstract_type() {
    let body = "let f = |x: i64| { let w = W(x + z); (w.0, select w { W(n) => n * 2 }) };
    type W = Abstract<i64>;";
    cold_and_warm(&outliving("", body, "2"), "(22, 44)");
}

#[test]
fn an_abstract_type_tested() {
    let body = "let f = |x: i64| {
        let w = W(x + z);
        let isw = |a: Any| select a { W as _ => true, _ => false };
        (select w { W(n) => n }, w.0, isw(w))
    };
    type W = Abstract<i64>;";
    cold_and_warm(&outliving("", body, "2"), "(22, 22, true)");
}
