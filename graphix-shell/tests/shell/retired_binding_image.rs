use super::common;
use common::{CACHE_LOG, graphix, program_image};
use std::fs;

/// `saved` escapes a slot whose body declares the interface module `m`,
/// and `get0` is a dynamic module's first body's `get`. The slot is
/// deleted and the module reloaded before either is first called.
const SOURCE: &str = r#"
let ep = 0;
ep <- select ep { n if n < 12 => n + 1, _ => never() };
let xs: Array<i64> = never();
xs <- select ep { 2 => [1, 2], 6 => [], _ => never() };
let ys: Array<i64> = never();
ys <- select ep { 9 => [0, 1, 2], _ => never() };
let f = |k: i64| { mod m; m::v <- once(k) ~ k * 100; m::get };
let saved: fn(x: i64) -> i64 = never();
let fs = array::map(xs, |x| f(x));
saved <- select ep { 4 => ep ~ fs[1]$, _ => never() };
let src: string = never();
src <- select ep {
    2 => "let v = 1; let get = |x: i64| x + v",
    6 => "let v = 20; let get = |x: i64| x * 1000 + v",
    _ => never()
};
let s = mod d dynamic { sandbox unrestricted; sig { val v: i64; val get: fn(x: i64) -> i64 }; source src };
let get0: fn(x: i64) -> i64 = never();
get0 <- select ep { 4 => ep ~ d::get, _ => never() };
let escaped = array::map(ys, |y| (saved(y + ep), get0(y + ep)));
println("[ep] [d::v] [escaped]");
sys::exit(select ep { 12 => ep ~ 0, _ => never() })
"#;

const PRINTED: &str = "\
10 20 [(210, 11), (211, 12), (212, 13)]
11 20 [(211, 12), (212, 13), (213, 14)]
12 20 [(212, 13), (213, 14), (214, 15)]
";

#[test]
fn a_function_reads_the_binding_of_a_body_that_went_cold_and_warm() {
    for args in [&["--no-fusion"][..], &[]] {
        let dir = tempfile::tempdir().unwrap();
        fs::write(dir.path().join("m.gxi"), "val v: i64; val get: fn(x: i64) -> i64")
            .unwrap();
        fs::write(dir.path().join("m.gx"), "let v = 1; let get = |x: i64| x + v")
            .unwrap();
        let cold = graphix(dir.path(), SOURCE, args, &[CACHE_LOG]);
        assert!(cold.status.success(), "{}", cold.stderr);
        assert_eq!(program_image(&cold), (true, false), "{}", cold.log);
        assert_eq!(cold.stdout, PRINTED, "{args:?}");
        let warm = graphix(dir.path(), SOURCE, args, &[CACHE_LOG]);
        assert!(warm.status.success(), "{}", warm.stderr);
        assert_eq!(program_image(&warm), (false, true), "{}", warm.log);
        assert_eq!(warm.stdout, PRINTED, "{args:?}");
    }
}
