//! Seed corpus for the mutation engine.
//!
//! A curated set of small, valid graphix expressions biased toward the
//! constructs where fusion/JIT bugs have actually lived: nested HOFs with
//! captures, composite-element + destructure callbacks, select arm
//! bindings, `?` on Results, checked/value-shape arithmetic, datetime/
//! duration, maps, string ops. Mutating + cross-pollinating these (the
//! mutate-deep loop in `mutate`) produces novel combinations of exactly
//! the shapes that recently broke — see design/graphix_fuzz.md (Source A).
//!
//! Each entry is an expression: the fuzzer wraps it as `let result =
//! {seed}` and runs it through the differential oracle. A later milestone
//! will harvest the full ~550-fixture corpus; this hand set bootstraps
//! the loop.

// The full graphix-tests fixture corpus, harvested at build time
// (see build.rs): `pub static HARVESTED: &[&str]`.
include!(concat!(env!("OUT_DIR"), "/harvested_seeds.rs"));

// The regression corpus — every saved finding under `findings/`, embedded
// at build time as `pub static REGRESSION_CORPUS: &[(&str, &str)]`
// (relative-name, program). Run automatically before each fuzz campaign.
include!(concat!(env!("OUT_DIR"), "/regression_corpus.rs"));

/// Hand seeds + the harvested fixture corpus. The hand seeds guarantee
/// the bug-rich shapes are present even if the fixture set shifts; the
/// harvest supplies breadth (every construct the stdlib tests exercise).
pub fn all_seeds() -> Vec<&'static str> {
    SEEDS.iter().copied().chain(HARVESTED.iter().copied()).collect()
}

pub const SEEDS: &[&str] = &[
    // arithmetic + comparison (value/edge sensitive)
    "i64:2 + i64:3 * i64:4",
    "f64:0.1 + f64:0.2 * f64:3.0",
    "(i64:10 / i64:2) - (i64:7 % i64:3)",
    "i64:5 +? i64:3",
    "i64:100 < i64:200 && i64:3 >= i64:3",
    "f64:1.0 / f64:0.0",
    // blocks + lets + internal dependencies
    "{ let a = i64:7; let b = a * i64:2; a + b }",
    "{ let x = f64:3.0; let y = x * x; y - x }",
    "{ let s = \"hi\"; let n = str::len(s); n + i64:1 }",
    // select / pattern matching incl. arm bindings
    "select i64:5 { 0 => i64:100, n => n * i64:2 }",
    "select i64:0 { 0 => \"zero\", n => \"other\" }",
    "{ let v: [i64, null] = i64:7; select v { null as _ => i64:0, i64 as n => n } }",
    // tuples / structs / variants (composite)
    "(i64:1, i64:2, i64:3)",
    "{ let p = (i64:10, i64:20); p.0 + p.1 }",
    "{ let s = { x: i64:1, y: i64:2 }; s.x + s.y }",
    "{ let s = { x: i64:1, y: i64:2 }; { s with x: i64:9 } }",
    // arrays + indexing + slices
    "[i64:1, i64:2, i64:3]",
    "{ let a = [i64:10, i64:20, i64:30]; a[1] }",
    "{ let a = [i64:1, i64:2, i64:3, i64:4]; a[1..3] }",
    // HOFs (scalar + composite + destructure)
    "array::map([i64:1, i64:2, i64:3], |x| x * i64:2)",
    "array::fold([i64:1, i64:2, i64:3], i64:0, |acc, x| acc + x)",
    "array::filter([i64:1, i64:2, i64:3, i64:4], |x| x > i64:2)",
    "array::map([(i64:1, i64:2), (i64:3, i64:4)], |(k, v)| k + v)",
    // nested HOF with grandparent capture
    "{ let n = i64:100; array::map([i64:1, i64:2], |y| array::map([i64:1], |x| x + n)) }",
    // closures + captures
    "{ let k = i64:10; let f = |x| x * k; f(i64:5) }",
    // maps
    "{\"a\" => i64:1, \"b\" => i64:2}",
    "{ let m = {\"a\" => i64:1, \"b\" => i64:2}; m{\"a\"} }",
    // strings
    "str::to_upper(\"hello\")",
    "\"sum is [i64:2 + i64:3]\"",
    // datetime / duration (value-shape arith)
    "datetime:\"2020-01-01T00:00:00Z\" + duration:1.s",
    "duration:1.s + duration:500.ms",
    // error operators
    "{ let r: [i64, null] = i64:5; r? }",
];
