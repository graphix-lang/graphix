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
    // composite-success unwrap (#199 — Value payload bits vs boxed
    // *mut ValArray composite ABI)
    "{ let a = [i64:1, i64:2, i64:3]; a[1..]$ }",
    "{ let a = [i64:1, i64:2, i64:3]; let x = a[1..]; x$ }",
    "{ let t = [(i64:1, i64:2)]; t[0]$ }",
    // recursion (E3): non-tail, tail loop, double recursion, capture,
    // shadowed name (#206). A mutated-away base case yields
    // Timeout==Timeout (or the whitelisted interp-value/jit-Timeout
    // infinite pure tail recursion) — both handled by the oracle.
    "{ let rec f = |n: i64| -> i64 select n { i64:0 => i64:0, _ => n + f(n - i64:1) }; f(i64:10) }",
    "{ let rec lp = |n: i64, acc: i64| -> i64 select n { i64:0 => acc, _ => lp(n - i64:1, acc + n) }; lp(i64:100, i64:0) }",
    "{ let rec fib = |n: i64| -> i64 select n { i64:0 => i64:0, i64:1 => i64:1, _ => fib(n - i64:1) + fib(n - i64:2) }; fib(i64:12) }",
    "{ let k = i64:3; let rec f = |n: i64| -> i64 select n { i64:0 => k, _ => f(n - i64:1) }; f(i64:5) }",
    "{ let f = |x: i64| -> i64 x + i64:1; let f = |n: i64| -> i64 f(n) * i64:2; f(i64:3) }",
    // ── Scheduled reactive seeds (Phase 3.1) — hand-written shapes
    // exercising the injection driver + the multi-epoch trace oracle
    // before reactive GENERATION lands. Each is a wrapper: a
    // `// schedule-v1:` header + body referencing driver-declared
    // root inputs (the D4 contract). Deterministic (no rand/IO) so
    // they're selfcheck subjects too.
    // Scalar accumulator through the connect lift.
    "// schedule-v1: cap=64 events=512; in0=i64:21; in0=i64:5\n{ let acc = i64:0; acc <- in0 ~ (acc + in0); acc }",
    // Array accumulator (composite lift) — the sliding-window idiom.
    "// schedule-v1: cap=64 events=512; in0=i64:1; in0=i64:2; in0=i64:3\n{ let data: Array<i64> = []; data <- in0 ~ array::push(data, in0); data }",
    // String accumulator (string lift, interpolation per epoch).
    "// schedule-v1: cap=64 events=512; in0=i64:7; in0=i64:8\n{ let s = \"\"; s <- in0 ~ \"[s][in0]\"; s }",
    // Struct accumulator (StructWith lift).
    "// schedule-v1: cap=64 events=512; in0=i64:10; in0=i64:20\n{ let st = { n: i64:0, last: i64:0 }; st <- in0 ~ { st with n: st.n + i64:1, last: in0 }; st.n * i64:100 + st.last }",
    // Cross-cycle builtins over an injected source: count / sum / uniq.
    "// schedule-v1: cap=64 events=512; in0=i64:4; in0=i64:4; in0=i64:9\ncount(in0)",
    "// schedule-v1: cap=64 events=512; in0=i64:4; in0=i64:4; in0=i64:9\nsum(in0)",
    "// schedule-v1: cap=64 events=512; in0=i64:4; in0=i64:4; in0=i64:9\nuniq(in0)",
    // once / take / skip / filter over the injected stream.
    "// schedule-v1: cap=64 events=512; in0=i64:1; in0=i64:2; in0=i64:3\nonce(in0)",
    "// schedule-v1: cap=64 events=512; in0=i64:1; in0=i64:2; in0=i64:3\ntake(i64:2, in0)",
    "// schedule-v1: cap=64 events=512; in0=i64:1; in0=i64:2; in0=i64:3\nskip(i64:2, in0)",
    "// schedule-v1: cap=64 events=512; in0=i64:1; in0=i64:6; in0=i64:3\nfilter(|x: i64| x > i64:2, in0)",
    // Injection-driven guarded select (the selection-memory shape).
    "// schedule-v1: cap=64 events=512; in0=i64:1; in0=i64:2; in0=i64:3; in0=i64:4\nselect i64:0 { i64:0 if in0 % i64:2 == i64:0 => i64:1, _ => i64:2 }",
    // Injection feeding a HOF over a rebuilt array (prev-len shape).
    "// schedule-v1: cap=64 events=512; in0=i64:5; in0=i64:5; in0=i64:6\narray::map([in0], |x: i64| x * i64:2)",
    // Two inputs, mixed types, simultaneous + separate epochs.
    "// schedule-v1: cap=64 events=512; in0=i64:2 in1=f64:1.5; in1=f64:2.5; in0=i64:3\ncast<f64>(in0)$ * in1",
    // A bounded self-clocked counter (no injections needed to move,
    // quiesces by construction at 5).
    "// schedule-v1: cap=64 events=512; in0=i64:1\n{ let c = i64:0; select c { n if n < i64:5 => c <- (n ~ c) + i64:1, _ => never() }; c + (in0 * i64:0) }",
    // A deliberate runaway cut by a small cycle budget (cap
    // determinism under schedules).
    "// schedule-v1: cap=16 events=128; in0=i64:1\n{ let x = i64:0; x <- x + i64:1; x + (in0 * i64:0) }",
];
