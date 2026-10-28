//! A warm start decodes a chain of statically bound functions in time
//! linear in its length: each imaged call site the dispatch of the one
//! above it decodes is dispatched in the same pass, so it registers no
//! reads to release again.

mod common;
use common::{CACHE_LOG, graphix, program_image};
use std::{fmt::Write, time::Instant};

#[test]
fn a_warm_chain_starts_faster_than_it_compiles() {
    const N: usize = 3000;
    let mut program = String::new();
    for i in 0..N {
        writeln!(program, "let v{i} = i64:{i};").unwrap();
    }
    writeln!(program, "let f{N} = |x: i64| -> i64 x;").unwrap();
    for i in (0..N).rev() {
        writeln!(program, "let f{i} = |x: i64| -> i64 f{}(x) + v{i};", i + 1).unwrap();
    }
    program.push_str("println(\"[f0(i64:1)]\");\n");
    program.push_str("sys::exit(sys::time::after_idle(duration:1.ms, 0))\n");
    let dir = tempfile::tempdir().expect("tempdir");
    let start = Instant::now();
    let cold = graphix(dir.path(), &program, &["--no-fusion"], &[CACHE_LOG]);
    let cold_time = start.elapsed();
    assert_eq!(program_image(&cold), (true, false), "{}", cold.log);
    let start = Instant::now();
    let warm = graphix(dir.path(), &program, &["--no-fusion"], &[CACHE_LOG]);
    let warm_time = start.elapsed();
    assert_eq!(program_image(&warm), (false, true), "{}", warm.log);
    let sum = (N * (N - 1) / 2 + 1).to_string();
    assert_eq!(cold.stdout.trim(), sum);
    assert_eq!(warm.stdout.trim(), sum);
    assert!(warm_time < cold_time, "warm {warm_time:?}, cold {cold_time:?}");
}
