//! A full JIT arena retires its module and the link reinstalls in a
//! fresh one: with an arena that holds one link's regions but not two,
//! the program still fuses and runs to its value, and the rotation says
//! so. The retired module's kernels move to the fresh one at the next
//! cycle's start (`design/jit_memory.md`).

mod common;
use common::{Ran, graphix, program_image};
use std::path::Path;

/// Regions for two links.
const REGIONS: usize = 512;

/// Each region computes `x * i + 1`; a `calling` one through a lambda,
/// so it fuses in a compile task of its own and its region links at the
/// join.
fn program(calling: bool) -> String {
    let mut s = String::from("let x = sys::time::after_idle(duration:1.ms, 3);\n");
    s.push_str("let f = |a: i64, i: i64| a * i + 1;\n");
    for i in 0..REGIONS {
        match calling {
            true => s.push_str(&format!("let r{i} = #[native] f(x, {i});\n")),
            false => s.push_str(&format!("let r{i} = #[native] (x * {i} + 1);\n")),
        }
    }
    let sum = (0..REGIONS).map(|i| format!("r{i}")).collect::<Vec<_>>().join(" + ");
    let expect: usize = (0..REGIONS).map(|i| 3 * i + 1).sum();
    s.push_str(&format!("let total = {sum};\n"));
    s.push_str(&format!("sys::exit(select total {{ {expect} => 0, _ => 1 }})\n"));
    s
}

/// What the image cache does, and every relocation.
const LOG: (&str, &str) =
    ("RUST_LOG", "warn,graphix_shell::cache=info,graphix_compiler::fusion=info");

const MOVED: &str = "JIT kernels out of retired generations";

/// Run `program` with an arena of `arena` bytes, caching its image under
/// `dir`; it computed its total when it succeeded.
fn run(dir: &Path, program: &str, arena: &str) -> Ran {
    graphix(dir, program, &[], &[LOG, ("GRAPHIX_JIT_ARENA", arena)])
}

fn arena_rotates(calling: bool) {
    let dir = tempfile::tempdir().expect("tempdir");
    let program = program(calling);
    let cold = run(dir.path(), &program, "196608");
    let warm = run(dir.path(), &program, "196608");
    let (cold_log, warm_log) = (&cold.log, &warm.log);
    assert!(
        cold.status.success(),
        "the program did not compute its total: {}\n{cold_log}",
        cold.stderr
    );
    assert_eq!(program_image(&cold), (true, false), "{cold_log}");
    assert_eq!(
        program_image(&warm),
        (false, true),
        "the warm start did not restore the program:\n{warm_log}"
    );
    assert!(
        cold_log.contains("JIT code arena exhausted: retired generation"),
        "a 192KB arena never rotated:\n{cold_log}"
    );
    assert!(cold_log.contains(MOVED), "the retired kernels never moved:\n{cold_log}");
    // the warm start installs the restored regions into the same arena
    assert!(
        warm.status.success(),
        "the warm start did not compute its total: {}\n{warm_log}",
        warm.stderr
    );
    assert!(
        warm_log.contains("JIT code arena exhausted: retired generation"),
        "the warm start never rotated:\n{warm_log}"
    );
}

#[test]
fn exhausted_arena_rotates() {
    arena_rotates(false)
}

/// Regions fused in parallel tasks link in batches as the serial walk's
/// do, so none outgrows a fresh arena.
#[test]
fn exhausted_arena_rotates_task_regions() {
    arena_rotates(true)
}

/// A link batch bigger than a whole arena installs in a fresh one of twice
/// the size, as many times as it takes.
#[test]
fn arena_grows_for_a_batch_it_cannot_hold() {
    let dir = tempfile::tempdir().expect("tempdir");
    let ran = run(dir.path(), &program(false), "16384");
    assert!(
        ran.status.success(),
        "the program did not compute its total: {}\n{}",
        ran.stderr,
        ran.log
    );
    assert!(
        ran.log.contains("reinstalling in a fresh module of 32768 bytes"),
        "a 16KB arena never grew:\n{}",
        ran.log
    );
}

/// A JIT that cannot be built leaves the program unfused, never failed.
#[test]
fn unbuildable_jit_runs_unfused() {
    let dir = tempfile::tempdir().expect("tempdir");
    let program = "let r = (|x: i64| x * 2 + 1)(20);\n\
         sys::exit(select sys::time::after_idle(duration:1.ms, r) { 41 => 0, _ => 1 })\n";
    let env = [("RUST_LOG", "warn"), ("GRAPHIX_JIT_ARENA", "1000000000000000")];
    let ran = graphix(dir.path(), program, &["--no-cache"], &env);
    assert!(ran.status.success(), "{}", ran.stderr);
    // the arena cannot be reserved, or the test proves nothing
    assert!(ran.log.contains("fusion is unavailable"), "{}", ran.log);
}

/// Every kernel moves to a fresh generation at every cycle's start, and
/// the program's kernels, a loop's slot state included, answer as before.
#[test]
fn kernels_moved_every_cycle_compute_the_same() {
    let dir = tempfile::tempdir().expect("tempdir");
    let program = "let clock = sys::time::timer(duration:1.ms, true);\n\
         let n = 0;\n\
         n <- clock ~ n + 1;\n\
         let f = |a: i64, i: i64| a * i + 1;\n\
         let r = #[native] f(n, 7);\n\
         let xs = array::init(n % 5 + 1, |i| i);\n\
         let s = #[native] array::fold(array::map(xs, |x| x * n), 0, |a, x| a + x);\n\
         sys::exit(select (n, r, s) { (33, 232, 198) => 0, (33, _, _) => 1, _ => never() })\n";
    let env = [LOG, ("GRAPHIX_JIT_RELOCATE", "every")];
    let ran = graphix(dir.path(), program, &["--no-cache"], &env);
    assert!(ran.status.success(), "{}\n{}", ran.stderr, ran.log);
    assert!(
        ran.log.matches(MOVED).count() >= 30,
        "the kernels did not move every cycle:\n{}",
        ran.log
    );
}
