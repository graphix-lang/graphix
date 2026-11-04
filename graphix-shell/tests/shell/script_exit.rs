//! `sys::exit` ends a script after its output: the value of the cycle
//! that asks to exit, and of every cycle before it, is printed first, on
//! a cold cache as on a warm one. A runtime that stops reports why.

use super::common;
use common::{CACHE_LOG, Ran, graphix, program_image, repl};

/// Run `program` five times over one cache: the first run cold, writing
/// the program's entry, the rest restoring it.
fn cold_then_warm(program: &str, env: &[(&str, &str)], check: impl Fn(&Ran)) {
    let dir = tempfile::tempdir().expect("tempdir");
    let env: Vec<_> = env.iter().copied().chain([CACHE_LOG]).collect();
    for i in 0..5 {
        let ran = graphix(dir.path(), program, &[], &env);
        let image = program_image(&ran);
        assert_eq!(image, (i == 0, i > 0), "run {i} of {program}:\n{}", ran.log);
        check(&ran)
    }
}

#[test]
fn exit_prints_its_own_cycle() {
    for program in [
        "let r = 42;\nsys::exit(r ~ 0);\nr\n",
        "let x = sys::time::after_idle(duration:50.ms, 42);\nsys::exit(x ~ 0);\nx\n",
    ] {
        cold_then_warm(program, &[], |ran| {
            assert_eq!(ran.stdout.trim(), "42", "{program}\n{}", ran.stderr)
        });
    }
}

#[test]
fn a_stopped_runtime_says_why() {
    let program = "let rec f = |n: i64| -> i64 select n { 0 => 0, k => k + f(k - 1) };\n\
                   let depth = 0;\n\
                   depth <- select depth { d if d < 100 => d + 10, _ => never() };\n\
                   sys::exit(sys::time::after_idle(duration:300.ms, depth ~ 0));\n\
                   f(depth)\n";
    cold_then_warm(program, &[("GRAPHIX_ACTIVATION_BUDGET", "20")], |ran| {
        assert!(ran.stderr.contains("exceeded the runtime's budget"), "{}", ran.stderr);
        assert!(!ran.stderr.contains("panicked"), "{}", ran.stderr);
    });
}

/// Bytes holding an abstract value, read before the program mints any,
/// are known for one and refused: the wrapper is registered before the
/// first value is read, and only an image decodes one.
#[test]
fn an_abstract_value_reads_before_any_is_minted() {
    let program = "type Box = Abstract<i64>;\n\
        impl Cast for Box {};\n\
        let t = sys::time::after_idle(duration:50.ms, null);\n\
        let r: Result<Box, [`PackErr(string), `InvalidCast(string)]> = \
        pack::read(t ~ bytes:GydaDDF+kktOYbgtbxPJpH0IgVt4u3Lm5Q8DQm94AAYAAAAAAAAABQ==);\n\
        sys::exit(r ~ 0);\n\
        select r { error as e => \"refused: [e]\", b => \"accepted: [b.0]\" }\n";
    let dir = tempfile::tempdir().expect("tempdir");
    let ran = graphix(dir.path(), program, &[], &[]);
    let refused = r#""refused: error:`PackErr(\"InvalidFormat\")""#;
    assert_eq!(ran.stdout.trim(), refused, "{}", ran.stderr);
}

/// A REPL whose stdin is a pipe goes on reading lines while a value
/// shows, so later lines run and EOF ends the shell. A line's value from
/// the cycle that follows its compile is printed before the next line
/// replaces it, though the shell reads the next line while it shows one.
#[test]
fn piped_lines_are_all_read() {
    const LINES: i64 = 200;
    let dir = tempfile::tempdir().expect("tempdir");
    let mut input: String = (1..=LINES).map(|i| format!("{i} + 0\n")).collect();
    input.push_str("sys::exit(3)\n");
    let ran = repl(dir.path(), &["--no-cache"], &input);
    assert_eq!(ran.status.code(), Some(3), "{}\n{}", ran.stdout, ran.stderr);
    let shown: Vec<i64> = ran.stdout.lines().filter_map(|l| l.parse().ok()).collect();
    assert_eq!(shown, (1..=LINES).collect::<Vec<_>>(), "{}", ran.stdout);
    let ran = repl(dir.path(), &["--no-cache"], "1 + 1\n");
    assert_eq!(ran.status.code(), Some(0), "{}\n{}", ran.stdout, ran.stderr);
    assert!(ran.stdout.lines().any(|l| l == "2"), "{}", ran.stdout);
}

/// The run warns of an unused import as the check does, so under
/// `-W error` the two agree.
#[test]
fn an_unused_import_is_refused_by_the_run_as_by_the_check() {
    let program =
        "use array::map;\nsys::exit(sys::time::after_idle(duration:50.ms, 0));\n1\n";
    let dir = tempfile::tempdir().expect("tempdir");
    for args in [&["-W", "error", "--check"][..], &["-W", "error", "--no-cache"][..]] {
        let ran = graphix(dir.path(), program, args, &[]);
        assert!(!ran.status.success(), "{args:?} accepted it:\n{}", ran.stderr);
        assert!(ran.stderr.contains("unused import `map`"), "{args:?}:\n{}", ran.stderr);
    }
}

/// A restored program compiles nothing, so its image carries the warnings
/// its compile printed, and every run prints them.
#[test]
fn a_program_that_warned_warns_at_every_run() {
    let program =
        "use array::map;\nsys::exit(sys::time::after_idle(duration:50.ms, 0));\n1\n";
    let dir = tempfile::tempdir().expect("tempdir");
    let mut first = None;
    for i in 0..3 {
        let ran = graphix(dir.path(), program, &[], &[CACHE_LOG]);
        let warnings: Vec<String> = ran
            .stderr
            .lines()
            .filter(|l| l.starts_with("WARNING"))
            .map(String::from)
            .collect();
        assert!(ran.stderr.contains("unused import `map`"), "run {i}:\n{}", ran.stderr);
        assert_eq!(program_image(&ran).1, i > 0, "run {i}:\n{}", ran.log);
        assert_eq!(*first.get_or_insert_with(|| warnings.clone()), warnings, "run {i}");
    }
}
