//! `sys::exit` ends a script after its output: the value of the cycle
//! that asks to exit, and of every cycle before it, is printed first, on
//! a cold cache as on a warm one. A runtime that stops reports why.

mod common;
use common::{CACHE_LOG, Ran, graphix, program_image};

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
