//! A REPL input holds a value to a written quantified function type as a
//! script does: a lambda that writes a binding outside it is refused.

mod common;
use common::repl;

#[test]
fn a_repl_input_holds_a_rank2_annotation_generally() {
    let dir = tempfile::tempdir().expect("tempdir");
    let input = "{ let y = never(); let p: fn<'b: Any>(x: 'b) -> 'b = |x| { y <- x; x }; \
                 (p(1), p(\"s\"), y + 1) }\n\
                 sys::exit(3)\n";
    let ran = repl(dir.path(), &["--no-cache"], input);
    assert_eq!(ran.status.code(), Some(3), "{}\n{}", ran.stdout, ran.stderr);
    let all = format!("{}\n{}", ran.stdout, ran.stderr);
    assert!(all.contains("the value is not polymorphic in 'b"), "{all}");
}
