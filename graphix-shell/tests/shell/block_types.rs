//! A block's type stays its own after a later REPL input declares a type
//! of the same name at the top level, and is its own above its
//! declaration over an earlier input's type.

use super::common::repl;

#[test]
fn a_later_input_does_not_shadow_a_blocks_type() {
    let dir = tempfile::tempdir().expect("tempdir");
    let input = "let g = |z: i64| { type L = [i64, null]; \
                 let f = |x: i64| { let b: L = 3; (b, x) }; f(z) }\n\
                 g(1)\n\
                 type L = string\n\
                 g(2)\n\
                 sys::exit(3)\n";
    for args in [&["--no-cache"][..], &["--no-cache", "--no-fusion"]] {
        let ran = repl(dir.path(), args, input);
        assert_eq!(ran.status.code(), Some(3), "{}\n{}", ran.stdout, ran.stderr);
        assert!(ran.stdout.contains("(3, 1)"), "{}", ran.stdout);
        assert!(ran.stdout.contains("(3, 2)"), "{}", ran.stdout);
    }
}

#[test]
fn a_blocks_type_above_its_declaration_is_not_an_earlier_inputs() {
    let dir = tempfile::tempdir().expect("tempdir");
    let input = "type L = i64\n\
                 let g = |z: i64| { let f = |x: i64| { let b: L = 3; (b, x) }; \
                 type L = string; f(z) }\n\
                 let h = |z: i64| { let f = |x: i64| { let b: L = \"s\"; (b, x) }; \
                 type L = string; f(z) }\n\
                 h(1)\n\
                 sys::exit(3)\n";
    for args in [&["--no-cache"][..], &["--no-cache", "--no-fusion"]] {
        let ran = repl(dir.path(), args, input);
        assert_eq!(ran.status.code(), Some(3), "{}\n{}", ran.stdout, ran.stderr);
        let all = format!("{}{}", ran.stdout, ran.stderr);
        assert!(all.contains("L does not contain i64"), "{all}");
        assert!(ran.stdout.contains("(\"s\", 1)"), "{}", ran.stdout);
    }
}

/// Two inputs of one text are two sources: what a block of the second
/// declares is its own, never the first input's.
#[test]
fn inputs_of_one_text_declare_apart() {
    let dir = tempfile::tempdir().expect("tempdir");
    let stmt = "let a = { trait Tr { val m: fn(self) -> i64 }; \
                impl Tr for i64 { let m = |x| x + 1 }; Tr::m(1) }";
    let input = format!("{stmt}\na\n{stmt}\na\nsys::exit(3)\n");
    for args in [&["--no-cache"][..], &["--no-cache", "--no-fusion"]] {
        let ran = repl(dir.path(), args, &input);
        assert_eq!(ran.status.code(), Some(3), "{}\n{}", ran.stdout, ran.stderr);
        assert_eq!(
            ran.stdout.matches("\n2\n").count(),
            2,
            "{}\n{}",
            ran.stdout,
            ran.stderr
        );
        assert!(!ran.stderr.contains("error"), "{}", ran.stderr);
    }
}
