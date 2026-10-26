//! A check over a restored registration image decides what a cold one
//! does: run each program's `--check` twice under a private cache, the
//! first compiling cold and writing the image, the second restoring it.

mod common;
use common::{Ran, graphix};

/// `src` checked twice under a private cache; the first run must write
/// the registration image and the second restore it.
fn check_twice(src: &str) -> [Ran; 2] {
    let dir = tempfile::tempdir().expect("tempdir");
    let run = || graphix(dir.path(), src, &["--check"], &[("RUST_LOG", "info")]);
    let r = [run(), run()];
    assert!(r[0].log.contains("Registration image written to"), "{}", r[0].log);
    assert!(
        r[1].log.contains("Registration image ") && !r[1].log.contains("written to"),
        "the second check did not restore: {}",
        r[1].log
    );
    r
}

fn refusal(r: &Ran) -> Option<String> {
    r.stderr.lines().find(|l| l.contains("cannot compute")).map(str::to_owned)
}

/// An interface `val`'s omitted default narrows the call's cells warm as
/// cold: the restored type carries no lambda ids, the binding names the
/// definition.
#[test]
fn interface_defaults_check_warm() {
    let [cold, warm] = check_twice("rand::rand(#clock: null) + 1\n");
    assert!(
        !cold.status.success() && !warm.status.success(),
        "a warm check accepted what a cold one refused"
    );
    assert!(refusal(&cold).is_some(), "{}", cold.stderr);
    assert_eq!(refusal(&cold), refusal(&warm), "{}", warm.stderr);
    let [cold, warm] =
        check_twice("let x = never();\nx <- rand::rand(#clock: null);\nx\n");
    assert!(
        cold.status.success() && warm.status.success(),
        "{}\n{}",
        cold.stderr,
        warm.stderr
    );
}

/// A restored abstract type prints by its name, as a cold one does.
#[test]
fn abstract_types_print_by_name_warm() {
    let [cold, warm] = check_twice("let s = sys::io::stdout(null);\ns + 1\n");
    assert!(refusal(&cold).is_some_and(|l| l.contains("Stdout")), "{}", cold.stderr);
    assert_eq!(refusal(&cold), refusal(&warm), "{}", warm.stderr);
}
