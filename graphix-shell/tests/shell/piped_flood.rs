//! A piped REPL line that floods the subscriber's channel is shown whole
//! when the input ends, and its `sys::exit` status ends the shell.

use super::common::repl;

const FLOOD: u32 = 5000;

#[test]
fn a_flood_is_shown_whole_at_the_end_of_input() {
    let dir = tempfile::tempdir().expect("tempdir");
    let input = format!(
        "{{ let n = 0; n <- select n {{ m if m < {FLOOD} => m + 1, _ => never() }}; n }}\n"
    );
    for args in [&["--no-cache"][..], &["--no-cache", "--no-fusion"]] {
        let ran = repl(dir.path(), args, &input);
        assert_eq!(ran.status.code(), Some(0), "{}\n{}", ran.stdout, ran.stderr);
        let shown: Vec<u32> = ran.stdout.lines().filter_map(|l| l.parse().ok()).collect();
        assert_eq!(shown, (0..=FLOOD).collect::<Vec<_>>(), "{}", ran.stderr);
    }
}

#[test]
fn a_flood_that_exits_ends_the_shell_with_its_status() {
    let dir = tempfile::tempdir().expect("tempdir");
    let input = "{ let n = 0; n <- n + 1; \
                 sys::exit(select n { m if m == 500 => 9, _ => never() }); n }\n";
    for args in [&["--no-cache"][..], &["--no-cache", "--no-fusion"]] {
        let ran = repl(dir.path(), args, input);
        assert_eq!(ran.status.code(), Some(9), "{}\n{}", ran.stdout, ran.stderr);
        let shown: Vec<u32> = ran.stdout.lines().filter_map(|l| l.parse().ok()).collect();
        assert_eq!(shown, (0..=500).collect::<Vec<_>>(), "{}", ran.stderr);
    }
}
