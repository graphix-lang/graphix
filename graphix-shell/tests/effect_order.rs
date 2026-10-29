//! A composite literal's effects run in written order, in the node-walk
//! as in a fused kernel.

mod common;
use common::graphix;

/// The values `program` logs through `dbg`, in order, with fusion on and
/// off.
fn dbg_order(program: &str) -> [Vec<String>; 2] {
    let run = |args: &[&str]| {
        let dir = tempfile::tempdir().expect("tempdir");
        let ran = graphix(dir.path(), program, args, &[]);
        format!("{}{}{}", ran.stdout, ran.stderr, ran.log)
            .lines()
            .filter_map(|l| l.split_once("dbg(").map(|(_, r)| r))
            .filter_map(|r| r.rsplit_once(": ").map(|(_, v)| v.trim().to_string()))
            .collect::<Vec<_>>()
    };
    [run(&["--no-cache"]), run(&["--no-cache", "--no-fusion"])]
}

#[test]
fn map_literal_entries_run_in_written_order() {
    let program = "let f = |x: i64| {dbg(x) => dbg(x + 1), dbg(x + 2) => dbg(x + 3)};\n\
                   let r = f(10);\n\
                   sys::exit(sys::time::after_idle(duration:100.ms, 0));\n\
                   r\n";
    let [fused, walked] = dbg_order(program);
    assert_eq!(fused, ["10", "11", "12", "13"]);
    assert_eq!(walked, ["10", "11", "12", "13"]);
}

/// A `dbg` whose argument a seq step's lowering rewrote names no lowered
/// expression: its id differs from run to run.
#[test]
fn dbg_in_a_seq_step_names_no_lowered_expression() {
    let program = "let r = seq { dbg(i64:7) };\n\
                   sys::exit(sys::time::after_idle(duration:100.ms, 0));\n\
                   r\n";
    for args in [&["--no-cache"][..], &["--no-cache", "--no-fusion"]] {
        let dir = tempfile::tempdir().expect("tempdir");
        let ran = graphix(dir.path(), program, args, &[]);
        let out = format!("{}{}{}", ran.stdout, ran.stderr, ran.log);
        let line = out.lines().find(|l| l.contains("dbg")).expect(&out);
        assert!(line.ends_with("dbg: 7") && !line.contains('#'), "{line}");
    }
}
