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
