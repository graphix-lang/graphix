//! A composite literal's effects run in written order, in the node-walk
//! as in a fused kernel.

use std::{
    fs,
    process::{Command, Stdio},
};

/// The values `program` logs through `dbg`, in order, with fusion on and
/// off.
// CR claude for claude: [structure/low] This helper, exit_code.rs::exit_code and
// seqq_nested_credit.rs each spawn graphix by hand and wait with .output()/.status()
// with no deadline. cargo test has no per-test timeout, so a hang here wedges the gate
// silently. common::graphix already gives the 120 s HANG abort, a private cache and a
// tempdir, and returns the status, stdout and stderr these read. Use it in all three
// and delete the hand-rolled Command and temp-file code. Found by reading; not run.
// (tests-shell-compiler-05)
fn dbg_order(case: &str, program: &str) -> [Vec<String>; 2] {
    let path = std::env::temp_dir()
        .join(format!("gx-effect-order-{case}-{}.gx", std::process::id()));
    fs::write(&path, program).expect("write program");
    let run = |fusion: bool| {
        let mut cmd = Command::new(env!("CARGO_BIN_EXE_graphix"));
        cmd.args(["--no-netidx", "--no-cache"]);
        if !fusion {
            cmd.arg("--no-fusion");
        }
        let out = cmd.arg(&path).stdin(Stdio::null()).output().expect("run graphix");
        let text = format!(
            "{}{}",
            String::from_utf8_lossy(&out.stdout),
            String::from_utf8_lossy(&out.stderr)
        );
        text.lines()
            .filter_map(|l| l.split_once("dbg(").map(|(_, r)| r))
            .filter_map(|r| r.rsplit_once(": ").map(|(_, v)| v.trim().to_string()))
            .collect::<Vec<_>>()
    };
    let r = [run(true), run(false)];
    let _ = fs::remove_file(&path);
    r
}

#[test]
fn map_literal_entries_run_in_written_order() {
    let program = "let f = |x: i64| {dbg(x) => dbg(x + 1), dbg(x + 2) => dbg(x + 3)};\n\
                   let r = f(10);\n\
                   sys::exit(sys::time::after_idle(duration:100.ms, 0));\n\
                   r\n";
    let [fused, walked] = dbg_order("map", program);
    assert_eq!(fused, ["10", "11", "12", "13"]);
    assert_eq!(walked, ["10", "11", "12", "13"]);
}
