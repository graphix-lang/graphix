//! A fused kernel's CLIF grows linearly in the locals it binds: a drop
//! that read a local's variables would carry it, as a block parameter,
//! through every join since its definition, and an abort exit's drops
//! through every abort exit, which Cranelift prints as `vN -> vM` aliases
//! and the register allocator spills into the kernel's frame.

use std::{
    fs,
    process::{Command, ExitStatus, Stdio},
};

/// Run `program` with fusion on: its exit status and, with `dump`, the
/// number of value aliases in its CLIF.
fn run(program: &str, dump: bool) -> (ExitStatus, usize) {
    let dir = tempfile::tempdir().expect("a dir for the program");
    let file = dir.path().join("program.gx");
    fs::write(&file, program).expect("write the program");
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_graphix"));
    cmd.arg("--no-cache").arg(&file).env("XDG_CACHE_HOME", dir.path());
    if dump {
        cmd.env("GRAPHIX_DUMP_CLIF", "1");
    }
    let out = cmd
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::piped())
        .output()
        .expect("run the program");
    let stderr = String::from_utf8_lossy(&out.stderr);
    (out.status, stderr.lines().filter(|l| l.contains(" -> v")).count())
}

/// `let f = |x: decl| { let a0 = x; let aI = step(aI-1); ..; aN-1 }`.
fn let_chain(n: usize, decl: &str, step: impl Fn(usize) -> String, arg: &str) -> String {
    let mut s = format!("let f = |x: {decl}| {{\n  let a0 = x;\n");
    for i in 1..n {
        s.push_str(&format!("  let a{i} = {};\n", step(i)));
    }
    s.push_str(&format!("  a{}\n}};\nlet r = f({arg});\nsys::exit(0)\n", n - 1));
    s
}

#[test]
fn scalar_select_chain_has_no_alias_per_join() {
    const N: usize = 100;
    let program = let_chain(
        N,
        "i64",
        |i| format!("select a{} {{ v if v > {i} => v - 1, v => v + 2 }}", i - 1),
        "1",
    );
    let (status, aliases) = run(&program, true);
    assert!(status.success(), "{status}");
    assert!(aliases <= 2 * N, "{aliases} aliases for {N} lets");
}

#[test]
fn owned_fold_chain_has_no_alias_per_exit() {
    const N: usize = 100;
    let program = let_chain(
        N,
        "Array<i64>",
        |i| format!("array::fold(a{}, [], |acc, x| array::push(acc, x))", i - 1),
        "[1, 2, 3]",
    );
    let (status, aliases) = run(&program, true);
    assert!(status.success(), "{status}");
    assert!(aliases <= 40 * N, "{aliases} aliases for {N} lets");
}

/// The kernel's frame fits the runtime's stack: a frame quadratic in the
/// chain overflows it, which aborts the process.
#[test]
fn long_owned_fold_chain_runs() {
    let program = let_chain(
        800,
        "Array<i64>",
        |i| format!("array::fold(a{}, [], |acc, x| array::push(acc, x))", i - 1),
        "[1, 2, 3]",
    );
    let (status, _) = run(&program, false);
    assert!(status.success(), "{status}");
}
