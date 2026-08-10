//! Adversarially nested programs must not overflow the stack.
//!
//! An embedded engine compiles programs it didn't write, so nesting
//! depth is attacker-controlled. Every recursion the pipeline drives off
//! program depth runs under `graphix_compiler::stack::ensure_sufficient`
//! (heap segments instead of the thread's stack), and the parser rejects
//! anything past `parser::max_nesting()` — so a deep program is a
//! compile error, never an abort.
//!
//! Each case runs in a CHILD PROCESS with a deliberately SMALL worker
//! stack (`STACK`, a quarter of what a tokio worker gets): a stack
//! overflow aborts the process, so it can't be caught in-process, and a
//! child lets the failure name the shape that caused it. `DEPTH` is far
//! past the parse limit — every case here should come back as a clean
//! error, and the point of the test is that it comes back at all.

use graphix_compiler::expr::{FilesResolver, Source};
use graphix_rt::NoExt;
use graphix_shell::{Mode, ShellBuilder};
use std::{
    collections::HashMap,
    env, fs,
    process::{Command, Stdio},
};

const STACK: usize = 512 * 1024;

/// Exit code the child uses for "the nesting limit refused this", so
/// the parent can tell a limit rejection from every other outcome.
const REFUSED: i32 = 3;

/// Past `parser::max_nesting()`: every shape must come back REFUSED.
const REJECTED: usize = 100_000;

/// Deepest nesting the limit admits for every shape, with room for the
/// shapes that cost several parser knots per source level. Derived from
/// the limit rather than fixed so this stays a real test of the deep
/// path if the limit moves — the pass that drives the guarded walks
/// only means something while the parser is still accepting.
fn accepted() -> usize {
    graphix_compiler::expr::parser::max_nesting() / 8
}

const SHAPE_VAR: &str = "GRAPHIX_DEEP_SHAPE";
const DEPTH_VAR: &str = "GRAPHIX_DEEP_DEPTH";

/// `(name, source)` — one per construct whose nesting recurses somewhere
/// in the pipeline. Add a case here when you add a recursive construct.
fn program(shape: &str, d: usize) -> String {
    match shape {
        "parens" => format!("let x = {}1{}", "(1 + ".repeat(d), ")".repeat(d)),
        // The bracket shapes are also valid netidx `Value` literals, so
        // `literal()` runs them through netidx-value's own parser —
        // these cover its nesting guard as much as graphix's.
        "array" => format!("let x = {}1{}", "[".repeat(d), "]".repeat(d)),
        "maplit" => format!("let x = {}1{}", r#"{"k" => "#.repeat(d), "}".repeat(d)),
        "slicepat" => {
            let pat = format!("{}x{}", "[".repeat(d), "]".repeat(d));
            format!("let v = [1];\nlet x = select v {{ {pat} => 1, _ => 0 }}")
        }
        "tuple" => format!("let x = {}(1, 1){}", "(1, ".repeat(d), ")".repeat(d)),
        "structlit" => format!("let x = {}1{}", "{a: ".repeat(d), "}".repeat(d)),
        "variant" => format!("let x = {}1{}", "`A(".repeat(d), ")".repeat(d)),
        "typ" => {
            format!("let x: {}i64{} = never()", "Array<".repeat(d), ">".repeat(d))
        }
        "uniontyp" => format!("let x: [{}null] = null", "i64, ".repeat(d)),
        "lambda" => format!("let f = {}1", "|x| ".repeat(d)),
        "call" => {
            format!("let f = |x| x;\nlet y = {}1{}", "f(".repeat(d), ")".repeat(d))
        }
        "block" => format!("let x = {}1{}", "{ let a = 1; ".repeat(d), " }".repeat(d)),
        "field" => format!("let s = {{a: 1}};\nlet x = s{}", ".a".repeat(d)),
        "index" => format!("let a = [1];\nlet x = a{}", "[0]".repeat(d)),
        "deref" => format!("let v = 1;\nlet r = &v;\nlet x = {}r", "*".repeat(d)),
        "interp" => {
            let mut s = String::from("1");
            for _ in 0..d {
                s = format!("\"[{s}]\"");
            }
            format!("let x = {s}")
        }
        "tuplepat" => {
            let pat = format!("{}x{}", "(1, ".repeat(d), ")".repeat(d));
            format!("let v = (1, 1);\nlet x = select v {{ {pat} => 1, _ => 0 }}")
        }
        "select" => {
            let mut s = String::from("1");
            for _ in 0..d {
                s = format!("select 1 {{ 1 => {s}, _ => 0 }}");
            }
            format!("let x = {s}")
        }
        "qop" => format!("let a = [1];\nlet x = a[0]{}", "$".repeat(d)),
        "neg" => format!("let x = {}1", "-".repeat(d)),
        "not" => format!("let x = {}true", "!".repeat(d)),
        "modnest" => {
            let mut s = String::from("let x = 1");
            for i in 0..d {
                s = format!("mod m{i} {{ {s} }}");
            }
            s
        }
        _ => panic!("unknown shape {shape}"),
    }
}

const SHAPES: &[&str] = &[
    "parens",
    "array",
    "maplit",
    "slicepat",
    "tuple",
    "structlit",
    "variant",
    "typ",
    "uniontyp",
    "lambda",
    "call",
    "block",
    "field",
    "index",
    "deref",
    "interp",
    "tuplepat",
    "select",
    "qop",
    "neg",
    "not",
    "modnest",
];

/// The child half: compile one shape on a small-stack runtime. Reaching
/// the end of this function at all is the assertion — Ok or Err both
/// mean the pipeline stayed on its feet.
fn run_child(shape: &str, depth: usize) {
    let dir = env::temp_dir().join(format!("gx-deep-{shape}-{}", std::process::id()));
    fs::create_dir_all(&dir).expect("tmpdir");
    let file = dir.join("deep.gx");
    fs::write(&file, program(shape, depth)).expect("write");
    let rt = tokio::runtime::Builder::new_multi_thread()
        .worker_threads(2)
        .thread_stack_size(STACK)
        .enable_all()
        .build()
        .expect("runtime");
    let r = rt.block_on(async {
        ShellBuilder::<NoExt>::default()
            .module_resolvers(vec![FilesResolver::new(dir.clone(), None)])
            .mode(Mode::Check(Source::File(file.clone())))
            .build()
            .expect("building shell")
            .check()
            .await
    });
    let _ = fs::remove_dir_all(&dir);
    // Any other error is fine and still counts: a type error means the
    // AST was built, walked and torn down, which is the exercise. Only
    // the limit refusing means the deep path never ran.
    if r.is_err_and(|e| format!("{e:#}").contains("nesting too deep")) {
        std::process::exit(REFUSED)
    }
}

#[test]
fn deep_nesting_does_not_overflow() {
    if let Ok(shape) = env::var(SHAPE_VAR) {
        let depth = env::var(DEPTH_VAR).expect("depth").parse().expect("depth");
        return run_child(&shape, depth);
    }
    let exe = env::current_exe().expect("current exe");
    // Every case pays a full stdlib compile, so run them in batches
    // rather than one at a time — bounded because each child's compile
    // is the memory-hungry part.
    const CONCURRENCY: usize = 8;
    let spawn = |shape: &str, depth: usize| {
        Command::new(&exe)
            .args(["deep_nesting_does_not_overflow", "--exact", "--nocapture"])
            .env(SHAPE_VAR, shape)
            .env(DEPTH_VAR, depth.to_string())
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .spawn()
            .expect("spawn child")
    };
    let cases: Vec<(&str, usize)> = SHAPES
        .iter()
        .flat_map(|s| [(*s, accepted()), (*s, REJECTED)])
        .chain([("parens", REJECTED)])
        .collect();
    let mut codes: HashMap<(&str, usize), Option<i32>> = HashMap::new();
    for batch in cases.chunks(CONCURRENCY) {
        let running: Vec<_> = batch.iter().map(|(s, d)| (*s, *d, spawn(s, *d))).collect();
        for (s, d, mut child) in running {
            codes.insert((s, d), child.wait().expect("wait child").code());
        }
    }
    let run = |shape: &str, depth: usize| codes[&(shape, depth)];
    let mut failed: Vec<String> = vec![];
    for shape in SHAPES {
        // Deep but accepted: the parser must let it through, else the
        // walks and destructors under test never run.
        match run(shape, accepted()) {
            Some(0) => (),
            Some(REFUSED) => failed.push(format!(
                "{shape}@{}: refused by the nesting limit, so the deep path \
                 was never exercised",
                accepted()
            )),
            other => failed.push(format!("{shape}@{}: {other:?}", accepted())),
        }
        // Far past anything sane. Whether the limit refuses depends on
        // the shape — `uniontyp` at 100k is a FLAT union, not nesting —
        // so the assertion is only that the child came back at all.
        if run(shape, REJECTED).is_none() {
            failed.push(format!("{shape}@{REJECTED}: killed by a signal"))
        }
    }
    // ...and that the limit does fire, on a shape that genuinely nests.
    if run("parens", REJECTED) != Some(REFUSED) {
        failed.push(format!("parens@{REJECTED}: the nesting limit did not fire"))
    }
    assert!(
        failed.is_empty(),
        "{} case(s) on a {STACK}-byte stack (killed by a signal is what a \
         stack overflow looks like — it aborts, so the child dies rather \
         than returning an error): {failed:#?}",
        failed.len(),
    );
}
