//! Adversarially nested programs must not overflow the stack: the
//! guarded walks run on heap segments and the parser refuses an AST
//! deeper than `parser::max_nesting()`, so a deep program is a compile
//! error. What the parser admits is checked, run with fusion on and off,
//! and formatted.
//!
//! Each case runs in a child process on a small worker stack (an
//! overflow aborts, so it cannot be caught in-process). The child
//! invocation passes `--include-ignored`; without it the child would
//! skip the test and exit 0, which reads as success.

use graphix_compiler::{
    BitFlags, CFlag,
    expr::{
        FilesResolver, Load, Source,
        format::{FormatConfig, SourceKind, format_source},
    },
};
use graphix_package::MainThreadHandle;
use graphix_rt::NoExt;
use graphix_shell::{CacheMode, Mode, ShellBuilder};
use std::{
    collections::HashMap,
    env, fs,
    process::{Command, Stdio},
};

const STACK: usize = 512 * 1024;

/// Exit code the child uses for "the nesting limit refused this".
const REFUSED: i32 = 3;

/// Exit code the child uses for any other parse error.
const UNPARSED: i32 = 4;

/// Past `parser::max_nesting()`: every shape that nests in its source
/// must come back REFUSED.
const REJECTED: usize = 100_000;

/// Shapes whose source does not nest at `REJECTED`, which the parser
/// cannot refuse.
const FLAT_AT_REJECTED: &[&str] = &["uniontyp", "seqarm"];

/// The deepest nesting of `shape` the parser admits, found by bisection,
/// so each shape runs as deep as a program may be.
fn accepted(shape: &str) -> usize {
    use graphix_compiler::expr::{Origin, parser};
    let admitted = |d: usize| {
        let text = arcstr::ArcStr::from(program(shape, d));
        let origin = Origin { parent: None, source: Source::Unspecified, text };
        match parser::parse(origin) {
            Ok(_) => true,
            Err(e) => !format!("{e:#}").contains("nesting too deep"),
        }
    };
    if FLAT_AT_REJECTED.contains(&shape) {
        return parser::max_nesting();
    }
    // every level costs the parser at least one knot
    let (mut lo, mut hi) = (1, parser::max_nesting() + 1);
    assert!(admitted(lo) && !admitted(hi), "{shape} is not refused at {hi}");
    while hi - lo > 1 {
        let mid = lo + (hi - lo) / 2;
        if admitted(mid) { lo = mid } else { hi = mid }
    }
    lo
}

const SHAPE_VAR: &str = "GRAPHIX_DEEP_SHAPE";
const DEPTH_VAR: &str = "GRAPHIX_DEEP_DEPTH";
const MODE_VAR: &str = "GRAPHIX_DEEP_MODE";

/// What a child does with the program: check it, run it (fusion on or
/// off), or format it.
const MODES: [&str; 4] = ["check", "run", "nofusion", "fmt"];

/// Exit code the child uses for any other failure where the program must
/// build and run.
const FAILED: i32 = 5;

/// The longest operator or postfix run a paren-chain shape writes.
const RUN: usize = 999;

/// `(name, source)` — one per construct whose nesting recurses somewhere
/// in the pipeline. Add a case here when you add a recursive construct.
fn program(shape: &str, d: usize) -> String {
    match shape {
        "parens" => format!("let x = {}1{}", "(1 + ".repeat(d), ")".repeat(d)),
        // Bracket shapes are also netidx `Value` literals, so these
        // cover netidx-value's nesting guard too.
        "array" => format!("let x = {}1{}", "[".repeat(d), "]".repeat(d)),
        "maplit" => format!("let x = {}1{}", r#"{"k" => "#.repeat(d), "}".repeat(d)),
        "slicepat" => {
            let v = format!("{}1{}", "[".repeat(d), "]".repeat(d));
            let pat = format!("{}x{}", "[".repeat(d), "]".repeat(d));
            format!("let v = {v};\nlet x = select v {{ {pat} => 1, _ => 0 }}")
        }
        "tuple" => format!("let x = {}(1, 1){}", "(1, ".repeat(d), ")".repeat(d)),
        "structlit" => format!("let x = {}1{}", "{a: ".repeat(d), "}".repeat(d)),
        "variant" => format!("let x = {}1{}", "`A(".repeat(d), ")".repeat(d)),
        "typ" => {
            format!("let x: {}i64{} = never()", "Array<".repeat(d), ">".repeat(d))
        }
        "structtyp" => {
            format!("let x: {}i64{} = never()", "{a: ".repeat(d), "}".repeat(d))
        }
        "tupletyp" => {
            format!("let x: {}i64{} = never()", "(i64, ".repeat(d), ")".repeat(d))
        }
        "uniontyp" => format!("let x: [{}null] = null", "i64, ".repeat(d)),
        "fntyp" => {
            format!("let x: {}i64{} = never()", "fn(x: ".repeat(d), ") -> i64".repeat(d))
        }
        "fnret" => format!("let x: {}i64 = never()", "fn() -> ".repeat(d)),
        "lambda" => format!("let f = {}1", "|x| ".repeat(d)),
        "call" => {
            format!("let f = |x| x;\nlet y = {}1{}", "f(".repeat(d), ")".repeat(d))
        }
        "block" => format!("let x = {}1{}", "{ let a = 1; ".repeat(d), " }".repeat(d)),
        "field" => format!(
            "let s = {}1{};\nlet x = s{}",
            "{a: ".repeat(d),
            "}".repeat(d),
            ".a".repeat(d)
        ),
        "index" => format!(
            "let a = {}1{};\nlet x = a{}",
            "[".repeat(d),
            "]".repeat(d),
            "[0]$".repeat(d)
        ),
        "deref" => format!(
            "let v = 1;\nlet r = {}v{};\nlet x = {}r",
            "&(".repeat(d),
            ")".repeat(d),
            "*".repeat(d)
        ),
        "interp" => {
            let mut s = String::from("1");
            for _ in 0..d {
                s = format!("\"[{s}]\"");
            }
            format!("let x = {s}")
        }
        "tuplepat" => {
            let v = format!("{}1{}", "(1, ".repeat(d), ")".repeat(d));
            let pat = format!("{}x{}", "(1, ".repeat(d), ")".repeat(d));
            format!("let v = {v};\nlet x = select v {{ {pat} => 1, _ => 0 }}")
        }
        "select" => {
            let mut s = String::from("1");
            for _ in 0..d {
                s = format!("select 1 {{ 1 => {s}, _ => 0 }}");
            }
            format!("let x = {s}")
        }
        "qop" => format!("let x = {}1{}", "cast<i64>(".repeat(d), ")$".repeat(d)),
        // Runs inside parens: each fold is under the limit, the AST they
        // build together is `d` deep.
        "parenchain" => format!("let x = {}", paren_chain(d)),
        "lambdachain" => format!("let f = |a| {};\nlet x = f(1)", paren_chain(d)),
        "neg" => format!("let x = {}1", "-".repeat(d)),
        "not" => format!("let x = {}true", "!".repeat(d)),
        "modnest" => {
            let mut s = String::from("{ mod x; x::k }");
            for _ in 0..d {
                s = format!("{{ let a = 1; {s} }}");
            }
            format!("let v = {s}")
        }
        "seqarm" => {
            let body = std::iter::repeat("1").take(d).collect::<Vec<_>>().join("; ");
            format!("seq {{ {body} }}")
        }
        "seqblock" => format!("seq {{ {}1{} }}", "{ 1; ".repeat(d), " }".repeat(d)),
        "seqabort" => {
            let mut s = String::from("1");
            for _ in 0..d {
                s = format!("seq 1; abort({s}) {{ 1 }}");
            }
            format!("let x = {s}")
        }
        // Flat programs whose types are as deep as the program is long.
        "flattype" => {
            let mut s = String::from("let x0 = 1;\n");
            for i in 1..=d {
                s.push_str(&format!("let x{i} = [x{}];\n", i - 1));
            }
            s.push_str(&format!("x{d} == x{d}"));
            s
        }
        // A chain of bounds as long as the program, every cell a position
        // of one signature: the call's settle orders them all.
        "chainbounds" => {
            let bounds = (0..d)
                .map(|i| format!("'a{i}: Array<'a{}>", i + 1))
                .chain([format!("'a{d}: Number")])
                .collect::<Vec<_>>()
                .join(", ");
            let cells = (0..=d).map(|i| format!("'a{i}")).collect::<Vec<_>>().join(", ");
            format!("let f = {bounds} |x: ({cells})| x;\nf(never())")
        }
        "flatcast" => {
            let mut s = String::from("type A0 = [`Z];\n");
            for i in 1..=d {
                s.push_str(&format!("type A{i} = [`N(A{}), `Z];\n", i - 1));
            }
            s.push_str(&format!("let x = cast<A{d}>(`Z)$;\nx == x"));
            s
        }
        // A static call chain as long as the program: its kernels'
        // records nest, and so do their frames.
        "fnchain" => {
            let mut s = String::from("let f0 = |x: i64| x;\n");
            for i in 1..=d {
                s.push_str(&format!("let f{i} = |x: i64| f{}(x) + 1;\n", i - 1));
            }
            s.push_str(&format!("let x = f{d}(1)"));
            s
        }
        _ => panic!("unknown shape {shape}"),
    }
}

/// `d` levels of `+ 1` runs, `RUN` to a paren level.
fn paren_chain(d: usize) -> String {
    let levels = d.div_ceil(RUN).max(1);
    let run = d.min(RUN);
    let mut s = String::from("1");
    for _ in 0..levels {
        s = format!("({s}{})", " + 1".repeat(run));
    }
    s
}

/// Shapes with no source nesting at all: the parser cannot refuse
/// them, so each must compile at `FLAT_DEPTH`.
const FLAT_SHAPES: &[&str] = &["flattype", "flatcast", "fnchain"];
const FLAT_DEPTH: usize = 3000;

/// Cells chained by bounds in one signature (`chainbounds`).
const CHAIN_DEPTH: usize = 10000;

/// A chain of module files, each its own parse, so the parser's limit
/// bounds none of it: module resolution recurses once per file.
const MODCHAIN_DEPTH: usize = 1000;

// CR claude for claude: [test-gap/low] These shapes miss the recursive constructs added
// since this list was last grown: List literals and patterns, variant, struct and
// or-patterns, &mut chains, nested catch, try/with and {s with ..}. CLAUDE.md asks for
// a case per construct. Each has its own parser knot and walks. Add them here
// (accepted() finds their depths: 331 for the pattern and list shapes, 248 for &mut).
// By hand they pass check, run, --no-fusion, fmt and the LSP queries at a 256K stack,
// so this is a missing pin, not a bug. Found by reading; shapes probed with the debug
// binary. (tests-shell-compiler-04)
const SHAPES: &[&str] = &[
    "parens",
    "array",
    "maplit",
    "slicepat",
    "tuple",
    "structlit",
    "variant",
    "typ",
    "structtyp",
    "tupletyp",
    "uniontyp",
    "fntyp",
    "fnret",
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
    "parenchain",
    "lambdachain",
    "neg",
    "not",
    "modnest",
    "seqarm",
    "seqblock",
    "seqabort",
];

/// What a child may map: a runaway case dies alone, by a signal.
const CHILD_MEMORY: u64 = 4 << 30;

/// The child half: check, run or format one shape on a small-stack
/// runtime. Returning at all is the assertion.
fn run_child(shape: &str, depth: usize, mode: &str) {
    #[cfg(unix)]
    unsafe {
        let limit = libc::rlimit { rlim_cur: CHILD_MEMORY, rlim_max: CHILD_MEMORY };
        libc::setrlimit(libc::RLIMIT_AS, &limit);
    }
    let dir = env::temp_dir().join(format!("gx-deep-{shape}-{}", std::process::id()));
    fs::create_dir_all(&dir).expect("tmpdir");
    let file = dir.join("deep.gx");
    match shape {
        "modchain" => {
            let link = "mod m;\nlet x = m::x";
            fs::write(&file, link).expect("write");
            let mut at = dir.clone();
            for i in 0..depth {
                let text = if i + 1 == depth { "let x = 1" } else { link };
                fs::write(at.join("m.gx"), text).expect("write");
                at = at.join("m");
                fs::create_dir_all(&at).expect("mkdir");
            }
        }
        "modnest" => {
            fs::write(&file, program(shape, depth)).expect("write");
            fs::write(dir.join("x.gx"), "let k = 1").expect("write");
        }
        _ => fs::write(&file, program(shape, depth)).expect("write"),
    }
    let text = fs::read_to_string(&file).expect("read");
    let refused = |e: &str| {
        if e.contains("nesting too deep") {
            std::process::exit(REFUSED)
        }
        if e.contains("arse error") {
            std::process::exit(UNPARSED)
        }
    };
    if mode == "fmt" {
        let r = std::thread::Builder::new()
            .stack_size(STACK)
            .spawn(move || {
                format_source(SourceKind::Program, &text, &FormatConfig::default())
                    .map(|_| ())
                    .map_err(|e| format!("{e:#}"))
            })
            .expect("spawn")
            .join()
            .expect("join");
        let _ = fs::remove_dir_all(&dir);
        if let Err(e) = r {
            if depth == REJECTED {
                refused(&e)
            }
            eprintln!("{shape}@{depth} fmt: {e}");
            std::process::exit(FAILED)
        }
        return;
    }
    if mode != "check" {
        fs::write(&file, format!("{text};\nsys::exit(0)")).expect("write");
    }
    let rt = tokio::runtime::Builder::new_multi_thread()
        .worker_threads(2)
        .thread_stack_size(STACK)
        .enable_all()
        .build()
        .expect("runtime");
    let r = rt.block_on(async {
        let shell = |cache| {
            ShellBuilder::<NoExt>::default()
                .cache(cache)
                .module_resolvers(vec![FilesResolver::new(dir.clone(), None)])
                .disable_flags(match mode {
                    "nofusion" => CFlag::FusionDisabled.into(),
                    _ => BitFlags::empty(),
                })
                .mode(match mode {
                    "check" => Mode::Check(Load::File(file.clone())),
                    _ => Mode::Script(Load::File(file.clone())),
                })
                .build()
                .expect("building shell")
        };
        match mode {
            "check" => shell(CacheMode::Off).check().await,
            // cold, writing the images under the parent's cache, then warm
            "image" => {
                for _ in 0..2 {
                    shell(CacheMode::On).run(MainThreadHandle::new().0).await?;
                }
                Ok(())
            }
            _ => shell(CacheMode::Off).run(MainThreadHandle::new().0).await.map(|_| ()),
        }
    });
    let _ = fs::remove_dir_all(&dir);
    if let Err(e) = r {
        let e = format!("{e:#}");
        if depth == REJECTED {
            refused(&e)
        }
        // short of the limit the program must build and run
        eprintln!("{shape}@{depth} {mode}: {e}");
        std::process::exit(FAILED)
    }
}

/// `graphix --check` of `program` finishes, accepted, within `budget`;
/// a check still running then is killed.
fn checks_within(program: &str, budget: std::time::Duration) {
    let dir = tempfile::tempdir().expect("a dir for the program");
    let file = dir.path().join("program.gx");
    fs::write(&file, program).expect("write the program");
    let start = std::time::Instant::now();
    let mut child = Command::new(env!("CARGO_BIN_EXE_graphix"))
        .args(["--check", "--no-cache"])
        .arg(&file)
        .env("XDG_CACHE_HOME", dir.path())
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .spawn()
        .expect("run the check");
    let status = loop {
        if let Some(status) = child.try_wait().expect("wait for the check") {
            break status;
        }
        if start.elapsed() > budget {
            let _ = child.kill();
            panic!("the check ran past {budget:?}");
        }
        std::thread::sleep(std::time::Duration::from_millis(20));
    };
    assert!(status.success(), "the check failed: {status}");
}

/// A type as deep as the program is long checks in time linear in the
/// program: `flattype` at eight times `FLAT_DEPTH` checks well inside a
/// budget a walk of each let's whole type blows several times over.
#[test]
fn flat_type_chain_checks_in_linear_time() {
    checks_within(
        &program("flattype", 8 * FLAT_DEPTH),
        std::time::Duration::from_secs(15),
    )
}

/// A value whose type shares each level twice (`(x, x)`), written to a
/// binding of an equal type and checked against a typedef built the same
/// way, checks in time linear in its depth: a walk of it as a tree takes
/// 2^depth steps.
#[test]
fn shared_type_dags_check_in_linear_time() {
    const DEPTH: usize = 60;
    let chain = |name: &str| {
        let mut s = format!("let {name}0 = 1;\n");
        for i in 1..=DEPTH {
            s.push_str(&format!("let {name}{i} = ({name}{}, {name}{});\n", i - 1, i - 1));
        }
        s
    };
    let mut written = chain("x") + &chain("z");
    written.push_str(&format!("let y = x{DEPTH};\ny <- z{DEPTH};\ny"));
    let mut typedef = String::from("type T0 = i64;\n");
    for i in 1..=DEPTH {
        typedef.push_str(&format!("type T{i} = (T{}, T{});\n", i - 1, i - 1));
    }
    typedef.push_str(&chain("x"));
    typedef.push_str(&format!("let y: T{DEPTH} = x{DEPTH};\ny"));
    for program in [written, typedef] {
        checks_within(&program, std::time::Duration::from_secs(15))
    }
}

#[test]
#[cfg_attr(not(feature = "slow-tests"), ignore = "slow-tests")]
fn deep_nesting_does_not_overflow() {
    if let Ok(shape) = env::var(SHAPE_VAR) {
        let depth = env::var(DEPTH_VAR).expect("depth").parse().expect("depth");
        let mode = env::var(MODE_VAR).expect("mode");
        return run_child(&shape, depth, &mode);
    }
    let exe = env::current_exe().expect("current exe");
    let cache = tempfile::tempdir().expect("a cache for the image cases");
    // Batched: each child pays a full stdlib compile.
    const CONCURRENCY: usize = 8;
    let spawn = |shape: &str, depth: usize, mode: &str| {
        Command::new(&exe)
            .args([
                "deep_nesting_does_not_overflow",
                "--exact",
                "--nocapture",
                "--include-ignored",
            ])
            .env(SHAPE_VAR, shape)
            .env(DEPTH_VAR, depth.to_string())
            .env(MODE_VAR, mode)
            .env("XDG_CACHE_HOME", cache.path())
            .stdin(Stdio::null())
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .spawn()
            .expect("spawn child")
    };
    let cases: Vec<(&str, usize, &str)> = SHAPES
        .iter()
        .flat_map(|s| {
            let depth = accepted(s);
            MODES.map(|m| (*s, depth, m))
        })
        .chain(SHAPES.iter().map(|s| (*s, REJECTED, "check")))
        .chain(FLAT_SHAPES.iter().flat_map(|s| {
            MODES.into_iter().chain(["image"]).map(|m| (*s, FLAT_DEPTH, m))
        }))
        .chain([("modchain", MODCHAIN_DEPTH, "check")])
        .chain([("chainbounds", CHAIN_DEPTH, "check")])
        .collect();
    let mut codes: HashMap<(&str, usize, &str), Option<i32>> = HashMap::new();
    for batch in cases.chunks(CONCURRENCY) {
        let running: Vec<_> =
            batch.iter().map(|&(s, d, m)| ((s, d, m), spawn(s, d, m))).collect();
        for (case, mut child) in running {
            codes.insert(case, child.wait().expect("wait child").code());
        }
    }
    let mut failed: Vec<String> = vec![];
    for (&(shape, depth, mode), code) in &codes {
        let want = match depth {
            REJECTED if FLAT_AT_REJECTED.contains(&shape) => continue,
            REJECTED => Some(REFUSED),
            _ => Some(0),
        };
        if *code != want {
            failed.push(format!("{shape}@{depth} {mode}: {code:?}, not {want:?}"))
        }
    }
    for shape in FLAT_AT_REJECTED {
        if codes[&(*shape, REJECTED, "check")].is_none() {
            failed.push(format!("{shape}@{REJECTED}: killed by a signal"))
        }
    }
    assert!(
        failed.is_empty(),
        "{} case(s) on a {STACK}-byte stack (killed by a signal is what a \
         stack overflow looks like — it aborts, so the child dies rather \
         than returning an error): {failed:#?}",
        failed.len(),
    );
}
