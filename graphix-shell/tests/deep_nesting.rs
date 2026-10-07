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
        FilesResolver, Source,
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

/// Deepest nesting the limit admits for every shape; derived from the
/// limit so the deep path stays exercised if the limit moves.
fn accepted() -> usize {
    graphix_compiler::expr::parser::max_nesting() / 8
}

const SHAPE_VAR: &str = "GRAPHIX_DEEP_SHAPE";
const DEPTH_VAR: &str = "GRAPHIX_DEEP_DEPTH";
const MODE_VAR: &str = "GRAPHIX_DEEP_MODE";

/// What a child does with the program: check it, run it (fusion on or
/// off), or format it.
const MODES: [&str; 4] = ["check", "run", "nofusion", "fmt"];

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
            s.push_str(&format!("x{d}"));
            s
        }
        "flatcast" => {
            let mut s = String::from("type A0 = [`Z];\n");
            for i in 1..=d {
                s.push_str(&format!("type A{i} = [`N(A{}), `Z];\n", i - 1));
            }
            s.push_str(&format!("let x = cast<A{d}>(`Z)"));
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
const FLAT_SHAPES: &[&str] = &["flattype", "flatcast"];
const FLAT_DEPTH: usize = 3000;

/// A chain of module files, each its own parse, so the parser's limit
/// bounds none of it: module resolution recurses once per file.
const MODCHAIN_DEPTH: usize = 1000;

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
    "parenchain",
    "lambdachain",
    "neg",
    "not",
    "modnest",
    "seqarm",
    "seqblock",
    "seqabort",
];

/// The child half: check, run or format one shape on a small-stack
/// runtime. Returning at all is the assertion.
fn run_child(shape: &str, depth: usize, mode: &str) {
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
            refused(&e)
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
        let shell = ShellBuilder::<NoExt>::default()
            .cache(CacheMode::Off)
            .module_resolvers(vec![FilesResolver::new(dir.clone(), None)])
            .disable_flags(match mode {
                "nofusion" => CFlag::FusionDisabled.into(),
                _ => BitFlags::empty(),
            })
            .mode(match mode {
                "check" => Mode::Check(Source::File(file.clone())),
                _ => Mode::Script(Source::File(file.clone())),
            })
            .build()
            .expect("building shell");
        match mode {
            "check" => shell.check().await,
            _ => shell.run(MainThreadHandle::new().0).await.map(|_| ()),
        }
    });
    let _ = fs::remove_dir_all(&dir);
    // Any other error means the AST was built, walked and torn down.
    // Only a parse error means the deep path never ran.
    if let Err(e) = r {
        refused(&format!("{e:#}"))
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
            .stdin(Stdio::null())
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .spawn()
            .expect("spawn child")
    };
    let cases: Vec<(&str, usize, &str)> = SHAPES
        .iter()
        .flat_map(|s| MODES.map(|m| (*s, accepted(), m)))
        .chain(SHAPES.iter().map(|s| (*s, REJECTED, "check")))
        .chain(FLAT_SHAPES.iter().map(|s| (*s, FLAT_DEPTH, "check")))
        .chain([("modchain", MODCHAIN_DEPTH, "check")])
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
