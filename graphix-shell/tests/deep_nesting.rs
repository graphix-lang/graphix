//! Adversarially nested programs must not overflow the stack: the
//! guarded walks run on heap segments and the parser refuses an AST
//! deeper than `parser::max_nesting()`, so a deep program is a compile
//! error. What the parser admits is checked, run with fusion on and off,
//! run restored from an image, run as a called lambda's body (which fuses)
//! and formatted.
//!
//! Each case runs in a child process on a small worker stack (an
//! overflow aborts, so it cannot be caught in-process). The child
//! invocation passes `--include-ignored`; without it the child would
//! skip the test and exit 0, which reads as success.

use graphix_compiler::{
    BitFlags, CFlag,
    expr::{
        Load, Source,
        format::{FormatConfig, SourceKind, format_source},
    },
};
use graphix_package::MainThreadHandle;
use graphix_rt::NoExt;
use graphix_shell::{CacheMode, Mode, ShellBuilder};
use poolshark::local::LPooled;
use std::{
    collections::{BTreeMap, HashMap},
    env,
    fmt::Write,
    fs,
    path::{Path, PathBuf},
    process::{Command, Stdio},
    time::SystemTime,
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

/// The deepest nesting of `shape` the parser admits in `mode`, found by
/// bisection, so each shape runs as deep as a program may be.
fn accepted(shape: &str, mode: &str) -> usize {
    use graphix_compiler::expr::{Origin, parser};
    let admitted = |d: usize| {
        let text = arcstr::ArcStr::from(source(shape, d, mode));
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
/// off), format it, compile it to an image then run it restored from the
/// image, or run it as the body of a called lambda, which fuses.
const MODES: [&str; 6] = ["check", "run", "nofusion", "fmt", "image", "twin"];

/// The program a child of `mode` compiles: a twin's typedefs stay at the
/// top level.
fn source(shape: &str, d: usize, mode: &str) -> String {
    let program = program(shape, d);
    match mode {
        "twin" => {
            let (types, body): (Vec<&str>, Vec<&str>) =
                program.lines().partition(|l| l.starts_with("type "));
            let types: String = types.iter().map(|t| format!("{t}\n")).collect();
            format!("{types}let twin = |z| {{\n{};\nz\n}};\ntwin(1)", body.join("\n"))
        }
        _ => program,
    }
}

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
        "list" => format!("let x = {}1{}", "[<".repeat(d), ">]".repeat(d)),
        "listpat" => {
            let v = format!("{}1{}", "[<".repeat(d), ">]".repeat(d));
            let pat = format!("{}x{}", "[<".repeat(d), ">]".repeat(d));
            format!("let v = {v};\nlet x = select v {{ {pat} => 1, _ => 0 }}")
        }
        "variantpat" => {
            let v = format!("{}1{}", "`A(".repeat(d), ")".repeat(d));
            let pat = format!("{}x{}", "`A(".repeat(d), ")".repeat(d));
            format!("let v = {v};\nlet x = select v {{ {pat} => x }}")
        }
        "structpat" => {
            let v = format!("{}1{}", "{a: ".repeat(d), "}".repeat(d));
            let pat = format!("{}x{}", "{a: ".repeat(d), "}".repeat(d));
            format!("let v = {v};\nlet x = select v {{ {pat} => x }}")
        }
        "orpat" => {
            let v = format!("{}1{}", "(1, ".repeat(d), ")".repeat(d));
            let pat = format!("{}x{}", "(1 | 2, ".repeat(d), ")".repeat(d));
            format!("let v = {v};\nlet x = select v {{ {pat} => x, _ => 0 }}")
        }
        "refmut" => {
            format!("let v = 1;\nlet r = {}v{}", "&mut (".repeat(d), ")".repeat(d))
        }
        "catch" => format!("let x = {}1{}", "{ catch(e) e; ".repeat(d), " }".repeat(d)),
        "trywith" => format!(
            "seq {{ {}1; 1{} }}",
            "try { 1; ".repeat(d),
            " } with(e) { 0 }".repeat(d)
        ),
        "live" => format!(
            "let x = seq {{ until {}true{}; 1 }}",
            "live(".repeat(d),
            ")".repeat(d)
        ),
        "never" => format!("let x: i64 = {}1{}", "never<i64>(".repeat(d), ")".repeat(d)),
        "any" => format!("let x = {}1{}", "any(".repeat(d), ")".repeat(d)),
        "abstract" => format!(
            "type T = Abstract<[`Z, T]>;\nlet x = {}`Z{}",
            "T(".repeat(d),
            ")".repeat(d)
        ),
        "tupleidx" => {
            format!(
                "let t = {}1{};\nlet x = t{}",
                "(1, ".repeat(d),
                ")".repeat(d),
                ".1".repeat(d)
            )
        }
        "mapidx" => format!(
            "let m = {}1{};\nlet x = m{}",
            r#"{"k" => "#.repeat(d),
            "}".repeat(d),
            r#"{"k"}$"#.repeat(d)
        ),
        "slice" => format!("let a = [1, 2];\nlet x = a{}", "[0..]$".repeat(d)),
        "qmark" => format!("let x = {}1{}", "cast<i64>(".repeat(d), ")?".repeat(d)),
        "structwith" => format!(
            "let s = {{a: 1}};\nlet x = {}s{}",
            "{(".repeat(d),
            ") with a: 1}".repeat(d)
        ),
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
        // a static call chain: each kernel calls the one before, so the
        // link installs (and an image writes) a record tree this deep
        "calleechain" => {
            let mut s = String::from("let f0 = |x| x;\n");
            for i in 1..d {
                s.push_str(&format!("let f{i} = |x| f{}(x);\n", i - 1));
            }
            s.push_str(&format!("let g = |x: i64| f{}(x) + 1;\ng(1)", d - 1));
            s
        }
        "flatcast" => {
            let mut s = String::from("type A0 = [`Z];\n");
            for i in 1..=d {
                s.push_str(&format!("type A{i} = [`N(A{}), `Z];\n", i - 1));
            }
            s.push_str(&format!("let x = cast<A{d}>(`Z)$;\nx == x"));
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
const FLAT_SHAPES: &[&str] = &["flattype", "flatcast", "calleechain"];
const FLAT_DEPTH: usize = 3000;

/// Cells chained by bounds in one signature (`chainbounds`).
const CHAIN_DEPTH: usize = 10000;

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
    "list",
    "listpat",
    "variantpat",
    "structpat",
    "orpat",
    "refmut",
    "catch",
    "trywith",
    "structwith",
    "live",
    "never",
    "any",
    "abstract",
    "tupleidx",
    "mapidx",
    "slice",
    "qmark",
];

/// What a child may map: a runaway case dies alone, by a signal. A runtime
/// maps about 3 GB, the image mode's two in one process more.
const CHILD_MEMORY: u64 = 8 << 30;

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
            fs::write(&file, source(shape, depth, mode)).expect("write");
            fs::write(dir.join("x.gx"), "let k = 1").expect("write");
        }
        _ => fs::write(&file, source(shape, depth, mode)).expect("write"),
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
                .enable_flags(match mode {
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
            // compiled and written, then restored: a restored program
            // writes nothing
            "image" => {
                shell(CacheMode::Warm).run(MainThreadHandle::new().0).await?;
                let written = cache_entries();
                shell(CacheMode::On).run(MainThreadHandle::new().0).await?;
                if cache_entries() != written {
                    anyhow::bail!("the second run did not restore the program image")
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

/// Every file under this process's image cache, with its creation time:
/// a load touches an entry's modification time, and a store replaces the
/// file.
fn cache_entries() -> BTreeMap<PathBuf, SystemTime> {
    fn walk(dir: &Path, out: &mut BTreeMap<PathBuf, SystemTime>) {
        for e in fs::read_dir(dir).into_iter().flatten().flatten() {
            let (path, meta) = (e.path(), e.metadata().expect("cache entry metadata"));
            if meta.is_dir() {
                walk(&path, out)
            } else {
                out.insert(path, meta.created().expect("cache entry birth time"));
            }
        }
    }
    let mut out = BTreeMap::new();
    walk(&dirs::cache_dir().expect("a cache dir").join("graphix"), &mut out);
    out
}

/// `graphix --check` of `program` finishes, accepted, within `budget`;
/// a check still running then is killed.
fn checks_within(program: &str, budget: std::time::Duration) {
    completes_within(&["--check", "--no-cache"], program, budget)
}

fn completes_within(args: &[&str], program: &str, budget: std::time::Duration) {
    let out = exits_within(args, &[("program.gx", program)], budget);
    assert!(out.status.success(), "{args:?} failed: {}", out.status);
}

/// `graphix` with `args` over `files` (the first is the program, the
/// others its modules) exits within `budget`, with its stderr; one still
/// running then is killed.
fn exits_within(
    args: &[&str],
    files: &[(&str, &str)],
    budget: std::time::Duration,
) -> std::process::Output {
    let dir = tempfile::tempdir().expect("a dir for the program");
    for (name, text) in files {
        fs::write(dir.path().join(name), text).expect("write the program");
    }
    let start = std::time::Instant::now();
    let mut child = Command::new(env!("CARGO_BIN_EXE_graphix"))
        .args(args)
        .arg(dir.path().join(files[0].0))
        .env("XDG_CACHE_HOME", dir.path())
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::piped())
        .spawn()
        .expect("run the check");
    loop {
        if child.try_wait().expect("wait for the check").is_some() {
            break child.wait_with_output().expect("the check's output");
        }
        if start.elapsed() > budget {
            let _ = child.kill();
            panic!("{args:?} ran past {budget:?}");
        }
        std::thread::sleep(std::time::Duration::from_millis(20));
    }
}

/// A type as deep as the program is long checks in time linear in the
/// program: `flattype` at eight times `FLAT_DEPTH` checks well inside a
/// budget a walk of each let's whole type blows several times over.
/// Nested `try` blocks, each a lexical scope, check in time quadratic
/// in their depth: a name's lookup consults its module's root once per
/// scope chain, not once per level.
#[test]
fn nested_trys_check_in_time() {
    checks_within(&program("trywith", 993), std::time::Duration::from_secs(30))
}

#[test]
fn flat_type_chain_checks_in_linear_time() {
    checks_within(
        &program("flattype", 8 * FLAT_DEPTH),
        std::time::Duration::from_secs(15),
    )
}

/// A let chain that wraps the last value in any constructor, or in an
/// array literal holding it twice, checks in time linear in its length:
/// a let's single name binds without an occurs walk of the value, and a
/// literal's element of an earlier element's very type adds nothing.
#[test]
fn wrapped_let_chains_check_in_linear_time() {
    const LETS: usize = 20_000;
    for wrap in ["{a: X}", "(X, 1)", "`A(X)", "[X, X]", "&X"] {
        let mut s = String::from("let x0 = 1;\n");
        for i in 1..=LETS {
            s.push_str(&format!(
                "let x{i} = {};\n",
                wrap.replace('X', &format!("x{}", i - 1))
            ));
        }
        s.push_str("1");
        checks_within(&s, std::time::Duration::from_secs(15))
    }
}

#[test]
fn a_long_seq_of_calls_checks_in_time() {
    const STEPS: usize = 4000;
    let body = vec!["a <- str::len(\"x\")"; STEPS].join("; ");
    checks_within(
        &format!("let a = 0;\nlet r = seq {{ {body}; a }};\nr"),
        std::time::Duration::from_secs(15),
    )
}

/// A chain of typedefs each naming the next, written before it is
/// defined, checks in time near linear in its length: a typedef with no
/// parameters rechecks nothing pending.
#[test]
fn forward_typedef_chain_checks_in_time() {
    const TYPES: usize = 1000;
    let mut s = String::new();
    for i in 0..TYPES {
        s.push_str(&format!("type T{i} = [`A{i}(i64), T{}];\n", i + 1));
    }
    s.push_str(&format!("type T{TYPES} = `End;\n1"));
    checks_within(&s, std::time::Duration::from_secs(15))
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

/// Comparing, ordering, keying and selecting over a value whose type is a
/// pair DAG checks in time linear in its depth: the Discernible and Ordered
/// judgments, the bounds they hand its cells and an arm's match walk each
/// shared node once.
#[test]
fn comparisons_over_a_shared_type_dag_check_in_linear_time() {
    const DEPTH: usize = 60;
    let mut chain = String::from("let x0 = 1;\n");
    for i in 1..=DEPTH {
        chain.push_str(&format!("let x{i} = (x{}, x{});\n", i - 1, i - 1));
    }
    let x = format!("x{DEPTH}");
    for use_ in [
        format!("{x} == {x}"),
        format!("{x} < {x}"),
        format!("uniq({x})"),
        format!("{{{x} => 1}}"),
        format!("select {x} {{ v => 1 }}"),
    ] {
        checks_within(&format!("{chain}{use_}"), std::time::Duration::from_secs(15))
    }
}

/// The defaults a written type hides are judged in time linear in the
/// types: a value typed by a recursive typedef holding no function with a
/// default is not expanded, and a DAG of 4-tuples checked against a
/// typedef built the same way is walked once per part.
#[test]
fn hidden_defaults_judge_in_linear_time() {
    const DEPTH: usize = 60;
    let tree = "type T = [`Leaf(fn(x: i64) -> i64), `Node(T, T, T, T, T, T, T, T)];\n\
                let f = |t: T| { let u: T = t; u };\nf";
    let mut typedef = String::from("let x0 = |#d = 1, x: i64| x + d;\n");
    for i in 1..=DEPTH {
        let x = format!("x{}", i - 1);
        typedef.push_str(&format!("let x{i} = ({x}, {x}, {x}, {x});\n"));
    }
    typedef.push_str("type T0 = fn(x: i64) -> i64;\n");
    for i in 1..=DEPTH {
        let t = format!("T{}", i - 1);
        typedef.push_str(&format!("type T{i} = ({t}, {t}, {t}, {t});\n"));
    }
    typedef.push_str(&format!("let y: T{DEPTH} = x{DEPTH};\ny"));
    for program in [tree.to_string(), typedef] {
        checks_within(&program, std::time::Duration::from_secs(15))
    }
}

// An instance's types are its definition's, taken row by row: a shared
// node is taken once, so a call of a definition building a DAG of pairs
// elaborates in time linear in the DAG.
#[test]
fn a_call_over_a_shared_type_dag_elaborates_in_linear_time() {
    const DEPTH: usize = 60;
    let mut program = String::from("let f = |a| {\n  let x0 = a;\n");
    for i in 1..=DEPTH {
        program.push_str(&format!("  let x{i} = (x{}, x{});\n", i - 1, i - 1));
    }
    program.push_str(&format!("  x{DEPTH}\n}};\nlet r = f(1);\nsys::exit(0)\n"));
    completes_within(&["--no-cache"], &program, std::time::Duration::from_secs(15))
}

// A search over a shared type walks each shared node once: the definition
// called at two types, whose kernels' types are DAGs, builds in time
// linear in the DAG.
#[test]
fn a_generic_call_over_a_shared_type_dag_builds_in_linear_time() {
    const DEPTH: usize = 60;
    let mut program = String::from("let f = |a| {\n  let x0 = a;\n");
    for i in 1..=DEPTH {
        program.push_str(&format!("  let x{i} = (x{}, x{});\n", i - 1, i - 1));
    }
    program.push_str(&format!(
        "  x{DEPTH}\n}};\nlet r = f(1);\nlet q = f(\"s\");\nsys::exit(0)\n"
    ));
    completes_within(&["--no-cache"], &program, std::time::Duration::from_secs(15))
}

/// A pair DAG named through a typedef builds in time linear in its depth:
/// a call's argument checked against its formal compares and welds two
/// equal DAGs, in different allocations, once per pair of nodes.
#[test]
fn a_typedef_named_type_dag_builds_in_linear_time() {
    const DEPTH: usize = 60;
    let mut program = String::from(
        "type R<'a, 'b> = ('a, 'b);\nlet mk = |x: 'a| -> R<'a, 'a> (x, x);\nlet f = |a| {\n  let x0 = a;\n",
    );
    for i in 1..=DEPTH {
        program.push_str(&format!("  let x{i} = mk(x{});\n", i - 1));
    }
    program.push_str(&format!("  x{DEPTH}\n}};\nlet r = f(1);\nsys::exit(0)\n"));
    completes_within(&["--no-cache"], &program, std::time::Duration::from_secs(15))
}

/// A pair DAG built through a generic constructor, and one reached
/// through generic callers, a monomorphic one and a collection's
/// callback, builds in time linear in its depth: the position and
/// pre-unification walks of a signature take a shared node once.
#[test]
fn shared_type_dags_through_generic_calls_build_in_linear_time() {
    const DEPTH: usize = 60;
    let body = |pair: &dyn Fn(usize) -> String| {
        let mut s = String::from("let f = |a| {\n  let x0 = a;\n");
        for i in 1..=DEPTH {
            s.push_str(&format!("  let x{i} = {};\n", pair(i - 1)));
        }
        s.push_str(&format!("  x{DEPTH}\n}};\n"));
        s
    };
    let constructor = format!(
        "let mk = |x: 'a| -> ('a, 'a) (x, x);\n{}let r = f(1);\nsys::exit(0)\n",
        body(&|i| format!("mk(x{i})"))
    );
    let callers = format!(
        "{}let g = |b| f(b);\nlet h = |c| g(c);\nlet r = h(1);\nlet q = h(\"s\");\n\
         let m = |b: i64| f(b);\nlet n = m(1);\nlet a = array::map([1, 2], |b| f(b));\n\
         sys::exit(0)\n",
        body(&|i| format!("(x{i}, x{i})"))
    );
    for program in [constructor, callers] {
        completes_within(&["--no-cache"], &program, std::time::Duration::from_secs(15))
    }
}

/// A module's refusal for deciding a cell the code around it left open
/// names the binding in time linear in the types in scope, a pair DAG's
/// included.
#[test]
fn a_foreign_write_refusal_beside_a_shared_type_dag_is_linear() {
    const DEPTH: usize = 60;
    let mut main = String::from("let a = never();\nlet x0 = 1;\n");
    for i in 1..=DEPTH {
        main.push_str(&format!("let x{i} = (x{}, x{});\n", i - 1, i - 1));
    }
    main.push_str(&format!("mod m;\n(m::y, x{DEPTH})\n"));
    let out = exits_within(
        &["--check", "--no-cache"],
        &[
            ("main.gx", &main),
            ("m.gxi", "val y: i64;\n"),
            ("m.gx", "let y = { super::a <- 1; 1 };\n"),
        ],
        std::time::Duration::from_secs(15),
    );
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(
        !out.status.success() && stderr.contains("annotate `a`"),
        "the module's write to `a` is refused: {stderr}"
    );
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
            .env("XDG_CACHE_HOME", cache.path().join(format!("{shape}-{depth}-{mode}")))
            .stdin(Stdio::null())
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .spawn()
            .expect("spawn child")
    };
    let cases: Vec<(&str, usize, &str)> = SHAPES
        .iter()
        .flat_map(|s| MODES.map(|m| (*s, accepted(s, m), m)))
        .chain(SHAPES.iter().map(|s| (*s, REJECTED, "check")))
        .chain(FLAT_SHAPES.iter().flat_map(|s| MODES.map(|m| (*s, FLAT_DEPTH, m))))
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

#[test]
fn select_discriminants_first_value_in_time() {
    let mut source: LPooled<String> = LPooled::take();
    let mut union: LPooled<String> = LPooled::take();
    union.push('[');
    for i in 0..100 {
        writeln!(source, "type C{i} = Abstract<i64>;").unwrap();
        if i != 0 {
            union.push_str(", ");
        }
        write!(union, "C{i}").unwrap();
    }
    union.push(']');
    writeln!(source, "let f = |x: {union}| select x {{").unwrap();
    for i in 0..100 {
        writeln!(source, "C{i} as c => c.0 + {i},").unwrap();
    }
    writeln!(source, "}}; let xs: Array<{union}> = array::init(2000, |i| C99(i));")
        .unwrap();
    source.push_str(
        "let r = array::fold(array::map(xs, |x| f(x)), 0, |a, b| a + b);\n\
        sys::exit(select r { 2197000 => 0, _ => 1 }); r",
    );
    completes_within(
        &["--no-cache", "--no-fusion"],
        &source,
        std::time::Duration::from_secs(4),
    );
}
