//! Adversarially nested programs must not overflow the stack: the
//! guarded walks run on heap segments and the parser refuses an AST
//! deeper than `parser::max_nesting()`, so a deep program is a compile
//! error. What the parser admits is checked, run with fusion on and off,
//! run restored from an image, run as a called lambda's body (which fuses)
//! and formatted.
//!
//! Each case runs in a child process on a small stack (an overflow
//! aborts, so it cannot be caught in-process). The child
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
    thread,
    time::SystemTime,
};

mod common;
#[cfg(target_os = "linux")]
use {
    common::Instructions,
    std::{
        process::Output,
        time::{Duration, Instant},
    },
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
        let origin = Origin::new(None, Source::Unspecified, text);
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
            // CR claude for claude: [test-gap/medium] The twin mode does not fuse the
            // nest it is meant to cover. `source` wraps the program as `|z| {
            // <program>; z }`, and every `let x = <deep>` shape leaves `x` unused, so
            // the fused region drops it. GRAPHIX_DUMP_CLIF of the twin is the same size
            // at depth 10 and at depth 200 for parens, array, tuple, structlit, variant
            // and select; returning `x` instead makes it grow with the depth. So for
            // these nests no mode runs lowering or the JIT emitter on the small stack
            // (only `call`, `lambdachain` and `calleechain` reach them), and an
            // overflow there would go unseen. Fix: end the twin's body with the name
            // the program's last `let` binds, and assert that the twin fused
            // (`#[native]` on the pure shapes, or a fusion count). Correct the header's
            // "(which fuses)" until it does. probe:
            // design/review-2026-11-16/repro/tests-shell-compiler-a-twin-never-fuses-the-nest.sh
            // (tests-shell-compiler-a-01)
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
        "onblock" => {
            format!("let k = 1;\non k {{ {}1{} }}", "{ 1; ".repeat(d), " }".repeat(d))
        }
        "onnest" => {
            let mut s = String::from("1");
            for _ in 0..d {
                s = format!("on k {{ {s} }}");
            }
            format!("let k = 1;\nlet x = {s}")
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
        "chainbounds" | "chainboundsreversed" => {
            let mut bounds = (0..d)
                .map(|i| format!("'a{i}: Array<'a{}>", i + 1))
                .chain([format!("'a{d}: Number")])
                .collect::<Vec<_>>();
            if shape == "chainboundsreversed" {
                bounds.reverse()
            }
            let bounds = bounds.join(", ");
            let cells = (0..=d).map(|i| format!("'a{i}")).collect::<Vec<_>>().join(", ");
            format!("let f = {bounds} |x: ({cells})| x;\nf(never())")
        }
        // The chain through a typedef's parameter, a variant's, a union and
        // a typedef that is a union of its parameter.
        "chainboundstypedef" | "chainboundsvariant" | "chainboundsunion"
        | "chainboundsalias" => {
            let link = |i: usize| match shape {
                "chainboundstypedef" => format!("'a{i}: T<'a{}>", i + 1),
                "chainboundsvariant" => format!("'a{i}: `V('a{})", i + 1),
                "chainboundsalias" => format!("'a{i}: M<'a{}>", i + 1),
                _ => format!("'a{i}: [Array<'a{}>, null]", i + 1),
            };
            let bounds = (0..d)
                .map(link)
                .chain([format!("'a{d}: Number")])
                .collect::<Vec<_>>()
                .join(", ");
            let cells = (0..=d).map(|i| format!("'a{i}")).collect::<Vec<_>>().join(", ");
            let typedef = match shape {
                "chainboundsalias" => "type M<'x> = ['x, null];",
                _ => "type T<'x> = Array<'x>;",
            };
            format!("{typedef}\nlet f = {bounds} |x: ({cells})| x;\nf(never())")
        }
        // The chain through a typedef's parameter, each cell a parameter of
        // its own, every argument a reference to a let that is decided but
        // for a cell inside a function type.
        "chainboundsfnargs" => {
            let bounds = (0..d)
                .map(|i| format!("'a{i}: T<'a{}>", i + 1))
                .collect::<Vec<_>>()
                .join(", ");
            let formals =
                (0..=d).map(|i| format!("x{i}: 'a{i}")).collect::<Vec<_>>().join(", ");
            let lets = (0..d)
                .rev()
                .map(|i| format!("let v{i} = [v{}];\n", i + 1))
                .collect::<String>();
            let args = (0..=d).map(|i| format!("v{i}")).collect::<Vec<_>>().join(", ");
            format!(
                "type T<'x> = Array<'x>;\nlet f = {bounds} |{formals}| x0;\n\
                 let v{d} = |z| 0;\n{lets}let r = f({args});\n0"
            )
        }
        // The chain through a typedef's parameter, each cell a parameter of
        // its own, every argument a call.
        "chainboundsformals" => {
            let bounds = (0..d)
                .map(|i| format!("'a{i}: T<'a{}>", i + 1))
                .chain([format!("'a{d}: Number")])
                .collect::<Vec<_>>()
                .join(", ");
            let formals =
                (0..=d).map(|i| format!("x{i}: 'a{i}")).collect::<Vec<_>>().join(", ");
            let args = vec!["never()"; d + 1].join(", ");
            format!(
                "type T<'x> = Array<'x>;\nlet f = {bounds} |{formals}| x0;\nf({args})"
            )
        }
        // The chain through a typedef's parameter, every argument a name of
        // its own in a chain of typedefs.
        "chainboundsdecided" => {
            let typedefs = (0..d)
                .map(|i| format!("type A{i} = Array<A{}>;\n", i + 1))
                .collect::<String>();
            let bounds = (0..d)
                .map(|i| format!("'a{i}: T<'a{}>", i + 1))
                .chain([format!("'a{d}: Number")])
                .collect::<Vec<_>>()
                .join(", ");
            let formals =
                (0..=d).map(|i| format!("x{i}: 'a{i}")).collect::<Vec<_>>().join(", ");
            let lets = (0..=d)
                .map(|i| format!("let v{i}: A{i} = never();\n"))
                .collect::<String>();
            let args = (0..=d).map(|i| format!("v{i}")).collect::<Vec<_>>().join(", ");
            format!(
                "type T<'x> = Array<'x>;\n{typedefs}type A{d} = i64;\nlet f = {bounds} |{formals}| x0;\n{lets}f({args})"
            )
        }
        // The chain with a second conjunct on every link, the last link a
        // type the comparison takes.
        "chainboundsdiscernible" | "chainboundsordered" => {
            let (conj, last) = match shape {
                "chainboundsordered" => ("Ordered", "i64"),
                _ => ("Discernible", "Number"),
            };
            let bounds = (0..d)
                .map(|i| format!("'a{i}: Array<'a{}> + {conj}", i + 1))
                .chain([format!("'a{d}: {last}")])
                .collect::<Vec<_>>()
                .join(", ");
            let cells = (0..=d).map(|i| format!("'a{i}")).collect::<Vec<_>>().join(", ");
            format!("let f = {bounds} |x: ({cells})| x;\nf(never())")
        }
        // The chain through a function type: the next variable its
        // parameter, its result, its parameter with only the first link on
        // the definition's, or its parameter through a typedef.
        "chainboundsfn" | "chainboundsfnret" | "chainboundsfnheld"
        | "chainboundsfnalias" => {
            let link = |i: usize| match shape {
                "chainboundsfnret" => format!("'a{i}: fn(x: i64) -> 'a{}", i + 1),
                "chainboundsfnalias" => format!("'a{i}: C<'a{}>", i + 1),
                _ => format!("'a{i}: fn(x: 'a{}) -> i64", i + 1),
            };
            let bounds = (0..d)
                .map(link)
                .chain([format!("'a{d}: Number")])
                .collect::<Vec<_>>()
                .join(", ");
            let cells = match shape {
                "chainboundsfnheld" => String::from("'a0"),
                _ => format!(
                    "({})",
                    (0..=d).map(|i| format!("'a{i}")).collect::<Vec<_>>().join(", ")
                ),
            };
            let typedef = match shape {
                "chainboundsfnalias" => "type C<'x> = fn(x: 'x) -> i64;\n",
                _ => "",
            };
            format!("{typedef}let f = {bounds} |x: {cells}| x;\nf(never())")
        }
        // The chain through a union with each link the next one bare, the
        // call passing a value for every link; with each link implementing a
        // trait too.
        "chainboundsvalued" | "chainboundsvaluedtrait" | "chainboundsarrays" => {
            let (pre, conj) = match shape {
                "chainboundsvaluedtrait" => (
                    "trait Tr { val m: fn(self) -> i64 };\n\
                     impl Tr for i64 { let m = |x| 1 };\n",
                    " + Tr",
                ),
                _ => ("", ""),
            };
            let (last, value) = match shape {
                "chainboundsarrays" => ("Array<i64>", "[1]"),
                _ => ("Number", "1"),
            };
            let bounds = (0..d)
                .map(|i| format!("'a{i}: ['a{}, null]{conj}", i + 1))
                .chain([format!("'a{d}: {last}")])
                .collect::<Vec<_>>()
                .join(", ");
            let cells = (0..=d).map(|i| format!("'a{i}")).collect::<Vec<_>>().join(", ");
            let values = vec![value; d + 1].join(", ");
            format!("{pre}let f = {bounds} |x: ({cells})| x;\nf(({values}))")
        }
        // The chain through a union with each link the next one bare, the
        // call passing a value to the first link alone: the links below it
        // wait for it, the last open or a `never()` fed at it.
        "chainboundswaiting" | "chainboundswaitingnever" => {
            let never = shape == "chainboundswaitingnever";
            let bounds = (0..d)
                .map(|i| format!("'a{i}: ['a{}, null]", i + 1))
                .chain(never.then(|| format!("'a{d}: i64")))
                .collect::<Vec<_>>()
                .join(", ");
            let (formals, args) = match never {
                true => (format!("x: 'a0, y: 'a{d}"), "1, never()"),
                false => (String::from("x: 'a0"), "1"),
            };
            format!("let f = {bounds} |{formals}| x;\nf({args})")
        }
        // The chain on an impl's head, the target's parts in the chain's
        // order or against it.
        "chainboundsimpl" | "chainboundsimplreversed" => {
            let bounds = (0..d)
                .map(|i| format!("'a{i}: Array<'a{}>", i + 1))
                .chain([format!("'a{d}: Number")])
                .collect::<Vec<_>>()
                .join(", ");
            let mut cells = (0..=d).map(|i| format!("'a{i}")).collect::<Vec<_>>();
            if shape == "chainboundsimplreversed" {
                cells.reverse()
            }
            let cells = cells.join(", ");
            format!(
                "trait Tr {{ val m: fn(self) -> i64 }};
                 impl<{bounds}> Tr for ({cells}) {{ let m = |x| 1 }};
0"
            )
        }
        // Open cells, each the lower bound of the next, the last below a
        // rigid cell: the merge takes every one into it.
        "rigidchain" => {
            let mut s = String::from(
                "type W<'t> = Abstract<'t>;\nlet outer = 'c: Any |z: W<'c>| {\n\
                 let a0 = never();\nlet a1 = W(a0);\n",
            );
            for i in 2..=d {
                s.push_str(&format!("let a{i} = W(a{}.0);\n", i - 1));
            }
            s.push_str(&format!("let q: W<'c> = a{d};\nz\n}};\nouter(W(1))"));
            s
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

/// Cells chained by bounds: in one signature (`chainbounds`), and by lower
/// bounds below a rigid cell (`rigidchain`).
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
    "onblock",
    "onnest",
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
        let r = thread::Builder::new()
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
    // a current-thread runtime runs the graphix runtime on the thread that
    // drives it
    let r = thread::scope(|s| {
        thread::Builder::new()
            .stack_size(STACK)
            .spawn_scoped(s, || {
                let rt = tokio::runtime::Builder::new_current_thread()
                    .enable_all()
                    .build()
                    .expect("runtime");
                rt.block_on(async {
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
                                anyhow::bail!(
                                    "the second run did not restore the program image"
                                )
                            }
                            Ok(())
                        }
                        _ => shell(CacheMode::Off)
                            .run(MainThreadHandle::new().0)
                            .await
                            .map(|_| ()),
                    }
                })
            })
            .expect("spawn")
            .join()
            .expect("join")
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

/// One billion instructions, the unit of a run's work budget.
#[cfg(target_os = "linux")]
const BILLION: u64 = 1_000_000_000;

/// How long a run may take before it is taken for a hang, at any load.
#[cfg(target_os = "linux")]
const HANG: Duration = Duration::from_secs(600);

/// `graphix --check` of `program` finishes, accepted, within `budget`
/// instructions.
#[cfg(target_os = "linux")]
fn checks_within(program: &str, budget: u64) {
    completes_within(&["--check", "--no-cache"], program, budget)
}

#[cfg(target_os = "linux")]
fn completes_within(args: &[&str], program: &str, budget: u64) {
    let out = exits_within(args, &[("program.gx", program)], budget);
    assert!(out.status.success(), "{args:?} failed: {}", out.status);
}

/// `graphix` with `args` over `files` (the first is the program, the
/// others its modules) exits within `budget` instructions, its threads'
/// together, with its stderr; one that passes the budget is killed.
#[cfg(target_os = "linux")]
// CR claude for claude: [risk/low] `exits_within` pipes the child's stderr but reads it
// only after the child exits. A run that writes more than a pipe buffer (64 KiB) to
// stderr, such as a future warning over 20,000 lets, blocks in write, retires no
// instructions and is reported as a hang after 600 s. common::within_deadline already
// drains both pipes on threads for this reason. Make `exits_within` use it, with its
// poll loop also checking the instruction budget beside the deadline, instead of
// keeping a second spawn-and-poll loop. Found by reading; not run.
// (tests-shell-compiler-a-08)
fn exits_within(args: &[&str], files: &[(&str, &str)], budget: u64) -> Output {
    let dir = tempfile::tempdir().expect("a dir for the program");
    for (name, text) in files {
        fs::write(dir.path().join(name), text).expect("write the program");
    }
    let work = Instructions::count()
        .expect("count instructions (kernel.perf_event_paranoid must be 2 or lower)");
    let start = Instant::now();
    let mut child = Command::new(env!("CARGO_BIN_EXE_graphix"))
        .args(args)
        .arg(dir.path().join(files[0].0))
        .env("XDG_CACHE_HOME", dir.path())
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::piped())
        .spawn()
        .expect("run graphix");
    let over = |done: u64| {
        format!("{args:?} retired {:.1}G instructions, over {:.1}G", gi(done), gi(budget))
    };
    loop {
        if child.try_wait().expect("wait for graphix").is_some() {
            let out = child.wait_with_output().expect("graphix's output");
            let done = work.read();
            eprintln!("{args:?}: {:.1}G instructions of {:.1}G", gi(done), gi(budget));
            assert!(done <= budget, "{}", over(done));
            break out;
        }
        let done = work.read();
        if done > budget || start.elapsed() > HANG {
            let _ = child.kill();
            let _ = child.wait();
            match done > budget {
                true => panic!("{}", over(done)),
                false => panic!("{args:?} ran past {HANG:?}"),
            }
        }
        thread::sleep(Duration::from_millis(20));
    }
}

/// `n` instructions in billions.
#[cfg(target_os = "linux")]
fn gi(n: u64) -> f64 {
    n as f64 / BILLION as f64
}

/// A type as deep as the program is long checks in time linear in the
/// program: `flattype` at eight times `FLAT_DEPTH` checks well inside a
/// budget a walk of each let's whole type blows several times over.
/// Nested `try` blocks, each a lexical scope, check in time quadratic
/// in their depth: a name's lookup consults its module's root once per
/// scope chain, not once per level.
#[test]
#[cfg(target_os = "linux")]
// CR claude for claude: [perf/low] Nested `try` builds in near-cubic time, not the
// quadratic the doc above says. --check at depths 248, 496, 700 and 993 costs 0.16,
// 0.74, 1.91 and 5.32 s above an empty program (debug build): x4.6 and then x7.2 per
// doubling. GRAPHIX_PROFILE puts it all in BuildGraph. A flat run of 1000 trys, nested
// blocks and nested catches all stay cheap, so the cost is in how the try lowering
// (`seq::desugar`, `Machine::lower_try`) and the compile of its machine handle nesting.
// The 250 G budget, by far the loosest in the file, is what lets it pass. Profile the
// 993-deep case's BuildGraph to find the walk repeated per enclosing level and make it
// once per try. Pin the growth between two depths instead of one absolute budget. The
// doc's first three lines describe `flattype`, the next test's subject: move them
// there. probe:
// design/review-2026-11-16/repro/tests-shell-compiler-a-nested-try-check-cubic.sh
// (tests-shell-compiler-a-04)
fn nested_trys_check_in_time() {
    checks_within(&program("trywith", 993), 250 * BILLION)
}

#[test]
#[cfg(target_os = "linux")]
fn flat_type_chain_checks_in_linear_time() {
    checks_within(&program("flattype", 8 * FLAT_DEPTH), 30 * BILLION)
}

/// A chain of bounds three times `CHAIN_DEPTH` long checks in time linear in
/// its length: its acyclicity, the quantifiers of its signature, the
/// call's settle of each cell and the site's bounds each walk the chain
/// once, not once a cell.
#[test]
#[cfg(target_os = "linux")]
fn a_chain_of_bounds_checks_in_linear_time() {
    checks_within(&program("chainbounds", 3 * CHAIN_DEPTH), 14 * BILLION)
}

/// A nested lambda's own variable bounded by an enclosing variable, whose
/// bound names the enclosing variable of the nested one's name, checks at
/// once: the two variables stay apart, so no bound reaches itself.
#[test]
#[cfg(target_os = "linux")]
fn a_nested_lambda_bounded_by_an_enclosing_variable_checks_at_once() {
    checks_within(
        "let f = 'z: Any, 'b: 'z |w0: 'z, w1: 'b| {\n\
           let g = 'z: 'b |x0: 'z, x1: 'b| x0;\n\
           g(w1, w1)\n\
         };\n\
         f(1, 2)",
        2 * BILLION,
    )
}

/// A chain of bounds through function types checks in time linear in its
/// length: a walk meets each link's conjunct once however many function
/// types name its variable, whether it aliases names, counts occurrences,
/// collects quantifiers or reaches cells.
#[test]
#[cfg(target_os = "linux")]
fn a_chain_of_bounds_through_function_types_checks_in_linear_time() {
    for shape in ["chainboundsfn", "chainboundsfnret", "chainboundsfnheld"] {
        checks_within(&program(shape, CHAIN_DEPTH), 8 * BILLION)
    }
}

/// A chain of bounds with a second conjunct, `Discernible` or `Ordered`,
/// on every link checks in time linear in its length: the call settles the
/// chain last link first, and neither the comparison's judgment of each
/// link's binding nor the bound it hands the binding's open cells walks
/// the links already bound again.
#[test]
#[cfg(target_os = "linux")]
fn a_chain_of_bounds_with_a_second_conjunct_checks_in_linear_time() {
    for shape in ["chainboundsdiscernible", "chainboundsordered"] {
        checks_within(&program(shape, CHAIN_DEPTH), 6 * BILLION)
    }
}

/// An impl whose head carries a chain of bounds checks its method in time
/// linear in the chain's length, the target's parts in the chain's order
/// or against it: each of the head's cells merges with the method's copy
/// of it, and the merge's occurs checks pass by the links below, numbered
/// by what they reach when the first walked them. The head's variables
/// are checked for a duplicate in one pass.
#[test]
#[cfg(target_os = "linux")]
fn an_impl_head_with_a_chain_of_bounds_checks_in_linear_time() {
    checks_within(&program("chainboundsimpl", 2 * CHAIN_DEPTH), 9 * BILLION);
    checks_within(&program("chainboundsimplreversed", CHAIN_DEPTH), 6 * BILLION)
}

/// A call passing a value for every link of a chain of bounds checks in
/// time linear in the chain's length: binding a link asks whether the
/// links below admit the value, and the answer for a link stands while
/// nothing it reaches is written, whichever links above it bind.
#[test]
#[cfg(target_os = "linux")]
fn a_call_passing_values_to_a_chain_of_bounds_checks_in_linear_time() {
    checks_within(&program("chainboundsvalued", CHAIN_DEPTH), 8 * BILLION)
}

/// The same with an array for every link: a link's lower bounds are held
/// once however many links lie below it, so the work does not double with
/// each link.
#[test]
#[cfg(target_os = "linux")]
fn a_call_passing_arrays_to_a_chain_of_bounds_checks_in_polynomial_time() {
    checks_within(&program("chainboundsarrays", 40), 3 * BILLION)
}

/// The same with a trait on every link: the answer that `i64` implements
/// it read no impl's cells, so the answers for the links below stand.
#[test]
#[cfg(target_os = "linux")]
fn a_call_passing_values_to_a_chain_of_trait_bounds_checks_in_linear_time() {
    checks_within(&program("chainboundsvaluedtrait", CHAIN_DEPTH), 9 * BILLION)
}

/// A call passing a value to the first link of a chain of union bounds
/// settles the links below in time linear in the chain's length: each
/// link settles after the one above it, and the answer that the links
/// below a link admit its value stands while nothing they reach is
/// written, across the call's settles.
#[test]
#[cfg(target_os = "linux")]
fn a_chain_of_waiting_bounds_settles_in_linear_time() {
    for shape in ["chainboundswaiting", "chainboundswaitingnever"] {
        checks_within(&program(shape, CHAIN_DEPTH), 6 * BILLION)
    }
}

/// An interface `val` carrying a chain of bounds matches its
/// implementation in time linear in the chain's length: the signature's
/// variables are held rigid once for every bound judged, and each bound
/// pairs its variable's cells by an index the pairing grows.
#[test]
#[cfg(target_os = "linux")]
fn an_interface_val_with_a_chain_of_bounds_checks_in_linear_time() {
    let d = CHAIN_DEPTH;
    let bounds = (0..d)
        .map(|i| format!("'a{i}: Array<'a{}>", i + 1))
        .chain([format!("'a{d}: Number")])
        .collect::<Vec<_>>()
        .join(", ");
    let cells = (0..=d).map(|i| format!("'a{i}")).collect::<Vec<_>>().join(", ");
    let out = exits_within(
        &["--check", "--no-cache"],
        &[
            ("main.gx", "mod m;\nm::f(never())\n"),
            ("m.gxi", &format!("val f: fn<{bounds}>(x: ({cells})) -> ({cells});\n")),
            ("m.gx", &format!("let f = {bounds} |x: ({cells})| x;\n")),
        ],
        10 * BILLION,
    );
    assert!(
        out.status.success(),
        "the module checks: {}",
        String::from_utf8_lossy(&out.stderr)
    );
}

/// The same chain declared last link first: seeding a conjunct walks no
/// chain its successors' conjuncts already hold.
#[test]
#[cfg(target_os = "linux")]
fn a_chain_of_bounds_declared_last_link_first_checks_in_linear_time() {
    checks_within(&program("chainboundsreversed", 3 * CHAIN_DEPTH), 14 * BILLION)
}

/// The chain elaborates in time linear in its length, in either order, at
/// two depths: the call binds each cell of the signature's copy to a tree
/// that holds no cell, with each conjunct probed once a chain and no
/// occurs check walking such a tree.
#[test]
#[cfg(target_os = "linux")]
fn a_chain_of_bounds_elaborates_in_linear_time() {
    let expand = ["--expand", "--no-cache"];
    completes_within(&expand, &program("chainbounds", CHAIN_DEPTH), 8 * BILLION);
    completes_within(
        &expand,
        &program("chainboundsreversed", 3 * CHAIN_DEPTH),
        18 * BILLION,
    )
}

/// The chain through a typedef's parameter, or a variant's, elaborates in
/// time linear in its length, at two depths: the instance's type holds
/// each link's application once, a node every walk and rewrite of it
/// visits once, and the call's check keys, hashes and expands each
/// application once. Through a typedef that is a union of its parameter,
/// whether each link's application names a union member is asked of its
/// parameter once a walk, not walked to the chain's end at every link.
#[test]
#[cfg(target_os = "linux")]
fn a_chain_of_bounds_through_a_typedef_or_a_variant_elaborates_in_linear_time() {
    let expand = ["--expand", "--no-cache"];
    for (shape, budget) in
        [("chainboundstypedef", 7), ("chainboundsvariant", 6), ("chainboundsalias", 8)]
    {
        completes_within(&expand, &program(shape, CHAIN_DEPTH), budget * BILLION);
        completes_within(
            &expand,
            &program(shape, 2 * CHAIN_DEPTH),
            (2 * budget - 1) * BILLION,
        )
    }
}

/// The chain through function types elaborates in time linear in its
/// length, at two depths: a link's positions are asked whether they hold a
/// union of open cells, or a cell the arguments widen, only where they
/// reach an open cell, which the walk keeps for each node it has met.
#[test]
#[cfg(target_os = "linux")]
fn a_chain_of_bounds_through_function_types_elaborates_in_linear_time() {
    let expand = ["--expand", "--no-cache"];
    for shape in ["chainboundsfn", "chainboundsfnret", "chainboundsfnheld"] {
        completes_within(&expand, &program(shape, CHAIN_DEPTH), 9 * BILLION);
        completes_within(&expand, &program(shape, 2 * CHAIN_DEPTH), 17 * BILLION)
    }
}

/// The chain through a function typedef elaborates in time linear in its
/// length, at two depths: no link's expansion is compared with every
/// other's, since a function type is keyed by its parts, any open cell
/// inside it alike; and a commit's copy of a link's expansion keeps the
/// rest of the chain, which holds no cell, without walking it.
#[test]
#[cfg(target_os = "linux")]
fn a_chain_of_bounds_through_a_function_typedef_elaborates_in_linear_time() {
    let expand = ["--expand", "--no-cache"];
    let chain = |d| program("chainboundsfnalias", d);
    completes_within(&expand, &chain(CHAIN_DEPTH), 7 * BILLION);
    completes_within(&expand, &chain(2 * CHAIN_DEPTH), 13 * BILLION)
}

/// The chain through a typedef's parameter, each cell a parameter of its
/// own and every argument a call, checks in time linear in its length, at
/// two depths: an argument whose type its own check makes is not
/// pre-unified, so no open copy of its formal, which would copy the chain
/// below it, is made for it.
#[test]
#[cfg(target_os = "linux")]
fn a_chain_of_bounds_as_parameters_checks_in_linear_time() {
    checks_within(&program("chainboundsformals", CHAIN_DEPTH), 5 * BILLION);
    checks_within(&program("chainboundsformals", 2 * CHAIN_DEPTH), 9 * BILLION)
}

/// The chain through a typedef's parameter, passed references to lets each
/// holding the rest of the chain around a function type, checks within a
/// budget at two small lengths. Its cost grows nearly with the cube of its
/// length: the arguments' copies of the chain differ only in their cells,
/// and each is compared with the others where a bound is recorded, a
/// typedef unfolded and a cell fixed. The budgets hold that growth, at
/// lengths no program reaches.
#[test]
#[cfg(target_os = "linux")]
fn a_chain_of_bounds_passed_function_references_checks_within_a_budget() {
    checks_within(&program("chainboundsfnargs", 60), 16 * BILLION / 10);
    checks_within(&program("chainboundsfnargs", 120), 4 * BILLION)
}

/// The chain of bounds as parameters, called with a name of each link's
/// type in a chain of typedefs, checks in time linear in its length, at
/// two depths: the call asks of each argument and each formal whether it
/// can carry a function or an unknown part, and what the walk decided of
/// the definition a name stands for is shared by every argument.
#[test]
#[cfg(target_os = "linux")]
fn a_chain_of_typedef_names_as_arguments_checks_in_linear_time() {
    checks_within(&program("chainboundsdecided", CHAIN_DEPTH / 2), 7 * BILLION);
    checks_within(&program("chainboundsdecided", CHAIN_DEPTH), 13 * BILLION)
}

/// The chain through a union elaborates in time linear in its length, at
/// two depths: each link's bound is found identical to the instance's
/// type once a walk, not once a link, and no cells are linked where none
/// were paired.
#[test]
#[cfg(target_os = "linux")]
fn a_chain_of_bounds_through_a_union_elaborates_in_linear_time() {
    let expand = ["--expand", "--no-cache"];
    completes_within(&expand, &program("chainboundsunion", CHAIN_DEPTH), 9 * BILLION);
    completes_within(&expand, &program("chainboundsunion", 2 * CHAIN_DEPTH), 17 * BILLION)
}

/// A let chain that wraps the last value in any constructor, or in an
/// array literal holding it twice, checks in time linear in its length:
/// a let's single name binds without an occurs walk of the value, and a
/// literal's element of an earlier element's very type adds nothing.
#[test]
#[cfg(target_os = "linux")]
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
        checks_within(&s, 30 * BILLION)
    }
}

/// A chain of array lets read by many literals after a generic call
/// (whose check changes a bound cell, so what was proved before it is
/// proved again) checks in time linear in the chain and the reads: the
/// first read walks the chain once, the others stop at its head.
#[test]
#[cfg(target_os = "linux")]
fn a_let_chain_read_after_a_call_checks_in_linear_time() {
    const LETS: usize = 20_000;
    const READS: usize = 4_000;
    let mut s = String::from("let x0 = 1;\n");
    for i in 1..=LETS {
        s.push_str(&format!("let x{i} = [x{p}, x{p}];\n", p = i - 1));
    }
    s.push_str("let z = array::push([1], 2);\n");
    for j in 1..=READS {
        s.push_str(&format!("let y{j} = [x{LETS}];\n"));
    }
    s.push_str("1");
    checks_within(&s, 30 * BILLION)
}

#[test]
#[cfg(target_os = "linux")]
fn a_long_seq_of_calls_checks_in_time() {
    const STEPS: usize = 4000;
    let body = vec!["a <- str::len(\"x\")"; STEPS].join("; ");
    checks_within(&format!("let a = 0;\nlet r = seq {{ {body}; a }};\nr"), 15 * BILLION)
}

/// A chain of typedefs each naming the next, written before it is
/// defined, checks in time near linear in its length: a typedef with no
/// parameters rechecks nothing pending.
#[test]
#[cfg(target_os = "linux")]
fn forward_typedef_chain_checks_in_time() {
    const TYPES: usize = 1000;
    let mut s = String::new();
    for i in 0..TYPES {
        s.push_str(&format!("type T{i} = [`A{i}(i64), T{}];\n", i + 1));
    }
    s.push_str(&format!("type T{TYPES} = `End;\n1"));
    checks_within(&s, 15 * BILLION)
}

/// Parametric typedefs each naming the next before it is defined, or
/// each the next around a cycle, check in time near linear in their
/// number: a typedef rechecks only the self-references of those pending,
/// and one walk that finds the definitions it reaches regular settles
/// them all.
#[test]
#[cfg(target_os = "linux")]
fn forward_parametric_typedefs_check_in_linear_time() {
    const TYPES: usize = 1000;
    let program = |last: &str| {
        let mut s = String::new();
        for i in 0..TYPES {
            s.push_str(&format!("type A{i}<'a> = [`N{i}, `C{i}('a, A{}<'a>)];\n", i + 1));
        }
        s.push_str(&format!("type A{TYPES}<'a> = {last};\nlet x: A0<i64> = `N0;\nx"));
        s
    };
    checks_within(&program("[`End, `C('a)]"), 4 * BILLION);
    checks_within(&program("[`End, `C('a, A0<'a>)]"), 4 * BILLION);
}

/// A value whose type shares each level twice (`(x, x)`), written to a
/// binding of an equal type and checked against a typedef built the same
/// way, checks in time linear in its depth: a walk of it as a tree takes
/// 2^depth steps.
#[test]
#[cfg(target_os = "linux")]
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
        checks_within(&program, 10 * BILLION)
    }
}

/// Comparing, ordering, keying and selecting over a value whose type is a
/// pair DAG checks in time linear in its depth: the Discernible and Ordered
/// judgments, the bounds they hand its cells and an arm's match walk each
/// shared node once.
#[test]
#[cfg(target_os = "linux")]
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
        checks_within(&format!("{chain}{use_}"), 10 * BILLION)
    }
}

/// The defaults a written type hides are judged in time linear in the
/// types: a value typed by a recursive typedef holding no function with a
/// default is not expanded, and a DAG of 4-tuples checked against a
/// typedef built the same way is walked once per part.
#[test]
#[cfg(target_os = "linux")]
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
        checks_within(&program, 10 * BILLION)
    }
}

// An instance's types are its definition's, taken row by row: a shared
// node is taken once, so a call of a definition building a DAG of pairs
// elaborates in time linear in the DAG.
#[test]
#[cfg(target_os = "linux")]
fn a_call_over_a_shared_type_dag_elaborates_in_linear_time() {
    const DEPTH: usize = 60;
    let mut program = String::from("let f = |a| {\n  let x0 = a;\n");
    for i in 1..=DEPTH {
        program.push_str(&format!("  let x{i} = (x{}, x{});\n", i - 1, i - 1));
    }
    program.push_str(&format!("  x{DEPTH}\n}};\nlet r = f(1);\nsys::exit(0)\n"));
    completes_within(&["--no-cache"], &program, 10 * BILLION)
}

/// Bounds that name the next variable twice (in a tuple, a struct or a
/// map, or the next two variables), and a pair DAG passed where a bound's
/// conjunct holds its cell, elaborate in time linear in their depth: the
/// commit of a cell's conjuncts and the position, closure and occurs
/// walks each take a shared node, a map's included, once.
#[test]
#[cfg(target_os = "linux")]
fn bound_dags_elaborate_in_linear_time() {
    const DEPTH: usize = 60;
    let cells = (0..=DEPTH).map(|i| format!("'a{i}")).collect::<Vec<_>>().join(", ");
    for link in
        ["('a{n}, 'a{n})", "{l: 'a{n}, r: 'a{n}}", "Map<'a{n}, 'a{n}>", "('a{n}, 'a{m})"]
    {
        let bounds = (0..DEPTH)
            .map(|i| {
                let bound = link
                    .replace("{n}", &(i + 1).to_string())
                    .replace("{m}", &(i + 2).min(DEPTH).to_string());
                format!("'a{i}: {bound}")
            })
            .collect::<Vec<_>>()
            .join(", ");
        let program = format!(
            "let f = {bounds}, 'a{DEPTH}: Number |x: ({cells})| x;\nf(never())\n"
        );
        completes_within(&["--expand", "--no-cache"], &program, 10 * BILLION)
    }
    let mut valued = String::from("let x0 = 1;\n");
    for i in 1..=DEPTH {
        valued.push_str(&format!("let x{i} = (x{}, x{});\n", i - 1, i - 1));
    }
    valued
        .push_str(&format!("let f = 'a: Array<'b> |x: 'a, y: 'b| 0;\nf([], x{DEPTH})\n"));
    completes_within(&["--expand", "--no-cache"], &valued, 10 * BILLION)
}

/// `f` over a DAG of bounds through a union typedef, each variable's
/// bound naming the next two under one constructor, and `calls` of it.
#[cfg(target_os = "linux")]
fn union_typedef_bound_dag(calls: &str) -> String {
    const DEPTH: usize = 40;
    let bounds = (0..DEPTH - 1)
        .map(|i| format!("'a{i}: P<'a{}, 'a{}>", i + 1, i + 2))
        .collect::<Vec<_>>()
        .join(", ");
    format!(
        "type P<'x, 'y> = [Array<'x>, Array<'y>, null];\n\
         let f = {bounds}, 'a{}: [Array<'a{DEPTH}>, null], 'a{DEPTH}: Number \
         |x: ('a0, 'a1)| 7;\n{calls}\n",
        DEPTH - 1
    )
}

/// A DAG of bounds through a union typedef builds its instance in
/// polynomial time: the guide probes of the commits that take lower
/// bounds are kept as any probe is, a verdict over two types holding no
/// cell stands through the binds, and a union's members are ordered by
/// depth with each shared node measured once.
#[test]
#[cfg(target_os = "linux")]
fn a_bound_dag_through_a_union_typedef_builds_in_polynomial_time() {
    let program = union_typedef_bound_dag("f(never())");
    completes_within(&["--expand", "--no-cache"], &program, 4 * BILLION)
}

/// The same instance called from a second function fuses in polynomial
/// time: the search of a node's type for an abstract value walks each
/// shared application once.
#[test]
#[cfg(target_os = "linux")]
fn a_bound_dag_through_a_union_typedef_fuses_in_polynomial_time() {
    let program = union_typedef_bound_dag("let g = |y| f(y);\ng(never())");
    completes_within(&["--expand", "--no-cache"], &program, 8 * BILLION)
}

/// A search over a shared type walks each shared node once: the definition
/// called at two types, whose kernels' types are DAGs, builds in time
/// linear in the DAG.
#[test]
#[cfg(target_os = "linux")]
fn a_generic_call_over_a_shared_type_dag_builds_in_linear_time() {
    const DEPTH: usize = 60;
    let mut program = String::from("let f = |a| {\n  let x0 = a;\n");
    for i in 1..=DEPTH {
        program.push_str(&format!("  let x{i} = (x{}, x{});\n", i - 1, i - 1));
    }
    program.push_str(&format!(
        "  x{DEPTH}\n}};\nlet r = f(1);\nlet q = f(\"s\");\nsys::exit(0)\n"
    ));
    completes_within(&["--no-cache"], &program, 10 * BILLION)
}

/// A typed value over a recursive union prints in time linear in its depth:
/// each level picks its member by its shape, where testing a member's whole
/// value at each level is quadratic.
#[test]
#[cfg(target_os = "linux")]
fn a_deep_typed_value_prints_in_linear_time() {
    use graphix_compiler::{
        env::Env,
        expr::{ModPath, TypeDefBody, parser::parse_type},
        rep,
        typ::TVal,
    };
    use netidx::publisher::Value;
    const DEPTH: i64 = 20_000;
    let mut env = Env::default();
    env.deftype(
        &ModPath::root(),
        "L",
        triomphe::Arc::from_iter([]),
        &TypeDefBody::Alias(parse_type("[`Cons(i64, L), `Nil]").unwrap()),
        true,
        None,
        Default::default(),
        Default::default(),
        Default::default(),
    )
    .unwrap();
    let typ = parse_type("L").unwrap().scope_refs(&ModPath::root());
    let mut v = rep::make_variant("Nil".into(), []);
    for i in 0..DEPTH {
        v = rep::make_variant("Cons".into(), [Value::I64(i), v]);
    }
    let work = Instructions::count()
        .expect("count instructions (kernel.perf_event_paranoid must be 2 or lower)");
    let printed = TVal { env: &env, typ: &typ, v: &v }.to_string();
    let done = work.read();
    assert!(printed.starts_with("`Cons(19999, "));
    eprintln!("printed {DEPTH} levels in {:.2}G instructions", gi(done));
    assert!(done <= BILLION / 2, "{:.2}G instructions, over 0.5G", gi(done));
}

/// A chain of written holders, each link a pair of the last, checks in
/// time linear in its length: a written binding holds one instance of each
/// node its value's type shares, so the instance keeps the value's DAG, where
/// an instance at each occurrence takes 2^depth.
#[test]
#[cfg(target_os = "linux")]
fn written_holder_chains_check_in_linear_time() {
    const LINKS: usize = 40;
    let head = "let x0 = (|x| x, 1);\n";
    let connected = chain(
        head,
        LINKS,
        &|i| format!("let x{i} = (x{p}, x{p}); x{i} <- (x{p}, x{p});\n", p = i - 1),
        "0\n",
    );
    let apart = chain(
        head,
        LINKS,
        &|i| {
            format!(
                "let (a{i}, w{i}) = (x{p}, x{p}); w{i} <- never(); let x{i} = (a{i}, a{i});\n",
                p = i - 1
            )
        },
        "0\n",
    );
    for program in [connected, apart] {
        checks_within(&program, 4 * BILLION)
    }
}

/// A chain of calls of a generic pair constructor, each link the pair of
/// the last, builds and runs within a few times an empty program's work,
/// written or not, with or without a function in the pair, and as a
/// variant's payload: elaboration takes each shared node of an argument's
/// type once, where once per path takes 2^links.
#[test]
#[cfg(target_os = "linux")]
fn generic_pair_call_chains_build_in_time() {
    const LINKS: usize = 40;
    let head = |pair: &str, x0: &str| format!("let mk = |f| {pair};\nlet x0 = {x0};\n");
    let called = |i: usize| format!("let x{i} = mk(x{p});\n", p = i - 1);
    let written =
        |i: usize| format!("let x{i} = mk(x{p}); x{i} <- mk(x{p});\n", p = i - 1);
    let links: [(&str, &str, &dyn Fn(usize) -> String); 5] = [
        ("(f, f)", "(|x| x, 1)", &called),
        ("(f, f)", "1", &called),
        ("(f, f)", "(|x| x, 1)", &written),
        ("(f, f)", "(1, 1)", &written),
        ("`V(f, f)", "(|x| x, 1)", &called),
    ];
    for (pair, x0, link) in links {
        let program = chain(&head(pair, x0), LINKS, link, "0\n");
        completes_within(&["--expand", "--no-cache"], &program, 4 * BILLION)
    }
    let run = chain(&head("(f, f)", "(|x| x, 1)"), LINKS, &called, "sys::exit(0)\n");
    completes_within(&["--no-cache"], &run, 4 * BILLION)
}

/// A chain of calls of a generic pair constructor over data, each link
/// the pair of the last, checks in time linear in its length: a call
/// proves its formals ground once its arguments are checked, and the
/// walks of the call and of its settle pass a ground cell by or read what
/// it keeps of its binding. Its build at 320 links is held where it
/// stands: an instance's elaboration and fusion still walk its resolved
/// signature (call-dag-arg-walk-01).
#[test]
#[cfg(target_os = "linux")]
// CR claude for claude: [style/low] These comments cite review ids that are no longer
// in the tree: (call-dag-arg-walk-01) here and on
// generic_pair_call_chains_share_their_resolutions, and (x-panics-02) on
// swallowed_error_logs.rs's an_empty_rand_range_is_logged. The "9.5 G before" and "14.5
// G before" on the select-arm cost tests narrate history. The limitation this comment
// names still holds and is now tracked nowhere: a chain of generic pair calls builds in
// quadratic time but checks in linear time (--expand --no-fusion: 0.28, 0.85 and 3.30 s
// at 640, 1280 and 2560 links). Drop the ids and the "before" figures, and record the
// quadratic build in design/ (or a CR) where someone will find it. probe:
// design/review-2026-11-16/repro/tests-shell-compiler-a-pair-chain-elab-quadratic.sh
// (tests-shell-compiler-a-10)
fn generic_pair_call_chains_check_in_linear_time() {
    let program = |links| {
        chain(
            "let mk = |f| (f, f);\nlet x0 = 1;\n",
            links,
            &|i| format!("let x{i} = mk(x{p});\n", p = i - 1),
            "0\n",
        )
    };
    checks_within(&program(1280), 3 * BILLION);
    completes_within(&["--expand", "--no-cache"], &program(320), 28 * BILLION / 10)
}

/// A chain of calls of a generic pair constructor over data elaborates,
/// and checked with every link written checks, within what a walk of
/// each link's whole argument once a call passes: a ground cell keeps
/// its resolution, a written binding's instance of a ground cell that
/// holds no function is that resolution, and a containment takes a
/// ground cell over parts its resolution shares at once
/// (call-dag-arg-walk-01).
#[test]
#[cfg(target_os = "linux")]
fn generic_pair_call_chains_share_their_resolutions() {
    const LINKS: usize = 1280;
    let called = chain(
        "let mk = |f| (f, f);\nlet x0 = 1;\n",
        LINKS,
        &|i| format!("let x{i} = mk(x{p});\n", p = i - 1),
        "0\n",
    );
    let written = chain(
        "let mk = |f| (f, f);\nlet x0 = (1, 1);\n",
        LINKS,
        &|i| format!("let x{i} = mk(x{p}); x{i} <- mk(x{p});\n", p = i - 1),
        "0\n",
    );
    completes_within(&["--expand", "--no-fusion", "--no-cache"], &called, 7 * BILLION);
    checks_within(&written, 5 * BILLION)
}

/// `head`, then `link(i)` for each `i` from 1 to `links`, then `tail`.
#[cfg(target_os = "linux")]
fn chain(head: &str, links: usize, link: &dyn Fn(usize) -> String, tail: &str) -> String {
    let mut s = String::from(head);
    for i in 1..=links {
        s.push_str(&link(i));
    }
    s.push_str(tail);
    s
}

/// A chain of holders, each link reading the last beside a select over it,
/// checks and runs in time linear in its length: the arm's pattern, whose
/// cell the scrutinee's DAG binds, is opened over its `Any` parts once per
/// pair of shared nodes, and the select's shallow test reads the scrutinee
/// and the pattern once a node, where once per path takes 2^depth.
#[test]
#[cfg(target_os = "linux")]
fn select_holder_chains_check_and_run_in_linear_time() {
    const LINKS: usize = 40;
    for link in ["{a: x, b: select x { v => v }}", "{a: x, b: select x { _ => x }, c: x}"]
    {
        let mut program = String::from("let y = (|z| z, 2);\nlet x = (|x| x, 1);\n");
        for _ in 0..LINKS {
            program.push_str(&format!("let x = {link};\n"));
        }
        checks_within(&format!("{program}0\n"), 4 * BILLION);
        completes_within(
            &["--no-cache"],
            &format!("{program}sys::exit(0)\n"),
            4 * BILLION,
        )
    }
}

/// A chain of selects, each over the pair of the last one's result, and a
/// seq's chain of steps, each a generic pair constructor's call on the
/// last, check and run in time linear in their length: every part of the
/// scrutinee's DAG is a bound cell (a select's result, an instance's), and
/// the arm's pattern is opened over its `Any` parts once per pair of the
/// nodes the cells hold, where once per path takes 2^links.
#[test]
#[cfg(target_os = "linux")]
fn select_chains_over_parts_shared_through_cells_are_linear() {
    const LINKS: usize = 40;
    let selects = chain(
        "let x0 = (|x| x, 1);\n",
        LINKS,
        &|i| format!("let x{i} = select (x{p}, x{p}) {{ v => v }};\n", p = i - 1),
        "",
    );
    let steps = chain(
        "let mk = |f| (f, f);\nlet x0 = (|x| x, 1);\nlet r = seq {\n",
        LINKS,
        &|i| format!("  let x{i} = mk(x{p});\n", p = i - 1),
        "  0\n};\n",
    );
    for program in [selects, steps] {
        checks_within(&format!("{program}0\n"), 4 * BILLION);
        completes_within(
            &["--no-cache"],
            &format!("{program}sys::exit(0)\n"),
            4 * BILLION,
        )
    }
}

/// Arms that each take the elements of a slice apart, over a parameter
/// nothing annotates, check in time polynomial in their number: the
/// settle of each arm's element binds judges the members of the type the
/// elements stand over once per member that walks alike, the open cell a
/// forwarding chain ends in is read once while no cell is written, and a
/// join reads the nodes of its alternatives once while no cell is written.
/// Debug, `--no-cache --check`, 16 arms: 5.0 G instructions, beside 0.8
/// over `x: Array<Array<i64>>`.
#[test]
#[cfg(target_os = "linux")]
fn slice_arms_over_element_slices_check_in_bounded_time() {
    const ARMS: usize = 16;
    let mut program: LPooled<String> = LPooled::take();
    program.push_str("let f = |x| select x {\n");
    for k in (1..=ARMS).rev() {
        let elems = (0..k).map(|i| format!("[a{i}, s{i}..]")).collect::<Vec<_>>();
        writeln!(program, "  [{}, t..] => t,", elems.join(", ")).unwrap();
    }
    program.push_str("  _ => []\n};\n0\n");
    checks_within(&program, 8 * BILLION);
}

/// A select over two pair DAGs, and an or-pattern's name over them, their
/// parts shared as composites or through cells, check and run in time
/// linear in their length: the meet of the alternatives meets each pair of
/// the nodes they hold once, where once per path takes 2^links.
#[test]
#[cfg(target_os = "linux")]
fn meets_over_two_pair_dags_are_linear() {
    const LINKS: usize = 40;
    let head = "let x: i64 = 1;\nlet y: Any = 1;\nlet b = true;\nlet tag: [`L, `R] = `L;\n\
                let x0 = (|q| q, x);\nlet y0 = (|q| q, y);\n";
    let composites = |i: usize| {
        format!("let x{i} = (x{p}, x{p});\nlet y{i} = (y{p}, y{p});\n", p = i - 1)
    };
    let cells = |i: usize| {
        format!(
            "let x{i} = select (x{p}, x{p}) {{ v => v }};\n\
             let y{i} = select (y{p}, y{p}) {{ v => v }};\n",
            p = i - 1
        )
    };
    let select = format!("let r = select b {{ true => x{LINKS}, false => y{LINKS} }};\n");
    let or_name = format!(
        "let r = select (tag, x{LINKS}, y{LINKS}) {{ (`L, a, _) | (`R, _, a) => 0 }};\n"
    );
    for link in [&composites as &dyn Fn(usize) -> String, &cells] {
        for tail in [&select, &or_name] {
            let program = chain(head, LINKS, link, tail);
            checks_within(&format!("{program}0\n"), 4 * BILLION);
            completes_within(
                &["--no-cache"],
                &format!("{program}sys::exit(0)\n"),
                4 * BILLION,
            )
        }
    }
}

/// A pair DAG named through a typedef builds in time linear in its depth:
/// a call's argument checked against its formal compares and welds two
/// equal DAGs, in different allocations, once per pair of nodes.
#[test]
#[cfg(target_os = "linux")]
fn a_typedef_named_type_dag_builds_in_linear_time() {
    const DEPTH: usize = 60;
    let mut program = String::from(
        "type R<'a, 'b> = ('a, 'b);\nlet mk = |x: 'a| -> R<'a, 'a> (x, x);\nlet f = |a| {\n  let x0 = a;\n",
    );
    for i in 1..=DEPTH {
        program.push_str(&format!("  let x{i} = mk(x{});\n", i - 1));
    }
    program.push_str(&format!("  x{DEPTH}\n}};\nlet r = f(1);\nsys::exit(0)\n"));
    completes_within(&["--no-cache"], &program, 10 * BILLION)
}

/// A pair DAG built through a generic constructor, and one reached
/// through generic callers, a monomorphic one and a collection's
/// callback, builds in time linear in its depth: the position and
/// pre-unification walks of a signature take a shared node once.
#[test]
#[cfg(target_os = "linux")]
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
        completes_within(&["--no-cache"], &program, 10 * BILLION)
    }
}

/// A module's refusal for deciding a cell the code around it left open
/// names the binding in time linear in the types in scope, a pair DAG's
/// included.
#[test]
#[cfg(target_os = "linux")]
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
        10 * BILLION,
    );
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(
        !out.status.success() && stderr.contains("annotate `a`"),
        "the module's write to `a` is refused: {stderr}"
    );
}

/// A value or a declared var refused against a DAG of named unions
/// (`L<i> = [L<i-1>, M<i-1>]`, `M<i>` the same) is refused, with its
/// message, in time linear in the DAG's depth, where its paths are 2^depth.
#[test]
#[cfg(target_os = "linux")]
fn a_refusal_against_a_dag_of_named_unions_is_linear() {
    const DEPTH: usize = 60;
    let mut types =
        String::from("type L0 = [null, Error<`E>, `A];\ntype M0 = [null, `B];\n");
    for i in 1..=DEPTH {
        let p = i - 1;
        types
            .push_str(&format!("type L{i} = [L{p}, M{p}];\ntype M{i} = [L{p}, M{p}];\n"));
    }
    let value = |value: &str| format!("let x: L{DEPTH} = {value};\nx?\n");
    for (program, shown) in [
        (value("\"foo\""), "string"),
        (value("{ let v: Any = 1; v }"), "Any"),
        (value("`C"), "`C"),
        (format!("let f = |x0: 'a| {{ let x: L{DEPTH} = x0; x }};\nf(1)\n"), "'a"),
    ] {
        let program = format!("{types}{program}");
        let out = exits_within(
            &["--check", "--no-cache"],
            &[("program.gx", &program)],
            10 * BILLION,
        );
        let stderr = String::from_utf8_lossy(&out.stderr);
        let refusal = format!("type mismatch L{DEPTH} does not contain {shown}");
        assert!(!out.status.success() && stderr.contains(&refusal), "{stderr}");
    }
}

/// A nest of function types with variables of their own, through their
/// parameters, their bounds or their results, in a `let`'s annotation or
/// below an interface `val`'s top, is refused in time linear in its depth:
/// each is refused before its bounds are read, which walks the nest below
/// it.
#[test]
#[cfg(target_os = "linux")]
fn a_nest_of_quantified_function_types_is_refused_in_linear_time() {
    const DEPTH: usize = 800;
    let nest = |leaf: &str, wrap: &dyn Fn(&str) -> String| {
        (1..DEPTH).fold(leaf.to_string(), |s, _| wrap(&s))
    };
    let through_parameters = format!(
        "fn<'z: Any>(x: {}) -> 'z",
        nest("fn<'a: Any>(x: 'z) -> 'a", &|s| format!(
            "fn<'a: Any>(x: {s}, y: 'z) -> 'a"
        ))
    );
    let leaf = "fn<'a: Any>(x: 'a) -> 'a";
    let through_bounds = nest(leaf, &|s| format!("fn<'a: {s}>(x: 'a) -> 'a"));
    let through_results = nest(leaf, &|s| format!("fn<'a: Any>(x: 'a) -> {s}"));
    let refused = |files: &[(&str, &str)]| {
        let out = exits_within(&["--check", "--no-cache"], files, 2 * BILLION);
        let stderr = String::from_utf8_lossy(&out.stderr);
        assert!(
            !out.status.success() && stderr.contains("type variables of its own"),
            "{stderr}"
        );
    };
    for t in [&through_parameters, &through_bounds, &through_results] {
        refused(&[("program.gx", &format!("let y: {t} = never()\n"))]);
    }
    refused(&[
        ("main.gx", "mod m;\nm::y\n"),
        ("m.gxi", &format!("val y: {through_parameters};\n")),
        ("m.gx", "let y = never()\n"),
    ]);
}

/// A call whose parameter or result is a DAG of parametrized named unions
/// (`L<i><'t> = [L<i-1><'t>, M<i-1><'t>]`, `M<i>` the same), and a declared
/// variable refused against one, check and elaborate in time polynomial in
/// the DAG's depth, where its paths are 2^depth: the walks of a signature's
/// positions and of a union's members take each application once.
#[test]
#[cfg(target_os = "linux")]
fn a_dag_of_parametrized_named_unions_checks_in_polynomial_time() {
    const DEPTH: usize = 40;
    let mut types =
        String::from("type L0<'t> = ['t, null, `A];\ntype M0<'t> = [Array<'t>, `B];\n");
    for i in 1..=DEPTH {
        let p = i - 1;
        types.push_str(&format!(
            "type L{i}<'t> = [L{p}<'t>, M{p}<'t>];\ntype M{i}<'t> = [L{p}<'t>, M{p}<'t>];\n"
        ));
    }
    for call in [
        format!("let f = |x0: L{DEPTH}<string>| 1;\nf(null)\n"),
        format!("let f = |x0: i64| -> L{DEPTH}<string> null;\nf(1)\n"),
    ] {
        completes_within(
            &["--expand", "--no-cache"],
            &format!("{types}{call}"),
            3 * BILLION,
        )
    }
    let refused = format!(
        "{types}let f = |x0: 'a, y0: 'b| {{ let x: L{DEPTH}<'b> = x0; x }};\nf(1, 2)\n"
    );
    let out = exits_within(
        &["--check", "--no-cache"],
        &[("program.gx", &refused)],
        3 * BILLION,
    );
    let stderr = String::from_utf8_lossy(&out.stderr);
    let refusal = format!("type mismatch L{DEPTH}<'b> does not contain 'a");
    assert!(!out.status.success() && stderr.contains(&refusal), "{stderr}");
}

/// A call whose signature holds typedefs that name each other, applied
/// over open variables (every `T<i><'a, 'm>` names every `T<j>`, in both
/// orders of its parameters), checks and elaborates in time polynomial in
/// their number, where the paths through them are its factorial: the walks
/// of a signature's positions pass an application met again by.
#[test]
#[cfg(target_os = "linux")]
fn typedefs_naming_each_other_over_open_variables_check_in_polynomial_time() {
    const DEFS: usize = 12;
    let mut types = String::new();
    for i in 0..DEFS {
        let mut members = format!("`L{i}('a)");
        for j in 0..DEFS {
            members.push_str(&format!(", `V{i}_{j}(T{j}<'a, 'm>, T{j}<'m, 'a>)"));
        }
        types.push_str(&format!("type T{i}<'a, 'm> = [{members}];\n"));
    }
    for call in [
        "let f = |x: 'a| -> T0<'a, 'm> `L0(x);\nf(0)\n",
        "let f = |x: 'a, y: T0<'a, 'm>| -> T0<'a, 'm> `L0(x);\nf(0, `L0(1))\n",
    ] {
        completes_within(
            &["--expand", "--no-fusion", "--no-cache"],
            &format!("{types}{call}"),
            3 * BILLION,
        )
    }
}

/// A call whose signature holds `D40<'b, 'a>` and then `W<'b, 'a>`,
/// where the chain under `D40` ends at `W<'a, 'a>` and `W` names `D40`,
/// checks and elaborates in time linear in the chain, each link of which
/// names the one below twice: an application walked outside `W` and met
/// again inside it is walked once more there, and that walk is the one
/// kept.
#[test]
#[cfg(target_os = "linux")]
fn an_application_met_again_inside_a_definition_it_expanded_is_walked_once_there() {
    const LINKS: usize = 40;
    let mut types = format!(
        "type W<'p, 'q> = [`W('p), `WD(D{LINKS}<'p, 'q>), null];\n\
         type D1<'p, 'q> = [`L1('p), `P1(W<'q, 'q>), `Q1(W<'q, 'q>), null];\n"
    );
    for i in 2..=LINKS {
        let below = i - 1;
        types.push_str(&format!(
            "type D{i}<'p, 'q> = \
             [`L{i}('p), `P{i}(D{below}<'p, 'q>), `Q{i}(D{below}<'p, 'q>), null];\n"
        ));
    }
    let call = format!(
        "let f = |x: D{LINKS}<'b, 'a>, y: W<'b, 'a>, z: 'a| z;\nf(null, null, 1)\n"
    );
    completes_within(
        &["--expand", "--no-fusion", "--no-cache"],
        &format!("{types}{call}"),
        3 * BILLION,
    )
}

/// Layers of two typedefs, each naming both of the layer below, the
/// lowest naming a typedef that names the first layer again, check and
/// elaborate in time linear in the layers, where the walk meets the
/// lowest inside a different set of open definitions for each choice of
/// one a layer: what it holds is told as held under a function at the
/// first of them, and it is passed by at the rest.
#[test]
#[cfg(target_os = "linux")]
fn layers_of_typedefs_over_a_recursive_one_check_in_polynomial_time() {
    const LAYERS: usize = 24;
    let mut types =
        "type X<'y> = [`X('y), `XA(A1<'y, 'y>), `XB(B1<'y, 'y>), null];\n".to_string();
    for i in 1..=LAYERS {
        for n in ["A", "B"] {
            let below = match i == LAYERS {
                true => format!("`{n}{i}x(X<'y>)"),
                false => {
                    let j = i + 1;
                    format!("`{n}{i}a(A{j}<'x, 'y>), `{n}{i}b(B{j}<'x, 'y>)")
                }
            };
            types.push_str(&format!(
                "type {n}{i}<'x, 'y> = [`{n}{i}('x), {below}, null];\n"
            ));
        }
    }
    completes_within(
        &["--expand", "--no-fusion", "--no-cache"],
        &format!("{types}let f = |p: A1<'b, 'a>, z: 'a| z;\nf(null, 1)\n"),
        3 * BILLION,
    )
}

/// A type test for one recursive union over a value of another that
/// shares a leaf with it, each returning to itself through `Error` or
/// `Array`, checks in the time of a program without it: the difference
/// the arm leaves is built of the unions' names, and the walks over it
/// (the runtime-form check, the narrowing) meet each pair of them once.
#[test]
#[cfg(target_os = "linux")]
fn a_type_test_over_two_recursive_unions_checks_in_time() {
    for through in ["Error", "Array"] {
        let program = format!(
            "type T0 = [{through}<[T0, `A1]>, {through}<T0>, `A0];\n\
            type U0 = [{through}<[U0, U1]>, {through}<U1>, `A0];\n\
            type U1 = [{through}<U1>, `B1];\n\
            let x: T0 = `A0;\n\
            select x {{ U0 as u0 => 0, _ => 999 }}\n"
        );
        checks_within(&program, 3 * BILLION)
    }
}

/// Arms that test one family of recursive unions over a value of another,
/// each taking a part of what reaches it through `Error` payloads, check
/// in the time of a program without them, accepted or refused: what an
/// arm leaves nests a few Errors over the recursion, so the next arm's
/// walk and the refusal's message read a small type.
#[test]
#[cfg(target_os = "linux")]
fn type_tests_over_two_families_of_recursive_unions_check_in_time() {
    let four_a_side = "\
        type T0 = [Error<T2>, i64, Error<[T3, T0]>, `A0];\n\
        type T1 = [string, Error<T0>, `A1];\n\
        type T2 = [string, Error<T3>, Error<T2>, Array<T3>, `A2];\n\
        type T3 = [`P0(T1), Error<T2>, (T3, T1), Error<Error<T3>>, `A3];\n\
        type U0 = [Error<U2>, i64, Error<[U3, U0]>, `B0, null];\n\
        type U1 = [string, Error<U0>, `B1];\n\
        type U2 = [string, Error<U3>, Error<U2>, Array<U3>, `B2];\n\
        type U3 = [`P0(U1), (U3, U1), Error<Error<U3>>, `B3];\n\
        let x: T0 = error(`A0);\n\
        select x {\n\
            U1 as u0 => 0,\n\
            Error<U2> as u1 => 1,\n\
            U3 as u2 => 2,\n\
            U2 as u3 => 3,\n\
            U0 as u4 => 4,\n\
            _ => 999\n\
        }\n";
    checks_within(four_a_side, 3 * BILLION);
    // a test an arm above made is dead, whatever the arms between took
    let a_test_again = four_a_side.replace("U2 as u3", "U1 as u3");
    let through_arrays = "\
        type T0 = [Array<T1>, Error<T1>, Error<T0>, `A0];\n\
        type T1 = [Array<T1>, (T1, T0), Error<T1>, `A1];\n\
        type T2 = [Error<T2>, Error<[T1, T2]>, Array<T0>, `A2];\n\
        type U0 = [Array<U1>, Error<U1>, Error<U0>, `A0];\n\
        type U1 = [(U1, U0), Error<U1>, `A1];\n\
        type U2 = [Error<[U1, U2]>, Array<U0>, `A2];\n\
        let x: T2 = `A2;\n\
        select x { U2 as u0 => 0, Error<U1> as u1 => 1 }\n";
    let one_family = "\
        type T0 = [Error<T2>, Error<T3>, Error<T0>, `A0];\n\
        type T1 = [i64, (T1, T3), (T0, T3), Error<T2>, `A1];\n\
        type T2 = [Error<[T2, T2]>, Array<T2>, `P2(T0), `A2];\n\
        type T3 = [Error<T0>, Error<[T0, T2]>, `A3];\n\
        let x: T0 = never();\n\
        select x { T3 as u0 => 0, `A2 => 1, _ => 999 }\n";
    for (program, refusal) in [
        (through_arrays, "missing match cases: nothing matches"),
        (one_family, "pattern `A2 will never match"),
        (&*a_test_again, "pattern U1 will never match"),
    ] {
        let out = exits_within(
            &["--check", "--no-cache"],
            &[("program.gx", program)],
            3 * BILLION,
        );
        let stderr = String::from_utf8_lossy(&out.stderr);
        assert!(!out.status.success() && stderr.contains(refusal), "{stderr}");
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
            .env("XDG_CACHE_HOME", cache.path().join(format!("{shape}-{depth}-{mode}")))
            .stdin(Stdio::null())
            .stdout(Stdio::null())
            // CR claude for claude: [readability/low] A failing child's message is
            // thrown away. `run_child` prints the error ("{shape}@{depth} {mode}:
            // ..."), and a stack overflow prints its own line, both to this null
            // stderr, so the parent's report says only "Some(5), not Some(0)" or
            // "None". Pipe stderr and put each failing child's text in its `failed`
            // entry. Found by reading; not run. (tests-shell-compiler-a-09)
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
        .chain([("rigidchain", CHAIN_DEPTH, "check")])
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
#[cfg(target_os = "linux")]
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
    completes_within(&["--no-cache", "--no-fusion"], &source, 40 * BILLION);
}

/// A select over an unannotated parameter whose arms test one part of a
/// tuple by a literal and bind another takes time quadratic in its arms,
/// each pair of arms' predicates walked once; the walk of a pair must be
/// cheap (1.8 G at 200 arms, beside the 0.75 G of an empty program).
#[test]
#[cfg(target_os = "linux")]
fn select_over_tuple_arms_binding_a_name_checks_in_time() {
    let mut source: LPooled<String> = LPooled::take();
    source.push_str("let f = |s| select s {\n");
    for i in 0..200 {
        writeln!(source, "({i}, a) => {i},").unwrap();
    }
    source.push_str("(_, _) => 0\n}");
    checks_within(&source, 2 * BILLION);
}

/// A select whose arms pair a null part with a bind part for each key
/// (`` `A(k, null) => k, `A(k, x) => 0 ``): the bound cells are one cell
/// each, so the union of them is walked once a pair of arms, not merged
/// member against member (3.8 G at 200 pairs, 9.5 G before).
#[test]
#[cfg(target_os = "linux")]
fn select_over_variant_arms_pairing_null_and_a_name_checks_in_time() {
    let mut source: LPooled<String> = LPooled::take();
    source.push_str("let g = |s| select s {\n");
    for i in 0..200 {
        writeln!(source, "`A({i}, null) => {i},").unwrap();
        writeln!(source, "`A({i}, x) => 0,").unwrap();
    }
    source.push_str("_ => 0\n}");
    checks_within(&source, 5 * BILLION);
}

/// [`select_over_variant_arms_pairing_null_and_a_name_checks_in_time`] over
/// tuples, whose members the union holds apart (8.7 G at 200 pairs, 14.5 G
/// before).
#[test]
#[cfg(target_os = "linux")]
fn select_over_tuple_arms_pairing_null_and_a_name_checks_in_time() {
    let mut source: LPooled<String> = LPooled::take();
    source.push_str("let g = |s| select s {\n");
    for i in 0..200 {
        writeln!(source, "({i}, null) => {i},").unwrap();
        writeln!(source, "({i}, x) => 0,").unwrap();
    }
    source.push_str("_ => 0\n}");
    checks_within(&source, 10 * BILLION);
}
