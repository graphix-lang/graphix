//! The old parser against the new: every input parsed by both, compared
//! by `Eq`, by a dump of everything `Eq` ignores, by the AsWritten print
//! and, for a failure, by its message.
use crate::{
    PrintFlag,
    expr::{Expr, Origin, Sig, SigKind, Source, parser, test as gens},
    format_with_flags,
};
use arcstr::ArcStr;
use proptest::{prelude::*, strategy::ValueTree, test_runner::TestRunner};
use std::{fmt::Write as _, path::PathBuf};

fn walk(e: &Expr, out: &mut String) {
    writeln!(out, "{:?} {:?} {:?} {:?}", e.pos, e.end.0, e.str_form, e.dec).unwrap();
    if let Some(d) = &e.dec {
        for a in d.attrs.iter() {
            for x in a.args.iter() {
                walk(x, out)
            }
        }
    }
    e.for_each_child(&mut |c| walk(c, out));
}

/// `dump` without the source text and the ids each parse mints anew.
fn masked(text: &str, dump: String) -> String {
    let dump = dump.replace(&format!("{text:?}"), "<TEXT>");
    let mut out = String::with_capacity(dump.len());
    let mut rest = &*dump;
    while let Some(i) = rest.find("Id(") {
        out.push_str(&rest[..i + 3]);
        rest = rest[i + 3..].trim_start_matches(|c: char| c.is_ascii_digit());
    }
    out.push_str(rest);
    out
}

/// Everything parsed, positions, comments and string forms included.
fn dump_exprs(text: &str, es: &[Expr]) -> String {
    let mut out = String::new();
    for e in es {
        writeln!(out, "{:?}", e.kind).unwrap();
        walk(e, &mut out);
    }
    masked(text, out)
}

fn dump_sig(text: &str, s: &Sig) -> String {
    let mut out = format!("{s:?}\n");
    for item in s.items.iter() {
        if let SigKind::Trait(t) = &item.kind {
            for m in t.methods.iter() {
                if let Some(d) = &m.default {
                    walk(d, &mut out)
                }
            }
        }
    }
    masked(text, out)
}

fn as_written(f: impl FnOnce() -> String) -> String {
    format_with_flags(PrintFlag::AsWritten, f)
}

/// A parse's comparable faces: the value's `Eq` key, its dump and its
/// AsWritten print, or the failure's message.
enum Face<T> {
    Ok(T, String, String),
    Err(String),
}

fn face<T>(
    r: anyhow::Result<T>,
    dump: impl Fn(&T) -> String,
    print: impl Fn(&T) -> String,
) -> Face<T> {
    match r {
        Ok(v) => {
            let (d, p) = (dump(&v), as_written(|| print(&v)));
            Face::Ok(v, d, p)
        }
        Err(e) => Face::Err(format!("{e:#}")),
    }
}

/// How two faces differ, if they do.
fn differ<T: PartialEq>(old: &Face<T>, new: &Face<T>) -> Option<&'static str> {
    match (old, new) {
        (Face::Ok(a, da, pa), Face::Ok(b, db, pb)) => {
            if a != b {
                Some("ast")
            } else if da != db {
                Some("dump")
            } else if pa != pb {
                Some("print")
            } else {
                None
            }
        }
        (Face::Err(a), Face::Err(b)) if a == b => None,
        (Face::Err(a), Face::Err(b)) => match listless(a) == listless(b) {
            true => Some("lists"),
            false => Some("error"),
        },
        (Face::Ok(..), Face::Err(_)) => Some("new-rejects"),
        (Face::Err(_), Face::Ok(..)) => Some("new-accepts"),
    }
}

/// A failure's message without what it lists as met and expected.
fn listless(msg: &str) -> String {
    let lists = |l: &&str| l.starts_with("Expected ") || l.starts_with("Unexpected ");
    msg.lines().filter(|l| !lists(l)).collect::<Vec<_>>().join("\n")
}

fn shown<T>(f: &Face<T>) -> String {
    match f {
        Face::Ok(_, d, p) => format!("OK\n--- print\n{p}\n--- dump\n{d}"),
        Face::Err(e) => format!("ERR\n{e}"),
    }
}

/// One input's comparison: its class of difference, and a report.
fn compare_text(kind: &str, text: &str) -> (&'static str, Option<String>) {
    let ori = |t: &str| Origin {
        parent: None,
        source: Source::Vfs(arcstr::literal!("/diff")),
        text: ArcStr::from(t),
    };
    macro_rules! both {
        ($old:expr, $new:expr, $dump:expr, $print:expr) => {{
            let old = face($old, $dump, $print);
            let new = face($new, $dump, $print);
            let class = differ(&old, &new);
            let ok = matches!(old, Face::Ok(..));
            let report = class.map(|c| {
                format!(
                    "class: {c}\nkind: {kind}\n=== input\n{text}\n=== old\n{}\n=== new\n{}",
                    shown(&old),
                    shown(&new)
                )
            });
            (class.unwrap_or(if ok { "equal-ok" } else { "equal-err" }), report)
        }};
    }
    let print_exprs = |es: &std::sync::Arc<[Expr]>| {
        es.iter().map(|e| format!("{e}")).collect::<Vec<_>>().join(";\n")
    };
    match kind {
        "gxi" => both!(
            parser::parse_sig(ori(text)),
            super::parse_sig(ori(text)),
            |s: &Sig| dump_sig(text, s),
            |s: &Sig| format!("{s}")
        ),
        "type" => both!(
            parser::parse_type(text),
            super::parse_type(text),
            |t| masked(text, format!("{t:?}")),
            |t| format!("{t}")
        ),
        "fntype" => both!(
            parser::parse_fn_type(text),
            super::parse_fn_type(text),
            |t| masked(text, format!("{t:?}")),
            |t| format!("{t}")
        ),
        "lit" => both!(
            parser::parse_literal_value(text).ok_or_else(|| anyhow::anyhow!("none")),
            super::parse_literal_value(text).ok_or_else(|| anyhow::anyhow!("none")),
            |v| format!("{v:?}"),
            |v| format!("{v}")
        ),
        "path" => both!(
            parser::parse_path(text),
            super::parse_path(text),
            |p| format!("{p:?}"),
            |p| format!("{p}")
        ),
        "one" => both!(
            parser::parse_one(text),
            super::parse_one(text),
            |e: &Expr| dump_exprs(text, std::slice::from_ref(e)),
            |e: &Expr| format!("{e}")
        ),
        _ => {
            let old =
                parser::parse(ori(text)).map(|a| std::sync::Arc::<[Expr]>::from(&*a));
            let new =
                super::parse(ori(text)).map(|a| std::sync::Arc::<[Expr]>::from(&*a));
            both!(old, new, |es| dump_exprs(text, es), print_exprs)
        }
    }
}

/// Tally the classes, write each difference to `out`.
struct Tally {
    out: Option<PathBuf>,
    counts: std::collections::BTreeMap<&'static str, usize>,
    n: usize,
}

impl Tally {
    fn new() -> Self {
        let out = std::env::var_os("GX_DIFF_OUT").map(PathBuf::from);
        if let Some(o) = &out {
            std::fs::create_dir_all(o).unwrap();
        }
        Self { out, counts: Default::default(), n: 0 }
    }

    fn add(&mut self, kind: &str, text: &str) {
        let (class, report) = compare_text(kind, text);
        *self.counts.entry(class).or_default() += 1;
        if let Some(r) = report {
            self.n += 1;
            match &self.out {
                Some(o) => {
                    std::fs::write(o.join(format!("{class}-{:05}.txt", self.n)), r)
                        .unwrap()
                }
                None if self.n <= 5 => eprintln!("{r}\n"),
                None => (),
            }
        }
    }

    fn finish(&self, what: &str) {
        eprintln!("{what}: {:?}", self.counts);
        let same = |c: &str| c.starts_with("equal") || c == "lists";
        let differing: usize =
            self.counts.iter().filter(|(c, _)| !same(c)).map(|(_, n)| n).sum();
        assert_eq!(differing, 0, "{what}: {:?}", self.counts);
    }
}

/// The kind of input a file holds, by its name.
fn kind_of(path: &str) -> &str {
    path.rsplit('.').next().unwrap_or("gx")
}

/// Every file `GX_DIFF_LIST` names: a `.gxi` an interface, a
/// `.lines.<kind>` one input of `kind` per line, any other a program.
#[test]
#[ignore = "differential harness: GX_DIFF_LIST"]
fn corpus() {
    let list = std::env::var("GX_DIFF_LIST").expect("GX_DIFF_LIST");
    let mut tally = Tally::new();
    for path in std::fs::read_to_string(&list).unwrap().lines().filter(|l| !l.is_empty())
    {
        let Ok(bytes) = std::fs::read(path) else { continue };
        let text = String::from_utf8_lossy(&bytes);
        if path.contains(".lines.") {
            let kind = kind_of(path);
            for line in text.lines().filter(|l| !l.is_empty()) {
                tally.add(kind, line)
            }
        } else {
            tally.add(if kind_of(path) == "gxi" { "gxi" } else { "gx" }, &text)
        }
    }
    tally.finish(&list)
}

/// Random trees, printed, parsed by both; `GX_DIFF_CASES` of each kind.
#[test]
#[ignore = "differential harness: random trees"]
fn generated() {
    let cases =
        std::env::var("GX_DIFF_CASES").ok().and_then(|s| s.parse().ok()).unwrap_or(256);
    let mut tally = Tally::new();
    let mut runner = TestRunner::deterministic();
    macro_rules! each {
        ($kind:literal, $strategy:expr, $print:expr) => {
            for _ in 0..cases {
                let v = $strategy.new_tree(&mut runner).unwrap().current();
                let text: String = $print(&v);
                tally.add($kind, &text);
            }
        };
    }
    each!("one", gens::expr(), |e: &Expr| as_written(|| format!("{e}")));
    each!("gx", gens::expr(), |e: &Expr| format!("{e}"));
    each!("type", gens::typexp(), |t: &crate::typ::Type| format!("{t}"));
    each!(
        "fntype",
        gens::trait_method_sig(),
        |t: &triomphe::Arc<crate::typ::FnType>| format!("{t}")
    );
    each!("lit", gens::value(), |v: &netidx_value::Value| format!("{v}"));
    each!("path", gens::modpath(), |p: &crate::expr::ModPath| format!("{p}"));
    each!("gxi", gens::module_sig(true), |s: &Sig| as_written(|| format!("{s}")));
    tally.finish("generated")
}

/// The dump sees each carrier `Eq` ignores: a moved position, an end, a
/// comment and a nested string's form each change it, and leave the
/// tree `Eq`.
#[test]
fn the_dump_sees_every_carrier() {
    use crate::expr::{Comment, Decorations, ExprKind, StrForm, WrittenAt};
    let text = "// c\nlet x = r\"a\";\nlet f = |#a: i64| a + 1;\n[x, f(#a: 2)]";
    let es: Vec<Expr> =
        parser::parse(Origin::unspecified(text)).unwrap().iter().cloned().collect();
    let base = dump_exprs(text, &es);
    let changed = |f: &dyn Fn(&mut Expr)| {
        let mut changed = es.clone();
        f(&mut changed[0]);
        assert!(changed == es);
        dump_exprs(text, &changed)
    };
    assert_ne!(base, changed(&|e| e.pos.column += 1));
    assert_ne!(base, changed(&|e| e.end = WrittenAt::NOWHERE));
    assert_ne!(
        base,
        changed(&|e| {
            let mut d = e.dec.as_deref().cloned().unwrap_or_else(Decorations::default);
            d.comments =
                triomphe::Arc::from_iter([Comment::line(arcstr::literal!(" d"))]);
            e.dec = Some(triomphe::Arc::new(d));
        })
    );
    assert_ne!(
        base,
        changed(&|e| {
            if let ExprKind::Bind(b) = &mut e.kind {
                triomphe::Arc::make_mut(b).value.str_form = StrForm::Quoted
            }
        })
    );
}
