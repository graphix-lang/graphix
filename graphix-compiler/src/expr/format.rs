use crate::{
    PrintFlag,
    expr::{
        Decorations, Expr, ExprKind, ModuleKind, Origin, Sig, SigItem, SigKind, StrForm,
        TraitExpr, TraitMethod, TryWithExpr, UseItem,
        parser::{parse, parse_sig},
        print::{
            PrettyBuf, PrettyDisplay, cmp_use_items, pretty_file_items, use_seg,
            use_seg_key,
        },
    },
    format_with_flags,
    stack::ensure_sufficient,
};
use anyhow::{Context, Result, bail};
use arcstr::ArcStr;
use compact_str::{CompactString, format_compact};
use smallvec::SmallVec;
use netidx_value::Value;
use poolshark::local::LPooled;
use serde_derive::Deserialize;
use std::{fmt, fs, path::Path};
use triomphe::Arc;

pub const DEFAULT_WIDTH: usize = 90;

/// The spaces one level of nesting indents by, unless configured.
pub const DEFAULT_INDENT: usize = 4;

/// The name of the formatter's configuration file.
pub const CONFIG_FILE: &str = "graphixfmt.json";

/// What `graphixfmt.json` can say; a field it leaves out keeps its default.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize)]
#[serde(default, deny_unknown_fields)]
pub struct FormatConfig {
    /// the line width to fit
    pub width: usize,
    /// the spaces one level of nesting indents by
    pub indent: usize,
}

impl Default for FormatConfig {
    fn default() -> Self {
        Self { width: DEFAULT_WIDTH, indent: DEFAULT_INDENT }
    }
}

impl FormatConfig {
    fn load(path: &Path) -> Result<Self> {
        let text = fs::read_to_string(path)
            .with_context(|| format_compact!("reading {}", path.display()))?;
        serde_json::from_str(&text)
            .with_context(|| format_compact!("parsing {}", path.display()))
    }

    /// The configuration that governs a source file in `dir`: the nearest
    /// `graphixfmt.json` in `dir` or above it (a project keeps one at its
    /// base), else the user's, in the platform's configuration directory
    /// under `graphix`, else the defaults. A file that is found and does
    /// not parse is an error, never the defaults.
    pub fn discover(dir: &Path) -> Result<Self> {
        let dir = std::path::absolute(dir)
            .with_context(|| format_compact!("resolving {}", dir.display()))?;
        let project = dir.ancestors().map(|d| d.join(CONFIG_FILE)).find(|p| p.is_file());
        let user = || dirs::config_dir().map(|d| d.join("graphix").join(CONFIG_FILE));
        match project.or_else(|| user().filter(|p| p.is_file())) {
            Some(path) => Self::load(&path),
            None => Ok(Self::default()),
        }
    }
}

/// The formatter would not hand back its own output: a bug in it, never
/// in the source, which is left as it was.
#[derive(Debug)]
pub struct Refused(ArcStr);

impl fmt::Display for Refused {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "formatter bug: {}", self.0)
    }
}

impl std::error::Error for Refused {}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SourceKind {
    /// a `.gx` program or module
    Program,
    /// a `.gxi` interface
    Interface,
}

impl SourceKind {
    pub fn of_path(path: &Path) -> Self {
        match path.extension().and_then(|e| e.to_str()) {
            Some("gxi") => Self::Interface,
            _ => Self::Program,
        }
    }
}

/// `items` with each run of adjacent use statements rewritten as one
/// statement per path root and visibility, statements and names sorted.
/// `as_use` is `None` for anything else, a decorated use included.
fn merge_uses<T: Clone>(
    items: &[T],
    as_use: impl Fn(&T) -> Option<(bool, &Arc<[UseItem]>)>,
    mk: impl Fn(&T, bool, Arc<[UseItem]>) -> T,
) -> Arc<[T]> {
    fn root(n: &UseItem) -> (u8, &str) {
        use_seg_key(use_seg(n, 0))
    }
    /// The name a use item binds; a glob binds any.
    fn binds(n: &UseItem) -> Option<&str> {
        n.rename.as_ref().map(|n| n.as_str()).or_else(|| netidx_core::path::Path::basename(&n.path.0))
    }
    /// The name a use item's path starts from, when it is not a keyword.
    fn reads(n: &UseItem) -> Option<&str> {
        use_seg(n, 0).filter(|r| !matches!(*r, "self" | "super" | "package"))
    }
    /// Would `names`, merged after `run`, read or shadow what `run` binds,
    /// or bind what `run` reads? The sort could reorder them. A glob binds
    /// names unknown here, so it joins only items of its own root.
    fn depends(run: &[(bool, UseItem)], names: &[UseItem]) -> bool {
        names.iter().any(|n| {
            run.iter().any(|(_, m)| {
                if m.is_glob() || n.is_glob() {
                    return root(m) != root(n);
                }
                let same = m.path == n.path && m.rename == n.rename;
                binds(m) == reads(n) || binds(n) == reads(m) || (binds(m) == binds(n) && !same)
            })
        })
    }
    let mut merged: LPooled<Vec<T>> = LPooled::take();
    let mut run: LPooled<Vec<(bool, UseItem)>> = LPooled::take();
    let mut i = 0;
    while i < items.len() {
        let first = &items[i];
        while let Some((reexport, names)) = items.get(i).and_then(&as_use) {
            if !run.is_empty() && depends(&run, names) {
                break;
            }
            run.extend(names.iter().map(|n| (reexport, n.clone())));
            i += 1;
        }
        if run.is_empty() {
            merged.push(first.clone());
            i += 1;
            continue;
        }
        run.sort_by(|(ra, a), (rb, b)| {
            root(a).cmp(&root(b)).then(ra.cmp(rb)).then_with(|| cmp_use_items(a, b))
        });
        for stmt in run.chunk_by(|(ra, a), (rb, b)| ra == rb && root(a) == root(b)) {
            let names = UseItem::sorted(stmt.iter().map(|(_, n)| n.clone()));
            merged.push(mk(first, stmt[0].0, names))
        }
        run.clear()
    }
    Arc::from_iter(merged.drain(..))
}

/// `sig` with its use runs merged, and those of its trait defaults.
fn merge_sig_uses(sig: &Sig) -> Sig {
    let items = merge_uses(
        &sig.items,
        |si| match &si.kind {
            SigKind::Use { reexport, names }
                if si.doc.0.is_none() && si.comments.lines().is_empty() =>
            {
                Some((*reexport, names))
            }
            _ => None,
        },
        |si, reexport, names| SigItem {
            kind: SigKind::Use { reexport, names },
            ..si.clone()
        },
    );
    let items = Arc::from_iter(items.iter().map(|si| match &si.kind {
        SigKind::Trait(t) => SigItem { kind: SigKind::Trait(merge_trait_uses(t)), ..si.clone() },
        _ => si.clone(),
    }));
    Sig { items, toplevel: sig.toplevel }
}

fn merge_trait_uses(t: &TraitExpr) -> Arc<TraitExpr> {
    Arc::new(TraitExpr {
        name: t.name.clone(),
        methods: Arc::from_iter(t.methods.iter().map(|m| TraitMethod {
            default: m.default.as_ref().map(merge_uses_within),
            ..m.clone()
        })),
    })
}

fn merge_expr_uses(exprs: &[Expr]) -> Arc<[Expr]> {
    merge_uses(
        exprs,
        |e| match &e.kind {
            ExprKind::Use { reexport, names } if e.dec.is_none() => {
                Some((*reexport, names))
            }
            _ => None,
        },
        |e, reexport, names| {
            let mut stmt = Expr::new(ExprKind::Use { reexport, names }, e.pos);
            stmt.id = e.id;
            stmt.ori = e.ori.clone();
            stmt
        },
    )
}

/// `e` with the use statements of every statement list in it merged.
fn merge_uses_within(e: &Expr) -> Expr {
    use ExprKind::*;
    let e = ensure_sufficient(|| e.map_children(&mut merge_uses_within));
    let kind = match &e.kind {
        Do { exprs } => Do { exprs: merge_expr_uses(exprs) },
        Seq { queued, trigger, abort, flush, body } => Seq {
            queued: *queued,
            trigger: trigger.clone(),
            abort: abort.clone(),
            flush: flush.clone(),
            body: merge_expr_uses(body),
        },
        TryWith(t) => TryWith(Arc::new(TryWithExpr {
            body: merge_expr_uses(&t.body),
            handler: merge_expr_uses(&t.handler),
            ..(**t).clone()
        })),
        Module { name, value: ModuleKind::Dynamic { sandbox, sig, source } } => Module {
            name: name.clone(),
            value: ModuleKind::Dynamic {
                sandbox: sandbox.clone(),
                sig: merge_sig_uses(sig),
                source: source.clone(),
            },
        },
        _ => return e,
    };
    e.with_kind(kind)
}

enum Parsed {
    Program(Arc<[Expr]>),
    Interface(Sig),
}

impl Parsed {
    fn new(kind: SourceKind, text: &str) -> Result<Self> {
        let ori = Origin::unspecified(text);
        Ok(match kind {
            SourceKind::Program => {
                let exprs = parse(ori)?;
                let exprs: LPooled<Vec<Expr>> =
                    exprs.iter().map(merge_uses_within).collect();
                Self::Program(merge_expr_uses(&exprs))
            }
            SourceKind::Interface => Self::Interface(merge_sig_uses(&parse_sig(ori)?)),
        })
    }

    fn print(&self, buf: &mut PrettyBuf) -> Result<()> {
        match self {
            Self::Program(exprs) => pretty_file_items(buf, exprs, |buf, e| match e.kind {
                ExprKind::NoOp => Ok(()),
                _ => e.fmt_pretty(buf),
            })?,
            Self::Interface(sig) => sig.fmt_pretty_inner(buf)?,
        }
        Ok(())
    }

    /// What equality does not see that the formatter must hand back, in
    /// source order: every comment and attribute, and every string
    /// literal's delimiters.
    fn ornaments(&self) -> LPooled<Vec<Ornament<'_>>> {
        let mut acc: LPooled<Vec<Ornament>> = LPooled::take();
        match self {
            Self::Program(exprs) => exprs.iter().for_each(|e| expr_ornaments(e, &mut acc)),
            Self::Interface(sig) => sig_ornaments(sig, &mut acc),
        }
        acc
    }

    /// The first place `other` says something else, as the text each
    /// side prints there.
    fn difference(&self, other: &Self) -> Option<(CompactString, CompactString)> {
        fn smallest(a: &Expr, b: &Expr) -> (CompactString, CompactString) {
            let children = |e| {
                let mut acc: SmallVec<[&Expr; 8]> = SmallVec::new();
                Expr::for_each_child(e, &mut |c| acc.push(c));
                acc
            };
            let (ac, bc) = (children(a), children(b));
            if ac.len() == bc.len()
                && let Some((a, b)) = ac.iter().zip(bc.iter()).find(|(a, b)| a != b)
            {
                return ensure_sufficient(|| smallest(a, b));
            }
            (format_compact!("{}: {a}", a.pos), format_compact!("{}: {b}", b.pos))
        }
        fn count(what: &str, a: usize, b: usize) -> Option<(CompactString, CompactString)> {
            (a != b).then(|| (format_compact!("{a} {what}"), format_compact!("{b} {what}")))
        }
        match (self, other) {
            (Self::Program(a), Self::Program(b)) => {
                let differ = a.iter().zip(b.iter()).find(|(a, b)| a != b);
                differ
                    .map(|(a, b)| smallest(a, b))
                    .or_else(|| count("expressions", a.len(), b.len()))
            }
            (Self::Interface(a), Self::Interface(b)) => {
                let differ = a.items.iter().zip(b.items.iter()).find(|(a, b)| a != b);
                differ
                    .map(|(a, b)| (format_compact!("{a}"), format_compact!("{b}")))
                    .or_else(|| count("items", a.len(), b.len()))
            }
            (Self::Program(_), Self::Interface(_))
            | (Self::Interface(_), Self::Program(_)) => unreachable!(),
        }
    }
}

/// Something a parse keeps beside the syntax, for the formatter to hand
/// back.
#[derive(Debug, PartialEq)]
enum Ornament<'a> {
    Decorations(&'a Decorations),
    Comments(&'a [ArcStr]),
    Delimiters(StrForm),
}

fn expr_ornaments<'a>(e: &'a Expr, acc: &mut Vec<Ornament<'a>>) {
    ensure_sufficient(|| {
        if let Some(d) = &e.dec {
            acc.push(Ornament::Decorations(d))
        }
        match &e.kind {
            ExprKind::Constant(Value::String(_)) | ExprKind::StringInterpolate { .. } => {
                acc.push(Ornament::Delimiters(e.str_form))
            }
            ExprKind::Trait(t) => {
                for m in t.methods.iter() {
                    acc.push(Ornament::Comments(m.comments.lines()))
                }
            }
            ExprKind::Module { value: ModuleKind::Dynamic { sig, .. }, .. } => {
                sig_ornaments(sig, acc)
            }
            _ => (),
        }
        e.for_each_child(&mut |c| expr_ornaments(c, acc))
    })
}

fn sig_ornaments<'a>(sig: &'a Sig, acc: &mut Vec<Ornament<'a>>) {
    for si in sig.items.iter() {
        acc.push(Ornament::Comments(si.comments.lines()));
        if let SigKind::Trait(t) = &si.kind {
            for m in t.methods.iter() {
                acc.push(Ornament::Comments(m.comments.lines()));
                if let Some(d) = &m.default {
                    expr_ornaments(d, acc)
                }
            }
        }
    }
}

fn layout(
    kind: SourceKind,
    text: &str,
    cfg: &FormatConfig,
) -> Result<(Parsed, PrettyBuf)> {
    let parsed = Parsed::new(kind, text)?;
    let mut buf = PrettyBuf::new(*cfg);
    format_with_flags(PrintFlag::AsWritten, || parsed.print(&mut buf))?;
    Ok((parsed, buf))
}

/// `format_source` without the reparse: for debugging the formatter.
pub fn format_source_unchecked(
    kind: SourceKind,
    text: &str,
    cfg: &FormatConfig,
) -> Result<LPooled<String>> {
    Ok(layout(kind, text, cfg)?.1.into_string())
}

/// `text` laid out canonically. The result is reparsed and refused unless
/// it says exactly what `text` said, comments and attributes included.
pub fn format_source(
    kind: SourceKind,
    text: &str,
    cfg: &FormatConfig,
) -> Result<LPooled<String>> {
    let (parsed, buf) = layout(kind, text, cfg)?;
    let reparsed = match Parsed::new(kind, buf.as_str()) {
        Ok(p) => p,
        Err(e) => {
            let msg = format_compact!("the formatted text does not parse: {e:#}");
            bail!(Refused(msg.as_str().into()))
        }
    };
    if let Some((was, now)) = parsed.difference(&reparsed) {
        let msg = format_compact!("the formatted text says something else\nwas {was}\nnow {now}");
        bail!(Refused(msg.as_str().into()))
    }
    let (was, now) = (parsed.ornaments(), reparsed.ornaments());
    if *was != *now {
        let lost = was.iter().zip(now.iter()).find(|(a, b)| a != b);
        let msg = match lost.map(|(a, _)| a).or_else(|| was.get(now.len())) {
            Some(Ornament::Delimiters(_)) => "the formatted text changed a string's delimiters",
            _ => "the formatted text lost a comment or an attribute",
        };
        bail!(Refused(arcstr::ArcStr::from(msg)))
    }
    Ok(buf.into_string())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn stable(kind: SourceKind, src: &str) {
        for width in [0, DEFAULT_WIDTH] {
            let cfg = FormatConfig { width, ..FormatConfig::default() };
            let once = format_source(kind, src, &cfg).unwrap();
            let twice = format_source(kind, &once, &cfg).unwrap();
            assert_eq!(*once, *twice, "not idempotent at width {width}")
        }
    }

    #[test]
    fn seq_trigger_reads_back_as_written() {
        stable(SourceKind::Program, "seqq let (a, b) = (x$, y) { f(a, b)? }");
        stable(
            SourceKind::Program,
            "seq let c = select r { `C(c) => c, _ => never() } { g(c)? }",
        );
        stable(SourceKind::Program, "seq a + b { h()? }");
        stable(SourceKind::Program, "seq (m{k}) { h()? }");
        stable(SourceKind::Program, "seq ({ a }) { h()? }");
        stable(SourceKind::Program, "seq (flush(x)) { h()? }");
    }

    #[test]
    fn template_string_brackets_and_splices() {
        stable(SourceKind::Program, r#"println("\nneeds [req.what] for a\[0\]")"#);
        stable(SourceKind::Program, "println(\"\"\"a [plain] \\[x] \\\\[y]\nb\"\"\")");
    }

    #[test]
    fn applied_self_receiver() {
        let src = "trait Coll { val map: fn(self<'a>, f: fn(x: 'a) -> 'b) -> self<'b> }";
        stable(SourceKind::Interface, src);
        stable(SourceKind::Program, src);
    }

    fn formats_to(kind: SourceKind, src: &str, want: &str) {
        assert_eq!(&**format_source(kind, src, &FormatConfig::default()).unwrap(), want);
        stable(kind, src)
    }

    #[test]
    fn uses_merge_at_every_level() {
        use SourceKind::*;
        formats_to(
            Program,
            "use foo::{bar::baz, bar::zee}",
            "use foo::bar::{baz, zee}\n",
        );
        formats_to(
            Program,
            "use b::y; use a::c::{e, d}; pub use a::p; use a::c; use a::*; use super::q; x",
            "use a::c::{self, d, e};\npub use a::p;\nuse b::y;\nuse a::*;\nuse super::q;\nx\n",
        );
        formats_to(Program, "use a::x as y; use a::x::z", "use a::x::{self as y, z}\n");
        formats_to(
            Program,
            "use a::*; use a::*; use a::b; use a::b; use a::b::c",
            "use a::{b, b::{self, c}, *, *}\n",
        );
        formats_to(
            Program,
            "use b::x;\n// why\nuse a::x",
            "use b::x;\n\n// why\nuse a::x\n",
        );
        formats_to(Program, "{ use b::x; use a::y; y }", "{ use a::y; use b::x; y }\n");
        // a statement reading or shadowing what an earlier one bound
        // closes the run: those stay in order
        formats_to(
            Program,
            "use z::x as a; use a::v; use b::c; use q::*; use r::s; use r::t; v",
            "use z::x as a;\nuse a::v;\nuse b::c;\nuse q::*;\nuse r::{s, t};\nv\n",
        );
        formats_to(
            Program,
            "use b::v; use a::v; use c::w; v",
            "use b::v;\nuse a::v;\nuse c::w;\nv\n",
        );
        formats_to(
            Interface,
            "use b::x; use a::{z, y}; val v: i64",
            "use a::{y, z};\nuse b::x;\nval v: i64\n",
        );
        let long = "use tui::{line, span, style, block::block, input_handler::{Event, on_press}, layout::{child, layout}, list::list}";
        formats_to(
            Program,
            long,
            "use tui::{\n    block::block,\n    input_handler::{Event, on_press},\n    layout::{child, layout},\n    line,\n    list::list,\n    span,\n    style\n}\n",
        );
    }

    #[test]
    fn blank_lines_stand_around_items_that_span_lines() {
        let src = "use a::b; let x = 1; // why\nlet y = 2; let z = 3; let f = |a| { let b = a; b }; f(x)";
        let want = "use a::b;\nlet x = 1;\n\n// why\nlet y = 2;\n\nlet z = 3;\nlet f = |a| { let b = a; b };\nf(x)\n";
        formats_to(SourceKind::Program, src, want);
        formats_to(
            SourceKind::Interface,
            "type T = i64; /// the v\nval v: T; val w: T",
            "type T = i64;\n\n/// the v\nval v: T;\n\nval w: T\n",
        );
    }

    #[test]
    fn strings_keep_their_delimiters() {
        use SourceKind::Program;
        formats_to(Program, r#"f("a\nb [x]", "plain")"#, "f(\"a\\nb [x]\", \"plain\")\n");
        formats_to(Program, "r\"a\nb\"", "r\"a\nb\"\n");
        formats_to(Program, "r#\"say \"hi\"\"#", "r#\"say \"hi\"\"#\n");
        formats_to(Program, "\"\"\"one line \\[x]\"\"\"", "\"\"\"one line \\[x]\"\"\"\n");
        formats_to(
            Program,
            "\"\"\"\n\nfirst [plain]\n\\[x] \"q\\\"\"\"\"",
            "\"\"\"\n\nfirst [plain]\n\\[x] \"q\\\"\"\"\"\n",
        );
    }

    #[test]
    fn struct_literal_fields_keep_their_order() {
        use SourceKind::Program;
        formats_to(
            Program,
            "{ domain, fingerprint: fp, addrs: [addr] }",
            "{ domain, fingerprint: fp, addrs: [addr] }\n",
        );
        formats_to(Program, "{ s with z: 1, a: 2 }", "{ s with z: 1, a: 2 }\n");
        stable(Program, "{ zebra: 1,\n// the a\n apple: 2 }");
    }

    #[test]
    fn struct_pattern_fields_keep_their_order() {
        use SourceKind::Program;
        formats_to(
            Program,
            "let { zed, apple: (a, _), .. } = s",
            "let { zed, apple: (a, _), .. } = s\n",
        );
        formats_to(
            Program,
            "select s { { y, x: 0 } => y, p@ { x, y } => x }",
            "select s { { y, x: 0 } => y, p@ { x, y } => x }\n",
        );
    }

    #[test]
    fn a_lone_bracketed_type_argument_hugs() {
        let fields = "alpha: Array<string>, beta: Array<string>, gamma: Array<string>, \
                      delta: Array<string>";
        let body = "        alpha: Array<string>,\n        beta: Array<string>,\n        \
                    gamma: Array<string>,\n        delta: Array<string>\n";
        formats_to(
            SourceKind::Interface,
            &format!(
                "type T = [`A({{ {fields} }}), `B(`C({{ {fields} }})), \
                 `D(Array<{{ {fields} }}>), `E(i64, {{ {fields} }})]"
            ),
            &format!(
                "type T = [\n    `A({{\n{body}    }}),\n    `B(`C({{\n{body}    }})),\n    \
                 `D(Array<{{\n{body}    }}>),\n    `E(\n        i64,\n        {{\n{}        }}\n    )\n]\n",
                body.replace("        ", "            ")
            ),
        );
    }

    #[test]
    fn struct_type_fields_keep_their_order() {
        use SourceKind::*;
        formats_to(
            Program,
            "type R = { title: string, lines: Array<string> }",
            "type R = { title: string, lines: Array<string> }\n",
        );
        formats_to(
            Interface,
            "val f: fn(r: { z: i64, a: i64 }) -> { y: i64, b: i64 }",
            "val f: fn(r: { z: i64, a: i64 }) -> { y: i64, b: i64 }\n",
        );
    }

    #[test]
    fn union_members_keep_their_order() {
        use SourceKind::*;
        formats_to(
            Interface,
            "type Log = [`Trace, `Debug, `Info, `Warn, `Error]",
            "type Log = [`Trace, `Debug, `Info, `Warn, `Error]\n",
        );
        formats_to(
            Program,
            "let x: [`Zed(i64), string, `Apple, null] = `Apple",
            "let x: [null, string, `Zed(i64), `Apple] = `Apple\n",
        );
        formats_to(Interface, "type N = [Real, Int]", "type N = [Real, Int]\n");
    }

    #[test]
    fn printing_outside_the_formatter_is_canonical() {
        use crate::expr::parser::parse_one;
        let cases = [
            ("{ z: 1, a: 2 }", "{ a: 2, z: 1 }"),
            ("let { z, a } = s", "let { a, z } = s"),
            ("let r: { z: i64, a: i64 } = s", "let r: { a: i64, z: i64 } = s"),
            ("f(r\"x\")?", "f(\"x\")?"),
            ("\"\"\"x \\[y]\"\"\"", "\"x [y]\""),
            ("let x: [`B, `A] = `A", "let x: [`A, `B] = `A"),
        ];
        for (src, want) in cases {
            assert_eq!(parse_one(src).unwrap().to_string(), want)
        }
    }

    #[test]
    fn the_config_sets_width_and_indent() {
        let cfg: FormatConfig = serde_json::from_str(r#"{ "indent": 2 }"#).unwrap();
        assert_eq!(cfg, FormatConfig { width: DEFAULT_WIDTH, indent: 2 });
        assert!(serde_json::from_str::<FormatConfig>(r#"{ "indnt": 2 }"#).is_err());
        let src = "let f = |a| { let b = a + 1; b * 2 }";
        let narrow = FormatConfig { width: 20, indent: 2 };
        let want = "let f = |a| {\n  let b = a + 1;\n  b * 2\n}\n";
        assert_eq!(&**format_source(SourceKind::Program, src, &narrow).unwrap(), want);
    }

    #[test]
    fn the_nearest_project_config_governs() {
        let root = tempfile::tempdir().unwrap();
        let deep = root.path().join("src/graphix/tui");
        fs::create_dir_all(&deep).unwrap();
        fs::write(root.path().join(CONFIG_FILE), r#"{ "width": 100 }"#).unwrap();
        let base = FormatConfig { width: 100, indent: DEFAULT_INDENT };
        assert_eq!(FormatConfig::discover(&deep).unwrap(), base);
        fs::write(deep.join(CONFIG_FILE), r#"{ "indent": 2 }"#).unwrap();
        let nearest = FormatConfig { width: DEFAULT_WIDTH, indent: 2 };
        assert_eq!(FormatConfig::discover(&deep).unwrap(), nearest);
        fs::write(deep.join(CONFIG_FILE), "{ width: 100 }").unwrap();
        assert!(FormatConfig::discover(&deep).is_err());
    }

    /// A relative directory climbs past the working directory: run in a
    /// child process, whose working directory is its own.
    #[test]
    fn a_relative_directory_finds_the_project_config() {
        if std::env::var_os("GRAPHIX_FMT_DISCOVER_CHILD").is_some() {
            let cfg = FormatConfig::discover(Path::new(".")).unwrap();
            assert_eq!(cfg.width, 37);
            return;
        }
        let root = tempfile::tempdir().unwrap();
        let deep = root.path().join("src/graphix");
        fs::create_dir_all(&deep).unwrap();
        fs::write(root.path().join(CONFIG_FILE), r#"{ "width": 37 }"#).unwrap();
        let module = module_path!().split_once("::").unwrap().1;
        let test = format!("{module}::a_relative_directory_finds_the_project_config");
        let out = std::process::Command::new(std::env::current_exe().unwrap())
            .args(["--exact", &test, "--include-ignored"])
            .env("GRAPHIX_FMT_DISCOVER_CHILD", "1")
            .current_dir(&deep)
            .output()
            .unwrap();
        let stdout = String::from_utf8_lossy(&out.stdout);
        assert!(out.status.success(), "{stdout}");
        assert!(stdout.contains("1 passed"), "the child ran no test: {stdout}");
    }

    #[test]
    fn a_long_head_gives_way_a_step_at_a_time() {
        use SourceKind::Program;
        let cfg = FormatConfig { width: 40, indent: 4 };
        let fmt = |src: &str| format_source(Program, src, &cfg).unwrap().to_string();
        assert_eq!(
            fmt("let f = |alpha: i64, beta: i64, gamma: i64| -> i64 alpha"),
            "let f = |\n    alpha: i64,\n    beta: i64,\n    gamma: i64\n| -> i64 alpha\n"
        );
        assert_eq!(
            fmt("let f = |@args: i64| -> Result<i64, `E(string)> 'a_builtin_name"),
            "let f = |\n    @args: i64\n| -> Result<i64, `E(string)>\n    'a_builtin_name\n"
        );
        assert_eq!(
            fmt("let f = |a: i64| -> { first_field: string, second_field: string } 'b"),
            "let f = |\n    a: i64\n| -> {\n    first_field: string,\n    second_field: string\n} 'b\n"
        );
        stable(
            Program,
            "let f = |a: i64| -> { first_field: string, second_field: string } 'b",
        );
    }

    #[test]
    fn a_long_let_annotation_breaks_the_type() {
        let cfg = FormatConfig { width: 40, indent: 4 };
        let src = "let v: { first_field: string, second_field: string } = f(x)";
        let want =
            "let v: {\n    first_field: string,\n    second_field: string\n} = f(x)\n";
        assert_eq!(&**format_source(SourceKind::Program, src, &cfg).unwrap(), want);
    }

    #[test]
    fn default_number_types_print_bare() {
        use SourceKind::Program;
        formats_to(
            Program,
            "[i64:1, -2, f64:3., 4.5, u8:6, f32:7., - 8, 1e3, 1e300, - 1.5]",
            "[1, -2, 3.0, 4.5, u8:6, f32:7., - 8, 1000.0, 1e300, - 1.5]\n",
        );
    }

    #[test]
    fn primitive_sets_print_as_written() {
        stable(SourceKind::Interface, "type Sint = [i8, i16, i32, z32, i64, z64]");
    }

    /// Every line of `text` fits `width`.
    fn fits(text: &str, width: usize) {
        for l in text.lines() {
            assert!(l.chars().count() <= width, "{} columns: {l}\n{text}", l.chars().count())
        }
    }

    #[test]
    fn a_use_binding_what_an_earlier_one_reads_stays_after_it() {
        use SourceKind::*;
        formats_to(Program, "use str::len; use a::str; x", "use str::len;\nuse a::str;\nx\n");
        formats_to(Program, "use a::str; use str::len; x", "use a::str;\nuse str::len;\nx\n");
    }

    #[test]
    fn comments_above_interface_items_and_trait_methods_stay() {
        use SourceKind::*;
        formats_to(Interface, "// plain comment\nval x: i64", "// plain comment\nval x: i64\n");
        formats_to(
            Interface,
            "trait T {\n  // c\n  /// d\n  val m: fn(self) -> i64\n}",
            "trait T {\n    // c\n    /// d\n    val m: fn(self) -> i64\n}\n",
        );
        formats_to(
            Program,
            "trait T { // a plain comment\nval show: fn(self) -> string }",
            "trait T {\n    // a plain comment\n    val show: fn(self) -> string\n}\n",
        );
        stable(
            Interface,
            "trait T {\n  val m: fn(self) -> i64 = |s| {\n    // why\n    use a::b;\n    b(s)\n  }\n}",
        );
    }

    #[test]
    fn a_decorated_tree_has_no_single_line_form() {
        use SourceKind::*;
        formats_to(
            Program,
            "let y = select x {\n // c\n `A => 1,\n _ => 2\n}",
            "let y = select x {\n    // c\n    `A => 1,\n    _ => 2\n}\n",
        );
        formats_to(
            Interface,
            "trait Eq {\n/// true if equal\nval eq: fn(self, other: self) -> bool }",
            "trait Eq {\n    /// true if equal\n    val eq: fn(self, other: self) -> bool\n}\n",
        );
    }

    #[test]
    fn broken_layouts_fit_and_close_where_they_open() {
        use SourceKind::Program;
        let fmt = |src: &str| {
            let out = format_source(Program, src, &FormatConfig::default()).unwrap();
            fits(&out, DEFAULT_WIDTH);
            stable(Program, src);
            out.to_string()
        };
        let long =
            "alpha_beta_gamma + delta_epsilon_zeta + eta_theta_iota + kappa_lambda_mu + nu_xi";
        let sum = fmt(&format!("let s = {long} + nu_xi_omicron + pi_rho + sigma_tau"));
        assert!(sum.lines().count() > 2, "{sum}");
        let call = "some_function_with_long_name(argument_number_one, argument_number_two, argument_three)";
        let field = fmt(&format!("let v = {call}.field"));
        assert_eq!(field.lines().last().map(str::trim), Some(").field"), "{field}");
        let map = fmt(&format!("let m = {{\"alpha\" => {call}, \"beta\" => 2}}"));
        assert!(map.contains("\"alpha\" =>"), "{map}");
        assert_eq!(
            fmt(&format!("let e = Counter({long})")),
            format!("let e = Counter(\n    {long}\n)\n")
        );
        assert_eq!(
            fmt("let v = *f(alpha_beta_gamma, delta_epsilon_zeta, eta_theta_iota, kappa_lambda, mu_nu_xi_omicron_pi)"),
            "let v = *f(\n    alpha_beta_gamma,\n    delta_epsilon_zeta,\n    eta_theta_iota,\n    kappa_lambda,\n    mu_nu_xi_omicron_pi\n)\n"
        );
        let caught = fmt(&format!("{{ catch(e) {{ println(e); {call} }}; x }}"));
        assert!(caught.lines().all(|l| !l.ends_with(' ')), "{caught:?}");
        let sandbox = fmt(
            "let m = mod m dynamic { sandbox whitelist [core, array, str, map, sys, http, toml, re, rand, pack, gui, tui, xls, net, fs, time, io, tcp, tls, dirs]; sig { val x: i64 }; source \"let x = 1\" }",
        );
        assert!(sandbox.contains("sandbox whitelist [\n") && !sandbox.contains(", \n"), "{sandbox}");
    }

    #[test]
    fn a_use_breaks_at_one_width_in_programs_and_interfaces() {
        // 90 columns, 91 with the `;`
        let names = "aaaaaaaaaa, bbbbbbbbbb, cccccccccc, dddddddddd, eeeeeeeeee, ffffffffff, ggggggg";
        let src = format!("use pkg::{{{names}}};\nval x: i64");
        let gxi = format_source(SourceKind::Interface, &src, &FormatConfig::default()).unwrap();
        fits(&gxi, DEFAULT_WIDTH);
        let src = format!("use pkg::{{{names}}};\nx");
        let gx = format_source(SourceKind::Program, &src, &FormatConfig::default()).unwrap();
        fits(&gx, DEFAULT_WIDTH);
        assert_eq!(gxi.lines().next(), gx.lines().next());
    }

    #[test]
    fn an_empty_statement_is_its_semicolon() {
        use SourceKind::Program;
        formats_to(Program, "{ a; b; }", "{ a; b; }\n");
        formats_to(Program, "x;", "x;\n");
        let cfg = FormatConfig { width: 12, ..FormatConfig::default() };
        let out = format_source(Program, "let f = { alpha; beta; }", &cfg).unwrap();
        assert_eq!(&*out, "let f = {\n    alpha;\n    beta;\n}\n");
    }

    #[test]
    fn a_deep_nest_formats_once_per_level() {
        let deep = format!("let s = {}1{}", "{ f: ".repeat(60), " }".repeat(60));
        stable(SourceKind::Program, &deep);
    }

    #[test]
    fn quantifiers_stand_apart_from_the_bar() {
        formats_to(
            SourceKind::Program,
            "let add = 'a: Int |a: 'a, b: 'a| a + b",
            "let add = 'a: Int |a: 'a, b: 'a| a + b\n",
        );
    }
}
