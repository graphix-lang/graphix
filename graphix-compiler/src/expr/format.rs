use crate::{
    PrintFlag,
    expr::{
        Decorations, Expr, ExprKind, ModuleKind, Origin, Sig, SigItem, SigKind, StrForm,
        TryWithExpr, UseItem,
        parser::{parse, parse_sig},
        print::{
            DEFAULT_INDENT, PrettyBuf, PrettyDisplay, cmp_use_items, pretty_file_items,
            use_seg, use_seg_key,
        },
    },
    format_with_flags,
};
use anyhow::{Context, Result, bail};
use compact_str::{CompactString, format_compact};
use netidx_value::Value;
use poolshark::local::LPooled;
use serde_derive::Deserialize;
use std::{fmt, fs, path::Path};
use triomphe::Arc;

pub const DEFAULT_WIDTH: usize = 90;

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
pub struct Refused(CompactString);

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
    let mut merged: LPooled<Vec<T>> = LPooled::take();
    let mut run: LPooled<Vec<(bool, UseItem)>> = LPooled::take();
    let mut i = 0;
    while i < items.len() {
        let first = &items[i];
        while let Some((reexport, names)) = items.get(i).and_then(&as_use) {
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

fn merge_sig_uses(sig: &Sig) -> Sig {
    let items = merge_uses(
        &sig.items,
        |si| match &si.kind {
            SigKind::Use { reexport, names } if si.doc.0.is_none() => {
                Some((*reexport, names))
            }
            _ => None,
        },
        |si, reexport, names| SigItem {
            kind: SigKind::Use { reexport, names },
            ..si.clone()
        },
    );
    Sig { items, toplevel: sig.toplevel }
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
        |e, reexport, names| Expr {
            id: e.id,
            ori: e.ori.clone(),
            pos: e.pos,
            kind: ExprKind::Use { reexport, names },
            dec: None,
            str_form: Default::default(),
        },
    )
}

/// `e` with the use statements of every statement list in it merged.
fn merge_uses_within(e: &Expr) -> Expr {
    use ExprKind::*;
    let e = crate::stack::ensure_sufficient(|| e.map_children(&mut merge_uses_within));
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
    Expr {
        id: e.id,
        ori: e.ori.clone(),
        pos: e.pos,
        kind,
        dec: e.dec.clone(),
        str_form: e.str_form,
    }
}

enum Parsed {
    Program(Arc<[Expr]>),
    Interface(Sig),
}

impl Parsed {
    fn new(kind: SourceKind, text: &str) -> Result<Self> {
        let ori = Origin::from_str(text);
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
            Self::Program(exprs) => {
                pretty_file_items(buf, exprs, |buf, e| e.fmt_pretty(buf))?
            }
            Self::Interface(sig) => sig.fmt_pretty_inner(buf)?,
        }
        Ok(())
    }

    fn decorations(&self) -> LPooled<Vec<Decorations>> {
        let mut acc: LPooled<Vec<Decorations>> = LPooled::take();
        if let Self::Program(exprs) = self {
            for e in exprs.iter() {
                e.fold((), &mut |(), e| {
                    if let Some(d) = &e.dec {
                        acc.push((**d).clone())
                    }
                })
            }
        }
        acc
    }

    /// The delimiters of every string literal, in source order.
    fn string_forms(&self) -> LPooled<Vec<StrForm>> {
        let mut acc: LPooled<Vec<StrForm>> = LPooled::take();
        if let Self::Program(exprs) = self {
            for e in exprs.iter() {
                e.fold((), &mut |(), e| {
                    if let ExprKind::Constant(Value::String(_))
                    | ExprKind::StringInterpolate { .. } = &e.kind
                    {
                        acc.push(e.str_form)
                    }
                })
            }
        }
        acc
    }

    /// The first place `other` says something else, as the text each
    /// side prints there.
    fn difference(&self, other: &Self) -> Option<(String, String)> {
        fn children(e: &Expr) -> Vec<Expr> {
            let mut acc = vec![];
            e.for_each_child(&mut |c| acc.push(c.clone()));
            acc
        }
        fn smallest(a: &Expr, b: &Expr) -> (String, String) {
            let (ac, bc) = (children(a), children(b));
            if ac.len() == bc.len() {
                if let Some((a, b)) = ac.iter().zip(bc.iter()).find(|(a, b)| a != b) {
                    return smallest(a, b);
                }
            }
            (format!("{}: {a}", a.pos), format!("{}: {b}", b.pos))
        }
        fn count(what: &str, a: usize, b: usize) -> Option<(String, String)> {
            (a != b).then(|| (format!("{a} {what}"), format!("{b} {what}")))
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
                    .map(|(a, b)| (a.to_string(), b.to_string()))
                    .or_else(|| count("items", a.len(), b.len()))
            }
            (Self::Program(_), Self::Interface(_))
            | (Self::Interface(_), Self::Program(_)) => unreachable!(),
        }
    }
}

fn layout(
    kind: SourceKind,
    text: &str,
    cfg: &FormatConfig,
) -> Result<(Parsed, PrettyBuf)> {
    let parsed = Parsed::new(kind, text)?;
    let mut buf = PrettyBuf::new(cfg.width);
    buf.step = cfg.indent;
    format_with_flags(PrintFlag::AsWritten, || parsed.print(&mut buf))?;
    Ok((parsed, buf))
}

/// `format_source` without the reparse: for debugging the formatter.
pub fn format_source_unchecked(
    kind: SourceKind,
    text: &str,
    cfg: &FormatConfig,
) -> Result<LPooled<String>> {
    Ok(layout(kind, text, cfg)?.1.buf)
}

/// `text` laid out canonically. The result is reparsed and refused unless
/// it says exactly what `text` said, comments and attributes included.
pub fn format_source(
    kind: SourceKind,
    text: &str,
    cfg: &FormatConfig,
) -> Result<LPooled<String>> {
    let (parsed, buf) = layout(kind, text, cfg)?;
    let reparsed = match Parsed::new(kind, &buf.buf) {
        Ok(p) => p,
        Err(e) => {
            bail!(Refused(format_compact!("the formatted text does not parse: {e:?}")))
        }
    };
    if let Some((was, now)) = parsed.difference(&reparsed) {
        bail!(Refused(format_compact!(
            "the formatted text says something else\nwas {was}\nnow {now}"
        )))
    }
    if parsed.string_forms() != reparsed.string_forms() {
        bail!(Refused("the formatted text changed a string's delimiters".into()))
    }
    if parsed.decorations() != reparsed.decorations() {
        bail!(Refused("the formatted text lost a comment or an attribute".into()))
    }
    Ok(buf.buf)
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
            "use super::q;\nuse a::{c::{self, d, e}, *};\npub use a::p;\nuse b::y;\nx\n",
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
            "[i64:1, -2, f64:3., 4.5, u8:6, f32:7., - 8, 1e3]",
            "[1, -2, 3.0, 4.5, u8:6, f32:7., -i64:8, 1000.0]\n",
        );
    }

    #[test]
    fn primitive_sets_print_as_written() {
        stable(SourceKind::Interface, "type Sint = [i8, i16, i32, z32, i64, z64]");
    }
}
