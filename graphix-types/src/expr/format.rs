use crate::{
    PrintFlag,
    expr::{
        Decorations, Expr, ExprKind, ModuleKind, Origin, SeqExpr, SeqTrigger, Sig,
        SigItem, SigKind, StrForm, StructurePattern, TraitExpr, TraitMethod, TryWithExpr,
        UseItem,
        parser::{parse, parse_sig},
        print::{
            PrettyBuf, PrettyDisplay, below_of, cmp_use_items, eol_of, pretty_file_items,
            pretty_item, use_seg, use_seg_key,
        },
    },
    format_with_flags,
    stack::ensure_sufficient,
};
use anyhow::{Context, Result, bail};
use arcstr::ArcStr;
use compact_str::{CompactString, format_compact};
use netidx_value::Value;
use poolshark::local::LPooled;
use serde_derive::Deserialize;
use smallvec::SmallVec;
use std::{
    fmt, fs,
    path::{Component, Path, PathBuf},
};
use triomphe::Arc;

pub const DEFAULT_WIDTH: usize = 90;

/// The spaces one level of nesting indents by, unless configured.
pub const DEFAULT_INDENT: usize = 4;
/// The widest indent the printer lays out: more is a mistake.
pub const MAX_INDENT: usize = 16;

/// The name of the formatter's configuration file.
pub const CONFIG_FILE: &str = "graphixfmt.json";

/// What `graphixfmt.json` can say; a field it leaves out keeps its default.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize)]
#[serde(default, deny_unknown_fields)]
pub struct FormatConfig {
    /// the line width to fit
    pub width: usize,
    /// the spaces one level of nesting indents by, at most `MAX_INDENT`
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
        let cfg: Self = serde_json::from_str(&text)
            .with_context(|| format_compact!("parsing {}", path.display()))?;
        cfg.checked().with_context(|| format_compact!("in {}", path.display()))
    }

    /// This configuration, if its indent is one the printer can lay out.
    pub fn checked(self) -> Result<Self> {
        if self.indent > MAX_INDENT {
            bail!("an indent of {} is more than {MAX_INDENT}", self.indent)
        }
        Ok(self)
    }

    /// The configuration that governs `file`, found from its directory
    /// as the path names it: absolute and lexically normalized, never
    /// canonical, so a link is laid out by the configuration above the
    /// link wherever it is formatted.
    pub fn for_file(file: &Path) -> Result<Self> {
        let abs = std::path::absolute(file)
            .with_context(|| format_compact!("resolving {}", file.display()))?;
        let mut path = PathBuf::new();
        for c in abs.components() {
            match c {
                Component::ParentDir => {
                    path.pop();
                }
                Component::CurDir => (),
                c => path.push(c),
            }
        }
        Self::discover(path.parent().unwrap_or(&path))
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
    fn binds(n: &UseItem) -> Option<&str> {
        n.bound_name()
    }
    /// The name a use item's path resolves through the imports of a
    /// level by: its first segment, or the one after a `self::` or
    /// `package::` anchor; a `super::` path reads another level's.
    fn reads(n: &UseItem) -> Option<&str> {
        match use_seg(n, 0) {
            Some("self" | "package") => use_seg(n, 1),
            Some("super") => None,
            r => r,
        }
    }
    /// Would `m` and `n` sorted into one statement read or shadow what
    /// the other binds? A glob binds names unknown here, so it merges
    /// with nothing.
    fn depends(m: &UseItem, n: &UseItem) -> bool {
        if m.is_glob() || n.is_glob() {
            return true;
        }
        let same = m.path == n.path && m.rename == n.rename;
        binds(m) == reads(n) || binds(n) == reads(m) || (binds(m) == binds(n) && !same)
    }
    let mut merged: LPooled<Vec<T>> = LPooled::take();
    // (group, reexport, name) of each name in the run of adjacent uses
    let mut run: LPooled<Vec<(usize, bool, UseItem)>> = LPooled::take();
    let mut i = 0;
    while i < items.len() {
        let first = &items[i];
        while let Some((reexport, names)) = items.get(i).and_then(&as_use) {
            // the group after the last one it depends on: every group
            // depends on the one before it, so a reparse groups alike
            let group = run
                .iter()
                .filter(|(_, _, m)| names.iter().any(|n| depends(m, n)))
                .map(|(g, _, _)| g + 1)
                .max()
                .unwrap_or(0);
            run.extend(names.iter().map(|n| (group, reexport, n.clone())));
            i += 1;
        }
        if run.is_empty() {
            merged.push(first.clone());
            i += 1;
            continue;
        }
        run.sort_by(|(ga, ra, a), (gb, rb, b)| {
            ga.cmp(gb)
                .then_with(|| root(a).cmp(&root(b)))
                .then(ra.cmp(rb))
                .then_with(|| cmp_use_items(a, b))
        });
        let same_stmt = |(ga, ra, a): &(usize, bool, UseItem),
                         (gb, rb, b): &(usize, bool, UseItem)| {
            ga == gb && ra == rb && root(a) == root(b)
        };
        for stmt in run.chunk_by(same_stmt) {
            let names = UseItem::sorted(stmt.iter().map(|(_, _, n)| n.clone()));
            merged.push(mk(first, stmt[0].1, names))
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
                if si.doc.0.is_none()
                    && si.comments.lines().is_empty()
                    && si.eol.text().is_none() =>
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
        SigKind::Trait(t) => {
            SigItem { kind: SigKind::Trait(merge_trait_uses(t)), ..si.clone() }
        }
        _ => si.clone(),
    }));
    Sig { items, toplevel: sig.toplevel, below: sig.below.clone() }
}

fn merge_trait_uses(t: &TraitExpr) -> Arc<TraitExpr> {
    Arc::new(TraitExpr {
        name: t.name.clone(),
        methods: Arc::from_iter(t.methods.iter().map(|m| TraitMethod {
            default: m.default.as_ref().map(merge_uses_within),
            ..m.clone()
        })),
        below: t.below.clone(),
    })
}

/// `exprs` with their use runs merged; `value`, a block's last statement,
/// stays out of them, since a block holds two statements or more.
fn merge_expr_uses(exprs: &[Expr], value: Option<&Expr>) -> Arc<[Expr]> {
    merge_uses(
        exprs,
        |e| match &e.kind {
            ExprKind::Use { reexport, names }
                if e.dec.is_none() && !value.is_some_and(|v| std::ptr::eq(v, e)) =>
            {
                Some((*reexport, names))
            }
            _ => None,
        },
        |e, reexport, names| e.rekind(ExprKind::Use { reexport, names }),
    )
}

/// `e` with the use statements of every statement list in it merged.
fn merge_uses_within(e: &Expr) -> Expr {
    use ExprKind::*;
    let e = ensure_sufficient(|| e.map_children(&mut merge_uses_within));
    let kind = match &e.kind {
        Block { exprs } => Block { exprs: merge_expr_uses(exprs, exprs.last()) },
        Seq(s) => Seq(Arc::new(SeqExpr {
            body: merge_expr_uses(&s.body, None),
            ..(**s).clone()
        })),
        TryWith(t) => TryWith(Arc::new(TryWithExpr {
            body: merge_expr_uses(&t.body, None),
            handler: merge_expr_uses(&t.handler, None),
            ..(**t).clone()
        })),
        Module { name, value }
            if let ModuleKind::Dynamic { sandbox, sig, source } = &**value =>
        {
            Module {
                name: name.clone(),
                value: Arc::new(ModuleKind::Dynamic {
                    sandbox: sandbox.clone(),
                    sig: merge_sig_uses(sig),
                    source: source.clone(),
                }),
            }
        }
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
                Self::Program(merge_expr_uses(&exprs, None))
            }
            SourceKind::Interface => Self::Interface(merge_sig_uses(&parse_sig(ori)?)),
        })
    }

    fn print(&self, buf: &mut PrettyBuf) -> Result<()> {
        match self {
            Self::Program(exprs) => pretty_file_items(
                buf,
                exprs,
                pretty_item,
                eol_of,
                exprs.last().map(below_of).unwrap_or(&[]),
            )?,
            Self::Interface(sig) => sig.fmt_pretty_inner(buf)?,
        }
        Ok(())
    }

    /// What equality does not see that the formatter must hand back, in
    /// source order: every comment and attribute, and every string
    /// literal's delimiters, each with the preorder ordinal of the node it
    /// stands on, so one that moves to a neighbour is a difference.
    fn ornaments(&self) -> LPooled<Vec<(usize, Ornament<'_>)>> {
        let mut w = Ornaments { node: 0, acc: LPooled::take() };
        match self {
            Self::Program(exprs) => exprs.iter().for_each(|e| w.expr(e)),
            Self::Interface(sig) => w.sig(sig),
        }
        w.acc
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
        fn count(
            what: &str,
            a: usize,
            b: usize,
        ) -> Option<(CompactString, CompactString)> {
            (a != b)
                .then(|| (format_compact!("{a} {what}"), format_compact!("{b} {what}")))
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
    Eol(Option<&'a ArcStr>),
    Delimiters(StrForm),
}

/// The delimiters of every string literal in `e`, an expression's or a
/// pattern's, in source order: what printing as written must keep.
#[cfg(test)]
pub(super) fn str_forms(e: &Expr) -> LPooled<Vec<StrForm>> {
    let mut w = Ornaments { node: 0, acc: LPooled::take() };
    w.expr(e);
    w.acc
        .drain(..)
        .filter_map(|(_, o)| match o {
            Ornament::Delimiters(form) => Some(form),
            _ => None,
        })
        .collect()
}

/// The ornaments of a tree in preorder, each beside its node's ordinal.
struct Ornaments<'a> {
    node: usize,
    acc: LPooled<Vec<(usize, Ornament<'a>)>>,
}

impl<'a> Ornaments<'a> {
    fn push(&mut self, o: Ornament<'a>) {
        self.acc.push((self.node, o))
    }

    /// A pattern's string literals' delimiters, in order.
    fn pattern(&mut self, p: &'a StructurePattern) {
        ensure_sufficient(|| {
            if let StructurePattern::Literal(Value::String(_), form) = p {
                self.push(Ornament::Delimiters(form.0))
            }
            p.for_each_sub(&mut |c| self.pattern(c))
        })
    }

    fn expr(&mut self, e: &'a Expr) {
        ensure_sufficient(|| {
            self.node += 1;
            if let Some(d) = &e.dec {
                self.push(Ornament::Decorations(d));
                for a in d.attrs.iter() {
                    a.args.iter().for_each(|a| self.expr(a))
                }
            }
            match &e.kind {
                ExprKind::Constant(Value::String(_))
                | ExprKind::StringInterpolate { .. } => {
                    self.push(Ornament::Delimiters(e.str_form))
                }
                ExprKind::Bind(b) => self.pattern(&b.pattern),
                ExprKind::Lambda(l) => {
                    l.args.iter().for_each(|a| self.pattern(&a.pattern))
                }
                ExprKind::Select(sel) => sel
                    .arms
                    .iter()
                    .for_each(|(p, _)| self.pattern(&p.structure_predicate)),
                ExprKind::Trait(t) => self.methods(t, |_, _| ()),
                ExprKind::Module { value, .. }
                    if let ModuleKind::Dynamic { sig, .. } = &**value =>
                {
                    self.sig(sig)
                }
                ExprKind::Seq(sq) if let Some(SeqTrigger::Bind(b)) = &sq.trigger => {
                    self.pattern(&b.pattern)
                }
                _ => (),
            }
            e.for_each_child(&mut |c| self.expr(c))
        })
    }

    /// A trait's comments; `default` walks a method's default body, which
    /// an expression's walk reaches as a child.
    fn methods(
        &mut self,
        t: &'a TraitExpr,
        mut default: impl FnMut(&mut Self, &'a Expr),
    ) {
        for m in t.methods.iter() {
            self.push(Ornament::Comments(m.comments.lines()));
            self.push(Ornament::Eol(m.eol.text()));
            if let Some(d) = &m.default {
                default(self, d)
            }
        }
        self.push(Ornament::Comments(t.below.lines()))
    }

    fn sig(&mut self, sig: &'a Sig) {
        for si in sig.items.iter() {
            self.node += 1;
            self.push(Ornament::Comments(si.comments.lines()));
            self.push(Ornament::Eol(si.eol.text()));
            if let SigKind::Trait(t) = &si.kind {
                self.methods(t, |w, d| w.expr(d))
            }
        }
        self.push(Ornament::Comments(sig.below.lines()))
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
/// it says exactly what `text` said, comments and attributes included. A
/// `#!` line is kept as it is.
pub fn format_source(
    kind: SourceKind,
    text: &str,
    cfg: &FormatConfig,
) -> Result<LPooled<String>> {
    let n = super::parser::shebang_len(text);
    if n > 0 {
        // the rest keeps the `#!` line's break, so its positions are the file's
        let (line, rest) = text.split_at(n);
        let brk = match rest {
            r if r.starts_with("\r\n") => "\r\n",
            r if r.starts_with('\n') => "\n",
            r if r.starts_with('\r') => "\r",
            _ => "",
        };
        let mut out: LPooled<String> = LPooled::take();
        out.push_str(line);
        out.push_str(brk);
        out.push_str(&format_source(kind, rest, cfg)?);
        return Ok(out);
    }
    // a file whose every line ends in CRLF keeps them; the layout is LF
    let lines = text.matches('\n').count();
    let (parsed, buf) = layout(kind, text, cfg)?;
    if lines > 0 && text.matches("\r\n").count() == lines {
        // the layout's line breaks become CRLF; a literal's own already are
        let mut crlf: LPooled<String> = LPooled::take();
        let mut prev = '\0';
        for c in buf.as_str().chars() {
            if c == '\n' && prev != '\r' {
                crlf.push('\r');
            }
            crlf.push(c);
            prev = c;
        }
        guard(kind, &parsed, &crlf)?;
        return Ok(crlf);
    }
    guard(kind, &parsed, buf.as_str())?;
    Ok(buf.into_string())
}

/// Refused unless `out` reads back as what `parsed` said, comments,
/// attributes and string delimiters included.
fn guard(kind: SourceKind, parsed: &Parsed, out: &str) -> Result<()> {
    let reparsed = match Parsed::new(kind, out) {
        Ok(p) => p,
        Err(e) => {
            let msg = format_compact!("the formatted text does not parse: {e:#}");
            bail!(Refused(msg.as_str().into()))
        }
    };
    if let Some((was, now)) = parsed.difference(&reparsed) {
        let msg = format_compact!(
            "the formatted text says something else\nwas {was}\nnow {now}"
        );
        bail!(Refused(msg.as_str().into()))
    }
    let (was, now) = (parsed.ornaments(), reparsed.ornaments());
    if *was != *now {
        let lost = was.iter().zip(now.iter()).find(|(a, b)| a != b);
        let msg = match lost.map(|(a, _)| a).or_else(|| was.get(now.len())) {
            Some((_, Ornament::Delimiters(_))) => {
                "the formatted text changed a string's delimiters"
            }
            _ => "the formatted text lost or moved a comment or an attribute",
        };
        bail!(Refused(arcstr::ArcStr::from(msg)))
    }
    Ok(())
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

    // a commented guard, until, abort, flush, scrutinee or dynamic source
    // starts on a line of its own after its head
    #[test]
    fn a_decorated_child_after_a_head_reads_back() {
        for src in [
            "let r = select x {\n  a if\n    // c\n    a > 1 => 1,\n  _ => 2\n};\nr\n",
            "seq t {\n  a;\n  until\n    // d\n    b;\n  c\n}\n",
            "seq t; abort(\n  // c\n  e\n) {\n  a;\n  c\n}\n",
            "seqq t; flush(\n  // c\n  e\n) {\n  a;\n  c\n}\n",
            "select\n  // c\n  x {\n  _ => 2\n}\n",
            "mod m dynamic {\n  sandbox unrestricted;\n  sig { val x: i64 };\n  source\n    // c\n    \"let x = 1\"\n};\nm::x\n",
        ] {
            stable(SourceKind::Program, src);
        }
    }

    // a quantifier that occurs only inside another's bound keeps its own
    #[test]
    fn a_bound_only_quantifier_keeps_its_bound() {
        let src = "type F = fn<'a: Array<'b>, 'b: Int>(x: 'a) -> 'a;\n0\n";
        let out =
            format_source(SourceKind::Program, src, &FormatConfig::default()).unwrap();
        assert!(out.contains("'b: Int"), "{}", *out);
    }

    #[test]
    fn a_spliced_string_constant_stays_spliced() {
        for src in [
            "let x = 1; \"hello[\"-\"][x]\"",
            "\"[\"a\"]\"",
            "\"a[\"b\"][\"c\"]d\"",
            "\"[\"\"]x\"",
        ] {
            stable(SourceKind::Program, src);
        }
        for src in [
            "\"a[\"b\"][\"q\"]\"\n",
            "\"[\"b\"][r\"q\"]\"\n",
            "\"[x][r\"q\"]\"\n",
            "\"[r\"b\"][r\"q\"]\"\n",
            "\"\"\"t \\[\"b\"]\\[r\"q\"]\"\"\"\n",
        ] {
            formats_to(SourceKind::Program, src, src);
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
        stable(SourceKind::Program, "seq *r { h()? }");
        stable(SourceKind::Program, "seq x ~ *r { h()? }");
        stable(SourceKind::Program, "seq let v = *r { v + 1 }");
        stable(SourceKind::Program, "seq let c = { a: go, b: 2 } { c.a }");
        stable(SourceKind::Program, "seqq let v = flush(go) { v }");
        // the source a postfix chain starts from takes the parentheses
        formats_to(
            SourceKind::Program,
            "seqq (flush)(1) { 1; 2 }",
            "seqq (flush)(1) { 1; 2 }\n",
        );
        formats_to(
            SourceKind::Program,
            "seq ({a: 1}).a { 1; 2 }",
            "seq ({ a: 1 }).a { 1; 2 }\n",
        );
        formats_to(
            SourceKind::Program,
            "seq ({s with a: 2}).a + 1 { 1; 2 }",
            "seq ({ s with a: 2 }).a + 1 { 1; 2 }\n",
        );
    }

    #[test]
    fn template_string_brackets_and_splices() {
        stable(SourceKind::Program, r#"println("\nneeds [req.what] for a\[0\]")"#);
        stable(SourceKind::Program, "println(\"\"\"a [plain] \\[x] \\\\[y]\nb\"\"\")");
    }

    #[test]
    fn string_patterns_keep_their_delimiters() {
        stable(
            SourceKind::Program,
            "select s { r\"C:\\p\\[x]\" => 1, \"\"\"t \"q\" [x]\"\"\" => 2, _ => 0 }",
        );
        stable(SourceKind::Program, "let `A(r\"a\\b\") = x; 1");
        stable(SourceKind::Program, "seq let `A(r\"a\\b\") = x { 1 }");
    }

    #[test]
    fn applied_self_receiver() {
        let src = "trait Coll { val map: fn(self<'a>, f: fn(x: 'a) -> 'b) -> self<'b> }";
        stable(SourceKind::Interface, src);
        stable(SourceKind::Program, src);
    }

    // a quantifier's bound reaches every use, a nested fn type's included
    #[test]
    fn nested_uses_keep_the_quantifier() {
        let src = "type F = fn<'a: Number>(g: fn(x: 'a) -> 'a, y: 'a) -> f64;\n0\n";
        formats_to(SourceKind::Program, src, src);
    }

    fn formats_to(kind: SourceKind, src: &str, want: &str) {
        assert_eq!(&**format_source(kind, src, &FormatConfig::default()).unwrap(), want);
        stable(kind, src)
    }

    /// The guard refuses `out` as the formatting of `src`, saying `why`.
    fn refused(kind: SourceKind, src: &str, out: &str, why: &str) {
        let e = guard(kind, &Parsed::new(kind, src).unwrap(), out).unwrap_err();
        assert!(e.downcast_ref::<Refused>().is_some(), "{e:#}");
        assert!(e.to_string().contains(why), "{e}");
    }

    #[test]
    fn the_guard_refuses_text_that_says_something_else() {
        use SourceKind::*;
        refused(Program, "let x = 1; x", "let x = 1; x +", "does not parse");
        refused(Program, "let x = 1; x", "let x = 2; x", "says something else");
        refused(Interface, "val x: i64", "val x: string", "says something else");
        refused(Program, "// c\nlet x = 1;\nx", "let x = 1;\n// c\nx", "comment");
        refused(
            Interface,
            "// c\nval x: i64;\nval y: i64",
            "val x: i64;\n// c\nval y: i64",
            "comment",
        );
        refused(
            Program,
            "trait T {\n// c\nval m: fn(self) -> i64;\nval n: fn(self) -> i64 }",
            "trait T {\nval m: fn(self) -> i64;\n// c\nval n: fn(self) -> i64 }",
            "comment",
        );
        refused(
            Program,
            "select s { r\"a\" => 1, _ => 0 }",
            "select s { \"a\" => 1, _ => 0 }",
            "delimiters",
        );
        refused(Program, "f(r\"a\")", "f(\"a\")", "delimiters");
    }

    #[test]
    fn crlf_lines_stay_crlf() {
        use SourceKind::*;
        formats_to(Program, "let x =   1;\r\nx\r\n", "let x = 1;\r\nx\r\n");
        formats_to(
            Program,
            "let s = r\"a\r\nb\";\r\ns\r\n",
            "let s = r\"a\r\nb\";\r\n\r\ns\r\n",
        );
        // a mixed file comes out LF
        formats_to(Program, "let s = \"a\";\r\ns\n", "let s = \"a\";\ns\n");
        // a quoted or template string spanning a line keeps its CR
        for src in
            ["let s = \"a\r\nb\";\r\ns\r\n", "let s = \"\"\"a\r\nb\"\"\";\r\ns\r\n"]
        {
            let out = format_source(Program, src, &FormatConfig::default()).unwrap();
            guard(Program, &Parsed::new(Program, src).unwrap(), &out).unwrap();
            stable(Program, src);
        }
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
            "use a::*;\nuse a::*;\nuse a::{b, b::{self, c}}\n",
        );
        formats_to(
            Program,
            "use b::x;\n// why\nuse a::x",
            "use b::x;\n\n// why\nuse a::x\n",
        );
        formats_to(Program, "{ use b::x; use a::y; y }", "{ use a::y; use b::x; y }\n");
        // a block's value is never merged away
        formats_to(
            Program,
            "{ use array::len; use array::map }",
            "{ use array::len; use array::map }\n",
        );
        // a use that depends on no earlier group joins the first one
        formats_to(
            Program,
            "use x::p; use p::q; use a::b; q + b",
            "use a::b;\nuse x::p;\nuse p::q;\nq + b\n",
        );
        formats_to(
            Program,
            "use m::v; use n::v; use a::w; v",
            "use a::w;\nuse m::v;\nuse n::v;\nv\n",
        );
        // a statement reading or shadowing what an earlier one bound
        // closes the run: those stay in order
        formats_to(
            Program,
            "use z::x as a; use a::v; use b::c; use q::*; use r::s; use r::t; v",
            "use b::c;\nuse z::x as a;\nuse a::v;\nuse q::*;\nuse r::{s, t};\nv\n",
        );
        formats_to(
            Program,
            "use b::v; use a::v; use c::w; v",
            "use b::v;\nuse c::w;\nuse a::v;\nv\n",
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
        let src = "use a::b; let x = 1;\n// why\nlet y = 2; let z = 3; let f = |a| { let b = a; b }; f(x)";
        let want = "use a::b;\nlet x = 1;\n\n// why\nlet y = 2;\n\nlet z = 3;\nlet f = |a| { let b = a; b };\nf(x)\n";
        formats_to(SourceKind::Program, src, want);
        formats_to(
            SourceKind::Interface,
            "type T = i64;\n/// the v\nval v: T; val w: T",
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

    // a comment, which prints only as written, decides no canonical layout
    #[test]
    fn a_comment_leaves_canonical_printing_alone() {
        use crate::expr::parser::parse_one;
        for (src, want) in [
            ("(\n// c\ns\n).a?", "s.a?"),
            ("{ (\n// c\ns\n) with a: 1 }", "{ s with a: 1 }"),
            (
                "select x { y if\n// c\ny > 1 => 1, _ => 2 }",
                "select x { y if y > 1 => 1, _ => 2 }",
            ),
        ] {
            assert_eq!(parse_one(src).unwrap().to_string(), want, "{src}")
        }
        let src = "trait T {\n// c\n/// d\nval m: fn(self) -> i64 }";
        let canonical = parse_sig(Origin::unspecified(src)).unwrap().to_string();
        assert!(canonical.contains("\n/// d\n"), "{canonical}");
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
            "let f = |a: i64| -> {\n    first_field: string,\n    second_field: string\n} 'b\n"
        );
        assert_eq!(
            fmt("let g = |x| -> i64 throws [`Aaaaaaaaaaaa, `Bbbbbbbbbbbbbb] x"),
            "let g = |x| -> i64 throws [\n    `Aaaaaaaaaaaa,\n    `Bbbbbbbbbbbbbb\n] x\n"
        );
        stable(
            Program,
            "let f = |a: i64| -> { first_field: string, second_field: string } 'b",
        );
    }

    // every type annotation lays its type out over lines when it does not fit
    #[test]
    fn a_long_annotation_breaks_the_type() {
        use SourceKind::Program;
        let cfg = FormatConfig { width: 40, indent: 4 };
        let fmt = |src: &str| format_source(Program, src, &cfg).unwrap().to_string();
        let t = "{ first_field: string, second: i64 }";
        let laid = "{\n        first_field: string,\n        second: i64\n    }";
        assert_eq!(
            fmt(&format!("let r = cast<{t}>(y)")),
            format!("let r =\n    cast<{laid}>(\n        y\n    )\n")
        );
        assert_eq!(
            fmt(&format!("let n = never<{t}>()")),
            format!("let n =\n    never<{laid}>()\n")
        );
        assert_eq!(
            fmt(&format!("let f = |x: {t}| x")),
            format!(
                "let f = |\n    x: {{\n        first_field: string,\n        second: i64\n    }}\n| x\n"
            )
        );
        let u = "[`Aaaaaaaaaaaaaa, `Bbbbbbbbbbbbbbbbb]";
        assert_eq!(
            fmt(&format!("{{ catch(e: {u}) e; x? }}")),
            "{\n    catch(e: [\n        `Aaaaaaaaaaaaaa,\n        `Bbbbbbbbbbbbbbbbb\n    ]) e;\n    x?\n}\n"
        );
        assert_eq!(
            fmt(&format!("let t = try {{ a; b }} with(e: {u}) {{ c; d }}")),
            "let t = try {\n    a;\n    b\n} with(e: [\n    `Aaaaaaaaaaaaaa,\n    `Bbbbbbbbbbbbbbbbb\n]) {\n    c;\n    d\n}\n"
        );
        for src in [
            format!("let r = cast<{t}>(y)"),
            format!("{{ catch(e: {u}) e; x? }}"),
            format!("select x {{ {t} as v => 1, _ => 0 }}"),
        ] {
            stable(Program, &src);
        }
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
            "[1, -2, 3.0, 4.5, u8:6, f32:7.0, - 8, 1000.0, 1e300, - 1.5]\n",
        );
    }

    // a seq's head is measured once, not by laying out its body: nested
    // seqs with long triggers format in linear time
    #[test]
    fn nested_seq_triggers_format_once() {
        let mut src = String::from("let x0 = 1; x0");
        for i in 1..=24 {
            src = format!("let x{i} = seq {} {{ {src} }}; x{i}", "a".repeat(95));
        }
        let cfg = FormatConfig { indent: 2, ..FormatConfig::default() };
        let start = std::time::Instant::now();
        format_source(SourceKind::Program, &src, &cfg).unwrap();
        assert!(start.elapsed() < std::time::Duration::from_secs(10));
    }

    // a select scrutinee or seq trigger that breaks continues nested past its
    // head, never at the column of the `let` holding it
    #[test]
    fn a_broken_scrutinee_or_trigger_continues_nested() {
        let a = "a".repeat(41);
        let b = "b".repeat(53);
        let src = format!(
            "let f = |x| {{\n  let r = select {a} + {b} {{ 0 => 1, _ => 2 }};\n  let s = seq {a} ~ {b} {{ r }};\n  s\n}};\nf\n"
        );
        let out =
            format_source(SourceKind::Program, &src, &FormatConfig::default()).unwrap();
        for l in out.lines().filter(|l| l.trim_start().starts_with(&b)) {
            assert!(l.starts_with("        "), "{l}\n{}", &*out)
        }
        stable(SourceKind::Program, &src);
    }

    #[test]
    fn an_f32_keeps_its_shortest_spelling() {
        formats_to(
            SourceKind::Program,
            "[f32:1e-45, f32:3.4028235e38, f32:0.1, f32:-0.0]",
            "[f32:1e-45, f32:3.4028235e38, f32:0.1, f32:-0.0]\n",
        );
    }

    // a guard counts toward whether its arm's pattern stays on one line
    #[test]
    fn a_guarded_arm_fits() {
        let src = "select x {\n  `Aaaaaaaaaaaaaaaaaaaa(bbbbbbbbbbbbbbbbbbbbb, cccccccccccccccccccccc, ddddddddd) if bbbbbbbbbbbbbbbbbbbbb => 1,\n  _ => 0\n}\n";
        let cfg = FormatConfig::default();
        fits(&format_source(SourceKind::Program, src, &cfg).unwrap(), cfg.width);
        stable(SourceKind::Program, src);
    }

    #[test]
    fn a_shebang_line_stays_and_positions_count_it() {
        use SourceKind::*;
        formats_to(
            Program,
            "#!/usr/bin/env graphix\nlet x =   1;\nx\n",
            "#!/usr/bin/env graphix\nlet x = 1;\nx\n",
        );
        formats_to(
            Program,
            "#!/usr/bin/env graphix\r\nlet x =   1;\r\nx\r\n",
            "#!/usr/bin/env graphix\r\nlet x = 1;\r\nx\r\n",
        );
        formats_to(
            Program,
            "#!/usr/bin/env graphix\rlet x =   1;\rx\r",
            "#!/usr/bin/env graphix\rlet x = 1;\nx\n",
        );
        formats_to(Program, "\u{feff}let x =   1;\nx\n", "\u{feff}let x = 1;\nx\n");
        formats_to(Interface, "\u{feff}val x:  i64", "\u{feff}val x: i64\n");
        formats_to(
            Program,
            "\u{feff}#!/usr/bin/env graphix\nlet x =   1;\nx\n",
            "\u{feff}#!/usr/bin/env graphix\nlet x = 1;\nx\n",
        );
        let e = format_source(
            Program,
            "#!/usr/bin/env graphix\nlet x = 1 +\n",
            &FormatConfig::default(),
        )
        .unwrap_err();
        assert!(format!("{e:#}").contains("line: 3"), "{e:#}");
    }

    // a zero-argument fn type keeps `fn()` whole at any width
    #[test]
    fn an_empty_fn_type_never_breaks_inside_its_parens() {
        let src = "val h: fn(x: &mut fn() -> i64) -> i64;";
        let cfg = FormatConfig { width: 20, ..FormatConfig::default() };
        let out = format_source(SourceKind::Interface, src, &cfg).unwrap();
        assert!(out.contains("x: &mut fn() -> i64"), "{out}");
        stable(SourceKind::Interface, src);
    }

    // a struct field's bracketed type hugs its name, and a typedef's lone
    // parameter hugs its brackets, as Array<..> does
    #[test]
    fn type_fields_and_lone_params_hug() {
        let fields = "beta_long_field_name: i64, gamma_long_field_name: string, delta_long_long_long: f64, eps: i64";
        let src = format!(
            "type O<'a> = [null, 'a];\ntype T = {{alpha: {{{fields}}}, z: i64}};\ntype U = O<{{{fields}}}>;\n0\n"
        );
        let out =
            format_source(SourceKind::Program, &src, &FormatConfig::default()).unwrap();
        assert!(
            out.contains("    alpha: {\n        beta_long_field_name: i64,"),
            "{out}"
        );
        assert!(out.contains("    },\n    z: i64"), "{out}");
        assert!(out.contains("type U = O<{\n    beta_long_field_name: i64,"), "{out}");
        stable(SourceKind::Program, &src);
    }

    #[test]
    fn primitive_sets_print_as_written() {
        stable(SourceKind::Interface, "type Sint = [i8, i16, i32, z32, i64, z64]");
    }

    /// Every line of `text` fits `width`.
    fn fits(text: &str, width: usize) {
        for l in text.lines() {
            assert!(
                l.chars().count() <= width,
                "{} columns: {l}\n{text}",
                l.chars().count()
            )
        }
    }

    // a source that closes with its own bracket prints bare before a
    // postfix operator, in a seq head too
    #[test]
    fn a_bracketed_postfix_source_prints_bare() {
        let src = "let a = any(a, b).f;\n\
                   let b = cast<T>(x).f;\n\
                   let c = `A(1).0;\n\
                   let d = T(1).0;\n\
                   let e = never<T>().f;\n\
                   let f = select x { _ => y }.f;\n\
                   let g = `A(1)(2);\n\
                   let h = seq select x { _ => y }.f ~ z { 1; 2 };\n\
                   let i = seqq let q = T(1).0 { 1; q };\n\
                   let j = seq live(x).f { 1; 2 };\n\
                   a\n";
        formats_to(SourceKind::Program, src, src);
    }

    // the ` {` after a select's scrutinee or a seq's trigger counts
    // against the width
    #[test]
    fn a_head_fits_with_its_brace() {
        let cfg = FormatConfig { width: 40, ..FormatConfig::default() };
        for src in [
            "select ffffffffffffffffffffffffff(1, 2) {\n    3 => 4,\n    _ => 5\n}\n",
            "seq fffffffffffffffffffffffffffff(1, 2) {\n    3;\n    4\n}\n",
            "seq (ffffffffffffffffffffffffff(1, 2) ~ x) {\n    3;\n    4\n}\n",
        ] {
            let once = format_source(SourceKind::Program, src, &cfg).unwrap();
            fits(&once, 40);
            let twice = format_source(SourceKind::Program, &once, &cfg).unwrap();
            assert_eq!(*once, *twice);
        }
    }

    #[test]
    fn a_use_binding_what_an_earlier_one_reads_stays_after_it() {
        use SourceKind::*;
        formats_to(
            Program,
            "use str::len; use a::str; x",
            "use str::len;\nuse a::str;\nx\n",
        );
        formats_to(
            Program,
            "use a::str; use str::len; x",
            "use a::str;\nuse str::len;\nx\n",
        );
    }

    // the comment ending an item's line, after its separator, and those
    // below a list's last item stay, in every list, with and without a
    // trailing separator
    #[test]
    fn comments_after_list_items_stay() {
        use SourceKind::*;
        for (kind, src) in [
            (Program, "// only a comment\n"),
            (Program, "let a = 1;\nlet b = 2;\n// the end\n"),
            (Program, "let z = {\n    a; // e\n    b // f\n    // c\n};\n\nz\n"),
            (Program, "seq t {\n    a;\n    b;\n    // below\n}\n"),
            (Program, "[\n    1, // one\n    2\n    // below\n]\n"),
            (Program, "f(\n    1, // one\n    2 // two\n    // below\n)\n"),
            (Program, "select x {\n    1 => 2, // a\n    _ => 3\n    // below\n}\n"),
            (Program, "{\n    a: 1, // a\n    b: 2\n    // below\n}\n"),
            (Program, "{\n    a,\n    b\n    // below\n}\n"),
            (Program, "f(\n    #b: 1,\n    #a\n    // below\n)\n"),
            (Program, "{\n    1 => 2, // one\n    3 => 4 // four\n    // below\n}\n"),
            (Program, "any(\n    1, // one\n    2\n    // below\n)\n"),
            (Program, "never(\n    1, // one\n    2 // two\n)\n"),
            (
                Program,
                "trait T {\n    val f: fn(self) -> i64; // f\n    val g: fn(self) -> i64 // g\n    // below\n};\n\n1\n",
            ),
            (Program, "trait T {\n    // nothing yet\n};\n\n1\n"),
            (
                Program,
                "impl T for i64 {\n    let f = |x| x; // f\n    let g = |x| x // g\n    // below\n};\n\n1\n",
            ),
            (
                Program,
                "mod m dynamic {\n    sandbox unrestricted;\n    sig {\n        val x: i64; // x\n        val y: i64\n        // below\n    };\n    source \"let x = 1\";\n};\n\nm::x\n",
            ),
            (Interface, "// only a comment\n"),
            (Interface, "val x: i64; // x\nval y: i64 // y\n// below\n"),
            (
                Interface,
                "trait T {\n    /// d\n    val f: fn(self) -> i64; // f\n    val g: fn(self) -> i64\n    // below\n}\n",
            ),
        ] {
            formats_to(kind, src, src)
        }
        // a trailing separator after the last item is not kept
        formats_to(
            Program,
            "[\n    1,\n    2,\n    // below\n]\n",
            "[\n    1,\n    2\n    // below\n]\n",
        );
        formats_to(
            Program,
            "impl T for i64 {\n    let f = |x| x;\n    // below\n};\n1\n",
            "impl T for i64 {\n    let f = |x| x\n    // below\n};\n\n1\n",
        );
        formats_to(
            Interface,
            "val x: i64;\n// val y: i64\n",
            "val x: i64\n// val y: i64\n",
        );
    }

    // the guard sees a comment inside an attribute's argument, which the
    // parser refuses
    #[test]
    fn the_guard_reads_attribute_arguments() {
        use crate::expr::Attr;
        use SourceKind::Program;
        use arcstr::literal;
        let src = "#[a(b)]\nlet x = 1";
        let Parsed::Program(exprs) = Parsed::new(Program, src).unwrap() else {
            unreachable!()
        };
        let mut e = exprs[0].clone();
        let mut d = (**e.dec.as_ref().unwrap()).clone();
        let mut arg = d.attrs[0].args[0].clone();
        arg.dec = Some(Arc::new(Decorations {
            comments: Arc::from_iter([literal!(" lost")]),
            ..Decorations::default()
        }));
        d.attrs =
            Arc::from_iter([Attr { args: Arc::from_iter([arg]), ..d.attrs[0].clone() }]);
        e.dec = Some(Arc::new(d));
        let e = guard(Program, &Parsed::Program(Arc::from_iter([e])), src).unwrap_err();
        assert!(e.to_string().contains("lost or moved a comment"), "{e}");
    }

    #[test]
    fn comments_above_interface_items_and_trait_methods_stay() {
        use SourceKind::*;
        formats_to(
            Interface,
            "// plain comment\nval x: i64",
            "// plain comment\nval x: i64\n",
        );
        formats_to(
            Interface,
            "trait T {\n  // c\n  /// d\n  val m: fn(self) -> i64\n}",
            "trait T {\n    // c\n    /// d\n    val m: fn(self) -> i64\n}\n",
        );
        formats_to(
            Program,
            "trait T {\n// a plain comment\nval show: fn(self) -> string }",
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
        let long = "alpha_beta_gamma + delta_epsilon_zeta + eta_theta_iota + kappa_lambda_mu + nu_xi";
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
            fmt(
                "let v = *f(alpha_beta_gamma, delta_epsilon_zeta, eta_theta_iota, kappa_lambda, mu_nu_xi_omicron_pi)"
            ),
            "let v = *f(\n    alpha_beta_gamma,\n    delta_epsilon_zeta,\n    eta_theta_iota,\n    kappa_lambda,\n    mu_nu_xi_omicron_pi\n)\n"
        );
        let caught = fmt(&format!("{{ catch(e) {{ println(e); {call} }}; x }}"));
        assert!(caught.lines().all(|l| !l.ends_with(' ')), "{caught:?}");
        let sandbox = fmt(
            "let m = mod m dynamic { sandbox whitelist [core, array, str, map, sys, http, toml, re, rand, pack, gui, tui, xls, net, fs, time, io, tcp, tls, dirs]; sig { val x: i64 }; source \"let x = 1\" }",
        );
        assert!(
            sandbox.contains("sandbox whitelist [\n") && !sandbox.contains(", \n"),
            "{sandbox}"
        );
    }

    #[test]
    fn a_use_breaks_at_one_width_in_programs_and_interfaces() {
        // 90 columns, 91 with the `;`
        let names = "aaaaaaaaaa, bbbbbbbbbb, cccccccccc, dddddddddd, eeeeeeeeee, ffffffffff, ggggggg";
        let src = format!("use pkg::{{{names}}};\nval x: i64");
        let gxi =
            format_source(SourceKind::Interface, &src, &FormatConfig::default()).unwrap();
        fits(&gxi, DEFAULT_WIDTH);
        let src = format!("use pkg::{{{names}}};\nx");
        let gx =
            format_source(SourceKind::Program, &src, &FormatConfig::default()).unwrap();
        fits(&gx, DEFAULT_WIDTH);
        assert_eq!(gxi.lines().next(), gx.lines().next());
    }

    #[test]
    fn an_empty_statement_is_its_semicolon() {
        use SourceKind::Program;
        formats_to(Program, "{ a; b; }", "{ a; b; }\n");
        formats_to(Program, "x;", "x;\n");
        formats_to(Program, "x;\n// c\n", "x;\n// c\n");
        formats_to(Program, "{ a; b;\n// c\n}", "{\n    a;\n    b;\n    // c\n}\n");
        formats_to(Program, "{ a;\n// c\n}", "{\n    a;\n    // c\n}\n");
        let cfg = FormatConfig { width: 12, ..FormatConfig::default() };
        let out = format_source(Program, "let f = { alpha; beta; }", &cfg).unwrap();
        assert_eq!(&*out, "let f = {\n    alpha;\n    beta;\n}\n");
    }

    // a value opening with a bracket follows its head, whatever postfix
    // ends it; a cast hugs its lone bracketed argument as a call does
    #[test]
    fn bracketed_values_follow_their_heads() {
        use SourceKind::Program;
        let cfg = FormatConfig { width: 40, ..FormatConfig::default() };
        let fmt = |src: &str| format_source(Program, src, &cfg).unwrap().to_string();
        let call = "some_function(aaaaaaaaaaaa, bbbbbbbbbbbbbbb, cccccccccc)";
        let laid =
            "some_function(\n    aaaaaaaaaaaa,\n    bbbbbbbbbbbbbbb,\n    cccccccccc\n)";
        for suffix in ["?", ".field", "[0]", ".0"] {
            let src = format!("let z = {call}{suffix};\nz");
            assert_eq!(fmt(&src), format!("let z = {laid}{suffix};\n\nz\n"));
            stable(Program, &src);
        }
        let src = "let x = cast<Array<i64>>([1111111111, 2222222222, 3333333333, 44444444444]);\nx";
        assert_eq!(
            fmt(src),
            "let x = cast<Array<i64>>([\n    1111111111,\n    2222222222,\n    3333333333,\n    44444444444\n]);\n\nx\n"
        );
        stable(Program, src);
    }

    // a hugged layout leaves the column of the separator after it, and a
    // lone argument hugs only where its head fits
    #[test]
    fn hugs_fit_the_width() {
        use SourceKind::Program;
        let a = "a".repeat(70);
        let src = format!("let zz = ffff([{a}, 1]);\nzz");
        let cfg = FormatConfig::default();
        fits(&format_source(Program, &src, &cfg).unwrap(), DEFAULT_WIDTH);
        stable(Program, &src);
        let src = format!("let x = {}1{};\nx", "ffffffffff(".repeat(12), ")".repeat(12));
        fits(&format_source(Program, &src, &cfg).unwrap(), DEFAULT_WIDTH);
        stable(Program, &src);
    }

    // a hugged type argument leaves the columns of the closers after it,
    // and a let's laid-out annotation those of its ` =`
    #[test]
    fn type_hugs_fit_the_width() {
        use SourceKind::Program;
        let cfg = FormatConfig::default();
        for n in 54..=64 {
            let a = "a".repeat(n);
            for src in [
                format!("let zz: Array<{{{a}: i64, b: i64}}> = [];\nzz"),
                format!("type T = Array<`A({{{a}: i64}})>;\n1"),
            ] {
                fits(&format_source(Program, &src, &cfg).unwrap(), DEFAULT_WIDTH);
                stable(Program, &src);
            }
        }
    }

    // the closer, a postfix suffix and the separator that follow a layout
    // on its last line are kept free: an index whose call fits flat only
    // without them breaks
    #[test]
    fn a_postfix_chain_keeps_its_tail_free() {
        use SourceKind::Program;
        for width in 50..=70 {
            let cfg = FormatConfig { width, ..FormatConfig::default() };
            let src = "let f = |s, v, cs, c| {\n    let col = s.columns[slot_item(v.x, cs, c)]$;\n    col\n};\nf";
            fits(&format_source(Program, src, &cfg).unwrap(), width);
        }
        stable(
            SourceKind::Program,
            "let f = |s, v, cs, c| {\n    let col = s.columns[slot_item(v.x, cs, c)]$;\n    col\n};\nf",
        );
    }

    // an index, a key, a slice's bounds and an update's source are laid
    // out where they do not fit flat or hold a comment
    #[test]
    fn indices_and_keys_are_laid_out() {
        use SourceKind::Program;
        let srcs = [
            "let f = |a: Array<i64>, i: i64| {\n    let v = a[select i {\n        // first\n        0 => 0,\n        _ => i - 1\n    }];\n    v$\n};\nf",
            "let m = {\"k\" => 1};\nlet w = m{select 1 {\n    // key\n    1 => \"k\",\n    _ => \"j\"\n}};\nw",
            "let long = [1, 2, 3][array::len([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19]) - 19..];\nlong",
            "let v = a[{\n    // c\n    let i = 0;\n    i\n}];\nv",
            "let w = m{{\n    #[sync]\n    let k = \"k\";\n    k\n}};\nw",
            "let n = {\n    {\n        // c\n        let k = 1;\n        k\n    } => 2\n};\nn",
            "let t = { ({\n    // c\n    let z = s;\n    z\n}) with a: 2 };\nt",
            "let v = structure.columns[slot_item(aaaaaaaaaaaaaaaaaaaa, bbbbbbbbbbbbbbbbbbbbb, ccccccccccccccc)]$;\nv",
        ];
        let cfg = FormatConfig::default();
        for src in srcs {
            let out = format_source(Program, src, &cfg).unwrap();
            fits(&out, DEFAULT_WIDTH);
            for l in out.lines() {
                assert!(!l.ends_with(' '), "trailing space: {l:?}\n{}", *out);
                let t = l.trim_start();
                assert!(
                    !(t.starts_with("//") || t.starts_with("#[")) || l.starts_with(' '),
                    "a decoration at column 0: {l:?}\n{}",
                    *out
                );
            }
            stable(Program, src);
        }
    }

    // a select's scrutinee is measured, not laid out, to place its head:
    // nested scrutinees format in time linear in their depth
    #[test]
    fn nested_scrutinees_format_once() {
        let mut src = String::from("0");
        for i in 0..24 {
            src = format!("select f(|v{i}| {src}) {{ _ => 1 }}");
        }
        let src = format!("let f = |g| g(1);\nlet r = {src};\nr");
        let start = std::time::Instant::now();
        format_source(SourceKind::Program, &src, &FormatConfig::default()).unwrap();
        assert!(start.elapsed() < std::time::Duration::from_secs(10));
        let mut src = String::from("0");
        for i in 0..20 {
            src = format!("select {{ let x{i} = {src}; x{i} }} {{ _ => 1 }}");
        }
        let start = std::time::Instant::now();
        format_source(SourceKind::Program, &src, &FormatConfig::default()).unwrap();
        assert!(start.elapsed() < std::time::Duration::from_secs(10));
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

    #[test]
    fn a_long_pattern_breaks_like_its_literal() {
        let cfg = FormatConfig { width: 30, ..FormatConfig::default() };
        let src = "let (alpha, `Beta(b, c), {gamma, ..}) = v; \
                   select v { `Aa(x) | `Bb(x) | `Cc(x) | `Dd(x) => x, {alpha: 1, beta} => beta }";
        let want = "let (\n    alpha,\n    `Beta(b, c),\n    { gamma, .. }\n) = v;\n\n\
                    select v {\n    `Aa(x)\n    | `Bb(x)\n    | `Cc(x)\n    | `Dd(x) => x,\n    \
                    { alpha: 1, beta } =>\n        beta\n}\n";
        assert_eq!(&*format_source(SourceKind::Program, src, &cfg).unwrap(), want);
        stable(SourceKind::Program, src)
    }
}
