use crate::{
    expr::{
        ApplyExpr, Arg, Attr, BinOp, BindExpr, BindSig, Decorations, Doc,
        Expr, ExprKind, ImplExpr, LambdaExpr, ModuleKind, Sandbox, SelectExpr, SeqTrigger,
        Sig, SigItem, SigKind, StrForm, StructExpr, StructWithExpr, TraitExpr,
        TraitMethod, TypeDefBody, TypeDefExpr, UseItem, format::FormatConfig, parser,
    },
    print_as_written,
    stack::ensure_sufficient,
    typ::Type,
};
use arcstr::ArcStr;
use compact_str::format_compact;
use netidx_core::{path::Path, utils::Either};
use netidx_value::{Value, parser::VAL_ESC};
use poolshark::local::LPooled;
use smallvec::SmallVec;
use std::{
    cmp::Ordering,
    fmt::{self, Formatter, Write},
};
use triomphe::Arc;

/// The `let pattern[: type] = ` of a bound seq trigger; the value
/// follows under the trigger's own parenthesization.
fn write_seq_let(f: &mut impl Write, b: &BindExpr) -> fmt::Result {
    let rec = if b.rec { " rec" } else { "" };
    match &b.typ {
        None => write!(f, "let{rec} {} = ", b.pattern),
        Some(typ) => write!(f, "let{rec} {}: {typ} = ", b.pattern),
    }
}

/// The source a postfix form prints bare in front of its suffix.
fn bare_postfix_source(e: &ExprKind) -> Option<&Expr> {
    use ExprKind::*;
    let source = match e {
        StructRef { source, .. }
        | TupleRef { source, .. }
        | ArrayRef { source, .. }
        | ArraySlice { source, .. }
        | MapRef { source, .. } => source,
        Apply(a) => &a.function,
        Qop(e) | OrNever(e) => e,
        _ => return None,
    };
    prints_as_bare_postfix(source).then_some(&**source)
}

/// A seq trigger is parenthesized exactly where the head parser would
/// not read it back bare: a leading `{` is the body, a map access is
/// refused outside brackets, a call of `flush` is the clause, and the
/// head admits operator expressions only.
fn trigger_needs_parens(t: &Expr) -> bool {
    use ExprKind::*;
    // the expression whose first token the head parser meets first, and
    // whether an argument list follows it directly
    fn leftmost(e: &Expr, called: bool) -> (&Expr, bool) {
        match (BinOp::of(&e.kind), bare_postfix_source(&e.kind)) {
            (Some((_, lhs, _)), _) => leftmost(lhs, false),
            (None, Some(source)) => leftmost(source, matches!(&e.kind, Apply(_))),
            (None, None) => (e, called),
        }
    }
    fn reads_bare(e: &Expr) -> bool {
        match &e.kind {
            MapRef { .. } => false,
            Ref { .. }
            | Constant(_)
            | ExplicitParens(_)
            | Tuple { .. }
            | Array { .. }
            | List { .. }
            | Map { .. }
            | Struct(_)
            | StructWith(_)
            | Do { .. }
            | Select(_)
            | Seq { .. }
            | Variant { .. }
            | Construct { .. }
            | TypeCast { .. }
            | Never { .. }
            | Any { .. }
            | StringInterpolate { .. } => true,
            k => match (BinOp::of(k), bare_postfix_source(k)) {
                (Some((_, lhs, rhs)), _) => reads_bare(lhs) && reads_bare(rhs),
                (None, Some(source)) => reads_bare(source),
                (None, None) => matches!(
                    k,
                    StructRef { .. }
                        | TupleRef { .. }
                        | ArrayRef { .. }
                        | ArraySlice { .. }
                        | Apply(_)
                        | Qop(_)
                        | OrNever(_)
                ),
            },
        }
    }
    let reads_as_body_or_clause = match leftmost(t, false) {
        (Expr { kind: Ref { name }, .. }, true) => &*name.0 == "/flush",
        (e, _) => matches!(&e.kind, Do { .. } | Struct(_) | StructWith(_) | Map { .. }),
    };
    reads_as_body_or_clause || !reads_bare(t)
}

/// `exprs` between `open` and `close`, one to a line; a lone expression
/// that opens with a bracket hugs the brackets when `hug` says so.
fn pretty_print_exprs(
    buf: &mut PrettyBuf,
    exprs: &[Expr],
    open: &str,
    close: &str,
    sep: &str,
    hug: bool,
) -> fmt::Result {
    if exprs.is_empty() {
        return writeln!(buf, "{open}{close}");
    }
    if let ([e], true) = (exprs, hug)
        && hugs_parens(e)
    {
        write!(buf, "{open}")?;
        e.fmt_pretty(buf)?;
        buf.kill_newline();
        return writeln!(buf, "{close}");
    }
    writeln!(buf, "{open}")?;
    buf.nested(|buf| pretty_items(buf, exprs, sep))?;
    writeln!(buf, "{close}")
}

/// Each of `exprs` on its own line, `sep` after all but the last. The
/// empty statement a trailing `;` leaves is that `;` and nothing more.
fn pretty_items(buf: &mut PrettyBuf, exprs: &[Expr], sep: &str) -> fmt::Result {
    for (i, e) in exprs.iter().enumerate() {
        if i > 0 {
            buf.kill_newline();
            writeln!(buf, "{sep}")?
        }
        if !matches!(e.kind, ExprKind::NoOp) {
            e.fmt_pretty(buf)?
        }
    }
    Ok(())
}

/// The `;`-separated items of a file: a blank line stands on both sides
/// of every item that spans lines, and runs of one-line items stay tight.
/// An item that prints nothing (a trailing `;`'s empty statement) is its
/// separator alone.
pub(crate) fn pretty_file_items<T>(
    buf: &mut PrettyBuf,
    items: &[T],
    item: impl Fn(&mut PrettyBuf, &T) -> fmt::Result,
) -> fmt::Result {
    let mut prev_spans_lines = false;
    for (i, it) in items.iter().enumerate() {
        let start = buf.mark();
        item(buf, it)?;
        let spans_lines = buf.since(start).trim_end_matches('\n').contains('\n');
        if i > 0 && (spans_lines || prev_spans_lines) {
            buf.insert_newline(start)
        }
        prev_spans_lines = spans_lines;
        if i < items.len() - 1 {
            buf.kill_newline();
            writeln!(buf, ";")?
        }
    }
    Ok(())
}

/// Whether the multi-line layout of `e` opens with a short head and a
/// bracket, closing at its own indent: it can sit on the line of
/// whatever introduces it.
fn opens_with_bracket(e: &ExprKind) -> bool {
    use ExprKind::*;
    match e {
        Do { .. }
        | Lambda(_)
        | Select(_)
        | Seq { .. }
        | TryWith(_)
        | Struct(_)
        | StructWith(_)
        | Array { .. }
        | List { .. }
        | Tuple { .. }
        | Map { .. }
        | Apply(_)
        | Any { .. }
        | Never { .. }
        | Construct { .. }
        | TypeCast { .. }
        | ExplicitParens(_)
        | Module { value: ModuleKind::Dynamic { .. }, .. } => true,
        Variant { args, .. } => !args.is_empty(),
        Qop(e) | OrNever(e) | Rethrow(e) | ByRef(e) | Deref(e) | Neg(e) => {
            opens_with_bracket(&e.kind)
        }
        Not { expr } => opens_with_bracket(&expr.kind),
        _ => false,
    }
}

/// The width `x` prints flat in.
fn flat_width(x: &impl fmt::Display) -> usize {
    struct Count(usize);
    impl Write for Count {
        fn write_str(&mut self, s: &str) -> fmt::Result {
            self.0 += s.chars().count();
            Ok(())
        }
    }
    let mut n = Count(0);
    let _ = write!(n, "{x}");
    n.0
}

/// The least the first line of `e`'s multi-line layout can take: the text
/// in front of its bracket that no layout breaks.
fn head_min(e: &ExprKind) -> usize {
    use ExprKind::*;
    match e {
        List { .. } => 2,
        TryWith(_) => "try {".len(),
        Seq { queued: true, .. } => "seqq {".len(),
        Seq { queued: false, .. } => "seq {".len(),
        Select(_) => "select _ {".len(),
        StructWith(_) => "{ _ with".len(),
        Any { .. } => "any(".len(),
        Never { typ: None, .. } => "never(".len(),
        Never { typ: Some(t), .. } => "never<>(".len() + flat_width(t),
        TypeCast { typ, .. } => "cast<>(".len() + flat_width(typ),
        Construct { name, .. } => flat_width(name) + 1,
        Variant { tag, .. } => tag.chars().count() + 2,
        Apply(a) => match &a.function.kind {
            Ref { name } => flat_width(name) + 1,
            _ => 1,
        },
        Module { name, .. } => "mod  dynamic {".len() + name.chars().count(),
        Qop(e) | OrNever(e) | Rethrow(e) => head_min(&e.kind),
        ByRef(e) | Deref(e) | Neg(e) | Not { expr: e } => 1 + head_min(&e.kind),
        _ => 1,
    }
}

/// Whether `e`, the only argument between parentheses, opens on their
/// line and closes with them: `f({`, `` `Tag([ ``.
fn hugs_parens(e: &Expr) -> bool {
    e.dec.is_none() && opens_with_bracket(&e.kind)
}

/// The expression a head introduces (`let x =`, `x <-`, `name:`, `=>`);
/// the head has been written without its trailing space. It follows on
/// the head's line when it fits there or opens with a bracket, else it
/// moves to its own line, indented.
fn pretty_tail(buf: &mut PrettyBuf, e: &Expr) -> fmt::Result {
    if e.dec.is_none() {
        return pretty_tail_bare(buf, e);
    }
    writeln!(buf)?;
    buf.nested(|buf| e.fmt_pretty(buf))
}

/// `pretty_tail` for an expression whose decorations the caller placed.
fn pretty_tail_bare(buf: &mut PrettyBuf, e: &Expr) -> fmt::Result {
    let head = buf.mark();
    write!(buf, " ")?;
    if Bare(e).fmt_flat(buf)? {
        return Ok(());
    }
    // the first line is measured by laying it out, so a head that cannot
    // fit is not tried: that would lay out the body twice at every level
    if opens_with_bracket(&e.kind) && buf.col() + head_min(&e.kind) <= buf.limit {
        let start = buf.mark();
        let col = buf.col();
        Bare(e).fmt_pretty_inner(buf)?;
        if col + buf.first_line_width(start) <= buf.limit {
            return Ok(());
        }
        buf.rollback(start);
    }
    buf.rollback(head);
    writeln!(buf)?;
    buf.nested(|buf| Bare(e).fmt_pretty(buf))
}

/// The lines above a decorated expression: its comments, then its
/// attributes.
pub(crate) fn write_leading(
    f: &mut impl fmt::Write,
    dec: &Option<Arc<Decorations>>,
) -> fmt::Result {
    if let Some(dec) = dec {
        write_comments(f, &dec.comments)?;
        for a in dec.attrs.iter() {
            writeln!(f, "{a}")?;
        }
    }
    Ok(())
}

fn write_comments(f: &mut impl fmt::Write, lines: &[ArcStr]) -> fmt::Result {
    for c in lines {
        writeln!(f, "//{c}")?;
    }
    Ok(())
}

/// Whether anything in `e`, `e` included, prints on lines of its own
/// above what it decorates: a comment, an attribute, a doc. Such a tree
/// has no single-line form.
pub(crate) fn decorated(e: &Expr) -> bool {
    e.dec.is_some() || kind_decorated(&e.kind)
}

fn kind_decorated(k: &ExprKind) -> bool {
    ensure_sufficient(|| {
        let mut any = match k {
            ExprKind::Trait(t) => t.decorated(),
            ExprKind::Module { value: ModuleKind::Dynamic { sig, .. }, .. } => {
                sig.decorated()
            }
            _ => false,
        };
        k.for_each_child(&mut |c| any = any || decorated(c));
        any
    })
}

fn sig_item_decorated(si: &SigItem) -> bool {
    si.doc.0.is_some()
        || !si.comments.lines().is_empty()
        || matches!(&si.kind, SigKind::Trait(t) if t.decorated())
}

/// A literal as source text: `i64` and `f64`, the types an unprefixed
/// number reads as, print bare, an `f64` always with a point or an
/// exponent; a string escapes as an expression's does.
pub(crate) struct Literal<'a>(pub &'a Value);

impl fmt::Display for Literal<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self.0 {
            Value::I64(v) => write!(f, "{v}"),
            Value::F64(v) if v.is_finite() => write!(f, "{v:?}"),
            v @ Value::String(_) => v.fmt_ext(f, &parser::GRAPHIX_ESC, true),
            v => v.fmt_ext(f, &VAL_ESC, true),
        }
    }
}

/// A position in a [`PrettyBuf`] to measure from or roll back to.
#[derive(Debug, Clone, Copy)]
pub struct Mark(usize);

/// Text being laid out to a width. Every multi-line layout
/// (`fmt_pretty_inner`) ends with a newline, which a caller that continues
/// the line takes back with `kill_newline`.
#[derive(Debug)]
pub struct PrettyBuf {
    indent: usize,
    /// what `nested` adds to `indent`
    step: usize,
    limit: usize,
    buf: LPooled<String>,
}

impl PrettyBuf {
    pub fn new(cfg: FormatConfig) -> Self {
        Self { indent: 0, step: cfg.indent, limit: cfg.width, buf: LPooled::take() }
    }

    /// Run `f` one level of nesting deeper.
    pub fn nested<R, F: FnOnce(&mut Self) -> R>(&mut self, f: F) -> R {
        self.with_indent(self.step, f)
    }

    fn at_line_start(&self) -> bool {
        self.buf.chars().next_back().map(|c| c == '\n').unwrap_or(true)
    }

    fn push_indent(&mut self) {
        if self.at_line_start() {
            self.buf.extend((0..self.indent).map(|_| ' '));
        }
    }

    pub fn with_indent<R, F: FnOnce(&mut Self) -> R>(&mut self, inc: usize, f: F) -> R {
        self.indent += inc;
        let r = f(self);
        self.indent -= inc;
        r
    }

    /// The width of the line being written.
    pub fn col(&self) -> usize {
        self.buf[self.buf.rfind('\n').map_or(0, |i| i + 1)..].chars().count()
    }

    pub fn mark(&self) -> Mark {
        Mark(self.buf.len())
    }

    /// Forget everything written since `m`.
    pub fn rollback(&mut self, m: Mark) {
        self.buf.truncate(m.0)
    }

    /// What was written since `m`.
    pub fn since(&self, m: Mark) -> &str {
        &self.buf[m.0..]
    }

    /// The width, in characters, of what was written since `m`.
    pub fn width_since(&self, m: Mark) -> usize {
        self.since(m).chars().count()
    }

    /// The width of the first line written since `m`.
    pub fn first_line_width(&self, m: Mark) -> usize {
        self.since(m).lines().next().map_or(0, |l| l.chars().count())
    }

    /// Put an empty line in front of what was written since `m`.
    pub fn insert_newline(&mut self, m: Mark) {
        self.buf.insert(m.0, '\n')
    }

    pub fn kill_newline(&mut self) {
        if let Some('\n') = self.buf.chars().next_back() {
            self.buf.pop();
        }
    }

    pub fn as_str(&self) -> &str {
        &self.buf
    }

    pub fn into_string(self) -> LPooled<String> {
        self.buf
    }
}

impl fmt::Write for PrettyBuf {
    fn write_char(&mut self, c: char) -> fmt::Result {
        self.push_indent();
        self.buf.write_char(c)
    }

    fn write_str(&mut self, s: &str) -> fmt::Result {
        self.push_indent();
        self.buf.write_str(s)
    }

    fn write_fmt(&mut self, args: fmt::Arguments<'_>) -> fmt::Result {
        self.push_indent();
        self.buf.write_fmt(args)
    }
}

// XCR claude for eric: agreed that a flat `Display` beside a hand-written broken twin
// drifts (the Construct, Deref, Sandbox and catch bugs were drift); describing each
// node once as a group and rendering it flat or broken is a printer rewrite, its own
// change, gated by the corpus harness. This change fixes the drifted twins in place.
pub trait PrettyDisplay: fmt::Display {
    /// The multi-line layout; `fmt_pretty` calls it when the single-line
    /// form does not fit. It ends with a newline.
    fn fmt_pretty_inner(&self, buf: &mut PrettyBuf) -> fmt::Result;

    /// Whether something inside prints on a line of its own (a comment,
    /// an attribute, a doc), so there is no single-line form.
    fn decorated(&self) -> bool {
        false
    }

    /// Write the single-line form and a newline if it fits the rest of
    /// the line, else write nothing.
    fn fmt_flat(&self, buf: &mut PrettyBuf) -> Result<bool, fmt::Error> {
        if self.decorated() {
            return Ok(false);
        }
        let start = buf.mark();
        let col = buf.col();
        writeln!(buf, "{}", self)?;
        // Best-effort: a multi-line string overcounts and a long token can
        // exceed any limit. The newline counts as the column kept for the
        // `;` or `,` that follows.
        let fits = col + buf.width_since(start) <= buf.limit;
        if !fits {
            buf.rollback(start);
        }
        Ok(fits)
    }

    /// Format on a single line when it fits, else via `fmt_pretty_inner`.
    fn fmt_pretty(&self, buf: &mut PrettyBuf) -> fmt::Result {
        ensure_sufficient(|| {
            if self.fmt_flat(buf)? { Ok(()) } else { self.fmt_pretty_inner(buf) }
        })
    }

    /// Pretty print to a pooled string
    fn to_string_pretty(&self, limit: usize) -> LPooled<String> {
        let mut buf = PrettyBuf::new(FormatConfig { width: limit, ..Default::default() });
        self.fmt_pretty(&mut buf).unwrap();
        buf.buf
    }
}

impl fmt::Display for Doc {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if let Some(doc) = self.0.as_ref() {
            if doc == "" {
                writeln!(f, "///")?;
            } else {
                for line in doc.lines() {
                    writeln!(f, "///{line}")?;
                }
            }
        }
        Ok(())
    }
}

impl fmt::Display for Attr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if self.args.is_empty() {
            write!(f, "#[{}]", self.name)
        } else {
            write!(f, "#[{}(", self.name)?;
            for (i, a) in self.args.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{a}")?;
            }
            write!(f, ")]")
        }
    }
}

impl PrettyDisplay for Doc {
    fn fmt_pretty_inner(&self, buf: &mut PrettyBuf) -> fmt::Result {
        if let Some(doc) = self.0.as_ref() {
            if doc == "" {
                writeln!(buf, "///")?;
            } else {
                for line in doc.lines() {
                    writeln!(buf, "///{line}")?;
                }
            }
        }
        Ok(())
    }
}

impl TypeDefExpr {
    fn write_name_and_params(&self, f: &mut impl fmt::Write) -> fmt::Result {
        write!(f, "type {}", self.name)?;
        if !self.params.is_empty() {
            write!(f, "<")?;
            for (i, (tv, ct)) in self.params.iter().enumerate() {
                write!(f, "{tv}")?;
                if let Some(ct) = ct {
                    write!(f, ": {ct}")?;
                }
                if i < self.params.len() - 1 {
                    write!(f, ", ")?;
                }
            }
            write!(f, ">")?;
        }
        Ok(())
    }
}

impl fmt::Display for TypeDefExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.write_name_and_params(f)?;
        match &self.body {
            TypeDefBody::Abstract(None) => Ok(()),
            TypeDefBody::Abstract(Some(rep)) => write!(f, " = Abstract<{rep}>"),
            TypeDefBody::Alias(typ) => write!(f, " = {typ}"),
        }
    }
}

impl PrettyDisplay for TypeDefExpr {
    fn fmt_pretty_inner(&self, buf: &mut PrettyBuf) -> fmt::Result {
        self.write_name_and_params(buf)?;
        match &self.body {
            TypeDefBody::Abstract(None) => writeln!(buf),
            TypeDefBody::Abstract(Some(rep)) => {
                write!(buf, " = Abstract<")?;
                rep.fmt_pretty(buf)?;
                buf.kill_newline();
                writeln!(buf, ">")
            }
            TypeDefBody::Alias(typ) => {
                write!(buf, " = ")?;
                typ.fmt_pretty(buf)
            }
        }
    }
}

impl fmt::Display for TraitMethod {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write_comments(f, self.comments.lines())?;
        write!(f, "{}val {}: {}", self.doc, self.name, self.typ)?;
        match &self.default {
            None => Ok(()),
            Some(d) => write!(f, " = {d}"),
        }
    }
}

impl PrettyDisplay for TraitMethod {
    fn fmt_pretty_inner(&self, buf: &mut PrettyBuf) -> fmt::Result {
        write_comments(buf, self.comments.lines())?;
        self.doc.fmt_pretty_inner(buf)?;
        write!(buf, "val {}: ", self.name)?;
        self.typ.fmt_pretty(buf)?;
        match &self.default {
            None => Ok(()),
            Some(d) => {
                buf.kill_newline();
                write!(buf, " =")?;
                pretty_tail(buf, d)
            }
        }
    }
}

impl fmt::Display for TraitExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "trait {} {{", self.name)?;
        for (i, m) in self.methods.iter().enumerate() {
            write!(f, " {m}")?;
            if i < self.methods.len() - 1 {
                write!(f, ";")?;
            }
        }
        write!(f, " }}")
    }
}

impl PrettyDisplay for TraitExpr {
    fn decorated(&self) -> bool {
        self.methods.iter().any(|m| {
            m.doc.0.is_some()
                || !m.comments.lines().is_empty()
                || m.default.as_ref().is_some_and(decorated)
        })
    }

    fn fmt_pretty_inner(&self, buf: &mut PrettyBuf) -> fmt::Result {
        writeln!(buf, "trait {} {{", self.name)?;
        buf.nested(|buf| {
            for (i, m) in self.methods.iter().enumerate() {
                m.fmt_pretty_inner(buf)?;
                if i < self.methods.len() - 1 {
                    buf.kill_newline();
                    writeln!(buf, ";")?;
                }
            }
            Ok(())
        })?;
        writeln!(buf, "}}")
    }
}

impl ImplExpr {
    fn write_head(&self, f: &mut impl fmt::Write) -> fmt::Result {
        write!(f, "impl")?;
        if !self.params.is_empty() {
            write!(f, "<")?;
            for (i, tv) in self.params.iter().enumerate() {
                write!(f, "{tv}")?;
                let mut first = true;
                for (ctv, c) in self.constraints.iter() {
                    if ctv.name == tv.name {
                        write!(f, "{}{c}", if first { ": " } else { " + " })?;
                        first = false;
                    }
                }
                if i < self.params.len() - 1 {
                    write!(f, ", ")?;
                }
            }
            write!(f, ">")?;
        }
        write!(f, " {} for {}", self.trait_name, self.target)
    }
}

impl fmt::Display for ImplExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.write_head(f)?;
        if self.methods.is_empty() {
            return Ok(());
        }
        write!(f, " {{")?;
        for (i, m) in self.methods.iter().enumerate() {
            write!(f, " {m}")?;
            if i < self.methods.len() - 1 {
                write!(f, ";")?;
            }
        }
        write!(f, " }}")
    }
}

impl PrettyDisplay for ImplExpr {
    fn decorated(&self) -> bool {
        self.methods.iter().any(decorated)
    }

    fn fmt_pretty_inner(&self, buf: &mut PrettyBuf) -> fmt::Result {
        self.write_head(buf)?;
        if self.methods.is_empty() {
            return writeln!(buf);
        }
        writeln!(buf, " {{")?;
        buf.nested(|buf| {
            for (i, m) in self.methods.iter().enumerate() {
                m.fmt_pretty(buf)?;
                if i < self.methods.len() - 1 {
                    buf.kill_newline();
                    writeln!(buf, ";")?;
                }
            }
            Ok(())
        })?;
        writeln!(buf, "}}")
    }
}

impl Sandbox {
    fn kind_and_list(&self) -> Option<(&str, &[crate::expr::ModPath])> {
        match self {
            Sandbox::Unrestricted => None,
            Sandbox::Blacklist(l) => Some(("blacklist", l)),
            Sandbox::Whitelist(l) => Some(("whitelist", l)),
        }
    }
}

impl fmt::Display for Sandbox {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self.kind_and_list() {
            None => write!(f, "sandbox unrestricted"),
            Some((kind, l)) => {
                write!(f, "sandbox {kind} [")?;
                for (i, p) in l.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?
                    }
                    write!(f, "{p}")?
                }
                write!(f, "]")
            }
        }
    }
}

impl PrettyDisplay for Sandbox {
    fn fmt_pretty_inner(&self, buf: &mut PrettyBuf) -> fmt::Result {
        match self.kind_and_list() {
            None => writeln!(buf, "sandbox unrestricted"),
            Some((kind, l)) => {
                writeln!(buf, "sandbox {kind} [")?;
                buf.nested(|buf| {
                    for (i, p) in l.iter().enumerate() {
                        let sep = if i + 1 < l.len() { "," } else { "" };
                        writeln!(buf, "{p}{sep}")?
                    }
                    Ok(())
                })?;
                writeln!(buf, "]")
            }
        }
    }
}

impl fmt::Display for BindSig {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "val {}: {}", self.name, self.typ)
    }
}

impl PrettyDisplay for BindSig {
    fn fmt_pretty_inner(&self, buf: &mut PrettyBuf) -> fmt::Result {
        write!(buf, "val {}: ", self.name)?;
        self.typ.fmt_pretty(buf)
    }
}

impl fmt::Display for SigItem {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write_comments(f, self.comments.lines())?;
        write!(f, "{}", self.doc)?;
        match &self.kind {
            SigKind::TypeDef(td) => write!(f, "{td}"),
            SigKind::Trait(t) => write!(f, "{t}"),
            SigKind::Impl(i) => write!(f, "{i}"),
            SigKind::Bind(bind) => write!(f, "{bind}"),
            SigKind::Module(name) => write!(f, "mod {name}"),
            SigKind::Use { reexport, names } => write!(f, "{}", UseStmt(*reexport, names)),
        }
    }
}

impl PrettyDisplay for SigItem {
    fn fmt_pretty_inner(&self, buf: &mut PrettyBuf) -> fmt::Result {
        write_comments(buf, self.comments.lines())?;
        self.doc.fmt_pretty_inner(buf)?;
        match &self.kind {
            SigKind::Bind(b) => b.fmt_pretty(buf),
            SigKind::TypeDef(d) => d.fmt_pretty(buf),
            SigKind::Trait(t) => t.fmt_pretty(buf),
            SigKind::Impl(i) => i.fmt_pretty(buf),
            SigKind::Module(name) => writeln!(buf, "mod {name}"),
            SigKind::Use { reexport, names } => UseStmt(*reexport, names).fmt_pretty(buf),
        }
    }
}

impl fmt::Display for Sig {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if !self.toplevel {
            write!(f, "sig {{ ")?;
        }
        for (i, si) in self.iter().enumerate() {
            write!(f, "{si}")?;
            if i < self.len() - 1 {
                write!(f, "; ")?
            }
        }
        if !self.toplevel {
            write!(f, " }}")?
        }
        Ok(())
    }
}

impl PrettyDisplay for Sig {
    fn decorated(&self) -> bool {
        self.items.iter().any(sig_item_decorated)
    }

    fn fmt_pretty_inner(&self, buf: &mut PrettyBuf) -> fmt::Result {
        if !self.toplevel {
            writeln!(buf, "sig {{")?;
        }
        if self.toplevel {
            pretty_file_items(buf, &self.items, |buf, si| si.fmt_pretty_inner(buf))?
        } else {
            buf.nested(|buf| {
                for (i, si) in self.iter().enumerate() {
                    si.fmt_pretty_inner(buf)?;
                    if i < self.len() - 1 {
                        buf.kill_newline();
                        writeln!(buf, ";")?
                    }
                }
                Ok(())
            })?
        }
        if !self.toplevel {
            writeln!(buf, "}}")?
        }
        Ok(())
    }
}

impl fmt::Display for BindExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let BindExpr { rec, pattern, typ, value } = self;
        let rec = if *rec { " rec" } else { "" };
        match typ {
            None => write!(f, "let{rec} {pattern} = {value}"),
            Some(typ) => write!(f, "let{rec} {pattern}: {typ} = {value}"),
        }
    }
}

impl PrettyDisplay for BindExpr {
    fn fmt_pretty_inner(&self, buf: &mut PrettyBuf) -> fmt::Result {
        let BindExpr { rec, pattern, typ, value } = self;
        let rec = if *rec { " rec" } else { "" };
        write!(buf, "let{rec} {pattern}")?;
        if let Some(typ) = typ {
            write!(buf, ": ")?;
            let start = buf.mark();
            write!(buf, "{typ} =")?;
            if buf.col() > buf.limit {
                buf.rollback(start);
                typ.fmt_pretty_inner(buf)?;
                buf.kill_newline();
                write!(buf, " =")?;
            }
        } else {
            write!(buf, " =")?;
        }
        pretty_tail(buf, value)
    }
}

/// Whether the field or labeled argument `name: e` is written `name`
/// alone.
fn puns(name: &str, e: &Expr) -> bool {
    matches!(&e.kind, ExprKind::Ref { name: n } if *n == [name])
        && !parser::is_reserved_binding(name)
}

/// The fields of a struct literal or a functional update, flat.
fn write_fields(f: &mut Formatter<'_>, fields: &[(ArcStr, Expr)]) -> fmt::Result {
    for (i, (name, e)) in as_written(fields).into_iter().enumerate() {
        if i > 0 {
            write!(f, ", ")?
        }
        write_leading(f, &e.dec)?;
        match puns(name, e) {
            true => write!(f, "{name}")?,
            false => write!(f, "{name}: {}", Bare(e))?,
        }
    }
    Ok(())
}

/// The fields of a struct literal or a functional update, one to a line.
fn pretty_fields(buf: &mut PrettyBuf, fields: &[(ArcStr, Expr)]) -> fmt::Result {
    buf.nested(|buf| {
        for (i, (name, e)) in as_written(fields).into_iter().enumerate() {
            if i > 0 {
                buf.kill_newline();
                writeln!(buf, ",")?
            }
            write_leading(buf, &e.dec)?;
            match puns(name, e) {
                true => writeln!(buf, "{name}")?,
                false => {
                    write!(buf, "{name}:")?;
                    pretty_tail_bare(buf, e)?
                }
            }
        }
        Ok(())
    })
}

impl StructWithExpr {
    fn write_head(&self, f: &mut impl Write) -> fmt::Result {
        match &self.source.kind {
            ExprKind::Ref { .. } => write!(f, "{{ {} with", self.source),
            _ => write!(f, "{{ ({}) with", self.source),
        }
    }
}

impl fmt::Display for StructWithExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.write_head(f)?;
        write!(f, " ")?;
        write_fields(f, &self.replace)?;
        write!(f, " }}")
    }
}

impl PrettyDisplay for StructWithExpr {
    fn fmt_pretty_inner(&self, buf: &mut PrettyBuf) -> fmt::Result {
        self.write_head(buf)?;
        writeln!(buf)?;
        pretty_fields(buf, &self.replace)?;
        writeln!(buf, "}}")
    }
}

impl fmt::Display for StructExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{{ ")?;
        write_fields(f, &self.args)?;
        write!(f, " }}")
    }
}

impl PrettyDisplay for StructExpr {
    fn fmt_pretty_inner(&self, buf: &mut PrettyBuf) -> fmt::Result {
        writeln!(buf, "{{")?;
        pretty_fields(buf, &self.args)?;
        writeln!(buf, "}}")
    }
}

/// Struct fields in the order they were written, which is the order of
/// their values' positions; compiler-built values have none and keep
/// the order of their names.
fn as_written(fields: &[(ArcStr, Expr)]) -> SmallVec<[&(ArcStr, Expr); 16]> {
    let mut fields: SmallVec<[&(ArcStr, Expr); 16]> = fields.iter().collect();
    if print_as_written() {
        fields.sort_by_key(|(_, e)| (e.pos.line, e.pos.column));
    }
    fields
}

/// Whether `e` can be the bare source of a postfix operator without parens:
/// true exactly for identifiers and postfix-chain nodes (`?`/`$` included). Anything else
/// must be parenthesized (`(a+b).c`; `(42).0` would lex as a float).
pub(super) fn prints_as_bare_postfix(e: &Expr) -> bool {
    matches!(
        &e.kind,
        ExprKind::Ref { .. }
            | ExprKind::StructRef { .. }
            | ExprKind::TupleRef { .. }
            | ExprKind::ArrayRef { .. }
            | ExprKind::ArraySlice { .. }
            | ExprKind::MapRef { .. }
            | ExprKind::Apply(_)
            | ExprKind::Qop(_)
            | ExprKind::OrNever(_)
    )
}

impl fmt::Display for ApplyExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let Self { args, function } = self;
        write!(f, "{}(", Postfix(function))?;
        for (i, (name, e)) in args.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?
            }
            match name {
                None => write!(f, "{e}")?,
                Some(name) if puns(name, e) => write!(f, "#{name}")?,
                Some(name) => write!(f, "#{name}: {e}")?,
            }
        }
        write!(f, ")")
    }
}

impl PrettyDisplay for ApplyExpr {
    fn fmt_pretty_inner(&self, buf: &mut PrettyBuf) -> fmt::Result {
        let Self { args, function } = self;
        if prints_as_bare_postfix(function) {
            function.fmt_pretty(buf)?
        } else {
            write!(buf, "(")?;
            function.fmt_pretty(buf)?;
            buf.kill_newline();
            write!(buf, ")")?;
        }
        buf.kill_newline();
        if let [(None, arg)] = &args[..]
            && hugs_parens(arg)
        {
            write!(buf, "(")?;
            arg.fmt_pretty(buf)?;
            buf.kill_newline();
            return writeln!(buf, ")");
        }
        writeln!(buf, "(")?;
        buf.nested(|buf| {
            for (i, (name, e)) in args.iter().enumerate() {
                if i > 0 {
                    buf.kill_newline();
                    writeln!(buf, ",")?
                }
                match name {
                    None => e.fmt_pretty(buf)?,
                    Some(name) if puns(name, e) => writeln!(buf, "#{name}")?,
                    Some(name) => {
                        write!(buf, "#{name}:")?;
                        pretty_tail(buf, e)?
                    }
                }
            }
            Ok(())
        })?;
        writeln!(buf, ")")
    }
}

impl fmt::Display for Arg {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if self.labeled.is_some() {
            write!(f, "#")?;
        }
        write!(f, "{}", self.pattern)?;
        if let Some(t) = &self.constraint {
            write!(f, ": {t}")?
        }
        match &self.labeled {
            Some(Some(def)) => write!(f, " = {def}"),
            Some(None) | None => Ok(()),
        }
    }
}

impl LambdaExpr {
    /// The quantifiers in front of the opening bar, and the space
    /// between them and it.
    fn write_constraints(&self, f: &mut impl Write) -> fmt::Result {
        for (i, (tvar, typ)) in self.constraints.iter().enumerate() {
            let sep = if i + 1 < self.constraints.len() { ", " } else { " " };
            write!(f, "{tvar}: {typ}{sep}")?;
        }
        Ok(())
    }

    /// The arguments between the bars, each followed by `sep` but the
    /// last, which `end` follows.
    fn write_args(&self, f: &mut impl Write, sep: &str, end: &str) -> fmt::Result {
        let n = self.args.len() + self.vargs.is_some() as usize;
        let after = |i: usize| if i + 1 < n { sep } else { end };
        for (i, a) in self.args.iter().enumerate() {
            write!(f, "{a}{}", after(i))?;
        }
        match &self.vargs {
            None => Ok(()),
            Some(None) => write!(f, "@args{end}"),
            Some(Some(typ)) => write!(f, "@args: {typ}{end}"),
        }
    }

    /// What follows the closing bar, up to the body.
    fn write_returns(&self, f: &mut impl Write) -> fmt::Result {
        match &self.rtype {
            None => (),
            Some(Type::Fn(ft)) => write!(f, " -> ({ft})")?,
            Some(Type::ByRef(t)) => match &**t {
                Type::Fn(ft) => write!(f, " -> &({ft})")?,
                t => write!(f, " -> &{t}")?,
            },
            Some(t) => write!(f, " -> {t}")?,
        }
        match &self.throws {
            None => Ok(()),
            Some(t) => write!(f, " throws {t}"),
        }
    }
}

impl fmt::Display for LambdaExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.write_constraints(f)?;
        write!(f, "|")?;
        self.write_args(f, ", ", "")?;
        write!(f, "|")?;
        self.write_returns(f)?;
        match &self.body {
            Either::Right(builtin) => write!(f, " '{builtin}"),
            Either::Left(body) => write!(f, " {body}"),
        }
    }
}

impl LambdaExpr {
    /// `write_returns` with the return type laid out over lines.
    fn pretty_returns(&self, buf: &mut PrettyBuf) -> fmt::Result {
        if let Some(rtype) = &self.rtype {
            let (open, typ, close): (&str, &dyn PrettyDisplay, &str) = match rtype {
                Type::Fn(ft) => (" -> (", &**ft, ")"),
                Type::ByRef(t) => match &**t {
                    Type::Fn(ft) => (" -> &(", &**ft, ")"),
                    t => (" -> &", t, ""),
                },
                t => (" -> ", t, ""),
            };
            write!(buf, "{open}")?;
            typ.fmt_pretty_inner(buf)?;
            buf.kill_newline();
            write!(buf, "{close}")?;
        }
        match &self.throws {
            None => Ok(()),
            Some(t) => write!(buf, " throws {t}"),
        }
    }
}

/// The head gives way a step at a time: all of it on one line with what
/// opens the body; else its arguments one to a line, closed by `| -> R`;
/// else the body under that closing line; else the return type itself
/// laid out over lines.
impl PrettyDisplay for LambdaExpr {
    fn fmt_pretty_inner(&self, buf: &mut PrettyBuf) -> fmt::Result {
        let start = buf.mark();
        self.write_constraints(buf)?;
        write!(buf, "|")?;
        self.write_args(buf, ", ", "")?;
        write!(buf, "|")?;
        self.write_returns(buf)?;
        let opener = match &self.body {
            Either::Right(builtin) => builtin.chars().count() + 3,
            Either::Left(_) => 2,
        };
        let has_args = !self.args.is_empty() || self.vargs.is_some();
        if buf.col() + opener > buf.limit && has_args {
            buf.rollback(start);
            self.write_constraints(buf)?;
            writeln!(buf, "|")?;
            buf.nested(|buf| self.write_args(buf, ",\n", "\n"))?;
            let closing = buf.mark();
            write!(buf, "|")?;
            self.write_returns(buf)?;
            if buf.col() > buf.limit && self.rtype.is_some() {
                buf.rollback(closing);
                write!(buf, "|")?;
                self.pretty_returns(buf)?;
            }
        }
        match &self.body {
            Either::Right(builtin) if buf.col() + opener > buf.limit => {
                writeln!(buf)?;
                buf.nested(|buf| writeln!(buf, "'{builtin}"))
            }
            Either::Right(builtin) => writeln!(buf, " '{builtin}"),
            Either::Left(body) => pretty_tail(buf, body),
        }
    }
}

impl fmt::Display for SelectExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let SelectExpr { arg, arms } = self;
        write!(f, "select {arg} {{ ")?;
        for (i, (pat, rhs)) in arms.iter().enumerate() {
            write_leading(f, &rhs.dec)?;
            if let Some(tp) = &pat.type_predicate {
                write!(f, "{tp} as ")?;
            }
            write!(f, "{} ", pat.structure_predicate)?;
            if let Some(guard) = &pat.guard {
                write!(f, "if {guard} ")?;
            }
            write!(f, "=> {}", Bare(rhs))?;
            if i < arms.len() - 1 {
                write!(f, ", ")?
            }
        }
        write!(f, " }}")
    }
}

impl PrettyDisplay for SelectExpr {
    fn fmt_pretty_inner(&self, buf: &mut PrettyBuf) -> fmt::Result {
        let SelectExpr { arg, arms } = self;
        write!(buf, "select ")?;
        arg.fmt_pretty(buf)?;
        buf.kill_newline();
        writeln!(buf, " {{")?;
        buf.nested(|buf| {
            for (i, (pat, expr)) in arms.iter().enumerate() {
                write_leading(buf, &expr.dec)?;
                if let Some(tp) = &pat.type_predicate {
                    write!(buf, "{tp} as ")?;
                }
                write!(buf, "{} ", pat.structure_predicate)?;
                if let Some(guard) = &pat.guard {
                    write!(buf, "if ")?;
                    buf.nested(|buf| guard.fmt_pretty(buf))?;
                    buf.kill_newline();
                    write!(buf, " ")?;
                }
                write!(buf, "=>")?;
                pretty_tail_bare(buf, expr)?;
                if i < arms.len() - 1 {
                    buf.kill_newline();
                    writeln!(buf, ",")?
                }
            }
            Ok(())
        })?;
        writeln!(buf, "}}")
    }
}

/// `e`, then `suffix` on the line `e` ends on: flat when both fit, else
/// `e` laid out over lines.
fn pretty_then(buf: &mut PrettyBuf, e: &Expr, suffix: &str) -> fmt::Result {
    if decorated(e) {
        e.fmt_pretty(buf)?
    } else {
        let start = buf.mark();
        let col = buf.col();
        write!(buf, "{}{suffix}", Bare(e))?;
        if col + buf.width_since(start) <= buf.limit {
            return writeln!(buf);
        }
        buf.rollback(start);
        Bare(e).fmt_pretty_inner(buf)?
    }
    buf.kill_newline();
    writeln!(buf, "{suffix}")
}

/// A postfix form: its source, parenthesized unless it reads bare, and
/// then the suffix.
fn pretty_postfix(buf: &mut PrettyBuf, source: &Expr, suffix: &str) -> fmt::Result {
    if prints_as_bare_postfix(source) {
        pretty_then(buf, source, suffix)
    } else {
        write!(buf, "(")?;
        pretty_then(buf, source, &format_compact!("){suffix}"))
    }
}

impl PrettyDisplay for ExprKind {
    fn decorated(&self) -> bool {
        kind_decorated(self)
    }

    fn fmt_pretty_inner(&self, buf: &mut PrettyBuf) -> fmt::Result {
        use ExprKind::*;
        if let Some((op, lhs, rhs)) = BinOp::of(self) {
            pretty_then(buf, lhs, &format_compact!(" {}", op.token()))?;
            return rhs.fmt_pretty(buf);
        }
        match self {
            NoOp => Ok(()),
            Use { reexport, names } => UseStmt(*reexport, names).fmt_pretty_inner(buf),
            Constant(_)
            | Ref { .. }
            | StringInterpolate { .. }
            | Module { value: ModuleKind::Unresolved { .. } | ModuleKind::Resolved { .. }, .. } => {
                writeln!(buf, "{self}")
            }
            StructRef { source, field } => {
                pretty_postfix(buf, source, &format_compact!(".{field}"))
            }
            TupleRef { source, field } => {
                pretty_postfix(buf, source, &format_compact!(".{field}"))
            }
            ArrayRef { source, i } => pretty_postfix(buf, source, &format_compact!("[{i}]")),
            MapRef { source, key } => {
                pretty_postfix(buf, source, &format_compact!("{{{key}}}"))
            }
            ArraySlice { source, start, end } => {
                pretty_postfix(buf, source, &format_compact!("{}", Slice(start, end)))
            }
            ExplicitParens(e) => {
                writeln!(buf, "(")?;
                buf.nested(|buf| e.fmt_pretty(buf))?;
                writeln!(buf, ")")
            }
            Do { exprs } => pretty_print_exprs(buf, exprs, "{", "}", ";", false),
            Seq { queued, trigger, abort, flush, body } => {
                write!(buf, "{} ", if *queued { "seqq" } else { "seq" })?;
                if let Some(t) = trigger {
                    if let SeqTrigger::Bind(b) = t {
                        write_seq_let(buf, b)?;
                    }
                    let t = t.expr();
                    let parens = trigger_needs_parens(t);
                    if parens {
                        write!(buf, "(")?;
                    }
                    t.fmt_pretty(buf)?;
                    buf.kill_newline();
                    if parens {
                        write!(buf, ")")?;
                    }
                }
                let mut first = trigger.is_none();
                for (name, e) in [("abort", abort), ("flush", flush)] {
                    if let Some(e) = e {
                        write!(buf, "{}{name}(", if first { "" } else { "; " })?;
                        first = false;
                        e.fmt_pretty(buf)?;
                        buf.kill_newline();
                        write!(buf, ")")?;
                    }
                }
                if !first {
                    write!(buf, " ")?;
                }
                pretty_print_exprs(buf, body, "{", "}", ";", false)
            }
            Until(e) => {
                write!(buf, "until ")?;
                e.fmt_pretty(buf)
            }
            TryWith(t) => {
                pretty_print_exprs(buf, &t.body, "try {", "}", ";", false)?;
                buf.kill_newline();
                match &t.constraint {
                    None => write!(buf, " with({}) ", t.bind)?,
                    Some(ty) => write!(buf, " with({}: {ty}) ", t.bind)?,
                }
                pretty_print_exprs(buf, &t.handler, "{", "}", ";", false)
            }
            Array { args } => pretty_print_exprs(buf, args, "[", "]", ",", false),
            List { args } => pretty_print_exprs(buf, args, "[<", ">]", ",", false),
            Tuple { args } => pretty_print_exprs(buf, args, "(", ")", ",", true),
            Bind(b) => b.fmt_pretty_inner(buf),
            TypeDef(td) => td.fmt_pretty_inner(buf),
            Trait(t) => t.fmt_pretty_inner(buf),
            Impl(i) => i.fmt_pretty_inner(buf),
            StructWith(sw) => sw.fmt_pretty_inner(buf),
            Module { name, value: ModuleKind::Dynamic { sandbox, sig, source } } => {
                writeln!(buf, "mod {name} dynamic {{")?;
                buf.nested(|buf| {
                    sandbox.fmt_pretty(buf)?;
                    buf.kill_newline();
                    writeln!(buf, ";")?;
                    sig.fmt_pretty(buf)?;
                    buf.kill_newline();
                    writeln!(buf, ";")?;
                    write!(buf, "source ")?;
                    buf.nested(|buf| source.fmt_pretty(buf))?;
                    buf.kill_newline();
                    writeln!(buf, ";")
                })?;
                writeln!(buf, "}}")
            }
            Connect { name, value, deref } => {
                let deref = if *deref { "*" } else { "" };
                write!(buf, "{deref}{name} <-")?;
                pretty_tail(buf, value)
            }
            TypeCast { expr, typ } => {
                writeln!(buf, "cast<{typ}>(")?;
                buf.nested(|buf| expr.fmt_pretty(buf))?;
                writeln!(buf, ")")
            }
            Map { args } => {
                writeln!(buf, "{{")?;
                buf.nested(|buf| {
                    for (i, (k, v)) in args.iter().enumerate() {
                        if i > 0 {
                            buf.kill_newline();
                            writeln!(buf, ",")?
                        }
                        write_leading(buf, &k.dec)?;
                        write!(buf, "{} =>", Bare(k))?;
                        pretty_tail(buf, v)?
                    }
                    Ok(())
                })?;
                writeln!(buf, "}}")
            }
            Any { args } => {
                write!(buf, "any")?;
                pretty_print_exprs(buf, args, "(", ")", ",", true)
            }
            Never { typ, args } => {
                match typ {
                    Some(t) => write!(buf, "never<{t}>")?,
                    None => write!(buf, "never")?,
                }
                pretty_print_exprs(buf, args, "(", ")", ",", true)
            }
            Variant { tag, args } if args.is_empty() => writeln!(buf, "`{tag}"),
            Variant { tag, args } => {
                write!(buf, "`{tag}")?;
                pretty_print_exprs(buf, args, "(", ")", ",", true)
            }
            Construct { name, arg } => {
                write!(buf, "{name}")?;
                pretty_print_exprs(buf, std::slice::from_ref(&**arg), "(", ")", ",", true)
            }
            Struct(st) => st.fmt_pretty_inner(buf),
            Qop(e) | Rethrow(e) => pretty_then(buf, e, "?"),
            SeqGuard(e) | SeqAbort(e) => e.fmt_pretty(buf),
            OrNever(e) => pretty_then(buf, e, "$"),
            Catch(c) => {
                match &c.constraint {
                    None => write!(buf, "catch({})", c.bind)?,
                    Some(t) => write!(buf, "catch({}: {t})", c.bind)?,
                }
                pretty_tail(buf, &c.handler)
            }
            Apply(ae) => ae.fmt_pretty_inner(buf),
            Lambda(l) => l.fmt_pretty_inner(buf),
            Not { expr } => match &expr.kind {
                Do { exprs } => pretty_print_exprs(buf, exprs, "!{", "}", ";", false),
                _ => {
                    write!(buf, "!")?;
                    expr.fmt_pretty(buf)
                }
            },
            ByRef(e) => {
                write!(buf, "&")?;
                e.fmt_pretty(buf)
            }
            Deref(e) => {
                write!(buf, "*")?;
                e.fmt_pretty(buf)
            }
            Neg(e) if matches!(e.kind, Constant(_)) => writeln!(buf, "{self}"),
            Neg(e) => {
                write!(buf, "-")?;
                e.fmt_pretty(buf)
            }
            Select(se) => se.fmt_pretty_inner(buf),
            Eq { .. }
            | Ne { .. }
            | Lt { .. }
            | Gt { .. }
            | Lte { .. }
            | Gte { .. }
            | And { .. }
            | Or { .. }
            | Add { .. }
            | CheckedAdd { .. }
            | Sub { .. }
            | CheckedSub { .. }
            | Mul { .. }
            | CheckedMul { .. }
            | Div { .. }
            | CheckedDiv { .. }
            | Mod { .. }
            | CheckedMod { .. }
            | Sample { .. }
            | StrictSample { .. } => unreachable!("BinOp::of matched it"),
        }
    }
}

/// The order a use statement lists names in: segment by segment, the
/// name that ends (`self`) first, then the path roots, then the rest by
/// name, a glob last.
pub(crate) fn cmp_use_items(a: &UseItem, b: &UseItem) -> Ordering {
    let key = use_seg_key;
    let (mut pa, mut pb) = (Path::parts(&a.path.0), Path::parts(&b.path.0));
    loop {
        let (sa, sb) = (pa.next(), pb.next());
        match key(sa).cmp(&key(sb)) {
            Ordering::Equal if sa.is_none() => break a.rename.cmp(&b.rename),
            Ordering::Equal => (),
            o => break o,
        }
    }
}

pub(crate) fn use_seg_key(s: Option<&str>) -> (u8, &str) {
    match s {
        None => (0, ""),
        Some("self") => (1, ""),
        Some("super") => (2, ""),
        Some("package") => (3, ""),
        Some("*") => (5, ""),
        Some(s) => (4, s),
    }
}

pub(crate) fn use_seg(item: &UseItem, depth: usize) -> Option<&str> {
    Path::parts(&item.path.0).nth(depth)
}

/// The names of a use statement as a tree of path segments: `items` is
/// sorted (`UseItem::sorted`) and shares its first `depth` segments.
#[derive(Clone, Copy)]
struct UseNames<'a> {
    items: &'a [UseItem],
    depth: usize,
}

impl<'a> UseNames<'a> {
    /// The entries at this level: a name that ends here, or every name
    /// continuing through one segment.
    fn entries(self) -> impl Iterator<Item = UseNames<'a>> {
        let UseNames { mut items, depth } = self;
        std::iter::from_fn(move || {
            let seg = use_seg(items.first()?, depth);
            let ends =
                |i: &UseItem| use_seg(i, depth) == seg && use_seg(i, depth + 1).is_none();
            // a name written twice, and a glob, are entries of their own:
            // neither can stand as a group's `self`
            let alone =
                ends(&items[0]) && (seg == Some("*") || items.get(1).is_some_and(ends));
            let n = match seg {
                None => 1,
                Some(_) if alone => 1,
                Some(_) => items.iter().take_while(|i| use_seg(i, depth) == seg).count(),
            };
            let (entry, rest) = items.split_at(n);
            items = rest;
            Some(UseNames { items: entry, depth })
        })
    }

    /// The names under this entry's segment.
    fn under(&self) -> UseNames<'a> {
        UseNames { items: self.items, depth: self.depth + 1 }
    }

    /// Write this entry up to a group that must be laid out: the rest
    /// of the entry when it has one, else nothing.
    fn write_path(&self, f: &mut impl Write) -> Result<Option<UseNames<'a>>, fmt::Error> {
        let mut this = *self;
        loop {
            match use_seg(&this.items[0], this.depth) {
                None => write!(f, "self")?,
                Some(seg) => {
                    write!(f, "{seg}")?;
                    let under = this.under();
                    let ends = under.items.len() == 1
                        && use_seg(&under.items[0], under.depth).is_none();
                    if !ends {
                        write!(f, "::")?;
                        let mut entries = under.entries();
                        match (entries.next(), entries.next()) {
                            (Some(only), None)
                                if use_seg(&only.items[0], only.depth).is_some() =>
                            {
                                this = only;
                                continue;
                            }
                            _ => return Ok(Some(under)),
                        }
                    }
                }
            }
            if let Some(n) = &this.items[0].rename {
                write!(f, " as {n}")?;
            }
            return Ok(None);
        }
    }

    fn write_entry(&self, f: &mut impl Write) -> fmt::Result {
        match self.write_path(f)? {
            None => Ok(()),
            Some(group) => group.write_group(f),
        }
    }

    fn write_group(&self, f: &mut impl Write) -> fmt::Result {
        write!(f, "{{")?;
        for (i, e) in self.entries().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            e.write_entry(f)?;
        }
        write!(f, "}}")
    }

    fn pretty_group(&self, buf: &mut PrettyBuf) -> fmt::Result {
        writeln!(buf, "{{")?;
        buf.nested::<fmt::Result, _>(|buf| {
            let n = self.entries().count();
            for (i, e) in self.entries().enumerate() {
                let start = buf.mark();
                e.write_entry(buf)?;
                if buf.width_since(start) >= buf.limit {
                    buf.rollback(start);
                    if let Some(group) = e.write_path(buf)? {
                        group.pretty_group(buf)?;
                        buf.kill_newline();
                    }
                }
                writeln!(buf, "{}", if i + 1 < n { "," } else { "" })?;
            }
            Ok(())
        })?;
        writeln!(buf, "}}")
    }
}

/// A use statement: whether it reexports, and its names.
struct UseStmt<'a>(bool, &'a [UseItem]);

impl fmt::Display for UseStmt<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write_use_names(f, self.0, self.1)
    }
}

impl PrettyDisplay for UseStmt<'_> {
    fn fmt_pretty_inner(&self, buf: &mut PrettyBuf) -> fmt::Result {
        pretty_use_names(buf, self.0, self.1)
    }
}

fn write_use_head(f: &mut impl Write, reexport: bool) -> fmt::Result {
    write!(f, "{}use ", if reexport { "pub " } else { "" })
}

/// Print a use statement's names as one tree, every shared prefix
/// written once (`use a::b`, `use a::{self, b, c::{d, e}}`, `use {a, b}`).
fn write_use_names(f: &mut impl Write, reexport: bool, names: &[UseItem]) -> fmt::Result {
    let names = UseNames { items: names, depth: 0 };
    write_use_head(f, reexport)?;
    let mut entries = names.entries();
    match (entries.next(), entries.next()) {
        (Some(only), None) => only.write_entry(f),
        _ => names.write_group(f),
    }
}

/// `write_use_names` with every group that does not fit its line laid
/// out one entry to a line.
fn pretty_use_names(
    buf: &mut PrettyBuf,
    reexport: bool,
    names: &[UseItem],
) -> fmt::Result {
    let names = UseNames { items: names, depth: 0 };
    write_use_head(buf, reexport)?;
    let mut entries = names.entries();
    match (entries.next(), entries.next()) {
        (Some(only), None) => match only.write_path(buf)? {
            None => writeln!(buf),
            Some(group) => group.pretty_group(buf),
        },
        _ => names.pretty_group(buf),
    }
}

/// Whether `s` prints as itself between raw-string delimiters.
fn raw_writable(s: &str) -> bool {
    !s.chars().any(|c| c.is_control() && c != '\n' && c != '\t')
}

/// Whether `s` prints between template delimiters, as itself or escaped.
fn template_writable(s: &str) -> bool {
    !s.chars().any(|c| c.is_control() && !matches!(c, '\n' | '\t' | '\r' | '\0'))
}

/// The hashes a raw string needs so that no `"#..` inside `s` closes it.
fn raw_hashes(s: &str) -> usize {
    let mut n = 0;
    let mut run: Option<usize> = None;
    for c in s.chars() {
        match (c, &mut run) {
            ('"', _) => run = Some(0),
            ('#', Some(r)) => {
                *r += 1;
                n = n.max(*r);
            }
            _ => run = None,
        }
    }
    if s.contains('"') { n + 1 } else { 0 }
}

fn write_raw(f: &mut Formatter<'_>, s: &str) -> fmt::Result {
    let n = raw_hashes(s);
    write!(f, "r{:#<n$}\"{s}\"{:#<n$}", "", "")
}

/// One part of a string literal: text, or a spliced expression.
#[derive(Clone, Copy)]
enum StrPart<'a> {
    Text(&'a str),
    Splice(&'a Expr),
}

impl<'a> StrPart<'a> {
    fn of(e: &'a Expr) -> Self {
        match &e.kind {
            ExprKind::Constant(Value::String(s)) if !s.is_empty() => Self::Text(s),
            _ => Self::Splice(e),
        }
    }

    fn text(&self) -> Option<&'a str> {
        match self {
            Self::Text(s) => Some(s),
            Self::Splice(_) => None,
        }
    }
}

/// A `"""template"""`. One newline after the opener is the parser's to
/// strip, so text that spans lines starts on its own line; a `"` that
/// would touch another quote is escaped so no `"""` forms inside.
fn write_template<'a>(
    f: &mut Formatter<'_>,
    parts: impl Iterator<Item = StrPart<'a>> + Clone,
) -> fmt::Result {
    write!(f, "\"\"\"")?;
    if parts.clone().filter_map(|p| p.text()).any(|s| s.contains('\n')) {
        writeln!(f)?
    }
    let mut parts = parts.peekable();
    while let Some(part) = parts.next() {
        let text = match part {
            StrPart::Text(s) => s,
            StrPart::Splice(e) => {
                write!(f, "\\[{e}]")?;
                continue;
            }
        };
        let quote_follows = match parts.peek() {
            None => true,
            Some(next) => next.text().is_some_and(|s| s.starts_with('"')),
        };
        let mut chars = text.chars().peekable();
        while let Some(c) = chars.next() {
            match c {
                '\\' => write!(f, "\\\\")?,
                '\t' => write!(f, "\\t")?,
                '\r' => write!(f, "\\r")?,
                '\0' => write!(f, "\\0")?,
                '"' => {
                    let touches = match chars.peek() {
                        Some(next) => *next == '"',
                        None => quote_follows,
                    };
                    write!(f, "{}\"", if touches { "\\" } else { "" })?
                }
                c => write!(f, "{c}")?,
            }
        }
    }
    write!(f, "\"\"\"")
}

/// A string constant between the delimiters its author chose when
/// printing as written, else raw exactly when it spans lines; quoted
/// wherever the delimiters cannot hold it.
fn write_str_constant(
    f: &mut Formatter<'_>,
    v: &Value,
    s: &str,
    form: StrForm,
) -> fmt::Result {
    let form = match form {
        form if print_as_written() => form,
        _ if s.contains('\n') => StrForm::Raw,
        _ => StrForm::Quoted,
    };
    match form {
        StrForm::Raw if raw_writable(s) => write_raw(f, s),
        StrForm::Template if template_writable(s) && !s.is_empty() => {
            write_template(f, std::iter::once(StrPart::Text(s)))
        }
        StrForm::Quoted | StrForm::Raw | StrForm::Template => {
            v.fmt_ext(f, &parser::GRAPHIX_ESC, true)
        }
    }
}

/// `write_str_constant` for an interpolated string.
fn write_interpolation(
    f: &mut Formatter<'_>,
    args: &[Expr],
    form: StrForm,
) -> fmt::Result {
    let parts = args.iter().map(StrPart::of);
    let texts = || parts.clone().filter_map(|p| p.text());
    let template = match form {
        form if print_as_written() => form == StrForm::Template,
        _ => texts().any(|s| s.contains('\n')),
    } && texts().all(template_writable);
    if template {
        return write_template(f, parts);
    }
    write!(f, "\"")?;
    for part in parts {
        match part {
            StrPart::Text(s) => write!(f, "{}", parser::GRAPHIX_ESC.escape(s))?,
            StrPart::Splice(e) => write!(f, "[{e}]")?,
        }
    }
    write!(f, "\"")
}

/// An expression without the lines above it, which its caller places.
pub(crate) struct Bare<'a>(pub &'a Expr);

impl fmt::Display for Bare<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let Expr { kind, str_form, .. } = self.0;
        match kind {
            ExprKind::Constant(v @ Value::String(s)) => {
                write_str_constant(f, v, s, *str_form)
            }
            ExprKind::StringInterpolate { args } => {
                write_interpolation(f, args, *str_form)
            }
            kind => write!(f, "{kind}"),
        }
    }
}

impl PrettyDisplay for Bare<'_> {
    fn decorated(&self) -> bool {
        kind_decorated(&self.0.kind)
    }

    fn fmt_pretty_inner(&self, buf: &mut PrettyBuf) -> fmt::Result {
        match &self.0.kind {
            ExprKind::Constant(Value::String(_)) | ExprKind::StringInterpolate { .. } => {
                writeln!(buf, "{self}")
            }
            kind => kind.fmt_pretty_inner(buf),
        }
    }
}

impl fmt::Display for ExprKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        crate::stack::ensure_sufficient(|| self.fmt_inner(f))
    }
}

impl ExprKind {
    fn fmt_inner(&self, f: &mut fmt::Formatter) -> fmt::Result {
        /// `exprs` separated by `sep`; the empty statement a trailing `;`
        /// leaves is that `;` and nothing more.
        fn print_exprs(
            f: &mut fmt::Formatter,
            exprs: &[Expr],
            open: &str,
            close: &str,
            sep: &str,
        ) -> fmt::Result {
            write!(f, "{open}")?;
            for (i, e) in exprs.iter().enumerate() {
                match (i, &e.kind) {
                    (0, _) => write!(f, "{e}")?,
                    (_, ExprKind::NoOp) => write!(f, "{}", sep.trim_end())?,
                    _ => write!(f, "{sep}{e}")?,
                }
            }
            write!(f, "{close}")
        }
        if let Some((op, lhs, rhs)) = BinOp::of(self) {
            return write!(f, "{lhs} {} {rhs}", op.token());
        }
        match self {
            ExprKind::Constant(v @ Value::String(s)) => {
                write_str_constant(f, v, s, StrForm::Quoted)
            }
            ExprKind::NoOp => Ok(()),
            ExprKind::ExplicitParens(e) => write!(f, "({e})"),
            ExprKind::Constant(v) => write!(f, "{}", Literal(v)),
            ExprKind::Bind(b) => write!(f, "{b}"),
            ExprKind::StructWith(sw) => write!(f, "{sw}"),
            ExprKind::Connect { name, value, deref } => {
                let deref = if *deref { "*" } else { "" };
                write!(f, "{deref}{name} <- {value}")
            }
            ExprKind::Use { reexport, names } => write!(f, "{}", UseStmt(*reexport, names)),
            ExprKind::Ref { name } => {
                write!(f, "{name}")
            }
            ExprKind::StructRef { source, field } => write!(f, "{}.{field}", Postfix(source)),
            ExprKind::TupleRef { source, field } => write!(f, "{}.{field}", Postfix(source)),
            ExprKind::Module {
                value:
                    ModuleKind::Resolved { from_interface: true, .. }
                    | ModuleKind::Unresolved { from_interface: true },
                ..
            } => Ok(()),
            ExprKind::Module { name, value } => {
                write!(f, "mod {name}")?;
                match value {
                    ModuleKind::Resolved { .. } | ModuleKind::Unresolved { .. } => Ok(()),
                    ModuleKind::Dynamic { sandbox, sig, source } => {
                        write!(f, " dynamic {{ {sandbox};")?;
                        write!(f, " {sig};")?;
                        write!(f, " source {source} }}")
                    }
                }
            }
            ExprKind::TypeCast { expr, typ } => write!(f, "cast<{typ}>({expr})"),
            ExprKind::TypeDef(td) => write!(f, "{td}"),
            ExprKind::Trait(t) => write!(f, "{t}"),
            ExprKind::Impl(i) => write!(f, "{i}"),
            ExprKind::Do { exprs } => print_exprs(f, &**exprs, "{ ", " }", "; "),
            ExprKind::Seq { queued, trigger, abort, flush, body } => {
                write!(f, "{} ", if *queued { "seqq" } else { "seq" })?;
                if let Some(t) = trigger {
                    if let SeqTrigger::Bind(b) = t {
                        write_seq_let(f, b)?;
                    }
                    let t = t.expr();
                    if trigger_needs_parens(t) {
                        write!(f, "({t})")?;
                    } else {
                        write!(f, "{t}")?;
                    }
                }
                let mut first = trigger.is_none();
                for (name, e) in [("abort", abort), ("flush", flush)] {
                    if let Some(e) = e {
                        write!(f, "{}{name}({e})", if first { "" } else { "; " })?;
                        first = false;
                    }
                }
                if !first {
                    write!(f, " ")?;
                }
                print_exprs(f, body, "{ ", " }", "; ")
            }
            ExprKind::Until(e) => write!(f, "until {e}"),
            ExprKind::TryWith(t) => {
                print_exprs(f, &t.body, "try { ", " }", "; ")?;
                match &t.constraint {
                    None => write!(f, " with({}) ", t.bind)?,
                    Some(ty) => write!(f, " with({}: {ty}) ", t.bind)?,
                }
                print_exprs(f, &t.handler, "{ ", " }", "; ")
            }
            ExprKind::Lambda(l) => write!(f, "{l}"),
            ExprKind::Array { args } => print_exprs(f, args, "[", "]", ", "),
            ExprKind::List { args } => print_exprs(f, args, "[<", ">]", ", "),
            ExprKind::Map { args } => {
                write!(f, "{{")?;
                for (i, (k, v)) in args.iter().enumerate() {
                    write!(f, "{k} => {v}")?;
                    if i < args.len() - 1 {
                        write!(f, ", ")?
                    }
                }
                write!(f, "}}")
            }
            ExprKind::MapRef { source, key } => write!(f, "{}{{{key}}}", Postfix(source)),
            ExprKind::Any { args } => {
                write!(f, "any")?;
                print_exprs(f, args, "(", ")", ", ")
            }
            ExprKind::Never { typ, args } => {
                match typ {
                    Some(t) => write!(f, "never<{t}>")?,
                    None => write!(f, "never")?,
                }
                print_exprs(f, args, "(", ")", ", ")
            }
            ExprKind::Tuple { args } => print_exprs(f, args, "(", ")", ", "),
            ExprKind::Variant { tag, args } if args.len() == 0 => {
                write!(f, "`{tag}")
            }
            ExprKind::Variant { tag, args } => {
                write!(f, "`{tag}")?;
                print_exprs(f, args, "(", ")", ", ")
            }
            ExprKind::Construct { name, arg } => write!(f, "{name}({arg})"),
            ExprKind::Struct(st) => write!(f, "{st}"),
            ExprKind::Qop(e) | ExprKind::Rethrow(e) => write!(f, "{}?", e),
            ExprKind::SeqGuard(e) | ExprKind::SeqAbort(e) => write!(f, "{e}"),
            ExprKind::OrNever(e) => write!(f, "{}$", e),
            ExprKind::Catch(c) => match &c.constraint {
                None => write!(f, "catch({}) {}", c.bind, c.handler),
                Some(t) => write!(f, "catch({}: {t}) {}", c.bind, c.handler),
            },
            ExprKind::StringInterpolate { args } => {
                write_interpolation(f, args, StrForm::Quoted)
            }
            ExprKind::ArrayRef { source, i } => write!(f, "{}[{i}]", Postfix(source)),
            ExprKind::ArraySlice { source, start, end } => {
                write!(f, "{}{}", Postfix(source), Slice(start, end))
            }
            ExprKind::Apply(ap) => write!(f, "{ap}"),
            ExprKind::Select(se) => write!(f, "{se}"),
            ExprKind::Eq { .. }
            | ExprKind::Ne { .. }
            | ExprKind::Lt { .. }
            | ExprKind::Gt { .. }
            | ExprKind::Lte { .. }
            | ExprKind::Gte { .. }
            | ExprKind::And { .. }
            | ExprKind::Or { .. }
            | ExprKind::Add { .. }
            | ExprKind::CheckedAdd { .. }
            | ExprKind::Sub { .. }
            | ExprKind::CheckedSub { .. }
            | ExprKind::Mul { .. }
            | ExprKind::CheckedMul { .. }
            | ExprKind::Div { .. }
            | ExprKind::CheckedDiv { .. }
            | ExprKind::Mod { .. }
            | ExprKind::CheckedMod { .. }
            | ExprKind::Sample { .. }
            | ExprKind::StrictSample { .. } => unreachable!("BinOp::of matched it"),
            ExprKind::ByRef(e) => write!(f, "&{e}"),
            ExprKind::Deref(e) => write!(f, "*{e}"),
            // `-1` reads back as the literal: a negated one keeps the space
            ExprKind::Neg(e) => match &e.kind {
                ExprKind::Constant(_) if e.dec.is_none() => write!(f, "- {}", Bare(e)),
                _ => write!(f, "-{e}"),
            },
            ExprKind::Not { expr } => write!(f, "!{expr}"),
        }
    }
}

/// The source of a postfix form, parenthesized unless it reads bare.
struct Postfix<'a>(&'a Expr);

impl fmt::Display for Postfix<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match prints_as_bare_postfix(self.0) {
            true => write!(f, "{}", self.0),
            false => write!(f, "({})", self.0),
        }
    }
}

/// The `[start..end]` of a slice.
struct Slice<'a>(&'a Option<Arc<Expr>>, &'a Option<Arc<Expr>>);

impl fmt::Display for Slice<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "[")?;
        if let Some(e) = self.0 {
            write!(f, "{e}")?
        }
        write!(f, "..")?;
        if let Some(e) = self.1 {
            write!(f, "{e}")?
        }
        write!(f, "]")
    }
}
