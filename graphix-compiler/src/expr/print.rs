// CR claude for eric: [style] `super::Sig` is `crate::expr::Sig`: one group with
// the rest. `fmt::Write` is imported at the top and again inside `fmt_flat`, and
// `Write`/`fmt::Write`, `Formatter`/`fmt::Formatter` are mixed through the file.
use super::Sig;
use crate::{
    expr::{
        ApplyExpr, Arg, Attr, BindExpr, BindSig, Decorations, Doc, Expr, ExprKind,
        ImplExpr, LambdaExpr, ModuleKind, Sandbox, SelectExpr, SeqTrigger, SigItem,
        SigKind, StrForm, StructExpr, StructWithExpr, TraitExpr, TraitMethod,
        TypeDefBody, TypeDefExpr, UseItem, parser,
    },
    print_as_written,
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

/// The `let pattern[: type] = ` of a bound seq trigger; the value
/// follows under the trigger's own parenthesization.
fn write_seq_let(f: &mut impl Write, b: &BindExpr) -> fmt::Result {
    let rec = if b.rec { " rec" } else { "" };
    match &b.typ {
        None => write!(f, "let{rec} {} = ", b.pattern),
        Some(typ) => write!(f, "let{rec} {}: {typ} = ", b.pattern),
    }
}

// CR claude for eric: [structure] The twenty binary operators are listed three
// times: here, in `fmt_inner`'s arms and in `fmt_pretty_inner`'s `binop!` arms.
// One `fn binop(&ExprKind) -> Option<(&str, &Expr, &Expr)>` that also returns
// the symbol would serve all three.
/// The operands of a binary operator, `None` for any other kind.
fn binop_operands(e: &ExprKind) -> Option<(&Expr, &Expr)> {
    use ExprKind::*;
    match e {
        Eq { lhs, rhs }
        | Ne { lhs, rhs }
        | Lt { lhs, rhs }
        | Gt { lhs, rhs }
        | Lte { lhs, rhs }
        | Gte { lhs, rhs }
        | And { lhs, rhs }
        | Or { lhs, rhs }
        | Add { lhs, rhs }
        | CheckedAdd { lhs, rhs }
        | Sub { lhs, rhs }
        | CheckedSub { lhs, rhs }
        | Mul { lhs, rhs }
        | CheckedMul { lhs, rhs }
        | Div { lhs, rhs }
        | CheckedDiv { lhs, rhs }
        | Mod { lhs, rhs }
        | CheckedMod { lhs, rhs }
        | Sample { lhs, rhs }
        | StrictSample { lhs, rhs } => Some((lhs, rhs)),
        _ => None,
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
        match (binop_operands(&e.kind), bare_postfix_source(&e.kind)) {
            (Some((lhs, _)), _) => leftmost(lhs, false),
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
            k => match (binop_operands(k), bare_postfix_source(k)) {
                (Some((lhs, rhs)), _) => reads_bare(lhs) && reads_bare(rhs),
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

// CR claude for eric: [structure] The projection `F` is only ever the identity
// (`pretty_print_exprs` is the one caller), and hugging is switched on by
// comparing `close` to ")". Take `&[Expr]` and say `hug: bool`.
fn pretty_print_exprs_int<'a, A, F: Fn(&'a A) -> &'a Expr>(
    buf: &mut PrettyBuf,
    exprs: &'a [A],
    open: &str,
    close: &str,
    sep: &str,
    f: F,
) -> fmt::Result {
    if exprs.is_empty() {
        return writeln!(buf, "{open}{close}");
    }
    if let ([e], ")") = (exprs, close)
        && hugs_parens(f(e))
    {
        write!(buf, "{open}")?;
        f(e).fmt_pretty(buf)?;
        buf.kill_newline();
        return writeln!(buf, "{close}");
    }
    writeln!(buf, "{}", open)?;
    buf.nested::<fmt::Result, _>(|buf| {
        for i in 0..exprs.len() {
            f(&exprs[i]).fmt_pretty(buf)?;
            if i < exprs.len() - 1 {
                buf.kill_newline();
                writeln!(buf, "{}", sep)?
            }
        }
        Ok(())
    })?;
    writeln!(buf, "{}", close)
}

fn pretty_print_exprs(
    buf: &mut PrettyBuf,
    exprs: &[Expr],
    open: &str,
    close: &str,
    sep: &str,
) -> fmt::Result {
    pretty_print_exprs_int(buf, exprs, open, close, sep, |a| a)
}

/// A body laid out inline after its head (`|x| {`, `=> {`, `catch(e) {`).
/// The body's own decorations are the caller's to place.
fn pretty_body(
    buf: &mut PrettyBuf,
    body: &Expr,
    open: &str,
    close: &str,
    sep: &str,
) -> fmt::Result {
    match &body.kind {
        ExprKind::Do { exprs } => pretty_print_exprs(buf, exprs, open, close, sep),
        _ => Bare(body).fmt_pretty(buf),
    }
}

/// The `;`-separated items of a file: a blank line stands on both sides
/// of every item that spans lines, and runs of one-line items stay tight.
pub(crate) fn pretty_file_items<T>(
    buf: &mut PrettyBuf,
    items: &[T],
    item: impl Fn(&mut PrettyBuf, &T) -> fmt::Result,
) -> fmt::Result {
    let mut prev_spans_lines = false;
    for (i, it) in items.iter().enumerate() {
        let start = buf.len();
        item(buf, it)?;
        let spans_lines = buf.buf[start..].trim_end_matches('\n').contains('\n');
        if i > 0 && (spans_lines || prev_spans_lines) {
            buf.buf.insert(start, '\n')
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
// CR claude for eric: [style] A dynamic module (`mod m dynamic {`) opens with a
// bracket too but is missing, so `let s = mod m dynamic {` moves under its head
// (probed).
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
        | ExplicitParens(_) => true,
        Variant { args, .. } => !args.is_empty(),
        Qop(e) | OrNever(e) | Rethrow(e) | ByRef(e) | Deref(e) | Neg(e) => {
            opens_with_bracket(&e.kind)
        }
        Not { expr } => opens_with_bracket(&expr.kind),
        _ => false,
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
    write!(buf, " ")?;
    if Bare(e).fmt_flat(buf)? {
        return Ok(());
    }
    // CR claude for eric: [perf] Exponential. The whole multi-line layout is
    // rendered only to measure its first line, then thrown away and rendered
    // again nested; once the indent passes the width every level fails and
    // doubles the work. probe: `let s = { f: { f: .. 1 .. } }` 38 deep formats
    // in 0.66s, 42 deep in 10.6s (`graphix fmt`; the LSP's formatting request
    // too). Measure the head (`{`, `f(`, `|a| {`) without rendering the body.
    if opens_with_bracket(&e.kind) {
        let start = buf.len();
        let col = buf.col();
        Bare(e).fmt_pretty_inner(buf)?;
        let first = buf.buf[start..].lines().next().map_or(0, |l| l.chars().count());
        if col + first <= buf.limit {
            return Ok(());
        }
        buf.buf.truncate(start);
    }
    buf.buf.pop();
    writeln!(buf)?;
    buf.nested(|buf| Bare(e).fmt_pretty(buf))
}

/// The lines above a decorated expression: its comments, then its
/// attributes.
pub(crate) fn write_leading(
    f: &mut impl fmt::Write,
    dec: &Option<Box<Decorations>>,
) -> fmt::Result {
    if let Some(dec) = dec {
        for c in dec.comments.iter() {
            writeln!(f, "//{c}")?;
        }
        for a in dec.attrs.iter() {
            writeln!(f, "{a}")?;
        }
    }
    Ok(())
}

/// A literal as source text: `i64` and `f64`, the types an unprefixed
/// number reads as, print bare.
pub(crate) struct Literal<'a>(pub &'a Value);

impl fmt::Display for Literal<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self.0 {
            Value::I64(v) => write!(f, "{v}"),
            // CR claude for eric: [readability] `{v}` never uses an exponent:
            // `1e300` formats to a 303-character literal, `6.02e23` to
            // `602000000000000000000000.0` (probed). `{v:?}` prints `1e300`,
            // `1000.0` and `0.5`, and makes the `fract` arm unnecessary.
            Value::F64(v) if v.is_finite() && v.fract() == 0. => write!(f, "{v}.0"),
            Value::F64(v) if v.is_finite() => write!(f, "{v}"),
            v => v.fmt_ext(f, &VAL_ESC, true),
        }
    }
}

/// The spaces one level of nesting indents by, unless configured.
pub const DEFAULT_INDENT: usize = 4;

// CR claude for eric: [structure] Every field is public and callers edit the raw
// string: the `truncate`/`pop`/`insert` in `pretty_file_items`, `pretty_tail_bare`,
// `fmt_flat`, SigItem's use, BindExpr, LambdaExpr, `pretty_group` and
// typ/print.rs are one idiom, try then roll back. A `mark()`/`rollback(mark)`
// pair would name it and keep the buffer private.
#[derive(Debug)]
pub struct PrettyBuf {
    pub indent: usize,
    /// what `nested` adds to `indent`
    pub step: usize,
    pub limit: usize,
    pub buf: LPooled<String>,
}

impl PrettyBuf {
    pub fn new(limit: usize) -> Self {
        Self { indent: 0, step: DEFAULT_INDENT, limit, buf: LPooled::take() }
    }

    /// Run `f` one level of nesting deeper.
    pub fn nested<R, F: FnOnce(&mut Self) -> R>(&mut self, f: F) -> R {
        self.with_indent(self.step, f)
    }

    pub fn len(&self) -> usize {
        self.buf.len()
    }

    pub fn newline(&self) -> bool {
        self.buf.chars().next_back().map(|c| c == '\n').unwrap_or(true)
    }

    pub fn push_indent(&mut self) {
        if self.newline() {
            self.buf.extend((0..self.indent).into_iter().map(|_| ' '));
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
        self.width_from(self.buf.rfind('\n').map_or(0, |i| i + 1))
    }

    /// The width, in characters, of what was written from `start` on.
    pub fn width_from(&self, start: usize) -> usize {
        self.buf[start..].chars().count()
    }

    pub fn kill_newline(&mut self) {
        if let Some('\n') = self.buf.chars().next_back() {
            self.buf.pop();
        }
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

// CR claude for eric: [structure] Two printers, not one: each node has a flat
// `Display` and a hand-written multi-line twin that must agree token for token
// (Struct, StructWith, Apply, Select, Seq, TryWith, Trait, Impl, Sig, Sandbox,
// Doc, the `let` head in `write_seq_let` and BindExpr, `write_returns` and
// `pretty_returns`). The Construct, Deref, Sandbox and catch layout bugs below
// are drift between twins. Describing each node once (text, group, break) and
// rendering it flat or broken would remove the twins, the three fits tests and
// the re-render in `pretty_tail_bare`.
pub trait PrettyDisplay: fmt::Display {
    // CR claude for eric: [risk] The multi-line recursion (`fmt_pretty` ->
    // `fmt_pretty_inner` -> a child's `fmt_pretty`) is not under
    // `ensure_sufficient`; only the flat `Display` is, and CLAUDE.md counts
    // printing as guarded. No overflow today (990-deep arrays format on a
    // 256K stack); `StructurePattern`'s Display (pattern.rs) is unguarded too.
    /// The multi-line layout; `fmt_pretty` calls it when the single-line
    /// form does not fit.
    fn fmt_pretty_inner(&self, buf: &mut PrettyBuf) -> fmt::Result;

    // CR claude for eric: [bug] A flat form holding a comment, doc or attribute
    // is accepted: `write_leading` puts a newline inside it, so the comment
    // trails the token before it and the next line starts unindented. probe:
    // `let y = select x {\n // c\n `A => 1,\n _ => 2\n}` formats to
    // `let y = select x { // c\n`A => 1, _ => 2 };`, and stdlib core/mod.gxi to
    // `trait Eq { /// true if ..`. A node with a decorated descendant must take
    // the multi-line layout.
    /// Write the single-line form and a newline if it fits the rest of
    /// the line, else write nothing.
    fn fmt_flat(&self, buf: &mut PrettyBuf) -> Result<bool, fmt::Error> {
        use fmt::Write;
        let start = buf.len();
        let col = buf.col();
        writeln!(buf, "{}", self)?;
        // Best-effort: embedded newlines overcount and a long token can
        // exceed any limit. The newline counts as the column kept for
        // the `;` or `,` that follows.
        let fits = col + buf.width_from(start) <= buf.limit;
        if !fits {
            buf.buf.truncate(start);
        }
        Ok(fits)
    }

    // CR claude for eric: [readability] Stale: `pretty_fmt` is `fmt_pretty_inner`.
    /// Format on a single line when it fits, else via `pretty_fmt`.
    fn fmt_pretty(&self, buf: &mut PrettyBuf) -> fmt::Result {
        if self.fmt_flat(buf)? { Ok(()) } else { self.fmt_pretty_inner(buf) }
    }

    /// Pretty print to a pooled string
    fn to_string_pretty(&self, limit: usize) -> LPooled<String> {
        let mut buf = PrettyBuf::new(limit);
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
            TypeDefBody::Abstract(None) => Ok(()),
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
        write!(f, "{}val {}: {}", self.doc, self.name, self.typ)?;
        match &self.default {
            None => Ok(()),
            Some(d) => write!(f, " = {d}"),
        }
    }
}

impl PrettyDisplay for TraitMethod {
    fn fmt_pretty_inner(&self, buf: &mut PrettyBuf) -> fmt::Result {
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

impl fmt::Display for Sandbox {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        macro_rules! write_sandbox {
            ($kind:literal, $l:expr) => {{
                write!(f, "sandbox {} [ ", $kind)?;
                for (i, p) in $l.iter().enumerate() {
                    if i < $l.len() - 1 {
                        write!(f, "{}, ", p)?
                    } else {
                        write!(f, "{}", p)?
                    }
                }
                write!(f, " ]")
            }};
        }
        match self {
            Sandbox::Unrestricted => write!(f, "sandbox unrestricted"),
            Sandbox::Blacklist(l) => write_sandbox!("blacklist", l),
            Sandbox::Whitelist(l) => write_sandbox!("whitelist", l),
        }
    }
}

// CR claude for eric: [bug] Writes a trailing space after `[` and after every
// `,`, closes with ` ]` one column right of the line that opened it, and ends
// without the newline the other layouts end with. probe: a long whitelist prints
// `sandbox whitelist [ `, `core, `, .., `         ];`. The flat `[ a, b ]`
// spacing differs from every other bracket list as well.
impl PrettyDisplay for Sandbox {
    fn fmt_pretty_inner(&self, buf: &mut PrettyBuf) -> fmt::Result {
        macro_rules! write_sandbox {
            ($kind:literal, $l:expr) => {{
                writeln!(buf, "sandbox {} [ ", $kind)?;
                buf.nested::<fmt::Result, _>(|buf| {
                    for (i, p) in $l.iter().enumerate() {
                        if i < $l.len() - 1 {
                            writeln!(buf, "{}, ", p)?
                        } else {
                            writeln!(buf, "{}", p)?
                        }
                    }
                    Ok(())
                })?;
                write!(buf, " ]")
            }};
        }
        match self {
            Sandbox::Blacklist(l) => write_sandbox!("blacklist", l),
            Sandbox::Whitelist(l) => write_sandbox!("whitelist", l),
            Sandbox::Unrestricted => writeln!(buf, "sandbox unrestricted"),
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
        write!(f, "{}", self.doc)?;
        match &self.kind {
            SigKind::TypeDef(td) => write!(f, "{td}"),
            SigKind::Trait(t) => write!(f, "{t}"),
            SigKind::Impl(i) => write!(f, "{i}"),
            SigKind::Bind(bind) => write!(f, "{bind}"),
            SigKind::Module(name) => write!(f, "mod {name}"),
            SigKind::Use { reexport, names } => write_use_names(f, *reexport, names),
        }
    }
}

impl PrettyDisplay for SigItem {
    fn fmt_pretty_inner(&self, buf: &mut PrettyBuf) -> fmt::Result {
        self.doc.fmt_pretty_inner(buf)?;
        match &self.kind {
            SigKind::Bind(b) => b.fmt_pretty(buf),
            SigKind::TypeDef(d) => d.fmt_pretty(buf),
            SigKind::Trait(t) => t.fmt_pretty(buf),
            SigKind::Impl(i) => i.fmt_pretty(buf),
            SigKind::Module(name) => writeln!(buf, "mod {name}"),
            // CR claude for eric: [bug] A second fits test for a use statement,
            // `> limit` without the column the `;` takes, where `ExprKind::Use`
            // goes through `fmt_flat`. probe: a 90-column `use` (91 with its `;`)
            // breaks in a .gx and stays one 91-column line in a .gxi. Route both
            // through one printer.
            SigKind::Use { reexport, names } => {
                let start = buf.len();
                write_use_names(buf, *reexport, names)?;
                if buf.width_from(start) > buf.limit {
                    buf.buf.truncate(start);
                    pretty_use_names(buf, *reexport, names)
                } else {
                    writeln!(buf)
                }
            }
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
            let start = buf.len();
            write!(buf, "{typ} =")?;
            if buf.col() > buf.limit {
                buf.buf.truncate(start);
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

impl fmt::Display for StructWithExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let Self { source, replace } = self;
        match &source.kind {
            ExprKind::Ref { .. } => write!(f, "{{ {source} with ")?,
            _ => write!(f, "{{ ({source}) with ")?,
        }
        for (i, (name, e)) in as_written(replace).into_iter().enumerate() {
            write_leading(f, &e.dec)?;
            // CR claude for eric: [structure] The field-pun test (a one-segment
            // ref equal to the name, not reserved) is written four times (here,
            // StructWith's pretty, StructExpr's Display and pretty), twice more
            // for labeled arguments in ApplyExpr (without the reserved check) and
            // once in pattern.rs. One `fn puns(name, &Expr) -> bool`; `ModPath`
            // already compares to `[&str; 1]`.
            match &e.kind {
                ExprKind::Ref { name: n }
                    if Path::dirname(&**n).is_none()
                        && Path::basename(&**n) == Some(&**name)
                        && !parser::RESERVED_BINDING.contains(&name.as_str()) =>
                {
                    write!(f, "{name}")?
                }
                _ => write!(f, "{name}: {}", Bare(e))?,
            }
            if i < replace.len() - 1 {
                write!(f, ", ")?
            }
        }
        write!(f, " }}")
    }
}

impl PrettyDisplay for StructWithExpr {
    fn fmt_pretty_inner(&self, buf: &mut PrettyBuf) -> fmt::Result {
        let Self { source, replace } = self;
        match &source.kind {
            ExprKind::Ref { .. } => writeln!(buf, "{{ {source} with")?,
            _ => writeln!(buf, "{{ ({source}) with")?,
        }
        buf.nested::<fmt::Result, _>(|buf| {
            for (i, (name, e)) in as_written(replace).into_iter().enumerate() {
                write_leading(buf, &e.dec)?;
                match &e.kind {
                    ExprKind::Ref { name: n }
                        if Path::dirname(&**n).is_none()
                            && Path::basename(&**n) == Some(&**name)
                            && !parser::RESERVED_BINDING.contains(&name.as_str()) =>
                    {
                        writeln!(buf, "{name}")?
                    }
                    _ => {
                        write!(buf, "{name}:")?;
                        pretty_tail_bare(buf, e)?
                    }
                }
                if i < replace.len() - 1 {
                    buf.kill_newline();
                    writeln!(buf, ",")?
                }
            }
            Ok(())
        })?;
        writeln!(buf, "}}")
    }
}

impl fmt::Display for StructExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let Self { args } = self;
        write!(f, "{{ ")?;
        for (i, (n, e)) in as_written(args).into_iter().enumerate() {
            write_leading(f, &e.dec)?;
            match &e.kind {
                ExprKind::Ref { name }
                    if Path::dirname(&**name).is_none()
                        && Path::basename(&**name) == Some(&**n)
                        && !parser::RESERVED_BINDING.contains(&n.as_str()) =>
                {
                    write!(f, "{n}")?
                }
                _ => write!(f, "{n}: {}", Bare(e))?,
            }
            if i < args.len() - 1 {
                write!(f, ", ")?
            }
        }
        write!(f, " }}")
    }
}

impl PrettyDisplay for StructExpr {
    fn fmt_pretty_inner(&self, buf: &mut PrettyBuf) -> fmt::Result {
        let Self { args } = self;
        writeln!(buf, "{{")?;
        buf.nested::<fmt::Result, _>(|buf| {
            for (i, (n, e)) in as_written(args).into_iter().enumerate() {
                write_leading(buf, &e.dec)?;
                match &e.kind {
                    ExprKind::Ref { name }
                        if Path::dirname(&**name).is_none()
                            && Path::basename(&**name) == Some(&**n)
                            && !parser::RESERVED_BINDING.contains(&n.as_str()) =>
                    {
                        writeln!(buf, "{n}")?
                    }
                    _ => {
                        write!(buf, "{n}:")?;
                        pretty_tail_bare(buf, e)?;
                    }
                }
                if i < args.len() - 1 {
                    buf.kill_newline();
                    writeln!(buf, ",")?
                }
            }
            Ok(())
        })?;
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
        if prints_as_bare_postfix(function) {
            write!(f, "{function}")?
        } else {
            write!(f, "({function})")?
        }
        write!(f, "(")?;
        for i in 0..args.len() {
            match &args[i].0 {
                None => write!(f, "{}", &args[i].1)?,
                Some(name) => match &args[i].1.kind {
                    ExprKind::Ref { name: n }
                        if Path::dirname(&n.0).is_none()
                            && Path::basename(&n.0) == Some(name.as_str()) =>
                    {
                        write!(f, "#{name}")?
                    }
                    _ => write!(f, "#{name}: {}", &args[i].1)?,
                },
            }
            if i < args.len() - 1 {
                write!(f, ", ")?
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
        buf.nested::<fmt::Result, _>(|buf| {
            for i in 0..args.len() {
                match &args[i].0 {
                    None => args[i].1.fmt_pretty(buf)?,
                    Some(name) => match &args[i].1.kind {
                        ExprKind::Ref { name: n }
                            if Path::dirname(&n.0).is_none()
                                && Path::basename(&n.0) == Some(name.as_str()) =>
                        {
                            writeln!(buf, "#{name}")?
                        }
                        _ => {
                            write!(buf, "#{name}:")?;
                            pretty_tail(buf, &args[i].1)?
                        }
                    },
                }
                if i < args.len() - 1 {
                    buf.kill_newline();
                    writeln!(buf, ",")?
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
    // CR claude for eric: [style] No space between the quantifiers and the bar:
    // `'a: Int |a: 'a, b: 'a|` (stdlib core, the book) formats to
    // `'a: Int|a: 'a, b: 'a|` (probed).
    /// The quantifiers in front of the opening bar.
    fn write_constraints(&self, f: &mut impl Write) -> fmt::Result {
        for (i, (tvar, typ)) in self.constraints.iter().enumerate() {
            write!(f, "{tvar}: {typ}")?;
            if i < self.constraints.len() - 1 {
                write!(f, ", ")?;
            }
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
        let start = buf.len();
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
            buf.buf.truncate(start);
            self.write_constraints(buf)?;
            writeln!(buf, "|")?;
            buf.nested(|buf| self.write_args(buf, ",\n", "\n"))?;
            let closing = buf.len();
            write!(buf, "|")?;
            self.write_returns(buf)?;
            if buf.col() > buf.limit && self.rtype.is_some() {
                buf.buf.truncate(closing);
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

impl PrettyDisplay for ExprKind {
    fn fmt_pretty_inner(&self, buf: &mut PrettyBuf) -> fmt::Result {
        // CR claude for eric: [bug] The left operand is always written flat, so a
        // left-leaning chain (`a + b + c`, the parser's shape) breaks only before
        // its last operand. probe: a six-term sum of long names prints a
        // 101-column line, then `sigma_tau;`. Break a chain at each operator.
        macro_rules! binop {
            ($sep:literal, $lhs:expr, $rhs:expr) => {{
                writeln!(buf, "{} {}", $lhs, $sep)?;
                $rhs.fmt_pretty(buf)
            }};
        }
        match self {
            ExprKind::Use { reexport, names } => pretty_use_names(buf, *reexport, names),
            // CR claude for eric: [bug] A field, index, slice or map access never
            // breaks, so neither does the call in front of it. probe: `let v =
            // some_function_with_long_name(argument_number_one,
            // argument_number_two, argument_three).field` prints a 97-column
            // line, where the same call with `?` breaks. Lay the source out with
            // `fmt_pretty` and append the suffix, as `Qop` does.
            ExprKind::Constant(_)
            | ExprKind::NoOp
            | ExprKind::Ref { .. }
            | ExprKind::StructRef { .. }
            | ExprKind::TupleRef { .. }
            | ExprKind::ArrayRef { .. }
            | ExprKind::MapRef { .. }
            | ExprKind::ArraySlice { .. }
            | ExprKind::StringInterpolate { .. }
            | ExprKind::Module {
                name: _,
                value: ModuleKind::Unresolved { .. } | ModuleKind::Resolved { .. },
            } => {
                writeln!(buf, "{self}")
            }
            ExprKind::ExplicitParens(e) => {
                writeln!(buf, "(")?;
                buf.nested(|buf| e.fmt_pretty(buf))?;
                writeln!(buf, ")")
            }
            ExprKind::Do { exprs } => pretty_print_exprs(buf, exprs, "{", "}", ";"),
            ExprKind::Seq { queued, trigger, abort, flush, body } => {
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
                pretty_print_exprs(buf, body, "{", "}", ";")
            }
            ExprKind::Until(e) => {
                write!(buf, "until ")?;
                e.fmt_pretty(buf)
            }
            ExprKind::TryWith(t) => {
                pretty_print_exprs(buf, &t.body, "try {", "}", ";")?;
                buf.kill_newline();
                match &t.constraint {
                    None => write!(buf, " with({}) ", t.bind)?,
                    Some(ty) => write!(buf, " with({}: {ty}) ", t.bind)?,
                }
                pretty_print_exprs(buf, &t.handler, "{", "}", ";")
            }
            ExprKind::Array { args } => pretty_print_exprs(buf, args, "[", "]", ","),
            ExprKind::List { args } => pretty_print_exprs(buf, args, "[<", ">]", ","),
            ExprKind::Tuple { args } => pretty_print_exprs(buf, args, "(", ")", ","),
            ExprKind::Bind(b) => b.fmt_pretty(buf),
            ExprKind::TypeDef(td) => td.fmt_pretty(buf),
            ExprKind::Trait(t) => t.fmt_pretty(buf),
            ExprKind::Impl(i) => i.fmt_pretty(buf),
            ExprKind::StructWith(sw) => sw.fmt_pretty(buf),
            ExprKind::Module {
                name,
                value: ModuleKind::Dynamic { sandbox, sig, source },
            } => {
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
            ExprKind::Connect { name, value, deref } => {
                let deref = if *deref { "*" } else { "" };
                write!(buf, "{deref}{name} <-")?;
                pretty_tail(buf, value)
            }
            ExprKind::TypeCast { expr, typ } => {
                writeln!(buf, "cast<{typ}>(")?;
                buf.nested(|buf| expr.fmt_pretty(buf))?;
                writeln!(buf, ")")
            }
            // CR claude for eric: [bug] Entries are written flat (`{k} => {v}`)
            // whatever their width. probe: `{"alpha" => f(long, args, ..),
            // "beta" => 2}` prints a 104-column entry. The value should take
            // `pretty_tail` as a struct field's does.
            ExprKind::Map { args } => {
                writeln!(buf, "{{")?;
                buf.nested::<fmt::Result, _>(|buf| {
                    for (i, (k, v)) in args.iter().enumerate() {
                        writeln!(buf, "{k} => {v}")?;
                        if i < args.len() - 1 {
                            buf.kill_newline();
                            writeln!(buf, ",")?
                        }
                    }
                    Ok(())
                })?;
                writeln!(buf, "}}")
            }
            ExprKind::Any { args } => {
                write!(buf, "any")?;
                pretty_print_exprs(buf, args, "(", ")", ",")
            }
            ExprKind::Never { typ, args } => {
                match typ {
                    Some(t) => write!(buf, "never<{t}>")?,
                    None => write!(buf, "never")?,
                }
                pretty_print_exprs(buf, args, "(", ")", ",")
            }
            // CR claude for eric: [structure] Ends without the newline every other
            // layout ends with (Sandbox's lists too). That contract is unstated,
            // and the ~40 `kill_newline` calls in this file exist to cope with
            // either. State it on `fmt_pretty_inner` and keep it. `is_empty()`.
            ExprKind::Variant { tag: _, args } if args.len() == 0 => {
                write!(buf, "{self}")
            }
            ExprKind::Variant { tag, args } => {
                write!(buf, "`{tag}")?;
                pretty_print_exprs(buf, args, "(", ")", ",")
            }
            // CR claude for eric: [bug] `writeln!` puts the argument on the next
            // line. probe: a `Counter(a + b + ..)` too long for its line prints
            // `let e = Counter\n(\n    alpha_beta_gamma + ..\n);` and a hugged
            // call as `Counter\n(f(..));` at column 0. Use `write!`.
            ExprKind::Construct { name, arg } => {
                writeln!(buf, "{name}")?;
                pretty_print_exprs(buf, std::slice::from_ref(&**arg), "(", ")", ",")
            }
            ExprKind::Struct(st) => st.fmt_pretty(buf),
            ExprKind::Qop(e) | ExprKind::Rethrow(e) => {
                e.fmt_pretty(buf)?;
                buf.kill_newline();
                writeln!(buf, "?")
            }
            ExprKind::SeqGuard(e) | ExprKind::SeqAbort(e) => e.fmt_pretty(buf),
            ExprKind::OrNever(e) => {
                e.fmt_pretty(buf)?;
                buf.kill_newline();
                writeln!(buf, "$")
            }
            // CR claude for eric: [bug] The `"; "` separator leaves a trailing
            // space on every broken handler statement (probed: `println(msg); `
            // then newline); every other statement list passes ";".
            // `pretty_body` has this one caller: inline it.
            ExprKind::Catch(c) => {
                match &c.constraint {
                    None => write!(buf, "catch({}) ", c.bind)?,
                    Some(t) => write!(buf, "catch({}: {t}) ", c.bind)?,
                }
                write_leading(buf, &c.handler.dec)?;
                pretty_body(buf, &c.handler, "{", "}", "; ")
            }
            ExprKind::Apply(ae) => ae.fmt_pretty(buf),
            ExprKind::Lambda(l) => l.fmt_pretty(buf),
            ExprKind::Eq { lhs, rhs } => binop!("==", lhs, rhs),
            ExprKind::Ne { lhs, rhs } => binop!("!=", lhs, rhs),
            ExprKind::Lt { lhs, rhs } => binop!("<", lhs, rhs),
            ExprKind::Gt { lhs, rhs } => binop!(">", lhs, rhs),
            ExprKind::Lte { lhs, rhs } => binop!("<=", lhs, rhs),
            ExprKind::Gte { lhs, rhs } => binop!(">=", lhs, rhs),
            ExprKind::And { lhs, rhs } => binop!("&&", lhs, rhs),
            ExprKind::Or { lhs, rhs } => binop!("||", lhs, rhs),
            ExprKind::Add { lhs, rhs } => binop!("+", lhs, rhs),
            ExprKind::CheckedAdd { lhs, rhs } => binop!("+?", lhs, rhs),
            ExprKind::Sub { lhs, rhs } => binop!("-", lhs, rhs),
            ExprKind::CheckedSub { lhs, rhs } => binop!("-?", lhs, rhs),
            ExprKind::Mul { lhs, rhs } => binop!("*", lhs, rhs),
            ExprKind::CheckedMul { lhs, rhs } => binop!("*?", lhs, rhs),
            ExprKind::Div { lhs, rhs } => binop!("/", lhs, rhs),
            ExprKind::CheckedDiv { lhs, rhs } => binop!("/?", lhs, rhs),
            ExprKind::Mod { lhs, rhs } => binop!("%", lhs, rhs),
            ExprKind::CheckedMod { lhs, rhs } => binop!("%?", lhs, rhs),
            ExprKind::Sample { lhs, rhs } => binop!("~", lhs, rhs),
            ExprKind::StrictSample { lhs, rhs } => binop!("~!", lhs, rhs),
            ExprKind::Not { expr } => match &expr.kind {
                ExprKind::Do { exprs } => pretty_print_exprs(buf, exprs, "!{", "}", ";"),
                _ => {
                    write!(buf, "!")?;
                    expr.fmt_pretty(buf)
                }
            },
            ExprKind::ByRef(e) => {
                write!(buf, "&")?;
                e.fmt_pretty(buf)
            }
            // CR claude for eric: [bug] `nested` indents the operand's inner lines
            // one step too far and closes its bracket a step right of the line
            // that opened it. probe: `let v = *f(a, b, c, d)` too long prints the
            // arguments at 8 and `);` at 4, where `&f(..)` and `!f(..)` print 4
            // and 0. Drop `nested`, as ByRef does.
            ExprKind::Deref(e) => {
                write!(buf, "*")?;
                buf.nested(|buf| e.fmt_pretty(buf))
            }
            ExprKind::Neg(e) if matches!(e.kind, ExprKind::Constant(_)) => {
                writeln!(buf, "{self}")
            }
            ExprKind::Neg(e) => {
                write!(buf, "-")?;
                e.fmt_pretty(buf)
            }
            ExprKind::Select(se) => se.fmt_pretty(buf),
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
                let start = buf.len();
                e.write_entry(buf)?;
                if buf.width_from(start) >= buf.limit {
                    buf.buf.truncate(start);
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
        fn print_exprs(
            f: &mut fmt::Formatter,
            exprs: &[Expr],
            open: &str,
            close: &str,
            sep: &str,
        ) -> fmt::Result {
            write!(f, "{open}")?;
            for i in 0..exprs.len() {
                write!(f, "{}", &exprs[i])?;
                if i < exprs.len() - 1 {
                    write!(f, "{sep}")?
                }
            }
            write!(f, "{close}")
        }
        match self {
            ExprKind::Constant(v @ Value::String(s)) => {
                write_str_constant(f, v, s, StrForm::Quoted)
            }
            // CR claude for eric: [bug] An empty statement (a trailing `;`) prints
            // as nothing and leaves debris in every layout: `{ a; b;  }` flat, a
            // line of indent spaces in a broken block (book gui/data_table_*.gx),
            // a blank last line when a file ends in `;`. Print it as part of the
            // `;` that precedes it.
            ExprKind::NoOp => Ok(()),
            ExprKind::ExplicitParens(e) => write!(f, "({e})"),
            ExprKind::Constant(v) => write!(f, "{}", Literal(v)),
            ExprKind::Bind(b) => write!(f, "{b}"),
            ExprKind::StructWith(sw) => write!(f, "{sw}"),
            ExprKind::Connect { name, value, deref } => {
                let deref = if *deref { "*" } else { "" };
                write!(f, "{deref}{name} <- {value}")
            }
            ExprKind::Use { reexport, names } => write_use_names(f, *reexport, names),
            ExprKind::Ref { name } => {
                write!(f, "{name}")
            }
            ExprKind::StructRef { source, field } => {
                if prints_as_bare_postfix(source) {
                    write!(f, "{source}.{field}")
                } else {
                    write!(f, "({source}).{field}")
                }
            }
            ExprKind::TupleRef { source, field } => {
                if prints_as_bare_postfix(source) {
                    write!(f, "{source}.{field}")
                } else {
                    write!(f, "({source}).{field}")
                }
            }
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
            ExprKind::MapRef { source, key } => {
                if prints_as_bare_postfix(source) {
                    write!(f, "{source}{{{key}}}")
                } else {
                    write!(f, "({source}){{{key}}}")
                }
            }
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
            ExprKind::ArrayRef { source, i } => {
                if prints_as_bare_postfix(source) {
                    write!(f, "{}[{}]", source, i)
                } else {
                    write!(f, "({})[{}]", &source, &i)
                }
            }
            // CR claude for eric: [perf] Formats both bounds into temporary
            // strings only to write them again: write the source, `[`, the
            // bounds and `..` straight to `f`. The bare-or-parenthesized source
            // test is also repeated for StructRef, TupleRef, MapRef, ArrayRef
            // and ApplyExpr; one `Postfix(source)` Display would hold it.
            ExprKind::ArraySlice { source, start, end } => {
                let s = match start.as_ref() {
                    None => "",
                    Some(e) => &format_compact!("{e}"),
                };
                let e = match &end.as_ref() {
                    None => "",
                    Some(e) => &format_compact!("{e}"),
                };
                if prints_as_bare_postfix(source) {
                    write!(f, "{}[{}..{}]", source, s, e)
                } else {
                    write!(f, "({})[{}..{}]", source, s, e)
                }
            }
            ExprKind::Apply(ap) => write!(f, "{ap}"),
            ExprKind::Select(se) => write!(f, "{se}"),
            ExprKind::Eq { lhs, rhs } => write!(f, "{lhs} == {rhs}"),
            ExprKind::Ne { lhs, rhs } => write!(f, "{lhs} != {rhs}"),
            ExprKind::Gt { lhs, rhs } => write!(f, "{lhs} > {rhs}"),
            ExprKind::Lt { lhs, rhs } => write!(f, "{lhs} < {rhs}"),
            ExprKind::Gte { lhs, rhs } => write!(f, "{lhs} >= {rhs}"),
            ExprKind::Lte { lhs, rhs } => write!(f, "{lhs} <= {rhs}"),
            ExprKind::And { lhs, rhs } => write!(f, "{lhs} && {rhs}"),
            ExprKind::Or { lhs, rhs } => write!(f, "{lhs} || {rhs}"),
            ExprKind::Add { lhs, rhs } => write!(f, "{lhs} + {rhs}"),
            ExprKind::CheckedAdd { lhs, rhs } => write!(f, "{lhs} +? {rhs}"),
            ExprKind::Sub { lhs, rhs } => write!(f, "{lhs} - {rhs}"),
            ExprKind::CheckedSub { lhs, rhs } => write!(f, "{lhs} -? {rhs}"),
            ExprKind::Mul { lhs, rhs } => write!(f, "{lhs} * {rhs}"),
            ExprKind::CheckedMul { lhs, rhs } => write!(f, "{lhs} *? {rhs}"),
            ExprKind::Div { lhs, rhs } => write!(f, "{lhs} / {rhs}"),
            ExprKind::CheckedDiv { lhs, rhs } => write!(f, "{lhs} /? {rhs}"),
            ExprKind::Mod { lhs, rhs } => write!(f, "{lhs} % {rhs}"),
            ExprKind::CheckedMod { lhs, rhs } => write!(f, "{lhs} %? {rhs}"),
            ExprKind::Sample { lhs, rhs } => write!(f, "{lhs} ~ {rhs}"),
            ExprKind::StrictSample { lhs, rhs } => write!(f, "{lhs} ~! {rhs}"),
            ExprKind::ByRef(e) => write!(f, "&{e}"),
            ExprKind::Deref(e) => write!(f, "*{e}"),
            // CR claude for eric: [readability] A negated literal prints its type
            // prefix: `- 8` formats to `-i64:8` and `- 1.5` to `-f64:1.5`, against
            // the rule that i64 and f64 print bare. `- 8` (with the space) reads
            // back as the negation too.
            // `-1` reads back as the literal, so a negated one keeps its type
            ExprKind::Neg(e) => match &e.kind {
                ExprKind::Constant(v @ (Value::I64(0..) | Value::F64(_)))
                    if e.dec.is_none() =>
                {
                    write!(f, "-")?;
                    v.fmt_ext(f, &VAL_ESC, true)
                }
                _ => write!(f, "-{e}"),
            },
            ExprKind::Not { expr } => write!(f, "!{expr}"),
        }
    }
}
