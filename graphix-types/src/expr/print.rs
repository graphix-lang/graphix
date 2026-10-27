use crate::{
    Restore,
    expr::{
        ApplyExpr, Arg, Attr, BinOp, BindExpr, BindSig, CatchRole, Comments, Decorations,
        Doc, Expr, ExprKind, ImplExpr, LambdaBody, LambdaExpr, ModuleKind, Sandbox,
        SelectExpr, SeqExpr, SeqTrigger, Sig, SigItem, SigKind, StrForm, StructExpr,
        StructWithExpr, StructurePattern, TraitExpr, TraitMethod, TypeDefBody,
        TypeDefExpr, UseItem, format::FormatConfig, parser, pattern::printed_fields,
    },
    print_as_written,
    stack::ensure_sufficient,
    typ::{
        Type,
        fntyp::{Ret, write_bounds},
    },
};
use arcstr::ArcStr;
use compact_str::{CompactString, format_compact};
use netidx_core::path::Path;
use netidx_value::Value;
use poolshark::local::LPooled;
use smallvec::SmallVec;
use std::{
    cell::Cell,
    cmp::Ordering,
    fmt::{self, Formatter, Write},
};
use triomphe::Arc;

/// A seq's head up to its body: keyword, `let`, trigger and clauses, the
/// trigger written by `trigger` (told whether it needs parentheses) and
/// each clause's expression, between parentheses, by `clause`.
fn write_seq_head<W: Write>(
    w: &mut W,
    s: &SeqExpr,
    trigger: impl FnOnce(&mut W, &Expr, bool) -> fmt::Result,
    mut clause: impl FnMut(&mut W, &Expr) -> fmt::Result,
) -> fmt::Result {
    write!(w, "{} ", if s.kind.queued() { "seqq" } else { "seq" })?;
    if let Some(t) = &s.trigger {
        let bound = matches!(t, SeqTrigger::Bind(_));
        if let SeqTrigger::Bind(b) = t {
            write_seq_let(w, b)?;
        }
        match trigger_parens(t.expr(), bound) {
            TriggerParens::None => trigger(w, t.expr(), false)?,
            TriggerParens::Whole => trigger(w, t.expr(), true)?,
            TriggerParens::Source(source) => {
                let _paren = Restore::replace(&PAREN_SOURCE, std::ptr::from_ref(source));
                trigger(w, t.expr(), false)?
            }
        }
    }
    let mut first = s.trigger.is_none();
    for (name, e) in [("abort", s.abort.as_ref()), ("flush", s.kind.flush())] {
        if let Some(e) = e {
            write!(w, "{}{name}", if first { "" } else { "; " })?;
            first = false;
            clause(w, e)?;
        }
    }
    if !first {
        write!(w, " ")?;
    }
    Ok(())
}

/// `write_seq_head` on one line.
fn write_flat_seq_head(f: &mut impl Write, s: &SeqExpr) -> fmt::Result {
    write_seq_head(
        f,
        s,
        |f, t, parens| match parens {
            true => write!(f, "({t})"),
            false => write!(f, "{t}"),
        },
        |f, e| write!(f, "({e})"),
    )
}

/// `write_seq_head` laid out, the trigger's continuation lines nested.
fn pretty_seq_head(buf: &mut PrettyBuf, s: &SeqExpr) -> fmt::Result {
    write_seq_head(
        buf,
        s,
        |buf, t, parens| {
            if parens {
                write!(buf, "(")?;
            }
            buf.nested(|buf| t.fmt_pretty(buf))?;
            buf.kill_newline();
            if parens {
                write!(buf, ")")?;
            }
            Ok(())
        },
        |buf, e| {
            pretty_parens(buf, e)?;
            buf.kill_newline();
            Ok(())
        },
    )
}

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
        Qop { arg: e, .. } | OrNever(e) => e,
        _ => return None,
    };
    prints_as_bare_postfix(source).then_some(&**source)
}

/// Where a seq trigger is parenthesized so the head parser reads it back.
enum TriggerParens<'a> {
    None,
    Whole,
    /// the source the trigger's postfix chain starts from, which reads
    /// back bare where the whole trigger in parentheses would not
    Source(&'a Expr),
}

thread_local! {
    /// The postfix source a seq head prints in parentheses, null when
    /// there is none.
    static PAREN_SOURCE: Cell<*const Expr> = const { Cell::new(std::ptr::null()) };
}

/// A seq trigger is parenthesized exactly where the head parser would
/// not read it back bare: a leading `{` is the body and a call of `flush`
/// the clause (not after `let p =`), a map access is refused outside
/// brackets, and the head admits operator expressions only.
fn trigger_parens(t: &Expr, bound: bool) -> TriggerParens<'_> {
    use ExprKind::*;
    // the expression whose first token the head parser meets first, and
    // the postfix form it is the source of
    fn leftmost<'a>(
        e: &'a Expr,
        postfix: Option<&'a ExprKind>,
    ) -> (&'a Expr, Option<&'a ExprKind>) {
        match (&e.kind, bare_postfix_source(&e.kind)) {
            (ExprKind::BinOp { lhs, .. }, _) => leftmost(lhs, None),
            (k, Some(source)) => leftmost(source, Some(k)),
            (_, None) => (e, postfix),
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
            | Block { .. }
            | Select(_)
            | Seq(_)
            | Variant { .. }
            | Construct { .. }
            | TypeCast { .. }
            | Never { .. }
            | Any { .. }
            | Live(_)
            | StringInterpolate { .. } => true,
            Deref(e) | Not { expr: e } | Neg(e) | ByRef(_, e) => reads_bare(e),
            // a lambda last operand's body is a whole expression
            ExprKind::BinOp { lhs, rhs, .. } => {
                reads_bare(lhs) && (matches!(rhs.kind, Lambda(_)) || reads_bare(rhs))
            }
            k => match bare_postfix_source(k) {
                Some(source) => reads_bare(source),
                None => matches!(
                    k,
                    StructRef { .. }
                        | TupleRef { .. }
                        | ArrayRef { .. }
                        | ArraySlice { .. }
                        | Apply(_)
                        | Qop { .. }
                        | OrNever(_)
                ),
            },
        }
    }
    let (first, postfix) = leftmost(t, None);
    let reads_as_body_or_clause = match first {
        _ if bound => false,
        Expr { kind: Ref { name }, .. } if matches!(postfix, Some(Apply(_))) => {
            &*name.0 == "/flush"
        }
        e => matches!(&e.kind, Block { .. } | Struct(_) | StructWith(_) | Map { .. }),
    };
    match (reads_bare(t), reads_as_body_or_clause, postfix) {
        (false, _, _) | (true, true, None) => TriggerParens::Whole,
        (true, true, Some(_)) => TriggerParens::Source(first),
        (true, false, _) => TriggerParens::None,
    }
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
        && hug_parens(buf, open, e, close)?
    {
        return Ok(());
    }
    writeln!(buf, "{open}")?;
    buf.without_tail(|buf| buf.nested(|buf| pretty_items(buf, exprs, sep)))?;
    writeln!(buf, "{close}")
}

/// Each of `exprs` on its own line, `sep` after all but the last. The
/// empty statement a trailing `;` leaves is that `;` and nothing more.
fn pretty_items(buf: &mut PrettyBuf, exprs: &[Expr], sep: &str) -> fmt::Result {
    // XCR claude for claude: [bug/medium] When a `;` list ends in its separator, its
    // last element is the NoOp `statements` makes for the empty slot. The parser
    // (`commented`'s `place`) puts the comments below that separator on the
    // statement before it. Here, and in `pretty_file_items`, only the last
    // element's trailing comments are printed, so they are dropped and the
    // formatter refuses its own output. A file `let a = 1;\nlet b = 2;\n// the
    // end\n` cannot be formatted, nor can a block or seq ending `2;\n// below\n}`.
    // Print the trailing comments of the last item that is not the trailing NoOp,
    // after the NoOp's `;`. probe:
    // design/review-2026-10-26/repro/t-parser-a-below-after-trailing-semi.sh
    // (t-parser-a-01)
    // 2026-10-26 claude: The comments below a trailing `;` now ride its empty
    // statement: `commented` returns the lines below the last item
    // (`Listed::below`) and `statements` hangs them on the list's last element, the
    // trailing NoOp included, so `item_end` prints them after the `;`;
    // `pretty_items`, `pretty_file_items` and the trait/sig/impl printers share
    // `pretty_separated`. Pins: format.rs an_empty_statement_is_its_semicolon and
    // comments_after_list_items_stay, and expr::test expr_pp_round_trip, which now
    // generates below lines on a trailing NoOp.
    let below = exprs.last().map(below_of).unwrap_or(&[]);
    pretty_separated(buf, exprs, sep, pretty_item, eol_of, below)
}

/// A list item: the empty statement a trailing `;` leaves prints nothing.
pub(crate) fn pretty_item(buf: &mut PrettyBuf, e: &Expr) -> fmt::Result {
    match e.kind {
        ExprKind::NoOp => Ok(()),
        _ => e.fmt_pretty(buf),
    }
}

/// Each of `items` on its own line, `sep` after all but the last: each
/// line ends with its item's `eol` comment, and `below` follows the last.
fn pretty_separated<T>(
    buf: &mut PrettyBuf,
    items: &[T],
    sep: &str,
    mut item: impl FnMut(&mut PrettyBuf, &T) -> fmt::Result,
    eol: impl Fn(&T) -> Option<&ArcStr>,
    below: &[ArcStr],
) -> fmt::Result {
    for (i, it) in items.iter().enumerate() {
        item(buf, it)?;
        match i < items.len() - 1 {
            true => item_sep(buf, sep, eol(it))?,
            false => item_end(buf, eol(it), below)?,
        }
    }
    if items.is_empty() {
        item_end(buf, None, below)?
    }
    Ok(())
}

/// The `;`-separated items of a file, as [`pretty_separated`]: a blank
/// line stands on both sides of every item that spans lines, and runs of
/// one-line items stay tight. An item that prints nothing (a trailing
/// `;`'s empty statement) is its separator alone.
pub(crate) fn pretty_file_items<T>(
    buf: &mut PrettyBuf,
    items: &[T],
    item: impl Fn(&mut PrettyBuf, &T) -> fmt::Result,
    eol: impl Fn(&T) -> Option<&ArcStr>,
    below: &[ArcStr],
) -> fmt::Result {
    let mut prev_spans_lines = None;
    let spaced = |buf: &mut PrettyBuf, it: &T| {
        let start = buf.mark();
        item(buf, it)?;
        let spans_lines = buf.since(start).trim_end_matches('\n').contains('\n');
        if prev_spans_lines.is_some_and(|prev| prev || spans_lines) {
            buf.insert_newline(start)
        }
        prev_spans_lines = Some(spans_lines);
        Ok(())
    };
    pretty_separated(buf, items, ";", spaced, eol, below)
}

/// Whether the multi-line layout of `e` opens with a short head and a
/// bracket, closing at its own indent: it can sit on the line of
/// whatever introduces it.
fn opens_with_bracket(e: &ExprKind) -> bool {
    use ExprKind::*;
    match e {
        Block { .. }
        | Lambda(_)
        | Select(_)
        | Seq(_)
        | TryWith(_)
        | Struct(_)
        | StructWith(_)
        | Array { .. }
        | List { .. }
        | Tuple { .. }
        | Map { .. }
        | Apply(_)
        | Any { .. }
        | Live(_)
        | Never { .. }
        | Construct { .. }
        | TypeCast { .. }
        | ExplicitParens(_) => true,
        Module { value, .. } => matches!(&**value, ModuleKind::Dynamic { .. }),
        Variant { args, .. } => !args.is_empty(),
        StructRef { .. }
        | TupleRef { .. }
        | ArrayRef { .. }
        | ArraySlice { .. }
        | MapRef { .. } => {
            bare_postfix_source(e).is_some_and(|s| opens_with_bracket(&s.kind))
        }
        Qop { arg: e, .. }
        | OrNever(e)
        | Rethrow(e)
        | ByRef(_, e)
        | Deref(e)
        | Neg(e) => opens_with_bracket(&e.kind),
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

/// Whether `x` prints flat in `room` columns, written no further than that.
fn fits_flat(x: &impl fmt::Display, room: usize) -> bool {
    struct Room(usize);
    impl Write for Room {
        fn write_str(&mut self, s: &str) -> fmt::Result {
            let n = s.chars().count();
            if n > self.0 {
                return Err(fmt::Error);
            }
            self.0 -= n;
            Ok(())
        }
    }
    write!(Room(room), "{x}").is_ok()
}

/// What `f` writes, as a `Display`.
struct Writes<F>(F);

impl<F: Fn(&mut Formatter<'_>) -> fmt::Result> fmt::Display for Writes<F> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        (self.0)(f)
    }
}

fn writes<F: Fn(&mut Formatter<'_>) -> fmt::Result>(f: F) -> Writes<F> {
    Writes(f)
}

/// Whether the first line of `e`'s multi-line layout fits from `col`. A
/// subexpression its head holds (a scrutinee, a source, a callee, a
/// trigger) counts flat where it fits flat, else by its own head: nothing
/// is laid out to measure.
fn head_fits(col: usize, limit: usize, e: &ExprKind) -> bool {
    use ExprKind::*;
    // `x` from `col`, `after` more columns following it on its line
    let sub = |col: usize, x: &Expr, after: usize| {
        shows(&x.dec)
            || (!decorated(x) && fits_flat(x, limit.saturating_sub(col + after)))
            || (opens_with_bracket(&x.kind) && head_fits(col, limit, &x.kind))
    };
    ensure_sufficient(|| match e {
        Select(se) => sub(col + "select ".len(), &se.arg, " {".len()),
        StructWith(sw) => sub(col + "{ (".len(), &sw.source, ") with".len()),
        Apply(a) if !matches!(a.function.kind, Ref { .. }) => {
            match prints_as_bare_postfix(&a.function) {
                true => sub(col, &a.function, "(".len()),
                false => sub(col + "(".len(), &a.function, ")(".len()),
            }
        }
        Lambda(l) => col + flat_width(&writes(|f| l.write_constraints(f))) < limit,
        Seq(s) => {
            let head = writes(|f| write_flat_seq_head(f, s));
            fits_flat(&head, limit.saturating_sub(col + "{".len()))
                || match &s.trigger {
                    None => true,
                    Some(t) => {
                        let kw = if s.kind.queued() { "seqq " } else { "seq " };
                        let bind = match t {
                            SeqTrigger::Bind(b) => {
                                flat_width(&writes(|f| write_seq_let(f, b)))
                            }
                            SeqTrigger::Expr(_) => 0,
                        };
                        sub(col + kw.len() + bind, t.expr(), 0)
                    }
                }
        }
        k => col + head_min(k) <= limit,
    })
}

/// The least the first line of `e`'s multi-line layout can take: the text
/// in front of its bracket that no layout breaks.
fn head_min(e: &ExprKind) -> usize {
    use ExprKind::*;
    match e {
        List { .. } => 2,
        TryWith(_) => "try {".len(),
        Seq(s) if s.kind.queued() => "seqq {".len(),
        Seq(_) => "seq {".len(),
        Select(_) => "select _ {".len(),
        StructWith(_) => "{ _ with".len(),
        Any { .. } => "any(".len(),
        Live(_) => "live(".len(),
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
        Qop { arg: e, .. }
        | OrNever(e)
        | Rethrow(e)
        | StructRef { source: e, .. }
        | TupleRef { source: e, .. }
        | ArrayRef { source: e, .. }
        | ArraySlice { source: e, .. }
        | MapRef { source: e, .. } => head_min(&e.kind),
        ByRef(m, e) => m.prefix().len() + head_min(&e.kind),
        Deref(e) | Neg(e) | Not { expr: e } => 1 + head_min(&e.kind),
        _ => 1,
    }
}

/// Whether `e`, the only argument between parentheses, opens on their
/// line and closes with them: `f({`, `` `Tag([ ``.
fn hugs_parens(e: &Expr) -> bool {
    !shows(&e.dec) && !trails(&e.dec) && opens_with_bracket(&e.kind)
}

/// `e` between `open` and `close`, hugging them, where it may and its
/// head fits on the line; false, with nothing written, otherwise.
fn hug_parens(
    buf: &mut PrettyBuf,
    open: &str,
    e: &Expr,
    close: &str,
) -> Result<bool, fmt::Error> {
    if !hugs_parens(e) {
        return Ok(false);
    }
    let start = buf.mark();
    write!(buf, "{open}")?;
    if !head_fits(buf.col(), buf.limit, &e.kind) {
        buf.rollback(start);
        return Ok(false);
    }
    buf.with_tail(close.chars().count(), |buf| e.fmt_pretty(buf))?;
    buf.kill_newline();
    writeln!(buf, "{close}")?;
    Ok(true)
}

/// The expression a head introduces (`let x =`, `x <-`, `name:`, `=>`);
/// the head has been written without its trailing space. It follows on
/// the head's line when it fits there or opens with a bracket, else it
/// moves to its own line, indented.
fn pretty_tail(buf: &mut PrettyBuf, e: &Expr) -> fmt::Result {
    if !shows(&e.dec) {
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
    if opens_with_bracket(&e.kind) && head_fits(buf.col(), buf.limit, &e.kind) {
        let start = buf.mark();
        let col = buf.col();
        Bare(e).fmt_pretty_inner(buf)?;
        // the column after the first line is the `;` or `,` that follows
        if col + buf.first_line_width(start) < buf.limit {
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

/// Comments as written, which only the formatter prints: canonical text
/// is a function of the syntax alone, and a comment is not syntax.
fn write_comments(f: &mut impl fmt::Write, lines: &[ArcStr]) -> fmt::Result {
    if !print_as_written() {
        return Ok(());
    }
    for c in lines {
        writeln!(f, "//{c}")?;
    }
    Ok(())
}

/// `write_leading` in a flat form, which is mid-line: decorations stand
/// on lines of their own, so they start a fresh one.
pub(crate) fn write_leading_flat(
    f: &mut impl fmt::Write,
    dec: &Option<Arc<Decorations>>,
) -> fmt::Result {
    if shows(dec) {
        writeln!(f)?;
    }
    write_leading(f, dec)
}

/// A doc in a flat form: on a fresh line unless comments above it began
/// one.
fn write_doc_flat(
    f: &mut impl fmt::Write,
    comments: &Comments,
    doc: &Doc,
) -> fmt::Result {
    if doc.0.is_some() && !shows_comments(comments.lines()) {
        writeln!(f)?;
    }
    write!(f, "{doc}")
}

/// `write_comments` in a flat form.
fn write_comments_flat(f: &mut impl fmt::Write, lines: &[ArcStr]) -> fmt::Result {
    if shows_comments(lines) {
        writeln!(f)?;
    }
    write_comments(f, lines)
}

/// Whether `lines` of comments print: only as written, since printed
/// text reaches values a program sees.
fn shows_comments(lines: &[ArcStr]) -> bool {
    print_as_written() && !lines.is_empty()
}

/// Whether `dec` prints anything above what it decorates: its attributes,
/// and its comments as written. Only what prints decides a layout.
fn shows(dec: &Option<Arc<Decorations>>) -> bool {
    dec.as_ref().is_some_and(|d| !d.attrs.is_empty() || shows_comments(&d.comments))
}

/// Whether `dec` prints comments after a list item: one ending its line,
/// or lines below the last.
fn trails(dec: &Option<Arc<Decorations>>) -> bool {
    dec.as_ref().is_some_and(|d| shows_trailing(d.eol.as_ref(), &d.below))
}

/// Whether anything in `e`, `e` included, prints on lines of its own or
/// ends one: a comment, an attribute, a doc. Such a tree has no
/// single-line form.
pub(crate) fn decorated(e: &Expr) -> bool {
    shows(&e.dec) || trails(&e.dec) || kind_decorated(&e.kind)
}

/// The comment ending the list item `e`'s line.
pub(crate) fn eol_of(e: &Expr) -> Option<&ArcStr> {
    e.dec.as_ref().and_then(|d| d.eol.as_ref())
}

/// The lines below `e`, the last item of a list.
pub(crate) fn below_of(e: &Expr) -> &[ArcStr] {
    e.dec.as_ref().map(|d| &d.below[..]).unwrap_or(&[])
}

/// Whether a list item's comments after it print: `eol`, the one ending
/// its line, and `below`, the lines below the last.
fn shows_trailing(eol: Option<&ArcStr>, below: &[ArcStr]) -> bool {
    print_as_written() && (eol.is_some() || !below.is_empty())
}

/// `sep` after a list item, then `eol`, the comment ending its line.
// XCR claude for claude: [bug/medium] A statement list ending in `;` and then an
// own-line comment (`{ a; b;\n// c\n}`, or a file `x;\n// c`) cannot be formatted. The
// parser's `commented` hangs the comment on `b` as `below`, but only `item_end` prints
// `below`, and it runs on the empty statement after the `;`. The comment is lost and
// the guard reports "formatter bug". Fix: write `prev`'s `below` lines here after the
// eol comment. Add both shapes to format.rs's `an_empty_statement_is_its_semicolon`.
// probe: design/review-2026-10-26/repro/t-format-resolver-trailing-semi-comment.sh Also
// reported as (tests-expr-01), (x-formatter-01). (t-format-resolver-01)
// 2026-10-26 claude: Fixed at the parser rather than here: a `below` now only ever
// rides a list's last element, the trailing `;`'s empty statement when there is one
// (`statements`), so `item_sep` has nothing to write and `item_end` prints the lines
// after the `;`. Both shapes are in format.rs an_empty_statement_is_its_semicolon;
// expr_pp_round_trip generates them.
fn item_sep(buf: &mut PrettyBuf, sep: &str, eol: Option<&ArcStr>) -> fmt::Result {
    buf.kill_newline();
    write!(buf, "{sep}")?;
    if let Some(c) = eol.filter(|_| print_as_written()) {
        write!(buf, " //{c}")?
    }
    writeln!(buf)
}

/// After a list's last item: the comment ending its line, then the lines
/// below it.
fn item_end(buf: &mut PrettyBuf, eol: Option<&ArcStr>, below: &[ArcStr]) -> fmt::Result {
    if !shows_trailing(eol, below) {
        return Ok(());
    }
    buf.kill_newline();
    if let Some(c) = eol {
        write!(buf, " //{c}")?
    }
    if !buf.at_line_start() {
        writeln!(buf)?
    }
    write_comments(buf, below)
}

fn kind_decorated(k: &ExprKind) -> bool {
    ensure_sufficient(|| {
        let mut any = match k {
            ExprKind::Trait(t) => t.decorated(),
            ExprKind::Module { value, .. }
                if let ModuleKind::Dynamic { sig, .. } = &**value =>
            {
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
        || shows_comments(si.comments.lines())
        || shows_trailing(si.eol.text(), &[])
        || matches!(&si.kind, SigKind::Trait(t) if t.decorated())
}

/// A literal as source text: `i64` and `f64`, the types an unprefixed
/// number reads as, print bare, an `f64` always with a point or an
/// exponent, an `f32` in its shortest form; a string escapes as an
/// expression's does.
pub(crate) struct Literal<'a>(pub &'a Value);

impl fmt::Display for Literal<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self.0 {
            Value::I64(v) => write!(f, "{v}"),
            Value::F64(v) if v.is_finite() => write!(f, "{v:?}"),
            Value::F32(v) if v.is_finite() => write!(f, "f32:{v:?}"),
            v @ Value::String(_) => v.fmt_ext(f, &parser::GRAPHIX_ESC, true),
            v => v.fmt_ext(f, &parser::VALUE_ESC, true),
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
    /// The width a postfix chain writes after the layout in progress, on
    /// its last line: what that line's fit checks keep free.
    tail: usize,
    buf: LPooled<String>,
}

impl PrettyBuf {
    pub fn new(cfg: FormatConfig) -> Self {
        Self {
            indent: 0,
            step: cfg.indent,
            limit: cfg.width,
            tail: 0,
            buf: LPooled::take(),
        }
    }

    /// Run `f` one level of nesting deeper.
    pub fn nested<R, F: FnOnce(&mut Self) -> R>(&mut self, f: F) -> R {
        self.with_indent(self.step, f)
    }

    /// Run `f` with `n` more columns following its last line.
    fn with_tail<R, F: FnOnce(&mut Self) -> R>(&mut self, n: usize, f: F) -> R {
        self.tail += n;
        let r = f(self);
        self.tail -= n;
        r
    }

    /// Run `f` over lines that end before the last one: nothing follows
    /// them.
    fn without_tail<R, F: FnOnce(&mut Self) -> R>(&mut self, f: F) -> R {
        let tail = std::mem::take(&mut self.tail);
        let r = f(self);
        self.tail = tail;
        r
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
        /// Stops the write once it is wider than the room left on the line.
        struct Room<'a> {
            buf: &'a mut PrettyBuf,
            room: usize,
            over: bool,
        }
        impl fmt::Write for Room<'_> {
            fn write_str(&mut self, s: &str) -> fmt::Result {
                let at = self.buf.mark();
                self.buf.write_str(s)?;
                let n = self.buf.width_since(at);
                if n > self.room {
                    self.over = true;
                    return Err(fmt::Error);
                }
                self.room -= n;
                Ok(())
            }
        }
        let start = buf.mark();
        // Best-effort: a multi-line string overcounts and a long token can
        // exceed any limit. The newline counts as the column kept for the
        // `;` or `,` that follows.
        let room = buf.limit.saturating_sub(buf.col() + buf.tail);
        let mut w = Room { buf, room, over: false };
        match writeln!(w, "{}", self) {
            Ok(()) => Ok(true),
            Err(_) if w.over => {
                w.buf.rollback(start);
                Ok(false)
            }
            Err(e) => Err(e),
        }
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

impl Doc {
    fn write(&self, f: &mut impl fmt::Write) -> fmt::Result {
        for line in self.0.iter().flat_map(|doc| doc.split('\n')) {
            writeln!(f, "///{line}")?;
        }
        Ok(())
    }
}

impl fmt::Display for Doc {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.write(f)
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
        self.write(buf)
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
        write_comments_flat(f, self.comments.lines())?;
        write_doc_flat(f, &self.comments, &self.doc)?;
        if !self.attrs.is_empty()
            && self.doc.0.is_none()
            && !shows_comments(self.comments.lines())
        {
            writeln!(f)?;
        }
        for a in self.attrs.iter() {
            writeln!(f, "{a}")?;
        }
        write!(f, "val {}: {}", self.name, self.typ)?;
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
        for a in self.attrs.iter() {
            writeln!(buf, "{a}")?;
        }
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
        shows_comments(self.below.lines())
            || self.methods.iter().any(|m| {
                m.doc.0.is_some()
                    || !m.attrs.is_empty()
                    || shows_comments(m.comments.lines())
                    || shows_trailing(m.eol.text(), &[])
                    || m.default.as_ref().is_some_and(decorated)
            })
    }

    fn fmt_pretty_inner(&self, buf: &mut PrettyBuf) -> fmt::Result {
        writeln!(buf, "trait {} {{", self.name)?;
        buf.nested(|buf| {
            pretty_separated(
                buf,
                &self.methods,
                ";",
                |buf, m| m.fmt_pretty_inner(buf),
                |m| m.eol.text(),
                self.below.lines(),
            )
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
        buf.nested(|buf| pretty_items(buf, &self.methods, ";"))?;
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
        write_comments_flat(f, self.comments.lines())?;
        write_doc_flat(f, &self.comments, &self.doc)?;
        match &self.kind {
            SigKind::TypeDef(td) => write!(f, "{td}"),
            SigKind::Trait(t) => write!(f, "{t}"),
            SigKind::Impl(i) => write!(f, "{i}"),
            SigKind::Bind(bind) => write!(f, "{bind}"),
            SigKind::Module(name) => write!(f, "mod {name}"),
            SigKind::Use { reexport, names } => {
                write!(f, "{}", UseStmt(*reexport, names))
            }
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
        shows_comments(self.below.lines()) || self.items.iter().any(sig_item_decorated)
    }

    fn fmt_pretty_inner(&self, buf: &mut PrettyBuf) -> fmt::Result {
        if !self.toplevel {
            writeln!(buf, "sig {{")?;
        }
        let item = |buf: &mut PrettyBuf, si: &SigItem| si.fmt_pretty_inner(buf);
        fn eol(si: &SigItem) -> Option<&ArcStr> {
            si.eol.text()
        }
        match self.toplevel {
            true => pretty_file_items(buf, &self.items, item, eol, self.below.lines())?,
            false => buf.nested(|buf| {
                pretty_separated(buf, &self.items, ";", item, eol, self.below.lines())
            })?,
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
        write!(buf, "let{rec} ")?;
        pretty_pattern_then(buf, pattern, if typ.is_some() { ":" } else { " =" })?;
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

/// `p`, flat when it and the `follows` text it is the head of fit the
/// line, else laid out over lines; the line it ends on is left open.
fn pretty_pattern_then(
    buf: &mut PrettyBuf,
    p: &StructurePattern,
    follows: &str,
) -> fmt::Result {
    let start = buf.mark();
    let col = buf.col();
    write!(buf, "{p}")?;
    if col + buf.width_since(start) + follows.len() > buf.limit {
        buf.rollback(start);
        p.fmt_pretty_inner(buf)?;
        buf.kill_newline();
    }
    Ok(())
}

/// One element of a bracketed pattern.
enum PatElt<'a> {
    Pat(&'a StructurePattern),
    Field(&'a str, &'a StructurePattern),
    Text(CompactString),
}

/// A bracketed pattern, one element to a line.
// CR claude for eric: [readability/low] A pattern's lone bracketed element never hugs.
// `` `Beta({ first, second: s }) `` prints as `` `Beta( ``, `{`, the fields, `}`, `)`,
// while the same shape as a type or a value hugs (`` `Beta({ ``), per the Formatter
// rule for expressions and types. Decide whether patterns follow the hug rule. If they
// do, give a single bracketed PatElt::Pat the hug_parens treatment here. If they do
// not, the Formatter rules in CLAUDE.md should say so. probe:
// design/review-2026-10-26/repro/t-print-pattern-no-hug.gx (t-print-03)
fn pretty_pattern_elts(
    buf: &mut PrettyBuf,
    open: &str,
    elts: &[PatElt<'_>],
    close: &str,
) -> fmt::Result {
    writeln!(buf, "{open}")?;
    buf.nested(|buf| {
        for (i, elt) in elts.iter().enumerate() {
            if i > 0 {
                buf.kill_newline();
                writeln!(buf, ",")?
            }
            match elt {
                PatElt::Pat(p) => p.fmt_pretty(buf)?,
                PatElt::Field(name, p) => {
                    write!(buf, "{name}: ")?;
                    p.fmt_pretty(buf)?
                }
                PatElt::Text(t) => writeln!(buf, "{t}")?,
            }
        }
        Ok(())
    })?;
    writeln!(buf, "{close}")
}

impl PrettyDisplay for StructurePattern {
    fn fmt_pretty_inner(&self, buf: &mut PrettyBuf) -> fmt::Result {
        use StructurePattern as P;
        if let Some(all) = self.all() {
            write!(buf, "{all}@ ")?
        }
        fn pats(ps: &[StructurePattern]) -> SmallVec<[PatElt<'_>; 8]> {
            ps.iter().map(PatElt::Pat).collect()
        }
        let brackets = |list: bool| if list { ("[<", ">]") } else { ("[", "]") };
        match self {
            P::Ignore | P::Literal(..) | P::Bind(_) => writeln!(buf, "{self}"),
            P::Slice { list, all: _, binds } => {
                let (open, close) = brackets(*list);
                pretty_pattern_elts(buf, open, &pats(binds), close)
            }
            P::SlicePrefix { list, all: _, prefix, tail } => {
                let (open, close) = brackets(*list);
                let mut elts = pats(prefix);
                elts.push(PatElt::Text(match tail {
                    None => format_compact!(".."),
                    Some(name) => format_compact!("{name}.."),
                }));
                pretty_pattern_elts(buf, open, &elts, close)
            }
            P::SliceSuffix { all: _, head, suffix } => {
                let mut elts: SmallVec<[PatElt<'_>; 8]> =
                    SmallVec::from_iter([PatElt::Text(match head {
                        None => format_compact!(".."),
                        Some(name) => format_compact!("{name}.."),
                    })]);
                elts.extend(suffix.iter().map(PatElt::Pat));
                pretty_pattern_elts(buf, "[", &elts, "]")
            }
            P::Tuple { all: _, binds } => {
                pretty_pattern_elts(buf, "(", &pats(binds), ")")
            }
            P::Variant { all: _, tag, binds } if binds.is_empty() => {
                writeln!(buf, "`{tag}")
            }
            P::Variant { all: _, tag, binds } => {
                pretty_pattern_elts(buf, &format_compact!("`{tag}("), &pats(binds), ")")
            }
            P::Abstract { all: _, name, bind } => {
                write!(buf, "{name}(")?;
                bind.fmt_pretty(buf)?;
                buf.kill_newline();
                writeln!(buf, ")")
            }
            P::Or(alts) => {
                for (i, p) in alts.iter().enumerate() {
                    if i > 0 {
                        write!(buf, "| ")?
                    }
                    p.fmt_pretty(buf)?
                }
                Ok(())
            }
            P::Struct { exhaustive, all: _, binds } => {
                let mut elts: SmallVec<[PatElt<'_>; 8]> = printed_fields(binds)
                    .into_iter()
                    .map(|(name, pat)| match pat {
                        None => PatElt::Text(name.as_str().into()),
                        Some(pat) => PatElt::Field(name.as_str(), pat),
                    })
                    .collect();
                if !exhaustive {
                    elts.push(PatElt::Text(format_compact!("..")));
                }
                pretty_pattern_elts(buf, "{", &elts, "}")
            }
        }
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
        write_leading_flat(f, &e.dec)?;
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
        let fields = as_written(fields);
        for (i, (name, e)) in fields.iter().enumerate() {
            if i > 0 {
                item_sep(buf, ",", eol_of(&fields[i - 1].1))?
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
        match fields.last() {
            Some((_, last)) => item_end(buf, eol_of(last), below_of(last)),
            None => Ok(()),
        }
    })
}

impl StructWithExpr {
    fn write_head(&self, f: &mut impl Write) -> fmt::Result {
        // CR claude for claude: [structure/low] This copies with_source's
        // `chain_on_name` in parser/mod.rs: what reads bare as a functional-update
        // source. The two already differ in where they check decorations, and a change
        // to the parser's rule must be repeated here by hand. Fix: make the parser's
        // `chain_on_name` a pub(crate) free function, call it here, and keep only the
        // `shows(&e.dec)` test local. Found by reading; not run. (t-print-04)
        fn chain_on_name(e: &Expr) -> bool {
            match &e.kind {
                ExprKind::Ref { .. } => !shows(&e.dec),
                ExprKind::Qop { arg: s, .. } | ExprKind::OrNever(s) => {
                    !shows(&e.dec) && chain_on_name(s)
                }
                _ => false,
            }
        }
        match chain_on_name(&self.source) {
            true => write!(f, "{{ {} with", self.source),
            false => write!(f, "{{ ({}) with", self.source),
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
        match decorated(&self.source) {
            false => self.write_head(buf)?,
            true => {
                write!(buf, "{{ ")?;
                pretty_parens(buf, &self.source)?;
                buf.kill_newline();
                write!(buf, " with")?
            }
        }
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
/// a name, a postfix chain (`?`/`$` included) or a bracketed primary, which
/// the parser reads as a source bare, unless a seq head parenthesizes it. Anything else must be parenthesized
/// (`(a+b).c`; `(42).0` would lex as a float; the editor grammar reads no
/// select, block or cast as a source), and so is a decorated source, whose
/// decorations would otherwise stand above the whole chain.
// CR claude for claude: [readability/low] Any postfix source not listed here gets
// parens. So `graphix fmt` rewrites `any(a, b).f`, `cast<T>(x).f`, `T(1).0`,
// `never<T>().f` and `select x { .. }.f` as `(any(a, b)).f` and so on. The parser reads
// each back bare to the same tree, because it drops parens around a postfix source.
// Fix: add the forms that close with their own bracket (`Any`, `Never`, `TypeCast`,
// `Construct`, `Live`, `Select`, and `Variant` with arguments), checking each against
// `trigger_parens` for the seq-head context. probe:
// design/review-2026-10-26/repro/tests-expr-fmt-postfix-parens.gx (tests-expr-08)
pub(super) fn prints_as_bare_postfix(e: &Expr) -> bool {
    use ExprKind::*;
    !shows(&e.dec)
        && !std::ptr::eq(e, PAREN_SOURCE.get())
        && matches!(
            &e.kind,
            Ref { .. }
                | StructRef { .. }
                | TupleRef { .. }
                | ArrayRef { .. }
                | ArraySlice { .. }
                | MapRef { .. }
                | Apply(_)
                | Qop { .. }
                | OrNever(_)
                | Tuple { .. }
                | Array { .. }
                | List { .. }
                | Map { .. }
                | Struct(_)
                | StructWith(_)
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
            pretty_parens(buf, function)?;
            buf.kill_newline();
        }
        buf.kill_newline();
        if let [(None, arg)] = &args[..]
            && hug_parens(buf, "(", arg, ")")?
        {
            return Ok(());
        }
        if args.is_empty() {
            return writeln!(buf, "()");
        }
        writeln!(buf, "(")?;
        buf.nested(|buf| {
            for (i, (name, e)) in args.iter().enumerate() {
                if i > 0 {
                    item_sep(buf, ",", eol_of(&args[i - 1].1))?
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
            match args.last() {
                Some((_, last)) => item_end(buf, eol_of(last), below_of(last)),
                None => Ok(()),
            }
        })?;
        writeln!(buf, ")")
    }
}

impl Arg {
    /// `[#]pattern[: T]`, what stands before a default.
    fn write_head(&self, f: &mut impl Write) -> fmt::Result {
        if self.kind.is_labeled() {
            write!(f, "#")?;
        }
        write!(f, "{}", self.pattern)?;
        if let Some(t) = &self.constraint {
            write!(f, ": {t}")?
        }
        Ok(())
    }

    /// `write_head` with the type laid out over lines when it does not fit.
    fn pretty_head(&self, buf: &mut PrettyBuf) -> fmt::Result {
        if self.kind.is_labeled() {
            write!(buf, "#")?;
        }
        write!(buf, "{}", self.pattern)?;
        if let Some(t) = &self.constraint {
            write!(buf, ": ")?;
            pretty_type(buf, t)?
        }
        Ok(())
    }
}

impl fmt::Display for Arg {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.write_head(f)?;
        match self.kind.default() {
            Some(def) => write!(f, " = {def}"),
            None => Ok(()),
        }
    }
}

impl LambdaExpr {
    /// The quantifiers in front of the opening bar, and the space
    /// between them and it.
    fn write_constraints(&self, f: &mut impl Write) -> fmt::Result {
        if !self.constraints.is_empty() {
            write_bounds(f, &self.constraints)?;
            write!(f, " ")?;
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

    /// The arguments one to a line, each default placed after its `=` as
    /// a value after its head.
    fn pretty_args(&self, buf: &mut PrettyBuf) -> fmt::Result {
        let n = self.args.len() + self.vargs.is_some() as usize;
        for (i, a) in self.args.iter().enumerate() {
            a.pretty_head(buf)?;
            if let Some(def) = a.kind.default() {
                write!(buf, " =")?;
                pretty_tail(buf, def)?;
                buf.kill_newline();
            }
            match i + 1 < n {
                true => writeln!(buf, ",")?,
                false => writeln!(buf)?,
            }
        }
        match &self.vargs {
            None => Ok(()),
            Some(None) => writeln!(buf, "@args"),
            Some(Some(typ)) => {
                write!(buf, "@args: ")?;
                pretty_type(buf, typ)?;
                writeln!(buf)
            }
        }
    }

    /// What follows the closing bar, up to the body.
    fn write_returns(&self, f: &mut impl Write) -> fmt::Result {
        if let Some(t) = &self.rtype {
            write!(f, " -> {}", Ret::of(t, self.throws.is_some()))?
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
            LambdaBody::Builtin(builtin) => write!(f, " '{builtin}"),
            LambdaBody::Expr(body) => write!(f, " {body}"),
        }
    }
}

impl LambdaExpr {
    /// `write_returns` with the types laid out over lines where they do
    /// not fit.
    fn pretty_returns(&self, buf: &mut PrettyBuf) -> fmt::Result {
        if let Some(rtype) = &self.rtype {
            let Ret { open, typ, close } = Ret::of(rtype, self.throws.is_some());
            write!(buf, " -> {open}")?;
            typ.fmt_pretty(buf)?;
            buf.kill_newline();
            write!(buf, "{close}")?;
        }
        match &self.throws {
            None => Ok(()),
            Some(t) => {
                write!(buf, " throws ")?;
                pretty_type(buf, t)
            }
        }
    }
}

/// The head gives way a step at a time: all of it on one line with what
/// opens the body; else its arguments one to a line, closed by `| -> R`;
/// else the body under that closing line; else the return and thrown
/// types laid out over lines. Where that closing line would not fit
/// either, listing the arguments shortens nothing: they stay on the head's
/// line and the return and thrown types are laid out there.
impl PrettyDisplay for LambdaExpr {
    fn fmt_pretty_inner(&self, buf: &mut PrettyBuf) -> fmt::Result {
        let start = buf.mark();
        self.write_constraints(buf)?;
        write!(buf, "|")?;
        self.write_args(buf, ", ", "")?;
        write!(buf, "|")?;
        self.write_returns(buf)?;
        let opener = match &self.body {
            LambdaBody::Builtin(builtin) => builtin.chars().count() + 3,
            LambdaBody::Expr(_) => 2,
        };
        let has_args = !self.args.is_empty() || self.vargs.is_some();
        let has_returns = self.rtype.is_some() || self.throws.is_some();
        let decorated_default =
            self.args.iter().any(|a| a.kind.default().is_some_and(decorated));
        let overflows = buf.col() + opener > buf.limit;
        let mut laid_out = false;
        if overflows && has_returns && !decorated_default {
            buf.rollback(start);
            let closing = buf.mark();
            write!(buf, "|")?;
            self.write_returns(buf)?;
            let closing_fits = buf.indent + buf.width_since(closing) <= buf.limit;
            buf.rollback(closing);
            let col = buf.col();
            self.write_constraints(buf)?;
            write!(buf, "|")?;
            self.write_args(buf, ", ", "")?;
            write!(buf, "|")?;
            self.pretty_returns(buf)?;
            laid_out = !(has_args && closing_fits)
                && fits_since(buf, start, col)
                && buf.col() + opener <= buf.limit;
        }
        if !laid_out && (overflows || decorated_default) && has_args {
            buf.rollback(start);
            self.write_constraints(buf)?;
            writeln!(buf, "|")?;
            buf.nested(|buf| self.pretty_args(buf))?;
            let closing = buf.mark();
            write!(buf, "|")?;
            self.write_returns(buf)?;
            if buf.col() > buf.limit && has_returns {
                buf.rollback(closing);
                write!(buf, "|")?;
                self.pretty_returns(buf)?;
            }
        }
        match &self.body {
            LambdaBody::Builtin(builtin) if buf.col() + opener > buf.limit => {
                writeln!(buf)?;
                buf.nested(|buf| writeln!(buf, "'{builtin}"))
            }
            LambdaBody::Builtin(builtin) => writeln!(buf, " '{builtin}"),
            LambdaBody::Expr(body) => pretty_tail(buf, body),
        }
    }
}

impl fmt::Display for SelectExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let SelectExpr { arg, arms } = self;
        match ends_bodyless(arg) {
            true => write!(f, "select {arg} {{}} {{ ")?,
            false => write!(f, "select {arg} {{ ")?,
        }
        for (i, (pat, rhs)) in arms.iter().enumerate() {
            write_leading_flat(f, &rhs.dec)?;
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
        pretty_scrutinee(buf, arg)?;
        writeln!(buf, " {{")?;
        buf.nested(|buf| {
            for (i, (pat, expr)) in arms.iter().enumerate() {
                write_leading(buf, &expr.dec)?;
                if let Some(tp) = &pat.type_predicate {
                    pretty_type(buf, tp)?;
                    write!(buf, " as ")?;
                }
                // the pattern stays flat only if its guard fits beside it too
                let follows = match &pat.guard {
                    Some(g) if !shows(&g.dec) => format_compact!(" if {g} =>"),
                    _ => CompactString::const_new(" =>"),
                };
                pretty_pattern_then(buf, &pat.structure_predicate, &follows)?;
                write!(buf, " ")?;
                if let Some(guard) = &pat.guard {
                    pretty_after(buf, "if", guard)?;
                    buf.kill_newline();
                    write!(buf, " ")?;
                }
                write!(buf, "=>")?;
                pretty_tail_bare(buf, expr)?;
                match i < arms.len() - 1 {
                    true => item_sep(buf, ",", eol_of(expr))?,
                    false => item_end(buf, eol_of(expr), below_of(expr))?,
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
        if col + buf.width_since(start) + buf.tail <= buf.limit {
            return writeln!(buf);
        }
        buf.rollback(start);
        let width = suffix.chars().count();
        buf.with_tail(width, |buf| Bare(e).fmt_pretty_inner(buf))?
    }
    buf.kill_newline();
    writeln!(buf, "{suffix}")
}

/// Whether `e` ends with an impl that has no body, which reads a `{`
/// printed after it as its body.
fn ends_bodyless(e: &Expr) -> bool {
    use ExprKind::*;
    match &e.kind {
        Impl(i) => i.methods.is_empty(),
        Bind(b) => ends_bodyless(&b.value),
        Connect { value, .. } => ends_bodyless(value),
        Lambda(l) => matches!(&l.body, LambdaBody::Expr(b) if ends_bodyless(b)),
        Not { expr } | Neg(expr) | Deref(expr) | ByRef(_, expr) => ends_bodyless(expr),
        ExprKind::BinOp { rhs, .. } => ends_bodyless(rhs),
        _ => false,
    }
}

/// `select` and its scrutinee, continuation lines nested under it; a
/// scrutinee ending in a bodyless impl gets the impl's empty body, which
/// the arms' `{` would otherwise be read as.
// CR claude for claude: [bug/low] The scrutinee is laid out with no tail, but ` {`
// follows it on its line. fmt_flat keeps one column for the newline and ` {` takes two.
// So `select ffffffffffffffffffffffffff(1, 2) {` prints 41 columns at --width 40.
// pretty_seq_head's trigger closure has the same gap, and head_fits already counts the
// ` {` for a Select. Fix: run pretty_after under `buf.with_tail(" {".len(), ..)`, and
// do the same around the trigger's `t.fmt_pretty` in pretty_seq_head. probe:
// design/review-2026-10-26/repro/t-print-select-head-overflow.sh (t-print-01)
fn pretty_scrutinee(buf: &mut PrettyBuf, arg: &Expr) -> fmt::Result {
    pretty_after(buf, "select", arg)?;
    buf.kill_newline();
    if ends_bodyless(arg) {
        write!(buf, " {{}}")?
    }
    Ok(())
}

/// `head e`, `e` continuing the line and nested under `head`; a
/// decorated `e` starts on a line of its own, its comments above it.
fn pretty_after(buf: &mut PrettyBuf, head: &str, e: &Expr) -> fmt::Result {
    match shows(&e.dec) {
        false => write!(buf, "{head} ")?,
        true => writeln!(buf, "{head}")?,
    }
    buf.nested(|buf| e.fmt_pretty(buf))
}

/// `(e)`; a decorated `e` stands on lines of its own inside.
fn pretty_parens(buf: &mut PrettyBuf, e: &Expr) -> fmt::Result {
    if decorated(e) {
        writeln!(buf, "(")?;
        buf.nested(|buf| e.fmt_pretty(buf))?;
        buf.kill_newline();
        writeln!(buf)?;
        writeln!(buf, ")")
    } else {
        write!(buf, "(")?;
        e.fmt_pretty(buf)?;
        buf.kill_newline();
        writeln!(buf, ")")
    }
}

/// `source[i]` or `source{key}` that does not fit flat: the source laid
/// out, then the index flat on its last line where it fits there, else
/// laid out like a lone argument between the brackets.
fn pretty_lookup(
    buf: &mut PrettyBuf,
    source: &Expr,
    open: &str,
    i: &Expr,
    close: &str,
) -> fmt::Result {
    pretty_postfix(buf, source, "")?;
    buf.kill_newline();
    if !decorated(i) {
        let start = buf.mark();
        write!(buf, "{open}{i}{close}")?;
        if buf.col() + buf.tail < buf.limit {
            return writeln!(buf);
        }
        buf.rollback(start)
    }
    pretty_print_exprs(buf, std::slice::from_ref(i), open, close, ",", true)
}

/// A postfix form: its source, parenthesized unless it reads bare, and
/// then the suffix.
fn pretty_postfix(buf: &mut PrettyBuf, source: &Expr, suffix: &str) -> fmt::Result {
    if prints_as_bare_postfix(source) {
        pretty_then(buf, source, suffix)
    } else if decorated(source) {
        pretty_parens(buf, source)?;
        buf.kill_newline();
        writeln!(buf, "{suffix}")
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
        match self {
            ExprKind::BinOp { op, lhs, rhs } => {
                pretty_operand(
                    buf,
                    lhs,
                    under(lhs, *op, false),
                    &format_compact!(" {}", op.token()),
                )?;
                match under(rhs, *op, true) {
                    false => rhs.fmt_pretty(buf),
                    true => pretty_parens(buf, rhs),
                }
            }
            NoOp => Ok(()),
            Use { reexport, names } => UseStmt(*reexport, names).fmt_pretty_inner(buf),
            Constant(_) | Ref { .. } | StringInterpolate { .. } => {
                writeln!(buf, "{self}")
            }
            StructRef { source, field } => {
                pretty_postfix(buf, source, &format_compact!(".{field}"))
            }
            TupleRef { source, field } => {
                pretty_postfix(buf, source, &format_compact!(".{field}"))
            }
            ArrayRef { source, i } => pretty_lookup(buf, source, "[", i, "]"),
            MapRef { source, key } => pretty_lookup(buf, source, "{", key, "}"),
            ArraySlice { source, start, end } => {
                pretty_postfix(buf, source, "")?;
                buf.kill_newline();
                let bounds = [start, end];
                if bounds.iter().all(|b| b.as_ref().is_none_or(|b| !decorated(b))) {
                    let start = buf.mark();
                    write!(buf, "{}", Slice(&bounds[0], &bounds[1]))?;
                    if buf.col() < buf.limit {
                        return writeln!(buf);
                    }
                    buf.rollback(start)
                }
                writeln!(buf, "[")?;
                buf.nested(|buf| {
                    for (i, b) in bounds.iter().enumerate() {
                        if i > 0 {
                            write!(buf, "..")?
                        }
                        if let Some(b) = b {
                            b.fmt_pretty(buf)?;
                            buf.kill_newline()
                        }
                    }
                    writeln!(buf)
                })?;
                writeln!(buf, "]")
            }
            ExplicitParens(e) => {
                writeln!(buf, "(")?;
                buf.nested(|buf| e.fmt_pretty(buf))?;
                writeln!(buf, ")")
            }
            Block { exprs } => pretty_print_exprs(buf, exprs, "{", "}", ";", false),
            Seq(s) => {
                pretty_seq_head(buf, s)?;
                pretty_print_exprs(buf, &s.body, "{", "}", ";", false)
            }
            Until(e) => pretty_after(buf, "until", e),
            TryWith(t) => {
                pretty_print_exprs(buf, &t.body, "try {", "}", ";", false)?;
                buf.kill_newline();
                match &t.constraint {
                    None => write!(buf, " with({}) ", t.bind)?,
                    Some(ty) => {
                        write!(buf, " with({}: ", t.bind)?;
                        pretty_type(buf, ty)?;
                        write!(buf, ") ")?
                    }
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
            Module { name, value } => {
                let ModuleKind::Dynamic { sandbox, sig, source } = &**value else {
                    return writeln!(buf, "{self}");
                };
                writeln!(buf, "mod {name} dynamic {{")?;
                buf.nested(|buf| {
                    sandbox.fmt_pretty(buf)?;
                    buf.kill_newline();
                    writeln!(buf, ";")?;
                    sig.fmt_pretty(buf)?;
                    buf.kill_newline();
                    writeln!(buf, ";")?;
                    pretty_after(buf, "source", source)?;
                    buf.kill_newline();
                    writeln!(buf, ";")
                })?;
                writeln!(buf, "}}")
            }
            Connect { name, value, deref, .. } => {
                let deref = if *deref { "*" } else { "" };
                write!(buf, "{deref}{name} <-")?;
                pretty_tail(buf, value)
            }
            TypeCast { expr, typ } => {
                write!(buf, "cast<")?;
                pretty_type(buf, typ)?;
                write!(buf, ">")?;
                pretty_print_exprs(
                    buf,
                    std::slice::from_ref(&**expr),
                    "(",
                    ")",
                    ",",
                    true,
                )
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
                        Bare(k).fmt_pretty(buf)?;
                        buf.kill_newline();
                        write!(buf, " =>")?;
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
            Live(e) => {
                write!(buf, "live")?;
                pretty_print_exprs(buf, std::slice::from_ref(&**e), "(", ")", ",", true)
            }
            Never { typ, args } => {
                write!(buf, "never")?;
                if let Some(t) = typ {
                    write!(buf, "<")?;
                    pretty_type(buf, t)?;
                    write!(buf, ">")?
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
            Qop { arg: e, .. } => pretty_operand(buf, e, suffixed(e), "?"),
            Rethrow(e) => {
                write!(buf, "rethrow(")?;
                e.fmt_pretty(buf)?;
                buf.kill_newline();
                writeln!(buf, ")")
            }
            SeqGuard(e) | SeqAbort(e) => e.fmt_pretty(buf),
            SeqCapture(c) => {
                write!(buf, "capture(")?;
                c.snapshot.fmt_pretty(buf)?;
                buf.kill_newline();
                write!(buf, " or live {})", c.live)
            }
            SeqMachine(m) => {
                writeln!(buf, "machine {} {{", m.pc)?;
                buf.nested(|buf| {
                    for s in m.steps.iter() {
                        write!(
                            buf,
                            "`{}{} => ",
                            s.label,
                            if s.until { " until" } else { "" }
                        )?;
                        pretty_print_exprs(buf, &s.items, "{", "}", ";", false)?;
                        buf.kill_newline();
                        match s.next {
                            Some(n) => {
                                writeln!(buf, " -> `{},", m.steps[n as usize].label)?
                            }
                            None => writeln!(buf, " -> end,")?,
                        }
                    }
                    Ok(())
                })?;
                writeln!(buf, "}}")
            }
            OrNever(e) => pretty_operand(buf, e, suffixed(e), "$"),
            Catch(c) => {
                match &c.constraint {
                    None => write!(buf, "catch({})", c.bind)?,
                    Some(t) => {
                        write!(buf, "catch({}: ", c.bind)?;
                        pretty_type(buf, t)?;
                        write!(buf, ")")?
                    }
                }
                pretty_tail(buf, &c.handler)?;
                if !matches!(c.role, CatchRole::User) {
                    buf.kill_newline();
                    writeln!(buf, "{}", Role(&c.role))?
                }
                Ok(())
            }
            Apply(ae) => ae.fmt_pretty_inner(buf),
            Lambda(l) => l.fmt_pretty_inner(buf),
            Not { expr } => match &expr.kind {
                Block { exprs } => pretty_print_exprs(buf, exprs, "!{", "}", ";", false),
                _ => pretty_prefixed(buf, "!", expr, false),
            },
            ByRef(m, e) => pretty_prefixed(buf, m.prefix(), e, true),
            Deref(e) => pretty_prefixed(buf, "*", e, false),
            Neg(e) if matches!(e.kind, Constant(_)) => writeln!(buf, "{self}"),
            Neg(e) => pretty_prefixed(buf, "-", e, false),
            Select(se) => se.fmt_pretty_inner(buf),
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

/// Whether the canonical form may write `s` raw: as itself, with no
/// control character but a newline or a tab (a `\r` would not survive an
/// editor's line endings).
fn raw_writable(s: &str) -> bool {
    !s.chars().any(|c| c.is_control() && c != '\n' && c != '\t')
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
    /// The parts of an interpolation. As written, the text is what the
    /// parser marked so. Canonically, a string constant after text, or
    /// alone, prints as a splice (`"a["b"]"`, `"["a"]"`) and any other as
    /// text, which reads back as the same tree.
    fn of_all(args: &'a [Expr]) -> impl Iterator<Item = Self> + Clone {
        let lone = args.len() == 1;
        let as_written = print_as_written();
        args.iter().scan(false, move |after_text, e| {
            let part = match &e.kind {
                ExprKind::Constant(Value::String(s)) if as_written => match e.str_form {
                    StrForm::Text => Self::Text(s),
                    _ => Self::Splice(e),
                },
                ExprKind::Constant(Value::String(s))
                    if !s.is_empty() && !lone && !*after_text =>
                {
                    Self::Text(s)
                }
                _ => Self::Splice(e),
            };
            *after_text = part.text().is_some();
            Some(part)
        })
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

/// A string literal, written in one piece: a layout indents only at the
/// start of a write, never inside a multi-line literal.
pub(crate) fn write_str_constant(
    f: &mut Formatter<'_>,
    v: &Value,
    s: &str,
    form: StrForm,
) -> fmt::Result {
    in_one_piece(f, |f| str_constant_pieces(f, v, s, form))
}

/// `write_str_constant` for an interpolated string.
fn write_interpolation(
    f: &mut Formatter<'_>,
    args: &[Expr],
    form: StrForm,
) -> fmt::Result {
    in_one_piece(f, |f| interpolation_pieces(f, args, form))
}

/// What `w` writes, handed to `f` in one write.
fn in_one_piece(
    f: &mut Formatter<'_>,
    w: impl Fn(&mut Formatter<'_>) -> fmt::Result,
) -> fmt::Result {
    let mut s: LPooled<String> = LPooled::take();
    write!(s, "{}", writes(w))?;
    f.write_str(&s)
}

/// A string constant between the delimiters its author chose when
/// printing as written, else raw exactly when it spans lines; quoted
/// wherever the delimiters cannot hold it.
fn str_constant_pieces(
    f: &mut Formatter<'_>,
    v: &Value,
    s: &str,
    form: StrForm,
) -> fmt::Result {
    // the parser reads any character back verbatim in either form, but
    // a line ending, which reads as LF
    let form = match form {
        StrForm::Raw if s.contains("\r\n") => StrForm::Quoted,
        form if print_as_written() => form,
        _ if s.contains('\n') && raw_writable(s) => StrForm::Raw,
        _ => StrForm::Quoted,
    };
    match form {
        StrForm::Raw => write_raw(f, s),
        StrForm::Template => write_template(f, std::iter::once(StrPart::Text(s))),
        StrForm::Quoted | StrForm::Text => v.fmt_ext(f, &parser::GRAPHIX_ESC, true),
    }
}

/// `write_str_constant` for an interpolated string.
fn interpolation_pieces(
    f: &mut Formatter<'_>,
    args: &[Expr],
    form: StrForm,
) -> fmt::Result {
    let parts = StrPart::of_all(args);
    let texts = || parts.clone().filter_map(|p| p.text());
    let template = match form {
        form if print_as_written() => form == StrForm::Template,
        _ => texts().any(|s| s.contains('\n')),
    };
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
        match self {
            ExprKind::BinOp { op, lhs, rhs } => {
                let (l, r) = (
                    Operand(lhs, under(lhs, *op, false)),
                    Operand(rhs, under(rhs, *op, true)),
                );
                write!(f, "{l} {} {r}", op.token())
            }
            ExprKind::Constant(v @ Value::String(s)) => {
                write_str_constant(f, v, s, StrForm::Quoted)
            }
            ExprKind::NoOp => Ok(()),
            ExprKind::ExplicitParens(e) => write!(f, "({e})"),
            ExprKind::Constant(v) => write!(f, "{}", Literal(v)),
            ExprKind::Bind(b) => write!(f, "{b}"),
            ExprKind::StructWith(sw) => write!(f, "{sw}"),
            ExprKind::Connect { name, value, deref, .. } => {
                let deref = if *deref { "*" } else { "" };
                write!(f, "{deref}{name} <- {value}")
            }
            ExprKind::Use { reexport, names } => {
                write!(f, "{}", UseStmt(*reexport, names))
            }
            ExprKind::Ref { name } => {
                write!(f, "{name}")
            }
            ExprKind::StructRef { source, field } => {
                write!(f, "{}.{field}", Postfix(source))
            }
            ExprKind::TupleRef { source, field } => {
                write!(f, "{}.{field}", Postfix(source))
            }
            ExprKind::Module { value, .. }
                if matches!(
                    &**value,
                    ModuleKind::Resolved { from_interface: true, .. }
                        | ModuleKind::Unresolved { from_interface: true }
                ) =>
            {
                Ok(())
            }
            ExprKind::Module { name, value } => {
                write!(f, "mod {name}")?;
                match &**value {
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
            ExprKind::Block { exprs } => print_exprs(f, &**exprs, "{ ", " }", "; "),
            ExprKind::Seq(s) => {
                write_flat_seq_head(f, s)?;
                print_exprs(f, &s.body, "{ ", " }", "; ")
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
            ExprKind::Live(e) => write!(f, "live({e})"),
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
            ExprKind::Qop { arg: e, .. } => write!(f, "{}?", Operand(e, suffixed(e))),
            ExprKind::Rethrow(e) => write!(f, "rethrow({e})"),
            ExprKind::SeqGuard(e) | ExprKind::SeqAbort(e) => write!(f, "{e}"),
            ExprKind::SeqCapture(c) => {
                write!(f, "capture({} or live {})", c.snapshot, c.live)
            }
            ExprKind::SeqMachine(m) => {
                write!(f, "machine {} {{ ", m.pc)?;
                for s in m.steps.iter() {
                    write!(f, "`{}{} => ", s.label, if s.until { " until" } else { "" })?;
                    print_exprs(f, &s.items, "{ ", " }", "; ")?;
                    match s.next {
                        Some(n) => write!(f, " -> `{}, ", m.steps[n as usize].label)?,
                        None => write!(f, " -> end ")?,
                    }
                }
                write!(f, "}}")
            }
            ExprKind::OrNever(e) => write!(f, "{}$", Operand(e, suffixed(e))),
            ExprKind::Catch(c) => {
                match &c.constraint {
                    None => write!(f, "catch({}) {}", c.bind, c.handler)?,
                    Some(t) => write!(f, "catch({}: {t}) {}", c.bind, c.handler)?,
                }
                write!(f, "{}", Role(&c.role))
            }
            ExprKind::StringInterpolate { args } => {
                write_interpolation(f, args, StrForm::Quoted)
            }
            ExprKind::ArrayRef { source, i } => write!(f, "{}[{i}]", Postfix(source)),
            ExprKind::ArraySlice { source, start, end } => {
                write!(f, "{}{}", Postfix(source), Slice(start, end))
            }
            ExprKind::Apply(ap) => write!(f, "{ap}"),
            ExprKind::Select(se) => write!(f, "{se}"),
            ExprKind::ByRef(m, e) => {
                write!(f, "{}{}", m.prefix(), Operand(e, prefixed(e, true)))
            }
            ExprKind::Deref(e) => write!(f, "*{}", Operand(e, prefixed(e, false))),
            // `-1` reads back as the literal: a negated one keeps the space
            ExprKind::Neg(e) => match &e.kind {
                ExprKind::Constant(_) if !shows(&e.dec) => write!(f, "- {}", Bare(e)),
                _ => write!(f, "-{}", Operand(e, prefixed(e, false))),
            },
            ExprKind::Not { expr } => {
                write!(f, "!{}", Operand(expr, prefixed(expr, false)))
            }
        }
    }
}

/// Whether `e`, an operand of `op` on the `right` or the left, needs
/// parens: of lower precedence, or of equal on the right, since every
/// operator is left-associative, or ending in a lambda on the left, whose
/// body would take the rest. A parsed tree holds its parens; a tree the
/// compiler built does not.
fn under(e: &Expr, op: BinOp, right: bool) -> bool {
    match &e.kind {
        ExprKind::BinOp { op: c, .. }
            if c.precedence() < op.precedence()
                || (right && c.precedence() == op.precedence()) =>
        {
            true
        }
        _ => !right && ends_in_lambda(e),
    }
}

/// Whether `e` prints ending in a bare lambda: a lambda, or an operator
/// whose last operand prints bare and ends in one.
fn ends_in_lambda(e: &Expr) -> bool {
    ensure_sufficient(|| match &e.kind {
        ExprKind::Lambda(_) => true,
        ExprKind::BinOp { op, rhs, .. } => !under(rhs, *op, true) && ends_in_lambda(rhs),
        ExprKind::ByRef(_, x) => !prefixed(x, true) && ends_in_lambda(x),
        _ => false,
    })
}

/// Whether `e`, the operand of `?` or `$`, needs parens: it is an
/// operator's, which binds looser than a postfix form, or a lambda's,
/// whose body would take the suffix.
fn suffixed(e: &Expr) -> bool {
    use ExprKind::*;
    matches!(
        &e.kind,
        BinOp { .. } | Deref(_) | Not { .. } | Neg(_) | ByRef(..) | Lambda(_)
    )
}

/// Whether `e`, the operand of a prefix operator (`!`, `-`, `*`, `&`),
/// needs parens: an operator's binds looser, and a lambda stands bare
/// only where the operator `takes_lambda` (`&`).
fn prefixed(e: &Expr, takes_lambda: bool) -> bool {
    match e.kind {
        ExprKind::BinOp { .. } => true,
        ExprKind::Lambda(_) => !takes_lambda,
        _ => false,
    }
}

/// `op e`, laid out, `e` in parens when it needs them.
fn pretty_prefixed(
    buf: &mut PrettyBuf,
    op: &str,
    e: &Expr,
    takes_lambda: bool,
) -> fmt::Result {
    write!(buf, "{op}")?;
    match prefixed(e, takes_lambda) {
        true => pretty_parens(buf, e),
        false => e.fmt_pretty(buf),
    }
}

/// `t` where it stands on the line, flat when it fits, else laid out
/// over lines; no newline after.
fn pretty_type(buf: &mut PrettyBuf, t: &Type) -> fmt::Result {
    t.fmt_pretty(buf)?;
    buf.kill_newline();
    Ok(())
}

/// Whether what was written since `m`, at column `col`, fits the width.
fn fits_since(buf: &PrettyBuf, m: Mark, col: usize) -> bool {
    col + buf.first_line_width(m) <= buf.limit
        && buf.since(m).lines().skip(1).all(|l| l.chars().count() <= buf.limit)
}

/// An operand, in parens when `parens` says so.
struct Operand<'a>(&'a Expr, bool);

impl fmt::Display for Operand<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self.1 {
            true => write!(f, "({})", self.0),
            false => write!(f, "{}", self.0),
        }
    }
}

/// `pretty_then` of an operand, in parens when `parens` says so.
fn pretty_operand(
    buf: &mut PrettyBuf,
    e: &Expr,
    parens: bool,
    suffix: &str,
) -> fmt::Result {
    match parens {
        false => pretty_then(buf, e, suffix),
        true => {
            pretty_parens(buf, e)?;
            buf.kill_newline();
            writeln!(buf, "{suffix}")
        }
    }
}

/// What a catch seq lowering installed does past its handler; nothing for
/// the program's own. Printed for `--expand`, never parsed.
struct Role<'a>(&'a CatchRole);

impl fmt::Display for Role<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self.0 {
            CatchRole::User => Ok(()),
            CatchRole::Machine { action, manual, pc, clock } => {
                write!(f, " then {action} (machine {pc}")?;
                if let Some(c) = clock {
                    write!(f, ", queue clock {c}")?;
                }
                match manual {
                    None => write!(f, ")"),
                    Some(m) => write!(f, ", abort on {m})"),
                }
            }
            CatchRole::Try { action, capture } => {
                write!(f, " then {action} (error to {capture})")
            }
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
