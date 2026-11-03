use super::{
    GRAPHIX_ESC, GRAPHIX_MUST_ESC, bridge, choice, expr,
    input::{Input, R},
};
use crate::expr::{
    Expr, ExprKind, StrForm,
    ornament::{holds_attr, holds_comment},
};
use arcstr::ArcStr;
use combine::stream::position::SourcePosition;
use compact_str::CompactString;
use netidx_value::{Value, parser::escaped_string};
use poolshark::local::LPooled;
use triomphe::Arc;

enum Intp {
    /// a run of text: where it starts and ends
    Lit(SourcePosition, SourcePosition, CompactString),
    Expr(Expr),
}

fn text(s: &str) -> ExprKind {
    ExprKind::Constant(Value::String(ArcStr::from(s)))
}

impl Intp {
    fn to_expr(self) -> Expr {
        match self {
            Intp::Lit(pos, end, s) => {
                text(&s).to_expr(pos).ending(end).written_as(StrForm::Text)
            }
            Intp::Expr(s) => s,
        }
    }
}

// Adjacent literal parts merge so the AST is canonical whichever
// delimiter the author wrote.
fn finish(pos: SourcePosition, mut toks: LPooled<Vec<Intp>>) -> Expr {
    let mut merged: LPooled<Vec<Intp>> = LPooled::take();
    for t in toks.drain(..) {
        match (merged.last_mut(), t) {
            (Some(Intp::Lit(_, prev_end, prev)), Intp::Lit(_, end, s)) => {
                prev.push_str(&s);
                *prev_end = end;
            }
            (_, t) => merged.push(t),
        }
    }
    // a string with no splice is one constant, quotes and all
    let whole = match &merged[..] {
        [] => Some(""),
        [Intp::Lit(_, _, s)] => Some(s.as_str()),
        _ => None,
    };
    match whole {
        Some(s) => text(s).to_expr(pos),
        None => ExprKind::StringInterpolate {
            args: Arc::from_iter(merged.drain(..).map(Intp::to_expr)),
        }
        .to_expr(pos),
    }
}

// a splice prints on its string's line, where no comment, doc or
// attribute can stand
fn spliced(i: &mut Input, e: Expr, end: SourcePosition) -> R<Intp> {
    match (holds_comment(&e), holds_attr(&e)) {
        (true, _) => {
            Err(i.refuse(end, "a comment cannot stand inside a string's splice"))
        }
        (_, true) => {
            Err(i.refuse(end, "an attribute cannot stand inside a string's splice"))
        }
        (false, false) => Ok(Intp::Expr(e)),
    }
}

fn interp_part(i: &mut Input) -> R<Intp> {
    i.attempt(|i| {
        i.token('[')?;
        let e = expr(i)?;
        let end = i.pos();
        i.sptoken(']')?;
        spliced(i, e, end)
    })
}

// A `[` that opens no well-formed `[expr]` ends the string parse; the note
// names the escape.
fn bracket_note(i: &mut Input) -> R<Intp> {
    let pos = i.pos();
    i.token('[')?;
    i.g.note_reason(pos, None, || {
        CompactString::const_new(
            "`[` opens an interpolated expression inside a string; write \
             `\\[` for a literal bracket (or a \"\"\"template\"\"\", where \
             brackets are plain text)",
        )
    });
    Err(i.unexpected("string interpolation"))
}

fn chunk_part(i: &mut Input) -> R<Intp> {
    let pos = i.pos();
    let s = bridge!(i, escaped_string(&GRAPHIX_MUST_ESC, &GRAPHIX_ESC))?;
    let end = i.pos();
    match s.is_empty() {
        true => Err(i.unexpected("empty string")),
        false => Ok(Intp::Lit(pos, end, CompactString::from(s))),
    }
}

// Template form: brackets are content and the splice is marked `\[expr]`;
// `\]` is an error; content ends at the first unescaped `"""`; one newline
// right after the opener is stripped.
fn splice_part(i: &mut Input) -> R<Intp> {
    i.attempt(|i| i.string("\\["))?;
    let e = expr(i)?;
    let end = i.pos();
    i.sptoken(']')?;
    spliced(i, e, end)
}

fn triple_run(i: &mut Input) -> R<Intp> {
    let pos = i.pos();
    let s = i.take_while1(|c| c != '"' && c != '\\')?;
    Ok(Intp::Lit(pos, i.pos(), CompactString::from(s)))
}

fn triple_escape(i: &mut Input) -> R<Intp> {
    const ESCAPES: [(char, char); 6] =
        [('n', '\n'), ('r', '\r'), ('t', '\t'), ('0', '\0'), ('"', '"'), ('\\', '\\')];
    let pos = i.pos();
    let c = i.attempt(|i| {
        i.token('\\')?;
        let mut best = None;
        for (t, c) in ESCAPES {
            if i.alt(&mut best, |i| i.token(t))?.is_some() {
                return Ok(c);
            }
        }
        Err(best.unwrap())
    })?;
    Ok(Intp::Lit(pos, i.pos(), CompactString::from_iter([c])))
}

fn triple_quote(i: &mut Input) -> R<Intp> {
    let pos = i.pos();
    i.attempt(|i| {
        i.token('"')?;
        i.not_followed_by(|i| i.string("\"\""))
    })?;
    Ok(Intp::Lit(pos, i.pos(), CompactString::const_new("\"")))
}

fn triple(i: &mut Input) -> R<Expr> {
    let pos = i.pos();
    i.attempt(|i| i.string("\"\"\""))?;
    i.optional(|i| i.token('\n'))?;
    let mut toks: LPooled<Vec<Intp>> = LPooled::take();
    i.many(
        |i| choice!(i, splice_part, triple_escape, triple_run, triple_quote),
        |t| toks.push(t),
    )?;
    i.string("\"\"\"")?;
    Ok(finish(pos, toks).written_as(StrForm::Template))
}

fn single(i: &mut Input) -> R<Expr> {
    let pos = i.pos();
    i.token('"')?;
    let mut toks: LPooled<Vec<Intp>> = LPooled::take();
    i.many(|i| choice!(i, interp_part, chunk_part, bracket_note), |t| toks.push(t))?;
    i.token('"')?;
    Ok(finish(pos, toks))
}

pub(super) fn interpolated(i: &mut Input) -> R<Expr> {
    i.knot(|i| choice!(i, triple, single))
}
