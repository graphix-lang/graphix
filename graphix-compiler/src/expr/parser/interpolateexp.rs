use super::{
    GRAPHIX_ESC, GRAPHIX_MUST_ESC, expr,
    grow::{grow, note_reason},
    sptoken,
};
use crate::expr::{Expr, ExprId, ExprKind, StrForm, WrittenAt, get_origin};
use combine::{
    RangeStream, attempt, between, choice, many, not_followed_by, optional,
    parser::char::string,
    position,
    stream::{Range, position::SourcePosition},
    token, unexpected_any, value,
};
use compact_str::CompactString;
use netidx_value::Value;
use netidx_value::parser::escaped_string;
use poolshark::local::LPooled;
use triomphe::Arc;

parser! {
    pub(super) fn interpolated[I]()(I) -> Expr
    where [I: RangeStream<Token = char, Position = SourcePosition>, I::Range: Range]
    {
        #[derive(Debug, Clone)]
        enum Intp {
            // a run of text: where it starts and ends
            Lit(SourcePosition, SourcePosition, String),
            Expr(Expr),
        }
        impl Intp {
            fn to_expr(self) -> Expr {
                match self {
                    Intp::Lit(pos, end, s) => Expr {
                        id: ExprId::new(),
                        ori: get_origin(),
                        pos,
                        kind: ExprKind::Constant(Value::from(s)),
                        dec: None,
                        str_form: Default::default(),
                        end: WrittenAt(end),
                    },
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
            let whole = match &mut merged[..] {
                [] => Some(String::new()),
                [Intp::Lit(_, _, s)] => Some(std::mem::take(s)),
                _ => None,
            };
            match whole {
                Some(s) => ExprKind::Constant(Value::from(s)).to_expr(pos),
                None => ExprKind::StringInterpolate {
                    args: Arc::from_iter(merged.drain(..).map(Intp::to_expr)),
                }
                .to_expr(pos),
            }
        }
        let interp_part = || attempt(
            between(token('['), sptoken(']'), expr()).map(Intp::Expr)
        );
        // A `[` that opens no well-formed `[expr]` ends the string parse;
        // the note names the escape.
        let bracket_note = || (position(), token('[')).then(|(pos, _)| {
            note_reason(
                pos,
                None,
                CompactString::const_new(
                    "`[` opens an interpolated expression inside a string; write \
                     `\\[` for a literal bracket (or a \"\"\"template\"\"\", where \
                     brackets are plain text)",
                ),
            );
            unexpected_any("string interpolation").map(|_: ()| unreachable!())
        });
        let chunk_part = || (
            position(),
            escaped_string(&GRAPHIX_MUST_ESC, &GRAPHIX_ESC),
            position(),
        )
            .then(|(pos, s, end)| {
                if s.is_empty() {
                    unexpected_any("empty string").right()
                } else {
                    value(Intp::Lit(pos, end, s)).left()
                }
            });
        // Template form: brackets are content and the splice is marked
        // `\[expr]`; `\]` is an error; content ends at the first unescaped
        // `"""`; one newline right after the opener is stripped.
        let splice_part = || attempt(string("\\["))
            .with(expr())
            .skip(sptoken(']'))
            .map(Intp::Expr);
        let triple_run = || (
            position(),
            combine::many1::<String, _, _>(combine::satisfy(|c| {
                c != '"' && c != '\\'
            })),
            position(),
        )
            .map(|(pos, s, end)| Intp::Lit(pos, end, s));
        let triple_escape = || (position(), attempt(token('\\').with(choice((
            token('n').map(|_| '\n'),
            token('r').map(|_| '\r'),
            token('t').map(|_| '\t'),
            token('0').map(|_| '\0'),
            token('"').map(|_| '"'),
            token('\\').map(|_| '\\'),
        )))), position())
            .map(|(pos, c, end)| Intp::Lit(pos, end, String::from(c)));
        let triple = (
            position(),
            between(
                attempt(string("\"\"\"")),
                string("\"\"\""),
                (
                    optional(attempt(string("\r\n")).or(string("\n"))),
                    many(choice((
                        splice_part(),
                        triple_escape(),
                        triple_run(),
                        attempt((
                            position(),
                            token('"').skip(not_followed_by(string("\"\""))),
                            position(),
                        ))
                        .map(|(pos, _, end)| Intp::Lit(pos, end, String::from("\""))),
                    ))),
                )
                    .map(|(_, toks)| toks),
            ),
        )
            .map(|(pos, toks): (_, LPooled<Vec<Intp>>)| {
                finish(pos, toks).written_as(StrForm::Template)
            });
        let single = (
            position(),
            between(
                token('"'),
                token('"'),
                many(choice((interp_part(), chunk_part(), bracket_note()))),
            ),
        )
            .map(|(pos, toks): (_, LPooled<Vec<Intp>>)| finish(pos, toks));
        grow(choice((triple, single)))
    }
}
