use super::{GRAPHIX_ESC, GRAPHIX_MUST_ESC, expr, grow::grow, sptoken};
use crate::expr::{Expr, ExprId, ExprKind, get_origin};
use combine::{
    RangeStream, attempt, between, choice, many, not_followed_by, optional,
    parser::char::string,
    position,
    stream::{Range, position::SourcePosition},
    token, unexpected_any, value,
};
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
            Lit(SourcePosition, String),
            Expr(Expr),
        }
        impl Intp {
            fn to_expr(self) -> Expr {
                match self {
                    Intp::Lit(pos, s) => Expr {
                        id: ExprId::new(),
                        ori: get_origin(),
                        pos,
                        kind: ExprKind::Constant(Value::from(s)),
                        dec: None,
                    },
                    Intp::Expr(s) => s,
                }
            }
        }
        // Adjacent literal parts merge (the triple form's bare-quote
        // continuation splits chunks at every quote), so the AST is
        // canonical whichever delimiter the author wrote.
        fn finish(pos: SourcePosition, mut toks: LPooled<Vec<Intp>>) -> Expr {
            let mut merged: LPooled<Vec<Intp>> = LPooled::take();
            for t in toks.drain(..) {
                match (merged.last_mut(), t) {
                    (Some(Intp::Lit(_, prev)), Intp::Lit(_, s)) => prev.push_str(&s),
                    (_, t) => merged.push(t),
                }
            }
            // A lone literal is a plain constant; anything else — any
            // interpolated expr, or several parts — is one
            // StringInterpolate over the parts in order.
            match &merged[..] {
                [] => ExprKind::Constant(Value::from("")).to_expr(pos),
                [Intp::Lit(_, _)] => merged.drain(..).next().unwrap().to_expr(),
                _ => ExprKind::StringInterpolate {
                    args: Arc::from_iter(merged.drain(..).map(Intp::to_expr)),
                }
                .to_expr(pos),
            }
        }
        let interp_part = || attempt(
            between(token('['), sptoken(']'), expr()).map(Intp::Expr)
        );
        let chunk_part = || (
            position(),
            escaped_string(&GRAPHIX_MUST_ESC, &GRAPHIX_ESC),
        )
            .then(|(pos, s)| {
                if s.is_empty() {
                    unexpected_any("empty string").right()
                } else {
                    value(Intp::Lit(pos, s)).left()
                }
            });
        // Triple-quoted form: IDENTICAL to the normal form — same
        // escapes, same interpolation — except a bare `"` is legal
        // (content ends at the FIRST unescaped `"""`; a quote that
        // would begin the terminator is written `\"`), and one newline
        // immediately after the opener is stripped (so the template's
        // first line needn't share the opener's line).
        let triple = (
            position(),
            between(
                attempt(string("\"\"\"")),
                string("\"\"\""),
                (
                    optional(attempt(string("\r\n")).or(string("\n"))),
                    many(choice((
                        interp_part(),
                        chunk_part(),
                        attempt((
                            position(),
                            token('"').skip(not_followed_by(string("\"\""))),
                        ))
                        .map(|(pos, _)| Intp::Lit(pos, String::from("\""))),
                    ))),
                )
                    .map(|(_, toks)| toks),
            ),
        )
            .map(|(pos, toks): (_, LPooled<Vec<Intp>>)| finish(pos, toks));
        let single = (
            position(),
            between(
                token('"'),
                token('"'),
                many(choice((interp_part(), chunk_part()))),
            ),
        )
            .map(|(pos, toks): (_, LPooled<Vec<Intp>>)| finish(pos, toks));
        grow(choice((triple, single)))
    }
}
