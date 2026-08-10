use super::{GRAPHIX_ESC, GRAPHIX_MUST_ESC, expr, grow::grow, sptoken};
use crate::expr::{Expr, ExprId, ExprKind, get_origin};
use combine::{
    RangeStream, attempt, between, choice, many, position,
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
        grow((
            position(),
            between(
                token('"'),
                token('"'),
                many(choice((
                    attempt(between(token('['), sptoken(']'), expr()).map(Intp::Expr)),
                    (position(), escaped_string(&GRAPHIX_MUST_ESC, &GRAPHIX_ESC))
                    .then(|(pos, s)| {
                        if s.is_empty() {
                            unexpected_any("empty string").right()
                        } else {
                            value(Intp::Lit(pos, s)).left()
                        }
                    }),
                ))),
            ),
        )
            .map(|(pos, mut toks): (_, LPooled<Vec<Intp>>)| {
                // A lone literal is a plain constant; anything else — any
                // interpolated expr, or several parts — is one
                // StringInterpolate over the parts in order.
                match &toks[..] {
                    [] => ExprKind::Constant(Value::from("")).to_expr(pos),
                    [Intp::Lit(_, _)] => toks.drain(..).next().unwrap().to_expr(),
                    _ => ExprKind::StringInterpolate {
                        args: Arc::from_iter(toks.drain(..).map(Intp::to_expr)),
                    }
                    .to_expr(pos),
                }
            }))
    }
}
