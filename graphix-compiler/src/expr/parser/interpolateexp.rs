use super::{expr, sptoken, GRAPHIX_ESC, GRAPHIX_MUST_ESC};
use crate::expr::{get_origin, Expr, ExprId, ExprKind};
use combine::{
    attempt, between, choice, many, position,
    stream::{position::SourcePosition, Range},
    token, unexpected_any, value, RangeStream,
};
use netidx::publisher::Value;
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
                    },
                    Intp::Expr(s) => s,
                }
            }
        }
        (
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
                let mut argvec = vec![];
                toks.drain(..)
                    .fold(None, |src, tok| -> Option<Expr> {
                        match (src, tok) {
                            (None, t @ Intp::Lit(_, _)) => Some(t.to_expr()),
                            (None, Intp::Expr(s)) => {
                                argvec.push(s);
                                Some(
                                    ExprKind::StringInterpolate {
                                        args: Arc::from_iter(argvec.clone().into_iter()),
                                    }
                                    .to_expr(pos),
                                )
                            }
                            (Some(src @ Expr { kind: ExprKind::Constant(_), .. }), s) => {
                                argvec.extend([src, s.to_expr()]);
                                Some(
                                    ExprKind::StringInterpolate {
                                        args: Arc::from_iter(argvec.clone().into_iter()),
                                    }
                                    .to_expr(pos),
                                )
                            }
                            (
                                Some(Expr {
                                    kind: ExprKind::StringInterpolate { args: _ },
                                    ..
                                }),
                                s,
                            ) => {
                                argvec.push(s.to_expr());
                                Some(
                                    ExprKind::StringInterpolate {
                                        args: Arc::from_iter(argvec.clone().into_iter()),
                                    }
                                    .to_expr(pos),
                                )
                            }
                            (_, _) => unreachable!(),
                        }
                    })
                    .unwrap_or_else(|| ExprKind::Constant(Value::from("")).to_expr(pos))
            })
    }
}
