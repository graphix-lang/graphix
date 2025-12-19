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
                            (Some(Expr { kind: ExprKind::Bind { .. }, .. }), _)
                                | (Some(Expr { kind: ExprKind::StructWith { .. }, .. }), _)
                                | (Some(Expr { kind: ExprKind::Array { .. }, .. }), _)
                                | (Some(Expr { kind: ExprKind::Map { .. }, .. }), _)
                                | (Some(Expr { kind: ExprKind::MapRef { .. }, .. }), _)
                                | (Some(Expr { kind: ExprKind::Any { .. }, .. }), _)
                                | (Some(Expr { kind: ExprKind::StructRef { .. }, .. }), _)
                                | (Some(Expr { kind: ExprKind::TupleRef { .. }, .. }), _)
                                | (Some(Expr { kind: ExprKind::Tuple { .. }, .. }), _)
                                | (Some(Expr { kind: ExprKind::Variant { .. }, .. }), _)
                                | (Some(Expr { kind: ExprKind::Struct { .. }, .. }), _)
                                | (Some(Expr { kind: ExprKind::Qop(_), .. }), _)
                                | (Some(Expr { kind: ExprKind::OrNever(_), .. }), _)
                                | (Some(Expr { kind: ExprKind::TryCatch(_), .. }), _)
                                | (Some(Expr { kind: ExprKind::NoOp {..}, .. }), _)
                                | (Some(Expr { kind: ExprKind::Do { .. }, .. }), _)
                                | (Some(Expr { kind: ExprKind::Module { .. }, .. }), _)
                                | (Some(Expr { kind: ExprKind::Use { .. }, .. }), _)
                                | (Some(Expr { kind: ExprKind::Connect { .. }, .. }), _)
                                | (Some(Expr { kind: ExprKind::Ref { .. }, .. }), _)
                                | (Some(Expr { kind: ExprKind::Eq { .. }, .. }), _)
                                | (Some(Expr { kind: ExprKind::Ne { .. }, .. }), _)
                                | (Some(Expr { kind: ExprKind::Lt { .. }, .. }), _)
                                | (Some(Expr { kind: ExprKind::Gt { .. }, .. }), _)
                                | (Some(Expr { kind: ExprKind::Gte { .. }, .. }), _)
                                | (Some(Expr { kind: ExprKind::Lte { .. }, .. }), _)
                                | (Some(Expr { kind: ExprKind::And { .. }, .. }), _)
                                | (Some(Expr { kind: ExprKind::Or { .. }, .. }), _)
                                | (Some(Expr { kind: ExprKind::Not { .. }, .. }), _)
                                | (Some(Expr { kind: ExprKind::Add { .. }, .. }), _)
                                | (Some(Expr { kind: ExprKind::Sub { .. }, .. }), _)
                                | (Some(Expr { kind: ExprKind::Mul { .. }, .. }), _)
                                | (Some(Expr { kind: ExprKind::Div { .. }, .. }), _)
                                | (Some(Expr { kind: ExprKind::Mod { .. }, .. }), _)
                                | (Some(Expr { kind: ExprKind::Select { .. }, .. }), _)
                                | (Some(Expr { kind: ExprKind::TypeCast { .. }, .. }), _)
                                | (Some(Expr { kind: ExprKind::TypeDef { .. }, .. }), _)
                                | (Some(Expr { kind: ExprKind::ArrayRef { .. }, .. }), _)
                                | (Some(Expr { kind: ExprKind::ArraySlice { .. }, .. }), _)
                                | (Some(Expr { kind: ExprKind::Apply { .. }, .. }), _)
                                | (Some(Expr { kind: ExprKind::ByRef(_), .. }), _)
                                | (Some(Expr { kind: ExprKind::Deref(_), .. }), _)
                                | (Some(Expr { kind: ExprKind::Sample { .. }, .. }), _)
                                | (Some(Expr { kind: ExprKind::Lambda { .. }, .. }), _) => {
                                    unreachable!()
                                }
                        }
                    })
                    .unwrap_or_else(|| ExprKind::Constant(Value::from("")).to_expr(pos))
            })
    }
}
