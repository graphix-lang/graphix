use super::{
    csep, expr, fname, qop, reference, spaces, sptoken, structure_pattern,
    typexp::{tvar, typ},
};
use crate::{
    expr::{
        parser::{sep_by_tok, spaces1},
        ApplyExpr, Arg, Expr, ExprKind, LambdaExpr, StructurePattern,
    },
    typ::{TVar, Type},
};
use anyhow::{bail, Result};
use arcstr::ArcStr;
use combine::{
    attempt, between, choice, not_followed_by, optional,
    parser::char::string,
    position,
    stream::{position::SourcePosition, Range},
    token, unexpected_any, value, ParseError, Parser, RangeStream,
};
use netidx::utils::Either;
use poolshark::local::LPooled;
use triomphe::Arc;

pub(super) fn apply_pexp<I>() -> impl Parser<I, Output = Expr>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    spaces().with(choice((between(token('('), sptoken(')'), expr()), qop(reference()))))
}

fn applyarg<I>() -> impl Parser<I, Output = (Option<ArcStr>, Expr)>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    position().skip(spaces()).then(|pos| {
        choice((
            token('#').with(fname()).skip(spaces()).then(move |name| {
                let name = name.clone();
                optional(token(':').with(expr())).map(move |e| match e {
                    Some(e) => (Some(name.clone()), e),
                    None => {
                        let e =
                            ExprKind::Ref { name: [name.clone()].into() }.to_expr(pos);
                        (Some(name.clone()), e)
                    }
                })
            }),
            expr().map(|e| (None, e)),
        ))
    })
}

pub(super) fn apply<I>() -> impl Parser<I, Output = Expr>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (
        position(),
        apply_pexp(),
        between(sptoken('('), sptoken(')'), sep_by_tok(applyarg(), csep(), token(')'))),
    )
        .then(|(pos, function, args): (_, Expr, Vec<(Option<ArcStr>, Expr)>)| {
            let mut anon = false;
            for (a, _) in &args {
                if a.is_some() && anon {
                    return unexpected_any(
                        "labeled arguments must come before anonymous arguments",
                    )
                    .right();
                }
                anon |= a.is_none();
            }
            value((pos, function, args)).left()
        })
        .map(|(pos, function, args): (_, Expr, Vec<(Option<ArcStr>, Expr)>)| {
            ExprKind::Apply(ApplyExpr {
                function: Arc::new(function),
                args: Arc::from(args),
            })
            .to_expr(pos)
        })
}

pub(super) fn lambda_args<I>(
) -> impl Parser<I, Output = (LPooled<Vec<Arg>>, Option<Option<Type>>)>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    sep_by_tok(
        (
            spaces().with(choice((
                string("@args").map(|s| (false, StructurePattern::Bind(ArcStr::from(s)))),
                token('#').with(fname()).map(|b| (true, StructurePattern::Bind(b))),
                structure_pattern().map(|p| (false, p)),
            ))),
            spaces().with(optional(token(':').with(typ()))),
            spaces().with(optional(token('=').with(expr()))),
        ),
        csep(),
        sptoken('|'),
    )
    .then(
        |mut v: LPooled<Vec<((bool, StructurePattern), Option<Type>, Option<Expr>)>>| {
            let args = v
                .drain(..)
                .map(|((labeled, pattern), constraint, default)| {
                    if !labeled && default.is_some() {
                        bail!("labeled")
                    } else {
                        Ok(Arg {
                            labeled: labeled.then_some(default),
                            pattern,
                            constraint,
                        })
                    }
                })
                .collect::<Result<LPooled<Vec<_>>>>();
            match args {
                Ok(a) => value(a).right(),
                Err(_) => {
                    unexpected_any("only labeled arguments may have a default value")
                        .left()
                }
            }
        },
    )
    // @args must be last
    .then(|mut v: LPooled<Vec<Arg>>| {
        match v.iter().enumerate().find(|(_, a)| match &a.pattern {
            StructurePattern::Bind(n) if n == "@args" => true,
            _ => false,
        }) {
            None => value((v, None)).left(),
            Some((i, _)) => {
                if i == v.len() - 1 {
                    let a = v.pop().unwrap();
                    value((v, Some(a.constraint))).left()
                } else {
                    unexpected_any("@args must be the last argument").right()
                }
            }
        }
    })
    // labeled before anonymous args
    .then(|(v, vargs): (LPooled<Vec<Arg>>, Option<Option<Type>>)| {
        let mut anon = false;
        for a in v.iter() {
            if a.labeled.is_some() && anon {
                return unexpected_any("labeled args must come before anon args").right();
            }
            anon |= a.labeled.is_none();
        }
        value((v, vargs)).left()
    })
}

pub(super) fn lambda<I>() -> impl Parser<I, Output = Expr>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (
        position(),
        spaces()
            .with(sep_by_tok((tvar().skip(sptoken(':')), typ()), csep(), token('|')))
            .map(|mut tvs: LPooled<Vec<(TVar, Type)>>| Arc::from_iter(tvs.drain(..))),
        between(sptoken('|'), sptoken('|'), lambda_args()),
        optional(attempt(spaces().with(string("->")).with(typ()))),
        optional(attempt(spaces1().with(string("throws")).with(spaces1()).with(typ()))),
        spaces1().with(choice((
            attempt(
                token('\'').with(fname()).skip(not_followed_by(attempt(sptoken(':')))),
            )
            .map(Either::Right),
            expr().map(|e| Either::Left(e)),
        ))),
    )
        .map(|(pos, constraints, (mut args, vargs), rtype, throws, body)| {
            let args = Arc::from_iter(args.drain(..));
            ExprKind::Lambda(Arc::new(LambdaExpr {
                args,
                vargs,
                rtype,
                throws,
                constraints,
                body,
            }))
            .to_expr(pos)
        })
}
