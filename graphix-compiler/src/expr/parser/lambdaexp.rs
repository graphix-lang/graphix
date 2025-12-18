use super::{
    csep, expr, fname, qop, reference, spaces, sptoken, structure_pattern,
    typ::{tvar, typexp},
};
use crate::{
    expr::{
        parser::{sep_by_tok, spaces1},
        ApplyExpr, Arg, Expr, ExprKind, Lambda, StructurePattern,
    },
    typ::{TVar, Type},
};
use anyhow::{bail, Result};
use arcstr::ArcStr;
use combine::{
    attempt, between, choice, not_followed_by, optional, parser::char::string, position,
    sep_by, stream::Range, token, unexpected_any, value, ParseError, Parser, RangeStream,
};
use netidx::utils::Either;
use poolshark::local::LPooled;
use triomphe::Arc;

fn apply_pexp<I>() -> impl Parser<I, Output = Expr>
where
    I: RangeStream<Token = char>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    spaces().with(choice((between(token('('), sptoken(')'), expr()), qop(reference()))))
}

fn applyarg<I>() -> impl Parser<I, Output = (Option<ArcStr>, Expr)>
where
    I: RangeStream<Token = char>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    position().with(spaces()).then(|pos| {
        choice((
            token('#').with(fname()).skip(spaces()).then(|name| {
                optional(token(':').with(expr())).map(|e| match e {
                    Some(e) => (Some(name), e),
                    None => {
                        let e =
                            ExprKind::Ref { name: [name.clone()].into() }.to_expr(pos);
                        (Some(name), e)
                    }
                })
            }),
            expr().map(|e| (None, e)),
        ))
    })
}

pub(super) fn apply<I>() -> impl Parser<I, Output = Expr>
where
    I: RangeStream<Token = char>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (
        position(),
        apply_pexp(),
        between(sptoken('('), sptoken(')'), sep_by_tok(applyarg(), csep(), sptoken(')'))),
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
    I: RangeStream<Token = char>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    sep_by(
        (
            spaces().with(choice((
                string("@args").map(|s| (false, StructurePattern::Bind(ArcStr::from(s)))),
                token('#').with(fname()).map(|b| (true, StructurePattern::Bind(b))),
                structure_pattern().map(|p| (false, p)),
            ))),
            spaces().with(optional(token(':').with(typexp()))),
            spaces().with(optional(token('=').with(expr()))),
        ),
        csep(),
    )
    .then(|v: LPooled<Vec<((bool, StructurePattern), Option<Type>, Option<Expr>)>>| {
        let args = v
            .into_iter()
            .map(|((labeled, pattern), constraint, default)| {
                if !labeled && default.is_some() {
                    bail!("labeled")
                } else {
                    Ok(Arg { labeled: labeled.then_some(default), pattern, constraint })
                }
            })
            .collect::<Result<LPooled<Vec<_>>>>();
        match args {
            Ok(a) => value(a).right(),
            Err(_) => {
                unexpected_any("only labeled arguments may have a default value").left()
            }
        }
    })
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
    I: RangeStream<Token = char>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (
        position(),
        spaces()
            .with(sep_by((tvar().skip(sptoken(':')), typexp()), csep()))
            .map(|mut tvs: LPooled<Vec<(TVar, Type)>>| Arc::from_iter(tvs.drain(..))),
        between(sptoken('|'), sptoken('|'), lambda_args()),
        spaces().with(optional(string("->").with(typexp()))),
        optional(attempt(
            spaces1().with(string("throws").with(spaces1()).with(typexp())),
        )),
        spaces1().with(choice((
            token('\'')
                .with(fname())
                .skip(not_followed_by(sptoken(':')))
                .map(Either::Right),
            expr().map(|e| Either::Left(e)),
        ))),
    )
        .map(|(pos, constraints, (mut args, vargs), rtype, throws, body)| {
            let args = Arc::from_iter(args.drain(..));
            ExprKind::Lambda(Arc::new(Lambda {
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
