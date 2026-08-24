use super::{
    csep, doc_comment, expr, fname, leading_comments, semisep, spaces, spaces1, sptoken,
    typexp::{bound, tvar, typ, typath},
    typname,
};
use crate::{
    expr::{Expr, ExprKind, ImplExpr, StructurePattern, TraitExpr, TraitMethod},
    typ::{FnArgKind, TVar, Type},
};
use ahash::AHashSet;
use arcstr::ArcStr;
use combine::{
    ParseError, Parser, RangeStream, attempt, between, optional,
    parser::char::string,
    position,
    stream::{Range, position::SourcePosition},
    token, unexpected_any, value,
};
use netidx_value::parser::{not_prefix, sep_by_tok, sep_by1_tok};
use poolshark::local::LPooled;
use triomphe::Arc;

/// One trait item: `val name: fn(self, ..) -> T` with an optional
/// `= default` body. The signature must be a function type with a
/// positional `self` parameter — that parameter's type is what selects
/// the implementation at a call.
fn trait_method<I>() -> impl Parser<I, Output = TraitMethod>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (
        leading_comments().with(doc_comment()).skip(spaces()),
        attempt(string("val").skip(spaces1())).with(fname()).skip(sptoken(':')),
        typ(),
        optional(attempt(sptoken('=')).with(expr())),
    )
        .then(|(doc, name, typ, default)| match typ {
            Type::Fn(ft) => {
                let self_index = ft.args.iter().position(|a| {
                    matches!(&a.kind, FnArgKind::Positional { name: Some(n) } if &**n == "self")
                });
                match self_index {
                    None => unexpected_any(
                        "a trait method needs a `self` parameter (`fn(self, ..)`)",
                    )
                    .left(),
                    Some(self_index) => {
                        value(TraitMethod { doc, name, typ: ft, self_index, default })
                            .right()
                    }
                }
            }
            _ => unexpected_any("a trait method must have a function type").left(),
        })
}

pub(super) fn trait_decl<I>() -> impl Parser<I, Output = Expr>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (
        position(),
        attempt(string("trait").skip(spaces1())).with(typname()),
        spaces().with(between(
            token('{'),
            sptoken('}'),
            spaces().with(sep_by_tok(trait_method(), semisep(), token('}'))),
        )),
    )
        .then(|(pos, name, mut methods): (_, ArcStr, LPooled<Vec<TraitMethod>>)| {
            let mut seen: LPooled<AHashSet<ArcStr>> = LPooled::take();
            for m in methods.iter() {
                if !seen.insert(m.name.clone()) {
                    return unexpected_any("duplicate trait method").left();
                }
            }
            let methods = Arc::from_iter(methods.drain(..));
            value(ExprKind::Trait(Arc::new(TraitExpr { name, methods })).to_expr(pos))
                .right()
        })
}

/// `impl<'a: C, ..> Trait for Target { let m = ..; .. }` — the body is
/// optional (`impl Trait for Target;` declares the implementation in an
/// interface, or implements a trait whose methods all have defaults).
pub(super) fn impl_decl<I>() -> impl Parser<I, Output = Expr>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (
        position(),
        attempt(string("impl").skip(not_prefix())).with(spaces()).with(optional(
            between(
                token('<'),
                sptoken('>'),
                sep_by1_tok(
                    (
                        spaces().with(tvar()),
                        spaces().then(|_| optional(token(':').with(bound()))),
                    ),
                    csep(),
                    token('>'),
                ),
            ),
        )),
        typath(),
        spaces1().with(string("for")).with(spaces1()).with(typ()),
        // the body is optional, and a following `{` may belong to an
        // enclosing form (`select impl T for X { arms }`): commit only
        // once it reads as a method block
        spaces().with(optional(attempt(between(
            token('{'),
            sptoken('}'),
            spaces().with(sep_by_tok(expr(), semisep(), token('}'))),
        )))),
    )
        .then(
            |(pos, params, trait_name, target, methods): (
                _,
                Option<LPooled<Vec<(TVar, Option<LPooled<Vec<Type>>>)>>>,
                _,
                _,
                Option<LPooled<Vec<Expr>>>,
            )| {
                let mut tvs: LPooled<Vec<TVar>> = LPooled::take();
                let mut constraints: LPooled<Vec<(TVar, Type)>> = LPooled::take();
                let mut seen: LPooled<AHashSet<ArcStr>> = LPooled::take();
                if let Some(mut params) = params {
                    for (tv, bounds) in params.drain(..) {
                        if !seen.insert(tv.name.clone()) {
                            return unexpected_any("duplicate impl type variable").left();
                        }
                        if let Some(mut bounds) = bounds {
                            for b in bounds.drain(..) {
                                constraints.push((tv.clone(), b));
                            }
                        }
                        tvs.push(tv);
                    }
                }
                let mut ms: LPooled<Vec<Expr>> = methods.unwrap_or_else(LPooled::take);
                for m in ms.iter() {
                    match &m.kind {
                        ExprKind::Bind(b)
                            if matches!(b.pattern, StructurePattern::Bind(_)) => {}
                        _ => {
                            return unexpected_any(
                                "an impl body holds only `let name = ..` methods",
                            )
                            .left();
                        }
                    }
                }
                let im = ImplExpr {
                    trait_name,
                    params: Arc::from_iter(tvs.drain(..)),
                    constraints: Arc::from_iter(constraints.drain(..)),
                    target,
                    methods: Arc::from_iter(ms.drain(..)),
                };
                value(ExprKind::Impl(Arc::new(im)).to_expr(pos)).right()
            },
        )
}
