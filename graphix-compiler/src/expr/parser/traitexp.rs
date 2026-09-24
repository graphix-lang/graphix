use super::{
    csep, doc_comment, expr,
    grow::refuse,
    leading_comments, name, semisep, sep_by_tok, sep_by1_tok, spaces, spaces1, sptoken,
    typexp::{flatten_bounds, tvar_opt_bound, typ, typath},
    typname,
};
use crate::{
    expr::{
        Comments, Doc, Expr, ExprKind, ImplExpr, Name, StructurePattern, TraitExpr,
        TraitMethod,
    },
    typ::{FnArgKind, TVar, Type},
};
use arcstr::ArcStr;
use combine::{
    ParseError, Parser, RangeStream, attempt, between, optional,
    parser::char::string,
    position,
    stream::{Range, position::SourcePosition},
    token, value,
};
use netidx_value::parser::not_prefix;
use poolshark::local::LPooled;
use triomphe::Arc;

/// One trait item: `val name: fn(self, ..) -> T` with an optional
/// `= default` body. The signature must be a function type with a
/// positional `self` parameter. The `//` lines above it are kept; in an
/// interface (`sig`) the `///` doc lines below those are its doc.
fn trait_method<I>(sig: bool) -> impl Parser<I, Output = TraitMethod>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    let doc = move || match sig {
        true => doc_comment().left(),
        false => value(Doc(None)).right(),
    };
    (
        leading_comments(),
        doc().skip(spaces()),
        attempt(string("val").skip(spaces1())).with(name()).skip(sptoken(':')),
        typ(),
        optional(attempt(sptoken('=')).with(expr())),
        position(),
    )
        .then(|(mut comments, doc, name, typ, default, end)| match typ {
            Type::Fn(ft) => {
                let self_index = ft.args.iter().position(|a| {
                    matches!(&a.kind, FnArgKind::Positional { name: Some(n) } if &**n == "self")
                });
                match self_index {
                    None => refuse(
                        end,
                        "a trait method needs a `self` parameter (`fn(self, ..)`)",
                    )
                    .right(),
                    Some(self_index) => {
                        let comments = Comments::of(comments.drain(..));
                        value(TraitMethod { comments, doc, name, typ: ft, self_index, default })
                            .left()
                    }
                }
            }
            _ => refuse(end, "a trait method must have a function type").right(),
        })
}

/// A trait declaration; `sig` in an interface, where methods carry docs.
pub(super) fn trait_decl<I>(sig: bool) -> impl Parser<I, Output = TraitExpr>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (
        attempt(string("trait").skip(spaces1())).with((position(), typname())),
        spaces().with(between(
            token('{'),
            sptoken('}'),
            spaces().with(sep_by_tok(trait_method(sig), semisep(), token('}'))),
        )),
        position(),
    )
        .then(
            |((at, name), mut methods, end): (
                (_, ArcStr),
                LPooled<Vec<TraitMethod>>,
                _,
            )| {
                let name = Name::written(name, at);
                let dup = |(i, m): (usize, &TraitMethod)| {
                    methods[..i].iter().any(|p| p.name.name == m.name.name)
                };
                if methods.iter().enumerate().any(dup) {
                    return refuse(end, "duplicate trait method").right();
                }
                let methods = Arc::from_iter(methods.drain(..));
                value(TraitExpr { name, methods }).left()
            },
        )
}

/// `impl<'a: C, ..> Trait for Target { let m = ..; .. }` — the body is
/// optional (`impl Trait for Target;` declares the implementation in an
/// interface, or implements a trait whose methods all have defaults).
pub(super) fn impl_decl<I>() -> impl Parser<I, Output = ImplExpr>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (
        attempt(string("impl").skip(not_prefix())).with(spaces()).with(optional(
            between(
                token('<'),
                sptoken('>'),
                sep_by1_tok(tvar_opt_bound(), csep(), token('>')),
            ),
        )),
        position(),
        typath(),
        spaces1().with(string("for")).with(spaces1()).with(typ()),
        // A following `{` may belong to an enclosing form: commit only
        // once it reads as a method block.
        spaces().with(optional(attempt(between(
            token('{'),
            sptoken('}'),
            spaces().with(sep_by_tok(expr(), semisep(), token('}'))),
        )))),
        position(),
    )
        .then(
            |(params, at_params, trait_name, target, methods, end): (
                Option<LPooled<Vec<(TVar, Option<LPooled<Vec<Type>>>)>>>,
                _,
                _,
                _,
                Option<LPooled<Vec<Expr>>>,
                _,
            )| {
                let mut params = params.unwrap_or_else(LPooled::take);
                let tvs: Arc<[TVar]> =
                    Arc::from_iter(params.iter().map(|(tv, _)| tv.clone()));
                if tvs
                    .iter()
                    .enumerate()
                    .any(|(i, tv)| tvs[..i].iter().any(|p| p.name == tv.name))
                {
                    return refuse(at_params, "duplicate impl type variable").right();
                }
                let mut constraints = flatten_bounds(
                    params.drain(..).filter_map(|(tv, b)| b.map(|b| (tv, b))),
                );
                let mut ms: LPooled<Vec<Expr>> = methods.unwrap_or_else(LPooled::take);
                let simple = |m: &Expr| {
                    matches!(&m.kind, ExprKind::Bind(b)
                        if matches!(b.pattern, StructurePattern::Bind(_)))
                };
                if !ms.iter().all(simple) {
                    return refuse(
                        end,
                        "an impl body holds only `let name = ..` methods",
                    )
                    .right();
                }
                value(ImplExpr {
                    trait_name,
                    params: tvs,
                    constraints: Arc::from_iter(constraints.drain(..)),
                    target,
                    methods: Arc::from_iter(ms.drain(..)),
                })
                .left()
            },
        )
}
