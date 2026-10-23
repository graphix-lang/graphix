use super::{
    attribute, csep, doc_comment, expr,
    grow::refuse,
    leading_comments, name, semisep, sep_by_tok, sep_by1_tok, spaces, spaces1, sptoken,
    typexp::{flatten_bounds, tvar_opt_bound, typ, typath},
    typname,
};
use crate::{
    expr::{
        Attr, Comments, Doc, Expr, ExprKind, ImplExpr, Name, StructurePattern, TraitExpr,
        TraitMethod,
    },
    typ::{FnArgKind, TVar, Type},
};
use arcstr::ArcStr;
use combine::{
    ParseError, Parser, RangeStream, attempt, between, many, optional,
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
/// interface (`sig`) the `///` doc lines below those are its doc, and
/// its `#[..]` requirements follow.
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
        leading_comments(sig),
        doc().skip(spaces()),
        many(attribute().skip(spaces())),
        attempt(string("val").skip(spaces1())).with(name()).skip(sptoken(':')),
        typ(),
        optional(attempt(sptoken('=')).with(expr())),
        position(),
    )
        .then(|(mut comments, doc, mut attrs, name, typ, default, end): (
            LPooled<Vec<ArcStr>>,
            Doc,
            LPooled<Vec<Attr>>,
            Name,
            Type,
            Option<Expr>,
            _,
        )| match &typ {
            Type::Fn(ft) => {
                let ft = ft.clone();
                let self_index = ft.args.iter().position(|a| {
                    matches!(&a.kind, FnArgKind::Positional { name: Some(n) } if &**n == "self")
                });
                // CR claude for claude: [bug/medium] Any trait may declare a method whose
                // only `self` is its result, but only core Cast::from_value has a
                // caller (the runtime). In a user trait (`val mk: fn(x: i64) -> self`)
                // the trait and the impl check, then every call, a default body's
                // included, is refused with "Mk::mk called without its self argument".
                // Either refuse this form outside core traits at the declaration, or
                // dispatch such a call on its expected result type. probe:
                // design/review-2026-10-19/repro/t-parser-b-self-result-uncallable.gx
                // (t-parser-b-04)
                // 2026-10-23 claude: Readdressed to claude in the 2026-10-23 triage: no
                // ruling needed. Refuse this form outside core traits at the declaration,
                // with a message naming why (with tests-fusion-traits-03).
                if self_index.is_none() && !ft.rtype.mentions_self() {
                    return refuse(
                        end,
                        "a trait method takes a `self` parameter (`fn(self, ..)`) or \
                         returns `self`",
                    )
                    .right();
                }
                let comments = Comments::of(comments.drain(..));
                let attrs = Arc::from_iter(attrs.drain(..));
                value(TraitMethod { comments, doc, attrs, name, typ: ft, self_index, default })
                    .left()
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
        spaces().with(position()),
        typath(),
        spaces1().with(string("for")).with(spaces1()).with(typ()),
        spaces().with(optional(between(
            token('{'),
            sptoken('}'),
            spaces().with(sep_by_tok(expr(), semisep(), token('}'))),
        ))),
        position(),
    )
        .then(
            |(params, at_params, (trait_name, trait_at), target, methods, end): (
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
                    trait_at,
                    params: tvs,
                    constraints: Arc::from_iter(constraints.drain(..)),
                    target,
                    methods: Arc::from_iter(ms.drain(..)),
                })
                .left()
            },
        )
}
