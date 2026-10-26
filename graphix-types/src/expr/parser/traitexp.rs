use super::{
    attribute, csep, decl_items, doc_comment, expr,
    grow::refuse,
    leading_comments, name, sep_by1_tok, spaces, spaces1, sptoken, statements, trail,
    typexp::{flatten_bounds, tvar_opt_bound, typ, typath},
    typname,
};
use crate::{
    expr::{
        Attr, Comments, Doc, Eol, Expr, ExprKind, ImplExpr, Name, StructurePattern,
        TraitExpr, TraitMethod,
    },
    typ::{FnArgKind, TVar, Type},
};
use arcstr::ArcStr;
use combine::{
    ParseError, Parser, RangeStream, attempt, between, look_ahead, many, optional,
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
                value(TraitMethod {
                    comments,
                    eol: Eol::default(),
                    doc,
                    attrs,
                    name,
                    typ: ft,
                    self_index,
                    default,
                })
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
            spaces().with(decl_items(move || trait_method(sig), || token('}'), sig)),
        )),
        position(),
    )
        .then(
            |((at, name), (mut methods, below), end): (
                (_, ArcStr),
                (LPooled<Vec<TraitMethod>>, Comments),
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
                value(TraitExpr { name, methods, below }).left()
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
        optional(attempt(spaces().skip(look_ahead(token('{')))).with(between(
            token('{'),
            sptoken('}'),
            // XCR claude for claude: [bug/medium] Impl methods are `let` statements, but
            // this list is `sep_by_tok`, not the block's `statements`. So `let show =
            // ..; // eol` and a `// below` line under the last method are parse errors,
            // while the same lines in a `{ .. }` block parse. CLAUDE.md makes both
            // legal. Parse with `statements(|| token('}'))`, drop the NoOp a trailing
            // `;` leaves before the `let name = ..` check, and have the ImplExpr
            // printer write eol/below as the block printer does. probe:
            // design/review-2026-10-26/repro/t-parser-b-list-comments.sh
            // (t-parser-b-03)
            // 2026-10-26 claude: Impl bodies parse with `statements(|| token('}'))`; a
            // trailing `;`'s empty statement is dropped and the lines below it move to
            // the last method (an empty body holding only comments is refused), and the
            // impl printer is `pretty_items`. The body's `{` is looked for before any
            // space is consumed, so a bodiless impl ending a list leaves the next
            // line's comment below it. Pins: parser/test.rs
            // a_comment_below_the_last_impl_method_rides_it (was
            // trailing_impl_comment_is_error) and
            // a_comment_starting_a_line_stands_below, format.rs
            // comments_after_list_items_stay (impl cases), expr_pp_round_trip
            // (generated impl method eol/below).
            spaces().with(statements(|| token('}'))),
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
                // a trailing `;` leaves an empty statement holding the lines below it
                if let Some(empty) = ms.pop_if(|e| matches!(e.kind, ExprKind::NoOp)) {
                    let below = empty.dec.as_ref().map(|d| &d.below[..]).unwrap_or(&[]);
                    match ms.last_mut() {
                        Some(m) => trail(m, None, below),
                        None if below.is_empty() => (),
                        None => {
                            return refuse(
                                end,
                                "a comment here has no method to describe",
                            )
                            .right();
                        }
                    }
                }
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
