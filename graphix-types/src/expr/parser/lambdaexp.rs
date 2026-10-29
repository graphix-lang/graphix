use super::{
    COMMA_WHY, Commentable,
    arithexp::mutability,
    comma_items, commented, csep, decorate, expr, fname,
    grow::refusal,
    leading_comments, leading_decorations, name, sep_by_tok, spaces, spaces1, sptoken,
    structure_pattern,
    typexp::{flatten_bounds, tvar_bound, typ},
};
use crate::{
    expr::{
        Arg, ArgKind, Comments, Eol, Expr, ExprKind, ItemComments, LambdaBody,
        LambdaExpr, Name, StructurePattern, WrittenAt,
    },
    typ::{TVar, Type},
};
use arcstr::{ArcStr, literal};
use combine::{
    ParseError, Parser, RangeStream, attempt, between, choice, look_ahead,
    not_followed_by, one_of, optional,
    parser::char::string,
    position,
    stream::{Range, position::SourcePosition},
    token,
};
use netidx_value::parser::not_prefix;
use poolshark::local::LPooled;
use triomphe::Arc;

/// Whether every labeled item stands before every positional one.
pub(super) fn labeled_first(labeled: impl IntoIterator<Item = bool>) -> bool {
    let mut positional = false;
    labeled.into_iter().all(|labeled| {
        let ok = !(labeled && positional);
        positional |= !labeled;
        ok
    })
}

pub(super) const LABELED_FIRST: &str = "labeled arguments come before positional ones";

fn applyarg<I>() -> impl Parser<I, Output = (Option<ArcStr>, Expr)>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (
        leading_decorations(),
        choice((
            // `#name` alone passes the variable of that name
            // `#[` is an attribute on a positional argument
            attempt(token('#').skip(not_followed_by(token('['))))
                .with((position(), fname(), position()))
                .skip(spaces())
                .then(|(pos, name, end)| {
                    optional(token(':').with(expr())).map(move |e| match e {
                        Some(e) => (Some(name.clone()), e),
                        None => {
                            let e = ExprKind::Ref { name: [name.clone()].into() };
                            (Some(name.clone()), e.to_expr(pos).ending(end))
                        }
                    })
                }),
            expr().map(|e| (None, e)),
        )),
    )
        .map(|(leading, (label, e))| (label, decorate(e, leading)))
}

/// The `( args )` of a call: labeled `#name: e` args before anonymous ones,
/// possibly empty.
pub(super) fn apply_args<I>()
-> impl Parser<I, Output = LPooled<Vec<(Option<ArcStr>, Expr)>>>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (
        between(
            token('('),
            sptoken(')'),
            spaces()
                .with(comma_items(applyarg, || token(')'), true).map(|(args, _)| args)),
        ),
        position(),
    )
        .and_then(|(args, end): (LPooled<Vec<(Option<ArcStr>, Expr)>>, _)| {
            match labeled_first(args.iter().map(|(l, _)| l.is_some())) {
                true => Ok(args),
                false => Err(refusal::<I>(end, LABELED_FIRST)),
            }
        })
}

impl Commentable for Arg {
    fn take_eol(&mut self, eol: ArcStr) {
        self.comments.eol = Eol::new(Some(eol));
    }
}

fn lambda_arg<I>() -> impl Parser<I, Output = Arg>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (
        leading_comments(false),
        spaces().with(position()).and(choice((
            (position(), string("@args")).map(|(pos, s)| {
                (false, StructurePattern::Bind(Name::written(ArcStr::from(s), pos)))
            }),
            token('#').with(name()).map(|b| (true, StructurePattern::Bind(b))),
            (position(), attempt(string("self").skip(not_prefix()))).map(|(pos, _)| {
                (false, StructurePattern::Bind(Name::written(literal!("self"), pos)))
            }),
            structure_pattern().map(|p| (false, p)),
        ))),
        spaces().with(optional(token(':').with(typ()))),
        spaces().with(optional(token('=').with(expr()))),
    )
        .and_then(
            |(mut comments, (pos, (labeled, pattern)), constraint, default)| {
                if !labeled && default.is_some() {
                    return Err(refusal::<I>(
                        pos,
                        "only labeled arguments may have a default value",
                    ));
                }
                Ok(Arg {
                    comments: ItemComments {
                        above: Comments::of(comments.drain(..)),
                        eol: Eol::default(),
                    },
                    kind: match (labeled, default) {
                        (false, _) => ArgKind::Positional,
                        (true, None) => ArgKind::Labeled,
                        (true, Some(e)) => ArgKind::Defaulted(e),
                    },
                    pattern,
                    constraint,
                    pos: WrittenAt(pos),
                })
            },
        )
}

type LambdaArgs = (LPooled<Vec<Arg>>, Option<Option<Type>>, ItemComments, Comments);

fn lambda_args<I>() -> impl Parser<I, Output = LambdaArgs>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (commented(lambda_arg, ',', COMMA_WHY, || sptoken('|'), false), position()).and_then(
        |(mut listed, end)| {
            let mut args: LPooled<Vec<Arg>> = LPooled::take();
            args.extend(listed.slots.drain(..).filter_map(|(_, a)| a));
            let mut vargs = None;
            let mut vargs_comments = ItemComments::default();
            if let Some(i) = args.iter().position(
                |a| matches!(&a.pattern, StructurePattern::Bind(b) if b == "@args"),
            ) {
                if i + 1 != args.len() {
                    return Err(refusal::<I>(end, "@args must be the last argument"));
                }
                let arg = args.pop().unwrap();
                vargs = Some(arg.constraint);
                vargs_comments = arg.comments;
            }
            match labeled_first(args.iter().map(|a| a.kind.is_labeled())) {
                true => Ok((
                    args,
                    vargs,
                    vargs_comments,
                    Comments::of(listed.below.drain(..)),
                )),
                false => Err(refusal::<I>(end, LABELED_FIRST)),
            }
        },
    )
}

pub(super) fn lambda<I>() -> impl Parser<I, Output = Expr>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (
        spaces().with(position()),
        sep_by_tok(tvar_bound(), csep(), token('|')).map(
            |mut tvs: LPooled<Vec<(TVar, LPooled<Vec<Type>>)>>| {
                let mut tvs = flatten_bounds(tvs.drain(..));
                Arc::from_iter(tvs.drain(..))
            },
        ),
        between(sptoken('|'), sptoken('|'), lambda_args()),
        optional(attempt(spaces().with(string("->")).with(typ()))),
        optional(attempt(spaces1().with(string("throws")).with(spaces1()).with(typ()))),
        spaces().with(choice((
            attempt(token('\'').with(fname()).skip(not_followed_by(sptoken(':'))))
                .map(LambdaBody::Builtin),
            expr().map(LambdaBody::Expr),
        ))),
        position(),
    )
        .map(
            |(
                pos,
                constraints,
                (mut args, vargs, vargs_comments, below),
                rtype,
                throws,
                body,
                end,
            )| {
                let args = Arc::from_iter(args.drain(..));
                ExprKind::Lambda(Arc::new(LambdaExpr {
                    vargs_comments,
                    below,
                    args,
                    vargs,
                    rtype,
                    throws,
                    constraints,
                    body,
                }))
                .to_expr(pos)
                .ending(end)
            },
        )
}

/// `&|x| ..` or `&mut |x| ..`: a reference to a lambda.
pub(super) fn byref_lambda<I>() -> impl Parser<I, Output = Expr>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (position(), token('&'), mutability(), lambda(), position()).map(
        |(pos, _, m, expr, end)| {
            ExprKind::ByRef(m, Arc::new(expr)).to_expr(pos).ending(end)
        },
    )
}

/// What starts a lambda, by reference or not, after any whitespace.
pub(super) fn lambda_ahead<I>() -> impl Parser<I, Output = ()>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    attempt(look_ahead(
        spaces()
            .with(optional(token('&').with(mutability())))
            .with(spaces())
            .with(one_of(['|', '\''])),
    ))
    .map(|_| ())
}

/// A lambda, by reference or not, after any whitespace.
pub(super) fn any_lambda<I>() -> impl Parser<I, Output = Expr>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    spaces().with(choice((byref_lambda(), lambda())))
}
