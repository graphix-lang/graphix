use super::{
    csep, expr, fname,
    grow::refusal,
    name, sep_by_tok, spaces, spaces1, sptoken, structure_pattern,
    typexp::{flatten_bounds, tvar_bound, typ},
};
use crate::{
    expr::{
        Arg, ArgKind, Expr, ExprKind, LambdaBody, LambdaExpr, Name, StructurePattern,
        WrittenAt,
    },
    typ::{TVar, Type},
};
use arcstr::{ArcStr, literal};
use combine::{
    ParseError, Parser, RangeStream, attempt, between, choice, not_followed_by, optional,
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
    spaces().with(choice((
        // `#name` alone passes the variable of that name
        // CR claude for claude: [bug/medium] This labeled-argument branch consumes `#`
        // before `fname()` sees the `[` of an attribute. So `f(#[native] e)` fails
        // inside this branch with input already consumed, and the `expr()` alternative,
        // whose `leading_decorations` would take the attribute, is never tried. The
        // postfix loop's `attempt(apply_args())` then gives up quietly, and the error
        // shows at the call's `(` with a misleading "a `;` separates this statement"
        // note. The same attribute works on a labeled argument's value and on an array
        // element, and a `//` comment above a positional argument works too. Fix: guard
        // the branch with `attempt(token('#').skip(not_followed_by(token('['))))`, or
        // equivalent, and add a parser test with an attribute on a positional argument,
        // both alone and after another argument. probe:
        // design/review-2026-10-12/repro/t-parser-a-04.gx (t-parser-a-04)
        token('#').with((position(), fname(), position())).skip(spaces()).then(
            |(pos, name, end)| {
                optional(token(':').with(expr())).map(move |e| match e {
                    Some(e) => (Some(name.clone()), e),
                    None => {
                        let e = ExprKind::Ref { name: [name.clone()].into() };
                        (Some(name.clone()), e.to_expr(pos).ending(end))
                    }
                })
            },
        ),
        expr().map(|e| (None, e)),
    )))
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
            spaces().with(sep_by_tok(applyarg(), csep(), token(')'))),
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

/// One argument between a lambda's bars: whether it is labeled, its
/// pattern, and its annotation and default.
type LambdaArg = ((SourcePosition, (bool, StructurePattern)), Option<Type>, Option<Expr>);

pub(super) fn lambda_args<I>()
-> impl Parser<I, Output = (LPooled<Vec<Arg>>, Option<Option<Type>>)>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    let arg = (
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
    );
    (sep_by_tok(arg, csep(), attempt(sptoken('|'))), position()).and_then(
        |(mut v, end): (LPooled<Vec<LambdaArg>>, _)| {
            let n = v.len();
            let mut args: LPooled<Vec<Arg>> = LPooled::take();
            let mut vargs = None;
            for (i, ((pos, (labeled, pattern)), constraint, default)) in
                v.drain(..).enumerate()
            {
                if !labeled && default.is_some() {
                    return Err(refusal::<I>(
                        end,
                        "only labeled arguments may have a default value",
                    ));
                }
                match &pattern {
                    StructurePattern::Bind(b) if b == "@args" => {
                        if i + 1 < n {
                            return Err(refusal::<I>(
                                end,
                                "@args must be the last argument",
                            ));
                        }
                        vargs = Some(constraint)
                    }
                    _ => args.push(Arg {
                        kind: match (labeled, default) {
                            (false, _) => ArgKind::Positional,
                            (true, None) => ArgKind::Labeled,
                            (true, Some(e)) => ArgKind::Defaulted(e),
                        },
                        pattern,
                        constraint,
                        pos: WrittenAt(pos),
                    }),
                }
            }
            match labeled_first(args.iter().map(|a| a.kind.is_labeled())) {
                true => Ok((args, vargs)),
                false => Err(refusal::<I>(end, LABELED_FIRST)),
            }
        },
    )
}

// CR claude for claude: [bug/medium] `lambda` does not set its own span. It takes
// `position()` before its leading `spaces()` and never calls `.ending`. Only `expr()`
// adds the end afterwards, so a lambda reached through `operand` (`t ~ |x| ..`),
// through `byref_lambda` (`&|x| ..`) or through a seq trigger has no end, and as an
// operand its start sits on whitespace. `shunting_yard`'s `reduce` then copies that
// missing end into the operator node. The LSP scope map records such a lambda as ending
// at NOWHERE, and `Completer::scope_at` takes it as the statement that ended before the
// cursor. As a result, completion on a line before a nested block offers that block's
// locals. Fix: make `lambda` take `spaces().with(position())` and end itself with a
// trailing `position()`. Then add `t ~ |x| x`, `&|x| x` and `seq t ~ |x| x { 1 }` to
// expr_spans.rs's FIXTURE. probe: design/review-2026-10-12/repro/t-parser-a-01.sh
// (t-parser-a-01)
pub(super) fn lambda<I>() -> impl Parser<I, Output = Expr>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (
        position(),
        spaces().with(sep_by_tok(tvar_bound(), csep(), token('|'))).map(
            |mut tvs: LPooled<Vec<(TVar, LPooled<Vec<Type>>)>>| {
                let mut tvs = flatten_bounds(tvs.drain(..));
                Arc::from_iter(tvs.drain(..))
            },
        ),
        between(sptoken('|'), sptoken('|'), lambda_args()),
        optional(attempt(spaces().with(string("->")).with(typ()))),
        optional(attempt(spaces1().with(string("throws")).with(spaces1()).with(typ()))),
        // CR claude for eric: [readability/low] A lambda body requires whitespace after
        // the closing `|`, and `select` requires it after the keyword (as does
        // `until`). `seq(t)`, `catch(e)` and `f(x)` do not. So `|x|x + 1` and
        // `select(x) { .. }` are refused. The first gets a long expected-token list,
        // and the second the misleading "can't use keyword as a function or variable
        // name". The tree-sitter grammar accepts both forms, so the highlighter and the
        // parser disagree. Either accept them (`spaces()` here, and after
        // `select`/`until` once `not_prefix` has matched) or keep the rule, give it a
        // `note_reason` like `separator` has, and make the tree-sitter grammar agree.
        // Probes: design/review-2026-10-12/repro/t-parser-a-lambda-no-space.gx; `let x
        // = select(1) { _ => 2 }`. (t-parser-a-12)
        spaces1().with(choice((
            attempt(token('\'').with(fname()).skip(not_followed_by(sptoken(':'))))
                .map(LambdaBody::Builtin),
            expr().map(LambdaBody::Expr),
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
