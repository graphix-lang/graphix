use super::{
    super::{
        COMMA_WHY,
        lambdaexp::{LABELED_FIRST, labeled_first},
        typexp::flatten_bounds,
    },
    arithexp::mutability,
    choice, comma_items, commented, decorate, expr, fname,
    input::{Input, R},
    leading_comments, leading_decorations, name, not_prefix,
    patternexp::structure_pattern,
    sep_by_tok,
    typexp::{tvar_bound, typ},
};
use crate::expr::{
    Arg, ArgKind, Comments, Eol, Expr, ExprKind, ItemComments, LambdaBody, LambdaExpr,
    Name, StructurePattern, WrittenAt,
};
use arcstr::{ArcStr, literal};
use poolshark::local::LPooled;
use triomphe::Arc;

fn applyarg(i: &mut Input) -> R<(Option<ArcStr>, Expr)> {
    let leading = leading_decorations(i)?;
    let (label, e) = choice!(
        i,
        // `#name` alone passes the variable of that name
        // `#[` is an attribute on a positional argument
        |i| {
            i.attempt(|i| {
                i.token('#')?;
                i.not_followed_by(|i| i.token('['))
            })?;
            let pos = i.pos();
            let name = fname(i)?;
            let end = i.pos();
            i.spaces();
            match i.optional(|i| {
                i.token(':')?;
                expr(i)
            })? {
                Some(e) => Ok((Some(name), e)),
                None => {
                    let e = ExprKind::Ref { name: [name.clone()].into() };
                    Ok((Some(name), e.to_expr(pos).ending(end)))
                }
            }
        },
        |i| expr(i).map(|e| (None, e))
    )?;
    Ok((label, decorate(e, leading)))
}

/// The `( args )` of a call: labeled `#name: e` args before anonymous ones,
/// possibly empty.
pub(super) fn apply_args(i: &mut Input) -> R<LPooled<Vec<(Option<ArcStr>, Expr)>>> {
    let start = i.mark();
    i.token('(')?;
    i.spaces();
    let (args, _) = comma_items(i, &applyarg, &|i| i.token(')').map(drop), true)?;
    i.sptoken(')')?;
    let end = i.pos();
    match labeled_first(args.iter().map(|(l, _)| l.is_some())) {
        true => Ok(args),
        false => Err(i.refuse_from(start, end, LABELED_FIRST)),
    }
}

fn lambda_arg(i: &mut Input) -> R<Arg> {
    let start = i.mark();
    let mut comments = leading_comments(i, false)?;
    i.spaces();
    let pos = i.pos();
    let (labeled, pattern) = choice!(
        i,
        |i| {
            let pos = i.pos();
            i.string("@args")?;
            Ok((false, StructurePattern::Bind(Name::written(literal!("@args"), pos))))
        },
        |i| {
            i.token('#')?;
            name(i).map(|b| (true, StructurePattern::Bind(b)))
        },
        |i| {
            let pos = i.pos();
            i.attempt(|i| {
                i.string("self")?;
                not_prefix(i)
            })?;
            Ok((false, StructurePattern::Bind(Name::written(literal!("self"), pos))))
        },
        |i| structure_pattern(i).map(|p| (false, p))
    )?;
    i.spaces();
    let constraint = i.optional(|i| {
        i.token(':')?;
        typ(i)
    })?;
    i.spaces();
    let default = i.optional(|i| {
        i.token('=')?;
        expr(i)
    })?;
    if !labeled && default.is_some() {
        return Err(i.refuse_from(
            start,
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
}

type LambdaArgs =
    (LPooled<Vec<Arg>>, Option<Option<crate::typ::Type>>, ItemComments, Comments);

fn lambda_args(i: &mut Input) -> R<LambdaArgs> {
    let start = i.mark();
    let mut listed =
        commented(i, &lambda_arg, ',', COMMA_WHY, &|i| i.sptoken('|').map(drop), false)?;
    let end = i.pos();
    let mut args: LPooled<Vec<Arg>> = LPooled::take();
    args.extend(listed.slots.drain(..).filter_map(|(_, a)| a));
    let mut vargs = None;
    let mut vargs_comments = ItemComments::default();
    if let Some(n) = args
        .iter()
        .position(|a| matches!(&a.pattern, StructurePattern::Bind(b) if b == "@args"))
    {
        if n + 1 != args.len() {
            return Err(i.refuse_from(start, end, "@args must be the last argument"));
        }
        let arg = args.pop().unwrap();
        vargs = Some(arg.constraint);
        vargs_comments = arg.comments;
    }
    match labeled_first(args.iter().map(|a| a.kind.is_labeled())) {
        true => Ok((args, vargs, vargs_comments, Comments::of(listed.below.drain(..)))),
        false => Err(i.refuse_from(start, end, LABELED_FIRST)),
    }
}

pub(super) fn lambda(i: &mut Input) -> R<Expr> {
    i.spaces();
    let pos = i.pos();
    let mut tvs = sep_by_tok(i, &tvar_bound, &|i| i.token('|').map(drop))?;
    let mut tvs = flatten_bounds(tvs.drain(..));
    let constraints = Arc::from_iter(tvs.drain(..));
    i.sptoken('|')?;
    let (mut args, vargs, vargs_comments, below) = lambda_args(i)?;
    i.sptoken('|')?;
    let rtype = i.optional(|i| {
        i.attempt(|i| {
            i.spstring("->")?;
            typ(i)
        })
    })?;
    let throws = i.optional(|i| {
        i.attempt(|i| {
            i.spaces1()?;
            i.string("throws")?;
            i.spaces1()?;
            typ(i)
        })
    })?;
    i.spaces();
    let body = choice!(
        i,
        |i| i.attempt(|i| {
            i.token('\'')?;
            let name = fname(i)?;
            i.not_followed_by(|i| i.sptoken(':'))?;
            Ok(LambdaBody::Builtin(name))
        }),
        |i| expr(i).map(LambdaBody::Expr)
    )?;
    let end = i.pos();
    let args = Arc::from_iter(args.drain(..));
    Ok(ExprKind::Lambda(Arc::new(LambdaExpr {
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
    .ending(end))
}

/// `&|x| ..` or `&mut |x| ..`: a reference to a lambda.
fn byref_lambda(i: &mut Input) -> R<Expr> {
    let pos = i.pos();
    i.token('&')?;
    let m = mutability(i)?;
    let e = lambda(i)?;
    let end = i.pos();
    Ok(ExprKind::ByRef(m, Arc::new(e)).to_expr(pos).ending(end))
}

/// What starts a lambda, by reference or not, after any whitespace.
pub(super) fn lambda_ahead(i: &mut Input) -> R<()> {
    i.attempt(|i| {
        i.look_ahead(|i| {
            i.spaces();
            i.optional(|i| {
                i.token('&')?;
                mutability(i)
            })?;
            i.spaces();
            choice!(i, |i| i.token('|'), |i| i.token('\''))
        })
    })
    .map(drop)
}

/// A lambda, by reference or not, after any whitespace.
pub(super) fn any_lambda(i: &mut Input) -> R<Expr> {
    i.spaces();
    choice!(i, byref_lambda, lambda)
}
