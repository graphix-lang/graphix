use super::{
    csep, doc_comment, expr, modpath, sep_by1_tok, spaces, spfname, spmodpath, spstring,
    sptoken, typ, typedef,
};
use crate::expr::{
    BindSig, Expr, ExprKind, ModPath, ModSig, ModuleKind, Sandbox, Sig, SigItem, SigKind,
};
use arcstr::ArcStr;
use combine::{
    attempt, between, choice, look_ahead, optional,
    parser::char::{space, string},
    position, sep_by,
    stream::{position::SourcePosition, Range},
    ParseError, Parser, RangeStream,
};
use poolshark::local::LPooled;
use triomphe::Arc;

parser! {
    pub(super) fn sig_item[I]()(I) -> SigItem
    where [I: RangeStream<Token = char, Position = SourcePosition>, I::Range: Range]
    {
        doc_comment().skip(spaces()).then(|doc| {
            choice((
                typedef().map(|e| match e.kind {
                    ExprKind::TypeDef(td) => SigItem { doc, kind: SigKind::TypeDef(td) },
                    _ => unreachable!()
                }),
                string("val").with(space()).with((spfname(), sptoken(':').with(typ())))
                    .map(|(name, typ)| {
                        SigItem { doc, kind: SigKind::Bind(BindSig { name, typ }) }
                    }),
                string("mod").with(space()).with((
                    spfname().skip(sptoken(':')).skip(spstring("sig")),
                    between(
                        sptoken('{'),
                        sptoken('}'),
                        sep_by1_tok(
                            sig_item(),
                            attempt(sptoken(';')),
                            sptoken('}')
                        )
                    )
                )).map(|(name, mut items): (ArcStr, LPooled<Vec<Option<SigItem>>>)| {
                    let items = Arc::from_iter(items.drain(..).filter_map(|e| e));
                    let sig = Sig { items, toplevel: false };
                    SigItem { doc, kind: SigKind::Module(ModSig { name, sig }) }
                })
            ))
        })
    }
}

parser! {
    fn sandbox[I]()(I) -> Sandbox
    where [I: RangeStream<Token = char, Position = SourcePosition>, I::Range: Range]
    {
        choice((
            spstring("unrestricted").map(|_| Sandbox::Unrestricted),
            spstring("blacklist").with(between(
                sptoken('['), sptoken(']'),
                sep_by1_tok(spaces().with(modpath()), csep(), sptoken(']'))
            )).map(|l: Vec<ModPath>| Sandbox::Blacklist(Arc::from(l))),
            spstring("whitelist").with(between(
                sptoken('['), sptoken(']'),
                sep_by1_tok(spaces().with(modpath()), csep(), sptoken(']'))
            )).map(|l: Vec<ModPath>| Sandbox::Whitelist(Arc::from(l)))
        ))
    }
}

parser! {
    pub(super) fn dynamic_module[I]()(I) -> ModuleKind
    where [I: RangeStream<Token = char, Position = SourcePosition>, I::Range: Range]
    {
        space().with(spstring("dynamic")).with(between(
            sptoken('{'), sptoken('}'),
            (
                spstring("sandbox").with(space()).with(sandbox()),
                spstring("sig").with(between(
                    sptoken('{'), sptoken('}'),
                    sep_by1_tok(sig_item(), attempt(sptoken(';')), sptoken('}'))
                        .map(|i: Vec<SigItem>| Sig { toplevel: true, items: Arc::from(i) })
                )),
                spstring("source").with(space()).with(expr())
            )
        )).map(|(sandbox, sig, source)| {
            ModuleKind::Dynamic { sandbox, sig, source: Arc::new(source) }
        })
    }
}

parser! {
    fn inline_module[I]()(I) -> ModuleKind
    where [I: RangeStream<Token = char, Position = SourcePosition>, I::Range: Range]
    {
        between(
            sptoken('{'), sptoken('}'),
            sep_by(
                (
                    position(),
                    choice((
                        look_ahead(sptoken('}')).map(None),
                        expr().map(Some)
                    ))
                ),
                attempt(sptoken(';'))
            )
        ).map(|m: LPooled<Vec<(SourcePosition, Option<Expr>)>>| {
            let exprs = Arc::from_iter(m.drain(..).map(|(pos, e)| match e {
                Some(e) => e,
                None => ExprKind::NoOp.to_expr(pos)
            }));
            ModuleKind::Inline(exprs)
        })
    }
}

parser! {
    pub(super) fn module[I]()(I) -> Expr
    where [I: RangeStream<Token = char, Position = SourcePosition>, I::Range: Range]
    {
        (
            position(),
            string("mod").with(space()).with(spfname()),
            optional(choice((
                attempt(inline_module()),
                attempt(dynamic_module())
            ))).map(|m| m.unwrap_or(ModuleKind::Unresolved))
        )
            .map(|(pos, name, value)| {
                ExprKind::Module { name, value }.to_expr(pos)
            })
    }
}

pub(super) fn use_module<I>() -> impl Parser<I, Output = Expr>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (position(), string("use").with(space()).with(spmodpath()))
        .map(|(pos, name)| ExprKind::Use { name }.to_expr(pos))
}
