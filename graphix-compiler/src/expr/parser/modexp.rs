use super::{
    csep, doc_comment, expr, modpath, sep_by1_tok, spaces, spfname, spmodpath, spstring,
    sptoken, typ, typedef,
};
use crate::expr::{
    parser::{semisep, sep_by1_tok_exp, spaces1},
    BindSig, Expr, ExprKind, ModPath, ModSig, ModuleKind, Sandbox, Sig, SigItem, SigKind,
};
use arcstr::ArcStr;
use combine::{
    attempt, between, choice, optional,
    parser::char::{space, string},
    position,
    stream::{position::SourcePosition, Range},
    token, ParseError, Parser, RangeStream,
};
use poolshark::local::LPooled;
use triomphe::Arc;

parser! {
    pub(super) fn sig_item[I]()(I) -> SigItem
    where [I: RangeStream<Token = char, Position = SourcePosition>, I::Range: Range]
    {
        doc_comment().skip(spaces()).then(|doc| {
            choice((
                typedef().map({
                    let doc = doc.clone();
                    move |e| match e.kind {
                        ExprKind::TypeDef(td) => SigItem { doc: doc.clone(), kind: SigKind::TypeDef(td) },
                        _ => unreachable!()
                    }
                }),
                string("val").with(space()).with((spfname(), sptoken(':').with(typ())))
                    .map({
                        let doc = doc.clone();
                        move |(name, typ)| {
                            SigItem { doc: doc.clone(), kind: SigKind::Bind(BindSig { name, typ }) }
                        }
                    }),
                string("mod").with(space()).with((
                    spfname().skip(sptoken(':')).skip(spstring("sig")),
                    spaces().with(between(
                        token('{'),
                        sptoken('}'),
                        sep_by1_tok(sig_item(), semisep(), token('}'))
                    ))
                )).map({
                    let doc = doc.clone();
                    move |(name, mut items): (ArcStr, LPooled<Vec<SigItem>>)| {
                        let items = Arc::from_iter(items.drain(..));
                        let sig = Sig { items, toplevel: false };
                        SigItem { doc: doc.clone(), kind: SigKind::Module(ModSig { name, sig }) }
                    }
                })
            ))
        })
    }
}

fn sandbox<I>() -> impl Parser<I, Output = Sandbox>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    choice((
        spstring("unrestricted").map(|_| Sandbox::Unrestricted),
        spstring("blacklist")
            .with(between(
                sptoken('['),
                sptoken(']'),
                sep_by1_tok(spaces().with(modpath()), csep(), token(']')),
            ))
            .map(|l: Vec<ModPath>| Sandbox::Blacklist(Arc::from(l))),
        spstring("whitelist")
            .with(between(
                sptoken('['),
                sptoken(']'),
                sep_by1_tok(spaces().with(modpath()), csep(), token(']')),
            ))
            .map(|l: Vec<ModPath>| Sandbox::Whitelist(Arc::from(l))),
    ))
}

fn dynamic_module<I>() -> impl Parser<I, Output = ModuleKind>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    spaces1()
        .with(string("dynamic"))
        .with(between(
            sptoken('{'),
            sptoken('}'),
            (
                spstring("sandbox").with(space()).with(sandbox()),
                spstring("sig").with(spaces()).with(between(
                    token('{'),
                    sptoken('}'),
                    sep_by1_tok(sig_item(), semisep(), token('}')).map(
                        |i: Vec<SigItem>| Sig { toplevel: true, items: Arc::from(i) },
                    ),
                )),
                spstring("source").with(space()).with(expr()),
            ),
        ))
        .map(|(sandbox, sig, source)| ModuleKind::Dynamic {
            sandbox,
            sig,
            source: Arc::new(source),
        })
}

fn inline_module<I>() -> impl Parser<I, Output = ModuleKind>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    between(
        token('{'),
        sptoken('}'),
        sep_by1_tok_exp(expr(), semisep(), token('}'), |pos| {
            ExprKind::NoOp.to_expr(pos)
        }),
    )
    .map(|mut m: LPooled<Vec<Expr>>| ModuleKind::Inline(Arc::from_iter(m.drain(..))))
}

pub(super) fn module<I>() -> impl Parser<I, Output = Expr>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (
        position(),
        attempt(string("mod")).with(space()).with(spfname()),
        spaces()
            .with(optional(choice((inline_module(), dynamic_module()))))
            .map(|m| m.unwrap_or(ModuleKind::Unresolved)),
    )
        .map(|(pos, name, value)| ExprKind::Module { name, value }.to_expr(pos))
}

pub(super) fn use_module<I>() -> impl Parser<I, Output = Expr>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (position(), attempt(string("use")).with(space()).with(spmodpath()))
        .map(|(pos, name)| ExprKind::Use { name }.to_expr(pos))
}
