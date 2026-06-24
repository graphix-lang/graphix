use super::{
    csep, doc_comment, expr, leading_comments, modpath, sep_by1_tok, spaces, spfname,
    spmodpath, spstring, sptoken, typ, typedef,
};
use crate::expr::{
    parser::{semisep, spaces1},
    BindSig, Expr, ExprKind, ModPath, ModuleKind, Sandbox, Sig, SigItem, SigKind,
};
use arcstr::ArcStr;
use combine::{
    attempt, between, choice, optional,
    parser::char::{space, string},
    position,
    stream::{position::SourcePosition, Range},
    token, ParseError, Parser, RangeStream,
};
use triomphe::Arc;

parser! {
    pub(super) fn sig_item[I]()(I) -> SigItem
    where [I: RangeStream<Token = char, Position = SourcePosition>, I::Range: Range]
    {
        // Tolerate (skip) plain `//` comment lines above an interface
        // declaration — `///` doc comments are captured by `doc_comment`,
        // and `.gxi` files use `//` for internal notes (e.g. XCRs). Their
        // retention isn't a goal; this restores the pre-change behavior
        // for the interface parser without affecting the `.gx` rule.
        (position(), leading_comments().with(doc_comment()).skip(spaces())).then(|(pos, doc)| {
            let ori = Some(crate::expr::get_origin());
            choice((
                typedef().map({
                    let doc = doc.clone();
                    let ori = ori.clone();
                    move |e| match e.kind {
                        ExprKind::TypeDef(td) => SigItem { doc: doc.clone(), kind: SigKind::TypeDef(td), pos, ori: ori.clone() },
                        _ => unreachable!()
                    }
                }),
                string("val").with(space()).with((spfname(), sptoken(':').with(typ())))
                    .map({
                        let doc = doc.clone();
                        let ori = ori.clone();
                        move |(name, typ)| {
                            SigItem { doc: doc.clone(), kind: SigKind::Bind(BindSig { name, typ }), pos, ori: ori.clone() }
                        }
                    }),
                string("use").with(space()).with(modpath()).map({
                    let doc = doc.clone();
                    let ori = ori.clone();
                    move |path| SigItem { doc: doc.clone(), kind: SigKind::Use(path), pos, ori: ori.clone() }
                }),
                string("mod").with(space()).with(spfname().skip(spaces())).map({
                    let doc = doc.clone();
                    let ori = ori.clone();
                    move |n: ArcStr| SigItem { doc: doc.clone(), kind: SigKind::Module(n), pos, ori: ori.clone() }
                })
            ))
        })
    }
}

fn sig<I>() -> impl Parser<I, Output = Sig>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    spstring("sig").with(spaces()).with(between(
        token('{'),
        sptoken('}'),
        sep_by1_tok(sig_item(), semisep(), token('}'))
            .map(|i: Vec<SigItem>| Sig { toplevel: false, items: Arc::from(i) }),
    ))
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

pub(crate) fn dynamic_module<I>() -> impl Parser<I, Output = ModuleKind>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    attempt(spaces1().with(string("dynamic")))
        .with(between(
            sptoken('{'),
            sptoken('}'),
            (
                spstring("sandbox").with(space()).with(sandbox()).skip(sptoken(';')),
                sig().skip(sptoken(';')),
                spstring("source")
                    .with(space())
                    .with(expr())
                    .skip(spaces())
                    .skip(optional(token(';'))),
            ),
        ))
        .map(|(sandbox, sig, source)| ModuleKind::Dynamic {
            sandbox,
            sig,
            source: Arc::new(source),
        })
}

pub(super) fn module<I>() -> impl Parser<I, Output = Expr>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (
        position(),
        attempt(string("mod").with(space())).with(spfname()),
        optional(dynamic_module())
            .map(|m| m.unwrap_or(ModuleKind::Unresolved { from_interface: false })),
    )
        .map(|(pos, name, value)| ExprKind::Module { name, value }.to_expr(pos))
}

pub(super) fn use_module<I>() -> impl Parser<I, Output = Expr>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (position(), attempt(string("use").with(space())).with(spmodpath()))
        .map(|(pos, name)| ExprKind::Use { name }.to_expr(pos))
}
