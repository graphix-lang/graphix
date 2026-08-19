use super::{
    csep, doc_comment, expr, fname, grow::grow, leading_comments, modpath, sep_by1_tok,
    spaces, spfname, spstring, sptoken, typ, typedef,
};
use crate::expr::{
    BindSig, Expr, ExprKind, ModPath, ModuleKind, Sandbox, Sig, SigItem, SigKind,
    parser::{semisep, spaces1},
};
use arcstr::ArcStr;
use combine::{
    ParseError, Parser, RangeStream, attempt, between, choice, optional,
    parser::char::{space, string},
    position,
    stream::{Range, position::SourcePosition},
    token, unexpected_any, value,
};
use netidx_core::path::Path;
use poolshark::local::LPooled;
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
        grow((position(), leading_comments().with(doc_comment()).skip(spaces())).then(|(pos, doc)| {
            let ori = Some(crate::expr::get_origin());
            choice((
                typedef().map({
                    let doc = doc.clone();
                    let ori = ori.clone();
                    move |mut e: Expr| match std::mem::replace(&mut e.kind, ExprKind::NoOp) {
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
                string("use").with(space()).with(use_tree()).then(|names: Vec<Vec<ArcStr>>| {
                    if names.iter().any(|n| n.is_empty()) {
                        unexpected_any("`self` outside a use group").left()
                    } else {
                        value(names).right()
                    }
                }).map({
                    let doc = doc.clone();
                    let ori = ori.clone();
                    move |mut names| SigItem {
                        doc: doc.clone(),
                        kind: SigKind::Use(Arc::from_iter(
                            names.drain(..).map(|n| ModPath(Path::from_iter(n))),
                        )),
                        pos,
                        ori: ori.clone(),
                    }
                }),
                string("mod").with(space()).with(spfname().skip(spaces())).map({
                    let doc = doc.clone();
                    let ori = ori.clone();
                    move |n: ArcStr| SigItem { doc: doc.clone(), kind: SigKind::Module(n), pos, ori: ori.clone() }
                })
            ))
        }))
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
        sep_by1_tok(sig_item(), semisep(), token('}')).map(
            |mut i: LPooled<Vec<SigItem>>| Sig {
                toplevel: false,
                items: Arc::from_iter(i.drain(..)),
            },
        ),
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
            .map(|mut l: LPooled<Vec<ModPath>>| {
                Sandbox::Blacklist(Arc::from_iter(l.drain(..)))
            }),
        spstring("whitelist")
            .with(between(
                sptoken('['),
                sptoken(']'),
                sep_by1_tok(spaces().with(modpath()), csep(), token(']')),
            ))
            .map(|mut l: LPooled<Vec<ModPath>>| {
                Sandbox::Whitelist(Arc::from_iter(l.drain(..)))
            }),
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

parser! {
    /// One element of a use tree, yielding the path SUFFIXES it
    /// denotes as segment lists: a plain path (`a::b`), a path ending
    /// in a group (`a::{b, c::d}` — nesting allowed), a bare group
    /// (`{a, b}` — what the printer emits when several names share no
    /// prefix), or `self` (the enclosing prefix itself — an empty
    /// suffix, rejected at top level where there is no prefix).
    fn use_tree[I]()(I) -> Vec<Vec<ArcStr>>
    where [I: RangeStream<Token = char, Position = SourcePosition>, I::Range: Range]
    {
        grow(choice((
            between(
                sptoken('{'),
                sptoken('}'),
                spaces().with(sep_by1_tok(use_tree(), csep(), token('}'))),
            )
            .then(|mut groups: LPooled<Vec<Vec<Vec<ArcStr>>>>| {
                let flat: Vec<Vec<ArcStr>> = groups.drain(..).flatten().collect();
                if flat.is_empty() {
                    unexpected_any("empty use group").left()
                } else {
                    value(flat).right()
                }
            }),
            (
                spaces().with(fname()),
                optional(attempt(spstring("::").with(use_tree()))),
            )
                .map(|(seg, tail): (ArcStr, _)| match tail {
                    None if &*seg == "self" => vec![vec![]],
                    None => vec![vec![seg]],
                    Some(sufs) => sufs
                        .into_iter()
                        .map(|mut suf| {
                            suf.insert(0, seg.clone());
                            suf
                        })
                        .collect(),
                }),
        )))
    }
}

pub(super) fn use_module<I>() -> impl Parser<I, Output = Expr>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (position(), attempt(string("use").with(space())).with(use_tree()))
        .then(|(pos, names)| {
            if names.iter().any(|n| n.is_empty()) {
                unexpected_any("`self` outside a use group").left()
            } else {
                value((pos, names)).right()
            }
        })
        .map(|(pos, mut names): (_, Vec<Vec<ArcStr>>)| {
            ExprKind::Use {
                names: Arc::from_iter(
                    names.drain(..).map(|n| ModPath(Path::from_iter(n))),
                ),
            }
            .to_expr(pos)
        })
}
