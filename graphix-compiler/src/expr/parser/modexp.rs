use super::{
    csep, doc_comment, expr, fname,
    grow::grow,
    leading_comments, modpath, sep_by1_tok, spaces, spname, spstring, sptoken,
    traitexp::{impl_decl, trait_decl},
    typ, typedef, typname,
};
use crate::expr::{
    BindSig, Expr, ExprKind, ModPath, ModuleKind, Name, Sandbox, Sig, SigItem, SigKind,
    UseItem, WrittenPath,
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
use netidx_value::parser::not_prefix;
use poolshark::local::LPooled;
use triomphe::Arc;

parser! {
    pub(super) fn sig_item[I]()(I) -> SigItem
    where [I: RangeStream<Token = char, Position = SourcePosition>, I::Range: Range]
    {
        // Plain `//` lines above an interface declaration are skipped, not
        // retained; only `///` doc comments are captured.
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
                trait_decl().map({
                    let doc = doc.clone();
                    let ori = ori.clone();
                    move |mut e: Expr| match std::mem::replace(&mut e.kind, ExprKind::NoOp) {
                        ExprKind::Trait(t) => SigItem { doc: doc.clone(), kind: SigKind::Trait(t), pos, ori: ori.clone() },
                        _ => unreachable!()
                    }
                }),
                impl_decl().map({
                    let doc = doc.clone();
                    let ori = ori.clone();
                    move |mut e: Expr| match std::mem::replace(&mut e.kind, ExprKind::NoOp) {
                        ExprKind::Impl(i) => SigItem { doc: doc.clone(), kind: SigKind::Impl(i), pos, ori: ori.clone() },
                        _ => unreachable!()
                    }
                }),
                string("val").with(space()).with((spname(), sptoken(':').with(typ())))
                    .map({
                        let doc = doc.clone();
                        let ori = ori.clone();
                        move |(name, typ)| {
                            SigItem { doc: doc.clone(), kind: SigKind::Bind(BindSig { name, typ }), pos, ori: ori.clone() }
                        }
                    }),
                (use_intro(), use_items()).map({
                    let doc = doc.clone();
                    let ori = ori.clone();
                    move |(reexport, names)| SigItem {
                        doc: doc.clone(),
                        kind: SigKind::Use { reexport, names },
                        pos,
                        ori: ori.clone(),
                    }
                }),
                string("mod").with(space()).with(spname().skip(spaces())).map({
                    let doc = doc.clone();
                    let ori = ori.clone();
                    move |n: Name| SigItem { doc: doc.clone(), kind: SigKind::Module(n), pos, ori: ori.clone() }
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
        attempt(string("mod").with(space())).with(spname()),
        optional(dynamic_module())
            .map(|m| m.unwrap_or(ModuleKind::Unresolved { from_interface: false })),
    )
        .map(|(pos, name, value)| ExprKind::Module { name, value }.to_expr(pos))
}

/// A use-tree path segment: an ordinary name or a path keyword
/// (`self`/`super`/`package`); [`check_use_items`] enforces the keywords'
/// positional rules on the assembled path.
fn use_segment<I>() -> impl Parser<I, Output = Name>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (
        position(),
        choice((
            attempt(string("self").skip(not_prefix())).map(|_| arcstr::literal!("self")),
            attempt(string("super").skip(not_prefix()))
                .map(|_| arcstr::literal!("super")),
            attempt(string("package").skip(not_prefix()))
                .map(|_| arcstr::literal!("package")),
            // A use imports every kind sharing the name, so both lowercase
            // and uppercase segments are legal here.
            fname(),
            typname(),
        )),
    )
        .map(|(pos, seg)| Name::written(seg, pos))
}

/// The positional rules for one assembled use path (segments +
/// optional rename). Returns the refusal message, or None if legal.
fn check_use_item(segs: &[Name], rename: &Option<ArcStr>) -> Option<&'static str> {
    if segs.is_empty() {
        return Some("`self` outside a use group");
    }
    let lead = match segs[0].as_str() {
        "self" | "package" => 1,
        "super" => segs.iter().take_while(|s| s.as_str() == "super").count(),
        _ => 0,
    };
    if lead == segs.len() {
        return Some("a use path must name something below self/super/package");
    }
    for (i, s) in segs.iter().enumerate().skip(lead) {
        match s.as_str() {
            "self" | "super" | "package" => {
                return Some("self/super/package are only legal leading a path");
            }
            "*" if i != segs.len() - 1 => {
                return Some("a glob must be the last segment of a use path");
            }
            _ => (),
        }
    }
    if segs.last().map(|s| s.as_str()) == Some("*") && rename.is_some() {
        return Some("a glob import cannot be renamed");
    }
    None
}

parser! {
    /// One element of a use tree, yielding the path suffixes it denotes as
    /// (segment list, rename) pairs: a path, a path ending in a group, a
    /// bare group, a glob leaf, a renamed leaf, or `self` (an empty suffix).
    fn use_tree[I]()(I) -> Vec<(Vec<Name>, Option<ArcStr>)>
    where [I: RangeStream<Token = char, Position = SourcePosition>, I::Range: Range]
    {
        grow(choice((
            between(
                sptoken('{'),
                sptoken('}'),
                spaces().with(sep_by1_tok(use_tree(), csep(), token('}'))),
            )
            .then(|mut groups: LPooled<Vec<Vec<(Vec<Name>, Option<ArcStr>)>>>| {
                let flat: Vec<(Vec<Name>, Option<ArcStr>)> =
                    groups.drain(..).flatten().collect();
                if flat.is_empty() {
                    unexpected_any("empty use group").left()
                } else {
                    value(flat).right()
                }
            }),
            spaces()
                .with((position(), token('*')))
                .map(|(pos, _)| vec![(vec![Name::written(arcstr::literal!("*"), pos)], None)]),
            (
                spaces().with(use_segment()),
                optional(attempt(spstring("::").with(use_tree()))),
                optional(attempt(
                    spaces1()
                        .with(string("as"))
                        .with(spaces1())
                        .with(choice((fname(), typname()))),
                )),
            )
                .then(|(seg, tail, rename): (Name, _, Option<ArcStr>)| {
                    match (tail, rename) {
                        (Some(_), Some(_)) => unexpected_any(
                            "`as` renames a single imported name, not a group",
                        )
                        .left(),
                        (None, rename) if seg.as_str() == "self" => {
                            value(vec![(vec![], rename)]).right()
                        }
                        (None, rename) => value(vec![(vec![seg], rename)]).right(),
                        (Some(sufs), None) => {
                            let sufs: Vec<(Vec<Name>, Option<ArcStr>)> = sufs;
                            value(
                                sufs.into_iter()
                                    .map(|(mut suf, rename)| {
                                        suf.insert(0, seg.clone());
                                        (suf, rename)
                                    })
                                    .collect::<Vec<_>>(),
                            )
                            .right()
                        }
                    }
                }),
        )))
    }
}

/// Parse a full use declaration's items (after the `use` keyword),
/// enforcing the positional rules, and assemble [`UseItem`]s.
fn use_items<I>() -> impl Parser<I, Output = Arc<[UseItem]>>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    use_tree().then(|items: Vec<(Vec<Name>, Option<ArcStr>)>| {
        for (segs, rename) in items.iter() {
            if let Some(msg) = check_use_item(segs, rename) {
                return unexpected_any(msg).left();
            }
        }
        value(UseItem::sorted(items.into_iter().map(|(segs, rename)| UseItem {
            at: WrittenPath(segs.iter().map(|s| s.at.0).collect()),
            path: ModPath(Path::from_iter(segs.iter().map(|s| s.as_str()))),
            rename,
        })))
        .right()
    })
}

/// The `use` / `pub use` introducer. Yields the reexport flag.
fn use_intro<I>() -> impl Parser<I, Output = bool>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    choice((
        attempt(string("pub").skip(spaces1()).skip(string("use")).skip(space()))
            .map(|_| true),
        attempt(string("use").with(space())).map(|_| false),
    ))
}

pub(super) fn use_module<I>() -> impl Parser<I, Output = Expr>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (position(), use_intro(), use_items())
        .map(|(pos, reexport, names)| ExprKind::Use { reexport, names }.to_expr(pos))
}
