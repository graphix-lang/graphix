use super::{
    csep, doc_comment, expr, fname,
    grow::{grow, refusal},
    leading_comments, modpath, sep_by1_tok, spaces, spname, spstring, sptoken,
    traitexp::{impl_decl, trait_decl},
    typ, typedef, typname,
};
use crate::expr::{
    BindSig, Comments, Expr, ExprKind, ModPath, ModuleKind, Name, Sandbox, Sig, SigItem,
    SigKind, UseItem, WrittenPath, get_origin,
    parser::{semisep, spaces1},
};
use combine::{
    ParseError, Parser, RangeStream, attempt, between, choice, optional,
    parser::char::{space, string},
    position,
    stream::{Range, position::SourcePosition},
    token,
};
use netidx_core::path::Path;
use netidx_value::parser::not_prefix;
use poolshark::local::LPooled;
use triomphe::Arc;

parser! {
    /// One interface item, with the `//` lines and then the `///` doc
    /// lines above it.
    pub(super) fn sig_item[I]()(I) -> SigItem
    where [I: RangeStream<Token = char, Position = SourcePosition>, I::Range: Range]
    {
        grow((
            leading_comments(),
            doc_comment().skip(spaces()),
            position(),
            choice((
                typedef().map(SigKind::TypeDef),
                trait_decl(true).map(|t| SigKind::Trait(Arc::new(t))),
                impl_decl().map(|i| SigKind::Impl(Arc::new(i))),
                string("val")
                    .with(space())
                    .with((spname(), sptoken(':').with(typ())))
                    .map(|(name, typ)| SigKind::Bind(BindSig { name, typ })),
                (use_intro(), use_items())
                    .map(|(reexport, names)| SigKind::Use { reexport, names }),
                string("mod").with(space()).with(spname().skip(spaces())).map(SigKind::Module),
            )),
        )
            .map(|(mut comments, doc, pos, kind)| SigItem {
                comments: Comments::of(comments.drain(..)),
                doc,
                kind,
                pos,
                ori: Some(get_origin()),
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
/// (`self`/`super`/`package`); [`check_use_item`] enforces the keywords'
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

/// The positional rules for one assembled use path. Returns the refusal
/// message, or None if legal.
fn check_use_item(segs: &[Name]) -> Option<&'static str> {
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
    if segs[lead..].iter().any(|s| matches!(s.as_str(), "self" | "super" | "package")) {
        return Some("self/super/package are only legal leading a path");
    }
    None
}

/// The paths of a use tree, each its segments, LAST FIRST, and rename.
type UsePaths = LPooled<Vec<(LPooled<Vec<Name>>, Option<Name>)>>;

parser! {
    /// One element of a use tree, yielding the path suffixes it denotes:
    /// a path, a path ending in a group, a bare group, a glob leaf, a
    /// renamed leaf, or `self` (an empty suffix).
    fn use_tree[I]()(I) -> UsePaths
    where [I: RangeStream<Token = char, Position = SourcePosition>, I::Range: Range]
    {
        let one = |seg: Option<Name>, rename: Option<Name>| {
            let mut paths: UsePaths = LPooled::take();
            let mut path: LPooled<Vec<Name>> = LPooled::take();
            path.extend(seg);
            paths.push((path, rename));
            paths
        };
        grow(choice((
            between(
                sptoken('{'),
                sptoken('}'),
                spaces().with(sep_by1_tok(use_tree(), csep(), token('}'))),
            )
            .map(|mut groups: LPooled<Vec<UsePaths>>| {
                let mut paths: UsePaths = LPooled::take();
                for mut g in groups.drain(..) {
                    paths.extend(g.drain(..));
                }
                paths
            }),
            spaces()
                .with((position(), token('*')))
                .map(move |(pos, _)| one(Some(Name::written(arcstr::literal!("*"), pos)), None)),
            (
                spaces().with(use_segment()),
                optional(attempt(spstring("::").with(use_tree()))),
                optional(attempt(
                    spaces1()
                        .with(string("as"))
                        .with(spaces1())
                        .with((position(), choice((fname(), typname())))),
                )),
                position(),
            )
                .and_then(move |(seg, tail, rename, end): (Name, Option<UsePaths>, _, _)| {
                    let rename = rename.map(|(pos, n)| Name::written(n, pos));
                    match (tail, rename) {
                        (Some(_), Some(_)) => Err(refusal::<I>(
                            end,
                            "`as` renames a single imported name, not a group",
                        )),
                        (None, rename) if seg.as_str() == "self" => Ok(one(None, rename)),
                        (None, rename) => Ok(one(Some(seg), rename)),
                        (Some(mut paths), None) => {
                            for (path, _) in paths.iter_mut() {
                                path.push(seg.clone());
                            }
                            Ok(paths)
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
    (use_tree(), position()).and_then(|(mut paths, end): (UsePaths, _)| {
        for (segs, _) in paths.iter_mut() {
            segs.reverse();
            if let Some(msg) = check_use_item(segs) {
                return Err(refusal::<I>(end, msg));
            }
        }
        Ok(UseItem::sorted(paths.drain(..).map(|(segs, rename)| UseItem {
            at: WrittenPath(segs.iter().map(|s| s.at.0).collect()),
            path: ModPath(Path::from_iter(segs.iter().map(|s| s.as_str()))),
            rename,
        })))
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
