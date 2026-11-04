use super::{
    COMMA_WHY, Commentable, choice, commented, decl_items, doc_comment, expr, fname,
    input::{Input, R},
    keyword, leading_comments, modpath, sep_by1_tok, spname,
    traitexp::{impl_sig, trait_decl},
    typexp::{typ, typedef},
    typname,
};
use crate::expr::{
    BindSig, Comment, Comments, Eol, Expr, ExprKind, ModPath, ModuleKind, Name, Sandbox,
    Sig, SigItem, SigKind, UseComment, UseCommentKind, UseComments, UseItem, WrittenPath,
    get_origin,
};
use arcstr::{ArcStr, literal};
use combine::stream::position::SourcePosition;
use netidx_core::path::Path;
use poolshark::local::LPooled;
use triomphe::Arc;

/// `word` and the whitespace char after it.
/// One interface item, with the `//` lines and then the `///` doc lines
/// above it.
pub(super) fn sig_item(i: &mut Input) -> R<SigItem> {
    i.knot(|i| {
        let mut comments = leading_comments(i, true)?;
        let doc = doc_comment(i)?;
        i.spaces();
        let pos = i.pos();
        let kind = choice!(
            i,
            |i| typedef(i).map(SigKind::TypeDef),
            |i| trait_decl(i, true).map(|t| SigKind::Trait(Arc::new(t))),
            |i| impl_sig(i).map(|h| SigKind::Impl(Arc::new(h))),
            |i| {
                i.word_space("val")?;
                let name = spname(i)?;
                i.sptoken(':')?;
                let typ = typ(i)?;
                Ok(SigKind::Bind(BindSig { name, typ }))
            },
            |i| {
                let reexport = use_intro(i)?;
                let names = use_items(i)?;
                Ok(SigKind::Use { reexport, names })
            },
            |i| {
                i.word_space("mod")?;
                let name = spname(i)?;
                i.spaces();
                Ok(SigKind::Module(name))
            }
        )?;
        Ok(SigItem {
            comments: Comments::of(comments.drain(..)),
            eol: Eol::default(),
            doc,
            kind,
            pos,
            ori: get_origin(),
        })
    })
}

fn sig(i: &mut Input) -> R<Sig> {
    i.spstring("sig")?;
    i.spaces();
    i.token('{')?;
    let start = i.mark();
    let (mut items, below) = decl_items(i, &sig_item, &|i| i.token('}').map(drop), true)?;
    if items.is_empty() {
        return Err(i.refuse_from(
            start,
            start.pos,
            "a `sig` declares at least one item",
        ));
    }
    i.sptoken('}')?;
    Ok(Sig { toplevel: false, items: Arc::from_iter(items.drain(..)), below })
}

fn sandbox(i: &mut Input) -> R<Sandbox> {
    i.spaces();
    choice!(
        i,
        |i| i.string("unrestricted").map(|_| Sandbox::Unrestricted),
        |i| {
            i.string("blacklist")?;
            paths(i).map(|mut l| Sandbox::Blacklist(Arc::from_iter(l.drain(..))))
        },
        |i| {
            i.string("whitelist")?;
            paths(i).map(|mut l| Sandbox::Whitelist(Arc::from_iter(l.drain(..))))
        }
    )
}

/// `[a::b, c]`: a sandbox's module paths.
fn paths(i: &mut Input) -> R<LPooled<Vec<ModPath>>> {
    i.sptoken('[')?;
    let paths = sep_by1_tok(
        i,
        &|i| {
            i.spaces();
            modpath(i)
        },
        &|i| i.token(']').map(drop),
    )?;
    i.sptoken(']')?;
    Ok(paths)
}

fn dynamic_module(i: &mut Input) -> R<ModuleKind> {
    i.attempt(|i| {
        i.spaces1()?;
        i.string("dynamic")
    })?;
    i.sptoken('{')?;
    i.spaces();
    i.word_space("sandbox")?;
    let sandbox = sandbox(i)?;
    i.sptoken(';')?;
    let sig = sig(i)?;
    i.sptoken(';')?;
    i.spaces();
    i.word_space("source")?;
    let source = expr(i)?;
    i.spaces();
    i.optional(|i| i.token(';'))?;
    i.sptoken('}')?;
    Ok(ModuleKind::Dynamic { sandbox, sig, source: Arc::new(source) })
}

pub(super) fn module(i: &mut Input) -> R<Expr> {
    let pos = i.pos();
    i.attempt(|i| i.word_space("mod"))?;
    let name = spname(i)?;
    let value = i
        .optional(dynamic_module)?
        .unwrap_or(ModuleKind::Unresolved { from_interface: false });
    Ok(ExprKind::Module { name, value: Arc::new(value) }.to_expr(pos))
}

/// A use-tree path segment: an ordinary name or a path keyword
/// (`self`/`super`/`package`); [`check_use_item`] enforces the keywords'
/// positional rules on the assembled path.
fn use_segment(i: &mut Input) -> R<Name> {
    let pos = i.pos();
    let seg = choice!(
        i,
        |i| keyword(i, "self").map(|_| literal!("self")),
        |i| keyword(i, "super").map(|_| literal!("super")),
        |i| keyword(i, "package").map(|_| literal!("package")),
        // A use imports every kind sharing the name, so both lowercase
        // and uppercase segments are legal here.
        fname,
        typname
    )?;
    Ok(Name::written(seg, pos))
}

/// The paths of one use tree: `seg` alone, renamed or not.
fn one(seg: Option<Name>, rename: Option<Name>) -> UsePaths {
    let mut paths = UsePaths(LPooled::take());
    let mut segments: LPooled<Vec<Name>> = LPooled::take();
    segments.extend(seg);
    paths.0.push(UsePath { segments, rename, comments: LPooled::take() });
    paths
}

fn use_group(i: &mut Input) -> R<UsePaths> {
    let start = i.mark();
    i.token('{')?;
    let mut groups =
        commented(i, &use_tree, ',', COMMA_WHY, &|i| i.token('}').map(drop), false)?;
    i.sptoken('}')?;
    let mut paths = UsePaths(LPooled::take());
    for (_, group) in groups.slots.drain(..) {
        if let Some(mut group) = group {
            paths.0.extend(group.0.drain(..));
        }
    }
    if paths.0.is_empty() {
        return Err(i.because_at(start, "expected a use item"));
    }
    for c in groups.below.drain(..) {
        paths.comment(UseCommentKind::Below, c);
    }
    Ok(paths)
}

fn use_path(i: &mut Input) -> R<UsePaths> {
    let start = i.mark();
    let seg = use_segment(i)?;
    let tail = i.optional(|i| {
        i.attempt(|i| {
            i.spstring("::")?;
            use_tree(i)
        })
    })?;
    let rename = i.optional(|i| {
        i.attempt(|i| {
            i.spaces1()?;
            i.word_space("as")?;
            i.spaces();
            let pos = i.pos();
            let n = choice!(i, fname, typname)?;
            Ok(Name::written(n, pos))
        })
    })?;
    let end = i.pos();
    match (tail, rename) {
        (Some(_), Some(_)) => Err(i.refuse_from(
            start,
            end,
            "`as` renames a single imported name, not a group",
        )),
        (None, rename) if seg.as_str() == "self" => Ok(one(None, rename)),
        (None, rename) => Ok(one(Some(seg), rename)),
        (Some(mut paths), None) => {
            for path in paths.0.iter_mut() {
                path.segments.push(seg.clone());
                for c in path.comments.iter_mut() {
                    c.depth += 1;
                }
            }
            Ok(paths)
        }
    }
}

fn use_tree(i: &mut Input) -> R<UsePaths> {
    i.knot(|i| {
        let mut comments = leading_comments(i, false)?;
        let mut paths = choice!(
            i,
            use_group,
            |i| {
                let pos = i.pos();
                i.token('*')?;
                Ok(one(Some(Name::written(literal!("*"), pos)), None))
            },
            use_path
        )?;
        for c in comments.drain(..) {
            paths.comment(UseCommentKind::Above, c);
        }
        Ok(paths)
    })
}

/// Parse a full use declaration's items (after the `use` keyword),
/// enforcing the positional rules, and assemble [`UseItem`]s.
fn use_items(i: &mut Input) -> R<Arc<[UseItem]>> {
    let start = i.mark();
    let mut paths = use_tree(i)?;
    let end = i.pos();
    for UsePath { segments: segs, rename, .. } in paths.0.iter_mut() {
        segs.reverse();
        if let Some(msg) = check_use_item(segs) {
            return Err(i.refuse_from(start, end, msg));
        }
        if let Some(r) = rename
            && segs.last().is_some_and(|s| capitalized(s) != capitalized(r))
        {
            return Err(i.refuse_from(
                start,
                r.at.0,
                "a rename keeps its name's case: a capitalized name stays capitalized, a lowercase one lowercase",
            ));
        }
    }
    Ok(UseItem::sorted(paths.0.drain(..).map(
        |UsePath { segments: segs, rename, mut comments }| UseItem {
            comments: UseComments::of(comments.drain(..)),
            at: WrittenPath(segs.iter().map(|s| s.at.0).collect()),
            path: ModPath(Path::from_iter(segs.iter().map(|s| s.as_str()))),
            rename,
        },
    )))
}

/// The `use` / `pub use` introducer. Yields the reexport flag.
fn use_intro(i: &mut Input) -> R<bool> {
    choice!(
        i,
        |i| i
            .attempt(|i| {
                i.word_space("pub")?;
                i.spaces();
                i.word_space("use")
            })
            .map(|_| true),
        |i| i.attempt(|i| i.word_space("use")).map(|_| false)
    )
}

pub(super) fn use_module(i: &mut Input) -> R<Expr> {
    let pos = i.pos();
    let reexport = use_intro(i)?;
    let names = use_items(i)?;
    Ok(ExprKind::Use { reexport, names }.to_expr(pos))
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

fn capitalized(n: &Name) -> bool {
    n.as_str().starts_with(char::is_uppercase)
}

struct UsePath {
    pub(super) segments: LPooled<Vec<Name>>,
    pub(super) rename: Option<Name>,
    pub(super) comments: LPooled<Vec<UseComment>>,
}

struct UsePaths(pub(super) LPooled<Vec<UsePath>>);

impl UsePaths {
    fn comment(&mut self, kind: UseCommentKind, line: Comment) {
        let single = self.0.len() == 1;
        if let Some(first) = self.0.first_mut() {
            let depth = if single && kind != UseCommentKind::Below {
                first.segments.len().saturating_sub(1) as u32
            } else {
                0
            };
            first.comments.push(UseComment { depth, kind, line });
        }
    }
}

impl Commentable for UsePaths {
    fn take_eol(&mut self, _at: SourcePosition, eol: ArcStr) {
        self.comment(UseCommentKind::Eol, Comment::line(eol));
    }
}
