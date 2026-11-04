use crate::{
    SourcePosition,
    expr::{
        Attr, BindExpr, CatchExpr, CatchRole, Comment, Comments, Decorations, Doc, Eol,
        Expr, ExprKind, ModPath, Name, Origin, OriginScope, ParserContext, Pattern,
        SelectExpr, SeqExpr, SeqKind, SeqTrigger, Sig, SigItem, StrForm, StructExpr,
        StructWithExpr, TraitMethod, TryWithExpr, WrittenAt, ornament::holds_comment,
    },
    profile::{self, Phase},
    typ::{FnType, Type},
};
use ahash::AHashMap;
use arcstr::{ArcStr, literal};
use compact_str::{CompactString, format_compact};
use netidx_core::path::Path;
use netidx_value::{
    Value,
    parser::{self as value_parser, StringSyntax, duration_of},
};
use poolshark::local::LPooled;
use std::sync::LazyLock;
use triomphe::Arc;

mod arithexp;
mod arrayexp;
mod input;
mod interpolateexp;
mod lambdaexp;
mod modexp;
mod patternexp;
mod state;
mod traitexp;
mod typexp;

#[cfg(test)]
mod test;

pub use state::{DEFAULT_MAX_NESTING, ParseFailure, max_nesting, set_max_nesting};

use input::{Input, Mark, R, choice};
use state::Mode;

/// A list item's parser.
type Item<'t, O> = &'t dyn Fn(&mut Input) -> R<O>;

/// What ends a list, consuming nothing a list reads.
type Term<'t> = &'t dyn Fn(&mut Input) -> R<()>;

/// A terminator ahead, past any whitespace; consumes nothing.
fn ahead(i: &mut Input, term: Term) -> R<()> {
    i.look_ahead(|i| {
        i.attempt(|i| {
            i.spaces();
            term(i)
        })
    })
}

/// Not the middle of a word: no letter, digit or `_` follows.
fn not_prefix(i: &mut Input) -> R<()> {
    i.not_followed_by(|i| i.satisfy(|c| c == '_' || c.is_alphanumeric()))
}

/// The word `w`, whole.
fn keyword(i: &mut Input, w: &'static str) -> R<()> {
    i.attempt(|i| {
        i.string(w)?;
        not_prefix(i)
    })
}

/// One or more `p` separated by `,`, a trailing `,` before `term` allowed,
/// and whether the list ended in its separator.
fn sep_by1_tok_trailing<O>(
    i: &mut Input,
    p: Item<O>,
    term: Term,
) -> R<(LPooled<Vec<O>>, bool)> {
    let start = i.mark();
    let elem =
        |i: &mut Input| choice!(i, |i| ahead(i, term).map(|_| None), |i| p(i).map(Some));
    let mut items: LPooled<Vec<Option<O>>> = LPooled::take();
    items.push(elem(i)?);
    i.many(
        |i| {
            csep(i)?;
            elem(i)
        },
        |o| items.push(o),
    )?;
    match items.first() {
        Some(Some(_)) => {
            let trailing = matches!(items.last(), Some(None));
            let mut res: LPooled<Vec<O>> = LPooled::take();
            res.extend(items.drain(..).flatten());
            Ok((res, trailing))
        }
        _ => Err(i.because_at(start, "expected an item")),
    }
}

fn sep_by1_tok<O>(i: &mut Input, p: Item<O>, term: Term) -> R<LPooled<Vec<O>>> {
    sep_by1_tok_trailing(i, p, term).map(|(items, _)| items)
}

/// None or more `p` separated by `,`, a trailing `,` before `term`
/// allowed.
fn sep_by_tok<O>(i: &mut Input, p: Item<O>, term: Term) -> R<LPooled<Vec<O>>> {
    let elem = |i: &mut Input| {
        choice!(i, |i| i.look_ahead(|i| term(i)).map(|_| None), |i| p(i).map(Some))
    };
    let mut items: LPooled<Vec<O>> = LPooled::take();
    match i.optional(&elem)? {
        None => return Ok(items),
        Some(o) => items.extend(o),
    }
    i.many(
        |i| {
            csep(i)?;
            elem(i)
        },
        |o| items.extend(o),
    )?;
    Ok(items)
}

/// The rest of the line.
fn line_text<'a>(i: &mut Input<'a>) -> &'a str {
    i.take_while(|c| c != '\r' && c != '\n')
}

/// Whitespace, and whether it holds a blank line.
fn spaces_blank(i: &mut Input) -> bool {
    i.take_while(char::is_whitespace).chars().filter(|c| *c == '\n').count() > 1
}

/// `//`, and whether it opens a `///` doc.
fn comment_open(i: &mut Input) -> R<bool> {
    i.opener("//")?;
    i.optional(|i| {
        i.attempt(|i| {
            i.token('/')?;
            i.not_followed_by(|i| i.token('/'))
        })
    })
    .map(|d| d.is_some())
}

// One own-line `//` comment line, text kept verbatim, and whether a blank
// line follows it. `///` is left for `doc_comment`; `docs` says whether one
// is legal here.
fn comment_line(i: &mut Input, docs: bool) -> R<(ArcStr, bool)> {
    let text = i.attempt(|i| {
        let m = i.mark();
        match comment_open(i)? {
            false if !i.starts_line_at(&m) => {
                i.g.note_reason(m.pos, None, || {
                    CompactString::const_new(
                        "a comment stands on a line of its own, above what it \
                         describes, or ends an item's line after its separator",
                    )
                });
                return Err(i.unexpected("trailing comment"));
            }
            true if docs => return Err(i.unexpected("doc comment")),
            true => {
                note_doc_in_gx(i, m.pos);
                return Err(i.unexpected("doc comment"));
            }
            false => (),
        }
        Ok(line_text(i))
    })?;
    let blank = spaces_blank(i);
    Ok((ArcStr::from(text), blank))
}

fn leading_comments(i: &mut Input, docs: bool) -> R<LPooled<Vec<Comment>>> {
    i.spaces();
    let mut lines = CommentLines::default();
    i.many(|i| comment_line(i, docs), |c| lines.extend([c]))?;
    Ok(lines.lines)
}

// `#[name]` or `#[name(arg, ...)]`; the args are full expressions, which
// print on the attribute's line. The `attempt` on `#[` keeps a labeled
// call arg `#name` from colliding.
fn attribute(i: &mut Input) -> R<Attr> {
    i.attempt(|i| i.string("#["))?;
    i.remembered(|i| {
        i.spaces();
        let name = fname(i)?;
        i.spaces();
        let args = i.optional(|i| {
            i.token('(')?;
            let args =
                sep_by_tok(i, &expr, &|i| i.attempt(|i| i.sptoken(')')).map(drop))?;
            i.sptoken(')')?;
            Ok(args)
        })?;
        let end = i.pos();
        i.sptoken(']')?;
        let mut args = args.unwrap_or_else(LPooled::take);
        match args.iter().any(holds_comment) {
            true => Err(i.refuse(end, "a comment cannot stand inside an attribute")),
            false => Ok(Attr { name, args: Arc::from_iter(args.drain(..)) }),
        }
    })
}

// The own-line `//` comments and `#[..]` attributes directly above an
// expression, as two flat lists; their relative interleaving is not kept.
fn leading_decorations(i: &mut Input) -> R<Leading> {
    enum Dec {
        Comment((ArcStr, bool)),
        Attr(Attr),
    }
    i.spaces();
    let mut comments = CommentLines::default();
    let mut attrs: LPooled<Vec<Attr>> = LPooled::take();
    i.many(
        |i| {
            choice!(i, |i| comment_line(i, false).map(Dec::Comment), |i| {
                let a = attribute(i)?;
                i.spaces();
                Ok(Dec::Attr(a))
            })
        },
        |d| match d {
            Dec::Comment(c) => comments.extend([c]),
            Dec::Attr(a) => attrs.push(a),
        },
    )?;
    Ok((comments.lines, attrs))
}

fn doc_comment(i: &mut Input) -> R<Doc> {
    i.spaces();
    let mut doc: Option<String> = None;
    i.many(
        |i| {
            let m = i.mark();
            i.attempt(|i| {
                i.look_ahead(|i| {
                    i.opener("///")?;
                    i.not_followed_by(|i| i.token('/'))
                })
            })?;
            if !i.starts_line_at(&m) {
                i.g.note_reason(m.pos, None, || {
                    CompactString::const_new(
                        "a doc comment stands on a line of its own, above what it describes",
                    )
                });
                return Err(i.unexpected("trailing doc comment"));
            }
            i.opener("///")?;
            let line = line_text(i);
            i.spaces();
            Ok(line)
        },
        |line| match &mut doc {
            None => doc = Some(String::from(line)),
            Some(s) => {
                s.push('\n');
                s.push_str(line)
            }
        },
    )?;
    let pos = i.pos();
    match doc {
        None => Ok(Doc(None)),
        Some(doc) => match i.optional(|i| i.attempt(|i| i.opener("//")))? {
            None => Ok(Doc(Some(ArcStr::from(doc.as_str())))),
            Some(_) => {
                i.g.note_reason(pos, None, || {
                    CompactString::const_new(
                        "a `//` comment goes above the `///` doc lines",
                    )
                });
                Err(i.unexpected("comment below the doc"))
            }
        },
    }
}

/// An identifier: a first char by case, then letters, digits and `_`.
fn ident_str<'a>(i: &mut Input<'a>, cap: bool) -> R<&'a str> {
    let m = i.mark();
    i.take_while1(|c| match cap {
        true => c.is_uppercase(),
        false => is_value_initial(c),
    })?;
    i.take_while(|c| c.is_alphanumeric() || c == '_');
    Ok(i.since(&m))
}

fn ident(i: &mut Input, cap: bool) -> R<ArcStr> {
    ident_str(i, cap).map(ArcStr::from)
}

fn fname(i: &mut Input) -> R<ArcStr> {
    fname_noting(i, true)
}

/// [`fname`]; a refused reserved word is noted only when `note`, since a
/// type path's segment is never a binding and a pattern head probes one.
fn fname_noting(i: &mut Input, note: bool) -> R<ArcStr> {
    let pos = i.pos();
    let s = ident_str(i, false)?;
    if !is_reserved_binding(s) {
        return Ok(ArcStr::from(s));
    }
    // Probing a statement's first token as a name is ordinary parsing;
    // only words that never begin a construct earn a note.
    if note && KEYWORDS.get(s) != Some(&Keyword::Construct) {
        i.g.note_reason(pos, Some(s.chars().count()), || {
            format_compact!("`{s}` is a reserved word and cannot be used as a name")
        });
    }
    Err(i.because("can't use keyword as a function or variable name"))
}

/// A binding name and where it stands.
fn name(i: &mut Input) -> R<Name> {
    let pos = i.pos();
    fname(i).map(|n| Name::written(n, pos))
}

fn spname(i: &mut Input) -> R<Name> {
    i.spaces();
    name(i)
}

fn fldname(i: &mut Input) -> R<ArcStr> {
    ident(i, false)
}

fn typname(i: &mut Input) -> R<ArcStr> {
    let s = ident_str(i, true)?;
    match is_reserved(s) {
        true => Err(i.because("can't use keyword as a type name")),
        false => Ok(ArcStr::from(s)),
    }
}

/// `w::`, attempted, and where it starts.
fn root_segment(i: &mut Input, w: &'static str) -> R<SourcePosition> {
    i.attempt(|i| {
        let pos = i.pos();
        i.string(w)?;
        i.string("::")?;
        Ok(pos)
    })
}

/// A path's optional keyword root: `self::`, `package::`, or a chain of
/// `super::`s. Yields the keyword segments consumed, each where it was
/// written.
fn path_root(i: &mut Input) -> R<LPooled<Vec<Name>>> {
    let mut v: LPooled<Vec<Name>> = LPooled::take();
    for (w, name) in [("package", literal!("package")), ("self", literal!("self"))] {
        if let Some(pos) = i.optional(|i| root_segment(i, w))? {
            v.push(Name::written(name, pos));
            return Ok(v);
        }
    }
    i.many(
        |i| root_segment(i, "super"),
        |pos| v.push(Name::written(literal!("super"), pos)),
    )?;
    Ok(v)
}

/// `p` separated by `::`, at least one.
fn path_segments<'a, O>(
    i: &mut Input<'a>,
    p: impl Fn(&mut Input<'a>) -> R<O>,
) -> R<LPooled<Vec<O>>> {
    let mut v: LPooled<Vec<O>> = LPooled::take();
    v.push(p(i)?);
    i.many(
        |i| {
            i.string("::")?;
            p(i)
        },
        |s| v.push(s),
    )?;
    Ok(v)
}

fn modpath(i: &mut Input) -> R<ModPath> {
    let mut root = path_root(i)?;
    let mut v = path_segments(i, fname)?;
    Ok(ModPath(Path::from_iter(root.drain(..).map(|n| n.name).chain(v.drain(..)))))
}

fn csep(i: &mut Input) -> R<char> {
    separator(i, ',', COMMA_WHY)
}

/// Why a missing `sep` before `c` is the failure, if it is.
fn separator_reason(sep: char, why: &'static str, c: char) -> Option<&'static str> {
    let starts_item = c.is_alphanumeric()
        || matches!(
            c,
            '_' | '"' | '\'' | '`' | '(' | '[' | '{' | '#' | '@' | '&' | '*' | '!'
        );
    match c {
        ';' if sep == ',' => Some("the list is not closed before this `;`"),
        _ if starts_item => Some(why),
        _ => None,
    }
}

/// `sep` between two items. Where what follows an item could start another
/// one, the reason names the missing separator; where a list meets a `;`,
/// it names the list left open; either for a failure right there.
fn separator_tok(i: &mut Input, sep: char, why: &'static str) -> R<char> {
    choice!(i, |i| i.attempt(|i| i.sptoken(sep)), |i| i.attempt(|i| {
        i.spaces();
        let pos = i.pos();
        let c =
            i.look_ahead(|i| i.satisfy(|c| separator_reason(sep, why, c).is_some()))?;
        let why = separator_reason(sep, why, c).expect("satisfied");
        i.g.note_reason(pos, Some(1), || CompactString::const_new(why));
        i.g.note_furthest(pos);
        Err(i.mismatch())
    }))
}

/// [`separator_tok`], then the space after it.
fn separator(i: &mut Input, sep: char, why: &'static str) -> R<char> {
    let c = separator_tok(i, sep, why)?;
    i.spaces();
    Ok(c)
}

/// [`separator`], keeping the comment that ends its line.
fn separator_eol(i: &mut Input, sep: char, why: &'static str) -> R<Option<EolComment>> {
    separator_tok(i, sep, why)?;
    let eol = eol_comment(i)?;
    i.spaces();
    Ok(eol)
}

/// Note why a `///` is refused where no doc can stand.
fn note_doc_in_gx(i: &mut Input, pos: SourcePosition) {
    i.g.note_reason(pos, None, || {
        CompactString::const_new(
            "`///` is a doc comment, legal only in a .gxi interface file; a .gx file \
             comments with `//`",
        )
    })
}

/// A `//` comment ending a line after code, its text kept verbatim. One
/// that starts its line stands below, and a `///` is a doc and ends no
/// line.
fn eol_comment(i: &mut Input) -> R<Option<EolComment>> {
    i.optional(|i| {
        i.attempt(|i| {
            i.take_while(|c| c == ' ' || c == '\t');
            let m = i.mark();
            match comment_open(i)? {
                true => {
                    note_doc_in_gx(i, m.pos);
                    Err(i.unexpected("doc comment"))
                }
                false if i.starts_line_at(&m) => Err(i.unexpected("comment")),
                false => Ok((m.pos, ArcStr::from(line_text(i)))),
            }
        })
    })
}

/// The own-line comments after a list's last item, above what closes it;
/// `docs` where a `///` line is an item's doc, which none follows.
fn below_comments(i: &mut Input, docs: bool) -> R<Below> {
    let at = i.look_ahead(|i| {
        i.spaces();
        Ok(i.pos())
    })?;
    let mut lines = CommentLines::default();
    i.many(
        |i| {
            i.attempt(|i| {
                i.spaces();
                comment_line(i, docs)
            })
        },
        |c| lines.extend([c]),
    )?;
    Ok((at, lines.lines))
}

/// `p` separated by `sep` (a `;` or `,`, see [`separator`]) up to `term`,
/// which is not consumed; a trailing separator leaves an empty slot, as
/// does a list with nothing in it. Each item takes the comment ending
/// its line, after its separator; `docs` as for [`below_comments`].
fn commented<O: Commentable>(
    i: &mut Input,
    p: Item<O>,
    sep: char,
    why: &'static str,
    term: Term,
    docs: bool,
) -> R<Listed<O>> {
    let first = slot(i, p, term, docs)?;
    let rest = list_rest(i, &|i| slot(i, p, term, docs), sep, why, docs)?;
    Ok(rest.after(first))
}

/// A list's place before `term`: an item, or the end with the comments
/// above it.
fn slot<O>(
    i: &mut Input,
    p: Item<O>,
    term: Term,
    docs: bool,
) -> R<(SourcePosition, Slot<O>)> {
    let pos = i.pos();
    let slot = choice!(
        i,
        |i| i.attempt(|i| {
            let below = below_comments(i, docs)?;
            ahead(i, term)?;
            Ok(Slot::End(below))
        }),
        |i| p(i).map(Slot::Item)
    )?;
    Ok((pos, slot))
}

/// What follows a [`commented`] list's first slot: each separator, with
/// the comment ending the line it is on, and the slot after it; then the
/// comment ending the last item's line and the own-line comments below.
fn list_rest<O>(
    i: &mut Input,
    slot: Item<(SourcePosition, Slot<O>)>,
    sep: char,
    why: &'static str,
    docs: bool,
) -> R<ListRest<O>> {
    let mut rest: LPooled<Vec<_>> = LPooled::take();
    i.many(
        |i| {
            let eol = separator_eol(i, sep, why)?;
            Ok((eol, slot(i)?))
        },
        |s| rest.push(s),
    )?;
    let eol = eol_comment(i)?;
    let below = below_comments(i, docs)?;
    Ok(ListRest { rest, eol, below })
}

/// [`commented`] statements: an empty slot is an empty statement, and
/// the last statement keeps the comments below it.
fn statements(i: &mut Input, term: Term) -> R<LPooled<Vec<Expr>>> {
    let mut l = commented(i, &expr, ';', SEMI_WHY, term, false)?;
    let mut res: LPooled<Vec<Expr>> = LPooled::take();
    res.extend(
        l.slots
            .drain(..)
            .map(|(pos, e)| e.unwrap_or_else(|| ExprKind::NoOp.to_expr(pos).ending(pos))),
    );
    if let Some(last) = res.last_mut() {
        trail(last, None, &l.below)
    }
    Ok(res)
}

/// [`commented`] `,` items, read as [`comma_listed`] reads them.
fn comma_items<O: ExprItem>(
    i: &mut Input,
    p: Item<O>,
    term: Term,
    empty: bool,
) -> R<(LPooled<Vec<O>>, bool)> {
    let start = i.mark();
    let l = commented(i, p, ',', COMMA_WHY, term, false)?;
    comma_listed(l, empty).map_err(|why| i.because_at(start, why))
}

/// [`commented`] `;` items of an interface or a trait, which has no empty
/// item, and the comments below the last.
fn decl_items<O: Commentable>(
    i: &mut Input,
    p: Item<O>,
    term: Term,
    docs: bool,
) -> R<(LPooled<Vec<O>>, Comments)> {
    let mut l = commented(i, p, ';', SEMI_WHY, term, docs)?;
    let mut res: LPooled<Vec<O>> = LPooled::take();
    res.extend(l.slots.drain(..).filter_map(|(_, o)| o));
    Ok((res, Comments::of(l.below.drain(..))))
}

/// `( ... )` after any whitespace around what `p` reads.
fn parens<O>(i: &mut Input, p: impl FnOnce(&mut Input) -> R<O>) -> R<O> {
    i.sptoken('(')?;
    let o = p(i)?;
    i.sptoken(')')?;
    Ok(o)
}

/// `< ... >` after any whitespace around what `p` reads.
fn angles<O>(i: &mut Input, p: impl FnOnce(&mut Input) -> R<O>) -> R<O> {
    i.sptoken('<')?;
    let o = p(i)?;
    i.sptoken('>')?;
    Ok(o)
}

/// The arguments of `any(..)` or `never(..)`, none or more.
fn call_args(i: &mut Input) -> R<LPooled<Vec<Expr>>> {
    let term = |i: &mut Input| i.attempt(|i| i.sptoken(')')).map(drop);
    comma_items(i, &expr, &term, true).map(|(args, _)| args)
}

/// `never<T>(args…)` / `never(args…)`: the value that never arrives.
fn never_expr(i: &mut Input) -> R<Expr> {
    let pos = i.pos();
    keyword(i, "never")?;
    let typ = i.optional(|i| i.attempt(|i| angles(i, typexp::typ)))?;
    let mut args = parens(i, call_args)?;
    Ok(ExprKind::Never { typ: typ.map(Arc::new), args: Arc::from_iter(args.drain(..)) }
        .to_expr(pos))
}

/// `live(e)`: in a seq step, `e`'s variables read live.
fn live_expr(i: &mut Input) -> R<Expr> {
    let pos = i.pos();
    keyword(i, "live")?;
    let e = parens(i, expr)?;
    Ok(ExprKind::Live(Arc::new(e)).to_expr(pos))
}

fn any(i: &mut Input) -> R<Expr> {
    let pos = i.pos();
    keyword(i, "any")?;
    i.token('(')?;
    let mut args = call_args(i)?;
    i.sptoken(')')?;
    Ok(ExprKind::Any { args: Arc::from_iter(args.drain(..)) }.to_expr(pos))
}

/// `let [rec] pattern[: type] = value`, with the value read by `value`:
/// the full `expr()` for a binding, the head form for a seq trigger.
fn letbind_with(i: &mut Input, value: impl FnOnce(&mut Input) -> R<Expr>) -> R<BindExpr> {
    i.attempt(|i| {
        i.word_space("let")?;
        Ok(i.spaces())
    })?;
    let rec = i
        .optional(|i| {
            i.attempt(|i| {
                i.word_space("rec")?;
                Ok(i.spaces())
            })
        })?
        .is_some();
    let pattern = patternexp::structure_pattern(i)?;
    if i.optional(|i| i.attempt(|i| i.sptoken('|')))?.is_some() {
        return Err(i.because("or-patterns are only legal in select arms"));
    }
    i.spaces();
    let typ = i.optional(|i| {
        i.token(':')?;
        typexp::typ(i)
    })?;
    i.sptoken('=')?;
    let value = value(i)?;
    Ok(BindExpr { rec, pattern, typ, value })
}

fn letbind(i: &mut Input) -> R<Expr> {
    let pos = i.pos();
    let b = letbind_with(i, expr)?;
    Ok(ExprKind::Bind(Arc::new(b)).to_expr(pos))
}

fn connect(i: &mut Input) -> R<Expr> {
    let pos = i.pos();
    let deref = i.optional(|i| i.token('*'))?.is_some();
    i.spaces();
    let at = i.pos();
    let name = connect_target(i)?;
    let e = expr(i)?;
    Ok(ExprKind::Connect { name, at: WrittenAt(at), value: Arc::new(e), deref }
        .to_expr(pos))
}

/// A connect's target and its `<-`. Every expression is probed as a
/// connect first, so a reserved word is noted as a misused name only
/// once the `<-` shows it is one: `(1, false]` is no misuse.
fn connect_target(i: &mut Input) -> R<ModPath> {
    i.spaces();
    let mut root = path_root(i)?;
    let mut segs = path_segments(i, |i| {
        let pos = i.pos();
        ident_str(i, false).map(|s| (pos, s))
    })?;
    i.spstring("<-")?;
    let arrow = i.pos();
    if let Some((pos, s)) = segs.iter().find(|(_, s)| is_reserved_binding(s)) {
        // explains a failure up to the arrow, where the expression parse
        // of the word stops
        let span = (arrow.line == pos.line)
            .then(|| usize::try_from(arrow.column - pos.column).unwrap_or(0));
        i.g.note_reason(*pos, span, || {
            format_compact!("`{s}` is a reserved word and cannot be used as a name")
        });
        return Err(i.because("can't use keyword as a function or variable name"));
    }
    let path =
        root.drain(..).map(|n| n.name).chain(segs.drain(..).map(|(_, s)| s.into()));
    Ok(ModPath(Path::from_iter(path)))
}

/// A printed datetime: `2024-01-01 00:00:00.5 UTC`.
fn printed_datetime(i: &mut Input) -> R<Value> {
    let m = i.mark();
    i.take_while1(|c: char| c.is_ascii_digit() || c == '-' || c == '+')?;
    i.token(' ')?;
    i.take_while1(|c: char| c.is_ascii_digit() || c == ':' || c == '.')?;
    i.string(" UTC")?;
    chrono::NaiveDateTime::parse_from_str(i.since(&m), "%Y-%m-%d %H:%M:%S%.f UTC")
        .map(|dt| Value::DateTime(Arc::new(dt.and_utc())))
        .map_err(|_| i.because_at(m, "a printed datetime"))
}

fn printed_nonfinite(i: &mut Input) -> R<Value> {
    let typ = i.optional(|i| {
        choice!(i, |i| i.attempt(|i| i.string("f32:")), |i| i.string("f64:"))
    })?;
    let n = choice!(
        i,
        |i| i.string("NaN").map(|_| f64::NAN),
        |i| i.string("inf").map(|_| f64::INFINITY),
        |i| i.string("-inf").map(|_| f64::NEG_INFINITY)
    )?;
    Ok(match typ {
        Some("f32:") => Value::F32(n as f32),
        _ => Value::F64(n),
    })
}

/// An integer too long for an i64, as a float prints.
fn printed_large_integer(i: &mut Input) -> R<Value> {
    let m = i.mark();
    i.optional(|i| i.token('-'))?;
    i.take_while1(|c| c.is_ascii_digit())?;
    let s = i.since(&m);
    i.not_followed_by(|i| i.token('.'))?;
    match (s.parse::<i64>(), s.parse::<f64>()) {
        (Err(_), Ok(n)) if n.is_finite() => Ok(Value::F64(n)),
        _ => Err(i.because_at(m, "a large printed float")),
    }
}

fn printed_scalar(i: &mut Input) -> R<Value> {
    if !i.printed_data {
        return Err(i.fail());
    }
    let v = choice!(
        i,
        |i| i.attempt(printed_datetime),
        |i| i.attempt(|i| {
            i.value_part(value_parser::duration).map(|d| Value::Duration(Arc::new(d)))
        }),
        |i| i.attempt(printed_nonfinite),
        printed_large_integer
    )?;
    not_prefix(i)?;
    Ok(v)
}

fn literal(i: &mut Input) -> R<Expr> {
    choice!(
        i,
        // An integer in front of `..` is a slice bound: the value parser
        // would read `1.` as a float and leave `.n` to be a field access.
        |i| i.attempt(|i| {
            let pos = i.pos();
            let v = arrayexp::idx(i)?;
            i.look_ahead(|i| i.string(".."))?;
            Ok(ExprKind::Constant(v).to_expr(pos))
        }),
        |i| i.attempt(|i| {
            let pos = i.pos();
            printed_scalar(i).map(|v| ExprKind::Constant(v).to_expr(pos))
        }),
        // A quoted string is `interpolated()`'s alone, so its failure is
        // reported inside it rather than past it.
        |i| i.attempt(|i| {
            let start = i.mark();
            let v = i.knot(|i| {
                i.not_followed_by(|i| i.token('"'))?;
                let v = i.value_part(value_parser::value)?;
                not_prefix(i)?;
                Ok(v)
            })?;
            finite(i, start, v).map(|v| ExprKind::Constant(v).to_expr(start.pos))
        }),
        |i| i.attempt(int_range_note),
        |i| i.knot(duration_unit_note)
    )
}

/// The integer types a literal's prefix may name, and whether an integer's
/// text fits each; an unprefixed literal is the first's.
const INT_TYPES: [(&str, fn(&str) -> bool); 12] = [
    ("i64", |s| s.parse::<i64>().is_ok()),
    ("z64", |s| s.parse::<i64>().is_ok()),
    ("i32", |s| s.parse::<i32>().is_ok()),
    ("z32", |s| s.parse::<i32>().is_ok()),
    ("i16", |s| s.parse::<i16>().is_ok()),
    ("i8", |s| s.parse::<i8>().is_ok()),
    ("u64", |s| s.parse::<u64>().is_ok()),
    ("v64", |s| s.parse::<u64>().is_ok()),
    ("u32", |s| s.parse::<u32>().is_ok()),
    ("v32", |s| s.parse::<u32>().is_ok()),
    ("u16", |s| s.parse::<u16>().is_ok()),
    ("u8", |s| s.parse::<u8>().is_ok()),
];

/// A diagnostic arm behind the literal parser: an integer literal out of
/// its type's range names the type, since the value parser fails without
/// a reason.
fn int_range_note<O>(i: &mut Input) -> R<O> {
    let pos = i.pos();
    let typ = i.optional(|i| {
        i.attempt(|i| {
            let t = i.take_while1(|c| c.is_ascii_alphanumeric())?;
            let Some(typ) = INT_TYPES.iter().find(|(n, _)| *n == t) else {
                return Err(i.mismatch());
            };
            i.token(':')?;
            Ok(typ)
        })
    })?;
    let m = i.mark();
    i.optional(|i| i.token('-'))?;
    i.take_while1(|c| c.is_ascii_digit())?;
    let int = i.since(&m);
    i.not_followed_by(|i| i.token('.'))?;
    let (name, fits) = typ.unwrap_or(&INT_TYPES[0]);
    if !fits(int) {
        let len = typ.map_or(0, |(t, _)| t.len() + 1) + int.len();
        i.g.note_reason(pos, Some(len), || {
            format_compact!("`{int}` is out of the range of {name}")
        });
    }
    Err(i.unexpected("integer literal"))
}

/// `v`, unless it is a float literal out of its type's range, which
/// would read as an infinity no literal can write; refused as from
/// `start`.
fn finite(i: &mut Input, start: Mark, v: Value) -> R<Value> {
    fn holds_infinity(v: &Value) -> bool {
        match v {
            Value::F64(f) => !f.is_finite(),
            Value::F32(f) => !f.is_finite(),
            Value::Error(e) => holds_infinity(e),
            Value::Array(a) => a.iter().any(holds_infinity),
            _ => false,
        }
    }
    match holds_infinity(&v) {
        true => Err(i.refuse_from(
            start,
            start.pos,
            "a float literal out of its type's range",
        )),
        false => Ok(v),
    }
}

/// A diagnostic arm behind the literal parser: a `duration:` literal with
/// an unknown unit names the unit, since the value parser accepts the
/// longest known prefix and fails after it, and one no duration holds
/// says so, since the value parser fails without a reason.
fn duration_unit_note<O>(i: &mut Input) -> R<O> {
    i.attempt(|i| i.string("duration:"))?;
    let start = i.pos();
    let n = i.take_while1(|c| c.is_ascii_digit() || c == '.' || c == '-' || c == '+')?;
    let pos = i.pos();
    let unit = i.take_while1(char::is_alphabetic)?;
    if duration_of("0", unit).is_none() {
        i.g.note_reason(pos, None, || {
            format_compact!(
                "`{unit}` is not a duration unit; the units are ns, us (or µs), \
                 ms, s, m, h, d, M and y (`duration:30.m`)"
            )
        });
    } else if n.parse::<f64>().is_ok() && duration_of(n, unit).is_none() {
        i.g.note_reason(start, Some(n.len() + unit.len()), || {
            format_compact!("`{n}{unit}` is negative or longer than the longest duration")
        });
    }
    Err(i.unexpected("duration literal"))
}

/// A value path: `x`, `m::x`, `Trait::m` (an uppercase interior
/// segment names a trait, whose methods are reached like a module's
/// items), or the bare receiver name `self` of an impl method.
fn valpath(i: &mut Input) -> R<ModPath> {
    choice!(
        i,
        |i| i.attempt(|i| {
            i.string("self")?;
            not_prefix(i)?;
            i.not_followed_by(|i| i.string("::"))?;
            Ok(ModPath::from([literal!("self")]))
        }),
        |i| {
            let mut root = path_root(i)?;
            let mut v = path_segments(i, |i| choice!(i, fname, typname))?;
            let terminal_is_value =
                v.last().and_then(|s| s.chars().next()).is_some_and(is_value_initial);
            if !terminal_is_value {
                return Err(i.because("expected a value name"));
            }
            Ok(ModPath(Path::from_iter(
                root.drain(..).map(|n| n.name).chain(v.drain(..)),
            )))
        }
    )
}

fn reference(i: &mut Input) -> R<Expr> {
    let pos = i.pos();
    valpath(i).map(|name| ExprKind::Ref { name }.to_expr(pos))
}

/// Rust-style raw strings: `r"…"`, `r#"…"#`, `r##"…"##`, … No escapes,
/// no interpolation, no newline stripping; the content ends at the first
/// `"` followed by the opener's hash count.
fn raw_string(i: &mut Input) -> R<Expr> {
    let pos = i.pos();
    let n = i.attempt(|i| {
        i.token('r')?;
        let n = i.take_while(|c| c == '#').len();
        i.token('"')?;
        Ok(n)
    })?;
    let m = i.mark();
    while let Some(c) = i.peek() {
        if c == '"' && n <= i.rest()[1..].bytes().take_while(|b| *b == b'#').count() {
            break;
        }
        i.bump();
    }
    let s = i.since(&m);
    i.token('"')?;
    for _ in 0..n {
        i.token('#')?;
    }
    Ok(ExprKind::Constant(Value::String(ArcStr::from(s)))
        .to_expr(pos)
        .written_as(StrForm::Raw))
}

fn until_expr(i: &mut Input) -> R<Expr> {
    let pos = i.pos();
    i.attempt(|i| {
        i.string("until")?;
        not_prefix(i)?;
        Ok(i.spaces())
    })?;
    let e = expr(i)?;
    Ok(ExprKind::Until(Arc::new(e)).to_expr(pos))
}

/// A brace-delimited seq statement list (the body of `seq`, `try`
/// and `with`). Empty positions come back as `NoOp`.
fn seq_stmts(i: &mut Input) -> R<LPooled<Vec<Expr>>> {
    i.sptoken('{')?;
    let stmts = statements(i, &|i| i.token('}').map(drop))?;
    i.sptoken('}')?;
    Ok(stmts)
}

/// `abort(e)` or `flush(e)`, after a `;` unless it opens the head.
/// `flush` is not a reserved word, so a trigger that calls a function of
/// that name is parenthesized.
fn seq_clause(i: &mut Input, name: &'static str, first: bool) -> R<Arc<Expr>> {
    i.attempt(|i| {
        if !first {
            i.sptoken(';')?;
        }
        i.spstring(name)?;
        i.sptoken('(')
    })?;
    let e = expr(i)?;
    i.sptoken(')')?;
    Ok(Arc::new(e))
}

/// A seq's trigger: an operator expression, bare (a `{` ahead is the
/// body), or `let pattern = ` one.
fn seq_trigger(i: &mut Input) -> R<SeqTrigger> {
    i.attempt(|i| {
        i.spaces();
        i.not_followed_by(|i| i.token('{'))?;
        i.not_followed_by(|i| {
            i.attempt(|i| {
                choice!(i, |i| i.string("abort"), |i| i.string("flush"))?;
                i.sptoken('(')
            })
        })?;
        choice!(
            i,
            |i| {
                let b = letbind_with(i, |i| arithexp::arith(i, false))?;
                let pos = i.pos();
                match b.rec {
                    true => Err(i.refuse(
                        pos,
                        "a seq trigger cannot be rec: it has no self to recurse on",
                    )),
                    false => Ok(SeqTrigger::Bind(Arc::new(b))),
                }
            },
            |i| arithexp::arith(i, false).map(|e| SeqTrigger::Expr(Arc::new(e)))
        )
    })
}

/// The head of a seq, in order: `[trigger][; abort(e)][; flush(e)]`.
fn seq_head(
    i: &mut Input,
    queued: bool,
) -> R<(SeqKind, Option<SeqTrigger>, Option<Arc<Expr>>)> {
    let trigger = i.optional(seq_trigger)?;
    let first = trigger.is_none();
    let abort = i.optional(|i| seq_clause(i, "abort", first))?;
    let flush = i.optional(|i| seq_clause(i, "flush", first && abort.is_none()))?;
    let at_flush = i.pos();
    let semi = i.optional(|i| i.attempt(|i| i.sptoken(';')))?;
    let at_semi = i.pos();
    if semi.is_some() {
        Err(i.refuse(at_semi, "a seq head is `[trigger][; abort(..)][; flush(..)]`"))
    } else if flush.is_some() && !queued {
        Err(i.refuse(
            at_flush,
            "flush empties a queue, and a seq has none: write seqq, or abort(..)",
        ))
    } else {
        let kind = if queued { SeqKind::Queued { flush } } else { SeqKind::Plain };
        Ok((kind, trigger, abort))
    }
}

fn seq(i: &mut Input) -> R<Expr> {
    let pos = i.pos();
    let queued = choice!(i, |i| keyword(i, "seqq").map(|_| true), |i| keyword(i, "seq")
        .map(|_| false))?;
    let (kind, trigger, abort) = seq_head(i, queued)?;
    let mut body = seq_stmts(i)?;
    let end = i.pos();
    if body.iter().all(|e| matches!(e.kind, ExprKind::NoOp)) {
        return Err(i.refuse(end, "a seq block must contain at least one step"));
    }
    let body = Arc::from_iter(body.drain(..));
    let seq = SeqExpr { kind, trigger, abort, body };
    Ok(ExprKind::Seq(Arc::new(seq)).to_expr(pos))
}

fn select_arm(i: &mut Input) -> R<(Pattern, Expr)> {
    let dec = leading_decorations(i)?;
    let pat = patternexp::pattern(i)?;
    i.spstring("=>")?;
    let body = expr(i)?;
    Ok((pat, decorate(body, dec)))
}

fn select(i: &mut Input) -> R<Expr> {
    let pos = i.pos();
    keyword(i, "select")?;
    i.spaces();
    let arg = expr(i)?;
    i.sptoken('{')?;
    i.spaces();
    let (mut arms, _) = comma_items(i, &select_arm, &|i| i.token('}').map(drop), false)?;
    i.sptoken('}')?;
    Ok(ExprKind::Select(SelectExpr {
        arg: Arc::new(arg),
        arms: Arc::from_iter(arms.drain(..)),
    })
    .to_expr(pos))
}

fn cast(i: &mut Input) -> R<Expr> {
    let pos = i.pos();
    keyword(i, "cast")?;
    let typ = angles(i, typexp::typ)?;
    let e = parens(i, expr)?;
    Ok(ExprKind::TypeCast { expr: Arc::new(e), typ: Arc::new(typ) }.to_expr(pos))
}

fn struct_field(i: &mut Input) -> R<(ArcStr, Expr)> {
    let dec = leading_decorations(i)?;
    let pos = i.pos();
    let name = fldname(i)?;
    let end = i.pos();
    i.spaces();
    let v = match i.optional(|i| {
        i.token(':')?;
        expr(i)
    })? {
        Some(v) => v,
        // routine: a block's first statement may begin with a keyword
        None if is_reserved_binding(&name) => {
            const WHY: &str =
                "a reserved word field needs the explicit `name: value` form";
            let span = Some(name.chars().count());
            i.g.note_reason(pos, span, || CompactString::const_new(WHY));
            i.g.note_furthest(pos);
            return Err(i.because(WHY));
        }
        None => {
            let name = ModPath::from([name.clone()]);
            ExprKind::Ref { name }.to_expr(pos).ending(end)
        }
    };
    Ok((name, decorate(v, dec)))
}

/// The `name: value, name, ..` field list of a struct literal or a
/// functional update: names unique, sorted by name; decorations above a
/// field attach to its value.
fn struct_fields(i: &mut Input) -> R<LPooled<Vec<(ArcStr, Expr)>>> {
    let start = i.mark();
    let (mut fields, _) =
        comma_items(i, &struct_field, &|i| i.token('}').map(drop), false)?;
    let end = i.pos();
    match sort_unique(&mut fields, |(n, _)| n) {
        true => Ok(fields),
        false => Err(i.refuse_from(start, end, "struct fields must be unique")),
    }
}

/// A struct literal's start: its first field's name and what may follow
/// a field's name.
fn struct_head(i: &mut Input) -> R<()> {
    i.token('{')?;
    leading_decorations(i)?;
    ident_str(i, false)?;
    i.spaces();
    i.many(
        |i| {
            i.attempt(|i| {
                i.opener("//")?;
                i.take_while(|c| c != '\n');
                Ok(i.spaces())
            })
        },
        drop,
    )?;
    i.satisfy(|c| matches!(c, ':' | ',' | '}')).map(drop)
}

fn structure(i: &mut Input) -> R<Expr> {
    let pos = i.pos();
    i.token('{')?;
    i.spaces();
    let mut fields = struct_fields(i)?;
    i.sptoken('}')?;
    Ok(ExprKind::Struct(StructExpr { args: Arc::from_iter(fields.drain(..)) })
        .to_expr(pos))
}

/// What follows a brace's first item.
enum Tail {
    Map(Expr, ListRest<(Expr, Expr)>),
    With(LPooled<Vec<(ArcStr, Expr)>>),
    /// The comment ending the first statement's line, and the rest.
    Block(Option<EolComment>, LPooled<Vec<Expr>>),
    One,
}

fn map_entry(i: &mut Input) -> R<(Expr, Expr)> {
    let k = expr(i)?;
    i.spstring("=>")?;
    Ok((k, expr(i)?))
}

fn brace_tail(i: &mut Input) -> R<Tail> {
    choice!(
        i,
        |i| {
            i.attempt(|i| i.spstring("=>"))?;
            let v = expr(i)?;
            let term = |i: &mut Input| i.attempt(|i| i.sptoken('}')).map(drop);
            let slot = |i: &mut Input| slot(i, &map_entry, &term, false);
            let rest = list_rest(i, &slot, ',', COMMA_WHY, false)?;
            Ok(Tail::Map(v, rest))
        },
        |i| {
            i.attempt(|i| {
                i.spaces1()?;
                i.word_space("with")
            })?;
            struct_fields(i).map(Tail::With)
        },
        |i| {
            let eol = separator_eol(i, ';', SEMI_WHY)?;
            let rest = statements(i, &|i| i.token('}').map(drop))?;
            Ok(Tail::Block(eol, rest))
        },
        |_| Ok(Tail::One)
    )
}

/// The source of a functional update: a name, a `?`/`$` chain on one,
/// or anything parenthesized.
fn with_source(e: &Expr) -> Option<Expr> {
    match &e.kind {
        _ if e.dec.is_some() => None,
        ExprKind::ExplicitParens(e) => Some((**e).clone()),
        _ if chain_on_name(e) => Some(e.clone()),
        _ => None,
    }
}

/// A struct literal, else a form that shares a first item parsed once: the
/// empty map `{}`, a map literal, a functional update `{ s with f: v }`,
/// or a block.
fn brace(i: &mut Input) -> R<Expr> {
    choice!(
        i,
        |i| i.attempt(|i| {
            i.look_ahead(struct_head)?;
            structure(i)
        }),
        |i| {
            let pos = i.pos();
            i.token('{')?;
            i.spaces();
            let body = i.optional(|i| {
                let e = expr(i)?;
                Ok((e, brace_tail(i)?))
            })?;
            let end = i.pos();
            i.sptoken('}')?;
            let kind = match body {
                None => ExprKind::Map { args: Arc::from_iter([]) },
                Some((k, Tail::Map(v, rest))) => {
                    let first = (k.pos, Slot::Item((k, v)));
                    match comma_listed(rest.after(first), false) {
                        Ok((mut args, _)) => {
                            ExprKind::Map { args: Arc::from_iter(args.drain(..)) }
                        }
                        Err(why) => return Err(i.refuse(end, why)),
                    }
                }
                Some((source, Tail::With(mut fields))) => match with_source(&source) {
                    None => {
                        return Err(i.refuse(
                            end,
                            "a functional update's source is a name or parenthesized",
                        ));
                    }
                    Some(source) => ExprKind::StructWith(StructWithExpr {
                        source: Arc::new(source),
                        replace: Arc::from_iter(fields.drain(..)),
                    }),
                },
                Some((mut first, Tail::Block(eol, mut rest))) => {
                    trail(&mut first, eol.map(|(_, eol)| eol), &[]);
                    ExprKind::Block {
                        exprs: Arc::from_iter(
                            std::iter::once(first).chain(rest.drain(..)),
                        ),
                    }
                }
                Some((_, Tail::One)) => {
                    return Err(
                        i.refuse(end, "a block must contain at least 2 expressions")
                    );
                }
            };
            Ok(kind.to_expr(pos))
        }
    )
}

fn variant(i: &mut Input) -> R<Expr> {
    let pos = i.pos();
    i.token('`')?;
    let tag = ident(i, true)?;
    // the spaces after the tag belong to a payload, when one follows
    let args = i.optional(|i| {
        i.attempt(|i| {
            i.spaces();
            i.look_ahead(|i| i.token('('))
        })?;
        i.token('(')?;
        let (args, _) = comma_items(i, &expr, &|i| i.token(')').map(drop), false)?;
        i.sptoken(')')?;
        Ok(args)
    })?;
    let mut args = args.unwrap_or_else(LPooled::take);
    Ok(ExprKind::Variant { tag, args: Arc::from_iter(args.drain(..)) }.to_expr(pos))
}

/// `T(v)` — a constructor call of the abstract type at the capitalized
/// path `T`; the capitalized last segment is what tells it from a call.
fn construct(i: &mut Input) -> R<Expr> {
    let pos = i.pos();
    let (name, _) = i.attempt(|i| {
        let p = typexp::typath(i)?;
        i.sptoken('(')?;
        Ok(p)
    })?;
    let arg = expr(i)?;
    i.sptoken(')')?;
    Ok(ExprKind::Construct { name, arg: Arc::new(arg) }.to_expr(pos))
}

/// `name[: T]` inside parens, a catch's or a with's binder.
fn binder(
    i: &mut Input,
    bind: impl FnOnce(&mut Input) -> R<Name>,
) -> R<(Name, Option<Type>)> {
    i.sptoken('(')?;
    i.spaces();
    let bind = bind(i)?;
    i.spaces();
    let constraint = i.optional(|i| {
        i.token(':')?;
        typexp::typ(i)
    })?;
    i.sptoken(')')?;
    Ok((bind, constraint))
}

fn catch_stmt(i: &mut Input) -> R<Expr> {
    let pos = i.pos();
    keyword(i, "catch")?;
    let (bind, constraint) = binder(i, name)?;
    let handler = expr(i)?;
    Ok(ExprKind::Catch(Arc::new(CatchExpr {
        bind,
        constraint,
        handler: Arc::new(handler),
        role: CatchRole::User,
    }))
    .to_expr(pos))
}

/// A try's or a with's steps, at least one.
fn try_block(i: &mut Input, what: &'static str) -> R<LPooled<Vec<Expr>>> {
    i.spaces();
    let b = seq_stmts(i)?;
    let end = i.pos();
    match b.iter().all(|e| matches!(e.kind, ExprKind::NoOp)) {
        true => Err(i.refuse(end, what)),
        false => Ok(b),
    }
}

/// `try { stmts } with(e[: T]) { stmts }` — a seq statement. Parsed
/// wherever an expression is; the compiler refuses it outside seq
/// statement position.
fn try_with(i: &mut Input) -> R<Expr> {
    let pos = i.pos();
    keyword(i, "try")?;
    let mut body = try_block(i, "a try body must contain at least one step")?;
    i.spaces();
    i.string("with")?;
    not_prefix(i)?;
    let (bind, constraint) = binder(i, |i| {
        choice!(
            i,
            |i| i.attempt(|i| {
                let pos = i.pos();
                i.token('_')?;
                not_prefix(i)?;
                Ok(Name::written(literal!("_"), pos))
            }),
            name
        )
    })?;
    let mut handler = try_block(i, "a with body must contain at least one step")?;
    Ok(ExprKind::TryWith(Arc::new(TryWithExpr {
        body: Arc::from_iter(body.drain(..)),
        bind,
        constraint,
        handler: Arc::from_iter(handler.drain(..)),
    }))
    .to_expr(pos))
}

fn expr(i: &mut Input) -> R<Expr> {
    i.knot(|i| {
        let dec = leading_decorations(i)?;
        let e = choice!(
            i,
            modexp::module,
            modexp::use_module,
            catch_stmt,
            try_with,
            until_expr,
            |i| {
                let pos = i.pos();
                typexp::typedef(i).map(|td| ExprKind::TypeDef(Arc::new(td)).to_expr(pos))
            },
            |i| {
                let pos = i.pos();
                traitexp::trait_decl(i, false)
                    .map(|t| ExprKind::Trait(Arc::new(t)).to_expr(pos))
            },
            |i| {
                let pos = i.pos();
                traitexp::impl_decl(i).map(|d| ExprKind::Impl(Arc::new(d)).to_expr(pos))
            },
            letbind,
            |i| {
                lambdaexp::lambda_ahead(i)?;
                lambdaexp::any_lambda(i)
            },
            |i| i.attempt(connect),
            |i| arithexp::arith(i, true)
        )?;
        let end = i.pos();
        Ok(decorate(e.ending(end), dec))
    })
}

/// Run `p` over all of `text`, trailing whitespace allowed: first for the
/// parse alone, and only when that fails again for the report, against
/// `ori()`. `printed` reads printed data as literals.
fn parse_all<T>(
    text: &str,
    ori: impl FnOnce() -> Arc<Origin>,
    printed: bool,
    p: impl Fn(&mut Input) -> R<T>,
) -> anyhow::Result<T> {
    let text = text.strip_prefix(BOM).unwrap_or(text);
    let mut lf: LPooled<String> = LPooled::take();
    let text = crlf_as_lf(text, &mut lf);
    let _profile = profile::phase(Phase::Parse);
    let run = |mode| {
        let mut i = Input::new(text, mode, printed);
        let r = p(&mut i).and_then(|v| {
            i.spaces();
            i.eof()?;
            Ok(v)
        });
        (r, i.g)
    };
    let fail = match run(Mode::Quick).0 {
        Ok(v) => return Ok(v),
        Err(_) => match run(Mode::Diag) {
            (Ok(v), _) => return Ok(v),
            (Err(e), g) => match g.report_at(e.at.pos) {
                (_, true) => {
                    let g = run(Mode::Labels(e.at.off)).1;
                    g.report(text, e.at.pos, Some(&g.labels))
                }
                (_, false) => g.report(text, e.at.pos, None),
            },
        },
    };
    let pos = fail.pos;
    Err(anyhow::Error::msg(fail).context(ParserContext { ori: ori(), pos }))
}

/// Parse the expressions of a file.
pub fn parse(ori: Origin) -> anyhow::Result<Arc<[Expr]>> {
    let ori = Arc::new(ori);
    let _scope = OriginScope::enter(ori.clone());
    let mut r =
        parse_all(&ori.text, || ori.clone(), false, |i| statements(i, &|i| i.eof()))?;
    Ok(Arc::from_iter(r.drain(..)))
}

/// Parse the items of an interface file, which may have none.
pub fn parse_sig(ori: Origin) -> anyhow::Result<Sig> {
    let ori = Arc::new(ori);
    let _scope = OriginScope::enter(ori.clone());
    let end = |i: &mut Input| {
        i.attempt(|i| {
            i.spaces();
            i.eof()
        })
    };
    let (mut r, below): (LPooled<Vec<SigItem>>, Comments) = parse_all(
        &ori.text,
        || ori.clone(),
        false,
        |i| decl_items(i, &modexp::sig_item, &end, true),
    )?;
    Ok(Sig { toplevel: true, items: Arc::from_iter(r.drain(..)), below })
}

/// Parse one and only one expression.
pub fn parse_one(s: &str) -> anyhow::Result<Expr> {
    parse_all(s, text_origin(s), false, expr)
}

/// Read printed data through the expression grammar, without evaluating names
/// or applications. Nonfinite float names are data only within this read.
pub fn parse_literal_value(s: &str) -> Option<Value> {
    parse_all(s, text_origin(s), true, expr).ok().and_then(|e| e.literal_value())
}

/// Parse one fntype expression
pub fn parse_fn_type(s: &str) -> anyhow::Result<FnType> {
    parse_all(s, text_origin(s), false, typexp::fntype)
}

/// Parse one type expression
pub fn parse_type(s: &str) -> anyhow::Result<Type> {
    parse_all(s, text_origin(s), false, typexp::typ)
}

pub(super) fn parse_path(s: &str) -> anyhow::Result<ModPath> {
    parse_all(s, text_origin(s), false, modpath)
}

pub const GRAPHIX_MUST_ESC: [char; 4] = ['"', '\\', '[', ']'];

/// The strings of graphix source: no `\u{..}`, so a control character
/// with no name prints raw and reads verbatim.
pub static GRAPHIX_STRINGS: LazyLock<StringSyntax> = LazyLock::new(|| {
    const NAMED: [(char, &str); 4] = [('\n', "n"), ('\r', "r"), ('\t', "t"), ('\0', "0")];
    StringSyntax::new('\\', &GRAPHIX_MUST_ESC, &NAMED, None).unwrap()
});

/// How a reserved word may be used.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Keyword {
    /// begins a construct: refusing it as a name is routine and reports
    /// nothing
    Construct,
    /// never a name
    Reserved,
    /// a primitive type name, legal as a binding name: every place it
    /// means a type is disambiguated by position or a following `:`/`as`
    Type,
}

/// `bytes` is not a [`Keyword::Type`]: `let bytes: T = v` is ambiguous
/// with a base64 literal pattern.
static KEYWORDS: LazyLock<AHashMap<&str, Keyword>> = LazyLock::new(|| {
    use Keyword::*;
    let construct = [
        "mod", "let", "select", "type", "fn", "cast", "never", "if", "use", "rec",
        "catch", "try", "pub", "trait", "impl", "seq", "seqq", "until", "live", "mut",
    ];
    // the compiler-known type and bound names among them: a user type of
    // one of those names would be unnameable, or silently the builtin
    let reserved = [
        "true",
        "false",
        "null",
        "bytes",
        "Array",
        "Map",
        "List",
        "any",
        "Any",
        "self",
        "super",
        "package",
        "abort",
        "Error",
        "Abstract",
        "Concrete",
        "Data",
        "Function",
        "Singleton",
        "OneNumber",
        "NoError",
        "Discernible",
        "Ordered",
        "JsonData",
        "TomlData",
    ];
    let typ = [
        "i8", "u8", "i16", "u16", "i32", "u32", "v32", "z32", "i64", "u64", "v64", "z64",
        "f32", "f64", "decimal", "datetime", "duration", "bool", "string",
    ];
    construct
        .map(|w| (w, Construct))
        .into_iter()
        .chain(reserved.map(|w| (w, Reserved)))
        .chain(typ.map(|w| (w, Type)))
        .collect()
});

/// A reserved word: never a type name, and a name only if a type keyword.
pub fn is_reserved(s: &str) -> bool {
    KEYWORDS.contains_key(s)
}

/// A word refused in BINDING positions (`let`, params, labeled args,
/// pattern binds, module/val names): every reserved word but the
/// primitive type names.
pub fn is_reserved_binding(s: &str) -> bool {
    matches!(KEYWORDS.get(s), Some(Keyword::Construct | Keyword::Reserved))
}

/// A letter that can begin a value name: any but an uppercase one, which
/// begins a type name, so a caseless script names values.
fn is_value_initial(c: char) -> bool {
    c.is_alphabetic() && !c.is_uppercase()
}

/// Graphix has no 1-tuple: `(x,)` is refused, not read as `(x)`.
pub(super) const ONE_TUPLE: &str = "tuples must have at least 2 elements";

/// A list's own-line comments as [`comment_line`] reads them: a line is
/// spaced when a blank line follows the one before it, so a blank before
/// the first or after the last is not the list's.
#[derive(Default)]
struct CommentLines {
    lines: LPooled<Vec<Comment>>,
    blank: bool,
}

impl Extend<(ArcStr, bool)> for CommentLines {
    fn extend<T: IntoIterator<Item = (ArcStr, bool)>>(&mut self, read: T) {
        for (text, blank) in read {
            self.lines.push(Comment { spaced: self.blank, text });
            self.blank = blank;
        }
    }
}

/// The comments and attributes `leading_decorations` captured, in
/// source order within each list.
type Leading = (LPooled<Vec<Comment>>, LPooled<Vec<Attr>>);

/// Give `e` the decorations captured directly above it, ahead of any it
/// captured for itself. What sits above a select arm's pattern, an impl
/// method or a struct field name belongs to the expression that follows.
fn decorate(mut e: Expr, (mut comments, mut attrs): Leading) -> Expr {
    if comments.is_empty() && attrs.is_empty() {
        return e;
    }
    let own = e.dec.take().map(|d| (*d).clone()).unwrap_or_default();
    comments.extend(own.comments.iter().cloned());
    attrs.extend(own.attrs.iter().cloned());
    e.dec = Some(Arc::new(Decorations {
        comments: Arc::from_iter(comments.drain(..)),
        attrs: Arc::from_iter(attrs.drain(..)),
        ..own
    }));
    e
}

const COMMA_WHY: &str = "a `,` separates this item from the one before";

const SEMI_WHY: &str = "a `;` separates this statement from the one before";

/// A comment ending a line: where it starts, and its text.
type EolComment = (SourcePosition, ArcStr);

/// Own-line comments, and where the first starts.
type Below = (SourcePosition, LPooled<Vec<Comment>>);

/// A list item that takes the comment ending its line, after its
/// separator.
pub(super) trait Commentable {
    fn take_eol(&mut self, at: SourcePosition, eol: ArcStr);
}

/// A list item whose comments its last expression carries.
pub(super) trait ExprItem {
    fn last_expr(&mut self) -> &mut Expr;
}

impl ExprItem for Expr {
    fn last_expr(&mut self) -> &mut Expr {
        self
    }
}

impl<K> ExprItem for (K, Expr) {
    fn last_expr(&mut self) -> &mut Expr {
        &mut self.1
    }
}

impl<T: ExprItem> Commentable for T {
    fn take_eol(&mut self, _at: SourcePosition, eol: ArcStr) {
        trail(self.last_expr(), Some(eol), &[])
    }
}

impl Commentable for SigItem {
    fn take_eol(&mut self, _at: SourcePosition, eol: ArcStr) {
        self.eol = Eol::new(Some(eol))
    }
}

impl Commentable for TraitMethod {
    fn take_eol(&mut self, _at: SourcePosition, eol: ArcStr) {
        self.eol = Eol::new(Some(eol))
    }
}

/// Give `e` the comment ending its line and the lines below it.
pub(super) fn trail(e: &mut Expr, eol: Option<ArcStr>, below: &[Comment]) {
    if eol.is_none() && below.is_empty() {
        return;
    }
    let mut d = e.dec.take().map(|d| (*d).clone()).unwrap_or_default();
    if eol.is_some() {
        d.eol = eol;
    }
    if !below.is_empty() {
        d.below = d.below.iter().chain(below.iter()).cloned().collect();
    }
    e.dec = Some(Arc::new(d));
}

/// What sits in a list's place: an item, or the end, with the comments
/// above it.
enum Slot<O> {
    Item(O),
    End(Below),
}

/// A list [`commented`] read: each slot with where it starts, `None` for
/// the empty one a trailing separator or an empty list leaves, and the
/// own-line comments below the last item, which the list's kind places.
struct Listed<O> {
    slots: LPooled<Vec<(SourcePosition, Option<O>)>>,
    below: LPooled<Vec<Comment>>,
    /// Where the first line of `below` starts.
    below_at: Option<SourcePosition>,
}

/// What follows a [`commented`] list's first slot: each separator, with
/// the comment ending the line it is on, and the slot after it; then the
/// comment ending the last item's line and the own-line comments below.
struct ListRest<O> {
    rest: LPooled<Vec<(Option<EolComment>, (SourcePosition, Slot<O>))>>,
    eol: Option<EolComment>,
    below: Below,
}

impl<O: Commentable> ListRest<O> {
    /// The list whose first slot is `first`.
    fn after(mut self, first: (SourcePosition, Slot<O>)) -> Listed<O> {
        let mut listed =
            Listed { slots: LPooled::take(), below: LPooled::take(), below_at: None };
        // only an item stands before a separator: the empty slot ends the list
        let take_eol = |slots: &mut LPooled<Vec<(SourcePosition, Option<O>)>>,
                        eol: Option<EolComment>| {
            if let (Some((at, eol)), Some((_, Some(o)))) = (eol, slots.last_mut()) {
                o.take_eol(at, eol)
            }
        };
        let below = |listed: &mut Listed<O>, (at, mut lines): Below| {
            if !lines.is_empty() {
                listed.below_at.get_or_insert(at);
            }
            listed.below.extend(lines.drain(..));
        };
        for (eol, (pos, slot)) in
            std::iter::once((None, first)).chain(self.rest.drain(..))
        {
            take_eol(&mut listed.slots, eol);
            match slot {
                Slot::Item(o) => listed.slots.push((pos, Some(o))),
                Slot::End(lines) => {
                    below(&mut listed, lines);
                    listed.slots.push((pos, None))
                }
            }
        }
        take_eol(&mut listed.slots, self.eol);
        below(&mut listed, self.below);
        listed
    }
}

/// The items of a `,` list and whether it ended in its separator; at
/// least one unless `empty`. The last item keeps the comments below it.
fn comma_listed<O: ExprItem>(
    mut l: Listed<O>,
    empty: bool,
) -> Result<(LPooled<Vec<O>>, bool), &'static str> {
    let trailing = l.slots.len() > 1 && matches!(l.slots.last(), Some((_, None)));
    if !empty && !matches!(l.slots.first(), Some((_, Some(_)))) {
        return Err("expected an item");
    }
    let mut res: LPooled<Vec<O>> = LPooled::take();
    res.extend(l.slots.drain(..).filter_map(|(_, o)| o));
    match res.last_mut() {
        Some(o) => trail(o.last_expr(), None, &l.below),
        None if l.below.is_empty() => (),
        None => return Err("a comment here has no item to describe"),
    }
    Ok((res, trailing))
}

/// Sort `items` by name; false when a name repeats.
fn sort_unique<T>(items: &mut [T], name: impl Fn(&T) -> &str) -> bool {
    items.sort_by(|a, b| name(a).cmp(name(b)));
    items.windows(2).all(|w| name(&w[0]) != name(&w[1]))
}

/// Whether `e` is a name or a `?`/`$` chain on one, which a functional
/// update reads as its source without parentheses.
pub(crate) fn chain_on_name(e: &Expr) -> bool {
    match &e.kind {
        ExprKind::Ref { .. } => true,
        ExprKind::Qop { arg: s, .. } | ExprKind::OrNever(s) => chain_on_name(s),
        _ => false,
    }
}

/// The byte-order mark every parse skips.
const BOM: char = '\u{feff}';

/// `text` with every CRLF line ending read as LF, as rustc reads source,
/// so a file means the same whatever its checkout's line endings. Only a
/// line ending ever moves: lines and columns are unchanged.
// CR claude for claude: [bug/low] Not for "\r\r\n": the lone CR and the CRLF
// are two line endings, but the result holds "\r\n", which `Mark::step` (its
// `after_cr`) and `lines` read as one, so every position after it is a line
// short of what an editor shows (the LSP's squiggles too). `let x = 1;\r\r\nlet
// y = ;` reports line 2, column 9; the break is line 3. Main's combine parser
// (its `Lines` positioner) counted the same. A fix reads the converted text's
// "\r\n" as two endings in `Mark::step` and shows the snippet from the
// original text, whose `lines` agree. (crcrlf-lines-01)
fn crlf_as_lf<'a>(text: &'a str, buf: &'a mut LPooled<String>) -> &'a str {
    if !text.contains("\r\n") {
        return text;
    }
    for (i, line) in text.split("\r\n").enumerate() {
        if i > 0 {
            buf.push('\n');
        }
        buf.push_str(line);
    }
    buf.as_str()
}

/// A text's lines, each without its break (`\r\n`, `\r` or `\n`); what
/// follows the last break, empty or not, is a line.
pub fn lines(text: &str) -> impl Iterator<Item = &str> {
    let mut rest = Some(text);
    std::iter::from_fn(move || {
        let t = rest?;
        Some(match t.find(['\r', '\n']) {
            None => {
                rest = None;
                t
            }
            Some(i) => {
                let brk = if t[i..].starts_with("\r\n") { 2 } else { 1 };
                rest = Some(&t[i + brk..]);
                &t[..i]
            }
        })
    })
}

/// The length of the byte-order mark and then the `#!` line a program
/// text may start with, the line's newline excluded; 0 when there is
/// neither. Every reader of a program skips them.
pub fn shebang_len(text: &str) -> usize {
    let bom = if text.starts_with(BOM) { BOM.len_utf8() } else { 0 };
    let rest = &text[bom..];
    bom + if rest.starts_with("#!") { lines(rest).next().map_or(0, str::len) } else { 0 }
}

/// `text` without its byte-order mark and `#!` line, the newline kept so
/// lines number alike.
pub fn without_shebang(text: &ArcStr) -> ArcStr {
    match shebang_len(text) {
        0 => text.clone(),
        n => ArcStr::from(&text[n..]),
    }
}

fn text_origin(s: &str) -> impl FnOnce() -> Arc<Origin> + '_ {
    move || Arc::new(Origin::unspecified(s))
}
