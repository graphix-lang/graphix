use super::{
    COMMA_WHY, Commentable, Listed, ONE_TUPLE, angles,
    arithexp::mutability,
    choice, commented, fldname, fname, fname_noting, ident_str,
    input::{Input, R},
    keyword,
    lambdaexp::{LABELED_FIRST, labeled_first},
    leading_comments, not_prefix, path_root, path_segments, sep_by_tok, sep_by1_tok,
    sep_by1_tok_trailing, sort_unique, typname,
};
use crate::{
    SourcePosition,
    expr::{
        Comment, Comments, Eol, ItemComments, ModPath, Name, TypeDefBody, TypeDefExpr,
        TypeNote, TypeNotes, WrittenAt, WrittenPath, get_origin, noted_items,
    },
    typ::{
        Bound, FnArgKind, FnArgType, FnType, TVar, Type, TypeRef, check_bounds_acyclic,
    },
};
use arcstr::{ArcStr, literal};
use compact_str::{CompactString, format_compact};
use netidx_core::utils::Either;
use netidx_value::Typ;
use poolshark::local::LPooled;
use std::str::FromStr;
use triomphe::Arc;

pub(super) fn typath(i: &mut Input) -> R<(ModPath, WrittenPath)> {
    i.spaces();
    let mut root = path_root(i)?;
    // a type path is written as a value path is: no space inside it
    let mut parts = path_segments(i, |i| {
        let pos = i.pos();
        let n = choice!(i, |i| fname_noting(i, false), typname)?;
        Ok(Name::written(n, pos))
    })?;
    let capitalized =
        parts.last().and_then(|p| p.name.chars().next()).is_some_and(char::is_uppercase);
    if !capitalized {
        return Err(i.because("type names must be capitalized"));
    }
    root.extend(parts.drain(..));
    let at = WrittenPath(root.iter().filter_map(|n| n.at.get()).collect());
    Ok((ModPath::from(root.drain(..).map(|n| n.name)), at))
}

/// A primitive type's name. `map` and `abstract` name no type here.
fn typeprim(i: &mut Input) -> R<Typ> {
    let s = ident_str(i, false)?;
    match Typ::from_str(s) {
        Ok(t) if t.name() == s && !matches!(t, Typ::Map | Typ::Abstract) => Ok(t),
        _ => Err(i.unexpected("a primitive type")),
    }
}

/// The predicates a bound may name, by keyword.
fn predicate(word: &str) -> Option<Type> {
    Some(match word {
        "Concrete" => Type::Bound(Bound::Concrete),
        "Data" => Type::Bound(Bound::Data),
        "Function" => Type::Bound(Bound::Function),
        "Singleton" => Type::Bound(Bound::Singleton),
        "OneNumber" => Type::Bound(Bound::OneNumber),
        "NoError" => Type::Bound(Bound::NoError),
        "Discernible" => Type::Bound(Bound::Discernible),
        "Ordered" => Type::Bound(Bound::Ordered),
        "JsonData" => Type::Bound(Bound::JsonData),
        "TomlData" => Type::Bound(Bound::TomlData),
        _ => return None,
    })
}

/// A type variable's bound: one type, or a `+`-joined conjunction of
/// traits (`'a: Read + Write`), one conjunct per member.
fn bound(i: &mut Input) -> R<LPooled<Vec<Type>>> {
    let conjunct = |i: &mut Input| {
        choice!(
            i,
            |i| i.attempt(|i| {
                i.spaces();
                let w = i.take_while1(|c| c.is_ascii_alphabetic())?;
                not_prefix(i)?;
                predicate(w).ok_or_else(|| i.because("not a predicate"))
            }),
            typ
        )
    };
    let mut v: LPooled<Vec<Type>> = LPooled::take();
    v.push(conjunct(i)?);
    i.many(
        |i| {
            i.attempt(|i| i.sptoken('+'))?;
            conjunct(i)
        },
        |t| v.push(t),
    )?;
    Ok(v)
}

/// `'a: A + B`: a type variable and its bound.
pub(super) fn tvar_bound(i: &mut Input) -> R<(TVar, LPooled<Vec<Type>>)> {
    i.spaces();
    let tv = tvar(i)?;
    i.sptoken(':')?;
    Ok((tv, bound(i)?))
}

/// `'a`, or `'a: A + B`.
pub(super) fn tvar_opt_bound(i: &mut Input) -> R<(TVar, Option<LPooled<Vec<Type>>>)> {
    i.spaces();
    let tv = tvar(i)?;
    let b = i.optional(|i| {
        i.attempt(|i| i.sptoken(':'))?;
        bound(i)
    })?;
    Ok((tv, b))
}

/// `< items >` of one or more, none of them empty.
pub(super) fn angle_list<O>(
    i: &mut Input,
    item: &dyn Fn(&mut Input) -> R<O>,
) -> R<LPooled<Vec<O>>> {
    i.token('<')?;
    let items = sep_by1_tok(i, item, &|i| i.token('>').map(drop))?;
    i.sptoken('>')?;
    Ok(items)
}

fn fnconstraints(i: &mut Input) -> R<LPooled<Vec<(TVar, Type)>>> {
    i.spaces();
    Ok(match i.optional(|i| angle_list(i, &tvar_bound))? {
        Some(mut cs) => flatten_bounds(cs.drain(..)),
        None => LPooled::take(),
    })
}

fn fnlabeled(i: &mut Input) -> R<FnArgType> {
    let optional =
        choice!(i, |i| i.string("?#").map(|_| true), |i| i.string("#").map(|_| false))?;
    let name = fname(i)?;
    i.sptoken(':')?;
    let typ = typ(i)?;
    Ok(FnArgType {
        kind: FnArgKind::Labeled { name: name.into(), has_default: optional },
        typ,
    })
}

fn fnpositional(i: &mut Input) -> R<FnArgType> {
    choice!(
        i,
        |i| {
            i.attempt(|i| {
                i.string("self")?;
                not_prefix(i)?;
                i.not_followed_by(|i| i.sptoken(':'))
            })?;
            let arg = i.optional(|i| i.attempt(|i| angles(i, typ)))?;
            Ok(FnArgType {
                kind: FnArgKind::Positional { name: Some(literal!("self")) },
                typ: match arg {
                    None => self_tvar(),
                    Some(arg) => Type::App(Arc::new(self_tvar()), Arc::new(arg)),
                },
            })
        },
        |i| {
            let name = fname(i)?;
            i.sptoken(':')?;
            let typ = typ(i)?;
            Ok(FnArgType { kind: FnArgKind::Positional { name: Some(name.into()) }, typ })
        }
    )
}

fn fnarg(i: &mut Input) -> R<Either<FnArgType, Type>> {
    i.spaces();
    choice!(
        i,
        |i| {
            i.string("@args:")?;
            typ(i).map(Either::Right)
        },
        |i| fnlabeled(i).map(Either::Left),
        |i| fnpositional(i).map(Either::Left)
    )
}

fn fnargs(i: &mut Input) -> R<LPooled<Vec<Either<FnArgType, Type>>>> {
    i.spaces();
    i.token('(')?;
    let args = sep_by_tok(i, &fnarg, &|i| i.attempt(|i| i.sptoken(')')).map(drop))?;
    i.sptoken(')')?;
    Ok(args)
}

pub(super) fn fntype(i: &mut Input) -> R<FnType> {
    keyword(i, "fn")?;
    let constraints = fnconstraints(i)?;
    let mut args = fnargs(i)?;
    i.spstring("->")?;
    let rtype = typ(i)?;
    let throws = i.optional(|i| {
        i.attempt(|i| {
            i.spaces1()?;
            i.string("throws")
        })?;
        i.word_end()?;
        i.spaces();
        typ(i)
    })?;
    let end = i.pos();
    let vargs = match args.pop() {
        None => None,
        Some(Either::Right(t)) => Some(t),
        Some(Either::Left(t)) => {
            args.push(Either::Left(t));
            None
        }
    };
    if !args.iter().all(|a| a.is_left()) {
        return Err(i.because("vargs must appear once at the end of the args"));
    }
    let args = Arc::from_iter(args.drain(..).map(|t| match t {
        Either::Left(t) => t,
        Either::Right(_) => unreachable!(),
    }));
    let named_before = |(n, a): (usize, &FnArgType)| {
        a.name().is_some_and(|name| args[..n].iter().any(|b| b.name() == Some(name)))
    };
    if args.iter().enumerate().any(named_before) {
        const UNIQUE: &str = "arguments must have unique names";
        i.g.note_reason(end, None, || CompactString::const_new(UNIQUE));
        return Err(i.because(UNIQUE));
    }
    if !labeled_first(args.iter().map(|a| a.is_labeled())) {
        return Err(i.because(LABELED_FIRST));
    }
    let explicit_throws = throws.is_some();
    let throws = throws.unwrap_or(Type::Bottom);
    let ft = FnType { args, vargs, rtype, throws, explicit_throws, ..Default::default() };
    let refuse = |i: &mut Input, e: anyhow::Error, why| {
        i.g.note_reason(end, None, || format_compact!("{e}"));
        i.because(why)
    };
    if let Err(e) = check_bounds_acyclic(&constraints) {
        return Err(refuse(i, e, "a quantifier whose bound reaches it"));
    }
    let ft = ft.declaring(&constraints);
    if let Err(e) = ft.check_quantifiers_held() {
        return Err(refuse(i, e, "a declared quantifier the signature does not hold"));
    }
    Ok(ft)
}

pub(super) fn tvar(i: &mut Input) -> R<TVar> {
    i.token('\'')?;
    fname(i).map(TVar::empty_generic)
}

fn varianttyp(i: &mut Input) -> R<Type> {
    let pos = i.pos();
    i.token('`')?;
    let tag = ArcStr::from(ident_str(i, true)?);
    let typs = i.optional(|i| {
        i.attempt(|i| {
            i.token('(')?;
            let typs = sep_by1_tok(i, &typ, &|i| i.token(')').map(drop))?;
            i.sptoken(')')?;
            Ok(typs)
        })
    })?;
    let mut t = typs.unwrap_or_else(LPooled::take);
    Ok(Type::Variant(tag, Arc::from_iter(t.drain(..)), WrittenAt(pos)))
}

/// A refusal of what was read at a position, or what was parsed.
fn parsed(i: &mut Input, r: Result<Noted, (SourcePosition, &'static str)>) -> R<Noted> {
    r.map_err(|(pos, why)| i.refuse_read(pos, why))
}

/// `{ name: T, .. }`, each field's type read by `field`; in a type
/// definition (`noted`) each field takes comments as a list's item does.
fn structtyp(i: &mut Input, field: fn(&mut Input) -> R<Noted>, noted: bool) -> R<Noted> {
    let item = |i: &mut Input| {
        let first = i.look_ahead(|i| {
            i.spaces();
            Ok(i.pos())
        })?;
        let above = leading_comments(i, false)?;
        i.spaces();
        let at = i.pos();
        let name = fldname(i)?;
        i.sptoken(':')?;
        let (typ, notes) = field(i)?;
        Ok(ListItem::new(first, above, (at, name, typ), notes))
    };
    i.token('{')?;
    let listed = commented(i, &item, ',', COMMA_WHY, &|i| i.token('}').map(drop), false)?;
    i.sptoken('}')?;
    let end = i.pos();
    parsed(i, struct_fields(listed, end, noted))
}

/// `[T, ..]`, each member read by `member`; in a type definition
/// (`noted`) each variant and named type takes comments as a list's item
/// does.
fn uniontyp(i: &mut Input, member: fn(&mut Input) -> R<Noted>, noted: bool) -> R<Noted> {
    let item = |i: &mut Input| {
        let first = i.look_ahead(|i| {
            i.spaces();
            Ok(i.pos())
        })?;
        let above = leading_comments(i, false)?;
        let (typ, notes) = member(i)?;
        Ok(ListItem::new(first, above, typ, notes))
    };
    i.token('[')?;
    let listed = commented(i, &item, ',', COMMA_WHY, &|i| i.token(']').map(drop), false)?;
    i.sptoken(']')?;
    let end = i.pos();
    parsed(i, union_members(listed, end, noted))
}

fn tupletyp(i: &mut Input) -> R<Type> {
    i.token('(')?;
    let (mut exps, trailing) =
        sep_by1_tok_trailing(i, &typ, &|i| i.token(')').map(drop))?;
    i.sptoken(')')?;
    let end = i.pos();
    match exps.len() {
        1 if trailing => Err(i.refuse(end, ONE_TUPLE)),
        1 => Ok(exps.pop().unwrap()),
        _ => Ok(Type::Tuple(Arc::from_iter(exps.drain(..)))),
    }
}

fn typref(i: &mut Input) -> R<Type> {
    let pos = i.pos();
    let (n, _) = typath(i)?;
    let params = match i.look_ahead(|i| i.optional(|i| i.attempt(|i| i.sptoken('<'))))? {
        None => None,
        Some(_) => {
            i.spaces();
            Some(angle_list(i, &typ)?)
        }
    };
    let params = params
        .map(|mut a| Arc::from_iter(a.drain(..)))
        .unwrap_or_else(|| Arc::from_iter([]));
    Ok(Type::Ref(Arc::new(TypeRef::new(
        ModPath::root(),
        n,
        params,
        Some(pos),
        Some(get_origin()),
    ))))
}

/// `name<T>` of a type constructor written as a keyword.
fn applied(i: &mut Input, name: &'static str) -> R<Type> {
    keyword(i, name)?;
    angles(i, typ)
}

pub(super) fn typ(i: &mut Input) -> R<Type> {
    i.knot(|i| {
        i.spaces();
        choice!(
            i,
            |i| {
                i.token('&')?;
                let m = mutability(i)?;
                typ(i).map(|t| Type::ByRef(m, Arc::new(t)))
            },
            |i| i.token('_').map(|_| Type::Bottom),
            |i| uniontyp(i, plain_typ, false).map(|(t, _)| t),
            tupletyp,
            |i| structtyp(i, plain_typ, false).map(|(t, _)| t),
            varianttyp,
            |i| fntype(i).map(|f| Type::Fn(Arc::new(f))),
            |i| applied(i, "Array").map(|t| Type::Array(Arc::new(t))),
            |i| applied(i, "List").map(|t| Type::List(Arc::new(t))),
            |i| keyword(i, "Any").map(|_| Type::Any),
            |i| {
                keyword(i, "Map")?;
                let (k, v) = angles(i, |i| {
                    let k = typ(i)?;
                    i.sptoken(',')?;
                    Ok((k, typ(i)?))
                })?;
                Ok(Type::Map { key: Arc::new(k), value: Arc::new(v) })
            },
            |i| applied(i, "Error").map(|t| Type::Error(Arc::new(t))),
            |i| {
                keyword(i, "Abstract")?;
                Err(i.because(
                    "Abstract<..> is legal only as the whole body of a type definition",
                ))
            },
            |i| i.attempt(|i| {
                let t = typeprim(i)?;
                i.not_followed_by(|i| i.string("::"))?;
                Ok(Type::Primitive(t.into()))
            }),
            |i| {
                i.attempt(|i| {
                    i.string("self")?;
                    not_prefix(i)?;
                    i.not_followed_by(|i| i.string("::"))
                })?;
                Ok(match i.optional(|i| i.attempt(|i| angles(i, typ)))? {
                    None => self_tvar(),
                    Some(arg) => Type::App(Arc::new(self_tvar()), Arc::new(arg)),
                })
            },
            |i| keyword(i, "'_").map(|_| Type::Hole),
            |i| {
                let tv = tvar(i)?;
                Ok(match i.optional(|i| i.attempt(|i| angles(i, typ)))? {
                    None => Type::TVar(tv),
                    Some(arg) => Type::App(Arc::new(Type::TVar(tv)), Arc::new(arg)),
                })
            },
            typref
        )
    })
}

/// [`typ`], noting no comment.
fn plain_typ(i: &mut Input) -> R<Noted> {
    typ(i).map(|t| (t, LPooled::take()))
}

/// A type in a type definition: a struct's fields and a union's members,
/// and the fields and members their types reach, take comments.
fn noted_typ(i: &mut Input) -> R<Noted> {
    i.knot(|i| {
        i.spaces();
        choice!(
            i,
            |i| structtyp(i, noted_typ, true),
            |i| uniontyp(i, noted_typ, true),
            plain_typ
        )
    })
}

fn typedef_params(i: &mut Input) -> R<Arc<[(TVar, Option<Type>)]>> {
    let mut ps = angle_list(i, &tvar_opt_bound)?;
    let end = i.pos();
    let one = |b: &Option<LPooled<Vec<Type>>>| b.as_ref().is_none_or(|b| b.len() == 1);
    if !ps.iter().all(|(_, b)| one(b)) {
        return Err(i.refuse(end, "a type parameter's bound is one type"));
    }
    Ok(Arc::from_iter(ps.drain(..).map(|(tv, b)| (tv, b.and_then(|mut b| b.pop())))))
}

pub(super) fn typedef(i: &mut Input) -> R<TypeDefExpr> {
    i.attempt(|i| {
        i.word_space("type")?;
        Ok(i.spaces())
    })?;
    let at = i.pos();
    let name = Name::written(typname(i)?, at);
    i.spaces();
    let params = i.optional(typedef_params)?.unwrap_or_else(|| Arc::from_iter([]));
    i.spaces();
    let body = i.optional(|i| {
        i.attempt(|i| {
            i.token('=')?;
            i.not_followed_by(|i| i.token('>'))
        })?;
        choice!(
            i,
            |i| {
                i.attempt(|i| {
                    i.spstring("Abstract")?;
                    not_prefix(i)
                })?;
                let (rep, notes) = angles(i, noted_typ)?;
                Ok((TypeDefBody::Abstract(Some(rep)), notes))
            },
            |i| noted_typ(i).map(|(t, notes)| (TypeDefBody::Alias(t), notes))
        )
    })?;
    let (body, mut notes) =
        body.unwrap_or_else(|| (TypeDefBody::Abstract(None), LPooled::take()));
    Ok(TypeDefExpr { name, params, body, notes: TypeNotes::of(&mut notes) })
}

/// Flatten `(tvar, bounds)` pairs into one `(tvar, type)` pair per
/// conjunct, the shape every constraint consumer takes.
pub(super) fn flatten_bounds(
    cs: impl IntoIterator<Item = (TVar, LPooled<Vec<Type>>)>,
) -> LPooled<Vec<(TVar, Type)>> {
    let mut out: LPooled<Vec<(TVar, Type)>> = LPooled::take();
    for (tv, mut bs) in cs {
        out.extend(bs.drain(..).map(|b| (tv.clone(), b)));
    }
    out
}

/// The receiver type of a trait method signature: the type variable
/// spelled `self`.
pub(crate) fn self_tvar() -> Type {
    Type::TVar(TVar::empty_generic(literal!("self")))
}

/// A type with the notes of the lists in it that take comments.
pub(super) type Noted = (Type, LPooled<Vec<TypeNote>>);

/// Where a comment in a type may not stand.
const TYPE_COMMENT: &str = "a comment in a type stands only in a type definition, beside a \
     field or a union member outside any type argument, payload or function type";

const MEMBER_COMMENT: &str =
    "a comment in a union stands beside a variant or a named type";

/// A struct type's field or a union's member as written: the comments
/// above it, ending its line and, as the last, below it, and the notes of
/// the lists inside it.
pub(super) struct ListItem<T> {
    above: LPooled<Vec<Comment>>,
    eol: Option<ArcStr>,
    below: LPooled<Vec<Comment>>,
    /// Where its first comment starts, and where the lines below it do.
    comment_at: Option<SourcePosition>,
    below_at: Option<SourcePosition>,
    item: T,
    notes: LPooled<Vec<TypeNote>>,
}

impl<T> Commentable for ListItem<T> {
    fn take_eol(&mut self, at: SourcePosition, eol: ArcStr) {
        self.comment_at.get_or_insert(at);
        self.eol = Some(eol)
    }
}

impl<T> ListItem<T> {
    /// `item` with the comments above it, which start at `at`.
    pub(super) fn new(
        at: SourcePosition,
        above: LPooled<Vec<Comment>>,
        item: T,
        notes: LPooled<Vec<TypeNote>>,
    ) -> Self {
        let comment_at = (!above.is_empty()).then_some(at);
        Self {
            above,
            eol: None,
            below: LPooled::take(),
            comment_at,
            below_at: None,
            item,
            notes,
        }
    }

    fn commented(&self) -> bool {
        !self.above.is_empty() || self.eol.is_some() || !self.below.is_empty()
    }

    /// Its comments, as the note on what was written at `at`.
    fn note(&mut self, at: SourcePosition) -> Option<TypeNote> {
        self.commented().then(|| TypeNote {
            at,
            comments: ItemComments {
                above: Comments::of(self.above.drain(..)),
                eol: Eol::new(self.eol.take()),
            },
            below: Comments::of(self.below.drain(..)),
        })
    }
}

/// The items of a list of fields or members, the lines below the last
/// on it; where they start for comments with no item to describe.
fn list_items<T>(
    mut l: Listed<ListItem<T>>,
) -> Result<LPooled<Vec<ListItem<T>>>, SourcePosition> {
    let mut items: LPooled<Vec<ListItem<T>>> =
        l.slots.drain(..).filter_map(|(_, i)| i).collect();
    match (items.last_mut(), l.below_at) {
        (_, None) => (),
        (Some(last), Some(at)) => {
            last.comment_at.get_or_insert(at);
            last.below_at = Some(at);
            last.below.extend(l.below.drain(..))
        }
        (None, Some(at)) => return Err(at),
    }
    Ok(items)
}

/// Where the first of `items` that holds a comment holds it.
fn first_comment<T>(items: &[ListItem<T>]) -> Option<SourcePosition> {
    items.iter().find_map(|i| i.comment_at)
}

pub(super) fn struct_fields(
    listed: Listed<ListItem<(SourcePosition, ArcStr, Type)>>,
    end: SourcePosition,
    noted: bool,
) -> Result<Noted, (SourcePosition, &'static str)> {
    let mut items = match list_items(listed) {
        Ok(items) if !items.is_empty() => items,
        Ok(_) => return Err((end, "a struct type has at least one field")),
        Err(at) => return Err((at, "a comment here has no field to describe")),
    };
    if !noted && let Some(at) = first_comment(&items) {
        return Err((at, TYPE_COMMENT));
    }
    if !sort_unique(&mut items, |i| &i.item.1) {
        return Err((end, "struct field names must be unique"));
    }
    let mut notes: LPooled<Vec<TypeNote>> = LPooled::take();
    let mut fields: LPooled<Vec<(ArcStr, Type, WrittenAt)>> = LPooled::take();
    for mut i in items.drain(..) {
        let at = i.item.0;
        notes.extend(i.notes.drain(..));
        notes.extend(i.note(at));
        let (_, name, typ) = i.item;
        fields.push((name, typ, WrittenAt(at)));
    }
    Ok((Type::Struct(Arc::from_iter(fields.drain(..))), notes))
}

pub(super) fn union_members(
    listed: Listed<ListItem<Type>>,
    end: SourcePosition,
    noted: bool,
) -> Result<Noted, (SourcePosition, &'static str)> {
    let mut items = list_items(listed)
        .map_err(|at| (at, "a comment here has no member to describe"))?;
    if !noted && let Some(at) = first_comment(&items) {
        return Err((at, TYPE_COMMENT));
    }
    let below = items
        .last_mut()
        .and_then(|i| Some((i.below_at.take()?, std::mem::take(&mut i.below))));
    let mut notes: LPooled<Vec<TypeNote>> = LPooled::take();
    for i in items.iter_mut() {
        notes.extend(i.notes.drain(..));
        if i.commented() {
            match i.item.written_at() {
                Some(at) => notes.extend(i.note(at)),
                None => return Err((i.comment_at.unwrap_or(end), MEMBER_COMMENT)),
            }
        }
    }
    let typ = Type::flatten_set(items.drain(..).map(|i| i.item));
    let key = |at: SourcePosition| (at.line, at.column);
    // the lines below the last member print below the member written last,
    // which prints last, a nested union's included
    if let Some((at, mut below)) = below {
        let last = match &typ {
            Type::Set(members) => {
                members.iter().filter_map(|m| m.written_at()).max_by_key(|at| key(*at))
            }
            t => t.written_at(),
        };
        let Some(last) = last else { return Err((at, MEMBER_COMMENT)) };
        match notes.iter_mut().find(|n| key(n.at) == key(last)) {
            Some(n) => {
                let mut lines: LPooled<Vec<Comment>> =
                    n.below.lines().iter().cloned().collect();
                lines.extend(below.drain(..));
                n.below = Comments::of(lines.drain(..));
            }
            None => notes.push(TypeNote {
                at: last,
                comments: ItemComments::default(),
                below: Comments::of(below.drain(..)),
            }),
        }
    }
    if !notes.is_empty() {
        let mut kept: LPooled<Vec<(i32, i32)>> = LPooled::take();
        noted_items(&typ, &mut |at| kept.push(key(at)));
        kept.sort();
        if let Some(n) = notes.iter().find(|n| kept.binary_search(&key(n.at)).is_err()) {
            let why = match typ {
                Type::Set(_) => {
                    "a commented member merges with another member of the union"
                }
                _ => "a comment in a union describes one of two or more members",
            };
            return Err((n.at, why));
        }
        // a nested union's members join this one, and only the last
        // prints the lines below
        if let Type::Set(members) = &typ {
            let ats = || members.iter().filter_map(|m| m.written_at()).map(key);
            let last = ats().max();
            let misplaced = |n: &&TypeNote| {
                !n.below.lines().is_empty()
                    && ats().any(|at| at == key(n.at))
                    && Some(key(n.at)) != last
            };
            if let Some(n) = notes.iter().find(misplaced) {
                return Err((
                    n.at,
                    "comments below a nested union's last member have no place in the union that holds it",
                ));
            }
        }
    }
    Ok((typ, notes))
}
