use super::{
    COMMA_WHY, Commentable, Listed, ONE_TUPLE, commented, csep, fldname, fname,
    fname_noting,
    grow::{because, grow, refuse},
    ident,
    lambdaexp::{LABELED_FIRST, labeled_first},
    leading_comments, not_prefix, path_root, sep_by_tok, sep_by1_tok,
    sep_by1_tok_trailing, sort_unique, spaces, spaces1, spstring, sptoken, typname,
};
use crate::{
    expr::{
        Comment, Comments, Eol, ItemComments, ModPath, Name, TypeDefBody, TypeDefExpr,
        TypeNote, TypeNotes, WrittenAt, WrittenPath, get_origin, noted_items,
    },
    typ::{
        Bound, FnArgKind, FnArgType, FnType, TVar, Type, TypeRef, check_bounds_acyclic,
    },
};
use arcstr::{ArcStr, literal};
use combine::{
    ParseError, Parser, RangeStream, attempt, between, choice, look_ahead, many1,
    not_followed_by, optional,
    parser::char::string,
    position, satisfy, sep_by1,
    stream::{Range, position::SourcePosition},
    token, unexpected_any, value,
};
use compact_str::{CompactString, format_compact};
use netidx_core::utils::Either;
use netidx_value::Typ;
use poolshark::local::LPooled;
use std::str::FromStr;
use triomphe::Arc;

pub(super) fn typath<I>() -> impl Parser<I, Output = (ModPath, WrittenPath)>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (
        spaces().with(path_root()),
        // a type path is written as a value path is: no space inside it
        sep_by1(
            (position(), choice((fname_noting(false), typname())))
                .map(|(pos, n)| Name::written(n, pos)),
            string("::"),
        ),
    )
        .then(|(mut root, mut parts): (LPooled<Vec<Name>>, LPooled<Vec<Name>>)| {
            let capitalized = parts
                .last()
                .and_then(|p| p.name.chars().next())
                .is_some_and(char::is_uppercase);
            if capitalized {
                root.extend(parts.drain(..));
                let at = WrittenPath(root.iter().filter_map(|n| n.at.get()).collect());
                value((ModPath::from(root.drain(..).map(|n| n.name)), at)).right()
            } else {
                because("type names must be capitalized").left()
            }
        })
}

/// A primitive type's name. `map` and `abstract` name no type here.
fn typeprim<I>() -> impl Parser<I, Output = Typ>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    ident(false).then(|s| match Typ::from_str(&s) {
        Ok(t) if t.name() == s && !matches!(t, Typ::Map | Typ::Abstract) => {
            value(t).left()
        }
        _ => unexpected_any("a primitive type").right(),
    })
}

/// A type variable's bound: one type, or a `+`-joined conjunction of
/// traits (`'a: Read + Write`), one conjunct per member.
pub(super) fn bound<I>() -> impl Parser<I, Output = LPooled<Vec<Type>>>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
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
    let keyword = attempt(
        spaces()
            .with(many1(satisfy(|c: char| c.is_ascii_alphabetic())))
            .skip(not_prefix())
            .then(|w: CompactString| match predicate(&w) {
                Some(t) => value(t).left(),
                None => because("not a predicate").right(),
            }),
    );
    sep_by1(choice((keyword, typ())), attempt(spaces().with(token('+'))))
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

/// `'a: A + B`: a type variable and its bound.
pub(super) fn tvar_bound<I>() -> impl Parser<I, Output = (TVar, LPooled<Vec<Type>>)>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (spaces().with(tvar()).skip(sptoken(':')), bound())
}

/// `'a`, or `'a: A + B`.
pub(super) fn tvar_opt_bound<I>()
-> impl Parser<I, Output = (TVar, Option<LPooled<Vec<Type>>>)>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (spaces().with(tvar()), optional(attempt(sptoken(':')).with(bound())))
}

fn fnconstraints<I>() -> impl Parser<I, Output = LPooled<Vec<(TVar, Type)>>>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    spaces()
        .with(optional(between(
            token('<'),
            sptoken('>'),
            sep_by1_tok(tvar_bound(), csep(), token('>')),
        )))
        .map(|cs: Option<LPooled<Vec<(TVar, LPooled<Vec<Type>>)>>>| match cs {
            Some(mut cs) => flatten_bounds(cs.drain(..)),
            None => LPooled::take(),
        })
}

fn fnlabeled<I>() -> impl Parser<I, Output = FnArgType>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    choice((string("?#").map(|_| true), string("#").map(|_| false))).then(|optional| {
        (fname().skip(sptoken(':')), typ()).map(move |(name, typ)| FnArgType {
            kind: FnArgKind::Labeled { name: name.into(), has_default: optional },
            typ,
        })
    })
}

fn fnpositional<I>() -> impl Parser<I, Output = FnArgType>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    choice((
        attempt(string("self").skip(not_prefix()).skip(not_followed_by(sptoken(':'))))
            .with(optional(attempt(between(sptoken('<'), sptoken('>'), typ()))))
            .map(|arg| FnArgType {
                kind: FnArgKind::Positional { name: Some(literal!("self")) },
                typ: match arg {
                    None => self_tvar(),
                    Some(arg) => Type::App(Arc::new(self_tvar()), Arc::new(arg)),
                },
            }),
        (fname().skip(sptoken(':')), typ()).map(|(name, typ)| FnArgType {
            kind: FnArgKind::Positional { name: Some(name.into()) },
            typ,
        }),
    ))
}

/// The receiver type of a trait method signature: the type variable
/// spelled `self`.
pub(crate) fn self_tvar() -> Type {
    Type::TVar(TVar::empty_generic(literal!("self")))
}

fn fnargs<I>() -> impl Parser<I, Output = LPooled<Vec<Either<FnArgType, Type>>>>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    spaces().with(between(
        token('('),
        sptoken(')'),
        sep_by_tok(
            spaces().with(choice((
                string("@args:").with(typ()).map(|e| Either::Right(e)),
                fnlabeled().map(Either::Left),
                fnpositional().map(Either::Left),
            ))),
            csep(),
            attempt(sptoken(')')),
        ),
    ))
}

pub(super) fn fntype<I>() -> impl Parser<I, Output = FnType>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    attempt(string("fn").skip(not_prefix()))
        .with((
            fnconstraints(),
            fnargs(),
            spstring("->").with(typ()),
            optional(
                attempt(spaces1().with(string("throws"))).with(spaces1()).with(typ()),
            ),
            position(),
        ))
        .then(|(constraints, mut args, rtype, throws, end)| {
            let vargs = match args.pop() {
                None => None,
                Some(Either::Right(t)) => Some(t),
                Some(Either::Left(t)) => {
                    args.push(Either::Left(t));
                    None
                }
            };
            if !args.iter().all(|a| a.is_left()) {
                return because("vargs must appear once at the end of the args").left();
            }
            let args = Arc::from_iter(args.drain(..).map(|t| match t {
                Either::Left(t) => t,
                Either::Right(_) => unreachable!(),
            }));
            let named_before = |(i, a): (usize, &FnArgType)| {
                a.name().is_some_and(|n| args[..i].iter().any(|b| b.name() == Some(n)))
            };
            if args.iter().enumerate().any(named_before) {
                const UNIQUE: &str = "arguments must have unique names";
                super::grow::note_reason(end, None, CompactString::const_new(UNIQUE));
                return because(UNIQUE).left();
            }
            if !labeled_first(args.iter().map(|a| a.is_labeled())) {
                return because(LABELED_FIRST).left();
            }
            let explicit_throws = throws.is_some();
            let throws = throws.unwrap_or(Type::Bottom);
            let ft = FnType {
                args,
                vargs,
                rtype,
                throws,
                explicit_throws,
                ..Default::default()
            };
            let refuse = |e: anyhow::Error, why| {
                super::grow::note_reason(end, None, format_compact!("{e}"));
                because(why).left()
            };
            if let Err(e) = check_bounds_acyclic(&constraints) {
                return refuse(e, "a quantifier whose bound reaches it");
            }
            let ft = ft.declaring(&constraints);
            if let Err(e) = ft.check_quantifiers_held() {
                return refuse(e, "a declared quantifier the signature does not hold");
            }
            value(ft).right()
        })
}

pub(super) fn tvar<I>() -> impl Parser<I, Output = TVar>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    token('\'').with(fname()).map(TVar::empty_generic)
}

fn varianttyp<I>() -> impl Parser<I, Output = Type>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (
        position(),
        token('`').with(ident(true)),
        optional(attempt(between(
            token('('),
            sptoken(')'),
            sep_by1_tok(typ(), csep(), token(')')),
        ))),
    )
        .map(|(pos, tag, typs): (_, ArcStr, Option<LPooled<Vec<Type>>>)| {
            let mut t = match typs {
                None => LPooled::take(),
                Some(v) => v,
            };
            Type::Variant(tag.clone(), Arc::from_iter(t.drain(..)), WrittenAt(pos))
        })
}

/// A type with the notes of the lists in it that take comments.
type Noted = (Type, LPooled<Vec<TypeNote>>);

/// Where a comment in a type may not stand.
const TYPE_COMMENT: &str = "a comment in a type stands only in a type definition, beside a \
     field or a union member outside any type argument, payload or function type";
const MEMBER_COMMENT: &str =
    "a comment in a union stands beside a variant or a named type";

/// A struct type's field or a union's member as written: the comments
/// above it, ending its line and, as the last, below it, and the notes of
/// the lists inside it.
struct ListItem<T> {
    above: LPooled<Vec<Comment>>,
    eol: Option<ArcStr>,
    below: LPooled<Vec<Comment>>,
    item: T,
    notes: LPooled<Vec<TypeNote>>,
}

impl<T> Commentable for ListItem<T> {
    fn take_eol(&mut self, eol: ArcStr) {
        self.eol = Some(eol)
    }
}

impl<T> ListItem<T> {
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
/// on it; `None` for comments with no item to describe.
fn list_items<T>(mut l: Listed<ListItem<T>>) -> Option<LPooled<Vec<ListItem<T>>>> {
    let mut items: LPooled<Vec<ListItem<T>>> =
        l.slots.drain(..).filter_map(|(_, i)| i).collect();
    match items.last_mut() {
        Some(last) => last.below.extend(l.below.drain(..)),
        None if !l.below.is_empty() => return None,
        None => (),
    }
    Some(items)
}

/// A refusal at a position, or what was parsed.
fn parsed<I>(
    r: Result<Noted, (SourcePosition, &'static str)>,
) -> impl Parser<I, Output = Noted>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    match r {
        Ok(t) => value(t).left(),
        Err((pos, why)) => refuse(pos, why).right(),
    }
}

/// `{ name: T, .. }`, each field's type read by `field`; in a type
/// definition (`noted`) each field takes comments as a list's item does.
fn structtyp<I, P, PF>(field: PF, noted: bool) -> impl Parser<I, Output = Noted>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
    P: Parser<I, Output = Noted>,
    PF: Fn() -> P,
{
    let item = move || {
        (
            leading_comments(false),
            spaces().with(position()),
            fldname().skip(sptoken(':')),
            field(),
        )
            .map(|(above, at, name, (typ, notes))| ListItem {
                above,
                eol: None,
                below: LPooled::take(),
                item: (at, name, typ),
                notes,
            })
    };
    (
        between(
            token('{'),
            sptoken('}'),
            commented(item, ',', COMMA_WHY, || token('}'), false),
        ),
        position(),
    )
        .then(move |(listed, end)| parsed(struct_fields(listed, end, noted)))
}

fn struct_fields(
    listed: Listed<ListItem<(SourcePosition, ArcStr, Type)>>,
    end: SourcePosition,
    noted: bool,
) -> Result<Noted, (SourcePosition, &'static str)> {
    let mut items = match list_items(listed) {
        Some(items) if !items.is_empty() => items,
        Some(_) => return Err((end, "a struct type has at least one field")),
        None => return Err((end, "a comment here has no field to describe")),
    };
    if !noted && items.iter().any(|i| i.commented()) {
        return Err((end, TYPE_COMMENT));
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

/// `[T, ..]`, each member read by `member`; in a type definition
/// (`noted`) each variant and named type takes comments as a list's item
/// does.
fn uniontyp<I, P, PF>(member: PF, noted: bool) -> impl Parser<I, Output = Noted>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
    P: Parser<I, Output = Noted>,
    PF: Fn() -> P,
{
    let item = move || {
        (leading_comments(false), member()).map(|(above, (typ, notes))| ListItem {
            above,
            eol: None,
            below: LPooled::take(),
            item: typ,
            notes,
        })
    };
    (
        between(
            token('['),
            sptoken(']'),
            commented(item, ',', COMMA_WHY, || token(']'), false),
        ),
        position(),
    )
        .then(move |(listed, end)| parsed(union_members(listed, end, noted)))
}

// CR claude for claude: [bug/low] The lines below a union's last item go to the last item
// that records a position, and a nested union's members record none, so `type T = [`A,
// [`B, `C] // below ]`, a comment under the outer list's last item, is refused as
// "comments below a nested union's last member have no place in the union that holds it".
// Attach them to the member with the largest written position after flatten_set, or
// refuse with the real reason. Also struct_fields and union_members report a refusal at
// the closing bracket, not at the comment (ann_union.gx: line 4 col 2 for a comment on
// line 2). Probes: ~/tmp/cr29/scratch/review2/RV3/d10/{nested_below,ann_union}.gx
// (type-notes-refusal-01)
fn union_members(
    listed: Listed<ListItem<Type>>,
    end: SourcePosition,
    noted: bool,
) -> Result<Noted, (SourcePosition, &'static str)> {
    let Some(mut items) = list_items(listed) else {
        return Err((end, "a comment here has no member to describe"));
    };
    if !noted && items.iter().any(|i| i.commented()) {
        return Err((end, TYPE_COMMENT));
    }
    // the lines below the last member print below the last that records
    // where it was written, which prints last
    let below = items.last_mut().map(|i| std::mem::take(&mut i.below));
    if let Some(mut below) = below.filter(|b| !b.is_empty()) {
        match items.iter_mut().rev().find(|i| i.item.written_at().is_some()) {
            Some(i) => i.below.extend(below.drain(..)),
            None => return Err((end, MEMBER_COMMENT)),
        }
    }
    let mut notes: LPooled<Vec<TypeNote>> = LPooled::take();
    for i in items.iter_mut() {
        notes.extend(i.notes.drain(..));
        if i.commented() {
            match i.item.written_at() {
                Some(at) => notes.extend(i.note(at)),
                None => return Err((end, MEMBER_COMMENT)),
            }
        }
    }
    let typ = Type::flatten_set(items.drain(..).map(|i| i.item));
    if !notes.is_empty() {
        let key = |at: SourcePosition| (at.line, at.column);
        let mut kept: LPooled<Vec<(i32, i32)>> = LPooled::take();
        noted_items(&typ, &mut |at| kept.push(key(at)));
        kept.sort();
        if notes.iter().any(|n| kept.binary_search(&key(n.at)).is_err()) {
            let why = match typ {
                Type::Set(_) => {
                    "a commented member merges with another member of the union"
                }
                _ => "a comment in a union describes one of two or more members",
            };
            return Err((end, why));
        }
        // a nested union's members join this one, and only the last
        // prints the lines below
        if let Type::Set(members) = &typ {
            let ats = || members.iter().filter_map(|m| m.written_at()).map(key);
            let last = ats().max();
            let misplaced = |n: &TypeNote| {
                !n.below.lines().is_empty()
                    && ats().any(|at| at == key(n.at))
                    && Some(key(n.at)) != last
            };
            if notes.iter().any(misplaced) {
                return Err((
                    end,
                    "comments below a nested union's last member have no place in the union that holds it",
                ));
            }
        }
    }
    Ok((typ, notes))
}

fn tupletyp<I>() -> impl Parser<I, Output = Type>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (
        between(
            token('('),
            sptoken(')'),
            sep_by1_tok_trailing(typ(), csep(), token(')')),
        ),
        position(),
    )
        .then(|((mut exps, trailing), end): ((LPooled<Vec<Type>>, bool), _)| {
            match exps.len() {
                1 if trailing => refuse(end, ONE_TUPLE).right(),
                1 => value(exps.pop().unwrap()).left(),
                _ => value(Type::Tuple(Arc::from_iter(exps.drain(..)))).left(),
            }
        })
}

pub(super) fn typref<I>() -> impl Parser<I, Output = Type>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    (
        position(),
        typath(),
        look_ahead(optional(attempt(sptoken('<')))).then(|o| match o {
            None => value(None).left(),
            Some(_) => between(
                sptoken('<'),
                sptoken('>'),
                sep_by1_tok(typ(), csep(), token('>')),
            )
            .map(Some)
            .right(),
        }),
    )
        .map(
            |(pos, (n, _), params): (_, (ModPath, _), Option<LPooled<Vec<Type>>>)| {
                let params = params
                    .map(|mut a| Arc::from_iter(a.drain(..)))
                    .unwrap_or_else(|| Arc::from_iter([]));
                Type::Ref(Arc::new(TypeRef::new(
                    ModPath::root(),
                    n,
                    params,
                    Some(pos),
                    Some(get_origin()),
                )))
            },
        )
}

parser! {
    pub(super) fn typ[I]()(I) -> Type
    where [I: RangeStream<Token = char, Position = SourcePosition>, I::Range: Range]
    {
        grow(spaces().with(choice((
            (token('&'), super::arithexp::mutability(), typ())
                .map(|(_, m, t)| Type::ByRef(m, Arc::new(t))),
            token('_').map(|_| Type::Bottom),
            uniontyp(plain_typ, false).map(|(t, _)| t),
            tupletyp(),
            structtyp(plain_typ, false).map(|(t, _)| t),
            varianttyp(),
            fntype().map(|f| Type::Fn(Arc::new(f))),
            attempt(string("Array").skip(not_prefix())).with(between(sptoken('<'), sptoken('>'), typ()))
                .map(|t| Type::Array(Arc::new(t))),
            attempt(string("List").skip(not_prefix())).with(between(sptoken('<'), sptoken('>'), typ()))
                .map(|t| Type::List(Arc::new(t))),
            attempt(string("Any").skip(not_prefix())).map(|_| Type::Any),
            attempt(string("Map").skip(not_prefix())).with(between(
                sptoken('<'), sptoken('>'),
                (typ().skip(sptoken(',')), typ())
            )).map(|(k, v)| Type::Map { key: Arc::new(k), value: Arc::new(v) }),
            attempt(string("Error").skip(not_prefix())).with(between(sptoken('<'), sptoken('>'), typ()))
                .map(|t| Type::Error(Arc::new(t))),
            attempt(string("Abstract").skip(not_prefix())).then(|_| {
                because("Abstract<..> is legal only as the whole body of a type definition")
            }),
            attempt(typeprim().skip(not_followed_by(string("::"))))
                .map(|typ| Type::Primitive(typ.into())),
            attempt(string("self").skip(not_prefix()).skip(not_followed_by(string("::"))))
                .with(optional(attempt(between(sptoken('<'), sptoken('>'), typ()))))
                .map(|arg| match arg {
                    None => self_tvar(),
                    Some(arg) => Type::App(Arc::new(self_tvar()), Arc::new(arg)),
                }),
            attempt(string("'_").skip(not_prefix())).map(|_| Type::Hole),
            (tvar(), optional(attempt(between(sptoken('<'), sptoken('>'), typ()))))
                .map(|(tv, arg)| match arg {
                    None => Type::TVar(tv),
                    Some(arg) => Type::App(Arc::new(Type::TVar(tv)), Arc::new(arg)),
                }),
            typref(),
        ))))
    }
}

/// [`typ`], noting no comment.
fn plain_typ<I>() -> impl Parser<I, Output = Noted>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    typ().map(|t| (t, LPooled::take()))
}

parser! {
    /// A type in a type definition: a struct's fields and a union's
    /// members, and the fields and members their types reach, take
    /// comments.
    fn noted_typ[I]()(I) -> Noted
    where [I: RangeStream<Token = char, Position = SourcePosition>, I::Range: Range]
    {
        grow(spaces().with(choice((
            structtyp(noted_typ, true),
            uniontyp(noted_typ, true),
            plain_typ(),
        ))))
    }
}

pub(super) fn typedef<I>() -> impl Parser<I, Output = TypeDefExpr>
where
    I: RangeStream<Token = char, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
    I::Range: Range,
{
    let params = (
        between(
            token('<'),
            sptoken('>'),
            sep_by1_tok(tvar_opt_bound(), csep(), token('>')),
        ),
        position(),
    )
        .then(
            |(mut ps, end): (LPooled<Vec<(TVar, Option<LPooled<Vec<Type>>>)>>, _)| {
                let one = |b: &Option<LPooled<Vec<Type>>>| {
                    b.as_ref().is_none_or(|b| b.len() == 1)
                };
                if !ps.iter().all(|(_, b)| one(b)) {
                    return refuse(end, "a type parameter's bound is one type").right();
                }
                let ps = ps.drain(..).map(|(tv, b)| (tv, b.and_then(|mut b| b.pop())));
                value(Arc::<[(TVar, Option<Type>)]>::from_iter(ps)).left()
            },
        );
    (
        attempt(string("type").skip(spaces1())).with((position(), typname())),
        spaces().with(optional(params)),
        spaces().with(optional(
            attempt(token('=').skip(not_followed_by(token('>')))).with(choice((
                attempt(spaces().with(string("Abstract")).skip(not_prefix()))
                    .with(between(sptoken('<'), sptoken('>'), noted_typ()))
                    .map(|(rep, notes)| (TypeDefBody::Abstract(Some(rep)), notes)),
                noted_typ().map(|(t, notes)| (TypeDefBody::Alias(t), notes)),
            ))),
        )),
    )
        .map(|((at, name), params, body)| {
            let name = Name::written(name, at);
            let params = params.unwrap_or_else(|| Arc::from_iter([]));
            let (body, mut notes) =
                body.unwrap_or_else(|| (TypeDefBody::Abstract(None), LPooled::take()));
            TypeDefExpr { name, params, body, notes: TypeNotes::of(&mut notes) }
        })
}
