//! Narrowing. A select is one use of its scrutinee: each decided type that
//! reaches the scrutinee is passed down the arms, each arm keeps the part
//! its pattern admits and binds its names to that part, and what the last
//! arm leaves must be nothing. A pattern tests only what is written in it,
//! and a test admits what the run time cannot tell from it.

use super::{Solving, alike, deep, open, rep, shallow, union_of};
use crate::{
    env::Env,
    expr::WrittenAt,
    stack::ensure_sufficient,
    typ::{AbstractId, Bound, Mutability, TVar, Type, setops::strictly_identical},
};
use anyhow::{Result, bail};
use arcstr::{ArcStr, literal};
use compact_str::format_compact;
use netidx_value::Typ;
use smallvec::SmallVec;
use std::ops::ControlFlow;
use triomphe::Arc;

#[derive(Clone, Debug)]
pub enum Pat {
    /// Admits everything; binds it where named.
    Any(Option<TVar>),
    /// A run-time type test, then a structure.
    Test(Type, Box<Pat>),
    /// A literal of a primitive type: `whole` where the literal is every
    /// value of the type (`null`).
    Lit(Type, bool),
    Tuple(Option<TVar>, Vec<Pat>),
    Variant(Option<TVar>, ArcStr, Vec<Pat>),
    /// The named fields; `exhaustive` where the pattern names every field.
    Struct(Option<TVar>, Vec<(ArcStr, Pat)>, bool),
    /// An array or list by its elements: the whole, the elements written,
    /// and the name of the run the pattern leaves open, if it leaves one.
    Elems {
        all: Option<TVar>,
        list: bool,
        elems: Vec<Pat>,
        open: Option<Option<TVar>>,
    },
    Or(Vec<Pat>),
    /// An abstract type's constructor around what it holds.
    Abs(Option<TVar>, AbstractId, Box<Pat>),
    /// A pattern, and a cell that takes the whole of what it admits.
    Whole(TVar, Box<Pat>),
}

impl Pat {
    /// The pattern with each cell it binds through `f`.
    pub(super) fn map_cells(&self, f: &mut dyn FnMut(&TVar) -> TVar) -> Pat {
        let named =
            |v: &Option<TVar>, f: &mut dyn FnMut(&TVar) -> TVar| v.as_ref().map(f);
        let all = |ps: &[Pat], f: &mut dyn FnMut(&TVar) -> TVar| -> Vec<Pat> {
            ps.iter().map(|p| p.map_cells(f)).collect()
        };
        match self {
            Pat::Any(v) => Pat::Any(named(v, f)),
            Pat::Test(t, p) => Pat::Test(t.clone(), Box::new(p.map_cells(f))),
            Pat::Lit(t, whole) => Pat::Lit(t.clone(), *whole),
            Pat::Tuple(v, ps) => {
                let v = named(v, f);
                Pat::Tuple(v, all(ps, f))
            }
            Pat::Variant(v, tag, ps) => {
                let v = named(v, f);
                Pat::Variant(v, tag.clone(), all(ps, f))
            }
            Pat::Struct(v, fs, exhaustive) => {
                let v = named(v, f);
                let fs = fs.iter().map(|(n, p)| (n.clone(), p.map_cells(f))).collect();
                Pat::Struct(v, fs, *exhaustive)
            }
            Pat::Elems { all: v, list, elems, open } => {
                let v = named(v, f);
                let elems = all(elems, f);
                let open = open.as_ref().map(|o| named(o, f));
                Pat::Elems { all: v, list: *list, elems, open }
            }
            Pat::Or(ps) => Pat::Or(all(ps, f)),
            Pat::Abs(v, id, p) => {
                let v = named(v, f);
                Pat::Abs(v, *id, Box::new(p.map_cells(f)))
            }
            Pat::Whole(v, p) => {
                let v = f(v);
                Pat::Whole(v, Box::new(p.map_cells(f)))
            }
        }
    }

    /// The widest type the pattern admits, the bound a test puts on a
    /// scrutinee nothing has decided; none where it admits anything.
    pub fn admits(&self) -> Option<Type> {
        let all = |ps: &[Pat]| -> Arc<[Type]> {
            Arc::from_iter(ps.iter().map(|p| p.admits().unwrap_or(Type::Any)))
        };
        match self {
            Pat::Any(_) | Pat::Struct(_, _, false) | Pat::Abs(..) => None,
            Pat::Test(t, _) | Pat::Lit(t, _) => Some(t.clone()),
            Pat::Tuple(_, ps) => Some(Type::Tuple(all(ps))),
            Pat::Variant(_, tag, ps) => {
                Some(Type::Variant(tag.clone(), all(ps), WrittenAt::default()))
            }
            Pat::Struct(_, fs, true) => {
                Some(Type::Struct(Arc::from_iter(fs.iter().map(|(n, p)| {
                    (n.clone(), p.admits().unwrap_or(Type::Any), WrittenAt::default())
                }))))
            }
            Pat::Elems { list: false, .. } => Some(Type::Array(Arc::new(Type::Any))),
            Pat::Elems { list: true, .. } => Some(Type::List(Arc::new(Type::Any))),
            Pat::Or(ps) => {
                let ts: Option<SmallVec<[Type; 4]>> =
                    ps.iter().map(|p| p.admits()).collect();
                ts.map(union_of)
            }
            Pat::Whole(_, p) => p.admits(),
        }
    }

    fn cells(&self, out: &mut SmallVec<[TVar; 4]>) {
        self.map_cells(&mut |v| {
            out.push(v.clone());
            v.clone()
        });
    }

    /// The abstract type whose constructor here takes nothing out of a
    /// value whose instance is not known ([`blind`]).
    fn blind(&self, env: &Env) -> Option<ArcStr> {
        match self {
            Pat::Any(_) | Pat::Lit(..) => None,
            Pat::Abs(_, id, p) => blind(env, id, p).or_else(|| p.blind(env)),
            Pat::Test(_, p) | Pat::Whole(_, p) => p.blind(env),
            Pat::Tuple(_, ps) | Pat::Variant(_, _, ps) | Pat::Or(ps) => {
                ps.iter().find_map(|p| p.blind(env))
            }
            Pat::Elems { elems, .. } => elems.iter().find_map(|p| p.blind(env)),
            Pat::Struct(_, fs, _) => fs.iter().find_map(|(_, p)| p.blind(env)),
        }
    }
}

/// The name of the abstract type `id` where its constructor around `p`
/// may take nothing out of a value whose instance is not known: `p` names
/// a part, and read at `Any` parameters the payload holds something a
/// value could be written into or called.
fn blind(env: &Env, id: &AbstractId, p: &Pat) -> Option<ArcStr> {
    let made = env.abstract_reps.get(id)?;
    let params: SmallVec<[Type; 4]> = made.params.iter().map(|_| Type::Any).collect();
    let mut names = SmallVec::new();
    p.cells(&mut names);
    let taken =
        rep(env, id, &params).is_some_and(|r| holds_taker(env, &r, &mut SmallVec::new()));
    (taken && !names.is_empty()).then(|| made.name.clone())
}

#[derive(Clone, Debug)]
pub struct Arm {
    pub pat: Pat,
    pub guarded: bool,
}

/// Inside a select `bool` is the union of two tags, so that `true` and
/// `false` arms each take their half.
pub const TRUE: ArcStr = literal!("#true");
pub const FALSE: ArcStr = literal!("#false");

fn tag(name: &ArcStr, parts: impl IntoIterator<Item = Type>) -> Type {
    Type::Variant(name.clone(), Arc::from_iter(parts), WrittenAt::default())
}

/// `t` with each part `f` answers for replaced, a part's own parts after
/// it; a union is rebuilt as one.
fn map_parts(t: &Type, f: &mut impl FnMut(&Type) -> Option<Type>) -> Type {
    ensure_sufficient(|| match f(t) {
        Some(t) => t,
        None => match t {
            Type::Set(ms) => union_of(ms.iter().map(|m| map_parts(m, f))),
            t => t
                .cow_children(&mut |c| Some(map_parts(c, f)))
                .unwrap_or_else(|| t.clone()),
        },
    })
}

pub fn split(t: &Type) -> Type {
    map_parts(t, &mut |t| match t {
        Type::Primitive(p) if p.contains(Typ::Bool) => {
            let halves = [tag(&TRUE, []), tag(&FALSE, [])];
            Some(union_of([Type::Primitive(*p & !Typ::Bool)].into_iter().chain(halves)))
        }
        _ => None,
    })
}

pub fn unsplit(t: &Type) -> Type {
    map_parts(t, &mut |t| match t {
        Type::Variant(name, _, _) if *name == TRUE || *name == FALSE => {
            Some(Type::Primitive(Typ::Bool.into()))
        }
        t => lengths(t)
            .map(|(_, _, inner)| inner)
            .or_else(|| payload_left(t).map(|(whole, _)| whole))
            .or_else(|| elems_left(t).map(|(array, ..)| array))
            .or_else(|| within_left(t).map(|(var, _)| var))
            .map(|inner| unsplit(&inner)),
    })
}

/// Inside a select, the values of a declared variable that earlier arms
/// left: those in the second type, a part of its bound. Written as a
/// variant around the variable and that part.
const WITHIN: &str = "#within";

fn within_left(t: &Type) -> Option<(Type, Type)> {
    match t {
        Type::Variant(name, ts, _) if name.as_str() == WITHIN && ts.len() == 2 => {
            Some((ts[0].clone(), ts[1].clone()))
        }
        _ => None,
    }
}

/// Inside a select, the arrays of one length that earlier arms left:
/// those whose elements, as a tuple of that length, are of the second
/// type. Written as a variant around the array type and the tuples.
const ELEMS: &str = "#elems:";

fn elems_left(t: &Type) -> Option<(Type, usize, Type)> {
    match t {
        Type::Variant(name, ts, _) if name.starts_with(ELEMS) && ts.len() == 2 => {
            Some((ts[0].clone(), name[ELEMS.len()..].parse().ok()?, ts[1].clone()))
        }
        _ => None,
    }
}

fn elems(array: &Type, n: usize, tuples: Type) -> Type {
    match nothing(&tuples) {
        true => Type::Bottom,
        false => {
            let name = format_compact!("{ELEMS}{n}");
            tag(&ArcStr::from(name.as_str()), [array.clone(), tuples])
        }
    }
}

/// Inside a select, the values of an abstract type that earlier arms
/// left: those whose payload is of the second type. Written as a variant
/// around the abstract type and what is left of its payload.
const PAYLOAD: &str = "#payload";

fn payload_left(t: &Type) -> Option<(Type, Type)> {
    match t {
        Type::Variant(name, ts, _) if name.as_str() == PAYLOAD && ts.len() == 2 => {
            Some((ts[0].clone(), ts[1].clone()))
        }
        _ => None,
    }
}

/// Inside a select, the arrays of a type that earlier arms left: those
/// whose length is not in `taken` (a bit per length under 64) and is under
/// `from`. Written as a variant around the array type.
const LENGTHS: &str = "#lengths:";

fn lengths(t: &Type) -> Option<(u64, usize, Type)> {
    match t {
        Type::Variant(name, ts, _) if name.starts_with(LENGTHS) && ts.len() == 1 => {
            let (taken, from) = name[LENGTHS.len()..].split_once(':')?;
            Some((taken.parse().ok()?, from.parse().ok()?, ts[0].clone()))
        }
        _ => None,
    }
}

fn left(array: &Type, taken: u64, from: usize) -> Type {
    match (taken, from) {
        (0, usize::MAX) => array.clone(),
        (taken, from) if from <= 64 && (0..from).all(|n| taken >> n & 1 == 1) => {
            Type::Bottom
        }
        (taken, from) => {
            let name = format_compact!("{LENGTHS}{taken}:{from}");
            tag(&ArcStr::from(name.as_str()), [array.clone()])
        }
    }
}

/// A select's walk over a pair of typedef names: the values in both (as
/// a test or not), or those of the first not in the second.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Walk {
    Both(bool),
    Less(bool),
}

/// The walks over pairs of typedef names in progress. Names recur, so a
/// pair met again inside itself is read without walking it: for the
/// values in both, as all of the first, or of the test; for those left,
/// as none, which holds where every pair so read ends with none left.
/// Where one does not, the outermost pair is walked again reading each as
/// all of the first. What a pair inside the outermost gives, where it
/// rests on such a reading, is kept as a name and never as its unfolding,
/// which grows with every pair it passes through. Each reading errs only
/// toward more values.
#[derive(Default)]
pub(super) struct Walks {
    /// Each pair in progress, and whether it was met again.
    open: SmallVec<[(Walk, Type, Type, bool); 8]>,
    /// What each pair finished inside the outermost gave, and whether it
    /// rests on a pair read without a walk.
    done: Vec<(Walk, Type, Type, Type, bool)>,
    /// How many times a pair was read without a walk.
    rested: usize,
    /// A pair read as leaving nothing left something.
    wrong: bool,
    /// A pair met again leaves all of its first type.
    whole: bool,
}

fn plain(t: Type) -> Type {
    lengths(&t)
        .map(|l| l.2)
        .or_else(|| payload_left(&t).map(|p| p.0))
        .or_else(|| elems_left(&t).map(|e| e.0))
        .or_else(|| within_left(&t).map(|w| w.0))
        .unwrap_or(t)
}

fn has(t: &Type, bit: Typ) -> bool {
    matches!(t, Type::Primitive(p) if p.contains(bit))
}

fn nothing(t: &Type) -> bool {
    matches!(t, Type::Bottom) || matches!(t, Type::Primitive(p) if p.is_empty())
}

/// Whether a value of `t` holds something a value could be written into
/// or called: a writable reference or a function, behind any name.
fn holds_taker(env: &Env, t: &Type, seen: &mut SmallVec<[Type; 4]>) -> bool {
    let named = |t: &Type, what: Option<Type>, seen: &mut SmallVec<[Type; 4]>| match seen
        .iter()
        .any(|s| strictly_identical(s, t))
    {
        true => false,
        false => {
            seen.push(t.clone());
            what.is_none_or(|t| holds_taker(env, &t, seen))
        }
    };
    ensure_sufficient(|| match t {
        Type::ByRef(Mutability::Mut, _) | Type::Fn(_) => true,
        Type::Ref(_) => named(t, t.lookup_ref(env).ok(), seen),
        Type::Abstract { id, params } if env.abstract_reps.get(id).is_some() => {
            named(t, rep(env, id, params), seen)
        }
        t => t
            .try_for_each_child(&mut |c| match holds_taker(env, c, seen) {
                true => ControlFlow::Break(()),
                false => ControlFlow::Continue(()),
            })
            .is_break(),
    })
}

/// A select as written: its arms, and where it is. A use of a scheme's
/// variable holds it, so each instance routes its own values down a copy.
#[derive(Debug)]
pub struct Template {
    pub(super) arms: Vec<Arm>,
    at: ArcStr,
}

impl Template {
    pub(super) fn parts(&self) -> (&[Arm], &ArcStr) {
        (&self.arms, &self.at)
    }

    pub(super) fn of_parts(arms: Vec<Arm>, at: ArcStr) -> Self {
        Self { arms, at }
    }

    /// Whether the two are the select written at one place.
    pub(super) fn written_at(&self, other: &Self) -> bool {
        self.at == other.at && self.arms.len() == other.arms.len()
    }

    /// The cells the patterns bind.
    pub(super) fn cells(&self, out: &mut SmallVec<[TVar; 4]>) {
        self.arms.iter().for_each(|a| a.pat.cells(out))
    }
}

pub(super) struct Select {
    pub(super) template: Arc<Template>,
    hit: Vec<bool>,
    pub(super) routed: bool,
    /// A copy made for an instance of a generic definition: an arm
    /// nothing reaches there is no dead arm.
    pub(super) instance: bool,
    pub(super) waiting: Vec<Type>,
    /// What was passed down the arms.
    seen: Vec<Type>,
    /// How each arm is written, for a refusal to name it.
    written: Option<Arc<[ArcStr]>>,
}

/// What one pattern makes of one type: the part it admits, what it
/// leaves, and what its names are bound to.
struct Routed {
    part: Type,
    rest: Type,
    binds: Vec<(TVar, Type)>,
}

/// Whether two lists of an abstract type's parameters are the same
/// types; unknown where one holds a variable, which an instance decides.
fn same_params(env: &Env, ps: &[Type], qs: &[Type]) -> Option<bool> {
    if ps.len() != qs.len() {
        return Some(false);
    }
    let mut known = true;
    for (p, q) in ps.iter().zip(qs.iter()).filter(|(p, q)| !strictly_identical(p, q)) {
        match super::cells_in(p).is_empty() && super::cells_in(q).is_empty() {
            false => known = false,
            true => {
                let holds = |a: &Type, b: &Type| a.contains(env, b).unwrap_or(false);
                if !holds(p, q) || !holds(q, p) {
                    return Some(false);
                }
            }
        }
    }
    known.then_some(true)
}

/// The open cells of `t` a pattern could look into: every one but those
/// of a function's type, which no pattern looks into.
fn undecided(t: &Type, out: &mut SmallVec<[TVar; 4]>) {
    let mut each =
        |ts: &mut dyn Iterator<Item = &Type>| ts.for_each(|t| undecided(t, out));
    match t {
        Type::TVar(tv) if tv.is_rigid() => (),
        Type::TVar(tv) => {
            if !out.iter().any(|u| u.same_cell(tv)) {
                out.push(tv.clone())
            }
        }
        Type::Array(t) | Type::List(t) | Type::Error(t) | Type::ByRef(_, t) => {
            each(&mut std::iter::once(&**t))
        }
        Type::Map(m) => each(&mut [&m.key, &m.value].into_iter()),
        Type::Tuple(ts) | Type::Variant(_, ts, _) | Type::Set(ts) => each(&mut ts.iter()),
        Type::Abstract { params, .. } => each(&mut params.iter()),
        Type::Ref(r) => each(&mut r.params.iter()),
        Type::Struct(fs) => each(&mut fs.iter().map(|(_, t, _)| t)),
        _ => (),
    }
}

/// The cells of `t`, undecided, that `pat` looks into: a name or a
/// wildcard takes whatever is there, decided or not.
fn looked(pat: &Pat, t: &Type, out: &mut SmallVec<[TVar; 4]>) {
    match (pat, t) {
        (Pat::Any(_), _) => (),
        (Pat::Whole(_, p), _) => looked(p, t, out),
        (Pat::Or(ps), _) => ps.iter().for_each(|p| looked(p, t, out)),
        (_, Type::Set(ms)) => ms.iter().for_each(|m| looked(pat, m, out)),
        (Pat::Tuple(_, ps), Type::Tuple(ts)) if ps.len() == ts.len() => {
            ps.iter().zip(ts.iter()).for_each(|(p, t)| looked(p, t, out))
        }
        (Pat::Variant(_, tag, ps), Type::Variant(u, ts, _))
            if tag == u && ps.len() == ts.len() =>
        {
            ps.iter().zip(ts.iter()).for_each(|(p, t)| looked(p, t, out))
        }
        _ => undecided(t, out),
    }
}

impl Solving {
    /// The pattern of an arm that writes the test `t` over the structure
    /// `inner`. A variable the test writes, or `_`, stands for whatever is
    /// there: the test admits anything in its place, and the variable
    /// takes what the arm is given. `whole` is a cell for the arm's whole
    /// value, used where the structure names none.
    pub fn tested(
        &mut self,
        env: &Env,
        t: &Type,
        inner: Pat,
        whole: TVar,
    ) -> Result<Pat> {
        let mut own: SmallVec<[TVar; 4]> = SmallVec::new();
        super::cells_of(t, true, &mut own);
        if let Some(v) = own.iter().find(|v| v.is_rigid()) {
            bail!(
                "a test cannot name a declared type variable ('{}): write `_` there, and the name takes the value's own type",
                v.name
            )
        }
        let t = map_parts(t, &mut |t| matches!(t, Type::Bottom).then_some(Type::Any));
        let test = super::map_cells(&t, &mut |_| Some(Type::Any));
        let whole = match &inner {
            Pat::Any(Some(v)) => v.clone(),
            _ => whole,
        };
        self.constrain(env, &Type::TVar(whole.clone()), &t)?;
        Ok(match (&inner, own.is_empty()) {
            (Pat::Any(Some(_)), _) | (_, true) => Pat::Test(test, Box::new(inner)),
            (_, false) => Pat::Test(test, Box::new(Pat::Whole(whole, Box::new(inner)))),
        })
    }

    /// A select of `arms`, written at `at`: a use to register on its
    /// scrutinee.
    pub fn select(&mut self, arms: Vec<Arm>, at: ArcStr) -> usize {
        let hit = vec![false; arms.len()];
        self.selects.push(Select {
            template: Arc::new(Template { arms, at }),
            hit,
            routed: false,
            instance: false,
            waiting: vec![],
            seen: vec![],
            written: None,
        });
        self.selects.len() - 1
    }

    /// Name the arms of the select `id` as they are written.
    pub fn name_arms(&mut self, id: usize, written: Arc<[ArcStr]>) {
        self.selects[id].written = Some(written)
    }

    /// A copy of the select `of` for an instance of the generic
    /// definition it is in, each cell its patterns bind through `f`.
    pub(super) fn select_copy(
        &mut self,
        of: &Template,
        f: &mut dyn FnMut(&mut Self, &TVar) -> TVar,
    ) -> usize {
        let arms: Vec<Arm> = of
            .arms
            .iter()
            .map(|a| Arm {
                pat: a.pat.map_cells(&mut |v| f(self, v)),
                guarded: a.guarded,
            })
            .collect();
        let new = self.select(arms, of.at.clone());
        self.selects[new].instance = true;
        new
    }

    /// The select `of` as written, each cell its patterns bind through
    /// `f`: the select of a copy of the scheme it is in.
    pub(super) fn template_copy(
        &mut self,
        of: &Template,
        f: &mut dyn FnMut(&mut Self, &TVar) -> TVar,
    ) -> Arc<Template> {
        let arms = of
            .arms
            .iter()
            .map(|a| Arm {
                pat: a.pat.map_cells(&mut |v| f(self, v)),
                guarded: a.guarded,
            })
            .collect();
        Arc::new(Template { arms, at: of.at.clone() })
    }

    /// The cells a select's patterns bind.
    pub(super) fn select_cells(&self, id: usize) -> SmallVec<[TVar; 4]> {
        let mut out = SmallVec::new();
        self.selects[id].template.cells(&mut out);
        out
    }

    /// No value has the type: a product with such a part, at any depth. A
    /// named type is taken to have values.
    fn empty(&self, t: &Type) -> bool {
        match &shallow(t) {
            t if nothing(t) => true,
            Type::Tuple(ts) | Type::Variant(_, ts, _) => ts.iter().any(|t| self.empty(t)),
            Type::Struct(fs) => fs.iter().any(|(_, t, _)| self.empty(t)),
            Type::Set(ms) => ms.iter().all(|t| self.empty(t)),
            _ => false,
        }
    }

    /// `t` with a typedef at its top unfolded; a type with no value is
    /// nothing.
    fn expand(&self, env: &Env, t: &Type) -> Type {
        let t = shallow(t);
        match &t {
            // what a name stands for is read as any type a select meets:
            // its `bool`s are the two tags
            Type::Ref(_) => match t.lookup_ref(env) {
                Ok(e) => ensure_sufficient(|| self.expand(env, &split(&e))),
                Err(_) => t,
            },
            t if self.empty(t) => Type::Bottom,
            _ => t,
        }
    }

    /// What a declared variable is known to be: its bounds, anything with
    /// none.
    fn bound_of(&self, env: &Env, v: &TVar) -> Type {
        v.cell_constraints()
            .iter()
            .filter(|u| {
                open(&shallow(u)).is_none()
                    && !matches!(u, Type::Bound(_))
                    && !u.is_trait_ref(env)
            })
            .fold(Type::Any, |b, u| self.both(env, true, &b, &split(&deep(u))))
    }

    /// The values in both `a` and `b`; where `b` is a `test`, the values
    /// of `a` that pass it, which is more where the test cannot tell.
    pub(super) fn both(&self, env: &Env, test: bool, a: &Type, b: &Type) -> Type {
        self.paired(Walk::Both(test), a, b, || {
            ensure_sufficient(|| self.both_in(env, test, a, b))
        })
    }

    /// The walk `f` of `a` and `b`, once for a pair holding a typedef's
    /// name ([`Walks`]).
    fn paired(&self, walk: Walk, a: &Type, b: &Type, f: impl Fn() -> Type) -> Type {
        // names recur out of step, so a pair with one name is a pair
        let (a, b) = match (shallow(a), shallow(b)) {
            (a, b)
                if (matches!(a, Type::Ref(_)) || matches!(b, Type::Ref(_)))
                    && !strictly_identical(&a, &b) =>
            {
                (a, b)
            }
            _ => return f(),
        };
        let same = |w: Walk, x: &Type, y: &Type| {
            w == walk && strictly_identical(x, &a) && strictly_identical(y, &b)
        };
        // every value the pair could give, by name
        let all = || match walk {
            Walk::Both(true) => b.clone(),
            Walk::Less(_) | Walk::Both(false) => a.clone(),
        };
        let before = {
            let mut walks = self.walks.borrow_mut();
            let whole = walks.whole;
            if let Some(p) = walks.open.iter_mut().find(|p| same(p.0, &p.1, &p.2)) {
                p.3 = true;
                walks.rested += 1;
                return match walk {
                    Walk::Less(_) if !whole => Type::Bottom,
                    _ => all(),
                };
            }
            if let Some(d) = walks.done.iter().find(|d| same(d.0, &d.1, &d.2)) {
                let (left, rests) = (d.3.clone(), d.4);
                walks.rested += rests as usize;
                return left;
            }
            walks.open.push((walk, a.clone(), b.clone(), false));
            walks.rested
        };
        let left = f();
        let mut walks = self.walks.borrow_mut();
        let met = walks.open.pop().is_some_and(|p| p.3);
        let rests = walks.rested != before;
        walks.wrong |=
            met && !walks.whole && matches!(walk, Walk::Less(_)) && !nothing(&left);
        if !walks.open.is_empty() {
            let left = match rests && !nothing(&left) {
                true => all(),
                false => left,
            };
            walks.done.push((walk, a.clone(), b.clone(), left.clone(), rests));
            return left;
        }
        walks.done.clear();
        if !std::mem::take(&mut walks.wrong) {
            return left;
        }
        walks.whole = true;
        walks.open.push((walk, a.clone(), b.clone(), false));
        drop(walks);
        let left = f();
        let mut walks = self.walks.borrow_mut();
        walks.open.pop();
        walks.done.clear();
        walks.whole = false;
        left
    }

    fn both_in(&self, env: &Env, test: bool, a: &Type, b: &Type) -> Type {
        let (a, b) = (plain(self.expand(env, a)), plain(self.expand(env, b)));
        let each = |xs: &[Type], ys: &[Type]| -> Option<Arc<[Type]>> {
            let ts: SmallVec<[Type; 4]> =
                xs.iter().zip(ys).map(|(x, y)| self.both(env, test, x, y)).collect();
            (!ts.iter().any(nothing)).then(|| Arc::from_iter(ts))
        };
        match (&a, &b) {
            _ if strictly_identical(&a, &b) => a,
            (Type::Any, t) | (t, Type::Any) => t.clone(),
            (Type::TVar(v), t) | (t, Type::TVar(v)) => {
                let r = self.both(env, test, &self.bound_of(env, v), t);
                // a variable that holds no error has none in common with
                // anything
                let errors = Type::Primitive(Typ::Error.into());
                match v.cell_constraints().contains(&Type::Bound(Bound::NoError)) {
                    true => self.less(env, false, &r, &errors),
                    false => r,
                }
            }
            (Type::Set(ms), t) => union_of(ms.iter().map(|m| self.both(env, test, m, t))),
            (t, Type::Set(ms)) => union_of(ms.iter().map(|m| self.both(env, test, t, m))),
            // a test tells a reference or a function from what is none,
            // and an abstract type made outside Graphix by its name: the
            // value keeps the type it had
            (Type::ByRef(..), Type::ByRef(..)) | (Type::Fn(_), Type::Fn(_)) if test => a,
            (Type::Abstract { id: x, .. }, Type::Abstract { id: y, .. })
                if test && x == y && env.abstract_reps.get(x).is_none() =>
            {
                a
            }
            (Type::Primitive(p), Type::Primitive(q)) if p.intersects(*q) => {
                Type::Primitive(*p & *q)
            }
            (Type::Error(_), p) | (p, Type::Error(_)) if has(p, Typ::Error) => match &a {
                Type::Error(_) => a,
                _ => b,
            },
            (Type::Array(_), p) | (p, Type::Array(_)) if has(p, Typ::Array) => match &a {
                Type::Array(_) => a,
                _ => b,
            },
            (Type::Array(x), Type::Array(y)) => {
                Type::Array(Arc::new(self.both(env, test, x, y)))
            }
            (Type::List(x), Type::List(y)) => {
                Type::List(Arc::new(self.both(env, test, x, y)))
            }
            (Type::Error(x), Type::Error(y)) => match self.both(env, test, x, y) {
                t if nothing(&t) => Type::Bottom,
                t => Type::Error(Arc::new(t)),
            },
            (Type::Tuple(xs), Type::Tuple(ys)) if xs.len() == ys.len() => {
                each(xs, ys).map(Type::Tuple).unwrap_or(Type::Bottom)
            }
            (Type::Variant(name, xs, at), Type::Variant(..)) if alike(&a, &b) => {
                let Type::Variant(_, ys, _) = &b else { unreachable!() };
                each(xs, ys)
                    .map(|ts| Type::Variant(name.clone(), ts, *at))
                    .unwrap_or(Type::Bottom)
            }
            (Type::Struct(xs), Type::Struct(ys)) if alike(&a, &b) => {
                let fs: SmallVec<[(ArcStr, Type, WrittenAt); 4]> = xs
                    .iter()
                    .zip(ys.iter())
                    .map(|(x, y)| (x.0.clone(), self.both(env, test, &x.1, &y.1), x.2))
                    .collect();
                match fs.iter().any(|(_, t, _)| nothing(t)) {
                    true => Type::Bottom,
                    false => Type::Struct(Arc::from_iter(fs)),
                }
            }
            // a value of an abstract type passes a test of it where its
            // parameters are the test's; one over a variable may
            (
                Type::Abstract { id: x, params: ps },
                Type::Abstract { id: y, params: qs },
            ) if test && x == y => match same_params(env, ps, qs) {
                Some(true) => a,
                Some(false) => Type::Bottom,
                None => b,
            },
            _ => Type::Bottom,
        }
    }

    /// The values of `a` not in `b`; where `b` is a `test`, those that do
    /// not pass it.
    pub(super) fn less(&self, env: &Env, test: bool, a: &Type, b: &Type) -> Type {
        self.paired(Walk::Less(test), a, b, || {
            ensure_sufficient(|| self.less_in(env, test, a, b))
        })
    }

    fn less_in(&self, env: &Env, test: bool, a: &Type, b: &Type) -> Type {
        let (a, b) = (plain(self.expand(env, a)), plain(self.expand(env, b)));
        // a product less a product: the products that differ from it in a
        // first component
        let product = |xs: &[Type], ys: &[Type], wrap: &dyn Fn(Arc<[Type]>) -> Type| {
            let mut out: SmallVec<[Type; 4]> = SmallVec::new();
            for i in 0..xs.len() {
                let left = self.less(env, test, &xs[i], &ys[i]);
                if !nothing(&left) {
                    let mut ts: SmallVec<[Type; 4]> = xs[..i]
                        .iter()
                        .zip(ys)
                        .map(|(x, y)| self.both(env, test, x, y))
                        .collect();
                    ts.push(left);
                    ts.extend(xs[i + 1..].iter().cloned());
                    if !ts.iter().any(nothing) {
                        out.push(wrap(Arc::from_iter(ts)))
                    }
                }
            }
            union_of(out)
        };
        match (&a, &b) {
            _ if strictly_identical(&a, &b) => Type::Bottom,
            (_, Type::Any) | (Type::Bottom, _) => Type::Bottom,
            (Type::TVar(v), _) => self.less(env, test, &self.bound_of(env, v), &b),
            (Type::Set(ms), _) => {
                union_of(ms.iter().map(|m| self.less(env, test, m, &b)))
            }
            (_, Type::Set(ms)) => {
                ms.iter().fold(a.clone(), |a, m| self.less(env, test, &a, m))
            }
            (Type::ByRef(..), Type::ByRef(..)) | (Type::Fn(_), Type::Fn(_)) if test => {
                Type::Bottom
            }
            (Type::Abstract { id: x, .. }, Type::Abstract { id: y, .. })
                if test && x == y && env.abstract_reps.get(x).is_none() =>
            {
                Type::Bottom
            }
            (Type::Primitive(p), Type::Primitive(q)) => match *p & !*q {
                left if left.is_empty() => Type::Bottom,
                left => Type::Primitive(left),
            },
            (Type::Error(_), p) if has(p, Typ::Error) => Type::Bottom,
            (Type::Array(_), p) if has(p, Typ::Array) => Type::Bottom,
            (Type::Array(x), Type::Array(y)) | (Type::List(x), Type::List(y))
                if nothing(&self.less(env, test, x, y)) =>
            {
                Type::Bottom
            }
            (
                Type::Abstract { id: x, params: ps },
                Type::Abstract { id: y, params: qs },
            ) if test && x == y && same_params(env, ps, qs) == Some(true) => Type::Bottom,
            // an error holds one value: what is left of it is an error
            // of what is left of that
            (Type::Error(x), Type::Error(y)) => match self.less(env, test, x, y) {
                left if nothing(&left) => Type::Bottom,
                left => Type::Error(Arc::new(left)),
            },
            (Type::Tuple(xs), Type::Tuple(ys)) if xs.len() == ys.len() => {
                product(xs, ys, &Type::Tuple)
            }
            (Type::Variant(name, xs, at), Type::Variant(..)) if alike(&a, &b) => {
                let Type::Variant(_, ys, _) = &b else { unreachable!() };
                product(xs, ys, &|ts| Type::Variant(name.clone(), ts, *at))
            }
            (Type::Struct(fs), Type::Struct(gs)) if alike(&a, &b) => {
                let (xs, ys): (SmallVec<[Type; 4]>, SmallVec<[Type; 4]>) = fs
                    .iter()
                    .zip(gs.iter())
                    .map(|(x, y)| (x.1.clone(), y.1.clone()))
                    .unzip();
                product(&xs, &ys, &|ts| {
                    let fields = fs.iter().zip(ts.iter());
                    Type::Struct(Arc::from_iter(
                        fields.map(|(f, t)| (f.0.clone(), t.clone(), f.2)),
                    ))
                })
            }
            _ => a,
        }
    }

    fn route(&self, env: &Env, pat: &Pat, t: &Type) -> Routed {
        ensure_sufficient(|| self.route_in(env, pat, t))
    }

    fn route_in(&self, env: &Env, pat: &Pat, t: &Type) -> Routed {
        let t = self.expand(env, t);
        if let Pat::Whole(v, p) = pat {
            let mut r = self.route(env, p, &t);
            if !nothing(&r.part) {
                r.binds.push((v.clone(), r.part.clone()))
            }
            return r;
        }
        // a pattern that looks into a declared variable sees its bound,
        // or what the arms before left of it; what it leaves is still
        // the variable, for an arm that names it
        let within = within_left(&t).or_else(|| match &t {
            Type::TVar(v) => Some((t.clone(), self.bound_of(env, v))),
            _ => None,
        });
        if let (Some((var, left)), false) = (within, matches!(pat, Pat::Any(_))) {
            let mut r = self.route(env, pat, &left);
            // a variable that holds no error gives a pattern none
            let no_error = matches!(&var, Type::TVar(v)
                if v.cell_constraints().contains(&Type::Bound(Bound::NoError)));
            if no_error {
                let errors = Type::Primitive(Typ::Error.into());
                r.part = self.less(env, false, &r.part, &errors);
                if nothing(&r.part) {
                    return Routed { part: Type::Bottom, rest: t.clone(), binds: vec![] };
                }
            }
            let rest = match nothing(&r.rest) {
                true => Type::Bottom,
                false => tag(&ArcStr::from(WITHIN), [var, r.rest.clone()]),
            };
            return Routed { rest, ..r };
        }
        // a value over a declared constructor may be any implementor's:
        // a pattern that looks into it sees each, and takes nothing from
        // what is left, since another implementor may come
        if let Type::App(head, arg) = &t
            && let Type::TVar(c) = &**head
            && c.is_rigid()
            && !matches!(pat, Pat::Any(_))
        {
            let bounds = c.cell_constraints();
            let traits = bounds.iter().filter_map(|b| match b {
                Type::Ref(tr) => env.trait_of_ref(tr),
                _ => None,
            });
            let impls = traits.filter_map(|id| env.impls_of(id));
            let members: Vec<Type> = impls
                .flat_map(|l| {
                    l.iter().filter_map(|im| im.target.fill_hole(arg)).collect::<Vec<_>>()
                })
                .collect();
            let r = self.route(env, pat, &split(&union_of(members)));
            return Routed { rest: t.clone(), ..r };
        }
        if let Type::Set(ms) = &t {
            let (mut parts, mut rests, mut binds) = (vec![], vec![], vec![]);
            for m in ms.iter() {
                let r = self.route(env, pat, m);
                parts.push(r.part);
                rests.push(r.rest);
                binds.extend(r.binds)
            }
            return Routed { part: union_of(parts), rest: union_of(rests), binds };
        }
        let named = |v: &Option<TVar>, part: &Type, mut binds: Vec<(TVar, Type)>| {
            if let Some(v) = v {
                binds.push((v.clone(), part.clone()))
            }
            binds
        };
        let none = || Routed { part: Type::Bottom, rest: t.clone(), binds: vec![] };
        // each sub-pattern over its component; the part is a product, the
        // rest the products that fail in a first component
        let product = |all: &Option<TVar>,
                       ps: &[&Pat],
                       xs: &[Type],
                       wrap: &dyn Fn(Vec<Type>) -> Type| {
            let rs: Vec<Routed> =
                ps.iter().zip(xs).map(|(p, x)| self.route(env, p, x)).collect();
            let mut rests = vec![];
            for (i, r) in rs.iter().enumerate() {
                if !nothing(&r.rest) {
                    let mut ts: Vec<Type> =
                        rs[..i].iter().map(|r| r.part.clone()).collect();
                    ts.push(r.rest.clone());
                    ts.extend(xs[i + 1..].iter().cloned());
                    if !ts.iter().any(nothing) {
                        rests.push(wrap(ts))
                    }
                }
            }
            match rs.iter().any(|r| nothing(&r.part)) {
                true => none(),
                false => {
                    let part = wrap(rs.iter().map(|r| r.part.clone()).collect());
                    let binds = rs.into_iter().flat_map(|r| r.binds).collect();
                    Routed {
                        binds: named(all, &part, binds),
                        part,
                        rest: union_of(rests),
                    }
                }
            }
        };
        let any = |n: usize| vec![Type::Any; n];
        match (pat, &t) {
            (_, t) if nothing(t) => none(),
            (Pat::Any(v), _) => Routed {
                part: t.clone(),
                rest: Type::Bottom,
                binds: named(v, &t, vec![]),
            },
            (Pat::Test(test, p), _) => {
                let test = split(test);
                let r = self.route(env, p, &self.both(env, true, &t, &test));
                let rest = union_of([self.less(env, true, &t, &test), r.rest.clone()]);
                Routed { rest, ..r }
            }
            (Pat::Lit(lit, whole), _) => {
                let rest = match whole {
                    true => self.less(env, true, &t, lit),
                    false => t.clone(),
                };
                Routed { part: self.both(env, true, &t, lit), rest, binds: vec![] }
            }
            (Pat::Abs(_, id, p), Type::Any) => match env.abstract_reps.get(id) {
                Some(made) => {
                    // which instance it is is not known, so it is read at
                    // `Any` parameters
                    let params: Arc<[Type]> =
                        Arc::from_iter(made.params.iter().map(|_| Type::Any));
                    match blind(env, id, p).is_some() {
                        true => none(),
                        false => {
                            let whole = Type::Abstract { id: *id, params };
                            let r = self.route(env, pat, &whole);
                            Routed { rest: Type::Any, ..r }
                        }
                    }
                }
                None => none(),
            },
            // what the arms before left of an abstract type's payload,
            // or all of it
            (Pat::Abs(all, id, p), _)
                if let Some((whole, payload)) =
                    payload_left(&t).or_else(|| match &t {
                        Type::Abstract { id: have, params } => {
                            Some((t.clone(), rep(env, have, params)?))
                        }
                        _ => None,
                    })
                    && matches!(&whole, Type::Abstract { id: have, .. } if have == id) =>
            {
                let r = self.route(env, p, &payload);
                match nothing(&r.part) {
                    true => none(),
                    false => Routed {
                        part: whole.clone(),
                        rest: match nothing(&r.rest) {
                            true => Type::Bottom,
                            false => tag(
                                &ArcStr::from(PAYLOAD),
                                [whole.clone(), r.rest.clone()],
                            ),
                        },
                        binds: named(all, &whole, r.binds),
                    },
                }
            }
            (Pat::Or(ps), _) => {
                let (mut reach, mut parts, mut binds) = (t.clone(), vec![], vec![]);
                for p in ps {
                    let r = self.route(env, p, &reach);
                    parts.push(r.part);
                    binds.extend(r.binds);
                    reach = r.rest
                }
                Routed { part: union_of(parts), rest: reach, binds }
            }
            // the arrays of one length the arms before left: a pattern
            // of that length, or a shorter one with a run, takes them as
            // the tuple of their elements
            (Pat::Elems { all, list, elems: ps, open: run }, _)
                if let Some((array, n, tuples)) = elems_left(&t) =>
            {
                let kind = matches!(&array, Type::List(_)) == *list;
                let fits = match run {
                    None => ps.len() == n,
                    Some(_) => ps.len() <= n,
                };
                if !kind || !fits {
                    return none();
                }
                let mut ps = ps.clone();
                ps.resize(n, Pat::Any(None));
                let r = self.route(env, &Pat::Tuple(None, ps), &tuples);
                if nothing(&r.part) {
                    return none();
                }
                let mut binds = r.binds;
                if let Some(Some(v)) = run {
                    binds.push((v.clone(), array.clone()))
                }
                Routed {
                    binds: named(all, &array, binds),
                    rest: elems(&array, n, r.rest),
                    part: array,
                }
            }
            (Pat::Elems { all, list, elems, open: run }, _)
                if matches!(plain(t.clone()), Type::Array(_) | Type::List(_)) =>
            {
                // a pattern takes the arrays of its length, or of its
                // length and longer where it leaves a run open
                let (taken, from, array) =
                    lengths(&t).unwrap_or((0, usize::MAX, t.clone()));
                let e = match &array {
                    Type::Array(e) if !*list => e.clone(),
                    Type::List(e) if *list => e.clone(),
                    _ => return none(),
                };
                let n = elems.len();
                let gone = n >= from || (run.is_none() && n < 64 && taken >> n & 1 == 1);
                let rs: Vec<Routed> =
                    elems.iter().map(|p| self.route(env, p, &e)).collect();
                if gone || rs.iter().any(|r| nothing(&r.part)) {
                    return none();
                }
                let whole = rs.iter().all(|r| nothing(&r.rest));
                let mut binds: Vec<(TVar, Type)> =
                    rs.into_iter().flat_map(|r| r.binds).collect();
                if let Some(Some(v)) = run {
                    binds.push((v.clone(), array.clone()))
                }
                // an array of nothing is the empty array alone
                let empty = nothing(&plain(Type::clone(&e)));
                let rest = match (whole, run) {
                    _ if empty => Type::Bottom,
                    // the arrays of this length it does not take are
                    // kept as the tuples of their elements, for the arms
                    // after to take
                    (false, None) if n < 64 => {
                        let each =
                            Type::Tuple(Arc::from_iter((0..n).map(|_| (*e).clone())));
                        let tuple = Pat::Tuple(None, elems.clone());
                        let rest = self.route(env, &tuple, &each).rest;
                        union_of([
                            left(&array, taken | 1 << n, from),
                            self::elems(&array, n, rest),
                        ])
                    }
                    (false, _) => t.clone(),
                    (true, Some(_)) => left(&array, taken, n.min(from)),
                    (true, None) if n < 64 => left(&array, taken | 1 << n, from),
                    (true, None) => t.clone(),
                };
                Routed { binds: named(all, &array, binds), part: array, rest }
            }
            (Pat::Elems { list, .. }, Type::Any) => {
                let e = Arc::new(Type::Any);
                let r = self.route(
                    env,
                    pat,
                    &if *list { Type::List(e) } else { Type::Array(e) },
                );
                Routed { rest: Type::Any, ..r }
            }
            (_, _) if lengths(&t).is_some() && !matches!(pat, Pat::Or(_)) => {
                self.route(env, pat, &plain(t.clone()))
            }
            (Pat::Tuple(all, ps), Type::Tuple(xs)) if ps.len() == xs.len() => {
                product(all, &ps.iter().collect::<Vec<_>>(), xs, &|ts| {
                    Type::Tuple(Arc::from(ts))
                })
            }
            (Pat::Tuple(all, ps), Type::Any) => {
                let r =
                    product(all, &ps.iter().collect::<Vec<_>>(), &any(ps.len()), &|ts| {
                        Type::Tuple(Arc::from(ts))
                    });
                Routed { rest: Type::Any, ..r }
            }
            (Pat::Variant(all, name, ps), Type::Variant(have, xs, at))
                if name == have && ps.len() == xs.len() =>
            {
                product(all, &ps.iter().collect::<Vec<_>>(), xs, &|ts| {
                    Type::Variant(name.clone(), Arc::from(ts), *at)
                })
            }
            (Pat::Variant(all, name, ps), Type::Any) => {
                let r =
                    product(all, &ps.iter().collect::<Vec<_>>(), &any(ps.len()), &|ts| {
                        tag(name, ts)
                    });
                Routed { rest: Type::Any, ..r }
            }
            (Pat::Struct(all, fs, exhaustive), Type::Struct(xs))
                if fs.iter().all(|(n, _)| xs.iter().any(|(m, ..)| m == n))
                    && (!exhaustive || fs.len() == xs.len()) =>
            {
                let ignore = Pat::Any(None);
                let ps: Vec<&Pat> = xs
                    .iter()
                    .map(|(n, ..)| {
                        fs.iter().find(|(m, _)| m == n).map(|(_, p)| p).unwrap_or(&ignore)
                    })
                    .collect();
                let ts: Vec<Type> = xs.iter().map(|(_, t, _)| t.clone()).collect();
                product(all, &ps, &ts, &|ts| {
                    let fields = xs.iter().zip(ts);
                    Type::Struct(Arc::from_iter(
                        fields.map(|(x, t)| (x.0.clone(), t, x.2)),
                    ))
                })
            }
            _ => none(),
        }
    }

    /// Pass one type that reaches a select's scrutinee down its arms.
    pub(super) fn select_route(&mut self, env: &Env, id: usize, t: &Type) -> Result<()> {
        let whole = deep(t);
        if !self.looked(id, &whole).is_empty() {
            self.selects[id].waiting.push(t.clone());
            return Ok(());
        }
        let mut reach = self.expand(env, &split(&whole));
        if nothing(&reach) {
            return Ok(());
        }
        let reached = unsplit(&reach);
        let (mut binds, mut hits) = (vec![], vec![]);
        for (i, arm) in self.selects[id].template.clone().arms.iter().enumerate() {
            let r = self.route(env, &arm.pat, &reach);
            if !nothing(&r.part) {
                hits.push(i);
                binds.extend(r.binds)
            }
            if !arm.guarded {
                reach = r.rest
            }
        }
        if !nothing(&reach) {
            bail!(
                "missing match cases: nothing matches {} ({})",
                unsplit(&reach),
                self.selects[id].template.at
            )
        }
        self.selects[id].routed = true;
        self.selects[id].seen.push(reached);
        hits.into_iter().for_each(|i| self.selects[id].hit[i] = true);
        // a name takes every part routed to it, as one type
        let mut names: Vec<(TVar, Vec<Type>)> = vec![];
        for (v, t) in binds {
            match names.iter_mut().find(|(n, _)| n.same_cell(&v)) {
                Some((_, ts)) => ts.push(t),
                None => names.push((v, vec![t])),
            }
        }
        for (v, ts) in names {
            let t = self.absorbed(env, &unsplit(&union_of(ts)));
            self.constrain(env, &t, &Type::TVar(v))?
        }
        Ok(())
    }

    /// A decided union less each member another member holds: the part of
    /// `Array<string>` an `Array<i64>` test takes is the empty array, which
    /// `Array<i64>` holds.
    fn absorbed(&mut self, env: &Env, t: &Type) -> Type {
        let Type::Set(ms) = t else { return t.clone() };
        if t.has_free() {
            return t.clone();
        }
        let mut kept: Vec<Type> = vec![];
        for m in ms.iter() {
            let held =
                |s: &mut Self, m: &Type, by: &Type| alike(m, by) && s.attempt(env, m, by);
            if kept.iter().any(|k| held(self, m, k)) {
                continue;
            }
            kept.retain(|k| !held(self, k, m));
            kept.push(m.clone())
        }
        union_of(kept)
    }

    /// The undecided cells of `t` the arms of the select `id` look into.
    fn looked(&self, id: usize, t: &Type) -> SmallVec<[TVar; 4]> {
        let mut out = SmallVec::new();
        self.selects[id].template.arms.iter().for_each(|a| looked(&a.pat, t, &mut out));
        out
    }

    /// Route what waited for a cell; true if anything was.
    pub(super) fn select_waiting(
        &mut self,
        env: &Env,
        ready: &dyn Fn(&Self, usize, &Type) -> bool,
    ) -> Result<bool> {
        let mut progress = false;
        for id in 0..self.selects.len() {
            for t in std::mem::take(&mut self.selects[id].waiting) {
                if ready(self, id, &t) {
                    // what an arm looks into is fixed; what it only names
                    // stays open for the name's uses to reach
                    loop {
                        let cells = self.looked(id, &deep(&t));
                        if cells.is_empty() {
                            break;
                        }
                        cells.iter().try_for_each(|v| self.fix(env, v))?
                    }
                    self.select_route(env, id, &t)?;
                    progress = true
                } else {
                    self.selects[id].waiting.push(t)
                }
            }
        }
        Ok(progress)
    }

    /// An arm of a select that was judged and that nothing reaches.
    pub(super) fn dead_arms(&self, env: &Env) -> Result<()> {
        // a select still waiting is one of a generic definition, whose
        // instances give it its values
        let judged = |s: &&Select| s.routed && !s.instance && s.waiting.is_empty();
        for s in self.selects.iter().filter(judged) {
            if let Some(i) = s.hit.iter().position(|h| !h) {
                let seen: Vec<String> = s.seen.iter().map(|t| t.to_string()).collect();
                let (at, seen) = (&s.template.at, seen.join(" | "));
                let arm = match s.written.as_ref().and_then(|w| w.get(i)) {
                    Some(w) => format_compact!("{w}"),
                    None => format_compact!("{}", i + 1),
                };
                if let Some(name) = s.template.arms[i].pat.blind(env) {
                    bail!(
                        "pattern {arm} takes nothing: {name}(..) names part of a payload \
                         something could be written into or called, which it may only \
                         where the value is known to be one instance of {name}; write \
                         the instance in a type test, `{name}<T> as {name}(..)` ({at}; \
                         the value is {seen})"
                    )
                }
                bail!(
                    "pattern {arm} will never match: nothing reaches it ({at}; the value \
                     is {seen})"
                )
            }
        }
        Ok(())
    }

    /// Have the select `id` wait on `t`.
    pub(super) fn select_wait(&mut self, id: usize, t: Type) {
        self.selects[id].waiting.push(t)
    }
}
