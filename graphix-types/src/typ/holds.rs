//! The relation over decided types: whether every value of one type is a
//! value of another. It decides nothing: the check by bounds decides
//! types, and this answers what the solver asks of types it has decided
//! and what the build asks of the types the check gave it.
//!
//! An undecided cell is one of three things here. A cell the check by
//! bounds left open (a scheme's variable) holds and is held by anything,
//! but where the walk asks for one type (an abstract type's parameters, a
//! writable reference's referent): there it is itself alone. A declared
//! variable a definition's check holds rigid is itself alone: it holds
//! itself and ⊥, and is held by `Any`, a union holding it, or what holds
//! one of its bounds. Any other cell stands for a type yet to be told: it
//! holds a decided type its bounds admit, and any type holds it. Two cells
//! hold each other but where both are themselves alone: then only through
//! their bounds.

use super::{Bound, FnType, Mutability, TVar, Type, node_addr};
use crate::{env::Env, stack::ensure_sufficient};
use ahash::AHashMap;
use anyhow::Result;
use netidx_value::Typ;
use poolshark::local::LPooled;
use smallvec::SmallVec;
use std::borrow::Cow;

/// [`holds`] of two function types.
pub fn holds_fn(env: &Env, a: &FnType, b: &FnType) -> Result<bool> {
    Holds::new(env).function(a, b)
}

/// [`holds`] over a fixed signature's variables: one meets a predicate
/// only where its own bounds imply it.
pub fn holds_sealed(env: &Env, a: &Type, b: &Type) -> Result<bool> {
    let mut h = Holds::new(env);
    h.sealed = true;
    h.go(a, b)
}

/// `t`'s structural identity, its cells by their own: two types with one
/// identity are one type.
pub(crate) fn identity(env: &Env, t: &Type) -> u128 {
    Holds::new(env).ident(t)
}

/// Whether every value of `b` is a value of `a`.
pub fn holds(env: &Env, a: &Type, b: &Type) -> Result<bool> {
    Holds::new(env).go(a, b)
}

/// What a failed alternative takes back ([`Holds::attempt`]).
enum Undo {
    Memo((usize, usize, bool)),
    Name((Side, Side)),
}

/// One side of a pair of names: a type's identity ([`Holds::ident`]).
type Side = u128;

/// No assumption relied on.
const NONE: usize = usize::MAX;

/// A pair of names that does not hold, which no assumption changes.
const REFUSED: usize = usize::MAX - 1;

struct Holds<'e> {
    env: &'e Env,
    /// Pairs of names taken to hold, whose expansions are new nodes each
    /// time: the pair under a walk, by its depth, and each one a walk
    /// found to hold, by the outermost assumption it relied on.
    names: LPooled<AHashMap<(Side, Side), usize>>,
    /// Each node's identity, once.
    idents: LPooled<AHashMap<usize, Side>>,
    /// Answers by pair of nodes, the nodes kept so no other takes their
    /// address, a pair that held with the outermost assumption it relied
    /// on. A refusal stands whatever was assumed.
    memo: LPooled<AHashMap<(usize, usize, bool), (bool, usize)>>,
    /// The types this walk made (a union's combinations), kept so no other
    /// takes their nodes' addresses.
    keep: LPooled<Vec<Type>>,
    /// The answers and names that rely on an assumption, in order: an
    /// alternative that fails takes back what it took.
    trail: LPooled<Vec<Undo>>,
    /// The depth of the walk.
    depth: usize,
    /// The outermost assumption the walk so far relied on ([`NONE`]).
    relied: usize,
    /// The walk asks for one type, not a holder ([`Self::equal`]).
    invariant: bool,
    /// A declared variable meets a predicate only where its own bounds
    /// imply it: the fixed signature others read ([`holds_sealed`]).
    sealed: bool,
    /// Each name's expansion, once: one node for every meeting of it.
    expansions: LPooled<AHashMap<Side, Option<Type>>>,
}

/// `t` with the bindings of its top cells followed.
fn decided(t: &Type) -> Cow<'_, Type> {
    let Type::TVar(tv) = t else { return Cow::Borrowed(t) };
    let Some(mut b) = tv.binding() else { return Cow::Borrowed(t) };
    loop {
        match &b {
            Type::TVar(tv) => match tv.binding() {
                Some(next) => b = next,
                None => return Cow::Owned(b),
            },
            _ => return Cow::Owned(b),
        }
    }
}

enum Cell {
    /// Left open by the check by bounds.
    Made,
    /// A declared variable held rigid: one type.
    Rigid,
    /// Yet to be told.
    Open,
}

impl<'e> Holds<'e> {
    fn new(env: &'e Env) -> Self {
        Holds {
            env,
            names: LPooled::take(),
            memo: LPooled::take(),
            trail: LPooled::take(),
            depth: 0,
            relied: NONE,
            invariant: false,
            sealed: false,
            expansions: LPooled::take(),
            idents: LPooled::take(),
            keep: LPooled::take(),
        }
    }
    fn go(&mut self, a: &Type, b: &Type) -> Result<bool> {
        ensure_sufficient(|| {
            let (a, b) = (decided(a), decided(b));
            let (a, b) = (&*a, &*b);
            let key =
                node_addr(&a).zip(node_addr(&b)).map(|(x, y)| (x, y, self.invariant));
            if let Some(k) = key {
                // one node holds itself, but for a name whose arguments
                // its parameters' bounds refuse, which is no type
                if k.0 == k.1 && !matches!(a, Type::Ref(_)) {
                    return Ok(true);
                }
                // one type, written twice, holds itself
                if !matches!(a, Type::Ref(_)) && self.ident(a) == self.ident(b) {
                    return Ok(true);
                }
                if let Some((r, relied)) = self.memo.get(&k) {
                    self.relied = self.relied.min(*relied);
                    return Ok(*r);
                }
            }
            self.depth += 1;
            let depth = self.depth;
            let outer = std::mem::replace(&mut self.relied, NONE);
            let r = self.step(&a, &b);
            // what this walk assumed is discharged by its answer
            let relied = match self.relied {
                d if d >= depth => NONE,
                d => d,
            };
            self.relied = outer.min(relied);
            self.depth -= 1;
            if let Some(k) = key {
                match &r {
                    Ok(false) => {
                        self.memo.insert(k, (false, NONE));
                    }
                    Ok(true) => {
                        self.memo.insert(k, (true, relied));
                        if relied != NONE {
                            self.trail.push(Undo::Memo(k));
                        }
                    }
                    Err(_) => (),
                }
            }
            r
        })
    }

    /// One alternative of several: where it does not hold, what it took
    /// as holding is taken back.
    fn attempt(&mut self, a: &Type, b: &Type) -> Result<bool> {
        let mark = self.trail.len();
        let r = self.go(a, b)?;
        if !r {
            for undo in self.trail.drain(mark..) {
                match undo {
                    Undo::Memo(k) => {
                        if self.memo.get(&k).is_some_and(|(r, _)| *r) {
                            self.memo.remove(&k);
                        }
                    }
                    Undo::Name(n) => {
                        self.names.remove(&n);
                    }
                }
            }
        }
        Ok(r)
    }

    fn all(&mut self, a: &Type, bs: &[Type]) -> Result<bool> {
        for b in bs {
            if !self.go(a, b)? {
                return Ok(false);
            }
        }
        Ok(true)
    }

    fn each(&mut self, a: &[Type], b: &[Type]) -> Result<bool> {
        if a.len() != b.len() {
            return Ok(false);
        }
        for (a, b) in a.iter().zip(b.iter()) {
            if !self.go(a, b)? {
                return Ok(false);
            }
        }
        Ok(true)
    }

    /// Whether `a` and `b` are one type: a scheme's variable is itself
    /// alone here.
    fn equal(&mut self, a: &Type, b: &Type) -> Result<bool> {
        let outer = std::mem::replace(&mut self.invariant, true);
        let r = (|| Ok(self.go(a, b)? && self.go(b, a)?))();
        self.invariant = outer;
        r
    }

    fn cell(&self, tv: &TVar) -> Cell {
        if tv.is_rigid() {
            Cell::Rigid
        } else if super::bounds::made(&Type::TVar(tv.clone())) {
            match self.invariant {
                true => Cell::Rigid,
                false => Cell::Made,
            }
        } else {
            Cell::Open
        }
    }

    fn step(&mut self, a: &Type, b: &Type) -> Result<bool> {
        use Type as T;
        // a trait in type position is a predicate
        if let T::Ref(tr) = a
            && let Some(tid) = self.env.trait_of_ref(tr)
        {
            return Type::implements(tid, self.env, b);
        }
        if let T::Ref(tr) = b
            && let Some(tid) = self.env.trait_of_ref(tr)
        {
            return Ok(match a {
                T::Any => true,
                T::Ref(tr0) => self.env.trait_of_ref(tr0) == Some(tid),
                _ => false,
            });
        }
        match (a, b) {
            (T::TVar(x), T::TVar(y)) if x.same_cell(y) => Ok(true),
            // two cells hold each other only through their bounds
            // two cells hold each other unless both are themselves alone,
            // when only their bounds tell
            (T::TVar(x), T::TVar(y)) => match (self.cell(x), self.cell(y)) {
                (Cell::Rigid, Cell::Rigid) => self.by_bounds(a, y),
                // this one's bounds must admit the other's: what it holds
                // at least (its lower bounds), and what it may be at most
                // where it says (one of its own bounds)
                _ => {
                    let (lower, upper) = (y.cell_lower(), y.cell_constraints());
                    for c in x.cell_constraints().iter() {
                        for l in lower.iter() {
                            if !self.go(c, l)? {
                                return Ok(false);
                            }
                        }
                        if !upper.is_empty() {
                            let mut held = false;
                            for u in upper.iter() {
                                if self.attempt(c, u)? {
                                    held = true;
                                    break;
                                }
                            }
                            if !held {
                                return Ok(false);
                            }
                        }
                    }
                    Ok(true)
                }
            },
            (_, T::Bottom) => Ok(true),
            (T::Bound(p), t) => self.predicate(*p, t),
            (_, T::Bound(_)) => Ok(false),
            (T::Hole, T::Hole) => Ok(true),
            (T::Hole, _) | (_, T::Hole) => Ok(false),
            (T::TVar(x), b) => match self.cell(x) {
                Cell::Made => Ok(true),
                Cell::Rigid => match b {
                    T::Set(ms) => self.all(a, ms),
                    T::Ref(_) => self.expanded(a, b),
                    _ => Ok(false),
                },
                Cell::Open => {
                    for c in x.cell_constraints().iter() {
                        if !self.go(c, b)? {
                            return Ok(false);
                        }
                    }
                    Ok(true)
                }
            },
            (a, T::TVar(y)) => match self.cell(y) {
                Cell::Made | Cell::Open => Ok(true),
                Cell::Rigid => match a {
                    T::Any => Ok(true),
                    T::Ref(_) => self.expanded(a, b),
                    T::Set(ms) => {
                        for m in ms.iter() {
                            if self.attempt(m, b)? {
                                return Ok(true);
                            }
                        }
                        self.by_bounds(a, y)
                    }
                    _ => self.by_bounds(a, y),
                },
            },
            (T::Ref(_), _) | (_, T::Ref(_)) => self.expanded(a, b),
            (T::App(c, x), b) | (b, T::App(c, x)) if Type::app_filled(c, x).is_some() => {
                let filled = Type::app_filled(c, x).expect("filled");
                match matches!(a, T::App(..)) {
                    true => self.go(&filled, b),
                    false => self.go(b, &filled),
                }
            }
            (T::App(c0, x0), T::App(c1, x1)) => {
                Ok(self.equal(c0, c1)? && self.equal(x0, x1)?)
            }
            (T::App(..), _) | (_, T::App(..)) => Ok(false),
            (T::Bottom, T::Set(ms)) => self.all(a, ms),
            (T::Bottom, _) => Ok(false),
            (T::Any, _) => Ok(true),
            (T::Set(ms), T::Any) => self.member_holds(ms, b),
            (_, T::Any) => Ok(false),
            (_, T::Set(ms)) => self.all(a, ms),
            (T::Set(ms), b) => self.member_holds(ms, b),
            (T::Primitive(p0), T::Primitive(p1)) => Ok(p0.contains(*p1)),
            (T::Primitive(p), T::Array(_)) => Ok(p.contains(Typ::Array)),
            (T::Primitive(p), T::Map(_)) => Ok(p.contains(Typ::Map)),
            (T::Primitive(p), T::Error(_)) => Ok(p.contains(Typ::Error)),
            (T::Array(e), T::Primitive(p)) if *p == Typ::Array => self.go(e, &T::Any),
            (T::Map(m), T::Primitive(p)) if *p == Typ::Map => {
                Ok(self.go(&m.key, &T::Any)? && self.go(&m.value, &T::Any)?)
            }
            (T::Error(e), T::Primitive(p)) if *p == Typ::Error => self.go(e, &T::Any),
            (T::Array(x), T::Array(y))
            | (T::List(x), T::List(y))
            | (T::Error(x), T::Error(y)) => self.go(x, y),
            (T::Map(m0), T::Map(m1)) => {
                Ok(self.go(&m0.key, &m1.key)? && self.go(&m0.value, &m1.value)?)
            }
            (T::Tuple(x), T::Tuple(y)) => self.each(x, y),
            (T::Struct(x), T::Struct(y)) => {
                if x.len() != y.len() || x.iter().zip(y.iter()).any(|(f, g)| f.0 != g.0) {
                    return Ok(false);
                }
                for ((_, x, _), (_, y, _)) in x.iter().zip(y.iter()) {
                    if !self.go(x, y)? {
                        return Ok(false);
                    }
                }
                Ok(true)
            }
            (T::Variant(t0, x, _), T::Variant(t1, y, _)) => {
                Ok(t0 == t1 && self.each(x, y)?)
            }
            (T::ByRef(Mutability::Shared, x), T::ByRef(_, y)) => self.go(x, y),
            (T::ByRef(Mutability::Mut, x), T::ByRef(Mutability::Mut, y)) => {
                self.equal(x, y)
            }
            (T::Abstract { id: i0, params: p0 }, T::Abstract { id: i1, params: p1 }) => {
                if i0 != i1 || p0.len() != p1.len() {
                    return Ok(false);
                }
                for (x, y) in p0.iter().zip(p1.iter()) {
                    if !self.equal(x, y)? {
                        return Ok(false);
                    }
                }
                Ok(true)
            }
            (T::Fn(f0), T::Fn(f1)) => self.function(f0, f1),
            _ => Ok(false),
        }
    }

    /// A union holds a type that one of its members holds; a primitive
    /// type of several primitives, each by a member.
    fn member_holds(&mut self, ms: &[Type], b: &Type) -> Result<bool> {
        for m in ms.iter() {
            if self.attempt(m, b)? {
                return Ok(true);
            }
        }
        if let Type::Primitive(p) = b
            && p.len() > 1
        {
            for bit in p.iter() {
                let one = Type::Primitive(bit.into());
                if !self.member_holds(ms, &one)? {
                    return Ok(false);
                }
            }
            return Ok(true);
        }
        self.distributed(ms, b)
    }

    /// A union holds a tuple, struct or variant whose parts are unions
    /// where it holds each one their members make: `[(`A, `C), (`B, `C)]`
    /// holds `([`A, `B], `C)`.
    fn distributed(&mut self, ms: &[Type], b: &Type) -> Result<bool> {
        const MOST: usize = 256;
        let parts: &[Type] = match b {
            Type::Tuple(ts) | Type::Variant(_, ts, _) => ts,
            Type::Struct(_) => &[],
            _ => return Ok(false),
        };
        let fields: SmallVec<[Type; 8]> = match b {
            Type::Struct(fs) => fs.iter().map(|(_, t, _)| t.clone()).collect(),
            _ => parts.iter().cloned().collect(),
        };
        let mut alts: SmallVec<[SmallVec<[Type; 4]>; 8]> = SmallVec::new();
        let mut count = 1usize;
        for f in fields.iter() {
            let a = self.alternatives(f)?;
            count = count.saturating_mul(a.len());
            alts.push(a);
        }
        if count <= 1 || count > MOST {
            return Ok(false);
        }
        let mut pick: SmallVec<[usize; 8]> = smallvec::smallvec![0; alts.len()];
        loop {
            let chosen: SmallVec<[Type; 8]> =
                pick.iter().zip(alts.iter()).map(|(i, a)| a[*i].clone()).collect();
            let one = match b {
                Type::Tuple(_) => Type::Tuple(triomphe::Arc::from_iter(chosen)),
                Type::Variant(tag, _, at) => {
                    Type::Variant(tag.clone(), triomphe::Arc::from_iter(chosen), *at)
                }
                Type::Struct(fs) => Type::Struct(triomphe::Arc::from_iter(
                    fs.iter().zip(chosen).map(|((n, _, at), t)| (n.clone(), t, *at)),
                )),
                _ => unreachable!(),
            };
            let held = ms.iter().try_fold(false, |held, m| {
                Ok::<_, anyhow::Error>(held || self.attempt(m, &one)?)
            })?;
            self.keep.push(one);
            if !held {
                return Ok(false);
            }
            let mut i = 0;
            loop {
                if i == pick.len() {
                    return Ok(true);
                }
                pick[i] += 1;
                if pick[i] < alts[i].len() {
                    break;
                }
                pick[i] = 0;
                i += 1;
            }
        }
    }

    /// The types a part may be one of: a union's members, through a
    /// typedef; each primitive of several.
    fn alternatives(&mut self, t: &Type) -> Result<SmallVec<[Type; 4]>> {
        let t = decided(t).into_owned();
        let t = match &t {
            Type::Ref(_)
                if self
                    .env
                    .trait_of_ref(match &t {
                        Type::Ref(r) => r,
                        _ => unreachable!(),
                    })
                    .is_none() =>
            {
                t.lookup_ref_with(self.env, false)?.unwrap_or(t.clone())
            }
            _ => t,
        };
        Ok(match &t {
            Type::Set(ms) => ms.iter().map(|m| decided(m).into_owned()).collect(),
            Type::Primitive(p) if p.len() > 1 => {
                p.iter().map(|bit| Type::Primitive(bit.into())).collect()
            }
            t => smallvec::smallvec![t.clone()],
        })
    }

    /// A declared variable is held by what holds one of its bounds.
    fn by_bounds(&mut self, a: &Type, y: &TVar) -> Result<bool> {
        for c in y.cell_constraints().iter() {
            if matches!(c, Type::Bound(_)) {
                continue;
            }
            if self.attempt(a, c)? {
                return Ok(true);
            }
        }
        Ok(false)
    }

    /// Typedefs by their expansion; one name on both sides by its
    /// arguments first.
    fn expanded(&mut self, a: &Type, b: &Type) -> Result<bool> {
        // two applications of one name meet by its parameters' variance
        if let (Type::Ref(r0), Type::Ref(r1)) = (a, b)
            && r0.same_definition(r1, self.env)
            && r0.params.len() == r1.params.len()
        {
            let meets = r0
                .resolve_peek(self.env)
                .and_then(|def| super::variance::meets(self.env, &def))
                .filter(|meets| !meets.iter().any(|m| m.walk()));
            if let Some(meets) = meets {
                for ((x, y), m) in
                    r0.params.iter().zip(r1.params.iter()).zip(meets.iter())
                {
                    let co = m.holds() || m.holds_swapped();
                    let contra = m.held() || m.held_swapped();
                    let ok = match (co, contra) {
                        (true, true) => self.equal(x, y)?,
                        (true, false) => self.go(x, y)?,
                        (false, true) => self.go(y, x)?,
                        (false, false) => true,
                    };
                    if !ok {
                        return Ok(false);
                    }
                }
                return r0.hold_bounds_with(r1, self.env, false);
            }
            let mut same = true;
            for (x, y) in r0.params.iter().zip(r1.params.iter()) {
                if !self.equal(x, y)? {
                    same = false;
                    break;
                }
            }
            if same {
                return r0.hold_bounds_with(r1, self.env, false);
            }
        }
        // an application whose argument its parameter's bound refuses is
        // no type
        let (Some(ea), Some(eb)) = (self.expand(a)?, self.expand(b)?) else {
            return Ok(false);
        };
        let key = (self.ident(a), self.ident(b));
        match self.names.get(&key) {
            Some(&REFUSED) => return Ok(false),
            Some(d) => {
                self.relied = self.relied.min(*d);
                return Ok(true);
            }
            None => (),
        }
        // the name pair is assumed at this walk's depth
        let depth = self.depth;
        self.names.insert(key.clone(), depth);
        let before = std::mem::replace(&mut self.relied, NONE);
        let r = self.go(&ea, &eb)?;
        let relied = match self.relied {
            d if d >= depth => NONE,
            d => d,
        };
        self.relied = before.min(relied);
        match r {
            true => {
                self.names.insert(key.clone(), relied);
                if relied != NONE {
                    self.trail.push(Undo::Name(key));
                }
            }
            false => {
                self.names.insert(key, REFUSED);
            }
        }
        Ok(r)
    }

    /// `t`'s identity: its structure, each cell by itself, not by its
    /// name, each shared node read once.
    fn ident(&mut self, t: &Type) -> Side {
        use std::hash::{BuildHasher, Hash, Hasher};
        let t = decided(t);
        let t = &*t;
        let addr = node_addr(t);
        if let Some(id) = addr.and_then(|a| self.idents.get(&a)) {
            return *id;
        }
        let parts: SmallVec<[Side; 8]> = ensure_sufficient(|| {
            let mut parts: SmallVec<[Side; 8]> = SmallVec::new();
            match t {
                Type::Ref(r) => r.params.iter().for_each(|p| parts.push(self.ident(p))),
                Type::App(c, x) => {
                    parts.push(self.ident(c));
                    parts.push(self.ident(x));
                }
                Type::Fn(f) => {
                    f.args.iter().for_each(|a| parts.push(self.ident(&a.typ)));
                    if let Some(v) = &f.vargs {
                        parts.push(self.ident(v));
                    }
                    parts.push(self.ident(&f.rtype));
                    parts.push(self.ident(&f.throws));
                }
                Type::Map(m) => {
                    parts.push(self.ident(&m.key));
                    parts.push(self.ident(&m.value));
                }
                t => t.for_each_child(&mut |c| parts.push(self.ident(c))),
            }
            parts
        });
        let mut id: Side = 0;
        for seed in [0x5157_u64, 0x9e37_79b9] {
            let mut h = ahash::RandomState::with_seeds(seed, 1, 2, 3).build_hasher();
            std::mem::discriminant(t).hash(&mut h);
            match t {
                Type::TVar(tv) => tv.cell_addr().hash(&mut h),
                Type::Primitive(p) => p.bits().hash(&mut h),
                Type::Bound(b) => b.hash(&mut h),
                Type::Ref(r) => {
                    r.scope.hash(&mut h);
                    r.name.hash(&mut h);
                }
                Type::Abstract { id, .. } => id.hash(&mut h),
                Type::Struct(fs) => fs.iter().for_each(|(n, _, _)| n.hash(&mut h)),
                Type::Variant(tag, _, _) => tag.hash(&mut h),
                Type::ByRef(m, _) => matches!(m, Mutability::Mut).hash(&mut h),
                Type::Fn(f) => {
                    for a in f.args.iter() {
                        a.kind.name().hash(&mut h);
                        a.has_default().hash(&mut h);
                        a.is_positional().hash(&mut h);
                    }
                    f.vargs.is_some().hash(&mut h);
                    // its quantifiers' bounds are its own: a function type
                    // with them is itself alone
                    if !f.constraint_view().is_empty() {
                        node_addr(t).hash(&mut h);
                    }
                }
                _ => (),
            }
            parts.hash(&mut h);
            id = (id << 64) | h.finish() as u128;
        }
        if let Some(a) = addr {
            self.idents.insert(a, id);
        }
        id
    }

    /// `t`'s expansion where it is a name, the same node at each meeting;
    /// `None` where its arguments break a parameter's bound.
    fn expand(&mut self, t: &Type) -> Result<Option<Type>> {
        if !matches!(t, Type::Ref(_)) {
            return Ok(Some(t.clone()));
        }
        let key = self.ident(t);
        if let Some(e) = self.expansions.get(&key) {
            return Ok(e.clone());
        }
        let e = t.lookup_ref_with(self.env, false)?;
        self.expansions.insert(key, e.clone());
        Ok(e)
    }

    /// Whether `t` meets the predicate `p`.
    fn predicate(&mut self, p: Bound, t: &Type) -> Result<bool> {
        let env = self.env;
        if self.sealed
            && let Type::TVar(tv) = t
            && tv.binding().is_none()
            && tv.is_rigid()
        {
            return tv.conjunct_implies(env, &Type::Bound(p));
        }
        let sealed = self.sealed;
        Ok(match p {
            Bound::Concrete => t.concrete_holds(env),
            Bound::Data => t.data_holds(env),
            Bound::Function => t.function_holds(env, false)?,
            Bound::Singleton => t.singleton_holds(env, false)?,
            Bound::OneNumber => t.one_number_holds(env, false, sealed)?,
            Bound::NoError => !t.may_be_error(env, false, sealed)?,
            Bound::Discernible => t.discernible(env, false),
            Bound::Ordered => t.discernible(env, true),
            Bound::JsonData | Bound::TomlData => true,
        })
    }

    /// A function of type `f1` serves where `f0` is wanted: every call of
    /// `f0`'s shape is one `f1` takes, a label `f0` lets a caller leave out
    /// one `f1` defaults; parameters the other way, result and throws this
    /// way; each side's declared bounds hold.
    // CR claude for claude: [bug/low] A function type with quantifiers of its own on the
    // held side is not instantiated: `[fn(x: string) -> i64, fn(x: string) -> string] ⊇
    // fn<'c: Any>(x: 'c) -> 'c` is false (the identity serves as `string -> string`). The
    // old relation bound copies of 'c; this relation binds nothing, so it needs a
    // substitution local to the walk (the first meeting of an own quantifier takes the
    // type it meets, later meetings must agree). False refuses, never admits, and no
    // fixture reaches it (the mirror over every module agreed); the old relation's pin
    // was typ::contains::tests::a_commit_decides_a_quantifier_an_earlier_task_left_open,
    // deleted with the old entry points. (holds-01)
    fn function(&mut self, f0: &FnType, f1: &FnType) -> Result<bool> {
        let (pairs, ok) = f0.align(f1);
        if !ok || pairs.iter().any(|(s, t)| s.has_default() && !t.has_default()) {
            return Ok(false);
        }
        match (&f0.vargs, &f1.vargs) {
            (Some(v0), Some(v1)) => {
                if !self.go(v1, v0)? {
                    return Ok(false);
                }
            }
            (None, None) => (),
            _ => return Ok(false),
        }
        for (s, t) in pairs.iter() {
            if !self.go(&t.typ, &s.typ)? {
                return Ok(false);
            }
        }
        Ok(self.go(&f0.rtype, &f1.rtype)? && self.go(&f0.throws, &f1.throws)?)
    }
}

impl Type {
    /// Whether every value of `t` is a value of `self`, over decided types,
    /// in a check by bounds or not ([`holds`]).
    pub fn decided_holds(&self, env: &Env, t: &Type) -> Result<bool> {
        holds(env, self, t)
    }

    /// Whether a type that `discernible` reads may be told apart (`ordered`:
    /// ordered) as [`Bound::Discernible`] and [`Bound::Ordered`] say.
    fn discernible(&self, env: &Env, ordered: bool) -> bool {
        self.indiscernible(env, super::Open::Benign, ordered).is_none()
    }

    /// Whether `t` implements the trait `tid`, as a check over decided
    /// types asks it.
    fn implements(tid: super::TraitId, env: &Env, t: &Type) -> Result<bool> {
        Type::trait_holds(tid, env, t)
    }
}
