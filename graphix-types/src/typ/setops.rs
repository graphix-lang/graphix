use crate::{
    env::Env,
    stack::ensure_sufficient,
    typ::{
        Bound, ContainsFlags, FnType, Kept, Lazy, NodePairs, RefHist, RefPair, TVar,
        Type, TypeRef, contains::on_copies, node_addr,
    },
};
use ahash::{AHashMap, AHashSet};
use anyhow::Result;
use enumflags2::BitFlags;
use netidx_value::Typ;
use poolshark::local::LPooled;
use smallvec::{SmallVec, smallvec};
use std::{iter, mem, ops::ControlFlow};
use triomphe::Arc;

struct MatchingTypes {
    memo: RefHist<AHashMap<RefPair, Kept<Type>>>,
    refs: LPooled<AHashSet<(Option<usize>, Option<usize>)>>,
    /// Type a part holding a rigid cell by the written test's part
    /// ([`Type::matching_parts`]).
    inside: bool,
}

/// diff's walk state: each pair in progress with what it subtracts when
/// met again inside its own difference (nothing), and the difference of
/// each finished pair whose walk met no pair in progress and whose
/// minuend, of which a difference is made, [`RefHist::ref_id`] keys
/// exactly. A subtrahend keyed alike is read alike: diff reads every part
/// of it through.
struct DiffHist {
    hist: RefHist<AHashMap<RefPair, Type>>,
    done: Lazy<AHashMap<RefPair, Kept<Type>>>,
    unfoldings: Unfoldings,
    /// The current walk met a pair in progress, or stopped unfolding.
    cycled: bool,
}

impl DiffHist {
    fn new() -> Self {
        DiffHist {
            hist: RefHist::new(),
            done: Lazy::new(),
            unfoldings: Unfoldings::default(),
            cycled: false,
        }
    }
}

/// The definitions of a pair a walk unfolds, a side that is no named type
/// `None`: arguments and the types around them ignored.
pub(super) type DefPair = (Option<usize>, Option<usize>);

pub(super) fn def_pair(env: &Env, t0: &Type, t1: &Type) -> DefPair {
    let def = |t: &Type| match t {
        Type::Ref(tr) => tr.resolve_in(env).map(|r| r.def_key()),
        _ => None,
    };
    (def(t0), def(t1))
}

/// How many unfoldings of one pair of definitions a walk's path may
/// hold; the next is the pair met again. Types a walk builds as it goes (a
/// union less each member in turn, parameters that grow) are new keys at
/// every level, and an exact difference may repeat only after the lcm of
/// the cycles it walks. Each more unfolding multiplies a walk's cost
/// (`design/tvar_constraints.md`, Unfolding a pair of definitions).
pub(super) const UNFOLDINGS: usize = 1;

/// The pairs of definitions a walk is inside, each with its unfoldings'
/// pairs in progress, innermost last.
#[derive(Default)]
pub(super) struct Unfoldings(LPooled<AHashMap<DefPair, SmallVec<[RefPair; 1]>>>);

impl Unfoldings {
    /// The innermost pair in progress of `defs` when the walk has unfolded
    /// it [`UNFOLDINGS`] times.
    pub(super) fn again(&self, defs: &DefPair) -> Option<RefPair> {
        self.0
            .get(defs)
            .filter(|ks| ks.len() >= UNFOLDINGS)
            .and_then(|ks| ks.last().copied())
    }

    pub(super) fn enter(&mut self, defs: DefPair, key: RefPair) {
        self.0.entry(defs).or_default().push(key)
    }

    pub(super) fn leave(&mut self, defs: &DefPair) {
        if let Some(ks) = self.0.get_mut(defs) {
            ks.pop();
        }
    }
}

/// Whether [`RefHist::ref_id`] may key `t` alike with a type that prints
/// apart: `t` holds a bound cell, keyed as its binding, or a cell in a
/// function type, paired by position.
fn keyed_loosely(t: &Type) -> bool {
    cell_beneath(t, |tv, in_fn| in_fn || tv.binding().is_some())
}

/// Whether `t` holds a cell `pick` takes (told whether the cell is in a
/// function type), each shared node walked once. A reference's parameters
/// are read, its expansion is not.
fn cell_beneath(t: &Type, pick: impl Fn(&TVar, bool) -> bool) -> bool {
    fn go(
        t: &Type,
        in_fn: bool,
        pick: &impl Fn(&TVar, bool) -> bool,
        seen: &mut AHashSet<(usize, bool)>,
    ) -> ControlFlow<()> {
        ensure_sufficient(|| match t {
            Type::TVar(tv) if pick(tv, in_fn) => ControlFlow::Break(()),
            Type::TVar(_) => ControlFlow::Continue(()),
            t if node_addr(t).is_some_and(|n| !seen.insert((n, in_fn))) => {
                ControlFlow::Continue(())
            }
            Type::Fn(_) => t.try_for_each_child(&mut |c| go(c, true, pick, seen)),
            t => t.try_for_each_child(&mut |c| go(c, in_fn, pick, seen)),
        })
    }
    go(t, false, &pick, &mut LPooled::take()).is_break()
}

fn is_empty_ty(t: &Type) -> bool {
    match t {
        Type::Bottom => true,
        Type::Primitive(p) => p.is_empty(),
        _ => false,
    }
}

impl Type {
    /// No value inhabits this type: bottom, the empty primitive set, or
    /// an empty union (a `diff` residual with nothing left).
    #[doc(hidden)]
    pub fn is_uninhabited(&self) -> bool {
        match self {
            Type::Set(ts) => ts.is_empty(),
            t => is_empty_ty(t),
        }
    }
}

fn diff_already_normal(before: &Type, after: &Type) -> bool {
    match (before, after) {
        (_, Type::Primitive(p)) if p.is_empty() => true,
        (Type::Set(s), Type::Set(out)) => {
            let mut i = 0;
            for o in out.iter() {
                loop {
                    if i >= s.len() {
                        return false;
                    }
                    let j = i;
                    i += 1;
                    if s[j] == *o {
                        break;
                    }
                }
            }
            true
        }
        (Type::Set(s), t) => s.iter().any(|m| m == t),
        _ => false,
    }
}

/// Structural identity for union-collapse decisions: two unbound
/// cells are identical only when they are one cell (a collapse must not
/// discard a cell whose future binding may diverge), a bound cell is its
/// binding, and inside a function type the cells pair consistently
/// (`fn('a) -> 'a` is `fn('b) -> 'b`, not `fn('a) -> 'b`).
pub(super) fn union_identical(t0: &Type, t1: &Type) -> bool {
    Identity::new(false).same(t0, t1)
}

/// [`union_identical`] with every unbound cell by identity, inside a
/// function type too: one signature's variables are its own.
pub(crate) fn strictly_identical(t0: &Type, t1: &Type) -> bool {
    Identity::new(true).same(t0, t1)
}

/// Identity of `t0` and `t1`.
struct Identity {
    strict: bool,
    /// Inside a function type: the cells paired so far, by address.
    pairs: Option<SmallVec<[(usize, usize); 4]>>,
    /// The nodes found identical outside a function type.
    found: NodePairs,
    /// Those found inside the function type being compared, which hold
    /// only under its pairing of cells.
    found_in_fn: NodePairs,
}

impl Identity {
    fn new(strict: bool) -> Self {
        Self {
            strict,
            pairs: None,
            found: NodePairs::default(),
            found_in_fn: NodePairs::default(),
        }
    }

    fn same(&mut self, t0: &Type, t1: &Type) -> bool {
        // one shared node is identical to itself, however large
        if t0.shared_identity().is_some_and(|a| t1.shared_identity() == Some(a)) {
            return true;
        }
        let pair = self.found.key(t0, t1);
        if let Some(pair) = pair
            && (self.found.contains(pair)
                || (self.pairs.is_some() && self.found_in_fn.contains(pair)))
        {
            return true;
        }
        let same = ensure_sufficient(|| self.same_inner(t0, t1));
        if let Some(pair) = pair.filter(|_| same) {
            match self.pairs {
                None => self.found.insert(pair, t0, t1),
                Some(_) => self.found_in_fn.insert(pair, t0, t1),
            }
        }
        same
    }

    fn all(&mut self, a: &[Type], b: &[Type]) -> bool {
        a.len() == b.len() && a.iter().zip(b.iter()).all(|(a, b)| self.same(a, b))
    }

    fn cells(&mut self, a: &TVar, b: &TVar) -> bool {
        if a.same_cell(b) {
            return true;
        }
        let (x, y) = (a.cell_addr(), b.cell_addr());
        let Some(pairs) = &mut self.pairs else { return false };
        match pairs.iter().find(|(p, q)| *p == x || *q == y) {
            Some(&(p, q)) => p == x && q == y,
            None => {
                pairs.push((x, y));
                let (ca, cb) = (a.cell_constraints(), b.cell_constraints());
                self.all(&ca, &cb)
            }
        }
    }

    #[inline(never)]
    fn same_inner(&mut self, t0: &Type, t1: &Type) -> bool {
        match (t0, t1) {
            (Type::TVar(a), Type::TVar(b)) => match (a.binding(), b.binding()) {
                (Some(x), Some(y)) => a.same_cell(b) || self.same(&x, &y),
                (None, None) => self.cells(a, b),
                (None, Some(_)) | (Some(_), None) => false,
            },
            (Type::TVar(a), t) | (t, Type::TVar(a)) => {
                a.binding().is_some_and(|x| self.same(&x, t))
            }
            (Type::Bottom, Type::Bottom)
            | (Type::Any, Type::Any)
            | (Type::Hole, Type::Hole)
            | (Type::Bound(Bound::Concrete), Type::Bound(Bound::Concrete))
            | (Type::Bound(Bound::Data), Type::Bound(Bound::Data))
            | (Type::Bound(Bound::Function), Type::Bound(Bound::Function))
            | (Type::Bound(Bound::OneNumber), Type::Bound(Bound::OneNumber))
            | (Type::Bound(Bound::NoError), Type::Bound(Bound::NoError))
            | (Type::Bound(Bound::Discernible), Type::Bound(Bound::Discernible))
            | (Type::Bound(Bound::Ordered), Type::Bound(Bound::Ordered))
            | (Type::Bound(Bound::JsonData), Type::Bound(Bound::JsonData))
            | (Type::Bound(Bound::TomlData), Type::Bound(Bound::TomlData))
            | (Type::Bound(Bound::Singleton), Type::Bound(Bound::Singleton)) => true,
            (Type::App(c0, a0), Type::App(c1, a1)) => {
                self.same(c0, c1) && self.same(a0, a1)
            }
            (Type::Primitive(a), Type::Primitive(b)) => a == b,
            (
                Type::Abstract { id: i0, params: p0 },
                Type::Abstract { id: i1, params: p1 },
            ) => i0 == i1 && self.all(p0, p1),
            (Type::Ref(r0), Type::Ref(r1)) => {
                r0.same_definition(r1) && self.all(&r0.params, &r1.params)
            }
            (Type::Set(s0), Type::Set(s1)) => self.all(s0, s1),
            (Type::Error(a), Type::Error(b))
            | (Type::Array(a), Type::Array(b))
            | (Type::List(a), Type::List(b)) => self.same(a, b),
            (Type::ByRef(m0, a), Type::ByRef(m1, b)) => m0 == m1 && self.same(a, b),
            (Type::Map { key: k0, value: v0 }, Type::Map { key: k1, value: v1 }) => {
                self.same(k0, k1) && self.same(v0, v1)
            }
            (Type::Tuple(a), Type::Tuple(b)) => self.all(a, b),
            (Type::Struct(a), Type::Struct(b)) => {
                a.len() == b.len()
                    && a.iter()
                        .zip(b.iter())
                        .all(|((n0, x, _), (n1, y, _))| n0 == n1 && self.same(x, y))
            }
            (Type::Variant(tg0, a, _), Type::Variant(tg1, b, _)) => {
                tg0 == tg1 && self.all(a, b)
            }
            (Type::Fn(f0), Type::Fn(f1)) => {
                let outer = !self.strict && self.pairs.is_none();
                if outer {
                    self.pairs = Some(SmallVec::new());
                }
                let r = f0.args.len() == f1.args.len()
                    && f0.quantifiers.len() == f1.quantifiers.len()
                    && f0
                        .args
                        .iter()
                        .zip(f1.args.iter())
                        .all(|(a, b)| a.kind == b.kind && self.same(&a.typ, &b.typ))
                    && match (&f0.vargs, &f1.vargs) {
                        (None, None) => true,
                        (Some(a), Some(b)) => self.same(a, b),
                        _ => false,
                    }
                    && self.same(&f0.rtype, &f1.rtype)
                    && self.same(&f0.throws, &f1.throws);
                if outer {
                    self.pairs = None;
                    self.found_in_fn.clear();
                }
                r
            }
            _ => false,
        }
    }
}

/// Two same-named refs to one definition with identical params.
fn same_ref(r0: &TypeRef, r1: &TypeRef) -> bool {
    r0.same_definition(r1)
        && r0.params.len() == r1.params.len()
        && r0.params.iter().zip(r1.params.iter()).all(|(a, b)| union_identical(a, b))
}

/// The two-member union of `a` and `b`.
fn pair(a: &Type, b: &Type) -> Type {
    Type::Set(Arc::from_iter([a.clone(), b.clone()]))
}

/// How a union treats two applications of one definition that differ
/// in a parameter the body does not hold linearly.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Merge {
    Exact,
    Supertype,
}

impl Type {
    /// Same-named refs `self` (`t0`) and `t` (`t1`): identical params are
    /// one member; one differing param merges param-wise when the merge
    /// holds both (a probe, so only a covariant param merges) and, for an
    /// exact union, the param stands once in the body outside any
    /// collection, so the merge holds no mixed value; more than one would
    /// invent pairs, so they stay two members.
    fn union_ref_params(
        &self,
        env: &Env,
        hist: &mut RefHist<AHashMap<RefPair, Type>>,
        merge: Merge,
        t0: &TypeRef,
        t1: &TypeRef,
        t: &Self,
    ) -> Result<Self> {
        let mut differ = t0
            .params
            .iter()
            .zip(t1.params.iter())
            .enumerate()
            .filter(|(_, (a, b))| !union_identical(a, b));
        let (i, (p0, p1)) = match (differ.next(), differ.next()) {
            (None, _) => return Ok(self.clone()),
            (Some(d), None) => d,
            (Some(_), Some(_)) => return Ok(pair(self, t)),
        };
        if merge == Merge::Exact
            && !t0.resolve_in(env).is_some_and(|d| d.param_is_linear(i))
        {
            return Ok(pair(self, t));
        }
        let mut params: LPooled<Vec<Type>> = t0.params.iter().cloned().collect();
        params[i] = p0.union_int(env, hist, merge, p1)?;
        let merged =
            Type::Ref(Arc::new(t0.with_params(Arc::from_iter(params.drain(..)))));
        let probe = BitFlags::empty();
        if merged.contains_with_flags(probe, env, self)?
            && merged.contains_with_flags(probe, env, t)?
        {
            Ok(merged)
        } else {
            Ok(pair(self, t))
        }
    }

    fn union_int(
        &self,
        env: &Env,
        hist: &mut RefHist<AHashMap<RefPair, Type>>,
        merge: Merge,
        t: &Self,
    ) -> Result<Self> {
        ensure_sufficient(|| self.union_inner(env, hist, merge, t))
    }

    fn union_inner(
        &self,
        env: &Env,
        hist: &mut RefHist<AHashMap<RefPair, Type>>,
        merge: Merge,
        t: &Self,
    ) -> Result<Self> {
        match (self, t) {
            (Type::Bottom, t) | (t, Type::Bottom) => Ok(t.clone()),
            (Type::Primitive(p), t) | (t, Type::Primitive(p)) if p.is_empty() => {
                Ok(t.clone())
            }
            (Type::Ref(t0), Type::Ref(t1))
                if t0.same_definition(t1) && t0.params.len() == t1.params.len() =>
            {
                self.union_ref_params(env, hist, merge, t0, t1, t)
            }
            (tr @ Type::Ref(_), t) | (t, tr @ Type::Ref(_)) => {
                let key = if matches!(self, Type::Ref(_)) {
                    (hist.ref_id(tr, env), hist.ref_id(t, env))
                } else {
                    (hist.ref_id(t, env), hist.ref_id(tr, env))
                };
                if let Some(u) = hist.get(&key) {
                    return Ok(u.clone());
                }
                let e = tr.lookup_ref(env)?;
                hist.insert(key, tr.clone());
                let r = if matches!(self, Type::Ref(_)) {
                    e.union_int(env, hist, merge, t)
                } else {
                    t.union_int(env, hist, merge, &e)
                };
                hist.remove(&key);
                r
            }
            (t0 @ Type::Abstract { .. }, t1 @ Type::Abstract { .. })
                if union_identical(t0, t1) =>
            {
                Ok(self.clone())
            }
            (t0 @ Type::Abstract { .. }, t1) | (t0, t1 @ Type::Abstract { .. }) => {
                Ok(pair(t0, t1))
            }
            (Type::Any, _) | (_, Type::Any) => Ok(Type::Any),
            (Type::App(..), _)
            | (_, Type::App(..))
            | (Type::Hole, _)
            | (_, Type::Hole)
            | (Type::Bound(_), _)
            | (_, Type::Bound(_)) => {
                if self == t {
                    Ok(self.clone())
                } else {
                    Ok(pair(self, t))
                }
            }
            (Type::Primitive(s0), Type::Primitive(s1)) => {
                let mut s = *s0;
                s.insert(*s1);
                Ok(Type::Primitive(s))
            }
            (Type::Primitive(p), Type::Array(_))
            | (Type::Array(_), Type::Primitive(p))
                if p.contains(Typ::Array) =>
            {
                Ok(Type::Primitive(*p))
            }
            (Type::Primitive(p), Type::Map { .. })
            | (Type::Map { .. }, Type::Primitive(p))
                if p.contains(Typ::Map) =>
            {
                Ok(Type::Primitive(*p))
            }
            (Type::Primitive(p), Type::Error(_))
            | (Type::Error(_), Type::Primitive(p))
                if p.contains(Typ::Error) =>
            {
                Ok(Type::Primitive(*p))
            }
            (Type::Error(e0), Type::Error(e1)) => {
                Ok(Type::Error(Arc::new(e0.union_int(env, hist, merge, e1)?)))
            }
            (Type::Set(s0), Type::Set(s1)) => Ok(Type::Set(Arc::from_iter(
                s0.iter().cloned().chain(s1.iter().cloned()),
            ))),
            (Type::Set(s), t) | (t, Type::Set(s)) => Ok(Type::Set(Arc::from_iter(
                s.iter().cloned().chain(iter::once(t.clone())),
            ))),
            (u @ Type::Variant(tg0, t0, at), t @ Type::Variant(tg1, t1, _)) => {
                // Component-wise union is exact only when at most one
                // position differs: `P(A, X) ∪ `P(B, Y) is not
                // `P([A, B], [X, Y]) (that invents `P(A, Y)).
                let differing = || {
                    t0.iter()
                        .zip(t1.iter())
                        .filter(|(a, b)| !union_identical(a, b))
                        .count()
                };
                if tg0 == tg1 && t0.len() == t1.len() && differing() <= 1 {
                    let mut typs = t0
                        .iter()
                        .zip(t1.iter())
                        .map(|(t0, t1)| t0.union_int(env, hist, merge, t1))
                        .collect::<Result<LPooled<Vec<_>>>>()?;
                    Ok(Type::Variant(tg0.clone(), Arc::from_iter(typs.drain(..)), *at))
                } else {
                    Ok(pair(u, t))
                }
            }
            (t0 @ Type::TVar(_), t1 @ Type::TVar(_)) if union_identical(t0, t1) => {
                Ok(t0.clone())
            }
            // A bound cell unions as its binding; an unbound cell
            // stays its own member.
            (Type::TVar(tv), t1) => match tv.binding() {
                Some(b) => b.union_int(env, hist, merge, t1),
                None => Ok(pair(self, t1)),
            },
            (t0, Type::TVar(tv)) => match tv.binding() {
                Some(b) => t0.union_int(env, hist, merge, &b),
                None => Ok(pair(t0, t)),
            },
            // Everything left of one variant is kept once if identical,
            // else as two members.
            (t0, t1)
                if std::mem::discriminant(t0) == std::mem::discriminant(t1)
                    && union_identical(t0, t1) =>
            {
                Ok(t0.clone())
            }
            (t0, t1) => Ok(pair(t0, t1)),
        }
    }

    /// The union of `ts`, which may widen an application's parameter
    /// that recurses or sits under a collection (`L<i64> ∪ L<string>` is
    /// `L<[i64, string]>`, which holds mixed lists): a type for the values
    /// of `ts`.
    pub fn union(env: &Env, ts: &[&Type]) -> Result<Self> {
        Self::union_with(env, Merge::Supertype, ts)
    }

    /// The union of `ts`, admitting nothing none of them admits: what
    /// a set of patterns covers.
    pub fn union_exact(env: &Env, ts: &[&Type]) -> Result<Self> {
        Self::union_with(env, Merge::Exact, ts)
    }

    /// The union of a select's alternatives, which a call through the
    /// select must accept alike: function members meet first
    /// ([`Self::meet_open_fns`]).
    pub fn union_of_alternatives(env: &Env, ts: &[&Type]) -> Result<Self> {
        match Self::meet_open_fns(env, ts)? {
            None => Self::union(env, ts),
            Some(met) => {
                let met: LPooled<Vec<&Type>> = met.iter().collect();
                Self::union(env, &met)
            }
        }
    }

    fn union_with(env: &Env, merge: Merge, ts: &[&Type]) -> Result<Self> {
        let mut iter = ts.iter().copied();
        let Some(first) = iter.next() else {
            return Ok(Type::Primitive(BitFlags::empty()));
        };
        let mut hist = RefHist::new();
        let mut acc = first.clone();
        for t in iter {
            acc = acc.union_int(env, &mut hist, merge, t)?;
        }
        Ok(acc.normalize())
    }

    /// Function members that meet both ways, one with open cells, are one
    /// member: calling either takes the same arguments, and the union's
    /// call needs one signature. A generic member meets through a fresh
    /// instance; an occurrence's own cells bind, as the call would bind
    /// them, once the meet holds on copies (a probe judges each part apart,
    /// and a refused commit would leave its bindings behind). `None` when
    /// no member changes.
    fn meet_open_fns(env: &Env, ts: &[&Type]) -> Result<Option<LPooled<Vec<Type>>>> {
        let fns: SmallVec<[(usize, Arc<FnType>); 4]> = ts
            .iter()
            .enumerate()
            .filter_map(|(i, t)| {
                t.with_deref(|t| match t {
                    Some(Type::Fn(f)) => Some((i, f.clone())),
                    _ => None,
                })
            })
            .collect();
        if fns.len() < 2 {
            return Ok(None);
        }
        let open = nohash::IntSet::default();
        let mut out: Option<LPooled<Vec<Type>>> = None;
        let mut met: SmallVec<[bool; 4]> = smallvec![false; fns.len()];
        let probe = BitFlags::empty();
        for (k, (i, f)) in fns.iter().enumerate() {
            if Type::Fn(f.clone()).tvar_free() {
                continue;
            }
            let inst = Type::Fn(Arc::new(f.instantiate(&open)));
            for (j, (_, g)) in fns.iter().enumerate() {
                if j == k || met[j] {
                    continue;
                }
                let g = Type::Fn(g.clone());
                if inst.contains_with_flags(probe, env, &g)?
                    && g.contains_with_flags(probe, env, &inst)?
                    && on_copies([&inst, &g], |[i, g], trial| {
                        let commit = ContainsFlags::Commit.into();
                        Ok(i.contains_int(commit, env, trial, &g)?
                            && g.contains_int(commit, env, trial, &i)?)
                    })?
                    && inst.contains(env, &g)?
                    && g.contains(env, &inst)?
                {
                    // the member it meets stands for it, so the union holds one
                    let out = out
                        .get_or_insert_with(|| ts.iter().map(|t| (*t).clone()).collect());
                    out[*i] = g;
                    met[k] = true;
                    break;
                }
            }
        }
        Ok(out)
    }

    // XCR claude for claude: [bug/medium] diff does not end over some names that recur
    // through Error payloads: main bbb06c0c6's --check of `type At = [Error<Bt>,
    // Error<Ct>, `R]; type Bt = [Error<At>, `P]; type Ct = [Array<Ct>, Error<Bt>]; type H
    // = [Error<Error<K>>, `Q]; type K = [Error<H>, Error<K>, `P]; let v: At = `R; select
    // v { K as k => 0, _ => 1 }` is killed at 60 s, and a unit walk of diff(At, K) had
    // not returned after 30 s. Of 1600 selects the review's generators write
    // (review-r8/X-r1-1 gen_rnd.py, gen_cm1.py, seeds 1-1200 and 1-400), 53 time out
    // at 30 s (six sampled, at 120 s too); rnd13's thread is deep in
    // Select::typecheck0_with -> Type::diff -> diff_inner, normalize innermost. A pair
    // met again is found by its sides' ids, and the (Set, Set) and (t, Set) arms
    // subtract each next member from the difference so far, a type built anew with a
    // new id, so a cycle through it may never meet a pair again (traced for the hang
    // the round-7 memo of diff-dag-01 caused, not yet for this one). probes:
    // ~/ptmp/cr29/scratch/r8-X/mainhang.gx, rnd13.gx (diff-cycle-hang-01)
    // 2026-11-03 claude: Carried to main from the parked branch cr45-X (326be068c), where
    // it was written. main ec53d4846 and lane cr46-X both still time out at 30 s on
    // mainhang.gx and rnd13.gx (--no-cache --check).
    // 2026-11-04 claude: Parked after four failed reviews; main does not change. Rounds
    // 11 and 13 (cr48-X, cr48-X2 61e37fe65) took an Error<e> less a union as e less the
    // union of its errors' payloads, and a union less a union member by member against
    // the whole union: 568 of 4800 generated base timeouts end, and the 22 accepted
    // programs it refuses are dead arms. Round 11's growth cut changed verdicts main gets
    // right and was quadratic. Round 13 found the payload union a new key at every level
    // over Error cycles of coprime lengths, repeating only after their lcm: cy1260 1.32G
    // instructions on main against 7.18G, sh23571113 1.35G against 1562G, n7 ends on main
    // and is killed at 200 s on the lane. Over disjoint tags (cy, n) the exact residual
    // is the minuend and the walk subtracts nothing for lcm levels; over shared tags (sh)
    // the exact residual itself repeats only every lcm levels, so a diff that stays
    // polynomial must stop subtracting somewhere, and keying by the minuend's definition
    // alone moves sh235e2.gx, which main refuses rightly. Where diff may stop subtracting
    // (a wider residual: more missing-case refusals, fewer dead arms found) is a rule
    // choice, so this goes to Eric. probes:
    // ~/ptmp/cr29/scratch/review-r11/X-r1-1/x2/lcm/, X-r3-1/x2/, X-r3-2/x2/,
    // ~/ptmp/cr29/scratch/r11-X/x2r13b/ (sh235e2.gx); the branch's own note on cr48-X2
    // has the three directions tried on paper.
    // 2026-11-05 claude: Eric ruled (2026-11-05): (b). diff keys a pair met again by its
    // two definitions, arguments ignored, and allows k unfoldings per definition pair, as
    // cast::grows_on counts; past them the pair subtracts nothing (a wider residual:
    // fewer dead arms found, an occasional missing-case refusal). k starts at 1 and is
    // chosen from the census of both repos, the 1600 generated selects and the review's
    // grow/lcm families. could_match's missing bound goes in the same lane. Analysis:
    // ~/ptmp/cr-discuss.md, Ruled 11-05.
    // 2026-11-05 claude: Built (lane U, round 16) at k = 1: diff and could_match key a
    // pair met again by its two definitions (setops::DefPair, arguments ignored) and
    // allow one unfolding per pair on a path (setops::UNFOLDINGS, Unfoldings); past it
    // diff subtracts nothing and could_match answers an overlap resting on the innermost
    // pair in progress. Chosen from the 1600 generated selects (perf instructions,
    // --no-cache --check, 30 s): main's 53 timeouts -> 2 (r669 and r1088, both in
    // matching_type's contains: matching-type-contains-dag-01); of the 1547 main
    // finishes, 24 dead-arm refusals become accepted, 17 become missing-case refusals, 3
    // refuse a later dead arm, none accepted is refused; k = 2 moves 10 and k = 3 3, but
    // rnd13 costs 2.24G / 12.01G / 533G at k = 1 / 2 / 3. Families (cy, ec, n, p, sh,
    // sh235e2) keep every verdict; ecs400x32 4.35G -> 4.40G; grow1, grow4-6, gr1-7, hd1,
    // name1 now end refused by rep_collision's growth rule; grow2 and grow3 still time
    // out, now in contains' set_covers_by_distribution. Pins:
    // typ::setops::tests::diff_over_error_cycles_unfolds_each_pair_of_definitions_once
    // (pairs 12/18/32/61; 12/24/43/178 without),
    // diff_over_names_recurring_through_errors_ends and
    // typ::matches::tests::could_match_unfolds_growing_definitions_once (hang without);
    // lang::select::coverage_over_{names_recurring_through_errors,generated_error_cycles,growing_parameters}_ends
    // (mainhang, rnd13, grow5: killed on base-r16),
    // dead_arm_over_shared_tag_cycles_refused (sh235e2 stays refused).
    fn diff_int(&self, env: &Env, hist: &mut DiffHist, t: &Self) -> Result<Self> {
        ensure_sufficient(|| self.diff_inner(env, hist, t))
    }

    fn diff_inner(&self, env: &Env, hist: &mut DiffHist, t: &Self) -> Result<Self> {
        match (self, t) {
            (Type::Ref(tr0), Type::Ref(tr1)) if same_ref(tr0, tr1) => {
                Ok(Type::Primitive(BitFlags::empty()))
            }
            (t0 @ Type::Ref(_), t1) | (t0, t1 @ Type::Ref(_)) => {
                let key = (hist.hist.ref_id(t0, env), hist.hist.ref_id(t1, env));
                let kept = key.0.is_some() && key.1.is_some() && !keyed_loosely(self);
                if kept
                    && memo_on()
                    && let Some(k) = hist.done.get().and_then(|m| m.get(&key))
                {
                    return Ok(k.answer.clone());
                }
                if let Some(r) = hist.hist.get(&key) {
                    hist.cycled = true;
                    return Ok(r.clone());
                }
                // a pair met again inside its own difference subtracts
                // nothing: assuming it covered would drop what it did not
                let defs = def_pair(env, t0, t1);
                if hist.unfoldings.again(&defs).is_some() {
                    hist.cycled = true;
                    return Ok(self.clone());
                }
                #[cfg(test)]
                DIFF_PAIRS.with(|n| n.set(n.get() + 1));
                let t0 = t0.lookup_ref(env)?;
                let t1 = t1.lookup_ref(env)?;
                hist.hist.insert(key, self.clone());
                hist.unfoldings.enter(defs, key);
                let outer = mem::replace(&mut hist.cycled, false);
                let r = t0.diff_int(env, hist, &t1);
                hist.unfoldings.leave(&defs);
                hist.hist.remove(&key);
                let cycled = mem::replace(&mut hist.cycled, outer);
                hist.cycled |= cycled;
                if kept
                    && !cycled
                    && let Ok(d) = &r
                {
                    hist.done.get_mut().insert(key, Kept::new(d.clone(), self, t));
                }
                r
            }
            (Type::App(c, a), t) if let Some(filled) = Type::app_filled(c, a) => {
                filled.diff_int(env, hist, t)
            }
            (t0, Type::App(c, a)) if let Some(filled) = Type::app_filled(c, a) => {
                t0.diff_int(env, hist, &filled)
            }
            (Type::App(..), Type::Any) => Ok(Type::Primitive(BitFlags::empty())),
            (Type::TVar(tv), t @ Type::App(..)) | (t @ Type::App(..), Type::TVar(tv))
                if let Some(b) = tv.binding() =>
            {
                match self {
                    Type::TVar(_) => b.diff_int(env, hist, t),
                    _ => t.diff_int(env, hist, &b),
                }
            }
            (Type::App(..), _)
            | (_, Type::App(..))
            | (Type::Hole, _)
            | (_, Type::Hole)
            | (Type::Bound(_), _)
            | (_, Type::Bound(_)) => Ok(if same_type(self, t) {
                Type::Primitive(BitFlags::empty())
            } else {
                self.clone()
            }),
            (Type::Set(s0), Type::Set(s1)) => {
                let mut s: LPooled<Vec<Type>> = LPooled::take();
                for i in 0..s0.len() {
                    s.push(s0[i].clone());
                    for j in 0..s1.len() {
                        s[i] = s[i].diff_int(env, hist, &s1[j])?
                    }
                }
                Ok(Self::flatten_set(s.drain(..)))
            }
            (Type::Set(s), t) => {
                let mut diffs: LPooled<Vec<Type>> = LPooled::take();
                let mut partial = false;
                for m in s.iter() {
                    let d = m.diff_int(env, hist, t)?;
                    if !is_empty_ty(&d) && d != *m {
                        partial = true;
                    }
                    diffs.push(d);
                }
                if partial {
                    Ok(Self::flatten_set(diffs.drain(..)))
                } else {
                    diffs.retain(|d| !is_empty_ty(d));
                    Ok(match diffs.len() {
                        0 => Type::Primitive(BitFlags::empty()),
                        1 => diffs.pop().unwrap(),
                        _ => Type::Set(Arc::from_iter(diffs.drain(..))),
                    })
                }
            }
            (t, Type::Set(s)) => {
                let mut t = t.clone();
                for st in s.iter() {
                    t = t.diff_int(env, hist, st)?;
                }
                Ok(t)
            }
            (Type::Tuple(t0), Type::Tuple(t1)) => {
                if same_resolved(t0.iter(), t1.iter()) {
                    Ok(Type::Primitive(BitFlags::empty()))
                } else {
                    Ok(self.clone())
                }
            }
            (Type::Struct(t0), Type::Struct(t1)) => {
                let same = t0.len() == t1.len()
                    && t0.iter().zip(t1.iter()).all(|((n0, _, _), (n1, _, _))| n0 == n1)
                    && same_resolved(
                        t0.iter().map(|(_, t, _)| t),
                        t1.iter().map(|(_, t, _)| t),
                    );
                if same {
                    Ok(Type::Primitive(BitFlags::empty()))
                } else {
                    Ok(self.clone())
                }
            }
            (Type::Variant(tg0, t0, _), Type::Variant(tg1, t1, _)) => {
                if tg0 == tg1 && same_resolved(t0.iter(), t1.iter()) {
                    Ok(Type::Primitive(BitFlags::empty()))
                } else {
                    Ok(self.clone())
                }
            }
            (Type::Map { .. }, Type::Map { .. }) => {
                if same_type(self, t) {
                    Ok(Type::Primitive(BitFlags::empty()))
                } else {
                    Ok(self.clone())
                }
            }
            (Type::Map { .. }, Type::Primitive(p)) => {
                if p.contains(Typ::Map) {
                    Ok(Type::Primitive(BitFlags::empty()))
                } else {
                    Ok(self.clone())
                }
            }
            (Type::Primitive(p), Type::Map { key, value }) => {
                if is_any(key) && is_any(value) {
                    let mut p = *p;
                    p.remove(Typ::Map);
                    Ok(Type::Primitive(p))
                } else {
                    Ok(Type::Primitive(*p))
                }
            }
            (Type::Fn(_), Type::Fn(_)) => {
                if same_type(self, t) {
                    Ok(Type::Primitive(BitFlags::empty()))
                } else {
                    Ok(self.clone())
                }
            }
            // Bindings are cloned out: the recursion may lock these cells.
            (Type::TVar(tv0), t1 @ Type::TVar(tv1)) => {
                if tv0.same_cell(tv1) {
                    return Ok(Type::Primitive(BitFlags::empty()));
                }
                Ok(match (tv0.binding(), tv1.binding()) {
                    (None, _) => Type::TVar(tv0.clone()),
                    (Some(t0), None) => t0.diff_int(env, hist, t1)?,
                    (Some(t0), Some(t1)) => t0.diff_int(env, hist, &t1)?,
                })
            }
            (Type::TVar(tv), t) => Ok(match tv.binding() {
                Some(tv) => tv.diff_int(env, hist, t)?,
                None => self.clone(),
            }),
            (t, Type::TVar(tv)) => Ok(match tv.binding() {
                Some(tv) => t.diff_int(env, hist, &tv)?,
                None => self.clone(),
            }),
            // A collection that fails an element type test may still hold
            // elements that pass it: one that is not all `t1` is any
            // collection of `t0`, so only a `t1` covering every element
            // subtracts anything.
            (Type::List(t0), Type::List(t1)) | (Type::Array(t0), Type::Array(t1)) => {
                let covered = t0 == t1
                    || matches!(
                        t0.diff_int(env, hist, t1)?,
                        Type::Primitive(p) if p.is_empty()
                    );
                match covered {
                    true => Ok(Type::Primitive(BitFlags::empty())),
                    false => Ok(self.clone()),
                }
            }
            (Type::Primitive(p), Type::Array(t)) => {
                if is_any(t) {
                    let mut s = *p;
                    s.remove(Typ::Array);
                    Ok(Type::Primitive(s))
                } else {
                    Ok(Type::Primitive(*p))
                }
            }
            (Type::Array(_), Type::Primitive(p)) => {
                if p.contains(Typ::Array) {
                    Ok(Type::Primitive(BitFlags::empty()))
                } else {
                    Ok(self.clone())
                }
            }
            (_, Type::Any) => Ok(Type::Primitive(BitFlags::empty())),
            (Type::Any, _) => Ok(Type::Any),
            (Type::Primitive(s0), Type::Primitive(s1)) => {
                let mut s = *s0;
                s.remove(*s1);
                Ok(Type::Primitive(s))
            }
            (Type::Primitive(p), Type::Error(e)) => {
                if is_any(e) {
                    let mut s = *p;
                    s.remove(Typ::Error);
                    Ok(Type::Primitive(s))
                } else {
                    Ok(Type::Primitive(*p))
                }
            }
            (Type::Error(_), Type::Primitive(p)) => {
                if p.contains(Typ::Error) {
                    Ok(Type::Primitive(BitFlags::empty()))
                } else {
                    Ok(self.clone())
                }
            }
            (Type::Error(e0), Type::Error(e1)) => {
                if e0 == e1 {
                    Ok(Type::Primitive(BitFlags::empty()))
                } else {
                    match e0.diff_int(env, hist, e1)? {
                        Type::Primitive(p) if p.is_empty() => {
                            Ok(Type::Primitive(BitFlags::empty()))
                        }
                        d => Ok(Type::Error(Arc::new(d))),
                    }
                }
            }
            (Type::ByRef(m0, t0), Type::ByRef(m1, t1)) => {
                if (m0 == m1 || *m1 == super::Mutability::Shared)
                    && union_identical(t0, t1)
                {
                    Ok(Type::Primitive(BitFlags::empty()))
                } else {
                    Ok(self.clone())
                }
            }
            (Type::Abstract { .. }, Type::Abstract { .. }) if same_type(self, t) => {
                Ok(Type::Primitive(BitFlags::empty()))
            }
            (Type::Abstract { .. }, _)
            | (_, Type::Abstract { .. })
            | (Type::Fn(_), _)
            | (_, Type::Fn(_))
            | (Type::Array(_), _)
            | (_, Type::Array(_))
            | (Type::List(_), _)
            | (_, Type::List(_))
            | (Type::Tuple(_), _)
            | (_, Type::Tuple(_))
            | (Type::Struct(_), _)
            | (_, Type::Struct(_))
            | (Type::Variant(_, _, _), _)
            | (_, Type::Variant(_, _, _))
            | (Type::ByRef(..), _)
            | (_, Type::ByRef(..))
            | (Type::Error(_), _)
            | (_, Type::Error(_))
            | (Type::Primitive(_), _)
            | (_, Type::Primitive(_))
            | (Type::Bottom, _)
            | (Type::Map { .. }, _) => Ok(self.clone()),
        }
    }

    // CR claude for claude: [perf/low] A select's check over a tuple DAG of names
    // whose only way back is through Error grows two to three times per level from
    // depth 14, on main as on cr46-X: Ti = (Ti+1, Ti+1), Tn = [Error<T0>, `Z], a twin
    // S with `Y, `let v: S0 = never(); select v { T0 as x => 0, _ => 1 }`.
    // --no-cache --check, one eval thread, perf instructions:u, main ec53d4846: rt14
    // 5.3G, rt16 10.9G, rt18 29.7G, rt22 501G (an empty program 2.2G). Callgrind of
    // rt16 on cr46-X (5.47G) puts the growth in contains_int under matching_type_int,
    // through Select::typecheck0_with and Type::narrow_pattern (contains_dispatch
    // 0.54G self, ContainsHist::assuming and its unfolding map's inserts next), not
    // in could_match, which walks each pair of the family once since
    // found-assumed-true-01. Found by lane X's r9 review (as main's own growth beside
    // its item 2 finding). probes: ~/ptmp/cr29/scratch/r9-X/r9fix/r1/rt14.gx ..
    // rt22.gx, written by ~/ptmp/cr29/scratch/review-r9/X-r1-1/perf/mkrt.py
    // (matching-type-contains-dag-01)
    // 2026-11-03 claude: cr48-X2 (from main 8814f118b): Type::needs_split keeps the
    // pairs of shared nodes it found to need no split (NodePairs; exact, since
    // needs_split is a pure test of the pair and a true ends the walk), so narrowing
    // S0 to what T0 takes of it asks each pair once: 174 split steps at depth 12,
    // 16382 without the memo (pin
    // typ::setops::tests::narrowing_over_a_tuple_dag_takes_each_pair_once). Still
    // open: each containment probe of matching_type starts a fresh ContainsHist,
    // whose memo keeps only verdicts that assumed no pair in progress, and (Ti+1,
    // Ti+1) against (Si+1, Si+1) meets its own pair again at Tn's Error<T0>, so every
    // true below it is walked once per path. Perf instructions, --no-cache --check,
    // one eval thread, base-r11 (8814f118b) / cr48-X2: rt14 2.35G / 2.18G, rt16 5.48G
    // / 4.79G, rt18 17.94G / 15.20G. A shared walk across the probes (cr48-X
    // ce6523c7c) was tried and found inexact by review r11: contains' verdict for a
    // pair depends on where the walk entered its cycle (a distribution question met
    // again answers false, a pair of names true), and the probes it skipped had
    // filled the TypeRef cells same_definition and def_key read
    // (same-definition-fill-01), so a capture type depended on the statements before
    // the select (X-r1-1 t/m830c.gx; X-r1-2 rgt/v5order.gx, v5ann.gx; pin
    // lang::select::a_capture_type_is_the_same_at_every_select). An exact shared walk
    // needs verdicts that do not depend on where a cycle was entered (one assumption
    // for a question met again, or trues held as resting on the pairs and questions
    // in progress, as could_match's Rests holds them) and walks that do not read
    // which cells earlier walks filled. Evidence:
    // ~/ptmp/cr29/scratch/review-r11/X-r1-1/, X-r1-2/, X-r2-1/.
    // 2026-11-05 claude: Lane U (round 16, diff-cycle-hang-01 at k = 1): generated
    // p1088 still times out at 30 s, in Select::typecheck0_with ->
    // PatternNode::capture_type -> matching_type -> contains
    // (set_covers_by_distribution); so does generated r669 (gen_rnd.py seed 669), on
    // the same path. grow2.gx and grow3.gx (r11-X/fix) now time out in contains'
    // set_covers_by_distribution too, where base-r16 timed out in diff.
    pub fn matching_type(&self, env: &Env, reaching: &Self) -> Result<Self> {
        let refs = LPooled::take();
        let mut walk = MatchingTypes { memo: RefHist::new(), refs, inside: false };
        self.matching_type_int(env, &mut walk, reaching)
    }

    /// [`Self::matching_type`], where a part of `reaching` holding a rigid
    /// cell, whose values no pattern takes apart, is this test's part: the
    /// types of the binds inside it.
    pub fn matching_parts(&self, env: &Env, reaching: &Self) -> Result<Self> {
        let refs = LPooled::take();
        let mut walk = MatchingTypes { memo: RefHist::new(), refs, inside: true };
        self.matching_type_int(env, &mut walk, reaching)
    }

    fn matching_type_int(
        &self,
        env: &Env,
        walk: &mut MatchingTypes,
        reaching: &Self,
    ) -> Result<Self> {
        ensure_sufficient(|| {
            let key = (walk.memo.ref_id(self, env), walk.memo.ref_id(reaching, env));
            if let Some(m) = walk.memo.get(&key) {
                return Ok(m.answer.clone());
            }
            let typ = self.matching_type_inner(env, walk, reaching)?;
            walk.memo.insert(key, Kept::new(typ.clone(), self, reaching));
            Ok(typ)
        })
    }

    fn matching_type_inner(
        &self,
        env: &Env,
        walk: &mut MatchingTypes,
        reaching: &Self,
    ) -> Result<Self> {
        if strictly_identical(self, reaching) {
            return Ok(self.clone());
        }
        match (self, reaching) {
            (Self::TVar(tv), t) => match tv.binding() {
                Some(p) => p.matching_type_int(env, walk, t),
                None => Ok(t.clone()),
            },
            (p, Self::TVar(tv)) => match tv.binding() {
                Some(t) => p.matching_type_int(env, walk, &t),
                None if tv.cell_constraints().contains(&Self::Bound(Bound::NoError)) => {
                    p.diff(env, &Self::Primitive(Typ::Error.into()))
                }
                None => Ok(p.clone()),
            },
            (p @ Self::Ref(_), t) | (p, t @ Self::Ref(_)) => {
                let definition = |t: &Type| match t {
                    Self::Ref(tr) => tr.resolve_in(env).map(|r| r.def_key()),
                    _ => None,
                };
                let key = (definition(p), definition(t));
                if !walk.refs.insert(key) {
                    return Ok(p.clone());
                }
                let r =
                    p.lookup_ref(env)?.matching_type_int(env, walk, &t.lookup_ref(env)?);
                walk.refs.remove(&key);
                r
            }

            (Self::Any, t) | (t, Self::Any) => Ok(t.clone()),
            (Self::Primitive(p), Self::Primitive(t)) => Ok(Self::Primitive(*p & *t)),
            (Self::Set(ps), t) | (t, Self::Set(ps)) => {
                let mut matched: LPooled<Vec<Type>> = LPooled::take();
                for p in ps.iter() {
                    let m = if matches!(self, Self::Set(_)) {
                        p.matching_type_int(env, walk, t)?
                    } else {
                        t.matching_type_int(env, walk, p)?
                    };
                    if !m.is_uninhabited() {
                        matched.push(m);
                    }
                }
                Ok(Self::flatten_set(matched.drain(..)))
            }
            (p, t) => {
                let same_shape = match (p, t) {
                    (Self::Array(_), Self::Array(_))
                    | (Self::List(_), Self::List(_))
                    | (Self::Error(_), Self::Error(_))
                    | (Self::Map { .. }, Self::Map { .. }) => true,
                    (Self::Tuple(a), Self::Tuple(b)) => a.len() == b.len(),
                    (Self::Variant(a, ats, _), Self::Variant(b, bts, _)) => {
                        a == b && ats.len() == bts.len()
                    }
                    (Self::Struct(a), Self::Struct(b)) => {
                        a.len() == b.len()
                            && a.iter().zip(b.iter()).all(|((a, _, _), (b, _, _))| a == b)
                    }
                    _ => false,
                };
                let rigid_inside = walk.inside && same_shape && t.has_rigid();
                if !rigid_inside
                    && !p.has_unbound()
                    && p.contains_with_flags(BitFlags::empty(), env, t)?
                {
                    return Ok(t.clone());
                }
                if !t.has_unbound() && t.contains_with_flags(BitFlags::empty(), env, p)? {
                    return Ok(p.clone());
                }
                if !p.could_match(env, t)? {
                    return Ok(Self::Bottom);
                }
                if !same_shape {
                    return Ok(p.clone());
                }
                let mut parts: LPooled<Vec<Type>> = LPooled::take();
                t.for_each_child(&mut |c| parts.push(c.clone()));
                let mut parts = parts.drain(..);
                let mut error = None;
                let narrowed = p.cow_children(&mut |c| {
                    let t = parts.next().expect("matching shape");
                    match c.matching_type_int(env, walk, &t) {
                        Ok(n) => (!strictly_identical(c, &n)).then_some(n),
                        Err(e) => {
                            error = Some(e);
                            None
                        }
                    }
                });
                match error {
                    Some(e) => Err(e),
                    None => Ok(narrowed.unwrap_or_else(|| p.clone())),
                }
            }
        }
    }

    pub fn diff(&self, env: &Env, t: &Self) -> Result<Self> {
        let r = self.diff_int(env, &mut DiffHist::new(), t)?;
        if diff_already_normal(self, &r) { Ok(r) } else { Ok(r.normalize()) }
    }
}

/// `Any`, read through bound type variables.
fn is_any(t: &Type) -> bool {
    t.with_deref(|t| matches!(t, Some(Type::Any)))
}

/// Equality read through bound type variables.
fn same_type(t0: &Type, t1: &Type) -> bool {
    let bound = |t: &Type| cell_beneath(t, |tv, _| tv.binding().is_some());
    t0 == t1 || (bound(t0) || bound(t1)) && same_normal(t0, t1)
}

/// Equality of both sides read through bound type variables, normalized.
fn same_normal(t0: &Type, t1: &Type) -> bool {
    t0.resolve_tvars().normalize() == t1.resolve_tvars().normalize()
}

/// Positional [`same_normal`]. `Any` in the subtrahend covers anything.
fn same_resolved<'a, 'b>(
    t0: impl IntoIterator<Item = &'a Type>,
    t1: impl IntoIterator<Item = &'b Type>,
) -> bool {
    let mut t0 = t0.into_iter();
    let mut t1 = t1.into_iter();
    loop {
        match (t0.next(), t1.next()) {
            (None, None) => return true,
            (Some(a), Some(b)) => {
                if !is_any(b) && a != b && !same_normal(a, b) {
                    return false;
                }
            }
            _ => return false,
        }
    }
}

#[cfg(test)]
thread_local! {
    pub(super) static DIFF_PAIRS: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
    static NO_MEMO: std::cell::Cell<bool> = const { std::cell::Cell::new(false) };
}

/// Whether diff and could_match read their memos of pairs: a test
/// compares their answers with the walks' without them.
#[inline(always)]
pub(super) fn memo_on() -> bool {
    #[cfg(test)]
    return !NO_MEMO.with(|m| m.get());
    #[cfg(not(test))]
    true
}

#[cfg(test)]
mod tests {
    use crate::{
        PrintFlag,
        env::Env,
        format_with_flags,
        typ::{
            Type,
            cast::tests::{parsed, typed, union_dag},
            matches::MATCH_PAIRS,
        },
    };
    use ahash::AHashMap;
    use netidx_value::Typ;
    use triomphe::Arc;

    /// `t` at the root of `env`, its variable 'c bound to i64.
    fn bound(env: &Env, t: &str) -> Type {
        let t = parsed(env, t);
        let mut cells = AHashMap::default();
        t.collect_tvars(&mut cells);
        cells["c"].bind(Type::Primitive(Typ::I64.into()));
        t
    }

    // Each instantiation of W expands to an abstract type built anew, so
    // the walk meets Box<i64> and Box<string> at fresh addresses.
    #[test]
    fn matching_type_tells_fresh_instantiations_apart() {
        let (env, _) =
            typed(&[("Box<'a>", "Abstract<'a>"), ("W<'a>", "[Box<'a>, `X]")], "null");
        let test = parsed(&env, "[W<i64>, W<string>]");
        let reaching = parsed(&env, "[Box<i64>, Box<string>, `X]");
        for _ in 0..16 {
            let m = test.matching_type(&env, &reaching).unwrap().normalize();
            assert_eq!(m.to_string(), "[`X, Box<i64>, Box<string>]");
        }
    }

    // diff reads a bound cell as its binding wherever it compares parts:
    // a map's, a function's, an abstract type's parameters and an open
    // constructor's argument.
    #[test]
    fn diff_reads_bound_cells_through() {
        let (env, _) =
            typed(&[("S", "Map<i64, i64>"), ("Box<'a>", "Abstract<'a>")], "null");
        let bound = |t: &str| bound(&env, t);
        let expanded = |t: Type| t.lookup_ref(&env).unwrap();
        for (t0, t1) in [
            (bound("Map<'c, i64>"), parsed(&env, "Map<i64, i64>")),
            (parsed(&env, "Map<i64, i64>"), bound("Map<'c, i64>")),
            (
                bound("[Error<Map<'c, i64>>, Array<Map<i64, i64>>]"),
                parsed(&env, "[Error<S>, Array<S>]"),
            ),
            (bound("fn(x: 'c) -> i64"), parsed(&env, "fn(x: i64) -> i64")),
            (expanded(bound("Box<'c>")), expanded(parsed(&env, "Box<i64>"))),
            (bound("'k<'c>"), parsed(&env, "'k<i64>")),
        ] {
            let d = t0.diff(&env, &t1).unwrap();
            assert!(d.is_uninhabited(), "{t0:?} - {t1:?} = {d:?}");
        }
    }

    // A cell bound to Any is Any where diff asks whether a part covers
    // anything: at a tuple's position and as a map's, an array's or an
    // error's every value.
    #[test]
    fn diff_reads_a_cell_bound_to_any_as_any() {
        let (env, _) = typed(&[], "null");
        let any = |t: &str| {
            let t = parsed(&env, t);
            let mut cells = AHashMap::default();
            t.collect_tvars(&mut cells);
            cells.values().for_each(|c| c.bind(Type::Any));
            t
        };
        let d = parsed(&env, "(i64, string)").diff(&env, &any("('c, string)")).unwrap();
        assert_eq!(d.to_string(), "[]");
        for (p, t) in [
            (Typ::Array, "Array<'c>"),
            (Typ::Map, "Map<'c, 'd>"),
            (Typ::Error, "Error<'c>"),
        ] {
            let d = Type::Primitive(p | Typ::I64).diff(&env, &any(t)).unwrap();
            assert_eq!(d.to_string(), "i64", "{p:?} - {t}");
        }
    }

    /// Every difference of two of `pool` and could_match verdict of two,
    /// and could_match of each of `pool` against each difference (the
    /// question a select's next arm asks), bound cells printed.
    fn memo_answers(env: &Env, pool: &[Type], memo: bool) -> Vec<String> {
        super::NO_MEMO.with(|m| m.set(!memo));
        let mut out = vec![];
        format_with_flags(PrintFlag::DerefTVars, || {
            for s in pool {
                for t in pool {
                    let d = s.diff(env, t).unwrap();
                    out.push(format!(
                        "{s} - {t} = {d}, {}",
                        s.could_match(env, t).unwrap()
                    ));
                    for a in pool {
                        out.push(format!(
                            "{a} ~ {d}: {}",
                            a.could_match(env, &d).unwrap()
                        ));
                    }
                }
            }
        });
        super::NO_MEMO.with(|m| m.set(false));
        out
    }

    fn memos_agree(env: &Env, pool: &[Type]) {
        let without = memo_answers(env, pool, false);
        let with = memo_answers(env, pool, true);
        for (a, b) in without.iter().zip(with.iter()) {
            assert_eq!(a, b);
        }
    }

    fn written_memos_agree(defs: &[(&str, &str)], pool: &[&str]) {
        let (env, _) = typed(defs, "null");
        let pool: Vec<Type> = pool.iter().map(|t| parsed(&env, t)).collect();
        memos_agree(&env, &pool)
    }

    /// `` (`A, .. `A, last) ``, `n` `A.
    fn long_tuple(n: usize, last: &str) -> String {
        format!("({}{last})", "`A, ".repeat(n))
    }

    // Over names that recur through payloads (a pair met in progress
    // subtracts nothing there, so where a cycle is cut decides what is
    // left), over instantiations of an abstract type (built anew at each
    // expansion), over a walk that grows past `NodePairs::EAGER` while a
    // pair is in progress (K0, K1), and over cells (a key reads a bound
    // one as its binding, and pairs a function type's by position), bound
    // cells included as a program binds them, to a recursive name's
    // expansion, the memos change no answer.
    #[test]
    fn diff_and_could_match_memos_change_no_answer() {
        written_memos_agree(
            &[
                ("At", "[Error<Bt>, Error<Ct>, `R]"),
                ("Bt", "[Error<At>, `P]"),
                ("Ct", "[Array<Ct>, Error<Bt>]"),
            ],
            &["At", "Bt", "Ct", "[At, Bt]", "[Bt, Ct]", "Error<Array<Ct>>", "`R"],
        );
        written_memos_agree(
            &[
                ("L", "[Error<M>, `A]"),
                ("M", "[Error<L>, `B, `C]"),
                ("X", "[Error<Y>, `A]"),
                ("Y", "[Error<X>, `B]"),
                ("Z", "[X, Y]"),
            ],
            &["L", "M", "X", "Y", "Z", "[L, M]"],
        );
        written_memos_agree(
            &[("H", "[Error<Error<K>>, `Q]"), ("K", "[Error<H>, Error<K>, `P]")],
            &["H", "K", "[H, K]", "Error<K>", "`Q"],
        );
        written_memos_agree(
            &[("Box<'a>", "Abstract<'a>"), ("W<'a>", "[Box<'a>, `X]")],
            &[
                "Box<i64>",
                "[Box<i64>, Box<string>]",
                "W<i64>",
                "W<string>",
                "[W<i64>, W<f64>]",
            ],
        );
        for n in [16, 24] {
            let (k0, k1) = (long_tuple(n, "`X"), long_tuple(n, "`Y"));
            written_memos_agree(
                &[
                    ("K0", &k0),
                    ("K1", &k1),
                    ("L0", "(N0, `P)"),
                    ("L1", "(N1, `Q)"),
                    ("N0", "[Error<K0>, Error<L0>, `Y]"),
                    ("N1", "[Error<K1>, Error<L1>, `W]"),
                ],
                &[
                    "[Error<K0>, Error<L0>, Error<N0>]",
                    "[Error<K1>, Error<L1>, Error<N1>]",
                    "Error<N0>",
                    "Error<Error<L1>>",
                    "[Error<K1>, Error<L1>, `Z]",
                ],
            );
        }
        let (env, _) = typed(
            &[
                ("R<'p>", "Map<'p, i64>"),
                ("S", "Map<i64, i64>"),
                ("Box<'a>", "Abstract<'a>"),
                ("W<'a>", "[Box<'a>, `X]"),
                ("E<'p>", "[Error<R<'p>>, Array<R<'p>>]"),
                ("P", "[i64, (i64, `X)]"),
            ],
            "null",
        );
        let pool = [
            bound(&env, "R<'c>"),
            parsed(&env, "R<i64>"),
            bound(&env, "[Error<R<'c>>, Array<R<i64>>]"),
            bound(&env, "[Error<R<i64>>, Array<R<'c>>]"),
            bound(&env, "[Error<R<'c>>, Error<R<i64>>]"),
            bound(&env, "[R<'c>, R<i64>]"),
            parsed(&env, "[Error<S>, Array<S>]"),
            bound(&env, "W<'c>"),
            parsed(&env, "W<i64>"),
            bound(&env, "E<'c>"),
            parsed(&env, "E<i64>"),
            parsed(&env, "Error<R<i64>>"),
            parsed(&env, "[Error<R<'x>>, Error<R<'y>>, `D('x)]"),
            parsed(&env, "`D('z)"),
            parsed(&env, "[Error<fn(x: 'a) -> 'a>, Error<fn(x: 'b) -> 'b>]"),
            parsed(&env, "Error<fn(x: 'q) -> 'q>"),
        ];
        memos_agree(&env, &pool);
        let any = |t: &str| {
            let t = parsed(&env, t);
            let mut cells = AHashMap::default();
            t.collect_tvars(&mut cells);
            cells.values().for_each(|c| c.bind(Type::Any));
            t
        };
        let pool = [
            parsed(&env, "[Error<R<i64>>, Array<i64>, (i64, `X)]"),
            any("[Error<R<'c>>, Array<'d>, ('e, `X)]"),
            any("[Error<R<'c>>, Error<R<Any>>]"),
            parsed(&env, "[Error<R<Any>>, Array<Any>, (Any, `X)]"),
            any("[Array<'c>, Array<Any>, ('d, `X), (Any, `X)]"),
            parsed(&env, "[Error<S>, Array<i64>]"),
            Type::Set(Arc::from_iter([
                parsed(&env, "Error<P>"),
                parsed(&env, "Error<Error<P>>"),
            ])),
            Type::Set(Arc::from_iter([
                any("Error<('d, `X)>"),
                parsed(&env, "Error<Error<(Any, `X)>>"),
            ])),
        ];
        memos_agree(&env, &pool);
        let (env, _) = typed(
            &[
                ("Box<'a>", "Abstract<'a>"),
                ("W<'a>", "[Box<'a>, `X]"),
                ("N0", "[(N0, `U1), Error<N5>, `E]"),
                ("N1", "[Error<N5>, Map<string, N0>]"),
                ("N2", "[W<N0>, W<N1>]"),
                ("N4", "[Box<i64>, Error<Error<N5>>, N1, `T0(N5)]"),
                ("N5", "[(N4, `U0), N4]"),
            ],
            "null",
        );
        let expanded = |t: &str, name: &str| {
            let t = parsed(&env, t);
            let mut cells = AHashMap::default();
            t.collect_tvars(&mut cells);
            cells["c"].bind(parsed(&env, name).lookup_ref(&env).unwrap());
            t
        };
        let pool = [
            expanded("[Error<'c>, Error<N5>]", "N5"),
            expanded("Error<'c>", "N1"),
            expanded("`T0('c)", "N4"),
            expanded("[Error<'c>, `T0(N5)]", "N5"),
            expanded("W<'c>", "N0"),
            parsed(&env, "[N1, N2]"),
            parsed(&env, "Error<N5>"),
            parsed(&env, "N4"),
            parsed(&env, "N5"),
        ];
        memos_agree(&env, &pool);
    }

    // Generated names over the K0/K1 shape, Error towers over them and
    // unions of towers: a select's coverage composes could_match over
    // diff's output, and the memos change none of it.
    #[test]
    fn memos_change_no_composed_answer() {
        struct Rng(u64);
        impl Rng {
            fn below(&mut self, n: usize) -> usize {
                self.0 ^= self.0 << 13;
                self.0 ^= self.0 >> 7;
                self.0 ^= self.0 << 17;
                (self.0 % n as u64) as usize
            }
        }
        let err =
            |t: &str, k: usize| (0..k).fold(t.to_string(), |t, _| format!("Error<{t}>"));
        let tags = ["`P", "`Q", "`W", "`Y", "`Z"];
        for seed in 1..=40u64 {
            let mut r = Rng(seed.wrapping_mul(0x9E3779B97F4A7C15) | 1);
            let n = [2, 8, 16, 24, 40][r.below(5)];
            let mut defs: Vec<(String, String)> = vec![
                ("K0".into(), long_tuple(n, "`X")),
                ("K1".into(), long_tuple(n, "`Y")),
                ("L0".into(), format!("(N0, {})", tags[r.below(5)])),
                ("L1".into(), format!("(N1, {})", tags[r.below(5)])),
                ("N0".into(), format!("[Error<K0>, Error<L0>, {}]", tags[r.below(5)])),
                ("N1".into(), format!("[Error<K1>, Error<L1>, {}]", tags[r.below(5)])),
            ];
            let mut names: Vec<String> = defs.iter().map(|(n, _)| n.clone()).collect();
            for i in 0..1 + r.below(3) {
                let mut ms = std::collections::BTreeSet::from(["`Z".to_string()]);
                for _ in 0..2 + r.below(3) {
                    let m = match r.below(10) {
                        0..5 => err(&names[r.below(names.len())], 1 + r.below(2)),
                        5..7 => tags[r.below(5)].to_string(),
                        _ => names[r.below(names.len())].clone(),
                    };
                    ms.insert(m);
                }
                let ms: Vec<String> = ms.into_iter().collect();
                defs.push((format!("D{i}"), format!("[{}]", ms.join(", "))));
                names.push(format!("D{i}"));
            }
            let mut pool: Vec<String> = vec![
                "[Error<K0>, Error<L0>, Error<N0>]".into(),
                "[Error<K1>, Error<L1>, Error<N1>]".into(),
            ];
            for _ in 0..6 {
                pool.push(err(&names[r.below(names.len())], 1 + r.below(3)));
            }
            for _ in 0..4 {
                let pivot = names[r.below(names.len())].clone();
                let mut ms = std::collections::BTreeSet::from([
                    err(&pivot, 1),
                    err(&pivot, 2 + r.below(2)),
                ]);
                if r.below(2) == 0 {
                    ms.insert(err(&names[r.below(names.len())], 1 + r.below(3)));
                }
                let ms: Vec<String> = ms.into_iter().collect();
                pool.push(format!("[{}]", ms.join(", ")));
            }
            let defs: Vec<(&str, &str)> =
                defs.iter().map(|(n, b)| (&**n, &**b)).collect();
            let pool: Vec<&str> = pool.iter().map(|t| &**t).collect();
            written_memos_agree(&defs, &pool);
        }
    }

    /// `Ti = (Ti+1, Ti+1)` down to `Tn = [Error<T0>, leaf]`, and its
    /// twin `Si` with another leaf.
    fn tuple_dag(n: usize) -> Vec<(String, String)> {
        let mut defs = vec![];
        for (p, leaf) in [("T", "`Z"), ("S", "`Y")] {
            for i in 0..n {
                defs.push((format!("{p}{i}"), format!("({p}{}, {p}{})", i + 1, i + 1)));
            }
            defs.push((format!("{p}{n}"), format!("[Error<{p}0>, {leaf}]")));
        }
        defs
    }

    // Narrowing S0 to what a test of T0 takes of it, over a tuple DAG
    // whose only way back is through Error, asks each pair of shared
    // nodes once whether a union stands under a constructor.
    #[test]
    fn narrowing_over_a_tuple_dag_takes_each_pair_once() {
        use crate::typ::tvar::SPLIT_STEPS;
        let defs = tuple_dag(12);
        let defs: Vec<(&str, &str)> = defs.iter().map(|(n, b)| (&**n, &**b)).collect();
        let (env, t) = typed(&defs, "T0");
        let s = parsed(&env, "S0");
        let m = t.matching_type(&env, &s).unwrap();
        SPLIT_STEPS.with(|n| n.set(0));
        assert!(!s.narrow_pattern(&env, &m).unwrap());
        assert_eq!(SPLIT_STEPS.with(|n| n.get()), 174);
    }

    /// Error cycles `C<l>_j = [Error<C<l>_j+1 mod l>, tag]` of the lengths
    /// `ls`, position 0 tagged `` `Mm `` where `shared`, `S` the union of
    /// their heads, and `M0 = [Error<M0>, `Mm]`.
    fn error_cycles(ls: &[usize], shared: bool) -> Vec<(String, String)> {
        let mut defs = vec![];
        for &l in ls {
            for j in 0..l {
                let tag = match j == 0 && shared {
                    true => "`Mm".to_string(),
                    false => format!("`K{l}_{j}"),
                };
                defs.push((
                    format!("C{l}_{j}"),
                    format!("[Error<C{l}_{}>, {tag}]", (j + 1) % l),
                ));
            }
        }
        let heads: Vec<String> = ls.iter().map(|l| format!("C{l}_0")).collect();
        defs.push(("S".into(), format!("[{}]", heads.join(", "))));
        defs.push(("M0".into(), "[Error<M0>, `Mm]".into()));
        defs
    }

    fn diff_pairs(defs: &[(String, String)], t: &str, less: &str) -> (String, usize) {
        let defs: Vec<(&str, &str)> = defs.iter().map(|(n, b)| (&**n, &**b)).collect();
        let (env, t) = typed(&defs, t);
        super::DIFF_PAIRS.with(|n| n.set(0));
        let rest = t.diff(&env, &parsed(&env, less)).unwrap();
        (rest.to_string(), super::DIFF_PAIRS.with(|n| n.get()))
    }

    // diff unfolds a pair of definitions once: over Error cycles of coprime
    // lengths, whose exact residual repeats only after their lcm, the pairs
    // it walks grow with the sum of the lengths.
    #[test]
    fn diff_over_error_cycles_unfolds_each_pair_of_definitions_once() {
        let pairs =
            |ls: &[usize], shared| diff_pairs(&error_cycles(ls, shared), "M0", "S").1;
        let sets: [&[usize]; 4] =
            [&[2, 3], &[2, 3, 5], &[2, 3, 5, 7], &[2, 3, 5, 7, 11, 13]];
        let apart: Vec<usize> = sets.iter().map(|ls| pairs(ls, false)).collect();
        let shared: Vec<usize> = sets.iter().map(|ls| pairs(ls, true)).collect();
        assert_eq!((apart, shared), (vec![12, 18, 32, 61], vec![12, 18, 32, 61]));
    }

    // Names recurring through Error payloads, and a definition whose
    // parameters grow as it recurses, end.
    #[test]
    fn diff_over_names_recurring_through_errors_ends() {
        let defs: Vec<(String, String)> = [
            ("At", "[Error<Bt>, Error<Ct>, `R]"),
            ("Bt", "[Error<At>, `P]"),
            ("Ct", "[Array<Ct>, Error<Bt>]"),
            ("H", "[Error<Error<K>>, `Q]"),
            ("K", "[Error<H>, Error<K>, `P]"),
            ("G<'a>", "[i64, Error<G<Array<'a>>>]"),
            ("P", "[i64, Error<P>]"),
        ]
        .iter()
        .map(|(n, b)| (n.to_string(), b.to_string()))
        .collect();
        let ends = |t: &str, less: &str| diff_pairs(&defs, t, less).1;
        assert_eq!((ends("At", "K"), ends("G<i64>", "P")), (33, 1));
    }

    // A select over a DAG of named unions (Li = [Li-1, Mi-1], Mi = [Mi-1,
    // Li-1]) takes an arm's test off each name once, and refutes a test
    // nothing the names hold could match once per name, not once per path.
    #[test]
    fn diff_and_could_match_take_each_name_of_a_union_dag_once() {
        let defs = union_dag(20);
        let defs: Vec<(&str, &str)> = defs.iter().map(|(n, b)| (&**n, &**b)).collect();
        let (env, t) = typed(&defs, "L20");
        super::DIFF_PAIRS.with(|n| n.set(0));
        let rest = t.diff(&env, &parsed(&env, "`A")).unwrap();
        assert_eq!(rest.to_string(), "[`B, `C]");
        assert_eq!(super::DIFF_PAIRS.with(|n| n.get()), 39);
        MATCH_PAIRS.with(|n| n.set(0));
        assert!(!parsed(&env, "`D(i64)").could_match(&env, &t).unwrap());
        assert_eq!(MATCH_PAIRS.with(|n| n.get()), 39);
    }
}
