use crate::{
    env::Env,
    stack::ensure_sufficient,
    typ::{
        Bound, ContainsFlags, FnType, Kept, Lazy, NodePairs, RefHist, RefPair, TVar,
        Type, TypeRef, contains::on_copies, tvar::Level,
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

// XCR claude for claude: [bug/medium] matching_type's memo (MatchingTypes::memo) keeps
// every pair for the walk, keyed by RefHist::ref_id, which keys an abstract type by
// its parameters' address and an open cell by its cell's and holds neither: an
// expansion's parameters (built anew by lookup_ref) die with its frame, and the next
// instantiation's take their address and read the dead one's answer. main bbb06c0c6
// refuses `type Box<'a> = Abstract<'a>; type W<'a> = [Box<'a>, `X]; let v: [Box<i64>,
// Box<string>, `X] = Box("s"); select v { [W<i64>, W<string>] as b => select b {
// Box<string> as s => 1, _ => 2 } }` with "pattern Box<string> will never match [`X,
// Box<i64>]" in 11 runs of 12 (address reuse follows allocation order and AHash's
// seeds, so the program is not a deterministic probe): the capture lost Box<string>.
// With the members written (`[Box<i64>, Box<string>, `X] as b`) it prints 1. A unit
// walk of matching_type([W<i64>, W<string>], [Box<i64>, Box<string>, `X]) gives [`X,
// Box<i64>]; keeping each memoized pair's sides alive for the walk gives [`X,
// Box<i64>, Box<string>]: pin the fix with that walk. Found by lane X (round 7).
// probes: ~/ptmp/cr29/scratch/r8-X/mt1.gx, mt1b.gx (matching-type-pins-01)
// 2026-11-02 claude: MatchingTypes::memo now holds each pair's sides beside its
// answer (Matched), so an address its key reads stays its node's for the walk. Pin:
// graphix-types typ::setops::tests::matching_type_tells_fresh_instantiations_apart
// (the unit walk named above; gives [`X, Box<i64>] without the fix, every run).
// Env::typed_at_root now takes a body Abstract<rep> as an abstract typedef for such
// tests.
// 2026-11-03 claude: Matched is now Kept<A> (typ/mod.rs), shared by this memo, diff's
// memo of finished pairs (DiffHist::done) and could_match's refuted memo
// (diff-dag-01).
struct MatchingTypes {
    memo: RefHist<AHashMap<RefPair, Kept<Type>>>,
    refs: LPooled<AHashSet<(Option<usize>, Option<usize>)>>,
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
    /// The current walk met a pair in progress.
    cycled: bool,
}

impl DiffHist {
    fn new() -> Self {
        DiffHist { hist: RefHist::new(), done: Lazy::new(), cycled: false }
    }
}

/// Whether [`RefHist::ref_id`] may key `t` alike with a type that prints
/// apart: `t` holds a bound cell, keyed as its binding, or a cell in a
/// function type, paired by position. A reference's parameters are read,
/// its expansion is not.
fn keyed_loosely(t: &Type) -> bool {
    fn go(t: &Type, in_fn: bool) -> ControlFlow<()> {
        ensure_sufficient(|| match t {
            Type::TVar(tv) if in_fn || tv.binding().is_some() => ControlFlow::Break(()),
            Type::TVar(_) => ControlFlow::Continue(()),
            Type::Fn(_) => t.try_for_each_child(&mut |c| go(c, true)),
            t => t.try_for_each_child(&mut |c| go(c, in_fn)),
        })
    }
    go(t, false).is_break()
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
    Identity::<_, false>::new(false, every_input).same(t0, t1)
}

/// [`union_identical`] with every unbound cell by identity, inside a
/// function type too: one signature's variables are its own.
pub(crate) fn strictly_identical(t0: &Type, t1: &Type) -> bool {
    Identity::<_, false>::new(true, every_input).same(t0, t1)
}

/// The input predicate of an unsettled [`Identity`], which reads none.
fn every_input(_: &TVar) -> bool {
    true
}

/// [`union_identical`] of an arm's completion `test` and another member's
/// completion `other` once the check has decided what it will, but for
/// the cells that hold no value, which are any type: ⊥ on either side, an
/// unbound top-level cell (nothing decides it later) on either side, and
/// an unbound cell of `other` where `input` says no caller can hand it
/// one. Any other unbound cell is only itself.
// CR claude for eric: [bug/low] A later REPL input can bind a top-level cell an
// earlier input left open, so the earlier input's judgment (open beside open) no
// longer holds, and the coverage hole the fix of partial-hole-01 closes in a script
// stays open there: inputs `let b = true`, `let e1 = []`, `let e2 = []`, `let v =
// select b { true => {a: [1], c: e1}, false => {a: ["s"], c: e2} }`, `let r = select
// v { {a, ..} => "[a]" }`, then `e1 <- [1]`, `e2 <- ["w"]`, `r`, `b <- false` print
// "[1]", then "no arm matches: a coverage hole in the check" (main 5f4b4677f and
// cr31-P alike; GRAPHIX_ABORT_ON_NO_MATCH=1 dumps core). A `never()` let is safe:
// its input's check settles it to ⊥ and a later write is refused. Either the check's
// end settles every top-level cell it leaves open (an empty literal's element to ⊥,
// as a `let` over ⊥ is), which refuses `e1 <- [1]` in a later input, or a later
// input's write to an earlier input's cell is held to a copy, as a generalized
// binding's is. probe: ~/tmp/cr29/scratch/laneP/repl3.txt piped into `graphix
// --no-cache` (partial-hole-repl-01)
// 2026-10-31 claude: Eric ruled (round 5 D3, 10-31): refuse at the select. After the
// select makes the decisions its own arms make, its coverage is judged with every
// type still undecided treated as a declared type variable (one unknown type, equal
// only to itself), so the verdict holds whatever those types become; a program whose
// types end up equal is refused until annotated. This replaces the late checks
// (PendingSettle::OneMember, and cr32-P's open-member check, which does not land).
// It covers this CR too: the verdict no longer waits for the check's end, which a
// REPL input never reaches.
// 2026-11-02 claude: Waits on Eric's ruling of the next-discuss item "D3 lane V
// (cr33-V)" (~/ptmp/cr29/next-discuss.md). D3 is ruled (10-31); how it is built
// (rebuilt small from main, or more rounds on cr33-V) decides this fix. Readdressed
// to eric until then.
pub(crate) fn settled_identical(
    test: &Type,
    other: &Type,
    input: &dyn Fn(&TVar) -> bool,
) -> bool {
    Identity::<_, true>::new(false, input).same(test, other)
}

/// Identity of `t0` and `t1`; `SETTLED`, as [`settled_identical`] judges
/// an arm's completion `t0` and another member's `t1`.
struct Identity<I, const SETTLED: bool> {
    strict: bool,
    /// `SETTLED`: whether a caller can hand an unbound cell a value.
    input: I,
    /// Inside a function type: the cells paired so far, by address.
    pairs: Option<SmallVec<[(usize, usize); 4]>>,
    /// The nodes found identical outside a function type.
    found: NodePairs,
    /// Those found inside the function type being compared, which hold
    /// only under its pairing of cells.
    found_in_fn: NodePairs,
}

impl<I: Fn(&TVar) -> bool, const SETTLED: bool> Identity<I, SETTLED> {
    fn new(strict: bool, input: I) -> Self {
        Self {
            strict,
            input,
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
        if a.same_cell(b) || self.passes_any(a) || self.holds_none(b) {
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

    /// Whether the unbound test cell `a` passes any value: `SETTLED` and
    /// top-level.
    fn passes_any(&self, a: &TVar) -> bool {
        SETTLED && a.level() == Level::TOP
    }

    /// Whether the unbound cell `b` of the tested values holds none. In
    /// a function type, whose values one runtime form tests alike, a
    /// definition's cell pairs as [`union_identical`]'s do.
    fn holds_none(&self, b: &TVar) -> bool {
        SETTLED && (b.level() == Level::TOP || (self.pairs.is_none() && !(self.input)(b)))
    }

    /// Whether `SETTLED` and `t` is ⊥, which passes any value and holds
    /// none.
    fn settled_bottom(&self, t: &Type) -> bool {
        SETTLED && t.with_deref(|t| matches!(t, Some(Type::Bottom)))
    }

    #[inline(never)]
    fn same_inner(&mut self, t0: &Type, t1: &Type) -> bool {
        match (t0, t1) {
            (Type::TVar(a), Type::TVar(b)) => match (a.binding(), b.binding()) {
                (Some(x), Some(y)) => a.same_cell(b) || self.same(&x, &y),
                (None, None) => self.cells(a, b),
                (None, Some(t)) => self.passes_any(a) || self.settled_bottom(&t),
                (Some(t), None) => self.settled_bottom(&t) || self.holds_none(b),
            },
            (Type::TVar(a), t) | (t, Type::TVar(a)) if !SETTLED => {
                a.binding().is_some_and(|x| self.same(&x, t))
            }
            (Type::TVar(a), t) => match a.binding() {
                Some(x) => self.same(&x, t),
                None => self.passes_any(a) || self.settled_bottom(t),
            },
            (t, Type::TVar(b)) => match b.binding() {
                Some(y) => self.same(t, &y),
                None => self.settled_bottom(t) || self.holds_none(b),
            },
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
            (Type::Bottom, _) | (_, Type::Bottom) => SETTLED,
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

    // CR claude for claude: [bug/medium] diff does not end over some names that recur
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
    fn diff_int(&self, env: &Env, hist: &mut DiffHist, t: &Self) -> Result<Self> {
        ensure_sufficient(|| self.diff_inner(env, hist, t))
    }

    // XCR claude for claude: [perf/low] diff takes a pair of a reference and the
    // subtrahend apart at every path that reaches it, so over a DAG of named unions
    // (cast.rs union_dag: Li = [Li-1, Mi-1], Mi = [Mi-1, Li-1]) a select's coverage
    // (Reach::cover and Select::typecheck0_with, both calling diff) walks 2^depth:
    // `let v: L18 = `C; select v { `A => 1, `B => 2, `C => 3 }` costs 8.6G instructions
    // at --check (L16 4.5G, L20 31.9G; over `_ => 3` 2.7G flat) once union_members and
    // flatten_union_members expand each name once (union-members-dag-01); callgrind at
    // L16 puts 1.2G of the run's 2.6G under diff_inner. Its RefHist is a cycle guard
    // only, inserted and removed around each pair. A memo of a finished pair's
    // difference, kept only where it assumed no pair in progress (as ContainsHist's
    // verdicts are), would make it linear. probes:
    // ~/ptmp/cr29/scratch/review-r6/E2-r1-2/sel16.gx, sel18.gx, sel20.gx (diff-dag-01)
    // 2026-11-02 claude: Round 7 lane X built a memo of finished pairs (DiffHist::done)
    // and one of refuted pairs in could_match; three review rounds each found it moving
    // select verdicts against main: a hang through recursive Error payloads, address
    // reuse, the refuted memo shifting where could_match's found cache starts
    // (found-assumed-true-01), a bound cell keyed as its binding (diff-bound-cell-01),
    // and a shared output node reaching found's stale trues. Not landed: branch cr45-X
    // (cdcab0a9f, 326be068c, ca7f82513) keeps it; on it sel16-32 check flat at about
    // 1.34G and selp16/18/20 at 1.65/2.65/6.62G (main 3.24/8.99G). Fix
    // found-assumed-true-01 and diff-bound-cell-01 first; then the memo moves nothing
    // (the lane measured selp linear with both memos once found keeps only trues no
    // in-progress pair assumed).
    // 2026-11-03 claude: Built from cr45-X's memos, after found-assumed-true-01 and
    // diff-bound-cell-01: diff keeps a finished pair's difference (DiffHist::done) and
    // could_match a refuted pair (MatchHist::refuted) where the pair's walk met no pair
    // in progress, each with its sides (Kept). diff keeps one only where ref_id keys
    // its minuend exactly (keyed_loosely: no bound cell, no cell in a function type),
    // since a difference is made of its minuend's parts; a subtrahend keyed alike reads
    // alike because diff now reads every subtrahend part through (is_any in the
    // follow-up of diff-bound-cell-01). callgrind --check, one eval thread, base
    // ec53d4846 vs lane: empty 1.332G/1.327G, sel16 2.542G/1.345G, sel18 -/1.344G,
    // sel20 -/1.339G, selp16 3.269G/1.345G, selp18 9.028G (lane linear like selp16).
    // Pins: graphix-types
    // typ::setops::tests::diff_and_could_match_take_each_name_of_a_union_dag_once (39
    // diff pairs and 39 could_match pairs over L20; 2^20-1 with the memos off),
    // diff_and_could_match_memos_change_no_answer (the four families of cr45-X, the
    // K0/K1 shape at 16 and 24, a family of bound, open, function-type and Any-bound
    // cells; fails with the minuend guard dropped, with the subtrahend's Any read as
    // written, and with found-assumed-true-01 reverted),
    // memos_change_no_composed_answer (could_match over diff's output, the round-8
    // review's generator, 40 seeds; 1500 pass). Census of 3828 files: see the lane's
    // report.
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
                #[cfg(test)]
                DIFF_PAIRS.with(|n| n.set(n.get() + 1));
                let t0 = t0.lookup_ref(env)?;
                let t1 = t1.lookup_ref(env)?;
                // a pair met again inside its own difference subtracts
                // nothing: assuming it covered would drop what it did not
                hist.hist.insert(key, self.clone());
                let outer = mem::replace(&mut hist.cycled, false);
                let r = t0.diff_int(env, hist, &t1);
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
            // XCR claude for claude: [bug/low] This arm, and the Fn, App and Abstract arms
            // below, compare with Type `==`, which does not read a bound cell through, so
            // coverage follows a cell's representation, not its type: `Error<Map<'k: i64,
            // 'v: i64>>` is not taken out by `Error<S>` with `S = Map<i64, i64>`, and in
            // bc2.gx an arm `[Error<S>, Array<S>] as x` covering the whole scrutinee leaves
            // a later dead arm `Array<R<i64>>` accepted (main 4a886eb8f). Compare with
            // bound cells read through (as same_resolved does). It also keeps any memo of
            // diff from conflating `R<'c: i64>` with `R<i64>` (RefHist::ref_id keys them
            // alike): lane X's memo for diff-dag-01 moved verdicts that way. Found by lane
            // X's r8 review. probes ~/ptmp/cr29/scratch/review-r8/X-r3-1/bc/bc2.gx,
            // bc2z.gx, bc5.gx (diff-bound-cell-01)
            // 2026-11-03 claude: diff's Map, Fn, Abstract and App/Hole/Bound arms now
            // compare with same_type (Type == or, failing that, == of both sides'
            // resolve_tvars().normalize(), the comparison same_resolved makes per part), so
            // a bound cell is its binding wherever diff compares parts. Pins: graphix-types
            // typ::setops::tests::diff_reads_bound_cells_through (a map, a union under
            // Error, a function, an abstract type's parameters and an open constructor's
            // argument: each case leaves its minuend without the fix); graphix-tests
            // lang::select coverage_reads_a_bound_map_parameter_through (bc2, main
            // accepts), coverage_reads_a_bound_map_parameter_through_beside_a_variant
            // (bc2z), coverage_reads_a_map_literals_cells_through (bc5),
            // coverage_reads_a_bound_abstract_parameter_through (bcA); all refuse the dead
            // arm "unreachable arm: the earlier arms already cover the whole scrutinee" and
            // fail without the fix. Moves main's verdicts: bc1 and bc2 (accepted) are
            // refused; bc1z, bc2z, bc3, bc4, bc5, bc6, bc7, bcA refuse with the uncovered
            // remainder read through.
            // 2026-11-03 claude: A follow-up commit reads a cell bound to Any through where
            // diff asks whether a part covers anything: same_resolved's Any test (a
            // tuple's, struct's or variant's position) and the (Primitive, Map/Array/Error)
            // arms, through is_any. Pin: graphix-types
            // typ::setops::tests::diff_reads_a_cell_bound_to_any_as_any (each of its four
            // cases fails without it). diff's memo of finished pairs (diff-dag-01) leans on
            // diff reading every subtrahend part through, since a key reads a bound cell as
            // its binding.
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

    pub fn matching_type(&self, env: &Env, reaching: &Self) -> Result<Self> {
        let mut walk = MatchingTypes { memo: RefHist::new(), refs: LPooled::take() };
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
                if !p.has_unbound() && p.contains_with_flags(BitFlags::empty(), env, t)? {
                    return Ok(t.clone());
                }
                if !t.has_unbound() && t.contains_with_flags(BitFlags::empty(), env, p)? {
                    return Ok(p.clone());
                }
                if !p.could_match(env, t)? {
                    return Ok(Self::Bottom);
                }
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
    t0 == t1 || t0.resolve_tvars().normalize() == t1.resolve_tvars().normalize()
}

/// Positional [`same_type`]. `Any` in the subtrahend covers anything.
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
                if !is_any(b) && !same_type(a, b) {
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
    // one as its binding, and pairs a function type's by position), the
    // memos change no answer.
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
