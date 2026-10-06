use crate::{
    expr::WrittenAt,
    stack::ensure_sufficient,
    typ::{Mutability, TVar, Type, TypeRef, setops::union_identical},
};
use ahash::AHashMap;
use arcstr::ArcStr;
use enumflags2::BitFlags;
use netidx_value::Typ;
use poolshark::local::LPooled;
use std::{iter, mem::Discriminant};
use triomphe::Arc;

/// Per-pass [`Type::normalize`] state: `cells` are the visited TVar
/// cells (the first visit normalizes the binding in place); `memo` is a
/// pointer-identity cache of composite results, keyed by (variant,
/// content Arc address) for variants whose Arcs are their whole content.
/// Each entry pins the keyed type: an in-place rebinding frees the old
/// binding, and its address must not be reused under its key.
pub(super) struct NormCx {
    pub(super) cells: LPooled<nohash::IntSet<usize>>,
    memo: LPooled<AHashMap<NormKey, (Type, Option<Type>)>>,
}

#[doc(hidden)]
pub type NormKey = (Discriminant<Type>, usize, usize);

impl NormCx {
    pub(super) fn take() -> Self {
        Self { cells: LPooled::take(), memo: LPooled::take() }
    }
}

/// Per-pass [`Type::resolve_tvars`] state: `done` holds completed
/// snapshots of bound cells, `fresh` the one fresh mint per unbound
/// source cell (preserving alias topology), `in_progress` the cycle
/// guard, `memo` a pointer-identity cache of composite results.
pub(super) struct ResolveTvarsCx {
    done: LPooled<nohash::IntMap<usize, Type>>,
    fresh: LPooled<nohash::IntMap<usize, TVar>>,
    in_progress: LPooled<nohash::IntSet<usize>>,
    memo: LPooled<AHashMap<NormKey, Option<Type>>>,
    /// An open cell's copy keeps its predicate conjuncts (`Concrete`,
    /// `Function`), which name no cell.
    predicates: bool,
}

impl ResolveTvarsCx {
    pub(super) fn take() -> Self {
        Self {
            done: LPooled::take(),
            fresh: LPooled::take(),
            in_progress: LPooled::take(),
            memo: LPooled::take(),
            predicates: false,
        }
    }
}

#[doc(hidden)]
pub fn norm_key(t: &Type) -> Option<NormKey> {
    let d = std::mem::discriminant(t);
    match t {
        Type::Set(a) | Type::Tuple(a) => Some((d, (**a).as_ptr() as usize, 0)),
        Type::Struct(a) => Some((d, (**a).as_ptr() as usize, 0)),
        Type::Fn(a) => Some((d, &**a as *const _ as usize, 0)),
        Type::Array(a) | Type::List(a) | Type::Error(a) | Type::ByRef(_, a) => {
            Some((d, &**a as *const Type as usize, 0))
        }
        Type::Map { key, value } => {
            Some((d, &**key as *const Type as usize, &**value as *const Type as usize))
        }
        Type::Bottom
        | Type::Any
        | Type::Primitive(_)
        | Type::Abstract { .. }
        | Type::App(..)
        | Type::Hole
        | Type::Concrete
        | Type::Function
        | Type::Singleton
        | Type::OneNumber
        | Type::Discernible
        | Type::Ref(_)
        | Type::TVar(_)
        | Type::Variant(_, _, _) => None,
    }
}

impl Type {
    pub(crate) fn flatten_set(set: impl IntoIterator<Item = Self>) -> Self {
        Self::flatten_set_tracked(set).0
    }

    /// [`Self::flatten_set`] reporting whether it changed anything
    /// relative to a `Type::Set` of the input members in input order.
    /// Conservative: `true` may be reported for an identical result.
    fn flatten_set_tracked(set: impl IntoIterator<Item = Self>) -> (Self, bool) {
        let mut nested: LPooled<Vec<(Arc<[Self]>, usize)>> = LPooled::take();
        let mut acc: LPooled<Vec<Self>> = LPooled::take();
        let mut saw_bottom = false;
        let mut changed = false;
        let mut absorb =
            |t: Self, nested: &mut Vec<(Arc<[Self]>, usize)>, acc: &mut Vec<Self>| {
                match t {
                    Type::Set(ref s) => {
                        changed = true;
                        nested.push((s.clone(), 0));
                    }
                    Type::Any => return false,
                    // ⊥ ∪ X = X; an all-⊥ set is ⊥ (the exit match).
                    Type::Bottom => {
                        changed = true;
                        saw_bottom = true
                    }
                    t => {
                        // `acc` is merge-saturated, so only the incoming
                        // element (or its merge result) can enable a new
                        // merge.
                        let mut incoming = t;
                        // CR claude for eric: [perf] Each incoming member is tried
                        // against every kept one, so normalizing an N-member union
                        // costs N² merge attempts. A flat seq's pc is an (N+1)-tag
                        // union that its idle select normalizes in check_dead_arms, so
                        // --check of `seq { 1; ..; 1 }` is quadratic (16k steps 2.2 s,
                        // 32k 7.9 s, 64k 32 s with the debug binary), and
                        // deep_nesting's seqarm@100000 child takes about 80 s. A
                        // hand-written select over N tags is worse (4000 tags 3.3 s,
                        // 16000 52.8 s), because check_coverage and check_dead_arms
                        // also rebuild the union per arm. Most members can merge only
                        // with one of their own shape (a variant only with the same tag
                        // and arity), so keying the kept members by shape would avoid
                        // the N² scan. Separately, seqarm needs no more than about 10k
                        // statements to exercise the lowering's stack.
                        // (tests-shell-compiler-18)
                        'merge: loop {
                            for j in 0..acc.len() {
                                if let Some(m) = incoming.merge(&acc[j]) {
                                    changed = true;
                                    acc.remove(j);
                                    incoming = m;
                                    continue 'merge;
                                }
                            }
                            acc.push(incoming);
                            break;
                        }
                    }
                }
                true
            };
        // CR claude for eric: [bug] The merge is greedy in arrival order and the sort
        // comes after it, so one member set has several normal forms. [(i64, bool),
        // (string, bool), (i64, f64)] gives [(i64, f64), ([i64, string], bool)], the
        // same members in another order give [(i64, [f64, bool]), (string, bool)], and
        // contains holds both ways. The interface checks compare normal forms by
        // position (the Set arm of sig_matches, matches.rs:287, and the typedef `!=` at
        // module.rs:477), so an implementation that writes or infers the union in
        // another order is refused. For example, `val x: [(i64, bool), (string, bool),
        // (i64, f64)]` over `let x = select c { 0 => (1, 2.0), 1 => (1, true), _ =>
        // ("a", true) }` is a signature mismatch. Sorting before the merge would not
        // make the form canonical, because a union written already merged, like [([i64,
        // string], bool), (i64, f64)], cannot merge further; those checks want mutual
        // containment, as find_impl uses. probe:
        // design/review-2026-10-05/repro/t-fntyp-09.sh (t-fntyp-09)
        for t in set {
            if !absorb(t, &mut nested, &mut acc) {
                return (Type::Any, true);
            }
            while let Some((members, i)) = nested.last_mut() {
                match members.get(*i) {
                    None => {
                        nested.pop();
                    }
                    Some(t) => {
                        *i += 1;
                        let t = t.clone();
                        if !absorb(t, &mut nested, &mut acc) {
                            return (Type::Any, true);
                        }
                    }
                }
            }
        }
        if !acc.is_sorted() {
            changed = true;
            acc.sort();
        }
        match &**acc {
            [] if saw_bottom => (Type::Bottom, true),
            [] => (Type::Primitive(BitFlags::empty()), true),
            [t] => (t.clone(), true),
            _ => (Type::Set(Arc::from_iter(acc.drain(..))), changed),
        }
    }

    /// Map `f` over a shared slice, rebuilding only if some element
    /// changed (`None` from `f` means unchanged). `None` when nothing
    /// changed.
    pub(crate) fn cow_slice<T: Clone>(
        orig: &[T],
        mut f: impl FnMut(&T) -> Option<T>,
    ) -> Option<Arc<[T]>> {
        let mut rebuilt: Option<LPooled<Vec<T>>> = None;
        for (i, x) in orig.iter().enumerate() {
            match f(x) {
                None => {
                    if let Some(v) = rebuilt.as_mut() {
                        v.push(x.clone())
                    }
                }
                Some(nx) => {
                    let v = rebuilt
                        .get_or_insert_with(|| orig[..i].iter().cloned().collect());
                    v.push(nx);
                }
            }
        }
        rebuilt.map(|mut v| Arc::from_iter(v.drain(..)))
    }

    /// Snapshot the type with every bound TVar replaced by its binding,
    /// recursively; unbound TVars become fresh cells, one per source
    /// cell, preserving alias topology. The result shares no cell with
    /// the original; TVar-free subtrees are returned shared.
    pub fn resolve_tvars(&self) -> Self {
        self.resolve_tvars_seen_int(&mut ResolveTvarsCx::take())
            .unwrap_or_else(|| self.clone())
    }

    /// [`Self::resolve_tvars`] for tooling: an open cell keeps the
    /// predicates it must satisfy.
    pub fn snapshot(&self) -> Self {
        let mut cx = ResolveTvarsCx { predicates: true, ..ResolveTvarsCx::take() };
        self.resolve_tvars_seen_int(&mut cx).unwrap_or_else(|| self.clone())
    }

    /// `None` = no TVar anywhere beneath — the caller keeps the original.
    pub(super) fn resolve_tvars_seen_int(&self, cx: &mut ResolveTvarsCx) -> Option<Self> {
        ensure_sufficient(|| self.resolve_tvars_seen(cx))
    }

    fn resolve_tvars_seen(&self, cx: &mut ResolveTvarsCx) -> Option<Self> {
        let key = norm_key(self);
        if let Some(k) = key
            && let Some(r) = cx.memo.get(&k)
        {
            return r.clone();
        }
        let r = match self {
            Type::Bottom
            | Type::Any
            | Type::Primitive(_)
            | Type::Hole
            | Type::Concrete
            | Type::Singleton
            | Type::OneNumber
            | Type::Discernible
            | Type::Function => None,
            Type::App(c, a) => {
                match (c.resolve_tvars_seen_int(cx), a.resolve_tvars_seen_int(cx)) {
                    (None, None) => None,
                    (c2, a2) => Some(Type::app(
                        c2.unwrap_or_else(|| (**c).clone()),
                        a2.unwrap_or_else(|| (**a).clone()),
                    )),
                }
            }
            Type::Abstract { id, params } => {
                Self::cow_slice(params, |t| t.resolve_tvars_seen_int(cx))
                    .map(|params| Type::Abstract { id: *id, params })
            }
            Type::Ref(tr) => {
                Self::cow_slice(&tr.params, |t| t.resolve_tvars_seen_int(cx))
                    .map(|params| Type::Ref(tr.with_params(params)))
            }
            Type::TVar(tv) => Some({
                let addr = tv.cell_addr();
                if let Some(t) = cx.done.get(&addr) {
                    return Some(t.clone());
                }
                if let Some(fresh) = cx.fresh.get(&addr) {
                    return Some(Type::TVar(fresh.clone()));
                }
                if !cx.in_progress.insert(addr) {
                    return Some(Type::TVar(tv.fresh_copy()));
                }
                let r = match tv.binding() {
                    Some(t) => {
                        let r = match t.resolve_tvars_seen_int(cx) {
                            Some(t) => t,
                            None => t,
                        };
                        cx.done.insert(addr, r.clone());
                        r
                    }
                    None => {
                        let fresh = tv.fresh_copy();
                        if cx.predicates {
                            for c in tv.cell_constraints() {
                                if matches!(
                                    c,
                                    Type::Concrete
                                        | Type::Function
                                        | Type::Singleton
                                        | Type::OneNumber
                                        | Type::Discernible
                                ) {
                                    fresh.add_cell_constraint(c);
                                }
                            }
                        }
                        cx.fresh.insert(addr, fresh.clone());
                        Type::TVar(fresh)
                    }
                };
                cx.in_progress.remove(&addr);
                r
            }),
            Type::Set(s) => {
                Self::cow_slice(s, |t| t.resolve_tvars_seen_int(cx)).map(Type::Set)
            }
            Type::Error(t) => {
                t.resolve_tvars_seen_int(cx).map(|t| Type::Error(Arc::new(t)))
            }
            Type::Array(t) => {
                t.resolve_tvars_seen_int(cx).map(|t| Type::Array(Arc::new(t)))
            }
            Type::List(t) => {
                t.resolve_tvars_seen_int(cx).map(|t| Type::List(Arc::new(t)))
            }
            Type::Map { key, value } => {
                match (key.resolve_tvars_seen_int(cx), value.resolve_tvars_seen_int(cx)) {
                    (None, None) => None,
                    (k, v) => Some(Type::Map {
                        key: k.map(Arc::new).unwrap_or_else(|| key.clone()),
                        value: v.map(Arc::new).unwrap_or_else(|| value.clone()),
                    }),
                }
            }
            Type::ByRef(m, t) => {
                t.resolve_tvars_seen_int(cx).map(|t| Type::ByRef(*m, Arc::new(t)))
            }
            Type::Tuple(t) => {
                Self::cow_slice(t, |t| t.resolve_tvars_seen_int(cx)).map(Type::Tuple)
            }
            Type::Struct(t) => Self::cow_slice(t, |(n, t, at)| {
                t.resolve_tvars_seen_int(cx).map(|t| (n.clone(), t, *at))
            })
            .map(Type::Struct),
            Type::Variant(tag, t, at) => {
                Self::cow_slice(t, |t| t.resolve_tvars_seen_int(cx))
                    .map(|t| Type::Variant(tag.clone(), t, *at))
            }
            Type::Fn(ft) => {
                ft.resolve_tvars_seen_int(cx).map(|ft| Type::Fn(Arc::new(ft)))
            }
        };
        if let Some(k) = key {
            cx.memo.insert(k, r.clone());
        }
        r
    }

    /// Normalize structure and the bindings of every reachable cell;
    /// each cell is normalized once per pass.
    pub fn normalize(&self) -> Self {
        self.normalize_int(&mut NormCx::take()).unwrap_or_else(|| self.clone())
    }

    /// `None` when already normal. A `TVar` normalizes its binding in
    /// place inside the shared cell and is therefore always `None`.
    pub(super) fn normalize_int(&self, cx: &mut NormCx) -> Option<Self> {
        ensure_sufficient(|| self.normalize_int_inner(cx))
    }

    fn normalize_int_inner(&self, cx: &mut NormCx) -> Option<Self> {
        let key = norm_key(self);
        if let Some(k) = key
            && let Some((_, r)) = cx.memo.get(&k)
        {
            return r.clone();
        }
        let r = match self {
            Type::Bottom
            | Type::Any
            | Type::Abstract { .. }
            | Type::Primitive(_)
            | Type::Hole
            | Type::Concrete
            | Type::Singleton
            | Type::OneNumber
            | Type::Discernible
            | Type::Function => None,
            Type::App(c, a) => match (c.normalize_int(cx), a.normalize_int(cx)) {
                (None, None) => None,
                (c2, a2) => Some(Type::app(
                    c2.unwrap_or_else(|| (**c).clone()),
                    a2.unwrap_or_else(|| (**a).clone()),
                )),
            },
            Type::Ref(tr) => Self::cow_slice(&tr.params, |t| t.normalize_int(cx))
                .map(|params| Type::Ref(tr.with_params(params))),
            Type::TVar(tv) => {
                tv.normalize_int(cx);
                None
            }
            Type::Set(s) => {
                let mut members_changed = false;
                let mut members: LPooled<Vec<Self>> = LPooled::take();
                for t in s.iter() {
                    match t.normalize_int(cx) {
                        Some(n) => {
                            members_changed = true;
                            members.push(n)
                        }
                        None => members.push(t.clone()),
                    }
                }
                let (flat, flat_changed) = Self::flatten_set_tracked(members.drain(..));
                (members_changed || flat_changed).then_some(flat)
            }
            Type::Error(t) => t.normalize_int(cx).map(|t| Type::Error(Arc::new(t))),
            Type::Array(t) => t.normalize_int(cx).map(|t| Type::Array(Arc::new(t))),
            Type::List(t) => t.normalize_int(cx).map(|t| Type::List(Arc::new(t))),
            Type::Map { key, value } => {
                match (key.normalize_int(cx), value.normalize_int(cx)) {
                    (None, None) => None,
                    (k, v) => Some(Type::Map {
                        key: k.map(Arc::new).unwrap_or_else(|| key.clone()),
                        value: v.map(Arc::new).unwrap_or_else(|| value.clone()),
                    }),
                }
            }
            Type::ByRef(m, t) => {
                t.normalize_int(cx).map(|t| Type::ByRef(*m, Arc::new(t)))
            }
            Type::Tuple(t) => {
                Self::cow_slice(t, |t| t.normalize_int(cx)).map(Type::Tuple)
            }
            Type::Struct(t) => Self::cow_slice(t, |(n, t, at)| {
                t.normalize_int(cx).map(|t| (n.clone(), t, *at))
            })
            .map(Type::Struct),
            Type::Variant(tag, t, at) => Self::cow_slice(t, |t| t.normalize_int(cx))
                .map(|t| Type::Variant(tag.clone(), t, *at)),
            Type::Fn(ft) => ft.normalize_int(cx).map(|ft| Type::Fn(Arc::new(ft))),
        };
        if let Some(k) = key {
            cx.memo.insert(k, (self.clone(), r.clone()));
        }
        r
    }

    fn merge(&self, t: &Self) -> Option<Self> {
        ensure_sufficient(|| self.merge_inner(t))
    }

    fn merge_inner(&self, t: &Self) -> Option<Self> {
        // Equality modulo set-flattening at a nested position.
        // CR claude for eric: [bug] flat_eq compares with `==`, and TVar equality
        // treats two distinct unbound cells as equal. So merge_one_differing and the
        // Array/List/Map/Error arms fold `Array<'a> ∪ Array<'b>` into `Array<'b>` and
        // `(i64, 'a) ∪ (string, 'b)` into `([i64, string], 'b)`, dropping a cell that
        // union_inner had kept apart. The Ref, Abstract and Fn arms below, and
        // union_identical's own Fn arm in setops.rs, have the same hole. As a result
        // `|a, b| [(1, a), (2, b)]` is typed `-> Array<(i64, 'b)>`, and `pick(true,
        // "x", 41)[0]$ + 1` over `|c: bool, a, b| select c { true => [a], false => [b]
        // }` passes --check and then panics the JIT at kernel.rs:243 (the struct twin
        // is a silent interp/JIT divergence). A written `[(i64, 'a), (string, 'b)]`
        // also parses as `([i64, string], 'b)`, so valid calls are refused and invalid
        // ones accepted. Compare with union_identical here and in those arms. probe:
        // design/review-2026-10-05/repro/t-fntyp-02.gx (t-fntyp-02)
        fn flat_eq(t0: &Type, t1: &Type) -> bool {
            match (t0, t1) {
                (Type::Set(_), _) | (_, Type::Set(_)) => {
                    let f = |t: &Type| match t {
                        Type::Set(s) => Type::flatten_set(s.iter().cloned()),
                        t => t.clone(),
                    };
                    f(t0) == f(t1)
                }
                (t0, t1) => t0 == t1,
            }
        }
        // Products merge component-wise only when at most one component
        // differs: `(A, X) ∪ (B, Y)` is not `([A, B], [X, Y])`.
        fn merge_one_differing<'a, T: 'a>(
            t0: impl IntoIterator<Item = &'a T>,
            t1: impl IntoIterator<Item = &'a T>,
            typ: impl Fn(&T) -> &Type,
        ) -> Option<Option<(usize, Type)>> {
            let mut differing = None;
            for (i, (a, b)) in t0.into_iter().zip(t1).enumerate() {
                let (a, b) = (typ(a), typ(b));
                if flat_eq(a, b) {
                    continue;
                }
                if differing.is_some() {
                    return None;
                }
                differing = Some((i, a.merge(b)?));
            }
            Some(differing)
        }
        // A bound constructor application is its filled type.
        if let Type::App(c, a) = self
            && let Some(filled) = Type::app_filled(c, a)
        {
            return filled.merge(t);
        }
        if let Type::App(c, a) = t
            && let Some(filled) = Type::app_filled(c, a)
        {
            return self.merge(&filled);
        }
        match (self, t) {
            (Type::Ref(t0), Type::Ref(t1)) => {
                if t0 == t1 {
                    Some(Type::Ref(t0.clone()))
                } else {
                    None
                }
            }
            (Type::Ref(TypeRef { .. }), _) | (_, Type::Ref(TypeRef { .. })) => None,
            (Type::App(..), _)
            | (_, Type::App(..))
            | (Type::Hole, _)
            | (_, Type::Hole)
            | (Type::Concrete, _)
            | (Type::Function, _)
            | (Type::Singleton, _)
            | (Type::OneNumber, _)
            | (Type::Discernible, _)
            | (_, Type::Function)
            | (_, Type::Singleton)
            | (_, Type::OneNumber)
            | (_, Type::Discernible)
            | (_, Type::Concrete) => {
                if self == t {
                    Some(self.clone())
                } else {
                    None
                }
            }
            (Type::Bottom, t) | (t, Type::Bottom) => Some(t.clone()),
            (Type::Any, _) | (_, Type::Any) => Some(Type::Any),
            (Type::Primitive(s0), Type::Primitive(s1)) => {
                Some(Type::Primitive(*s0 | *s1))
            }
            (Type::Primitive(p), t) | (t, Type::Primitive(p)) if p.is_empty() => {
                Some(t.clone())
            }
            (
                Type::Abstract { id: id0, params: p0 },
                Type::Abstract { id: id1, params: p1 },
            ) => {
                if id0 == id1 && p0 == p1 {
                    Some(self.clone())
                } else {
                    None
                }
            }
            // CR claude for eric: [bug] Two fn types merge only when `==`, and `==`
            // never equates a bound cell with its binding, though the TVar arms below
            // look through it. So `|x: i64| x * 2` and `|x: i64| -> i64 x + 1`, both
            // fn(x: i64) -> i64, stay two union members, and a call through `select b {
            // true => g, false => h }` is refused with "expected fn not [..]"
            // (deref_typ!, graphix-compiler/src/node/mod.rs:143); with both annotated,
            // or both unannotated, it is accepted. A generic member beside a
            // monomorphic one (`|x| x` beside `|x: i64| x + 1`) is refused the same
            // way, since nothing instantiates it to meet the other. So whether a select
            // of functions can be called depends on which annotations were written;
            // `let f: fn(x: i64) -> i64 = select ..` is accepted. probe:
            // design/review-2026-10-05/repro/x-typecheck-generics-F12.gx
            // (x-typecheck-generics-F12)
            (Type::Fn(f0), Type::Fn(f1)) => {
                if f0 == f1 {
                    Some(Type::Fn(f0.clone()))
                } else {
                    None
                }
            }
            (Type::Array(t0), Type::Array(t1)) => {
                if flat_eq(t0, t1) {
                    Some(Type::Array(t0.clone()))
                } else {
                    None
                }
            }
            (Type::List(t0), Type::List(t1)) => {
                if flat_eq(t0, t1) {
                    Some(Type::List(t0.clone()))
                } else {
                    None
                }
            }
            (Type::Primitive(p), Type::Array(_))
            | (Type::Array(_), Type::Primitive(p)) => {
                if p.contains(Typ::Array) {
                    Some(Type::Primitive(*p))
                } else {
                    None
                }
            }
            (Type::Map { key: k0, value: v0 }, Type::Map { key: k1, value: v1 }) => {
                if flat_eq(k0, k1) && flat_eq(v0, v1) {
                    Some(Type::Map { key: k0.clone(), value: v0.clone() })
                } else {
                    None
                }
            }
            (Type::Error(t0), Type::Error(t1)) => {
                if flat_eq(t0, t1) {
                    Some(Type::Error(t0.clone()))
                } else {
                    None
                }
            }
            // XCR claude for eric: [bug] Merging two references' targets types `select c
            // { true => &x, false => &y }` (x: i64, y: string) as `&[i64, string]`, a
            // reference to a variable that exists nowhere. So `*r <- "s"` passes the
            // check and stores a string in the i64 `x`. The fused read of `x` then
            // panics at fusion/kernel.rs:243 and the runtime dies. A reference is
            // written through, so its target must not widen, here or in
            // contains.rs:717. Not merging is not enough on its own:
            // ConnectDeref::typecheck0_with (node/mod.rs:2361) accepts a write when any
            // member of r's type contains `&typeof(v)`, so the unmerged `[&i64,
            // &Array<i64>]` lets `*r <- [2, 3]` write into `x` too; a write has to fit
            // every referent. probe: design/review-2026-10-05/repro/t-fntyp-01.gx
            // (t-fntyp-01)
            // 2026-10-06 claude: only two `&T` merge their referents; two `&mut` merge
            // only when equal, and a write must be `&mut` and fit every member
            // (ConnectDeref::typecheck0_with). The probe is refused.
            // Reading either of two references reads the union of their
            // referents; a writable one only merges with itself.
            (
                Type::ByRef(Mutability::Shared, t0),
                Type::ByRef(Mutability::Shared, t1),
            ) => t0.merge(t1).map(|t| Type::ByRef(Mutability::Shared, Arc::new(t))),
            (Type::ByRef(m0, t0), Type::ByRef(m1, t1)) => {
                if m0 == m1 && flat_eq(t0, t1) {
                    Some(Type::ByRef(*m0, t0.clone()))
                } else {
                    None
                }
            }
            (Type::Set(s0), Type::Set(s1)) => {
                Some(Self::flatten_set(s0.iter().cloned().chain(s1.iter().cloned())))
            }
            (Type::Set(s), t) | (t, Type::Set(s)) => {
                Some(Self::flatten_set(s.iter().cloned().chain(iter::once(t.clone()))))
            }
            (Type::Tuple(t0), Type::Tuple(t1)) if t0.len() == t1.len() => {
                match merge_one_differing(t0.iter(), t1.iter(), |t| t)? {
                    None => Some(self.clone()),
                    Some((i, m)) => {
                        let mut m = Some(m);
                        Some(Type::Tuple(Arc::from_iter(t0.iter().enumerate().map(
                            |(j, t)| if j == i { m.take().unwrap() } else { t.clone() },
                        ))))
                    }
                }
            }
            (Type::Variant(tag0, t0, at), Type::Variant(tag1, t1, _))
                if tag0 == tag1 && t0.len() == t1.len() =>
            {
                match merge_one_differing(t0.iter(), t1.iter(), |t| t)? {
                    None => Some(self.clone()),
                    Some((i, m)) => {
                        let mut m = Some(m);
                        let ts = Arc::from_iter(t0.iter().enumerate().map(|(j, t)| {
                            if j == i { m.take().unwrap() } else { t.clone() }
                        }));
                        Some(Type::Variant(tag0.clone(), ts, *at))
                    }
                }
            }
            (Type::Struct(t0), Type::Struct(t1))
                if t0.len() == t1.len()
                    && t0
                        .iter()
                        .zip(t1.iter())
                        .all(|((n0, _, _), (n1, _, _))| n0 == n1) =>
            {
                match merge_one_differing(t0.iter(), t1.iter(), |(_, t, _)| t)? {
                    None => Some(self.clone()),
                    Some((i, m)) => {
                        let mut m = Some(m);
                        let fs: Arc<[(ArcStr, Type, WrittenAt)]> = Arc::from_iter(
                            t0.iter().enumerate().map(|(j, (n, t, at))| {
                                let t =
                                    if j == i { m.take().unwrap() } else { t.clone() };
                                (n.clone(), t, *at)
                            }),
                        );
                        Some(Type::Struct(fs))
                    }
                }
            }
            // Strict tvar identity: two distinct unbound cells never merge.
            (t0v @ Type::TVar(_), t1v @ Type::TVar(_)) if union_identical(t0v, t1v) => {
                Some(t0v.clone())
            }
            (Type::TVar(tv), t) => tv.binding().and_then(|b| b.merge(t)),
            (t, Type::TVar(tv)) => tv.binding().and_then(|b| t.merge(&b)),
            (Type::ByRef(..), _)
            | (_, Type::ByRef(..))
            | (Type::Abstract { .. }, _)
            | (_, Type::Abstract { .. })
            | (Type::Array(_), _)
            | (_, Type::Array(_))
            | (Type::List(_), _)
            | (_, Type::List(_))
            | (_, Type::Map { .. })
            | (Type::Map { .. }, _)
            | (Type::Tuple(_), _)
            | (_, Type::Tuple(_))
            | (Type::Struct(_), _)
            | (_, Type::Struct(_))
            | (Type::Variant(_, _, _), _)
            | (_, Type::Variant(_, _, _))
            | (_, Type::Fn(_))
            | (Type::Fn(_), _)
            | (Type::Error(_), _)
            | (_, Type::Error(_)) => None,
        }
    }
}
