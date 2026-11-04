use crate::{
    AddrMap, AddrSet, PrintFlag,
    env::Env,
    format_with_flags,
    stack::ensure_sufficient,
    typ::{
        AndAc, Bound, FnType, Kept, Lazy, NodePairs, RefHist, RefPair, Type,
        contains::{ContainsHist, find_open_cell},
        setops::{memo_on, strictly_identical},
    },
};
use ahash::{AHashMap, AHashSet};
use anyhow::{Result, bail};
use enumflags2::BitFlags;
use netidx_value::Typ;
use poolshark::local::LPooled;
use smallvec::SmallVec;
use std::{
    mem,
    ops::{Deref, DerefMut},
};

/// What a signature match has paired: each implementation cell's
/// signature type, and the unions holding an open variable, whose pairing
/// waits until the rest of the signature has paired what it can.
#[derive(Debug, Default, Clone)]
pub(super) struct SigPairs {
    pub(super) cells: AddrMap<Type>,
    /// The unions whose pairing waits.
    deferred: Vec<DeferredUnion>,
    /// Each function type paired, signature and implementation, whose
    /// bounds are judged once every variable has paired, and whether it
    /// stands in a contravariant position.
    pub(super) fns: Vec<(FnType, FnType, bool)>,
    /// The walk is under an odd number of function arguments.
    pub(super) contra: bool,
}

/// A union pair whose pairing waits ([`SigPairs::settle`]).
#[derive(Debug, Clone)]
struct DeferredUnion {
    sig: Type,
    imp: Type,
    /// It stands in a contravariant position.
    contra: bool,
    /// The typedef pairs whose expansion it is part of, in progress again
    /// while its members pair, so a recursive typedef's union meets its
    /// own pair as the walk that deferred it would.
    within: SmallVec<[RefPair; 2]>,
}

/// The most members an open union's pairing searches over; a larger one
/// pairs in stored order.
const MAX_PAIRED_MEMBERS: usize = 6;

impl SigPairs {
    /// End a signature match: pair the deferred unions, then judge every
    /// paired function type's bounds over the finished pairing.
    pub(super) fn finish(
        &mut self,
        env: &Env,
        hist: &mut RefHist<AHashSet<RefPair>>,
    ) -> Result<()> {
        self.settle(env, hist)?;
        let mut i = 0;
        while i < self.fns.len() {
            let (sig, imp, contra) = self.fns[i].clone();
            sig.sig_bounds(env, &imp, contra, self, hist)?;
            i += 1;
        }
        self.settle(env, hist)
    }

    /// Pair every deferred union: the first assignment of signature
    /// members to implementation members that every union, and the unions
    /// those pairings defer in turn, accept. Unions that share no open
    /// implementation cell pair apart, so one that cannot pair fails
    /// without retrying the others.
    pub(super) fn settle(
        &mut self,
        env: &Env,
        hist: &mut RefHist<AHashSet<RefPair>>,
    ) -> Result<()> {
        while !self.deferred.is_empty() {
            let rest = self.split_group();
            let r = self.search(env, hist);
            self.deferred = rest;
            r?
        }
        Ok(())
    }

    /// Keep in `deferred` the last union and every union linked to it
    /// through an open implementation cell no pairing has decided; return
    /// the others, in order.
    fn split_group(&mut self) -> Vec<DeferredUnion> {
        let cells: LPooled<Vec<LPooled<AddrSet>>> = self
            .deferred
            .iter()
            .map(|d| {
                let mut free: LPooled<AddrSet> = LPooled::take();
                find_open_cell(&d.imp, true, &mut |tv| {
                    if !self.cells.contains_key(&tv.cell_addr()) {
                        free.insert(tv.cell_addr());
                    }
                    false
                });
                free
            })
            .collect();
        let n = cells.len();
        let mut grouped: LPooled<Vec<bool>> = (0..n).map(|i| i + 1 == n).collect();
        let mut linked: LPooled<AddrSet> = LPooled::take();
        linked.extend(cells[n - 1].iter().copied());
        let mut grew = true;
        while grew {
            grew = false;
            for (i, c) in cells.iter().enumerate() {
                if !grouped[i] && !c.is_disjoint(&linked) {
                    grouped[i] = true;
                    linked.extend(c.iter().copied());
                    grew = true;
                }
            }
        }
        let mut rest = Vec::new();
        for (pair, grouped) in
            mem::take(&mut self.deferred).into_iter().zip(grouped.iter())
        {
            match grouped {
                true => self.deferred.push(pair),
                false => rest.push(pair),
            }
        }
        rest
    }

    /// Pair the last deferred union by the first assignment of its
    /// members under which the rest settle.
    fn search(&mut self, env: &Env, hist: &mut RefHist<AHashSet<RefPair>>) -> Result<()> {
        let Some(DeferredUnion { sig, imp, contra, within }) = self.deferred.pop() else {
            return Ok(());
        };
        let (Type::Set(m0), Type::Set(m1)) = (&sig, &imp) else { unreachable!() };
        let n = m0.len();
        let mut order: LPooled<Vec<usize>> = (0..n).collect();
        let mut first_err = None;
        loop {
            let mut scratch = self.clone();
            scratch.contra = contra;
            let entered: SmallVec<[RefPair; 2]> =
                within.iter().filter(|k| hist.insert(**k)).copied().collect();
            let r = m0.iter().zip(order.iter()).try_for_each(|(t0, i)| {
                t0.sig_matches_int(env, &m1[*i], &mut scratch, hist)
            });
            entered.iter().for_each(|k| {
                hist.remove(k);
            });
            scratch.contra = self.contra;
            let r = r.and_then(|()| ensure_sufficient(|| scratch.settle(env, hist)));
            match r {
                Ok(()) => {
                    *self = scratch;
                    return Ok(());
                }
                Err(e) => {
                    first_err.get_or_insert(e);
                }
            }
            if n > MAX_PAIRED_MEMBERS || !next_permutation(&mut order) {
                return Err(first_err.unwrap());
            }
        }
    }
}

/// Step `v` to its next permutation in lexical order; false after the last.
fn next_permutation(v: &mut [usize]) -> bool {
    let Some(i) = (1..v.len()).rev().find(|&i| v[i - 1] < v[i]) else { return false };
    let j = (i..v.len()).rev().find(|&j| v[j] > v[i - 1]).unwrap();
    v.swap(i - 1, j);
    v[i..].reverse();
    true
}

/// Whether a union member is a variable still open: its pairing is
/// decided by the rest of the signature.
fn open_member(t: &Type) -> bool {
    matches!(t, Type::TVar(tv) if tv.binding().is_none())
}

#[cfg(test)]
thread_local! {
    pub(super) static MATCH_PAIRS: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
    pub(super) static REST_STEPS: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
}

/// could_match's walk state: its pairs in progress (assumed to overlap),
/// each with its node in [`Rests`] once one is met in progress, the pairs
/// found apart, and a memo of its own for the containment questions it
/// asks.
pub(super) struct MatchHist {
    pub(super) hist: RefHist<AHashMap<RefPair, u32>>,
    /// The pairs found apart, wherever they were found: assuming that pairs
    /// in progress overlap only adds overlaps, so such a pair is apart with
    /// no pair in progress, as at the top of the walk, whose answer it
    /// leaves as the walk without it gives.
    refuted: Lazy<AHashMap<RefPair, Kept<()>>>,
    contains: ContainsHist,
    /// The node pairs found to overlap resting on no pair in progress,
    /// which overlap wherever they are asked.
    found: NodePairs,
    rests: Rests,
}

impl MatchHist {
    pub(super) fn new() -> Self {
        MatchHist {
            hist: RefHist::new(),
            refuted: Lazy::new(),
            contains: ContainsHist::new(),
            found: NodePairs::default(),
            rests: Rests::new(),
        }
    }
}

const NO_LINK: u32 = u32::MAX;
const NO_NODE: u32 = u32::MAX;

// CR claude for claude: [perf/low] could_match is polynomial where a failing pair
// takes back a chain of trues that each level above walks again: P_k = ([Error<P_k+1>,
// `K], C1, `A) over a chain C1..CM ending in [Error<P_1>, .., Error<P_N>], a twin Q/D
// side with `B, names sorted so the union scan meets the innermost pair in progress
// first. Each P_k fails at its tag and its failure takes the chain's trues; the level
// above walks the chain again and rescans every refuted pair. --check callgrind, main
// ec53d4846 / Rests (c9b04b08b): nest128 2.81G / 6.26G, nest256 7.00G / 32.40G
// (could_match_int calls at M=32, N=64/128/256: 318k / 2.30M / 17.6M against main's
// 919 / 1687 / 3223). Main is linear only because its found kept trues a failed pair
// gave, the unsound rule found-assumed-true-01 removed. Bounded: refuted keeps a named
// pair's false, so each pair fails once. A second shape is quadratic (n pairs each
// failing at its tag after a chain resting on it: q800 4.56G / 16.88G). A possible
// direction, sound by refuted's own argument: a NodePairs memo of node-pair falses read
// where refuted is (66k / 247k / 953k calls, still superlinear; 0 mismatches over
// review_cm3_random seeds 1-200). Only generated programs with hundreds of mutually
// recursive names reach it; guicheck and the census are unchanged, random name graphs
// cost about 5% over main (rg40 68.25G / 72.16G). probes:
// ~/ptmp/cr29/scratch/review-r9/X-r3-1/nest/mknest.py, X-r3-2/p/mkq.py
// (could-match-rests-cost-01)
/// What could_match's trues rest on. Each pair of names entered and each
/// node pair found to overlap by assuming a pair in progress is a node,
/// which rests on the nodes its walk read true. A pair whose walk fails
/// takes every node resting on it, directly or through others, and no
/// other; a node still standing is true wherever it is asked.
struct Rests {
    /// Whether each node is gone, and the first link of those resting on it.
    nodes: Lazy<Vec<(bool, u32)>>,
    /// A node resting on another, and the next link of that other's list.
    links: Lazy<Vec<(u32, u32)>>,
    /// The nodes the walk in progress read true, each call's from its mark.
    read: Lazy<Vec<u32>>,
    /// The node pairs found to overlap resting on a node, with that node.
    resting: Lazy<AHashMap<(usize, usize), Kept<u32>>>,
}

impl Rests {
    fn new() -> Self {
        Rests {
            nodes: Lazy::new(),
            links: Lazy::new(),
            read: Lazy::new(),
            resting: Lazy::new(),
        }
    }

    fn mark(&self) -> usize {
        self.read.get().map_or(0, |r| r.len())
    }

    fn read(&mut self, node: u32) {
        self.read.get_mut().push(node)
    }

    /// The walk since `mark` found nothing true.
    fn unread(&mut self, mark: usize) {
        if let Some(read) = self.read.0.as_mut() {
            read.truncate(mark)
        }
    }

    fn node(&mut self) -> u32 {
        let nodes = self.nodes.get_mut();
        nodes.push((false, NO_LINK));
        (nodes.len() - 1) as u32
    }

    /// The node a standing true of the node pair `key` rests on.
    fn standing(&self, key: (usize, usize)) -> Option<u32> {
        let node = self.resting.get()?.get(&key)?.answer;
        (!self.nodes.get()?[node as usize].0).then_some(node)
    }

    /// The node a true the walk since `mark` found rests on, if any: the
    /// one node that walk read, else a new node resting on each it read.
    /// The walk around it reads that node.
    fn rest(&mut self, mark: usize) -> Option<u32> {
        let read = self.read.0.as_mut()?;
        let &first = read.get(mark)?;
        if read[mark + 1..].iter().all(|on| *on == first) {
            read.truncate(mark + 1);
            return Some(first);
        }
        let node = self.node();
        self.hold(mark, node);
        Some(node)
    }

    /// `node` is true by the walk since `mark`: it rests on what that
    /// walk read, and the walk around it reads `node` where that is
    /// anything.
    fn hold(&mut self, mark: usize, node: u32) {
        let Some(read) = self.read.0.as_mut() else { return };
        let (nodes, links) = (self.nodes.get_mut(), self.links.get_mut());
        if read.len() > mark + 1 {
            read[mark..].sort_unstable();
        }
        let mut last = node;
        for &on in &read[mark..] {
            if on != last && on != node {
                #[cfg(test)]
                REST_STEPS.with(|n| n.set(n.get() + 1));
                links.push((node, nodes[on as usize].1));
                nodes[on as usize].1 = (links.len() - 1) as u32;
                last = on;
            }
        }
        read.truncate(mark);
        if last != node {
            read.push(node)
        }
    }

    /// `node`'s walk since `mark` failed: it goes, with every node
    /// resting on it.
    fn fail(&mut self, mark: usize, node: u32) {
        self.unread(mark);
        if node == NO_NODE {
            return;
        }
        let (Some(nodes), links) = (self.nodes.0.as_mut(), self.links.get()) else {
            return;
        };
        let mut todo: LPooled<Vec<u32>> = LPooled::take();
        todo.push(node);
        while let Some(n) = todo.pop() {
            let (gone, first) = &mut nodes[n as usize];
            if mem::replace(gone, true) {
                continue;
            }
            #[cfg(test)]
            REST_STEPS.with(|n| n.set(n.get() + 1));
            let mut l = *first;
            while let Some(&(on, next)) = links.and_then(|ls| ls.get(l as usize)) {
                todo.push(on);
                l = next;
            }
        }
    }
}

impl Deref for MatchHist {
    type Target = RefHist<AHashMap<RefPair, u32>>;

    fn deref(&self) -> &Self::Target {
        &self.hist
    }
}

impl DerefMut for MatchHist {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.hist
    }
}

impl Type {
    pub(super) fn could_match_int(
        &self,
        env: &Env,
        hist: &mut MatchHist,
        t: &Self,
    ) -> Result<bool> {
        let pair = hist.found.key(self, t);
        if let Some(pair) = pair.filter(|_| memo_on()) {
            if hist.found.contains(pair) {
                return Ok(true);
            }
            if let Some(node) = hist.rests.standing(pair) {
                hist.rests.read(node);
                return Ok(true);
            }
        }
        let mark = hist.rests.mark();
        let could = ensure_sufficient(|| self.could_match_inner(env, hist, t));
        match (&could, pair) {
            (Ok(true), None) => (),
            (Ok(true), Some(pair)) if hist.found.contains(pair) => {
                hist.rests.unread(mark)
            }
            (Ok(true), Some(pair)) => match hist.rests.rest(mark) {
                None => hist.found.insert(pair, self, t),
                Some(node) => {
                    hist.rests.resting.get_mut().insert(pair, Kept::new(node, self, t));
                }
            },
            (Ok(false) | Err(_), _) => hist.rests.unread(mark),
        }
        could
    }

    fn could_match_inner(
        &self,
        env: &Env,
        hist: &mut MatchHist,
        t: &Self,
    ) -> Result<bool> {
        // A bound constructor application is its filled type.
        if let Type::App(c, a) = self
            && let Some(filled) = Type::app_filled(c, a)
        {
            return filled.could_match_int(env, hist, t);
        }
        if let Type::App(c, a) = t
            && let Some(filled) = Type::app_filled(c, a)
        {
            return self.could_match_int(env, hist, &filled);
        }
        match (self, t) {
            (Self::Ref(tr0), Self::Ref(tr1))
                if tr0.scope == tr1.scope
                    && tr0.name == tr1.name
                    && tr0.same_definition(tr1) =>
            {
                Ok(tr0.params.len() == tr1.params.len()
                    && tr0
                        .params
                        .iter()
                        .zip(tr1.params.iter())
                        .map(|(t0, t1)| t0.could_match_int(env, hist, t1))
                        .collect::<Result<AndAc>>()?
                        .0)
            }
            (t0 @ Self::Ref(_), t1) | (t0, t1 @ Self::Ref(_)) => {
                let key = (hist.ref_id(t0, env), hist.ref_id(t1, env));
                if memo_on() && hist.refuted.get().is_some_and(|m| m.contains_key(&key)) {
                    return Ok(false);
                }
                if let Some(node) = hist.hist.get_mut(&key) {
                    if *node == NO_NODE {
                        *node = hist.rests.node()
                    }
                    let node = *node;
                    hist.rests.read(node);
                    return Ok(true);
                }
                #[cfg(test)]
                MATCH_PAIRS.with(|n| n.set(n.get() + 1));
                let t0 = t0.lookup_ref(env)?;
                let t1 = t1.lookup_ref(env)?;
                let mark = hist.rests.mark();
                hist.insert(key, NO_NODE);
                let r = t0.could_match_int(env, hist, &t1);
                let node = hist.remove(&key).unwrap_or(NO_NODE);
                match r {
                    Ok(true) if node == NO_NODE => {
                        hist.rests.rest(mark);
                    }
                    Ok(true) => hist.rests.hold(mark, node),
                    Ok(false) | Err(_) => hist.rests.fail(mark, node),
                }
                if let Ok(false) = r
                    && key.0.is_some()
                    && key.1.is_some()
                {
                    hist.refuted.get_mut().insert(key, Kept::new((), self, t));
                }
                r
            }
            (Type::App(c, a), t1) => {
                Self::app_could_match(c, a, env, |f| f.could_match_int(env, hist, t1))
            }
            (t0, Type::App(c, a)) => {
                Self::app_could_match(c, a, env, |f| t0.could_match_int(env, hist, f))
            }
            (Type::Hole, _)
            | (_, Type::Hole)
            | (Type::Bound(_), _)
            | (_, Type::Bound(_)) => Ok(self == t),
            (t0, Self::Primitive(s)) => {
                for t1 in s.iter() {
                    let t1 = Type::Primitive(t1.into());
                    if t0.contains_int(BitFlags::empty(), env, &mut hist.contains, &t1)? {
                        return Ok(true);
                    }
                }
                Ok(false)
            }
            (Type::Primitive(p), Type::Error(_)) => Ok(p.contains(Typ::Error)),
            (Type::Error(t0), Type::Error(t1)) => t0.could_match_int(env, hist, t1),
            (Type::Array(t0), Type::Array(t1)) => t0.could_match_int(env, hist, t1),
            (Type::List(t0), Type::List(t1)) => t0.could_match_int(env, hist, t1),
            (Type::Primitive(p), Type::Array(_)) => Ok(p.contains(Typ::Array)),
            (Type::Map { key: k0, value: v0 }, Type::Map { key: k1, value: v1 }) => {
                Ok(k0.could_match_int(env, hist, k1)?
                    && v0.could_match_int(env, hist, v1)?)
            }
            (Type::Primitive(p), Type::Map { .. }) => Ok(p.contains(Typ::Map)),
            (Type::Tuple(ts0), Type::Tuple(ts1)) => Ok(ts0.len() == ts1.len()
                && ts0
                    .iter()
                    .zip(ts1.iter())
                    .map(|(t0, t1)| t0.could_match_int(env, hist, t1))
                    .collect::<Result<AndAc>>()?
                    .0),
            (Type::Struct(ts0), Type::Struct(ts1)) => Ok(ts0.len() == ts1.len()
                && ts0
                    .iter()
                    .zip(ts1.iter())
                    .map(|((n0, t0, _), (n1, t1, _))| {
                        Ok(n0 == n1 && t0.could_match_int(env, hist, t1)?)
                    })
                    .collect::<Result<AndAc>>()?
                    .0),
            (Type::Variant(n0, ts0, _), Type::Variant(n1, ts1, _)) => Ok(ts0.len()
                == ts1.len()
                && n0 == n1
                && ts0
                    .iter()
                    .zip(ts1.iter())
                    .map(|(t0, t1)| t0.could_match_int(env, hist, t1))
                    .collect::<Result<AndAc>>()?
                    .0),
            (Type::ByRef(_, t0), Type::ByRef(_, t1)) => t0.could_match_int(env, hist, t1),
            (t0, Self::Set(ts)) => {
                for t1 in ts.iter() {
                    if t0.could_match_int(env, hist, t1)? {
                        return Ok(true);
                    }
                }
                Ok(false)
            }
            (Type::Set(ts), t1) => {
                for t0 in ts.iter() {
                    if t0.could_match_int(env, hist, t1)? {
                        return Ok(true);
                    }
                }
                Ok(false)
            }
            // Bindings are cloned out: the recursion may lock these cells.
            (Type::TVar(t0), t1) => match t0.binding() {
                Some(t0) => t0.could_match_int(env, hist, t1),
                None => Ok(true),
            },
            (t0, Type::TVar(t1)) => match t1.binding() {
                Some(t1) => t0.could_match_int(env, hist, &t1),
                None => Ok(true),
            },
            (
                Type::Abstract { id: id0, params: p0 },
                Type::Abstract { id: id1, params: p1 },
            ) => Ok(id0 == id1
                && p0.len() == p1.len()
                && p0
                    .iter()
                    .zip(p1.iter())
                    .map(|(t0, t1)| t0.could_match_int(env, hist, t1))
                    .collect::<Result<AndAc>>()?
                    .0),
            (Type::Fn(f0), Type::Fn(f1)) => f0.could_match_int(env, hist, f1),
            (_, Type::Bottom) => Ok(true),
            (Type::Bottom, _) => Ok(false),
            (Type::Any, _) | (_, Type::Any) => Ok(true),
            (Type::Abstract { .. }, _)
            | (_, Type::Abstract { .. })
            | (Type::Fn(_), _)
            | (_, Type::Fn(_))
            | (Type::Tuple(_), _)
            | (_, Type::Tuple(_))
            | (Type::Struct(_), _)
            | (_, Type::Struct(_))
            | (Type::Variant(_, _, _), _)
            | (_, Type::Variant(_, _, _))
            | (Type::ByRef(..), _)
            | (_, Type::ByRef(..))
            | (Type::Array(_), _)
            | (_, Type::Array(_))
            | (Type::List(_), _)
            | (_, Type::List(_))
            | (_, Type::Map { .. })
            | (Type::Map { .. }, _) => Ok(false),
        }
    }

    /// Whether an application of the open constructor `c` to `a` could
    /// match: as some head `c`'s trait bounds register, filled with `a`,
    /// could; as anything when no bound names a head.
    fn app_could_match(
        c: &Type,
        a: &Type,
        env: &Env,
        mut could: impl FnMut(&Type) -> Result<bool>,
    ) -> Result<bool> {
        let Type::TVar(cv) = c else { return Ok(true) };
        let mut bounded = false;
        for b in cv.cell_constraints().iter() {
            let Type::Ref(tr) = b else { continue };
            let Some(tid) = env.trait_of_ref(tr) else { continue };
            let Some(heads) = env.impls_of(tid) else { continue };
            for im in heads.iter() {
                let Some(filled) = im.target.reset_tvars().fill_hole(a) else { continue };
                bounded = true;
                if could(&filled)? {
                    return Ok(true);
                }
            }
        }
        Ok(!bounded)
    }

    pub fn could_match(&self, env: &Env, t: &Self) -> Result<bool> {
        self.could_match_int(env, &mut MatchHist::new(), t)
    }

    /// Whether a value of `t` may pass the run-time test (`is_a`) of the
    /// predicate `self`, where a ⊥ part, a cell nothing decided, passes
    /// any value.
    pub fn could_pass(&self, env: &Env, t: &Self) -> Result<bool> {
        fn bottom_as_any(t: &Type) -> Option<Type> {
            ensure_sufficient(|| match t {
                Type::Bottom => Some(Type::Any),
                Type::TVar(tv) => tv.binding().map(|b| bottom_as_any(&b).unwrap_or(b)),
                t => t.cow_children(&mut bottom_as_any),
            })
        }
        match bottom_as_any(self) {
            Some(p) => p.could_match(env, t),
            None => self.could_match(env, t),
        }
    }

    /// Whether the predicate `self` holds the whole of every one of
    /// `members` that `meets` (`could_match`, `could_pass`) says it may
    /// meet, so a value it admits is of `self` throughout.
    pub fn holds_what_it_meets(
        &self,
        env: &Env,
        members: &[Type],
        meets: fn(&Type, &Env, &Type) -> Result<bool>,
    ) -> bool {
        !members.is_empty()
            && members.iter().all(|m| match meets(self, env, m) {
                Ok(false) => true,
                Ok(true) => {
                    !m.has_unbound()
                        && self
                            .contains_with_flags(BitFlags::empty(), env, m)
                            .unwrap_or(false)
                }
                Err(_) => false,
            })
    }

    pub fn sig_matches(&self, env: &Env, impl_type: &Self) -> Result<()> {
        let (mut pairs, mut hist) = (SigPairs::default(), RefHist::new());
        self.sig_matches_int(env, impl_type, &mut pairs, &mut hist)?;
        pairs.finish(env, &mut hist)
    }

    pub(super) fn sig_matches_int(
        &self,
        env: &Env,
        impl_type: &Self,
        tvar_map: &mut SigPairs,
        hist: &mut RefHist<AHashSet<RefPair>>,
    ) -> Result<()> {
        ensure_sufficient(|| self.sig_matches_inner(env, impl_type, tvar_map, hist))
    }

    fn sig_matches_inner(
        &self,
        env: &Env,
        impl_type: &Self,
        tvar_map: &mut SigPairs,
        hist: &mut RefHist<AHashSet<RefPair>>,
    ) -> Result<()> {
        if (self as *const Type) == (impl_type as *const Type) {
            return Ok(());
        }
        // a filled application is its filled type (`self<'b>` with `self`
        // an array is `Array<'b>`)
        if let Self::App(c, a) = impl_type
            && let Some(filled) = Self::app_filled(c, a)
        {
            return self.sig_matches_int(env, &filled, tvar_map, hist);
        }
        if let Self::App(c, a) = self
            && let Some(filled) = Self::app_filled(c, a)
        {
            return filled.sig_matches_int(env, impl_type, tvar_map, hist);
        }
        match (self, impl_type) {
            (Self::Bottom, Self::Bottom) => Ok(()),
            (Self::Any, Self::Any) => Ok(()),
            (Self::Primitive(p0), Self::Primitive(p1)) if p0 == p1 => Ok(()),
            // one name over two definitions (a reload's) is compared by
            // what each means
            (Self::Ref(r0), Self::Ref(r1))
                if r0.scope == r1.scope
                    && r0.name == r1.name
                    && r0.same_definition(r1)
                    && r0.params.len() == r1.params.len() =>
            {
                for (t0, t1) in r0.params.iter().zip(r1.params.iter()) {
                    t0.sig_matches_int(env, t1, tvar_map, hist)?;
                }
                Ok(())
            }
            (t0 @ Self::Ref(_), t1) | (t0, t1 @ Self::Ref(_)) => {
                let key = (hist.ref_id(t0, env), hist.ref_id(t1, env));
                if hist.contains(&key) {
                    return Ok(());
                }
                let (e0, e1) = (t0.lookup_ref(env)?, t1.lookup_ref(env)?);
                hist.insert(key);
                let r = e0.sig_matches_int(env, &e1, tvar_map, hist);
                hist.remove(&key);
                r.map_err(|e| {
                    format_with_flags(PrintFlag::DerefTVars, || {
                        e.context(format!("signature has {t0}, implementation has {t1}"))
                    })
                })
            }
            (Self::Fn(f0), Self::Fn(f1)) => {
                f0.sig_matches_int(env, f1, tvar_map, hist)?;
                f0.lambda_ids.link(&f1.lambda_ids);
                Ok(())
            }
            // one member set has several normal forms (products merged as
            // written, primitives folded into one), so ground unions match
            // as sets
            (Self::Set(_) | Self::Primitive(_), _)
            | (_, Self::Set(_) | Self::Primitive(_))
                if !self.has_unbound() && !impl_type.has_unbound() =>
            {
                let f = BitFlags::empty();
                if self.contains_with_flags(f, env, impl_type)?
                    && impl_type.contains_with_flags(f, env, self)?
                {
                    Ok(())
                } else {
                    format_with_flags(PrintFlag::DerefTVars, || {
                        bail!(
                            "type mismatch: signature has {self}, implementation has {impl_type}"
                        )
                    })
                }
            }

            // a union is sorted as it was built, while its cells were open,
            // so its members pair in whichever order the rest accepts
            (Self::Set(s0), Self::Set(s1)) if s0.len() == s1.len() => {
                tvar_map.deferred.push(DeferredUnion {
                    sig: self.clone(),
                    imp: impl_type.clone(),
                    contra: tvar_map.contra,
                    within: hist.iter().copied().collect(),
                });
                Ok(())
            }
            (Self::Error(e0), Self::Error(e1)) => {
                e0.sig_matches_int(env, e1, tvar_map, hist)
            }
            (Self::Array(a0), Self::Array(a1)) => {
                a0.sig_matches_int(env, a1, tvar_map, hist)
            }
            (Self::List(a0), Self::List(a1)) => {
                a0.sig_matches_int(env, a1, tvar_map, hist)
            }
            (Self::ByRef(m0, b0), Self::ByRef(m1, b1)) if m0 == m1 => {
                b0.sig_matches_int(env, b1, tvar_map, hist)
            }
            (Self::Tuple(t0), Self::Tuple(t1)) if t0.len() == t1.len() => {
                for (t0, t1) in t0.iter().zip(t1.iter()) {
                    t0.sig_matches_int(env, t1, tvar_map, hist)?;
                }
                Ok(())
            }
            (Self::Struct(s0), Self::Struct(s1)) if s0.len() == s1.len() => {
                for ((n0, t0, _), (n1, t1, _)) in s0.iter().zip(s1.iter()) {
                    if n0 != n1 {
                        format_with_flags(PrintFlag::DerefTVars, || {
                            bail!("struct field name mismatch: {n0} vs {n1}")
                        })?
                    }
                    t0.sig_matches_int(env, t1, tvar_map, hist)?;
                }
                Ok(())
            }
            (Self::Variant(tag0, t0, _), Self::Variant(tag1, t1, _))
                if tag0 == tag1 && t0.len() == t1.len() =>
            {
                for (t0, t1) in t0.iter().zip(t1.iter()) {
                    t0.sig_matches_int(env, t1, tvar_map, hist)?;
                }
                Ok(())
            }
            (Self::Map { key: k0, value: v0 }, Self::Map { key: k1, value: v1 }) => {
                k0.sig_matches_int(env, k1, tvar_map, hist)?;
                v0.sig_matches_int(env, v1, tvar_map, hist)
            }
            (
                Self::Abstract { id: id0, params: p0 },
                Self::Abstract { id: id1, params: p1 },
            ) if id0 == id1 && p0.len() == p1.len() => {
                for (t0, t1) in p0.iter().zip(p1.iter()) {
                    t0.sig_matches_int(env, t1, tvar_map, hist)?;
                }
                Ok(())
            }
            (Self::App(c0, a0), Self::App(c1, a1)) => {
                c0.sig_matches_int(env, c1, tvar_map, hist)?;
                a0.sig_matches_int(env, a1, tvar_map, hist)
            }
            (Self::Hole, Self::Hole)
            | (Self::Bound(Bound::Concrete), Self::Bound(Bound::Concrete))
            | (Self::Bound(Bound::Data), Self::Bound(Bound::Data))
            | (Self::Bound(Bound::Function), Self::Bound(Bound::Function))
            | (Self::Bound(Bound::OneNumber), Self::Bound(Bound::OneNumber))
            | (Self::Bound(Bound::NoError), Self::Bound(Bound::NoError))
            | (Self::Bound(Bound::Discernible), Self::Bound(Bound::Discernible))
            | (Self::Bound(Bound::Ordered), Self::Bound(Bound::Ordered))
            | (Self::Bound(Bound::JsonData), Self::Bound(Bound::JsonData))
            | (Self::Bound(Bound::TomlData), Self::Bound(Bound::TomlData))
            | (Self::Bound(Bound::Singleton), Self::Bound(Bound::Singleton)) => Ok(()),
            // A bound signature var is its binding.
            (Self::TVar(sig_tv), impl_type) if let Some(b) = sig_tv.binding() => {
                b.sig_matches_int(env, impl_type, tvar_map, hist)
            }
            (sig_type, Self::TVar(impl_tv)) => {
                // A bound impl tvar is a solved fact: the signature's
                // concrete type must match its binding structurally.
                if let Some(b) = impl_tv.binding() {
                    return sig_type.sig_matches_int(env, &b, tvar_map, hist);
                }
                match tvar_map.cells.get(&impl_tv.cell_addr()) {
                    Some(prev_sig_type) => {
                        let matches = match (sig_type, prev_sig_type) {
                            (Type::TVar(tv0), Type::TVar(tv1)) => tv0.same_cell(tv1),
                            _ => strictly_identical(sig_type, prev_sig_type),
                        };
                        if matches {
                            Ok(())
                        } else {
                            format_with_flags(PrintFlag::DerefTVars, || {
                                bail!(
                                    "type variable usage mismatch: expected {prev_sig_type}, got {sig_type}"
                                )
                            })
                        }
                    }
                    None => {
                        tvar_map.cells.insert(impl_tv.cell_addr(), sig_type.clone());
                        Ok(())
                    }
                }
            }

            // a union of open implementation cells is the signature's type
            // when each cell is
            (_, Self::Set(members)) if members.iter().all(open_member) => {
                for m in members.iter() {
                    self.sig_matches_int(env, m, tvar_map, hist)?;
                }
                Ok(())
            }
            (Self::TVar(sig_tv), impl_type) => {
                format_with_flags(PrintFlag::DerefTVars, || {
                    bail!(
                        "signature has type variable '{} where implementation has {impl_type}",
                        sig_tv.name
                    )
                })
            }
            (sig_type, impl_type) => format_with_flags(PrintFlag::DerefTVars, || {
                bail!(
                    "type mismatch: signature has {sig_type}, implementation has {impl_type}"
                )
            }),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{MATCH_PAIRS, REST_STEPS};
    use crate::typ::cast::tests::{parsed, typed};

    fn could_match(defs: &[(&str, String)], t0: &str, t1: &str) -> bool {
        let defs: Vec<(&str, &str)> = defs.iter().map(|(n, b)| (*n, &**b)).collect();
        let (env, _) = typed(&defs, "null");
        parsed(&env, t0).could_match(&env, &parsed(&env, t1)).unwrap()
    }

    // Error<N0> against Error<N1> walks a pair of names in progress
    // (assumed to overlap) and fails, past the walk's first
    // `NodePairs::EAGER` nodes when K0 and K1 are long: the answer is the
    // same at every length.
    #[test]
    fn could_match_keeps_no_true_a_pair_in_progress_gave() {
        for n in [4, 8, 12, 16, 20, 24, 28, 32, 40] {
            let k = |last: &str| format!("({}{last})", "`A, ".repeat(n));
            let defs = [
                ("K0", k("`X")),
                ("K1", k("`Y")),
                ("L0", "(N0, `P)".into()),
                ("L1", "(N1, `Q)".into()),
                ("N0", "[Error<K0>, Error<L0>, `Y]".into()),
                ("N1", "[Error<K1>, Error<L1>, `W]".into()),
                ("S0", "[Error<K0>, Error<L0>, Error<N0>]".into()),
                ("S1", "[Error<K1>, Error<L1>, Error<N1>]".into()),
            ];
            assert!(!could_match(&defs, "S0", "S1"), "{n}");
        }
        for (n, m) in [(4, 3), (4, 4), (6, 4), (6, 6)] {
            let tags = |p: &str, n: usize| {
                let ts: Vec<String> = (0..n).map(|i| format!("`{p}{i}")).collect();
                format!("[{}]", ts.join(", "))
            };
            let defs = [
                ("PX", tags("Pa", n)),
                ("PY", tags("Pb", m)),
                ("PA", "[`K(PX), `L(PX)]".into()),
                ("PB", "[`K(PY), `L(PY)]".into()),
                ("EA", "Error<QA>".into()),
                ("EB", "Error<QB>".into()),
                ("QA", "[(EA, `X), `Na]".into()),
                ("QB", "[(EB, `Y), `Nb]".into()),
            ];
            let (t0, t1) = ("[`A(PA), `G(QA), `M(EA)]", "[`A(PB), `G(QB), `M(EB)]");
            assert!(!could_match(&defs, t0, t1), "{n} {m}");
        }
    }

    // A true a pair in progress gave goes when that pair fails: A against
    // B walks C against D while the pair A, B is in progress, then fails
    // at `K, `L, so C against D, asked again, fails too. P0 and P1 take
    // the walk past `NodePairs::EAGER`. In the first,
    // C against D reuses A2 against B2, which rests on A, B; in the
    // second, A3 against B3 rests on A2, B2, which rests on A, B.
    #[test]
    fn a_true_resting_on_a_failed_pair_goes_with_it() {
        let pad = format!("({}`A)", "`A, ".repeat(70));
        let shapes = [
            vec![
                ("A2", "[Error<A>, `M]"),
                ("B2", "[Error<B>, `N]"),
                ("C", "[(A2, `Z), `Q]"),
                ("D", "[(B2, `Z), `R]"),
            ],
            vec![
                ("A2", "(A3, Error<A>)"),
                ("B2", "(B3, Error<B>)"),
                ("A3", "[Error<A2>, `S]"),
                ("B3", "[Error<B2>, `T]"),
                ("C", "[(A3, `Z), `Q]"),
                ("D", "[(B3, `Z), `R]"),
            ],
        ];
        for shape in shapes {
            let mut defs = vec![
                ("P0", pad.clone()),
                ("P1", pad.clone()),
                ("A", "(P0, A2, C, `K)".into()),
                ("B", "(P1, B2, D, `L)".into()),
            ];
            defs.extend(shape.into_iter().map(|(n, b)| (n, b.to_string())));
            assert!(!could_match(&defs, "[`F(A), `G(C)]", "[`F(B), `G(D)]"));
        }
    }

    /// could_match of `t0` and `t1` over `defs`, and how many pairs of
    /// names it walked.
    fn pairs_walked(defs: &[(String, String)], t0: &str, t1: &str) -> (bool, usize) {
        let defs: Vec<(&str, &str)> = defs.iter().map(|(n, b)| (&**n, &**b)).collect();
        let (env, _) = typed(&defs, "null");
        let (t0, t1) = (parsed(&env, t0), parsed(&env, t1));
        MATCH_PAIRS.with(|n| n.set(0));
        let could = t0.could_match(&env, &t1).unwrap();
        (could, MATCH_PAIRS.with(|n| n.get()))
    }

    // Where every overlap rests on a pair in progress, the walk takes each
    // pair of names a bounded number of times: a true is reused while
    // nothing it rests on has failed, and a pair found apart stays apart.
    #[test]
    fn could_match_walks_each_pair_once_where_overlaps_are_assumed() {
        let named = |p: &str, i: usize, body: String| (format!("{p}{i}"), body);
        let linear = |n: usize, (could, pairs): (bool, usize), expect: bool| {
            assert_eq!(could, expect, "{n}");
            assert!(pairs <= 4 * n + 32, "{n}: {pairs} pairs");
        };
        for n in [8, 16, 24] {
            // a cycle of n names, each naming the next twice
            let mut defs = vec![];
            for (p, nil) in [("T", "`TNil"), ("U", "`UNil")] {
                for i in 0..n {
                    let next = format!("{p}{}", (i + 1) % n);
                    defs.push(named(p, i, format!("[({next}, {next}), {nil}]")));
                }
            }
            linear(n, pairs_walked(&defs, "T0", "U0"), true);
            // a chain of pairs back to its first name through Error or a
            // variant only
            for back in ["Error<{p}0>", "`A({p}0)"] {
                let mut defs = vec![];
                for (p, leaf) in [("T", "`Z"), ("S", "`Y")] {
                    for i in 0..n {
                        defs.push(named(p, i, format!("({p}{}, {p}{})", i + 1, i + 1)));
                    }
                    let back = back.replace("{p}", p);
                    defs.push(named(p, n, format!("[{back}, {leaf}]")));
                }
                linear(n, pairs_walked(&defs, "T0", "S0"), true);
            }
            // a DAG of pairs over two names that overlap by assumption only
            let mut defs = vec![
                named("T", 0, "[(T0, T0), `TNil]".into()),
                named("U", 0, "[(U0, U0), `UNil]".into()),
                named("D", 1, "(T0, T0)".into()),
                named("E", 1, "(U0, U0)".into()),
            ];
            for i in 2..=n {
                defs.push(named("D", i, format!("(D{}, D{})", i - 1, i - 1)));
                defs.push(named("E", i, format!("(E{}, E{})", i - 1, i - 1)));
            }
            linear(n, pairs_walked(&defs, &format!("D{n}"), &format!("E{n}")), true);
            // a DAG whose every level fails after the level below it
            // succeeded by assumption
            let mut defs = vec![
                named("P", 0, "[(P0, P0), `PNil]".into()),
                named("Q", 0, "[(Q0, Q0), `QNil]".into()),
            ];
            for i in 1..=n {
                let j = i - 1;
                defs.push(named("P", i, format!("[(P{j}, `A), (P{j}, `B)]")));
                defs.push(named("Q", i, format!("[(Q{j}, `C), (Q{j}, `D)]")));
            }
            let (t0, t1) =
                (format!("[`X(P{n}), `Y(P{n})]"), format!("[`X(Q{n}), `Y(Q{n})]"));
            linear(n, pairs_walked(&defs, &t0, &t1), false);
            // a level fails at its tag after asking the level below twice
            // and overlapping by itself in progress
            let mut defs = vec![
                named("P", 0, "[`Z0, `S(P0)]".into()),
                named("Q", 0, "[`Z1, `S(Q0)]".into()),
            ];
            for i in 1..=n {
                let j = i - 1;
                defs.push(named("P", i, format!("([`L(P{j}), `R(P{j}), `S(P{i})], `A)")));
                defs.push(named("Q", i, format!("([`L(Q{j}), `R(Q{j}), `S(Q{i})], `B)")));
            }
            linear(n, pairs_walked(&defs, &format!("P{n}"), &format!("Q{n}")), false);
        }
        // every level's trues rest on the outermost pair alone, and pairs
        // walked after them fail at their tags
        for n in [8, 16, 24] {
            let mut defs = vec![];
            for (p, f, bad, z) in [("T", "F", "`BadT", "`Z"), ("U", "G", "`BadU", "`Y")] {
                defs.push(named(p, 0, format!("[Error<{p}{n}>, {z}]")));
                for i in 1..=n {
                    defs.push(named(f, i, format!("({p}{}, {bad})", i - 1)));
                    defs.push(named(p, i, format!("[{f}{i}, ({p}{}, `Ok)]", i - 1)));
                }
            }
            let (t0, t1) =
                (format!("([`K, `M], T{n}, `X)"), format!("([`L, `M], U{n}, `Y)"));
            let (could, pairs) = pairs_walked(&defs, &t0, &t1);
            assert!(!could && pairs <= 8 * n + 32, "{n}: {pairs} pairs");
        }
    }

    // In a cycle of names every true rests on the outermost pair: each is
    // linked to what it rests on once, never again as the pairs between
    // finish.
    #[test]
    fn a_cycle_of_names_links_each_true_once() {
        for n in [50, 100, 200, 400] {
            let mut defs = vec![];
            for (p, nil) in [("T", "`TNil"), ("U", "`UNil")] {
                for i in 0..n {
                    let next = format!("{p}{}", (i + 1) % n);
                    defs.push((format!("{p}{i}"), format!("[({next}, {next}), {nil}]")));
                }
            }
            REST_STEPS.with(|s| s.set(0));
            let (could, pairs) = pairs_walked(&defs, "T0", "U0");
            let steps = REST_STEPS.with(|s| s.get());
            assert!(
                could && pairs <= 4 * n + 32 && steps <= 10 * n,
                "{n}: {pairs} {steps}"
            );
        }
    }
}
