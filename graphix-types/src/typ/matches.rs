use crate::{
    env::Env,
    stack::ensure_sufficient,
    typ::{
        AndAc, Kept, Lazy, NodeKey, NodePairs, RefHist, RefPair, Type,
        setops::{Pairs, Unfoldings, def_pair, memo_on},
    },
};
use ahash::AHashMap;
use anyhow::Result;
use netidx_value::Typ;
use poolshark::local::LPooled;
use std::{
    mem,
    ops::{Deref, DerefMut},
};

#[cfg(test)]
thread_local! {
    pub(super) static MATCH_PAIRS: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
    pub(super) static REST_STEPS: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
    pub(super) static MATCH_CALLS: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
    static BOTTOM_AS_ANY_STEPS: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
}

/// could_match's walk state: its pairs in progress (assumed to overlap),
/// each with its node in [`Rests`] once one is met in progress, and the
/// pairs found apart.
pub(super) struct MatchHist {
    pub(super) hist: RefHist<AHashMap<RefPair, Entered>>,
    /// The pairs found apart, wherever they were found: assuming that pairs
    /// in progress overlap only adds overlaps, so such a pair is apart with
    /// no pair in progress, as at the top of the walk, whose answer it
    /// leaves as the walk without it gives.
    refuted: Lazy<AHashMap<RefPair, Kept<()>>>,
    /// The node pairs found to overlap resting on no pair in progress,
    /// which overlap wherever they are asked.
    found: NodePairs,
    /// The node pairs found apart, wherever they were found, as `refuted`'s
    /// pairs of names are.
    apart: NodePairs,
    rests: Rests,
    unfoldings: Unfoldings,
    pairs: Pairs,
    /// Whether the walk asks if one value is a value of both types
    /// ([`Type::shares_value`]): two arrays, lists or maps share the empty
    /// one whatever they hold, and two instantiations of an abstract type
    /// made in Graphix share a value only at the same parameters, which
    /// a value carries.
    shared: bool,
    /// Under `shared`, the shallowest pair in progress that the walk since
    /// the last call began read as apart ([`NO_DEPTH`] when none): a pair
    /// found apart on it holds only while that pair does, so it is kept
    /// only when none is read.
    lowest: u32,
}

/// A pair of names in progress: the node resting on it once met again,
/// and how many pairs were in progress when it began.
pub(super) struct Entered {
    node: u32,
    depth: u32,
}

impl MatchHist {
    pub(super) fn new() -> Self {
        MatchHist {
            hist: RefHist::new(),
            refuted: Lazy::new(),
            found: NodePairs::default(),
            apart: NodePairs::default(),
            rests: Rests::new(),
            unfoldings: Unfoldings::default(),
            pairs: Pairs::default(),
            shared: false,
            lowest: NO_DEPTH,
        }
    }
}

const NO_LINK: u32 = u32::MAX;
const NO_NODE: u32 = u32::MAX;
const NO_DEPTH: u32 = u32::MAX;

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
    resting: Lazy<AHashMap<(NodeKey, NodeKey), Kept<u32>>>,
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
    fn standing(&self, key: (NodeKey, NodeKey)) -> Option<u32> {
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
    type Target = RefHist<AHashMap<RefPair, Entered>>;

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
        #[cfg(test)]
        MATCH_CALLS.with(|n| n.set(n.get() + 1));
        let pair = hist.found.key(self, t);
        if let Some(pair) = pair.filter(|_| memo_on()) {
            if hist.found.contains(pair) {
                return Ok(true);
            }
            if hist.apart.contains(pair) {
                return Ok(false);
            }
            if let Some(node) = hist.rests.standing(pair) {
                hist.rests.read(node);
                return Ok(true);
            }
        }
        let mark = hist.rests.mark();
        let outer = mem::replace(&mut hist.lowest, NO_DEPTH);
        let could = ensure_sufficient(|| self.could_match_inner(env, hist, t));
        let lowest = hist.lowest;
        hist.lowest = outer.min(lowest);
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
            (Ok(false), Some(pair)) => {
                hist.rests.unread(mark);
                if lowest == NO_DEPTH {
                    hist.apart.insert(pair, self, t)
                }
            }
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
        if let (Self::Ref(tr0), Self::Ref(tr1)) = (self, t)
            && tr0.scope == tr1.scope
            && tr0.name == tr1.name
            && tr0.same_definition(tr1, env)
        {
            let alike = tr0.params.len() == tr1.params.len()
                && tr0.hold_bounds_with(tr1, env, true)?
                && tr0
                    .params
                    .iter()
                    .zip(tr1.params.iter())
                    .map(|(t0, t1)| t0.could_match_int(env, hist, t1))
                    .collect::<Result<AndAc>>()?
                    .0;
            // one definition at parameters that are apart may still share
            // a value, the empty array of each: its expansions say
            // CR claude for claude: [bug/high] Outside `shared`, two applications of
            // one definition whose parameters are apart are answered apart without
            // their expansions, though a member that holds no parameter is a value of
            // both: over `type T<'a> = [`K('a), null]; type W<'a> = `W(T<'a>)`,
            // `W<i64>` and `W<string>` share `W(null)`. The arm `[W<i64>, `W(i64)] as
            // v` over `[W<string>, `W(i64)]` then types `v` as `W(i64)`, takes
            // `W(null)` at run time, and fused code answers 1 where the node-walk logs
            // "can't add null"; the same test written `W(T<i64>)` is typed with the
            // null. Return here only on a true and let a false fall to the expansion
            // arm, as `shared` does. probe:
            // design/review-2026-11-16/repro/t-contains-b-could-match-same-definition-apart.gx
            // (t-contains-b-02)
            if alike || !hist.shared {
                return Ok(alike);
            }
        }
        match (self, t) {
            (t0 @ Self::Ref(_), t1) | (t0, t1 @ Self::Ref(_)) => {
                let key = (hist.ref_id(t0, env), hist.ref_id(t1, env));
                if memo_on() && hist.refuted.get().is_some_and(|m| m.contains_key(&key)) {
                    return Ok(false);
                }
                if hist.shared
                    && let Some(entered) = hist.hist.get(&key)
                {
                    hist.lowest = hist.lowest.min(entered.depth);
                    return Ok(false);
                }
                // a pair of definitions unfolded as often as a walk may is
                // met again as the innermost of them in progress
                let defs = def_pair(env, t0, t1);
                let again = match hist.hist.contains_key(&key) {
                    true => Some(key),
                    false => hist.unfoldings.again(defs),
                };
                if let Some(again) = again
                    && let Some(entered) = hist.hist.get_mut(&again)
                {
                    if entered.node == NO_NODE {
                        entered.node = hist.rests.node()
                    }
                    let node = entered.node;
                    hist.rests.read(node);
                    return Ok(true);
                }
                if !hist.pairs.unfold() {
                    return Ok(true);
                }
                #[cfg(test)]
                MATCH_PAIRS.with(|n| n.set(n.get() + 1));
                let t0 = t0.lookup_ref(env)?;
                let t1 = t1.lookup_ref(env)?;
                let mark = hist.rests.mark();
                let depth = hist.hist.len() as u32;
                hist.insert(key, Entered { node: NO_NODE, depth });
                hist.unfoldings.enter(defs, key);
                let r = t0.could_match_int(env, hist, &t1);
                hist.unfoldings.leave(defs);
                let node = hist.remove(&key).map_or(NO_NODE, |e| e.node);
                if hist.lowest >= depth {
                    hist.lowest = NO_DEPTH
                }
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
                    && hist.lowest == NO_DEPTH
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
            (Type::Primitive(p), Self::Primitive(s)) => Ok(p.intersects(*s)),
            (t0, Self::Primitive(s)) => {
                for t1 in s.iter() {
                    let t1 = Type::Primitive(t1.into());
                    if t0.decided_holds(env, &t1)? {
                        return Ok(true);
                    }
                }
                Ok(false)
            }
            (Type::Primitive(p), Type::Error(_)) => Ok(p.contains(Typ::Error)),
            (Type::Error(t0), Type::Error(t1)) => t0.could_match_int(env, hist, t1),
            (Type::Array(_), Type::Array(_))
            | (Type::List(_), Type::List(_))
            | (Type::Map(_), Type::Map(_))
                if hist.shared =>
            {
                Ok(true)
            }
            (
                Type::Abstract { id: id0, params: p0 },
                Type::Abstract { id: id1, params: p1 },
            ) if hist.shared && env.abstract_reps.contains_key(id0) => {
                let mut same = |(t0, t1): (&Type, &Type)| -> Result<bool> {
                    let holds = |a: &Type, b: &Type| a.decided_holds(env, b);
                    Ok(t0.has_unbound()
                        || t1.has_unbound()
                        || (holds(t0, t1)? && holds(t1, t0)?))
                };
                Ok(id0 == id1
                    && p0.len() == p1.len()
                    && p0
                        .iter()
                        .zip(p1.iter())
                        .map(&mut same)
                        .collect::<Result<AndAc>>()?
                        .0)
            }
            // CR claude for eric: [bug/high] Outside `shared`, could_match reads two
            // arrays (and lists, maps) as apart where their elements are, though the
            // empty one is a value of both and passes either's run-time test.
            // Type::narrow_pattern_inner keeps only the members could_match admits, so
            // over `[(Array<string>, f64), (Array<i64>, i64)]` the arm `(Array<i64>,
            // [i64, f64]) as v` types `v.1` as i64, takes `([], 2.5)` at run time, and
            // fused code answers 1 where the node-walk answers 3.5; with a variant
            // member both engines log a coverage hole. Either keep a member whose value
            // may pass the test (as cast.rs::may_pass judges it) and type its
            // collection part as the test's over `_`, or refuse the arm naming the
            // member. probe:
            // design/review-2026-11-16/repro/t-contains-b-could-match-empty-array-narrowing.gx
            // (t-contains-b-01)
            (Type::Array(t0), Type::Array(t1)) => t0.could_match_int(env, hist, t1),
            (Type::List(t0), Type::List(t1)) => t0.could_match_int(env, hist, t1),
            (Type::Primitive(p), Type::Array(_)) => Ok(p.contains(Typ::Array)),
            (Type::Map(m0), Type::Map(m1)) => {
                Ok(m0.key.could_match_int(env, hist, &m1.key)?
                    && m0.value.could_match_int(env, hist, &m1.value)?)
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
        for b in cv.cell_above().iter() {
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

    /// Whether one value may be a value of both `self` and `t`:
    /// [`Self::could_match`] either way, under [`MatchHist::shared`], where a
    /// pair met again inside itself is apart: a value is finite. An open
    /// cell may be any type.
    pub fn shares_value(&self, env: &Env, t: &Self) -> Result<bool> {
        let sharing = || MatchHist { shared: true, ..MatchHist::new() };
        Ok(self.could_match_int(env, &mut sharing(), t)?
            || t.could_match_int(env, &mut sharing(), self)?)
    }

    /// Whether a value of `t` may pass the run-time test (`is_a`) of the
    /// predicate `self`, where a ⊥ part, a cell nothing decided, passes
    /// any value.
    pub fn could_pass(&self, env: &Env, t: &Self) -> Result<bool> {
        type Done = AHashMap<NodeKey, (Type, Option<Type>)>;
        fn bottom_as_any(t: &Type, done: &mut Done) -> Option<Type> {
            ensure_sufficient(|| match t {
                Type::Bottom => Some(Type::Any),
                Type::TVar(tv) => {
                    tv.binding().map(|b| bottom_as_any(&b, done).unwrap_or(b))
                }
                t => {
                    #[cfg(test)]
                    BOTTOM_AS_ANY_STEPS.with(|n| n.set(n.get() + 1));
                    let key = t.node_key();
                    if let Some((_, r)) = key.and_then(|k| done.get(&k)) {
                        return r.clone();
                    }
                    let r = t.cow_children(&mut |c| bottom_as_any(c, done));
                    if let Some(k) = key {
                        done.insert(k, (t.clone(), r.clone()));
                    }
                    r
                }
            })
        }
        match bottom_as_any(self, &mut LPooled::<Done>::take()) {
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
                    !m.has_unbound() && self.decided_holds(env, m).unwrap_or(false)
                }
                Err(_) => false,
            })
    }
}

#[cfg(test)]
mod tests {
    use super::{MATCH_CALLS, MATCH_PAIRS, REST_STEPS};
    use crate::typ::cast::tests::{parsed, typed};

    fn shares_value(defs: &[(&str, &str)], t0: &str, t1: &str) -> bool {
        let (env, _) = typed(defs, "null");
        parsed(&env, t0).shares_value(&env, &parsed(&env, t1)).unwrap()
    }

    #[test]
    fn recursive_types_share_a_value_only_where_a_finite_one_is_of_both() {
        let apart = [
            ("Ti", "[`L(i64), `N(Ti, Ti)]"),
            ("Ts", "[`L(string), `N(Ts, Ts)]"),
            ("Ei", "`N(Ei)"),
            ("Es", "`N(Es)"),
        ];
        assert!(!shares_value(&apart, "Ti", "Ts"));
        assert!(!shares_value(&apart, "`N(Ti, Ti)", "`N(Ts, Ts)"));
        assert!(!shares_value(&apart, "Ei", "Es"));
        assert!(shares_value(&apart, "Ti", "Ti"));
        let meet = [
            ("Ti", "[`L(i64), `N(Ti, Ti)]"),
            ("Tj", "[`L(i64), `N(Tj, Tj)]"),
            ("Lp", "[`L(i64), `L(string), `N(Lp, Lp)]"),
            ("Ts", "[`L(string), `N(Ts, Ts)]"),
        ];
        assert!(shares_value(&meet, "Ti", "Tj"));
        assert!(shares_value(&meet, "Ti", "Lp"));
        assert!(shares_value(&meet, "Ts", "Lp"));
    }

    // Ua against Ub is walked with the pair Ta, Tb in progress and read
    // apart there; Ta, Tb then share `Z, so Ua, Ub, asked again beside it,
    // share `M(`Z).
    #[test]
    fn a_pair_apart_on_one_in_progress_is_asked_again_when_that_pair_shares() {
        let defs = [
            ("Ta", "[`N(Ua), `Z]"),
            ("Tb", "[`N(Ub), `Z]"),
            ("Ua", "`M(Ta)"),
            ("Ub", "`M(Tb)"),
        ];
        assert!(shares_value(&defs, "Ta", "Tb"));
        assert!(shares_value(&defs, "(Ta, Ua)", "(Tb, Ub)"));
        assert!(shares_value(&defs, "(Ua, Ta)", "(Ub, Tb)"));
    }

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

    /// `P_k = ([Error<P_k+1>, `K], C1, `A)` for k up to `n`, over a
    /// chain `C1 .. Cm` whose last union holds an error of every `P_k`,
    /// and a twin `Q_k` over `D1 .. Dm` with `B. `P_k` is written
    /// `P<999 - k>`, so a union scan meets the innermost pair first.
    fn nested_pairs(n: usize, m: usize) -> Vec<(String, String)> {
        let mut defs = vec![];
        for (chain, name, tag) in [("C", "P", "`A"), ("D", "Q", "`B")] {
            let named = |k: usize| format!("{name}{}", 999 - k);
            for j in 1..m {
                defs.push((
                    format!("{chain}{j}"),
                    format!("[({chain}{}, `Cc), `X]", j + 1),
                ));
            }
            let errs: Vec<String> =
                (1..=n).map(|k| format!("Error<{}>", named(k))).collect();
            defs.push((format!("{chain}{m}"), format!("[{}]", errs.join(", "))));
            for k in 1..n {
                let body = format!("([Error<{}>, `K], {chain}1, {tag})", named(k + 1));
                defs.push((named(k), body));
            }
            defs.push((named(n), format!("({chain}1, {tag})")));
        }
        defs
    }

    /// A predicate whose ⊥ parts and bound cells a DAG of tuples shares
    /// is read once a node, and keeps its sharing for the match.
    #[test]
    fn could_pass_reads_a_predicate_dag_once_a_node() {
        use crate::{
            env::Env,
            typ::{TVar, Type},
        };
        use arcstr::literal;
        use triomphe::Arc;
        let n = 20;
        let pair = |t: Type| Type::Tuple(Arc::from_iter([t.clone(), t]));
        let c = TVar::empty_named(literal!("c"));
        let i64 = parsed(&Env::default(), "i64");
        c.bind(i64.clone());
        let (mut p, mut q, mut t) = (Type::TVar(c), Type::Bottom, i64);
        for _ in 0..n {
            (p, q, t) = (pair(p), pair(q), pair(t));
        }
        for pred in [p, q] {
            super::BOTTOM_AS_ANY_STEPS.with(|n| n.set(0));
            MATCH_PAIRS.with(|n| n.set(0));
            assert!(pred.could_pass(&Env::default(), &t).unwrap());
            let steps = super::BOTTOM_AS_ANY_STEPS.with(|n| n.get());
            let pairs = MATCH_PAIRS.with(|n| n.get());
            assert!(steps <= 2 * n + 8, "{steps} reads of a DAG of depth {n}");
            assert!(pairs <= 4 * n + 128, "{pairs} pairs matched at depth {n}");
        }
    }

    // Each P_k fails at its tag and takes back the trues of the chain,
    // which rest on it, so the level above walks the chain again; the
    // node pairs that walk found apart stay apart, as a pair of names
    // found apart does. A larger family meets the walk's pair budget and
    // is still found apart.
    #[test]
    fn could_match_keeps_the_node_pairs_found_apart() {
        let walk = |n: usize, m: usize| {
            let defs = nested_pairs(n, m);
            let defs: Vec<(&str, &str)> =
                defs.iter().map(|(n, b)| (&**n, &**b)).collect();
            let (env, _) = typed(&defs, "null");
            let (p, q) = (parsed(&env, "P998"), parsed(&env, "Q998"));
            MATCH_CALLS.with(|n| n.set(0));
            MATCH_PAIRS.with(|n| n.set(0));
            let could = p.could_match(&env, &q).unwrap();
            (could, MATCH_CALLS.with(|n| n.get()), MATCH_PAIRS.with(|n| n.get()))
        };
        assert_eq!(walk(16, 8), (false, 3868, 384));
        assert_eq!(walk(64, 32), (false, 9437, super::super::setops::WALK_PAIRS));
    }

    // Past the walk's pair budget could_match answers an overlap wherever a
    // pair is met, beside the pair that spent it too: the nested pairs take
    // n * (n + m) pairs, and once that passes the budget `H(Z0) against
    // `H(Z1) overlaps, so the arm testing [`G(P998), `H(Z0)] is not found
    // dead.
    #[test]
    fn could_match_answers_an_overlap_past_the_pair_budget() {
        let walk = |n: usize, m: usize| {
            let mut defs = nested_pairs(n, m);
            defs.push(("Z0".into(), "[`ZA]".into()));
            defs.push(("Z1".into(), "[`ZB]".into()));
            pairs_walked(&defs, "[`G(P998), `H(Z0)]", "[`G(Q998), `H(Z1)]")
        };
        assert_eq!(walk(18, 32), (false, 901));
        assert_eq!(walk(20, 32), (true, super::super::setops::WALK_PAIRS));
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
