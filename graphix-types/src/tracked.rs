//! Persistent maps and sets a compile task forks and joins: a fork
//! records every key it writes, and joining it writes those keys'
//! values as the fork left them, or their absence, back.

use crate::env::{Map, Set};
use ahash::AHashSet;
use poolshark::local::LPooled;
use std::{fmt::Debug, hash::Hash, ops::Deref};

/// The keys a joined fork wrote, each once, recorded as writes of the
/// container it joins; `None` when it wrote none.
fn merged_keys<K: Hash + Eq + Clone>(
    generation: &mut u64,
    mine: &mut Option<LPooled<Vec<K>>>,
    theirs: Option<LPooled<Vec<K>>>,
) -> Option<LPooled<Vec<K>>> {
    let mut keys = theirs.filter(|t| !t.is_empty())?;
    let mut seen: LPooled<AHashSet<K>> = LPooled::take();
    keys.retain(|k| seen.insert(k.clone()));
    *generation += 1;
    if let Some(mine) = mine {
        mine.extend(keys.iter().cloned());
    }
    Some(keys)
}

/// Add a joined fork's touched keys to the parent's, whose order does
/// not matter: the larger list is kept and the smaller copied in, so a
/// chain of nested forks copies each key a bounded number of times.
fn absorb<K>(mine: &mut LPooled<Vec<K>>, theirs: &mut LPooled<Vec<K>>) {
    if theirs.len() > mine.len() {
        std::mem::swap(mine, theirs)
    }
    mine.extend(theirs.drain(..))
}

/// Whether two of a map's values are one write: identity for a shared
/// value, equality for a plain one. [`TrackedMap::join`] asks it of each
/// key a fork wrote, so a write made since the fork is never replaced
/// unseen.
pub trait SameValue {
    fn same_value(&self, other: &Self) -> bool;
}

impl<T: ?Sized> SameValue for triomphe::Arc<T> {
    fn same_value(&self, other: &Self) -> bool {
        triomphe::Arc::ptr_eq(self, other)
    }
}

macro_rules! same_by_eq {
    ($($t:ty),*) => {$(
        impl SameValue for $t {
            fn same_value(&self, other: &Self) -> bool {
                self == other
            }
        }
    )*};
}

same_by_eq!((), crate::BindId, crate::LambdaId, netidx_value::Value);

#[cfg(test)]
same_by_eq!(u32);

/// A [`Map`] with fork and join.
#[derive(Clone, Debug)]
pub struct TrackedMap<K: Hash + Eq + Clone + Debug, V: Clone + Debug + SameValue> {
    map: Map<K, V>,
    /// The keys written since the fork; `None` outside one.
    touched: Option<LPooled<Vec<K>>>,
    /// Bumped by every write.
    generation: u64,
    /// The generation of the map this one forked from, at the fork.
    forked_at: u64,
    /// The map this one forked from, as it stood at the fork.
    base: Option<Map<K, V>>,
}

impl<K: Hash + Eq + Clone + Debug, V: Clone + Debug + SameValue> Default
    for TrackedMap<K, V>
{
    fn default() -> Self {
        Self::from(Map::default())
    }
}

impl<K: Hash + Eq + Clone + Debug, V: Clone + Debug + SameValue> From<Map<K, V>>
    for TrackedMap<K, V>
{
    fn from(map: Map<K, V>) -> Self {
        Self { map, touched: None, generation: 0, forked_at: 0, base: None }
    }
}

impl<K: Hash + Eq + Clone + Debug, V: Clone + Debug + SameValue> Deref
    for TrackedMap<K, V>
{
    type Target = Map<K, V>;

    fn deref(&self) -> &Map<K, V> {
        &self.map
    }
}

impl<K: Hash + Eq + Clone + Debug, V: Clone + Debug + SameValue> TrackedMap<K, V> {
    fn touch(&mut self, k: &K) {
        self.generation += 1;
        if let Some(t) = &mut self.touched {
            t.push(k.clone())
        }
    }

    pub fn insert(&mut self, k: K, v: V) -> Option<V> {
        self.touch(&k);
        self.map.insert_cow(k, v)
    }

    pub fn remove(&mut self, k: &K) -> Option<V> {
        self.touch(k);
        self.map.remove_cow(k)
    }

    pub fn contains_key(&self, k: &K) -> bool {
        self.map.get(k).is_some()
    }

    pub fn iter(&self) -> <&Map<K, V> as IntoIterator>::IntoIter {
        (&self.map).into_iter()
    }

    pub fn extend(&mut self, kvs: impl IntoIterator<Item = (K, V)>) {
        for (k, v) in kvs {
            self.insert(k, v);
        }
    }

    pub fn clear(&mut self) {
        let mut all: LPooled<Vec<K>> =
            self.map.into_iter().map(|(k, _)| k.clone()).collect();
        self.remove_many(all.drain(..))
    }

    pub fn get_mut(&mut self, k: &K) -> Option<&mut V> {
        self.map.get(k)?;
        self.touch(k);
        self.map.get_mut_cow(k)
    }

    pub fn get_or_default(&mut self, k: K) -> &mut V
    where
        V: Default,
    {
        self.touch(&k);
        self.map.get_or_default_cow(k)
    }

    pub fn remove_many(&mut self, ks: impl IntoIterator<Item = K>) {
        let mut ks: LPooled<Vec<K>> = ks.into_iter().collect();
        for k in ks.iter() {
            self.touch(k);
        }
        self.map = self.map.remove_many(ks.drain(..));
    }

    /// Keep only the entries `keep` accepts.
    pub fn retain(&mut self, mut keep: impl FnMut(&K, &V) -> bool) {
        let mut gone: LPooled<Vec<K>> = self
            .map
            .into_iter()
            .filter(|(k, v)| !keep(k, v))
            .map(|(k, _)| k.clone())
            .collect();
        if !gone.is_empty() {
            self.remove_many(gone.drain(..))
        }
    }

    /// A copy that records what it writes.
    pub fn fork(&self) -> Self {
        Self {
            map: self.map.clone(),
            touched: Some(LPooled::take()),
            generation: self.generation,
            forked_at: self.generation,
            base: Some(self.map.clone()),
        }
    }

    /// Write back what `fork` wrote. A map written nowhere since the
    /// fork takes the fork's whole.
    pub fn join(&mut self, fork: Self) {
        let Self { map, touched, generation, forked_at, base } = fork;
        if self.generation == forked_at {
            self.map = map;
            self.generation = generation;
            if let (Some(mine), Some(mut theirs)) = (&mut self.touched, touched) {
                // XCR claude for claude: [perf/low] [perf/low] A static bind runs in a
                // forked task, and a nested bind forks inside it. Every join copies
                // what the child accumulated: this `touched` list, and in
                // `CompileCtx::join` also pending_refs, pending_settles and discarded.
                // Since the child already holds what its own descendants joined into
                // it, a resolution chain of depth d copies its whole subtree at every
                // level: O(depth x instances). TaskJoin per call is 45 / 85 / 165 us at
                // chain length 800 / 1600 / 3200 (debug). Fix: for accumulators whose
                // order does not matter (touched keys, pending_refs, discarded), keep
                // the larger collection and extend it with the smaller (swap first).
                // For pending_settles, push the child's Vec as one chunk and flatten
                // once where the frame drains. probe:
                // design/review-2026-10-19/repro/x-perf-compile-nested-task-join.sh
                // (x-perf-compile-03)
                // 2026-10-20 claude: TrackedMap/TrackedSet joins keep the larger
                // touched list and copy the smaller in (absorb), and CompileCtx::join
                // does the same for pending_refs and discarded. pending_settles still
                // extends the parent frame with the child's (the chunk-and-flatten form
                // is not done). Not re-measured.
                absorb(mine, &mut theirs)
            }
            return;
        }
        let Some(mut touched) =
            merged_keys(&mut self.generation, &mut self.touched, touched)
        else {
            return;
        };
        for k in touched.drain(..) {
            // CR claude for eric: [risk/medium] The only guard against a fork
            // overwriting a sibling's write is this debug_assert. In a release build
            // the loop below keeps the later-joined value, so a registry with an
            // aggregate value (Env::impls holds one Vec per trait) silently loses the
            // other task's registration; check_sig's Cast re-registration in sibling
            // module tasks is one live case. Either make the check an assert! in
            // release (one comparison per written key) or key Env::impls by
            // implementation so that independent registrations never share a key. Found
            // by reading; not run. (t-env-02)
            let same = |a: Option<&V>, b: Option<&V>| match (a, b) {
                (None, None) => true,
                (Some(a), Some(b)) => a.same_value(b),
                _ => false,
            };
            // a write since the fork is replaced only by a different one
            debug_assert!(
                same(map.get(&k), self.map.get(&k))
                    || base
                        .as_ref()
                        .is_none_or(|base| same(self.map.get(&k), base.get(&k))),
                "a fork's write to {k:?} would replace a write made since the fork"
            );
            match map.get(&k) {
                Some(v) => {
                    self.map.insert_cow(k, v.clone());
                }
                None => {
                    self.map.remove_cow(&k);
                }
            }
        }
    }
}

/// A set with fork and join: a [`TrackedMap`] to `()`, so a join checks a
/// fork's writes as the map's does.
#[derive(Clone, Debug)]
pub struct TrackedSet<K: Hash + Eq + Clone + Debug>(TrackedMap<K, ()>);

impl<K: Hash + Eq + Clone + Debug> Default for TrackedSet<K> {
    fn default() -> Self {
        Self(TrackedMap::default())
    }
}

impl<K: Hash + Eq + Clone + Debug> From<Set<K>> for TrackedSet<K> {
    fn from(set: Set<K>) -> Self {
        Self(TrackedMap::from(
            set.into_iter().map(|k| (k.clone(), ())).collect::<Map<K, ()>>(),
        ))
    }
}

impl<K: Hash + Eq + Clone + Debug> TrackedSet<K> {
    /// The members as a plain set.
    pub fn to_set(&self) -> Set<K> {
        self.iter().cloned().collect()
    }

    /// The member equal to `k`, as the set holds it.
    pub fn get(&self, k: &K) -> Option<&K> {
        self.0.map.get_key(k)
    }

    pub fn contains<Q: ?Sized + Hash + Eq>(&self, k: &Q) -> bool
    where
        K: std::borrow::Borrow<Q>,
    {
        self.0.map.get(k).is_some()
    }

    pub fn len(&self) -> usize {
        self.0.map.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Whether `k` was not a member.
    pub fn insert(&mut self, k: K) -> bool {
        self.0.insert(k, ()).is_none()
    }

    /// Whether `k` was a member.
    pub fn remove(&mut self, k: &K) -> bool {
        self.0.remove(k).is_some()
    }

    pub fn iter(&self) -> impl Iterator<Item = &K> {
        self.0.iter().map(|(k, ())| k)
    }

    pub fn extend(&mut self, ks: impl IntoIterator<Item = K>) {
        self.0.extend(ks.into_iter().map(|k| (k, ())))
    }

    pub fn remove_many(&mut self, ks: impl IntoIterator<Item = K>) {
        self.0.remove_many(ks)
    }

    /// Keep only the members `keep` accepts.
    pub fn retain(&mut self, mut keep: impl FnMut(&K) -> bool) {
        self.0.retain(|k, ()| keep(k))
    }

    pub fn clear(&mut self) {
        self.0.clear()
    }

    /// A copy that records what it writes.
    pub fn fork(&self) -> Self {
        Self(self.0.fork())
    }

    /// Write back what `fork` wrote; see [`TrackedMap::join`].
    pub fn join(&mut self, fork: Self) {
        self.0.join(fork.0)
    }
}

impl<'a, K: Hash + Eq + Clone + Debug, V: Clone + Debug + SameValue> IntoIterator
    for &'a TrackedMap<K, V>
{
    type Item = <&'a Map<K, V> as IntoIterator>::Item;
    type IntoIter = <&'a Map<K, V> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        (&self.map).into_iter()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn map(kvs: &[(u32, u32)]) -> TrackedMap<u32, u32> {
        let mut m = TrackedMap::default();
        for (k, v) in kvs {
            m.insert(*k, *v);
        }
        m
    }

    fn entries(m: &TrackedMap<u32, u32>) -> Vec<(u32, u32)> {
        let mut v: Vec<_> = m.iter().map(|(k, v)| (*k, *v)).collect();
        v.sort();
        v
    }

    /// The parent unwritten since the fork takes the fork's whole map.
    #[test]
    fn join_into_an_unwritten_parent() {
        let mut parent = map(&[(1, 1), (2, 2)]);
        let mut fork = parent.fork();
        fork.insert(3, 3);
        fork.remove(&1);
        parent.join(fork);
        assert_eq!(entries(&parent), [(2, 2), (3, 3)]);
    }

    /// A parent written since the fork takes the fork's writes, removes
    /// included, and keeps its own.
    #[test]
    fn join_into_a_written_parent() {
        let mut parent = map(&[(1, 1), (2, 2)]);
        let mut fork = parent.fork();
        fork.insert(3, 3);
        fork.remove(&1);
        parent.insert(4, 4);
        parent.join(fork);
        assert_eq!(entries(&parent), [(2, 2), (3, 3), (4, 4)]);
    }

    /// A sibling's write to a key another sibling also wrote is reported,
    /// never replaced unseen.
    #[test]
    #[cfg(debug_assertions)]
    #[should_panic(expected = "would replace a write made since the fork")]
    fn join_reports_a_conflicting_write() {
        let mut parent = map(&[(1, 1)]);
        let mut a = parent.fork();
        let mut b = parent.fork();
        a.insert(2, 2);
        b.insert(2, 3);
        parent.join(a);
        parent.join(b);
    }

    /// A set's join checks a fork's writes as a map's does: here one
    /// sibling removes a key the other inserted since the fork.
    #[test]
    #[cfg(debug_assertions)]
    #[should_panic(expected = "would replace a write made since the fork")]
    fn set_join_reports_a_conflicting_write() {
        let mut parent = TrackedSet::default();
        parent.insert(1u32);
        let mut a = parent.fork();
        let mut b = parent.fork();
        a.insert(2);
        b.insert(2);
        b.remove(&2);
        parent.join(a);
        parent.join(b);
    }

    /// Siblings forked together each bring back their own writes.
    #[test]
    fn join_siblings() {
        let mut parent = map(&[(1, 1)]);
        let mut a = parent.fork();
        let mut b = parent.fork();
        a.insert(2, 2);
        b.insert(3, 3);
        b.remove(&1);
        parent.join(a);
        parent.join(b);
        assert_eq!(entries(&parent), [(2, 2), (3, 3)]);
    }

    /// A fork of a fork joins into its fork, which carries the writes on
    /// to the parent as its own.
    #[test]
    fn join_a_fork_of_a_fork() {
        let mut parent = map(&[(1, 1)]);
        let mut outer = parent.fork();
        let mut inner = outer.fork();
        inner.insert(2, 2);
        outer.insert(3, 3);
        outer.join(inner);
        parent.insert(4, 4);
        parent.join(outer);
        assert_eq!(entries(&parent), [(1, 1), (2, 2), (3, 3), (4, 4)]);
    }

    /// A fork that wrote nothing changes nothing.
    #[test]
    fn join_an_idle_fork() {
        let mut parent = map(&[(1, 1)]);
        let fork = parent.fork();
        parent.insert(2, 2);
        parent.join(fork);
        assert_eq!(entries(&parent), [(1, 1), (2, 2)]);
    }
}
