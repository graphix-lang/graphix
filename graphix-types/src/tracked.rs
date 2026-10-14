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

/// A [`Map`] with fork and join.
#[derive(Clone, Debug)]
pub struct TrackedMap<K: Hash + Eq + Clone + Debug, V: Clone + Debug> {
    map: Map<K, V>,
    /// The keys written since the fork; `None` outside one.
    touched: Option<LPooled<Vec<K>>>,
    /// Bumped by every write.
    generation: u64,
    /// The generation of the map this one forked from, at the fork.
    forked_at: u64,
}

impl<K: Hash + Eq + Clone + Debug, V: Clone + Debug> Default for TrackedMap<K, V> {
    fn default() -> Self {
        Self::from(Map::default())
    }
}

impl<K: Hash + Eq + Clone + Debug, V: Clone + Debug> From<Map<K, V>>
    for TrackedMap<K, V>
{
    fn from(map: Map<K, V>) -> Self {
        Self { map, touched: None, generation: 0, forked_at: 0 }
    }
}

impl<K: Hash + Eq + Clone + Debug, V: Clone + Debug> Deref for TrackedMap<K, V> {
    type Target = Map<K, V>;

    fn deref(&self) -> &Map<K, V> {
        &self.map
    }
}

impl<K: Hash + Eq + Clone + Debug, V: Clone + Debug> TrackedMap<K, V> {
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
        }
    }

    /// Write back what `fork` wrote. A map written nowhere since the
    /// fork takes the fork's whole.
    // CR claude for claude: [test-gap/low] No test covers fork/join: not the fast path
    // (the parent is unwritten since the fork, so it takes the fork's whole map), the
    // slow path, a fork of a fork, a remove then join, or propagation of the parent's
    // `touched`. In the slow path, a key that a sibling task or the parent wrote after
    // the fork is overwritten by this fork's value without a word. `impls` (keyed by
    // trait, a whole Vec) and `names` (keyed by scope) would then silently drop an
    // earlier task's entry if a task ever registered one. Add unit tests for these
    // cases. Keep the base map at fork (an O(1) persistent clone), and debug-assert in
    // the slow path that the parent's entry for each touched key is still the base's,
    // so a conflicting write is reported instead of lost. (t-misc-04) (t-misc-04)
    // 2026-10-14 claude: the fast path, the slow path, siblings, a fork of a fork,
    // removes and an idle fork are tested (tracked::tests). The conflict check stands:
    // it needs the base map kept at the fork and V: PartialEq, which not every value
    // type here has.
    pub fn join(&mut self, fork: Self) {
        let Self { map, touched, generation, forked_at } = fork;
        if self.generation == forked_at {
            self.map = map;
            self.generation = generation;
            if let (Some(mine), Some(mut theirs)) = (&mut self.touched, touched) {
                mine.extend(theirs.drain(..))
            }
            return;
        }
        let Some(mut touched) =
            merged_keys(&mut self.generation, &mut self.touched, touched)
        else {
            return;
        };
        for k in touched.drain(..) {
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

/// A [`Set`] with fork and join.
#[derive(Clone, Debug)]
pub struct TrackedSet<K: Hash + Eq + Clone + Debug> {
    set: Set<K>,
    touched: Option<LPooled<Vec<K>>>,
    /// See [`TrackedMap`].
    generation: u64,
    forked_at: u64,
}

impl<K: Hash + Eq + Clone + Debug> Default for TrackedSet<K> {
    fn default() -> Self {
        Self::from(Set::default())
    }
}

impl<K: Hash + Eq + Clone + Debug> From<Set<K>> for TrackedSet<K> {
    fn from(set: Set<K>) -> Self {
        Self { set, touched: None, generation: 0, forked_at: 0 }
    }
}

impl<K: Hash + Eq + Clone + Debug> Deref for TrackedSet<K> {
    type Target = Set<K>;

    fn deref(&self) -> &Set<K> {
        &self.set
    }
}

impl<K: Hash + Eq + Clone + Debug> TrackedSet<K> {
    fn touch(&mut self, k: &K) {
        self.generation += 1;
        if let Some(t) = &mut self.touched {
            t.push(k.clone())
        }
    }

    pub fn insert(&mut self, k: K) -> bool {
        self.touch(&k);
        self.set.insert_cow(k)
    }

    pub fn remove(&mut self, k: &K) -> bool {
        self.touch(k);
        self.set.remove_cow(k)
    }

    pub fn iter(&self) -> <&Set<K> as IntoIterator>::IntoIter {
        (&self.set).into_iter()
    }

    pub fn extend(&mut self, ks: impl IntoIterator<Item = K>) {
        for k in ks {
            self.insert(k);
        }
    }

    pub fn remove_many(&mut self, ks: impl IntoIterator<Item = K>) {
        let mut ks: LPooled<Vec<K>> = ks.into_iter().collect();
        for k in ks.iter() {
            self.touch(k);
        }
        self.set = self.set.remove_many(ks.drain(..));
    }

    /// Keep only the members `keep` accepts.
    pub fn retain(&mut self, mut keep: impl FnMut(&K) -> bool) {
        let mut gone: LPooled<Vec<K>> =
            self.set.into_iter().filter(|k| !keep(k)).cloned().collect();
        if !gone.is_empty() {
            self.remove_many(gone.drain(..))
        }
    }

    pub fn clear(&mut self) {
        let mut all: LPooled<Vec<K>> = self.set.into_iter().cloned().collect();
        self.remove_many(all.drain(..))
    }

    pub fn fork(&self) -> Self {
        Self {
            set: self.set.clone(),
            touched: Some(LPooled::take()),
            generation: self.generation,
            forked_at: self.generation,
        }
    }

    pub fn join(&mut self, fork: Self) {
        let Self { set, touched, generation, forked_at } = fork;
        if self.generation == forked_at {
            self.set = set;
            self.generation = generation;
            if let (Some(mine), Some(mut theirs)) = (&mut self.touched, touched) {
                mine.extend(theirs.drain(..))
            }
            return;
        }
        let Some(mut touched) =
            merged_keys(&mut self.generation, &mut self.touched, touched)
        else {
            return;
        };
        for k in touched.drain(..) {
            match set.contains(&k) {
                true => self.set.insert_cow(k),
                false => self.set.remove_cow(&k),
            };
        }
    }
}

impl<'a, K: Hash + Eq + Clone + Debug, V: Clone + Debug> IntoIterator
    for &'a TrackedMap<K, V>
{
    type Item = <&'a Map<K, V> as IntoIterator>::Item;
    type IntoIter = <&'a Map<K, V> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        (&self.map).into_iter()
    }
}

impl<'a, K: Hash + Eq + Clone + Debug> IntoIterator for &'a TrackedSet<K> {
    type Item = <&'a Set<K> as IntoIterator>::Item;
    type IntoIter = <&'a Set<K> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        (&self.set).into_iter()
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
