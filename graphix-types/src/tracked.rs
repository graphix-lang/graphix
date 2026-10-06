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
    mine: &mut Option<Vec<K>>,
    theirs: Option<Vec<K>>,
) -> Option<Vec<K>> {
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
    // CR claude for claude: [perf] Each fork's touched log is a plain Vec. It allocates
    // at the fork's first write and grows by one entry per write, repeats included
    // (TrackedSet's at line 175 does the same), and compile tasks fork per statement
    // and per static bind. remove_many, clear and retain (lines 93-130, 229-248) also
    // collect their keys into plain Vecs, where env.rs:371-387 uses LPooled<Vec<K>> for
    // the same collect-then-remove_many. Pool them: LPooled for the scratch lists and
    // GPooled for touched, since a branch's fork is made on a pool worker
    // (graphix-compiler/src/branch.rs:541) and joined on the parent's thread.
    // (t-misc-11)
    touched: Option<Vec<K>>,
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
        let all: Vec<K> = self.map.into_iter().map(|(k, _)| k.clone()).collect();
        self.remove_many(all)
    }

    pub fn get_mut(&mut self, k: &K) -> Option<&mut V> {
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
        let ks: Vec<K> = ks.into_iter().collect();
        for k in ks.iter() {
            self.touch(k);
        }
        self.map = self.map.remove_many(ks);
    }

    /// Keep only the entries `keep` accepts.
    pub fn retain(&mut self, mut keep: impl FnMut(&K, &V) -> bool) {
        let gone: Vec<K> = self
            .map
            .into_iter()
            .filter(|(k, v)| !keep(k, v))
            .map(|(k, _)| k.clone())
            .collect();
        if !gone.is_empty() {
            self.remove_many(gone)
        }
    }

    /// A copy that records what it writes.
    pub fn fork(&self) -> Self {
        Self {
            map: self.map.clone(),
            touched: Some(Vec::new()),
            generation: self.generation,
            forked_at: self.generation,
        }
    }

    /// Write back what `fork` wrote. A map written nowhere since the
    /// fork takes the fork's whole.
    // CR claude for claude: [risk] join writes back the fork's value of every key it
    // touched, so it matches serial order only while sibling forks touch disjoint keys.
    // A breach is silent: two siblings calling register_impl on one trait keep only the
    // later impl, because impls holds the whole list per key. design/parallel_eval.md
    // ("Joins touch disjoint keys") says GRAPHIX_PAR_AUDIT asserts the rule here, but
    // no code does; that audit (graphix-compiler/src/branch.rs:783) checks only
    // variable reads. get_mut (line 98) also touches a key it does not find, and a
    // slow-path join turns that into a removal of whatever an earlier sibling put
    // there. (t-misc-06)
    pub fn join(&mut self, fork: Self) {
        let Self { map, touched, generation, forked_at } = fork;
        if self.generation == forked_at {
            self.map = map;
            self.generation = generation;
            if let (Some(mine), Some(theirs)) = (&mut self.touched, touched) {
                mine.extend(theirs)
            }
            return;
        }
        let Some(touched) = merged_keys(&mut self.generation, &mut self.touched, touched)
        else {
            return;
        };
        for k in touched {
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
    touched: Option<Vec<K>>,
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
        let ks: Vec<K> = ks.into_iter().collect();
        for k in ks.iter() {
            self.touch(k);
        }
        self.set = self.set.remove_many(ks);
    }

    /// Keep only the members `keep` accepts.
    pub fn retain(&mut self, mut keep: impl FnMut(&K) -> bool) {
        let gone: Vec<K> = self.set.into_iter().filter(|k| !keep(k)).cloned().collect();
        if !gone.is_empty() {
            self.remove_many(gone)
        }
    }

    pub fn clear(&mut self) {
        let all: Vec<K> = self.set.into_iter().cloned().collect();
        self.remove_many(all)
    }

    pub fn fork(&self) -> Self {
        Self {
            set: self.set.clone(),
            touched: Some(Vec::new()),
            generation: self.generation,
            forked_at: self.generation,
        }
    }

    pub fn join(&mut self, fork: Self) {
        let Self { set, touched, generation, forked_at } = fork;
        if self.generation == forked_at {
            self.set = set;
            self.generation = generation;
            if let (Some(mine), Some(theirs)) = (&mut self.touched, touched) {
                mine.extend(theirs)
            }
            return;
        }
        let Some(touched) = merged_keys(&mut self.generation, &mut self.touched, touched)
        else {
            return;
        };
        for k in touched {
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
