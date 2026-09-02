use ahash::AHashMap;
use anyhow::Result;
use std::hash::Hash;

/// A bounded memo of a fast fn's compiled configuration — a regex, an escape
/// table, a template registry — keyed by the argument value(s) that configure
/// it. The fn stays a pure function of its arguments: a miss rebuilds from the
/// key, so the memo is a cache, never state, and one per thread serves every
/// site (a fast fn has no site identity). When full a random entry is evicted,
/// so it never holds more than `cap` entries.
pub struct FastMemo<K, V> {
    map: AHashMap<K, V>,
    cap: usize,
}

impl<K: Hash + Eq + Clone, V> FastMemo<K, V> {
    pub fn new(cap: usize) -> Self {
        Self { map: AHashMap::with_capacity(cap), cap }
    }

    /// Run `f` on the entry for `key`, building it with `build` on a
    /// miss; a failed build is not remembered.
    pub fn with<T>(
        &mut self,
        key: &K,
        build: impl FnOnce() -> Result<V>,
        f: impl FnOnce(&V) -> T,
    ) -> Result<T> {
        if let Some(v) = self.map.get(key) {
            return Ok(f(v));
        }
        let v = build()?;
        if self.map.len() >= self.cap {
            let _ = self.map.extract_if(|_, _| true).next();
        }
        Ok(f(self.map.entry(key.clone()).or_insert(v)))
    }
}
