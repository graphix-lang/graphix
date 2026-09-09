use ahash::AHashMap;
use anyhow::Result;
use std::hash::Hash;

/// A bounded memo of a fast fn's compiled configuration (a regex, an
/// escape table) keyed by the configuring argument value(s). A miss
/// rebuilds from the key; when full a random entry is evicted.
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
