//! Keeping what a widget built from each value of a list across a new
//! list, the GUI's and the TUI's alike.

use ahash::AHashMap;
use anyhow::Result;
use netidx_value::Value;
use poolshark::local::LPooled;
use std::{future::Future, hash::Hash};

/// Pair each of the `new` values with an old item made from an equal
/// value, the one at the same index first, else any; `None` where no old
/// item is left.
pub fn keep_equal<K: Eq + Hash, T>(
    old: impl IntoIterator<Item = (K, T)>,
    new: &[K],
) -> LPooled<Vec<Option<T>>> {
    let mut keys: LPooled<Vec<K>> = LPooled::take();
    let mut items: LPooled<Vec<Option<T>>> = LPooled::take();
    for (key, item) in old {
        keys.push(key);
        items.push(Some(item));
    }
    let mut index: Option<LPooled<AHashMap<&K, usize>>> = None;
    let mut next: LPooled<Vec<Option<usize>>> = LPooled::take();
    new.iter()
        .enumerate()
        .map(|(i, val)| {
            if keys.get(i) == Some(val) && items[i].is_some() {
                return items[i].take();
            }
            let index = index.get_or_insert_with(|| {
                let mut index: LPooled<AHashMap<&K, usize>> = LPooled::take();
                next.resize(keys.len(), None);
                for (j, key) in keys.iter().enumerate().rev() {
                    if items[j].is_some() {
                        next[j] = index.insert(key, j);
                    }
                }
                index
            });
            while let Some(j) = index.get(val).copied() {
                match next[j] {
                    Some(k) => {
                        index.insert(&keys[j], k);
                    }
                    None => {
                        index.remove(val);
                    }
                }
                if let Some(item) = items[j].take() {
                    return Some(item);
                }
            }
            None
        })
        .collect()
}

/// Old entries stay intact until every fresh item compiles. Fresh items
/// compile concurrently; equal values keep their existing item.
pub async fn reconcile<T, F: Future<Output = Result<T>>>(
    old: &mut Vec<(Value, T)>,
    mut items: LPooled<Vec<Value>>,
    mut compile: impl FnMut(Value) -> F,
) -> Result<()> {
    let mut kept =
        keep_equal(old.iter().enumerate().map(|(i, (v, _))| (v.clone(), i)), &items);
    let fresh = items
        .iter()
        .zip(kept.iter())
        .filter(|(_, t)| t.is_none())
        .map(|(v, _)| compile(v.clone()));
    let mut fresh = futures::future::try_join_all(fresh).await?.into_iter();
    let mut slots: LPooled<Vec<Option<T>>> =
        old.drain(..).map(|(_, t)| Some(t)).collect();
    old.extend(items.drain(..).zip(kept.drain(..)).map(|(v, at)| {
        let t = match at {
            Some(i) => slots[i].take().expect("each kept item is used once"),
            None => fresh.next().expect("each fresh item has a result"),
        };
        (v, t)
    }));
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test(flavor = "current_thread")]
    async fn reconciliation_preserves_old_entries_until_success() -> Result<()> {
        let mut old = vec![(Value::I64(1), "first"), (Value::I64(2), "second")];
        let items = || [2, 3, 1].map(Value::I64).into_iter().collect();
        assert!(
            reconcile(&mut old, items(), |_| async { anyhow::bail!("bad item") })
                .await
                .is_err()
        );
        assert_eq!(old, [(Value::I64(1), "first"), (Value::I64(2), "second")]);
        reconcile(&mut old, items(), |v| async move {
            assert_eq!(v, Value::I64(3), "retained entries must not compile again");
            Ok("fresh")
        })
        .await?;
        assert_eq!(
            old,
            [
                (Value::I64(2), "second"),
                (Value::I64(3), "fresh"),
                (Value::I64(1), "first")
            ]
        );
        Ok(())
    }

    #[tokio::test(flavor = "current_thread")]
    async fn reconciliation_compiles_fresh_entries_concurrently() -> Result<()> {
        use std::time::Duration;
        use tokio::sync::Barrier;
        use triomphe::Arc;

        let barrier = Arc::new(Barrier::new(3));
        let mut old = Vec::new();
        let items = [1, 2, 3].map(Value::I64).into_iter().collect();
        tokio::time::timeout(
            Duration::from_secs(5),
            reconcile(&mut old, items, |v| {
                let barrier = barrier.clone();
                async move {
                    barrier.wait().await;
                    Ok(v)
                }
            }),
        )
        .await??;
        assert_eq!(old.len(), 3);
        Ok(())
    }

    #[test]
    fn reordered_children_match_in_linear_comparisons() {
        use std::{
            cell::Cell,
            hash::{Hash, Hasher},
        };
        struct Key<'a>(usize, &'a Cell<usize>);
        impl PartialEq for Key<'_> {
            fn eq(&self, other: &Self) -> bool {
                self.1.set(self.1.get() + 1);
                self.0 == other.0
            }
        }
        impl Eq for Key<'_> {}
        impl Hash for Key<'_> {
            fn hash<H: Hasher>(&self, state: &mut H) {
                self.0.hash(state)
            }
        }
        let comparisons = Cell::new(0);
        let n = 1000;
        let new: poolshark::local::LPooled<Vec<_>> =
            (0..n).rev().map(|i| Key(i, &comparisons)).collect();
        let kept = keep_equal((0..n).map(|i| (Key(i, &comparisons), i)), &new);
        assert!(kept.iter().copied().eq((0..n).rev().map(Some)));
        assert!(comparisons.get() < 8 * n, "{} comparisons", comparisons.get());
        let kept = keep_equal([(1, 'a'), (1, 'b'), (2, 'c')], &[1, 2, 1, 1]);
        assert_eq!(&**kept, &[Some('a'), Some('c'), Some('b'), None]);
        let kept = keep_equal([(1, 'a'), (1, 'b'), (2, 'c')], &[2, 1, 1]);
        assert_eq!(&**kept, &[Some('c'), Some('b'), Some('a')]);
    }
}
