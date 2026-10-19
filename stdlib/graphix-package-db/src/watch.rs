use anyhow::Result;
use futures::channel::mpsc;
use parking_lot::RwLock;
use poolshark::local::LPooled;
use sled::IVec;
use smallvec::SmallVec;
use std::{ptr, sync::Arc};

/// A change a write made to a tree: the key and the value it holds now,
/// `None` once removed.
#[derive(Debug, Clone)]
pub(crate) struct Change {
    pub(crate) key: IVec,
    pub(crate) value: Option<IVec>,
}

#[derive(Debug)]
struct Watch {
    prefix: Box<[u8]>,
    tx: mpsc::UnboundedSender<Change>,
}

/// The watches on one tree. Every write to the tree holds its lock:
/// shared while the tree has no watch, else exclusive until what it
/// changed is sent, so each watch receives the tree's changes in the
/// order they were made.
#[derive(Debug, Default)]
pub(crate) struct Watches(RwLock<Vec<Watch>>);

impl Watches {
    /// Send each change to a key under `prefix` to `tx`, until the
    /// registration drops.
    pub(crate) fn add(
        self: &Arc<Self>,
        prefix: &[u8],
        tx: mpsc::UnboundedSender<Change>,
    ) -> Registered {
        // CR claude for eric: [perf/medium] This exclusive lock is taken on the runtime
        // thread (DbSubscribe::update -> spawn_watch). It waits for every write in
        // flight on the tree, and watch::write holds the lock for the whole sled
        // operation: an apply_batch, a commit's transaction, an import. A subscription
        // made during a 100k-entry db::batch stopped a 5 ms ticker for 409 ms, the
        // whole batch. Registered::drop blocks a tokio worker the same way. The fix is
        // a choice. Register without blocking: keep the list behind its own short
        // mutex, and have writers that saw no watch record their changes anyway. Or
        // register on a blocking thread and fire the subscription once registered,
        // giving up "registered in the cycle the subscription fires". probe:
        // design/review-2026-10-19/repro/db2-subscribe-blocks-runtime.gx (db2-01)
        self.0.write().push(Watch { prefix: prefix.into(), tx: tx.clone() });
        Registered { watches: self.clone(), tx }
    }

    #[cfg(test)]
    pub(crate) fn is_empty(&self) -> bool {
        self.0.read().is_empty()
    }
}

/// A watch's registration: dropping it ends the watch.
#[derive(Debug)]
pub(crate) struct Registered {
    watches: Arc<Watches>,
    tx: mpsc::UnboundedSender<Change>,
}

impl Drop for Registered {
    fn drop(&mut self) {
        self.watches.0.write().retain(|w| !w.tx.same_receiver(&self.tx))
    }
}

/// What a write changed on each of its trees, kept only while one of them
/// has a watch.
pub(crate) struct Changes(Option<LPooled<Vec<(usize, Change)>>>);

impl Changes {
    /// Whether the changes are kept: a write that must read to learn what
    /// it changes reads only then.
    pub(crate) fn wanted(&self) -> bool {
        self.0.is_some()
    }

    /// The write set `key` of its `tree`th tree from `old` to `new`.
    pub(crate) fn record(
        &mut self,
        tree: usize,
        key: &[u8],
        old: Option<&[u8]>,
        new: Option<&[u8]>,
    ) {
        if let Some(changes) = &mut self.0
            && old != new
        {
            changes.push((tree, Change { key: key.into(), value: new.map(IVec::from) }));
        }
    }

    /// Forget what was recorded: the write it describes did not happen.
    pub(crate) fn clear(&mut self) {
        if let Some(changes) = &mut self.0 {
            changes.clear()
        }
    }
}

/// Write to `trees` with `write`, which records what it changed (tree `i`
/// is `trees[i]`); each change is sent to the watches whose prefix its key
/// has before the trees are released.
pub(crate) fn write<T>(
    trees: &[&Watches],
    write: impl FnOnce(&mut Changes) -> Result<T>,
) -> Result<T> {
    let mut order: SmallVec<[&Watches; 4]> = trees.iter().copied().collect();
    order.sort_unstable_by_key(|w| *w as *const Watches);
    order.dedup_by(|a, b| ptr::eq(*a, *b));
    {
        let shared: SmallVec<[_; 4]> = order.iter().map(|w| w.0.read()).collect();
        if shared.iter().all(|w| w.is_empty()) {
            return write(&mut Changes(None));
        }
    }
    let exclusive: SmallVec<[_; 4]> = order.iter().map(|w| (*w, w.0.write())).collect();
    let mut changes = Changes(Some(LPooled::take()));
    let res = write(&mut changes);
    for (tree, change) in changes.0.iter_mut().flat_map(|c| c.drain(..)) {
        let (_, watches) = exclusive
            .iter()
            .find(|(w, _)| ptr::eq(*w, trees[tree]))
            .expect("every tree written is locked");
        for w in watches.iter().filter(|w| change.key.starts_with(&w.prefix)) {
            let _ = w.tx.unbounded_send(change.clone());
        }
    }
    res
}
