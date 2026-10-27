use crate::tree::Database;
use futures::channel::mpsc;
use parking_lot::{Mutex, RwLock};
use poolshark::local::LPooled;
use sled::{
    IVec,
    transaction::{
        ConflictableTransactionResult, TransactionResult, Transactional,
        TransactionalTree,
    },
};
use smallvec::SmallVec;
use std::{cell::RefCell, ptr, sync::Arc};

/// A change a write made to a tree: the key and the value it holds now,
/// `None` once removed.
#[derive(Debug, Clone)]
pub(crate) struct Change {
    pub(crate) key: IVec,
    pub(crate) value: Option<IVec>,
}

#[derive(Debug, Clone)]
struct Watch {
    prefix: Box<[u8]>,
    tx: mpsc::UnboundedSender<Change>,
}

/// The watches on one tree. A write holds `order` shared while the tree
/// has no watch, else exclusive until what it changed is sent, so each
/// watch receives the tree's changes in the order they were made. The
/// list is replaced whole under its own lock, so registering or dropping
/// a watch never waits for a write.
#[derive(Debug, Default)]
pub(crate) struct Watches {
    order: RwLock<()>,
    list: Mutex<Arc<[Watch]>>,
}

impl Watches {
    fn list(&self) -> Arc<[Watch]> {
        self.list.lock().clone()
    }

    fn watched(&self) -> bool {
        !self.list.lock().is_empty()
    }

    /// Send each change to a key under `prefix` to `tx`, until the
    /// registration drops.
    pub(crate) fn add(
        self: &Arc<Self>,
        prefix: &[u8],
        tx: mpsc::UnboundedSender<Change>,
    ) -> Registered {
        let mut list = self.list.lock();
        let watch = Watch { prefix: prefix.into(), tx: tx.clone() };
        *list = list.iter().cloned().chain([watch]).collect();
        Registered { watches: self.clone(), tx }
    }

    /// End every watch: each receiver sees its channel close after the
    /// changes already sent.
    // CR claude for claude: [bug/medium] Nothing records that a list has ended, so
    // `add` after `end` registers a watch that is never closed. This happens when a
    // subscription asleep during db::drop_tree re-attaches at its wake
    // (DbSubscribe::watch from the Quiet branch), and when a subscription is made from
    // a tree handle taken before the drop. The accessors then never fire, though
    // mod.gxi promises one DbErr. Also, `end` does not take `order`, so a watched write
    // applied before the drop but not yet sent loses its change. Fix: hold the list as
    // Option<Arc<[Watch]>>, set to None here, and have `add` close `tx` at once on an
    // ended list. Take `self.order.write()` before closing (lock order creating ->
    // order is kept). probe: design/review-2026-10-26/repro/db2-sub-sleep-drop.gx
    // (db2-02)
    pub(crate) fn end(&self) {
        let mut list = self.list.lock();
        for w in list.iter() {
            w.tx.close_channel();
        }
        *list = Arc::new([]);
    }

    #[cfg(test)]
    pub(crate) fn is_empty(&self) -> bool {
        !self.watched()
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
        let mut list = self.watches.list.lock();
        *list = list.iter().filter(|w| !w.tx.same_receiver(&self.tx)).cloned().collect();
    }
}

/// What a write changed on each of its trees.
pub(crate) struct Changes {
    changes: LPooled<Vec<(usize, Change)>>,
    exact: bool,
}

impl Changes {
    /// Whether the changes must be exact: a write that must read to learn
    /// whether a write changes its key reads only then.
    pub(crate) fn wanted(&self) -> bool {
        self.exact
    }

    /// The write set `key` of its `tree`th tree from `old` to `new`.
    pub(crate) fn record(
        &mut self,
        tree: usize,
        key: &[u8],
        old: Option<&[u8]>,
        new: Option<&[u8]>,
    ) {
        if old != new {
            self.touched(tree, key, new)
        }
    }

    /// The write set `key` of its `tree`th tree to `new`, whatever it held.
    pub(crate) fn touched(&mut self, tree: usize, key: &[u8], new: Option<&[u8]>) {
        self.changes.push((tree, Change { key: key.into(), value: new.map(IVec::from) }));
    }

    /// Forget what was recorded: the write it describes did not happen.
    pub(crate) fn clear(&mut self) {
        self.changes.clear()
    }
}

/// Held by every sender a write found unwatched when it began.
static LATE: Mutex<()> = Mutex::new(());

/// Write to `trees` with `write`, which records what it changed (tree `i`
/// is `trees[i]`); each change is sent to the watches whose prefix its key
/// has before the trees are released. A watch registered while a write
/// that found none ran gets each key it changed as the tree holds it
/// after, sent under [`LATE`], so the last value a watch receives for a
/// key is the key's.
pub(crate) fn write<T>(
    trees: &[(&Watches, &sled::Tree)],
    write: impl FnOnce(&mut Changes) -> T,
) -> T {
    let mut order: SmallVec<[&Watches; 4]> = trees.iter().map(|(w, _)| *w).collect();
    order.sort_unstable_by_key(|w| *w as *const Watches);
    order.dedup_by(|a, b| ptr::eq(*a, *b));
    let exact = order.iter().any(|w| w.watched());
    let mut changes = Changes { changes: LPooled::take(), exact };
    if !exact {
        let _shared: SmallVec<[_; 4]> = order.iter().map(|w| w.order.read()).collect();
        let res = write(&mut changes);
        if order.iter().any(|w| w.watched()) {
            let _late = LATE.lock();
            send(trees, &mut changes, true);
        }
        return res;
    }
    let _exclusive: SmallVec<[_; 4]> = order.iter().map(|w| w.order.write()).collect();
    let res = write(&mut changes);
    send(trees, &mut changes, false);
    res
}

/// One sled transaction over `trees`, the first `watched` of them
/// watched (tree `i` of the changes is `trees[i]`). Each attempt records
/// what it changes; the watches receive exactly what the attempt that
/// applied changed, and nothing when none did.
pub(crate) fn transaction<T, E>(
    db: &Database,
    trees: &[sled::Tree],
    watched: usize,
    attempt: impl Fn(
        &[TransactionalTree],
        &mut Changes,
    ) -> ConflictableTransactionResult<T, E>,
) -> TransactionResult<T, E> {
    let watches: LPooled<Vec<Arc<Watches>>> =
        trees[..watched].iter().map(|t| db.watches(t)).collect();
    let locked: SmallVec<[_; 4]> =
        watches.iter().map(|w| &**w).zip(trees.iter()).collect();
    write(&locked, |changes| {
        let changes = RefCell::new(changes);
        let res = trees.transaction(|tx| {
            let mut changes = changes.borrow_mut();
            changes.clear();
            attempt(tx, &mut changes)
        });
        if res.is_err() {
            changes.borrow_mut().clear();
        }
        res
    })
}

/// Send each change to its tree's watches under its key's prefix, a
/// `late` one with the value its key holds now.
fn send(trees: &[(&Watches, &sled::Tree)], changes: &mut Changes, late: bool) {
    let lists: SmallVec<[_; 4]> = trees.iter().map(|(w, _)| w.list()).collect();
    for (tree, mut change) in changes.changes.drain(..) {
        let mut watches =
            lists[tree].iter().filter(|w| change.key.starts_with(&w.prefix)).peekable();
        if watches.peek().is_none() {
            continue;
        }
        if late && let Ok(now) = trees[tree].1.get(&change.key) {
            change.value = now
        }
        for w in watches {
            let _ = w.tx.unbounded_send(change.clone());
        }
    }
}
