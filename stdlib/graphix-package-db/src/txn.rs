use crate::{
    encoding::for_batch_ops,
    tree::{
        Db, KeyTyp, META_TREE, MetaStore, OpenTreeEv, TreeOpener, TreeTypes,
        abstract_arg, blocking, get_db, insert_arg, key_arg, meta_key, value_or_null,
    },
};
use ahash::AHashMap;
use anyhow::{Result, anyhow, bail};
use arcstr::{ArcStr, literal};
use graphix_package_core::{CachedArgsAsync, CachedVals, EvalCachedAsync};
use netidx::publisher::Typ;
use netidx_value::Value;
use parking_lot::Mutex;
use poolshark::global::GPooled;
use sled::{
    IVec,
    transaction::{
        ConflictableTransactionError, TransactionError, Transactional, TransactionalTree,
    },
};
use std::{collections::hash_map::Entry, fmt, sync::Arc};

/// One tree's part of a transaction: each key it read, with the value it
/// saw, and each write it will make (`None` removes).
struct TreeTxn {
    tree: sled::Tree,
    read: AHashMap<IVec, Option<IVec>>,
    write: AHashMap<IVec, Option<IVec>>,
}

impl TreeTxn {
    /// The key's value as this transaction sees it: its own write, else
    /// what it read before, else the tree's, which is recorded.
    fn current(&mut self, key: &IVec) -> Result<Option<IVec>> {
        if let Some(v) = self.write.get(key).or_else(|| self.read.get(key)) {
            return Ok(v.clone());
        }
        let v = self.tree.get(key)?;
        self.read.insert(key.clone(), v.clone());
        Ok(v)
    }

    fn get(&mut self, key: &[u8]) -> Result<Value> {
        value_or_null(self.current(&IVec::from(key))?)
    }

    /// Buffer a write and answer the value it replaces.
    fn put(&mut self, key: &[u8], value: Option<IVec>) -> Result<Value> {
        let key = IVec::from(key);
        let prev = self.current(&key)?;
        self.write.insert(key, value);
        value_or_null(prev)
    }
}

/// Why a commit's sled transaction aborted.
enum Refused {
    Conflict,
    Meta(anyhow::Error),
}

/// A transaction: reads go to the trees as they stand and writes are
/// buffered, so nothing is held between the program's operations;
/// `commit` checks every read and applies every write in one sled
/// transaction.
struct TxnState {
    trees: Vec<TreeTxn>,
    /// The slot of each tree opened, by meta key: a tree opened twice is
    /// one slot, so both handles see one transaction.
    opened: AHashMap<ArcStr, usize>,
    pending_meta: AHashMap<ArcStr, TreeTypes>,
    /// Declared last: the trees drop while it holds the database, which
    /// it drops on a blocking thread.
    db: Db,
}

impl TxnState {
    fn open_tree(&mut self, name: Option<ArcStr>, types: TreeTypes) -> Result<usize> {
        types.concrete()?;
        let key = ArcStr::from(meta_key(name.as_ref())?);
        if !self.db.open_tree(META_TREE)?.check(&key, &types)? {
            match self.pending_meta.entry(key.clone()) {
                Entry::Vacant(e) => {
                    e.insert(types);
                }
                Entry::Occupied(e) if *e.get() != types => {
                    bail!("conflicting types for tree '{key}' within transaction")
                }
                Entry::Occupied(_) => (),
            }
        }
        if let Some(idx) = self.opened.get(&key) {
            return Ok(*idx);
        }
        let tree = match &name {
            None => (***self.db).clone(),
            // CR claude for claude: [bug/low] Opening a tree in a transaction creates
            // it on disk immediately, while its type metadata waits in pending_meta
            // until commit. A rollback, a Conflict or a dropped transaction therefore
            // leaves a tree that db::tree_names lists and db::get_type answers null
            // for, so the transaction is not all-or-nothing. probe:
            // design/review-2026-10-12/repro/db2-10.gx (tree_names gives ["fresh"] and
            // get_type gives null after rollback). Fix: when the tree does not exist
            // yet, defer creating it to commit (create it just before the sled
            // transaction, together with its meta entry), or drop it on
            // rollback/conflict when this transaction created it. (db2-10)
            Some(n) => self.db.open_tree(n.as_bytes())?,
        };
        let idx = self.trees.len();
        self.trees.push(TreeTxn { tree, read: AHashMap::new(), write: AHashMap::new() });
        self.opened.insert(key, idx);
        Ok(idx)
    }

    fn tree(&mut self, idx: usize) -> Result<&mut TreeTxn> {
        self.trees.get_mut(idx).ok_or_else(|| anyhow!("invalid tree index"))
    }

    /// Every key read still holds the value read, the new trees' types are
    /// stored and the writes made, or nothing is.
    fn check_and_apply(
        &self,
        tx: &[TransactionalTree],
    ) -> Result<(), ConflictableTransactionError<Refused>> {
        let abort = |r| Err(ConflictableTransactionError::Abort(r));
        for (t, tx) in self.trees.iter().zip(tx) {
            for (k, v) in &t.read {
                if tx.get(k)? != *v {
                    return abort(Refused::Conflict);
                }
            }
        }
        if let Some(meta) = tx.get(self.trees.len()) {
            for (name, types) in &self.pending_meta {
                if let Err(e) = meta.check_or_store(name, types) {
                    return abort(Refused::Meta(e));
                }
            }
        }
        for (t, tx) in self.trees.iter().zip(tx) {
            for (k, v) in &t.write {
                match v {
                    Some(v) => tx.insert(k.clone(), v.clone())?,
                    None => tx.remove(k.clone())?,
                };
            }
        }
        Ok(())
    }

    fn commit(self) -> Result<Value> {
        let mut trees: Vec<sled::Tree> =
            self.trees.iter().map(|t| t.tree.clone()).collect();
        if !self.pending_meta.is_empty() {
            trees.push(self.db.open_tree(META_TREE)?);
        }
        if trees.is_empty() {
            return Ok(Value::Null);
        }
        match (&trees[..]).transaction(|tx| self.check_and_apply(tx)) {
            Ok(()) => {
                // CR claude for eric: [bug/low] commit applies the transaction and then
                // flushes. If the flush fails (disk full, an I/O error), the program
                // gets a DbErr although every write is already applied and visible.
                // That contradicts the gxi's "all writes become visible atomically, or
                // none do", and a program that retries a read-modify-write on that
                // DbErr applies it twice. commit is also the only write in the package
                // that flushes on its own, and the gxi does not say so. Pick one
                // contract: either commit is not durable (drop the flush; the program
                // calls db::flush as it does after every other write), or commit is
                // durable and a flush failure after the apply comes back as its own
                // outcome, documented as "committed, not yet durable". (db2-07)
                self.db.flush()?;
                Ok(Value::Null)
            }
            Err(TransactionError::Abort(Refused::Conflict)) => {
                Ok(Value::Error(Value::String(literal!("Conflict")).into()))
            }
            Err(TransactionError::Abort(Refused::Meta(e))) => Err(e),
            Err(TransactionError::Storage(e)) => Err(e.into()),
        }
    }
}

pub(crate) struct TxnInner {
    /// `None` once committed or rolled back.
    state: Mutex<Option<TxnState>>,
}

impl TxnInner {
    fn with_state<T>(&self, f: impl FnOnce(&mut TxnState) -> Result<T>) -> Result<T> {
        match &mut *self.state.lock() {
            Some(s) => f(s),
            None => bail!("the transaction has ended"),
        }
    }

    fn end(&self) -> Result<TxnState> {
        self.state.lock().take().ok_or_else(|| anyhow!("the transaction has ended"))
    }
}

impl fmt::Debug for TxnInner {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("TxnInner").finish()
    }
}

#[derive(Debug, Clone)]
struct TxnValue {
    inner: Arc<TxnInner>,
}

graphix_package_core::impl_abstract_arc!(
    TxnValue,
    static TXN_WRAPPER = "db::txn::Txn"
);

fn get_txn(cached: &CachedVals) -> Option<Arc<TxnInner>> {
    abstract_arg::<TxnValue>(cached, 0).map(|t| t.inner.clone())
}

pub(crate) struct TxnTreeInner {
    txn: Arc<TxnInner>,
    tree_idx: usize,
    key_typ: Option<Typ>,
}

impl fmt::Debug for TxnTreeInner {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("TxnTreeInner").field("tree_idx", &self.tree_idx).finish()
    }
}

impl KeyTyp for &Arc<TxnTreeInner> {
    fn key_typ(&self) -> Option<Typ> {
        self.key_typ
    }
}

#[derive(Debug, Clone)]
struct TxnTreeValue {
    inner: Arc<TxnTreeInner>,
}

graphix_package_core::impl_abstract_arc!(
    TxnTreeValue,
    static TXN_TREE_WRAPPER = "db::txn::TxnTree"
);

type Writes = Vec<(IVec, Option<IVec>)>;

fn writes_arg(tree: impl KeyTyp, cached: &CachedVals) -> Option<Result<Writes>> {
    match cached.0.get(1)?.as_ref()? {
        Value::Array(a) => {
            let mut writes = Vec::with_capacity(a.len());
            let res = for_batch_ops(tree.key_typ(), a, |k, v| {
                writes.push((IVec::from(k), v.map(IVec::from)))
            });
            Some(res.map(|()| writes))
        }
        v => Some(Err(anyhow!("not a batch: {v}"))),
    }
}

#[derive(Debug, Default)]
pub(crate) struct DbTxnBeginEv;

impl EvalCachedAsync for DbTxnBeginEv {
    type Args = Db;

    const NAME: &str = "db_txn_begin";

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        get_db(cached, 0)
    }

    fn eval(db: Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            let state = TxnState {
                trees: Vec::new(),
                opened: AHashMap::new(),
                pending_meta: AHashMap::new(),
                db,
            };
            TXN_WRAPPER.wrap(TxnValue {
                inner: Arc::new(TxnInner { state: Mutex::new(Some(state)) }),
            })
        }
    }
}

pub(crate) type DbTxnBegin = CachedArgsAsync<DbTxnBeginEv>;

#[derive(Debug)]
pub(crate) struct FromTxn;

impl TreeOpener for FromTxn {
    const NAME: &str = "db_txn_tree";
    type Handle = Arc<TxnInner>;

    fn handle(cached: &CachedVals) -> Option<Arc<TxnInner>> {
        get_txn(cached)
    }

    fn open(
        txn: Arc<TxnInner>,
        name: Option<ArcStr>,
        types: TreeTypes,
    ) -> impl Future<Output = Value> + Send {
        blocking(move || {
            let key_typ = types.key;
            let tree_idx = txn.with_state(|s| s.open_tree(name, types))?;
            Ok(TXN_TREE_WRAPPER.wrap(TxnTreeValue {
                inner: Arc::new(TxnTreeInner { txn, tree_idx, key_typ }),
            }))
        })
    }
}

pub(crate) type DbTxnTree = CachedArgsAsync<OpenTreeEv<FromTxn>>;

/// A builtin applying one operation to a transaction's tree, built from
/// the encoding of its arguments; a failed encoding is its DbErr.
macro_rules! txn_op {
    ($ev:ident, $alias:ident, $name:literal, $enc:expr, $arg:ty,
     |$t:ident, $a:pat_param| $body:expr) => {
        #[derive(Debug, Default)]
        pub(crate) struct $ev;

        impl EvalCachedAsync for $ev {
            type Args = (Arc<TxnTreeInner>, Result<$arg>);

            const NAME: &str = $name;

            fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
                let tt = abstract_arg::<TxnTreeValue>(cached, 0)?.inner.clone();
                let arg = $enc(&tt, cached)?;
                Some((tt, arg))
            }

            fn eval((tt, arg): Self::Args) -> impl Future<Output = Value> + Send {
                blocking(move || {
                    let $a = arg?;
                    tt.txn.with_state(|s| {
                        let $t = s.tree(tt.tree_idx)?;
                        $body
                    })
                })
            }
        }

        graphix_package_core::unit_image_state!($ev);
        pub(crate) type $alias = CachedArgsAsync<$ev>;
    };
}

txn_op!(DbTxnGetEv, DbTxnGet, "db_txn_get", key_arg, GPooled<Vec<u8>>, |t, key| t
    .get(&key));
txn_op!(
    DbTxnRemoveEv,
    DbTxnRemove,
    "db_txn_remove",
    key_arg,
    GPooled<Vec<u8>>,
    |t, key| t.put(&key, None)
);
txn_op!(
    DbTxnInsertEv,
    DbTxnInsert,
    "db_txn_insert",
    insert_arg,
    (GPooled<Vec<u8>>, GPooled<Vec<u8>>),
    |t, (key, value)| t.put(&key, Some(IVec::from(value.as_slice())))
);
txn_op!(DbTxnBatchEv, DbTxnBatch, "db_txn_batch", writes_arg, Writes, |t, writes| {
    t.write.extend(writes);
    Ok(Value::Null)
});

#[derive(Debug, Default)]
pub(crate) struct DbTxnCommitEv;

impl EvalCachedAsync for DbTxnCommitEv {
    type Args = Arc<TxnInner>;

    const NAME: &str = "db_txn_commit";

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        get_txn(cached)
    }

    fn eval(txn: Self::Args) -> impl Future<Output = Value> + Send {
        blocking(move || txn.end()?.commit())
    }
}

pub(crate) type DbTxnCommit = CachedArgsAsync<DbTxnCommitEv>;

#[derive(Debug, Default)]
pub(crate) struct DbTxnRollbackEv;

impl EvalCachedAsync for DbTxnRollbackEv {
    type Args = Arc<TxnInner>;

    const NAME: &str = "db_txn_rollback";

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        get_txn(cached)
    }

    fn eval(txn: Self::Args) -> impl Future<Output = Value> + Send {
        blocking(move || txn.end().map(|_| Value::Null))
    }
}

pub(crate) type DbTxnRollback = CachedArgsAsync<DbTxnRollbackEv>;

graphix_package_core::unit_image_state!(DbTxnBeginEv, DbTxnCommitEv, DbTxnRollbackEv);
