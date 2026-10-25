use crate::{
    tree::{
        Db, KeyTyp, META_TREE, MetaStore, OpenTreeEv, TreeOpener, TreeTypes, blocking,
        blocking_op, get_db, insert_arg, key_arg, meta_key, raw_args, value_or_null,
        writes_arg,
    },
    watch::{self, Changes, Watches},
};
use ahash::AHashMap;
use anyhow::{Result, anyhow, bail};
use arcstr::{ArcStr, literal};
use compact_str::format_compact;
use graphix_compiler::rep;
use graphix_package_core::{CachedArgsAsync, CachedVals, EvalCachedAsync};
use netidx::publisher::Typ;
use netidx_value::Value;
use parking_lot::Mutex;
use poolshark::local::LPooled;
use sled::{
    IVec,
    transaction::{
        ConflictableTransactionError, TransactionError, Transactional, TransactionalTree,
    },
};
use smallvec::SmallVec;
use std::{cell::RefCell, collections::hash_map::Entry, fmt, sync::Arc};

/// The tree a transaction's part is over: one that stood when it was
/// opened, or a name the commit creates, empty until then.
enum Target {
    Tree(sled::Tree),
    New(ArcStr),
}

/// One tree's part of a transaction: each key it read, with the value it
/// saw, and each write it will make (`None` removes).
struct TreeTxn {
    target: Target,
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
        let v = match &self.target {
            Target::Tree(tree) => tree.get(key)?,
            Target::New(_) => None,
        };
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
        let target = match name {
            None => Target::Tree((****self.db).clone()),
            Some(n) if self.db.tree_names().iter().any(|t| *t == n.as_bytes()) => {
                Target::Tree(self.db.open_tree(n.as_bytes())?)
            }
            Some(n) => Target::New(n),
        };
        let idx = self.trees.len();
        self.trees.push(TreeTxn {
            target,
            read: AHashMap::new(),
            write: AHashMap::new(),
        });
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
        changes: &RefCell<&mut Changes>,
    ) -> Result<(), ConflictableTransactionError<Refused>> {
        changes.borrow_mut().clear();
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
        for (i, (t, tx)) in self.trees.iter().zip(tx).enumerate() {
            for (k, v) in &t.write {
                let old = match v {
                    Some(v) => tx.insert(k.clone(), v.clone())?,
                    None => tx.remove(k.clone())?,
                };
                changes.borrow_mut().record(i, k, old.as_deref(), v.as_deref());
            }
        }
        Ok(())
    }

    /// Apply the transaction. A tree it opened that did not stand is
    /// created here, and dropped again when the commit fails and nothing
    /// claimed it meanwhile.
    fn commit(self) -> Result<Value> {
        let _creating = self.db.creating.lock();
        let mut created: LPooled<Vec<ArcStr>> = LPooled::take();
        let res = self.apply(&mut created);
        if !matches!(res, Ok(Value::Null)) {
            self.drop_unclaimed(&created)?;
        }
        res
    }

    /// Drop each of `created` that is still empty and has no types: a
    /// program that opened it outside the transaction stored its types.
    fn drop_unclaimed(&self, created: &[ArcStr]) -> Result<()> {
        let meta = self.db.open_tree(META_TREE)?;
        for name in created {
            if self.db.open_tree(name.as_bytes())?.is_empty()
                && meta.get(name.as_bytes())?.is_none()
            {
                self.db.drop_tree(name.as_bytes())?;
            }
        }
        Ok(())
    }

    fn apply(&self, created: &mut Vec<ArcStr>) -> Result<Value> {
        let mut trees: Vec<sled::Tree> = Vec::with_capacity(self.trees.len() + 1);
        for t in &self.trees {
            trees.push(match &t.target {
                Target::Tree(tree) => tree.clone(),
                Target::New(name) => {
                    if !self.db.tree_names().iter().any(|t| *t == name.as_bytes()) {
                        created.push(name.clone());
                    }
                    self.db.open_tree(name.as_bytes())?
                }
            });
        }
        if !self.pending_meta.is_empty() {
            trees.push(self.db.open_tree(META_TREE)?);
        }
        if trees.is_empty() {
            return Ok(Value::Null);
        }
        let watches: LPooled<Vec<Arc<Watches>>> =
            trees[..self.trees.len()].iter().map(|t| self.db.watches(t)).collect();
        let locked: SmallVec<[_; 4]> =
            watches.iter().map(|w| &**w).zip(trees.iter()).collect();
        let res = watch::write(&locked, |changes| {
            let changes = RefCell::new(changes);
            let res = (&trees[..]).transaction(|tx| self.check_and_apply(tx, &changes));
            if res.is_err() {
                changes.borrow_mut().clear();
            }
            Ok(res)
        })?;
        match res {
            Ok(()) => {
                // applied and visible either way: a failed flush is its own
                // outcome, never a DbErr a retry would apply again
                match self.db.flush() {
                    Ok(_) => Ok(Value::Null),
                    Err(e) => Ok(Value::Error(
                        rep::make_variant(
                            literal!("NotDurable"),
                            [Value::String(format_compact!("{e}").as_str().into())],
                        )
                        .into(),
                    )),
                }
            }
            Err(TransactionError::Abort(Refused::Conflict)) => {
                Ok(Value::Error(rep::make_variant(literal!("Conflict"), []).into()))
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
    cached.abstract_arg::<TxnValue>(0).map(|t| t.inner.clone())
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
    ($ev:ident, $alias:ident, $name:literal, $enc:expr, $n:literal,
     |$t:ident, $a:pat_param| $body:expr) => {
        blocking_op!(
            $ev,
            $alias,
            $name,
            (Arc<TxnTreeInner>, [Value; $n]),
            |cached| {
                let tt = cached.abstract_arg::<TxnTreeValue>(0)?.inner.clone();
                Some((tt, raw_args(cached)?))
            },
            |(tt, args)| {
                let $a = $enc(&tt, args)?;
                tt.txn.with_state(|s| {
                    let $t = s.tree(tt.tree_idx)?;
                    $body
                })
            }
        );
    };
}

txn_op!(DbTxnGetEv, DbTxnGet, "db_txn_get", key_arg, 1, |t, key| t.get(&key));
txn_op!(DbTxnRemoveEv, DbTxnRemove, "db_txn_remove", key_arg, 1, |t, key| t
    .put(&key, None));
txn_op!(DbTxnInsertEv, DbTxnInsert, "db_txn_insert", insert_arg, 2, |t, (key, value)| t
    .put(&key, Some(IVec::from(value.as_slice()))));
txn_op!(DbTxnBatchEv, DbTxnBatch, "db_txn_batch", writes_arg, 1, |t, mut writes| {
    t.write.extend(writes.drain(..));
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

#[cfg(test)]
mod test {
    use super::*;
    use crate::tree::{open_db, test::cached};
    use poolshark::global::GPooled;
    use std::thread::{self, ThreadId};

    static ENCODED_ON: Mutex<Option<ThreadId>> = Mutex::new(None);

    fn probe_key_arg(
        tree: &Arc<TxnTreeInner>,
        args: [Value; 1],
    ) -> Result<GPooled<Vec<u8>>> {
        *ENCODED_ON.lock() = Some(thread::current().id());
        key_arg(tree, args)
    }

    txn_op!(
        EncodingProbeEv,
        EncodingProbe,
        "db_txn_encoding_probe",
        probe_key_arg,
        1,
        |tree, key| tree.get(&key)
    );

    #[tokio::test]
    async fn txn_encoding_runs_on_a_blocking_thread() -> Result<()> {
        let dir = tempfile::tempdir()?;
        let db = open_db(dir.path().to_str().unwrap().into()).await?;
        let tree = TreeTxn {
            target: Target::Tree(db.open_tree("t")?),
            read: AHashMap::new(),
            write: AHashMap::new(),
        };
        let txn = Arc::new(TxnInner {
            state: Mutex::new(Some(TxnState {
                trees: vec![tree],
                opened: AHashMap::new(),
                pending_meta: AHashMap::new(),
                db,
            })),
        });
        let tree = TXN_TREE_WRAPPER.wrap(TxnTreeValue {
            inner: Arc::new(TxnTreeInner {
                txn,
                tree_idx: 0,
                key_typ: Some(Typ::String),
            }),
        });
        let args =
            EncodingProbeEv.prepare_args(&cached([tree, Value::from("key")])).unwrap();
        assert!(ENCODED_ON.lock().is_none(), "prepare_args must not encode");
        assert_eq!(EncodingProbeEv::eval(args).await, Value::Null);
        assert_ne!(*ENCODED_ON.lock(), Some(thread::current().id()));
        assert!(ENCODED_ON.lock().is_some());
        let _: Option<EncodingProbe> = None;
        Ok(())
    }

    /// A commit sends its changes to the trees' watches; one that fails
    /// sends nothing.
    #[tokio::test]
    async fn commit_reaches_watches() -> Result<()> {
        let dir = tempfile::tempdir()?;
        let db = open_db(dir.path().to_str().unwrap().into()).await?;
        let tree = db.open_tree("t")?;
        let (tx, mut heard) = futures::channel::mpsc::unbounded();
        let _watch = db.watches(&tree).add(b"k", tx);
        let types = TreeTypes::string_to_string();
        let enc = |v: &str| {
            crate::encoding::encode_value(&Value::String(ArcStr::from(v))).unwrap()
        };
        let commit = |write: &str, read: bool| -> Result<Value> {
            let write = IVec::from(enc(write).as_slice());
            let mut state = TxnState {
                trees: Vec::new(),
                opened: AHashMap::new(),
                pending_meta: AHashMap::new(),
                db: db.clone(),
            };
            let t = state.open_tree(Some("t".into()), types.clone())?;
            if read {
                state.tree(t)?.get(b"k1")?;
                tree.insert(b"k1", enc("moved").as_slice())?;
            }
            state.tree(t)?.put(b"k1", Some(write.clone()))?;
            state.tree(t)?.put(b"other", Some(write))?;
            state.commit()
        };
        assert_eq!(commit("v", false)?, Value::Null);
        let change = heard.try_recv()?;
        assert_eq!(&*change.key, b"k1");
        assert_eq!(change.value.as_deref(), Some(enc("v").as_slice()));
        assert!(heard.try_recv().is_err(), "a key outside the prefix was heard");
        assert!(matches!(commit("w", true)?, Value::Error(_)), "expected Conflict");
        assert!(heard.try_recv().is_err(), "a failed commit was heard");
        Ok(())
    }

    /// A commit that fails drops the tree it created, and a later open
    /// starts it empty with its own types.
    #[tokio::test]
    async fn failed_commit_drops_its_new_tree() -> Result<()> {
        let dir = tempfile::tempdir()?;
        let db = open_db(dir.path().to_str().unwrap().into()).await?;
        let types = TreeTypes::string_to_string();
        let old = db.open_tree("old")?;
        let mut state = TxnState {
            trees: Vec::new(),
            opened: AHashMap::new(),
            pending_meta: AHashMap::new(),
            db: db.clone(),
        };
        let o = state.open_tree(Some("old".into()), types.clone())?;
        let f = state.open_tree(Some("fresh".into()), types)?;
        assert!(!db.tree_names().iter().any(|n| &**n == b"fresh"));
        state.tree(o)?.get(b"k")?;
        state.tree(f)?.put(b"k", Some(IVec::from(b"v")))?;
        old.insert(b"k", b"changed")?;
        let conflict = state.commit()?;
        assert!(matches!(conflict, Value::Error(_)), "expected Conflict: {conflict:?}");
        assert!(!db.tree_names().iter().any(|n| &**n == b"fresh"));
        Ok(())
    }
}
