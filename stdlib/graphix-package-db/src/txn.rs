use crate::{
    encoding::decode_value,
    tree::{
        Db, KeyTyp, META_TREE, MetaStore, OpenTreeEv, TreeOpener, TreeTypes,
        abstract_arg, batch_arg, get_db, insert_arg, key_arg, meta_key, value_or_null,
    },
};
use ahash::AHashMap;
use anyhow::{Result, anyhow, bail};
use arcstr::ArcStr;
use graphix_compiler::errf;
use graphix_package_core::{CachedArgsAsync, CachedVals, EvalCachedAsync};
use netidx::publisher::Typ;
use netidx_value::Value;
use poolshark::global::{GPooled, Pool};
use std::{
    cell::{Cell, RefCell},
    collections::hash_map::Entry,
    fmt,
    sync::{Arc, LazyLock, mpsc},
};
use tokio::sync::oneshot;

type TxnMsg = (TxnCommand, oneshot::Sender<Value>);

enum TxnCommand {
    OpenTree { name: Option<ArcStr>, types: TreeTypes },
    Get { tree_idx: usize, key: GPooled<Vec<u8>> },
    Insert { tree_idx: usize, key: GPooled<Vec<u8>>, value: GPooled<Vec<u8>> },
    Remove { tree_idx: usize, key: GPooled<Vec<u8>> },
    Batch { tree_idx: usize, batch: sled::Batch },
    Commit,
    Rollback,
}

pub(crate) struct TxnInner {
    cmd_tx: mpsc::Sender<TxnMsg>,
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

async fn txn_send_recv(cmd_tx: &mpsc::Sender<TxnMsg>, cmd: TxnCommand) -> Value {
    let (reply_tx, reply_rx) = oneshot::channel();
    if cmd_tx.send((cmd, reply_tx)).is_err() {
        return errf!("DbErr", "transaction thread gone");
    }
    match reply_rx.await {
        Ok(v) => v,
        Err(_) => errf!("DbErr", "transaction thread gone"),
    }
}

fn db_err(e: anyhow::Error) -> Value {
    errf!("DbErr", "{e:#}")
}

struct TxnCtx<'a> {
    trees: &'a [sled::transaction::TransactionalTree],
    rx: mpsc::Receiver<TxnMsg>,
    commit_reply: &'a RefCell<Option<oneshot::Sender<Value>>>,
    aborted: &'a Cell<bool>,
    meta_idx: Option<usize>,
    pending_meta: &'a AHashMap<ArcStr, TreeTypes>,
}

impl TxnCtx<'_> {
    fn write_meta(&self) -> Result<()> {
        let Some(mi) = self.meta_idx else { return Ok(()) };
        for (tree_name, types) in self.pending_meta {
            self.trees[mi].check_or_store(tree_name, types)?;
        }
        Ok(())
    }

    fn tree(&self, tree_idx: usize) -> Result<&sled::transaction::TransactionalTree> {
        self.trees.get(tree_idx).ok_or_else(|| anyhow!("invalid tree index"))
    }

    fn abort(&self) -> sled::transaction::ConflictableTransactionResult<(), ()> {
        self.aborted.set(true);
        sled::transaction::abort(())
    }

    fn run(
        self,
        first_msg: TxnMsg,
    ) -> sled::transaction::ConflictableTransactionResult<(), ()> {
        if let Err(e) = self.write_meta() {
            let _ = first_msg.1.send(db_err(e));
            return self.abort();
        }
        let mut pending = Some(first_msg);
        loop {
            let (cmd, reply) = match pending.take() {
                Some(msg) => msg,
                None => match self.rx.recv() {
                    Ok(msg) => msg,
                    Err(_) => return self.abort(),
                },
            };
            let res = match cmd {
                TxnCommand::OpenTree { .. } => {
                    Err(anyhow!("cannot open trees after data operations"))
                }
                TxnCommand::Get { tree_idx, key } => self
                    .tree(tree_idx)
                    .and_then(|t| Ok(t.get(key.as_slice())?))
                    .and_then(|v| v.map_or(Ok(Value::Null), |v| decode_value(&v))),
                TxnCommand::Insert { tree_idx, key, value } => self
                    .tree(tree_idx)
                    .and_then(|t| Ok(t.insert(key.as_slice(), value.as_slice())?))
                    .and_then(value_or_null),
                TxnCommand::Remove { tree_idx, key } => self
                    .tree(tree_idx)
                    .and_then(|t| Ok(t.remove(key.as_slice())?))
                    .and_then(value_or_null),
                TxnCommand::Batch { tree_idx, ref batch } => self
                    .tree(tree_idx)
                    .and_then(|t| Ok(t.apply_batch(batch)?))
                    .map(|()| Value::Null),
                TxnCommand::Commit => {
                    *self.commit_reply.borrow_mut() = Some(reply);
                    return Ok(());
                }
                TxnCommand::Rollback => {
                    *self.commit_reply.borrow_mut() = Some(reply);
                    return self.abort();
                }
            };
            match res {
                Ok(v) => {
                    let _ = reply.send(v);
                }
                Err(e) => {
                    let is_txn_err =
                        e.is::<sled::transaction::UnabortableTransactionError>();
                    let _ = reply.send(db_err(e));
                    if is_txn_err {
                        return self.abort();
                    }
                }
            }
        }
    }
}

fn run_transaction(
    trees: &[sled::Tree],
    meta_idx: Option<usize>,
    pending_meta: &AHashMap<ArcStr, TreeTypes>,
    rx: mpsc::Receiver<TxnMsg>,
    first_msg: TxnMsg,
) {
    let state: RefCell<Option<(TxnMsg, mpsc::Receiver<TxnMsg>)>> =
        RefCell::new(Some((first_msg, rx)));
    let commit_reply: RefCell<Option<oneshot::Sender<Value>>> = RefCell::new(None);
    let aborted = Cell::new(false);
    // CR claude for eric: [bug] The whole interactive transaction runs inside this
    // closure: TxnCtx::run waits in rx.recv() for the program's next command. For as
    // long as the closure runs, sled 0.34 holds stage()'s concurrency_control::write().
    // That is one static RwLock shared by every sled Db in the process, and every plain
    // tree op, iterator step, generate_id, flush and sled's log flusher take its read
    // side. So from a txn's first data op until its commit or rollback, every other db
    // operation in the process blocks, on any database file. A plain op that the commit
    // is sequenced after deadlocks, so do two txns interleaved across two databases,
    // and a txn that is never committed freezes all db I/O for good. probe:
    // design/review-2026-10-05/repro/db2-01.gx (hangs after "txn insert done";
    // committing before the get exits 0). (db2-01)
    let result = sled::transaction::Transactional::transaction(
        trees,
        |tx_trees: &Vec<sled::transaction::TransactionalTree>| {
            if let Some((first_msg, rx)) = state.borrow_mut().take() {
                TxnCtx {
                    trees: tx_trees,
                    rx,
                    commit_reply: &commit_reply,
                    aborted: &aborted,
                    meta_idx,
                    pending_meta,
                }
                .run(first_msg)
            } else {
                // a conflict retry cannot re-run the program's commands
                sled::transaction::abort(())
            }
        },
    );
    if let Some(reply) = commit_reply.borrow_mut().take() {
        let _ = reply.send(match result {
            // XCR claude for claude: [bug] Commit replies null but never calls
            // self.trees[0].flush(), so sled leaves the commit in its in-memory log
            // until the 500 ms flusher runs. A program that commits and then calls
            // sys::exit (std::process::exit, so no Drop runs) or crashes loses the
            // commit. The book calls these ACID transactions. The flusher also
            // takes sled's process-global concurrency lock, which every open
            // db::txn holds for its whole life, so while another transaction is
            // open nothing reaches disk. Either set flush_on_commit here (an fsync
            // per commit), or document that a commit is durable only after
            // db::flush. probe: design/review-2026-10-05/repro/db2-09.sh (db2-09)
            // 2026-10-07 claude: a commit flushes before it answers (an fsync per commit). The
            // flush takes sled's process-wide lock, so a commit answers only once every other
            // open txn has ended (db2-01). design/review-2026-10-05/repro/db2-09.sh: k = 42
            // after every restart.
            Ok(()) => match trees[0].flush() {
                Ok(_) => Value::Null,
                Err(e) => errf!("DbErr", "{e}"),
            },
            Err(sled::transaction::TransactionError::Abort(())) if aborted.get() => {
                Value::Null
            }
            Err(sled::transaction::TransactionError::Abort(())) => {
                errf!("DbErr", "transaction conflict")
            }
            Err(sled::transaction::TransactionError::Storage(e)) => errf!("DbErr", "{e}"),
        });
    }
}

struct BeginTxnCtx {
    trees: GPooled<Vec<sled::Tree>>,
    /// The slot of each tree opened, by meta key: a tree opened twice is
    /// one slot, so both handles see one overlay.
    opened: AHashMap<ArcStr, usize>,
    pending_meta: AHashMap<ArcStr, TreeTypes>,
    db: Db,
    rx: mpsc::Receiver<TxnMsg>,
}

impl BeginTxnCtx {
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
            Some(n) => self.db.open_tree(n.as_bytes())?,
        };
        let idx = self.trees.len();
        self.trees.push(tree);
        self.opened.insert(key, idx);
        Ok(idx)
    }

    /// A transaction that opened trees and touched no data: its new
    /// trees' types are all it writes.
    fn commit(&mut self) -> Result<()> {
        let meta = self.db.open_tree(META_TREE)?;
        for (tree_name, types) in self.pending_meta.drain() {
            meta.check_or_store(&tree_name, &types)?
        }
        meta.flush()?;
        Ok(())
    }

    fn run(mut self) {
        loop {
            let Ok((msg, reply)) = self.rx.recv() else { return };
            match msg {
                TxnCommand::OpenTree { name, types } => {
                    let res = match self.open_tree(name, types) {
                        Ok(tid) => Value::U64(tid as u64),
                        Err(e) => db_err(e),
                    };
                    let _ = reply.send(res);
                }
                TxnCommand::Commit => {
                    let res = match self.commit() {
                        Ok(()) => Value::Null,
                        Err(e) => db_err(e),
                    };
                    let _ = reply.send(res);
                    return;
                }
                TxnCommand::Rollback => {
                    let _ = reply.send(Value::Null);
                    return;
                }
                // the first data op starts the sled transaction
                first_msg => {
                    if self.trees.is_empty() {
                        let _ =
                            reply.send(errf!("DbErr", "no trees opened in transaction"));
                        return;
                    }
                    let meta_idx = if !self.pending_meta.is_empty() {
                        match self.db.open_tree(META_TREE) {
                            Ok(meta) => {
                                let idx = self.trees.len();
                                self.trees.push(meta);
                                Some(idx)
                            }
                            Err(e) => {
                                let _ = reply.send(errf!("DbErr", "{e}"));
                                return;
                            }
                        }
                    } else {
                        None
                    };
                    run_transaction(
                        &self.trees,
                        meta_idx,
                        &self.pending_meta,
                        self.rx,
                        (first_msg, reply),
                    );
                    return;
                }
            }
        }
    }

    fn new(db: Db, rx: mpsc::Receiver<TxnMsg>) -> Self {
        static TREES: LazyLock<Pool<Vec<sled::Tree>>> =
            LazyLock::new(|| Pool::new(64, 256));
        Self {
            db,
            rx,
            opened: AHashMap::new(),
            pending_meta: AHashMap::new(),
            trees: TREES.take(),
        }
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
            let (cmd_tx, cmd_rx) = mpsc::channel();
            match std::thread::Builder::new()
                .name("graphix-db-txn".into())
                .spawn(move || BeginTxnCtx::new(db, cmd_rx).run())
            {
                Ok(_) => {
                    TXN_WRAPPER.wrap(TxnValue { inner: Arc::new(TxnInner { cmd_tx }) })
                }
                Err(e) => errf!("DbErr", "could not start the transaction thread: {e}"),
            }
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
        async move {
            let key_typ = types.key;
            match txn_send_recv(&txn.cmd_tx, TxnCommand::OpenTree { name, types }).await {
                Value::U64(idx) => TXN_TREE_WRAPPER.wrap(TxnTreeValue {
                    inner: Arc::new(TxnTreeInner {
                        txn: txn.clone(),
                        tree_idx: idx as usize,
                        key_typ,
                    }),
                }),
                v => v,
            }
        }
    }
}

pub(crate) type DbTxnTree = CachedArgsAsync<OpenTreeEv<FromTxn>>;

/// A builtin sending one command for a transaction's tree, built from the
/// encoding of its arguments; a failed encoding is its DbErr.
macro_rules! txn_op {
    ($ev:ident, $alias:ident, $name:literal, $enc:expr, $arg:ty,
     |$a:pat_param, $idx:ident| $cmd:expr) => {
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
                async move {
                    match arg {
                        Err(e) => db_err(e),
                        Ok($a) => {
                            let $idx = tt.tree_idx;
                            txn_send_recv(&tt.txn.cmd_tx, $cmd).await
                        }
                    }
                }
            }
        }

        graphix_package_core::unit_image_state!($ev);
        pub(crate) type $alias = CachedArgsAsync<$ev>;
    };
}

txn_op!(
    DbTxnGetEv,
    DbTxnGet,
    "db_txn_get",
    key_arg,
    GPooled<Vec<u8>>,
    |key, tree_idx| TxnCommand::Get { tree_idx, key }
);
txn_op!(
    DbTxnRemoveEv,
    DbTxnRemove,
    "db_txn_remove",
    key_arg,
    GPooled<Vec<u8>>,
    |key, tree_idx| TxnCommand::Remove { tree_idx, key }
);
txn_op!(
    DbTxnInsertEv,
    DbTxnInsert,
    "db_txn_insert",
    insert_arg,
    (GPooled<Vec<u8>>, GPooled<Vec<u8>>),
    |(key, value), tree_idx| TxnCommand::Insert { tree_idx, key, value }
);
txn_op!(
    DbTxnBatchEv,
    DbTxnBatch,
    "db_txn_batch",
    batch_arg,
    sled::Batch,
    |batch, tree_idx| TxnCommand::Batch { tree_idx, batch }
);

/// A builtin ending a transaction.
macro_rules! txn_end {
    ($ev:ident, $alias:ident, $name:literal, $cmd:expr) => {
        #[derive(Debug, Default)]
        pub(crate) struct $ev;

        impl EvalCachedAsync for $ev {
            type Args = Arc<TxnInner>;

            const NAME: &str = $name;

            fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
                get_txn(cached)
            }

            fn eval(txn: Self::Args) -> impl Future<Output = Value> + Send {
                async move { txn_send_recv(&txn.cmd_tx, $cmd).await }
            }
        }

        graphix_package_core::unit_image_state!($ev);
        pub(crate) type $alias = CachedArgsAsync<$ev>;
    };
}

txn_end!(DbTxnCommitEv, DbTxnCommit, "db_txn_commit", TxnCommand::Commit);
txn_end!(DbTxnRollbackEv, DbTxnRollback, "db_txn_rollback", TxnCommand::Rollback);

graphix_package_core::unit_image_state!(DbTxnBeginEv);
