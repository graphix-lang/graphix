use crate::encoding::{decode_value, encode_key, encode_value, parse_batch_ops};
use crate::tree::{
    check_or_store_meta, extract_key_typ_from_rtype, extract_type_strings_from_rtype,
    get_db, read_meta, types_are_concrete, DEFAULT_TREE_META, META_TREE,
};
use anyhow::{bail, Result};
use arcstr::ArcStr;
use fxhash::FxHashMap;
use graphix_compiler::{
    errf, expr::ExprId, typ::FnType, ExecCtx, Node, Rt, Scope, TypecheckPhase, UserEvent,
};
use graphix_package_core::{CachedArgsAsync, CachedVals, EvalCachedAsync};
use netidx::publisher::Typ;
use netidx_value::Value;
use poolshark::global::{GPooled, Pool};
use std::collections::hash_map::Entry;
use std::sync::LazyLock;
use std::{
    cell::{Cell, RefCell},
    fmt,
    sync::{mpsc, Arc},
};
use tokio::sync::oneshot;

// ── Transaction types ─────────────────────────────────────────────

type TxnMsg = (TxnCommand, oneshot::Sender<Value>);

enum TxnCommand {
    OpenTree { name: Option<ArcStr>, key_typ_str: ArcStr, val_typ_str: ArcStr },
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

graphix_package_core::impl_abstract_arc!(TxnValue, static TXN_WRAPPER = [
    0xd5, 0xe6, 0xf7, 0x08, 0x19, 0x2a, 0x4b, 0x3c,
    0x4d, 0x5e, 0x6f, 0x70, 0x81, 0xa2, 0xb3, 0xc4,
]);

fn get_txn(cached: &CachedVals, idx: usize) -> Option<Arc<TxnInner>> {
    match cached.0.get(idx)?.as_ref()? {
        Value::Abstract(a) => {
            let tv = a.downcast_ref::<TxnValue>()?;
            Some(tv.inner.clone())
        }
        _ => None,
    }
}

// -- TxnTreeValue --

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

#[derive(Debug, Clone)]
struct TxnTreeValue {
    inner: Arc<TxnTreeInner>,
}

graphix_package_core::impl_abstract_arc!(TxnTreeValue, static TXN_TREE_WRAPPER = [
    0xd6, 0xe7, 0xf8, 0x09, 0x1a, 0x2b, 0x4c, 0x3d,
    0x4e, 0x5f, 0x60, 0x71, 0x82, 0xa3, 0xb4, 0xc5,
]);

fn get_txn_tree(cached: &CachedVals, idx: usize) -> Option<Arc<TxnTreeInner>> {
    match cached.0.get(idx)?.as_ref()? {
        Value::Abstract(a) => {
            let tv = a.downcast_ref::<TxnTreeValue>()?;
            Some(tv.inner.clone())
        }
        _ => None,
    }
}

// ── Transaction thread machinery ──────────────────────────────────

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

// -- Phase 2: data operations inside a sled transaction --

struct TxnCtx<'a> {
    trees: &'a [sled::transaction::TransactionalTree],
    rx: mpsc::Receiver<TxnMsg>,
    commit_reply: &'a RefCell<Option<oneshot::Sender<Value>>>,
    aborted: &'a Cell<bool>,
    meta_idx: Option<usize>,
    pending_meta: &'a FxHashMap<ArcStr, (ArcStr, ArcStr)>,
}

impl TxnCtx<'_> {
    fn write_meta(&self) -> Result<()> {
        let Some(mi) = self.meta_idx else { return Ok(()) };
        for (tree_name, (key_typ_str, val_typ_str)) in self.pending_meta {
            check_or_store_meta(&self.trees[mi], tree_name, key_typ_str, val_typ_str)?;
        }
        Ok(())
    }

    fn get(&self, tree_idx: usize, key: &[u8]) -> Result<Value> {
        if tree_idx >= self.trees.len() {
            bail!("invalid tree index");
        }
        match self.trees[tree_idx].get(key)? {
            None => Ok(Value::Null),
            Some(ivec) => decode_value(&ivec)
                .ok_or_else(|| anyhow::anyhow!("failed to decode value")),
        }
    }

    fn insert(&self, tree_idx: usize, key: &[u8], value: &[u8]) -> Result<Value> {
        if tree_idx >= self.trees.len() {
            bail!("invalid tree index");
        }
        let prev = self.trees[tree_idx].insert(key, value)?;
        Ok(match prev {
            None => Value::Null,
            Some(ivec) => decode_value(&ivec).unwrap_or(Value::Null),
        })
    }

    fn remove(&self, tree_idx: usize, key: &[u8]) -> Result<Value> {
        if tree_idx >= self.trees.len() {
            bail!("invalid tree index");
        }
        let prev = self.trees[tree_idx].remove(key)?;
        Ok(match prev {
            None => Value::Null,
            Some(ivec) => decode_value(&ivec).unwrap_or(Value::Null),
        })
    }

    fn apply_batch(&self, tree_idx: usize, batch: &sled::Batch) -> Result<Value> {
        if tree_idx >= self.trees.len() {
            bail!("invalid tree index");
        }
        self.trees[tree_idx].apply_batch(batch)?;
        Ok(Value::Null)
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
            let _ = first_msg.1.send(errf!("DbErr", "{e:?}"));
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
                    Err(anyhow::anyhow!("cannot open trees after data operations"))
                }
                TxnCommand::Get { tree_idx, key } => self.get(tree_idx, &key),
                TxnCommand::Insert { tree_idx, key, value } => {
                    self.insert(tree_idx, &key, &value)
                }
                TxnCommand::Remove { tree_idx, key } => self.remove(tree_idx, &key),
                TxnCommand::Batch { tree_idx, ref batch } => {
                    self.apply_batch(tree_idx, batch)
                }
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
                    let _ = reply.send(errf!("DbErr", "{e:?}"));
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
    pending_meta: &FxHashMap<ArcStr, (ArcStr, ArcStr)>,
    rx: mpsc::Receiver<TxnMsg>,
    first_msg: TxnMsg,
) {
    let state: RefCell<Option<(TxnMsg, mpsc::Receiver<TxnMsg>)>> =
        RefCell::new(Some((first_msg, rx)));
    let commit_reply: RefCell<Option<oneshot::Sender<Value>>> = RefCell::new(None);
    let aborted = Cell::new(false);
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
                // Conflict retry — we cannot re-run user code, abort
                sled::transaction::abort(())
            }
        },
    );

    if let Some(reply) = commit_reply.borrow_mut().take() {
        match result {
            Ok(()) => {
                let _ = reply.send(Value::Null);
            }
            Err(sled::transaction::TransactionError::Abort(())) if aborted.get() => {
                let _ = reply.send(Value::Null);
            }
            Err(sled::transaction::TransactionError::Abort(())) => {
                let _ = reply.send(errf!("DbErr", "transaction conflict"));
            }
            Err(sled::transaction::TransactionError::Storage(e)) => {
                let _ = reply.send(errf!("DbErr", "{e}"));
            }
        }
    }
}

// -- Phase 1: tree opens and metadata, before any data operations --

struct BeginTxnCtx {
    trees: GPooled<Vec<sled::Tree>>,
    pending_meta: GPooled<FxHashMap<ArcStr, (ArcStr, ArcStr)>>,
    db: sled::Db,
    rx: mpsc::Receiver<TxnMsg>,
}

impl BeginTxnCtx {
    fn open_tree(
        &mut self,
        name: Option<ArcStr>,
        key_typ_str: ArcStr,
        val_typ_str: ArcStr,
    ) -> Result<usize> {
        let tree_name =
            name.as_ref().cloned().unwrap_or_else(|| DEFAULT_TREE_META.clone());
        if let Some(n) = name.as_ref() {
            if n == &DEFAULT_TREE_META || n == &META_TREE {
                bail!("tree name '{n}' is reserved");
            }
        }
        if !types_are_concrete(&key_typ_str, &val_typ_str) {
            bail!("tree requires concrete type annotations")
        }
        // Read-only check for early mismatch detection
        let meta = self.db.open_tree(&META_TREE)?;
        match read_meta(&meta, &tree_name)? {
            Some((sk, sv)) => {
                if sk != key_typ_str || sv != val_typ_str {
                    bail!(
                        "tree '{tree_name}' has type Tree<{sk}, {sv}>, \
                         but was opened as Tree<{key_typ_str}, {val_typ_str}>"
                    );
                }
            }
            None => {
                match self.pending_meta.entry(tree_name.clone()) {
                    Entry::Vacant(e) => {
                        e.insert((key_typ_str, val_typ_str));
                    }
                    Entry::Occupied(e) => {
                        let (k, v) = e.get();
                        if k != &key_typ_str || v != &val_typ_str {
                            bail!("conflicting types for tree '{tree_name}' within transaction")
                        }
                    }
                }
            }
        }
        let tree = match &name {
            None => (*self.db).clone(),
            Some(n) => self.db.open_tree(n.as_bytes())?,
        };
        let idx = self.trees.len();
        self.trees.push(tree);
        Ok(idx)
    }

    fn commit(&mut self) -> Result<()> {
        let meta = self.db.open_tree(&META_TREE)?;
        for (tree_name, (key_typ_str, val_typ_str)) in self.pending_meta.drain() {
            check_or_store_meta(&meta, &tree_name, &key_typ_str, &val_typ_str)?
        }
        Ok(())
    }

    fn run(mut self) {
        loop {
            let Ok((msg, reply)) = self.rx.recv() else { return };
            match msg {
                TxnCommand::OpenTree { name, key_typ_str, val_typ_str } => {
                    let res = match self.open_tree(name, key_typ_str, val_typ_str) {
                        Ok(tid) => Value::U64(tid as u64),
                        Err(e) => errf!("DbErr", "{e:?}"),
                    };
                    let _ = reply.send(res);
                }
                TxnCommand::Commit => {
                    let res = match self.commit() {
                        Ok(()) => Value::Null,
                        Err(e) => errf!("DbErr", "{e:?}"),
                    };
                    let _ = reply.send(res);
                    return;
                }
                TxnCommand::Rollback => {
                    let _ = reply.send(Value::Null);
                    return;
                }
                // First data op transitions to phase 2
                first_msg => {
                    if self.trees.is_empty() {
                        let _ =
                            reply.send(errf!("DbErr", "no trees opened in transaction"));
                        return;
                    }
                    let meta_idx = if !self.pending_meta.is_empty() {
                        match self.db.open_tree(&META_TREE) {
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

    fn new(db: sled::Db, rx: mpsc::Receiver<TxnMsg>) -> Self {
        static TREES: LazyLock<Pool<Vec<sled::Tree>>> =
            LazyLock::new(|| Pool::new(64, 256));
        static PENDING: LazyLock<Pool<FxHashMap<ArcStr, (ArcStr, ArcStr)>>> =
            LazyLock::new(|| Pool::new(64, 256));
        Self { db, rx, pending_meta: PENDING.take(), trees: TREES.take() }
    }
}

fn txn_thread(db: sled::Db, cmd_rx: mpsc::Receiver<TxnMsg>) {
    BeginTxnCtx::new(db, cmd_rx).run();
}

// ── Transaction builtins ──────────────────────────────────────────

// -- DbTxnBegin --

#[derive(Debug, Default)]
pub(crate) struct DbTxnBeginEv;

impl EvalCachedAsync for DbTxnBeginEv {
    const NAME: &str = "db_txn_begin";
    const NEEDS_CALLSITE: bool = false;
    type Args = sled::Db;

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        get_db(cached, 0)
    }

    fn eval(db: Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            let (cmd_tx, cmd_rx) = mpsc::channel();
            std::thread::Builder::new()
                .name("graphix-db-txn".into())
                .spawn(move || txn_thread(db, cmd_rx))
                .expect("failed to spawn transaction thread");
            TXN_WRAPPER.wrap(TxnValue { inner: Arc::new(TxnInner { cmd_tx }) })
        }
    }
}

pub(crate) type DbTxnBegin = CachedArgsAsync<DbTxnBeginEv>;

// -- DbTxnTree --

#[derive(Debug)]
pub(crate) struct DbTxnTreeArgs {
    txn: Arc<TxnInner>,
    name: Option<ArcStr>,
    key_typ: Option<Typ>,
    key_typ_str: ArcStr,
    val_typ_str: ArcStr,
}

#[derive(Debug, Default)]
pub(crate) struct DbTxnTreeEv {
    key_typ: Option<Typ>,
    key_typ_str: ArcStr,
    val_typ_str: ArcStr,
}

impl EvalCachedAsync for DbTxnTreeEv {
    const NAME: &str = "db_txn_tree";
    const NEEDS_CALLSITE: bool = true;
    type Args = DbTxnTreeArgs;

    fn init<R: Rt, E: UserEvent>(
        _ctx: &mut ExecCtx<R, E>,
        _typ: &FnType,
        resolved: Option<&FnType>,
        _scope: &Scope,
        _from: &[Node<R, E>],
        _top_id: ExprId,
    ) -> Self {
        let key_typ = extract_key_typ_from_rtype(resolved);
        let (key_typ_str, val_typ_str) = extract_type_strings_from_rtype(resolved);
        DbTxnTreeEv { key_typ, key_typ_str, val_typ_str }
    }

    fn typecheck<R: Rt, E: UserEvent>(
        &mut self,
        _ctx: &mut ExecCtx<R, E>,
        _from: &mut [Node<R, E>],
        phase: TypecheckPhase<'_>,
    ) -> Result<()> {
        match phase {
            TypecheckPhase::Lambda => Ok(()),
            TypecheckPhase::CallSite(resolved) => {
                self.key_typ = extract_key_typ_from_rtype(Some(resolved));
                let (k, v) = extract_type_strings_from_rtype(Some(resolved));
                self.key_typ_str = k;
                self.val_typ_str = v;
                if self.key_typ.is_none() {
                    bail!("db::tree requires concrete key and value types")
                }
                Ok(())
            }
        }
    }

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        let txn = get_txn(cached, 0)?;
        let name = match cached.0.get(1)?.as_ref()? {
            Value::Null => None,
            Value::String(s) => Some(s.clone()),
            _ => return None,
        };
        Some(DbTxnTreeArgs {
            txn,
            name,
            key_typ: self.key_typ,
            key_typ_str: self.key_typ_str.clone(),
            val_typ_str: self.val_typ_str.clone(),
        })
    }

    fn eval(args: Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            let DbTxnTreeArgs { txn, name, key_typ, key_typ_str, val_typ_str } = args;
            let v = txn_send_recv(
                &txn.cmd_tx,
                TxnCommand::OpenTree { name, key_typ_str, val_typ_str },
            )
            .await;
            match &v {
                Value::U64(idx) => TXN_TREE_WRAPPER.wrap(TxnTreeValue {
                    inner: Arc::new(TxnTreeInner {
                        txn: txn.clone(),
                        tree_idx: *idx as usize,
                        key_typ,
                    }),
                }),
                _ => v,
            }
        }
    }
}

pub(crate) type DbTxnTree = CachedArgsAsync<DbTxnTreeEv>;

// -- DbTxnGet --

#[derive(Debug, Default)]
pub(crate) struct DbTxnGetEv;

impl EvalCachedAsync for DbTxnGetEv {
    const NAME: &str = "db_txn_get";
    const NEEDS_CALLSITE: bool = false;
    type Args = (Arc<TxnTreeInner>, GPooled<Vec<u8>>);

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        let tt = get_txn_tree(cached, 0)?;
        let key_val = cached.0.get(1)?.as_ref()?;
        let key = encode_key(tt.key_typ, key_val)?;
        Some((tt, key))
    }

    fn eval((tt, key): Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            let tree_idx = tt.tree_idx;
            txn_send_recv(&tt.txn.cmd_tx, TxnCommand::Get { tree_idx, key }).await
        }
    }
}

pub(crate) type DbTxnGet = CachedArgsAsync<DbTxnGetEv>;

// -- DbTxnInsert --

#[derive(Debug, Default)]
pub(crate) struct DbTxnInsertEv;

impl EvalCachedAsync for DbTxnInsertEv {
    const NAME: &str = "db_txn_insert";
    const NEEDS_CALLSITE: bool = false;
    type Args = (Arc<TxnTreeInner>, GPooled<Vec<u8>>, GPooled<Vec<u8>>);

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        let tt = get_txn_tree(cached, 0)?;
        let key_val = cached.0.get(1)?.as_ref()?;
        let key = encode_key(tt.key_typ, key_val)?;
        let val = encode_value(cached.0.get(2)?.as_ref()?)?;
        Some((tt, key, val))
    }

    fn eval((tt, key, val): Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            let tree_idx = tt.tree_idx;
            txn_send_recv(
                &tt.txn.cmd_tx,
                TxnCommand::Insert { tree_idx, key, value: val },
            )
            .await
        }
    }
}

pub(crate) type DbTxnInsert = CachedArgsAsync<DbTxnInsertEv>;

// -- DbTxnRemove --

#[derive(Debug, Default)]
pub(crate) struct DbTxnRemoveEv;

impl EvalCachedAsync for DbTxnRemoveEv {
    const NAME: &str = "db_txn_remove";
    const NEEDS_CALLSITE: bool = false;
    type Args = (Arc<TxnTreeInner>, GPooled<Vec<u8>>);

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        let tt = get_txn_tree(cached, 0)?;
        let key_val = cached.0.get(1)?.as_ref()?;
        let key = encode_key(tt.key_typ, key_val)?;
        Some((tt, key))
    }

    fn eval((tt, key): Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            let tree_idx = tt.tree_idx;
            txn_send_recv(&tt.txn.cmd_tx, TxnCommand::Remove { tree_idx, key }).await
        }
    }
}

pub(crate) type DbTxnRemove = CachedArgsAsync<DbTxnRemoveEv>;

// -- DbTxnCommit --

#[derive(Debug, Default)]
pub(crate) struct DbTxnCommitEv;

impl EvalCachedAsync for DbTxnCommitEv {
    const NAME: &str = "db_txn_commit";
    const NEEDS_CALLSITE: bool = false;
    type Args = Arc<TxnInner>;

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        get_txn(cached, 0)
    }

    fn eval(txn: Self::Args) -> impl Future<Output = Value> + Send {
        async move { txn_send_recv(&txn.cmd_tx, TxnCommand::Commit).await }
    }
}

pub(crate) type DbTxnCommit = CachedArgsAsync<DbTxnCommitEv>;

// -- DbTxnRollback --

#[derive(Debug, Default)]
pub(crate) struct DbTxnRollbackEv;

impl EvalCachedAsync for DbTxnRollbackEv {
    const NAME: &str = "db_txn_rollback";
    const NEEDS_CALLSITE: bool = false;
    type Args = Arc<TxnInner>;

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        get_txn(cached, 0)
    }

    fn eval(txn: Self::Args) -> impl Future<Output = Value> + Send {
        async move { txn_send_recv(&txn.cmd_tx, TxnCommand::Rollback).await }
    }
}

pub(crate) type DbTxnRollback = CachedArgsAsync<DbTxnRollbackEv>;

// -- DbTxnBatch --

#[derive(Debug, Default)]
pub(crate) struct DbTxnBatchEv;

impl EvalCachedAsync for DbTxnBatchEv {
    const NAME: &str = "db_txn_batch";
    const NEEDS_CALLSITE: bool = false;
    type Args = (Arc<TxnTreeInner>, sled::Batch);

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        let tt = get_txn_tree(cached, 0)?;
        let arr = match cached.0.get(1)?.as_ref()? {
            Value::Array(a) => a,
            _ => return None,
        };
        let batch = parse_batch_ops(tt.key_typ, arr)?;
        Some((tt, batch))
    }

    fn eval((tt, batch): Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            let tree_idx = tt.tree_idx;
            txn_send_recv(&tt.txn.cmd_tx, TxnCommand::Batch { tree_idx, batch }).await
        }
    }
}

pub(crate) type DbTxnBatch = CachedArgsAsync<DbTxnBatchEv>;
