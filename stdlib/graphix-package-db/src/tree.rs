use ahash::AHashMap;
use anyhow::{Result, anyhow, bail};
use arcstr::{ArcStr, literal};
use compact_str::format_compact;
use enumflags2::BitFlags;
use graphix_compiler::{
    CompileCtx, ExecCtx, Node, Rt, Scope, UserEvent,
    env::Env,
    errf,
    expr::ExprId,
    image::ImageBuf,
    typ::{FnType, Type},
};
use graphix_package_core::{CachedArgsAsync, CachedVals, EvalCachedAsync, ImageState};
use netidx::{path::Path, publisher::Typ};
use netidx_core::pack::{Pack, PackError};
use netidx_derive::Pack;
use netidx_value::{ValArray, Value};
use parking_lot::Mutex;
use poolshark::{global::GPooled, local::LPooled};
use std::{
    fmt,
    marker::PhantomData,
    path::PathBuf,
    sync::{Arc, LazyLock, Weak},
};

use crate::encoding::{
    ENCODE_MANY_POOL, decode_key, decode_value, encode_key, encode_value, parse_batch_ops,
};

pub(crate) use graphix_package_core::Reaped;

pub(crate) type Db = Arc<Reaped<sled::Db>>;

#[derive(Debug, Clone)]
pub struct DbValue {
    pub(crate) inner: Db,
}

graphix_package_core::impl_abstract_arc!(
    DbValue,
    pub(crate) static DB_WRAPPER = "db::Db"
);

pub(crate) fn abstract_arg<T: std::any::Any + Send + Sync>(
    cached: &CachedVals,
    idx: usize,
) -> Option<&T> {
    match cached.0.get(idx)?.as_ref()? {
        Value::Abstract(a) => a.downcast_ref::<T>(),
        _ => None,
    }
}

pub(crate) fn get_db(cached: &CachedVals, idx: usize) -> Option<Db> {
    abstract_arg::<DbValue>(cached, idx).map(|d| d.inner.clone())
}

/// One sled handle per database in the process: sled locks a database's
/// files while any handle lives, so a second open would fail.
static OPEN: LazyLock<Mutex<AHashMap<PathBuf, Weak<Reaped<sled::Db>>>>> =
    LazyLock::new(|| Mutex::new(AHashMap::new()));

fn open_db(path: &str) -> Result<Db> {
    let mut open = OPEN.lock();
    let canonical = || std::fs::canonicalize(path);
    if let Ok(p) = canonical()
        && let Some(db) = open.get(&p).and_then(Weak::upgrade)
    {
        return Ok(db);
    }
    let db = Arc::new(Reaped::new(sled::open(path)?));
    open.retain(|_, w| w.strong_count() > 0);
    open.insert(canonical()?, Arc::downgrade(&db));
    Ok(db)
}

#[derive(Debug)]
// CR claude for claude: [bug/medium] A tree, and a cursor's `sled::Iter` made from one,
// keeps sled's context alive, and that context holds the database's file lock. Neither
// keeps the `Db` Arc alive. So once the last `DbValue` drops, the `OPEN` entry's Weak
// is dead while the lock is still held, and `open_db` calls `sled::open` again, which
// fails with WouldBlock. A program that switches a.db -> b.db -> a.db while a tree of
// a.db is still alive (for example, kept by `once`) gets `DbErr("IO error: could not
// acquire lock ...")`. Fix: have `TreeInner` own the `Db` Arc, and have `CursorInner`
// own the `Arc<TreeInner>` it iterates. Then the registry entry stays upgradable for as
// long as any handle holding the lock exists. The async drop in `Reaped` leaves a
// smaller window of the same kind: the lock is released on a blocking thread after the
// Weak is already dead. Consider having that drop clear the registry entry only once
// sled's drop has finished. probe: design/review-2026-10-12/repro/db2-03.gx (db2-03)
pub(crate) struct TreeInner {
    pub(crate) tree: Reaped<sled::Tree>,
    pub(crate) key_typ: Option<Typ>,
}

#[derive(Debug, Clone)]
pub struct TreeValue {
    pub(crate) inner: Arc<TreeInner>,
}

graphix_package_core::impl_abstract_arc!(
    TreeValue,
    pub(crate) static TREE_WRAPPER = "db::Tree"
);

pub(crate) fn get_tree_inner(cached: &CachedVals, idx: usize) -> Option<Arc<TreeInner>> {
    abstract_arg::<TreeValue>(cached, idx).map(|t| t.inner.clone())
}

fn wrap_tree(tree: sled::Tree, key_typ: Option<Typ>) -> Value {
    let tree = Reaped::new(tree);
    TREE_WRAPPER.wrap(TreeValue { inner: Arc::new(TreeInner { tree, key_typ }) })
}

/// Run `f` on a blocking thread; its error, or its panic, is a `DbErr`.
pub(crate) async fn blocking<F>(f: F) -> Value
where
    F: FnOnce() -> Result<Value> + Send + 'static,
{
    match tokio::task::spawn_blocking(f).await {
        Err(e) => errf!("DbErr", "task panicked: {e}"),
        Ok(Err(e)) => errf!("DbErr", "{e:#}"),
        Ok(Ok(v)) => v,
    }
}

pub(crate) fn value_or_null(v: Option<sled::IVec>) -> Result<Value> {
    v.map_or(Ok(Value::Null), |v| decode_value(&v))
}

fn entry(tree: &TreeInner, e: Option<(sled::IVec, sled::IVec)>) -> Result<Value> {
    match e {
        None => Ok(Value::Null),
        Some((k, v)) => Ok(Value::Array(ValArray::from([
            decode_key(tree.key_typ, &k)?,
            decode_value(&v)?,
        ]))),
    }
}

pub(crate) const META_TREE: &[u8] = b"$$__graphix_meta__$$";
const DEFAULT_TREE_META: &str = "$$__graphix_default__$$";

/// The names a program may not open, drop or see: the meta tree, the
/// default tree's meta key, and sled's own name for the default tree.
const RESERVED: [&[u8]; 3] =
    [META_TREE, DEFAULT_TREE_META.as_bytes(), b"__sled__default"];

/// The meta key of the tree a program names, null for the default tree.
pub(crate) fn meta_key(name: Option<&ArcStr>) -> Result<&str> {
    match name {
        None => Ok(DEFAULT_TREE_META),
        Some(n) if RESERVED.contains(&n.as_bytes()) => {
            bail!("tree name '{n}' is reserved")
        }
        Some(n) => Ok(n),
    }
}

/// The version of the stored form: the meta entry's text and the key
/// encoding. A tree written under another version does not open.
const META_VERSION: &str = "2";

/// The types a tree was opened with, `key` set when its keys are one
/// primitive type (stored untagged).
#[derive(Debug, Clone, Default, PartialEq)]
pub(crate) struct TreeTypes {
    pub(crate) key: Option<Typ>,
    key_str: ArcStr,
    val_str: ArcStr,
}

impl TreeTypes {
    fn of(resolved: Option<&FnType>, env: &Env) -> Self {
        let Some(params) = resolved.and_then(|ft| tree_params_of_result_type(&ft.rtype))
        else {
            return Self::default();
        };
        Self {
            key: prim_typ(&params[0]),
            key_str: stored_type(&params[0], env),
            val_str: stored_type(&params[1], env),
        }
    }

    pub(crate) fn concrete(&self) -> Result<()> {
        let concrete = |s: &str| !s.is_empty() && !s.starts_with('\'');
        if !concrete(&self.key_str) || !concrete(&self.val_str) {
            bail!("tree requires concrete type annotations")
        }
        Ok(())
    }

    fn entry(&self) -> compact_str::CompactString {
        format_compact!("{META_VERSION}\0{}\0{}", self.key_str, self.val_str)
    }

    /// Whether a stored meta entry is these types.
    fn check(&self, name: &str, stored: &[u8]) -> Result<()> {
        let (k, v) = parse_meta(name, stored)?;
        if k != self.key_str || v != self.val_str {
            bail!(
                "tree '{name}' has type Tree<{k}, {v}> but was opened as Tree<{}, {}>",
                self.key_str,
                self.val_str
            )
        }
        Ok(())
    }
}

impl ImageState for TreeTypes {
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        self.key.map(|t| t as u64).encode(buf)?;
        self.key_str.encode(buf)?;
        self.val_str.encode(buf)
    }

    fn image_decode<R: Rt, E: UserEvent>(
        _ctx: &mut ExecCtx<'_, R, E>,
        buf: &mut &[u8],
    ) -> Result<Self, PackError> {
        let key = <Option<u64>>::decode(buf)?
            .map(|bits| {
                BitFlags::<Typ>::from_bits(bits)
                    .ok()
                    .and_then(|f| f.exactly_one())
                    .ok_or(PackError::UnknownTag)
            })
            .transpose()?;
        Ok(Self { key, key_str: ArcStr::decode(buf)?, val_str: ArcStr::decode(buf)? })
    }
}

fn parse_meta<'a>(name: &str, stored: &'a [u8]) -> Result<(&'a str, &'a str)> {
    let mut parts = std::str::from_utf8(stored)?.split('\0');
    match (parts.next(), parts.next(), parts.next(), parts.next()) {
        (Some(META_VERSION), Some(k), Some(v), None) => Ok((k, v)),
        _ => bail!("tree '{name}' was written by another version of the db package"),
    }
}

/// What every tree opener does to the meta tree: an absent entry is
/// written, a present one must be these types.
pub(crate) trait MetaStore {
    fn get(&self, key: &[u8]) -> Result<Option<sled::IVec>>;
    fn insert_if_absent(&self, key: &[u8], value: &[u8]) -> Result<Option<sled::IVec>>;

    fn check_or_store(&self, name: &str, types: &TreeTypes) -> Result<()> {
        match self.insert_if_absent(name.as_bytes(), types.entry().as_bytes())? {
            None => Ok(()),
            Some(stored) => types.check(name, &stored),
        }
    }

    /// Whether the stored entry, if any, is these types.
    fn check(&self, name: &str, types: &TreeTypes) -> Result<bool> {
        match self.get(name.as_bytes())? {
            None => Ok(false),
            Some(stored) => types.check(name, &stored).map(|()| true),
        }
    }
}

impl MetaStore for sled::Tree {
    fn get(&self, key: &[u8]) -> Result<Option<sled::IVec>> {
        Ok(sled::Tree::get(self, key)?)
    }

    fn insert_if_absent(&self, key: &[u8], value: &[u8]) -> Result<Option<sled::IVec>> {
        match self.compare_and_swap(key, None as Option<&[u8]>, Some(value))? {
            Ok(()) => Ok(None),
            Err(cas_err) => Ok(cas_err.current),
        }
    }
}

impl MetaStore for sled::transaction::TransactionalTree {
    fn get(&self, key: &[u8]) -> Result<Option<sled::IVec>> {
        Ok(sled::transaction::TransactionalTree::get(self, key)?)
    }

    fn insert_if_absent(&self, key: &[u8], value: &[u8]) -> Result<Option<sled::IVec>> {
        match sled::transaction::TransactionalTree::get(self, key)? {
            Some(existing) => Ok(Some(existing)),
            None => {
                self.insert(key, value)?;
                Ok(None)
            }
        }
    }
}

fn prim_typ(t: &Type) -> Option<Typ> {
    match t {
        Type::Primitive(flags) => flags.exactly_one(),
        _ => None,
    }
}

fn tree_params_of_result_type(t: &Type) -> Option<&[Type]> {
    match t {
        Type::Ref(r) if Path::basename(&*r.name) == Some("Result") => {
            r.params.iter().find_map(|p| match p {
                Type::Ref(t)
                    if matches!(Path::basename(&*t.name), Some("Tree" | "TxnTree"))
                        && t.params.len() == 2 =>
                {
                    Some(&*t.params)
                }
                _ => None,
            })
        }
        _ => None,
    }
}

/// A key or value type as its tree's meta stores it: printed with every
/// typedef expanded, a recursive one by name inside its own expansion, so
/// two programs agree on the text exactly when they agree on the type.
fn stored_type(t: &Type, env: &Env) -> ArcStr {
    const MAX_DEPTH: usize = 256;
    fn expand(t: &Type, env: &Env, open: &mut Vec<*const ()>) -> Type {
        if open.len() > MAX_DEPTH {
            return t.clone();
        }
        if let Type::Ref(tr) = t {
            let Some(r) = tr.resolve_in(env) else { return t.clone() };
            let id = &*r as *const _ as *const ();
            if open.contains(&id) {
                return t.clone();
            }
            let Ok(body) = t.lookup_ref(env) else { return t.clone() };
            open.push(id);
            let body = expand(&body, env, open);
            open.pop();
            return body;
        }
        let mut go = |t: &Type| expand(t, env, open);
        match t {
            Type::Set(ts) => Type::Set(ts.iter().map(go).collect()),
            Type::Tuple(ts) => Type::Tuple(ts.iter().map(go).collect()),
            Type::Error(t) => Type::Error(triomphe::Arc::new(go(t))),
            Type::Array(t) => Type::Array(triomphe::Arc::new(go(t))),
            Type::List(t) => Type::List(triomphe::Arc::new(go(t))),
            Type::Map { key, value } => Type::Map {
                key: triomphe::Arc::new(go(key)),
                value: triomphe::Arc::new(go(value)),
            },
            Type::Struct(fs) => {
                Type::Struct(fs.iter().map(|(n, t, w)| (n.clone(), go(t), *w)).collect())
            }
            Type::Variant(n, ts, w) => {
                Type::Variant(n.clone(), ts.iter().map(go).collect(), *w)
            }
            Type::Abstract { id, params } => {
                Type::Abstract { id: *id, params: params.iter().map(go).collect() }
            }
            t => t.clone(),
        }
    }
    let t = expand(&t.resolve_tvars(), env, &mut vec![]).normalize();
    ArcStr::from(format_compact!("{t}").as_str())
}

/// A builtin over a db, its work on a blocking thread.
// CR claude for claude: [structure/low] db_op, tree_op, tree_arg_op and db_name_op
// here, and txn_op in txn.rs, each repeat the same struct, EvalCachedAsync impl,
// unit_image_state! and alias. They differ only in the Args type, the prepare_args
// expression and the eval body. Write one base macro that takes those three, and make
// each of the five a one-line delegation to it, the way tree_arg_op's short arm already
// delegates, so the call sites stay as short as they are. Then a change to the shared
// shape, such as moving encoding into the blocking closure (db2-08), is made once.
// (db2-11)
macro_rules! db_op {
    ($ev:ident, $alias:ident, $name:literal, |$db:ident| $body:expr) => {
        #[derive(Debug, Default)]
        pub(crate) struct $ev;

        impl EvalCachedAsync for $ev {
            type Args = Db;

            const NAME: &str = $name;

            fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
                get_db(cached, 0)
            }

            fn eval($db: Self::Args) -> impl Future<Output = Value> + Send {
                blocking(move || $body)
            }
        }

        graphix_package_core::unit_image_state!($ev);
        pub(crate) type $alias = CachedArgsAsync<$ev>;
    };
}

/// A builtin over a tree, its work on a blocking thread.
macro_rules! tree_op {
    ($ev:ident, $alias:ident, $name:literal, |$t:ident| $body:expr) => {
        #[derive(Debug, Default)]
        pub(crate) struct $ev;

        impl EvalCachedAsync for $ev {
            type Args = Arc<TreeInner>;

            const NAME: &str = $name;

            fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
                get_tree_inner(cached, 0)
            }

            fn eval($t: Self::Args) -> impl Future<Output = Value> + Send {
                blocking(move || $body)
            }
        }

        graphix_package_core::unit_image_state!($ev);
        pub(crate) type $alias = CachedArgsAsync<$ev>;
    };
}

/// A builtin over a tree and the encoding of its second argument (a key
/// unless `$enc` says otherwise); a failed encoding is the builtin's DbErr.
macro_rules! tree_arg_op {
    ($ev:ident, $alias:ident, $name:literal, $enc:expr, $arg:ty,
     |$t:ident, $k:pat_param| $body:expr) => {
        #[derive(Debug, Default)]
        pub(crate) struct $ev;

        impl EvalCachedAsync for $ev {
            type Args = (Arc<TreeInner>, Result<$arg>);

            const NAME: &str = $name;

            fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
                let tree = get_tree_inner(cached, 0)?;
                // CR claude for claude: [perf/low] prepare_args runs on the runtime
                // thread during the cycle, so $enc (encode_key, the batch and get_many
                // encoders) does O(n) encoding and allocation there, and only the sled
                // call moves to the blocking thread. A db::batch of 100k ops stalls the
                // cycle for the whole encoding before any I/O starts. Make Args the
                // handle plus the raw argument Values (clones are cheap) and encode
                // inside the blocking closure. That also removes the Result<$arg>
                // plumbing. txn_op in txn.rs has the same shape and needs the same
                // change. (db2-08)
                let arg = $enc(&tree, cached)?;
                Some((tree, arg))
            }

            fn eval(($t, arg): Self::Args) -> impl Future<Output = Value> + Send {
                blocking(move || {
                    let $k = arg?;
                    $body
                })
            }
        }

        graphix_package_core::unit_image_state!($ev);
        pub(crate) type $alias = CachedArgsAsync<$ev>;
    };
    ($ev:ident, $alias:ident, $name:literal, |$t:ident, $k:pat_param| $body:expr) => {
        tree_arg_op!($ev, $alias, $name, key_arg, GPooled<Vec<u8>>, |$t, $k| $body);
    };
}

pub(crate) fn key_arg(
    tree_key: impl KeyTyp,
    cached: &CachedVals,
) -> Option<Result<GPooled<Vec<u8>>>> {
    Some(encode_key(tree_key.key_typ(), cached.0.get(1)?.as_ref()?))
}

pub(crate) fn batch_arg(
    tree: impl KeyTyp,
    cached: &CachedVals,
) -> Option<Result<sled::Batch>> {
    match cached.0.get(1)?.as_ref()? {
        Value::Array(a) => Some(parse_batch_ops(tree.key_typ(), a)),
        v => Some(Err(anyhow!("not a batch: {v}"))),
    }
}

pub(crate) fn insert_arg(
    tree: impl KeyTyp,
    cached: &CachedVals,
) -> Option<Result<(GPooled<Vec<u8>>, GPooled<Vec<u8>>)>> {
    let k = cached.0.get(1)?.as_ref()?;
    let v = cached.0.get(2)?.as_ref()?;
    Some(encode_key(tree.key_typ(), k).and_then(|k| Ok((k, encode_value(v)?))))
}

/// What knows a tree's key type: a tree or a transaction's tree.
pub(crate) trait KeyTyp {
    fn key_typ(&self) -> Option<Typ>;
}

impl KeyTyp for &Arc<TreeInner> {
    fn key_typ(&self) -> Option<Typ> {
        self.key_typ
    }
}

fn many_keys_arg(
    tree: &Arc<TreeInner>,
    cached: &CachedVals,
) -> Option<Result<GPooled<Vec<GPooled<Vec<u8>>>>>> {
    let arr = match cached.0.get(1)?.as_ref()? {
        Value::Array(a) => a,
        v => return Some(Err(anyhow!("not an array of keys: {v}"))),
    };
    let mut keys = ENCODE_MANY_POOL.take();
    for k in arr.iter() {
        match encode_key(tree.key_typ, k) {
            Ok(k) => keys.push(k),
            Err(e) => return Some(Err(e)),
        }
    }
    Some(Ok(keys))
}

type Swap = (GPooled<Vec<u8>>, Option<GPooled<Vec<u8>>>, Option<GPooled<Vec<u8>>>);

fn swap_arg(tree: &Arc<TreeInner>, cached: &CachedVals) -> Option<Result<Swap>> {
    let opt = |i: usize| -> Option<Result<Option<GPooled<Vec<u8>>>>> {
        Some(match cached.0.get(i)?.as_ref()? {
            Value::Null => Ok(None),
            v => encode_value(v).map(Some),
        })
    };
    let key = key_arg(tree, cached)?;
    let (old, new) = (opt(2)?, opt(3)?);
    Some((|| Ok((key?, old?, new?)))())
}

db_op!(DbFlushEv, DbFlush, "db_flush", |db| {
    db.flush()?;
    Ok(Value::Null)
});
db_op!(DbGenerateIdEv, DbGenerateId, "db_generate_id", |db| Ok(Value::U64(
    db.generate_id()?
)));
db_op!(DbSizeOnDiskEv, DbSizeOnDisk, "db_size_on_disk", |db| Ok(Value::U64(
    db.size_on_disk()?
)));
db_op!(DbWasRecoveredEv, DbWasRecovered, "db_was_recovered", |db| Ok(Value::Bool(
    db.was_recovered()
)));
db_op!(DbChecksumEv, DbChecksum, "db_checksum", |db| Ok(Value::U32(db.checksum()?)));
db_op!(DbTreeNamesEv, DbTreeNames, "db_tree_names", |db| {
    let mut names: LPooled<Vec<Value>> = LPooled::take();
    for n in db.tree_names() {
        if !RESERVED.contains(&&*n)
            && let Ok(s) = std::str::from_utf8(&n)
        {
            names.push(Value::String(ArcStr::from(s)));
        }
    }
    Ok(Value::Array(ValArray::from_iter_exact(names.drain(..))))
});

tree_op!(DbFirstEv, DbFirst, "db_first", |t| entry(&t, t.tree.first()?));
tree_op!(DbLastEv, DbLast, "db_last", |t| entry(&t, t.tree.last()?));
tree_op!(DbPopMinEv, DbPopMin, "db_pop_min", |t| entry(&t, t.tree.pop_min()?));
tree_op!(DbPopMaxEv, DbPopMax, "db_pop_max", |t| entry(&t, t.tree.pop_max()?));
tree_op!(DbLenEv, DbLen, "db_len", |t| Ok(Value::U64(t.tree.len() as u64)));
tree_op!(DbIsEmptyEv, DbIsEmpty, "db_is_empty", |t| Ok(Value::Bool(t.tree.is_empty())));

tree_arg_op!(DbGetEv, DbGet, "db_get", |t, k| value_or_null(t.tree.get(&*k)?));
tree_arg_op!(DbRemoveEv, DbRemove, "db_remove", |t, k| value_or_null(
    t.tree.remove(&*k)?
));
tree_arg_op!(DbContainsKeyEv, DbContainsKey, "db_contains_key", |t, k| Ok(Value::Bool(
    t.tree.contains_key(&*k)?
)));
tree_arg_op!(DbGetLtEv, DbGetLt, "db_get_lt", |t, k| entry(&t, t.tree.get_lt(&*k)?));
tree_arg_op!(DbGetGtEv, DbGetGt, "db_get_gt", |t, k| entry(&t, t.tree.get_gt(&*k)?));
tree_arg_op!(
    DbInsertEv,
    DbInsert,
    "db_insert",
    insert_arg,
    (GPooled<Vec<u8>>, GPooled<Vec<u8>>),
    |t, (k, v)| value_or_null(t.tree.insert(&*k, v.as_slice())?)
);
tree_arg_op!(DbBatchEv, DbBatch, "db_batch", batch_arg, sled::Batch, |t, b| {
    t.tree.apply_batch(b)?;
    Ok(Value::Null)
});
tree_arg_op!(
    DbGetManyEv,
    DbGetMany,
    "db_get_many",
    many_keys_arg,
    GPooled<Vec<GPooled<Vec<u8>>>>,
    |t, keys| {
        let mut vals: LPooled<Vec<Value>> = LPooled::take();
        for k in keys.iter() {
            vals.push(value_or_null(t.tree.get(&**k)?)?);
        }
        Ok(Value::Array(ValArray::from_iter_exact(vals.drain(..))))
    }
);
tree_arg_op!(
    DbCompareAndSwapEv,
    DbCompareAndSwap,
    "db_compare_and_swap",
    swap_arg,
    Swap,
    |t, (k, old, new)| {
        let old = old.as_ref().map(|v| v.as_slice());
        let new = new.as_ref().map(|v| v.as_slice());
        match t.tree.compare_and_swap(k.as_slice(), old, new)? {
            Ok(()) => Ok(Value::Null),
            Err(e) => Ok(Value::Array(ValArray::from([
                Value::String(literal!("Mismatch")),
                value_or_null(e.current)?,
            ]))),
        }
    }
);

/// A builtin over a db and a name or path.
macro_rules! db_name_op {
    ($ev:ident, $alias:ident, $name:literal, |$db:ident, $n:ident: $nt:ty| $body:expr) => {
        #[derive(Debug, Default)]
        pub(crate) struct $ev;

        impl EvalCachedAsync for $ev {
            type Args = (Db, $nt);

            const NAME: &str = $name;

            fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
                Some((get_db(cached, 0)?, cached.get::<$nt>(1)?))
            }

            fn eval(($db, $n): Self::Args) -> impl Future<Output = Value> + Send {
                blocking(move || $body)
            }
        }

        graphix_package_core::unit_image_state!($ev);
        pub(crate) type $alias = CachedArgsAsync<$ev>;
    };
}

db_name_op!(DbGetTypeEv, DbGetType, "db_get_type", |db, name: Option<ArcStr>| {
    let key = meta_key(name.as_ref())?;
    match db.open_tree(META_TREE)?.get(key.as_bytes())? {
        None => Ok(Value::Null),
        Some(stored) => {
            let (k, v) = parse_meta(key, &stored)?;
            Ok(Value::Array(ValArray::from([
                Value::String(k.into()),
                Value::String(v.into()),
            ])))
        }
    }
});

db_name_op!(DbDropTreeEv, DbDropTree, "db_drop_tree", |db, name: ArcStr| {
    meta_key(Some(&name))?;
    let existed = db.drop_tree(name.as_bytes())?;
    db.open_tree(META_TREE)?.remove(name.as_bytes())?;
    Ok(Value::Bool(existed))
});

#[derive(Pack)]
struct ExportTree {
    typ: Vec<u8>,
    name: Vec<u8>,
    entries: Vec<Vec<Vec<u8>>>,
}

#[derive(Pack)]
struct ExportData {
    trees: Vec<ExportTree>,
}

db_name_op!(DbExportEv, DbExport, "db_export", |db, path: ArcStr| {
    use std::io::Write;
    let data = ExportData {
        trees: db
            .export()
            .into_iter()
            .map(|(typ, name, iter)| ExportTree { typ, name, entries: iter.collect() })
            .collect(),
    };
    let mut buf = Vec::with_capacity(data.encoded_len());
    data.encode(&mut buf)?;
    let mut w = std::io::BufWriter::new(std::fs::File::create(&*path)?);
    w.write_all(&buf)?;
    w.flush()?;
    Ok(Value::Null)
});

db_name_op!(DbImportEv, DbImport, "db_import", |db, path: ArcStr| {
    let buf = std::fs::read(&*path)?;
    let data = ExportData::decode(&mut buf.as_slice())?;
    let collections: Vec<_> =
        data.trees.into_iter().map(|t| (t.typ, t.name, t.entries.into_iter())).collect();
    // CR claude for claude: [bug/medium] sled's `Db::import` panics instead of
    // returning errors. It asserts that no key it inserts already exists, it `expect`s
    // every insert and every entry's key and value, and it panics on a collection type
    // other than "tree". The export carries graphix's meta tree, so a database that has
    // only opened a tree (no entries, so empty as the gxi demands) collides on that
    // tree's meta key. The task panics, the panic is printed on stderr, and the caller
    // gets `DbErr "task panicked: ..."`. Every entry inserted before the collision
    // stays written. In the probe, the meta entry of a tree the destination never
    // opened is stored with no data, so opening that tree later fixes its type to what
    // the file said. A crafted or corrupt file panics the same way. Fix: never hand
    // file input to sled's import. Validate the collection type and entry shape, refuse
    // an existing key with a DbErr (or skip an identical meta entry), and check every
    // key before writing anything, so a refused import writes nothing. Also state
    // precisely in the gxi what "empty" means. probe:
    // design/review-2026-10-12/repro/http-sqlite-db1-07.gx (http-sqlite-db1-07)
    // CR claude for claude: [bug/medium] sled's `Db::import` asserts that no key it
    // writes already exists, and it expects and panics on a malformed file (an entry
    // that is not [k, v], or a collection type other than "tree"). Here we call it on
    // whatever the file decodes to. An export carries the meta tree, so a destination
    // where the program has only called `db::tree(dst, "a")` already collides. The
    // import then panics partway: `blocking` reports DbErr("task panicked: ...import is
    // overwriting existing data"), the panic is printed on stderr, and the trees
    // imported before the collision stay imported (probe:
    // design/review-2026-10-12/repro/db2-06.gx prints a=0 b=1 c=1 beside the error).
    // Fix: validate ExportData before writing (every entry is [k, v], every typ is
    // "tree"), and refuse with a clean DbErr when any destination tree, the meta tree
    // included, already holds a key the import would write. Alternatively, do the
    // inserts ourselves in one transaction so a refusal changes nothing. Then the
    // .gxi's "must be empty" should say what counts as empty. (db2-06)
    db.import(collections);
    Ok(Value::Null)
});

#[derive(Debug, Default)]
pub(crate) struct DbOpenEv;

impl EvalCachedAsync for DbOpenEv {
    type Args = ArcStr;

    const NAME: &str = "db_open";

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        cached.get::<ArcStr>(0)
    }

    fn eval(path: Self::Args) -> impl Future<Output = Value> + Send {
        blocking(move || Ok(DB_WRAPPER.wrap(DbValue { inner: open_db(&path)? })))
    }
}

graphix_package_core::unit_image_state!(DbOpenEv);
pub(crate) type DbOpen = CachedArgsAsync<DbOpenEv>;

/// How a tree opener reaches its tree: `db::tree` from a database,
/// `db::txn::tree` from a transaction.
pub(crate) trait TreeOpener: fmt::Debug + Send + Sync + 'static {
    const NAME: &str;
    type Handle: fmt::Debug + Send + Sync + 'static;

    fn handle(cached: &CachedVals) -> Option<Self::Handle>;

    fn open(
        h: Self::Handle,
        name: Option<ArcStr>,
        types: TreeTypes,
    ) -> impl Future<Output = Value> + Send;
}

#[derive(Debug)]
pub(crate) struct OpenTreeEv<O> {
    types: TreeTypes,
    opener: PhantomData<O>,
}

impl<O> Default for OpenTreeEv<O> {
    fn default() -> Self {
        Self { types: TreeTypes::default(), opener: PhantomData }
    }
}

impl<O: TreeOpener> ImageState for OpenTreeEv<O> {
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        self.types.image_encode(buf)
    }

    fn image_decode<R: Rt, E: UserEvent>(
        ctx: &mut ExecCtx<'_, R, E>,
        buf: &mut &[u8],
    ) -> Result<Self, PackError> {
        Ok(Self { types: TreeTypes::image_decode(ctx, buf)?, opener: PhantomData })
    }
}

impl<O: TreeOpener> EvalCachedAsync for OpenTreeEv<O> {
    type Args = (O::Handle, Option<ArcStr>, TreeTypes);

    const NAME: &str = O::NAME;

    fn init<R: Rt, E: UserEvent>(
        ctx: &mut CompileCtx<R, E>,
        _typ: &FnType,
        resolved: Option<&FnType>,
        _scope: &Scope,
        _from: &[Node<R, E>],
        _top_id: ExprId,
    ) -> Self {
        Self { types: TreeTypes::of(resolved, &ctx.env), opener: PhantomData }
    }

    fn typecheck1<R: Rt, E: UserEvent>(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        _from: &mut [Node<R, E>],
        resolved: &FnType,
    ) -> Result<()> {
        self.types = TreeTypes::of(Some(resolved), &ctx.env);
        Ok(())
    }

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        let h = O::handle(cached)?;
        let name = cached.get::<Option<ArcStr>>(1)?;
        Some((h, name, self.types.clone()))
    }

    fn eval((h, name, types): Self::Args) -> impl Future<Output = Value> + Send {
        O::open(h, name, types)
    }
}

#[derive(Debug)]
pub(crate) struct FromDb;

impl TreeOpener for FromDb {
    const NAME: &str = "db_tree";
    type Handle = Db;

    fn handle(cached: &CachedVals) -> Option<Db> {
        get_db(cached, 0)
    }

    fn open(
        db: Db,
        name: Option<ArcStr>,
        types: TreeTypes,
    ) -> impl Future<Output = Value> {
        blocking(move || {
            types.concrete()?;
            let key = meta_key(name.as_ref())?;
            db.open_tree(META_TREE)?.check_or_store(key, &types)?;
            let tree = match name {
                None => (***db).clone(),
                Some(n) => db.open_tree(n.as_bytes())?,
            };
            Ok(wrap_tree(tree, types.key))
        })
    }
}

pub(crate) type DbTree = CachedArgsAsync<OpenTreeEv<FromDb>>;
