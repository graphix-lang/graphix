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
    rep,
    typ::{FnType, Type},
};
use graphix_package_core::{CachedArgsAsync, CachedVals, EvalCachedAsync, ImageState};
use netidx::{path::Path, publisher::Typ};
use netidx_core::pack::{Pack, PackError};
use netidx_derive::Pack;
use netidx_value::{ValArray, Value};
use parking_lot::Mutex;
use poolshark::{global::GPooled, local::LPooled};
use smallvec::SmallVec;
use std::{
    cell::RefCell,
    fmt,
    marker::PhantomData,
    ops::Deref,
    path::PathBuf,
    pin::pin,
    sync::{Arc, LazyLock, Weak},
};

use crate::{
    encoding::{
        ENCODE_MANY_POOL, decode_key, decode_value, encode_key, encode_value,
        for_batch_ops,
    },
    watch::{self, Changes, Watches},
};

use sled::transaction::{ConflictableTransactionError, TransactionError, Transactional};
use tokio::sync::Notify;

pub(crate) use graphix_package_core::Reaped;

pub(crate) struct Database {
    db: sled::Db,
    /// Held while a tree is created with its types or dropped, so a
    /// transaction's commit that created a tree and failed drops only one
    /// nothing else has claimed.
    pub(crate) creating: Mutex<()>,
    /// The watches on each tree, by its name, while anything holds them.
    watches: Mutex<AHashMap<sled::IVec, Weak<Watches>>>,
    // The database drops before waiting openers are notified.
    _registration: Registration,
}

impl Deref for Database {
    type Target = sled::Db;

    fn deref(&self) -> &Self::Target {
        &self.db
    }
}

impl Database {
    /// The watches on `tree`.
    pub(crate) fn watches(&self, tree: &sled::Tree) -> Arc<Watches> {
        let mut all = self.watches.lock();
        let name = tree.name();
        if let Some(w) = all.get(&name).and_then(Weak::upgrade) {
            return w;
        }
        all.retain(|_, w| w.strong_count() > 0);
        let w = Arc::new(Watches::default());
        all.insert(name, Arc::downgrade(&w));
        w
    }

    /// End the watches on a dropped tree: one made later under its name
    /// starts with none.
    fn forget_watches(&self, name: &[u8]) {
        self.watches.lock().remove(name);
    }
}

struct Registration(PathBuf);

impl Drop for Registration {
    fn drop(&mut self) {
        OPEN.dbs.lock().remove(&self.0);
        OPEN.closed.notify_waiters();
    }
}

pub(crate) type Db = Arc<Reaped<Database>>;

#[derive(Debug, Clone)]
pub struct DbValue {
    pub(crate) inner: Db,
}

graphix_package_core::impl_abstract_arc!(
    DbValue,
    pub(crate) static DB_WRAPPER = "db::Db"
);

pub(crate) fn get_db(cached: &CachedVals, idx: usize) -> Option<Db> {
    cached.abstract_arg::<DbValue>(idx).map(|d| d.inner.clone())
}

/// One sled handle per database in the process: sled locks a database's
/// files while any handle lives, so a second open would fail.
struct OpenDbs {
    dbs: Mutex<AHashMap<PathBuf, Weak<Reaped<Database>>>>,
    closed: Notify,
}

static OPEN: LazyLock<OpenDbs> =
    LazyLock::new(|| OpenDbs { dbs: Mutex::new(AHashMap::new()), closed: Notify::new() });

fn try_open_db(path: &str) -> Result<Option<Db>> {
    let mut open = OPEN.dbs.lock();
    if let Ok(path) = std::fs::canonicalize(path)
        && let Some(db) = open.get(&path)
    {
        return Ok(db.upgrade());
    }
    let db = sled::open(path)?;
    let path = std::fs::canonicalize(path)?;
    let db = Arc::new(Reaped::new(Database {
        db,
        creating: Mutex::new(()),
        watches: Mutex::new(AHashMap::new()),
        _registration: Registration(path.clone()),
    }));
    open.insert(path, Arc::downgrade(&db));
    Ok(Some(db))
}

pub(super) async fn open_db(path: ArcStr) -> Result<Db> {
    loop {
        let mut closed = pin!(OPEN.closed.notified());
        closed.as_mut().enable();
        let path = path.clone();
        match tokio::task::spawn_blocking(move || try_open_db(&path)).await?? {
            Some(db) => return Ok(db),
            None => closed.await,
        }
    }
}

#[derive(Debug)]
pub(crate) struct TreeInner {
    pub(crate) tree: sled::Tree,
    pub(crate) key_typ: Option<Typ>,
    pub(crate) watches: Arc<Watches>,
    _db: Db,
}

impl TreeInner {
    /// Write to the tree with `f`, which records what it changed as tree 0.
    fn write<T>(
        &self,
        f: impl FnOnce(&sled::Tree, &mut Changes) -> Result<T>,
    ) -> Result<T> {
        watch::write(&[&self.watches], |changes| f(&self.tree, changes))
    }

    pub(crate) fn insert(&self, key: &[u8], value: &[u8]) -> Result<Option<sled::IVec>> {
        self.write(|tree, changes| {
            let old = tree.insert(key, value)?;
            changes.record(0, key, old.as_deref(), Some(value));
            Ok(old)
        })
    }

    pub(crate) fn remove(&self, key: &[u8]) -> Result<Option<sled::IVec>> {
        self.write(|tree, changes| {
            let old = tree.remove(key)?;
            changes.record(0, key, old.as_deref(), None);
            Ok(old)
        })
    }

    /// Remove the least key's entry, or the greatest's when `max`.
    pub(crate) fn pop(&self, max: bool) -> Result<Option<(sled::IVec, sled::IVec)>> {
        self.write(|tree, changes| {
            let popped = if max { tree.pop_max()? } else { tree.pop_min()? };
            if let Some((k, v)) = &popped {
                changes.record(0, k, Some(v), None);
            }
            Ok(popped)
        })
    }

    pub(crate) fn compare_and_swap(
        &self,
        key: &[u8],
        old: Option<&[u8]>,
        new: Option<&[u8]>,
    ) -> Result<Result<(), sled::CompareAndSwapError>> {
        self.write(|tree, changes| {
            let res = tree.compare_and_swap(key, old, new)?;
            if res.is_ok() {
                changes.record(0, key, old, new);
            }
            Ok(res)
        })
    }

    /// Apply `writes` in order, atomically.
    pub(crate) fn apply_batch(&self, writes: &Writes) -> Result<()> {
        self.write(|tree, changes| {
            if changes.wanted() {
                let mut now: LPooled<AHashMap<sled::IVec, Option<sled::IVec>>> =
                    LPooled::take();
                for (k, v) in writes.iter() {
                    let old = match now.get(k) {
                        Some(old) => old.clone(),
                        None => tree.get(k)?,
                    };
                    changes.record(0, k, old.as_deref(), v.as_deref());
                    now.insert(k.clone(), v.clone());
                }
            }
            let mut batch = sled::Batch::default();
            for (k, v) in writes.iter() {
                match v {
                    Some(v) => batch.insert(k.clone(), v.clone()),
                    None => batch.remove(k.clone()),
                }
            }
            if let Err(e) = tree.apply_batch(batch) {
                changes.clear();
                return Err(e.into());
            }
            Ok(())
        })
    }
}

pub(crate) type Tree = Arc<Reaped<TreeInner>>;

#[derive(Debug, Clone)]
pub struct TreeValue {
    pub(crate) inner: Tree,
}

graphix_package_core::impl_abstract_arc!(
    TreeValue,
    pub(crate) static TREE_WRAPPER = "db::Tree"
);

pub(crate) fn get_tree_inner(cached: &CachedVals, idx: usize) -> Option<Tree> {
    cached.abstract_arg::<TreeValue>(idx).map(|t| t.inner.clone())
}

pub(crate) fn tree_inner(db: Db, tree: sled::Tree, key_typ: Option<Typ>) -> Tree {
    let watches = db.watches(&tree);
    Arc::new(Reaped::new(TreeInner { tree, key_typ, watches, _db: db }))
}

pub(super) fn wrap_tree(db: Db, tree: sled::Tree, key_typ: Option<Typ>) -> Value {
    TREE_WRAPPER.wrap(TreeValue { inner: tree_inner(db, tree, key_typ) })
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
        Some((k, v)) => {
            Ok(rep::tuple([decode_key(tree.key_typ, &k)?, decode_value(&v)?]))
        }
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

    #[cfg(test)]
    pub(crate) fn string_to_string() -> Self {
        Self {
            key: Some(Typ::String),
            key_str: "string".into(),
            val_str: "string".into(),
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

macro_rules! blocking_op {
    ($ev:ident, $alias:ident, $name:literal, $args:ty,
     |$cached:ident| $prepare:expr, |$arg:pat_param| $body:expr) => {
        #[derive(Debug, Default)]
        pub(crate) struct $ev;

        impl EvalCachedAsync for $ev {
            type Args = $args;
            const NAME: &str = $name;
            // every argument past the handle is a key or a value
            const DATA_ARGS: &[usize] = &[1, 2, 3, 4];
            const DATA_RESULT: bool = true;

            fn prepare_args(&mut self, $cached: &CachedVals) -> Option<Self::Args> {
                $prepare
            }

            fn eval($arg: Self::Args) -> impl Future<Output = Value> + Send {
                blocking(move || $body)
            }
        }

        graphix_package_core::unit_image_state!($ev);
        pub(crate) type $alias = CachedArgsAsync<$ev>;
    };
}

pub(crate) use blocking_op;

/// A builtin over a db, its work on a blocking thread.
macro_rules! db_op {
    ($ev:ident, $alias:ident, $name:literal, |$db:ident| $body:expr) => {
        blocking_op!($ev, $alias, $name, Db, |cached| get_db(cached, 0), |$db| $body);
    };
}

/// A builtin over a tree, its work on a blocking thread.
macro_rules! tree_op {
    ($ev:ident, $alias:ident, $name:literal, |$t:ident| $body:expr) => {
        blocking_op!(
            $ev,
            $alias,
            $name,
            Tree,
            |cached| get_tree_inner(cached, 0),
            |$t| $body
        );
    };
}

/// A builtin over a tree and the encoding of its second argument (a key
/// unless `$enc` says otherwise); a failed encoding is the builtin's DbErr.
macro_rules! tree_arg_op {
    ($ev:ident, $alias:ident, $name:literal, $enc:expr, $n:literal,
     |$t:ident, $k:pat_param| $body:expr) => {
        blocking_op!(
            $ev,
            $alias,
            $name,
            (Tree, [Value; $n]),
            |cached| Some((get_tree_inner(cached, 0)?, raw_args(cached)?)),
            |($t, args)| {
                let $k = $enc(&$t, args)?;
                $body
            }
        );
    };
    ($ev:ident, $alias:ident, $name:literal, |$t:ident, $k:pat_param| $body:expr) => {
        tree_arg_op!($ev, $alias, $name, key_arg, 1, |$t, $k| $body);
    };
}

pub(crate) fn raw_args<const N: usize>(cached: &CachedVals) -> Option<[Value; N]> {
    let mut args = [const { Value::Null }; N];
    for (i, arg) in args.iter_mut().enumerate() {
        *arg = cached.value(i + 1)?.clone();
    }
    Some(args)
}

pub(crate) fn key_arg(
    tree_key: impl KeyTyp,
    [key]: [Value; 1],
) -> Result<GPooled<Vec<u8>>> {
    encode_key(tree_key.key_typ(), &key)
}

/// A batch's writes in order, each a key and the value it writes (`None`
/// removes).
pub(crate) type Writes = LPooled<Vec<(sled::IVec, Option<sled::IVec>)>>;

pub(crate) fn writes_arg(tree: impl KeyTyp, [batch]: [Value; 1]) -> Result<Writes> {
    match batch {
        Value::Array(a) => {
            let mut writes: Writes = LPooled::take();
            for_batch_ops(tree.key_typ(), &a, |k, v| {
                writes.push((sled::IVec::from(k), v.map(sled::IVec::from)))
            })?;
            Ok(writes)
        }
        v => bail!("not a batch: {}", graphix_compiler::typ::Naked(&v)),
    }
}

pub(crate) fn insert_arg(
    tree: impl KeyTyp,
    [key, value]: [Value; 2],
) -> Result<(GPooled<Vec<u8>>, GPooled<Vec<u8>>)> {
    Ok((encode_key(tree.key_typ(), &key)?, encode_value(&value)?))
}

/// What knows a tree's key type: a tree or a transaction's tree.
pub(crate) trait KeyTyp {
    fn key_typ(&self) -> Option<Typ>;
}

impl KeyTyp for &Tree {
    fn key_typ(&self) -> Option<Typ> {
        self.key_typ
    }
}

fn many_keys_arg(
    tree: &Tree,
    [keys]: [Value; 1],
) -> Result<GPooled<Vec<GPooled<Vec<u8>>>>> {
    let Value::Array(arr) = keys else { bail!("not an array of keys: {keys}") };
    let mut keys = ENCODE_MANY_POOL.take();
    for key in arr.iter() {
        keys.push(encode_key(tree.key_typ, key)?);
    }
    Ok(keys)
}

type Swap = (GPooled<Vec<u8>>, Option<GPooled<Vec<u8>>>, Option<GPooled<Vec<u8>>>);

fn swap_arg(tree: &Tree, [key, old, new]: [Value; 3]) -> Result<Swap> {
    let opt = |v: &Value| match v {
        Value::Null => Ok(None),
        v => encode_value(v).map(Some),
    };
    Ok((encode_key(tree.key_typ, &key)?, opt(&old)?, opt(&new)?))
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
tree_op!(DbPopMinEv, DbPopMin, "db_pop_min", |t| entry(&t, t.pop(false)?));
tree_op!(DbPopMaxEv, DbPopMax, "db_pop_max", |t| entry(&t, t.pop(true)?));
tree_op!(DbLenEv, DbLen, "db_len", |t| Ok(Value::U64(t.tree.len() as u64)));
tree_op!(DbIsEmptyEv, DbIsEmpty, "db_is_empty", |t| Ok(Value::Bool(t.tree.is_empty())));

tree_arg_op!(DbGetEv, DbGet, "db_get", |t, k| value_or_null(t.tree.get(&*k)?));
tree_arg_op!(DbRemoveEv, DbRemove, "db_remove", |t, k| value_or_null(t.remove(&k)?));
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
    2,
    |t, (k, v)| value_or_null(t.insert(&k, &v)?)
);
tree_arg_op!(DbBatchEv, DbBatch, "db_batch", writes_arg, 1, |t, writes| {
    t.apply_batch(&writes)?;
    Ok(Value::Null)
});
tree_arg_op!(DbGetManyEv, DbGetMany, "db_get_many", many_keys_arg, 1, |t, keys| {
    let mut vals: LPooled<Vec<Value>> = LPooled::take();
    for k in keys.iter() {
        vals.push(value_or_null(t.tree.get(&**k)?)?);
    }
    Ok(Value::Array(ValArray::from_iter_exact(vals.drain(..))))
});
tree_arg_op!(
    DbCompareAndSwapEv,
    DbCompareAndSwap,
    "db_compare_and_swap",
    swap_arg,
    3,
    |t, (k, old, new)| {
        let old = old.as_ref().map(|v| v.as_slice());
        let new = new.as_ref().map(|v| v.as_slice());
        match t.compare_and_swap(&k, old, new)? {
            Ok(()) => Ok(Value::Null),
            Err(e) => {
                Ok(rep::make_variant(literal!("Mismatch"), [value_or_null(e.current)?]))
            }
        }
    }
);

/// A builtin over a db and a name or path.
macro_rules! db_name_op {
    ($ev:ident, $alias:ident, $name:literal, |$db:ident, $n:ident: $nt:ty| $body:expr) => {
        blocking_op!(
            $ev,
            $alias,
            $name,
            (Db, $nt),
            |cached| Some((get_db(cached, 0)?, cached.get::<$nt>(1)?)),
            |($db, $n)| $body
        );
    };
}

db_name_op!(DbGetTypeEv, DbGetType, "db_get_type", |db, name: Option<ArcStr>| {
    let key = meta_key(name.as_ref())?;
    match db.open_tree(META_TREE)?.get(key.as_bytes())? {
        None => Ok(Value::Null),
        Some(stored) => {
            let (k, v) = parse_meta(key, &stored)?;
            Ok(rep::tuple([Value::String(k.into()), Value::String(v.into())]))
        }
    }
});

db_name_op!(DbDropTreeEv, DbDropTree, "db_drop_tree", |db, name: ArcStr| {
    meta_key(Some(&name))?;
    let _creating = db.creating.lock();
    let existed = db.drop_tree(name.as_bytes())?;
    // CR claude for eric: [bug/medium] Dropping a tree ends its subscriptions silently.
    // The registered watches keep the old Watches, which nothing writes to again: old
    // handles fail, and a tree re-created under the name has fresh watches. No remove
    // and no error reaches on_change/on_insert/on_remove, so a copy built from
    // on_change keeps the dropped keys forever, contrary to the subscription gxi.
    // Probe: after a drop and re-create, the copy held a=1, get("a") was null, and the
    // new insert of b was never heard. Decide what a drop means to a subscription.
    // Options: an end/error item on the watch channel, so each accessor yields `DbErr`
    // once and the subscription ends, or a Remove per key. Then document it in
    // drop_tree's and subscription::new's docs. probe:
    // design/review-2026-10-19/repro/db2-drop-tree-silences-subscription.gx (db2-02)
    db.forget_watches(name.as_bytes());
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

struct ImportTree {
    name: Vec<u8>,
    entries: LPooled<Vec<[Vec<u8>; 2]>>,
}

fn import_data(db: &Database, data: ExportData) -> Result<()> {
    if data.trees.is_empty() {
        return Ok(());
    }
    let data = data
        .trees
        .into_iter()
        .map(|tree| {
            if tree.typ != b"tree" {
                bail!("invalid import collection type")
            }
            let entries = tree
                .entries
                .into_iter()
                .map(|entry| {
                    entry.try_into().map_err(|_| {
                        anyhow!("invalid import entry: expected key and value")
                    })
                })
                .collect::<Result<LPooled<Vec<_>>>>()?;
            Ok(ImportTree { name: tree.name, entries })
        })
        .collect::<Result<LPooled<Vec<_>>>>()?;
    let mut order: LPooled<Vec<usize>> = (0..data.len()).collect();
    order.sort_unstable_by(|&a, &b| data[a].name.cmp(&data[b].name));
    if order.windows(2).any(|pair| data[pair[0]].name == data[pair[1]].name) {
        bail!("duplicate import collection")
    }
    for tree in &*data {
        order.clear();
        order.extend(0..tree.entries.len());
        order.sort_unstable_by(|&a, &b| tree.entries[a][0].cmp(&tree.entries[b][0]));
        if order
            .windows(2)
            .any(|pair| tree.entries[pair[0]][0] == tree.entries[pair[1]][0])
        {
            bail!("duplicate import key")
        }
    }
    let existing: LPooled<Vec<_>> = db.tree_names().into_iter().collect();
    for tree in &*data {
        if existing.iter().any(|name| name.as_ref() == tree.name) {
            let dest = db.open_tree(&tree.name)?;
            for [key, _] in &*tree.entries {
                if dest.contains_key(key)? {
                    bail!("import would overwrite an existing key")
                }
            }
        }
    }
    let trees = data
        .iter()
        .map(|tree| db.open_tree(&tree.name))
        .collect::<sled::Result<LPooled<Vec<_>>>>()?;
    let watches: LPooled<Vec<Arc<Watches>>> =
        trees.iter().map(|t| db.watches(t)).collect();
    let locked: SmallVec<[&Watches; 4]> = watches.iter().map(|w| &**w).collect();
    watch::write(&locked, |changes| {
        let changes = RefCell::new(changes);
        let res = trees.as_slice().transaction(|tx| {
            changes.borrow_mut().clear();
            for (i, (src, dest)) in data.iter().zip(tx).enumerate() {
                for [key, value] in &*src.entries {
                    if dest.get(key)?.is_some() {
                        return Err(ConflictableTransactionError::Abort(anyhow!(
                            "import would overwrite an existing key"
                        )));
                    }
                    dest.insert(key.as_slice(), value.as_slice())?;
                    changes.borrow_mut().record(i, key, None, Some(value));
                }
            }
            Ok(())
        });
        if res.is_err() {
            changes.borrow_mut().clear();
        }
        match res {
            Ok(()) => Ok(()),
            Err(TransactionError::Abort(e)) => Err(e),
            Err(TransactionError::Storage(e)) => Err(e.into()),
        }
    })
}

db_name_op!(DbImportEv, DbImport, "db_import", |db, path: ArcStr| {
    let buf = std::fs::read(&*path)?;
    let data = ExportData::decode(&mut buf.as_slice())?;
    import_data(&db, data)?;
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

    async fn eval(path: Self::Args) -> Value {
        match open_db(path).await {
            Ok(inner) => DB_WRAPPER.wrap(DbValue { inner }),
            Err(e) => errf!("DbErr", "{e:#}"),
        }
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
            let tree = {
                let _creating = db.creating.lock();
                db.open_tree(META_TREE)?.check_or_store(key, &types)?;
                match name {
                    None => (*db.db).clone(),
                    Some(n) => db.open_tree(n.as_bytes())?,
                }
            };
            Ok(wrap_tree(db, tree, types.key))
        })
    }
}

pub(crate) type DbTree = CachedArgsAsync<OpenTreeEv<FromDb>>;

#[cfg(test)]
pub(crate) mod test {
    use super::*;
    use std::{
        sync::mpsc,
        thread::{self, ThreadId},
        time::Duration,
    };

    pub(crate) fn cached<const N: usize>(values: [Value; N]) -> CachedVals {
        CachedVals::from_values(values.map(Some))
    }

    static ENCODED_ON: Mutex<Option<ThreadId>> = Mutex::new(None);

    fn probe_key_arg(tree: &Tree, args: [Value; 1]) -> Result<GPooled<Vec<u8>>> {
        *ENCODED_ON.lock() = Some(thread::current().id());
        key_arg(tree, args)
    }

    tree_arg_op!(
        EncodingProbeEv,
        EncodingProbe,
        "db_encoding_probe",
        probe_key_arg,
        1,
        |tree, key| value_or_null(tree.tree.get(&*key)?)
    );

    #[tokio::test]
    async fn tree_encoding_runs_on_a_blocking_thread() -> Result<()> {
        let dir = tempfile::tempdir()?;
        let db = open_db(dir.path().to_str().unwrap().into()).await?;
        let tree = wrap_tree(db.clone(), db.open_tree("t")?, Some(Typ::String));
        let args =
            EncodingProbeEv.prepare_args(&cached([tree, Value::from("key")])).unwrap();
        assert!(ENCODED_ON.lock().is_none(), "prepare_args must not encode");
        assert_eq!(EncodingProbeEv::eval(args).await, Value::Null);
        assert_ne!(*ENCODED_ON.lock(), Some(thread::current().id()));
        assert!(ENCODED_ON.lock().is_some());
        let _: Option<EncodingProbe> = None;
        Ok(())
    }

    #[tokio::test]
    async fn trees_keep_the_database_registered() -> Result<()> {
        let dir = tempfile::tempdir()?;
        let path = dir.path().to_str().unwrap();
        let db = open_db(path.into()).await?;
        let weak = Arc::downgrade(&db);
        let tree = wrap_tree(db.clone(), db.open_tree("t")?, Some(Typ::String));
        drop(db);
        let owner = weak.upgrade().expect("tree must retain the registered database");
        assert!(Arc::ptr_eq(&owner, &open_db(path.into()).await?));
        drop(tree);
        Ok(())
    }

    fn exported(name: &[u8], entries: &[(&[u8], &[u8])]) -> ExportTree {
        ExportTree {
            typ: b"tree".to_vec(),
            name: name.to_vec(),
            entries: entries.iter().map(|(k, v)| vec![k.to_vec(), v.to_vec()]).collect(),
        }
    }

    async fn import_result(db: Db, trees: Vec<ExportTree>) -> Value {
        let file = tempfile::NamedTempFile::new().unwrap();
        let mut buf: LPooled<Vec<u8>> = LPooled::take();
        ExportData { trees }.encode(&mut *buf).unwrap();
        std::fs::write(file.path(), &*buf).unwrap();
        DbImportEv::eval((db, file.path().to_str().unwrap().into())).await
    }

    fn assert_import_error(value: Value) {
        let Value::Error(e) = value else { panic!("expected DbErr, got {value}") };
        let Some((tag, [Value::String(msg)])) = graphix_compiler::rep::variant(&e) else {
            panic!("expected tagged error text")
        };
        assert_eq!(tag, "DbErr");
        assert!(!msg.contains("panicked"), "import panicked: {msg}");
    }

    #[tokio::test]
    async fn import_accepts_empty_archives() -> Result<()> {
        let dir = tempfile::tempdir()?;
        let db = open_db(dir.path().to_str().unwrap().into()).await?;
        let checksum = db.checksum()?;
        assert_eq!(import_result(db.clone(), vec![]).await, Value::Null);
        assert_eq!(db.checksum()?, checksum);
        Ok(())
    }

    #[tokio::test]
    async fn malformed_import_leaves_database_unchanged() -> Result<()> {
        let dir = tempfile::tempdir()?;
        let db = open_db(dir.path().to_str().unwrap().into()).await?;
        let checksum = db.checksum()?;
        for invalid in [
            ExportTree {
                typ: b"unknown".to_vec(),
                name: b"bad".to_vec(),
                entries: vec![],
            },
            ExportTree {
                typ: b"tree".to_vec(),
                name: b"bad".to_vec(),
                entries: vec![vec![b"key".to_vec()]],
            },
            exported(b"bad", &[(b"same", b"1"), (b"same", b"2")]),
            exported(b"new", &[(b"other", b"value")]),
        ] {
            assert_import_error(
                import_result(
                    db.clone(),
                    vec![exported(b"new", &[(b"k", b"v")]), invalid],
                )
                .await,
            );
            assert_eq!(db.checksum()?, checksum);
            assert!(!db.tree_names().iter().any(|n| &**n == b"new"));
        }
        Ok(())
    }

    #[tokio::test]
    async fn colliding_import_leaves_database_unchanged() -> Result<()> {
        let dir = tempfile::tempdir()?;
        let db = open_db(dir.path().to_str().unwrap().into()).await?;
        db.open_tree(META_TREE)?.insert(b"existing", b"2\0string\0i64")?;
        let checksum = db.checksum()?;
        assert_import_error(
            import_result(
                db.clone(),
                vec![
                    exported(b"new", &[(b"k", b"v")]),
                    exported(META_TREE, &[(b"existing", b"2\0string\0i64")]),
                ],
            )
            .await,
        );
        assert_eq!(db.checksum()?, checksum);
        assert!(!db.tree_names().iter().any(|n| &**n == b"new"));
        Ok(())
    }

    #[test]
    fn reopening_waits_for_deferred_close() -> Result<()> {
        let dir = tempfile::tempdir()?;
        let path = ArcStr::from(dir.path().to_str().unwrap());
        let runtime = || {
            tokio::runtime::Builder::new_current_thread()
                .max_blocking_threads(1)
                .enable_time()
                .build()
        };
        let reaper = runtime()?;
        let opener = runtime()?;
        let db = opener.block_on(open_db(path.clone()))?;
        let (started_tx, started) = mpsc::channel();
        let (release, released) = mpsc::channel();
        reaper.spawn_blocking(move || {
            started_tx.send(()).unwrap();
            released.recv().unwrap();
        });
        started.recv_timeout(Duration::from_secs(5))?;
        {
            let _enter = reaper.enter();
            drop(db);
        }
        let pending = try_open_db(&path);
        opener.block_on(async move {
            let mut reopened = pin!(open_db(path));
            let waiting =
                tokio::time::timeout(Duration::from_millis(30), &mut reopened).await;
            let release_copy = release.clone();
            let progress = tokio::time::timeout(
                Duration::from_secs(1),
                tokio::task::spawn_blocking(move || release_copy.send(())),
            )
            .await;
            let _ = release.send(());
            assert!(waiting.is_err(), "reopen must wait for the old database to close");
            assert!(
                progress.is_ok(),
                "waiting for close must not occupy a blocking worker"
            );
            assert!(pending?.is_none());
            let db = tokio::time::timeout(Duration::from_secs(5), reopened).await??;
            drop(db);
            Ok::<_, anyhow::Error>(())
        })?;
        Ok(())
    }
}
