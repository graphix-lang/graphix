#![doc(
    html_logo_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg",
    html_favicon_url = "https://graphix-lang.github.io/graphix/graphix-icon.svg"
)]
use anyhow::Result;
use arcstr::ArcStr;
use futures::{channel::mpsc, SinkExt};
use fxhash::FxHashSet;
use graphix_compiler::{
    errf,
    expr::ExprId,
    typ::{FnType, Type},
    Apply, BindId, BuiltIn, CustomBuiltinType, Event, ExecCtx, Node, Rt, Scope,
    UserEvent, CBATCH_POOL,
};
use graphix_package_core::{CachedArgsAsync, CachedVals, EvalCachedAsync};
use netidx::publisher::Typ;
use netidx_core::pack::Pack;
use netidx_value::{abstract_type::AbstractWrapper, Abstract, ValArray, Value};
use poolshark::{global::GPooled, local::LPooled};
use std::{
    any::Any,
    cmp::Ordering,
    fmt,
    hash::{Hash, Hasher},
    sync::{Arc, LazyLock},
};

// ── Value encoding helpers ───────────────────────────────────────

fn encode_value(v: &Value) -> Option<GPooled<Vec<u8>>> {
    static POOL: LazyLock<poolshark::global::Pool<Vec<u8>>> =
        LazyLock::new(|| poolshark::global::Pool::new(64, 4096));
    let len = v.encoded_len();
    let mut buf = POOL.take();
    buf.reserve(len);
    v.encode(&mut *buf).ok()?;
    Some(buf)
}

fn decode_value(data: &[u8]) -> Option<Value> {
    Value::decode(&mut &*data).ok()
}

// ── Key encoding ─────────────────────────────────────────────────
//
// Order-preserving raw encoding for primitive key types:
//   String  → raw UTF-8 bytes
//   Bytes   → raw bytes
//   Unsigned integers → fixed-width big-endian
//   Signed integers   → fixed-width big-endian with sign-bit XOR
//   Everything else   → Pack encoding (works as keys, no ordering)

fn encode_key(key_typ: Option<Typ>, v: &Value) -> Option<GPooled<Vec<u8>>> {
    static POOL: LazyLock<poolshark::global::Pool<Vec<u8>>> =
        LazyLock::new(|| poolshark::global::Pool::new(64, 4096));
    match key_typ {
        Some(Typ::String) => match v {
            Value::String(s) => {
                let mut buf = POOL.take();
                buf.extend_from_slice(s.as_bytes());
                Some(buf)
            }
            _ => None,
        },
        Some(Typ::Bytes) => match v {
            Value::Bytes(b) => {
                let mut buf = POOL.take();
                buf.extend_from_slice(&**b);
                Some(buf)
            }
            _ => None,
        },
        Some(Typ::U8) => match v {
            Value::U8(n) => {
                let mut buf = POOL.take();
                buf.push(*n);
                Some(buf)
            }
            _ => None,
        },
        Some(Typ::I8) => match v {
            Value::I8(n) => {
                let mut buf = POOL.take();
                buf.push((*n as u8) ^ 0x80);
                Some(buf)
            }
            _ => None,
        },
        Some(Typ::U16) => match v {
            Value::U16(n) => {
                let mut buf = POOL.take();
                buf.extend_from_slice(&n.to_be_bytes());
                Some(buf)
            }
            _ => None,
        },
        Some(Typ::I16) => match v {
            Value::I16(n) => {
                let mut buf = POOL.take();
                let raw = (*n as u16) ^ 0x8000;
                buf.extend_from_slice(&raw.to_be_bytes());
                Some(buf)
            }
            _ => None,
        },
        Some(Typ::U32) => match v {
            Value::U32(n) => {
                let mut buf = POOL.take();
                buf.extend_from_slice(&n.to_be_bytes());
                Some(buf)
            }
            _ => None,
        },
        Some(Typ::V32) => match v {
            Value::V32(n) => {
                let mut buf = POOL.take();
                buf.extend_from_slice(&n.to_be_bytes());
                Some(buf)
            }
            _ => None,
        },
        Some(Typ::I32) => match v {
            Value::I32(n) => {
                let mut buf = POOL.take();
                let raw = (*n as u32) ^ 0x8000_0000;
                buf.extend_from_slice(&raw.to_be_bytes());
                Some(buf)
            }
            _ => None,
        },
        Some(Typ::Z32) => match v {
            Value::Z32(n) => {
                let mut buf = POOL.take();
                let raw = (*n as u32) ^ 0x8000_0000;
                buf.extend_from_slice(&raw.to_be_bytes());
                Some(buf)
            }
            _ => None,
        },
        Some(Typ::U64) => match v {
            Value::U64(n) => {
                let mut buf = POOL.take();
                buf.extend_from_slice(&n.to_be_bytes());
                Some(buf)
            }
            _ => None,
        },
        Some(Typ::V64) => match v {
            Value::V64(n) => {
                let mut buf = POOL.take();
                buf.extend_from_slice(&n.to_be_bytes());
                Some(buf)
            }
            _ => None,
        },
        Some(Typ::I64) => match v {
            Value::I64(n) => {
                let mut buf = POOL.take();
                let raw = (*n as u64) ^ 0x8000_0000_0000_0000;
                buf.extend_from_slice(&raw.to_be_bytes());
                Some(buf)
            }
            _ => None,
        },
        Some(Typ::Z64) => match v {
            Value::Z64(n) => {
                let mut buf = POOL.take();
                let raw = (*n as u64) ^ 0x8000_0000_0000_0000;
                buf.extend_from_slice(&raw.to_be_bytes());
                Some(buf)
            }
            _ => None,
        },
        _ => encode_value(v),
    }
}

fn decode_key(key_typ: Option<Typ>, data: &[u8]) -> Option<Value> {
    match key_typ {
        Some(Typ::String) => {
            std::str::from_utf8(data).ok().map(|s| Value::String(ArcStr::from(s)))
        }
        Some(Typ::Bytes) => {
            Some(Value::Bytes(bytes::Bytes::copy_from_slice(data).into()))
        }
        Some(Typ::U8) if data.len() == 1 => Some(Value::U8(data[0])),
        Some(Typ::I8) if data.len() == 1 => Some(Value::I8((data[0] ^ 0x80) as i8)),
        Some(Typ::U16) if data.len() == 2 => {
            Some(Value::U16(u16::from_be_bytes([data[0], data[1]])))
        }
        Some(Typ::I16) if data.len() == 2 => {
            let raw = u16::from_be_bytes([data[0], data[1]]);
            Some(Value::I16((raw ^ 0x8000) as i16))
        }
        Some(Typ::U32) if data.len() == 4 => {
            Some(Value::U32(u32::from_be_bytes(data[..4].try_into().ok()?)))
        }
        Some(Typ::V32) if data.len() == 4 => {
            Some(Value::V32(u32::from_be_bytes(data[..4].try_into().ok()?)))
        }
        Some(Typ::I32) if data.len() == 4 => {
            let raw = u32::from_be_bytes(data[..4].try_into().ok()?);
            Some(Value::I32((raw ^ 0x8000_0000) as i32))
        }
        Some(Typ::Z32) if data.len() == 4 => {
            let raw = u32::from_be_bytes(data[..4].try_into().ok()?);
            Some(Value::Z32((raw ^ 0x8000_0000) as i32))
        }
        Some(Typ::U64) if data.len() == 8 => {
            Some(Value::U64(u64::from_be_bytes(data[..8].try_into().ok()?)))
        }
        Some(Typ::V64) if data.len() == 8 => {
            Some(Value::V64(u64::from_be_bytes(data[..8].try_into().ok()?)))
        }
        Some(Typ::I64) if data.len() == 8 => {
            let raw = u64::from_be_bytes(data[..8].try_into().ok()?);
            Some(Value::I64((raw ^ 0x8000_0000_0000_0000) as i64))
        }
        Some(Typ::Z64) if data.len() == 8 => {
            let raw = u64::from_be_bytes(data[..8].try_into().ok()?);
            Some(Value::Z64((raw ^ 0x8000_0000_0000_0000) as i64))
        }
        _ => decode_value(data),
    }
}

fn kv_struct(key: Value, value: Value) -> Value {
    Value::Array(ValArray::from([
        Value::Array(ValArray::from([Value::String(arcstr::literal!("key")), key])),
        Value::Array(ValArray::from([Value::String(arcstr::literal!("value")), value])),
    ]))
}

fn key_struct(key: Value) -> Value {
    Value::Array(ValArray::from([Value::Array(ValArray::from([
        Value::String(arcstr::literal!("key")),
        key,
    ]))]))
}

// ── Type extraction helpers ──────────────────────────────────────

/// Extract a single Typ from a Type if it's a single primitive type
/// suitable for raw key encoding.
fn prim_typ(t: &Type) -> Option<Typ> {
    match t {
        Type::Primitive(flags) if flags.iter().count() == 1 => flags.iter().next(),
        _ => None,
    }
}

/// Walk a resolved Type to find a Ref or Abstract with 2+ params
/// (representing Tree<K, V>) and extract its first type parameter
/// (the key type). Looks through Ref wrappers like Result, Set, etc.
fn find_tree_params(t: &Type) -> Option<&[Type]> {
    match t {
        // Abstract type with params — e.g. Tree<K, V>
        Type::Abstract { params, .. } if params.len() >= 2 => Some(params),
        // Ref type — could be Tree<K, V> itself (abstract type decl)
        // or a wrapper like Result<Tree<K, V>, ...>
        Type::Ref { params, .. } if params.len() >= 2 => {
            // Check if first param is a primitive (= this IS the tree-like type)
            if matches!(&params[0], Type::Primitive(_)) {
                Some(params)
            } else {
                // Recurse into params to find the tree
                for p in params.iter() {
                    if let r @ Some(_) = find_tree_params(p) {
                        return r;
                    }
                }
                None
            }
        }
        Type::Set(parts) => {
            for p in parts.iter() {
                if let r @ Some(_) = find_tree_params(p) {
                    return r;
                }
            }
            None
        }
        _ => None,
    }
}

/// Extract key_typ from the resolved rtype of a Tree-producing function.
fn extract_key_typ_from_rtype(resolved_typ: Option<&FnType>) -> Option<Typ> {
    let ft = resolved_typ?;
    find_tree_params(&ft.rtype).and_then(|params| prim_typ(&params[0]))
}

/// Extract key and value type display strings from a Tree-producing
/// function's resolved rtype.
fn extract_type_strings_from_rtype(resolved_typ: Option<&FnType>) -> (ArcStr, ArcStr) {
    let Some(ft) = resolved_typ else {
        return (arcstr::literal!("?"), arcstr::literal!("?"));
    };
    match find_tree_params(&ft.rtype) {
        Some(params) if params.len() >= 2 => (
            ArcStr::from(format!("{}", params[0]).as_str()),
            ArcStr::from(format!("{}", params[1]).as_str()),
        ),
        _ => (arcstr::literal!("?"), arcstr::literal!("?")),
    }
}

// ── Abstract types ────────────────────────────────────────────────

// -- DbValue --

#[derive(Debug, Clone)]
pub struct DbValue {
    inner: Arc<sled::Db>,
}

impl PartialEq for DbValue {
    fn eq(&self, other: &Self) -> bool {
        Arc::as_ptr(&self.inner) == Arc::as_ptr(&other.inner)
    }
}

impl Eq for DbValue {}

impl PartialOrd for DbValue {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for DbValue {
    fn cmp(&self, other: &Self) -> Ordering {
        Arc::as_ptr(&self.inner).cmp(&Arc::as_ptr(&other.inner))
    }
}

impl Hash for DbValue {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Arc::as_ptr(&self.inner).hash(state)
    }
}

graphix_package_core::impl_no_pack!(DbValue);

static DB_WRAPPER: LazyLock<AbstractWrapper<DbValue>> = LazyLock::new(|| {
    let id = uuid::Uuid::from_bytes([
        0xd1, 0xe2, 0xf3, 0x04, 0x15, 0x26, 0x47, 0x38, 0x49, 0x5a, 0x6b, 0x7c, 0x8d,
        0x9e, 0xaf, 0xb0,
    ]);
    Abstract::register::<DbValue>(id).expect("failed to register DbValue")
});

fn get_db(cached: &CachedVals, idx: usize) -> Option<sled::Db> {
    match cached.0.get(idx)?.as_ref()? {
        Value::Abstract(a) => {
            let dv = a.downcast_ref::<DbValue>()?;
            Some((*dv.inner).clone())
        }
        _ => None,
    }
}

// -- TreeInner --

#[derive(Debug)]
struct TreeInner {
    tree: sled::Tree,
    key_typ: Option<Typ>,
}

// -- TreeValue --

#[derive(Debug, Clone)]
pub struct TreeValue {
    inner: Arc<TreeInner>,
}

impl PartialEq for TreeValue {
    fn eq(&self, other: &Self) -> bool {
        Arc::as_ptr(&self.inner) == Arc::as_ptr(&other.inner)
    }
}

impl Eq for TreeValue {}

impl PartialOrd for TreeValue {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for TreeValue {
    fn cmp(&self, other: &Self) -> Ordering {
        Arc::as_ptr(&self.inner).cmp(&Arc::as_ptr(&other.inner))
    }
}

impl Hash for TreeValue {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Arc::as_ptr(&self.inner).hash(state)
    }
}

graphix_package_core::impl_no_pack!(TreeValue);

static TREE_WRAPPER: LazyLock<AbstractWrapper<TreeValue>> = LazyLock::new(|| {
    let id = uuid::Uuid::from_bytes([
        0xd2, 0xe3, 0xf4, 0x05, 0x16, 0x27, 0x48, 0x39, 0x4a, 0x5b, 0x6c, 0x7d, 0x8e,
        0x9f, 0xa0, 0xb1,
    ]);
    Abstract::register::<TreeValue>(id).expect("failed to register TreeValue")
});

fn get_tree_inner(cached: &CachedVals, idx: usize) -> Option<Arc<TreeInner>> {
    match cached.0.get(idx)?.as_ref()? {
        Value::Abstract(a) => {
            let tv = a.downcast_ref::<TreeValue>()?;
            Some(tv.inner.clone())
        }
        _ => None,
    }
}

fn wrap_tree(tree: sled::Tree, key_typ: Option<Typ>) -> Value {
    TREE_WRAPPER.wrap(TreeValue { inner: Arc::new(TreeInner { tree, key_typ }) })
}

// -- CursorValue --

struct CursorInner {
    iter: parking_lot::Mutex<sled::Iter>,
    key_typ: Option<Typ>,
}

impl fmt::Debug for CursorInner {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("CursorInner").finish()
    }
}

#[derive(Debug, Clone)]
struct CursorValue {
    inner: Arc<CursorInner>,
}

impl PartialEq for CursorValue {
    fn eq(&self, other: &Self) -> bool {
        Arc::as_ptr(&self.inner) == Arc::as_ptr(&other.inner)
    }
}

impl Eq for CursorValue {}

impl PartialOrd for CursorValue {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for CursorValue {
    fn cmp(&self, other: &Self) -> Ordering {
        Arc::as_ptr(&self.inner).cmp(&Arc::as_ptr(&other.inner))
    }
}

impl Hash for CursorValue {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Arc::as_ptr(&self.inner).hash(state)
    }
}

graphix_package_core::impl_no_pack!(CursorValue);

static CURSOR_WRAPPER: LazyLock<AbstractWrapper<CursorValue>> = LazyLock::new(|| {
    let id = uuid::Uuid::from_bytes([
        0xd3, 0xe4, 0xf5, 0x06, 0x17, 0x28, 0x49, 0x3a, 0x4b, 0x5c, 0x6d, 0x7e, 0x8f,
        0xa0, 0xb1, 0xc2,
    ]);
    Abstract::register::<CursorValue>(id).expect("failed to register CursorValue")
});

// -- SubscriptionValue --

#[derive(Debug, Clone)]
struct SubscriptionValue {
    bind_id: BindId,
}

impl PartialEq for SubscriptionValue {
    fn eq(&self, other: &Self) -> bool {
        self.bind_id == other.bind_id
    }
}

impl Eq for SubscriptionValue {}

impl PartialOrd for SubscriptionValue {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for SubscriptionValue {
    fn cmp(&self, other: &Self) -> Ordering {
        self.bind_id.cmp(&other.bind_id)
    }
}

impl Hash for SubscriptionValue {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.bind_id.hash(state)
    }
}

graphix_package_core::impl_no_pack!(SubscriptionValue);

static SUBSCRIPTION_WRAPPER: LazyLock<AbstractWrapper<SubscriptionValue>> =
    LazyLock::new(|| {
        let id = uuid::Uuid::from_bytes([
            0xd4, 0xe5, 0xf6, 0x07, 0x18, 0x29, 0x4a, 0x3b, 0x4c, 0x5d, 0x6e, 0x7f, 0x80,
            0xa1, 0xb2, 0xc3,
        ]);
        Abstract::register::<SubscriptionValue>(id)
            .expect("failed to register SubscriptionValue")
    });

// ── Custom event ──────────────────────────────────────────────────

#[derive(Debug)]
enum DbEvent {
    Insert { key: Value, value: Value },
    Remove { key: Value },
}

impl CustomBuiltinType for DbEvent {}

// ── Tree metadata ─────────────────────────────────────────────────

static META_TREE: &[u8] = b"$$__graphix_meta__$$";

/// Check/store type metadata for a named tree. Returns Err on mismatch.
fn check_or_store_meta(
    db: &sled::Db,
    tree_name: &str,
    key_str: &str,
    val_str: &str,
) -> std::result::Result<(), Value> {
    let meta = db.open_tree(META_TREE).map_err(|e| errf!("DbErr", "{e}"))?;
    let meta_key = format!("type:{tree_name}");
    match meta.get(meta_key.as_bytes()) {
        Err(e) => Err(errf!("DbErr", "{e}")),
        Ok(Some(stored)) => {
            let stored = std::str::from_utf8(&stored)
                .map_err(|e| errf!("DbErr", "corrupt metadata: {e}"))?;
            let expected = format!("{key_str}\0{val_str}");
            if stored != expected {
                // CR estokes: LPool this
                let parts: Vec<&str> = stored.splitn(2, '\0').collect();
                let (sk, sv) =
                    if parts.len() == 2 { (parts[0], parts[1]) } else { (stored, "?") };
                Err(errf!("DbErr",
                    "tree '{tree_name}' has type Tree<{sk}, {sv}>, but was opened as Tree<{key_str}, {val_str}>"
                ))
            } else {
                Ok(())
            }
        }
        Ok(None) => {
            let meta_val = format!("{key_str}\0{val_str}");
            meta.insert(meta_key.as_bytes(), meta_val.as_bytes())
                .map_err(|e| errf!("DbErr", "{e}"))?;
            Ok(())
        }
    }
}

// ── DbGetType ─────────────────────────────────────────────────────

#[derive(Debug)]
enum DbOrPath {
    Db(sled::Db),
    Path(ArcStr),
}

#[derive(Debug, Default)]
struct DbGetTypeEv;

impl EvalCachedAsync for DbGetTypeEv {
    const NAME: &str = "db_get_type";
    type Args = (DbOrPath, ArcStr);

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        let db_or_path = match cached.0.get(0)?.as_ref()? {
            Value::Abstract(a) => {
                let dv = a.downcast_ref::<DbValue>()?;
                DbOrPath::Db((*dv.inner).clone())
            }
            Value::String(s) => DbOrPath::Path(s.clone()),
            _ => return None,
        };
        let name = match cached.0.get(1)?.as_ref()? {
            Value::Null => ArcStr::from(DEFAULT_TREE_META),
            Value::String(s) => s.clone(),
            _ => return None,
        };
        Some((db_or_path, name))
    }

    fn eval((db_or_path, name): Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            match tokio::task::spawn_blocking(move || {
                let db = match db_or_path {
                    DbOrPath::Db(db) => db,
                    DbOrPath::Path(path) => {
                        sled::open(&*path).map_err(|e| errf!("DbErr", "{e}"))?
                    }
                };
                let meta =
                    db.open_tree(META_TREE).map_err(|e| errf!("DbErr", "{e}"))?;
                let meta_key = format!("type:{name}");
                match meta.get(meta_key.as_bytes()) {
                    Err(e) => Err(errf!("DbErr", "{e}")),
                    Ok(None) => Ok(Value::Null),
                    Ok(Some(stored)) => {
                        let stored = std::str::from_utf8(&stored)
                            .map_err(|e| errf!("DbErr", "corrupt metadata: {e}"))?;
                        let mut parts = stored.splitn(2, '\0');
                        let k = parts.next().unwrap_or("?");
                        let v = parts.next().unwrap_or("?");
                        Ok(Value::Array(ValArray::from([
                            Value::String(ArcStr::from(k)),
                            Value::String(ArcStr::from(v)),
                        ])))
                    }
                }
            })
            .await
            {
                Err(e) => errf!("DbErr", "task panicked: {e}"),
                Ok(Err(e)) => e,
                Ok(Ok(v)) => v,
            }
        }
    }
}

type DbGetType = CachedArgsAsync<DbGetTypeEv>;

// ── EvalCachedAsync builtins ──────────────────────────────────────

// -- DbOpen --

#[derive(Debug, Default)]
struct DbOpenEv;

impl EvalCachedAsync for DbOpenEv {
    const NAME: &str = "db_open";
    type Args = ArcStr;

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        cached.get::<ArcStr>(0)
    }

    fn eval(path: Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            match tokio::task::spawn_blocking(move || sled::open(&*path)).await {
                Err(e) => errf!("DbErr", "task panicked: {e}"),
                Ok(Err(e)) => errf!("DbErr", "{e}"),
                Ok(Ok(db)) => DB_WRAPPER.wrap(DbValue { inner: Arc::new(db) }),
            }
        }
    }
}

type DbOpen = CachedArgsAsync<DbOpenEv>;

// -- DbFlush --

#[derive(Debug, Default)]
struct DbFlushEv;

impl EvalCachedAsync for DbFlushEv {
    const NAME: &str = "db_flush";
    type Args = sled::Db;

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        get_db(cached, 0)
    }

    fn eval(db: Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            match tokio::task::spawn_blocking(move || db.flush()).await {
                Err(e) => errf!("DbErr", "task panicked: {e}"),
                Ok(Err(e)) => errf!("DbErr", "{e}"),
                Ok(Ok(_)) => Value::Null,
            }
        }
    }
}

type DbFlush = CachedArgsAsync<DbFlushEv>;

// -- DbTreeNames --

#[derive(Debug, Default)]
struct DbTreeNamesEv;

impl EvalCachedAsync for DbTreeNamesEv {
    const NAME: &str = "db_tree_names";
    type Args = sled::Db;

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        get_db(cached, 0)
    }

    fn eval(db: Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            match tokio::task::spawn_blocking(move || db.tree_names()).await {
                Err(e) => errf!("DbErr", "task panicked: {e}"),
                Ok(names) => {
                    let mut vals: LPooled<Vec<Value>> = names
                        .into_iter()
                        .filter_map(|ivec| {
                            std::str::from_utf8(&ivec)
                                .ok()
                                .map(|s| Value::String(ArcStr::from(s)))
                        })
                        .collect();
                    Value::Array(ValArray::from_iter_exact(vals.drain(..)))
                }
            }
        }
    }
}

type DbTreeNames = CachedArgsAsync<DbTreeNamesEv>;

// -- DbDropTree --

#[derive(Debug, Default)]
struct DbDropTreeEv;

impl EvalCachedAsync for DbDropTreeEv {
    const NAME: &str = "db_drop_tree";
    type Args = (sled::Db, ArcStr);

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        let db = get_db(cached, 0)?;
        let name = cached.get::<ArcStr>(1)?;
        Some((db, name))
    }

    fn eval((db, name): Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            match tokio::task::spawn_blocking(move || db.drop_tree(name.as_bytes())).await
            {
                Err(e) => errf!("DbErr", "task panicked: {e}"),
                Ok(Err(e)) => errf!("DbErr", "{e}"),
                Ok(Ok(existed)) => Value::Bool(existed),
            }
        }
    }
}

type DbDropTree = CachedArgsAsync<DbDropTreeEv>;

// ── DbTree — manual BuiltIn with type extraction ─────────────────

#[derive(Debug)]
struct DbTree {
    cached: CachedVals,
    id: BindId,
    top_id: ExprId,
    running: bool,
    key_typ: Option<Typ>,
    key_str: ArcStr,
    val_str: ArcStr,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for DbTree {
    const NAME: &str = "db_tree";

    fn init<'a, 'b, 'c>(
        ctx: &'a mut ExecCtx<R, E>,
        _typ: &'a graphix_compiler::typ::FnType,
        resolved_typ: Option<&'a graphix_compiler::typ::FnType>,
        _scope: &'b Scope,
        from: &'c [Node<R, E>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        eprintln!("DbTree::init resolved_typ = {resolved_typ:?}");
        let key_typ = extract_key_typ_from_rtype(resolved_typ);
        eprintln!("DbTree::init key_typ = {key_typ:?}");
        let (key_str, val_str) = extract_type_strings_from_rtype(resolved_typ);
        let id = BindId::new();
        ctx.rt.ref_var(id, top_id);
        Ok(Box::new(DbTree {
            cached: CachedVals::new(from),
            id,
            top_id,
            running: false,
            key_typ,
            key_str,
            val_str,
        }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for DbTree {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        let result = event.variables.remove(&self.id).map(|v| {
            self.running = false;
            v
        });
        if self.cached.update(ctx, from, event) && !self.running {
            let db = get_db(&self.cached, 0)?;
            let name = self.cached.get::<ArcStr>(1)?;
            let key_typ = self.key_typ;
            let key_str = self.key_str.clone();
            let val_str = self.val_str.clone();
            let id = self.id;
            self.running = true;
            ctx.rt.spawn_var(async move {
                match tokio::task::spawn_blocking(move || {
                    if &*name == DEFAULT_TREE_META
                        || name.as_bytes() == META_TREE
                    {
                        return Err(errf!(
                            "DbErr",
                            "tree name '{name}' is reserved"
                        ));
                    }
                    if let Err(e) = check_or_store_meta(&db, &name, &key_str, &val_str) {
                        return Err(e);
                    }
                    db.open_tree(name.as_bytes())
                        .map(|tree| wrap_tree(tree, key_typ))
                        .map_err(|e| errf!("DbErr", "{e}"))
                })
                .await
                {
                    Err(e) => (id, errf!("DbErr", "task panicked: {e}")),
                    Ok(Err(e)) => (id, e),
                    Ok(Ok(v)) => (id, v),
                }
            });
        }
        result
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        ctx.rt.unref_var(self.id, self.top_id);
        self.running = false;
        self.cached.clear();
        let id = BindId::new();
        ctx.rt.ref_var(id, self.top_id);
        self.id = id;
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        ctx.rt.unref_var(self.id, self.top_id);
    }
}

// -- DbDefault --

static DEFAULT_TREE_META: &str = "$$__graphix_default__$$";

/// Type strings are concrete if they aren't fallback "?" and don't
/// look like unresolved type variables (e.g. "'a").
fn types_are_concrete(key_str: &str, val_str: &str) -> bool {
    fn concrete(s: &str) -> bool {
        s != "?" && !s.starts_with('\'')
    }
    concrete(key_str) && concrete(val_str)
}

#[derive(Debug)]
struct DbDefault {
    cached: CachedVals,
    id: BindId,
    top_id: ExprId,
    running: bool,
    key_typ: Option<Typ>,
    key_str: ArcStr,
    val_str: ArcStr,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for DbDefault {
    const NAME: &str = "db_default";

    fn init<'a, 'b, 'c>(
        ctx: &'a mut ExecCtx<R, E>,
        _typ: &'a graphix_compiler::typ::FnType,
        resolved_typ: Option<&'a graphix_compiler::typ::FnType>,
        _scope: &'b Scope,
        from: &'c [Node<R, E>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        let key_typ = extract_key_typ_from_rtype(resolved_typ);
        let (key_str, val_str) = extract_type_strings_from_rtype(resolved_typ);
        let id = BindId::new();
        ctx.rt.ref_var(id, top_id);
        Ok(Box::new(DbDefault {
            cached: CachedVals::new(from),
            id,
            top_id,
            running: false,
            key_typ,
            key_str,
            val_str,
        }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for DbDefault {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        let result = event.variables.remove(&self.id).map(|v| {
            self.running = false;
            v
        });
        if self.cached.update(ctx, from, event) && !self.running {
            let db = get_db(&self.cached, 0)?;
            let key_typ = self.key_typ;
            let key_str = self.key_str.clone();
            let val_str = self.val_str.clone();
            let id = self.id;
            self.running = true;
            ctx.rt.spawn_var(async move {
                match tokio::task::spawn_blocking(move || {
                    if types_are_concrete(&key_str, &val_str) {
                        if let Err(e) = check_or_store_meta(
                            &db,
                            DEFAULT_TREE_META,
                            &key_str,
                            &val_str,
                        ) {
                            return Err(e);
                        }
                    }
                    Ok(wrap_tree((*db).clone(), key_typ))
                })
                .await
                {
                    Err(e) => (id, errf!("DbErr", "task panicked: {e}")),
                    Ok(Err(e)) => (id, e),
                    Ok(Ok(v)) => (id, v),
                }
            });
        }
        result
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        ctx.rt.unref_var(self.id, self.top_id);
        self.running = false;
        self.cached.clear();
        let id = BindId::new();
        ctx.rt.ref_var(id, self.top_id);
        self.id = id;
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        ctx.rt.unref_var(self.id, self.top_id);
    }
}

// ── Key-encoding builtins ─────────────────────────────────────────

// -- DbGet --

#[derive(Debug, Default)]
struct DbGetEv;

impl EvalCachedAsync for DbGetEv {
    const NAME: &str = "db_get";
    type Args = (Arc<TreeInner>, GPooled<Vec<u8>>);

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        let tree = get_tree_inner(cached, 0)?;
        let key_val = cached.0.get(1)?.as_ref()?;
        let key = encode_key(tree.key_typ, key_val)?;
        Some((tree, key))
    }

    fn eval((tree, key): Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            match tokio::task::spawn_blocking(move || tree.tree.get(&*key)).await {
                Err(e) => errf!("DbErr", "task panicked: {e}"),
                Ok(Err(e)) => errf!("DbErr", "{e}"),
                Ok(Ok(None)) => Value::Null,
                Ok(Ok(Some(ivec))) => match decode_value(&ivec) {
                    Some(v) => v,
                    None => errf!("DbErr", "failed to decode value"),
                },
            }
        }
    }
}

type DbGet = CachedArgsAsync<DbGetEv>;

// -- DbInsert --

#[derive(Debug, Default)]
struct DbInsertEv;

impl EvalCachedAsync for DbInsertEv {
    const NAME: &str = "db_insert";
    type Args = (Arc<TreeInner>, GPooled<Vec<u8>>, GPooled<Vec<u8>>);

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        let tree = get_tree_inner(cached, 0)?;
        let key_val = cached.0.get(1)?.as_ref()?;
        let key = encode_key(tree.key_typ, key_val)?;
        let val = encode_value(cached.0.get(2)?.as_ref()?)?;
        Some((tree, key, val))
    }

    fn eval((tree, key, val): Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            match tokio::task::spawn_blocking(move || {
                tree.tree.insert(&*key, val.as_slice())
            })
            .await
            {
                Err(e) => errf!("DbErr", "task panicked: {e}"),
                Ok(Err(e)) => errf!("DbErr", "{e}"),
                Ok(Ok(None)) => Value::Null,
                Ok(Ok(Some(old))) => match decode_value(&old) {
                    Some(v) => v,
                    None => errf!("DbErr", "failed to decode previous value"),
                },
            }
        }
    }
}

type DbInsert = CachedArgsAsync<DbInsertEv>;

// -- DbInsertMany --

#[derive(Debug, Default)]
struct DbInsertManyEv;

impl EvalCachedAsync for DbInsertManyEv {
    const NAME: &str = "db_insert_many";
    type Args = (Arc<TreeInner>, Vec<(GPooled<Vec<u8>>, GPooled<Vec<u8>>)>);

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        let tree = get_tree_inner(cached, 0)?;
        let arr = match cached.0.get(1)?.as_ref()? {
            Value::Array(a) => a,
            _ => return None,
        };
        let mut pairs = Vec::with_capacity(arr.len());
        for pair in arr.iter() {
            match pair {
                Value::Array(p) if p.len() == 2 => {
                    let key = encode_key(tree.key_typ, &p[0])?;
                    let val = encode_value(&p[1])?;
                    pairs.push((key, val));
                }
                _ => return None,
            }
        }
        Some((tree, pairs))
    }

    fn eval((tree, pairs): Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            match tokio::task::spawn_blocking(move || {
                let mut results: LPooled<Vec<Value>> = LPooled::take();
                for (key, val) in pairs.iter() {
                    match tree.tree.insert(&**key, val.as_slice()) {
                        Err(e) => return Err(errf!("DbErr", "{e}")),
                        Ok(None) => results.push(Value::Null),
                        Ok(Some(prev)) => match decode_value(&prev) {
                            Some(v) => results.push(v),
                            None => {
                                return Err(errf!("DbErr", "failed to decode previous value"))
                            }
                        },
                    }
                }
                Ok(Value::Array(ValArray::from_iter_exact(results.drain(..))))
            })
            .await
            {
                Err(e) => errf!("DbErr", "task panicked: {e}"),
                Ok(Err(e)) => e,
                Ok(Ok(v)) => v,
            }
        }
    }
}

type DbInsertMany = CachedArgsAsync<DbInsertManyEv>;

// -- DbRemove --

#[derive(Debug, Default)]
struct DbRemoveEv;

impl EvalCachedAsync for DbRemoveEv {
    const NAME: &str = "db_remove";
    type Args = (Arc<TreeInner>, GPooled<Vec<u8>>);

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        let tree = get_tree_inner(cached, 0)?;
        let key_val = cached.0.get(1)?.as_ref()?;
        let key = encode_key(tree.key_typ, key_val)?;
        Some((tree, key))
    }

    fn eval((tree, key): Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            match tokio::task::spawn_blocking(move || tree.tree.remove(&*key)).await {
                Err(e) => errf!("DbErr", "task panicked: {e}"),
                Ok(Err(e)) => errf!("DbErr", "{e}"),
                Ok(Ok(None)) => Value::Null,
                Ok(Ok(Some(old))) => match decode_value(&old) {
                    Some(v) => v,
                    None => errf!("DbErr", "failed to decode previous value"),
                },
            }
        }
    }
}

type DbRemove = CachedArgsAsync<DbRemoveEv>;

// -- DbContainsKey --

#[derive(Debug, Default)]
struct DbContainsKeyEv;

impl EvalCachedAsync for DbContainsKeyEv {
    const NAME: &str = "db_contains_key";
    type Args = (Arc<TreeInner>, GPooled<Vec<u8>>);

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        let tree = get_tree_inner(cached, 0)?;
        let key_val = cached.0.get(1)?.as_ref()?;
        let key = encode_key(tree.key_typ, key_val)?;
        Some((tree, key))
    }

    fn eval((tree, key): Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            match tokio::task::spawn_blocking(move || tree.tree.contains_key(&*key)).await
            {
                Err(e) => errf!("DbErr", "task panicked: {e}"),
                Ok(Err(e)) => errf!("DbErr", "{e}"),
                Ok(Ok(exists)) => Value::Bool(exists),
            }
        }
    }
}

type DbContainsKey = CachedArgsAsync<DbContainsKeyEv>;

// -- DbGetMany --

#[derive(Debug, Default)]
struct DbGetManyEv;

impl EvalCachedAsync for DbGetManyEv {
    const NAME: &str = "db_get_many";
    type Args = (Arc<TreeInner>, Vec<GPooled<Vec<u8>>>);

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        let tree = get_tree_inner(cached, 0)?;
        let arr = match cached.0.get(1)?.as_ref()? {
            Value::Array(a) => a,
            _ => return None,
        };
        let mut keys = Vec::with_capacity(arr.len());
        for k in arr.iter() {
            keys.push(encode_key(tree.key_typ, k)?);
        }
        Some((tree, keys))
    }

    fn eval((tree, keys): Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            match tokio::task::spawn_blocking(move || {
                let mut results: LPooled<Vec<Value>> = LPooled::take();
                for key in keys.iter() {
                    match tree.tree.get(&**key) {
                        Err(e) => return Err(errf!("DbErr", "{e}")),
                        Ok(None) => results.push(Value::Null),
                        Ok(Some(ivec)) => match decode_value(&ivec) {
                            Some(v) => results.push(v),
                            None => {
                                return Err(errf!("DbErr", "failed to decode value"))
                            }
                        },
                    }
                }
                Ok(Value::Array(ValArray::from_iter_exact(results.drain(..))))
            })
            .await
            {
                Err(e) => errf!("DbErr", "task panicked: {e}"),
                Ok(Err(e)) => e,
                Ok(Ok(v)) => v,
            }
        }
    }
}

type DbGetMany = CachedArgsAsync<DbGetManyEv>;

// -- DbRemoveMany --

#[derive(Debug, Default)]
struct DbRemoveManyEv;

impl EvalCachedAsync for DbRemoveManyEv {
    const NAME: &str = "db_remove_many";
    type Args = (Arc<TreeInner>, Vec<GPooled<Vec<u8>>>);

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        let tree = get_tree_inner(cached, 0)?;
        let arr = match cached.0.get(1)?.as_ref()? {
            Value::Array(a) => a,
            _ => return None,
        };
        let mut keys = Vec::with_capacity(arr.len());
        for k in arr.iter() {
            keys.push(encode_key(tree.key_typ, k)?);
        }
        Some((tree, keys))
    }

    fn eval((tree, keys): Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            match tokio::task::spawn_blocking(move || {
                let mut results: LPooled<Vec<Value>> = LPooled::take();
                for key in keys.iter() {
                    match tree.tree.remove(&**key) {
                        Err(e) => return Err(errf!("DbErr", "{e}")),
                        Ok(None) => results.push(Value::Null),
                        Ok(Some(old)) => match decode_value(&old) {
                            Some(v) => results.push(v),
                            None => {
                                return Err(errf!("DbErr", "failed to decode previous value"))
                            }
                        },
                    }
                }
                Ok(Value::Array(ValArray::from_iter_exact(results.drain(..))))
            })
            .await
            {
                Err(e) => errf!("DbErr", "task panicked: {e}"),
                Ok(Err(e)) => e,
                Ok(Ok(v)) => v,
            }
        }
    }
}

type DbRemoveMany = CachedArgsAsync<DbRemoveManyEv>;

// ── Cursor ────────────────────────────────────────────────────────

// -- DbCursorNew (with optional prefix) --

#[derive(Debug, Default)]
struct DbCursorNewEv;

impl EvalCachedAsync for DbCursorNewEv {
    const NAME: &str = "db_cursor_new";
    type Args = (Option<Value>, Arc<TreeInner>);

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        let prefix_val = match cached.0.get(0)?.as_ref()? {
            Value::Null => None,
            v => Some(v.clone()),
        };
        let tree = get_tree_inner(cached, 1)?;
        Some((prefix_val, tree))
    }

    fn eval((prefix_val, tree): Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            match tokio::task::spawn_blocking(move || {
                let iter = match prefix_val {
                    Some(ref pv) => match encode_key(tree.key_typ, pv) {
                        Some(encoded) => tree.tree.scan_prefix(&*encoded),
                        None => tree.tree.iter(),
                    },
                    None => tree.tree.iter(),
                };
                CURSOR_WRAPPER.wrap(CursorValue {
                    inner: Arc::new(CursorInner {
                        iter: parking_lot::Mutex::new(iter),
                        key_typ: tree.key_typ,
                    }),
                })
            })
            .await
            {
                Err(e) => errf!("DbErr", "task panicked: {e}"),
                Ok(v) => v,
            }
        }
    }
}

type DbCursorNew = CachedArgsAsync<DbCursorNewEv>;

// -- DbCursorRead --

#[derive(Debug, Default)]
struct DbCursorReadEv;

impl EvalCachedAsync for DbCursorReadEv {
    const NAME: &str = "db_cursor_read";
    type Args = Arc<CursorInner>;

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        cached.0.get(1)?.as_ref()?;
        match cached.0.get(0)?.as_ref()? {
            Value::Abstract(a) => {
                a.downcast_ref::<CursorValue>().map(|c| c.inner.clone())
            }
            _ => None,
        }
    }

    fn eval(inner: Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            match tokio::task::spawn_blocking(move || {
                let key_typ = inner.key_typ;
                inner.iter.lock().next().map(|r| {
                    r.map(|(k, v)| {
                        let key = decode_key(key_typ, &k);
                        let val = decode_value(&v);
                        (key, val)
                    })
                })
            })
            .await
            {
                Ok(Some(Ok((Some(k), Some(v))))) => {
                    Value::Array(ValArray::from([k, v]))
                }
                Ok(Some(Ok(_))) => errf!("DbErr", "failed to decode entry"),
                Ok(Some(Err(e))) => errf!("DbErr", "{e}"),
                Ok(None) => Value::Null,
                Err(e) => errf!("DbErr", "task panicked: {e}"),
            }
        }
    }
}

type DbCursorRead = CachedArgsAsync<DbCursorReadEv>;

// -- DbCursorReadMany --

#[derive(Debug, Default)]
struct DbCursorReadManyEv;

impl EvalCachedAsync for DbCursorReadManyEv {
    const NAME: &str = "db_cursor_read_many";
    type Args = (Arc<CursorInner>, i64);

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        let n = match cached.0.get(1)?.as_ref()? {
            Value::I64(n) => *n,
            _ => return None,
        };
        match cached.0.get(0)?.as_ref()? {
            Value::Abstract(a) => {
                a.downcast_ref::<CursorValue>().map(|c| (c.inner.clone(), n))
            }
            _ => None,
        }
    }

    fn eval((inner, count): Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            let n = count.max(0) as usize;
            match tokio::task::spawn_blocking(move || {
                let key_typ = inner.key_typ;
                let mut results: LPooled<Vec<Value>> = LPooled::take();
                let mut iter = inner.iter.lock();
                for _ in 0..n {
                    match iter.next() {
                        None => break,
                        Some(Err(e)) => return Err(errf!("DbErr", "{e}")),
                        Some(Ok((k, v))) => {
                            match (decode_key(key_typ, &k), decode_value(&v)) {
                                (Some(k), Some(v)) => {
                                    results.push(Value::Array(ValArray::from([k, v])));
                                }
                                _ => {
                                    return Err(errf!("DbErr", "failed to decode entry"))
                                }
                            }
                        }
                    }
                }
                Ok(Value::Array(ValArray::from_iter_exact(results.drain(..))))
            })
            .await
            {
                Err(e) => errf!("DbErr", "task panicked: {e}"),
                Ok(Err(e)) => e,
                Ok(Ok(v)) => v,
            }
        }
    }
}

type DbCursorReadMany = CachedArgsAsync<DbCursorReadManyEv>;

// ── Subscribe (with optional prefix) ──────────────────────────────

#[derive(Debug)]
struct DbSubscribe {
    tree_val: Option<Value>,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for DbSubscribe {
    const NAME: &str = "db_subscribe";

    fn init<'a, 'b, 'c>(
        _ctx: &'a mut ExecCtx<R, E>,
        _typ: &'a graphix_compiler::typ::FnType,
        _resolved_typ: Option<&'a graphix_compiler::typ::FnType>,
        _scope: &'b Scope,
        _from: &'c [Node<R, E>],
        _top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        Ok(Box::new(DbSubscribe { tree_val: None }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for DbSubscribe {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        // from[0] = optional prefix (null = no prefix), from[1] = tree
        let prefix_val = from[0].update(ctx, event);
        let tree_changed = from[1].update(ctx, event);
        let tree_is_new = tree_changed.is_some();
        if let Some(v) = tree_changed {
            self.tree_val = Some(v);
        }
        if self.tree_val.is_none() || (prefix_val.is_none() && !tree_is_new) {
            return None;
        }
        if let Some(Value::Abstract(ref a)) = self.tree_val {
            if let Some(tv) = a.downcast_ref::<TreeValue>() {
                let tree_inner = tv.inner.clone();
                let key_typ = tree_inner.key_typ;
                let prefix_bytes: Vec<u8> = match &prefix_val {
                    Some(Value::Null) | None => vec![],
                    Some(pv) => match encode_key(key_typ, pv) {
                        Some(buf) => buf.to_vec(),
                        None => vec![],
                    },
                };
                let bind_id = BindId::new();
                let (mut tx, rx) = mpsc::channel(10);
                ctx.rt.watch(rx);
                std::thread::spawn(move || {
                    let subscriber = tree_inner.tree.watch_prefix(prefix_bytes);
                    for event in subscriber {
                        let db_event = match event {
                            sled::Event::Insert { key, value } => {
                                match (decode_key(key_typ, &key), decode_value(&value)) {
                                    (Some(k), Some(v)) => {
                                        DbEvent::Insert { key: k, value: v }
                                    }
                                    _ => continue,
                                }
                            }
                            sled::Event::Remove { key } => {
                                match decode_key(key_typ, &key) {
                                    Some(k) => DbEvent::Remove { key: k },
                                    None => continue,
                                }
                            }
                        };
                        let mut batch: GPooled<
                            Vec<(BindId, Box<dyn CustomBuiltinType>)>,
                        > = CBATCH_POOL.take();
                        batch.push((bind_id, Box::new(db_event)));
                        if futures::executor::block_on(tx.send(batch)).is_err() {
                            break;
                        }
                    }
                });
                return Some(SUBSCRIPTION_WRAPPER.wrap(SubscriptionValue { bind_id }));
            }
        }
        None
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {
        self.tree_val = None;
    }
}

// ── Subscription accessors ────────────────────────────────────────

fn extract_sub_bind_id(v: &Value) -> Option<BindId> {
    match v {
        Value::Abstract(a) => Some(a.downcast_ref::<SubscriptionValue>()?.bind_id),
        _ => None,
    }
}

fn scan_db_events<E: UserEvent>(
    bind_ids: &FxHashSet<BindId>,
    event: &mut Event<E>,
    convert: fn(&mut DbEvent) -> Option<Value>,
) -> Option<Value> {
    for bid in bind_ids {
        if let Some(mut cbt) = event.custom.remove(bid) {
            if let Some(se) = (&mut *cbt as &mut dyn Any).downcast_mut::<DbEvent>() {
                return convert(se);
            }
        }
    }
    None
}

// -- DbOnInsert --

#[derive(Debug)]
struct DbOnInsert {
    top_id: ExprId,
    cached: CachedVals,
    bind_ids: FxHashSet<BindId>,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for DbOnInsert {
    const NAME: &str = "db_on_insert";

    fn init<'a, 'b, 'c>(
        _ctx: &'a mut ExecCtx<R, E>,
        _typ: &'a graphix_compiler::typ::FnType,
        _resolved_typ: Option<&'a graphix_compiler::typ::FnType>,
        _scope: &'b Scope,
        from: &'c [Node<R, E>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        Ok(Box::new(DbOnInsert {
            top_id,
            cached: CachedVals::new(from),
            bind_ids: FxHashSet::default(),
        }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for DbOnInsert {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        if self.cached.update(ctx, from, event) {
            for bid in self.bind_ids.drain() {
                ctx.rt.unref_var(bid, self.top_id);
            }
            for v in self.cached.0.iter() {
                if let Some(v) = v {
                    if let Some(bid) = extract_sub_bind_id(v) {
                        self.bind_ids.insert(bid);
                    }
                }
            }
            for bid in &self.bind_ids {
                ctx.rt.ref_var(*bid, self.top_id);
            }
        }
        scan_db_events(&self.bind_ids, event, |se| match se {
            DbEvent::Insert { key, value } => Some(kv_struct(key.clone(), value.clone())),
            DbEvent::Remove { .. } => None,
        })
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        for bid in self.bind_ids.drain() {
            ctx.rt.unref_var(bid, self.top_id);
        }
        self.cached.clear();
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        for bid in &self.bind_ids {
            ctx.rt.unref_var(*bid, self.top_id);
        }
    }
}

// -- DbOnRemove --

#[derive(Debug)]
struct DbOnRemove {
    top_id: ExprId,
    cached: CachedVals,
    bind_ids: FxHashSet<BindId>,
}

impl<R: Rt, E: UserEvent> BuiltIn<R, E> for DbOnRemove {
    const NAME: &str = "db_on_remove";

    fn init<'a, 'b, 'c>(
        _ctx: &'a mut ExecCtx<R, E>,
        _typ: &'a graphix_compiler::typ::FnType,
        _resolved_typ: Option<&'a graphix_compiler::typ::FnType>,
        _scope: &'b Scope,
        from: &'c [Node<R, E>],
        top_id: ExprId,
    ) -> Result<Box<dyn Apply<R, E>>> {
        Ok(Box::new(DbOnRemove {
            top_id,
            cached: CachedVals::new(from),
            bind_ids: FxHashSet::default(),
        }))
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for DbOnRemove {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        if self.cached.update(ctx, from, event) {
            for bid in self.bind_ids.drain() {
                ctx.rt.unref_var(bid, self.top_id);
            }
            for v in self.cached.0.iter() {
                if let Some(v) = v {
                    if let Some(bid) = extract_sub_bind_id(v) {
                        self.bind_ids.insert(bid);
                    }
                }
            }
            for bid in &self.bind_ids {
                ctx.rt.ref_var(*bid, self.top_id);
            }
        }
        scan_db_events(&self.bind_ids, event, |se| match se {
            DbEvent::Remove { key } => Some(key_struct(key.clone())),
            DbEvent::Insert { .. } => None,
        })
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        for bid in self.bind_ids.drain() {
            ctx.rt.unref_var(bid, self.top_id);
        }
        self.cached.clear();
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        for bid in &self.bind_ids {
            ctx.rt.unref_var(*bid, self.top_id);
        }
    }
}

// ── Package registration ──────────────────────────────────────────

graphix_derive::defpackage! {
    builtins => [
        DbGetType,
        DbOpen,
        DbFlush,
        DbTreeNames,
        DbDropTree,
        DbTree,
        DbDefault,
        DbGet,
        DbInsert,
        DbInsertMany,
        DbRemove,
        DbContainsKey,
        DbGetMany,
        DbRemoveMany,
        DbCursorNew,
        DbCursorRead,
        DbCursorReadMany,
        DbSubscribe,
        DbOnInsert,
        DbOnRemove,
    ],
}
