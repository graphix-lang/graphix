use anyhow::{bail, Result};
use arcstr::{literal, ArcStr};
use compact_str::format_compact;
use graphix_compiler::{
    errf,
    expr::ExprId,
    typ::{FnType, Type},
    ExecCtx, Node, Rt, Scope, TypecheckPhase, UserEvent,
};
use graphix_package_core::{CachedArgsAsync, CachedVals, EvalCachedAsync};
use netidx::{path::Path, publisher::Typ};
use netidx_core::pack::Pack;
use netidx_derive::Pack;
use netidx_value::{ValArray, Value};
use poolshark::{global::GPooled, local::LPooled};
use std::sync::Arc;

use crate::encoding::{
    decode_key, decode_value, encode_key, encode_value, parse_batch_ops, ENCODE_MANY_POOL,
};

// ── Abstract types ────────────────────────────────────────────────

// -- DbValue --

#[derive(Debug, Clone)]
pub struct DbValue {
    pub(crate) inner: Arc<sled::Db>,
}

graphix_package_core::impl_abstract_arc!(DbValue, pub(crate) static DB_WRAPPER = [
    0xd1, 0xe2, 0xf3, 0x04, 0x15, 0x26, 0x47, 0x38,
    0x49, 0x5a, 0x6b, 0x7c, 0x8d, 0x9e, 0xaf, 0xb0,
]);

pub(crate) fn get_db(cached: &CachedVals, idx: usize) -> Option<sled::Db> {
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
pub(crate) struct TreeInner {
    pub(crate) tree: sled::Tree,
    pub(crate) key_typ: Option<Typ>,
}

// -- TreeValue --

#[derive(Debug, Clone)]
pub struct TreeValue {
    pub(crate) inner: Arc<TreeInner>,
}

graphix_package_core::impl_abstract_arc!(TreeValue, pub(crate) static TREE_WRAPPER = [
    0xd2, 0xe3, 0xf4, 0x05, 0x16, 0x27, 0x48, 0x39,
    0x4a, 0x5b, 0x6c, 0x7d, 0x8e, 0x9f, 0xa0, 0xb1,
]);

pub(crate) fn get_tree_inner(cached: &CachedVals, idx: usize) -> Option<Arc<TreeInner>> {
    match cached.0.get(idx)?.as_ref()? {
        Value::Abstract(a) => {
            let tv = a.downcast_ref::<TreeValue>()?;
            Some(tv.inner.clone())
        }
        _ => None,
    }
}

pub(crate) fn wrap_tree(tree: sled::Tree, key_typ: Option<Typ>) -> Value {
    TREE_WRAPPER.wrap(TreeValue { inner: Arc::new(TreeInner { tree, key_typ }) })
}

// ── Tree metadata ─────────────────────────────────────────────────

pub(crate) static META_TREE: ArcStr = literal!("$$__graphix_meta__$$");
pub(crate) static DEFAULT_TREE_META: ArcStr = literal!("$$__graphix_default__$$");

// ── MetaStore trait ──────────────────────────────────────────────
//
// Unifies sled::Tree (CAS-based) and TransactionalTree (get+insert)
// so that check_or_store_meta works in both contexts.

pub(crate) trait MetaStore {
    fn get(&self, key: &[u8]) -> Result<Option<sled::IVec>>;
    fn insert_if_absent(&self, key: &[u8], value: &[u8]) -> Result<Option<sled::IVec>>;
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

pub(crate) fn read_meta(
    meta: &impl MetaStore,
    tree_name: &str,
) -> Result<Option<(ArcStr, ArcStr)>> {
    match meta.get(tree_name.as_bytes())? {
        None => Ok(None),
        Some(stored) => {
            let stored = std::str::from_utf8(&stored)?;
            let mut parts = stored.splitn(2, '\0');
            let k = parts.next().unwrap_or("?");
            let v = parts.next().unwrap_or("?");
            Ok(Some((ArcStr::from(k), ArcStr::from(v))))
        }
    }
}

pub(crate) fn check_or_store_meta(
    meta: &impl MetaStore,
    tree_name: &str,
    key_typ_str: &str,
    val_typ_str: &str,
) -> Result<()> {
    let meta_val = format_compact!("{key_typ_str}\0{val_typ_str}");
    match meta.insert_if_absent(tree_name.as_bytes(), meta_val.as_bytes())? {
        None => Ok(()),
        Some(existing) => {
            let stored = std::str::from_utf8(&existing)?;
            let mut parts = stored.splitn(2, '\0');
            let sk = parts.next().unwrap_or("?");
            let sv = parts.next().unwrap_or("?");
            if sk != key_typ_str || sv != val_typ_str {
                bail!(
                    "tree '{tree_name}' has type Tree<{sk}, {sv}> \
                    but was opened as Tree<{key_typ_str}, {val_typ_str}>"
                )
            } else {
                Ok(())
            }
        }
    }
}

// ── Type extraction helpers ──────────────────────────────────────

fn prim_typ(t: &Type) -> Option<Typ> {
    match t {
        Type::Primitive(flags) if flags.iter().count() == 1 => flags.iter().next(),
        _ => None,
    }
}

// The resolved return type is Ref("/Result", [Ref("/Tree"|"/TxnTree", [k, v]), ...]).
fn find_tree_params(t: &Type) -> Option<&[Type]> {
    match t {
        Type::Ref { name, params, .. } if Path::basename(&**name) == Some("Result") => {
            params.iter().find_map(|p| match p {
                Type::Ref { name, params, .. }
                    if matches!(Path::basename(&**name), Some("Tree" | "TxnTree"))
                        && params.len() == 2 =>
                {
                    Some(&**params)
                }
                _ => None,
            })
        }
        _ => None,
    }
}

pub(crate) fn extract_key_typ_from_rtype(resolved_typ: Option<&FnType>) -> Option<Typ> {
    let ft = resolved_typ?;
    find_tree_params(&ft.rtype).and_then(|params| prim_typ(&params[0]))
}

pub(crate) fn extract_type_strings_from_rtype(
    resolved_typ: Option<&FnType>,
) -> (ArcStr, ArcStr) {
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

pub(crate) fn types_are_concrete(key_typ_str: &str, val_typ_str: &str) -> bool {
    fn concrete(s: &str) -> bool {
        s != "?" && !s.starts_with('\'')
    }
    concrete(key_typ_str) && concrete(val_typ_str)
}

// ── Builtins ──────────────────────────────────────────────────────

// -- DbGetType --

#[derive(Debug, Default)]
pub(crate) struct DbGetTypeEv;

impl EvalCachedAsync for DbGetTypeEv {
    const NAME: &str = "db_get_type";
    const NEEDS_CALLSITE: bool = false;
    type Args = (sled::Db, ArcStr);

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        let db = get_db(cached, 0)?;
        let name = match cached.0.get(1)?.as_ref()? {
            Value::Null => DEFAULT_TREE_META.clone(),
            Value::String(s) => s.clone(),
            _ => return None,
        };
        Some((db, name))
    }

    fn eval((db, name): Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            match tokio::task::spawn_blocking(move || -> Result<Value> {
                let meta = db.open_tree(&*META_TREE)?;
                match read_meta(&meta, &name)? {
                    None => Ok(Value::Null),
                    Some((k, v)) => Ok(Value::Array(ValArray::from([
                        Value::String(k),
                        Value::String(v),
                    ]))),
                }
            })
            .await
            {
                Err(e) => errf!("DbErr", "task panicked: {e:?}"),
                Ok(Err(e)) => errf!("DbErr", "{e:?}"),
                Ok(Ok(v)) => v,
            }
        }
    }
}

pub(crate) type DbGetType = CachedArgsAsync<DbGetTypeEv>;

// -- DbOpen --

#[derive(Debug, Default)]
pub(crate) struct DbOpenEv;

impl EvalCachedAsync for DbOpenEv {
    const NAME: &str = "db_open";
    const NEEDS_CALLSITE: bool = false;
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

pub(crate) type DbOpen = CachedArgsAsync<DbOpenEv>;

// -- DbFlush --

#[derive(Debug, Default)]
pub(crate) struct DbFlushEv;

impl EvalCachedAsync for DbFlushEv {
    const NAME: &str = "db_flush";
    const NEEDS_CALLSITE: bool = false;
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

pub(crate) type DbFlush = CachedArgsAsync<DbFlushEv>;

// -- DbGenerateId --

#[derive(Debug, Default)]
pub(crate) struct DbGenerateIdEv;

impl EvalCachedAsync for DbGenerateIdEv {
    const NAME: &str = "db_generate_id";
    const NEEDS_CALLSITE: bool = false;
    type Args = sled::Db;

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        get_db(cached, 0)
    }

    fn eval(db: Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            match tokio::task::spawn_blocking(move || db.generate_id()).await {
                Err(e) => errf!("DbErr", "task panicked: {e}"),
                Ok(Err(e)) => errf!("DbErr", "{e}"),
                Ok(Ok(id)) => Value::U64(id),
            }
        }
    }
}

pub(crate) type DbGenerateId = CachedArgsAsync<DbGenerateIdEv>;

// -- DbTreeNames --

#[derive(Debug, Default)]
pub(crate) struct DbTreeNamesEv;

impl EvalCachedAsync for DbTreeNamesEv {
    const NAME: &str = "db_tree_names";
    const NEEDS_CALLSITE: bool = false;
    type Args = sled::Db;

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        get_db(cached, 0)
    }

    fn eval(db: Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            match tokio::task::spawn_blocking(move || db.tree_names()).await {
                Err(e) => errf!("DbErr", "task panicked: {e}"),
                Ok(names) => {
                    let mut vals: LPooled<Vec<_>> = names
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

pub(crate) type DbTreeNames = CachedArgsAsync<DbTreeNamesEv>;

// -- DbDropTree --

#[derive(Debug, Default)]
pub(crate) struct DbDropTreeEv;

impl EvalCachedAsync for DbDropTreeEv {
    const NAME: &str = "db_drop_tree";
    const NEEDS_CALLSITE: bool = false;
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

pub(crate) type DbDropTree = CachedArgsAsync<DbDropTreeEv>;

// -- DbTree --

#[derive(Debug)]
pub(crate) struct DbTreeArgs {
    db: sled::Db,
    name: Option<ArcStr>,
    key_typ: Option<Typ>,
    key_typ_str: ArcStr,
    val_typ_str: ArcStr,
}

#[derive(Debug, Default)]
pub(crate) struct DbTreeEv {
    key_typ: Option<Typ>,
    key_typ_str: ArcStr,
    val_typ_str: ArcStr,
}

impl EvalCachedAsync for DbTreeEv {
    const NAME: &str = "db_tree";
    const NEEDS_CALLSITE: bool = true;
    type Args = DbTreeArgs;

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
        DbTreeEv { key_typ, key_typ_str, val_typ_str }
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
        let db = get_db(cached, 0)?;
        let name = match cached.0.get(1)?.as_ref()? {
            Value::Null => None,
            Value::String(s) => Some(s.clone()),
            _ => return None,
        };
        Some(DbTreeArgs {
            db,
            name,
            key_typ: self.key_typ,
            key_typ_str: self.key_typ_str.clone(),
            val_typ_str: self.val_typ_str.clone(),
        })
    }

    fn eval(args: Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            let DbTreeArgs { db, name, key_typ, key_typ_str, val_typ_str } = args;
            match tokio::task::spawn_blocking(move || -> Result<Value> {
                if !types_are_concrete(&key_typ_str, &val_typ_str) {
                    bail!("tree requires concrete type annotations")
                }
                let meta = db.open_tree(&META_TREE)?;
                match name {
                    Some(name) => {
                        if &*name == DEFAULT_TREE_META
                            || name.as_bytes() == META_TREE.as_bytes()
                        {
                            bail!("tree name '{name}' is reserved");
                        }
                        check_or_store_meta(&meta, &name, &key_typ_str, &val_typ_str)?;
                        Ok(db
                            .open_tree(name.as_bytes())
                            .map(|tree| wrap_tree(tree, key_typ))?)
                    }
                    None => {
                        check_or_store_meta(
                            &meta,
                            &DEFAULT_TREE_META,
                            &key_typ_str,
                            &val_typ_str,
                        )?;
                        Ok(wrap_tree((*db).clone(), key_typ))
                    }
                }
            })
            .await
            {
                Err(e) => errf!("DbErr", "task panicked: {e}"),
                Ok(Err(e)) => errf!("DbErr", "{e:?}"),
                Ok(Ok(v)) => v,
            }
        }
    }
}

pub(crate) type DbTree = CachedArgsAsync<DbTreeEv>;

// ── Key-encoding builtins ─────────────────────────────────────────

// -- DbGet --

#[derive(Debug, Default)]
pub(crate) struct DbGetEv;

impl EvalCachedAsync for DbGetEv {
    const NAME: &str = "db_get";
    const NEEDS_CALLSITE: bool = false;
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

pub(crate) type DbGet = CachedArgsAsync<DbGetEv>;

// -- DbInsert --

#[derive(Debug, Default)]
pub(crate) struct DbInsertEv;

impl EvalCachedAsync for DbInsertEv {
    const NAME: &str = "db_insert";
    const NEEDS_CALLSITE: bool = false;
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

pub(crate) type DbInsert = CachedArgsAsync<DbInsertEv>;

// -- DbRemove --

#[derive(Debug, Default)]
pub(crate) struct DbRemoveEv;

impl EvalCachedAsync for DbRemoveEv {
    const NAME: &str = "db_remove";
    const NEEDS_CALLSITE: bool = false;
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

pub(crate) type DbRemove = CachedArgsAsync<DbRemoveEv>;

// -- DbContainsKey --

#[derive(Debug, Default)]
pub(crate) struct DbContainsKeyEv;

impl EvalCachedAsync for DbContainsKeyEv {
    const NAME: &str = "db_contains_key";
    const NEEDS_CALLSITE: bool = false;
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

pub(crate) type DbContainsKey = CachedArgsAsync<DbContainsKeyEv>;

// -- DbGetMany --

#[derive(Debug, Default)]
pub(crate) struct DbGetManyEv;

impl EvalCachedAsync for DbGetManyEv {
    const NAME: &str = "db_get_many";
    const NEEDS_CALLSITE: bool = false;
    type Args = (Arc<TreeInner>, GPooled<Vec<GPooled<Vec<u8>>>>);

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        let tree = get_tree_inner(cached, 0)?;
        let arr = match cached.0.get(1)?.as_ref()? {
            Value::Array(a) => a,
            _ => return None,
        };
        let mut keys = ENCODE_MANY_POOL.take();
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
                            None => return Err(errf!("DbErr", "failed to decode value")),
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

pub(crate) type DbGetMany = CachedArgsAsync<DbGetManyEv>;

// ── Key-value decode helper ─────────────────────────────────────

fn decode_kv_result(
    tree: &TreeInner,
    result: sled::Result<Option<(sled::IVec, sled::IVec)>>,
) -> Value {
    match result {
        Err(e) => errf!("DbErr", "{e}"),
        Ok(None) => Value::Null,
        Ok(Some((k, v))) => match (decode_key(tree.key_typ, &k), decode_value(&v)) {
            (Some(key), Some(val)) => Value::Array(ValArray::from([key, val])),
            _ => errf!("DbErr", "failed to decode entry"),
        },
    }
}

// ── Ordered access builtins ─────────────────────────────────────

// -- DbFirst --

#[derive(Debug, Default)]
pub(crate) struct DbFirstEv;

impl EvalCachedAsync for DbFirstEv {
    const NAME: &str = "db_first";
    const NEEDS_CALLSITE: bool = false;
    type Args = Arc<TreeInner>;

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        get_tree_inner(cached, 0)
    }

    fn eval(tree: Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            match tokio::task::spawn_blocking(move || {
                let result = tree.tree.first();
                decode_kv_result(&tree, result)
            })
            .await
            {
                Err(e) => errf!("DbErr", "task panicked: {e}"),
                Ok(v) => v,
            }
        }
    }
}

pub(crate) type DbFirst = CachedArgsAsync<DbFirstEv>;

// -- DbLast --

#[derive(Debug, Default)]
pub(crate) struct DbLastEv;

impl EvalCachedAsync for DbLastEv {
    const NAME: &str = "db_last";
    const NEEDS_CALLSITE: bool = false;
    type Args = Arc<TreeInner>;

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        get_tree_inner(cached, 0)
    }

    fn eval(tree: Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            match tokio::task::spawn_blocking(move || {
                let result = tree.tree.last();
                decode_kv_result(&tree, result)
            })
            .await
            {
                Err(e) => errf!("DbErr", "task panicked: {e}"),
                Ok(v) => v,
            }
        }
    }
}

pub(crate) type DbLast = CachedArgsAsync<DbLastEv>;

// -- DbPopMin --

#[derive(Debug, Default)]
pub(crate) struct DbPopMinEv;

impl EvalCachedAsync for DbPopMinEv {
    const NAME: &str = "db_pop_min";
    const NEEDS_CALLSITE: bool = false;
    type Args = Arc<TreeInner>;

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        get_tree_inner(cached, 0)
    }

    fn eval(tree: Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            match tokio::task::spawn_blocking(move || {
                let result = tree.tree.pop_min();
                decode_kv_result(&tree, result)
            })
            .await
            {
                Err(e) => errf!("DbErr", "task panicked: {e}"),
                Ok(v) => v,
            }
        }
    }
}

pub(crate) type DbPopMin = CachedArgsAsync<DbPopMinEv>;

// -- DbPopMax --

#[derive(Debug, Default)]
pub(crate) struct DbPopMaxEv;

impl EvalCachedAsync for DbPopMaxEv {
    const NAME: &str = "db_pop_max";
    const NEEDS_CALLSITE: bool = false;
    type Args = Arc<TreeInner>;

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        get_tree_inner(cached, 0)
    }

    fn eval(tree: Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            match tokio::task::spawn_blocking(move || {
                let result = tree.tree.pop_max();
                decode_kv_result(&tree, result)
            })
            .await
            {
                Err(e) => errf!("DbErr", "task panicked: {e}"),
                Ok(v) => v,
            }
        }
    }
}

pub(crate) type DbPopMax = CachedArgsAsync<DbPopMaxEv>;

// -- DbGetLt --

#[derive(Debug, Default)]
pub(crate) struct DbGetLtEv;

impl EvalCachedAsync for DbGetLtEv {
    const NAME: &str = "db_get_lt";
    const NEEDS_CALLSITE: bool = false;
    type Args = (Arc<TreeInner>, GPooled<Vec<u8>>);

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        let tree = get_tree_inner(cached, 0)?;
        let key_val = cached.0.get(1)?.as_ref()?;
        let key = encode_key(tree.key_typ, key_val)?;
        Some((tree, key))
    }

    fn eval((tree, key): Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            match tokio::task::spawn_blocking(move || {
                let result = tree.tree.get_lt(&*key);
                decode_kv_result(&tree, result)
            })
            .await
            {
                Err(e) => errf!("DbErr", "task panicked: {e}"),
                Ok(v) => v,
            }
        }
    }
}

pub(crate) type DbGetLt = CachedArgsAsync<DbGetLtEv>;

// -- DbGetGt --

#[derive(Debug, Default)]
pub(crate) struct DbGetGtEv;

impl EvalCachedAsync for DbGetGtEv {
    const NAME: &str = "db_get_gt";
    const NEEDS_CALLSITE: bool = false;
    type Args = (Arc<TreeInner>, GPooled<Vec<u8>>);

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        let tree = get_tree_inner(cached, 0)?;
        let key_val = cached.0.get(1)?.as_ref()?;
        let key = encode_key(tree.key_typ, key_val)?;
        Some((tree, key))
    }

    fn eval((tree, key): Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            match tokio::task::spawn_blocking(move || {
                let result = tree.tree.get_gt(&*key);
                decode_kv_result(&tree, result)
            })
            .await
            {
                Err(e) => errf!("DbErr", "task panicked: {e}"),
                Ok(v) => v,
            }
        }
    }
}

pub(crate) type DbGetGt = CachedArgsAsync<DbGetGtEv>;

// ── Atomic operations ───────────────────────────────────────────

// -- DbCompareAndSwap --

#[derive(Debug, Default)]
pub(crate) struct DbCompareAndSwapEv;

impl EvalCachedAsync for DbCompareAndSwapEv {
    const NAME: &str = "db_compare_and_swap";
    const NEEDS_CALLSITE: bool = false;
    type Args = (
        Arc<TreeInner>,
        GPooled<Vec<u8>>,
        Option<GPooled<Vec<u8>>>,
        Option<GPooled<Vec<u8>>>,
    );

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        let tree = get_tree_inner(cached, 0)?;
        let key_val = cached.0.get(1)?.as_ref()?;
        let key = encode_key(tree.key_typ, key_val)?;
        let old_val = match cached.0.get(2)?.as_ref()? {
            Value::Null => None,
            v => Some(encode_value(v)?),
        };
        let new_val = match cached.0.get(3)?.as_ref()? {
            Value::Null => None,
            v => Some(encode_value(v)?),
        };
        Some((tree, key, old_val, new_val))
    }

    fn eval(
        (tree, key, old_val, new_val): Self::Args,
    ) -> impl Future<Output = Value> + Send {
        async move {
            match tokio::task::spawn_blocking(move || {
                let old_ref: Option<&[u8]> = old_val.as_ref().map(|v| v.as_slice());
                let new_ref: Option<&[u8]> = new_val.as_ref().map(|v| v.as_slice());
                tree.tree.compare_and_swap(key.as_slice(), old_ref, new_ref)
            })
            .await
            {
                Err(e) => errf!("DbErr", "task panicked: {e}"),
                Ok(Err(e)) => errf!("DbErr", "{e}"),
                Ok(Ok(Ok(()))) => Value::Null,
                Ok(Ok(Err(cas_err))) => {
                    let current = match cas_err.current {
                        None => Value::Null,
                        Some(ivec) => match decode_value(&ivec) {
                            Some(v) => v,
                            None => {
                                return errf!("DbErr", "failed to decode current value")
                            }
                        },
                    };
                    Value::Array(ValArray::from([
                        Value::String(arcstr::literal!("Mismatch")),
                        current,
                    ]))
                }
            }
        }
    }
}

pub(crate) type DbCompareAndSwap = CachedArgsAsync<DbCompareAndSwapEv>;

// -- DbBatch --

#[derive(Debug, Default)]
pub(crate) struct DbBatchEv;

impl EvalCachedAsync for DbBatchEv {
    const NAME: &str = "db_batch";
    const NEEDS_CALLSITE: bool = false;
    type Args = (Arc<TreeInner>, sled::Batch);

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        let tree = get_tree_inner(cached, 0)?;
        let arr = match cached.0.get(1)?.as_ref()? {
            Value::Array(a) => a,
            _ => return None,
        };
        let batch = parse_batch_ops(tree.key_typ, arr)?;
        Some((tree, batch))
    }

    fn eval((tree, batch): Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            match tokio::task::spawn_blocking(move || tree.tree.apply_batch(batch)).await
            {
                Err(e) => errf!("DbErr", "task panicked: {e}"),
                Ok(Err(e)) => errf!("DbErr", "{e}"),
                Ok(Ok(())) => Value::Null,
            }
        }
    }
}

pub(crate) type DbBatch = CachedArgsAsync<DbBatchEv>;

// ── Collection introspection ────────────────────────────────────

// -- DbLen --

#[derive(Debug, Default)]
pub(crate) struct DbLenEv;

impl EvalCachedAsync for DbLenEv {
    const NAME: &str = "db_len";
    const NEEDS_CALLSITE: bool = false;
    type Args = Arc<TreeInner>;

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        get_tree_inner(cached, 0)
    }

    fn eval(tree: Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            match tokio::task::spawn_blocking(move || tree.tree.len()).await {
                Err(e) => errf!("DbErr", "task panicked: {e}"),
                Ok(len) => Value::U64(len as u64),
            }
        }
    }
}

pub(crate) type DbLen = CachedArgsAsync<DbLenEv>;

// -- DbIsEmpty --

#[derive(Debug, Default)]
pub(crate) struct DbIsEmptyEv;

impl EvalCachedAsync for DbIsEmptyEv {
    const NAME: &str = "db_is_empty";
    const NEEDS_CALLSITE: bool = false;
    type Args = Arc<TreeInner>;

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        get_tree_inner(cached, 0)
    }

    fn eval(tree: Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            match tokio::task::spawn_blocking(move || tree.tree.is_empty()).await {
                Err(e) => errf!("DbErr", "task panicked: {e}"),
                Ok(empty) => Value::Bool(empty),
            }
        }
    }
}

pub(crate) type DbIsEmpty = CachedArgsAsync<DbIsEmptyEv>;

// ── Database-level operations ───────────────────────────────────

// -- DbSizeOnDisk --

#[derive(Debug, Default)]
pub(crate) struct DbSizeOnDiskEv;

impl EvalCachedAsync for DbSizeOnDiskEv {
    const NAME: &str = "db_size_on_disk";
    const NEEDS_CALLSITE: bool = false;
    type Args = sled::Db;

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        get_db(cached, 0)
    }

    fn eval(db: Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            match tokio::task::spawn_blocking(move || db.size_on_disk()).await {
                Err(e) => errf!("DbErr", "task panicked: {e}"),
                Ok(Err(e)) => errf!("DbErr", "{e}"),
                Ok(Ok(size)) => Value::U64(size),
            }
        }
    }
}

pub(crate) type DbSizeOnDisk = CachedArgsAsync<DbSizeOnDiskEv>;

// -- DbWasRecovered --

#[derive(Debug, Default)]
pub(crate) struct DbWasRecoveredEv;

impl EvalCachedAsync for DbWasRecoveredEv {
    const NAME: &str = "db_was_recovered";
    const NEEDS_CALLSITE: bool = false;
    type Args = sled::Db;

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        get_db(cached, 0)
    }

    fn eval(db: Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            match tokio::task::spawn_blocking(move || db.was_recovered()).await {
                Err(e) => errf!("DbErr", "task panicked: {e}"),
                Ok(recovered) => Value::Bool(recovered),
            }
        }
    }
}

pub(crate) type DbWasRecovered = CachedArgsAsync<DbWasRecoveredEv>;

// -- DbChecksum --

#[derive(Debug, Default)]
pub(crate) struct DbChecksumEv;

impl EvalCachedAsync for DbChecksumEv {
    const NAME: &str = "db_checksum";
    const NEEDS_CALLSITE: bool = false;
    type Args = sled::Db;

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        get_db(cached, 0)
    }

    fn eval(db: Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            match tokio::task::spawn_blocking(move || db.checksum()).await {
                Err(e) => errf!("DbErr", "task panicked: {e}"),
                Ok(Err(e)) => errf!("DbErr", "{e}"),
                Ok(Ok(crc)) => Value::U32(crc),
            }
        }
    }
}

pub(crate) type DbChecksum = CachedArgsAsync<DbChecksumEv>;

// -- DbExport / DbImport serialization format --

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

// -- DbExport --

#[derive(Debug, Default)]
pub(crate) struct DbExportEv;

impl EvalCachedAsync for DbExportEv {
    const NAME: &str = "db_export";
    const NEEDS_CALLSITE: bool = false;
    type Args = (sled::Db, ArcStr);

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        let db = get_db(cached, 0)?;
        let path = cached.get::<ArcStr>(1)?;
        Some((db, path))
    }

    fn eval((db, path): Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            match tokio::task::spawn_blocking(move || {
                use std::io::Write;
                let data = ExportData {
                    trees: db
                        .export()
                        .into_iter()
                        .map(|(typ, name, iter)| ExportTree {
                            typ,
                            name,
                            entries: iter.collect(),
                        })
                        .collect(),
                };
                let mut buf = Vec::with_capacity(data.encoded_len());
                data.encode(&mut buf).map_err(|e| errf!("DbErr", "{e}"))?;
                let file =
                    std::fs::File::create(&*path).map_err(|e| errf!("DbErr", "{e}"))?;
                let mut w = std::io::BufWriter::new(file);
                w.write_all(&buf).map_err(|e| errf!("DbErr", "{e}"))?;
                w.flush().map_err(|e| errf!("DbErr", "{e}"))?;
                Ok(Value::Null)
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

pub(crate) type DbExport = CachedArgsAsync<DbExportEv>;

// -- DbImport --

#[derive(Debug, Default)]
pub(crate) struct DbImportEv;

impl EvalCachedAsync for DbImportEv {
    const NAME: &str = "db_import";
    const NEEDS_CALLSITE: bool = false;
    type Args = (sled::Db, ArcStr);

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        let db = get_db(cached, 0)?;
        let path = cached.get::<ArcStr>(1)?;
        Some((db, path))
    }

    fn eval((db, path): Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            match tokio::task::spawn_blocking(move || {
                let buf = std::fs::read(&*path).map_err(|e| errf!("DbErr", "{e}"))?;
                let data = ExportData::decode(&mut buf.as_slice())
                    .map_err(|e| errf!("DbErr", "{e}"))?;
                let collections: Vec<_> = data
                    .trees
                    .into_iter()
                    .map(|t| (t.typ, t.name, t.entries.into_iter()))
                    .collect();
                db.import(collections);
                Ok(Value::Null)
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

pub(crate) type DbImport = CachedArgsAsync<DbImportEv>;
