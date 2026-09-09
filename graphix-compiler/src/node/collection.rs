use super::{
    MAX_ARRAY_INIT_LEN, NOP, callsite::CallSite, genn, pattern::StructPatternNode,
};
use crate::{
    ApplyView, BindId, Event, ExecCtx, Node, NodeView, Refs, Rt, Scope, Tag, TagValue,
    Update, UserEvent,
    expr::{Expr, ExprId},
    fusion::{
        emit::{self, BodyCx, CompiledExpr, CompositeSource, scaffold},
        kernel_abi::{self, PrimType},
    },
    typ::{FnArgKind, FnType, Type},
    wrap,
};
use anyhow::{Result, bail};
use arcstr::{ArcStr, literal};
use cranelift_codegen::ir::{InstBuilder, Value as ClifValue};
use immutable_chunkmap::map::Map as CMap;
use netidx_value::ValArray;
use netidx_value::{Typ, Value};
use poolshark::local::LPooled;
use smallvec::smallvec;
use std::{fmt::Debug, marker::PhantomData};
use triomphe::Arc;

pub mod list {
    use super::*;

    /// The list rep: cons = a 2-slot array `[head, tail]`, nil = a
    /// clone of this static (not `Null`: `[List, null]` must not
    /// collapse). The discriminant is the length; only this module
    /// knows the layout.
    static EMPTY: std::sync::LazyLock<ValArray> =
        std::sync::LazyLock::new(|| ValArray::from_iter_exact(std::iter::empty()));

    pub fn nil() -> Value {
        Value::Array(EMPTY.clone())
    }

    pub fn cons(head: Value, tail: Value) -> Value {
        Value::Array(ValArray::from_iter_exact([head, tail].into_iter()))
    }

    pub fn split(v: &Value) -> Option<(&Value, &Value)> {
        match v {
            Value::Array(a) if a.len() == 2 => Some((&a[0], &a[1])),
            _ => None,
        }
    }

    pub fn is_nil(v: &Value) -> bool {
        matches!(v, Value::Array(a) if a.is_empty())
    }

    pub fn is_list(v: &Value) -> bool {
        is_nil(v) || split(v).is_some()
    }

    pub fn len(v: &Value) -> Option<usize> {
        let mut n = 0;
        let mut cur = v.clone();
        loop {
            if is_nil(&cur) {
                return Some(n);
            }
            let (_, tail) = split(&cur)?;
            n += 1;
            cur = tail.clone();
        }
    }

    pub fn from_iter(iter: impl IntoIterator<Item = Value>) -> Value {
        let mut values: LPooled<Vec<Value>> = iter.into_iter().collect();
        let mut result = nil();
        while let Some(value) = values.pop() {
            result = cons(value, result);
        }
        result
    }

    #[derive(Debug, Clone)]
    pub struct Iter {
        cur: Value,
    }

    impl Iter {
        pub fn new(value: Value) -> Self {
            Self { cur: value }
        }
    }

    impl Iterator for Iter {
        type Item = Value;

        fn next(&mut self) -> Option<Self::Item> {
            let cur = self.cur.clone();
            let (head, tail) = split(&cur)?;
            let head = head.clone();
            self.cur = tail.clone();
            Some(head)
        }
    }

    pub fn to_array(value: &Value) -> Option<ValArray> {
        is_list(value).then(|| ValArray::from_iter(Iter::new(value.clone())))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum CollectionIntrinsic {
    ArrayInit,
    ArrayMap,
    ArrayFilter,
    ArrayFilterMap,
    ArrayFlatMap,
    ArrayFind,
    ArrayFindMap,
    ArrayFold,
    ListInit,
    ListMap,
    ListFilter,
    ListFilterMap,
    ListFlatMap,
    ListFind,
    ListFindMap,
    ListFold,
    MapMap,
    MapFilter,
    MapFilterMap,
    MapFold,
}

impl CollectionIntrinsic {
    pub(crate) fn from_name(name: &str) -> Option<Self> {
        Some(match name {
            "array_init" => Self::ArrayInit,
            "array_map" => Self::ArrayMap,
            "array_filter" => Self::ArrayFilter,
            "array_filter_map" => Self::ArrayFilterMap,
            "array_flat_map" => Self::ArrayFlatMap,
            "array_find" => Self::ArrayFind,
            "array_find_map" => Self::ArrayFindMap,
            "array_fold" => Self::ArrayFold,
            "list_init" => Self::ListInit,
            "list_map" => Self::ListMap,
            "list_filter" => Self::ListFilter,
            "list_filter_map" => Self::ListFilterMap,
            "list_flat_map" => Self::ListFlatMap,
            "list_find" => Self::ListFind,
            "list_find_map" => Self::ListFindMap,
            "list_fold" => Self::ListFold,
            "map_map" => Self::MapMap,
            "map_filter" => Self::MapFilter,
            "map_filter_map" => Self::MapFilterMap,
            "map_fold" => Self::MapFold,
            _ => return None,
        })
    }

    pub(crate) fn build<R: Rt, E: UserEvent>(
        self,
        ctx: &mut ExecCtx<R, E>,
        spec: Expr,
        scope: &Scope,
        top_id: ExprId,
        typ: &Arc<FnType>,
        args: &[StructPatternNode],
    ) -> Result<Node<R, E>> {
        match self {
            Self::ArrayInit => {
                MapQ::<R, E, ArrayInit>::new(ctx, spec, scope, top_id, typ, args)
            }
            Self::ArrayMap => {
                MapQ::<R, E, ArrayMap>::new(ctx, spec, scope, top_id, typ, args)
            }
            Self::ArrayFilter => {
                MapQ::<R, E, ArrayFilter>::new(ctx, spec, scope, top_id, typ, args)
            }
            Self::ArrayFilterMap => {
                MapQ::<R, E, ArrayFilterMap>::new(ctx, spec, scope, top_id, typ, args)
            }
            Self::ArrayFlatMap => {
                MapQ::<R, E, ArrayFlatMap>::new(ctx, spec, scope, top_id, typ, args)
            }
            Self::ArrayFind => {
                MapQ::<R, E, ArrayFind>::new(ctx, spec, scope, top_id, typ, args)
            }
            Self::ArrayFindMap => {
                MapQ::<R, E, ArrayFindMap>::new(ctx, spec, scope, top_id, typ, args)
            }
            Self::ArrayFold => {
                FoldQ::<R, E, ArrayFold>::new(ctx, spec, scope, top_id, typ, args)
            }
            Self::ListInit => {
                MapQ::<R, E, ListInit>::new(ctx, spec, scope, top_id, typ, args)
            }
            Self::ListMap => {
                MapQ::<R, E, ListMap>::new(ctx, spec, scope, top_id, typ, args)
            }
            Self::ListFilter => {
                MapQ::<R, E, ListFilter>::new(ctx, spec, scope, top_id, typ, args)
            }
            Self::ListFilterMap => {
                MapQ::<R, E, ListFilterMap>::new(ctx, spec, scope, top_id, typ, args)
            }
            Self::ListFlatMap => {
                MapQ::<R, E, ListFlatMap>::new(ctx, spec, scope, top_id, typ, args)
            }
            Self::ListFind => {
                MapQ::<R, E, ListFind>::new(ctx, spec, scope, top_id, typ, args)
            }
            Self::ListFindMap => {
                MapQ::<R, E, ListFindMap>::new(ctx, spec, scope, top_id, typ, args)
            }
            Self::ListFold => {
                FoldQ::<R, E, ListFold>::new(ctx, spec, scope, top_id, typ, args)
            }
            Self::MapMap => {
                MapQ::<R, E, MapMap>::new(ctx, spec, scope, top_id, typ, args)
            }
            Self::MapFilter => {
                MapQ::<R, E, MapFilter>::new(ctx, spec, scope, top_id, typ, args)
            }
            Self::MapFilterMap => {
                MapQ::<R, E, MapFilterMap>::new(ctx, spec, scope, top_id, typ, args)
            }
            Self::MapFold => {
                FoldQ::<R, E, MapFold>::new(ctx, spec, scope, top_id, typ, args)
            }
        }
    }
}

trait MapCollection: Debug + Clone + Default + Send + Sync + 'static {
    fn len(&self) -> usize;
    fn values(&self) -> impl Iterator<Item = Value>;
    fn select(value: Value) -> Option<Self>;
    fn element_type(ft: &FnType) -> Result<Type>;
}

impl MapCollection for ValArray {
    fn len(&self) -> usize {
        self.as_ref().len()
    }

    fn values(&self) -> impl Iterator<Item = Value> {
        self.iter().cloned()
    }

    fn select(value: Value) -> Option<Self> {
        match value {
            Value::Array(a) => Some(a),
            _ => None,
        }
    }

    fn element_type(ft: &FnType) -> Result<Type> {
        match &ft.args[0].typ {
            Type::Array(t) => Ok((**t).clone()),
            t => bail!("expected Array, got {t}"),
        }
    }
}

type ValueMap = CMap<Value, Value, 32>;

/// The Map-HOF pair encoding: each `(k, v)` entry crosses the callback
/// as `Value::Array([k, v])`. Every encoder and decoder, interpreted
/// or JIT, goes through `make_pair` / [`split_pair`].
pub(crate) fn make_pair(key: &Value, value: &Value) -> Value {
    Value::Array(ValArray::from_iter_exact([key.clone(), value.clone()].into_iter()))
}

/// See [`make_pair`].
pub(crate) fn split_pair(value: &Value) -> Option<(Value, Value)> {
    match value {
        Value::Array(values) if values.len() == 2 => {
            Some((values[0].clone(), values[1].clone()))
        }
        _ => None,
    }
}

impl MapCollection for ValueMap {
    fn len(&self) -> usize {
        CMap::len(self)
    }

    fn values(&self) -> impl Iterator<Item = Value> {
        self.into_iter().map(|(k, v)| make_pair(k, v))
    }

    fn select(value: Value) -> Option<Self> {
        match value {
            Value::Map(m) => Some(m),
            _ => None,
        }
    }

    fn element_type(ft: &FnType) -> Result<Type> {
        match &ft.args[0].typ {
            Type::Map { key, value } => {
                Ok(Type::Tuple(Arc::from_iter([(**key).clone(), (**value).clone()])))
            }
            t => bail!("expected Map, got {t}"),
        }
    }
}

#[derive(Debug, Clone)]
struct ListCollection {
    value: Value,
    len: usize,
}

impl Default for ListCollection {
    fn default() -> Self {
        Self { value: list::nil(), len: 0 }
    }
}

impl MapCollection for ListCollection {
    fn len(&self) -> usize {
        self.len
    }

    fn values(&self) -> impl Iterator<Item = Value> {
        list::Iter::new(self.value.clone())
    }

    fn select(value: Value) -> Option<Self> {
        let len = list::len(&value)?;
        Some(Self { value, len })
    }

    fn element_type(ft: &FnType) -> Result<Type> {
        match &ft.args[0].typ {
            Type::List(t) => return Ok((**t).clone()),
            Type::Abstract { params, .. } if !params.is_empty() => {
                return Ok(params[0].clone());
            }
            Type::Ref(tr) if !tr.params.is_empty() => return Ok(tr.params[0].clone()),
            _ => (),
        }
        for arg in ft.args.iter() {
            if let Type::Fn(inner) = &arg.typ
                && let Some(arg) = inner.args.last()
            {
                return Ok(arg.typ.clone());
            }
        }
        bail!("cannot extract List element type from {}", ft.args[0].typ)
    }
}

#[derive(Debug, Clone, Default)]
struct IndexRange(usize);

impl MapCollection for IndexRange {
    fn len(&self) -> usize {
        self.0
    }

    fn values(&self) -> impl Iterator<Item = Value> {
        fn value(i: usize) -> Value {
            Value::I64(i as i64)
        }
        (0..self.0).map(value)
    }

    fn select(value: Value) -> Option<Self> {
        let Value::I64(n) = value else { return None };
        if n > MAX_ARRAY_INIT_LEN {
            log::error!(
                "collection init size {n} exceeds the {MAX_ARRAY_INIT_LEN} element limit"
            );
            return None;
        }
        Some(Self(n.max(0) as usize))
    }

    fn element_type(_: &FnType) -> Result<Type> {
        Ok(Type::Primitive(Typ::I64.into()))
    }
}

#[derive(Debug)]
struct Slot<R: Rt, E: UserEvent> {
    id: BindId,
    call: Node<R, E>,
    value: Option<Value>,
    tag: Tag,
}

impl<R: Rt, E: UserEvent> Slot<R, E> {
    fn new(
        ctx: &mut ExecCtx<R, E>,
        scope: &Scope,
        top_id: ExprId,
        callback: BindId,
        callback_type: &Arc<FnType>,
        element_type: &Type,
        prototype: bool,
        resolved: Option<Value>,
    ) -> Self {
        let (id, element) = genn::bind(
            ctx,
            &scope.lexical,
            "collection_element",
            element_type.clone(),
            top_id,
        );
        let function = callback_fnode(ctx, callback, callback_type, top_id, resolved);
        let call = if prototype {
            genn::apply_prototype(
                function,
                scope.clone(),
                smallvec![element],
                callback_type,
                top_id,
            )
        } else {
            genn::apply(
                function,
                scope.clone(),
                smallvec![element],
                callback_type,
                top_id,
            )
        };
        Self { id, call, value: None, tag: Tag::STALE }
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.call.delete(ctx);
        ctx.rt.store_remove(&self.id);
        ctx.env.unbind_variable(self.id);
    }
}

trait MapFn<R: Rt, E: UserEvent>: Debug + Default + Send + Sync + 'static {
    type Collection: MapCollection;

    /// `true` iff `finish` reads the source elements into the result
    /// (filter, find): a same-length source refresh with quiet callback
    /// slots must then still fire the production.
    const PASS_THROUGH: bool = false;

    fn finish(
        &mut self,
        slots: &[Slot<R, E>],
        source: &Self::Collection,
    ) -> Option<Value>;

    fn emit_clif(
        _cx: &mut BodyCx,
        _source: &Node<R, E>,
        _body: &Node<R, E>,
        _param: &CallbackParam,
        _element_type: &Type,
    ) -> Result<Option<CompiledExpr>> {
        Ok(None)
    }
}

#[derive(Debug)]
struct CallbackParam {
    name: ArcStr,
    id: Option<BindId>,
    binds: Vec<(BindId, usize)>,
}

/// The callback's `index`-th positional parameter; `None` for a
/// callback with labeled parameters, which the collection interprets.
fn callback_param<R: Rt, E: UserEvent>(
    callback: &super::lambda::GXLambda<R, E>,
    index: usize,
    fallback: ArcStr,
) -> Option<CallbackParam> {
    if callback.typ().first_positional() > 0 {
        return None;
    }
    match callback.args().get(index)?.tuple_leaves() {
        Some(binds) => Some(CallbackParam { name: fallback, id: None, binds }),
        None => {
            let name = match &callback.typ().args.get(index)?.kind {
                FnArgKind::Positional { name: Some(name) }
                | FnArgKind::Labeled { name, .. } => name.clone(),
                _ => return None,
            };
            Some(CallbackParam {
                name,
                id: callback.args()[index].single_bind_id(),
                binds: Vec::new(),
            })
        }
    }
}

fn bindable_array_element(
    typ: &Type,
    binds: &[(BindId, usize)],
) -> Option<(Type, Vec<(BindId, usize, scaffold::LeafShape)>)> {
    use kernel_abi::AbiKind;

    let typ = kernel_abi::freeze_for_abi_normalized(typ)?;
    let leaves = scaffold::elem_leaves(&typ, binds)?;
    match kernel_abi::abi_kind(&typ) {
        Some(
            AbiKind::Scalar(_)
            | AbiKind::Array
            | AbiKind::Tuple
            | AbiKind::Struct
            | AbiKind::String
            | AbiKind::Variant
            | AbiKind::Nullable
            | AbiKind::Value,
        ) => Some((typ, leaves)),
        _ => None,
    }
}

fn is_unit_or_null(typ: &Type) -> bool {
    use kernel_abi::AbiKind;

    matches!(kernel_abi::abi_kind(typ), Some(AbiKind::Unit | AbiKind::Null))
}

/// Whether a frozen type admits `null`, filter_map's drop marker.
/// An unknown shape answers true, so the caller keeps interpreting.
fn frozen_may_be_null(t: &Type) -> bool {
    t.with_deref(|t| match t {
        Some(Type::Primitive(p)) => p.contains(netidx_value::Typ::Null),
        Some(Type::Set(ms)) => ms.iter().any(frozen_may_be_null),
        Some(
            Type::Array(_)
            | Type::List(_)
            | Type::Tuple(_)
            | Type::Struct(_)
            | Type::Variant(_, _)
            | Type::Fn(_)
            | Type::Error(_)
            | Type::Map { .. }
            | Type::Abstract { .. }
            | Type::ByRef(_),
        ) => false,
        _ => true,
    })
}

/// Fold the loop's [`scaffold::SlotFlags`] and the source's firing
/// into the emitted result — the shared tail of every kind emitter.
fn finish_loop_result(
    cx: &mut BodyCx,
    result: CompiledExpr,
    mut flags: scaffold::SlotFlags,
    source: &CompiledExpr,
    source_invariant: bool,
) -> CompiledExpr {
    if source_invariant {
        flags.set_src_invariant();
    }
    flags.apply(cx, result, &[source.disc])
}

/// Emit a List/Map HOF source: marshal the collection Value owned and
/// flatten it to a fresh ValArray through `helper`, which consumes it.
/// Returns the source's (disc, payload) and the [`scaffold::ArraySrc`]
/// that owns the flattened array.
fn emit_flattened_source<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    source: &Node<R, E>,
    helper: &'static str,
) -> Result<(CompiledExpr, scaffold::ArraySrc)> {
    let value = emit::emit_owned_value_operand_node(cx, source)?;
    let flatten = cx.helper(helper)?;
    let call = cx.b.ins().call(flatten, &[value.disc, value.payload]);
    let ptr = cx.b.inst_results(call)[0];
    Ok((value, scaffold::ArraySrc { ptr, disc: value.disc, owned: true }))
}

/// The exit boundary for collection-returning loops: consume the
/// loop's finalize'd ValArray and rebuild the collection Value
/// (`graphix_valarray_into_list` / `graphix_valarray_into_cmap`).
fn convert_collection_result(
    cx: &mut BodyCx,
    ptr: ClifValue,
    helper: &'static str,
) -> Result<CompiledExpr> {
    let f = cx.helper(helper)?;
    let call = cx.b.ins().call(f, &[ptr]);
    let rs = cx.b.inst_results(call);
    let (disc, payload) = (rs[0], rs[1]);
    Ok(CompiledExpr::new(disc, payload))
}

#[derive(Debug)]
pub struct MapQBase<R: Rt, E: UserEvent> {
    pub(crate) source: Node<R, E>,
    pub(crate) prototype: Node<R, E>,
    element_type: Type,
    emit_call:
        fn(&MapQBase<R, E>, &CallSite<R, E>, &mut BodyCx) -> Result<Option<CompiledExpr>>,
    prototype_id: BindId,
    spec: Expr,
    typ: Type,
}

impl<R: Rt, E: UserEvent> MapQBase<R, E> {
    pub(crate) fn emit_clif_call(
        &self,
        callsite: &CallSite<R, E>,
        cx: &mut BodyCx,
    ) -> Result<Option<CompiledExpr>> {
        (self.emit_call)(self, callsite, cx)
    }

    pub(crate) fn callback_body(&self) -> Option<&Node<R, E>> {
        callback(&self.prototype).map(super::lambda::GXLambda::body)
    }
}

#[derive(Debug)]
struct MapQ<R: Rt, E: UserEvent, T: MapFn<R, E>> {
    /// Set by `sleep()`, taken by the next update.
    slept: bool,
    base: MapQBase<R, E>,
    scope: Scope,
    callback: BindId,
    callback_type: Arc<FnType>,
    top_id: ExprId,
    slots: LPooled<Vec<Slot<R, E>>>,
    current: T::Collection,
    operation: T,
    resident: TagValue,
}

impl<R: Rt, E: UserEvent, T: MapFn<R, E>> MapQ<R, E, T> {
    fn new(
        ctx: &mut ExecCtx<R, E>,
        spec: Expr,
        scope: &Scope,
        top_id: ExprId,
        typ: &Arc<FnType>,
        args: &[StructPatternNode],
    ) -> Result<Node<R, E>> {
        if typ.args.len() != 2 || args.len() != 2 {
            bail!("collection map intrinsic requires two arguments")
        }
        let source_id = args[0].single_bind_id().ok_or_else(|| {
            anyhow::anyhow!("collection source argument must be a name")
        })?;
        let callback = args[1].single_bind_id().ok_or_else(|| {
            anyhow::anyhow!("collection callback argument must be a name")
        })?;
        let callback_type = match &typ.args[1].typ {
            Type::Fn(ft) => ft.clone(),
            t => bail!("collection callback must be a function, got {t}"),
        };
        let element_type = T::Collection::element_type(typ)?;
        let source = genn::reference(ctx, source_id, typ.args[0].typ.clone(), top_id);
        let prototype = Slot::new(
            ctx,
            scope,
            top_id,
            callback,
            &callback_type,
            &element_type,
            true,
            None,
        );
        Ok(Node::new(Self {
            slept: false,
            base: MapQBase {
                source,
                prototype: prototype.call,
                element_type,
                emit_call: emit_map_call::<R, E, T>,
                prototype_id: prototype.id,
                spec,
                typ: typ.rtype.clone(),
            },
            scope: scope.clone(),
            callback,
            callback_type,
            top_id,
            slots: LPooled::take(),
            current: T::Collection::default(),
            operation: T::default(),
            resident: TagValue::phantom(),
        }))
    }

    fn add_slot(&mut self, ctx: &mut ExecCtx<R, E>) {
        let resolved = prototype_def(ctx, &self.base.prototype);
        self.slots.push(Slot::new(
            ctx,
            &self.scope,
            self.top_id,
            self.callback,
            &self.callback_type,
            &self.base.element_type,
            false,
            resolved,
        ));
    }
}

/// Fold one slot production into the collection's tag via [`Tag::join`].
fn merge_tag(current: Option<Tag>, next: Tag) -> Option<Tag> {
    Some(match current {
        None => next,
        Some(tag) => tag.join(next),
    })
}

impl<R: Rt, E: UserEvent, T: MapFn<R, E>> Update<R, E> for MapQ<R, E, T> {
    fn fuse(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<Option<Node<R, E>>> {
        // The fuse driver never descends a collection callback, so the
        // prototype body's attributes dispatch here.
        if !ctx.attr_census.lock().is_empty() {
            if let Some(body) = self.base.callback_body() {
                crate::fusion::check_attributes_subtree(body, ctx)?;
            }
        }
        Ok(None)
    }

    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> &TagValue {
        let woke = std::mem::take(&mut self.slept) && ctx.frame_depth == 0;
        let old_len = self.slots.len();
        let mut production = None;
        let mut resized = false;
        let mut forced_taint = false;
        let src_trig;
        {
            let (tag, sval) = {
                let tv = self.base.source.update(ctx, event);
                let tag = tv.tag();
                (tag, if tag.is_bottom() { None } else { Some(tv.value_cloned()) })
            };
            src_trig = tag.triggers();
            // A tainted or unselectable source is bottom; the slot walk
            // still runs so slot-internal state sees this cycle's events.
            if tag.is_tainted() {
                forced_taint = true;
            } else if let Some(source) =
                sval.and_then(|value| T::Collection::select(value))
            {
                while self.slots.len() > source.len() {
                    match self.slots.last_mut() {
                        Some(slot) => slot.delete(ctx),
                        None => return self.resident.ride(),
                    }
                    self.slots.pop();
                    resized = true;
                }
                while self.slots.len() < source.len() {
                    self.add_slot(ctx);
                    resized = true;
                }
                for (slot, value) in self.slots.iter().zip(source.values()) {
                    ctx.rt.store_insert(slot.id, TagValue::fired(value.clone()));
                    event.variables.insert(slot.id, TagValue::tagged(value, tag));
                }
                self.current = source;
                if resized || T::PASS_THROUGH {
                    production = merge_tag(production, tag);
                }
                if self.slots.is_empty() {
                    match self.operation.finish(&self.slots, &self.current) {
                        Some(value) => {
                            return self.resident.set(TagValue::tagged(value, tag));
                        }
                        None => return self.resident.ride(),
                    }
                }
            } else {
                // Slots stay retained: bottom is not a reset.
                forced_taint = true;
            }
        }

        let saved_init = event.init;
        for i in 0..self.slots.len() {
            if ctx.interrupted() {
                event.init = saved_init;
                return self.resident.ride();
            }
            // A fresh slot's first dispatch runs under a forced init view.
            if i >= old_len {
                event.init = true;
            }
            let tv = self.slots[i].call.update(ctx, event).clone();
            let tag = tv.tag();
            if crate::dbgenv::gxdbg_slot() {
                eprintln!(
                    "SLOT call[{i}] produced tag={} fresh={}",
                    tag.bits(),
                    i >= old_len
                );
            }
            // Only triggering productions fold into the firing decision;
            // any triggering bottom taints its slot and poisons the
            // collection. `value` is sleep/frame memory only.
            if tag.triggers() {
                if tag.is_bottom() {
                    production = merge_tag(production, tag);
                    self.slots[i].tag = Tag::TAINT;
                } else {
                    production = merge_tag(production, tag);
                    self.slots[i].value = Some(tv.value());
                    self.slots[i].tag = Tag::STALE;
                }
            } else if !tag.is_bottom() {
                // A stale production must still fill the slot, or the
                // collection can never build.
                self.slots[i].value = Some(tv.value());
            }
        }
        event.init = saved_init;

        if forced_taint {
            return if src_trig {
                self.resident.set(TagValue::tagged(Value::Null, Tag::FRESH_BOTTOM))
            } else {
                self.resident.ride()
            };
        }
        // A slot's taint is persistent until a clean production, so
        // bottomness is a question about the slots now; the production
        // tag decides only the fired bit. The resident never substitutes
        // for a poisoned production.
        let poisoned = self.slots.iter().any(|slot| slot.tag.is_tainted());
        if crate::dbgenv::gxdbg_slot() {
            eprintln!(
                "SLOT map prod={:?} resized={resized} forced={forced_taint} poisoned={poisoned} slots={:?}",
                production.map(|t| t.bits()),
                self.slots
                    .iter()
                    .map(|s| (s.tag.bits(), s.value.is_some()))
                    .collect::<Vec<_>>(),
            );
        }
        let tag = match production {
            Some(tag) => tag,
            None if poisoned => {
                return self
                    .resident
                    .set(TagValue::tagged(Value::Null, Tag::STALE_BOTTOM));
            }
            // After a sleep the resident may lag the stale-refreshed
            // slots; rebuild quietly (design/wake_catchup.md).
            None if woke
                && !self.slots.is_empty()
                && self.slots.iter().all(|slot| slot.value.is_some()) =>
            {
                Tag::STALE
            }
            None => return self.resident.ride(),
        };
        if tag.is_tainted() || poisoned {
            let t = if tag.triggers() { Tag::FRESH_BOTTOM } else { Tag::STALE_BOTTOM };
            return self.resident.set(TagValue::tagged(Value::Null, t));
        }
        if self.slots.iter().all(|slot| slot.value.is_some()) {
            match self.operation.finish(&self.slots, &self.current) {
                Some(value) => self.resident.set(TagValue::tagged(value, tag)),
                None => self.resident.ride(),
            }
        } else if tag.triggers() {
            self.resident.set(TagValue::tagged(Value::Null, Tag::FRESH_BOTTOM))
        } else {
            self.resident.ride()
        }
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        let Self { base, slots, .. } = self;
        base.source.delete(ctx);
        base.prototype.delete(ctx);
        ctx.rt.store_remove(&base.prototype_id);
        ctx.env.unbind_variable(base.prototype_id);
        for slot in slots.iter_mut() {
            slot.delete(ctx);
        }
    }

    fn typecheck0(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        wrap!(self.base.source, self.base.source.typecheck0(ctx))?;
        wrap!(self.base.prototype, self.base.prototype.typecheck0(ctx))
    }

    fn typecheck1(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        wrap!(self.base.source, self.base.source.typecheck1(ctx))?;
        wrap!(self.base.prototype, self.base.prototype.typecheck1(ctx))
    }

    fn typ(&self) -> &Type {
        &self.base.typ
    }

    fn refs(&self, refs: &mut Refs) {
        self.base.source.refs(refs);
        refs.bound.insert(self.base.prototype_id);
        self.base.prototype.refs(refs);
    }

    fn spec(&self) -> &Expr {
        &self.base.spec
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.slept = true;
        // Slot values survive sleep: sleep is pause.
        self.base.source.sleep(ctx);
        for slot in self.slots.iter_mut() {
            slot.call.sleep(ctx);
        }
    }

    fn reset_replay(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.base.source.reset_replay(ctx);
        self.current = T::Collection::default();
        for slot in self.slots.iter_mut() {
            slot.value = None;
            slot.tag = Tag::STALE;
            slot.call.reset_replay(ctx);
        }
    }

    fn view(&self) -> NodeView<'_, R, E> {
        NodeView::MapQ(&self.base)
    }

    fn emit_clif(&self, cx: &mut BodyCx) -> Result<CompiledExpr> {
        emit_map::<R, E, T>(&self.base, &self.base.source, cx)?
            .ok_or_else(|| anyhow::anyhow!("collection operation does not emit CLIF"))
    }
}

fn emit_map<R: Rt, E: UserEvent, T: MapFn<R, E>>(
    base: &MapQBase<R, E>,
    source: &Node<R, E>,
    cx: &mut BodyCx,
) -> Result<Option<CompiledExpr>> {
    if emit::node_is_bottom(source) {
        return Ok(None);
    }
    let Some(callback) = callback(&base.prototype) else { return Ok(None) };
    let Some(param) = callback_param(callback, 0, literal!("__elem")) else {
        return Ok(None);
    };
    T::emit_clif(cx, source, callback.body(), &param, &base.element_type)
}

fn emit_map_call<R: Rt, E: UserEvent, T: MapFn<R, E>>(
    base: &MapQBase<R, E>,
    callsite: &CallSite<R, E>,
    cx: &mut BodyCx,
) -> Result<Option<CompiledExpr>> {
    let Some(source) = callsite.arg_positional(0) else {
        return Ok(None);
    };
    let prev = cx.swap_collection_site(Some(callsite.spec.id));
    let r = emit_map::<R, E, T>(base, source, cx);
    cx.swap_collection_site(prev);
    r
}

trait FoldFn<R: Rt, E: UserEvent>: Debug + Send + Sync + 'static {
    type Collection: MapCollection;

    fn emit_clif(
        _cx: &mut BodyCx,
        _source: &Node<R, E>,
        _init: &Node<R, E>,
        _body: &Node<R, E>,
        _acc: &CallbackParam,
        _element: &CallbackParam,
        _acc_type: &Type,
        _element_type: &Type,
    ) -> Result<Option<CompiledExpr>> {
        Ok(None)
    }
}

#[derive(Debug)]
struct FoldSlot<R: Rt, E: UserEvent> {
    acc_id: BindId,
    element_id: BindId,
    call: Node<R, E>,
    cycle: Option<Value>,
    held: Option<Value>,
    tag: Tag,
}

impl<R: Rt, E: UserEvent> FoldSlot<R, E> {
    fn new(
        ctx: &mut ExecCtx<R, E>,
        scope: &Scope,
        top_id: ExprId,
        callback: BindId,
        callback_type: &Arc<FnType>,
        acc_type: &Type,
        element_type: &Type,
        prototype: bool,
        resolved: Option<Value>,
    ) -> Self {
        let (acc_id, acc) =
            genn::bind(ctx, &scope.lexical, "collection_acc", acc_type.clone(), top_id);
        let (element_id, element) = genn::bind(
            ctx,
            &scope.lexical,
            "collection_element",
            element_type.clone(),
            top_id,
        );
        let function = callback_fnode(ctx, callback, callback_type, top_id, resolved);
        let call = if prototype {
            genn::apply_prototype(
                function,
                scope.clone(),
                smallvec![acc, element],
                callback_type,
                top_id,
            )
        } else {
            genn::apply(
                function,
                scope.clone(),
                smallvec![acc, element],
                callback_type,
                top_id,
            )
        };
        Self { acc_id, element_id, call, cycle: None, held: None, tag: Tag::STALE }
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.call.delete(ctx);
        for id in [self.acc_id, self.element_id] {
            ctx.rt.store_remove(&id);
            ctx.env.unbind_variable(id);
        }
    }
}

#[derive(Debug)]
pub struct FoldQBase<R: Rt, E: UserEvent> {
    pub(crate) source: Node<R, E>,
    pub(crate) init: Node<R, E>,
    pub(crate) prototype: Node<R, E>,
    element_type: Type,
    emit_call: fn(
        &FoldQBase<R, E>,
        &CallSite<R, E>,
        &mut BodyCx,
    ) -> Result<Option<CompiledExpr>>,
    prototype_ids: [BindId; 2],
    spec: Expr,
    typ: Type,
}

impl<R: Rt, E: UserEvent> FoldQBase<R, E> {
    pub(crate) fn emit_clif_call(
        &self,
        callsite: &CallSite<R, E>,
        cx: &mut BodyCx,
    ) -> Result<Option<CompiledExpr>> {
        (self.emit_call)(self, callsite, cx)
    }

    pub(crate) fn callback_body(&self) -> Option<&Node<R, E>> {
        callback(&self.prototype).map(super::lambda::GXLambda::body)
    }
}

#[derive(Debug)]
struct FoldQ<R: Rt, E: UserEvent, T: FoldFn<R, E>> {
    base: FoldQBase<R, E>,
    scope: Scope,
    callback: BindId,
    callback_type: Arc<FnType>,
    acc_type: Type,
    top_id: ExprId,
    slots: LPooled<Vec<FoldSlot<R, E>>>,
    init: Option<Value>,
    source_present: bool,
    operation: PhantomData<T>,
    resident: TagValue,
}

impl<R: Rt, E: UserEvent, T: FoldFn<R, E>> FoldQ<R, E, T> {
    fn new(
        ctx: &mut ExecCtx<R, E>,
        spec: Expr,
        scope: &Scope,
        top_id: ExprId,
        typ: &Arc<FnType>,
        args: &[StructPatternNode],
    ) -> Result<Node<R, E>> {
        if typ.args.len() != 3 || args.len() != 3 {
            bail!("collection fold intrinsic requires three arguments")
        }
        let source_id = args[0].single_bind_id().ok_or_else(|| {
            anyhow::anyhow!("collection source argument must be a name")
        })?;
        let init_id = args[1]
            .single_bind_id()
            .ok_or_else(|| anyhow::anyhow!("collection init argument must be a name"))?;
        let callback = args[2].single_bind_id().ok_or_else(|| {
            anyhow::anyhow!("collection callback argument must be a name")
        })?;
        let callback_type = match &typ.args[2].typ {
            Type::Fn(ft) => ft.clone(),
            t => bail!("collection callback must be a function, got {t}"),
        };
        let acc_type = typ.args[1].typ.clone();
        let element_type = T::Collection::element_type(typ)?;
        let source = genn::reference(ctx, source_id, typ.args[0].typ.clone(), top_id);
        let init = genn::reference(ctx, init_id, acc_type.clone(), top_id);
        let prototype = FoldSlot::new(
            ctx,
            scope,
            top_id,
            callback,
            &callback_type,
            &acc_type,
            &element_type,
            true,
            None,
        );
        Ok(Node::new(Self {
            base: FoldQBase {
                source,
                init,
                prototype: prototype.call,
                element_type,
                emit_call: emit_fold_call::<R, E, T>,
                prototype_ids: [prototype.acc_id, prototype.element_id],
                spec,
                typ: typ.rtype.clone(),
            },
            scope: scope.clone(),
            callback,
            callback_type,
            acc_type,
            top_id,
            slots: LPooled::take(),
            init: None,
            source_present: false,
            operation: PhantomData,
            resident: TagValue::phantom(),
        }))
    }

    fn add_slot(&mut self, ctx: &mut ExecCtx<R, E>) {
        let resolved = prototype_def(ctx, &self.base.prototype);
        self.slots.push(FoldSlot::new(
            ctx,
            &self.scope,
            self.top_id,
            self.callback,
            &self.callback_type,
            &self.acc_type,
            &self.base.element_type,
            false,
            resolved,
        ));
    }
}

impl<R: Rt, E: UserEvent, T: FoldFn<R, E>> Update<R, E> for FoldQ<R, E, T> {
    fn fuse(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<Option<Node<R, E>>> {
        // The fuse driver never descends a collection callback, so the
        // prototype body's attributes dispatch here.
        if !ctx.attr_census.lock().is_empty() {
            if let Some(body) = self.base.callback_body() {
                crate::fusion::check_attributes_subtree(body, ctx)?;
            }
        }
        Ok(None)
    }

    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> &TagValue {
        let old_len = self.slots.len();
        let mut resized = false;
        let mut forced_taint = false;
        let mut source_tag = None;
        let src_trig;
        {
            let (tag, sval) = {
                let tv = self.base.source.update(ctx, event);
                let tag = tv.tag();
                (tag, if tag.is_bottom() { None } else { Some(tv.value_cloned()) })
            };
            src_trig = tag.triggers();
            // A tainted or unselectable source is bottom; the slot walk
            // still runs so slot-internal state sees this cycle's events.
            // A tainted init is a poisoned delivery instead, see below.
            if tag.is_tainted() {
                forced_taint = true;
            } else if let Some(source) =
                sval.and_then(|value| T::Collection::select(value))
            {
                self.source_present = true;
                source_tag = Some(tag);
                while self.slots.len() > source.len() {
                    match self.slots.last_mut() {
                        Some(slot) => slot.delete(ctx),
                        None => return self.resident.ride(),
                    }
                    self.slots.pop();
                    resized = true;
                }
                while self.slots.len() < source.len() {
                    self.add_slot(ctx);
                    resized = true;
                }
                for (slot, value) in self.slots.iter().zip(source.values()) {
                    ctx.rt.store_insert(slot.element_id, TagValue::fired(value.clone()));
                    event.variables.insert(slot.element_id, TagValue::tagged(value, tag));
                }
            } else {
                // Slots stay retained: bottom is not a reset.
                forced_taint = true;
            }
        }

        let init_tag;
        {
            let (tag, ival) = {
                let tv = self.base.init.update(ctx, event);
                let tag = tv.tag();
                (tag, if tag.is_bottom() { None } else { Some(tv.value_cloned()) })
            };
            init_tag = Some(tag);
            if tag.is_tainted() {
                // A tainted init is a poisoned delivery to slot 0's acc,
                // not a whole-fold abort: a callback that never consumes
                // the acc recovers. The delivery mirrors the init's tag.
                if let Some(slot) = self.slots.first() {
                    let poison = if tag.triggers() {
                        Tag::FRESH_BOTTOM
                    } else {
                        Tag::STALE_BOTTOM
                    };
                    event
                        .variables
                        .insert(slot.acc_id, TagValue::tagged(Value::Null, poison));
                }
            } else {
                self.init = ival;
                if let (Some(slot), Some(value)) = (self.slots.first(), self.init.clone())
                {
                    ctx.rt.store_insert(slot.acc_id, TagValue::fired(value.clone()));
                    event.variables.insert(slot.acc_id, TagValue::tagged(value, tag));
                }
            }
        }

        if self.slots.is_empty() && self.source_present && !forced_taint {
            // A bottomed init delivery poisons the empty fold this cycle;
            // the retained init serves only quiet cycles.
            if let Some(t) = init_tag {
                if t.is_bottom() {
                    return if t.triggers() {
                        self.resident
                            .set(TagValue::tagged(Value::Null, Tag::FRESH_BOTTOM))
                    } else {
                        self.resident.ride()
                    };
                }
            }
            let tag = match (source_tag, init_tag) {
                (None, None) => return self.resident.ride(),
                (Some(a), Some(b)) => match merge_tag(Some(a), b) {
                    Some(tag) => tag,
                    None => return self.resident.ride(),
                },
                (Some(tag), None) | (None, Some(tag)) => tag,
            };
            return match self.init.clone() {
                Some(value) => self.resident.set(TagValue::tagged(value, tag)),
                None if tag.triggers() => {
                    self.resident.set(TagValue::tagged(Value::Null, Tag::FRESH_BOTTOM))
                }
                None => self.resident.ride(),
            };
        }

        // Only slot productions seed the firing decision: a source or
        // init delivery reaches the result only through a slot that
        // consumes it. A triggering taint still counts, for the bottom arm.
        let mut any_trig = forced_taint && src_trig;
        // A resize must propagate the chain's current poison to a fresh
        // slot's seed, not resurrect `held`.
        let mut prev_slot_bottom = false;
        let saved_init = event.init;
        for i in 0..self.slots.len() {
            if ctx.interrupted() {
                event.init = saved_init;
                return self.resident.ride();
            }
            // A fresh slot's first dispatch runs under a forced init view.
            if i >= old_len {
                event.init = true;
                let acc_id = self.slots[i].acc_id;
                // The seed is the chain's current state, never a
                // last-good substitute for a bottomed chain.
                let incoming_bottom = if i == 0 {
                    init_tag.is_some_and(|t| t.is_bottom())
                } else {
                    prev_slot_bottom
                };
                if incoming_bottom {
                    event
                        .variables
                        .insert(acc_id, TagValue::tagged(Value::Null, Tag::FRESH_BOTTOM));
                } else {
                    let seed = if i == 0 {
                        self.init.clone()
                    } else {
                        self.slots[i - 1].held.clone()
                    };
                    if let Some(value) = seed {
                        ctx.rt.store_insert(acc_id, TagValue::fired(value.clone()));
                        event.variables.insert(acc_id, TagValue::fired(value));
                    }
                }
            }
            let tv = self.slots[i].call.update(ctx, event).clone();
            let tag = tv.tag();
            prev_slot_bottom = tag.is_bottom();
            // Only triggering productions advance the fold; a bottomed
            // slot delivers a poisoned acc rather than withdrawing it.
            if tag.triggers() {
                any_trig = true;
                if tag.is_bottom() {
                    // The bottom travels the acc chain; `held` is
                    // sleep/frame memory and never substitutes for it.
                    self.slots[i].tag = tag;
                    self.slots[i].cycle = None;
                    if i + 1 < self.slots.len() {
                        let next = self.slots[i + 1].acc_id;
                        event.variables.insert(
                            next,
                            TagValue::tagged(Value::Null, Tag::FRESH_BOTTOM),
                        );
                    }
                } else {
                    self.slots[i].tag = tag;
                    let value = tv.value();
                    self.slots[i].cycle = Some(value.clone());
                    self.slots[i].held = Some(value.clone());
                    if i + 1 < self.slots.len() {
                        let next = self.slots[i + 1].acc_id;
                        ctx.rt.store_insert(next, TagValue::fired(value.clone()));
                        event.variables.insert(next, TagValue::tagged(value, tag));
                    }
                }
            } else if !tag.is_bottom() {
                // A stale production advances the acc chain silently.
                self.slots[i].tag = Tag::STALE;
                let value = tv.value();
                self.slots[i].cycle = Some(value.clone());
                self.slots[i].held = Some(value.clone());
                if i + 1 < self.slots.len() {
                    let next = self.slots[i + 1].acc_id;
                    ctx.rt.store_insert(next, TagValue::fired(value.clone()));
                    event.variables.insert(next, TagValue::stale(value));
                }
            } else {
                // A standing bottom still poisons the chain; record it
                // so the last-slot check sees a chain born bottom.
                self.slots[i].tag = tag;
                self.slots[i].cycle = None;
            }
        }
        event.init = saved_init;

        // An interior slot's poison bottoms the fold only if a downstream
        // callback consumes it; only the last slot's state is the result.
        if forced_taint || self.slots.last().is_some_and(|s| s.tag.is_tainted()) {
            // A resize is an event even when the chain is poisoned.
            return if any_trig || resized {
                self.resident.set(TagValue::tagged(Value::Null, Tag::FRESH_BOTTOM))
            } else {
                self.resident.ride()
            };
        }
        if let Some(last) = self.slots.last() {
            if let Some(value) = last.cycle.clone() {
                // A fold fires iff it resized, a slot fired, or the
                // source fired empty.
                let tag = if resized || any_trig { Tag::FIRED } else { Tag::STALE };
                return self.resident.set(TagValue::tagged(value, tag));
            }
            if resized && let Some(value) = last.held.clone() {
                let tag = source_tag.unwrap_or(Tag::FIRED);
                return self.resident.set(TagValue::tagged(value, tag));
            }
        }
        self.resident.ride()
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        let Self { base, slots, .. } = self;
        base.source.delete(ctx);
        base.init.delete(ctx);
        base.prototype.delete(ctx);
        for id in base.prototype_ids {
            ctx.rt.store_remove(&id);
            ctx.env.unbind_variable(id);
        }
        for slot in slots.iter_mut() {
            slot.delete(ctx);
        }
    }

    fn typecheck0(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        wrap!(self.base.source, self.base.source.typecheck0(ctx))?;
        wrap!(self.base.init, self.base.init.typecheck0(ctx))?;
        wrap!(self.base.prototype, self.base.prototype.typecheck0(ctx))
    }

    fn typecheck1(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        wrap!(self.base.source, self.base.source.typecheck1(ctx))?;
        wrap!(self.base.init, self.base.init.typecheck1(ctx))?;
        wrap!(self.base.prototype, self.base.prototype.typecheck1(ctx))
    }

    fn typ(&self) -> &Type {
        &self.base.typ
    }

    fn refs(&self, refs: &mut Refs) {
        self.base.source.refs(refs);
        self.base.init.refs(refs);
        refs.bound.extend(self.base.prototype_ids);
        self.base.prototype.refs(refs);
    }

    fn spec(&self) -> &Expr {
        &self.base.spec
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        // The seed, `held` and slot tags survive sleep: sleep is pause.
        self.base.source.sleep(ctx);
        self.base.init.sleep(ctx);
        for slot in self.slots.iter_mut() {
            slot.call.sleep(ctx);
        }
    }

    fn reset_replay(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.base.source.reset_replay(ctx);
        self.base.init.reset_replay(ctx);
        self.init = None;
        self.source_present = false;
        for slot in self.slots.iter_mut() {
            slot.cycle = None;
            slot.held = None;
            slot.tag = Tag::STALE;
            slot.call.reset_replay(ctx);
        }
    }

    fn view(&self) -> NodeView<'_, R, E> {
        NodeView::FoldQ(&self.base)
    }

    fn emit_clif(&self, cx: &mut BodyCx) -> Result<CompiledExpr> {
        emit_fold::<R, E, T>(&self.base, &self.base.source, &self.base.init, cx)?
            .ok_or_else(|| anyhow::anyhow!("collection fold does not emit CLIF"))
    }
}

fn emit_fold<R: Rt, E: UserEvent, T: FoldFn<R, E>>(
    base: &FoldQBase<R, E>,
    source: &Node<R, E>,
    init: &Node<R, E>,
    cx: &mut BodyCx,
) -> Result<Option<CompiledExpr>> {
    if emit::node_is_bottom(source) || emit::node_is_bottom(init) {
        return Ok(None);
    }
    let Some(callback) = callback(&base.prototype) else { return Ok(None) };
    let Some(acc) = callback_param(callback, 0, literal!("__acc")) else {
        return Ok(None);
    };
    let Some(element) = callback_param(callback, 1, literal!("__elem")) else {
        return Ok(None);
    };
    T::emit_clif(
        cx,
        source,
        init,
        callback.body(),
        &acc,
        &element,
        &callback.typ().rtype,
        &base.element_type,
    )
}

fn emit_fold_call<R: Rt, E: UserEvent, T: FoldFn<R, E>>(
    base: &FoldQBase<R, E>,
    callsite: &CallSite<R, E>,
    cx: &mut BodyCx,
) -> Result<Option<CompiledExpr>> {
    let (Some(source), Some(init)) =
        (callsite.arg_positional(0), callsite.arg_positional(1))
    else {
        return Ok(None);
    };
    let prev = cx.swap_collection_site(Some(callsite.spec.id));
    let r = emit_fold::<R, E, T>(base, source, init, cx);
    cx.swap_collection_site(prev);
    r
}

/// The callback's definition value when the prototype call resolved
/// statically; a slot then calls that lambda directly, which a trait
/// method dispatcher requires (the parameter carries no value).
fn prototype_def<R: Rt, E: UserEvent>(
    ctx: &ExecCtx<R, E>,
    prototype: &Node<R, E>,
) -> Option<Value> {
    let NodeView::CallSite(site) = prototype.view() else { return None };
    let target = site.static_target.as_ref()?;
    ctx.lambda_defs.get(&target.definition).cloned()
}

/// A slot's function node: the resolved definition as a constant when
/// the prototype settled one, else a reference to the callback
/// parameter (bound at runtime by the value it carries).
fn callback_fnode<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<R, E>,
    callback: BindId,
    callback_type: &Arc<FnType>,
    top_id: ExprId,
    resolved: Option<Value>,
) -> Node<R, E> {
    match resolved {
        Some(v) => {
            super::Constant::new(v, Type::Fn(callback_type.clone()), Expr::clone(&NOP))
        }
        None => genn::reference(ctx, callback, Type::Fn(callback_type.clone()), top_id),
    }
}

fn callback<R: Rt, E: UserEvent>(
    prototype: &Node<R, E>,
) -> Option<&super::lambda::GXLambda<R, E>> {
    let NodeView::CallSite(site) = prototype.view() else { return None };
    let ApplyView::Lambda(callback) = site.resolved_apply()? else { return None };
    Some(callback)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Flavor {
    Array,
    List,
    CMap,
}

impl Flavor {
    /// Emit the loop source as the scaffold's ValArray. Returns the
    /// source's (disc, payload), whose disc drives the firing wrap,
    /// plus the loop's [`scaffold::ArraySrc`].
    fn emit_source<R: Rt, E: UserEvent>(
        self,
        cx: &mut BodyCx,
        source: &Node<R, E>,
    ) -> Result<(CompiledExpr, scaffold::ArraySrc)> {
        match self {
            Self::Array => {
                let owned = emit::node_composite_source(source) == CompositeSource::Owned;
                let array = source.emit_clif(cx)?;
                let src =
                    scaffold::ArraySrc { ptr: array.payload, disc: array.disc, owned };
                Ok((array, src))
            }
            Self::List => emit_flattened_source(cx, source, "graphix_list_to_valarray"),
            Self::CMap => emit_flattened_source(cx, source, "graphix_cmap_to_pairs"),
        }
    }

    /// The exit boundary for collection-returning loops: the loop's
    /// finalized ValArray as this flavor's collection Value.
    fn emit_result(self, cx: &mut BodyCx, ptr: ClifValue) -> Result<CompiledExpr> {
        match self {
            Self::Array => Ok(emit::array_result(cx, ptr)),
            Self::List => {
                convert_collection_result(cx, ptr, "graphix_valarray_into_list")
            }
            Self::CMap => {
                convert_collection_result(cx, ptr, "graphix_valarray_into_cmap")
            }
        }
    }
}

/// The filter/find gate: the callback must compile to a bool scalar.
fn predicate_is_bool<R: Rt, E: UserEvent>(body: &Node<R, E>) -> bool {
    kernel_abi::freeze_for_abi_normalized(body.typ())
        .as_ref()
        .and_then(|typ| kernel_abi::scalar_prim(typ))
        == Some(PrimType::Bool)
}

fn emit_init_kind<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    source: &Node<R, E>,
    body: &Node<R, E>,
    param: &CallbackParam,
    flavor: Flavor,
) -> Result<Option<CompiledExpr>> {
    if !param.binds.is_empty() {
        return Ok(None);
    }
    let count_prim = match kernel_abi::freeze_for_abi_normalized(source.typ())
        .as_ref()
        .and_then(|typ| kernel_abi::scalar_prim(typ))
    {
        Some(prim) if prim.is_integer() => prim,
        _ => return Ok(None),
    };
    let Some(output_type) = kernel_abi::freeze_for_abi_normalized(body.typ()) else {
        return Ok(None);
    };
    if is_unit_or_null(&output_type) {
        return Ok(None);
    }
    let count = source.emit_clif(cx)?;
    let source_invariant = emit::node_loop_invariant_ref(cx, source);
    let output_source = emit::node_composite_source(body);
    let (ptr, flags, count_disc) = scaffold::emit_init_loop(
        cx,
        count.payload,
        count.disc,
        count_prim,
        &param.name,
        param.id,
        &output_type,
        output_source,
        &emit::slot_state_sites(body),
        |cx| body.emit_clif(cx),
    )?;
    let result = flavor.emit_result(cx, ptr)?;
    // The firing wrap must see an over-limit count as a tainted source.
    let count = CompiledExpr::new(count_disc, count.payload);
    Ok(Some(finish_loop_result(cx, result, flags, &count, source_invariant)))
}

fn emit_map_kind<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    source: &Node<R, E>,
    body: &Node<R, E>,
    param: &CallbackParam,
    element_type: &Type,
    flavor: Flavor,
) -> Result<Option<CompiledExpr>> {
    let Some((element_type, leaves)) = bindable_array_element(element_type, &param.binds)
    else {
        return Ok(None);
    };
    let Some(output_type) = kernel_abi::freeze_for_abi_normalized(body.typ()) else {
        return Ok(None);
    };
    if is_unit_or_null(&output_type) {
        return Ok(None);
    }
    let (value, src) = flavor.emit_source(cx, source)?;
    let source_invariant = emit::node_loop_invariant_ref(cx, source);
    let output_source = emit::node_composite_source(body);
    let (ptr, flags) = scaffold::emit_map_loop(
        cx,
        src,
        &scaffold::HofElem {
            name: &param.name,
            id: param.id,
            typ: &element_type,
            leaves: &leaves,
        },
        &output_type,
        output_source,
        &emit::slot_state_sites(body),
        |cx| body.emit_clif(cx),
    )?;
    let result = flavor.emit_result(cx, ptr)?;
    Ok(Some(finish_loop_result(cx, result, flags, &value, source_invariant)))
}

fn emit_filter_kind<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    source: &Node<R, E>,
    body: &Node<R, E>,
    param: &CallbackParam,
    element_type: &Type,
    flavor: Flavor,
) -> Result<Option<CompiledExpr>> {
    let Some((element_type, leaves)) = bindable_array_element(element_type, &param.binds)
    else {
        return Ok(None);
    };
    if !predicate_is_bool(body) {
        return Ok(None);
    }
    let (value, src) = flavor.emit_source(cx, source)?;
    let source_invariant = emit::node_loop_invariant_ref(cx, source);
    let (ptr, flags) = scaffold::emit_filter_loop(
        cx,
        src,
        &scaffold::HofElem {
            name: &param.name,
            id: param.id,
            typ: &element_type,
            leaves: &leaves,
        },
        &emit::slot_state_sites(body),
        |cx| body.emit_clif(cx),
    )?;
    let result = flavor.emit_result(cx, ptr)?;
    Ok(Some(finish_loop_result(cx, result, flags, &value, source_invariant)))
}

fn emit_filter_map_kind<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    source: &Node<R, E>,
    body: &Node<R, E>,
    param: &CallbackParam,
    element_type: &Type,
    flavor: Flavor,
) -> Result<Option<CompiledExpr>> {
    let Some(output_type) = kernel_abi::freeze_for_abi_normalized(body.typ()) else {
        return Ok(None);
    };
    let Some(output_element) = kernel_abi::nullable_inner(&output_type) else {
        // A callback that can never return null makes filter_map a map.
        if frozen_may_be_null(&output_type) {
            return Ok(None);
        }
        return emit_map_kind(cx, source, body, param, element_type, flavor);
    };
    let Some((element_type, leaves)) = bindable_array_element(element_type, &param.binds)
    else {
        return Ok(None);
    };
    if is_unit_or_null(&output_element) {
        return Ok(None);
    }
    let (value, src) = flavor.emit_source(cx, source)?;
    let source_invariant = emit::node_loop_invariant_ref(cx, source);
    let output_source = emit::node_composite_source(body);
    let (ptr, flags) = scaffold::emit_filter_map_loop(
        cx,
        src,
        &scaffold::HofElem {
            name: &param.name,
            id: param.id,
            typ: &element_type,
            leaves: &leaves,
        },
        &output_element,
        output_source,
        &emit::slot_state_sites(body),
        |cx| body.emit_clif(cx),
    )?;
    let result = flavor.emit_result(cx, ptr)?;
    Ok(Some(finish_loop_result(cx, result, flags, &value, source_invariant)))
}

fn emit_flat_map_kind<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    source: &Node<R, E>,
    body: &Node<R, E>,
    param: &CallbackParam,
    element_type: &Type,
    flavor: Flavor,
) -> Result<Option<CompiledExpr>> {
    use kernel_abi::AbiKind;

    let Some((element_type, leaves)) = bindable_array_element(element_type, &param.binds)
    else {
        return Ok(None);
    };
    // A List callback's return freezes to a 2-word opaque Variant; the
    // extend helper walks it. No CMap flat_map intrinsic exists.
    let output_kind = kernel_abi::freeze_for_abi_normalized(body.typ())
        .as_ref()
        .and_then(|typ| kernel_abi::abi_kind(typ));
    let extend = match (flavor, output_kind) {
        (Flavor::Array, Some(AbiKind::Array)) => scaffold::FlatMapExtend::Array,
        (Flavor::List, Some(AbiKind::Variant)) => scaffold::FlatMapExtend::List,
        _ => return Ok(None),
    };
    let (value, src) = flavor.emit_source(cx, source)?;
    let source_invariant = emit::node_loop_invariant_ref(cx, source);
    let body_source = emit::node_composite_source(body);
    let (ptr, flags) = scaffold::emit_flat_map_loop(
        cx,
        src,
        &scaffold::HofElem {
            name: &param.name,
            id: param.id,
            typ: &element_type,
            leaves: &leaves,
        },
        extend,
        &emit::slot_state_sites(body),
        |cx| {
            let value = body.emit_clif(cx)?;
            match flavor {
                Flavor::Array => {
                    let payload =
                        emit::ensure_owned_composite_src(cx, body_source, value.payload)?;
                    Ok(CompiledExpr::new(value.disc, payload))
                }
                Flavor::List | Flavor::CMap => {
                    let (disc, payload) = emit::ensure_owned_value_src(
                        cx,
                        body_source,
                        value.disc,
                        value.payload,
                    )?;
                    Ok(CompiledExpr::new(disc, payload))
                }
            }
        },
    )?;
    let result = flavor.emit_result(cx, ptr)?;
    Ok(Some(finish_loop_result(cx, result, flags, &value, source_invariant)))
}

fn emit_find_kind<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    source: &Node<R, E>,
    body: &Node<R, E>,
    param: &CallbackParam,
    element_type: &Type,
    flavor: Flavor,
) -> Result<Option<CompiledExpr>> {
    let Some((element_type, leaves)) = bindable_array_element(element_type, &param.binds)
    else {
        return Ok(None);
    };
    if !predicate_is_bool(body) {
        return Ok(None);
    }
    let (value, src) = flavor.emit_source(cx, source)?;
    let source_invariant = emit::node_loop_invariant_ref(cx, source);
    let ((disc, payload), flags) = scaffold::emit_find_loop(
        cx,
        src,
        &scaffold::HofElem {
            name: &param.name,
            id: param.id,
            typ: &element_type,
            leaves: &leaves,
        },
        &emit::slot_state_sites(body),
        |cx| body.emit_clif(cx),
    )?;
    let result = CompiledExpr::new(disc, payload);
    Ok(Some(finish_loop_result(cx, result, flags, &value, source_invariant)))
}

fn emit_find_map_kind<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    source: &Node<R, E>,
    body: &Node<R, E>,
    param: &CallbackParam,
    element_type: &Type,
    flavor: Flavor,
) -> Result<Option<CompiledExpr>> {
    use kernel_abi::AbiKind;

    let Some((element_type, leaves)) = bindable_array_element(element_type, &param.binds)
    else {
        return Ok(None);
    };
    let output_is_nullable = matches!(
        kernel_abi::freeze_for_abi_normalized(body.typ())
            .as_ref()
            .and_then(|typ| kernel_abi::abi_kind(typ)),
        Some(AbiKind::Nullable)
    );
    if !output_is_nullable {
        return Ok(None);
    }
    let (value, src) = flavor.emit_source(cx, source)?;
    let source_invariant = emit::node_loop_invariant_ref(cx, source);
    let body_source = emit::node_composite_source(body);
    let ((disc, payload), flags) = scaffold::emit_find_map_loop(
        cx,
        src,
        &scaffold::HofElem {
            name: &param.name,
            id: param.id,
            typ: &element_type,
            leaves: &leaves,
        },
        &emit::slot_state_sites(body),
        |cx| {
            let value = body.emit_clif(cx)?;
            emit::ensure_owned_value_src(cx, body_source, value.disc, value.payload)
        },
    )?;
    let result = CompiledExpr::new(disc, payload);
    Ok(Some(finish_loop_result(cx, result, flags, &value, source_invariant)))
}

/// The fold kind. A List- or Map-valued accumulator has no `FoldAcc`
/// carry and stays interpreted.
fn emit_fold_kind<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    source: &Node<R, E>,
    init: &Node<R, E>,
    body: &Node<R, E>,
    acc: &CallbackParam,
    element: &CallbackParam,
    acc_type: &Type,
    element_type: &Type,
    flavor: Flavor,
) -> Result<Option<CompiledExpr>> {
    use kernel_abi::AbiKind;

    let Some((element_type, element_leaves)) =
        bindable_array_element(element_type, &element.binds)
    else {
        return Ok(None);
    };
    let Some(acc_type) = kernel_abi::freeze_for_abi_normalized(acc_type) else {
        return Ok(None);
    };
    let acc_leaves = match kernel_abi::abi_kind(&acc_type) {
        Some(AbiKind::Array | AbiKind::Tuple | AbiKind::Struct) => {
            let Some(leaves) = scaffold::elem_leaves(&acc_type, &acc.binds) else {
                return Ok(None);
            };
            Some(leaves)
        }
        _ => None,
    };
    // A Bottom-typed body unifies with any acc type but emits a
    // shapeless placeholder that violates the owned-acc discipline.
    if emit::node_is_bottom(body) {
        return Ok(None);
    }
    let acc_shape = match kernel_abi::abi_kind(&acc_type) {
        Some(AbiKind::Scalar(prim)) if acc.binds.is_empty() => {
            scaffold::FoldAcc::Scalar(prim)
        }
        Some(AbiKind::Array | AbiKind::Tuple | AbiKind::Struct) => {
            scaffold::FoldAcc::Composite {
                init_src: emit::node_composite_source(init),
                body_src: emit::node_composite_source(body),
                leaves: acc_leaves.as_deref().unwrap(),
            }
        }
        Some(AbiKind::String) if acc.binds.is_empty() => scaffold::FoldAcc::Str,
        // The init and body may emit narrower members of the acc union;
        // `emit_owned_value_operand_node` normalizes them to an owned Value.
        Some(k @ (AbiKind::Variant | AbiKind::Nullable | AbiKind::Value))
            if acc.binds.is_empty() =>
        {
            for n in [init, body] {
                match kernel_abi::abi_kind(n.typ()) {
                    Some(AbiKind::Unit) | None => return Ok(None),
                    Some(_) => {}
                }
            }
            scaffold::FoldAcc::Value {
                init_src: CompositeSource::Owned,
                body_src: CompositeSource::Owned,
                kind: match k {
                    AbiKind::Variant => scaffold::ValueLeafKind::Variant,
                    AbiKind::Nullable => scaffold::ValueLeafKind::Nullable,
                    _ => scaffold::ValueLeafKind::Value,
                },
            }
        }
        _ => return Ok(None),
    };
    let value_acc = matches!(acc_shape, scaffold::FoldAcc::Value { .. });
    let (value, src) = flavor.emit_source(cx, source)?;
    let source_invariant = emit::node_loop_invariant_ref(cx, source);
    let (result, flags) = scaffold::emit_fold_loop(
        cx,
        src,
        acc_shape,
        &acc.name,
        acc.id,
        &scaffold::HofElem {
            name: &element.name,
            id: element.id,
            typ: &element_type,
            leaves: &element_leaves,
        },
        &emit::slot_state_sites(body),
        |cx| {
            if value_acc {
                emit::emit_owned_value_operand_node(cx, init)
            } else {
                init.emit_clif(cx)
            }
        },
        |cx| {
            if value_acc {
                emit::emit_owned_value_operand_node(cx, body)
            } else {
                body.emit_clif(cx)
            }
        },
    )?;
    Ok(Some(finish_loop_result(cx, result, flags, &value, source_invariant)))
}

#[derive(Debug, Default)]
struct ArrayInit;

impl<R: Rt, E: UserEvent> MapFn<R, E> for ArrayInit {
    type Collection = IndexRange;

    fn finish(&mut self, slots: &[Slot<R, E>], _: &IndexRange) -> Option<Value> {
        Some(Value::Array(ValArray::from_iter_exact(
            slots.iter().map(|slot| slot.value.clone().unwrap()),
        )))
    }

    fn emit_clif(
        cx: &mut BodyCx,
        source: &Node<R, E>,
        body: &Node<R, E>,
        param: &CallbackParam,
        _: &Type,
    ) -> Result<Option<CompiledExpr>> {
        emit_init_kind(cx, source, body, param, Flavor::Array)
    }
}

#[derive(Debug, Default)]
struct ArrayMap;

impl<R: Rt, E: UserEvent> MapFn<R, E> for ArrayMap {
    type Collection = ValArray;

    fn finish(&mut self, slots: &[Slot<R, E>], _: &ValArray) -> Option<Value> {
        Some(Value::Array(ValArray::from_iter_exact(
            slots.iter().map(|slot| slot.value.clone().unwrap()),
        )))
    }

    fn emit_clif(
        cx: &mut BodyCx,
        source: &Node<R, E>,
        body: &Node<R, E>,
        param: &CallbackParam,
        element_type: &Type,
    ) -> Result<Option<CompiledExpr>> {
        emit_map_kind(cx, source, body, param, element_type, Flavor::Array)
    }
}

#[derive(Debug, Default)]
struct ArrayFilter;

impl<R: Rt, E: UserEvent> MapFn<R, E> for ArrayFilter {
    type Collection = ValArray;

    const PASS_THROUGH: bool = true;

    fn finish(&mut self, slots: &[Slot<R, E>], source: &ValArray) -> Option<Value> {
        Some(Value::Array(ValArray::from_iter(
            slots.iter().zip(source.iter()).filter_map(|(slot, value)| {
                matches!(slot.value, Some(Value::Bool(true))).then(|| value.clone())
            }),
        )))
    }

    fn emit_clif(
        cx: &mut BodyCx,
        source: &Node<R, E>,
        body: &Node<R, E>,
        param: &CallbackParam,
        element_type: &Type,
    ) -> Result<Option<CompiledExpr>> {
        emit_filter_kind(cx, source, body, param, element_type, Flavor::Array)
    }
}

#[derive(Debug, Default)]
struct ArrayFilterMap;

impl<R: Rt, E: UserEvent> MapFn<R, E> for ArrayFilterMap {
    type Collection = ValArray;

    fn finish(&mut self, slots: &[Slot<R, E>], _: &ValArray) -> Option<Value> {
        Some(Value::Array(ValArray::from_iter(slots.iter().filter_map(|slot| {
            match slot.value.as_ref().unwrap() {
                Value::Null => None,
                value => Some(value.clone()),
            }
        }))))
    }

    fn emit_clif(
        cx: &mut BodyCx,
        source: &Node<R, E>,
        body: &Node<R, E>,
        param: &CallbackParam,
        element_type: &Type,
    ) -> Result<Option<CompiledExpr>> {
        emit_filter_map_kind(cx, source, body, param, element_type, Flavor::Array)
    }
}

#[derive(Debug, Default)]
struct ArrayFlatMap;

impl<R: Rt, E: UserEvent> MapFn<R, E> for ArrayFlatMap {
    type Collection = ValArray;

    fn finish(&mut self, slots: &[Slot<R, E>], _: &ValArray) -> Option<Value> {
        let mut values: LPooled<Vec<Value>> = LPooled::take();
        for slot in slots {
            match slot.value.as_ref().unwrap() {
                Value::Array(array) => values.extend(array.iter().cloned()),
                value => values.push(value.clone()),
            }
        }
        Some(Value::Array(ValArray::from_iter_exact(values.drain(..))))
    }

    fn emit_clif(
        cx: &mut BodyCx,
        source: &Node<R, E>,
        body: &Node<R, E>,
        param: &CallbackParam,
        element_type: &Type,
    ) -> Result<Option<CompiledExpr>> {
        emit_flat_map_kind(cx, source, body, param, element_type, Flavor::Array)
    }
}

#[derive(Debug, Default)]
struct ArrayFind;

impl<R: Rt, E: UserEvent> MapFn<R, E> for ArrayFind {
    type Collection = ValArray;

    const PASS_THROUGH: bool = true;

    fn finish(&mut self, slots: &[Slot<R, E>], source: &ValArray) -> Option<Value> {
        Some(
            slots
                .iter()
                .position(|slot| matches!(slot.value, Some(Value::Bool(true))))
                .map(|i| source[i].clone())
                .unwrap_or(Value::Null),
        )
    }

    fn emit_clif(
        cx: &mut BodyCx,
        source: &Node<R, E>,
        body: &Node<R, E>,
        param: &CallbackParam,
        element_type: &Type,
    ) -> Result<Option<CompiledExpr>> {
        emit_find_kind(cx, source, body, param, element_type, Flavor::Array)
    }
}

#[derive(Debug, Default)]
struct ArrayFindMap;

impl<R: Rt, E: UserEvent> MapFn<R, E> for ArrayFindMap {
    type Collection = ValArray;

    fn finish(&mut self, slots: &[Slot<R, E>], _: &ValArray) -> Option<Value> {
        Some(
            slots
                .iter()
                .find_map(|slot| match slot.value.as_ref().unwrap() {
                    Value::Null => None,
                    value => Some(value.clone()),
                })
                .unwrap_or(Value::Null),
        )
    }

    fn emit_clif(
        cx: &mut BodyCx,
        source: &Node<R, E>,
        body: &Node<R, E>,
        param: &CallbackParam,
        element_type: &Type,
    ) -> Result<Option<CompiledExpr>> {
        emit_find_map_kind(cx, source, body, param, element_type, Flavor::Array)
    }
}

#[derive(Debug)]
struct ArrayFold;

impl<R: Rt, E: UserEvent> FoldFn<R, E> for ArrayFold {
    type Collection = ValArray;

    fn emit_clif(
        cx: &mut BodyCx,
        source: &Node<R, E>,
        init: &Node<R, E>,
        body: &Node<R, E>,
        acc: &CallbackParam,
        element: &CallbackParam,
        acc_type: &Type,
        element_type: &Type,
    ) -> Result<Option<CompiledExpr>> {
        emit_fold_kind(
            cx,
            source,
            init,
            body,
            acc,
            element,
            acc_type,
            element_type,
            Flavor::Array,
        )
    }
}

#[derive(Debug, Default)]
struct ListInit;

impl<R: Rt, E: UserEvent> MapFn<R, E> for ListInit {
    type Collection = IndexRange;

    fn finish(&mut self, slots: &[Slot<R, E>], _: &IndexRange) -> Option<Value> {
        Some(list::from_iter(slots.iter().map(|slot| slot.value.clone().unwrap())))
    }

    fn emit_clif(
        cx: &mut BodyCx,
        source: &Node<R, E>,
        body: &Node<R, E>,
        param: &CallbackParam,
        _: &Type,
    ) -> Result<Option<CompiledExpr>> {
        emit_init_kind(cx, source, body, param, Flavor::List)
    }
}

#[derive(Debug, Default)]
struct ListMap;

impl<R: Rt, E: UserEvent> MapFn<R, E> for ListMap {
    type Collection = ListCollection;

    fn finish(&mut self, slots: &[Slot<R, E>], _: &ListCollection) -> Option<Value> {
        Some(list::from_iter(slots.iter().map(|slot| slot.value.clone().unwrap())))
    }

    fn emit_clif(
        cx: &mut BodyCx,
        source: &Node<R, E>,
        body: &Node<R, E>,
        param: &CallbackParam,
        element_type: &Type,
    ) -> Result<Option<CompiledExpr>> {
        emit_map_kind(cx, source, body, param, element_type, Flavor::List)
    }
}

#[derive(Debug, Default)]
struct ListFilter;

impl<R: Rt, E: UserEvent> MapFn<R, E> for ListFilter {
    type Collection = ListCollection;

    const PASS_THROUGH: bool = true;

    fn finish(&mut self, slots: &[Slot<R, E>], source: &ListCollection) -> Option<Value> {
        Some(list::from_iter(slots.iter().zip(source.values()).filter_map(
            |(slot, value)| {
                matches!(slot.value, Some(Value::Bool(true))).then_some(value)
            },
        )))
    }

    fn emit_clif(
        cx: &mut BodyCx,
        source: &Node<R, E>,
        body: &Node<R, E>,
        param: &CallbackParam,
        element_type: &Type,
    ) -> Result<Option<CompiledExpr>> {
        emit_filter_kind(cx, source, body, param, element_type, Flavor::List)
    }
}

#[derive(Debug, Default)]
struct ListFilterMap;

impl<R: Rt, E: UserEvent> MapFn<R, E> for ListFilterMap {
    type Collection = ListCollection;

    fn finish(&mut self, slots: &[Slot<R, E>], _: &ListCollection) -> Option<Value> {
        Some(list::from_iter(slots.iter().filter_map(|slot| {
            match slot.value.as_ref().unwrap() {
                Value::Null => None,
                value => Some(value.clone()),
            }
        })))
    }

    fn emit_clif(
        cx: &mut BodyCx,
        source: &Node<R, E>,
        body: &Node<R, E>,
        param: &CallbackParam,
        element_type: &Type,
    ) -> Result<Option<CompiledExpr>> {
        emit_filter_map_kind(cx, source, body, param, element_type, Flavor::List)
    }
}

#[derive(Debug, Default)]
struct ListFlatMap;

impl<R: Rt, E: UserEvent> MapFn<R, E> for ListFlatMap {
    type Collection = ListCollection;

    fn finish(&mut self, slots: &[Slot<R, E>], _: &ListCollection) -> Option<Value> {
        let mut values: LPooled<Vec<Value>> = LPooled::take();
        for slot in slots {
            let value = slot.value.as_ref().unwrap();
            if list::is_list(value) {
                values.extend(list::Iter::new(value.clone()));
            } else {
                values.push(value.clone());
            }
        }
        Some(list::from_iter(values.drain(..)))
    }

    fn emit_clif(
        cx: &mut BodyCx,
        source: &Node<R, E>,
        body: &Node<R, E>,
        param: &CallbackParam,
        element_type: &Type,
    ) -> Result<Option<CompiledExpr>> {
        emit_flat_map_kind(cx, source, body, param, element_type, Flavor::List)
    }
}

#[derive(Debug, Default)]
struct ListFind;

impl<R: Rt, E: UserEvent> MapFn<R, E> for ListFind {
    type Collection = ListCollection;

    const PASS_THROUGH: bool = true;

    fn finish(&mut self, slots: &[Slot<R, E>], source: &ListCollection) -> Option<Value> {
        Some(
            slots
                .iter()
                .zip(source.values())
                .find_map(|(slot, value)| {
                    matches!(slot.value, Some(Value::Bool(true))).then_some(value)
                })
                .unwrap_or(Value::Null),
        )
    }

    fn emit_clif(
        cx: &mut BodyCx,
        source: &Node<R, E>,
        body: &Node<R, E>,
        param: &CallbackParam,
        element_type: &Type,
    ) -> Result<Option<CompiledExpr>> {
        emit_find_kind(cx, source, body, param, element_type, Flavor::List)
    }
}

#[derive(Debug, Default)]
struct ListFindMap;

impl<R: Rt, E: UserEvent> MapFn<R, E> for ListFindMap {
    type Collection = ListCollection;

    fn finish(&mut self, slots: &[Slot<R, E>], _: &ListCollection) -> Option<Value> {
        Some(
            slots
                .iter()
                .find_map(|slot| match slot.value.as_ref().unwrap() {
                    Value::Null => None,
                    value => Some(value.clone()),
                })
                .unwrap_or(Value::Null),
        )
    }

    fn emit_clif(
        cx: &mut BodyCx,
        source: &Node<R, E>,
        body: &Node<R, E>,
        param: &CallbackParam,
        element_type: &Type,
    ) -> Result<Option<CompiledExpr>> {
        emit_find_map_kind(cx, source, body, param, element_type, Flavor::List)
    }
}

#[derive(Debug)]
struct ListFold;

impl<R: Rt, E: UserEvent> FoldFn<R, E> for ListFold {
    type Collection = ListCollection;

    fn emit_clif(
        cx: &mut BodyCx,
        source: &Node<R, E>,
        init: &Node<R, E>,
        body: &Node<R, E>,
        acc: &CallbackParam,
        element: &CallbackParam,
        acc_type: &Type,
        element_type: &Type,
    ) -> Result<Option<CompiledExpr>> {
        emit_fold_kind(
            cx,
            source,
            init,
            body,
            acc,
            element,
            acc_type,
            element_type,
            Flavor::List,
        )
    }
}

#[derive(Debug, Default)]
struct MapMap;

impl<R: Rt, E: UserEvent> MapFn<R, E> for MapMap {
    type Collection = ValueMap;

    fn finish(&mut self, slots: &[Slot<R, E>], _: &ValueMap) -> Option<Value> {
        let mut values = slots
            .iter()
            .map(|slot| split_pair(slot.value.as_ref().unwrap()))
            .collect::<Option<LPooled<Vec<_>>>>()?;
        Some(Value::Map(CMap::from_iter(values.drain(..))))
    }

    fn emit_clif(
        cx: &mut BodyCx,
        source: &Node<R, E>,
        body: &Node<R, E>,
        param: &CallbackParam,
        element_type: &Type,
    ) -> Result<Option<CompiledExpr>> {
        emit_map_kind(cx, source, body, param, element_type, Flavor::CMap)
    }
}

#[derive(Debug, Default)]
struct MapFilter;

impl<R: Rt, E: UserEvent> MapFn<R, E> for MapFilter {
    type Collection = ValueMap;

    const PASS_THROUGH: bool = true;

    fn finish(&mut self, slots: &[Slot<R, E>], source: &ValueMap) -> Option<Value> {
        Some(Value::Map(CMap::from_iter(
            slots.iter().zip(source.into_iter()).filter_map(|(slot, (key, value))| {
                matches!(slot.value, Some(Value::Bool(true)))
                    .then(|| (key.clone(), value.clone()))
            }),
        )))
    }

    fn emit_clif(
        cx: &mut BodyCx,
        source: &Node<R, E>,
        body: &Node<R, E>,
        param: &CallbackParam,
        element_type: &Type,
    ) -> Result<Option<CompiledExpr>> {
        emit_filter_kind(cx, source, body, param, element_type, Flavor::CMap)
    }
}

#[derive(Debug, Default)]
struct MapFilterMap;

impl<R: Rt, E: UserEvent> MapFn<R, E> for MapFilterMap {
    type Collection = ValueMap;

    fn finish(&mut self, slots: &[Slot<R, E>], _: &ValueMap) -> Option<Value> {
        let mut values: LPooled<Vec<(Value, Value)>> = LPooled::take();
        for slot in slots {
            match slot.value.as_ref().unwrap() {
                Value::Null => {}
                value => values.push(split_pair(value)?),
            }
        }
        Some(Value::Map(CMap::from_iter(values.drain(..))))
    }

    fn emit_clif(
        cx: &mut BodyCx,
        source: &Node<R, E>,
        body: &Node<R, E>,
        param: &CallbackParam,
        element_type: &Type,
    ) -> Result<Option<CompiledExpr>> {
        emit_filter_map_kind(cx, source, body, param, element_type, Flavor::CMap)
    }
}

#[derive(Debug)]
struct MapFold;

impl<R: Rt, E: UserEvent> FoldFn<R, E> for MapFold {
    type Collection = ValueMap;

    fn emit_clif(
        cx: &mut BodyCx,
        source: &Node<R, E>,
        init: &Node<R, E>,
        body: &Node<R, E>,
        acc: &CallbackParam,
        element: &CallbackParam,
        acc_type: &Type,
        element_type: &Type,
    ) -> Result<Option<CompiledExpr>> {
        emit_fold_kind(
            cx,
            source,
            init,
            body,
            acc,
            element,
            acc_type,
            element_type,
            Flavor::CMap,
        )
    }
}
