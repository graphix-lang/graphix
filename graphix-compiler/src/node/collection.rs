use super::{
    NOP, WakeBit, callsite::CallSite, coretraits::with_hooks, genn, lambda::GXLambda,
    list, pattern::StructPatternNode,
};
use crate::{
    ApplyView, BindId, Event, ExecCtx, Node, NodeView, Refs, Rt, Scope, Tag, TagValue,
    Update, UserEvent,
    dbgenv::gxdbg_slot,
    expr::{Expr, ExprId},
    fusion::{
        check_attributes_subtree,
        emit::{self, BodyCx, CompiledExpr, CompositeSource, scaffold},
        kernel_abi::{self, AbiKind, PrimType},
    },
    image::{
        self, ImageBuf,
        nodes::{NodeTag, decode_node, put_tag, tag_len},
        scope_decode, scope_encode, scope_len,
    },
    typ::{FnArgKind, FnType, Type},
    wrap,
};
use anyhow::{Result, anyhow, bail};
use arcstr::{ArcStr, literal};
use cranelift_codegen::ir::{InstBuilder, Value as ClifValue};
use immutable_chunkmap::map::Map as CMap;
use netidx_core::pack::{Pack, PackError};
use netidx_value::{Typ, ValArray, Value};
use poolshark::local::LPooled;
use smallvec::{SmallVec, smallvec};
use std::{fmt::Debug, marker::PhantomData};
use triomphe::Arc;

/// A traversal that calls its callback once per element.
#[derive(Debug, Clone, Copy, PartialEq, Eq, netidx_derive::Pack)]
pub enum MapOp {
    Init,
    Map,
    Filter,
    FilterMap,
    FlatMap,
    Find,
    FindMap,
}

impl MapOp {
    /// The result reads the source's elements, so a moved source changes
    /// it even when no callback slot fires.
    fn reads_elements(self) -> bool {
        matches!(self, Self::Filter | Self::Find)
    }
}

/// The collection a HOF traverses and builds.
#[derive(Debug, Clone, Copy, PartialEq, Eq, netidx_derive::Pack)]
pub enum Flavor {
    Array,
    List,
    CMap,
}

/// A collection HOF: its traversal and its collection.
#[derive(Debug, Clone, Copy, PartialEq, Eq, netidx_derive::Pack)]
pub enum CollectionIntrinsic {
    Map(MapOp, Flavor),
    Fold(Flavor),
}

/// The intrinsic's node over its collection type.
macro_rules! by_collection {
    ($intrinsic:expr, $f:ident($($arg:expr),*)) => {
        match $intrinsic {
            CollectionIntrinsic::Map(MapOp::Init, _) => MapQ::<R, E, IndexRange>::$f($($arg),*),
            CollectionIntrinsic::Map(_, Flavor::Array) => MapQ::<R, E, ValArray>::$f($($arg),*),
            CollectionIntrinsic::Map(_, Flavor::List) => {
                MapQ::<R, E, ListCollection>::$f($($arg),*)
            }
            CollectionIntrinsic::Map(_, Flavor::CMap) => MapQ::<R, E, ValueMap>::$f($($arg),*),
            CollectionIntrinsic::Fold(Flavor::Array) => FoldQ::<R, E, ValArray>::$f($($arg),*),
            CollectionIntrinsic::Fold(Flavor::List) => {
                FoldQ::<R, E, ListCollection>::$f($($arg),*)
            }
            CollectionIntrinsic::Fold(Flavor::CMap) => FoldQ::<R, E, ValueMap>::$f($($arg),*),
        }
    };
}

impl CollectionIntrinsic {
    /// The intrinsic a reserved marker name (`'array_map`, …) names.
    pub(crate) fn from_name(name: &str) -> Option<Self> {
        let (flavor, op) = if let Some(op) = name.strip_prefix("array_") {
            (Flavor::Array, op)
        } else if let Some(op) = name.strip_prefix("list_") {
            (Flavor::List, op)
        } else if let Some(op) = name.strip_prefix("map_") {
            (Flavor::CMap, op)
        } else {
            return None;
        };
        let op = match op {
            "fold" => return Some(Self::Fold(flavor)),
            "init" => MapOp::Init,
            "map" => MapOp::Map,
            "filter" => MapOp::Filter,
            "filter_map" => MapOp::FilterMap,
            "flat_map" => MapOp::FlatMap,
            "find" => MapOp::Find,
            "find_map" => MapOp::FindMap,
            _ => return None,
        };
        match (op, flavor) {
            (
                MapOp::Init | MapOp::FlatMap | MapOp::Find | MapOp::FindMap,
                Flavor::CMap,
            ) => None,
            (op, flavor) => Some(Self::Map(op, flavor)),
        }
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
        by_collection!(self, new(self, ctx, spec, scope, top_id, typ, args))
    }

    pub(crate) fn image_decode<R: Rt, E: UserEvent>(
        ctx: &mut ExecCtx<R, E>,
        buf: &mut &[u8],
    ) -> Result<Node<R, E>, PackError> {
        let intrinsic = Self::decode(buf)?;
        by_collection!(intrinsic, image_decode(intrinsic, ctx, buf))
    }
}

trait MapCollection: Debug + Clone + Default + Send + Sync + 'static {
    fn len(&self) -> usize;
    fn values(&self) -> impl Iterator<Item = Value>;
    /// The collection a source value holds; `fired` = the value was
    /// just produced, so a refusal is news worth logging.
    fn select(value: Value, fired: bool) -> Option<Self>;
    /// The element type the callback takes, from the intrinsic's
    /// signature.
    fn element_type(ft: &FnType) -> Result<Type>;
}

/// The intrinsic's source argument type, dereferenced.
fn source_type(ft: &FnType) -> Option<Type> {
    ft.args[0].typ.deref_cloned()
}

impl MapCollection for ValArray {
    fn len(&self) -> usize {
        self.as_ref().len()
    }

    fn values(&self) -> impl Iterator<Item = Value> {
        self.iter().cloned()
    }

    fn select(value: Value, _: bool) -> Option<Self> {
        match value {
            Value::Array(a) => Some(a),
            _ => None,
        }
    }

    fn element_type(ft: &FnType) -> Result<Type> {
        match &source_type(ft) {
            Some(Type::Array(t)) => Ok((**t).clone()),
            _ => bail!("expected Array, got {}", ft.args[0].typ),
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

/// The map a Map HOF builds from its `[k, v]` pairs, both engines; a
/// malformed pair is logged and skipped. Key order reads the core-trait
/// hooks, so the caller runs this under them.
pub(crate) fn pairs_to_map<'a>(pairs: impl IntoIterator<Item = &'a Value>) -> Value {
    Value::Map(CMap::from_iter(pairs.into_iter().filter_map(|v| {
        let pair = split_pair(v);
        if pair.is_none() {
            log::error!("map result: malformed pair {v:?}");
        }
        pair
    })))
}

impl MapCollection for ValueMap {
    fn len(&self) -> usize {
        CMap::len(self)
    }

    fn values(&self) -> impl Iterator<Item = Value> {
        self.into_iter().map(|(k, v)| make_pair(k, v))
    }

    fn select(value: Value, _: bool) -> Option<Self> {
        match value {
            Value::Map(m) => Some(m),
            _ => None,
        }
    }

    fn element_type(ft: &FnType) -> Result<Type> {
        match &source_type(ft) {
            Some(Type::Map { key, value }) => {
                Ok(Type::Tuple(Arc::from_iter([(**key).clone(), (**value).clone()])))
            }
            _ => bail!("expected Map, got {}", ft.args[0].typ),
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

    fn select(value: Value, _: bool) -> Option<Self> {
        let len = list::len(&value)?;
        Some(Self { value, len })
    }

    fn element_type(ft: &FnType) -> Result<Type> {
        match &source_type(ft) {
            Some(Type::List(t)) => Ok((**t).clone()),
            _ => bail!("expected List, got {}", ft.args[0].typ),
        }
    }
}

/// The largest count `array::init`/`list::init` build; a larger one is
/// bottom on both engines, and the node-walk logs it once per fired count.
pub const MAX_ARRAY_INIT_LEN: i64 = 16 * 1024 * 1024;

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

    fn select(value: Value, fired: bool) -> Option<Self> {
        let Value::I64(n) = value else { return None };
        if n > MAX_ARRAY_INIT_LEN {
            if fired {
                log::error!(
                    "collection init size {n} exceeds the {MAX_ARRAY_INIT_LEN} element limit"
                );
            }
            return None;
        }
        Some(Self(n.max(0) as usize))
    }

    fn element_type(_: &FnType) -> Result<Type> {
        Ok(Type::Primitive(Typ::I64.into()))
    }
}

/// A slot's last production.
#[derive(Debug, Default)]
enum SlotState {
    /// Never produced.
    #[default]
    Empty,
    Value(Value),
    Bottom,
}

impl SlotState {
    /// Every production, fired or standing, replaces the state: a bottom
    /// is never answered from an earlier value.
    fn set(&mut self, tv: &TagValue) {
        *self = if tv.tag().is_bottom() {
            Self::Bottom
        } else {
            Self::Value(tv.value_cloned())
        }
    }

    fn value(&self) -> Option<&Value> {
        match self {
            Self::Value(v) => Some(v),
            Self::Empty | Self::Bottom => None,
        }
    }

    fn is_bottom(&self) -> bool {
        matches!(self, Self::Bottom)
    }
}

/// The callback a collection intrinsic calls, and where it calls it.
#[derive(Debug)]
struct Callback {
    scope: Scope,
    id: BindId,
    typ: Arc<FnType>,
    top_id: ExprId,
}

impl Callback {
    fn image_len(&self) -> usize {
        scope_len(&self.scope)
            + self.id.encoded_len()
            + self.typ.encoded_len()
            + self.top_id.encoded_len()
    }

    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        scope_encode(&self.scope, buf)?;
        self.id.encode(buf)?;
        self.typ.encode(buf)?;
        self.top_id.encode(buf)
    }

    fn image_decode(buf: &mut &[u8]) -> Result<Self, PackError> {
        Ok(Self {
            scope: scope_decode(buf)?,
            id: BindId::decode(buf)?,
            typ: Pack::decode(buf)?,
            top_id: ExprId::decode(buf)?,
        })
    }

    /// A fresh bind for one callback argument.
    fn arg<R: Rt, E: UserEvent>(
        &self,
        ctx: &mut ExecCtx<R, E>,
        name: &str,
        typ: &Type,
    ) -> (BindId, Node<R, E>) {
        genn::bind(ctx, &self.scope.lexical, name, typ.clone(), self.top_id)
    }

    /// A call of the callback over `args`: the prototype, whose dispatch
    /// the typecheck resolves, or a slot, which calls the definition the
    /// prototype resolved to when it resolved one.
    fn call<R: Rt, E: UserEvent>(
        &self,
        ctx: &mut ExecCtx<R, E>,
        args: SmallVec<[Node<R, E>; 2]>,
        kind: CallKind,
    ) -> Node<R, E> {
        let (Self { scope, id, typ, top_id }, fty) = (self, Type::Fn(self.typ.clone()));
        match kind {
            CallKind::Prototype => {
                let function = genn::reference(ctx, *id, fty, *top_id);
                genn::apply_prototype(function, scope.clone(), args, typ, *top_id)
            }
            CallKind::Slot(Some(def)) => {
                let function = super::Constant::new(def, fty, Expr::clone(&NOP));
                genn::apply(function, scope.clone(), args, typ, *top_id)
            }
            CallKind::Slot(None) => {
                let function = genn::reference(ctx, *id, fty, *top_id);
                genn::apply(function, scope.clone(), args, typ, *top_id)
            }
        }
    }
}

/// Which call of its callback a collection builds.
#[derive(Clone)]
enum CallKind {
    Prototype,
    /// A slot; the definition value when the prototype call resolved
    /// statically (a trait method dispatcher requires it: the parameter
    /// carries no value).
    Slot(Option<Value>),
}

impl CallKind {
    /// The slot call a prototype settled on.
    fn slot<R: Rt, E: UserEvent>(ctx: &ExecCtx<R, E>, prototype: &Node<R, E>) -> Self {
        let NodeView::CallSite(site) = prototype.view() else { return Self::Slot(None) };
        let def = site
            .static_target
            .as_ref()
            .and_then(|target| ctx.lambda_defs.get(&target.definition).cloned());
        Self::Slot(def)
    }
}

/// Resize `slots` to `n`, deleting the excess and adding with `add`:
/// true when the length changed.
fn resize<R: Rt, E: UserEvent, S>(
    ctx: &mut ExecCtx<R, E>,
    slots: &mut Vec<S>,
    n: usize,
    delete: fn(&mut S, &mut ExecCtx<R, E>),
    mut add: impl FnMut(&mut ExecCtx<R, E>) -> S,
) -> bool {
    let old = slots.len();
    for mut s in slots.drain(n.min(old)..) {
        delete(&mut s, ctx)
    }
    while slots.len() < n {
        let s = add(ctx);
        slots.push(s)
    }
    old != n
}

#[derive(Debug)]
struct Slot<R: Rt, E: UserEvent> {
    id: BindId,
    call: Node<R, E>,
    state: SlotState,
}

impl<R: Rt, E: UserEvent> Slot<R, E> {
    fn new(
        ctx: &mut ExecCtx<R, E>,
        callback: &Callback,
        element_type: &Type,
        kind: CallKind,
    ) -> Self {
        let (id, element) = callback.arg(ctx, "collection_element", element_type);
        let call = callback.call(ctx, smallvec![element], kind);
        Self { id, call, state: SlotState::Empty }
    }

    /// The slot's value; `finish` runs only once every slot holds one.
    fn value(&self) -> &Value {
        self.state.value().expect("finish runs once every slot holds a value")
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.call.delete(ctx);
        ctx.rt.store_remove(&self.id);
        ctx.env.unbind_variable(self.id);
    }
}

#[derive(Debug)]
struct CallbackParam {
    name: ArcStr,
    id: Option<BindId>,
    binds: Vec<(BindId, usize)>,
}

impl CallbackParam {
    /// The loop's element bind for this parameter.
    fn elem<'a>(
        &'a self,
        typ: &'a Type,
        leaves: &'a [(BindId, usize, scaffold::LeafShape)],
    ) -> scaffold::HofElem<'a> {
        scaffold::HofElem { name: &self.name, id: self.id, typ, leaves }
    }
}

/// The callback's `index`-th positional parameter; `None` for a
/// callback with labeled parameters, which the collection interprets.
fn callback_param<R: Rt, E: UserEvent>(
    callback: &GXLambda<R, E>,
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
    matches!(kernel_abi::abi_kind(typ), Some(AbiKind::Unit | AbiKind::Null))
}

/// Whether a frozen type admits `null`, filter_map's drop marker.
/// An unknown shape answers true, so the caller keeps interpreting.
fn frozen_may_be_null(t: &Type) -> bool {
    t.with_deref(|t| match t {
        Some(Type::Primitive(p)) => p.contains(Typ::Null),
        Some(Type::Set(ms)) => ms.iter().any(frozen_may_be_null),
        Some(
            Type::Array(_)
            | Type::List(_)
            | Type::Tuple(_)
            | Type::Struct(_)
            | Type::Variant(_, _, _)
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
    Ok((
        value,
        scaffold::ArraySrc { ptr, disc: value.disc, ownership: CompositeSource::Owned },
    ))
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
    op: MapOp,
    flavor: Flavor,
    element_type: Type,
    prototype_id: BindId,
    spec: Expr,
    typ: Type,
}

impl<R: Rt, E: UserEvent> MapQBase<R, E> {
    /// The fused loop for a call site's intrinsic call.
    pub(crate) fn emit_clif_call(
        &self,
        callsite: &CallSite<R, E>,
        cx: &mut BodyCx,
    ) -> Result<Option<CompiledExpr>> {
        let Some(source) = callsite.arg_positional(0) else {
            return Ok(None);
        };
        let prev = cx.swap_collection_site(Some(callsite.spec.id));
        let r = self.emit(source, cx);
        cx.swap_collection_site(prev);
        r
    }

    fn emit(&self, source: &Node<R, E>, cx: &mut BodyCx) -> Result<Option<CompiledExpr>> {
        if emit::node_is_bottom(source) {
            return Ok(None);
        }
        let Some(callback) = callback(&self.prototype) else { return Ok(None) };
        let Some(param) = callback_param(callback, 0, literal!("__elem")) else {
            return Ok(None);
        };
        let (body, et, flavor) = (callback.body(), &self.element_type, self.flavor);
        match self.op {
            MapOp::Init => emit_init_kind(cx, source, body, &param, flavor),
            MapOp::Map => emit_map_kind(cx, source, body, &param, et, flavor),
            MapOp::Filter => emit_filter_kind(cx, source, body, &param, et, flavor),
            MapOp::FilterMap => {
                emit_filter_map_kind(cx, source, body, &param, et, flavor)
            }
            MapOp::FlatMap => emit_flat_map_kind(cx, source, body, &param, et, flavor),
            MapOp::Find => emit_find_kind(cx, source, body, &param, et, flavor),
            MapOp::FindMap => emit_find_map_kind(cx, source, body, &param, et, flavor),
        }
    }

    pub(crate) fn callback_body(&self) -> Option<&Node<R, E>> {
        callback(&self.prototype).map(GXLambda::body)
    }

    /// The traversal's result once every slot holds a value.
    fn finish<C: MapCollection>(&self, slots: &[Slot<R, E>], source: &C) -> Value {
        let taken = |slot: &Slot<R, E>| matches!(slot.value(), Value::Bool(true));
        let kept = |v: &&Value| !matches!(v, Value::Null);
        let mut elems: LPooled<Vec<Value>> = match self.op {
            MapOp::Init | MapOp::Map => slots.iter().map(|s| s.value().clone()).collect(),
            MapOp::Filter => slots
                .iter()
                .zip(source.values())
                .filter_map(|(s, v)| taken(s).then_some(v))
                .collect(),
            MapOp::FilterMap => {
                slots.iter().map(Slot::value).filter(kept).cloned().collect()
            }
            MapOp::FlatMap => {
                let mut elems = LPooled::take();
                for s in slots {
                    self.flavor.extend(&mut elems, s.value())
                }
                elems
            }
            MapOp::Find => {
                let found = slots.iter().zip(source.values()).find(|(s, _)| taken(s));
                return found.map_or(Value::Null, |(_, v)| v);
            }
            MapOp::FindMap => {
                let found = slots.iter().map(Slot::value).find(kept);
                return found.cloned().unwrap_or(Value::Null);
            }
        };
        self.flavor.build(&mut elems)
    }
}

#[derive(Debug)]
struct MapQ<R: Rt, E: UserEvent, C: MapCollection> {
    slept: WakeBit,
    base: MapQBase<R, E>,
    callback: Callback,
    slots: LPooled<Vec<Slot<R, E>>>,
    current: C,
    /// The source was bottom (or never read): its length is forgotten,
    /// so its return changes the result.
    src_bottom: bool,
    resident: TagValue,
}

impl<R: Rt, E: UserEvent, C: MapCollection> MapQ<R, E, C> {
    fn with(base: MapQBase<R, E>, callback: Callback) -> Node<R, E> {
        Node::new(Self {
            slept: WakeBit::default(),
            base,
            callback,
            slots: LPooled::take(),
            current: C::default(),
            src_bottom: true,
            resident: TagValue::phantom(),
        })
    }

    fn new(
        intrinsic: CollectionIntrinsic,
        ctx: &mut ExecCtx<R, E>,
        spec: Expr,
        scope: &Scope,
        top_id: ExprId,
        typ: &Arc<FnType>,
        args: &[StructPatternNode],
    ) -> Result<Node<R, E>> {
        let CollectionIntrinsic::Map(op, flavor) = intrinsic else {
            bail!("{intrinsic:?} is not a map intrinsic")
        };
        if typ.args.len() != 2 || args.len() != 2 {
            bail!("collection map intrinsic requires two arguments")
        }
        let source_id = args[0]
            .single_bind_id()
            .ok_or_else(|| anyhow!("collection source argument must be a name"))?;
        let id = args[1]
            .single_bind_id()
            .ok_or_else(|| anyhow!("collection callback argument must be a name"))?;
        let callback_type = match &typ.args[1].typ {
            Type::Fn(ft) => ft.clone(),
            t => bail!("collection callback must be a function, got {t}"),
        };
        let callback = Callback { scope: scope.clone(), id, typ: callback_type, top_id };
        let element_type = C::element_type(typ)?;
        let source = genn::reference(ctx, source_id, typ.args[0].typ.clone(), top_id);
        let prototype = Slot::new(ctx, &callback, &element_type, CallKind::Prototype);
        let base = MapQBase {
            source,
            prototype: prototype.call,
            op,
            flavor,
            element_type,
            prototype_id: prototype.id,
            spec,
            typ: typ.rtype.clone(),
        };
        Ok(Self::with(base, callback))
    }

    fn image_decode(
        intrinsic: CollectionIntrinsic,
        ctx: &mut ExecCtx<R, E>,
        buf: &mut &[u8],
    ) -> Result<Node<R, E>, PackError> {
        let CollectionIntrinsic::Map(op, flavor) = intrinsic else {
            return Err(PackError::InvalidFormat);
        };
        let base = MapQBase {
            source: decode_node(ctx, buf)?,
            prototype: decode_node(ctx, buf)?,
            op,
            flavor,
            element_type: Type::decode(buf)?,
            prototype_id: BindId::decode(buf)?,
            spec: Expr::decode(buf)?,
            typ: Type::decode(buf)?,
        };
        Ok(Self::with(base, Callback::image_decode(buf)?))
    }

    fn finish(&self, ctx: &mut ExecCtx<R, E>, event: &Event<E>) -> Value {
        with_hooks(ctx, event, || self.base.finish(&self.slots, &self.current))
    }
}

/// Fold one slot production into the collection's tag via [`Tag::join`].
fn merge_tag(current: Option<Tag>, next: Tag) -> Option<Tag> {
    Some(match current {
        None => next,
        Some(tag) => tag.join(next),
    })
}

impl<R: Rt, E: UserEvent, C: MapCollection> Update<R, E> for MapQ<R, E, C> {
    fn image_len(&self) -> usize {
        let intrinsic = CollectionIntrinsic::Map(self.base.op, self.base.flavor);
        tag_len()
            + intrinsic.encoded_len()
            + self.base.source.image_len()
            + self.base.prototype.image_len()
            + self.base.element_type.encoded_len()
            + self.base.prototype_id.encoded_len()
            + self.base.spec.encoded_len()
            + self.base.typ.encoded_len()
            + self.callback.image_len()
    }

    /// The slots and the current collection exist only once a cycle has
    /// run.
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        if !self.slots.is_empty() {
            return Err(PackError::Application(image::NOT_QUIESCENT));
        }
        put_tag(NodeTag::Collection, buf);
        CollectionIntrinsic::Map(self.base.op, self.base.flavor).encode(buf)?;
        self.base.source.image_encode(buf)?;
        self.base.prototype.image_encode(buf)?;
        self.base.element_type.encode(buf)?;
        self.base.prototype_id.encode(buf)?;
        self.base.spec.encode(buf)?;
        self.base.typ.encode(buf)?;
        self.callback.image_encode(buf)
    }

    fn fuse(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<Option<Node<R, E>>> {
        fuse_callback(ctx, &self.base.prototype)
    }

    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> &TagValue {
        let woke = self.slept.take() && ctx.frame_depth == 0;
        let old_len = self.slots.len();
        let mut production = None;
        let (tag, sval) = {
            let tv = self.base.source.update(ctx, event);
            let tag = tv.tag();
            (tag, if tag.is_bottom() { None } else { Some(tv.value_cloned()) })
        };
        let src_trig = tag.triggers();
        // A tainted or unselectable source is bottom and forgets the length;
        // the slots stay retained (bottom is not a reset) and still run, so
        // their internal state sees this cycle's events.
        let source = sval.and_then(|value| C::select(value, src_trig));
        let source_ok = source.is_some();
        match source {
            None => self.src_bottom = true,
            Some(source) => {
                let kind = CallKind::slot(ctx, &self.base.prototype);
                let resized =
                    resize(ctx, &mut self.slots, source.len(), Slot::delete, |ctx| {
                        Slot::new(
                            ctx,
                            &self.callback,
                            &self.base.element_type,
                            kind.clone(),
                        )
                    });
                // Elements move only on a fire, in a frame (a rebound loop
                // variable arrives stale) or past a sleep; a fresh slot
                // always takes its element.
                let moved = src_trig || ctx.frame_depth > 0 || woke;
                let from = if moved { 0 } else { old_len.min(self.slots.len()) };
                for (slot, value) in
                    self.slots[from..].iter().zip(source.values().skip(from))
                {
                    deliver(ctx, event, slot.id, TagValue::tagged(value, tag));
                }
                self.current = source;
                // A resize or a source back from bottom changes the result
                // whether or not a slot fires, and so do moved elements
                // under a result that reads them.
                let back = std::mem::take(&mut self.src_bottom);
                if resized || back || (self.base.op.reads_elements() && moved) {
                    production = merge_tag(production, tag);
                }
                if self.slots.is_empty() {
                    let v = self.finish(ctx, event);
                    return self.resident.set(TagValue::tagged(v, tag));
                }
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
            let slot = &mut self.slots[i];
            let tv = slot.call.update(ctx, event);
            let tag = tv.tag();
            if gxdbg_slot() {
                eprintln!(
                    "SLOT call[{i}] produced tag={} fresh={}",
                    tag.bits(),
                    i >= old_len
                );
            }
            // Only triggering productions fold into the firing decision.
            if tag.triggers() {
                production = merge_tag(production, tag);
            }
            slot.state.set(tv);
        }
        event.init = saved_init;
        if !source_ok {
            return self.resident.set_bottom(src_trig);
        }
        // Bottomness is a question about the slots now; the production
        // tag decides only the fired bit.
        let poisoned = self.slots.iter().any(|slot| slot.state.is_bottom());
        if gxdbg_slot() {
            eprintln!(
                "SLOT map prod={:?} poisoned={poisoned} slots={:?}",
                production.map(|t| t.bits()),
                self.slots.iter().map(|s| &s.state).collect::<LPooled<Vec<_>>>(),
            );
        }
        let tag = match production {
            Some(tag) => tag,
            None if poisoned => return self.resident.set_bottom(false),
            // After a sleep the resident may lag the stale-refreshed
            // slots; rebuild quietly (design/wake_catchup.md).
            None if woke
                && self.slots.iter().all(|slot| slot.state.value().is_some()) =>
            {
                Tag::STALE
            }
            None => return self.resident.ride(),
        };
        if tag.is_bottom() || poisoned {
            return self.resident.set_bottom(tag.triggers());
        }
        if self.slots.iter().all(|slot| slot.state.value().is_some()) {
            let v = self.finish(ctx, event);
            self.resident.set(TagValue::tagged(v, tag))
        } else {
            self.resident.set_bottom(tag.triggers())
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
        self.slept.set();
        // Slot values survive sleep: sleep is pause.
        self.base.source.sleep(ctx);
        for slot in self.slots.iter_mut() {
            slot.call.sleep(ctx);
        }
    }

    fn reset_replay(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.base.source.reset_replay(ctx);
        self.current = C::default();
        for slot in self.slots.iter_mut() {
            slot.state = SlotState::Empty;
            slot.call.reset_replay(ctx);
        }
    }

    fn view(&self) -> NodeView<'_, R, E> {
        NodeView::MapQ(&self.base)
    }

    fn emit_clif(&self, cx: &mut BodyCx) -> Result<CompiledExpr> {
        self.base
            .emit(&self.base.source, cx)?
            .ok_or_else(|| anyhow!("collection operation does not emit CLIF"))
    }
}

#[derive(Debug)]
struct FoldSlot<R: Rt, E: UserEvent> {
    acc_id: BindId,
    element_id: BindId,
    call: Node<R, E>,
    state: SlotState,
}

impl<R: Rt, E: UserEvent> FoldSlot<R, E> {
    fn new(
        ctx: &mut ExecCtx<R, E>,
        callback: &Callback,
        acc_type: &Type,
        element_type: &Type,
        kind: CallKind,
    ) -> Self {
        let (acc_id, acc) = callback.arg(ctx, "collection_acc", acc_type);
        let (element_id, element) = callback.arg(ctx, "collection_element", element_type);
        let call = callback.call(ctx, smallvec![acc, element], kind);
        Self { acc_id, element_id, call, state: SlotState::Empty }
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
    flavor: Flavor,
    element_type: Type,
    prototype_ids: [BindId; 2],
    spec: Expr,
    typ: Type,
}

impl<R: Rt, E: UserEvent> FoldQBase<R, E> {
    /// The fused loop for a call site's intrinsic call.
    pub(crate) fn emit_clif_call(
        &self,
        callsite: &CallSite<R, E>,
        cx: &mut BodyCx,
    ) -> Result<Option<CompiledExpr>> {
        let (Some(source), Some(init)) =
            (callsite.arg_positional(0), callsite.arg_positional(1))
        else {
            return Ok(None);
        };
        let prev = cx.swap_collection_site(Some(callsite.spec.id));
        let r = self.emit(source, init, cx);
        cx.swap_collection_site(prev);
        r
    }

    fn emit(
        &self,
        source: &Node<R, E>,
        init: &Node<R, E>,
        cx: &mut BodyCx,
    ) -> Result<Option<CompiledExpr>> {
        if emit::node_is_bottom(source) || emit::node_is_bottom(init) {
            return Ok(None);
        }
        let Some(callback) = callback(&self.prototype) else { return Ok(None) };
        let Some(acc) = callback_param(callback, 0, literal!("__acc")) else {
            return Ok(None);
        };
        let Some(element) = callback_param(callback, 1, literal!("__elem")) else {
            return Ok(None);
        };
        let fold =
            FoldParts { init, body: callback.body(), acc: &acc, element: &element };
        emit_fold_kind(
            cx,
            source,
            fold,
            &callback.typ().rtype,
            &self.element_type,
            self.flavor,
        )
    }

    pub(crate) fn callback_body(&self) -> Option<&Node<R, E>> {
        callback(&self.prototype).map(GXLambda::body)
    }
}

#[derive(Debug)]
struct FoldQ<R: Rt, E: UserEvent, C: MapCollection> {
    slept: WakeBit,
    base: FoldQBase<R, E>,
    callback: Callback,
    acc_type: Type,
    slots: LPooled<Vec<FoldSlot<R, E>>>,
    /// The source was bottom (or never read): its length is forgotten,
    /// so its return changes the result.
    src_bottom: bool,
    collection: PhantomData<C>,
    resident: TagValue,
}

impl<R: Rt, E: UserEvent, C: MapCollection> FoldQ<R, E, C> {
    fn with(base: FoldQBase<R, E>, callback: Callback, acc_type: Type) -> Node<R, E> {
        Node::new(Self {
            slept: WakeBit::default(),
            base,
            callback,
            acc_type,
            slots: LPooled::take(),
            src_bottom: true,
            collection: PhantomData,
            resident: TagValue::phantom(),
        })
    }

    fn new(
        intrinsic: CollectionIntrinsic,
        ctx: &mut ExecCtx<R, E>,
        spec: Expr,
        scope: &Scope,
        top_id: ExprId,
        typ: &Arc<FnType>,
        args: &[StructPatternNode],
    ) -> Result<Node<R, E>> {
        let CollectionIntrinsic::Fold(flavor) = intrinsic else {
            bail!("{intrinsic:?} is not a fold intrinsic")
        };
        if typ.args.len() != 3 || args.len() != 3 {
            bail!("collection fold intrinsic requires three arguments")
        }
        let source_id = args[0]
            .single_bind_id()
            .ok_or_else(|| anyhow!("collection source argument must be a name"))?;
        let init_id = args[1]
            .single_bind_id()
            .ok_or_else(|| anyhow!("collection init argument must be a name"))?;
        let id = args[2]
            .single_bind_id()
            .ok_or_else(|| anyhow!("collection callback argument must be a name"))?;
        let callback_type = match &typ.args[2].typ {
            Type::Fn(ft) => ft.clone(),
            t => bail!("collection callback must be a function, got {t}"),
        };
        let callback = Callback { scope: scope.clone(), id, typ: callback_type, top_id };
        let acc_type = typ.args[1].typ.clone();
        let element_type = C::element_type(typ)?;
        let source = genn::reference(ctx, source_id, typ.args[0].typ.clone(), top_id);
        let init = genn::reference(ctx, init_id, acc_type.clone(), top_id);
        let prototype =
            FoldSlot::new(ctx, &callback, &acc_type, &element_type, CallKind::Prototype);
        let base = FoldQBase {
            source,
            init,
            prototype: prototype.call,
            flavor,
            element_type,
            prototype_ids: [prototype.acc_id, prototype.element_id],
            spec,
            typ: typ.rtype.clone(),
        };
        Ok(Self::with(base, callback, acc_type))
    }

    fn image_decode(
        intrinsic: CollectionIntrinsic,
        ctx: &mut ExecCtx<R, E>,
        buf: &mut &[u8],
    ) -> Result<Node<R, E>, PackError> {
        let CollectionIntrinsic::Fold(flavor) = intrinsic else {
            return Err(PackError::InvalidFormat);
        };
        let base = FoldQBase {
            source: decode_node(ctx, buf)?,
            init: decode_node(ctx, buf)?,
            prototype: decode_node(ctx, buf)?,
            flavor,
            element_type: Type::decode(buf)?,
            prototype_ids: [BindId::decode(buf)?, BindId::decode(buf)?],
            spec: Expr::decode(buf)?,
            typ: Type::decode(buf)?,
        };
        let callback = Callback::image_decode(buf)?;
        let acc_type = Type::decode(buf)?;
        Ok(Self::with(base, callback, acc_type))
    }
}

/// Deliver `tv` to a callback argument this cycle and stand it in the
/// store, a bottom as a stale bottom.
fn deliver<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<R, E>,
    event: &mut Event<E>,
    id: BindId,
    tv: TagValue,
) {
    let standing = if tv.tag().is_bottom() {
        TagValue::tagged(Value::Null, Tag::STALE_BOTTOM)
    } else {
        tv.clone()
    };
    ctx.rt.store_insert(id, standing);
    event.variables.insert(id, tv);
}

impl<R: Rt, E: UserEvent, C: MapCollection> Update<R, E> for FoldQ<R, E, C> {
    fn image_len(&self) -> usize {
        tag_len()
            + CollectionIntrinsic::Fold(self.base.flavor).encoded_len()
            + self.base.source.image_len()
            + self.base.init.image_len()
            + self.base.prototype.image_len()
            + self.base.element_type.encoded_len()
            + self.base.prototype_ids[0].encoded_len()
            + self.base.prototype_ids[1].encoded_len()
            + self.base.spec.encoded_len()
            + self.base.typ.encoded_len()
            + self.callback.image_len()
            + self.acc_type.encoded_len()
    }

    /// The slots exist only once a cycle has run.
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        if !self.slots.is_empty() {
            return Err(PackError::Application(image::NOT_QUIESCENT));
        }
        put_tag(NodeTag::Collection, buf);
        CollectionIntrinsic::Fold(self.base.flavor).encode(buf)?;
        self.base.source.image_encode(buf)?;
        self.base.init.image_encode(buf)?;
        self.base.prototype.image_encode(buf)?;
        self.base.element_type.encode(buf)?;
        self.base.prototype_ids[0].encode(buf)?;
        self.base.prototype_ids[1].encode(buf)?;
        self.base.spec.encode(buf)?;
        self.base.typ.encode(buf)?;
        self.callback.image_encode(buf)?;
        self.acc_type.encode(buf)
    }

    fn fuse(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<Option<Node<R, E>>> {
        fuse_callback(ctx, &self.base.prototype)
    }

    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> &TagValue {
        let woke = self.slept.take() && ctx.frame_depth == 0;
        let old_len = self.slots.len();
        let (tag, sval) = {
            let tv = self.base.source.update(ctx, event);
            let tag = tv.tag();
            (tag, if tag.is_bottom() { None } else { Some(tv.value_cloned()) })
        };
        let src_trig = tag.triggers();
        // A tainted or unselectable source is bottom and forgets the
        // length; the slots stay retained and the slot walk still runs.
        let source = sval.and_then(|value| C::select(value, src_trig));
        let source_ok = source.is_some();
        let (mut resized, mut back) = (false, false);
        match source {
            None => self.src_bottom = true,
            Some(source) => {
                back = std::mem::take(&mut self.src_bottom);
                let kind = CallKind::slot(ctx, &self.base.prototype);
                resized =
                    resize(ctx, &mut self.slots, source.len(), FoldSlot::delete, |ctx| {
                        let (acc, elt) = (&self.acc_type, &self.base.element_type);
                        FoldSlot::new(ctx, &self.callback, acc, elt, kind.clone())
                    });
                // Elements move only on a fire, in a frame or past a sleep; a
                // fresh slot always takes its element.
                let moved = src_trig || ctx.frame_depth > 0 || woke;
                let from = if moved { 0 } else { old_len.min(self.slots.len()) };
                for (slot, value) in
                    self.slots[from..].iter().zip(source.values().skip(from))
                {
                    deliver(ctx, event, slot.element_id, TagValue::tagged(value, tag));
                }
            }
        }
        // A bottom init is a poisoned delivery to slot 0's acc, not a
        // whole-fold abort: a callback that never consumes the acc
        // recovers.
        let init = self.base.init.update(ctx, event).clone();
        if let Some(slot) = self.slots.first() {
            deliver(ctx, event, slot.acc_id, init.clone());
        }
        if self.slots.is_empty() && source_ok {
            return match init.tag() {
                t if t.is_bottom() => self.resident.set_bottom(t.triggers()),
                t => {
                    self.resident.set(TagValue::tagged(init.value_cloned(), tag.join(t)))
                }
            };
        }
        // Only slot productions seed the firing decision: a source or
        // init delivery reaches the result only through a slot that
        // consumes it. A triggering taint still counts, for the bottom arm.
        let mut any_trig = !source_ok && src_trig;
        let saved_init = event.init;
        for i in 0..self.slots.len() {
            if ctx.interrupted() {
                event.init = saved_init;
                return self.resident.ride();
            }
            // A fresh slot's first dispatch runs under a forced init view,
            // its acc seeded with the chain's state as it stands.
            if i >= old_len {
                event.init = true;
                let seed = match i {
                    0 if init.tag().is_bottom() => {
                        Some(TagValue::tagged(Value::Null, Tag::FRESH_BOTTOM))
                    }
                    0 => Some(TagValue::fired(init.value_cloned())),
                    _ => match &self.slots[i - 1].state {
                        SlotState::Bottom => {
                            Some(TagValue::tagged(Value::Null, Tag::FRESH_BOTTOM))
                        }
                        SlotState::Value(v) => Some(TagValue::fired(v.clone())),
                        SlotState::Empty => None,
                    },
                };
                if let Some(seed) = seed {
                    deliver(ctx, event, self.slots[i].acc_id, seed);
                }
            }
            let slot = &mut self.slots[i];
            let tv = slot.call.update(ctx, event).clone();
            any_trig |= tv.tag().triggers();
            slot.state.set(&tv);
            // The production, a bottom included, travels the acc chain.
            if let Some(next) = self.slots.get(i + 1) {
                deliver(ctx, event, next.acc_id, tv);
            }
        }
        event.init = saved_init;
        // An interior slot's poison bottoms the fold only if a downstream
        // callback consumes it; only the last slot's state is the result.
        match self.slots.last().map(|s| &s.state) {
            _ if !source_ok => self.resident.set_bottom(any_trig),
            Some(SlotState::Bottom) => self.resident.set_bottom(any_trig || resized),
            // A fold fires iff it resized, a slot fired, or the source
            // fired back from bottom.
            Some(SlotState::Value(v)) => {
                let fired = resized || any_trig || (src_trig && back);
                let tag = if fired { Tag::FIRED } else { Tag::STALE };
                let v = v.clone();
                self.resident.set(TagValue::tagged(v, tag))
            }
            Some(SlotState::Empty) | None => self.resident.ride(),
        }
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
        // The slot states survive sleep: sleep is pause.
        self.slept.set();
        self.base.source.sleep(ctx);
        self.base.init.sleep(ctx);
        for slot in self.slots.iter_mut() {
            slot.call.sleep(ctx);
        }
    }

    fn reset_replay(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.base.source.reset_replay(ctx);
        self.base.init.reset_replay(ctx);
        for slot in self.slots.iter_mut() {
            slot.state = SlotState::Empty;
            slot.call.reset_replay(ctx);
        }
    }

    fn view(&self) -> NodeView<'_, R, E> {
        NodeView::FoldQ(&self.base)
    }

    fn emit_clif(&self, cx: &mut BodyCx) -> Result<CompiledExpr> {
        self.base
            .emit(&self.base.source, &self.base.init, cx)?
            .ok_or_else(|| anyhow!("collection fold does not emit CLIF"))
    }
}

fn callback<R: Rt, E: UserEvent>(prototype: &Node<R, E>) -> Option<&GXLambda<R, E>> {
    let NodeView::CallSite(site) = prototype.view() else { return None };
    let ApplyView::Lambda(callback) = site.resolved_apply()? else { return None };
    Some(callback)
}

/// The fuse driver never descends a collection callback, so the
/// prototype body's attributes dispatch here.
fn fuse_callback<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<R, E>,
    prototype: &Node<R, E>,
) -> Result<Option<Node<R, E>>> {
    if !ctx.attr_census.lock().is_empty()
        && let Some(callback) = callback(prototype)
    {
        check_attributes_subtree(callback.body(), ctx)?;
    }
    Ok(None)
}

impl Flavor {
    /// This flavor's collection over `elems`, which it drains.
    fn build(self, elems: &mut LPooled<Vec<Value>>) -> Value {
        match self {
            Self::Array => Value::Array(ValArray::from_iter_exact(elems.drain(..))),
            Self::List => list::from_iter(elems.drain(..)),
            Self::CMap => pairs_to_map(elems.iter()),
        }
    }

    /// Append a flat_map callback's result: its elements when it is this
    /// flavor's collection, else itself.
    fn extend(self, elems: &mut LPooled<Vec<Value>>, v: &Value) {
        match (self, v) {
            (Self::Array, Value::Array(a)) => elems.extend(a.iter().cloned()),
            (Self::List, v) if list::is_list(v) => {
                elems.extend(list::Iter::new(v.clone()))
            }
            (_, v) => elems.push(v.clone()),
        }
    }

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
                let ownership = emit::node_composite_source(source);
                let array = source.emit_clif(cx)?;
                let src = scaffold::ArraySrc {
                    ptr: array.payload,
                    disc: array.disc,
                    ownership,
                };
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

/// Emit a loop over `source` built by `emit`, which gets the flattened
/// source; the source's firing folds into the loop's result.
fn emit_loop<'a, 'f, 'c, R: Rt, E: UserEvent>(
    cx: &mut BodyCx<'a, 'f, 'c>,
    source: &Node<R, E>,
    flavor: Flavor,
    emit: impl FnOnce(
        &mut BodyCx<'a, 'f, 'c>,
        scaffold::ArraySrc,
    ) -> Result<(CompiledExpr, scaffold::SlotFlags)>,
) -> Result<Option<CompiledExpr>> {
    let (value, src) = flavor.emit_source(cx, source)?;
    let source_invariant = emit::node_loop_invariant_ref(cx, source);
    let (result, flags) = emit(cx, src)?;
    Ok(Some(finish_loop_result(cx, result, flags, &value, source_invariant)))
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
    emit_loop(cx, source, flavor, |cx, src| {
        let output_source = emit::node_composite_source(body);
        let (ptr, flags) = scaffold::emit_map_loop(
            cx,
            src,
            &param.elem(&element_type, &leaves),
            &output_type,
            output_source,
            &emit::slot_state_sites(body),
            |cx| body.emit_clif(cx),
        )?;
        Ok((flavor.emit_result(cx, ptr)?, flags))
    })
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
    emit_loop(cx, source, flavor, |cx, src| {
        let (ptr, flags) = scaffold::emit_filter_loop(
            cx,
            src,
            &param.elem(&element_type, &leaves),
            &emit::slot_state_sites(body),
            |cx| body.emit_clif(cx),
        )?;
        Ok((flavor.emit_result(cx, ptr)?, flags))
    })
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
    emit_loop(cx, source, flavor, |cx, src| {
        let output_source = emit::node_composite_source(body);
        let (ptr, flags) = scaffold::emit_filter_map_loop(
            cx,
            src,
            &param.elem(&element_type, &leaves),
            &output_element,
            output_source,
            &emit::slot_state_sites(body),
            |cx| body.emit_clif(cx),
        )?;
        Ok((flavor.emit_result(cx, ptr)?, flags))
    })
}

fn emit_flat_map_kind<R: Rt, E: UserEvent>(
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
    // A List callback's return is an opaque Value; the extend helper walks
    // it. No Map flat_map intrinsic exists.
    let output_kind = kernel_abi::freeze_for_abi_normalized(body.typ())
        .as_ref()
        .and_then(|typ| kernel_abi::abi_kind(typ));
    let extend = match (flavor, output_kind) {
        (Flavor::Array, Some(AbiKind::Array)) => scaffold::FlatMapExtend::Array,
        (Flavor::List, Some(AbiKind::Value)) => scaffold::FlatMapExtend::List,
        _ => return Ok(None),
    };
    emit_loop(cx, source, flavor, |cx, src| {
        let body_source = emit::node_composite_source(body);
        let (ptr, flags) = scaffold::emit_flat_map_loop(
            cx,
            src,
            &param.elem(&element_type, &leaves),
            extend,
            &emit::slot_state_sites(body),
            |cx| {
                let value = body.emit_clif(cx)?;
                match extend {
                    scaffold::FlatMapExtend::Array => {
                        let payload = emit::ensure_owned_composite_src(
                            cx,
                            body_source,
                            value.payload,
                        )?;
                        Ok(CompiledExpr::new(value.disc, payload))
                    }
                    scaffold::FlatMapExtend::List => {
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
        Ok((flavor.emit_result(cx, ptr)?, flags))
    })
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
    emit_loop(cx, source, flavor, |cx, src| {
        let ((disc, payload), flags) = scaffold::emit_find_loop(
            cx,
            src,
            &param.elem(&element_type, &leaves),
            &emit::slot_state_sites(body),
            |cx| body.emit_clif(cx),
        )?;
        Ok((CompiledExpr::new(disc, payload), flags))
    })
}

fn emit_find_map_kind<R: Rt, E: UserEvent>(
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
    let output_is_nullable = matches!(
        kernel_abi::freeze_for_abi_normalized(body.typ())
            .as_ref()
            .and_then(|typ| kernel_abi::abi_kind(typ)),
        Some(AbiKind::Nullable)
    );
    if !output_is_nullable {
        return Ok(None);
    }
    emit_loop(cx, source, flavor, |cx, src| {
        let body_source = emit::node_composite_source(body);
        let ((disc, payload), flags) = scaffold::emit_find_map_loop(
            cx,
            src,
            &param.elem(&element_type, &leaves),
            &emit::slot_state_sites(body),
            |cx| {
                let value = body.emit_clif(cx)?;
                emit::ensure_owned_value_src(cx, body_source, value.disc, value.payload)
            },
        )?;
        Ok((CompiledExpr::new(disc, payload), flags))
    })
}

/// A fold's callback parts: its init, body and parameters.
struct FoldParts<'a, R: Rt, E: UserEvent> {
    init: &'a Node<R, E>,
    body: &'a Node<R, E>,
    acc: &'a CallbackParam,
    element: &'a CallbackParam,
}

/// The fold kind. A List- or Map-valued accumulator has no `FoldAcc`
/// carry and stays interpreted.
fn emit_fold_kind<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    source: &Node<R, E>,
    fold: FoldParts<R, E>,
    acc_type: &Type,
    element_type: &Type,
    flavor: Flavor,
) -> Result<Option<CompiledExpr>> {
    let FoldParts { init, body, acc, element } = fold;
    let Some((element_type, element_leaves)) =
        bindable_array_element(element_type, &element.binds)
    else {
        return Ok(None);
    };
    let Some(acc_type) = kernel_abi::freeze_for_abi_normalized(acc_type) else {
        return Ok(None);
    };
    // A Bottom-typed body unifies with any acc type but emits a
    // shapeless placeholder that violates the owned-acc discipline.
    if emit::node_is_bottom(body) {
        return Ok(None);
    }
    let acc_leaves;
    let acc_shape = match kernel_abi::abi_kind(&acc_type) {
        Some(AbiKind::Scalar(prim)) if acc.binds.is_empty() => {
            scaffold::FoldAcc::Scalar(prim)
        }
        Some(AbiKind::Array | AbiKind::Tuple | AbiKind::Struct) => {
            let Some(leaves) = scaffold::elem_leaves(&acc_type, &acc.binds) else {
                return Ok(None);
            };
            acc_leaves = leaves;
            scaffold::FoldAcc::Composite {
                init_src: emit::node_composite_source(init),
                body_src: emit::node_composite_source(body),
                leaves: &acc_leaves,
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
    let operand = move |cx: &mut BodyCx, n: &Node<R, E>| {
        if value_acc {
            emit::emit_owned_value_operand_node(cx, n)
        } else {
            n.emit_clif(cx)
        }
    };
    emit_loop(cx, source, flavor, |cx, src| {
        scaffold::emit_fold_loop(
            cx,
            src,
            acc_shape,
            &acc.name,
            acc.id,
            &element.elem(&element_type, &element_leaves),
            &emit::slot_state_sites(body),
            |cx| operand(cx, init),
            |cx| operand(cx, body),
        )
    })
}
