use super::{
    NOP, WakeBit, callsite::CallSite, coretraits::with_hooks, genn, lambda::GXLambda,
    list, pattern::StructPatternNode,
};
use crate::cost::{ProbeSite, SlotPlan, SlotSite};
use crate::{
    ApplyView, BindId, CompileCtx, ExecCtx, Node, NodeView, Refs, Rt, Scope, Tag,
    TagValue, Update, UserEvent,
    dbgenv::gxdbg_slot,
    expr::{Expr, ExprId},
    fusion::{
        emit::{self, BodyCx, CompiledExpr, CompositeSource, scaffold},
        kernel_abi::{self, AbiKind, PrimType},
        share::{self, SlotShare},
    },
    image::{
        self, ImageBuf,
        nodes::{NodeTag, decode_node, put_tag},
        scope_decode, scope_encode,
    },
    typ::{FnArgKind, FnType, Type},
    wrap,
};
use anyhow::{Result, anyhow, bail};
use arcstr::{ArcStr, literal};
use bytes::BufMut;
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
        ctx: &mut CompileCtx<R, E>,
        spec: Expr,
        scope: &Scope,
        top_id: ExprId,
        typ: &Arc<FnType>,
        args: &[StructPatternNode],
    ) -> Result<Node<R, E>> {
        by_collection!(self, new(self, ctx, spec, scope, top_id, typ, args))
    }

    pub(crate) fn image_decode<R: Rt, E: UserEvent>(
        ctx: &mut ExecCtx<'_, R, E>,
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

/// The map a Map HOF builds from its `[k, v]` pairs, both engines; the
/// last pair of a key wins, as in a literal, and a malformed pair is
/// logged and skipped. Key order reads the core-trait hooks, so the
/// caller runs this under them.
pub(crate) fn pairs_to_map<'a>(pairs: impl IntoIterator<Item = &'a Value>) -> Value {
    let mut kv: LPooled<Vec<(Value, Value)>> = LPooled::take();
    kv.extend(pairs.into_iter().filter_map(|v| {
        let pair = split_pair(v);
        if pair.is_none() {
            log::error!("map result: malformed pair {v:?}");
        }
        pair
    }));
    // insert_many keeps the first of equal keys, so it sees them last
    // first; an Ord that is no total order (a user impl) may panic its
    // sort, and the map is then built a pair at a time, wrong but whole
    let bulk = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        CMap::from_iter(kv.iter().rev().cloned())
    }));
    Value::Map(bulk.unwrap_or_else(|_| {
        let mut m = CMap::new();
        for (k, v) in kv.drain(..) {
            m.insert_cow(k, v);
        }
        m
    }))
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

/// The diagnostic of a collection init count past the element limit,
/// from both engines.
pub(crate) fn log_init_oversize(n: i64) {
    log::error!(
        "collection init size {n} exceeds the {MAX_ARRAY_INIT_LEN} element limit"
    );
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

    fn select(value: Value, fired: bool) -> Option<Self> {
        let Value::I64(n) = value else { return None };
        if n > MAX_ARRAY_INIT_LEN {
            if fired {
                log_init_oversize(n);
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
// CR claude for claude: [dead] SlotState::Empty is never observed: set() never writes it,
// and every read follows a run of every slot (an interrupt returns ride() before any
// read). So once poisoned is false every slot holds a value: the all(value().is_some())
// tests at 1100 and 1109 are always true and the else at 1112-1114 is dead, as are
// FoldQ's SlotState::Empty seed (1497) and its Some(SlotState::Empty) arm (1527). Drop
// the variant (a slot holds its last value or bottom) and the dead arms.
// (c-collection-04)
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
    /// The kernels a slot takes from the fused prototype.
    share: Option<SlotShare>,
}

impl Callback {
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        scope_encode(&self.scope, buf)?;
        self.id.encode(buf)?;
        self.typ.encode(buf)?;
        self.top_id.encode(buf)?;
        match &self.share {
            None => Ok(buf.put_u8(0)),
            Some(share) => {
                buf.put_u8(1);
                share.image_encode(buf)
            }
        }
    }

    fn image_decode<R: Rt, E: UserEvent>(
        ctx: &mut ExecCtx<'_, R, E>,
        buf: &mut &[u8],
    ) -> Result<Self, PackError> {
        let scope = scope_decode(buf)?;
        let id = BindId::decode(buf)?;
        let typ = Pack::decode(buf)?;
        let top_id = ExprId::decode(buf)?;
        let share = match u8::decode(buf)? {
            0 => None,
            _ => Some(SlotShare::image_decode(ctx, buf)?),
        };
        Ok(Self { scope, id, typ, top_id, share })
    }

    /// A fresh bind for one callback argument.
    fn arg<R: Rt, E: UserEvent>(
        &self,
        ctx: &mut CompileCtx<R, E>,
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
        ctx: &mut CompileCtx<R, E>,
        args: SmallVec<[Node<R, E>; 2]>,
        kind: CallKind,
    ) -> Node<R, E> {
        let (Self { scope, id, typ, top_id, share }, fty) =
            (self, Type::Fn(self.typ.clone()));
        match kind {
            CallKind::Prototype => {
                let function = genn::reference(ctx, *id, fty, *top_id);
                genn::apply_prototype(function, scope.clone(), args, typ, *top_id)
            }
            CallKind::Slot(Some(def)) => {
                let function = super::Constant::new(def, fty, Expr::clone(&NOP));
                let mut call = genn::apply(function, scope.clone(), args, typ, *top_id);
                if let Some(cs) = call.downcast_mut::<CallSite<R, E>>() {
                    cs.share = share.clone();
                }
                call
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
    fn slot<R: Rt, E: UserEvent>(
        ctx: &ExecCtx<'_, R, E>,
        prototype: &Node<R, E>,
    ) -> Self {
        // CR claude for claude: [bug] When resolve_trait_call has lowered the prototype
        // (a core trait, or a user trait over a union element type), it views as its
        // lowered block and has no static_target. So this returns Slot(None), and every
        // slot calls through the callback parameter. That parameter is bound to the
        // dispatcher, which never holds a value. As a result `array::map([1, 2],
        // Display::fmt)` and `array::map(ys, Show::show)` over `Array<[A, B]>` produce
        // nothing in both engines. fold, init and list::map behave the same, `--check`
        // passes, and nothing is logged. A sibling case diverges: in `app(Display::fmt,
        // [1, 2])` with `app = |f: fn(x: i64) -> string, xs: Array<i64>| array::map(xs,
        // |x| f(x))`, the node-walk produces nothing but the JIT gives ["1", "2"]. A
        // slot's instance is built at run time, after unregister_fn_params has dropped
        // f from trait_methods. probe:
        // design/review-2026-10-05/repro/x-engine-seq-errors-03.gx
        // (x-engine-seq-errors-03)
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
    ctx: &mut ExecCtx<'_, R, E>,
    slots: &mut Vec<S>,
    n: usize,
    delete: fn(&mut S, &mut ExecCtx<'_, R, E>),
    mut add: impl FnMut(&mut ExecCtx<'_, R, E>) -> S,
) -> bool {
    let old = slots.len();
    for mut s in slots.drain(n.min(old)..) {
        delete(&mut s, ctx)
    }
    if slots.len() < n {
        while slots.len() < n {
            let s = add(ctx);
            slots.push(s)
        }
        ctx.apply_deferred();
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
        ctx: &mut CompileCtx<R, E>,
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

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
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
        leaves: &'a [scaffold::Leaf],
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
) -> Option<(Type, scaffold::Leaves)> {
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
            | Type::ByRef(..),
        ) => false,
        _ => true,
    })
}

/// Fold the loop's [`scaffold::SlotFlags`] and the source's firing
/// into the emitted result — the shared tail of every kind emitter.
fn finish_loop_result(
    cx: &mut BodyCx,
    result: CompiledExpr,
    flags: scaffold::SlotFlags,
    source: &CompiledExpr,
) -> CompiledExpr {
    flags.apply(cx, result, source.disc)
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
    fork: SlotSite,
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
            fork: SlotSite::default(),
        })
    }

    fn new(
        intrinsic: CollectionIntrinsic,
        ctx: &mut CompileCtx<R, E>,
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
        let callback = Callback {
            scope: scope.clone(),
            id,
            typ: callback_type,
            top_id,
            share: None,
        };
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
        ctx: &mut ExecCtx<'_, R, E>,
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
        Ok(Self::with(base, Callback::image_decode(ctx, buf)?))
    }

    fn finish(&self, ctx: &mut ExecCtx<'_, R, E>) -> Value {
        with_hooks(ctx, || self.base.finish(&self.slots, &self.current))
    }
}

/// Fold one slot production into the collection's tag via [`Tag::join`].
fn merge_tag(current: Option<Tag>, next: Tag) -> Option<Tag> {
    Some(match current {
        None => next,
        Some(tag) => tag.join(next),
    })
}

/// Update `slots`, in order or forked where the site's plans say: the
/// join of their triggering productions, or `None` when an interrupt
/// stopped the loop. A slot at `old_len` or past it is fresh, and its
/// first dispatch runs under a forced init view.
fn update_slots<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<'_, R, E>,
    slots: &mut [Slot<R, E>],
    site: &mut SlotSite,
    old_len: usize,
) -> Option<Option<Tag>> {
    let (standing, fresh) = slots.split_at_mut(old_len.min(slots.len()));
    let n = standing.len();
    // CR claude for claude: [bug] The standing slots fork here on cost alone, and the
    // fresh ones fork through site.fresh.run below. Nothing tells the collection that
    // its callback reaches an ordered or opaque call, so slots that share one queuefn
    // queue push into it and pop from it in thread order. That breaks the rule
    // plan_block enforces for statements (design/parallel_eval.md §3.2, §6). In the
    // default Auto mode the program's value then depends on scheduling. Probe:
    // design/review-2026-10-05/repro/c-collection-02.gx prints null under
    // GRAPHIX_PAR=off and an out-of-order index in most default runs, and graphix-fuzz
    // check on `array::map([1, 2, 3, 4, 5, 6, 7, 8], |x| q(x) ~ n)` over a shared
    // queuefn reports a parallel-evaluation DIVERGENCE. The ForkSite users (gather,
    // call arguments, operands) have the same hole: `(q(1) ~ n, q(2) ~ n, q(3) ~ n,
    // q(4) ~ n)` comes out in a different order under GRAPHIX_PAR=force.
    // (c-collection-02)
    let production = match site.plan(ctx, n) {
        SlotPlan::Serial => update_slots_in_order(ctx, standing, 0, old_len)?,
        SlotPlan::Measure(t0) => {
            let r = update_slots_in_order(ctx, standing, 0, old_len)?;
            site.measured(t0, n);
            r
        }
        SlotPlan::Fork { grain } => update_ranges(ctx, standing, 0, old_len, grain)?,
    };
    site.fresh.run(
        ctx,
        fresh,
        n,
        production,
        |ctx, slot, at| {
            update_slots_in_order(ctx, std::slice::from_mut(slot), at, old_len)
        },
        |ctx, slots, at, grain| match grain {
            None => update_slots_in_order(ctx, slots, at, old_len),
            Some(grain) => update_ranges(ctx, slots, at, old_len, grain),
        },
        merge_tags,
    )
}

fn merge_tags(a: Option<Tag>, b: Option<Tag>) -> Option<Tag> {
    match b {
        Some(tag) => merge_tag(a, tag),
        None => a,
    }
}

/// Update `slots`, the first at index `at`, in sibling branches of
/// `grain` slots each.
fn update_ranges<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<'_, R, E>,
    slots: &mut [Slot<R, E>],
    at: usize,
    old_len: usize,
    grain: usize,
) -> Option<Option<Tag>> {
    let n = slots.len();
    let grain = grain.max(1);
    if n < 2 * grain {
        return update_slots_in_order(ctx, slots, at, old_len);
    }
    let ranges = ranges(n, grain);
    let parts = crate::branch::cut(slots, &ranges);
    let parts = parts.into_iter().zip(ranges.iter().map(|r| at + r.0));
    let mut production = None;
    for p in crate::branch::fork_each(ctx, parts, |c, (p, at)| {
        update_slots_in_order(c, p, at, old_len)
    }) {
        production = merge_tags(production, p?);
    }
    Some(production)
}

/// `0..n` in ranges of `grain`.
fn ranges(n: usize, grain: usize) -> LPooled<Vec<(usize, usize)>> {
    (0..n).step_by(grain).map(|lo| (lo, (lo + grain).min(n))).collect()
}

/// Build the instances of `fresh` slots' callbacks in compile tasks,
/// ahead of their first updates, where `site` says the builds pay for
/// it; a slot built in order binds at its first update. `call` is a
/// slot's call.
// CR claude for claude: [perf] build_fresh runs on every update with a valid source, as
// does CallKind::slot (a lambda_defs lookup and a Value clone, 1035/1437), though both
// matter only when slots were added. With nothing fresh, its apply_deferred still
// drains pending_refs, and a hashbrown drain rewrites every control byte of a table
// that keeps the capacity of the largest compile batch; in a forked branch the DerefMut
// on ctx.cx also boxes a CompileCtx fork that the merge joins back. Return early when
// fresh is empty, and build the CallKind inside resize's add closure. (c-collection-05)
fn build_fresh<R: Rt, E: UserEvent, S: Send>(
    ctx: &mut ExecCtx<'_, R, E>,
    fresh: &mut [S],
    site: &mut ProbeSite,
    call: fn(&mut S) -> &mut Node<R, E>,
) {
    let prebind = |ctx: &mut CompileCtx<R, E>, slots: &mut [S]| {
        for slot in slots {
            if let Some(cs) = call(slot).downcast_mut::<CallSite<R, E>>() {
                cs.prebind(ctx)
            }
        }
    };
    site.run(
        ctx,
        fresh,
        0,
        (),
        |ctx, slot, _| Some(prebind(ctx, std::slice::from_mut(slot))),
        |ctx, slots, _, grain| {
            if let Some(grain) = grain {
                let ranges = ranges(slots.len(), grain);
                crate::branch::compile_each(
                    ctx,
                    crate::branch::cut(slots, &ranges),
                    prebind,
                )
            }
            Some(())
        },
        |(), ()| (),
    );
    ctx.apply_deferred();
}

fn update_slots_in_order<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<'_, R, E>,
    slots: &mut [Slot<R, E>],
    at: usize,
    old_len: usize,
) -> Option<Option<Tag>> {
    let mut production = None;
    for (j, slot) in slots.iter_mut().enumerate() {
        let i = at + j;
        if ctx.interrupted() {
            return None;
        }
        if i >= old_len {
            ctx.event.init = true;
        }
        let tv = slot.call.update(ctx);
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
    Some(production)
}

impl<R: Rt, E: UserEvent, C: MapCollection> Update<R, E> for MapQ<R, E, C> {
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

    fn fuse(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<Option<Node<R, E>>> {
        fuse_callback(ctx, &mut self.base.prototype, &mut self.callback)
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R, E>) -> &TagValue {
        let woke = self.slept.take();
        let old_len = self.slots.len();
        let mut production = None;
        let (tag, sval) = {
            let tv = self.base.source.update(ctx);
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
                let fresh = old_len.min(self.slots.len());
                build_fresh(ctx, &mut self.slots[fresh..], &mut self.fork.build, |s| {
                    &mut s.call
                });
                // Elements move only on a fire, in a frame (a rebound loop
                // variable arrives stale) or past a sleep; a fresh slot
                // always takes its element.
                // CR claude for claude: [doc-drift] The comment above says elements also
                // move 'in a frame (a rebound loop variable arrives stale)', but the
                // node-walk has no frames (design/tail_calls_are_calls.md) and moved is
                // src_trig || woke; FoldQ's copy at 1447 says the same. Drop the frame
                // clause in both. (c-collection-07)
                let moved = src_trig || woke;
                let from = if moved { 0 } else { old_len.min(self.slots.len()) };
                for (slot, value) in
                    self.slots[from..].iter().zip(source.values().skip(from))
                {
                    deliver(ctx, slot.id, TagValue::tagged(value, tag));
                }
                self.current = source;
                // A resize or a source back from bottom changes the result
                // whether or not a slot fires, and so do moved elements
                // under a result that reads them.
                let back = std::mem::take(&mut self.src_bottom);
                // CR claude for claude: [bug] MapQ and FoldQ each carry a copy of the
                // source/resize/deliver prologue (1018-1059, 1420-1456), and the firing
                // rules after it have drifted from each other and from the JIT's exact
                // SlotFlags rule, which fires on any resize and treats a source back
                // from bottom as one. Here MapQ merges the source's tag, so a resize or
                // a return that the source delivers STALE (an arm waking after another
                // arm consumed the source's fire) does not fire, and the empty-source
                // return at 1069 takes the source's tag alone; FoldQ fires on a resize
                // but needs src_trig for a return (1522). graphix-fuzz check reports
                // DIVERGENCE (interp 4:0, jit 4:4) for map on a shrink and for map and
                // fold on a return; probe:
                // design/review-2026-10-05/repro/c-collection-03.gx. One shared
                // prologue with one rule (fire iff resized, back from bottom, a slot
                // fired, or the source fired empty) closes it; typecheck*, delete,
                // sleep, image and emit_clif_call are pairwise copies too.
                // (c-collection-03)
                if resized || back || (self.base.op.reads_elements() && moved) {
                    production = merge_tag(production, tag);
                }
                if self.slots.is_empty() {
                    let v = self.finish(ctx);
                    return self.resident.set(TagValue::tagged(v, tag));
                }
            }
        }
        let saved_init = ctx.event.init;
        let slots = update_slots(ctx, &mut self.slots, &mut self.fork, old_len);
        ctx.event.init = saved_init;
        match slots {
            None => return self.resident.ride(),
            Some(Some(tag)) => production = merge_tag(production, tag),
            Some(None) => (),
        }
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
            let v = self.finish(ctx);
            self.resident.set(TagValue::tagged(v, tag))
        } else {
            self.resident.set_bottom(tag.triggers())
        }
    }

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        let Self { base, slots, .. } = self;
        base.source.delete(ctx);
        base.prototype.delete(ctx);
        ctx.rt.store_remove(&base.prototype_id);
        ctx.env.unbind_variable(base.prototype_id);
        for slot in slots.iter_mut() {
            slot.delete(ctx);
        }
    }

    fn typecheck0(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
        wrap!(self.base.source, self.base.source.typecheck0(ctx))?;
        wrap!(self.base.prototype, self.base.prototype.typecheck0(ctx))
    }

    fn typecheck0_instance(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        types: &mut super::lambda::InstanceTypes,
    ) -> Result<()> {
        wrap!(self.base.source, self.base.source.typecheck0_instance(ctx, types))?;
        wrap!(self.base.prototype, self.base.prototype.typecheck0_instance(ctx, types))
    }

    fn typecheck1(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
        wrap!(self.base.source, self.base.source.typecheck1(ctx))?;
        wrap!(self.base.prototype, self.base.prototype.typecheck1(ctx))
    }

    fn typ(&self) -> &Type {
        &self.base.typ
    }

    // CR claude for claude: [bug] MapQ::refs, and FoldQ::refs at line 1571, report the
    // source and the prototype but not the slots, whose call sites hold the bound
    // instances. When the callback is chosen at run time, or the callback calls a
    // function chosen at run time, the prototype never binds. What the slot instances
    // read is then missing from the refs that Select and the seq machine re-collect at
    // each deselect, and from the refs Bind::input_fired reads at a wake. As a result,
    // a fire that lands while the arm sleeps is not re-raised at the wake, and a live
    // fire in the wake cycle is held back from a let that a connect writes; `[g(n)]` in
    // place of `array::map([n], g)` handles both correctly. Probe:
    // design/review-2026-10-05/repro/x-node-contract-04.gx (graphix-fuzz run gives
    // Trace([]), expected 9:[i64:9]). Walking each slot's call with its arg ids bound
    // (acc_id and element_id in FoldQ) would close it; slots are empty at compile time,
    // so compile-time users see no change. (x-node-contract-04)
    fn refs(&self, refs: &mut Refs) {
        self.base.source.refs(refs);
        refs.bound.insert(self.base.prototype_id);
        self.base.prototype.refs(refs);
    }

    fn spec(&self) -> &Expr {
        &self.base.spec
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.slept.set();
        // Slot values survive sleep: sleep is pause.
        self.base.source.sleep(ctx);
        for slot in self.slots.iter_mut() {
            slot.call.sleep(ctx);
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
        ctx: &mut CompileCtx<R, E>,
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

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
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
    build: ProbeSite,
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
            build: ProbeSite::default(),
        })
    }

    fn new(
        intrinsic: CollectionIntrinsic,
        ctx: &mut CompileCtx<R, E>,
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
        let callback = Callback {
            scope: scope.clone(),
            id,
            typ: callback_type,
            top_id,
            share: None,
        };
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
        ctx: &mut ExecCtx<'_, R, E>,
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
        let callback = Callback::image_decode(ctx, buf)?;
        let acc_type = Type::decode(buf)?;
        Ok(Self::with(base, callback, acc_type))
    }
}

/// Deliver `tv` to a callback argument this cycle and stand it in the
/// store, a bottom as a stale bottom.
fn deliver<R: Rt, E: UserEvent>(ctx: &mut ExecCtx<'_, R, E>, id: BindId, tv: TagValue) {
    let standing = if tv.tag().is_bottom() {
        TagValue::tagged(Value::Null, Tag::STALE_BOTTOM)
    } else {
        tv.clone()
    };
    ctx.rt.store_insert(id, standing);
    ctx.event.variables.insert(id, tv);
}

impl<R: Rt, E: UserEvent, C: MapCollection> Update<R, E> for FoldQ<R, E, C> {
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

    fn fuse(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<Option<Node<R, E>>> {
        fuse_callback(ctx, &mut self.base.prototype, &mut self.callback)
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R, E>) -> &TagValue {
        let woke = self.slept.take();
        let old_len = self.slots.len();
        let (tag, sval) = {
            let tv = self.base.source.update(ctx);
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
                let fresh = old_len.min(self.slots.len());
                build_fresh(ctx, &mut self.slots[fresh..], &mut self.build, |s| {
                    &mut s.call
                });
                // Elements move only on a fire, in a frame or past a sleep; a
                // fresh slot always takes its element.
                let moved = src_trig || woke;
                let from = if moved { 0 } else { old_len.min(self.slots.len()) };
                for (slot, value) in
                    self.slots[from..].iter().zip(source.values().skip(from))
                {
                    deliver(ctx, slot.element_id, TagValue::tagged(value, tag));
                }
            }
        }
        // A bottom init is a poisoned delivery to slot 0's acc, not a
        // whole-fold abort: a callback that never consumes the acc
        // recovers.
        let init = self.base.init.update(ctx).clone();
        if let Some(slot) = self.slots.first() {
            deliver(ctx, slot.acc_id, init.clone());
        }
        if self.slots.is_empty() && source_ok {
            return match init.tag() {
                // CR claude for claude: [bug] When the init is bottom, a fired empty
                // source does not fire the fold. This arm takes its trigger from the
                // init alone. The value arm below joins in the source's tag
                // (`tag.join(t)`), and the kernel fires on a fired empty source
                // (fusion/emit/scaffold.rs:602-604). So the node-walk returns a stale
                // bottom where the JIT returns a fresh one. A fold whose callback
                // ignores its acc misses the fire, and a `<-` target initialized by
                // such a fold keeps same-cycle writes that the JIT re-publishes over
                // (both engines do that with a valid init).
                // `set_bottom(tag.join(t).triggers())` makes the two arms agree. probe:
                // design/review-2026-10-05/repro/f-scaffold-body-05.gx
                // (f-scaffold-body-05)
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
        let saved_init = ctx.event.init;
        for i in 0..self.slots.len() {
            if ctx.interrupted() {
                ctx.event.init = saved_init;
                return self.resident.ride();
            }
            // A fresh slot's first dispatch runs under a forced init view,
            // its acc seeded with the chain's state as it stands.
            if i >= old_len {
                ctx.event.init = true;
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
                    deliver(ctx, self.slots[i].acc_id, seed);
                }
            }
            let slot = &mut self.slots[i];
            let tv = slot.call.update(ctx).clone();
            any_trig |= tv.tag().triggers();
            slot.state.set(&tv);
            // The production, a bottom included, travels the acc chain.
            if let Some(next) = self.slots.get(i + 1) {
                deliver(ctx, next.acc_id, tv);
            }
        }
        ctx.event.init = saved_init;
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

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
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

    fn typecheck0(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
        wrap!(self.base.source, self.base.source.typecheck0(ctx))?;
        wrap!(self.base.init, self.base.init.typecheck0(ctx))?;
        wrap!(self.base.prototype, self.base.prototype.typecheck0(ctx))
    }

    fn typecheck0_instance(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        types: &mut super::lambda::InstanceTypes,
    ) -> Result<()> {
        wrap!(self.base.source, self.base.source.typecheck0_instance(ctx, types))?;
        wrap!(self.base.init, self.base.init.typecheck0_instance(ctx, types))?;
        wrap!(self.base.prototype, self.base.prototype.typecheck0_instance(ctx, types))
    }

    fn typecheck1(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
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

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        // The slot states survive sleep: sleep is pause.
        self.slept.set();
        self.base.source.sleep(ctx);
        self.base.init.sleep(ctx);
        for slot in self.slots.iter_mut() {
            slot.call.sleep(ctx);
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

/// Fuse the prototype's instance of a statically resolved callback,
/// whose kernels the slots' instances share.
fn fuse_callback<R: Rt, E: UserEvent>(
    ctx: &mut CompileCtx<R, E>,
    prototype: &mut Node<R, E>,
    callback: &mut Callback,
) -> Result<Option<Node<R, E>>> {
    let Some(site) = prototype.downcast_mut::<CallSite<R, E>>() else { return Ok(None) };
    if site.static_target.is_none() {
        return Ok(None);
    }
    let Some(apply) = site.callee.apply_mut() else { return Ok(None) };
    if matches!(apply.view(), ApplyView::Lambda(_)) {
        callback.share = share::fuse_prototype(ctx, |ctx| apply.fuse(ctx))?;
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
        // XCR claude for eric: [bug] flat_map chooses between splicing and pushing by
        // looking at the value, but its callback may return a bare 'b (`['b,
        // Array<'b>]`, `['b, List<'b>]`). A tuple, struct, payload variant or list 'b
        // is an array at run time, and any empty or 2-slot array passes list::is_list.
        // So `array::flat_map([1, 2], |x| (x, x * 10))` checks as Array<(i64, i64)> but
        // is [1, 10, 2, 20] in both engines, `list::flat_map` with the same callback
        // drops every second component, and code that reads the result by its type then
        // diverges (the node-walk bottoms, kernels read 0).
        // graphix_value_buf_extend_from_list in fusion/emit_helpers.rs makes the same
        // value test. The splice has to follow the callback's resolved return type, or
        // the signatures become `-> Array<'b>` / `-> List<'b>` like
        // Collection::flat_map's (lang::functions::flat_map_declared_union pins the
        // bare form). probe: design/review-2026-10-05/repro/c-collection-01.gx
        // (c-collection-01)
        // 2026-10-06 claude: Eric ruled for the signature change: array::flat_map's
        // callback is `fn(x: 'a) -> Array<'b>` and list::flat_map's `fn(x: 'a) ->
        // List<'b>`, so the result is always spliced and nothing is decided by the
        // value's shape. The push arm is gone. The probe is now refused (its callback
        // returns a tuple).
        match (self, v) {
            (Self::Array, Value::Array(a)) => elems.extend(a.iter().cloned()),
            (Self::List, v) => elems.extend(list::Iter::new(v.clone())),
            (Self::Array | Self::CMap, _) => (),
        }
    }

    /// Emit the loop source as the scaffold's ValArray. Returns the
    /// source's (disc, payload), whose disc drives the firing wrap,
    /// plus the loop's [`scaffold::ArraySrc`].
    // CR claude for claude: [structure] The fused-loop emission (514-648 and 1649-2072,
    // about 550 lines: CallbackParam, the emit_*_kind gates,
    // Flavor::emit_source/emit_result, emit_flattened_source) is the only cranelift
    // code under node/; other nodes' emit_clif delegate to fusion/emit. Moving it
    // beside scaffold.rs leaves this file the node-walk semantics. emit_source also
    // returns a CompiledExpr whose payload the List/Map flatten helper has already
    // consumed, kept only for the disc the ArraySrc carries: return the ArraySrc alone
    // and call flags.apply directly in place of finish_loop_result, a one-line wrapper.
    // (c-collection-06)
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
    let (result, flags) = emit(cx, src)?;
    Ok(Some(finish_loop_result(cx, result, flags, &value)))
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
    let output_source = emit::node_composite_source(body);
    let sites = emit::slot_state_sites(cx, body);
    let (ptr, flags, count_disc) = scaffold::emit_init_loop(
        cx,
        count.payload,
        count.disc,
        count_prim,
        &param.name,
        param.id,
        &output_type,
        output_source,
        &sites,
        |cx| body.emit_clif(cx),
    )?;
    let result = flavor.emit_result(cx, ptr)?;
    // The firing wrap must see an over-limit count as a tainted source.
    let count = CompiledExpr::new(count_disc, count.payload);
    Ok(Some(finish_loop_result(cx, result, flags, &count)))
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
        let sites = emit::slot_state_sites(cx, body);
        let (ptr, flags) = scaffold::emit_map_loop(
            cx,
            src,
            &param.elem(&element_type, &leaves),
            &output_type,
            output_source,
            &sites,
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
        let sites = emit::slot_state_sites(cx, body);
        let (ptr, flags) = scaffold::emit_filter_loop(
            cx,
            src,
            &param.elem(&element_type, &leaves),
            &sites,
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
        let sites = emit::slot_state_sites(cx, body);
        let (ptr, flags) = scaffold::emit_filter_map_loop(
            cx,
            src,
            &param.elem(&element_type, &leaves),
            &output_element,
            output_source,
            &sites,
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
        let sites = emit::slot_state_sites(cx, body);
        let (ptr, flags) = scaffold::emit_flat_map_loop(
            cx,
            src,
            &param.elem(&element_type, &leaves),
            extend,
            &sites,
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
        let sites = emit::slot_state_sites(cx, body);
        let ((disc, payload), flags) = scaffold::emit_find_loop(
            cx,
            src,
            &param.elem(&element_type, &leaves),
            &sites,
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
        let sites = emit::slot_state_sites(cx, body);
        let ((disc, payload), flags) = scaffold::emit_find_map_loop(
            cx,
            src,
            &param.elem(&element_type, &leaves),
            &sites,
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
        Some(AbiKind::Variant | AbiKind::Nullable | AbiKind::Value)
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
        let sites = emit::slot_state_sites(cx, body);
        scaffold::emit_fold_loop(
            cx,
            src,
            acc_shape,
            &acc.name,
            acc.id,
            &element.elem(&element_type, &element_leaves),
            &sites,
            |cx| operand(cx, init),
            |cx| operand(cx, body),
        )
    })
}
