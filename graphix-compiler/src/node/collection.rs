use super::{
    NOP, WakeBit,
    callsite::{CallNode, CallSite},
    coretraits::with_hooks,
    genn,
    lambda::GXLambda,
    list,
    pattern::StructPatternNode,
};
use crate::cost::{ProbeSite, SlotPlan, SlotSite};
use crate::{
    ApplyView, BindId, CompileCtx, ExecCtx, Node, NodeView, Refs, Rt, Scope, Tag,
    TagValue, Update, UserEvent, View,
    dbgenv::gxdbg_slot,
    expr::{Expr, ExprId},
    fusion::{
        emit::{
            self, BodyCx, CompiledExpr,
            loops::{
                FoldParts, callback_param, emit_filter_kind, emit_filter_map_kind,
                emit_find_kind, emit_find_map_kind, emit_flat_map_kind, emit_fold_kind,
                emit_init_kind, emit_map_kind,
            },
        },
        share::{self, SlotShare},
    },
    image::{
        self, ImageBuf,
        nodes::{NodeTag, decode_node, put_tag},
        scope_decode, scope_encode,
    },
    typ::{FnType, Type},
    wrap,
};
use anyhow::{Result, anyhow, bail};
use arcstr::literal;
use bytes::BufMut;
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
/// A slot's last production. A fresh slot is bottom until its first
/// run, which precedes every read of it.
enum SlotState {
    Value(Value),
    #[default]
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
            Self::Bottom => None,
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
                if let Some(cs) =
                    call.downcast_mut::<CallNode<R, E>>().and_then(CallNode::call_mut)
                {
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
    /// statically, else it calls through the callback parameter.
    Slot(Option<Value>),
}

impl CallKind {
    /// The slot call a prototype settled on.
    fn slot<R: Rt, E: UserEvent>(
        ctx: &ExecCtx<'_, R, E>,
        prototype: &Node<R, E>,
    ) -> Self {
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

/// Run a check pass `f` over `nodes` in order, each error in its node's
/// context.
fn each<R: Rt, E: UserEvent, const N: usize>(
    nodes: [&mut Node<R, E>; N],
    mut f: impl FnMut(&mut Node<R, E>) -> Result<()>,
) -> Result<()> {
    nodes.into_iter().try_for_each(|n| {
        let r = f(n);
        wrap!(n, r)
    })
}

/// A collection's source as one update read it.
struct Sourced<C> {
    tag: Tag,
    /// `None` while the source is bottom or not a collection.
    source: Option<C>,
    /// The slot count changed.
    resized: bool,
    /// The source returned from bottom.
    back: bool,
    /// Its elements moved: it fired, or the collection woke.
    moved: bool,
}

/// What a collection's slots are to [`take_source`].
struct SlotKind<R: Rt, E: UserEvent, S> {
    delete: fn(&mut S, &mut ExecCtx<'_, R, E>),
    call: fn(&mut S) -> &mut Node<R, E>,
    element: fn(&S) -> BindId,
}

/// Update a collection's source and, when it holds a collection, resize
/// the slots to it (`make` builds a slot of the call kind the prototype
/// settled on), build the fresh slots' instances where `build` says, and
/// deliver each slot its element: a fresh slot always, any slot when the
/// elements moved. A bottom source forgets the length; the slots stay.
#[allow(clippy::too_many_arguments)]
fn take_source<R: Rt, E: UserEvent, C: MapCollection, S: Send>(
    ctx: &mut ExecCtx<'_, R, E>,
    source: &mut Node<R, E>,
    prototype: &Node<R, E>,
    slots: &mut Vec<S>,
    src_bottom: &mut bool,
    woke: bool,
    build: &mut ProbeSite,
    kind: SlotKind<R, E, S>,
    mut make: impl FnMut(&mut ExecCtx<'_, R, E>, CallKind) -> S,
) -> Sourced<C> {
    let old_len = slots.len();
    let (tag, sval) = {
        let tv = source.update(ctx);
        let tag = tv.tag();
        (tag, if tag.is_bottom() { None } else { Some(tv.value_cloned()) })
    };
    let moved = tag.triggers() || woke;
    let source = sval.and_then(|value| C::select(value, tag.triggers()));
    let Some(source) = source else {
        *src_bottom = true;
        return Sourced { tag, source: None, resized: false, back: false, moved };
    };
    let back = std::mem::take(src_bottom);
    // what a slot calls, decided once a slot is added
    let mut call_kind = None;
    let resized = resize(ctx, slots, source.len(), kind.delete, |ctx| {
        let k = call_kind.get_or_insert_with(|| CallKind::slot(ctx, prototype)).clone();
        make(ctx, k)
    });
    let fresh = old_len.min(slots.len());
    build_fresh(ctx, &mut slots[fresh..], build, kind.call);
    let from = if moved { 0 } else { fresh };
    for (slot, value) in slots[from..].iter().zip(source.values().skip(from)) {
        deliver(ctx, (kind.element)(slot), TagValue::tagged(value, tag));
    }
    Sourced { tag, source: Some(source), resized, back, moved }
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
        Self { id, call, state: SlotState::Bottom }
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
    /// The source and the prototype, in check order.
    fn nodes_mut(&mut self) -> [&mut Node<R, E>; 2] {
        [&mut self.source, &mut self.prototype]
    }

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
    site.decide_siblings(ctx, slots.len(), || {
        crate::analysis::independent(slots.iter().take(2).map(|s| &s.call), ctx)
    });
    if site.dependent() {
        return update_slots_in_order(ctx, slots, 0, old_len);
    }
    let (standing, fresh) = slots.split_at_mut(old_len.min(slots.len()));
    let n = standing.len();
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
fn build_fresh<R: Rt, E: UserEvent, S: Send>(
    ctx: &mut ExecCtx<'_, R, E>,
    fresh: &mut [S],
    site: &mut ProbeSite,
    call: fn(&mut S) -> &mut Node<R, E>,
) {
    if fresh.is_empty() {
        return;
    }
    let prebind = |ctx: &mut CompileCtx<R, E>, slots: &mut [S]| {
        for slot in slots {
            if let Some(cs) =
                call(slot).downcast_mut::<CallNode<R, E>>().and_then(CallNode::call_mut)
            {
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
        let view = if i >= old_len { View::Birth } else { View::Cycle };
        let tv = ctx.under(view, |ctx| slot.call.update(ctx));
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
        let (callback, element_type) = (&self.callback, &self.base.element_type);
        let Sourced { tag, source, resized, back, moved } = take_source::<_, _, C, _>(
            ctx,
            &mut self.base.source,
            &self.base.prototype,
            &mut self.slots,
            &mut self.src_bottom,
            woke,
            &mut self.fork.build,
            SlotKind { delete: Slot::delete, call: |s| &mut s.call, element: |s| s.id },
            |ctx, kind| Slot::new(ctx, callback, element_type, kind),
        );
        let source_ok = source.is_some();
        if let Some(source) = source {
            self.current = source;
            // a resize or a return is a new result whatever the source's
            // tag, a wake's own when the source's is; so are moved
            // elements under a result that reads them
            let tag = if resized || back { tag.as_fire() } else { tag };
            if resized || back || (self.base.op.reads_elements() && moved) {
                production = merge_tag(production, tag);
            }
            if self.slots.is_empty() {
                let v = self.finish(ctx);
                return self.resident.set(TagValue::tagged(v, tag));
            }
        }
        let slots = update_slots(ctx, &mut self.slots, &mut self.fork, old_len);
        match slots {
            None => return self.resident.ride(),
            Some(Some(tag)) => production = merge_tag(production, tag),
            Some(None) => (),
        }
        if !source_ok {
            return self.resident.set_bottom_as(tag);
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
            None if woke => Tag::STALE,
            None => return self.resident.ride(),
        };
        if tag.is_bottom() || poisoned {
            return self.resident.set_bottom_as(tag);
        }
        let v = self.finish(ctx);
        self.resident.set(TagValue::tagged(v, tag))
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
        each(self.base.nodes_mut(), |n| n.typecheck0(ctx))
    }

    fn typecheck0_instance(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        types: &mut super::lambda::InstanceTypes,
    ) -> Result<()> {
        each(self.base.nodes_mut(), |n| n.typecheck0_instance(ctx, types))
    }

    fn typecheck1(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
        each(self.base.nodes_mut(), |n| n.typecheck1(ctx))
    }

    fn typ(&self) -> &Type {
        &self.base.typ
    }

    /// The source, the prototype and every slot's call, whose instance
    /// a run-time callback binds: what it reads is the collection's.
    fn refs(&self, refs: &mut Refs) {
        self.base.source.refs(refs);
        refs.bound.insert(self.base.prototype_id);
        self.base.prototype.refs(refs);
        for slot in self.slots.iter() {
            refs.bound.insert(slot.id);
            slot.call.refs(refs);
        }
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
        Self { acc_id, element_id, call, state: SlotState::Bottom }
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
    /// The source, the init and the prototype, in check order.
    fn nodes_mut(&mut self) -> [&mut Node<R, E>; 3] {
        [&mut self.source, &mut self.init, &mut self.prototype]
    }

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
        let (callback, acc, elt) =
            (&self.callback, &self.acc_type, &self.base.element_type);
        let Sourced { tag, source, resized, back, moved: _ } = take_source::<_, _, C, _>(
            ctx,
            &mut self.base.source,
            &self.base.prototype,
            &mut self.slots,
            &mut self.src_bottom,
            woke,
            &mut self.build,
            SlotKind {
                delete: FoldSlot::delete,
                call: |s| &mut s.call,
                element: |s| s.element_id,
            },
            |ctx, kind| FoldSlot::new(ctx, callback, acc, elt, kind),
        );
        let source_ok = source.is_some();
        // A bottom init is a poisoned delivery to slot 0's acc, not a
        // whole-fold abort: a callback that never consumes the acc
        // recovers.
        let init = self.base.init.update(ctx).clone();
        if let Some(slot) = self.slots.first() {
            deliver(ctx, slot.acc_id, init.clone());
        }
        if self.slots.is_empty() && source_ok {
            // a resize or a return is a new result whatever the source's
            // tag, a wake's own when the source's is
            let tag = if resized || back { tag.as_fire() } else { tag };
            return match init.tag() {
                t if t.is_bottom() => self.resident.set_bottom(tag.join(t).triggers()),
                t => {
                    self.resident.set(TagValue::tagged(init.value_cloned(), tag.join(t)))
                }
            };
        }
        // Only slot productions seed the firing decision: a source or
        // init delivery reaches the result only through a slot that
        // consumes it, past a resize or a return. A triggering taint still
        // counts, for the bottom arm.
        let mut prod: Option<Tag> = None;
        if !source_ok {
            prod = merge_tag(prod, tag);
        } else if resized || back {
            prod = merge_tag(prod, tag.as_fire());
        }
        for i in 0..self.slots.len() {
            if ctx.interrupted() {
                return self.resident.ride();
            }
            // A fresh slot's first dispatch runs under the birth view, its
            // acc seeded with the chain's state as it stands.
            let view = if i >= old_len { View::Birth } else { View::Cycle };
            if i >= old_len {
                let seed = match i {
                    0 if init.tag().is_bottom() => {
                        TagValue::tagged(Value::Null, Tag::FRESH_BOTTOM)
                    }
                    0 => TagValue::fired(init.value_cloned()),
                    _ => match &self.slots[i - 1].state {
                        SlotState::Bottom => {
                            TagValue::tagged(Value::Null, Tag::FRESH_BOTTOM)
                        }
                        SlotState::Value(v) => TagValue::fired(v.clone()),
                    },
                };
                deliver(ctx, self.slots[i].acc_id, seed);
            }
            let slot = &mut self.slots[i];
            let tv = ctx.under(view, |ctx| slot.call.update(ctx).clone());
            prod = merge_tag(prod, tv.tag());
            slot.state.set(&tv);
            // The production, a bottom included, travels the acc chain.
            if let Some(next) = self.slots.get(i + 1) {
                deliver(ctx, next.acc_id, tv);
            }
        }
        // An interior slot's poison bottoms the fold only if a downstream
        // callback consumes it; only the last slot's state is the result.
        let prod = prod.unwrap_or(Tag::STALE);
        match self.slots.last().map(|s| &s.state) {
            _ if !source_ok => self.resident.set_bottom_as(prod),
            Some(SlotState::Bottom) => self.resident.set_bottom_as(prod),
            // A fold fires iff it resized, a slot fired, or the source came
            // back from bottom: a wake's own fire when each of those was.
            Some(SlotState::Value(v)) => {
                let tag = if prod.triggers() { prod.fresh_or_wake() } else { Tag::STALE };
                let v = v.clone();
                self.resident.set(TagValue::tagged(v, tag))
            }
            None => self.resident.ride(),
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
        each(self.base.nodes_mut(), |n| n.typecheck0(ctx))
    }

    fn typecheck0_instance(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        types: &mut super::lambda::InstanceTypes,
    ) -> Result<()> {
        each(self.base.nodes_mut(), |n| n.typecheck0_instance(ctx, types))
    }

    fn typecheck1(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
        each(self.base.nodes_mut(), |n| n.typecheck1(ctx))
    }

    fn typ(&self) -> &Type {
        &self.base.typ
    }

    /// The source, the init, the prototype and every slot's call
    /// ([`MapQ::refs`]).
    fn refs(&self, refs: &mut Refs) {
        self.base.source.refs(refs);
        self.base.init.refs(refs);
        refs.bound.extend(self.base.prototype_ids);
        self.base.prototype.refs(refs);
        for slot in self.slots.iter() {
            refs.bound.extend([slot.acc_id, slot.element_id]);
            slot.call.refs(refs);
        }
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
    let Some(site) =
        prototype.downcast_mut::<CallNode<R, E>>().and_then(CallNode::call_mut)
    else {
        return Ok(None);
    };
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
    pub(crate) fn extend(self, elems: &mut LPooled<Vec<Value>>, v: &Value) {
        match (self, v) {
            (Self::Array, Value::Array(a)) => elems.extend(a.iter().cloned()),
            (Self::List, v) => elems.extend(list::Iter::new(v.clone())),
            (Self::Array | Self::CMap, _) => (),
        }
    }
}
