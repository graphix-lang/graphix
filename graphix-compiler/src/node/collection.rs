use super::{
    NOP, WakeBit,
    bind::Ref,
    callsite::{CallNode, CallSite},
    coretraits::with_hooks,
    genn,
    lambda::GXLambda,
    list,
    pattern::StructPatternNode,
    wake::TrackedFires,
};
use crate::cost::{ProbeSite, SlotPlan, SlotSite};
use crate::{
    ApplyView, BindId, CompileCtx, ExecCtx, Node, NodeView, Refs, Rt, Scope,
    SourcePosition, Tag, TagValue, Update, UserEvent, View,
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
        spec: Arc<Expr>,
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
    /// The elements `keep` keeps, as this collection is under `flavor`.
    fn kept(&self, flavor: Flavor, keep: impl Iterator<Item = bool>) -> Value {
        let kept = self.values().zip(keep).filter_map(|(v, k)| k.then_some(v));
        flavor.build(&mut kept.collect())
    }
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
/// as the tuple `(k, v)`. Every encoder and decoder, interpreted or JIT,
/// goes through `make_pair` / [`split_pair`].
pub(crate) fn make_pair(key: &Value, value: &Value) -> Value {
    crate::rep::tuple([key.clone(), value.clone()])
}

/// See [`make_pair`].
pub(crate) fn split_pair(value: &Value) -> Option<(Value, Value)> {
    match crate::rep::tuple_elems(value)? {
        [k, v] => Some((k.clone(), v.clone())),
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

    /// The kept entries are the source's, already a map's: no pairs.
    fn kept(&self, _: Flavor, keep: impl Iterator<Item = bool>) -> Value {
        let kept = self.into_iter().zip(keep).filter(|(_, k)| *k);
        Value::Map(CMap::from_iter(kept.map(|((k, v), _)| (k.clone(), v.clone()))))
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
        target: crate::FAILURE_TARGET,
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

/// A slot's last production. A fresh slot is bottom until its first
/// run, which precedes every read of it.
#[derive(Debug, Default)]
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
    /// The argument the callback was at the call, once a check saw it.
    arg: Option<Arc<Expr>>,
}

impl Callback {
    /// The source's bind and the callback of an intrinsic whose formals
    /// are `args`: the source first, the callback last, taking `arity`
    /// arguments.
    fn of_args(
        scope: &Scope,
        top_id: ExprId,
        typ: &FnType,
        args: &[StructPatternNode],
        arity: usize,
    ) -> Result<(BindId, Self)> {
        let name = |p: &StructPatternNode, what: &str| {
            p.single_bind_id()
                .ok_or_else(|| anyhow!("collection {what} argument must be a name"))
        };
        let (Some(source), Some(callback)) = (args.first(), args.last()) else {
            bail!("a collection intrinsic takes a source and a callback")
        };
        let typ = match typ.args.last().map(|a| &a.typ) {
            Some(Type::Fn(ft)) => ft.clone(),
            Some(t) => bail!("collection callback must be a function, got {t}"),
            None => bail!("a collection intrinsic takes a callback"),
        };
        if typ.args.len() != arity {
            let s = if arity == 1 { "" } else { "s" };
            bail!(
                "this collection's callback takes {arity} argument{s}, but its type {typ} takes {}",
                typ.args.len()
            )
        }
        let id = name(callback, "callback")?;
        let cb = Self { scope: scope.clone(), id, typ, top_id, share: None, arg: None };
        Ok((name(source, "source")?, cb))
    }

    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        scope_encode(&self.scope, buf)?;
        self.id.encode(buf)?;
        self.typ.encode(buf)?;
        self.top_id.encode(buf)?;
        match &self.share {
            None => buf.put_u8(0),
            Some(share) => {
                buf.put_u8(1);
                share.image_encode(buf)?
            }
        }
        self.arg.as_deref().cloned().encode(buf)
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
        let arg = Option::<Expr>::decode(buf)?.map(Arc::new);
        Ok(Self { scope, id, typ, top_id, share, arg })
    }

    fn see_arg<R: Rt, E: UserEvent>(&mut self, ctx: &CompileCtx<R, E>) {
        if let Some(arg) = ctx.fn_param_args.get(&self.id) {
            self.arg = Some(arg.clone());
        }
    }

    /// A slot's argument: a variable only the slot's call reads, so no
    /// name in the environment.
    // XCR claude for claude: [bug/low] The slot's synthetic argument is positioned
    // at SourcePosition::default() with the SYNTHETIC origin. `array::map([1, 2,
    // 3], dbg)` therefore still prints `line: 1, column: 1 dbg(collection_element):
    // 1`, a place in no file, while `|x| dbg(x)` prints its own site. Give the
    // argument the position and origin of what this instance's callback parameter
    // was bound to at the call: the user's `dbg` argument, or at least the
    // parameter binding that Callback::spec already looks up. probe:
    // design/review-2026-10-19/repro/x-rerun-6-builtin-callback-position.gx
    // (x-rerun-6-03)
    // 2026-10-19 claude: Tried positioning the slot argument at the callback
    // formal's binding (env.by_id(self.id)): for array::map that is the stdlib
    // formal, which has no position either (it printed line 0, column 0). The
    // user's argument spec at the call is what is needed, and Callback does not
    // reach it; threading the call site's argument spec into the instance (or into
    // Callback at its build) is the remaining work.
    // 2026-10-20 claude: A call that binds an instance records each fn-typed
    // argument spec by formal (CompileCtx::fn_param_args, beside
    // instance_fn_params, while the instance checks); a collection callback takes
    // it at typecheck1 (Callback::see_arg) and its slots are positioned there; the
    // argument is imaged with the callback (REGISTRATION_FORMAT 77). Pin:
    // lang::printing::dbg_as_a_callback_reports_its_argument (every mode).
    fn slot_arg<R: Rt, E: UserEvent>(
        &self,
        ctx: &mut CompileCtx<R, E>,
        name: &str,
        typ: &Type,
    ) -> (BindId, Node<R, E>) {
        let id = BindId::new();
        let spec = match &self.arg {
            Some(arg) => genn::name_spec(name, arg.pos, arg.ori.clone()),
            None => {
                genn::name_spec(name, SourcePosition::default(), genn::SYNTHETIC.clone())
            }
        };
        (id, Ref::new(ctx, id, typ.clone(), self.top_id, spec))
    }

    /// The callback parameter as written, the source a slot's call of a
    /// resolved definition reports.
    fn spec<R: Rt, E: UserEvent>(&self, ctx: &CompileCtx<R, E>) -> Arc<Expr> {
        match ctx.env.by_id.get(&self.id) {
            Some(b) => Arc::new(genn::name_spec(&b.name, b.pos, b.ori.clone())),
            None => NOP.clone(),
        }
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
        let (Self { scope, id, typ, top_id, share, arg: _ }, fty) =
            (self, Type::Fn(self.typ.clone()));
        let call = match kind {
            CallKind::Prototype => {
                let function = genn::reference(ctx, *id, fty, *top_id);
                genn::apply_prototype(function, scope.clone(), args, typ, *top_id)
            }
            CallKind::Slot(Some(def)) => {
                let spec = self.spec(ctx);
                genn::apply_constant(def, spec, scope.clone(), args, typ, *top_id).map(
                    |mut call| {
                        if let Some(cs) = call
                            .downcast_mut::<CallNode<R, E>>()
                            .and_then(CallNode::call_mut)
                        {
                            cs.share = share.clone();
                        }
                        call
                    },
                )
            }
            CallKind::Slot(None) => {
                let function = genn::reference(ctx, *id, fty, *top_id);
                genn::apply(function, scope.clone(), args, typ, *top_id)
            }
        };
        call.expect("of_args held the callback to the intrinsic's arity")
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
    /// The slots it paused, which wake with what they missed.
    paused: Option<TrackedFires>,
    /// Its elements moved: it fired, or the collection woke.
    moved: bool,
}

/// The source as the slots last saw it.
#[derive(Debug, Default)]
enum SourceSeen {
    /// Never read, or bottom with no slot: its length is forgotten, so
    /// its return changes the result.
    #[default]
    Absent,
    /// Bottom while slots stood: they are paused, and this tracks the
    /// fires of what they read meanwhile.
    Paused(TrackedFires),
    Present,
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
/// elements moved. A bottom source forgets the length; the slots stay,
/// paused when it turns bottom (`kind.call`'s sleep, as a deselected arm's)
/// and woken, caught up, when it returns ([`SourceSeen::Paused`]).
#[allow(clippy::too_many_arguments)]
fn take_source<R: Rt, E: UserEvent, C: MapCollection, S: Send>(
    ctx: &mut ExecCtx<'_, R, E>,
    source: &mut Node<R, E>,
    prototype: &Node<R, E>,
    slots: &mut Vec<S>,
    seen: &mut SourceSeen,
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
        match seen {
            SourceSeen::Paused(missed) => missed.observe(ctx),
            _ if slots.is_empty() => *seen = SourceSeen::Absent,
            _ => {
                for slot in slots.iter_mut() {
                    (kind.call)(slot).sleep(ctx);
                }
                let mut missed = TrackedFires::new(&ctx.env, 1, |_, r| {
                    slots.iter_mut().for_each(|slot| (kind.call)(slot).refs(r))
                });
                missed.observe(ctx);
                *seen = SourceSeen::Paused(missed);
            }
        }
        return Sourced {
            tag,
            source: None,
            resized: false,
            back: false,
            paused: None,
            moved,
        };
    };
    let (back, paused) = match std::mem::replace(seen, SourceSeen::Present) {
        SourceSeen::Absent => (true, None),
        SourceSeen::Paused(missed) => (true, Some(missed)),
        SourceSeen::Present => (false, None),
    };
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
        ctx.deliver((kind.element)(slot), TagValue::tagged(value, tag));
    }
    Sourced { tag, source: Some(source), resized, back, paused, moved }
}

#[derive(Debug)]
struct Slot<R: Rt, E: UserEvent> {
    id: BindId,
    call: Node<R, E>,
    state: SlotState,
}

impl<R: Rt, E: UserEvent> Slot<R, E> {
    // CR claude for eric: [perf/medium] Every slot is a full run-time bind: an env
    // binding for the synthetic `collection_element` argument, a fresh instantiate of
    // the callback type, and at first dispatch build_bound (compile, typecheck0/1,
    // analysis::analyze_bound_callee and share::fuse_slot). The analysis gives the same
    // result for every sibling. Measured with `array::init(100000, |i| i)` under
    // --no-fusion: about 4.5 KB resident and about 40 us of setup per slot (debug
    // build). A 1M-element node-walk collection therefore needs gigabytes. probe:
    // design/review-2026-10-12/repro/c-collection-03.gx. Candidates: mint the element
    // BindId without an env binding nothing looks up by name; run analyze_bound_callee
    // once per prototype definition and copy its facts to sibling instances; reuse the
    // prototype's instance types instead of instantiating per slot. (c-collection-03)
    // 2026-10-16 claude: Done: apply_deferred and drop_deferred shrink pending_refs
    // after a batch (a drain walked the table's whole capacity, so the slots' first
    // large batch made every later bind quadratic: 300k slots 8.2 s, now 4.3 s on the
    // perf profile, linear from 50k to 400k at about 10 us a slot); a slot's arguments
    // are bare ids, no env binding (peak RSS at 100k 468 MB to 450 MB). Open:
    // analyze_bound_callee (about 5% of a slot) writes each instance's plans into its
    // own nodes, so running it once per prototype means copying node state; instantiate
    // per slot (about 10%) and the about 4 KB a slot keeps. Measured with
    // ~/tmp/target/perf/graphix --no-fusion over array::init(n, |i| i),
    // GRAPHIX_PAR=off, taskset -c 2.
    // 2026-10-17 claude: Profiled 10-17 (perf build, --no-fusion, array::init(100000,
    // |i| i), 2.4 s): no hot spot left. Self time spreads over the runtime's per-slot
    // variables (read_var, the store's and the event's maps, ref_var: about 15%), the
    // per-slot env copy and insert (about 10%), instantiation's cell work (about 5%)
    // and a long tail. What remains is the slot itself: each is a full node graph with
    // its own variables, so going further means slots sharing their prototype's graph,
    // the node-graph sharing question left open (not ruled out, not advocated).
    // Readdressed for that decision.
    fn new(
        ctx: &mut CompileCtx<R, E>,
        callback: &Callback,
        element_type: &Type,
        kind: CallKind,
    ) -> Self {
        let (id, element) = callback.slot_arg(ctx, "collection_element", element_type);
        let call = callback.call(ctx, smallvec![element], kind);
        Self { id, call, state: SlotState::Bottom }
    }

    /// The slot's value; `finish` runs only once every slot holds one.
    fn value(&self) -> &Value {
        self.state.value().expect("finish runs once every slot holds a value")
    }

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.call.delete(ctx);
        ctx.rt.store_release(&self.id);
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
    spec: Arc<Expr>,
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
        let r = self.emit(source, &callsite.spec, cx);
        cx.swap_collection_site(prev);
        r
    }

    fn emit(
        &self,
        source: &Node<R, E>,
        spec: &Expr,
        cx: &mut BodyCx,
    ) -> Result<Option<CompiledExpr>> {
        if emit::node_is_bottom(source) {
            return Ok(None);
        }
        let Some(callback) = callback(&self.prototype) else { return Ok(None) };
        let Some(param) = callback_param(callback, 0, literal!("__elem"), spec)? else {
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
            MapOp::Filter => return source.kept(self.flavor, slots.iter().map(taken)),
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
    seen: SourceSeen,
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
            seen: SourceSeen::Absent,
            resident: TagValue::phantom(),
            fork: SlotSite::default(),
        })
    }

    fn new(
        intrinsic: CollectionIntrinsic,
        ctx: &mut CompileCtx<R, E>,
        spec: Arc<Expr>,
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
        let (source_id, callback) = Callback::of_args(scope, top_id, typ, args, 1)?;
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
            spec: Arc::new(Expr::decode(buf)?),
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
/// first dispatch runs under a forced init view. The slots run in order
/// when `rebound`, the cycle their callback changes, and are decided again
/// once they are bound to it.
fn update_slots<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<'_, R, E>,
    slots: &mut [Slot<R, E>],
    site: &mut SlotSite,
    views: SlotViews,
    rebound: bool,
) -> Option<Option<Tag>> {
    if rebound {
        site.redecide();
        return update_slots_in_order(ctx, slots, 0, views);
    }
    site.decide_siblings(ctx, slots.len(), || {
        crate::analysis::independent(
            slots.iter().take(2).map(|s| &s.call),
            ctx,
            crate::analysis::Sites::Slots,
        )
    });
    if site.in_order() {
        return update_slots_in_order(ctx, slots, 0, views);
    }
    let (standing, fresh) = slots.split_at_mut(views.old_len.min(slots.len()));
    let n = standing.len();
    let production = match site.plan(ctx, n) {
        SlotPlan::Serial => update_slots_in_order(ctx, standing, 0, views)?,
        SlotPlan::Measure(t0) => {
            let r = update_slots_in_order(ctx, standing, 0, views)?;
            site.measured(t0, n);
            r
        }
        SlotPlan::Fork { grain } => update_ranges(ctx, standing, 0, views, grain)?,
    };
    site.fresh.run(
        ctx,
        fresh,
        n,
        production,
        |ctx, slot, at| update_slots_in_order(ctx, std::slice::from_mut(slot), at, views),
        |ctx, slots, at, grain| match grain {
            None => update_slots_in_order(ctx, slots, at, views),
            Some(grain) => update_ranges(ctx, slots, at, views, grain),
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

/// Run `update` over a collection's slots, `old_len` of them standing:
/// when the source `paused` them, they wake with one catch-up fire for
/// each input they read that fired meanwhile.
fn woken_slots<R: Rt, E: UserEvent, T>(
    ctx: &mut ExecCtx<'_, R, E>,
    old_len: usize,
    paused: Option<TrackedFires>,
    update: impl FnOnce(&mut ExecCtx<'_, R, E>, SlotViews) -> T,
) -> T {
    let Some(mut missed) = paused else {
        return update(ctx, SlotViews { old_len, standing: View::Cycle });
    };
    let caught_up = missed.deliver(ctx, 0);
    let r = update(ctx, SlotViews { old_len, standing: View::Wake });
    TrackedFires::restore(ctx.event, caught_up);
    r
}

/// The view each slot updates under: a fresh slot's birth, a standing
/// one's cycle, or its wake when the source came back from bottom.
#[derive(Clone, Copy)]
struct SlotViews {
    old_len: usize,
    standing: View,
}

impl SlotViews {
    fn fresh(self, i: usize) -> bool {
        i >= self.old_len
    }

    fn of(self, i: usize) -> View {
        if self.fresh(i) { View::Birth } else { self.standing }
    }
}

/// Update `slots`, the first at index `at`, in sibling branches of
/// `grain` slots each.
fn update_ranges<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<'_, R, E>,
    slots: &mut [Slot<R, E>],
    at: usize,
    views: SlotViews,
    grain: usize,
) -> Option<Option<Tag>> {
    let n = slots.len();
    let grain = grain.max(1);
    if n < 2 * grain {
        return update_slots_in_order(ctx, slots, at, views);
    }
    let ranges = ranges(n, grain);
    let parts = crate::branch::cut(slots, &ranges);
    let parts = parts.into_iter().zip(ranges.iter().map(|r| at + r.0));
    let mut production = None;
    for p in crate::branch::fork_each(ctx, parts, |c, (p, at)| {
        update_slots_in_order(c, p, at, views)
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
    views: SlotViews,
) -> Option<Option<Tag>> {
    let mut production = None;
    for (j, slot) in slots.iter_mut().enumerate() {
        let i = at + j;
        if ctx.interrupted() {
            return None;
        }
        let tv = ctx.under(views.of(i), |ctx| slot.call.update(ctx));
        let tag = tv.tag();
        if gxdbg_slot() {
            eprintln!(
                "SLOT call[{i}] produced tag={} fresh={}",
                tag.bits(),
                views.fresh(i)
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
    fn for_each_child<'a>(&'a self, f: &mut dyn FnMut(&'a Node<R, E>)) {
        f(&self.base.source);
        f(&self.base.prototype)
    }

    fn for_each_child_mut(&mut self, f: &mut dyn FnMut(&mut Node<R, E>)) {
        f(&mut self.base.source);
        f(&mut self.base.prototype)
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

    fn fuse(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<Option<Node<R, E>>> {
        fuse_callback(ctx, &mut self.base.prototype, &mut self.callback)
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R, E>) -> &TagValue {
        let woke = self.slept.take();
        let old_len = self.slots.len();
        let mut production = None;
        let (callback, element_type) = (&self.callback, &self.base.element_type);
        let Sourced { tag, source, resized, back, paused, moved } = take_source::<
            _,
            _,
            C,
            _,
        >(
            ctx,
            &mut self.base.source,
            &self.base.prototype,
            &mut self.slots,
            &mut self.seen,
            woke,
            &mut self.fork.build,
            SlotKind { delete: Slot::delete, call: |s| &mut s.call, element: |s| s.id },
            |ctx, kind| Slot::new(ctx, callback, element_type, kind),
        );
        let Some(source) = source else {
            return self.resident.set_bottom_as(tag);
        };
        self.current = source;
        {
            // a resize or a return is a new result whatever the source's
            // tag; so are moved elements under a result that reads them
            let tag = if resized || back { tag.fresh() } else { tag };
            if resized || back || (self.base.op.reads_elements() && moved) {
                production = merge_tag(production, tag);
            }
            if self.slots.is_empty() {
                let v = self.finish(ctx);
                return self.resident.set(TagValue::tagged(v, tag));
            }
        }
        let rebound = ctx.event.variables.contains_key(&self.callback.id);
        let slots = woken_slots(ctx, old_len, paused, |ctx, views| {
            update_slots(ctx, &mut self.slots, &mut self.fork, views, rebound)
        });
        match slots {
            None => return self.resident.ride(),
            Some(Some(tag)) => production = merge_tag(production, tag),
            Some(None) => (),
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
        ctx.rt.store_release(&base.prototype_id);
        for slot in slots.iter_mut() {
            slot.delete(ctx);
        }
    }

    fn typecheck0(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
        each(self.base.nodes_mut(), |n| n.typecheck0(ctx))
    }

    fn typecheck1(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
        self.callback.see_arg(ctx);
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
            .emit(&self.base.source, &self.base.spec, cx)?
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
        let (acc_id, acc) = callback.slot_arg(ctx, "collection_acc", acc_type);
        let (element_id, element) =
            callback.slot_arg(ctx, "collection_element", element_type);
        let call = callback.call(ctx, smallvec![acc, element], kind);
        Self { acc_id, element_id, call, state: SlotState::Bottom }
    }

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.call.delete(ctx);
        for id in [self.acc_id, self.element_id] {
            ctx.rt.store_release(&id);
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
    spec: Arc<Expr>,
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
        let r = self.emit(source, init, &callsite.spec, cx);
        cx.swap_collection_site(prev);
        r
    }

    fn emit(
        &self,
        source: &Node<R, E>,
        init: &Node<R, E>,
        spec: &Expr,
        cx: &mut BodyCx,
    ) -> Result<Option<CompiledExpr>> {
        if emit::node_is_bottom(source) || emit::node_is_bottom(init) {
            return Ok(None);
        }
        let Some(callback) = callback(&self.prototype) else { return Ok(None) };
        let Some(acc) = callback_param(callback, 0, literal!("__acc"), spec)? else {
            return Ok(None);
        };
        let Some(element) = callback_param(callback, 1, literal!("__elem"), spec)? else {
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
    seen: SourceSeen,
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
            seen: SourceSeen::Absent,
            collection: PhantomData,
            resident: TagValue::phantom(),
            build: ProbeSite::default(),
        })
    }

    fn new(
        intrinsic: CollectionIntrinsic,
        ctx: &mut CompileCtx<R, E>,
        spec: Arc<Expr>,
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
        let (source_id, callback) = Callback::of_args(scope, top_id, typ, args, 2)?;
        let init_id = args[1]
            .single_bind_id()
            .ok_or_else(|| anyhow!("collection init argument must be a name"))?;
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
            spec: Arc::new(Expr::decode(buf)?),
            typ: Type::decode(buf)?,
        };
        let callback = Callback::image_decode(ctx, buf)?;
        let acc_type = Type::decode(buf)?;
        Ok(Self::with(base, callback, acc_type))
    }
}

impl<R: Rt, E: UserEvent, C: MapCollection> Update<R, E> for FoldQ<R, E, C> {
    fn for_each_child<'a>(&'a self, f: &mut dyn FnMut(&'a Node<R, E>)) {
        f(&self.base.source);
        f(&self.base.init);
        f(&self.base.prototype)
    }

    fn for_each_child_mut(&mut self, f: &mut dyn FnMut(&mut Node<R, E>)) {
        f(&mut self.base.source);
        f(&mut self.base.init);
        f(&mut self.base.prototype)
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

    fn fuse(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<Option<Node<R, E>>> {
        fuse_callback(ctx, &mut self.base.prototype, &mut self.callback)
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R, E>) -> &TagValue {
        let woke = self.slept.take();
        let old_len = self.slots.len();
        let (callback, acc, elt) =
            (&self.callback, &self.acc_type, &self.base.element_type);
        let Sourced { tag, source, resized, back, paused, moved } =
            take_source::<_, _, C, _>(
                ctx,
                &mut self.base.source,
                &self.base.prototype,
                &mut self.slots,
                &mut self.seen,
                woke,
                &mut self.build,
                SlotKind {
                    delete: FoldSlot::delete,
                    call: |s| &mut s.call,
                    element: |s| s.element_id,
                },
                |ctx, kind| FoldSlot::new(ctx, callback, acc, elt, kind),
            );
        // A bottom init is a poisoned delivery to slot 0's acc, not a
        // whole-fold abort: a callback that never consumes the acc
        // recovers.
        let init = self.base.init.update(ctx).clone();
        if source.is_none() {
            return self.resident.set_bottom_as(tag);
        }
        // a quiet acc is news to no slot: it only stands, unless the
        // slots read their accs anew
        let anew = moved || woke || back;
        let pass_acc = |ctx: &mut ExecCtx<'_, R, E>, id, tv: TagValue| {
            if tv.tag().triggers() || anew {
                ctx.deliver(id, tv)
            } else {
                ctx.rt.store_insert_standing(id, tv)
            }
        };
        if let Some(slot) = self.slots.first() {
            pass_acc(ctx, slot.acc_id, init.clone());
        }
        if self.slots.is_empty() {
            // a resize or a return is a new result whatever the source's tag
            let tag = if resized || back { tag.fresh() } else { tag };
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
        if resized || back {
            prod = merge_tag(prod, tag.fresh());
        }
        let slots = &mut self.slots;
        let prod = woken_slots(ctx, old_len, paused, |ctx, views| {
            for i in 0..slots.len() {
                if ctx.interrupted() {
                    return None;
                }
                // A fresh slot's first dispatch runs under the birth view, its
                // acc seeded with the chain's state as it stands.
                if views.fresh(i) {
                    let seed = match i {
                        0 if init.tag().is_bottom() => {
                            TagValue::tagged(Value::Null, Tag::FRESH_BOTTOM)
                        }
                        0 => TagValue::fired(init.value_cloned()),
                        _ => match &slots[i - 1].state {
                            SlotState::Bottom => {
                                TagValue::tagged(Value::Null, Tag::FRESH_BOTTOM)
                            }
                            SlotState::Value(v) => TagValue::fired(v.clone()),
                        },
                    };
                    ctx.deliver(slots[i].acc_id, seed);
                }
                let slot = &mut slots[i];
                let tv = ctx.under(views.of(i), |ctx| slot.call.update(ctx).clone());
                prod = merge_tag(prod, tv.tag());
                slot.state.set(&tv);
                // The production, a bottom included, travels the acc chain.
                if let Some(next) = slots.get(i + 1) {
                    pass_acc(ctx, next.acc_id, tv);
                }
            }
            Some(prod)
        });
        let Some(prod) = prod else { return self.resident.ride() };
        // An interior slot's poison bottoms the fold only if a downstream
        // callback consumes it; only the last slot's state is the result.
        let prod = prod.unwrap_or(Tag::STALE);
        match self.slots.last().map(|s| &s.state) {
            Some(SlotState::Bottom) => self.resident.set_bottom_as(prod),
            // A fold fires iff it resized, a slot fired, or the source came
            // back from bottom.
            Some(SlotState::Value(v)) => {
                let tag = if prod.triggers() { Tag::FIRED } else { Tag::STALE };
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
            ctx.rt.store_release(&id);
        }
        for slot in slots.iter_mut() {
            slot.delete(ctx);
        }
    }

    fn typecheck0(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
        each(self.base.nodes_mut(), |n| n.typecheck0(ctx))
    }

    fn typecheck1(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
        self.callback.see_arg(ctx);
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
            .emit(&self.base.source, &self.base.init, &self.base.spec, cx)?
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
