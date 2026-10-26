use crate::{
    BindId, CAST_ERR, CAST_ERR_TAG, CFlag, CompileCtx, Event, ExecCtx, Node, NodeView,
    ParMode, PendingSettle, PrintFlag, Refs, Restore, Rt, Scope, Tag, TagValue, Update,
    UserEvent,
    branch::{fork_point, timed},
    cost::{ForkSite, Meter},
    defer_unresolved_names, env, errf,
    expr::{
        self, At, Expr, ExprId, ExprKind, ModPath, ModuleKind, Name, Origin, TypeDefBody,
    },
    format_with_flags,
    fusion::{
        self,
        emit::{
            BodyCx, CompiledExpr, emit_block_node, emit_cast_node, emit_const_node,
            emit_string_interpolate_node,
        },
        fuse,
    },
    ide::ReferenceSite,
    image::{
        self, ImageBuf,
        nodes::{NodeTag, decode_node, decode_nodes, encode_nodes, put_tag},
    },
    typ::{
        Mutability, TVal, TVar, Type, TypeMismatch,
        tvar::{InTask, TaskContext, TaskId, current_task, new_task},
    },
};
use ahash::{AHashMap, AHashSet};
use anyhow::{Context, Result, anyhow, bail};
use arcstr::ArcStr;
use compact_str::{CompactString, format_compact};
use compiler::{compile, compile_module};
use enumflags2::BitFlags;
use netidx_core::pack::{Pack, PackError};
use netidx_value::{Typ, Value};
use poolshark::local::LPooled;
use rayon::prelude::*;
use smallvec::SmallVec;
use std::{cell::Cell, iter, mem, sync::OnceLock};
use triomphe::Arc;

pub(crate) mod array;
pub use collection::MAX_ARRAY_INIT_LEN;
pub use error::ErrorRelay;
pub(crate) mod bind;
pub mod callsite;
pub mod collection;
pub(crate) mod compiler;
pub mod coretraits;
pub(crate) mod data;
pub(crate) mod error;
pub(crate) mod fork_control;
pub mod genn;
pub mod lambda;
pub use graphix_types::list;
pub(crate) mod map;
pub(crate) mod module;
pub(crate) mod op;
pub(crate) mod pattern;
pub mod place;
pub(crate) mod prune;
pub(crate) mod select;
pub(crate) mod seq_machine;
pub mod traits;
pub(crate) mod wake;

/// A variable read's provenance under dense delivery — see [`read_var`].
pub(crate) enum VarRead<'a> {
    /// Found in the overlay (this cycle's transient deliveries) or
    /// store-stamped THIS cycle: the entry's own tag applies.
    Delivered(&'a TagValue),
    /// A standing store entry from an earlier cycle: the value
    /// channel. Readers view it Stale, or Fired at a birth.
    Standing(&'a TagValue),
}

/// Read a variable: the overlay, then the persistent store. `None`
/// means the bind has never delivered.
pub(crate) fn read_var<'a, R: Rt, E: UserEvent>(
    ctx: &'a ExecCtx<'_, R, E>,
    id: &BindId,
) -> Option<VarRead<'a>> {
    if let crate::branch::RtView::Fork(f) = &ctx.rt {
        f.note_read(*id);
    }
    if let Some(tv) = ctx.event.variables.get(id) {
        return Some(VarRead::Delivered(tv));
    }
    match ctx.rt.store_get(id) {
        Some((tv, stamp)) if *stamp == ctx.rt.cycle() => Some(VarRead::Delivered(tv)),
        Some((tv, _)) => Some(VarRead::Standing(tv)),
        None => None,
    }
}

/// A standing entry as a reader sees it: fresh at a birth only. A wake
/// or a step reads it stale: its value is a past event the graph
/// already consumed.
pub(crate) fn standing_view<E: UserEvent>(event: &Event<E>, tv: &TagValue) -> TagValue {
    let tag = if event.view == crate::View::Birth {
        tv.tag().fresh()
    } else {
        tv.tag().quiet()
    };
    let mut tv = tv.clone();
    tv.retag(tag);
    tv
}

#[macro_export]
macro_rules! wrap {
    ($n:expr, $e:expr) => {
        $crate::expr::At::at($e, $n.spec())
    };
}

/// Compile-time `bail!` of an error that arose at the expression
/// `$spec`.
#[macro_export]
macro_rules! bailat {
    ($spec:expr, $($arg:tt)*) => {
        return ::std::result::Result::Err(
            $crate::expr::At::at(::anyhow::anyhow!($($arg)*), &$spec)
        )
    };
}

#[macro_export]
macro_rules! deref_typ {
    ($name:literal, $ctx:expr, $typ:expr, $($pat:pat => $body:expr),+) => {{
        // an unbound cell is read through its one structural bound, which
        // every type it admits has
        let mut bound = match &$typ {
            $crate::typ::Type::TVar(tv) => {
                let mut c = tv.cell_constraints();
                match (c.len(), c.pop()) {
                    (
                        1,
                        Some(
                            t @ ($crate::typ::Type::Struct(_)
                            | $crate::typ::Type::Tuple(_)
                            | $crate::typ::Type::Fn(_)),
                        ),
                    ) => Some(t),
                    _ => None,
                }
            }
            _ => None,
        };
        $typ.with_deref(|typ| {
            let mut typ = typ.cloned();
            loop {
                #[allow(unreachable_patterns)]
                match &typ {
                    $($pat => break $body),+,
                    // an alias chain ends: Env::deftype refuses a cycle
                    Some(rt @ $crate::typ::Type::Ref(_)) => {
                        typ = Some(rt.lookup_ref(&$ctx.env)?);
                    }
                    // a Set built while a member still held unbound TVars
                    // never re-collapses on its own; normalize may collapse it
                    Some(t @ $crate::typ::Type::Set(_)) => {
                        let nt = t.normalize();
                        if matches!(nt, $crate::typ::Type::Set(_)) {
                            $crate::format_with_flags($crate::PrintFlag::DerefTVars, || {
                                anyhow::bail!("expected {} not {nt}", $name)
                            })?
                        }
                        typ = Some(nt);
                    }
                    Some(t) => $crate::format_with_flags($crate::PrintFlag::DerefTVars, || {
                        anyhow::bail!("expected {} not {t}", $name)
                    })?,
                    None => match bound.take() {
                        Some(b) => typ = Some(b),
                        None => anyhow::bail!(
                            "reading a {} needs its type known here: annotate the binding",
                            $name
                        ),
                    },
                }
            }
        })
    }};
}

pub(crate) use crate::expr::NOP;

/// Set by a node's `sleep()`, taken by its next update: the first update
/// after a sleep recomputes from the present world.
#[derive(Debug, Default, Clone, Copy)]
pub struct WakeBit(bool);

impl WakeBit {
    pub fn set(&mut self) {
        self.0 = true
    }

    pub fn take(&mut self) -> bool {
        std::mem::take(&mut self.0)
    }

    pub fn is_set(&self) -> bool {
        self.0
    }
}

thread_local! {
    static DESELECTING_ARM: Cell<bool> = const { Cell::new(false) };
    static RUNTIME_BIND: Cell<bool> = const { Cell::new(false) };
}

/// Run `f` inside (`true`) or outside (`false`) the sleep of an arm a
/// select or a seq machine is deselecting: under it a recursive-edge
/// `CallSite::sleep` deletes its callee (shrink = delete). A callee body
/// is outside, so a whole-recursion pause retains.
pub(crate) fn deselecting_arm<T>(inside: bool, f: impl FnOnce() -> T) -> T {
    let _restore = Restore::replace(&DESELECTING_ARM, inside);
    f()
}

pub(crate) fn in_deselected_arm() -> bool {
    DESELECTING_ARM.get()
}

#[derive(Debug)]
pub struct Nop {
    pub typ: Type,
}

impl Nop {
    pub(crate) fn new<R: Rt, E: UserEvent>(typ: Type) -> Node<R, E> {
        Node::new(Nop { typ })
    }
}

impl Nop {
    pub(crate) fn image_decode<R: Rt, E: UserEvent>(
        _ctx: &mut ExecCtx<'_, R, E>,
        buf: &mut &[u8],
    ) -> Result<Node<R, E>, PackError> {
        Ok(Self::new(Type::decode(buf)?))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Nop {
    fn for_each_child<'a>(&'a self, f: &mut dyn FnMut(&'a Node<R, E>)) {
        let _ = f;
    }

    fn for_each_child_mut(&mut self, f: &mut dyn FnMut(&mut Node<R, E>)) {
        let _ = f;
    }

    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::Nop, buf);
        self.typ.encode(buf)
    }

    fn update(&mut self, _ctx: &mut ExecCtx<'_, R, E>) -> &TagValue {
        TagValue::phantom_ref()
    }

    fn spec(&self) -> &Expr {
        &NOP
    }

    fn typ(&self) -> &Type {
        &self.typ
    }

    fn view(&self) -> NodeView<'_, R, E> {
        NodeView::Nop(self)
    }
}

#[derive(Debug)]
pub struct ExplicitParens<R: Rt, E: UserEvent> {
    pub(crate) spec: Arc<Expr>,
    pub n: Node<R, E>,
}

impl<R: Rt, E: UserEvent> ExplicitParens<R, E> {
    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<'_, R, E>,
        buf: &mut &[u8],
    ) -> Result<Node<R, E>, PackError> {
        let spec = Arc::new(Expr::decode(buf)?);
        let n = decode_node(ctx, buf)?;
        Ok(Node::new(Self { spec, n }))
    }

    pub(crate) fn compile(
        ctx: &mut CompileCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: Arc<Expr>,
        inner: Expr,
        scope: &Scope,
        top_id: ExprId,
    ) -> Result<Node<R, E>> {
        // `spec` is the outer parens expression: it carries the position
        // and any `#[..]` decorations, so the node must own it
        let n = compile(ctx, flags, inner, scope, top_id)?;
        Ok(Node::new(ExplicitParens { spec, n }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for ExplicitParens<R, E> {
    fn for_each_child<'a>(&'a self, f: &mut dyn FnMut(&'a Node<R, E>)) {
        f(&self.n)
    }

    fn for_each_child_mut(&mut self, f: &mut dyn FnMut(&mut Node<R, E>)) {
        f(&mut self.n)
    }

    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::ExplicitParens, buf);
        self.spec.encode(buf)?;
        self.n.image_encode(buf)
    }

    fn fuse(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<Option<Node<R, E>>> {
        // parens mean nothing to fusion, unless an attribute they carry
        // names the expression a region (`#[native] (e)`)
        match self.spec.dec.as_ref().is_some_and(|d| !d.attrs.is_empty()) {
            true => fuse(&mut self.n, ctx).map(|()| None),
            false => fusion::fuse_parts([&mut self.n], ctx),
        }
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R, E>) -> &TagValue {
        self.n.update(ctx)
    }

    typed_by_row!(children);

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn typ(&self) -> &Type {
        &self.n.typ()
    }

    fn view(&self) -> NodeView<'_, R, E> {
        NodeView::ExplicitParens(self)
    }

    fn emit_clif(&self, cx: &mut BodyCx) -> Result<CompiledExpr> {
        // `(x)` — grouping only; transparent recurse.
        self.n.emit_clif(cx)
    }
}

/// A child `Node` plus its last value, kept under a poisoned tag, for
/// the readers that must read history a production cannot carry: the
/// select scrutinee, a pattern guard's truth, and `~`'s held arg.
/// Everything else reads its children's productions directly.
#[derive(Debug)]
pub struct Held<R: Rt, E: UserEvent> {
    /// The last value-bearing production (`Some` = there was once a
    /// real value; a bottom's placeholder never lands here).
    pub value: Option<Value>,
    /// The tag of the child's last production. Only the TAINT bit is
    /// meaningful at rest; firedness belongs to a production, not to
    /// held memory.
    pub tag: Tag,
    pub node: Node<R, E>,
}

impl<R: Rt, E: UserEvent> Held<R, E> {
    pub(crate) fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        self.node.image_encode(buf)
    }

    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<'_, R, E>,
        buf: &mut &[u8],
    ) -> Result<Self, PackError> {
        Ok(Self::new(decode_node(ctx, buf)?))
    }

    pub fn new(node: Node<R, E>) -> Self {
        Self { value: None, tag: Tag::FIRED, node }
    }

    /// Update the node, returning the production's tag. A bottom
    /// production poisons the tag but never overwrites the value.
    pub fn update(&mut self, ctx: &mut ExecCtx<'_, R, E>) -> Tag {
        let tv = self.node.update(ctx);
        let tag = tv.tag();
        if !tag.is_bottom() {
            self.value = Some(tv.value_cloned());
        }
        self.tag = tag;
        tag
    }

    /// Sleep is pause, not reset: the held value and its at-rest taint
    /// survive.
    pub fn sleep(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.node.sleep(ctx)
    }
}

/// Update every child of a composite and join their tags
/// ([`Tag::join`]): the join triggers when a child did and is bottom
/// when a child is; past the caller's [`dense_gate!`] it is the
/// composite's own tag. The productions stay borrowed, so a value is
/// cloned only once the gate let it through.
pub(crate) fn gather<'a, R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<'_, R, E>,
    nodes: &'a mut [Node<R, E>],
    site: &mut ForkSite,
) -> (Tag, SmallVec<[&'a TagValue; 32]>) {
    let n = nodes.len();
    site.decide_siblings(ctx, n, nodes.iter());
    fork_point(ctx, nodes, site, gather_in_order, |(t0, mut p0), (t1, p1)| {
        p0.extend(p1);
        (t0.join(t1), p0)
    })
}

fn gather_in_order<'a, R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<'_, R, E>,
    nodes: &'a mut [Node<R, E>],
    mut meter: Option<&mut Meter<'_>>,
) -> (Tag, SmallVec<[&'a TagValue; 32]>) {
    let mut tag = Tag::STALE;
    let prods = nodes
        .iter_mut()
        .enumerate()
        .map(|(i, c)| {
            let tv = timed(&mut meter, i, || c.update(ctx));
            tag = tag.join(tv.tag());
            tv
        })
        .collect();
    (tag, prods)
}

/// A strict computation propagates consumed bottom before considering
/// its cached result. Quiet recomputation cannot manufacture an event.
/// `$tag` is the join of the consumed productions.
macro_rules! dense_gate {
    ($self:ident, $tag:expr, $bottom:expr) => {{
        let woke = $self.slept.take();
        $crate::node::dense_gate!($self.resident, $tag, $bottom, woke);
    }};
    ($resident:expr, $tag:expr, $bottom:expr, $woke:expr) => {{
        let tag: $crate::Tag = $tag;
        if $bottom {
            return $resident.set_bottom_as(tag);
        }
        if !(tag.triggers() || $resident.tag().is_bottom() || $woke) {
            return $resident.ride();
        }
    }};
}
pub(crate) use dense_gate;

/// `typecheck0` and `typecheck0_instance` for a node whose
/// `typecheck0_with(ctx, child, check)` visits its children with `child`
/// and judges its own type only when `check`: an instance takes the type
/// from its row and judges it only where its definition's check recorded
/// none (an id two nodes share).
/// An `Update`'s `typecheck0`/`typecheck0_instance` pair over its
/// `typecheck0_with`: by the instance's row (no argument), with the
/// instance judging nothing (`source`: its types come from its source),
/// or over its children alone (`children`).
macro_rules! typed_by_row {
    () => {
        fn typecheck0(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
            self.typecheck0_with(ctx, &mut |n, ctx| n.typecheck0(ctx), true)
        }

        fn typecheck0_instance(
            &mut self,
            ctx: &mut CompileCtx<R, E>,
            types: &mut $crate::node::lambda::InstanceTypes,
        ) -> Result<()> {
            let row = types.settle(self.spec.id, &self.typ);
            self.typecheck0_with(
                ctx,
                &mut |n, ctx| n.typecheck0_instance(ctx, types),
                !row,
            )
        }
    };
    (source) => {
        fn typecheck0(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
            self.typecheck0_with(ctx, &mut |n, ctx| n.typecheck0(ctx), true)
        }

        fn typecheck0_instance(
            &mut self,
            ctx: &mut CompileCtx<R, E>,
            types: &mut $crate::node::lambda::InstanceTypes,
        ) -> Result<()> {
            self.typecheck0_with(
                ctx,
                &mut |n, ctx| n.typecheck0_instance(ctx, types),
                false,
            )
        }
    };
    (children) => {
        fn typecheck0(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
            self.typecheck0_with(ctx, &mut |n, ctx| n.typecheck0(ctx))
        }

        fn typecheck0_instance(
            &mut self,
            ctx: &mut CompileCtx<R, E>,
            types: &mut $crate::node::lambda::InstanceTypes,
        ) -> Result<()> {
            self.typecheck0_with(ctx, &mut |n, ctx| n.typecheck0_instance(ctx, types))
        }
    };
}
pub(crate) use typed_by_row;

#[derive(Debug)]
pub struct TypeDef {
    spec: Arc<Expr>,
    scope: ModPath,
    name: ArcStr,
}

impl TypeDef {
    pub(crate) fn compile<R: Rt, E: UserEvent>(
        ctx: &mut CompileCtx<R, E>,
        spec: Arc<Expr>,
        scope: &Scope,
        name: &Name,
        params: &Arc<[(TVar, Option<Type>)]>,
        body: &TypeDefBody,
    ) -> Result<Node<R, E>> {
        let (params, body) = Type::reset_typedef_tvars(params, body);
        let parametric = !params.is_empty();
        let verdict = ctx
            .env
            .deftype(
                &scope.lexical,
                name,
                params,
                &body,
                false,
                None,
                name.pos_or(spec.pos),
                spec.pos,
                spec.ori.clone(),
            )
            .at(&spec)?;
        let name = name.name.clone();
        if parametric {
            // A cycle a definition completes runs through it: without
            // parameters through its body, which deftype walked; with them also
            // through an argument (`type T = G<[i64, T]>`), which only T's walk sees.
            crate::recheck_pending_contractive(ctx)?;
        }
        if verdict == crate::typ::Unguarded::Unknown {
            ctx.pending_names.push(crate::Pending::Contractive(
                scope.lexical.clone(),
                name.clone(),
                (*spec).clone(),
            ));
        }
        Ok(Node::new(Self { spec, scope: scope.lexical.clone(), name }))
    }
}

impl TypeDef {
    pub(crate) fn image_decode<R: Rt, E: UserEvent>(
        _ctx: &mut ExecCtx<'_, R, E>,
        buf: &mut &[u8],
    ) -> Result<Node<R, E>, PackError> {
        let spec = Arc::new(Expr::decode(buf)?);
        let scope = ModPath::decode(buf)?;
        let name = ArcStr::decode(buf)?;
        Ok(Node::new(Self { spec, scope, name }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for TypeDef {
    fn for_each_child<'a>(&'a self, f: &mut dyn FnMut(&'a Node<R, E>)) {
        let _ = f;
    }

    fn for_each_child_mut(&mut self, f: &mut dyn FnMut(&mut Node<R, E>)) {
        let _ = f;
    }

    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::TypeDef, buf);
        self.spec.encode(buf)?;
        self.scope.encode(buf)?;
        self.name.encode(buf)
    }

    fn update(&mut self, _ctx: &mut ExecCtx<'_, R, E>) -> &TagValue {
        TagValue::phantom_ref()
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        ctx.env.undeftype(&self.scope, &self.name)
    }

    fn typ(&self) -> &Type {
        Type::BOTTOM
    }

    fn view(&self) -> NodeView<'_, R, E> {
        NodeView::TypeDef(self)
    }
}

#[derive(Debug)]
pub struct Constant {
    pub(super) spec: Arc<Expr>,
    pub value: Value,
    pub typ: Type,
    resident: TagValue,
}

impl Constant {
    /// Construct a `Constant` node from its final components.
    pub fn new<R: Rt, E: UserEvent>(
        value: Value,
        typ: Type,
        spec: Arc<Expr>,
    ) -> Node<R, E> {
        // the resident starts Stale so an instance bound outside a birth
        // still computes; it fires only where it is born
        let resident = TagValue::stale(value.clone());
        Node::new(Self { spec, value, typ, resident })
    }
}

impl Constant {
    pub(crate) fn image_decode<R: Rt, E: UserEvent>(
        _ctx: &mut ExecCtx<'_, R, E>,
        buf: &mut &[u8],
    ) -> Result<Node<R, E>, PackError> {
        let spec = Arc::new(Expr::decode(buf)?);
        let value = Value::decode(buf)?;
        let typ = Type::decode(buf)?;
        Ok(Self::new(value, typ, spec))
    }
}

/// A constant's production: fired where it is born, standing otherwise.
/// Every argument-less literal is a constant.
pub(crate) fn produce_constant<'a, E: UserEvent>(
    event: &Event<E>,
    resident: &'a mut TagValue,
    value: impl FnOnce() -> Value,
) -> &'a TagValue {
    // a constant fires where it is born; off a birth, a resume included,
    // it stands stale with its value, as a kernel's does
    if event.born() {
        resident.set(TagValue::fired(value()))
    } else if resident.is_bottom() {
        resident.set(TagValue::stale(value()))
    } else {
        resident.ride()
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Constant {
    fn for_each_child<'a>(&'a self, f: &mut dyn FnMut(&'a Node<R, E>)) {
        let _ = f;
    }

    fn for_each_child_mut(&mut self, f: &mut dyn FnMut(&mut Node<R, E>)) {
        let _ = f;
    }

    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::Constant, buf);
        self.spec.encode(buf)?;
        self.value.encode(buf)?;
        self.typ.encode(buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R, E>) -> &TagValue {
        produce_constant(ctx.event, &mut self.resident, || self.value.clone())
    }

    fn typ(&self) -> &Type {
        &self.typ
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn view(&self) -> NodeView<'_, R, E> {
        NodeView::Constant(self)
    }

    fn emit_clif(&self, cx: &mut BodyCx) -> Result<CompiledExpr> {
        emit_const_node(cx, &self.value, &self.typ)
    }
}

#[derive(Debug)]
pub struct Block<R: Rt, E: UserEvent> {
    pub(crate) module: bool,
    pub(crate) spec: Arc<Expr>,
    pub(crate) children: Box<[Node<R, E>]>,
    /// Indices of `catch(e) expr` children, in syntactic order.
    /// update/typecheck run the covered children first, then catches in
    /// reverse syntactic order: an inner handler's rethrow delivers to
    /// its predecessor, which only sees it if it updates after.
    pub(crate) catches: Box<[usize]>,
    /// The children that are `let`s, which take back at the block's end
    /// what a wake re-raised (`Bind::take_back_reraised`).
    lets: Box<[u32]>,
    /// The block's production when it cannot hand back its last
    /// child's borrow: after a catch pass or a forked run.
    resident: TagValue,
    pub(crate) runs: Runs,
}

/// A statement list's runs for forking: as the analysis planned them
/// (`analysis::plan_forks`), which an image carries, and their fork
/// points, made at the first update that may fork (planned there for a
/// body no analysis reached).
#[derive(Debug, Default)]
pub(crate) struct Runs {
    pub(crate) planned: OnceLock<Box<[(u32, u32)]>>,
    sites: Option<Box<[(u32, u32, ForkSite)]>>,
}

impl Runs {
    pub(crate) fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        self.planned.get().map(|p| p.to_vec()).encode(buf)
    }

    pub(crate) fn image_decode(buf: &mut &[u8]) -> Result<Self, PackError> {
        let runs = Self::default();
        if let Some(p) = Option::<Vec<(u32, u32)>>::decode(buf)? {
            let _ = runs.planned.set(p.into_boxed_slice());
        }
        Ok(runs)
    }

    /// Update `children` run by run, each forked where its site says, then
    /// the catches in reverse syntactic order: the last child's
    /// production, unless it is a catch.
    pub(crate) fn update<R: Rt, E: UserEvent>(
        &mut self,
        ctx: &mut ExecCtx<'_, R, E>,
        children: &mut [Node<R, E>],
        catches: &[usize],
    ) -> Option<TagValue> {
        let Self { planned, sites } = self;
        let sites = sites.get_or_insert_with(|| {
            planned
                .get_or_init(|| crate::analysis::plan_block(children, catches, ctx))
                .iter()
                .map(|&(a, b)| (a, b, ForkSite::planned()))
                .collect()
        });
        let last = children.len();
        let mut res: Option<TagValue> = None;
        for (a, b, site) in sites.iter_mut() {
            let (a, b) = (*a, *b);
            let tv = update_run(ctx, &mut children[a as usize..b as usize], site);
            if b as usize == last {
                res = Some(tv.clone());
            }
        }
        for &c in catches.iter().rev() {
            children[c].update(ctx);
        }
        res
    }
}

impl<R: Rt, E: UserEvent> Block<R, E> {
    /// [`Update::update`] forking each run of its plan.
    fn update_forking(&mut self, ctx: &mut ExecCtx<'_, R, E>) -> &TagValue {
        let res = self.runs.update(ctx, &mut self.children, &self.catches);
        self.take_back_reraised(ctx);
        match res {
            _ if self.module => TagValue::phantom_ref(),
            Some(tv) => self.resident.set(tv),
            None => self.resident.ride(),
        }
    }

    fn take_back_reraised(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        for i in self.lets.iter() {
            if let Some(b) = self.children[*i as usize].downcast_mut::<bind::Bind<R, E>>()
            {
                b.take_back_reraised(ctx)
            }
        }
    }

    /// Build a `Block` from compiled children. A module produces no
    /// value; a non-module block's value is its last child's.
    pub fn new(module: bool, children: Box<[Node<R, E>]>, spec: Arc<Expr>) -> Node<R, E> {
        Node::new(Self {
            module,
            spec,
            lets: lets_of(&children),
            children,
            catches: Box::default(),
            resident: TagValue::phantom(),
            runs: Runs::default(),
        })
    }

    pub(crate) fn compile(
        ctx: &mut CompileCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: Arc<Expr>,
        scope: &Scope,
        top_id: ExprId,
        module: bool,
        exprs: &Arc<[Expr]>,
    ) -> Result<Node<R, E>> {
        let (children, catches) =
            compile_block_children(ctx, flags, scope, top_id, module, exprs.iter())?;
        Ok(Node::new(Self {
            module,
            spec,
            lets: lets_of(&children),
            children,
            catches,
            resident: TagValue::phantom(),
            runs: Runs::default(),
        }))
    }
}

/// The indices of the `let`s among a block's children.
fn lets_of<R: Rt, E: UserEvent>(children: &[Node<R, E>]) -> Box<[u32]> {
    let mut lets: SmallVec<[u32; 8]> = SmallVec::new();
    for (i, n) in children.iter().enumerate() {
        if matches!(n.view(), NodeView::Bind(_)) {
            lets.push(i as u32)
        }
    }
    lets.into_iter().collect()
}

/// Compile a statement list: each `catch(e) expr` child compiles
/// through [`error::Catch::compile`], which advances the dynamic scope
/// for all subsequent siblings. Returns the children in syntactic order
/// plus the catch indices.
pub(crate) fn compile_block_children<'a, R: Rt, E: UserEvent>(
    ctx: &mut CompileCtx<R, E>,
    flags: BitFlags<CFlag>,
    scope: &Scope,
    top_id: ExprId,
    module: bool,
    exprs: impl Iterator<Item = &'a Expr>,
) -> Result<(Box<[Node<R, E>]>, Box<[usize]>)> {
    let exprs: SmallVec<[&'a Expr; 32]> = exprs.collect();
    // pre-register the block's `mod` names so resolution is independent
    // of declaration order; the `Module` arm is told, so its own
    // duplicate guard does not trip
    for e in exprs.iter() {
        if let ExprKind::Module { name, .. } = &e.kind {
            let p = ModPath(scope.lexical.append(name));
            if ctx.env.modules.contains(&p) {
                return Err(anyhow!("duplicate module definition {p}").at(&(*e)));
            }
            ctx.env.modules.insert_cow(p);
        }
    }
    let mut scope = scope.clone();
    let mut children: LPooled<Vec<Node<R, E>>> = LPooled::take();
    let mut catches: LPooled<Vec<usize>> = LPooled::take();
    let n = exprs.len();
    ctx.written_lets.push(written_lets(&exprs));
    let compiled = exprs.iter().copied().enumerate().try_for_each(|(i, e)| {
        if matches!(e.kind, ExprKind::Catch(_)) {
            catches.push(i);
        }
        let at = StmtAt::Block { value: !module && i + 1 == n, last: i + 1 == n };
        let boundary = prune::Prune::boundary(ctx);
        let (node, next) = ctx.in_boundary(Some(&boundary), |ctx| {
            compile_statement(ctx, flags, e, &scope, top_id, at)
        })?;
        scope = next;
        children.push(prune::Prune::wrap(boundary, node));
        Ok::<_, anyhow::Error>(())
    });
    ctx.written_lets.pop();
    compiled?;
    for n in children.iter() {
        defer_typedef_names(ctx, n);
    }
    Ok((Box::from_iter(children.drain(..)), Box::from_iter(catches.drain(..))))
}

/// The lets among a block's statements that a `<-` or a `&mut` in the
/// block writes, lambda bodies included, each as its statement's id and
/// the written name: a write names the latest let of the name before it
/// (its own, for a `let rec`), where no nearer binding stands between.
/// Such a let is never generalized, whatever the order of its uses and
/// writers.
pub(crate) fn written_lets(exprs: &[&Expr]) -> AHashSet<(ExprId, CompactString)> {
    let mut w = Writes::default();
    for e in exprs {
        let ExprKind::Bind(b) = &e.kind else {
            w.expr(e);
            continue;
        };
        if b.rec {
            w.declare(e.id, &b.pattern);
            w.expr(&b.value);
        } else {
            w.expr(&b.value);
            w.declare(e.id, &b.pattern);
        }
    }
    w.written
}

#[derive(Default)]
struct Writes {
    /// The block's let of each name in scope at the walk's position.
    declared: AHashMap<CompactString, ExprId>,
    /// The nearer bindings in scope at the walk's position.
    shadow: Vec<ArcStr>,
    written: AHashSet<(ExprId, CompactString)>,
}

impl Writes {
    fn declare(&mut self, id: ExprId, p: &expr::StructurePattern) {
        p.with_names(&mut |n| {
            self.declared.insert(n.as_str().into(), id);
        });
    }

    fn target(&mut self, name: &ModPath) {
        let mut parts = netidx_core::path::Path::parts(&**name);
        let (Some(n), None) = (parts.next(), parts.next()) else { return };
        if let Some(id) = self.declared.get(n)
            && !self.shadow.iter().any(|s| s == n)
        {
            self.written.insert((*id, n.into()));
        }
    }

    fn bound(&mut self, p: &expr::StructurePattern) {
        p.with_names(&mut |n| self.shadow.push(n.clone()));
    }

    fn scoped(&mut self, f: impl FnOnce(&mut Self)) {
        let len = self.shadow.len();
        f(self);
        self.shadow.truncate(len);
    }

    fn block(&mut self, exprs: &[Expr]) {
        self.scoped(|w| {
            for e in exprs {
                w.expr(e);
                if let ExprKind::Bind(b) = &e.kind {
                    w.bound(&b.pattern)
                }
            }
        })
    }

    fn expr(&mut self, e: &Expr) {
        crate::stack::ensure_sufficient(|| match &e.kind {
            ExprKind::Connect { name, value, deref, .. } => {
                if !deref {
                    self.target(name)
                }
                self.expr(value)
            }
            ExprKind::ByRef(Mutability::Mut, inner) => {
                let root = match inner.unparen().place() {
                    Some((root, _)) => root,
                    None => inner.unparen(),
                };
                if let ExprKind::Ref { name } = &root.unparen().kind {
                    self.target(name)
                }
                self.expr(inner)
            }
            ExprKind::Bind(b) if b.rec => self.scoped(|w| {
                w.bound(&b.pattern);
                w.expr(&b.value)
            }),
            ExprKind::Lambda(l) => self.scoped(|w| {
                l.args.iter().for_each(|a| w.bound(&a.pattern));
                e.for_each_child(&mut |c| w.expr(c))
            }),
            ExprKind::Block { exprs } => self.block(exprs),
            ExprKind::Select(s) => {
                self.expr(&s.arg);
                for (p, body) in s.arms.iter() {
                    self.scoped(|w| {
                        w.bound(&p.structure_predicate);
                        if let Some(g) = &p.guard {
                            w.expr(g)
                        }
                        w.expr(body)
                    })
                }
            }
            ExprKind::Catch(c) => self.scoped(|w| {
                w.shadow.push(c.bind.name.clone());
                w.expr(&c.handler)
            }),
            ExprKind::TryWith(t) => {
                self.block(&t.body);
                self.scoped(|w| {
                    w.shadow.push(t.bind.name.clone());
                    w.block(&t.handler)
                })
            }
            ExprKind::Seq(sq) => {
                if let Some(a) = &sq.abort {
                    self.expr(a)
                }
                self.scoped(|w| {
                    match &sq.trigger {
                        Some(expr::SeqTrigger::Bind(b)) => {
                            w.expr(&b.value);
                            w.bound(&b.pattern)
                        }
                        Some(expr::SeqTrigger::Expr(t)) => w.expr(t),
                        None => (),
                    }
                    w.block(&sq.body)
                })
            }
            ExprKind::Module { .. } => (),
            _ => e.for_each_child(&mut |c| self.expr(c)),
        })
    }
}

/// Defer the names a type definition's body writes (`defer_unresolved_names`),
/// once every name its statement list declares is registered.
pub(crate) fn defer_typedef_names<R: Rt, E: UserEvent>(
    ctx: &mut CompileCtx<R, E>,
    n: &Node<R, E>,
) {
    let NodeView::TypeDef(td) = n.view() else { return };
    let Some(def) = ctx
        .env
        .typedefs
        .get(&td.scope)
        .and_then(|m| m.get(&CompactString::from(td.name.as_str())))
    else {
        return;
    };
    let written: LPooled<Vec<Type>> = iter::once(def.typ().clone())
        .chain(def.rep.clone())
        .chain(def.params().iter().filter_map(|(_, c)| c.clone()))
        .collect();
    for t in written.iter() {
        defer_unresolved_names(ctx, t, &td.spec);
    }
}

/// Where a statement stands: a top-level statement, or one of a block's,
/// `value` when it is the block's value (not a module block's).
#[derive(Clone, Copy)]
pub(crate) enum StmtAt {
    TopLevel,
    /// `value`: the statement is the block's value, its last statement
    /// unless the block is a module's body.
    Block {
        value: bool,
        last: bool,
    },
}

/// Compile one statement of a block or a top level: a declaration
/// (`let`, `catch`, `use`, `mod`, `type`, `trait`, `impl`) or an
/// expression. Returns the scope the statements after it compile in,
/// which a `catch` covers. In a block's value slot a declaration other
/// than a `let` compiles as an expression, which refuses it; a dynamic
/// module is an expression.
pub(crate) fn compile_statement<R: Rt, E: UserEvent>(
    ctx: &mut CompileCtx<R, E>,
    flags: BitFlags<CFlag>,
    e: &Expr,
    scope: &Scope,
    top_id: ExprId,
    at: StmtAt,
) -> Result<(Node<R, E>, Scope)> {
    let predeclared = matches!(at, StmtAt::Block { .. });
    // an attribute decorates a computation; a declaration has none
    let declaration = match &e.kind {
        ExprKind::Catch(_)
        | ExprKind::Use { .. }
        | ExprKind::TypeDef(_)
        | ExprKind::Trait(_)
        | ExprKind::Impl(_) => true,
        ExprKind::Module { value, .. } => !matches!(&**value, ModuleKind::Dynamic { .. }),
        _ => false,
    };
    if declaration && let Some(attr) = e.dec.as_ref().and_then(|d| d.attrs.first()) {
        bailat!(
            e,
            "#[{}] on a declaration: an attribute decorates a computation",
            attr.name
        )
    }
    let node = match &e.kind {
        ExprKind::Catch(_) if matches!(at, StmtAt::Block { last: true, .. }) => {
            bailat!(
                e,
                "a catch covers the statements after it, and the last statement of a \
                 block has none: it would cover nothing"
            )
        }
        ExprKind::Catch(c) => {
            let spec = ctx.shared_spec(e.clone());
            return error::Catch::compile(ctx, flags, spec, scope, top_id, c);
        }
        ExprKind::Module { name, value }
            if matches!(&**value, ModuleKind::Dynamic { .. }) =>
        {
            let spec = ctx.shared_spec(e.clone());
            compile_module(ctx, flags, spec, scope, top_id, name, value, predeclared)
        }
        ExprKind::Bind(_) => {
            compiler::compile_statement_expr(ctx, flags, e.clone(), scope, top_id)
        }
        _ if matches!(at, StmtAt::Block { value: true, .. }) => {
            compile(ctx, flags, e.clone(), scope, top_id)
        }
        ExprKind::Use { reexport, names } => {
            let spec = ctx.shared_spec(e.clone());
            module::compile_use(ctx, flags, spec, scope, *reexport, names)
        }
        ExprKind::Module { name, value } => {
            let spec = ctx.shared_spec(e.clone());
            compile_module(ctx, flags, spec, scope, top_id, name, value, predeclared)
        }
        ExprKind::TypeDef(td) => {
            let spec = ctx.shared_spec(e.clone());
            TypeDef::compile(ctx, spec, scope, &td.name, &td.params, &td.body)
        }
        ExprKind::Trait(t) => {
            let spec = ctx.shared_spec(e.clone());
            traits::Trait::compile(ctx, flags, spec, scope, t, top_id)
        }
        ExprKind::Impl(im) => {
            let spec = ctx.shared_spec(e.clone());
            traits::Impl::compile(ctx, flags, spec, scope, im, top_id)
        }
        _ => compile(ctx, flags, e.clone(), scope, top_id),
    }?;
    Ok((node, scope.clone()))
}

/// Update a run of independent statements in order, or forked where
/// its site's plan says: the last statement's production.
fn update_run<'a, R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<'_, R, E>,
    nodes: &'a mut [Node<R, E>],
    site: &mut ForkSite,
) -> &'a TagValue {
    fork_point(ctx, nodes, site, update_in_order, |_, last| last)
}

fn update_in_order<'a, R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<'_, R, E>,
    nodes: &'a mut [Node<R, E>],
    mut meter: Option<&mut Meter<'_>>,
) -> &'a TagValue {
    nodes
        .iter_mut()
        .enumerate()
        .fold(TagValue::phantom_ref(), |_, (i, n)| timed(&mut meter, i, || n.update(ctx)))
}

/// A block's children in evaluation order: those a `catch` covers in
/// order, then the catches innermost first, so a handler sees every error
/// its covered statements raise. `catches` is ascending.
pub(crate) fn evaluation_order(
    len: usize,
    catches: &[usize],
) -> impl Iterator<Item = usize> + '_ {
    let mut next = catches.iter().copied().peekable();
    (0..len)
        .filter(move |i| next.next_if_eq(i).is_none())
        .chain(catches.iter().rev().copied())
}

/// How a node's check visits a child: `typecheck0`, or an instance's
/// `typecheck0_instance`. A node whose check is the same either way
/// writes it once over this.
pub(crate) type Child<'a, R, E> =
    dyn FnMut(&mut Node<R, E>, &mut CompileCtx<R, E>) -> Result<()> + 'a;

/// A statement's check result: its errors carry the statement, and in a
/// module's body the module's origin.
fn statement_result<R: Rt, E: UserEvent>(
    r: Result<()>,
    n: &Node<R, E>,
    module: bool,
) -> Result<()> {
    let r = wrap!(n, r);
    match module {
        true => r.with_context(|| n.spec().ori.clone()),
        false => r,
    }
}

/// Run `check` over each statement of `work` in its compile task, in
/// parallel, and join the tasks in order: the first error in order is
/// the body's.
fn check_in_tasks<R: Rt, E: UserEvent>(
    ctx: &mut CompileCtx<R, E>,
    mut work: LPooled<Vec<(&mut Node<R, E>, CompileCtx<R, E>, TaskId)>>,
    module: bool,
    check: impl Fn(&mut Node<R, E>, &mut CompileCtx<R, E>) -> Result<()> + Sync,
) -> Result<()> {
    let cx = TaskContext::current();
    let mut results: LPooled<Vec<Result<()>>> = LPooled::take();
    ctx.control.eval_pool().install(|| {
        work.par_iter_mut()
            .map(|(n, task, id)| {
                let _cx = cx.with_task(*id).enter();
                let r = task.run_task(|task| check(n, task));
                statement_result(r, n, module)
            })
            .collect_into_vec(&mut results)
    });
    for (_, task, _) in work.drain(..) {
        ctx.join(task);
    }
    results.drain(..).find(|r| r.is_err()).unwrap_or(Ok(()))
}

/// Run a typecheck `pass` over `nodes` in [`evaluation_order`]. A module
/// body's errors also carry each statement's origin, the file it is in.
pub(crate) fn typecheck_in_order<R: Rt, E: UserEvent>(
    ctx: &mut CompileCtx<R, E>,
    nodes: &mut [Node<R, E>],
    catches: &[usize],
    module: bool,
    mut pass: impl FnMut(&mut Node<R, E>, &mut CompileCtx<R, E>) -> Result<()>,
) -> Result<()> {
    for i in evaluation_order(nodes.len(), catches) {
        let n = &mut nodes[i];
        let r = pass(n, ctx);
        statement_result(r, n, module)?
    }
    Ok(())
}

/// Check a block's statements (`typecheck0`) in [`evaluation_order`]. A
/// run of static modules checks in compile tasks, after the statements
/// before it: a body reaches its siblings only through their
/// interfaces (an impl a sibling's body adds undeclared is hidden from
/// it, `Env::hidden_impls`), so no module's check reads another's. The
/// tasks join in order; the first error in order is the block's.
pub(crate) fn typecheck0_statements<R: Rt, E: UserEvent>(
    ctx: &mut CompileCtx<R, E>,
    nodes: &mut [Node<R, E>],
    catches: &[usize],
    module: bool,
) -> Result<()> {
    let order: LPooled<Vec<usize>> = evaluation_order(nodes.len(), catches).collect();
    let mut slots: LPooled<Vec<Option<&mut Node<R, E>>>> =
        nodes.iter_mut().map(Some).collect();
    let is_static_module = |n: &Option<&mut Node<R, E>>| {
        n.as_deref().is_some_and(|n| match n.view() {
            NodeView::Module(m) => m.static_task().is_some(),
            _ => false,
        })
    };
    // a module's check sees no static sibling's undeclared impl, whether
    // it checks in a run or alone
    let siblings: LPooled<Vec<ModPath>> = slots
        .iter()
        .filter(|n| is_static_module(n))
        .filter_map(|n| module_path(n.as_deref()?))
        .collect();
    let mut at = 0;
    while at < order.len() {
        let run =
            order[at..].iter().take_while(|i| is_static_module(&slots[**i])).count();
        if run >= 2 && !RUNTIME_BIND.get() {
            let run_nodes = order[at..at + run]
                .iter()
                .map(|i| slots[*i].take().expect("an order visits each once"));
            typecheck0_modules(ctx, run_nodes, &siblings, module)?;
            at += run;
        } else {
            let n = slots[order[at]].take().expect("an order visits each once");
            let prev = ctx.env.hidden_impls.clone();
            if let NodeView::Module(m) = n.view()
                && m.static_task().is_some()
            {
                let own = m.scope.lexical.clone();
                ctx.env.hidden_impls = Arc::new(hide_siblings(&prev, &siblings, &own));
            }
            let r = n.typecheck0(ctx);
            ctx.env.hidden_impls = prev;
            statement_result(r, n, module)?;
            at += 1;
        }
    }
    Ok(())
}

/// The scope of a module statement.
fn module_path<R: Rt, E: UserEvent>(n: &Node<R, E>) -> Option<ModPath> {
    match n.view() {
        NodeView::Module(m) => Some(m.scope.lexical.clone()),
        _ => None,
    }
}

/// `hidden` and every sibling but `own`.
fn hide_siblings(
    hidden: &[ModPath],
    siblings: &[ModPath],
    own: &ModPath,
) -> Vec<ModPath> {
    hidden.iter().chain(siblings.iter().filter(|p| *p != own)).cloned().collect()
}

/// Check each module of a run in a compile task forked before any runs.
fn typecheck0_modules<'a, R: Rt, E: UserEvent>(
    ctx: &mut CompileCtx<R, E>,
    run: impl Iterator<Item = &'a mut Node<R, E>>,
    siblings: &[ModPath],
    module: bool,
) -> Result<()> {
    let mut run: LPooled<Vec<&mut Node<R, E>>> = run.collect();
    let mut work: LPooled<Vec<(&mut Node<R, E>, CompileCtx<R, E>, TaskId)>> =
        run.iter_mut().map(|n| (&mut **n, ctx.fork(), current_task())).collect();
    for (n, task, _) in work.iter_mut() {
        let own = module_path(n).expect("a run is of modules");
        task.env.hidden_impls =
            Arc::new(hide_siblings(&task.env.hidden_impls, siblings, &own));
    }
    check_in_tasks(ctx, work, module, |n, task| n.typecheck0(task))?;
    // an interface binding takes a sibling's definition only once the
    // run's tasks joined: each task was forked before the others checked
    for n in run.iter() {
        if let NodeView::Module(m) = n.view() {
            m.proxy_lambda_defs(ctx)
        }
    }
    Ok(())
}

/// Run a runtime bind `f` as a compile task of its own, in a settle
/// frame of its own, and settle what it deferred when it returns: no
/// statement boundary follows a bind at run time. A refused settle is a
/// compiler bug, logged. Binds in parallel branches run concurrently:
/// one that changes a cell the program's compile created is a race.
pub(crate) fn with_runtime_settles<R: Rt, E: UserEvent, T>(
    ctx: &mut CompileCtx<R, E>,
    f: impl FnOnce(&mut CompileCtx<R, E>) -> Result<T>,
) -> Result<T> {
    ctx.pending_settles.push(crate::SettleFrame::default());
    let names = ctx.pending_names.len();
    let _task = InTask::enter(new_task());
    let res = {
        let _restore = Restore::replace(&RUNTIME_BIND, true);
        f(ctx)
    };
    // a runtime bind elaborates: what it defers is no check's
    ctx.pending_names.truncate(names);
    let pending = ctx.pending_settles.pop().expect("runtime settle frame");
    let _ = PendingSettle::drain(&pending, &ctx.env, |spec, e| {
        let msg = format_compact!("a run-time bind's settle at {spec} refused: {e:#}");
        error::report_failure!(&msg);
        Ok(())
    });
    res
}

/// Defer `settle` to the check's settle of the current frame. A
/// definition's body defers nothing: its cells are its signature's,
/// settled by each call.
pub(crate) fn defer_settle<R: Rt, E: UserEvent>(
    ctx: &mut CompileCtx<R, E>,
    settle: impl FnOnce() -> PendingSettle,
) {
    if ctx.def_gate_depth == 0 {
        ctx.pending_settles.last_mut().expect("settle frame").push(settle())
    }
}

/// Defer a judgment that only reads types to the check's settle of the
/// current frame, a definition's body included: its open cells read as
/// any type, and a later instance's types are its definition's.
/// A map type written in `t`, which makes values of it out of others (a
/// cast's target, a type test), takes keys of distinct runtime forms, as
/// a literal's do: judged at the statement's settle. Every other map
/// value comes from a producer judged already.
pub(crate) fn defer_written_map_keys<R: Rt, E: UserEvent>(
    ctx: &mut CompileCtx<R, E>,
    t: &Type,
    spec: &Arc<Expr>,
) {
    if t.holds_map(&ctx.env) {
        let what = crate::Discerned::Keys;
        let judgment =
            PendingSettle::Discernible { typ: t.clone(), what, spec: spec.clone() };
        defer_judgment(ctx, judgment)
    }
}

pub(crate) fn defer_judgment<R: Rt, E: UserEvent>(
    ctx: &mut CompileCtx<R, E>,
    judgment: PendingSettle,
) {
    ctx.pending_settles.last_mut().expect("settle frame").push(judgment)
}

/// A statement's `typecheck1`, then the settles it deferred.
fn typecheck1_settled<R: Rt, E: UserEvent>(
    n: &mut Node<R, E>,
    ctx: &mut CompileCtx<R, E>,
) -> Result<()> {
    n.typecheck1(ctx)?;
    crate::drain_pending_settles(ctx)
}

/// Elaborate a body's statements (`typecheck1`, then each one's
/// settles) in [`evaluation_order`], each in a compile task forked from
/// the body's state before any runs: no statement's elaboration reads
/// another's. The tasks join in order; the first error in order is the
/// body's. A bind at run time elaborates them in order on its own
/// thread: an activation's body is small, and waking the pool for it
/// costs more than its work.
pub(crate) fn typecheck1_statements<R: Rt, E: UserEvent>(
    ctx: &mut CompileCtx<R, E>,
    nodes: &mut [Node<R, E>],
    catches: &[usize],
    module: bool,
) -> Result<()> {
    let order: LPooled<Vec<usize>> = evaluation_order(nodes.len(), catches).collect();
    if order.len() < 2 || RUNTIME_BIND.get() {
        return typecheck_in_order(ctx, nodes, catches, module, typecheck1_settled);
    }
    let mut slots: LPooled<Vec<Option<&mut Node<R, E>>>> =
        nodes.iter_mut().map(Some).collect();
    let work: LPooled<Vec<(&mut Node<R, E>, CompileCtx<R, E>, TaskId)>> = order
        .iter()
        .map(|i| {
            let n = slots[*i].take().expect("an order visits each once");
            (n, ctx.fork(), new_task())
        })
        .collect();
    check_in_tasks(ctx, work, module, typecheck1_settled)
}

impl<R: Rt, E: UserEvent> Block<R, E> {
    /// Whether a `catch` covers the block's value, its last child.
    pub(crate) fn value_is_caught(&self) -> bool {
        self.catches.first().is_some_and(|i| i + 1 < self.children.len())
    }

    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<'_, R, E>,
        buf: &mut &[u8],
    ) -> Result<Node<R, E>, PackError> {
        let module = bool::decode(buf)?;
        let spec = Arc::new(Expr::decode(buf)?);
        let children = decode_nodes(ctx, buf)?.into_boxed_slice();
        let catches = Vec::<usize>::decode(buf)?.into_boxed_slice();
        let runs = Runs::image_decode(buf)?;
        Ok(Node::new(Self {
            module,
            spec,
            lets: lets_of(&children),
            children,
            catches,
            resident: TagValue::phantom(),
            runs,
        }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Block<R, E> {
    fn for_each_child<'a>(&'a self, f: &mut dyn FnMut(&'a Node<R, E>)) {
        self.children.iter().for_each(f)
    }

    fn for_each_child_mut(&mut self, f: &mut dyn FnMut(&mut Node<R, E>)) {
        self.children.iter_mut().for_each(f)
    }

    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::Block, buf);
        self.module.encode(buf)?;
        self.spec.encode(buf)?;
        encode_nodes(&self.children, buf)?;
        image::slice_encode(&self.catches, buf)?;
        self.runs.image_encode(buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R, E>) -> &TagValue {
        if self.children.len() > 1 && ctx.fork_mode() != ParMode::Off {
            return self.update_forking(ctx);
        }
        if self.catches.is_empty() && self.lets.is_empty() {
            let res = self
                .children
                .iter_mut()
                .fold(TagValue::phantom_ref(), |_, n| n.update(ctx));
            return if self.module { TagValue::phantom_ref() } else { res };
        }
        if self.catches.is_empty() {
            let Some((last, rest)) = self.children.split_last_mut() else {
                return TagValue::phantom_ref();
            };
            for n in rest {
                n.update(ctx);
            }
            if self.module {
                last.update(ctx);
                self.take_back_reraised(ctx);
                return TagValue::phantom_ref();
            }
            let res = last.update(ctx).clone();
            self.take_back_reraised(ctx);
            return self.resident.set(res);
        }
        // the value is the last syntactic child's (absent if it is a catch)
        let last = self.children.len() - 1;
        let mut res: Option<TagValue> = None;
        for i in evaluation_order(self.children.len(), &self.catches) {
            let r = self.children[i].update(ctx);
            if i == last && self.catches.last() != Some(&last) {
                res = Some(r.clone());
            }
        }
        self.take_back_reraised(ctx);
        match res {
            Some(tv) if !self.module => self.resident.set(tv),
            _ if self.module => TagValue::phantom_ref(),
            _ => self.resident.ride(),
        }
    }

    fn typ(&self) -> &Type {
        self.children.last().map(|n| n.typ()).unwrap_or(Type::BOTTOM)
    }

    fn typecheck0(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
        typecheck0_statements(ctx, &mut self.children, &self.catches, self.module)
    }

    fn typecheck0_instance(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        types: &mut lambda::InstanceTypes,
    ) -> Result<()> {
        typecheck_in_order(
            ctx,
            &mut self.children,
            &self.catches,
            self.module,
            |n, ctx| n.typecheck0_instance(ctx, types),
        )
    }

    fn typecheck1(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
        typecheck1_statements(ctx, &mut self.children, &self.catches, self.module)
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn view(&self) -> NodeView<'_, R, E> {
        NodeView::Block(self)
    }

    fn emit_clif(&self, cx: &mut BodyCx) -> Result<CompiledExpr> {
        if self.module {
            // a module's binds must publish to the persistent env; fused
            // into a parent region they would become SSA locals
            bail!("emit_clif: module statement is structure, not computation")
        }
        emit_block_node(cx, &self.children)
    }

    fn fuse(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<Option<Node<R, E>>> {
        fusion::fuse_each(ctx, self.children.iter_mut(), |n, _, ctx| fuse(n, ctx))?;
        Ok(None)
    }
}

#[derive(Debug)]
pub struct StringInterpolate<R: Rt, E: UserEvent> {
    /// set by `sleep()`, taken by the next update
    slept: WakeBit,
    pub(crate) spec: Arc<Expr>,
    pub typ: Type,
    pub(crate) typs: Box<[Type]>,
    pub args: Box<[Node<R, E>]>,
    resident: TagValue,
    pub(crate) fork: ForkSite,
}

impl<R: Rt, E: UserEvent> StringInterpolate<R, E> {
    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<'_, R, E>,
        buf: &mut &[u8],
    ) -> Result<Node<R, E>, PackError> {
        let spec = Arc::new(Expr::decode(buf)?);
        let typ = Type::decode(buf)?;
        let typs = Vec::<Type>::decode(buf)?.into_boxed_slice();
        let args = decode_nodes(ctx, buf)?.into_boxed_slice();
        Ok(Node::new(Self {
            spec,
            typ,
            typs,
            args,
            resident: TagValue::phantom(),
            slept: WakeBit::default(),
            fork: ForkSite::image_decode(buf)?,
        }))
    }

    pub(crate) fn compile(
        ctx: &mut CompileCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: Arc<Expr>,
        scope: &Scope,
        top_id: ExprId,
        args: &[Expr],
    ) -> Result<Node<R, E>> {
        let args: Box<[Node<R, E>]> = args
            .iter()
            .map(|e| compile(ctx, flags, e.clone(), scope, top_id))
            .collect::<Result<_>>()?;
        let typs = args.iter().map(|n| n.typ().clone()).collect();
        let typ = Type::Primitive(Typ::String.into());
        Ok(Node::new(Self {
            spec,
            typ,
            typs,
            args,
            resident: TagValue::phantom(),
            slept: WakeBit::default(),
            fork: Default::default(),
        }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for StringInterpolate<R, E> {
    fn for_each_child<'a>(&'a self, f: &mut dyn FnMut(&'a Node<R, E>)) {
        self.args.iter().for_each(f)
    }

    fn for_each_child_mut(&mut self, f: &mut dyn FnMut(&mut Node<R, E>)) {
        self.args.iter_mut().for_each(f)
    }

    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::StringInterpolate, buf);
        self.spec.encode(buf)?;
        self.typ.encode(buf)?;
        image::slice_encode(&self.typs, buf)?;
        encode_nodes(&self.args, buf)?;
        self.fork.image_encode(buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R, E>) -> &TagValue {
        use std::fmt::Write;
        // rendered under the value-hook loan so a core `Display` impl on
        // an abstract part applies (`coretraits::with_display_hooks`)
        let (tag, prods) = gather(ctx, &mut self.args, &mut self.fork);
        dense_gate!(self, tag, tag.is_bottom());
        let mut buf: LPooled<String> = LPooled::take();
        coretraits::with_display_hooks(ctx, |env| {
            for (typ, tv) in self.typs.iter().zip(prods.iter()) {
                tv.with_value(|v| match v {
                    Value::String(s) => write!(buf, "{s}"),
                    v => write!(buf, "{}", TVal { env, typ, v }),
                })
                .unwrap()
            }
        });
        self.resident.set(TagValue::tagged(Value::String(buf.as_str().into()), tag))
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn typ(&self) -> &Type {
        &self.typ
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.slept.set();
        for n in &mut self.args {
            n.sleep(ctx);
        }
    }

    fn fuse(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<Option<Node<R, E>>> {
        fusion::fuse_parts(self.args.iter_mut(), ctx)
    }

    typed_by_row!(children);

    fn typecheck1(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
        for (i, a) in self.args.iter_mut().enumerate() {
            wrap!(a, a.typecheck1(ctx))?;
            // a cell still open at tc0 is bound by now
            self.typs[i] = part_type(a.typ());
        }
        Ok(())
    }

    fn view(&self) -> NodeView<'_, R, E> {
        NodeView::StringInterpolate(self)
    }

    fn emit_clif(&self, cx: &mut BodyCx) -> Result<CompiledExpr> {
        emit_string_interpolate_node(cx, &self.args, &self.typs)
    }
}

/// A part's type for the typed printer: dereferenced at the top, `Any`
/// for an open cell.
fn part_type(t: &Type) -> Type {
    t.with_deref(|t| match t {
        None => Type::Any,
        Some(t) => t.clone(),
    })
}

#[derive(Debug)]
pub struct Connect<R: Rt, E: UserEvent> {
    pub(crate) spec: Arc<Expr>,
    pub(super) node: Node<R, E>,
    pub(crate) id: BindId,
}

/// Where a connect's target name is written, past a `*`.
fn target_pos(spec: &Expr) -> crate::SourcePosition {
    match &spec.kind {
        ExprKind::Connect { at, .. } => at.get().unwrap_or(spec.pos),
        _ => spec.pos,
    }
}

impl<R: Rt, E: UserEvent> Connect<R, E> {
    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<'_, R, E>,
        buf: &mut &[u8],
    ) -> Result<Node<R, E>, PackError> {
        let spec = Arc::new(Expr::decode(buf)?);
        let node = decode_node(ctx, buf)?;
        let id = BindId::decode(buf)?;
        Ok(Node::new(Self { spec, node, id }))
    }

    pub(crate) fn compile(
        ctx: &mut CompileCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: Arc<Expr>,
        scope: &Scope,
        top_id: ExprId,
        name: &ModPath,
        value: &Expr,
    ) -> Result<Node<R, E>> {
        let id =
            match ctx.env.lookup_bind(&scope.lexical, name).map_err(|e| e.at(&spec))? {
                None => bailat!(spec, "{name} is undefined"),
                Some((_, b)) => b.id,
            };
        // a `<-` target is never a static call target
        ctx.mark_connect_target(id).map_err(|e| e.at(&spec))?;
        if ctx.env.ide.is_lsp() {
            ctx.env.push_reference(ReferenceSite {
                pos: target_pos(&spec),
                ori: spec.ori.clone(),
                name: name.clone(),
                bind_id: id,
            });
        }
        let node = compile(ctx, flags, value.clone(), scope, top_id)?;
        Ok(Node::new(Self { spec, node, id }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Connect<R, E> {
    fn for_each_child<'a>(&'a self, f: &mut dyn FnMut(&'a Node<R, E>)) {
        f(&self.node)
    }

    fn for_each_child_mut(&mut self, f: &mut dyn FnMut(&mut Node<R, E>)) {
        f(&mut self.node)
    }

    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::Connect, buf);
        self.spec.encode(buf)?;
        self.node.image_encode(buf)?;
        self.id.encode(buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R, E>) -> &TagValue {
        // only a fired RHS writes
        let tv = self.node.update(ctx);
        if tv.is_fired() {
            ctx.rt.set_var(self.id, tv.value_cloned())
        }
        TagValue::phantom_ref()
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn typ(&self) -> &Type {
        Type::BOTTOM
    }

    fn fuse(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<Option<Node<R, E>>> {
        fusion::fuse_parts([&mut self.node], ctx)
    }

    // The target's type is its binding's row: the write was judged in
    // the check.
    typed_by_row!(source);

    fn view(&self) -> NodeView<'_, R, E> {
        NodeView::Connect(self)
    }

    fn emit_clif(&self, _cx: &mut BodyCx) -> Result<CompiledExpr> {
        Err(anyhow!("emit_clif: connect is an effect — node-walks"))
    }
}

/// A written `<-` whose value `bind` cannot hold: the target, its type,
/// and the declaration that would hold both.
fn write_mismatch(bind: &env::Bind, written: &Type, at: &Arc<Origin>) -> anyhow::Error {
    let deref = |t: &Type| t.deref_cloned().unwrap_or_else(|| t.clone());
    let (held, written) = (deref(&bind.typ), deref(written));
    let both = Type::Set(Arc::from_iter([held.clone(), written.clone()])).normalize();
    let inferred = match &bind.typ {
        Type::TVar(tv) if tv.bottom_fed() => ", inferred from an earlier use,",
        _ => "",
    };
    let (name, line, col) = (&bind.name, bind.pos.line, bind.pos.column);
    // a type variable is declared, and no declaration widens it; nor does
    // one widen what holds a function, as two function types share a
    // runtime form
    fn holds_fn(t: &Type) -> bool {
        crate::stack::ensure_sufficient(|| match t {
            Type::Fn(_) => true,
            Type::TVar(tv) => tv.binding().is_some_and(|b| holds_fn(&b)),
            t => {
                let mut any = false;
                t.for_each_child(&mut |c| any |= holds_fn(c));
                any
            }
        })
    }
    if matches!(held, Type::TVar(_)) || holds_fn(&held) || holds_fn(&written) {
        return format_with_flags(
            PrintFlag::DerefTVars | PrintFlag::ReplacePrims,
            || anyhow!("{name} is {held} and cannot hold {written}"),
        );
    }
    let file = match bind.ori == *at {
        true => format_compact!(""),
        false => format_compact!(" {}", bind.ori),
    };
    // a name a destructuring let binds is declared by the pattern's type
    let declare = match bind.binder {
        Some(env::Binder::LetPattern) => format_compact!(
            "annotate the let's pattern type with {name}'s position as {both}"
        ),
        _ => format_compact!("declare {name}: {both} where it is bound"),
    };
    format_with_flags(PrintFlag::DerefTVars | PrintFlag::ReplacePrims, || {
        anyhow!(
            "{name} is {held}{inferred} and cannot hold {written}; {declare} \
             (line {line}, column {col}{file})"
        )
    })
}

/// Where a write through a reference lands: a bound variable, or a
/// place inside one (`design/place_references.md`).
#[derive(Debug, Clone, PartialEq)]
pub enum WriteTarget {
    Bind(BindId),
    Place(BindId, place::Path),
}

#[derive(Debug)]
pub struct ConnectDeref<R: Rt, E: UserEvent> {
    pub(crate) spec: Arc<Expr>,
    pub(super) rhs: Node<R, E>,
    pub(crate) src_id: BindId,
    /// The reference's cell; where it writes is resolved at each write,
    /// so a moving place writes where its key names then.
    pub(super) cell: Option<BindId>,
    pub(super) top_id: ExprId,
    /// The RHS fired before the reference arrived: its first target lands
    /// it. A reference that arrived naming no place (a key that is bottom)
    /// drops the write, as any operation drops a bottom input.
    pending: bool,
    slept: WakeBit,
}

impl<R: Rt, E: UserEvent> ConnectDeref<R, E> {
    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<'_, R, E>,
        buf: &mut &[u8],
    ) -> Result<Node<R, E>, PackError> {
        let spec = Arc::new(Expr::decode(buf)?);
        let rhs = decode_node(ctx, buf)?;
        let src_id = BindId::decode(buf)?;
        let top_id = ExprId::decode(buf)?;
        ctx.record_ref(src_id, top_id);
        Ok(Node::new(Self {
            spec,
            rhs,
            src_id,
            cell: None,
            top_id,
            pending: false,
            slept: WakeBit::default(),
        }))
    }

    pub(crate) fn compile(
        ctx: &mut CompileCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: Arc<Expr>,
        scope: &Scope,
        top_id: ExprId,
        name: &ModPath,
        value: &Expr,
    ) -> Result<Node<R, E>> {
        let src_id =
            match ctx.env.lookup_bind(&scope.lexical, name).map_err(|e| e.at(&spec))? {
                None => bailat!(spec, "{name} is undefined"),
                Some((_, b)) => b.id,
            };
        if ctx.env.ide.is_lsp() {
            ctx.env.push_reference(ReferenceSite {
                pos: target_pos(&spec),
                ori: spec.ori.clone(),
                name: name.clone(),
                bind_id: src_id,
            });
        }
        ctx.record_ref(src_id, top_id);
        let rhs = compile(ctx, flags, value.clone(), scope, top_id)?;
        Ok(Node::new(Self {
            spec,
            rhs,
            src_id,
            cell: None,
            top_id,
            pending: false,
            slept: WakeBit::default(),
        }))
    }

    /// The cell a delivered reference value names; where a write lands
    /// is resolved from it at the write (`ExecCtx::write_target`).
    fn cell(tv: &TagValue) -> Option<BindId> {
        tv.with_value(bind::ref_cell)
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for ConnectDeref<R, E> {
    fn for_each_child<'a>(&'a self, f: &mut dyn FnMut(&'a Node<R, E>)) {
        f(&self.rhs)
    }

    fn for_each_child_mut(&mut self, f: &mut dyn FnMut(&mut Node<R, E>)) {
        f(&mut self.rhs)
    }

    /// `target` is resolved at update: a resolved one is runtime state.
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        if self.cell.is_some() || self.pending {
            return Err(PackError::Application(image::NOT_QUIESCENT));
        }
        put_tag(NodeTag::ConnectDeref, buf);
        self.spec.encode(buf)?;
        self.rhs.image_encode(buf)?;
        self.src_id.encode(buf)?;
        self.top_id.encode(buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R, E>) -> &TagValue {
        // a fired RHS writes; a write before the reference arrived lands at
        // its first target; a bottom RHS or address never writes
        let woke = self.slept.take();
        let (rhs_fired, rhs_val) = {
            let tv = self.rhs.update(ctx);
            let t = tv.tag();
            (t.is_fired(), if t.is_bottom() { None } else { Some(tv.value_cloned()) })
        };
        if let crate::branch::RtView::Fork(f) = &ctx.rt {
            f.note_read(self.src_id);
        }
        // a reference that arrived naming no place (a place whose address
        // is bottom) has nowhere to write
        let mut addressless = false;
        if let Some(tv) = ctx.event.variables.get(&self.src_id) {
            self.cell = Self::cell(tv);
            addressless = self.cell.is_none();
        } else if self.cell.is_none() || woke {
            // an instance created after the reference value was
            // delivered, or woken after it moved, finds it standing
            if let Some(read) = read_var(ctx, &self.src_id) {
                let tv = match read {
                    VarRead::Delivered(tv) | VarRead::Standing(tv) => tv,
                };
                self.cell = Self::cell(tv);
                addressless = self.cell.is_none();
            }
        }
        self.pending = !addressless && (self.pending || rhs_fired);
        if self.pending
            && let Some(v) = rhs_val
            && let Some(cell) = self.cell
        {
            match ctx.write_target(cell) {
                WriteTarget::Bind(id) => ctx.rt.set_var(id, v),
                WriteTarget::Place(root, path) => {
                    ctx.rt.patch_var(root, path, v, Some(self.spec.clone()))
                }
            }
            self.pending = false;
        }
        TagValue::phantom_ref()
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn typ(&self) -> &Type {
        Type::BOTTOM
    }

    fn refs(&self, refs: &mut Refs) {
        refs.read(self.src_id);
        self.rhs.refs(refs)
    }

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        ctx.unref_var(self.src_id, self.top_id);
        self.rhs.delete(ctx)
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.slept.set();
        self.rhs.sleep(ctx);
    }

    // The target's type is its binding's row: the write was judged in
    // the check.
    typed_by_row!(source);

    fn view(&self) -> NodeView<'_, R, E> {
        NodeView::ConnectDeref(self)
    }
}

#[derive(Debug)]
pub struct TypeCast<R: Rt, E: UserEvent> {
    slept: WakeBit,
    pub(crate) spec: Arc<Expr>,
    pub typ: Type,
    pub target: Type,
    pub n: Node<R, E>,
    /// The source can hold a reference: every value casts to the cast
    /// error. Known only once the source's type is, so the check refuses
    /// what it can see and an instance of a generic body fails here.
    src_ref: bool,
    resident: TagValue,
}

impl<R: Rt, E: UserEvent> TypeCast<R, E> {
    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<'_, R, E>,
        buf: &mut &[u8],
    ) -> Result<Node<R, E>, PackError> {
        let spec = Arc::new(Expr::decode(buf)?);
        let typ = Type::decode(buf)?;
        let target = Type::decode(buf)?;
        let n = decode_node(ctx, buf)?;
        let src_ref = n.typ().holds_ref(&ctx.env);
        Ok(Node::new(Self {
            slept: WakeBit::default(),
            spec,
            typ,
            target,
            n,
            src_ref,
            resident: TagValue::phantom(),
        }))
    }

    pub(crate) fn compile(
        ctx: &mut CompileCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: Arc<Expr>,
        scope: &Scope,
        top_id: ExprId,
        expr: &Expr,
        typ: &Type,
    ) -> Result<Node<R, E>> {
        let n = compile(ctx, flags, expr.clone(), scope, top_id)?;
        let target = typ.scope_refs(&scope.lexical);
        target.check_cast(&ctx.env).at(&spec)?;
        let typ = Type::union(&ctx.env, &[&target, &CAST_ERR])?;
        Ok(Node::new(Self {
            slept: WakeBit::default(),
            spec,
            typ,
            target,
            n,
            src_ref: false,
            resident: TagValue::phantom(),
        }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for TypeCast<R, E> {
    fn for_each_child<'a>(&'a self, f: &mut dyn FnMut(&'a Node<R, E>)) {
        f(&self.n)
    }

    fn for_each_child_mut(&mut self, f: &mut dyn FnMut(&mut Node<R, E>)) {
        f(&mut self.n)
    }

    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::TypeCast, buf);
        self.spec.encode(buf)?;
        self.typ.encode(buf)?;
        self.target.encode(buf)?;
        self.n.image_encode(buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R, E>) -> &TagValue {
        let tv = self.n.update(ctx);
        let tag = tv.tag();
        dense_gate!(self, tag, tag.is_bottom());
        let v = tv.value_cloned();
        let v = if self.src_ref {
            errf!(CAST_ERR_TAG, "can't cast a reference")
        } else {
            let (target, src) = (&self.target, self.n.typ());
            coretraits::with_display_hooks(ctx, |env| target.cast_from(env, src, v))
        };
        self.resident.set(TagValue::tagged(v, tag))
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn typ(&self) -> &Type {
        &self.typ
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.slept.set();
        self.n.sleep(ctx);
    }

    fn fuse(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<Option<Node<R, E>>> {
        fusion::fuse_parts([&mut self.n], ctx)
    }

    fn typecheck0(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
        self.typecheck0_with(ctx, &mut |n, ctx| n.typecheck0(ctx))?;
        if self.n.typ().holds_ref(&ctx.env) {
            bailat!(self.spec, "can't cast a reference")
        }
        if self.target.holds_uncastable(&ctx.env) {
            bailat!(
                self.spec,
                "can't cast to {}: it holds an abstract type with no Cast implementation",
                self.target
            )
        }
        defer_written_map_keys(ctx, &self.target, &self.spec);
        Ok(())
    }

    fn typecheck0_instance(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        types: &mut lambda::InstanceTypes,
    ) -> Result<()> {
        self.typecheck0_with(ctx, &mut |n, ctx| n.typecheck0_instance(ctx, types))
    }

    fn typecheck1(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
        wrap!(self.n, self.n.typecheck1(ctx))?;
        self.src_ref = self.n.typ().holds_ref(&ctx.env);
        Ok(())
    }

    fn view(&self) -> NodeView<'_, R, E> {
        NodeView::TypeCast(self)
    }

    fn emit_clif(&self, cx: &mut BodyCx) -> Result<CompiledExpr> {
        if self.src_ref {
            bail!("a cast of a reference does not emit CLIF")
        }
        emit_cast_node(cx, &self.n, &self.target, self.spec.id)
    }
}

/// `never<T>(args…)`: produces nothing, ever; its arguments stay live
/// and are consumed. Typed `T` where given, bottom otherwise — known at
/// compile time, so a select unions it away and a field or argument
/// carries the declared shape.
#[derive(Debug)]
pub struct Never<R: Rt, E: UserEvent> {
    pub(crate) spec: Arc<Expr>,
    pub typ: Type,
    pub n: Box<[Node<R, E>]>,
}

impl<R: Rt, E: UserEvent> Never<R, E> {
    pub(crate) fn compile(
        ctx: &mut CompileCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: Arc<Expr>,
        scope: &Scope,
        top_id: ExprId,
        typ: &Option<Arc<Type>>,
        args: &[Expr],
    ) -> Result<Node<R, E>> {
        let n = args
            .iter()
            .map(|e| compile(ctx, flags, e.clone(), scope, top_id))
            .collect::<Result<Box<[_]>>>()?;
        let typ = match typ {
            Some(t) => {
                let t = t.scope_refs(&scope.lexical).rewrite_trait_args(&ctx.env)?;
                lambda::body_annotation(ctx, &t);
                t
            }
            None => Type::Bottom,
        };
        Ok(Node::new(Self { spec, typ, n }))
    }
}

impl<R: Rt, E: UserEvent> Never<R, E> {
    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<'_, R, E>,
        buf: &mut &[u8],
    ) -> Result<Node<R, E>, PackError> {
        let spec = Arc::new(Expr::decode(buf)?);
        let typ = Type::decode(buf)?;
        let n = decode_nodes(ctx, buf)?.into_boxed_slice();
        Ok(Node::new(Self { spec, typ, n }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Never<R, E> {
    fn for_each_child<'a>(&'a self, f: &mut dyn FnMut(&'a Node<R, E>)) {
        self.n.iter().for_each(f)
    }

    fn for_each_child_mut(&mut self, f: &mut dyn FnMut(&mut Node<R, E>)) {
        self.n.iter_mut().for_each(f)
    }

    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::Never, buf);
        self.spec.encode(buf)?;
        self.typ.encode(buf)?;
        encode_nodes(&self.n, buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R, E>) -> &TagValue {
        for n in self.n.iter_mut() {
            n.update(ctx);
        }
        TagValue::phantom_ref()
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn typ(&self) -> &Type {
        &self.typ
    }

    fn fuse(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<Option<Node<R, E>>> {
        fusion::fuse_parts(self.n.iter_mut(), ctx)
    }

    fn typecheck0(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
        defer_unresolved_names(ctx, &self.typ, &self.spec);
        // each application of a definition T names holds its parameter
        // bounds, as where a contains expands it
        fn applications(t: &Type, env: &env::Env, r: &mut Result<()>) {
            if r.is_err() {
                return;
            }
            if let Type::Ref(tr) = t
                && tr.resolve_in(env).is_some()
            {
                *r = t.lookup_ref(env).map(|_| ());
            }
            crate::stack::ensure_sufficient(|| {
                t.for_each_child(&mut |c| applications(c, env, r))
            })
        }
        let mut r = Ok(());
        applications(&self.typ, &ctx.env, &mut r);
        wrap!(self, r)?;
        self.typecheck0_with(ctx, &mut |n, ctx| n.typecheck0(ctx))
    }

    fn typecheck0_instance(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        types: &mut lambda::InstanceTypes,
    ) -> Result<()> {
        self.typecheck0_with(ctx, &mut |n, ctx| n.typecheck0_instance(ctx, types))
    }

    fn view(&self) -> NodeView<'_, R, E> {
        NodeView::Never(self)
    }
}

#[derive(Debug)]
pub struct Any<R: Rt, E: UserEvent> {
    pub(crate) spec: Arc<Expr>,
    pub typ: Type,
    pub n: Box<[Node<R, E>]>,
    resident: TagValue,
    /// The child whose production the resident is.
    from: Option<usize>,
    slept: WakeBit,
}

impl<R: Rt, E: UserEvent> Any<R, E> {
    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<'_, R, E>,
        buf: &mut &[u8],
    ) -> Result<Node<R, E>, PackError> {
        let spec = Arc::new(Expr::decode(buf)?);
        let typ = Type::decode(buf)?;
        let n = decode_nodes(ctx, buf)?.into_boxed_slice();
        Ok(Node::new(Self {
            spec,
            typ,
            n,
            resident: TagValue::phantom(),
            from: None,
            slept: WakeBit::default(),
        }))
    }

    pub(crate) fn compile(
        ctx: &mut CompileCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: Arc<Expr>,
        scope: &Scope,
        top_id: ExprId,
        args: &[Expr],
    ) -> Result<Node<R, E>> {
        let n = args
            .iter()
            .map(|e| compile(ctx, flags, e.clone(), scope, top_id))
            .collect::<Result<Box<[_]>>>()?;
        Ok(Node::new(Self {
            spec,
            typ: Type::empty_tvar(),
            n,
            resident: TagValue::phantom(),
            from: None,
            slept: WakeBit::default(),
        }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Any<R, E> {
    fn for_each_child<'a>(&'a self, f: &mut dyn FnMut(&'a Node<R, E>)) {
        self.n.iter().for_each(f)
    }

    fn for_each_child_mut(&mut self, f: &mut dyn FnMut(&mut Node<R, E>)) {
        self.n.iter_mut().for_each(f)
    }

    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::Any, buf);
        self.spec.encode(buf)?;
        self.typ.encode(buf)?;
        encode_nodes(&self.n, buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R, E>) -> &TagValue {
        // the first triggering value-bearing production wins; a
        // triggering bottom never beats one (`any(risky?, default)`); at a
        // wake, the child the resident came from standing bottom now
        // stands the resident bottom, as its missed fire would have
        let woke = self.slept.take();
        let mut winner: Option<(usize, TagValue)> = None;
        let mut bottomed = false;
        let mut lost = false;
        for (i, s) in self.n.iter_mut().enumerate() {
            let tv = s.update(ctx);
            let tag = tv.tag();
            if tag.triggers() {
                if tag.is_bottom() {
                    bottomed = true;
                } else if winner.is_none() {
                    winner = Some((i, tv.clone()));
                }
            } else if woke && tag.is_bottom() && self.from == Some(i) {
                lost = true;
            }
        }
        match winner {
            Some((i, tv)) => {
                self.from = Some(i);
                self.resident.set(tv)
            }
            None if bottomed => self.resident.set_bottom(true),
            None if lost => self.resident.set_bottom(false),
            None => self.resident.ride(),
        }
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn typ(&self) -> &Type {
        &self.typ
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.slept.set();
        self.n.iter_mut().for_each(|n| n.sleep(ctx))
    }

    fn fuse(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<Option<Node<R, E>>> {
        fusion::fuse_parts(self.n.iter_mut(), ctx)
    }

    typed_by_row!();

    fn view(&self) -> NodeView<'_, R, E> {
        NodeView::Any(self)
    }
}

/// How `~` answers a trigger that finds its RHS absent.
#[derive(Debug)]
enum Banking {
    /// `~!`: the trigger produces bottom and banks nothing.
    Strict,
    /// `~`: a trigger that finds the RHS absent is banked, and what is
    /// owed is paid through the private `id` at the RHS's value.
    Debt { owed: u64, each: Owed, id: BindId },
}

/// What a banking sample owes the triggers that found its argument absent.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Owed {
    /// One payment however many fired: `~`.
    One,
    /// A payment per trigger: the `seqq` request queue's
    /// (`BinOp::BankedSample`).
    Each,
}

#[derive(Debug)]
pub struct Sample<R: Rt, E: UserEvent> {
    pub(crate) spec: Arc<Expr>,
    banking: Banking,
    pub typ: Type,
    top_id: ExprId,
    pub trigger: Node<R, E>,
    pub arg: Held<R, E>,
    resident: TagValue,
}

impl<R: Rt, E: UserEvent> Sample<R, E> {
    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<'_, R, E>,
        buf: &mut &[u8],
    ) -> Result<Node<R, E>, PackError> {
        let spec = Arc::new(Expr::decode(buf)?);
        let typ = Type::decode(buf)?;
        let top_id = ExprId::decode(buf)?;
        let each = match bool::decode(buf)? {
            true => Owed::Each,
            false => Owed::One,
        };
        let banking = match Option::<BindId>::decode(buf)? {
            None => Banking::Strict,
            Some(id) => {
                ctx.record_ref(id, top_id);
                Banking::Debt { owed: 0, each, id }
            }
        };
        let trigger = decode_node(ctx, buf)?;
        let arg = Held::image_decode(ctx, buf)?;
        Ok(Node::new(Self {
            spec,
            banking,
            typ,
            top_id,
            trigger,
            arg,
            resident: TagValue::phantom(),
        }))
    }

    pub(crate) fn compile(
        ctx: &mut CompileCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: Arc<Expr>,
        scope: &Scope,
        top_id: ExprId,
        lhs: &Arc<Expr>,
        rhs: &Arc<Expr>,
        owed: Option<Owed>,
    ) -> Result<Node<R, E>> {
        let banking = match owed {
            None => Banking::Strict,
            Some(each) => {
                let id = BindId::new();
                ctx.record_ref(id, top_id);
                Banking::Debt { owed: 0, each, id }
            }
        };
        let trigger = compile(ctx, flags, (**lhs).clone(), scope, top_id)?;
        let arg = Held::new(compile(ctx, flags, (**rhs).clone(), scope, top_id)?);
        let typ = arg.node.typ().clone();
        Ok(Node::new(Self {
            banking,
            top_id,
            spec,
            typ,
            trigger,
            arg,
            resident: TagValue::phantom(),
        }))
    }

    /// The debt's payment variable; `None` under `~!`.
    fn debt_id(&self) -> Option<BindId> {
        match &self.banking {
            Banking::Strict => None,
            Banking::Debt { id, .. } => Some(*id),
        }
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Sample<R, E> {
    fn for_each_child<'a>(&'a self, f: &mut dyn FnMut(&'a Node<R, E>)) {
        f(&self.trigger);
        f(&self.arg.node)
    }

    fn for_each_child_mut(&mut self, f: &mut dyn FnMut(&mut Node<R, E>)) {
        f(&mut self.trigger);
        f(&mut self.arg.node)
    }

    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::Sample, buf);
        self.spec.encode(buf)?;
        self.typ.encode(buf)?;
        self.top_id.encode(buf)?;
        matches!(self.banking, Banking::Debt { each: Owed::Each, .. }).encode(buf)?;
        self.debt_id().encode(buf)?;
        self.trigger.image_encode(buf)?;
        self.arg.image_encode(buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R, E>) -> &TagValue {
        // CR claude for eric: [bug/low] A bottom trigger is not `is_fired`, so both `~`
        // and `~!` ride their resident while the trigger is bottom. The sample keeps
        // showing its last value: in the probe (7, 2) fires while `a / z` is bottom.
        // design/dense_delivery.md R3 has a consumed bottom bottom the production. Its
        // designated rides are the select scrutinee, a guard's truth and `~`'s
        // argument, not its trigger, and a bottom scrutinee bottoms a select. Decide
        // one way. Either bottom the sample when the trigger is bottom
        // (set_bottom(t.tag().triggers()), the debt untouched), or name the trigger as
        // a fourth designated ride in R3 and CLAUDE.md. Pin it either way. probe:
        // design/review-2026-10-19/repro/x-engine-firing-sample-bottom-trigger.gx
        // (x-engine-firing-04)
        // 2026-10-24 claude: Eric ruled (2026-10-24): try it: a sample goes bottom with
        // its trigger (dense_delivery R1/R3), and see how much it breaks.
        // 2026-10-25 claude: Tried (bottom the sample when its trigger is bottom, debt
        // kept): 48 fixtures fail, 196 runs. The cause is one idiom: an event written as
        // a select with never() arms (select step { 2 => null, _ => never() }) stands
        // bottom between its fires, and `t ~ v` over it is how a program samples on it;
        // with the change the sample is bottom at every cycle but the fire, so a tuple or
        // select over it never produces (lang::byref::place_bottom_key hangs on (t3 ~ a,
        // t5 ~ a, ..)). The seq lowering samples its pc and step events the same way,
        // which is most of the failures (seq, seqq, seq_errors, seq_abort). Reverted; the
        // sample rides a bottom trigger as before. The rule wants a decision on that
        // idiom: either a bottom trigger keeps the ride (named as R3's fourth designated
        // ride), or events are written some other way than never() arms. Readdressed to
        // eric.
        let fired = self.trigger.update(ctx).tag().is_fired();
        self.arg.update(ctx);
        let (triggered, id) = match &mut self.banking {
            Banking::Strict => {
                return match (fired, self.arg.value.as_ref(), self.arg.tag.is_bottom()) {
                    (true, Some(v), false) => {
                        self.resident.set(TagValue::fired(v.clone()))
                    }
                    (true, _, _) => self.resident.set_bottom(true),
                    (false, _, _) => self.resident.ride(),
                };
            }
            Banking::Debt { owed, each, id } => match (fired, each) {
                (true, Owed::One) => {
                    *owed = 1;
                    (owed, *id)
                }
                (true, Owed::Each) => {
                    *owed += 1;
                    (owed, *id)
                }
                (false, _) => (owed, *id),
            },
        };
        let var = ctx.event.variables.get(&id).cloned();
        // a banked trigger is answered from the held arg this cycle; the
        // rest of the debt is paid through `id`
        let answered = match &self.arg.value {
            Some(v) if *triggered > 0 && var.is_none() => {
                *triggered -= 1;
                Some(v)
            }
            _ => None,
        };
        if let Some(v) = &self.arg.value
            && !self.arg.tag.is_bottom()
        {
            for _ in 0..mem::take(triggered) {
                ctx.rt.set_var(id, v.clone());
            }
        }
        match (answered, var) {
            (Some(_), _) if self.arg.tag.is_bottom() => self.resident.set_bottom(true),
            // paid in a cycle the trigger did not fire: the argument's
            // arrival is the event
            (Some(v), _) => self.resident.set(TagValue::fired(v.clone())),
            (None, Some(tv)) => self.resident.set(tv),
            (None, None) => self.resident.ride(),
        }
    }

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        if let Some(id) = self.debt_id() {
            ctx.release_var(id, self.top_id);
        }
        self.arg.node.delete(ctx);
        self.trigger.delete(ctx);
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.arg.sleep(ctx);
        self.trigger.sleep(ctx);
    }

    fn fuse(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<Option<Node<R, E>>> {
        fusion::fuse_parts([&mut self.trigger, &mut self.arg.node], ctx)
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn typ(&self) -> &Type {
        &self.typ
    }

    fn refs(&self, refs: &mut Refs) {
        if let Some(id) = self.debt_id() {
            refs.read(id);
        }
        refs.banked += 1;
        self.arg.node.refs(refs);
        refs.banked -= 1;
        self.trigger.refs(refs);
    }

    typed_by_row!(children);

    fn view(&self) -> NodeView<'_, R, E> {
        NodeView::Sample(self)
    }
}

impl<R: Rt, E: UserEvent> ExplicitParens<R, E> {
    fn typecheck0_with(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        child: &mut Child<'_, R, E>,
    ) -> Result<()> {
        wrap!(self.n, child(&mut self.n, ctx))
    }
}

impl<R: Rt, E: UserEvent> StringInterpolate<R, E> {
    fn typecheck0_with(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        child: &mut Child<'_, R, E>,
    ) -> Result<()> {
        for (i, a) in self.args.iter_mut().enumerate() {
            wrap!(a, child(a, ctx))?;
            self.typs[i] = part_type(a.typ());
        }
        Ok(())
    }
}

impl<R: Rt, E: UserEvent> Connect<R, E> {
    fn typecheck0_with(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        child: &mut Child<'_, R, E>,
        check: bool,
    ) -> Result<()> {
        wrap!(self.node, child(&mut self.node, ctx))?;
        if !check {
            return Ok(());
        }
        let written = &ctx.monomorphic(self.node.typ());
        let bind = match ctx.env.by_id.get(&self.id) {
            None => bail!("BUG missing bind {:?}", self.id),
            Some(bind) => bind,
        };
        // a binding an earlier compile generalized holds what is written
        // as its every instance: a copy of its type, the variables rigid
        let generic = ctx.env.poly_binds.contains(&self.id);
        let held = match generic {
            true => ctx.monomorphic(&bind.typ),
            false => bind.typ.clone(),
        };
        let mut rigid: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
        if generic {
            held.collect_tvars(&mut rigid);
        }
        let _gates: LPooled<Vec<_>> = rigid.values().map(|tv| tv.open_rigid()).collect();
        wrap!(
            self,
            held.check_contains(&ctx.env, written).map_err(|e| {
                match e.downcast_ref::<TypeMismatch>() {
                    Some(_) if generic => format_with_flags(
                        PrintFlag::DerefTVars | PrintFlag::ReplacePrims,
                        || {
                            anyhow!(
                                "{} is {held}, made generic by an earlier compile, so \
                                 what is written to it must be as general, and {written} \
                                 is not",
                                bind.name
                            )
                        },
                    ),
                    // a write a lowering built has no declaration to point at
                    Some(_) if !crate::is_lowered(&bind.name) => {
                        write_mismatch(bind, written, &self.spec.ori)
                    }
                    _ => e,
                }
            })
        )?;
        // as general as the binding: the written value may not carry one of
        // its variables into the environment, as a function's parameter would
        let quantified: LPooled<Vec<(ArcStr, usize)>> =
            rigid.iter().map(|(n, tv)| (n.clone(), tv.cell_addr())).collect();
        if generic && callsite::carried_out(&quantified, self.node.typ()).is_some() {
            return format_with_flags(PrintFlag::DerefTVars | PrintFlag::ReplacePrims, || {
                Err(anyhow!(
                    "{} is {}, made generic by an earlier compile, so what is written to \
                     it must be as general, and {written} is not: it holds a type variable \
                     of the code around the write",
                    bind.name,
                    bind.typ
                ))
            })
            .map_err(|e| e.at(&self.spec));
        }
        let scope = Scope::root();
        wrap!(
            self,
            callsite::check_hidden_defaults(
                ctx,
                BitFlags::empty(),
                &scope,
                self.spec.id,
                &held,
                written,
                callsite::Narrowing::Copy
            )
        )
    }
}

impl<R: Rt, E: UserEvent> ConnectDeref<R, E> {
    fn typecheck0_with(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        child: &mut Child<'_, R, E>,
        check: bool,
    ) -> Result<()> {
        wrap!(self.rhs, child(&mut self.rhs, ctx))?;
        if !check {
            return Ok(());
        }
        let rhs = &ctx.monomorphic(self.rhs.typ());
        let btyp = match ctx.env.by_id.get(&self.src_id) {
            None => bail!("BUG missing bind {:?}", self.src_id),
            Some(bind) => bind.typ.clone(),
        };
        // a typedef naming a reference, alone or as a union member, is that
        // reference
        let expand = |t: &Type| -> Result<Option<Type>> {
            let mut t = t.with_deref(|t| t.cloned());
            while let Some(r @ Type::Ref(_)) = &t {
                t = Some(r.lookup_ref(&ctx.env)?);
            }
            Ok(t)
        };
        let targets = expand(&btyp).and_then(|t| match &t {
            None => Ok(None),
            Some(Type::ByRef(m, t)) => Ok(Some(vec![(*m, (**t).clone())])),
            Some(Type::Set(ts)) => ts
                .iter()
                .map(|t| match &expand(t)? {
                    Some(Type::ByRef(m, t)) => Ok((*m, (**t).clone())),
                    _ => bail!("can't write through {btyp}: not a reference"),
                })
                .collect::<Result<Vec<_>>>()
                .map(Some),
            Some(t) => bail!("can't write through {t}: not a reference"),
        });
        match wrap!(self, targets)? {
            // an unbound reference is inferred writable at the written type
            None => {
                let typ = Type::ByRef(Mutability::Mut, Arc::new(rhs.clone()));
                wrap!(self, btyp.check_contains(&ctx.env, &typ))
            }
            // a write must fit every reference the binding may hold
            Some(targets) => {
                for (m, t) in targets {
                    if m == Mutability::Shared {
                        bail!(
                            "can't write through a read-only reference ({btyp}): take it with `&mut`"
                        )
                    }
                    wrap!(self, t.check_contains(&ctx.env, rhs))?;
                    wrap!(
                        self,
                        callsite::check_hidden_defaults(
                            ctx,
                            BitFlags::empty(),
                            &Scope::root(),
                            self.spec.id,
                            &t,
                            rhs,
                            callsite::Narrowing::Copy
                        )
                    )?;
                }
                Ok(())
            }
        }
    }
}

impl<R: Rt, E: UserEvent> TypeCast<R, E> {
    fn typecheck0_with(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        child: &mut Child<'_, R, E>,
    ) -> Result<()> {
        Ok(wrap!(self.n, child(&mut self.n, ctx))?)
    }
}

impl<R: Rt, E: UserEvent> Never<R, E> {
    fn typecheck0_with(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        child: &mut Child<'_, R, E>,
    ) -> Result<()> {
        for n in self.n.iter_mut() {
            wrap!(n, child(n, ctx))?
        }
        Ok(())
    }
}

impl<R: Rt, E: UserEvent> Any<R, E> {
    fn typecheck0_with(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        child: &mut Child<'_, R, E>,
        check: bool,
    ) -> Result<()> {
        for n in self.n.iter_mut() {
            wrap!(n, child(n, ctx))?
        }
        if !check {
            return Ok(());
        }
        let bottom = Type::Bottom;
        let mut ts: LPooled<Vec<&Type>> = LPooled::take();
        ts.push(&bottom);
        ts.extend(self.n.iter().map(|n| n.typ()));
        let rtyp = wrap!(self, Type::union_of_alternatives(&ctx.env, &ts))?;
        let rtyp = if rtyp == Type::Bottom { Type::empty_tvar() } else { rtyp };
        self.typ.check_contains(&ctx.env, &rtyp)?;
        Ok(())
    }
}

impl<R: Rt, E: UserEvent> Sample<R, E> {
    fn typecheck0_with(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        child: &mut Child<'_, R, E>,
    ) -> Result<()> {
        wrap!(self.trigger, child(&mut self.trigger, ctx))?;
        wrap!(self.arg.node, child(&mut self.arg.node, ctx))?;
        // the child may replace its typ during typecheck0
        self.typ = self.arg.node.typ().clone();
        Ok(())
    }
}
