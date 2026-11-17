use crate::{
    BindId, CAST_ERR, CAST_ERR_TAG, CFlag, CompileCtx, Event, ExecCtx, Node, NodeView,
    ParMode, PendingSettle, PrintFlag, Refs, Restore, Rt, Scope, Tag, TagValue, Update,
    View,
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
    prune::ReadStamp,
    typ::{
        AmbiguousMember, Brief, CastInput, Mutability, NodeKey, TVal, TVar, Type,
        TypeMismatch, TypeRef,
        bounds::{self, Solving},
        tvar::{
            AtLevel, BoundAt, InTask, Level, TaskContext, TaskId, current_task, new_task,
        },
    },
};
use ahash::{AHashMap, AHashSet};
use anyhow::{Context, Result, anyhow, bail};
use arcstr::ArcStr;
use compact_str::{CompactString, format_compact};
use compiler::{compile, compile_module};
use enumflags2::BitFlags;
use netidx_core::{
    pack::{Pack, PackError},
    path::Path,
};
use netidx_value::{Typ, Value};
use poolshark::local::LPooled;
use smallvec::SmallVec;
use std::{
    cell::Cell,
    collections::VecDeque,
    iter, mem,
    sync::{
        OnceLock,
        atomic::{AtomicBool, Ordering},
    },
};
use triomphe::{Arc, ThinArc};

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
    /// store-stamped THIS cycle: the entry's own tag applies
    /// ([`delivered_view`]).
    Delivered(&'a TagValue),
    /// Last cycle's delivery, made after the walk passed this reader and
    /// handed to it alone now: its own tag applies.
    Late(&'a TagValue),
    /// A standing store entry, stamped in an earlier cycle: the value
    /// channel. Readers view it Stale, or Fired at a birth.
    Standing(&'a (TagValue, u64)),
}

impl<'a> VarRead<'a> {
    /// The entry and its stamp, where the reader of `id` reads one that
    /// stands: stamped in an earlier cycle, or, to a resume or a step,
    /// stood again quietly in this one ([`wake::stands_since`]).
    #[inline(always)]
    pub(crate) fn standing<R: Rt>(
        &self,
        ctx: &ExecCtx<'_, R>,
        id: &BindId,
    ) -> Option<(&'a TagValue, u64)> {
        match *self {
            VarRead::Standing((tv, stamp)) => Some((tv, *stamp)),
            VarRead::Delivered(tv) => wake::stands_since(ctx, id, tv).map(|s| (tv, s)),
            VarRead::Late(_) => None,
        }
    }

    /// The production a reader of `id` under `ctx`'s view takes from the
    /// read.
    #[inline(always)]
    pub(crate) fn production<R: Rt>(
        &self,
        ctx: &ExecCtx<'_, R>,
        id: &BindId,
    ) -> TagValue {
        match *self {
            VarRead::Late(tv) => tv.clone(),
            VarRead::Delivered(tv) | VarRead::Standing((tv, _)) => {
                match self.standing(ctx, id) {
                    Some((tv, _)) => {
                        let mut tv = standing_view(ctx.event, tv);
                        if wake::resumes(ctx) {
                            let tag = tv.tag().moved_if(true);
                            tv.retag(tag);
                        }
                        tv
                    }
                    None => delivered_view(ctx.event, tv),
                }
            }
        }
    }
}

/// Read a variable: the overlay, then the persistent store. `None`
/// means the bind has never delivered.
pub(crate) fn read_var<'a, R: Rt>(
    ctx: &'a ExecCtx<'_, R>,
    id: &BindId,
) -> Option<VarRead<'a>> {
    if let crate::branch::RtView::Fork(f) = &ctx.rt {
        f.note_read(*id);
    }
    if let Some(tv) = ctx.event.variables.get(id) {
        return Some(VarRead::Delivered(tv));
    }
    match ctx.rt.store_get_var(ctx.rt.var_of(*id)) {
        Some((tv, stamp)) if *stamp == ctx.rt.cycle() => Some(VarRead::Delivered(tv)),
        Some(standing) => Some(VarRead::Standing(standing)),
        None => None,
    }
}

/// [`read_var`] by a reader at the root `root`, inside the walk's
/// boundary, keeping the read stamp `read`: a fire made last cycle after
/// the walk passed it is delivered to it now ([`crate::Event::late`]).
#[inline]
pub(crate) fn read_var_by<'a, R: Rt>(
    ctx: &'a ExecCtx<'_, R>,
    id: &BindId,
    root: ExprId,
    read: Option<&ReadStamp>,
) -> Option<VarRead<'a>> {
    if ctx.event.late.is_some()
        && let Some(tv) = late_fire(ctx, id, root, read)
    {
        return Some(VarRead::Late(tv));
    }
    read_var(ctx, id)
}

/// The fire of `id` made last cycle after the walk passed a reader at the
/// root `root`, inside the walk's boundary, keeping the read stamp `read`,
/// delivered to it now ([`crate::Event::late`]).
fn late_fire<'a, R: Rt>(
    ctx: &'a ExecCtx<'_, R>,
    id: &BindId,
    root: ExprId,
    read: Option<&ReadStamp>,
) -> Option<&'a TagValue> {
    let late = ctx.event.late.as_ref()?;
    let var = ctx.event.variables.var_of(*id);
    let fires = late.get(&var.id())?;
    if ctx.event.variables.get_var(var).is_some() {
        return None;
    }
    // CR claude for claude: [perf/medium] A fire that N readers take late costs N^2 a
    // cycle: this find scans every late fire of the variable for each reader that reads
    // it, and GX::deliver_late dedups the same list with one scan per queued reader
    // (`fires.iter().any`). A map of N slots whose callback reads a binding made below
    // the map (through a function value or a reference written later) takes each fire
    // of that binding late in all N slots: 18 such cycles cost 1.4 s at N=8000, 5.4 s
    // at 16000 and 21 s at 32000 on the debug build, against 0.08, 0.2 and 0.6 s for
    // the same map over a binding above it. Key a variable's late fires by reader
    // (Reader is Hash + Eq; a small vector up to a few and a map past that, as rt.rs's
    // Readers does), so the dedup and this lookup are one probe each. probe:
    // design/review-2026-11-16/repro/x-prune-late-readers-quadratic.sh (x-prune-01)
    let (tv, _) = fires.iter().find(|(_, r)| {
        r.root == root && r.boundary == ctx.boundary && r.read.as_ref() == read
    })?;
    if let crate::branch::RtView::Fork(f) = &ctx.rt {
        f.note_read(*id);
    }
    Some(tv)
}

/// `GRAPHIX_QUIET=audit`: a reader in an ordinary cycle took the quiet
/// production `standing`, which no wake stood again ([`Tag::moved`]), over
/// its last production `resident`. The two agree, or a value changed under
/// the reader with nothing telling it to recompute. A seq machine's own
/// names are read by steps it runs one at a time, which is no sleep.
#[inline]
pub(crate) fn audit_quiet<R: Rt>(
    ctx: &ExecCtx<'_, R>,
    id: &BindId,
    resident: &TagValue,
    standing: &TagValue,
    spec: &Expr,
) {
    if crate::dbgenv::graphix_quiet_audit() {
        audit_standing(ctx, id, resident, standing, spec)
    }
}

#[cold]
fn audit_standing<R: Rt>(
    ctx: &ExecCtx<'_, R>,
    id: &BindId,
    resident: &TagValue,
    standing: &TagValue,
    spec: &Expr,
) {
    let lowered = || ctx.env.by_id.get(id).is_some_and(|b| crate::is_lowered(&b.name));
    if ctx.event.view != View::Cycle
        || standing.tag().moved()
        || resident.tag().is_bottom()
        || lowered()
    {
        return;
    }
    let same = !standing.tag().is_bottom()
        && resident.with_value(|a| standing.with_value(|b| a == b));
    if !same {
        panic!(
            "GRAPHIX_QUIET=audit: a value changed under a reader nothing told to recompute: \
             {resident:?} -> {standing:?}, at {} `{spec}`",
            spec.pos
        )
    }
}

/// An entry delivered this cycle as a reader sees it: its own tag. What a
/// wake stood moved ([`Tag::moved`]) is a fire to no reader; a step, born
/// into a standing world, computes everything anew and reads it plain
/// stale.
#[inline(always)]
pub(crate) fn delivered_view(event: &Event, tv: &TagValue) -> TagValue {
    let tag = tv.tag();
    let mut tv = tv.clone();
    if tag.moved() && event.view == View::Step {
        tv.retag(tag.quiet());
    }
    tv
}

/// A standing entry as a reader sees it: fresh at a birth only. A wake
/// or a step reads it stale: its value is a past event the graph
/// already consumed.
pub(crate) fn standing_view(event: &Event, tv: &TagValue) -> TagValue {
    let tag = if event.view == View::Birth { tv.tag().fresh() } else { tv.tag().quiet() };
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

/// Read `$typ` through the first of the patterns that matches. A union with
/// an open member, whose other members the patterns read, is refused as one
/// open cell is: its type is needed here ([`crate::node::needs_member_type`]).
/// Another union no pattern reads is refused with the error
/// `$union(env, union)` (optional, after `;`) gives for it, or else as
/// "expected `$name` not" the union.
#[macro_export]
macro_rules! deref_typ {
    ($name:literal, $ctx:expr, $typ:expr, $($pat:pat => $body:expr),+) => {
        $crate::deref_typ!(
            $name, $ctx, $typ;
            |_: &$crate::env::Env, _: &$crate::typ::Type| -> Option<anyhow::Error> { None };
            $($pat => $body),+
        )
    };
    ($name:literal, $ctx:expr, $typ:expr; $union:expr; $($pat:pat => $body:expr),+) => {{
        let mut bound = $crate::node::one_bound(&$typ);
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
                        if let $crate::typ::Type::Set(ms) = &nt {
                            #[allow(unused_variables, unreachable_patterns)]
                            let reads = |m: &$crate::typ::Type| match &Some(m.clone()) {
                                $($pat => true),+,
                                _ => false,
                            };
                            if $crate::node::needs_member_type(&$ctx.env, ms, reads) {
                                return Err($crate::node::unknown_read($name, &nt));
                            }
                            $crate::format_with_flags($crate::PrintFlag::DerefTVars, || {
                                match ($union)(&$ctx.env, &nt) {
                                    Some(e) => Err(e),
                                    None => anyhow::bail!("expected {} not {nt}", $name),
                                }
                            })?
                        }
                        typ = Some(nt);
                    }
                    Some(t) => $crate::format_with_flags($crate::PrintFlag::DerefTVars, || {
                        anyhow::bail!("expected {} not {t}", $name)
                    })?,
                    None => match $crate::node::read_bound(&$ctx.env, bound.take())? {
                        Some(b) => typ = Some(b),
                        None => return Err($crate::node::unknown_read($name, &$typ)),
                    },
                }
            }
        })
    }};
}

/// The one bound of the cell `t` is, when it has exactly one.
#[doc(hidden)]
pub fn one_bound(t: &Type) -> Option<Type> {
    let Type::TVar(tv) = t else { return None };
    let mut c = tv.cell_constraints();
    match c.len() {
        1 => c.pop(),
        _ => None,
    }
}

/// What a read takes an open cell's one `bound` as: the structure or
/// abstract type it is or a typedef names, which every type the cell
/// admits has.
#[doc(hidden)]
pub fn read_bound(env: &env::Env, mut bound: Option<Type>) -> Result<Option<Type>> {
    while let Some(rt @ Type::Ref(_)) = &bound
        && !rt.is_trait_ref(env)
    {
        bound = Some(rt.lookup_ref(env)?);
    }
    Ok(bound.filter(|b| {
        matches!(
            b,
            Type::Struct(_) | Type::Tuple(_) | Type::Fn(_) | Type::Abstract { .. }
        )
    }))
}

/// Whether a read through the union `ms` needs a member's type: one is an
/// open cell no bound reads, and the read `reads` each other one, through
/// its typedefs and its bound.
#[doc(hidden)]
pub fn needs_member_type(
    env: &env::Env,
    ms: &[Type],
    reads: impl Fn(&Type) -> bool,
) -> bool {
    let mut open = false;
    for m in ms {
        let mut t = match m.deref_cloned() {
            Some(t) => t,
            None => match read_bound(env, one_bound(m)) {
                Ok(Some(b)) => b,
                Ok(None) => {
                    open = true;
                    continue;
                }
                Err(_) => return false,
            },
        };
        while let Type::Ref(_) = &t
            && !t.is_trait_ref(env)
        {
            match t.lookup_ref(env).ok().and_then(|t| t.deref_cloned()) {
                Some(next) => t = next,
                None => return false,
            }
        }
        if !reads(&t) {
            return false;
        }
    }
    open
}

/// The refusal of a read of a `name` (a fn, a struct, a tuple) through
/// `typ`, a type not yet known. Where `typ` holds a variable the definition
/// being checked declares, that variable is rigid and no annotation of a
/// binding gives it a type: the remedy is in its declaration.
#[doc(hidden)]
pub fn unknown_read(name: &str, typ: &Type) -> anyhow::Error {
    match declared_open(typ) {
        Some(var) => {
            let bound = match name {
                "fn" => "fn(..) -> ..",
                "struct" => "{..}",
                _ => "(..)",
            };
            anyhow!(
                "reading a {name} needs the type of `'{var}` known here: the definition \
                 declares `'{var}`, so declare a bound it reads as a {name}, `'{var}: \
                 {bound}`, or write its type there"
            )
        }
        None => {
            anyhow!("reading a {name} needs its type known here: annotate the binding")
        }
    }
}

/// The name of a variable the definition being checked declares that `typ`
/// is or holds as a member of a union.
fn declared_open(typ: &Type) -> Option<ArcStr> {
    match typ {
        Type::TVar(tv) => tv.declared_name(),
        Type::Set(ms) => ms.iter().find_map(declared_open),
        _ => None,
    }
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
pub(crate) fn deselecting_arm(inside: bool, f: impl FnOnce()) {
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
    pub(crate) fn new<R: Rt>(typ: Type) -> Node<R> {
        Node::new(Nop { typ })
    }
}

impl Nop {
    pub(crate) fn image_decode<R: Rt>(
        _ctx: &mut ExecCtx<'_, R>,
        buf: &mut &[u8],
    ) -> Result<Node<R>, PackError> {
        Ok(Self::new(Type::decode(buf)?))
    }
}

impl<R: Rt> Update<R> for Nop {
    fn for_each_child<'a>(&'a self, f: &mut dyn FnMut(&'a Node<R>)) {
        let _ = f;
    }

    fn for_each_child_mut(&mut self, f: &mut dyn FnMut(&mut Node<R>)) {
        let _ = f;
    }

    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::Nop, buf);
        self.typ.encode(buf)
    }

    fn update(&mut self, _ctx: &mut ExecCtx<'_, R>) -> &TagValue {
        TagValue::phantom_ref()
    }

    fn spec(&self) -> &Expr {
        &NOP
    }

    fn typ(&self) -> &Type {
        &self.typ
    }

    fn view(&self) -> NodeView<'_, R> {
        NodeView::Nop(self)
    }
}

#[derive(Debug)]
pub struct ExplicitParens<R: Rt> {
    pub(crate) spec: Arc<Expr>,
    pub n: Node<R>,
}

impl<R: Rt> ExplicitParens<R> {
    fn with(spec: Arc<Expr>, n: Node<R>) -> Node<R> {
        Node::new(Self { spec, n })
    }

    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<'_, R>,
        buf: &mut &[u8],
    ) -> Result<Node<R>, PackError> {
        let spec = Arc::new(Expr::decode(buf)?);
        let n = decode_node(ctx, buf)?;
        Ok(Self::with(spec, n))
    }

    pub(crate) fn compile(
        ctx: &mut CompileCtx<R>,
        flags: BitFlags<CFlag>,
        spec: Arc<Expr>,
        inner: Expr,
        scope: &Scope,
        top_id: ExprId,
    ) -> Result<Node<R>> {
        // `spec` is the outer parens expression: it carries the position
        // and any `#[..]` decorations, so the node must own it
        let n = compile(ctx, flags, inner, scope, top_id)?;
        Ok(Self::with(spec, n))
    }
}

impl<R: Rt> Update<R> for ExplicitParens<R> {
    fn for_each_child<'a>(&'a self, f: &mut dyn FnMut(&'a Node<R>)) {
        f(&self.n)
    }

    fn for_each_child_mut(&mut self, f: &mut dyn FnMut(&mut Node<R>)) {
        f(&mut self.n)
    }

    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::ExplicitParens, buf);
        self.spec.encode(buf)?;
        self.n.image_encode(buf)
    }

    fn fuse(&mut self, ctx: &mut CompileCtx<R>) -> Result<Option<Node<R>>> {
        // parens mean nothing to fusion, unless an attribute they carry
        // names the expression a region (`#[native] (e)`)
        match self.spec.dec.as_ref().is_some_and(|d| !d.attrs.is_empty()) {
            true => fuse(&mut self.n, ctx).map(|()| None),
            false => fusion::fuse_parts([&mut self.n], ctx),
        }
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R>) -> &TagValue {
        self.n.update(ctx)
    }

    typed_by_row!(children);

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn typ(&self) -> &Type {
        &self.n.typ()
    }

    fn view(&self) -> NodeView<'_, R> {
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
pub struct Held<R: Rt> {
    /// The last value-bearing production (`Some` = there was once a
    /// real value; a bottom's placeholder never lands here).
    pub value: Option<Value>,
    /// The tag of the child's last production. Only the TAINT bit is
    /// meaningful at rest; firedness belongs to a production, not to
    /// held memory.
    pub tag: Tag,
    pub node: Node<R>,
}

impl<R: Rt> Held<R> {
    pub(crate) fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        self.node.image_encode(buf)
    }

    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<'_, R>,
        buf: &mut &[u8],
    ) -> Result<Self, PackError> {
        Ok(Self::new(decode_node(ctx, buf)?))
    }

    pub fn new(node: Node<R>) -> Self {
        Self { value: None, tag: Tag::FIRED, node }
    }

    /// Update the node, returning the production's tag. A bottom
    /// production poisons the tag but never overwrites the value.
    pub fn update(&mut self, ctx: &mut ExecCtx<'_, R>) -> Tag {
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
    pub fn sleep(&mut self, ctx: &mut ExecCtx<'_, R>) {
        self.node.sleep(ctx)
    }
}

/// Update every child of a composite and join their tags
/// ([`Tag::join`]): the join triggers when a child did and is bottom
/// when a child is; past the caller's [`dense_gate!`] it is the
/// composite's own tag. The productions stay borrowed, so a value is
/// cloned only once the gate let it through.
pub(crate) fn gather<'a, R: Rt>(
    ctx: &mut ExecCtx<'_, R>,
    nodes: &'a mut [Node<R>],
    site: &mut ForkSite,
) -> (Tag, SmallVec<[&'a TagValue; 32]>) {
    let n = nodes.len();
    site.decide_siblings(ctx, n, nodes.iter());
    fork_point(ctx, nodes, site, gather_in_order, |(t0, mut p0), (t1, p1)| {
        p0.extend(p1);
        (t0.join(t1), p0)
    })
}

fn gather_in_order<'a, R: Rt>(
    ctx: &mut ExecCtx<'_, R>,
    nodes: &'a mut [Node<R>],
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
        if !(tag.triggers() || tag.moved() || $resident.tag().is_bottom() || $woke) {
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
        fn typecheck0(&mut self, ctx: &mut CompileCtx<R>) -> Result<()> {
            self.typecheck0_with(ctx, &mut |n, ctx| n.typecheck0(ctx), true)
        }

        fn typecheck0_instance(
            &mut self,
            ctx: &mut CompileCtx<R>,
            types: &mut $crate::node::lambda::InstanceTypes,
        ) -> Result<()> {
            let row = types.settle(ctx, &self.spec, &self.typ)?;
            self.typecheck0_with(
                ctx,
                &mut |n, ctx| n.typecheck0_instance(ctx, types),
                !row,
            )
        }
    };
    (source) => {
        fn typecheck0(&mut self, ctx: &mut CompileCtx<R>) -> Result<()> {
            self.typecheck0_with(ctx, &mut |n, ctx| n.typecheck0(ctx), true)
        }

        fn typecheck0_instance(
            &mut self,
            ctx: &mut CompileCtx<R>,
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
        fn typecheck0(&mut self, ctx: &mut CompileCtx<R>) -> Result<()> {
            self.typecheck0_with(ctx, &mut |n, ctx| n.typecheck0(ctx))
        }

        fn typecheck0_instance(
            &mut self,
            ctx: &mut CompileCtx<R>,
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
    /// The registered representation this abstract one must be, compared
    /// at the check, once the typedefs it names are declared.
    unchanged_from: Option<Arc<env::AbstractRep>>,
    /// The definition, kept while the node lives: a block compiled in an
    /// environment it does not outlive still names it.
    definition: Option<std::sync::Arc<crate::typ::ResolvedRef>>,
}

impl TypeDef {
    fn with<R: Rt>(
        spec: Arc<Expr>,
        scope: ModPath,
        name: ArcStr,
        unchanged_from: Option<Arc<env::AbstractRep>>,
        definition: Option<std::sync::Arc<crate::typ::ResolvedRef>>,
    ) -> Node<R> {
        Node::new(Self { spec, scope, name, unchanged_from, definition })
    }

    /// A registration this definition made owns what its representation
    /// names; one another made is compared with this definition's.
    fn check_unchanged<R: Rt>(&self, ctx: &CompileCtx<R>) -> Result<()> {
        match &self.unchanged_from {
            None => Ok(ctx.env.own_abstract_rep(&self.scope, &self.name)),
            Some(old) => ctx.env.check_abstract_rep(&self.scope, &self.name, old),
        }
    }

    pub(crate) fn compile<R: Rt>(
        ctx: &mut CompileCtx<R>,
        spec: Arc<Expr>,
        scope: &Scope,
        name: &Name,
        params: &Arc<[(TVar, Option<Type>)]>,
        body: &TypeDefBody,
    ) -> Result<Node<R>> {
        let (params, body) = Type::reset_typedef_tvars(params, body);
        let parametric = !params.is_empty();
        let (verdict, unchanged_from) = ctx
            .env
            .deftype_in_block(
                &scope.lexical,
                name,
                params,
                &body,
                name.pos_or(spec.pos),
                spec.pos,
                spec.ori.clone(),
            )
            .at(&spec)?;
        let name = name.name.clone();
        let definition = ctx.env.typedef_definition(&scope.lexical, &name);
        if parametric {
            // A cycle a definition completes runs through it: without
            // parameters through its body, which deftype walked; with them also
            // through an argument (`type T = G<[i64, T]>`), which only T's walk sees.
            crate::recheck_pending_typedefs(ctx, crate::Completed::Typedef)?;
        }
        if verdict == crate::typ::Unguarded::Unknown {
            ctx.pending_names.push(crate::Pending::Typedef(
                scope.lexical.clone(),
                name.clone(),
                (*spec).clone(),
                crate::Undecided::Shape,
            ));
        }
        Ok(Self::with(spec, scope.lexical.clone(), name, unchanged_from, definition))
    }
}

impl TypeDef {
    pub(crate) fn image_decode<R: Rt>(
        _ctx: &mut ExecCtx<'_, R>,
        buf: &mut &[u8],
    ) -> Result<Node<R>, PackError> {
        let spec = Arc::new(Expr::decode(buf)?);
        let scope = ModPath::decode(buf)?;
        let name = ArcStr::decode(buf)?;
        let definition = match bool::decode(buf)? {
            false => None,
            true => Some(crate::image::resolved_decode(buf)?),
        };
        Ok(Self::with(spec, scope, name, None, definition))
    }
}

impl<R: Rt> Update<R> for TypeDef {
    fn for_each_child<'a>(&'a self, f: &mut dyn FnMut(&'a Node<R>)) {
        let _ = f;
    }

    fn for_each_child_mut(&mut self, f: &mut dyn FnMut(&mut Node<R>)) {
        let _ = f;
    }

    fn typecheck0(&mut self, ctx: &mut CompileCtx<R>) -> Result<()> {
        self.check_unchanged(ctx)
    }

    fn typecheck0_instance(
        &mut self,
        ctx: &mut CompileCtx<R>,
        _types: &mut lambda::InstanceTypes,
    ) -> Result<()> {
        self.check_unchanged(ctx)
    }

    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::TypeDef, buf);
        self.spec.encode(buf)?;
        self.scope.encode(buf)?;
        self.name.encode(buf)?;
        self.definition.is_some().encode(buf)?;
        match &self.definition {
            None => Ok(()),
            Some(def) => crate::image::resolved_encode(def, buf),
        }
    }

    fn update(&mut self, _ctx: &mut ExecCtx<'_, R>) -> &TagValue {
        TagValue::phantom_ref()
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R>) {
        ctx.env.undeftype(&self.scope, &self.name)
    }

    fn typ(&self) -> &Type {
        Type::BOTTOM
    }

    fn view(&self) -> NodeView<'_, R> {
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
    pub fn new<R: Rt>(value: Value, typ: Type, spec: Arc<Expr>) -> Node<R> {
        // the resident starts Stale so an instance bound outside a birth
        // still computes; it fires only where it is born
        let resident = TagValue::stale(value.clone());
        Node::new(Self { spec, value, typ, resident })
    }
}

impl Constant {
    pub(crate) fn image_decode<R: Rt>(
        _ctx: &mut ExecCtx<'_, R>,
        buf: &mut &[u8],
    ) -> Result<Node<R>, PackError> {
        let spec = Arc::new(Expr::decode(buf)?);
        let value = Value::decode(buf)?;
        let typ = Type::decode(buf)?;
        Ok(Self::new(value, typ, spec))
    }
}

/// A constant's production: fired where it is born, standing otherwise.
/// Every argument-less literal is a constant.
pub(crate) fn produce_constant<'a>(
    event: &Event,
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

impl<R: Rt> Update<R> for Constant {
    fn for_each_child<'a>(&'a self, f: &mut dyn FnMut(&'a Node<R>)) {
        let _ = f;
    }

    fn for_each_child_mut(&mut self, f: &mut dyn FnMut(&mut Node<R>)) {
        let _ = f;
    }

    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::Constant, buf);
        self.spec.encode(buf)?;
        self.value.encode(buf)?;
        self.typ.encode(buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R>) -> &TagValue {
        produce_constant(ctx.event, &mut self.resident, || self.value.clone())
    }

    fn typ(&self) -> &Type {
        &self.typ
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn view(&self) -> NodeView<'_, R> {
        NodeView::Constant(self)
    }

    fn emit_clif(&self, cx: &mut BodyCx) -> Result<CompiledExpr> {
        emit_const_node(cx, &self.value, &self.typ)
    }
}

#[derive(Debug)]
pub struct Block<R: Rt> {
    pub(crate) module: bool,
    /// A file module's path, which its delete unbinds from the lexical
    /// maps.
    declared: Option<ModPath>,
    /// A module's: a `let` among its children re-raised at a wake in the
    /// last update that walked it, and the block around it has not taken
    /// that back yet.
    reraised: bool,
    /// A module's: the lets of the block around it, which it marks when it
    /// notes a re-raise.
    around: Option<Lets>,
    pub(crate) spec: Arc<Expr>,
    pub(crate) children: Box<[Node<R>]>,
    /// Indices of `catch(e) expr` children, in syntactic order.
    /// update/typecheck run the covered children first, then catches in
    /// reverse syntactic order: an inner handler's rethrow delivers to
    /// its predecessor, which only sees it if it updates after.
    pub(crate) catches: Box<[usize]>,
    /// The children that are `let`s or modules, whose wake re-raises a
    /// non-module block takes back at its end and a module notes for the
    /// block around it (`Bind::take_back_reraised`).
    lets: Option<Lets>,
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

    /// Whether a block of `len` statements updates run by run.
    fn forks<R: Rt>(ctx: &ExecCtx<'_, R>, len: usize) -> bool {
        len > 1 && ctx.fork_mode() != ParMode::Off
    }

    /// Update a block's `children`: run by run, each forked where its
    /// site says, when the runtime forks, else in order; the catches last,
    /// in reverse syntactic order. The last child's production (a block
    /// never ends in a catch); `None` when there are no children.
    pub(crate) fn update<R: Rt>(
        &mut self,
        ctx: &mut ExecCtx<'_, R>,
        children: &mut [Node<R>],
        catches: &[usize],
    ) -> Option<TagValue> {
        let last = children.len();
        if !Self::forks(ctx, last) {
            let mut res = None;
            for i in evaluation_order(last, catches) {
                let tv = children[i].update(ctx);
                if i + 1 == last {
                    res = Some(tv.clone());
                }
            }
            return res;
        }
        let Self { planned, sites } = self;
        let sites = sites.get_or_insert_with(|| {
            planned
                .get_or_init(|| crate::analysis::plan_block(children, catches, ctx))
                .iter()
                .map(|&(a, b)| (a, b, ForkSite::planned()))
                .collect()
        });
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

/// Whether the `let` or module `n` holds what a wake re-raised and its
/// block has not taken back: a module notes it as its update ends.
pub(crate) fn holds_reraised<R: Rt>(n: &Node<R>) -> bool {
    match n.view() {
        NodeView::Bind(b) => b.holds_reraised(),
        NodeView::Block(b) => b.reraised,
        NodeView::Module(m) => m.reraised(),
        _ => false,
    }
}

/// Take back what a wake re-raised to the `let` `n`, or to the `let`s of
/// the module `n`, whose names the block around it reads.
pub(crate) fn take_back_reraised<R: Rt>(n: &mut Node<R>, ctx: &mut ExecCtx<'_, R>) {
    if !holds_reraised(n) {
        return;
    }
    if let Some(b) = n.downcast_mut::<bind::Bind<R>>() {
        return b.take_back_reraised(ctx);
    }
    crate::stack::ensure_sufficient(|| {
        if let Some(b) = n.downcast_mut::<Block<R>>() {
            b.take_back_reraised(ctx)
        } else if let Some(m) = n.downcast_mut::<module::Module<R>>() {
            m.take_back_reraised(ctx)
        }
    })
}

/// The `let` and module children of a block or a module, by index, and
/// the mark one of them sets when a wake re-raised to it, which the block
/// reads and clears as its update ends: only an update after a re-raise
/// visits them.
#[derive(Debug, Clone)]
pub(crate) struct Lets(ThinArc<AtomicBool, u32>);

impl Lets {
    /// The `let`s and modules among `children`, each handed these to
    /// mark; `None` when there are none. A module's names are the block
    /// around it to read, so that block takes back what its `let`s
    /// re-raise.
    pub(crate) fn wire<R: Rt>(children: &mut [Node<R>]) -> Option<Self> {
        let mut at: SmallVec<[u32; 8]> = SmallVec::new();
        for (i, n) in children.iter().enumerate() {
            match n.view() {
                NodeView::Bind(_) | NodeView::Module(_) => at.push(i as u32),
                NodeView::Block(b) if b.module => at.push(i as u32),
                _ => (),
            }
        }
        if at.is_empty() {
            return None;
        }
        let lets = Self(ThinArc::from_header_and_iter(
            AtomicBool::new(false),
            at.iter().copied(),
        ));
        for i in lets.iter() {
            let n = &mut children[i];
            let around = Some(lets.clone());
            if let Some(b) = n.downcast_mut::<bind::Bind<R>>() {
                b.around = around
            } else if let Some(b) = n.downcast_mut::<Block<R>>() {
                b.around = around
            } else if let Some(m) = n.downcast_mut::<module::Module<R>>() {
                m.around = around
            }
        }
        Some(lets)
    }

    pub(crate) fn iter(&self) -> impl Iterator<Item = usize> + '_ {
        self.0.slice.iter().map(|i| *i as usize)
    }

    /// One of the lets holds a re-raise this update.
    pub(crate) fn mark(&self) {
        self.0.header.header.store(true, Ordering::Relaxed)
    }

    /// Whether one of the lets marked since the last take.
    pub(crate) fn take_mark(lets: &Option<Self>) -> bool {
        lets.as_ref().is_some_and(|l| l.0.header.header.swap(false, Ordering::Relaxed))
    }
}

impl<R: Rt> Block<R> {
    /// Take back what a wake re-raised to the `let`s among the children,
    /// those of the modules among them included.
    fn take_back_reraised(&mut self, ctx: &mut ExecCtx<'_, R>) {
        self.reraised = false;
        let Self { lets: Some(lets), children, .. } = self else { return };
        for i in lets.iter() {
            take_back_reraised(&mut children[i], ctx)
        }
    }

    /// Build a `Block` from compiled children. A module produces no
    /// value; a non-module block's value is its last child's.
    pub fn new(module: bool, children: Box<[Node<R>]>, spec: Arc<Expr>) -> Node<R> {
        Self::with(module, None, spec, children, Box::default(), Runs::default())
    }

    fn with(
        module: bool,
        declared: Option<ModPath>,
        spec: Arc<Expr>,
        mut children: Box<[Node<R>]>,
        catches: Box<[usize]>,
        runs: Runs,
    ) -> Node<R> {
        Node::new(Self {
            module,
            declared,
            reraised: false,
            around: None,
            spec,
            lets: Lets::wire(&mut children),
            children,
            catches,
            resident: TagValue::phantom(),
            runs,
        })
    }

    pub(crate) fn compile(
        ctx: &mut CompileCtx<R>,
        flags: BitFlags<CFlag>,
        spec: Arc<Expr>,
        scope: &Scope,
        top_id: ExprId,
        module: bool,
        declared: Option<ModPath>,
        exprs: &Arc<[Expr]>,
    ) -> Result<Node<R>> {
        let (children, catches) = compile_block_children(
            ctx,
            flags,
            scope,
            top_id,
            module,
            exprs.iter(),
            true,
        )?;
        Ok(Self::with(module, declared, spec, children, catches, Runs::default()))
    }
}

/// A module's top block's path below its package root, which its
/// statements' `self::` writes name; `None` for any other block.
pub(crate) fn block_top<R: Rt>(
    ctx: &CompileCtx<R>,
    scope: &Scope,
) -> Option<CompactString> {
    let lexical: &str = &scope.lexical;
    (crate::mod_root(lexical) == lexical).then(|| {
        let below = &lexical[ctx.env.package_root(lexical).len()..];
        CompactString::from(below.trim_start_matches('/'))
    })
}

/// Compile a statement list: each `catch(e) expr` child compiles
/// through [`error::Catch::compile`], which advances the dynamic scope
/// for all subsequent siblings. Returns the children in syntactic order
/// plus the catch indices. The lets something writes are found among the
/// list's own statements where `own_writers`, else the caller entered the
/// statements its lets are visible to ([`BlockWrites::enter`]).
pub(crate) fn compile_block_children<'a, R: Rt>(
    ctx: &mut CompileCtx<R>,
    flags: BitFlags<CFlag>,
    scope: &Scope,
    top_id: ExprId,
    module: bool,
    exprs: impl Iterator<Item = &'a Expr>,
    own_writers: bool,
) -> Result<(Box<[Node<R>]>, Box<[usize]>)> {
    let exprs: SmallVec<[&'a Expr; 32]> = exprs.collect();
    // pre-register the block's `mod` names so resolution is independent
    // of declaration order; the `Module` arm is told, so its own
    // duplicate guard does not trip
    let mut dup = None;
    for e in exprs.iter() {
        module::declared_modules(e, &mut |name| {
            let p = ModPath(scope.lexical.append(name));
            if ctx.env.lexical.modules.contains(&p) {
                dup.get_or_insert_with(|| {
                    anyhow!("duplicate module definition {p}").at(&(*e))
                });
            }
            ctx.env.lexical.modules.insert_cow(p);
        });
    }
    if let Some(e) = dup {
        return Err(e);
    }
    let n = exprs.len();
    let typedefs: SmallVec<[ArcStr; 4]> = exprs
        .iter()
        .enumerate()
        .filter_map(|(i, e)| match &e.kind {
            ExprKind::TypeDef(td) if module || i + 1 < n => Some(td.name.name.clone()),
            _ => None,
        })
        .collect();
    let declares_types = !typedefs.is_empty();
    if declares_types {
        ctx.block_typedefs.push((scope.lexical.clone(), typedefs));
    }
    let list = &scope.lexical;
    ctx.env.open_block(
        list,
        exprs.iter().filter_map(|e| match &e.kind {
            ExprKind::TypeDef(td) => Some(td.name.name.as_str()),
            _ => None,
        }),
    );
    let mut later = env::Unsettled::default();
    for (i, e) in exprs.iter().enumerate() {
        match &e.kind {
            ExprKind::Use { names, .. } => {
                module::later_use(&ctx.env, list, i, e, names, &mut later)
            }
            _ => module::declared_modules(e, &mut |name| {
                later.modules.push((i, name.name.as_str().into()))
            }),
        }
    }
    let brings = !later.is_empty();
    let reopened = ctx.env.open_list(list, later);
    let mut scope = scope.clone();
    let mut children: LPooled<Vec<Node<R>>> = LPooled::take();
    let mut catches: LPooled<Vec<usize>> = LPooled::take();
    if own_writers {
        let top = block_top(ctx, &scope);
        ctx.written.enter(&exprs, top);
    }
    let compiled = exprs.iter().copied().enumerate().try_for_each(|(i, e)| {
        if matches!(e.kind, ExprKind::Catch(_)) {
            catches.push(i);
        }
        let at = StmtAt::Block { value: !module && i + 1 == n, last: i + 1 == n };
        if brings && module::brings_names(e) {
            ctx.env.settle_statement(list, i)
        }
        let boundary = prune::Prune::boundary(ctx);
        let (node, next) = ctx.in_boundary(Some(&boundary), |ctx| {
            compile_statement(ctx, flags, e, &scope, top_id, at)
        })?;
        scope = next;
        children.push(prune::Prune::wrap(boundary, node));
        Ok::<_, anyhow::Error>(())
    });
    if own_writers {
        ctx.written.exit();
    }
    if declares_types {
        ctx.block_typedefs.pop();
    }
    ctx.env.close_block(list);
    ctx.env.close_list(list, reopened);
    compiled?;
    for n in children.iter() {
        defer_typedef_names(ctx, n)?;
    }
    Ok((Box::from_iter(children.drain(..)), Box::from_iter(catches.drain(..))))
}

/// Whether the name `tr` writes means, by the end of the compile, a type
/// a later statement of a block being compiled declares: the block declares
/// it and has not yet, and no scope between declares it either. What the
/// name means now is not what it will mean.
pub(crate) fn later_typedef<R: Rt>(ctx: &CompileCtx<R>, tr: &TypeRef) -> bool {
    let (1, Some(name)) = (Path::levels(&*tr.name), Path::basename(&*tr.name)) else {
        return false;
    };
    let key = CompactString::from(name);
    let declared = |scope: &str| {
        let scope = ModPath(Path::from(ArcStr::from(scope)));
        ctx.env.lexical.typedefs.get(&scope).is_some_and(|m| m.get(&key).is_some())
    };
    ctx.block_typedefs.iter().any(|(block, names)| {
        names.iter().any(|n| n.as_str() == name)
            && Path::is_parent(&**block, &*tr.scope)
            && !Path::dirnames(&*tr.scope)
                .rev()
                .filter(|s| Path::is_parent(&**block, s))
                .any(declared)
    })
}

/// The lets among a block's statements that a `<-` or a `&mut` in the
/// block writes, lambda bodies and modules included, each as its
/// statement's id and the written name: a write names the nearest binding
/// of the name before it (its own let, for a `let rec`). A `self::` path,
/// and a `package::` path through the block's own module path, name the
/// block's lets where the block is its module's top (`top`, that path below
/// the package root). Such a let is never generalized, whatever the order
/// of its uses and writers. Outside a lambda body, a write under a nested
/// block's glob is left to the resolver: it compiles before the binding's
/// check, which then holds a binding it reaches to one instance
/// ([`CompileCtx::mark_connect_target`]). What a module in the block writes
/// outside it comes from `known`, and what this walk finds goes there.
fn written_lets(
    exprs: &[&Expr],
    top: Option<CompactString>,
    known: &mut ModuleWrites,
) -> LPooled<AHashSet<(ExprId, CompactString)>> {
    let mut w = Writes::new(Walk::Block { top }, known);
    w.statements(exprs.iter().copied());
    mem::take(&mut w.written)
}

/// What each module statement walked writes outside it, by the
/// statement's id: a function of the module's body alone, kept for the
/// compile of the outermost block, so a module nested d blocks deep is
/// walked once, not once for each block around it.
#[derive(Default)]
struct ModuleWrites(AHashMap<ExprId, LPooled<Vec<OuterWrite>>>);

/// A write a module's body makes outside it: where it lands, the name, and
/// whether it sits in a lambda body (it compiles at its definition's check).
type OuterWrite = (Anchor, CompactString, bool);

/// Per block being compiled, innermost last, the lets something in it
/// writes ([`written_lets`]), and what the modules they hold write
/// outside them, kept while any of them compiles.
#[derive(Default)]
pub(crate) struct BlockWrites {
    blocks: Vec<LPooled<AHashSet<(ExprId, CompactString)>>>,
    modules: ModuleWrites,
}

impl BlockWrites {
    /// A block's compile starts: walk its statements.
    pub(crate) fn enter(&mut self, exprs: &[&Expr], top: Option<CompactString>) {
        let written = written_lets(exprs, top, &mut self.modules);
        self.blocks.push(written)
    }

    /// The innermost block's compile ended.
    pub(crate) fn exit(&mut self) {
        self.blocks.pop();
        if self.blocks.is_empty() {
            self.modules.0.clear()
        }
    }

    /// The lets the innermost block being compiled writes.
    pub(crate) fn innermost(&self) -> Option<&AHashSet<(ExprId, CompactString)>> {
        self.blocks.last().map(|w| &**w)
    }

    /// A compile task's copy: the blocks around it, and a memo of its own.
    pub(crate) fn fork(&self) -> Self {
        Self { blocks: self.blocks.clone(), modules: ModuleWrites::default() }
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.blocks.is_empty() && self.modules.0.is_empty()
    }

    pub(crate) fn clear(&mut self) {
        self.blocks.clear();
        self.modules.0.clear()
    }
}

/// The names outside the module `id` that its body `exprs` may write, each
/// with where it lands: through a `super::` or `package::` path, through a
/// name it imports from there (a glob importing each name the body does
/// not bind), and through what its own modules write there. Taken out of
/// `known`, the caller puts it back.
fn module_writes(
    id: ExprId,
    exprs: &[Expr],
    known: &mut ModuleWrites,
) -> LPooled<Vec<OuterWrite>> {
    if let Some(outer) = known.0.remove(&id) {
        return outer;
    }
    let mut w = Writes::new(Walk::Module, known);
    w.statements(exprs.iter());
    mem::take(&mut w.outer)
}

/// Where a name outside a module stands: `k` levels of `super` out, or
/// in the module at a path below the package root (empty: the root).
#[derive(Debug, Clone, PartialEq, Eq)]
enum Anchor {
    Super(usize),
    Package(CompactString),
}

impl Anchor {
    /// The anchor `parts` leads with and the one name after it.
    fn of<'a>(parts: &[&'a str]) -> Option<(Anchor, &'a str)> {
        let (n, lead) = parts.split_last()?;
        match lead {
            ["package", path @ ..] if !path.iter().any(|p| is_path_keyword(p)) => {
                let mut at = CompactString::default();
                for (i, p) in path.iter().enumerate() {
                    if i > 0 {
                        at.push('/')
                    }
                    at.push_str(p)
                }
                Some((Anchor::Package(at), n))
            }
            [_, ..] if lead.iter().all(|p| *p == "super") => {
                Some((Anchor::Super(lead.len()), n))
            }
            _ => None,
        }
    }
}

fn is_path_keyword(p: &str) -> bool {
    matches!(p, "self" | "super" | "package")
}

/// What a name names at a point of the walk.
#[derive(Debug, Clone)]
enum Binding {
    /// A let of the walked block.
    Let(ExprId),
    /// A name a module imports from outside it.
    Outer(Anchor, CompactString),
    /// Any other binding.
    Other,
}

#[derive(Debug)]
enum Walk {
    /// A block's statements; `top`, the path below the package root of
    /// the module whose top block it is.
    Block { top: Option<CompactString> },
    /// A module's body.
    Module,
}

// CR claude for claude: [structure/low] This file holds two things that are not nodes:
// the written-lets walk (`written_lets`, `BlockWrites`, `ModuleWrites`, `Anchor`,
// `Writes` and its test, about 500 lines) and the wording of a `<-` refusal
// (`lower_shown`, `Holders`, `decided_decl`, `params_union`, `generic_target`,
// `write_mismatch`, about 520 lines). Neither reads a node's state, and a reader
// looking for `Sample` or `Block::update` pages through both. Move the walk to
// `node/writes.rs` and the refusal wording to `node/write_mismatch.rs` as they are;
// nothing else changes. Found by reading; not run. (c-node-mod-06)
struct Writes<'a> {
    walk: Walk,
    /// The walked statements' binding of each name at the walk's position.
    declared: LPooled<AHashMap<CompactString, Binding>>,
    /// Each name's nearer bindings in scope at the walk's position,
    /// innermost last.
    shadow: LPooled<AHashMap<CompactString, SmallVec<[Binding; 1]>>>,
    /// The names bound into `shadow`, in order, so a scope's end takes
    /// back what it bound.
    shadowed: LPooled<Vec<CompactString>>,
    /// Where the walked module's globs import from.
    globs: SmallVec<[Anchor; 1]>,
    /// A block walk's written lets.
    written: LPooled<AHashSet<(ExprId, CompactString)>>,
    /// A module walk's writes outside it.
    outer: LPooled<Vec<OuterWrite>>,
    /// The modules' writes outside them known so far.
    known: &'a mut ModuleWrites,
    /// The globs of the nested blocks around the walk's position.
    nested_globs: usize,
    /// The lambda bodies around the walk's position: a write in one
    /// compiles after the checks of the lets it may name.
    lambdas: usize,
}

#[cfg(test)]
thread_local! {
    static WRITES_STEPS: Cell<usize> = const { Cell::new(0) };
}

/// A statement a writes walk visits, counted under test.
#[inline(always)]
fn writes_step() {
    #[cfg(test)]
    WRITES_STEPS.with(|n| n.set(n.get() + 1));
}

impl<'a> Writes<'a> {
    fn new(walk: Walk, known: &'a mut ModuleWrites) -> Self {
        Self {
            walk,
            declared: LPooled::take(),
            shadow: LPooled::take(),
            shadowed: LPooled::take(),
            globs: SmallVec::new(),
            written: LPooled::take(),
            outer: LPooled::take(),
            known,
            nested_globs: 0,
            lambdas: 0,
        }
    }

    fn statements<'e>(&mut self, exprs: impl Iterator<Item = &'e Expr>) {
        for e in exprs {
            writes_step();
            match &e.kind {
                ExprKind::Bind(b) if b.rec => {
                    self.declare(e.id, &b.pattern);
                    self.expr(&b.value)
                }
                ExprKind::Bind(b) => {
                    self.expr(&b.value);
                    self.declare(e.id, &b.pattern)
                }
                ExprKind::Use { names, .. } => {
                    for item in names.iter() {
                        if let Some((n, b)) = self.imported(item) {
                            self.declared.insert(n, b);
                        }
                    }
                }
                _ => self.expr(e),
            }
        }
    }

    fn declare(&mut self, id: ExprId, p: &expr::StructurePattern) {
        let b = match self.walk {
            Walk::Block { .. } => Binding::Let(id),
            Walk::Module => Binding::Other,
        };
        p.with_names(&mut |n| {
            self.declared.insert(n.as_str().into(), b.clone());
        });
    }

    /// The name `item` binds and what it names; a glob from outside a
    /// module is noted instead.
    fn imported(&mut self, item: &expr::UseItem) -> Option<(CompactString, Binding)> {
        let parts: SmallVec<[&str; 4]> = Path::parts(&*item.path.0).collect();
        let outer = match self.walk {
            Walk::Module => Anchor::of(&parts),
            Walk::Block { .. } => None,
        };
        match (item.bound_name(), outer) {
            (None, Some((a, _))) => {
                self.globs.push(a);
                None
            }
            (None, None) => None,
            (Some(local), Some((a, n))) => {
                Some((local.into(), Binding::Outer(a, n.into())))
            }
            (Some(local), None) => Some((local.into(), Binding::Other)),
        }
    }

    fn target(&mut self, name: &ModPath) {
        let parts: SmallVec<[&str; 4]> = Path::parts(&**name).collect();
        match &parts[..] {
            [n] => self.write(n, false),
            ["self", n] => match self.walk {
                Walk::Module | Walk::Block { top: Some(_) } => self.write(n, true),
                Walk::Block { top: None } => (),
            },
            parts => {
                if let Some((a, n)) = Anchor::of(parts) {
                    self.write_outer(a, n)
                }
            }
        }
    }

    /// A write to `n` as it is named here: the nearest binding of `n`, or,
    /// `top`, the walked statements' own (a `self::` path).
    fn write(&mut self, n: &str, top: bool) {
        let nearer = match top {
            true => None,
            false => self.shadow.get(n).and_then(|bs| bs.last()),
        };
        let b = match nearer {
            Some(b) => Some(b),
            // a glob may provide `n`: the resolver names what this write
            // reaches before the binding's check
            None if !top && self.nested_globs > 0 && self.lambdas == 0 => return,
            None => self.declared.get(n),
        };
        match b {
            Some(Binding::Let(id)) => {
                self.written.insert((*id, n.into()));
            }
            Some(Binding::Outer(a, m)) => {
                self.outer.push((a.clone(), m.clone(), self.lambdas > 0))
            }
            Some(Binding::Other) => (),
            None => {
                for a in self.globs.iter() {
                    self.outer.push((a.clone(), n.into(), self.lambdas > 0))
                }
            }
        }
    }

    /// A write to `n` at `a` from here.
    fn write_outer(&mut self, a: Anchor, n: &str) {
        match (&self.walk, a) {
            (Walk::Module, a) => self.outer.push((a, n.into(), self.lambdas > 0)),
            (Walk::Block { top: Some(at) }, Anchor::Package(p)) if *at == p => {
                if let Some(Binding::Let(id)) = self.declared.get(n) {
                    self.written.insert((*id, n.into()));
                }
            }
            (Walk::Block { .. }, _) => (),
        }
    }

    /// What the module `id` written here writes outside it.
    fn module(&mut self, id: ExprId, exprs: &[Expr]) {
        if let Walk::Block { .. } = self.walk
            && !self.declared.values().any(|b| matches!(b, Binding::Let(_)))
        {
            return;
        }
        let outer = module_writes(id, exprs, self.known);
        for (a, n, in_lambda) in outer.iter() {
            let lambdas = self.lambdas;
            self.lambdas += *in_lambda as usize;
            match a {
                Anchor::Super(1) => self.write(n, false),
                Anchor::Super(k) => match self.walk {
                    Walk::Module => self.outer.push((
                        Anchor::Super(k - 1),
                        n.clone(),
                        self.lambdas > 0,
                    )),
                    Walk::Block { .. } => (),
                },
                Anchor::Package(_) => self.write_outer(a.clone(), n),
            }
            self.lambdas = lambdas;
        }
        self.known.0.insert(id, outer);
    }

    fn nearer(&mut self, n: CompactString, b: Binding) {
        self.shadow.entry(n.clone()).or_default().push(b);
        self.shadowed.push(n)
    }

    fn bound(&mut self, p: &expr::StructurePattern) {
        p.with_names(&mut |n| self.nearer(n.as_str().into(), Binding::Other));
    }

    fn scoped(&mut self, f: impl FnOnce(&mut Self)) {
        let (len, globs) = (self.shadowed.len(), self.nested_globs);
        f(self);
        self.nested_globs = globs;
        for n in self.shadowed.drain(len..) {
            if let Some(bs) = self.shadow.get_mut(&n) {
                bs.pop();
            }
        }
    }

    fn block(&mut self, exprs: &[Expr]) {
        self.scoped(|w| {
            for e in exprs {
                writes_step();
                match &e.kind {
                    ExprKind::Use { names, .. } => {
                        for item in names.iter() {
                            if item.bound_name().is_none() {
                                w.nested_globs += 1
                            }
                            if let Some((n, b)) = w.imported(item) {
                                w.nearer(n, b)
                            }
                        }
                    }
                    ExprKind::Bind(b) => {
                        w.expr(e);
                        w.bound(&b.pattern)
                    }
                    _ => w.expr(e),
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
                w.lambdas += 1;
                l.args.iter().for_each(|a| w.bound(&a.pattern));
                e.for_each_child(&mut |c| w.expr(c));
                w.lambdas -= 1
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
                w.nearer(c.bind.name.as_str().into(), Binding::Other);
                w.expr(&c.handler)
            }),
            ExprKind::TryWith(t) => {
                self.block(&t.body);
                self.scoped(|w| {
                    w.nearer(t.bind.name.as_str().into(), Binding::Other);
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
            ExprKind::Module { value, .. } => {
                if let ModuleKind::Resolved { exprs, .. } = &**value {
                    self.module(e.id, exprs)
                }
            }
            _ => e.for_each_child(&mut |c| self.expr(c)),
        })
    }
}

/// Defer the names a type definition's body writes (`defer_unresolved_names`),
/// once every name its statement list declares is registered.
pub(crate) fn defer_typedef_names<R: Rt>(
    ctx: &mut CompileCtx<R>,
    n: &Node<R>,
) -> Result<()> {
    let NodeView::TypeDef(td) = n.view() else { return Ok(()) };
    let Some(def) = ctx
        .env
        .lexical
        .typedefs
        .get(&td.scope)
        .and_then(|m| m.get(&CompactString::from(td.name.as_str())))
    else {
        return Ok(());
    };
    let bodies: LPooled<Vec<Type>> =
        iter::once(def.typ().clone()).chain(def.rep.clone()).collect();
    let bounds: LPooled<Vec<Type>> =
        def.params().iter().filter_map(|(_, c)| c.clone()).collect();
    for t in bodies.iter() {
        let before = ctx.pending_names.len();
        if defer_unresolved_names(ctx, t, &td.spec, None)? {
            t.rewrite_trait_args(&ctx.env).at(&td.spec)?;
        }
        if ctx.pending_names.len() > before {
            ctx.pending_names.push(crate::Pending::Body(t.clone(), (*td.spec).clone()));
        }
    }
    for t in bounds.iter() {
        defer_unresolved_names(ctx, t, &td.spec, None)?;
    }
    Ok(())
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
pub(crate) fn compile_statement<R: Rt>(
    ctx: &mut CompileCtx<R>,
    flags: BitFlags<CFlag>,
    e: &Expr,
    scope: &Scope,
    top_id: ExprId,
    at: StmtAt,
) -> Result<(Node<R>, Scope)> {
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
fn update_run<'a, R: Rt>(
    ctx: &mut ExecCtx<'_, R>,
    nodes: &'a mut [Node<R>],
    site: &mut ForkSite,
) -> &'a TagValue {
    fork_point(ctx, nodes, site, update_in_order, |_, last| last)
}

fn update_in_order<'a, R: Rt>(
    ctx: &mut ExecCtx<'_, R>,
    nodes: &'a mut [Node<R>],
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
pub(crate) type Child<'a, R> =
    dyn FnMut(&mut Node<R>, &mut CompileCtx<R>) -> Result<()> + 'a;

/// The solver of the check by bounds this thread is running.
pub(crate) fn solving<T>(f: impl FnOnce(&mut Solving) -> T) -> T {
    bounds::with(f).expect("a check by bounds is running")
}

/// `t`, where it is a cell, may hold a union nobody wrote: a literal's
/// element, a read's result, a call's.
pub(crate) fn joins(t: &Type) {
    if let Type::TVar(tv) = t {
        tv.set_joins()
    }
}

/// A fresh cell that may hold a union nobody wrote.
pub(crate) fn joined_cell() -> TVar {
    let tv = TVar::default();
    tv.set_joins();
    tv
}

/// [`joined_cell`] as a type.
pub(crate) fn joined() -> Type {
    Type::TVar(joined_cell())
}

/// A statement's check result: its errors carry the statement, and in a
/// module's body the module's origin.
fn statement_result<R: Rt>(r: Result<()>, n: &Node<R>, module: bool) -> Result<()> {
    let r = wrap!(n, r);
    match module {
        // CR claude for claude: [readability/low] This adds a frame that prints as "in
        // file F" with no position to every refusal in a module body, an impl's
        // included. The frames around it already name that file, so it says nothing.
        // The method frames under it also print the lambda annotate_lambda rebuilt
        // ("let m: fn(self: i64, x: string) -> str..", "|s: i64, x: i64| -> string x")
        // where the author wrote `let m = |s, x: self| x`. Add the origin only where it
        // differs from the frames' own, and print a rebuilt expression by its source
        // span (ErrorContext::source_text), as is done for lowered ones. probe:
        // design/review-2026-11-16/repro/x-diagnostics-impl-frames.gx
        // (x-diagnostics-12)
        true => r.with_context(|| n.spec().ori.clone()),
        false => r,
    }
}

/// Run `check` over each statement of `work` in its compile task, in
/// parallel, and join the tasks in order: the first error in order is
/// the body's.
fn check_in_tasks<R: Rt>(
    ctx: &mut CompileCtx<R>,
    mut work: LPooled<Vec<(&mut Node<R>, CompileCtx<R>, TaskId)>>,
    module: bool,
    check: impl Fn(&mut Node<R>, &mut CompileCtx<R>) -> Result<()> + Sync,
) -> Result<()> {
    let cx = TaskContext::current();
    let mut results: LPooled<Vec<Result<()>>> = LPooled::take();
    let check_task = |(n, task, id): &mut (&mut Node<R>, CompileCtx<R>, TaskId)| {
        let _cx = cx.with_task(*id).enter();
        let r = task.run_task(|task| check(n, task));
        statement_result(r, n, module)
    };
    // the solver of a check by bounds is this thread's: its tasks run here,
    // one after another
    match ctx.control.eval_pool().filter(|_| !bounds::active()) {
        // CR claude for claude: [structure/low] Three functions fork a `CompileCtx` per
        // part, run the parts on the evaluation pool and join them in order: this one,
        // `fusion::fuse_each` and `branch::compile_each`. Only `compile_each` enters
        // the pool through `branch::on_pool` and `as_part`; this one and `fuse_each`
        // call `EvalPool::map` bare, so a dynamic module's load checked inside a cycle
        // runs its tasks without the runtime's `Control` on their threads (no stack
        // budget, no interrupt) and uncounted by `Control::eval_saturated`. Give the
        // three one body over `&mut CompileCtx` that enters the pool as `compile_each`
        // does. Found by reading; not run. (x-parallel-05)
        Some(pool) => pool.map(&mut work, &mut results, check_task),
        None => results.extend(work.iter_mut().map(check_task)),
    }
    for (_, task, _) in work.drain(..) {
        ctx.join(task);
    }
    results.drain(..).find(|r| r.is_err()).unwrap_or(Ok(()))
}

/// Run a typecheck `pass` over `nodes` in [`evaluation_order`]. A module
/// body's errors also carry each statement's origin, the file it is in.
pub(crate) fn typecheck_in_order<R: Rt>(
    ctx: &mut CompileCtx<R>,
    nodes: &mut [Node<R>],
    catches: &[usize],
    module: bool,
    mut pass: impl FnMut(&mut Node<R>, &mut CompileCtx<R>) -> Result<()>,
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
/// tasks join in order; the first error in order is the block's. A
/// sibling's write through a module's `val` is judged once all are
/// checked (`Module::check_written_vals`).
pub(crate) fn typecheck0_statements<R: Rt>(
    ctx: &mut CompileCtx<R>,
    nodes: &mut [Node<R>],
    catches: &[usize],
    module: bool,
) -> Result<()> {
    let order: LPooled<Vec<usize>> = evaluation_order(nodes.len(), catches).collect();
    let mut slots: LPooled<Vec<Option<&mut Node<R>>>> =
        nodes.iter_mut().map(Some).collect();
    let is_static_module = |n: &Option<&mut Node<R>>| {
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
    drop(slots);
    if siblings.len() >= 2 {
        for n in nodes.iter() {
            if let NodeView::Module(m) = n.view() {
                statement_result(m.check_written_vals(ctx), n, module)?
            }
        }
    }
    Ok(())
}

/// The scope of a module statement.
fn module_path<R: Rt>(n: &Node<R>) -> Option<ModPath> {
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
fn typecheck0_modules<'a, R: Rt>(
    ctx: &mut CompileCtx<R>,
    run: impl Iterator<Item = &'a mut Node<R>>,
    siblings: &[ModPath],
    module: bool,
) -> Result<()> {
    let mut run: LPooled<Vec<&mut Node<R>>> = run.collect();
    let mut work: LPooled<Vec<(&mut Node<R>, CompileCtx<R>, TaskId)>> =
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
/// statement boundary follows a bind at run time. Its cells are top
/// level, as a static bind's are. A refused settle is a
/// compiler bug, logged. Binds in parallel branches run concurrently:
/// one that changes a cell the program's compile created is a race.
pub(crate) fn with_runtime_settles<R: Rt, T>(
    ctx: &mut CompileCtx<R>,
    f: impl FnOnce(&mut CompileCtx<R>) -> Result<T>,
) -> Result<T> {
    ctx.pending_settles.push(crate::SettleFrame::default());
    let names = ctx.pending_names.len();
    let _task = InTask::enter(new_task());
    let _level = AtLevel::enter(Level::TOP);
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
pub(crate) fn defer_settle<R: Rt>(
    ctx: &mut CompileCtx<R>,
    settle: impl FnOnce() -> PendingSettle,
) {
    if ctx.def_gate_depth == 0 && !bounds::active() {
        ctx.pending_settles.last_mut().expect("settle frame").push(settle())
    }
}

/// A map type written in `t`, which makes values of it out of others (a
/// cast's target, a type test), takes keys of distinct runtime forms, as
/// a literal's do: judged at the statement's settle. Every other map
/// value comes from a producer judged already.
pub(crate) fn defer_written_map_keys<R: Rt>(
    ctx: &mut CompileCtx<R>,
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

/// Defer a judgment that only reads types to the check's settle of the
/// current frame, a definition's body included: its open cells read as
/// any type, and a later instance's types are its definition's.
pub(crate) fn defer_judgment<R: Rt>(ctx: &mut CompileCtx<R>, judgment: PendingSettle) {
    ctx.pending_settles.last_mut().expect("settle frame").push(judgment)
}

/// A statement's `typecheck1`, then the settles it deferred.
fn typecheck1_settled<R: Rt>(n: &mut Node<R>, ctx: &mut CompileCtx<R>) -> Result<()> {
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
pub(crate) fn typecheck1_statements<R: Rt>(
    ctx: &mut CompileCtx<R>,
    nodes: &mut [Node<R>],
    catches: &[usize],
    module: bool,
) -> Result<()> {
    let order: LPooled<Vec<usize>> = evaluation_order(nodes.len(), catches).collect();
    if order.len() < 2 || RUNTIME_BIND.get() {
        return typecheck_in_order(ctx, nodes, catches, module, typecheck1_settled);
    }
    let mut slots: LPooled<Vec<Option<&mut Node<R>>>> =
        nodes.iter_mut().map(Some).collect();
    let work: LPooled<Vec<(&mut Node<R>, CompileCtx<R>, TaskId)>> = order
        .iter()
        .map(|i| {
            let n = slots[*i].take().expect("an order visits each once");
            (n, ctx.fork(), new_task())
        })
        .collect();
    check_in_tasks(ctx, work, module, typecheck1_settled)
}

impl<R: Rt> Block<R> {
    /// Whether a `catch` covers the block's value, its last child.
    pub(crate) fn value_is_caught(&self) -> bool {
        !self.catches.is_empty()
    }

    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<'_, R>,
        buf: &mut &[u8],
    ) -> Result<Node<R>, PackError> {
        let module = bool::decode(buf)?;
        let declared = Option::<ModPath>::decode(buf)?;
        let spec = Arc::new(Expr::decode(buf)?);
        let children = decode_nodes(ctx, buf)?.into_boxed_slice();
        let catches = Vec::<usize>::decode(buf)?.into_boxed_slice();
        let runs = Runs::image_decode(buf)?;
        Ok(Self::with(module, declared, spec, children, catches, runs))
    }
}

impl<R: Rt> Update<R> for Block<R> {
    fn for_each_child<'a>(&'a self, f: &mut dyn FnMut(&'a Node<R>)) {
        self.children.iter().for_each(f)
    }

    fn for_each_child_mut(&mut self, f: &mut dyn FnMut(&mut Node<R>)) {
        self.children.iter_mut().for_each(f)
    }

    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::Block, buf);
        self.module.encode(buf)?;
        self.declared.encode(buf)?;
        self.spec.encode(buf)?;
        encode_nodes(&self.children, buf)?;
        image::slice_encode(&self.catches, buf)?;
        self.runs.image_encode(buf)
    }

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R>) {
        self.for_each_child_mut(&mut |c| c.delete(ctx));
        if let Some(scope) = &self.declared {
            ctx.env.unbind_lexical_under(scope);
        }
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R>) -> &TagValue {
        if self.catches.is_empty()
            && self.lets.is_none()
            && !Runs::forks(ctx, self.children.len())
        {
            let res = self
                .children
                .iter_mut()
                .fold(TagValue::phantom_ref(), |_, n| n.update(ctx));
            return if self.module { TagValue::phantom_ref() } else { res };
        }
        let res = self.runs.update(ctx, &mut self.children, &self.catches);
        if Lets::take_mark(&self.lets) {
            match self.module {
                true => {
                    self.reraised = true;
                    self.around.iter().for_each(Lets::mark)
                }
                false => self.take_back_reraised(ctx),
            }
        }
        match res {
            Some(tv) if !self.module => self.resident.set(tv),
            _ => TagValue::phantom_ref(),
        }
    }

    fn typ(&self) -> &Type {
        self.children.last().map(|n| n.typ()).unwrap_or(Type::BOTTOM)
    }

    fn typecheck0(&mut self, ctx: &mut CompileCtx<R>) -> Result<()> {
        typecheck0_statements(ctx, &mut self.children, &self.catches, self.module)
    }

    fn typecheck0_instance(
        &mut self,
        ctx: &mut CompileCtx<R>,
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

    fn typecheck1(&mut self, ctx: &mut CompileCtx<R>) -> Result<()> {
        typecheck1_statements(ctx, &mut self.children, &self.catches, self.module)
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn view(&self) -> NodeView<'_, R> {
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

    fn fuse(&mut self, ctx: &mut CompileCtx<R>) -> Result<Option<Node<R>>> {
        fusion::fuse_each(ctx, self.children.iter_mut(), |n, _, ctx| fuse(n, ctx))?;
        Ok(None)
    }
}

#[derive(Debug)]
pub struct StringInterpolate<R: Rt> {
    /// set by `sleep()`, taken by the next update
    slept: WakeBit,
    pub(crate) spec: Arc<Expr>,
    pub typ: Type,
    pub(crate) typs: Box<[Type]>,
    pub args: Box<[Node<R>]>,
    resident: TagValue,
    pub(crate) fork: ForkSite,
}

impl<R: Rt> StringInterpolate<R> {
    fn with(
        spec: Arc<Expr>,
        typ: Type,
        typs: Box<[Type]>,
        args: Box<[Node<R>]>,
        fork: ForkSite,
    ) -> Node<R> {
        Node::new(Self {
            spec,
            typ,
            typs,
            args,
            resident: TagValue::phantom(),
            slept: WakeBit::default(),
            fork,
        })
    }

    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<'_, R>,
        buf: &mut &[u8],
    ) -> Result<Node<R>, PackError> {
        let spec = Arc::new(Expr::decode(buf)?);
        let typ = Type::decode(buf)?;
        let typs = Vec::<Type>::decode(buf)?.into_boxed_slice();
        let args = decode_nodes(ctx, buf)?.into_boxed_slice();
        let fork = ForkSite::image_decode(buf)?;
        Ok(Self::with(spec, typ, typs, args, fork))
    }

    pub(crate) fn compile(
        ctx: &mut CompileCtx<R>,
        flags: BitFlags<CFlag>,
        spec: Arc<Expr>,
        scope: &Scope,
        top_id: ExprId,
        args: &[Expr],
    ) -> Result<Node<R>> {
        let args: Box<[Node<R>]> = args
            .iter()
            .map(|e| compile(ctx, flags, e.clone(), scope, top_id))
            .collect::<Result<_>>()?;
        let typs = args.iter().map(|n| n.typ().clone()).collect();
        let typ = Type::Primitive(Typ::String.into());
        Ok(Self::with(spec, typ, typs, args, Default::default()))
    }
}

impl<R: Rt> Update<R> for StringInterpolate<R> {
    fn for_each_child<'a>(&'a self, f: &mut dyn FnMut(&'a Node<R>)) {
        self.args.iter().for_each(f)
    }

    fn for_each_child_mut(&mut self, f: &mut dyn FnMut(&mut Node<R>)) {
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

    fn update(&mut self, ctx: &mut ExecCtx<'_, R>) -> &TagValue {
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

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R>) {
        self.slept.set();
        for n in &mut self.args {
            n.sleep(ctx);
        }
    }

    fn fuse(&mut self, ctx: &mut CompileCtx<R>) -> Result<Option<Node<R>>> {
        fusion::fuse_parts(self.args.iter_mut(), ctx)
    }

    typed_by_row!(children);

    fn typecheck1(&mut self, ctx: &mut CompileCtx<R>) -> Result<()> {
        for (i, a) in self.args.iter_mut().enumerate() {
            wrap!(a, a.typecheck1(ctx))?;
            // a cell still open at tc0 is bound by now
            self.typs[i] = part_type(a.typ());
        }
        Ok(())
    }

    fn view(&self) -> NodeView<'_, R> {
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
pub struct Connect<R: Rt> {
    pub(crate) spec: Arc<Expr>,
    pub(super) node: Node<R>,
    pub(crate) id: BindId,
}

/// Where a connect's target name is written, past a `*`.
fn target_pos(spec: &Expr) -> crate::SourcePosition {
    match &spec.kind {
        ExprKind::Connect { at, .. } => at.get().unwrap_or(spec.pos),
        _ => spec.pos,
    }
}

impl<R: Rt> Connect<R> {
    fn with(spec: Arc<Expr>, node: Node<R>, id: BindId) -> Node<R> {
        Node::new(Self { spec, node, id })
    }

    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<'_, R>,
        buf: &mut &[u8],
    ) -> Result<Node<R>, PackError> {
        let spec = Arc::new(Expr::decode(buf)?);
        let node = decode_node(ctx, buf)?;
        let id = BindId::decode(buf)?;
        Ok(Self::with(spec, node, id))
    }

    pub(crate) fn compile(
        ctx: &mut CompileCtx<R>,
        flags: BitFlags<CFlag>,
        spec: Arc<Expr>,
        scope: &Scope,
        top_id: ExprId,
        name: &ModPath,
        value: &Expr,
    ) -> Result<Node<R>> {
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
                scope: scope.lexical.clone(),
                name: name.clone(),
                bind_id: id,
            });
        }
        let mut node = compile(ctx, flags, value.clone(), scope, top_id)?;
        // a generalized binding names what is written to it
        if ctx.env.poly_binds.contains(&id) {
            bind::name_value(&mut node, true);
        }
        Ok(Self::with(spec, node, id))
    }
}

impl<R: Rt> Update<R> for Connect<R> {
    fn for_each_child<'a>(&'a self, f: &mut dyn FnMut(&'a Node<R>)) {
        f(&self.node)
    }

    fn for_each_child_mut(&mut self, f: &mut dyn FnMut(&mut Node<R>)) {
        f(&mut self.node)
    }

    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::Connect, buf);
        self.spec.encode(buf)?;
        self.node.image_encode(buf)?;
        self.id.encode(buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R>) -> &TagValue {
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

    fn fuse(&mut self, ctx: &mut CompileCtx<R>) -> Result<Option<Node<R>>> {
        fusion::fuse_parts([&mut self.node], ctx)
    }

    // The target's type is its binding's row: the write was judged in
    // the check.
    typed_by_row!(source);

    fn view(&self) -> NodeView<'_, R> {
        NodeView::Connect(self)
    }

    fn emit_clif(&self, _cx: &mut BodyCx) -> Result<CompiledExpr> {
        Err(anyhow!("emit_clif: connect is an effect — node-walks"))
    }
}

/// `t` with each open cell that has lower bounds as their union: the type
/// a value written there has, which a declaration can name.
fn lower_shown(t: &Type) -> Type {
    let mut named: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
    t.collect_tvars(&mut named);
    let known: AHashMap<ArcStr, Type> = named
        .iter()
        .filter_map(|(n, tv)| {
            let lower = tv.open_cell()?.cell_lower();
            (!lower.is_empty())
                .then(|| (n.clone(), Type::Set(Arc::from_iter(lower)).normalize()))
        })
        .collect();
    match known.is_empty() {
        true => t.clone(),
        false => t.replace_tvars(&known),
    }
}

/// How a binding names a cell: as its type, or as the type its
/// reference refers to.
#[derive(Clone, Copy, PartialEq)]
enum Role {
    Typ,
    Referent,
}

type Cells<'a> = LPooled<AHashMap<usize, SmallVec<[&'a env::Bind; 2]>>>;

/// The bindings a hint names for the cells a type it writes waits for,
/// by the cell each stands for, those [`Holders::nameable`] admits. The
/// cell of a reference's referent no binding names goes by the bindings
/// holding the reference. `numbered` holds the cells no binding names
/// that a hint wrote, each `'_n` by its place there.
pub(crate) struct Holders<'a> {
    by_cell: Cells<'a>,
    by_referent: Cells<'a>,
    named: SmallVec<[(TVar, &'a env::Bind, Role); 2]>,
    numbered: SmallVec<[usize; 2]>,
}

impl<'a> Holders<'a> {
    /// Each binding's cell but `except`'s, and the cells of a union its
    /// cell is bound to (`x` is `['h, 'r]`: its type waits for theirs).
    pub(crate) fn new(env: &'a env::Env, except: Option<BindId>) -> Self {
        let mut by_cell: Cells<'a> = LPooled::take();
        let mut by_referent: Cells<'a> = LPooled::take();
        let mut unnamed: LPooled<AHashSet<usize>> = LPooled::take();
        for (_, b) in env.by_id.iter() {
            if let (Some(env::Binder::Referent), Type::TVar(tv)) = (&b.binder, &b.typ) {
                unnamed.insert(tv.stands_for());
            }
            if !Self::nameable(b) || Some(b.id) == except {
                continue;
            }
            if let Some(cell) = Self::referent(&b.typ) {
                by_referent.entry(cell).or_default().push(b);
            }
            let Type::TVar(tv) = &b.typ else { continue };
            by_cell.entry(tv.stands_for()).or_default().push(b);
            if let Some(Type::Set(ref ms)) = Type::TVar(tv.clone()).deref_cloned() {
                for m in ms.iter() {
                    if let Type::TVar(m) = m {
                        by_cell.entry(m.stands_for()).or_default().push(b)
                    }
                }
            }
        }
        by_referent.retain(|cell, _| unnamed.contains(cell));
        Self { by_cell, by_referent, named: SmallVec::new(), numbered: SmallVec::new() }
    }

    /// The cell the reference type `t` refers to.
    fn referent(t: &Type) -> Option<usize> {
        let mut t = t.clone();
        loop {
            t = match &t {
                Type::ByRef(_, r) => {
                    return match &**r {
                        Type::TVar(tv) => Some(tv.stands_for()),
                        _ => None,
                    };
                }
                Type::TVar(tv) => tv.binding()?,
                _ => return None,
            }
        }
    }

    /// Whether a hint may name `b`: what a hint asks to annotate is built
    /// before a select's arms, and the type of a lowering's binding or a
    /// reference's referent cannot be written.
    fn nameable(b: &env::Bind) -> bool {
        !crate::is_lowered(&b.name)
            && !matches!(
                b.binder,
                Some(env::Binder::SelectPattern(_) | env::Binder::Referent)
            )
    }

    /// Whether a hint names `tv`: a cell no value was written to whose
    /// type still waits for a cell.
    fn waits(tv: &TVar) -> bool {
        !tv.open_cell().is_some_and(|c| !c.cell_lower().is_empty())
            && Type::TVar(tv.clone()).has_unbound()
    }

    /// The binding written first among those of `cells` `prefer` admits
    /// that go by the cell `tv` stands for, else among them all.
    fn first(
        cells: &Cells<'a>,
        tv: &TVar,
        prefer: impl Fn(&env::Bind) -> bool,
    ) -> Option<&'a env::Bind> {
        let holders = cells.get(&tv.stands_for())?;
        let first = |bs: &mut dyn Iterator<Item = &&'a env::Bind>| {
            bs.min_by_key(|b| (b.pos.line, b.pos.column)).copied()
        };
        first(&mut holders.iter().filter(|b| prefer(b)))
            .or_else(|| first(&mut holders.iter()))
    }

    /// The binding written first among those `prefer` admits whose type
    /// is the cell `tv` stands for, else among them all.
    fn holder(
        &self,
        tv: &TVar,
        prefer: impl Fn(&env::Bind) -> bool,
    ) -> Option<&'a env::Bind> {
        Self::first(&self.by_cell, tv, prefer)
    }

    /// `'x` for the binding `x`, standing for what `role` says.
    fn name(&mut self, b: &'a env::Bind, role: Role) -> Type {
        let named =
            |(_, n, how): &&(TVar, &env::Bind, Role)| std::ptr::eq(*n, b) && *how == role;
        if let Some((name, _, _)) = self.named.iter().find(named) {
            return Type::TVar(name.clone());
        }
        let mut name = ArcStr::from(b.name.as_str());
        let mut i = 1;
        while self.named.iter().any(|(n, _, _)| n.name == name) {
            i += 1;
            name = ArcStr::from(format_compact!("{}{i}", b.name).as_str());
        }
        let name = TVar::empty_named(name);
        self.named.push((name.clone(), b, role));
        Type::TVar(name)
    }

    /// The cell `tv` stands for named after its binding, else after the
    /// binding holding the reference it is the unnamed referent of.
    pub(crate) fn name_cell(&mut self, tv: &TVar) -> Option<Type> {
        match self.holder(tv, |_| true) {
            Some(b) => Some(self.name(b, Role::Typ)),
            None => Self::first(&self.by_referent, tv, |_| true)
                .map(|b| self.name(b, Role::Referent)),
        }
    }

    /// The cell `tv` stands for, which no binding names, as `'_1`, `'_2`
    /// and so on in the order a hint meets them.
    pub(crate) fn number(&mut self, tv: &TVar) -> Type {
        let cell = tv.stands_for();
        let i = match self.numbered.iter().position(|c| *c == cell) {
            Some(i) => i,
            None => {
                self.numbered.push(cell);
                self.numbered.len() - 1
            }
        };
        Type::TVar(TVar::empty_named(format_compact!("_{}", i + 1).as_str().into()))
    }

    /// The binding to annotate where the cell `tv` stands for is a part of
    /// a binding's type where the select `at` tests it, named: among the
    /// bindings [`Self::nameable`] admits whose type holds the cell outside
    /// a function type, the nearest one written above `at`, else the one
    /// written first.
    pub(crate) fn name_part(
        &mut self,
        env: &'a env::Env,
        tv: &TVar,
        at: &Expr,
    ) -> Option<Type> {
        fn holds(t: &Type, cell: usize, seen: &mut AHashSet<usize>) -> bool {
            crate::stack::ensure_sufficient(|| match t {
                Type::TVar(tv) if tv.stands_for() == cell => true,
                t if t.shared_identity().is_some_and(|n| !seen.insert(n)) => false,
                Type::TVar(tv) => tv.binding().is_some_and(|b| holds(&b, cell, seen)),
                Type::Fn(_) => false,
                t => {
                    let mut found = false;
                    t.for_each_child(&mut |c| found = found || holds(c, cell, seen));
                    found
                }
            })
        }
        let cell = tv.stands_for();
        let held = || {
            env.by_id.iter().map(|(_, b)| &**b).filter(|b| {
                Self::nameable(b) && holds(&b.typ, cell, &mut LPooled::take())
            })
        };
        let pos = |b: &env::Bind| (b.pos.line, b.pos.column);
        let b = held()
            .filter(|b| *b.ori == *at.ori && pos(b) <= (at.pos.line, at.pos.column))
            .max_by_key(|b| pos(b))
            .or_else(|| held().min_by_key(|b| pos(b)))?;
        Some(self.name(b, Role::Typ))
    }

    /// [`Self::name_cell`] where a hint names the cell.
    pub(crate) fn name_open(&mut self, tv: &TVar) -> Option<Type> {
        match Self::waits(tv) {
            true => self.name_cell(tv),
            false => None,
        }
    }

    /// What each name stands for: `, writing for 'x the type of `x`
    /// (line 3, column 5)`, or `the type `r` refers to`, and last `for '_1
    /// and '_2 types no binding names`.
    pub(crate) fn meanings(&self, at: &Origin) -> CompactString {
        let mut out = CompactString::new("");
        let lead = |i: usize| match i {
            0 => ", writing for ",
            _ => " and for ",
        };
        for (i, (name, b, role)) in self.named.iter().enumerate() {
            let sep = lead(i);
            let (name, at) = (&name.name, BoundAt(b, Some(at)));
            out.push_str(&match role {
                Role::Typ => {
                    format_compact!("{sep}'{name} the type of `{}` ({at})", b.name)
                }
                Role::Referent => {
                    format_compact!("{sep}'{name} the type `{}` refers to ({at})", b.name)
                }
            });
        }
        let n = self.numbered.len();
        for i in 0..n {
            let sep = match i {
                0 => lead(self.named.len()),
                i if i + 1 == n => " and ",
                _ => ", ",
            };
            out.push_str(&format_compact!("{sep}'_{}", i + 1));
        }
        out.push_str(match n {
            0 => "",
            1 => " a type no binding names, annotated where its value is built",
            _ => " types no binding names, annotated where their values are built",
        });
        out
    }
}

/// `held` with `cell`, which a use decided, as the union of what was
/// built into it and `part`, written where it is: what a declaration
/// holding both writes, `Some(None)` where it has no written form. `None`
/// where that union waits for, or holds a value of, a cell of a binding
/// other than the built values', as it does where what is written holds
/// the decided type itself.
fn decided_decl(
    held: &Type,
    cell: &TVar,
    part: Type,
    holders: &mut Holders,
) -> Option<Option<Type>> {
    let mut built = cell.cell_lower();
    let mut in_built: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
    for t in built.iter() {
        t.collect_tvars(&mut in_built)
    }
    let in_built: LPooled<AHashSet<usize>> =
        in_built.values().map(|tv| tv.stands_for()).collect();
    let built_by = |b: &env::Bind| match &b.typ {
        Type::TVar(tv) => in_built.contains(&tv.stands_for()),
        _ => false,
    };
    built.push(part);
    let mut foreign = false;
    let built = Type::Set(Arc::from_iter(built)).written_form(&mut |tv| {
        let lowered = tv.open_cell().is_some_and(|c| !c.cell_lower().is_empty());
        if !lowered && !Holders::waits(tv) {
            return None;
        }
        match holders.holder(tv, built_by) {
            Some(b) if built_by(b) && !lowered => Some(holders.name(b, Role::Typ)),
            Some(b) if built_by(b) => None,
            Some(_) => {
                foreign = true;
                None
            }
            None => {
                foreign |= !lowered && tv.open_cell().is_some();
                None
            }
        }
    });
    if foreign {
        return None;
    }
    let addr = cell.stands_for();
    Some(built.and_then(|built| {
        held.written_form(&mut |tv| match tv.stands_for() == addr {
            true => Some(built.clone()),
            false => holders.name_open(tv),
        })
    }))
}

/// The union of two applications of one Graphix-minted abstract type
/// as the application of each parameter's union: a construction's payload
/// decides its parameters, and the payload is read through one
/// application.
fn params_union(env: &env::Env, held: &Type, written: &Type) -> Option<Type> {
    match (held, written) {
        (
            Type::Abstract { id: i0, params: p0 },
            Type::Abstract { id: i1, params: p1 },
        ) if i0 == i1 && p0.len() == p1.len() && env.abstract_minted(*i0) => {
            Some(Type::Abstract {
                id: *i0,
                params: Arc::from_iter(
                    p0.iter()
                        .zip(p1.iter())
                        .map(|(a, b)| Type::Set(Arc::from_iter([a.clone(), b.clone()]))),
                ),
            })
        }
        _ => None,
    }
}

/// What a refusal of a write to `bind`, a generic binding, calls it (its
/// module path, where a module holds it) and the hint that ends the
/// refusal: the annotation that lets the write pass, none for a package's
/// binding, which the program cannot annotate.
pub(crate) fn generic_target(
    env: &env::Env,
    bind: &env::Bind,
) -> (CompactString, &'static str) {
    let mut parts = netidx_core::path::Path::parts(&bind.scope.0)
        .filter(|p| !graphix_types::is_block_component(p))
        .peekable();
    let package = parts.peek().is_some_and(|p| env.package_roots.contains(*p));
    let mut name = CompactString::new("");
    for p in parts {
        name.push_str(p);
        name.push_str("::");
    }
    name.push_str(&bind.name);
    let hint = match package {
        true => "",
        false => {
            "; to write it at one type, write that type on its let (on its val, where an \
             interface declares it)"
        }
    };
    (name, hint)
}

/// A written `<-` whose value `bind` cannot hold: the target, its type,
/// and the declaration that would hold both. `reads_target`: what is
/// written reads `bind`.
fn write_mismatch(
    env: &env::Env,
    bind: &env::Bind,
    written: &Type,
    reads_target: bool,
    at: &Arc<Origin>,
) -> anyhow::Error {
    let deref = |t: &Type| t.deref_cloned().unwrap_or_else(|| t.clone());
    let (held, written) = (deref(&bind.typ), deref(written));
    let infinite = held.meets_itself(&written);
    // what an earlier input left open was settled to ⊥ at its end: the
    // declaration writes the type there
    let settled = bind.typ.fill_settled(&written).filter(|t| {
        t.contains_with_flags(BitFlags::empty(), env, &written).unwrap_or(false)
    });
    let inferred = match &bind.typ {
        _ if settled.is_some() => ", settled when its input ended,",
        Type::TVar(tv) if tv.bottom_fed() => ", inferred from an earlier use,",
        _ => "",
    };
    let name = &bind.name;
    // a type variable is declared, and no declaration widens it; nor does
    // one widen what holds a function, as two function types share a
    // runtime form; what was settled is declared as what is written
    fn holds_fn(t: &Type, seen: &mut AHashSet<NodeKey>) -> bool {
        crate::stack::ensure_sufficient(|| {
            if t.node_key().is_some_and(|k| !seen.insert(k)) {
                return false;
            }
            match t {
                Type::Fn(_) => true,
                Type::TVar(tv) => tv.binding().is_some_and(|b| holds_fn(&b, seen)),
                t => {
                    let mut any = false;
                    t.for_each_child(&mut |c| any = any || holds_fn(c, seen));
                    any
                }
            }
        })
    }
    let holds_fn = |t: &Type| holds_fn(t, &mut LPooled::take());
    if !infinite
        && settled.is_none()
        && (matches!(held, Type::TVar(_)) || holds_fn(&held) || holds_fn(&written))
    {
        return format_with_flags(
            PrintFlag::DerefTVars | PrintFlag::ReplacePrims,
            || {
                anyhow!(
                    "{name} is {} and cannot hold {}",
                    Brief(&held),
                    Brief(&lower_shown(&written))
                )
            },
        );
    }
    let refusal = |declare: &str, meanings: &str| {
        format_with_flags(PrintFlag::DerefTVars | PrintFlag::ReplacePrims, || {
            format_compact!(
                "{name} is {}{inferred} and cannot hold {}; {declare} ({}){meanings}",
                Brief(&held),
                Brief(&lower_shown(&written)),
                BoundAt(bind, Some(at))
            )
        })
    };
    let recursive = format_compact!(
        "no finite type holds both: declare a named recursive type for {name} where it \
         is bound"
    );
    let mut holders = Holders::new(env, Some(bind.id));
    let finite = match infinite {
        true => held
            .decided_part(&written)
            .and_then(|(cell, part)| decided_decl(&held, &cell, part, &mut holders))
            .map(|decl| {
                let decl = match decl {
                    Some(decl) => format_compact!("declare {name}: {decl}"),
                    None => {
                        holders.named.clear();
                        format_compact!("declare {name} with a type holding both")
                    }
                };
                format_compact!(
                    "a use before this write decided {name}'s type from what was built \
                     into it then: {decl} where it is bound"
                )
            }),
        false => {
            // CR claude for claude: [bug/medium] When the written value's type is a
            // definition's declared variable (`'a: Any |x: 'a| -> 'a { y <- x; x }`
            // over `let y: i64`), this hint names the variable after its binding as if
            // it were an open cell: "declare y: [i64, 'x] where it is bound, writing
            // for 'x the type of `x`". That declaration is then refused by the escape
            // rule ("'a escapes the definition ... the bound of 'a, or Any"); only Any
            // or the bound passes. A trait method's variable gets the same hint
            // (lang::types::rigid_survives_alias pins only "cannot hold"). Where the
            // written type holds a cell a declaration holds rigid, give the escape
            // refusal's remedy instead (Holders::name_open must not name a rigid cell),
            // and pin the hint with an Any twin that runs. probe:
            // design/review-2026-11-16/repro/tests-types-a-escape-hint-declares-a-quantifier.sh
            // (tests-types-a-02)
            let write = |t: &Type, holders: &mut Holders| {
                t.written_form(&mut |tv| holders.name_open(tv))
                    .filter(|t| Brief::within(t, Brief::DECLARATION).is_some())
            };
            // a parameter decided apart is held by no union of parameters
            let both = match settled.as_ref().and_then(|t| write(t, &mut holders)) {
                Some(t) => Some(t),
                // CR claude for claude: [bug/medium] params_union offers `B<[p0, p1]>`
                // whenever the target and the written value apply one minted abstract.
                // An abstract's parameters are invariant, so that declaration holds the
                // written value only while its parameter is still open, as in a
                // construction written directly (errors.rs
                // a_write_hint_names_a_writable_type). Where a use has decided the
                // written value's parameter, following the hint is refused again: `let
                // b: B<i64> = B(1); let c: B<string> = B("s"); b <- c` says `declare b:
                // B<[i64, string]>`, and with b so declared it is refused with `declare
                // b: [B<string>, B<[i64, string]>]`.
                // lang::types::payload_write_declares_the_parameters_union pins this
                // wrong hint: its select over c.0 decides c's parameter, and only
                // `[B<Array<Array<i64>>>, B<Array<i64>>]` checks there. Fix: offer the
                // parameter union only where a non-binding probe shows it holds both
                // types, and otherwise fall through to the union of the two
                // applications. Then repin that fixture and give it a declared twin.
                // probe:
                // design/review-2026-11-16/repro/tests-types-b-params-union-hint.gx
                // (tests-types-b-01)
                None => match params_union(env, &held, &written)
                    .and_then(|t| write(&t, &mut holders))
                {
                    Some(t)
                        if held.written_form(&mut |_| None).is_none_or(|h| h != t) =>
                    {
                        Some(t)
                    }
                    _ => {
                        holders.named.clear();
                        let both =
                            Type::Set(Arc::from_iter([held.clone(), written.clone()]));
                        write(&both, &mut holders)
                    }
                },
            };
            if both.is_none() {
                holders.named.clear()
            }
            Some(match (&bind.binder, both) {
                // a name a destructuring let binds is declared by the
                // pattern's type
                (Some(env::Binder::LetPattern), Some(both)) => format_compact!(
                    "annotate the let's pattern type with {name}'s position as {both}"
                ),
                (Some(env::Binder::LetPattern), None) => format_compact!(
                    "annotate the let's pattern type with a type holding both at \
                     {name}'s position"
                ),
                // CR claude for claude: [bug/medium] This remedy declares the binding
                // as the union of what it holds and what is written, without asking
                // whether the use that decided its type holds that union, so it is
                // wrong wherever a consumer decided it. After `let r = f(&mut z)` over
                // `'a: &mut [i64, null]`, `z <- "hello"` is told `declare z: [i64,
                // null, string]`, and with that declaration the call is refused. The
                // same happens with a union bound's member (`f((z, 1))` over `'a:
                // [(i64, i64), (string, string)]`, told `declare z: [i64, f64]`) and
                // with arithmetic (`let y = x + 1; x <- "s"`, told `declare x: [i64,
                // string]`, then `cannot compute`). CLAUDE.md asks a refusal to name
                // the annotation that makes the program pass. Check the declaration on
                // copies against the uses the binding met (as
                // callsite::annotation_may_fit does for arguments) and name it only
                // where it fits; otherwise name the use that decided the type.
                // lang::types::writable_reference_bound_decides_the_referent and
                // slice_in_each_alternative_of_an_or_arm_used_as_a_number_then_written_a_string_refused
                // carry the wrong advice today; their pins stop before it. probe:
                // design/review-2026-11-16/repro/tests-types-c-declare-remedy.sh
                // (tests-types-c-02)
                (_, Some(both)) => {
                    format_compact!("declare {name}: {both} where it is bound")
                }
                (_, None) => format_compact!(
                    "declare {name} with a type holding both where it is bound"
                ),
            })
        }
    };
    // a finite type holds both only where what is written holds no value
    // of the target
    let may_hold_target = infinite || written.nests(&held);
    match finite {
        Some(_) if may_hold_target && reads_target => {
            anyhow!("{}", refusal(&recursive, ""))
        }
        // a use decided the target's type, and what is written may read it
        Some(finite) if infinite => anyhow!(
            "{}. If what is written reads {name} (through a function, a reference or \
             another binding), {recursive}",
            refusal(&finite, &holders.meanings(at))
        ),
        Some(finite) => anyhow!("{}", refusal(&finite, &holders.meanings(at))),
        None => anyhow!("{}", refusal(&recursive, "")),
    }
}

/// Where a write through a reference lands: a bound variable, or a
/// place inside one (`design/place_references.md`).
#[derive(Debug, Clone, PartialEq)]
pub enum WriteTarget {
    Bind(BindId),
    Place(BindId, place::Path),
}

#[derive(Debug)]
pub struct ConnectDeref<R: Rt> {
    pub(crate) spec: Arc<Expr>,
    pub(super) rhs: Node<R>,
    pub(crate) src_id: BindId,
    /// The reference's cell; where it writes is resolved at each write,
    /// so a moving place writes where its key names then.
    pub(super) cell: Option<BindId>,
    pub(super) top_id: ExprId,
    /// The value the RHS wrote before the reference arrived: its first
    /// target lands it. A reference that arrived naming no place (a key
    /// that is bottom) drops the write, as any operation drops a bottom
    /// input.
    pending: Option<Value>,
    slept: WakeBit,
}

impl<R: Rt> ConnectDeref<R> {
    fn with(spec: Arc<Expr>, rhs: Node<R>, src_id: BindId, top_id: ExprId) -> Node<R> {
        Node::new(Self {
            spec,
            rhs,
            src_id,
            cell: None,
            top_id,
            pending: None,
            slept: WakeBit::default(),
        })
    }

    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<'_, R>,
        buf: &mut &[u8],
    ) -> Result<Node<R>, PackError> {
        let spec = Arc::new(Expr::decode(buf)?);
        let rhs = decode_node(ctx, buf)?;
        let src_id = BindId::decode(buf)?;
        let top_id = ExprId::decode(buf)?;
        let reader = ctx.late_reader(top_id);
        ctx.record_reader(src_id, reader);
        Ok(Self::with(spec, rhs, src_id, top_id))
    }

    pub(crate) fn compile(
        ctx: &mut CompileCtx<R>,
        flags: BitFlags<CFlag>,
        spec: Arc<Expr>,
        scope: &Scope,
        top_id: ExprId,
        name: &ModPath,
        value: &Expr,
    ) -> Result<Node<R>> {
        let src_id =
            match ctx.env.lookup_bind(&scope.lexical, name).map_err(|e| e.at(&spec))? {
                None => bailat!(spec, "{name} is undefined"),
                Some((_, b)) => b.id,
            };
        if ctx.env.ide.is_lsp() {
            ctx.env.push_reference(ReferenceSite {
                pos: target_pos(&spec),
                ori: spec.ori.clone(),
                scope: scope.lexical.clone(),
                name: name.clone(),
                bind_id: src_id,
            });
        }
        let reader = ctx.late_reader(top_id);
        ctx.record_reader(src_id, reader);
        let rhs = compile(ctx, flags, value.clone(), scope, top_id)?;
        Ok(Self::with(spec, rhs, src_id, top_id))
    }

    /// The cell a delivered reference value names; where a write lands
    /// is resolved from it at the write (`ExecCtx::write_target`).
    fn cell(tv: &TagValue) -> Option<BindId> {
        tv.with_value(bind::ref_cell)
    }
}

impl<R: Rt> Update<R> for ConnectDeref<R> {
    fn for_each_child<'a>(&'a self, f: &mut dyn FnMut(&'a Node<R>)) {
        f(&self.rhs)
    }

    fn for_each_child_mut(&mut self, f: &mut dyn FnMut(&mut Node<R>)) {
        f(&mut self.rhs)
    }

    /// `target` is resolved at update: a resolved one is runtime state.
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        if self.cell.is_some() || self.pending.is_some() {
            return Err(PackError::Application(image::NOT_QUIESCENT));
        }
        put_tag(NodeTag::ConnectDeref, buf);
        self.spec.encode(buf)?;
        self.rhs.image_encode(buf)?;
        self.src_id.encode(buf)?;
        self.top_id.encode(buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R>) -> &TagValue {
        // a fired RHS writes; a write before the reference arrived lands at
        // its first target; a bottom RHS or address never writes
        // CR claude for eric: [bug/low] A write kept in `pending` because the reference
        // had not arrived is made at the arm's wake when the reference arrived while
        // the arm slept: `*r <- once(clk) ~ 77` writes in the wake cycle, in which
        // neither the right side nor the reference fires. This is the banked `~`
        // question (Sample::update) at a second site, and it takes the same ruling: if
        // a wake fires nothing, drop `pending` in `sleep`; if the debt is meant to be
        // paid, say so in place_references.md and pin it. probe:
        // design/review-2026-11-16/repro/x-sleep-wake-deref-pending-write-at-wake.gx
        // (x-sleep-wake-05)
        let woke = self.slept.take();
        let written = {
            let tv = self.rhs.update(ctx);
            let t = tv.tag();
            (t.is_fired() && !t.is_bottom()).then(|| tv.value_cloned())
        };
        if let crate::branch::RtView::Fork(f) = &ctx.rt {
            f.note_read(self.src_id);
        }
        // a reference that arrived naming no place (a place whose address
        // is bottom) has nowhere to write
        // CR claude for eric: [bug/medium] A write made before its reference arrives is
        // held only while the reference binding has no standing value. A function
        // formal over an argument that has not produced yet is delivered as bottom at
        // the call's birth. read_var then finds it standing, Self::cell gives None, and
        // addressless clears pending, so the write is dropped. Example: `let f = |r:
        // &mut i64| { *r <- v; 0 }; f(rb)` with rb = never<&mut i64>() written at step
        // 3 and v firing at step 1 never writes, while `{ let r = ra; *r <- v }` over
        // the same kind of reference lands at the first target. A top-level `let rr =
        // select .. { _ => never() }` drops it as well, the case the
        // write_before_reference_arrives pin calls a bottom reference. Whether a write
        // waits now depends on whether a bottom was delivered, which the text does not
        // show. Decide where 'not yet arrived' ends (a parameter, a select over
        // never()), make addressless follow that rule, and pin the let and formal
        // spellings together. probe:
        // design/review-2026-11-16/repro/x-rerun-2-prearrival-write-formal.gx
        // (x-rerun-2-02)
        let mut addressless = false;
        if let Some(tv) = ctx
            .event
            .variables
            .get(&self.src_id)
            .or_else(|| late_fire(ctx, &self.src_id, self.top_id, None))
        {
            self.cell = Self::cell(tv);
            addressless = self.cell.is_none();
        } else if self.cell.is_none() || woke {
            // an instance created after the reference value was
            // delivered, or woken after it moved, finds it standing
            if let Some(read) = read_var(ctx, &self.src_id) {
                let tv = match read {
                    VarRead::Delivered(tv)
                    | VarRead::Late(tv)
                    | VarRead::Standing((tv, _)) => tv,
                };
                self.cell = Self::cell(tv);
                addressless = self.cell.is_none();
            }
        }
        // CR claude for eric: [bug/medium] Whether a write fired while the reference
        // has no value lands later depends on how the reference came to have none. A
        // reference nothing was ever delivered to (`read_var` answers `None`: a sample
        // not yet fired, a timer) keeps the write in `pending` for its first target;
        // one that holds a delivered bottom (a select's `never()` arm, `k$` over null)
        // sets `addressless` and drops it. `let r = select c { k if k >= 4 => k ~ &mut
        // x, _ => never() }` and `let go = ..; let r = go ~ &mut x` are the same stream
        // of values, and a `*r <- 99` fired before either arrives writes x through the
        // second only. representable_bottom.md has one bottom and no pending channel,
        // and dense_delivery.md reads an absent store entry as the phantom. Rule one
        // answer for both: the write waits for the reference's first value however it
        // is absent, or it is dropped. place_references.md and
        // `lang::byref::write_before_reference_arrives` pin the split today. probe:
        // design/review-2026-11-16/repro/c-node-mod-deref-write-absent-vs-bottom.sh
        // (c-node-mod-02)
        if addressless {
            self.pending = None;
        } else if written.is_some() {
            self.pending = written;
        }
        if let Some(cell) = self.cell
            && let Some(v) = self.pending.take()
        {
            match ctx.write_target(cell) {
                Some(WriteTarget::Bind(id)) => ctx.rt.set_var(id, v),
                Some(WriteTarget::Place(root, path)) => {
                    ctx.rt.patch_var(root, path, v, Some(self.spec.clone()))
                }
                None => (),
            }
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

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R>) {
        let reader = ctx.late_reader(self.top_id);
        ctx.unref_reader(self.src_id, reader);
        self.rhs.delete(ctx)
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R>) {
        self.slept.set();
        self.rhs.sleep(ctx);
    }

    fn fuse(&mut self, ctx: &mut CompileCtx<R>) -> Result<Option<Node<R>>> {
        fusion::fuse_parts([&mut self.rhs], ctx)
    }

    // The target's type is its binding's row: the write was judged in
    // the check.
    typed_by_row!(source);

    fn view(&self) -> NodeView<'_, R> {
        NodeView::ConnectDeref(self)
    }
}

#[derive(Debug)]
pub struct TypeCast<R: Rt> {
    slept: WakeBit,
    pub(crate) spec: Arc<Expr>,
    pub typ: Type,
    pub target: Type,
    pub n: Node<R>,
    /// The source can hold a reference: every value casts to the cast
    /// error. Known only once the source's type is, so the check refuses
    /// what it can see and an instance of a generic body fails here.
    src_ref: bool,
    pub(crate) check_keys: bool,
    resident: TagValue,
}

impl<R: Rt> TypeCast<R> {
    fn with(
        spec: Arc<Expr>,
        typ: Type,
        target: Type,
        n: Node<R>,
        src_ref: bool,
        check_keys: bool,
    ) -> Node<R> {
        Node::new(Self {
            slept: WakeBit::default(),
            spec,
            typ,
            target,
            n,
            src_ref,
            check_keys,
            resident: TagValue::phantom(),
        })
    }

    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<'_, R>,
        buf: &mut &[u8],
    ) -> Result<Node<R>, PackError> {
        let spec = Arc::new(Expr::decode(buf)?);
        let typ = Type::decode(buf)?;
        let target = Type::decode(buf)?;
        let n = decode_node(ctx, buf)?;
        let src_ref = n.typ().holds_ref(&ctx.env);
        let check_keys = target.holds_abstract(&ctx.env);
        Ok(Self::with(spec, typ, target, n, src_ref, check_keys))
    }

    pub(crate) fn compile(
        ctx: &mut CompileCtx<R>,
        flags: BitFlags<CFlag>,
        spec: Arc<Expr>,
        scope: &Scope,
        top_id: ExprId,
        expr: &Expr,
        typ: &Type,
    ) -> Result<Node<R>> {
        let n = compile(ctx, flags, expr.clone(), scope, top_id)?;
        let target = typ.scope_refs(&scope.lexical);
        crate::warn_later_block_names(ctx, flags, &target)?;
        target.refuse_quantified_fns(&ctx.env).at(&spec)?;
        target.check_cast(&ctx.env).at(&spec)?;
        let typ = Type::union(&ctx.env, &[&target, &CAST_ERR])?;
        Ok(Self::with(spec, typ, target, n, false, true))
    }
}

impl<R: Rt> Update<R> for TypeCast<R> {
    fn for_each_child<'a>(&'a self, f: &mut dyn FnMut(&'a Node<R>)) {
        f(&self.n)
    }

    fn for_each_child_mut(&mut self, f: &mut dyn FnMut(&mut Node<R>)) {
        f(&mut self.n)
    }

    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::TypeCast, buf);
        self.spec.encode(buf)?;
        self.typ.encode(buf)?;
        self.target.encode(buf)?;
        self.n.image_encode(buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R>) -> &TagValue {
        let tv = self.n.update(ctx);
        let tag = tv.tag();
        dense_gate!(self, tag, tag.is_bottom());
        let v = tv.value_cloned();
        let v = if self.src_ref {
            errf!(CAST_ERR_TAG, "can't cast a reference")
        } else {
            let (target, src) = (&self.target, self.n.typ());
            // CR claude for claude: [bug/medium] A cast whose `from_value` hook was cut
            // off by the activation or stack budget still produces a value: the deepest
            // hook rides bottom, `Type::cast_abstract` makes "its Cast implementation
            // produced nothing" of it, and each level of the recursion above wraps the
            // refusal of the level below and keeps it. The unwind's memory is quadratic
            // in the budget, so `impl Cast for T { let from_value = |v| cast<T>(v) }`
            // is OOM-killed under a 50000 budget where a plain recursion stops as
            // ActivationBudget. Ride the resident when `ctx.interrupted()` after the
            // cast, as `GXLambda::update` does, here and in `TypedRead::map_value`.
            // probe:
            // design/review-2026-11-16/repro/x-traits-abstract-from-value-recursion-uncontained.gx
            // (x-traits-abstract-04)
            coretraits::with_display_hooks(ctx, |env| {
                target
                    .cast_with(env, Some(src), v, CastInput::Data, self.check_keys)
                    .unwrap_or_else(|e| e)
            })
        };
        self.resident.set(TagValue::tagged(v, tag))
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn typ(&self) -> &Type {
        &self.typ
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R>) {
        self.slept.set();
        self.n.sleep(ctx);
    }

    fn fuse(&mut self, ctx: &mut CompileCtx<R>) -> Result<Option<Node<R>>> {
        fusion::fuse_parts([&mut self.n], ctx)
    }

    fn typecheck0(&mut self, ctx: &mut CompileCtx<R>) -> Result<()> {
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
        ctx: &mut CompileCtx<R>,
        types: &mut lambda::InstanceTypes,
    ) -> Result<()> {
        self.typecheck0_with(ctx, &mut |n, ctx| n.typecheck0_instance(ctx, types))
    }

    fn typecheck1(&mut self, ctx: &mut CompileCtx<R>) -> Result<()> {
        wrap!(self.n, self.n.typecheck1(ctx))?;
        self.src_ref = self.n.typ().holds_ref(&ctx.env);
        self.check_keys = self.target.holds_abstract(&ctx.env);
        Ok(())
    }

    fn view(&self) -> NodeView<'_, R> {
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
pub struct Never<R: Rt> {
    pub(crate) spec: Arc<Expr>,
    pub typ: Type,
    pub n: Box<[Node<R>]>,
}

impl<R: Rt> Never<R> {
    fn with(spec: Arc<Expr>, typ: Type, n: Box<[Node<R>]>) -> Node<R> {
        Node::new(Self { spec, typ, n })
    }

    pub(crate) fn compile(
        ctx: &mut CompileCtx<R>,
        flags: BitFlags<CFlag>,
        spec: Arc<Expr>,
        scope: &Scope,
        top_id: ExprId,
        typ: &Option<Arc<Type>>,
        args: &[Expr],
    ) -> Result<Node<R>> {
        let n = args
            .iter()
            .map(|e| compile(ctx, flags, e.clone(), scope, top_id))
            .collect::<Result<Box<[_]>>>()?;
        let typ = match typ {
            Some(t) => {
                let t = t.scope_refs(&scope.lexical);
                crate::warn_later_block_names(ctx, flags, &t)?;
                let t = t.rewrite_trait_args(&ctx.env)?;
                lambda::body_annotation(ctx, &t, true)?;
                t
            }
            None => Type::Bottom,
        };
        Ok(Self::with(spec, typ, n))
    }
}

impl<R: Rt> Never<R> {
    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<'_, R>,
        buf: &mut &[u8],
    ) -> Result<Node<R>, PackError> {
        let spec = Arc::new(Expr::decode(buf)?);
        let typ = Type::decode(buf)?;
        let n = decode_nodes(ctx, buf)?.into_boxed_slice();
        Ok(Self::with(spec, typ, n))
    }
}

impl<R: Rt> Update<R> for Never<R> {
    fn for_each_child<'a>(&'a self, f: &mut dyn FnMut(&'a Node<R>)) {
        self.n.iter().for_each(f)
    }

    fn for_each_child_mut(&mut self, f: &mut dyn FnMut(&mut Node<R>)) {
        self.n.iter_mut().for_each(f)
    }

    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::Never, buf);
        self.spec.encode(buf)?;
        self.typ.encode(buf)?;
        encode_nodes(&self.n, buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R>) -> &TagValue {
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

    fn fuse(&mut self, ctx: &mut CompileCtx<R>) -> Result<Option<Node<R>>> {
        fusion::fuse_parts(self.n.iter_mut(), ctx)
    }

    fn typecheck0(&mut self, ctx: &mut CompileCtx<R>) -> Result<()> {
        defer_unresolved_names(ctx, &self.typ, &self.spec, None)?;
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
        ctx: &mut CompileCtx<R>,
        types: &mut lambda::InstanceTypes,
    ) -> Result<()> {
        self.typecheck0_with(ctx, &mut |n, ctx| n.typecheck0_instance(ctx, types))
    }

    fn view(&self) -> NodeView<'_, R> {
        NodeView::Never(self)
    }
}

#[derive(Debug)]
pub struct Any<R: Rt> {
    pub(crate) spec: Arc<Expr>,
    pub typ: Type,
    pub n: Box<[Node<R>]>,
    resident: TagValue,
    /// The child whose production the resident is.
    from: Option<usize>,
    slept: WakeBit,
}

impl<R: Rt> Any<R> {
    fn with(spec: Arc<Expr>, typ: Type, n: Box<[Node<R>]>) -> Node<R> {
        Node::new(Self {
            spec,
            typ,
            n,
            resident: TagValue::phantom(),
            from: None,
            slept: WakeBit::default(),
        })
    }

    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<'_, R>,
        buf: &mut &[u8],
    ) -> Result<Node<R>, PackError> {
        let spec = Arc::new(Expr::decode(buf)?);
        let typ = Type::decode(buf)?;
        let n = decode_nodes(ctx, buf)?.into_boxed_slice();
        Ok(Self::with(spec, typ, n))
    }

    pub(crate) fn compile(
        ctx: &mut CompileCtx<R>,
        flags: BitFlags<CFlag>,
        spec: Arc<Expr>,
        scope: &Scope,
        top_id: ExprId,
        args: &[Expr],
    ) -> Result<Node<R>> {
        let n = args
            .iter()
            .map(|e| compile(ctx, flags, e.clone(), scope, top_id))
            .collect::<Result<Box<[_]>>>()?;
        Ok(Self::with(spec, Type::empty_tvar(), n))
    }
}

impl<R: Rt> Update<R> for Any<R> {
    fn for_each_child<'a>(&'a self, f: &mut dyn FnMut(&'a Node<R>)) {
        self.n.iter().for_each(f)
    }

    fn for_each_child_mut(&mut self, f: &mut dyn FnMut(&mut Node<R>)) {
        self.n.iter_mut().for_each(f)
    }

    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::Any, buf);
        self.spec.encode(buf)?;
        self.typ.encode(buf)?;
        encode_nodes(&self.n, buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R>) -> &TagValue {
        // the first triggering value-bearing production wins; a
        // triggering bottom never beats one (`any(risky?, default)`); at a
        // wake, the child the resident came from standing bottom now
        // stands the resident bottom, moved
        // CR claude for claude: [bug/low] The rule that an `any` stands bottom when the
        // child its value came from is bottom at a wake is applied only where the `any`
        // itself slept. An `any` outside the arm, reading the same value through a
        // reference that left it, sees the child as a moved bottom and keeps the old
        // value, while an operator in its place and an `any` inside the arm are bottom.
        // Take the child's `tag.moved()` beside `woke` in the `lost` test. probe:
        // design/review-2026-11-16/repro/x-sleep-wake-any-outside-keeps-bottomed-child.gx
        // (x-sleep-wake-06)
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
            None if lost => self.resident.set_bottom_as(Tag::MOVED),
            None => self.resident.ride(),
        }
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn typ(&self) -> &Type {
        &self.typ
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R>) {
        self.slept.set();
        self.n.iter_mut().for_each(|n| n.sleep(ctx))
    }

    fn fuse(&mut self, ctx: &mut CompileCtx<R>) -> Result<Option<Node<R>>> {
        fusion::fuse_parts(self.n.iter_mut(), ctx)
    }

    typed_by_row!();

    fn view(&self) -> NodeView<'_, R> {
        NodeView::Any(self)
    }
}

/// How `~` answers a trigger that finds its RHS absent.
#[derive(Debug)]
enum Banking {
    /// `~!`: the trigger produces bottom and banks nothing.
    Strict,
    /// `~`: one answer is owed however many triggers found the RHS
    /// absent, given from the held RHS in the cycle it arrives.
    One { owed: bool },
    /// `BinOp::BankedSample` (the `seqq` request queue): an answer per
    /// trigger, paired with its captured RHS.
    Each(Payments),
}

/// The private variable a [`Banking::Each`] sample pays through.
#[derive(Debug)]
struct Payments {
    id: BindId,
    /// Paid through `id` and not yet received.
    sent: usize,
    /// The first `sent` entries have been mailed but not received.
    pending: LPooled<VecDeque<Payment>>,
}

#[derive(Debug)]
enum Payment {
    Waiting(Value),
    Ready(Value),
}

impl Payments {
    fn new(id: BindId) -> Self {
        Self { id, sent: 0, pending: LPooled::take() }
    }

    fn update<R: Rt>(
        &mut self,
        ctx: &mut ExecCtx<'_, R>,
        trigger: &TagValue,
        arg: Option<&Value>,
    ) -> Option<TagValue> {
        if trigger.is_fired() {
            self.pending.push_back(Payment::Waiting(trigger.value_cloned()));
        }
        let mut answer = ctx.event.variables.get(&self.id).cloned();
        if answer.is_some() {
            self.sent -= 1;
            self.pending.pop_front().expect("a sent payment is pending");
        }
        for payment in self.pending.iter_mut().skip(self.sent) {
            if let Payment::Waiting(request) = payment {
                let Some(arg) = arg else { break };
                *payment =
                    Payment::Ready(crate::rep::tuple([request.clone(), arg.clone()]));
            }
        }
        if answer.is_none()
            && self.sent == 0
            && matches!(self.pending.front(), Some(Payment::Ready(_)))
            && let Some(Payment::Ready(value)) = self.pending.pop_front()
        {
            answer = Some(TagValue::fired(value));
        }
        for payment in self.pending.iter().skip(self.sent) {
            let Payment::Ready(value) = payment else { break };
            ctx.rt.set_var(self.id, value.clone());
            self.sent += 1;
        }
        answer
    }
}

impl Banking {
    /// A sample's banking: strict (`None`), one answer owed, or each paid
    /// through the variable `pay`, which the sample at `top_id` reads.
    fn new<R: Rt>(
        ctx: &mut CompileCtx<R>,
        owed: Option<Option<BindId>>,
        top_id: ExprId,
    ) -> Self {
        match owed {
            None => Banking::Strict,
            Some(None) => Banking::One { owed: false },
            Some(Some(pay)) => {
                ctx.record_ref(pay, top_id);
                Banking::Each(Payments::new(pay))
            }
        }
    }
    fn result_type(&self, trigger: &Type, arg: &Type) -> Type {
        match self {
            Self::Each(_) => Type::Tuple(Arc::from_iter([trigger.clone(), arg.clone()])),
            Self::Strict | Self::One { .. } => arg.clone(),
        }
    }
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
pub struct Sample<R: Rt> {
    pub(crate) spec: Arc<Expr>,
    banking: Banking,
    pub typ: Type,
    top_id: ExprId,
    pub trigger: Node<R>,
    pub arg: Held<R>,
    resident: TagValue,
    slept: WakeBit,
}

impl<R: Rt> Sample<R> {
    fn with(
        spec: Arc<Expr>,
        banking: Banking,
        typ: Type,
        top_id: ExprId,
        trigger: Node<R>,
        arg: Held<R>,
    ) -> Node<R> {
        Node::new(Self {
            spec,
            banking,
            typ,
            top_id,
            trigger,
            arg,
            resident: TagValue::phantom(),
            slept: WakeBit::default(),
        })
    }

    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<'_, R>,
        buf: &mut &[u8],
    ) -> Result<Node<R>, PackError> {
        let spec = Arc::new(Expr::decode(buf)?);
        let typ = Type::decode(buf)?;
        let top_id = ExprId::decode(buf)?;
        let banking = Banking::new(ctx, Option::<Option<BindId>>::decode(buf)?, top_id);
        let trigger = decode_node(ctx, buf)?;
        let arg = Held::image_decode(ctx, buf)?;
        Ok(Self::with(spec, banking, typ, top_id, trigger, arg))
    }

    pub(crate) fn compile(
        ctx: &mut CompileCtx<R>,
        flags: BitFlags<CFlag>,
        spec: Arc<Expr>,
        scope: &Scope,
        top_id: ExprId,
        lhs: &Arc<Expr>,
        rhs: &Arc<Expr>,
        owed: Option<Owed>,
    ) -> Result<Node<R>> {
        let pay = owed.map(|owed| match owed {
            Owed::One => None,
            Owed::Each => Some(BindId::new()),
        });
        let banking = Banking::new(ctx, pay, top_id);
        let trigger = compile(ctx, flags, (**lhs).clone(), scope, top_id)?;
        let arg = Held::new(compile(ctx, flags, (**rhs).clone(), scope, top_id)?);
        let typ = banking.result_type(trigger.typ(), arg.node.typ());
        Ok(Self::with(spec, banking, typ, top_id, trigger, arg))
    }

    /// The payment variable of a [`Banking::Each`] sample.
    fn pay_id(&self) -> Option<BindId> {
        match &self.banking {
            Banking::Strict | Banking::One { .. } => None,
            Banking::Each(pay) => Some(pay.id),
        }
    }
}

impl<R: Rt> Update<R> for Sample<R> {
    fn for_each_child<'a>(&'a self, f: &mut dyn FnMut(&'a Node<R>)) {
        f(&self.trigger);
        f(&self.arg.node)
    }

    fn for_each_child_mut(&mut self, f: &mut dyn FnMut(&mut Node<R>)) {
        f(&mut self.trigger);
        f(&mut self.arg.node)
    }

    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::Sample, buf);
        self.spec.encode(buf)?;
        self.typ.encode(buf)?;
        self.top_id.encode(buf)?;
        let banking = match &self.banking {
            Banking::Strict => None,
            Banking::One { owed: false } => Some(None),
            Banking::Each(pay) if pay.pending.is_empty() && pay.sent == 0 => {
                Some(Some(pay.id))
            }
            Banking::One { .. } | Banking::Each(_) => {
                return Err(PackError::Application(image::NOT_QUIESCENT));
            }
        };
        banking.encode(buf)?;
        self.trigger.image_encode(buf)?;
        self.arg.image_encode(buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R>) -> &TagValue {
        // CR claude for claude: [bug/high] A sample's rebirth clears its bank and keeps
        // its last output: `resident` rides into the step's next run, so a step, or a
        // callee a step calls, whose `~` is not triggered in that run completes on the
        // previous run's sample. `seq go { let s = live(ev) ~ 7; s }` waits for `ev` in
        // its first run and completes at once in every later one, where
        // `once(live(ev))` waits in each (`Apply::reborn`). organic_firing.md rule 1
        // starts a step's run with every value bottom, and the skill's seq.md says a
        // step never completes on a value standing from an earlier run. Set `resident`
        // to the phantom in this branch, for `~`, `~!` and the banked sample alike, and
        // pin it beside `lang::seq::sample_bank_cleared_at_seq_rebirth` (that fixture
        // with `v` present gives 2, not 1). probe:
        // design/review-2026-11-16/repro/c-node-mod-sample-output-survives-step-rebirth.gx
        // Also reported as (x-rerun-6-01), (x-engine-seq-errors-03). (c-node-mod-01)
        if self.slept.take() && ctx.event.born() {
            match &mut self.banking {
                Banking::Strict => (),
                Banking::One { owed } => *owed = false,
                Banking::Each(pay) => pay.pending.clear(),
            }
        }
        let trigger = self.trigger.update(ctx);
        let fired = trigger.is_fired();
        self.arg.update(ctx);
        let owed = match &mut self.banking {
            Banking::Strict => {
                return match (fired, self.arg.value.as_ref(), self.arg.tag.is_bottom()) {
                    (true, Some(v), false) => {
                        self.resident.set(TagValue::fired(v.clone()))
                    }
                    (true, _, _) => self.resident.set_bottom(true),
                    (false, _, _) => self.resident.ride(),
                };
            }
            Banking::One { owed } => {
                *owed |= fired;
                owed
            }
            Banking::Each(pay) => {
                let arg = self.arg.value.as_ref().filter(|_| !self.arg.tag.is_bottom());
                return match pay.update(ctx, trigger, arg) {
                    Some(tv) => self.resident.set(tv),
                    None => self.resident.ride(),
                };
            }
        };

        // CR claude for eric: [bug/medium] A banked payment is made whenever the
        // argument is present, fired or not. A sample that banked a trigger and then
        // slept pays at its arm's wake if the argument arrived meanwhile, and a sample
        // outside pays when its argument leaves bottom by a wake's quiet recompute: a
        // connect writes in a cycle in which nothing it reads fired. `hold` and `queue`
        // with a banked clock, and 12 other consumers tried on the same input, do
        // nothing there. Either pay only in a cycle the argument fires and drop a debt
        // whose arrival was missed, or name this as the exception to "a wake fires
        // nothing" in the docs and pin it. probe:
        // design/review-2026-11-16/repro/x-sleep-wake-banked-sample-pays-at-wake.gx and
        // x-sleep-wake-banked-sample-pays-on-moved.gx (x-sleep-wake-04)
        // CR claude for eric: [bug/medium] A debt is paid whenever the held argument is
        // not bottom, so an argument that a wake recomputed quietly (moved, no fire)
        // pays it and the sample fires: a wake fires an event. A banked `e ~ h(1)`
        // outside a woken arm writes through `<-` at the wake, where a plain `h(1)`
        // reader does not, a call over it does not fire and a seq step waiting for it
        // stalls; the same sample inside the arm fires at its own wake. That
        // contradicts "a wake fires nothing for anyone" and "a sample whose trigger did
        // not fire rides" (bind_driven_bodies.md). Either pay only on a triggering
        // production of the argument (or with a trigger fire) and keep the debt across
        // a moved arrival, or write the exception into the rule; no pin covers a wake
        // that finds the banked argument standing. probe:
        // design/review-2026-11-16/repro/x-seq-sampling-bank-paid-by-wake.gx
        // (x-seq-sampling-03)
        let live = self.arg.value.as_ref().filter(|_| !self.arg.tag.is_bottom());
        match live {
            // CR claude for eric: [bug/medium] A banked trigger is paid whenever the
            // argument is present, fired or not, and the bank survives an arm's sleep.
            // A sample in an arm whose argument arrived while the arm slept therefore
            // fires at the wake: `let t = go ~ v; w <- t` writes at a cycle in which
            // neither `go` nor `v` fires. CLAUDE.md says a wake fires nothing for
            // anyone and nothing is kept for a body that does not run;
            // organic_firing.md says only that the bank is kept. Which holds: the
            // arrival the arm slept through is missed and the bank waits for the
            // argument's next fire, or the wake pays and the docs name it as the
            // exception? Pin the ruling beside
            // banked_sample_payment_asleep_is_owed_nothing. probe:
            // design/review-2026-11-16/repro/x-engine-firing-bank-pays-at-wake.gx
            // (x-engine-firing-05)
            // CR claude for eric: [bug/medium] A bank is paid in the cycle its arm
            // wakes when the argument arrived during the sleep: the held argument reads
            // present, quietly, and `owed` fires the sample though nothing the arm
            // reads fired. In the probe `hits <- (trig ~ v) ~ hits + 1` writes in a
            // wake cycle in which only the select's scrutinee fires. CLAUDE.md has a
            // wake fire nothing for anyone, organic_firing.md rule 3 has a sleeping
            // body miss a value leaving bottom, and sleep_and_wake.md says `~` rides.
            // `ConnectDeref::update` lands its `pending` write at a wake the same way.
            // Either the bank is state whose payment is the one fire a wake may make
            // (say so in sleep_and_wake.md and bind_driven_bodies.md), or a wake that
            // finds the argument present with no fire drops the debt, the arrival
            // having been missed. No pin holds either today. probe:
            // design/review-2026-11-16/repro/c-node-mod-bank-paid-at-wake.gx
            // (c-node-mod-03)
            Some(value) if *owed => {
                *owed = false;
                self.resident.set(TagValue::fired(value.clone()))
            }
            _ => self.resident.ride(),
        }
    }

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R>) {
        if let Some(id) = self.pay_id() {
            ctx.release_var(id, self.top_id);
        }
        self.arg.node.delete(ctx);
        self.trigger.delete(ctx);
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R>) {
        self.slept.set();
        if let Banking::Each(pay) = &mut self.banking
            && pay.sent > 0
        {
            ctx.release_var(pay.id, self.top_id);
            pay.id = BindId::new();
            ctx.ref_var(pay.id, self.top_id);
            pay.sent = 0;
        }
        self.arg.sleep(ctx);
        self.trigger.sleep(ctx);
    }

    fn fuse(&mut self, ctx: &mut CompileCtx<R>) -> Result<Option<Node<R>>> {
        fusion::fuse_parts([&mut self.trigger, &mut self.arg.node], ctx)
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn typ(&self) -> &Type {
        &self.typ
    }

    fn refs(&self, refs: &mut Refs) {
        if let Some(id) = self.pay_id() {
            refs.read(id);
        }
        refs.banked += 1;
        self.arg.node.refs(refs);
        refs.banked -= 1;
        self.trigger.refs(refs);
    }

    typed_by_row!(children);

    fn view(&self) -> NodeView<'_, R> {
        NodeView::Sample(self)
    }
}

impl<R: Rt> ExplicitParens<R> {
    fn typecheck0_with(
        &mut self,
        ctx: &mut CompileCtx<R>,
        child: &mut Child<'_, R>,
    ) -> Result<()> {
        wrap!(self.n, child(&mut self.n, ctx))
    }
}

impl<R: Rt> StringInterpolate<R> {
    fn typecheck0_with(
        &mut self,
        ctx: &mut CompileCtx<R>,
        child: &mut Child<'_, R>,
    ) -> Result<()> {
        for (i, a) in self.args.iter_mut().enumerate() {
            wrap!(a, child(a, ctx))?;
            self.typs[i] = part_type(a.typ());
        }
        Ok(())
    }
}

impl<R: Rt> Connect<R> {
    fn typecheck0_with(
        &mut self,
        ctx: &mut CompileCtx<R>,
        child: &mut Child<'_, R>,
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
        // a binding generalized before the write holds what is written
        // as its every instance: a copy of its type, the variables rigid
        let generic = ctx.env.poly_binds.contains(&self.id);
        let held = match generic {
            true => ctx.monomorphic(&bind.typ),
            false => bind.typ.clone(),
        };
        let mut rigid: LPooled<AHashMap<usize, TVar>> = LPooled::take();
        if generic {
            held_cells(&held, &mut rigid);
        }
        let _gates: LPooled<Vec<_>> = rigid.values().map(|tv| tv.open_rigid()).collect();
        let contain = |t: &Type| {
            t.check_contains(&ctx.env, written).map_err(|e| {
                match e.downcast_ref::<TypeMismatch>() {
                    Some(_) if generic => format_with_flags(
                        PrintFlag::DerefTVars | PrintFlag::ReplacePrims,
                        || {
                            let (name, hint) = generic_target(&ctx.env, bind);
                            let scheme = bind.typ.scheme_read(&ctx.rec_defs, &|_| false);
                            anyhow!(
                                "{name} is {}, a generic binding, so what is written to \
                                 it must be as general, and {written} is not{hint}",
                                scheme.as_ref().unwrap_or(&bind.typ)
                            )
                        },
                    ),
                    None => match e.downcast_ref::<AmbiguousMember>() {
                        Some(a) if a.in_holder() && !crate::is_lowered(&bind.name) => {
                            format_with_flags(
                                PrintFlag::DerefTVars | PrintFlag::ReplacePrims,
                                || {
                                    anyhow!(
                                        "{} is {held}: {}; declare {}'s type where it \
                                         is bound ({})",
                                        bind.name,
                                        a.fact(),
                                        bind.name,
                                        BoundAt(bind, Some(&self.spec.ori))
                                    )
                                },
                            )
                        }
                        _ => e,
                    },
                    // a write a lowering built has no declaration to point at
                    Some(_) if !crate::is_lowered(&bind.name) => {
                        let mut refs = Refs::without_callees();
                        self.node.refs(&mut refs);
                        let mut reads = false;
                        refs.with_refs(|id| reads |= id == self.id);
                        write_mismatch(&ctx.env, bind, written, reads, &self.spec.ori)
                    }
                    _ => e,
                }
            })
        };
        wrap!(self, contain(&held))?;
        // as general as the binding: the written value may not carry one of
        // its variables into the environment, as a function's parameter would
        let quantified: LPooled<Vec<(ArcStr, usize)>> =
            rigid.iter().map(|(addr, tv)| (tv.name.clone(), *addr)).collect();
        if generic && callsite::carried_out(&quantified, self.node.typ()).is_some() {
            return format_with_flags(PrintFlag::DerefTVars | PrintFlag::ReplacePrims, || {
                let (name, hint) = generic_target(&ctx.env, bind);
                Err(anyhow!(
                    "{name} is {}, a generic binding, so what is written to it must be as \
                     general, and {written} is not: it holds a type variable of the code \
                     around the write{hint}",
                    bind.typ,
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
                match generic {
                    true => callsite::Narrowing::Copy,
                    false => callsite::Narrowing::Written,
                }
            )
        )
    }
}

/// The open cells `held`, a generic binding's copy, reaches, a bound's and
/// a function type's own quantifiers included, by address: two of one name
/// are two cells where the copy took each function type apart.
fn held_cells(held: &Type, out: &mut AHashMap<usize, TVar>) {
    held.reached_cells(out);
    out.retain(|_, tv| !tv.is_bound());
}

fn reference_targets(
    env: &env::Env,
    typ: &Type,
) -> Result<Option<LPooled<Vec<(Mutability, Type)>>>> {
    fn collect(
        env: &env::Env,
        typ: &Type,
        out: &mut Vec<(Mutability, Type)>,
    ) -> Result<bool> {
        crate::stack::ensure_sufficient(|| match typ.deref_cloned().as_ref() {
            None => Ok(false),
            Some(t @ Type::Ref(_)) => collect(env, &t.lookup_ref(env)?, out),
            Some(Type::Set(ts)) => {
                for t in ts.iter() {
                    if !collect(env, t, out)? {
                        bail!("expected a reference not {t}");
                    }
                }
                Ok(true)
            }
            Some(Type::ByRef(m, t)) => {
                out.push((*m, (**t).clone()));
                Ok(true)
            }
            Some(t) => format_with_flags(PrintFlag::DerefTVars, || {
                bail!("expected a reference not {t}")
            }),
        })
    }
    let mut targets = LPooled::take();
    Ok(collect(env, typ, &mut targets)?.then_some(targets))
}

impl<R: Rt> ConnectDeref<R> {
    fn typecheck0_with(
        &mut self,
        ctx: &mut CompileCtx<R>,
        child: &mut Child<'_, R>,
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
        let targets = reference_targets(&ctx.env, &btyp);
        match wrap!(self, targets)? {
            // an unbound reference is inferred writable at the written type
            None => {
                let typ = Type::ByRef(Mutability::Mut, Arc::new(rhs.clone()));
                wrap!(self, btyp.check_contains(&ctx.env, &typ))
            }
            // a write must fit every reference the binding may hold
            Some(mut targets) => {
                for (m, t) in targets.drain(..) {
                    if m == Mutability::Shared {
                        bail!(
                            "can't write through a read-only reference ({btyp}): take it with `&mut`"
                        )
                    }
                    let contain = |t: &Type| t.check_contains(&ctx.env, rhs);
                    wrap!(self, contain(&t))?;
                    wrap!(
                        self,
                        callsite::check_hidden_defaults(
                            ctx,
                            BitFlags::empty(),
                            &Scope::root(),
                            self.spec.id,
                            &t,
                            rhs,
                            callsite::Narrowing::Written
                        )
                    )?;
                }
                Ok(())
            }
        }
    }
}

impl<R: Rt> TypeCast<R> {
    fn typecheck0_with(
        &mut self,
        ctx: &mut CompileCtx<R>,
        child: &mut Child<'_, R>,
    ) -> Result<()> {
        Ok(wrap!(self.n, child(&mut self.n, ctx))?)
    }
}

impl<R: Rt> Never<R> {
    fn typecheck0_with(
        &mut self,
        ctx: &mut CompileCtx<R>,
        child: &mut Child<'_, R>,
    ) -> Result<()> {
        for n in self.n.iter_mut() {
            wrap!(n, child(n, ctx))?
        }
        Ok(())
    }
}

impl<R: Rt> Any<R> {
    /// Whether this is a seq lowering's `any(#seqpc ~! v, v)`: `v` fired at
    /// its step's entry, then tracked, both alternatives one read's value.
    pub(crate) fn fired_at_entry(&self, env: &env::Env) -> bool {
        let [at_entry, v] = &self.n[..] else { return false };
        let NodeView::Sample(s) = at_entry.view() else { return false };
        let read = |n: &Node<R>| match n.view() {
            NodeView::Ref(r) => Some(r.id),
            _ => None,
        };
        s.is_strict()
            && bind::reads_lowered(env, &s.trigger, crate::expr::seq::is_pc)
            && read(&s.arg.node).is_some_and(|id| read(v) == Some(id))
    }

    fn typecheck0_with(
        &mut self,
        ctx: &mut CompileCtx<R>,
        child: &mut Child<'_, R>,
        check: bool,
    ) -> Result<()> {
        for n in self.n.iter_mut() {
            wrap!(n, child(n, ctx))?
        }
        if !check {
            return Ok(());
        }
        if bounds::active() {
            // whichever fires is the value: each flows into a cell that
            // may join them
            joins(&self.typ);
            for n in self.n.iter() {
                wrap!(n, self.typ.check_contains(&ctx.env, n.typ()))?
            }
            return Ok(());
        }
        let fn_of = |t: &Type| {
            t.with_deref(|t| match t {
                Some(Type::Fn(f)) => Some(f.clone()),
                _ => None,
            })
        };
        // a passed read's two reads share one copy of a generic function,
        // which meeting would pin to one instance
        let one_copy = self.fired_at_entry(&ctx.env)
            && fn_of(self.n[0].typ())
                .zip(fn_of(self.n[1].typ()))
                .is_some_and(|(f, g)| Arc::ptr_eq(&f, &g));
        let bottom = Type::Bottom;
        let mut ts: LPooled<Vec<&Type>> = LPooled::take();
        ts.push(&bottom);
        ts.extend(self.n.iter().skip(one_copy as usize).map(|n| n.typ()));
        let rtyp = wrap!(self, Type::union_of_alternatives(&ctx.env, &ts))?;
        let rtyp = if rtyp == Type::Bottom { Type::empty_tvar() } else { rtyp };
        self.typ.check_contains(&ctx.env, &rtyp)?;
        Ok(())
    }
}

impl<R: Rt> Sample<R> {
    /// `~!`: a trigger finding the argument absent produces bottom.
    pub(crate) fn is_strict(&self) -> bool {
        matches!(self.banking, Banking::Strict)
    }

    fn typecheck0_with(
        &mut self,
        ctx: &mut CompileCtx<R>,
        child: &mut Child<'_, R>,
    ) -> Result<()> {
        wrap!(self.trigger, child(&mut self.trigger, ctx))?;
        wrap!(self.arg.node, child(&mut self.arg.node, ctx))?;
        self.typ = self.banking.result_type(self.trigger.typ(), self.arg.node.typ());
        Ok(())
    }
}

#[cfg(test)]
mod test {
    use super::{BlockWrites, WRITES_STEPS};
    use crate::expr::{Expr, ExprKind, ModuleKind, Origin, Source, parser};
    use arcstr::ArcStr;
    use triomphe::Arc;

    fn parse(text: &str) -> Arc<[Expr]> {
        let ori = Origin::new(None, Source::Unspecified, ArcStr::from(text));
        parser::parse(ori).expect("parses")
    }

    /// A chain of `d` modules, each holding a let, the next module and a
    /// let reading it, as files would: the outermost block's statements.
    fn nested(d: usize) -> Arc<[Expr]> {
        let mut body = parse(&format!("let x = {d};\nlet y = x"));
        for k in (0..d).rev() {
            let stmts = parse(&format!(
                "let x = {k};\nmod m{};\nlet y = m{}::y + x",
                k + 1,
                k + 1
            ));
            body = Arc::from_iter(stmts.iter().map(|e| match &e.kind {
                ExprKind::Module { name, .. } => {
                    let value = ModuleKind::Resolved {
                        exprs: body.clone(),
                        sig: None,
                        from_interface: false,
                    };
                    let mut m =
                        ExprKind::Module { name: name.clone(), value: Arc::new(value) }
                            .to_expr(e.pos);
                    m.id = e.id;
                    m
                }
                _ => e.clone(),
            }));
        }
        body
    }

    /// The statements the writes walks visit compiling `top` and each
    /// module in it, block inside block, as `compile_block_children`
    /// enters and exits them (`nested`), or each block on its own.
    fn compile_steps(top: &Arc<[Expr]>, nested: bool) -> usize {
        let mut written = BlockWrites::default();
        WRITES_STEPS.with(|n| n.set(0));
        let mut block = top.clone();
        let mut depth = 0;
        loop {
            let exprs: Vec<&Expr> = block.iter().collect();
            written.enter(&exprs, None);
            if nested {
                depth += 1
            } else {
                written.exit()
            }
            let inner = block.iter().find_map(|e| match &e.kind {
                ExprKind::Module { value, .. } => match &**value {
                    ModuleKind::Resolved { exprs, .. } => Some(exprs.clone()),
                    _ => None,
                },
                _ => None,
            });
            match inner {
                Some(exprs) => block = exprs,
                None => break,
            }
        }
        for _ in 0..depth {
            written.exit()
        }
        assert!(written.is_empty());
        WRITES_STEPS.with(|n| n.get())
    }

    // A chain of d modules, each with a let before its `mod`, is walked
    // once in all: each block's walk takes what a module in it writes
    // outside it from the walk of a block around it, never walking the
    // module again (d^2 / 2 statement visits across the d blocks).
    #[test]
    fn nested_modules_walk_linear() {
        for d in [100, 400] {
            let top = nested(d);
            let steps = compile_steps(&top, true);
            assert!(steps <= 7 * (d + 1), "{d}: {steps}");
            let apart = compile_steps(&top, false);
            eprintln!("{d} modules: {steps} steps, {apart} with a walk per block");
            assert!(apart > d * d, "{d}: {apart} steps with a walk per block");
        }
    }
}
