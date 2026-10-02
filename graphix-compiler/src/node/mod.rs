use crate::{
    BindId, CAST_ERR, CAST_ERR_TAG, CFlag, CompileCtx, Event, ExecCtx, Node, NodeView,
    PrintFlag, Refs, Restore, Rt, Scope, Tag, TagValue, Update, UserEvent, env, errf,
    expr::{At, Expr, ExprId, ExprKind, ModPath, ModuleKind, Name, Origin, TypeDefBody},
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
    typ::{TVal, TVar, Type, TypeMismatch},
};
use anyhow::{Context, Result, anyhow, bail};
use arcstr::{ArcStr, literal};
use compact_str::{CompactString, format_compact};
use compiler::{compile, compile_module};
use enumflags2::BitFlags;
use netidx_core::pack::{Pack, PackError};
use netidx_value::{Typ, Value};
use poolshark::local::LPooled;
use rayon::prelude::*;
use smallvec::SmallVec;
use std::{cell::Cell, iter, mem, sync::LazyLock};
use triomphe::Arc;

pub(crate) mod array;
pub use collection::MAX_ARRAY_INIT_LEN;
pub(crate) mod bind;
pub mod callsite;
pub mod collection;
pub(crate) mod compiler;
pub mod coretraits;
pub(crate) mod data;
pub(crate) mod error;
pub mod genn;
pub mod lambda;
pub mod list;
pub(crate) mod map;
pub(crate) mod module;
pub(crate) mod op;
pub(crate) mod pattern;
pub mod place;
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
    /// channel. Readers view it Stale, or Fired under an init view.
    Standing(&'a TagValue),
}

/// Read a variable: the overlay, then the persistent store. `None`
/// means the bind has never delivered.
pub(crate) fn read_var<'a, R: Rt, E: UserEvent>(
    ctx: &'a ExecCtx<R, E>,
    event: &'a Event<E>,
    id: &BindId,
) -> Option<VarRead<'a>> {
    if let Some(tv) = event.variables.get(id) {
        return Some(VarRead::Delivered(tv));
    }
    match ctx.rt.store().get(id) {
        Some((tv, stamp)) if *stamp == ctx.rt.cycle() => Some(VarRead::Delivered(tv)),
        Some((tv, _)) => Some(VarRead::Standing(tv)),
        None => None,
    }
}

/// A standing entry as a reader sees it: fresh under a genuine init
/// view only. A wake-forced view reads it stale, its value is a past
/// event the graph already consumed.
pub(crate) fn standing_view<E: UserEvent>(event: &Event<E>, tv: &TagValue) -> TagValue {
    let tag =
        if event.init && !event.wake_init { tv.tag().fresh() } else { tv.tag().quiet() };
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

/// Type alias chains are followed this deep; a deeper chain is a cyclic
/// typedef.
pub const MAX_ALIAS_DEPTH: usize = 64;

#[macro_export]
macro_rules! deref_typ {
    ($name:literal, $ctx:expr, $typ:expr, $($pat:pat => $body:expr),+) => {
        $typ.with_deref(|typ| {
            let mut typ = typ.cloned();
            let mut depth = 0usize;
            loop {
                #[allow(unreachable_patterns)]
                match &typ {
                    $($pat => break $body),+,
                    Some(rt @ $crate::typ::Type::Ref($crate::typ::TypeRef { .. })) => {
                        depth += 1;
                        if depth > $crate::node::MAX_ALIAS_DEPTH {
                            $crate::format_with_flags($crate::PrintFlag::DerefTVars, || {
                                anyhow::bail!(
                                    "cyclic type alias while dereferencing {rt} \
                                     (expected {})",
                                    $name
                                )
                            })?
                        }
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
                    None => anyhow::bail!("type must be known, annotations needed")
                }
            }
        })
    };
}

pub(crate) static NOP: LazyLock<Arc<Expr>> = LazyLock::new(|| {
    let mut nop = Expr::default();
    nop.kind = ExprKind::Constant(Value::String(literal!("nop")));
    Arc::new(nop)
});

/// Set by a node's `sleep()`, taken by its next update: the first update
/// after a sleep recomputes from the present world.
#[derive(Debug, Default, Clone, Copy)]
pub(crate) struct WakeBit(bool);

impl WakeBit {
    pub(crate) fn set(&mut self) {
        self.0 = true
    }

    pub(crate) fn take(&mut self) -> bool {
        std::mem::take(&mut self.0)
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
        _ctx: &mut ExecCtx<R, E>,
        buf: &mut &[u8],
    ) -> Result<Node<R, E>, PackError> {
        Ok(Self::new(Type::decode(buf)?))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Nop {
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::Nop, buf);
        self.typ.encode(buf)
    }

    fn update(&mut self, _ctx: &mut ExecCtx<R, E>, _event: &mut Event<E>) -> &TagValue {
        TagValue::phantom_ref()
    }

    fn delete(&mut self, _ctx: &mut ExecCtx<R, E>) {}

    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {}

    fn typecheck0(&mut self, _ctx: &mut CompileCtx<R, E>) -> Result<()> {
        Ok(())
    }

    fn typecheck1(&mut self, _ctx: &mut CompileCtx<R, E>) -> Result<()> {
        Ok(())
    }

    fn spec(&self) -> &Expr {
        &NOP
    }

    fn typ(&self) -> &Type {
        &self.typ
    }

    fn refs(&self, _refs: &mut Refs) {}

    fn view(&self) -> NodeView<'_, R, E> {
        NodeView::Nop(self)
    }
}

#[derive(Debug)]
pub struct ExplicitParens<R: Rt, E: UserEvent> {
    pub(crate) spec: Expr,
    pub n: Node<R, E>,
}

impl<R: Rt, E: UserEvent> ExplicitParens<R, E> {
    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<R, E>,
        buf: &mut &[u8],
    ) -> Result<Node<R, E>, PackError> {
        let spec = Expr::decode(buf)?;
        let n = decode_node(ctx, buf)?;
        Ok(Node::new(Self { spec, n }))
    }

    pub(crate) fn compile(
        ctx: &mut CompileCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: Expr,
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
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::ExplicitParens, buf);
        self.spec.encode(buf)?;
        self.n.image_encode(buf)
    }

    fn fuse(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<Option<Node<R, E>>> {
        // parens are a fusion boundary: the interior gets its own region pass
        fuse(&mut self.n, ctx)?;
        Ok(None)
    }

    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> &TagValue {
        self.n.update(ctx, event)
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.n.delete(ctx);
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.n.sleep(ctx);
    }

    fn typecheck0(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
        self.typecheck0_with(ctx, &mut |n, ctx| n.typecheck0(ctx))
    }

    fn typecheck0_instance(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        types: &mut lambda::InstanceTypes,
    ) -> Result<()> {
        self.typecheck0_with(ctx, &mut |n, ctx| n.typecheck0_instance(ctx, types))
    }

    fn typecheck1(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
        wrap!(self.n, self.n.typecheck1(ctx))
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn typ(&self) -> &Type {
        &self.n.typ()
    }

    fn refs(&self, refs: &mut Refs) {
        self.n.refs(refs);
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
        ctx: &mut ExecCtx<R, E>,
        buf: &mut &[u8],
    ) -> Result<Self, PackError> {
        Ok(Self::new(decode_node(ctx, buf)?))
    }

    pub fn new(node: Node<R, E>) -> Self {
        Self { value: None, tag: Tag::FIRED, node }
    }

    /// Update the node, returning the production's tag. A bottom
    /// production poisons the tag but never overwrites the value.
    pub fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> Tag {
        let tv = self.node.update(ctx, event);
        let tag = tv.tag();
        if !tag.is_bottom() {
            self.value = Some(tv.value_cloned());
        }
        self.tag = tag;
        tag
    }

    /// Sleep is pause, not reset: the held value and its at-rest taint
    /// survive.
    pub fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.node.sleep(ctx)
    }
}

/// Update every child of a composite and join their tags
/// ([`Tag::join`]): the join triggers when a child did and is bottom
/// when a child is; past the caller's [`dense_gate!`] it is the
/// composite's own tag. The productions stay borrowed, so a value is
/// cloned only once the gate let it through.
pub(crate) fn gather<'a, R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<R, E>,
    event: &mut Event<E>,
    nodes: &'a mut [Node<R, E>],
) -> (Tag, SmallVec<[&'a TagValue; 8]>) {
    let mut tag = Tag::STALE;
    let prods = nodes
        .iter_mut()
        .map(|c| {
            let tv = c.update(ctx, event);
            tag = tag.join(tv.tag());
            tv
        })
        .collect();
    (tag, prods)
}

/// A strict computation propagates consumed bottom before considering
/// its cached result. Quiet recomputation cannot manufacture an event.
macro_rules! dense_gate {
    ($self:ident, $trig:expr, $bottom:expr) => {{
        let woke = $self.slept.take();
        $crate::node::dense_gate!($self.resident, $trig, $bottom, woke);
    }};
    ($resident:expr, $trig:expr, $bottom:expr, $woke:expr) => {{
        if $bottom {
            return $resident.set_bottom($trig);
        }
        if !($trig || $resident.tag().is_bottom() || $woke) {
            return $resident.ride();
        }
    }};
}
pub(crate) use dense_gate;

#[derive(Debug)]
pub struct TypeDef {
    spec: Expr,
    scope: ModPath,
    name: ArcStr,
}

impl TypeDef {
    pub(crate) fn compile<R: Rt, E: UserEvent>(
        ctx: &mut CompileCtx<R, E>,
        spec: Expr,
        scope: &Scope,
        name: &Name,
        params: &Arc<[(TVar, Option<Type>)]>,
        body: &TypeDefBody,
    ) -> Result<Node<R, E>> {
        ctx.env
            .deftype(
                &scope.lexical,
                name,
                params.clone(),
                body,
                false,
                None,
                name.pos_or(spec.pos),
                spec.ori.clone(),
            )
            .at(&spec)?;
        let name = name.name.clone();
        Ok(Node::new(Self { spec, scope: scope.lexical.clone(), name }))
    }
}

impl TypeDef {
    pub(crate) fn image_decode<R: Rt, E: UserEvent>(
        _ctx: &mut ExecCtx<R, E>,
        buf: &mut &[u8],
    ) -> Result<Node<R, E>, PackError> {
        let spec = Expr::decode(buf)?;
        let scope = ModPath::decode(buf)?;
        let name = ArcStr::decode(buf)?;
        Ok(Node::new(Self { spec, scope, name }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for TypeDef {
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::TypeDef, buf);
        self.spec.encode(buf)?;
        self.scope.encode(buf)?;
        self.name.encode(buf)
    }

    fn update(&mut self, _ctx: &mut ExecCtx<R, E>, _event: &mut Event<E>) -> &TagValue {
        TagValue::phantom_ref()
    }

    fn typecheck0(&mut self, _ctx: &mut CompileCtx<R, E>) -> Result<()> {
        Ok(())
    }

    fn typecheck1(&mut self, _ctx: &mut CompileCtx<R, E>) -> Result<()> {
        Ok(())
    }

    fn refs(&self, _refs: &mut Refs) {}

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        ctx.env.undeftype(&self.scope, &self.name)
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {}

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
    pub fn new<R: Rt, E: UserEvent>(value: Value, typ: Type, spec: Expr) -> Node<R, E> {
        // the resident starts Stale so an instance bound without an init
        // view still computes; firing stays init-gated
        let resident = TagValue::stale(value.clone());
        Node::new(Self { spec: Arc::new(spec), value, typ, resident })
    }
}

impl Constant {
    pub(crate) fn image_decode<R: Rt, E: UserEvent>(
        _ctx: &mut ExecCtx<R, E>,
        buf: &mut &[u8],
    ) -> Result<Node<R, E>, PackError> {
        let spec = Expr::decode(buf)?;
        let value = Value::decode(buf)?;
        let typ = Type::decode(buf)?;
        Ok(Self::new(value, typ, spec))
    }
}

/// A constant's production: fired at an init view, standing otherwise.
/// Every argument-less literal is a constant.
pub(crate) fn produce_constant<'a, E: UserEvent>(
    event: &Event<E>,
    resident: &'a mut TagValue,
    value: impl FnOnce() -> Value,
) -> &'a TagValue {
    if event.init { resident.set(TagValue::fired(value())) } else { resident.ride() }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Constant {
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::Constant, buf);
        self.spec.encode(buf)?;
        self.value.encode(buf)?;
        self.typ.encode(buf)
    }

    fn update(&mut self, _ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> &TagValue {
        produce_constant(event, &mut self.resident, || self.value.clone())
    }

    fn delete(&mut self, _ctx: &mut ExecCtx<R, E>) {}

    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {}

    fn refs(&self, _refs: &mut Refs) {}

    fn typ(&self) -> &Type {
        &self.typ
    }

    fn typecheck0(&mut self, _ctx: &mut CompileCtx<R, E>) -> Result<()> {
        Ok(())
    }

    fn typecheck1(&mut self, _ctx: &mut CompileCtx<R, E>) -> Result<()> {
        Ok(())
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
    pub(crate) spec: Expr,
    pub(crate) children: Box<[Node<R, E>]>,
    /// Indices of `catch(e) expr` children, in syntactic order.
    /// update/typecheck run the covered children first, then catches in
    /// reverse syntactic order: an inner handler's rethrow delivers to
    /// its predecessor, which only sees it if it updates after.
    pub(crate) catches: Box<[usize]>,
    /// Production slot for the catch-bearing path: the last covered
    /// child's borrow can't be held across the catches pass.
    resident: TagValue,
}

impl<R: Rt, E: UserEvent> Block<R, E> {
    /// Build a `Block` from compiled children. A module produces no
    /// value; a `do` block's value is its last child's.
    pub fn new(module: bool, children: Box<[Node<R, E>]>, spec: Expr) -> Node<R, E> {
        Node::new(Self {
            module,
            spec,
            children,
            catches: Box::default(),
            resident: TagValue::phantom(),
        })
    }

    pub(crate) fn compile(
        ctx: &mut CompileCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: Expr,
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
            children,
            catches,
            resident: TagValue::phantom(),
        }))
    }
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
    let exprs: smallvec::SmallVec<[&'a Expr; 32]> = exprs.collect();
    // pre-register the block's `mod` names so resolution is independent
    // of declaration order; the `Module` arm is told, so its own
    // duplicate guard does not trip
    for e in exprs.iter() {
        if let ExprKind::Module { name, .. } = &e.kind {
            let p = ModPath(scope.lexical.append(name));
            if ctx.env.modules.contains(&p) {
                return Err(anyhow::anyhow!("duplicate module definition {p}").at(&(*e)));
            }
            ctx.env.modules.insert_cow(p);
        }
    }
    let mut scope = scope.clone();
    let mut children: LPooled<Vec<Node<R, E>>> = LPooled::take();
    let mut catches: LPooled<Vec<usize>> = LPooled::take();
    let n = exprs.len();
    for (i, e) in exprs.iter().copied().enumerate() {
        if matches!(e.kind, ExprKind::Catch(_)) {
            catches.push(i);
        }
        let at = StmtAt::Block { value: !module && i + 1 == n };
        let (node, next) = compile_statement(ctx, flags, e, &scope, top_id, at)?;
        scope = next;
        children.push(node);
    }
    for n in children.iter() {
        defer_typedef_names(ctx, n);
    }
    Ok((Box::from_iter(children.drain(..)), Box::from_iter(catches.drain(..))))
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
        crate::defer_unresolved_names(ctx, t, &td.spec);
    }
}

/// Where a statement stands: a top-level statement, or one of a block's,
/// `value` when it is the block's value (not a module block's).
#[derive(Clone, Copy)]
pub(crate) enum StmtAt {
    TopLevel,
    Block { value: bool },
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
    let node = match &e.kind {
        ExprKind::Catch(c) => {
            return error::Catch::compile(ctx, flags, e.clone(), scope, top_id, c);
        }
        ExprKind::Module { name, value: value @ ModuleKind::Dynamic { .. } } => {
            compile_module(ctx, flags, e.clone(), scope, top_id, name, value, predeclared)
        }
        ExprKind::Bind(_) => {
            compiler::compile_statement_expr(ctx, flags, e.clone(), scope, top_id)
        }
        _ if matches!(at, StmtAt::Block { value: true }) => {
            compile(ctx, flags, e.clone(), scope, top_id)
        }
        ExprKind::Use { reexport, names } => {
            module::compile_use(ctx, flags, e.clone(), scope, *reexport, names)
        }
        ExprKind::Module { name, value } => {
            compile_module(ctx, flags, e.clone(), scope, top_id, name, value, predeclared)
        }
        ExprKind::TypeDef(td) => {
            TypeDef::compile(ctx, e.clone(), scope, &td.name, &td.params, &td.body)
        }
        ExprKind::Trait(t) => {
            traits::Trait::compile(ctx, flags, e.clone(), scope, t, top_id)
        }
        ExprKind::Impl(im) => {
            traits::Impl::compile(ctx, flags, e.clone(), scope, im, top_id)
        }
        _ => compile(ctx, flags, e.clone(), scope, top_id),
    }?;
    Ok((node, scope.clone()))
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
        let r = wrap!(n, pass(n, ctx));
        match module {
            true => r.with_context(|| n.spec().ori.clone())?,
            false => r?,
        }
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
    let mut at = 0;
    while at < order.len() {
        let run =
            order[at..].iter().take_while(|i| is_static_module(&slots[**i])).count();
        if run >= 2 && !RUNTIME_BIND.get() {
            let run_nodes = order[at..at + run]
                .iter()
                .map(|i| slots[*i].take().expect("an order visits each once"));
            typecheck0_modules(ctx, run_nodes, module)?;
            at += run;
        } else {
            let n = slots[order[at]].take().expect("an order visits each once");
            let r = wrap!(n, n.typecheck0(ctx));
            match module {
                true => r.with_context(|| n.spec().ori.clone())?,
                false => r?,
            }
            at += 1;
        }
    }
    Ok(())
}

/// Check each module of a run in a compile task forked before any runs.
fn typecheck0_modules<'a, R: Rt, E: UserEvent>(
    ctx: &mut CompileCtx<R, E>,
    run: impl Iterator<Item = &'a mut Node<R, E>>,
    module: bool,
) -> Result<()> {
    let mut work: LPooled<Vec<(&mut Node<R, E>, CompileCtx<R, E>)>> =
        run.map(|n| (n, ctx.fork())).collect();
    let paths: LPooled<Vec<ModPath>> = work
        .iter()
        .map(|(n, _)| match n.view() {
            NodeView::Module(m) => m.scope.lexical.clone(),
            _ => unreachable!("a run is of modules"),
        })
        .collect();
    for (i, (_, task)) in work.iter_mut().enumerate() {
        let mut hidden = (*task.env.hidden_impls).clone();
        hidden.extend(
            paths.iter().enumerate().filter(|(j, _)| *j != i).map(|(_, p)| p.clone()),
        );
        task.env.hidden_impls = Arc::new(hidden);
    }
    let level = crate::typ::tvar::current_level();
    let mut results: Vec<Result<()>> = work
        .par_iter_mut()
        .map(|(n, task)| {
            let _level = crate::typ::tvar::AtLevel::enter(level);
            let r = wrap!(n, n.typecheck0(task));
            match module {
                true => r.with_context(|| n.spec().ori.clone()),
                false => r,
            }
        })
        .collect();
    for (_, task) in work.drain(..) {
        ctx.join(task);
    }
    results.drain(..).find(|r| r.is_err()).unwrap_or(Ok(()))
}

/// Run a runtime bind `f` in a settle frame of its own and settle what
/// it deferred when it returns: no statement boundary follows a bind
/// at run time. A refused settle is a compiler bug, logged.
pub(crate) fn with_runtime_settles<R: Rt, E: UserEvent, T>(
    ctx: &mut ExecCtx<R, E>,
    f: impl FnOnce(&mut ExecCtx<R, E>) -> Result<T>,
) -> Result<T> {
    ctx.pending_settles.push(Vec::new());
    let names = ctx.pending_names.len();
    let res = {
        let _restore = Restore::replace(&RUNTIME_BIND, true);
        f(ctx)
    };
    // a runtime bind elaborates: what it defers is no check's
    ctx.pending_names.truncate(names);
    let pending = ctx.pending_settles.pop().expect("runtime settle frame");
    let _ = crate::PendingSettle::drain(&pending, &ctx.env, |spec, e| {
        log::error!("a run-time bind's settle at {spec} refused: {e:#}");
        Ok(())
    });
    res
}

/// Defer `settle` to the check's settle of the current frame. A
/// definition's body defers nothing: its cells are its signature's,
/// settled by each call.
pub(crate) fn defer_settle<R: Rt, E: UserEvent>(
    ctx: &mut CompileCtx<R, E>,
    settle: impl FnOnce() -> crate::PendingSettle,
) {
    if ctx.def_gate_depth == 0 {
        ctx.pending_settles.last_mut().expect("settle frame").push(settle())
    }
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
    let mut work: LPooled<Vec<(&mut Node<R, E>, CompileCtx<R, E>)>> = order
        .iter()
        .map(|i| {
            let mut task = ctx.fork();
            task.task = crate::typ::tvar::new_task();
            (slots[*i].take().expect("an order visits each once"), task)
        })
        .collect();
    let level = crate::typ::tvar::current_level();
    let mut results: Vec<Result<()>> = work
        .par_iter_mut()
        .map(|(n, task)| {
            let _level = crate::typ::tvar::AtLevel::enter(level);
            let _task = crate::typ::tvar::InTask::enter(task.task);
            let r = wrap!(n, typecheck1_settled(n, task));
            match module {
                true => r.with_context(|| n.spec().ori.clone()),
                false => r,
            }
        })
        .collect();
    for (_, task) in work.drain(..) {
        ctx.join(task);
    }
    results.drain(..).find(|r| r.is_err()).unwrap_or(Ok(()))
}

impl<R: Rt, E: UserEvent> Block<R, E> {
    /// Whether a `catch` covers the block's value, its last child.
    pub(crate) fn value_is_caught(&self) -> bool {
        self.catches.first().is_some_and(|i| i + 1 < self.children.len())
    }

    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<R, E>,
        buf: &mut &[u8],
    ) -> Result<Node<R, E>, PackError> {
        let module = bool::decode(buf)?;
        let spec = Expr::decode(buf)?;
        let children = decode_nodes(ctx, buf)?.into_boxed_slice();
        let catches = Vec::<usize>::decode(buf)?.into_boxed_slice();
        Ok(Node::new(Self {
            module,
            spec,
            children,
            catches,
            resident: TagValue::phantom(),
        }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Block<R, E> {
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::Block, buf);
        self.module.encode(buf)?;
        self.spec.encode(buf)?;
        encode_nodes(&self.children, buf)?;
        image::slice_encode(&self.catches, buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> &TagValue {
        if self.catches.is_empty() {
            let res = self
                .children
                .iter_mut()
                .fold(TagValue::phantom_ref(), |_, n| n.update(ctx, event));
            return if self.module { TagValue::phantom_ref() } else { res };
        }
        // the value is the last syntactic child's (absent if it is a catch)
        let last = self.children.len() - 1;
        let mut res: Option<TagValue> = None;
        for i in evaluation_order(self.children.len(), &self.catches) {
            let r = self.children[i].update(ctx, event);
            if i == last && self.catches.last() != Some(&last) {
                res = Some(r.clone());
            }
        }
        match res {
            Some(tv) if !self.module => self.resident.set(tv),
            _ if self.module => TagValue::phantom_ref(),
            _ => self.resident.ride(),
        }
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        for n in &mut self.children {
            n.delete(ctx)
        }
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        for n in &mut self.children {
            n.sleep(ctx)
        }
    }

    fn refs(&self, refs: &mut Refs) {
        for n in &self.children {
            n.refs(refs)
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
        fusion::fuse_each(ctx, self.children.iter_mut(), fuse)?;
        Ok(None)
    }
}

#[derive(Debug)]
pub struct StringInterpolate<R: Rt, E: UserEvent> {
    /// set by `sleep()`, taken by the next update
    slept: WakeBit,
    pub(crate) spec: Expr,
    pub typ: Type,
    pub(crate) typs: Box<[Type]>,
    pub args: Box<[Node<R, E>]>,
    resident: TagValue,
}

impl<R: Rt, E: UserEvent> StringInterpolate<R, E> {
    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<R, E>,
        buf: &mut &[u8],
    ) -> Result<Node<R, E>, PackError> {
        let spec = Expr::decode(buf)?;
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
        }))
    }

    pub(crate) fn compile(
        ctx: &mut CompileCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: Expr,
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
        }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for StringInterpolate<R, E> {
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::StringInterpolate, buf);
        self.spec.encode(buf)?;
        self.typ.encode(buf)?;
        image::slice_encode(&self.typs, buf)?;
        encode_nodes(&self.args, buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> &TagValue {
        use std::fmt::Write;
        // rendered under the value-hook loan so a core `Display` impl on
        // an abstract part applies (`coretraits::with_display_hooks`)
        let (tag, prods) = gather(ctx, event, &mut self.args);
        dense_gate!(self, tag.triggers(), tag.is_bottom());
        let mut buf: LPooled<String> = LPooled::take();
        coretraits::with_display_hooks(ctx, event, |env| {
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

    fn refs(&self, refs: &mut Refs) {
        for a in &self.args {
            a.refs(refs)
        }
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        for n in &mut self.args {
            n.delete(ctx)
        }
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.slept.set();
        for n in &mut self.args {
            n.sleep(ctx);
        }
    }

    fn fuse(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<Option<Node<R, E>>> {
        fusion::fuse_parts(self.args.iter_mut(), ctx)
    }

    fn typecheck0(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
        self.typecheck0_with(ctx, &mut |n, ctx| n.typecheck0(ctx))
    }

    fn typecheck0_instance(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        types: &mut lambda::InstanceTypes,
    ) -> Result<()> {
        self.typecheck0_with(ctx, &mut |n, ctx| n.typecheck0_instance(ctx, types))
    }

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
        emit_string_interpolate_node(cx, &self.args)
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
    pub(crate) spec: Expr,
    pub(super) node: Node<R, E>,
    pub(crate) id: BindId,
}

impl<R: Rt, E: UserEvent> Connect<R, E> {
    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<R, E>,
        buf: &mut &[u8],
    ) -> Result<Node<R, E>, PackError> {
        let spec = Expr::decode(buf)?;
        let node = decode_node(ctx, buf)?;
        let id = BindId::decode(buf)?;
        Ok(Node::new(Self { spec, node, id }))
    }

    pub(crate) fn compile(
        ctx: &mut CompileCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: Expr,
        scope: &Scope,
        top_id: ExprId,
        name: &ModPath,
        value: &Expr,
    ) -> Result<Node<R, E>> {
        let (id, def_pos, def_ori) =
            match ctx.env.lookup_bind(&scope.lexical, name).map_err(|e| e.at(&spec))? {
                None => bailat!(spec, "{name} is undefined"),
                Some((_, b)) => (b.id, b.pos, b.ori.clone()),
            };
        // a `<-` target is never a static call target
        ctx.mark_connect_target(id);
        if ctx.env.ide.is_lsp() {
            ctx.env.push_reference(ReferenceSite {
                pos: spec.pos,
                ori: spec.ori.clone(),
                name: name.clone(),
                bind_id: id,
                def_pos,
                def_ori,
            });
        }
        let node = compile(ctx, flags, value.clone(), scope, top_id)?;
        Ok(Node::new(Self { spec, node, id }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Connect<R, E> {
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::Connect, buf);
        self.spec.encode(buf)?;
        self.node.image_encode(buf)?;
        self.id.encode(buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> &TagValue {
        // only a fired RHS writes
        let tv = self.node.update(ctx, event);
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

    fn refs(&self, refs: &mut Refs) {
        self.node.refs(refs)
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.node.delete(ctx)
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.node.sleep(ctx);
    }

    fn fuse(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<Option<Node<R, E>>> {
        fusion::fuse_parts([&mut self.node], ctx)
    }

    fn typecheck0(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
        self.typecheck0_with(ctx, &mut |n, ctx| n.typecheck0(ctx))
    }

    fn typecheck0_instance(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        types: &mut lambda::InstanceTypes,
    ) -> Result<()> {
        self.typecheck0_with(ctx, &mut |n, ctx| n.typecheck0_instance(ctx, types))
    }

    fn typecheck1(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
        wrap!(self.node, self.node.typecheck1(ctx))?;
        Ok(())
    }

    fn view(&self) -> NodeView<'_, R, E> {
        NodeView::Connect(self)
    }

    fn emit_clif(&self, _cx: &mut BodyCx) -> Result<CompiledExpr> {
        Err(anyhow::anyhow!("emit_clif: connect is an effect — node-walks"))
    }
}

/// A written `<-` whose value `bind` cannot hold: the target, its type,
/// and the declaration that would hold both.
fn write_mismatch(bind: &env::Bind, written: &Type, at: &Arc<Origin>) -> anyhow::Error {
    let deref = |t: &Type| t.deref_cloned().unwrap_or_else(|| t.clone());
    let (held, written) = (deref(&bind.typ), deref(written));
    let both = Type::Set(Arc::from_iter([held.clone(), written.clone()])).normalize();
    let inferred = match &bind.typ {
        Type::TVar(_) => ", inferred from an earlier use,",
        _ => "",
    };
    let file = match bind.ori == *at {
        true => format_compact!(""),
        false => format_compact!(" {}", bind.ori),
    };
    let (name, line, col) = (&bind.name, bind.pos.line, bind.pos.column);
    format_with_flags(PrintFlag::DerefTVars | PrintFlag::ReplacePrims, || {
        anyhow!(
            "{name} is {held}{inferred} and cannot hold {written}; declare \
             {name}: {both} where it is bound (line {line}, column {col}{file})"
        )
    })
}

/// Where a write through a reference lands: a bound variable, or a
/// place inside one (`design/place_references.md`).
#[derive(Debug, Clone, PartialEq)]
pub(super) enum WriteTarget {
    Bind(BindId),
    Place(BindId, place::Path),
}

#[derive(Debug)]
pub struct ConnectDeref<R: Rt, E: UserEvent> {
    pub(crate) spec: Expr,
    pub(super) rhs: Node<R, E>,
    pub(super) src_id: BindId,
    pub(super) target: Option<WriteTarget>,
    pub(super) top_id: ExprId,
}

impl<R: Rt, E: UserEvent> ConnectDeref<R, E> {
    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<R, E>,
        buf: &mut &[u8],
    ) -> Result<Node<R, E>, PackError> {
        let spec = Expr::decode(buf)?;
        let rhs = decode_node(ctx, buf)?;
        let src_id = BindId::decode(buf)?;
        let top_id = ExprId::decode(buf)?;
        ctx.record_ref(src_id, top_id);
        Ok(Node::new(Self { spec, rhs, src_id, target: None, top_id }))
    }

    pub(crate) fn compile(
        ctx: &mut CompileCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: Expr,
        scope: &Scope,
        top_id: ExprId,
        name: &ModPath,
        value: &Expr,
    ) -> Result<Node<R, E>> {
        let (src_id, def_pos, def_ori) =
            match ctx.env.lookup_bind(&scope.lexical, name).map_err(|e| e.at(&spec))? {
                None => bailat!(spec, "{name} is undefined"),
                Some((_, b)) => (b.id, b.pos, b.ori.clone()),
            };
        if ctx.env.ide.is_lsp() {
            ctx.env.push_reference(ReferenceSite {
                pos: spec.pos,
                ori: spec.ori.clone(),
                name: name.clone(),
                bind_id: src_id,
                def_pos,
                def_ori,
            });
        }
        ctx.record_ref(src_id, top_id);
        let rhs = compile(ctx, flags, value.clone(), scope, top_id)?;
        Ok(Node::new(Self { spec, rhs, src_id, target: None, top_id }))
    }

    /// Resolve a reference value (the cell a `&` minted) to where a
    /// write lands: a place, when the cell stands for one, else the
    /// referent through the byref chain. A chainless plain reference
    /// (`&(a + b)`) has nowhere to write.
    fn resolve(ctx: &ExecCtx<R, E>, tv: &TagValue) -> Option<WriteTarget> {
        let cell = tv.with_value(|v| match v {
            Value::U64(id) => Some(BindId::from(*id)),
            _ => None,
        })?;
        if let Some((root, path)) = ctx.rt.ref_path(&cell) {
            return Some(WriteTarget::Place(*root, path.clone()));
        }
        ctx.env.byref_chain.get(&cell).map(|id| WriteTarget::Bind(*id))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for ConnectDeref<R, E> {
    /// `target` is resolved at update: a resolved one is runtime state.
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        if self.target.is_some() {
            return Err(PackError::Application(image::NOT_QUIESCENT));
        }
        put_tag(NodeTag::ConnectDeref, buf);
        self.spec.encode(buf)?;
        self.rhs.image_encode(buf)?;
        self.src_id.encode(buf)?;
        self.top_id.encode(buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> &TagValue {
        // a fired RHS writes; a retarget writes the RHS's current value;
        // a bottom RHS never writes
        let (rhs_fired, rhs_val) = {
            let tv = self.rhs.update(ctx, event);
            let t = tv.tag();
            (t.is_fired(), if t.is_bottom() { None } else { Some(tv.value_cloned()) })
        };
        let mut up = rhs_fired;
        if let Some(tv) = event.variables.get(&self.src_id) {
            // a reference delivered without a target (a place whose
            // address is undetermined) has nowhere to write
            let t = Self::resolve(ctx, tv);
            if self.target != t {
                self.target = t;
                up = true;
            }
        } else if self.target.is_none() {
            // an instance created after the reference value was delivered
            // finds it only in the standing store
            if let Some(read) = read_var(ctx, event, &self.src_id) {
                let tv = match read {
                    VarRead::Delivered(tv) | VarRead::Standing(tv) => tv,
                };
                if let Some(t) = Self::resolve(ctx, tv) {
                    self.target = Some(t);
                    up = true;
                }
            }
        }
        if up {
            match (rhs_val, &self.target) {
                (Some(v), Some(WriteTarget::Bind(id))) => ctx.rt.set_var(*id, v),
                (Some(v), Some(WriteTarget::Place(root, path))) => {
                    ctx.rt.patch_var(*root, path.clone(), v)
                }
                _ => (),
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

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        ctx.unref_var(self.src_id, self.top_id);
        self.rhs.delete(ctx)
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.rhs.sleep(ctx);
    }

    fn typecheck0(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
        self.typecheck0_with(ctx, &mut |n, ctx| n.typecheck0(ctx))
    }

    fn typecheck0_instance(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        types: &mut lambda::InstanceTypes,
    ) -> Result<()> {
        self.typecheck0_with(ctx, &mut |n, ctx| n.typecheck0_instance(ctx, types))
    }

    fn typecheck1(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
        wrap!(self.rhs, self.rhs.typecheck1(ctx))?;
        Ok(())
    }

    fn view(&self) -> NodeView<'_, R, E> {
        NodeView::ConnectDeref(self)
    }
}

#[derive(Debug)]
pub struct TypeCast<R: Rt, E: UserEvent> {
    slept: WakeBit,
    pub(crate) spec: Expr,
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
        ctx: &mut ExecCtx<R, E>,
        buf: &mut &[u8],
    ) -> Result<Node<R, E>, PackError> {
        let spec = Expr::decode(buf)?;
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
        spec: Expr,
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
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::TypeCast, buf);
        self.spec.encode(buf)?;
        self.typ.encode(buf)?;
        self.target.encode(buf)?;
        self.n.image_encode(buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> &TagValue {
        let tv = self.n.update(ctx, event);
        let tag = tv.tag();
        dense_gate!(self, tag.triggers(), tag.is_bottom());
        let v = tv.value_cloned();
        let v = if self.src_ref {
            errf!(CAST_ERR_TAG, "can't cast a reference")
        } else {
            self.target.cast_from(&ctx.env, self.n.typ(), v)
        };
        self.resident.set(TagValue::tagged(v, tag))
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn typ(&self) -> &Type {
        &self.typ
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.n.delete(ctx)
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.slept.set();
        self.n.sleep(ctx);
    }

    fn fuse(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<Option<Node<R, E>>> {
        fusion::fuse_parts([&mut self.n], ctx)
    }

    fn refs(&self, refs: &mut Refs) {
        self.n.refs(refs)
    }

    fn typecheck0(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
        self.typecheck0_with(ctx, &mut |n, ctx| n.typecheck0(ctx))?;
        if self.n.typ().holds_ref(&ctx.env) {
            bailat!(self.spec, "can't cast a reference")
        }
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
    pub(crate) spec: Expr,
    pub typ: Type,
    pub n: Box<[Node<R, E>]>,
}

impl<R: Rt, E: UserEvent> Never<R, E> {
    pub(crate) fn compile(
        ctx: &mut CompileCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: Expr,
        scope: &Scope,
        top_id: ExprId,
        typ: &Option<Type>,
        args: &[Expr],
    ) -> Result<Node<R, E>> {
        let n = args
            .iter()
            .map(|e| compile(ctx, flags, e.clone(), scope, top_id))
            .collect::<Result<Box<[_]>>>()?;
        let typ = match typ {
            Some(t) => t.rewrite_trait_args(&ctx.env)?.scope_refs(&scope.lexical),
            None => Type::Bottom,
        };
        Ok(Node::new(Self { spec, typ, n }))
    }
}

impl<R: Rt, E: UserEvent> Never<R, E> {
    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<R, E>,
        buf: &mut &[u8],
    ) -> Result<Node<R, E>, PackError> {
        let spec = Expr::decode(buf)?;
        let typ = Type::decode(buf)?;
        let n = decode_nodes(ctx, buf)?.into_boxed_slice();
        Ok(Node::new(Self { spec, typ, n }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Never<R, E> {
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::Never, buf);
        self.spec.encode(buf)?;
        self.typ.encode(buf)?;
        encode_nodes(&self.n, buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> &TagValue {
        for n in self.n.iter_mut() {
            n.update(ctx, event);
        }
        TagValue::phantom_ref()
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn typ(&self) -> &Type {
        &self.typ
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.n.iter_mut().for_each(|n| n.delete(ctx))
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.n.iter_mut().for_each(|n| n.sleep(ctx))
    }

    fn fuse(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<Option<Node<R, E>>> {
        fusion::fuse_parts(self.n.iter_mut(), ctx)
    }

    fn refs(&self, refs: &mut Refs) {
        self.n.iter().for_each(|n| n.refs(refs))
    }

    fn typecheck0(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
        crate::defer_unresolved_names(ctx, &self.typ, &self.spec);
        self.typecheck0_with(ctx, &mut |n, ctx| n.typecheck0(ctx))
    }

    fn typecheck0_instance(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        types: &mut lambda::InstanceTypes,
    ) -> Result<()> {
        self.typecheck0_with(ctx, &mut |n, ctx| n.typecheck0_instance(ctx, types))
    }

    fn typecheck1(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
        for n in self.n.iter_mut() {
            wrap!(n, n.typecheck1(ctx))?
        }
        Ok(())
    }

    fn view(&self) -> NodeView<'_, R, E> {
        NodeView::Never(self)
    }
}

#[derive(Debug)]
pub struct Any<R: Rt, E: UserEvent> {
    pub(crate) spec: Expr,
    pub typ: Type,
    pub n: Box<[Node<R, E>]>,
    resident: TagValue,
}

impl<R: Rt, E: UserEvent> Any<R, E> {
    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<R, E>,
        buf: &mut &[u8],
    ) -> Result<Node<R, E>, PackError> {
        let spec = Expr::decode(buf)?;
        let typ = Type::decode(buf)?;
        let n = decode_nodes(ctx, buf)?.into_boxed_slice();
        Ok(Node::new(Self { spec, typ, n, resident: TagValue::phantom() }))
    }

    pub(crate) fn compile(
        ctx: &mut CompileCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: Expr,
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
        }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Any<R, E> {
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::Any, buf);
        self.spec.encode(buf)?;
        self.typ.encode(buf)?;
        encode_nodes(&self.n, buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> &TagValue {
        // the first triggering value-bearing production wins; a
        // triggering bottom never beats one (`any(risky?, default)`)
        let mut winner: Option<TagValue> = None;
        let mut bottomed = false;
        for s in self.n.iter_mut() {
            let tv = s.update(ctx, event);
            let tag = tv.tag();
            if tag.triggers() {
                if tag.is_bottom() {
                    bottomed = true;
                } else if winner.is_none() {
                    winner = Some(tv.clone());
                }
            }
        }
        match winner {
            Some(tv) => self.resident.set(tv),
            None if bottomed => self.resident.set_bottom(true),
            None => self.resident.ride(),
        }
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn typ(&self) -> &Type {
        &self.typ
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.n.iter_mut().for_each(|n| n.delete(ctx))
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.n.iter_mut().for_each(|n| n.sleep(ctx))
    }

    fn fuse(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<Option<Node<R, E>>> {
        fusion::fuse_parts(self.n.iter_mut(), ctx)
    }

    fn refs(&self, refs: &mut Refs) {
        self.n.iter().for_each(|n| n.refs(refs))
    }

    fn typecheck0(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
        self.typecheck0_with(ctx, &mut |n, ctx| n.typecheck0(ctx))
    }

    fn typecheck0_instance(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        types: &mut lambda::InstanceTypes,
    ) -> Result<()> {
        self.typecheck0_with(ctx, &mut |n, ctx| n.typecheck0_instance(ctx, types))
    }

    fn typecheck1(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
        for n in self.n.iter_mut() {
            wrap!(n, n.typecheck1(ctx))?
        }
        Ok(())
    }

    fn view(&self) -> NodeView<'_, R, E> {
        NodeView::Any(self)
    }
}

/// How `~` answers a trigger that finds its RHS absent.
#[derive(Debug)]
enum Banking {
    /// `~!`: the trigger produces bottom and banks nothing.
    Strict,
    /// `~`: the trigger is banked and paid through the private `id` at
    /// the RHS's first value.
    Debt { triggered: usize, id: BindId },
}

#[derive(Debug)]
pub struct Sample<R: Rt, E: UserEvent> {
    pub(crate) spec: Expr,
    banking: Banking,
    pub typ: Type,
    top_id: ExprId,
    pub trigger: Node<R, E>,
    pub arg: Held<R, E>,
    resident: TagValue,
}

impl<R: Rt, E: UserEvent> Sample<R, E> {
    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<R, E>,
        buf: &mut &[u8],
    ) -> Result<Node<R, E>, PackError> {
        let spec = Expr::decode(buf)?;
        let typ = Type::decode(buf)?;
        let top_id = ExprId::decode(buf)?;
        let banking = match Option::<BindId>::decode(buf)? {
            None => Banking::Strict,
            Some(id) => {
                ctx.record_ref(id, top_id);
                Banking::Debt { triggered: 0, id }
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
        spec: Expr,
        scope: &Scope,
        top_id: ExprId,
        lhs: &Arc<Expr>,
        rhs: &Arc<Expr>,
        strict: bool,
    ) -> Result<Node<R, E>> {
        let banking = if strict {
            Banking::Strict
        } else {
            let id = BindId::new();
            ctx.record_ref(id, top_id);
            Banking::Debt { triggered: 0, id }
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
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::Sample, buf);
        self.spec.encode(buf)?;
        self.typ.encode(buf)?;
        self.top_id.encode(buf)?;
        self.debt_id().encode(buf)?;
        self.trigger.image_encode(buf)?;
        self.arg.image_encode(buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> &TagValue {
        // only a fired trigger samples or banks debt
        let t = self.trigger.update(ctx, event);
        let fired = t.tag().is_fired();
        self.arg.update(ctx, event);
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
            Banking::Debt { triggered, id } => (triggered, *id),
        };
        if fired {
            *triggered += 1;
        }
        let var = event.variables.get(&id).cloned();
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
            (Some(v), _) => self.resident.set(TagValue::fired(v.clone())),
            (None, Some(tv)) => self.resident.set(tv),
            (None, None) => self.resident.ride(),
        }
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        if let Some(id) = self.debt_id() {
            ctx.unref_var(id, self.top_id);
            ctx.rt.store_remove(&id);
        }
        self.arg.node.delete(ctx);
        self.trigger.delete(ctx);
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
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

    fn typecheck0(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
        self.typecheck0_with(ctx, &mut |n, ctx| n.typecheck0(ctx))
    }

    fn typecheck0_instance(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        types: &mut lambda::InstanceTypes,
    ) -> Result<()> {
        self.typecheck0_with(ctx, &mut |n, ctx| n.typecheck0_instance(ctx, types))
    }

    fn typecheck1(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
        wrap!(self.trigger, self.trigger.typecheck1(ctx))?;
        wrap!(self.arg.node, self.arg.node.typecheck1(ctx))?;
        Ok(())
    }

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
    ) -> Result<()> {
        wrap!(self.node, child(&mut self.node, ctx))?;
        let bind = match ctx.env.by_id.get(&self.id) {
            None => bail!("BUG missing bind {:?}", self.id),
            Some(bind) => bind,
        };
        let written = self.node.typ();
        wrap!(
            self,
            bind.typ.check_contains(&ctx.env, written).map_err(|e| {
                match e.downcast_ref::<TypeMismatch>() {
                    Some(_) if self.spec.end.get().is_some() => {
                        write_mismatch(bind, written, &self.spec.ori)
                    }
                    _ => e,
                }
            })
        )
    }
}

impl<R: Rt, E: UserEvent> ConnectDeref<R, E> {
    fn typecheck0_with(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        child: &mut Child<'_, R, E>,
    ) -> Result<()> {
        wrap!(self.rhs, child(&mut self.rhs, ctx))?;
        let bind = match ctx.env.by_id.get(&self.src_id) {
            None => bail!("BUG missing bind {:?}", self.src_id),
            Some(bind) => bind,
        };
        let typ = Type::ByRef(Arc::new(self.rhs.typ().clone()));
        wrap!(self, bind.typ.check_contains(&ctx.env, &typ))
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
    ) -> Result<()> {
        for n in self.n.iter_mut() {
            wrap!(n, child(n, ctx))?
        }
        let bottom = Type::Bottom;
        let mut ts: LPooled<Vec<&Type>> = LPooled::take();
        ts.push(&bottom);
        ts.extend(self.n.iter().map(|n| n.typ()));
        let rtyp = wrap!(self, Type::union(&ctx.env, &ts))?;
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
