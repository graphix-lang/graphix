use crate::image::{
    self,
    nodes::{
        NodeTag, decode_node, decode_nodes, encode_nodes, nodes_len, put_tag, tag_len,
    },
};
use crate::{
    BindId, CAST_ERR, CFlag, Event, ExecCtx, Node, NodeView, PendingImport, Refs, Rt,
    Scope, Tag, TagValue, Update, UserEvent,
    env::{Env, ImportEntry},
    expr::{ErrorContext, Expr, ExprId, ExprKind, ModPath, TypeDefBody},
    fusion::{
        emit::{
            BodyCx, CompiledExpr, emit_block_node, emit_cast_node, emit_const_node,
            emit_string_interpolate_node,
        },
        fuse,
    },
    ide::{ModuleRefSite, ReferenceSite},
    typ::{TVal, TVar, Type},
};
use anyhow::{Context, Result, bail};
use arcstr::{ArcStr, literal};
use bytes::{Buf, BufMut};
use crate::image::ImageBuf;
use compiler::{compile, compile_module};
use enumflags2::BitFlags;
use netidx_core::pack::{Pack, PackError};
use netidx_value::{Typ, Value};
use poolshark::local::LPooled;
use std::sync::LazyLock;
use triomphe::Arc;

pub(crate) mod array;
pub use array::MAX_ARRAY_INIT_LEN;
pub(crate) mod bind;
pub mod callsite;
pub mod collection;
pub(crate) mod compiler;
pub mod coretraits;
pub(crate) mod data;
pub(crate) mod error;
pub mod genn;
pub mod lambda;
pub(crate) mod map;
pub(crate) mod module;
pub(crate) mod op;
pub(crate) mod pattern;
pub mod place;
pub(crate) mod select;
pub mod traits;

/// A variable read's provenance under dense delivery — see [`read_var`].
pub(crate) enum VarRead<'a> {
    /// Found in an overlay (this cycle's transient deliveries, or a
    /// frame's private writes) or store-stamped THIS cycle: the
    /// entry's own tag applies.
    Delivered(&'a TagValue),
    /// A standing store entry from an earlier cycle: the value
    /// channel. Readers view it Stale, or Fired under an init view.
    Standing(&'a TagValue),
}

/// Read a variable: innermost overlay, then the enclosing frame stack
/// (an inner dispatch's captures live in its caller's frame), then the
/// persistent store. `None` means the bind has never delivered.
pub(crate) fn read_var<'a, R: Rt, E: UserEvent>(
    ctx: &'a ExecCtx<R, E>,
    event: &'a Event<E>,
    id: &BindId,
) -> Option<VarRead<'a>> {
    if let Some(tv) = event.variables.get(id) {
        return Some(VarRead::Delivered(tv));
    }
    for f in event.frames.iter().rev() {
        if let Some(tv) = f.get(id) {
            return Some(VarRead::Delivered(tv));
        }
    }
    match ctx.rt.store().get(id) {
        Some((tv, stamp)) if *stamp == ctx.rt.cycle() => Some(VarRead::Delivered(tv)),
        Some((tv, _)) => Some(VarRead::Standing(tv)),
        None => None,
    }
}

#[macro_export]
macro_rules! wrap {
    ($n:expr, $e:expr) => {
        match $e {
            Ok(x) => Ok(x),
            e => {
                anyhow::Context::context(e, $crate::expr::ErrorContext($n.spec().clone()))
            }
        }
    };
}

/// Compile-time `bail!` that attaches an `ErrorContext` carrying the
/// expression's `Origin` and `SourcePosition`, which the LSP downcasts
/// out of the anyhow chain. Use it wherever the spec `Expr` is in scope.
#[macro_export]
macro_rules! bailat {
    ($spec:expr, $($arg:tt)*) => {
        return ::std::result::Result::Err(
            <::anyhow::Error>::context(
                ::anyhow::anyhow!($($arg)*),
                $crate::expr::ErrorContext(::std::clone::Clone::clone(&$spec)),
            )
        )
    };
}

#[macro_export]
macro_rules! deref_typ {
    ($name:literal, $ctx:expr, $typ:expr, $($pat:pat => $body:expr),+) => {
        $typ.with_deref(|typ| {
            let mut typ = typ.cloned();
            // an alias chain deeper than 64 is a cyclic typedef
            let mut depth = 0usize;
            loop {
                #[allow(unreachable_patterns)]
                match &typ {
                    $($pat => break $body),+,
                    Some(rt @ $crate::typ::Type::Ref($crate::typ::TypeRef { .. })) => {
                        depth += 1;
                        if depth > 64 {
                            $crate::format_with_flags(PrintFlag::DerefTVars, || {
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
                            $crate::format_with_flags(PrintFlag::DerefTVars, || {
                                anyhow::bail!("expected {} not {nt}", $name)
                            })?
                        }
                        typ = Some(nt);
                    }
                    Some(t) => $crate::format_with_flags(PrintFlag::DerefTVars, || {
                        anyhow::bail!("expected {} not {t}", $name)
                    })?,
                    None => anyhow::bail!("type must be known, annotations needed")
                }
            }
        })
    };
}

pub(crate) static NOP: LazyLock<Arc<Expr>> = LazyLock::new(|| {
    Arc::new(
        ExprKind::Constant(Value::String(literal!("nop"))).to_expr(Default::default()),
    )
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
    fn image_len(&self) -> usize {
        tag_len() + self.typ.encoded_len()
    }

    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::Nop, buf);
        self.typ.encode(buf)
    }

    fn update(&mut self, _ctx: &mut ExecCtx<R, E>, _event: &mut Event<E>) -> &TagValue {
        TagValue::phantom_ref()
    }

    fn delete(&mut self, _ctx: &mut ExecCtx<R, E>) {}

    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {}

    fn reset_replay(&mut self, _ctx: &mut ExecCtx<R, E>) {}

    fn typecheck0(&mut self, _ctx: &mut ExecCtx<R, E>) -> Result<()> {
        Ok(())
    }

    fn typecheck1(&mut self, _ctx: &mut ExecCtx<R, E>) -> Result<()> {
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
        ctx: &mut ExecCtx<R, E>,
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
    fn image_len(&self) -> usize {
        tag_len() + self.spec.encoded_len() + self.n.image_len()
    }

    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::ExplicitParens, buf);
        self.spec.encode(buf)?;
        self.n.image_encode(buf)
    }

    fn fuse(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<Option<Node<R, E>>> {
        // parens are a fusion boundary: the interior gets its own region pass
        crate::fusion::fuse(&mut self.n, ctx)?;
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

    fn reset_replay(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.n.reset_replay(ctx);
    }

    fn typecheck0(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        self.n.typecheck0(ctx)
    }

    fn typecheck1(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        self.n.typecheck1(ctx)?;
        Ok(())
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
    /// Lazily computed: the subtree references no bindings at all, so
    /// its value is identical in every evaluation frame — see
    /// [`Self::reset_replay`].
    invariant: std::sync::OnceLock<bool>,
}

impl<R: Rt, E: UserEvent> Held<R, E> {
    pub(crate) fn image_len(&self) -> usize {
        self.node.image_len()
    }

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
        Self { value: None, tag: Tag::FIRED, node, invariant: std::sync::OnceLock::new() }
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

    /// [`Self::update`], reduced to whether the production triggers
    /// evaluation: true for fired and fresh-bottom productions, false
    /// for the stale states. Bottomness is read back off [`Self::tag`].
    pub fn update_triggers(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        event: &mut Event<E>,
    ) -> bool {
        self.update(ctx, event).triggers()
    }

    /// Sleep is pause, not reset: the held value and its at-rest taint
    /// survive. Contrast [`Self::reset_replay`].
    pub fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.node.sleep(ctx)
    }

    /// Clears the held value, except when the subtree references no
    /// bindings: such a value is identical in every frame and the
    /// subtree cannot re-produce it without an init view.
    pub fn reset_replay(&mut self, ctx: &mut ExecCtx<R, E>) {
        let invariant = *self.invariant.get_or_init(|| {
            let mut refs = Refs::default();
            self.node.refs(&mut refs);
            refs.refed.is_empty()
        });
        if !invariant {
            self.value = None;
            self.tag = Tag::FIRED;
        }
        self.node.reset_replay(ctx)
    }
}

/// Update every child of a composite, join the tags, and clone the
/// values. Returns `(trig, fired, bottom)`; `vals` receives the element
/// values in order and is meaningful only when `bottom` is false.
pub(crate) fn gather<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<R, E>,
    event: &mut Event<E>,
    nodes: &mut [Node<R, E>],
    vals: &mut Vec<Value>,
) -> (bool, bool, bool) {
    let (mut trig, mut fired, mut bottom) = (false, false, false);
    for c in nodes.iter_mut() {
        let tv = c.update(ctx, event);
        let t = tv.tag();
        trig |= t.triggers();
        fired |= t.is_fired();
        if t.is_bottom() {
            bottom = true
        } else if !bottom {
            vals.push(tv.value_cloned())
        }
    }
    (trig, fired, bottom)
}

/// A strict computation propagates consumed bottom before considering
/// its cached result. Quiet recomputation cannot manufacture an event.
macro_rules! dense_gate {
    ($self:ident, $ctx:ident, $trig:expr, $bottom:expr) => {{
        let woke = $self.slept.take();
        $crate::node::dense_gate!($self.resident, $ctx, $trig, $bottom, woke);
    }};
    ($resident:expr, $ctx:ident, $trig:expr, $bottom:expr, $woke:expr) => {{
        if $bottom {
            let tag =
                if $trig { $crate::Tag::FRESH_BOTTOM } else { $crate::Tag::STALE_BOTTOM };
            return $resident.set($crate::TagValue::tagged(Value::Null, tag));
        }
        if !($trig || $resident.tag().is_bottom() || $ctx.frame_depth > 0 || $woke) {
            return $resident.ride();
        }
    }};
}
pub(crate) use dense_gate;

/// Read one child's dense production into the caller's join
/// accumulators, yielding `Some(value)` for the value-bearing states
/// and `None` for a bottom (which also sets `$bottom`).
macro_rules! read_prod {
    ($n:expr, $ctx:ident, $event:ident, $trig:ident, $fired:ident, $bottom:ident) => {{
        let tv = $n.update($ctx, $event);
        let t = tv.tag();
        $trig |= t.triggers();
        $fired |= t.is_fired();
        if t.is_bottom() {
            $bottom = true;
            None
        } else {
            Some(tv.value_cloned())
        }
    }};
}
pub(crate) use read_prod;

/// Compile one `use` item into the scope's namespace table
/// ([`crate::env::Env::names`]): resolve its module prefix, then
/// install a glob source or an explicit [`ImportEntry`].
pub(crate) fn compile_use_item(
    env: &mut Env,
    pending: &mut Vec<PendingImport>,
    pos: combine::stream::position::SourcePosition,
    ori: &Arc<crate::expr::Origin>,
    scope: &Scope,
    replace: bool,
    item: &crate::expr::UseItem,
) -> Result<()> {
    use netidx_core::path::Path;
    let parts: LPooled<Vec<&str>> = Path::parts(&*item.path.0).collect();
    let Some((&base, prefix)) = parts.split_last() else { bail!("use: empty path") };
    // a bare keyword anchor resolves along its lexical chain (a `super`
    // anchor may be a block level); everything else is a canonical module
    enum Anchor<'a> {
        Chain(&'a str),
        Module(ModPath),
    }
    let n_super = prefix.iter().take_while(|s| **s == "super").count();
    let anchor = match prefix.first() {
        None => None,
        Some(&"self") if prefix.len() == 1 => {
            Some(Anchor::Chain(crate::mod_root(&scope.lexical)))
        }
        Some(&"super") if n_super == prefix.len() => {
            Some(Anchor::Chain(env.super_anchor(&scope.lexical, n_super)?))
        }
        Some(&"package") if prefix.len() == 1 => {
            Some(Anchor::Chain(env.package_root(&scope.lexical)))
        }
        Some(_) => {
            let p = ModPath(Path::from_iter(prefix.iter().copied()));
            match env.canonical_modpath(&scope.lexical, &p)? {
                Some(m) => Some(Anchor::Module(m)),
                None => bail!("use: no module `{p}` in scope"),
            }
        }
    };
    if item.is_glob() {
        let scope_l = &scope.lexical;
        match anchor {
            None => bail!("a glob needs a path prefix"),
            Some(Anchor::Chain(a)) => {
                // a `super::*` anchor may span block levels: capture
                // each level as its own glob source
                let levels: LPooled<Vec<ModPath>> = crate::env::chain_levels(a)
                    .map(|l| ModPath(Path::from(ArcStr::from(l))))
                    .collect();
                for l in levels.iter() {
                    env.import_glob(scope_l, l.clone());
                }
            }
            Some(Anchor::Module(m)) => env.import_glob(scope_l, m),
        }
        return Ok(());
    }
    let key: &str = item.rename.as_deref().unwrap_or(base);
    let entry = match anchor {
        Some(Anchor::Chain(a)) => ImportEntry {
            scope: ModPath(Path::from(ArcStr::from(a))),
            name: base.into(),
            keyword_anchored: true,
            pos,
            ori: ori.clone(),
        },
        Some(Anchor::Module(m)) => ImportEntry {
            scope: m,
            name: base.into(),
            keyword_anchored: false,
            pos,
            ori: ori.clone(),
        },
        None => {
            // `use m;` — a single segment names a module; importing
            // it means importing the name from its parent
            let p = ModPath(Path::from_iter([base]));
            match env.canonical_modpath(&scope.lexical, &p)? {
                Some(m) => ImportEntry {
                    scope: ModPath(Path::from(ArcStr::from(
                        Path::dirname(&*m).unwrap_or("/"),
                    ))),
                    name: base.into(),
                    keyword_anchored: false,
                    pos,
                    ori: ori.clone(),
                },
                None => bail!("use: no module `{base}` in scope"),
            }
        }
    };
    // the prelude already provides every package name as a path root
    if &**entry.scope == "/" && entry.name == key && env.package_roots.contains(key) {
        return Ok(());
    }
    if env.lsp_mode {
        let canonical =
            ModPath(Path::from(ArcStr::from(&**entry.scope)).append(&entry.name));
        env.push_module_reference(ModuleRefSite {
            pos,
            ori: ori.clone(),
            name: item.path.clone(),
            canonical,
            def_ori: None,
        });
    }
    if !env.import_target_exists(&entry) {
        pending.push(PendingImport {
            scope: scope.lexical.clone(),
            key: key.into(),
            pos,
            ori: ori.clone(),
        });
    }
    env.import(&scope.lexical, key, entry, replace)
}

/// Compile a `use` statement: every item registers in the namespace
/// table; the graph gets a [`Nop`].
pub(crate) fn compile_use<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<R, E>,
    flags: BitFlags<CFlag>,
    spec: Expr,
    scope: &Scope,
    reexport: bool,
    items: &Arc<[crate::expr::UseItem]>,
) -> Result<Node<R, E>> {
    if reexport {
        bail!("re-exports (`pub use`) are not yet supported")
    }
    let replace = flags.contains(CFlag::ReplaceImports);
    for item in items.iter() {
        compile_use_item(
            &mut ctx.env,
            &mut ctx.pending_imports,
            spec.pos,
            &spec.ori,
            scope,
            replace,
            item,
        )
        .with_context(|| ErrorContext(spec.clone()))?;
    }
    Ok(Nop::new(Type::Bottom))
}

#[derive(Debug)]
pub struct TypeDef {
    spec: Expr,
    scope: ModPath,
    name: ArcStr,
}

impl TypeDef {
    pub(crate) fn compile<R: Rt, E: UserEvent>(
        ctx: &mut ExecCtx<R, E>,
        spec: Expr,
        scope: &Scope,
        name: &ArcStr,
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
                spec.pos,
                spec.ori.clone(),
            )
            .with_context(|| format!("in typedef at {}", spec.pos))?;
        let name = name.clone();
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
    fn image_len(&self) -> usize {
        tag_len()
            + self.spec.encoded_len()
            + self.scope.encoded_len()
            + self.name.encoded_len()
    }

    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::TypeDef, buf);
        self.spec.encode(buf)?;
        self.scope.encode(buf)?;
        self.name.encode(buf)
    }

    fn update(&mut self, _ctx: &mut ExecCtx<R, E>, _event: &mut Event<E>) -> &TagValue {
        TagValue::phantom_ref()
    }

    fn typecheck0(&mut self, _ctx: &mut ExecCtx<R, E>) -> Result<()> {
        Ok(())
    }

    fn typecheck1(&mut self, _ctx: &mut ExecCtx<R, E>) -> Result<()> {
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

    fn reset_replay(&mut self, _ctx: &mut ExecCtx<R, E>) {}

    fn typ(&self) -> &Type {
        &Type::Bottom
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

    pub(crate) fn compile<R: Rt, E: UserEvent>(
        spec: Expr,
        value: &Value,
    ) -> Result<Node<R, E>> {
        let spec = Arc::new(spec);
        let value = value.clone();
        let typ = Type::Primitive(Typ::get(&value).into());
        let resident = TagValue::stale(value.clone());
        Ok(Node::new(Self { spec, value, typ, resident }))
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

impl<R: Rt, E: UserEvent> Update<R, E> for Constant {
    fn image_len(&self) -> usize {
        tag_len()
            + self.spec.encoded_len()
            + self.value.encoded_len()
            + self.typ.encoded_len()
    }

    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::Constant, buf);
        self.spec.encode(buf)?;
        self.value.encode(buf)?;
        self.typ.encode(buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> &TagValue {
        // frames force `event.init`, so the frame gate must come first;
        // a genuine init is always frame depth 0
        if ctx.frame_depth > 0 {
            // in a frame a constant fires only on a genuine init dispatch,
            // never on an arm's wake
            if ctx.dispatch_init {
                self.resident.set(TagValue::fired(self.value.clone()))
            } else {
                self.resident.set(TagValue::stale(self.value.clone()))
            }
        } else if event.init {
            self.resident.set(TagValue::fired(self.value.clone()))
        } else {
            self.resident.ride()
        }
    }

    fn delete(&mut self, _ctx: &mut ExecCtx<R, E>) {}

    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {}

    fn reset_replay(&mut self, _ctx: &mut ExecCtx<R, E>) {}

    fn refs(&self, _refs: &mut Refs) {}

    fn typ(&self) -> &Type {
        &self.typ
    }

    fn typecheck0(&mut self, _ctx: &mut ExecCtx<R, E>) -> Result<()> {
        Ok(())
    }

    fn typecheck1(&mut self, _ctx: &mut ExecCtx<R, E>) -> Result<()> {
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
    /// Scope at the block's declaration point: the containing scope for
    /// a module, the lexical scope for a `do` block.
    #[allow(dead_code)]
    pub(crate) scope: Scope,
}

impl<R: Rt, E: UserEvent> Block<R, E> {
    /// Build a `Block` from compiled children. A module produces no
    /// value; a `do` block's value is its last child's.
    pub fn new(
        module: bool,
        children: Box<[Node<R, E>]>,
        spec: Expr,
        scope: Scope,
    ) -> Node<R, E> {
        Node::new(Self {
            module,
            spec,
            children,
            catches: Box::default(),
            scope,
            resident: TagValue::phantom(),
        })
    }

    pub(crate) fn compile(
        ctx: &mut ExecCtx<R, E>,
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
            scope: scope.clone(),
            resident: TagValue::phantom(),
        }))
    }
}

/// Compile a statement list: each `catch(e) expr` child compiles
/// through [`error::Catch::compile`], which advances the dynamic scope
/// for all subsequent siblings. Returns the children in syntactic order
/// plus the catch indices.
pub(crate) fn compile_block_children<'a, R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<R, E>,
    flags: BitFlags<CFlag>,
    scope: &Scope,
    top_id: ExprId,
    module: bool,
    exprs: impl Iterator<Item = &'a Expr>,
) -> Result<(Box<[Node<R, E>]>, Box<[usize]>)> {
    let exprs: smallvec::SmallVec<[&'a Expr; 32]> = exprs.collect();
    // pre-register the block's `mod` names so resolution is independent
    // of declaration order; the `Module` arm removes its entry from
    // `predeclared_mods` instead of tripping the duplicate guard
    for e in exprs.iter() {
        if let ExprKind::Module { name, .. } = &e.kind {
            let p = ModPath(scope.lexical.append(name));
            if ctx.env.modules.contains(&p) {
                return Err(anyhow::anyhow!("duplicate module definition {p}")
                    .context(ErrorContext((*e).clone())));
            }
            ctx.predeclared_mods.insert(p.clone());
            ctx.env.modules.insert_cow(p);
        }
    }
    let mut scope = scope.clone();
    let mut children: LPooled<Vec<Node<R, E>>> = LPooled::take();
    let mut catches: LPooled<Vec<usize>> = LPooled::take();
    let n = exprs.len();
    for (i, e) in exprs.iter().copied().enumerate() {
        // `mod`/`use` are declarations: legal everywhere but a `do`
        // block's value slot, and compiled directly only here
        let value_position = !module && i + 1 == n;
        match &e.kind {
            ExprKind::Catch(c) => {
                let (node, advanced) =
                    error::Catch::compile(ctx, flags, e.clone(), &scope, top_id, c)?;
                scope = advanced;
                catches.push(i);
                children.push(node);
            }
            ExprKind::Use { reexport, names } if !value_position => children
                .push(compile_use(ctx, flags, e.clone(), &scope, *reexport, names)?),
            ExprKind::Module { name, value } if !value_position => children.push(
                compile_module(ctx, flags, e.clone(), &scope, top_id, name, value)?,
            ),
            ExprKind::TypeDef(td) if !value_position => children.push(TypeDef::compile(
                ctx,
                e.clone(),
                &scope,
                &td.name,
                &td.params,
                &td.body,
            )?),
            ExprKind::Trait(t) if !value_position => children
                .push(traits::Trait::compile(ctx, flags, e.clone(), &scope, t, top_id)?),
            ExprKind::Impl(im) if !value_position => children
                .push(traits::Impl::compile(ctx, flags, e.clone(), &scope, im, top_id)?),
            _ => children.push(compile(ctx, flags, e.clone(), &scope, top_id)?),
        }
    }
    Ok((Box::from_iter(children.drain(..)), Box::from_iter(catches.drain(..))))
}

impl<R: Rt, E: UserEvent> Block<R, E> {
    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<R, E>,
        buf: &mut &[u8],
    ) -> Result<Node<R, E>, PackError> {
        let module = bool::decode(buf)?;
        let spec = Expr::decode(buf)?;
        let children = decode_nodes(ctx, buf)?.into_boxed_slice();
        let catches = Vec::<usize>::decode(buf)?.into_boxed_slice();
        let scope = crate::image::scope_decode(buf)?;
        Ok(Node::new(Self {
            module,
            spec,
            children,
            catches,
            scope,
            resident: TagValue::phantom(),
        }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Block<R, E> {
    fn image_len(&self) -> usize {
        tag_len()
            + self.module.encoded_len()
            + self.spec.encoded_len()
            + nodes_len(&self.children)
            + self.catches.to_vec().encoded_len()
            + crate::image::scope_len(&self.scope)
    }

    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::Block, buf);
        self.module.encode(buf)?;
        self.spec.encode(buf)?;
        encode_nodes(&self.children, buf)?;
        self.catches.to_vec().encode(buf)?;
        crate::image::scope_encode(&self.scope, buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> &TagValue {
        if self.catches.is_empty() {
            let res = self
                .children
                .iter_mut()
                .fold(TagValue::phantom_ref(), |_, n| n.update(ctx, event));
            return if self.module { TagValue::phantom_ref() } else { res };
        }
        // covered children first, then catches innermost first; the
        // value is the last syntactic child's (absent if it is a catch)
        let last = self.children.len() - 1;
        let mut res: Option<TagValue> = None;
        let mut catch = self.catches.iter().copied().peekable();
        for (i, n) in self.children.iter_mut().enumerate() {
            if catch.peek() == Some(&i) {
                catch.next();
                continue;
            }
            let r = n.update(ctx, event);
            if i == last {
                res = Some(r.clone());
            }
        }
        for i in self.catches.iter().rev() {
            let _ = self.children[*i].update(ctx, event);
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

    fn reset_replay(&mut self, ctx: &mut ExecCtx<R, E>) {
        for n in &mut self.children {
            n.reset_replay(ctx)
        }
    }

    fn refs(&self, refs: &mut Refs) {
        for n in &self.children {
            n.refs(refs)
        }
    }

    fn typ(&self) -> &Type {
        &self.children.last().map(|n| n.typ()).unwrap_or(&Type::Bottom)
    }

    fn typecheck0(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        // catches typecheck after the covered children so a handler sees
        // the complete error-type accumulation
        let mut catch = self.catches.iter().copied().peekable();
        for (i, n) in self.children.iter_mut().enumerate() {
            if catch.peek() == Some(&i) {
                catch.next();
                continue;
            }
            if self.module {
                wrap!(n, n.typecheck0(ctx)).with_context(|| self.spec.ori.clone())?
            } else {
                wrap!(n, n.typecheck0(ctx))?
            }
        }
        for i in self.catches.iter().rev() {
            let n = &mut self.children[*i];
            if self.module {
                wrap!(n, n.typecheck0(ctx)).with_context(|| self.spec.ori.clone())?
            } else {
                wrap!(n, n.typecheck0(ctx))?
            }
        }
        Ok(())
    }

    fn typecheck1(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        let mut catch = self.catches.iter().copied().peekable();
        for (i, n) in self.children.iter_mut().enumerate() {
            if catch.peek() == Some(&i) {
                catch.next();
                continue;
            }
            if self.module {
                wrap!(n, n.typecheck1(ctx)).with_context(|| self.spec.ori.clone())?
            } else {
                wrap!(n, n.typecheck1(ctx))?
            }
            // a later statement's resolution reads settled facts
            wrap!(n, crate::drain_pending_settles(ctx))?;
        }
        for i in self.catches.iter().rev() {
            let n = &mut self.children[*i];
            if self.module {
                wrap!(n, n.typecheck1(ctx)).with_context(|| self.spec.ori.clone())?
            } else {
                wrap!(n, n.typecheck1(ctx))?
            }
            wrap!(n, crate::drain_pending_settles(ctx))?;
        }
        Ok(())
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

    fn fuse(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<Option<Node<R, E>>> {
        for child in self.children.iter_mut() {
            fuse(child, ctx)?;
        }
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
        ctx: &mut ExecCtx<R, E>,
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
    fn image_len(&self) -> usize {
        tag_len()
            + self.spec.encoded_len()
            + self.typ.encoded_len()
            + image::slice_len(&self.typs)
            + nodes_len(&self.args)
    }

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
        // an abstract part applies (`coretraits::with_value_hooks`)
        let woke = self.slept.take();
        let (args, typs, resident) = (&mut self.args, &self.typs, &mut self.resident);
        coretraits::with_value_hooks(ctx, event, |ctx, event| {
            let mut trig = false;
            let mut fired = false;
            let mut bottom = false;
            let mut vals: LPooled<Vec<Value>> = LPooled::take();
            for c in args.iter_mut() {
                let tv = c.update(ctx, event);
                let t = tv.tag();
                trig |= t.triggers();
                fired |= t.is_fired();
                if t.is_bottom() {
                    bottom = true
                } else if !bottom {
                    vals.push(tv.value_cloned())
                }
            }
            dense_gate!(resident, ctx, trig, bottom, woke);
            let tag = if fired { Tag::FIRED } else { Tag::STALE };
            let mut buf: LPooled<String> = LPooled::take();
            for (typ, v) in typs.iter().zip(vals.iter()) {
                match v {
                    Value::String(s) => write!(buf, "{s}"),
                    v => write!(buf, "{}", TVal { env: &ctx.env, typ, v }),
                }
                .unwrap()
            }
            resident.set(TagValue::tagged(Value::String(buf.as_str().into()), tag))
        })
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

    fn reset_replay(&mut self, ctx: &mut ExecCtx<R, E>) {
        for n in &mut self.args {
            n.reset_replay(ctx);
        }
    }

    fn typecheck0(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        for (i, a) in self.args.iter_mut().enumerate() {
            wrap!(a, a.typecheck0(ctx))?;
            self.typs[i] = part_type(a.typ());
        }
        Ok(())
    }

    fn typecheck1(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
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

    /// Build a `Connect` node from an already-compiled RHS expression
    /// and the BindId of the variable to be updated on each cycle.
    pub fn new(id: BindId, rhs: Node<R, E>, spec: Expr) -> Node<R, E> {
        Node::new(Self { spec, node: rhs, id })
    }

    pub(crate) fn compile(
        ctx: &mut ExecCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: Expr,
        scope: &Scope,
        top_id: ExprId,
        name: &ModPath,
        value: &Expr,
    ) -> Result<Node<R, E>> {
        let (id, def_pos, def_ori) = match ctx
            .env
            .lookup_bind(&scope.lexical, name)
            .map_err(|e| e.context(ErrorContext(spec.clone())))?
        {
            None => bailat!(spec, "{name} is undefined"),
            Some((_, b)) => (b.id, b.pos, b.ori.clone()),
        };
        // a `<-` target is never a static call target
        ctx.mark_connect_target(id);
        if ctx.env.lsp_mode {
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
    fn image_len(&self) -> usize {
        tag_len()
            + self.spec.encoded_len()
            + self.node.image_len()
            + self.id.encoded_len()
    }

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
        &Type::Bottom
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

    fn reset_replay(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.node.reset_replay(ctx);
    }

    fn typecheck0(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        wrap!(self.node, self.node.typecheck0(ctx))?;
        let bind = match ctx.env.by_id.get(&self.id) {
            None => bail!("BUG missing bind {:?}", self.id),
            Some(bind) => bind,
        };
        wrap!(self, bind.typ.check_contains(&ctx.env, self.node.typ()))
    }

    fn typecheck1(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
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

/// Where a write through a reference lands: a bound variable, or a
/// place inside one (`design/place_references.md`).
#[derive(Debug, Clone, PartialEq)]
pub(super) enum WriteTarget {
    Bind(BindId),
    Place(BindId, place::Path),
}

impl WriteTarget {
    fn image_len(target: &Option<Self>) -> usize {
        1 + match target {
            None => 0,
            Some(WriteTarget::Bind(id)) => id.encoded_len(),
            Some(WriteTarget::Place(id, path)) => {
                id.encoded_len() + place::path_len(path)
            }
        }
    }

    fn image_encode(target: &Option<Self>, buf: &mut ImageBuf) -> Result<(), PackError> {
        match target {
            None => Ok(buf.put_u8(0)),
            Some(WriteTarget::Bind(id)) => {
                buf.put_u8(1);
                id.encode(buf)
            }
            Some(WriteTarget::Place(id, path)) => {
                buf.put_u8(2);
                id.encode(buf)?;
                place::path_encode(path, buf)
            }
        }
    }

    fn image_decode(buf: &mut &[u8]) -> Result<Option<Self>, PackError> {
        if !buf.has_remaining() {
            return Err(PackError::BufferShort);
        }
        match buf.get_u8() {
            0 => Ok(None),
            1 => Ok(Some(WriteTarget::Bind(BindId::decode(buf)?))),
            2 => Ok(Some(WriteTarget::Place(
                BindId::decode(buf)?,
                place::path_decode(buf)?,
            ))),
            _ => Err(PackError::UnknownTag),
        }
    }
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
        let target = WriteTarget::image_decode(buf)?;
        let top_id = ExprId::decode(buf)?;
        ctx.rt.ref_var(src_id, top_id);
        Ok(Node::new(Self { spec, rhs, src_id, target, top_id }))
    }

    /// Build a `ConnectDeref` from an already-compiled RHS node and
    /// the source reference's BindId. The caller is responsible for
    /// registering the reference with the runtime (via
    /// `ctx.rt.ref_var(src_id, top_id)`).
    pub fn new(
        src_id: BindId,
        rhs: Node<R, E>,
        top_id: ExprId,
        spec: Expr,
    ) -> Node<R, E> {
        Node::new(Self { spec, rhs, src_id, target: None, top_id })
    }

    pub(crate) fn compile(
        ctx: &mut ExecCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: Expr,
        scope: &Scope,
        top_id: ExprId,
        name: &ModPath,
        value: &Expr,
    ) -> Result<Node<R, E>> {
        let (src_id, def_pos, def_ori) = match ctx
            .env
            .lookup_bind(&scope.lexical, name)
            .map_err(|e| {
            e.context(ErrorContext(spec.clone()))
        })? {
            None => bailat!(spec, "{name} is undefined"),
            Some((_, b)) => (b.id, b.pos, b.ori.clone()),
        };
        if ctx.env.lsp_mode {
            ctx.env.push_reference(ReferenceSite {
                pos: spec.pos,
                ori: spec.ori.clone(),
                name: name.clone(),
                bind_id: src_id,
                def_pos,
                def_ori,
            });
        }
        ctx.rt.ref_var(src_id, top_id);
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
    fn image_len(&self) -> usize {
        tag_len()
            + self.spec.encoded_len()
            + self.rhs.image_len()
            + self.src_id.encoded_len()
            + WriteTarget::image_len(&self.target)
            + self.top_id.encoded_len()
    }

    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::ConnectDeref, buf);
        self.spec.encode(buf)?;
        self.rhs.image_encode(buf)?;
        self.src_id.encode(buf)?;
        WriteTarget::image_encode(&self.target, buf)?;
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
            if let Some(t) = Self::resolve(ctx, tv) {
                if self.target.as_ref() != Some(&t) {
                    self.target = Some(t);
                    up = true;
                }
            }
        } else if self.target.is_none() {
            // an instance created after the reference value was delivered
            // finds it only in the standing store
            if let Some(read) = super::node::read_var(ctx, event, &self.src_id) {
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
        &Type::Bottom
    }

    fn refs(&self, refs: &mut Refs) {
        refs.refed.insert(self.src_id);
        self.rhs.refs(refs)
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        ctx.rt.unref_var(self.src_id, self.top_id);
        self.rhs.delete(ctx)
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.rhs.sleep(ctx);
    }

    fn reset_replay(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.rhs.reset_replay(ctx);
    }

    fn typecheck0(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        wrap!(self.rhs, self.rhs.typecheck0(ctx))?;
        let bind = match ctx.env.by_id.get(&self.src_id) {
            None => bail!("BUG missing bind {:?}", self.src_id),
            Some(bind) => bind,
        };
        let typ = Type::ByRef(Arc::new(self.rhs.typ().clone()));
        wrap!(self, bind.typ.check_contains(&ctx.env, &typ))
    }

    fn typecheck1(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        wrap!(self.rhs, self.rhs.typecheck1(ctx))?;
        Ok(())
    }

    fn view(&self) -> NodeView<'_, R, E> {
        NodeView::ConnectDeref(self)
    }
}

#[derive(Debug)]
pub struct TypeCast<R: Rt, E: UserEvent> {
    pub(crate) spec: Expr,
    pub typ: Type,
    pub target: Type,
    pub n: Node<R, E>,
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
        Ok(Node::new(Self { spec, typ, target, n, resident: TagValue::phantom() }))
    }

    pub(crate) fn compile(
        ctx: &mut ExecCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: Expr,
        scope: &Scope,
        top_id: ExprId,
        expr: &Expr,
        typ: &Type,
    ) -> Result<Node<R, E>> {
        let n = compile(ctx, flags, expr.clone(), scope, top_id)?;
        let target = typ.scope_refs(&scope.lexical);
        if let Err(e) = target.check_cast(&ctx.env) {
            bail!("in cast at {} {e}", spec.pos);
        }
        let typ = Type::union(&ctx.env, &[&target, &CAST_ERR])?;
        Ok(Node::new(Self { spec, typ, target, n, resident: TagValue::phantom() }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for TypeCast<R, E> {
    fn image_len(&self) -> usize {
        tag_len()
            + self.spec.encoded_len()
            + self.typ.encoded_len()
            + self.target.encoded_len()
            + self.n.image_len()
    }

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
        if tag.is_bottom() {
            self.resident.set(TagValue::tagged(Value::Null, tag))
        } else {
            let v = tv.value_cloned();
            self.resident.set(TagValue::tagged(self.target.cast_value(&ctx.env, v), tag))
        }
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
        self.n.sleep(ctx);
    }

    fn reset_replay(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.n.reset_replay(ctx);
    }

    fn refs(&self, refs: &mut Refs) {
        self.n.refs(refs)
    }

    fn typecheck0(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        Ok(wrap!(self.n, self.n.typecheck0(ctx))?)
    }

    fn typecheck1(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        wrap!(self.n, self.n.typecheck1(ctx))?;
        Ok(())
    }

    fn view(&self) -> NodeView<'_, R, E> {
        NodeView::TypeCast(self)
    }

    fn emit_clif(&self, cx: &mut BodyCx) -> Result<CompiledExpr> {
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
    resident: TagValue,
}

impl<R: Rt, E: UserEvent> Never<R, E> {
    pub(crate) fn compile(
        ctx: &mut ExecCtx<R, E>,
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
        Ok(Node::new(Self { spec, typ, n, resident: TagValue::phantom() }))
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
        Ok(Node::new(Self { spec, typ, n, resident: TagValue::phantom() }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Never<R, E> {
    fn image_len(&self) -> usize {
        tag_len() + self.spec.encoded_len() + self.typ.encoded_len() + nodes_len(&self.n)
    }

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
        self.resident.ride()
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

    fn reset_replay(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.n.iter_mut().for_each(|n| n.reset_replay(ctx))
    }

    fn refs(&self, refs: &mut Refs) {
        self.n.iter().for_each(|n| n.refs(refs))
    }

    fn typecheck0(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        for n in self.n.iter_mut() {
            wrap!(n, n.typecheck0(ctx))?
        }
        Ok(())
    }

    fn typecheck1(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
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
        ctx: &mut ExecCtx<R, E>,
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
    fn image_len(&self) -> usize {
        tag_len() + self.spec.encoded_len() + self.typ.encoded_len() + nodes_len(&self.n)
    }

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
            None if bottomed => {
                self.resident.set(TagValue::tagged(Value::Null, Tag::FRESH_BOTTOM))
            }
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

    fn reset_replay(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.n.iter_mut().for_each(|n| n.reset_replay(ctx))
    }

    fn refs(&self, refs: &mut Refs) {
        self.n.iter().for_each(|n| n.refs(refs))
    }

    fn typecheck0(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        for n in self.n.iter_mut() {
            wrap!(n, n.typecheck0(ctx))?
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

    fn typecheck1(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        for n in self.n.iter_mut() {
            wrap!(n, n.typecheck1(ctx))?
        }
        Ok(())
    }

    fn view(&self) -> NodeView<'_, R, E> {
        NodeView::Any(self)
    }
}

#[derive(Debug)]
pub struct Sample<R: Rt, E: UserEvent> {
    pub(crate) spec: Expr,
    /// `~!`: a trigger that finds the RHS bottom produces bottom and
    /// banks nothing; `~` banks it and pays at the RHS's first value.
    strict: bool,
    triggered: usize,
    pub typ: Type,
    id: BindId,
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
        let strict = bool::decode(buf)?;
        let typ = Type::decode(buf)?;
        let id = BindId::decode(buf)?;
        let top_id = ExprId::decode(buf)?;
        let trigger = decode_node(ctx, buf)?;
        let arg = Held::image_decode(ctx, buf)?;
        ctx.rt.ref_var(id, top_id);
        Ok(Node::new(Self {
            spec,
            strict,
            triggered: 0,
            typ,
            id,
            top_id,
            trigger,
            arg,
            resident: TagValue::phantom(),
        }))
    }

    pub(crate) fn compile(
        ctx: &mut ExecCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: Expr,
        scope: &Scope,
        top_id: ExprId,
        lhs: &Arc<Expr>,
        rhs: &Arc<Expr>,
        strict: bool,
    ) -> Result<Node<R, E>> {
        let id = BindId::new();
        ctx.rt.ref_var(id, top_id);
        let trigger = compile(ctx, flags, (**lhs).clone(), scope, top_id)?;
        let arg = Held::new(compile(ctx, flags, (**rhs).clone(), scope, top_id)?);
        let typ = arg.node.typ().clone();
        Ok(Node::new(Self {
            strict,
            triggered: 0,
            id,
            top_id,
            spec,
            typ,
            trigger,
            arg,
            resident: TagValue::phantom(),
        }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Sample<R, E> {
    fn image_len(&self) -> usize {
        tag_len()
            + self.spec.encoded_len()
            + self.strict.encoded_len()
            + self.typ.encoded_len()
            + self.id.encoded_len()
            + self.top_id.encoded_len()
            + self.trigger.image_len()
            + self.arg.image_len()
    }

    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::Sample, buf);
        self.spec.encode(buf)?;
        self.strict.encode(buf)?;
        self.typ.encode(buf)?;
        self.id.encode(buf)?;
        self.top_id.encode(buf)?;
        self.trigger.image_encode(buf)?;
        self.arg.image_encode(buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> &TagValue {
        // only a fired trigger samples or banks debt
        let t = self.trigger.update(ctx, event);
        let fired = t.tag().is_fired();
        self.arg.update(ctx, event);
        if self.strict {
            return match (fired, self.arg.value.as_ref(), self.arg.tag.is_bottom()) {
                (true, Some(v), false) => self.resident.set(TagValue::fired(v.clone())),
                (true, _, _) => {
                    self.resident.set(TagValue::tagged(Value::Null, Tag::FRESH_BOTTOM))
                }
                (false, _, _) => self.resident.ride(),
            };
        }
        if fired {
            self.triggered += 1;
        }
        let var = event.variables.get(&self.id).cloned();
        let held = || match &self.arg.value {
            Some(_) if self.arg.tag.is_bottom() => {
                TagValue::tagged(Value::Null, Tag::FRESH_BOTTOM)
            }
            Some(v) => TagValue::fired(v.clone()),
            None => unreachable!(),
        };
        let res = if self.triggered > 0 && self.arg.value.is_some() && var.is_none() {
            self.triggered -= 1;
            Some(held())
        } else {
            var
        };
        if self.arg.value.is_some() && !self.arg.tag.is_bottom() {
            while self.triggered > 0 {
                self.triggered -= 1;
                ctx.rt.set_var(self.id, self.arg.value.clone().unwrap());
            }
        }
        match res {
            Some(tv) => self.resident.set(tv),
            None => self.resident.ride(),
        }
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        ctx.rt.unref_var(self.id, self.top_id);
        self.arg.node.delete(ctx);
        self.trigger.delete(ctx);
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.arg.sleep(ctx);
        self.trigger.sleep(ctx);
    }

    fn reset_replay(&mut self, ctx: &mut ExecCtx<R, E>) {
        // the held RHS is this node's contract and survives a frame reset
        self.arg.node.reset_replay(ctx);
        self.trigger.reset_replay(ctx);
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn typ(&self) -> &Type {
        &self.typ
    }

    fn refs(&self, refs: &mut Refs) {
        refs.refed.insert(self.id);
        self.arg.node.refs(refs);
        self.trigger.refs(refs);
    }

    fn typecheck0(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        wrap!(self.trigger, self.trigger.typecheck0(ctx))?;
        wrap!(self.arg.node, self.arg.node.typecheck0(ctx))?;
        // the child may replace its typ during typecheck0
        self.typ = self.arg.node.typ().clone();
        Ok(())
    }

    fn typecheck1(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        wrap!(self.trigger, self.trigger.typecheck1(ctx))?;
        wrap!(self.arg.node, self.arg.node.typecheck1(ctx))?;
        Ok(())
    }

    fn view(&self) -> NodeView<'_, R, E> {
        NodeView::Sample(self)
    }
}
