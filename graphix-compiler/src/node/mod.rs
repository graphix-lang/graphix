use crate::{
    BindId, CAST_ERR, CFlag, Event, ExecCtx, Node, NodeView, PendingImport, Refs, Rt,
    Scope, Tag, TagValue, Update, UserEvent,
    env::{Env, ImportEntry},
    expr::{ErrorContext, Expr, ExprId, ExprKind, ModPath, TypeDefBody},
    fusion::{
        emit::{
            BodyCx, CompiledExpr, emit_block_node, emit_cast_node, emit_connect_node,
            emit_const_node, emit_string_interpolate_node,
        },
        fuse,
    },
    ide::{ModuleRefSite, ReferenceSite},
    typ::{TVal, TVar, Type},
};
use anyhow::{Context, Result, bail};
use arcstr::{ArcStr, literal};
use compiler::compile;
use enumflags2::BitFlags;
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
pub(crate) mod data;
pub(crate) mod error;
pub mod genn;
pub mod lambda;
pub(crate) mod map;
pub(crate) mod module;
pub(crate) mod op;
pub(crate) mod pattern;
pub(crate) mod select;
pub mod traits;

/// A variable read's provenance under dense delivery — see [`read_var`].
pub(crate) enum VarRead<'a> {
    /// Found in an overlay (this cycle's transient deliveries, or a
    /// frame's private writes) or store-stamped THIS cycle: the
    /// entry's own tag applies.
    Delivered(&'a TagValue),
    /// A standing store entry from an earlier cycle: the value
    /// channel. Readers view it Stale — or Fired under an init view
    /// (R2), which is the whole of init backfilling.
    Standing(&'a TagValue),
}

/// THE variable read seam (design/dense_delivery.md R2/R3): innermost
/// overlay, then the enclosing frame stack (an inner dispatch's
/// captures live in its caller's frame), then the persistent store
/// with the cycle-stamp rule. `None` is the phantom — the bind has
/// never delivered.
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
/// expression's `Origin` and `SourcePosition`. The LSP recovers both by
/// downcasting `ErrorContext` out of the anyhow chain — no message-string
/// scraping. Use this instead of `bail!("at {} …", spec.pos)` in compile
/// paths where the spec `Expr` is in scope.
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
            // alias chains are follow-your-nose; a bound this deep is a
            // cyclic typedef, not a real program
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
                    // A Set whose members have since become mergeable (a
                    // union built while a member type still held unbound
                    // TVars — e.g. a select's arm union over a `$` result —
                    // never re-collapses on its own). normalize's merge
                    // sees through bound TVars; if it collapses the set,
                    // keep dereferencing the merged type.
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

#[derive(Debug)]
pub struct Nop {
    pub typ: Type,
}

impl Nop {
    pub(crate) fn new<R: Rt, E: UserEvent>(typ: Type) -> Node<R, E> {
        Node::new(Nop { typ })
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Nop {
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
    pub(crate) fn compile(
        ctx: &mut ExecCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: Expr,
        inner: Expr,
        scope: &Scope,
        top_id: ExprId,
    ) -> Result<Node<R, E>> {
        // `spec` is the OUTER parens expression — it carries the position
        // and any `#[..]` decorations, so the node (and a kernel that
        // replaces it) must own it; the INNER expression compiles the
        // value. Storing the inner as the spec orphaned decorations on
        // parenthesized expressions (no node carried them — `#[native]
        // (…)` was silently unchecked until the census made it loud).
        let n = compile(ctx, flags, inner, scope, top_id)?;
        Ok(Node::new(ExplicitParens { spec, n }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for ExplicitParens<R, E> {
    fn fuse(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<Option<Node<R, E>>> {
        // A parenthesized expression is a fusion boundary the user drew
        // deliberately — give the interior its own region pass (this is
        // what lets `clock ~ (a + b)` fuse the `a + b` without the old
        // hoist-into-a-let workaround, wherever the parens are reachable)
        // and routes attribute dispatch to a decorated interior.
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

/// DESIGNATED ride memory: a child `Node` plus its last VALUE, kept
/// under a poisoned tag, for the three readers whose semantics
/// require reading history a production cannot carry (the P5b′
/// designation pass, design/dense_delivery.md):
///
/// - the select SCRUTINEE (the scrutinee ride, aug06ghz0: a bottomed
///   scrutinee with history rides — binds and re-matches read the
///   held value),
/// - a pattern GUARD's truth (`is_match` takes `&self` and cannot
///   consume the guard's production — the held truth is the only
///   channel),
/// - `~`'s held arg ("sample the latest" IS the contract).
///
/// Everything else reads its children's dense productions directly —
/// the operand-cache role this type had under sparse delivery is
/// gone (each cache slot mirrored the child's resident).
#[derive(Debug)]
pub struct Held<R: Rt, E: UserEvent> {
    /// The last value-bearing production (`Some` = there was once a
    /// real value; a bottom's placeholder never lands here).
    pub value: Option<Value>,
    /// The tag of the child's LAST production. Only the TAINT bit is
    /// meaningful at rest (a standing bottom keeps poisoning until a
    /// value overwrites it — the kernel's slot-disc twin); firedness
    /// is a property of a PRODUCTION, not of held memory.
    pub tag: Tag,
    pub node: Node<R, E>,
    /// Lazily computed: the subtree references no bindings at all, so
    /// its value is identical in every evaluation frame — see
    /// [`Self::reset_replay`].
    invariant: std::sync::OnceLock<bool>,
}

impl<R: Rt, E: UserEvent> Held<R, E> {
    pub fn new(node: Node<R, E>) -> Self {
        Self { value: None, tag: Tag::FIRED, node, invariant: std::sync::OnceLock::new() }
    }

    /// Update the node, returning the production's tag. A
    /// value-bearing production lands in `value`/`tag`; a BOTTOM
    /// production poisons the tag but never overwrites the value —
    /// the ride memory.
    pub fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> Tag {
        let tv = self.node.update(ctx, event);
        let tag = tv.tag();
        if !tag.is_bottom() {
            self.value = Some(tv.value_cloned());
        }
        self.tag = tag;
        tag
    }

    /// [`Self::update`], reduced to "should this production trigger
    /// my evaluation" — true for fired AND fresh-bottom productions
    /// (a bottom event must ride toward a force point), false for the
    /// stale states. The consumed bottomness is read back off
    /// [`Self::tag`].
    pub fn update_triggers(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        event: &mut Event<E>,
    ) -> bool {
        self.update(ctx, event).triggers()
    }

    /// Sleep is PAUSE, not reset: the held value (and its at-rest
    /// taint) survives, so a re-woken subtree resumes from its history
    /// exactly like the kernel's replay/ride words (Eric's ruling
    /// 2026-07-31, select_reselect_interior_bottom). Contrast
    /// [`Self::reset_replay`], where frame state never survives.
    pub fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.node.sleep(ctx)
    }

    /// The held value is replay memory — EXCEPT when the subtree is a
    /// closed expression (references no bindings): such a value is
    /// identical in every frame and the subtree cannot re-produce it
    /// without an init view, so the hold IS the value channel — the
    /// interpreter's twin of the kernel's constant immediates.
    /// Crucially it stays a hold, not a firing: a body that consumes
    /// only constants stays quiet after its first-ever evaluation,
    /// which is what keeps const-callback folds quiet in both
    /// backends (the hof-lift-firing pin).
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

/// Gather a composite's element productions (dense delivery): update
/// every child, join the tags, and clone the values. Returns
/// `(trig, fired, bottom)`; `vals` receives every element value in
/// order and is meaningful only when `bottom` is false (the gather
/// stops pushing once a bottom is seen — the values are abandoned).
/// The per-element clone is the same clone the deleted operand-cache
/// fill paid per delivery.
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

/// The dense recompute gate + bottom join, shared by every node that
/// computes from its children's productions. Skip (ride) unless a
/// consumed production triggered, the resident is a bottom needing a
/// value-channel refill, or we're inside an evaluation frame (R1:
/// framed passes recompute unconditionally — exactly the kernel). Any
/// consumed bottom bottoms the result: FreshBottom iff a delivery
/// triggered this cycle, else the quiet ride (the join rule —
/// standing bottoms never re-mint events).
macro_rules! dense_gate {
    ($self:ident, $ctx:ident, $trig:expr, $bottom:expr) => {{
        if !($trig || $self.resident.tag().is_bottom() || $ctx.frame_depth > 0) {
            return $self.resident.ride();
        }
        if $bottom {
            return if $trig {
                $self
                    .resident
                    .set($crate::TagValue::tagged(Value::Null, $crate::Tag::FRESH_BOTTOM))
            } else {
                $self.resident.ride()
            };
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
/// ([`crate::env::Env::names`]): resolve its module prefix
/// (keyword-anchored or package/chain-rooted), then install a glob
/// source or an explicit [`ImportEntry`]. A `use` is a compile-time
/// declaration — the statement compiles to [`Nop`]; nothing lives in
/// the graph. See design/module_system.md.
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
    // The module context the terminal name lives in: a bare keyword
    // anchor resolves along its lexical chain (a `super` anchor may
    // be a block level — a script file's top level), everything else
    // is a canonical module.
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
            chain: true,
            pos,
            ori: ori.clone(),
        },
        Some(Anchor::Module(m)) => ImportEntry {
            scope: m,
            name: base.into(),
            chain: false,
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
                    chain: false,
                    pos,
                    ori: ori.clone(),
                },
                None => bail!("use: no module `{base}` in scope"),
            }
        }
    };
    // the package prelude already provides every package name as a
    // path root, so importing a package under its own name is a
    // no-op (a DIFFERENT target under a package's name shadows the
    // prelude by precedence, like every other explicit entry)
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

impl<R: Rt, E: UserEvent> Update<R, E> for TypeDef {
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
    /// Construct a `Constant` node from its final components. AOT-
    /// generated code uses this after it has already chosen the
    /// value, type, and spec at code-generation time.
    pub fn new<R: Rt, E: UserEvent>(value: Value, typ: Type, spec: Expr) -> Node<R, E> {
        // a constant IS its value from birth: the resident starts on
        // the value channel (Stale), so a fresh instance bound without
        // an init view still computes — firing stays init-gated
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

impl<R: Rt, E: UserEvent> Update<R, E> for Constant {
    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> &TagValue {
        // FRAME DEPTH FIRST: frames force `event.init`, so checking
        // init before the frame gate made constants produce FIRED on
        // every framed evaluation — the frame-stale arm was
        // unreachable exactly where it was written for. Latent while
        // the select/lambda result derivations were trigger-coarse;
        // surfaced by the organic-tag algebra (replay-frames v3 —
        // gtailr epoch 2: a quiet-selection cycle fired because the
        // inner select's CONST scrutinee read as fired-under-forced-
        // init). A genuine init is always frame depth 0.
        if ctx.frame_depth > 0 {
            // In-frame VALUE channel: the kernel recomputes every
            // constant per invocation with a `const_stale_gate`d disc
            // — FIRED iff the dispatch itself was a genuine init
            // (`ctx.frame_init`, the invocation-uniform kernel
            // `init_flag`), else the STALE value channel, so a body
            // that reads only constants computes (quietly) instead of
            // bottoming when the frame discipline has cleared its
            // consumers' operand caches.
            // ORDINARY framed passes are per-invocation re-derivation:
            // constants stay on the value channel (the gtailr epoch-2
            // gate). A select arm's wake does NOT change that — firing
            // a guard's constant under the wake manufactures freshness
            // and re-emits provably-unchanged outputs
            // (`findings/tail-jump-honest-tags-jul2026/00`). What a
            // fresh subtree needs is a VALUE, not a fire, and that is
            // the producers' own first-production rule (Bind, and the
            // builtin arg seam).
            if ctx.frame_init {
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

// used for both mod and do
#[derive(Debug)]
pub struct Block<R: Rt, E: UserEvent> {
    pub(crate) module: bool,
    pub(crate) spec: Expr,
    pub(crate) children: Box<[Node<R, E>]>,
    /// Indices of `catch(e) expr` children, in syntactic order.
    /// Children stay physically in syntactic order (typ() and the
    /// tail-leaf walk read children.last()), but update/typecheck run
    /// the two-phase order: non-catch children first (the covered
    /// region), then catches in REVERSE syntactic order — the
    /// nested-try equivalence runs inner handlers first, and an inner
    /// handler's rethrow delivers to its predecessor by same-cycle
    /// Vacant-insert, which the predecessor only sees if it updates
    /// after (forward order would silently LOSE the rethrown error).
    pub(crate) catches: Box<[usize]>,
    /// Production slot for the catch-bearing path only: the last
    /// covered child's borrow can't be held across the catches pass
    /// (it re-borrows `children`), so its production is cloned here.
    /// The catch-free path forwards the child's borrow directly.
    resident: TagValue,
    /// Module scope at the block's declaration point. For
    /// `Block { module: true }` this is the *containing* scope —
    /// the inner module's scope is `scope.append(name)`. For
    /// `Block { module: false }` (Do block at expression position)
    /// this is the lexical scope the Do is in.
    #[allow(dead_code)]
    pub(crate) scope: Scope,
}

impl<R: Rt, E: UserEvent> Block<R, E> {
    /// Build a `Block` / `do` node from an already-compiled list of
    /// child expressions. `module` selects "module" semantics (no
    /// returned value) vs "do" semantics (the last child's value is
    /// the block's value).
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
            compile_block_children(ctx, flags, scope, top_id, exprs.iter())?;
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

/// Compile a statement list with catch-statement support: each
/// `catch(e) expr` child compiles through [`error::Catch::compile`],
/// which registers its handler and ADVANCES the dynamic scope for all
/// subsequent siblings (the implicit nested scope — coverage by path
/// depth, exactly the old nested-try discipline). Returns the children
/// in syntactic order plus the catch indices. Shared by
/// [`Block::compile`] and the sig-bearing module body compile.
pub(crate) fn compile_block_children<'a, R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<R, E>,
    flags: BitFlags<CFlag>,
    scope: &Scope,
    top_id: ExprId,
    exprs: impl Iterator<Item = &'a Expr>,
) -> Result<(Box<[Node<R, E>]>, Box<[usize]>)> {
    let exprs: smallvec::SmallVec<[&'a Expr; 32]> = exprs.collect();
    // Headers pass: pre-register the block's `mod` NAMES so name
    // resolution (imports, sibling references, the mid-compile
    // resolution horizon) is independent of declaration order. The
    // `Module` compile arm removes its entry from `predeclared_mods`
    // instead of tripping the duplicate-module guard on it.
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
    for (i, e) in exprs.iter().copied().enumerate() {
        match &e.kind {
            ExprKind::Catch(c) => {
                let (node, advanced) =
                    error::Catch::compile(ctx, flags, e.clone(), &scope, top_id, c)?;
                scope = advanced;
                catches.push(i);
                children.push(node);
            }
            _ => children.push(compile(ctx, flags, e.clone(), &scope, top_id)?),
        }
    }
    Ok((Box::from_iter(children.drain(..)), Box::from_iter(catches.drain(..))))
}

impl<R: Rt, E: UserEvent> Update<R, E> for Block<R, E> {
    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> &TagValue {
        if self.catches.is_empty() {
            let res = self
                .children
                .iter_mut()
                .fold(TagValue::phantom_ref(), |_, n| n.update(ctx, event));
            return if self.module { TagValue::phantom_ref() } else { res };
        }
        // Two-phase order (see `catches`): covered children first —
        // the block's value is the last SYNTACTIC child's production
        // (absent if that child is a catch: an installation never
        // produces) — then catches, innermost first.
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
        // Catches typecheck AFTER the covered children in each pass
        // (innermost first): a handler's check must see the complete
        // error-type accumulation — Qop tc0 unions and callsite
        // throws-unions from every covered sibling, including inner
        // handlers' rethrow contributions.
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
        }
        for i in self.catches.iter().rev() {
            let n = &mut self.children[*i];
            if self.module {
                wrap!(n, n.typecheck1(ctx)).with_context(|| self.spec.ori.clone())?
            } else {
                wrap!(n, n.typecheck1(ctx))?
            }
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
            // A signature-less `mod` statement compiles to
            // `Block { module: true }` but is still a MODULE: its binds
            // publish into the persistent env for readers outside any
            // region. Swallowed into a parent region they become SSA
            // locals popped at the statement's end, and the exported
            // stream dies or loses its seed
            // (modstmt-fused-no-publish-aug2026). Refuse; the parent
            // de-fuses and `fuse` recurses per child — the same
            // structure `Module::fuse` gives sig-bearing modules.
            bail!("emit_clif: module statement is structure, not computation")
        }
        emit_block_node(cx, &self.children)
    }

    fn fuse(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<Option<Node<R, E>>> {
        // Statement spine: each child fuses its own maximal subtree
        // (or recurses further). The Block itself only fuses when a
        // PARENT's try_fuse succeeds on a region containing it (via
        // emit_clif above) — module-level blocks never do (their Bind
        // children must stay live to publish), and that is structural:
        // emit_block_node has no publish, so a region containing a
        // bind only ever covers block-scoped lets.
        for child in self.children.iter_mut() {
            fuse(child, ctx)?;
        }
        Ok(None)
    }
}

#[derive(Debug)]
pub struct StringInterpolate<R: Rt, E: UserEvent> {
    pub(crate) spec: Expr,
    pub typ: Type,
    pub(crate) typs: Box<[Type]>,
    pub args: Box<[Node<R, E>]>,
    resident: TagValue,
}

impl<R: Rt, E: UserEvent> StringInterpolate<R, E> {
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
        Ok(Node::new(Self { spec, typ, typs, args, resident: TagValue::phantom() }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for StringInterpolate<R, E> {
    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> &TagValue {
        use std::fmt::Write;
        let mut trig = false;
        let mut fired = false;
        let mut bottom = false;
        let mut vals: LPooled<Vec<Value>> = LPooled::take();
        for c in self.args.iter_mut() {
            let tv = c.update(ctx, event);
            let t = tv.tag();
            trig |= t.triggers();
            fired |= t.is_fired();
            if t.is_bottom() {
                bottom = true
            } else if !bottom {
                // gathered by clone within the iteration — the same
                // clone the old cache fill paid per delivery
                vals.push(tv.value_cloned())
            }
        }
        if !(trig || self.resident.tag().is_bottom() || ctx.frame_depth > 0) {
            return self.resident.ride();
        }
        if bottom {
            return if trig {
                self.resident.set(TagValue::tagged(Value::Null, Tag::FRESH_BOTTOM))
            } else {
                self.resident.ride()
            };
        }
        let tag = if fired { Tag::FIRED } else { Tag::STALE };
        let mut buf: LPooled<String> = LPooled::take();
        for (typ, v) in self.typs.iter().zip(vals.iter()) {
            match v {
                Value::String(s) => write!(buf, "{s}"),
                v => write!(buf, "{}", TVal { env: &ctx.env, typ, v }),
            }
            .unwrap()
        }
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
            self.typs[i] = a.typ().with_deref(|t| match t {
                None => Type::Any,
                Some(t) => t.clone(),
            });
        }
        Ok(())
    }

    fn typecheck1(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        for a in &mut self.args {
            wrap!(a, a.typecheck1(ctx))?;
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

#[derive(Debug)]
pub struct Connect<R: Rt, E: UserEvent> {
    pub(crate) spec: Expr,
    pub(super) node: Node<R, E>,
    pub(super) id: BindId,
}

impl<R: Rt, E: UserEvent> Connect<R, E> {
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
        // Record `id` as a `<-` target so downstream fusion call-site
        // lowering (`emit_known_fused_call`, `resolve_binding_fn_input`,
        // etc.) can refuse to register it as a static call target. Keyed
        // by BindId so an inner shadow of a Connect-target name stays
        // stable (only the specific BindId being written is unstable).
        ctx.unstable_bindings.insert(id);
        ctx.connect_targets.insert(id);
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
    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> &TagValue {
        // A variable write requires a FIRED RHS — the interp twin of
        // the kernel's `set_var_typed` gate (a stale or tainted RHS
        // must not become a cross-cycle event).
        let tv = self.node.update(ctx, event);
        if tv.is_fired() {
            let v = tv.value_cloned();
            ctx.rt.set_var(self.id, v)
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

    fn emit_clif(&self, cx: &mut BodyCx) -> Result<CompiledExpr> {
        emit_connect_node(cx, &self.node, self.id)
    }
}

#[derive(Debug)]
pub struct ConnectDeref<R: Rt, E: UserEvent> {
    pub(crate) spec: Expr,
    pub(super) rhs: Node<R, E>,
    pub(super) src_id: BindId,
    pub(super) target_id: Option<BindId>,
    pub(super) top_id: ExprId,
}

impl<R: Rt, E: UserEvent> ConnectDeref<R, E> {
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
        Node::new(Self { spec, rhs, src_id, target_id: None, top_id })
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
        Ok(Node::new(Self { spec, rhs, src_id, target_id: None, top_id }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for ConnectDeref<R, E> {
    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> &TagValue {
        // Fired-RHS write gate, as in `Connect`. A retarget (the ref
        // value switching binds) forces a write of the RHS's current
        // value — the dense production carries it on the value channel
        // whether or not the RHS fired this cycle; a bottom RHS never
        // writes.
        let (rhs_fired, rhs_val) = {
            let tv = self.rhs.update(ctx, event);
            let t = tv.tag();
            (t.is_fired(), if t.is_bottom() { None } else { Some(tv.value_cloned()) })
        };
        let mut up = rhs_fired;
        let as_bind = |tv: &TagValue| {
            tv.with_value(|v| match v {
                Value::U64(id) => Some(BindId::from(*id)),
                _ => None,
            })
        };
        if let Some(tv) = event.variables.get(&self.src_id) {
            if let Some(id) = as_bind(tv) {
                if let Some(target_id) = ctx.env.byref_chain.get(&id) {
                    self.target_id = Some(*target_id);
                    up = true;
                }
            }
        } else if self.target_id.is_none() {
            // A lazily-created instance (a runtime callable's first
            // dispatch) inits on a cycle AFTER the reference value was
            // delivered — the standing store is the only place it
            // still lives. Resolving from it is the write-side twin of
            // Deref's standing read; acquiring a target counts as the
            // retarget that forces a write.
            if let Some(read) = super::node::read_var(ctx, event, &self.src_id) {
                let tv = match read {
                    VarRead::Delivered(tv) | VarRead::Standing(tv) => tv,
                };
                if let Some(id) = as_bind(tv) {
                    if let Some(target_id) = ctx.env.byref_chain.get(&id) {
                        self.target_id = Some(*target_id);
                        up = true;
                    }
                }
            }
        }
        if up {
            if let (Some(v), Some(id)) = (rhs_val, self.target_id) {
                ctx.rt.set_var(id, v);
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
        let typ = target.union(&ctx.env, &CAST_ERR)?;
        Ok(Node::new(Self { spec, typ, target, n, resident: TagValue::phantom() }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for TypeCast<R, E> {
    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> &TagValue {
        let tv = self.n.update(ctx, event);
        let tag = tv.tag();
        if tag.is_tainted() {
            // never cast a taint placeholder — pass the bottom on with
            // the production's own freshness (the join rule: a standing
            // bottom rides, a triggering one is an event)
            if tag.triggers() {
                self.resident.set(TagValue::tagged(Value::Null, Tag::FRESH_BOTTOM))
            } else {
                self.resident.ride()
            }
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

#[derive(Debug)]
pub struct Any<R: Rt, E: UserEvent> {
    pub(crate) spec: Expr,
    pub typ: Type,
    pub n: Box<[Node<R, E>]>,
    resident: TagValue,
}

impl<R: Rt, E: UserEvent> Any<R, E> {
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
    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> &TagValue {
        // Dense restatement: the first triggering VALUE-BEARING
        // production wins — a stale delivery is every child's value
        // channel and never beats it, and a triggering BOTTOM never
        // beats a value-bearing alternative (`any(risky?, default)` is
        // the fallback idiom: the handled error's fresh bottom must
        // not eat the default). A cycle whose only events are bottoms
        // produces a fresh bottom; a quiet cycle rides Any's OWN last
        // winner.
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
        let rtyp = Type::Bottom;
        let rtyp = wrap!(
            self,
            self.n.iter().fold(Ok(rtyp), |rtype, n| rtype?.union(&ctx.env, n.typ()))
        )?;
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
    triggered: usize,
    pub typ: Type,
    id: BindId,
    top_id: ExprId,
    pub trigger: Node<R, E>,
    pub arg: Held<R, E>,
    resident: TagValue,
}

impl<R: Rt, E: UserEvent> Sample<R, E> {
    pub(crate) fn compile(
        ctx: &mut ExecCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: Expr,
        scope: &Scope,
        top_id: ExprId,
        lhs: &Arc<Expr>,
        rhs: &Arc<Expr>,
    ) -> Result<Node<R, E>> {
        let id = BindId::new();
        ctx.rt.ref_var(id, top_id);
        let trigger = compile(ctx, flags, (**lhs).clone(), scope, top_id)?;
        let arg = Held::new(compile(ctx, flags, (**rhs).clone(), scope, top_id)?);
        let typ = arg.node.typ().clone();
        Ok(Node::new(Self {
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
    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> &TagValue {
        // Debt on FIRED only (ruled delta 8, the jul23e protection
        // relocated from the dispatch-exit seam): a stale refresh must
        // not sample, and neither may a bottoming trigger — a `~` fed
        // by a bottoming recursive callee holds its debt until the
        // trigger recovers, exactly the kernel.
        let t = self.trigger.update(ctx, event);
        if t.tag().is_fired() {
            self.triggered += 1;
        }
        self.arg.update(ctx, event);
        let var = event.variables.get(&self.id).cloned();
        let held = || match &self.arg.value {
            Some(_) if self.arg.tag.is_tainted() => {
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
        if self.arg.value.is_some() && !self.arg.tag.is_tainted() {
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
        // `arg.value` (the held RHS) is SEMANTIC — "sample the latest
        // value when the trigger fires" IS this node's contract, so the
        // held value survives a frame reset. Children still reset.
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
        // Re-read the RHS type: the compile-time snapshot is ORPHANED
        // when the child REPLACES its typ field during typecheck0 (a
        // select sets `self.typ = rtype` — the finding-37 orphan class).
        // The stale snapshot was the select's pre-typecheck EMPTY
        // primitive set, which every type contains, so `st <- in0 ~
        // select {...}` passed the connect containment vacuously and a
        // mistyped struct flowed at runtime (soak-jul14b 000005).
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
