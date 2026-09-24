use super::{
    NOP, Nop, WakeBit,
    bind::Ref,
    compiler::compile,
    error::{Qop, join_raised},
    lambda::{BuiltInLambda, GXLambda, LambdaDef, build_builtin_check, same_parameters},
    pattern::StructPatternNode,
    read_quiet,
};
use crate::{
    Apply, ApplyView, BindId, BindMode, CFlag, Event, ExecCtx, FnArgIdentity, LambdaId,
    LambdaInstanceId, Node, NodeView, PendingTailCall, Refs, ResolvingLambda, Rt, Scope,
    Tag, TagValue, Update, UserEvent, analysis, bailat, dbgenv, deref_typ,
    env::Env,
    expr::{ApplyExpr, At, Expr, ExprId, ExprKind},
    fusion::{
        self,
        emit::{BodyCx, CompiledExpr, emit_builtin_call_node, emit_lambda_call_node},
        lowering::MarshalArg,
    },
    image::{
        self, ImageBuf,
        nodes::{
            NodeTag, decode_node, decode_nodes, encode_nodes, nodes_len, opt_node_decode,
            opt_node_encode, opt_node_len, put_tag, tag_len,
        },
    },
    perfdbg,
    profile::{self, Phase},
    typ::{FnArgKind, FnArgType, FnType, TVar, Type, tvar::RigidGate},
    wrap,
};
use ahash::{AHashMap, AHashSet};
use anyhow::{Context, Result, anyhow, bail};
use arcstr::{ArcStr, literal};
use bytes::{Buf, BufMut};
use compact_str::format_compact;
use enumflags2::BitFlags;
use indexmap::{IndexMap, map::Entry as ArgEntry};
use log::{error, warn};
use netidx_core::pack::{Pack, PackError, decode_varint, encode_varint, varint_len};
use netidx_value::Value;
use nohash::IntSet;
use poolshark::local::LPooled;
use smallvec::SmallVec;
use std::{
    collections::hash_map::Entry,
    fmt, mem,
    sync::{
        OnceLock,
        atomic::{AtomicBool, Ordering::Relaxed},
    },
};
use triomphe::Arc as TArc;

/// Reject a direct call to a sync variadic builtin with no positional
/// arguments (`str::concat()`, `sum()`): the node has no data inputs and
/// can never fire. Only a direct `Ref` to the builtin is checkable.
fn reject_dead_variadic_call<R: Rt, E: UserEvent>(
    ctx: &ExecCtx<R, E>,
    scope: &Scope,
    f: &Expr,
    args: &TArc<[(Option<ArcStr>, Expr)]>,
) -> Result<()> {
    let path = match &f.kind {
        ExprKind::Ref { name } => name,
        _ => return Ok(()),
    };
    if args.iter().any(|(label, _)| label.is_none()) {
        return Ok(());
    }
    let Some((_, bind)) = ctx.env.lookup_bind(&scope.lexical, path).ok().flatten() else {
        return Ok(());
    };
    let key = (bind.scope.clone(), bind.name.clone());
    let Some(info) = ctx.builtin_bindings.get(&key) else {
        return Ok(());
    };
    if info.typ.vargs.is_none()
        || info.typ.args.iter().any(|a| a.is_positional())
        || !ctx.builtin_effect(info.name.as_str()).kind().is_sync()
    {
        return Ok(());
    }
    bail!(
        "calling `{path}` with no positional arguments can never produce \
         a value: a sync variadic builtin with no data inputs never fires. \
         Pass it at least one argument, or use never() to express a value \
         that never arrives"
    )
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, netidx_derive::Pack)]
pub(crate) enum ArgKey {
    Positional(usize),
    Named(ArcStr),
}

impl ArgKey {
    fn keyed<'a>(
        labels: impl Iterator<Item = Option<&'a ArcStr>>,
    ) -> impl Iterator<Item = ArgKey> {
        let mut positional = 0;
        labels.map(move |label| match label {
            Some(name) => ArgKey::Named(name.clone()),
            None => {
                positional += 1;
                ArgKey::Positional(positional - 1)
            }
        })
    }

    /// Each formal's key in signature order: a labeled parameter by
    /// name, the k-th positional one as `Positional(k)`.
    pub(crate) fn of_formals(args: &[FnArgType]) -> impl Iterator<Item = ArgKey> + '_ {
        Self::keyed(args.iter().map(|a| a.label()))
    }

    /// Each written argument's key in source order.
    pub(crate) fn of_written(
        args: &[(Option<ArcStr>, Expr)],
    ) -> impl Iterator<Item = ArgKey> + '_ {
        Self::keyed(args.iter().map(|(label, _)| label.as_ref()))
    }

    /// The variadic keys of a signature with `positional` positional
    /// formals.
    fn variadic(positional: usize) -> impl Iterator<Item = ArgKey> {
        (positional..).map(ArgKey::Positional)
    }
}

impl fmt::Display for ArgKey {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ArgKey::Positional(k) => write!(f, "positional argument {k}"),
            ArgKey::Named(name) => write!(f, "#{name}"),
        }
    }
}

/// The call's argument nodes, keyed for signature lookups but iterating
/// in source order. Order is load-bearing: args form a sequential scope
/// chain, so `update` must evaluate them left to right.
pub(crate) type ArgMap<R, E> = IndexMap<ArgKey, Arg<R, E>, ahash::RandomState>;

#[derive(Debug)]
pub(crate) struct Arg<R: Rt, E: UserEvent> {
    pub id: BindId,
    pub node: Option<Node<R, E>>,
    pub is_default: bool,
}

impl<R: Rt, E: UserEvent> Arg<R, E> {
    pub(crate) fn new(id: BindId, node: Option<Node<R, E>>, is_default: bool) -> Self {
        Arg { id, node, is_default }
    }
}

/// Collect every `Type::Fn` arm reachable in `t`: a bare `Fn`, or the
/// `Fn` arms of a `[fn(...), null]` / Set union.
fn collect_fn_arms(t: &Type, out: &mut LPooled<Vec<TArc<FnType>>>) {
    match t {
        Type::Fn(ft) => out.push(ft.clone()),
        Type::Set(ts) => {
            for arm in ts.iter() {
                collect_fn_arms(arm, out)
            }
        }
        _ => (),
    }
}

// XCR claude for eric: renamed, "restored" made explicit, and the lock is not
// held across typecheck1. `&mut []` stays: no builtin's typecheck1 reads its
// args (they check `resolved`), and keeping the gate's faux args alive beside
// the check only to hand them back buys nothing.
/// Re-run a builtin definition's check `Apply` at this site's resolved
/// type; a user definition has no check. The check is shared by every
/// site, the last one's type wins. A definition restored from an image
/// has none until its first site rebuilds it.
fn recheck_builtin<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<R, E>,
    id: LambdaId,
    resolved: &FnType,
    spec: &TArc<Expr>,
) -> Result<()> {
    let _profile = profile::phase(Phase::LambdaFinalize);
    let Some(val) = ctx.lambda_defs.get(&id).cloned() else { return Ok(()) };
    let ldef = val
        .downcast_ref::<LambdaDef<R, E>>()
        .expect("failed to unwrap lambda for typecheck1");
    let Some(check) = ldef.builtin_check() else { return Ok(()) };
    let restored = check.lock().is_none();
    if restored {
        let f = build_builtin_check(ldef, ctx)?;
        *check.lock() = Some(f);
    }
    let mut apply = check.lock().take().expect("builtin check");
    let res = apply.typecheck1(ctx, &mut [], resolved).at(&(**spec));
    *check.lock() = Some(apply);
    res
}

fn compile_apply_args<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<R, E>,
    flags: BitFlags<CFlag>,
    scope: &Scope,
    top_id: ExprId,
    spec: &Expr,
    args: &TArc<[(Option<ArcStr>, Expr)]>,
) -> Result<ArgMap<R, E>> {
    let mut res = ArgMap::default();
    for ((_, expr), key) in args.iter().zip(ArgKey::of_written(args)) {
        let node = Some(compile(ctx, flags, expr.clone(), scope, top_id)?);
        match res.entry(key) {
            ArgEntry::Occupied(e) => bailat!(spec, "duplicate argument {}", e.key()),
            ArgEntry::Vacant(e) => {
                e.insert(Arg::new(BindId::new(), node, false));
            }
        }
    }
    Ok(res)
}

/// A formal of quantified function type `fn<'b: C>(..)`, expanded once,
/// with its quantifiers held rigid for the argument's check: the argument
/// must be well typed for every 'b the bound admits, since the callee may
/// call it at any. `None` for any other formal.
fn quantified_formal(
    env: &Env,
    typ: &Type,
) -> Result<Option<(Type, LPooled<Vec<RigidGate>>)>> {
    let deref = typ.with_deref(|t| t.cloned());
    let expanded = match &deref {
        Some(Type::Fn(_)) => deref.clone(),
        Some(t @ Type::Ref(_)) => t.lookup_ref_with(env, false)?,
        _ => None,
    };
    let Some(formal @ Type::Fn(ft)) = &expanded else { return Ok(None) };
    if ft.quantifiers.is_empty() {
        return Ok(None);
    }
    let mut named: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
    ft.collect_tvars(&mut named);
    let gates = named
        .iter()
        .filter(|(name, _)| ft.quantifiers.contains(*name))
        .map(|(_, tv)| tv.open_rigid())
        .collect();
    Ok(Some((formal.clone(), gates)))
}

/// Check a call's argument node against its formal's type `typ`.
fn typecheck_arg<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<R, E>,
    typ: &Type,
    n: &mut Node<R, E>,
) -> Result<()> {
    // A reference instantiates its signature in its own typecheck0,
    // which must precede the pre-unify.
    if matches!(n.view(), NodeView::Ref(_)) {
        wrap!(n, n.typecheck0(ctx))?;
    }
    match quantified_formal(&ctx.env, typ)? {
        None => {
            Type::pre_unify_arg(&ctx.env, typ, n.typ())?;
            wrap!(n, n.typecheck0(ctx))?;
            wrap!(n, typ.check_contains(&ctx.env, &n.typ()))
        }
        Some((formal, _rigid)) => {
            Type::pre_unify_arg(&ctx.env, &formal, n.typ())?;
            wrap!(n, n.typecheck0(ctx))?;
            wrap!(n, formal.check_contains_rigid(&ctx.env, &n.typ()))
        }
    }
}

/// A `Ref` to `arg`'s id, typed and placed by its node, else by `typ`.
fn arg_ref<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<R, E>,
    top_id: ExprId,
    arg: &Arg<R, E>,
    typ: &Type,
) -> Node<R, E> {
    let (typ, spec) = match &arg.node {
        Some(n) => (n.typ().clone(), TArc::new(n.spec().clone())),
        None => (typ.clone(), NOP.clone()),
    };
    Ref::new(ctx, arg.id, typ, top_id, spec)
}

/// What a [`CallSite`] knows about its callee.
#[derive(Debug)]
pub(crate) enum Callee<R: Rt, E: UserEvent> {
    /// No callee bound yet. `fnode` is re-evaluated every cycle; the
    /// first cycle it yields a `LambdaDef`, `update()` binds.
    DynamicUnbound,
    /// Bound to a callee that may change cycle-to-cycle; `def` is kept
    /// for the per-cycle identity check against `fnode.update()`.
    DynamicBound { def: Value, apply: Box<dyn Apply<R, E>> },
    /// Binding `def` failed: the call is bottom until `fnode` yields
    /// another definition.
    Failed { def: Value },
    /// Pre-bound at compile time by [`CallSite::try_static_resolve`]; the
    /// per-cycle identity check is skipped (`fnode.update()` still runs
    /// for effects). `first_update` primes the body's refs once.
    Static { apply: Box<dyn Apply<R, E>>, first_update: bool },
    /// A statically bound instance the image holds in its heap, decoded
    /// by the first dispatch; `refs` is the body's summary until then.
    Imaged { instance: LambdaInstanceId, first_update: bool, summary: RefsSummary },
}

/// What an imaged instance's body answers `refs` with before it is
/// decoded.
#[derive(Debug, Clone, netidx_derive::Pack)]
pub(crate) struct RefsSummary {
    refed: Vec<BindId>,
    triggering: Vec<BindId>,
    bound: Vec<BindId>,
}

/// The definition and instance a site resolved to at compile time, and
/// the instance's resolved type. A self-call site (resolved while its
/// definition's instance with the same identity was elaborating) names
/// that enclosing instance and stays dynamically bound: at runtime it
/// binds an instance per activation.
#[derive(Debug, Clone, netidx_derive::Pack)]
pub(crate) struct StaticCallTarget {
    pub definition: LambdaId,
    pub instance: LambdaInstanceId,
    pub ftype: FnType,
}

impl<R: Rt, E: UserEvent> Callee<R, E> {
    fn is_bound(&self) -> bool {
        !matches!(self, Callee::DynamicUnbound | Callee::Failed { .. })
    }

    fn apply(&self) -> Option<&dyn Apply<R, E>> {
        match self {
            Callee::DynamicUnbound | Callee::Failed { .. } | Callee::Imaged { .. } => {
                None
            }
            Callee::DynamicBound { apply, .. } | Callee::Static { apply, .. } => {
                Some(&**apply)
            }
        }
    }

    fn apply_mut(&mut self) -> Option<&mut (dyn Apply<R, E> + 'static)> {
        match self {
            Callee::DynamicUnbound | Callee::Failed { .. } | Callee::Imaged { .. } => {
                None
            }
            Callee::DynamicBound { apply, .. } | Callee::Static { apply, .. } => {
                Some(&mut **apply)
            }
        }
    }

    /// Reset to `DynamicUnbound`, returning the bound apply for deletion;
    /// an imaged instance has nothing to delete and stays imaged.
    fn take_apply(&mut self) -> Option<Box<dyn Apply<R, E>>> {
        if matches!(self, Callee::Imaged { .. }) {
            return None;
        }
        match mem::replace(self, Callee::DynamicUnbound) {
            Callee::DynamicUnbound | Callee::Failed { .. } | Callee::Imaged { .. } => {
                None
            }
            Callee::DynamicBound { apply, .. } | Callee::Static { apply, .. } => {
                Some(apply)
            }
        }
    }
}

#[derive(Debug)]
pub struct CallSite<R: Rt, E: UserEvent> {
    pub(super) slept: WakeBit,
    pub(super) spec: TArc<Expr>,
    pub(super) ftype: Option<FnType>,
    pub(super) rtype: Type,
    pub(crate) fnode: Node<R, E>,
    pub(crate) args: ArgMap<R, E>,
    pub(super) arg_refs: Vec<Node<R, E>>,
    pub(crate) callee: Callee<R, E>,
    pub(crate) static_target: Option<StaticCallTarget>,
    /// A trait call over a union self type lowered to a select, one
    /// static call per member; once set every `Update` method delegates.
    pub(crate) lowered: Option<Node<R, E>>,
    pub(crate) recursive_edge: AtomicBool,
    pub(super) flags: BitFlags<CFlag>,
    pub(super) scope: Scope,
    pub(super) top_id: ExprId,
    /// Set by `analysis::analyze` when this is a tail-position self-call
    /// in a sync tail-recursive body: the rebind args in callee-signature
    /// order. `update` then stashes its args in `ctx.pending_tail_call`,
    /// keyed by `static_target`'s definition, instead of dispatching.
    tail_arg_order: OnceLock<Box<[BindId]>>,
    pub(super) resident: TagValue,
}

impl<R: Rt, E: UserEvent> CallSite<R, E> {
    /// An unbound site over compiled parts; `ftype` is `None` until
    /// `typecheck0` instantiates it.
    pub(crate) fn unbound(
        spec: TArc<Expr>,
        ftype: Option<FnType>,
        rtype: Type,
        fnode: Node<R, E>,
        args: ArgMap<R, E>,
        scope: Scope,
        flags: BitFlags<CFlag>,
        top_id: ExprId,
    ) -> Self {
        Self {
            slept: WakeBit::default(),
            spec,
            ftype,
            rtype,
            fnode,
            args,
            arg_refs: Vec::new(),
            callee: Callee::DynamicUnbound,
            static_target: None,
            lowered: None,
            recursive_edge: AtomicBool::new(false),
            flags,
            scope,
            top_id,
            tail_arg_order: OnceLock::new(),
            resident: TagValue::phantom(),
        }
    }

    /// Mark this site a tail self-call rebinding the callee's formals
    /// from `order` (`analysis::analyze`); a site is marked once.
    pub(crate) fn mark_self_tail_call(&self, order: Box<[BindId]>) {
        let _ = self.tail_arg_order.set(order);
    }

    /// The function type at this call site with the site's tvars unified
    /// in. `None` before typecheck, or if this site errored first.
    pub fn ftype(&self) -> Option<&FnType> {
        self.ftype.as_ref()
    }

    /// The detached, resolved function type owned by a statically-bound
    /// callee instance.
    pub fn resolved_ftype(&self) -> Option<&FnType> {
        self.static_target.as_ref().map(|target| &target.ftype)
    }

    pub(crate) fn static_target(&self) -> Option<&StaticCallTarget> {
        self.static_target.as_ref()
    }

    pub(crate) fn is_recursive_edge(&self) -> bool {
        self.recursive_edge.load(Relaxed)
    }

    pub(crate) fn set_recursive_edge(&self, recursive: bool) {
        self.recursive_edge.store(recursive, Relaxed)
    }

    /// Source-order argument list. Pair with `args()` to recover the
    /// runtime sub-Node per arg.
    pub fn spec_args(&self) -> &TArc<[(Option<ArcStr>, Expr)]> {
        match &self.spec.kind {
            ExprKind::Apply(a) => &a.args,
            _ => unreachable!("CallSite spec must be ExprKind::Apply"),
        }
    }

    /// Look up a positional argument's compiled sub-Node.
    pub fn arg_positional(&self, idx: usize) -> Option<&Node<R, E>> {
        self.arg(&ArgKey::Positional(idx))
    }

    /// Look up a labeled argument's compiled sub-Node.
    pub fn arg_named(&self, name: &ArcStr) -> Option<&Node<R, E>> {
        self.arg(&ArgKey::Named(name.clone()))
    }

    fn arg(&self, key: &ArgKey) -> Option<&Node<R, E>> {
        self.args.get(key).and_then(|a| a.node.as_ref())
    }

    /// The function expression's compiled Node.
    pub fn fnode(&self) -> &Node<R, E> {
        &self.fnode
    }

    /// The scope this call site was compiled in.
    pub(crate) fn scope(&self) -> &Scope {
        &self.scope
    }

    /// View the [`Apply`] this CallSite is bound to; `None` until a
    /// runtime bind or `try_static_resolve` has populated `self.callee`.
    pub fn resolved_apply(&self) -> Option<ApplyView<'_, R, E>> {
        self.callee.apply().map(|a| a.view())
    }

    /// Signature-order `Ref` Nodes, one per formal, with labeled defaults
    /// resolved. `None` until bound. [`Self::arg_positional`] /
    /// [`Self::arg_named`] give the source-order view.
    pub fn arg_refs(&self) -> Option<&[Node<R, E>]> {
        if self.callee.is_bound() { Some(&self.arg_refs) } else { None }
    }

    pub(crate) fn compile(
        ctx: &mut ExecCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: Expr,
        scope: &Scope,
        top_id: ExprId,
        args: &TArc<[(Option<ArcStr>, Expr)]>,
        f: &TArc<Expr>,
    ) -> Result<Node<R, E>> {
        reject_dead_variadic_call(ctx, scope, f, args).at(&spec)?;
        let fnode = compile(ctx, flags, (**f).clone(), scope, top_id)?;
        let args = compile_apply_args(ctx, flags, scope, top_id, &spec, args)?;
        let site = Self::unbound(
            TArc::new(spec),
            None,
            Type::empty_tvar(),
            fnode,
            args,
            scope.clone(),
            flags,
            top_id,
        );
        Ok(Node::new(site))
    }

    fn clear_prepared_bind(&mut self, ctx: &mut ExecCtx<R, E>) {
        if let Some(mut apply) = self.callee.take_apply() {
            apply.delete(ctx);
        }
        for mut n in self.arg_refs.drain(..) {
            n.delete(ctx);
        }
        self.args.retain(|_, arg| {
            if arg.is_default {
                ctx.rt.store_remove(&arg.id);
                if let Some(mut n) = arg.node.take() {
                    n.delete(ctx);
                }
                false
            } else {
                true
            }
        });
    }

    fn prepare_bind<F>(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        scope: &Scope,
        flags: BitFlags<CFlag>,
        f: &LambdaDef<R, E>,
        mut prime_default_refs: F,
    ) -> Result<()>
    where
        F: FnMut(&mut ExecCtx<R, E>, &Refs),
    {
        let mut flags = flags;
        flags.remove(CFlag::WarnUnhandled);
        self.clear_prepared_bind(ctx);
        let formals = f.typ.args.iter().zip(f.argspec.iter());
        for ((farg, argspec), key) in formals.zip(ArgKey::of_formals(&f.typ.args)) {
            if let Some(arg) = self.args.get(&key) {
                let r = arg_ref(ctx, self.top_id, arg, &farg.typ);
                self.arg_refs.push(r);
                continue;
            }
            let ArgKey::Named(name) = &key else { bail!("missing required {key}") };
            if !farg.kind.has_default() {
                bail!("BUG: in bind missing required argument {name}")
            }
            let Some(Some(expr)) = &argspec.labeled else {
                bail!("expected default value")
            };
            let mut default_node = ctx.with_restored(f.env.clone(), |ctx| {
                let local_scope = Scope {
                    dynamic: scope.dynamic.clone(),
                    lexical: f.scope.lexical.clone(),
                };
                let n = compile(ctx, flags, expr.clone(), &local_scope, self.top_id)?;
                let mut refs = Refs::default();
                n.refs(&mut refs);
                prime_default_refs(ctx, &refs);
                Ok::<_, anyhow::Error>(n)
            })?;
            // A default typechecks against this site's instantiated
            // signature, so an omitting site infers from it.
            wrap!(default_node, default_node.typecheck0(ctx))?;
            let typ = default_node.typ().clone();
            let site_arg = self
                .ftype
                .as_ref()
                .and_then(|ft| ft.args.iter().find(|a| a.label() == Some(name)));
            if let Some(sarg) = site_arg {
                wrap!(default_node, sarg.typ.check_contains(&ctx.env, &typ))?;
            }
            let id = BindId::new();
            let spec = TArc::new(default_node.spec().clone());
            self.arg_refs.push(Ref::new(ctx, id, typ, self.top_id, spec));
            self.args.insert(key, Arg::new(id, Some(default_node), true));
        }
        if f.typ.vargs.is_some() {
            let positional = f.typ.args.iter().filter(|a| a.is_positional()).count();
            for key in ArgKey::variadic(positional) {
                let Some(arg) = self.args.get(&key) else { break };
                let r = arg_ref(ctx, self.top_id, arg, &Type::Bottom);
                self.arg_refs.push(r);
            }
        }
        Ok(())
    }

    fn init_prepared_bind(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        scope: &Scope,
        f: &LambdaDef<R, E>,
        mode: BindMode<'_>,
    ) -> Result<Box<dyn Apply<R, E>>> {
        (f.init)(scope, ctx, &mut self.arg_refs, mode, self.top_id)
    }

    fn setup_dynamic_bind<F>(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        scope: &Scope,
        flags: BitFlags<CFlag>,
        f: &LambdaDef<R, E>,
        prime_default_refs: F,
    ) -> Result<Box<dyn Apply<R, E>>>
    where
        F: FnMut(&mut ExecCtx<R, E>, &Refs),
    {
        self.prepare_bind(ctx, scope, flags, f, prime_default_refs)?;
        let resolved_ftype = self.ftype.as_ref().map(FnType::resolve_tvars);
        let mode = resolved_ftype
            .as_ref()
            .map(BindMode::Dynamic)
            .unwrap_or(BindMode::Definition);
        let mut apply = self.init_prepared_bind(ctx, scope, f, mode)?;
        if let Err(e) = apply.typecheck0(ctx, &mut self.arg_refs) {
            if crate::dbgenv::gxdbg_swallow() {
                eprintln!("SWALLOWED-TC0 at {}: {e:#}", self.spec);
            }
        }
        Ok(apply)
    }

    fn typecheck_static_defaults(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        for arg in self.args.values_mut() {
            if arg.is_default
                && let Some(node) = arg.node.as_mut()
            {
                wrap!(node, node.typecheck1(ctx))?;
            }
        }
        Ok(())
    }

    fn instance_ftype(&self) -> Option<FnType> {
        self.callee.apply().map(|apply| apply.typ().resolve_tvars())
    }

    /// Re-read the bound instance's resolved ftype into `static_target`.
    /// `None` when no apply is bound.
    fn refresh_static_ftype(&mut self) -> Option<FnType> {
        let ftype = self.instance_ftype()?;
        if let Some(target) = &mut self.static_target {
            target.ftype = ftype.clone();
        }
        Some(ftype)
    }

    /// The resolution half of `typecheck1`, run under the caller's cell
    /// protection so it unwinds on every error path.
    fn typecheck1_resolve(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        ftype: &FnType,
    ) -> Result<()> {
        self.try_static_resolve(ctx)?;
        self.refresh_static_ftype();
        let resolved = ftype.resolve_tvars();
        let spec = self.spec.clone();
        for id in ftype.lambda_ids.ids().iter().copied() {
            recheck_builtin::<R, E>(ctx, id, &resolved, &spec)?;
        }
        // Callbacks reachable through a fn-typed argument.
        let mut fts: LPooled<Vec<TArc<FnType>>> = LPooled::take();
        for arg in resolved.args.iter() {
            fts.clear();
            collect_fn_arms(&arg.typ, &mut fts);
            for ft in fts.iter() {
                for id in ft.lambda_ids.ids().iter().copied() {
                    recheck_builtin::<R, E>(ctx, id, ft, &spec)?;
                }
            }
        }
        Ok(())
    }

    fn setup_static_bind(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        scope: &Scope,
        flags: BitFlags<CFlag>,
        f: &LambdaDef<R, E>,
    ) -> Result<(Box<dyn Apply<R, E>>, FnType)> {
        let _profile = profile::phase(Phase::StaticBind);
        self.prepare_bind(ctx, scope, flags, f, |_, _| {})?;
        if self.ftype.is_none() {
            bail!("statically resolving an untyped call site: {}", self.spec)
        }
        let site_ftype = self.ftype.as_ref().unwrap().resolve_tvars();
        let instance_ftype = if same_parameters(&site_ftype, &f.typ) {
            site_ftype.clone()
        } else {
            let definition_ftype = f.typ.reset_tvars();
            definition_ftype.alias_tvars(&mut LPooled::take());
            site_ftype.check_contains(&ctx.env, &definition_ftype)?;
            definition_ftype.resolve_tvars()
        };
        let mut apply = self.init_prepared_bind(
            ctx,
            scope,
            f,
            BindMode::Static { instance: &instance_ftype, site: &site_ftype },
        )?;
        let instance_ftype = apply.typ().as_ref().clone();
        // `site_ftype` is a deep clone: the instance's inferred return
        // must be unified back into the site's live rtype cell.
        if let Some(site_ft) = self.ftype.as_ref()
            && let Err(e) = site_ft.rtype.check_contains(&ctx.env, &instance_ftype.rtype)
        {
            apply.delete(ctx);
            return Err(e.at(self.fnode.spec()));
        }
        Ok((apply, instance_ftype))
    }

    fn bind(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        scope: Scope,
        flags: BitFlags<CFlag>,
        fv: Value,
        f: &LambdaDef<R, E>,
        event: &mut Event<E>,
        set: &mut Vec<BindId>,
    ) -> Result<()> {
        let _bind_span = perfdbg::span(&perfdbg::BIND_NS);
        if perfdbg::enabled() {
            perfdbg::BIND_CALLS.fetch_add(1, Relaxed);
        }
        let setup_span = perfdbg::span(&perfdbg::SETUP_NS);
        // XCR claude for eric: not dead. A bind can run under an arm's wake view
        // (`event.wake_init`), where standing_view reads a standing entry stale;
        // these FIRED entries give a fresh default's subtree its birth there
        // (design/wake_catchup.md, the birth rule). The body sees them too.
        let apply = self.setup_dynamic_bind(ctx, &scope, flags, f, |ctx, refs| {
            refs.with_external_refs(|id| {
                if let Some(v) = ctx.rt.store_value(&id) {
                    if let Entry::Vacant(e) = event.variables.entry(id) {
                        e.insert(TagValue::fired(v));
                        set.push(id);
                    }
                }
            });
        })?;
        drop(setup_span);
        // A def whose defining Lambda node was deleted has no
        // `lambda_defs` entry; restore it for this elaboration only.
        let restored_def = if ctx.lambda_defs.contains_key(&f.id) {
            false
        } else {
            ctx.lambda_defs.insert(f.id, fv.clone());
            true
        };
        self.callee = Callee::DynamicBound { def: fv, apply };
        // The lazy-bound body postdates the program-wide typecheck1 and
        // analysis passes: resolve its call sites and analyze it here.
        let identity = self.fn_arg_identity(ctx);
        if let Some(apply) = self.callee.apply_mut()
            && let ApplyView::Lambda(g) = apply.view()
        {
            let instance = g.instance_id();
            let instance_ftype = apply.typ();
            // A recursive lazy bind: its body stays lazy.
            let already_active = ctx.resolving(f.id, &identity).is_some();
            ctx.push_resolving(
                f.id,
                ResolvingLambda {
                    instance,
                    ftype: instance_ftype.as_ref().clone(),
                    identity,
                },
            );
            if !already_active {
                let _tc1_span = perfdbg::span(&perfdbg::TC1_NS);
                if let Err(e) = apply.typecheck1(ctx, &mut [], &instance_ftype) {
                    if dbgenv::gxdbg_swallow() {
                        eprintln!("SWALLOWED-LAZY-TC1 at {}: {e:#}", self.spec);
                    }
                    log::trace!("bind: lazy-bound callee body typecheck1 failed: {e:#}");
                }
            }
            ctx.pop_resolving(f.id, instance);
            if let ApplyView::Lambda(g) = apply.view() {
                let _an_span = perfdbg::span(&perfdbg::ANALYZE_NS);
                let self_bind = match self.fnode.view() {
                    NodeView::Ref(r) => Some(r.id),
                    _ => None,
                };
                analysis::analyze_bound_callee(g, self_bind, ctx);
            }
        }
        // Defaults update for the first time under the init view.
        let prev_init = mem::replace(&mut event.init, true);
        for arg in self.args.values_mut() {
            if arg.is_default
                && let Some(node) = &mut arg.node
            {
                let tv = node.update(ctx, event).clone();
                let feeds = Feeds::Id(arg.id);
                if publish_production(ctx, event, feeds, &tv, true, QuietAtRoot::Deliver)
                {
                    set.push(arg.id);
                }
            }
        }
        event.init = prev_init;
        if restored_def {
            ctx.lambda_defs.remove(&f.id);
        }
        Ok(())
    }

    /// Pre-bind this CallSite to a statically known `LambdaDef` at compile
    /// time, replacing the lazy bind `update()` would run. Idempotent.
    pub fn resolve_static(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        def: &LambdaDef<R, E>,
    ) -> Result<()> {
        if matches!(self.callee, Callee::Static { .. }) || self.static_target.is_some() {
            return Ok(());
        }
        // A site reached while an instantiation with the same fn-arg
        // identity is resolving is a self-call and shares that instance;
        // a different identity is a fresh instantiation.
        let identity = self.fn_arg_identity(ctx);
        let active = ctx.resolving(def.id, &identity);
        if let Some(active) = active {
            let scope = self.scope.clone();
            self.prepare_bind(ctx, &scope, self.flags, def, |_, _| {})?;
            self.typecheck_static_defaults(ctx)?;
            if self.ftype.is_none() {
                bail!("statically resolving an untyped call site: {}", self.spec)
            }
            self.static_target = Some(StaticCallTarget {
                definition: def.id,
                instance: active.instance,
                ftype: active.ftype.resolve_tvars(),
            });
            return Ok(());
        }
        let scope = self.scope.clone();
        let (apply, instance_ftype) =
            self.setup_static_bind(ctx, &scope, self.flags, def)?;
        let instance = match apply.view() {
            ApplyView::Lambda(g) => Some(g.instance_id()),
            ApplyView::BuiltIn => None,
        };
        if let Some(instance) = instance {
            self.static_target = Some(StaticCallTarget {
                definition: def.id,
                instance,
                ftype: instance_ftype.clone(),
            });
        }
        self.callee = Callee::Static { apply, first_update: true };
        // Fn-typed args are registered under the instance's param
        // BindIds for the whole body typecheck (`register_fn_params`).
        let (param_binds, trait_param_binds) =
            self.register_fn_params(ctx, &instance_ftype);
        if let Some(instance) = instance {
            ctx.push_resolving(
                def.id,
                ResolvingLambda {
                    instance,
                    ftype: instance_ftype.clone(),
                    identity: identity.clone(),
                },
            );
        }
        let typecheck0 = {
            let (callee, arg_refs) = (&mut self.callee, &mut self.arg_refs);
            callee
                .apply_mut()
                .expect("static callee must have an apply")
                .typecheck0(ctx, arg_refs)
        }
        .with_context(|| format!("in the instance of {} at this call site", self.spec));
        let resolved_ftype =
            self.refresh_static_ftype().expect("static callee must have an apply");
        let res = typecheck0.and_then(|()| self.typecheck_static_defaults(ctx)).and_then(
            |()| {
                self.callee
                    .apply_mut()
                    .expect("static callee must have an apply")
                    .typecheck1(ctx, &mut [], &resolved_ftype)
                    .with_context(|| {
                        format!("in the instance of {} at this call site", self.spec)
                    })
            },
        );
        self.refresh_static_ftype().expect("static callee must have an apply");
        if res.is_ok() {
            if let Callee::Static { apply, .. } = &self.callee {
                if let ApplyView::Lambda(g) = apply.view() {
                    profile::instance_signature(
                        g.instance_id(),
                        g.typ(),
                        Some(&identity),
                    );
                }
            }
        }
        Self::unregister_fn_params(ctx, param_binds, trait_param_binds);
        if let Some(instance) = instance {
            ctx.pop_resolving(def.id, instance);
        }
        res
    }

    /// Pre-bind this site when its function expression resolves to one
    /// known `LambdaDef` (a `Ref` to a non-`<-`-target lambda binding, or a
    /// lambda literal), or dispatch a trait method by its self type.
    /// No-op for dynamic call sites.
    fn try_static_resolve(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        if matches!(self.callee, Callee::Static { .. }) {
            return Ok(());
        }
        let target: Option<Value> = match self.fnode.view() {
            NodeView::Ref(r) => {
                if dbgenv::gxdbg_resolve() {
                    eprintln!(
                        "RESOLVE {} id={:?} unstable={} b2l={} cached={}",
                        self.spec,
                        r.id,
                        ctx.batch_connect_targets.contains(&r.id),
                        ctx.bind_to_lambda.contains_key(&r.id),
                        ctx.rt.store_value(&r.id).is_some(),
                    );
                }
                if ctx.batch_connect_targets.contains(&r.id) {
                    None
                } else {
                    ctx.bind_to_lambda
                        .get(&r.id)
                        .cloned()
                        .or_else(|| ctx.rt.store_value(&r.id))
                }
            }
            NodeView::Lambda(l) => Some(l.def_value().clone()),
            _ => None,
        };
        let fv = match target {
            Some(fv) => fv,
            None => {
                if let NodeView::Ref(r) = self.fnode.view()
                    && let Some(tm) = ctx.env.trait_methods.get(&r.id).copied()
                {
                    return self.resolve_trait_call(ctx, tm);
                }
                return Ok(());
            }
        };
        let Some(def) = fv.downcast_ref::<LambdaDef<R, E>>() else {
            return Ok(());
        };
        self.resolve_static(ctx, def)
    }

    /// This site's instantiation identity ([`FnArgIdentity`]): per
    /// argument, the source lambda it resolves to (a literal is its own
    /// source; a `Ref` goes through `bind_to_lambda`; a `<-` target is
    /// dynamic).
    fn fn_arg_identity(&self, ctx: &ExecCtx<R, E>) -> FnArgIdentity {
        let mut identity: FnArgIdentity = self
            .args
            .iter()
            .map(|(key, arg)| {
                let source = arg.node.as_ref().and_then(|node| match node.view() {
                    NodeView::Lambda(l) => Some(l.source_id()),
                    NodeView::Ref(r) if !ctx.batch_connect_targets.contains(&r.id) => ctx
                        .bind_to_lambda
                        .get(&r.id)
                        .and_then(|fv| fv.downcast_ref::<LambdaDef<R, E>>())
                        .map(|def| def.source),
                    _ => None,
                });
                (key.clone(), source)
            })
            .collect();
        identity.sort_unstable_by(|(a, _), (b, _)| a.cmp(b));
        identity
    }

    /// Register this site's statically known fn-typed args under the
    /// instance's param BindIds. Held for the whole body typecheck so calls
    /// to and captures of a fn parameter resolve in one pass.
    fn register_fn_params(
        &self,
        ctx: &mut ExecCtx<R, E>,
        ftype: &FnType,
    ) -> (LPooled<Vec<BindId>>, LPooled<Vec<BindId>>) {
        let mut param_binds: LPooled<Vec<BindId>> = LPooled::take();
        let mut trait_param_binds: LPooled<Vec<BindId>> = LPooled::take();
        let apply = match self.callee.apply() {
            Some(a) => a,
            None => return (param_binds, trait_param_binds),
        };
        let ApplyView::Lambda(g) = apply.view() else {
            return (param_binds, trait_param_binds);
        };
        let formals =
            ftype.args.iter().zip(g.args()).zip(ArgKey::of_formals(&ftype.args));
        for ((farg, pat), key) in formals {
            if !farg.typ.with_deref(|t| matches!(t, Some(Type::Fn(_)))) {
                continue;
            }
            let Some(id) = pat.single_bind_id() else { continue };
            let Some(arg_node) = self.arg(&key) else { continue };
            match arg_node.view() {
                NodeView::Lambda(l) => {
                    let fv = l.def_value().clone();
                    if let Some(def) = fv.downcast_ref::<LambdaDef<R, E>>() {
                        ctx.fn_forward_resolutions.insert(id, def.id);
                    }
                    ctx.bind_to_lambda.insert(id, fv);
                    param_binds.push(id);
                }
                NodeView::Ref(r) => {
                    if ctx.batch_connect_targets.contains(&r.id) {
                        continue;
                    }
                    if let Some(fv) = ctx.bind_to_lambda.get(&r.id).cloned() {
                        if let Some(def) = fv.downcast_ref::<LambdaDef<R, E>>() {
                            ctx.fn_forward_resolutions.insert(id, def.id);
                        }
                        ctx.bind_to_lambda.insert(id, fv);
                        param_binds.push(id);
                    } else if let Some(tm) = ctx.env.trait_methods.get(&r.id).copied() {
                        ctx.env.trait_methods.insert_cow(id, tm);
                        trait_param_binds.push(id);
                    }
                }
                _ => {}
            }
        }
        (param_binds, trait_param_binds)
    }

    /// Undo [`Self::register_fn_params`]; the `fn_forward_resolutions`
    /// snapshot stays for the kernel cache fingerprint.
    fn unregister_fn_params(
        ctx: &mut ExecCtx<R, E>,
        mut param_binds: LPooled<Vec<BindId>>,
        mut trait_param_binds: LPooled<Vec<BindId>>,
    ) {
        for id in param_binds.drain(..) {
            ctx.bind_to_lambda.remove(&id);
        }
        for id in trait_param_binds.drain(..) {
            ctx.env.trait_methods.remove_cow(&id);
        }
    }

    /// Take this call's argument nodes in source order under synthesized
    /// names (`#a<i>`, or `#s` for the argument at `self_key`). Returns the
    /// `(name, node)` pairs and the `(label, name)` list a synthesized
    /// call spells them with.
    pub(super) fn take_operands(
        &mut self,
        self_key: Option<&ArgKey>,
    ) -> Result<(
        LPooled<Vec<(ArcStr, Node<R, E>)>>,
        LPooled<Vec<(Option<ArcStr>, ArcStr)>>,
    )> {
        let ExprKind::Apply(ApplyExpr { args, .. }) = &self.spec.kind else {
            bail!("call site without an apply spec: {}", self.spec)
        };
        let mut operands: LPooled<Vec<(ArcStr, Node<R, E>)>> = LPooled::take();
        let mut names: LPooled<Vec<(Option<ArcStr>, ArcStr)>> = LPooled::take();
        for (i, ((label, _), key)) in
            args.iter().zip(ArgKey::of_written(args)).enumerate()
        {
            let name: ArcStr = if Some(&key) == self_key {
                literal!("#s")
            } else {
                format_compact!("#a{i}").as_str().into()
            };
            let Some(node) = self.args.get_mut(&key).and_then(|a| a.node.take()) else {
                bail!("call site argument {i} has no node: {}", self.spec)
            };
            operands.push((name.clone(), node));
            names.push((label.clone(), name));
        }
        Ok((operands, names))
    }

    /// Install `node` as this call's lowering: the function node and
    /// any remaining argument nodes are deleted, every `Update` method
    /// delegates to it from now on.
    pub(super) fn install_lowered(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        node: Node<R, E>,
    ) -> Result<()> {
        wrap!(node, self.rtype.check_contains(&ctx.env, node.typ()))?;
        for arg in self.args.values_mut() {
            if let Some(mut n) = arg.node.take() {
                n.delete(ctx);
            }
        }
        for mut n in self.arg_refs.drain(..) {
            n.delete(ctx);
        }
        let mut old = mem::replace(&mut self.fnode, Node::new(Nop { typ: Type::Bottom }));
        old.delete(ctx);
        self.lowered = Some(node);
        Ok(())
    }

    /// Re-point this call's function node at binding `bind`.
    pub(super) fn retarget(&mut self, ctx: &mut ExecCtx<R, E>, bind: BindId) {
        let typ = ctx
            .env
            .by_id
            .get(&bind)
            .map(|b| b.typ.clone())
            .unwrap_or_else(Type::empty_tvar);
        let fspec = match &self.spec.kind {
            ExprKind::Apply(a) => (*a.function).clone(),
            _ => (*self.spec).clone(),
        };
        let fnode = Ref::new(ctx, bind, typ, self.top_id, fspec);
        let mut old = mem::replace(&mut self.fnode, fnode);
        old.delete(ctx);
    }
}

impl<R: Rt, E: UserEvent> CallSite<R, E> {
    fn update_call(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        event: &mut Event<E>,
    ) -> &TagValue {
        let woke = self.slept.take() && ctx.frame_depth == 0;
        if matches!(self.callee, Callee::Imaged { .. })
            && let Err(e) = self.materialize(ctx)
        {
            warn!("decoding the instance of {}: {e:#}; resolving it afresh", self.spec);
            self.callee = Callee::DynamicUnbound;
        }
        let mut set: LPooled<Vec<BindId>> = LPooled::take();
        let mut arg_fired = false;
        let tail = self
            .tail_arg_order
            .get()
            .zip(self.static_target.as_ref().map(|target| target.definition));
        // XCR claude for eric: a bind seeds from the quiet productions only, so only
        // those are captured, and a static callee's fnode value is no longer cloned.
        // A dynamic site still clones its quiet args each cycle: whether it rebinds
        // is known only after `fnode`, which updates after the args.
        let may_bind = match &self.callee {
            Callee::Static { first_update, .. } => *first_update,
            _ => true,
        };
        let root = if woke { QuietAtRoot::Stand } else { QuietAtRoot::Skip };
        let mut prods: SmallVec<[(BindId, TagValue); 4]> = SmallVec::new();
        for arg in self.args.values_mut() {
            let Some(node) = &mut arg.node else { continue };
            let tv = node.update(ctx, event);
            let fired = tv.tag().triggers();
            arg_fired |= fired;
            if tail.is_some() || (may_bind && !fired) {
                prods.push((arg.id, tv.clone()));
            }
            if publish_production(ctx, event, Feeds::Id(arg.id), tv, false, root) {
                set.push(arg.id);
            }
        }
        // Tail-call interception: stash the rebind args for the enclosing
        // `GXLambda::update` loop instead of dispatching. Only a genuine
        // call (an arg fired, or an init view) enters the loop.
        if let Some((order, lambda)) = tail {
            if event.init || arg_fired {
                let args = order
                    .iter()
                    .map(|id| match prods.iter().find(|(pid, _)| pid == id) {
                        Some((_, tv)) => Some(tv.clone()),
                        None => read_quiet(ctx, event, id),
                    })
                    .collect();
                debug_assert!(ctx.pending_tail_call.is_none());
                ctx.pending_tail_call = Some(PendingTailCall { lambda, args });
            }
            // A quiet tail self-call rides without dispatching, or it
            // would consume the callee's first-dispatch init view.
            for id in set.drain(..) {
                event.variables.remove(&id);
            }
            return self.resident.ride();
        }
        // `fnode.update` runs every cycle for its effects; a `Static`
        // callee discards the value.
        let static_callee = matches!(self.callee, Callee::Static { .. });
        let (fnode_tag, fnode_value) = {
            let tv = self.fnode.update(ctx, event);
            let tag = tv.tag();
            (tag, (!static_callee && !tag.is_bottom()).then(|| tv.value_cloned()))
        };
        if fnode_tag.is_bottom() && !static_callee {
            for id in set.drain(..) {
                event.variables.remove(&id);
            }
            return self.resident.set_bottom(fnode_tag.triggers() || arg_fired);
        }
        let bound = if let Callee::Static { first_update, .. } = &mut self.callee {
            mem::replace(first_update, false)
        } else {
            fnode_value.is_some_and(|v| self.rebind(ctx, event, v, &mut set))
        };
        if bound {
            for (id, tv) in prods.iter() {
                let tag = tv.tag();
                if tag.triggers() || tag.is_bottom() {
                    continue;
                }
                let Some(arg) = self.args.values().find(|a| a.id == *id) else {
                    continue;
                };
                let root = if arg.is_default {
                    QuietAtRoot::Deliver
                } else {
                    QuietAtRoot::Stand
                };
                if publish_production(ctx, event, Feeds::Id(*id), tv, true, root) {
                    set.push(*id);
                }
            }
        }
        if dbgenv::gxdbg_cs() {
            let kind = match self.callee.apply() {
                None => "none",
                Some(a) => match a.view() {
                    ApplyView::Lambda(_) => "lambda",
                    ApplyView::BuiltIn => "builtin",
                },
            };
            eprintln!(
                "CS spec={} bound={bound} kind={kind} argfired={arg_fired}",
                self.spec,
            );
        }
        let res = match self.callee.apply_mut() {
            None => None,
            Some(f) if !bound => Some(f.update(ctx, &mut self.arg_refs, event).clone()),
            Some(f) => {
                // A fresh bind dispatches under the init view.
                let init = mem::replace(&mut event.init, true);
                let res = f.update(ctx, &mut self.arg_refs, event).clone();
                event.init = init;
                Some(res)
            }
        };
        if dbgenv::gxdbg_cs() {
            eprintln!(
                "CS-RES spec={} res={:?} fd={}",
                self.spec,
                res.as_ref().map(|tv| tv.tag()),
                ctx.frame_depth
            );
        }
        for id in set.drain(..) {
            event.variables.remove(&id);
        }
        match res {
            Some(tv) => self.resident.set(tv),
            None if matches!(self.callee, Callee::Failed { .. }) => {
                self.resident.set_bottom(fnode_tag.triggers() || arg_fired)
            }
            None => self.resident.ride(),
        }
    }

    /// Bind the definition `v` unless it is the one bound, or the one
    /// whose bind failed; true when a fresh callee was bound.
    fn rebind(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        event: &mut Event<E>,
        v: Value,
        set: &mut Vec<BindId>,
    ) -> bool {
        let same = match &self.callee {
            Callee::DynamicBound { def, .. } | Callee::Failed { def } => def == &v,
            _ => false,
        };
        if same {
            return false;
        }
        let Some(lb) = v.downcast_ref::<LambdaDef<R, E>>() else {
            panic!("value {v:?} is not a function")
        };
        let scope = self.scope.clone();
        match self.bind(ctx, scope, self.flags, v.clone(), lb, event, set) {
            Ok(()) => true,
            Err(e) => {
                error!("{}: binding the callee failed: {e:#}", self.spec);
                self.clear_prepared_bind(ctx);
                self.callee = Callee::Failed { def: v };
                false
            }
        }
    }
}

/// How a site's callee travels: unbound (a dynamic site before its
/// first cycle), an imaged instance of a lambda, or a builtin's own
/// image, decoded by its registered decoder over the imaged arguments.
const CALLEE_UNBOUND: u8 = 0;
const CALLEE_INSTANCE: u8 = 1;
const CALLEE_BUILTIN: u8 = 2;

impl<R: Rt, E: UserEvent> Arg<R, E> {
    fn image_len(&self, key: &ArgKey) -> usize {
        key.encoded_len()
            + self.id.encoded_len()
            + opt_node_len(self.node.as_ref())
            + self.is_default.encoded_len()
    }

    fn image_encode(&self, key: &ArgKey, buf: &mut ImageBuf) -> Result<(), PackError> {
        key.encode(buf)?;
        self.id.encode(buf)?;
        opt_node_encode(self.node.as_ref(), buf)?;
        self.is_default.encode(buf)
    }

    fn image_decode(
        ctx: &mut ExecCtx<R, E>,
        buf: &mut &[u8],
    ) -> Result<(ArgKey, Self), PackError> {
        let key = ArgKey::decode(buf)?;
        let id = BindId::decode(buf)?;
        let node = opt_node_decode(ctx, buf)?;
        let is_default = bool::decode(buf)?;
        Ok((key, Arg { id, node, is_default }))
    }
}

impl RefsSummary {
    fn of(refs: &Refs) -> Self {
        let sorted = |ids: &IntSet<BindId>| {
            let mut v: Vec<BindId> = ids.iter().copied().collect();
            v.sort_unstable();
            v
        };
        RefsSummary {
            refed: sorted(&refs.refed),
            triggering: sorted(&refs.triggering),
            bound: sorted(&refs.bound),
        }
    }

    fn add_to(&self, refs: &mut Refs) {
        refs.refed.extend(self.refed.iter().copied());
        refs.triggering.extend(self.triggering.iter().copied());
        refs.bound.extend(self.bound.iter().copied());
    }
}

impl<R: Rt, E: UserEvent> CallSite<R, E> {
    fn callee_mode(&self) -> Result<u8, PackError> {
        match &self.callee {
            Callee::DynamicUnbound => Ok(CALLEE_UNBOUND),
            Callee::DynamicBound { .. } | Callee::Failed { .. } => {
                Err(PackError::Application(image::NOT_QUIESCENT))
            }
            Callee::Static { apply, .. } => match apply.view() {
                ApplyView::Lambda(_) => Ok(CALLEE_INSTANCE),
                ApplyView::BuiltIn => Ok(CALLEE_BUILTIN),
            },
            Callee::Imaged { .. } => Err(PackError::Application(image::NOT_IMAGED)),
        }
    }

    /// The body's reference summary an imaged site answers `refs` with
    /// before its instance is decoded, as `f` sees it; walked once per
    /// instance and kept by the encoder.
    fn with_refs_summary<T>(
        apply: &dyn Apply<R, E>,
        f: impl FnOnce(&RefsSummary) -> T,
    ) -> Result<T, PackError> {
        let ApplyView::Lambda(g) = apply.view() else {
            return Err(PackError::Application(image::NOT_IMAGED));
        };
        let instance = g.instance_id();
        if image::encoding(|e| e.instance_refs.contains_key(&instance)) != Some(true) {
            let mut refs = Refs::default();
            apply.refs(&mut refs);
            let summary = RefsSummary::of(&refs);
            image::encoding(|e| e.instance_refs.insert(instance, summary))
                .ok_or(PackError::Application(image::NOT_IMAGED))?;
        }
        image::encoding(|e| f(&e.instance_refs[&instance]))
            .ok_or(PackError::Application(image::NOT_IMAGED))
    }

    /// Decode the instance the image holds for this site and bind it.
    fn materialize(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        let Callee::Imaged { instance, .. } = &self.callee else { return Ok(()) };
        let instance = *instance;
        let mut dec = ctx
            .image_decoder
            .take()
            .ok_or_else(|| anyhow!("no image to decode instance {instance:?} from"))?;
        let decoded = match dec.instance_offset(instance) {
            None => Err(anyhow!("instance {instance:?} is not in the image")),
            Some(at) => {
                let image = dec.image().clone();
                image::DecodeImage::with(&mut dec, || {
                    let mut sub = &image[at as usize..];
                    GXLambda::image_decode(ctx, &mut sub)
                        .map_err(|e| anyhow!("instance {instance:?} at {at}: {e:?}"))
                })
            }
        };
        ctx.image_decoder = Some(dec);
        let apply: Box<dyn Apply<R, E>> = Box::new(decoded?);
        let Callee::Imaged { first_update, .. } =
            mem::replace(&mut self.callee, Callee::DynamicUnbound)
        else {
            unreachable!()
        };
        self.callee = Callee::Static { apply, first_update };
        Ok(())
    }

    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<R, E>,
        buf: &mut &[u8],
    ) -> Result<Node<R, E>, PackError> {
        let spec = TArc::new(Expr::decode(buf)?);
        let ftype = Option::<FnType>::decode(buf)?;
        let rtype = Type::decode(buf)?;
        let fnode = decode_node(ctx, buf)?;
        let n = decode_varint(buf)? as usize;
        let mut args = ArgMap::with_capacity_and_hasher(n, Default::default());
        for _ in 0..n {
            let (key, arg) = Arg::image_decode(ctx, buf)?;
            args.insert(key, arg);
        }
        if !buf.has_remaining() {
            return Err(PackError::BufferShort);
        }
        let (arg_refs, callee) = match buf.get_u8() {
            CALLEE_UNBOUND => (Vec::new(), Callee::DynamicUnbound),
            CALLEE_INSTANCE => {
                let arg_refs = decode_nodes(ctx, buf)?;
                let callee = match bool::decode(buf)? {
                    false => {
                        let apply: Box<dyn Apply<R, E>> =
                            Box::new(GXLambda::image_decode(ctx, buf)?);
                        Callee::Static { apply, first_update: bool::decode(buf)? }
                    }
                    true => Callee::Imaged {
                        instance: LambdaInstanceId::decode(buf)?,
                        first_update: bool::decode(buf)?,
                        summary: RefsSummary::decode(buf)?,
                    },
                };
                (arg_refs, callee)
            }
            CALLEE_BUILTIN => {
                let arg_refs = decode_nodes(ctx, buf)?;
                let apply: Box<dyn Apply<R, E>> =
                    Box::new(BuiltInLambda::image_decode(ctx, &arg_refs, buf)?);
                (arg_refs, Callee::Static { apply, first_update: bool::decode(buf)? })
            }
            _ => return Err(PackError::UnknownTag),
        };
        let static_target = Option::<StaticCallTarget>::decode(buf)?;
        let lowered = opt_node_decode(ctx, buf)?;
        let recursive_edge = bool::decode(buf)?;
        let flags = image::flags_decode(buf)?;
        let scope = image::scope_decode(buf)?;
        let top_id = ExprId::decode(buf)?;
        let tail_arg_order = Option::<Vec<BindId>>::decode(buf)?;
        let mut site =
            Self::unbound(spec, ftype, rtype, fnode, args, scope, flags, top_id);
        site.arg_refs = arg_refs;
        site.callee = callee;
        site.static_target = static_target;
        site.lowered = lowered;
        site.recursive_edge = AtomicBool::new(recursive_edge);
        if let Some(order) = tail_arg_order {
            site.mark_self_tail_call(order.into_boxed_slice());
        }
        Ok(Node::new(site))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for CallSite<R, E> {
    fn image_len(&self) -> usize {
        let mode = self.callee_mode().unwrap_or(CALLEE_UNBOUND);
        let args: usize = self.args.iter().map(|(k, a)| a.image_len(k)).sum();
        let callee = match (&self.callee, mode) {
            (Callee::Static { apply, first_update }, CALLEE_INSTANCE) => {
                let deferred = image::encoding(|e| e.defer_instances).unwrap_or(false);
                let body = apply.image_len();
                let body = match apply.view() {
                    ApplyView::Lambda(g) if deferred => {
                        image::encoding(|e| e.deferred_len += body);
                        let summary =
                            Self::with_refs_summary(&**apply, |s| s.encoded_len());
                        g.instance_id().encoded_len() + summary.unwrap_or(0)
                    }
                    _ => body,
                };
                nodes_len(&self.arg_refs) + 1 + body + first_update.encoded_len()
            }
            (Callee::Static { apply, first_update }, CALLEE_BUILTIN) => {
                nodes_len(&self.arg_refs) + apply.image_len() + first_update.encoded_len()
            }
            _ => 0,
        };
        tag_len()
            + self.spec.encoded_len()
            + self.ftype.encoded_len()
            + self.rtype.encoded_len()
            + self.fnode.image_len()
            + varint_len(self.args.len() as u64)
            + args
            + 1
            + callee
            + self.static_target.encoded_len()
            + opt_node_len(self.lowered.as_ref())
            + 1
            + image::flags_len(self.flags)
            + image::scope_len(&self.scope)
            + self.top_id.encoded_len()
            + 1
            + self.tail_arg_order.get().map_or(0, |order| image::slice_len(order))
    }

    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        let mode = self.callee_mode()?;
        put_tag(NodeTag::CallSite, buf);
        self.spec.encode(buf)?;
        self.ftype.encode(buf)?;
        self.rtype.encode(buf)?;
        self.fnode.image_encode(buf)?;
        encode_varint(self.args.len() as u64, buf);
        for (k, a) in self.args.iter() {
            a.image_encode(k, buf)?;
        }
        buf.put_u8(mode);
        match (&self.callee, mode) {
            (Callee::Static { apply, first_update }, CALLEE_INSTANCE) => {
                encode_nodes(&self.arg_refs, buf)?;
                let deferred = image::encoding(|e| e.defer_instances).unwrap_or(false);
                deferred.encode(buf)?;
                if deferred {
                    let ApplyView::Lambda(g) = apply.view() else {
                        return Err(PackError::Application(image::NOT_IMAGED));
                    };
                    let instance = g.instance_id();
                    instance.encode(buf)?;
                    first_update.encode(buf)?;
                    Self::with_refs_summary(&**apply, |s| s.encode(buf))??;
                    // The session borrows every node it encodes for its
                    // whole length; the heap is written before it ends.
                    let body: &'static dyn Apply<R, E> =
                        unsafe { mem::transmute::<&dyn Apply<R, E>, _>(&**apply) };
                    image::encoding(|e| {
                        e.deferred
                            .push((instance, Box::new(move |buf| body.image_encode(buf))))
                    });
                } else {
                    apply.image_encode(buf)?;
                    first_update.encode(buf)?;
                }
            }
            (Callee::Static { apply, first_update }, CALLEE_BUILTIN) => {
                encode_nodes(&self.arg_refs, buf)?;
                apply.image_encode(buf)?;
                first_update.encode(buf)?;
            }
            _ => (),
        }
        self.static_target.encode(buf)?;
        opt_node_encode(self.lowered.as_ref(), buf)?;
        self.recursive_edge.load(Relaxed).encode(buf)?;
        image::flags_encode(self.flags, buf)?;
        image::scope_encode(&self.scope, buf)?;
        self.top_id.encode(buf)?;
        match self.tail_arg_order.get() {
            None => buf.put_u8(0),
            Some(order) => {
                buf.put_u8(1);
                image::slice_encode(order, buf)?;
            }
        }
        Ok(())
    }

    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> &TagValue {
        match self.lowered.is_some() {
            true => self.lowered.as_mut().unwrap().update(ctx, event),
            false => self.update_call(ctx, event),
        }
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        if let Some(mut n) = self.lowered.take() {
            n.delete(ctx);
            return;
        }
        if let Some(mut f) = self.callee.take_apply() {
            f.delete(ctx)
        }
        self.fnode.delete(ctx);
        for arg in self.args.values_mut() {
            ctx.rt.store_remove(&arg.id);
            if let Some(ref mut n) = arg.node {
                n.delete(ctx);
            }
        }
        for n in &mut self.arg_refs {
            n.delete(ctx);
        }
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.slept.set();
        if let Some(n) = &mut self.lowered {
            return n.sleep(ctx);
        }
        // A recursive edge deselected by a shrink is deleted, so
        // re-reaching this depth binds a fresh activation.
        if ctx.deselecting_arm && self.is_recursive_edge() {
            if let Some(mut f) = self.callee.take_apply() {
                f.delete(ctx)
            }
        } else if let Some(f) = self.callee.apply_mut() {
            f.sleep(ctx)
        }
        self.fnode.sleep(ctx);
        for arg in self.args.values_mut() {
            if let Some(ref mut n) = arg.node {
                n.sleep(ctx);
            }
        }
        for n in &mut self.arg_refs {
            n.sleep(ctx);
        }
    }

    fn reset_replay(&mut self, ctx: &mut ExecCtx<R, E>) {
        if let Some(n) = &mut self.lowered {
            return n.reset_replay(ctx);
        }
        if let Some(f) = self.callee.apply_mut() {
            f.reset_replay(ctx)
        }
        self.fnode.reset_replay(ctx);
        for arg in self.args.values_mut() {
            if let Some(ref mut n) = arg.node {
                n.reset_replay(ctx);
            }
        }
        for n in &mut self.arg_refs {
            n.reset_replay(ctx);
        }
    }

    fn typ(&self) -> &Type {
        match &self.lowered {
            Some(n) => n.typ(),
            None => &self.rtype,
        }
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn typecheck0(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        if let Some(n) = &mut self.lowered {
            return n.typecheck0(ctx);
        }
        wrap!(self.fnode, self.fnode.typecheck0(ctx))?;
        let ftype = match self.ftype.as_ref() {
            Some(ftype) => ftype, // already initialized
            None => {
                let ftype = deref_typ!("fn", ctx, self.fnode.typ(),
                    Some(Type::Fn(ftype)) => Ok(ftype.clone())
                )?;
                // A self-call inside the def-time body check unifies against
                // the def's own cells (`ExecCtx::rec_defs`).
                let is_rec_self_call = !ctx.rec_defs.is_empty()
                    && ftype.lambda_ids.ids().iter().any(|id| ctx.rec_defs.contains(id));
                let identity = self.fn_arg_identity(ctx);
                let active_ftype = ftype
                    .lambda_ids
                    .own()
                    .and_then(|id| ctx.resolving(id, &identity))
                    .map(|active| active.ftype);
                // A call to the enclosing def's fn-typed param during its gate.
                let is_param_knot = !ctx.def_gate_params.is_empty()
                    && matches!(
                        self.fnode.view(),
                        NodeView::Ref(r) if ctx.def_gate_params.contains(&r.id)
                    );
                let ftype = if let Some(active) = active_ftype {
                    active
                } else if is_rec_self_call || is_param_knot {
                    // A shallow clone shares the def's TVar cells.
                    (*ftype).clone()
                } else {
                    let ftype = ftype.reset_tvars();
                    ftype.alias_tvars(&mut LPooled::take());
                    ftype
                };
                self.ftype = Some(ftype.clone());
                let ftype = self.ftype.as_ref().unwrap();
                for arg in ftype.args.iter() {
                    if let FnArgKind::Labeled { name, has_default } = &arg.kind {
                        match self.args.entry(ArgKey::Named(name.clone())) {
                            ArgEntry::Occupied(_) => (),
                            ArgEntry::Vacant(e) if *has_default => {
                                let nop = Nop::new(arg.typ.clone());
                                e.insert(Arg::new(BindId::new(), Some(nop), true));
                            }
                            ArgEntry::Vacant(_) => {
                                bail!("missing required argument {name}")
                            }
                        }
                    }
                }
                for key in self.args.keys() {
                    if let ArgKey::Named(name) = key
                        && !ftype.args.iter().any(|a| a.label() == Some(name))
                    {
                        bail!("unknown labeled argument {name}")
                    }
                }
                let required = ftype.args.iter().filter(|a| a.is_positional()).count();
                let provided = self
                    .args
                    .keys()
                    .filter(|k| matches!(k, ArgKey::Positional(_)))
                    .count();
                if provided < required {
                    bail!(
                        "missing required argument: expected {required} positional, \
                         received {provided}"
                    )
                }
                if provided > required && ftype.vargs.is_none() {
                    bail!(
                        "too many positional arguments, expected {required}, received {provided}"
                    )
                }
                ftype
            }
        };
        for (farg, key) in ftype.args.iter().zip(ArgKey::of_formals(&ftype.args)) {
            if let Some(n) = self.args.get_mut(&key).and_then(|a| a.node.as_mut()) {
                typecheck_arg(ctx, &farg.typ, n)?;
            }
        }
        if let Some(typ) = &ftype.vargs {
            let positional = ftype.args.iter().filter(|a| a.is_positional()).count();
            for key in ArgKey::variadic(positional) {
                let Some(arg) = self.args.get_mut(&key) else { break };
                if let Some(n) = arg.node.as_mut() {
                    typecheck_arg(ctx, typ, n)?;
                }
            }
        }
        // A constrained cell reachable from the rtype/throws but no arg is
        // produced by the callee's body: settle it to its witness before an
        // annotation could narrow it unsoundly.
        {
            let mut arg_tvs: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
            for a in ftype.args.iter() {
                a.typ.collect_tvars(&mut arg_tvs);
            }
            if let Some(t) = &ftype.vargs {
                t.collect_tvars(&mut arg_tvs);
            }
            let arg_cells: LPooled<AHashSet<usize>> =
                arg_tvs.drain().map(|(_, tv)| tv.cell_addr()).collect();
            let mut rt_tvs: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
            ftype.rtype.collect_tvars(&mut rt_tvs);
            ftype.throws.collect_tvars(&mut rt_tvs);
            for (_, tv) in rt_tvs.drain() {
                if !arg_cells.contains(&tv.cell_addr()) {
                    wrap!(self, tv.settle(&ctx.env))?;
                }
            }
        }
        if let Some(t) = ftype.throws.deref_cloned() {
            match self.scope.dynamic.catch() {
                Some((id, _)) => join_raised(&ctx.env, id, &t)?,
                // it doesn't throw any errors
                None if t == Type::Bottom => (),
                None => Qop::<R, E>::check_unhandled(
                    &ctx.env,
                    self.flags,
                    &self.spec,
                    format_args!(
                        "error {t} raised from function call {}",
                        self.fnode.spec()
                    ),
                )?,
            }
        }
        wrap!(self.fnode, self.rtype.check_contains(&ctx.env, &ftype.rtype))?;
        Ok(())
    }

    /// Second pass: after the subtrees, drive `Apply::typecheck1` for every
    /// lambda dispatchable here (the callee and each fn-typed callback).
    fn typecheck1(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        if let Some(n) = &mut self.lowered {
            return n.typecheck1(ctx);
        }
        wrap!(self.fnode, self.fnode.typecheck1(ctx))?;
        for arg in self.args.values_mut() {
            if let Some(n) = arg.node.as_mut() {
                wrap!(n, n.typecheck1(ctx))?;
            }
        }
        let ftype = match self.ftype.as_ref() {
            Some(ftype) => ftype.clone(),
            None => return Ok(()),
        };
        // A settle frame for this site's re-drives; leftovers merge up and
        // drain only after this site's writers have run.
        ctx.pending_settles.push(Vec::new());
        let res = self.typecheck1_resolve(ctx, &ftype);
        let leftover = ctx.pending_settles.pop().expect("settle frame");
        ctx.pending_settles.last_mut().expect("root settle frame").extend(leftover);
        res?;
        // Terminal settle of still-unbound constrained cells, deferred to
        // the statement boundary. Cells reachable from an omitted defaulted
        // arg are exempt: the default expression binds them at static
        // resolution.
        {
            let mut dtv: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
            for farg in ftype.args.iter() {
                if let FnArgKind::Labeled { name, .. } = &farg.kind
                    && let Some(a) = self.args.get(&ArgKey::Named(name.clone()))
                    && a.is_default
                {
                    farg.typ.collect_tvars(&mut dtv);
                }
            }
            let defaulted: AHashSet<usize> =
                dtv.drain().map(|(_, tv)| tv.cell_addr()).collect();
            // The call's own result cell joins the settle set: a literal ⊥
            // rtype unifies without binding it.
            let rtc = match &self.rtype {
                Type::TVar(tv) => Some(tv.clone()),
                _ => None,
            };
            ctx.pending_settles.last_mut().expect("root settle frame").push(
                crate::PendingSettle {
                    ftype: ftype.clone(),
                    rtype: rtc,
                    defaulted,
                    spec: self.spec.clone(),
                },
            );
        }
        Ok(())
    }

    fn refs(&self, refs: &mut Refs) {
        if let Some(n) = &self.lowered {
            return n.refs(refs);
        }
        if let Some(fun) = self.callee.apply() {
            fun.refs(refs)
        }
        if let Callee::Imaged { summary, .. } = &self.callee {
            summary.add_to(refs);
        }
        self.fnode.refs(refs);
        for arg in self.args.values() {
            refs.bound.insert(arg.id);
            if let Some(ref n) = arg.node {
                n.refs(refs);
            }
        }
        for n in &self.arg_refs {
            n.refs(refs);
        }
    }

    fn view(&self) -> NodeView<'_, R, E> {
        match &self.lowered {
            Some(n) => n.view(),
            None => NodeView::CallSite(self),
        }
    }

    fn fuse(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<Option<Node<R, E>>> {
        if let Some(n) = &mut self.lowered {
            return n.fuse(ctx);
        }
        // Reached when this call did not inline. Descend via `Update::fuse`
        // (not `fusion::fuse`, which would fuse constant args into 0-input
        // kernels), then give the callee its hook.
        for arg in self.args.values_mut() {
            if let Some(node) = &mut arg.node {
                if let Some(new) = node.fuse(ctx)? {
                    let mut old = mem::replace(node, new);
                    old.delete(ctx);
                }
            }
        }
        if let Some(apply) = self.callee.apply_mut() {
            apply.fuse(ctx)?;
        }
        Ok(None)
    }

    fn emit_clif(&self, cx: &mut BodyCx) -> Result<CompiledExpr> {
        if let Some(n) = &self.lowered {
            return n.emit_clif(cx);
        }
        if let Some(f) = self.callee.apply() {
            if let Some(cv) = f.emit_clif(self, cx)? {
                return Ok(cv);
            }
            // A resolved lambda callee is a cross-kernel call; an
            // undiscovered site de-fuses.
            if matches!(f.view(), ApplyView::Lambda(_)) {
                if let Some(info) = cx.lambda_site(self.spec.id).cloned() {
                    return emit_lambda_call_node(cx, self, &info, false);
                }
                return Err(fusion::blocker(
                    &self.spec,
                    format_compact!(
                        "emit_clif: lambda call site `{}` not discovered — \
                         subtree node-walks",
                        self.spec
                    ),
                ));
            }
        }
        // A value-position self-call: `self.callee` is unresolved here, so
        // match by the self BindId and call the kernel's own FuncRef.
        if let Some((sb, info)) = cx.self_call_info() {
            let is_self = matches!(
                self.fnode.view(),
                NodeView::Ref(r) if r.id == *sb
            );
            if is_self {
                let info = info.clone();
                return emit_lambda_call_node(cx, self, &info, true);
            }
        }
        if self.is_recursive_edge() {
            bail!("emit_clif: mutually recursive static call edge is not supported")
        }
        // A `MarshalArg::Call(i)` indexes the source-order arg list, which
        // spans labeled and positional args; `Default(name)` is this site's
        // compiled default node.
        let info = match cx.builtin_site(self.spec.id) {
            Some(info) => info.clone(),
            None => {
                return Err(fusion::blocker(
                    &self.spec,
                    format_compact!(
                        "emit_clif: builtin call site `{}` not discovered — doesn't fuse",
                        self.spec
                    ),
                ));
            }
        };
        let written = self.spec_args();
        let source_nodes = ArgKey::of_written(written)
            .map(|key| {
                self.arg(&key)
                    .ok_or_else(|| anyhow!("emit_clif: missing call-site arg node"))
            })
            .collect::<Result<SmallVec<[&Node<R, E>; 8]>>>()?;
        let arg_nodes = info
            .marshal_args
            .iter()
            .map(|m| match m {
                MarshalArg::Call(call_idx) => {
                    source_nodes.get(*call_idx).copied().ok_or_else(|| {
                        anyhow!("emit_clif: marshal arg index {call_idx} out of range")
                    })
                }
                MarshalArg::Default(name) => self.arg_named(name).ok_or_else(|| {
                    anyhow!("emit_clif: defaulted arg `{name}` has no compiled node")
                }),
            })
            .collect::<Result<SmallVec<[_; 8]>>>()?;
        emit_builtin_call_node(cx, &info, &arg_nodes)
    }
}

/// The bind ids a production feeds: one call argument's id, or a
/// formal's pattern ids.
pub(crate) enum Feeds<'a> {
    Id(BindId),
    Pattern(&'a StructPatternNode),
}

/// What a quiet production does at depth 0, where the store serves the
/// value channel.
#[derive(Clone, Copy)]
pub(crate) enum QuietAtRoot {
    /// Nothing: the store already holds it.
    Skip,
    /// Stand it in the store: a wake refresh, a fresh formal's seed.
    Stand,
    /// Deliver it on the overlay.
    Deliver,
}

/// Publish `tv`, a production feeding `feeds`, to the readers of this
/// dispatch. A fire (a value or a fresh bottom) is stored, at depth 0
/// only, and delivered on the overlay. A quiet one is delivered on the
/// overlay in a frame, whose store holds the pre-frame value, and per
/// `root` at depth 0. `born` delivers a quiet value FIRED: a fresh
/// callee's first dispatch reads its arguments as new. Returns whether
/// the overlay was written.
pub(crate) fn publish_production<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<R, E>,
    event: &mut Event<E>,
    feeds: Feeds<'_>,
    tv: &TagValue,
    born: bool,
    root: QuietAtRoot,
) -> bool {
    let tag = tv.tag();
    let bottom = tag.is_bottom();
    let delivered = if born && !bottom { Tag::FIRED } else { tag };
    let (store, overlay) = if tag.triggers() {
        let store = if bottom { Tag::FRESH_BOTTOM } else { Tag::FIRED };
        ((ctx.frame_depth == 0).then_some((store, false)), Some(tag))
    } else if ctx.frame_depth > 0 {
        (None, Some(delivered))
    } else {
        match root {
            QuietAtRoot::Skip => return false,
            QuietAtRoot::Stand => (Some((tag, true)), None),
            QuietAtRoot::Deliver => (None, Some(delivered)),
        }
    };
    let mut put = |id: BindId, v: Value| {
        match store {
            None => (),
            Some((t, false)) => ctx.rt.store_insert(id, TagValue::tagged(v.clone(), t)),
            Some((t, true)) => {
                ctx.rt.store_insert_standing(id, TagValue::tagged(v.clone(), t))
            }
        }
        if let Some(t) = overlay {
            event.variables.insert(id, TagValue::tagged(v, t));
        }
    };
    match (feeds, bottom) {
        (Feeds::Id(id), true) => put(id, Value::Null),
        (Feeds::Id(id), false) => put(id, tv.value_cloned()),
        (Feeds::Pattern(pat), true) => pat.ids(&mut |id| put(id, Value::Null)),
        (Feeds::Pattern(pat), false) => {
            let v = tv.value_cloned();
            pat.bind(&v, &mut |id, v| put(id, v))
        }
    }
    overlay.is_some()
}
