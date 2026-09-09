use super::{NOP, Nop, WakeBit, bind::Ref, compiler::compile};
use crate::{
    Apply, ApplyView, ApplyViewMut, BindId, BindMode, CFlag, Event, ExecCtx, LambdaId,
    LambdaInstanceId, Node, NodeView, PendingTailCall, PrintFlag, Refs, Rt, Scope, Tag,
    TagValue, Update, UserEvent, deref_typ,
    env::TraitMethodRef,
    expr::{ErrorContext, Expr, ExprId, ExprKind, ModPath},
    fusion::{
        self,
        emit::{BodyCx, CompiledExpr, emit_builtin_call_node, emit_lambda_call_node},
        lowering::MarshalArg,
    },
    node::lambda::LambdaDef,
    profile::{self, Phase},
    typ::{FnArgKind, FnType, TVar, Type},
    wrap,
};
use ahash::{AHashMap, AHashSet};
use anyhow::{Context, Result, anyhow, bail};
use arcstr::ArcStr;
use enumflags2::BitFlags;
use indexmap::IndexMap;
use netidx_value::Value;
use parking_lot::Mutex;
use poolshark::local::LPooled;
use smallvec::SmallVec;
use std::{
    collections::hash_map::Entry,
    mem,
    sync::atomic::{AtomicBool, Ordering},
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
        || !ctx.builtin_effect(info.name.as_str()).is_sync()
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

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub(crate) enum ArgKey {
    Positional(usize),
    Named(ArcStr),
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

fn finalize_lambda<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<R, E>,
    id: LambdaId,
    resolved: &FnType,
    spec: &TArc<Expr>,
) -> Result<()> {
    let _profile = profile::phase(Phase::LambdaFinalize);
    if let Some(val) = ctx.lambda_defs.get(&id).cloned() {
        let ldef = val
            .downcast_ref::<LambdaDef<R, E>>()
            .expect("failed to unwrap lambda for typecheck1");
        if let Some(apply) = &mut *ldef.check.lock() {
            apply
                .typecheck1(ctx, &mut [], resolved)
                .with_context(|| ErrorContext((**spec).clone()))?;
        }
    }
    Ok(())
}

fn compile_apply_args<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<R, E>,
    flags: BitFlags<CFlag>,
    scope: &Scope,
    top_id: ExprId,
    args: &TArc<[(Option<ArcStr>, Expr)]>,
) -> Result<ArgMap<R, E>> {
    let mut res = ArgMap::default();
    let mut pos = 0;
    for (name, expr) in args.iter() {
        let node = Some(compile(ctx, flags, expr.clone(), scope, top_id)?);
        match name {
            None => {
                res.insert(ArgKey::Positional(pos), Arg::new(BindId::new(), node, false));
                pos += 1;
            }
            Some(k) => match res.entry(ArgKey::Named(k.clone())) {
                indexmap::map::Entry::Occupied(_) => {
                    bail!("duplicate named argument {k}")
                }
                indexmap::map::Entry::Vacant(e) => {
                    e.insert(Arg::new(BindId::new(), node, false));
                }
            },
        }
    }
    Ok(res)
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
    /// Pre-bound at compile time by [`CallSite::try_static_resolve`]; the
    /// per-cycle identity check is skipped (`fnode.update()` still runs
    /// for effects). `first_update` primes the body's refs once.
    Static { apply: Box<dyn Apply<R, E>>, resolved_ftype: FnType, first_update: bool },
}

#[derive(Debug, Clone)]
pub(crate) struct StaticCallTarget {
    pub definition: LambdaId,
    pub instance: LambdaInstanceId,
    pub ftype: FnType,
}

impl<R: Rt, E: UserEvent> Callee<R, E> {
    fn is_bound(&self) -> bool {
        !matches!(self, Callee::DynamicUnbound)
    }

    fn apply(&self) -> Option<&dyn Apply<R, E>> {
        match self {
            Callee::DynamicUnbound => None,
            Callee::DynamicBound { apply, .. } | Callee::Static { apply, .. } => {
                Some(&**apply)
            }
        }
    }

    fn apply_mut(&mut self) -> Option<&mut (dyn Apply<R, E> + 'static)> {
        match self {
            Callee::DynamicUnbound => None,
            Callee::DynamicBound { apply, .. } | Callee::Static { apply, .. } => {
                Some(&mut **apply)
            }
        }
    }

    /// Reset to `DynamicUnbound`, returning the bound apply for deletion.
    fn take_apply(&mut self) -> Option<Box<dyn Apply<R, E>>> {
        match mem::replace(self, Callee::DynamicUnbound) {
            Callee::DynamicUnbound => None,
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
    pub(super) callee_is_builtin: bool,
    pub(crate) static_target: Option<StaticCallTarget>,
    /// A trait call over a union self type lowered to a select, one
    /// static call per member; once set every `Update` method delegates.
    pub(crate) lowered: Option<Node<R, E>>,
    pub(crate) recursive_edge: AtomicBool,
    pub(super) flags: BitFlags<CFlag>,
    pub(super) scope: Scope,
    pub(super) top_id: ExprId,
    /// Set by `analysis::analyze` when this is a tail-position self-call in
    /// a sync tail-recursive body; `update` then stashes its args in
    /// `ctx.pending_tail_call` instead of dispatching.
    pub(crate) is_self_tail_call: AtomicBool,
    /// The rebind args in callee-signature order. `Some` iff
    /// `is_self_tail_call`.
    pub(crate) tail_arg_order: Mutex<Option<Box<[BindId]>>>,
    /// The loop key `GXLambda::update` matches `ctx.pending_tail_call`
    /// against. `Some` iff `is_self_tail_call`.
    pub(crate) callee_lambda_id: Mutex<Option<LambdaId>>,
    pub(super) resident: TagValue,
}

impl<R: Rt, E: UserEvent> CallSite<R, E> {
    /// The function type at this call site with the site's tvars unified
    /// in. `None` before typecheck, or if this site errored first.
    pub fn ftype(&self) -> Option<&FnType> {
        self.ftype.as_ref()
    }

    /// The detached, resolved function type owned by a statically-bound
    /// callee instance.
    pub fn resolved_ftype(&self) -> Option<&FnType> {
        if let Some(target) = &self.static_target {
            return Some(&target.ftype);
        }
        match &self.callee {
            Callee::Static { resolved_ftype, .. } => Some(resolved_ftype),
            Callee::DynamicUnbound | Callee::DynamicBound { .. } => None,
        }
    }

    pub(crate) fn static_target(&self) -> Option<&StaticCallTarget> {
        self.static_target.as_ref()
    }

    pub(crate) fn is_recursive_edge(&self) -> bool {
        self.recursive_edge.load(Ordering::Relaxed)
    }

    pub(crate) fn set_recursive_edge(&self, recursive: bool) {
        self.recursive_edge.store(recursive, Ordering::Relaxed)
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
        self.args.get(&ArgKey::Positional(idx)).and_then(|a| a.node.as_ref())
    }

    /// Look up a labeled argument's compiled sub-Node.
    pub fn arg_named(&self, name: &ArcStr) -> Option<&Node<R, E>> {
        self.args.get(&ArgKey::Named(name.clone())).and_then(|a| a.node.as_ref())
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

    /// The resolved callee as a raw `&dyn Apply`.
    pub fn callee_apply(&self) -> Option<&dyn Apply<R, E>> {
        self.callee.apply()
    }

    /// Mutable counterpart to [`Self::resolved_apply`].
    pub fn resolved_apply_mut(&mut self) -> Option<ApplyViewMut<'_, R, E>> {
        self.callee.apply_mut().map(|a| a.view_mut())
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
        reject_dead_variadic_call(ctx, scope, f, args)?;
        let fnode = compile(ctx, flags, (**f).clone(), scope, top_id)?;
        let spec = TArc::new(spec);
        let args = compile_apply_args(ctx, flags, scope, top_id, args)?;
        let site = Self {
            slept: WakeBit::default(),
            spec,
            ftype: None,
            rtype: Type::empty_tvar(),
            fnode,
            args,
            arg_refs: Vec::new(),
            callee: Callee::DynamicUnbound,
            callee_is_builtin: false,
            static_target: None,
            lowered: None,
            recursive_edge: AtomicBool::new(false),
            flags,
            top_id,
            scope: scope.clone(),
            is_self_tail_call: AtomicBool::new(false),
            tail_arg_order: Mutex::new(None),
            callee_lambda_id: Mutex::new(None),
            resident: TagValue::phantom(),
        };
        Ok(Node::new(site))
    }

    fn make_ref(&self, id: BindId, typ: Type, spec: TArc<Expr>) -> Node<R, E> {
        Node::new(Ref {
            spec,
            typ,
            id,
            top_id: self.top_id,
            resident: TagValue::phantom(),
            instantiated: false,
        })
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
        let mut pos_idx = 0;
        for (i, farg) in f.typ.args.iter().enumerate() {
            if let FnArgKind::Labeled { name, has_default: default } = &farg.kind {
                match self.args.get(&ArgKey::Named(name.clone())) {
                    Some(arg) => {
                        let typ = arg
                            .node
                            .as_ref()
                            .map(|n| n.typ().clone())
                            .unwrap_or_else(|| farg.typ.clone());
                        let spec = arg
                            .node
                            .as_ref()
                            .map(|n| TArc::new(n.spec().clone()))
                            .unwrap_or_else(|| NOP.clone());
                        self.arg_refs.push(self.make_ref(arg.id, typ, spec));
                    }
                    None if *default => {
                        let id = BindId::new();
                        let mut default_node = match &f.argspec[i].labeled {
                            None | Some(None) => {
                                bail!("expected default value")
                            }
                            Some(Some(expr)) => {
                                ctx.with_restored(f.env.clone(), |ctx| {
                                    let local_scope = Scope {
                                        dynamic: scope.dynamic.clone(),
                                        lexical: f.scope.lexical.clone(),
                                    };
                                    let n = compile(
                                        ctx,
                                        flags,
                                        expr.clone(),
                                        &local_scope,
                                        self.top_id,
                                    )?;
                                    let mut refs = Refs::default();
                                    n.refs(&mut refs);
                                    prime_default_refs(ctx, &refs);
                                    Ok::<_, anyhow::Error>(n)
                                })?
                            }
                        };
                        // A default typechecks against this site's instantiated
                        // signature, so an omitting site infers from it.
                        wrap!(default_node, default_node.typecheck0(ctx))?;
                        let typ = default_node.typ().clone();
                        if let Some(site) = self.ftype.as_ref() {
                            if let Some(sarg) = site.args.get(i) {
                                wrap!(
                                    default_node,
                                    sarg.typ.check_contains(&ctx.env, &typ)
                                )?;
                            }
                        }
                        let spec = TArc::new(default_node.spec().clone());
                        self.args.insert(
                            ArgKey::Named(name.clone()),
                            Arg::new(id, Some(default_node), true),
                        );
                        self.arg_refs.push(self.make_ref(id, typ, spec));
                    }
                    None => bail!("BUG: in bind missing required argument {name}"),
                }
            } else {
                let key = loop {
                    let candidate = ArgKey::Positional(pos_idx);
                    pos_idx += 1;
                    if self.args.contains_key(&candidate) {
                        break candidate;
                    }
                    if pos_idx > self.args.len() + f.typ.args.len() {
                        bail!("missing required positional argument {i}")
                    }
                };
                let arg = &self.args[&key];
                let typ = arg
                    .node
                    .as_ref()
                    .map(|n| n.typ().clone())
                    .unwrap_or_else(|| farg.typ.clone());
                let spec = arg
                    .node
                    .as_ref()
                    .map(|n| TArc::new(n.spec().clone()))
                    .unwrap_or_else(|| NOP.clone());
                self.arg_refs.push(self.make_ref(arg.id, typ, spec));
            }
        }
        if f.typ.vargs.is_some() {
            loop {
                let key = ArgKey::Positional(pos_idx);
                pos_idx += 1;
                match self.args.get(&key) {
                    Some(arg) => {
                        let typ = arg
                            .node
                            .as_ref()
                            .map(|n| n.typ().clone())
                            .unwrap_or_else(|| Type::Bottom);
                        let spec = arg
                            .node
                            .as_ref()
                            .map(|n| TArc::new(n.spec().clone()))
                            .unwrap_or_else(|| NOP.clone());
                        self.arg_refs.push(self.make_ref(arg.id, typ, spec));
                    }
                    None => break,
                }
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

    /// Re-read the bound instance's resolved ftype into `static_target`
    /// and `Callee::Static`. `None` when no apply is bound.
    fn refresh_static_ftype(&mut self) -> Option<FnType> {
        let ftype = self.instance_ftype()?;
        if let Some(target) = &mut self.static_target {
            target.ftype = ftype.clone();
        }
        if let Callee::Static { resolved_ftype, .. } = &mut self.callee {
            *resolved_ftype = ftype.clone();
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
            finalize_lambda::<R, E>(ctx, id, &resolved, &spec)?;
        }
        // Callbacks reachable through a fn-typed argument.
        let mut fts: LPooled<Vec<TArc<FnType>>> = LPooled::take();
        for arg in resolved.args.iter() {
            fts.clear();
            collect_fn_arms(&arg.typ, &mut fts);
            for ft in fts.iter() {
                for id in ft.lambda_ids.ids().iter().copied() {
                    finalize_lambda::<R, E>(ctx, id, ft, &spec)?;
                }
            }
        }
        // Runs after static resolution replaced the Nop placeholders with
        // the compiled defaults; a dynamic site's Nops make it vacuous.
        if ftype.lambda_ids.ids().len() == 1 {
            for farg in ftype.args.iter() {
                let name = match &farg.kind {
                    FnArgKind::Labeled { name, has_default: true } => name,
                    _ => continue,
                };
                let def_typ = match self.args.get(&ArgKey::Named(name.clone())) {
                    Some(a) if a.is_default => a.node.as_ref().map(|n| n.typ().clone()),
                    _ => continue,
                };
                if let Some(dt) = def_typ {
                    wrap!(self.fnode, farg.typ.check_contains(&ctx.env, &dt))?;
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
        let same_shape = site_ftype.args.len() == f.typ.args.len()
            && site_ftype
                .args
                .iter()
                .zip(f.typ.args.iter())
                .all(|(site, definition)| site.kind == definition.kind);
        let instance_ftype = if same_shape {
            site_ftype.clone()
        } else {
            let definition_ftype = f.typ.reset_tvars();
            definition_ftype.alias_tvars(&mut LPooled::take());
            site_ftype.check_contains(&ctx.env, &definition_ftype)?;
            definition_ftype.resolve_tvars()
        };
        let apply = self.init_prepared_bind(
            ctx,
            scope,
            f,
            BindMode::Static { instance: &instance_ftype, site: &site_ftype },
        )?;
        let instance_ftype = apply.typ().as_ref().clone();
        // `site_ftype` is a deep clone: the instance's inferred return
        // must be unified back into the site's live rtype cell.
        if let Some(site_ft) = self.ftype.as_ref() {
            wrap!(
                self.fnode,
                site_ft.rtype.check_contains(&ctx.env, &instance_ftype.rtype)
            )?;
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
        let _bind_span = crate::perfdbg::span(&crate::perfdbg::BIND_NS);
        if crate::perfdbg::enabled() {
            crate::perfdbg::BIND_CALLS.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        }
        let setup_span = crate::perfdbg::span(&crate::perfdbg::SETUP_NS);
        // Prime each fresh default's external refs so the bound body
        // sees outer values on its first update in this cycle.
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
        self.callee_is_builtin = matches!(apply.view(), ApplyView::BuiltIn);
        self.callee = Callee::DynamicBound { def: fv, apply };
        // The publish loop ran before the callee was known; retract the
        // poisoned deliveries the gate would have silenced.
        if self.callee_is_builtin {
            for arg in self.args.values() {
                if event.variables.get(&arg.id).is_some_and(|tv| tv.is_bottom()) {
                    event.variables.remove(&arg.id);
                }
            }
        }
        // The lazy-bound body postdates the program-wide typecheck1 and
        // analysis passes: resolve its call sites and analyze it here.
        let identity = self.fn_arg_identity(ctx);
        if let Some(apply) = self.callee.apply_mut()
            && matches!(apply.view(), ApplyView::Lambda(_))
        {
            let instance = match apply.view() {
                ApplyView::Lambda(g) => g.instance_id(),
                ApplyView::BuiltIn => unreachable!(),
            };
            let instance_ftype = apply.typ();
            // A recursive lazy bind: its body stays lazy.
            let already_active = ctx.resolving(f.id, &identity).is_some();
            ctx.push_resolving(
                f.id,
                crate::ResolvingLambda {
                    instance,
                    ftype: instance_ftype.as_ref().clone(),
                    identity,
                },
            );
            if !already_active {
                let _tc1_span = crate::perfdbg::span(&crate::perfdbg::TC1_NS);
                if let Err(e) = apply.typecheck1(ctx, &mut [], &instance_ftype) {
                    if crate::dbgenv::gxdbg_swallow() {
                        eprintln!("SWALLOWED-LAZY-TC1 at {}: {e:#}", self.spec);
                    }
                    log::trace!("bind: lazy-bound callee body typecheck1 failed: {e:#}");
                }
            }
            ctx.pop_resolving(f.id, instance);
            if let ApplyView::Lambda(g) = apply.view() {
                let _an_span = crate::perfdbg::span(&crate::perfdbg::ANALYZE_NS);
                let self_bind = match self.fnode.view() {
                    NodeView::Ref(r) => Some(r.id),
                    _ => None,
                };
                crate::analysis::analyze_bound_callee(g, self_bind, ctx);
            }
        }
        // Defaults update for the first time under the init view.
        let prev_init = mem::replace(&mut event.init, true);
        for arg in self.args.values_mut() {
            if arg.is_default {
                if let Some(ref mut node) = arg.node {
                    let tv = node.update(ctx, event);
                    if tv.tag().triggers() && !tv.tag().is_bottom() {
                        let v = tv.value_cloned();
                        if ctx.frame_depth == 0 {
                            ctx.rt.store_insert(arg.id, TagValue::fired(v.clone()));
                        }
                        event.variables.insert(arg.id, TagValue::fired(v));
                        set.push(arg.id);
                    }
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
        self.callee_is_builtin = matches!(apply.view(), ApplyView::BuiltIn);
        self.callee = Callee::Static {
            apply,
            resolved_ftype: instance_ftype.clone(),
            first_update: true,
        };
        // Fn-typed args are registered under the instance's param
        // BindIds for the whole body typecheck (`register_fn_params`).
        let (param_binds, trait_param_binds) =
            self.register_fn_params(ctx, &instance_ftype);
        if let Some(instance) = instance {
            ctx.push_resolving(
                def.id,
                crate::ResolvingLambda {
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
                if crate::dbgenv::gxdbg_resolve() {
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

    /// This site's instantiation identity ([`crate::FnArgIdentity`]): per
    /// argument, the source lambda it resolves to (a literal is its own
    /// source; a `Ref` goes through `bind_to_lambda`; a `<-` target is
    /// dynamic).
    fn fn_arg_identity(&self, ctx: &ExecCtx<R, E>) -> crate::FnArgIdentity {
        self.args
            .values()
            .map(|arg| {
                let node = arg.node.as_ref()?;
                match node.view() {
                    NodeView::Lambda(l) => Some(l.source_id()),
                    NodeView::Ref(r) if !ctx.batch_connect_targets.contains(&r.id) => ctx
                        .bind_to_lambda
                        .get(&r.id)
                        .and_then(|fv| fv.downcast_ref::<LambdaDef<R, E>>())
                        .map(|def| def.source),
                    _ => None,
                }
            })
            .collect()
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
        for (i, farg) in ftype.args.iter().enumerate() {
            if !farg.typ.with_deref(|t| matches!(t, Some(Type::Fn(_)))) {
                continue;
            }
            let Some(id) = g.args().get(i).and_then(|p| p.single_bind_id()) else {
                continue;
            };
            let Some(arg_node) = self.arg_positional(i) else { continue };
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

    /// Resolve a trait method call to an implementation by the self
    /// argument's type. An open self type is an error outside a
    /// definition gate; a union self type lowers to a select.
    fn resolve_trait_call(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        tm: TraitMethodRef,
    ) -> Result<()> {
        let Some(def) = ctx.env.trait_def(tm.trait_id).cloned() else {
            bail!("trait method call through an unknown trait at {}", self.spec)
        };
        let m = &def.methods[tm.index];
        let Some(ftype) = self.ftype.as_ref() else { return Ok(()) };
        // Dispatch reasons per union member, so the self type must be in
        // union normal form with its cells settled first.
        let mut self_t = match ftype.args.get(m.self_index) {
            Some(a) => {
                {
                    let mut tvs: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
                    a.typ.collect_tvars(&mut tvs);
                    for (_, tv) in tvs.drain() {
                        wrap!(self, tv.settle_or_bottom(&ctx.env))?;
                    }
                }
                a.typ.resolve_tvars().normalize()
            }
            None => bail!("{}::{} called without its self argument", def.name, m.name),
        };
        if !def.hole {
            while let Type::Ref(tr) = &self_t
                && ctx.env.trait_of_ref(tr).is_none()
            {
                self_t = self_t.lookup_ref(&ctx.env)?;
            }
        }
        if self_t.has_unbound() {
            if ctx.def_gate_depth > 0 {
                return Ok(());
            }
            return Err(anyhow!(
                "cannot resolve {}::{}: the type of its self argument ({}) is not \
                 known at this call; annotate it",
                def.name,
                m.name,
                self_t
            )
            .context(ErrorContext((*self.spec).clone())));
        }
        if let Some(core) = crate::node::coretraits::CoreTrait::of_id(def.id) {
            return self.lower_core_call(ctx, core);
        }
        if let Type::Set(members) = &self_t
            && !def.hole
        {
            let members = members.clone();
            return self.lower_trait_union(ctx, &def, tm.index, &members);
        }
        // A constructor trait selects by the receiver's outermost form.
        if def.hole {
            self_t = match Type::app_split(&self_t, &ctx.env)? {
                Some((ctor, _)) => ctor,
                None => {
                    return Err(anyhow!(
                        "cannot resolve {}::{}: {} is not a type constructor (it has no \
                         last type parameter for {} to abstract over)",
                        def.name,
                        m.name,
                        self_t,
                        def.name
                    )
                    .context(ErrorContext((*self.spec).clone())));
                }
            };
        }
        let Some(im) = ctx.env.find_impl(def.id, &self_t)? else {
            return Err(anyhow!("no implementation of {} for {}", def.name, self_t)
                .context(ErrorContext((*self.spec).clone())));
        };
        let Some(bind) = im.methods.get(m.name.as_str()).copied().or(m.default) else {
            bail!(
                "impl {} for {} has no method {} and the trait declares no default",
                def.name,
                self_t,
                m.name
            )
        };
        self.retarget(ctx, bind);
        let fv =
            ctx.bind_to_lambda.get(&bind).cloned().or_else(|| ctx.rt.store_value(&bind));
        if let Some(fv) = fv
            && let Some(ldef) = fv.downcast_ref::<LambdaDef<R, E>>()
        {
            self.resolve_static(ctx, ldef)?;
        }
        Ok(())
    }

    /// Take this call's argument nodes in spec order under synthesized
    /// names (`#a<i>`, or `self_name` at `self_pos`). Returns the
    /// `(name, node)` pairs and the `(label, name)` list a synthesized
    /// call spells them with.
    fn take_operands(
        &mut self,
        self_pos: Option<usize>,
        self_name: ArcStr,
    ) -> Result<(Vec<(ArcStr, Node<R, E>)>, Vec<(Option<ArcStr>, ArcStr)>)> {
        let ExprKind::Apply(crate::expr::ApplyExpr { args, function: _ }) =
            &self.spec.kind
        else {
            bail!("call site without an apply spec: {}", self.spec)
        };
        let mut operands = Vec::with_capacity(args.len());
        let mut names = Vec::with_capacity(args.len());
        let mut positional = 0usize;
        for (i, (label, _)) in args.iter().enumerate() {
            let key = match label {
                Some(l) => ArgKey::Named(l.clone()),
                None => {
                    let p = positional;
                    positional += 1;
                    ArgKey::Positional(p)
                }
            };
            let is_self = label.is_none() && Some(positional - 1) == self_pos;
            let name: ArcStr = if is_self {
                self_name.clone()
            } else {
                compact_str::format_compact!("#a{i}").as_str().into()
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
    fn install_lowered(
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
        let mut old =
            std::mem::replace(&mut self.fnode, Node::new(Nop { typ: Type::Bottom }));
        old.delete(ctx);
        self.lowered = Some(node);
        Ok(())
    }

    /// A core trait's dispatcher is the operator it stands behind:
    /// `Eq::eq(a, b)` is `a == b`, `Display::fmt(x)` is `"[x]"`,
    /// `Ord::cmp(a, b)` tests `<` and `>`.
    fn lower_core_call(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        core: crate::node::coretraits::CoreTrait,
    ) -> Result<()> {
        use crate::{
            expr::{Pattern, SelectExpr, StructurePattern},
            node::coretraits::CoreTrait,
        };
        let (operands, names) = self.take_operands(None, arcstr::literal!("#s"))?;
        let pos = self.spec.pos;
        let ori = self.spec.ori.clone();
        let mk = |kind: ExprKind| Expr {
            id: ExprId::new(),
            ori: ori.clone(),
            pos,
            kind,
            dec: None,
        };
        let mut positional = names
            .iter()
            .filter(|(l, _)| l.is_none())
            .map(|(_, n)| mk(ExprKind::Ref { name: ModPath::from([n.clone()]) }));
        let (Some(a), b) = (positional.next(), positional.next()) else {
            bail!("core trait call without its self argument: {}", self.spec)
        };
        let (a, b) = (&a, b.as_ref());
        let tag = |t: &'static str| {
            mk(ExprKind::Variant { tag: ArcStr::from(t), args: TArc::from_iter([]) })
        };
        let e = match (core, b) {
            (CoreTrait::Display, _) => {
                mk(ExprKind::StringInterpolate { args: TArc::from_iter([a.clone()]) })
            }
            (CoreTrait::Eq, Some(b)) => {
                mk(ExprKind::Eq { lhs: TArc::new(a.clone()), rhs: TArc::new(b.clone()) })
            }
            (CoreTrait::Ord, Some(b)) => {
                let lt = mk(ExprKind::Lt {
                    lhs: TArc::new(a.clone()),
                    rhs: TArc::new(b.clone()),
                });
                let gt = mk(ExprKind::Gt {
                    lhs: TArc::new(a.clone()),
                    rhs: TArc::new(b.clone()),
                });
                let scrutinee = mk(ExprKind::Tuple { args: TArc::from_iter([lt, gt]) });
                let arm = |l: StructurePattern, r: StructurePattern, body: Expr| {
                    (
                        Pattern {
                            type_predicate: None,
                            structure_predicate: StructurePattern::Tuple {
                                all: None,
                                binds: TArc::from_iter([l, r]),
                            },
                            guard: None,
                        },
                        body,
                    )
                };
                let lit = |b: bool| StructurePattern::Literal(Value::Bool(b));
                let any = || StructurePattern::Ignore;
                mk(ExprKind::Select(SelectExpr {
                    arg: TArc::new(scrutinee),
                    arms: TArc::from_iter([
                        arm(lit(true), any(), tag("Less")),
                        arm(any(), lit(true), tag("Greater")),
                        arm(any(), any(), tag("Equal")),
                    ]),
                }))
            }
            (CoreTrait::Eq | CoreTrait::Ord, None) => {
                bail!("core trait call without its other argument: {}", self.spec)
            }
        };
        let scope = self.scope.clone();
        let spec = (*self.spec).clone();
        let node = super::bind::lower_over_operands(
            ctx,
            self.flags,
            &scope,
            &spec,
            self.top_id,
            operands,
            e,
        )?;
        self.install_lowered(ctx, node)
    }

    /// Dispatch over a union self type: the call becomes
    ///
    /// ```text
    /// { let #s = <self>; let #a0 = <arg0>; ..;
    ///   select #s { M1 as #t => <impl M1>(#t, #a0, ..), M2 as #t => .. } }
    /// ```
    ///
    /// The implementation bindings are named by id (`#bind::N`).
    fn lower_trait_union(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        def: &crate::env::TraitDef,
        index: usize,
        members: &[Type],
    ) -> Result<()> {
        use crate::expr::{ApplyExpr, Pattern, SelectExpr, StructurePattern};
        let m = &def.methods[index];
        let mut targets: LPooled<Vec<(Type, BindId)>> = LPooled::take();
        for mem in members.iter() {
            let Some(im) = ctx.env.find_impl(def.id, mem)? else {
                return Err(anyhow!(
                    "no implementation of {} for {mem}, a member of the self type {}",
                    def.name,
                    Type::Set(TArc::from_iter(members.iter().cloned()))
                )
                .context(ErrorContext((*self.spec).clone())));
            };
            let Some(bind) = im.methods.get(m.name.as_str()).copied().or(m.default)
            else {
                bail!("impl {} for {mem} has no method {}", def.name, m.name)
            };
            targets.push((mem.clone(), bind));
        }
        let pos = self.spec.pos;
        let ori = self.spec.ori.clone();
        let mk = |kind: ExprKind| Expr {
            id: ExprId::new(),
            ori: ori.clone(),
            pos,
            kind,
            dec: None,
        };
        let (operands, names) =
            self.take_operands(Some(m.self_index), arcstr::literal!("#s"))?;
        let call_args: LPooled<Vec<(Option<ArcStr>, Expr)>> = names
            .iter()
            .map(|(label, name)| {
                let arg =
                    if name == "#s" { arcstr::literal!("#t") } else { name.clone() };
                (label.clone(), mk(ExprKind::Ref { name: ModPath::from([arg]) }))
            })
            .collect();
        let arms = targets.drain(..).map(|(mem, bind)| {
            let f = mk(ExprKind::Ref {
                name: ModPath::from([
                    arcstr::literal!("#bind"),
                    ArcStr::from(
                        compact_str::format_compact!("{}", bind.inner()).as_str(),
                    ),
                ]),
            });
            let call = mk(ExprKind::Apply(ApplyExpr {
                function: TArc::new(f),
                args: TArc::from_iter(call_args.iter().cloned()),
            }));
            let pat = Pattern {
                type_predicate: Some(mem),
                structure_predicate: StructurePattern::Bind(arcstr::literal!("#t")),
                guard: None,
            };
            (pat, call)
        });
        let select = mk(ExprKind::Select(SelectExpr {
            arg: TArc::new(mk(ExprKind::Ref {
                name: ModPath::from([arcstr::literal!("#s")]),
            })),
            arms: TArc::from_iter(arms),
        }));
        let scope = self.scope.clone();
        let spec = (*self.spec).clone();
        let node = super::bind::lower_over_operands(
            ctx,
            self.flags,
            &scope,
            &spec,
            self.top_id,
            operands,
            select,
        )?;
        self.install_lowered(ctx, node)
    }

    /// Re-point this call's function node at binding `bind`.
    fn retarget(&mut self, ctx: &mut ExecCtx<R, E>, bind: BindId) {
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
        let mut old =
            std::mem::replace(&mut self.fnode, Ref::new(bind, typ, self.top_id, fspec));
        old.delete(ctx);
        ctx.rt.ref_var(bind, self.top_id);
    }
}

impl<R: Rt, E: UserEvent> CallSite<R, E> {
    fn update_call(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        event: &mut Event<E>,
    ) -> &TagValue {
        let woke = self.slept.take() && ctx.frame_depth == 0;
        let mut set: LPooled<Vec<BindId>> = LPooled::take();
        let mut arg_fired = false;
        let capture_prods = self.is_self_tail_call.load(Ordering::Relaxed)
            || match &self.callee {
                Callee::Static { first_update, .. } => *first_update,
                _ => true,
            };
        let mut prods: SmallVec<[(BindId, TagValue); 4]> = SmallVec::new();
        for arg in self.args.values_mut() {
            if let Some(ref mut node) = arg.node {
                let tv = node.update(ctx, event);
                let tag = tv.tag();
                if capture_prods {
                    prods.push((arg.id, tv.clone()));
                }
                if tag.triggers() {
                    arg_fired = true;
                    if tag.is_bottom() {
                        // A fresh bottom persists in the store, like a value.
                        if ctx.frame_depth == 0 {
                            ctx.rt.store_insert(
                                arg.id,
                                TagValue::tagged(Value::Null, Tag::FRESH_BOTTOM),
                            );
                        }
                        event.variables.insert(
                            arg.id,
                            TagValue::tagged(Value::Null, Tag::FRESH_BOTTOM),
                        );
                    } else {
                        let v = tv.value_cloned();
                        // Frames never write the store.
                        if ctx.frame_depth == 0 {
                            ctx.rt.store_insert(arg.id, TagValue::fired(v.clone()));
                        }
                        event.variables.insert(arg.id, TagValue::tagged(v, tag));
                    }
                    set.push(arg.id);
                } else if woke && !tag.is_bottom() {
                    // Wake catch-up: the standing entry may have drifted
                    // behind while the arm slept; refresh it, stale.
                    ctx.rt.store_insert_standing(
                        arg.id,
                        TagValue::stale(tv.value_cloned()),
                    );
                } else if ctx.frame_depth > 0 && !tag.is_bottom() {
                    // In a frame the store holds the pre-frame value; publish
                    // the frame's value on the cycle-scoped overlay, stale.
                    let tv = tv.clone();
                    event.variables.insert(arg.id, tv);
                    set.push(arg.id);
                }
            }
        }
        // Tail-call interception: stash the rebind args for the enclosing
        // `GXLambda::update` loop instead of dispatching. Only a genuine
        // call (an arg fired, or an init view) enters the loop.
        if self.is_self_tail_call.load(Ordering::Relaxed) {
            let order = self.tail_arg_order.lock();
            let lambda = *self.callee_lambda_id.lock();
            if let (Some(order), Some(lambda)) = (order.as_ref(), lambda) {
                if !event.init && !arg_fired {
                    for id in set.drain(..) {
                        event.variables.remove(&id);
                    }
                    // A quiet tail self-call rides without dispatching, or it
                    // would consume the callee's first-dispatch init view.
                    return self.resident.ride();
                }
                // Each arg carries its honest production tag; a bottomed
                // arg is `None` and the formal rides its previous value.
                let args: SmallVec<[Option<TagValue>; 4]> = order
                    .iter()
                    .map(|id| {
                        if let Some((_, tv)) = prods.iter().find(|(pid, _)| pid == id) {
                            return if tv.tag().is_bottom() {
                                None
                            } else {
                                Some(tv.clone())
                            };
                        }
                        match super::read_var(ctx, event, id) {
                            Some(super::VarRead::Delivered(tv))
                                if !tv.tag().is_bottom() =>
                            {
                                Some(tv.clone())
                            }
                            Some(super::VarRead::Delivered(_)) => None,
                            Some(super::VarRead::Standing(tv))
                                if !tv.tag().is_bottom() =>
                            {
                                let mut c = tv.clone();
                                let t = c.tag().quiet();
                                c.retag(t);
                                Some(c)
                            }
                            Some(super::VarRead::Standing(_)) | None => None,
                        }
                    })
                    .collect();
                debug_assert!(ctx.pending_tail_call.is_none());
                ctx.pending_tail_call = Some(PendingTailCall { lambda, args });
                for id in set.drain(..) {
                    event.variables.remove(&id);
                }
                return self.resident.ride();
            }
        }
        // `fnode.update` runs every cycle for its effects; a `Static`
        // callee discards the value.
        let fnode_value = {
            let tv = self.fnode.update(ctx, event);
            if tv.tag().is_bottom() { None } else { Some(tv.value_cloned()) }
        };
        let bound = if let Callee::Static { first_update, .. } = &mut self.callee {
            let first = *first_update;
            *first_update = false;
            first
        } else {
            match fnode_value {
                None => false,
                Some(v) => {
                    let same = matches!(
                        &self.callee,
                        Callee::DynamicBound { def, .. } if def == &v
                    );
                    if same {
                        false
                    } else {
                        match v.downcast_ref::<LambdaDef<R, E>>() {
                            None => panic!("value {v:?} is not a function"),
                            Some(lb) => {
                                let scope = self.scope.clone();
                                self.bind(
                                    ctx,
                                    scope,
                                    self.flags,
                                    v.clone(),
                                    lb,
                                    event,
                                    &mut set,
                                )
                                .expect("failed to bind to lambda");
                                true
                            }
                        }
                    }
                }
            }
        };
        if bound {
            for (id, tv) in prods.iter() {
                let tag = tv.tag();
                if !tag.triggers() && !tag.is_bottom() {
                    let is_default =
                        self.args.values().any(|a| a.id == *id && a.is_default);
                    seed_quiet_arg(ctx, event, *id, tv, is_default, &mut set);
                }
            }
        }
        if crate::dbgenv::gxdbg_cs() {
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
        if crate::dbgenv::gxdbg_cs() {
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
            None => self.resident.ride(),
        }
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for CallSite<R, E> {
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
                if ftype.args.len() < self.args.len() && ftype.vargs.is_none() {
                    bail!(
                        "too many arguments, expected {}, received {}",
                        ftype.args.len(),
                        self.args.len()
                    )
                }
                let mut labeled: LPooled<AHashSet<ArcStr>> = LPooled::take();
                for arg in ftype.args.iter() {
                    if let FnArgKind::Labeled { name, has_default } = &arg.kind {
                        labeled.insert(name.clone());
                        match self.args.get(&ArgKey::Named(name.clone())) {
                            None if !*has_default => {
                                bail!("missing required argument {name}")
                            }
                            None => {
                                self.args.insert(
                                    ArgKey::Named(name.clone()),
                                    Arg::new(
                                        BindId::new(),
                                        Some(Nop::new(arg.typ.clone())),
                                        true,
                                    ),
                                );
                            }
                            Some(_) => {}
                        }
                    }
                }
                for key in self.args.keys() {
                    if let ArgKey::Named(name) = key {
                        if !labeled.contains(name) {
                            bail!("unknown labeled argument {name}")
                        }
                    }
                }
                let n_positional_required =
                    ftype.args.iter().filter(|a| a.is_positional()).count();
                let n_positional_provided = self
                    .args
                    .keys()
                    .filter(|k| matches!(k, ArgKey::Positional(_)))
                    .count();
                if n_positional_provided < n_positional_required {
                    bail!("missing required argument")
                }
                // The total-count guard misses this when defaults inflate
                // the callee's budget.
                if n_positional_provided > n_positional_required && ftype.vargs.is_none()
                {
                    bail!(
                        "too many positional arguments, expected {n_positional_required}, received {n_positional_provided}"
                    )
                }
                ftype
            }
        };
        let mut pos_idx = 0;
        for (i, farg) in ftype.args.iter().enumerate() {
            let key = if let FnArgKind::Labeled { name, .. } = &farg.kind {
                ArgKey::Named(name.clone())
            } else {
                let key = loop {
                    let candidate = ArgKey::Positional(pos_idx);
                    pos_idx += 1;
                    if self.args.contains_key(&candidate) {
                        break candidate;
                    }
                    bail!("missing required positional argument {i}")
                };
                key
            };
            if let Some(arg) = self.args.get_mut(&key) {
                if let Some(n) = arg.node.as_mut() {
                    // A reference instantiates its signature in its own
                    // typecheck0, which must precede the pre-unify.
                    if matches!(n.view(), NodeView::Ref(_)) {
                        wrap!(n, n.typecheck0(ctx))?;
                    }
                    Type::pre_unify_arg(&ctx.env, &farg.typ, n.typ())?;
                    wrap!(n, n.typecheck0(ctx))?;
                    wrap!(n, farg.typ.check_contains(&ctx.env, &n.typ()))?;
                }
            }
        }
        if let Some(typ) = &ftype.vargs {
            loop {
                let key = ArgKey::Positional(pos_idx);
                pos_idx += 1;
                match self.args.get_mut(&key) {
                    Some(arg) => {
                        if let Some(ref mut n) = arg.node {
                            if matches!(n.view(), NodeView::Ref(_)) {
                                wrap!(n, n.typecheck0(ctx))?;
                            }
                            Type::pre_unify_arg(&ctx.env, typ, n.typ())?;
                            wrap!(n, n.typecheck0(ctx))?;
                            wrap!(n, typ.check_contains(&ctx.env, &n.typ()))?;
                        }
                    }
                    None => break,
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
                Some((id, _)) => {
                    if let Some(bind) = ctx.env.by_id.get(&id)
                        && let Type::TVar(tv) = &bind.typ
                    {
                        let tv = tv.read();
                        let mut cell = tv.typ.write();
                        cell.typ = match &cell.typ {
                            None => Some(t),
                            Some(inner) => Some(Type::union(&ctx.env, &[inner, &t])?),
                        };
                    }
                }
                None if t == Type::Bottom => (), // it doesn't throw any errors
                None => {
                    if self
                        .flags
                        .contains(CFlag::WarnUnhandled | CFlag::WarningsAreErrors)
                    {
                        bail!(
                            "ERROR: {} at {} error {} raised from function call {} will not be caught",
                            self.spec.ori,
                            self.spec.pos,
                            t,
                            self.fnode.spec()
                        )
                    }
                    if self.flags.contains(CFlag::WarnUnhandled) {
                        eprintln!(
                            "WARNING: {} at {} error {} raised from function call {} will not be caught",
                            self.spec.ori,
                            self.spec.pos,
                            t,
                            self.fnode.spec()
                        )
                    }
                }
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
            ctx.pending_settles.last_mut().expect("root settle frame").push((
                ftype.clone(),
                rtc,
                defaulted,
                self.spec.clone(),
            ));
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
                    compact_str::format_compact!(
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
                    compact_str::format_compact!(
                        "emit_clif: builtin call site `{}` not discovered — doesn't fuse",
                        self.spec
                    ),
                ));
            }
        };
        let spec_apply = match &self.spec.kind {
            ExprKind::Apply(a) => a,
            _ => bail!("CallSite spec must be ExprKind::Apply"),
        };
        let mut source_nodes: smallvec::SmallVec<[&Node<R, E>; 8]> =
            smallvec::SmallVec::new();
        let mut pos_idx: usize = 0;
        for (label, _) in spec_apply.args.iter() {
            let n = match label {
                Some(name) => self.arg_named(name),
                None => {
                    let n = self.arg_positional(pos_idx);
                    pos_idx += 1;
                    n
                }
            };
            match n {
                Some(n) => source_nodes.push(n),
                None => bail!("emit_clif: missing call-site arg node"),
            }
        }
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
            .collect::<Result<smallvec::SmallVec<[_; 8]>>>()?;
        emit_builtin_call_node(cx, &info, &arg_nodes)
    }
}

fn seed_quiet_arg<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<R, E>,
    event: &mut Event<E>,
    id: BindId,
    tv: &TagValue,
    is_default: bool,
    set: &mut LPooled<Vec<BindId>>,
) {
    if !is_default && ctx.frame_depth == 0 {
        ctx.rt.store_insert_standing(id, TagValue::stale(tv.value_cloned()));
    } else {
        event.variables.insert(id, TagValue::fired(tv.value_cloned()));
        set.push(id);
    }
}
