use super::{compiler::compile, Nop};
use crate::{
    env::{Bind, Env},
    expr::{self, Arg, ErrorContext, Expr, ExprId, Origin},
    node::pattern::StructPatternNode,
    typ::{FnArgKind, FnArgType, FnType, Type},
    wrap, Apply, BindId, CFlag, Event, ExecCtx, InitFn, LambdaId, Node, Refs, Rt, Scope,
    TypecheckPhase, Update, UserEvent,
};
use anyhow::{anyhow, bail, Context, Result};
use arcstr::ArcStr;
use combine::stream::position::SourcePosition;
use compact_str::format_compact;
use enumflags2::BitFlags;
use netidx::{pack::Pack, subscriber::Value, utils::Either};
use nohash::IntSet;
use parking_lot::{Mutex, RwLock};
use poolshark::local::LPooled;
use std::{fmt, hash::Hash, sync::Arc as SArc};
use triomphe::Arc;

pub struct LambdaDef<R: Rt, E: UserEvent> {
    pub id: LambdaId,
    pub env: Env,
    pub scope: Scope,
    pub argspec: Arc<[Arg]>,
    pub typ: Arc<FnType>,
    pub init: InitFn<R, E>,
    pub needs_callsite: bool,
    pub check: Mutex<Option<Box<dyn Apply<R, E>>>>,
}

impl<R: Rt, E: UserEvent> fmt::Debug for LambdaDef<R, E> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "lambda#{}", self.id.inner())
    }
}

impl<R: Rt, E: UserEvent> PartialEq for LambdaDef<R, E> {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl<R: Rt, E: UserEvent> Eq for LambdaDef<R, E> {}

impl<R: Rt, E: UserEvent> PartialOrd for LambdaDef<R, E> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.id.cmp(&other.id))
    }
}

impl<R: Rt, E: UserEvent> Ord for LambdaDef<R, E> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.id.cmp(&other.id)
    }
}

impl<R: Rt, E: UserEvent> Hash for LambdaDef<R, E> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state)
    }
}

impl<R: Rt, E: UserEvent> Pack for LambdaDef<R, E> {
    fn encoded_len(&self) -> usize {
        0
    }

    fn encode(
        &self,
        _buf: &mut impl bytes::BufMut,
    ) -> std::result::Result<(), netidx::pack::PackError> {
        Err(netidx::pack::PackError::Application(0))
    }

    fn decode(
        _buf: &mut impl bytes::Buf,
    ) -> std::result::Result<Self, netidx::pack::PackError> {
        Err(netidx::pack::PackError::Application(0))
    }
}

#[derive(Debug)]
struct GXLambda<R: Rt, E: UserEvent> {
    args: Box<[StructPatternNode]>,
    body: Node<R, E>,
    typ: Arc<FnType>,
}

impl<R: Rt, E: UserEvent> Apply<R, E> for GXLambda<R, E> {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        for (arg, pat) in from.iter_mut().zip(&self.args) {
            if let Some(v) = arg.update(ctx, event) {
                pat.bind(&v, &mut |id, v| {
                    ctx.cached.insert(id, v.clone());
                    event.variables.insert(id, v);
                })
            }
        }
        self.body.update(ctx, event)
    }

    fn typecheck(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        args: &mut [Node<R, E>],
        _phase: TypecheckPhase<'_>,
    ) -> Result<()> {
        for (arg, FnArgType { typ, .. }) in args.iter_mut().zip(self.typ.args.iter()) {
            wrap!(arg, arg.typecheck(ctx))?;
            wrap!(arg, typ.check_contains(&ctx.env, &arg.typ()))?;
        }
        wrap!(self.body, self.body.typecheck(ctx))?;
        wrap!(self.body, self.typ.rtype.check_contains(&ctx.env, &self.body.typ()))?;
        for (tv, tc) in self.typ.constraints.read().iter() {
            tc.check_contains(&ctx.env, &Type::TVar(tv.clone()))?
        }
        Ok(())
    }

    fn typ(&self) -> Arc<FnType> {
        Arc::clone(&self.typ)
    }

    fn refs(&self, refs: &mut Refs) {
        for pat in &self.args {
            pat.ids(&mut |id| {
                refs.bound.insert(id);
            })
        }
        self.body.refs(refs)
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.body.delete(ctx);
        for n in &self.args {
            n.delete(ctx)
        }
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.body.sleep(ctx);
    }
}

impl<R: Rt, E: UserEvent> GXLambda<R, E> {
    pub(super) fn new(
        ctx: &mut ExecCtx<R, E>,
        flags: BitFlags<CFlag>,
        typ: Arc<FnType>,
        argspec: Arc<[Arg]>,
        args: &[Node<R, E>],
        scope: &Scope,
        tid: ExprId,
        body: Expr,
    ) -> Result<Self> {
        if args.len() != argspec.len() {
            bail!("arity mismatch, expected {} arguments", argspec.len())
        }
        let mut argpats = vec![];
        for (a, atyp) in argspec.iter().zip(typ.args.iter()) {
            let pattern = StructPatternNode::compile(
                ctx,
                &atyp.typ,
                &a.pattern,
                scope,
                a.pos,
                body.ori.clone(),
            )?;
            if pattern.is_refutable() {
                bail!(
                    "refutable patterns are not allowed in lambda arguments {}",
                    a.pattern
                )
            }
            argpats.push(pattern);
        }
        let body = compile(ctx, flags, body, &scope, tid)?;
        Ok(Self { args: Box::from(argpats), typ, body })
    }
}

#[derive(Debug)]
struct BuiltInLambda<R: Rt, E: UserEvent> {
    typ: Arc<FnType>,
    apply: Box<dyn Apply<R, E> + Send + Sync + 'static>,
}

impl<R: Rt, E: UserEvent> Apply<R, E> for BuiltInLambda<R, E> {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        self.apply.update(ctx, from, event)
    }

    fn typecheck(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        args: &mut [Node<R, E>],
        phase: TypecheckPhase<'_>,
    ) -> Result<()> {
        match &phase {
            TypecheckPhase::CallSite(_) => (),
            TypecheckPhase::Lambda => {
                if args.len() < self.typ.args.len()
                    || (args.len() > self.typ.args.len() && self.typ.vargs.is_none())
                {
                    let vargs = if self.typ.vargs.is_some() { "at least " } else { "" };
                    bail!(
                        "expected {}{} arguments got {}",
                        vargs,
                        self.typ.args.len(),
                        args.len()
                    )
                }
                for i in 0..args.len() {
                    wrap!(args[i], args[i].typecheck(ctx))?;
                    let atyp = if i < self.typ.args.len() {
                        &self.typ.args[i].typ
                    } else {
                        self.typ.vargs.as_ref().unwrap()
                    };
                    wrap!(args[i], atyp.check_contains(&ctx.env, &args[i].typ()))?
                }
                for (tv, tc) in self.typ.constraints.read().iter() {
                    tc.check_contains(&ctx.env, &Type::TVar(tv.clone()))?
                }
            }
        }
        self.apply.typecheck(ctx, args, phase)
    }

    fn typ(&self) -> Arc<FnType> {
        Arc::clone(&self.typ)
    }

    fn refs(&self, refs: &mut Refs) {
        self.apply.refs(refs)
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.apply.delete(ctx)
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.apply.sleep(ctx);
    }
}

#[derive(Debug)]
pub(crate) struct Lambda {
    top_id: ExprId,
    spec: Expr,
    def: Value,
    flags: BitFlags<CFlag>,
    typ: Type,
}

impl Lambda {
    pub(crate) fn compile<R: Rt, E: UserEvent>(
        ctx: &mut ExecCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: Expr,
        scope: &Scope,
        l: &expr::LambdaExpr,
        top_id: ExprId,
    ) -> Result<Node<R, E>> {
        let mut s: LPooled<Vec<&ArcStr>> = LPooled::take();
        for a in l.args.iter() {
            a.pattern.with_names(&mut |n| s.push(n));
        }
        let len = s.len();
        s.sort();
        s.dedup();
        if len != s.len() {
            bail!("arguments must have unique names");
        }
        let id = LambdaId::new();
        let vargs = match l.vargs.as_ref() {
            None => None,
            Some(None) => Some(None),
            Some(Some(typ)) => Some(Some(typ.scope_refs(&scope.lexical))),
        };
        let rtype = l.rtype.as_ref().map(|t| t.scope_refs(&scope.lexical));
        let throws = l.throws.as_ref().map(|t| t.scope_refs(&scope.lexical));
        let mut argspec = l
            .args
            .iter()
            .map(|a| match &a.constraint {
                None => Arg {
                    labeled: a.labeled.clone(),
                    pattern: a.pattern.clone(),
                    constraint: None,
                    pos: a.pos,
                },
                Some(typ) => Arg {
                    labeled: a.labeled.clone(),
                    pattern: a.pattern.clone(),
                    constraint: Some(typ.scope_refs(&scope.lexical)),
                    pos: a.pos,
                },
            })
            .collect::<LPooled<Vec<_>>>();
        let argspec = Arc::from_iter(argspec.drain(..));
        let constraints = l
            .constraints
            .iter()
            .map(|(tv, tc)| {
                let tv = tv.scope_refs(&scope.lexical);
                let tc = tc.scope_refs(&scope.lexical);
                Ok((tv, tc))
            })
            .collect::<Result<LPooled<Vec<_>>>>()?;
        let constraints = Arc::new(RwLock::new(constraints));
        let original_scope = scope.clone();
        let _original_scope = scope.clone();
        let scope = scope.append(&format_compact!("fn{}", id.0));
        let _scope = scope.clone();
        let env = ctx.env.clone();
        let _env = ctx.env.clone();
        let mut needs_callsite = false;
        if let Either::Right(builtin) = &l.body {
            if let Some((_, nc)) = ctx.builtins.get(builtin.as_str()) {
                needs_callsite = *nc;
            } else {
                bail!("unknown builtin function {builtin}")
            }
            if !ctx.builtins_allowed {
                bail!("defining builtins is not allowed in this context")
            }
            for a in argspec.iter() {
                if a.constraint.is_none() {
                    bail!("builtin function {builtin} requires all arguments to have type annotations")
                }
            }
            if rtype.is_none() {
                bail!("builtin function {builtin} requires a return type annotation")
            }
        }
        let typ = {
            let args = Arc::from_iter(argspec.iter().map(|a| {
                let kind = match (a.labeled.as_ref(), a.pattern.single_bind()) {
                    (Some(default), Some(name)) => FnArgKind::Labeled {
                        name: name.clone(),
                        has_default: default.is_some(),
                    },
                    (Some(_), None) => FnArgKind::Positional { name: None },
                    (None, name) => FnArgKind::Positional { name: name.cloned() },
                };
                let typ = match a.constraint.as_ref() {
                    Some(t) => t.clone(),
                    None => Type::empty_tvar(),
                };
                FnArgType { kind, typ }
            }));
            let vargs = match vargs {
                Some(Some(t)) => Some(t.clone()),
                Some(None) => Some(Type::empty_tvar()),
                None => None,
            };
            let rtype = rtype.clone().unwrap_or_else(|| Type::empty_tvar());
            let explicit_throws = throws.is_some();
            let throws = throws.clone().unwrap_or_else(|| Type::empty_tvar());
            Arc::new(FnType {
                constraints,
                args,
                vargs,
                rtype,
                throws,
                explicit_throws,
                lambda_ids: Arc::new(RwLock::new(IntSet::default())),
            })
        };
        typ.alias_tvars(&mut LPooled::take());
        if needs_callsite || ctx.env.lsp_mode {
            typ.lambda_ids.write().insert(id);
        }
        let _typ = typ.clone();
        let _argspec = argspec.clone();
        let body = l.body.clone();
        // Honour GRAPHIX_NO_KIR=1 by skipping the KirNode path
        // entirely. Captured at compile time so the env var only
        // matters for the process lifetime — runtime A/B benches
        // (KirNode vs GXLambda) toggle it before launching graphix.
        let kir_enabled = std::env::var_os("GRAPHIX_NO_KIR").is_none();
        // GRAPHIX_JIT=1: synchronous JIT compile at Lambda::compile.
        // Predictable (fully native by the time we run the program)
        // but pays codegen cost up-front; with many fusable
        // lambdas this delays startup.
        //
        // GRAPHIX_JIT_ASYNC=1: queue compile to a background worker
        // thread. KirNode interprets until the worker fills the
        // async slot, then transparently swaps to native dispatch.
        // No startup penalty; first-N invocations of each kernel
        // run interpreted (still much faster than the node graph).
        //
        // If both are set, sync wins (more predictable). If neither,
        // no JIT.
        let jit_enabled = std::env::var_os("GRAPHIX_JIT").is_some();
        let jit_async = !jit_enabled
            && std::env::var_os("GRAPHIX_JIT_ASYNC").is_some();
        // Eager fusion: try to build the KIR kernel (and JIT-compile
        // it if requested) at compile time, while we have the user's
        // annotations on the argspec. The result is shared via Arc
        // into the InitFn so every call site reuses the same kernel
        // — no per-instantiation rebuild, no per-call JIT. Lambdas
        // that don't fully annotate get `None` here and fall through
        // to the deferred path (which retries fusion at first call
        // using the typechecker-resolved FnType).
        // FusedSlot carries the kernel, an optional sync JIT
        // wrapper, an optional async JIT slot (filled by the
        // background worker), and the KernelRegistry snapshot the
        // runtime interpreter walks for KirOp::Call.
        //
        // Sync wrapper takes precedence over async slot at construct-
        // ion time: if sync JIT is enabled we don't even submit an
        // async request. If neither is set, KirNode runs through
        // the interpreter.
        type FusedSlot = Option<(
            SArc<crate::kernel_ir::KirKernel>,
            Option<SArc<crate::kir_jit::WrappedKernel>>,
            Option<SArc<crate::kir_jit::AsyncJitSlot>>,
            SArc<crate::kir_interp::KernelRegistry>,
        )>;
        // Snapshot the fusion-visibility state at compile time, both
        // for the eager attempt below and for the deferred attempt
        // captured into the init closure. These are clones of the
        // BTreeMaps; not free but typically small (handful of
        // entries).
        let snapshot_consts = ctx.fusion_known_consts.clone();
        let snapshot_kernels_full = ctx.fusion_known_kernels.clone();
        let known_signatures: std::collections::BTreeMap<
            ArcStr,
            crate::kernel_ir::KnownFusedFn,
        > = snapshot_kernels_full
            .iter()
            .map(|(k, v)| (k.clone(), v.signature.clone()))
            .collect();
        // Build the runtime kernel registry once. `KirNode::Call`
        // dispatch reads it; cheap to share via Arc.
        let runtime_registry: SArc<crate::kir_interp::KernelRegistry> = {
            let mut reg = crate::kir_interp::KernelRegistry::default();
            for (name, entry) in snapshot_kernels_full.iter() {
                reg.kernels.insert(name.clone(), entry.kernel.clone());
            }
            SArc::new(reg)
        };
        let precomputed: FusedSlot = if kir_enabled {
            match &l.body {
                Either::Left(_) => {
                    let synth_name = match ctx.current_binding_name.as_ref() {
                        Some(n) => n.clone(),
                        None => ArcStr::from(
                            format_compact!("kir_{}", id.0).as_str(),
                        ),
                    };
                    match crate::fusion::build_kir_kernel(
                        synth_name.as_str(),
                        l,
                        &known_signatures,
                        &snapshot_consts,
                    ) {
                        Some((kernel, sig)) => {
                            // The kernel may legitimately contain
                            // `KirOp::Call` now — the interpreter
                            // resolves them via runtime_registry.
                            // The JIT path can't yet (cross-module
                            // calls aren't wired), so we only build
                            // a wrapper for call-free kernels.
                            let kernel_arc = SArc::new(kernel);
                            let jit_safe =
                                !crate::kernel_ir::kernel_contains_call(
                                    &kernel_arc,
                                );
                            let wrapped = if jit_enabled && jit_safe {
                                match crate::kir_jit::compile_kernel_with_wrapper(
                                    &kernel_arc,
                                ) {
                                    Ok(w) => Some(SArc::new(w)),
                                    Err(e) => {
                                        log::warn!(
                                            "kir_jit: compile failed for {}: \
                                             {e:#}; falling back to \
                                             interpreter",
                                            synth_name
                                        );
                                        None
                                    }
                                }
                            } else {
                                None
                            };
                            let async_slot = if jit_async
                                && jit_safe
                                && wrapped.is_none()
                            {
                                Some(crate::kir_jit::submit_async_compile(
                                    kernel_arc.clone(),
                                    synth_name.clone(),
                                ))
                            } else {
                                None
                            };
                            // Publish under the binding name so later
                            // siblings in the same scope (and inner
                            // lambdas they contain) can lower
                            // calls/refs to this kernel.
                            if let Some(binding_name) =
                                ctx.current_binding_name.as_ref()
                            {
                                ctx.fusion_known_kernels.insert(
                                    binding_name.clone(),
                                    crate::FusedKernelEntry {
                                        signature: sig,
                                        kernel: kernel_arc.clone(),
                                    },
                                );
                            }
                            Some((
                                kernel_arc,
                                wrapped,
                                async_slot,
                                runtime_registry.clone(),
                            ))
                        }
                        _ => None,
                    }
                }
                Either::Right(_) => None,
            }
        } else {
            None
        };
        // Deferred fusion state: when the eager attempt fails
        // (unannotated argspec), the InitFn re-attempts at first
        // call using the call-site `resolved` FnType to fill in the
        // missing types. Result is cached so the second call site
        // doesn't re-fuse + re-JIT.
        //
        // Polymorphic lambdas: this caches the FIRST resolved type
        // seen. If a later call site uses different types, we'd
        // dispatch into the wrong native code — a known v1 limit.
        // Detect-and-skip-cache for type mismatch is a follow-up.
        #[derive(Clone)]
        enum DeferredState {
            Pending,
            Failed,
            Ready(FusedSlot),
        }
        let deferred_state: SArc<Mutex<DeferredState>> =
            if kir_enabled && precomputed.is_none() {
                SArc::new(Mutex::new(DeferredState::Pending))
            } else {
                // No deferred path needed. Use a permanently-Failed
                // state so the closure's lookup just no-ops.
                SArc::new(Mutex::new(DeferredState::Failed))
            };
        let l_for_deferred = if kir_enabled && precomputed.is_none() {
            Some(l.clone())
        } else {
            None
        };
        let synth_name_for_deferred = if l_for_deferred.is_some() {
            Some(match ctx.current_binding_name.as_ref() {
                Some(n) => n.clone(),
                None => ArcStr::from(format_compact!("kir_{}", id.0).as_str()),
            })
        } else {
            None
        };
        // Snapshots threaded into the deferred fusion attempt — same
        // shape as the eager attempt, just deferred until first call
        // when the typechecker has resolved the lambda's argspec.
        // Always clone (cheap; usually small maps), even when
        // deferred isn't needed, so we don't pay branchy capture
        // logic in the closure.
        let deferred_consts = snapshot_consts.clone();
        let deferred_known_signatures = known_signatures.clone();
        let deferred_registry = runtime_registry.clone();
        let init: InitFn<R, E> = SArc::new(move |scope, ctx, args, resolved, tid| {
            // restore the lexical environment to the state it was in
            // when the closure was created
            ctx.with_restored(_env.clone(), |ctx| match body.clone() {
                Either::Left(body) => {
                    // Fast path: precomputed fusion result. Every
                    // call site clones the shared Arc<KirKernel> and
                    // (when JIT-on) Arc<WrappedKernel>. The KirNode
                    // gets its own per-call-site arg buffer but the
                    // expensive bits (kernel build, native codegen)
                    // were already done.
                    if let Some((kernel_arc, wrapped, async_slot, registry)) =
                        &precomputed
                    {
                        if args.len() == kernel_arc.params.len() {
                            let n_args = args.len();
                            let kn = match (wrapped, async_slot) {
                                (Some(w), _) => crate::kir_interp::KirNode::with_jit(
                                    kernel_arc.clone(),
                                    n_args,
                                    SArc::clone(w),
                                    SArc::clone(registry),
                                ),
                                (None, Some(s)) => {
                                    crate::kir_interp::KirNode::with_async_jit(
                                        kernel_arc.clone(),
                                        n_args,
                                        SArc::clone(s),
                                        SArc::clone(registry),
                                    )
                                }
                                (None, None) => crate::kir_interp::KirNode::new(
                                    kernel_arc.clone(),
                                    n_args,
                                    SArc::clone(registry),
                                ),
                            };
                            return Ok(Box::new(kn) as Box<dyn Apply<R, E>>);
                        }
                    }
                    // Deferred path: when the eager attempt failed
                    // (e.g. an unannotated callback like `|idx| ...`
                    // passed to array::map), retry now that the
                    // typechecker has handed us a resolved FnType.
                    // Patch the lambda's argspec from `resolved` and
                    // try fusion again. Cache the result so later
                    // call sites of the same LambdaDef skip the
                    // work.
                    if let (
                        Some(l_def),
                        Some(synth),
                        Some(rft),
                    ) = (
                        l_for_deferred.as_ref(),
                        synth_name_for_deferred.as_ref(),
                        resolved,
                    ) {
                        let mut state = deferred_state.lock();
                        let slot: FusedSlot = match &*state {
                            DeferredState::Ready(slot) => slot.clone(),
                            DeferredState::Failed => None,
                            DeferredState::Pending => {
                                let mut patched =
                                    triomphe::Arc::new(l_def.clone());
                                crate::fusion::apply_fntype_to_lambda(
                                    &mut patched,
                                    rft,
                                );
                                let attempt = crate::fusion::build_kir_kernel(
                                    synth.as_str(),
                                    &*patched,
                                    &deferred_known_signatures,
                                    &deferred_consts,
                                );
                                let slot: FusedSlot = match attempt {
                                    Some((kernel, _sig)) => {
                                        let kernel_arc = SArc::new(kernel);
                                        // JIT only when no calls
                                        // (cross-module dispatch is
                                        // not yet wired); the
                                        // interpreter handles Call
                                        // via the registry.
                                        let wrapped = if jit_enabled
                                            && !crate::kernel_ir::kernel_contains_call(
                                                &kernel_arc,
                                            )
                                        {
                                            match crate::kir_jit::compile_kernel_with_wrapper(
                                                &kernel_arc,
                                            ) {
                                                Ok(w) => Some(SArc::new(w)),
                                                Err(e) => {
                                                    log::warn!(
                                                        "kir_jit (deferred): {}: \
                                                         {e:#}; using interpreter",
                                                        synth
                                                    );
                                                    None
                                                }
                                            }
                                        } else {
                                            None
                                        };
                                        // Async deferred-fusion JIT
                                        // is supported the same way
                                        // as eager — when sync didn't
                                        // produce a wrapper we may
                                        // still queue a background
                                        // compile.
                                        let async_slot = if jit_async
                                            && wrapped.is_none()
                                        {
                                            Some(
                                                crate::kir_jit::submit_async_compile(
                                                    kernel_arc.clone(),
                                                    synth.clone(),
                                                ),
                                            )
                                        } else {
                                            None
                                        };
                                        Some((
                                            kernel_arc,
                                            wrapped,
                                            async_slot,
                                            deferred_registry.clone(),
                                        ))
                                    }
                                    _ => None,
                                };
                                *state = match &slot {
                                    Some(_) => DeferredState::Ready(slot.clone()),
                                    None => DeferredState::Failed,
                                };
                                slot
                            }
                        };
                        drop(state);
                        if let Some((kernel_arc, wrapped, async_slot, registry)) =
                            slot
                        {
                            if args.len() == kernel_arc.params.len() {
                                let n_args = args.len();
                                let kn = match (wrapped, async_slot) {
                                    (Some(w), _) =>
                                        crate::kir_interp::KirNode::with_jit(
                                            kernel_arc, n_args, w, registry,
                                        ),
                                    (None, Some(s)) =>
                                        crate::kir_interp::KirNode::with_async_jit(
                                            kernel_arc, n_args, s, registry,
                                        ),
                                    (None, None) =>
                                        crate::kir_interp::KirNode::new(
                                            kernel_arc, n_args, registry,
                                        ),
                                };
                                return Ok(Box::new(kn) as Box<dyn Apply<R, E>>);
                            }
                        }
                    }
                    let scope = Scope {
                        dynamic: scope.dynamic.clone(),
                        lexical: _scope.lexical.clone(),
                    };
                    GXLambda::new(
                        ctx,
                        flags,
                        _typ.clone(),
                        _argspec.clone(),
                        args,
                        &scope,
                        tid,
                        body.clone(),
                    )
                    .map(|a| -> Box<dyn Apply<R, E>> { Box::new(a) })
                }
                Either::Right(builtin) => match ctx.builtins.get(&*builtin) {
                    None => bail!("unknown builtin function {builtin}"),
                    Some((init, _)) => init(ctx, &_typ, resolved, &_scope, args, tid)
                        .map(|apply| {
                            let f: Box<dyn Apply<R, E>> =
                                Box::new(BuiltInLambda { typ: _typ.clone(), apply });
                            f
                        }),
                },
            })
        });
        let def = ctx.lambdawrap.wrap(LambdaDef {
            id,
            typ: typ.clone(),
            env,
            argspec,
            init,
            scope: original_scope,
            needs_callsite,
            check: Mutex::new(None),
        });
        ctx.lambda_defs.insert(id, def.clone());
        // Publish the lambda's FnType under its source-level ExprId
        // so later consumers (the fusion pass) can resolve the type
        // without re-doing inference. `typ` here is Arc-shared with
        // the copy stored on the lambda's Value; TVars inside get
        // unified in place as the containing expression typechecks,
        // so our cloned-out FnType sees the same resolved state
        // through its TVar Arcs.
        ctx.fn_types.insert(spec.id, (*typ).clone());
        Ok(Box::new(Self { spec, def, typ: Type::Fn(typ), top_id, flags }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Lambda {
    fn update(
        &mut self,
        _ctx: &mut ExecCtx<R, E>,
        event: &mut Event<E>,
    ) -> Option<Value> {
        event.init.then(|| self.def.clone())
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn refs(&self, _refs: &mut Refs) {}

    fn delete(&mut self, _ctx: &mut ExecCtx<R, E>) {}

    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {}

    fn typ(&self) -> &Type {
        &self.typ
    }

    fn typecheck(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        let def = self
            .def
            .downcast_ref::<LambdaDef<R, E>>()
            .ok_or_else(|| anyhow!("failed to unwrap lambda"))?;
        let needs_callsite = def.needs_callsite;
        let mut faux_args: LPooled<Vec<Node<R, E>>> = def
            .argspec
            .iter()
            .zip(def.typ.args.iter())
            .map(|(a, at)| match &a.labeled {
                Some(Some(e)) => ctx.with_restored(def.env.clone(), |ctx| {
                    compile(ctx, self.flags, e.clone(), &def.scope, self.top_id)
                }),
                Some(None) | None => {
                    let n: Node<R, E> = Box::new(Nop { typ: at.typ.clone() });
                    Ok(n)
                }
            })
            .collect::<Result<_>>()?;
        let faux_id = BindId::new();
        ctx.env.by_id.insert_cow(
            faux_id,
            Bind {
                doc: None,
                export: false,
                id: faux_id,
                name: "faux".into(),
                scope: def.scope.lexical.clone(),
                typ: Type::empty_tvar(),
                pos: SourcePosition::default(),
                ori: Arc::new(Origin::default()),
            },
        );
        let prev_catch = ctx.env.catch.insert_cow(def.scope.dynamic.clone(), faux_id);
        let res = (def.init)(&def.scope, ctx, &mut faux_args, None, ExprId::new())
            .with_context(|| ErrorContext(Update::<R, E>::spec(self).clone()));
        let res = res.and_then(|mut f| {
            let ftyp = f.typ().clone();
            let res = f
                .typecheck(ctx, &mut faux_args, TypecheckPhase::Lambda)
                .with_context(|| ErrorContext(Update::<R, E>::spec(self).clone()));
            if !needs_callsite {
                f.delete(ctx)
            } else {
                let def = self
                    .def
                    .downcast_ref::<LambdaDef<R, E>>()
                    .expect("failed to unwrap lambda");
                *def.check.lock() = Some(f);
            }
            res?;
            let inferred_throws = ctx.env.by_id[&faux_id]
                .typ
                .with_deref(|t| t.cloned())
                .unwrap_or(Type::Bottom)
                .scope_refs(&def.scope.lexical)
                .normalize();
            ftyp.throws
                .check_contains(&ctx.env, &inferred_throws)
                .with_context(|| ErrorContext(Update::<R, E>::spec(self).clone()))?;
            ftyp.constrain_known();
            Ok(())
        });
        ctx.env.by_id.remove_cow(&faux_id);
        match prev_catch {
            Some(id) => ctx.env.catch.insert_cow(def.scope.dynamic.clone(), id),
            None => ctx.env.catch.remove_cow(&def.scope.dynamic),
        };
        self.typ.unbind_tvars();
        res
    }
}
