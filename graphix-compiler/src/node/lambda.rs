use super::{compiler::compile, Nop};
use crate::{
    effects::EffectKind,
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
    /// Intrinsic sync/async effect — see `effects::EffectKind` and
    /// `design/whole_graph_fusion.md`. Computed by the M6 effect
    /// inference pass after all lambdas have been compiled. Defaults
    /// to `Sync`; the pass walks each lambda body and flips to
    /// `Async` if it finds an async-effect builtin call or a call to
    /// another async user lambda. Function-typed parameter calls do
    /// NOT contribute here — those are handled at the call site via
    /// the lattice join with the resolved fn-arg's effect.
    pub intrinsic_effect: Mutex<EffectKind>,
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
        // Honour the per-runtime fusion/JIT configuration set on
        // `ExecCtx::fusion_config`. Captured at Lambda::compile time
        // so the decision is sticky for the lifetime of the compiled
        // lambda — flipping the config mid-program produces undefined
        // behavior because already-compiled lambdas snapshot here.
        let kir_enabled = !ctx.fusion_config.fusion_disabled;
        let jit_enabled =
            matches!(ctx.fusion_config.jit_mode, crate::JitMode::Sync);
        let jit_async =
            matches!(ctx.fusion_config.jit_mode, crate::JitMode::Async);
        // Lazy fusion. We capture the lambda + a snapshot of the
        // current `fusion_known_consts` map into the InitFn closure
        // and run the actual fusion at first call. Cross-kernel
        // calls resolve through `ctx.fusion_lambdas` via
        // `fusion::lazy_resolve_kernel`, which builds callee kernels
        // on demand the first time they're referenced. Cycles are
        // broken by the per-entry `InProgress` cache state.
        //
        // The result is cached on a `Mutex<LazyState>` shared across
        // all call sites of this LambdaDef, so the work happens once
        // — kernels build, JIT compiles (sync) or queues (async),
        // and subsequent invocations just clone Arcs.
        //
        // [`LazySlot`] is what the cached state holds when fusion
        // succeeded: the kernel, an optional sync JIT wrapper, an
        // optional async slot the worker fills later, and the
        // KernelRegistry the interpreter uses for KirOp::Call
        // dispatch. Sync wrapper wins over async slot when both
        // would be applicable (in practice we only set one).
        type LazySlot = Option<(
            SArc<crate::kernel_ir::KirKernel>,
            Option<SArc<crate::kir_jit::WrappedKernel>>,
            Option<SArc<crate::kir_jit::AsyncJitSlot>>,
            SArc<crate::kir_interp::KernelRegistry>,
        )>;
        #[derive(Clone)]
        enum LazyState {
            Pending,
            Failed,
            Ready(LazySlot),
        }
        let lazy_state: SArc<Mutex<LazyState>> = if kir_enabled {
            SArc::new(Mutex::new(LazyState::Pending))
        } else {
            // KIR disabled — never attempt fusion.
            SArc::new(Mutex::new(LazyState::Failed))
        };
        let lambda_for_lazy = if kir_enabled { Some(l.clone()) } else { None };
        let synth_name = if kir_enabled {
            Some(match ctx.current_binding_name.as_ref() {
                Some(n) => n.clone(),
                None => ArcStr::from(format_compact!("kir_{}", id.0).as_str()),
            })
        } else {
            None
        };
        // Const snapshot for inlining outer-scope `let x = <literal>;`
        // bindings inside fusable bodies. Captured at compile time
        // because Block::compile saves/restores the live map at
        // scope boundaries — by the time the InitFn fires the
        // outer-scope consts may have been restored away.
        let consts_snapshot = ctx.fusion_known_consts.clone();
        let init: InitFn<R, E> = SArc::new(move |scope, ctx, args, resolved, tid| {
            // restore the lexical environment to the state it was in
            // when the closure was created
            ctx.with_restored(_env.clone(), |ctx| match body.clone() {
                Either::Left(body) => {
                    // Lazy fusion: build the kernel + JIT wrapper at
                    // first call, cache, reuse for subsequent calls.
                    // Resolves cross-kernel callees through
                    // `ctx.fusion_lambdas` via `lazy_resolve_kernel`,
                    // which builds each callee's kernel on demand.
                    if let (Some(lambda), Some(synth), Some(rft)) = (
                        lambda_for_lazy.as_ref(),
                        synth_name.as_ref(),
                        resolved,
                    ) {
                        let mut state = lazy_state.lock();
                        let slot: LazySlot = match &*state {
                            LazyState::Ready(slot) => slot.clone(),
                            LazyState::Failed => None,
                            LazyState::Pending => {
                                // Patch the argspec from the call
                                // site's resolved FnType (no-op when
                                // the user already annotated).
                                let mut patched =
                                    triomphe::Arc::new(lambda.clone());
                                crate::fusion::apply_fntype_to_lambda(
                                    &mut patched, rft,
                                );
                                // Discover callees, lazy-resolve each
                                // through ctx.fusion_lambdas. The
                                // signatures feed `build_kir_kernel`
                                // for compile-time call lowering;
                                // the kernels feed the runtime
                                // `KernelRegistry` for `KirOp::Call`
                                // dispatch.
                                let mut known_signatures: std::collections::BTreeMap<
                                    ArcStr,
                                    crate::kernel_ir::KnownFusedFn,
                                > = std::collections::BTreeMap::new();
                                let mut registry_kernels: std::collections::BTreeMap<
                                    ArcStr,
                                    SArc<crate::kernel_ir::KirKernel>,
                                > = std::collections::BTreeMap::new();
                                let mut binding_fn_inputs: Vec<
                                    crate::kernel_ir::FnParam,
                                > = Vec::new();
                                if let netidx::utils::Either::Left(body) =
                                    &patched.body
                                {
                                    // First, lazy-resolve every direct
                                    // callee referenced from the body's
                                    // source AST. This builds each
                                    // callee's KirKernel.
                                    let mut seen: std::collections::BTreeSet<ArcStr> =
                                        std::collections::BTreeSet::new();
                                    for (callee, apply_id) in
                                        crate::fusion::discover_callee_names(body)
                                    {
                                        if !seen.insert(callee.clone()) {
                                            continue;
                                        }
                                        if let Some((sig, kernel)) =
                                            crate::fusion::lazy_resolve_kernel(
                                                ctx,
                                                &callee,
                                                Some(apply_id),
                                            )
                                        {
                                            known_signatures
                                                .insert(callee.clone(), sig);
                                            registry_kernels
                                                .insert(callee, kernel);
                                        } else if let Some(fp) =
                                            crate::fusion::resolve_binding_fn_input(
                                                ctx,
                                                &_scope,
                                                &callee,
                                            )
                                        {
                                            // Callee can't fuse but has a
                                            // valid LambdaDef binding —
                                            // register as DynCall Binding
                                            // source so the parent kernel
                                            // can dispatch through
                                            // Apply::update.
                                            binding_fn_inputs.push(fp);
                                        }
                                    }
                                    // Then, walk transitively: for each
                                    // kernel we just resolved, scan its
                                    // KIR for further `KirOp::Call` and
                                    // pull those in too. Required for
                                    // the JIT shared-module path so the
                                    // whole call graph is declared up
                                    // front. The interpreter only
                                    // strictly needs the immediate
                                    // callees, but feeding it the full
                                    // closure is harmless (just larger
                                    // registries).
                                    let mut work: Vec<ArcStr> =
                                        registry_kernels.keys().cloned().collect();
                                    while let Some(name) = work.pop() {
                                        let inner_kernel = registry_kernels
                                            .get(&name)
                                            .expect("just-inserted")
                                            .clone();
                                        for inner_name in
                                            crate::kernel_ir::collect_call_sites(
                                                &inner_kernel,
                                            )
                                        {
                                            // Skip the kernel-being-built's
                                            // own name (self-recursion of
                                            // the parent goes through the
                                            // parent's own FuncId, not a
                                            // separate registry entry).
                                            if inner_name.as_str()
                                                == synth.as_str()
                                            {
                                                continue;
                                            }
                                            if !seen.insert(inner_name.clone()) {
                                                continue;
                                            }
                                            if let Some((sig, kernel)) =
                                                crate::fusion::lazy_resolve_kernel(
                                                    ctx,
                                                    &inner_name,
                                                    None,
                                                )
                                            {
                                                known_signatures.insert(
                                                    inner_name.clone(),
                                                    sig,
                                                );
                                                registry_kernels.insert(
                                                    inner_name.clone(),
                                                    kernel,
                                                );
                                                work.push(inner_name);
                                            }
                                        }
                                    }
                                }
                                let attempt =
                                    crate::fusion::build_kir_kernel_with_binding_inputs(
                                        synth.as_str(),
                                        &*patched,
                                        &known_signatures,
                                        &consts_snapshot,
                                        &binding_fn_inputs,
                                    );
                                let slot: LazySlot = match attempt {
                                    Some((kernel, _sig)) => {
                                        let kernel_arc = SArc::new(kernel);
                                        let has_calls =
                                            crate::kernel_ir::kernel_contains_call(
                                                &kernel_arc,
                                            );
                                        // Sync JIT path. Kernels with
                                        // `KirOp::Call` go through the
                                        // shared-module path so the
                                        // compiled code can directly
                                        // CLIF-call its callees; leaf
                                        // kernels use a private module
                                        // for cheaper isolation.
                                        let wrapped = if jit_enabled {
                                            let res = if has_calls {
                                                crate::kir_jit::compile_kernel_with_callees(
                                                    &kernel_arc,
                                                    &registry_kernels,
                                                )
                                            } else {
                                                crate::kir_jit::compile_kernel_with_wrapper(
                                                    &kernel_arc,
                                                )
                                            };
                                            match res {
                                                Ok(w) => Some(SArc::new(w)),
                                                Err(e) => {
                                                    log::warn!(
                                                        "kir_jit: {}: {e:#}; \
                                                         using interpreter",
                                                        synth
                                                    );
                                                    None
                                                }
                                            }
                                        } else {
                                            None
                                        };
                                        // Async JIT path: leaf kernels
                                        // only for now. Kernels with
                                        // calls would need the shared
                                        // module on the worker thread,
                                        // which the bg compile path
                                        // doesn't yet thread (M4e v2
                                        // territory). Until then they
                                        // run interpreted, with the
                                        // registry resolving the call
                                        // boundary.
                                        let async_slot = if jit_async
                                            && wrapped.is_none()
                                            && !has_calls
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
                                        let registry = SArc::new(
                                            crate::kir_interp::KernelRegistry {
                                                kernels: registry_kernels,
                                            },
                                        );
                                        Some((
                                            kernel_arc,
                                            wrapped,
                                            async_slot,
                                            registry,
                                        ))
                                    }
                                    _ => None,
                                };
                                *state = match &slot {
                                    Some(_) => LazyState::Ready(slot.clone()),
                                    None => LazyState::Failed,
                                };
                                slot
                            }
                        };
                        drop(state);
                        if let Some((kernel_arc, wrapped, async_slot, registry)) =
                            slot
                        {
                            // KirNode arity = all slot kinds (scalar +
                            // array + tuple + struct + variant + HOF-arg
                            // fn params). Binding-source fn params
                            // resolve through ctx.cached and don't take
                            // an input slot.
                            let total_params =
                                crate::kir_interp::total_kernel_arity(
                                    &kernel_arc,
                                );
                            if args.len() == total_params {
                                let n_args = args.len();
                                let kn_scope = scope.clone();
                                let mut kn = match (wrapped, async_slot) {
                                    (Some(w), _) =>
                                        crate::kir_interp::KirNode::with_jit(
                                            kernel_arc,
                                            n_args,
                                            w,
                                            registry,
                                            kn_scope,
                                            tid,
                                        ),
                                    (None, Some(s)) =>
                                        crate::kir_interp::KirNode::with_async_jit(
                                            kernel_arc,
                                            n_args,
                                            s,
                                            registry,
                                            kn_scope,
                                            tid,
                                        ),
                                    (None, None) =>
                                        crate::kir_interp::KirNode::new(
                                            kernel_arc,
                                            n_args,
                                            registry,
                                            kn_scope,
                                            tid,
                                        ),
                                };
                                kn.pre_init_binding_slots(ctx);
                                return Ok(Box::new(kn) as Box<dyn Apply<R, E>>);
                            }
                        }
                    }
                    // Lazy fusion didn't produce a usable kernel —
                    // either KIR disabled, no resolved type yet, or
                    // body has unsupported constructs. Fall back to
                    // the regular node-graph interpreter.
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
            intrinsic_effect: Mutex::new(EffectKind::Sync),
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
