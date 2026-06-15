use super::{compiler::compile, Nop};
use crate::{
    effects::EffectKind,
    env::{Bind, Env},
    expr::{self, Arg, ErrorContext, Expr, ExprId, Origin},
    node::pattern::StructPatternNode,
    typ::{fntyp::LambdaIds, FnArgKind, FnArgType, FnType, Type},
    wrap, Apply, BindId, CFlag, Event, ExecCtx, InitFn, LambdaId, Node, Refs, Rt, Scope,
    Update, UserEvent,
};
use anyhow::{anyhow, bail, Context, Result};
use arcstr::ArcStr;
use combine::stream::position::SourcePosition;
use compact_str::format_compact;
use enumflags2::BitFlags;
use netidx::{pack::Pack, subscriber::Value, utils::Either};
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

/// Runtime representation of a graphix-language lambda (i.e. a user
/// `fn` defined in `.gx` source). Produced by [`LambdaDef::init`]
/// when a `CallSite` resolves to this lambda — either lazily on
/// first runtime use, or eagerly at compile time by
/// `CallSite::try_static_resolve` (in `typecheck1`).
///
/// Public surface for fusion: `Apply::view()` on `GXLambda` returns
/// [`crate::ApplyView::Lambda(&self)`], letting fusion's walker
/// reach `self.body()` and inline the lambda body into the kernel
/// being built.
#[derive(Debug)]
pub struct GXLambda<R: Rt, E: UserEvent> {
    id: LambdaId,
    args: Box<[StructPatternNode]>,
    body: Node<R, E>,
    typ: Arc<FnType>,
}

impl<R: Rt, E: UserEvent> GXLambda<R, E> {
    /// The lambda definition's stable id. All `GXLambda` instances
    /// produced from the same `LambdaDef::init` carry the same id;
    /// fusion uses this as part of the `(LambdaId, FnType)` cache
    /// key for on-demand kernel monomorphization.
    pub fn id(&self) -> LambdaId {
        self.id
    }

    /// The compiled body Node — the lambda's expression tree.
    /// Fusion walks this via [`crate::Update::view`] /
    /// [`crate::NodeView`].
    pub fn body(&self) -> &Node<R, E> {
        &self.body
    }

    /// Mutable body — for fusion's splicing of inner sub-kernels.
    pub fn body_mut(&mut self) -> &mut Node<R, E> {
        &mut self.body
    }

    /// Argument-binding patterns, in signature order. Parallel to
    /// `self.typ().args`. Each pattern binds one positional or
    /// labeled arg from the call-site `arg_refs` into the body's
    /// scope.
    pub fn args(&self) -> &[StructPatternNode] {
        &self.args
    }

    /// The fully-resolved `FnType` of this lambda. Same as what
    /// `Apply::typ()` returns; provided as a direct accessor for
    /// consumers that have a `&GXLambda` without going through the
    /// trait.
    pub fn typ(&self) -> &Arc<FnType> {
        &self.typ
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for GXLambda<R, E> {
    fn view(&self) -> crate::ApplyView<'_, R, E> {
        crate::ApplyView::Lambda(self)
    }

    fn view_mut(&mut self) -> crate::ApplyViewMut<'_, R, E> {
        crate::ApplyViewMut::Lambda(self)
    }

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

    fn typecheck0(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        args: &mut [Node<R, E>],
    ) -> Result<()> {
        for (arg, FnArgType { typ, .. }) in args.iter_mut().zip(self.typ.args.iter()) {
            wrap!(arg, arg.typecheck0(ctx))?;
            wrap!(arg, typ.check_contains(&ctx.env, &arg.typ()))?;
        }
        wrap!(self.body, self.body.typecheck0(ctx))?;
        wrap!(self.body, self.typ.rtype.check_contains(&ctx.env, &self.body.typ()))?;
        for (tv, tc) in self.typ.constraints.read().iter() {
            tc.check_contains(&ctx.env, &Type::TVar(tv.clone()))?
        }
        Ok(())
    }

    /// CallSite phase: drive the body's `typecheck1` so nested call
    /// sites in the body finalize against their resolved types (the
    /// cascade). The caller's args are walked by the driving
    /// `CallSite::typecheck1`, so we don't re-walk them here.
    fn typecheck1(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        _from: &mut [Node<R, E>],
        _resolved: &FnType,
        _fn_args: &[crate::StaticFnArg<'_, R, E>],
    ) -> Result<()> {
        wrap!(self.body, self.body.typecheck1(ctx))
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

    fn clone_rebind(
        &self,
        ctx: &mut ExecCtx<R, E>,
        scope: &Scope,
    ) -> Box<dyn Apply<R, E>> {
        // Re-mint the arg patterns first (fresh param ids enter the scope
        // name map), then recurse the body STRUCTURALLY so any spliced
        // FusedKernels are preserved (NOT re-init from spec). Keep the
        // LambdaId so clones share the cached JIT kernel. Mirrors the
        // order in `GXLambda::new` (patterns then body, one scope).
        let args: Box<[StructPatternNode]> =
            self.args.iter().map(|p| p.clone_rebind(ctx, scope)).collect();
        let body = self.body.clone_rebind(ctx, scope);
        Box::new(Self { id: self.id, args, body, typ: self.typ.clone() })
    }
}

impl<R: Rt, E: UserEvent> GXLambda<R, E> {
    pub(super) fn new(
        ctx: &mut ExecCtx<R, E>,
        flags: BitFlags<CFlag>,
        id: LambdaId,
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
        Ok(Self { id, args: Box::from(argpats), typ, body })
    }
}

#[derive(Debug)]
struct BuiltInLambda<R: Rt, E: UserEvent> {
    typ: Arc<FnType>,
    apply: Box<dyn Apply<R, E> + Send + Sync + 'static>,
}

impl<R: Rt, E: UserEvent> Apply<R, E> for BuiltInLambda<R, E> {
    /// Pass-through to the inner builtin. `BuiltInLambda` is a
    /// runtime-plumbing wrapper (typecheck/refs); fusion sees the
    /// wrapped builtin's own view as if the wrapper weren't here.
    fn view(&self) -> crate::ApplyView<'_, R, E> {
        self.apply.view()
    }

    fn view_mut(&mut self) -> crate::ApplyViewMut<'_, R, E> {
        self.apply.view_mut()
    }


    fn emit_clif(
        &self,
        callsite: &crate::node::callsite::CallSite<R, E>,
        cx: &mut crate::gir_jit::BodyCx,
    ) -> Result<Option<crate::gir_jit::CompiledExpr>> {
        // Without this delegation the trait default's `Ok(None)`
        // silently swallows every builtin's emission hook — the
        // call site falls to DynCall and the builtin "loses fusion"
        // with no error anywhere (it happened: Stage D2 landed
        // MapQ::emit_clif and no probe inlined until the wrapper
        // was caught).
        self.apply.emit_clif(callsite, cx)
    }

    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        self.apply.update(ctx, from, event)
    }

    fn typecheck0(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        args: &mut [Node<R, E>],
    ) -> Result<()> {
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
            wrap!(args[i], args[i].typecheck0(ctx))?;
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
        self.apply.typecheck0(ctx, args)
    }

    fn typecheck1(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        args: &mut [Node<R, E>],
        resolved: &FnType,
        fn_args: &[crate::StaticFnArg<'_, R, E>],
    ) -> Result<()> {
        self.apply.typecheck1(ctx, args, resolved, fn_args)
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

    fn clone_rebind(
        &self,
        ctx: &mut ExecCtx<R, E>,
        scope: &Scope,
    ) -> Box<dyn Apply<R, E>> {
        // Plumbing wrapper — delegate to the inner builtin's own
        // clone_rebind (builtins own their clone, 100%).
        Box::new(Self {
            typ: self.typ.clone(),
            apply: self.apply.clone_rebind(ctx, scope),
        })
    }
}

#[derive(Debug)]
pub struct Lambda {
    top_id: ExprId,
    spec: Expr,
    def: Value,
    flags: BitFlags<CFlag>,
    typ: Type,
}

impl Lambda {
    /// LambdaId of this lambda's definition — pulled from the
    /// `LambdaDef` stored as a `Value`. Used by `Bind::compile`
    /// to thread the id into `BuiltinBindInfo` so the fusion
    /// pre-binding pass can later look up the lambda's env+scope
    /// for compiling labeled-default expressions.
    pub fn lambda_id<R: Rt, E: UserEvent>(&self) -> Option<crate::LambdaId> {
        self.def.downcast_ref::<LambdaDef<R, E>>().map(|d| d.id)
    }

    /// Borrow the underlying `LambdaDef`. The static-call resolution
    /// pass uses this to call `InitFn` (or construct a `GXLambda`
    /// directly) at compile time when it can prove the call site's
    /// function expression always resolves to this Lambda.
    pub fn def<R: Rt, E: UserEvent>(&self) -> Option<&LambdaDef<R, E>> {
        self.def.downcast_ref::<LambdaDef<R, E>>()
    }

    /// The wrapped `LambdaDef` `Value`. Equivalent to the value the
    /// Lambda Node emits on its init event.
    pub fn def_value(&self) -> &Value {
        &self.def
    }
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
        if let Either::Right(builtin) = &l.body {
            if ctx.builtins.get(builtin.as_str()).is_none() {
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
                lambda_ids: LambdaIds::default(),
            })
        };
        typ.alias_tvars(&mut LPooled::take());
        typ.lambda_ids.set_id(id);
        let _typ = typ.clone();
        let _argspec = argspec.clone();
        let body = l.body.clone();
        let init: InitFn<R, E> = SArc::new(move |scope, ctx, args, resolved, tid| {
            // restore the lexical environment to the state it was in
            // when the closure was created
            ctx.with_restored(_env.clone(), |ctx| match body.clone() {
                Either::Left(body) => {
                    // Always GXLambda for now. The new fusion pipeline
                    // (`crate::fusion::fuse`) will splice a
                    // `FusedKernel` Update node into the graph
                    // *before* runtime, so by the time this InitFn
                    // fires we either have no kernel for this lambda
                    // (run via GXLambda) or the runtime is already
                    // calling into the FusedKernel directly via the
                    // splice. No InitFn cache lookup needed.
                    let scope = Scope {
                        dynamic: scope.dynamic.clone(),
                        lexical: _scope.lexical.clone(),
                    };
                    GXLambda::new(
                        ctx,
                        flags,
                        id,
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
                    Some(init) => init(ctx, &_typ, resolved, &_scope, args, tid)
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
            check: Mutex::new(None),
            intrinsic_effect: Mutex::new(EffectKind::Sync),
        });
        ctx.lambda_defs.insert(id, def.clone());
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

    fn clone_rebind(&self, ctx: &mut ExecCtx<R, E>, scope: &Scope) -> Node<R, E> {
        // `Lambda::refs` is intentionally empty (a lambda VALUE has no
        // runtime refs until it is applied), so the recompile-default
        // `clone_rebind` never aliases this lambda's CAPTURES into the
        // clone scope. Recompiling the body in `scope` then fails to
        // resolve any capture that isn't reachable from `scope` — e.g. a
        // grandparent binding when this lambda is the callback of a HOF
        // NESTED inside another HOF's callback, whose per-slot clone
        // scope is rooted in the array package, not the program (#168).
        // The lambda's DEFINITION env still resolves those captures, so
        // alias each body free-var that's unresolvable in the clone scope
        // but resolvable in `def.env` before recompiling. Slot-local
        // captures (the enclosing HOF's element) already resolve in the
        // clone scope and are left untouched, so existing cases are
        // unaffected. Over-collection is harmless: an inner-let / nested-
        // lambda-arg name won't resolve in `def.scope` either, so it's
        // skipped.
        if let (expr::ExprKind::Lambda(lam), Some(def)) =
            (&self.spec.kind, self.def.downcast_ref::<LambdaDef<R, E>>())
        {
            if let Either::Left(body) = &lam.body {
                let mut names: LPooled<Vec<ArcStr>> = LPooled::take();
                body.fold((), &mut |(), e| {
                    if let expr::ExprKind::Ref { name } = &e.kind {
                        let s: &str = name.0.as_ref();
                        if netidx::path::Path::levels(s) == 1 {
                            if let Some(base) = netidx::path::Path::basename(s) {
                                names.push(ArcStr::from(base));
                            }
                        }
                    }
                });
                for base in names.iter() {
                    let mp = crate::expr::ModPath::from_iter([base.as_str()]);
                    if ctx.env.lookup_bind(&scope.lexical, &mp).is_some() {
                        continue;
                    }
                    let cap_id =
                        def.env.lookup_bind(&def.scope.lexical, &mp).map(|(_, b)| b.id);
                    if let Some(id) = cap_id {
                        ctx.env.alias_variable(&scope.lexical, base.as_str(), id);
                    }
                }
            }
        }
        compile(ctx, BitFlags::empty(), self.spec.clone(), scope, self.spec.id)
            .expect("Lambda::clone_rebind recompile failed")
    }

    fn delete(&mut self, _ctx: &mut ExecCtx<R, E>) {}

    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {}

    fn typ(&self) -> &Type {
        &self.typ
    }

    fn typecheck0(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        let def = self
            .def
            .downcast_ref::<LambdaDef<R, E>>()
            .ok_or_else(|| anyhow!("failed to unwrap lambda"))?;
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
                .typecheck0(ctx, &mut faux_args)
                .with_context(|| ErrorContext(Update::<R, E>::spec(self).clone()));
            // Retain a check `Apply` for every BUILTIN lambda so
            // `CallSite::typecheck1` can run its resolved-`typecheck1`
            // (validation / type extraction). A user `GXLambda`
            // (`ApplyView::Lambda`) is discarded — its body is not
            // re-checked per call site. Structural test replacing the
            // old `needs_callsite` flag.
            if matches!(f.view(), crate::ApplyView::Lambda(_)) {
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

    /// A lambda *definition* node has no children in the main node tree —
    /// its body is reached and `typecheck1`'d per call site through
    /// `GXLambda::Apply::typecheck1`. Nothing to do here.
    fn typecheck1(&mut self, _ctx: &mut ExecCtx<R, E>) -> Result<()> {
        Ok(())
    }

    fn view(&self) -> crate::NodeView<'_, R, E> {
        crate::NodeView::Lambda(self)
    }
}
