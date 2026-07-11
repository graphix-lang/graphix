use super::{NOP, Nop, bind::Ref, compiler::compile};
use crate::{
    Apply, ApplyView, ApplyViewMut, BindId, CFlag, Event, ExecCtx, LambdaId, Node,
    NodeView, PendingTailCall, PrintFlag, Refs, Rt, Scope, StaticFnArg, TagValue, Update,
    UserEvent, deref_typ,
    expr::{ErrorContext, Expr, ExprId, ExprKind},
    fusion::{
        self,
        emit::{BodyCx, CompiledExpr, emit_dyncall_node, emit_lambda_call_node},
    },
    node::lambda::{GXLambda, LambdaDef},
    typ::{FnArgKind, FnType, TVar, Type},
    wrap,
};
use ahash::{AHashMap, AHashSet};
use anyhow::{Context, Result, anyhow, bail};
use arcstr::ArcStr;
use enumflags2::BitFlags;
use netidx::subscriber::Value;
use parking_lot::Mutex;
use poolshark::local::LPooled;
use smallvec::SmallVec;
use std::{
    collections::hash_map::Entry,
    mem,
    sync::atomic::{AtomicBool, Ordering},
};
use triomphe::Arc as TArc;

/// Reject a direct call to a same-cycle (`EffectKind::Sync`) variadic
/// builtin that supplies NO positional arguments, when the builtin's
/// signature has no positional formals — e.g. `str::concat()`,
/// `str::join(#sep: ",")`, `sum()`. Such a call has no data inputs:
/// the node can never fire, so the program just contains a silent
/// bottom the user has to debug ("where did my value go?"). If a
/// value that never arrives is what's wanted, `never()` says so
/// explicitly (and is exempt here — it's declared `Async`, whose
/// contract is "later, autonomously, or never"). Only a direct `Ref`
/// to the builtin binding is statically checkable; a builtin passed
/// around as a first-class value degrades to the (safe) runtime
/// bottom instead.
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
    let Some((_, bind)) = ctx.env.lookup_bind(&scope.lexical, path) else {
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) enum ArgKey {
    Positional(usize),
    Named(ArcStr),
}

#[derive(Debug)]
pub(crate) struct Arg<R: Rt, E: UserEvent> {
    pub id: BindId,
    pub node: Option<Node<R, E>>,
    pub is_default: bool,
    /// Lazily computed: the arg expression references no bindings, so
    /// its published value is identical in every evaluation frame —
    /// see the invariant-arg re-delivery in `update` and the removal
    /// exemption in `reset_replay`.
    pub invariant: std::sync::OnceLock<bool>,
}

impl<R: Rt, E: UserEvent> Arg<R, E> {
    pub(crate) fn new(id: BindId, node: Option<Node<R, E>>, is_default: bool) -> Self {
        Arg { id, node, is_default, invariant: std::sync::OnceLock::new() }
    }

    fn is_invariant(&self) -> bool {
        *self.invariant.get_or_init(|| {
            self.node.as_ref().is_some_and(|n| {
                let mut refs = Refs::default();
                n.refs(&mut refs);
                refs.refed.is_empty()
            })
        })
    }
}

/// Collect every `Type::Fn` arm reachable in `t` into `out` — a bare
/// `Fn`, or the `Fn` arms of a `[fn(...), null]` / Set union (the typical
/// optional-callback shape). Used by `CallSite::typecheck1` to find the
/// callbacks passed in a fn-typed argument.
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

/// Drive the resolved-`typecheck1` ("CallSite phase") of the lambda `id`
/// against `resolved`. Looks the lambda up in `ctx.lambda_defs` and, if it
/// retained a check `Apply` (`def.check`), runs that apply's `typecheck1`.
/// This is the body of the former deferred check, called directly from
/// `CallSite::typecheck1`.
fn finalize_lambda<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<R, E>,
    id: LambdaId,
    resolved: &FnType,
    spec: &TArc<Expr>,
) -> Result<()> {
    if let Some(val) = ctx.lambda_defs.get(&id).cloned() {
        let ldef = val
            .downcast_ref::<LambdaDef<R, E>>()
            .expect("failed to unwrap lambda for typecheck1");
        if let Some(apply) = &mut *ldef.check.lock() {
            // Scratch `def.check` firing: empty `fn_args` — the HOF
            // callback hook only fires on the bound apply (try_static_resolve).
            apply
                .typecheck1(ctx, &mut [], resolved, &[])
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
) -> Result<AHashMap<ArgKey, Arg<R, E>>> {
    let mut res = AHashMap::default();
    let mut pos = 0;
    for (name, expr) in args.iter() {
        let node = Some(compile(ctx, flags, expr.clone(), scope, top_id)?);
        match name {
            None => {
                res.insert(ArgKey::Positional(pos), Arg::new(BindId::new(), node, false));
                pos += 1;
            }
            Some(k) => match res.entry(ArgKey::Named(k.clone())) {
                Entry::Occupied(_) => bail!("duplicate named argument {k}"),
                Entry::Vacant(e) => {
                    e.insert(Arg::new(BindId::new(), node, false));
                }
            },
        }
    }
    Ok(res)
}

/// What a [`CallSite`] knows about its callee. Folds the former
/// (`function: Option<(Value, Box<dyn Apply>)>`, `statically_resolved`,
/// `first_static_update`) trio into one enum so the previously
/// representable invalid state (`statically_resolved && function.is_none()`)
/// cannot occur.
#[derive(Debug)]
pub(crate) enum Callee<R: Rt, E: UserEvent> {
    /// No callee bound yet — the just-compiled state. `fnode` is
    /// re-evaluated every cycle; the first cycle it yields a `LambdaDef`
    /// Value, `update()` transitions to `DynamicBound` via `bind()`. Also
    /// the state a *builtin* callee resets to in `clone_rebind`, so the
    /// clone re-inits a fresh instance on its first update.
    DynamicUnbound,
    /// Bound to a callee that may still change cycle-to-cycle. `def` is the
    /// `LambdaDef`-wrapped Value, kept for the per-cycle IDENTITY check
    /// against `fnode.update()`; when it differs we re-`bind()`.
    /// `transient` marks a recursive-unfold binding whose instance is a
    /// pure activation record (see [`transient_body_ok`]): `update()`
    /// deletes it when its dispatch returns, transitioning to
    /// [`Callee::TransientParked`]. Never survives across `update()`
    /// calls — a transient bind and its parking happen in the same one.
    DynamicBound { def: Value, apply: Box<dyn Apply<R, E>>, transient: bool },
    /// A transient recursive callee, parked between calls. The instance
    /// built by `bind()` was DELETED when its dispatch returned — a
    /// qualifying Sync body re-entered recursively is a pure activation
    /// record, and retaining one per dynamic call held the whole call
    /// TREE alive, O(2^depth) memory (fib(28) = 1M instances / 9.6GB).
    /// `def` is kept because the `fnode` Ref delivers the LambdaDef only
    /// once — the next genuine call re-`bind()`s from it. `ext_refs` are
    /// the deleted instance's external refs (captures): a retained
    /// instance is reactively LIVE to its captures, so a capture firing
    /// must also trigger the re-bind (the JIT twin: captures are kernel
    /// inputs).
    TransientParked { def: Value, ext_refs: Box<[BindId]> },
    /// Pre-bound at compile time by [`CallSite::try_static_resolve`]:
    /// `fnode` provably resolves to one `LambdaDef`, so the per-cycle
    /// identity check + lazy bind is skipped (`fnode.update()` still runs
    /// for side effects, value discarded). `def` is the held LambdaDef the
    /// read-through accessors recover typed; `first_update` primes the
    /// body's external refs exactly once.
    Static { def: Value, apply: Box<dyn Apply<R, E>>, first_update: bool },
}

impl<R: Rt, E: UserEvent> Callee<R, E> {
    fn is_bound(&self) -> bool {
        !matches!(self, Callee::DynamicUnbound | Callee::TransientParked { .. })
    }

    fn apply(&self) -> Option<&dyn Apply<R, E>> {
        match self {
            Callee::DynamicUnbound | Callee::TransientParked { .. } => None,
            Callee::DynamicBound { apply, .. } | Callee::Static { apply, .. } => {
                Some(&**apply)
            }
        }
    }

    fn apply_mut(&mut self) -> Option<&mut (dyn Apply<R, E> + 'static)> {
        match self {
            Callee::DynamicUnbound | Callee::TransientParked { .. } => None,
            Callee::DynamicBound { apply, .. } | Callee::Static { apply, .. } => {
                Some(&mut **apply)
            }
        }
    }

    /// Reset to `DynamicUnbound`, returning the bound apply for deletion.
    /// A `TransientParked` def is discarded — the callers replace the
    /// binding wholesale (a fresh `bind`, or `delete`).
    fn take_apply(&mut self) -> Option<Box<dyn Apply<R, E>>> {
        match mem::replace(self, Callee::DynamicUnbound) {
            Callee::DynamicUnbound | Callee::TransientParked { .. } => None,
            Callee::DynamicBound { apply, .. } | Callee::Static { apply, .. } => {
                Some(apply)
            }
        }
    }
}

/// Whether a recursively-bound Sync callee instance is a pure
/// activation record — deletable when its dispatch returns (parked
/// [`Callee::TransientParked`]) with no observable difference from
/// today's retained instance. True iff the instance body —
/// transitively through every already-bound callee body — holds no
/// cross-dispatch state or obligations:
///
/// - no STATEFUL builtin call sites: a builtin `Apply` may hold
///   per-instance state (`count`, `sum`, `min`, …) that today
///   accumulates across calls, or emit per invocation (`print`,
///   `log`). Builtins declaring [`crate::BuiltIn::STATELESS`] are fine
///   — delete-and-reinit is unobservable for them. Anything else —
///   including a builtin call whose binding can't be resolved back to
///   its declaration — refuses,
/// - no `connect` (plain or deref): the write target and its
///   next-cycle delivery live in the instance,
/// - no `&x`: a reference to an instance-local binding can escape the
///   instance's lifetime.
///
/// An UNBOUND inner call site is fine only when it provably targets a
/// stable, known user lambda — that is exactly a `#203` recursive
/// back-edge (its target is an ancestor of this walk, whose own body
/// this walk covers, and its unfolds gate themselves at their own
/// bind). Anything dynamic refuses. The Sync gate in `bind()` already
/// excludes async bodies, `~`/`any`/`try` (classified Async), and
/// fn-typed-parameter calls (dynamic callees are Async).
fn transient_body_ok<R: Rt, E: UserEvent>(
    root: &GXLambda<R, E>,
    ctx: &ExecCtx<R, E>,
) -> bool {
    let mut seen: LPooled<nohash::IntSet<LambdaId>> = LPooled::take();
    seen.insert(root.id());
    let mut ok = true;
    let mut stack: LPooled<Vec<&Node<R, E>>> = LPooled::take();
    stack.push(root.body());
    while let Some(node) = stack.pop() {
        if !ok {
            break;
        }
        let mut to_descend: LPooled<Vec<&Node<R, E>>> = LPooled::take();
        fusion::for_each_node(node, &mut |n| {
            if !ok {
                return;
            }
            match n.view() {
                NodeView::Connect(_) | NodeView::ConnectDeref(_) | NodeView::ByRef(_) => {
                    ok = false
                }
                NodeView::CallSite(cs) => match cs.callee_apply() {
                    Some(a) => match a.view() {
                        ApplyView::Lambda(g) => {
                            if seen.insert(g.id()) {
                                to_descend.push(g.body());
                            }
                        }
                        // Resolve the builtin binding back to its
                        // declaration (the `callee_effect` resolution)
                        // and consult `BuiltIn::STATELESS`; a call that
                        // can't be resolved refuses.
                        ApplyView::BuiltIn => {
                            let stateless = match &cs.fnode().spec().kind {
                                ExprKind::Ref { name } => ctx
                                    .env
                                    .lookup_bind(&cs.scope().lexical, name)
                                    .and_then(|(_, bind)| {
                                        let key = (bind.scope.clone(), bind.name.clone());
                                        ctx.builtin_bindings.get(&key)
                                    })
                                    .map(|info| ctx.builtin_stateless(info.name.as_str()))
                                    .unwrap_or(false),
                                _ => false,
                            };
                            if !stateless {
                                ok = false;
                            }
                        }
                    },
                    None => {
                        let known = match cs.fnode().view() {
                            NodeView::Ref(r) => {
                                !ctx.unstable_bindings.contains(&r.id)
                                    && ctx
                                        .bind_to_lambda
                                        .get(&r.id)
                                        .or_else(|| ctx.rt.cached().get(&r.id))
                                        .map(|v| {
                                            v.downcast_ref::<LambdaDef<R, E>>().is_some()
                                        })
                                        .unwrap_or(false)
                            }
                            _ => false,
                        };
                        if !known {
                            ok = false;
                        }
                    }
                },
                _ => (),
            }
        });
        stack.extend(to_descend.drain(..));
    }
    ok
}

#[derive(Debug)]
pub struct CallSite<R: Rt, E: UserEvent> {
    pub(super) spec: TArc<Expr>,
    pub(super) ftype: Option<FnType>,
    pub(super) resolved_ftype: Option<FnType>,
    pub(super) rtype: Type,
    pub(crate) fnode: Node<R, E>,
    pub(crate) args: AHashMap<ArgKey, Arg<R, E>>,
    pub(super) arg_refs: Vec<Node<R, E>>,
    /// The callee — static/dynamic-bound/unbound. See [`Callee`]. Replaces
    /// the former `function` + `statically_resolved` + `first_static_update`
    /// trio (the `Static` tag carries `first_update`; the old invalid
    /// `statically_resolved && function == None` state is unrepresentable).
    pub(crate) callee: Callee<R, E>,
    pub(super) flags: BitFlags<CFlag>,
    pub(super) scope: Scope,
    pub(super) top_id: ExprId,
    /// Set by `analysis::analyze` when THIS call site is a tail-position
    /// self-call inside a sync, tail-recursive lambda body. At runtime
    /// the interpreter (`CallSite::update`) reads it to loop in place
    /// (stash args in `ctx.pending_tail_call`, return without dispatch)
    /// instead of recursing on the Rust stack. Atomic because the
    /// analysis writes it through a shared `&CallSite`.
    pub(crate) is_self_tail_call: AtomicBool,
    /// The recursive call's arg `BindId`s in callee-signature order —
    /// what the tail-loop rebinds each iteration. `Some` iff
    /// `is_self_tail_call`. Written once by the analysis.
    pub(crate) tail_arg_order: Mutex<Option<Box<[BindId]>>>,
    /// The `LambdaId` of the tail-recursive callee — the loop key the
    /// owning `GXLambda::update` matches `ctx.pending_tail_call` against.
    /// `Some` iff `is_self_tail_call`.
    pub(crate) callee_lambda_id: Mutex<Option<LambdaId>>,
}

impl<R: Rt, E: UserEvent> CallSite<R, E> {
    /// The resolved function type at this call site. Populated by
    /// `typecheck0` during the typechecker's call-site unification
    /// pass — after typecheck, every reachable CallSite has this set
    /// to the lambda's FnType with the call-site's TVars unified in.
    ///
    /// `None` only if the typechecker hasn't run yet, or this call
    /// site reached an error before unification.
    pub fn ftype(&self) -> Option<&FnType> {
        self.ftype.as_ref()
    }

    /// The resolved-and-deref'd function type at this call site. Same
    /// as `ftype()` but with all bound TVars dereferenced to their
    /// concrete forms — produced by `FnType::resolve_tvars`. Cached
    /// inside `Self::bind` after the lambda binds the value, so a
    /// post-typecheck consumer can ask for it cheaply.
    pub fn resolved_ftype(&self) -> Option<&FnType> {
        self.resolved_ftype.as_ref()
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

    /// The lexical+dynamic scope this call site was compiled in — used by
    /// `analysis::analyze` to resolve a builtin callee's `(scope, name)`
    /// key for its declared effect.
    pub(crate) fn scope(&self) -> &Scope {
        &self.scope
    }

    /// View the [`Apply`] this CallSite is currently bound to. Returns
    /// `None` if the CallSite hasn't bound yet (the typical
    /// just-compiled state — runtime `bind()` fires lazily on the
    /// first cycle the `fnode` produces a LambdaDef Value).
    /// `Some(view)` after either the runtime dynamic bind or the
    /// `try_static_resolve` step in `typecheck1` has populated
    /// `self.callee`.
    ///
    /// Used by fusion to descend through a resolved call site into a
    /// user lambda's body. See [`ApplyView`] for the variants.
    pub fn resolved_apply(&self) -> Option<ApplyView<'_, R, E>> {
        self.callee.apply().map(|a| a.view())
    }

    /// The resolved callee as a raw `&dyn Apply` (vs the typed
    /// [`ApplyView`] of [`Self::resolved_apply`]). Fusion discovery uses
    /// it to call [`crate::Apply::for_each_hof_callback_body`] — reaching
    /// a HOF builtin's inline-emitted callback body, which the opaque
    /// `ApplyView::BuiltIn` doesn't expose.
    pub fn callee_apply(&self) -> Option<&dyn Apply<R, E>> {
        self.callee.apply()
    }

    /// Mutable counterpart to [`Self::resolved_apply`]. Fusion uses
    /// this when it needs to splice an inner sub-kernel into a Node
    /// reachable through the resolved Apply — e.g. a
    /// [`ApplyViewMut::Lambda`]'s body Node.
    pub fn resolved_apply_mut(&mut self) -> Option<ApplyViewMut<'_, R, E>> {
        self.callee.apply_mut().map(|a| a.view_mut())
    }

    /// Signature-order `Ref` Nodes — one per formal argument in the
    /// function's [`FnType`], with labeled defaults already resolved.
    /// `None` until the CallSite has bound (matches
    /// [`Self::resolved_apply`]).
    ///
    /// Together with [`Self::arg_positional`] / [`Self::arg_named`]
    /// (which expose the original source-order call-site Nodes),
    /// this gives [`crate::Apply::emit_clif`] impls both views of
    /// the arg list.
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
            spec,
            ftype: None,
            resolved_ftype: None,
            rtype: Type::empty_tvar(),
            fnode,
            args,
            arg_refs: Vec::new(),
            callee: Callee::DynamicUnbound,
            flags,
            top_id,
            scope: scope.clone(),
            is_self_tail_call: AtomicBool::new(false),
            tail_arg_order: Mutex::new(None),
            callee_lambda_id: Mutex::new(None),
        };
        Ok(Box::new(site))
    }

    fn make_ref(&self, id: BindId, typ: Type, spec: TArc<Expr>) -> Node<R, E> {
        Box::new(Ref { spec, typ, id, top_id: self.top_id })
    }

    /// Shared "compile arg_refs + call InitFn + typecheck" pipeline
    /// used by both runtime [`Self::bind`] and the compile-time
    /// [`Self::resolve_static`] pass. The two callers differ only in
    /// what they need to do to a live update cycle's `Event` — when
    /// running at compile time there is no `Event`, when running
    /// from inside an `update()` cycle we must prime the freshly-
    /// compiled defaults' external refs into `event.variables`.
    /// `prime_default_refs` is the per-default-Node hook that handles
    /// that event-priming; it's a no-op closure for the static path.
    fn setup_bind<F>(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        scope: &Scope,
        flags: BitFlags<CFlag>,
        f: &LambdaDef<R, E>,
        mut prime_default_refs: F,
    ) -> Result<Box<dyn Apply<R, E>>>
    where
        F: FnMut(&mut ExecCtx<R, E>, &Refs),
    {
        if self.resolved_ftype.is_none() {
            if let Some(ftype) = &self.ftype {
                self.resolved_ftype = Some(ftype.resolve_tvars());
            }
        }
        let mut flags = flags;
        // we already warned about this
        flags.remove(CFlag::WarnUnhandled);
        // Clean up previous binding (no-op on a fresh CallSite).
        if let Some(mut old_f) = self.callee.take_apply() {
            old_f.delete(ctx);
        }
        for mut n in self.arg_refs.drain(..) {
            n.delete(ctx);
        }
        // Remove and delete default args from a previous bind — or
        // the `Nop` placeholders the typechecker inserts for missing
        // labeled-default args. Either way, the real default Node is
        // re-created in the arg_refs loop below.
        self.args.retain(|_, arg| {
            if arg.is_default {
                ctx.rt.cached_mut().remove(&arg.id);
                if let Some(mut n) = arg.node.take() {
                    n.delete(ctx);
                }
                false
            } else {
                true
            }
        });
        // Build arg_refs in function-signature order.
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
                        // PER-CALLSITE default checking (Eric's ruling,
                        // 2026-07-09): a default participates exactly
                        // when the caller omits the arg, and typechecks
                        // HERE against this SITE's instantiated
                        // signature — never at the def gate (where the
                        // rigid check rejected any generic-typed
                        // default: rand's f64 seeds vs `'a: [Float,
                        // Int]`). The containment BINDS the site's
                        // cells, so an omitting site infers from the
                        // default (`rand()` gets `'a := f64`) while a
                        // providing site never sees it. Loud: a
                        // mismatch is a compile error on the static
                        // path and a bind error on the dynamic path.
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
                // Positional argument — find the pos_idx'th positional arg.
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
        // Handle vargs — remaining positional args.
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
        let mut rf = (f.init)(
            scope,
            ctx,
            &mut self.arg_refs,
            self.resolved_ftype.as_ref(),
            self.top_id,
        )?;
        let swallowed = rf.typecheck0(ctx, &mut self.arg_refs);
        if let Err(e) = &swallowed {
            if std::env::var_os("GXDBG_SWALLOW").is_some() {
                eprintln!("SWALLOWED-TC0 at {}: {e:#}", self.spec);
            }
        }
        Ok(rf)
    }

    /// Release the runtime var registrations a [`Callee::TransientParked`]
    /// binding holds for its wake-set (see the parking block in
    /// [`Update::update`]). Idempotent-by-construction callers: `bind()`
    /// (the fresh instance re-registers its own refs) and `delete()`.
    fn release_parked(&mut self, ctx: &mut ExecCtx<R, E>) {
        if let Callee::TransientParked { ext_refs, .. } = &self.callee {
            for id in ext_refs.iter() {
                ctx.rt.unref_var(*id, self.top_id);
            }
        }
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
        self.release_parked(ctx);
        // Build arg_refs + InitFn + typecheck. The closure primes
        // each freshly-compiled default's external refs into
        // `event.variables` from `ctx.cached`, so the bound function
        // sees outer-binding values on its first update inside this
        // same cycle.
        let apply = self.setup_bind(ctx, &scope, flags, f, |ctx, refs| {
            refs.with_external_refs(|id| {
                if let Some(v) = ctx.rt.cached().get(&id) {
                    if let Entry::Vacant(e) = event.variables.entry(id) {
                        // FIRED: first-dispatch init semantics (a fresh
                        // bind sees everything as new)
                        e.insert(TagValue::fired(v.clone()));
                        set.push(id);
                    }
                }
            });
        })?;
        self.callee = Callee::DynamicBound { def: fv, apply, transient: false };
        // The lazy-bound body was compiled fresh AFTER the program-wide
        // typecheck1 + `analysis::analyze` passes, so its nested call
        // sites are unresolved and nothing in the subtree carries
        // effect/recursion/tail facts. Mirror `resolve_static`'s #203
        // cascade (resolve the body's own call sites; errors swallowed —
        // an unresolved inner call just stays lazy), then run the
        // analysis over the fresh subtree. Without this a tail-recursive
        // `let rec` nested in the body (e.g. inside an HOF callback
        // slot) stack-recursed into the call-depth guard and bottomed at
        // ~256 where the JIT — and a compile-time-resolved node-walk
        // site — tail-looped to the value (soak-jul06c B8).
        if let Some(apply) = self.callee.apply_mut()
            && matches!(apply.view(), ApplyView::Lambda(_))
        {
            let lid = f.id.inner();
            let resolving = ctx.fusion.resolving.clone();
            if resolving.lock().insert(lid) {
                let ftype = f.typ.clone();
                if let Err(e) = apply.typecheck1(ctx, &mut [], &ftype, &[]) {
                    log::trace!("bind: lazy-bound callee body typecheck1 failed: {e:#}");
                }
                resolving.lock().remove(&lid);
            }
            if let ApplyView::Lambda(g) = apply.view() {
                let self_bind = match self.fnode.view() {
                    NodeView::Ref(r) => Some(r.id),
                    _ => None,
                };
                crate::analysis::analyze_bound_callee(g, self_bind, ctx);
            }
        }
        // Transient-recursion gate: this bind is a recursive unfold
        // (the def is already active on the dispatch stack — the
        // enclosing body IS an activation of the same lambda) and the
        // fresh instance is a pure activation record. Mark it
        // transient: `update()` deletes it when the dispatch returns
        // and parks the def, so the recursion holds O(depth) instances
        // instead of one per dynamic call (the full call tree).
        if ctx.active_lambdas.contains_key(&f.id) && f.intrinsic_effect.lock().is_sync() {
            let ok = match self.callee.apply() {
                Some(a) => match a.view() {
                    ApplyView::Lambda(g) => transient_body_ok(g, ctx),
                    ApplyView::BuiltIn => false,
                },
                None => false,
            };
            if ok && let Callee::DynamicBound { transient, .. } = &mut self.callee {
                *transient = true;
            }
        }
        // Ensure all arg values are available for the init cycle.
        // Defaults need to be updated for the first time (with init=true
        // since Constant only fires on init); existing args may not have
        // changed this cycle but their cached values must be visible to
        // the newly bound function body.
        let prev_init = mem::replace(&mut event.init, true);
        for arg in self.args.values_mut() {
            if arg.is_default {
                if let Some(ref mut node) = arg.node {
                    if let Some(tv) = node.update(ctx, event) {
                        let v = tv.value();
                        ctx.rt.cached_mut().insert(arg.id, v.clone());
                        event.variables.insert(arg.id, TagValue::fired(v));
                        set.push(arg.id);
                    }
                }
            } else if let Entry::Vacant(e) = event.variables.entry(arg.id) {
                if let Some(v) = ctx.rt.cached().get(&arg.id) {
                    e.insert(TagValue::fired(v.clone()));
                    set.push(arg.id);
                }
            }
        }
        event.init = prev_init;
        Ok(())
    }

    /// Pre-bind this CallSite to a statically known `LambdaDef` at
    /// compile time, replacing the lazy "bind on first call" path
    /// `bind()` runs from inside `update()`. Called from
    /// [`Self::try_static_resolve`] (at the end of `typecheck1`) for
    /// every CallSite whose function expression can be proven to resolve
    /// to exactly one Lambda (i.e. a `Ref` to a non-`<-`-target binding
    /// whose value is a Lambda, or a direct lambda literal `(|x|…)(42)`).
    ///
    /// Same arg-build + InitFn + typecheck work as [`Self::bind`],
    /// minus the event-cycle side effects. The runtime's first
    /// update through this CallSite handles arg init-priming via the
    /// `first_static_update` flag set here.
    pub fn resolve_static(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        def: &LambdaDef<R, E>,
        fv: Value,
    ) -> Result<()> {
        if matches!(self.callee, Callee::Static { .. }) {
            // Idempotent.
            return Ok(());
        }
        // #203 self/mutual-recursion cut: if `def`'s body is CURRENTLY being
        // driven through typecheck1 (below), this call is a recursive
        // back-edge. Leave it `DynamicUnbound` — a self-call is handled by
        // the `self_call` emit mechanism, a mutual back-edge de-fuses cleanly
        // — and don't recurse the drive.
        if ctx.fusion.resolving.lock().contains(&def.id.inner()) {
            return Ok(());
        }
        let scope = self.scope.clone();
        let apply = self.setup_bind(ctx, &scope, self.flags, def, |_, _| {})?;
        self.callee = Callee::Static { def: fv, apply, first_update: true };
        // #203: drive the resolved callee's BODY through typecheck1 so its OWN
        // nested call sites resolve (the cascade `GXLambda::typecheck1 →
        // body.typecheck1`). `setup_bind` ran only `typecheck0`, so without
        // this a call inside the body stays `DynamicUnbound` and can't fuse —
        // the gap that made an HOF callback's `iterate(..)` (and any nested
        // call) un-fusable. Guarded by the in-progress `LambdaId` set (the
        // back-edge skip above) so recursion terminates. NOT fusion-gated:
        // `analysis::analyze`'s tail-loop marking only reaches sites with a
        // `resolved_apply`, and the NODE-WALK consumes those marks — with
        // the cascade off under `FusionDisabled`, a rec lambda NESTED in
        // another lambda's body stack-recursed (and, past the depth guard,
        // bottomed) where the fused path tail-looped: a mode divergence the
        // depth guard made value-visible (fuzz/triage-fuzzer-v2/
        // divergence_000008, 2026-07-03). Errors are swallowed — a body
        // that fails its first typecheck1 stays partly-resolved and its
        // calls de-fuse (the interp lazy-binds them); never a NEW compile
        // failure.
        if let Some(apply) = self.callee.apply_mut()
            && matches!(apply.view(), ApplyView::Lambda(_))
        {
            let lid = def.id.inner();
            let resolving = ctx.fusion.resolving.clone();
            resolving.lock().insert(lid);
            let ftype = def.typ.clone();
            if let Err(e) = apply.typecheck1(ctx, &mut [], &ftype, &[]) {
                log::trace!("#203: callee body typecheck1 failed (de-fuse): {e:#}");
            }
            resolving.lock().remove(&lid);
        }
        Ok(())
    }

    /// Static call resolution — folded in from the deleted
    /// `static_resolve` pass and invoked at the end of
    /// [`Update::typecheck1`], by which point `ctx.bind_to_lambda` is
    /// complete (built during `typecheck0`) and this site's callbacks
    /// are finalized. If the function expression resolves to a single
    /// known `LambdaDef` — a `Ref` to a non-`<-`-target lambda binding
    /// (looked up in `bind_to_lambda`, with a fallback to `ctx.cached`
    /// for separately-compiled stdlib callees), or a direct lambda
    /// literal — pre-bind it via [`Self::resolve_static`]. Then give HOF
    /// builtins the chance to pre-materialize their callback
    /// `analysis_pred`s via the bound-instance firing of
    /// [`Apply::typecheck1`] (with the discovered `fn_args`). No-op for
    /// dynamic call sites. (`bind_to_lambda` is a compile-time analysis
    /// map kept distinct from runtime `cached`; the `.or_else(cached)`
    /// only READS stdlib lambdas already legitimately there.)
    fn try_static_resolve(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        if matches!(self.callee, Callee::Static { .. }) {
            return Ok(());
        }
        // Determine the target without holding a borrow on `self.fnode`
        // / `ctx` past the match — the cloned `Value` owns its LambdaDef,
        // so the `&mut self` for `resolve_static` is unencumbered.
        let target: Option<Value> = match self.fnode.view() {
            NodeView::Ref(r) => {
                if ctx.unstable_bindings.contains(&r.id) {
                    None
                } else {
                    ctx.bind_to_lambda
                        .get(&r.id)
                        .or_else(|| ctx.rt.cached().get(&r.id))
                        .cloned()
                }
            }
            NodeView::Lambda(l) => Some(l.def_value().clone()),
            _ => None,
        };
        let Some(fv) = target else { return Ok(()) };
        let Some(def) = fv.downcast_ref::<LambdaDef<R, E>>() else {
            return Ok(());
        };
        self.resolve_static(ctx, def, fv.clone())?;
        // HOF callback pre-materialization: every fn-typed positional arg
        // that itself resolves to a known lambda becomes a `StaticFnArg`
        // handed to the just-bound Apply.
        let ftype = match self.resolved_ftype().or_else(|| self.ftype()) {
            Some(ft) => ft.clone(),
            None => return Ok(()),
        };
        let mut fn_arg_targets: Vec<(usize, Value)> = Vec::new();
        for (i, farg) in ftype.args.iter().enumerate() {
            if !farg.typ.with_deref(|t| matches!(t, Some(Type::Fn(_)))) {
                continue;
            }
            let Some(arg_node) = self.arg_positional(i) else { continue };
            match arg_node.view() {
                NodeView::Lambda(l) => {
                    fn_arg_targets.push((i, l.def_value().clone()));
                }
                NodeView::Ref(r) => {
                    if ctx.unstable_bindings.contains(&r.id) {
                        continue;
                    }
                    if let Some(fv) = ctx.bind_to_lambda.get(&r.id) {
                        fn_arg_targets.push((i, fv.clone()));
                    }
                }
                _ => {}
            }
        }
        if fn_arg_targets.is_empty() {
            return Ok(());
        }
        let Some(apply) = self.callee.apply_mut() else {
            return Ok(());
        };
        let mut fn_args: Vec<StaticFnArg<'_, R, E>> =
            Vec::with_capacity(fn_arg_targets.len());
        for (idx, fv) in &fn_arg_targets {
            if let Some(def) = fv.downcast_ref::<LambdaDef<R, E>>() {
                fn_args.push(StaticFnArg { arg_idx: *idx, lambda: def });
            }
        }
        if fn_args.is_empty() {
            return Ok(());
        }
        // PER-CALLSITE ELABORATION (sync-subset P4): when the bound
        // callee is a user lambda, register each statically-known
        // fn-typed arg under the INSTANCE's arg-pattern BindId in
        // `bind_to_lambda` for the duration of the re-driven body
        // typecheck1 below. The instance's body callsites then
        // statically resolve calls to the lambda PARAM (`f(v)` inside
        // `|a, f| sync { … f(v) … }`) exactly like calls to a lambda
        // binding — per-instance BindIds are fresh per callsite, so
        // there is no cross-site contamination by construction, and
        // fusion needs no new code (it reads resolved CallSites). This
        // is what lets an in-language HOF compile to the same native
        // loop MapQ's builtins get.
        let mut param_binds: LPooled<Vec<BindId>> = LPooled::take();
        if let ApplyView::Lambda(g) = apply.view() {
            for (idx, fv) in &fn_arg_targets {
                if let Some(id) = g.args().get(*idx).and_then(|p| p.single_bind_id()) {
                    ctx.bind_to_lambda.insert(id, fv.clone());
                    param_binds.push(id);
                }
            }
        }
        // Bound-instance firing of the CallSite phase: hand the resolved
        // callbacks to the bound apply's `typecheck1`. `fn_args` non-empty
        // here (HOF call site) is what distinguishes this from the scratch
        // `def.check` firing in `finalize_lambda` (empty `fn_args`).
        // For a user-lambda callee this re-drives the body walk, during
        // which still-unbound nested sites re-attempt static resolution
        // with the param bindings above in scope.
        let res = apply.typecheck1(ctx, &mut [], &ftype, &fn_args);
        for id in param_binds.drain(..) {
            ctx.bind_to_lambda.remove(&id);
        }
        res?;
        Ok(())
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for CallSite<R, E> {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        event: &mut Event<E>,
    ) -> Option<TagValue> {
        let mut set: LPooled<Vec<BindId>> = LPooled::take();
        // A FIRED (or tainted) arg production this cycle — the genuine
        // -call signal (a stale production is a value-channel refresh,
        // not an event).
        let mut arg_fired = false;
        // Update all arg nodes every cycle, publishing values via bind IDs
        for arg in self.args.values_mut() {
            if let Some(ref mut node) = arg.node {
                if let Some(tv) = node.update(ctx, event) {
                    let (v, tag) = tv.into_parts();
                    arg_fired |= tag.triggers();
                    if tag.is_tainted() {
                        // poison the formal delivery; keep the
                        // placeholder out of the cross-cycle store
                        event.variables.insert(arg.id, TagValue::tainted(Value::Null));
                    } else {
                        ctx.rt.cached_mut().insert(arg.id, v.clone());
                        event.variables.insert(arg.id, TagValue::tagged(v, tag));
                    }
                    set.push(arg.id);
                }
            }
        }
        // FRAME-ONLY: deliver quiet args on the STALE channel when the
        // frame's private variables map lacks them and the runtime
        // cache still holds their value (invariant args survive
        // `reset_replay`; a closed arg fires exactly once ever and
        // can't re-produce inside a frame). The kernel twin: DynCall
        // marshals every arg slot on every call, quiet ones with STALE
        // discs. The stale tag is what keeps this from re-FIRING
        // anything — a const-arg callee stays quiet (the reactive-land
        // effectful-callee hazard that forced the old fired
        // re-delivery to be gated is structurally gone), and the tag
        // rides into the callee's formals so its body computes on the
        // value channel.
        if ctx.frame_depth > 0 {
            for arg in self.args.values() {
                if !event.variables.contains_key(&arg.id)
                    && let Some(v) = ctx.rt.cached().get(&arg.id)
                {
                    event.variables.insert(arg.id, TagValue::stale(v.clone()));
                    set.push(arg.id);
                }
            }
        }
        // Tail-call interception. When `analysis::analyze` flagged this
        // call as a tail-position self-call inside a sync tail-recursive
        // body, don't bind/dispatch (which would recurse on the Rust stack
        // and overflow). Instead stash the just-evaluated rebind args and
        // return — the enclosing `GXLambda::update` loop rebinds the
        // formals and re-runs the body, looping in place.
        //
        // Gated on this being a GENUINE call this cycle: an arg fired
        // (`set`), or we're under an init-forced view (the callsite's
        // first dispatch, a loop re-entry, an arm wake). The cached
        // back-fill below then completes any quiet args (combineLatest,
        // e.g. a capture in `f(n - 1, cap)`). Ungated, a PASSIVE re-poll
        // (nothing fired) collected an entire arg set from stale cache
        // and re-entered the loop — an infinite pure tail loop re-wedged
        // on EVERY cycle any event flowed, needing one interrupt per
        // cycle where the JIT wedges once and quiesces (soak jul04
        // items 3/4). A quiet tail self-call contributes nothing this
        // cycle — return None WITHOUT dispatching: falling through to
        // the normal path would consume the callee's first-dispatch
        // init-forcing and re-create the wedge one level deeper.
        if self.is_self_tail_call.load(Ordering::Relaxed) {
            let order = self.tail_arg_order.lock();
            let lambda = *self.callee_lambda_id.lock();
            if let (Some(order), Some(lambda)) = (order.as_ref(), lambda) {
                if !event.init && !arg_fired {
                    for id in set.drain(..) {
                        event.variables.remove(&id);
                    }
                    return None;
                }
                let args: SmallVec<[Value; 4]> = order
                    .iter()
                    .filter_map(|id| ctx.rt.cached().get(id).cloned())
                    .collect();
                if args.len() == order.len() {
                    debug_assert!(ctx.pending_tail_call.is_none());
                    ctx.pending_tail_call = Some(PendingTailCall { lambda, args });
                    for id in set.drain(..) {
                        event.variables.remove(&id);
                    }
                    return None;
                }
            }
        }
        // Statically resolved fast path. The `try_static_resolve` step
        // in `typecheck1` already invoked `(def.init)(...)` and stored
        // the Apply on `self.callee`. We still run
        // `fnode.update` for its side effects (Ref unref-counts,
        // downstream `ctx.cached` writes by other nodes that share
        // the binding's update path) but ignore the value. On the
        // very first cycle we emulate the priming the dynamic
        // `bind=true` arm runs once when a fresh bind happens.
        // `fnode.update` runs every cycle regardless of whether the
        // function value can ever change — the function expression
        // can have side effects. We only skip the value-equality
        // check + lazy `bind()` arm when we already pre-bound the
        // call site at compile time. Re-using `bound=true` on the
        // first statically-resolved cycle drives the existing
        // priming arm below.
        // `fnode.update` runs every cycle for its side effects, regardless
        // of variant — evaluate it before the `Static` arm that discards
        // its value (mirrors the old tuple scrutinee's eager evaluation).
        let fv_new = self.fnode.update(ctx, event).map(|tv| tv.value());
        let bound = if let Callee::Static { first_update, .. } = &mut self.callee {
            let first = *first_update;
            *first_update = false;
            first
        } else {
            match fv_new {
                None => false,
                Some(v) => {
                    // The immutable `matches!` borrow ends before `self.bind`'s
                    // `&mut self` below. A parked def is "same" too — the
                    // wake gate below decides whether it re-binds this cycle.
                    let same = matches!(
                        &self.callee,
                        Callee::DynamicBound { def, .. }
                        | Callee::TransientParked { def, .. } if def == &v
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
        // A parked transient callee (see `Callee::TransientParked`)
        // re-binds from its stashed def on a GENUINE call: an arg fired
        // this cycle, an init-forced view (an enclosing fresh bind's
        // first dispatch, an arm wake), or one of the deleted instance's
        // captures fired — the retained instance this replaces was
        // reactively live to its captures. Quiet cycles stay parked:
        // the retained twin's passive re-poll produced nothing either.
        let bound = match &self.callee {
            Callee::TransientParked { def, ext_refs } => {
                let wake = event.init
                    || arg_fired
                    || ext_refs.iter().any(|id| {
                        event.variables.get(id).is_some_and(|tv| tv.tag().triggers())
                    });
                if wake {
                    let fv = def.clone();
                    let lb = fv
                        .downcast_ref::<LambdaDef<R, E>>()
                        .expect("parked def must be a lambda");
                    let scope = self.scope.clone();
                    self.bind(ctx, scope, self.flags, fv.clone(), lb, event, &mut set)
                        .expect("failed to re-bind parked lambda");
                    true
                } else {
                    bound
                }
            }
            _ => bound,
        };
        if std::env::var_os("GXDBG_CS").is_some() {
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
            Some(f) if !bound => {
                let res = f.update(ctx, &mut self.arg_refs, event);
                // Reconstitute the two-channel tag across the clean-
                // Value Apply boundary (see `Apply::out_tag`).
                res.map(|v| TagValue::tagged(v, f.out_tag()))
            }
            Some(f) => {
                let init = mem::replace(&mut event.init, true);
                let mut refs = Refs::default();
                f.refs(&mut refs);
                refs.with_external_refs(|id| {
                    if let Entry::Vacant(e) = event.variables.entry(id) {
                        if let Some(v) = ctx.rt.cached().get(&id) {
                            // FIRED: a fresh bind's first dispatch is an
                            // init view — everything it sees is new to it
                            e.insert(TagValue::fired(v.clone()));
                            set.push(id);
                        }
                    }
                });
                let res = f.update(ctx, &mut self.arg_refs, event);
                event.init = init;
                res.map(|v| TagValue::tagged(v, f.out_tag()))
            }
        };
        // Park a transient binding: the dispatch above was this call —
        // stash the instance's external refs (its capture wake-set,
        // minus the fnode's own target: a re-delivered identical def
        // must stay a quiet no-op, as it is for a retained instance),
        // take over their runtime registrations so capture events keep
        // flowing to this top, then delete the instance. `transient`
        // never survives an update — bind and park happen in the same
        // call.
        if matches!(&self.callee, Callee::DynamicBound { transient: true, .. }) {
            let Callee::DynamicBound { def, mut apply, .. } =
                mem::replace(&mut self.callee, Callee::DynamicUnbound)
            else {
                unreachable!()
            };
            let fnode_id = match self.fnode.view() {
                NodeView::Ref(r) => Some(r.id),
                _ => None,
            };
            let mut refs = Refs::default();
            apply.refs(&mut refs);
            let mut ext: LPooled<Vec<BindId>> = LPooled::take();
            refs.with_external_refs(|id| {
                if Some(id) != fnode_id {
                    ext.push(id);
                }
            });
            for id in ext.iter() {
                ctx.rt.ref_var(*id, self.top_id);
            }
            apply.delete(ctx);
            self.callee =
                Callee::TransientParked { def, ext_refs: ext.drain(..).collect() };
        }
        for id in set.drain(..) {
            event.variables.remove(&id);
        }
        res
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.release_parked(ctx);
        if let Some(mut f) = self.callee.take_apply() {
            f.delete(ctx)
        }
        self.fnode.delete(ctx);
        for arg in self.args.values_mut() {
            ctx.rt.cached_mut().remove(&arg.id);
            if let Some(ref mut n) = arg.node {
                n.delete(ctx);
            }
        }
        for n in &mut self.arg_refs {
            n.delete(ctx);
        }
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        if let Some(f) = self.callee.apply_mut() {
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
        // The published arg values (`update` inserts them into
        // `rt.cached` under this site's own per-instance arg ids) are
        // replay memory: the dispatch back-fills quiet args from there
        // and the tail-call interception collects its whole rebind set
        // from there, so a frame whose arg expression bottoms would
        // otherwise dispatch with the PREVIOUS frame's value. Removing
        // them is safe — the ids are minted by and read only through
        // this site; a capture-fed arg re-publishes when the caller
        // re-primes the frame's external refs. EXCEPTION: a closed
        // (refs-free) arg expression is frame-INVARIANT and can't
        // re-produce without an init view — its published value is the
        // value channel, kept for the same reason `Cached` keeps a
        // closed subtree's cache (kernel twin: constant immediates).
        if let Some(f) = self.callee.apply_mut() {
            f.reset_replay(ctx)
        }
        self.fnode.reset_replay(ctx);
        for arg in self.args.values_mut() {
            if !arg.is_invariant() {
                ctx.rt.cached_mut().remove(&arg.id);
            }
            if let Some(ref mut n) = arg.node {
                n.reset_replay(ctx);
            }
        }
        for n in &mut self.arg_refs {
            n.reset_replay(ctx);
        }
    }

    fn typ(&self) -> &Type {
        &self.rtype
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn typecheck0(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        wrap!(self.fnode, self.fnode.typecheck0(ctx))?;
        let ftype = match self.ftype.as_ref() {
            Some(ftype) => ftype, // already initialized
            None => {
                let ftype = deref_typ!("fn", ctx, self.fnode.typ(),
                    Some(Type::Fn(ftype)) => Ok(ftype.clone())
                )?;
                // A self-call inside the def-time body check unifies
                // against the def's OWN cells — monomorphic recursion
                // (see `ExecCtx::rec_defs`). Every other site freshens
                // for per-site monomorphization.
                let is_rec_self_call = !ctx.rec_defs.is_empty()
                    && ftype.lambda_ids.ids().iter().any(|id| ctx.rec_defs.contains(id));
                // A call to one of the enclosing def's fn-typed PARAMS
                // during its def gate — the param knot (see
                // `ExecCtx::def_gate_params`).
                let is_param_knot = !ctx.def_gate_params.is_empty()
                    && matches!(
                        self.fnode.view(),
                        NodeView::Ref(r) if ctx.def_gate_params.contains(&r.id)
                    );
                let ftype = if is_rec_self_call || is_param_knot {
                    // A shallow clone shares every TVar cell with the
                    // def's ftype — the knot.
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
                                // Will be filled with default at bind time; insert placeholder
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
                // Check we have enough positional args
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
                ftype
            }
        };
        // Typecheck positional args in order
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
                    farg.typ.contains(&ctx.env, n.typ())?;
                    wrap!(n, n.typecheck0(ctx))?;
                    wrap!(n, farg.typ.check_contains(&ctx.env, n.typ()))?;
                }
            }
        }
        // Typecheck vargs
        if let Some(typ) = &ftype.vargs {
            loop {
                let key = ArgKey::Positional(pos_idx);
                pos_idx += 1;
                match self.args.get_mut(&key) {
                    Some(arg) => {
                        if let Some(ref mut n) = arg.node {
                            typ.contains(&ctx.env, n.typ())?;
                            wrap!(n, n.typecheck0(ctx))?;
                            wrap!(n, typ.check_contains(&ctx.env, n.typ()))?;
                        }
                    }
                    None => break,
                }
            }
        }
        // Settle DERIVED result cells (design/tvar_constraints.md phase
        // B): a constrained cell reachable from the rtype/throws but not
        // from any arg is produced by the callee's body — narrowing it
        // from the outside can't be checked against anything, so it
        // settles to its constraint's witness HERE, before an annotation
        // could narrow it unsoundly. This is the sound remnant of the
        // old eager post-hoc constraint loop. Arg-reachable cells stay
        // open: annotations may narrow them and the args themselves
        // enforce the narrowing (observations #3/#4).
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
        if let Some(t) = ftype.throws.with_deref(|t| t.cloned()) {
            match ctx.env.lookup_catch(&self.scope.dynamic) {
                Ok(id) => {
                    if let Some(bind) = ctx.env.by_id.get(&id)
                        && let Type::TVar(tv) = &bind.typ
                    {
                        let tv = tv.read();
                        let mut cell = tv.typ.write();
                        cell.typ = match &cell.typ {
                            None => Some(t),
                            Some(inner) => Some(inner.union(&ctx.env, &t)?),
                        };
                    }
                }
                Err(_) if t == Type::Bottom => (), // it doesn't throw any errors
                Err(_) => {
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

    /// Second typecheck pass. After recursing into the call's own
    /// subtrees, finalize call-site-dependent type info: by now every
    /// `lambda_ids` closure is complete, so we read the resolved fn type
    /// and drive `Apply::typecheck1` for every lambda that can be
    /// dispatched here — the callee, plus any callback passed as a
    /// fn-typed argument (each against that arg's resolved fn type). This
    /// is the former deferred check, now run with `&mut self` in a real
    /// second tree pass.
    fn typecheck1(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
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
        // Terminal settle for still-unbound constrained cells: bind each
        // to its conjunction's witness. Deferred from typecheck0 (where
        // the old eager version WAS the wide-binder of observations
        // #3/#4) so annotations and settled inference get the whole
        // typecheck0 phase to narrow the cells first. Walks the LIVE
        // ftype structure, never the stored constraints list — a list
        // tvar orphans when unification re-points its arg's cell.
        //
        // Cells reachable from an OMITTED defaulted labeled arg are
        // exempt: that arg's type belongs to its default EXPRESSION,
        // which compiles at static resolution (`setup_bind`, driven
        // from `try_static_resolve` below) and binds the cell through
        // the apply's own arg unification — settling first would
        // foreclose it (`rand::rand(#clock:1)` must get `'a := f64`
        // from the `0.0`/`1.0` defaults, not `[Int, Float]` wide). A
        // dynamically-dispatched site leaves them unbound: fusion
        // refuses (de-fuse) and the node-walk is type-tolerant.
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
            let defaulted: LPooled<AHashSet<usize>> =
                dtv.drain().map(|(_, tv)| tv.cell_addr()).collect();
            let mut tvs: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
            ftype.collect_tvars(&mut tvs);
            for (_, tv) in tvs.drain() {
                if !defaulted.contains(&tv.cell_addr()) {
                    // Terminal: an unconstrained cell still open after
                    // the whole tc0 phase defaults to ⊥ — see
                    // `TVar::settle_or_bottom`.
                    wrap!(self, tv.settle_or_bottom(&ctx.env))?;
                }
            }
            // The call's OWN result cell settles terminally too: for a
            // callee whose declared rtype is the LITERAL ⊥ (`never()`)
            // the cell never appears in the ftype walk above — the ⊥
            // unifies against it WITHOUT binding (the open-cell rule in
            // contains) — so if no writer refined it during tc0
            // (`let res = never(); res <- v` binds it to v's type
            // there), it is the type of a value that never arrives.
            // The defaulted-arg exemption applies here too: tc0 aliased
            // `self.rtype` with the instance rtype, so for a callee
            // like `rand(#start='a, #end='a) -> 'a` this IS the
            // defaulted cell, and settling it would foreclose the
            // default exprs binding it at static resolution.
            if let Type::TVar(tv) = &self.rtype
                && !defaulted.contains(&tv.cell_addr())
            {
                wrap!(self, tv.settle_or_bottom(&ctx.env))?;
            }
        }
        let resolved = ftype.resolve_tvars();
        let spec = self.spec.clone();
        // The callee's own identities, against the whole resolved type.
        for id in ftype.lambda_ids.ids().iter().copied() {
            finalize_lambda::<R, E>(ctx, id, &resolved, &spec)?;
        }
        // Callbacks: every lambda reachable through a fn-typed argument,
        // against that arg's resolved fn type. (Replaces the old
        // `hof_idmap`, which only saw bare `Type::Fn` args and merged
        // callback ids into the callee — polluting derived closures.)
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
        // Fold the former `static_resolve` pass in here: pre-bind the
        // callee + pre-materialize HOF callbacks now that the index is
        // complete and this site's callbacks are finalized.
        self.try_static_resolve(ctx)?;
        // Labeled-default type check — now sound: in this second pass the
        // closure is complete, so `len() == 1` truly means "exactly one
        // possible callee." Runs AFTER static resolution, whose
        // `setup_bind` replaced the typecheck0 Nop placeholders with the
        // per-site COMPILED default nodes — so the check reads the real
        // default's type, and its unification is what binds a
        // defaulted-arg cell the terminal settle deliberately left open
        // (`rand::rand(#clock:1)`: `'a := f64` from the `0.0`/`1.0`
        // defaults). A dynamically-dispatched site still holds Nops here
        // (typed as the arg's own tvar — the check is vacuous) and the
        // cell stays unbound.
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

    fn refs(&self, refs: &mut Refs) {
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
        NodeView::CallSite(self)
    }

    fn fuse(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<Option<Node<R, E>>> {
        // Reached only when `try_fuse` on this call site already failed
        // (the call did NOT inline). Two jobs:
        //
        // 1. DESCEND into the arg value nodes via `Update::fuse` — NOT
        //    `fusion::fuse`. `fusion::fuse` would `try_fuse` each arg,
        //    fusing bare constant args (string/int literals to async ops)
        //    into 0-input kernels — a marginal pessimization that drifts
        //    the FuseExpect metric on ~90 fixtures. Plain `node.fuse`
        //    only descends: a nested HOF in arg position (the common
        //    `list::to_array(list::map(..))` shape — list HOFs live in
        //    arg position) reaches its own `CallSite::fuse` and builds
        //    its callback template, while a constant arg's `fuse` is the
        //    no-op default. (Fusing genuinely compute-heavy args as
        //    regions is a separate, deliberate enhancement.)
        // 2. Give the callee its fusion-phase hook: for an HOF builtin,
        //    `Apply::fuse` builds its per-element callback template now.
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
        if let Some(f) = self.callee.apply() {
            // A resolved user-lambda callee is a cross-kernel call:
            // `try_fuse`'s analysis discovered the site and built (or
            // cache-hit) the callee kernel — emit a CLIF `call`
            // against it. An undiscovered site (the lambda didn't
            // build — unsupported arg/return shape, body that doesn't
            // lower) de-fuses and the subtree node-walks.
            if matches!(f.view(), ApplyView::Lambda(_)) {
                if let Some(info) = cx.lambda_site(self.spec.id).cloned() {
                    return emit_lambda_call_node(cx, self, &info);
                }
                bail!(
                    "emit_clif: lambda call site `{}` not discovered — \
                     subtree node-walks",
                    self.spec
                );
            }
            // Builtin-owned emission hook ([`Apply::emit_clif`]).
            // `Some` is the call's result; `None` falls through to
            // the DynCall path below.
            if let Some(cv) = f.emit_clif(self, cx)? {
                return Ok(cv);
            }
        }
        // A VALUE-position self-call inside a recursive callee body
        // (tail-position self-calls were intercepted by
        // `emit_body_tail`): call the kernel's own FuncRef. The inner
        // site is #203-UNRESOLVED — `self.callee` is `DynamicUnbound` —
        // so this check lives OUTSIDE the resolved-Apply block. Matched
        // by the self BindId (names shadow, #206; ids don't); captures
        // forward from this kernel's own params (bound with their
        // BindIds).
        if let Some((sb, info)) = cx.self_call_info() {
            let is_self = matches!(
                self.fnode.view(),
                NodeView::Ref(r) if r.id == *sb
            );
            if is_self {
                let info = info.clone();
                return emit_lambda_call_node(cx, self, &info);
            }
        }
        // Builtin DynCall. `marshal_arg_indices[i]` is a position in
        // the source-order arg list `spec_apply.args` — which spans
        // both labeled and positional args. The Node-side lookup has
        // to mirror that: labeled args go through `arg_named`,
        // positional args through `arg_positional` indexed by running
        // positional count (not source position).
        let info = match cx.builtin_site(self.spec.id) {
            Some(info) => info.clone(),
            None => bail!(
                "emit_clif: builtin call site `{}` not discovered — doesn't fuse",
                self.spec
            ),
        };
        let spec_apply = match &self.spec.kind {
            ExprKind::Apply(a) => a,
            _ => bail!("CallSite spec must be ExprKind::Apply"),
        };
        let mut source_nodes: Vec<&Node<R, E>> =
            Vec::with_capacity(spec_apply.args.len());
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
            .marshal_arg_indices
            .iter()
            .map(|&call_idx| {
                source_nodes.get(call_idx).copied().ok_or_else(|| {
                    anyhow!("emit_clif: marshal arg index {call_idx} out of range")
                })
            })
            .collect::<Result<Vec<_>>>()?;
        emit_dyncall_node(cx, &info, &arg_nodes)
    }
}
