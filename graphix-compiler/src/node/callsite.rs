use super::{NOP, Nop, bind::Ref, compiler::compile};
use crate::{
    Apply, ApplyView, ApplyViewMut, BindId, BindMode, CFlag, Event, ExecCtx, LambdaId,
    LambdaInstanceId, Node, NodeView, PendingTailCall, PrintFlag, Refs, Rt, Scope, Tag,
    TagValue, Update, UserEvent, deref_typ,
    env::TraitMethodRef,
    expr::{ErrorContext, Expr, ExprId, ExprKind, ModPath},
    fusion::{
        self,
        emit::{BodyCx, CompiledExpr, emit_dyncall_node, emit_lambda_call_node},
    },
    node::lambda::LambdaDef,
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

/// The call's argument nodes, keyed for signature lookups but
/// ITERATING IN SOURCE ORDER (IndexMap, insertion-ordered). Source
/// order is load-bearing at runtime: the compiler threads the args as
/// a sequential scope chain (a `let` in one argument is in scope for
/// the arguments to its right — a forward reference is a compile
/// error), so `update` must evaluate them left to right for the
/// bind's same-cycle delivery to reach its sibling readers. The old
/// AHashMap iterated in per-process seeded hash order, making that
/// delivery a coin flip (`skip(#n: let x = [...], array::iter(x))`
/// starved in ~half of processes — the aug06 settle-flap witness 2).
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

/// What a [`CallSite`] knows about its callee. Folds the former
/// (`function: Option<(Value, Box<dyn Apply>)>`, `statically_resolved`,
/// `first_static_update`) trio into one enum so the previously
/// representable invalid state (`statically_resolved && function.is_none()`)
/// cannot occur.
#[derive(Debug)]
pub(crate) enum Callee<R: Rt, E: UserEvent> {
    /// No callee bound yet — the just-compiled state. `fnode` is
    /// re-evaluated every cycle; the first cycle it yields a `LambdaDef`
    /// Value, `update()` transitions to `DynamicBound` via `bind()`.
    DynamicUnbound,
    /// Bound to a callee that may still change cycle-to-cycle. `def` is the
    /// `LambdaDef`-wrapped Value, kept for the per-cycle IDENTITY check
    /// against `fnode.update()`; when it differs we re-`bind()`. A
    /// recursive unfold's instances are RETAINED like any other binding
    /// (Eric's structural ruling 2026-08-13 — the delete-park/snapshot/
    /// prime machinery is gone; recursion holds its call tree of
    /// instances, and memory is the user's).
    DynamicBound { def: Value, apply: Box<dyn Apply<R, E>> },
    /// Pre-bound at compile time by [`CallSite::try_static_resolve`]:
    /// `fnode` provably resolves to one `LambdaDef`, so the per-cycle
    /// identity check + lazy bind is skipped (`fnode.update()` still runs
    /// for side effects, value discarded). No held def `Value` — the
    /// `ExecCtx`'s `lambda_defs` map owns every def for the ctx's
    /// lifetime; `first_update` primes the body's external refs
    /// exactly once.
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
    /// A dynamic def in flight — the callers replace the
    /// binding wholesale (a fresh `bind`, or `delete`).
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
    pub(super) spec: TArc<Expr>,
    pub(super) ftype: Option<FnType>,
    pub(super) rtype: Type,
    pub(crate) fnode: Node<R, E>,
    pub(crate) args: ArgMap<R, E>,
    pub(super) arg_refs: Vec<Node<R, E>>,
    /// The callee — static/dynamic-bound/unbound. See [`Callee`]. Replaces
    /// the former `function` + `statically_resolved` + `first_static_update`
    /// trio (the `Static` tag carries `first_update`; the old invalid
    /// `statically_resolved && function == None` state is unrepresentable).
    pub(crate) callee: Callee<R, E>,
    /// The callee is a BUILTIN: tainted arg productions are gated to
    /// silence before delivery — taint == bottom == no input to a
    /// builtin (Eric's rulings 2026-07-19/20). Builtin authors never
    /// see the taint channel: a bottomed arg is ABSENCE, the builtin's
    /// cached slot keeps its previous state, and eval decides what a
    /// missing arg means. The kernel twins agree by construction — a
    /// fused arg region's tainted result forces to None at the output
    /// boundary, and a fused DynCall delivers taint-masked slots as
    /// absence (dyncall-partial-args-jul2026). Lambda callees keep
    /// the poisoned delivery (formals poison). Set at every
    /// callee-binding site.
    pub(super) gate_tainted_args: bool,
    pub(crate) static_target: Option<StaticCallTarget>,
    /// A trait call over a UNION self type lowers to the select the
    /// programmer would otherwise write — one arm per member, each a
    /// static call to that member's implementation
    /// (`design/traits.md` §3). Once set, this node is that select: every
    /// `Update` method delegates to it and fusion sees a select, not a
    /// call.
    pub(crate) lowered: Option<Node<R, E>>,
    pub(crate) recursive_edge: AtomicBool,
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
    pub(super) resident: TagValue,
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

    /// The resolved callee as a raw `&dyn Apply`.
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
            rtype: Type::empty_tvar(),
            fnode,
            args,
            arg_refs: Vec::new(),
            callee: Callee::DynamicUnbound,
            gate_tainted_args: false,
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
        // we already warned about this
        flags.remove(CFlag::WarnUnhandled);
        self.clear_prepared_bind(ctx);
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

    /// Re-read the bound instance's resolved ftype and store it on
    /// both static channels (`static_target` + `Callee::Static`);
    /// returns it for callers that need the value. `None` = no bound
    /// apply (nothing refreshed).
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

    /// The resolution half of `typecheck1`, bracketed by the caller's
    /// cell PROTECTION: static resolution (whose re-drives typecheck
    /// interior call sites), per-lambda finalization, then the
    /// labeled-default check. Split out so protection unwinds on every
    /// error path — the ctx outlives a failed compile in check/LSP
    /// runtimes, and a leaked protected cell would poison later
    /// settles.
    fn typecheck1_resolve(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        ftype: &FnType,
    ) -> Result<()> {
        self.try_static_resolve(ctx)?;
        self.refresh_static_ftype();
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
        // Labeled-default type check — now sound: in this second pass the
        // closure is complete, so `len() == 1` truly means "exactly one
        // possible callee." Runs AFTER static resolution, whose
        // `prepare_bind` replaced the typecheck0 Nop placeholders with the
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

    fn setup_static_bind(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        scope: &Scope,
        flags: BitFlags<CFlag>,
        f: &LambdaDef<R, E>,
    ) -> Result<(Box<dyn Apply<R, E>>, FnType)> {
        self.prepare_bind(ctx, scope, flags, f, |_, _| {})?;
        if self.ftype.is_none() {
            bail!("statically resolving an untyped call site: {}", self.spec)
        }
        let site_ftype = self.ftype.as_ref().unwrap().resolve_tvars();
        // Arg count/kinds are resolution-independent — compare the raw
        // site ftype against the definition directly.
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
        // RETURN write-back: unify the instance's settled rtype into
        // the site's LIVE rtype cell. `site_ftype` above is a
        // `resolve_tvars` DEEP CLONE, so the instance re-drive binds
        // its inferred return into snapshot cells only — a site rtype
        // cell that is UNBOUND here (a REC def's gate defers body
        // inference past tc0, so no def-gate settle preceded the
        // site's freshen) would stay orphaned, and a later tc1
        // constraint could bind it to anything unchecked (the fn
        // value that read as Array<i64> —
        // fuzz/pending-triage/fn_value_hof_compare.gx). `contains`
        // binds an unbound site cell to the instance's truth; a bound
        // cell gets the conflict check (the t7 "i64 does not contain
        // fn" shape). Non-rec defs settled at the def gate, so this
        // is a consistency no-op for them; ⊥ (never) unifies without
        // binding per the open-cell rule.
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
        // Build arg_refs + InitFn + typecheck. The closure primes
        // each freshly-compiled default's external refs into
        // `event.variables` from `ctx.cached`, so the bound function
        // sees outer-binding values on its first update inside this
        // same cycle.
        let apply = self.setup_dynamic_bind(ctx, &scope, flags, f, |ctx, refs| {
            refs.with_external_refs(|id| {
                if let Some(v) = ctx.rt.store_value(&id) {
                    if let Entry::Vacant(e) = event.variables.entry(id) {
                        // FIRED: first-dispatch init semantics (a fresh
                        // bind sees everything as new)
                        e.insert(TagValue::fired(v));
                        set.push(id);
                    }
                }
            });
        })?;
        drop(setup_span);
        // Defensive: if the def being bound lost its `lambda_defs`
        // entry (its defining `Lambda` node was deleted — an escaped
        // value from a torn-down subtree, e.g. a dyn-module reload),
        // restore the entry for the duration of this bind's
        // elaboration so the by-id consumers below (`finalize_lambda`
        // via the body typecheck1, `analyze_bound_callee`'s effect
        // resolution) see the def exactly as a compile-time
        // elaboration would. Removed again before returning: entry
        // lifetime stays tied to the defining node (the jul22b
        // `lambda_defs` retention fix), and a live def's entry hits
        // the `contains_key` and is never touched.
        let restored_def = if ctx.lambda_defs.contains_key(&f.id) {
            false
        } else {
            ctx.lambda_defs.insert(f.id, fv.clone());
            true
        };
        self.gate_tainted_args = matches!(apply.view(), ApplyView::BuiltIn);
        self.callee = Callee::DynamicBound { def: fv, apply };
        // The publish loop ran before this bind resolved the callee —
        // retract any poisoned deliveries the gate would have silenced.
        if self.gate_tainted_args {
            for arg in self.args.values() {
                if event.variables.get(&arg.id).is_some_and(|tv| tv.is_tainted()) {
                    event.variables.remove(&arg.id);
                }
            }
        }
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
        let identity = self.fn_arg_identity(ctx);
        if let Some(apply) = self.callee.apply_mut()
            && matches!(apply.view(), ApplyView::Lambda(_))
        {
            let instance = match apply.view() {
                ApplyView::Lambda(g) => g.instance_id(),
                ApplyView::BuiltIn => unreachable!(),
            };
            let instance_ftype = apply.typ();
            // Same identity already resolving = a recursive lazy bind;
            // its body stays lazy (see `resolve_static`'s knot).
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
        // Ensure all arg values are available for the init cycle.
        // Defaults need to be updated for the first time (with init=true
        // since Constant only fires on init); existing args may not have
        // changed this cycle but their cached values must be visible to
        // the newly bound function body.
        let prev_init = mem::replace(&mut event.init, true);
        for arg in self.args.values_mut() {
            if arg.is_default {
                if let Some(ref mut node) = arg.node {
                    let tv = node.update(ctx, event);
                    if tv.tag().triggers() && !tv.tag().is_bottom() {
                        let v = tv.value_cloned();
                        // R3: frames never write the store.
                        if ctx.frame_depth == 0 {
                            ctx.rt.store_insert(arg.id, TagValue::fired(v.clone()));
                        }
                        event.variables.insert(arg.id, TagValue::fired(v));
                        set.push(arg.id);
                    }
                }
            }
            // non-default args need no backfill: the fresh body's
            // formal refs read the store under the init view (R2)
        }
        event.init = prev_init;
        if restored_def {
            ctx.lambda_defs.remove(&f.id);
        }
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
    /// The runtime's first update through this CallSite handles arg
    /// init-priming via the `first_static_update` flag set here.
    pub fn resolve_static(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        def: &LambdaDef<R, E>,
    ) -> Result<()> {
        if matches!(self.callee, Callee::Static { .. }) || self.static_target.is_some() {
            // Idempotent.
            return Ok(());
        }
        // The recursion knot, keyed on INSTANTIATION identity: a site
        // reached while an instantiation of `def` with the same fn-arg
        // identity is resolving is a self-call of that instance and
        // shares it (bounded regress); a different identity is a
        // distinct instantiation even mid-resolution — a callback premats
        // while its HOF site resolves, so a use of the same HOF nested
        // under its own callback arrives here with the def active and
        // is a nested loop, not a cycle (`crate::FnArgIdentity`).
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
        self.gate_tainted_args = matches!(apply.view(), ApplyView::BuiltIn);
        self.callee = Callee::Static {
            apply,
            resolved_ftype: instance_ftype.clone(),
            first_update: true,
        };
        // Per-callsite elaboration: register the fn-typed args under the
        // instance's param BindIds BEFORE typechecking the instance body,
        // so both direct calls to and captures of a fn parameter resolve
        // in this one downward pass (see `register_fn_params`). Held
        // through the whole body typecheck, removed after.
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
        Self::unregister_fn_params(ctx, param_binds, trait_param_binds);
        if let Some(instance) = instance {
            ctx.pop_resolving(def.id, instance);
        }
        res
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
                if crate::dbgenv::gxdbg_resolve() {
                    eprintln!(
                        "RESOLVE {} id={:?} unstable={} b2l={} cached={}",
                        self.spec,
                        r.id,
                        ctx.unstable_bindings.contains(&r.id),
                        ctx.bind_to_lambda.contains_key(&r.id),
                        ctx.rt.store_value(&r.id).is_some(),
                    );
                }
                if ctx.unstable_bindings.contains(&r.id) {
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
                // a TRAIT METHOD: the dispatcher binding names no
                // lambda; the self argument's type picks one
                if let NodeView::Ref(r) = self.fnode.view()
                    && let Some(tm) = ctx.env.trait_methods.get(&r.id).copied()
                {
                    // Both paths funnel through `resolve_static`, which
                    // registers the callee's fn-params before typechecking
                    // its body (per-callsite elaboration).
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

    /// Register this call site's statically-known fn-typed args under the
    /// resolved INSTANCE's param BindIds — the per-callsite elaboration
    /// channel. [`Self::resolve_static`] calls this right BEFORE
    /// typechecking the instance body, so the registration is in scope
    /// for the WHOLE downward body typecheck: the instance's own call
    /// sites resolve calls to the lambda parameter (`f(v)`) like a lambda
    /// binding, and — crucially — a nested lambda that CLOSES OVER the
    /// parameter (a trait-default wrapper body's `filter_map(c, |x|
    /// f(x))`) resolves in the same pass, because its instance is created
    /// mid-typecheck while the registration is still live. Returns the
    /// registered BindIds for [`Self::unregister_fn_params`] to remove
    /// after the body typecheck; records the persistent
    /// forward-resolution snapshot the kernel cache key
    /// (`FnResolutions`) reads once the b2l entries are gone. Per-instance
    /// BindIds are fresh per callsite, so there is no cross-site
    /// contamination; a recursive callee registers once (its self-calls
    /// reuse the resolving instance without re-registering), so no
    /// separate back-edge guard is needed. A trait-dispatched HOF needs
    /// this exactly like a direct call, or a collection-bodied impl's
    /// prototype can't resolve its callback and emission refuses (P2b).
    /// This site's instantiation identity ([`crate::FnArgIdentity`]).
    /// Resolves each argument the way [`Self::register_fn_params`]
    /// does: a lambda literal is its own source, a `Ref` goes through
    /// `bind_to_lambda` (a let-bound lambda, or a fn param an enclosing
    /// premat registered), a `<-` target is dynamic.
    fn fn_arg_identity(&self, ctx: &ExecCtx<R, E>) -> crate::FnArgIdentity {
        self.args
            .values()
            .map(|arg| {
                let node = arg.node.as_ref()?;
                match node.view() {
                    NodeView::Lambda(l) => Some(l.source_id()),
                    NodeView::Ref(r) if !ctx.unstable_bindings.contains(&r.id) => ctx
                        .bind_to_lambda
                        .get(&r.id)
                        .and_then(|fv| fv.downcast_ref::<LambdaDef<R, E>>())
                        .map(|def| def.source),
                    _ => None,
                }
            })
            .collect()
    }

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
                    if ctx.unstable_bindings.contains(&r.id) {
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

    /// Undo [`Self::register_fn_params`] after the instance body
    /// typecheck (the `fn_forward_resolutions` snapshot is deliberately
    /// permanent — the fingerprint reads it at fusion time).
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

    /// Resolve a call through a trait method's dispatcher to an
    /// implementation by the self argument's type (`design/traits.md`
    /// §2): the call site is re-pointed at the implementation's (or the
    /// default's) binding and pre-bound statically when its lambda is
    /// known. An open self type inside a definition gate is the
    /// polymorphic case — each instance resolves for itself; open
    /// anywhere else is the error the design demands. A union self
    /// type dispatches through a generated select (§3).
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
        // NORMALIZE, not just resolve: dispatch reasons per union
        // member, so the self type has to be in union normal form. A
        // `never()` select arm types as a cell that resolves to `⊥`,
        // and `resolve_tvars` alone leaves it standing as a member —
        // `[⊥, Pipe]` then demanded an impl of Write for `⊥`.
        let mut self_t = match ftype.args.get(m.self_index) {
            Some(a) => {
                // Trait dispatch is STATIC — it must decide on the
                // self type HERE, so it forces the otherwise-DEFERRED
                // terminal settle of the self type's cells
                // (`pending_settles` drains at the statement boundary;
                // dispatch is the one mid-typecheck1 consumer of
                // settled facts): a never() arm's open unconstrained
                // cell settles ⊥ and the normalize below drops it from
                // the union, exactly as the statement-boundary settle
                // would have. A writer that would still have bound the
                // cell meets the settled value loudly at its own
                // check, never a silently wrong dispatch.
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
        // a constructor trait selects by the receiver's outermost form:
        // the constructor, never the element (a reference by name)
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

    /// Take this call's argument NODES in the spec's argument order,
    /// each under a synthesized name (`#a<i>`, or `self_name` for the
    /// positional self argument at `self_pos`), for a lowering over
    /// them. Returns `(name, node)` pairs and the call-args list
    /// (`(label, name)`) a synthesized call spells them with.
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
    /// `Eq::eq(a, b)` is `a == b`, `Display::fmt(x)` is `"[x]"`, and
    /// `Ord::cmp(a, b)` tests `<` and `>` — so the call works on every
    /// type, and an implementation is reached exactly where the
    /// operator would reach it (`design/traits.md` §8).
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
    /// — the arguments bound once (the call's own argument NODES,
    /// moved), then the select the programmer would have written,
    /// with a static call in every arm. The select compiles under
    /// this site's scope; the implementation bindings are named by id
    /// (`#bind::N`, a form no source can spell).
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
        let mut set: LPooled<Vec<BindId>> = LPooled::take();
        // A FIRED (or tainted) arg production this cycle — the genuine
        // -call signal (a stale production is a value-channel refresh,
        // not an event).
        let mut arg_fired = false;
        // Update all arg nodes every cycle, publishing TRIGGERING
        // productions via bind IDs. A stale production is the value
        // channel — the formal's store read already serves it (a
        // standing entry reads Stale), so nothing is published. A
        // fresh bottom is a genuine delivery: the formal is poisoned
        // (the placeholder never enters the store) and the callee's
        // seam decides — a builtin's wrapper bottoms the invocation
        // (the P5a Q1 arms), a lambda keeps the poisoned formal. The
        // old `gate_tainted_args` builtin silencing is gone with it.
        // The old FRAME-ONLY stale backfill is gone too: the overlay
        // stack's read-through IS that delivery.
        //
        // A SELF-TAIL-CALL site additionally keeps every arg's whole
        // production for the stash below: stale jump plumbing is
        // never published (triggering-only), so a map read cannot see
        // it — the stash must consume the productions directly.
        let stash_prods = self.is_self_tail_call.load(Ordering::Relaxed);
        // Capture the productions whenever a BIND could happen this
        // cycle (first-ever dispatch or any dynamic callee): a bind
        // mints/rewires arg ids, and QUIET (stale) arg productions —
        // never published by the triggering-only loop — must be
        // seeded onto them or the fresh callee reads phantom formals
        // (transient-prime-park/01: the rebound chain's interior
        // fresh callsites starved one level down). Steady-state
        // static callsites skip the capture.
        let may_bind = match &self.callee {
            Callee::Static { first_update, .. } => *first_update,
            _ => true,
        };
        let mut prods: SmallVec<[(BindId, TagValue); 4]> = SmallVec::new();
        for arg in self.args.values_mut() {
            if let Some(ref mut node) = arg.node {
                let tv = node.update(ctx, event);
                let tag = tv.tag();
                if stash_prods || may_bind {
                    prods.push((arg.id, tv.clone()));
                }
                if tag.triggers() {
                    arg_fired = true;
                    if tag.is_bottom() {
                        // A fresh bottom PERSISTS in the store, exactly
                        // like the clean value below and like the
                        // GXLambda formal-publish twin (ruled delta 7 /
                        // STRICT). Poisoning only the cycle-scoped
                        // overlay left the store holding the last CLEAN
                        // value, so the next cycle's standing read
                        // resurrected it: `str::len(v0)` served
                        // "graphix" to a builtin whose argument had
                        // bottomed a cycle earlier, and the call fired
                        // a stale 7 where the kernel stayed bottom
                        // (aug15b hz0 reactive 000000). Same hole as
                        // formal-bottom-persists-aug2026, in the
                        // sibling path.
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
                        // R3: frames never write the store (see the
                        // GXLambda entry-publish twin) — an arg
                        // published inside an enclosing loop's frame
                        // is loop plumbing.
                        if ctx.frame_depth == 0 {
                            ctx.rt.store_insert(arg.id, TagValue::fired(v.clone()));
                        }
                        event.variables.insert(arg.id, TagValue::tagged(v, tag));
                    }
                    set.push(arg.id);
                } else if ctx.frame_depth == 0 && ctx.wake_recompute() && !tag.is_bottom()
                {
                    // WAKE CATCH-UP (design/wake_catchup.md): this
                    // site's first update after sleep recomputed the
                    // arg — its STALE production may carry a value the
                    // arg id's standing store entry drifted behind
                    // while the arm slept. Refresh the standing entry
                    // so the callee's formal read (arg_refs → store)
                    // serves the present value; honest STALE, a
                    // value-channel refresh, never a fire.
                    ctx.rt.store_insert_standing(
                        arg.id,
                        TagValue::stale(tv.value_cloned()),
                    );
                } else if ctx.frame_depth > 0 && !tag.is_bottom() {
                    // Inside a frame the store holds the STALE pre-frame
                    // value (R3: frames never write the store), so a
                    // rebound loop formal read as a STALE DynCall arg
                    // would reach the callee as the pre-frame value: the
                    // callee reads the distinct ARG id, which the formal
                    // overlay read-through (the store-read premise above)
                    // does NOT cover. Publish the current frame-overlay
                    // value onto the arg id through the cycle-scoped
                    // overlay (withdrawn with `set`), honest STALE tag —
                    // a value-channel refresh, not a fire, so the callee
                    // still fires off its own fresh operands. This is the
                    // frame-only stale backfill, re-narrowed to the arg
                    // channel: without it a tail loop's `f(acc, i)` read a
                    // stale entry-formal `i` on every re-triggered
                    // dispatch (aug28b fold_go, the aug13i shape).
                    let tv = tv.clone();
                    event.variables.insert(arg.id, tv);
                    set.push(arg.id);
                }
            }
        }
        // Tail-call interception. When `analysis::analyze` flagged this
        // call as a tail-position self-call inside a synchronous tail-recursive
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
                    // a quiet tail self-call contributes nothing this
                    // cycle: ride without dispatching (dispatching
                    // would consume the callee's first-dispatch
                    // init-forcing and re-create the jul04 wedge)
                    return self.resident.ride();
                }
                // A `None` arg (bottomed this jump, never cached) makes
                // the formal RIDE its previous value — the kernel's
                // taint-gated rebind (`emit_tail_rebind_jump`). The old
                // all-or-nothing gate fell through to genuine recursion
                // here, which agreed with the kernel below the depth
                // limit and silently depth-aborted above it.
                //
                // Each present arg carries its HONEST production tag
                // (Eric's ruling 2026-07-18, tail_jump_fired_plumbing):
                // the production published into `event.variables` just
                // above — fired if the arg expression genuinely fired,
                // stale for a frame backfill / value-channel refresh. A
                // TAINTED production is the bottomed case → ride. A
                // quiet arg with only a cross-cycle cached value rides
                // the STALE channel — the kernel's marshaled quiet
                // slot. Stashing bare cached Values here forced the
                // rebind to mint FIRED unconditionally, manufacturing
                // freshness for results that depend on nothing that
                // fired.
                // Stash each arg's PRODUCTION directly (honest value
                // + tag): a stale production is the framed jump
                // plumbing the map never carries (triggering-only
                // publish), a bottomed one rides (None — the kernel's
                // taint-gated rebind keeps the old loop slot). The
                // read_var fallback serves node-less args (defaults).
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
                // the stash is consumed by the enclosing loop; this
                // site's own production rides
                return self.resident.ride();
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
        let fv_new = {
            let tv = self.fnode.update(ctx, event);
            if tv.tag().is_bottom() { None } else { Some(tv.value_cloned()) }
        };
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
        // A bind happened this cycle: seed QUIET (stale, non-bottom)
        // arg productions onto the arg ids on the VALUE channel — the
        // triggering-only publish never carries them, and a fresh
        // callee (fresh ids) would read phantom formals otherwise.
        // Cycle-scoped overlay entry for the dispatch below (any
        // frame depth; withdrawn with `set`), standing store entry at
        // depth 0 for later quiet cycles (R3: frames never write the
        // store).
        if bound {
            for (id, tv) in prods.iter() {
                let tag = tv.tag();
                if !tag.triggers() && !tag.is_bottom() {
                    if ctx.frame_depth == 0 {
                        // The STORE standing entry serves both dispatch
                        // views (Stale ordinarily, Fired under the
                        // real-init arm — R2). An overlay entry here
                        // would SHADOW that init upgrade (overlay reads
                        // precede the store and carry STALE verbatim).
                        ctx.rt.store_insert_standing(
                            *id,
                            TagValue::stale(tv.value_cloned()),
                        );
                    } else {
                        // In frames the store is off-limits (R3): the
                        // cycle-scoped overlay entry is the channel —
                        // and it must carry the view R2 gives the store
                        // read. A `bound` dispatch runs under the REAL
                        // init view (below), where a standing read
                        // upgrades to Fired; the overlay has no
                        // read-time upgrade, so seed FIRED directly.
                        // The entry is cycle-scoped (withdrawn with
                        // `set`), exactly as wide as the init-view
                        // dispatch. Seeding STALE verbatim left a woken
                        // arm's tick-gated builtin (is_err) riding its
                        // phantom forever — the arm had no value to
                        // emit (aug14f iter_rec_guard).
                        event.variables.insert(*id, TagValue::fired(tv.value_cloned()));
                        set.push(*id);
                    }
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
            // The tag rides in the borrowed production; own it here —
            // the park/filter pipeline below reworks `self.callee`, so
            // the callee's borrow can't be forwarded through.
            Some(f) if !bound => Some(f.update(ctx, &mut self.arg_refs, event).clone()),
            Some(f) => {
                // A fresh bind (or parked rebind) on a REAL init view:
                // seed the parked twin's selections if any, then
                // dispatch under the init view — the callee's refs
                // read standing store entries as Fired (R2), which IS
                // the old explicit FIRED backfill.
                let init = mem::replace(&mut event.init, true);
                let res = f.update(ctx, &mut self.arg_refs, event).clone();
                event.init = init;
                Some(res)
            }
        };
        // Under dense delivery stale and bottom productions are
        // first-class currency at every depth — the old depth-0
        // fired-only escape filter (replay_frames Ruling A.2, the
        // jul10h-000007 protection) is repealed: tag-aware consumers
        // (P4's seam_tick families — array::group among them) gate on
        // firedness themselves.
        if crate::dbgenv::gxdbg_cs() {
            // Result-tag companion to the pre-dispatch CS line above —
            // localized the tail-loop tag derivation and the fd0 stale
            // escape (jul10h 000007).
            eprintln!(
                "CS-RES spec={} res={:?} fd={}",
                self.spec,
                res.as_ref().map(|tv| tv.tag()),
                ctx.frame_depth
            );
        }
        // RETENTION IS UNCONDITIONAL (Eric's structural ruling
        // 2026-08-13): a transient instance never parks — it stays
        // bound, its own live refs are the wake set, and the ordinary
        // already-bound dispatch path serves every later cycle. That
        // is retained-twin parity BY CONSTRUCTION: the delete-park /
        // snapshot / rebuild machinery kept reproducing retained
        // state channel by channel (selections, pattern binds, guard
        // helds, formals, init views) and each hole was a soak class.
        // Memory is the user's: a fib(28)-shaped tree materializes
        // its full call tree of retained instances, exactly as the
        // hand-inlined equivalent would ("you can't fix stupid").
        // The defended semantics: whether a callee is recursive is
        // not observable in firing — a pure function re-applied to
        // unchanged inputs is not an event, and recursion fires like
        // the hand-inlined chain of distinct functions.
        for id in set.drain(..) {
            event.variables.remove(&id);
        }
        match res {
            Some(tv) => self.resident.set(tv),
            // no callee bound (unresolvable/parked-quiet): the site
            // rides its last result on the value channel
            None => self.resident.ride(),
        }
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for CallSite<R, E> {
    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> &TagValue {
        // two arms, each its own borrow: a conditional early return of
        // the lowered node's production would hold `self` for the
        // function's whole lifetime
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
        if let Some(n) = &mut self.lowered {
            return n.sleep(ctx);
        }
        // A recursive edge whose ARM is being actively deselected (the
        // recursion reached a shallower depth this cycle) is a SHRUNK
        // slot: delete the deeper activation rather than retain it, so
        // re-reaching this depth binds a FRESH one (the collection-slot
        // rule, `ctx.shrink_unwind`). The delete cascades down the
        // retained chain via `GXLambda::delete`. A whole-recursion PAUSE
        // clears the flag (see `GXLambda::sleep`), so this only fires on
        // a genuine shrink.
        if ctx.shrink_unwind && self.is_recursive_edge() {
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
        // The published arg values (`update` inserts them into
        // the store under this site's own per-instance arg ids) are
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
            // arg STORE entries survive (the dense value channel — see
            // GXLambda::reset_replay)
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
                // A self-call inside the def-time body check unifies
                // against the def's OWN cells — monomorphic recursion
                // (see `ExecCtx::rec_defs`). Every other site freshens
                // for per-site monomorphization.
                let is_rec_self_call = !ctx.rec_defs.is_empty()
                    && ftype.lambda_ids.ids().iter().any(|id| ctx.rec_defs.contains(id));
                let identity = self.fn_arg_identity(ctx);
                let active_ftype = ftype
                    .lambda_ids
                    .own()
                    .and_then(|id| ctx.resolving(id, &identity))
                    .map(|active| active.ftype);
                // A call to one of the enclosing def's fn-typed PARAMS
                // during its def gate — the param knot (see
                // `ExecCtx::def_gate_params`).
                let is_param_knot = !ctx.def_gate_params.is_empty()
                    && matches!(
                        self.fnode.view(),
                        NodeView::Ref(r) if ctx.def_gate_params.contains(&r.id)
                    );
                let ftype = if let Some(active) = active_ftype {
                    active
                } else if is_rec_self_call || is_param_knot {
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
                // Excess positionals with no vargs to absorb them. The
                // total-count guard above can't catch this when the
                // callee has labeled params: defaults inflate its
                // budget, and an unmatched positional would otherwise
                // skip the arg-typecheck loop entirely (its own
                // compile errors never surface) and fail or be
                // silently dropped at bind time.
                if n_positional_provided > n_positional_required && ftype.vargs.is_none()
                {
                    bail!(
                        "too many positional arguments, expected {n_positional_required}, received {n_positional_provided}"
                    )
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
                    // a reference instantiates its (generalized)
                    // signature in its own typecheck0 — that must
                    // precede the pre-bind, or the pre-bind would
                    // unify against the definition's cells
                    if matches!(n.view(), NodeView::Ref(_)) {
                        wrap!(n, n.typecheck0(ctx))?;
                    }
                    Type::pre_unify_arg(&ctx.env, &farg.typ, n.typ())?;
                    wrap!(n, n.typecheck0(ctx))?;
                    wrap!(n, farg.typ.check_contains(&ctx.env, &n.typ()))?;
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
            match self.scope.dynamic.catch() {
                Some((id, _)) => {
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

    /// Second typecheck pass. After recursing into the call's own
    /// subtrees, finalize call-site-dependent type info: by now every
    /// `lambda_ids` closure is complete, so we read the resolved fn type
    /// and drive `Apply::typecheck1` for every lambda that can be
    /// dispatched here — the callee, plus any callback passed as a
    /// fn-typed argument (each against that arg's resolved fn type). This
    /// is the former deferred check, now run with `&mut self` in a real
    /// second tree pass.
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
        // A fresh settle FRAME for this site's re-drives (see
        // `ExecCtx::pending_settles`): statement boundaries inside the
        // re-driven bodies drain their own frame, and whatever remains
        // when the resolution returns — entries whose cells THIS
        // resolution owns, like a collection prototype's signature —
        // merges up and drains only after this site's writers have
        // run.
        ctx.pending_settles.push(Vec::new());
        let res = self.typecheck1_resolve(ctx, &ftype);
        let leftover = ctx.pending_settles.pop().expect("settle frame");
        ctx.pending_settles.last_mut().expect("root settle frame").extend(leftover);
        res?;
        // Terminal settle for still-unbound constrained cells: bind each
        // to its conjunction's witness. Deferred from typecheck0 (where
        // the old eager version WAS the wide-binder of observations
        // #3/#4) so annotations and settled inference get the whole
        // typecheck0 phase to narrow the cells first — and run LAST in
        // this pass, after the finalize loops (Eric's ruling
        // 2026-08-26): the jul22e discriminator ("open + unconstrained
        // at terminal settle → error/⊥") is sound only once every
        // writer has run, and the CALLBACK finalizations above are
        // writers — a generalized fn-valued argument's cells bind only
        // there (inline callbacks bind in tc0's arg loop). Settling
        // between static resolution and the finalize loops ⊥-settled
        // find_map's `'b` before the extracted callback's return could
        // bind it: "Option<_> does not contain [i64, null]" (typemorph
        // let-extract, return-side face). Walks the LIVE ftype
        // structure, never the stored constraints list — a list tvar
        // orphans when unification re-points its arg's cell.
        //
        // Cells reachable from an OMITTED defaulted labeled arg are
        // exempt: that arg's type belongs to its default EXPRESSION,
        // which compiles at static resolution (`setup_static_bind`,
        // driven from `try_static_resolve` above) and binds the cell
        // through the apply's own arg unification and the
        // labeled-default check above — a dynamically-dispatched site
        // leaves them unbound: fusion refuses (de-fuse) and the
        // node-walk is type-tolerant.
        // The terminal settle is DEFERRED to the statement boundary
        // (`compile_stmt` drains `ctx.pending_settles` once typecheck1
        // has completed for the whole statement): a settle is sound
        // only after every writer has run, and writers live at every
        // level above an interior site — the parent's finalize loops,
        // an enclosing collection node's prototype-return check
        // (find_map's `'b`, ⊥-settled mid-resolution, failed the
        // extracted-callback spelling with "Option<_> does not contain
        // [i64, null]" while the inline spelling compiled — typemorph
        // let-extract, return-side face). Each site still contributes
        // its own resolved signature, so instance cells get their
        // witnesses (fusion) and the μ-refusal channel fires at the
        // drain.
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
            // The call's OWN result cell joins the settle set: for a
            // callee whose declared rtype is the LITERAL ⊥ (`never()`)
            // the cell never appears in the ftype walk — the ⊥ unifies
            // against it WITHOUT binding (the open-cell rule in
            // contains) — so if no writer refined it during tc0
            // (`let res = never(); res <- v` binds it to v's type
            // there), it is the type of a value that never arrives.
            // The defaulted-arg exemption covers it too: tc0 aliased
            // `self.rtype` with the instance rtype, so for a callee
            // like `rand(#start='a, #end='a) -> 'a` this IS the
            // defaulted cell, and settling it would foreclose the
            // default exprs binding it at static resolution. Settle
            // ORDER is dependency-first — see
            // `FnType::settle_terminal` (the jul22e settle-order
            // flap).
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
        // 2. Give the callee its fusion-phase hook.
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
            // A resolved user-lambda callee is a cross-kernel call:
            // `try_fuse`'s analysis discovered the site and built (or
            // cache-hit) the callee kernel — emit a CLIF `call`
            // against it. An undiscovered site (the lambda didn't
            // build — unsupported arg/return shape, body that doesn't
            // lower) de-fuses and the subtree node-walks.
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
                return emit_lambda_call_node(cx, self, &info, true);
            }
        }
        if self.is_recursive_edge() {
            bail!("emit_clif: mutually recursive static call edge is not supported")
        }
        // Builtin DynCall. `marshal_arg_indices[i]` is a position in
        // the source-order arg list `spec_apply.args` — which spans
        // both labeled and positional args. The Node-side lookup has
        // to mirror that: labeled args go through `arg_named`,
        // positional args through `arg_positional` indexed by running
        // positional count (not source position).
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
            .marshal_arg_indices
            .iter()
            .map(|&call_idx| {
                source_nodes.get(call_idx).copied().ok_or_else(|| {
                    anyhow!("emit_clif: marshal arg index {call_idx} out of range")
                })
            })
            .collect::<Result<smallvec::SmallVec<[_; 8]>>>()?;
        emit_dyncall_node(cx, self.spec.id, &info, &arg_nodes)
    }
}
