use super::{bind::Ref, compiler::compile, Nop, NOP};
use crate::{
    deref_typ,
    expr::{ErrorContext, Expr, ExprId},
    node::lambda::LambdaDef,
    typ::{FnArgKind, FnType, Type},
    wrap, Apply, BindId, CFlag, Event, ExecCtx, LambdaId, Node, PrintFlag, Refs, Rt,
    Scope, Update, UserEvent,
};
use ahash::{AHashMap, AHashSet};
use anyhow::{anyhow, bail, Context, Result};
use arcstr::ArcStr;
use enumflags2::BitFlags;
use netidx::subscriber::Value;
use poolshark::local::LPooled;
use std::{collections::hash_map::Entry, mem};
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
        crate::expr::ExprKind::Ref { name } => name,
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
) -> Result<AHashMap<ArgKey, Arg<R, E>>> {
    let mut res = AHashMap::default();
    let mut pos = 0;
    for (name, expr) in args.iter() {
        let node = Some(compile(ctx, flags, expr.clone(), scope, top_id)?);
        match name {
            None => {
                res.insert(
                    ArgKey::Positional(pos),
                    Arg { id: BindId::new(), node, is_default: false },
                );
                pos += 1;
            }
            Some(k) => match res.entry(ArgKey::Named(k.clone())) {
                Entry::Occupied(_) => bail!("duplicate named argument {k}"),
                Entry::Vacant(e) => {
                    e.insert(Arg { id: BindId::new(), node, is_default: false });
                }
            },
        }
    }
    Ok(res)
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
    pub(crate) function: Option<(Value, Box<dyn Apply<R, E>>)>,
    pub(super) flags: BitFlags<CFlag>,
    pub(super) scope: Scope,
    pub(super) top_id: ExprId,
    /// Set by the post-typecheck `resolve_static_calls` pass when the
    /// function expression can be proven to always resolve to a single
    /// known `LambdaDef`. When `true`, `update()` skips the lazy
    /// `(fnode_value, function_value)`-equality + downcast +
    /// `bind()` arm — `self.function` is already bound and won't
    /// change. `fnode.update(ctx, event)` is still called every
    /// cycle (so the function expression's side effects fire) but
    /// its returned `Value` is discarded.
    pub(crate) statically_resolved: bool,
    /// True on the first runtime update of a statically-resolved
    /// CallSite. Mirrors the priming step the dynamic `bind=true` arm
    /// runs once when the function first binds: emulates `event.init =
    /// true` and primes `event.variables` from `ctx.cached` for every
    /// external Ref the function's body reads. Cleared after the first
    /// update.
    pub(super) first_static_update: bool,
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
            crate::expr::ExprKind::Apply(a) => &a.args,
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

    /// View the [`Apply`] this CallSite is currently bound to. Returns
    /// `None` if the CallSite hasn't bound yet (the typical
    /// just-compiled state — runtime `bind()` fires lazily on the
    /// first cycle the `fnode` produces a LambdaDef Value).
    /// `Some(view)` after either the runtime dynamic bind or the
    /// post-typecheck `static_resolve` pass has populated
    /// `self.function`.
    ///
    /// Used by fusion to descend through a resolved call site into a
    /// user lambda's body. See [`crate::ApplyView`] for the variants.
    pub fn resolved_apply(&self) -> Option<crate::ApplyView<'_, R, E>> {
        self.function.as_ref().map(|(_, a)| a.view())
    }

    /// Mutable counterpart to [`Self::resolved_apply`]. Fusion uses
    /// this when it needs to splice an inner sub-kernel into a Node
    /// reachable through the resolved Apply — e.g. a
    /// [`crate::ApplyViewMut::Lambda`]'s body Node.
    pub fn resolved_apply_mut(&mut self) -> Option<crate::ApplyViewMut<'_, R, E>> {
        self.function.as_mut().map(|(_, a)| a.view_mut())
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
        if self.function.is_some() {
            Some(&self.arg_refs)
        } else {
            None
        }
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
            function: None,
            flags,
            top_id,
            scope: scope.clone(),
            statically_resolved: false,
            first_static_update: false,
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
        fv: Value,
        f: &LambdaDef<R, E>,
        mut prime_default_refs: F,
    ) -> Result<()>
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
        if let Some((_, mut old_f)) = self.function.take() {
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
                        let default_node = match &f.argspec[i].labeled {
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
                        let typ = default_node.typ().clone();
                        let spec = TArc::new(default_node.spec().clone());
                        self.args.insert(
                            ArgKey::Named(name.clone()),
                            Arg { id, node: Some(default_node), is_default: true },
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
        let _ = rf.typecheck0(ctx, &mut self.arg_refs);
        self.function = Some((fv, rf));
        Ok(())
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
        // Build arg_refs + InitFn + typecheck. The closure primes
        // each freshly-compiled default's external refs into
        // `event.variables` from `ctx.cached`, so the bound function
        // sees outer-binding values on its first update inside this
        // same cycle.
        self.setup_bind(ctx, &scope, flags, fv, f, |ctx, refs| {
            refs.with_external_refs(|id| {
                if let Some(v) = ctx.cached.get(&id) {
                    if let Entry::Vacant(e) = event.variables.entry(id) {
                        e.insert(v.clone());
                        set.push(id);
                    }
                }
            });
        })?;
        // Ensure all arg values are available for the init cycle.
        // Defaults need to be updated for the first time (with init=true
        // since Constant only fires on init); existing args may not have
        // changed this cycle but their cached values must be visible to
        // the newly bound function body.
        let prev_init = mem::replace(&mut event.init, true);
        for arg in self.args.values_mut() {
            if arg.is_default {
                if let Some(ref mut node) = arg.node {
                    if let Some(v) = node.update(ctx, event) {
                        ctx.cached.insert(arg.id, v.clone());
                        event.variables.insert(arg.id, v);
                        set.push(arg.id);
                    }
                }
            } else if let Entry::Vacant(e) = event.variables.entry(arg.id) {
                if let Some(v) = ctx.cached.get(&arg.id) {
                    e.insert(v.clone());
                    set.push(arg.id);
                }
            }
        }
        event.init = prev_init;
        Ok(())
    }

    /// Pre-bind this CallSite to a statically known `LambdaDef` at
    /// compile time, replacing the lazy "bind on first call" path
    /// `bind()` runs from inside `update()`. The post-typecheck
    /// `resolve_static_calls` pass calls this once for every CallSite
    /// whose function expression can be proven to resolve to exactly
    /// one Lambda (i.e. a `Ref` to a non-`<-`-target binding whose
    /// value is a Lambda, or a direct lambda literal `(|x|…)(42)`).
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
        if self.statically_resolved {
            // Idempotent.
            return Ok(());
        }
        let scope = self.scope.clone();
        self.setup_bind(ctx, &scope, self.flags, fv, def, |_, _| {})?;
        self.statically_resolved = true;
        self.first_static_update = true;
        Ok(())
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for CallSite<R, E> {
    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> Option<Value> {
        let mut set: LPooled<Vec<BindId>> = LPooled::take();
        // Update all arg nodes every cycle, publishing values via bind IDs
        for arg in self.args.values_mut() {
            if let Some(ref mut node) = arg.node {
                if let Some(v) = node.update(ctx, event) {
                    ctx.cached.insert(arg.id, v.clone());
                    event.variables.insert(arg.id, v);
                    set.push(arg.id);
                }
            }
        }
        // Statically resolved fast path. The post-typecheck
        // `resolve_static_calls` pass already invoked `(def.init)(...)`
        // and stored the Apply on `self.function`. We still run
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
        let bound = match (&self.function, self.fnode.update(ctx, event)) {
            _ if self.statically_resolved => {
                let first = self.first_static_update;
                self.first_static_update = false;
                first
            }
            (_, None) => false,
            (Some((fv, _)), Some(v)) if fv == &v => false,
            (_, Some(v)) => match v.downcast_ref::<LambdaDef<R, E>>() {
                None => panic!("value {v:?} is not a function"),
                Some(lb) => {
                    let scope = self.scope.clone();
                    self.bind(ctx, scope, self.flags, v.clone(), lb, event, &mut set)
                        .expect("failed to bind to lambda");
                    true
                }
            },
        };
        match &mut self.function {
            None => {
                for id in set.drain(..) {
                    event.variables.remove(&id);
                }
                None
            }
            Some((_, f)) if !bound => {
                let res = f.update(ctx, &mut self.arg_refs, event);
                for id in set.drain(..) {
                    event.variables.remove(&id);
                }
                res
            }
            Some((_, f)) => {
                let init = mem::replace(&mut event.init, true);
                let mut refs = Refs::default();
                f.refs(&mut refs);
                refs.with_external_refs(|id| {
                    if let Entry::Vacant(e) = event.variables.entry(id) {
                        if let Some(v) = ctx.cached.get(&id) {
                            e.insert(v.clone());
                            set.push(id);
                        }
                    }
                });
                let res = f.update(ctx, &mut self.arg_refs, event);
                event.init = init;
                for id in set.drain(..) {
                    event.variables.remove(&id);
                }
                res
            }
        }
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        if let Some((_, mut f)) = self.function.take() {
            f.delete(ctx)
        }
        self.fnode.delete(ctx);
        for arg in self.args.values_mut() {
            if let Some(ref mut n) = arg.node {
                n.delete(ctx);
            }
        }
        for n in &mut self.arg_refs {
            n.delete(ctx);
        }
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        if let Some((_, f)) = &mut self.function {
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
                let ftype = ftype.reset_tvars();
                ftype.alias_tvars(&mut LPooled::take());
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
                                    Arg {
                                        id: BindId::new(),
                                        node: Some(Nop::new(arg.typ.clone())),
                                        is_default: true,
                                    },
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
        for (tv, tc) in ftype.constraints.read().iter() {
            wrap!(self, tc.check_contains(&ctx.env, &Type::TVar(tv.clone())))?;
        }
        if let Some(t) = ftype.throws.with_deref(|t| t.cloned()) {
            match ctx.env.lookup_catch(&self.scope.dynamic) {
                Ok(id) => {
                    if let Some(bind) = ctx.env.by_id.get(&id)
                        && let Type::TVar(tv) = &bind.typ
                    {
                        let tv = tv.read();
                        let mut ty = tv.typ.write();
                        *ty = match &*ty {
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
                            self.spec.ori, self.spec.pos, t, self.fnode.spec()
                        )
                    }
                    if self.flags.contains(CFlag::WarnUnhandled) {
                        eprintln!(
                            "WARNING: {} at {} error {} raised from function call {} will not be caught",
                            self.spec.ori, self.spec.pos, t, self.fnode.spec()
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
        // possible callee." The default's type comes straight off the
        // default node this call site compiled (`a.node`), which is the
        // same value typecheck0 resolved — no AST cell, and correct
        // per-call-site (the default Expr is shared across sites, but its
        // compiled node is not).
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
        if let Some((_, fun)) = &self.function {
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

    fn view(&self) -> crate::NodeView<'_, R, E> {
        crate::NodeView::CallSite(self)
    }

    fn emit_clif(
        &self,
        cx: &mut crate::gir_jit::BodyCx,
    ) -> Result<crate::gir_jit::CompiledExpr> {
        if let Some((_, f)) = &self.function {
            // A resolved user-lambda callee is a cross-kernel call:
            // `try_fuse`'s analysis discovered the site and built (or
            // cache-hit) the callee kernel — emit a CLIF `call`
            // against it. An undiscovered site (the lambda didn't
            // build — unsupported arg/return shape, body that doesn't
            // lower) de-fuses and the subtree node-walks.
            if matches!(f.view(), crate::ApplyView::Lambda(_)) {
                if let Some(info) = cx.lambda_site(self.spec.id).cloned() {
                    return crate::gir_jit::emit_lambda_call_node(cx, self, &info);
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
        // site is #203-UNRESOLVED — `self.function` is None — so this
        // check lives OUTSIDE the resolved-Apply block. Matched by the
        // self BindId (names shadow, #206; ids don't); captures
        // forward from this kernel's own params (bound with their
        // BindIds).
        if let Some((sb, info)) = cx.self_call_info() {
            let is_self = matches!(
                self.fnode.view(),
                crate::NodeView::Ref(r) if r.id == *sb
            );
            if is_self {
                let info = info.clone();
                return crate::gir_jit::emit_lambda_call_node(cx, self, &info);
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
            None => bail!("emit_clif: builtin call site not discovered — doesn't fuse"),
        };
        let spec_apply = match &self.spec.kind {
            crate::expr::ExprKind::Apply(a) => a,
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
        crate::gir_jit::emit_dyncall_node(cx, &info, &arg_nodes)
    }

    fn clone_rebind(&self, ctx: &mut ExecCtx<R, E>, scope: &Scope) -> Node<R, E> {
        // Bind-shaped: re-mint the arg-slot ids (genn-minted, NOT env-named,
        // so they need a local old→new map), clone each arg value node (the
        // element ref re-resolves its name to MapQ's fresh element), rebuild
        // arg_refs to point at the fresh slot ids, then clone the fnode and
        // the bound callee `Apply` structurally (preserving its fused body).
        // Precondition: the template CallSite is already bound + fused.
        let mut id_remap: AHashMap<BindId, BindId> = AHashMap::default();
        let mut new_args: AHashMap<ArgKey, Arg<R, E>> = AHashMap::default();
        for (key, arg) in &self.args {
            let new_id = BindId::new();
            id_remap.insert(arg.id, new_id);
            let node = arg.node.as_ref().map(|n| n.clone_rebind(ctx, scope));
            new_args.insert(
                key.clone(),
                Arg { id: new_id, node, is_default: arg.is_default },
            );
        }
        let arg_refs: Vec<Node<R, E>> = self
            .arg_refs
            .iter()
            .map(|r| {
                let old = (&**r as &dyn std::any::Any)
                    .downcast_ref::<Ref>()
                    .expect("arg_ref must be a Ref");
                let id = id_remap.get(&old.id).copied().unwrap_or(old.id);
                ctx.rt.ref_var(id, self.top_id);
                Box::new(Ref {
                    spec: old.spec.clone(),
                    typ: old.typ.clone(),
                    id,
                    top_id: self.top_id,
                }) as Node<R, E>
            })
            .collect();
        let fnode = self.fnode.clone_rebind(ctx, scope);
        // A `GXLambda` callee is cloned STRUCTURALLY — preserving its fused
        // body. A builtin callee is instead left UNBOUND (`function: None`,
        // `statically_resolved: false`) so the clone re-binds it on its
        // first update, re-running the builtin's own `init` to produce a
        // FRESH independent instance (its own subscription, counter, timer,
        // …). Re-bind is correct for builtins precisely because they don't
        // fuse their body, so nothing is lost by rebuilding from `init` —
        // and it needs zero per-builtin clone code. (A `GXLambda` must NOT
        // be re-bound that way: re-init would recompile its body from spec,
        // discarding the spliced FusedKernels.)
        let (function, statically_resolved) = match &self.function {
            Some((v, apply)) if matches!(apply.view(), crate::ApplyView::Lambda(_)) => (
                Some((v.clone(), apply.clone_rebind(ctx, scope))),
                self.statically_resolved,
            ),
            _ => (None, false),
        };
        Box::new(Self {
            spec: self.spec.clone(),
            ftype: self.ftype.clone(),
            resolved_ftype: self.resolved_ftype.clone(),
            rtype: self.rtype.clone(),
            fnode,
            args: new_args,
            arg_refs,
            function,
            flags: self.flags,
            scope: self.scope.clone(),
            top_id: self.top_id,
            statically_resolved,
            // Re-prime on the clone's first update (like a fresh bind).
            first_static_update: true,
        })
    }
}
