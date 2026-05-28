use super::{bind::Ref, compiler::compile, Nop, NOP};
use crate::{
    deref_typ,
    expr::{ErrorContext, Expr, ExprId},
    node::lambda::LambdaDef,
    typ::{FnArgKind, FnType, Type},
    wrap, Apply, BindId, CFlag, Event, ExecCtx, LambdaId, Node, PrintFlag, Refs, Rt,
    Scope, TypecheckPhase, Update, UserEvent,
};
use ahash::{AHashMap, AHashSet};
use anyhow::{bail, Context, Result};
use arcstr::ArcStr;
use enumflags2::BitFlags;
use netidx::subscriber::Value;
use nohash::IntMap;
use poolshark::local::LPooled;
use std::{collections::hash_map::Entry, mem};
use triomphe::Arc as TArc;

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

/// Find the FnType inside `t` that the lambda with id `id` was unified
/// with at typecheck time. The formal arg may be a bare `Type::Fn`, but
/// can also be a union like `[fn(...), null]` (the typical
/// optional-callback shape) or wrapped in a Set of fn types — in those
/// cases we walk the arms to find the unique matching Fn. Returns
/// `None` if no Fn arm is found.
fn find_fn_in_arg_type(t: &Type, id: LambdaId) -> Option<&TArc<FnType>> {
    match t {
        Type::Fn(ft) => Some(ft),
        Type::Set(ts) => {
            // Prefer an arm whose lambda_ids include the lambda we're
            // checking; fall back to the first Fn arm if none claim it.
            let mut fallback: Option<&TArc<FnType>> = None;
            for arm in ts.iter() {
                if let Some(ft) = find_fn_in_arg_type(arm, id) {
                    if ft.lambda_ids.read().contains(&id) {
                        return Some(ft);
                    }
                    fallback.get_or_insert(ft);
                }
            }
            fallback
        }
        _ => None,
    }
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
    /// `typecheck_inner` during the typechecker's call-site unification
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
    /// Used by fusion's walker to descend through a resolved call
    /// site into a user lambda's body or into a fusible builtin's
    /// own [`crate::GirEmitter::emit_gir`] hook. See
    /// [`crate::ApplyView`] for the variants.
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
    /// this gives [`crate::GirEmitter::emit_gir`] both views of the
    /// arg list.
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
                        let default_node =
                            match &f.argspec[i].labeled {
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
        let _ = rf.typecheck(ctx, &mut self.arg_refs, TypecheckPhase::Lambda);
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

    fn typecheck_inner(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        wrap!(self.fnode, self.fnode.typecheck(ctx))?;
        let ftype = match self.ftype.as_ref() {
            Some(ftype) => ftype, // already initialized
            None => {
                let ftype = deref_typ!("fn", ctx, self.fnode.typ(),
                    Some(Type::Fn(ftype)) => Ok(ftype.clone())
                )?;
                let ftype = ftype.reset_tvars();
                ftype.alias_tvars(&mut LPooled::take());
                self.ftype = Some(ftype.clone());
                // (Previously: published this Apply's call-site
                // FnType under `ctx.fn_types[self.spec.id]`. After
                // Phase 5, the fusion pass reads it off the
                // function expression's `typ` cell — set by
                // trait-default Update propagation — instead. The
                // Apply's typecheck propagation handles populating
                // the function expr's cell as part of recursing
                // into children.)
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
        let mut hof_idmap: LPooled<IntMap<LambdaId, usize>> = LPooled::take();
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
                if let Some(ref mut n) = arg.node {
                    farg.typ.contains(&ctx.env, n.typ())?;
                    wrap!(n, n.typecheck(ctx))?;
                    wrap!(n, farg.typ.check_contains(&ctx.env, n.typ()))?;
                    match deref_typ!("arg", ctx, n.typ(), Some(t) => Ok(Some(t.clone())), None => Ok(None))
                    {
                        Ok(Some(Type::Fn(ft))) => {
                            if !TArc::ptr_eq(&ftype.lambda_ids, &ft.lambda_ids) {
                                let ids = ft.lambda_ids.read();
                                if ids.len() > 0 {
                                    let mut wids = ftype.lambda_ids.write();
                                    for id in ids.iter().copied() {
                                        hof_idmap.insert(id, i);
                                        wids.insert(id);
                                    }
                                }
                            }
                        }
                        Ok(None | Some(_)) | Err(_) => (),
                    }
                }
            }
        }
        // Propagate concrete types from labeled-default expressions
        // into formal-arg TVars that the explicit args didn't refine.
        //
        // Why this matters: a polymorphic lambda like
        // `'a: [Int, Float] |#x: 'a = 0.0, …|` invoked without any
        // call-site arg that touches 'a would otherwise leave 'a
        // unresolved at this call site — the runtime later evaluates
        // the default and picks a concrete type, but compile-time
        // inference never sees that. The resulting Apply's return
        // type stays as a constraint-set TVar, which downstream
        // consumers (fusion, type-directed dispatch) can't lower.
        //
        // We run AFTER the args loop above so any explicit positional
        // or labeled-with-value arg has already refined 'a — `value:
        // &"option_a"` setting 'a=string takes precedence over a
        // sibling `#selected: &'a = &null` default. If both refine
        // and disagree, the unification here surfaces a real type
        // error rather than masking it.
        //
        // For a callee backed by a single LambdaDef (the common case
        // for stdlib direct calls), `ftype.lambda_ids` has one entry;
        // multi-lambda call sites (control flow yielding different
        // lambdas) are skipped — too ambiguous to refine soundly.
        let lambda_ids_for_defaults: Vec<LambdaId> = ftype
            .lambda_ids
            .read()
            .iter()
            .copied()
            .collect();
        if lambda_ids_for_defaults.len() == 1 {
            let id = lambda_ids_for_defaults[0];
            if let Some(val) = ctx.lambda_defs.get(&id).cloned() {
                if let Some(ldef) = val.downcast_ref::<LambdaDef<R, E>>() {
                    for farg in ftype.args.iter() {
                        let name = match &farg.kind {
                            FnArgKind::Labeled { name, has_default: true } => {
                                name
                            }
                            _ => continue,
                        };
                        let arg = match self.args.get(&ArgKey::Named(name.clone())) {
                            Some(a) if a.is_default => a,
                            _ => continue,
                        };
                        let _ = arg;
                        let def_typ = ldef.argspec.iter().find_map(|src| {
                            let n = src.pattern.single_bind()?;
                            if n.as_str() != name.as_str() {
                                return None;
                            }
                            let d = src.labeled.as_ref()?.as_ref()?;
                            d.typ.get().cloned()
                        });
                        if let Some(dt) = def_typ {
                            wrap!(self.fnode, farg.typ.check_contains(&ctx.env, &dt))?;
                        }
                    }
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
                            wrap!(n, n.typecheck(ctx))?;
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
        if !ftype.lambda_ids.read().is_empty() {
            let ftype = ftype.clone();
            let spec = self.spec.clone();
            ctx.deferred_checks.push(Box::new(move |ctx| {
                let resolved = ftype.resolve_tvars();
                let mut ids: LPooled<Vec<_>> =
                    ftype.lambda_ids.read().iter().copied().collect();
                for id in ids.drain(..) {
                    let resolved = match hof_idmap.get(&id) {
                        None => &resolved,
                        Some(i) => {
                            match find_fn_in_arg_type(&resolved.args[*i].typ, id) {
                                Some(ft) => ft,
                                None => bail!(
                                    "unexpected resolved arg type {}",
                                    &resolved.args[*i].typ
                                ),
                            }
                        }
                    };
                    if let Some(val) = ctx.lambda_defs.get(&id).cloned() {
                        let ldef = val
                            .downcast_ref::<LambdaDef<R, E>>()
                            .expect("failed to unwrap lambda for deferred check");
                        if let Some(apply) = &mut *ldef.check.lock() {
                            apply
                                .typecheck(
                                    ctx,
                                    &mut [],
                                    TypecheckPhase::CallSite(resolved),
                                )
                                .with_context(|| ErrorContext((*spec).clone()))?;
                        }
                    }
                }
                Ok(())
            }));
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
}
