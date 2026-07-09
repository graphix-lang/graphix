use super::{Cached, compiler::compile, pattern::StructPatternNode};
use crate::{
    BindId, CFlag, Event, ExecCtx, Node, NodeView, PrintFlag, RebindMap, Refs, Rt, Scope,
    Update, UserEvent,
    expr::{Expr, ExprId, Pattern},
    format_with_flags,
    fusion::emit::{BodyCx, CompiledExpr, emit_select_node},
    node::pattern::PatternNode,
    typ::Type,
    wrap,
};
use anyhow::{Context, Result, anyhow, bail};
use compact_str::format_compact;
use enumflags2::BitFlags;
use netidx::subscriber::Value;
use netidx_value::Typ;
use poolshark::local::LPooled;
use std::collections::hash_map::Entry;

atomic_id!(SelectId);

#[derive(Debug)]
pub struct Select<R: Rt, E: UserEvent> {
    pub(super) selected: Option<usize>,
    pub arg: Cached<R, E>,
    pub arms: Vec<(PatternNode<R, E>, Cached<R, E>)>,
    pub typ: Type,
    pub(crate) spec: Expr,
}

impl<R: Rt, E: UserEvent> Select<R, E> {
    /// Build a `Select` node from an already-compiled scrutinee
    /// expression and a vector of (pattern, arm body) pairs.
    #[allow(dead_code)]
    pub fn new(
        arg: Node<R, E>,
        arms: Vec<(PatternNode<R, E>, Node<R, E>)>,
        typ: Type,
        spec: Expr,
    ) -> Node<R, E> {
        let arms = arms.into_iter().map(|(p, n)| (p, Cached::new(n))).collect::<Vec<_>>();
        Box::new(Self { spec, typ, arg: Cached::new(arg), arms, selected: None })
    }

    pub(crate) fn compile(
        ctx: &mut ExecCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: Expr,
        scope: &Scope,
        top_id: ExprId,
        arg: &Expr,
        arms: &[(Pattern, Expr)],
    ) -> Result<Node<R, E>> {
        let arg = Cached::new(compile(ctx, flags, arg.clone(), scope, top_id)?);
        let arms = arms
            .iter()
            .map(|(pat, spec)| {
                let scope = scope.append(&format_compact!("sel{}", SelectId::new().0));
                let pat = PatternNode::compile(
                    ctx,
                    flags,
                    pat,
                    &scope,
                    top_id,
                    spec.pos,
                    spec.ori.clone(),
                )
                .with_context(|| format!("in select at {}", spec.pos))?;
                let n = Cached::new(compile(ctx, flags, spec.clone(), &scope, top_id)?);
                Ok((pat, n))
            })
            .collect::<Result<Vec<_>>>()
            .with_context(|| format!("in select at {}", spec.pos))?;
        let typ = Type::empty_tvar();
        Ok(Box::new(Self { spec, typ, arg, arms, selected: None }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Select<R, E> {
    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> Option<Value> {
        let Self { selected, arg, arms, typ: _, spec: _ } = self;
        let mut pat_up = false;
        let arg_up = arg.update(ctx, event);
        macro_rules! bind {
            ($i:expr) => {{
                if let Some(arg) = arg.cached.as_ref() {
                    arms[$i].0.bind_event(ctx, event, arg);
                }
            }};
        }
        for (pat, _) in arms.iter_mut() {
            if arg_up && pat.guard.is_some() {
                if let Some(arg) = arg.cached.as_ref() {
                    pat.bind_event(ctx, event, arg);
                }
            }
            pat_up |= pat.update(ctx, event);
            if arg_up && pat.guard.is_some() {
                pat.unbind_event(event);
            }
        }
        if std::env::var_os("GRAPHIX_DBG_SELECT").is_some() {
            eprintln!(
                "SELECT upd init={} arg_up={arg_up} pat_up={pat_up} sel={:?} vars={}",
                event.init,
                selected,
                event.variables.len()
            );
        }
        if !arg_up && !pat_up {
            self.selected.and_then(|i| {
                if arms[i].1.update(ctx, event) { arms[i].1.cached.clone() } else { None }
            })
        } else {
            let sel = match arg.cached.as_ref() {
                None => None,
                Some(v) => arms.iter().enumerate().find_map(|(i, (pat, _))| {
                    if pat.is_match(&ctx.env, v) { Some(i) } else { None }
                }),
            };
            match (sel, *selected) {
                (Some(i), Some(j)) if i == j => {
                    if arg_up {
                        bind!(i);
                    }
                    if arms[i].1.update(ctx, event) || arg_up {
                        arms[i].1.cached.clone()
                    } else {
                        None
                    }
                }
                (Some(i), Some(_) | None) => {
                    let mut set: LPooled<Vec<BindId>> = LPooled::take();
                    if let Some(j) = *selected {
                        arms[j].1.node.sleep(ctx);
                    }
                    *selected = Some(i);
                    bind!(i);
                    let mut refs = Refs::default();
                    arms[i].1.node.refs(&mut refs);
                    refs.with_external_refs(|id| {
                        if let Entry::Vacant(e) = event.variables.entry(id)
                            && let Some(v) = ctx.cached.get(&id)
                        {
                            e.insert(v.clone());
                            set.push(id);
                        }
                    });
                    let init = event.init;
                    event.init = true;
                    arms[i].1.update(ctx, event);
                    event.init = init;
                    for id in set.drain(..) {
                        event.variables.remove(&id);
                    }
                    arms[i].1.cached.clone()
                }
                (None, Some(j)) => {
                    arms[j].1.node.sleep(ctx);
                    *selected = None;
                    None
                }
                (None, None) => None,
            }
        }
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        let Self { selected: _, arg, arms, typ: _, spec: _ } = self;
        arg.node.delete(ctx);
        for (pat, arg) in arms {
            arg.node.delete(ctx);
            pat.delete(ctx);
        }
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        let Self { selected: _, arg, arms, typ: _, spec: _ } = self;
        arg.sleep(ctx);
        for (pat, arg) in arms {
            arg.sleep(ctx);
            if let Some(n) = &mut pat.guard {
                n.sleep(ctx)
            }
        }
    }

    fn refs(&self, refs: &mut Refs) {
        let Self { selected: _, arg, arms, typ: _, spec: _ } = self;
        arg.node.refs(refs);
        for (pat, arg) in arms {
            arg.node.refs(refs);
            pat.structure_predicate.ids(&mut |id| {
                refs.bound.insert(id);
            });
            if let Some(n) = &pat.guard {
                n.node.refs(refs);
            }
        }
    }

    fn typecheck0(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        self.arg.node.typecheck0(ctx)?;
        let mut rtype = Type::Primitive(BitFlags::empty());
        let mut mtype = Type::Primitive(BitFlags::empty());
        let mut itype = Type::Primitive(BitFlags::empty());
        let mut saw_true = false;
        let mut saw_false = false;
        // An UNGUARDED wildcard (an arm whose pattern is irrefutable
        // with an INFERRED type predicate — a bind-all / destructure of
        // binds) matches anything, so its presence makes the select
        // exhaustive by construction. Its raw predicate is a fresh TVar
        // (or a composite of them) and must stay OUT of the coverage
        // unions: `check_contains` is a greedy unifying walk, and fed
        // the wildcard's tvar it bound it to the FIRST scrutinee union
        // member it met and reported the rest missing (a guarded arm
        // followed by a bind-all final was rejected as non-exhaustive);
        // fed the scrutinee's own not-yet-bound cell it burned that
        // instead.
        let mut wildcard = false;
        for (pat, n) in self.arms.iter_mut() {
            let inferred_irrefutable = !pat.explicit_type_predicate
                && pat.structure_predicate.matches_anything();
            match &mut pat.guard {
                Some(guard) => guard.node.typecheck0(ctx)?,
                None => {
                    if inferred_irrefutable {
                        wildcard = true;
                    } else if !pat.structure_predicate.is_refutable() {
                        mtype = mtype.union(&ctx.env, &pat.type_predicate)?
                    } else if let StructPatternNode::Literal(Value::Bool(b)) =
                        &pat.structure_predicate
                    {
                        saw_true |= b;
                        saw_false |= !b;
                        if saw_true && saw_false {
                            mtype = mtype
                                .union(&ctx.env, &Type::Primitive(Typ::Bool.into()))?;
                        }
                    }
                }
            }
            if !inferred_irrefutable {
                itype = itype.union(&ctx.env, &pat.type_predicate)?;
            }
            // NOTE: rtype (the arm-result union) is built in the
            // SECOND loop, after each arm's typecheck0 — an arm whose
            // node REPLACES its typ field there (a nested select sets
            // `self.typ = rtype`) would otherwise contribute its
            // pre-typecheck EMPTY tvar to the union: an orphaned cell
            // the enclosing annotation check greedily bound (an
            // `-> i64` lambda/let accepted a nested select yielding
            // STRUCTS; the fused return slot then leaked the struct
            // pointer as a scalar — soak jul08o).
        }
        if wildcard {
            // Exhaustive by construction — but still narrow an
            // under-constrained scrutinee against the union of the
            // informative arm predicates (a side-effect-only walk, the
            // bool is discarded). This is where
            // `|n, acc| select n { 0 => .., _ => .. }` learns n: i64;
            // without a wildcard the coverage checks below perform the
            // same narrowing.
            if itype != Type::Primitive(BitFlags::empty()) {
                let _ = itype.contains(&ctx.env, &self.arg.node.typ())?;
            }
        } else {
            itype.check_contains(&ctx.env, &self.arg.node.typ()).map_err(|e| {
                format_with_flags(PrintFlag::DerefTVars, || {
                    anyhow!("missing match cases {e}")
                })
            })?;
            mtype.check_contains(&ctx.env, &self.arg.node.typ()).map_err(|e| {
                format_with_flags(PrintFlag::DerefTVars, || {
                    anyhow!("missing match cases {e}")
                })
            })?;
        }
        let mut ntype = self.arg.node.typ().clone().normalize();
        for (pat, n) in self.arms.iter_mut() {
            // make sure tvars are aliased properly even if itype was Any.
            // Alias against the NARROWED scrutinee type — the scrutinee
            // minus every EARLIER unguarded irrefutable arm's coverage
            // (the same subtraction the dead-arm walk below performs):
            // in `select opt { null as _ => "", s => s }` the value
            // reaching `s` cannot be null, so `s` is `string`, not
            // `[string, null]`. Aliasing against the full scrutinee
            // widened every post-narrowing bind (this used to come out
            // right only when the coverage walk happened to greedily
            // bind the tvar to the union's first member).
            //
            // Unify through the `any_as_tvar` VIEW (same TVar cells, `Any`
            // leaves swapped for throwaway fresh TVars): the contains walk
            // short-circuits composite pairs on the first false, and
            // `T.contains(Any)` is false — a `_` slot would otherwise stop
            // the walk and leave every LATER slot's bind TVars un-narrowed.
            ntype.contains(&ctx.env, &pat.type_predicate.any_as_tvar())?;
            wrap!(n.node, n.node.typecheck0(ctx))?;
            rtype = rtype.union(&ctx.env, n.node.typ())?;
            if !pat.structure_predicate.is_refutable() && pat.guard.is_none() {
                ntype = ntype.diff(&ctx.env, &pat.type_predicate)?;
            }
        }
        let mut atype = self.arg.node.typ().clone().normalize();
        for (pat, _) in self.arms.iter() {
            if !&pat.type_predicate.could_match(&ctx.env, &atype)? {
                format_with_flags(PrintFlag::DerefTVars, || {
                    bail!(
                        "pattern {} will never match {}, unused match cases",
                        pat.type_predicate,
                        atype
                    )
                })?
            }
            if !pat.structure_predicate.is_refutable() && pat.guard.is_none() {
                atype = atype.diff(&ctx.env, &pat.type_predicate)?;
            }
        }
        self.typ = rtype;
        Ok(())
    }

    fn typecheck1(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        self.arg.node.typecheck1(ctx)?;
        for (pat, n) in self.arms.iter_mut() {
            if let Some(guard) = &mut pat.guard {
                guard.node.typecheck1(ctx)?;
            }
            wrap!(n.node, n.node.typecheck1(ctx))?;
        }
        Ok(())
    }

    fn typ(&self) -> &Type {
        &self.typ
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn view(&self) -> NodeView<'_, R, E> {
        NodeView::Select(self)
    }

    fn emit_clif(&self, cx: &mut BodyCx) -> Result<CompiledExpr> {
        emit_select_node(cx, self)
    }

    fn clone_rebind(
        &self,
        ctx: &mut ExecCtx<R, E>,
        scope: &Scope,
        remap: &mut RebindMap,
    ) -> Node<R, E> {
        // The arg (scrutinee) references no arm bindings. Per arm, re-mint
        // the pattern FIRST (binds enter the scope name map), then clone the
        // body so its Refs resolve to the fresh pattern ids.
        let arg = Cached::new(self.arg.node.clone_rebind(ctx, scope, remap));
        let arms = self
            .arms
            .iter()
            .map(|(pat, body)| {
                // Each arm gets a FRESH sub-scope, exactly as
                // `Select::compile` does (`scope.append("sel{id}")`).
                // Without it, re-minting arm 2's binding `n` into the
                // shared `scope` pollutes it: a later clone (template →
                // per-slot) then resolves arm 1's `Ref(n)` to that stale
                // sibling binding (never written when arm 1 fires)
                // instead of the outer `n` → the arm produces nothing and
                // the HOF hangs (#167). Per-arm isolation is a
                // correctness invariant of `select`, and clone_rebind —
                // which builds the per-slot node graph — must preserve it.
                let arm_scope =
                    scope.append(&format_compact!("sel{}", SelectId::new().0));
                let pat = pat.clone_rebind(ctx, &arm_scope, remap);
                let body = Cached::new(body.node.clone_rebind(ctx, &arm_scope, remap));
                (pat, body)
            })
            .collect();
        Box::new(Self {
            selected: None,
            arg,
            arms,
            typ: self.typ.clone(),
            spec: self.spec.clone(),
        })
    }
}
