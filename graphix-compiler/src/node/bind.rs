use super::{
    VarRead, WakeBit,
    coretraits::with_hooks,
    data::{struct_field_types, tuple_field_types},
    lambda::{DefBody, DefOrigin, LambdaDef},
    pattern::StructPatternNode,
    place::{self, Path, RefPath},
    read_var, standing_view,
};
use crate::{
    BindId, BuiltinBindInfo, CFlag, CompileCtx, ExecCtx, Node, NodeView, PrintFlag, Refs,
    Rt, Scope, Tag, TagValue, Update, UserEvent, bailat,
    compiler::compile,
    dbgenv,
    env::{self, Env},
    expr::{self, At, Expr, ExprId, ExprKind, ModPath, PlaceAccess},
    format_with_flags,
    fusion::{
        emit::{BodyCx, CompiledExpr, emit_ref_node},
        fuse,
    },
    ide::{FieldRefSite, ReferenceSite},
    image::{
        ImageBuf,
        nodes::{NodeTag, decode_node, put_tag},
    },
    typ::{
        CoreTrait, FnType, Mutability, Type,
        tvar::{AtLevel, Level, current_level},
    },
    wrap,
};
use anyhow::{Result, bail};
use arcstr::ArcStr;
use bytes::{Buf, BufMut};
use compact_str::CompactString;
use enumflags2::BitFlags;
use netidx_core::pack::{Pack, PackError, encode_varint};
use netidx_value::Value;
use poolshark::local::LPooled;
use smallvec::SmallVec;
use std::{any::Any, mem};
use triomphe::Arc;

/// A `let` pattern's fit to its value, which waits for the value's check.
#[derive(Debug)]
enum Deferred {
    /// The predicate of a pattern that leaves struct fields out, completed
    /// against the value as a select arm's against its scrutinee.
    Partial(Type),
    /// The predicate a pattern compiled against, over a value typed by an
    /// open cell, each name a fresh cell: held to every member of the
    /// value, each name typed by its part of each member.
    Shape(Type),
}

#[derive(Debug)]
pub struct Bind<R: Rt, E: UserEvent> {
    slept: WakeBit,
    /// The last cycle its readers were walked before it slept: a write
    /// landing after it is a fire no reader saw, re-raised at the wake.
    slept_at: u64,
    pub(crate) spec: Arc<Expr>,
    pub(crate) typ: Type,
    pub(crate) pattern: StructPatternNode,
    /// The pattern's fit to a value whose type the compile did not know,
    /// judged at the check.
    deferred: Option<Deferred>,
    pub(crate) node: Node<R, E>,
    ever_published: bool,
    /// The ids a wake re-raised into this cycle's event, absent from it
    /// before: the enclosing block takes them back after its last
    /// statement, so only the readers that slept with this `let` see them.
    reraised: SmallVec<[BindId; 1]>,
}

/// Rewrite a node into a block: each already-compiled operand becomes a
/// `let <name> = <node>` binding (moved, never recompiled) and `body`,
/// an expression over those names compiled under a block scope of its
/// own below `scope`, is the value.
pub(crate) fn lower_over_operands<R: Rt, E: UserEvent>(
    ctx: &mut CompileCtx<R, E>,
    flags: BitFlags<CFlag>,
    scope: &Scope,
    spec: &Expr,
    top_id: ExprId,
    operands: impl IntoIterator<Item = (ArcStr, Node<R, E>)>,
    body: Expr,
) -> Result<Node<R, E>> {
    // named by the lowered node's spec, which an image writes, as a
    // block's scope is
    let scope = scope.append_block("op", spec.id.inner());
    let mut children: Vec<Node<R, E>> = Vec::new();
    for (name, node) in operands {
        let typ = node.typ().clone();
        let pattern = StructPatternNode::compile(
            ctx,
            &typ,
            &expr::StructurePattern::Bind(name.clone().into()),
            &scope,
            spec.pos,
            spec.ori.clone(),
        )?;
        let bspec = Expr::synth(
            spec,
            ExprKind::Bind(Arc::new(expr::BindExpr {
                rec: false,
                pattern: expr::StructurePattern::Bind(name.into()),
                typ: None,
                value: node.spec().clone(),
            })),
        );
        children.push(Node::new(Bind {
            spec: Arc::new(bspec),
            typ,
            pattern,
            node,
            deferred: None,
            ever_published: false,
            slept: WakeBit::default(),
            slept_at: 0,
            reraised: SmallVec::new(),
        }));
    }
    let mut body = compile(ctx, flags, body, &scope, top_id)?;
    body.typecheck0(ctx)?;
    body.typecheck1(ctx)?;
    let bspec = Expr::synth(
        spec,
        ExprKind::Block {
            exprs: Arc::from_iter(
                children.iter().chain([&body]).map(|n| n.spec().clone()),
            ),
        },
    );
    children.push(body);
    Ok(super::Block::new(false, children.into_boxed_slice(), Arc::new(bspec)))
}

/// What `let name = |..| 'builtin` records of the builtin it binds;
/// `None` for any other binding.
fn builtin_binding<R: Rt, E: UserEvent>(
    node: &Node<R, E>,
    value: &Expr,
) -> Option<BuiltinBindInfo> {
    let (Some(l), ExprKind::Lambda(lam), Type::Fn(typ)) =
        (lambda_value(node), &value.unparen().kind, node.typ())
    else {
        return None;
    };
    let def = l.def_value().downcast_ref::<LambdaDef<R, E>>()?;
    let DefOrigin::Source { body: DefBody::BuiltIn(name), .. } = &def.origin else {
        return None;
    };
    Some(BuiltinBindInfo {
        name: name.clone(),
        argspec: lam.args.clone(),
        typ: typ.clone(),
        lambda_id: Some(def.id),
    })
}

impl<R: Rt, E: UserEvent> Bind<R, E> {
    /// A write that landed while the binding slept (one queued before the
    /// sleep, a seq machine's reset) was seen by none of the readers that
    /// slept with it: deliver it to them fired, once, at the wake. A reader
    /// outside (a `&mut` that escaped) saw it live, so the enclosing block
    /// takes the delivery back after its last statement.
    fn reraise_writes_while_asleep(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        let slept_at = self.slept_at;
        let mut late: SmallVec<[(BindId, TagValue); 2]> = SmallVec::new();
        self.pattern.ids(&mut |id| {
            if !ctx.event.variables.contains_key(&id)
                && let Some((tv, stamp)) = ctx.rt.store_get(&id)
                && *stamp > slept_at
                && !tv.tag().is_bottom()
            {
                late.push((id, TagValue::fired(tv.value_cloned())))
            }
        });
        self.reraised.clear();
        for (id, tv) in late {
            ctx.publish(id, tv);
            self.reraised.push(id);
        }
    }

    /// Take back what the wake re-raised ([`Self::reraised`]).
    pub(crate) fn take_back_reraised(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        for id in self.reraised.drain(..) {
            ctx.event.variables.remove(&id);
        }
    }

    pub(crate) fn compile(
        ctx: &mut CompileCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: Arc<Expr>,
        scope: &Scope,
        top_id: ExprId,
        b: &expr::BindExpr,
    ) -> Result<Node<R, E>> {
        let expr::BindExpr { rec, pattern: pat, typ: annotation, value } = b;
        let annotation = match annotation {
            Some(t) => {
                let t = t.scope_refs(&scope.lexical).rewrite_trait_args(&ctx.env)?;
                super::lambda::body_annotation(ctx, &t);
                Some(t)
            }
            None => None,
        };
        let compile_pattern = |ctx: &mut CompileCtx<R, E>, typ: &Type| {
            StructPatternNode::compile(ctx, typ, pat, scope, spec.pos, spec.ori.clone())
                .at(&spec)
        };
        let mut deferred = None;
        let mut narrowed = None;
        let (node, pattern, typ) = if *rec {
            if pat.single_bind().is_none() {
                bailat!(spec, "can't use rec on a complex pattern")
            }
            if !matches!(value.unparen().kind, ExprKind::Lambda(_)) {
                bailat!(spec, "let rec may only be used for lambdas")
            }
            // a name for the definition's scheme: it lowers nothing it binds
            let typ = annotation.unwrap_or_else(|| {
                let _generic = AtLevel::enter(Level::GENERIC);
                Type::empty_tvar()
            });
            // bound before the value compiles, so the body's
            // self-references see the annotation
            let pattern = compile_pattern(ctx, &typ)?;
            let node = compile(ctx, flags, value.clone(), &scope, top_id)?;
            let ntyp = node.typ();
            if !typ.contains(&ctx.env, ntyp).at(&spec)? {
                format_with_flags(PrintFlag::DerefTVars, || {
                    bailat!(spec, "match error {ntyp} can't be matched by {typ}")
                })?
            }
            (node, pattern, typ)
        } else {
            let node = compile(ctx, flags, value.clone(), &scope, top_id)?;
            let typ = match annotation {
                Some(typ) => typ,
                None => {
                    // A ⊥ initializer seeds a fresh cell its writers
                    // refine, settled to ⊥ in typecheck1 if none do.
                    let typ =
                        if node.typ().with_deref(|t| matches!(t, Some(Type::Bottom))) {
                            Type::empty_tvar()
                        } else {
                            node.typ().clone()
                        };
                    // a shape check: its cells lower nothing they bind; a
                    // partial struct pattern completes against the value, as
                    // a select arm's does against its scrutinee
                    let (ptyp, completed) = {
                        let _generic = AtLevel::enter(Level::GENERIC);
                        let ptyp = pat.infer_type_predicate(&ctx.env, &scope.lexical)?;
                        match pat
                            .complete_type_predicate(&ctx.env, &ptyp, &typ)
                            .at(&spec)?
                        {
                            Some(t) => (t, true),
                            None => (ptyp, false),
                        }
                    };
                    // a `_` matches what the value holds, deciding none of it
                    if !completed && pat.has_partial_struct() {
                        deferred = Some(Deferred::Partial(ptyp));
                    } else if pat.single_bind().is_none() {
                        if node.typ().with_deref(|t| t.is_none()) {
                            let pred = pat
                                .infer_type_predicate(&ctx.env, &scope.lexical)?
                                .any_as_tvar();
                            deferred = Some(Deferred::Shape(pred));
                        } else if !fits_every_member(
                            ctx,
                            &ptyp,
                            &typ,
                            &mut narrowed,
                            || pat.infer_type_predicate(&ctx.env, &scope.lexical),
                        )
                        .at(&spec)?
                        {
                            misfit(ctx, &spec, &ptyp, &typ)?
                        }
                    }
                    typ
                }
            };
            let shape = match &deferred {
                Some(Deferred::Partial(t) | Deferred::Shape(t)) => Some(t),
                None => narrowed.as_ref(),
            };
            let pattern = compile_pattern(ctx, shape.unwrap_or(&typ))?;
            if let Some(shape) = &narrowed {
                pattern.retype_by_member(ctx, &typ, shape).at(&spec)?;
            }
            if let Some(Deferred::Shape(_)) = &deferred {
                pattern.ids(&mut |id| ctx.env.retype(id, Type::empty_tvar()));
            }
            (node, pattern, typ)
        };
        if deferred.is_none()
            && narrowed.is_none()
            && !pattern.covers(&ctx.env, &typ, false)
        {
            bailat!(spec, "refutable patterns are not allowed in let");
        }
        // Registered after the value compiled so a `let rec` body's
        // self-references keep the definition's cells.
        if lambda_value(&node).is_some() || forwards(&ctx.env, b, &node) {
            pattern.ids(&mut |id| {
                ctx.env.poly_binds.insert(id);
            });
        }
        // a name its block writes, wherever the writer stands, is a `<-`
        // target from its binding on
        if let Some(written) = ctx.written_lets.last()
            && !written.is_empty()
        {
            let mut targets: SmallVec<[BindId; 4]> = SmallVec::new();
            pattern.ids(&mut |id| {
                let name = ctx.env.by_id.get(&id).map(|b| b.name.clone());
                if name.is_some_and(|n| written.contains(&(spec.id, n))) {
                    targets.push(id)
                }
            });
            for id in targets {
                ctx.mark_connect_target(id).map_err(|e| e.at(&spec))?;
                if let Some(b) = ctx.env.by_id.get(&id) {
                    b.typ.claim_free(current_level())
                }
            }
        }
        if !matches!(pat, expr::StructurePattern::Bind(_)) {
            pattern.ids(&mut |id| ctx.env.mark_let_pattern(id));
        }
        // Keyed by (scope, name), not BindId: sig and impl get different
        // ids for one builtin binding. A later `let` of the name shadows it.
        // every name the pattern rebinds forgets the builtin it named; a
        // single name over a builtin names that one
        pat.with_names(&mut |name| {
            let key = (scope.lexical.clone(), CompactString::from(name.as_str()));
            ctx.builtin_bindings.remove(&key);
        });
        if let expr::StructurePattern::Bind(name) = pat
            && let Some(info) = builtin_binding(&node, value)
        {
            let key = (scope.lexical.clone(), CompactString::from(name.as_str()));
            ctx.builtin_bindings.insert(key, info);
        }
        Ok(Node::new(Self {
            spec,
            typ,
            pattern,
            node,
            deferred,
            ever_published: false,
            slept: WakeBit::default(),
            slept_at: 0,
            reraised: SmallVec::new(),
        }))
    }

    /// Fit the pattern to the value's type, which must be known by now.
    fn complete_deferred(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
        match self.deferred.take() {
            None => Ok(()),
            Some(Deferred::Partial(ptyp)) => self.complete_partial(ctx, ptyp),
            Some(Deferred::Shape(pred)) => self.complete_shape(ctx, pred),
        }
    }

    /// Hold `pred`, the predicate the pattern compiled against, to every
    /// member of the value's type, and type each name, a fresh cell, by
    /// its part of each member.
    fn complete_shape(&mut self, ctx: &mut CompileCtx<R, E>, pred: Type) -> Result<()> {
        let spec = &self.spec;
        let mut narrowed = None;
        if !fits_every_member(ctx, &pred, &self.typ, &mut narrowed, || Ok(pred.clone()))
            .at(spec)?
        {
            return misfit(ctx, spec, &pred, &self.typ);
        }
        if narrowed.is_none() {
            self.pattern.realign(&ctx.env, &self.typ).at(spec)?;
            if !self.pattern.covers(&ctx.env, &self.typ, false) {
                bailat!(spec, "refutable patterns are not allowed in let");
            }
        }
        for leaf in self.pattern.names_by_member(&ctx.env, &self.typ, &pred).at(spec)? {
            if let Some(b) = ctx.env.by_id.get(&leaf.id) {
                b.typ.clone().check_contains(&ctx.env, &leaf.typ).at(spec)?
            }
        }
        Ok(())
    }

    /// Complete a pattern that leaves struct fields out against the
    /// value's type.
    fn complete_partial(&mut self, ctx: &mut CompileCtx<R, E>, ptyp: Type) -> Result<()> {
        let ExprKind::Bind(b) = &self.spec.kind else { return Ok(()) };
        let spec = &self.spec;
        let t = b.pattern.complete_type_predicate(&ctx.env, &ptyp, &self.typ).at(spec)?;
        let Some(t) = t else {
            if self.typ.with_deref(|t| t.is_none()) {
                bailat!(spec, "non exhaustive struct matches require type annotations")
            }
            return format_with_flags(PrintFlag::DerefTVars, || {
                bailat!(spec, "match error {} can't be matched by {ptyp}", self.typ)
            });
        };
        if !t.contains(&ctx.env, &self.typ).at(spec)? {
            format_with_flags(PrintFlag::DerefTVars, || {
                bailat!(spec, "match error {} can't be matched by {t}", self.typ)
            })?
        }
        self.pattern.realign(&ctx.env, &t).at(spec)?;
        if !self.pattern.covers(&ctx.env, &self.typ, false) {
            bailat!(spec, "refutable patterns are not allowed in let");
        }
        Ok(())
    }

    /// The binding holds its value's type, unless it forwards it.
    fn check_value(&self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
        let forwards = match &self.spec.kind {
            ExprKind::Bind(b) => forwards(&ctx.env, b, &self.node),
            _ => false,
        };
        if !forwards {
            let mut written = false;
            self.pattern.ids(&mut |id| written |= ctx.connect_targets.contains(&id));
            let value = match written {
                true => ctx.monomorphic(self.node.typ()),
                false => self.node.typ().clone(),
            };
            wrap!(self.node, self.typ.check_contains(&ctx.env, &value))?;
            let annotated =
                matches!(&self.spec.kind, ExprKind::Bind(b) if b.typ.is_some());
            if annotated {
                let scope = Scope::root();
                let (flags, top) = (BitFlags::empty(), self.spec.id);
                wrap!(
                    self.node,
                    super::callsite::check_hidden_defaults(
                        ctx,
                        flags,
                        &scope,
                        top,
                        &self.typ,
                        &value,
                        super::callsite::Narrowing::Copy
                    )
                )?;
            }
        }
        Ok(())
    }

    /// What the binding tells the rest of the check: the settle of a
    /// `let` over ⊥, and the lambda a name binds.
    fn publish(&self, ctx: &mut CompileCtx<R, E>) {
        if let Type::TVar(tv) = &self.typ {
            super::defer_settle(ctx, || crate::PendingSettle::LetOverBottom {
                tv: tv.clone(),
                init: self.node.typ().clone(),
                spec: self.spec.clone(),
            });
        }
        if let Some(fv) = self.lambda_def_value().or_else(|| self.aliased_lambda(ctx)) {
            self.pattern.ids(&mut |id| {
                if crate::dbgenv::gxdbg_resolve() {
                    eprintln!("B2L-INS {id:?} {}", self.spec);
                }
                ctx.bind_to_lambda.insert(id, fv.clone());
            });
        }
    }

    /// The LambdaDef `Value` of the binding this one aliases (`let h =
    /// f`), when `f` resolves statically and no `<-` writes it.
    fn aliased_lambda(&self, ctx: &mut CompileCtx<R, E>) -> Option<Value> {
        match ungrouped(&self.node) {
            NodeView::Ref(r) if !ctx.batch_connect_targets.contains(&r.id) => {
                let fv = ctx.bind_to_lambda.get(&r.id).cloned();
                if fv.is_some() {
                    ctx.record_static_callee(r.id, &self.spec);
                }
                fv
            }
            _ => None,
        }
    }

    /// The LambdaDef `Value` this binding holds when its value node is
    /// a lambda; `None` otherwise.
    pub(crate) fn lambda_def_value(&self) -> Option<Value> {
        lambda_value(&self.node).map(|l| l.def_value().clone())
    }
}

impl<R: Rt, E: UserEvent> Bind<R, E> {
    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<'_, R, E>,
        buf: &mut &[u8],
    ) -> Result<Node<R, E>, PackError> {
        let spec = Arc::new(Expr::decode(buf)?);
        let typ = Type::decode(buf)?;
        let pattern = StructPatternNode::decode(buf)?;
        let node = decode_node(ctx, buf)?;
        Ok(Node::new(Self {
            spec,
            typ,
            pattern,
            node,
            deferred: None,
            ever_published: false,
            slept: WakeBit::default(),
            slept_at: 0,
            reraised: SmallVec::new(),
        }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Bind<R, E> {
    fn for_each_child<'a>(&'a self, f: &mut dyn FnMut(&'a Node<R, E>)) {
        f(&self.node)
    }

    fn for_each_child_mut(&mut self, f: &mut dyn FnMut(&mut Node<R, E>)) {
        f(&mut self.node)
    }

    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::Bind, buf);
        self.spec.encode(buf)?;
        self.typ.encode(buf)?;
        self.pattern.encode(buf)?;
        self.node.image_encode(buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R, E>) -> &TagValue {
        let woke = self.slept.take();
        if woke {
            // CR claude for eric: [bug/low] At a wake the write that landed during the
            // sleep is re-raised here. The initializer then updates with its input's
            // catch-up fire and publishes over it, even when that input fired before
            // the write. Catch-up loses the order inside the sleep, so the let ends on
            // a value the awake program never ends on: in the probe 200 where awake it
            // settles on 2. Rule whether a caught-up initializer fire may override a
            // write stamped after it. If not, keep the re-raised write when its stamp
            // is newer than the stamp of the initializer inputs' last write, as `held`
            // keeps a connect target. probe:
            // design/review-2026-10-26/repro/x-engine-firing-init-catchup-beats-later-write.gx
            // (x-engine-firing-05)
            self.reraise_writes_while_asleep(ctx);
        }
        let tv = self.node.update(ctx);
        let tag = tv.tag();
        // At a wake a `<-` target holding a value keeps it unless the
        // initializer fired; otherwise a production publishes when it
        // fires, first, or quietly after a sleep.
        let mut held: SmallVec<[BindId; 4]> = SmallVec::new();
        // CR claude for eric: [bug/medium] Two events missed during a sleep are applied
        // in the wrong order. The initializer input fired at c=4 and a write to this
        // target landed at c=5. At the wake the re-raised write is published first,
        // then the initializer's caught-up fire overwrites it, so x is 100 where an arm
        // that never slept stands at 555. Which should win: the later write (hold the
        // target when its write stamp is after every initializer input's), or always
        // the initializer? probe: design/review-2026-10-26/repro/c-bind-wake-order.gx
        // (c-bind-06)
        if (woke || ctx.event.wake()) && !tag.triggers() {
            self.pattern.ids(&mut |id| {
                let holds =
                    || ctx.rt.store_get(&id).is_some_and(|(tv, _)| !tv.tag().is_bottom());
                if ctx.connect_targets.contains(&id) && (tag.is_bottom() || holds()) {
                    held.push(id)
                }
            });
        }
        let publish = tag.triggers()
            || (!self.ever_published && !tag.is_bottom())
            || (woke && !tag.triggers());
        if dbgenv::gxdbg_letbind() {
            eprintln!(
                "LETBIND {} tag={tag:?} val={:?} ever_published={} held={held:?} publishing={publish}",
                self.spec.pos,
                tv.value_cloned(),
                self.ever_published,
            );
        }
        if publish {
            self.reraised.retain(|id| held.contains(id));
            let quiet = !tag.triggers();
            if tag.is_bottom() {
                self.pattern.ids(&mut |id| {
                    if held.contains(&id) {
                        return;
                    }
                    ctx.event.variables.insert(id, TagValue::tagged(Value::Null, tag));
                    let stored = TagValue::tagged(Value::Null, Tag::FRESH_BOTTOM);
                    if quiet {
                        ctx.rt.store_insert_standing(id, stored)
                    } else {
                        ctx.rt.store_insert(id, stored);
                        ctx.rt.notify_set(id);
                    }
                });
            } else {
                let v = tv.value_cloned();
                self.pattern.bind(&v, &mut |id, v| {
                    if held.contains(&id) {
                        return;
                    }
                    ctx.event.variables.insert(id, TagValue::tagged(v.clone(), tag));
                    if quiet {
                        ctx.rt.store_insert_standing(id, TagValue::fired(v))
                    } else {
                        ctx.rt.store_insert(id, TagValue::fired(v));
                        ctx.rt.notify_set(id);
                    }
                });
                self.ever_published = true;
            }
        }
        TagValue::phantom_ref()
    }

    fn refs(&self, refs: &mut Refs) {
        self.pattern.ids(&mut |id| {
            refs.bound.insert(id);
        });
        self.node.refs(refs);
    }

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        // The static-resolution index outlives batches; a deleted
        // bind's entry must go with it.
        self.pattern.ids(&mut |id| {
            ctx.bind_to_lambda.remove(&id);
            ctx.static_callees.remove(&id);
            ctx.connect_targets.remove(&id);
            ctx.batch_connect_targets.remove(&id);
        });
        if let Some(l) = lambda_value(&self.node)
            && let Some(lambda) = l.lambda_id::<R, E>()
            && let Some(id) = self.pattern.single_bind_id()
            && let Some(b) = ctx.env.by_id.get(&id)
        {
            let key = (b.scope.clone(), b.name.clone());
            if ctx.builtin_bindings.get(&key).is_some_and(|i| i.lambda_id == Some(lambda))
            {
                ctx.builtin_bindings.remove(&key);
            }
        }
        self.node.delete(ctx);
        self.pattern.delete(ctx);
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        if !self.slept.is_set() {
            let cycle = ctx.rt.cycle();
            let walked = ctx.boundary.as_ref().is_some_and(|b| b.visited(cycle));
            self.slept_at = if walked { cycle } else { cycle.saturating_sub(1) };
        }
        self.slept.set();
        self.node.sleep(ctx);
    }

    fn typ(&self) -> &Type {
        &self.typ
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn typecheck0(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
        if let ExprKind::Bind(b) = &self.spec.kind
            && b.typ.is_some()
        {
            crate::defer_unresolved_names(ctx, &self.typ, &self.spec);
            // an abstract type's parameters are invariant: the annotation,
            // not the payload, decides a constructed value's
            // CR claude for eric: [bug/medium] With invariant abstract parameters,
            // `Box(1)` is typed `Box<i64>` eagerly. A wider annotation can only decide
            // the parameter here (a let whose initializer is itself the construction)
            // and at call arguments (`pre_unify_arg`, which does not look inside an
            // array argument). The following are all refused with "Box<[i64, string]>
            // does not contain Box<i64>": a lambda return `-> Box<[i64, string]>
            // Box(1)`, `let b: (Box<[i64, string]>, i64) = (Box(1), 2)`, the same in a
            // struct or array literal, select arms building `Box(1)` and `Box("s")`,
            // and `b <- Box(1)` into a `Box<[i64, string]>` variable. For the `<-` case
            // write_mismatch also advises declaring the union `[Box<[i64, string]>,
            // Box<i64>]`. That contradicts design/nominal_abstract_types.md rule 4 ("a
            // let annotation decides ... as a formal decides an argument's"), and every
            // case passes once it goes through a `let r: Box<[i64, string]> = Box(1)`
            // first. The fix is a typing-rule choice. One option: give `T(v)` a fresh
            // parameter cell that the payload only bounds from below, so context
            // decides it wherever the value flows. The other: apply the pre-unify to
            // every annotated position (returns, nested literals, select arms,
            // connects). Either way, write_mismatch should not suggest a union of two
            // instantiations of one abstract. probe:
            // design/review-2026-10-12/repro/x-fix-types-07.sh (x-fix-types-07)
            // 2026-10-24 claude: Eric ruled (2026-10-24): fresh cell: T(v) gets a fresh
            // parameter cell the payload bounds from below, so the context decides it.
            // 2026-10-25 claude: Tried the ruling as a deferred judgment: T(v) left its
            // parameter cells open and a PendingSettle::LowerBound, judged after the
            // frame's sites and before its cells settle (and at a definition gate's
            // close, before the signature generalizes), bound what nothing decided to
            // the payload's type and checked the rest held it. All nine repro cases
            // then checked, a misfit was still refused, and |x| Box(x) stayed generic.
            // But an annotation that is a union of two instantiations broke: `let x:
            // [Box<i64>, Box<string>] = Box("hi")` checks the union against Box<'a>
            // while 'a is open, takes the first member (Box<i64>), and the payload then
            // fails (lang::traits::abstract_test_matches_parameters and four other
            // trait and select fixtures). A deferred check cannot steer that choice.
            // What the ruling needs is a lower bound on the cell itself: a TCell field
            // beside its constraints that every bind checks (the binding must contain
            // the payload), so the union check skips Box<i64>; the settle binds a cell
            // still open to it. That is a change to the cell core (copies, merges,
            // aliases and images carry it), so it is reverted and back to eric to
            // confirm before building it.
            if let Some(Type::Abstract { .. }) = self.node.typ().deref_cloned() {
                wrap!(
                    self.node,
                    Type::pre_unify_arg(&ctx.env, &self.typ, self.node.typ())
                )?;
            }
        }
        wrap!(self.node, self.node.typecheck0(ctx))?;
        self.check_value(ctx)?;
        self.complete_deferred(ctx)?;
        self.publish(ctx);
        Ok(())
    }

    fn typecheck0_instance(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        types: &mut super::lambda::InstanceTypes,
    ) -> Result<()> {
        wrap!(self.node, self.node.typecheck0_instance(ctx, types))?;
        if !types.settle(self.spec.id, &self.typ) {
            self.check_value(ctx)?;
        }
        self.complete_deferred(ctx)?;
        self.publish(ctx);
        Ok(())
    }

    fn view(&self) -> NodeView<'_, R, E> {
        NodeView::Bind(self)
    }

    fn fuse(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<Option<Node<R, E>>> {
        // The Bind stays live to publish the fused value to its BindId.
        fuse(&mut self.node, ctx)?;
        Ok(None)
    }
}

#[derive(Debug)]
pub struct Ref {
    pub(crate) spec: Arc<Expr>,
    pub typ: Type,
    pub id: BindId,
    pub(super) top_id: ExprId,
    pub(crate) resident: TagValue,
    pub(crate) signature: Signature,
}

/// Whose cells a reference's signature holds. The first `typecheck0`
/// decides, once: fresh cells, or the definition's own (a rec knot, a
/// gate parameter, the instance being elaborated).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Signature {
    /// The binding's, undecided.
    Binding,
    /// A generalized binding's reference holds a fresh cell until
    /// `typecheck0` binds it to the decided signature, so nothing that
    /// reaches it first unifies into the definition's cells.
    Pending,
    Decided,
}

/// A fresh instance of a generalized signature under the open gates.
fn instance(ft: &FnType, open: &nohash::IntSet<crate::LambdaId>) -> FnType {
    let fresh = ft.scheme(open);
    fresh.alias_tvars(&mut LPooled::take());
    fresh
}

/// `let g = f` with `f` generalized and no annotation: `g` shares `f`'s
/// scheme; unifying with a fresh instance would pin it.
fn forwards<R: Rt, E: UserEvent>(
    env: &Env,
    b: &expr::BindExpr,
    node: &Node<R, E>,
) -> bool {
    b.typ.is_none()
        && match ungrouped(node) {
            NodeView::Ref(r) => env.poly_binds.contains(&r.id),
            _ => false,
        }
}

/// The `BindId` a `#bind::N` path names, if `name` is one.
fn synthesized_bind_ref(name: &ModPath) -> Option<BindId> {
    let mut parts = netidx_core::path::Path::parts(&**name);
    match (parts.next(), parts.next(), parts.next()) {
        (Some("#bind"), Some(n), None) => n.parse().ok().map(BindId::from_inner),
        _ => None,
    }
}

impl Ref {
    /// A reference to `id`, registered with the runtime; `delete`
    /// unregisters it.
    pub(crate) fn new<R: Rt, E: UserEvent>(
        ctx: &mut CompileCtx<R, E>,
        id: BindId,
        typ: Type,
        top_id: ExprId,
        spec: impl Into<Arc<Expr>>,
    ) -> Node<R, E> {
        Self::with_signature(ctx, id, typ, Signature::Binding, top_id, spec)
    }

    fn with_signature<R: Rt, E: UserEvent>(
        ctx: &mut CompileCtx<R, E>,
        id: BindId,
        typ: Type,
        signature: Signature,
        top_id: ExprId,
        spec: impl Into<Arc<Expr>>,
    ) -> Node<R, E> {
        ctx.record_ref(id, top_id);
        Node::new(Self {
            spec: spec.into(),
            typ,
            id,
            top_id,
            resident: TagValue::phantom(),
            signature,
        })
    }

    /// A reference to `name`, as a call's own function names it.
    pub(crate) fn compile<R: Rt, E: UserEvent>(
        ctx: &mut CompileCtx<R, E>,
        spec: Arc<Expr>,
        scope: &Scope,
        top_id: ExprId,
        name: &ModPath,
    ) -> Result<Node<R, E>> {
        Self::compile_as(ctx, None, spec, scope, top_id, name)
    }

    /// A value occurrence of `name`: a trait method named is its
    /// eta-expansion, compiled under `flags`.
    pub(crate) fn compile_value<R: Rt, E: UserEvent>(
        ctx: &mut CompileCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: Arc<Expr>,
        scope: &Scope,
        top_id: ExprId,
        name: &ModPath,
    ) -> Result<Node<R, E>> {
        Self::compile_as(ctx, Some(flags), spec, scope, top_id, name)
    }

    fn compile_as<R: Rt, E: UserEvent>(
        ctx: &mut CompileCtx<R, E>,
        value: Option<BitFlags<CFlag>>,
        spec: Arc<Expr>,
        scope: &Scope,
        top_id: ExprId,
        name: &ModPath,
    ) -> Result<Node<R, E>> {
        // `#bind::N` is the compiler's spelling for a synthesized
        // reference; no source can write it.
        if let Some(id) = synthesized_bind_ref(name) {
            let Some(bind) = ctx.env.by_id.get(&id) else {
                bailat!(spec, "synthesized reference to an unknown binding {id:?}")
            };
            let typ = bind.typ.clone();
            return Ok(Self::new(ctx, id, typ, top_id, spec));
        }
        let resolved = match ctx.env.lookup_bind(&scope.lexical, name) {
            Ok(r) => r,
            Err(e) => {
                return Err(e.at(&spec));
            }
        };
        match resolved {
            None => bailat!(spec, "{name} not defined"),
            Some((_, bind)) => {
                if ctx
                    .env
                    .trait_methods
                    .get(&bind.id)
                    .is_some_and(|tm| tm.trait_id == CoreTrait::Cast.id())
                {
                    bailat!(
                        spec,
                        "{name} is called by casts and writers, never by a program"
                    )
                }
                if let Some(flags) = value
                    && let Some(eta) = super::compiler::lowering(ctx, &spec, |ctx| {
                        super::compiler::eta_dispatcher(ctx, &spec, name, bind)
                    })?
                {
                    return compile(ctx, flags, eta, scope, top_id);
                }
                let bind_id = bind.id;
                let (typ, signature) = match ctx.env.poly_binds.contains(&bind_id) {
                    true => (Type::empty_tvar(), Signature::Pending),
                    false => (bind.typ.clone(), Signature::Binding),
                };
                if ctx.env.ide.is_lsp() {
                    ctx.env.push_reference(ReferenceSite {
                        pos: spec.pos,
                        ori: spec.ori.clone(),
                        scope: scope.lexical.clone(),
                        name: name.clone(),
                        bind_id,
                    });
                }
                Ok(Self::with_signature(ctx, bind_id, typ, signature, top_id, spec))
            }
        }
    }
}

/// The refusal of a `let` whose pattern `ptyp` does not hold its value
/// type `typ`.
fn misfit<R: Rt, E: UserEvent>(
    ctx: &CompileCtx<R, E>,
    spec: &Expr,
    ptyp: &Type,
    typ: &Type,
) -> Result<()> {
    let part = ptyp.could_match(&ctx.env, typ)?;
    format_with_flags(PrintFlag::DerefTVars, || match part {
        true => bailat!(
            spec,
            "refutable patterns are not allowed in let: {ptyp} matches only part of {typ}"
        ),
        false => bailat!(spec, "match error {typ} can't be matched by {ptyp}"),
    })
}

/// A let's pattern predicate `ptyp` holds every member of the value type
/// `typ`, each on its own; over several members `narrowed` takes the
/// predicate with each cell the union of its parts, as a select arm's is:
/// `binds` infers it afresh, at the binds' level (`ptyp`'s cells are the
/// shape check's). A bare name takes the value's type.
fn fits_every_member<R: Rt, E: UserEvent>(
    ctx: &CompileCtx<R, E>,
    ptyp: &Type,
    typ: &Type,
    narrowed: &mut Option<Type>,
    binds: impl FnOnce() -> Result<Type>,
) -> Result<bool> {
    let mut members: SmallVec<[Type; 8]> = SmallVec::new();
    expr::union_members(&ctx.env, typ, &mut members)?;
    // XCR claude for claude: [bug/medium] At compile, a value whose type is still an
    // open cell (a select's, a call's result) is one 'member' here, and
    // `ptyp.contains(typ)` binds that cell to the pattern's shape before the value is
    // checked. So `let t = select c { 0 => (1, "x"), _ => ("y", 2) }; let (a, b) = t`
    // is refused with '(i64, string) does not contain [..]' at the select (or at the
    // callee for `let (a, b) = f(1)`). The same pattern in a select arm accepts it.
    // Fix: when the value's type is an unbound cell, defer this check to typecheck0
    // after the value's check, as complete_partial is deferred. probe:
    // design/review-2026-10-26/repro/c-bind-let-shape-binds-value.gx (c-bind-05)
    // 2026-10-26 claude: Bind::compile defers the fit of a destructuring pattern over
    // a value typed by an open cell (Deferred::Shape): the pattern compiles against
    // its own predicate with each name a fresh cell, and typecheck0 (complete_shape)
    // holds the predicate to every member of the checked value and types each name by
    // names_by_member. Pins:
    // lang::tuples_structs::let_destructures_a_union_typed_by_the_check,
    // let_over_a_later_typed_union_refuses_a_member_it_misses.
    if members.len() < 2 || matches!(ptyp, Type::TVar(_)) {
        return ptyp.any_as_tvar().contains(&ctx.env, typ);
    }
    for m in members.iter() {
        if !ptyp.any_as_tvar().reset_tvars().contains(&ctx.env, m)? {
            // the whole check, which fails too, binds the cells the refusal
            // names
            return ptyp.any_as_tvar().contains(&ctx.env, typ);
        }
    }
    let pred = binds()?.any_as_tvar();
    pred.narrow_per_member(&ctx.env, &members)?;
    *narrowed = Some(pred);
    Ok(true)
}

impl Ref {
    /// The signature a reference to the generalized `ft` holds: the
    /// definition's own cells in a rec knot or a gate parameter, the
    /// innermost active instance (a bare value reference has no
    /// arguments to key an instance identity on), else fresh cells.
    fn decide<R: Rt, E: UserEvent>(
        &self,
        ctx: &CompileCtx<R, E>,
        ft: Arc<FnType>,
    ) -> Arc<FnType> {
        let rec_knot = !ctx.rec_defs.is_empty()
            && ft.lambda_ids.ids().iter().any(|id| ctx.rec_defs.contains(id));
        if rec_knot || ctx.def_gate_params.contains_key(&self.id) {
            return ft;
        }
        match ft.lambda_ids.own().and_then(|id| ctx.resolving_innermost(id)) {
            Some(active) => Arc::new(active.ftype),
            None => Arc::new(instance(&ft, &ctx.rec_defs)),
        }
    }

    /// Replays the reference registration `compile` made with the runtime.
    pub(crate) fn image_decode<R: Rt, E: UserEvent>(
        ctx: &mut ExecCtx<'_, R, E>,
        buf: &mut &[u8],
    ) -> Result<Node<R, E>, PackError> {
        let spec = Arc::new(Expr::decode(buf)?);
        let typ = Type::decode(buf)?;
        let id = BindId::decode(buf)?;
        let top_id = ExprId::decode(buf)?;
        Ok(Self::new(ctx, id, typ, top_id, spec))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Ref {
    /// An instance checks this node as its definition's check did.
    fn typecheck0_instance(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        _types: &mut crate::node::lambda::InstanceTypes,
    ) -> Result<()> {
        self.typecheck0(ctx)
    }

    fn for_each_child<'a>(&'a self, f: &mut dyn FnMut(&'a Node<R, E>)) {
        let _ = f;
    }

    fn for_each_child_mut(&mut self, f: &mut dyn FnMut(&mut Node<R, E>)) {
        let _ = f;
    }

    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::Ref, buf);
        self.spec.encode(buf)?;
        self.typ.encode(buf)?;
        self.id.encode(buf)?;
        self.top_id.encode(buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R, E>) -> &TagValue {
        // Overlays first, then the store; a store miss rides the resident.
        let dbg = dbgenv::gxdbg_ref();
        let r = match read_var(ctx, &self.id) {
            Some(VarRead::Delivered(tv)) => {
                if dbg {
                    eprintln!(
                        "REF {} @{} {:?} DELIVERED tag={:?}",
                        self.spec,
                        self.spec.pos,
                        self.id,
                        tv.tag()
                    );
                }
                self.resident.set(tv.clone())
            }
            Some(VarRead::Standing(tv)) => {
                let tv = standing_view(ctx.event, tv);
                if dbg {
                    eprintln!(
                        "REF {} @{} {:?} STANDING (ei={} wi={}) tag={:?} val={:?}",
                        self.spec,
                        self.spec.pos,
                        self.id,
                        ctx.event.init(),
                        ctx.event.wake(),
                        tv.tag(),
                        tv.value_cloned()
                    );
                }
                self.resident.set(tv)
            }
            None => {
                if dbg {
                    eprintln!(
                        "REF {} {:?} MISS ride tag={:?}",
                        self.spec,
                        self.id,
                        self.resident.tag()
                    );
                }
                self.resident.ride()
            }
        };
        r
    }

    fn refs(&self, refs: &mut Refs) {
        refs.read(self.id);
    }

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        ctx.unref_var(self.id, self.top_id)
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn typ(&self) -> &Type {
        &self.typ
    }

    /// A value occurrence of a generalized binding (`Env::poly_binds`)
    /// mints fresh cells for its signature, like a call. Exempt: a
    /// self-reference inside the definition's gate, a fn-typed
    /// parameter during its gate, and a reference to the instance
    /// being elaborated, which must share the definition's cells.
    fn typecheck0(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
        let poly = ctx.env.poly_binds.contains(&self.id);
        let def = match self.signature {
            Signature::Decided => return Ok(()),
            Signature::Binding if !poly => return Ok(()),
            Signature::Binding => self.typ.clone(),
            Signature::Pending => match ctx.env.by_id.get(&self.id) {
                None => bail!("BUG missing bind {:?}", self.id),
                Some(b) => b.typ.clone(),
            },
        };
        let ft = def.with_deref(|t| match t {
            Some(Type::Fn(ft)) if poly => Some(ft.clone()),
            _ => None,
        });
        let signature = match (ft, self.signature) {
            (Some(ft), _) => Type::Fn(self.decide(ctx, ft)),
            (None, Signature::Pending) => def,
            (None, _) => return Ok(()),
        };
        let held = mem::replace(&mut self.typ, signature);
        if mem::replace(&mut self.signature, Signature::Decided) == Signature::Pending {
            held.check_contains(&ctx.env, &self.typ).map_err(|e| e.at(&*self.spec))?;
        }
        Ok(())
    }

    fn view(&self) -> NodeView<'_, R, E> {
        NodeView::Ref(self)
    }

    fn emit_clif(&self, cx: &mut BodyCx) -> Result<CompiledExpr> {
        emit_ref_node(cx, self.spec.as_ref(), self.id, &self.typ)
    }
}

/// One accessor of a place reference's path, with its key expression
/// where the key is dynamic.
#[derive(Debug)]
enum PlaceStep<R: Rt, E: UserEvent> {
    Index(Node<R, E>),
    Tuple(usize),
    Field(expr::Name),
    Key(Node<R, E>),
}

/// The place a `&root[i].f` reference stands for: the root node (a
/// variable reference or a dereference) and the path's steps
/// (design/place_references.md), typed from `scope`. `path` is the path
/// as last resolved.
#[derive(Debug)]
struct Place<R: Rt, E: UserEvent> {
    root: Node<R, E>,
    steps: Vec<PlaceStep<R, E>>,
    scope: ModPath,
    path: Path,
}

impl<R: Rt, E: UserEvent> PlaceStep<R, E> {
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        match self {
            PlaceStep::Index(n) => {
                buf.put_u8(0);
                n.image_encode(buf)
            }
            PlaceStep::Tuple(i) => {
                buf.put_u8(1);
                i.encode(buf)
            }
            PlaceStep::Field(f) => {
                buf.put_u8(2);
                f.encode(buf)
            }
            PlaceStep::Key(n) => {
                buf.put_u8(3);
                n.image_encode(buf)
            }
        }
    }

    fn image_decode(
        ctx: &mut ExecCtx<'_, R, E>,
        buf: &mut &[u8],
    ) -> Result<Self, PackError> {
        if !buf.has_remaining() {
            return Err(PackError::BufferShort);
        }
        match buf.get_u8() {
            0 => Ok(PlaceStep::Index(decode_node(ctx, buf)?)),
            1 => Ok(PlaceStep::Tuple(usize::decode(buf)?)),
            2 => Ok(PlaceStep::Field(expr::Name::decode(buf)?)),
            3 => Ok(PlaceStep::Key(decode_node(ctx, buf)?)),
            _ => Err(PackError::UnknownTag),
        }
    }
}

enum PlaceSpec {
    Index(Expr),
    Tuple(usize),
    Field(expr::Name),
    Key(Expr),
}

/// The lambda `node` is, seen through grouping parentheses.
fn lambda_value<R: Rt, E: UserEvent>(
    node: &Node<R, E>,
) -> Option<&super::lambda::Lambda> {
    match ungrouped(node) {
        NodeView::Lambda(l) => Some(l),
        _ => None,
    }
}

/// What `node` is under its grouping parentheses.
fn ungrouped<R: Rt, E: UserEvent>(node: &Node<R, E>) -> NodeView<'_, R, E> {
    let mut view = node.view();
    while let NodeView::ExplicitParens(p) = view {
        view = p.n.view();
    }
    view
}

/// Where a place resolved to this cycle.
struct Address<'a> {
    /// The binding at the bottom of the reference chain.
    bind: BindId,
    /// The path from `bind`'s value.
    path: &'a Path,
    /// The tail of `path` the place's own steps resolved: the path
    /// from the root node's production.
    steps: &'a [place::Step],
}

/// What a place resolved to this cycle.
struct Resolved<'a> {
    /// `None` while the root's address or a key is undetermined.
    address: Option<Address<'a>>,
    /// The root's production.
    root: TagValue,
    /// Did the root or a key fire?
    moved: bool,
}

/// The binding a place's root stands for and the path already under
/// it (a dereferenced place reference composes); `None` while a
/// dereferenced reference is bottom.
fn root_place<R: Rt, E: UserEvent>(
    root: &Node<R, E>,
) -> Option<(BindId, &[place::Step])> {
    let any = &**root as &dyn Any;
    match any.downcast_ref::<Ref>() {
        Some(r) => Some((r.id, &[])),
        None => {
            let a = any.downcast_ref::<Deref<R, E>>()?.addr.as_ref()?;
            Some((a.id, &a.path))
        }
    }
}

impl<R: Rt, E: UserEvent> Place<R, E> {
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        self.root.image_encode(buf)?;
        encode_varint(self.steps.len() as u64, buf);
        for s in &self.steps {
            s.image_encode(buf)?;
        }
        self.scope.encode(buf)
    }

    fn image_decode(
        ctx: &mut ExecCtx<'_, R, E>,
        buf: &mut &[u8],
    ) -> Result<Self, PackError> {
        let root = decode_node(ctx, buf)?;
        let n = crate::image::count_decode(buf)?;
        let mut steps = Vec::with_capacity(n);
        for _ in 0..n {
            steps.push(PlaceStep::image_decode(ctx, buf)?);
        }
        let scope = ModPath::decode(buf)?;
        Ok(Place { root, steps, scope, path: Path::new() })
    }

    /// The accessor chain of `expr` down to a variable or a
    /// dereference, root first, a bare dereference (`*r`) included;
    /// `None` for anything else, a bare variable included.
    fn of(expr: &Expr) -> Option<(Expr, Vec<PlaceSpec>)> {
        let (root, steps) = expr.place()?;
        if steps.is_empty() && !matches!(root.unparen().kind, ExprKind::Deref(_)) {
            return None;
        }
        let specs = steps
            .into_iter()
            .map(|s| match s {
                PlaceAccess::Index(i) => PlaceSpec::Index(i.clone()),
                PlaceAccess::Tuple(f) => PlaceSpec::Tuple(f),
                PlaceAccess::Field(f) => PlaceSpec::Field(f.clone()),
                PlaceAccess::Key(k) => PlaceSpec::Key(k.clone()),
            })
            .collect();
        Some((root.clone(), specs))
    }

    fn compile(
        ctx: &mut CompileCtx<R, E>,
        flags: BitFlags<CFlag>,
        scope: &Scope,
        top_id: ExprId,
        root: Expr,
        specs: Vec<PlaceSpec>,
    ) -> Result<Self> {
        let root = compile(ctx, flags, root, scope, top_id)?;
        let steps = specs
            .into_iter()
            .map(|s| {
                Ok(match s {
                    PlaceSpec::Index(e) => {
                        PlaceStep::Index(compile(ctx, flags, e, scope, top_id)?)
                    }
                    PlaceSpec::Tuple(i) => PlaceStep::Tuple(i),
                    PlaceSpec::Field(f) => PlaceStep::Field(f),
                    PlaceSpec::Key(e) => {
                        PlaceStep::Key(compile(ctx, flags, e, scope, top_id)?)
                    }
                })
            })
            .collect::<Result<Vec<_>>>()?;
        Ok(Place { root, steps, scope: scope.lexical.clone(), path: Path::new() })
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R, E>) -> Resolved<'_> {
        let root = self.root.update(ctx).clone();
        let mut moved = root.tag().triggers();
        self.path.clear();
        let id = root_place(&self.root).map(|(id, under)| {
            self.path.extend(under.iter().cloned());
            id
        });
        let under = self.path.len();
        let mut complete = true;
        for step in &mut self.steps {
            match step {
                PlaceStep::Index(n) => {
                    let tv = n.update(ctx);
                    moved |= tv.tag().triggers();
                    let i = if tv.tag().is_bottom() {
                        None
                    } else {
                        tv.with_value(super::array::index_i64)
                    };
                    match i {
                        Some(i) => self.path.push(place::Step::Index(i)),
                        None => complete = false,
                    }
                }
                PlaceStep::Tuple(i) => self.path.push(place::Step::Index(*i as i64)),
                PlaceStep::Field(name) => {
                    self.path.push(place::Step::Field(name.name.clone()))
                }
                PlaceStep::Key(n) => {
                    let tv = n.update(ctx);
                    moved |= tv.tag().triggers();
                    if tv.tag().is_bottom() {
                        complete = false
                    } else {
                        self.path.push(place::Step::Key(tv.value_cloned()))
                    }
                }
            }
        }
        let address = match (complete, id) {
            (true, Some(bind)) => {
                Some(Address { bind, path: &self.path, steps: &self.path[under..] })
            }
            _ => None,
        };
        Resolved { address, root, moved }
    }

    fn each(&mut self, f: &mut dyn FnMut(&mut Node<R, E>)) {
        f(&mut self.root);
        for step in &mut self.steps {
            match step {
                PlaceStep::Index(n) | PlaceStep::Key(n) => f(n),
                PlaceStep::Tuple(_) | PlaceStep::Field(_) => (),
            }
        }
    }

    fn each_ref<'a>(&'a self, f: &mut dyn FnMut(&'a Node<R, E>)) {
        f(&self.root);
        for step in &self.steps {
            match step {
                PlaceStep::Index(n) | PlaceStep::Key(n) => f(n),
                PlaceStep::Tuple(_) | PlaceStep::Field(_) => (),
            }
        }
    }

    /// The element type: each step's accessor rule over the root's type,
    /// without the access's failure (a place handles that at runtime). A
    /// writable field step over a union needs every member's field to be
    /// one type, so a write keeps the root in its own member.
    fn elem_type(
        &self,
        ctx: &mut CompileCtx<R, E>,
        spec: &Expr,
        writable: bool,
    ) -> Result<Type> {
        let one = |cur: &Type, ts: LPooled<Vec<Type>>| -> Result<Type> {
            match ts.iter().skip(1).find(|t| **t != ts[0]) {
                Some(other) if writable => {
                    format_with_flags(PrintFlag::DerefTVars, || {
                        bail!(
                            "a writable reference to a field of {cur} would write {} into a \
                         member whose field is {other}: the members' fields differ",
                            ts[0]
                        )
                    })
                }
                _ => {
                    let ts: LPooled<Vec<&Type>> = ts.iter().collect();
                    Type::union(&ctx.env, &ts)
                }
            }
        };
        let mut cur = self.root.typ().clone();
        for step in &self.steps {
            cur = match step {
                PlaceStep::Index(i) => {
                    super::array::check_index(&ctx.env, i)?;
                    let et = Type::empty_tvar();
                    Type::Array(Arc::new(et.clone())).check_contains(&ctx.env, &cur)?;
                    et
                }
                PlaceStep::Key(k) => {
                    let (kt, vt) = (Type::empty_tvar(), Type::empty_tvar());
                    let mt = Type::Map {
                        key: Arc::new(kt.clone()),
                        value: Arc::new(vt.clone()),
                    };
                    mt.check_contains(&ctx.env, &cur)?;
                    kt.check_contains(&ctx.env, k.typ())?;
                    vt
                }
                PlaceStep::Tuple(i) => {
                    one(&cur, tuple_field_types(ctx, &self.scope, &cur, *i)?)?
                }
                PlaceStep::Field(name) => {
                    let (_, ts) = struct_field_types(ctx, &cur, &name.name)?;
                    let t = one(&cur, ts)?;
                    if ctx.env.ide.is_lsp() {
                        ctx.env.push_field_ref(FieldRefSite {
                            pos: name.pos_or(spec.pos),
                            ori: spec.ori.clone(),
                            name: name.name.clone(),
                            typ: t.clone(),
                        });
                    }
                    t
                }
            };
        }
        Ok(cur)
    }
}

/// What a reference stands for: a binding's value channel (`&x`, or a
/// derived channel for any other expression), or a place.
#[derive(Debug)]
enum Referent<R: Rt, E: UserEvent> {
    Channel(Node<R, E>),
    Place(Place<R, E>),
}

impl<R: Rt, E: UserEvent> Referent<R, E> {
    fn each(&mut self, f: &mut dyn FnMut(&mut Node<R, E>)) {
        match self {
            Referent::Channel(n) => f(n),
            Referent::Place(p) => p.each(f),
        }
    }

    fn each_ref<'a>(&'a self, f: &mut dyn FnMut(&'a Node<R, E>)) {
        match self {
            Referent::Channel(n) => f(n),
            Referent::Place(p) => p.each_ref(f),
        }
    }
}

#[derive(Debug)]
pub struct ByRef<R: Rt, E: UserEvent> {
    pub(crate) spec: Arc<Expr>,
    pub typ: Type,
    referent: Referent<R, E>,
    pub id: BindId,
    resident: TagValue,
    /// What the runtime holds the cell stands for, when it holds anything.
    registered: Option<RefPath>,
    /// Shared by every reference value this site published: the last to
    /// drop releases the cell, its place and what it names.
    keep: Option<triomphe::Arc<crate::reference::Keep>>,
}

impl<R: Rt, E: UserEvent> ByRef<R, E> {
    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<'_, R, E>,
        buf: &mut &[u8],
    ) -> Result<Node<R, E>, PackError> {
        let spec = Arc::new(Expr::decode(buf)?);
        let typ = Type::decode(buf)?;
        let id = BindId::decode(buf)?;
        let referent = match bool::decode(buf)? {
            false => Referent::Channel(decode_node(ctx, buf)?),
            true => Referent::Place(Place::image_decode(ctx, buf)?),
        };
        Ok(Node::new(Self {
            spec,
            typ,
            referent,
            id,
            resident: TagValue::phantom(),
            registered: None,
            keep: None,
        }))
    }

    pub(crate) fn compile(
        ctx: &mut CompileCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: Arc<Expr>,
        scope: &Scope,
        top_id: ExprId,
        mutability: Mutability,
        expr: &Expr,
    ) -> Result<Node<R, E>> {
        let id = BindId::new();
        // A place reference types as a reference to the element and
        // still mints a cell so embedders keep reading the mirror.
        let (referent, typ) = match Place::<R, E>::of(expr) {
            Some((root, specs)) => {
                let place = Place::compile(ctx, flags, scope, top_id, root, specs)?;
                (
                    Referent::Place(place),
                    Type::ByRef(mutability, Arc::new(Type::empty_tvar())),
                )
            }
            None => {
                let child = compile(ctx, flags, expr.unparen().clone(), scope, top_id)?;
                let named = match (&*child as &dyn Any).downcast_ref::<Ref>() {
                    Some(c) => {
                        ctx.env.byref_chain.insert(id, c.id);
                        true
                    }
                    None => false,
                };
                let referent = match mutability {
                    Mutability::Mut if !named => Type::empty_tvar(),
                    _ => child.typ().clone(),
                };
                // a reference to an unnamed expression is its own cell: its
                // binding types an embedder's write through it
                if !named {
                    let cell = env::Bind {
                        id,
                        typ: referent.clone(),
                        doc: None,
                        scope: scope.lexical.clone(),
                        name: compact_str::format_compact!("{spec}"),
                        pos: spec.pos,
                        ori: spec.ori.clone(),
                        binder: None,
                    };
                    ctx.env.by_id.insert(id, Arc::new(cell));
                }
                (Referent::Channel(child), Type::ByRef(mutability, Arc::new(referent)))
            }
        };
        // a binding a writable reference names is written as a `<-` target
        // is: a wake keeps its last write, and a call through it dispatches
        if let Mutability::Mut = mutability {
            let named = match &referent {
                Referent::Place(p) => (&*p.root as &dyn Any).downcast_ref::<Ref>(),
                Referent::Channel(c) => (&**c as &dyn Any).downcast_ref::<Ref>(),
            };
            if let Some(r) = named {
                // XCR claude for claude: [bug/medium] In a single script, `&mut f`
                // inside a function body is refused here as "made generic by an earlier
                // compile" when f holds a function. generalized_earlier judges by the
                // levels of the cells f's type reaches, and inside a body those read as
                // generic. The binding is not generalized: written_lets found the
                // write, and mark_connect_target removed f from env.poly_binds. The
                // same write as `f <- ..` is accepted, because Connect::typecheck0_with
                // asks env.poly_binds. A `let r = &mut f` outside the function is
                // accepted too. Fix: refuse on `ctx.env.poly_binds.contains(&r.id)`
                // instead, and pin the lambda-body case. probe:
                // design/review-2026-10-26/repro/c-node-mod-mut-ref-in-lambda.gx
                // (c-node-mod-02)
                // 2026-10-26 claude: The probe no longer reproduced at the tip, nor
                // did the run-time bind case once with_runtime_settles entered
                // Level::TOP (c-bind-01). ByRef::compile still judged a writable
                // reference by the levels of the cells the binding's type reaches;
                // it now refuses only a binding env.poly_binds still holds, the fact
                // Connect::typecheck0_with asks (mark_connect_target keeps there
                // only what an earlier compile generalized), so `&mut f` and `f <-`
                // decide alike. Pins: lang::types::mut_ref_in_a_lambda_to_a_written_function
                // (the probe's case), written_after_generalized_is_held_to_it (the
                // REPL refusal); no pin at the tip fails with the old test. With it,
                // c-bind-01's pin no longer fails with that fix's AtLevel removed:
                // the refusal does not read levels at all.
                // XCR claude for claude: [bug/high] `&mut y` over an inferred let is
                // refused at every run-time bind. In a collection slot or a dynamic
                // call, `{ let y = i + 1; let r = &mut y; .. }` fails with 'binding the
                // callee failed: y was made generic by an earlier compile' although the
                // check accepted it. Cause: generalized_earlier counts BOUND cells at
                // Level::Generic, and that is the level of the cells a run-time bind
                // creates for an operator, a call result or an array literal. `<-`
                // never asks this, so it works. Fix: count only unbound cells in
                // generalized_earlier (reached_cells already walks through a binding),
                // and pin `&mut` over an inferred let inside an array::map callback.
                // probe: design/review-2026-10-26/repro/c-bind-mut-ref-runtime-bind.gx
                // (c-bind-01)
                // 2026-10-26 claude: The cell &mut y saw was UNBOUND, not bound: ByRef
                // compiles before the instance's typecheck substitutes its types, and a
                // run-time bind compiled at the thread's default level, Generic, so
                // every fresh cell of the instance looked generalized.
                // with_runtime_settles (node/mod.rs, the one entry of build_bound,
                // prebind and the core-trait hook sites) now enters Level::TOP, as
                // compile_top and check_and_fuse_inner do for a static bind;
                // generalized_earlier is unchanged. Pin:
                // lang::byref::mut_reference_to_an_inferred_local_at_a_run_time_bind (a
                // dynamic call and an array::map slot; fails with 'binding the callee
                // failed: y was made generic' when the AtLevel is removed).
                if ctx.env.poly_binds.contains(&r.id) {
                    bailat!(
                        spec,
                        "{} was made generic by an earlier compile, and a writable \
                         reference to it would let a write narrow it: write it with <-",
                        r.spec
                    );
                }
                ctx.mark_connect_target(r.id).map_err(|e| e.at(&spec))?;
            }
        }
        Ok(Node::new(Self {
            spec,
            typ,
            referent,
            id,
            resident: TagValue::phantom(),
            registered: None,
            keep: None,
        }))
    }

    /// Write the cell, which `Deref` reads for a chainless reference and
    /// embedders read directly: a fire this cycle, bottoms included, as a
    /// `let` publishes; a quiet production under an init view stands in
    /// it, so the cell exists from the reference's birth.
    fn publish(&self, ctx: &mut ExecCtx<'_, R, E>, tv: TagValue) {
        let tag = tv.tag();
        if tag.triggers() {
            let stored = if tag.is_bottom() {
                TagValue::tagged(Value::Null, Tag::FRESH_BOTTOM)
            } else {
                TagValue::fired(tv.value_cloned())
            };
            ctx.rt.store_insert(self.id, stored);
            ctx.event.variables.insert(self.id, tv);
            ctx.rt.notify_set(self.id);
        } else if ctx.event.init() {
            ctx.rt.store_insert_standing(self.id, tv);
        }
    }

    /// Drop the place registration.
    // XCR claude for claude: [bug/medium] When a place's key goes bottom, unregister
    // clears the cell's ref_path. ref_target then falls back to the cell itself. A copy
    // of the reference held elsewhere (`holder <- &mut a[k]`) then writes into the
    // mirror cell (`write_target` gives `WriteTarget::Bind(cell)`), and its Deref reads
    // that value back: `*w` shows 99 while k is bottom and for a cycle after, and `a`
    // never held 99. place_references.md says such a write does not happen. Fix: keep
    // an addressless entry for the cell, so write_target has no target and the deref
    // reads bottom. probe: design/review-2026-10-26/repro/c-bind-addressless-write.gx
    // (c-bind-02)
    // 2026-10-26 claude: The runtime's place registry holds a node::place::RefPath,
    // At(root, path) or Nowhere: a place whose address is bottom (from birth or after
    // its key went bottom) registers Nowhere, so ExecCtx::write_target is None and
    // every writer (ConnectDeref, core write_through, queuefn, buffer decode, the
    // embedder's set_deref) drops the write, and ref_target reads the cell's bottom; a
    // delete or release clears the entry. ByRef::registered mirrors what the runtime
    // holds. Pin: lang::byref::a_write_through_an_addressless_place_does_not_happen (a
    // (3, 99) read appears with the old write target).
    fn unregister(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        if self.registered.take().is_some() {
            ctx.rt.clear_ref_path(&self.id);
        }
    }

    /// The cell when the place has no element: bottom.
    fn bottom_mirror(&self, ctx: &mut ExecCtx<'_, R, E>) {
        ctx.rt.store_insert(self.id, TagValue::tagged(Value::Null, Tag::FRESH_BOTTOM));
    }
}

/// The cell a reference value names.
pub(crate) fn ref_cell(v: &Value) -> Option<BindId> {
    crate::reference::cell(v)
}

/// What the reference `cell` names: its registered place, else the
/// binding at the end of its byref chain, else the cell itself (a
/// chainless reference's own cell is its only storage).
pub(crate) fn ref_target<'a, R: Rt, E: UserEvent>(
    ctx: &'a ExecCtx<'_, R, E>,
    cell: BindId,
) -> (BindId, &'a [place::Step]) {
    match ctx.rt.ref_path(&cell) {
        Some(RefPath::At(root, path)) => (*root, &path[..]),
        Some(RefPath::Nowhere) => (cell, &[][..]),
        None => (ctx.env.byref_chain.get(&cell).copied().unwrap_or(cell), &[][..]),
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for ByRef<R, E> {
    fn for_each_child<'a>(&'a self, f: &mut dyn FnMut(&'a Node<R, E>)) {
        self.referent.each_ref(f)
    }

    fn for_each_child_mut(&mut self, f: &mut dyn FnMut(&mut Node<R, E>)) {
        self.referent.each(f)
    }

    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::ByRef, buf);
        self.spec.encode(buf)?;
        self.typ.encode(buf)?;
        self.id.encode(buf)?;
        match &self.referent {
            Referent::Channel(n) => {
                false.encode(buf)?;
                n.image_encode(buf)
            }
            Referent::Place(p) => {
                true.encode(buf)?;
                p.image_encode(buf)
            }
        }
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R, E>) -> &TagValue {
        // the reference fires when its address changes; a write of its
        // root only refreshes the mirror
        let retargeted = match &mut self.referent {
            Referent::Channel(n) => {
                let tv = n.update(ctx).clone();
                self.publish(ctx, tv);
                false
            }
            Referent::Place(place) => {
                let Resolved { address, root, moved } = place.update(ctx);
                let Some(Address { bind, path, steps }) = address else {
                    let lost = matches!(self.registered, Some(RefPath::At(..)));
                    if !matches!(self.registered, Some(RefPath::Nowhere)) {
                        ctx.rt.set_ref_path(self.id, RefPath::Nowhere);
                        self.registered = Some(RefPath::Nowhere);
                    }
                    if lost {
                        self.bottom_mirror(ctx);
                    }
                    return self.resident.set_bottom(lost || ctx.event.born());
                };
                let same = matches!(&self.registered,
                    Some(RefPath::At(r, p)) if *r == bind && p == path);
                let moved = moved || !same;
                if !same {
                    let to = RefPath::At(bind, path.clone());
                    ctx.rt.set_ref_path(self.id, to.clone());
                    self.registered = Some(to);
                }
                // the cell mirrors the element, read through the address;
                // a bottom root sets it bottom
                if moved || ctx.event.init() {
                    let read = match root.tag().is_bottom() {
                        true => None,
                        false => Some(with_hooks(ctx, || {
                            root.with_value(|v| place::read_path(v, steps))
                        })),
                    };
                    match read {
                        Some(Ok(v)) => {
                            let tag = if moved { Tag::FIRED } else { root.tag() };
                            self.publish(ctx, TagValue::tagged(v, tag))
                        }
                        Some(Err(e)) => {
                            log::warn!("read through a reference: {e}");
                            self.bottom_mirror(ctx);
                        }
                        None => self.bottom_mirror(ctx),
                    }
                }
                !same
            }
        };
        if ctx.event.born() || retargeted {
            // XCR claude for claude: [bug/medium] The Keep's target is fixed at the
            // first publication. A place rooted at a dereference (`&mut *r`) that
            // retargets when r moves keeps pinning its first root, not the binding its
            // ref_path now names. After the site is deleted, a kept copy's binding can
            // be released under it, and writes through the copy are dropped with "its
            // root has no value". Move the pin on retarget: a mutable Keep target
            // updated under the Lives lock. A second Keep would not work, because the
            // old one dropping would release the cell. probe:
            // design/review-2026-10-26/repro/x-refs-catchup-moving-root-unpinned.gx
            // (x-refs-catchup-03)
            // 2026-10-26 claude: Lives keeps each cell's pinned binding
            // (LivesState::targets) instead of the Keep holding it, and Keep::retarget
            // moves the pin under the lock: ByRef::update retargets its keep whenever
            // the place re-registers on another binding, unpinning the old one (an
            // orphan whose last pin it was is released). Pins:
            // lang::byref::a_kept_place_pins_the_root_it_moved_to (writes dropped at
            // c=4.. without it),
            // reference::test::a_retargeted_keep_pins_its_new_binding_alone; both
            // repros give their expected output.
            // XCR claude for claude: [bug/medium] The keep is made once and pins the
            // binding the place named at that moment. A place rooted at `*r` that later
            // retargets (r moves from &mut y1 to &mut y2) still pins y1. When the slot
            // holding y2 is deleted, y2's value is released, and every write through a
            // surviving copy is dropped with 'its root has no value'. Fix: move the pin
            // to the new target whenever the place re-registers on another binding (a
            // Lives method that swaps the Keep's target). probe:
            // design/review-2026-10-26/repro/c-bind-keep-retarget.gx (c-bind-03)
            // 2026-10-26 claude: Lives keeps each cell's pinned binding
            // (LivesState::targets) instead of the Keep holding it, and Keep::retarget
            // moves the pin under the lock: ByRef::update retargets its keep whenever
            // the place re-registers on another binding, unpinning the old one (an
            // orphan whose last pin it was is released). Pins:
            // lang::byref::a_kept_place_pins_the_root_it_moved_to (writes dropped at
            // c=4.. without it),
            // reference::test::a_retargeted_keep_pins_its_new_binding_alone; both
            // repros give their expected output. (same fix as x-refs-catchup-03)
            let target = ctx.env.byref_chain.get(&self.id).copied();
            let target = target.or_else(|| match &self.registered {
                Some(RefPath::At(r, _)) => Some(*r),
                Some(RefPath::Nowhere) | None => None,
            });
            let keep = match &self.keep {
                Some(keep) => {
                    if let Some(t) = target {
                        keep.retarget(t)
                    }
                    keep
                }
                None => self.keep.insert(crate::reference::Lives::keep(
                    ctx.control.lives(),
                    self.id,
                    target,
                )),
            };
            let r = crate::reference::reference(self.id, Some(keep.clone()));
            self.resident.set(TagValue::fired(r))
        } else {
            self.resident.ride()
        }
    }

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        match self.keep.take() {
            // the references it published keep the cell, its place and what
            // it names until the last one drops (ExecCtx::release_references)
            Some(_) => {
                self.registered = None;
                self.resident = TagValue::phantom();
            }
            None => {
                ctx.env.byref_chain.remove(&self.id);
                ctx.env.by_id.remove(&self.id);
                ctx.rt.store_release(&self.id);
                self.unregister(ctx);
            }
        }
        self.referent.each(&mut |n| n.delete(ctx));
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn typ(&self) -> &Type {
        &self.typ
    }

    super::typed_by_row!();

    fn view(&self) -> NodeView<'_, R, E> {
        NodeView::ByRef(self)
    }
}

#[derive(Debug)]
pub struct Deref<R: Rt, E: UserEvent> {
    pub(crate) spec: Arc<Expr>,
    pub typ: Type,
    pub child: Node<R, E>,
    pub(super) top_id: ExprId,
    resident: TagValue,
    /// Where the reference points; `None` while it is bottom.
    addr: Option<Addr>,
}

/// A reference's target as a `Deref` reads it: the reference's own
/// cell, which a moving place reference notifies when its path moves,
/// the binding it resolves to and the path into that binding's value
/// (empty unless the reference is a place).
#[derive(Debug, Clone, netidx_derive::Pack)]
struct Addr {
    cell: BindId,
    id: BindId,
    path: Path,
}

/// How a `Deref`'s target changed at an update.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Moved {
    No,
    /// Same binding, another path: a moving place reference's key moved.
    Path,
    /// Another binding than the one it read.
    Target,
    /// The first binding since it read none.
    First,
}

impl<R: Rt, E: UserEvent> Deref<R, E> {
    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<'_, R, E>,
        buf: &mut &[u8],
    ) -> Result<Node<R, E>, PackError> {
        let spec = Arc::new(Expr::decode(buf)?);
        let typ = Type::decode(buf)?;
        let child = decode_node(ctx, buf)?;
        let top_id = ExprId::decode(buf)?;
        let addr = Option::<Addr>::decode(buf)?;
        if let Some(a) = &addr {
            ctx.record_ref(a.id, top_id);
            if a.cell != a.id {
                ctx.record_ref(a.cell, top_id);
            }
        }
        Ok(Node::new(Self {
            spec,
            typ,
            child,
            top_id,
            resident: TagValue::phantom(),
            addr,
        }))
    }

    pub(crate) fn compile(
        ctx: &mut CompileCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: Arc<Expr>,
        scope: &Scope,
        top_id: ExprId,
        expr: &Expr,
    ) -> Result<Node<R, E>> {
        let child = compile(ctx, flags, expr.clone(), scope, top_id)?;
        let typ = Type::empty_tvar();
        Ok(Node::new(Self {
            spec,
            typ,
            child,
            top_id,
            resident: TagValue::phantom(),
            addr: None,
        }))
    }

    /// Address `cell`'s referent: the place it stands for, else the
    /// binding at the end of its byref chain (`&x`'s cell mirrors x a
    /// cycle late, the referent does not; a chainless reference's own
    /// cell is its only storage). Returns the binding and how it moved.
    fn address(&mut self, ctx: &mut ExecCtx<'_, R, E>, cell: BindId) -> (BindId, Moved) {
        let (id, path) = ref_target(ctx, cell);
        let (same_id, same_path) = match &self.addr {
            Some(a) => (a.id == id, a.id == id && &a.path[..] == path),
            None => (false, false),
        };
        let path: Option<Path> = (!same_path).then(|| path.into());
        let moved = match (&mut self.addr, path) {
            (Some(a), None) => {
                Self::swap_cell(ctx, a, cell, self.top_id);
                Moved::No
            }
            (Some(a), Some(p)) if same_id => {
                Self::swap_cell(ctx, a, cell, self.top_id);
                a.path = p;
                Moved::Path
            }
            (prev, p) => {
                let moved = match prev {
                    Some(_) => Moved::Target,
                    None => Moved::First,
                };
                self.release(ctx);
                ctx.ref_var(id, self.top_id);
                if cell != id {
                    ctx.ref_var(cell, self.top_id);
                }
                self.addr = Some(Addr { cell, id, path: p.unwrap_or_default() });
                moved
            }
        };
        (id, moved)
    }

    /// Follow another reference cell to the same binding.
    fn swap_cell(
        ctx: &mut ExecCtx<'_, R, E>,
        a: &mut Addr,
        cell: BindId,
        top_id: ExprId,
    ) {
        if a.cell == cell {
            return;
        }
        if a.cell != a.id {
            ctx.unref_var(a.cell, top_id);
        }
        if cell != a.id {
            ctx.ref_var(cell, top_id);
        }
        a.cell = cell;
    }

    fn release(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        if let Some(a) = self.addr.take() {
            ctx.unref_var(a.id, self.top_id);
            if a.cell != a.id {
                ctx.unref_var(a.cell, self.top_id);
            }
        }
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Deref<R, E> {
    fn for_each_child<'a>(&'a self, f: &mut dyn FnMut(&'a Node<R, E>)) {
        f(&self.child)
    }

    fn for_each_child_mut(&mut self, f: &mut dyn FnMut(&mut Node<R, E>)) {
        f(&mut self.child)
    }

    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::Deref, buf);
        self.spec.encode(buf)?;
        self.typ.encode(buf)?;
        self.child.image_encode(buf)?;
        self.top_id.encode(buf)?;
        self.addr.encode(buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R, E>) -> &TagValue {
        let tv = self.child.update(ctx);
        let addr = tv.tag();
        let cell = match addr.is_bottom() {
            true => None,
            false => tv.with_value(ref_cell),
        };
        let Some(cell) = cell else {
            self.release(ctx);
            return self.resident.set_bottom(addr.triggers());
        };
        let (id, moved) = self.address(ctx, cell);
        // a target the arm around it learns only now may owe a fire
        let owed = super::wake::owed(ctx, &id);
        let res = match read_var(ctx, &id) {
            Some(VarRead::Delivered(tv)) => Some(tv.clone()),
            Some(VarRead::Standing(tv)) if owed => {
                Some(TagValue::fired(tv.value_cloned()))
            }
            Some(VarRead::Standing(tv)) => Some(standing_view(ctx.event, tv)),
            None => None,
        };
        let res = match (res, &self.addr) {
            (Some(tv), Some(a)) if !a.path.is_empty() && !tv.tag().is_bottom() => {
                let read =
                    with_hooks(ctx, || tv.with_value(|v| place::read_path(v, &a.path)));
                match read {
                    Ok(v) => {
                        let mut c = TagValue::fired(v);
                        c.retag(tv.tag());
                        Some(c)
                    }
                    Err(e) => {
                        log::warn!("read through a reference: {e}");
                        return self.resident.set_bottom(tv.tag().join(addr).triggers());
                    }
                }
            }
            (res, _) => res,
        };
        match res {
            Some(mut tv) => {
                // a move to another element is read fired
                let t = match moved {
                    // at a wake, catch-up decides through the reference's fire
                    // XCR claude for claude: [bug/medium] A key move is read fired here,
                    // but a move to another root binding (Moved::Target) is read quiet.
                    // Through a held copy of a place `&(*r)[0]` whose r moves from &a
                    // to &b, `*w` keeps the old element for a cycle (c=2: 10 while `*p`
                    // gives 30), because `let rd = *w` does not republish a quiet read.
                    // Fix: when a Moved::Target replaces an existing address, read it
                    // fresh outside a wake, as Moved::Path is; a first address keeps
                    // join(addr). probe:
                    // design/review-2026-10-26/repro/c-bind-target-move-quiet.gx
                    // (c-bind-04)
                    // 2026-10-26 claude: Deref::address tells a first binding
                    // (Moved::First, read joined with the reference's tag) from a move
                    // to another binding (Moved::Target), which is read fired outside a
                    // wake as a key's move is, a bottom included. Pin:
                    // lang::byref::a_held_place_reads_its_new_root_at_once (via the
                    // holder reads 10 at c=2 and 30 at c=4 without it).
                    Moved::Path | Moved::Target if !ctx.event.wake() => {
                        tv.tag().join(addr).fresh()
                    }
                    Moved::Path | Moved::Target | Moved::No | Moved::First => {
                        tv.tag().join(addr)
                    }
                };
                tv.retag(t);
                self.resident.set(tv)
            }
            // a moved address with nothing to read is bottom
            None if moved != Moved::No => {
                let fresh =
                    matches!(moved, Moved::Path | Moved::Target) && !ctx.event.wake();
                self.resident.set_bottom(addr.triggers() || fresh)
            }
            None => self.resident.ride(),
        }
    }

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.release(ctx);
        self.child.delete(ctx);
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn typ(&self) -> &Type {
        &self.typ
    }

    fn refs(&self, refs: &mut Refs) {
        self.child.refs(refs);
        if let Some(a) = &self.addr {
            refs.read(a.id);
            refs.read(a.cell);
        }
    }

    super::typed_by_row!();

    fn view(&self) -> NodeView<'_, R, E> {
        NodeView::Deref(self)
    }
}

impl<R: Rt, E: UserEvent> ByRef<R, E> {
    fn typecheck0_with(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        child: &mut super::Child<'_, R, E>,
        check: bool,
    ) -> Result<()> {
        let mut res = Ok(());
        self.referent.each(&mut |n| {
            if res.is_ok() {
                res = child(n, ctx).map_err(|e| e.at(n.spec()));
            }
        });
        res?;
        if !check {
            return Ok(());
        }
        let Type::ByRef(mutability, cell) = &self.typ else {
            bail!("BUG: a reference not typed as one")
        };
        let t = match &self.referent {
            // A writable reference to a fresh cell only it reaches: the
            // cell's type is any type the initializer fits, decided by the
            // uses.
            Referent::Channel(n)
                if *mutability == Mutability::Mut && !(&**n as &dyn Any).is::<Ref>() =>
            {
                super::defer_settle(ctx, || crate::PendingSettle::Contains {
                    outer: (**cell).clone(),
                    inner: n.typ().clone(),
                    spec: self.spec.clone(),
                });
                return Ok(());
            }
            Referent::Channel(n) => n.typ().clone(),
            Referent::Place(p) => {
                // a writable place through `*r` writes through `r`
                if *mutability == Mutability::Mut
                    && let Some(d) = (&*p.root as &dyn Any).downcast_ref::<Deref<R, E>>()
                {
                    let through =
                        Type::ByRef(Mutability::Mut, Arc::new(Type::empty_tvar()));
                    wrap!(self, through.check_contains(&ctx.env, d.child.typ()))?;
                }
                wrap!(self, p.elem_type(ctx, &self.spec, *mutability == Mutability::Mut))?
            }
        };
        // XCR claude for claude: [perf/low] A let chain of references (`let x1 = &x0;
        // let x2 = &x1; ..`) still checks in quadratic time: 1.43 s at 5000 lets and
        // 5.79 s at 10000 (debug --check), almost all of it in Typecheck0. The [X], {a:
        // X}, (X, 1) and `A(X) chains are now linear. The x-stack-03 note says &X is
        // linear too; that does not hold past about 2000 lets. By reading, this
        // check_contains binds the reference's own cell with an occurs walk over the
        // referent's whole type. SeqLit and Map skip that walk with
        // array::bind_element. Bind the cell the same way when it is this node's own,
        // and add an &X case to deep_nesting::wrapped_let_chains_check_in_linear_time.
        // probe: design/review-2026-10-26/repro/x-rerun-1-ref-chain-quadratic.sh
        // (x-rerun-1-03)
        // 2026-10-26 claude: The quadratic part was check_contains' closed() walk of
        // the whole referent at the top of every committing call. ByRef's check now
        // skips a referent whose type shares content with the one the reference was
        // typed with at compile (Type::shares_content, contains' own reflexive test,
        // now public), the case of every &x, and binds its own unshared cell with
        // array::bind_element (a place); anything else checks as before. Debug --check
        // of the chain: 1.16 / 1.33 / 1.8 s at 2500 / 5000 / 10000 (startup about 1 s)
        // against 2.2 / 3.7 / 9.4 s. Pin:
        // deep_nesting::wrapped_let_chains_check_in_linear_time's new &X case (past 15
        // s without it).
        if cell.shares_content(&t) || super::array::bind_element(cell, &t) {
            return Ok(());
        }
        let t = Type::ByRef(*mutability, Arc::new(t));
        wrap!(self, self.typ.check_contains(&ctx.env, &t))
    }
}

impl<R: Rt, E: UserEvent> Deref<R, E> {
    fn typecheck0_with(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        child: &mut super::Child<'_, R, E>,
        check: bool,
    ) -> Result<()> {
        wrap!(self.child, child(&mut self.child, ctx))?;
        if !check {
            return Ok(());
        }
        // A container read's type is a TVar bound to `&T`, not a bare
        // `Type::ByRef`.
        let typ = self.child.typ().with_deref(|t| match t {
            Some(Type::ByRef(_, t)) => Some((**t).clone()),
            _ => None,
        });
        let typ = match typ {
            Some(t) => t,
            None => match self.child.typ().deref_cloned() {
                None => bail!(
                    "dereferencing needs the reference's type known here: annotate \
                     the binding"
                ),
                Some(t) => format_with_flags(PrintFlag::DerefTVars, || {
                    bail!("expected a reference not {t}")
                })?,
            },
        };
        wrap!(self, self.typ.check_contains(&ctx.env, &typ))?;
        Ok(())
    }
}
