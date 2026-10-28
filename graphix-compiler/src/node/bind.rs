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
    Rt, Scope, Tag, TagValue, Update, bailat,
    compiler::compile,
    dbgenv,
    env::{self, Env},
    expr::{self, At, Expr, ExprId, ExprKind, ModPath, PlaceAccess},
    format_with_flags,
    fusion::{
        descend,
        emit::{BodyCx, CompiledExpr, emit_ref_node},
        fuse, fuse_parts,
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
pub struct Bind<R: Rt> {
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
    pub(crate) node: Node<R>,
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
pub(crate) fn lower_over_operands<R: Rt>(
    ctx: &mut CompileCtx<R>,
    flags: BitFlags<CFlag>,
    scope: &Scope,
    spec: &Expr,
    top_id: ExprId,
    operands: impl IntoIterator<Item = (ArcStr, Node<R>)>,
    body: Expr,
) -> Result<Node<R>> {
    // named by the lowered node's spec, which an image writes, as a
    // block's scope is
    let scope = scope.append_block("op", spec.id.inner());
    let mut children: Vec<Node<R>> = Vec::new();
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
        children.push(Bind::with(Arc::new(bspec), typ, pattern, node, None));
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
fn builtin_binding<R: Rt>(node: &Node<R>, value: &Expr) -> Option<BuiltinBindInfo> {
    let (Some(l), ExprKind::Lambda(lam), Type::Fn(typ)) =
        (lambda_value(node), &value.unparen().kind, node.typ())
    else {
        return None;
    };
    let def = l.def_value().downcast_ref::<LambdaDef<R>>()?;
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

impl<R: Rt> Bind<R> {
    fn with(
        spec: Arc<Expr>,
        typ: Type,
        pattern: StructPatternNode,
        node: Node<R>,
        deferred: Option<Deferred>,
    ) -> Node<R> {
        Node::new(Self {
            slept: WakeBit::default(),
            slept_at: 0,
            spec,
            typ,
            pattern,
            deferred,
            node,
            ever_published: false,
            reraised: SmallVec::new(),
        })
    }

    /// A write that landed while the binding slept (one queued before the
    /// sleep, a seq machine's reset) was seen by none of the readers that
    /// slept with it: deliver it to them fired, once, at the wake. A reader
    /// outside (a `&mut` that escaped) saw it live, so the enclosing block
    /// takes the delivery back after its last statement.
    fn reraise_writes_while_asleep(&mut self, ctx: &mut ExecCtx<'_, R>) {
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
    pub(crate) fn take_back_reraised(&mut self, ctx: &mut ExecCtx<'_, R>) {
        for id in self.reraised.drain(..) {
            ctx.event.variables.remove(&id);
        }
    }

    pub(crate) fn compile(
        ctx: &mut CompileCtx<R>,
        flags: BitFlags<CFlag>,
        spec: Arc<Expr>,
        scope: &Scope,
        top_id: ExprId,
        b: &expr::BindExpr,
    ) -> Result<Node<R>> {
        let expr::BindExpr { rec, pattern: pat, typ: annotation, value } = b;
        let annotation = match annotation {
            Some(t) => {
                let t = t.scope_refs(&scope.lexical).rewrite_trait_args(&ctx.env)?;
                super::lambda::body_annotation(ctx, &t);
                Some(t)
            }
            None => None,
        };
        let compile_pattern = |ctx: &mut CompileCtx<R>, typ: &Type| {
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
        Ok(Self::with(spec, typ, pattern, node, deferred))
    }

    /// Fit the pattern to the value's type, which must be known by now.
    fn complete_deferred(&mut self, ctx: &mut CompileCtx<R>) -> Result<()> {
        match self.deferred.take() {
            None => Ok(()),
            Some(Deferred::Partial(ptyp)) => self.complete_partial(ctx, ptyp),
            Some(Deferred::Shape(pred)) => self.complete_shape(ctx, pred),
        }
    }

    /// Hold `pred`, the predicate the pattern compiled against, to every
    /// member of the value's type, and type each name, a fresh cell, by
    /// its part of each member.
    fn complete_shape(&mut self, ctx: &mut CompileCtx<R>, pred: Type) -> Result<()> {
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
    fn complete_partial(&mut self, ctx: &mut CompileCtx<R>, ptyp: Type) -> Result<()> {
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
    fn check_value(&self, ctx: &mut CompileCtx<R>) -> Result<()> {
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
    fn publish(&self, ctx: &mut CompileCtx<R>) {
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
    fn aliased_lambda(&self, ctx: &mut CompileCtx<R>) -> Option<Value> {
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

impl<R: Rt> Bind<R> {
    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<'_, R>,
        buf: &mut &[u8],
    ) -> Result<Node<R>, PackError> {
        let spec = Arc::new(Expr::decode(buf)?);
        let typ = Type::decode(buf)?;
        let pattern = StructPatternNode::decode(buf)?;
        let node = decode_node(ctx, buf)?;
        Ok(Self::with(spec, typ, pattern, node, None))
    }
}

impl<R: Rt> Update<R> for Bind<R> {
    fn for_each_child<'a>(&'a self, f: &mut dyn FnMut(&'a Node<R>)) {
        f(&self.node)
    }

    fn for_each_child_mut(&mut self, f: &mut dyn FnMut(&mut Node<R>)) {
        f(&mut self.node)
    }

    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::Bind, buf);
        self.spec.encode(buf)?;
        self.typ.encode(buf)?;
        self.pattern.encode(buf)?;
        self.node.image_encode(buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R>) -> &TagValue {
        let woke = self.slept.take();
        if woke {
            // CR claude for claude: [bug/low] At a wake the write that landed during the
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
            // 2026-10-27 claude: Eric ruled (2026-10-27, cr-questions round 2, q4, A caught-up
            // initializer against a later write): yes, later wins (the recommendation: the
            // later write wins.)
            // 2026-10-27 codex: Partial, no implementation landed. Reproduced both supplied
            // probes. Tried retaining each target whose write stamp is newer than every free
            // initializer input, excluding a rebirth. The direct target/write-order and
            // queued-write pins passed (12 mode runs), but two additional pins still failed in
            // all four modes: c-bind-wake-order-alias.gx gives 100 instead of 555 because the
            // caught-up alias gets the wake cycle's stamp; c-bind-wake-order-sample.gx gives
            // 200 instead of 555 because the sample argument's later update is not the fire
            // that initialized the target. Added both probes under
            // design/review-2026-10-26/repro. Reverted the local comparison: it does not
            // implement the ruling through derived bindings or sampling. Remaining work is
            // causal provenance for initializer fires across those nodes; the store's delivery
            // stamp alone is insufficient and also serves read_var's live-delivery contract.
            // Trial Rust pins and the candidate patch are retained in /tmp/q4-trial-select.rs
            // and /tmp/q4-candidate.patch. No language rule or FuseExpect changed.
            self.reraise_writes_while_asleep(ctx);
        }
        let tv = self.node.update(ctx);
        let tag = tv.tag();
        // At a wake a `<-` target holding a value keeps it unless the
        // initializer fired; otherwise a production publishes when it
        // fires, first, or quietly after a sleep.
        let mut held: SmallVec<[BindId; 4]> = SmallVec::new();
        // CR claude for claude: [bug/medium] Two events missed during a sleep are applied
        // in the wrong order. The initializer input fired at c=4 and a write to this
        // target landed at c=5. At the wake the re-raised write is published first,
        // then the initializer's caught-up fire overwrites it, so x is 100 where an arm
        // that never slept stands at 555. Which should win: the later write (hold the
        // target when its write stamp is after every initializer input's), or always
        // the initializer? probe: design/review-2026-10-26/repro/c-bind-wake-order.gx
        // (c-bind-06)
        // 2026-10-27 claude: Eric ruled (2026-10-27, cr-questions round 2, q4, A
        // caught-up initializer against a later write): yes, later wins (the
        // recommendation: the later write wins.)
        // 2026-10-27 codex: Partial, no implementation landed. Reproduced both supplied
        // probes. Tried retaining each target whose write stamp is newer than every free
        // initializer input, excluding a rebirth. The direct target/write-order and
        // queued-write pins passed (12 mode runs), but two additional pins still failed
        // in all four modes: c-bind-wake-order-alias.gx gives 100 instead of 555 because
        // the caught-up alias gets the wake cycle's stamp; c-bind-wake-order-sample.gx
        // gives 200 instead of 555 because the sample argument's later update is not the
        // fire that initialized the target. Added both probes under
        // design/review-2026-10-26/repro. Reverted the local comparison: it does not
        // implement the ruling through derived bindings or sampling. Remaining work is
        // causal provenance for initializer fires across those nodes; the store's
        // delivery stamp alone is insufficient and also serves read_var's live-delivery
        // contract. Trial Rust pins and the candidate patch are retained in
        // /tmp/q4-trial-select.rs and /tmp/q4-candidate.patch. No language rule or
        // FuseExpect changed.
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

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R>) {
        // The static-resolution index outlives batches; a deleted
        // bind's entry must go with it.
        self.pattern.ids(&mut |id| {
            ctx.bind_to_lambda.remove(&id);
            ctx.static_callees.remove(&id);
            ctx.connect_targets.remove(&id);
            ctx.batch_connect_targets.remove(&id);
        });
        if let Some(l) = lambda_value(&self.node)
            && let Some(lambda) = l.lambda_id::<R>()
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

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R>) {
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

    fn typecheck0(&mut self, ctx: &mut CompileCtx<R>) -> Result<()> {
        if let ExprKind::Bind(b) = &self.spec.kind
            && b.typ.is_some()
        {
            crate::defer_unresolved_names(ctx, &self.typ, &self.spec)?;
        }
        wrap!(self.node, self.node.typecheck0(ctx))?;
        self.check_value(ctx)?;
        self.complete_deferred(ctx)?;
        self.publish(ctx);
        Ok(())
    }

    fn typecheck0_instance(
        &mut self,
        ctx: &mut CompileCtx<R>,
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

    fn view(&self) -> NodeView<'_, R> {
        NodeView::Bind(self)
    }

    fn fuse(&mut self, ctx: &mut CompileCtx<R>) -> Result<Option<Node<R>>> {
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
fn forwards<R: Rt>(env: &Env, b: &expr::BindExpr, node: &Node<R>) -> bool {
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
    pub(crate) fn new<R: Rt>(
        ctx: &mut CompileCtx<R>,
        id: BindId,
        typ: Type,
        top_id: ExprId,
        spec: impl Into<Arc<Expr>>,
    ) -> Node<R> {
        Self::with_signature(ctx, id, typ, Signature::Binding, top_id, spec)
    }

    fn with_signature<R: Rt>(
        ctx: &mut CompileCtx<R>,
        id: BindId,
        typ: Type,
        signature: Signature,
        top_id: ExprId,
        spec: impl Into<Arc<Expr>>,
    ) -> Node<R> {
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
    pub(crate) fn compile<R: Rt>(
        ctx: &mut CompileCtx<R>,
        spec: Arc<Expr>,
        scope: &Scope,
        top_id: ExprId,
        name: &ModPath,
    ) -> Result<Node<R>> {
        Self::compile_as(ctx, None, spec, scope, top_id, name)
    }

    /// A value occurrence of `name`: a trait method named is its
    /// eta-expansion, compiled under `flags`.
    pub(crate) fn compile_value<R: Rt>(
        ctx: &mut CompileCtx<R>,
        flags: BitFlags<CFlag>,
        spec: Arc<Expr>,
        scope: &Scope,
        top_id: ExprId,
        name: &ModPath,
    ) -> Result<Node<R>> {
        Self::compile_as(ctx, Some(flags), spec, scope, top_id, name)
    }

    fn compile_as<R: Rt>(
        ctx: &mut CompileCtx<R>,
        value: Option<BitFlags<CFlag>>,
        spec: Arc<Expr>,
        scope: &Scope,
        top_id: ExprId,
        name: &ModPath,
    ) -> Result<Node<R>> {
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
fn misfit<R: Rt>(
    ctx: &CompileCtx<R>,
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
fn fits_every_member<R: Rt>(
    ctx: &CompileCtx<R>,
    ptyp: &Type,
    typ: &Type,
    narrowed: &mut Option<Type>,
    binds: impl FnOnce() -> Result<Type>,
) -> Result<bool> {
    let mut members: SmallVec<[Type; 8]> = SmallVec::new();
    expr::union_members(&ctx.env, typ, &mut members)?;
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
    fn decide<R: Rt>(&self, ctx: &CompileCtx<R>, ft: Arc<FnType>) -> Arc<FnType> {
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
    pub(crate) fn image_decode<R: Rt>(
        ctx: &mut ExecCtx<'_, R>,
        buf: &mut &[u8],
    ) -> Result<Node<R>, PackError> {
        let spec = Arc::new(Expr::decode(buf)?);
        let typ = Type::decode(buf)?;
        let id = BindId::decode(buf)?;
        let top_id = ExprId::decode(buf)?;
        Ok(Self::new(ctx, id, typ, top_id, spec))
    }
}

impl<R: Rt> Update<R> for Ref {
    /// An instance checks this node as its definition's check did.
    fn typecheck0_instance(
        &mut self,
        ctx: &mut CompileCtx<R>,
        _types: &mut crate::node::lambda::InstanceTypes,
    ) -> Result<()> {
        self.typecheck0(ctx)
    }

    fn for_each_child<'a>(&'a self, f: &mut dyn FnMut(&'a Node<R>)) {
        let _ = f;
    }

    fn for_each_child_mut(&mut self, f: &mut dyn FnMut(&mut Node<R>)) {
        let _ = f;
    }

    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::Ref, buf);
        self.spec.encode(buf)?;
        self.typ.encode(buf)?;
        self.id.encode(buf)?;
        self.top_id.encode(buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R>) -> &TagValue {
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

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R>) {
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
    fn typecheck0(&mut self, ctx: &mut CompileCtx<R>) -> Result<()> {
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

    fn view(&self) -> NodeView<'_, R> {
        NodeView::Ref(self)
    }

    fn emit_clif(&self, cx: &mut BodyCx) -> Result<CompiledExpr> {
        emit_ref_node(cx, self.spec.as_ref(), self.id, &self.typ)
    }
}

/// One accessor of a place reference's path, with its key expression
/// where the key is dynamic.
#[derive(Debug)]
enum PlaceStep<R: Rt> {
    Index(Node<R>),
    Tuple(usize),
    Field(expr::Name),
    Key(Node<R>),
}

/// The place a `&root[i].f` reference stands for: the root node (a
/// variable reference or a dereference) and the path's steps
/// (design/place_references.md), typed from `scope`. `path` is the path
/// as last resolved.
#[derive(Debug)]
struct Place<R: Rt> {
    root: Node<R>,
    steps: Vec<PlaceStep<R>>,
    scope: ModPath,
    path: Path,
}

impl<R: Rt> PlaceStep<R> {
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
        ctx: &mut ExecCtx<'_, R>,
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
fn lambda_value<R: Rt>(node: &Node<R>) -> Option<&super::lambda::Lambda> {
    match ungrouped(node) {
        NodeView::Lambda(l) => Some(l),
        _ => None,
    }
}

/// What `node` is under its grouping parentheses.
fn ungrouped<R: Rt>(node: &Node<R>) -> NodeView<'_, R> {
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
fn root_place<R: Rt>(root: &Node<R>) -> Option<(BindId, &Path)> {
    let any = &**root as &dyn Any;
    match any.downcast_ref::<Ref>() {
        Some(r) => Some((r.id, Path::empty())),
        None => {
            let a = any.downcast_ref::<Deref<R>>()?.addr.as_ref()?;
            Some((a.id, &a.path))
        }
    }
}

impl<R: Rt> Place<R> {
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        self.root.image_encode(buf)?;
        encode_varint(self.steps.len() as u64, buf);
        for s in &self.steps {
            s.image_encode(buf)?;
        }
        self.scope.encode(buf)
    }

    fn image_decode(
        ctx: &mut ExecCtx<'_, R>,
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
        ctx: &mut CompileCtx<R>,
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

    fn update(&mut self, ctx: &mut ExecCtx<'_, R>) -> Resolved<'_> {
        let root = self.root.update(ctx).clone();
        let mut moved = root.tag().triggers();
        let mut suffix: LPooled<Vec<place::Step>> = LPooled::take();
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
                        Some(i) => suffix.push(place::Step::Index(i)),
                        None => complete = false,
                    }
                }
                PlaceStep::Tuple(i) => suffix.push(place::Step::Index(*i as i64)),
                PlaceStep::Field(name) => {
                    suffix.push(place::Step::Field(name.name.clone()))
                }
                PlaceStep::Key(n) => {
                    let tv = n.update(ctx);
                    moved |= tv.tag().triggers();
                    if tv.tag().is_bottom() {
                        complete = false
                    } else {
                        suffix.push(place::Step::Key(tv.value_cloned()))
                    }
                }
            }
        }
        let own_steps = suffix.len();
        let address = if complete {
            root_place(&self.root).map(|(bind, prefix)| {
                self.path.set(prefix, &suffix);
                Address {
                    bind,
                    path: &self.path,
                    steps: if own_steps == 0 { &[] } else { self.path.suffix() },
                }
            })
        } else {
            None
        };
        Resolved { address, root, moved }
    }

    fn each(&mut self, f: &mut dyn FnMut(&mut Node<R>)) {
        f(&mut self.root);
        for step in &mut self.steps {
            match step {
                PlaceStep::Index(n) | PlaceStep::Key(n) => f(n),
                PlaceStep::Tuple(_) | PlaceStep::Field(_) => (),
            }
        }
    }

    fn each_ref<'a>(&'a self, f: &mut dyn FnMut(&'a Node<R>)) {
        f(&self.root);
        for step in &self.steps {
            match step {
                PlaceStep::Index(n) | PlaceStep::Key(n) => f(n),
                PlaceStep::Tuple(_) | PlaceStep::Field(_) => (),
            }
        }
    }

    /// The keys fuse as parts; the root only descends, so it stays the
    /// variable or dereference `root_place` reads.
    fn fuse(&mut self, ctx: &mut CompileCtx<R>) -> Result<()> {
        descend(&mut self.root, ctx)?;
        let keys = self.steps.iter_mut().filter_map(|s| match s {
            PlaceStep::Index(n) | PlaceStep::Key(n) => Some(n),
            PlaceStep::Tuple(_) | PlaceStep::Field(_) => None,
        });
        fuse_parts(keys, ctx).map(|_| ())
    }

    /// The element type: each step's accessor rule over the root's type,
    /// without the access's failure (a place handles that at runtime). A
    /// writable field step over a union needs every member's field to be
    /// one type, so a write keeps the root in its own member.
    fn elem_type(
        &self,
        ctx: &mut CompileCtx<R>,
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
enum Referent<R: Rt> {
    Channel(Node<R>),
    Place(Place<R>),
}

impl<R: Rt> Referent<R> {
    fn each(&mut self, f: &mut dyn FnMut(&mut Node<R>)) {
        match self {
            Referent::Channel(n) => f(n),
            Referent::Place(p) => p.each(f),
        }
    }

    fn each_ref<'a>(&'a self, f: &mut dyn FnMut(&'a Node<R>)) {
        match self {
            Referent::Channel(n) => f(n),
            Referent::Place(p) => p.each_ref(f),
        }
    }
}

#[derive(Debug)]
pub struct ByRef<R: Rt> {
    pub(crate) spec: Arc<Expr>,
    pub typ: Type,
    referent: Referent<R>,
    pub id: BindId,
    resident: TagValue,
    /// What the runtime holds the cell stands for, when it holds anything.
    registered: Option<RefPath>,
    /// Shared by every reference value this site published: the last to
    /// drop releases the cell, its place and what it names.
    keep: Option<triomphe::Arc<crate::reference::Keep>>,
}

impl<R: Rt> ByRef<R> {
    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<'_, R>,
        buf: &mut &[u8],
    ) -> Result<Node<R>, PackError> {
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
        ctx: &mut CompileCtx<R>,
        flags: BitFlags<CFlag>,
        spec: Arc<Expr>,
        scope: &Scope,
        top_id: ExprId,
        mutability: Mutability,
        expr: &Expr,
    ) -> Result<Node<R>> {
        let id = BindId::new();
        // A place reference types as a reference to the element and
        // still mints a cell so embedders keep reading the mirror.
        let (referent, typ) = match Place::<R>::of(expr) {
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
    fn publish(&self, ctx: &mut ExecCtx<'_, R>, tv: TagValue) {
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
    fn unregister(&mut self, ctx: &mut ExecCtx<'_, R>) {
        if self.registered.take().is_some() {
            ctx.rt.clear_ref_path(&self.id);
        }
    }

    /// The cell when the place has no element: bottom.
    fn bottom_mirror(&self, ctx: &mut ExecCtx<'_, R>) {
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
pub(crate) fn ref_target<'a, R: Rt>(
    ctx: &'a ExecCtx<'_, R>,
    cell: BindId,
) -> (BindId, &'a Path) {
    match ctx.rt.ref_path(&cell) {
        Some(RefPath::At(root, path)) => (*root, path),
        Some(RefPath::Nowhere) => (cell, Path::empty()),
        None => (ctx.env.byref_chain.get(&cell).copied().unwrap_or(cell), Path::empty()),
    }
}

impl<R: Rt> Update<R> for ByRef<R> {
    fn for_each_child<'a>(&'a self, f: &mut dyn FnMut(&'a Node<R>)) {
        self.referent.each_ref(f)
    }

    fn for_each_child_mut(&mut self, f: &mut dyn FnMut(&mut Node<R>)) {
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

    fn update(&mut self, ctx: &mut ExecCtx<'_, R>) -> &TagValue {
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

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R>) {
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

    fn fuse(&mut self, ctx: &mut CompileCtx<R>) -> Result<Option<Node<R>>> {
        match &mut self.referent {
            Referent::Channel(n) => fuse_parts([n], ctx),
            Referent::Place(p) => p.fuse(ctx).map(|()| None),
        }
    }

    super::typed_by_row!();

    fn view(&self) -> NodeView<'_, R> {
        NodeView::ByRef(self)
    }
}

#[derive(Debug)]
pub struct Deref<R: Rt> {
    pub(crate) spec: Arc<Expr>,
    pub typ: Type,
    pub child: Node<R>,
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

impl<R: Rt> Deref<R> {
    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<'_, R>,
        buf: &mut &[u8],
    ) -> Result<Node<R>, PackError> {
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
        ctx: &mut CompileCtx<R>,
        flags: BitFlags<CFlag>,
        spec: Arc<Expr>,
        scope: &Scope,
        top_id: ExprId,
        expr: &Expr,
    ) -> Result<Node<R>> {
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
    fn address(&mut self, ctx: &mut ExecCtx<'_, R>, cell: BindId) -> (BindId, Moved) {
        let (id, path) = ref_target(ctx, cell);
        let (same_id, same_path) = match &self.addr {
            Some(a) => (a.id == id, a.id == id && &a.path == path),
            None => (false, false),
        };
        let path: Option<Path> = (!same_path).then(|| path.clone());
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
    fn swap_cell(ctx: &mut ExecCtx<'_, R>, a: &mut Addr, cell: BindId, top_id: ExprId) {
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

    fn release(&mut self, ctx: &mut ExecCtx<'_, R>) {
        if let Some(a) = self.addr.take() {
            ctx.unref_var(a.id, self.top_id);
            if a.cell != a.id {
                ctx.unref_var(a.cell, self.top_id);
            }
        }
    }
}

impl<R: Rt> Update<R> for Deref<R> {
    fn for_each_child<'a>(&'a self, f: &mut dyn FnMut(&'a Node<R>)) {
        f(&self.child)
    }

    fn for_each_child_mut(&mut self, f: &mut dyn FnMut(&mut Node<R>)) {
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

    fn update(&mut self, ctx: &mut ExecCtx<'_, R>) -> &TagValue {
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

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R>) {
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

    fn fuse(&mut self, ctx: &mut CompileCtx<R>) -> Result<Option<Node<R>>> {
        fuse_parts([&mut self.child], ctx)
    }

    super::typed_by_row!();

    fn view(&self) -> NodeView<'_, R> {
        NodeView::Deref(self)
    }
}

impl<R: Rt> ByRef<R> {
    fn typecheck0_with(
        &mut self,
        ctx: &mut CompileCtx<R>,
        child: &mut super::Child<'_, R>,
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
                    && let Some(d) = (&*p.root as &dyn Any).downcast_ref::<Deref<R>>()
                {
                    let through =
                        Type::ByRef(Mutability::Mut, Arc::new(Type::empty_tvar()));
                    wrap!(self, through.check_contains(&ctx.env, d.child.typ()))?;
                }
                wrap!(self, p.elem_type(ctx, &self.spec, *mutability == Mutability::Mut))?
            }
        };
        if cell.shares_content(&t) || super::array::bind_element(cell, &t) {
            return Ok(());
        }
        let t = Type::ByRef(*mutability, Arc::new(t));
        wrap!(self, self.typ.check_contains(&ctx.env, &t))
    }
}

impl<R: Rt> Deref<R> {
    // XCR claude for claude: [bug/low] A read through a union of writable references is
    // refused where a write through the same union is accepted. With `x: i64`, `y:
    // f64` and `let r = select b { true => &mut x, false => &mut y }`, `*r <- ..` is
    // checked against every member (ConnectDeref::typecheck0_with) but `*r` fails
    // "expected a reference not [&mut i64, &mut f64]". `select b { true => &mut x,
    // false => &mut (i64:2) }` fails the same way, as `[&mut i64, &mut '_: unbound]`:
    // the fresh cell is still open when the arms join, so the two members do not
    // merge. A union of `&` references reads, since `&` merges covariantly. Question:
    // does `*r` over a union of references read the union of their referents (the
    // natural dual of the write rule)? If so, share ConnectDeref's member expansion
    // (typedefs included) and take the union here; if not, the write should be
    // refused too. Found by the fuzz generator's arm-minted references
    // (fuzz-gen-a-06), which now mint only named targets. (deref-union-01)
    // 2026-10-27 claude: Eric ruled (2026-10-27, cr-questions round 2, q25, Reading
    // through a union of references): yes, read the union (the recommendation: read
    // the union.)
    // 2026-10-27 codex: Implemented Eric's q25 ruling. Deref and ConnectDeref share a
    // pooled reference-target traversal that expands aliases and union members; reads
    // union every referent, writes retain the requirement that every target be
    // writable and accept the value. Pins
    // lang::byref::deref_union_{reads_every_referent,alias_members,fresh_cell}: all 12
    // mode cases failed without the fix with expected-reference diagnostics. With the
    // fix the complete lang::byref:: module passes (250 tests); the read pin also
    // asserts its observed JIT fusion. Validation: shared flock,
    // CARGO_TARGET_DIR=$PWD/target, cargo test -p graphix-tests --offline --lib
    // lang::byref::. Rules, book references page and language skill updated. No netidx
    // public signature changed.
    fn typecheck0_with(
        &mut self,
        ctx: &mut CompileCtx<R>,
        child: &mut super::Child<'_, R>,
        check: bool,
    ) -> Result<()> {
        wrap!(self.child, child(&mut self.child, ctx))?;
        if !check {
            return Ok(());
        }
        let targets = super::reference_targets(&ctx.env, self.child.typ())?;
        let Some(targets) = targets else {
            bail!(
                "dereferencing needs the reference's type known here: annotate \
                 the binding"
            );
        };
        let types: LPooled<Vec<&Type>> = targets.iter().map(|(_, t)| t).collect();
        let typ = Type::union(&ctx.env, &types)?;
        wrap!(self, self.typ.check_contains(&ctx.env, &typ))?;
        Ok(())
    }
}
