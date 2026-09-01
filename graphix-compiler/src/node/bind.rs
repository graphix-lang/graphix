use super::{collection::CollectionIntrinsic, pattern::StructPatternNode};
use crate::{
    BindId, BuiltinBindInfo, CFlag, Event, ExecCtx, Node, NodeView, PrintFlag, Refs, Rt,
    Scope, Tag, TagValue, Update, UserEvent, bailat,
    compiler::compile,
    expr::{self, Expr, ExprId, ExprKind, ModPath},
    format_with_flags,
    fusion::{
        emit::{BodyCx, CompiledExpr, emit_ref_node},
        fuse,
    },
    ide::ReferenceSite,
    typ::Type,
    wrap,
};
use anyhow::{Context, Result, bail};
use arcstr::ArcStr;
use enumflags2::BitFlags;
use netidx_value::Value;
use poolshark::local::LPooled;
use triomphe::Arc;

#[derive(Debug)]
pub struct Bind<R: Rt, E: UserEvent> {
    pub(crate) spec: Expr,
    pub(crate) typ: Type,
    pub(crate) pattern: StructPatternNode,
    pub(crate) node: Node<R, E>,
    /// Has this binding ever put a value on the value channel? Until it
    /// has, there is no store entry for a reader to fall through to, so
    /// even a QUIET production must be published — see `update`.
    published: bool,
}

/// A compiler lowering that REWRITES a node into a block: the node's
/// already-compiled operand nodes become `let <name> = <node>`
/// bindings (moved, never recompiled — their source may name
/// bindings of an enclosing lambda whose lexical env is long
/// restored), and `body` — an expression over those names, compiled
/// and typechecked here under `scope` — is the block's value.
pub(crate) fn lower_over_operands<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<R, E>,
    flags: BitFlags<CFlag>,
    scope: &Scope,
    spec: &Expr,
    top_id: ExprId,
    operands: impl IntoIterator<Item = (ArcStr, Node<R, E>)>,
    body: Expr,
) -> Result<Node<R, E>> {
    let mk = |kind: ExprKind| Expr {
        id: ExprId::new(),
        ori: spec.ori.clone(),
        pos: spec.pos,
        kind,
        dec: None,
    };
    let mut children: Vec<Node<R, E>> = Vec::new();
    for (name, node) in operands {
        let typ = node.typ().clone();
        let pattern = StructPatternNode::compile(
            ctx,
            &typ,
            &expr::StructurePattern::Bind(name.clone()),
            scope,
            spec.pos,
            spec.ori.clone(),
        )?;
        let bspec = mk(ExprKind::Bind(Arc::new(expr::BindExpr {
            rec: false,
            pattern: expr::StructurePattern::Bind(name),
            typ: None,
            value: node.spec().clone(),
        })));
        children.push(Node::new(Bind {
            spec: bspec,
            typ,
            pattern,
            node,
            published: false,
        }));
    }
    let mut body = compile(ctx, flags, body, scope, top_id)?;
    body.typecheck0(ctx)?;
    body.typecheck1(ctx)?;
    let bspec = mk(ExprKind::Do {
        exprs: Arc::from_iter(children.iter().chain([&body]).map(|n| n.spec().clone())),
    });
    children.push(body);
    Ok(super::Block::new(false, children.into_boxed_slice(), bspec, scope.clone()))
}

impl<R: Rt, E: UserEvent> Bind<R, E> {
    /// The single `BindId` this binding introduces, when the pattern
    /// binds exactly one name (`let x = …`). `None` for destructuring
    /// patterns. Used by the fusion walker (ValueBind candidates) and
    /// the JIT block-let binder (BindId-keyed env slots).
    pub(crate) fn single_bind_id(&self) -> Option<BindId> {
        let mut id: Option<BindId> = None;
        let mut count = 0usize;
        self.pattern.ids(&mut |i| {
            count += 1;
            if id.is_none() {
                id = Some(i);
            }
        });
        if count == 1 { id } else { None }
    }

    pub(crate) fn compile(
        ctx: &mut ExecCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: Expr,
        scope: &Scope,
        top_id: ExprId,
        b: &expr::BindExpr,
    ) -> Result<Node<R, E>> {
        let expr::BindExpr { rec, pattern, typ, value } = b;
        let (node, pattern, typ) = if *rec {
            if !pattern.single_bind().is_some() {
                bailat!(spec, "can't use rec on a complex pattern")
            }
            // Parens are transparent everywhere else, so look through
            // them here too: `let rec f = (|n| …)` refused while the
            // bare spelling compiled (typemorph parens-wrap, SOUND
            // grade — 14 corpus hits on the first sweep). The check is
            // purely syntactic; the value compiles generically below.
            let mut v = value;
            while let ExprKind::ExplicitParens(inner) = &v.kind {
                v = inner;
            }
            match v {
                Expr { kind: ExprKind::Lambda(_), .. } => (),
                _ => bail!("let rec may only be used for lambdas"),
            }
            let typ = match typ {
                Some(typ) => typ.scope_refs(&scope.lexical),
                None => Type::empty_tvar(),
            };
            let pattern = StructPatternNode::compile(
                ctx,
                &typ,
                pattern,
                scope,
                spec.pos,
                spec.ori.clone(),
            )
            .with_context(|| expr::ErrorContext(spec.clone()))?;
            let node = compile(ctx, flags, value.clone(), &scope, top_id)?;
            let ntyp = node.typ();
            if !typ.contains(&ctx.env, ntyp)? {
                format_with_flags(PrintFlag::DerefTVars, || {
                    bailat!(spec, "error {} can't be matched by {typ}", ntyp)
                })?
            }
            (node, pattern, typ)
        } else {
            let node = compile(ctx, flags, value.clone(), &scope, top_id)?;
            let typ = match typ {
                Some(typ) => typ.rewrite_trait_args(&ctx.env)?.scope_refs(&scope.lexical),
                None => {
                    let typ = node.typ().clone();
                    let ptyp = pattern.infer_type_predicate(&ctx.env, &scope.lexical)?;
                    if !ptyp.contains(&ctx.env, &typ)? {
                        format_with_flags(PrintFlag::DerefTVars, || {
                            bailat!(spec, "match error {typ} can't be matched by {ptyp}")
                        })?
                    }
                    typ
                }
            };
            let pattern = StructPatternNode::compile(
                ctx,
                &typ,
                pattern,
                scope,
                spec.pos,
                spec.ori.clone(),
            )
            .with_context(|| expr::ErrorContext(spec.clone()))?;
            (node, pattern, typ)
        };
        if pattern.is_refutable() {
            bailat!(spec, "refutable patterns are not allowed in let");
        }
        // a let-bound lambda is GENERALIZED: every later reference
        // instantiates its signature (see `instantiate`). Registered
        // after the value compiled, so a `let rec` body's own
        // self-references keep the definition's cells (monomorphic
        // recursion — the def gate's knot).
        if matches!(node.view(), NodeView::Lambda(_)) {
            pattern.ids(&mut |id| {
                ctx.env.poly_binds.insert_cow(id);
            });
        }
        // If the bind's value is a builtin lambda (`let foo = |...| 'name`),
        // stash the metadata on `ctx.builtin_bindings` so the fusion
        // pass can recognise `Apply { function: Ref(foo) }` sites as
        // direct calls to a builtin and lower them via
        // `FnSource::Builtin`. Only fires for single-bind patterns
        // (multi-bind destructure of a lambda doesn't happen in
        // practice) and when the lambda body is the `'name` form.
        // If the bind's value is a builtin lambda (`let foo = |...| 'name`)
        // and the pattern is a simple `let <name> = ...`, register
        // the builtin metadata on `ctx.builtin_bindings` keyed by
        // (scope, name). Fusion's discovery pass looks up by
        // (scope, name) at every `Apply` site, so it doesn't
        // matter that sig and impl get different `BindId`s.
        if let ExprKind::Bind(be) = &spec.kind {
            if let expr::StructurePattern::Bind(bind_name) = &be.pattern {
                if let ExprKind::Lambda(lam) = &value.kind {
                    if let netidx_core::utils::Either::Right(builtin_name) = &lam.body {
                        if CollectionIntrinsic::from_name(builtin_name).is_none()
                            && let Type::Fn(fn_type) = node.typ()
                        {
                            // Lambda Node's def field holds the
                            // LambdaDef; downcast through NodeView::Lambda
                            // to pull its id. Used at fusion time to
                            // look up the lambda's env+scope when
                            // compiling labeled-default arg expressions.
                            let lambda_id = match node.view() {
                                NodeView::Lambda(l) => l.lambda_id::<R, E>(),
                                _ => None,
                            };
                            ctx.builtin_bindings.insert(
                                (
                                    scope.lexical.clone(),
                                    compact_str::CompactString::from(bind_name.as_str()),
                                ),
                                BuiltinBindInfo {
                                    name: builtin_name.clone(),
                                    argspec: lam.args.clone(),
                                    typ: fn_type.clone(),
                                    lambda_id,
                                },
                            );
                        }
                    }
                }
            }
        }
        Ok(Node::new(Self { spec, typ, pattern, node, published: false }))
    }

    /// The LambdaDef `Value` this binding holds, when its value node is
    /// a lambda (`let f = |…| …`) — `None` otherwise. The one home for
    /// "is this a lambda binding," consumed by `Bind::typecheck0` to
    /// populate `ctx.bind_to_lambda` (the static-resolution index).
    pub(crate) fn lambda_def_value(&self) -> Option<Value> {
        match self.node.view() {
            NodeView::Lambda(l) => Some(l.def_value().clone()),
            _ => None,
        }
    }

    /// Return the id if this bind has only a single binding, otherwise return None
    pub(crate) fn single_id(&self) -> Option<BindId> {
        let mut id = None;
        let mut n = 0;
        self.pattern.ids(&mut |i| {
            if n == 0 {
                id = Some(i)
            }
            n += 1
        });
        if n == 1 { id } else { None }
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Bind<R, E> {
    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> &TagValue {
        let tv = self.node.update(ctx, event);
        let tag = tv.tag();
        // Publish TRIGGERING productions — a stale RHS is the value
        // channel, already served by the bound names' store entries. A
        // fresh bottom poisons each bound name AND persists in the
        // store (ruled delta 7: a fresh reader must see the standing
        // bottom, not resurrect the pre-bottom value; the store keeps
        // at-rest bottoms, `cached` never sees the placeholder).
        //
        // "already served" holds only once there IS an entry, so the
        // FIRST value-bearing production publishes whatever its tag:
        // a binding whose RHS is constant-only, inside a select arm
        // that was asleep for the one genuine-init dispatch, never
        // fires again (in a frame `Constant` delivers STALE by design,
        // node/mod.rs — frame depth is checked before `event.init`).
        // Without this its readers MISS the store outright and ride a
        // phantom bottom, so the arm computes nothing at all while the
        // kernel, which recomputes constants per invocation, produces
        // the value (fuzz/pending-triage/rec_arm_let_missing_fire.gx).
        // The tag stays honest, so a quiet publish is value channel
        // only and wakes nobody.
        // A select arm's WAKE resumes the arm, it does not create one:
        // re-running the initializer of a binding that is a `<-` target
        // and already holds a value would throw away what the connect
        // accumulated while the arm slept, and sleep is PAUSE (Eric
        // 2026-07-31). Only `<-` targets are held back — every other
        // binding's initializer is idempotent w.r.t. the value channel,
        // and the wake's init view stays intact for everything else
        // (kernels, call-site priming, fresh reads).
        let wake_hold = event.wake_init && self.published && {
            let mut target = false;
            self.pattern.ids(&mut |id| {
                target = target || ctx.connect_targets.contains(&id);
            });
            target
        };
        if crate::dbgenv::gxdbg_letbind() {
            eprintln!(
                "LETBIND {} tag={tag:?} val={:?} published={} fd={} wake_hold={wake_hold} publishing={}",
                self.spec.pos,
                tv.value_cloned(),
                self.published,
                ctx.frame_depth,
                !wake_hold && (tag.triggers() || (!self.published && !tag.is_bottom()))
            );
        }
        // WAKE CATCH-UP (design/wake_catchup.md): the first update
        // after this node's sleep recomputes the RHS from present
        // values — a STALE production here may carry a value the
        // store's standing entry has drifted behind. Re-publish it on
        // the value channel (quiet — the tag stays honest, wakes
        // nobody); `<-` targets stay held back exactly as above.
        let wake_refresh =
            ctx.wake_recompute() && !tag.triggers() && !tag.is_bottom() && !wake_hold;
        if !wake_hold
            && (tag.triggers() || (!self.published && !tag.is_bottom()) || wake_refresh)
        {
            if tag.is_bottom() {
                self.pattern.ids(&mut |id| {
                    event
                        .variables
                        .insert(id, TagValue::tagged(Value::Null, Tag::FRESH_BOTTOM));
                    ctx.rt.store_insert(
                        id,
                        TagValue::tagged(Value::Null, Tag::FRESH_BOTTOM),
                    );
                    ctx.rt.notify_set(id);
                });
            } else {
                let quiet = !tag.triggers();
                let v = tv.value_cloned();
                self.pattern.bind(&v, &mut |id, v| {
                    event.variables.insert(id, TagValue::tagged(v.clone(), tag));
                    ctx.rt.store_insert(id, TagValue::fired(v));
                    if !quiet {
                        ctx.rt.notify_set(id);
                    }
                });
                self.published = true;
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

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        // The static-resolution index survives across batches since the
        // jul12 flap fix — a deleted bind's entry must go with it, or a
        // long-lived runtime (LSP/REPL) accumulates dead LambdaDefs.
        self.pattern.ids(&mut |id| {
            ctx.bind_to_lambda.remove(&id);
            ctx.connect_targets.remove(&id);
        });
        self.node.delete(ctx);
        self.pattern.delete(ctx);
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.node.sleep(ctx);
    }

    fn reset_replay(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.node.reset_replay(ctx);
    }

    fn typ(&self) -> &Type {
        &self.typ
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn typecheck0(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        wrap!(self.node, self.node.typecheck0(ctx))?;
        // `let g = f` with `f` generalized and no annotation: `g` IS
        // `f`'s scheme (its recorded type shares the definition's
        // cells) and is generalized in turn — unifying it with the
        // occurrence's fresh instance would pin the definition
        let forwards = match &self.spec.kind {
            ExprKind::Bind(b) if b.typ.is_none() => match self.node.view() {
                NodeView::Ref(r) => ctx.env.poly_binds.contains(&r.id),
                _ => false,
            },
            _ => false,
        };
        if forwards {
            self.pattern.ids(&mut |id| {
                ctx.env.poly_binds.insert_cow(id);
            });
        } else {
            wrap!(self.node, self.typ.check_contains(&ctx.env, self.node.typ()))?;
        }
        // Record this binding in the static-resolution index so a
        // `CallSite` whose `fnode` resolves to it can pre-bind in
        // `typecheck1`. Recording faux/inside-lambda binds is harmless:
        // lexical scoping means no outside `Ref` resolves to them, and
        // resolution never descends lambda bodies.
        if let Some(fv) = self.lambda_def_value() {
            self.pattern.ids(&mut |id| {
                if crate::dbgenv::gxdbg_resolve() {
                    eprintln!("B2L-INS {id:?} {}", self.spec);
                }
                ctx.bind_to_lambda.insert(id, fv.clone());
            });
        }
        Ok(())
    }

    fn typecheck1(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        wrap!(self.node, self.node.typecheck1(ctx))?;
        Ok(())
    }

    fn view(&self) -> NodeView<'_, R, E> {
        NodeView::Bind(self)
    }

    fn fuse(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<Option<Node<R, E>>> {
        // Fuse the bound VALUE, never the Bind itself: the Bind must
        // stay live to drive the publish of the result to its BindId
        // (the ValueBind splice shape). A whole-Bind fusion can't
        // happen anyway — Bind has no emit_clif, so any try_fuse
        // rooted here fails structurally.
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
    /// This occurrence's signature has been minted (see `typecheck0`).
    pub(crate) instantiated: bool,
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
    /// Construct a `Ref` node from its already-resolved components.
    /// AOT codegen uses this after name resolution has already
    /// assigned a BindId and a Type.
    ///
    /// Callers must ensure the runtime is told about the reference
    /// (via `ctx.rt.ref_var(id, top_id)`) separately — this
    /// constructor is a pure builder and does not touch ExecCtx.
    #[allow(dead_code)]
    pub fn new<R: Rt, E: UserEvent>(
        id: BindId,
        typ: Type,
        top_id: ExprId,
        spec: Expr,
    ) -> Node<R, E> {
        Node::new(Self {
            spec: Arc::new(spec),
            typ,
            id,
            top_id,
            resident: TagValue::phantom(),
            instantiated: false,
        })
    }

    pub(crate) fn compile<R: Rt, E: UserEvent>(
        ctx: &mut ExecCtx<R, E>,
        spec: Expr,
        scope: &Scope,
        top_id: ExprId,
        name: &ModPath,
    ) -> Result<Node<R, E>> {
        // `#bind::N` names binding N directly — the compiler's own
        // spelling for a synthesized reference (a trait call lowered to
        // a select over a union, `CallSite::lower_trait_union`); no
        // source can write it, `#` not being an identifier character
        if let Some(id) = synthesized_bind_ref(name) {
            let Some(bind) = ctx.env.by_id.get(&id) else {
                bailat!(spec, "synthesized reference to an unknown binding {id:?}")
            };
            let typ = bind.typ.clone();
            ctx.rt.ref_var(id, top_id);
            return Ok(Self::new(id, typ, top_id, spec));
        }
        let resolved = match ctx.env.lookup_bind(&scope.lexical, name) {
            Ok(r) => r,
            Err(e) => {
                return Err(e.context(expr::ErrorContext(spec.clone())));
            }
        };
        match resolved {
            None => bailat!(spec, "{name} not defined"),
            Some((_, bind)) => {
                let bind_id = bind.id;
                let typ = bind.typ.clone();
                let def_pos = bind.pos;
                let def_ori = bind.ori.clone();
                if ctx.env.lsp_mode {
                    ctx.env.push_reference(ReferenceSite {
                        pos: spec.pos,
                        ori: spec.ori.clone(),
                        name: name.clone(),
                        bind_id,
                        def_pos,
                        def_ori,
                    });
                }
                ctx.rt.ref_var(bind_id, top_id);
                let spec = Arc::new(spec);
                Ok(Node::new(Self {
                    spec,
                    typ,
                    id: bind_id,
                    top_id,
                    resident: TagValue::phantom(),
                    instantiated: false,
                }))
            }
        }
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Ref {
    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> &TagValue {
        // THE dense read (R2/R3): overlays first (a delivery's own tag
        // flows through), then the store — a standing entry is the
        // value channel, viewed Stale, or Fired under the reader's
        // REAL init view (frames force `event.init` for re-derivation,
        // so the framed read consults `frame_init`, the
        // const_stale_gate convention — a capture must not re-fire on
        // every framed pass). This read IS what every init backfill
        // used to synthesize. A store miss rides the resident (the
        // phantom until the first delivery ever).
        // GXDBG_REF=1 — print every Ref read's arm + tag (the tool
        // that found the aug13b double-read: one bind read twice in a
        // cycle flipping STALE→FIRED through the poisoned store twin).
        let dbg = crate::dbgenv::gxdbg_ref();
        let r = match super::read_var(ctx, event, &self.id) {
            Some(super::VarRead::Delivered(tv)) => {
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
            Some(super::VarRead::Standing(tv)) => {
                // Fresh under a GENUINE init view only. A WAKE-forced
                // view (becoming-selected — `event.wake_init`) reads
                // standing entries present-but-STALE (Eric's ruling
                // 2026-08-31): the standing value of an external bind
                // is a PAST event the rest of the graph already
                // consumed, and delivering it fired re-raised it into
                // the woken arm — the pump's name-modal Enter, still
                // standing when the Secret question woke the next arm,
                // phantom-submitted the fresh modal with "". The woken
                // arm still evaluates: values materialize through the
                // quiet-first-production rules, and firing comes only
                // from inputs that fired THIS cycle.
                let init = if ctx.frame_depth > 0 {
                    ctx.frame_init
                } else {
                    event.init && !event.wake_init
                };
                let tag = if init { tv.tag().fresh() } else { tv.tag().quiet() };
                let mut tv = tv.clone();
                tv.retag(tag);
                if dbg {
                    eprintln!(
                        "REF {} @{} {:?} STANDING init={init} tag={:?} val={:?}",
                        self.spec,
                        self.spec.pos,
                        self.id,
                        tag,
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
        refs.refed.insert(self.id);
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        ctx.rt.unref_var(self.id, self.top_id)
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {}

    fn reset_replay(&mut self, _ctx: &mut ExecCtx<R, E>) {}

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn typ(&self) -> &Type {
        &self.typ
    }

    /// A VALUE occurrence of a GENERALIZED binding (`Env::poly_binds`)
    /// is an instantiation site like a call: it mints fresh cells for
    /// the signature, so two uses of one polymorphic lambda
    /// (`array::map([1], f); array::map([1.5], f)`) do not pin each
    /// other through the definition's own cells. Minted at typecheck
    /// time — after the definition's gate has recorded the body's
    /// facts in those cells — and before anything reads this
    /// occurrence's type (`CallSite::typecheck0` typechecks a `Ref`
    /// argument ahead of its operand pre-bind). The exemptions are the
    /// call site's knots, where the definition's cells ARE the point:
    /// a self-reference inside the definition's gate (monomorphic
    /// recursion), a fn-typed parameter during its gate, and a
    /// reference to the instance being elaborated.
    fn typecheck0(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        if self.instantiated || !ctx.env.poly_binds.contains(&self.id) {
            return Ok(());
        }
        let Type::Fn(ft) = &self.typ else { return Ok(()) };
        self.instantiated = true;
        let rec_knot = !ctx.rec_defs.is_empty()
            && ft.lambda_ids.ids().iter().any(|id| ctx.rec_defs.contains(id));
        if rec_knot || ctx.def_gate_params.contains(&self.id) {
            return Ok(());
        }
        // A bare value reference has no arguments to key an
        // instantiation identity on: it refers to the innermost
        // instance of its def being elaborated.
        let active = ft
            .lambda_ids
            .own()
            .and_then(|id| ctx.resolving_innermost(id))
            .map(|a| a.ftype);
        let fresh = match active {
            Some(ft) => ft,
            None => {
                let fresh = ft.reset_tvars();
                fresh.alias_tvars(&mut LPooled::take());
                fresh
            }
        };
        self.typ = Type::Fn(Arc::new(fresh));
        Ok(())
    }

    fn typecheck1(&mut self, _ctx: &mut ExecCtx<R, E>) -> Result<()> {
        Ok(())
    }

    fn view(&self) -> NodeView<'_, R, E> {
        NodeView::Ref(self)
    }

    fn emit_clif(&self, cx: &mut BodyCx) -> Result<CompiledExpr> {
        emit_ref_node(cx, self.spec.as_ref(), &self.typ, self.id)
    }
}

#[derive(Debug)]
pub struct ByRef<R: Rt, E: UserEvent> {
    pub(crate) spec: Expr,
    pub typ: Type,
    pub child: Node<R, E>,
    pub id: BindId,
    resident: TagValue,
}

impl<R: Rt, E: UserEvent> ByRef<R, E> {
    /// Construct a `ByRef` node from an already-compiled child.
    /// AOT codegen supplies the `BindId` (allocated at codegen time,
    /// reused when the generated tree is built) and the resolved
    /// `Type`. Interpreter `compile` still handles the additional
    /// byref-chain plumbing it needs — generated code that wants
    /// ref-to-ref chaining must mirror that separately via
    /// `ctx.env.byref_chain.insert_cow(...)` before building the
    /// node.
    #[allow(dead_code)]
    pub fn new(id: BindId, typ: Type, child: Node<R, E>, spec: Expr) -> Node<R, E> {
        Node::new(Self { spec, typ, child, id, resident: TagValue::phantom() })
    }

    pub(crate) fn compile(
        ctx: &mut ExecCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: Expr,
        scope: &Scope,
        top_id: ExprId,
        expr: &Expr,
    ) -> Result<Node<R, E>> {
        let child = compile(ctx, flags, expr.clone(), scope, top_id)?;
        let id = BindId::new();
        if let Some(c) = (&*child as &dyn std::any::Any).downcast_ref::<Ref>() {
            ctx.env.byref_chain.insert_cow(id, c.id);
        }
        let typ = Type::ByRef(Arc::new(child.typ().clone()));
        Ok(Node::new(Self { spec, typ, child, id, resident: TagValue::phantom() }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for ByRef<R, E> {
    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> &TagValue {
        // Fired-only write gate: a stale refresh must not re-write the
        // referent, and a taint placeholder must never enter the
        // cross-cycle store.
        let tv = self.child.update(ctx, event);
        if tv.is_fired() {
            let v = tv.value_cloned();
            if event.init {
                // Seed the store WITHOUT a delivery (a STANDING write):
                // `Deref`'s init read serves it THIS cycle via the R2
                // store view; a queued write would arrive next cycle as
                // a duplicate (corpus-fuzz/divergence_000027; Eric's
                // ruling 2026-07-04: the JIT's single delivery is
                // correct).
                ctx.rt.store_insert_standing(self.id, TagValue::fired(v));
            } else {
                ctx.rt.set_var(self.id, v);
            }
        } else if event.init && !tv.tag().is_bottom() && !tv.tag().is_tainted() {
            // The wake-forced init view reads present-but-stale
            // (2026-08-31), but the cell must still MATERIALIZE:
            // embedders read it directly (a tui input_handler
            // compile_refs the handler cell — unseeded, the pump's
            // modal never received a key), and a chainless ref's cell
            // is its only storage. A standing STALE seed is the value
            // rule, not a firing one — the CachedArgs eval-once arm's
            // twin at the ByRef seam. (A never-produced child is the
            // STALE_BOTTOM phantom, excluded by the bottom gate.)
            ctx.rt.store_insert_standing(self.id, TagValue::stale(tv.value_cloned()));
        }
        if event.init {
            self.resident.set(TagValue::fired(Value::U64(self.id.inner())))
        } else {
            self.resident.ride()
        }
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        ctx.env.byref_chain.remove_cow(&self.id);
        self.child.delete(ctx)
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.child.sleep(ctx);
    }

    fn reset_replay(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.child.reset_replay(ctx);
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn typ(&self) -> &Type {
        &self.typ
    }

    fn refs(&self, refs: &mut Refs) {
        self.child.refs(refs)
    }

    fn typecheck0(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        wrap!(self.child, self.child.typecheck0(ctx))?;
        let t = Type::ByRef(Arc::new(self.child.typ().clone()));
        wrap!(self, self.typ.check_contains(&ctx.env, &t))
    }

    fn typecheck1(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        wrap!(self.child, self.child.typecheck1(ctx))?;
        Ok(())
    }

    fn view(&self) -> NodeView<'_, R, E> {
        NodeView::ByRef(self)
    }
}

#[derive(Debug)]
pub struct Deref<R: Rt, E: UserEvent> {
    pub(crate) spec: Expr,
    pub typ: Type,
    pub child: Node<R, E>,
    pub id: Option<BindId>,
    pub(super) top_id: ExprId,
    resident: TagValue,
}

impl<R: Rt, E: UserEvent> Deref<R, E> {
    /// Build a `Deref` node from an already-compiled child that
    /// evaluates to a `Value::U64` / `Value::V64` holding a BindId.
    /// AOT codegen passes the resolved type rather than leaving an
    /// empty type variable for the interpreter to pin down later.
    #[allow(dead_code)]
    pub fn new(typ: Type, child: Node<R, E>, top_id: ExprId, spec: Expr) -> Node<R, E> {
        Node::new(Self {
            spec,
            typ,
            child,
            id: None,
            top_id,
            resident: TagValue::phantom(),
        })
    }

    pub(crate) fn compile(
        ctx: &mut ExecCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: Expr,
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
            id: None,
            top_id,
            resident: TagValue::phantom(),
        }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Deref<R, E> {
    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> &TagValue {
        let tv = self.child.update(ctx, event);
        if !tv.tag().is_bottom() {
            let id = tv.with_value(|v| match v {
                Value::U64(i) | Value::V64(i) => Some(BindId::from(*i)),
                _ => None,
            });
            // Resolve the reference through the BYREF CHAIN, exactly as
            // the WRITE path does (`ConnectDeref`). `&x` mints its own
            // cell and mirrors x into it with a cross-cycle `set_var`,
            // so reading the cell reports x's PREVIOUS value on every
            // cycle x changes — `(x, *r)` produced `(7, 0)`. The write
            // path never had that problem because it resolved to the
            // referent, and the asymmetry was the whole bug: one side
            // of the chain was being followed and the other wasn't.
            // Interest is registered on the resolved id too, so the
            // deref wakes with the referent instead of a cycle behind
            // it. Chainless references (`&(a + b)`) have no entry and
            // keep reading their own cell, which is their only storage.
            let id = id.map(|id| ctx.env.byref_chain.get(&id).copied().unwrap_or(id));
            if let Some(new_id) = id {
                if self.id != Some(new_id) {
                    if let Some(old) = self.id {
                        ctx.rt.unref_var(old, self.top_id);
                    }
                    ctx.rt.ref_var(new_id, self.top_id);
                    self.id = Some(new_id);
                }
            }
        }
        let res = self.id.and_then(|id| match super::read_var(ctx, event, &id) {
            Some(super::VarRead::Delivered(tv)) => Some(tv.clone()),
            Some(super::VarRead::Standing(tv)) => {
                // Fresh under a genuine init view only — a wake-forced
                // view reads stale (see Ref::update above).
                let init = if ctx.frame_depth > 0 {
                    ctx.frame_init
                } else {
                    event.init && !event.wake_init
                };
                let tag = if init { tv.tag().fresh() } else { tv.tag().quiet() };
                let mut c = tv.clone();
                c.retag(tag);
                Some(c)
            }
            None => None,
        });
        match res {
            Some(tv) => self.resident.set(tv),
            None => self.resident.ride(),
        }
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        if let Some(id) = self.id.take() {
            ctx.rt.unref_var(id, self.top_id);
        }
        self.child.delete(ctx);
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.child.sleep(ctx);
    }

    fn reset_replay(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.child.reset_replay(ctx);
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn typ(&self) -> &Type {
        &self.typ
    }

    fn refs(&self, refs: &mut Refs) {
        self.child.refs(refs);
        if let Some(id) = self.id {
            refs.refed.insert(id);
        }
    }

    fn typecheck0(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        wrap!(self.child, self.child.typecheck0(ctx))?;
        // Deref TVars before matching: a container/accessor read's
        // type is a TVar BOUND to `&T`, not a bare `Type::ByRef` —
        // `*(a[0]$)` over `Array<&i64>` was rejected here while the
        // runtime handles it by construction (a ref VALUE is
        // `Value::U64(bind_id)` wherever it came from; `update`
        // re-registers lazily off the value). The structural match
        // made container-stored refs a compile error for no semantic
        // reason (2026-07-08).
        let typ = self.child.typ().with_deref(|t| match t {
            Some(Type::ByRef(t)) => Some((**t).clone()),
            _ => None,
        });
        let typ = match typ {
            Some(t) => t,
            None => bail!("expected reference"),
        };
        wrap!(self, self.typ.check_contains(&ctx.env, &typ))?;
        Ok(())
    }

    fn typecheck1(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        wrap!(self.child, self.child.typecheck1(ctx))?;
        Ok(())
    }

    fn view(&self) -> NodeView<'_, R, E> {
        NodeView::Deref(self)
    }
}
