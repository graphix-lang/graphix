use super::{
    VarRead, WakeBit,
    coretraits::with_hooks,
    data::{struct_field_type, tuple_field_type},
    lambda::{DefBody, DefOrigin, LambdaDef},
    pattern::StructPatternNode,
    place::{self, Path},
    read_var, standing_view,
};
use crate::{
    BindId, BuiltinBindInfo, CFlag, CompileCtx, ExecCtx, Node, NodeView, PrintFlag, Refs,
    Rt, Scope, Tag, TagValue, Update, UserEvent, bailat,
    compiler::compile,
    dbgenv,
    env::Env,
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
        tvar::{AtLevel, Level},
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

#[derive(Debug)]
pub struct Bind<R: Rt, E: UserEvent> {
    slept: WakeBit,
    /// The last cycle its readers were walked before it slept: a write
    /// landing after it is a fire no reader saw, re-raised at the wake.
    slept_at: u64,
    pub(crate) spec: Arc<Expr>,
    pub(crate) typ: Type,
    pub(crate) pattern: StructPatternNode,
    /// The predicate of a pattern that leaves struct fields out, over a
    /// value whose type the compile did not know: completed against the
    /// value at the check, as a select arm's against its scrutinee.
    partial: Option<Type>,
    pub(crate) node: Node<R, E>,
    ever_published: bool,
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
            partial: None,
            ever_published: false,
            slept: WakeBit::default(),
            slept_at: 0,
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
    /// sleep, a seq machine's reset) was seen by no reader, all of which
    /// slept with it: deliver it fired, once, at the wake.
    // CR claude for eric: [bug/medium] The re-raise is published to every reader of the
    // binding (`ctx.publish`), but not every reader slept with it. A `&mut x` that
    // escapes the arm lets a reader outside read the write live through its Deref. At
    // the wake that reader gets the same write again as a phantom fire, and `count(*h)`
    // counts one write twice. Scope the re-raise to the readers that slept: inject it
    // for the woken region's evaluation and restore it afterwards, as
    // TrackedFires::deliver does, or skip it for a binding a reference names. Which one
    // depends on the escaped-reference rule, so it is your call. probe:
    // design/review-2026-10-19/repro/c-bind-reraise-escaped-ref.gx (c-bind-02)
    fn reraise_writes_while_asleep(&self, ctx: &mut ExecCtx<'_, R, E>) {
        let slept_at = self.slept_at;
        let mut late: SmallVec<[(BindId, TagValue); 2]> = SmallVec::new();
        self.pattern.ids(&mut |id| {
            if !ctx.event.variables.contains_key(&id)
                && let Some((tv, stamp)) = ctx.rt.store_get(&id)
                // XCR claude for claude: [bug/high] A write queued in the arm's last
                // awake cycle lands in the cycle the arm is deselected, so its stamp
                // equals `slept_at` (both are `rt.cycle()` then) and it is never
                // re-raised. That is the case this function's doc names. The value
                // stands, but every fire-driven reader in the arm (`out <- s`,
                // `count(s)`, `t ~ s`) misses it for good: in the probe `out` stays 10
                // while `s` reads 11 and 12, on both engines. Only a write landing a
                // cycle after the sleep is re-raised. Fix: compare against the cycle of
                // the binding's last update, not of its sleep. A plain `>=` would
                // re-raise a write a seq step saw and then slept on in the same cycle.
                // Then make `select_arm_local_persists` read `s` through a
                // fire-sensitive reader. probe:
                // design/review-2026-10-19/repro/tests-select-reraise-last-awake-write.gx
                // Also reported as (c-node-mod-01), (c-prune-wake-02).
                // (tests-select-01)
                // 2026-10-19 claude: Fixed with c-bind-01 (same mechanism); pin
                // lang::prune::write_landing_at_the_deselect_is_reraised reads the
                // re-raised write through count, a fire-sensitive reader.
                && *stamp > slept_at
                && !tv.tag().is_bottom()
            {
                late.push((id, TagValue::fired(tv.value_cloned())))
            }
        });
        for (id, tv) in late {
            ctx.publish(id, tv)
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
            // CR claude for eric: [bug/medium] The parser makes a separate cell for
            // every `'a` in a type, and only `typexp::fntype` and `Lambda::compile`
            // alias same-named ones. This annotation never goes through
            // `Type::alias_tvars`, so its same-named variables are tied together only
            // inside one fn type. `let w: ('a, 'a) = (1, "s")`, `let s: {x: 'b, y: 'b}
            // = {x: 1, y: "s"}` and `let p: ('c, fn(x: 'c) -> 'c) = (1, |x: string| x)`
            // are all accepted. Inside a lambda, `let u: ('d, 'd) = (x, y)` relates x
            // to nothing. The pin `annotation_variable_is_one_variable` states the rule
            // but covers only a bare fn-type annotation. Fix: call `alias_tvars` once
            // over the scoped annotation here, and likewise over the type argument of
            // `never<T>`. A separate decision is needed: a body annotation's `'e` is
            // currently a fresh variable, not the enclosing definition's rigid
            // quantifier, so `'e: Number |x: 'e| -> 'e { let v: 'e = 1; x }` is
            // accepted. Either seed `known` with the enclosing definition's declared
            // quantifiers, or refuse the shadowing name. probe:
            // design/review-2026-10-12/repro/t-tvar-04.gx (t-tvar-04)
            Some(t) => Some(t.scope_refs(&scope.lexical).rewrite_trait_args(&ctx.env)?),
            None => None,
        };
        let compile_pattern = |ctx: &mut CompileCtx<R, E>, typ: &Type| {
            StructPatternNode::compile(ctx, typ, pat, scope, spec.pos, spec.ori.clone())
                .at(&spec)
        };
        let mut partial = None;
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
                    // CR claude for claude: [bug/medium] A let's shape check still uses
                    // ptyp.any_as_tvar().contains(typ), which binds the pattern's cells
                    // to the union's first member. So `let (x, y) = v` over [(i64,
                    // string), (string, i64)] is refused with "refutable patterns are
                    // not allowed in let: (i64, string) matches only part of ...". The
                    // pattern covers both members, and the same pattern as a select arm
                    // is accepted and runs ("1 s", "s 1"). The same happens for a
                    // struct let over [{x: i64, y: string}, {x: string, y: i64}]. The
                    // x-typecheck-patterns-02 fix narrowed select arms with
                    // Type::narrow_pattern; the let needs the same narrowing before it
                    // compiles its pattern, refusing only what pattern.covers calls
                    // refutable. probe:
                    // design/review-2026-10-19/repro/x-rerun-6-let-union-destructure.gx
                    // (x-rerun-6-02)
                    if !completed && pat.has_partial_struct() {
                        partial = Some(ptyp);
                    // CR claude for claude: [perf/low] For a single-name pattern `ptyp`
                    // is a fresh cell, so this check cannot fail, but binding the cell
                    // runs `TVar::would_cycle` over the whole value type. Each let in a
                    // chain that wraps the last value in a struct, tuple or variant
                    // therefore walks everything before it: --check costs 0.85 s at
                    // 5000 lets, 4.2 s at 10000 and 17.9 s at 20000, all in BuildGraph,
                    // while the same chain with `[x]` takes 0.75 s at 20000. Fix: skip
                    // this check for a bare `StructurePattern::Bind` (or skip the
                    // occurs check for a cell created after the type), and add these
                    // shapes to `flat_type_chain_checks_in_linear_time`, which pins
                    // arrays only. probe:
                    // design/review-2026-10-19/repro/x-stack-flat-chain-quadratic.sh
                    // (x-stack-02)
                    } else if !ptyp.any_as_tvar().contains(&ctx.env, &typ).at(&spec)? {
                        let part = ptyp.could_match(&ctx.env, &typ)?;
                        format_with_flags(PrintFlag::DerefTVars, || match part {
                            true => bailat!(
                                spec,
                                "refutable patterns are not allowed in let: {ptyp} \
                                 matches only part of {typ}"
                            ),
                            false => {
                                bailat!(
                                    spec,
                                    "match error {typ} can't be matched by {ptyp}"
                                )
                            }
                        })?
                    }
                    typ
                }
            };
            let pattern = compile_pattern(ctx, partial.as_ref().unwrap_or(&typ))?;
            (node, pattern, typ)
        };
        if partial.is_none() && !pattern.covers(&ctx.env, &typ, false) {
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
            targets.into_iter().for_each(|id| ctx.mark_connect_target(id));
        }
        // a destructuring let's siblings fire together, facets of one input,
        // but for one something writes, which fires alone, its own facet
        let mut siblings: SmallVec<[BindId; 4]> = SmallVec::new();
        pattern.ids(&mut |id| siblings.push(id));
        // XCR claude for claude: [bug/high] The facet grouping is decided here from the
        // targets known when the let compiles. A sibling first written from another
        // module (`m::a <- ..` or `&mut m::a`: the module compiles before the
        // importer's writers) or by a later REPL input stays grouped with its siblings.
        // TrackedFires::deliver then injects a catch-up fire of every grouped sibling,
        // so each write of `a` fires `b` in any select arm that reads both. The probe's
        // count(m::b) counts every write of m::a. Fix: have mark_connect_target split a
        // written binding off its Facet::Let group, promoting a new representative when
        // needed (record the group's members with the facet). Deciding the facets in
        // typecheck0 instead covers modules but not the REPL. probe:
        // design/review-2026-10-19/repro/c-bind-facet-module/main.gx (c-bind-03)
        // 2026-10-19 claude: CompileCtx::mark_connect_target calls Env::split_facet: a
        // sibling written after its let compiled (from another module, a later REPL
        // input) leaves its Facet::Let group, and a representative leaving passes the
        // group to the next sibling. The repro prints count(m::b) = 1 throughout. Pin:
        // lang::modules::sibling_written_from_outside_its_module_fires_alone. Needs a
        // soak (wake catch-up).
        if siblings.len() > 1 {
            let rep = siblings.iter().find(|id| !ctx.connect_targets.contains(*id));
            let rep = rep.copied();
            for id in siblings {
                match rep {
                    Some(rep) if !ctx.connect_targets.contains(&id) => {
                        ctx.env.mark_facet(id, rep)
                    }
                    _ => ctx.env.mark_facet(id, id),
                }
            }
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
            partial,
            ever_published: false,
            slept: WakeBit::default(),
            slept_at: 0,
        }))
    }

    /// Complete a [`Self::partial`] pattern against the value's type,
    /// which must be known by now.
    fn complete_partial(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
        let Some(ptyp) = self.partial.take() else { return Ok(()) };
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
    fn aliased_lambda(&self, ctx: &CompileCtx<R, E>) -> Option<Value> {
        let mut view = self.node.view();
        while let NodeView::ExplicitParens(p) = view {
            view = p.n.view();
        }
        match view {
            NodeView::Ref(r) if !ctx.batch_connect_targets.contains(&r.id) => {
                ctx.bind_to_lambda.get(&r.id).cloned()
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
            partial: None,
            ever_published: false,
            slept: WakeBit::default(),
            slept_at: 0,
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
            self.reraise_writes_while_asleep(ctx);
        }
        let tv = self.node.update(ctx);
        let tag = tv.tag();
        // At a wake a `<-` target holding a value keeps it unless the
        // initializer fired; otherwise a production publishes when it
        // fires, first, or quietly after a sleep.
        let mut held: SmallVec<[BindId; 4]> = SmallVec::new();
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
            let quiet = !tag.triggers();
            if tag.is_bottom() {
                self.pattern.ids(&mut |id| {
                    if held.contains(&id) {
                        return;
                    }
                    ctx.event.variables.insert(id, TagValue::tagged(Value::Null, tag));
                    ctx.rt.store_insert(
                        id,
                        TagValue::tagged(Value::Null, Tag::FRESH_BOTTOM),
                    );
                    if !quiet {
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
                    ctx.rt.store_insert(id, TagValue::fired(v));
                    if !quiet {
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
        // XCR claude for claude: [bug/high] slept_at is the wrong reference point for
        // the re-raise. (a) A `<-` write queued in the arm's last awake cycle lands in
        // the cycle the select deselects the arm. The select does not walk the arm in
        // that cycle, but the write's stamp equals slept_at, so `stamp > slept_at`
        // fails and the write is never re-raised. design/wake_catchup.md cites this
        // case as the one the re-raise covers; only the seq pc reset, which lands a
        // cycle after sleep(), passes. (b) Select::sleep sleeps already-sleeping arms
        // again, so an enclosing deselect moves slept_at past a write that landed in
        // between, and the write is dropped. Fix: record the last cycle the binding's
        // region was walked (the previous cycle for a select deselect, the current one
        // for a seq step that ran), and set it only on the first sleep (when `slept` is
        // clear). probe: design/review-2026-10-19/repro/c-bind-reraise-same-cycle.gx
        // and design/review-2026-10-19/repro/c-bind-reraise-resleep.gx (c-bind-01)
        // 2026-10-19 claude: Bind::sleep takes slept_at only at its first sleep (a
        // re-sleep keeps it), and takes the cycle its readers were last walked: the
        // sleep cycle when the let's innermost boundary was visited in it (a seq step
        // that ran), else the cycle before (a deselect). prune::Boundary records each
        // visit (BoundaryRef::visit, set by every Prune::update, skipped or not). Pins:
        // lang::prune::write_landing_at_the_deselect_is_reraised,
        // second_sleep_keeps_the_first_sleeps_cycle (both fail on the review binary).
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
            if let Some(Type::Abstract { .. }) = self.node.typ().deref_cloned() {
                wrap!(
                    self.node,
                    Type::pre_unify_arg(&ctx.env, &self.typ, self.node.typ())
                )?;
            }
        }
        wrap!(self.node, self.node.typecheck0(ctx))?;
        self.check_value(ctx)?;
        self.complete_partial(ctx)?;
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
        self.complete_partial(ctx)?;
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
                    && let Some(eta) =
                        super::compiler::lowering(ctx, &spec, &scope.lexical, |ctx| {
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
                        name: name.clone(),
                        bind_id,
                    });
                }
                Ok(Self::with_signature(ctx, bind_id, typ, signature, top_id, spec))
            }
        }
    }
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
        if rec_knot || ctx.def_gate_params.contains(&self.id) {
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
    /// without the access's failure (a place handles that at runtime).
    fn elem_type(&self, ctx: &mut CompileCtx<R, E>, spec: &Expr) -> Result<Type> {
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
                PlaceStep::Tuple(i) => tuple_field_type(ctx, &self.scope, &cur, *i)?,
                PlaceStep::Field(name) => {
                    let (_, t) = struct_field_type(ctx, &cur, &name.name)?;
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
    registered: Option<(BindId, Path)>,
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
                if ctx.generalized_earlier(r.id) {
                    bailat!(
                        spec,
                        "{} was made generic by an earlier compile, and a writable \
                         reference to it would let a write narrow it: write it with <-",
                        r.spec
                    );
                }
                ctx.mark_connect_target(r.id);
            }
        }
        Ok(Node::new(Self {
            spec,
            typ,
            referent,
            id,
            resident: TagValue::phantom(),
            registered: None,
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

    /// Drop the place registration; was there one?
    fn unregister(&mut self, ctx: &mut ExecCtx<'_, R, E>) -> bool {
        let was = self.registered.take().is_some();
        if was {
            ctx.rt.clear_ref_path(&self.id);
        }
        was
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
        Some((root, path)) => (*root, &path[..]),
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
                    let lost = self.unregister(ctx);
                    if lost {
                        self.bottom_mirror(ctx);
                    }
                    return self.resident.set_bottom(lost || ctx.event.born());
                };
                let same = self
                    .registered
                    .as_ref()
                    .is_some_and(|(r, p)| *r == bind && p == path);
                let moved = moved || !same;
                if !same {
                    ctx.rt.set_ref_path(self.id, bind, path.clone());
                    self.registered = Some((bind, path.clone()));
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
            self.resident.set(TagValue::fired(crate::reference::reference(self.id)))
        } else {
            self.resident.ride()
        }
    }

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        // CR claude for eric: [bug/medium] c-bind-11 variant: a reference outliving
        // its `&` reads bottom but still writes its referent. ByRef::delete now removes
        // the cell's store entry and its byref_chain link (bind.rs), which fixes the
        // leak. But a reference value that escaped the deleted site (kept in an outer
        // variable) now behaves two ways at once. Deref resolves through ref_target on
        // every read, finds no chain and no store entry, and reads bottom, although the
        // referent `x` is alive. A ConnectDeref that had already resolved its target
        // (`target: Option<WriteTarget>`, node/mod.rs) keeps writing `x`. A reference
        // minted after the deletion would resolve to the dead cell and lose its writes
        // silently. CLAUDE.md says a reference names the place or the byref chain's
        // binding; nothing documents a reference dying with the expression that minted
        // it. Failure: In the repro_sketch, after the slot is deleted (c = 3), the
        // tuple goes bottom for good: `c ~ *keep` never produces again. Meanwhile
        // `*keep <- c ~ c * 1000` keeps writing x (x reaches 7000). Reading through the
        // reference says nothing while writing through it works. Fix: Decide the
        // lifetime rule and apply it to both paths. Either a reference to a binding
        // stays valid while the binding lives: keep the chain link (or encode the
        // target in the value) and let Deref read the referent. Or the reference dies
        // with its minting site: then ConnectDeref must drop its cached target too.
        // Document the rule in design/place_references.md and pin it. Unverified: the
        // reviewer's report, not checked by a verifier. (x-rerun-1-06)
        // 2026-10-15 claude: Since ConnectDeref resolves its target at each write, the
        // two paths agree: a reference kept past its minting site's deletion reads and
        // writes its own dead cell, so x stops changing and *keep reads back what was
        // written to the cell (probe: design/review-2026-10-12/repro/x-rerun-1-06.gx,
        // both engines). The cell's store entry is recreated by those writes, one per
        // dead reference written. What remains is the rule: a reference to a binding
        // valid while the binding lives (needs the chain link kept, or the target in
        // the value), or a reference that dies with its site (reads bottom, writes
        // dropped, which needs a liveness test a same-cycle birth cannot fail). That is
        // a semantics choice, so it goes to Eric.
        ctx.env.byref_chain.remove(&self.id);
        ctx.rt.store_release(&self.id);
        self.unregister(ctx);
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
    /// Another binding, or the first.
    Target,
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
            (_, p) => {
                self.release(ctx);
                ctx.ref_var(id, self.top_id);
                if cell != id {
                    ctx.ref_var(cell, self.top_id);
                }
                self.addr = Some(Addr { cell, id, path: p.unwrap_or_default() });
                Moved::Target
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
        let res = match read_var(ctx, &id) {
            Some(VarRead::Delivered(tv)) => Some(tv.clone()),
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
                // a move of the path is a new element, read fired
                let t = match moved {
                    // at a wake, catch-up decides through the reference's fire
                    Moved::Path if !ctx.event.wake() => tv.tag().join(addr).fresh(),
                    Moved::Path | Moved::No | Moved::Target => tv.tag().join(addr),
                };
                tv.retag(t);
                self.resident.set(tv)
            }
            // a moved address with nothing to read is bottom
            None if moved != Moved::No => {
                let path = moved == Moved::Path && !ctx.event.wake();
                self.resident.set_bottom(addr.triggers() || path)
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
                wrap!(self, p.elem_type(ctx, &self.spec))?
            }
        };
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
