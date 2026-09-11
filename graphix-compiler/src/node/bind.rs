use super::{
    WakeBit, collection::CollectionIntrinsic, pattern::StructPatternNode, place,
};
use crate::image::nodes::{NodeTag, decode_node, put_tag, tag_len};
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
use bytes::{Buf, BufMut, BytesMut};
use enumflags2::BitFlags;
use netidx_core::pack::{Pack, PackError};
use netidx_core::pack::{decode_varint, encode_varint, varint_len};
use netidx_value::{Typ, Value};
use poolshark::local::LPooled;
use triomphe::Arc;

#[derive(Debug)]
pub struct Bind<R: Rt, E: UserEvent> {
    slept: WakeBit,
    pub(crate) spec: Expr,
    pub(crate) typ: Type,
    pub(crate) pattern: StructPatternNode,
    pub(crate) node: Node<R, E>,
    ever_published: bool,
}

/// Rewrite a node into a block: each already-compiled operand becomes a
/// `let <name> = <node>` binding (moved, never recompiled) and `body`,
/// an expression over those names compiled under `scope`, is the value.
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
            ever_published: false,
            slept: WakeBit::default(),
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
    /// The single `BindId` this binding introduces when the pattern
    /// binds exactly one name; `None` for destructuring patterns.
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
                    // A ⊥ initializer seeds a fresh cell its writers
                    // refine, settled to ⊥ in typecheck1 if none do.
                    let typ =
                        if node.typ().with_deref(|t| matches!(t, Some(Type::Bottom))) {
                            Type::empty_tvar()
                        } else {
                            node.typ().clone()
                        };
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
        let mut siblings: smallvec::SmallVec<[BindId; 4]> = smallvec::SmallVec::new();
        pattern.ids(&mut |id| siblings.push(id));
        if let Some(&rep) = siblings.first()
            && siblings.len() > 1
        {
            for id in siblings {
                ctx.env.mark_facet(id, rep);
            }
        }
        // Registered after the value compiled so a `let rec` body's
        // self-references keep the definition's cells.
        if matches!(node.view(), NodeView::Lambda(_)) {
            pattern.ids(&mut |id| {
                ctx.env.poly_binds.insert_cow(id);
            });
        }
        // Keyed by (scope, name), not BindId: sig and impl get
        // different ids for one builtin binding.
        if let ExprKind::Bind(be) = &spec.kind {
            if let expr::StructurePattern::Bind(bind_name) = &be.pattern {
                if let ExprKind::Lambda(lam) = &value.kind {
                    if let netidx_core::utils::Either::Right(builtin_name) = &lam.body {
                        if CollectionIntrinsic::from_name(builtin_name).is_none()
                            && let Type::Fn(fn_type) = node.typ()
                        {
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
        Ok(Node::new(Self {
            spec,
            typ,
            pattern,
            node,
            ever_published: false,
            slept: WakeBit::default(),
        }))
    }

    /// The LambdaDef `Value` this binding holds when its value node is
    /// a lambda; `None` otherwise.
    pub(crate) fn lambda_def_value(&self) -> Option<Value> {
        match self.node.view() {
            NodeView::Lambda(l) => Some(l.def_value().clone()),
            _ => None,
        }
    }

    /// The id if this bind has exactly one binding, otherwise `None`.
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

impl<R: Rt, E: UserEvent> Bind<R, E> {
    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<R, E>,
        buf: &mut &[u8],
    ) -> Result<Node<R, E>, PackError> {
        let spec = Expr::decode(buf)?;
        let typ = Type::decode(buf)?;
        let pattern = StructPatternNode::decode(buf)?;
        let node = decode_node(ctx, buf)?;
        Ok(Node::new(Self {
            spec,
            typ,
            pattern,
            node,
            ever_published: false,
            slept: WakeBit::default(),
        }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Bind<R, E> {
    fn image_len(&self) -> usize {
        tag_len()
            + self.spec.encoded_len()
            + self.typ.encoded_len()
            + self.pattern.encoded_len()
            + self.node.image_len()
    }

    fn image_encode(&self, buf: &mut BytesMut) -> Result<(), PackError> {
        put_tag(NodeTag::Bind, buf);
        self.spec.encode(buf)?;
        self.typ.encode(buf)?;
        self.pattern.encode(buf)?;
        self.node.image_encode(buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> &TagValue {
        let woke = self.slept.take() && ctx.frame_depth == 0;
        let tv = self.node.update(ctx, event);
        let tag = tv.tag();
        // A stale RHS is already served by the store, except before the
        // first publish, which goes out whatever its tag. A fresh bottom
        // persists in the store.
        let keep_connect_target_value = event.wake_init && self.ever_published && {
            let mut target = false;
            self.pattern.ids(&mut |id| {
                target = target || ctx.connect_targets.contains(&id);
            });
            target
        };
        if crate::dbgenv::gxdbg_letbind() {
            eprintln!(
                "LETBIND {} tag={tag:?} val={:?} ever_published={} fd={} keep_connect_target_value={keep_connect_target_value} publishing={}",
                self.spec.pos,
                tv.value_cloned(),
                self.ever_published,
                ctx.frame_depth,
                !keep_connect_target_value
                    && (tag.triggers() || (!self.ever_published && !tag.is_bottom()))
            );
        }
        // After a sleep the store entry may lag a stale recompute;
        // re-publish quietly (design/wake_catchup.md).
        let wake_refresh =
            woke && !tag.triggers() && !tag.is_bottom() && !keep_connect_target_value;
        if !keep_connect_target_value
            && (tag.triggers()
                || (!self.ever_published && !tag.is_bottom())
                || wake_refresh)
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

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        // The static-resolution index outlives batches; a deleted
        // bind's entry must go with it.
        self.pattern.ids(&mut |id| {
            ctx.bind_to_lambda.remove(&id);
            ctx.connect_targets.remove(&id);
        });
        self.node.delete(ctx);
        self.pattern.delete(ctx);
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.slept.set();
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
        // `let g = f` with `f` generalized and no annotation: `g` shares
        // `f`'s scheme; unifying with a fresh instance would pin it.
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
        if let Type::TVar(tv) = &self.typ
            && self.node.typ().with_deref(|t| matches!(t, Some(Type::Bottom)))
        {
            tv.settle_or_bottom(&ctx.env)?;
        }
        Ok(())
    }

    fn view(&self) -> NodeView<'_, R, E> {
        NodeView::Bind(self)
    }

    fn fuse(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<Option<Node<R, E>>> {
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
    /// Construct a `Ref` from resolved components. Does not touch the
    /// ExecCtx: the caller must register the reference with
    /// `ctx.rt.ref_var(id, top_id)`.
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
        // `#bind::N` is the compiler's spelling for a synthesized
        // reference; no source can write it.
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

impl Ref {
    /// Replays the reference registration `compile` made with the runtime.
    pub(crate) fn image_decode<R: Rt, E: UserEvent>(
        ctx: &mut ExecCtx<R, E>,
        buf: &mut &[u8],
    ) -> Result<Node<R, E>, PackError> {
        let spec = Arc::new(Expr::decode(buf)?);
        let typ = Type::decode(buf)?;
        let id = BindId::decode(buf)?;
        let top_id = ExprId::decode(buf)?;
        ctx.rt.ref_var(id, top_id);
        Ok(Node::new(Self {
            spec,
            typ,
            id,
            top_id,
            resident: TagValue::phantom(),
            instantiated: false,
        }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Ref {
    fn image_len(&self) -> usize {
        tag_len()
            + self.spec.encoded_len()
            + self.typ.encoded_len()
            + self.id.encoded_len()
            + self.top_id.encoded_len()
    }

    fn image_encode(&self, buf: &mut BytesMut) -> Result<(), PackError> {
        put_tag(NodeTag::Ref, buf);
        self.spec.encode(buf)?;
        self.typ.encode(buf)?;
        self.id.encode(buf)?;
        self.top_id.encode(buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> &TagValue {
        // Overlays first, then the store; a store miss rides the resident.
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
                // Fresh under a genuine init view only: a wake-forced
                // view reads a standing entry stale, since its value is
                // a past event the graph already consumed. Frames force
                // `event.init`, so a framed read consults `dispatch_init`.
                let init = if ctx.frame_depth > 0 {
                    ctx.dispatch_init
                } else {
                    event.init && !event.wake_init
                };
                let tag = if init { tv.tag().fresh() } else { tv.tag().quiet() };
                let mut tv = tv.clone();
                tv.retag(tag);
                if dbg {
                    eprintln!(
                        "REF {} @{} {:?} STANDING init={init} (ei={} wi={} fd={} fi={}) tag={:?} val={:?}",
                        self.spec,
                        self.spec.pos,
                        self.id,
                        event.init,
                        event.wake_init,
                        ctx.frame_depth,
                        ctx.dispatch_init,
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

    /// A value occurrence of a generalized binding (`Env::poly_binds`)
    /// mints fresh cells for its signature, like a call. Exempt: a
    /// self-reference inside the definition's gate, a fn-typed
    /// parameter during its gate, and a reference to the instance
    /// being elaborated, which must share the definition's cells.
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
        // A bare value reference has no arguments to key an instance
        // identity on; it takes the innermost active instance.
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

/// One accessor of a place reference's path, with its key expression
/// where the key is dynamic.
#[derive(Debug)]
enum PlaceStep<R: Rt, E: UserEvent> {
    Index(Node<R, E>),
    Tuple(usize),
    Field(ArcStr),
    Key(Node<R, E>),
}

/// The place a `&root[i].f` reference stands for: the root node (a
/// variable reference or a dereference) and the path's steps
/// (design/place_references.md).
#[derive(Debug)]
struct Place<R: Rt, E: UserEvent> {
    root: Node<R, E>,
    steps: Vec<PlaceStep<R, E>>,
}

impl<R: Rt, E: UserEvent> PlaceStep<R, E> {
    fn image_len(&self) -> usize {
        1 + match self {
            PlaceStep::Index(n) | PlaceStep::Key(n) => n.image_len(),
            PlaceStep::Tuple(i) => i.encoded_len(),
            PlaceStep::Field(f) => f.encoded_len(),
        }
    }

    fn image_encode(&self, buf: &mut BytesMut) -> Result<(), PackError> {
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

    fn image_decode(ctx: &mut ExecCtx<R, E>, buf: &mut &[u8]) -> Result<Self, PackError> {
        if !buf.has_remaining() {
            return Err(PackError::BufferShort);
        }
        match buf.get_u8() {
            0 => Ok(PlaceStep::Index(decode_node(ctx, buf)?)),
            1 => Ok(PlaceStep::Tuple(usize::decode(buf)?)),
            2 => Ok(PlaceStep::Field(ArcStr::decode(buf)?)),
            3 => Ok(PlaceStep::Key(decode_node(ctx, buf)?)),
            _ => Err(PackError::UnknownTag),
        }
    }
}

impl<R: Rt, E: UserEvent> Place<R, E> {
    fn image_len(place: &Option<Self>) -> usize {
        1 + place.as_ref().map_or(0, |p| {
            p.root.image_len()
                + varint_len(p.steps.len() as u64)
                + p.steps.iter().map(|s| s.image_len()).sum::<usize>()
        })
    }

    fn image_encode(place: &Option<Self>, buf: &mut BytesMut) -> Result<(), PackError> {
        let Some(p) = place else { return Ok(buf.put_u8(0)) };
        buf.put_u8(1);
        p.root.image_encode(buf)?;
        encode_varint(p.steps.len() as u64, buf);
        for s in &p.steps {
            s.image_encode(buf)?;
        }
        Ok(())
    }

    fn image_decode(
        ctx: &mut ExecCtx<R, E>,
        buf: &mut &[u8],
    ) -> Result<Option<Self>, PackError> {
        if !buf.has_remaining() {
            return Err(PackError::BufferShort);
        }
        match buf.get_u8() {
            0 => Ok(None),
            1 => {
                let root = decode_node(ctx, buf)?;
                let n = decode_varint(buf)? as usize;
                let mut steps = Vec::with_capacity(n);
                for _ in 0..n {
                    steps.push(PlaceStep::image_decode(ctx, buf)?);
                }
                Ok(Some(Place { root, steps }))
            }
            _ => Err(PackError::UnknownTag),
        }
    }
}

enum PlaceSpec {
    Index(Expr),
    Tuple(usize),
    Field(ArcStr),
    Key(Expr),
}

impl<R: Rt, E: UserEvent> Place<R, E> {
    /// The accessor chain of `expr` down to a variable or a
    /// dereference, root first; `None` for anything else.
    fn of(expr: &Expr) -> Option<(Expr, Vec<PlaceSpec>)> {
        let mut steps = vec![];
        let mut cur = expr;
        loop {
            match &cur.kind {
                ExprKind::ArrayRef { source, i } => {
                    steps.push(PlaceSpec::Index((**i).clone()));
                    cur = source;
                }
                ExprKind::TupleRef { source, field } => {
                    steps.push(PlaceSpec::Tuple(*field));
                    cur = source;
                }
                ExprKind::StructRef { source, field } => {
                    steps.push(PlaceSpec::Field(field.clone()));
                    cur = source;
                }
                ExprKind::MapRef { source, key } => {
                    steps.push(PlaceSpec::Key((**key).clone()));
                    cur = source;
                }
                ExprKind::Ref { .. } | ExprKind::Deref(_) if !steps.is_empty() => {
                    steps.reverse();
                    return Some((cur.clone(), steps));
                }
                _ => return None,
            }
        }
    }

    fn root_id(&self) -> Option<BindId> {
        let any = &*self.root as &dyn std::any::Any;
        match any.downcast_ref::<Ref>() {
            Some(r) => Some(r.id),
            None => any.downcast_ref::<Deref<R, E>>().and_then(|d| d.id),
        }
    }

    /// Update the root and the keys: the current path (`None` while a
    /// key is undetermined) and whether anything moved.
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        event: &mut Event<E>,
    ) -> (Option<(BindId, place::Path)>, bool) {
        let mut moved = self.root.update(ctx, event).is_fired();
        let mut path = place::Path::new();
        let mut complete = true;
        for step in &mut self.steps {
            match step {
                PlaceStep::Index(n) => {
                    let tv = n.update(ctx, event);
                    moved |= tv.is_fired();
                    let i = if tv.tag().is_bottom() {
                        None
                    } else {
                        tv.with_value(|v| v.clone().cast_to::<i64>().ok())
                    };
                    match i {
                        Some(i) => path.push(place::Step::Index(i)),
                        None => complete = false,
                    }
                }
                PlaceStep::Tuple(i) => path.push(place::Step::Index(*i as i64)),
                PlaceStep::Field(name) => path.push(place::Step::Field(name.clone())),
                PlaceStep::Key(n) => {
                    let tv = n.update(ctx, event);
                    moved |= tv.is_fired();
                    if tv.tag().is_bottom() {
                        complete = false
                    } else {
                        path.push(place::Step::Key(tv.value_cloned()))
                    }
                }
            }
        }
        match (complete, self.root_id()) {
            (true, Some(root)) => (Some((root, path)), moved),
            _ => (None, moved),
        }
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

    fn refs(&self, refs: &mut Refs) {
        self.root.refs(refs);
        for step in &self.steps {
            match step {
                PlaceStep::Index(n) | PlaceStep::Key(n) => n.refs(refs),
                PlaceStep::Tuple(_) | PlaceStep::Field(_) => (),
            }
        }
    }
}

#[derive(Debug)]
pub struct ByRef<R: Rt, E: UserEvent> {
    pub(crate) spec: Expr,
    pub typ: Type,
    pub child: Node<R, E>,
    pub id: BindId,
    resident: TagValue,
    place: Option<Place<R, E>>,
    registered: Option<(BindId, place::Path)>,
}

impl<R: Rt, E: UserEvent> ByRef<R, E> {
    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<R, E>,
        buf: &mut &[u8],
    ) -> Result<Node<R, E>, PackError> {
        let spec = Expr::decode(buf)?;
        let typ = Type::decode(buf)?;
        let child = decode_node(ctx, buf)?;
        let id = BindId::decode(buf)?;
        let place = Place::image_decode(ctx, buf)?;
        Ok(Node::new(Self {
            spec,
            typ,
            child,
            id,
            resident: TagValue::phantom(),
            place,
            registered: None,
        }))
    }

    /// Construct a `ByRef` from an already-compiled child. Does no
    /// byref-chain plumbing: a caller wanting ref-to-ref chaining must
    /// insert into `ctx.env.byref_chain` itself.
    #[allow(dead_code)]
    pub fn new(id: BindId, typ: Type, child: Node<R, E>, spec: Expr) -> Node<R, E> {
        Node::new(Self {
            spec,
            typ,
            child,
            id,
            resident: TagValue::phantom(),
            place: None,
            registered: None,
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
        let id = BindId::new();
        // A place reference types as a reference to the element and
        // still mints a cell so embedders keep reading the mirror.
        let place = match Place::<R, E>::of(expr) {
            None => None,
            Some((root, specs)) => {
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
                Some(Place { root, steps })
            }
        };
        let typ = if place.is_some() {
            Type::ByRef(Arc::new(Type::empty_tvar()))
        } else {
            if let Some(c) = (&*child as &dyn std::any::Any).downcast_ref::<Ref>() {
                ctx.env.byref_chain.insert_cow(id, c.id);
            }
            Type::ByRef(Arc::new(child.typ().clone()))
        };
        Ok(Node::new(Self {
            spec,
            typ,
            child,
            id,
            resident: TagValue::phantom(),
            place,
            registered: None,
        }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for ByRef<R, E> {
    fn image_len(&self) -> usize {
        tag_len()
            + self.spec.encoded_len()
            + self.typ.encoded_len()
            + self.child.image_len()
            + self.id.encoded_len()
            + Place::image_len(&self.place)
    }

    fn image_encode(&self, buf: &mut BytesMut) -> Result<(), PackError> {
        put_tag(NodeTag::ByRef, buf);
        self.spec.encode(buf)?;
        self.typ.encode(buf)?;
        self.child.image_encode(buf)?;
        self.id.encode(buf)?;
        Place::image_encode(&self.place, buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> &TagValue {
        // A moved key re-fires the reference so readers re-resolve.
        let mut moved = false;
        if let Some(place) = &mut self.place {
            let (path, m) = place.update(ctx, event);
            moved = m;
            if let Some((root, path)) = path
                && self.registered.as_ref() != Some(&(root, path.clone()))
            {
                ctx.rt.set_ref_path(self.id, root, path.clone());
                self.registered = Some((root, path));
                moved = true;
            }
        }
        // A stale refresh must not re-write the referent, and a taint
        // placeholder must never enter the cross-cycle store.
        let tv = self.child.update(ctx, event);
        if tv.is_fired() {
            let v = tv.value_cloned();
            if event.init {
                // A standing write: `Deref`'s init read serves it this
                // cycle; a queued write would arrive again next cycle.
                ctx.rt.store_insert_standing(self.id, TagValue::fired(v));
            } else {
                ctx.rt.set_var(self.id, v);
            }
        } else if event.init && !tv.tag().is_bottom() && !tv.tag().is_bottom() {
            // A wake-forced init view reads stale, but the cell must
            // still materialize: embedders read it directly and a
            // chainless ref's cell is its only storage.
            ctx.rt.store_insert_standing(self.id, TagValue::stale(tv.value_cloned()));
        }
        if event.init || moved {
            self.resident.set(TagValue::fired(Value::U64(self.id.inner())))
        } else {
            self.resident.ride()
        }
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        ctx.env.byref_chain.remove_cow(&self.id);
        if let Some(place) = &mut self.place {
            ctx.rt.clear_ref_path(&self.id);
            place.each(&mut |n| n.delete(ctx));
        }
        self.child.delete(ctx)
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        if let Some(place) = &mut self.place {
            place.each(&mut |n| n.sleep(ctx));
        }
        self.child.sleep(ctx);
    }

    fn reset_replay(&mut self, ctx: &mut ExecCtx<R, E>) {
        if let Some(place) = &mut self.place {
            place.each(&mut |n| n.reset_replay(ctx));
        }
        self.child.reset_replay(ctx);
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn typ(&self) -> &Type {
        &self.typ
    }

    fn refs(&self, refs: &mut Refs) {
        if let Some(place) = &self.place {
            place.refs(refs);
        }
        self.child.refs(refs)
    }

    fn typecheck0(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        wrap!(self.child, self.child.typecheck0(ctx))?;
        match &mut self.place {
            Some(place) => {
                let mut res = Ok(());
                place.each(&mut |n| {
                    if res.is_ok() {
                        res = n.typecheck0(ctx);
                    }
                });
                wrap!(self, res)
            }
            None => {
                let t = Type::ByRef(Arc::new(self.child.typ().clone()));
                wrap!(self, self.typ.check_contains(&ctx.env, &t))
            }
        }
    }

    fn typecheck1(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        wrap!(self.child, self.child.typecheck1(ctx))?;
        if let Some(place) = &mut self.place {
            let mut res = Ok(());
            place.each(&mut |n| {
                if res.is_ok() {
                    res = n.typecheck1(ctx);
                }
            });
            wrap!(self, res)?;
            // The element type is the access's type minus its failure;
            // a place handles the failure at runtime.
            let err = Type::Primitive(Typ::Error.into());
            let elem = wrap!(self, self.child.typ().diff(&ctx.env, &err))?;
            wrap!(self, self.typ.check_contains(&ctx.env, &Type::ByRef(Arc::new(elem))))?;
        }
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
    /// The path to apply to `id`'s value when the reference is a place.
    path: Option<place::Path>,
}

impl<R: Rt, E: UserEvent> Deref<R, E> {
    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<R, E>,
        buf: &mut &[u8],
    ) -> Result<Node<R, E>, PackError> {
        let spec = Expr::decode(buf)?;
        let typ = Type::decode(buf)?;
        let child = decode_node(ctx, buf)?;
        let id = Option::<BindId>::decode(buf)?;
        let top_id = ExprId::decode(buf)?;
        let path = match bool::decode(buf)? {
            true => Some(place::path_decode(buf)?),
            false => None,
        };
        if let Some(id) = id {
            ctx.rt.ref_var(id, top_id);
        }
        Ok(Node::new(Self {
            spec,
            typ,
            child,
            id,
            top_id,
            resident: TagValue::phantom(),
            path,
        }))
    }

    /// Build a `Deref` from an already-compiled child that evaluates to
    /// a `Value::U64` / `Value::V64` holding a BindId.
    #[allow(dead_code)]
    pub fn new(typ: Type, child: Node<R, E>, top_id: ExprId, spec: Expr) -> Node<R, E> {
        Node::new(Self {
            spec,
            typ,
            child,
            id: None,
            top_id,
            resident: TagValue::phantom(),
            path: None,
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
            path: None,
        }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Deref<R, E> {
    fn image_len(&self) -> usize {
        tag_len()
            + self.spec.encoded_len()
            + self.typ.encoded_len()
            + self.child.image_len()
            + self.id.encoded_len()
            + self.top_id.encoded_len()
            + 1
            + self.path.as_ref().map_or(0, place::path_len)
    }

    fn image_encode(&self, buf: &mut BytesMut) -> Result<(), PackError> {
        put_tag(NodeTag::Deref, buf);
        self.spec.encode(buf)?;
        self.typ.encode(buf)?;
        self.child.image_encode(buf)?;
        self.id.encode(buf)?;
        self.top_id.encode(buf)?;
        self.path.is_some().encode(buf)?;
        match &self.path {
            Some(p) => place::path_encode(p, buf),
            None => Ok(()),
        }
    }

    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> &TagValue {
        let tv = self.child.update(ctx, event);
        if !tv.tag().is_bottom() {
            let id = tv.with_value(|v| match v {
                Value::U64(i) | Value::V64(i) => Some(BindId::from(*i)),
                _ => None,
            });
            // Resolve through the byref chain as the write path does:
            // `&x`'s cell mirrors x a cycle late, the referent does not.
            // A chainless reference's own cell is its only storage.
            let id = id.map(|cell| match ctx.rt.ref_path(&cell) {
                Some((root, path)) => {
                    self.path = Some(path.clone());
                    *root
                }
                None => {
                    self.path = None;
                    ctx.env.byref_chain.get(&cell).copied().unwrap_or(cell)
                }
            });
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
                // Fresh under a genuine init view only (see Ref::update).
                let init = if ctx.frame_depth > 0 {
                    ctx.dispatch_init
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
        let res = match (res, &self.path) {
            (Some(tv), Some(path)) if !tv.tag().is_bottom() => {
                match tv.with_value(|v| place::read_path(v, path)) {
                    Ok(v) => {
                        let mut c = TagValue::fired(v);
                        c.retag(tv.tag());
                        Some(c)
                    }
                    Err(e) => {
                        log::warn!("read through a reference: {e}");
                        None
                    }
                }
            }
            (res, _) => res,
        };
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
        // A container read's type is a TVar bound to `&T`, not a bare
        // `Type::ByRef`.
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
