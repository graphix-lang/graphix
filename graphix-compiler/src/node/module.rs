use crate::{
    BindId, CFlag, Event, ExecCtx, Node, Refs, Rt, Scope, Tag, TagValue, Update,
    UserEvent,
    compiler::compile,
    env::Env,
    errf,
    expr::{
        BindSig, Doc, Expr, ExprId, ExprKind, ModPath, Origin, Sandbox, Sig, SigKind,
        Source, StructurePattern, TypeDefExpr, parser,
    },
    ide::{ModuleInternalView, ModuleRefSite, SigImplLink},
    node::{Nop, bind::Bind},
    typ::{AbstractId, Type},
    wrap,
};
use ahash::AHashSet;
use anyhow::{Context, Result, bail};
use arcstr::{ArcStr, literal};
use compact_str::{CompactString, format_compact};
use enumflags2::BitFlags;
use netidx_value::{Typ, Value};
use nohash::IntMap;
use poolshark::local::LPooled;
use std::{any::Any, mem, sync::LazyLock};
use triomphe::Arc;

fn bind_sig(env: &mut Env, mod_env: &mut Env, scope: &Scope, sig: &Sig) -> Result<()> {
    env.modules.insert_cow(scope.lexical.clone());
    for si in sig.items.iter() {
        let si_ori = si.ori.clone().unwrap_or_else(|| Arc::new(Origin::default()));
        match &si.kind {
            SigKind::Module(name) => {
                let scope = scope.append(name);
                env.modules.insert_cow(scope.lexical.clone());
                if env.lsp_mode {
                    env.push_module_reference(ModuleRefSite {
                        pos: si.pos,
                        ori: si_ori.clone(),
                        name: ModPath::from_iter([name.clone()]),
                        canonical: scope.lexical.clone(),
                        def_ori: None,
                    });
                }
            }
            SigKind::Use(name) => {
                env.use_in_scope(scope, name)?;
                mod_env.use_in_scope(scope, name)?;
                if env.lsp_mode {
                    let canonical = env
                        .canonical_modpath(&scope.lexical, name)
                        .unwrap_or_else(|| name.clone());
                    env.push_module_reference(ModuleRefSite {
                        pos: si.pos,
                        ori: si_ori.clone(),
                        name: name.clone(),
                        canonical,
                        def_ori: None,
                    });
                }
            }
            SigKind::Bind(BindSig { name, typ }) => {
                let typ = typ.scope_refs(&scope.lexical);
                typ.alias_tvars(&mut LPooled::take());
                if env.lsp_mode {
                    typ.record_ide_refs(env, &scope.lexical);
                }
                let bind =
                    env.bind_variable(&scope.lexical, name, typ, si.pos, si_ori.clone());
                if let Doc(Some(s)) = &si.doc {
                    bind.doc = Some(s.clone());
                }
            }
            SigKind::TypeDef(td) => {
                let typ = td.typ.scope_refs(&scope.lexical);
                env.deftype(
                    &scope.lexical,
                    &td.name,
                    td.params.clone(),
                    typ.clone(),
                    si.doc.0.clone(),
                    si.pos,
                    si_ori,
                )?;
            }
        }
    }
    Ok(())
}

// copy the exported signature of all the exported inner modules in this sig to
// the global env
fn export_sig(env: &mut Env, inner_env: &Env, scope: &Scope, sig: &Sig) {
    let mut buf: LPooled<String> = LPooled::take();
    for si in sig.items.iter() {
        if let SigKind::Module(name) = &si.kind {
            use std::fmt::Write;
            let scope = scope.append(name);
            env.modules.insert_cow(scope.lexical.clone());
            buf.clear();
            write!(buf, "{}/", scope.lexical.0).unwrap();
            for m in inner_env.modules.range::<ModPath, _>(&scope.lexical..) {
                if m == &scope.lexical || m.starts_with(&*buf) {
                    env.modules.insert_cow(m.clone());
                } else {
                    break;
                }
            }
            macro_rules! copy_sig {
                ($kind:ident) => {
                    let iter = inner_env.$kind.range::<ModPath, _>(&scope.lexical..);
                    for (path, inner) in iter {
                        buf.clear();
                        write!(buf, "{}/", scope.lexical.0).unwrap();
                        if path == &scope.lexical || path.starts_with(&*buf) {
                            env.$kind.insert_cow(path.clone(), inner.clone());
                        }
                    }
                };
            }
            copy_sig!(binds);
            copy_sig!(typedefs);
        }
    }
}

fn check_sig<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<R, E>,
    top_id: ExprId,
    proxy: &mut IntMap<BindId, BindId>,
    scope: &Scope,
    sig: &Sig,
    nodes: &[Node<R, E>],
) -> Result<()> {
    let mut has_bind: LPooled<AHashSet<ArcStr>> = LPooled::take();
    let mut abstract_types: LPooled<IntMap<AbstractId, Type>> = LPooled::take();
    for n in nodes {
        if let Some(bind) = (&**n as &dyn Any).downcast_ref::<Bind<R, E>>()
            && let Some(binds) = ctx.env.binds.get(&scope.lexical)
            && let Expr { kind: ExprKind::Bind(bexp), .. } = bind.spec()
            && let StructurePattern::Bind(name) = &bexp.pattern
            && let Some(id) = bind.single_id()
            && let Some(proxy_id) = binds.get(&CompactString::from(name.as_str()))
            && let Some(proxy_bind) = ctx.env.by_id.get(&proxy_id)
        {
            proxy_bind.typ.unbind_tvars();
            proxy_bind.typ.sig_matches(&ctx.env, bind.typ(), &abstract_types).with_context(|| {
                format!(
                    "signature mismatch \"val {name}: ...\", signature has type {}, implementation has type {}",
                    proxy_bind.typ,
                    bind.typ()
                )
            })?;
            proxy.insert(id, *proxy_id);
            ctx.rt.ref_var(id, top_id);
            ctx.rt.ref_var(*proxy_id, top_id);
            if ctx.env.lsp_mode {
                ctx.env.push_sig_link(SigImplLink {
                    scope: scope.lexical.clone(),
                    name: CompactString::from(name.as_str()),
                    sig_id: *proxy_id,
                    impl_id: id,
                });
            }
            has_bind.insert(name.clone());
        }
        if let Expr { kind: ExprKind::TypeDef(td), .. } = n.spec()
            && let Some(defs) = ctx.env.typedefs.get(&scope.lexical)
            && let Some(sig_td) = defs.get(&CompactString::from(td.name.as_str()))
        {
            let sig_td = TypeDefExpr {
                name: td.name.clone(),
                params: sig_td.params.clone(),
                typ: sig_td.typ.clone(),
            };
            match &sig_td.typ {
                Type::Abstract { id, params: _ } => {
                    for (tv0, con0) in td.params.iter() {
                        match sig_td.params.iter().find(|(tv1, _)| tv0.name == tv1.name) {
                            Some((_, con1)) if con0 != con1 => {
                                let con0 = match con0 {
                                    None => "missing",
                                    Some(t) => &format_compact!("{t}"),
                                };
                                let con1 = match con1 {
                                    None => "missing",
                                    Some(t) => &format_compact!("{t}"),
                                };
                                bail!(
                                    "signature mismatch in {}, constraint mismatch on {}, signature constraint {con1} vs implementation constraint {con0}",
                                    td.name,
                                    tv0.name
                                )
                            }
                            None => bail!(
                                "signature mismatch in {}, missing parameter {}",
                                sig_td.name,
                                tv0.name
                            ),
                            Some(_) => (),
                        }
                    }
                    abstract_types.insert(*id, td.typ.clone());
                    // Persist the abstract→concrete mapping so fusion's
                    // `abi_kind` can lower abstract-typed values
                    // to their concrete representation (the abstraction
                    // stays opaque to the type system; only the optimizer
                    // peeks). Stored on the `ExecCtx`, so it drops with
                    // the context (`AbstractId`s are minted fresh per
                    // compile — a global map would leak).
                    ctx.fusion
                        .abstract_registry
                        .insert(*id, td.typ.scope_refs(&scope.lexical));
                }
                _ => {
                    if sig_td.name != td.name
                        || sig_td.params != td.params
                        || sig_td.typ != td.typ.scope_refs(&scope.lexical)
                    {
                        bail!(
                            "signature mismatch in {}, expected {}, found {}",
                            td.name,
                            sig_td,
                            td
                        )
                    }
                }
            }
        }
    }
    for si in sig.items.iter() {
        let missing = match &si.kind {
            SigKind::Bind(BindSig { name, .. }) => !has_bind.contains(name),
            SigKind::TypeDef(TypeDefExpr {
                typ: Type::Abstract { id, params: _ },
                ..
            }) if !abstract_types.contains_key(id) => {
                bail!(
                    "abstract signature types must have a concrete definition in the implementation"
                )
            }
            SigKind::Module(_)
            | SigKind::Use(_)
            | SigKind::TypeDef(TypeDefExpr { .. }) => false,
        };
        if missing {
            bail!("sig item {si} is missing an implementation")
        }
    }
    Ok(())
}

static ERR_TAG: ArcStr = literal!("DynamicLoadError");
static TYP: LazyLock<Type> = LazyLock::new(|| {
    let t = Arc::from_iter([Type::Primitive(Typ::String.into())]);
    let err = Type::Error(Arc::new(Type::Variant(ERR_TAG.clone(), t)));
    Type::Set(Arc::from_iter([err, Type::Primitive(Typ::Null.into())]))
});

#[derive(Debug)]
pub struct Module<R: Rt, E: UserEvent> {
    spec: Expr,
    flags: BitFlags<CFlag>,
    source: Node<R, E>,
    // we need to be able to check the module sig at run time, so we must keep
    // both the environment we compile in as well as the inner private module
    // environment (env). We must keep the outer sig environment because the
    // dynamic module may itself not be exported from it's parent module, and in
    // that case it's bound signature would be lost at run time.
    dynamic_sig_env: Option<Env>,
    env: Env,
    sig: Sig,
    pub(crate) scope: Scope,
    proxy: IntMap<BindId, BindId>,
    pub(crate) nodes: Box<[Node<R, E>]>,
    top_id: ExprId,
}

impl<R: Rt, E: UserEvent> Module<R, E> {
    /// The module's body node. Used by graph introspection
    /// (`crate::node_shape`) to walk into a module.
    pub(crate) fn source(&self) -> &Node<R, E> {
        &self.source
    }

    pub(super) fn compile_dynamic(
        ctx: &mut ExecCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: Expr,
        scope: &Scope,
        sandbox: Sandbox,
        sig: Sig,
        source: Arc<Expr>,
        top_id: ExprId,
    ) -> Result<Node<R, E>> {
        let source = compile(ctx, flags, (*source).clone(), scope, top_id)?;
        let mut env = ctx.env.apply_sandbox(&sandbox).context("applying sandbox")?;
        bind_sig(&mut ctx.env, &mut env, &scope, &sig)
            .context("binding module signature")?;
        Ok(Box::new(Self {
            spec,
            flags,
            env,
            sig,
            source,
            dynamic_sig_env: Some(ctx.env.clone()),
            scope: scope.clone(),
            proxy: IntMap::default(),
            nodes: Box::new([]),
            top_id,
        }))
    }

    pub(super) fn compile_static(
        ctx: &mut ExecCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: Expr,
        scope: &Scope,
        sig: Sig,
        exprs: Arc<[Expr]>,
        top_id: ExprId,
    ) -> Result<Node<R, E>> {
        let source = Nop::new(Type::Primitive(Typ::String | Typ::Error));
        let mut env = ctx.env.clone();
        bind_sig(&mut ctx.env, &mut env, &scope, &sig)
            .with_context(|| format!("binding signature for module {}", scope.lexical))?;
        let mut t = Self {
            spec,
            flags,
            env,
            sig,
            source,
            dynamic_sig_env: None,
            scope: scope.clone(),
            proxy: IntMap::default(),
            nodes: Box::new([]),
            top_id,
        };
        t.compile_inner(ctx, &exprs)
            .with_context(|| format!("compiling module {}", scope.lexical))?;
        if ctx.env.lsp_mode {
            ctx.env.push_module_internal_view(ModuleInternalView {
                scope: t.scope.lexical.clone(),
                env: t.env.clone(),
            });
        }
        Ok(Box::new(t))
    }

    fn compile_source(&mut self, ctx: &mut ExecCtx<R, E>, text: ArcStr) -> Result<()> {
        let ori = Origin { parent: None, source: Source::Unspecified, text };
        let exprs = parser::parse(ori)?;
        self.compile_inner(ctx, &exprs)
    }

    fn compile_inner(&mut self, ctx: &mut ExecCtx<R, E>, exprs: &[Expr]) -> Result<()> {
        ctx.builtins_allowed = self.dynamic_sig_env.is_none();
        let nodes = ctx.with_restored_mut(&mut self.env, |ctx| -> Result<_> {
            let mut nodes = exprs
                .iter()
                .map(|e| compile(ctx, self.flags, e.clone(), &self.scope, self.top_id))
                .collect::<Result<Vec<_>>>()?;
            for n in &mut nodes {
                n.typecheck0(ctx)?
            }
            Ok(nodes)
        });
        ctx.builtins_allowed = true;
        let nodes = nodes?;
        match &mut self.dynamic_sig_env {
            None => check_sig(
                ctx,
                self.top_id,
                &mut self.proxy,
                &self.scope,
                &self.sig,
                &nodes,
            )?,
            Some(env) => ctx.with_restored_mut(env, |ctx| {
                check_sig(
                    ctx,
                    self.top_id,
                    &mut self.proxy,
                    &self.scope,
                    &self.sig,
                    &nodes,
                )
            })?,
        }
        self.nodes = Box::from(nodes);
        export_sig(&mut ctx.env, &self.env, &self.scope, &self.sig);
        Ok(())
    }

    fn clear_compiled(&mut self, ctx: &mut ExecCtx<R, E>) {
        for (id, proxy_id) in self.proxy.drain() {
            ctx.rt.unref_var(id, self.top_id);
            ctx.rt.unref_var(proxy_id, self.top_id);
        }
        ctx.with_restored_mut(&mut self.env, |ctx| {
            for mut n in mem::take(&mut self.nodes) {
                n.delete(ctx)
            }
        })
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Module<R, E> {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        event: &mut Event<E>,
    ) -> Option<TagValue> {
        let mut compiled = false;
        let mut src_tag = Tag::FIRED;
        if self.dynamic_sig_env.is_some()
            && let Some(tv) = self.source.update(ctx, event)
        {
            // never compile from a taint placeholder (and don't tear
            // down the running module on one) — pass the taint on
            if tv.is_tainted() {
                return Some(TagValue::tainted(Value::Null));
            }
            let (v, tag) = tv.into_parts();
            src_tag = tag;
            self.clear_compiled(ctx);
            match v {
                Value::String(s) => {
                    if let Err(e) = self.compile_source(ctx, s) {
                        return Some(TagValue::tagged(
                            errf!(ERR_TAG, "compile error {e:?}"),
                            tag,
                        ));
                    }
                }
                v => {
                    return Some(TagValue::tagged(errf!(ERR_TAG, "unexpected {v}"), tag));
                }
            }
            compiled = true;
            // Prime the fresh nodes' EXTERNAL refs from `ctx.cached` —
            // exactly what the lazy `CallSite::bind` does for a
            // runtime-compiled lambda body. The events that carried
            // outer-binding values (a stdlib lambda like `str::len`'s
            // `len`, bound at startup) are long gone, and `Ref::update`
            // reads only `event.variables`, so without this a
            // module-level builtin CALL in a dynamically loaded module
            // never saw its callee value and never fired — while the
            // module's status still reported success (soak-jul07b's
            // first dynamic-module findings).
            let mut refs = Refs::default();
            for n in self.nodes.iter() {
                n.refs(&mut refs);
            }
            refs.with_external_refs(|id| {
                if let Some(v) = ctx.rt.cached().get(&id) {
                    if let std::collections::hash_map::Entry::Vacant(e) =
                        event.variables.entry(id)
                    {
                        // FIRED: the priming is the fresh nodes' init view
                        e.insert(TagValue::fired(v.clone()));
                    }
                }
            });
        }
        let init = event.init;
        if compiled {
            event.init = true;
        }
        for (inner_id, proxy_id) in &self.proxy {
            if let Some(tv) = event.variables.get(proxy_id) {
                let tv = tv.clone();
                // the entry's tag flows through the proxy; the clean
                // cache never holds a taint placeholder
                if !tv.is_tainted() {
                    ctx.rt.cached_mut().insert(*inner_id, tv.value_cloned());
                }
                event.variables.insert(*inner_id, tv);
            }
        }
        self.nodes.iter_mut().fold(None, |_, n| n.update(ctx, event));
        event.init = init;
        for (inner_id, proxy_id) in &self.proxy {
            if let Some(tv) = event.variables.remove(inner_id) {
                if !tv.is_tainted() {
                    ctx.rt.cached_mut().insert(*proxy_id, tv.value_cloned());
                }
                event.variables.insert(*proxy_id, tv);
            }
        }
        compiled.then(|| TagValue::tagged(Value::Null, src_tag))
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        if self.dynamic_sig_env.is_none() {
            ctx.with_restored_mut(&mut self.env, |ctx| {
                for n in &mut self.nodes {
                    n.delete(ctx);
                }
            });
        } else {
            self.source.delete(ctx);
            self.clear_compiled(ctx);
        }
    }

    fn refs(&self, refs: &mut Refs) {
        self.source.refs(refs);
        for n in &self.nodes {
            n.refs(refs)
        }
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        if self.dynamic_sig_env.is_none() {
            ctx.with_restored_mut(&mut self.env, |ctx| {
                for n in &mut self.nodes {
                    n.sleep(ctx);
                }
            });
        } else {
            self.source.sleep(ctx);
            self.clear_compiled(ctx);
        }
    }

    fn reset_replay(&mut self, ctx: &mut ExecCtx<R, E>) {
        if self.dynamic_sig_env.is_some() {
            self.source.reset_replay(ctx);
        }
        ctx.with_restored_mut(&mut self.env, |ctx| {
            for n in &mut self.nodes {
                n.reset_replay(ctx);
            }
        });
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn typ(&self) -> &Type {
        if self.dynamic_sig_env.is_none() {
            self.nodes.last().map(|n| n.typ()).unwrap_or(&Type::Bottom)
        } else {
            &TYP
        }
    }

    fn typecheck0(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        wrap!(self.source, self.source.typecheck0(ctx))?;
        let t = Type::Primitive(Typ::String | Typ::Error);
        wrap!(self.source, t.check_contains(&self.env, self.source.typ()))?;
        // Interface re-exports: a caller references the public signature
        // binding's `BindId`, but the lambda lives on the impl binding
        // (recorded by its own `Bind::typecheck0` during `compile_inner`).
        // Proxy each sig id to its impl's LambdaDef so cross-module calls
        // resolve. All `typecheck0` precedes all `typecheck1`, so the
        // entry is present before resolution consumes it.
        for (impl_id, sig_id) in self.proxy.iter() {
            if let Some(fv) = ctx.bind_to_lambda.get(impl_id).cloned() {
                ctx.bind_to_lambda.insert(*sig_id, fv);
            }
        }
        Ok(())
    }

    fn typecheck1(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        wrap!(self.source, self.source.typecheck1(ctx))?;
        // Module children were typecheck0'd in `compile_inner` under the
        // module env, but the main `typecheck1` walk doesn't reach them
        // (this node only recurses `source`). Drive their `typecheck1`
        // here under the restored env — to finalize call sites AND run the
        // static resolution folded into `CallSite::typecheck1` (which the
        // deleted `static_resolve` pass used to reach via its own walk).
        // The restored env is required: `finalize_lambda` reads `ctx.env`.
        let Self { env, nodes, .. } = self;
        ctx.with_restored_mut(env, |ctx| {
            for n in nodes.iter_mut() {
                wrap!(n, n.typecheck1(ctx))?;
            }
            Ok(())
        })
    }

    fn view(&self) -> crate::NodeView<'_, R, E> {
        crate::NodeView::Module(self)
    }

    fn fuse(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<Option<Node<R, E>>> {
        // A module is structure, not computation — recurse into its
        // statement nodes so the contents fuse. (`source` is NOT a
        // child to fuse: for a dynamic module it's the node producing
        // the module's source string, whose compiled graph gets its
        // own fusion pass inside `compile_source` at runtime.)
        for child in self.nodes.iter_mut() {
            crate::fusion::fuse(child, ctx)?;
        }
        Ok(None)
    }
}
