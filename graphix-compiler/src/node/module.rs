use crate::env::Map;
use crate::{
    BindId, CFlag, Event, ExecCtx, Node, Refs, Rt, Scope, Tag, TagValue, Update,
    UserEvent,
    compiler::compile,
    env::{Env, ImplDef},
    errf,
    expr::{
        BindSig, Doc, Expr, ExprId, ExprKind, ModPath, Origin, Sandbox, Sig, SigKind,
        Source, StructurePattern, TypeDefBody, TypeDefExpr, add_interface_modules,
        parser,
    },
    ide::{ModuleInternalView, ModuleRefSite, SigImplLink},
    node::{Nop, bind::Bind, traits},
    typ::{AbstractId, Type},
    wrap,
};
use ahash::AHashSet;
use anyhow::{Context, Result, bail};
use arcstr::{ArcStr, literal};
use compact_str::{CompactString, format_compact};
use enumflags2::BitFlags;
use netidx_value::{Typ, Value};
use poolshark::local::LPooled;
use std::{any::Any, mem, sync::LazyLock};
use triomphe::Arc;

fn bind_sig(
    env: &mut Env,
    pending: &mut Vec<crate::PendingImport>,
    scope: &Scope,
    sig: &Sig,
) -> Result<()> {
    env.modules.insert_cow(scope.lexical.clone());
    // headers pass: a sig `use self::sub::…` may precede `mod sub;`
    // — declaration order carries no visibility meaning
    for si in sig.items.iter() {
        if let SigKind::Module(name) = &si.kind {
            env.modules.insert_cow(scope.append(name).lexical);
        }
    }
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
            SigKind::Use { reexport, names } => {
                if *reexport {
                    bail!("re-exports (`pub use`) are not yet supported")
                }
                // `names` is a global registry keyed by unique scope
                // paths (kept from `self` across the privacy swap),
                // so registering in the OUTER env alone covers the
                // impl compile too
                for item in names.iter() {
                    super::compile_use_item(
                        env, pending, si.pos, &si_ori, scope, false, item,
                    )?;
                }
            }
            SigKind::Bind(BindSig { name, typ }) => {
                let typ = typ.scope_refs(&scope.lexical).rewrite_trait_args(env)?;
                typ.alias_tvars(&mut LPooled::take());
                if env.lsp_mode {
                    typ.record_ide_refs(env, &scope.lexical);
                }
                let poly = matches!(typ, Type::Fn(_));
                let bind =
                    env.bind_variable(&scope.lexical, name, typ, si.pos, si_ori.clone());
                if let Doc(Some(s)) = &si.doc {
                    bind.doc = Some(s.clone());
                }
                if poly {
                    let id = bind.id;
                    env.poly_binds.insert_cow(id);
                }
            }
            SigKind::TypeDef(td) => {
                env.deftype(
                    &scope.lexical,
                    &td.name,
                    td.params.clone(),
                    &td.body,
                    true,
                    si.doc.0.clone(),
                    si.pos,
                    si_ori,
                )?;
            }
            SigKind::Trait(t) => {
                let tref = traits::trait_ref(&scope.lexical, &t.name, si.pos, &si_ori);
                let sigs = t.methods.iter().map(|m| {
                    let ft = traits::method_sig(&m.typ, &tref, &scope.lexical);
                    (m.name.clone(), Arc::new(ft), m.self_index, m.default.is_some())
                });
                env.deftrait(
                    &scope.lexical,
                    &t.name,
                    sigs,
                    si.doc.0.clone(),
                    si.pos,
                    si_ori,
                )?;
            }
            SigKind::Impl(im) => {
                // a DECLARED implementation: its method bindings are
                // minted here with the trait's signatures at the
                // target, and the implementation's own registration
                // of the same (trait, target) replaces it
                let Some(trait_id) = env.lookup_trait(&scope.lexical, &im.trait_name)?
                else {
                    bail!("no trait `{}` in scope at {}", im.trait_name, si.pos)
                };
                let trait_def = env.trait_def(trait_id).cloned().expect("trait def");
                let (target, params) =
                    traits::impl_head(env, &scope.lexical, &trait_def, im, true)
                        .with_context(|| format!("at {}", si.pos))?;
                if !im.methods.is_empty() {
                    bail!(
                        "an interface declares `impl {} for {target};` without a body",
                        im.trait_name
                    )
                }
                let bscope = scope.append_block("impl", ExprId::new().inner());
                let mut methods: Map<CompactString, BindId> = Map::new();
                for d in trait_def.methods.iter() {
                    let typ = Type::Fn(Arc::new(traits::method_sig_at(
                        &d.typ.reset_tvars(),
                        &target,
                    )));
                    let bind = env.bind_variable(
                        &bscope.lexical,
                        &d.name,
                        typ,
                        si.pos,
                        si_ori.clone(),
                    );
                    methods.insert_cow(d.name.as_str().into(), bind.id);
                }
                env.register_impl(Arc::new(ImplDef {
                    trait_id,
                    target,
                    params,
                    scope: bscope.lexical,
                    methods,
                    declared: true,
                    pos: si.pos,
                    ori: si_ori,
                }))
                .with_context(|| format!("at {}", si.pos))?;
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
            copy_sig!(traits);
            // a re-exported module's Graphix-minted abstracts are
            // public exactly where their typedef entries are copied
            let exported: LPooled<Vec<AbstractId>> = inner_env
                .typedefs
                .range::<ModPath, _>(&scope.lexical..)
                .filter(|(path, _)| {
                    buf.clear();
                    write!(buf, "{}/", scope.lexical.0).unwrap();
                    *path == &scope.lexical || path.starts_with(&*buf)
                })
                .flat_map(|(_, defs)| defs.into_iter())
                .filter_map(|(_, td)| match (&td.typ, &td.rep) {
                    (Type::Abstract { id, .. }, Some(_)) => Some(*id),
                    _ => None,
                })
                .collect();
            for id in exported.iter() {
                env.publish_abstract_rep(*id);
            }
        }
    }
}

/// A signature binding and the binding behind it: the implementation's
/// own for a `val` or an overridden method, the trait's default for a
/// method the implementation leaves to it. The module copies the
/// inner production to the outer id every cycle (`Module::update`)
/// and proxies the inner lambda for static resolution
/// (`proxy_lambda_defs`). `owned` says the inner binding is the
/// module's private one — its production moves out instead of being
/// shared with the rest of the cycle, and a write to the outer id
/// flows in.
#[derive(Debug, Clone, Copy)]
struct Proxy {
    inner: BindId,
    outer: BindId,
    owned: bool,
}

fn check_sig<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<R, E>,
    top_id: ExprId,
    proxy: &mut Vec<Proxy>,
    scope: &Scope,
    sig: &Sig,
    nodes: &[Node<R, E>],
) -> Result<()> {
    let mut has_bind: LPooled<AHashSet<ArcStr>> = LPooled::take();
    let mut defined_abstracts: LPooled<AHashSet<ArcStr>> = LPooled::take();
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
            proxy_bind.typ.sig_matches(&ctx.env, bind.typ()).with_context(|| {
                format!(
                    "signature mismatch \"val {name}: ...\", signature has type {}, implementation has type {}",
                    proxy_bind.typ,
                    bind.typ()
                )
            })?;
            proxy.push(Proxy { inner: id, outer: *proxy_id, owned: true });
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
                body: match (&sig_td.typ, &sig_td.rep) {
                    (Type::Abstract { .. }, rep) => TypeDefBody::Abstract(rep.clone()),
                    (typ, _) => TypeDefBody::Alias(typ.clone()),
                },
            };
            match &sig_td.body {
                TypeDefBody::Abstract(None) => {
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
                    let TypeDefBody::Abstract(_) = &td.body else {
                        bail!(
                            "{} is hidden by the interface, so its definition must be \
                             `type {} = Abstract<..>` (a Rust-backed type declares \
                             `type {};`)",
                            td.name,
                            td.name,
                            td.name
                        )
                    };
                    defined_abstracts.insert(td.name.clone());
                }
                _ => {
                    let impl_body = match &td.body {
                        TypeDefBody::Alias(t) => {
                            TypeDefBody::Alias(t.scope_refs(&scope.lexical))
                        }
                        TypeDefBody::Abstract(rep) => TypeDefBody::Abstract(
                            rep.as_ref().map(|r| r.scope_refs(&scope.lexical)),
                        ),
                    };
                    if sig_td.name != td.name
                        || sig_td.params != td.params
                        || sig_td.body != impl_body
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
            SigKind::Impl(im) => {
                let trait_id = ctx
                    .env
                    .lookup_trait(&scope.lexical, &im.trait_name)?
                    .expect("bound by bind_sig");
                let target = im.target.scope_refs(&scope.lexical);
                let declared =
                    ctx.env.impl_entry(trait_id, &target)?.expect("bound by bind_sig");
                let mut fulfilled = nodes.iter().filter_map(|n| {
                    (&**n as &dyn Any).downcast_ref::<traits::Impl<R, E>>().filter(|i| {
                        i.fulfils.as_ref().is_some_and(|d| Arc::ptr_eq(d, &declared))
                    })
                });
                match (fulfilled.next(), fulfilled.next()) {
                    (None, _) => true,
                    (Some(_), Some(dup)) => bail!(
                        "impl {} for {target} is implemented twice (at {})",
                        im.trait_name,
                        dup.spec().pos
                    ),
                    (Some(i), None) => {
                        let trait_def = ctx
                            .env
                            .trait_def(trait_id)
                            .cloned()
                            .expect("bound by bind_sig");
                        for (name, outer) in declared.methods.into_iter() {
                            let (inner, owned) = match i.def.methods.get(name) {
                                Some(id) => (*id, true),
                                None => {
                                    let default = trait_def
                                        .methods
                                        .iter()
                                        .find(|m| m.name.as_str() == name.as_str())
                                        .and_then(|m| m.default);
                                    match default {
                                        Some(id) => (id, false),
                                        None => bail!(
                                            "impl {} for {target} does not implement {name}",
                                            im.trait_name
                                        ),
                                    }
                                }
                            };
                            proxy.push(Proxy { inner, outer: *outer, owned });
                            ctx.rt.ref_var(inner, top_id);
                            ctx.rt.ref_var(*outer, top_id);
                        }
                        false
                    }
                }
            }
            SigKind::Trait(t) => {
                // the implementation re-declares the trait (the
                // interface's declaration is prepended to its body
                // unless it wrote its own); a written re-declaration
                // must agree with the interface
                for n in nodes {
                    if let Expr { kind: ExprKind::Trait(t2), .. } = n.spec()
                        && t2.name == t.name
                        && (t2.methods.len() != t.methods.len()
                            || t2.methods.iter().zip(t.methods.iter()).any(|(a, b)| {
                                a.name != b.name
                                    || format_compact!("{}", a.typ)
                                        != format_compact!("{}", b.typ)
                            }))
                    {
                        bail!(
                            "trait {} is declared by the interface as {t}; the \
                             implementation's {t2} does not match",
                            t.name
                        )
                    }
                }
                false
            }
            SigKind::TypeDef(TypeDefExpr {
                name,
                body: TypeDefBody::Abstract(None),
                ..
            }) if !defined_abstracts.contains(name) => {
                bail!(
                    "{name} is hidden by the interface, so the implementation must \
                     define it: `type {name} = Abstract<..>`, or `type {name};` for \
                     a Rust-backed type"
                )
            }
            SigKind::Module(_)
            | SigKind::Use { .. }
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
    proxy: Vec<Proxy>,
    pub(crate) nodes: Box<[Node<R, E>]>,
    /// catch-statement indices in `nodes` (see `Block::catches`).
    pub(crate) catches: Box<[usize]>,
    top_id: ExprId,
    resident: TagValue,
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
        enclosing: &Scope,
        scope: &Scope,
        sandbox: Sandbox,
        sig: Sig,
        source: Arc<Expr>,
        top_id: ExprId,
    ) -> Result<Node<R, E>> {
        // The source expression is LOADER-side code: it compiles in the
        // enclosing scope, so `let src = …; mod foo dynamic { … source
        // src }` resolves. Only the loaded module text compiles under
        // the module's own scope.
        let source = compile(ctx, flags, (*source).clone(), enclosing, top_id)?;
        let mut env = ctx.env.apply_sandbox(&sandbox).context("applying sandbox")?;
        env.modules.insert_cow(scope.lexical.clone());
        bind_sig(&mut ctx.env, &mut ctx.pending_imports, &scope, &sig)
            .context("binding module signature")?;
        Ok(Node::new(Self {
            spec,
            flags,
            env,
            sig,
            source,
            dynamic_sig_env: Some(ctx.env.clone()),
            scope: scope.clone(),
            proxy: Vec::new(),
            nodes: Box::new([]),
            catches: Box::new([]),
            top_id,
            resident: TagValue::phantom(),
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
        // the private snapshot predates bind_sig, but the module's
        // own path must be visible from inside it (its submodules
        // resolve package-rooted paths through it)
        env.modules.insert_cow(scope.lexical.clone());
        bind_sig(&mut ctx.env, &mut ctx.pending_imports, &scope, &sig)
            .with_context(|| format!("binding signature for module {}", scope.lexical))?;
        let mut t = Self {
            spec,
            flags,
            env,
            sig,
            source,
            dynamic_sig_env: None,
            scope: scope.clone(),
            proxy: Vec::new(),
            nodes: Box::new([]),
            catches: Box::new([]),
            top_id,
            resident: TagValue::phantom(),
        };
        t.compile_inner(ctx, &exprs)
            .with_context(|| format!("compiling module {}", scope.lexical))?;
        if ctx.env.lsp_mode {
            ctx.env.push_module_internal_view(ModuleInternalView {
                scope: t.scope.lexical.clone(),
                env: t.env.clone(),
            });
        }
        Ok(Node::new(t))
    }

    fn compile_source(&mut self, ctx: &mut ExecCtx<R, E>, text: ArcStr) -> Result<()> {
        let ori = Origin { parent: None, source: Source::Unspecified, text };
        // the signature's declarations apply to the loaded source
        // exactly as a `.gxi`'s do to its file (the resolvers splice
        // them the same way)
        let exprs = add_interface_modules(parser::parse(ori)?, &self.sig);
        // the namespace table is a global registry (it survives the
        // privacy swap), so a recompile must scrub the previous
        // source's imports explicitly or they'd accumulate; the
        // spliced signature items re-register the sig's own uses
        ctx.env.clear_names_under(&self.scope.lexical);
        self.compile_inner(ctx, &exprs)
    }

    fn compile_inner(&mut self, ctx: &mut ExecCtx<R, E>, exprs: &[Expr]) -> Result<()> {
        ctx.builtins_allowed = self.dynamic_sig_env.is_none();
        let nodes = ctx.with_restored_mut(&mut self.env, |ctx| -> Result<_> {
            let (mut nodes, catches) = crate::node::compile_block_children(
                ctx,
                self.flags,
                &self.scope,
                self.top_id,
                true,
                exprs.iter(),
            )
            .map(|(n, c)| (Vec::from(n), c))?;
            // Two-phase tc0, catches last innermost-first (see
            // `Block::typecheck0`).
            let mut catch = catches.iter().copied().peekable();
            for (i, n) in nodes.iter_mut().enumerate() {
                if catch.peek() == Some(&i) {
                    catch.next();
                    continue;
                }
                n.typecheck0(ctx)?
            }
            for i in catches.iter().rev() {
                nodes[*i].typecheck0(ctx)?
            }
            Ok((nodes, catches))
        });
        ctx.builtins_allowed = true;
        let (nodes, catches) = nodes?;
        self.catches = catches;
        self.nodes = nodes.into_boxed_slice();
        match &mut self.dynamic_sig_env {
            None => check_sig(
                ctx,
                self.top_id,
                &mut self.proxy,
                &self.scope,
                &self.sig,
                &self.nodes,
            )?,
            Some(env) => {
                ctx.with_restored_mut(env, |ctx| {
                    check_sig(
                        ctx,
                        self.top_id,
                        &mut self.proxy,
                        &self.scope,
                        &self.sig,
                        &self.nodes,
                    )
                })?;
                // a load happens at run time, long after the batch
                // walk that `typecheck1`s a static module's children
                self.proxy_lambda_defs(ctx);
                self.typecheck1_nodes(ctx)?;
            }
        }
        export_sig(&mut ctx.env, &self.env, &self.scope, &self.sig);
        Ok(())
    }

    /// Interface re-exports: a caller references the public signature
    /// binding's `BindId`, but the lambda lives on the impl binding
    /// (recorded by its own `Bind::typecheck0`). Proxy each outer id
    /// to its inner LambdaDef so cross-module calls resolve.
    fn proxy_lambda_defs(&self, ctx: &mut ExecCtx<R, E>) {
        for Proxy { inner, outer, .. } in self.proxy.iter() {
            let hit = ctx.bind_to_lambda.contains_key(inner);
            if crate::dbgenv::gxdbg_resolve() {
                eprintln!("B2L-PROXY {inner:?} -> {outer:?} hit={hit}");
            }
            if let Some(fv) = ctx.bind_to_lambda.get(inner).cloned() {
                ctx.bind_to_lambda.insert(*outer, fv);
            }
        }
    }

    /// Drive the children's `typecheck1` under the module's private
    /// env (`finalize_lambda` reads `ctx.env`); it finalizes call
    /// sites and runs the static resolution folded into
    /// `CallSite::typecheck1`.
    fn typecheck1_nodes(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        let Self { env, nodes, catches, .. } = self;
        ctx.with_restored_mut(env, |ctx| {
            let mut catch = catches.iter().copied().peekable();
            for (i, n) in nodes.iter_mut().enumerate() {
                if catch.peek() == Some(&i) {
                    catch.next();
                    continue;
                }
                wrap!(n, n.typecheck1(ctx))?;
                // Per-STATEMENT settle drain (see
                // `drain_pending_settles`): a later statement's
                // resolution reads settled facts, so each statement's
                // deferred settles land before the next statement
                // resolves — the CURRENT frame only; entries an
                // enclosing resolution owns live in ITS frame.
                wrap!(n, crate::drain_pending_settles(ctx))?;
            }
            for i in catches.iter().rev() {
                let n = &mut nodes[*i];
                wrap!(n, n.typecheck1(ctx))?;
                wrap!(n, crate::drain_pending_settles(ctx))?;
            }
            Ok(())
        })
    }

    fn clear_compiled(&mut self, ctx: &mut ExecCtx<R, E>) {
        for Proxy { inner, outer, .. } in self.proxy.drain(..) {
            ctx.rt.unref_var(inner, self.top_id);
            ctx.rt.unref_var(outer, self.top_id);
        }
        ctx.with_restored_mut(&mut self.env, |ctx| {
            for mut n in mem::take(&mut self.nodes) {
                n.delete(ctx)
            }
        })
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Module<R, E> {
    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> &TagValue {
        let mut compiled = false;
        let mut src_tag = Tag::FIRED;
        let src = if self.dynamic_sig_env.is_some() {
            let tv = self.source.update(ctx, event);
            let tag = tv.tag();
            if !tag.triggers() {
                // a quiet source production (the value channel) never
                // recompiles the running module
                None
            } else if tag.is_bottom() {
                // never compile from a taint placeholder (and don't tear
                // down the running module on one) — pass the taint on
                return self
                    .resident
                    .set(TagValue::tagged(Value::Null, Tag::FRESH_BOTTOM));
            } else {
                Some((tv.value_cloned(), tag))
            }
        } else {
            None
        };
        if let Some((v, tag)) = src {
            src_tag = tag;
            self.clear_compiled(ctx);
            match v {
                Value::String(s) => {
                    if let Err(e) = self.compile_source(ctx, s) {
                        return self.resident.set(TagValue::tagged(
                            errf!(ERR_TAG, "compile error {e:?}"),
                            tag,
                        ));
                    }
                }
                v => {
                    return self
                        .resident
                        .set(TagValue::tagged(errf!(ERR_TAG, "unexpected {v}"), tag));
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
                if let Some(v) = ctx.rt.store_value(&id) {
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
        for Proxy { inner, outer, owned } in &self.proxy {
            if *owned && let Some(tv) = event.variables.get(outer) {
                let tv = tv.clone();
                // the entry's tag flows through the proxy; the clean
                // cache never holds a taint placeholder
                if !tv.is_tainted() {
                    ctx.rt.store_insert(*inner, TagValue::fired(tv.value_cloned()));
                }
                event.variables.insert(*inner, tv);
            }
        }
        {
            // Two-phase order, catches last innermost-first (see
            // `Block::update`); a module discards productions, so no
            // value capture is needed.
            let mut catch = self.catches.iter().copied().peekable();
            for (i, n) in self.nodes.iter_mut().enumerate() {
                if catch.peek() == Some(&i) {
                    catch.next();
                    continue;
                }
                let _ = n.update(ctx, event);
            }
            for i in self.catches.iter().rev() {
                let _ = self.nodes[*i].update(ctx, event);
            }
        }
        event.init = init;
        for Proxy { inner, outer, owned } in &self.proxy {
            let tv = if *owned {
                event.variables.remove(inner)
            } else {
                event.variables.get(inner).cloned()
            };
            let tv = match tv {
                Some(tv) => tv,
                // a shared inner binding (a trait default) may have
                // produced long before this load: its standing value
                // is the fresh outer binding's init view
                None if compiled => match ctx.rt.store_value(inner) {
                    Some(v) => TagValue::fired(v.clone()),
                    None => continue,
                },
                None => continue,
            };
            if !tv.is_tainted() {
                ctx.rt.store_insert(*outer, TagValue::fired(tv.value_cloned()));
            }
            event.variables.insert(*outer, tv);
        }
        if compiled {
            self.resident.set(TagValue::tagged(Value::Null, src_tag))
        } else {
            self.resident.ride()
        }
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
        // All `typecheck0` precedes all `typecheck1`, so the proxied
        // entries are present before resolution consumes them.
        self.proxy_lambda_defs(ctx);
        Ok(())
    }

    fn typecheck1(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        wrap!(self.source, self.source.typecheck1(ctx))?;
        // the main walk recurses only `source`; the children were
        // `typecheck0`'d in `compile_inner` under the module env
        self.typecheck1_nodes(ctx)
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
