use crate::{
    BindId, CFlag, Event, ExecCtx, Node, PendingImport, Refs, Rt, Scope, Tag, TagValue,
    Update, UserEvent,
    compiler::compile,
    env::{Env, ImplDef, Map, scope_params},
    errf,
    expr::{
        BindSig, Doc, Expr, ExprId, ExprKind, ModPath, Origin, ParserContext, Sandbox,
        Sig, SigItem, SigKind, Source, TypeDefBody, TypeDefExpr, WrittenAt,
        add_interface_modules, parser,
    },
    ide::{ModuleInternalView, ModuleRefSite, SigImplLink},
    image::{
        self, ImageBuf,
        nodes::{
            NodeTag, decode_node, decode_nodes, encode_nodes, nodes_len, put_tag, tag_len,
        },
    },
    node::{bind::Bind, traits},
    profile::{self, Phase},
    typ::{AbstractId, Type},
    wrap,
};
use ahash::AHashSet;
use anyhow::{Context, Result, bail};
use arcstr::{ArcStr, literal};
use compact_str::{CompactString, format_compact};
use enumflags2::BitFlags;
use netidx_core::pack::{Pack, PackError};
use netidx_value::{Typ, Value};
use poolshark::local::LPooled;
use std::{any::Any, collections::hash_map::Entry, fmt::Write, mem, sync::LazyLock};
use triomphe::Arc;

fn bind_sig(
    env: &mut Env,
    pending: &mut Vec<PendingImport>,
    scope: &Scope,
    sig: &Sig,
) -> Result<()> {
    env.modules.insert_cow(scope.lexical.clone());
    // a sig `use self::sub::…` may precede `mod sub;`
    for si in sig.items.iter() {
        if let SigKind::Module(name) = &si.kind {
            env.modules.insert_cow(scope.append(name).lexical);
        }
    }
    for si in sig.items.iter() {
        let ori = si.ori.clone().unwrap_or_else(|| Arc::new(Origin::default()));
        bind_sig_item(env, pending, scope, si, &ori)
            .map_err(|e| e.context(ParserContext { ori, pos: si.pos }))?;
    }
    Ok(())
}

fn bind_sig_item(
    env: &mut Env,
    pending: &mut Vec<PendingImport>,
    scope: &Scope,
    si: &SigItem,
    si_ori: &Arc<Origin>,
) -> Result<()> {
    match &si.kind {
        SigKind::Module(name) => {
            let scope = scope.append(name);
            env.modules.insert_cow(scope.lexical.clone());
            if env.lsp_mode {
                env.push_module_reference(ModuleRefSite {
                    pos: name.pos_or(si.pos),
                    ori: si_ori.clone(),
                    name: ModPath::from_iter([name.name.clone()]),
                    canonical: scope.lexical.clone(),
                    def_ori: None,
                    segments: None,
                });
            }
        }
        SigKind::Use { reexport, names } => {
            if *reexport {
                bail!("re-exports (`pub use`) are not yet supported")
            }
            // `names` is a global registry keyed by scope path, so
            // registering in the outer env covers the impl compile too
            for item in names.iter() {
                super::compile_use_item(
                    env, pending, si.pos, si_ori, scope, false, item,
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
            let bind = env.bind_variable(
                &scope.lexical,
                name,
                typ,
                name.pos_or(si.pos),
                si_ori.clone(),
            );
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
                td.name.pos_or(si.pos),
                si_ori.clone(),
            )?;
        }
        SigKind::Trait(t) => {
            let tref = traits::trait_ref(&scope.lexical, &t.name, si.pos, si_ori);
            let sigs = t.methods.iter().map(|m| {
                let ft = traits::method_sig(&m.typ, &tref, &scope.lexical);
                (m.name.name.clone(), Arc::new(ft), m.self_index, m.default.is_some())
            });
            env.deftrait(
                &scope.lexical,
                &t.name,
                sigs,
                si.doc.0.clone(),
                t.name.pos_or(si.pos),
                si_ori.clone(),
            )?;
        }
        SigKind::Impl(im) => {
            // the implementation's own registration of the same
            // (trait, target) replaces these bindings
            let Some(trait_id) = env.lookup_trait(&scope.lexical, &im.trait_name)? else {
                bail!("no trait `{}` in scope", im.trait_name)
            };
            let trait_def = env.trait_def(trait_id).cloned().expect("trait def");
            let (target, params) =
                traits::impl_head(env, &scope.lexical, &trait_def, im, true)?;
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
                ori: si_ori.clone(),
            }))?;
        }
    }
    Ok(())
}

fn export_sig(env: &mut Env, inner_env: &Env, scope: &Scope, sig: &Sig) {
    let mut sub: LPooled<String> = LPooled::take();
    for si in sig.items.iter() {
        if let SigKind::Module(name) = &si.kind {
            let scope = scope.append(name);
            let at = &scope.lexical;
            sub.clear();
            write!(sub, "{}/", at.0).unwrap();
            let under = |path: &ModPath| path == at || path.starts_with(sub.as_str());
            env.modules.insert_cow(at.clone());
            for m in inner_env.modules.range::<ModPath, _>(at..).take_while(|m| under(m))
            {
                env.modules.insert_cow(m.clone());
            }
            macro_rules! copy_sig {
                ($kind:ident) => {
                    let iter = inner_env.$kind.range::<ModPath, _>(at..);
                    for (path, inner) in iter.take_while(|(path, _)| under(path)) {
                        env.$kind.insert_cow(path.clone(), inner.clone());
                    }
                };
            }
            copy_sig!(binds);
            copy_sig!(typedefs);
            copy_sig!(traits);
            let exported: LPooled<Vec<AbstractId>> = inner_env
                .typedefs
                .range::<ModPath, _>(at..)
                .take_while(|(path, _)| under(path))
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

/// A signature binding (`outer`) and the binding behind it (`inner`):
/// the implementation's own, or a trait default. A private inner
/// binding's production moves out and a write to the outer id flows in.
#[derive(Debug, Clone, Copy, netidx_derive::Pack)]
#[pack(unwrapped)]
struct Proxy {
    inner: BindId,
    outer: BindId,
    private_inner: bool,
}

fn check_sig<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<R, E>,
    top_id: ExprId,
    proxy: &mut Vec<Proxy>,
    scope: &Scope,
    sig: &Sig,
    nodes: &[Node<R, E>],
) -> Result<()> {
    let _profile = profile::phase(Phase::ModuleSignature);
    let mut has_bind: LPooled<AHashSet<CompactString>> = LPooled::take();
    let mut defined_abstracts: LPooled<AHashSet<ArcStr>> = LPooled::take();
    for n in nodes {
        if let Some(bind) = (&**n as &dyn Any).downcast_ref::<Bind<R, E>>()
            && let Some(binds) = ctx.env.binds.get(&scope.lexical)
        {
            // every name the `let` binds, each with its own binding; a
            // single name's type is the whole pattern's
            let single = bind.pattern.single_bind_id();
            let mut ids: LPooled<Vec<BindId>> = LPooled::take();
            bind.pattern.ids(&mut |id| ids.push(id));
            for id in ids.drain(..) {
                let Some(inner) = ctx.env.by_id.get(&id) else { continue };
                let name = inner.name.clone();
                let Some(proxy_id) = binds.get(&name) else { continue };
                let Some(proxy_bind) = ctx.env.by_id.get(proxy_id) else { continue };
                let typ = if single.is_some() { bind.typ() } else { &inner.typ };
                proxy_bind.typ.unbind_tvars();
                proxy_bind.typ.sig_matches(&ctx.env, typ).with_context(|| {
                    format_compact!(
                        "signature mismatch \"val {name}: ...\", signature has type {}, implementation has type {}",
                        proxy_bind.typ,
                        typ
                    )
                })?;
                proxy.push(Proxy { inner: id, outer: *proxy_id, private_inner: true });
                ctx.rt.ref_var(id, top_id);
                ctx.rt.ref_var(*proxy_id, top_id);
                if ctx.env.lsp_mode {
                    ctx.env.push_sig_link(SigImplLink {
                        scope: scope.lexical.clone(),
                        name: name.clone(),
                        sig_id: *proxy_id,
                        impl_id: id,
                    });
                }
                has_bind.insert(name);
            }
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
            let impl_params = scope_params(&td.params, &scope.lexical);
            match &sig_td.body {
                TypeDefBody::Abstract(None) => {
                    for (tv0, con0) in impl_params.iter() {
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
                    defined_abstracts.insert(td.name.name.clone());
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
                        || sig_td.params != impl_params
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
            SigKind::Bind(BindSig { name, .. }) => !has_bind.contains(name.name.as_str()),
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
                            let (inner, private_inner) = match i.def.methods.get(name) {
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
                            proxy.push(Proxy { inner, outer: *outer, private_inner });
                            ctx.rt.ref_var(inner, top_id);
                            ctx.rt.ref_var(*outer, top_id);
                        }
                        false
                    }
                }
            }
            SigKind::Trait(t) => {
                // an implementation's own re-declaration must agree
                // with the interface
                let ori = si.ori.clone().unwrap_or_default();
                let tref = traits::trait_ref(&scope.lexical, &t.name, si.pos, &ori);
                let method = |m: &crate::expr::TraitMethod| {
                    traits::method_sig(&m.typ, &tref, &scope.lexical)
                };
                for n in nodes {
                    if let Expr { kind: ExprKind::Trait(t2), .. } = n.spec()
                        && t2.name == t.name
                    {
                        let agree = t2.methods.len() == t.methods.len()
                            && t2.methods.iter().zip(t.methods.iter()).all(|(a, b)| {
                                a.name == b.name
                                    && a.self_index == b.self_index
                                    && a.default.is_some() == b.default.is_some()
                                    && method(b).sig_matches(&ctx.env, &method(a)).is_ok()
                            });
                        if !agree {
                            bail!(
                                "trait {} is declared by the interface as {t}; the \
                                 implementation's {t2} does not match",
                                t.name
                            )
                        }
                    }
                }
                false
            }
            SigKind::TypeDef(TypeDefExpr {
                name,
                body: TypeDefBody::Abstract(None),
                ..
            }) if !defined_abstracts.contains(&name.name) => {
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
/// A dynamic module's value: `null` once its text loaded, else the
/// load's error.
static DYNAMIC_TYP: LazyLock<Type> = LazyLock::new(|| {
    let t = Arc::from_iter([Type::Primitive(Typ::String.into())]);
    let err =
        Type::Error(Arc::new(Type::Variant(ERR_TAG.clone(), t, WrittenAt::NOWHERE)));
    Type::Set(Arc::from_iter([err, Type::Primitive(Typ::Null.into())]))
});

/// Where a module's body comes from.
#[derive(Debug)]
enum Body<R: Rt, E: UserEvent> {
    /// Compiled with the program.
    Static,
    /// Loaded at run time from the text `source` produces. The load's
    /// signature check runs in `sig_env`: a dynamic module not exported
    /// from its parent would otherwise lose its bound signature.
    Dynamic { source: Node<R, E>, sig_env: Env },
}

#[derive(Debug)]
pub struct Module<R: Rt, E: UserEvent> {
    spec: Expr,
    flags: BitFlags<CFlag>,
    body: Body<R, E>,
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

/// Store a production for `id` as the binding that made it does, a
/// bottom included.
fn store_production<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<R, E>,
    id: BindId,
    tv: &TagValue,
) {
    let stored = match tv.is_bottom() {
        true => TagValue::tagged(Value::Null, Tag::FRESH_BOTTOM),
        false => TagValue::fired(tv.value_cloned()),
    };
    ctx.rt.store_insert(id, stored);
}

impl<R: Rt, E: UserEvent> Module<R, E> {
    /// A dynamic module's loader expression.
    pub(crate) fn source(&self) -> Option<&Node<R, E>> {
        match &self.body {
            Body::Static => None,
            Body::Dynamic { source, .. } => Some(source),
        }
    }

    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<R, E>,
        buf: &mut &[u8],
    ) -> Result<Node<R, E>, PackError> {
        let spec = Expr::decode(buf)?;
        let flags = image::flags_decode(buf)?;
        let body = match u8::decode(buf)? {
            0 => Body::Static,
            1 => Body::Dynamic {
                source: decode_node(ctx, buf)?,
                sig_env: Env::decode(buf)?,
            },
            _ => return Err(PackError::UnknownTag),
        };
        let env = image::lexical_decode(buf)?;
        let sig = Sig::decode(buf)?;
        let scope = image::scope_decode(buf)?;
        let proxy = Vec::<Proxy>::decode(buf)?;
        let nodes = decode_nodes(ctx, buf)?.into_boxed_slice();
        let catches = Vec::<usize>::decode(buf)?.into_boxed_slice();
        let top_id = ExprId::decode(buf)?;
        // the registrations check_sig made
        for Proxy { inner, outer, .. } in proxy.iter() {
            ctx.rt.ref_var(*inner, top_id);
            ctx.rt.ref_var(*outer, top_id);
        }
        Ok(Node::new(Self {
            spec,
            flags,
            body,
            env,
            sig,
            scope,
            proxy,
            nodes,
            catches,
            top_id,
            resident: TagValue::phantom(),
        }))
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
        // the source expression compiles in the enclosing scope; only
        // the loaded text compiles under the module's own scope
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
            body: Body::Dynamic { source, sig_env: ctx.env.clone() },
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
        let mut env = ctx.env.clone();
        // the module's own path must be visible from inside it
        env.modules.insert_cow(scope.lexical.clone());
        bind_sig(&mut ctx.env, &mut ctx.pending_imports, &scope, &sig).with_context(
            || format_compact!("binding signature for module {}", scope.lexical),
        )?;
        let mut t = Self {
            spec,
            flags,
            env,
            sig,
            body: Body::Static,
            scope: scope.clone(),
            proxy: Vec::new(),
            nodes: Box::new([]),
            catches: Box::new([]),
            top_id,
            resident: TagValue::phantom(),
        };
        t.compile_inner(ctx, &exprs)
            .with_context(|| format_compact!("compiling module {}", scope.lexical))?;
        if ctx.env.lsp_mode {
            ctx.env.push_module_internal_view(ModuleInternalView {
                scope: t.scope.lexical.clone(),
                env: t.env.clone(),
            });
        }
        Ok(Node::new(t))
    }

    /// Compile loaded text as the module's body, with the end of a
    /// program statement's checks: a deferred import must name
    /// something, and analysis checks the definition assertions it
    /// reaches. A loaded body is never fused, so no fusion pass
    /// reconciles its registry attributes.
    fn compile_source(&mut self, ctx: &mut ExecCtx<R, E>, text: ArcStr) -> Result<()> {
        let ori = Arc::new(Origin { parent: None, source: Source::Unspecified, text });
        let exprs =
            add_interface_modules(parser::parse((*ori).clone())?, &self.sig, &ori);
        // `names` is a global registry: a recompile must scrub the
        // previous source's imports or they accumulate
        ctx.env.clear_names_under(&self.scope.lexical);
        let pending = mem::take(&mut ctx.pending_imports);
        let census = ctx.attr_census.lock().len();
        let res = self.compile_inner(ctx, &exprs).and_then(|()| {
            crate::check_pending_imports(ctx)?;
            self.nodes.iter().try_for_each(|n| crate::analysis::analyze(n, ctx))
        });
        ctx.attr_census.lock().truncate(census);
        ctx.pending_imports = pending;
        res
    }

    // CR claude for eric: [structure] the "covered children, then catches in
    // reverse" walk appears three times in this file (here, typecheck1_nodes,
    // update) and three more in Block; one iterator helper for all six.
    fn compile_inner(&mut self, ctx: &mut ExecCtx<R, E>, exprs: &[Expr]) -> Result<()> {
        let builtins_allowed =
            mem::replace(&mut ctx.builtins_allowed, matches!(self.body, Body::Static));
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
            let _profile = profile::phase(Phase::ModuleCheck);
            // catches last, innermost-first (see `Block::typecheck0`)
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
        ctx.builtins_allowed = builtins_allowed;
        let (nodes, catches) = nodes?;
        self.catches = catches;
        self.nodes = nodes.into_boxed_slice();
        match &mut self.body {
            Body::Static => check_sig(
                ctx,
                self.top_id,
                &mut self.proxy,
                &self.scope,
                &self.sig,
                &self.nodes,
            )?,
            Body::Dynamic { sig_env, .. } => {
                ctx.with_restored_mut(sig_env, |ctx| {
                    check_sig(
                        ctx,
                        self.top_id,
                        &mut self.proxy,
                        &self.scope,
                        &self.sig,
                        &self.nodes,
                    )
                })?;
                self.proxy_lambda_defs(ctx);
                self.typecheck1_nodes(ctx)?;
            }
        }
        export_sig(&mut ctx.env, &self.env, &self.scope, &self.sig);
        Ok(())
    }

    /// Map each signature `BindId` to its impl binding's `LambdaDef` so
    /// cross-module calls resolve statically.
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

    /// Run the children's `typecheck1` under the module's private env.
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
                // a later statement's resolution reads settled facts
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

    fn sleep_nodes(&mut self, ctx: &mut ExecCtx<R, E>) {
        ctx.with_restored_mut(&mut self.env, |ctx| {
            for n in &mut self.nodes {
                n.sleep(ctx);
            }
        });
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Module<R, E> {
    fn image_len(&self) -> usize {
        let body = 1 + match &self.body {
            Body::Static => 0,
            Body::Dynamic { source, sig_env } => {
                source.image_len() + sig_env.encoded_len()
            }
        };
        tag_len()
            + self.spec.encoded_len()
            + image::flags_len(self.flags)
            + body
            + image::lexical_len(&self.env)
            + self.sig.encoded_len()
            + image::scope_len(&self.scope)
            + self.proxy.encoded_len()
            + nodes_len(&self.nodes)
            + image::slice_len(&self.catches)
            + self.top_id.encoded_len()
    }

    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::Module, buf);
        self.spec.encode(buf)?;
        image::flags_encode(self.flags, buf)?;
        match &self.body {
            Body::Static => 0u8.encode(buf)?,
            Body::Dynamic { source, sig_env } => {
                1u8.encode(buf)?;
                source.image_encode(buf)?;
                sig_env.encode(buf)?;
            }
        }
        image::lexical_encode(&self.env, buf)?;
        self.sig.encode(buf)?;
        image::scope_encode(&self.scope, buf)?;
        self.proxy.encode(buf)?;
        encode_nodes(&self.nodes, buf)?;
        image::slice_encode(&self.catches, buf)?;
        self.top_id.encode(buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> &TagValue {
        let mut compiled = false;
        let mut src_tag = Tag::FIRED;
        let src = match &mut self.body {
            Body::Static => None,
            Body::Dynamic { source, .. } => {
                let tv = source.update(ctx, event);
                let tag = tv.tag();
                if !tag.triggers() {
                    None
                } else if tag.is_bottom() {
                    // a taint placeholder never compiles or tears down
                    return self
                        .resident
                        .set(TagValue::tagged(Value::Null, Tag::FRESH_BOTTOM));
                } else {
                    Some((tv.value_cloned(), tag))
                }
            }
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
            // prime the fresh nodes' external refs from the store: the
            // events that carried those values are long gone
            let mut refs = Refs::default();
            for n in self.nodes.iter() {
                n.refs(&mut refs);
            }
            refs.with_external_refs(|id| {
                if let Some(v) = ctx.rt.store_value(&id)
                    && let Entry::Vacant(e) = event.variables.entry(id)
                {
                    e.insert(TagValue::fired(v.clone()));
                }
            });
        }
        let init = event.init;
        if compiled {
            event.init = true;
        }
        for Proxy { inner, outer, private_inner } in &self.proxy {
            if *private_inner && let Some(tv) = event.variables.get(outer) {
                let tv = tv.clone();
                store_production(ctx, *inner, &tv);
                event.variables.insert(*inner, tv);
            }
        }
        {
            // catches last, innermost-first (see `Block::update`)
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
        for Proxy { inner, outer, private_inner } in &self.proxy {
            let tv = if *private_inner {
                event.variables.remove(inner)
            } else {
                event.variables.get(inner).cloned()
            };
            let tv = match tv {
                Some(tv) => tv,
                // a shared inner binding (a trait default) may have
                // produced long before this load
                None if compiled => match ctx.rt.store_value(inner) {
                    Some(v) => TagValue::fired(v.clone()),
                    None => continue,
                },
                None => continue,
            };
            store_production(ctx, *outer, &tv);
            event.variables.insert(*outer, tv);
        }
        if compiled {
            self.resident.set(TagValue::tagged(Value::Null, src_tag))
        } else {
            self.resident.ride()
        }
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        if let Body::Dynamic { source, .. } = &mut self.body {
            source.delete(ctx);
        }
        self.clear_compiled(ctx);
    }

    fn refs(&self, refs: &mut Refs) {
        if let Body::Dynamic { source, .. } = &self.body {
            source.refs(refs);
        }
        for n in &self.nodes {
            n.refs(refs)
        }
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        if let Body::Dynamic { source, .. } = &mut self.body {
            source.sleep(ctx);
        }
        self.sleep_nodes(ctx);
    }

    fn reset_replay(&mut self, ctx: &mut ExecCtx<R, E>) {
        if let Body::Dynamic { source, .. } = &mut self.body {
            source.reset_replay(ctx);
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
        match &self.body {
            Body::Static => self.nodes.last().map(|n| n.typ()).unwrap_or(Type::BOTTOM),
            Body::Dynamic { .. } => &DYNAMIC_TYP,
        }
    }

    fn typecheck0(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        if let Body::Dynamic { source, .. } = &mut self.body {
            wrap!(source, source.typecheck0(ctx))?;
            let t = Type::Primitive(Typ::String | Typ::Error);
            wrap!(source, t.check_contains(&self.env, source.typ()))?;
        }
        self.proxy_lambda_defs(ctx);
        Ok(())
    }

    fn typecheck1(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        if let Body::Dynamic { source, .. } = &mut self.body {
            wrap!(source, source.typecheck1(ctx))?;
        }
        self.typecheck1_nodes(ctx)
    }

    fn view(&self) -> crate::NodeView<'_, R, E> {
        crate::NodeView::Module(self)
    }

    fn fuse(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<Option<Node<R, E>>> {
        // A dynamic module's body compiles at run time (`compile_source`),
        // after fusion, and is never fused; its loader is not fused here.
        for child in self.nodes.iter_mut() {
            crate::fusion::fuse(child, ctx)?;
        }
        Ok(None)
    }
}
