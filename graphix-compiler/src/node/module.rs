use crate::{
    BindId, CFlag, CompileCtx, ExecCtx, Node, ParMode, PendingImport, PrintFlag, Refs,
    Rt, Saved, Scope, Tag, TagValue, Update, UserEvent, View,
    compiler::compile,
    env::{Env, Glob, ImplDef, ImportEntry, Map, UseAnchor, scope_params},
    errf,
    expr::{
        At, BindSig, Doc, Expr, ExprId, ExprKind, ModPath, Origin, ParserContext,
        Sandbox, Sig, SigItem, SigKind, Source, TypeDefBody, TypeDefExpr, UseItem,
        WrittenAt, add_interface_modules, parser,
    },
    format_with_flags,
    ide::{ModuleRefSite, SigImplLink},
    image::{
        self, ImageBuf,
        nodes::{NodeTag, decode_node, decode_nodes, encode_nodes, put_tag},
    },
    node::{Nop, Runs, bind::Bind, traits},
    profile::{self, Phase},
    typ::{
        FnArgKind, FnArgType, FnType, TVar, Type,
        tvar::{AtLevel, ForeignWrite, InTask, Level, OwnWrites, TaskId, new_task},
    },
    wrap,
};
use ahash::{AHashMap, AHashSet};
use anyhow::{Context, Result, anyhow, bail};
use arcstr::{ArcStr, literal};
use compact_str::{CompactString, format_compact};
use enumflags2::BitFlags;
use netidx_core::{
    pack::{Pack, PackError},
    path::Path,
};
use netidx_value::{Typ, Value};
use poolshark::local::LPooled;
use std::{fmt::Write, mem, sync::LazyLock};
use triomphe::Arc;

/// What one item of a `use` installs.
enum UseInstall {
    Glob(Glob),
    Import {
        key: CompactString,
        entry: ImportEntry,
    },
    /// Nothing: the prelude provides it, or this site was compiled
    /// here already (a `.gxi` use is registered by the signature and
    /// again where it is spliced into the body).
    Nothing,
}

/// Resolve one item of a `use` against the namespace table
/// ([`crate::env::Env::names`]): its module prefix, then a glob source
/// or an explicit [`ImportEntry`].
/// The glob the `use` item `item` installs at `scope`; `None` when it
/// is no glob.
fn use_glob(env: &Env, scope: &ModPath, item: &UseItem) -> Result<Option<Glob>> {
    if !item.is_glob() {
        return Ok(None);
    }
    let parts: LPooled<Vec<&str>> = Path::parts(&*item.path.0).collect();
    let Some((_, prefix)) = parts.split_last() else { bail!("use: empty path") };
    Ok(Some(match env.use_anchor(scope, prefix)? {
        None => bail!("a glob needs a path prefix"),
        Some(UseAnchor::Chain(a)) => Glob::Chain(ModPath(Path::from(ArcStr::from(a)))),
        Some(UseAnchor::Module(m)) => Glob::Module(m),
    }))
}

fn resolve_use_item(
    env: &mut Env,
    pos: crate::SourcePosition,
    ori: &Arc<Origin>,
    scope: &Scope,
    item: &UseItem,
) -> Result<UseInstall> {
    let modpath = |p: &str| ModPath(Path::from(ArcStr::from(p)));
    let parts: LPooled<Vec<&str>> = Path::parts(&*item.path.0).collect();
    let Some((&base, prefix)) = parts.split_last() else { bail!("use: empty path") };
    let key: &str = item.bound_name().unwrap_or(base);
    let compiled_here = env
        .names
        .get(&scope.lexical)
        .and_then(|sn| sn.imports.get(key))
        .is_some_and(|e| e.pos == pos && *e.ori == **ori);
    if compiled_here {
        return Ok(UseInstall::Nothing);
    }
    if let Some(g) = use_glob(env, &scope.lexical, item)? {
        return Ok(UseInstall::Glob(g));
    }
    let anchor = env.use_anchor(&scope.lexical, prefix)?;
    let (target, keyword_anchored) = match anchor {
        Some(UseAnchor::Chain(a)) => (modpath(a), true),
        Some(UseAnchor::Module(m)) => (m, false),
        None => {
            // `use m;` — a single segment names a module; importing
            // it means importing the name from its parent
            let p = ModPath(Path::from_iter([base]));
            match env.canonical_modpath(&scope.lexical, &p)? {
                Some(m) => (modpath(Path::dirname(&*m).unwrap_or("/")), false),
                None => bail!("use: no module `{base}` in scope"),
            }
        }
    };
    let name_at = match &item.rename {
        Some(n) => n.pos_or(pos),
        None => item.at.0.last().copied().unwrap_or(pos),
    };
    let entry = ImportEntry {
        scope: target,
        name: base.into(),
        keyword_anchored,
        pos,
        ori: ori.clone(),
        name_at,
        used: Default::default(),
    };
    // the prelude already provides every package name as a path root
    if &**entry.scope == "/" && entry.name == key && env.package_roots.contains(key) {
        return Ok(UseInstall::Nothing);
    }
    if env.ide.is_lsp() {
        let canonical = ModPath(entry.scope.append(&entry.name));
        env.push_module_reference(ModuleRefSite {
            pos,
            ori: ori.clone(),
            name: item.path.clone(),
            canonical,
            def_ori: None,
            segments: Some(item.at.clone()),
        });
    }
    Ok(UseInstall::Import { key: key.into(), entry })
}

/// Compile the items of a `use` statement, or of a signature's `use`,
/// into the namespace table. Every item's prefix resolves against the
/// scope as it stood before the statement: an item never sees a name
/// its sibling installs.
pub(crate) fn compile_use_items(
    env: &mut Env,
    pending: &mut Vec<PendingImport>,
    pos: crate::SourcePosition,
    ori: &Arc<Origin>,
    scope: &Scope,
    replace: bool,
    reexport: bool,
    items: &[UseItem],
) -> Result<()> {
    if reexport {
        bail!("re-exports (`pub use`) are not yet supported")
    }
    let installs = items
        .iter()
        .map(|item| resolve_use_item(env, pos, ori, scope, item))
        .collect::<Result<LPooled<Vec<_>>>>()?;
    for install in installs.iter() {
        match install {
            UseInstall::Nothing => (),
            UseInstall::Glob(g) => env.import_glob(&scope.lexical, g.clone()),
            UseInstall::Import { key, entry } => {
                if !env.import_target_exists(entry) {
                    pending.push(PendingImport {
                        scope: scope.lexical.clone(),
                        key: key.clone(),
                        pos,
                        ori: ori.clone(),
                    });
                }
                env.import(&scope.lexical, key, entry.clone(), replace)?
            }
        }
    }
    Ok(())
}

/// Compile a `use` statement: every item registers in the namespace
/// table; the graph gets a [`Nop`].
pub(crate) fn compile_use<R: Rt, E: UserEvent>(
    ctx: &mut CompileCtx<R, E>,
    flags: BitFlags<CFlag>,
    spec: Arc<Expr>,
    scope: &Scope,
    reexport: bool,
    items: &Arc<[UseItem]>,
) -> Result<Node<R, E>> {
    compile_use_items(
        &mut ctx.env,
        &mut ctx.pending_imports,
        spec.pos,
        &spec.ori,
        scope,
        flags.contains(CFlag::ReplaceImports),
        reexport,
        items,
    )
    .at(&spec)?;
    crate::recheck_pending_contractive(ctx)?;
    Ok(Nop::new(Type::Bottom))
}

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
    for (i, si) in sig.items.iter().enumerate() {
        let ori = si.ori.clone();
        bind_sig_item(env, pending, scope, i, si, &ori)
            .map_err(|e| e.context(ParserContext { ori, pos: si.pos }))?;
    }
    Ok(())
}

/// Bind the interface's item `si`, the `item`th.
fn bind_sig_item(
    env: &mut Env,
    pending: &mut Vec<PendingImport>,
    scope: &Scope,
    item: usize,
    si: &SigItem,
    si_ori: &Arc<Origin>,
) -> Result<()> {
    match &si.kind {
        SigKind::Module(name) => {
            let scope = scope.append(name);
            env.modules.insert_cow(scope.lexical.clone());
            if env.ide.is_lsp() {
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
            compile_use_items(env, pending, si.pos, si_ori, scope, false, false, names)?;
        }
        SigKind::Bind(BindSig { name, typ }) => {
            let typ = typ.scope_refs(&scope.lexical).rewrite_trait_args(env)?;
            typ.alias_tvars(&mut LPooled::take());
            // a declared signature has no gate to close over its cells:
            // every one is a quantifier, a constructor trait's element
            // included, copied at each use
            if let Type::Fn(ft) = &typ {
                ft.generalize(0)
            }
            if env.ide.is_lsp() {
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
                env.poly_binds.insert(id);
            }
        }
        SigKind::TypeDef(td) => {
            let (params, body) = Type::reset_typedef_tvars(&td.params, &td.body);
            env.deftype(
                &scope.lexical,
                &td.name,
                params,
                &body,
                true,
                si.doc.0.clone(),
                td.name.pos_or(si.pos),
                si.pos,
                si_ori.clone(),
            )?;
        }
        SigKind::Trait(t) => {
            let tref = traits::trait_ref(&scope.lexical, &t.name, si.pos, si_ori);
            let mut sigs = t
                .methods
                .iter()
                .map(|m| {
                    traits::method_decl(env, m, &tref, &scope.lexical, &t.name, si.pos)
                })
                .collect::<Result<LPooled<Vec<_>>>>()?;
            env.deftrait(
                &scope.lexical,
                &t.name,
                sigs.drain(..),
                si.doc.0.clone(),
                t.name.pos_or(si.pos),
                si_ori.clone(),
            )?;
        }
        SigKind::Impl(im) => {
            traits::impl_outside_functions(&scope.lexical, &im.trait_name)?;
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
            // named by its place in the interface, which every load of
            // the module, an image's included, gives it alike
            let bscope = scope.append_block("impl", item as u64);
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
            for m in inner_env.modules.iter().filter(|m| under(m)) {
                env.modules.insert_cow(m.clone());
            }
            macro_rules! copy_sig {
                ($kind:ident) => {
                    for (path, inner) in inner_env.$kind.iter().filter(|(p, _)| under(p))
                    {
                        env.$kind.insert_cow(path.clone(), inner.clone());
                    }
                };
            }
            copy_sig!(binds);
            copy_sig!(typedefs);
            copy_sig!(traits);
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

/// An impl head as the type of a function over it, quantified by the
/// head's variables, so a declaration and its implementation compare by
/// [`FnType::sig_matches`]: shape, variables and bounds.
fn head_sig(im: &ImplDef) -> FnType {
    impl_sig(&im.target, &im.params)
}

/// [`head_sig`] of a head `impl<params> .. for target`.
fn impl_sig(target: &Type, params: &[TVar]) -> FnType {
    FnType {
        args: Arc::from_iter([FnArgType {
            kind: FnArgKind::Positional { name: None },
            typ: target.clone(),
        }]),
        quantifiers: Arc::from_iter(params.iter().map(|tv| tv.name.clone())),
        ..FnType::default()
    }
}

/// A type or trait the interface `si` declares is the implementation's
/// already, spliced into its body from the interface: the body's own
/// definition of one is refused, but for a hidden type's, an abstract with
/// the declared parameters.
fn check_redefinition<R: Rt, E: UserEvent>(
    scope: &ModPath,
    si: &crate::expr::SigItem,
    nodes: &[Node<R, E>],
) -> Result<()> {
    // a spliced item shares the interface item's origin
    let own = |name: &dyn Fn(&ExprKind) -> bool| {
        nodes
            .iter()
            .map(|n| n.spec())
            .find(|e| !Arc::ptr_eq(&e.ori, &si.ori) && name(&e.kind))
    };
    match &si.kind {
        SigKind::Trait(t) => {
            let named =
                |k: &ExprKind| matches!(k, ExprKind::Trait(t2) if t2.name == t.name);
            if let Some(e) = own(&named) {
                return Err(anyhow!(
                    "trait {} is declared by its interface; delete this definition",
                    t.name
                )
                .at(e));
            }
        }
        SigKind::TypeDef(sig_td) => {
            let named = |k: &ExprKind| matches!(k, ExprKind::TypeDef(td) if td.name == sig_td.name);
            let Some(e) = own(&named) else { return Ok(()) };
            let ExprKind::TypeDef(td) = &e.kind else { unreachable!() };
            check_hidden_typedef(scope, sig_td, td).at(e)?
        }
        _ => (),
    }
    Ok(())
}

/// The implementation's definition `td` of a type the interface declares
/// as `sig_td`: the interface hides it, and `td` is an abstract with the
/// declared parameters.
fn check_hidden_typedef(
    scope: &ModPath,
    sig_td: &TypeDefExpr,
    td: &TypeDefExpr,
) -> Result<()> {
    if !matches!(&sig_td.body, TypeDefBody::Abstract(None)) {
        bail!("{} is defined by its interface; delete this definition", td.name)
    }
    let sig_params = scope_params(&sig_td.params, scope);
    for (tv0, con0) in scope_params(&td.params, scope).iter() {
        match sig_params.iter().find(|(tv1, _)| tv0.name == tv1.name) {
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
    if !matches!(&td.body, TypeDefBody::Abstract(_)) {
        bail!(
            "{} is hidden by the interface, so its definition must be \
             `type {} = Abstract<..>` (a Rust-backed type declares \
             `type {};`)",
            td.name,
            td.name,
            td.name
        )
    }
    Ok(())
}

/// The env as the interface `sig` of the names at `scope` sees it: the
/// names it imports and the types and traits it declares, never what
/// the implementation adds there.
fn interface_env(env: &Env, scope: &ModPath, sig: &Sig) -> Result<Env> {
    let mut imports: LPooled<AHashSet<&str>> = LPooled::take();
    let mut globs: LPooled<Vec<Glob>> = LPooled::take();
    let mut declared: LPooled<AHashSet<&str>> = LPooled::take();
    for si in sig.items.iter() {
        match &si.kind {
            SigKind::Use { names, .. } => {
                for item in names.iter() {
                    match use_glob(env, scope, item)? {
                        Some(g) => globs.push(g),
                        None => imports.extend(item.bound_name()),
                    }
                }
            }
            SigKind::TypeDef(td) => {
                declared.insert(td.name.as_str());
            }
            SigKind::Trait(t) => {
                declared.insert(t.name.as_str());
            }
            SigKind::Bind(_) | SigKind::Impl(_) | SigKind::Module(_) => (),
        }
    }
    Ok(env.interface_view(
        scope,
        |k| imports.contains(k),
        |g| globs.contains(g),
        |n| declared.contains(n),
    ))
}

/// Refuse a name the interface item `si` writes that the interface
/// neither imports nor declares: each resolves in `view`
/// ([`interface_env`]) alone, which fills its cell. `val` is a `val`
/// item's declared type.
fn check_interface_names(
    view: &Env,
    scope: &ModPath,
    si: &SigItem,
    val: Option<&Type>,
) -> Result<()> {
    match &si.kind {
        SigKind::Bind(_) => val.map_or(Ok(()), |t| t.resolve_names_only_in(view)),
        SigKind::TypeDef(td) => {
            let Some(def) =
                view.typedefs.get(scope).and_then(|m| m.get(td.name.as_str()))
            else {
                return Ok(());
            };
            for (_, c) in def.params().iter() {
                c.as_ref().map_or(Ok(()), |c| c.resolve_names_only_in(view))?
            }
            def.typ().resolve_names_only_in(view)?;
            match (&td.body, &def.rep) {
                (TypeDefBody::Abstract(None), _) | (_, None) => Ok(()),
                (_, Some(rep)) => rep.resolve_names_only_in(view),
            }
        }
        SigKind::Trait(t) => {
            let Some(id) = view.traits.get(scope).and_then(|m| m.get(t.name.as_str()))
            else {
                return Ok(());
            };
            let def = view.trait_def(*id).expect("a declared trait's def");
            def.methods
                .iter()
                .try_for_each(|m| Type::Fn(m.typ.clone()).resolve_names_only_in(view))
        }
        SigKind::Impl(im) => {
            if view.lookup_trait(scope, &im.trait_name)?.is_none() {
                bail!("no trait `{}` in the interface's scope", im.trait_name)
            }
            im.target.scope_refs(scope).resolve_names_only_in(view)?;
            im.constraints
                .iter()
                .try_for_each(|(_, c)| c.scope_refs(scope).resolve_names_only_in(view))
        }
        SigKind::Module(_) | SigKind::Use { .. } => Ok(()),
    }
}

/// Check the implementation against `sig`, adding the proxies it links;
/// their references are recorded once every item matched, and a refused
/// check adds none.
fn check_sig<R: Rt, E: UserEvent>(
    ctx: &mut CompileCtx<R, E>,
    top_id: ExprId,
    proxy: &mut Vec<Proxy>,
    scope: &Scope,
    sig: &Sig,
    nodes: &[Node<R, E>],
) -> Result<()> {
    let _profile = profile::phase(Phase::ModuleSignature);
    let first_proxy = proxy.len();
    if let Err(e) = check_sig_items(ctx, proxy, scope, sig, nodes) {
        proxy.truncate(first_proxy);
        return Err(e);
    }
    for Proxy { inner, outer, .. } in proxy[first_proxy..].iter() {
        ctx.record_ref(*inner, top_id);
        ctx.record_ref(*outer, top_id);
    }
    Ok(())
}

fn check_sig_items<R: Rt, E: UserEvent>(
    ctx: &mut CompileCtx<R, E>,
    proxy: &mut Vec<Proxy>,
    scope: &Scope,
    sig: &Sig,
    nodes: &[Node<R, E>],
) -> Result<()> {
    // the interface's types and traits first: a val's type names them
    let view = interface_env(&ctx.env, &scope.lexical, sig)?;
    for si in sig.items.iter() {
        let val = match &si.kind {
            SigKind::Bind(BindSig { name, .. }) => ctx
                .env
                .binds
                .get(&scope.lexical)
                .and_then(|m| m.get(name.name.as_str()))
                .and_then(|id| ctx.env.by_id.get(id))
                .map(|b| &b.typ),
            _ => None,
        };
        check_redefinition(&scope.lexical, si, nodes)
            .and_then(|()| check_interface_names(&view, &scope.lexical, si, val))
            .map_err(|e| e.context(ParserContext { ori: si.ori.clone(), pos: si.pos }))?;
    }
    let mut has_bind: LPooled<AHashSet<CompactString>> = LPooled::take();
    // a name a later `let` shadows is not the one the interface exports
    let mut effective: LPooled<AHashMap<CompactString, BindId>> = LPooled::take();
    for n in nodes {
        if let Some(bind) = n.downcast_ref::<Bind<R, E>>() {
            bind.pattern.ids(&mut |id| {
                if let Some(b) = ctx.env.by_id.get(&id) {
                    effective.insert(b.name.clone(), id);
                }
            });
        }
    }
    for n in nodes {
        let mut check_node = || -> Result<()> {
            if let Some(bind) = n.downcast_ref::<Bind<R, E>>()
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
                    if effective.get(&name) != Some(&id) {
                        continue;
                    }
                    let Some(proxy_id) = binds.get(&name) else { continue };
                    let Some(proxy_bind) = ctx.env.by_id.get(proxy_id) else { continue };
                    let typ = if single.is_some() { bind.typ() } else { &inner.typ };
                    proxy_bind.typ.unbind_tvars();
                    if let Err(e) = proxy_bind.typ.sig_matches(&ctx.env, typ) {
                        let typ = format_with_flags(PrintFlag::DerefTVars, || {
                            format_compact!("{typ}")
                        });
                        bail!(
                            "val {name} is declared {} but implemented {typ}: {e:#}",
                            proxy_bind.typ
                        )
                    }
                    proxy.push(Proxy {
                        inner: id,
                        outer: *proxy_id,
                        private_inner: true,
                    });
                    if ctx.env.ide.is_lsp() {
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
            Ok(())
        };
        check_node().at(n.spec())?;
    }
    for si in sig.items.iter() {
        let at = |e: anyhow::Error| {
            let ori = si.ori.clone();
            e.context(ParserContext { ori, pos: si.pos })
        };
        let missing = (|| -> Result<bool> { Ok(match &si.kind {
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
                    n.downcast_ref::<traits::Impl<R, E>>().filter(|i| {
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
                        head_sig(&declared)
                            .sig_matches(&ctx.env, &head_sig(&i.def))
                            .map_err(|e| {
                                anyhow!(
                                    "impl {} for {} does not implement the declared impl {} for {target}: {e:#}",
                                    im.trait_name,
                                    i.def.target,
                                    im.trait_name
                                )
                                .at(i.spec())
                            })?;
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
                        }
                        false
                    }
                }
            }
            SigKind::Trait(_)
            | SigKind::TypeDef(_)
            | SigKind::Module(_)
            | SigKind::Use { .. } => false,
        }) })()
        .map_err(at)?;
        if missing {
            return Err(at(anyhow!("sig item {si} is missing an implementation")));
        }
    }
    Ok(())
}

/// A script's interface over its names at `scope` and its statements
/// `nodes`, once it compiled: each `val` is the binding of its name
/// there, of a type that matches the declaration; each declared impl is
/// implemented, under the declared head; and the script's own types and
/// traits agree with the declared ones, as a module's do (the
/// interface's others were spliced into the script).
pub fn check_script_sig<R: Rt, E: UserEvent>(
    ctx: &CompileCtx<R, E>,
    sig: &Sig,
    scope: &ModPath,
    nodes: &[Node<R, E>],
) -> Result<()> {
    let view = interface_env(&ctx.env, scope, sig)?;
    for si in sig.items.iter() {
        let at = |e: anyhow::Error| {
            let ori = si.ori.clone();
            e.context(ParserContext { ori, pos: si.pos })
        };
        let missing = || at(anyhow!("sig item {si} is missing an implementation"));
        if !matches!(si.kind, SigKind::Bind(_)) {
            check_interface_names(&view, scope, si, None).map_err(at)?
        }
        match &si.kind {
            SigKind::Bind(BindSig { name, typ: declared }) => {
                let bind = ctx
                    .env
                    .binds
                    .get(scope)
                    .and_then(|m| m.get(name.name.as_str()))
                    .and_then(|id| ctx.env.by_id.get(id));
                let Some(bind) = bind else { return Err(missing()) };
                let declared = declared
                    .scope_refs(scope)
                    .rewrite_trait_args(&ctx.env)
                    .map_err(at)?;
                if let Type::Fn(ft) = &declared {
                    ft.generalize(0)
                }
                check_interface_names(&view, scope, si, Some(&declared)).map_err(at)?;
                if let Err(e) = declared.sig_matches(&ctx.env, &bind.typ) {
                    let typ = format_with_flags(PrintFlag::DerefTVars, || {
                        format_compact!("{}", bind.typ)
                    });
                    return Err(anyhow!(
                        "val {name} is declared {declared} but implemented {typ}: {e:#}"
                    )
                    .context(ParserContext { ori: bind.ori.clone(), pos: bind.pos }));
                }
                // the language server sees the val as a module's: a binding
                // of its own, with its doc, linked to the script's
                if ctx.env.ide.is_lsp() {
                    let sig = crate::env::Bind {
                        id: BindId::new(),
                        typ: declared.clone(),
                        doc: si.doc.0.clone(),
                        scope: scope.clone(),
                        name: name.name.as_str().into(),
                        pos: name.pos_or(si.pos),
                        ori: si.ori.clone(),
                        binder: None,
                    };
                    let link = SigImplLink {
                        scope: scope.clone(),
                        name: sig.name.clone(),
                        sig_id: sig.id,
                        impl_id: bind.id,
                    };
                    ctx.env.with_ide(|ide| ide.binds.push(sig));
                    ctx.env.push_sig_link(link);
                }
            }
            SigKind::Impl(im) => {
                let target = im.target.scope_refs(scope);
                let tid = match ctx.env.lookup_trait(scope, &im.trait_name) {
                    Ok(Some(tid)) => tid,
                    Ok(None) => return Err(missing()),
                    Err(e) => return Err(at(e)),
                };
                let Some(entry) = ctx.env.impl_entry(tid, &target).map_err(at)? else {
                    return Err(missing());
                };
                let trait_def = ctx.env.trait_def(tid).cloned().expect("a trait's def");
                let (st, sp) = traits::impl_head(&ctx.env, scope, &trait_def, im, true)
                    .map_err(at)?;
                impl_sig(&st, &sp).sig_matches(&ctx.env, &head_sig(&entry)).map_err(
                    |e| {
                        at(anyhow!(
                            "impl {} for {} does not implement the declared impl {} for \
                         {target}: {e:#}",
                            im.trait_name,
                            entry.target,
                            im.trait_name
                        ))
                    },
                )?;
            }
            SigKind::TypeDef(_) | SigKind::Trait(_) => {
                check_redefinition(scope, si, nodes).map_err(at)?
            }
            SigKind::Module(_) | SigKind::Use { .. } => (),
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
    spec: Arc<Expr>,
    flags: BitFlags<CFlag>,
    body: Body<R, E>,
    env: Env,
    sig: Sig,
    pub(crate) scope: Scope,
    proxy: Vec<Proxy>,
    pub(crate) nodes: Box<[Node<R, E>]>,
    /// catch-statement indices in `nodes` (see `Block::catches`).
    pub(crate) catches: Box<[usize]>,
    /// The statements as runs for forking, as a block's are.
    pub(crate) runs: Runs,
    top_id: ExprId,
    /// The compile task a static body compiles and checks in: the cells
    /// it creates are its own, and its check writes no others.
    task: TaskId,
    resident: TagValue,
}

/// Store a production for `id` as the binding that made it does, a
/// bottom included.
fn store_production<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<'_, R, E>,
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
    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<'_, R, E>,
        buf: &mut &[u8],
    ) -> Result<Node<R, E>, PackError> {
        let spec = Arc::new(Expr::decode(buf)?);
        let flags = image::flags_decode(buf)?;
        let body = match u8::decode(buf)? {
            0 => Body::Static,
            1 => Body::Dynamic {
                source: decode_node(ctx, buf)?,
                sig_env: image::lexical_decode(buf)?,
            },
            _ => return Err(PackError::UnknownTag),
        };
        let env = image::lexical_decode(buf)?;
        let sig = Sig::decode(buf)?;
        let scope = image::scope_decode(buf)?;
        let proxy = Vec::<Proxy>::decode(buf)?;
        let nodes = decode_nodes(ctx, buf)?.into_boxed_slice();
        let catches = Vec::<usize>::decode(buf)?.into_boxed_slice();
        let runs = Runs::image_decode(buf)?;
        let top_id = ExprId::decode(buf)?;
        // the registrations check_sig made
        for Proxy { inner, outer, .. } in proxy.iter() {
            ctx.record_ref(*inner, top_id);
            ctx.record_ref(*outer, top_id);
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
            runs,
            top_id,
            task: TaskId::OUTSIDE,
            resident: TagValue::phantom(),
        }))
    }

    pub(super) fn compile_dynamic(
        ctx: &mut CompileCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: Arc<Expr>,
        enclosing: &Scope,
        scope: &Scope,
        sandbox: Sandbox,
        sig: Sig,
        source: Arc<Expr>,
        top_id: ExprId,
    ) -> Result<Node<R, E>> {
        if let Some(si) =
            sig.items.iter().find(|si| matches!(si.kind, SigKind::Module(_)))
        {
            return Err(anyhow!(
                "a dynamic module has no sub-modules, so its sig cannot declare `{si}`"
            )
            .context(ParserContext { ori: si.ori.clone(), pos: si.pos }));
        }
        // the source expression compiles in the enclosing scope; only
        // the loaded text compiles under the module's own scope
        let source = compile(ctx, flags, (*source).clone(), enclosing, top_id)?;
        let mut env = ctx.env.apply_sandbox(&sandbox).context("applying sandbox")?;
        env.modules.insert_cow(scope.lexical.clone());
        bind_sig(&mut ctx.env, &mut ctx.pending_imports, &scope, &sig)
            .context("binding module signature")?;
        // the interface grants the traits it declares impls of, wherever
        // the sandbox leaves them
        for si in sig.items.iter() {
            if let SigKind::Impl(im) = &si.kind
                && let Some(tid) = ctx.env.lookup_trait(&scope.lexical, &im.trait_name)?
            {
                env.grant_trait(tid)
            }
        }
        Ok(Node::new(Self {
            spec,
            flags,
            env: env.lexical(),
            sig,
            body: Body::Dynamic { source, sig_env: ctx.env.lexical() },
            scope: scope.clone(),
            proxy: Vec::new(),
            nodes: Box::new([]),
            catches: Box::new([]),
            runs: Runs::default(),
            top_id,
            task: TaskId::OUTSIDE,
            resident: TagValue::phantom(),
        }))
    }

    pub(super) fn compile_static(
        ctx: &mut CompileCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: Arc<Expr>,
        scope: &Scope,
        sig: Sig,
        exprs: Arc<[Expr]>,
        top_id: ExprId,
    ) -> Result<Node<R, E>> {
        let task = new_task();
        let _task = InTask::enter(task);
        let mut env = ctx.env.lexical();
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
            runs: Runs::default(),
            top_id,
            task,
            resident: TagValue::phantom(),
        };
        let _module = profile::module(&scope.lexical);
        t.compile_inner(ctx, &exprs)
            .with_context(|| format_compact!("compiling module {}", scope.lexical))?;
        Ok(Node::new(t))
    }

    /// Compile loaded text as the module's body, with the end of a
    /// program statement's checks: a deferred import must name
    /// something, and analysis checks the definition assertions it
    /// reaches. A loaded body is never fused, so no fusion pass
    /// reconciles its registry attributes. A failure leaves nothing it
    /// registered.
    fn compile_source(
        &mut self,
        ctx: &mut ExecCtx<'_, R, E>,
        text: ArcStr,
    ) -> Result<()> {
        let ori = Arc::new(Origin { parent: None, source: Source::Unspecified, text });
        let exprs = add_interface_modules(parser::parse((*ori).clone())?, &self.sig);
        // `names` is a global registry: a recompile must scrub the
        // previous source's imports or they accumulate
        ctx.env.clear_names_under(&self.scope.lexical);
        let saved = Saved::take(ctx);
        let env = self.env.clone();
        let sig_env = match &self.body {
            Body::Static => None,
            Body::Dynamic { sig_env, .. } => Some(sig_env.clone()),
        };
        let pending = mem::take(&mut ctx.pending_imports);
        let names = mem::take(&mut ctx.pending_names);
        let census = ctx.attr_census.lock().len();
        // the body is a program of its own: top-level cells, and a check
        // that settles what it deferred before it elaborates
        let _level = AtLevel::enter(Level::TOP);
        ctx.pending_settles.push(crate::SettleFrame::default());
        let res = self.compile_inner(ctx, &exprs).and_then(|()| {
            crate::check_pending_imports(ctx)?;
            self.nodes.iter().try_for_each(|n| crate::analysis::analyze(n, ctx).map(drop))
        });
        ctx.pending_settles.pop().expect("load settle frame");
        ctx.attr_census.lock().truncate(census);
        ctx.pending_imports = pending;
        ctx.pending_names = names;
        if res.is_err() {
            // before `drop_deferred`: a reference the body took under the
            // module's statement cancels while it is still pending
            self.clear_compiled(ctx);
            ctx.drop_deferred();
            self.env = env;
            if let (Body::Dynamic { sig_env, .. }, Some(saved_sig)) =
                (&mut self.body, sig_env)
            {
                *sig_env = saved_sig;
            }
            saved.restore(ctx);
        }
        res
    }

    /// Compile the body. A static body is checked with its module
    /// statement (`typecheck0`), a loaded one here.
    fn compile_inner(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        exprs: &[Expr],
    ) -> Result<()> {
        let flags = match self.body {
            Body::Static => self.flags,
            Body::Dynamic { .. } => self.flags | CFlag::NoBuiltins,
        };
        let compiled = ctx.with_restored_mut(&mut self.env, |ctx| {
            crate::node::compile_block_children(
                ctx,
                flags,
                &self.scope,
                self.top_id,
                true,
                exprs.iter(),
            )
        });
        (self.nodes, self.catches) = compiled?;
        self.runs = Runs::default();
        if let Body::Dynamic { .. } = &self.body {
            self.check_body(ctx)
                .and_then(|()| crate::drain_pending_settles(ctx))
                .and_then(|()| crate::check_pending_names(ctx))
                .map_err(|e| crate::resite_pending_name(ctx, e))?;
            self.proxy_lambda_defs(ctx);
            self.typecheck1_nodes(ctx)?;
            crate::drain_pending_settles(ctx)?;
        }
        export_sig(&mut ctx.env, &self.env, &self.scope, &self.sig);
        Ok(())
    }

    /// Check the body's statements under the module's env, then the body
    /// against the signature.
    fn check_body(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
        let _profile = profile::phase(Phase::ModuleCheck);
        let Self { env, nodes, catches, body, top_id, proxy, scope, sig, .. } = self;
        ctx.with_restored_mut(env, |ctx| {
            super::typecheck0_statements(ctx, nodes, catches, false)
        })?;
        match body {
            Body::Static => check_sig(ctx, *top_id, proxy, scope, sig, nodes),
            Body::Dynamic { sig_env, .. } => ctx.with_restored_mut(sig_env, |ctx| {
                check_sig(ctx, *top_id, proxy, scope, sig, nodes)
            }),
        }
    }

    /// The compile task of a static body, which its statement checks in.
    pub(crate) fn static_task(&self) -> Option<TaskId> {
        matches!(self.body, Body::Static).then_some(self.task)
    }

    /// Map each signature `BindId` to its impl binding's `LambdaDef` so
    /// cross-module calls resolve statically.
    pub(crate) fn proxy_lambda_defs(&self, ctx: &mut CompileCtx<R, E>) {
        // a dynamic body's outer bindings are rebound at each load, so a
        // call through one dispatches dynamically
        if let Body::Dynamic { .. } = self.body {
            return;
        }
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
    fn typecheck1_nodes(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
        let Self { env, nodes, catches, scope, .. } = self;
        let _module = profile::module(&scope.lexical);
        ctx.with_restored_mut(env, |ctx| {
            super::typecheck1_statements(ctx, nodes, catches, false)
        })
    }

    fn clear_compiled(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        for Proxy { inner, outer, .. } in self.proxy.drain(..) {
            ctx.unref_var(inner, self.top_id);
            ctx.unref_var(outer, self.top_id);
        }
        ctx.with_restored_mut(&mut self.env, |ctx| {
            for mut n in mem::take(&mut self.nodes) {
                n.delete(ctx)
            }
        })
    }

    fn sleep_nodes(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        ctx.with_restored_mut(&mut self.env, |ctx| {
            for n in &mut self.nodes {
                n.sleep(ctx);
            }
        });
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Module<R, E> {
    /// A static body's statements take their definition's types, as a
    /// block's do; its signature is checked against them again.
    fn typecheck0_instance(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        types: &mut crate::node::lambda::InstanceTypes,
    ) -> Result<()> {
        if let Body::Dynamic { .. } = self.body {
            return self.typecheck0(ctx);
        }
        let _task = InTask::enter(self.task);
        let Self { env, nodes, catches, top_id, proxy, scope, sig, .. } = self;
        ctx.with_restored_mut(env, |ctx| {
            super::typecheck_in_order(ctx, nodes, catches, false, |n, ctx| {
                n.typecheck0_instance(ctx, types)
            })
        })
        .and_then(|()| check_sig(ctx, *top_id, proxy, scope, sig, nodes))
        .with_context(|| format_compact!("compiling module {}", scope.lexical))?;
        self.proxy_lambda_defs(ctx);
        Ok(())
    }

    fn for_each_child<'a>(&'a self, f: &mut dyn FnMut(&'a Node<R, E>)) {
        if let Body::Dynamic { source, .. } = &self.body {
            f(source)
        }
        self.nodes.iter().for_each(f)
    }

    fn for_each_child_mut(&mut self, f: &mut dyn FnMut(&mut Node<R, E>)) {
        if let Body::Dynamic { source, .. } = &mut self.body {
            f(source)
        }
        self.nodes.iter_mut().for_each(f)
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
                image::lexical_encode(sig_env, buf)?;
            }
        }
        image::lexical_encode(&self.env, buf)?;
        self.sig.encode(buf)?;
        image::scope_encode(&self.scope, buf)?;
        self.proxy.encode(buf)?;
        encode_nodes(&self.nodes, buf)?;
        image::slice_encode(&self.catches, buf)?;
        self.runs.image_encode(buf)?;
        self.top_id.encode(buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R, E>) -> &TagValue {
        let mut compiled = false;
        let mut primed: LPooled<Vec<BindId>> = LPooled::take();
        let mut src_tag = Tag::FIRED;
        let mut bottom = false;
        let src = match &mut self.body {
            Body::Static => None,
            Body::Dynamic { source, .. } => {
                let tv = source.update(ctx);
                let tag = tv.tag();
                if !tag.triggers() {
                    None
                } else if tag.is_bottom() {
                    // a taint placeholder never compiles or tears down; the
                    // loaded body runs on
                    bottom = true;
                    None
                } else {
                    Some((tv.value_cloned(), tag))
                }
            }
        };
        if let Some((v, tag)) = src {
            src_tag = tag;
            // a source that is no program reports, and the last good load
            // runs on
            match v {
                Value::String(s) => {
                    self.clear_compiled(ctx);
                    let compiled = self.compile_source(ctx, s);
                    ctx.apply_deferred();
                    if let Err(e) = compiled {
                        return self.resident.set(TagValue::tagged(
                            errf!(ERR_TAG, "compile error {e:?}"),
                            tag,
                        ));
                    }
                    ctx.core_hook_sites.lock().reloaded();
                }
                v => {
                    return self.resident.set(TagValue::tagged(
                        errf!(ERR_TAG, "unexpected {}", crate::typ::Naked(&v)),
                        tag,
                    ));
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
                    && ctx.try_publish(id, TagValue::fired(v)).is_ok()
                {
                    primed.push(id)
                }
            });
        }
        let view = if compiled { View::Birth } else { View::Cycle };
        ctx.under(view, |ctx| {
            // the interface binding and the implementation's are one
            // variable: a write to each in one cycle delivers the body's
            // now and queues the outside's, as a second write is queued
            for Proxy { inner, outer, private_inner } in &self.proxy {
                if *private_inner && let Some(tv) = ctx.event.variables.get(outer) {
                    let tv = tv.clone();
                    if ctx.event.variables.contains_key(inner) && !tv.tag().is_bottom() {
                        ctx.rt.set_var(*inner, tv.value());
                        continue;
                    }
                    store_production(ctx, *inner, &tv);
                    ctx.event.variables.insert(*inner, tv);
                }
            }
            if self.nodes.len() > 1 && ctx.fork_mode() != ParMode::Off {
                self.runs.update(ctx, &mut self.nodes, &self.catches);
            } else {
                for i in super::evaluation_order(self.nodes.len(), &self.catches) {
                    let _ = self.nodes[i].update(ctx);
                }
            }
        });
        // a prime is for the fresh body alone: left, it would fire readers
        // later in the cycle, and a forked merge would requeue it
        for id in primed.drain(..) {
            ctx.event.variables.remove(&id);
        }
        for Proxy { inner, outer, private_inner } in &self.proxy {
            let tv = if *private_inner {
                ctx.event.variables.remove(inner)
            } else {
                ctx.event.variables.get(inner).cloned()
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
            ctx.event.variables.insert(*outer, tv);
        }
        if bottom {
            self.resident.set(TagValue::tagged(Value::Null, Tag::FRESH_BOTTOM))
        } else if compiled {
            self.resident.set(TagValue::tagged(Value::Null, src_tag))
        } else {
            self.resident.ride()
        }
    }

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        if let Body::Dynamic { source, .. } = &mut self.body {
            source.delete(ctx);
        }
        self.clear_compiled(ctx);
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        if let Body::Dynamic { source, .. } = &mut self.body {
            source.sleep(ctx);
        }
        self.sleep_nodes(ctx);
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

    fn typecheck0(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
        match &mut self.body {
            Body::Dynamic { source, .. } => {
                wrap!(source, source.typecheck0(ctx))?;
                let t = Type::Primitive(Typ::String | Typ::Error);
                wrap!(source, t.check_contains(&self.env, source.typ()))?;
            }
            Body::Static => {
                let _task = InTask::enter(self.task);
                let _module = profile::module(&self.scope.lexical);
                let writes = OwnWrites::enter(self.task);
                let res = self.check_body(ctx).and_then(|()| match writes.foreign() {
                    Some(w) => bail!(
                        "the check decides a type the code around the module \
                         left open: annotate {}",
                        decided_binds(ctx, &self.scope.lexical, &w)
                    ),
                    None => Ok(()),
                });
                res.with_context(|| {
                    format_compact!("compiling module {}", self.scope.lexical)
                })?;
            }
        }
        self.proxy_lambda_defs(ctx);
        Ok(())
    }

    fn typecheck1(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
        if let Body::Dynamic { source, .. } = &mut self.body {
            wrap!(source, source.typecheck1(ctx))?;
        }
        self.typecheck1_nodes(ctx)
    }

    fn view(&self) -> crate::NodeView<'_, R, E> {
        crate::NodeView::Module(self)
    }

    fn fuse(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<Option<Node<R, E>>> {
        // A dynamic module's body compiles at run time (`compile_source`),
        // after fusion, and is never fused; its loader is not fused here.
        let _module = profile::module(&self.scope.lexical);
        for child in self.nodes.iter_mut() {
            crate::fusion::fuse(child, ctx)?;
        }
        Ok(None)
    }
}

/// The bindings outside `module` whose types hold what its check wrote,
/// by position, for its refusal.
fn decided_binds<R: Rt, E: UserEvent>(
    ctx: &CompileCtx<R, E>,
    module: &ModPath,
    w: &ForeignWrite,
) -> CompactString {
    let mut held: LPooled<Vec<(crate::SourcePosition, &str)>> = ctx
        .env
        .by_id
        .iter()
        .map(|(_, b)| b)
        .filter(|b| !Path::is_parent(&*module.0, &*b.scope.0) && w.held_by(&b.typ))
        .map(|b| (b.pos, b.name.as_str()))
        .collect();
    held.sort();
    held.dedup();
    let mut out = CompactString::new("");
    for (i, (pos, name)) in held.iter().take(3).enumerate() {
        let sep = if i == 0 { "" } else { ", " };
        let _ = write!(out, "{sep}`{name}` (line {}, column {})", pos.line, pos.column);
    }
    if out.is_empty() {
        out.push_str("the binding it decides");
    }
    out
}
