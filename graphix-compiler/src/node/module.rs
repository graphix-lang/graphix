use crate::{
    BindId, CFlag, CompileCtx, ExecCtx, Node, PendingImport, Refs, Rt, Saved, Scope, Tag,
    TagValue, Update, UserEvent,
    compiler::compile,
    env::{Env, Glob, ImplDef, ImportEntry, Map, UseAnchor, scope_params},
    errf,
    expr::{
        At, BindSig, Doc, Expr, ExprId, ExprKind, ModPath, Origin, ParserContext,
        Sandbox, Sig, SigItem, SigKind, Source, TypeDefBody, TypeDefExpr, UseItem,
        WrittenAt, add_interface_modules, parser,
    },
    ide::{ModuleRefSite, SigImplLink},
    image::{
        self, ImageBuf,
        nodes::{NodeTag, decode_node, decode_nodes, encode_nodes, put_tag},
    },
    node::{Nop, bind::Bind, traits},
    profile::{self, Phase},
    typ::{
        AbstractId, FnArgKind, FnArgType, FnType, Type,
        tvar::{AtLevel, Level},
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
use std::{any::Any, fmt::Write, mem, sync::LazyLock};
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
    let key: &str = item.rename.as_ref().map_or(base, |n| n.as_str());
    let compiled_here = env
        .names
        .get(&scope.lexical)
        .and_then(|sn| sn.imports.get(key))
        .is_some_and(|e| e.pos == pos && *e.ori == **ori);
    if compiled_here {
        return Ok(UseInstall::Nothing);
    }
    let anchor = env.use_anchor(&scope.lexical, prefix)?;
    if item.is_glob() {
        return match anchor {
            None => bail!("a glob needs a path prefix"),
            Some(UseAnchor::Chain(a)) => Ok(UseInstall::Glob(Glob::Chain(modpath(a)))),
            Some(UseAnchor::Module(m)) => Ok(UseInstall::Glob(Glob::Module(m))),
        };
    }
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
    let entry = ImportEntry {
        scope: target,
        name: base.into(),
        keyword_anchored,
        pos,
        ori: ori.clone(),
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
    spec: Expr,
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
            env.deftype(
                &scope.lexical,
                &td.name,
                td.params.clone(),
                &td.body,
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
                    let ft = traits::method_sig(env, &m.typ, &tref, &scope.lexical)?;
                    Ok((
                        m.name.name.clone(),
                        m.name.pos_or(si.pos),
                        Arc::new(ft),
                        m.self_index,
                    ))
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
            // CR claude for eric: [bug] This publishes the rep of every abstract
            // typedef with a body under a re-exported `mod sub;`. A gxi body is already
            // public from bind_sig, so the only reps this changes are an interface-less
            // descendant's own `type T = Abstract<..>`. As a result, a parent gxi that
            // lists `mod sub;` makes `outer::sub::T(5)`, `.0` and `T(p)` legal from
            // anywhere, while the same tree without the parent gxi, or a top-level
            // interface-less module, refuses them: adding an interface widens what the
            // child exposes. design/nominal_abstract_types.md calls an interface-less
            // `Abstract<..>` module-private, but book/src/modules/interfaces.md:379 and
            // the doc on Env::publish_abstract_rep call it public. For module-private,
            // delete this loop and fix those two docs; for public, register an
            // interface-less module's reps public in TypeDef::compile and fix the
            // design table; either way, pin both nestings. probe:
            // design/review-2026-10-05/repro/x-typecheck-patterns-10.sh (nested_gxi and
            // deep print "5 6"; nested_plain and flat refuse).
            // (x-typecheck-patterns-10)
            // 2026-10-08 claude: needs a ruling. As built, a top-level interface-less
            // module's rep is private (the flat case refuses), which the design table's
            // "module-private" row says; the book's "a module with no interface file
            // exports its definitions too, so its Abstract types are public newtypes"
            // was written in the same commit (927b1e75) and says the opposite. Private
            // means deleting this loop; public means TypeDef::compile registers an
            // interface-less module's reps public.
            let exported: LPooled<Vec<AbstractId>> = inner_env
                .typedefs
                .iter()
                .filter(|(path, _)| under(path))
                .flat_map(|(_, defs)| defs.iter())
                .filter_map(|(_, td)| match (td.typ(), &td.rep) {
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

/// An impl head as the type of a function over it, quantified by the
/// head's variables, so a declaration and its implementation compare by
/// [`FnType::sig_matches`]: shape, variables and bounds.
fn head_sig(im: &ImplDef) -> FnType {
    FnType {
        args: Arc::from_iter([FnArgType {
            kind: FnArgKind::Positional { name: None },
            typ: im.target.clone(),
        }]),
        quantifiers: Arc::from_iter(im.params.iter().map(|tv| tv.name.clone())),
        ..FnType::default()
    }
}

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
    let mut has_bind: LPooled<AHashSet<CompactString>> = LPooled::take();
    let mut defined_abstracts: LPooled<AHashSet<ArcStr>> = LPooled::take();
    // a name a later `let` shadows is not the one the interface exports
    let mut effective: LPooled<AHashMap<CompactString, BindId>> = LPooled::take();
    for n in nodes {
        if let Some(bind) = (&**n as &dyn Any).downcast_ref::<Bind<R, E>>() {
            bind.pattern.ids(&mut |id| {
                if let Some(b) = ctx.env.by_id.get(&id) {
                    effective.insert(b.name.clone(), id);
                }
            });
        }
    }
    for n in nodes {
        let mut check_node = || -> Result<()> {
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
                    if effective.get(&name) != Some(&id) {
                        continue;
                    }
                    let Some(proxy_id) = binds.get(&name) else { continue };
                    let Some(proxy_bind) = ctx.env.by_id.get(proxy_id) else { continue };
                    let typ = if single.is_some() { bind.typ() } else { &inner.typ };
                    proxy_bind.typ.unbind_tvars();
                    if let Err(e) = proxy_bind.typ.sig_matches(&ctx.env, typ) {
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
            if let Expr { kind: ExprKind::TypeDef(td), .. } = n.spec()
                && let Some(defs) = ctx.env.typedefs.get(&scope.lexical)
                && let Some(sig_td) = defs.get(&CompactString::from(td.name.as_str()))
            {
                let sig_td = TypeDefExpr {
                    name: td.name.clone(),
                    params: sig_td.params().clone(),
                    body: match (sig_td.typ(), &sig_td.rep) {
                        (Type::Abstract { .. }, rep) => {
                            TypeDefBody::Abstract(rep.clone())
                        }
                        (typ, _) => TypeDefBody::Alias(typ.clone()),
                    },
                };
                let impl_params = scope_params(&td.params, &scope.lexical);
                match &sig_td.body {
                    TypeDefBody::Abstract(None) => {
                        for (tv0, con0) in impl_params.iter() {
                            match sig_td
                                .params
                                .iter()
                                .find(|(tv1, _)| tv0.name == tv1.name)
                            {
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
                        // a ground union has several normal forms: one type is
                        // one by mutual containment
                        let one_type = |t0: &Type, t1: &Type| {
                            let f = BitFlags::empty();
                            !t0.has_unbound()
                                && !t1.has_unbound()
                                && t0
                                    .contains_with_flags(f, &ctx.env, t1)
                                    .unwrap_or(false)
                                && t1
                                    .contains_with_flags(f, &ctx.env, t0)
                                    .unwrap_or(false)
                        };
                        let same_body = sig_td.body == impl_body
                            || match (&sig_td.body, &impl_body) {
                                (TypeDefBody::Alias(t0), TypeDefBody::Alias(t1)) => {
                                    one_type(t0, t1)
                                }
                                _ => false,
                            };
                        if sig_td.name != td.name
                            || sig_td.params != impl_params
                            || !same_body
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
            Ok(())
        };
        check_node().at(n.spec())?;
    }
    for si in sig.items.iter() {
        let at = |e: anyhow::Error| {
            let ori = si.ori.clone().unwrap_or_else(|| Arc::new(Origin::default()));
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
            SigKind::Trait(t) => {
                // an implementation's own re-declaration must agree
                // with the interface
                let ori = si.ori.clone().unwrap_or_default();
                let tref = traits::trait_ref(&scope.lexical, &t.name, si.pos, &ori);
                let method = |m: &crate::expr::TraitMethod| {
                    traits::method_sig(&ctx.env, &m.typ, &tref, &scope.lexical)
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
                                    && match (method(b), method(a)) {
                                        (Ok(b), Ok(a)) => b.sig_matches(&ctx.env, &a).is_ok(),
                                        _ => false,
                                    }
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
        }) })()
        .map_err(at)?;
        if missing {
            return Err(at(anyhow!("sig item {si} is missing an implementation")));
        }
    }
    for Proxy { inner, outer, .. } in proxy[first_proxy..].iter() {
        ctx.record_ref(*inner, top_id);
        ctx.record_ref(*outer, top_id);
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
    /// The compile task a static body compiles and checks in: the cells
    /// it creates are its own, and its check writes no others.
    task: u32,
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
    /// A dynamic module's loader expression.
    pub(crate) fn source(&self) -> Option<&Node<R, E>> {
        match &self.body {
            Body::Static => None,
            Body::Dynamic { source, .. } => Some(source),
        }
    }

    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<'_, R, E>,
        buf: &mut &[u8],
    ) -> Result<Node<R, E>, PackError> {
        let spec = Expr::decode(buf)?;
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
            top_id,
            task: 0,
            resident: TagValue::phantom(),
        }))
    }

    pub(super) fn compile_dynamic(
        ctx: &mut CompileCtx<R, E>,
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
            top_id,
            task: 0,
            resident: TagValue::phantom(),
        }))
    }

    pub(super) fn compile_static(
        ctx: &mut CompileCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: Expr,
        scope: &Scope,
        sig: Sig,
        exprs: Arc<[Expr]>,
        top_id: ExprId,
    ) -> Result<Node<R, E>> {
        let task = crate::typ::tvar::new_task();
        let _task = crate::typ::tvar::InTask::enter(task);
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
        let exprs =
            add_interface_modules(parser::parse((*ori).clone())?, &self.sig, &ori);
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
        ctx.pending_settles.push(Vec::new());
        let res = self.compile_inner(ctx, &exprs).and_then(|()| {
            crate::check_pending_imports(ctx)?;
            self.nodes.iter().try_for_each(|n| crate::analysis::analyze(n, ctx))
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
        if let Body::Dynamic { .. } = &self.body {
            self.check_body(ctx)?;
            crate::drain_pending_settles(ctx)?;
            crate::check_pending_names(ctx)?;
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
    pub(crate) fn static_task(&self) -> Option<u32> {
        matches!(self.body, Body::Static).then_some(self.task)
    }

    /// Map each signature `BindId` to its impl binding's `LambdaDef` so
    /// cross-module calls resolve statically.
    fn proxy_lambda_defs(&self, ctx: &mut CompileCtx<R, E>) {
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
        self.top_id.encode(buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R, E>) -> &TagValue {
        let mut compiled = false;
        let mut primed: LPooled<Vec<BindId>> = LPooled::take();
        let mut src_tag = Tag::FIRED;
        let src = match &mut self.body {
            Body::Static => None,
            Body::Dynamic { source, .. } => {
                let tv = source.update(ctx);
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
                    let compiled = self.compile_source(ctx, s);
                    ctx.apply_deferred();
                    if let Err(e) = compiled {
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
                    && ctx.event.variables.try_insert(id, TagValue::fired(v)).is_ok()
                {
                    primed.push(id)
                }
            });
        }
        let init = ctx.event.init;
        if compiled {
            ctx.event.init = true;
        }
        for Proxy { inner, outer, private_inner } in &self.proxy {
            if *private_inner && let Some(tv) = ctx.event.variables.get(outer) {
                let tv = tv.clone();
                store_production(ctx, *inner, &tv);
                ctx.event.variables.insert(*inner, tv);
            }
        }
        for i in super::evaluation_order(self.nodes.len(), &self.catches) {
            let _ = self.nodes[i].update(ctx);
        }
        // a prime is for the fresh body alone: left, it would fire readers
        // later in the cycle, and a forked merge would requeue it
        for id in primed.drain(..) {
            ctx.event.variables.remove(&id);
        }
        ctx.event.init = init;
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
        if compiled {
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

    fn refs(&self, refs: &mut Refs) {
        if let Body::Dynamic { source, .. } = &self.body {
            source.refs(refs);
        }
        for n in &self.nodes {
            n.refs(refs)
        }
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
                use crate::typ::tvar::{InTask, OwnWrites};
                let _task = InTask::enter(self.task);
                let _module = profile::module(&self.scope.lexical);
                let writes = OwnWrites::enter(self.task);
                let res = self.check_body(ctx).and_then(|()| {
                    if writes.foreign() {
                        bail!(
                            "the check decides a type the code around the module \
                             left open: annotate the binding it decides"
                        )
                    }
                    Ok(())
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
