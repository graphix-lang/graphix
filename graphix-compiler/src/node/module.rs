use crate::{
    BindId, CFlag, CompileCtx, ExecCtx, Node, Notify, PendingImport, PrintFlag, Refs, Rt,
    Saved, Scope, StaticCallees, Tag, TagValue, Update, View,
    compiler::compile,
    env::{
        Env, Glob, ImplDef, ImportEntry, Lexical, LoadedBody, Map, TraitDef, Unsettled,
        UseAnchor, scope_params,
    },
    errf,
    expr::{
        At, BindSig, Doc, ErrorSite, Expr, ExprId, ExprKind, ModPath, ModuleKind, Name,
        Origin, ParserContext, Sig, SigItem, SigKind, Source, Told, TypeDefBody,
        TypeDefExpr, UseItem, WrittenAt, add_interface_modules, parser,
    },
    format_with_flags,
    ide::{ModuleRefSite, SigImplLink},
    image::{
        self, ImageBuf,
        nodes::{NodeTag, decode_node, decode_nodes, encode_nodes, put_tag},
    },
    node::{self, Nop, Runs, bind::Bind, traits},
    profile::{self, Phase},
    shared_map::SharedMap,
    stack::ensure_sufficient,
    typ::{
        Brief, FnArgKind, FnArgType, FnType, TVar, Type, UnresolvableRef, Written,
        bounds,
        settle::VariableUnion,
        tvar::{ForeignWrite, InTask, OwnWrites, TaskId, new_task, settled_note},
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
use std::{
    fmt::Write,
    mem,
    sync::{LazyLock, atomic::Ordering},
};
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

/// What the `use` at statement `stmt` of the list at `scope` has still to
/// bring, into `later`: nothing where the item is installed already (a
/// `.gxi`'s `use` spliced into the body, a block compiled again) or names
/// a package, which the prelude provides.
pub(crate) fn later_use(
    env: &Env,
    scope: &ModPath,
    stmt: usize,
    spec: &Expr,
    items: &[UseItem],
    later: &mut Unsettled,
) {
    let sn = env.names.get(scope);
    for item in items {
        match item.bound_name() {
            Some(key) => {
                let here = sn
                    .and_then(|sn| sn.imports.get(key))
                    .is_some_and(|e| e.pos == spec.pos && *e.ori == *spec.ori);
                let package = item.rename.is_none()
                    && Path::levels(&*item.path.0) == 1
                    && env.package_roots.contains(key);
                if !here && !package {
                    later.uses.push((stmt, key.into()))
                }
            }
            None => {
                let here = match use_glob(env, scope, item) {
                    Ok(Some(g)) => sn.is_some_and(|sn| sn.globs.contains(&g)),
                    Ok(None) | Err(_) => false,
                };
                if !here {
                    later.globs.push(stmt)
                }
            }
        }
    }
}

/// Each module `e` declares at the level of the list it is a statement of:
/// the statement itself when it is a `mod`, and a dynamic module written
/// where a value is (`let st = mod foo dynamic { .. }`, a call's argument,
/// a dynamic module's source). What opens a scope of its own declares at
/// that scope's level, and is not entered.
pub(crate) fn declared_modules<'a>(e: &'a Expr, f: &mut impl FnMut(&'a Name)) {
    match &e.kind {
        ExprKind::Block { .. }
        | ExprKind::Lambda(_)
        | ExprKind::Select(_)
        | ExprKind::Seq(_)
        | ExprKind::Catch(_)
        | ExprKind::Trait(_)
        | ExprKind::Impl(_)
        | ExprKind::SeqMachine(_)
        | ExprKind::SeqCapture(_) => (),
        ExprKind::Module { name, value } => {
            f(name);
            if let ModuleKind::Dynamic { source, .. } = &**value {
                declared_modules(source, f)
            }
        }
        _ => ensure_sufficient(|| e.for_each_child(&mut |c| declared_modules(c, f))),
    }
}

/// Whether statement `e` of a list is a `use` or declares a module there:
/// what [`Unsettled`] records of it is no longer to come once it begins.
pub(crate) fn brings_names(e: &Expr) -> bool {
    let mut declares = false;
    declared_modules(e, &mut |_| declares = true);
    declares || matches!(e.kind, ExprKind::Use { .. })
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
    // CR claude for claude: [bug/medium] This drops every import whose target scope is
    // the root and whose name is a package, so `use super::db` (or `use package::db`)
    // in a module declared at the top level is lost where the top level declares a
    // binding `db`: the body gets `db not defined`, while `use super::db as d`, `use
    // super::*` and `super::db` all work. A dynamic module's sig `use super::str` at
    // the top level is accepted as a grant and grants nothing. Only a one-segment item
    // (`use db;`) is the prelude's: test that the item has no prefix, as `later_use`
    // and `compile_dynamic`'s package filter do, through one predicate the three share.
    // Fix it with the CR on `Env::imported_view`, or the entry this installs grants the
    // package to a dynamic body. probe:
    // design/review-2026-11-16/repro/t-env-use-super-package-named-binding.sh
    // (t-env-02)
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
pub(crate) fn compile_use<R: Rt>(
    ctx: &mut CompileCtx<R>,
    flags: BitFlags<CFlag>,
    spec: Arc<Expr>,
    scope: &Scope,
    reexport: bool,
    items: &Arc<[UseItem]>,
) -> Result<Node<R>> {
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
    crate::recheck_pending_typedefs(ctx, crate::Completed::Use)?;
    Ok(Nop::new(Type::Bottom))
}

/// The traits and impls an interface declared: with its `val`s'
/// bindings, what [`bind_sig`] registered outside the lexical maps, which
/// goes when the module does.
#[derive(Debug, Default)]
struct SigDecls {
    traits: Vec<Arc<TraitDef>>,
    impls: Vec<Arc<ImplDef>>,
}

impl SigDecls {
    /// Give the registries back what is still these declarations'.
    fn unregister(&self, env: &mut Env) {
        self.impls.iter().for_each(|im| env.unregister_impl(im));
        self.traits.iter().for_each(|def| env.unrecord_trait(def))
    }

    /// The bindings a dispatch through these declarations names.
    fn dispatch_bindings(&self) -> impl Iterator<Item = BindId> {
        let dispatchers = self.traits.iter().flat_map(|def| def.dispatchers());
        let methods = self.impls.iter().flat_map(|im| im.methods.iter());
        dispatchers.chain(methods.map(|(_, id)| *id))
    }
}

fn bind_sig(
    env: &mut Env,
    pending: &mut Vec<PendingImport>,
    scope: &Scope,
    sig: &Sig,
) -> Result<SigDecls> {
    env.lexical.modules.insert_cow(scope.lexical.clone());
    // a sig `use self::sub::…` may precede `mod sub;`
    for si in sig.items.iter() {
        if let SigKind::Module(name) = &si.kind {
            env.lexical.modules.insert_cow(scope.append(name).lexical);
        }
    }
    let mut decls = SigDecls::default();
    for (i, si) in sig.items.iter().enumerate() {
        bind_sig_item(env, pending, &mut decls, scope, i, si, &si.ori)
            .placed(&si.ori, si.pos)?;
    }
    if let Some(body) = &env.lexical.body {
        body.declare(decls.dispatch_bindings())
    }
    Ok(decls)
}

/// Bind the interface's item `si`, the `item`th.
fn bind_sig_item(
    env: &mut Env,
    pending: &mut Vec<PendingImport>,
    decls: &mut SigDecls,
    scope: &Scope,
    item: usize,
    si: &SigItem,
    si_ori: &Arc<Origin>,
) -> Result<()> {
    match &si.kind {
        SigKind::Module(name) => {
            let scope = scope.append(name);
            env.lexical.modules.insert_cow(scope.lexical.clone());
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
            let typ = typ.scope_refs(&scope.lexical).rewrite_declared_args(env)?;
            // a declared signature has no gate to close over its cells:
            // every one is a quantifier, a constructor trait's element
            // included, copied at each use, and its own are its scheme's,
            // which each use picks
            match &typ {
                Type::Fn(ft) => {
                    ft.alias_tvars(&mut LPooled::take());
                    ft.generalize(0)
                }
                typ => typ.alias_tvars(&mut LPooled::take()),
            }
            if env.ide.is_lsp() {
                typ.record_ide_refs(env, &scope.lexical);
            }
            let poly = matches!(typ, Type::Fn(_)) || {
                let mut cells: LPooled<AHashMap<usize, TVar>> = LPooled::take();
                typ.reached_cells(&mut cells);
                cells.values().any(|tv| !tv.is_bound())
            };
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
        // CR claude for claude: [bug/medium] A dynamic module in a function body whose
        // sig declares a trait passes the check, and then no source can load. The load
        // splices the trait into the body, where `Trait::compile` refuses it (`trait Sh
        // is in a function body`), for a source that never names the trait. A sig
        // `impl` there is refused at the check by `impl_outside_functions`, and a
        // static module's interface with a trait is refused at the check through its
        // splice. Refuse it here by the same test, with one helper for the trait and
        // the impl. probe:
        // design/review-2026-11-16/repro/c-module-traits-sig-trait-in-fn-body.gx
        // (c-module-traits-02)
        SigKind::Trait(t) => {
            let tref = traits::trait_ref(&scope.lexical, &t.name, si.pos, si_ori);
            let mut sigs = t
                .methods
                .iter()
                .map(|m| {
                    traits::method_decl(env, m, &tref, &scope.lexical, &t.name, si.pos)
                })
                .collect::<Result<LPooled<Vec<_>>>>()?;
            decls.traits.push(env.deftrait(
                &scope.lexical,
                &t.name,
                sigs.drain(..),
                si.doc.0.clone(),
                t.name.pos_or(si.pos),
                si_ori.clone(),
            )?);
        }
        SigKind::Impl(im) => {
            traits::impl_outside_functions(&scope.lexical, &im.trait_name)?;
            // the implementation's own registration of the same
            // (trait, target) replaces these bindings
            let Some(trait_id) = env.lookup_trait(&scope.lexical, &im.trait_name)? else {
                bail!("no trait `{}` in scope", im.trait_name)
            };
            let trait_def = env.trait_def(trait_id).expect("trait def");
            let (target, params) =
                traits::impl_head(env, &scope.lexical, &trait_def, im, true)?;
            // named by its place in the interface, which every load of
            // the module, an image's included, gives it alike
            let bscope = scope.append_block("impl", item as u64);
            let mut methods: Map<CompactString, BindId> = Map::new();
            for d in trait_def.methods.iter() {
                let typ = Type::Fn(Arc::new(traits::method_sig_at(&d.typ, &target)));
                let bind = env.bind_variable(
                    &bscope.lexical,
                    &d.name,
                    typ,
                    si.pos,
                    si_ori.clone(),
                );
                methods.insert_cow(d.name.as_str().into(), bind.id);
            }
            let def = Arc::new(ImplDef {
                trait_id,
                target,
                params,
                scope: bscope.lexical,
                methods,
                declared: true,
                pos: si.pos,
                ori: si_ori.clone(),
            });
            env.register_impl(def.clone())?;
            decls.impls.push(def);
        }
    }
    Ok(())
}

fn export_sig(env: &mut Env, inner_env: &Lexical, scope: &Scope, sig: &Sig) {
    let mut sub: LPooled<String> = LPooled::take();
    for si in sig.items.iter() {
        if let SigKind::Module(name) = &si.kind {
            let scope = scope.append(name);
            let at = &scope.lexical;
            sub.clear();
            write!(sub, "{}/", at.0).unwrap();
            let under = |path: &ModPath| path == at || path.starts_with(sub.as_str());
            env.lexical.modules.insert_cow(at.clone());
            for m in inner_env.modules.iter().filter(|m| under(m)) {
                env.lexical.modules.insert_cow(m.clone());
            }
            macro_rules! copy_sig {
                ($kind:ident) => {
                    for (path, inner) in inner_env.$kind.iter().filter(|(p, _)| under(p))
                    {
                        env.lexical.$kind.insert_cow(path.clone(), inner.clone());
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
/// the implementation's own, or a trait default. An implementation's
/// binding is the signature's variable at run time
/// ([`crate::branch::VarAliases`]); only a trait default's production is
/// relayed out and a write to the outer id relayed in.
#[derive(Debug, Clone, Copy, netidx_derive::Pack)]
#[pack(unwrapped)]
struct Proxy {
    inner: BindId,
    outer: BindId,
    private_inner: bool,
}

impl Proxy {
    /// A private inner binding is the outer one's variable at run time
    /// ([`crate::branch::VarAliases`]), and one `<-` target with it: a
    /// static call through either binds only where neither is written.
    fn join<R: Rt>(&self, ctx: &mut CompileCtx<R>) {
        if self.private_inner {
            ctx.join_interface_pair(self.inner, self.outer)
        }
    }
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

/// Refuse an implementation of `val name` whose type `typ` is not the
/// declared `declared` ([`hold_paired`]); `generalized` says its binding is
/// generic in the cells its own definitions own, and `holder` what holds a
/// cell of it ([`Holder::of`]).
// CR claude for eric: [structure/medium] A val's declared type is compared with its
// implementation's inferred type after the fact (`Type::sig_matches`, `hold_paired`,
// `FnType::sig_bounds`) instead of being given to the implementing `let` as its
// annotation, so the author writes the type twice: `val x: [i64, null]` over `let x =
// 3` is refused until the let says `: [i64, null]`, and `val f: fn(n: i64) -> [i64,
// null]` over `|n| n + 1` until the lambda says `-> [i64, null]`, though `n: i64` was
// taken from the val. In netidx-admin about 20 of the 35 vals implemented in Graphix
// restate a type (a regex count). The comparison is also a second subsumption judgment
// beside `contains`, with 220 lines of rules in design/module_system.md and an
// incompleteness it documents. Give the implementing `let` the val's type as its
// annotation, its variables rigid as a declared signature's are, and keep `sig_matches`
// for what no annotation can say. probe:
// design/review-2026-11-16/repro/a-packages-val-is-not-an-annotation.sh (a-packages-04)
fn check_val(env: &Env, name: &str, declared: &Type, typ: &Type) -> Result<()> {
    let refused = |e: anyhow::Error| {
        let settled = match settled_note(env, &[typ]) {
            Some(n) => format_compact!(": {n}"),
            None => format_compact!(""),
        };
        let typ = format_with_flags(PrintFlag::DerefTVars, || {
            format_compact!("{}", Brief(typ))
        });
        anyhow!("val {name} is declared {declared} but implemented {typ}: {e:#}{settled}")
    };
    // XCR claude for claude: [bug/critical] This comparison runs outside the module's
    // own env (Module::check_body calls check_sig after it leaves with_restored_mut),
    // so a lambda parameter whose whole annotation is a type name is compared with the
    // declared type by name: a body-private type named like one the interface sees (the
    // prelude's Number, a `use super::C`) is taken for the interface's
    // (TypeRef::same_definition answers true for two open cells of one scope and name),
    // and a private alias the interface does not see is `undefined type D in m`. So
    // `type Number = string; let g = |x: Number| x` passes under `val g: fn(x: Number)
    // -> string`, and `m::g(48)` runs string code over an i64: SIGSEGV under the JIT,
    // and from a dynamic module's body a runtime panic (`value I64(41) is not a
    // function`). Fill the implementation type's references under the module's env
    // before the comparison, or refuse a body typedef whose name an interface val
    // writes, as check_redefinition does for a type the interface declares. probe:
    // design/review-2026-11-16/repro/x-typecheck-generics-sig-shadowed-param-type.sh
    // (x-typecheck-generics-01)
    // 2026-11-18 claude: On bounds-spike the old comparison this sat on (sig_matches,
    // hold_paired) is deleted with the old check; the val is held to its declaration by
    // bounds::fits. The probe now gives: case 1 (body's Number over the prelude's)
    // refused, case 2 (dynamic body's own C) refused at the load, case 3 (private alias
    // D = i64) accepted. No fixture pins the three cases yet: pin them before this is
    // cleared.

    // XCR claude for claude: [bug/medium] Only a lambda implementation that is not
    // generalized reaches this arm, which means a written let (`let h = |x| x; h <-
    // |x| x`), and its second remedy cannot be followed. A let annotated `fn<'a:
    // Any>(..)` is refused as a quantified function type outside a definition's
    // signature. `'a: Any |x: 'a| ..` on the lambda gets this same advice again,
    // because a written binding is never generalized. Only "declare the val at one
    // type" makes the module check. Drop the second remedy here, the text
    // `rank2_val_over_a_written_let_names_the_annotation` pins for it, and the
    // "quantified annotation on the let" way out in design/module_system.md. probe:
    // design/review-2026-11-16/repro/tests-interfaces-image-quantified-let-remedy.sh
    // (tests-interfaces-image-01)
    // 2026-11-18 claude: On bounds-spike the arm and its remedy text are deleted
    // with the old check; rank2_val_over_a_written_let_names_the_annotation passes
    // by its bounds.manifest line, the check's own message. Left: the fixture's
    // written text and the 'quantified annotation on the let' way out in
    // design/module_system.md, for the manifest fold (step 4 of the plan).

    // a binding the check typed is its declaration where one instance of
    // it fits the declaration, its variables constants
    let outer = crate::typ::tvar::current_level();
    crate::typ::bounds::fits(env, typ, declared, outer).map_err(refused)
}

/// Refuse `val name: typ` where its type variable stands outside every
/// function type ([`Type::variable_outside_functions`]): that is one type
/// all the val's uses share, which no declaration may leave open.
fn check_val_variable(env: &Env, name: &str, typ: &Type) -> Result<()> {
    match typ.variable_outside_functions(env) {
        None => Ok(()),
        Some(v) => bail!(
            "val {name} has its type variable {v} outside a function type, one type \
             all its uses share: declare the val at one type"
        ),
    }
}

/// [`check_val_variable`] for each `val` of `sig`, whose binding is
/// among `sig_binds`.
fn check_val_variables(
    env: &Env,
    sig: &Sig,
    sig_binds: &Map<CompactString, BindId>,
) -> Result<()> {
    for si in sig.items.iter() {
        let SigKind::Bind(BindSig { name, .. }) = &si.kind else { continue };
        if let Some(b) =
            sig_binds.get(name.name.as_str()).and_then(|id| env.by_id.get(id))
        {
            check_val_variable(env, &name.name, &b.typ).placed(&si.ori, si.pos)?
        }
    }
    Ok(())
}

/// Refuse `implemented`, the implementation of the declared impl
/// `trait_name` for `target`, unless its head is the declared head
/// `declared` ([`impl_sig`]).
fn check_impl_head(
    env: &Env,
    trait_name: &ModPath,
    target: &Type,
    declared: &FnType,
    implemented: &ImplDef,
) -> Result<()> {
    declared.sig_matches(env, &head_sig(implemented)).map(drop).map_err(|e| {
        anyhow!(
            "impl {trait_name} for {} does not implement the declared impl \
             {trait_name} for {target}: {e:#}",
            implemented.target
        )
    })
}

/// A type or trait the interface `si` declares is the implementation's
/// already, spliced into its body from the interface: the body's own
/// definition of one is refused, but for a hidden type's, an abstract with
/// the declared parameters.
fn check_redefinition<R: Rt>(
    scope: &ModPath,
    si: &crate::expr::SigItem,
    nodes: &[Node<R>],
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
    let params = scope_params(&td.params, scope);
    if params.len() != sig_params.len() {
        bail!(
            "signature mismatch in {}, the interface declares {} parameters, the implementation {}",
            td.name,
            sig_params.len(),
            params.len()
        )
    }
    for ((tv0, con0), (tv1, con1)) in params.iter().zip(sig_params.iter()) {
        if tv0.name != tv1.name {
            bail!(
                "signature mismatch in {}, parameter {} where the interface declares {}",
                td.name,
                tv0.name,
                tv1.name
            )
        }
        if con0 != con1 {
            let show = |c: &Option<Type>| match c {
                None => CompactString::const_new("missing"),
                Some(t) => format_compact!("{t}"),
            };
            bail!(
                "signature mismatch in {}, constraint mismatch on {}, signature constraint {} vs implementation constraint {}",
                td.name,
                tv0.name,
                show(con1),
                show(con0)
            )
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
/// item's declared type, which may not write a union of two of its
/// variables in a function type ([`VariableUnion`]).
fn check_interface_names(
    view: &Env,
    scope: &ModPath,
    si: &SigItem,
    val: Option<&Type>,
) -> Result<()> {
    interface_names(view, scope, si, val).map_err(|e| told_self_target(e, si))
}

/// A refused name written outside the item `si`, before it or in another
/// source, is the target of the impl the interface sits in, which the
/// item's `self` was read as: say so, and that the interface imports it.
fn told_self_target(e: anyhow::Error, si: &SigItem) -> anyhow::Error {
    let Some(u) = e.downcast_ref::<UnresolvableRef>() else { return e };
    let (Some(pos), Some(ori)) = (u.pos, u.ori.as_ref()) else { return e };
    if **ori == *si.ori && pos >= si.pos {
        return e;
    }
    let told = format_compact!(
        "{} is the impl's type, which `self` reads in this signature; the signature \
         sees only the names it imports, so it must `use` {}",
        u.name,
        u.name
    );
    Told::over(e, &told)
}

fn interface_names(
    view: &Env,
    scope: &ModPath,
    si: &SigItem,
    val: Option<&Type>,
) -> Result<()> {
    match &si.kind {
        SigKind::Bind(BindSig { name, .. }) => val.map_or(Ok(()), |t| {
            let mut res = Ok(());
            t.for_each_written(false, &mut |w| {
                if res.is_ok() {
                    res = match w {
                        Written::Name(tr) => tr.resolve_only_in(view),
                        Written::Union(u) => match VariableUnion::of(u, &|_| true) {
                            None => Ok(()),
                            Some(e) => Err(anyhow!("val {}: {e}", name.name)),
                        },
                    }
                }
            });
            res
        }),
        SigKind::TypeDef(td) => {
            let Some(def) =
                view.lexical.typedefs.get(scope).and_then(|m| m.get(td.name.as_str()))
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
            let Some(id) =
                view.lexical.traits.get(scope).and_then(|m| m.get(t.name.as_str()))
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

/// The bindings `bind_sig` made at the module's own scope.
fn sig_binds(env: &Env, scope: &Scope) -> Map<CompactString, BindId> {
    env.lexical.binds.get(&scope.lexical).cloned().unwrap_or_default()
}

/// Each binding of the body that implements an interface `val`: the
/// index of its `let` in `nodes`, the binding, and the interface's
/// binding of its name.
fn implemented_vals<R: Rt>(
    env: &Env,
    sig_binds: &Map<CompactString, BindId>,
    nodes: &[Node<R>],
) -> LPooled<Vec<(usize, BindId, BindId)>> {
    let mut vals: LPooled<Vec<(usize, BindId, BindId)>> = LPooled::take();
    if sig_binds.is_empty() {
        return vals;
    }
    let binds = || {
        nodes
            .iter()
            .enumerate()
            .filter_map(|(i, n)| Some((i, n.downcast_ref::<Bind<R>>()?)))
    };
    // a name a later `let` shadows is not the one the interface exports
    let mut effective: LPooled<AHashMap<CompactString, BindId>> = LPooled::take();
    for (_, bind) in binds() {
        bind.pattern.ids(&mut |id| {
            if let Some(b) = env.by_id.get(&id) {
                effective.insert(b.name.clone(), id);
            }
        });
    }
    for (i, bind) in binds() {
        bind.pattern.ids(&mut |id| {
            let Some(b) = env.by_id.get(&id) else { return };
            if effective.get(&b.name) == Some(&id)
                && let Some(outer) = sig_binds.get(&b.name)
                && env.by_id.contains_key(outer)
            {
                vals.push((i, id, *outer))
            }
        });
    }
    vals
}

/// The type of `inner`, a binding of the `let` `bind` implementing a
/// `val`: a single name's type is the whole pattern's.
fn implementation_type<'a, R: Rt>(
    bind: &'a Bind<R>,
    inner: &'a crate::env::Bind,
) -> &'a Type {
    match bind.pattern.single_bind_id() {
        Some(_) => bind.typ(),
        None => &inner.typ,
    }
}

/// Join each implemented `val` with the binding implementing it before
/// the body's check resolves a call through either: a `<-` compiled
/// before the module's check, the root's or the body's own, is a write
/// to both.
fn join_implemented_vals<R: Rt>(
    ctx: &mut CompileCtx<R>,
    sig_binds: &Map<CompactString, BindId>,
    nodes: &[Node<R>],
) {
    for (_, inner, outer) in implemented_vals(&ctx.env, sig_binds, nodes).drain(..) {
        ctx.join_interface_pair(inner, outer)
    }
}

/// Check the implementation against `sig`, adding the proxies it links;
/// their references are recorded once every item matched, and a refused
/// check adds none.
fn check_sig<R: Rt>(
    ctx: &mut CompileCtx<R>,
    top_id: ExprId,
    proxy: &mut Vec<Proxy>,
    scope: &Scope,
    sig: &Sig,
    sig_binds: &Map<CompactString, BindId>,
    nodes: &[Node<R>],
) -> Result<()> {
    let _profile = profile::phase(Phase::ModuleSignature);
    let first_proxy = proxy.len();
    if let Err(e) = check_sig_items(ctx, proxy, scope, sig, sig_binds, nodes) {
        proxy.truncate(first_proxy);
        return Err(e);
    }
    for p in proxy[first_proxy..].iter() {
        ctx.record_ref(p.inner, top_id);
        ctx.record_ref(p.outer, top_id);
        p.join(ctx);
    }
    Ok(())
}

/// What the interface `sig` of the names at `scope` states by itself,
/// whatever implements it: each `val`'s variables stand inside a
/// function type, each name an item writes is one the interface imports
/// or declares, and `nodes`, the implementation's statements, redefine
/// none of its types and traits.
fn check_sig_decls<R: Rt>(
    env: &Env,
    scope: &ModPath,
    sig: &Sig,
    sig_binds: &Map<CompactString, BindId>,
    nodes: &[Node<R>],
) -> Result<()> {
    // the interface's types and traits first: a val's type names them
    let view = interface_env(env, scope, sig)?;
    check_val_variables(env, sig, sig_binds)?;
    for si in sig.items.iter() {
        let val = match &si.kind {
            SigKind::Bind(BindSig { name, .. }) => sig_binds
                .get(name.name.as_str())
                .and_then(|id| env.by_id.get(id))
                .map(|b| &b.typ),
            _ => None,
        };
        check_redefinition(scope, si, nodes)
            .and_then(|()| check_interface_names(&view, scope, si, val))
            .placed(&si.ori, si.pos)?;
    }
    Ok(())
}

fn check_sig_items<R: Rt>(
    ctx: &mut CompileCtx<R>,
    proxy: &mut Vec<Proxy>,
    scope: &Scope,
    sig: &Sig,
    sig_binds: &Map<CompactString, BindId>,
    nodes: &[Node<R>],
) -> Result<()> {
    check_sig_decls(&ctx.env, &scope.lexical, sig, sig_binds, nodes)?;
    let mut has_bind: LPooled<AHashSet<CompactString>> = LPooled::take();
    for (i, id, proxy_id) in implemented_vals(&ctx.env, sig_binds, nodes).drain(..) {
        let n = &nodes[i];
        let checked = || -> Result<CompactString> {
            let bind = n.downcast_ref::<Bind<R>>().expect("an implemented val's let");
            let (Some(inner), Some(proxy_bind)) =
                (ctx.env.by_id.get(&id), ctx.env.by_id.get(&proxy_id))
            else {
                bail!("BUG: an implemented val's binding is gone")
            };
            let typ = implementation_type(bind, inner);
            proxy_bind.typ.unbind_tvars();
            check_val(&ctx.env, &inner.name, &proxy_bind.typ, typ)?;
            if ctx.connect_targets.contains(&proxy_id) {
                ctx.check_val_written(&inner.name, &proxy_bind.typ, typ)?
            }
            Ok(inner.name.clone())
        };
        let name = checked().at(n.spec())?;
        proxy.push(Proxy { inner: id, outer: proxy_id, private_inner: true });
        if ctx.env.ide.is_lsp() {
            ctx.env.push_sig_link(SigImplLink {
                scope: scope.lexical.clone(),
                name: name.clone(),
                sig_id: proxy_id,
                impl_id: id,
            });
        }
        has_bind.insert(name);
    }
    for si in sig.items.iter() {
        let at = |e: anyhow::Error| e.placed(&si.ori, si.pos);
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
                    n.downcast_ref::<traits::Impl<R>>().filter(|i| {
                        i.fulfils.as_ref().is_some_and(|f| Arc::ptr_eq(&f.entry, &declared))
                    })
                });
                match (fulfilled.next(), fulfilled.next()) {
                    (None, _) => true,
                    // CR claude for claude: [bug/low] The surrounding `at` places this
                    // refusal at the signature's `impl T for X;`, and the duplicate is
                    // named as "(at line: L, column: C)" with no file. Read in the
                    // frame's file (the .gxi), that position points at nothing, and an
                    // editor marks the declaration rather than the second impl. Place
                    // the refusal at the duplicate's spec and name the first
                    // implementation's position. probe:
                    // design/review-2026-11-16/repro/tests-traits-a-implemented-twice-placed-at-gxi.sh
                    // (tests-traits-a-07)
                    (Some(_), Some(dup)) => bail!(
                        "impl {} for {target} is implemented twice (at {})",
                        im.trait_name,
                        dup.spec().pos
                    ),
                    (Some(i), None) => {
                        let trait_def = ctx
                            .env
                            .trait_def(trait_id)
                            .expect("bound by bind_sig");
                        check_impl_head(
                            &ctx.env,
                            &im.trait_name,
                            &target,
                            &head_sig(&declared),
                            &i.def,
                        )
                        .at(i.spec())?;
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
pub fn check_script_sig<R: Rt>(
    ctx: &CompileCtx<R>,
    sig: &Sig,
    scope: &ModPath,
    nodes: &[Node<R>],
) -> Result<()> {
    let view = interface_env(&ctx.env, scope, sig)?;
    for si in sig.items.iter() {
        let at = |e: anyhow::Error| e.placed(&si.ori, si.pos);
        let missing = || at(anyhow!("sig item {si} is missing an implementation"));
        if !matches!(si.kind, SigKind::Bind(_)) {
            check_interface_names(&view, scope, si, None).map_err(at)?
        }
        match &si.kind {
            SigKind::Bind(BindSig { name, typ: declared }) => {
                let bind = ctx
                    .env
                    .lexical
                    .binds
                    .get(scope)
                    .and_then(|m| m.get(name.name.as_str()))
                    .and_then(|id| ctx.env.by_id.get(id));
                let Some(bind) = bind else { return Err(missing()) };
                let declared = declared
                    .scope_refs(scope)
                    .rewrite_declared_args(&ctx.env)
                    .map_err(at)?;
                check_val_variable(&ctx.env, &name.name, &declared).map_err(at)?;
                if let Type::Fn(ft) = &declared {
                    ft.generalize(0)
                }
                check_interface_names(&view, scope, si, Some(&declared)).map_err(at)?;
                check_val(&ctx.env, name.name.as_str(), &declared, &bind.typ)
                    .context(ParserContext { ori: bind.ori.clone(), pos: bind.pos })?;
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
                let trait_def = ctx.env.trait_def(tid).expect("a trait's def");
                let (st, sp) = traits::impl_head(&ctx.env, scope, &trait_def, im, true)
                    .map_err(at)?;
                check_impl_head(
                    &ctx.env,
                    &im.trait_name,
                    &target,
                    &impl_sig(&st, &sp),
                    &entry,
                )
                .map_err(at)?;
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
enum Body<R: Rt> {
    /// Compiled with the program.
    Static,
    /// Loaded at run time from the text `source` produces. The program's
    /// check of the signature and each load's run in `sig_env`: a dynamic
    /// module not exported from its parent would otherwise lose its bound
    /// signature.
    Dynamic { source: Node<R>, sig_env: Lexical },
}

impl<R: Rt> Body<R> {
    fn dynamic(source: Node<R>, sig_env: Lexical) -> Self {
        Body::Dynamic { source, sig_env }
    }
}

#[derive(Debug)]
pub struct Module<R: Rt> {
    spec: Arc<Expr>,
    flags: BitFlags<CFlag>,
    body: Body<R>,
    env: Lexical,
    /// What `env` was before any body compiled: the names the module
    /// statement made. A load compiles into a copy of it, so the body it
    /// replaces keeps the names it declared until it is gone.
    base: Lexical,
    /// The signature's bindings at the module's scope, which the check
    /// pairs with the body's: the env a statement checks under may not
    /// hold them (a lambda instance's holds only the binds from before
    /// its body).
    sig_binds: Map<CompactString, BindId>,
    sig_decls: SigDecls,
    sig: Sig,
    pub(crate) scope: Scope,
    proxy: Vec<Proxy>,
    pub(crate) nodes: Box<[Node<R>]>,
    /// catch-statement indices in `nodes` (see `Block::catches`).
    pub(crate) catches: Box<[usize]>,
    /// The statements as runs for forking, as a block's are.
    pub(crate) runs: Runs,
    top_id: ExprId,
    /// The compile task a static body compiles and checks in: the cells
    /// it creates are its own, and its check writes no others.
    task: TaskId,
    resident: TagValue,
    /// A `let` among the statements re-raised at a wake in the last update
    /// that walked them, and the block around the module has not taken
    /// that back yet (`Block::reraised`).
    reraised: bool,
    /// The `let`s and modules among the statements (`Block::lets`).
    lets: Option<node::Lets>,
    /// The lets of the block around the module, which it marks when it
    /// notes a re-raise.
    pub(crate) around: Option<node::Lets>,
}

/// A module's compiled body, set aside while its replacement compiles:
/// put back if the new source does not check, retired when it does.
struct SetAside<R: Rt> {
    env: Lexical,
    nodes: Box<[Node<R>]>,
    lets: Option<node::Lets>,
    catches: Box<[usize]>,
    proxy: Vec<Proxy>,
    runs: Runs,
}

/// The load of the module's source that `env` is the view of: none where
/// it is still `base`, whose load is the one around the module.
fn own_load<'a>(base: &Lexical, env: &'a Lexical) -> Option<&'a Arc<LoadedBody>> {
    let around = base.body.as_ref();
    env.body.as_ref().filter(|b| around.is_none_or(|a| !Arc::ptr_eq(a, b)))
}

impl<R: Rt> Module<R> {
    fn with(
        spec: Arc<Expr>,
        flags: BitFlags<CFlag>,
        body: Body<R>,
        env: Lexical,
        sig_binds: Map<CompactString, BindId>,
        sig_decls: SigDecls,
        sig: Sig,
        scope: Scope,
        proxy: Vec<Proxy>,
        mut nodes: Box<[Node<R>]>,
        catches: Box<[usize]>,
        runs: Runs,
        top_id: ExprId,
        task: TaskId,
    ) -> Self {
        let base = env.clone();
        let lets = node::Lets::wire(&mut nodes);
        Self {
            spec,
            flags,
            body,
            env,
            base,
            sig_binds,
            sig_decls,
            sig,
            scope,
            proxy,
            nodes,
            catches,
            runs,
            top_id,
            task,
            resident: TagValue::phantom(),
            reraised: false,
            lets,
            around: None,
        }
    }

    pub(crate) fn reraised(&self) -> bool {
        self.reraised
    }

    /// Take back what a wake re-raised to the `let`s among the statements
    /// (`node::take_back_reraised`).
    pub(crate) fn take_back_reraised(&mut self, ctx: &mut ExecCtx<'_, R>) {
        self.reraised = false;
        let Self { lets: Some(lets), nodes, .. } = self else { return };
        lets.iter().for_each(|i| node::take_back_reraised(&mut nodes[i], ctx))
    }

    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<'_, R>,
        buf: &mut &[u8],
    ) -> Result<Node<R>, PackError> {
        let spec = Arc::new(Expr::decode(buf)?);
        let flags = image::flags_decode(buf)?;
        let body = match u8::decode(buf)? {
            0 => Body::Static,
            1 => Body::dynamic(decode_node(ctx, buf)?, image::lexical_decode(buf)?),
            _ => return Err(PackError::UnknownTag),
        };
        let env = image::lexical_decode(buf)?;
        let sig_binds = SharedMap::decode(buf)?.0;
        let sig_decls = SigDecls {
            traits: Vec::<Arc<TraitDef>>::decode(buf)?,
            impls: Vec::<Arc<ImplDef>>::decode(buf)?,
        };
        let sig = Sig::decode(buf)?;
        let scope = image::scope_decode(buf)?;
        let proxy = Vec::<Proxy>::decode(buf)?;
        let nodes = decode_nodes(ctx, buf)?.into_boxed_slice();
        let catches = Vec::<usize>::decode(buf)?.into_boxed_slice();
        let runs = Runs::image_decode(buf)?;
        let top_id = ExprId::decode(buf)?;
        // the registrations check_sig made
        for p in proxy.iter() {
            ctx.record_ref(p.inner, top_id);
            ctx.record_ref(p.outer, top_id);
            p.join(ctx);
        }
        Ok(Node::new(Self::with(
            spec,
            flags,
            body,
            env,
            sig_binds,
            sig_decls,
            sig,
            scope,
            proxy,
            nodes,
            catches,
            runs,
            top_id,
            TaskId::OUTSIDE,
        )))
    }

    pub(super) fn compile_dynamic(
        ctx: &mut CompileCtx<R>,
        flags: BitFlags<CFlag>,
        spec: Arc<Expr>,
        enclosing: &Scope,
        scope: &Scope,
        sig: Sig,
        source: Arc<Expr>,
        top_id: ExprId,
    ) -> Result<Node<R>> {
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
        ctx.env.declare_module_site(&scope.lexical, &spec.ori, spec.pos);
        let outside = ctx.env.clone();
        // CR claude for eric: [bug/medium] A trait default body written in a dynamic
        // module's sig is program text, but this only declares the sig (bind_sig,
        // traits::method_decl). The default is spliced into the body and compiled at
        // each load. So --check and the build accept `val d: fn(self) -> i64 = |s|
        // T::m(s) + "x"`, and the error appears only as a DynamicLoadError at run time;
        // a file module's .gxi refuses the same trait at the check. The default is also
        // outside Expr::for_each_child (ModuleKind::Dynamic visits only `source`), so
        // no walk over the program sees it. Either compile each sig trait default here,
        // under the view the load gives the body (Env::imported_view of the sig's
        // imports, the sig's traits declared), or refuse a default body in a dynamic
        // sig. probe:
        // design/review-2026-11-16/repro/t-expr-core-dynamic-sig-default-unchecked.gx
        // (t-expr-core-01)
        let sig_decls = bind_sig(&mut ctx.env, &mut ctx.pending_imports, &scope, &sig)
            .context("binding module signature")?;
        // the body sees what the sig imports and nothing else, so an import
        // is a grant: it must name something here, and is never unused
        if let Some(names) = ctx.env.names.get(&scope.lexical) {
            for (key, e) in names.imports.iter() {
                if !ctx.env.import_target_declared(e) {
                    return Err(anyhow!(
                        "the sig imports `{key}`, which nothing above this module \
                         declares: a dynamic module's body sees only what its sig imports"
                    )
                    .context(ParserContext { ori: e.ori.clone(), pos: e.pos }));
                }
                e.used.store(true, Ordering::Relaxed)
            }
        }
        let packages = sig
            .items
            .iter()
            .filter_map(|si| match &si.kind {
                SigKind::Use { names, .. } => Some(names.iter()),
                _ => None,
            })
            .flatten()
            .filter_map(|u| {
                let mut parts = Path::parts(&*u.path.0);
                match (parts.next(), parts.next(), &u.rename) {
                    (Some(pkg), None, None) if ctx.env.package_roots.contains(pkg) => {
                        Some(ModPath::from([pkg]))
                    }
                    _ => None,
                }
            });
        let mut env = outside.imported_view(
            &scope.lexical,
            ctx.env.names.get(&scope.lexical),
            packages,
        );
        env.lexical.modules.insert_cow(scope.lexical.clone());
        // the interface grants the traits it declares impls of, wherever
        // the imports leave them
        for si in sig.items.iter() {
            if let SigKind::Impl(im) = &si.kind
                && let Some(tid) = ctx.env.lookup_trait(&scope.lexical, &im.trait_name)?
            {
                env.grant_trait(tid)
            }
        }
        let body = Body::dynamic(source, ctx.env.lexical());
        Ok(Node::new(Self::with(
            spec,
            flags,
            body,
            env.lexical(),
            sig_binds(&ctx.env, scope),
            sig_decls,
            sig,
            scope.clone(),
            Vec::new(),
            Box::new([]),
            Box::new([]),
            Runs::default(),
            top_id,
            TaskId::OUTSIDE,
        )))
    }

    pub(super) fn compile_static(
        ctx: &mut CompileCtx<R>,
        flags: BitFlags<CFlag>,
        spec: Arc<Expr>,
        scope: &Scope,
        sig: Sig,
        exprs: Arc<[Expr]>,
        top_id: ExprId,
    ) -> Result<Node<R>> {
        let task = new_task();
        let _task = InTask::enter(task);
        let mut env = ctx.env.lexical();
        // the module's own path must be visible from inside it
        env.modules.insert_cow(scope.lexical.clone());
        let sig_decls = bind_sig(&mut ctx.env, &mut ctx.pending_imports, &scope, &sig)
            .with_context(|| {
                format_compact!("binding signature for module {}", scope.lexical)
            })?;
        let mut t = Self::with(
            spec,
            flags,
            Body::Static,
            env,
            sig_binds(&ctx.env, scope),
            sig_decls,
            sig,
            scope.clone(),
            Vec::new(),
            Box::new([]),
            Box::new([]),
            Runs::default(),
            top_id,
            task,
        );
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
    /// registered. `replaced` is the body this load takes the name of:
    /// it gives back what it registered in the global registries first,
    /// so the new source registers as the first load did, and `saved`
    /// puts those registrations back when the new source does not check.
    fn compile_source(
        &mut self,
        ctx: &mut ExecCtx<'_, R>,
        text: ArcStr,
        replaced: &mut SetAside<R>,
    ) -> Result<()> {
        let load = ctx.env.block_types.next_load();
        let ori = Arc::new(Origin::new(None, Source::Unspecified, text).loaded(load));
        // CR claude for eric: [bug/medium] A default method that a dynamic module's sig
        // writes is compiled only when a source loads: `add_interface_modules` splices
        // it into the loaded text and the body's `Trait::compile` builds it in the
        // body's imported view. So the program's check never sees it. A default with an
        // undefined name and a type error passes `--check` and the build and fails
        // every load, and a default that is right where it is written (`array::len`) is
        // `not defined` at the load unless the sig also grants `array` to the guest. A
        // static module's interface is refused at the check for the same default.
        // Either compile and check a sig's defaults once with the module statement, in
        // the env the sig is bound in, and splice the trait into a load without them,
        // or refuse a default body in a dynamic sig in `bind_sig_item`. probe:
        // design/review-2026-11-16/repro/c-module-traits-dyn-sig-default-compiled-in-guest.gx
        // (c-module-traits-01)
        let exprs = add_interface_modules(parser::parse((*ori).clone())?, &self.sig);
        // saved before the scrub, so a failure puts back the imports the
        // body that keeps running registered
        let saved = Saved::take(ctx);
        let SetAside { env, nodes, proxy, .. } = replaced;
        ctx.with_restored_mut(env, |ctx| {
            nodes.iter_mut().for_each(|n| n.unregister(ctx))
        });
        // what the replaced body binds statically, and what it writes,
        // go with it
        let mut holders = Refs::holders();
        nodes.iter().for_each(|n| n.refs(&mut holders));
        let vars = proxy.iter().filter(|p| p.private_inner).map(|p| p.inner).collect();
        let _aside =
            StaticCallees::set_aside(&ctx.static_callees, holders.take_holders(), vars);
        // `names` is a global registry: a recompile must scrub the
        // previous source's imports or they accumulate
        ctx.env.clear_names_under(&self.scope.lexical, self.env.input.as_ref());
        let env = self.env.clone();
        let interface = self.sig.items.iter().filter_map(|si| match &si.kind {
            SigKind::TypeDef(td) => Some(td.name.name.clone()),
            _ => None,
        });
        let own = ctx.env.own_types(
            &self.scope.lexical,
            &self.spec.ori,
            self.spec.pos,
            interface,
        );
        let load =
            LoadedBody::of_source(env.body.clone(), self.scope.lexical.clone(), own);
        self.env.body = Some(load.clone());
        let sig_env = match &self.body {
            Body::Static => None,
            Body::Dynamic { sig_env, .. } => Some(sig_env.clone()),
        };
        let pending = mem::take(&mut ctx.pending_imports);
        let names = mem::take(&mut ctx.pending_names);
        let census = ctx.attr_census.lock().len();
        // the body is an input of its own, after the program that loads
        // it, with a check that settles what it deferred before it
        // elaborates
        let input = crate::Input::enter();
        ctx.pending_settles.push(crate::SettleFrame::default());
        let res = self
            .compile_inner(ctx, &exprs)
            .and_then(|()| {
                crate::check_pending_imports(ctx)?;
                self.nodes
                    .iter()
                    .try_for_each(|n| crate::analysis::analyze(n, ctx).map(drop))
            })
            .map_err(|e| input.refused(ctx, e))
            .and_then(|()| input.end(ctx, &self.env, None));
        ctx.pending_settles.pop().expect("load settle frame");
        // CR claude for eric: [bug/medium] A loaded body's statements are never fused
        // and the load drops what it added to the attribute census here unchecked, so
        // `#[native]` on a statement of a loaded source asserts nothing: `let v =
        // #[native] (count(k) + 1)` loads and node-walks where the same statement in a
        // script is refused, and a pure one is not fused either. Inside a function of
        // the body the attribute is checked when an instance binds. Refuse `#[native]`
        // on a loaded body's statement at the load, or fuse loaded bodies. probe:
        // design/review-2026-11-16/repro/x-dyn-modules-native-top-level.gx
        // (x-dyn-modules-09)
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
        } else {
            load.keep_names(ctx.env.names_under(&self.scope.lexical))
        }
        res
    }

    /// Compile the body. A static body is checked with its module
    /// statement (`typecheck0`), a loaded one here.
    fn compile_inner(&mut self, ctx: &mut CompileCtx<R>, exprs: &[Expr]) -> Result<()> {
        let flags = match self.body {
            Body::Static => self.flags,
            // CR claude for eric: [risk/medium] What a loaded body is denied is names
            // (the imported view) and builtin markers (this flag); it is denied no
            // resource. A body of operators alone, importing nothing, wedges the host's
            // cycle for good with a tail loop (`let rec f = |x: i64| -> i64 f(x + 1);
            // let v = f(0)`: the host's timer never ticks again), and its non-tail twin
            // grows past 6 GB; the stack and activation budgets are unlimited by
            // default and stop the whole runtime, not the load. So a sig is a naming
            // discipline for code the host trusts not to hang it, which the book's
            // dynamic chapter does not say. Either say so there, or charge budgets to
            // the load (`Scope::load` names it at every call) so that exceeding one
            // fails the load into its status: a per-load activation count and a
            // back-edge check in kernels compiled under a load. probe:
            // design/review-2026-11-16/repro/a-packages-guest-wedges-host.gx
            // (a-packages-02)
            Body::Dynamic { .. } => self.flags | CFlag::NoBuiltins,
        };
        let dynamic = matches!(self.body, Body::Dynamic { .. });
        let scope = self.scope.in_view(&self.env);
        let compiled = ctx.with_restored_mut(&mut self.env, |ctx| {
            if dynamic {
                ctx.env.declare_module_site(
                    &self.scope.lexical,
                    &self.spec.ori,
                    self.spec.pos,
                );
            }
            crate::node::compile_block_children(
                ctx,
                flags,
                &scope,
                self.top_id,
                true,
                exprs.iter(),
                true,
            )
        });
        (self.nodes, self.catches) = compiled?;
        self.lets = node::Lets::wire(&mut self.nodes);
        self.runs = Runs::default();
        if let Body::Dynamic { .. } = &self.body {
            let check = bounds::Check::begin();
            self.check_body(ctx)
                .and_then(|()| check.end(&ctx.env))
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
    fn check_body(&mut self, ctx: &mut CompileCtx<R>) -> Result<()> {
        let _profile = profile::phase(Phase::ModuleCheck);
        let Self {
            env, nodes, catches, body, top_id, proxy, scope, sig, sig_binds, ..
        } = self;
        join_implemented_vals(ctx, sig_binds, nodes);
        ctx.with_restored_mut(env, |ctx| {
            super::typecheck0_statements(ctx, nodes, catches, false)
        })?;
        match body {
            // CR claude for claude: [bug/medium] A val implemented by a lambda whose
            // parameter is annotated with a type the module's body declares (`type V =
            // i64; let f = |x: V| x` under `val f: fn(x: i64) -> i64`) is refused,
            // `undefined type V in foo`: a static module at the check, a dynamic one at
            // its load. `check_sig` runs outside the body's env
            // (`with_restored_mut(env, ..)` covers `typecheck0_statements` alone, and a
            // dynamic body runs it in `sig_env`), so `Type::sig_matches_inner` cannot
            // resolve a name of the implementation whose cell no earlier lookup filled.
            // `let f: fn(x: V) -> i64 = |x| x` and `|x: i64| -> V x` pass only because
            // their own checks filled it. Resolve the names each implemented val's type
            // writes while the body's env is in force, before `check_sig`, or have
            // `Lambda` resolve its written parameter types where it scopes them; pin
            // both module kinds. probe:
            // design/review-2026-11-16/repro/t-fntyp-private-alias-param.sh
            // (t-fntyp-01)
            Body::Static => check_sig(ctx, *top_id, proxy, scope, sig, sig_binds, nodes),
            Body::Dynamic { sig_env, .. } => ctx.with_restored_mut(sig_env, |ctx| {
                check_sig(ctx, *top_id, proxy, scope, sig, sig_binds, nodes)
            }),
        }
    }

    /// The compile task of a static body, which its statement checks in.
    pub(crate) fn static_task(&self) -> Option<TaskId> {
        matches!(self.body, Body::Static).then_some(self.task)
    }

    /// Each signature binding outside the module, which a load
    /// publishes: a static body's are linked at its compile, a dynamic
    /// body's at each load.
    pub(crate) fn exports(&self, f: &mut dyn FnMut(BindId)) {
        let sig_env = match &self.body {
            Body::Static => return self.proxy.iter().for_each(|p| f(p.outer)),
            Body::Dynamic { sig_env, .. } => sig_env,
        };
        let binds = |scope: &Scope| sig_env.binds.get(&scope.lexical);
        for (i, si) in self.sig.items.iter().enumerate() {
            match &si.kind {
                SigKind::Bind(BindSig { name, .. }) => {
                    binds(&self.scope)
                        .and_then(|m| m.get(name.name.as_str()))
                        .map(|id| f(*id));
                }
                SigKind::Impl(_) => binds(&self.scope.append_block("impl", i as u64))
                    .into_iter()
                    .flat_map(|m| m.iter())
                    .for_each(|(_, id)| f(*id)),
                _ => (),
            }
        }
    }

    /// Map each signature `BindId` to its impl binding's `LambdaDef`, and
    /// to the builtin it names, so cross-module calls resolve statically.
    pub(crate) fn proxy_lambda_defs(&self, ctx: &mut CompileCtx<R>) {
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
            if let Some(info) = ctx.builtin_bindings.get(inner).cloned() {
                ctx.builtin_bindings.insert(*outer, info);
            }
        }
    }

    /// Refuse a write through a `val` of this static module, or of a
    /// static module in it, that its implementation would not take
    /// ([`CompileCtx::check_val_written`]), once the statements of the
    /// block it stands in are checked: a sibling module's check, which
    /// may have run beside this one's, writes it by then.
    pub(crate) fn check_written_vals(&self, ctx: &CompileCtx<R>) -> Result<()> {
        if let Body::Dynamic { .. } = self.body {
            return Ok(());
        }
        let vals = implemented_vals(&ctx.env, &self.sig_binds, &self.nodes);
        let written = || -> Result<()> {
            for (i, id, outer) in
                vals.iter().filter(|v| ctx.connect_targets.contains(&v.2))
            {
                let n = &self.nodes[*i];
                let bind = n.downcast_ref::<Bind<R>>().expect("an implemented val's let");
                if let (Some(inner), Some(val)) =
                    (ctx.env.by_id.get(id), ctx.env.by_id.get(outer))
                {
                    let typ = implementation_type(bind, inner);
                    ctx.check_val_written(&inner.name, &val.typ, typ).at(n.spec())?
                }
            }
            for n in self.nodes.iter() {
                if let crate::NodeView::Module(m) = n.view() {
                    wrap!(n, m.check_written_vals(ctx))?
                }
            }
            Ok(())
        };
        written()
            .with_context(|| format_compact!("compiling module {}", self.scope.lexical))
    }

    /// Run the children's `typecheck1` under the module's private env,
    /// its typedefs seeded there before (the check has ended) and after
    /// (for what elaboration defined), as the root's are.
    fn typecheck1_nodes(&mut self, ctx: &mut CompileCtx<R>) -> Result<()> {
        let Self { env, nodes, catches, scope, .. } = self;
        let _module = profile::module(&scope.lexical);
        ctx.with_restored_mut(env, |ctx| {
            ctx.env.seed_typedef_refs_under(&scope.lexical);
            let r = super::typecheck1_statements(ctx, nodes, catches, false);
            ctx.env.seed_typedef_refs_under(&scope.lexical);
            let seen = &mut LPooled::take();
            nodes.iter().for_each(|n| crate::seed_node_types(&ctx.env, n, seen));
            r
        })
    }

    fn clear_compiled(&mut self, ctx: &mut ExecCtx<'_, R>) {
        let set_aside = self.set_aside();
        self.retire(ctx, set_aside, false)
    }

    /// Take the compiled body out of the module and start its env again
    /// from the module statement's own: a replacement compiles into
    /// fresh names, as the first load did, while the body it replaces
    /// keeps the env it declared its names in.
    fn set_aside(&mut self) -> SetAside<R> {
        // CR claude for claude: [structure/low] A loaded body is six loose fields of
        // `Module` (`env`, `nodes`, `lets`, `catches`, `proxy`, `runs`) mirrored by
        // `SetAside` and moved one by one here and in `put_back`. A per-body field
        // added to `Module` and not to `SetAside` would stay with the wrong body across
        // a failed reload, and nothing in the types says so. Hold the body as one
        // `SetAside` field, so this is one `mem::replace` and `put_back` one
        // assignment. `task` is read only for a static body (`static_task`), so it
        // belongs in `Body::Static`. `Module::with` then takes nine arguments, not
        // fourteen. Found by reading; not run. (c-module-traits-07)
        let env = mem::replace(&mut self.env, self.base.clone());
        SetAside {
            env,
            nodes: mem::take(&mut self.nodes),
            lets: self.lets.take(),
            catches: mem::take(&mut self.catches),
            proxy: mem::take(&mut self.proxy),
            runs: mem::take(&mut self.runs),
        }
    }

    /// Put a set-aside body back where it was: the load that would
    /// replace it failed, so it runs on.
    fn put_back(&mut self, body: SetAside<R>) {
        let SetAside { env, nodes, lets, catches, proxy, runs } = body;
        if let Some(body) = own_load(&self.base, &env) {
            body.revive()
        }
        self.env = env;
        self.nodes = nodes;
        self.lets = lets;
        self.catches = catches;
        self.proxy = proxy;
        self.runs = runs;
    }

    /// Release a set-aside body, one a `reload` replaced or one deleted
    /// with its module. Its references go to the runtime, so what still
    /// holds one keeps it.
    fn retire(&self, ctx: &mut ExecCtx<'_, R>, body: SetAside<R>, reload: bool) {
        let SetAside { mut env, nodes, proxy, .. } = body;
        if let Some(load) = own_load(&self.base, &env) {
            load.retire()
        }
        for Proxy { inner, outer, .. } in proxy.iter() {
            ctx.unref_var(*inner, self.top_id);
            ctx.unref_var(*outer, self.top_id);
        }
        if !reload {
            for p in proxy.iter().filter(|p| p.private_inner) {
                ctx.static_callees.keep(p.inner, p.outer)
            }
        }
        // the body's nodes let go of the variables their bindings are
        // while those still are
        ctx.with_restored_mut(&mut env, |ctx| {
            for mut n in nodes {
                n.delete(ctx)
            }
        });
        for Proxy { inner, outer, private_inner } in proxy {
            if !private_inner {
                continue;
            }
            if reload {
                // what the body wrote through its binding and has not
                // landed goes with it, a reference to it alive or not
                ctx.rt.forget_writes(inner);
                ctx.static_callees.unjoin(inner, outer)
            } else {
                ctx.part_deleted_pair(inner, outer)
            }
        }
        ctx.sync_var_aliases()
    }

    fn sleep_nodes(&mut self, ctx: &mut ExecCtx<'_, R>) {
        ctx.with_restored_mut(&mut self.env, |ctx| {
            for n in &mut self.nodes {
                n.sleep(ctx);
            }
        });
    }
}

impl<R: Rt> Update<R> for Module<R> {
    /// A static body's statements take their definition's types, as a
    /// block's do; its signature is checked against them again.
    fn typecheck0_instance(
        &mut self,
        ctx: &mut CompileCtx<R>,
        types: &mut crate::node::lambda::InstanceTypes,
    ) -> Result<()> {
        // the check judged a dynamic module's signature and source: an
        // instance types the source from its rows
        if let Body::Dynamic { source, .. } = &mut self.body {
            return wrap!(source, source.typecheck0_instance(ctx, types));
        }
        let _task = InTask::enter(self.task);
        let Self { env, nodes, catches, top_id, proxy, scope, sig, sig_binds, .. } = self;
        join_implemented_vals(ctx, sig_binds, nodes);
        ctx.with_restored_mut(env, |ctx| {
            super::typecheck_in_order(ctx, nodes, catches, false, |n, ctx| {
                n.typecheck0_instance(ctx, types)
            })
        })
        .and_then(|()| check_sig(ctx, *top_id, proxy, scope, sig, sig_binds, nodes))
        .with_context(|| format_compact!("compiling module {}", scope.lexical))?;
        self.proxy_lambda_defs(ctx);
        Ok(())
    }

    fn for_each_child<'a>(&'a self, f: &mut dyn FnMut(&'a Node<R>)) {
        if let Body::Dynamic { source, .. } = &self.body {
            f(source)
        }
        self.nodes.iter().for_each(f)
    }

    fn for_each_child_mut(&mut self, f: &mut dyn FnMut(&mut Node<R>)) {
        if let Body::Dynamic { source, .. } = &mut self.body {
            f(source)
        }
        self.nodes.iter_mut().for_each(f)
    }

    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        // A load declares its names in `env`, which `base` no longer
        // says: an image is written before any cycle, so no body has
        // loaded and the two are one.
        if let Body::Dynamic { .. } = &self.body
            && !self.nodes.is_empty()
        {
            return Err(PackError::Application(image::NOT_QUIESCENT));
        }
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
        SharedMap(self.sig_binds.clone()).encode(buf)?;
        self.sig_decls.traits.encode(buf)?;
        self.sig_decls.impls.encode(buf)?;
        self.sig.encode(buf)?;
        image::scope_encode(&self.scope, buf)?;
        self.proxy.encode(buf)?;
        encode_nodes(&self.nodes, buf)?;
        image::slice_encode(&self.catches, buf)?;
        self.runs.image_encode(buf)?;
        self.top_id.encode(buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R>) -> &TagValue {
        let mut compiled = false;
        let mut primed: LPooled<Vec<BindId>> = LPooled::take();
        let mut src_tag = Tag::FIRED;
        let mut bottom = false;
        let mut failed = None;
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
                    let mut set_aside = self.set_aside();
                    let res = self.compile_source(ctx, s, &mut set_aside);
                    // the new body's references reach the runtime first: an
                    // unref cancels a reference still pending for the same
                    // (variable, reader), so retiring the old body first would
                    // cancel the new one's
                    ctx.apply_deferred();
                    match res {
                        Err(e) => {
                            self.put_back(set_aside);
                            failed = Some(errf!(ERR_TAG, "compile error {e:?}"))
                        }
                        Ok(()) => {
                            self.retire(ctx, set_aside, true);
                            ctx.env.sweep_load_types();
                            compiled = true
                        }
                    }
                }
                v => {
                    failed = Some(errf!(ERR_TAG, "unexpected {}", crate::typ::Naked(&v)))
                }
            }
        }
        if compiled {
            ctx.core_hook_sites.lock().reloaded();
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
        ctx.under(view, |ctx| self.runs.update(ctx, &mut self.nodes, &self.catches));
        if node::Lets::take_mark(&self.lets) {
            self.reraised = true;
            self.around.iter().for_each(node::Lets::mark)
        }
        // a prime is for the fresh body alone: left, it would fire readers
        // later in the cycle, and a forked merge would requeue it
        for id in primed.drain(..) {
            ctx.event.variables.remove(&id);
        }
        // a trait default's binding is shared by every impl it serves, so
        // each impl's method binding takes its production
        for Proxy { inner, outer, private_inner } in &self.proxy {
            if *private_inner {
                continue;
            }
            let tv = match ctx.event.variables.get(inner).cloned() {
                Some(tv) => tv,
                // it may have produced long before this load
                None if compiled => match ctx.rt.store_value(inner) {
                    Some(v) => TagValue::fired(v.clone()),
                    None => continue,
                },
                None => continue,
            };
            ctx.deliver(*outer, tv, Notify::Roots);
        }
        if let Some(e) = failed {
            self.resident.set(TagValue::tagged(e, src_tag))
        } else if bottom {
            self.resident.set(TagValue::tagged(Value::Null, Tag::FRESH_BOTTOM))
        } else if compiled {
            self.resident.set(TagValue::tagged(Value::Null, src_tag))
        } else {
            self.resident.ride()
        }
    }

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R>) {
        if let Body::Dynamic { source, .. } = &mut self.body {
            source.delete(ctx);
        }
        for def in self.sig_decls.traits.iter() {
            let body = self.env.binds.get(&def.path);
            let names = def.methods.iter().map(|m| m.name.as_str());
            let splice = names.chain([TraitDef::ANCHOR]).filter_map(|n| body?.get(n));
            ctx.keep_trait(def, splice.copied())
        }
        self.clear_compiled(ctx);
        ctx.env.sweep_load_types();
        self.sig_decls.unregister(&mut ctx.env);
        let SigDecls { traits, impls } = &self.sig_decls;
        let methods = impls.iter().flat_map(|im| im.methods.iter());
        for (_, id) in self.sig_binds.iter().chain(methods) {
            ctx.forget_binding(*id);
            ctx.delete_binding(*id)
        }
        for id in traits.iter().flat_map(|def| def.dispatchers()) {
            ctx.delete_dispatcher(id)
        }
        ctx.env.unbind_lexical_under(&self.scope.lexical);
    }

    fn unregister(&mut self, ctx: &mut ExecCtx<'_, R>) {
        self.for_each_child_mut(&mut |c| c.unregister(ctx));
        if let Some(body) = &ctx.env.lexical.body {
            let SigDecls { traits, impls } = &self.sig_decls;
            let traits = traits.iter().map(|def| ctx.env.trait_of_record(def));
            body.give(traits, impls.iter().cloned())
        }
        self.sig_decls.unregister(&mut ctx.env)
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R>) {
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

    fn typecheck0(&mut self, ctx: &mut CompileCtx<R>) -> Result<()> {
        match &mut self.body {
            Body::Dynamic { source, sig_env } => {
                ctx.with_restored_mut(sig_env, |ctx| {
                    check_sig_decls(
                        &ctx.env,
                        &self.scope.lexical,
                        &self.sig,
                        &self.sig_binds,
                        &self.nodes,
                    )
                })
                .at(&self.spec)?;
                wrap!(source, source.typecheck0(ctx))?;
                let t = Type::Primitive(Typ::String | Typ::Error);
                wrap!(source, t.check_contains(&ctx.env, source.typ()))?;
            }
            Body::Static => {
                let _task = InTask::enter(self.task);
                let _module = profile::module(&self.scope.lexical);
                let writes = OwnWrites::enter(self.task);
                let res = match (self.check_body(ctx), writes.foreign()) {
                    (res, None) => res,
                    (res, Some(w)) => {
                        let shown = res.as_ref().err().and_then(shown_in);
                        let shown = shown.unwrap_or(&self.spec.ori);
                        let m = format_compact!(
                            "the check decides a type the code around the module \
                             left open: {}",
                            decided_binds(ctx, &self.scope.lexical, shown, &w)
                        );
                        match res {
                            Ok(()) => Err(anyhow!("{m}")),
                            Err(e) => Err(Told::over(e, &m)),
                        }
                    }
                };
                res.with_context(|| {
                    format_compact!("compiling module {}", self.scope.lexical)
                })?;
            }
        }
        self.proxy_lambda_defs(ctx);
        Ok(())
    }

    fn typecheck1(&mut self, ctx: &mut CompileCtx<R>) -> Result<()> {
        if let Body::Dynamic { source, .. } = &mut self.body {
            wrap!(source, source.typecheck1(ctx))?;
        }
        self.typecheck1_nodes(ctx)
    }

    fn view(&self) -> crate::NodeView<'_, R> {
        crate::NodeView::Module(self)
    }

    fn fuse(&mut self, ctx: &mut CompileCtx<R>) -> Result<Option<Node<R>>> {
        // A dynamic module's body compiles at run time (`compile_source`),
        // after fusion, and is never fused; its loader is not fused here.
        let _module = profile::module(&self.scope.lexical);
        for child in self.nodes.iter_mut() {
            crate::fusion::fuse(child, ctx)?;
        }
        Ok(None)
    }
}

/// The file `e` is reported in: its site's, or the one a parse or an
/// interface item placed it in.
fn shown_in(e: &anyhow::Error) -> Option<&Arc<Origin>> {
    match e.downcast_ref::<ErrorSite>() {
        Some(site) => Some(&site.expr().ori),
        None => e.downcast_ref::<ParserContext>().map(|p| &p.ori),
    }
}

/// The bindings around `module` whose types hold what its check wrote,
/// by position, for a refusal reported in `at`: each named with its file
/// where that is not `at`'s. A binding a lowering made (a definition's
/// own throws) is never named.
fn decided_binds<R: Rt>(
    ctx: &CompileCtx<R>,
    module: &ModPath,
    at: &Origin,
    w: &ForeignWrite,
) -> CompactString {
    let mut lowered = false;
    let mut held: LPooled<Vec<(bool, crate::SourcePosition, &str, &Source)>> = ctx
        .env
        .by_id
        .iter()
        .map(|(_, b)| b)
        .filter(|b| {
            b.scope != *module
                && Path::is_parent(&*b.scope.0, &*module.0)
                && w.held_by(&b.typ)
        })
        .filter(|b| {
            let named = !crate::is_lowered(&b.name);
            lowered |= !named;
            named
        })
        .map(|b| {
            let src = &b.ori.source;
            (*src != at.source, b.pos, b.name.as_str(), src)
        })
        .collect();
    held.sort_by(|a, b| (a.0, a.1, a.2).cmp(&(b.0, b.1, b.2)));
    held.dedup();
    let mut out = CompactString::new("");
    for (i, (elsewhere, pos, name, src)) in held.iter().take(3).enumerate() {
        let sep = if i == 0 { "annotate " } else { ", " };
        let _ = write!(out, "{sep}`{name}` (line {}, column {}", pos.line, pos.column);
        let _ = if *elsewhere { write!(out, " in {src})") } else { write!(out, ")") };
    }
    match (out.is_empty(), lowered) {
        (false, _) => (),
        (true, true) => out.push_str(
            "an error the module raises reaches the function around it, whose \
             throws only its own check may decide: catch it inside the module",
        ),
        (true, false) => out.push_str("annotate the binding it decides"),
    }
    out
}
