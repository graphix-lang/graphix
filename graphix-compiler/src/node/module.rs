use crate::{
    BindId, CFlag, CompileCtx, ExecCtx, Node, Notify, PendingImport, PrintFlag, Refs, Rt,
    Saved, Scope, StaticCallees, Tag, TagValue, Update, View,
    compiler::compile,
    env::{Env, Glob, ImplDef, ImportEntry, Lexical, Map, UseAnchor, scope_params},
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
    node::{self, Nop, Runs, bind::Bind, traits},
    profile::{self, Phase},
    shared_map::SharedMap,
    typ::{
        Brief, FnArgKind, FnArgType, FnType, TVar, Type,
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
    crate::recheck_pending_contractive(ctx)?;
    Ok(Nop::new(Type::Bottom))
}

fn bind_sig(
    env: &mut Env,
    pending: &mut Vec<PendingImport>,
    scope: &Scope,
    sig: &Sig,
) -> Result<()> {
    env.lexical.modules.insert_cow(scope.lexical.clone());
    // a sig `use self::sub::…` may precede `mod sub;`
    for si in sig.items.iter() {
        if let SigKind::Module(name) = &si.kind {
            env.lexical.modules.insert_cow(scope.append(name).lexical);
        }
    }
    for (i, si) in sig.items.iter().enumerate() {
        bind_sig_item(env, pending, scope, i, si, &si.ori).placed(&si.ori, si.pos)?;
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
            let typ = typ.scope_refs(&scope.lexical).rewrite_trait_args(env)?;
            // a declared signature has no gate to close over its cells:
            // every one is a quantifier, a constructor trait's element
            // included, copied at each use, and its own are its scheme's,
            // which each use picks, not a rank-2 type's
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
            // CR claude for eric: [bug/critical] A val's type variable outside a function
            // type is never claimed at the top level, so a definition outside the module
            // copies it: with `val v: Array<'a>` over `let v = never()`, the main program's
            // `let wr = |x| { m::v <- x; x }; let rd = |u| u ~ m::v` write v at Array<i64>
            // and read it at Array<string>; the check passes and the run aborts reading an
            // i64 as an ArcStr ("misaligned pointer dereference" in arcstr; undefined
            // behaviour in a release build) (b2). The same through a sibling module's
            // generic vals (e1), through a variable standing both in a function type and in
            // data (`type Codec<'a> = {show: fn(x: 'a) -> string, last: ['a, null]}; val c:
            // Codec<'a>`, hw17, hw12), and in the REPL, where a dynamic module's load
            // unbinds a variable an earlier input decided (repl2). Only a val typed `fn(..)`
            // goes in poly_binds, so a write through any other generic val is held to
            // nothing. Branch cr44-E claims such a variable at the top level (b4682bb47,
            // closing b2 and e1); its review found hw17, a load's `unbind_tvars` reopening
            // the claimed variable (dl2), an alias defined after the module claimed whole
            // (fw3), and a sibling's one-type use refused with no binding named (e2). The
            // rule is Eric's (next-discuss): refuse a val whose variable stands anywhere
            // outside a function type (no .gxi in either repo has one), or claim it as one
            // type. probes: ~/ptmp/cr29/scratch/review-r7/E5-r3-1/{b2,e1},
            // E5-r4-2/{hw17,hw12,e2,fw3}, E5-r4-1/{repl2,dl2} (iface-val-data-var-01)
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
/// the implementation's own, or a trait default. A private inner
/// binding's production moves out and a write to the outer id flows in.
#[derive(Debug, Clone, Copy, netidx_derive::Pack)]
#[pack(unwrapped)]
struct Proxy {
    inner: BindId,
    outer: BindId,
    private_inner: bool,
}

impl Proxy {
    /// A private inner binding is one variable with the outer one: a
    /// static call to either holds both, and a write to either is one to
    /// both.
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

// CR claude for eric: [bug/medium] A generic `val` passes over an implementation
// with one type: `val f: fn(x: 'a) -> 'a` over `let f = |x| x; f <- |x| x` (a written
// `let`, so monomorphic) and `val set: fn(g: fn(x: 'a) -> 'a) -> null` over `let set =
// |g| { f <- g; null }` (g shares f's cell) both match, so a write that narrows f is
// accepted and a use of `a::f` at another type fails at run time: `a::set(go ~ |x: i64|
// -> i64 x + 100)` then `a::f(t ~ "s")` (u4), or a nested dynamic module's `super::f <-
// |x: i64| -> i64 x + 100` (u2), gives "a run-time bind at a::f(t ~ "s") did not type:
// an instance at fn(x: string) -> string of a definition typed fn(x: i64) -> i64".
// Main refused both only because a static call through a::f held f
// (iface-join-mark-01); with that fixed both reach run time. A val declares exactly its
// implementation's type, so check_val should refuse a declared quantifier the
// implementation holds as a cell it does not own (p1, `f <- |x| x` under a generic
// val, which main runs, would be refused: declare it at one type). probe:
// ~/ptmp/cr29/scratch/r6-W/w11/probes.sh (u2, u4) (iface-generic-written-01)
// 2026-11-01 claude: Review of cr37-W: 'Main refused both' holds for base 432e34ac6;
// main a084f9887 accepted u2 and ran u4 stale. The hole predates this lane without any
// static call: u4i (a::f called through an array, let fs = [a::f]; h = fs[0]$; h(t ~
// "s")) fails at run time with the same 'did not type' message on main, base and lane.
// probe: ~/ptmp/cr29/scratch/r6-W/r38/u4i
// 2026-11-02 claude: Lane I (branch cr44-E, wave 5) worked this through three fix
// rounds and is parked: each review found a new route (round 4: dl2, a dynamic load
// unbinding a val variable item 2 decided, reads a string as an i64; hw17, main's own,
// CR iface-val-data-var-01). Its commits are the starting point once Eric rules on the
// val variable rule (next-discuss item 13). Final reviews:
// ~/ptmp/cr29/scratch/review-r7/E5-r4-{1,2}/.
// 2026-11-02 claude: Waits on Eric's ruling of the next-discuss item "Interface val
// type variables" (~/ptmp/cr29/next-discuss.md). Readdressed to eric until then.
// CR claude for eric: [bug/low] An implementation binding's open cell is not decided by
// the val it implements: with `type B<'x>; val e: B<i64>; val i: B<i64>;` in m.gxi and
// `type B<'x> = Abstract<i64>; let e = B(1); let i: B<i64> = B(1)` in m.gx, e's parameter
// stays open past the check, its value is built at B<⊥>, and `m::e == m::i` is false
// where both have the static type B<i64>; a map keyed by them takes two entries and
// `array::dedup` keeps two. The same through a val written with an aliased or inline
// recursive argument (modt: `m::c == m::e` false where `m::c == m::a`, `a` annotated, is
// true). sig_matches compares the implementation's type without binding its cells, where
// an annotation on the let decides them; a hidden abstract type with parameters is
// declared exactly so. Main a084f9887 and lane E (cr40-E) alike. probes:
// ~/ptmp/cr29/scratch/review-r6/E4-r1-1/modv/main.gx (prints 'static e==i false'),
// modu/main.gx, modt/main.gx (iface-open-param-01)
// 2026-11-02 claude: Lane I (branch cr44-E, wave 5) worked this through three fix rounds
// and is parked: each review found a new route (round 4: dl2, a dynamic load unbinding a
// val variable item 2 decided, reads a string as an i64; hw17, main's own, CR
// iface-val-data-var-01). Its commits are the starting point once Eric rules on the val
// variable rule (next-discuss item 13). Final reviews:
// ~/ptmp/cr29/scratch/review-r7/E5-r4-{1,2}/.
// 2026-11-02 claude: Waits on Eric's ruling of the next-discuss item "Interface val type
// variables" (~/ptmp/cr29/next-discuss.md). Readdressed to eric until then.
// CR claude for eric: [bug/medium] A write through a val narrower than its implementation
// reaches the implementation at the val's type alone: with `val f: fn(x: i64) -> i64; val
// g: fn(t: Any) -> string` over `let f = |x| x; let g = |t| f(t ~ "s")`, the main
// program's `m::f <- t ~ |x: i64| -> i64 x + 100` passes --check, held to the val's type,
// lands on the body's f, and g's call fails at run time ("a run-time bind at f(t ~ "s")
// did not type: an instance at fn(x: string) -> string of a definition typed fn(x: i64)
// -> i64", ca1); `val f: fn<'a: Number + Singleton>(x: 'a) -> 'a` over `|x| x` written
// `|x| x * x` alike (rb1); a dynamic module's `super::m::f <- ..` over a body calling f
// through `let fs = [f]` alike (dyn2b). The run-time bind catches it: no value of the
// wrong type flows. `&mut m::f` is refused at the check (poly_binds), which is what keeps
// mut-ref-poly-01 open: branch cr44-E's 535c0c322 let `&mut` of a val at one type through
// and so reached this route (ca2, rb2), and was held back. Choices: hold a write through a
// val to its implementation's type too, rigid where a use copies it (as
// Connect::typecheck0_with holds a write to an earlier-generalized binding); or refuse, at
// the val's check, a val whose implementation has a cell a use copies where the val has a
// fixed type. Either must leave rf2 (`val f: fn(x: i64) -> i64` over `|x: i64| -> i64 x +
// 1`) writable: its open throws cell is one a use copies, and ⊥ fits every instance of it.
// probes: ~/ptmp/cr29/scratch/review-r7/E5-r1-1/{ca1,rb1,ca2,rb2},
// ~/ptmp/cr29/scratch/r7-E/rv/dyn2b (iface-narrow-write-01)
/// Refuse an implementation of `val name` whose type `typ` is not the
/// declared `declared`.
fn check_val(env: &Env, name: &str, declared: &Type, typ: &Type) -> Result<()> {
    // CR claude for eric: [bug/medium] An interface val's written quantified fn type
    // lets its quantifier escape through the implementation, as a let annotation did
    // before rank2-escape-01: `val h: fn<'a: Any>(x: 'a) -> 'a; val y: i64` over `let y
    // = never(); let h = |x| { y <- x; x }` (if1) and `val mk: fn(z: i64) -> fn<'a:
    // Any>(x: 'a) -> 'a` over `let mk = |z| |x| { y <- x; x }` (if3) pass --check, and
    // the build refuses the instance of m::h(1) ("an instance at fn(x: i64) -> i64 of a
    // definition typed fn(x: _) -> _"), of m::mk(0) alike; main a084f9887 the same.
    // sig_matches pairs the declared quantifier with the implementation's cell, which
    // is y's, a cell the implementation does not own; it binds nothing, so holding the
    // quantifier rigid and asking carried_out of the implementation's type after the
    // match finds nothing. The rule is main's iface-generic-written-01's: a declared
    // quantifier, a rank-2 one included, paired with a cell the implementation does
    // not own is refused. probes: ~/ptmp/cr29/scratch/r6-B/r7/if1/, if3/ (controls
    // if4/ runs) (iface-rank2-escape-01)
    // 2026-11-02 claude: Main has CR iface-generic-written-01 (node/module.rs, at
    // Module::typecheck0's val check) for the generic-val class; both are a val's
    // quantifier the implementation ties to an outer binding, so fix them together.
    // 2026-11-02 claude: Lane I (branch cr44-E, wave 5) worked this through three fix
    // rounds and is parked: each review found a new route (round 4: dl2, a dynamic load
    // unbinding a val variable item 2 decided, reads a string as an i64; hw17, main's
    // own, CR iface-val-data-var-01). Its commits are the starting point once Eric
    // rules on the val variable rule (next-discuss item 13). Final reviews:
    // ~/ptmp/cr29/scratch/review-r7/E5-r4-{1,2}/.
    // 2026-11-02 claude: Waits on Eric's ruling of the next-discuss item "Interface val
    // type variables" (~/ptmp/cr29/next-discuss.md). Readdressed to eric until then.
    declared.sig_matches(env, typ).map_err(|e| {
        let typ = format_with_flags(PrintFlag::DerefTVars, || {
            format_compact!("{}", Brief(typ))
        });
        anyhow!("val {name} is declared {declared} but implemented {typ}: {e:#}")
    })
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
    declared.sig_matches(env, &head_sig(implemented)).map_err(|e| {
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

/// [`CompileCtx::carry_static_mark`] over each implemented `val`, before
/// the body's check resolves anything through either binding: a `<-` is
/// marked as it compiles, which may be before [`check_sig`] joins them.
fn carry_static_marks<R: Rt>(
    ctx: &mut CompileCtx<R>,
    sig_binds: &Map<CompactString, BindId>,
    nodes: &[Node<R>],
) {
    for (_, inner, outer) in implemented_vals(&ctx.env, sig_binds, nodes).drain(..) {
        ctx.carry_static_mark(inner, outer)
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

fn check_sig_items<R: Rt>(
    ctx: &mut CompileCtx<R>,
    proxy: &mut Vec<Proxy>,
    scope: &Scope,
    sig: &Sig,
    sig_binds: &Map<CompactString, BindId>,
    nodes: &[Node<R>],
) -> Result<()> {
    // the interface's types and traits first: a val's type names them
    let view = interface_env(&ctx.env, &scope.lexical, sig)?;
    for si in sig.items.iter() {
        let val = match &si.kind {
            SigKind::Bind(BindSig { name, .. }) => sig_binds
                .get(name.name.as_str())
                .and_then(|id| ctx.env.by_id.get(id))
                .map(|b| &b.typ),
            _ => None,
        };
        check_redefinition(&scope.lexical, si, nodes)
            .and_then(|()| check_interface_names(&view, &scope.lexical, si, val))
            .placed(&si.ori, si.pos)?;
    }
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
            // a single name's type is the whole pattern's
            let typ = match bind.pattern.single_bind_id() {
                Some(_) => bind.typ(),
                None => &inner.typ,
            };
            proxy_bind.typ.unbind_tvars();
            check_val(&ctx.env, &inner.name, &proxy_bind.typ, typ)?;
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
                    .rewrite_trait_args(&ctx.env)
                    .map_err(at)?;
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
                let trait_def = ctx.env.trait_def(tid).cloned().expect("a trait's def");
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
    /// Loaded at run time from the text `source` produces. The load's
    /// signature check runs in `sig_env`: a dynamic module not exported
    /// from its parent would otherwise lose its bound signature.
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

// CR claude for eric: [structure/medium] An interface pair is two runtime variables (the
// val's binding and the body's) kept in step by relays: the static-call mark carry, the seq
// planner's joined pairs (StaticCallees::add_joined), this cycle-start delivery, the module
// node's productions, a wake's re-raise. Each review round of iface-join-mark-01 and
// dyn-reload-reader-01 found another place the relay is not one variable. Still open here,
// main alike: two place writes from opposite sides in one cycle lose one (pp: {x: 0, y: 9}
// where one variable gives {x: 7, y: 9}; n7), the (Some, Some) arm reverses one variable's
// set order (n2: 10 where 20), and a write through the val while the module's arm sleeps
// fires readers outside the arm twice at the wake (s1: 3 where 2). A third round
// (branch cr37-W, 2681714fa: both sides' writes merged into the val's queue) fixed those
// and regressed two shapes main gets right: a replaced body's queued writes land on its
// replacement, so one function answers two ways (d6: d::r 3, d::f(1) 21), and a level
// write is ordered by the seq of the one it replaced (lv ends at 100 where 0). Proposed:
// make the pair one variable at run time (one queue, the val's id, from the join), and
// rule what a reload does with the replaced body's pending writes (main drops them with
// the old binding; a root write queued across the reload lands, d4). probes:
// ~/ptmp/cr29/scratch/review-r6/W2-r2-1/{pp,ppn,ppctl}, W2-r3-2/cr37/p/{n7,n2,s1,s1ctl},
// W2-r3-1/cr37/{d6,d2,d1,d4}, W2-r3-2/cr37/findings.sh (lv) (iface-pair-one-var-01)
/// Deliver each write the cycle's start brought to one binding of an
/// interface pair to the other too, before any node runs, so a reader of
/// either sees it this cycle wherever it runs. Writes to both in one cycle
/// are two writes to one variable: the body's lands on both now, the
/// other's next cycle.
pub(crate) fn deliver_pair_writes<R: Rt>(ctx: &mut ExecCtx<'_, R>) {
    let mut pairs: LPooled<Vec<(BindId, BindId)>> = LPooled::take();
    ctx.static_callees.pairs_of(ctx.event.variables.own_ids(), &mut pairs);
    for (inner, outer) in pairs.drain(..) {
        let written = |id| ctx.event.variables.get(&id).cloned();
        match (written(inner), written(outer)) {
            (Some(tv), None) => ctx.deliver(outer, tv, Notify::Roots),
            (None, Some(tv)) => ctx.deliver(inner, tv, Notify::Roots),
            (Some(tv), Some(next)) => {
                ctx.rt.set_var(inner, next.value());
                ctx.deliver(outer, tv, Notify::Roots)
            }
            (None, None) => (),
        }
    }
}

impl<R: Rt> Module<R> {
    fn with(
        spec: Arc<Expr>,
        flags: BitFlags<CFlag>,
        body: Body<R>,
        env: Lexical,
        sig_binds: Map<CompactString, BindId>,
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
        sandbox: Sandbox,
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
        let mut env = ctx.env.apply_sandbox(&sandbox).context("applying sandbox")?;
        env.declare_module_site(&scope.lexical, &spec.ori, spec.pos);
        env.lexical.modules.insert_cow(scope.lexical.clone());
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
        let body = Body::dynamic(source, ctx.env.lexical());
        Ok(Node::new(Self::with(
            spec,
            flags,
            body,
            env.lexical(),
            sig_binds(&ctx.env, scope),
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
        bind_sig(&mut ctx.env, &mut ctx.pending_imports, &scope, &sig).with_context(
            || format_compact!("binding signature for module {}", scope.lexical),
        )?;
        let mut t = Self::with(
            spec,
            flags,
            Body::Static,
            env,
            sig_binds(&ctx.env, scope),
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
        let ori = Arc::new(Origin { parent: None, source: Source::Unspecified, text });
        let exprs = add_interface_modules(parser::parse((*ori).clone())?, &self.sig);
        // saved before the scrub, so a failure puts back the imports the
        // body that keeps running registered
        let saved = Saved::take(ctx);
        let SetAside { env, nodes, .. } = replaced;
        ctx.with_restored_mut(env, |ctx| {
            nodes.iter_mut().for_each(|n| n.unregister(ctx))
        });
        // what the replaced body binds statically goes with it
        let mut holders = Refs::holders();
        nodes.iter().for_each(|n| n.refs(&mut holders));
        let _aside =
            StaticCallees::set_aside(&ctx.static_callees, holders.take_holders());
        // `names` is a global registry: a recompile must scrub the
        // previous source's imports or they accumulate
        ctx.env.clear_names_under(&self.scope.lexical);
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
    fn compile_inner(&mut self, ctx: &mut CompileCtx<R>, exprs: &[Expr]) -> Result<()> {
        let flags = match self.body {
            Body::Static => self.flags,
            Body::Dynamic { .. } => self.flags | CFlag::NoBuiltins,
        };
        let dynamic = matches!(self.body, Body::Dynamic { .. });
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
                &self.scope,
                self.top_id,
                true,
                exprs.iter(),
            )
        });
        (self.nodes, self.catches) = compiled?;
        self.lets = node::Lets::wire(&mut self.nodes);
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
    fn check_body(&mut self, ctx: &mut CompileCtx<R>) -> Result<()> {
        let _profile = profile::phase(Phase::ModuleCheck);
        let Self {
            env, nodes, catches, body, top_id, proxy, scope, sig, sig_binds, ..
        } = self;
        carry_static_marks(ctx, sig_binds, nodes);
        ctx.with_restored_mut(env, |ctx| {
            super::typecheck0_statements(ctx, nodes, catches, false)
        })?;
        match body {
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

    /// Map each signature `BindId` to its impl binding's `LambdaDef` so
    /// cross-module calls resolve statically.
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
        }
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
            r
        })
    }

    fn clear_compiled(&mut self, ctx: &mut ExecCtx<'_, R>) {
        let set_aside = self.set_aside();
        self.retire(ctx, set_aside)
    }

    /// Take the compiled body out of the module and start its env again
    /// from the module statement's own: a replacement compiles into
    /// fresh names, as the first load did, while the body it replaces
    /// keeps the env it declared its names in.
    fn set_aside(&mut self) -> SetAside<R> {
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
        self.env = env;
        self.nodes = nodes;
        self.lets = lets;
        self.catches = catches;
        self.proxy = proxy;
        self.runs = runs;
    }

    /// Release a set-aside body. Its references go to the runtime, so
    /// what still holds one keeps it.
    fn retire(&self, ctx: &mut ExecCtx<'_, R>, body: SetAside<R>) {
        let SetAside { mut env, nodes, proxy, .. } = body;
        for Proxy { inner, outer, private_inner } in proxy {
            ctx.unref_var(inner, self.top_id);
            ctx.unref_var(outer, self.top_id);
            if private_inner {
                ctx.static_callees.unjoin(inner, outer)
            }
        }
        ctx.with_restored_mut(&mut env, |ctx| {
            for mut n in nodes {
                n.delete(ctx)
            }
        });
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
        if let Body::Dynamic { .. } = self.body {
            return self.typecheck0(ctx);
        }
        let _task = InTask::enter(self.task);
        let Self { env, nodes, catches, top_id, proxy, scope, sig, sig_binds, .. } = self;
        carry_static_marks(ctx, sig_binds, nodes);
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
                            self.retire(ctx, set_aside);
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
        ctx.under(view, |ctx| {
            // a write the cycle's start did not pair: one to a body this
            // cycle's load joined
            for Proxy { inner, outer, private_inner } in &self.proxy {
                if *private_inner
                    && !ctx.event.variables.contains_key(inner)
                    && let Some(tv) = ctx.event.variables.get(outer)
                {
                    ctx.deliver(*inner, tv.clone(), Notify::Roots);
                }
            }
            self.runs.update(ctx, &mut self.nodes, &self.catches);
        });
        if node::Lets::take_mark(&self.lets) {
            self.reraised = true;
            self.around.iter().for_each(node::Lets::mark)
        }
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
        self.clear_compiled(ctx);
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
            Body::Dynamic { source, .. } => {
                wrap!(source, source.typecheck0(ctx))?;
                let t = Type::Primitive(Typ::String | Typ::Error);
                wrap!(source, t.check_contains(&ctx.env, source.typ()))?;
            }
            Body::Static => {
                let _task = InTask::enter(self.task);
                let _module = profile::module(&self.scope.lexical);
                let writes = OwnWrites::enter(self.task);
                let res = self.check_body(ctx).and_then(|()| match writes.foreign() {
                    Some(w) => bail!(
                        "the check decides a type the code around the module \
                         left open: {}",
                        decided_binds(ctx, &self.scope.lexical, &self.spec.ori, &w)
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

/// The bindings around `module` whose types hold what its check wrote,
/// by position, for the refusal of its statement in `at`: each named
/// with its file where that is not `at`'s. A binding a lowering made (a
/// definition's own throws) is never named.
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
