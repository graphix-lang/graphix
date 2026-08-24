//! `trait` declarations and `impl` blocks (`design/traits.md`).
//!
//! A trait declaration registers the trait and binds one DISPATCHER
//! per method under the trait's own module-like scope (`Read::read`);
//! a call through a dispatcher resolves to an implementation by its
//! `self` argument's type at typecheck1 (`CallSite::resolve_trait_call`).
//! Default method bodies compile as ordinary typed bindings in a
//! block below the declaring module, with the trait's dispatchers
//! glob-visible so a default can call its siblings bare.
//!
//! An `impl` compiles its methods as a block of bindings (the trait's
//! dispatchers glob-visible there too), each annotated with the
//! trait's signature instantiated at the target, and registers the
//! implementation globally.

use super::Block;
use crate::env::Map;
use crate::{
    CFlag, Event, ExecCtx, Node, NodeView, Refs, Rt, Scope, TagValue, Update, UserEvent,
    env::{Env, ImplDef, TraitDef},
    expr::{
        BindExpr, Expr, ExprId, ExprKind, ImplExpr, ModPath, StructurePattern, TraitExpr,
    },
    typ::{FnType, TVar, Type, TypeRef},
    wrap,
};
use anyhow::{Context, Result, bail};
use arcstr::ArcStr;
use compact_str::{CompactString, format_compact};
use enumflags2::BitFlags;
use poolshark::local::LPooled;
use smallvec::SmallVec;
use triomphe::Arc;

/// The declared signature of a trait method, scoped to the declaring
/// module, with the receiver `self` constrained by the trait and
/// declared as a quantifier (so it is rigid while a default body is
/// checked — a default must be well-typed for every implementor).
pub(crate) fn method_sig(parsed: &FnType, tref: &Type, scope: &ModPath) -> FnType {
    let ft = parsed.scope_refs(scope);
    let mut known: LPooled<ahash::AHashMap<ArcStr, TVar>> = LPooled::take();
    ft.alias_tvars(&mut known);
    if let Some(tv) = known.get("self") {
        tv.add_cell_constraint(tref.clone());
    }
    let mut quantifiers: LPooled<Vec<ArcStr>> = ft.quantifiers.iter().cloned().collect();
    if !quantifiers.iter().any(|q| &**q == "self") {
        quantifiers.push(arcstr::literal!("self"));
    }
    FnType { quantifiers: Arc::from_iter(quantifiers.drain(..)), ..ft }
}

/// The method signature instantiated at an implementation target:
/// `self := target`, everything else fresh.
pub(crate) fn method_sig_at(sig: &FnType, target: &Type) -> FnType {
    let mut known: LPooled<ahash::AHashMap<ArcStr, Type>> = LPooled::take();
    known.insert(arcstr::literal!("self"), target.clone());
    sig.replace_tvars(&known)
}

/// Push a declared signature into a method body: a lambda whose
/// parameters or return carry no annotation takes the signature's
/// (positional by position, labeled by name), so its body is checked
/// against the receiver's real type — `|c| c.0` in `impl Show for
/// Counter` sees `c: Counter`. Written annotations are kept (and
/// checked against the signature by the binding's own annotation).
fn annotate_lambda(value: &Expr, sig: &FnType) -> Expr {
    use crate::{expr::LambdaExpr, typ::FnArgKind};
    let ExprKind::Lambda(l) = &value.kind else { return value.clone() };
    let positional: LPooled<Vec<&Type>> = sig
        .args
        .iter()
        .filter(|a| matches!(a.kind, FnArgKind::Positional { .. }))
        .map(|a| &a.typ)
        .collect();
    let mut pos = 0usize;
    let args = Arc::from_iter(l.args.iter().map(|a| {
        let declared = match &a.labeled {
            None => {
                let t = positional.get(pos).copied();
                pos += 1;
                t
            }
            Some(_) => a.pattern.single_bind().and_then(|n| {
                sig.args
                    .iter()
                    .find(|d| matches!(&d.kind, FnArgKind::Labeled { name, .. } if name == n))
                    .map(|d| &d.typ)
            }),
        };
        let constraint = match (&a.constraint, declared) {
            (None, Some(t)) => Some(t.clone()),
            (c, _) => c.clone(),
        };
        crate::expr::Arg { constraint, ..a.clone() }
    }));
    let rtype = l.rtype.clone().or_else(|| Some(sig.rtype.clone()));
    let throws =
        l.throws.clone().or_else(|| sig.explicit_throws.then(|| sig.throws.clone()));
    let kind = ExprKind::Lambda(Arc::new(LambdaExpr {
        args,
        vargs: l.vargs.clone(),
        rtype,
        throws,
        constraints: l.constraints.clone(),
        body: l.body.clone(),
    }));
    Expr {
        id: value.id,
        ori: value.ori.clone(),
        pos: value.pos,
        kind,
        dec: value.dec.clone(),
    }
}

pub(crate) fn trait_ref(
    scope: &ModPath,
    name: &ArcStr,
    pos: crate::SourcePosition,
    ori: &Arc<crate::expr::Origin>,
) -> Type {
    Type::Ref(TypeRef::new(
        scope.clone(),
        ModPath::from([name.clone()]),
        Arc::from_iter([]),
        Some(pos),
        Some(ori.clone()),
    ))
}

#[derive(Debug)]
pub struct Trait<R: Rt, E: UserEvent> {
    spec: Expr,
    def: Arc<TraitDef>,
    defaults: Node<R, E>,
}

impl<R: Rt, E: UserEvent> Trait<R, E> {
    pub(crate) fn compile(
        ctx: &mut ExecCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: Expr,
        scope: &Scope,
        t: &TraitExpr,
        top_id: ExprId,
    ) -> Result<Node<R, E>> {
        let tref = trait_ref(&scope.lexical, &t.name, spec.pos, &spec.ori);
        let mut sigs: LPooled<Vec<(ArcStr, Arc<FnType>, usize, bool)>> = LPooled::take();
        for m in t.methods.iter() {
            let ft = method_sig(&m.typ, &tref, &scope.lexical);
            sigs.push((m.name.clone(), Arc::new(ft), m.self_index, m.default.is_some()));
        }
        let def = ctx
            .env
            .deftrait(
                &scope.lexical,
                &t.name,
                sigs.drain(..),
                None,
                spec.pos,
                spec.ori.clone(),
            )
            .with_context(|| format!("in trait declaration at {}", spec.pos))?;
        // default bodies: typed bindings in a block under the
        // DECLARING module (so they see its items), with the trait's
        // dispatchers glob-visible
        let dscope = scope.append_block("trait", spec.id.inner());
        ctx.env.import_glob(&dscope.lexical, def.path.clone());
        let mut exprs: LPooled<Vec<Expr>> = LPooled::take();
        for (m, d) in t.methods.iter().zip(def.methods.iter()) {
            if let Some(body) = &m.default {
                let sig = d.typ.reset_tvars();
                let b = BindExpr {
                    rec: false,
                    pattern: StructurePattern::Bind(m.name.clone()),
                    typ: Some(Type::Fn(Arc::new(sig.clone()))),
                    value: annotate_lambda(body, &sig),
                };
                exprs.push(Expr {
                    id: ExprId::new(),
                    ori: body.ori.clone(),
                    pos: body.pos,
                    kind: ExprKind::Bind(Arc::new(b)),
                    dec: None,
                });
            }
        }
        let exprs: Arc<[Expr]> = Arc::from_iter(exprs.drain(..));
        let defaults =
            Block::compile(ctx, flags, spec.clone(), &dscope, top_id, true, &exprs)
                .with_context(|| format!("in the default methods of trait {}", t.name))?;
        let mut defaults_by_name: LPooled<Vec<(CompactString, crate::BindId)>> =
            LPooled::take();
        if let Some(binds) = ctx.env.binds.get(&dscope.lexical) {
            for (n, id) in binds.into_iter() {
                defaults_by_name.push((n.clone(), *id));
            }
        }
        let def = ctx.env.set_trait_defaults(def.id, defaults_by_name.drain(..));
        Ok(Node::new(Self { spec, def, defaults }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Trait<R, E> {
    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> &TagValue {
        self.defaults.update(ctx, event);
        TagValue::phantom_ref()
    }

    fn typecheck0(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        wrap!(self.defaults, self.defaults.typecheck0(ctx))
    }

    fn typecheck1(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        wrap!(self.defaults, self.defaults.typecheck1(ctx))
    }

    fn refs(&self, refs: &mut Refs) {
        self.defaults.refs(refs)
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.defaults.delete(ctx);
        ctx.env.undeftrait(&self.def);
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.defaults.sleep(ctx)
    }

    fn reset_replay(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.defaults.reset_replay(ctx)
    }

    fn typ(&self) -> &Type {
        &Type::Bottom
    }

    fn view(&self) -> NodeView<'_, R, E> {
        self.defaults.view()
    }

    fn fuse(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<Option<Node<R, E>>> {
        self.defaults.fuse(ctx)
    }
}

#[derive(Debug)]
pub struct Impl<R: Rt, E: UserEvent> {
    spec: Expr,
    pub(crate) def: Arc<ImplDef>,
    /// The interface declaration (`impl T for X;`) this
    /// implementation fulfils, when there is one: the declaration is
    /// the registered impl and `def`'s methods proxy to its bindings
    /// (`Env::register_impl`).
    pub(crate) fulfils: Option<Arc<ImplDef>>,
    trait_def: Arc<TraitDef>,
    pub(crate) body: Node<R, E>,
    /// For a core trait (`Eq`/`Ord`/`Display`), one never-run call
    /// site per method so the analysis REACHES the method's body —
    /// the hooked walks call it from sites built after analysis, and
    /// the implicit `#[sync]` the methods carry is verified only on a
    /// covered definition.
    pub(crate) prototypes: Vec<Node<R, E>>,
}

/// Where may `impl Trait for target` be written? An abstract type's
/// impl belongs to the type's package or the trait's (the orphan
/// rule); any other target only to the trait's package — a
/// structural impl applies to every type of that shape program-wide,
/// and only the trait's author answers for that (`design/traits.md`
/// §4). `declared` is an interface's `impl T for X;`: what kind of
/// abstract a hidden `type X;` is becomes known only when the
/// implementation defines it, and the implementation's own `impl`
/// block (which the declaration requires) answers for that.
pub(crate) fn check_target(
    env: &Env,
    scope: &ModPath,
    trait_def: &TraitDef,
    target: &Type,
    declared: bool,
) -> Result<()> {
    let here = env.package_root(scope);
    let trait_pkg = env.package_root(&trait_def.scope);
    let canonical = match target {
        Type::Ref(tr) => {
            if env.trait_of_ref(tr).is_some() {
                bail!("a trait is not an implementation target")
            }
            target.lookup_ref(env)?
        }
        t => t.clone(),
    };
    // A CORE trait rides the VALUE (`design/traits.md` §12): the
    // implementation is consulted through the box a Graphix
    // constructor mints, which is the only kind of abstract value that
    // carries a payload for the implementation to read. A Rust-backed
    // value carries none, so an implementation for one would compile
    // and never be called — refuse it rather than let it look like it
    // works.
    if !declared
        && crate::node::coretraits::CoreTrait::of_id(trait_def.id).is_some()
        && let Type::Abstract { id, .. } = &canonical
        && !env.abstract_minted(*id)
    {
        bail!(
            "impl {} for {target}: {target} is backed by Rust, so it has no \
             payload for the implementation to read and nothing would consult \
             it — its equality, ordering and printing are the ones its package \
             defined",
            trait_def.name
        )
    }
    match &canonical {
        Type::Abstract { id, .. } => {
            let type_pkg = match env.abstract_reps.get(id) {
                Some(rep) => env.package_root(&rep.scope).to_string(),
                None => match target {
                    Type::Ref(tr) => tr
                        .resolve_in(env)
                        .map(|r| env.package_root(r.canonical_scope()).to_string())
                        .unwrap_or_default(),
                    _ => String::new(),
                },
            };
            if here != trait_pkg && here != type_pkg {
                bail!(
                    "impl {} for {target}: an abstract type's implementation must live \
                     in the type's package ({type_pkg}) or the trait's ({trait_pkg})",
                    trait_def.name
                )
            }
        }
        Type::Set(_) => bail!(
            "impl {} for {target}: a union is never an implementation target; \
             implement each member",
            trait_def.name
        ),
        Type::TVar(_) => bail!(
            "impl {} for {target}: a bare type variable is not an implementation target",
            trait_def.name
        ),
        Type::Any | Type::Bottom => {
            bail!("impl {} for {target}: not an implementation target", trait_def.name)
        }
        _ => {
            if here != trait_pkg {
                bail!(
                    "impl {} for {target}: only the trait's own package ({trait_pkg}) may \
                     implement it for a non-abstract type; give the type a name with \
                     `type T = Abstract<..>`",
                    trait_def.name
                )
            }
        }
    }
    Ok(())
}

/// The head of an impl, scoped: the target with its declared type
/// variables aliased across it and their bounds seeded on the cells,
/// checked for stray/unused variables and against the target rule.
pub(crate) fn impl_head(
    env: &Env,
    scope: &ModPath,
    trait_def: &TraitDef,
    im: &ImplExpr,
    declared: bool,
) -> Result<(Type, Arc<[TVar]>)> {
    let target = im.target.scope_refs(scope);
    let mut known: LPooled<ahash::AHashMap<ArcStr, TVar>> = LPooled::take();
    let params: Arc<[TVar]> = Arc::from_iter(im.params.iter().map(|tv| {
        let tv = TVar::empty_named(tv.name.clone());
        known.insert(tv.name.clone(), tv.clone());
        tv
    }));
    target.alias_tvars(&mut known);
    for (tv, tc) in im.constraints.iter() {
        let tc = tc.scope_refs(scope);
        tc.alias_tvars(&mut known);
        known[&tv.name].add_cell_constraint(tc);
    }
    let mut in_target: LPooled<ahash::AHashMap<ArcStr, TVar>> = LPooled::take();
    target.collect_tvars(&mut in_target);
    for tv in params.iter() {
        if !in_target.contains_key(&tv.name) {
            bail!("impl type variable {tv} does not occur in the target {target}")
        }
    }
    for (name, _) in in_target.iter() {
        if !known.contains_key(name) {
            bail!("undeclared type variable '{name} in impl target {target}")
        }
    }
    check_target(env, scope, trait_def, &target, declared)?;
    Ok((target, params))
}

impl<R: Rt, E: UserEvent> Impl<R, E> {
    pub(crate) fn compile(
        ctx: &mut ExecCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: Expr,
        scope: &Scope,
        im: &ImplExpr,
        top_id: ExprId,
    ) -> Result<Node<R, E>> {
        let trait_id = match ctx.env.lookup_trait(&scope.lexical, &im.trait_name)? {
            Some(id) => id,
            None => bail!("no trait `{}` in scope at {}", im.trait_name, spec.pos),
        };
        let trait_def = ctx.env.trait_def(trait_id).cloned().ok_or_else(|| {
            anyhow::anyhow!("trait {} has no definition", im.trait_name)
        })?;
        let (target, params) = impl_head(&ctx.env, &scope.lexical, &trait_def, im, false)
            .with_context(|| format!("at {}", spec.pos))?;
        // the methods: a block below the declaring module, the trait's
        // dispatchers glob-visible, each binding annotated with the
        // declared signature at the target (a user annotation is
        // checked against it in typecheck0)
        let bscope = scope.append_block("impl", spec.id.inner());
        ctx.env.import_glob(&bscope.lexical, trait_def.path.clone());
        let core = super::coretraits::CoreTrait::of_id(trait_id).is_some();
        let mut exprs: LPooled<Vec<Expr>> = LPooled::take();
        let mut provided: LPooled<ahash::AHashSet<ArcStr>> = LPooled::take();
        for m in im.methods.iter() {
            let ExprKind::Bind(b) = &m.kind else {
                unreachable!("impl methods are binds")
            };
            let StructurePattern::Bind(name) = &b.pattern else {
                unreachable!("impl methods are simple binds")
            };
            let Some(decl) = trait_def.methods.iter().find(|d| &d.name == name) else {
                bail!(
                    "{} is not a method of trait {} (at {})",
                    name,
                    trait_def.name,
                    m.pos
                )
            };
            if !provided.insert(name.clone()) {
                bail!("method {name} is implemented twice (at {})", m.pos);
            }
            let sig = method_sig_at(&decl.typ.reset_tvars(), &target);
            let b = BindExpr {
                rec: b.rec,
                pattern: b.pattern.clone(),
                typ: b.typ.clone().or_else(|| Some(Type::Fn(Arc::new(sig.clone())))),
                value: annotate_lambda(&b.value, &sig),
            };
            // a core trait's method runs INSIDE a comparison or a
            // print, so it is implicitly `#[sync]`
            let dec = match (core, &m.dec) {
                (false, dec) => dec.clone(),
                (true, dec) => {
                    let sync = crate::expr::Attr {
                        name: arcstr::literal!("sync"),
                        args: Arc::from_iter([]),
                    };
                    let (comments, attrs) = match dec {
                        Some(d) => (d.comments.clone(), d.attrs.clone()),
                        None => (Arc::from_iter([]), Arc::from_iter([])),
                    };
                    Some(Box::new(crate::expr::Decorations {
                        comments,
                        attrs: Arc::from_iter(attrs.iter().cloned().chain([sync])),
                    }))
                }
            };
            exprs.push(Expr {
                id: m.id,
                ori: m.ori.clone(),
                pos: m.pos,
                kind: ExprKind::Bind(Arc::new(b)),
                dec,
            });
        }
        for d in trait_def.methods.iter() {
            if !provided.contains(&d.name) && d.default.is_none() {
                bail!(
                    "impl {} for {target} is missing the required method {} (at {})",
                    trait_def.name,
                    d.name,
                    spec.pos
                )
            }
        }
        let exprs: Arc<[Expr]> = Arc::from_iter(exprs.drain(..));
        let body =
            Block::compile(ctx, flags, spec.clone(), &bscope, top_id, true, &exprs)
                .with_context(|| format!("in impl {} for {target}", trait_def.name))?;
        let mut methods: Map<CompactString, crate::BindId> = Map::new();
        if let Some(binds) = ctx.env.binds.get(&bscope.lexical) {
            for (n, id) in binds.into_iter() {
                methods.insert_cow(n.clone(), *id);
            }
        }
        let def = Arc::new(ImplDef {
            trait_id,
            target,
            params,
            scope: bscope.lexical.clone(),
            methods,
            declared: false,
            pos: spec.pos,
            ori: spec.ori.clone(),
        });
        let fulfils = ctx
            .env
            .register_impl(def.clone())
            .with_context(|| format!("at {}", spec.pos))?;
        Ok(Node::new(Self {
            spec,
            def,
            fulfils,
            trait_def,
            body,
            prototypes: Vec::new(),
        }))
    }

    /// The core-trait prototypes: a call site per method over
    /// synthesized argument bindings of the target type, typechecked
    /// and statically resolved like any call, never updated.
    fn build_prototypes(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        use super::genn;
        if super::coretraits::CoreTrait::of_id(self.def.trait_id).is_none() {
            return Ok(());
        }
        let scope =
            Scope { lexical: self.def.scope.clone(), dynamic: self.def.scope.clone() };
        let top_id = self.spec.id;
        for (k, (_, bind)) in self.def.methods.clone().into_iter().enumerate() {
            let Some(ftype) = super::coretraits::method_ftype(&ctx.env, *bind) else {
                bail!("impl method {:?} is not a function", bind)
            };
            let mut args: SmallVec<[Node<R, E>; 2]> = SmallVec::new();
            for (i, a) in ftype.args.iter().enumerate() {
                let name = format_compact!("#proto{}_{k}_{i}", self.spec.id.inner());
                let (_, n) =
                    genn::bind(ctx, &scope.lexical, &name, a.typ.clone(), top_id);
                args.push(n);
            }
            let fnode = genn::reference(ctx, *bind, Type::Fn(ftype.clone()), top_id);
            let mut site = genn::apply(fnode, scope.clone(), args, &ftype, top_id);
            site.typecheck0(ctx)?;
            site.typecheck1(ctx)?;
            self.prototypes.push(site);
        }
        Ok(())
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Impl<R, E> {
    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> &TagValue {
        self.body.update(ctx, event);
        TagValue::phantom_ref()
    }

    fn typecheck0(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        wrap!(self.body, self.body.typecheck0(ctx))?;
        // a user-annotated method must still fit the declared signature
        for d in self.trait_def.methods.iter() {
            let Some(id) = self.def.methods.get(d.name.as_str()) else { continue };
            let Some(bind) = ctx.env.by_id.get(id) else { continue };
            let expected =
                Type::Fn(Arc::new(method_sig_at(&d.typ.reset_tvars(), &self.def.target)));
            expected.check_contains(&ctx.env, &bind.typ).with_context(|| {
                format!(
                    "method {} of impl {} for {} has type {}, the trait declares {expected}",
                    d.name, self.trait_def.name, self.def.target, bind.typ
                )
            })?;
        }
        Ok(())
    }

    fn typecheck1(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        wrap!(self.body, self.body.typecheck1(ctx))?;
        if self.prototypes.is_empty() {
            wrap!(self.body, self.build_prototypes(ctx))?;
        }
        Ok(())
    }

    fn refs(&self, refs: &mut Refs) {
        self.body.refs(refs);
        for p in self.prototypes.iter() {
            p.refs(refs)
        }
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.body.delete(ctx);
        for p in self.prototypes.iter_mut() {
            p.delete(ctx)
        }
        if self.fulfils.is_none() {
            ctx.env.unregister_impl(&self.def);
        }
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.body.sleep(ctx)
    }

    fn reset_replay(&mut self, ctx: &mut ExecCtx<R, E>) {
        self.body.reset_replay(ctx)
    }

    fn typ(&self) -> &Type {
        &Type::Bottom
    }

    fn view(&self) -> NodeView<'_, R, E> {
        NodeView::Impl(self)
    }

    fn fuse(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<Option<Node<R, E>>> {
        self.body.fuse(ctx)
    }
}
