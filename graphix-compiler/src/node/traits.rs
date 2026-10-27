//! `trait` declarations and `impl` blocks.
//!
//! A trait declaration registers the trait and binds one dispatcher
//! per method under the trait's scope (`Read::read`); a call through a
//! dispatcher resolves by its `self` argument's type at typecheck1
//! (`CallSite::resolve_trait_call`). Default bodies and impl methods
//! compile as typed bindings in a block below the declaring module
//! with the trait's dispatchers glob-visible.

use super::{
    Block,
    bind::lower_over_operands,
    callsite::{ArgKey, CallSite},
    coretraits::method_ftype,
    genn::SynthCall,
    lambda::LambdaDef,
};
use crate::{
    BindId, CFlag, CompileCtx, DefAssertionKind, ExecCtx, Node, NodeView, Rt, Scope,
    SourcePosition, TagValue, Update, bailat,
    env::{Env, Glob, ImplDef, Map, TraitDef, TraitMethodRef, TraitMethodSig},
    expr::{
        ApplyExpr, Arg, ArgKind, At, Attr, BindExpr, Decorations, Expr, ExprId, ExprKind,
        ImplExpr, ImplHead, LambdaExpr, ModPath, Name, Origin, Pattern, SelectExpr,
        StructurePattern, TraitExpr, TraitMethod, WrittenAt,
    },
    ide::ModuleRefSite,
    image::{
        ImageBuf,
        nodes::{NodeTag, decode_node, put_tag},
    },
    typ::{CoreTrait, FnArgKind, FnType, TVar, TraitId, Type, TypeRef, seed_conjuncts},
    wrap,
};
use ahash::{AHashMap, AHashSet};
use anyhow::{Context, Result, anyhow, bail};
use arcstr::{ArcStr, literal};
use compact_str::{CompactString, format_compact};
use enumflags2::BitFlags;
use netidx_core::pack::{Pack, PackError, encode_varint};
use netidx_value::Value;
use poolshark::local::LPooled;
use smallvec::SmallVec;
use triomphe::Arc;

/// The declared signature of a trait method, scoped to the declaring
/// module and written as any signature is (`rewrite_trait_args`), with
/// `self` a trait-bounded quantifier (rigid while a default body is
/// checked).
pub(crate) fn method_sig(
    env: &Env,
    parsed: &FnType,
    tref: &Type,
    scope: &ModPath,
) -> Result<FnType> {
    let ft =
        match &Type::Fn(Arc::new(parsed.scope_refs(scope))).rewrite_trait_args(env)? {
            Type::Fn(ft) => (**ft).clone(),
            _ => unreachable!("a rewritten function type is a function type"),
        };
    let mut known: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
    ft.alias_tvars(&mut known);
    if let Some(tv) = known.get("self") {
        tv.add_cell_constraint(tref.clone());
    }
    let mut quantifiers: LPooled<Vec<ArcStr>> = ft.quantifiers.iter().cloned().collect();
    if !quantifiers.iter().any(|q| &**q == "self") {
        quantifiers.push(literal!("self"));
    }
    Ok(FnType { quantifiers: Arc::from_iter(quantifiers.drain(..)), ..ft })
}

/// Whether `implemented`, a method's type as its body checked it, is
/// the `declared` signature: its shape, its variables, and no bound the
/// declaration does not carry ([`FnType::sig_matches`], as an interface
/// `val` is checked).
fn implements(env: &Env, declared: &FnType, implemented: &Type) -> Result<()> {
    match implemented.with_deref(|t| match t {
        Some(Type::Fn(ft)) => Some(ft.clone()),
        _ => None,
    }) {
        Some(ft) => declared.sig_matches(env, &ft),
        None => bail!("{implemented} is not a function"),
    }
}

/// A trait method as the trait declares it: its signature
/// ([`method_sig`]) and what every body of it must be, the effect
/// assertions written above it.
pub(crate) fn method_decl(
    env: &Env,
    m: &TraitMethod,
    tref: &Type,
    scope: &ModPath,
    trait_name: &str,
    pos: SourcePosition,
) -> Result<TraitMethodSig> {
    if m.self_index.is_none()
        && CoreTrait::of_id(TraitId::of(scope, trait_name)).is_none()
    {
        bail!(
            "trait method {} takes no `self` argument, so no call can dispatch it: \
             only a core trait's method, which the runtime calls, may return `self` \
             alone",
            m.name.name
        )
    }
    let mut requires: SmallVec<[ArcStr; 2]> = SmallVec::new();
    for a in m.attrs.iter() {
        let kind = DefAssertionKind::from_name(&a.name)
            .filter(|k| matches!(k, DefAssertionKind::Pure | DefAssertionKind::Sync));
        if kind.is_none() || !a.args.is_empty() {
            bail!(
                "{a} on trait method {}: a method requires only #[pure] or #[sync]",
                m.name.name
            )
        }
        if requires.contains(&a.name) {
            bail!("{a} is written twice on trait method {}", m.name.name)
        }
        requires.push(a.name.clone());
    }
    Ok(TraitMethodSig {
        name: m.name.name.clone(),
        pos: m.name.pos_or(pos),
        typ: Arc::new(method_sig(env, &m.typ, tref, scope)?),
        self_index: m.self_index,
        requires: Arc::from_iter(requires),
    })
}

/// `requires` as the attributes that assert it of a body.
fn required_attrs(requires: &[ArcStr]) -> impl Iterator<Item = Attr> + '_ {
    requires.iter().map(|name| Attr { name: name.clone(), args: Arc::from_iter([]) })
}

/// The method signature, a fresh copy, instantiated at an
/// implementation target: `self := target`, and a variable named like
/// one of the target's renamed apart, so the two never read as one.
pub(crate) fn method_sig_at(sig: &FnType, target: &Type) -> FnType {
    let mut theirs: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
    target.collect_tvars(&mut theirs);
    let mut ours: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
    sig.collect_tvars(&mut ours);
    let mut known: LPooled<AHashMap<ArcStr, Type>> = LPooled::take();
    let mut renames: LPooled<AHashMap<ArcStr, ArcStr>> = LPooled::take();
    for (name, tv) in ours.iter() {
        let t = match theirs.contains_key(name) {
            false => Type::TVar(tv.clone()),
            true => {
                let fresh = (1u64..)
                    .map(|i| ArcStr::from(format_compact!("{name}{i}").as_str()))
                    .find(|n| !theirs.contains_key(n) && !ours.contains_key(n))
                    .unwrap();
                renames.insert(name.clone(), fresh.clone());
                Type::TVar(tv.renamed(fresh))
            }
        };
        known.insert(name.clone(), t);
    }
    known.insert(literal!("self"), target.clone());
    let mut sig = sig.replace_tvars(&known);
    if !renames.is_empty() {
        sig.quantifiers =
            sig.quantifiers.iter().map(|q| renames.get(q).unwrap_or(q).clone()).collect();
    }
    sig
}

/// An impl is a global fact: one a function body declares, in a module
/// or an interface, would be registered by every compile of the body.
pub(crate) fn impl_outside_functions(
    lexical: &ModPath,
    trait_name: &ModPath,
) -> Result<()> {
    if netidx_core::path::Path::parts(&**lexical).any(crate::is_fn_block) {
        bail!(
            "impl {trait_name} is in a function body; an implementation is global, \
             declare it outside every function"
        )
    }
    Ok(())
}

/// Fill a method lambda's missing parameter and return annotations
/// from the declared signature (positional by position, labeled by
/// name), through parentheses. Written annotations are kept.
fn annotate_lambda(value: &Expr, sig: &FnType) -> Expr {
    let l = match &value.kind {
        ExprKind::Lambda(l) => l,
        ExprKind::ExplicitParens(inner) => {
            let mut e = value.clone();
            e.kind = ExprKind::ExplicitParens(Arc::new(annotate_lambda(inner, sig)));
            return e;
        }
        _ => return value.clone(),
    };
    let positional: LPooled<Vec<&Type>> = sig
        .args
        .iter()
        .filter(|a| matches!(a.kind, FnArgKind::Positional { .. }))
        .map(|a| &a.typ)
        .collect();
    let mut pos = 0usize;
    let args = Arc::from_iter(l.args.iter().map(|a| {
        let declared = match &a.kind {
            ArgKind::Positional => {
                let t = positional.get(pos).copied();
                pos += 1;
                t
            }
            ArgKind::Labeled | ArgKind::Defaulted(_) => a.pattern.single_bind().and_then(|n| {
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
        Arg { constraint, ..a.clone() }
    }));
    let rtype = l.rtype.clone().or_else(|| Some(sig.rtype.clone()));
    let throws =
        l.throws.clone().or_else(|| sig.explicit_throws.then(|| sig.throws.clone()));
    let mut e = value.clone();
    e.kind = ExprKind::Lambda(Arc::new(LambdaExpr {
        args,
        vargs: l.vargs.clone(),
        rtype,
        throws,
        constraints: l.constraints.clone(),
        body: l.body.clone(),
    }));
    e
}

pub(crate) fn trait_ref(
    scope: &ModPath,
    name: &ArcStr,
    pos: SourcePosition,
    ori: &Arc<Origin>,
) -> Type {
    Type::Ref(Arc::new(TypeRef::new(
        scope.clone(),
        ModPath::from([name.clone()]),
        Arc::from_iter([]),
        Some(pos),
        Some(ori.clone()),
    )))
}

#[derive(Debug)]
pub struct Trait<R: Rt> {
    spec: Arc<Expr>,
    def: Arc<TraitDef>,
    defaults: Node<R>,
}

impl<R: Rt> Trait<R> {
    pub(crate) fn compile(
        ctx: &mut CompileCtx<R>,
        flags: BitFlags<CFlag>,
        spec: Arc<Expr>,
        scope: &Scope,
        t: &TraitExpr,
        top_id: ExprId,
    ) -> Result<Node<R>> {
        if netidx_core::path::Path::parts(&*scope.lexical).any(crate::is_fn_block) {
            bailat!(
                spec,
                "trait {} is in a function body; a trait is declared once, outside \
                 every function",
                t.name
            )
        }
        let tref = trait_ref(&scope.lexical, &t.name, spec.pos, &spec.ori);
        let mut sigs = t
            .methods
            .iter()
            .map(|m| method_decl(&ctx.env, m, &tref, &scope.lexical, &t.name, spec.pos))
            .collect::<Result<LPooled<Vec<_>>>>()
            .at(&spec)?;
        let def = ctx
            .env
            .deftrait(
                &scope.lexical,
                &t.name,
                sigs.drain(..),
                None,
                t.name.pos_or(spec.pos),
                spec.ori.clone(),
            )
            .at(&spec)?;
        let dscope = scope.append_block("trait", spec.id.inner());
        ctx.env.import_glob(&dscope.lexical, Glob::Module(def.path.clone()));
        let mut exprs: LPooled<Vec<Expr>> = LPooled::take();
        for (m, d) in t.methods.iter().zip(def.methods.iter()) {
            // bound under a name no method call reaches (`#show`): a
            // bare `show` in a sibling default is the dispatcher
            if let Some(body) = &m.default {
                let sig = d.typ.reset_tvars();
                let private = format_compact!("#{}", m.name.name);
                let name =
                    Name { name: ArcStr::from(private.as_str()), ..m.name.clone() };
                let b = BindExpr {
                    rec: false,
                    pattern: StructurePattern::Bind(name),
                    typ: Some(Type::Fn(Arc::new(sig.clone()))),
                    value: annotate_lambda(body, &sig),
                };
                let mut e = body.clone();
                e.id = ExprId::new();
                e.kind = ExprKind::Bind(Arc::new(b));
                e.dec = (!d.requires.is_empty()).then(|| {
                    Arc::new(Decorations {
                        attrs: Arc::from_iter(required_attrs(&d.requires)),
                        ..Decorations::default()
                    })
                });
                exprs.push(e);
            }
        }
        let exprs: Arc<[Expr]> = Arc::from_iter(exprs.drain(..));
        let defaults =
            Block::compile(ctx, flags, spec.clone(), &dscope, top_id, true, &exprs)
                .with_context(|| format!("in the default methods of trait {}", t.name))?;
        let mut defaults_by_name: LPooled<Vec<(CompactString, BindId)>> = LPooled::take();
        if let Some(binds) = ctx.env.lexical.binds.get(&dscope.lexical) {
            for (n, id) in binds.into_iter() {
                let n = n.strip_prefix('#').unwrap_or(n);
                defaults_by_name.push((CompactString::from(n), *id));
            }
        }
        // the node keeps its own declaration: deleting it removes the
        // dispatchers it minted, and the record only when it is the record
        ctx.env.set_trait_defaults(def.id, defaults_by_name.drain(..));
        Ok(Node::new(Self { spec, def, defaults }))
    }
}

impl<R: Rt> Trait<R> {
    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<'_, R>,
        buf: &mut &[u8],
    ) -> Result<Node<R>, PackError> {
        let spec = Arc::new(Expr::decode(buf)?);
        let def = Arc::<TraitDef>::decode(buf)?;
        let defaults = decode_node(ctx, buf)?;
        Ok(Node::new(Self { spec, def, defaults }))
    }
}

impl<R: Rt> Update<R> for Trait<R> {
    /// An instance checks this node as its definition's check did.
    fn typecheck0_instance(
        &mut self,
        ctx: &mut CompileCtx<R>,
        _types: &mut crate::node::lambda::InstanceTypes,
    ) -> Result<()> {
        self.typecheck0(ctx)
    }

    fn for_each_child<'a>(&'a self, f: &mut dyn FnMut(&'a Node<R>)) {
        f(&self.defaults)
    }

    fn for_each_child_mut(&mut self, f: &mut dyn FnMut(&mut Node<R>)) {
        f(&mut self.defaults)
    }

    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::Trait, buf);
        self.spec.encode(buf)?;
        self.def.encode(buf)?;
        self.defaults.image_encode(buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R>) -> &TagValue {
        self.defaults.update(ctx);
        TagValue::phantom_ref()
    }

    fn typecheck0(&mut self, ctx: &mut CompileCtx<R>) -> Result<()> {
        wrap!(self.defaults, self.defaults.typecheck0(ctx))?;
        let Some(record) = ctx.env.trait_def(self.def.id).cloned() else { return Ok(()) };
        // every written name in a method's signature must name something by
        // the check's end
        for m in record.methods.iter() {
            crate::defer_unresolved_names(ctx, &Type::Fn(m.typ.clone()), &self.spec)?;
        }
        for m in record.methods.iter() {
            let Some(id) = m.default else { continue };
            let Some(bind) = ctx.env.by_id.get(&id) else { continue };
            let declared = m.typ.reset_tvars();
            implements(&ctx.env, &declared, &bind.typ).with_context(|| {
                format!(
                    "the default {} of trait {} does not implement the declared {declared}",
                    m.name, self.def.name
                )
            })?;
        }
        Ok(())
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R>) {
        self.defaults.delete(ctx);
        ctx.env.undeftrait(&self.def);
    }

    fn typ(&self) -> &Type {
        Type::BOTTOM
    }

    /// A declaration has nothing of its own to walk: every view walker
    /// treats a trait exactly as its defaults' module block.
    fn view(&self) -> NodeView<'_, R> {
        self.defaults.view()
    }

    fn fuse(&mut self, ctx: &mut CompileCtx<R>) -> Result<Option<Node<R>>> {
        self.defaults.fuse(ctx)
    }
}

/// The interface declaration (`impl T for X;`) an implementation
/// fulfils: the registered impl, whose bindings the implementation's
/// methods proxy to.
#[derive(Debug, Clone, netidx_derive::Pack)]
pub(crate) struct Fulfils {
    /// The registered entry.
    pub(crate) entry: Arc<ImplDef>,
    /// The interface's declaration, when `entry` narrows it to the
    /// methods the implementation writes; the implementation's delete
    /// registers it again.
    narrowed: Option<Arc<ImplDef>>,
}

#[derive(Debug)]
pub struct Impl<R: Rt> {
    spec: Arc<Expr>,
    pub(crate) def: Arc<ImplDef>,
    pub(crate) fulfils: Option<Fulfils>,
    trait_def: Arc<TraitDef>,
    pub(crate) body: Node<R>,
    /// For a core trait, one never-run call site per method so the
    /// analysis reaches the method's body and verifies its implicit
    /// `#[sync]`.
    pub(crate) prototypes: Vec<SynthCall<R>>,
}

/// The orphan rule: an abstract type's impl belongs to the type's
/// package or the trait's; any other target only to the trait's
/// package. `declared` is an interface's `impl T for X;`, whose
/// abstract kind is checked by the implementation's own `impl` block.
pub(crate) fn check_target(
    env: &Env,
    scope: &ModPath,
    trait_def: &TraitDef,
    target: &Type,
    declared: bool,
) -> Result<()> {
    let here = env.package_root(scope);
    let trait_pkg = env.package_root(&trait_def.scope);
    // a constructor trait's reference head is the named constructor,
    // never expanded: it belongs to the package defining the name
    if trait_def.hole
        && let Type::Ref(tr) = target
        && env.trait_of_ref(tr).is_none()
    {
        let resolved = tr.resolve_in(env);
        let type_pkg =
            resolved.as_ref().map_or("", |r| env.package_root(r.canonical_scope()));
        if here != trait_pkg && here != type_pkg {
            bail!(
                "impl {} for {target}: a named type's implementation must live in the \
                 type's package ({type_pkg}) or the trait's ({trait_pkg})",
                trait_def.name
            )
        }
        return Ok(());
    }
    let canonical = match target {
        Type::Ref(tr) => {
            if env.trait_of_ref(tr).is_some() {
                bail!("a trait is not an implementation target")
            }
            target.lookup_ref(env)?
        }
        t => t.clone(),
    };
    // a core-trait impl is consulted through a Graphix-minted box; a
    // Rust-backed abstract carries no payload, so its impl would
    // never be called
    if !declared
        && CoreTrait::of_id(trait_def.id).is_some()
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
    if !declared && CoreTrait::of_id(trait_def.id) == Some(CoreTrait::Cast) {
        match &canonical {
            Type::Abstract { id, .. } if env.abstract_rep(*id, scope).is_some() => (),
            _ => bail!(
                "impl Cast for {target}: only an abstract type implements Cast, \
                 where its representation is visible"
            ),
        }
    }
    match &canonical {
        Type::Abstract { id, .. } => {
            let resolved = match target {
                Type::Ref(tr) => tr.resolve_in(env),
                _ => None,
            };
            let type_pkg = match env.abstract_reps.get(id) {
                Some(rep) => env.package_root(&rep.scope),
                None => resolved
                    .as_ref()
                    .map_or("", |r| env.package_root(r.canonical_scope())),
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
        Type::Primitive(p) if p.len() > 1 => bail!(
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
    head: &ImplHead,
    declared: bool,
) -> Result<(Type, Arc<[TVar]>)> {
    let target = head.target.scope_refs(scope);
    let mut known: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
    let params: Arc<[TVar]> = Arc::from_iter(head.params.iter().map(|tv| {
        let tv = TVar::empty_generic(tv.name.clone());
        known.insert(tv.name.clone(), tv.clone());
        tv
    }));
    let mut in_target: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
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
    for (tv, tc) in head.constraints.iter() {
        in_target.clear();
        tc.collect_tvars(&mut in_target);
        for (name, _) in in_target.iter() {
            if !known.contains_key(name) {
                bail!("undeclared type variable '{name} in the constraint on {tv}")
            }
        }
    }
    target.alias_tvars(&mut known);
    seed_conjuncts(
        &mut known,
        head.constraints.iter().map(|(tv, tc)| (tv, tc.scope_refs(scope))),
    );
    let holes = target.holes();
    if trait_def.hole {
        if holes != 1 || !matches!(target.decompose(), Some((_, Type::Hole))) {
            bail!(
                "impl {} for {target}: {} is a constructor trait, so its target names a \
                 type constructor with the last parameter left as the hole '_ \
                 (`Array<'_>`, `Map<'k, '_>`, `List<'_>`)",
                trait_def.name,
                trait_def.name
            )
        }
    } else if holes != 0 {
        bail!(
            "impl {} for {target}: '_ is the hole of a constructor trait's target, and \
             {} applies `self` as a type, not a constructor",
            trait_def.name,
            trait_def.name
        )
    }
    check_target(env, scope, trait_def, &target, declared)?;
    Ok((target, params))
}

impl<R: Rt> Impl<R> {
    pub(crate) fn compile(
        ctx: &mut CompileCtx<R>,
        flags: BitFlags<CFlag>,
        spec: Arc<Expr>,
        scope: &Scope,
        im: &ImplExpr,
        top_id: ExprId,
    ) -> Result<Node<R>> {
        impl_outside_functions(&scope.lexical, &im.head.trait_name).at(&spec)?;
        let trait_id =
            match ctx.env.lookup_trait(&scope.lexical, &im.head.trait_name).at(&spec)? {
                Some(id) => id,
                None => bailat!(spec, "no trait `{}` in scope", im.head.trait_name),
            };
        let trait_def = ctx.env.trait_def(trait_id).cloned().ok_or_else(|| {
            anyhow!("trait {} has no definition", im.head.trait_name).at(&spec)
        })?;
        if ctx.env.ide.is_lsp() {
            ctx.env.push_module_reference(ModuleRefSite {
                pos: im.head.trait_at.0.first().copied().unwrap_or(spec.pos),
                ori: spec.ori.clone(),
                name: im.head.trait_name.clone(),
                canonical: trait_def.path.clone(),
                def_ori: None,
                segments: Some(im.head.trait_at.clone()),
            });
        }
        let (target, params) =
            impl_head(&ctx.env, &scope.lexical, &trait_def, &im.head, false).at(&spec)?;
        let bscope = scope.append_block("impl", spec.id.inner());
        ctx.env.import_glob(&bscope.lexical, Glob::Module(trait_def.path.clone()));
        let mut exprs: LPooled<Vec<Expr>> = LPooled::take();
        let mut provided: LPooled<AHashSet<ArcStr>> = LPooled::take();
        for m in im.methods.iter() {
            let ExprKind::Bind(b) = &m.kind else {
                unreachable!("impl methods are binds")
            };
            let StructurePattern::Bind(name) = &b.pattern else {
                unreachable!("impl methods are simple binds")
            };
            let Some(decl) = trait_def.methods.iter().find(|d| d.name == name.name)
            else {
                bailat!(m, "{} is not a method of trait {}", name, trait_def.name)
            };
            if !provided.insert(name.name.clone()) {
                bailat!(m, "method {name} is implemented twice");
            }
            let sig = method_sig_at(&decl.typ.reset_tvars(), &target);
            let b = BindExpr {
                rec: b.rec,
                pattern: b.pattern.clone(),
                typ: b.typ.clone().or_else(|| Some(Type::Fn(Arc::new(sig.clone())))),
                value: annotate_lambda(&b.value, &sig),
            };
            let dec = match &m.dec {
                _ if decl.requires.is_empty() => m.dec.clone(),
                None => Some(Arc::new(Decorations {
                    attrs: Arc::from_iter(required_attrs(&decl.requires)),
                    ..Decorations::default()
                })),
                Some(d) => Some(Arc::new(Decorations {
                    attrs: Arc::from_iter(
                        d.attrs.iter().cloned().chain(required_attrs(&decl.requires)),
                    ),
                    ..(**d).clone()
                })),
            };
            let mut e = m.clone();
            e.kind = ExprKind::Bind(Arc::new(b));
            e.dec = dec;
            exprs.push(e);
        }
        if CoreTrait::representation_default(trait_id)
            && trait_def.methods.iter().any(|d| !provided.contains(&d.name))
            && let Type::Abstract { id, .. } =
                target.lookup_ref(&ctx.env).unwrap_or(target.clone())
            && let Some(rep) = ctx.env.abstract_reps.get(&id)
            && (rep.rep.holds_ref(&ctx.env) || rep.rep.holds_fn(&ctx.env))
        {
            bailat!(
                spec,
                "impl Cast for {target}: its representation holds a function or a \
                 reference, which no data carries; write to_value and from_value"
            )
        }
        for d in trait_def.methods.iter() {
            if !provided.contains(&d.name)
                && d.default.is_none()
                && !CoreTrait::representation_default(trait_id)
            {
                bailat!(
                    spec,
                    "impl {} for {target} is missing the required method {}",
                    trait_def.name,
                    d.name
                )
            }
        }
        let exprs: Arc<[Expr]> = Arc::from_iter(exprs.drain(..));
        let body =
            Block::compile(ctx, flags, spec.clone(), &bscope, top_id, true, &exprs)
                .with_context(|| format!("in impl {} for {target}", trait_def.name))?;
        let mut methods: Map<CompactString, BindId> = Map::new();
        if let Some(binds) = ctx.env.lexical.binds.get(&bscope.lexical) {
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
        let fulfils = ctx.env.register_impl(def.clone()).at(&spec)?.map(|declared| {
            if CoreTrait::representation_default(trait_id) {
                narrow_to_representation(ctx, declared, &def)
            } else {
                Fulfils { entry: declared, narrowed: None }
            }
        });
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
    // CR claude for claude: [bug/medium] Only core traits get prototype call sites, so a
    // user trait's declared #[pure]/#[sync] is checked of an impl only when some
    // build-time call reaches it. An uncalled violating impl compiles. A dynamic
    // module's impure impl of a sig-declared `#[pure]` method loads with status null:
    // the violation is only logged at the consumer's runtime bind, and the stateful
    // body runs. CLAUDE.md says the requirement is asserted of every impl body, and an
    // impure core-trait impl in the same position is refused as a DynamicLoadError.
    // Decide whether the requirement is a contract on every impl. If it is, build
    // prototypes here for every method whose TraitMethodDef carries a requirement, and
    // pin both the static and the dynamic case. probe:
    // design/review-2026-10-26/repro/tests-fusion-traits-trait-requirement-unchecked.gx
    // (tests-fusion-traits-01)
    // 2026-10-27 claude: Eric ruled (2026-10-27, cr-questions round 2, q36, A trait
    // method's `#[pure]` is a contract on every impl): yes, every impl (the
    // recommendation: every impl, as CLAUDE.md says.)
    fn build_prototypes(&mut self, ctx: &mut CompileCtx<R>) -> Result<()> {
        if CoreTrait::of_id(self.def.trait_id).is_none() {
            return Ok(());
        }
        let scope = Scope { lexical: self.def.scope.clone(), ..Scope::root() };
        let top_id = self.spec.id;
        for (k, (_, bind)) in self.def.methods.clone().into_iter().enumerate() {
            let Some(ftype) = method_ftype(&ctx.env, *bind) else {
                bail!("impl method {:?} is not a function", bind)
            };
            let prefix = format_compact!("#proto{}_{k}", self.spec.id.inner());
            let types = ftype.args.iter().map(|a| a.typ.clone());
            let call =
                SynthCall::build(ctx, &scope, &prefix, *bind, &ftype, types, top_id)?;
            self.prototypes.push(call);
        }
        Ok(())
    }
}

/// A declared impl of a trait whose methods default to the
/// representation (`Cast`) keeps only the methods its implementation
/// writes: one it leaves out is the representation. Registered here,
/// where the implementation registers, never in a module's check, which
/// runs beside its siblings'.

fn narrow_to_representation<R: Rt>(
    ctx: &mut CompileCtx<R>,
    declared: Arc<ImplDef>,
    implementation: &ImplDef,
) -> Fulfils {
    let mut methods = declared.methods.clone();
    for (name, _) in declared.methods.into_iter() {
        if !implementation.methods.contains_key(name) {
            methods.remove_cow(name);
        }
    }
    if methods.len() == declared.methods.len() {
        return Fulfils { entry: declared, narrowed: None };
    }
    let entry = Arc::new(ImplDef { methods, ..(*declared).clone() });
    ctx.env.replace_impl(&declared, entry.clone());
    Fulfils { entry, narrowed: Some(declared) }
}

impl<R: Rt> Impl<R> {
    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<'_, R>,
        buf: &mut &[u8],
    ) -> Result<Node<R>, PackError> {
        let spec = Arc::new(Expr::decode(buf)?);
        let def = Arc::<ImplDef>::decode(buf)?;
        let fulfils = Option::<Fulfils>::decode(buf)?;
        let trait_def = Arc::<TraitDef>::decode(buf)?;
        let body = decode_node(ctx, buf)?;
        let n = crate::image::count_decode(buf)?;
        let mut prototypes = Vec::with_capacity(n);
        for _ in 0..n {
            let site = decode_node(ctx, buf)?;
            let args = SmallVec::<[BindId; 2]>::decode(buf)?;
            prototypes.push(SynthCall { site, args });
        }
        Ok(Node::new(Self { spec, def, fulfils, trait_def, body, prototypes }))
    }
}

impl<R: Rt> Update<R> for Impl<R> {
    /// An instance checks this node as its definition's check did.
    fn typecheck0_instance(
        &mut self,
        ctx: &mut CompileCtx<R>,
        _types: &mut crate::node::lambda::InstanceTypes,
    ) -> Result<()> {
        self.typecheck0(ctx)
    }

    fn for_each_child<'a>(&'a self, f: &mut dyn FnMut(&'a Node<R>)) {
        f(&self.body);
        self.prototypes.iter().for_each(|p| f(&p.site))
    }

    fn for_each_child_mut(&mut self, f: &mut dyn FnMut(&mut Node<R>)) {
        f(&mut self.body);
        self.prototypes.iter_mut().for_each(|p| f(&mut p.site))
    }

    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::Impl, buf);
        self.spec.encode(buf)?;
        self.def.encode(buf)?;
        self.fulfils.encode(buf)?;
        self.trait_def.encode(buf)?;
        self.body.image_encode(buf)?;
        encode_varint(self.prototypes.len() as u64, buf);
        for p in self.prototypes.iter() {
            p.site.image_encode(buf)?;
            p.args.encode(buf)?;
        }
        Ok(())
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R>) -> &TagValue {
        self.body.update(ctx);
        TagValue::phantom_ref()
    }

    fn typecheck0(&mut self, ctx: &mut CompileCtx<R>) -> Result<()> {
        // the head's variables are the impl's declared ones: no method
        // decides them
        let _gates: LPooled<Vec<_>> =
            self.def.params.iter().map(|tv| tv.open_rigid()).collect();
        wrap!(self.body, self.body.typecheck0(ctx))?;
        // a user-annotated method must still fit the declared signature
        for d in self.trait_def.methods.iter() {
            let Some(id) = self.def.methods.get(d.name.as_str()) else { continue };
            let Some(bind) = ctx.env.by_id.get(id) else { continue };
            let expected =
                Type::Fn(Arc::new(method_sig_at(&d.typ.reset_tvars(), &self.def.target)));
            expected.check_contains(&ctx.env, &bind.typ).with_context(|| {
                format!(
                    "method {} of impl {} for {} has type {}, the trait declares {}",
                    d.name,
                    self.trait_def.name,
                    self.def.target,
                    bind.typ.resolve_tvars(),
                    expected.resolve_tvars()
                )
            })?;
            let declared = method_sig_at(&d.typ.reset_tvars(), &self.def.target);
            implements(&ctx.env, &declared, &bind.typ).with_context(|| {
                format!(
                    "method {} of impl {} for {} does not implement the declared {}",
                    d.name,
                    self.trait_def.name,
                    self.def.target,
                    declared.resolve_tvars()
                )
            })?;
        }
        Ok(())
    }

    fn typecheck1(&mut self, ctx: &mut CompileCtx<R>) -> Result<()> {
        wrap!(self.body, self.body.typecheck1(ctx))?;
        if self.prototypes.is_empty() {
            wrap!(self.body, self.build_prototypes(ctx))?;
        }
        Ok(())
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R>) {
        self.body.delete(ctx);
        for p in self.prototypes.iter_mut() {
            p.delete(ctx)
        }
        match &self.fulfils {
            None => ctx.env.unregister_impl(&self.def),
            Some(Fulfils { entry, narrowed: Some(declared) }) => {
                ctx.env.replace_impl(entry, declared.clone())
            }
            Some(Fulfils { narrowed: None, .. }) => (),
        }
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R>) {
        self.body.sleep(ctx)
    }

    fn typ(&self) -> &Type {
        Type::BOTTOM
    }

    fn view(&self) -> NodeView<'_, R> {
        NodeView::Impl(self)
    }

    fn fuse(&mut self, ctx: &mut CompileCtx<R>) -> Result<Option<Node<R>>> {
        self.body.fuse(ctx)
    }
}

impl<R: Rt> CallSite<R> {
    /// Resolve a trait method call to an implementation by the self
    /// argument's type. A self type nothing produced into settles to ⊥
    /// and the call is a standing bottom; a union self type lowers to a
    /// select.
    pub(super) fn resolve_trait_call(
        &mut self,
        ctx: &mut CompileCtx<R>,
        tm: TraitMethodRef,
    ) -> Result<Option<Node<R>>> {
        let Some(def) = ctx.env.trait_def(tm.trait_id).cloned() else {
            bailat!(self.spec, "trait method call through an unknown trait")
        };
        let m = &def.methods[tm.index];
        let Some(ftype) = self.ftype.as_ref() else { return Ok(None) };
        // Dispatch reasons per union member, so the self type must be in
        // union normal form with its cells settled first.
        let mut self_t = match m.self_index.and_then(|i| ftype.args.get(i)) {
            Some(a) => {
                let mut tvs: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
                a.typ.collect_tvars(&mut tvs);
                for (_, tv) in tvs.drain() {
                    wrap!(self, tv.settle_or_bottom(&ctx.env))?;
                }
                a.typ.resolve_tvars().normalize()
            }
            None => {
                bailat!(
                    self.spec,
                    "{}::{} called without its self argument",
                    def.name,
                    m.name
                )
            }
        };
        // a self only ⊥ reached is never produced: nothing to dispatch
        let bottom = |t: &Type| matches!(t, Type::Bottom);
        match &self_t {
            Type::Bottom => return self.lower_bottom_self(ctx),
            Type::App(head, _) if head.with_deref(|h| h.is_some_and(bottom)) => {
                return self.lower_bottom_self(ctx);
            }
            _ => (),
        }
        if !def.hole {
            while let Type::Ref(tr) = &self_t
                && ctx.env.trait_of_ref(tr).is_none()
            {
                self_t = self_t.lookup_ref(&ctx.env)?;
            }
        }
        if self_t.has_unbound() {
            if ctx.def_gate_depth > 0 {
                return Ok(None);
            }
            bailat!(
                self.spec,
                "cannot resolve {}::{}: the type of its self argument ({}) is not \
                 known at this call; annotate it",
                def.name,
                m.name,
                self_t
            )
        }
        if let Some(core) = CoreTrait::of_id(def.id).filter(|c| c.structural()) {
            return self.lower_core_call(ctx, core);
        }
        // a union self dispatches per member, as the check judged it
        let union = match &self_t {
            Type::Set(_) => true,
            Type::Primitive(p) => p.len() > 1,
            _ => false,
        };
        if union && !def.hole {
            let members = Type::dispatch_members(&ctx.env, &self_t)?;
            return self.lower_trait_union(ctx, &def, tm.index, &members);
        }
        // A constructor trait selects by the receiver's outermost form.
        if def.hole {
            self_t = match Type::app_split(&self_t, &ctx.env)? {
                Some((ctor, _)) => ctor,
                None => bailat!(
                    self.spec,
                    "cannot resolve {}::{}: {} is not a type constructor (it has no \
                     last type parameter for {} to abstract over)",
                    def.name,
                    m.name,
                    self_t,
                    def.name
                ),
            };
        }
        let Some(im) = ctx.env.find_impl(def.id, &self_t)? else {
            bailat!(self.spec, "no implementation of {} for {}", def.name, self_t)
        };
        let Some(bind) = im.methods.get(m.name.as_str()).copied().or(m.default) else {
            bailat!(
                self.spec,
                "impl {} for {} has no method {} and the trait declares no default",
                def.name,
                self_t,
                m.name
            )
        };
        self.retarget(ctx, bind);
        let fv = ctx.bind_to_lambda.get(&bind).cloned();
        if let Some(fv) = fv
            && let Some(ldef) = fv.downcast_ref::<LambdaDef<R>>()
        {
            self.resolve_static(ctx, ldef)?;
        }
        Ok(None)
    }

    /// A call whose self argument is never produced: `never(<args>)`,
    /// its arguments still live.
    fn lower_bottom_self(&mut self, ctx: &mut CompileCtx<R>) -> Result<Option<Node<R>>> {
        let (mut operands, names) = self.take_operands(None)?;
        let spec = (*self.spec).clone();
        let args = names.iter().map(|(_, n)| {
            Expr::synth(&spec, ExprKind::Ref { name: ModPath::from([n.clone()]) })
        });
        let e =
            Expr::synth(&spec, ExprKind::Never { typ: None, args: Arc::from_iter(args) });
        let scope = self.scope.clone();
        let node = lower_over_operands(
            ctx,
            self.flags,
            &scope,
            &spec,
            self.top_id,
            operands.drain(..),
            e,
        )?;
        self.lowering(ctx, node).map(Some)
    }

    /// A core trait's dispatcher is the operator it stands behind:
    /// `Eq::eq(a, b)` is `a == b`, `Display::fmt(x)` is `"[x]"`,
    /// `Ord::cmp(a, b)` tests `<` and `>`.
    fn lower_core_call(
        &mut self,
        ctx: &mut CompileCtx<R>,
        core: CoreTrait,
    ) -> Result<Option<Node<R>>> {
        let (mut operands, names) = self.take_operands(None)?;
        let spec = (*self.spec).clone();
        let mk = |kind: ExprKind| Expr::synth(&spec, kind);
        let mut positional = names
            .iter()
            .filter(|(l, _)| l.is_none())
            .map(|(_, n)| mk(ExprKind::Ref { name: ModPath::from([n.clone()]) }));
        let (Some(a), b) = (positional.next(), positional.next()) else {
            bailat!(spec, "core trait call without its self argument")
        };
        let (a, b) = (&a, b.as_ref());
        let tag = |t: &'static str| {
            mk(ExprKind::Variant { tag: ArcStr::from(t), args: Arc::from_iter([]) })
        };
        let e = match (core, b) {
            (CoreTrait::Display, _) => {
                mk(ExprKind::StringInterpolate { args: Arc::from_iter([a.clone()]) })
            }
            (CoreTrait::Eq, Some(b)) => mk(ExprKind::BinOp {
                op: crate::expr::BinOp::Eq,
                lhs: Arc::new(a.clone()),
                rhs: Arc::new(b.clone()),
            }),
            (CoreTrait::Ord, Some(b)) => {
                let lt = mk(ExprKind::BinOp {
                    op: crate::expr::BinOp::Lt,
                    lhs: Arc::new(a.clone()),
                    rhs: Arc::new(b.clone()),
                });
                let gt = mk(ExprKind::BinOp {
                    op: crate::expr::BinOp::Gt,
                    lhs: Arc::new(a.clone()),
                    rhs: Arc::new(b.clone()),
                });
                let scrutinee = mk(ExprKind::Tuple { args: Arc::from_iter([lt, gt]) });
                let arm = |l: StructurePattern, r: StructurePattern, body: Expr| {
                    (
                        Pattern {
                            type_predicate: None,
                            structure_predicate: StructurePattern::Tuple {
                                all: None,
                                binds: Arc::from_iter([l, r]),
                            },
                            guard: None,
                            pos: WrittenAt::NOWHERE,
                            end: WrittenAt::NOWHERE,
                        },
                        body,
                    )
                };
                let lit = |b: bool| StructurePattern::literal(Value::Bool(b));
                let any = || StructurePattern::Ignore;
                mk(ExprKind::Select(SelectExpr {
                    arg: Arc::new(scrutinee),
                    arms: Arc::from_iter([
                        arm(lit(true), any(), tag("Less")),
                        arm(any(), lit(true), tag("Greater")),
                        arm(any(), any(), tag("Equal")),
                    ]),
                }))
            }
            (CoreTrait::Eq | CoreTrait::Ord, None) => {
                bailat!(spec, "core trait call without its other argument")
            }
            (CoreTrait::Cast, _) => unreachable!("Cast has no operator"),
        };
        let scope = self.scope.clone();
        let node = lower_over_operands(
            ctx,
            self.flags,
            &scope,
            &spec,
            self.top_id,
            operands.drain(..),
            e,
        )?;
        self.lowering(ctx, node).map(Some)
    }

    /// Dispatch over a union self type: the call becomes
    ///
    /// ```text
    /// { let #s = <self>; let #a0 = <arg0>; ..;
    ///   select #s { M1 as #t => <impl M1>(#t, #a0, ..), M2 as #t => .. } }
    /// ```
    ///
    /// The implementation bindings are named by id (`#bind::N`).
    fn lower_trait_union(
        &mut self,
        ctx: &mut CompileCtx<R>,
        def: &TraitDef,
        index: usize,
        members: &[Type],
    ) -> Result<Option<Node<R>>> {
        let m = &def.methods[index];
        let mut targets: LPooled<Vec<(Type, BindId)>> = LPooled::take();
        for mem in members.iter() {
            let Some(im) = ctx.env.find_impl(def.id, mem)? else {
                bailat!(
                    self.spec,
                    "no implementation of {} for {mem}, a member of the self type {}",
                    def.name,
                    Type::Set(Arc::from_iter(members.iter().cloned()))
                )
            };
            let Some(bind) = im.methods.get(m.name.as_str()).copied().or(m.default)
            else {
                bailat!(self.spec, "impl {} for {mem} has no method {}", def.name, m.name)
            };
            targets.push((mem.clone(), bind));
        }
        let spec = (*self.spec).clone();
        let mk = |kind: ExprKind| Expr::synth(&spec, kind);
        let self_key = self
            .ftype
            .as_ref()
            .and_then(|ft| ArgKey::of_formals(&ft.args).nth(m.self_index?));
        let (mut operands, names) = self.take_operands(self_key.as_ref())?;
        let call_args: LPooled<Vec<(Option<ArcStr>, Expr)>> = names
            .iter()
            .map(|(label, name)| {
                let arg = if name == "#s" { literal!("#t") } else { name.clone() };
                (label.clone(), mk(ExprKind::Ref { name: ModPath::from([arg]) }))
            })
            .collect();
        let arms = targets.drain(..).map(|(mem, bind)| {
            let bind = format_compact!("{}", bind.inner());
            let f = mk(ExprKind::Ref {
                name: ModPath::from([literal!("#bind"), ArcStr::from(bind.as_str())]),
            });
            let call = mk(ExprKind::Apply(ApplyExpr {
                function: Arc::new(f),
                args: Arc::from_iter(call_args.iter().cloned()),
            }));
            let pat = Pattern {
                type_predicate: Some(mem),
                structure_predicate: StructurePattern::Bind(literal!("#t").into()),
                guard: None,
                pos: WrittenAt::NOWHERE,
                end: WrittenAt::NOWHERE,
            };
            (pat, call)
        });
        let select = mk(ExprKind::Select(SelectExpr {
            arg: Arc::new(mk(ExprKind::Ref { name: ModPath::from([literal!("#s")]) })),
            arms: Arc::from_iter(arms),
        }));
        let scope = self.scope.clone();
        let node = lower_over_operands(
            ctx,
            self.flags,
            &scope,
            &spec,
            self.top_id,
            operands.drain(..),
            select,
        )?;
        self.lowering(ctx, node).map(Some)
    }
}
