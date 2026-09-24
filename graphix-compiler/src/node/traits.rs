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
    coretraits::{CoreTrait, method_ftype},
    genn::SynthCall,
    lambda::LambdaDef,
};
use crate::{
    BindId, CFlag, Event, ExecCtx, Node, NodeView, Refs, Rt, Scope, SourcePosition,
    TagValue, Update, UserEvent, bailat,
    env::{Env, ImplDef, Map, TraitDef, TraitMethodRef},
    expr::{
        ApplyExpr, Arg, At, Attr, BindExpr, Decorations, Expr, ExprId, ExprKind,
        ImplExpr, LambdaExpr, ModPath, Origin, Pattern, SelectExpr, StructurePattern,
        TraitExpr,
    },
    image::{
        ImageBuf,
        nodes::{NodeTag, decode_node, put_tag, tag_len},
    },
    typ::{FnArgKind, FnType, TVar, Type, TypeRef},
    wrap,
};
use ahash::{AHashMap, AHashSet};
use anyhow::{Context, Result, anyhow, bail};
use arcstr::{ArcStr, literal};
use compact_str::{CompactString, format_compact};
use enumflags2::BitFlags;
use netidx_core::pack::{Pack, PackError, decode_varint, encode_varint, varint_len};
use netidx_value::Value;
use poolshark::local::LPooled;
use smallvec::SmallVec;
use triomphe::Arc;

/// The declared signature of a trait method, scoped to the declaring
/// module, with `self` a trait-bounded quantifier (rigid while a
/// default body is checked).
pub(crate) fn method_sig(parsed: &FnType, tref: &Type, scope: &ModPath) -> FnType {
    let ft = parsed.scope_refs(scope);
    let mut known: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
    ft.alias_tvars(&mut known);
    if let Some(tv) = known.get("self") {
        tv.add_cell_constraint(tref.clone());
    }
    let mut quantifiers: LPooled<Vec<ArcStr>> = ft.quantifiers.iter().cloned().collect();
    if !quantifiers.iter().any(|q| &**q == "self") {
        quantifiers.push(literal!("self"));
    }
    FnType { quantifiers: Arc::from_iter(quantifiers.drain(..)), ..ft }
}

/// The method signature instantiated at an implementation target:
/// `self := target`, everything else fresh.
pub(crate) fn method_sig_at(sig: &FnType, target: &Type) -> FnType {
    let mut known: LPooled<AHashMap<ArcStr, Type>> = LPooled::take();
    known.insert(literal!("self"), target.clone());
    sig.replace_tvars(&known)
}

/// Fill a method lambda's missing parameter and return annotations
/// from the declared signature (positional by position, labeled by
/// name). Written annotations are kept.
fn annotate_lambda(value: &Expr, sig: &FnType) -> Expr {
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
            sigs.push((
                m.name.name.clone(),
                Arc::new(ft),
                m.self_index,
                m.default.is_some(),
            ));
        }
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
                let mut e = body.clone();
                e.id = ExprId::new();
                e.kind = ExprKind::Bind(Arc::new(b));
                e.dec = None;
                exprs.push(e);
            }
        }
        let exprs: Arc<[Expr]> = Arc::from_iter(exprs.drain(..));
        let defaults =
            Block::compile(ctx, flags, spec.clone(), &dscope, top_id, true, &exprs)
                .with_context(|| format!("in the default methods of trait {}", t.name))?;
        let mut defaults_by_name: LPooled<Vec<(CompactString, BindId)>> = LPooled::take();
        if let Some(binds) = ctx.env.binds.get(&dscope.lexical) {
            for (n, id) in binds.into_iter() {
                defaults_by_name.push((n.clone(), *id));
            }
        }
        let def = ctx.env.set_trait_defaults(def.id, defaults_by_name.drain(..));
        Ok(Node::new(Self { spec, def, defaults }))
    }
}

impl<R: Rt, E: UserEvent> Trait<R, E> {
    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<R, E>,
        buf: &mut &[u8],
    ) -> Result<Node<R, E>, PackError> {
        let spec = Expr::decode(buf)?;
        let def = Arc::<TraitDef>::decode(buf)?;
        let defaults = decode_node(ctx, buf)?;
        Ok(Node::new(Self { spec, def, defaults }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Trait<R, E> {
    fn image_len(&self) -> usize {
        tag_len()
            + self.spec.encoded_len()
            + self.def.encoded_len()
            + self.defaults.image_len()
    }

    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        put_tag(NodeTag::Trait, buf);
        self.spec.encode(buf)?;
        self.def.encode(buf)?;
        self.defaults.image_encode(buf)
    }

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

    /// A declaration has nothing of its own to walk: every view walker
    /// treats a trait exactly as its defaults' module block.
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
    /// The interface declaration (`impl T for X;`) this implementation
    /// fulfils, when there is one; the declaration is the registered
    /// impl and `def`'s methods proxy to its bindings.
    pub(crate) fulfils: Option<Arc<ImplDef>>,
    trait_def: Arc<TraitDef>,
    pub(crate) body: Node<R, E>,
    /// For a core trait, one never-run call site per method so the
    /// analysis reaches the method's body and verifies its implicit
    /// `#[sync]`.
    pub(crate) prototypes: Vec<SynthCall<R, E>>,
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
    let mut known: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
    let params: Arc<[TVar]> = Arc::from_iter(im.params.iter().map(|tv| {
        let tv = TVar::empty_named(tv.name.clone());
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
    for (tv, tc) in im.constraints.iter() {
        in_target.clear();
        tc.collect_tvars(&mut in_target);
        for (name, _) in in_target.iter() {
            if !known.contains_key(name) {
                bail!("undeclared type variable '{name} in the constraint on {tv}")
            }
        }
    }
    target.alias_tvars(&mut known);
    for (tv, tc) in im.constraints.iter() {
        let tc = tc.scope_refs(scope);
        tc.alias_tvars(&mut known);
        known[&tv.name].add_cell_constraint(tc);
    }
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

impl<R: Rt, E: UserEvent> Impl<R, E> {
    pub(crate) fn compile(
        ctx: &mut ExecCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: Expr,
        scope: &Scope,
        im: &ImplExpr,
        top_id: ExprId,
    ) -> Result<Node<R, E>> {
        let trait_id =
            match ctx.env.lookup_trait(&scope.lexical, &im.trait_name).at(&spec)? {
                Some(id) => id,
                None => bailat!(spec, "no trait `{}` in scope", im.trait_name),
            };
        let trait_def = ctx.env.trait_def(trait_id).cloned().ok_or_else(|| {
            anyhow!("trait {} has no definition", im.trait_name).at(&spec)
        })?;
        let (target, params) =
            impl_head(&ctx.env, &scope.lexical, &trait_def, im, false).at(&spec)?;
        let bscope = scope.append_block("impl", spec.id.inner());
        ctx.env.import_glob(&bscope.lexical, trait_def.path.clone());
        let core = CoreTrait::of_id(trait_id).is_some();
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
            // a core trait's method runs inside a comparison or print
            let dec = match (core, &m.dec) {
                (false, dec) => dec.clone(),
                (true, dec) => {
                    let sync = Attr { name: literal!("sync"), args: Arc::from_iter([]) };
                    let (comments, attrs) = match dec {
                        Some(d) => (d.comments.clone(), d.attrs.clone()),
                        None => (Arc::from_iter([]), Arc::from_iter([])),
                    };
                    Some(Box::new(Decorations {
                        comments,
                        attrs: Arc::from_iter(attrs.iter().cloned().chain([sync])),
                    }))
                }
            };
            let mut e = m.clone();
            e.kind = ExprKind::Bind(Arc::new(b));
            e.dec = dec;
            exprs.push(e);
        }
        for d in trait_def.methods.iter() {
            if !provided.contains(&d.name) && d.default.is_none() {
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
        let fulfils = ctx.env.register_impl(def.clone()).at(&spec)?;
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

impl<R: Rt, E: UserEvent> Impl<R, E> {
    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<R, E>,
        buf: &mut &[u8],
    ) -> Result<Node<R, E>, PackError> {
        let spec = Expr::decode(buf)?;
        let def = Arc::<ImplDef>::decode(buf)?;
        let fulfils = Option::<Arc<ImplDef>>::decode(buf)?;
        let trait_def = Arc::<TraitDef>::decode(buf)?;
        let body = decode_node(ctx, buf)?;
        let n = decode_varint(buf)? as usize;
        let mut prototypes = Vec::with_capacity(n);
        for _ in 0..n {
            let site = decode_node(ctx, buf)?;
            let args = SmallVec::<[BindId; 2]>::decode(buf)?;
            prototypes.push(SynthCall { site, args });
        }
        Ok(Node::new(Self { spec, def, fulfils, trait_def, body, prototypes }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for Impl<R, E> {
    fn image_len(&self) -> usize {
        tag_len()
            + self.spec.encoded_len()
            + self.def.encoded_len()
            + self.fulfils.encoded_len()
            + self.trait_def.encoded_len()
            + self.body.image_len()
            + varint_len(self.prototypes.len() as u64)
            + self
                .prototypes
                .iter()
                .map(|p| p.site.image_len() + p.args.encoded_len())
                .sum::<usize>()
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
            p.site.refs(refs)
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

impl<R: Rt, E: UserEvent> CallSite<R, E> {
    /// Resolve a trait method call to an implementation by the self
    /// argument's type. An open self type is an error outside a
    /// definition gate; a union self type lowers to a select.
    pub(super) fn resolve_trait_call(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        tm: TraitMethodRef,
    ) -> Result<()> {
        let Some(def) = ctx.env.trait_def(tm.trait_id).cloned() else {
            bailat!(self.spec, "trait method call through an unknown trait")
        };
        let m = &def.methods[tm.index];
        let Some(ftype) = self.ftype.as_ref() else { return Ok(()) };
        // Dispatch reasons per union member, so the self type must be in
        // union normal form with its cells settled first.
        let mut self_t = match ftype.args.get(m.self_index) {
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
        if !def.hole {
            while let Type::Ref(tr) = &self_t
                && ctx.env.trait_of_ref(tr).is_none()
            {
                self_t = self_t.lookup_ref(&ctx.env)?;
            }
        }
        if self_t.has_unbound() {
            if ctx.def_gate_depth > 0 {
                return Ok(());
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
        if let Some(core) = CoreTrait::of_id(def.id) {
            return self.lower_core_call(ctx, core);
        }
        if let Type::Set(members) = &self_t
            && !def.hole
        {
            let members = members.clone();
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
        let fv =
            ctx.bind_to_lambda.get(&bind).cloned().or_else(|| ctx.rt.store_value(&bind));
        if let Some(fv) = fv
            && let Some(ldef) = fv.downcast_ref::<LambdaDef<R, E>>()
        {
            self.resolve_static(ctx, ldef)?;
        }
        Ok(())
    }

    /// A core trait's dispatcher is the operator it stands behind:
    /// `Eq::eq(a, b)` is `a == b`, `Display::fmt(x)` is `"[x]"`,
    /// `Ord::cmp(a, b)` tests `<` and `>`.
    fn lower_core_call(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        core: CoreTrait,
    ) -> Result<()> {
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
            (CoreTrait::Eq, Some(b)) => {
                mk(ExprKind::Eq { lhs: Arc::new(a.clone()), rhs: Arc::new(b.clone()) })
            }
            (CoreTrait::Ord, Some(b)) => {
                let lt = mk(ExprKind::Lt {
                    lhs: Arc::new(a.clone()),
                    rhs: Arc::new(b.clone()),
                });
                let gt = mk(ExprKind::Gt {
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
                        },
                        body,
                    )
                };
                let lit = |b: bool| StructurePattern::Literal(Value::Bool(b));
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
        self.install_lowered(ctx, node)
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
        ctx: &mut ExecCtx<R, E>,
        def: &TraitDef,
        index: usize,
        members: &[Type],
    ) -> Result<()> {
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
            .and_then(|ft| ArgKey::of_formals(&ft.args).nth(m.self_index));
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
        self.install_lowered(ctx, node)
    }
}
