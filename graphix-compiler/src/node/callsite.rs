use super::{
    NOP, Nop, WakeBit,
    bind::Ref,
    compiler::compile,
    error::{Qop, join_open_raised, join_raised},
    lambda::{
        BuiltInLambda, GXLambda, InstanceTypes, Lambda, LambdaDef, same_parameters,
    },
    pattern::StructPatternNode,
    wake::TrackedFires,
};
use crate::{
    Apply, ApplyView, BindId, BindMode, CFlag, CompileCtx, ExecCtx, FnArgIdentity,
    LambdaId, LambdaInstanceId, Node, NodeView, PrintFlag, Refs, ResolvingLambda, Rt,
    Scope, Tag, TagValue, Update, UserEvent, View, analysis, bailat, dbgenv, deref_typ,
    env::Env,
    expr::{ApplyExpr, At, Expr, ExprId, ExprKind},
    format_with_flags,
    fusion::{
        self,
        emit::{
            BodyCx, Called, CompiledExpr, emit_builtin_call_node, emit_lambda_call_node,
        },
        lowering::MarshalArg,
        share::{self, SlotShare},
    },
    image::{
        self, ImageBuf,
        nodes::{
            NodeTag, decode_node, decode_nodes, encode_nodes, opt_node_decode,
            opt_node_encode, put_tag,
        },
    },
    perfdbg,
    profile::{self, Phase},
    stack::ensure_sufficient,
    typ::{
        ContainsFlags, FnArgKind, FnArgType, FnType, TVar, Type,
        tvar::{Level, RigidGate},
    },
    wrap,
};
use crate::{
    branch::timed,
    cost::{ForkSite, Meter},
};
use ahash::{AHashMap, AHashSet};
use anyhow::{Context, Result, anyhow, bail};
use arcstr::{ArcStr, literal};
use bytes::{Buf, BufMut};
use compact_str::format_compact;
use enumflags2::BitFlags;
use indexmap::{IndexMap, map::Entry as ArgEntry};
use log::warn;
use netidx_core::pack::{Pack, PackError, encode_varint};
use netidx_value::Value;
use nohash::IntSet;
use poolshark::local::LPooled;
use smallvec::SmallVec;
use std::{
    fmt, mem,
    sync::{
        OnceLock,
        atomic::{AtomicBool, Ordering::Relaxed},
    },
};
use triomphe::Arc as TArc;

/// Reject a direct call to a sync variadic builtin with no positional
/// arguments (`str::concat()`, `sum()`): the node has no data inputs and
/// can never fire. Only a direct `Ref` to the builtin is checkable.
fn reject_dead_variadic_call<R: Rt, E: UserEvent>(
    ctx: &CompileCtx<R, E>,
    scope: &Scope,
    f: &Expr,
    args: &TArc<[(Option<ArcStr>, Expr)]>,
) -> Result<()> {
    let path = match &f.kind {
        ExprKind::Ref { name } => name,
        _ => return Ok(()),
    };
    if args.iter().any(|(label, _)| label.is_none()) {
        return Ok(());
    }
    let Some((_, bind)) = ctx.env.lookup_bind(&scope.lexical, path).ok().flatten() else {
        return Ok(());
    };
    let key = (bind.scope.clone(), bind.name.clone());
    let Some(info) = ctx.builtin_bindings.get(&key) else {
        return Ok(());
    };
    if info.typ.vargs.is_none()
        || info.typ.args.iter().any(|a| a.is_positional())
        || !ctx.builtin_effect(info.name.as_str()).kind().is_sync()
    {
        return Ok(());
    }
    bail!(
        "calling `{path}` with no positional arguments can never produce \
         a value: a sync variadic builtin with no data inputs never fires. \
         Pass it at least one argument, or use never() to express a value \
         that never arrives"
    )
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, netidx_derive::Pack)]
pub(crate) enum ArgKey {
    Positional(usize),
    Named(ArcStr),
}

impl ArgKey {
    fn keyed<'a>(
        labels: impl Iterator<Item = Option<&'a ArcStr>>,
    ) -> impl Iterator<Item = ArgKey> {
        let mut positional = 0;
        labels.map(move |label| match label {
            Some(name) => ArgKey::Named(name.clone()),
            None => {
                positional += 1;
                ArgKey::Positional(positional - 1)
            }
        })
    }

    /// Each formal's key in signature order: a labeled parameter by
    /// name, the k-th positional one as `Positional(k)`.
    pub(crate) fn of_formals(args: &[FnArgType]) -> impl Iterator<Item = ArgKey> + '_ {
        Self::keyed(args.iter().map(|a| a.label()))
    }

    /// Each written argument's key in source order.
    pub(crate) fn of_written(
        args: &[(Option<ArcStr>, Expr)],
    ) -> impl Iterator<Item = ArgKey> + '_ {
        Self::keyed(args.iter().map(|(label, _)| label.as_ref()))
    }

    /// The variadic keys of a signature with `positional` positional
    /// formals.
    fn variadic(positional: usize) -> impl Iterator<Item = ArgKey> {
        (positional..).map(ArgKey::Positional)
    }
}

impl fmt::Display for ArgKey {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ArgKey::Positional(k) => write!(f, "positional argument {k}"),
            ArgKey::Named(name) => write!(f, "#{name}"),
        }
    }
}

/// What an instance registers for its body's check: its fn-typed formals
/// bound to the lambdas its arguments are, and keyed by definition and
/// position (`CompileCtx::instance_fn_params`), with what each key held.
#[derive(Default)]
struct FnParams {
    binds: LPooled<Vec<BindId>>,
    formals: SmallVec<[((LambdaId, u32), Option<BindId>); 4]>,
    args: SmallVec<[BindId; 4]>,
}

/// The call's argument nodes, keyed for signature lookups and iterating
/// in source order: the written arguments, then the defaults a bind
/// added. A forked update merges its parts in that order.
pub(crate) type ArgMap<R, E> = IndexMap<ArgKey, Arg<R, E>, ahash::RandomState>;

#[derive(Debug)]
pub(crate) struct Arg<R: Rt, E: UserEvent> {
    pub id: BindId,
    pub node: Option<Node<R, E>>,
    pub stage: ArgStage,
    /// The argument's expression, which a reference to it shows.
    pub spec: TArc<Expr>,
}

impl<R: Rt, E: UserEvent> Arg<R, E> {
    pub(crate) fn new(
        id: BindId,
        node: Option<Node<R, E>>,
        stage: ArgStage,
        spec: TArc<Expr>,
    ) -> Self {
        Arg { id, node, stage, spec }
    }
}

/// Where an argument came from and, for a default the call omits, how
/// far the bind has taken it.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ArgStage {
    /// Written at the call.
    Given,
    /// An omitted default's placeholder before any bind (`fill_omitted`).
    Placeholder,
    /// A default a bind compiled and has not run (`prepare_bind`).
    Compiled,
    /// A default every update runs: a static bind's from the first
    /// update, another bind's once `prime_bound` ran it.
    Running,
}

impl ArgStage {
    pub(crate) fn is_default(self) -> bool {
        self != ArgStage::Given
    }

    /// Whether an update runs the argument.
    fn runs(self) -> bool {
        matches!(self, ArgStage::Given | ArgStage::Running)
    }
}

/// A labeled default of `f` compiled and checked in `f`'s environment,
/// where its names are, against the site's view of the label, which the
/// default narrows and the instance runs with; where the site hides the
/// label, against `f`'s own parameter.
#[allow(clippy::too_many_arguments)]
fn checked_default<R: Rt, E: UserEvent>(
    ctx: &mut CompileCtx<R, E>,
    flags: BitFlags<CFlag>,
    scope: &Scope,
    top_id: ExprId,
    f: &LambdaDef<R, E>,
    name: &ArcStr,
    expr: &Expr,
    defaults: &mut Refs,
    site: Option<&FnType>,
) -> Result<Node<R, E>> {
    let mut gates: LPooled<Vec<RigidGate>> = LPooled::take();
    let (node, res) = ctx.with_restored(f.env.clone(), |ctx| {
        let local_scope =
            Scope { dynamic: scope.dynamic.clone(), lexical: f.scope.lexical.clone() };
        let mut node = compile(ctx, flags, expr.clone(), &local_scope, top_id)?;

        let res = node.typecheck0(ctx).and_then(|()| {
            match site.and_then(|ft| ft.args.iter().find(|a| a.label() == Some(name))) {
                Some(a) => a.typ.check_contains(&ctx.env, node.typ()),
                None => match callee_param(ctx, f, name, site, &mut gates) {
                    Some(t) => t.check_contains(&ctx.env, node.typ()),
                    None => Ok(()),
                },
            }
        });
        Ok::<_, anyhow::Error>((node, res))
    })?;
    node.refs(defaults);
    match wrap!(node, res) {
        Ok(()) => Ok(node),
        Err(e) => {
            ctx.discard(node);
            Err(e)
        }
    }
}

/// The labeled defaults of the lambdas `value` may hold that `formal`
/// hides or makes optional, each judged at `formal`'s view.
fn judge_hidden_defaults<R: Rt, E: UserEvent>(
    ctx: &mut CompileCtx<R, E>,
    flags: BitFlags<CFlag>,
    scope: &Scope,
    top_id: ExprId,
    formal: &FnType,
    value: &FnType,
) -> Result<()> {
    for id in value.lambda_ids.ids().iter() {
        let def = ctx.lambda_defs.get(id).cloned();
        let Some(f) = def.as_ref().and_then(|v| v.downcast_ref::<LambdaDef<R, E>>())
        else {
            continue;
        };
        for (a, spec) in f.typ.args.iter().zip(f.argspec.iter()) {
            let (Some(name), Some(expr)) = (a.label(), spec.kind.default()) else {
                continue;
            };
            let required = formal.args.iter().find(|fa| fa.label() == Some(name));
            if required.is_some_and(|fa| !fa.kind.has_default()) {
                continue;
            }
            let node = checked_default(
                ctx,
                flags,
                scope,
                top_id,
                f,
                name,
                expr,
                &mut Refs::default(),
                Some(formal),
            )?;
            ctx.discard(node);
        }
    }
    Ok(())
}

/// Whose cells a hidden default's judgment may narrow.
#[derive(Clone, Copy, PartialEq, Eq)]
pub(crate) enum Narrowing {
    /// A call's formals: the site's own instantiation, which a default
    /// narrows.
    Site,
    /// A declaration's type (an annotation, a `<-` target, a return
    /// type): judged on a copy, so it binds nothing of the declaration.
    Copy,
}

/// [`judge_hidden_defaults`] wherever `declared` and `value` both give a
/// function at one place: a union's one function arm, a struct field, a
/// tuple, variant, array, list, map or reference part, through typedefs.
/// What a call's formal, a `let`'s annotation, a `<-` target and a
/// declared return type hold.
pub(crate) fn check_hidden_defaults<R: Rt, E: UserEvent>(
    ctx: &mut CompileCtx<R, E>,
    flags: BitFlags<CFlag>,
    scope: &Scope,
    top_id: ExprId,
    declared: &Type,
    value: &Type,
    narrowing: Narrowing,
) -> Result<()> {
    if !may_hold_fn(declared, &mut LPooled::take()) {
        return Ok(());
    }
    match narrowing {
        Narrowing::Site => {
            hidden_defaults_at(ctx, flags, scope, top_id, declared, value, 0)
        }
        Narrowing::Copy => {
            let declared = declared.reset_tvars();
            hidden_defaults_at(ctx, flags, scope, top_id, &declared, value, 0)
        }
    }
}

/// Whether a function may stand somewhere in `t`, through bindings; a
/// typedef may expand to one. Each shared node is asked once (`seen`).
fn may_hold_fn(t: &Type, seen: &mut AHashSet<usize>) -> bool {
    ensure_sufficient(|| match t {
        _ if t.shared_identity().is_some_and(|n| !seen.insert(n)) => false,
        Type::Fn(_) | Type::Ref(_) => true,
        Type::TVar(tv) => tv.binding().is_some_and(|b| may_hold_fn(&b, seen)),
        t => {
            let mut any = false;
            t.for_each_child(&mut |c| any = any || may_hold_fn(c, seen));
            any
        }
    })
}

fn hidden_defaults_at<R: Rt, E: UserEvent>(
    ctx: &mut CompileCtx<R, E>,
    flags: BitFlags<CFlag>,
    scope: &Scope,
    top_id: ExprId,
    declared: &Type,
    value: &Type,
    expanded: u8,
) -> Result<()> {
    // typedefs expand this deep at most, so a recursive one ends
    const MAX_EXPANSIONS: u8 = 16;
    let (Some(d), Some(v)) = (declared.deref_cloned(), value.deref_cloned()) else {
        return Ok(());
    };
    let mut expanded = expanded;
    let mut open = |t: Type| match t {
        Type::Ref(_) if expanded < MAX_EXPANSIONS => {
            expanded += 1;
            t.lookup_ref(&ctx.env).ok()
        }
        Type::Ref(_) => None,
        t => Some(t),
    };
    let (Some(d), Some(v)) = (open(d), open(v)) else { return Ok(()) };
    let at = |ctx: &mut CompileCtx<R, E>, d: &Type, v: &Type| {
        hidden_defaults_at(ctx, flags, scope, top_id, d, v, expanded)
    };
    ensure_sufficient(|| match (&d, &v) {
        (Type::Fn(formal), Type::Fn(fv)) => {
            judge_hidden_defaults(ctx, flags, scope, top_id, formal, fv)
        }
        (Type::Set(_), _) | (_, Type::Set(_)) => {
            let mut formals: LPooled<Vec<TArc<FnType>>> = LPooled::take();
            collect_fn_arms(&d, &mut formals);
            let mut values: LPooled<Vec<TArc<FnType>>> = LPooled::take();
            collect_fn_arms(&v, &mut values);
            match &formals[..] {
                [formal] => values.iter().try_for_each(|fv| {
                    judge_hidden_defaults(ctx, flags, scope, top_id, formal, fv)
                }),
                _ => Ok(()),
            }
        }
        (Type::Struct(df), Type::Struct(vf)) => df.iter().try_for_each(|(n, dt, _)| {
            match vf.iter().find(|(m, _, _)| m == n) {
                Some((_, vt, _)) => at(ctx, dt, vt),
                None => Ok(()),
            }
        }),
        (Type::Tuple(a), Type::Tuple(b))
        | (Type::Variant(_, a, _), Type::Variant(_, b, _))
            if a.len() == b.len() =>
        {
            a.iter().zip(b.iter()).try_for_each(|(d, v)| at(ctx, d, v))
        }
        (Type::Array(d), Type::Array(v))
        | (Type::List(d), Type::List(v))
        | (Type::Error(d), Type::Error(v))
        | (Type::ByRef(_, d), Type::ByRef(_, v)) => at(ctx, d, v),
        (Type::Map { key: dk, value: dv }, Type::Map { key: vk, value: vv }) => {
            at(ctx, dk, vk)?;
            at(ctx, dv, vv)
        }
        _ => Ok(()),
    })
}

/// Collect every `Type::Fn` arm reachable in `t`: a bare `Fn`, or the
/// `Fn` arms of a `[fn(...), null]` / Set union.
fn collect_fn_arms(t: &Type, out: &mut LPooled<Vec<TArc<FnType>>>) {
    match t {
        Type::Fn(ft) => out.push(ft.clone()),
        Type::Set(ts) => {
            for arm in ts.iter() {
                collect_fn_arms(arm, out)
            }
        }
        _ => (),
    }
}

/// `t` printed through its cells as they stand: a snapshot that a later
/// bind does not reach.
fn printed_deref(t: &Type) -> String {
    crate::format_with_flags(crate::PrintFlag::DerefTVars, || t.to_string())
}

fn compile_apply_args<R: Rt, E: UserEvent>(
    ctx: &mut CompileCtx<R, E>,
    flags: BitFlags<CFlag>,
    scope: &Scope,
    top_id: ExprId,
    spec: &Expr,
    args: &TArc<[(Option<ArcStr>, Expr)]>,
) -> Result<ArgMap<R, E>> {
    let mut res = ArgMap::default();
    for ((_, expr), key) in args.iter().zip(ArgKey::of_written(args)) {
        let arg_spec = ctx.shared_spec(expr.clone());
        let node = Some(compile(ctx, flags, expr.clone(), scope, top_id)?);
        match res.entry(key) {
            ArgEntry::Occupied(e) => bailat!(spec, "duplicate argument {}", e.key()),
            ArgEntry::Vacant(e) => {
                e.insert(Arg::new(BindId::new(), node, ArgStage::Given, arg_spec));
            }
        }
    }
    Ok(res)
}

/// A formal of quantified function type `fn<'b: C>(..)`, its aliases expanded,
/// with its quantifiers held rigid for the argument's check: the argument
/// must be well typed for every 'b the bound admits, since the callee may
/// call it at any. `None` for any other formal.
fn quantified_formal(
    env: &Env,
    typ: &Type,
) -> Result<Option<(Type, LPooled<Vec<RigidGate>>, SmallVec<[TVar; 2]>)>> {
    let mut expanded = typ.with_deref(|t| t.cloned());
    while let Some(t @ Type::Ref(_)) = &expanded {
        expanded = t.lookup_ref_with(env, false)?;
    }
    let Some(formal @ Type::Fn(ft)) = &expanded else { return Ok(None) };
    if ft.quantifiers.is_empty() {
        return Ok(None);
    }
    let mut named: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
    ft.collect_tvars(&mut named);
    let quantifiers: SmallVec<[TVar; 2]> = named
        .iter()
        .filter(|(name, _)| ft.quantifiers.contains(*name))
        .map(|(_, tv)| tv.clone())
        .collect();
    let gates = quantifiers.iter().map(|tv| tv.open_quantifier_rigid()).collect();
    Ok(Some((formal.clone(), gates, quantifiers)))
}

/// A rank-2 quantifier `q` keeps exactly the bounds its formal `declared`:
/// the callee may call the argument at any type they admit, so the
/// argument's check may add no conjunct, a predicate included, that none
/// of them implies.
fn added_bound(env: &Env, q: &TVar, declared: &[Type]) -> Result<()> {
    for c in q.cell_constraints() {
        let mut implied = false;
        for d in declared {
            if d.implies(env, &c)? {
                implied = true;
                break;
            }
        }
        if !implied {
            format_with_flags(PrintFlag::DerefTVars, || {
                bail!(
                    "the argument requires '{}: {c}, which the parameter's quantifier \
                     does not declare: the callee may call it at any type its bound admits",
                    q.name
                )
            })?
        }
    }
    Ok(())
}

/// Whether a value of `t` can carry a value of a cell its formal holds:
/// a function may be called with one, an unknown part may be one.
fn carries(env: &Env, t: &Type) -> bool {
    t.holds_fn(env) || t.holds_unknown(env)
}

/// Fit a site's arguments to its signature: a placeholder for each
/// defaulted label it omits; a missing or unknown label, or a wrong
/// positional count, is refused.
fn fill_omitted<R: Rt, E: UserEvent>(
    args: &mut ArgMap<R, E>,
    ftype: &FnType,
) -> Result<()> {
    for arg in ftype.args.iter() {
        if let FnArgKind::Labeled { name, has_default } = &arg.kind {
            match args.entry(ArgKey::Named(name.clone())) {
                ArgEntry::Occupied(_) => (),
                ArgEntry::Vacant(e) if *has_default => {
                    let nop = Nop::new(arg.typ.clone());
                    e.insert(Arg::new(
                        BindId::new(),
                        Some(nop),
                        ArgStage::Placeholder,
                        NOP.clone(),
                    ));
                }
                ArgEntry::Vacant(_) => bail!("missing required argument {name}"),
            }
        }
    }
    for key in args.keys() {
        if let ArgKey::Named(name) = key
            && !ftype.args.iter().any(|a| a.label() == Some(name))
        {
            bail!("unknown labeled argument {name}")
        }
    }
    let required = ftype.args.iter().filter(|a| a.is_positional()).count();
    let provided = args.keys().filter(|k| matches!(k, ArgKey::Positional(_))).count();
    if provided < required {
        bail!(
            "missing required argument: expected {required} positional, received {provided}"
        )
    }
    if provided > required && ftype.vargs.is_none() {
        bail!("too many positional arguments, expected {required}, received {provided}")
    }
    Ok(())
}

/// How a call's argument met its formal.
enum ArgFit {
    Fits,
    /// It does not fit the formal as it stands.
    Misfit,
    /// It holds a cell nothing has decided, and the formal asked it to
    /// wait: checked now it would take what the formal holds so far.
    Open,
}

/// Check a call's argument node against its formal's type `typ`; an
/// argument holding an undecided cell is [`ArgFit::Open`] when `waits`.
fn typecheck_arg<R: Rt, E: UserEvent>(
    ctx: &mut CompileCtx<R, E>,
    typ: &Type,
    hint: Option<&Type>,
    waits: bool,
    n: &mut Node<R, E>,
) -> Result<ArgFit> {
    // A reference instantiates its signature in its own typecheck0,
    // which must precede the pre-unify.
    if matches!(n.view(), NodeView::Ref(_)) {
        wrap!(n, n.typecheck0(ctx))?;
    }
    match quantified_formal(&ctx.env, typ)? {
        None => {
            Type::pre_unify_arg(&ctx.env, hint.unwrap_or(typ), n.typ())?;
            wrap!(n, n.typecheck0(ctx))?;
            Ok(if waits && n.typ().has_free() {
                ArgFit::Open
            } else if typ.contains(&ctx.env, n.typ())? {
                ArgFit::Fits
            } else {
                ArgFit::Misfit
            })
        }
        Some((formal, _rigid, quantifiers)) => {
            let declared: SmallVec<[SmallVec<[Type; 1]>; 2]> =
                quantifiers.iter().map(|q| q.cell_constraints()).collect();
            Type::pre_unify_arg(&ctx.env, &formal, n.typ())?;
            wrap!(n, n.typecheck0(ctx))?;
            for (q, declared) in quantifiers.iter().zip(declared.iter()) {
                wrap!(n, added_bound(&ctx.env, q, declared))?;
            }
            wrap!(n, formal.check_contains_rigid(&ctx.env, &n.typ()))?;
            wrap!(n, polymorphic_in(&formal, n.typ()))?;
            let mut refs = Refs::default();
            n.refs(&mut refs);
            let mut free: SmallVec<[Type; 4]> = SmallVec::new();
            refs.with_external_refs(|id| {
                if let Some(b) = ctx.env.by_id.get(&id) {
                    free.push(b.typ.clone())
                }
            });
            for t in free.iter() {
                wrap!(n, escapes_through(&formal, t))?;
            }
            Ok(ArgFit::Fits)
        }
    }
}

/// `f`'s parameter `name` as a call through `site` instantiates it: a
/// fresh instance of `f`'s signature whose other parameters take the
/// arguments of a copy of the site's, so a variable the site fixes
/// through another parameter fixes this one.
fn callee_param<R: Rt, E: UserEvent>(
    ctx: &CompileCtx<R, E>,
    f: &LambdaDef<R, E>,
    name: &ArcStr,
    site: Option<&FnType>,
    gates: &mut LPooled<Vec<RigidGate>>,
) -> Option<Type> {
    let inst = f.typ.instantiate(&ctx.rec_defs);
    if let Some(site) = site {
        // each argument the site passes fits the callee's parameter for it;
        // a rank-2 site's quantifiers stand for any type, so they are held
        // rigid while `gates` lives
        let site = site.replace_tvars(&Default::default());
        let mut named: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
        site.collect_tvars(&mut named);
        gates.extend(
            named
                .iter()
                .filter(|(n, _)| site.quantifiers.contains(*n))
                .map(|(_, tv)| tv.open_quantifier_rigid()),
        );
        let keys =
            |args: &[FnArgType]| ArgKey::of_formals(args).collect::<SmallVec<[_; 4]>>();
        let (site_keys, inst_keys) = (keys(&site.args), keys(&inst.args));
        for (sa, k) in site.args.iter().zip(site_keys.iter()) {
            if sa.label() == Some(name) {
                continue;
            }
            if let Some(i) = inst_keys.iter().position(|ik| ik == k) {
                let _ = inst.args[i].typ.contains(&ctx.env, &sa.typ);
            }
        }
    }
    inst.args.iter().find(|a| a.label() == Some(name)).map(|a| a.typ.clone())
}

/// An argument checked against a formal with quantifiers of its own
/// holds for every choice of them only if none was aliased to a cell the
/// argument does not own: a top-level variable or an enclosing
/// definition's, which would carry the quantifier out.
fn polymorphic_in(formal: &Type, arg: &Type) -> Result<()> {
    let Type::Fn(ft) = formal else { return Ok(()) };
    let mut named: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
    ft.collect_tvars(&mut named);
    let quantified: SmallVec<[(ArcStr, usize); 2]> = named
        .iter()
        .filter(|(name, _)| ft.quantifiers.contains(*name))
        .map(|(name, tv)| (name.clone(), tv.cell_addr()))
        .collect();
    if let Some((q, tv)) = carried_out(&quantified, arg) {
        bail!(
            "the argument is not polymorphic in '{q}: its type holds {tv}, a variable \
             of its environment, which would carry '{q} out"
        )
    }
    Ok(())
}

/// The cells of the quantifiers a function type under `t` binds.
pub(crate) fn bound_quantifiers(
    t: &Type,
    out: &mut AHashSet<usize>,
    seen: &mut AHashSet<usize>,
) {
    ensure_sufficient(|| match t {
        Type::TVar(tv) => {
            if seen.insert(tv.cell_addr())
                && let Some(b) = tv.binding()
            {
                bound_quantifiers(&b, out, seen)
            }
        }
        Type::Fn(ft) => {
            if !ft.quantifiers.is_empty() {
                let mut named: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
                ft.collect_tvars(&mut named);
                out.extend(
                    named
                        .iter()
                        .filter(|(n, _)| ft.quantifiers.contains(*n))
                        .map(|(_, tv)| tv.cell_addr()),
                );
            }
            t.for_each_child(&mut |c| bound_quantifiers(c, out, seen))
        }
        t => {
            if t.shared_identity().is_none_or(|a| seen.insert(a)) {
                t.for_each_child(&mut |c| bound_quantifiers(c, out, seen))
            }
        }
    })
}
/// A binding the argument reads or writes holds none of the formal's
/// quantifiers: its cell would carry the quantifier out.
fn escapes_through(formal: &Type, env: &Type) -> Result<()> {
    let Type::Fn(ft) = formal else { return Ok(()) };
    let mut named: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
    ft.collect_tvars(&mut named);
    let quantified: SmallVec<[(ArcStr, usize); 2]> = named
        .iter()
        .filter(|(name, _)| ft.quantifiers.contains(*name))
        .map(|(name, tv)| (name.clone(), tv.cell_addr()))
        .collect();
    let mut own = AHashSet::new();
    bound_quantifiers(env, &mut own, &mut AHashSet::new());
    let mut reached = None;
    let mut seen = AHashSet::new();
    reaches_cell(env, &mut seen, &mut |c| {
        if own.contains(&c.cell_addr()) {
            return;
        }
        if let Some((q, _)) = quantified.iter().find(|(_, a)| *a == c.cell_addr()) {
            reached = Some(q.clone())
        }
    });
    match reached {
        None => Ok(()),
        Some(q) => bail!(
            "the argument is not polymorphic in '{q}: a binding it reaches, of type \
             {}, would carry '{q} out",
            printed_deref(env)
        ),
    }
}

/// Every cell under `t`, following bindings.
fn reaches_cell(t: &Type, seen: &mut AHashSet<usize>, f: &mut impl FnMut(&TVar)) {
    ensure_sufficient(|| match t {
        Type::TVar(tv) => {
            if seen.insert(tv.cell_addr()) {
                f(tv);
                if let Some(b) = tv.binding() {
                    reaches_cell(&b, seen, f)
                }
            }
        }
        t => t.for_each_child(&mut |c| reaches_cell(c, seen, f)),
    })
}

/// The first of the `quantified` cells (name, cell address) that a
/// variable of `value`'s environment reaches, with that variable: a
/// cell at the top level or another definition's, never one the
/// lambdas `value` holds own nor a quantifier a function type in `value`
/// binds. Such a value is not general in that cell.
pub(crate) fn carried_out(
    quantified: &[(ArcStr, usize)],
    value: &Type,
) -> Option<(ArcStr, TVar)> {
    let own: SmallVec<[LambdaId; 2]> = match value.deref_cloned() {
        Some(Type::Fn(ref aft)) => aft.lambda_ids.ids().iter().copied().collect(),
        _ => SmallVec::new(),
    };
    let mut bound = AHashSet::new();
    bound_quantifiers(value, &mut bound, &mut AHashSet::new());
    fn walk(t: &Type, f: &mut impl FnMut(&TVar), seen: &mut AHashSet<usize>) {
        ensure_sufficient(|| match t {
            Type::TVar(tv) => {
                if seen.insert(tv.cell_addr()) {
                    f(tv);
                    if let Some(b) = tv.binding() {
                        walk(&b, f, seen)
                    }
                }
            }
            t => t.for_each_child(&mut |c| walk(c, f, seen)),
        })
    }
    let mut foreign: SmallVec<[TVar; 4]> = SmallVec::new();
    walk(
        value,
        &mut |tv| match tv.level() {
            _ if bound.contains(&tv.cell_addr()) => (),
            Level::Top => foreign.push(tv.clone()),
            Level::Def { owner, .. } if !own.contains(&owner) => foreign.push(tv.clone()),
            Level::Def { .. } | Level::Generic => (),
        },
        &mut AHashSet::new(),
    );
    foreign.into_iter().find_map(|tv| {
        let mut reached = None;
        walk(
            &Type::TVar(tv.clone()),
            &mut |c| {
                if let Some((q, _)) = quantified.iter().find(|(_, a)| *a == c.cell_addr())
                {
                    reached = Some(q.clone())
                }
            },
            &mut AHashSet::new(),
        );
        reached.map(|q| (q, tv))
    })
}

/// Every type variable under `t` by name, with whether each occurrence
/// is data: never under a function, a reference, a typedef application,
/// a nominal type or a constructor application. Bindings are walked.
fn data_positions(
    t: &Type,
    data: bool,
    seen: &mut AHashSet<(usize, bool)>,
    out: &mut AHashMap<ArcStr, (TVar, bool)>,
) {
    ensure_sufficient(|| match t {
        Type::TVar(tv) => {
            out.entry(tv.name.clone()).or_insert_with(|| (tv.clone(), true)).1 &= data;
            if seen.insert((tv.cell_addr(), data))
                && let Some(b) = tv.binding()
            {
                data_positions(&b, data, seen, out)
            }
        }
        // a shared node is walked once in each kind of position
        t if t.shared_identity().is_some_and(|n| !seen.insert((n, data))) => (),
        Type::Fn(_)
        | Type::ByRef(..)
        | Type::Ref(_)
        | Type::Abstract { .. }
        | Type::App(..) => t.for_each_child(&mut |c| data_positions(c, false, seen, out)),
        t => t.for_each_child(&mut |c| data_positions(c, data, seen, out)),
    })
}

/// Whether `t` holds a type variable under an abstract type, a type
/// application or a reference, outside any function type: such a position
/// fixes the variable exactly, so its argument is checked first and the
/// order a call writes its arguments in decides nothing. A function's
/// positions stay in order: an earlier argument types a callback's.
fn holds_invariantly(t: &Type) -> bool {
    ensure_sufficient(|| match t {
        Type::TVar(tv) => tv.binding().is_some_and(|b| holds_invariantly(&b)),
        Type::Fn(_) => false,
        Type::Abstract { .. } | Type::App(..) | Type::Ref(_) | Type::ByRef(..) => {
            let mut tvars: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
            t.collect_tvars(&mut tvars);
            !tvars.is_empty()
        }
        t => {
            let mut any = false;
            t.for_each_child(&mut |c| any = any || holds_invariantly(c));
            any
        }
    })
}

/// A fresh instance's type variables that settle to the widest argument
/// rather than the first: open in the definition `def`, and met only as
/// data in `inst`, so a value of a wider type is well typed wherever the
/// signature holds one.
fn widenable(def: &FnType, inst: &FnType) -> LPooled<AHashMap<ArcStr, TVar>> {
    let mut at_def: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
    def.collect_tvars(&mut at_def);
    let mut seen: LPooled<AHashSet<(usize, bool)>> = LPooled::take();
    let mut found: LPooled<AHashMap<ArcStr, (TVar, bool)>> = LPooled::take();
    inst.for_each_type(&mut |t| data_positions(t, true, &mut seen, &mut found));
    found
        .drain()
        .filter(|(name, (tv, data))| {
            *data && !tv.is_rigid() && at_def.get(name).is_some_and(|d| !d.is_bound())
        })
        .map(|(name, (tv, _))| (name, tv))
        .collect()
}

/// How a fresh instance's arguments settle its [`widenable`] variables:
/// to the widest argument, whatever the order. An argument that does not
/// fit is probed once against its formal with those variables open; it
/// widens the variables it holds wider, and waits for the end of the
/// argument loop when it is neither wider nor narrower.
struct Widening {
    fresh: bool,
    cells: Option<LPooled<AHashMap<ArcStr, TVar>>>,
    deferred: LPooled<Vec<(ArgKey, Type)>>,
}

impl Widening {
    fn new(fresh: bool) -> Self {
        Self { fresh, cells: None, deferred: LPooled::take() }
    }

    fn cells<'a>(
        cells: &'a mut Option<LPooled<AHashMap<ArcStr, TVar>>>,
        def: &Type,
        inst: &FnType,
    ) -> &'a AHashMap<ArcStr, TVar> {
        cells.get_or_insert_with(|| {
            def.with_deref(|t| match t {
                Some(Type::Fn(def)) => widenable(def, inst),
                _ => LPooled::take(),
            })
        })
    }

    /// A copy of `formal` with the widenable variables it holds unbound,
    /// and the copy's variables by name.
    fn open(
        formal: &Type,
        cells: &AHashMap<ArcStr, TVar>,
    ) -> (Type, LPooled<AHashMap<ArcStr, TVar>>) {
        let open = formal.reset_tvars();
        let mut opened: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
        open.collect_tvars(&mut opened);
        for name in cells.keys() {
            if let Some(tv) = opened.get(name) {
                tv.unbind()
            }
        }
        (open, opened)
    }

    /// The formal an argument is pre-unified with: once an earlier
    /// argument settled a widenable variable `formal` holds, the copy
    /// with it open, so the hint cannot refuse a wider argument before
    /// [`Self::misfit`] probes it.
    fn hint(&mut self, def: &Type, inst: &FnType, formal: &Type) -> Option<Type> {
        if !self.fresh {
            return None;
        }
        let cells = Self::cells(&mut self.cells, def, inst);
        let mut held: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
        formal.collect_tvars(&mut held);
        held.iter()
            .any(|(name, tv)| cells.contains_key(name) && tv.is_bound())
            .then(|| Self::open(formal, cells).0)
    }

    /// Whether an argument at `formal` holding a cell nothing has decided
    /// waits for the end of the argument loop: `formal` holds a widenable
    /// variable, which the argument would take as it stands so far, not
    /// as the widest argument settles it.
    fn waits(&mut self, def: &Type, inst: &FnType, formal: &Type) -> bool {
        if !self.fresh {
            return false;
        }
        let cells = Self::cells(&mut self.cells, def, inst);
        let mut held: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
        formal.collect_tvars(&mut held);
        held.keys().any(|name| cells.contains_key(name))
    }

    /// The argument for `key` met `formal`: widen, wait, or the mismatch
    /// error.
    fn fit<R: Rt, E: UserEvent>(
        &mut self,
        env: &Env,
        def: &Type,
        inst: &FnType,
        key: &ArgKey,
        formal: &Type,
        n: &Node<R, E>,
        fit: ArgFit,
    ) -> Result<()> {
        match fit {
            ArgFit::Fits => Ok(()),
            ArgFit::Open => Ok(self.deferred.push((key.clone(), formal.clone()))),
            ArgFit::Misfit => self.misfit(env, def, inst, key, formal, n),
        }
    }

    /// The argument `n` for `key` did not fit `formal`: widen, defer, or
    /// the mismatch error.
    fn misfit<R: Rt, E: UserEvent>(
        &mut self,
        env: &Env,
        def: &Type,
        inst: &FnType,
        key: &ArgKey,
        formal: &Type,
        n: &Node<R, E>,
    ) -> Result<()> {
        let check = || wrap!(n, formal.check_contains(env, n.typ()));
        if !self.fresh || n.typ().has_unbound() {
            return check();
        }
        let cells = Self::cells(&mut self.cells, def, inst);
        if cells.is_empty() {
            return check();
        }
        let (open, opened) = Self::open(formal, cells);
        if !open.contains(env, n.typ())? {
            return match open.discernible_refusal(env, n.typ()) {
                Some(e) => wrap!(n, Err(e)),
                None => check(),
            };
        }
        // every variable the argument holds wider than it stands widens; one
        // that is neither wider nor narrower defers the argument until a
        // later one widens it, whatever the order
        let probe = ContainsFlags::RigidCheck.into();
        let mut wider: LPooled<Vec<(TVar, Type)>> = LPooled::take();
        let mut incomparable = false;
        for (name, cell) in cells.iter() {
            let (Some(new), Some(old)) =
                (opened.get(name).and_then(|tv| tv.binding()), cell.binding())
            else {
                continue;
            };
            if old.contains_with_flags(probe, env, &new)? {
                continue;
            }
            if new.contains_with_flags(probe, env, &old)? {
                wider.push((cell.clone(), new));
            } else {
                incomparable = true;
            }
        }
        for (cell, t) in wider.drain(..) {
            cell.bind(t)
        }
        if incomparable {
            self.deferred.push((key.clone(), formal.clone()));
            return Ok(());
        }
        check()
    }
}

/// A `Ref` to `arg`'s id, typed and placed by its node, else by `typ`.
fn arg_ref<R: Rt, E: UserEvent>(
    ctx: &mut CompileCtx<R, E>,
    top_id: ExprId,
    arg: &Arg<R, E>,
    typ: &Type,
) -> Node<R, E> {
    let (typ, spec) = match &arg.node {
        Some(n) => (n.typ().clone(), arg.spec.clone()),
        None => (typ.clone(), NOP.clone()),
    };
    Ref::new(ctx, arg.id, typ, top_id, spec)
}

/// What a [`CallSite`] knows about its callee.
#[derive(Debug)]
pub(crate) enum Callee<R: Rt, E: UserEvent> {
    /// No callee bound yet. `fnode` is re-evaluated every cycle; the
    /// first cycle it yields a `LambdaDef`, `update()` binds.
    DynamicUnbound,
    /// Bound to a callee that may change cycle-to-cycle; `def` is kept
    /// for the per-cycle identity check against `fnode.update()`.
    DynamicBound { def: Value, apply: Box<dyn Apply<R, E>> },
    /// `def`'s instance, built before the first dispatch
    /// ([`CallSite::prebind`]), which primes `defaults`, the outer
    /// variables the defaults it compiled read, and binds it.
    Prebound { def: Value, apply: Box<dyn Apply<R, E>>, defaults: SmallVec<[BindId; 2]> },
    /// Binding `def` failed: the call is bottom until `fnode` yields
    /// another definition.
    Failed { def: Value },
    /// Pre-bound at compile time by [`CallSite::try_static_resolve`]; the
    /// per-cycle identity check is skipped (`fnode.update()` still runs
    /// for effects). `first_update` primes the body's refs once.
    Static { apply: Box<dyn Apply<R, E>>, first_update: bool },
    /// A statically bound instance the image holds in its heap, decoded
    /// by the first dispatch; `refs` is the body's summary until then.
    Imaged { instance: LambdaInstanceId, first_update: bool, refs: ImagedRefs },
}

/// The reads of an imaged instance's whole tree, flattened from the
/// image's table when first asked.
#[derive(Debug)]
pub(crate) struct ImagedRefs {
    table: TArc<InstanceRefsTable>,
    flat: OnceLock<RefsSummary>,
}

impl ImagedRefs {
    fn flat(&self, instance: LambdaInstanceId) -> &RefsSummary {
        self.flat.get_or_init(|| {
            let mut refed: LPooled<IntSet<BindId>> = LPooled::take();
            let mut triggering: LPooled<IntSet<BindId>> = LPooled::take();
            let mut bound: LPooled<IntSet<BindId>> = LPooled::take();
            let mut seen: LPooled<AHashSet<(LambdaInstanceId, bool)>> = LPooled::take();
            let mut stack: LPooled<Vec<(LambdaInstanceId, bool)>> = LPooled::take();
            stack.push((instance, false));
            while let Some((i, banked)) = stack.pop() {
                if !seen.insert((i, banked)) {
                    continue;
                }
                let Some(r) = self.table.get(&i) else { continue };
                refed.extend(r.own.refed.iter().copied());
                bound.extend(r.own.bound.iter().copied());
                if !banked {
                    triggering.extend(r.own.triggering.iter().copied());
                }
                stack.extend(r.callees.iter().map(|(c, b)| (*c, banked || *b)));
            }
            RefsSummary::of_sets(&refed, &triggering, &bound)
        })
    }
}

/// What a heap instance's own nodes read, and its callees by instance,
/// each marked when a sample's right side holds it.
#[derive(Debug, Clone, netidx_derive::Pack)]
pub(crate) struct InstanceRefs {
    own: RefsSummary,
    callees: Vec<(LambdaInstanceId, bool)>,
}

pub(crate) type InstanceRefsTable = AHashMap<LambdaInstanceId, InstanceRefs>;

/// A walk's reads, each sorted.
#[derive(Debug, Clone, netidx_derive::Pack)]
pub(crate) struct RefsSummary {
    refed: Vec<BindId>,
    triggering: Vec<BindId>,
    bound: Vec<BindId>,
}

/// The definition and instance a site resolved to at compile time, and
/// the instance's resolved type. A self-call site (resolved while its
/// definition's instance with the same identity was elaborating) names
/// that enclosing instance and stays dynamically bound: at runtime it
/// binds an instance per activation.
#[derive(Debug, Clone, netidx_derive::Pack)]
pub(crate) struct StaticCallTarget {
    pub definition: LambdaId,
    pub instance: LambdaInstanceId,
    pub ftype: TArc<FnType>,
}

impl<R: Rt, E: UserEvent> Callee<R, E> {
    fn apply(&self) -> Option<&dyn Apply<R, E>> {
        match self {
            Callee::DynamicUnbound | Callee::Failed { .. } | Callee::Imaged { .. } => {
                None
            }
            Callee::DynamicBound { apply, .. }
            | Callee::Prebound { apply, .. }
            | Callee::Static { apply, .. } => Some(&**apply),
        }
    }

    pub(crate) fn apply_mut(&mut self) -> Option<&mut (dyn Apply<R, E> + 'static)> {
        match self {
            Callee::DynamicUnbound | Callee::Failed { .. } | Callee::Imaged { .. } => {
                None
            }
            Callee::DynamicBound { apply, .. }
            | Callee::Prebound { apply, .. }
            | Callee::Static { apply, .. } => Some(&mut **apply),
        }
    }

    /// Reset to `DynamicUnbound`, returning the bound apply for deletion;
    /// an imaged instance has nothing to delete and stays imaged.
    fn take_apply(&mut self) -> Option<Box<dyn Apply<R, E>>> {
        if matches!(self, Callee::Imaged { .. }) {
            return None;
        }
        match mem::replace(self, Callee::DynamicUnbound) {
            Callee::DynamicUnbound | Callee::Failed { .. } | Callee::Imaged { .. } => {
                None
            }
            Callee::DynamicBound { apply, .. }
            | Callee::Prebound { apply, .. }
            | Callee::Static { apply, .. } => Some(apply),
        }
    }
}

#[derive(Debug)]
pub struct CallSite<R: Rt, E: UserEvent> {
    pub(super) slept: WakeBit,
    pub(super) spec: TArc<Expr>,
    pub(super) ftype: Option<TArc<FnType>>,
    pub(super) rtype: Type,
    pub(crate) fnode: Node<R, E>,
    pub(crate) args: ArgMap<R, E>,
    pub(super) arg_refs: Vec<Node<R, E>>,
    pub(crate) callee: Callee<R, E>,
    pub(crate) static_target: Option<StaticCallTarget>,
    pub(crate) recursive_edge: AtomicBool,
    pub(super) flags: BitFlags<CFlag>,
    pub(super) scope: Scope,
    pub(super) top_id: ExprId,
    pub(super) resident: TagValue,
    /// A collection slot's share of its prototype's kernels, taken by
    /// the instance it binds.
    pub(crate) share: Option<SlotShare>,
    /// The check did not see every default this site omits: the cells
    /// they reach stay open for the bind.
    defaults_open: bool,
    /// The bound callee's function went bottom and its instance sleeps.
    absent: Option<Absent>,
    pub(crate) fork: ForkSite,
    /// The reads of an imaged tree this site registered when it decoded
    /// outside every heap instance, released at delete.
    held: Option<Box<[BindId]>>,
}

/// The registered builtin a site calls by name: its function is a
/// binding the index resolves to a builtin's definition, or one bound to
/// the builtin directly.
pub(crate) fn static_builtin<'a, R: Rt, E: UserEvent>(
    ctx: &'a CompileCtx<R, E>,
    cs: &CallSite<R, E>,
) -> Option<&'a ArcStr> {
    let NodeView::Ref(r) = cs.fnode().view() else { return None };
    if let Some(d) = ctx
        .bind_to_lambda
        .get(&r.id)
        .and_then(|v| v.downcast_ref::<super::lambda::LambdaDef<R, E>>())
    {
        return d.builtin_name();
    }
    let bind = ctx.env.by_id.get(&r.id)?;
    ctx.builtin_bindings.get(&(bind.scope.clone(), bind.name.clone())).map(|i| &i.name)
}

impl<R: Rt, E: UserEvent> CallSite<R, E> {
    /// An unbound site over compiled parts; `ftype` is `None` until
    /// `typecheck0` instantiates it.
    pub(crate) fn unbound(
        spec: TArc<Expr>,
        ftype: Option<FnType>,
        rtype: Type,
        fnode: Node<R, E>,
        args: ArgMap<R, E>,
        scope: Scope,
        flags: BitFlags<CFlag>,
        top_id: ExprId,
    ) -> Self {
        Self {
            slept: WakeBit::default(),
            spec,
            ftype: ftype.map(TArc::new),
            rtype,
            fnode,
            args,
            arg_refs: Vec::new(),
            callee: Callee::DynamicUnbound,
            static_target: None,
            recursive_edge: AtomicBool::new(false),
            flags,
            scope,
            top_id,
            resident: TagValue::phantom(),
            share: None,
            defaults_open: true,
            absent: None,
            fork: ForkSite::default(),
            held: None,
        }
    }

    /// The function type at this call site with the site's tvars unified
    /// in. `None` before typecheck, or if this site errored first.
    pub fn ftype(&self) -> Option<&FnType> {
        self.ftype.as_deref()
    }

    /// The detached, resolved function type owned by a statically-bound
    /// callee instance.
    pub fn resolved_ftype(&self) -> Option<&FnType> {
        self.static_target.as_ref().map(|target| &*target.ftype)
    }

    pub(crate) fn static_target(&self) -> Option<&StaticCallTarget> {
        self.static_target.as_ref()
    }

    /// Whether this site's callee was bound at compile time and no rebind
    /// replaces it.
    pub(crate) fn bound_statically(&self) -> bool {
        matches!(self.callee, Callee::Static { .. })
    }

    pub(crate) fn is_recursive_edge(&self) -> bool {
        self.recursive_edge.load(Relaxed)
    }

    pub(crate) fn set_recursive_edge(&self, recursive: bool) {
        self.recursive_edge.store(recursive, Relaxed)
    }

    /// Source-order argument list. Pair with `args()` to recover the
    /// runtime sub-Node per arg.
    pub fn spec_args(&self) -> &TArc<[(Option<ArcStr>, Expr)]> {
        match &self.spec.kind {
            ExprKind::Apply(a) => &a.args,
            _ => unreachable!("CallSite spec must be ExprKind::Apply"),
        }
    }

    /// Look up a positional argument's compiled sub-Node.
    pub fn arg_positional(&self, idx: usize) -> Option<&Node<R, E>> {
        self.arg(&ArgKey::Positional(idx))
    }

    /// Every argument the call wrote, compiled.
    pub(crate) fn arg_nodes(&self) -> impl Iterator<Item = &Node<R, E>> {
        self.args.values().filter_map(|a| a.node.as_ref())
    }

    /// The order the node-walk updates `nodes`, some of this call's
    /// argument nodes in another order: their indices by each one's place
    /// among the arguments, the written ones then the defaults a bind
    /// added; a node that is no argument goes last. It is the order their
    /// raises reach a handler.
    pub(crate) fn update_order(
        &self,
        nodes: &[Option<&Node<R, E>>],
    ) -> SmallVec<[usize; 12]> {
        let rank = |n: Option<&Node<R, E>>| {
            n.and_then(|n| self.arg_nodes().position(|a| std::ptr::eq(a, n)))
                .unwrap_or(usize::MAX)
        };
        let mut order: SmallVec<[usize; 12]> = (0..nodes.len()).collect();
        order.sort_by_key(|i| rank(nodes[*i]));
        order
    }

    /// Look up a labeled argument's compiled sub-Node.
    pub fn arg_named(&self, name: &ArcStr) -> Option<&Node<R, E>> {
        self.arg(&ArgKey::Named(name.clone()))
    }

    fn arg(&self, key: &ArgKey) -> Option<&Node<R, E>> {
        self.args.get(key).and_then(|a| a.node.as_ref())
    }

    /// The function expression's compiled Node.
    pub fn fnode(&self) -> &Node<R, E> {
        &self.fnode
    }

    /// View the [`Apply`] this CallSite is bound to; `None` until a
    /// runtime bind or `try_static_resolve` has populated `self.callee`.
    pub fn resolved_apply(&self) -> Option<ApplyView<'_, R, E>> {
        self.callee.apply().map(|a| a.view())
    }

    /// The call `spec` compiled: its function and arguments, unbound.
    pub(crate) fn compile(
        ctx: &mut CompileCtx<R, E>,
        flags: BitFlags<CFlag>,
        spec: TArc<Expr>,
        scope: &Scope,
        top_id: ExprId,
        args: &TArc<[(Option<ArcStr>, Expr)]>,
        f: &TArc<Expr>,
    ) -> Result<Node<R, E>> {
        reject_dead_variadic_call(ctx, scope, f, args).at(&spec)?;
        // a call's function is compiled as written: a trait method named
        // here is the dispatcher itself, never its eta-expansion
        let fnode = match &f.kind {
            ExprKind::Ref { name } => Ref::compile(ctx, f.clone(), scope, top_id, name)?,
            _ => compile(ctx, flags, (**f).clone(), scope, top_id)?,
        };
        let args = compile_apply_args(ctx, flags, scope, top_id, &spec, args)?;
        let site = Self::unbound(
            spec,
            None,
            Type::empty_tvar(),
            fnode,
            args,
            scope.clone(),
            flags,
            top_id,
        );
        Ok(Node::new(CallNode::Call(site)))
    }

    fn clear_prepared_bind(&mut self, ctx: &mut CompileCtx<R, E>) {
        if let Some(apply) = self.callee.take_apply() {
            ctx.discard_apply(apply);
        }
        // the arguments change: whether they may fork is decided again
        self.fork = ForkSite::default();
        for n in self.arg_refs.drain(..) {
            ctx.discard(n);
        }
        self.args.retain(|_, arg| {
            if arg.stage.is_default() {
                ctx.discard_stored(arg.id);
                if let Some(n) = arg.node.take() {
                    ctx.discard(n);
                }
                false
            } else {
                true
            }
        });
    }

    /// Build the site's argument references for `f`, compiling the
    /// defaults it omits; `defaults` collects what those read.
    fn prepare_bind(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        scope: &Scope,
        flags: BitFlags<CFlag>,
        f: &LambdaDef<R, E>,
        defaults: &mut Refs,
    ) -> Result<()> {
        let mut flags = flags;
        flags.remove(CFlag::WarnUnhandled);
        self.clear_prepared_bind(ctx);
        let formals = f.typ.args.iter().zip(f.argspec.iter());
        for ((farg, argspec), key) in formals.zip(ArgKey::of_formals(&f.typ.args)) {
            if let Some(arg) = self.args.get(&key) {
                let r = arg_ref(ctx, self.top_id, arg, &farg.typ);
                self.arg_refs.push(r);
                continue;
            }
            let ArgKey::Named(name) = &key else { bail!("missing required {key}") };
            if !farg.kind.has_default() {
                bail!("BUG: in bind missing required argument {name}")
            }
            let Some(expr) = argspec.kind.default() else {
                bail!("expected default value")
            };
            let default_node =
                self.checked_default(ctx, flags, scope, f, name, expr, defaults)?;
            let typ = default_node.typ().clone();
            let id = BindId::new();
            let spec = TArc::new(default_node.spec().clone());
            self.arg_refs.push(Ref::new(ctx, id, typ, self.top_id, spec.clone()));
            self.args
                .insert(key, Arg::new(id, Some(default_node), ArgStage::Compiled, spec));
        }
        if f.typ.vargs.is_some() {
            let positional = f.typ.args.iter().filter(|a| a.is_positional()).count();
            for key in ArgKey::variadic(positional) {
                let Some(arg) = self.args.get(&key) else { break };
                let r = arg_ref(ctx, self.top_id, arg, &Type::Bottom);
                self.arg_refs.push(r);
            }
        }
        Ok(())
    }

    /// `f`'s default for the labeled argument `name` as this site sees
    /// it: compiled in `f`'s environment under the site's handlers, and
    /// checked against the site's view of the parameter, else, where the
    /// site's fn type hides the label, `f`'s own parameter as that type
    /// instantiates it.
    fn checked_default(
        &self,
        ctx: &mut CompileCtx<R, E>,
        flags: BitFlags<CFlag>,
        scope: &Scope,
        f: &LambdaDef<R, E>,
        name: &ArcStr,
        expr: &Expr,
        defaults: &mut Refs,
    ) -> Result<Node<R, E>> {
        let entered = ctx.enter_default(f.id, name);
        let node = checked_default(
            ctx,
            flags,
            scope,
            self.top_id,
            f,
            name,
            expr,
            defaults,
            self.ftype.as_deref(),
        );
        if entered {
            ctx.leave_default(f.id, name)
        }
        node
    }

    /// A function passed where a fn type is expected is called through it:
    /// each default of its definition that such a call may omit, the
    /// formal hiding the label or making it optional, is judged here, at
    /// the parameter as the formal instantiates it.
    fn check_value_defaults(
        &self,
        ctx: &mut CompileCtx<R, E>,
        ftype: &FnType,
    ) -> Result<()> {
        let mut flags = self.flags;
        flags.remove(CFlag::WarnUnhandled);
        for (farg, key) in ftype.args.iter().zip(ArgKey::of_formals(&ftype.args)) {
            let Some(n) = self.args.get(&key).and_then(|a| a.node.as_ref()) else {
                continue;
            };
            wrap!(
                n,
                check_hidden_defaults(
                    ctx,
                    flags,
                    &self.scope,
                    self.top_id,
                    &farg.typ,
                    n.typ(),
                    Narrowing::Site
                )
            )?;
        }
        Ok(())
    }

    /// What the call raises joins the enclosing catch's type; with no
    /// catch, `check` judges it an unhandled error.
    fn raise_throws(
        &self,
        ctx: &CompileCtx<R, E>,
        ftype: &FnType,
        check: bool,
    ) -> Result<()> {
        // a definition's own `throws 'e` (a call through a `fn(..) throws
        // 'e` parameter) still reaches the catch: it joins as the cell
        let Some(t) = ftype.throws.deref_cloned() else {
            let rigid = match &ftype.throws {
                Type::TVar(tv) => tv.open_cell().is_some_and(|c| c.is_rigid()),
                _ => false,
            };
            return match self.scope.dynamic.handler() {
                Some(h) if rigid => join_open_raised(&ctx.env, &h, &ftype.throws),
                _ => Ok(()),
            };
        };
        match self.scope.dynamic.handler() {
            Some(h) => join_raised(&ctx.env, &h, &t),
            // it doesn't throw any errors
            None if t == Type::Bottom || !check => Ok(()),
            None => Qop::<R, E>::check_unhandled(
                &ctx.env,
                self.flags,
                &self.spec,
                format_args!("error {t} raised from function call {}", self.fnode.spec()),
            ),
        }
    }

    /// Check every default this site omits, for each definition the
    /// callee may be. False when one is not known here: its default
    /// narrows the site's cells at its bind. `carriers` collects the
    /// labels whose default can carry a value of a cell its type holds.
    fn check_omitted_defaults(
        &self,
        ctx: &mut CompileCtx<R, E>,
        ftype: &FnType,
        carriers: &mut SmallVec<[ArcStr; 2]>,
    ) -> Result<bool> {
        let omitted = |name: &ArcStr| {
            self.args
                .get(&ArgKey::Named(name.clone()))
                .is_some_and(|a| a.stage.is_default())
        };
        if !ftype.args.iter().any(|a| a.label().is_some_and(omitted)) {
            return Ok(true);
        }
        let mut flags = self.flags;
        flags.remove(CFlag::WarnUnhandled);
        // the definitions the callee may be: by its type's lambda ids, else
        // (a restored interface `val`'s type carries none) by the binding
        // the function names
        let ids = ftype.lambda_ids.ids();
        let mut defs: SmallVec<[Option<Value>; 2]> =
            ids.iter().map(|id| ctx.lambda_defs.get(id).cloned()).collect();
        if defs.is_empty()
            && let NodeView::Ref(r) = self.fnode.view()
            && let Some(fv) = ctx.bind_to_lambda.get(&r.id)
        {
            defs.push(Some(fv.clone()));
        }
        let mut known = !defs.is_empty();
        for def in defs.iter() {
            let Some(f) = def.as_ref().and_then(|v| v.downcast_ref::<LambdaDef<R, E>>())
            else {
                known = false;
                continue;
            };
            for (farg, argspec) in f.typ.args.iter().zip(f.argspec.iter()) {
                let Some(name) = farg.label().filter(|n| omitted(n)) else { continue };
                let Some(expr) = argspec.kind.default() else {
                    known = false;
                    continue;
                };
                // a default that reaches its own label's check again (`let
                // rec f = |#a = f(1), b| b`) is not known here: the run-time
                // bind narrows it
                if ctx.default_in_check(f.id, name) {
                    known = false;
                    continue;
                }
                let node = self.checked_default(
                    ctx,
                    flags,
                    &self.scope,
                    f,
                    name,
                    expr,
                    &mut Refs::default(),
                )?;
                if carries(&ctx.env, node.typ()) {
                    carriers.push(name.clone());
                }
                ctx.discard(node);
            }
        }
        Ok(known)
    }

    /// A builtin reads a quantifier of its signature only through the
    /// values of the arguments that hold it. One the result and throws do
    /// not hold, and no argument this call passes or omits can carry a
    /// value of, is never read here: it settles to ⊥ whatever its bounds.
    fn settle_unread(&self, env: &Env, ftype: &FnType, carriers: &[ArcStr]) {
        let mut read: LPooled<AHashMap<usize, TVar>> = LPooled::take();
        ftype.rtype.reached_cells(&mut read);
        ftype.throws.reached_cells(&mut read);
        if let Some(t) = &ftype.vargs {
            t.reached_cells(&mut read)
        }
        for (farg, key) in ftype.args.iter().zip(ArgKey::of_formals(&ftype.args)) {
            let carried = match self.args.get(&key) {
                Some(a) if a.stage.is_default() => {
                    farg.label().is_some_and(|l| carriers.contains(l))
                }
                Some(a) => a.node.as_ref().is_none_or(|n| carries(env, n.typ())),
                None => true,
            };
            if carried {
                farg.typ.reached_cells(&mut read)
            }
        }
        let mut tvs: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
        ftype.collect_tvars(&mut tvs);
        for (name, tv) in tvs.iter() {
            if ftype.quantifiers.contains(name) && !read.contains_key(&tv.cell_addr()) {
                tv.settle_unread()
            }
        }
    }

    fn init_prepared_bind(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        scope: &Scope,
        f: &LambdaDef<R, E>,
        mode: BindMode<'_>,
    ) -> Result<Box<dyn Apply<R, E>>> {
        (f.init)(scope, ctx, &mut self.arg_refs, mode, self.top_id)
    }

    /// The instance of `f` a run-time bind dispatches, and the outer
    /// variables the defaults it compiled read.
    fn setup_dynamic_bind(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        scope: &Scope,
        flags: BitFlags<CFlag>,
        f: &LambdaDef<R, E>,
    ) -> Result<(Box<dyn Apply<R, E>>, SmallVec<[BindId; 2]>)> {
        let mut refs = Refs::default();
        self.prepare_bind(ctx, scope, flags, f, &mut refs)?;
        let mut defaults = SmallVec::new();
        refs.with_external_refs(|id| defaults.push(id));
        // a site its check never typed sees the callee as a call would
        let view = match &self.ftype {
            Some(ft) => ft.resolve_tvars(),
            None => f.typ.instantiate(&ctx.rec_defs),
        };
        let mut apply =
            self.init_prepared_bind(ctx, scope, f, BindMode::Dynamic(&view))?;
        if let Err(e) = apply.typecheck0(ctx, &mut self.arg_refs) {
            ctx.discard_apply(apply);
            return Err(
                e.context(format!("a run-time bind at {} did not type", self.spec))
            );
        }
        Ok((apply, defaults))
    }

    /// Elaborate the omitted defaults in `env`, their definition's.
    fn typecheck_static_defaults(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        def: &LambdaDef<R, E>,
    ) -> Result<()> {
        ctx.with_restored(def.env.clone(), |ctx| {
            for (key, arg) in self.args.iter_mut() {
                if arg.stage.is_default()
                    && let Some(node) = arg.node.as_mut()
                {
                    let label = match key {
                        ArgKey::Named(name) => Some(name),
                        _ => None,
                    };
                    let entered = label.filter(|l| ctx.enter_default(def.id, l));
                    let res = wrap!(node, node.typecheck1(ctx));
                    if let Some(l) = entered {
                        ctx.leave_default(def.id, l)
                    }
                    res?;
                }
            }
            Ok(())
        })
    }

    /// This site's terminal settle of still-unbound constrained cells,
    /// deferred to the statement boundary. Cells reachable from an
    /// omitted default the check did not see are exempt: the default
    /// binds them at the bind.
    fn pending_settle(&self, ftype: &FnType) -> crate::PendingSettle {
        let mut dtv: LPooled<AHashMap<usize, TVar>> = LPooled::take();
        for farg in ftype.args.iter().filter(|_| self.defaults_open) {
            if let FnArgKind::Labeled { name, .. } = &farg.kind
                && let Some(a) = self.args.get(&ArgKey::Named(name.clone()))
                && a.stage.is_default()
            {
                crate::typ::settle::position_cells(&farg.typ, &mut dtv);
            }
        }
        let exempt: AHashSet<usize> = dtv.drain().map(|(addr, _)| addr).collect();
        // The call's own result cell joins the settle set: a literal ⊥
        // rtype unifies without binding it.
        let rtype = match &self.rtype {
            Type::TVar(tv) => Some(tv.clone()),
            _ => None,
        };
        crate::PendingSettle::Site {
            sigs: smallvec::SmallVec::new(),
            ftype: ftype.clone(),
            rtype,
            exempt,
            spec: self.spec.clone(),
        }
    }

    fn instance_ftype(&self) -> Option<FnType> {
        self.callee.apply().map(|apply| apply.typ().resolve_tvars())
    }

    /// Re-read the bound instance's resolved ftype into `static_target`.
    /// `None` when no apply is bound.
    fn refresh_static_ftype(&mut self) -> Option<FnType> {
        let ftype = self.instance_ftype()?;
        if let Some(target) = &mut self.static_target {
            target.ftype = TArc::new(ftype.clone());
        }
        Some(ftype)
    }

    /// The resolution half of `typecheck1`, run under the caller's cell
    /// protection so it unwinds on every error path.
    fn typecheck1_resolve(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
    ) -> Result<Option<Node<R, E>>> {
        if let Some(lowered) = self.try_static_resolve(ctx)? {
            return Ok(Some(lowered));
        }
        self.refresh_static_ftype();
        Ok(None)
    }

    fn setup_static_bind(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        scope: &Scope,
        flags: BitFlags<CFlag>,
        f: &LambdaDef<R, E>,
    ) -> Result<(Box<dyn Apply<R, E>>, FnType)> {
        let _profile = profile::phase(Phase::StaticBind);
        self.prepare_bind(ctx, scope, flags, f, &mut Refs::default())?;
        if self.ftype.is_none() {
            bail!("statically resolving an untyped call site: {}", self.spec)
        }
        let site_ftype = self.ftype.as_ref().unwrap().resolve_tvars();
        let instance_ftype = if same_parameters(&site_ftype, &f.typ) {
            site_ftype.clone()
        } else {
            super::lambda::fit_callee(ctx, &site_ftype, &f.typ)?.resolve_tvars()
        };
        let apply = self.init_prepared_bind(
            ctx,
            scope,
            f,
            BindMode::Static { instance: &instance_ftype },
        )?;
        let instance_ftype = apply.typ().as_ref().clone();
        // `site_ftype` is a deep clone: the instance's inferred return
        // must be unified back into the site's live rtype cell.
        if let Some(site_ft) = self.ftype.as_ref() {
            let before =
                dbgenv::graphix_elab_audit().then(|| printed_deref(&site_ft.rtype));
            if let Err(e) = site_ft.rtype.check_contains(&ctx.env, &instance_ftype.rtype)
            {
                if ctx.def_gate_depth == 0 {
                    super::lambda::elab_audit::report(
                        "unify-back",
                        &self.spec,
                        format_args!("{e:#}"),
                    );
                }
                ctx.discard_apply(apply);
                return Err(e.at(self.fnode.spec()));
            }
            if let Some(before) = before
                && ctx.def_gate_depth == 0
            {
                let after = printed_deref(&site_ft.rtype);
                if before != after {
                    super::lambda::elab_audit::report(
                        "unify-back",
                        &self.spec,
                        format_args!(
                            "narrowed the site's return from {before} to {after}"
                        ),
                    );
                }
            }
        }
        Ok((apply, instance_ftype))
    }

    fn bind(
        &mut self,
        ctx: &mut ExecCtx<'_, R, E>,
        fv: Value,
        f: &LambdaDef<R, E>,
        set: &mut Published,
    ) -> Result<()> {
        // the previous binding leaves in a batch of its own, so the build's
        // holds only what it made: a failed build registers nothing it
        // recorded, and its deletes cancel only its own pending refs
        self.clear_prepared_bind(ctx);
        ctx.apply_deferred();
        let defaults = match self.build_bound(ctx, fv, f) {
            Ok(d) => {
                ctx.control.rebuilt();
                ctx.apply_deferred();
                d
            }
            Err(e) => {
                self.clear_prepared_bind(ctx);
                ctx.drop_deferred();
                return Err(e);
            }
        };
        self.prime_bound(ctx, &defaults, set);
        Ok(())
    }

    /// Build this site's instance of the slot callee its function is the
    /// constant of, in the compile task `ctx`, ahead of its first
    /// dispatch. A no-op for any other site.
    pub(crate) fn prebind(&mut self, ctx: &mut CompileCtx<R, E>) {
        if !matches!(self.callee, Callee::DynamicUnbound) {
            return;
        }
        let NodeView::Constant(c) = self.fnode.view() else { return };
        let fv = c.value.clone();
        let Some(f) = fv.downcast_ref::<LambdaDef<R, E>>() else { return };
        if !ctx.lambda_defs.contains_key(&f.id) {
            return;
        }
        match self.build_bound(ctx, fv.clone(), f) {
            Ok(defaults) => {
                let Callee::DynamicBound { apply, .. } =
                    mem::replace(&mut self.callee, Callee::DynamicUnbound)
                else {
                    unreachable!("a built callee is bound")
                };
                self.callee = Callee::Prebound { def: fv, apply, defaults }
            }
            Err(e) => {
                let msg =
                    format_compact!("{}: binding the callee failed: {e:#}", self.spec);
                super::error::report_failure!(&msg);
                self.clear_prepared_bind(ctx);
                self.callee = Callee::Failed { def: fv };
            }
        }
    }

    /// Build, check and analyze this site's instance of `f` (the value
    /// `fv`) as a run-time bind, a compile task of its own, leaving it
    /// `DynamicBound`: the outer variables its compiled defaults read,
    /// for [`Self::prime_bound`].
    fn build_bound(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        fv: Value,
        f: &LambdaDef<R, E>,
    ) -> Result<SmallVec<[BindId; 2]>> {
        let _bind_span = perfdbg::span(&perfdbg::BIND_NS);
        if perfdbg::enabled() {
            perfdbg::BIND_CALLS.fetch_add(1, Relaxed);
        }
        let (scope, flags) = (self.scope.clone(), self.flags);
        // The lazy-bound body's typecheck defers settles no statement
        // boundary will drain.
        let defaults = super::with_runtime_settles(ctx, |ctx| {
            let setup_span = perfdbg::span(&perfdbg::SETUP_NS);
            let (apply, defaults) = self.setup_dynamic_bind(ctx, &scope, flags, f)?;
            drop(setup_span);
            // its defaults are elaborated as a static bind's are: trait
            // dispatch resolves in typecheck1
            if let Err(e) = self.typecheck_static_defaults(ctx, f) {
                ctx.discard_apply(apply);
                return Err(e);
            }
            // A def whose defining Lambda node was deleted has no
            // `lambda_defs` entry; restore it for this elaboration only.
            let restored_def = !ctx.lambda_defs.contains_key(&f.id);
            if restored_def {
                ctx.lambda_defs.insert(f.id, fv.clone());
            }
            self.callee = Callee::DynamicBound { def: fv, apply };
            // The lazy-bound body postdates the program-wide typecheck1 and
            // analysis passes: resolve its call sites and analyze it here.
            let identity = self.fn_arg_identity(ctx);
            if let Some(apply) = self.callee.apply_mut()
                && let ApplyView::Lambda(g) = apply.view()
            {
                let instance = g.instance_id();
                let instance_ftype = apply.typ();
                // A recursive lazy bind: its body stays lazy.
                let already_active = ctx.resolving(f.id, &identity).is_some();
                ctx.push_resolving(
                    f.id,
                    ResolvingLambda {
                        instance,
                        ftype: instance_ftype.as_ref().clone(),
                        identity,
                    },
                );
                let elaborated = match already_active {
                    true => Ok(()),
                    false => {
                        let _tc1_span = perfdbg::span(&perfdbg::TC1_NS);
                        apply.typecheck1(ctx, &mut [], &instance_ftype)
                    }
                };
                ctx.pop_resolving(f.id, instance);
                if let Err(e) = elaborated {
                    if restored_def {
                        ctx.lambda_defs.remove(&f.id);
                    }
                    if let Callee::DynamicBound { def, apply } =
                        mem::replace(&mut self.callee, Callee::DynamicUnbound)
                    {
                        ctx.discard_apply(apply);
                        self.callee = Callee::Failed { def };
                    }
                    return Err(e.context(format!(
                        "a run-time bind at {} did not elaborate",
                        self.spec
                    )));
                }
                if let ApplyView::Lambda(g) = apply.view() {
                    let _an_span = perfdbg::span(&perfdbg::ANALYZE_NS);
                    let self_bind = match self.fnode.view() {
                        NodeView::Ref(r) => Some(r.id),
                        _ => None,
                    };
                    analysis::analyze_bound_callee(g, self_bind, ctx);
                }
            }
            if restored_def {
                ctx.lambda_defs.remove(&f.id);
            }
            Ok(defaults)
        })?;
        if let Some(share) = &self.share
            && let Some(apply) = self.callee.apply_mut()
        {
            share::fuse_slot(ctx, share, self.top_id, |ctx| apply.fuse(ctx));
        }
        Ok(defaults)
    }

    /// Prime `defaults`, the outer variables a fresh bind's compiled
    /// defaults read, with their stored values, and run the defaults for
    /// the first time, under the init view.
    fn prime_bound(
        &mut self,
        ctx: &mut ExecCtx<'_, R, E>,
        defaults: &[BindId],
        set: &mut Published,
    ) {
        for id in defaults {
            if let Some(v) = ctx.rt.store_value(id)
                && ctx.try_publish(*id, TagValue::fired(v)).is_ok()
            {
                set.push(*id);
            }
        }
        ctx.under(View::Birth, |ctx| {
            for arg in self.args.values_mut() {
                if arg.stage == ArgStage::Compiled
                    && let Some(node) = &mut arg.node
                {
                    arg.stage = ArgStage::Running;
                    let tv = node.update(ctx).clone();
                    let feeds = Feeds::Id(arg.id);
                    if publish_production(ctx, feeds, &tv, true, QuietAtRoot::Deliver) {
                        set.push(arg.id);
                    }
                }
            }
        });
    }

    /// Pre-bind this CallSite to a statically known `LambdaDef` at compile
    /// time, replacing the lazy bind `update()` would run. Idempotent.
    pub fn resolve_static(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        def: &LambdaDef<R, E>,
    ) -> Result<()> {
        if matches!(self.callee, Callee::Static { .. }) || self.static_target.is_some() {
            return Ok(());
        }
        // a site its own omitted default's elaboration reaches stays
        // dynamic: elaborating it would elaborate that default again
        let labels = self.omitted_labels(def);
        if labels.iter().any(|l| ctx.default_in_check(def.id, l)) {
            return Ok(());
        }
        self.resolve_static_entered(ctx, def)
    }

    /// [`Self::resolve_static`] outside the checks of its omitted
    /// defaults.
    fn resolve_static_entered(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        def: &LambdaDef<R, E>,
    ) -> Result<()> {
        // A site reached while an instantiation with the same fn-arg
        // identity is resolving is a self-call and shares that instance;
        // a different identity is a fresh instantiation.
        let identity = self.fn_arg_identity(ctx);
        let active = ctx.resolving(def.id, &identity);
        if let Some(active) = active {
            let scope = self.scope.clone();
            self.prepare_bind(ctx, &scope, self.flags, def, &mut Refs::default())?;
            self.typecheck_static_defaults(ctx, def)?;
            if self.ftype.is_none() {
                bail!("statically resolving an untyped call site: {}", self.spec)
            }
            self.static_target = Some(StaticCallTarget {
                definition: def.id,
                instance: active.instance,
                ftype: TArc::new(active.ftype.resolve_tvars()),
            });
            return Ok(());
        }
        let p = profile::phase(Phase::TaskFork);
        let mut task = ctx.fork();
        drop(p);
        let res = self.bind_instance(&mut task, def, identity);
        let _p = profile::phase(Phase::TaskJoin);
        ctx.join(task);
        res
    }

    /// The labels of `def` with a default this site does not pass.
    fn omitted_labels(&self, def: &LambdaDef<R, E>) -> SmallVec<[ArcStr; 2]> {
        def.typ
            .args
            .iter()
            .zip(def.argspec.iter())
            .filter_map(|(farg, spec)| {
                let name = farg.label()?;
                spec.kind.default()?;
                match self.args.get(&ArgKey::Named(name.clone())) {
                    Some(a) if !a.stage.is_default() => None,
                    _ => Some(name.clone()),
                }
            })
            .collect()
    }

    /// Build and check this site's instance of `def`, whose fn-arg
    /// identity is `identity`, in the compile task `ctx`.
    fn bind_instance(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        def: &LambdaDef<R, E>,
        identity: FnArgIdentity,
    ) -> Result<()> {
        let scope = self.scope.clone();
        let (apply, instance_ftype) =
            self.setup_static_bind(ctx, &scope, self.flags, def)?;
        let instance = match apply.view() {
            ApplyView::Lambda(g) => Some(g.instance_id()),
            ApplyView::BuiltIn(_) | ApplyView::Opaque => None,
        };
        if let Some(instance) = instance {
            self.static_target = Some(StaticCallTarget {
                definition: def.id,
                instance,
                ftype: TArc::new(instance_ftype.clone()),
            });
        }
        self.callee = Callee::Static { apply, first_update: true };
        // a static bind's defaults run from its first update
        for arg in self.args.values_mut() {
            if arg.stage == ArgStage::Compiled {
                arg.stage = ArgStage::Running;
            }
        }
        // Fn-typed args are registered under the instance's param
        // BindIds for the whole body typecheck (`register_fn_params`).
        let param_binds = self.register_fn_params(ctx, &instance_ftype, def.id);
        if let Some(instance) = instance {
            ctx.push_resolving(
                def.id,
                ResolvingLambda {
                    instance,
                    ftype: instance_ftype.clone(),
                    identity: identity.clone(),
                },
            );
        }
        let typecheck0 = {
            let (callee, arg_refs) = (&mut self.callee, &mut self.arg_refs);
            callee
                .apply_mut()
                .expect("static callee must have an apply")
                .typecheck0(ctx, arg_refs)
        }
        .with_context(|| format!("in the instance of {} at this call site", self.spec));
        let resolved_ftype =
            self.refresh_static_ftype().expect("static callee must have an apply");
        let res = typecheck0
            .and_then(|()| self.typecheck_static_defaults(ctx, def))
            .and_then(|()| {
                self.callee
                    .apply_mut()
                    .expect("static callee must have an apply")
                    .typecheck1(ctx, &mut [], &resolved_ftype)
                    .with_context(|| {
                        format!("in the instance of {} at this call site", self.spec)
                    })
            });
        self.refresh_static_ftype().expect("static callee must have an apply");
        if res.is_ok() {
            if let Callee::Static { apply, .. } = &self.callee {
                if let ApplyView::Lambda(g) = apply.view() {
                    profile::instance_signature(g.instance_id(), g.typ(), || {
                        Some(
                            ahash::RandomState::with_seeds(0, 0, 0, 0)
                                .hash_one(&identity),
                        )
                    });
                }
            }
        }
        Self::unregister_fn_params(ctx, param_binds);
        if let Some(instance) = instance {
            ctx.pop_resolving(def.id, instance);
        }
        res
    }

    /// Pre-bind this site when its function expression resolves to one
    /// known `LambdaDef` (a `Ref` to a non-`<-`-target lambda binding, or a
    /// lambda literal), or dispatch a trait method by its self type.
    /// No-op for dynamic call sites.
    fn try_static_resolve(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
    ) -> Result<Option<Node<R, E>>> {
        if matches!(self.callee, Callee::Static { .. }) {
            return Ok(None);
        }
        let target: Option<Value> = match self.fnode.view() {
            NodeView::Ref(r) => {
                if dbgenv::gxdbg_resolve() {
                    eprintln!(
                        "RESOLVE {} id={:?} unstable={} b2l={}",
                        self.spec,
                        r.id,
                        ctx.batch_connect_targets.contains(&r.id),
                        ctx.bind_to_lambda.contains_key(&r.id),
                    );
                }
                if ctx.batch_connect_targets.contains(&r.id) {
                    None
                } else {
                    let fv = ctx.bind_to_lambda.get(&r.id).cloned();
                    if fv.is_some() {
                        ctx.record_static_callee(r.id, &self.spec);
                    }
                    fv
                }
            }
            NodeView::Lambda(l) => Some(l.def_value().clone()),
            // an embedder's callable calls the lambda value it was given
            NodeView::Constant(c) => Some(c.value.clone()),
            _ => None,
        };
        let fv = match target {
            Some(fv) => fv,
            None => {
                if let NodeView::Ref(r) = self.fnode.view()
                    && let Some(tm) = ctx.env.trait_methods.get(&r.id).copied()
                {
                    return self.resolve_trait_call(ctx, tm);
                }
                return Ok(None);
            }
        };
        let Some(def) = fv.downcast_ref::<LambdaDef<R, E>>() else {
            return Ok(None);
        };
        self.resolve_static(ctx, def).map(|()| None)
    }

    /// This site's instantiation identity ([`FnArgIdentity`]): per
    /// written argument, the source lambda it resolves to (a literal is
    /// its own source; a `Ref` goes through `bind_to_lambda`; a `<-`
    /// target is dynamic). A default is the definition's, not the site's,
    /// so it is left out wherever the bind has got to.
    fn fn_arg_identity(&self, ctx: &CompileCtx<R, E>) -> FnArgIdentity {
        let mut identity: FnArgIdentity = self
            .args
            .iter()
            .filter(|(_, arg)| !arg.stage.is_default())
            .filter_map(|(key, arg)| {
                let source = arg.node.as_ref().and_then(|node| match node.view() {
                    _ if let Some(l) = lambda_literal(node) => Some(l.source_id()),
                    NodeView::Ref(r) if !ctx.batch_connect_targets.contains(&r.id) => ctx
                        .bind_to_lambda
                        .get(&r.id)
                        .and_then(|fv| fv.downcast_ref::<LambdaDef<R, E>>())
                        .map(|def| def.source),
                    _ => None,
                });
                source.map(|s| (key.clone(), Some(s)))
            })
            .collect();
        identity.sort_unstable_by(|(a, _), (b, _)| a.cmp(b));
        identity
    }

    /// Register this site's statically known fn-typed args under the
    /// instance's param BindIds. Held for the whole body typecheck so calls
    /// to and captures of a fn parameter resolve in one pass.
    fn register_fn_params(
        &self,
        ctx: &mut CompileCtx<R, E>,
        ftype: &FnType,
        lid: LambdaId,
    ) -> FnParams {
        let mut params = FnParams::default();
        let apply = match self.callee.apply() {
            Some(a) => a,
            None => return params,
        };
        let ApplyView::Lambda(g) = apply.view() else {
            return params;
        };
        let formals =
            ftype.args.iter().zip(g.args()).zip(ArgKey::of_formals(&ftype.args));
        for (i, ((farg, pat), key)) in formals.enumerate() {
            if !farg.typ.with_deref(|t| matches!(t, Some(Type::Fn(_)))) {
                continue;
            }
            let Some(id) = pat.single_bind_id() else { continue };
            let k = (lid, i as u32);
            params.formals.push((k, ctx.instance_fn_params.insert(k, id)));
            let param_binds = &mut params.binds;
            let Some(arg) = self.args.get(&key) else { continue };
            ctx.fn_param_args.insert(id, arg.spec.clone());
            params.args.push(id);
            let Some(arg_node) = self.arg(&key) else { continue };
            match arg_node.view() {
                _ if let Some(l) = lambda_literal(arg_node) => {
                    let fv = l.def_value().clone();
                    if let Some(def) = fv.downcast_ref::<LambdaDef<R, E>>() {
                        ctx.fn_forward_resolutions.insert(id, def.id);
                    }
                    ctx.bind_to_lambda.insert(id, fv);
                    param_binds.push(id);
                }
                NodeView::Ref(r) => {
                    if ctx.batch_connect_targets.contains(&r.id) {
                        continue;
                    }
                    if let Some(fv) = ctx.bind_to_lambda.get(&r.id).cloned() {
                        ctx.record_static_callee(r.id, &self.spec);
                        if let Some(def) = fv.downcast_ref::<LambdaDef<R, E>>() {
                            ctx.fn_forward_resolutions.insert(id, def.id);
                        }
                        ctx.bind_to_lambda.insert(id, fv);
                        param_binds.push(id);
                    }
                }
                _ => {}
            }
        }
        params
    }

    /// Undo [`Self::register_fn_params`]; the `fn_forward_resolutions`
    /// snapshot stays for the kernel cache fingerprint.
    fn unregister_fn_params(ctx: &mut CompileCtx<R, E>, mut params: FnParams) {
        for id in params.binds.drain(..) {
            ctx.bind_to_lambda.remove(&id);
        }
        for id in params.args.drain(..) {
            ctx.fn_param_args.remove(&id);
        }
        for (k, prev) in params.formals.drain(..).rev() {
            match prev {
                Some(id) => ctx.instance_fn_params.insert(k, id),
                None => ctx.instance_fn_params.remove(&k),
            };
        }
    }

    /// Take this call's argument nodes in source order under synthesized
    /// names (`#a<i>`, or `#s` for the argument at `self_key`). Returns the
    /// `(name, node)` pairs and the `(label, name)` list a synthesized
    /// call spells them with.
    pub(super) fn take_operands(
        &mut self,
        self_key: Option<&ArgKey>,
    ) -> Result<(
        LPooled<Vec<(ArcStr, Node<R, E>)>>,
        LPooled<Vec<(Option<ArcStr>, ArcStr)>>,
    )> {
        let ExprKind::Apply(ApplyExpr { args, .. }) = &self.spec.kind else {
            bail!("call site without an apply spec: {}", self.spec)
        };
        let mut operands: LPooled<Vec<(ArcStr, Node<R, E>)>> = LPooled::take();
        let mut names: LPooled<Vec<(Option<ArcStr>, ArcStr)>> = LPooled::take();
        for (i, ((label, _), key)) in
            args.iter().zip(ArgKey::of_written(args)).enumerate()
        {
            let name: ArcStr = if Some(&key) == self_key {
                literal!("#s")
            } else {
                format_compact!("#a{i}").as_str().into()
            };
            let Some(node) = self.args.get_mut(&key).and_then(|a| a.node.take()) else {
                bail!("call site argument {i} has no node: {}", self.spec)
            };
            operands.push((name.clone(), node));
            names.push((label.clone(), name));
        }
        Ok((operands, names))
    }

    /// `node` as this call's lowering, which stands for it from now on
    /// ([`CallNode`]): the function node and any remaining argument
    /// nodes are discarded.
    pub(super) fn lowering(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        node: Node<R, E>,
    ) -> Result<Node<R, E>> {
        wrap!(node, self.rtype.check_contains(&ctx.env, node.typ()))?;
        for arg in self.args.values_mut() {
            if let Some(n) = arg.node.take() {
                ctx.discard(n);
            }
        }
        for n in self.arg_refs.drain(..) {
            ctx.discard(n);
        }
        let old = mem::replace(&mut self.fnode, Node::new(Nop { typ: Type::Bottom }));
        ctx.discard(old);
        Ok(node)
    }

    /// Re-point this call's function node at binding `bind`.
    pub(super) fn retarget(&mut self, ctx: &mut CompileCtx<R, E>, bind: BindId) {
        let typ = ctx
            .env
            .by_id
            .get(&bind)
            .map(|b| b.typ.clone())
            .unwrap_or_else(Type::empty_tvar);
        let fspec = match &self.spec.kind {
            ExprKind::Apply(a) => (*a.function).clone(),
            _ => (*self.spec).clone(),
        };
        let fnode = Ref::new(ctx, bind, typ, self.top_id, fspec);
        let old = mem::replace(&mut self.fnode, fnode);
        ctx.discard(old);
    }
}

impl<R: Rt, E: UserEvent> CallSite<R, E> {
    fn update_call(&mut self, ctx: &mut ExecCtx<'_, R, E>) -> &TagValue {
        let woke = self.slept.take();
        if woke
            && ctx.event.born()
            && let Some(f) = self.callee.apply_mut()
        {
            f.reborn(ctx)
        }
        if matches!(self.callee, Callee::Imaged { .. })
            && let Err(e) = self.materialize(ctx)
        {
            warn!("decoding the instance of {}: {e:#}; resolving it afresh", self.spec);
            self.callee = Callee::DynamicUnbound;
        }
        // a first bind reads every quiet production; a bound site's rebind
        // finds its arguments' standing values in the store, which its
        // first bind stood them in, and compiles its defaults afresh; a
        // constant function never rebinds
        let keep = match &self.callee {
            Callee::Static { first_update: true, .. } => Keep::Every,
            Callee::Static { .. } => Keep::Nothing,
            Callee::DynamicBound { .. }
                if matches!(self.fnode.view(), NodeView::Constant(_)) =>
            {
                Keep::Nothing
            }
            Callee::DynamicBound { .. } => Keep::Stored,
            _ => Keep::Every,
        };
        let root = if woke { QuietAtRoot::Stand } else { QuietAtRoot::Skip };
        let pass = Pass { keep, root };
        let ArgsOut { fired: arg_fired, prods, mut set } =
            update_args(ctx, self.args.as_mut_slice(), &mut self.fork, pass);
        // `fnode.update` runs every cycle for its effects; a `Static`
        // callee discards the value.
        let static_callee = matches!(self.callee, Callee::Static { .. });
        let (fnode_tag, fnode_value) = {
            let tv = self.fnode.update(ctx);
            let tag = tv.tag();
            (tag, (!static_callee && !tag.is_bottom()).then(|| tv.value_cloned()))
        };
        if fnode_tag.is_bottom() && !static_callee {
            // a bound instance sleeps while its callee is bottom, so it wakes
            // with catch-up when the same function returns
            if let Some(f) = self.callee.apply_mut() {
                let absent = self.absent.get_or_insert_with(|| Absent {
                    missed: SmallVec::new(),
                    captured: TrackedFires::new(&ctx.env, 1, |_, r| f.refs(r)),
                });
                absent.observe(ctx, &set);
                f.sleep(ctx);
                self.slept.set();
            }
            for id in set.drain(..) {
                ctx.event.variables.remove(&id);
            }
            return self.resident.set_bottom(fnode_tag.triggers() || arg_fired);
        }
        let bound = if let Callee::Static { first_update, .. } = &mut self.callee {
            mem::replace(first_update, false)
        } else {
            fnode_value.is_some_and(|v| self.rebind(ctx, v, &mut set))
        };
        // a fresh bind reads its quiet arguments as new
        if bound {
            for arg in self.args.values() {
                if arg.node.is_none()
                    || !arg.stage.runs()
                    || ctx.event.variables.contains_key(&arg.id)
                {
                    continue;
                }
                let tv = match prods.iter().find(|(id, _)| *id == arg.id) {
                    Some((_, tv)) => tv.clone(),
                    None if keep == Keep::Stored && !arg.stage.is_default() => {
                        let Some((tv, _)) = ctx.rt.store_get(&arg.id) else { continue };
                        TagValue::tagged(tv.value_cloned(), tv.tag().quiet())
                    }
                    None => continue,
                };
                if tv.tag().is_bottom() {
                    continue;
                }
                let root = match arg.stage.is_default() {
                    true => QuietAtRoot::Deliver,
                    false => QuietAtRoot::Stand,
                };
                if publish_production(ctx, Feeds::Id(arg.id), &tv, true, root) {
                    set.push(arg.id);
                }
            }
        }
        if dbgenv::gxdbg_cs() {
            let kind = match self.callee.apply() {
                None => "none",
                Some(a) => match a.view() {
                    ApplyView::Lambda(_) => "lambda",
                    ApplyView::BuiltIn(_) => "builtin",
                    ApplyView::Opaque => "opaque",
                },
            };
            eprintln!(
                "CS spec={} bound={bound} kind={kind} argfired={arg_fired}",
                self.spec,
            );
        }
        let absent = self.absent.take();
        let returned = absent.is_some();
        let res = match self.callee.apply_mut() {
            None => None,
            Some(f) if !bound => {
                let arg_refs = &mut self.arg_refs;
                match absent {
                    None => Some(f.update(ctx, arg_refs).clone()),
                    // the same function back: its instance wakes, caught up
                    Some(absent) => {
                        Some(absent.wake(ctx, |ctx| f.update(ctx, arg_refs).clone()))
                    }
                }
            }
            Some(f) => {
                // A fresh bind dispatches under the init view.
                let arg_refs = &mut self.arg_refs;
                Some(ctx.under(View::Birth, |ctx| f.update(ctx, arg_refs).clone()))
            }
        };
        if dbgenv::gxdbg_cs() {
            eprintln!(
                "CS-RES spec={} res={:?}",
                self.spec,
                res.as_ref().map(|tv| tv.tag()),
            );
        }
        for id in set.drain(..) {
            ctx.event.variables.remove(&id);
        }
        match res {
            // a function back from bottom fires the call with its current
            // value, as a scrutinee back from bottom fires its select
            Some(tv) if returned => {
                let tag = tv.tag().fresh();
                self.resident.set(tv);
                self.resident.retag(tag)
            }
            Some(tv) => self.resident.set(tv),
            None if matches!(self.callee, Callee::Failed { .. }) => {
                self.resident.set_bottom(fnode_tag.triggers() || arg_fired)
            }
            None => self.resident.ride(),
        }
    }

    /// Bind the definition `v` unless it is the one bound, or the one
    /// whose bind failed; true when a fresh callee was bound.
    fn rebind(
        &mut self,
        ctx: &mut ExecCtx<'_, R, E>,
        v: Value,
        set: &mut Published,
    ) -> bool {
        let same = match &self.callee {
            Callee::DynamicBound { def, .. } | Callee::Failed { def } => def == &v,
            Callee::Prebound { def, .. } if def == &v => {
                let Callee::Prebound { def, apply, defaults } =
                    mem::replace(&mut self.callee, Callee::DynamicUnbound)
                else {
                    unreachable!("matched above")
                };
                self.callee = Callee::DynamicBound { def, apply };
                self.prime_bound(ctx, &defaults, set);
                return true;
            }
            _ => false,
        };
        if same {
            return false;
        }
        let Some(lb) = v.downcast_ref::<LambdaDef<R, E>>() else {
            panic!("value {v:?} is not a function")
        };
        match self.bind(ctx, v.clone(), lb, set) {
            Ok(()) => true,
            Err(e) => {
                let msg =
                    format_compact!("{}: binding the callee failed: {e:#}", self.spec);
                super::error::report_failure!(&msg);
                self.callee = Callee::Failed { def: v };
                false
            }
        }
    }
}

/// How a site's callee travels: unbound (a dynamic site before its
/// first cycle), an imaged instance of a lambda, or a builtin's own
/// image, decoded by its registered decoder over the imaged arguments.
const CALLEE_UNBOUND: u8 = 0;
const CALLEE_INSTANCE: u8 = 1;
const CALLEE_BUILTIN: u8 = 2;

impl<R: Rt, E: UserEvent> Arg<R, E> {
    fn image_encode(&self, key: &ArgKey, buf: &mut ImageBuf) -> Result<(), PackError> {
        key.encode(buf)?;
        self.id.encode(buf)?;
        opt_node_encode(self.node.as_ref(), buf)?;
        buf.put_u8(match self.stage {
            ArgStage::Given => 0,
            ArgStage::Placeholder => 1,
            ArgStage::Compiled => 2,
            ArgStage::Running => 3,
        });
        Ok(())
    }

    fn image_decode(
        ctx: &mut ExecCtx<'_, R, E>,
        buf: &mut &[u8],
    ) -> Result<(ArgKey, Self), PackError> {
        let key = ArgKey::decode(buf)?;
        let id = BindId::decode(buf)?;
        let node = opt_node_decode(ctx, buf)?;
        let stage = match u8::decode(buf)? {
            0 => ArgStage::Given,
            1 => ArgStage::Placeholder,
            2 => ArgStage::Compiled,
            3 => ArgStage::Running,
            _ => return Err(PackError::UnknownTag),
        };
        let spec = match &node {
            Some(n) => TArc::new(n.spec().clone()),
            None => NOP.clone(),
        };
        Ok((key, Arg { id, node, stage, spec }))
    }
}

impl RefsSummary {
    fn of_sets(
        refed: &IntSet<BindId>,
        triggering: &IntSet<BindId>,
        bound: &IntSet<BindId>,
    ) -> Self {
        let sorted = |ids: &IntSet<BindId>| {
            let mut v: Vec<BindId> = ids.iter().copied().collect();
            v.sort_unstable();
            v
        };
        RefsSummary {
            refed: sorted(refed),
            triggering: sorted(triggering),
            bound: sorted(bound),
        }
    }

    fn add_to(&self, refs: &mut Refs) {
        refs.refed.extend(self.refed.iter().copied());
        refs.triggering.extend(self.triggering.iter().copied());
        refs.bound.extend(self.bound.iter().copied());
    }
}

impl<R: Rt, E: UserEvent> CallSite<R, E> {
    fn callee_mode(&self) -> Result<u8, PackError> {
        match &self.callee {
            Callee::DynamicUnbound => Ok(CALLEE_UNBOUND),
            Callee::DynamicBound { .. }
            | Callee::Prebound { .. }
            | Callee::Failed { .. } => Err(PackError::Application(image::NOT_QUIESCENT)),
            Callee::Static { apply, .. } => match apply.view() {
                ApplyView::Lambda(_) => Ok(CALLEE_INSTANCE),
                ApplyView::BuiltIn(_) => Ok(CALLEE_BUILTIN),
                ApplyView::Opaque => Err(PackError::Application(image::NOT_IMAGED)),
            },
            Callee::Imaged { .. } => Err(PackError::Application(image::NOT_IMAGED)),
        }
    }

    /// Record what `apply`'s body reads, its callees by instance, once
    /// per instance however many sites write it.
    fn record_instance_refs(apply: &dyn Apply<R, E>) -> Result<(), PackError> {
        let ApplyView::Lambda(g) = apply.view() else {
            return Err(PackError::Application(image::NOT_IMAGED));
        };
        let instance = g.instance_id();
        let known = image::encoding(|e| {
            e.ext::<image::Compiled>().instance_refs.contains_key(&instance)
        })
        .ok_or(PackError::Application(image::NOT_IMAGED))?;
        if !known {
            let mut refs = Refs::stopping_at_callees();
            apply.refs(&mut refs);
            let callees = refs.take_callees();
            let own = RefsSummary::of_sets(&refs.refed, &refs.triggering, &refs.bound);
            image::encoding(|e| {
                e.ext::<image::Compiled>()
                    .instance_refs
                    .insert(instance, InstanceRefs { own, callees })
            });
        }
        Ok(())
    }

    /// Release what this site registered for an imaged tree.
    fn release_held(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        if let Some(held) = self.held.take() {
            for id in held.iter() {
                let reader = ctx.reader(self.top_id);
                ctx.rt.unref_var(*id, reader)
            }
        }
    }

    /// Decode the instance the image holds for this site and bind it.
    fn materialize(&mut self, ctx: &mut ExecCtx<'_, R, E>) -> Result<()> {
        let Callee::Imaged { instance, .. } = &self.callee else { return Ok(()) };
        let instance = *instance;
        let shared =
            ctx.image_decoder.get().cloned().ok_or_else(|| {
                anyhow!("no image to decode instance {instance:?} from")
            })?;
        let mut dec = shared.lock();
        let mark = ctx.fusion.restored_mark();
        let at = dec
            .ext_ref::<image::Restored>()
            .and_then(|r| r.instances.get(&instance).copied());
        let decoded = match at {
            None => Err(anyhow!("instance {instance:?} is not in the image")),
            Some(at) => {
                let image = dec.image().clone();
                image::DecodeImage::with(&mut dec, || {
                    let Some(mut sub) = image.get(at as usize..) else {
                        bail!("instance {instance:?} at {at} is past the image")
                    };
                    GXLambda::image_decode(ctx, &mut sub)
                        .map_err(|e| anyhow!("instance {instance:?} at {at}: {e:?}"))
                })
            }
        };
        // an instance whose kernels do not install is deleted, not dropped
        let decoded = decoded.and_then(|mut g| match ctx.fusion.install_restored() {
            Ok(()) => Ok(g),
            Err(e) => {
                Apply::delete(&mut g, ctx);
                Err(e)
            }
        });
        drop(dec);
        let apply: Box<dyn Apply<R, E>> = match decoded {
            Ok(g) => {
                ctx.apply_deferred();
                self.release_held(ctx);
                Box::new(g)
            }
            Err(e) => {
                ctx.fusion.forget_restored_since(mark);
                ctx.drop_deferred();
                return Err(e);
            }
        };
        let Callee::Imaged { first_update, .. } =
            mem::replace(&mut self.callee, Callee::DynamicUnbound)
        else {
            unreachable!()
        };
        self.callee = Callee::Static { apply, first_update };
        Ok(())
    }

    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<'_, R, E>,
        buf: &mut &[u8],
    ) -> Result<Node<R, E>, PackError> {
        let spec = TArc::new(Expr::decode(buf)?);
        let ftype = Option::<FnType>::decode(buf)?;
        let rtype = Type::decode(buf)?;
        let fnode = decode_node(ctx, buf)?;
        let n = crate::image::count_decode(buf)?;
        let mut args = ArgMap::with_capacity_and_hasher(n, Default::default());
        for _ in 0..n {
            let (key, arg) = Arg::image_decode(ctx, buf)?;
            args.insert(key, arg);
        }
        if !buf.has_remaining() {
            return Err(PackError::BufferShort);
        }
        let (arg_refs, callee) = match buf.get_u8() {
            CALLEE_UNBOUND => (Vec::new(), Callee::DynamicUnbound),
            CALLEE_INSTANCE => {
                let arg_refs = decode_nodes(ctx, buf)?;
                let callee = match bool::decode(buf)? {
                    false => {
                        let apply: Box<dyn Apply<R, E>> =
                            Box::new(GXLambda::image_decode(ctx, buf)?);
                        Callee::Static { apply, first_update: bool::decode(buf)? }
                    }
                    true => {
                        let instance = LambdaInstanceId::decode(buf)?;
                        let first_update = bool::decode(buf)?;
                        let table = image::decoding(|d| {
                            d.ext::<image::Restored>().instance_refs.clone()
                        })
                        .ok_or(PackError::Application(image::NOT_IMAGED))?;
                        let refs = ImagedRefs { table, flat: OnceLock::new() };
                        Callee::Imaged { instance, first_update, refs }
                    }
                };
                (arg_refs, callee)
            }
            CALLEE_BUILTIN => {
                let arg_refs = decode_nodes(ctx, buf)?;
                let apply: Box<dyn Apply<R, E>> =
                    Box::new(BuiltInLambda::image_decode(ctx, &arg_refs, buf)?);
                (arg_refs, Callee::Static { apply, first_update: bool::decode(buf)? })
            }
            _ => return Err(PackError::UnknownTag),
        };
        let static_target = Option::<StaticCallTarget>::decode(buf)?;
        let recursive_edge = bool::decode(buf)?;
        let flags = image::flags_decode(buf)?;
        let scope = image::scope_decode(buf)?;
        let top_id = ExprId::decode(buf)?;
        let fork = ForkSite::image_decode(buf)?;
        let mut site =
            Self::unbound(spec, ftype, rtype, fnode, args, scope, flags, top_id);
        site.fork = fork;
        site.arg_refs = arg_refs;
        site.callee = callee;
        site.static_target = static_target;
        site.recursive_edge = AtomicBool::new(recursive_edge);
        // an imaged callee's reads schedule this site's boundary before its
        // body decodes, as a cold compile's registrations do; the site holds
        // them until the body decodes and registers its own (recorded, so a
        // failed read unwinds them)
        // CR claude for claude: [perf/medium] Every imaged site flattens and registers
        // its whole subtree's reads here (ImagedRefs::flat, recomputed per site).
        // materialize releases them, and each imaged site in the decoded body then
        // registers its own subtree again. A call chain of depth N costs O(N^2) at its
        // first dispatch, and a warm start is slower than compiling. Measured on a
        // debug build with --no-fusion: a 2000-function chain takes 0.85 s cold and
        // 3.36 s warm; 4000 takes 2.66 s cold and 14.7 s warm. Commit 966be26bf had
        // this chain at 75 ms warm before walk pruning removed its in_instance rule.
        // Fix: memoize the flat reads per instance in the shared table, and pass the
        // outer site's held set down at materialize instead of re-registering whole
        // subtrees per level; the right shape depends on how readers must sit per
        // boundary. The `held` field's doc still describes the old rule. probe:
        // design/review-2026-10-19/repro/c-callsite-warm-chain-quadratic.sh
        // (c-callsite-04)
        // 2026-10-23 claude: Readdressed to claude in the 2026-10-23 triage: no ruling
        // needed. Take the fix: memoize the flat reads per instance in the shared
        // table and pass the held set down at materialize.
        if let Callee::Imaged { instance, refs, .. } = &site.callee {
            let held: Box<[BindId]> =
                refs.flat(*instance).refed.iter().copied().collect();
            for id in held.iter() {
                ctx.record_ref(*id, top_id)
            }
            site.held = Some(held);
        }
        Ok(Node::new(CallNode::Call(site)))
    }
}

/// A call site as a node: the call, or, once its check lowered it (a
/// trait call over a union self, a self never produced), the node that
/// stands for it, with no call parts left to reach.
#[derive(Debug)]
pub(crate) enum CallNode<R: Rt, E: UserEvent> {
    Call(CallSite<R, E>),
    /// The call's lowering, under the call's own spec: errors and the
    /// attribute walk name the user's call.
    Lowered(Node<R, E>, TArc<Expr>),
}

impl<R: Rt, E: UserEvent> CallNode<R, E> {
    /// The call, unless it was lowered.
    pub(crate) fn call_mut(&mut self) -> Option<&mut CallSite<R, E>> {
        match self {
            Self::Call(c) => Some(c),
            Self::Lowered(..) => None,
        }
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for CallNode<R, E> {
    fn for_each_child<'a>(&'a self, f: &mut dyn FnMut(&'a Node<R, E>)) {
        match self {
            Self::Call(c) => {
                c.args.values().filter_map(|a| a.node.as_ref()).for_each(&mut *f);
                f(&c.fnode)
            }
            Self::Lowered(n, _) => n.for_each_child(f),
        }
    }

    fn for_each_child_mut(&mut self, f: &mut dyn FnMut(&mut Node<R, E>)) {
        match self {
            Self::Call(c) => {
                c.args.values_mut().filter_map(|a| a.node.as_mut()).for_each(&mut *f);
                f(&mut c.fnode)
            }
            Self::Lowered(n, _) => n.for_each_child_mut(f),
        }
    }

    /// A lowered call is imaged as its lowering, which decodes as itself.
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        match self {
            Self::Call(c) => c.image_encode(buf),
            Self::Lowered(n, _) => n.image_encode(buf),
        }
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R, E>) -> &TagValue {
        match self {
            Self::Call(c) => c.update(ctx),
            Self::Lowered(n, _) => n.update(ctx),
        }
    }

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        match self {
            Self::Call(c) => c.delete(ctx),
            Self::Lowered(n, _) => n.delete(ctx),
        }
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        match self {
            Self::Call(c) => c.sleep(ctx),
            Self::Lowered(n, _) => n.sleep(ctx),
        }
    }

    fn typ(&self) -> &Type {
        match self {
            Self::Call(c) => c.typ(),
            Self::Lowered(n, _) => n.typ(),
        }
    }

    fn spec(&self) -> &Expr {
        match self {
            Self::Call(c) => c.spec(),
            Self::Lowered(_, spec) => spec,
        }
    }

    fn typecheck0(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
        match self {
            Self::Call(c) => c.typecheck0(ctx),
            Self::Lowered(n, _) => n.typecheck0(ctx),
        }
    }

    fn typecheck0_instance(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        types: &mut InstanceTypes,
    ) -> Result<()> {
        match self {
            Self::Call(c) => c.typecheck0_instance(ctx, types),
            // a lowering is built by elaboration: no table types it
            Self::Lowered(n, _) => n.typecheck0(ctx),
        }
    }

    fn typecheck1(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
        match self {
            Self::Call(c) => {
                if let Some(lowered) = c.typecheck1(ctx)? {
                    let spec = c.spec.clone();
                    *self = Self::Lowered(lowered, spec);
                }
                Ok(())
            }
            Self::Lowered(n, _) => n.typecheck1(ctx),
        }
    }

    fn refs(&self, refs: &mut Refs) {
        match self {
            Self::Call(c) => c.refs(refs),
            Self::Lowered(n, _) => n.refs(refs),
        }
    }

    fn view(&self) -> NodeView<'_, R, E> {
        match self {
            Self::Call(c) => c.view(),
            Self::Lowered(n, _) => n.view(),
        }
    }

    fn fuse(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<Option<Node<R, E>>> {
        match self {
            Self::Call(c) => c.fuse(ctx),
            Self::Lowered(n, _) => n.fuse(ctx),
        }
    }

    fn emit_clif(&self, cx: &mut BodyCx) -> Result<CompiledExpr> {
        match self {
            Self::Call(c) => c.emit_clif(cx),
            Self::Lowered(n, _) => n.emit_clif(cx),
        }
    }
}

/// The call's half of [`CallNode`]'s `Update`.
impl<R: Rt, E: UserEvent> CallSite<R, E> {
    pub(crate) fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        let mode = self.callee_mode()?;
        put_tag(NodeTag::CallSite, buf);
        self.spec.encode(buf)?;
        self.ftype.encode(buf)?;
        self.rtype.encode(buf)?;
        self.fnode.image_encode(buf)?;
        encode_varint(self.args.len() as u64, buf);
        for (k, a) in self.args.iter() {
            a.image_encode(k, buf)?;
        }
        buf.put_u8(mode);
        match (&self.callee, mode) {
            (Callee::Static { apply, first_update }, CALLEE_INSTANCE) => {
                encode_nodes(&self.arg_refs, buf)?;
                let deferred =
                    image::encoding(|e| e.ext::<image::Compiled>().defer_instances)
                        .unwrap_or(false);
                deferred.encode(buf)?;
                if deferred {
                    let ApplyView::Lambda(g) = apply.view() else {
                        return Err(PackError::Application(image::NOT_IMAGED));
                    };
                    let instance = g.instance_id();
                    instance.encode(buf)?;
                    first_update.encode(buf)?;
                    Self::record_instance_refs(&**apply)?;
                    // The session borrows every node it encodes for its
                    // whole length; the heap is written before it ends.
                    let body: &'static dyn Apply<R, E> =
                        unsafe { mem::transmute::<&dyn Apply<R, E>, _>(&**apply) };
                    image::encoding(|e| {
                        e.ext::<image::Compiled>()
                            .deferred
                            .push((instance, Box::new(move |buf| body.image_encode(buf))))
                    });
                } else {
                    apply.image_encode(buf)?;
                    first_update.encode(buf)?;
                }
            }
            (Callee::Static { apply, first_update }, CALLEE_BUILTIN) => {
                encode_nodes(&self.arg_refs, buf)?;
                apply.image_encode(buf)?;
                first_update.encode(buf)?;
            }
            _ => (),
        }
        self.static_target.encode(buf)?;
        self.recursive_edge.load(Relaxed).encode(buf)?;
        image::flags_encode(self.flags, buf)?;
        image::scope_encode(&self.scope, buf)?;
        self.top_id.encode(buf)?;
        self.fork.image_encode(buf)
    }

    pub(crate) fn update(&mut self, ctx: &mut ExecCtx<'_, R, E>) -> &TagValue {
        self.update_call(ctx)
    }

    pub(crate) fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.release_held(ctx);
        if let Some(mut f) = self.callee.take_apply() {
            f.delete(ctx)
        }
        self.fnode.delete(ctx);
        for arg in self.args.values_mut() {
            ctx.rt.store_release(&arg.id);
            if let Some(ref mut n) = arg.node {
                n.delete(ctx);
            }
        }
        for n in &mut self.arg_refs {
            n.delete(ctx);
        }
    }

    pub(crate) fn sleep(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        self.slept.set();
        // A recursive edge deselected by a shrink is deleted, so
        // re-reaching this depth binds a fresh activation.
        if super::in_deselected_arm() && self.is_recursive_edge() {
            if let Some(mut f) = self.callee.take_apply() {
                f.delete(ctx)
            }
        } else if let Some(f) = self.callee.apply_mut() {
            f.sleep(ctx)
        }
        self.fnode.sleep(ctx);
        for arg in self.args.values_mut() {
            if let Some(ref mut n) = arg.node {
                n.sleep(ctx);
            }
        }
        for n in &mut self.arg_refs {
            n.sleep(ctx);
        }
    }

    pub(crate) fn typ(&self) -> &Type {
        &self.rtype
    }

    pub(crate) fn spec(&self) -> &Expr {
        &self.spec
    }

    pub(crate) fn typecheck0(&mut self, ctx: &mut CompileCtx<R, E>) -> Result<()> {
        wrap!(self.fnode, self.fnode.typecheck0(ctx))?;
        let mut fresh = false;
        let ftype = match self.ftype.as_ref() {
            Some(ftype) => ftype, // already initialized
            None => {
                let ftype = deref_typ!("fn", ctx, self.fnode.typ(),
                    Some(Type::Fn(ftype)) => Ok(ftype.clone())
                )?;
                // A self-call inside the def-time body check unifies against
                // the def's own cells (`ExecCtx::rec_defs`).
                let is_rec_self_call = !ctx.rec_defs.is_empty()
                    && ftype.lambda_ids.ids().iter().any(|id| ctx.rec_defs.contains(id));
                let identity = self.fn_arg_identity(ctx);
                let active_ftype = ftype
                    .lambda_ids
                    .own()
                    .and_then(|id| ctx.resolving(id, &identity))
                    .map(|active| active.ftype);
                // A call to the enclosing def's fn-typed param during its gate.
                let is_param_knot = !ctx.def_gate_params.is_empty()
                    && matches!(
                        self.fnode.view(),
                        NodeView::Ref(r) if ctx.def_gate_params.contains_key(&r.id)
                    );
                let ftype = if let Some(active) = active_ftype {
                    active
                } else if is_rec_self_call {
                    // A shallow clone shares the def's TVar cells.
                    (*ftype).clone()
                } else if is_param_knot {
                    ftype.shared_call()
                } else {
                    fresh = true;
                    ftype.instantiate(&ctx.rec_defs)
                };
                fill_omitted(&mut self.args, &ftype)?;
                self.ftype = Some(TArc::new(ftype));
                self.ftype.as_ref().unwrap()
            }
        };
        let mut widening = Widening::new(fresh);
        let mut formals: SmallVec<[_; 8]> =
            ftype.args.iter().zip(ArgKey::of_formals(&ftype.args)).collect();
        formals.sort_by_key(|(farg, _)| !holds_invariantly(&farg.typ));
        for (farg, key) in formals {
            if let Some(n) = self.args.get_mut(&key).and_then(|a| a.node.as_mut()) {
                let waits = widening.waits(self.fnode.typ(), ftype, &farg.typ);
                let hint = widening.hint(self.fnode.typ(), ftype, &farg.typ);
                let fit = typecheck_arg(ctx, &farg.typ, hint.as_ref(), waits, n)?;
                widening.fit(
                    &ctx.env,
                    self.fnode.typ(),
                    ftype,
                    &key,
                    &farg.typ,
                    n,
                    fit,
                )?;
            }
        }
        if let Some(typ) = &ftype.vargs {
            let positional = ftype.args.iter().filter(|a| a.is_positional()).count();
            for key in ArgKey::variadic(positional) {
                let Some(arg) = self.args.get_mut(&key) else { break };
                if let Some(n) = arg.node.as_mut() {
                    let waits = widening.waits(self.fnode.typ(), ftype, typ);
                    let hint = widening.hint(self.fnode.typ(), ftype, typ);
                    let fit = typecheck_arg(ctx, typ, hint.as_ref(), waits, n)?;
                    widening.fit(&ctx.env, self.fnode.typ(), ftype, &key, typ, n, fit)?;
                }
            }
        }
        for (key, formal) in widening.deferred.drain(..) {
            if let Some(n) = self.args.get(&key).and_then(|a| a.node.as_ref()) {
                wrap!(n, formal.check_contains(&ctx.env, n.typ()))?;
            }
        }
        self.check_value_defaults(ctx, ftype)?;
        if fresh {
            let mut carriers = SmallVec::new();
            self.defaults_open =
                !self.check_omitted_defaults(ctx, ftype, &mut carriers)?;
            if !self.defaults_open && static_builtin(ctx, self).is_some() {
                self.settle_unread(&ctx.env, ftype, &carriers);
            }
        }
        // A constrained cell reachable from the rtype/throws but no arg is
        // produced by the callee's body: settle it to its witness before an
        // annotation could narrow it unsoundly. Its writers are not checked
        // yet, so a ⊥-fed cell is not ⊥ here. A quantifier, the callee's own
        // or of a function type the signature holds, is its callers' to pick.
        {
            let mut quantifiers: LPooled<AHashMap<usize, TVar>> = LPooled::take();
            ftype.inner_quantifiers(&mut quantifiers);
            let mut arg_tvs: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
            for a in ftype.args.iter() {
                a.typ.collect_tvars(&mut arg_tvs);
            }
            if let Some(t) = &ftype.vargs {
                t.collect_tvars(&mut arg_tvs);
            }
            let arg_cells: LPooled<AHashSet<usize>> =
                arg_tvs.drain().map(|(_, tv)| tv.cell_addr()).collect();
            let mut rt_tvs: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
            ftype.rtype.collect_tvars(&mut rt_tvs);
            ftype.throws.collect_tvars(&mut rt_tvs);
            for (name, tv) in rt_tvs.drain() {
                let addr = tv.cell_addr();
                // the callee's declared quantifiers were rigid in its check, so
                // its body produced nothing into them: the caller narrows them
                let declared = ftype.quantifiers.contains(&name);
                if !arg_cells.contains(&addr)
                    && !quantifiers.contains_key(&addr)
                    && !declared
                {
                    wrap!(self, tv.settle_witness(&ctx.env))?;
                }
            }
        }
        self.raise_throws(ctx, ftype, true)?;
        wrap!(self.fnode, self.rtype.check_contains(&ctx.env, &ftype.rtype))?;
        // the check settles before elaboration (typecheck1) runs; a
        // definition's check runs no typecheck1, its gate's frame hands the
        // settle to the enclosing statement
        let settle = self.pending_settle(ftype);
        ctx.pending_settles.last_mut().expect("settle frame").push(settle);
        Ok(())
    }

    pub(crate) fn typecheck0_instance(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        types: &mut InstanceTypes,
    ) -> Result<()> {
        let ftype = match self.ftype.is_none() {
            true => types.ftype(self.spec.id),
            false => None,
        };
        let Some(ftype) = ftype else {
            if self.ftype.is_none() {
                super::lambda::elab_audit::fallback(&self.spec);
            }
            return self.typecheck0(ctx);
        };
        wrap!(self.fnode, self.fnode.typecheck0_instance(ctx, types))?;
        fill_omitted(&mut self.args, &ftype)?;
        // a lambda argument learns its parameters' types from the formal
        for (farg, key) in ftype.args.iter().zip(ArgKey::of_formals(&ftype.args)) {
            if let Some(n) = self.args.get_mut(&key).and_then(|a| a.node.as_mut()) {
                Type::pre_unify_arg(&ctx.env, &farg.typ, n.typ())?;
                wrap!(n, n.typecheck0_instance(ctx, types))?;
                // a callback formal's other cells (its return) are decided
                // here, in the instance's task, not by its elaboration
                if farg.typ.with_deref(|t| matches!(t, Some(Type::Fn(_)))) {
                    wrap!(n, farg.typ.check_contains(&ctx.env, n.typ()))?;
                }
            }
        }
        if let Some(typ) = &ftype.vargs {
            let positional = ftype.args.iter().filter(|a| a.is_positional()).count();
            for key in ArgKey::variadic(positional) {
                let Some(arg) = self.args.get_mut(&key) else { break };
                if let Some(n) = arg.node.as_mut() {
                    Type::pre_unify_arg(&ctx.env, typ, n.typ())?;
                    wrap!(n, n.typecheck0_instance(ctx, types))?;
                }
            }
        }
        self.raise_throws(ctx, &ftype, false)?;
        if !types.settle(self.spec.id, &self.rtype) {
            wrap!(self.fnode, self.rtype.check_contains(&ctx.env, &ftype.rtype))?;
        }
        self.ftype = Some(TArc::new(ftype));
        Ok(())
    }

    /// Second pass: after the subtrees, drive `Apply::typecheck1` for every
    /// lambda dispatchable here (the callee and each fn-typed callback). A
    /// trait call it resolves may lower to another node, which then
    /// stands for the call.
    pub(crate) fn typecheck1(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
    ) -> Result<Option<Node<R, E>>> {
        wrap!(self.fnode, self.fnode.typecheck1(ctx))?;
        for arg in self.args.values_mut() {
            if let Some(n) = arg.node.as_mut() {
                wrap!(n, n.typecheck1(ctx))?;
            }
        }
        let ftype = match self.ftype.as_ref() {
            Some(ftype) => ftype.clone(),
            None => return Ok(None),
        };
        // A settle frame for this site's re-drives; leftovers merge up and
        // drain only after this site's writers have run.
        ctx.pending_settles.push(crate::SettleFrame::default());
        let res = self.typecheck1_resolve(ctx);
        let leftover = ctx.pending_settles.pop().expect("settle frame");
        ctx.pending_settles.last_mut().expect("root settle frame").append(leftover);
        if let Some(lowered) = res? {
            return Ok(Some(lowered));
        }
        let settle = self.pending_settle(&ftype);
        ctx.pending_settles.last_mut().expect("root settle frame").push(settle);
        Ok(None)
    }

    pub(crate) fn refs(&self, refs: &mut Refs) {
        if !refs.skip_callees {
            if let Some(fun) = self.callee.apply() {
                match (fun.view(), refs.callees.as_mut()) {
                    (ApplyView::Lambda(g), Some(callees)) => {
                        callees.push((g.instance_id(), refs.banked > 0))
                    }
                    _ => fun.refs(refs),
                }
            }
            if let Callee::Imaged { instance, refs: imaged, .. } = &self.callee {
                imaged.flat(*instance).add_to(refs);
            }
        }
        self.fnode.refs(refs);
        for arg in self.args.values() {
            refs.bound.insert(arg.id);
            if let Some(ref n) = arg.node {
                n.refs(refs);
            }
        }
        for n in &self.arg_refs {
            n.refs(refs);
        }
    }

    pub(crate) fn view(&self) -> NodeView<'_, R, E> {
        NodeView::CallSite(self)
    }

    pub(crate) fn fuse(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
    ) -> Result<Option<Node<R, E>>> {
        // Reached when this call did not inline: fuse its args in source
        // order, then give the callee its hook.
        fusion::fuse_parts(self.args.values_mut().filter_map(|a| a.node.as_mut()), ctx)?;
        if let Some(apply) = self.callee.apply_mut() {
            apply.fuse(ctx)?;
        }
        Ok(None)
    }

    pub(crate) fn emit_clif(&self, cx: &mut BodyCx) -> Result<CompiledExpr> {
        if let Some(f) = self.callee.apply() {
            if let Some(cv) = f.emit_clif(self, cx)? {
                return Ok(cv);
            }
            // A resolved lambda callee is a cross-kernel call; an
            // undiscovered site de-fuses.
            if matches!(f.view(), ApplyView::Lambda(_)) {
                if let Some(info) = cx.lambda_site(self.spec.id).cloned() {
                    return emit_lambda_call_node(cx, self, Called::Other(&info));
                }
                return Err(fusion::blocker(
                    &self.spec,
                    format_compact!(
                        "emit_clif: lambda call site `{}` not discovered — \
                         subtree node-walks",
                        self.spec
                    ),
                ));
            }
        }
        // A value-position self-call: `self.callee` is unresolved here, so
        // match by the self BindId and call the kernel's own FuncRef.
        if let Some(own) = cx.self_call()
            && matches!(self.fnode.view(), NodeView::Ref(r) if r.id == own.bind)
        {
            return emit_lambda_call_node(cx, self, Called::Own(own));
        }
        if self.is_recursive_edge() {
            bail!("emit_clif: mutually recursive static call edge is not supported")
        }
        // A `MarshalArg::Call(i)` indexes the source-order arg list, which
        // spans labeled and positional args; `Default(name)` is this site's
        // compiled default node.
        let info = match cx.builtin_site(self.spec.id) {
            Some(info) => info.clone(),
            None => {
                return Err(fusion::blocker(
                    &self.spec,
                    format_compact!(
                        "emit_clif: builtin call site `{}` not discovered — doesn't fuse",
                        self.spec
                    ),
                ));
            }
        };
        let written = self.spec_args();
        let source_nodes = ArgKey::of_written(written)
            .map(|key| {
                self.arg(&key)
                    .ok_or_else(|| anyhow!("emit_clif: missing call-site arg node"))
            })
            .collect::<Result<SmallVec<[&Node<R, E>; 8]>>>()?;
        let arg_nodes = info
            .marshal_args
            .iter()
            .map(|m| match m {
                MarshalArg::Call(call_idx) => {
                    source_nodes.get(*call_idx).copied().ok_or_else(|| {
                        anyhow!("emit_clif: marshal arg index {call_idx} out of range")
                    })
                }
                MarshalArg::Default(name) => self.arg_named(name).ok_or_else(|| {
                    anyhow!("emit_clif: defaulted arg `{name}` has no compiled node")
                }),
            })
            .collect::<Result<SmallVec<[_; 8]>>>()?;
        let order = self.update_order(
            &arg_nodes.iter().map(|n| Some(*n)).collect::<SmallVec<[_; 8]>>(),
        );
        emit_builtin_call_node(cx, &info, &arg_nodes, &order)
    }
}

/// The bind ids a production feeds: one call argument's id, or a
/// formal's pattern ids.
pub(crate) enum Feeds<'a> {
    Id(BindId),
    Pattern(&'a StructPatternNode),
}

/// What updating a call's arguments left: whether one fired, the
/// productions a fresh bind replays, and the argument ids published
/// this cycle.
#[derive(Default)]
struct ArgsOut {
    fired: bool,
    prods: SmallVec<[(BindId, TagValue); 4]>,
    set: Published,
}

/// A bound instance asleep while its function is bottom: what fired that
/// it did not see, caught up when the same function returns, as a
/// deselected arm is at its wake.
#[derive(Debug)]
struct Absent {
    /// The arguments that fired meanwhile.
    missed: SmallVec<[BindId; 2]>,
    /// The inputs the instance captures.
    captured: TrackedFires,
}

impl Absent {
    /// Record this cycle's fires: the arguments published in `set`, and
    /// the captured inputs.
    fn observe<R: Rt, E: UserEvent>(&mut self, ctx: &ExecCtx<'_, R, E>, set: &Published) {
        for id in set.iter() {
            let fired = ctx.event.variables.get(id).is_some_and(|tv| tv.tag().triggers());
            if fired && !self.missed.contains(id) {
                self.missed.push(*id);
            }
        }
        self.captured.observe(ctx);
    }

    /// Run `update` under the wake view with one catch-up fire for each
    /// argument and captured input that fired unseen and is not delivered
    /// live this cycle, at its current value.
    fn wake<R: Rt, E: UserEvent, T>(
        mut self,
        ctx: &mut ExecCtx<'_, R, E>,
        update: impl FnOnce(&mut ExecCtx<'_, R, E>) -> T,
    ) -> T {
        let mut injected = super::wake::Injected::new();
        for id in self.missed.drain(..) {
            let live = ctx.event.variables.get(&id).is_some_and(|tv| tv.tag().triggers());
            if !live && let Some((tv, _)) = ctx.rt.store_get(&id) {
                let catch_up = TagValue::tagged(tv.value_cloned(), Tag::FIRED);
                injected.push((id, ctx.event.variables.put(id, catch_up)));
            }
        }
        let captured = &mut self.captured;
        let r = ctx.under(View::Wake, |ctx| {
            let caught_up = captured.deliver(ctx, 0);
            let r = update(ctx);
            TrackedFires::restore(ctx.event, caught_up);
            r
        });
        TrackedFires::restore(ctx.event, injected);
        r
    }
}

/// Which quiet productions an update pass keeps for a bind to read.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Keep {
    Every,
    /// None: a rebind reads the given arguments from the store, and its
    /// defaults compile afresh.
    Stored,
    Nothing,
}

/// The argument ids a dispatch published on the overlay.
type Published = SmallVec<[BindId; 4]>;

/// How an update pass treats its arguments.
#[derive(Clone, Copy)]
struct Pass {
    keep: Keep,
    root: QuietAtRoot,
}

/// Update `args` and publish each production on its argument's id into
/// `out`, in order or forked where the site's plan says.
fn update_args<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<'_, R, E>,
    args: &mut indexmap::map::Slice<ArgKey, Arg<R, E>>,
    site: &mut ForkSite,
    pass: Pass,
) -> ArgsOut {
    let n = args.len();
    site.decide_siblings(ctx, n, args.values().filter_map(|a| a.node.as_ref()));
    let in_order = |c: &mut ExecCtx<'_, R, E>, p, m: Option<&mut Meter<'_>>| {
        update_args_in_order(c, p, m, pass)
    };
    crate::branch::fork_point(ctx, args, site, in_order, |mut a, b| {
        a.fired |= b.fired;
        a.prods.extend(b.prods);
        a.set.extend(b.set);
        a
    })
}

fn update_args_in_order<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<'_, R, E>,
    args: &mut indexmap::map::Slice<ArgKey, Arg<R, E>>,
    mut meter: Option<&mut Meter<'_>>,
    pass: Pass,
) -> ArgsOut {
    let mut out = ArgsOut::default();
    let Pass { keep, root } = pass;
    for (i, arg) in args.values_mut().enumerate() {
        let Some(node) = &mut arg.node else { continue };
        if !arg.stage.runs() {
            continue;
        }
        let tv = timed(&mut meter, i, || node.update(ctx));
        let fired = tv.tag().triggers();
        out.fired |= fired;
        let kept = match keep {
            Keep::Every => true,
            Keep::Stored | Keep::Nothing => false,
        };
        if kept && !fired {
            out.prods.push((arg.id, tv.clone()));
        }
        if publish_production(ctx, Feeds::Id(arg.id), tv, false, root) {
            out.set.push(arg.id);
        }
    }
    out
}

/// What a quiet production does; the store serves the value channel.
#[derive(Clone, Copy)]
pub(crate) enum QuietAtRoot {
    /// Nothing: the store already holds it.
    Skip,
    /// Stand it in the store: a wake refresh, a fresh formal's seed.
    Stand,
    /// Deliver it on the overlay.
    Deliver,
}

/// Publish `tv`, a production feeding `feeds`, to the readers of this
/// dispatch. A fire (a value or a fresh bottom) is stored and delivered
/// on the overlay; a quiet one per `root`. `born` delivers a quiet value FIRED: a fresh
/// callee's first dispatch reads its arguments as new. Returns whether
/// the overlay was written.
pub(crate) fn publish_production<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<'_, R, E>,
    feeds: Feeds<'_>,
    tv: &TagValue,
    born: bool,
    root: QuietAtRoot,
) -> bool {
    let tag = tv.tag();
    let bottom = tag.is_bottom();
    let delivered = if born && !bottom { Tag::FIRED } else { tag };
    let (store, overlay) = if tag.triggers() {
        let store = if bottom { Tag::FRESH_BOTTOM } else { Tag::FIRED };
        (Some((store, false)), Some(tag))
    } else {
        match root {
            QuietAtRoot::Skip => return false,
            QuietAtRoot::Stand => (Some((tag, true)), None),
            QuietAtRoot::Deliver => (None, Some(delivered)),
        }
    };
    let mut put = |id: BindId, v: Value| {
        match store {
            None => (),
            Some((t, false)) => ctx.rt.store_insert(id, TagValue::tagged(v.clone(), t)),
            Some((t, true)) => {
                ctx.rt.store_insert_standing(id, TagValue::tagged(v.clone(), t))
            }
        }
        if let Some(t) = overlay {
            ctx.publish(id, TagValue::tagged(v, t));
        }
    };
    match (feeds, bottom) {
        (Feeds::Id(id), true) => put(id, Value::Null),
        (Feeds::Id(id), false) => put(id, tv.value_cloned()),
        (Feeds::Pattern(pat), true) => pat.ids(&mut |id| put(id, Value::Null)),
        (Feeds::Pattern(pat), false) => {
            let v = tv.value_cloned();
            pat.bind(&v, &mut |id, v| put(id, v))
        }
    }
    overlay.is_some()
}

/// The lambda literal an argument is: bare, or sampled (a seq step's
/// inline callback, `pc ~! |x| ..`), whose value is the literal's.
fn lambda_literal<R: Rt, E: UserEvent>(node: &Node<R, E>) -> Option<&Lambda> {
    match node.view() {
        NodeView::Lambda(l) => Some(l),
        NodeView::Sample(s) => match s.arg.node.view() {
            NodeView::Lambda(l) => Some(l),
            _ => None,
        },
        _ => None,
    }
}
