use super::{
    NOP, Nop, WakeBit,
    bind::Ref,
    compiler::compile,
    error::{Qop, join_open_raised, join_raised},
    lambda::{
        BuiltInLambda, GXLambda, InstanceTypes, Lambda, LambdaDef, same_parameters,
    },
    pattern::StructPatternNode,
    wake::{FreshFrame, TrackedFires},
};
use crate::{
    Apply, ApplyView, BindId, BindMode, CFlag, CompileCtx, ExecCtx, FnArgIdentity,
    LambdaId, LambdaInstanceId, Node, NodeView, PrintFlag, Refs, ResolvingLambda, Rt,
    Scope, StaticHold, Tag, TagValue, Update, View, analysis, bailat, dbgenv, deref_typ,
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
        ContainsFlags, FnArgKind, FnArgType, FnType, Mutability, ParamKey, TVar, Type,
        TypeMismatch,
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
    collections::hash_map::Entry,
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
fn reject_dead_variadic_call<R: Rt>(
    ctx: &CompileCtx<R>,
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
    /// The arguments' bindings the instance binds statically, which the
    /// site holds ([`CallSite::hold_static`]).
    held: SmallVec<[BindId; 2]>,
}

/// The call's argument nodes, keyed for signature lookups and iterating
/// in source order: the written arguments, then the defaults a bind
/// added. A forked update merges its parts in that order.
pub(crate) type ArgMap<R> = IndexMap<ArgKey, Arg<R>, ahash::RandomState>;

#[derive(Debug)]
pub(crate) struct Arg<R: Rt> {
    pub id: BindId,
    pub node: Option<Node<R>>,
    pub stage: ArgStage,
    /// The argument's expression, which a reference to it shows.
    pub spec: TArc<Expr>,
}

impl<R: Rt> Arg<R> {
    pub(crate) fn new(
        id: BindId,
        node: Option<Node<R>>,
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

/// Where a site judges a default. At the check, the default compiles in
/// its definition's environment, where a variable of an enclosing
/// definition whose check has closed is that definition's scheme, of which
/// the callee is one instance: the judgment takes a copy of it rather than
/// deciding it. At the bind, the environment is the instance's.
#[derive(Clone, Copy)]
enum Judge {
    Check,
    Bind,
}

/// A labeled default of `f` compiled in `f`'s environment, where its
/// names are, and judged in the site's, where the site's types are:
/// against the site's view of the label, which the default narrows and
/// the instance runs with; where the site hides the label, against `f`'s
/// own parameter, and then against the label of `value`, the function as
/// the site holds it, which it narrows.
#[allow(clippy::too_many_arguments)]
fn checked_default<R: Rt>(
    ctx: &mut CompileCtx<R>,
    flags: BitFlags<CFlag>,
    scope: &Scope,
    top_id: ExprId,
    f: &LambdaDef<R>,
    name: &ArcStr,
    expr: &Expr,
    defaults: &mut Refs,
    site: Option<&FnType>,
    value: Option<&FnType>,
    judge: Judge,
) -> Result<Node<R>> {
    // CR claude for claude: [bug/medium] A labeled default compiled at a bind is checked
    // in full here at the instance's types, so a select in it over an enclosing
    // definition's variable is judged for coverage there: `let outer = 'a: [(bool, i64),
    // i64] |x: 'a| -> i64 { let f = |#d = select x { (true, n) => n, (false, n) => 0 - n,
    // i64 as n => n * 10 }, y: i64| d + y; f(1) }; outer((true, 3))` is refused "cannot
    // compute [] * i64" (the arm `i64 as n` reaches nothing at (bool, i64)), while
    // `outer(v)` with `v: [(bool, i64), i64]` prints 51 and the same select in outer's body
    // rather than a default checks at both (lane E on 6441a6d70, main a084f9887 alike).
    // The definition's check owns the coverage verdicts, as for a GRAPHIX_NO_SUBST
    // re-check (no-subst-dead-arm-01). probe: ~/ptmp/cr29/scratch/r6-E/r6G2/ns3.gx
    // (default-bind-coverage-01)
    let (node, res) = ctx.with_restored(f.env.clone(), |ctx| {
        let local_scope =
            Scope { dynamic: scope.dynamic.clone(), lexical: f.scope.lexical.clone() };
        let mut node = f.in_signature(ctx, |ctx| {
            compile(ctx, flags, expr.clone(), &local_scope, top_id)
        })?;
        let res = node.typecheck0(ctx);
        Ok::<_, anyhow::Error>((node, res))
    })?;
    let label = |ft: &FnType| -> Option<Type> {
        ft.args.iter().find(|a| a.label() == Some(name)).map(|a| a.typ.clone())
    };
    let copy = match judge {
        Judge::Check if node.typ().has_unbound() => {
            Some(node.typ().instantiate_with(&mut LPooled::take(), &ctx.rec_defs))
        }
        Judge::Check | Judge::Bind => None,
    };
    let typ = copy.as_ref().unwrap_or(node.typ());
    let res = res.and_then(|()| match site.and_then(label) {
        Some(t) => t.check_contains(&ctx.env, typ),
        None => {
            let mut gates: LPooled<Vec<RigidGate>> = LPooled::take();
            for t in callee_param(ctx, f, name, site, &mut gates)
                .into_iter()
                .chain(value.and_then(label))
            {
                t.check_contains(&ctx.env, typ)?
            }
            Ok(())
        }
    });
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
fn judge_hidden_defaults<R: Rt>(
    ctx: &mut CompileCtx<R>,
    flags: BitFlags<CFlag>,
    scope: &Scope,
    top_id: ExprId,
    formal: &FnType,
    value: &FnType,
) -> Result<()> {
    for id in value.lambda_ids.ids().iter() {
        let def = ctx.lambda_defs.get(id).cloned();
        let Some(f) = def.as_ref().and_then(|v| v.downcast_ref::<LambdaDef<R>>()) else {
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
                Some(value),
                Judge::Check,
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
/// function at one place: a struct field, a tuple, variant, array, list,
/// map or reference part, through typedefs, and in a union the one
/// member built by the value's constructor.
/// What a call's formal, a `let`'s annotation, a `<-` target and a
/// declared return type hold.
pub(crate) fn check_hidden_defaults<R: Rt>(
    ctx: &mut CompileCtx<R>,
    flags: BitFlags<CFlag>,
    scope: &Scope,
    top_id: ExprId,
    declared: &Type,
    value: &Type,
    narrowing: Narrowing,
) -> Result<()> {
    if !may_hold_fn(declared, &mut LPooled::take())
        || !holds_defaulted(ctx, value, &mut LPooled::take())
    {
        return Ok(());
    }
    let visited = &mut LPooled::take();
    match narrowing {
        Narrowing::Site => {
            hidden_defaults_at(ctx, flags, scope, top_id, declared, value, 0, visited)
        }
        Narrowing::Copy => {
            let declared = declared.reset_tvars();
            hidden_defaults_at(ctx, flags, scope, top_id, &declared, value, 0, visited)
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

/// Whether a function whose definition has a labeled default stands in
/// `t` outside any function type, through bindings: what
/// [`hidden_defaults_at`] can judge. Each shared node is asked once
/// (`seen`).
fn holds_defaulted<R: Rt>(
    ctx: &CompileCtx<R>,
    t: &Type,
    seen: &mut AHashSet<usize>,
) -> bool {
    ensure_sufficient(|| match t {
        _ if t.shared_identity().is_some_and(|n| !seen.insert(n)) => false,
        Type::Fn(ft) => ft.lambda_ids.ids().iter().any(|id| {
            ctx.lambda_defs
                .get(id)
                .and_then(|v| v.downcast_ref::<LambdaDef<R>>())
                .is_some_and(|f| f.argspec.iter().any(|a| a.kind.default().is_some()))
        }),
        Type::TVar(tv) => tv.binding().is_some_and(|b| holds_defaulted(ctx, &b, seen)),
        // a typedef's body is a written type, whose functions were judged
        // where they entered it: only its parameters hold the value's own
        t => {
            let mut any = false;
            t.for_each_child(&mut |c| any = any || holds_defaulted(ctx, c, seen));
            any
        }
    })
}

/// Typedefs expand this deep at most on one path of the walk, so a
/// recursive one ends.
const MAX_EXPANSIONS: u8 = 16;

/// The pairs of shared nodes a hidden-default walk has walked, each
/// held for the walk so an address stays its node's.
type Visited = AHashMap<(usize, usize), (Type, Type)>;

/// `expanded` counts the typedefs expanded on the way here; `visited`
/// holds the pairs of shared nodes walked already, so a type that shares
/// a part is walked once per part.
#[allow(clippy::too_many_arguments)]
fn hidden_defaults_at<R: Rt>(
    ctx: &mut CompileCtx<R>,
    flags: BitFlags<CFlag>,
    scope: &Scope,
    top_id: ExprId,
    declared: &Type,
    value: &Type,
    expanded: u8,
    visited: &mut Visited,
) -> Result<()> {
    let Some(v) = value.deref_cloned() else { return Ok(()) };
    if let Type::Ref(_) = &v {
        if expanded >= MAX_EXPANSIONS || !holds_defaulted(ctx, &v, &mut LPooled::take()) {
            return Ok(());
        }
        return match v.lookup_ref_probe(&ctx.env) {
            Some((_, v)) => hidden_defaults_at(
                ctx,
                flags,
                scope,
                top_id,
                declared,
                &v,
                expanded + 1,
                visited,
            ),
            None => Ok(()),
        };
    }
    let mut expanded = expanded;
    let Some(d) = opened(&ctx.env, declared, &mut expanded) else { return Ok(()) };
    if let (Some(a), Some(b)) = (d.shared_identity(), v.shared_identity()) {
        match visited.entry((a, b)) {
            Entry::Occupied(_) => return Ok(()),
            Entry::Vacant(e) => e.insert((d.clone(), v.clone())),
        };
    }
    let mut at = |ctx: &mut CompileCtx<R>, d: &Type, v: &Type| {
        hidden_defaults_at(ctx, flags, scope, top_id, d, v, expanded, visited)
    };
    ensure_sufficient(|| match (&d, &v) {
        (Type::Fn(formal), Type::Fn(fv)) => {
            judge_hidden_defaults(ctx, flags, scope, top_id, formal, fv)
        }
        (Type::Set(_), _) | (_, Type::Set(_)) => {
            let mut formals: LPooled<Vec<Type>> = LPooled::take();
            union_members(&d, &mut formals, &mut |m| opened(&ctx.env, m, &mut 0));
            let mut values: LPooled<Vec<Type>> = LPooled::take();
            union_members(&v, &mut values, &mut |m| m.deref_cloned());
            values.iter().try_for_each(|v| {
                if let Type::Ref(_) = v {
                    return at(ctx, &d, v);
                }
                let mut holders = formals.iter().filter(|d| same_constructor(d, v));
                match (holders.next(), holders.next()) {
                    (Some(d), None) => at(ctx, d, v),
                    _ => Ok(()),
                }
            })
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

/// `t` through its cells and typedefs, probed: a resolution cell another
/// module's check may fill is left as it is. `None` where a cell is open,
/// a typedef does not resolve or `expanded` reaches [`MAX_EXPANSIONS`].
fn opened(env: &Env, t: &Type, expanded: &mut u8) -> Option<Type> {
    let mut t = t.deref_cloned()?;
    while let Type::Ref(_) = &t {
        if *expanded >= MAX_EXPANSIONS {
            return None;
        }
        *expanded += 1;
        t = t.lookup_ref_probe(env)?.1.deref_cloned()?;
    }
    Some(t)
}

/// The members of the union `t`, nested unions flattened, each through
/// `open`; a member `open` refuses is left out.
fn union_members(
    t: &Type,
    out: &mut LPooled<Vec<Type>>,
    open: &mut impl FnMut(&Type) -> Option<Type>,
) {
    match t {
        Type::Set(ts) => {
            for m in ts.iter() {
                if let Some(m) = open(m) {
                    union_members(&m, out, open)
                }
            }
        }
        t => out.push(t.clone()),
    }
}

/// Whether `d` and `v` are built by one constructor, so a value of `v`
/// can only be a `d` among a union's members.
fn same_constructor(d: &Type, v: &Type) -> bool {
    match (d, v) {
        (Type::Fn(_), Type::Fn(_))
        | (Type::Array(_), Type::Array(_))
        | (Type::List(_), Type::List(_))
        | (Type::Error(_), Type::Error(_))
        | (Type::ByRef(..), Type::ByRef(..))
        | (Type::Map { .. }, Type::Map { .. }) => true,
        (Type::Tuple(a), Type::Tuple(b)) => a.len() == b.len(),
        (Type::Variant(t, a, _), Type::Variant(u, b, _)) => t == u && a.len() == b.len(),
        (Type::Struct(a), Type::Struct(b)) => {
            a.len() == b.len() && a.iter().zip(b.iter()).all(|(f, g)| f.0 == g.0)
        }
        _ => false,
    }
}

/// `t` printed through its cells as they stand: a snapshot that a later
/// bind does not reach.
fn printed_deref(t: &Type) -> String {
    crate::format_with_flags(crate::PrintFlag::DerefTVars, || t.to_string())
}

fn compile_apply_args<R: Rt>(
    ctx: &mut CompileCtx<R>,
    flags: BitFlags<CFlag>,
    scope: &Scope,
    top_id: ExprId,
    spec: &Expr,
    args: &TArc<[(Option<ArcStr>, Expr)]>,
) -> Result<ArgMap<R>> {
    let mut res = ArgMap::with_capacity_and_hasher(args.len(), Default::default());
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

/// The rank-2 quantifiers of a formal: those of each function type at a
/// part of it the value produces (the formal itself, a field, an element,
/// a member, an abstract type's parameter, a function type's result or
/// throws; never a parameter), as the function type names them.
type Quantified = SmallVec<[(ArcStr, TVar); 2]>;

/// A formal holding a quantified function type `fn<'b: C>(..)`, with the
/// typedefs on the way to one expanded, and its [`Quantified`] held rigid
/// for the argument's check: the argument must be well typed for every
/// 'b the bound admits, since the callee may call it at any. `None` for
/// any other formal.
fn quantified_formal(
    env: &Env,
    typ: &Type,
) -> Option<(Type, LPooled<Vec<RigidGate>>, Quantified)> {
    if !typ.may_hold_quantified_fn(env) {
        return None;
    }
    let mut quantified = Quantified::new();
    let formal = rank2_parts(env, typ, &mut Expanding::default(), &mut quantified);
    if quantified.is_empty() {
        return None;
    }
    let gates = quantified.iter().map(|(_, tv)| tv.open_quantifier_rigid()).collect();
    Some((formal.unwrap_or_else(|| typ.clone()), gates, quantified))
}

/// `t`, a typedef reference, expanded, with its definition's key; `None`
/// where it does not resolve (its check reports that) or `expanding`
/// holds its definition already (a recursive typedef met inside its own
/// expansion).
fn expansion(env: &Env, t: &Type, expanding: &[usize]) -> Option<(usize, Type)> {
    t.lookup_ref_probe(env).filter(|(key, _)| !expanding.contains(key))
}

/// A walk's typedef expansions: those in progress, each one it met in
/// progress and so did not expand again, and each parameterless one whose
/// expansion held nothing, with the ones in progress it met there; each
/// shared node of the walked type outside every expansion, and each
/// application whose walk met none in progress, by its definition and its
/// parameters' keys (the parameters held, so the identities stay theirs:
/// [`Type::param_key`]), with what its walk built and whether it met a
/// quantifier; and the quantified function types met so far.
#[derive(Default)]
struct Expanding {
    stack: SmallVec<[usize; 4]>,
    met: SmallVec<[usize; 4]>,
    clean: LPooled<AHashMap<usize, SmallVec<[usize; 2]>>>,
    nodes: LPooled<AHashMap<usize, (Option<Type>, bool)>>,
    apps: LPooled<
        AHashMap<(usize, SmallVec<[ParamKey; 2]>), (Option<Type>, bool, TArc<[Type]>)>,
    >,
    found: usize,
}

impl Expanding {
    /// `t` expanded, or `None` where it does not resolve or is in progress.
    fn expand(&mut self, env: &Env, t: &Type) -> Option<(usize, Type)> {
        let (key, whole) = t.lookup_ref_probe(env)?;
        if self.stack.contains(&key) {
            self.met.push(key);
            return None;
        }
        Some((key, whole))
    }

    /// Whether `key`'s expansion held nothing while the ones in progress it
    /// met are in progress again; met again for the walk's later records.
    fn known_clean(&mut self, key: usize) -> bool {
        match self.clean.get(&key) {
            Some(met) if met.iter().all(|k| self.stack.contains(k)) => {
                self.met.extend(met.iter().copied());
                true
            }
            Some(_) | None => false,
        }
    }
}

/// Push to `out` the quantifiers of each function type at a part of `t`
/// the value produces ([`Quantified`]), each once; `t` rebuilt with every
/// binding and typedef on the way to one expanded, `None` where it is
/// unchanged. A typedef already being expanded is not expanded again.
fn rank2_parts(
    env: &Env,
    t: &Type,
    expanding: &mut Expanding,
    out: &mut Quantified,
) -> Option<Type> {
    #[cfg(test)]
    RANK2_STEPS.with(|n| n.set(n.get() + 1));
    ensure_sufficient(|| {
        // a node many paths share is walked once; one an expansion built
        // lives only while it is walked, so it has no identity
        let node = t.shared_identity().filter(|_| expanding.stack.is_empty());
        if let Some((r, found)) = node.and_then(|n| expanding.nodes.get(&n)) {
            expanding.found += usize::from(*found);
            return r.clone();
        }
        let before = expanding.found;
        let found = |expanding: &Expanding| expanding.found > before;
        let r = match t {
            Type::Fn(ft) => {
                ft.each_quantifier(|name, tv| {
                    if !out.iter().any(|(_, q)| q.cell_addr() == tv.cell_addr()) {
                        out.push((name.clone(), tv.clone()))
                    }
                });
                expanding.found += usize::from(!ft.quantifiers.is_empty());
                let rtype = rank2_parts(env, &ft.rtype, expanding, out);
                let throws = rank2_parts(env, &ft.throws, expanding, out);
                (rtype.is_some() || throws.is_some()).then(|| {
                    Type::Fn(TArc::new(FnType {
                        rtype: rtype.unwrap_or_else(|| ft.rtype.clone()),
                        throws: throws.unwrap_or_else(|| ft.throws.clone()),
                        ..(**ft).clone()
                    }))
                })
            }
            Type::TVar(tv) => tv.binding().and_then(|b| {
                let r = rank2_parts(env, &b, expanding, out);
                found(expanding).then(|| r.unwrap_or(b))
            }),
            Type::Ref(tr) => {
                let bare = tr.params.is_empty();
                match expanding.expand(env, t) {
                    None => None,
                    Some((key, _)) if bare && expanding.known_clean(key) => None,
                    Some((key, whole)) => {
                        let app = (key, tr.params.iter().map(Type::param_key).collect());
                        if let Some((r, hit, _)) = expanding.apps.get(&app) {
                            expanding.found += usize::from(*hit);
                            return r.clone();
                        }
                        let met = expanding.met.len();
                        expanding.stack.push(key);
                        let r = rank2_parts(env, &whole, expanding, out);
                        expanding.stack.pop();
                        if bare && !found(expanding) {
                            let mut seen: SmallVec<[usize; 2]> = expanding.met[met..]
                                .iter()
                                .copied()
                                .filter(|k| *k != key)
                                .collect();
                            seen.sort_unstable();
                            seen.dedup();
                            expanding.clean.insert(key, seen);
                        }
                        let r = found(expanding).then(|| r.unwrap_or(whole));
                        if expanding.met.len() == met {
                            let held = tr.params.clone();
                            let f = found(expanding);
                            expanding.apps.insert(app, (r.clone(), f, held));
                        }
                        r
                    }
                }
            }
            t => t.cow_children(&mut |c| rank2_parts(env, c, expanding, out)),
        };
        if let Some(n) = node {
            let f = found(expanding);
            expanding.nodes.insert(n, (r.clone(), f));
        }
        r
    })
}

#[cfg(test)]
thread_local! {
    static RANK2_STEPS: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
}

/// A rank-2 quantifier `q` keeps exactly the bounds its formal `declared`:
/// the callee may call the argument at any type they admit, so the
/// argument's check may add no conjunct, a predicate included, that none
/// of them implies.
fn added_bound(env: &Env, name: &ArcStr, q: &TVar, declared: &[Type]) -> Result<()> {
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
                    "the argument requires '{name}: {c}, which the parameter's quantifier \
                     does not declare: the callee may call it at any type its bound admits"
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
fn fill_omitted<R: Rt>(args: &mut ArgMap<R>, ftype: &FnType) -> Result<()> {
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
fn typecheck_arg<R: Rt>(
    ctx: &mut CompileCtx<R>,
    typ: &Type,
    hint: Option<&Type>,
    waits: bool,
    n: &mut Node<R>,
) -> Result<ArgFit> {
    // A reference instantiates its signature in its own typecheck0,
    // which must precede the pre-unify.
    if matches!(n.view(), NodeView::Ref(_)) {
        wrap!(n, n.typecheck0(ctx))?;
    }
    match quantified_formal(&ctx.env, typ) {
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
        // CR claude for claude: [perf/low] A formal holding a quantified function
        // type through a chain of parametric typedefs is still checked in time
        // exponential in the chain's depth, though rank2_parts walks it once:
        // `type P0<'a> = ('a, i64); type P1<'a> = (P0<'a>, P0<'a>); ..; let x:
        // P20<fn<'b: Number + Singleton>(x: 'b) -> 'b> = never(); let f = |a: 'a, b:
        // 'a| a; f(x, x)` takes 55G instructions to --check (debug; 9G at 16; main
        // a084f9887 297G and 23G). callgrind puts it in pre_unify_arg and
        // check_contains_rigid below: their containment expands each application
        // anew (ContainsHist::expand_ref, lookup_ref_by, replace_tvars), and an
        // expansion's applications are new nodes, so the ref-pair memo never meets
        // one twice. Memoize a containment of two applications by definition and
        // parameter identity, as Expanding::apps does here. probe:
        // ~/ptmp/cr29/scratch/r6-B/w4/ptq-16.gx, ptq-20.gx (dag-check-02)
        // 2026-11-01 claude: ContainsHist's committed-pair memo now keys an
        // application by its definition and its parameters' identities
        // (ContainsHist::pair_key, interned in apps, pinned), not by its node: an
        // expansion builds its applications anew, so P15<F> met twice under one
        // P16<F> expansion was a new pair each time and a containment of an expanded
        // chain against its application walked 2^depth. --check instructions (debug,
        // cpu_atom + cpu_core) before -> after: ptq-16 6.6G -> 2.5G, ptq-20 89.7G ->
        // 2.5G (the stdlib registration alone is about 2G). Pin graphix-types
        // typ::contains::tests::an_expanded_chain_contains_its_application_once_per_level:
        // 43 containment steps at depth 12, 24577 without (bound 768).
        // 2026-11-02 claude: Review B4-r1-2: an application with a parameter of no node
        // identity (a primitive, Any, a map) got no memo key, so the chain with a leaf
        // parameter was still 2^depth (ptp-16 25.0G, ptp-18 98.1G). Now every parameter
        // has a key (Type::param_key: a node's identity, a map's or an application's two
        // parts', a leaf's content), used by ContainsHist::pair_key and rank2_parts'
        // Expanding::apps alike. --check instructions (debug, atom+core) after: ptp-12
        // 2.30G, ptp-16 2.46G, ptp-18 2.45G, ptp-20 2.52G, pta-16 (Any) 2.80G, ptm-16
        // (Map<string, i64>) 2.40G (main a084f9887 16.7G), pts-16 2.42G, ptq-16 2.35G,
        // ptq-20 2.25G. Pins graphix-types
        // typ::contains::tests::an_expanded_chain_over_a_leaf_contains_its_application_once_per_level
        // (24577 steps without, bound 768) and graphix-compiler
        // node::callsite::test::rank2_parts_walks_an_application_over_a_leaf_once (524286
        // steps without, bound 136). Probes ~/ptmp/cr29/scratch/r6-B/r7/i4/.
        // 2026-11-02 claude: Review B4-r2 (reviewer 2 medium, reviewer 1 low): a chain whose
        // applications take a parameter each expansion builds anew (P<i><'a> = (P<i-1><('a, i64)>,
        // ..), or Array<'c> per level) was still 2^depth, in rank2_parts at a let annotation (ptc0-18
        // 14.1G) and in the call's containment (ptc-14 11.6G, ptg-16 36.7G): Type::param_key keyed a
        // composite parameter by its node, which no two expansions share. Now ParamKey is a
        // parameter's content (a variable by its cell, a name by its resolution cell and parameters,
        // a function type by its node, a part past 64 words by its node), so equal parameters meet.
        // --check instructions (debug, atom+core) before -> after: ptc0-14 3.90G -> 2.54G, ptc0-18
        // 14.07G -> 2.43G, ptc-12 4.23G -> 2.37G, ptc-14 11.64G -> 2.24G, ptg-12 4.97G -> 2.45G,
        // ptg-14 8.37G -> 2.48G, ptg-16 36.70G -> 2.48G; ptc-18 2.26G, ptg-18 2.59G, ptq-20 2.53G,
        // ptp-16 2.33G, pta-16 2.42G, ptm-16 2.65G. Pins graphix-types
        // typ::contains::tests::an_expanded_chain_over_a_rebuilt_parameter_contains_its_application_once_per_level
        // (40957 steps without, bound 768) and graphix-compiler
        // node::callsite::test::rank2_parts_walks_an_application_over_a_rebuilt_parameter_once
        // (2621438 steps without, bound 136). Probes ~/ptmp/cr29/scratch/r6-B/r8/i4/.
        // 2026-11-02 claude: Review B4-r3 (both reviewers): still 2^depth where each level passes a function type
        // built anew (`type P<i><'a> = (P<i-1><fn(x: 'a) -> i64>, P<i-1><fn(x: 'a) -> i64>)`), since ParamKey keys
        // a function type by its node, or a parameter whose key passes ParamKey::WORDS (a 25-field struct). At a
        // let annotation that is new against main, whose let runs no rank2_parts: ptf0-14 27.4G instructions on
        // the lane against 2.31G on main, pts-14 6.33G against 2.14G. With a call f(x, x) both are exponential
        // (ptf-14 94.8G lane, 25.5G main). Fix: key a function type with no quantifiers by its content, and keep
        // equality past WORDS (hash the rest rather than fall back to the node). probes
        // ~/ptmp/cr29/scratch/review-r6/B4-r3-1/i4/genf.sh, B4-r3-2/i4/{genf,gens}.sh (./inst BIN ptf0-14.gx).
        Some((formal, _rigid, quantified)) => {
            let declared: SmallVec<[SmallVec<[Type; 1]>; 2]> =
                quantified.iter().map(|(_, q)| q.cell_constraints()).collect();
            Type::pre_unify_arg(&ctx.env, &formal, n.typ())?;
            wrap!(n, n.typecheck0(ctx))?;
            for ((name, q), declared) in quantified.iter().zip(declared.iter()) {
                wrap!(n, added_bound(&ctx.env, name, q, declared))?;
            }
            wrap!(n, formal.check_contains_rigid(&ctx.env, &n.typ()))?;
            check_general(ctx, &quantified, n, "the argument")?;
            Ok(ArgFit::Fits)
        }
    }
}

// CR claude for claude: [perf/low] The quick test (Type::may_hold_quantified_fn) at each
// written holder walks the typedef graph its written type reaches once per holder: a
// gui definition written `-> Widget` walks Widget's every member, so the GUI examples'
// --check costs about 1.5% more instructions than before the holders were checked
// (~/ptmp/cr29/guicheck.sh: main a084f9887 91.47G, lane 92.82G). A verdict per
// definition, kept where every name its body reaches is resolved and no definition
// in progress was met, would make each walk one lookup. (quantified-quick-test-01)
/// `contain` the value `n` in `holder`, a written type. Where the type
/// holds quantified function types it is expanded to them, their
/// quantifiers held rigid, for the containment (a mismatch shows
/// `holder`), and the value must be general in each, as a call's argument
/// must be in its formal's ([`check_general`]).
pub(crate) fn contain_generally<R: Rt>(
    ctx: &CompileCtx<R>,
    holder: &Type,
    n: &Node<R>,
    contain: impl FnOnce(&Type) -> Result<()>,
) -> Result<()> {
    match quantified_formal(&ctx.env, holder) {
        None => contain(holder),
        Some((formal, _rigid, quantified)) => {
            contain(&formal).map_err(|e| match e.downcast::<TypeMismatch>() {
                Ok(m) => anyhow::Error::new(m.shown_as(holder.clone())),
                Err(e) => e,
            })?;
            check_general(ctx, &quantified, n, "the value")
        }
    }
}

// CR claude for claude: [bug/high] A written quantified fn type checks the lambda it
// holds with its quantifiers rigid, but only a call's argument is checked for a cell
// outside the lambda that a quantifier reaches (polymorphic_in, escapes_through). A
// let annotation, a struct field or an array element lets the quantifier escape:
// `let y = never(); let h: fn<'a: Any>(x: 'a) -> 'a = |x| { y <- x; x }; let k:
// fn(x: string) -> string = h; println(k("s")); println(y + 1)` passes --check and
// its build refuses k("s") ("an instance at fn(x: string) -> string of a definition
// typed fn(x: i64) -> 'a: i64"); in the REPL `p(1); p("s"); y + 1` runs and a string
// reaches `y + 1` (rp2.txt). The same with `|x| (x, n)` over an outer `n` (v4),
// inside a definition (d5, d6), in a struct field (w2) and through a module's use of
// the parent's typedef (mt4, mt5); main a084f9887 the same. Fix: run
// polymorphic_in/escapes_through wherever a written quantified fn type contains its
// value (a let annotation, a field, an element, a return type), as a call does for
// its formal. probes: ~/ptmp/cr29/scratch/review-r6/B2-r3-1/q/{w7,v4,d5,d6,w2}.gx,
// mt4/main.gx, mt5/main.gx, rp2.txt (graphix --no-cache < rp2.txt)
// (rank2-escape-01)
// 2026-11-01 claude: Every place a written type holds a value now checks it as a
// call checks its argument (callsite::contain_generally: the type expanded to its
// quantified fn types, their quantifiers held rigid, then check_general): a let
// annotation (fields, elements, typedefs, a module using the parent's typedef), a
// connect's target, *r <-, a struct update's field, a written return type and a
// labeled default. The holds_quantified_fn probe no longer resolves the names it
// meets (Verdicts::looking): run at those sites over sibling modules' types it
// filled a TypeRef cell from the wrong environment (data_table's Source as core's),
// a rare nondeterministic 'ColumnSpec does not contain' refusal of the gui package
// (1 in 60 --check runs before, 0 in 120 after). The REPL shape (rp2.txt) still
// runs: the statements of a line are separate inputs, and a later input's write to
// an earlier ⊥ let is the separate CR repl-bottom-settle-01; one input holding the
// shape is refused. Pins: the 14 RANK2_LET_*/CONNECT/DEREF/UPDATE/RETURN/DEFAULT
// cases of lang::types::unsound_acceptances_are_refused,
// rank2_module_let_writes_outer, rank2_written_holders_take_general_values (no
// over-refusal), graphix-shell tests/rank2_escape.rs,
// node::callsite::test::quantified_formal_decides_no_name; each fails without it;
// pass under GRAPHIX_NO_SUBST=1.
// 2026-11-01 claude: Follow-up 0966d1faa: the quick test is now
// Type::may_hold_quantified_fn (walks the type and each definition it reaches once,
// as written, copying nothing and peeking names only; rank2_parts makes it exact),
// and holds_quantified_fn and Verdicts::looking are gone; only a written return type
// is checked as a holder. guicheck main a084f9887 91.47G, lane 92.82G (95.95G before
// the follow-up); CR quantified-quick-test-01 for the rest. The unit pin
// quantified_formal_decides_no_name holds; 0 refusals in 60 --check runs of a trivial
// program.
// 2026-11-02 claude: Review B4-r1 (both reviewers): the escape check missed a
// quantified fn type in another function type's result (let, call formal, labeled
// default, written return type, struct field, a quantified outer type, a struct
// result: b1b b6 b7 b8 b9 b15 b16, fuzz DIVERGENCE) and an abstract constructor's
// payload (b2b, ab1, ab2). Now rank2_parts and Type::may_hold_quantified_fn walk a
// function type's result and throws (never its parameters: a value receives those,
// and a call through one inside it is checked at that call), and
// Construct::typecheck0_with contains its payload through contain_generally. Pins:
// the RANK2_RESULT_* and RANK2_CONSTRUCT_* cases of
// lang::types::unsound_acceptances_are_refused (each passed --check before; the test
// fails without the fix), lang::types::rank2_result_holders_take_general_values (no
// over-refusal). Still open: an interface val (if1, if3) is the new CR
// iface-rank2-escape-01, the class of main's iface-generic-written-01; the REPL shape
// rp2 is repl-bottom-settle-01. Probes ~/ptmp/cr29/scratch/r6-B/r7/.
// 2026-11-02 claude: Review B4-r2 (both reviewers, medium): an abstract type applied
// to a quantified fn type (type W<'t> = Abstract<'t>; let w: W<fn<'a: Any>(..)> =
// W(|x| { y <- x; x }), core's Transparent<fn<'a>..> alike) was no holder:
// rank2_parts and Type::may_hold_quantified_fn stopped at Type::Abstract, and
// Construct saw its parameter still open. Both now walk an abstract type's parameters
// as parts the value produces (conservative: a parameter used only in a function's
// parameter inside the representation is held too). Pins:
// RANK2_ABSTRACT_PARAMETER_WRITES_OUTER, _FIELD_, _RETURN_ and
// RANK2_TRANSPARENT_WRITES_OUTER in lang::types::unsound_acceptances_are_refused
// (accepted before, the run failing its bind), the B<..>/Transparent<..> row of
// lang::types::rank2_result_holders_take_general_values (no over-refusal).
// 2026-11-02 claude: Landed on main without lane B's last fix round: cr39-B 0cb9a50bd
// (callsite::Makes, check_made) is left out, and the result-position maker shape it
// chased (a let of the maker's own body captured by the function it makes; l1 l3-l12,
// e1b e4 e9-e13) is the new CR rank2-result-maker-01 for eric. On main: the holder
// sites above, a function type's result and throws, a constructor's payload, an
// abstract type's parameter. Still open here (review B4-r3-1, medium): the let path
// does not follow a cell's lower bound (TCell::lower), so a write through an abstract
// constructor escapes. `type W<'t> = Abstract<'t>; let y = never(); let f: fn<'a:
// Any>(x: 'a) -> 'a = |x| { y <- W(x); x }; let s = f("s"); println(y.0 + 1)` passes
// --check and the build refuses f("s")'s instance (lb4; lb12 a struct field, lb13
// Transparent, lb14 a written return type; main a084f9887 the same). A call refuses
// it (lb5), because it pre-unifies first. Fix: reaches_cell/reaches_through follow an
// open cell's lower bounds, or the let path pre-unifies as a call does. probes
// ~/ptmp/cr29/scratch/review-r6/B4-r3-1/s/lb*.gx (NS=1 ./both). The interface val is
// iface-rank2-escape-01, the REPL shape repl-bottom-settle-01.
// CR claude for eric: [bug/high] A quantified fn type in a function type's result
// (`fn(z: i64) -> fn<'a: Any>(x: 'a) -> 'a`) is general in 'a only if the function
// that makes the result shares nothing that reaches 'a across its calls, and nothing
// checks that: check_general judges the bindings outside the outer value, so a let of
// the maker's own body that the made function captures passes. `let mk: fn(z: i64) ->
// fn<'a: Any>(x: 'a) -> 'a = |z| { let v = never(); |x| { v <- x; v } }; let h =
// mk(0); let s = h("s"); println(h(1) + 1)` passes --check, and the run logs an arith
// error over "s" at a value typed i64. The same at a call's formal, a labeled default,
// a struct field, a connect, a constructor's payload, through a call's instance in the
// body (`|q| mk2(never())`) and through a generic definition read whole (main a084f9887
// the same); the same body under a written return type (e8) is refused. Lane B round 6
// tried a record of what outlives a call of each lambda (callsite::Makes, judged by
// check_made; branch cr39-B 0cb9a50bd, not landed). It missed a maker reached through a
// field read, an element, an index or a select's first arm (ms15, ms20-ms22, s5b: the
// run puts "s" in an i64), gave a sibling module's interface val a verdict that
// depended on which compile task finished first (34 of 40 --check runs refused), and
// refused a valid maker returning a struct through a let (st1, which main runs). The
// local rule both reviewers propose: a result-position quantifier takes only a lambda
// literal, checked as if the holder's result were its written return type (the path
// that refuses e8), or a value whose own type quantifies 'a there; any other value is
// refused with "write its return type". That refuses programs main runs today (a safe
// maker read from a field, `s.mk`; a let-bound generic maker), hence for eric. probes:
// ~/ptmp/cr29/scratch/review-r6/B4-r2-1/s/{l1,l3,l10}.gx, B4-r2-2/n/{e1b,e4,e9,e11}.gx,
// B4-r3-1/s/{ms15,ms20,ms1,st1}.gx, B4-r3-1/s/sib/main.gx, B4-r3-2/p/{s5b,s3a,s7a}.gx
// (rank2-result-maker-01)
/// A value checked against quantifiers held rigid holds for every choice
/// of them only if nothing outside it that lives on reaches one: a binding
/// it reads or writes from outside (its lambdas' captures included), or the
/// instance a call in it makes, whose signature types that instance's
/// bindings. Its own cells (a literal's, a lambda's) live only in it.
fn check_general<R: Rt>(
    ctx: &CompileCtx<R>,
    quantified: &Quantified,
    n: &Node<R>,
    what: &str,
) -> Result<()> {
    let quantified: SmallVec<[(ArcStr, usize); 2]> =
        quantified.iter().map(|(name, tv)| (name.clone(), tv.cell_addr())).collect();
    let mut refs = Refs::default();
    n.refs(&mut refs);
    let mut free: SmallVec<[(Type, &str); 4]> = SmallVec::new();
    let binding = "a binding it reaches";
    refs.with_external_refs(|id| {
        if let Some(b) = ctx.env.by_id.get(&id) {
            free.push((b.typ.clone(), binding))
        }
    });
    // a lambda's body is no child of its node: the bindings it reads
    // and writes are its definition's captures
    fusion::for_each_node(n, &mut |c| match c.view() {
        NodeView::Lambda(l) => {
            if let Some(d) = l.def_value().downcast_ref::<LambdaDef<R>>() {
                d.captured_ids(&mut |id| {
                    if let Some(b) = ctx.env.by_id.get(&id) {
                        free.push((b.typ.clone(), binding))
                    }
                })
            }
        }
        NodeView::CallSite(cs) => {
            if let Some(ft) = &cs.ftype {
                free.push((Type::Fn(ft.clone()), "the instance a call in it makes"))
            }
        }
        _ => (),
    });
    for (t, through) in free.iter() {
        wrap!(n, escapes_through(&quantified, t, what, through))?;
    }
    Ok(())
}

/// `f`'s parameter `name` as a call through `site` instantiates it: a
/// fresh instance of `f`'s signature whose other parameters take the
/// arguments of a copy of the site's, and whose result and throws fit
/// the copy's, so a variable the site fixes through another parameter,
/// its result or its throws fixes this one.
fn callee_param<R: Rt>(
    ctx: &CompileCtx<R>,
    f: &LambdaDef<R>,
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
        site.each_quantifier(|_, tv| gates.push(tv.open_quantifier_rigid()));
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
        let _ = site.rtype.contains(&ctx.env, &inst.rtype);
        let _ = site.throws.contains(&ctx.env, &inst.throws);
    }
    inst.args.iter().find(|a| a.label() == Some(name)).map(|a| a.typ.clone())
}

/// The cells of the quantifiers a function type under `t` binds.
pub(crate) fn bound_quantifiers(t: &Type) -> AHashSet<usize> {
    let mut out = AHashSet::new();
    t.each_quantifier(&mut |_, tv| {
        out.insert(tv.cell_addr());
    });
    out
}

/// A binding the value reads or writes, or an instance it makes, of type
/// `env`, holds none of the quantifiers: its cell would carry one out.
fn escapes_through(
    quantified: &[(ArcStr, usize)],
    env: &Type,
    what: &str,
    through: &str,
) -> Result<()> {
    let own = bound_quantifiers(env);
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
            "{what} is not polymorphic in '{q}: {through}, of type {}, would carry \
             '{q} out",
            printed_deref(env)
        ),
    }
}

/// Every cell under `t`, following bindings, each once: a cell `seen`
/// holds is not visited, nor a shared node walked already.
fn reaches_cell(t: &Type, seen: &mut AHashSet<usize>, f: &mut impl FnMut(&TVar)) {
    fn go(
        t: &Type,
        seen: &mut AHashSet<usize>,
        nodes: &mut AHashSet<usize>,
        f: &mut impl FnMut(&TVar),
    ) {
        #[cfg(test)]
        RANK2_STEPS.with(|n| n.set(n.get() + 1));
        ensure_sufficient(|| match t {
            Type::TVar(tv) => {
                if seen.insert(tv.cell_addr()) {
                    f(tv);
                    if let Some(b) = tv.binding() {
                        go(&b, seen, nodes, f)
                    }
                }
            }
            t if t.shared_identity().is_some_and(|n| !nodes.insert(n)) => (),
            t => t.for_each_child(&mut |c| go(c, seen, nodes, f)),
        })
    }
    go(t, seen, &mut LPooled::take(), f)
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
    let bound = bound_quantifiers(value);
    let mut foreign: SmallVec<[TVar; 4]> = SmallVec::new();
    reaches_cell(value, &mut AHashSet::new(), &mut |tv| match tv.level() {
        _ if bound.contains(&tv.cell_addr()) => (),
        Level::Top => foreign.push(tv.clone()),
        Level::Def { owner, .. } if !own.contains(&owner) => foreign.push(tv.clone()),
        Level::Def { .. } | Level::Generic | Level::Written => (),
    });
    foreign.into_iter().find_map(|tv| {
        let mut reached = None;
        reaches_cell(&Type::TVar(tv.clone()), &mut AHashSet::new(), &mut |c| {
            if let Some((q, _)) = quantified.iter().find(|(_, a)| *a == c.cell_addr()) {
                reached = Some(q.clone())
            }
        });
        reached.map(|q| (q, tv))
    })
}

/// The shared nodes a [`data_positions`] walk has walked, by identity and
/// position kind, each held so an expansion's node keeps its address.
type DataSeen = AHashMap<(usize, bool), Type>;

/// Every type variable under `t` by name, with whether each occurrence
/// is data: never under a function, a writable reference, a typedef application,
/// a nominal type or a constructor application. Bindings are walked.
fn data_positions(
    env: &Env,
    t: &Type,
    data: bool,
    expanding: &mut SmallVec<[usize; 4]>,
    seen: &mut DataSeen,
    out: &mut AHashMap<ArcStr, (TVar, bool)>,
) {
    ensure_sufficient(|| match t {
        Type::TVar(tv) => {
            out.entry(tv.name.clone()).or_insert_with(|| (tv.clone(), true)).1 &= data;
            if seen.insert((tv.cell_addr(), data), t.clone()).is_none()
                && let Some(b) = tv.binding()
            {
                data_positions(env, &b, data, expanding, seen, out)
            }
        }
        // a shared node is walked once in each kind of position
        t if t
            .shared_identity()
            .is_some_and(|n| seen.insert((n, data), t.clone()).is_some()) => {}
        Type::Ref(_) if let Some((key, whole)) = expansion(env, t, expanding) => {
            expanding.push(key);
            data_positions(env, &whole, data, expanding, seen, out);
            expanding.pop();
        }
        Type::Fn(_)
        | Type::ByRef(Mutability::Mut, _)
        | Type::Ref(_)
        | Type::Abstract { .. }
        | Type::App(..) => {
            t.for_each_child(&mut |c| data_positions(env, c, false, expanding, seen, out))
        }
        t => {
            t.for_each_child(&mut |c| data_positions(env, c, data, expanding, seen, out))
        }
    })
}

/// Whether `t` holds a type variable under an abstract type, a type
/// application or a writable reference, outside any function type: such a position
/// fixes the variable exactly, so its argument is checked first and the
/// order a call writes its arguments in decides nothing. A function's
/// positions stay in order: an earlier argument types a callback's.
/// `clean` holds the parameterless typedefs whose expansion holds none, so
/// a typedef the type reaches by many paths is walked once.
fn holds_invariantly(
    env: &Env,
    t: &Type,
    expanding: &mut SmallVec<[usize; 4]>,
    clean: &mut AHashSet<usize>,
) -> bool {
    ensure_sufficient(|| match t {
        Type::TVar(tv) => {
            tv.binding().is_some_and(|b| holds_invariantly(env, &b, expanding, clean))
        }
        Type::Fn(_) => false,
        Type::Ref(tr) if let Some((key, whole)) = expansion(env, t, expanding) => {
            let bare = tr.params.is_empty();
            if bare && clean.contains(&key) {
                return false;
            }
            expanding.push(key);
            let r = holds_invariantly(env, &whole, expanding, clean);
            expanding.pop();
            if bare && !r {
                clean.insert(key);
            }
            r
        }
        Type::Abstract { .. }
        | Type::App(..)
        | Type::Ref(_)
        | Type::ByRef(Mutability::Mut, _) => {
            let mut tvars: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
            t.collect_tvars(&mut tvars);
            !tvars.is_empty()
        }
        t => {
            let mut any = false;
            t.for_each_child(&mut |c| {
                any = any || holds_invariantly(env, c, expanding, clean)
            });
            any
        }
    })
}

/// A fresh instance's type variables that settle to the widest argument
/// rather than the first: open in the definition `def`, and met only as
/// data in `inst`, so a value of a wider type is well typed wherever the
/// signature holds one.
fn widenable(env: &Env, def: &FnType, inst: &FnType) -> LPooled<AHashMap<ArcStr, TVar>> {
    let mut at_def: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
    def.collect_tvars(&mut at_def);
    let mut seen: LPooled<DataSeen> = LPooled::take();
    let mut found: LPooled<AHashMap<ArcStr, (TVar, bool)>> = LPooled::take();
    let mut expanding = SmallVec::new();
    inst.for_each_type(&mut |t| {
        data_positions(env, t, true, &mut expanding, &mut seen, &mut found)
    });
    found
        .drain()
        .filter(|(name, (tv, data))| {
            *data && !tv.is_rigid() && at_def.get(name).is_some_and(|d| !d.is_bound())
        })
        .map(|(name, (tv, _))| (name, tv))
        .collect()
}

// CR claude for claude: [bug/medium] An argument whose type holds a cell its uses decide
// (a read target such as str::parse's or sys::net::subscribe's, an unwritten `let x =
// never()`, an empty `[]`'s element) is not widened over: its cell is decided by
// whichever argument the loop meets first, so "the widest argument, whatever the
// order" fails for it. `{ let f = |#x: Array<'a> = [1], y: 'a| -> 'a y;
// f(sys::net::subscribe("/local/dimpl1")) }` checks, and the same call with the
// default written, `f(#x: [1], sys::net::subscribe(..))`, is refused ("'a: iN does
// not contain ['a: iN, Error<..>]"): typemorph's default-materialize flip,
// fuzz/pending-triage/oct29a/aieka/typeflip_000000.gx, still flips on main
// df6600e9b. Four review rounds on branch cr30-u3 built a widening over such
// arguments (+557/-114 lines here); each round's review found a new path dependence
// (round 4: renaming f's type variables flips acceptance, and a parse result held in
// a `let` is refused where main runs it). The rule is cr-discuss round 4 D1; the
// rounds' probes are under ~/tmp/cr29/scratch/review{3,4,5,6}/u3/ (widen-open-01)
// 2026-10-30 claude: Eric ruled (2026-10-30, cr-discuss round 4 D1): option (c). Each
// argument adds a lower bound to the variable (the TCell::lower mechanism an
// abstract's payload uses) and the statement's settle sets the variable to the join of
// its lower bounds, so the order cannot matter by construction. A design doc and a
// prototype first; branch cr30-u3 stays for reference.
// 2026-10-31 claude: Eric ruled (round 5 D1): a widening variable met by a
// function-typed argument does not widen; it binds as on main, pre-unification on, the
// first argument deciding. Decisions: 1 (b'): the call's end judges the join, the widest
// argument must contain every other, else the call is refused naming the argument; 2
// yes, a parameter reaching a widening call as a union member stays generic; 3 yes, the
// variable binds at the call's end; 4 confirmed, provided a census shows the newly
// refused population is not too big; 5 accept the refusal; 6 refuse the call. Lane E
// continues stages 2 and 3 on cr31-E.
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
        env: &Env,
        def: &Type,
        inst: &FnType,
    ) -> &'a AHashMap<ArcStr, TVar> {
        cells.get_or_insert_with(|| {
            def.with_deref(|t| match t {
                Some(Type::Fn(def)) => widenable(env, def, inst),
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
    fn hint(
        &mut self,
        env: &Env,
        def: &Type,
        inst: &FnType,
        formal: &Type,
    ) -> Option<Type> {
        if !self.fresh {
            return None;
        }
        let cells = Self::cells(&mut self.cells, env, def, inst);
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
    fn waits(&mut self, env: &Env, def: &Type, inst: &FnType, formal: &Type) -> bool {
        if !self.fresh {
            return false;
        }
        let cells = Self::cells(&mut self.cells, env, def, inst);
        let mut held: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
        formal.collect_tvars(&mut held);
        held.keys().any(|name| cells.contains_key(name))
    }

    /// The argument for `key` met `formal`: widen, wait, or the mismatch
    /// error.
    fn fit<R: Rt>(
        &mut self,
        env: &Env,
        def: &Type,
        inst: &FnType,
        key: &ArgKey,
        formal: &Type,
        n: &Node<R>,
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
    fn misfit<R: Rt>(
        &mut self,
        env: &Env,
        def: &Type,
        inst: &FnType,
        key: &ArgKey,
        formal: &Type,
        n: &Node<R>,
    ) -> Result<()> {
        let check = || wrap!(n, formal.check_contains(env, n.typ()));
        if !self.fresh || n.typ().has_unbound() {
            return check();
        }
        let cells = Self::cells(&mut self.cells, env, def, inst);
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
fn arg_ref<R: Rt>(
    ctx: &mut CompileCtx<R>,
    top_id: ExprId,
    arg: &Arg<R>,
    typ: &Type,
) -> Node<R> {
    let (typ, spec) = match &arg.node {
        Some(n) => (n.typ().clone(), arg.spec.clone()),
        None => (typ.clone(), NOP.clone()),
    };
    Ref::new(ctx, arg.id, typ, top_id, spec)
}

/// What a [`CallSite`] knows about its callee.
#[derive(Debug)]
pub(crate) enum Callee<R: Rt> {
    /// No callee bound yet. `fnode` is re-evaluated every cycle; the
    /// first cycle it yields a `LambdaDef`, `update()` binds.
    DynamicUnbound,
    /// Bound to a callee that may change cycle-to-cycle; `def` is kept
    /// for the per-cycle identity check against `fnode.update()`.
    DynamicBound { def: Value, apply: Box<dyn Apply<R>> },
    /// `def`'s instance, built before the first dispatch
    /// ([`CallSite::prebind`]), which primes `defaults`, the outer
    /// variables the defaults it compiled read, and binds it.
    Prebound { def: Value, apply: Box<dyn Apply<R>>, defaults: SmallVec<[BindId; 2]> },
    /// Binding `def` failed: the call is bottom until `fnode` yields
    /// another definition.
    Failed { def: Value },
    /// Pre-bound at compile time by [`CallSite::try_static_resolve`]; the
    /// per-cycle identity check is skipped (`fnode.update()` still runs
    /// for effects). `first_update` marks the next update as the
    /// instance's first dispatch: it keeps every quiet production,
    /// delivers its arguments and defaults fired and dispatches under
    /// `View::Birth`.
    Static { apply: Box<dyn Apply<R>>, first_update: bool },
    /// A statically bound instance the image holds in its heap, decoded
    /// by the first dispatch; `refs` is the body's summary until then,
    /// whose reads the site holds registered while it is imaged.
    Imaged {
        instance: LambdaInstanceId,
        first_update: bool,
        refs: ImagedRefs,
        /// The reader its reads stand for, and whether they are
        /// registered: not while the instance around it decodes at its
        /// own first dispatch, which dispatches this site too unless it
        /// leaves it unreached (`CallSite::hold_imaged`).
        reader: crate::prune::Reader,
        held: bool,
    },
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
            let mut bound: LPooled<IntSet<BindId>> = LPooled::take();
            self.reach(instance, &mut |r| {
                refed.extend(r.own.refed.iter().copied());
                bound.extend(r.own.bound.iter().copied());
            });
            RefsSummary::of_sets(&refed, &bound)
        })
    }

    /// Each instance of `instance`'s tree, once.
    fn reach(&self, instance: LambdaInstanceId, f: &mut dyn FnMut(&InstanceRefs)) {
        let mut seen: LPooled<IntSet<LambdaInstanceId>> = LPooled::take();
        let mut stack: LPooled<Vec<LambdaInstanceId>> = LPooled::take();
        stack.push(instance);
        while let Some(i) = stack.pop() {
            if !seen.insert(i) {
                continue;
            }
            let Some(r) = self.table.get(&i) else { continue };
            f(r);
            stack.extend(r.callees.iter().copied());
        }
    }
}

/// What a heap instance's own nodes read and bind statically, and its
/// callees by instance.
#[derive(Debug, Clone, netidx_derive::Pack)]
pub(crate) struct InstanceRefs {
    own: RefsSummary,
    callees: Vec<LambdaInstanceId>,
    holds: Vec<(BindId, TArc<Expr>)>,
}

pub(crate) type InstanceRefsTable = AHashMap<LambdaInstanceId, InstanceRefs>;

/// An imaged site's reads for registration, each sorted.
#[derive(Debug, Clone, netidx_derive::Pack)]
pub(crate) struct RefsSummary {
    refed: Vec<BindId>,
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

impl<R: Rt> Callee<R> {
    fn to_static(apply: Box<dyn Apply<R>>, first_update: bool) -> Self {
        Callee::Static { apply, first_update }
    }

    fn apply(&self) -> Option<&dyn Apply<R>> {
        match self {
            Callee::DynamicUnbound | Callee::Failed { .. } | Callee::Imaged { .. } => {
                None
            }
            Callee::DynamicBound { apply, .. }
            | Callee::Prebound { apply, .. }
            | Callee::Static { apply, .. } => Some(&**apply),
        }
    }

    pub(crate) fn apply_mut(&mut self) -> Option<&mut (dyn Apply<R> + 'static)> {
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
    fn take_apply(&mut self) -> Option<Box<dyn Apply<R>>> {
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
pub struct CallSite<R: Rt> {
    pub(super) slept: WakeBit,
    pub(super) spec: TArc<Expr>,
    pub(super) ftype: Option<TArc<FnType>>,
    pub(super) rtype: Type,
    pub(crate) fnode: Node<R>,
    pub(crate) args: ArgMap<R>,
    pub(super) arg_refs: Vec<Node<R>>,
    pub(crate) callee: Callee<R>,
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
    absent: Option<Box<Absent>>,
    /// What a shrink kept of this recursive edge's pure callee.
    kept: Kept,
    /// The imaged instance decoded at this update's dispatch: the sites
    /// its first update leaves imaged are held after it.
    decoded: bool,
    pub(crate) fork: ForkSite,
    /// The bindings this call bound statically, its function's and its
    /// fn-typed arguments', and, for an imaged callee, its whole tree's.
    static_holds: SmallVec<[StaticHold; 1]>,
}

/// A pure callee a shrink left asleep at a recursive edge (`CallSite::
/// sleep`). Re-reaching the edge dispatches it as a fresh bind, which no
/// program can tell from one, and restarts the count; each exit of the
/// recursion past the edge sheds its deepest levels (`CallSite::shed`). An
/// exit is an update of the select whose arm holds the edge in which the
/// chain runs (the scrutinee or a consulted guard fired, or the select
/// woke) and takes another arm, or the scrutinee is bottom
/// (`select.rs::exit_kept`); the update that deselects the arm is the
/// shrink, not an exit.
#[derive(Debug, Clone, Copy, Default)]
enum Kept {
    #[default]
    Nothing,
    /// No exit has passed it since the shrink.
    Asleep,
    /// `levels` deep at the first exit past it; `exits` exits have shed
    /// the deepest `shed_levels(levels, exits)` of them.
    Shedding { levels: u32, exits: u8 },
}

/// Each recursive edge in `node`'s graph, not looking into callees.
fn for_each_edge<R: Rt>(node: &mut Node<R>, f: &mut dyn FnMut(&mut CallSite<R>)) {
    ensure_sufficient(|| {
        if let Some(site) =
            node.downcast_mut::<CallNode<R>>().and_then(CallNode::call_mut)
            && site.is_recursive_edge()
        {
            f(site)
        }
        node.for_each_child_mut(&mut |c| for_each_edge(c, f))
    })
}

/// The levels of recursion `f` holds: itself and its deepest chain of
/// recursive edges.
fn kept_levels<R: Rt>(f: &mut dyn Apply<R>) -> u32 {
    let mut below = 0;
    f.for_each_body_mut(&mut |body| {
        for_each_edge(body, &mut |site| {
            if let Some(callee) = site.callee.apply_mut() {
                below = below.max(kept_levels(callee))
            }
        })
    });
    below + 1
}

/// How many of `levels` kept levels `exits` exits have shed, deepest
/// first: a level an exit at least, 1% of them an exit where that is more,
/// so all of them by the hundredth exit and `exits` never passes 100.
fn shed_levels(levels: u32, exits: u8) -> u32 {
    let paced = u64::from(levels) * u64::from(exits) / 100;
    paced.max(u64::from(exits)).min(u64::from(levels)) as u32
}

/// Delete every callee more than `keep` levels below `f`, `f` itself the
/// first.
fn cut_below<R: Rt>(ctx: &mut ExecCtx<'_, R>, f: &mut dyn Apply<R>, keep: u32) {
    f.for_each_body_mut(&mut |body| {
        for_each_edge(body, &mut |site| {
            if keep > 1 {
                if let Some(callee) = site.callee.apply_mut() {
                    cut_below(ctx, callee, keep - 1)
                }
            } else if let Some(mut callee) = site.callee.take_apply() {
                site.kept = Kept::Nothing;
                callee.delete(ctx)
            }
        })
    });
}

/// An exit of a recursion through the select whose untaken arm `node` is:
/// each callee a recursive edge in it kept sheds its deepest levels
/// (`CallSite::shed`). Whether any still keeps something.
pub(super) fn shed_kept<R: Rt>(ctx: &mut ExecCtx<'_, R>, node: &mut Node<R>) -> bool {
    let mut kept = false;
    for_each_edge(node, &mut |site| kept |= site.shed(ctx));
    kept
}

/// The registered builtin a site calls by name: its function is a
/// binding the index resolves to a builtin's definition, or one bound to
/// the builtin directly.
pub(crate) fn static_builtin<'a, R: Rt>(
    ctx: &'a CompileCtx<R>,
    cs: &CallSite<R>,
) -> Option<&'a ArcStr> {
    let NodeView::Ref(r) = cs.fnode().view() else { return None };
    if let Some(d) = ctx
        .bind_to_lambda
        .get(&r.id)
        .and_then(|v| v.downcast_ref::<super::lambda::LambdaDef<R>>())
    {
        return d.builtin_name();
    }
    let bind = ctx.env.by_id.get(&r.id)?;
    ctx.builtin_bindings.get(&(bind.scope.clone(), bind.name.clone())).map(|i| &i.name)
}

impl<R: Rt> CallSite<R> {
    /// An unbound site over compiled parts; `ftype` is `None` until
    /// `typecheck0` instantiates it.
    pub(crate) fn unbound(
        spec: TArc<Expr>,
        ftype: Option<FnType>,
        rtype: Type,
        fnode: Node<R>,
        args: ArgMap<R>,
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
            kept: Kept::Nothing,
            decoded: false,
            fork: ForkSite::default(),
            static_holds: SmallVec::new(),
        }
    }

    /// Hold `id`, a binding this call binds statically.
    fn hold_static(&mut self, ctx: &CompileCtx<R>, id: BindId) {
        if !self.static_holds.iter().any(|h| h.id == id) {
            self.static_holds.push(ctx.hold_static(id, &self.spec))
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
    pub fn arg_positional(&self, idx: usize) -> Option<&Node<R>> {
        self.arg(&ArgKey::Positional(idx))
    }

    /// Every argument the call wrote, compiled.
    pub(crate) fn arg_nodes(&self) -> impl Iterator<Item = &Node<R>> {
        self.args.values().filter_map(|a| a.node.as_ref())
    }

    /// The order the node-walk updates `nodes`, some of this call's
    /// argument nodes in another order: their indices by each one's place
    /// among the arguments, the written ones then the defaults a bind
    /// added; a node that is no argument goes last. It is the order their
    /// raises reach a handler.
    pub(crate) fn update_order(
        &self,
        nodes: &[Option<&Node<R>>],
    ) -> SmallVec<[usize; 12]> {
        let rank = |n: Option<&Node<R>>| {
            n.and_then(|n| self.arg_nodes().position(|a| std::ptr::eq(a, n)))
                .unwrap_or(usize::MAX)
        };
        let mut order: SmallVec<[usize; 12]> = (0..nodes.len()).collect();
        order.sort_by_key(|i| rank(nodes[*i]));
        order
    }

    /// Look up a labeled argument's compiled sub-Node.
    pub fn arg_named(&self, name: &ArcStr) -> Option<&Node<R>> {
        self.arg(&ArgKey::Named(name.clone()))
    }

    fn arg(&self, key: &ArgKey) -> Option<&Node<R>> {
        self.args.get(key).and_then(|a| a.node.as_ref())
    }

    /// The function expression's compiled Node.
    pub fn fnode(&self) -> &Node<R> {
        &self.fnode
    }

    /// View the [`Apply`] this CallSite is bound to; `None` until a
    /// runtime bind or `try_static_resolve` has populated `self.callee`.
    pub fn resolved_apply(&self) -> Option<ApplyView<'_, R>> {
        self.callee.apply().map(|a| a.view())
    }

    /// The call `spec` compiled: its function and arguments, unbound.
    pub(crate) fn compile(
        ctx: &mut CompileCtx<R>,
        flags: BitFlags<CFlag>,
        spec: TArc<Expr>,
        scope: &Scope,
        top_id: ExprId,
        args: &TArc<[(Option<ArcStr>, Expr)]>,
        f: &TArc<Expr>,
    ) -> Result<Node<R>> {
        let mut f = f;
        while let ExprKind::ExplicitParens(inner) = &f.kind {
            f = inner;
        }
        reject_dead_variadic_call(ctx, scope, f, args).at(&spec)?;
        // a call's function is compiled as written: a trait method named
        // here is the dispatcher itself, never its eta-expansion
        let fnode = match &f.kind {
            ExprKind::Ref { name } => Ref::compile(ctx, f.clone(), scope, top_id, name)?,
            _ => compile(ctx, flags, (**f).clone(), scope, top_id)?,
        };
        let args = compile_apply_args(ctx, flags, scope, top_id, &spec, args)?;
        let rtype = ctx.instance_node_type(spec.id).unwrap_or_else(Type::empty_tvar);
        let site =
            Self::unbound(spec, None, rtype, fnode, args, scope.clone(), flags, top_id);
        Ok(Node::new(CallNode::Call(site)))
    }

    fn clear_prepared_bind(&mut self, ctx: &mut CompileCtx<R>) {
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
        ctx: &mut CompileCtx<R>,
        scope: &Scope,
        flags: BitFlags<CFlag>,
        f: &LambdaDef<R>,
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
            let default_node = self.checked_default(
                ctx,
                flags,
                scope,
                f,
                name,
                expr,
                defaults,
                Judge::Bind,
            )?;
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
        ctx: &mut CompileCtx<R>,
        flags: BitFlags<CFlag>,
        scope: &Scope,
        f: &LambdaDef<R>,
        name: &ArcStr,
        expr: &Expr,
        defaults: &mut Refs,
        judge: Judge,
    ) -> Result<Node<R>> {
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
            None,
            judge,
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
        ctx: &mut CompileCtx<R>,
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
        ctx: &CompileCtx<R>,
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
            None => Qop::<R>::check_unhandled(
                &ctx.env,
                self.flags,
                &self.spec,
                format_args!("error {t} raised from function call {}", self.fnode.spec()),
            ),
        }
    }

    // CR claude for claude: [risk/medium] An omitted labeled default only narrows: it is
    // checked after Widening::settle has set the call's widenable variables, so a
    // default wider than the arguments is refused when omitted and accepted when
    // written. Over `let f = |#x: Array<'a> = [1, null], y: 'a| -> 'a y`, `f(#x: [1,
    // null], 1)` checks and `f(1)` is refused ("Array<'a: i64> does not contain
    // Array<[i64, null]>"); with an open argument, `f(#x: [1, null], str::parse("5"),
    // 1)` checks and `f(str::parse("5"), 1)` is refused, a default-elide TYPEFLIP the
    // fuzzer's typemorph reports. CLAUDE.md says an omitting site's check "may narrow
    // that site's cells", and design/must_reject.md grades default-elide flips EXPECTED.
    // Options: (1) an omitted default takes part in widening as a written labeled
    // argument does (one more closed argument to Widening, before settle), so the two
    // spellings always agree; (2) it stays narrowing-only and the flip is documented
    // (the book's labeled-default section, CLAUDE.md). Recommend (1): a default is an
    // argument the caller did not write, and the widest-argument rule promises a type
    // that does not depend on how the arguments are written. probe:
    // ~/tmp/cr29/scratch/review3/u3/d1_omit.gx against d1_written.gx, flip2_omit.gx
    // against flip2_written.gx (default-widen-01)
    // 2026-10-30 claude: Written on branch cr30-u3 (round 2 of the U3 soak fix, which
    // did not land) and copied here unchanged. On main df6600e9b the closed pair splits
    // as stated (d1_written checks, d1_omit is refused), but the open-argument pair is
    // refused in both spellings: its written form checks only under the branch's
    // widening of open arguments, which is cr-discuss round 4 D1.
    // 2026-10-30 claude: Eric ruled (2026-10-30, cr-discuss round 4 D1): option (c) for
    // arguments, so this CR's recommendation holds: an omitted default widens as a
    // written one does, as one more lower bound under D1's mechanism. Built with
    // widen-open-01.
    // 2026-10-31 claude: Eric ruled (round 5 D1): a widening variable met by a
    // function-typed argument does not widen; it binds as on main, pre-unification on,
    // the first argument deciding. Decisions: 1 (b'): the call's end judges the join,
    // the widest argument must contain every other, else the call is refused naming the
    // argument; 2 yes, a parameter reaching a widening call as a union member stays
    // generic; 3 yes, the variable binds at the call's end; 4 confirmed, provided a
    // census shows the newly refused population is not too big; 5 accept the refusal; 6
    // refuse the call. Lane E continues stages 2 and 3 on cr31-E.
    /// Check every default this site omits, for each definition the
    /// callee may be. False when one is not known here: its default
    /// narrows the site's cells at its bind. `carriers` collects the
    /// labels whose default can carry a value of a cell its type holds.
    fn check_omitted_defaults(
        &self,
        ctx: &mut CompileCtx<R>,
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
            let Some(f) = def.as_ref().and_then(|v| v.downcast_ref::<LambdaDef<R>>())
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
                    Judge::Check,
                )?;
                if carries(&ctx.env, node.typ()) {
                    carriers.push(name.clone());
                }
                ctx.discard(node);
            }
        }
        Ok(known)
    }

    /// Cells the result and throws do not hold, and no argument this
    /// call passes or omits can carry a value of.
    fn for_each_unread(
        &self,
        env: &Env,
        ftype: &FnType,
        carriers: &[ArcStr],
        mut f: impl FnMut(&ArcStr, &TVar),
    ) {
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
            if !read.contains_key(&tv.cell_addr()) {
                f(name, tv)
            }
        }
    }

    /// The signature this site's instance of `f` runs with where the
    /// site's `view` lists other parameters than `f`'s (`None`: the view):
    /// a call's copy of `f`'s fitted to it, each label the view hides
    /// holding the default this site compiled for it.
    fn instance_signature(
        &self,
        ctx: &CompileCtx<R>,
        view: &FnType,
        f: &LambdaDef<R>,
    ) -> Result<Option<FnType>> {
        if same_parameters(view, &f.typ) {
            return Ok(None);
        }
        let fitted = super::lambda::fit_callee(ctx, view, &f.typ)?;
        for farg in fitted.args.iter() {
            let Some(name) = farg.label() else { continue };
            if view.args.iter().any(|a| a.label() == Some(name)) {
                continue;
            }
            if let Some(node) = self
                .args
                .get(&ArgKey::Named(name.clone()))
                .filter(|a| a.stage.is_default())
                .and_then(|a| a.node.as_ref())
            {
                wrap!(node, farg.typ.check_contains(&ctx.env, node.typ()))?;
            }
        }
        Ok(Some(fitted))
    }

    fn init_prepared_bind(
        &mut self,
        ctx: &mut CompileCtx<R>,
        scope: &Scope,
        f: &LambdaDef<R>,
        mode: BindMode<'_>,
    ) -> Result<Box<dyn Apply<R>>> {
        (f.init)(scope, ctx, &mut self.arg_refs, mode, self.top_id)
    }

    /// The instance of `f` a run-time bind dispatches, and the outer
    /// variables the defaults it compiled read.
    fn setup_dynamic_bind(
        &mut self,
        ctx: &mut CompileCtx<R>,
        scope: &Scope,
        flags: BitFlags<CFlag>,
        f: &LambdaDef<R>,
    ) -> Result<(Box<dyn Apply<R>>, SmallVec<[BindId; 2]>, FnParams)> {
        let mut refs = Refs::default();
        self.prepare_bind(ctx, scope, flags, f, &mut refs)?;
        let mut defaults = SmallVec::new();
        refs.with_external_refs(|id| defaults.push(id));
        let context =
            |spec: &Expr| format_compact!("a run-time bind at {spec} did not type");
        // a site its check never typed sees the callee as a call would
        let view = match &self.ftype {
            Some(ft) => {
                let view = ft.resolve_tvars();
                self.instance_signature(ctx, &view, f)
                    .with_context(|| context(&self.spec))?
                    .unwrap_or(view)
            }
            None => f.typ.instantiate(&ctx.rec_defs),
        };
        let mut apply = self
            .init_prepared_bind(ctx, scope, f, BindMode::Dynamic(&view))
            .with_context(|| context(&self.spec))?;
        let params = self.register_fn_params(ctx, &*apply, &apply.typ(), f.id);
        if let Err(e) = apply.typecheck0(ctx, &mut self.arg_refs) {
            Self::unregister_fn_params(ctx, params);
            ctx.discard_apply(apply);
            return Err(e.context(context(&self.spec)));
        }
        params.held.iter().for_each(|id| self.hold_static(ctx, *id));
        Ok((apply, defaults, params))
    }

    /// Elaborate the omitted defaults in `env`, their definition's.
    fn typecheck_static_defaults(
        &mut self,
        ctx: &mut CompileCtx<R>,
        def: &LambdaDef<R>,
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
    fn pending_settle(
        &self,
        ftype: &FnType,
        unread_callbacks: SmallVec<[TVar; 1]>,
    ) -> crate::PendingSettle {
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
        let text = match super::instance_rechecked() {
            true => crate::typ::TextSite::Instance,
            false => crate::typ::TextSite::Written,
        };
        crate::PendingSettle::Site {
            sigs: smallvec::SmallVec::new(),
            unread_callbacks,
            ftype: ftype.clone(),
            rtype,
            exempt,
            text,
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
    fn typecheck1_resolve(&mut self, ctx: &mut CompileCtx<R>) -> Result<Option<Node<R>>> {
        if let Some(lowered) = self.try_static_resolve(ctx)? {
            return Ok(Some(lowered));
        }
        self.refresh_static_ftype();
        Ok(None)
    }

    fn setup_static_bind(
        &mut self,
        ctx: &mut CompileCtx<R>,
        scope: &Scope,
        flags: BitFlags<CFlag>,
        f: &LambdaDef<R>,
    ) -> Result<(Box<dyn Apply<R>>, FnType)> {
        let _profile = profile::phase(Phase::StaticBind);
        self.prepare_bind(ctx, scope, flags, f, &mut Refs::default())?;
        if self.ftype.is_none() {
            bail!("statically resolving an untyped call site: {}", self.spec)
        }
        let site_ftype = self.ftype.as_ref().unwrap().resolve_tvars();
        let instance_ftype = match self.instance_signature(ctx, &site_ftype, f)? {
            Some(fitted) => fitted.resolve_tvars(),
            None => site_ftype,
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
        ctx: &mut ExecCtx<'_, R>,
        fv: Value,
        f: &LambdaDef<R>,
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
    pub(crate) fn prebind(&mut self, ctx: &mut CompileCtx<R>) {
        if !matches!(self.callee, Callee::DynamicUnbound) {
            return;
        }
        let NodeView::Constant(c) = self.fnode.view() else { return };
        let fv = c.value.clone();
        let Some(f) = fv.downcast_ref::<LambdaDef<R>>() else { return };
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
        ctx: &mut CompileCtx<R>,
        fv: Value,
        f: &LambdaDef<R>,
    ) -> Result<SmallVec<[BindId; 2]>> {
        let _bind_span = perfdbg::span(&perfdbg::BIND_NS);
        if perfdbg::enabled() {
            perfdbg::BIND_CALLS.fetch_add(1, Relaxed);
        }
        crate::with_attribute_frame(ctx, |ctx| {
            let (scope, flags) = (self.scope.clone(), self.flags);
            // The lazy-bound body's typecheck defers settles no statement
            // boundary will drain.
            let (defaults, tail_loops) = super::with_runtime_settles(ctx, |ctx| {
                let setup_span = perfdbg::span(&perfdbg::SETUP_NS);
                let (apply, defaults, params) =
                    self.setup_dynamic_bind(ctx, &scope, flags, f)?;
                drop(setup_span);
                let res = self.elaborate_bound(ctx, fv, f, apply);
                Self::unregister_fn_params(ctx, params);
                res.map(|tail_loops| (defaults, tail_loops))
            })?;
            // CR claude for eric: [perf/low] A run-time bind fuses only a collection
            // slot's instance and one an assertion it reaches covers (`asserted`): a
            // function value called with no attribute in reach, `let g = select b {
            // true => fib, .. }; g(20)`, runs every level of its recursion in the
            // node-walk where `fib(20)` named fuses (GXDBG_CS: the same dispatch count
            // with and without fusion, no INSTANCE-FUSION line). Fusing every bind
            // would make a function value cost what a named call does, at a JIT
            // compile for each new instance at run time (fusion::fuse_instance caches
            // by definition and type, so a later bind of it takes the kernels). Should
            // every run-time bind fuse? probe:
            // ~/ptmp/cr29/scratch/review-r6/E2-r1-1/b8.gx (bound-bind-policy-01)
            if !ctx.fusion.disabled() {
                // an assertion the bind reaches asks for the kernels its
                // check reads
                let asserted =
                    !ctx.attr_census.lock().is_empty() || tail_loops.checking();
                if let Some(apply) = self.callee.apply_mut() {
                    if let Some(share) = &self.share {
                        share::fuse_slot(ctx, share, self.top_id, |ctx| apply.fuse(ctx))?;
                    } else if asserted && ctx.fusion.available() {
                        fusion::fuse_instance(ctx, self.top_id, apply)?;
                    }
                }
                crate::check_attribute_census(ctx)?;
                if ctx.fusion.available()
                    && let Some(ApplyView::Lambda(g)) = self.resolved_apply()
                {
                    analysis::check_bound_tail_loops(tail_loops, &self.spec, g, ctx);
                }
            }
            Ok(defaults)
        })
    }

    /// Elaborate and analyze `apply`, this site's run-time instance of
    /// `f` (the value `fv`), checked with its fn-typed formals registered,
    /// leaving it `DynamicBound`: the `#[tail_recursive]`s its analysis
    /// passed.
    fn elaborate_bound(
        &mut self,
        ctx: &mut CompileCtx<R>,
        fv: Value,
        f: &LambdaDef<R>,
        apply: Box<dyn Apply<R>>,
    ) -> Result<analysis::TailLoops> {
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
        let mut tail_loops = analysis::TailLoops::default();
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
                tail_loops = analysis::analyze_bound_callee(g, self_bind, ctx);
            }
        }
        if restored_def {
            ctx.lambda_defs.remove(&f.id);
        }
        Ok(tail_loops)
    }

    /// Prime `defaults`, the outer variables a fresh bind's compiled
    /// defaults read, with their stored values, and run the defaults for
    /// the first time, under the init view.
    fn prime_bound(
        &mut self,
        ctx: &mut ExecCtx<'_, R>,
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
        ctx: &mut CompileCtx<R>,
        def: &LambdaDef<R>,
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
        ctx: &mut CompileCtx<R>,
        def: &LambdaDef<R>,
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
    fn omitted_labels(&self, def: &LambdaDef<R>) -> SmallVec<[ArcStr; 2]> {
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
        ctx: &mut CompileCtx<R>,
        def: &LambdaDef<R>,
        identity: FnArgIdentity,
    ) -> Result<()> {
        let scope = self.scope.clone();
        let context =
            |spec: &Expr| format_compact!("in the instance of {spec} at this call site");
        let (apply, instance_ftype) = self
            .setup_static_bind(ctx, &scope, self.flags, def)
            .with_context(|| context(&self.spec))?;
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
        self.callee = Callee::to_static(apply, true);
        // a static bind's defaults run from its first update
        for arg in self.args.values_mut() {
            if arg.stage == ArgStage::Compiled {
                arg.stage = ArgStage::Running;
            }
        }
        // Fn-typed args are registered under the instance's param
        // BindIds for the whole body typecheck (`register_fn_params`).
        let param_binds = {
            let apply = self.callee.apply().expect("static callee must have an apply");
            self.register_fn_params(ctx, apply, &instance_ftype, def.id)
        };
        param_binds.held.iter().for_each(|id| self.hold_static(ctx, *id));
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
        .with_context(|| context(&self.spec));
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
    fn try_static_resolve(&mut self, ctx: &mut CompileCtx<R>) -> Result<Option<Node<R>>> {
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
                    let id = r.id;
                    let fv = ctx.bind_to_lambda.get(&id).cloned();
                    if fv.is_some() {
                        self.hold_static(ctx, id);
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
        let Some(def) = fv.downcast_ref::<LambdaDef<R>>() else {
            return Ok(None);
        };
        self.resolve_static(ctx, def).map(|()| None)
    }

    /// This site's instantiation identity ([`FnArgIdentity`]): per
    /// written argument, the source lambda it resolves to (a literal is
    /// its own source; a `Ref` goes through `bind_to_lambda`; a `<-`
    /// target is dynamic). A default is the definition's, not the site's,
    /// so it is left out wherever the bind has got to.
    fn fn_arg_identity(&self, ctx: &CompileCtx<R>) -> FnArgIdentity {
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
                        .and_then(|fv| fv.downcast_ref::<LambdaDef<R>>())
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
    /// param BindIds of `apply`, its instance of `lid`. Held for the whole
    /// body typecheck so calls to and captures of a fn parameter resolve
    /// in one pass. An arg that is an enclosing instance's formal, whose
    /// check has ended, passes on only the definition forwarded to it
    /// (`fn_forward_resolutions`, which the instance's delete drops): the
    /// analysis judges the calls through it by that definition, and they
    /// stay dynamic.
    fn register_fn_params(
        &self,
        ctx: &mut CompileCtx<R>,
        apply: &dyn Apply<R>,
        ftype: &FnType,
        lid: LambdaId,
    ) -> FnParams {
        let mut params = FnParams::default();
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
                    if let Some(def) = fv.downcast_ref::<LambdaDef<R>>() {
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
                        params.held.push(r.id);
                        if let Some(def) = fv.downcast_ref::<LambdaDef<R>>() {
                            ctx.fn_forward_resolutions.insert(id, def.id);
                        }
                        ctx.bind_to_lambda.insert(id, fv);
                        param_binds.push(id);
                    } else if let Some(lid) =
                        ctx.fn_forward_resolutions.get(&r.id).copied()
                    {
                        ctx.fn_forward_resolutions.insert(id, lid);
                    }
                }
                _ => {}
            }
        }
        params
    }

    /// Undo [`Self::register_fn_params`]; the `fn_forward_resolutions`
    /// snapshot stays for the kernel cache fingerprint.
    fn unregister_fn_params(ctx: &mut CompileCtx<R>, mut params: FnParams) {
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
    ) -> Result<(LPooled<Vec<(ArcStr, Node<R>)>>, LPooled<Vec<(Option<ArcStr>, ArcStr)>>)>
    {
        let ExprKind::Apply(ApplyExpr { args, .. }) = &self.spec.kind else {
            bail!("call site without an apply spec: {}", self.spec)
        };
        let mut operands: LPooled<Vec<(ArcStr, Node<R>)>> = LPooled::take();
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
        ctx: &mut CompileCtx<R>,
        node: Node<R>,
    ) -> Result<Node<R>> {
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
    pub(super) fn retarget(&mut self, ctx: &mut CompileCtx<R>, bind: BindId) {
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

impl<R: Rt> CallSite<R> {
    fn update_call(&mut self, ctx: &mut ExecCtx<'_, R>) -> &TagValue {
        let woke = self.slept.take();
        let rebirth = !matches!(mem::take(&mut self.kept), Kept::Nothing);
        let reborn = woke && (rebirth || ctx.event.born());
        if reborn && let Some(f) = self.callee.apply_mut() {
            f.reborn(ctx)
        }
        if matches!(self.callee, Callee::Imaged { .. })
            && let Err(e) = self.materialize(ctx).map(|()| self.decoded = true)
        {
            warn!("decoding the instance of {}: {e:#}; resolving it afresh", self.spec);
            self.release_held(ctx);
            self.callee = Callee::DynamicUnbound;
        }
        // a first bind reads every quiet production; a bound site's rebind
        // finds its arguments' standing values in the store, which its
        // first bind stood them in, and compiles its defaults afresh; a
        // constant function never rebinds
        let keep = match &self.callee {
            _ if rebirth => Keep::Every,
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
            // with catch-up when the same function returns, or is born then
            // if it was due a birth meanwhile
            if let Some(f) = self.callee.apply_mut() {
                let absent = self.absent.get_or_insert_with(|| {
                    Box::new(Absent {
                        missed: SmallVec::new(),
                        captured: TrackedFires::new(
                            &ctx.env,
                            ctx.rt.cycle(),
                            1,
                            |_, r| f.refs(r),
                        ),
                        unborn: None,
                    })
                });
                if reborn {
                    let view = if rebirth {
                        ctx.event.view.join(View::Birth)
                    } else {
                        ctx.event.view
                    };
                    let frame = super::wake::in_fresh_frame(ctx);
                    absent.unborn = Some(Unborn { view, frame });
                }
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
        } || rebirth;
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
        let decoded = mem::take(&mut self.decoded);
        let absent = self.absent.take();
        let returned = absent.is_some();
        let res = match self.callee.apply_mut() {
            None => None,
            Some(f) if !bound => {
                let arg_refs = &mut self.arg_refs;
                match absent {
                    None => Some(f.update(ctx, arg_refs).clone()),
                    Some(absent) => Some((*absent).dispatch(ctx, f, arg_refs)),
                }
            }
            Some(f) => {
                // A fresh bind dispatches under the init view, owed the
                // fires its body's inputs had while the arm around it slept.
                let owed = super::wake::deliver_owed(ctx, |r| f.refs(r));
                let arg_refs = &mut self.arg_refs;
                let tv = ctx.under(View::Birth, |ctx| f.update(ctx, arg_refs).clone());
                TrackedFires::restore(ctx.event, owed);
                Some(tv)
            }
        };
        if decoded && let Some(f) = self.callee.apply_mut() {
            f.for_each_body_mut(&mut |n| Self::hold_imaged(ctx, n));
            ctx.apply_deferred();
        }
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
            Some(tv) if returned || (!static_callee && fnode_tag.is_fired()) => {
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
        ctx: &mut ExecCtx<'_, R>,
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
        let Some(lb) = v.downcast_ref::<LambdaDef<R>>() else {
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

impl<R: Rt> Arg<R> {
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
        ctx: &mut ExecCtx<'_, R>,
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
        Ok((key, Arg::new(id, node, stage, spec)))
    }
}

impl RefsSummary {
    fn of_sets(refed: &IntSet<BindId>, bound: &IntSet<BindId>) -> Self {
        let sorted = |ids: &IntSet<BindId>| {
            let mut v: Vec<BindId> = ids.iter().copied().collect();
            v.sort_unstable();
            v
        };
        RefsSummary { refed: sorted(refed), bound: sorted(bound) }
    }

    fn add_to(&self, refs: &mut Refs) {
        refs.refed.extend(self.refed.iter().copied());
        refs.bound.extend(self.bound.iter().copied());
    }
}

impl<R: Rt> CallSite<R> {
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
    fn record_instance_refs(apply: &dyn Apply<R>) -> Result<(), PackError> {
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
            let holds = refs.take_static_holds();
            let own = RefsSummary::of_sets(&refs.refed, &refs.bound);
            image::encoding(|e| {
                e.ext::<image::Compiled>()
                    .instance_refs
                    .insert(instance, InstanceRefs { own, callees, holds })
            });
        }
        Ok(())
    }

    /// Release what this site registered for an imaged tree.
    fn release_held(&self, ctx: &mut ExecCtx<'_, R>) {
        if let Callee::Imaged { instance, refs, reader, held: true, .. } = &self.callee {
            for id in refs.flat(*instance).refed.iter() {
                ctx.unref_reader(*id, reader.clone())
            }
        }
    }

    /// Register the reads of every imaged site in `node` that holds none:
    /// the sites a just decoded instance's first update left imaged.
    fn hold_imaged(ctx: &mut ExecCtx<'_, R>, node: &mut Node<R>) {
        ensure_sufficient(|| {
            if let Some(site) =
                node.downcast_mut::<CallNode<R>>().and_then(CallNode::call_mut)
                && let Callee::Imaged { instance, refs, reader, held, .. } =
                    &mut site.callee
                && !*held
            {
                for id in refs.flat(*instance).refed.iter() {
                    ctx.record_reader(*id, reader.clone())
                }
                *held = true;
            }
            node.for_each_child_mut(&mut |c| Self::hold_imaged(ctx, c))
        })
    }

    /// Decode the instance the image holds for this site and bind it.
    fn materialize(&mut self, ctx: &mut ExecCtx<'_, R>) -> Result<()> {
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
                let outer =
                    mem::replace(&mut dec.ext::<image::Restored>().materializing, true);
                let decoded = image::DecodeImage::with(&mut dec, || {
                    let Some(mut sub) = image.get(at as usize..) else {
                        bail!("instance {instance:?} at {at} is past the image")
                    };
                    GXLambda::image_decode(ctx, &mut sub)
                        .map_err(|e| anyhow!("instance {instance:?} at {at}: {e:?}"))
                });
                dec.ext::<image::Restored>().materializing = outer;
                decoded
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
        let apply: Box<dyn Apply<R>> = match decoded {
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
        self.callee = Callee::to_static(apply, first_update);
        Ok(())
    }

    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<'_, R>,
        buf: &mut &[u8],
    ) -> Result<Node<R>, PackError> {
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
        let mut imaged = None;
        let (arg_refs, callee) = match buf.get_u8() {
            CALLEE_UNBOUND => (Vec::new(), Callee::DynamicUnbound),
            CALLEE_INSTANCE => {
                let arg_refs = decode_nodes(ctx, buf)?;
                let callee = match bool::decode(buf)? {
                    false => {
                        let apply: Box<dyn Apply<R>> =
                            Box::new(GXLambda::image_decode(ctx, buf)?);
                        Callee::to_static(apply, bool::decode(buf)?)
                    }
                    true => {
                        let instance = LambdaInstanceId::decode(buf)?;
                        let first_update = bool::decode(buf)?;
                        let table = image::decoding(|d| {
                            d.ext::<image::Restored>().instance_refs.clone()
                        })
                        .ok_or(PackError::Application(image::NOT_IMAGED))?;
                        let refs = ImagedRefs { table, flat: OnceLock::new() };
                        imaged = Some((instance, first_update, refs));
                        Callee::DynamicUnbound
                    }
                };
                (arg_refs, callee)
            }
            CALLEE_BUILTIN => {
                let arg_refs = decode_nodes(ctx, buf)?;
                let apply: Box<dyn Apply<R>> =
                    Box::new(BuiltInLambda::image_decode(ctx, &arg_refs, buf)?);
                (arg_refs, Callee::to_static(apply, bool::decode(buf)?))
            }
            _ => return Err(PackError::UnknownTag),
        };
        let static_target = Option::<StaticCallTarget>::decode(buf)?;
        let recursive_edge = bool::decode(buf)?;
        let flags = image::flags_decode(buf)?;
        let scope = image::scope_decode(buf)?;
        let top_id = ExprId::decode(buf)?;
        let fork = ForkSite::image_decode(buf)?;
        let n = crate::image::count_decode(buf)?;
        let held = (0..n)
            .map(|_| BindId::decode(buf))
            .collect::<Result<SmallVec<[BindId; 1]>, _>>()?;
        let mut site =
            Self::unbound(spec, ftype, rtype, fnode, args, scope, flags, top_id);
        site.fork = fork;
        held.into_iter().for_each(|id| site.hold_static(ctx, id));
        site.arg_refs = arg_refs;
        site.callee = match imaged {
            None => callee,
            Some((instance, first_update, refs)) => {
                let held = !image::decoding(|d| d.ext::<image::Restored>().materializing)
                    .unwrap_or(false);
                let reader = ctx.reader(top_id);
                Callee::Imaged { instance, first_update, refs, reader, held }
            }
        };
        site.static_target = static_target;
        site.recursive_edge = AtomicBool::new(recursive_edge);
        // an imaged callee's reads schedule this site's boundary before its
        // body decodes, as a cold compile's registrations do; the site holds
        // them until the body decodes and registers its own (recorded, so a
        // failed read unwinds them)
        // and holds what its tree binds statically for as long as it lives:
        // an imaged site inside a decoding instance is under one that does
        if let Callee::Imaged { instance, refs, reader, held: true, .. } = &site.callee {
            for id in refs.flat(*instance).refed.iter() {
                ctx.record_reader(*id, reader.clone())
            }
            refs.reach(*instance, &mut |r| {
                site.static_holds
                    .extend(r.holds.iter().map(|(id, call)| ctx.hold_static(*id, call)))
            });
        }
        Ok(Node::new(CallNode::Call(site)))
    }
}

/// A call site as a node: the call, or, once its check lowered it (a
/// trait call over a union self, a self never produced), the node that
/// stands for it, with no call parts left to reach.
#[derive(Debug)]
pub(crate) enum CallNode<R: Rt> {
    Call(CallSite<R>),
    /// The call's lowering, under the call's own spec: errors and the
    /// attribute walk name the user's call.
    Lowered(Node<R>, TArc<Expr>),
}

impl<R: Rt> CallNode<R> {
    /// The call, unless it was lowered.
    pub(crate) fn call_mut(&mut self) -> Option<&mut CallSite<R>> {
        match self {
            Self::Call(c) => Some(c),
            Self::Lowered(..) => None,
        }
    }
}

impl<R: Rt> Update<R> for CallNode<R> {
    fn for_each_child<'a>(&'a self, f: &mut dyn FnMut(&'a Node<R>)) {
        match self {
            Self::Call(c) => {
                c.args.values().filter_map(|a| a.node.as_ref()).for_each(&mut *f);
                f(&c.fnode)
            }
            Self::Lowered(n, _) => n.for_each_child(f),
        }
    }

    fn for_each_child_mut(&mut self, f: &mut dyn FnMut(&mut Node<R>)) {
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

    fn update(&mut self, ctx: &mut ExecCtx<'_, R>) -> &TagValue {
        match self {
            Self::Call(c) => c.update(ctx),
            Self::Lowered(n, _) => n.update(ctx),
        }
    }

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R>) {
        match self {
            Self::Call(c) => c.delete(ctx),
            Self::Lowered(n, _) => n.delete(ctx),
        }
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R>) {
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

    fn typecheck0(&mut self, ctx: &mut CompileCtx<R>) -> Result<()> {
        match self {
            Self::Call(c) => c.typecheck0(ctx),
            Self::Lowered(n, _) => n.typecheck0(ctx),
        }
    }

    fn typecheck0_instance(
        &mut self,
        ctx: &mut CompileCtx<R>,
        types: &mut InstanceTypes,
    ) -> Result<()> {
        match self {
            Self::Call(c) => c.typecheck0_instance(ctx, types),
            // a lowering is built by elaboration: no table types it
            Self::Lowered(n, _) => n.typecheck0(ctx),
        }
    }

    fn typecheck1(&mut self, ctx: &mut CompileCtx<R>) -> Result<()> {
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

    fn view(&self) -> NodeView<'_, R> {
        match self {
            Self::Call(c) => c.view(),
            Self::Lowered(n, _) => n.view(),
        }
    }

    fn fuse(&mut self, ctx: &mut CompileCtx<R>) -> Result<Option<Node<R>>> {
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
impl<R: Rt> CallSite<R> {
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
                    let body: &'static dyn Apply<R> =
                        unsafe { mem::transmute::<&dyn Apply<R>, _>(&**apply) };
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
        self.fork.image_encode(buf)?;
        encode_varint(self.static_holds.len() as u64, buf);
        self.static_holds.iter().try_for_each(|h| h.id.encode(buf))
    }

    pub(crate) fn update(&mut self, ctx: &mut ExecCtx<'_, R>) -> &TagValue {
        self.update_call(ctx)
    }

    pub(crate) fn delete(&mut self, ctx: &mut ExecCtx<'_, R>) {
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

    pub(crate) fn sleep(&mut self, ctx: &mut ExecCtx<'_, R>) {
        self.slept.set();
        // A recursive edge deselected by a shrink deletes its callee, so
        // re-reaching this depth binds a fresh activation, or keeps a pure
        // one asleep (`Kept`).
        if super::in_deselected_arm() && self.is_recursive_edge() {
            if self.callee_is_pure(ctx) {
                if let Kept::Nothing = self.kept {
                    self.kept = Kept::Asleep
                }
                super::kept_in_arm();
                if let Some(f) = self.callee.apply_mut() {
                    f.sleep(ctx)
                }
            } else if let Some(mut f) = self.callee.take_apply() {
                self.kept = Kept::Nothing;
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

    /// An exit of the recursion past this edge's kept callee: the
    /// deepest `shed_levels(levels, exits)` of the `levels` it held at the
    /// first exit are deleted, the callee itself once that is all of them.
    /// Whether anything stays kept.
    fn shed(&mut self, ctx: &mut ExecCtx<'_, R>) -> bool {
        let (levels, exits) = match (self.kept, self.callee.apply_mut()) {
            (Kept::Nothing, _) => return false,
            (_, None) => {
                self.kept = Kept::Nothing;
                return false;
            }
            (Kept::Asleep, Some(f)) => (kept_levels(f), 1),
            (Kept::Shedding { levels, exits }, Some(_)) => (levels, exits + 1),
        };
        let keep = levels - shed_levels(levels, exits);
        if keep == 0 {
            if let Some(mut f) = self.callee.take_apply() {
                f.delete(ctx)
            }
            self.kept = Kept::Nothing;
            return false;
        }
        if let Some(f) = self.callee.apply_mut() {
            cut_below(ctx, f, keep)
        }
        self.kept = Kept::Shedding { levels, exits };
        true
    }

    /// Whether the bound callee is a lambda whose definition is pure
    /// (sync, no per-activation state): nothing in it outlives a birth.
    fn callee_is_pure(&self, ctx: &ExecCtx<'_, R>) -> bool {
        match self.callee.apply().map(|a| a.view()) {
            Some(ApplyView::Lambda(g)) => crate::analysis::lambda_def(ctx, g.id())
                .is_some_and(|d| d.facts.lock().is_pure()),
            _ => false,
        }
    }

    pub(crate) fn spec(&self) -> &Expr {
        &self.spec
    }

    pub(crate) fn typecheck0(&mut self, ctx: &mut CompileCtx<R>) -> Result<()> {
        wrap!(self.fnode, self.fnode.typecheck0(ctx))?;
        let mut fresh = false;
        let mut self_call = false;
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
                    self_call = true;
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
        let mut expanding = SmallVec::new();
        let mut clean: LPooled<AHashSet<usize>> = LPooled::take();
        let mut formals: SmallVec<[_; 8]> = ftype
            .args
            .iter()
            .zip(ArgKey::of_formals(&ftype.args))
            .map(|(farg, key)| {
                let fixed =
                    holds_invariantly(&ctx.env, &farg.typ, &mut expanding, &mut clean);
                (!fixed, farg, key)
            })
            .collect();
        formals.sort_by_key(|(later, ..)| *later);
        for (_, farg, key) in formals {
            if let Some(n) = self.args.get_mut(&key).and_then(|a| a.node.as_mut()) {
                let waits = widening.waits(&ctx.env, self.fnode.typ(), ftype, &farg.typ);
                let hint = widening.hint(&ctx.env, self.fnode.typ(), ftype, &farg.typ);
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
                    let waits = widening.waits(&ctx.env, self.fnode.typ(), ftype, typ);
                    let hint = widening.hint(&ctx.env, self.fnode.typ(), ftype, typ);
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
        let mut unread_callbacks = SmallVec::new();
        // a recursive call shares its definition's cells, which the
        // defaults it omits narrow as a fresh instance's
        if fresh || self_call {
            let mut carriers = SmallVec::new();
            self.defaults_open =
                !self.check_omitted_defaults(ctx, ftype, &mut carriers)?;
            if fresh && !self.defaults_open {
                if static_builtin(ctx, self).is_some() {
                    self.for_each_unread(&ctx.env, ftype, &carriers, |name, tv| {
                        if ftype.quantifiers.contains(name) {
                            tv.settle_unread();
                        }
                    });
                } else {
                    let mut callbacks: LPooled<AHashMap<usize, TVar>> = LPooled::take();
                    for arg in ftype.args.iter().filter(|a| a.typ.holds_fn(&ctx.env)) {
                        arg.typ.reached_cells(&mut callbacks);
                    }
                    self.for_each_unread(&ctx.env, ftype, &carriers, |_, tv| {
                        if callbacks.contains_key(&tv.cell_addr()) {
                            unread_callbacks.push(tv.clone());
                        }
                    });
                }
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
        let settle = self.pending_settle(ftype, unread_callbacks);
        ctx.pending_settles.last_mut().expect("settle frame").push(settle);
        Ok(())
    }

    pub(crate) fn typecheck0_instance(
        &mut self,
        ctx: &mut CompileCtx<R>,
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
        ctx: &mut CompileCtx<R>,
    ) -> Result<Option<Node<R>>> {
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
        let settle = self.pending_settle(&ftype, SmallVec::new());
        ctx.pending_settles.last_mut().expect("root settle frame").push(settle);
        Ok(None)
    }

    pub(crate) fn refs(&self, refs: &mut Refs) {
        if !refs.skip_callees {
            if let Some(fun) = self.callee.apply() {
                match (fun.view(), refs.callees.as_mut()) {
                    (ApplyView::Lambda(g), Some(callees)) => {
                        callees.push(g.instance_id())
                    }
                    _ => fun.refs(refs),
                }
            }
            if let Callee::Imaged { instance, refs: imaged, .. } = &self.callee {
                imaged.flat(*instance).add_to(refs);
            }
        }
        refs.static_holds(&self.static_holds);
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

    pub(crate) fn view(&self) -> NodeView<'_, R> {
        NodeView::CallSite(self)
    }

    pub(crate) fn fuse(&mut self, ctx: &mut CompileCtx<R>) -> Result<Option<Node<R>>> {
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
                        self.spec.kind
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
        if let Some(info) = cx.lambda_site(self.spec.id).cloned() {
            return emit_lambda_call_node(cx, self, Called::Other(&info));
        }
        if self.is_recursive_edge() {
            bail!("emit_clif: mutually recursive static call edge is not supported")
        }
        // A `MarshalArg::Call(i)` indexes the source-order arg list, which
        // spans labeled and positional args; `Default(name)` is this site's
        // compiled default node.
        let info = match cx.builtin_site(self.spec.id) {
            Some(info) => info.clone(),
            None if self.callee.apply().is_none() => {
                return Err(fusion::blocker(
                    &self.spec,
                    format_compact!(
                        "emit_clif: call `{}` has no static callee: its function is \
                         a run-time value, so it dispatches at run time",
                        self.spec.kind
                    ),
                ));
            }
            None => {
                return Err(fusion::blocker(
                    &self.spec,
                    format_compact!(
                        "emit_clif: builtin call site `{}` not discovered — doesn't fuse",
                        self.spec.kind
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
            .collect::<Result<SmallVec<[&Node<R>; 8]>>>()?;
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
/// deselected arm is at its wake, or the birth it was due meanwhile,
/// which it makes up then.
#[derive(Debug)]
struct Absent {
    /// The arguments that fired meanwhile.
    missed: SmallVec<[BindId; 2]>,
    /// The inputs the instance captures.
    captured: TrackedFires,
    unborn: Option<Unborn>,
}

/// A birth an instance was due while its function was bottom: the view
/// it was due (`Birth` or `Step`), and whether a seq step issued it as a
/// fresh frame ([`FreshFrame`]).
#[derive(Debug, Clone, Copy)]
struct Unborn {
    view: View,
    frame: bool,
}

impl Absent {
    /// Dispatch `f` as the same function returns: the birth it was due,
    /// else a resume caught up.
    fn dispatch<R: Rt>(
        self,
        ctx: &mut ExecCtx<'_, R>,
        f: &mut dyn Apply<R>,
        from: &mut [Node<R>],
    ) -> TagValue {
        match self.unborn {
            Some(Unborn { view, frame: true }) => {
                let frame = FreshFrame::open(ctx, |r| {
                    f.refs(r);
                    from.iter().for_each(|n| n.refs(r))
                });
                let tv = ctx.under(view, |ctx| f.update(ctx, from).clone());
                frame.close(ctx);
                tv
            }
            unborn => {
                let view = unborn.map_or(View::Wake, |u| u.view);
                self.wake(ctx, view, |ctx| f.update(ctx, from).clone())
            }
        }
    }

    /// Record this cycle's fires: the arguments published in `set`, and
    /// the captured inputs.
    fn observe<R: Rt>(&mut self, ctx: &ExecCtx<'_, R>, set: &Published) {
        for id in set.iter() {
            let fired = ctx.event.variables.get(id).is_some_and(|tv| tv.tag().triggers());
            if fired && !self.missed.contains(id) {
                self.missed.push(*id);
            }
        }
        self.captured.observe(ctx);
    }

    /// Run `update` under `view` with one catch-up fire for each argument
    /// and captured input that fired unseen and is not delivered live
    /// this cycle, at its current value.
    fn wake<R: Rt, T>(
        mut self,
        ctx: &mut ExecCtx<'_, R>,
        view: View,
        update: impl FnOnce(&mut ExecCtx<'_, R>) -> T,
    ) -> T {
        let mut injected = super::wake::Injected::new();
        for id in self.missed.drain(..) {
            super::wake::inject(ctx, id, &mut injected);
        }
        let captured = &mut self.captured;
        let r = ctx.under(view, |ctx| {
            let caught_up = captured.deliver(ctx, 0);
            let scope = captured.open(ctx, 0);
            let r = update(ctx);
            captured.ran(ctx, 0, scope);
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
fn update_args<R: Rt>(
    ctx: &mut ExecCtx<'_, R>,
    args: &mut indexmap::map::Slice<ArgKey, Arg<R>>,
    site: &mut ForkSite,
    pass: Pass,
) -> ArgsOut {
    let n = args.len();
    site.decide_siblings(ctx, n, args.values().filter_map(|a| a.node.as_ref()));
    let in_order = |c: &mut ExecCtx<'_, R>, p, m: Option<&mut Meter<'_>>| {
        update_args_in_order(c, p, m, pass)
    };
    crate::branch::fork_point(ctx, args, site, in_order, |mut a, b| {
        a.fired |= b.fired;
        a.prods.extend(b.prods);
        a.set.extend(b.set);
        a
    })
}

fn update_args_in_order<R: Rt>(
    ctx: &mut ExecCtx<'_, R>,
    args: &mut indexmap::map::Slice<ArgKey, Arg<R>>,
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
pub(crate) fn publish_production<R: Rt>(
    ctx: &mut ExecCtx<'_, R>,
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
pub(crate) fn lambda_literal<R: Rt>(node: &Node<R>) -> Option<&Lambda> {
    match node.view() {
        NodeView::Lambda(l) => Some(l),
        NodeView::Sample(s) => match s.arg.node.view() {
            NodeView::Lambda(l) => Some(l),
            _ => None,
        },
        _ => None,
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{
        SourcePosition,
        expr::{
            ModPath,
            parser::{parse_one, parse_type},
        },
    };
    use netidx_value::Typ;

    // A type whose nodes many paths share is walked once per node, not once
    // per path: `let x1 = (x0, x0); let x2 = (x1, x1); ..` passed to a call.
    #[test]
    fn rank2_parts_walks_a_shared_node_once() {
        let mut t = Type::Primitive(Typ::I64.into());
        for _ in 0..16 {
            t = Type::Tuple(TArc::from_iter([t.clone(), t.clone()]));
        }
        RANK2_STEPS.with(|n| n.set(0));
        let mut out = Quantified::new();
        let r = rank2_parts(&Env::default(), &t, &mut Expanding::default(), &mut out);
        assert!(r.is_none() && out.is_empty());
        assert!(RANK2_STEPS.with(|n| n.get()) <= 2 * 17);
    }

    // So is one each of whose paths ends in a quantified function: `let x0 =
    // (g, 1)` with `g: fn<'b: Number>(..)`, its quantifier taken once.
    #[test]
    fn rank2_parts_walks_a_quantified_dag_once() {
        let g = parse_type("fn<'b: Number>(x: 'b) -> 'b").unwrap();
        let mut t = Type::Tuple(TArc::from_iter([g, Type::Primitive(Typ::I64.into())]));
        for _ in 0..16 {
            t = Type::Tuple(TArc::from_iter([t.clone(), t.clone()]));
        }
        RANK2_STEPS.with(|n| n.set(0));
        let mut out = Quantified::new();
        let r = rank2_parts(&Env::default(), &t, &mut Expanding::default(), &mut out);
        assert!(r.is_none() && out.len() == 1);
        assert!(RANK2_STEPS.with(|n| n.get()) <= 2 * 17 + 3);
    }

    // A call's escape checks walk the argument's shared nodes once:
    // `carried_out` over a 24-deep shared tuple holding a cell.
    #[test]
    fn reaches_cell_walks_a_shared_node_once() {
        let mut t = Type::Tuple(TArc::from_iter([
            Type::empty_tvar(),
            Type::Primitive(Typ::I64.into()),
        ]));
        for _ in 0..24 {
            t = Type::Tuple(TArc::from_iter([t.clone(), t.clone()]));
        }
        RANK2_STEPS.with(|n| n.set(0));
        assert!(carried_out(&[], &t).is_none());
        assert!(RANK2_STEPS.with(|n| n.get()) <= 2 * (2 * 25 + 2));
    }

    // A formal holding no quantified function is not walked, a chain of
    // parametric typedefs whose expansions no walk can share included:
    // `type P1<'a> = (P0<'a>, P0<'a>); ..` and `let x: P16<i64>`.
    fn typedef_chain() -> Env {
        chain_of("P", "'a", "('a, i64)")
    }

    fn chain_of(name: &str, params: &str, base: &str) -> Env {
        chain_passing(name, params, base, params)
    }

    /// `type <name>0<params> = base; type <name>1<params> = (<name>0<pass>,
    /// <name>0<pass>); ..` to depth 16.
    fn chain_passing(name: &str, params: &str, base: &str, pass: &str) -> Env {
        let mut env = Env::default();
        let mut defs = vec![format!("type {name}0<{params}> = {base}")];
        for i in 1..=16 {
            let below = format!("{name}{}<{pass}>", i - 1);
            defs.push(format!("type {name}{i}<{params}> = ({below}, {below})"));
        }
        for d in defs.iter() {
            let e = parse_one(d).unwrap();
            let ExprKind::TypeDef(td) = &e.kind else { panic!("not a typedef: {d}") };
            let (params, body) = Type::reset_typedef_tvars(&td.params, &td.body);
            let at = SourcePosition::default();
            env.deftype(
                &ModPath::root(),
                &td.name.name,
                params,
                &body,
                true,
                None,
                at,
                at,
                e.ori.clone(),
            )
            .unwrap();
        }
        env
    }

    #[test]
    fn quantified_formal_skips_a_formal_holding_none() {
        let env = typedef_chain();
        let t = parse_type("P16<i64>").unwrap();
        assert!(t.lookup_ref_probe(&env).is_some());
        RANK2_STEPS.with(|n| n.set(0));
        assert!(quantified_formal(&env, &t).is_none());
        assert_eq!(RANK2_STEPS.with(|n| n.get()), 0);
    }

    // The walk that asks whether a type holds one only looks: it decides no
    // name it meets, which another task's check may resolve in another
    // environment.
    #[test]
    fn quantified_formal_decides_no_name() {
        let env = typedef_chain();
        let t = parse_type("P16<i64>").unwrap();
        let Type::Ref(tr) = &t else { panic!("not a reference: {t}") };
        assert!(quantified_formal(&env, &t).is_none());
        assert!(tr.def_key().is_none());
    }

    // One that holds one walks each application of a typedef to the same
    // parameters once: `P16<fn<'b: Number>(x: 'b) -> 'b>`.
    #[test]
    fn rank2_parts_walks_an_application_once() {
        let env = typedef_chain();
        let t = parse_type("P16<fn<'b: Number>(x: 'b) -> 'b>").unwrap();
        RANK2_STEPS.with(|n| n.set(0));
        let (_, _, quantified) =
            quantified_formal(&env, &t).expect("a quantified formal");
        assert_eq!(quantified.len(), 1);
        assert!(RANK2_STEPS.with(|n| n.get()) <= 6 * 17);
    }

    // And to the same parameters by content where one has no node
    // identity: a primitive, `Any`, a map.
    #[test]
    fn rank2_parts_walks_an_application_over_a_leaf_once() {
        let env = chain_of("Q", "'a, 'c", "('a, 'c)");
        for leaf in ["i64", "Any", "Map<string, i64>"] {
            let t =
                parse_type(&format!("Q16<fn<'b: Number>(x: 'b) -> 'b, {leaf}>")).unwrap();
            RANK2_STEPS.with(|n| n.set(0));
            let (_, _, quantified) =
                quantified_formal(&env, &t).expect("a quantified formal");
            assert_eq!(quantified.len(), 1);
            let steps = RANK2_STEPS.with(|n| n.get());
            assert!(steps <= 8 * 17, "{leaf}: {steps} steps");
        }
    }

    // And to a parameter each level rebuilds, by its content: `P<i><'a> =
    // (P<i-1><('a, i64)>, P<i-1><('a, i64)>)` under a let's annotation.
    #[test]
    fn rank2_parts_walks_an_application_over_a_rebuilt_parameter_once() {
        let env = chain_passing("R", "'a", "('a, i64)", "('a, i64)");
        let t = parse_type("R16<fn<'b: Number>(x: 'b) -> 'b>").unwrap();
        RANK2_STEPS.with(|n| n.set(0));
        let (_, _, quantified) =
            quantified_formal(&env, &t).expect("a quantified formal");
        assert_eq!(quantified.len(), 1);
        let steps = RANK2_STEPS.with(|n| n.get());
        assert!(steps <= 8 * 17, "{steps} steps");
    }
}
