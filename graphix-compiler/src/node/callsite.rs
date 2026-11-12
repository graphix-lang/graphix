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
    LambdaId, LambdaInstanceId, Node, NodeView, Notify, Refs, ResolvingLambda, Rt, Scope,
    StaticHold, TagValue, Update, View, analysis, bailat, dbgenv, deref_typ,
    env::Env,
    expr::{ApplyExpr, At, Expr, ExprId, ExprKind},
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
    prune::BoundaryRef,
    stack::ensure_sufficient,
    typ::{
        AmbiguousMember, Brief, ContainsFlags, FnArgKind, FnArgType, FnType, Mutability,
        TVar, Type,
        settle::{DataPositions, Held},
        tvar::{AtLevel, BoundAt, Level, RigidGate},
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
use compact_str::{CompactString, format_compact};
use enumflags2::BitFlags;
use indexmap::{IndexMap, map::Entry as ArgEntry};
use log::warn;
use netidx_core::pack::{Pack, PackError, encode_varint};
use netidx_value::Value;
use nohash::{IntMap, IntSet};
use poolshark::local::LPooled;
use smallvec::SmallVec;
use std::{
    collections::hash_map::Entry,
    fmt::{self, Write as _},
    mem,
    sync::{
        OnceLock,
        atomic::{AtomicBool, Ordering::Relaxed},
    },
};
use triomphe::Arc as TArc;

/// Reject a direct call to a sync variadic builtin with no positional
/// arguments (`str::concat()`, `sum()`): the node has no data inputs and
/// can never fire. Only a `Ref` to a binding that names the builtin is
/// checkable.
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
    let Some(info) = ctx.builtin_bindings.get(&bind.id) else {
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
/// deciding it. At the bind, the environment is the instance's, and the
/// default takes its types from its definition's check
/// ([`InstanceTypes::of_default`]), as an instance's body does.
#[derive(Clone, Copy)]
enum Judge {
    Check,
    Bind,
}

/// How an omitted default meets its formal at a call: given the formal
/// and the default's type, it checks the default as the call's widening
/// checks a written argument; the note names the bindings to annotate
/// where the default is undecided ([`undecided_binds`]).
type Join<'a> = dyn FnMut(&Env, &ArcStr, &Type, &Type, &Expr, Option<CompactString>) -> Result<()>
    + 'a;

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
    join: Option<&mut Join<'_>>,
) -> Result<Node<R>> {
    let (node, res) = ctx.with_restored(f.env.clone(), |ctx| {
        let local_scope =
            Scope { dynamic: scope.dynamic.clone(), lexical: f.scope.lexical.clone() };
        let types = match judge {
            Judge::Bind => InstanceTypes::of_default(ctx, f, expr.id),
            Judge::Check => None,
        };
        // a check outside the definition reads its scheme, whose cells it
        // may not decide: it compiles among generic cells, and the copy
        // below is what it judges
        let _level = (matches!(judge, Judge::Check) && !ctx.rec_defs.contains(&f.id))
            .then(|| AtLevel::enter(Level::GENERIC));
        let mut node = f.in_signature(ctx, types.clone(), |ctx| {
            compile(ctx, flags, expr.clone(), &local_scope, top_id)
        })?;
        super::bind::name_value(&mut node, true);
        let res = match types {
            Some(mut types) => node.typecheck0_instance(ctx, &mut types),
            None => {
                let bind = matches!(judge, Judge::Bind);
                super::rechecking_instance(bind, || node.typecheck0(ctx))
            }
        };
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
    // the default is held as a written argument is, in the signature the
    // call sees it in
    let held = |sig: &FnType| match label(sig) {
        None => Ok(()),
        Some(t) => t.check_contains(&ctx.env, typ),
    };
    let res = res.and_then(|()| match site.and_then(|s| label(s)) {
        Some(t) => match join {
            Some(join) => {
                let undecided =
                    typ.has_free_in_data().then(|| undecided_binds(ctx, &node)).flatten();
                join(&ctx.env, name, &t, typ, expr, undecided)
            }
            None => t.check_contains(&ctx.env, typ),
        },
        None => {
            let mut gates: LPooled<Vec<RigidGate>> = LPooled::take();
            held(&callee_instance(ctx, f, name, site, &mut gates))?;
            value.map_or(Ok(()), held)
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
    let defs = written_defs(ctx, value);
    // CR claude for claude: [bug/low] The verdict depends on the order the defaults are
    // judged in: each narrows `formal`'s label, and a later one must fit what an earlier
    // one decided. With `let n: [i64, null] = null`, `[|#d = n, x: i64| x, |#d = 1, x:
    // i64| x]` passed to `array::len` checks, and the same two written the other way
    // round are refused (`i64 does not contain [i64, null]`); a call that omits the label
    // (`fs[0]$(7)`) accepts both, its omitted defaults joined by `Widening` whatever the
    // order. The order is now the written one (default-judge-order-01); it was the ids'
    // hash order, so the verdict could flip between runs. A fix: judge as the call does,
    // the label taking the widest default, or refuse any two defaults of different types
    // naming the annotation. probes: ~/ptmp/cr29/scratch/r20/20d/djo/ord1.gx .. ord6.gx
    // (met-default-order-verdict-01)
    for def in defs.iter() {
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
                None,
            )
            .map_err(|e| met_default_note::<R>(e, &defs, name))?;
            ctx.discard(node);
        }
    }
    Ok(())
}

/// The definitions a function of type `ft` may be, in the order the
/// program writes them ([`LambdaDef::written_order`]); an id no definition
/// is registered for comes last.
fn written_defs<R: Rt>(ctx: &CompileCtx<R>, ft: &FnType) -> SmallVec<[Option<Value>; 2]> {
    let mut defs: SmallVec<[Option<Value>; 2]> =
        ft.lambda_ids.ids().iter().map(|id| ctx.lambda_defs.get(id).cloned()).collect();
    fn def<R: Rt>(v: &Option<Value>) -> Option<&LambdaDef<R>> {
        v.as_ref().and_then(|v| v.downcast_ref::<LambdaDef<R>>())
    }
    defs.sort_by(|a, b| match (def::<R>(a), def::<R>(b)) {
        (Some(a), Some(b)) => a.written_order(b),
        (a, b) => b.is_some().cmp(&a.is_some()),
    });
    defs
}

/// A refused default of one of several functions met into one type,
/// two or more of `defs` with a default for `name`, names the annotation
/// that holds them all.
fn met_default_note<R: Rt>(
    e: anyhow::Error,
    defs: &[Option<Value>],
    name: &ArcStr,
) -> anyhow::Error {
    match met_defaults::<R>(defs, name) {
        Some(note) => e.context(note),
        None => e,
    }
}

/// The annotation [`met_default_note`] names, where two or more of `defs`
/// have a default for `name`.
fn met_defaults<R: Rt>(defs: &[Option<Value>], name: &ArcStr) -> Option<CompactString> {
    let defaulted =
        defs.iter()
            .filter_map(|d| d.as_ref().and_then(|v| v.downcast_ref::<LambdaDef<R>>()))
            .filter(|f| {
                f.typ.args.iter().zip(f.argspec.iter()).any(|(a, spec)| {
                    a.label() == Some(name) && spec.kind.default().is_some()
                })
            })
            .count();
    (defaulted >= 2).then(|| {
        format_compact!(
            "the functions met here share one #{name}: annotate the binding that \
             holds them with a function type whose #{name} holds every default"
        )
    })
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
                let mut holders = formals.iter().filter(|d| d.takes_constructor_of(v));
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
        (Type::Map(d), Type::Map(v)) => {
            at(ctx, &d.key, &v.key)?;
            at(ctx, &d.value, &v.value)
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

/// `t`, a typedef reference, expanded, with its definition's key; `None`
/// where it does not resolve (its check reports that) or `expanding`
/// holds its definition already (a recursive typedef met inside its own
/// expansion).
fn expansion(env: &Env, t: &Type, expanding: &[usize]) -> Option<(usize, Type)> {
    t.lookup_ref_probe(env).filter(|(key, _)| !expanding.contains(key))
}

#[cfg(test)]
thread_local! {
    static WALK_STEPS: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
}

/// A step of an escape walk, counted under test.
#[inline(always)]
fn walk_step() {
    #[cfg(test)]
    WALK_STEPS.with(|n| n.set(n.get() + 1));
}

/// Whether a value of `t` can carry a value of a cell its formal holds:
/// a function may be called with one, an unknown part may be one.
fn carries(env: &Env, t: &Type) -> bool {
    t.holds_fn(env) || t.holds_unknown(env)
}

/// A callee that is one of two or more function types: no one call takes
/// them all.
struct FnUnion<'a>(&'a [Type]);

impl FnUnion<'_> {
    /// The refusal of a call through `union`, where every member is a
    /// function type, written or named.
    fn refusal(env: &Env, union: &Type) -> Option<anyhow::Error> {
        let is_fn = |m: &Type| matches!(opened(env, m, &mut 0), Some(Type::Fn(_)));
        match union {
            Type::Set(ms) if ms.len() > 1 && ms.iter().all(is_fn) => {
                Some(anyhow!("{}", FnUnion(ms)))
            }
            _ => None,
        }
    }
}

impl fmt::Display for FnUnion<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "a call takes one function type, and this callee is one of ")?;
        for (i, m) in self.0.iter().enumerate() {
            let sep = match i {
                0 => "",
                i if i + 1 == self.0.len() => " and ",
                _ => ", ",
            };
            write!(f, "{sep}{}", Brief(m))?
        }
        let all = match self.0.len() {
            2 => "both",
            _ => "each",
        };
        write!(f, "; write one variable in {all}, or one written type in {all}")
    }
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

/// The cells of a formal's open copy ([`Widening::opening`]), each by the
/// address of the formal's cell it copies.
type Copies = LPooled<AHashMap<usize, TVar>>;

/// A formal's cells, each with its copy in the formal's open copy.
type Copied = LPooled<Vec<(TVar, TVar)>>;

/// How a call's argument stands once its own check is done.
enum ArgFit {
    /// Every cell it holds outside function types is decided: it joins
    /// the call's widening where it is written ([`Widening::join`]).
    Decided,
    /// It holds a cell nothing has decided outside function types, at a
    /// formal the call opened: checked now it would take what the formal
    /// holds so far. It pre-unified with the copy of these cells.
    Open(Copied),
}

/// How a decided argument, or an omitted default, met its formal.
enum Joined {
    Fits,
    /// It is neither wider nor narrower than a variable it holds.
    Incomparable,
    /// Two undecided members of a union could take it
    /// ([`AmbiguousMember`], the refusal): the call's other arguments may
    /// decide them.
    Choice(anyhow::Error),
}

/// An argument that could go to either of two undecided members of its
/// parameter's type: the parameter is what to annotate.
fn ambiguous_parameter(e: anyhow::Error) -> anyhow::Error {
    match e.downcast_ref::<AmbiguousMember>() {
        Some(a) if a.in_holder() => anyhow!(
            "{}: write the type of the parameter this argument is passed to, where \
             the function is defined",
            a.fact()
        ),
        _ => e,
    }
}

// XCR claude for claude: [bug/low] Under GRAPHIX_NO_SUBST=1 an instance's own check
// refuses a call its definition's check accepted where an argument was undecided at
// the definition and is decided at the instance: `let g = |y0, y1, y4, y5, y6| f(y0,
// y1, y4, y5, y6); g([[]], null, [[1]], [1], 1)` over `'a0: O<P<'a1, 'a2>>, 'a1:
// O<P<'a2, 'a3>>, .. |x0: 'a0, x1: 'a1, ..|` checks and runs 7 in every other mode
// (--no-fusion, GRAPHIX_PAR=force), and with NO_SUBST the instance of g is refused,
// "Array<Array<_>> could go to Array<'a1 ..> or Array<'a2 ..>". In the definition y0
// is an undecided cell, so it waits for the other arguments, which decide 'a1; in the
// instance it is Array<Array<_>>, checked where it is written, before y1. The same
// order decides a direct call's decided argument (f([[1]], null, ..) is refused, the
// skill's generics.md says so), so the question is whether a decided argument at a
// union two undecided members could take should wait for the join as an undecided
// one does. 2 of the 191 generated programs r20-x6 newly accepts; the shipped mode
// substitutes and agrees. probes:
// ~/ptmp/cr29/scratch/r20/20b/x6/residual/{nosubst_g3_p00561,nosubst_g1_p00424}.gx
// (decided-arg-waits-01)
// 2026-11-11 claude: Review of r20-x6: the divergence is not only the two generated
// programs, and not new with the round. The plain wrapper has it: `let f = |x: P<'a,
// 'b>, y: 'a, z: 'b| x; let g = |u, v, w| f(u, v, w); g([], 1, "s")` over `type P<'p,
// 'q> = [Array<'p>, Array<'q>]` checks, builds and prints [] in every shipped mode,
// and under GRAPHIX_NO_SUBST=1 the instance of g is refused, "Array<i64> could go to
// Array<'a> or Array<'b>": r20-staging 557beb5ed (before the round) the same,
// base-r20 runs it under NO_SUBST too. The round's first tip refused that program at
// the check in every mode (the wrapper's parameter was never opened, so [] did not
// wait); the review made it wait as the direct call does (Widening::standing, pin
// lang::types::wrapper_empty_array_into_a_typedef_union_is_decided_by_the_other_parameters),
// which puts it back where staging had it: right in the shipped modes, split under
// NO_SUBST. Also `g([], 2, never())` over `'a: P<'b, 'c> |x: 'a, y: 'b, z: 'c|`.
// Recommendation: let a decided argument that meets the choice at a formal the call
// opened wait for the join, as an undecided one does, and refuse it there if two
// members still could take it. That makes ruling (c)'s refusal independent of
// argument order (the skill's `f(1, 2)` over `|x: E<'a, 'b>, y: 'a|` would run, as
// `|y: 'a, x: E<'a, 'b>|` does), lets a wrapper of such a function be written with
// annotated parameters (`|u: P<'a, 'b>, v: 'a, w: 'b| f(u, v, w)` is refused today,
// at u), and closes this split. It changes a documented refusal, so it is Eric's to
// rule. probes: ~/ptmp/cr29/scratch/r20/20b/x6rev/p/{e3,e9,e13,e19}.gx, b/pb15.gx,
// q/t5.gx
// 2026-11-11 claude: Eric ruled (11-11, D32): (a) wait. A decided argument that meets a
// choice between undecided members waits for the call's other arguments, and is refused only
// if two members could still take it; the refusal must not depend on argument order.
// 2026-11-11 claude: Built as ruled (D32). A decided argument meets its formal through
// Widening::join, and one that two undecided members of the formal's union could take
// (AmbiguousMember from the containment or from the widening probe) is no longer refused
// where it is written: it waits (Widening::choosing) and joins again in
// CallSite::join_choosing, once the written arguments and the omitted defaults have, before
// the undecided arguments are checked, in rounds while one of them joins; one two members
// could still take is refused there with the same message. A decided argument one member
// takes is placed at once as before. The four programs of D32 run in every mode, the CR's
// probes run alike with and without GRAPHIX_NO_SUBST (q/t5.gx is refused by its own
// annotation in both), and `f([1], never(), never())`, `f([1])` over `|x: P<'a, 'b>|`
// stay refused. Pins that fail with the wait undone (Joined::Choice turned back into the
// refusal):
// lang::types::{decided_argument_into_a_typedef_union_is_decided_by_the_other_parameters,
// decided_argument_into_a_typedef_union_of_bare_variables,
// decided_argument_into_a_typedef_union_takes_the_member_left_open,
// decided_argument_into_a_typedef_union_widens_its_member_in_any_order,
// decided_argument_into_a_typedef_union_other_callers,
// annotated_wrapper_of_a_typedef_union_function,
// two_decided_arguments_into_typedef_unions_join_in_any_order,
// typedef_union_of_variables_decided_by_a_later_parameter,
// nested_union_typedef_decided_by_a_later_parameter,
// typedef_union_of_function_types_decided_by_the_other_parameters,
// typedef_union_of_function_types_no_decided_member_takes_refused,
// function_types_as_typedef_arguments_decided_by_the_other_parameters,
// named_function_types_as_typedef_arguments_decided_by_the_other_parameters},
// lang::interfaces::open_union_call_decided_by_the_other_argument, and the shell's
// no_subst::an_instance_checked_again_waits_for_the_arguments_that_decide_a_union; the
// refused twins (decided_argument_into_a_typedef_union_no_parameter_decides_refused,
// _no_member_holds_it_refused,
// two_decided_arguments_into_typedef_unions_neither_decided_refused, and the five
// *_refused_at_{its,their}_use pins and lang::interfaces::open_union_call_refused, rewritten
// to calls no other argument decides) pass either way.
// What is left of the order is three new CRs: choice-widen-order-01 (an argument wider than
// the member the others decided), choice-within-arg-order-01 (the parts of one argument),
// wrapper-widen-order-01 (no union; an inferred wrapper). Differential (gend.py,
// rund.sh): ~/ptmp/cr29/scratch/r21/21t/d32/{g1,g2,g3}.summary, {g1,g2,g3}/all.txt
/// Check a call's argument node, whose formal's type is `typ`,
/// answering how it stands. Where the call opens the formal
/// ([`Widening::opening`]) the argument pre-unifies with the open copy,
/// and one holding an undecided cell outside function types is
/// [`ArgFit::Open`]. An expression whose type its own check has yet to
/// make is not pre-unified with an open copy: its check would commit the
/// copy's bounds before the other arguments decide their variables. A
/// binding's type stands already, and takes the copy's shape for the
/// arguments after it to read.
fn typecheck_arg<R: Rt>(
    ctx: &mut CompileCtx<R>,
    typ: &Type,
    opening: Option<(Type, Copies)>,
    prebound: &AHashSet<usize>,
    n: &mut Node<R>,
) -> Result<ArgFit> {
    // A reference instantiates its signature in its own typecheck0,
    // which must precede the pre-unify.
    let named = matches!(n.view(), NodeView::Ref(_));
    if named {
        wrap!(n, n.typecheck0(ctx))?;
    }
    let (opening, copies) = opening.unzip();
    let note = match opening {
        None if n.typ().has_free_in_data() => {
            decided_elsewhere(&ctx.env, typ, prebound, n.typ())?
        }
        _ => None,
    };
    let refused = |e: anyhow::Error| match &note {
        Some(note) => e.context(note.clone()),
        None => e,
    };
    let unchecked = !named && n.typ().with_deref(|t| t.is_none());
    if opening.is_none() || !unchecked {
        Type::pre_unify_arg(&ctx.env, opening.as_ref().unwrap_or(typ), n.typ())
            .map_err(refused)?;
    }
    wrap!(n, n.typecheck0(ctx)).map_err(refused)?;
    Ok(match copies {
        Some(copies) if n.typ().has_free_in_data() => {
            let mut copied: Copied = LPooled::take();
            Reach::default().go(typ, &mut |_| false, &mut |tv| {
                if let Some(copy) = copies.get(&tv.cell_addr()) {
                    copied.push((tv.clone(), copy.clone()))
                }
                false
            });
            ArgFit::Open(copied)
        }
        _ => ArgFit::Decided,
    })
}

/// Make each cell of a formal's open copy that nothing decided the
/// formal's cell it copies ([`TVar::rejoin`]): what an undecided argument
/// took from the copy is then the formal's own, as the other arguments
/// settled it.
fn rejoin(copied: &Copied) {
    for (cell, copy) in copied.iter() {
        copy.rejoin(cell)
    }
}

/// `f`'s signature as a call through `site` instantiates it to pass the
/// parameter `name`: a fresh instance whose other parameters take the
/// arguments of a copy of the site's, and whose result and throws fit
/// the copy's, so a variable the site fixes through another parameter,
/// its result or its throws fixes `name`'s.
fn callee_instance<R: Rt>(
    ctx: &CompileCtx<R>,
    f: &LambdaDef<R>,
    name: &ArcStr,
    site: Option<&FnType>,
    gates: &mut LPooled<Vec<RigidGate>>,
) -> FnType {
    let inst = f.typ.instantiate(&ctx.rec_defs);
    if let Some(site) = site {
        // each argument the site passes fits the callee's parameter for it;
        // a site's own quantifiers stand for any type, so they are held
        // rigid while `gates` lives
        let site = site.replace_tvars(&Default::default());
        site.each_quantifier(|_, tv| gates.push(tv.open_rigid()));
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
    inst
}

/// A walk of the cells under a type, following bindings and an open
/// cell's lower bounds and constraints (what it will bind to holds the
/// one and is held by the other), each cell and shared node once.
#[derive(Default)]
struct Reach {
    seen: LPooled<AHashSet<usize>>,
}

impl Reach {
    /// Walk `t`, handing `f` each cell until it answers stop, entering no
    /// function type `cut` refuses; whether `f` stopped it.
    fn go(
        &mut self,
        t: &Type,
        cut: &mut impl FnMut(&Type) -> bool,
        f: &mut impl FnMut(&TVar) -> bool,
    ) -> bool {
        walk_step();
        ensure_sufficient(|| match t {
            Type::TVar(tv) => {
                if !self.seen.insert(tv.cell_addr()) {
                    return false;
                }
                if f(tv) {
                    return true;
                }
                match tv.binding() {
                    Some(b) => self.go(&b, cut, f),
                    None => {
                        let (lower, cons) = (tv.cell_lower(), tv.cell_constraints());
                        lower.iter().chain(cons.iter()).any(|t| self.go(t, cut, f))
                    }
                }
            }
            t if t.shared_identity().is_some_and(|n| !self.seen.insert(n)) => false,
            Type::Fn(_) if cut(t) => false,
            t => {
                let mut stop = false;
                t.for_each_child(&mut |c| stop = stop || self.go(c, cut, f));
                stop
            }
        })
    }
}

/// A node [`Free`] met: on the walk's path at a depth, or answered.
enum Met {
    OnPath(usize),
    Answered(Option<Ans>),
}

/// The asked cells free under a node: one, by its number, or two or more,
/// a set of [`Free::sets`] by its id.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum Ans {
    One(u32),
    Many(u32),
}

/// The cells [`Free`] is asked of, by address, each with its number.
type Asked = LPooled<AHashMap<usize, u32>>;

/// Runs interned by content: equal runs share one id.
struct Interned<T> {
    data: LPooled<Vec<T>>,
    spans: LPooled<Vec<(u32, u32)>>,
    by_hash: LPooled<IntMap<u64, SmallVec<[u32; 1]>>>,
}

const RUN_HASH: ahash::RandomState = ahash::RandomState::with_seeds(1, 2, 3, 4);

impl<T: Copy + Eq + std::hash::Hash> Interned<T> {
    fn new() -> Self {
        Self { data: LPooled::take(), spans: LPooled::take(), by_hash: LPooled::take() }
    }

    fn get(&self, id: u32) -> &[T] {
        let (s, e) = self.spans[id as usize];
        &self.data[s as usize..e as usize]
    }

    /// The id of the run `data` holds from `at`, and whether it is new; a
    /// run equal to an earlier one is taken off `data`.
    fn seal(&mut self, at: usize) -> (u32, bool) {
        let Self { data, spans, by_hash } = self;
        for _ in at..data.len() {
            walk_step();
        }
        let ids = by_hash.entry(RUN_HASH.hash_one(&data[at..])).or_default();
        for id in ids.iter() {
            let (s, e) = spans[*id as usize];
            for _ in s..e {
                walk_step();
            }
            if data[s as usize..e as usize] == data[at..] {
                data.truncate(at);
                return (*id, false);
            }
        }
        let id = spans.len() as u32;
        spans.push((at as u32, data.len() as u32));
        ids.push(id);
        (id, true)
    }
}

/// The asked cells, each by its number, free under a type walked as
/// [`Reach`] walks it: under a cell, itself if asked and those under what
/// it reaches; under a function type, its children's less the cells it
/// quantifies. A node is answered once, except one on a cycle back to a
/// node still on the walk's path: its answer misses what the cycle reaches
/// there, so it is answered again where it is met again. Answers are
/// interned, so a node whose children answer alike shares their answer,
/// and a join of the same answers is made once.
struct Free<'a> {
    asked: &'a AHashMap<usize, u32>,
    nodes: LPooled<AHashMap<usize, Met>>,
    /// The answers of two or more cells: their numbers, sorted.
    sets: Interned<u32>,
    /// Every join of answers made by a node that binds no asked cell: the
    /// answers, sorted; `joined` holds what each one answered.
    joins: Interned<Ans>,
    joined: LPooled<Vec<Option<Ans>>>,
    /// The answers of the children of the nodes in progress, each node's
    /// the tail it pushed.
    stack: LPooled<Vec<Ans>>,
    depth: usize,
}

impl<'a> Free<'a> {
    fn new(asked: &'a AHashMap<usize, u32>) -> Self {
        Self {
            asked,
            nodes: LPooled::take(),
            sets: Interned::new(),
            joins: Interned::new(),
            joined: LPooled::take(),
            stack: LPooled::take(),
            depth: 0,
        }
    }

    /// The asked cells free under `t`, in order; `met` is handed each asked
    /// cell, with its number, as the walk meets it. A walk of another type
    /// takes what this one answered.
    fn free(&mut self, t: &Type, met: &mut impl FnMut(u32, &TVar)) -> LPooled<Vec<u32>> {
        let mut free: LPooled<Vec<u32>> = LPooled::take();
        match self.go(t, met).1 {
            None => (),
            Some(Ans::One(i)) => free.push(i),
            Some(Ans::Many(k)) => free.extend_from_slice(self.sets.get(k)),
        }
        free
    }

    /// What is free under `t`, none for nothing; the shallowest depth on the
    /// path a cycle from `t` returned to, `usize::MAX` for none.
    fn go(&mut self, t: &Type, met: &mut impl FnMut(u32, &TVar)) -> (usize, Option<Ans>) {
        walk_step();
        ensure_sufficient(|| {
            let key = t.shared_identity();
            if let Some(k) = key {
                match self.nodes.get(&k) {
                    Some(Met::Answered(a)) => return (usize::MAX, *a),
                    Some(Met::OnPath(d)) => return (*d, None),
                    None => {
                        self.nodes.insert(k, Met::OnPath(self.depth));
                    }
                }
            }
            let (depth, start) = (self.depth, self.stack.len());
            self.depth += 1;
            let mut back = usize::MAX;
            let mut own: SmallVec<[u32; 2]> = SmallVec::new();
            let mut child = |walk: &mut Self, (b, ans): (usize, Option<Ans>)| {
                back = back.min(b);
                if let Some(a) = ans
                    && walk.stack[start..].last() != Some(&a)
                {
                    walk_step();
                    walk.stack.push(a)
                }
            };
            match t {
                Type::TVar(tv) => {
                    if let Some(i) = self.number(tv) {
                        met(i, tv);
                        child(self, (usize::MAX, Some(Ans::One(i))))
                    }
                    match tv.binding() {
                        Some(b) => {
                            let r = self.go(&b, met);
                            child(self, r)
                        }
                        None => {
                            let (lower, cons) = (tv.cell_lower(), tv.cell_constraints());
                            for t in lower.iter().chain(cons.iter()) {
                                let r = self.go(t, met);
                                child(self, r)
                            }
                        }
                    }
                }
                t => {
                    t.for_each_child(&mut |c| {
                        let r = self.go(c, met);
                        child(self, r)
                    });
                    if let Type::Fn(ft) = t {
                        ft.each_quantifier(|_, q| own.extend(self.number(q)));
                        own.sort_unstable()
                    }
                }
            }
            self.depth -= 1;
            let held = match self.stack.len() - start {
                0 => None,
                1 if own.is_empty() => self.stack.pop(),
                _ => self.join(start, &own),
            };
            if let Some(k) = key {
                match back >= depth {
                    true => {
                        self.nodes.insert(k, Met::Answered(held));
                        back = usize::MAX;
                    }
                    false => {
                        self.nodes.remove(&k);
                    }
                }
            }
            (back, held)
        })
    }

    /// `tv`'s number, if it is asked.
    fn number(&self, tv: &TVar) -> Option<u32> {
        walk_step();
        self.asked.get(&tv.cell_addr()).copied()
    }

    /// The answer of a node whose children answered `stack[start..]`,
    /// taken off the stack, less the cells it binds, `bound`, sorted.
    fn join(&mut self, start: usize, bound: &[u32]) -> Option<Ans> {
        let parts = &mut self.stack[start..];
        let mut n = parts.len();
        if n > 1 {
            for _ in 0..n {
                walk_step();
            }
            parts.sort_unstable();
            n = 1;
            for at in 1..parts.len() {
                if parts[n - 1] != parts[at] {
                    parts[n] = parts[at];
                    n += 1
                }
            }
        }
        let parts = &parts[..n];
        let r = match (parts, bound) {
            ([], _) => None,
            ([a], []) => Some(*a),
            (parts, []) => {
                let at = self.joins.data.len();
                self.joins.data.extend_from_slice(parts);
                match self.joins.seal(at) {
                    (k, false) => self.joined[k as usize],
                    (_, true) => {
                        let r = union(&mut self.sets, parts, &[]);
                        self.joined.push(r);
                        r
                    }
                }
            }
            (parts, bound) => union(&mut self.sets, parts, bound),
        };
        self.stack.truncate(start);
        r
    }
}

/// The union of the answers `parts` less the cells in `bound`, both
/// sorted; none for nothing.
fn union(sets: &mut Interned<u32>, parts: &[Ans], bound: &[u32]) -> Option<Ans> {
    let at = sets.data.len();
    for p in parts {
        match *p {
            Ans::One(i) => {
                walk_step();
                sets.data.push(i)
            }
            Ans::Many(k) => {
                let (s, e) = sets.spans[k as usize];
                for _ in s..e {
                    walk_step();
                }
                sets.data.extend_from_within(s as usize..e as usize)
            }
        }
    }
    if parts.len() > 1 {
        sets.data[at..].sort_unstable()
    }
    let (mut kept, mut b) = (at, 0);
    for i in at..sets.data.len() {
        let c = sets.data[i];
        while bound.get(b).is_some_and(|q| *q < c) {
            walk_step();
            b += 1
        }
        walk_step();
        if (kept == at || sets.data[kept - 1] != c) && bound.get(b) != Some(&c) {
            sets.data[kept] = c;
            kept += 1
        }
    }
    sets.data.truncate(kept);
    match kept - at {
        0 => None,
        1 => sets.data.pop().map(Ans::One),
        _ => Some(Ans::Many(sets.seal(at).0)),
    }
}

/// The first of the `quantified` cells (name, cell address) that a
/// variable of `value`'s environment reaches, with that variable: a
/// cell at the top level or another definition's, never one the
/// lambdas `value` holds own nor a quantifier under a function type in
/// `value` that binds it. Such a value is not general in that cell.
pub(crate) fn carried_out(
    quantified: &[(ArcStr, usize)],
    value: &Type,
) -> Option<(ArcStr, TVar)> {
    let own: SmallVec<[LambdaId; 2]> = match value.deref_cloned() {
        Some(Type::Fn(ref aft)) => aft.lambda_ids.ids().iter().copied().collect(),
        _ => SmallVec::new(),
    };
    let is_foreign = |tv: &TVar| match tv.level() {
        Level::Top => true,
        Level::Def { owner, .. } => !own.contains(&owner),
        Level::Generic | Level::Written => false,
    };
    // a foreign cell some function type in `value` quantifies is asked of
    // [`Free`]; any other is free
    let mut foreign: SmallVec<[TVar; 4]> = SmallVec::new();
    let mut index: Asked = LPooled::take();
    let mut note = |fnt: &Type| {
        if let Type::Fn(ft) = fnt {
            ft.each_quantifier(|_, tv| {
                if is_foreign(tv) {
                    let next = index.len() as u32;
                    index.entry(tv.cell_addr()).or_insert(next);
                }
            })
        }
        false
    };
    Reach::default().go(value, &mut note, &mut |tv| {
        if is_foreign(tv) {
            foreign.push(tv.clone())
        }
        false
    });
    if !index.is_empty() {
        let free = Free::new(&index).free(value, &mut |_, _| ());
        foreign.retain(|tv| {
            index.get(&tv.cell_addr()).is_none_or(|i| free.binary_search(i).is_ok())
        });
    }
    // what an earlier variable's walk met reaches none of the cells
    let mut reach = Reach::default();
    foreign.into_iter().find_map(|tv| {
        let mut reached = None;
        reach.go(&Type::TVar(tv.clone()), &mut |_| false, &mut |c| {
            reached = quantified.iter().find(|(_, a)| *a == c.cell_addr());
            reached.is_some()
        });
        reached.map(|(q, _)| (q.clone(), tv))
    })
}

// CR claude for claude: [perf/medium] A call whose signature holds a DAG of
// parametrized named unions checks in time exponential in the DAG's depth: `type
// L0<'t> = ['t, null, `A]; type M0<'t> = [Array<'t>, `B]; type L<i><'t> =
// [L<i-1><'t>, M<i-1><'t>]`, `M<i>` the same, then `let f = |x0: L<n><string>| 1;
// f(null)`. --no-cache --check, debug, instructions: n = 10 / 12 / 14 / 16 / 18 0.82 /
// 1.03 / 1.89 / 5.29 / 18.9 G (an empty program 0.75 G); as the return type (`|x0: i64|
// -> L<n><string> null`, `f(1)`) 0.79 / 0.89 / 1.31 / 2.98 / 9.68 G. The uncalled
// definition, `let x: L<n><string> = null` and the parameterless DAG are flat. perf at
// 16: Widening::cells -> widenable -> data_positions (35%) and holds_invariantly (30%),
// each through expansion -> Type::lookup_ref_probe. `clean` remembers only parameterless
// typedefs and DataSeen keys a node by its address, where an application's expansion is
// built anew at every visit, so each path is walked. Key an application by its
// definition and its parameters' ids, as RefHist::ref_id and ContainsHist::pair_key do,
// and pin the shape in deep_nesting. The contains side of the same DAG, a declared var
// refused against `L<n><'b>` (`|x0: 'a, y0: 'b| { let x: L<n><'b> = x0; x }`, 12.0 G at
// 18, and 1.7x main's peak memory since the probe memo keys a var), is
// typedef-open-expansion-01's walk. Main 82569fed3 retires the same instructions on
// each. Found by the review of union-dag-rigid-cell-01. probes:
// ~/ptmp/cr29/scratch/r20/20g/review/{pc,pr,pv}{10,12,14,16,18}.gx
// (call-param-dag-exp-01)
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

// CR claude for claude: [bug/low] A variable a declared bound holds is decided
// by the first argument that meets it, so the order of the parameters decides a
// call. Over `type T<'x> = [`Leaf('x), `Node(Array<T<'x>>)]; let v: (T<[i64,
// string]>, T<[i64, string]>) = (`Node([]), `Leaf(1))`, `let f = 'a0: (T<'a1>,
// T<'a1>), 'a1: [i64, string] |x0: 'a0, x1: 'a1| x1; f(v, 1)` runs (the bound
// leaves 'a1 above [i64, string], which holds `1`) and `|x1: 'a1, x0: 'a0| x1`
// called `f(1, v)` is refused: "'a0: unbound within (T<'a1: i64>, T<'a1: i64>)
// does not contain (T<[i64, string]>, T<[i64, string]>)", `1` having bound 'a1
// before the bound met `v`. Widening opens the variables the formal itself holds
// (Widening::open over the formal's own cells), not one a bound of its variable
// holds. base-r20 alike; `let one: [i64, string] = 1` runs. probes:
// ~/ptmp/cr29/scratch/r20/coord/conflicts-verify/{d1swap,c3swap}.gx
// (bound-variable-arg-order-01)
// 2026-11-11 claude: The same refusal with the variable one bound further in:
// `type O<'t> = ['t, null]; let f = 'a0: (O<'a1>, O<'a2>), 'a1: O<'a2>, 'a2:
// [i64, string] |x0: 'a0, x1: 'a1| x1; let v: (O<O<i64>>, O<[i64, string]>) =
// (null, null); f(v, null)` is refused "'a1: unbound within O<'a2: unbound
// within [i64, string] above [i64, string]> above [i64, null] does not contain
// null" (base-r20 alike): Widening::settle_bounded settles 'a1 at the union of
// its lower bounds only where its bound holds that union, and the probe of
// O<'a2> against it asks 'a2, itself above a lower bound, to bind below it.
// Settling the variables a bound holds first was tried: the check then passes
// and the build refuses the instance (q1.gx; base-r20 and rev20 refuse the
// instance of the annotated twin q2.gx the same way), so it was not landed. 4 of
// 1200 generated programs (coord/rebase/mix.py, seeds 31 and 32) are refused
// this way or by the order above and run on base-r20. Probes:
// ~/ptmp/cr29/scratch/r20/coord/x1/{q1,q2,p00415,p00430}.gx.
/// A fresh instance's type variables that settle to the widest argument
/// rather than the first: open in the definition `def`, and met only as
/// data in `inst`, so a value of a wider type is well typed wherever the
/// signature holds one.
fn widenable(env: &Env, def: &FnType, inst: &FnType) -> LPooled<AHashMap<ArcStr, TVar>> {
    let mut at_def: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
    def.collect_tvars(&mut at_def);
    let mut found: LPooled<AHashMap<ArcStr, (TVar, bool)>> = LPooled::take();
    let mut positions = DataPositions::new(env);
    inst.for_each_type(&mut |t| {
        positions.walk(t, Held::default(), &mut |tv, held| {
            found.entry(tv.name.clone()).or_insert_with(|| (tv.clone(), true)).1 &=
                held.data()
        })
    });
    found
        .drain()
        .filter(|(name, (tv, data))| {
            *data && !tv.is_rigid() && at_def.get(name).is_some_and(|d| !d.is_bound())
        })
        .map(|(name, (tv, _))| (name, tv))
        .collect()
}

/// How a fresh instance's arguments settle its [`widenable`] variables:
/// to the widest argument holding no undecided cell, the omitted defaults
/// included, whatever the order. An argument that does not fit is probed
/// once against its formal with those variables open; it widens the
/// variables it holds wider, and waits for the end of the argument loop
/// when it is neither wider nor narrower. One that two undecided members
/// of its formal could take waits for the others to decide them, and
/// joins after them, so the order a call writes its arguments in decides
/// no such choice. An argument holding an undecided cell decides none of
/// the variables: it waits, and takes what the others settle.
struct Widening {
    fresh: bool,
    cells: Option<LPooled<AHashMap<ArcStr, TVar>>>,
    /// What is checked at the end of the argument loop, at its formal.
    deferred: LPooled<Vec<(Waiter, Type)>>,
    /// The decided arguments that met a choice ([`Joined::Choice`]), each
    /// with its formal: they join once the others have.
    choosing: LPooled<Vec<(ArgKey, Type)>>,
}

/// What waits for the end of a call's argument loop, and why.
enum Waiter {
    /// An argument holding a cell nothing has decided ([`ArgFit::Open`]).
    Undecided(ArgKey, Copied),
    /// An argument neither wider nor narrower than a variable it holds.
    Incomparable(ArgKey),
    /// An omitted default of type `typ`, written at `at`, neither wider nor
    /// narrower than a variable it holds, met a choice or holding an
    /// undecided cell, with the note a refusal of it takes
    /// ([`met_defaults`]) and, where it is undecided, the bindings to
    /// annotate ([`undecided_binds`]).
    Default {
        typ: Type,
        at: Expr,
        met: Option<CompactString>,
        undecided: Option<CompactString>,
    },
}

impl Widening {
    fn new(fresh: bool) -> Self {
        Self { fresh, cells: None, deferred: LPooled::take(), choosing: LPooled::take() }
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

    /// A copy of `formal` with the variables `names` unbound, and the
    /// copy's variables by name.
    fn open<'a>(
        formal: &Type,
        names: impl Iterator<Item = &'a ArcStr>,
    ) -> (Type, LPooled<AHashMap<ArcStr, TVar>>) {
        let open = formal.reset_tvars();
        let mut opened: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
        open.collect_tvars(&mut opened);
        for name in names {
            if let Some(tv) = opened.get(name) {
                tv.unbind()
            }
        }
        (open, opened)
    }

    // CR claude for claude: [bug/low] Through an inferred wrapper a variable settles
    // to the first argument, not the widest: over `let f = |x: Array<'b>, y: ('b,
    // i64)| (x, y); let g = |u, v| f(u, v)` and `n: [i64, null]`, `g([1], (n, 2))` is
    // refused, "type mismatch ('b: i64, i64) does not contain ([i64, null], i64)",
    // where `f([1], (n, 2))` runs, and so does the wrapper with the wider argument
    // first (`let f = |y: ('b, i64), x: Array<'b>| ..; g((n, 2), [1])`). The wrapper's
    // parameters are cells bound to the callee's formals; Widening::standing opens
    // them for an undecided argument, and the decided argument that does not fit as
    // the first left the variable is not widened. No union is needed; a typedef
    // union in the callee's formal shows the same (`x: P<'b, string>`). base-r22
    // alike. Not looked into further. probes:
    // ~/ptmp/cr29/scratch/r21/21t/d32/wr/{a,b,c,d,e,f}.gx (wrapper-widen-order-01)
    /// What `formal` stands for: a cell that is not widenable is what it
    /// is bound to, so an inferred parameter that took a type holding
    /// widenable variables opens as a written one does.
    fn standing(cells: &AHashMap<ArcStr, TVar>, formal: &Type) -> Type {
        let mut t = formal.clone();
        while let Type::TVar(tv) = &t
            && !cells.contains_key(&tv.name)
            && let Some(b) = tv.binding()
        {
            t = b
        }
        t
    }

    /// The widenable variables `formal` holds.
    fn held(
        &mut self,
        env: &Env,
        def: &Type,
        inst: &FnType,
        formal: &Type,
    ) -> LPooled<AHashMap<ArcStr, TVar>> {
        let mut held: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
        if !self.fresh {
            return held;
        }
        let cells = Self::cells(&mut self.cells, env, def, inst);
        Self::standing(cells, formal).collect_tvars(&mut held);
        held.retain(|name, _| cells.contains_key(name));
        held
    }

    /// The formal an argument is pre-unified with where `formal` holds a
    /// widenable variable, and its cells: a copy with those variables open
    /// and every other cell the formal's own, so no argument decides a
    /// widenable one before the closed arguments join, and each decides
    /// the others as it meets them. An argument holding a cell nothing has
    /// decided then waits for the end of the argument loop, to take the
    /// widenable variables as the closed arguments settle them.
    fn opening(
        &mut self,
        env: &Env,
        def: &Type,
        inst: &FnType,
        formal: &Type,
    ) -> Option<(Type, Copies)> {
        if !self.fresh {
            return None;
        }
        let cells = Self::cells(&mut self.cells, env, def, inst);
        let formal = &Self::standing(cells, formal);
        let mut all: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
        formal.collect_tvars(&mut all);
        if !all.keys().any(|name| cells.contains_key(name)) {
            return None;
        }
        let kept: LPooled<AHashMap<ArcStr, Type>> = all
            .iter()
            .filter(|(name, _)| !cells.contains_key(*name))
            .map(|(name, tv)| (name.clone(), Type::TVar(tv.clone())))
            .collect();
        let mut copies: Copies = LPooled::take();
        let open = formal.replace_tvars_copied(&kept, &mut copies);
        let widened = |name: &ArcStr| cells.contains_key(name) && all.contains_key(name);
        for tv in copies.values().filter(|tv| widened(&tv.name)) {
            tv.unbind()
        }
        Some((open, copies))
    }

    // CR claude for claude: [bug/low] A decided argument wider than the member the
    // other arguments decide still meets its formal's union by the order of the
    // parameters. Over `type P<'p, 'q> = [Array<'p>, Array<'q>]` and `n: [i64, null]`,
    // `f(1.5, [n], 1)` over `|z: 'b, x: P<'a, 'b>, y: 'a|` runs (`[n]` met 'a undecided
    // beside 'b: f64, which does not hold it, so the one member left open took it at
    // once, and `1` then fits), and the same call with the parameters in the other
    // three orders is refused: `|x, y, z|` and `|y, z, x|` "Array<[i64, null]> could
    // go to Array<'a: unbound> or Array<'b: unbound>" (both members are decided,
    // neither holds it, and Widening::widen's probe opens both), `|y, x, z|` "'b:
    // [i64, null] does not contain f64" ('b was the member left open). With `null`
    // for `1.5` two orders run at two types: `f(null, [n], 1)` over `|z, x, y|` has
    // 'a: [i64, null], 'b: null and `f(1, [n], null)` over `|y, x, z|` has 'a: i64,
    // 'b: [i64, null]; the other two orders are refused. decided-arg-waits-01 made
    // the choice between two undecided members wait; here one member is decided and
    // could take the argument by widening. A rule that would close it: a decided
    // member whose variable is widenable counts as able to take the argument, so the
    // argument waits as a choice does and, once the others have joined, widens the
    // one variable that opened alone holds it wider (Widening::open over one name),
    // refused where two could. That refuses the two orders that run at two types, so
    // it needs a census. An instance checked again (GRAPHIX_NO_SUBST) meets the same
    // probe where its definition met declared variables: `let h = |y0: O<'a>, x0:
    // P<'b, string>, y1: Array<'b>, x1: E<'a, 'b>| f(y0, x0, y1, x1); h(null, ["s"],
    // ["s"], n)` over an `f` of the same parameters (`type O<'t> = ['t, null]; type
    // E<'p, 'q> = ['p, 'q]`) runs, and under NO_SUBST its instance is refused, "[i64,
    // null, string] could go to 'a: unbound or 'b: unbound". base-r22 the same on
    // every program above. Two arguments that both waited meet it among themselves
    // (CallSite::join_choosing joins them in the order they stand): `f(#x0: [n],
    // #x1: [n], 1)` over `|#x0: O<P<'a, 'b>>, #x1: Array<E<'a, 'b>>, y0: O<'b>|` runs
    // ('b: i64, then `x0` gives 'a [i64, null], which holds `x1`), and with `#x1`
    // first it is refused: `x1` joins by giving 'a the `null` that 'b: i64 leaves
    // of [i64, null], and `x0` then fits neither member (base-r22 refuses both). Of
    // 1360 generated families (gend.py, seeds 101, 202 and 303) 5 are decided by
    // order this way: g1/f0306_direct, g2/f0353_bound, g2/f0324_lab, g3/f0246_sel,
    // g3/f0348_wrap. probes:
    // ~/ptmp/cr29/scratch/r21/21t/d32/res/{r1,r2,r3,r4,s1,s2,s3,s4}.gx,
    // g1/f0075_ann_p0.gx, g1/f0306_direct_p{0,1,2}.gx, g2/f0324_lab_p{0,1}.gx
    // (choice-widen-order-01)
    /// A decided argument or omitted default of type `typ` meets `formal`:
    /// it fits as the formal stands or widens the variables it holds
    /// wider, waits to be either, or meets a choice the other arguments
    /// may make; else the mismatch error.
    fn join(
        &mut self,
        env: &Env,
        def: &Type,
        inst: &FnType,
        formal: &Type,
        typ: &Type,
    ) -> Result<Joined> {
        let joined = match formal.contains(env, typ) {
            Ok(false) => self.widen(env, def, inst, formal, typ),
            fits => fits,
        };
        match joined {
            Ok(true) => Ok(Joined::Fits),
            Ok(false) => Ok(Joined::Incomparable),
            Err(e) if e.is::<AmbiguousMember>() => Ok(Joined::Choice(e)),
            Err(e) => Err(e),
        }
    }

    /// The argument for `key` met `formal`: join, wait, or the mismatch
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
            ArgFit::Open(copied) => self
                .deferred
                .push((Waiter::Undecided(key.clone(), copied), formal.clone())),
            ArgFit::Decided => {
                match wrap!(n, self.join(env, def, inst, formal, n.typ()))? {
                    Joined::Fits => (),
                    Joined::Incomparable => self
                        .deferred
                        .push((Waiter::Incomparable(key.clone()), formal.clone())),
                    Joined::Choice(_) => {
                        self.choosing.push((key.clone(), formal.clone()))
                    }
                }
            }
        }
        Ok(())
    }

    /// An omitted default of type `typ`, written at `at`, meets `formal` as
    /// a written argument does: it decides or widens the variables it
    /// holds, or waits, as one holding an undecided cell does; `met` and
    /// `undecided` name the annotations a refusal asks for.
    #[allow(clippy::too_many_arguments)]
    fn join_default(
        &mut self,
        env: &Env,
        def: &Type,
        inst: &FnType,
        formal: &Type,
        typ: &Type,
        at: &Expr,
        met: Option<CompactString>,
        undecided: Option<CompactString>,
    ) -> Result<()> {
        let waits =
            typ.has_free_in_data() && !self.held(env, def, inst, formal).is_empty();
        if waits || !matches!(self.join(env, def, inst, formal, typ)?, Joined::Fits) {
            let waiter =
                Waiter::Default { typ: typ.clone(), at: at.clone(), met, undecided };
            self.deferred.push((waiter, formal.clone()))
        }
        Ok(())
    }

    /// Settle each of `cells` that `formal` holds undecided above a lower
    /// bound, at the union of its lower bounds: it binds to no argument
    /// that does not hold them, and whatever it becomes holds them, so an
    /// argument inside them fits and a wider one widens it.
    fn settle_bounded(env: &Env, cells: &AHashMap<ArcStr, TVar>, formal: &Type) {
        let mut held: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
        formal.collect_tvars(&mut held);
        for (name, tv) in held.iter() {
            if cells.contains_key(name) && !tv.is_bound() && !tv.cell_lower().is_empty() {
                let _ = tv.settle_witness(env);
            }
        }
    }

    // CR claude for claude: [bug/low] Two parts of one argument that hold one
    // widenable variable still decide it by their order: over `let f = |k: ('y,
    // 'y)| 0` and `n: [i64, null]`, `f((1, n))` is refused ("('y: i64, 'y: i64)
    // does not contain (i64, [i64, null])") and `f((n, 1))` runs, where two
    // arguments widen in either order and so do two parts of one position of a
    // written function type (FnType::order_free_cells). Base-r20 the same.
    // Widening::widen probes the argument whole against the open formal, and
    // that probe binds the variable at the first part. probes:
    // ~/ptmp/cr29/scratch/r20/20b/fao/{c3,c5}.gx (widen-within-arg-order-01)
    /// An argument of type `typ` did not fit `formal`: widen the variables
    /// it holds wider and check it, or the mismatch error. False when it
    /// is neither wider nor narrower than a variable, to be checked once
    /// the other arguments have widened it.
    fn widen(
        &mut self,
        env: &Env,
        def: &Type,
        inst: &FnType,
        formal: &Type,
        typ: &Type,
    ) -> Result<bool> {
        let check = || formal.check_contains(env, typ).map(|()| true);
        if !self.fresh || typ.has_unbound() {
            return check();
        }
        let cells = Self::cells(&mut self.cells, env, def, inst);
        if cells.is_empty() {
            return check();
        }
        Self::settle_bounded(env, cells, formal);
        let (open, opened) = Self::open(formal, cells.keys());
        if !open.contains(env, typ)? {
            return match open.discernible_refusal(env, typ) {
                Some(e) => Err(e),
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
            return Ok(false);
        }
        check()
    }
}

/// Whether some decision of the undecided cells the type `arg` holds outside
/// function types could fit it to `formal`, whose widenable variables are
/// `held`: on copies, it fits the formal with them fresh, and what it gives
/// each holds or is held by what the call gave it, which an argument
/// decided so would widen.
fn annotation_may_fit(
    env: &Env,
    formal: &Type,
    held: &AHashMap<ArcStr, TVar>,
    arg: &Type,
) -> Result<bool> {
    if !arg.has_free_in_data() {
        return Ok(false);
    }
    let fresh: AHashMap<ArcStr, TVar> =
        held.keys().map(|name| (name.clone(), TVar::empty_named(name.clone()))).collect();
    let open: AHashMap<ArcStr, Type> =
        fresh.iter().map(|(name, tv)| (name.clone(), Type::TVar(tv.clone()))).collect();
    let open = formal.replace_tvars(&open);
    let arg = arg.reset_tvars();
    if !open.contains_ordered(env, &arg)? {
        return Ok(false);
    }
    for (name, tv) in held.iter() {
        let (Some(given), Some(new)) =
            (tv.binding(), fresh.get(name).and_then(|c| c.binding()))
        else {
            continue;
        };
        let given = given.reset_tvars();
        if !new.contains_ordered(env, &given)? && !given.contains_ordered(env, &new)? {
            return Ok(false);
        }
    }
    Ok(true)
}

/// What a refusal of an argument of type `arg`, holding an undecided cell,
/// at a formal the call did not open asks to annotate: where an earlier
/// argument of this call decided a written variable `formal` holds outside
/// function types to a type a wider decision could make hold `arg`
/// ([`annotation_may_fit`]), the argument that decided it, since annotating
/// `arg` widens nothing. `prebound` holds the cells bound before the call's
/// arguments were checked.
fn decided_elsewhere(
    env: &Env,
    formal: &Type,
    prebound: &AHashSet<usize>,
    arg: &Type,
) -> Result<Option<CompactString>> {
    let mut decided: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
    formal.collect_tvars(&mut decided);
    decided.retain(|_, tv| {
        tv.is_bound() && !tv.generated() && !prebound.contains(&tv.cell_addr())
    });
    if decided.is_empty() {
        return Ok(None);
    }
    let mut outside: LPooled<AHashMap<usize, TVar>> = LPooled::take();
    formal.variables_outside_functions(env, &mut outside);
    decided.retain(|_, tv| outside.contains_key(&tv.cell_addr()));
    if decided.is_empty() || !annotation_may_fit(env, formal, &decided, arg)? {
        return Ok(None);
    }
    let mut names: SmallVec<[&ArcStr; 2]> = decided.keys().collect();
    names.sort();
    let mut note = CompactString::const_new(
        "this argument is undecided at this call, and another argument decided ",
    );
    for (i, name) in names.iter().enumerate() {
        let sep = if i == 0 { "" } else { ", " };
        write!(note, "{sep}'{name}").unwrap();
    }
    note.push_str(
        " before it; annotate that argument as wide as this one (bind it to a `let` \
         with its type written)",
    );
    Ok(Some(note))
}

/// What to annotate where the argument `n` is undecided at its call: the
/// bindings it reads whose types are undecided, else the argument.
fn undecided_note<R: Rt>(ctx: &CompileCtx<R>, n: &Node<R>) -> CompactString {
    undecided_binds(ctx, n).unwrap_or(CompactString::const_new(
        "this argument is undecided at this call; annotate it (bind it to a `let` \
         with its type written)",
    ))
}

/// The bindings `n` reads whose types are undecided, to annotate.
fn undecided_binds<R: Rt>(ctx: &CompileCtx<R>, n: &Node<R>) -> Option<CompactString> {
    let mut refs = Refs::without_callees();
    n.refs(&mut refs);
    let mut binds: SmallVec<[_; 2]> = SmallVec::new();
    refs.with_external_refs(|id| {
        if let Some(b) = ctx.env.by_id.get(&id)
            && !crate::is_lowered(&b.name)
            && !b.typ.with_deref(|t| matches!(t, Some(Type::Fn(_))))
            && b.typ.has_free()
        {
            binds.push(b)
        }
    });
    binds.sort_by_key(|b| b.id);
    match &*binds {
        [] => None,
        [b] => Some(format_compact!(
            "`{}` is undecided at this call; annotate `{}` ({})",
            b.name,
            b.name,
            BoundAt(b, None)
        )),
        binds => {
            let mut names = CompactString::new("");
            for (i, b) in binds.iter().enumerate() {
                let sep = if i == 0 { "" } else { ", " };
                write!(names, "{sep}`{}` ({})", b.name, BoundAt(b, None)).unwrap();
            }
            write!(names, " are undecided at this call; annotate them").unwrap();
            Some(names)
        }
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
    /// A seq step issued this call while its function was bottom and
    /// unbound: its bind, when the function returns, is the step's birth.
    step_due: bool,
    /// The imaged instance decoded at this update's dispatch: the sites
    /// its first update leaves imaged are held after it.
    decoded: bool,
    pub(crate) fork: ForkSite,
    /// The bindings this call bound statically, its function's and its
    /// fn-typed arguments', and, for an imaged callee, its whole tree's.
    static_holds: SmallVec<[StaticHold; 1]>,
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
    ctx.builtin_bindings.get(&r.id).map(|i| &i.name)
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
            step_due: false,
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

    /// Whether this is a self-call: its target is an instance that was
    /// still checking where the call resolved, which it shares, holding
    /// none of its own.
    pub(crate) fn shares_instance(&self) -> bool {
        self.static_target.is_some() && matches!(self.callee, Callee::DynamicUnbound)
    }

    /// Where this call runs a collection whose callback is the bare name
    /// of a recursion still checking here (`array::map(xs, t)` in `t`):
    /// the collection's prototype call, a self-call of that recursion,
    /// and the binding this call names it by.
    pub(crate) fn recursion_callback(&self) -> Option<(&CallSite<R>, BindId)> {
        let Some(ApplyView::Lambda(g)) = self.resolved_apply() else { return None };
        let (call, name) = self.loop_callback(g)?;
        call.shares_instance().then_some((call, name))
    }

    /// Where this call shares `g`, a collection's instance whose
    /// callback is the recursion this call names bare (`array::map(xs,
    /// k)` in `k`, run by `array::map(ys, k)`): `g`'s prototype call,
    /// which holds the recursion's instance, and the binding this call
    /// names it by.
    pub(crate) fn shared_loop_callback<'a>(
        &'a self,
        g: &'a GXLambda<R>,
    ) -> Option<(&'a CallSite<R>, BindId)> {
        let target = self.static_target.as_ref()?;
        if !self.shares_instance() || target.instance != g.instance_id() {
            return None;
        }
        let (call, name) = self.loop_callback(g)?;
        matches!(call.resolved_apply(), Some(ApplyView::Lambda(_)))
            .then_some((call, name))
    }

    /// The prototype call of the loop of `g`, a collection-bodied
    /// instance, and the binding this call passes for the formal that
    /// call makes its call through.
    fn loop_callback<'a>(
        &'a self,
        g: &'a GXLambda<R>,
    ) -> Option<(&'a CallSite<R>, BindId)> {
        let prototype = match g.body().view() {
            NodeView::MapQ(m) => &m.prototype,
            NodeView::FoldQ(f) => &f.prototype,
            _ => return None,
        };
        let NodeView::CallSite(call) = prototype.view() else { return None };
        let NodeView::Ref(formal) = call.fnode().view() else { return None };
        let key =
            ArgKey::of_formals(&g.typ().args).zip(g.args()).find_map(|(key, pat)| {
                (pat.single_bind_id() == Some(formal.id)).then_some(key)
            })?;
        match self.arg(&key)?.view() {
            NodeView::Ref(r) => Some((call, r.id)),
            _ => None,
        }
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
        let mut fnode = match &f.kind {
            ExprKind::Ref { name } => Ref::compile(ctx, f.clone(), scope, top_id, name)?,
            _ => compile(ctx, flags, (**f).clone(), scope, top_id)?,
        };
        // a call by name reads its callee's scheme, which the call copies
        super::bind::name_value(&mut fnode, false);
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
                None,
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
    /// instantiates it; `join` meets it with the site's view of the
    /// parameter, else it is checked against it.
    #[allow(clippy::too_many_arguments)]
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
        join: Option<&mut Join<'_>>,
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
            join,
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

    /// Check every default this site omits, for each definition the
    /// callee may be. False when one is not known here: its default
    /// narrows the site's cells at its bind. `carriers` collects the
    /// labels whose default can carry a value of a cell its type holds.
    /// Each default joins `widening` as a written argument does.
    fn check_omitted_defaults(
        &self,
        ctx: &mut CompileCtx<R>,
        ftype: &FnType,
        carriers: &mut SmallVec<[ArcStr; 2]>,
        widening: &mut Widening,
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
        let mut defs = written_defs(ctx, ftype);
        if defs.is_empty()
            && let NodeView::Ref(r) = self.fnode.view()
            && let Some(fv) = ctx.bind_to_lambda.get(&r.id)
        {
            defs.push(Some(fv.clone()));
        }
        let callee = self.fnode.typ();
        let mut join = |env: &Env,
                        name: &ArcStr,
                        formal: &Type,
                        typ: &Type,
                        at: &Expr,
                        undecided| {
            let met = met_defaults::<R>(&defs, name);
            widening.join_default(env, callee, ftype, formal, typ, at, met, undecided)
        };
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
                let node = self
                    .checked_default(
                        ctx,
                        flags,
                        &self.scope,
                        f,
                        name,
                        expr,
                        &mut Refs::default(),
                        Judge::Check,
                        Some(&mut join),
                    )
                    .map_err(|e| met_default_note::<R>(e, &defs, name))?;
                if carries(&ctx.env, node.typ()) {
                    carriers.push(name.clone());
                }
                ctx.discard(node);
            }
        }
        Ok(known)
    }

    // CR claude for claude: [bug/low] The parts of one argument still meet a choice
    // in their order: over `type P<'p, 'q> = [Array<'p>, Array<'q>]`, `f(([1], 1,
    // "s"))` over `|x: (P<'a, 'b>, 'a, 'b)|` is refused ("Array<i64> could go to
    // Array<'a: unbound> or Array<'b: unbound>") and `f((1, "s", [1]))` over `|x: ('a,
    // 'b, P<'a, 'b>)|` runs, where the same three as arguments run in every order
    // since decided-arg-waits-01. The containment of the one argument raises the
    // choice at its first part, before the parts that would decide the members are
    // met, and joining the argument again finds the same; an annotation meets it the
    // same way (`let v: (P<'a, 'b>, 'a, 'b) = ([1], 1, "s")`). The skill's
    // generics.md states the order as the rule. Closing it means a containment that
    // defers a part meeting a choice until its other parts are related, as
    // ContainsHist::defer does for a function type's positions. base-r22 the same.
    // probes: ~/ptmp/cr29/scratch/r21/21t/d32/{t1,t2}.gx (choice-within-arg-order-01)
    /// Join the decided arguments that met a choice, once the written
    /// arguments and the omitted defaults have joined: each as one written
    /// after them would, again while another of them joins, so the order
    /// they stand in decides nothing. One that two undecided members
    /// could still take is refused.
    fn join_choosing(
        &self,
        ctx: &CompileCtx<R>,
        ftype: &FnType,
        widening: &mut Widening,
    ) -> Result<()> {
        let mut choosing = mem::take(&mut widening.choosing);
        let (env, def) = (&ctx.env, self.fnode.typ());
        while !choosing.is_empty() {
            let waiting = choosing.len();
            let mut refusal = None;
            let mut i = 0;
            while let Some((key, formal)) = choosing.get(i) {
                let Some(n) = self.args.get(key).and_then(|a| a.node.as_ref()) else {
                    choosing.remove(i);
                    continue;
                };
                match wrap!(n, widening.join(env, def, ftype, formal, n.typ()))? {
                    Joined::Fits => drop(choosing.remove(i)),
                    Joined::Incomparable => {
                        let (key, formal) = choosing.remove(i);
                        widening.deferred.push((Waiter::Incomparable(key), formal))
                    }
                    Joined::Choice(e) => {
                        refusal.get_or_insert(wrap!(n, ambiguous_parameter(e)));
                        i += 1
                    }
                }
            }
            if let Some(e) = refusal
                && choosing.len() == waiting
            {
                return Err(e);
            }
        }
        Ok(())
    }

    /// Check the arguments `widening` held back at their formals, once the
    /// written arguments and the omitted defaults have joined. One holding
    /// an undecided cell that the join, or an undecided argument before
    /// it, decided a variable of is refused, naming what to annotate.
    fn check_waiting(
        &self,
        ctx: &CompileCtx<R>,
        ftype: &FnType,
        widening: &mut Widening,
    ) -> Result<()> {
        self.join_choosing(ctx, ftype, widening)?;
        let deferred = mem::take(&mut *widening.deferred);
        let mut met: LPooled<AHashSet<ArcStr>> = LPooled::take();
        let env = &ctx.env;
        let arg = |key: &ArgKey| self.args.get(key).and_then(|a| a.node.as_ref());
        // whether a refusal of a waiter of type `typ` names what to annotate:
        // the join, or an undecided waiter before it, decided a variable it
        // holds, and some decision of its cells could fit; judged before the
        // check, which may decide what it reads
        let undecided = |widening: &mut Widening,
                         met: &mut AHashSet<ArcStr>,
                         formal: &Type,
                         typ: &Type|
         -> Result<bool> {
            let held = widening.held(env, self.fnode.typ(), ftype, formal);
            let joined =
                held.iter().any(|(name, tv)| tv.is_bound() || met.contains(name));
            met.extend(held.keys().cloned());
            Ok(joined && annotation_may_fit(env, formal, &held, typ)?)
        };
        let noted = |res: Result<()>, note: Option<CompactString>| match (
            res.map_err(ambiguous_parameter),
            note,
        ) {
            (Err(e), Some(note)) => Err(anyhow!("{e}: {note}")),
            (res, _) => res,
        };
        for (waiter, formal) in deferred {
            match waiter {
                Waiter::Undecided(key, copied) => {
                    let Some(n) = arg(&key) else { continue };
                    let note = undecided(widening, &mut met, &formal, n.typ())?
                        .then(|| undecided_note(ctx, n));
                    rejoin(&copied);
                    wrap!(n, noted(formal.check_contains(env, n.typ()), note))?
                }
                Waiter::Incomparable(key) => {
                    let Some(n) = arg(&key) else { continue };
                    wrap!(n, noted(formal.check_contains(env, n.typ()), None))?
                }
                Waiter::Default { typ, at, met: shared, undecided: names } => {
                    let note = match typ.has_free_in_data()
                        && undecided(widening, &mut met, &formal, &typ)?
                    {
                        true => names,
                        false => None,
                    };
                    noted(formal.check_contains(env, &typ), note)
                        .map_err(|e| match shared {
                            Some(shared) => e.context(shared),
                            None => e,
                        })
                        .at(&at)?
                }
            }
        }
        Ok(())
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
            // CR claude for claude: [perf/low] A run-time bind fuses only a collection
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
            // 2026-11-03 claude: Eric ruled (2026-11-03): D20 (b) in
            // ~/ptmp/cr-discuss.md: fuse every run-time bind, so a function value
            // costs what a named call does (a JIT compile at the first bind of each
            // new instance, cached by definition and type); measure the admin TUI's
            // first-bind cost first and bring it back if it stalls visibly.
            // 2026-11-05 claude: Built D20 (b) in lane o1 r17 and did not land it
            // (patch, pins and probes: ~/ptmp/cr29/scratch/r17-o1/bound-bind-policy/).
            // The change: build_bound calls fusion::fuse_instance for every non-slot
            // bind, and fuse_instance caches an instance whose walk fused nothing
            // (None), walking it again only where an assertion reaches the bind;
            // without that cache a node-walked recursion whose body does not fuse
            // retries fusion at every level (nf_bound.gx at depth 2000: 2001 walks,
            // 4002 recorded failures, growing with depth). Measured (instructions:u,
            // debug): b8.gx 24.5G on main to 1.15G; nf_named/nf_bound +1-3% (the
            // per-bind fingerprint). Pins in the patch:
            // lang::fusion::bound_function_value_fuses (fails without the policy
            // change: no kernel runs) and bound_unfused_instance_tried_once (fails
            // without the cache: 16 against 406 failures). Two things block it, both
            // Eric's: (1) a wrong answer under fusion:
            // lang::prune::standing_formal_refresh_marks_its_readers fails in Jit,
            // since the kernel of a bound closure reading a capture bare computes as
            // an operator over it does in the node-walk, not as the bare read does
            // (quiet-refresh-op-01); fusing every bind makes that a node-walk/JIT
            // divergence. (2) 81 run! fixtures marked FuseExpect::None now fuse (list
            // in fuseexpect-flips.txt): fixture_mode counts the regions fused by the
            // time its fusion_stats read arrives, which a run-time bind in the first
            // cycle reaches; rec: count the build alone (a snapshot at the compile's
            // end), keeping FuseExpect's documented meaning, rather than flipping 81
            // annotations on a race. The admin TUI's first-bind cost was not measured.
            // Rec: settle quiet-refresh-op-01 first, then land the patch with the
            // FuseExpect change.
            // 2026-11-05 claude: Eric ruled (11-05, D20 follow-up):
            // quiet-refresh-op-01 (a), so a fused run-time bind and the node-walk
            // agree. Build (b) with that change, and make FuseExpect count only what
            // the build fused (its doc), which settles the 81 fixtures' flips.
            // 2026-11-09 claude: Eric ruled (2026-11-09): D20 follow-up 2 (3): a quiet
            // change updates values in place, never structure. Lane 19e did not land
            // (b): its 11-05 ruling builds it with quiet-refresh-op-01 (a), which is
            // not on main and is back with Eric (the kernel half of (3) is blocked,
            // see that CR). Rebuilt on main 3cfcd2407 as branch r19-e-wip-bound-bind
            // (357421777 FuseExpect, 7c1013eae, d0c0e9857, bea429a76 the
            // strict_fusion.md rule): without (a),
            // lang::prune::standing_formal_refresh_marks_its_readers fails in Jit
            // (cycle 6: 1, the node-walk 3), a wrong answer under fusion;
            // bound_function_value_fuses, bound_unfused_instance_tried_once,
            // first_cycle_bind_is_not_the_builds pass. b8.gx instructions:u (debug):
            // base-r19 24.60G, the branch 1.04G. graphix-fuzz regress on the branch:
            // 567 programs, 0 regressions, outcome and fusion manifests 0 mismatches.
            // Land the branch with quiet-refresh-op-01 once that CR's kernel half is
            // settled; the admin TUI's first-bind cost is still to measure in quick.
            // 2026-11-10 claude: quiet-refresh-op-01 is built and reviewed on lane 20a
            // (e81a36cc6, 817ad8039; Eric's 11-10 ruling (A) with a tag bit:
            // Tag::MOVED, design/wake_catchup.md, What a wake stands) and its XCR is
            // gone: a run-time bind's closure reads a formal a wake re-stood as a fire
            // in both engines (lang::wake_outside,
            // lang::prune::standing_formal_refresh_marks_its_readers in every mode).
            // What (b) still waits on is wake-entry-split-01 (cr-discuss D30).
            // 2026-11-11 claude: wake-entry-split-01 is closed: Eric ruled (11-11)
            // that bodies are bind-driven and a wake fires nothing for anyone
            // (design/bind_driven_bodies.md), so a select a wake re-routes gives the
            // same answer in both engines and a formal a wake re-stood is read stale,
            // moved, by a run-time bind's closure in both. (b) waits on nothing but
            // its own measurement.
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
        let _ = defaults;
        ctx.enter_body(|ctx| {
            for arg in self.args.values_mut() {
                if arg.stage == ArgStage::Compiled
                    && let Some(node) = &mut arg.node
                {
                    arg.stage = ArgStage::Running;
                    let tv = node.update(ctx).clone();
                    let feeds = Feeds::Id(arg.id);
                    if publish_production(ctx, feeds, &tv, QuietAtRoot::Stand) {
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
                        ctx.rebinds(r.id),
                        ctx.bind_to_lambda.contains_key(&r.id),
                    );
                }
                if ctx.rebinds(r.id) {
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
                    NodeView::Ref(r) if !ctx.rebinds(r.id) => ctx
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
            if !farg.typ.is_function(&ctx.env) {
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
                    if ctx.rebinds(r.id) {
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
        let reborn = woke && ctx.event.born();
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
            // a bound instance sleeps while its callee is bottom and
            // resumes when the same function returns, or is born then if a
            // seq step's call was due a birth meanwhile
            if let Some(f) = self.callee.apply_mut() {
                let absent = self.absent.get_or_insert_with(|| {
                    Box::new(Absent {
                        // a call a seq step issues is the step's: as built
                        step: super::wake::in_fresh_frame(ctx).then(|| {
                            let captured =
                                TrackedFires::new(&ctx.env, ctx.rt.cycle(), 1, |_, r| {
                                    f.refs(r)
                                });
                            (SmallVec::new(), captured)
                        }),
                        unborn: None,
                    })
                });
                if reborn {
                    let view = ctx.event.view;
                    let frame = super::wake::in_fresh_frame(ctx);
                    absent.unborn = Some(Unborn { view, frame });
                }
                absent.observe(ctx, &set, instance_boundary(&*f));
                f.sleep(ctx);
                self.slept.set();
            } else if super::wake::in_fresh_frame(ctx) {
                self.step_due = true
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
        // a fresh bind's quiet arguments stand: a formal fires when its
        // argument does, and an omitted default is a constant
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
                if publish_production(ctx, Feeds::Id(arg.id), &tv, QuietAtRoot::Stand) {
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
                // A fresh bind is a body's entry: nothing in it is born,
                // unless a seq step issued the call, which is owed what
                // its body reads.
                let owed = super::wake::deliver_owed(ctx, |r| f.refs(r));
                let arg_refs = &mut self.arg_refs;
                let tv = match mem::take(&mut self.step_due) {
                    true => ctx.under(View::Birth, |ctx| f.update(ctx, arg_refs).clone()),
                    false => ctx.enter_body(|ctx| f.update(ctx, arg_refs).clone()),
                };
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
            // a function that moved at a wake moved what its call gives
            Some(mut tv) => {
                let tag = tv.tag().moved_if(fnode_tag.moved());
                tv.retag(tag);
                self.resident.set(tv)
            }
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
                // a plain reader: the held reads only mark the site's boundary, and
                // the body decodes at the site's first update, the instance's birth,
                // which reads every standing value, so no late fire is owed
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

    pub(crate) fn typecheck0(&mut self, ctx: &mut CompileCtx<R>) -> Result<()> {
        wrap!(self.fnode, self.fnode.typecheck0(ctx))?;
        let mut fresh = false;
        let mut self_call = false;
        let ftype = match self.ftype.as_ref() {
            Some(ftype) => ftype, // already initialized
            None => {
                let ftype = deref_typ!("fn", ctx, self.fnode.typ(); FnUnion::refusal;
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
                    (*ftype).clone()
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
        let mut prebound: LPooled<AHashSet<usize>> = LPooled::take();
        {
            let mut cells: LPooled<AHashMap<ArcStr, TVar>> = LPooled::take();
            for farg in ftype.args.iter() {
                farg.typ.collect_tvars(&mut cells);
            }
            if let Some(typ) = &ftype.vargs {
                typ.collect_tvars(&mut cells);
            }
            prebound.extend(
                cells.values().filter(|tv| tv.is_bound()).map(|tv| tv.cell_addr()),
            );
        }
        for (_, farg, key) in formals {
            let Some(arg) = self.args.get_mut(&key) else { continue };
            if let Some(n) = arg.node.as_mut() {
                let opening =
                    widening.opening(&ctx.env, self.fnode.typ(), ftype, &farg.typ);
                let fit = typecheck_arg(ctx, &farg.typ, opening, &prebound, n)?;
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
                    let opening =
                        widening.opening(&ctx.env, self.fnode.typ(), ftype, typ);
                    let fit = typecheck_arg(ctx, typ, opening, &prebound, n)?;
                    widening.fit(&ctx.env, self.fnode.typ(), ftype, &key, typ, n, fit)?;
                }
            }
        }
        let mut carriers = SmallVec::new();
        // a recursive call shares its definition's cells, which the
        // defaults it omits narrow as a fresh instance's
        if fresh || self_call {
            self.defaults_open =
                !self.check_omitted_defaults(ctx, ftype, &mut carriers, &mut widening)?;
        }
        self.check_waiting(ctx, ftype, &mut widening)?;
        self.check_value_defaults(ctx, ftype)?;
        let mut unread_callbacks = SmallVec::new();
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
                if farg.typ.is_function(&ctx.env) {
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
                if let Some(own) = cx.loop_own_call(self) {
                    return emit_lambda_call_node(cx, self, Called::Own(own));
                }
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
        if let Some(g) = cx.shared_loop(self)
            && let Some(cv) = Apply::emit_clif(g, self, cx)?
        {
            return Ok(cv);
        }
        // A value-position self-call: `self.callee` is unresolved here.
        if let Some(own) = cx.own_call(self) {
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

/// The walk-pruning boundary around the body of the instance `f`.
fn instance_boundary<R: Rt>(f: &dyn Apply<R>) -> Option<&BoundaryRef> {
    match f.view() {
        ApplyView::Lambda(l) => l.body_boundary(),
        ApplyView::BuiltIn(_) | ApplyView::Opaque => None,
    }
}

/// A bound instance asleep while its function is bottom. It resumes when
/// the same function returns, as a body does; a call a seq step issued is
/// caught up then, or makes up the birth it was due meanwhile.
#[derive(Debug)]
struct Absent {
    /// For a call a seq step issued: the arguments that fired meanwhile,
    /// and the tracker of the inputs the instance captures.
    step: Option<(SmallVec<[BindId; 2]>, TrackedFires)>,
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

    /// Record this cycle's fires, for a call a seq step issued: the
    /// arguments published in `set`, and the captured inputs.
    fn observe<R: Rt>(
        &mut self,
        ctx: &ExecCtx<'_, R>,
        set: &Published,
        body: Option<&BoundaryRef>,
    ) {
        let Some((missed, captured)) = &mut self.step else { return };
        for id in set.iter() {
            let fired = ctx.event.variables.get(id).is_some_and(|tv| tv.tag().triggers());
            if fired && !missed.contains(id) {
                missed.push(*id);
            }
        }
        captured.observe(ctx);
        captured.observe_late(ctx, |_, b| BoundaryRef::held_by(body, b));
    }

    /// Run `update` under `view`: a body's resume, or, for a call a seq
    /// step issued, with one catch-up fire for each argument and captured
    /// input that fired unseen and is not delivered live this cycle, at
    /// its current value.
    fn wake<R: Rt, T>(
        self,
        ctx: &mut ExecCtx<'_, R>,
        view: View,
        update: impl FnOnce(&mut ExecCtx<'_, R>) -> T,
    ) -> T {
        let Some((missed, mut captured)) = self.step else {
            return ctx.under(view, |ctx| super::wake::in_body(ctx, update));
        };
        let mut injected = super::wake::Injected::new();
        for id in missed {
            super::wake::inject(ctx, id, &mut injected);
        }
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
        if publish_production(ctx, Feeds::Id(arg.id), tv, root) {
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
}

/// Publish `tv`, a production feeding `feeds`, to the readers of this
/// dispatch. A fire (a value or a fresh bottom) is delivered
/// ([`ExecCtx::deliver`]); a quiet one per `root`. Returns whether the
/// overlay was written.
pub(crate) fn publish_production<R: Rt>(
    ctx: &mut ExecCtx<'_, R>,
    feeds: Feeds<'_>,
    tv: &TagValue,
    root: QuietAtRoot,
) -> bool {
    let tag = tv.tag();
    let bottom = tag.is_bottom();
    let fired = tag.triggers();
    // what a wake recomputed moved stands whatever the pass keeps
    let root = match root {
        QuietAtRoot::Skip if tag.moved() => QuietAtRoot::Stand,
        root => root,
    };
    if !fired && matches!(root, QuietAtRoot::Skip) {
        return false;
    }
    let mut put = |id: BindId, v: Value| match (fired, root) {
        (true, _) => ctx.deliver(id, TagValue::tagged(v, tag), Notify::Walk),
        (false, QuietAtRoot::Skip) => (),
        (false, QuietAtRoot::Stand) => ctx.stand(id, TagValue::tagged(v, tag)),
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
    fired
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
    use netidx_value::Typ;

    // A call's escape checks walk the argument's shared nodes once:
    // `carried_out` over a 24-deep shared tuple holding a cell.
    #[test]
    fn carried_out_walks_a_shared_node_once() {
        let mut t = Type::Tuple(TArc::from_iter([
            Type::empty_tvar(),
            Type::Primitive(Typ::I64.into()),
        ]));
        for _ in 0..24 {
            t = Type::Tuple(TArc::from_iter([t.clone(), t.clone()]));
        }
        WALK_STEPS.with(|n| n.set(0));
        assert!(carried_out(&[], &t).is_none());
        assert!(WALK_STEPS.with(|n| n.get()) <= 2 * (2 * 25 + 2));
    }
}
