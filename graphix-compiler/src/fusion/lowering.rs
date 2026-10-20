//! Fusion analysis: fastcall-site discovery, lambda kernel signature
//! derivation, and the env-free type-ref expander. Code generation
//! lives with each node's `emit_clif`; this module supplies what those
//! emitters consume.

use crate::{
    ApplyView, BindId, CompileCtx, LambdaInstanceId, Node, NodeView, PrintFlag, Refs, Rt,
    UserEvent,
    effects::NoLoop,
    env::Env,
    expr::{Expr, ExprId, ExprKind, ModPath},
    fusion::{
        self, Brief, FusionBlocker,
        kernel_abi::{
            self, AbiKind, KernelParam, KernelSig, ParamKind, Seen, abi_kind,
            expand_key_fp, freeze_for_abi_normalized, scalar_prim,
        },
    },
    node::{callsite::CallSite, lambda::GXLambda},
    profile::{self, Phase},
    typ::{FnArgKind, FnArgType, FnType, Type, TypeRef},
};
use arcstr::ArcStr;
use compact_str::{CompactString, format_compact};
use enumflags2::BitFlags;
use netidx_value::Value;
use nohash::IntSet;
use poolshark::local::LPooled;
use smallvec::SmallVec;
use std::{
    cell::{Cell, RefCell},
    collections::hash_map::Entry,
};
use triomphe::Arc;

pub use kernel_abi::PrimType;

/// A lambda kernel, cached per monomorphization in
/// `FusionCtx::kernels` and shared by every call site that reaches it.
#[derive(Debug)]
pub struct CachedKernel {
    /// The kernel; calls resolve by its identity ([`kernel_abi::kernel_key`]).
    pub kernel: Arc<KernelSig>,
    /// The callee's input types in signature order, formals then
    /// captures, frozen at build time: the caller's type authority for
    /// arg classification (env is unavailable at emit time).
    pub arg_types: Vec<Type>,
    /// Captured outer bindings, appended to the signature after the
    /// formals in this order; callers forward each capture's current value.
    pub captures: Vec<CaptureSlot>,
    /// The binding a self-recursive body calls itself through; body
    /// emission recognises self-calls by it.
    pub self_call: Option<BindId>,
    /// Fastcall/cast sites in the body, by `Apply` expr id.
    pub apply_sites: nohash::IntMap<ExprId, BuiltinCallSiteInfo>,
}

/// One captured outer-scope binding lifted into a lambda kernel's
/// signature.
#[derive(Debug, Clone)]
pub struct CaptureSlot {
    /// Keyed by id, not name, so a same-named shadow in the parent
    /// cannot mis-route the capture.
    pub bind_id: BindId,
    /// The kernel input slot name the body's `Ref` resolves to.
    pub name: ArcStr,
    /// Frozen type of the capture.
    pub typ: Type,
}

/// How a discovered site dispatches inside a kernel: a `FastFn` called
/// directly on the site's stack buffer, or a `TypedFastFn` with the
/// `Type` that directs its result (a resolved return type, or a cast's
/// target).
#[derive(Debug, Clone)]
pub enum SiteDispatch {
    /// A builtin's plain fast fn, by the builtin's name.
    Fast { name: ArcStr, f: crate::FastFn },
    /// A builtin's typed fast fn, directed by the site's resolved type.
    Typed { name: ArcStr, f: crate::TypedFastFn, typ: Arc<FnType> },
    /// A cast from `source` to `target`, through [`cast_typed`].
    Cast { target: Type, source: Type },
    /// An `Effect::Io` builtin: `f` is its result, and each fire of
    /// argument `trigger` (`arg`, typed `typ`) queues it to perform.
    Io { name: ArcStr, f: crate::FastFn, trigger: usize, arg: Expr, typ: Type },
}

/// The cast pseudo-site's fn: the same `cast_from` call `TypeCast::update`
/// makes on the node-walk. `typ` is the site's `(target, source)` pair.
pub(crate) type CastFn = fn(&Env, &Type, &[Value]) -> Option<Value>;

pub(crate) fn cast_typed(env: &Env, typ: &Type, args: &[Value]) -> Option<Value> {
    let Type::Tuple(ts) = typ else { return None };
    Some(ts[0].cast_from(env, &ts[1], args[0].clone()))
}

/// Where one buffer slot of a fastcall site comes from: an arg the call
/// wrote (its index in the source-order arg list), or a labeled default
/// the call left to the callee (the CallSite's compiled default node,
/// by name).
#[derive(Debug, Clone)]
pub enum MarshalArg {
    Call(usize),
    Default(ArcStr),
}

/// Layout of one fusable call site (a builtin with a fast fn, or a
/// non-inline `cast<T>(x)`). `marshal_args[i]` feeds buffer slot `i`,
/// typed `arg_types[i]` (frozen; variadic args carry the element type).
#[derive(Debug, Clone)]
pub struct BuiltinCallSiteInfo {
    pub marshal_args: Vec<MarshalArg>,
    pub arg_types: Vec<Type>,
    pub return_type: Type,
    pub dispatch: SiteDispatch,
}

/// Output of [`walk_node_for_builtin_calls`], keyed by `Apply` expr id.
#[derive(Debug, Default, Clone)]
pub struct BuiltinCallDiscovery {
    pub apply_sites: nohash::IntMap<ExprId, BuiltinCallSiteInfo>,
}

impl BuiltinCallDiscovery {
    /// Two instances of one callback in a region share their sites'
    /// ids, and emission finds a site by id, so a site that two
    /// instances type apart refuses the region.
    fn insert(
        &mut self,
        spec: &Expr,
        info: BuiltinCallSiteInfo,
    ) -> Result<(), FusionBlocker> {
        match self.apply_sites.entry(spec.id) {
            Entry::Vacant(e) => {
                e.insert(info);
                Ok(())
            }
            Entry::Occupied(e)
                if e.get().arg_types == info.arg_types
                    && e.get().return_type == info.return_type =>
            {
                Ok(())
            }
            Entry::Occupied(_) => Err(FusionBlocker {
                spec: spec.clone(),
                reason: "two instances in one region type this call apart".into(),
            }),
        }
    }
}

/// Discover the fusable call sites in a subtree: every `CallSite` on a
/// builtin with a fast fn and every non-inline `cast<T>(x)`. Descent
/// is [`fusion::for_each_emitted_node`], so collection callbacks are
/// included and ordinary lambda bodies are not. An [`fusion::effect_blocker`]
/// anywhere, a [`root_blocker`] at the root, or a builtin without a
/// fast-call entry rejects the region. Other unsupported sites are
/// omitted and checked by emission.
pub(crate) fn walk_node_for_builtin_calls<R: Rt, E: UserEvent>(
    node: &Node<R, E>,
    ctx: &CompileCtx<R, E>,
    out: &mut BuiltinCallDiscovery,
) -> Result<(), FusionBlocker> {
    let mut failure = None;
    let mut selects: LPooled<Vec<&Node<R, E>>> = LPooled::take();
    fusion::for_each_emitted_node(node, &mut |n| {
        if failure.is_some() {
            return;
        }
        let reason = match n.view() {
            NodeView::CallSite(cs) => {
                failure = try_register_builtin_call_from_callsite(cs, ctx, out);
                return;
            }
            NodeView::TypeCast(tc) => {
                failure = try_register_cast(tc, out).err();
                return;
            }
            NodeView::Select(_) => {
                selects.push(n);
                return;
            }
            _ => fusion::effect_blocker(n)
                .or_else(|| std::ptr::eq(n, node).then(|| root_blocker(n)).flatten()),
        };
        let Some(reason) = reason else { return };
        failure = Some(FusionBlocker { spec: n.spec().clone(), reason: reason.into() });
    });
    match failure
        .or_else(|| arm_raise_blocker(node, &selects))
        .or_else(|| arm_io_blocker(&selects, ctx))
    {
        Some(failure) => Err(failure),
        None => Ok(()),
    }
}

/// A handler-ful `?` under a select arm raises on an edge its arm may
/// owe to the select's fire tracker: an input whose fire no selected
/// arm saw is re-delivered FIRED when this arm reads it
/// (design/wake_catchup.md), and a constant fires at the arm's first
/// entry only, a wake being a resume. A kernel derives its selection
/// fresh every run and remembers neither,
/// so the raise node-walks. `selects` are the region's selects in
/// pre-order; one nested in another's arm is covered by the outer walk.
fn arm_raise_blocker<R: Rt, E: UserEvent>(
    root: &Node<R, E>,
    selects: &[&Node<R, E>],
) -> Option<FusionBlocker> {
    // CR claude for eric: [bug/medium] Only handler-ful raises count here and in
    // `claims_no_site_memory`. A handler-less `?` or a `$` over a constant error also
    // owes its report to a birth or Step view a kernel lacks: a fused select arm never
    // reports `e?` at the arm's first entry, and a native tail loop reports it at its
    // entry depth only, not at each newly reached depth. The node-walk reports both, so
    // a failure that `--no-fusion` prints is missing from the default run, against the
    // rule that `$` and handler-less `?` log from both backends. The fuzzer cannot see
    // this, because its oracle compares neither FAILURE_TARGET nor warn logs. Decide
    // whether these reports are owed. If they are, gate a handler-less Qop whose
    // operand does not follow the arm's entry binds (as `io_follows` does for Io) here,
    // and on the formals in `claims_no_site_memory`. If they are not, say so in
    // CLAUDE.md. probe:
    // design/review-2026-10-19/repro/f-call-flow-arm-entry-unhandled-raise.gx (0 vs 1
    // reports) and f-call-flow-tail-loop-unhandled-raise.gx (1 vs 6) (f-call-flow-02)
    let mut any = false;
    if !selects.is_empty() {
        fusion::for_each_reachable_node(root, &mut |n| {
            any |= fusion::raises_to_handler(n)
        });
    }
    if !any {
        return None;
    }
    let mut nested: LPooled<nohash::IntSet<usize>> = LPooled::take();
    let mut found: Option<&Node<R, E>> = None;
    for s in selects {
        let NodeView::Select(sel) = s.view() else { continue };
        if found.is_some() {
            break;
        }
        if nested.contains(&(*s as *const Node<R, E> as usize)) {
            continue;
        }
        for (_, body) in sel.arms.iter() {
            fusion::for_each_reachable_node(body, &mut |n| match n.view() {
                NodeView::Select(_) => {
                    nested.insert(n as *const Node<R, E> as usize);
                }
                _ if found.is_none() && fusion::raises_to_handler(n) => found = Some(n),
                _ => (),
            });
        }
    }
    found.map(|n| FusionBlocker {
        spec: n.spec().clone(),
        reason: "a `?` under a handler raises an edge its arm may owe to the select's \
                 fire tracker — arm entry is the node-walk's"
            .into(),
    })
}

/// A call that performs (an `Effect::Io` builtin's, or a function's
/// that reaches one) under a select arm performs on a fire of its
/// arguments, and a kernel derives its selection fresh every run: a fire
/// the arm missed asleep, which the node-walk re-delivers at entry
/// (design/wake_catchup.md), and a constant, which fires at the arm's
/// wake, never reach it. So it fuses there only when each argument that
/// can trigger it fires at every entry: no arm of the select has a guard
/// (a guard re-enters an arm under a standing scrutinee) and the
/// argument reads the scrutinee or one of the arm's binds, at every
/// select it sits under. `selects` are the region's selects.
fn arm_io_blocker<R: Rt, E: UserEvent>(
    selects: &[&Node<R, E>],
    ctx: &CompileCtx<R, E>,
) -> Option<FusionBlocker> {
    let mut found: Option<&Node<R, E>> = None;
    for s in selects {
        let NodeView::Select(sel) = s.view() else { continue };
        let guarded = sel.arms.iter().any(|(p, _)| p.guard.is_some());
        let scrutinee = match sel.arg.node.view() {
            NodeView::Ref(r) => Some(r.id),
            _ => None,
        };
        for (pat, body) in sel.arms.iter() {
            // every performing call fires whenever the arm is entered: its
            // trigger fires with the scrutinee or the arm's binds
            let mut entry: LPooled<IntSet<BindId>> = LPooled::take();
            entry.extend(scrutinee);
            pat.structure_predicate.ids(&mut |id| {
                entry.insert(id);
            });
            let follows =
                || crate::analysis::io_follows(body, &entry, ctx, &mut IntSet::default());
            if guarded && performs(body, ctx) || !guarded && !follows() {
                found = Some(body);
            }
            if found.is_some() {
                return found.map(io_blocker);
            }
        }
    }
    None
}

/// Whether `n` makes a performing call.
fn performs<R: Rt, E: UserEvent>(n: &Node<R, E>, ctx: &CompileCtx<R, E>) -> bool {
    let mut performs = false;
    fusion::for_each_emitted_node(n, &mut |x| {
        performs = performs
            || matches!(x.view(), NodeView::CallSite(cs) if performing_args(cs, ctx).is_some());
    });
    performs
}

fn io_blocker<R: Rt, E: UserEvent>(n: &Node<R, E>) -> FusionBlocker {
    FusionBlocker {
        spec: n.spec().clone(),
        reason: "an effect under a select arm whose trigger may not fire when the arm \
                 is entered — arm entry is the node-walk's"
            .into(),
    }
}

/// When `cs` performs, the arguments whose fire can make it perform:
/// an `Effect::Io` builtin's trigger, or every argument of a function
/// that reaches one.
pub(crate) fn performing_args<'a, R: Rt, E: UserEvent>(
    cs: &'a CallSite<R, E>,
    ctx: &CompileCtx<R, E>,
) -> Option<SmallVec<[&'a Node<R, E>; 4]>> {
    let builtin = match cs.fnode().view() {
        NodeView::Ref(r) => ctx.env.by_id.get(&r.id).and_then(|bind| {
            ctx.builtin_bindings.get(&(bind.scope.clone(), bind.name.clone()))
        }),
        _ => None,
    };
    if let Some(info) = builtin {
        let crate::Effect::Io(Some(io)) = ctx.builtin_effect(&info.name) else {
            return None;
        };
        let args = &info.typ.args;
        let node = match &args.get(io.trigger)?.kind {
            FnArgKind::Positional { .. } => cs.arg_positional(
                args[..io.trigger]
                    .iter()
                    .filter(|a| matches!(a.kind, FnArgKind::Positional { .. }))
                    .count(),
            ),
            FnArgKind::Labeled { name, .. } => cs.arg_named(name),
        };
        return Some(node.into_iter().collect());
    }
    crate::analysis::call_facts(cs, ctx).io.then(|| cs.arg_nodes().collect())
}

/// A root that can never emit a value. Nested, the same node may sit
/// in a statement block emission discards, so only the root rejects.
fn root_blocker<R: Rt, E: UserEvent>(node: &Node<R, E>) -> Option<&'static str> {
    match node.view() {
        NodeView::Sample(_) => Some("sample keeps cross-cycle state"),
        NodeView::Any(_) => Some("any depends on partial argument delivery"),
        NodeView::Never(_) => Some("never consumes inputs without producing a value"),
        NodeView::ByRef(_) | NodeView::Deref(_) => {
            Some("references require the node-walk")
        }
        _ => None,
    }
}

/// The `(source, target)` prims of a cast emitted inline: numeric
/// scalar to numeric scalar, branchless and infallible. Every other
/// cast is a discovered call site.
pub(crate) fn inline_cast(source: &Type, target: &Type) -> Option<FittingCast> {
    let src = scalar_prim(source).filter(|p| p.is_numeric())?;
    let tgt = PrimType::from_type(target).filter(|p| p.is_numeric())?;
    always_fits(src, tgt).then_some(FittingCast { src, tgt })
}

/// A numeric cast every source value survives: an integer widened,
/// any number to a float. Only [`inline_cast`] makes one.
#[derive(Debug, Clone, Copy)]
pub(crate) struct FittingCast {
    src: PrimType,
    tgt: PrimType,
}

impl FittingCast {
    pub(crate) fn src(self) -> PrimType {
        self.src
    }

    pub(crate) fn tgt(self) -> PrimType {
        self.tgt
    }
}

/// Whether every `src` value converts to `tgt` with no refusal: any number
/// to a float, an integer to one whose range holds the source's. The rest
/// cast through `Type::cast_value`, which refuses a value that does not fit.
fn always_fits(src: PrimType, tgt: PrimType) -> bool {
    let bits = |p: PrimType| match p {
        PrimType::I8 | PrimType::U8 => 8,
        PrimType::I16 | PrimType::U16 => 16,
        PrimType::I32 | PrimType::U32 | PrimType::F32 => 32,
        _ => 64,
    };
    match (src.is_integer(), tgt.is_integer()) {
        (_, false) => true,
        (false, true) => false,
        (true, true) => match (src.is_signed(), tgt.is_signed()) {
            (false, false) | (true, true) => bits(tgt) >= bits(src),
            (false, true) => bits(tgt) > bits(src),
            (true, false) => false,
        },
    }
}

/// `t` frozen for the kernel ABI, or the blocker naming it at `spec`.
fn frozen_or_blocked(t: &Type, spec: &Expr, what: &str) -> Result<Type, FusionBlocker> {
    freeze_for_abi_normalized(t).ok_or_else(|| FusionBlocker {
        spec: spec.clone(),
        reason: format_compact!("{what} type {} has no kernel ABI form", Brief(t)),
    })
}

/// Register a cast that is not emitted inline.
fn try_register_cast<R: Rt, E: UserEvent>(
    tc: &crate::node::TypeCast<R, E>,
    out: &mut BuiltinCallDiscovery,
) -> Result<(), FusionBlocker> {
    let source = tc.n.typ();
    if inline_cast(source, &tc.target).is_some() {
        return Ok(());
    }
    let arg_frozen = frozen_or_blocked(source, &tc.spec, "the cast source's")?;
    let ret_frozen = frozen_or_blocked(&tc.typ, &tc.spec, "the cast result's")?;
    out.insert(
        &tc.spec,
        BuiltinCallSiteInfo {
            marshal_args: vec![MarshalArg::Call(0)],
            arg_types: vec![arg_frozen],
            return_type: ret_frozen,
            dispatch: SiteDispatch::Cast {
                target: tc.target.clone(),
                source: source.clone(),
            },
        },
    )
}

/// Register one CallSite as a fastcall site, if it qualifies. Types
/// come from the CallSite's resolved FnType and its compiled arg and
/// return nodes, never the AST, which is unresolved for generic
/// builtins.
fn try_register_builtin_call_from_callsite<R: Rt, E: UserEvent>(
    cs: &CallSite<R, E>,
    ctx: &CompileCtx<R, E>,
    out: &mut BuiltinCallDiscovery,
) -> Option<FusionBlocker> {
    let apply_expr = cs.spec();
    let a = match &apply_expr.kind {
        ExprKind::Apply(a) => a,
        _ => return None,
    };
    let path = match &a.function.kind {
        ExprKind::Ref { name } => name,
        _ => return None,
    };
    let NodeView::Ref(r) = cs.fnode().view() else { return None };
    let bind = ctx.env.by_id.get(&r.id)?;
    let key = (bind.scope.clone(), bind.name.clone());
    let info = match ctx.builtin_bindings.get(&key) {
        Some(i) => i.clone(),
        None => return None,
    };
    let effect = ctx.builtin_effect(info.name.as_str());
    let fastcall = match effect.fastcall() {
        Some(fastcall) => fastcall,
        None => {
            return Some(FusionBlocker {
                spec: apply_expr.clone(),
                reason: format_compact!("builtin `{path}` has no fast-call entry"),
            });
        }
    };
    // `cs.ftype()` is `None` when typecheck did not reach this site;
    // the binding's generic FnType then usually fails to freeze.
    let fn_type: FnType = match cs.ftype() {
        Some(ft) => ft.resolve_tvars(),
        None => (*info.typ).clone(),
    };
    let mut call_positional: SmallVec<[usize; 8]> = SmallVec::new();
    let mut call_labeled: LPooled<ahash::AHashMap<&str, usize>> = LPooled::take();
    for (call_idx, (label, _)) in a.args.iter().enumerate() {
        match label {
            Some(name) => {
                call_labeled.insert(name.as_str(), call_idx);
            }
            None => call_positional.push(call_idx),
        }
    }
    let mut arg_types: Vec<Type> = Vec::new();
    let mut written_types: SmallVec<[Type; 8]> = SmallVec::new();
    let mut marshal_args: Vec<MarshalArg> = Vec::new();
    let mut pos_iter = call_positional.iter().enumerate();
    for fa in fn_type.args.iter() {
        match &fa.kind {
            FnArgKind::Positional { .. } => {
                let (pos_idx, call_idx) = match pos_iter.next() {
                    Some(p) => p,
                    None => return None,
                };
                let arg_typ = cs
                    .arg_positional(pos_idx)
                    .map(|n| n.typ().clone())
                    .unwrap_or_else(|| fa.typ.clone());
                let kt = match frozen_or_blocked(&arg_typ, apply_expr, "an argument's") {
                    Ok(t) => t,
                    Err(b) => return Some(b),
                };
                arg_types.push(kt);
                written_types.push(arg_typ);
                marshal_args.push(MarshalArg::Call(*call_idx));
            }
            FnArgKind::Labeled { name, has_default } => {
                // A fast fn sees the buffer as the whole argument list,
                // so an unwritten label marshals the compiled default.
                let (source, arg_typ) = match call_labeled.remove(name.as_str()) {
                    Some(call_idx) => (
                        MarshalArg::Call(call_idx),
                        cs.arg_named(name)
                            .map(|n| n.typ().clone())
                            .unwrap_or_else(|| fa.typ.clone()),
                    ),
                    None => {
                        if !*has_default {
                            return None;
                        }
                        let Some(n) = cs.arg_named(name) else { return None };
                        (MarshalArg::Default(name.clone()), n.typ().clone())
                    }
                };
                let kt = match frozen_or_blocked(&arg_typ, apply_expr, "an argument's") {
                    Ok(t) => t,
                    Err(b) => return Some(b),
                };
                arg_types.push(kt);
                written_types.push(arg_typ);
                marshal_args.push(source);
            }
        }
    }
    let remaining: SmallVec<[_; 8]> = pos_iter.collect();
    if !remaining.is_empty() {
        if fn_type.vargs.is_none() {
            return None;
        }
        for (pos_idx, call_idx) in remaining {
            let arg_typ = cs
                .arg_positional(pos_idx)
                .map(|n| n.typ().clone())
                .or_else(|| fn_type.vargs.as_ref().and_then(|t| t.deref_cloned()));
            let arg_typ = match arg_typ {
                Some(t) => t,
                None => return None,
            };
            let kt = match frozen_or_blocked(&arg_typ, apply_expr, "an argument's") {
                Ok(t) => t,
                Err(b) => return Some(b),
            };
            arg_types.push(kt);
            written_types.push(arg_typ);
            marshal_args.push(MarshalArg::Call(*call_idx));
        }
    }
    if !call_labeled.is_empty() {
        return None;
    }
    let ret_typ = cs.typ().clone();
    let return_type = match frozen_or_blocked(&ret_typ, apply_expr, "its result's") {
        Ok(t) => t,
        Err(b) => return Some(b),
    };
    if !arg_types.iter().all(|t| is_call_arg_supported(t)) {
        return None;
    }
    if abi_kind(&return_type).is_none() {
        return None;
    }
    let name: ArcStr = info.name.as_str().into();
    let dispatch = match (effect, fastcall) {
        (crate::Effect::Io(Some(io)), _) => {
            let MarshalArg::Call(i) = marshal_args[io.trigger] else {
                return Some(FusionBlocker {
                    spec: apply_expr.clone(),
                    reason: format_compact!("`{path}`'s trigger is its default"),
                });
            };
            SiteDispatch::Io {
                name,
                f: io.result,
                trigger: io.trigger,
                arg: a.args[i].1.clone(),
                typ: written_types[io.trigger].clone(),
            }
        }
        (_, crate::FastCall::Plain(f)) => SiteDispatch::Fast { name, f },
        (_, crate::FastCall::Typed(f)) => {
            SiteDispatch::Typed { name, f, typ: Arc::new(fn_type.clone()) }
        }
    };
    out.insert(
        apply_expr,
        BuiltinCallSiteInfo { marshal_args, arg_types, return_type, dispatch },
    )
    .err()
}

/// The bare identifier of a single-level path; a kernel reads a `Ref`
/// from a local slot, so a module-qualified path has none.
pub(crate) fn ident_of(path: &ModPath) -> Option<&str> {
    let s: &str = path.0.as_ref();
    let base = netidx_core::path::Path::basename(s)?;
    if netidx_core::path::Path::levels(s) != 1 {
        return None;
    }
    Some(base)
}

/// The constant `Value` of a node, seeing through `ExplicitParens`.
/// `None` for anything that isn't a compile-time-known literal.
pub(crate) fn node_const_value<R: Rt, E: UserEvent>(node: &Node<R, E>) -> Option<Value> {
    crate::stack::ensure_sufficient(|| node_const_value_inner(node))
}

fn node_const_value_inner<R: Rt, E: UserEvent>(node: &Node<R, E>) -> Option<Value> {
    match node.view() {
        NodeView::Constant(c) => Some(c.value.clone()),
        NodeView::ExplicitParens(ep) => node_const_value(&ep.n),
        NodeView::Array(a) => const_valarray(&a.n),
        NodeView::ListLit(l) => const_valarray(&l.n).map(|v| match v {
            Value::Array(a) => crate::node::list::from_iter(a.iter().cloned()),
            v => v,
        }),
        NodeView::Tuple(t) => {
            let mut vals = const_values(&t.n)?;
            Some(crate::rep::tuple(vals.drain(..)))
        }
        NodeView::Variant(v) => {
            let mut vals = const_values(&v.n)?;
            Some(crate::rep::make_variant(v.tag.clone(), vals.drain(..)))
        }
        NodeView::Struct(st) => {
            let mut vals = const_values(&st.n)?;
            let fields = st.names.iter().cloned().zip(vals.drain(..));
            Some(crate::rep::structure(fields))
        }
        NodeView::Map(m) => const_map(&m.n),
        _ => None,
    }
}

/// Fold element nodes into a constant `Value::Array` (the runtime shape
/// of array and tuple literals), or `None` if any element isn't constant.
fn const_valarray<R: Rt, E: UserEvent>(elems: &[Node<R, E>]) -> Option<Value> {
    let mut vals = const_values(elems)?;
    Some(Value::Array(netidx_value::ValArray::from_iter_exact(vals.drain(..))))
}

/// The constant value of each node, or `None` if one isn't constant.
fn const_values<R: Rt, E: UserEvent>(
    elems: &[Node<R, E>],
) -> Option<LPooled<Vec<Value>>> {
    let mut vals: LPooled<Vec<Value>> = LPooled::take();
    for c in elems.iter() {
        vals.push(node_const_value(c)?);
    }
    Some(vals)
}

/// Fold a map literal's entries (each key, then its value) into a
/// constant `Value::Map`, or `None` if any entry isn't constant.
pub(crate) fn const_map<R: Rt, E: UserEvent>(entries: &[Node<R, E>]) -> Option<Value> {
    let mut map = netidx_value::Map::new();
    for kv in entries.chunks_exact(2) {
        map.insert_cow(node_const_value(&kv[0])?, node_const_value(&kv[1])?);
    }
    Some(Value::Map(map))
}

/// Whether a value of an abstract type, or of `Any`, can pass through a
/// kernel built from `n`, its callees included: a core-trait impl may
/// run on it.
pub(crate) fn meets_abstract<R: Rt, E: UserEvent>(n: &Node<R, E>, env: &Env) -> bool {
    let mut meets = false;
    fusion::for_each_reachable_node(n, &mut |x| {
        meets = meets || x.typ().holds_abstract(env)
    });
    meets
}

/// Expand named types to their definitions through composites so
/// `abi_kind`/`freeze_for_abi` can classify a shape env-free. Abstract
/// types are leaves. A recursive expansion is left as-is (the kernel
/// then does not fuse); non-regular recursion is stopped by a length
/// backstop on the expansion chain. Truncation only ever de-fuses, so
/// the work budget is small.
pub(crate) fn expand_refs(typ: &Type, env: &Env) -> Type {
    super::TypeMemo::expanded(typ, || {
        let _profile = profile::phase(Phase::ExpandRefs);
        let cx = ResolveCx {
            budget: 2_048,
            size_cap: FUSION_SIZE_CAP,
            ..ResolveCx::default()
        };
        expand_ref_d(typ, env, None, &cx).unwrap_or_else(|| typ.clone())
    })
}

/// No kernel encodes a type that unfolds past this; resolving further
/// is work the freeze would discard.
pub(crate) const FUSION_SIZE_CAP: u32 = 4_096;

/// Whether `t` unfolds, as a tree, past `cap` nodes. A shared part
/// counts at every place it unfolds but is walked once, so the check is
/// linear in the type's allocations however much it shares.
pub(crate) fn unfolds_past(t: &Type, cap: u32) -> bool {
    fn size(t: &Type, cap: u64, memo: &mut ahash::AHashMap<super::Identity, u64>) -> u64 {
        crate::stack::ensure_sufficient(|| {
            if cap == 0 {
                return 1;
            }
            if let Type::TVar(tv) = t {
                return tv.binding().map_or(1, |b| size(&b, cap, memo));
            }
            let key = match t {
                Type::Abstract { params, .. } => Some((
                    std::mem::discriminant(t),
                    Arc::as_ptr(params) as *const () as usize,
                    0,
                )),
                _ => super::identity(t).map(|(key, _)| key),
            };
            if let Some(n) = key.as_ref().and_then(|k| memo.get(k)) {
                return *n;
            }
            let mut n = 1;
            let mut visit = |c: &Type| {
                if n <= cap {
                    n += size(c, cap - n, memo);
                }
            };
            match t {
                Type::Fn(ft) => ft.for_each_part(&mut |c, _| visit(c)),
                t => t.for_each_child(&mut visit),
            }
            if n <= cap
                && let Some(k) = key
            {
                memo.insert(k, n);
            }
            n
        })
    }
    let mut memo = LPooled::take();
    size(t, cap as u64, &mut memo) > cap as u64
}

/// State of one top-level resolve. Memo entries are keyed by the
/// expansion plus the `Seen` keys it consulted, so an entry is valid on
/// any path containing all its dependencies and a resolve costs the
/// number of distinct types, not paths.
struct ResolveCx {
    /// Ref expansions by [`expand_key_fp`] of their key.
    memo: RefCell<LPooled<ahash::AHashMap<u64, SmallVec<[MemoEntry; 1]>>>>,
    /// Shared composite subtrees between expansions.
    nodes: RefCell<LPooled<ahash::AHashMap<crate::typ::NormKey, NodeEntry>>>,
    /// `Seen` keys the current frame consulted; a frame's own key is
    /// not a dependency on its caller's path.
    consulted: RefCell<LPooled<Vec<TypeRef>>>,
    /// The current frame's result was truncated and must not be memoized.
    poisoned: Cell<bool>,
    expansions: Cell<u32>,
    budget: u32,
    /// Output nodes so far, tree-wise; a memo hit charges its recorded
    /// size. Past `size_cap` the resolve stops.
    unfolded: Cell<u32>,
    size_cap: u32,
}

impl Default for ResolveCx {
    fn default() -> Self {
        Self {
            memo: Default::default(),
            nodes: Default::default(),
            consulted: Default::default(),
            poisoned: Default::default(),
            expansions: Default::default(),
            budget: 65_536,
            unfolded: Default::default(),
            size_cap: u32::MAX,
        }
    }
}

struct NodeEntry {
    /// The key's owner: the key is its address.
    _key: Type,
    deps: Vec<(u64, TypeRef)>,
    resolved: Option<Type>,
    size: u32,
}

struct FrameResult {
    deps: Vec<(u64, TypeRef)>,
    poisoned: bool,
    size: u32,
}

struct MemoEntry {
    key: Arc<TypeRef>,
    deps: Vec<(u64, TypeRef)>,
    /// `None` when nothing beneath the ref resolved: the ref stays opaque.
    resolved: Option<Type>,
    /// Tree-wise node count of `resolved`, charged on every hit.
    size: u32,
}

impl std::fmt::Debug for MemoEntry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "MemoEntry(size {})", self.size)
    }
}

fn key_closed(key: &TypeRef) -> bool {
    key.params.iter().all(|t| t.tvar_free())
}

impl ResolveCx {
    /// Record that the current frame's result depends on `key` being on
    /// the path.
    fn consult(&self, key: &TypeRef) {
        let mut cur = self.consulted.borrow_mut();
        if !cur.contains(key) {
            cur.push(key.clone());
        }
    }

    fn lookup(&self, key: &TypeRef, seen: Option<&Seen>) -> Option<Option<Type>> {
        let memo = self.memo.borrow();
        let e = memo.get(&expand_key_fp(key))?.iter().find(|e| {
            &*e.key == key
                && e.deps.iter().all(|(fp, d)| Seen::find_fp(seen, *fp, d).is_some())
        })?;
        self.count_nodes(e.size);
        for (_, d) in &e.deps {
            self.consult(d);
        }
        Some(e.resolved.clone())
    }

    /// Count `n` output nodes against the size cap; `false` once past it.
    fn count_nodes(&self, n: u32) -> bool {
        let total = self.unfolded.get().saturating_add(n);
        self.unfolded.set(total);
        if total > self.size_cap {
            self.poisoned.set(true);
            return false;
        }
        true
    }

    /// `Some(result)` when `key` has an entry whose dependencies are all
    /// on the current path; charges its size and re-registers them.
    fn node_lookup(
        &self,
        key: &crate::typ::NormKey,
        seen: Option<&Seen>,
    ) -> Option<Option<Type>> {
        let nodes = self.nodes.borrow();
        let e = nodes.get(key)?;
        if !e.deps.iter().all(|(fp, d)| Seen::find_fp(seen, *fp, d).is_some()) {
            return None;
        }
        self.count_nodes(e.size);
        for (_, d) in &e.deps {
            self.consult(d);
        }
        Some(e.resolved.clone())
    }

    /// Run `f` as a tracked frame, returning what it consulted, whether
    /// it was truncated and its output size; merges both into the
    /// enclosing frame.
    fn node_frame<R>(&self, f: impl FnOnce(&Self) -> R) -> (R, FrameResult) {
        let saved_consulted =
            std::mem::replace(&mut *self.consulted.borrow_mut(), LPooled::take());
        let saved_poisoned = self.poisoned.replace(false);
        let before = self.unfolded.get();
        let r = f(self);
        let mut delta =
            std::mem::replace(&mut *self.consulted.borrow_mut(), saved_consulted);
        let poisoned = self.poisoned.get();
        let size = self.unfolded.get() - before;
        let deps: Vec<(u64, TypeRef)> =
            delta.iter().map(|k| (expand_key_fp(k), k.clone())).collect();
        for k in delta.drain(..) {
            let mut cur = self.consulted.borrow_mut();
            if !cur.contains(&k) {
                cur.push(k);
            }
        }
        self.poisoned.set(saved_poisoned || poisoned);
        (r, FrameResult { deps, poisoned, size })
    }

    /// Charge one expansion against the budget; `false` when exhausted.
    fn charge(&self) -> bool {
        let n = self.expansions.get();
        if n >= self.budget {
            if n == self.budget {
                self.expansions.set(n + 1);
                log::warn!(
                    "expand_refs: expansion budget ({}) exhausted — \
                     leaving the remainder opaque",
                    self.budget
                );
            }
            self.poisoned.set(true);
            return false;
        }
        self.expansions.set(n + 1);
        true
    }

    /// Run `f` as one expansion frame; memoize an untruncated result
    /// against the keys it consulted when `memoize`.
    fn expand(
        &self,
        key: Arc<TypeRef>,
        memoize: bool,
        f: impl FnOnce() -> Option<Type>,
    ) -> Option<Type> {
        let saved_consulted =
            std::mem::replace(&mut *self.consulted.borrow_mut(), LPooled::take());
        let saved_poisoned = self.poisoned.replace(false);
        let unfolded_before = self.unfolded.get();
        let resolved = f();
        let mut deps =
            std::mem::replace(&mut *self.consulted.borrow_mut(), saved_consulted);
        let poisoned = self.poisoned.get();
        deps.retain(|k| *k != *key);
        // An unbound tvar param compares equal to any other unbound
        // cell, so an open key would hand one site's cells to another.
        if memoize && !poisoned && key_closed(&key) {
            let fp = expand_key_fp(&key);
            let entry = MemoEntry {
                key,
                deps: deps.iter().map(|k| (expand_key_fp(k), k.clone())).collect(),
                resolved: resolved.clone(),
                size: self.unfolded.get() - unfolded_before,
            };
            self.memo.borrow_mut().entry(fp).or_default().push(entry);
        }
        for k in deps.drain(..) {
            let mut cur = self.consulted.borrow_mut();
            if !cur.contains(&k) {
                cur.push(k);
            }
        }
        self.poisoned.set(saved_poisoned || poisoned);
        resolved
    }
}

/// `None` when nothing beneath resolved (the caller keeps the original)
/// or the resolve was truncated (`cx.poisoned` set).
fn expand_ref_d<'a>(
    typ: &Type,
    env: &Env,
    seen: Option<&'a Seen<'a>>,
    cx: &ResolveCx,
) -> Option<Type> {
    crate::stack::ensure_sufficient(|| expand_ref_d_inner(typ, env, seen, cx))
}

fn expand_ref_d_inner<'a>(
    typ: &Type,
    env: &Env,
    seen: Option<&'a Seen<'a>>,
    cx: &ResolveCx,
) -> Option<Type> {
    // Bounds distinct expansions on one path, which is what stops
    // non-regular recursion.
    if Seen::len(seen) > kernel_abi::MAX_FREEZE_EXPANSIONS {
        cx.poisoned.set(true);
        return None;
    }
    if !cx.count_nodes(1) {
        return None;
    }
    let nkey = crate::typ::norm_key(typ);
    if let Some(k) = &nkey
        && let Some(hit) = cx.node_lookup(k, seen)
    {
        return hit;
    }
    let (r, frame) = cx.node_frame(|cx| expand_ref_node(typ, env, seen, cx));
    if let Some(k) = nkey
        && !frame.poisoned
    {
        cx.nodes.borrow_mut().insert(
            k,
            NodeEntry {
                _key: typ.clone(),
                deps: frame.deps,
                resolved: r.clone(),
                size: frame.size,
            },
        );
    }
    r
}

fn expand_ref_node<'a>(
    typ: &Type,
    env: &Env,
    seen: Option<&'a Seen<'a>>,
    cx: &ResolveCx,
) -> Option<Type> {
    match typ {
        // The inner type is cloned out so the tvar's read guard is not
        // held across `lookup_ref`'s lock acquisitions (a deadlock under
        // concurrent compiles). A deref is not an expansion: `seen`
        // passes through.
        Type::TVar(_) => match typ.deref_cloned() {
            Some(t) => Some(expand_ref_d(&t, env, seen, cx).unwrap_or(t)),
            None => None,
        },
        Type::Ref(tr) => {
            if Seen::find(seen, tr).is_some() {
                cx.consult(tr);
                return None;
            }
            if let Some(t) = cx.lookup(tr, seen) {
                return t;
            }
            if !cx.charge() {
                return None;
            }
            match typ.lookup_ref(env) {
                Ok(resolved) => cx.expand(tr.clone(), true, || {
                    let node = Seen::push(seen, tr.clone());
                    Some(
                        expand_ref_d(&resolved, env, Some(&node), cx).unwrap_or(resolved),
                    )
                }),
                _ => None,
            }
        }
        Type::Abstract { .. } => None,
        t => t.cow_children(&mut |c| expand_ref_d(c, env, seen, cx)),
    }
}

/// The catch-coverage fingerprint of a lambda instance's body: each
/// `?`/`$` site's resolved handler bind in emission order (`u64::MAX`
/// = handler-less), inline collection callbacks included. Part of the
/// kernel cache key, because a kernel bakes its handler ids into its
/// deliver sites.
pub(crate) type QopCoverage = SmallVec<[u64; 4]>;

/// The lambda-resolution fingerprint of a lambda instance's body: per
/// statically-resolved call site it emits, the callee's `LambdaId` and the
/// identity of each fn-typed argument forwarded (`u64::MAX` when
/// unresolvable). Part of the kernel cache key, because a kernel bakes
/// those resolutions as CLIF calls.
pub(crate) type FnResolutions = SmallVec<[u64; 4]>;

fn body_fingerprint<R: Rt, E: UserEvent>(
    body: &Node<R, E>,
    ec: &CompileCtx<R, E>,
) -> (QopCoverage, FnResolutions) {
    let mut cov = QopCoverage::new();
    let mut res = FnResolutions::new();
    fusion::for_each_emitted_node(body, &mut |n| match n.view() {
        NodeView::Qop(q) if q.raises() => match q.handler().map(|h| h.id()) {
            Some((bind, top)) => {
                cov.push(bind.inner());
                cov.push(top.inner());
            }
            None => cov.push(u64::MAX),
        },
        NodeView::CallSite(cs) => {
            let Some(crate::ApplyView::Lambda(callee)) = cs.resolved_apply() else {
                return;
            };
            res.push(callee.id().inner());
            each_arg_node(cs, &mut |a: &Node<R, E>| {
                if !is_fn_shaped(a.typ(), &ec.env) {
                    return;
                }
                let id = match a.view() {
                    NodeView::Lambda(l) => l.lambda_id::<R, E>().map(|id| id.inner()),
                    NodeView::Ref(r) => ec
                        .bind_to_lambda
                        .get(&r.id)
                        .and_then(|v| {
                            v.downcast_ref::<crate::LambdaDef<R, E>>()
                                .map(|d| d.id.inner())
                        })
                        // A forwarded fn formal's `bind_to_lambda` entry
                        // is gone by fingerprint time; the snapshot keeps it.
                        .or_else(|| {
                            ec.fn_forward_resolutions.get(&r.id).map(|id| id.inner())
                        }),
                    _ => None,
                };
                res.push(id.unwrap_or(u64::MAX));
            });
        }
        _ => (),
    });
    (cov, res)
}

/// Visit a call site's arg nodes in source order.
fn each_arg_node<R: Rt, E: UserEvent>(
    cs: &CallSite<R, E>,
    f: &mut impl FnMut(&Node<R, E>),
) {
    let mut pos = 0usize;
    for (label, _) in cs.spec_args().clone().iter() {
        let n = match label {
            Some(name) => cs.arg_named(name),
            None => {
                let n = cs.arg_positional(pos);
                pos += 1;
                n
            }
        };
        if let Some(n) = n {
            f(n)
        }
    }
}

/// Is this an fn type once tvars deref and aliases expand?
fn is_fn_shaped(t: &Type, env: &Env) -> bool {
    match t.deref_cloned() {
        Some(Type::Fn(_)) => true,
        Some(r @ Type::Ref(_)) => {
            expand_refs(&r, env).with_deref(|t| matches!(t, Some(Type::Fn(_))))
        }
        _ => false,
    }
}

/// Per formal: is it passed unchanged (a `Ref` to its own binding) by
/// every self-call the body emits? Such a formal needs no rebind slot.
/// Vacuously true with no self-calls; a destructured formal is never
/// invariant.
pub(crate) fn invariant_formals<R: Rt, E: UserEvent>(
    g: &GXLambda<R, E>,
    self_bind: Option<BindId>,
) -> SmallVec<[bool; 8]> {
    let ids: SmallVec<[Option<BindId>; 8]> =
        g.args().iter().map(|p| p.single_bind_id()).collect();
    let mut inv: SmallVec<[bool; 8]> = ids.iter().map(|id| id.is_some()).collect();
    let Some(sb) = self_bind else { return inv };
    fusion::for_each_emitted_node(g.body(), &mut |n| {
        let NodeView::CallSite(cs) = n.view() else { return };
        if !matches!(cs.fnode().view(), NodeView::Ref(r) if r.id == sb) {
            return;
        }
        for i in 0..inv.len() {
            if !inv[i] {
                continue;
            }
            let arg = formal_arg(cs, g.typ(), i);
            let keep = matches!(
                (ids[i], arg),
                (Some(fid), Some(a))
                    if matches!(a.view(), NodeView::Ref(r) if r.id == fid)
            );
            if !keep {
                inv[i] = false;
            }
        }
    });
    inv
}

/// The argument a call passes formal `i` of `ftype`: a labeled one by
/// its name, a positional one by its place among the positionals.
pub(crate) fn formal_arg<'a, R: Rt, E: UserEvent>(
    cs: &'a CallSite<R, E>,
    ftype: &FnType,
    i: usize,
) -> Option<&'a Node<R, E>> {
    match &ftype.args.get(i)?.kind {
        FnArgKind::Labeled { name, .. } => cs.arg_named(name),
        FnArgKind::Positional { .. } => {
            let positional =
                |a: &&FnArgType| matches!(a.kind, FnArgKind::Positional { .. });
            cs.arg_positional(ftype.args[..i].iter().filter(positional).count())
        }
    }
}

/// `g`'s own ids for the formal parameters of `kernel`, which may have
/// been built for another instance of `g`'s definition: each pair the
/// kernel's id and `g`'s where they differ.
pub(crate) fn rebound_formals<R: Rt, E: UserEvent>(
    g: &GXLambda<R, E>,
    kernel: &KernelSig,
) -> SmallVec<[(BindId, BindId); 4]> {
    let formals =
        (0..g.typ().args.len()).filter(|i| !kernel.skipped_args.contains(&(*i as u32)));
    formals
        .zip(kernel.params.iter())
        .filter_map(|(i, p)| {
            let (old, new) = (p.bind_id?, g.args().get(i)?.single_bind_id()?);
            (old != new).then_some((old, new))
        })
        .collect()
}

/// Why a lambda has no kernel; its call sites node-walk.
pub(crate) type Refusal = CompactString;

/// Build, or fetch from the per-`ExecCtx` cache, the kernel for the
/// lambda `g` at the call site's resolved type. Signature derivation
/// only: the body is validated by the compile attempt. `kernel_name`
/// labels a fresh kernel's symbols.
pub(crate) fn build_lambda_kernel<R: Rt, E: UserEvent>(
    g: &GXLambda<R, E>,
    site_ftype: &FnType,
    kernel_name: &ArcStr,
    ec: &CompileCtx<R, E>,
    reads: &mut ReadsMemo,
) -> Result<triomphe::Arc<CachedKernel>, Refusal> {
    // The key is the site's type, not `g.typ()`: the instance shares
    // tvar cells with the def, so `g.typ()` reports whichever
    // monomorphization unified first.
    let resolved_typ = triomphe::Arc::new(site_ftype.resolve_tvars());
    let (coverage, fn_resolutions) = body_fingerprint(g.body(), ec);
    let key = (g.id(), resolved_typ, coverage, fn_resolutions);
    if let Some(cached) = ec.fusion.kernel(&key) {
        return Ok(cached);
    }
    let mut discovery = BuiltinCallDiscovery::default();
    walk_node_for_builtin_calls(g.body(), ec, &mut discovery)
        .map_err(|b| format_compact!("its body: {}", b.reason))?;
    instance_agrees(g, &key.1, ec)?;
    let self_bind = g.self_bind();
    let mut formals = formal_slots(g, self_bind, ec)?;
    let mut captures = capture_slots(g, self_bind, ec, reads)?;
    let return_type =
        kernel_abi::freeze_for_abi_normalized(&expand_refs(&g.typ().rtype, &ec.env))
            .ok_or("its return type has no kernel encoding")?;
    // A unit-typed call has no value to return; bare Null should have
    // widened.
    if matches!(abi_kind(&return_type), Some(AbiKind::Unit | AbiKind::Null)) {
        return Err("it returns no value".into());
    }
    let self_call = self_bind;
    // Defense in depth behind static instance checking: a self-call
    // feeding a formal a differently-shaped value would marshal it under
    // the wrong ABI.
    if let Some(sb) = self_call
        && !self_calls_abi_consistent(g, sb, &formals.by_position, ec)
    {
        return Err("a self-call passes a formal a value of another shape".into());
    }
    let mut arg_types = std::mem::take(&mut formals.arg_types);
    let mut params = std::mem::take(&mut formals.params);
    let mut capture_slots = Vec::with_capacity(captures.len());
    for (cap, kind) in captures.drain(..) {
        params.push(KernelParam {
            name: cap.name.clone(),
            kind,
            bind_id: Some(cap.bind_id),
        });
        arg_types.push(cap.typ.clone());
        capture_slots.push(cap);
    }
    let mut sig = fusion::sig_from_params(kernel_name.clone(), params, return_type);
    sig.has_tail_loop = g.tail_loop();
    sig.skipped_args = formals.skipped;
    sig.tail_invariant = formals.tail_invariant;
    if crate::dbgenv::graphix_dbg_kernels() {
        crate::format_with_flags(PrintFlag::DerefTVars, || {
            eprintln!(
                "KERNEL BUILT {kernel_name}: ret={} kind={:?}",
                sig.return_type,
                abi_kind(&sig.return_type)
            );
            Ok::<_, anyhow::Error>(())
        })
        .unwrap();
    }
    let cached = triomphe::Arc::new(CachedKernel {
        kernel: triomphe::Arc::new(sig),
        arg_types,
        captures: capture_slots,
        self_call,
        apply_sites: discovery.apply_sites,
    });
    ec.fusion.cache_kernel(key, cached.clone());
    Ok(cached)
}

/// Emission reads the instance's node types, so an instance that
/// disagrees with the site would emit a kernel whose CLIF types
/// mismatch the call. Constraint lists differ benignly; compare only
/// args, vargs and return.
fn instance_agrees<R: Rt, E: UserEvent>(
    g: &GXLambda<R, E>,
    site: &FnType,
    ec: &CompileCtx<R, E>,
) -> Result<(), Refusal> {
    let gt = g.typ().resolve_tvars();
    let args_agree = gt.args.len() == site.args.len()
        && gt.args.iter().zip(site.args.iter()).all(|(a, b)| {
            // An fn-typed arg is never a value slot and is keyed by
            // `FnResolutions`; its `throws` differs benignly here.
            let (af, bf) = (is_fn_shaped(&a.typ, &ec.env), is_fn_shaped(&b.typ, &ec.env));
            if af || bf { af && bf } else { a.typ == b.typ }
        });
    if !args_agree || gt.vargs != site.vargs || gt.rtype != site.rtype {
        return Err(format_compact!(
            "the instance's type {gt} disagrees with the site's {site}"
        ));
    }
    Ok(())
}

/// A lambda's formals as kernel params.
struct FormalSlots {
    params: Vec<KernelParam>,
    /// Each param's frozen type.
    arg_types: Vec<Type>,
    /// `(formal position, frozen slot type)` per param.
    by_position: SmallVec<[(usize, Type); 8]>,
    /// Formal positions with no slot (see `KernelSig::skipped_args`).
    skipped: Vec<u32>,
    /// Formal positions every self-call forwards unchanged.
    tail_invariant: Vec<u32>,
}

/// Slots carry the formal's BindId because a declared fn type's
/// parameter names may differ from the lambda literal's own; body
/// `Ref`s resolve by id first.
fn formal_slots<R: Rt, E: UserEvent>(
    g: &GXLambda<R, E>,
    self_bind: Option<BindId>,
    ec: &CompileCtx<R, E>,
) -> Result<FormalSlots, Refusal> {
    let inv = invariant_formals(g, self_bind);
    let mut out = FormalSlots {
        params: Vec::new(),
        arg_types: Vec::new(),
        by_position: SmallVec::new(),
        skipped: Vec::new(),
        tail_invariant: Vec::new(),
    };
    for (i, fa) in g.typ().args.iter().enumerate() {
        let name = match &fa.kind {
            FnArgKind::Positional { name: Some(n) } => n.clone(),
            FnArgKind::Labeled { name, .. } => name.clone(),
            FnArgKind::Positional { name: None } => {
                return Err("a formal has no name".into());
            }
        };
        let arg_typ = expand_refs(&fa.typ, &ec.env);
        // An invariant fn-typed formal drops out of the signature: its
        // uses are statically-resolved calls, and a body that needs it
        // as a value fails to emit. A rebind slot cannot carry a lambda.
        if is_fn_shaped(&arg_typ, &ec.env) {
            if inv[i] {
                out.skipped.push(i as u32);
                continue;
            }
            return Err(format_compact!(
                "fn-typed formal `{name}` is not forwarded unchanged by its self-calls"
            ));
        }
        let no_slot = || format_compact!("formal `{name}` has no kernel encoding");
        let kt = kernel_abi::freeze_for_abi_normalized(&arg_typ).ok_or_else(no_slot)?;
        let kind = param_kind(&kt).ok_or_else(no_slot)?;
        let id = g.args().get(i).and_then(|p| p.single_bind_id());
        // A destructured formal refuses: a name-only slot would let a
        // same-named body leaf resolve to the whole composite.
        if id.is_none() {
            let mut has_ids = false;
            if let Some(p) = g.args().get(i) {
                p.ids(&mut |_| has_ids = true);
            }
            if has_ids {
                return Err(format_compact!("formal `{name}` is destructured"));
            }
        }
        out.params.push(KernelParam { name, kind, bind_id: id });
        out.arg_types.push(kt.clone());
        out.by_position.push((i, kt));
    }
    out.tail_invariant = inv
        .iter()
        .enumerate()
        .filter(|(i, v)| **v && !out.skipped.contains(&(*i as u32)))
        .map(|(i, _)| i as u32)
        .collect();
    Ok(out)
}

/// One discovery's [`external_reads`] of each lambda instance, and
/// whether each binding they meet holds a function.
#[derive(Default)]
pub(crate) struct ReadsMemo {
    reads: nohash::IntMap<LambdaInstanceId, Arc<[BindId]>>,
    fn_shaped: nohash::IntMap<BindId, bool>,
}

impl ReadsMemo {
    /// A function-valued binding is called, never captured.
    fn captured<R: Rt, E: UserEvent>(
        &mut self,
        id: BindId,
        ec: &CompileCtx<R, E>,
    ) -> bool {
        !*self.fn_shaped.entry(id).or_insert_with(|| {
            ec.env
                .by_id
                .get(&id)
                .is_some_and(|b| is_fn_shaped(&expand_refs(&b.typ, &ec.env), &ec.env))
        })
    }
}

/// What `g`'s body reads from outside `g` that is not a function, its
/// callees' bodies included: its own reads (a builtin callee's walked
/// whole) joined with each lambda callee's, each instance walked once
/// (`memo`).
fn external_reads<R: Rt, E: UserEvent>(
    g: &GXLambda<R, E>,
    ec: &CompileCtx<R, E>,
    memo: &mut ReadsMemo,
) -> Arc<[BindId]> {
    if let Some(r) = memo.reads.get(&g.instance_id()) {
        return r.clone();
    }
    let reads = crate::stack::ensure_sufficient(|| {
        let mut formals: LPooled<IntSet<BindId>> = LPooled::take();
        for pat in g.args() {
            pat.ids(&mut |id| {
                formals.insert(id);
            });
        }
        let mut refs = Refs::stopping_at_callees();
        g.body().refs(&mut refs);
        let callees = refs.take_callees();
        let mut by_id: LPooled<nohash::IntMap<LambdaInstanceId, &GXLambda<R, E>>> =
            LPooled::take();
        if !callees.is_empty() {
            fusion::for_each_node(g.body(), &mut |n| {
                if let NodeView::CallSite(cs) = n.view()
                    && let Some(ApplyView::Lambda(c)) = cs.resolved_apply()
                {
                    by_id.insert(c.instance_id(), c);
                }
            });
        }
        // a callee the node walk does not reach is read the long way
        let mut own: LPooled<Vec<BindId>> = LPooled::take();
        if callees.iter().any(|(id, _)| !by_id.contains_key(id)) {
            let mut refs = Refs::default();
            g.body().refs(&mut refs);
            refs.with_external_refs(|id| own.push(id));
            own.retain(|id| !formals.contains(id));
            own.retain(|id| memo.captured(*id, ec));
            return Arc::from_iter(own.drain(..));
        }
        refs.with_external_refs(|id| own.push(id));
        let mut reads: LPooled<IntSet<BindId>> = LPooled::take();
        for id in own.drain(..) {
            if !formals.contains(&id) && memo.captured(id, ec) {
                reads.insert(id);
            }
        }
        for (id, _) in callees.iter() {
            for b in external_reads(by_id[id], ec, memo).iter() {
                if !refs.bound.contains(b) && !formals.contains(b) {
                    reads.insert(*b);
                }
            }
        }
        Arc::from_iter(reads.drain())
    });
    memo.reads.insert(g.instance_id(), reads.clone());
    reads
}

/// The outer bindings a lambda's body reads, in BindId order, as kernel
/// params the caller forwards from its own env. A self-reference lowers
/// as a call and a statically-resolved fn capture as a call, never as a
/// value, so neither takes a slot.
fn capture_slots<R: Rt, E: UserEvent>(
    g: &GXLambda<R, E>,
    self_bind: Option<BindId>,
    ec: &CompileCtx<R, E>,
    memo: &mut ReadsMemo,
) -> Result<LPooled<Vec<(CaptureSlot, ParamKind)>>, Refusal> {
    let mut external: LPooled<Vec<BindId>> = external_reads(g, ec, memo)
        .iter()
        .copied()
        .filter(|id| Some(*id) != self_bind)
        .collect();
    external.sort_by_key(|id| id.inner());
    let mut out: LPooled<Vec<(CaptureSlot, ParamKind)>> = LPooled::take();
    for bind_id in external.drain(..) {
        let b = ec.env.by_id.get(&bind_id).ok_or("a capture has no binding")?;
        let typ = expand_refs(&b.typ, &ec.env);
        let no_slot = || format_compact!("capture `{}` has no kernel encoding", b.name);
        let kt = kernel_abi::freeze_for_abi_normalized(&typ).ok_or_else(no_slot)?;
        let kind = param_kind(&kt).ok_or_else(no_slot)?;
        let name = ArcStr::from(b.name.as_str());
        out.push((CaptureSlot { bind_id, name, typ: kt }, kind));
    }
    Ok(out)
}

/// What keeps the structural tail-loop predicate from holding shared by the JIT's native-loop
/// gate and the interpreter's `tail_loop` gate, so both backends loop
/// the same lambdas: `g` is self-recursive through `self_bind`, every
/// formal is positional, every loop-carried formal is a kernel-encodable
/// kind, and a self-call sits in tail position. Not sync-gated; the
/// interpreter's `analyze` adds that gate.
pub(crate) fn tail_loop_blocker<R: Rt, E: UserEvent>(
    g: &GXLambda<R, E>,
    self_bind: BindId,
    ec: &CompileCtx<R, E>,
) -> Option<NoLoop> {
    let mut refs = Refs::default();
    g.body().refs(&mut refs);
    let mut is_rec = false;
    refs.with_external_refs(|id| {
        if id == self_bind {
            is_rec = true;
        }
    });
    if !is_rec {
        return Some(NoLoop::Memory);
    }
    // The rebind maps the self-call's positional args 1:1 onto the formals.
    if g.typ().vargs.is_some() {
        return Some(NoLoop::Formals);
    }
    let inv = invariant_formals(g, Some(self_bind));
    for (i, fa) in g.typ().args.iter().enumerate() {
        if !matches!(fa.kind, FnArgKind::Positional { .. }) {
            return Some(NoLoop::Formals);
        }
        if inv[i] {
            continue;
        }
        let arg_typ = expand_refs(&fa.typ, &ec.env);
        match kernel_abi::freeze_for_abi_normalized(&arg_typ) {
            Some(kt) if param_kind(&kt).is_some() => (),
            _ => return Some(NoLoop::Formals),
        }
    }
    if !body_has_self_tail_call(g.body(), self_bind) {
        return Some(NoLoop::Memory);
    }
    let mut visiting: LPooled<Vec<LambdaInstanceId>> = LPooled::take();
    visiting.push(g.instance_id());
    site_memory(g.body(), Some(self_bind), &g.formal_ids(), &mut visiting)
}

/// The per-call-site memory a body holds that a native tail loop's
/// passes would share where the node-walk's activations each hold their
/// own: a collection loop's words, a non-tail self-call's block root, a
/// handler-ful `?` that does not fire with the body's `formals` (an
/// activation raises it at its birth, a pass never does), a callee whose
/// body holds any; `None` when it holds none. Such a body recurses
/// natively.
fn site_memory<R: Rt, E: UserEvent>(
    body: &Node<R, E>,
    self_bind: Option<BindId>,
    formals: &IntSet<BindId>,
    visiting: &mut LPooled<Vec<LambdaInstanceId>>,
) -> Option<NoLoop> {
    let is_self = |cs: &CallSite<R, E>| {
        self_bind
            .is_some_and(|sb| matches!(cs.fnode().view(), NodeView::Ref(r) if r.id == sb))
    };
    let mut tail_calls: LPooled<IntSet<ExprId>> = LPooled::take();
    fusion::for_each_tail_leaf(
        body,
        &mut |n| match n.view() {
            NodeView::CallSite(cs) if is_self(cs) => {
                tail_calls.insert(n.spec().id);
                true
            }
            _ => false,
        },
        &mut |_| (),
    );
    let mut held = None;
    fusion::for_each_emitted_node(body, &mut |n| {
        if held.is_some() {
            return;
        }
        held = match n.view() {
            NodeView::MapQ(_) | NodeView::FoldQ(_) => Some(NoLoop::Memory),
            NodeView::Qop(q)
                if q.handler().is_some()
                    && !crate::analysis::fires_with(&q.n, formals) =>
            {
                Some(NoLoop::Raise(n.spec().clone()))
            }
            NodeView::CallSite(cs) if is_self(cs) => {
                (!tail_calls.contains(&n.spec().id)).then_some(NoLoop::Memory)
            }
            NodeView::CallSite(cs) => match cs.resolved_apply() {
                // a cycle through another function roots blocks too
                Some(ApplyView::Lambda(g)) if visiting.contains(&g.instance_id()) => {
                    Some(NoLoop::Memory)
                }
                Some(ApplyView::Lambda(g)) => {
                    visiting.push(g.instance_id());
                    let held =
                        site_memory(g.body(), g.self_bind(), &g.formal_ids(), visiting);
                    visiting.pop();
                    held
                }
                _ => None,
            },
            _ => None,
        }
    });
    held
}

/// Does the body contain a self-call in tail position (as
/// [`fusion::for_each_tail_leaf`] defines it)? A tail call is emitted
/// only when this said so, so a kernel never emits one without its loop
/// head; a rejected tail call leaves a harmless vestigial head.
pub(crate) fn body_has_self_tail_call<R: Rt, E: UserEvent>(
    node: &Node<R, E>,
    self_bind: BindId,
) -> bool {
    fusion::for_each_tail_leaf(
        node,
        &mut |n| match n.view() {
            NodeView::CallSite(cs) => {
                matches!(cs.fnode().view(), NodeView::Ref(r) if r.id == self_bind)
            }
            _ => false,
        },
        &mut |_| (),
    )
}

/// Does every self-call the body emits feed each positional formal a
/// value whose frozen type fits that formal's slot type? Conservative:
/// an unfreezable arg or a self-call that does not map 1:1 onto the
/// formals counts as inconsistent.
fn self_calls_abi_consistent<R: Rt, E: UserEvent>(
    g: &GXLambda<R, E>,
    self_bind: BindId,
    formal_slot_types_by_position: &[(usize, Type)],
    ec: &CompileCtx<R, E>,
) -> bool {
    let mut ok = true;
    let ftype = g.typ();
    fusion::for_each_emitted_node(g.body(), &mut |n| {
        if !ok {
            return;
        }
        let NodeView::CallSite(cs) = n.view() else { return };
        if !matches!(cs.fnode().view(), NodeView::Ref(r) if r.id == self_bind) {
            return;
        }
        for (i, formal_kt) in formal_slot_types_by_position.iter() {
            let Some(arg) = formal_arg(cs, ftype, *i) else {
                ok = false;
                return;
            };
            let Some(arg_kt) =
                kernel_abi::freeze_for_abi_normalized(&expand_refs(arg.typ(), &ec.env))
            else {
                ok = false;
                return;
            };
            // A probe: fusion must not bind a cell the typechecker sees.
            if !formal_kt
                .contains_with_flags(BitFlags::empty(), &ec.env, &arg_kt)
                .unwrap_or(false)
            {
                ok = false;
                return;
            }
        }
    });
    ok
}

/// The kernel param shape of a frozen value type; `None` for `Unit`,
/// bare `Null` (widened to a nullable at construction), fn types, and a
/// type whose top level is not concrete (an opaque recursive leaf).
pub(crate) fn param_kind(t: &Type) -> Option<ParamKind> {
    Some(match abi_kind(t)? {
        AbiKind::Scalar(p) => ParamKind::Scalar(p),
        AbiKind::Array => ParamKind::Array { elem: kernel_abi::array_elem(t)?.clone() },
        AbiKind::Tuple => {
            ParamKind::Tuple { elems: kernel_abi::tuple_slots(t)?.to_vec() }
        }
        AbiKind::Struct => ParamKind::Struct {
            fields: kernel_abi::struct_fields(t)?
                .iter()
                .map(|(n, t, _)| (n.clone(), t.clone()))
                .collect(),
        },
        AbiKind::Variant => ParamKind::Variant { cases: kernel_abi::variant_cases(t)? },
        AbiKind::Nullable => ParamKind::Nullable { elem: kernel_abi::nullable_inner(t)? },
        AbiKind::String => ParamKind::String,
        AbiKind::Value => ParamKind::Value { typ: t.clone() },
        AbiKind::Unit | AbiKind::Null => return None,
    })
}

/// A marshallable call argument shape: every fusable shape but `Unit`.
fn is_call_arg_supported(t: &Type) -> bool {
    match abi_kind(t) {
        Some(
            AbiKind::Scalar(_)
            | AbiKind::Array
            | AbiKind::Tuple
            | AbiKind::Struct
            | AbiKind::Variant
            | AbiKind::Nullable
            | AbiKind::Value
            | AbiKind::String
            | AbiKind::Null,
        ) => true,
        Some(AbiKind::Unit) | None => false,
    }
}

/// True if `t` derefs to the single-bit `bytes` primitive.
pub(crate) fn is_bytes(t: &Type) -> bool {
    t.with_deref(|r| match r {
        Some(Type::Primitive(p)) => {
            p.contains(netidx_value::Typ::Bytes) && p.iter().count() == 1
        }
        _ => false,
    })
}

/// True if `t` derefs to the single-bit `Map` shape.
pub(crate) fn is_map(t: &Type) -> bool {
    t.with_deref(|r| matches!(r, Some(Type::Map { .. })))
}

#[cfg(test)]
mod tests {
    use super::*;
    use netidx_value::Typ;

    #[test]
    fn variant_unfolding_is_bounded() {
        let mut typ = Type::Primitive(Typ::I64.into());
        for level in 1..=40 {
            typ = Type::Variant(
                arcstr::literal!("A"),
                Arc::from_iter([typ.clone(), typ]),
                Default::default(),
            );
            if level <= 10 {
                let size = (1 << (level + 1)) - 1;
                assert!(!unfolds_past(&typ, size));
                assert!(unfolds_past(&typ, size - 1));
            }
        }
        assert!(unfolds_past(&typ, FUSION_SIZE_CAP));
        assert!(unfolds_past(&typ, u32::MAX));
    }

    /// A refusal renders a type that shares its parts in time bounded by
    /// its prefix, not 2^depth.
    #[test]
    fn shared_type_reason_is_bounded() {
        let mut variant = Type::Primitive(Typ::I64.into());
        let mut tuple = variant.clone();
        for _ in 1..=60 {
            variant = Type::Variant(
                arcstr::literal!("A"),
                Arc::from_iter([variant.clone(), variant]),
                Default::default(),
            );
            tuple = Type::Tuple(Arc::from_iter([tuple.clone(), tuple]));
        }
        for typ in [&variant, &tuple] {
            let s = format_compact!("{}", Brief(typ));
            assert!(s.len() <= Brief::BYTES + '…'.len_utf8() && s.ends_with('…'), "{s}");
        }
        let small = Type::Primitive(Typ::I64.into());
        assert_eq!(format_compact!("{}", Brief(&small)), "i64");
    }
}
