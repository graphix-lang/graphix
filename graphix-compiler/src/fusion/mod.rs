//! Fusion: identify sync (pure) subtrees of the compiled node graph
//! and JIT-compile them to native kernels.
//!
//! The architecture is distributed (`design/distributed_jit.md`):
//! body code generation is `Update::emit_clif` / `Apply::emit_clif`
//! trait methods living with each node, and the fusion phase is
//! `Update::fuse` recursion driven by [`fuse`]. This module supplies
//! the shared mechanics those impls call:
//!
//! - [`try_fuse`] — the whole-subtree compile attempt: region-input
//!   collection → [`sig_from_inputs`] → builtin/lambda call-site
//!   discovery → compile under the jit lock (`emit_clif` recursion
//!   from the root) → [`builder::FusedKernel`] + feeders. "Is it
//!   fusable" IS the compile attempt.
//! - [`fuse`] — the uniform child-visit protocol (try_fuse,
//!   else recurse, swap in any replacement).
//! - [`lowering`] — discovery, signature derivation (lambda callees,
//!   per-slot HOF callbacks, body splits), abstract-type resolution.
//! - [`builder`] — the runtime [`builder::FusedKernel`] carrier.

pub mod builder;
pub mod emit;
pub mod emit_helpers;
pub mod intern;
pub mod kernel;
pub mod kernel_abi;
pub mod lowering;

// The runtime kernel carrier is the one piece of fusion external code
// constructs directly; everything else is reached through the named
// submodules.
pub use builder::FusedKernel;

use crate::{
    ApplyView, BindId, ExecCtx, LambdaId, Node, NodeView, Refs, Rt, Scope, Update,
    UserEvent,
    effects::EffectKind,
    env::Env,
    expr::{ExprId, ExprKind},
    fusion::{
        kernel_abi::{KernelSig, freeze_for_abi_normalized},
        lowering::{RegionInputKind, resolve_abstract},
    },
    node::genn,
    typ::{FnType, Type},
};

/// Compile-time fusion outcome counters, accumulated on
/// [`FusionCtx::stats`] by every `compile()` the context
/// runs. Per-context (the compiler supports many instances per
/// process and tests run in parallel) — unlike the runtime-side
/// per-thread `emit_helpers` invocation counters, which can't
/// give per-region failure reasons and are wrong for a per-program
/// assertion on a multi-thread runtime.
///
/// Exists because the direct JIT path degrades silently: a region
/// that fails to compile just node-walks and produces the correct
/// value, so value-agreement tests cannot distinguish "fused
/// correctly" from "never fused". These counters make "did it
/// actually fuse, and if not why" a queryable fact.
#[derive(Debug, Clone, Default)]
pub struct FusionStats {
    /// `try_fuse` attempts that passed the cheap gates (identity,
    /// return-type) and reached the compile attempt.
    pub attempted: usize,
    /// Regions that compiled + spliced (direct path), or classic-path
    /// splices (for old-vs-new coverage comparison at the flip).
    pub fused: usize,
    /// Per-failure (region root ExprId, compile error) — the blocker
    /// profile. Compile-time only; bounded by program size.
    pub failed: Vec<(ExprId, compact_str::CompactString)>,
    /// Region-root ExprIds that successfully fused. Lets a consumer
    /// (e.g. the `#[native]` reporter) tell a STRUCTURAL `failed` entry —
    /// a `let`/block whose region attempt failed but whose VALUE fused in
    /// a sub-region — from a REAL blocker (a residue node with nothing
    /// fused beneath it). Compile-time only; bounded by program size.
    pub fused_ids: Vec<ExprId>,
}

/// Per-[`ExecCtx`] state owned by the fusion subsystem, grouped here
/// (rather than as loose `ExecCtx` fields) because it's all fusion's
/// own concern. Non-generic — none of these types depend on `R`/`E` —
/// so it's a plain `ctx.fusion` field. Reached as `ctx.fusion.<x>`.
pub struct FusionCtx {
    /// Per-context JIT state — cranelift module + cross-kernel-call
    /// cache. Each `ExecCtx` gets its own isolated JIT target (no
    /// shared global module, no races across concurrent in-process
    /// runtimes). The `parking_lot::Mutex` is interior mutability only
    /// (not shared ownership) — it satisfies the `Sync` bound graphix-rt
    /// puts on `ExecCtx` (cranelift's `JITModule` holds a `RefCell` so
    /// isn't auto-`Sync`). Access via `ctx.fusion.jit.lock()`. JIT ops
    /// are compile-time only, so the lock cost is negligible. Kernels
    /// compile into this module via
    /// [`emit::compile_kernel_with_callees_direct`] (parent + callees
    /// declared/defined together → direct CLIF cross-kernel calls).
    pub jit: parking_lot::Mutex<emit::Jit>,
    /// On-demand monomorphized lambda-kernel cache, keyed by
    /// `(LambdaId, Arc<FnType>)`. Populated by `build_lambda_kernel`:
    /// derive the signature once, reuse it for every later call to the
    /// same (lambda definition, monomorphization). The cached
    /// `Arc<KernelSig>` IS the compiled-callable handle — the JIT's
    /// `by_kernel` cache keys on its pointer identity.
    pub kernels: parking_lot::Mutex<
        std::collections::BTreeMap<
            (LambdaId, std::sync::Arc<FnType>),
            lowering::CachedKernel,
        >,
    >,
    /// Lambdas whose kernel build is CURRENTLY on the stack —
    /// `build_lambda_kernel`'s re-entrancy guard. Mutual recursion
    /// (f's body builds g, whose body re-enters f before f's cache
    /// entry lands) would otherwise recurse the build forever; the
    /// guard refuses the inner build so the chain de-fuses to the
    /// node-walk instead of hanging the compiler. `Arc` so a drop-guard
    /// can hold the set without borrowing the `ExecCtx`.
    pub(crate) building: triomphe::Arc<parking_lot::Mutex<nohash::IntSet<u64>>>,
    /// Lambdas whose BODY is CURRENTLY being driven through `typecheck1`
    /// by `CallSite::resolve_static` (#203 nested-call resolution). A call
    /// inside a body to a lambda already in this set is a self- or
    /// mutually-recursive back-edge: `resolve_static` leaves it
    /// `DynamicUnbound` (the `self_call` emit mechanism handles a self-call;
    /// a mutual back-edge de-fuses cleanly) instead of recursing the drive
    /// forever. `Arc` so the insert/remove can straddle the
    /// `apply.typecheck1(ctx, …)` call without borrowing the `ExecCtx`.
    pub(crate) resolving: triomphe::Arc<parking_lot::Mutex<nohash::IntSet<u64>>>,
    /// Whether fusion is enabled for the current compile. Set by
    /// [`crate::compile`] from `!flags.contains(CFlag::FusionDisabled)`.
    /// The in-context mirror of the flag, for the per-slot HOF fusion
    /// path (`fuse_callsite`, `design/impure_hof_fusion.md`) — which
    /// runs from a HOF builtin's `typecheck1` callback hook and from
    /// `MapQ::update`, neither of which receives the compile flags.
    /// Gating it here (not just the compile-time `fuse()` phase) is what
    /// makes `FusionDisabled` a TRUE node-walk. Defaults `true`.
    pub enabled: bool,
    /// Compile-time fusion outcome counters, accumulated across every
    /// `compile()` this context runs. See [`FusionStats`].
    pub stats: FusionStats,
    /// Fusion-time registry mapping each abstract type's `AbstractId` to
    /// its concrete implementation type. Written by `check_sig` during
    /// typecheck; read by the fusion classifiers
    /// ([`kernel_abi::freeze_for_abi`] / `abi_kind` / `resolve_abstract`)
    /// to peek through an abstract type's opacity to its wire shape.
    /// Owned per-context (not a process-global) so it drops with the
    /// `ExecCtx` — `AbstractId`s are minted fresh per compile, so a
    /// global would leak.
    pub abstract_registry: kernel_abi::AbstractRegistry,
    /// The TOP expression id of the compile currently running — set by
    /// [`crate::compile`] before the fusion phase. `try_fuse` builds its
    /// feeder Refs with this id: `Rt::ref_var`/`unref_var` are keyed
    /// `(BindId, top_id)`, and the runtime wakes a top expression on
    /// `set_var` only while its ref count is nonzero. A feeder
    /// registered under the REGION's interior ExprId would strand the
    /// real top expression at count zero once the spliced original's
    /// Refs unref — a fused region fed by a `<-`-written variable would
    /// then never see updates past the first cycle.
    pub(crate) top_id: Option<ExprId>,
    /// Declared facts of each registered builtin, keyed by name.
    /// Populated by `register_builtin` from `T::EFFECT`/`T::STATELESS`.
    /// The effect is read by fusion's effect inference to decide
    /// whether a builtin call site can be absorbed into a sync kernel;
    /// the stateless bit by the transient-recursion gate
    /// (`node::callsite::transient_body_ok`). Builtins absent from this
    /// map are treated as `Async` + stateful (the conservative
    /// defaults), always correct.
    pub builtin_facts: ahash::AHashMap<&'static str, crate::effects::BuiltinFacts>,
}

impl FusionCtx {
    pub fn new() -> anyhow::Result<Self> {
        Ok(Self {
            jit: parking_lot::Mutex::new(emit::Jit::new()?),
            kernels: parking_lot::Mutex::new(std::collections::BTreeMap::new()),
            building: triomphe::Arc::new(parking_lot::Mutex::new(
                nohash::IntSet::default(),
            )),
            resolving: triomphe::Arc::new(parking_lot::Mutex::new(
                nohash::IntSet::default(),
            )),
            enabled: true,
            stats: FusionStats::default(),
            abstract_registry: kernel_abi::AbstractRegistry::default(),
            top_id: None,
            builtin_facts: ahash::AHashMap::default(),
        })
    }

    /// Reset the JIT to a clean, empty module, discarding every compiled
    /// kernel and the cross-kernel `by_kernel` cache. The lambda-kernel
    /// SIGNATURE cache (`kernels`) is module-independent — it holds
    /// `Arc<KernelSig>` descriptors, not `FuncId`s (those live in the
    /// Jit's `by_kernel`) — so it deliberately survives: a surviving sig
    /// simply re-declares into the fresh module on next use.
    ///
    /// For the check/LSP path ONLY. That path shares one long-lived
    /// runtime across every checked file and NEVER executes a fused
    /// kernel (the checked nodes are deleted right after), so without a
    /// per-check reset each file's kernels pile into the single
    /// persistent module until cranelift's `finalize_definitions` chokes
    /// on the bloat — a cross-file crash that bricks the shared runtime.
    /// A per-check reset makes each file hermetic, like a fresh-runtime
    /// `--check`. MUST NOT be called on a runtime with live (executing)
    /// kernels: it frees their compiled code.
    pub fn reset_jit_for_check(&self) -> anyhow::Result<()> {
        *self.jit.lock() = emit::Jit::new()?;
        Ok(())
    }
}

/// One free-var input slot resolved during walker analysis.
#[derive(Debug, Clone)]
pub(crate) struct FreeVarInput {
    pub(crate) bind_id: BindId,
    pub(crate) name: arcstr::ArcStr,
    /// Kernel-input classification, computed once from the binding's
    /// type. Drives the `RegionInput.kind` at build time.
    pub(crate) kind: RegionInputKind,
    /// Full graphix type — used to construct the runtime feeder Node
    /// (`genn::reference`), which needs the complete `Type`.
    pub(crate) typ: Type,
}

/// Walk the candidate subtree, collect every external Ref's
/// BindId (referenced but NOT bound inside the body), and resolve
/// each to its name + kernel-input classification via `ctx.env.by_id`.
///
/// Because `CallSite::refs` recurses into a statically-resolved
/// lambda's body (via `GXLambda::refs`), a lambda's captures
/// transitively surface here as external Refs of the enclosing
/// region — so this pre-pass registers them as parent kernel inputs,
/// and `emit_lambda_call`'s capture forwarding finds them via
/// `lookup_local_by_bind_id`. This is the mechanism that makes
/// closure conversion's capture cascade automatic.
///
/// Slots whose type has no kernel-input representation (function
/// types, bare `String`/`Null`/`Unit`) are skipped — the builder
/// fails those Refs when emitting them and the candidate stays
/// unfused (correct fall-back to the interpreter).
pub(crate) fn collect_region_inputs<R: Rt, E: UserEvent>(
    subtree: &dyn Update<R, E>,
    ctx: &ExecCtx<R, E>,
) -> Vec<FreeVarInput> {
    let mut refs = Refs::default();
    subtree.refs(&mut refs);
    let mut out: Vec<FreeVarInput> = Vec::new();
    let mut seen: ahash::AHashSet<BindId> = ahash::AHashSet::default();
    refs.with_external_refs(|id| {
        if !seen.insert(id) {
            return;
        }
        if let Some(fv) = free_var_input(id, ctx) {
            out.push(fv);
        }
    });
    out
}

/// Resolve a single binding `id` to its [`FreeVarInput`] (name + kernel
/// slot classification + full type), or `None` if its type has no
/// kernel-input representation (function types, bare `String`/`Null`).
/// Shared by [`collect_region_inputs`] (external refs) and the
/// connect-target LIFT (a let-bound counter routed in as a feeder).
pub(crate) fn free_var_input<R: Rt, E: UserEvent>(
    id: BindId,
    ctx: &ExecCtx<R, E>,
) -> Option<FreeVarInput> {
    let b = ctx.env.by_id.get(&id)?;
    // Resolve named/abstract type refs to their concrete rep BEFORE
    // freezing — `freeze_for_abi` is deliberately Env-free and rejects
    // `Type::Ref`, so an abstract-typed input needs the same
    // `resolve_abstract` pre-pass the classic kernel-signature derivation
    // applies (#218). The feeder type stays UNRESOLVED — the runtime Ref
    // wants the type system's view; only the kernel-slot classification
    // (`kind`, which carries the frozen types) needs the concrete rep.
    let resolved = resolve_abstract(&ctx.fusion.abstract_registry, &b.typ, &ctx.env);
    // Normalized: a binding whose defining select has a never() arm
    // carries a Set polluted by the arm's late-bound TVar — the
    // resolve_tvars rung collapses it so the local threads as a region
    // input instead of silently de-fusing every consumer
    // (bench/stream_stats.gx's window-gate idiom).
    let frozen =
        kernel_abi::freeze_for_abi_normalized(&ctx.fusion.abstract_registry, &resolved)?;
    let kind =
        lowering::type_to_region_input_kind(&ctx.fusion.abstract_registry, frozen)?;
    Some(FreeVarInput {
        bind_id: id,
        name: arcstr::ArcStr::from(b.name.as_str()),
        kind,
        typ: b.typ.clone(),
    })
}

/// Detect "lifted" connect targets in a region — a SCALAR variable that
/// is (a) `let`-bound in the region to a compile-time CONSTANT and (b)
/// the target of EXACTLY ONE `connect` (`x <- e`). Such a variable is a
/// reactive counter / accumulator whose READS must see the
/// connect-written value (through a feeder), not the stale let-local that
/// the kernel would otherwise bind. We route it in as a kernel INPUT;
/// `emit_let_node`'s seed-select then reads the feeder (or the constant
/// seed when the feeder has never fired), and the connect's `set_var`
/// writes it. The fired-bit (STALE) makes this correct with NO cadence
/// restriction: the constant seed (fresh at init, STALE after — used when
/// the feeder is missing) reproduces the node-walk's one-shot `Bind` plus
/// its downstream combineLatest cache, and `set_var_typed`'s fresh-gate
/// reproduces `Connect::update`'s `if let Some(v) = ..` guard.
///
/// A non-constant seed is excluded (the node-walk re-runs the `Bind` when
/// the seed's input fires, re-seeding the variable — the lift's
/// fire-once seed can't model that). `ConnectDeref` (`*r <- e`) and
/// multiple connects to one var are excluded (v1).
pub(crate) fn collect_lifted_connect_targets<R: Rt, E: UserEvent>(
    node: &Node<R, E>,
    ctx: &ExecCtx<R, E>,
) -> ahash::AHashSet<BindId> {
    let mut connect_count: ahash::AHashMap<BindId, usize> = ahash::AHashMap::default();
    let mut const_lets: ahash::AHashSet<BindId> = ahash::AHashSet::default();
    for_each_node(node, &mut |n| match n.view() {
        NodeView::Connect(c) => {
            *connect_count.entry(c.id).or_default() += 1;
        }
        NodeView::Bind(b) => {
            if let Some(id) = b.single_bind_id() {
                // The seed must be a compile-time CONSTANT (a scalar
                // Constant, or a constant-foldable array/tuple literal /
                // string — `node_const_value`): such a producer fires
                // once (at init) exactly like the node-walk's `Bind` of a
                // literal, which is what the STALE-gated seed reproduces.
                // The shape gate matches `emit_let_node`'s lifted arms —
                // scalar (register select), composite / string
                // (branch-based clone-vs-seed).
                use kernel_abi::AbiKind;
                let shape_ok = matches!(
                    kernel_abi::abi_kind(&ctx.fusion.abstract_registry, b.node.typ()),
                    Some(
                        AbiKind::Scalar(_)
                            | AbiKind::Array
                            | AbiKind::Tuple
                            | AbiKind::Struct
                            | AbiKind::String
                    )
                );
                if shape_ok && lowering::node_const_value(&b.node).is_some() {
                    const_lets.insert(id);
                }
            }
        }
        _ => {}
    });
    connect_count
        .into_iter()
        .filter(|&(t, count)| count == 1 && const_lets.contains(&t))
        .map(|(t, _)| t)
        .collect()
}

/// Visit `node` and every reachable descendant (pre-order: `f` sees a
/// node before its children) — the canonical full-coverage immutable
/// walker. The `NodeView` match is EXHAUSTIVE on purpose: a new node
/// variant is a compile error here, not a silently-untraversed
/// container. Lambda BODIES are
/// not descended (a body compiles per call site — its call sites belong
/// to the callee kernel's own discovery), and `FusedKernel` is opaque
/// (post-fusion synthetic).
pub(crate) fn for_each_node<'a, R: Rt, E: UserEvent>(
    node: &'a Node<R, E>,
    f: &mut dyn FnMut(&'a Node<R, E>),
) {
    f(node);
    macro_rules! rec {
        ($($n:expr),*) => {{ $(for_each_node::<R, E>($n, f);)* }};
    }
    match node.view() {
        NodeView::Bind(b) => rec!(&b.node),
        NodeView::Module(m) => {
            for child in m.nodes.iter() {
                rec!(child)
            }
        }
        NodeView::Block(blk) => {
            for child in blk.children.iter() {
                rec!(child)
            }
        }
        NodeView::CallSite(cs) => {
            for arg in cs.args.values() {
                if let Some(n) = &arg.node {
                    rec!(n)
                }
            }
            rec!(&cs.fnode)
        }
        NodeView::Select(s) => {
            rec!(&s.arg.node);
            for (pat, body) in s.arms.iter() {
                if let Some(g) = &pat.guard {
                    rec!(&g.node)
                }
                rec!(&body.node)
            }
        }
        NodeView::TryCatch(t) => {
            for n in t.nodes.iter() {
                rec!(n)
            }
            rec!(&t.handler)
        }
        NodeView::Qop(q) => rec!(&q.n),
        NodeView::OrNever(o) => rec!(&o.n),
        NodeView::ExplicitParens(p) => rec!(&p.n),
        NodeView::TypeCast(t) => rec!(&t.n),
        NodeView::Not(n) => rec!(&n.n),
        NodeView::Neg(n) => rec!(&n.n),
        NodeView::Connect(c) => rec!(&c.node),
        NodeView::ConnectDeref(c) => rec!(&c.rhs.node),
        NodeView::StringInterpolate(s) => {
            for a in s.args.iter() {
                rec!(&a.node)
            }
        }
        NodeView::Any(a) => {
            for n in a.n.iter() {
                rec!(n)
            }
        }
        NodeView::Sample(s) => rec!(&s.trigger, &s.arg.node),
        NodeView::Struct(s) => {
            for c in s.n.iter() {
                rec!(&c.node)
            }
        }
        NodeView::StructWith(s) => {
            rec!(&s.source);
            for r in s.replace.iter() {
                rec!(&r.n.node)
            }
        }
        NodeView::Tuple(t) => {
            for c in t.n.iter() {
                rec!(&c.node)
            }
        }
        NodeView::Variant(v) => {
            for c in v.n.iter() {
                rec!(&c.node)
            }
        }
        NodeView::Array(a) => {
            for c in a.n.iter() {
                rec!(&c.node)
            }
        }
        NodeView::Map(m) => {
            for c in m.keys.iter() {
                rec!(&c.node)
            }
            for c in m.vals.iter() {
                rec!(&c.node)
            }
        }
        NodeView::StructRef(s) => rec!(&s.source),
        NodeView::TupleRef(t) => rec!(&t.source),
        NodeView::ArrayRef(a) => rec!(&a.source.node, &a.i.node),
        NodeView::ArraySlice(a) => {
            rec!(&a.source.node);
            if let Some(s) = &a.start {
                rec!(&s.node)
            }
            if let Some(e) = &a.end {
                rec!(&e.node)
            }
        }
        NodeView::MapRef(m) => rec!(&m.source.node, &m.key.node),
        NodeView::ByRef(b) => rec!(&b.child),
        NodeView::Deref(d) => rec!(&d.child),
        NodeView::Add(o) => rec!(&o.lhs.node, &o.rhs.node),
        NodeView::Sub(o) => rec!(&o.lhs.node, &o.rhs.node),
        NodeView::Mul(o) => rec!(&o.lhs.node, &o.rhs.node),
        NodeView::Div(o) => rec!(&o.lhs.node, &o.rhs.node),
        NodeView::Mod(o) => rec!(&o.lhs.node, &o.rhs.node),
        NodeView::CheckedAdd(o) => rec!(&o.lhs.node, &o.rhs.node),
        NodeView::CheckedSub(o) => rec!(&o.lhs.node, &o.rhs.node),
        NodeView::CheckedMul(o) => rec!(&o.lhs.node, &o.rhs.node),
        NodeView::CheckedDiv(o) => rec!(&o.lhs.node, &o.rhs.node),
        NodeView::CheckedMod(o) => rec!(&o.lhs.node, &o.rhs.node),
        NodeView::Eq(o) => rec!(&o.lhs.node, &o.rhs.node),
        NodeView::Ne(o) => rec!(&o.lhs.node, &o.rhs.node),
        NodeView::Lt(o) => rec!(&o.lhs.node, &o.rhs.node),
        NodeView::Gt(o) => rec!(&o.lhs.node, &o.rhs.node),
        NodeView::Lte(o) => rec!(&o.lhs.node, &o.rhs.node),
        NodeView::Gte(o) => rec!(&o.lhs.node, &o.rhs.node),
        NodeView::And(o) => rec!(&o.lhs.node, &o.rhs.node),
        NodeView::Or(o) => rec!(&o.lhs.node, &o.rhs.node),
        NodeView::Lambda(_) => {}
        NodeView::Ref(_)
        | NodeView::Constant(_)
        | NodeView::Use(_)
        | NodeView::TypeDef(_)
        | NodeView::Nop(_) => {}
        NodeView::FusedKernel(_) => {}
    }
}

/// One discovered statically-resolved lambda call site in a region
/// being directly compiled — the lambda-call analogue of
/// [`lowering::BuiltinCallSiteInfo`]. Recorded by
/// [`discover_lambda_calls`] during `try_fuse`'s ANALYSIS phase (before
/// the jit lock — `build_lambda_kernel` needs `&mut ExecCtx`); consumed
/// by `CallSite::emit_clif` via `BodyCx::lambda_site` to emit a CLIF
/// `call` against the declared callee.
#[derive(Debug, Clone)]
pub struct LambdaCallInfo {
    /// The callee kernel's name in the `funcids`/`callee_refs` maps.
    /// Always the CACHED kernel's name (a cache hit returns the first
    /// builder's name — possibly a `__hof_*` name from the per-slot
    /// HOF path), never this call site's source name.
    pub fn_name: arcstr::ArcStr,
    /// Kept alive so the return-ABI read outlives the build and the
    /// `by_kernel` entry pins the same `Arc`.
    pub kernel: std::sync::Arc<KernelSig>,
    /// The callee's flat input types in signature order — formals
    /// first, captures appended (`KnownFusedFn::arg_types`, cloned
    /// from the `CachedKernel` at discovery). These were resolved
    /// (`resolve_abstract`) and frozen at BUILD time, so they're the
    /// caller's type authority for arg classification — freezing the
    /// caller-side node type instead re-rejects abstract Refs (#218),
    /// and env isn't available at emit time to resolve them. The
    /// classic caller (`emit_lambda_call`) types args the same way.
    pub arg_types: Vec<Type>,
    /// Closure-converted captures, appended after the formal args in
    /// the callee's input list (the caller marshals each from its own
    /// env, BindId-first).
    pub captures: Vec<lowering::CaptureSlot>,
}

/// A discovered callee's body Node + self-call info, recorded by
/// [`discover_lambda_calls`] for the direct path's per-callee body
/// emission (F0). The body reference is live through this region's
/// resolved `GXLambda` for the duration of `try_fuse`.
pub struct CalleeBody<'n, R: Rt, E: UserEvent> {
    pub body: &'n Node<R, E>,
    /// `Some((self_bind, info))` for a self-recursive callee: the
    /// binding its body's self-references carry, and the kernel's own
    /// call descriptor — non-tail self-calls emit through the regular
    /// lambda-call path against the kernel's own FuncRef; tail-position
    /// self-calls become the rebind-and-jump loop.
    pub self_call: Option<(BindId, LambdaCallInfo)>,
    /// This callee body's OWN statically-resolved lambda call sites
    /// (#203 Phase C — the transitive closure). The callee's
    /// `NodeBodyEmitter` consumes these so its body emits its nested
    /// cross-kernel calls; empty for a leaf callee (one whose body calls
    /// no other fusable lambda).
    pub sites: nohash::IntMap<ExprId, LambdaCallInfo>,
    /// This callee body's OWN sync builtin/cast/qop Apply sites
    /// (`CachedKernel.apply_sites`, mirrored here so the callee's
    /// `NodeBodyEmitter` can emit them). Consumed by Stage 2's
    /// combined-slot runtime delivery; until then the callee emitter
    /// ignores it and a callee-with-a-builtin de-fuses.
    pub apply_sites: nohash::IntMap<ExprId, lowering::BuiltinCallSiteInfo>,
}

/// Walk the region collecting every statically-resolved lambda call
/// site, building (or cache-hitting) each callee's [`CachedKernel`]
/// signature — TRANSITIVELY (#203 Phase C). After a callee's kernel is
/// built its OWN body is scanned for further lambda calls, so the whole
/// reachable closure of cross-kernel calls is discovered (callback → g →
/// g2 → …). A lambda that fails to build (unsupported arg/return shape)
/// is simply NOT recorded — its call site bails at emission and the
/// subtree node-walks. A callee whose body has a call this discovery
/// can't record (a non-fusable builtin, a dynamic dispatch) emits no
/// CLIF for that call and the whole region de-fuses (never a partial
/// kernel).
///
/// Returns: the ROOT's call sites; the name→`KernelSig` map of EVERY
/// callee in the closure (the define loop declares them all, so they can
/// call each other — including mutual recursion); and a `kernel-ptr →
/// CalleeBody` map giving each callee's body `Node` (reached live
/// through its resolved `GXLambda`), its self-call info, and its OWN
/// discovered call sites. Termination: a callee already recorded in
/// `bodies` is not re-scanned, so self- and mutual-recursion close the
/// loop (the back-edge's call site is still recorded for emission, but
/// the body isn't re-enqueued).
pub(crate) fn discover_lambda_calls<'n, R: Rt, E: UserEvent>(
    root: &'n Node<R, E>,
    ctx: &mut ExecCtx<R, E>,
) -> (
    nohash::IntMap<ExprId, LambdaCallInfo>,
    Vec<(usize, std::sync::Arc<KernelSig>)>,
    std::collections::BTreeMap<usize, CalleeBody<'n, R, E>>,
) {
    // Identified by kernel IDENTITY (`kernel_key`), like `bodies` —
    // names shadow and monomorphizations share a name, so a name-keyed
    // map here bound call sites to the wrong kernel (audit-jul2026
    // 01/02). Kept as a Vec in DISCOVERY order: emission iterates this
    // list to assign fn indices, DynCall slot bases, and the region
    // layout, and a pointer-ordered map made all of those
    // ASLR-dependent — the compiled shape of the same program differed
    // across processes (#19).
    let mut callees: Vec<(usize, std::sync::Arc<KernelSig>)> = Vec::new();
    let mut bodies: std::collections::BTreeMap<usize, CalleeBody<'n, R, E>> =
        std::collections::BTreeMap::new();
    // Bodies still to scan; the second field says where the body's
    // discovered sites land — `None` = the root (returned), `Some(ptr)`
    // = that callee's `CalleeBody.sites`. LIFO; scan order is irrelevant
    // (every reachable body is scanned exactly once, gated by `bodies`).
    let mut worklist: Vec<(&'n Node<R, E>, Option<usize>)> = vec![(root, None)];
    let mut root_sites: nohash::IntMap<ExprId, LambdaCallInfo> =
        nohash::IntMap::default();
    while let Some((body, target)) = worklist.pop() {
        let mut local_sites: nohash::IntMap<ExprId, LambdaCallInfo> =
            nohash::IntMap::default();
        let mut enqueue: Vec<(&'n Node<R, E>, usize)> = Vec::new();
        for_each_node(body, &mut |n| {
            let NodeView::CallSite(cs) = n.view() else {
                return;
            };
            let Some(ApplyView::Lambda(g)) = cs.resolved_apply() else {
                return;
            };
            // Kernel name: the call site's SOURCE name — a LABEL for the
            // emitted symbol and diagnostics only; resolution is by kernel
            // identity, so shadowed same-name lambdas and multiple
            // monomorphizations coexist. A lambda-literal call
            // (`(|x| x)(1)` — fnode isn't a Ref) has no name and stays on
            // the node-walk.
            let ExprKind::Ref { name } = &cs.fnode.spec().kind else {
                return;
            };
            let name: arcstr::ArcStr = match lowering::ident_of(name) {
                Some(ident) => arcstr::ArcStr::from(ident),
                None => {
                    let s: &str = name.0.as_ref();
                    arcstr::ArcStr::from(s)
                }
            };
            // The fnode Ref's BindId identifies the binding being called —
            // build_lambda_kernel uses it to recognise self-recursion.
            let call_bind = match cs.fnode.view() {
                NodeView::Ref(r) => Some(r.id),
                _ => None,
            };
            // The SITE's resolved FnType keys the kernel cache — see
            // build_lambda_kernel (the instance's g.typ() lies about
            // which monomorphization a later site calls).
            let Some(site_ftype) = cs.resolved_ftype().or_else(|| cs.ftype()) else {
                return;
            };
            let Some(cached) =
                lowering::build_lambda_kernel(g, site_ftype, &name, call_bind, ctx)
            else {
                return;
            };
            let ptr = kernel_abi::kernel_key(&cached.kernel);
            // First time we reach this callee: record it (discovery
            // order), record its body + self-call info, and enqueue it
            // for transitive scanning. A repeat (its own self-call, or
            // a mutual back-edge) records the site below but does NOT
            // re-enqueue — that's the termination guard.
            if !bodies.contains_key(&ptr) {
                callees.push((ptr, cached.kernel.clone()));
                let self_call = cached.is_rec.then(|| {
                    (
                        cached.self_bind.expect(
                            "is_rec without self_bind — \
                             build_lambda_kernel derives is_rec FROM \
                             self_bind",
                        ),
                        LambdaCallInfo {
                            fn_name: cached.fn_name.clone(),
                            kernel: cached.kernel.clone(),
                            arg_types: cached.signature.arg_types.clone(),
                            captures: cached.captures.clone(),
                        },
                    )
                });
                bodies.insert(
                    ptr,
                    CalleeBody {
                        body: g.body(),
                        self_call,
                        sites: nohash::IntMap::default(),
                        apply_sites: cached.apply_sites.clone(),
                    },
                );
                enqueue.push((g.body(), ptr));
            }
            local_sites.insert(
                n.spec().id,
                LambdaCallInfo {
                    fn_name: cached.fn_name,
                    kernel: cached.kernel,
                    arg_types: cached.signature.arg_types.clone(),
                    captures: cached.captures,
                },
            );
        });
        match target {
            None => root_sites = local_sites,
            Some(ptr) => {
                if let Some(cb) = bodies.get_mut(&ptr) {
                    cb.sites = local_sites;
                }
            }
        }
        worklist.extend(enqueue.into_iter().map(|(body, ptr)| (body, Some(ptr))));
    }
    (root_sites, callees, bodies)
}

/// Run the fusion phase on one `Node` — the distributed path's uniform
/// visit protocol, used by [`crate::compile`] on the root and by every
/// [`Update::fuse`] impl on its children: first try to fuse the
/// WHOLE subtree via [`try_fuse`]; if it doesn't fuse, recurse into the
/// node's own `fuse` (which fuses ITS children's maximal subtrees in
/// turn). Either way a produced replacement is swapped in and the
/// original deleted — the parent owns the swap because a node can't
/// replace itself behind `&mut self`.
///
/// Maximality falls out of the top-down order: the highest subtree
/// whose `try_fuse` succeeds is spliced and nothing below it is ever
/// attempted; a failed attempt falls through to finer-grained fusion
/// inside.
///
/// The `FusionDisabled` short-circuit is NOT here — it
/// is checked once in [`crate::compile`] before this is called, rather
/// than on every recursive step.
pub fn fuse<R: Rt, E: UserEvent>(
    child: &mut Node<R, E>,
    ctx: &mut ExecCtx<R, E>,
) -> anyhow::Result<()> {
    if let Some(new) = try_fuse(child, ctx)? {
        let mut old = std::mem::replace(child, new);
        old.delete(ctx);
        return Ok(());
    }
    if let Some(new) = child.fuse(ctx)? {
        let mut old = std::mem::replace(child, new);
        old.delete(ctx);
    }
    Ok(())
}

/// Try to fuse the whole subtree rooted at `node` into one JIT kernel.
/// Mechanics only, NO policy — policy (where to attempt, what to
/// recurse) lives in each node's [`Update::fuse`] impl.
///
/// `Ok(Some(replacement))` — the subtree compiled; the replacement is
/// a [`FusedKernel`] node (feeders + JIT dispatch) the caller swaps in
/// (deleting the original). `Ok(None)` — not fusable: the root type
/// isn't representable at the kernel boundary, the subtree is an
/// identity passthrough (zero compute — the runtime `Ref` feeder
/// already produces the value), or some node in the subtree doesn't
/// emit CLIF (the `emit_clif` default `Err`) — async ops, unsupported
/// shapes. "Is it fusable" IS the compile attempt; there is no
/// separate analysis to drift out of sync with the emitter.
pub fn try_fuse<R: Rt, E: UserEvent>(
    node: &Node<R, E>,
    ctx: &mut ExecCtx<R, E>,
) -> anyhow::Result<Option<Node<R, E>>> {
    // Identity suppression: a bare binding read (possibly through
    // grouping parens) forwards one input unchanged — fusing it wraps
    // zero compute in dispatch overhead (and the `run!` harness's
    // `let result = {code}` wrapper would otherwise register as
    // "fused" without any real body fusion — see CLAUDE.md #139).
    if region_is_identity(node) {
        return Ok(None);
    }
    let Some(return_type) =
        freeze_region_return(&ctx.fusion.abstract_registry, node.typ(), &ctx.env)
    else {
        return Ok(None);
    };
    ctx.fusion.stats.attempted += 1;
    let mut inputs = collect_region_inputs(&**node, ctx);
    // LIFT: a let-bound scalar counter/accumulator that is a `connect`
    // target is routed in as a kernel INPUT (a feeder) so its READS see
    // the connect-written variable, not the stale let-local. It's bound
    // in-region (so `with_external_refs` skipped it) — add it explicitly.
    // `emit_let_node` reads `lifted` to emit the seed-select instead of a
    // plain let, and `emit_connect_node` reads it to allow the write.
    let mut lifted = collect_lifted_connect_targets(node, ctx);
    // Inject each lifted var as an input, KEEPING only those that yield a
    // valid `FreeVarInput` (a scalar always does — but stay consistent: a
    // var left in `lifted` without a matching feeder param would make
    // `emit_let_node`'s seed-select fail and de-fuse). It's bound
    // in-region, so it was never an external input (the `any` guard is
    // defensive — it can't fire).
    lifted.retain(|&t| {
        if inputs.iter().any(|fv| fv.bind_id == t) {
            return false;
        }
        match free_var_input(t, ctx) {
            Some(fv) => {
                inputs.push(fv);
                true
            }
            None => false,
        }
    });
    // #219 taint rides each param's disc word (no separate validity
    // bitmask), and the per-param firing trackers spill to the heap, so
    // there is no input-count ceiling — a region of any width fuses.
    if let Some(name) = non_scalar_basename_collision(&inputs) {
        // A real blocker, not protocol noise — log it (a silent
        // Ok(None) after `attempted += 1` makes the stats disagree
        // with the failure list).
        ctx.fusion.stats.failed.push((
            node.spec().id,
            compact_str::format_compact!(
                "non-scalar region inputs share basename `{name}` — \
                 refuse to fuse"
            ),
        ));
        return Ok(None);
    }
    // Discover sync-builtin Apply sites BEFORE the kernel build (the
    // same Node-based prepass the classic path runs in `fuse`):
    // `fn_params` installs the `FnSource::Builtin` slots on the sig —
    // the runtime (`Kernel::pre_init_builtin_slots`) constructs each
    // site's Apply from them — and `apply_sites` lets
    // `CallSite::emit_clif` recognise a registered site and lower it
    // to a DynCall.
    let mut discovery = lowering::BuiltinCallDiscovery::default();
    lowering::walk_node_for_builtin_calls::<R, E>(node, ctx, &mut discovery);
    // Statically-resolved lambda call sites: build (or cache-hit) each
    // callee's kernel NOW — `build_lambda_kernel` needs `&mut ExecCtx`,
    // which emission (under the jit lock) can't have. The callees
    // compile from their body Nodes during the parallel period; the
    // parent's call sites emit CLIF `call`s against them.
    let (lambda_sites, lambda_callees, callee_bodies) = discover_lambda_calls(node, ctx);
    let source_id = node.spec().id;
    let (mut sig, _arg_types) = match sig_from_inputs(
        arcstr::ArcStr::from(
            compact_str::format_compact!("region_{:?}", source_id).as_str(),
        ),
        inputs.iter().map(|fv| (fv.name.clone(), &fv.kind, Some(fv.bind_id))),
        return_type,
    ) {
        Ok(v) => v,
        Err(e) => {
            // A frozen region input whose ABI kind doesn't match its
            // Type — a freeze invariant that should hold for well-typed
            // input, but de-fuse rather than panic if it ever doesn't.
            ctx.fusion.stats.failed.push((
                source_id,
                compact_str::format_compact!("sig_from_inputs: {e:#}"),
            ));
            return Ok(None);
        }
    };
    sig.fn_params = discovery.fn_params;
    let kernel = std::sync::Arc::new(sig);
    // The compile attempt: entry binds the declared params, then the
    // body is emitted by `emit_clif` recursion from the root. Any Err
    // (a node that doesn't emit) discards the half-built function —
    // the subtree node-walks.
    let wrapped = match emit::compile_kernel_with_callees_direct(
        &mut ctx.fusion.jit.lock(),
        &kernel,
        &lambda_callees,
        node,
        &discovery.apply_sites,
        &lambda_sites,
        &callee_bodies,
        None,
        &ctx.env,
        &ctx.fusion.abstract_registry,
        &lifted,
    ) {
        Ok(w) => std::sync::Arc::new(w),
        Err(e) => {
            log::trace!("fusion::try_fuse: region {source_id:?} doesn't fuse: {e:#}");
            ctx.fusion
                .stats
                .failed
                .push((source_id, compact_str::format_compact!("{e:#}")));
            return Ok(None);
        }
    };
    // Feeders register `Rt::ref_var(bind_id, TOP_ID)` — the runtime
    // wakes a top expression on `set_var` only while its (id, top_id)
    // ref count is nonzero. The REAL top id (from `ExecCtx::
    // fuse_top_id`, set per `compile()`), NOT the region's interior
    // `source_id`: registering under an id no installed expression
    // matches would strand the top expression at count zero once the
    // spliced original's Refs unref on delete — a region fed by a
    // `<-`-written variable would never see updates past cycle one.
    let feeder_top = ctx.fusion.top_id.unwrap_or(source_id);
    let feeders: Box<[Node<R, E>]> = inputs
        .iter()
        .map(|fv| genn::reference::<R, E>(ctx, fv.bind_id, fv.typ.clone(), feeder_top))
        .collect();
    match builder::FusedKernel::<R, E>::new(
        ctx,
        node.spec().clone(),
        node.typ().clone(),
        kernel,
        Some(wrapped),
        feeders,
        Scope::root(),
        source_id,
    ) {
        Ok(n) => {
            log::debug!(
                "fusion::try_fuse: fused region {source_id:?} with {} input(s)",
                inputs.len()
            );
            // `GRAPHIX_DBG_REGION`: dump each fused region's input wiring
            // (name, BindId, declared vs deref'd type, cell constraints,
            // derived slot kind). The region-side complement of
            // `GRAPHIX_DUMP_CLIF` — a kernel gated forever on a "missing"
            // input usually means an input here froze under a lied-about
            // type (#18 was diagnosed with exactly this dump).
            if std::env::var_os("GRAPHIX_DBG_REGION").is_some() {
                for (i, fv) in inputs.iter().enumerate() {
                    let deref =
                        crate::format_with_flags(crate::PrintFlag::DerefTVars, || {
                            compact_str::format_compact!("{}", fv.typ)
                        });
                    let cons = match &fv.typ {
                        crate::typ::Type::TVar(tv) => {
                            let cs = tv.cell_constraints();
                            compact_str::format_compact!("{cs:?}")
                        }
                        _ => compact_str::format_compact!("-"),
                    };
                    eprintln!(
                        "DBGREGION {source_id:?} input[{i}] name={} bind={:?} \
                         typ={} deref={deref} cons={cons} kind={:?}",
                        fv.name, fv.bind_id, fv.typ, fv.kind
                    );
                }
            }
            ctx.fusion.stats.fused += 1;
            ctx.fusion.stats.fused_ids.push(source_id);
            Ok(Some(n))
        }
        Err(e) => {
            // The kernel COMPILED but the runtime carrier refused it —
            // log like any other blocker (a silent Ok(None) here made
            // `attempted` and `failed` disagree, which is exactly the
            // drift FusionStats exists to expose).
            ctx.fusion.stats.failed.push((
                source_id,
                compact_str::format_compact!("FusedKernel::new: {e:#}"),
            ));
            Ok(None)
        }
    }
}

/// Scalar env slots resolve BindId-first (C2) — duplicate basenames
/// are fine there. The OTHER per-kind tables are still name-keyed
/// (BindId-keying lands per-table as each shape gains emission):
/// two distinct non-scalar inputs sharing a basename would silently
/// alias one slot — callers refuse to fuse instead. Returns the
/// first colliding name.
pub(crate) fn non_scalar_basename_collision(
    inputs: &[FreeVarInput],
) -> Option<&arcstr::ArcStr> {
    let mut names: ahash::AHashSet<&str> = ahash::AHashSet::default();
    for fv in inputs {
        if matches!(fv.kind, RegionInputKind::Prim(_)) {
            continue;
        }
        if !names.insert(fv.name.as_str()) {
            return Some(&fv.name);
        }
    }
    None
}

/// Freeze a region root's graphix type into the kernel's return ABI
/// type, or `None` if the region can't fuse. The kernel boundary
/// can't represent bare `Null` (fusion must widen to Nullable first)
/// or `Unit` (a side-effect-only marker), and a type that doesn't
/// freeze to a concrete shape can't have an ABI at all.
/// `freeze_for_abi_normalized` because a select-rooted region's type is
/// typecheck's raw arm union (`Set([i64, TVar→i64])`), which only
/// freezes once flattened. On plain-freeze failure, retry through
/// `resolve_abstract` (#218): a region returning an abstract-typed
/// value (e.g. `Array<Elem>` with `Elem` an interface type) carries
/// Refs the env-free freeze rejects; the resolved concrete rep IS
/// the return ABI.
pub(crate) fn freeze_region_return(
    reg: &kernel_abi::AbstractRegistry,
    typ: &Type,
    env: &Env,
) -> Option<Type> {
    use kernel_abi::AbiKind;
    if std::env::var_os("GRAPHIX_DBG_FREEZE").is_some() {
        let d = crate::format_with_flags(crate::PrintFlag::DerefTVars, || {
            compact_str::format_compact!("{typ}")
        });
        eprintln!(
            "DBGFREEZE typ={typ} deref={d} resolved={:?}",
            typ.resolve_tvars().normalize()
        );
    }
    let return_type = match freeze_for_abi_normalized(reg, typ) {
        Some(t) => t,
        None => {
            let resolved = resolve_abstract(reg, typ, env);
            freeze_for_abi_normalized(reg, &resolved)?
        }
    };
    match kernel_abi::abi_kind(reg, &return_type) {
        Some(
            AbiKind::Scalar(_)
            | AbiKind::Array
            | AbiKind::Tuple
            | AbiKind::Struct
            | AbiKind::Variant
            | AbiKind::Nullable
            | AbiKind::Value
            | AbiKind::String,
        ) => Some(return_type),
        Some(AbiKind::Unit | AbiKind::Null) | None => None,
    }
}

/// True iff the subtree is a bare binding read (through any number of
/// grouping parens) — an identity passthrough that forwards one input
/// unchanged, so there's no compute to fuse.
fn region_is_identity<R: Rt, E: UserEvent>(node: &Node<R, E>) -> bool {
    let mut n: &dyn Update<R, E> = &**node;
    loop {
        match n.view() {
            NodeView::Ref(_) => return true,
            NodeView::ExplicitParens(p) => n = &*p.n,
            _ => return false,
        }
    }
}

/// Build a [`KernelSig`] straight from a typed input list
/// — signature only, no body. The single source of truth for per-kind slot routing
/// (one slot per input, one [`kernel_abi::TailCallSlot`] per input in
/// source order — the runtime's `build_arg_layout` reads the slot list
/// for per-position routing even in non-tail kernels). Used by every
/// kernel-build path: `try_fuse` regions, lambda kernels
/// (`build_lambda_kernel` — formals carry `bind_id: None`, captures
/// their binding), and body-split sub-regions.
///
/// The second return is the flat per-input graphix type list in slot
/// order — [`kernel_abi::KnownFusedFn::arg_types`], the caller-side
/// type authority for cross-kernel call marshalling.
pub(crate) fn sig_from_inputs<'k>(
    fn_name: arcstr::ArcStr,
    inputs: impl IntoIterator<Item = (arcstr::ArcStr, &'k RegionInputKind, Option<BindId>)>,
    return_type: Type,
) -> anyhow::Result<(KernelSig, Vec<Type>)> {
    use kernel_abi::{
        ArrayInput, Input, NullableInput, StringInput, StructInput, TailCallSlot,
        TailCallSlotKind, TupleInput, ValueInput, VariantInput,
    };
    let mut sig = KernelSig {
        fn_name,
        params: Vec::new(),
        fn_params: Vec::new(),
        array_params: Vec::new(),
        tuple_params: Vec::new(),
        struct_params: Vec::new(),
        variant_params: Vec::new(),
        nullable_params: Vec::new(),
        string_params: Vec::new(),
        value_params: Vec::new(),
        tail_call_slots: Vec::new(),
        return_type,
        has_tail_loop: false,
    };
    let mut arg_types: Vec<Type> = Vec::new();
    for (name, kind, bind_id) in inputs.into_iter() {
        let slot_kind = match kind {
            RegionInputKind::Prim(prim) => {
                sig.params.push(Input { name: name.clone(), prim: *prim, bind_id });
                arg_types.push(kernel_abi::prim_type(*prim));
                TailCallSlotKind::Scalar(*prim)
            }
            RegionInputKind::Array(elem) => {
                sig.array_params.push(ArrayInput {
                    name: name.clone(),
                    elem: elem.clone(),
                    bind_id,
                });
                arg_types.push(kernel_abi::array_type(elem.clone()));
                TailCallSlotKind::ValArray
            }
            RegionInputKind::Tuple(t) => {
                let elems = kernel_abi::tuple_slots(t).map(<[Type]>::to_vec).ok_or_else(
                    || {
                        anyhow::anyhow!(
                            "RegionInputKind::Tuple must carry a frozen \
                             Type::Tuple (freeze invariant)"
                        )
                    },
                )?;
                sig.tuple_params.push(TupleInput { name: name.clone(), elems, bind_id });
                arg_types.push(t.clone());
                TailCallSlotKind::ValArray
            }
            RegionInputKind::Struct(t) => {
                let fields = kernel_abi::struct_fields(t)
                    .map(<[(arcstr::ArcStr, Type)]>::to_vec)
                    .ok_or_else(|| {
                        anyhow::anyhow!(
                            "RegionInputKind::Struct must carry a frozen \
                             Type::Struct (freeze invariant)"
                        )
                    })?;
                sig.struct_params.push(StructInput {
                    name: name.clone(),
                    fields,
                    bind_id,
                });
                arg_types.push(t.clone());
                TailCallSlotKind::ValArray
            }
            RegionInputKind::Variant(t) => {
                let cases = kernel_abi::variant_cases(t).ok_or_else(|| {
                    anyhow::anyhow!(
                        "RegionInputKind::Variant must carry a frozen variant \
                         Type (freeze invariant)"
                    )
                })?;
                sig.variant_params.push(VariantInput {
                    name: name.clone(),
                    cases,
                    bind_id,
                });
                arg_types.push(t.clone());
                TailCallSlotKind::Variant
            }
            RegionInputKind::Nullable(elem) => {
                sig.nullable_params.push(NullableInput {
                    name: name.clone(),
                    elem: elem.clone(),
                    bind_id,
                });
                arg_types.push(kernel_abi::nullable_type(elem.clone()));
                TailCallSlotKind::Nullable
            }
            RegionInputKind::String => {
                sig.string_params.push(StringInput { name: name.clone(), bind_id });
                arg_types.push(kernel_abi::string_type());
                TailCallSlotKind::String
            }
            RegionInputKind::Value(t) => {
                sig.value_params.push(ValueInput {
                    name: name.clone(),
                    typ: t.clone(),
                    bind_id,
                });
                arg_types.push(t.clone());
                TailCallSlotKind::Value
            }
        };
        sig.tail_call_slots.push(TailCallSlot { name, kind: slot_kind });
    }
    Ok((sig, arg_types))
}
