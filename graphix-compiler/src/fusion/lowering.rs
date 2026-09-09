//! Fusion analysis: fastcall-site discovery, lambda kernel signature
//! derivation, and the env-free type-ref expander. Code generation
//! lives with each node's `emit_clif`; this module supplies what those
//! emitters consume.

use crate::{
    BindId, ExecCtx, Node, NodeView, Refs, Rt, Update, UserEvent,
    env::Env,
    expr::{ExprId, ExprKind, ModPath},
    fusion::{
        self, FusionBlocker,
        kernel_abi::{
            self, AbiKind, KernelSig, Seen, abi_kind, freeze_for_abi_normalized,
            scalar_prim,
        },
    },
    node::{callsite::CallSite, lambda::GXLambda},
    profile::{self, Phase},
    typ::{FnArgKind, FnType, Type},
};
use arcstr::ArcStr;
use netidx_value::Value;
use poolshark::local::LPooled;

pub use kernel_abi::{KnownFusedFn, PrimType};

/// A lambda kernel signature, cached per monomorphization in
/// `FusionCtx::kernels`. `fn_name` is the symbol call sites resolve
/// against.
#[derive(Debug, Clone)]
pub struct CachedKernel {
    pub fn_name: ArcStr,
    pub kernel: std::sync::Arc<KernelSig>,
    pub signature: KnownFusedFn,
    /// Captured outer bindings, appended to the signature after the
    /// formals in this order; callers forward each capture's current value.
    pub captures: Vec<CaptureSlot>,
    /// The body references its own binding.
    pub is_rec: bool,
    /// The binding the kernel was built from; body emission recognises
    /// self-calls by it.
    pub self_bind: Option<BindId>,
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
    Fast(crate::FastFn),
    Typed(crate::TypedFastFn, Type),
}

/// The cast pseudo-site's typed fast fn: the same `cast_value` call
/// `TypeCast::update` makes on the node-walk.
fn cast_typed(env: &Env, target: &Type, args: &[Value]) -> Option<Value> {
    Some(target.cast_value(env, args[0].clone()))
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

/// Discover the fusable call sites in a subtree: every `CallSite` on a
/// builtin with a fast fn and every non-inline `cast<T>(x)`. Descent
/// is [`fusion::for_each_emitted_node`], so collection callbacks are
/// included and ordinary lambda bodies are not. Effects reject the
/// region immediately. Other unsupported sites are omitted and checked
/// by emission.
pub(crate) fn walk_node_for_builtin_calls<R: Rt, E: UserEvent>(
    node: &Node<R, E>,
    ctx: &ExecCtx<R, E>,
    out: &mut BuiltinCallDiscovery,
) -> Result<(), FusionBlocker> {
    let mut failure = None;
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
                try_register_cast(tc, out);
                return;
            }
            NodeView::Connect(_) | NodeView::ConnectDeref(_) => "connect is an effect",
            NodeView::Catch(_) => "catch installs an error handler",
            NodeView::SeqGuard(_) => "sequence guard keeps cross-cycle state",
            NodeView::Sample(_) if std::ptr::eq(n, node) => {
                "sample keeps cross-cycle state"
            }
            NodeView::Any(_) if std::ptr::eq(n, node) => {
                "any depends on partial argument delivery"
            }
            NodeView::ByRef(_) | NodeView::Deref(_) if std::ptr::eq(n, node) => {
                "references require the node-walk"
            }
            _ => return,
        };
        failure = Some(FusionBlocker { spec: n.spec().clone(), reason: reason.into() });
    });
    match failure {
        Some(failure) => Err(failure),
        None => Ok(()),
    }
}

/// Register a cast that is not emitted inline. The inline test here
/// must mirror `emit_cast_node`'s, or the site registers out of step
/// with emission.
fn try_register_cast<R: Rt, E: UserEvent>(
    tc: &crate::node::TypeCast<R, E>,
    out: &mut BuiltinCallDiscovery,
) {
    let source = tc.n.typ();
    if scalar_prim(source).is_some_and(|p| p.is_numeric())
        && PrimType::from_type(&tc.target).is_some_and(|p| p.is_numeric())
    {
        return;
    }
    let Some(arg_frozen) = freeze_for_abi_normalized(source) else { return };
    let Some(ret_frozen) = freeze_for_abi_normalized(&tc.typ) else { return };
    out.apply_sites.insert(
        tc.spec.id,
        BuiltinCallSiteInfo {
            marshal_args: vec![MarshalArg::Call(0)],
            arg_types: vec![arg_frozen],
            return_type: ret_frozen,
            dispatch: SiteDispatch::Typed(cast_typed, tc.target.clone()),
        },
    );
}

/// Register one CallSite as a fastcall site, if it qualifies. Types
/// come from the CallSite's resolved FnType and its compiled arg and
/// return nodes, never the AST, which is unresolved for generic
/// builtins.
fn try_register_builtin_call_from_callsite<R: Rt, E: UserEvent>(
    cs: &CallSite<R, E>,
    ctx: &ExecCtx<R, E>,
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
    // The CallSite's own scope, so a call inside a nested lambda body
    // resolves in its module, not the region root's.
    let (_, bind) = match ctx.env.lookup_bind(&cs.scope().lexical, path).ok().flatten() {
        Some(b) => b,
        None => return None,
    };
    let key = (bind.scope.clone(), bind.name.clone());
    let info = match ctx.builtin_bindings.get(&key) {
        Some(i) => i.clone(),
        None => return None,
    };
    let fastcall = match ctx.builtin_fastcall(info.name.as_str()) {
        Some(fastcall) => fastcall,
        None => {
            return Some(FusionBlocker {
                spec: apply_expr.clone(),
                reason: "builtin has no fast-call entry".into(),
            });
        }
    };
    // `cs.ftype()` is `None` when typecheck did not reach this site;
    // the binding's generic FnType then usually fails to freeze.
    let fn_type: std::sync::Arc<FnType> = match cs.ftype() {
        Some(ft) => std::sync::Arc::new(ft.resolve_tvars()),
        None => {
            let inner: &FnType = &info.typ;
            std::sync::Arc::new(inner.clone())
        }
    };
    let apply_id = apply_expr.id;
    let mut call_positional: smallvec::SmallVec<[usize; 8]> = smallvec::SmallVec::new();
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
    let mut marshal_args: Vec<MarshalArg> = Vec::new();
    let mut pos_iter = call_positional.iter().enumerate();
    for fa in fn_type.args.iter() {
        use FnArgKind;
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
                let kt = match kernel_abi::freeze_for_abi_normalized(&arg_typ) {
                    Some(t) => t,
                    None => return None,
                };
                arg_types.push(kt);
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
                let kt = match kernel_abi::freeze_for_abi_normalized(&arg_typ) {
                    Some(t) => t,
                    None => return None,
                };
                arg_types.push(kt);
                marshal_args.push(source);
            }
        }
    }
    let remaining: smallvec::SmallVec<[_; 8]> = pos_iter.collect();
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
            let kt = match kernel_abi::freeze_for_abi_normalized(&arg_typ) {
                Some(t) => t,
                None => return None,
            };
            arg_types.push(kt);
            marshal_args.push(MarshalArg::Call(*call_idx));
        }
    }
    if !call_labeled.is_empty() {
        return None;
    }
    let ret_typ = cs.typ().clone();
    let return_type = match kernel_abi::freeze_for_abi_normalized(&ret_typ) {
        Some(t) => t,
        None => return None,
    };
    if !arg_types.iter().all(|t| is_call_arg_supported(t)) {
        return None;
    }
    if !is_call_return_supported(&return_type) {
        return None;
    }
    let dispatch = match fastcall {
        crate::FastCall::Plain(f) => SiteDispatch::Fast(f),
        crate::FastCall::Typed(f) => SiteDispatch::Typed(f, ret_typ),
    };
    out.apply_sites.insert(
        apply_id,
        BuiltinCallSiteInfo { marshal_args, arg_types, return_type, dispatch },
    );
    None
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
    use NodeView;
    match node.view() {
        NodeView::Constant(c) => Some(c.value.clone()),
        NodeView::ExplicitParens(ep) => node_const_value(&ep.n),
        NodeView::Array(a) => const_valarray(&a.n),
        NodeView::ListLit(l) => const_valarray(&l.n).map(|v| match v {
            Value::Array(a) => {
                crate::node::collection::list::from_iter(a.iter().cloned())
            }
            v => v,
        }),
        NodeView::Tuple(t) => const_valarray(&t.n),
        NodeView::Map(m) => const_map(&m.keys, &m.vals),
        _ => None,
    }
}

/// Fold element nodes into a constant `Value::Array` (the runtime shape
/// of array and tuple literals), or `None` if any element isn't constant.
fn const_valarray<R: Rt, E: UserEvent>(elems: &[Node<R, E>]) -> Option<Value> {
    let mut vals: poolshark::local::LPooled<Vec<Value>> =
        poolshark::local::LPooled::take();
    for c in elems.iter() {
        vals.push(node_const_value(c)?);
    }
    Some(Value::Array(netidx_value::ValArray::from_iter_exact(vals.drain(..))))
}

/// Fold parallel key/value node slices into a constant `Value::Map`,
/// or `None` if any entry isn't constant.
pub(crate) fn const_map<R: Rt, E: UserEvent>(
    keys: &[Node<R, E>],
    vals: &[Node<R, E>],
) -> Option<Value> {
    if keys.len() != vals.len() {
        return None;
    }
    let mut map = netidx_value::Map::new();
    for (k, v) in keys.iter().zip(vals.iter()) {
        map.insert_cow(node_const_value(k)?, node_const_value(v)?);
    }
    Some(Value::Map(map))
}

/// Expand named types to their definitions through composites so
/// `abi_kind`/`freeze_for_abi` can classify a shape env-free. Abstract
/// types are leaves. A recursive expansion is left as-is (the kernel
/// then does not fuse); non-regular recursion is stopped by a length
/// backstop on the expansion chain. Truncation only ever de-fuses, so
/// the work budget is small.
pub(crate) fn expand_refs(typ: &Type, env: &Env) -> Type {
    let _profile = profile::phase(Phase::ExpandRefs);
    let cx =
        ResolveCx { budget: 2_048, size_cap: FUSION_SIZE_CAP, ..ResolveCx::default() };
    resolve_abstract_d(typ, env, None, &cx).unwrap_or_else(|| typ.clone())
}

/// No kernel encodes a type that unfolds past this; resolving further
/// is work the freeze would discard.
pub(crate) const FUSION_SIZE_CAP: u32 = 4_096;

/// State of one top-level resolve. Memo entries are keyed by the
/// expansion plus the `Seen` keys it consulted, so an entry is valid on
/// any path containing all its dependencies and a resolve costs the
/// number of distinct types, not paths.
struct ResolveCx {
    /// Ref expansions.
    memo: std::cell::RefCell<LPooled<Vec<MemoEntry>>>,
    /// Shared composite subtrees between expansions.
    nodes: std::cell::RefCell<LPooled<ahash::AHashMap<crate::typ::NormKey, NodeEntry>>>,
    /// `Seen` keys the current frame consulted; a frame's own key is
    /// not a dependency on its caller's path.
    consulted: std::cell::RefCell<LPooled<Vec<kernel_abi::ExpandKey>>>,
    /// The current frame's result was truncated and must not be memoized.
    poisoned: std::cell::Cell<bool>,
    expansions: std::cell::Cell<u32>,
    budget: u32,
    /// Output nodes so far, tree-wise; a memo hit charges its recorded
    /// size. Past `size_cap` the resolve stops.
    unfolded: std::cell::Cell<u32>,
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
    deps: Vec<(u64, kernel_abi::ExpandKey)>,
    resolved: Option<Type>,
    size: u32,
}

struct FrameResult {
    deps: Vec<(u64, kernel_abi::ExpandKey)>,
    poisoned: bool,
    size: u32,
}

#[derive(Clone)]
struct MemoEntry {
    /// [`expand_key_fp`] of `key`, compared before the key itself.
    fp: u64,
    key: kernel_abi::ExpandKey,
    deps: Vec<(u64, kernel_abi::ExpandKey)>,
    /// `None` when nothing beneath the ref resolved: the ref stays opaque.
    resolved: Option<Type>,
    /// Tree-wise node count of `resolved`, charged on every hit.
    size: u32,
}

use kernel_abi::expand_key_fp;

impl std::fmt::Debug for MemoEntry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "MemoEntry(size {})", self.size)
    }
}

fn key_closed(key: &kernel_abi::ExpandKey) -> bool {
    let kernel_abi::ExpandKey::Ref(tr) = key;
    tr.params.iter().all(|t| t.tvar_free())
}

impl ResolveCx {
    /// Record that the current frame's result depends on `key` being on
    /// the path.
    fn consult(&self, key: &kernel_abi::ExpandKey) {
        let mut cur = self.consulted.borrow_mut();
        if !cur.contains(key) {
            cur.push(key.clone());
        }
    }

    fn lookup(
        &self,
        key: &kernel_abi::ExpandKey,
        seen: Option<&Seen>,
    ) -> Option<Option<Type>> {
        let fp = expand_key_fp(key);
        let hit = |e: &MemoEntry| {
            e.fp == fp
                && &e.key == key
                && e.deps.iter().all(|(fp, d)| Seen::contains_fp(seen, *fp, d))
        };
        let entry = self.memo.borrow().iter().find(|e| hit(e))?.clone();
        self.count_nodes(entry.size);
        for (_, d) in &entry.deps {
            self.consult(d);
        }
        Some(entry.resolved)
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
        if !e.deps.iter().all(|(fp, d)| Seen::contains_fp(seen, *fp, d)) {
            return None;
        }
        let resolved = e.resolved.clone();
        let deps = e.deps.clone();
        let size = e.size;
        drop(nodes);
        self.count_nodes(size);
        for (_, d) in &deps {
            self.consult(d);
        }
        Some(resolved)
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
        let deps: Vec<(u64, kernel_abi::ExpandKey)> =
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
        key: kernel_abi::ExpandKey,
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
        deps.retain(|k| k != &key);
        // An unbound tvar param compares equal to any other unbound
        // cell, so an open key would hand one site's cells to another.
        if memoize && !poisoned && key_closed(&key) {
            let entry = MemoEntry {
                fp: expand_key_fp(&key),
                key,
                deps: deps.iter().map(|k| (expand_key_fp(k), k.clone())).collect(),
                resolved: resolved.clone(),
                size: self.unfolded.get() - unfolded_before,
            };
            self.memo.borrow_mut().push(entry);
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
fn resolve_abstract_d<'a>(
    typ: &Type,
    env: &Env,
    seen: Option<&'a Seen<'a>>,
    cx: &ResolveCx,
) -> Option<Type> {
    use kernel_abi::Seen;
    // Bounds distinct expansions on one path, which is what stops
    // non-regular recursion; structural depth is not counted.
    if Seen::len(seen) > 256 {
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
    let (r, frame) = cx.node_frame(|cx| resolve_abstract_node(typ, env, seen, cx));
    if let Some(k) = nkey
        && !frame.poisoned
    {
        cx.nodes.borrow_mut().insert(
            k,
            NodeEntry { deps: frame.deps, resolved: r.clone(), size: frame.size },
        );
    }
    r
}

fn resolve_abstract_node<'a>(
    typ: &Type,
    env: &Env,
    seen: Option<&'a Seen<'a>>,
    cx: &ResolveCx,
) -> Option<Type> {
    use kernel_abi::{ExpandKey, Seen};
    match typ {
        // The inner type is cloned out so the tvar's read guard is not
        // held across `lookup_ref`'s lock acquisitions (a deadlock under
        // concurrent compiles). A deref is not an expansion: `seen`
        // passes through.
        Type::TVar(_) => match typ.deref_cloned() {
            Some(t) => Some(resolve_abstract_d(&t, env, seen, cx).unwrap_or(t)),
            None => None,
        },
        Type::Ref(tr) => {
            let key = ExpandKey::Ref(tr.clone());
            if Seen::contains(seen, &key) {
                cx.consult(&key);
                return None;
            }
            if let Some(t) = cx.lookup(&key, seen) {
                return t;
            }
            if !cx.charge() {
                return None;
            }
            match typ.lookup_ref(env) {
                Ok(resolved) => cx.expand(key, true, || {
                    let node = Seen::push(seen, ExpandKey::Ref(tr.clone()));
                    Some(
                        resolve_abstract_d(&resolved, env, Some(&node), cx)
                            .unwrap_or(resolved),
                    )
                }),
                _ => None,
            }
        }
        Type::Abstract { .. } => None,
        t => t.cow_children(&mut |c| resolve_abstract_d(c, env, seen, cx)),
    }
}

/// The catch-coverage fingerprint of a lambda instance's body: each
/// `?`/`$` site's resolved handler bind in node-visit order (`u64::MAX`
/// = handler-less). Part of the kernel cache key, because a kernel
/// bakes its handler ids into its deliver sites.
pub(crate) type QopCoverage = smallvec::SmallVec<[u64; 4]>;

/// The lambda-resolution fingerprint of a lambda instance's body: per
/// statically-resolved call site, the callee's `LambdaId` and the
/// identity of each fn-typed argument forwarded (`u64::MAX` when
/// unresolvable). Part of the kernel cache key, because a kernel bakes
/// those resolutions as CLIF calls.
pub(crate) type FnResolutions = smallvec::SmallVec<[u64; 4]>;

fn body_fingerprint<R: Rt, E: UserEvent>(
    body: &Node<R, E>,
    ec: &ExecCtx<R, E>,
) -> (QopCoverage, FnResolutions) {
    let mut cov = QopCoverage::new();
    let mut res = FnResolutions::new();
    crate::fusion::for_each_node(body, &mut |n| match n.view() {
        NodeView::Qop(q) => match q.handler.as_ref().map(|h| h.id()) {
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
    t.with_deref(|t| match t {
        Some(Type::Fn(_)) => true,
        Some(r @ Type::Ref(_)) => {
            expand_refs(r, env).with_deref(|t| matches!(t, Some(Type::Fn(_))))
        }
        _ => false,
    })
}

/// Per formal: is it passed unchanged (a `Ref` to its own binding) by
/// every self-call in the body? Such a formal needs no rebind slot.
/// Vacuously true with no self-calls; a destructured formal is never
/// invariant. Nested lambda bodies are not walked.
pub(crate) fn invariant_formals<R: Rt, E: UserEvent>(
    g: &GXLambda<R, E>,
    self_bind: Option<BindId>,
) -> smallvec::SmallVec<[bool; 8]> {
    let ids: smallvec::SmallVec<[Option<BindId>; 8]> =
        g.args().iter().map(|p| p.single_bind_id()).collect();
    let mut inv: smallvec::SmallVec<[bool; 8]> =
        ids.iter().map(|id| id.is_some()).collect();
    let Some(sb) = self_bind else { return inv };
    enum ALook {
        Pos(usize),
        Named(ArcStr),
    }
    let looks: smallvec::SmallVec<[ALook; 8]> = {
        let mut p = 0usize;
        g.typ()
            .args
            .iter()
            .map(|fa| match &fa.kind {
                FnArgKind::Positional { .. } => {
                    let k = p;
                    p += 1;
                    ALook::Pos(k)
                }
                FnArgKind::Labeled { name, .. } => ALook::Named(name.clone()),
            })
            .collect()
    };
    fusion::for_each_node(g.body(), &mut |n| {
        let NodeView::CallSite(cs) = n.view() else { return };
        if !matches!(cs.fnode().view(), NodeView::Ref(r) if r.id == sb) {
            return;
        }
        for i in 0..inv.len() {
            if !inv[i] {
                continue;
            }
            let arg = match &looks[i] {
                ALook::Pos(p) => cs.arg_positional(*p),
                ALook::Named(name) => cs.arg_named(name),
            };
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

/// Build, or fetch from the per-`ExecCtx` cache, the kernel signature
/// for the lambda `g` at the call site's resolved type. Signature
/// derivation only: the body is validated by the compile attempt.
/// `None` means the lambda has no kernel-representable signature and
/// its call sites node-walk. A cache hit returns the first builder's
/// `fn_name`.
pub(crate) fn build_lambda_kernel<R: Rt, E: UserEvent>(
    g: &GXLambda<R, E>,
    site_ftype: &FnType,
    kernel_name: &ArcStr,
    ec: &mut ExecCtx<R, E>,
) -> Option<CachedKernel> {
    let self_bind = g.self_bind();
    // A collection-bodied lambda is inline-emitted at its call sites; a
    // standalone kernel for it would fail to define.
    if matches!(g.body().view(), NodeView::MapQ(_) | NodeView::FoldQ(_)) {
        return None;
    }
    // The key is the site's type, not `g.typ()`: the instance shares
    // tvar cells with the def, so `g.typ()` reports whichever
    // monomorphization unified first.
    let resolved_typ = std::sync::Arc::new(site_ftype.resolve_tvars());
    let (coverage, fn_resolutions) = body_fingerprint(g.body(), ec);
    let key = (g.id(), resolved_typ, coverage, fn_resolutions);
    if let Some(cached) = ec.fusion.kernels.lock().get(&key).cloned() {
        return Some(cached);
    }
    let mut discovery = BuiltinCallDiscovery::default();
    walk_node_for_builtin_calls(g.body(), ec, &mut discovery).ok()?;
    // Kernel names are module-wide, so each coverage variant needs its
    // own symbol; the suffix is deterministic in compile order.
    let variant = {
        let kernels = ec.fusion.kernels.lock();
        kernels
            .range((key.0, key.1.clone(), QopCoverage::new(), FnResolutions::new())..)
            .take_while(|((id, ft, _, _), _)| *id == key.0 && *ft == key.1)
            .count()
    };
    let kernel_name: ArcStr = if variant == 0 {
        kernel_name.clone()
    } else {
        compact_str::format_compact!("{kernel_name}__cov{variant}").as_str().into()
    };
    let kernel_name = &kernel_name;
    // Emission reads the instance's node types, so an instance that
    // disagrees with the site would emit a kernel whose CLIF types
    // mismatch the call. Constraint lists differ benignly; compare only
    // args, vargs and return.
    {
        let gt = g.typ().resolve_tvars();
        let st = &*key.1;
        let args_agree = gt.args.len() == st.args.len()
            && gt.args.iter().zip(st.args.iter()).all(|(a, b)| {
                // An fn-typed arg is never a value slot and is keyed by
                // `FnResolutions`; its `throws` differs benignly here.
                if is_fn_shaped(&a.typ, &ec.env) || is_fn_shaped(&b.typ, &ec.env) {
                    is_fn_shaped(&a.typ, &ec.env) && is_fn_shaped(&b.typ, &ec.env)
                } else {
                    a.typ == b.typ
                }
            });
        if !args_agree || gt.vargs != st.vargs || gt.rtype != st.rtype {
            log::trace!(
                "build_lambda_kernel: site mono {} disagrees with lambda \
                 instance {} — refusing (site node-walks)",
                st,
                gt
            );
            return None;
        }
    }
    // Mutual recursion would re-enter this build forever (the cache
    // entry lands only on completion); a re-entered build refuses and
    // the chain de-fuses.
    struct BuildingGuard(triomphe::Arc<parking_lot::Mutex<nohash::IntSet<u64>>>, u64);
    impl Drop for BuildingGuard {
        fn drop(&mut self) {
            self.0.lock().remove(&self.1);
        }
    }
    let lid = g.id().inner();
    if !ec.fusion.building.lock().insert(lid) {
        return None;
    }
    let _building = BuildingGuard(ec.fusion.building.clone(), lid);
    // Slots carry the formal's BindId because a declared fn type's
    // parameter names may differ from the lambda literal's own; body
    // `Ref`s resolve by id first.
    let typ = g.typ();
    let mut inputs: LPooled<Vec<(ArcStr, RegionInputKind, Option<BindId>)>> =
        LPooled::take();
    let mut formal_slot_types_by_position: LPooled<Vec<(usize, Type)>> = LPooled::take();
    let inv = invariant_formals(g, self_bind);
    let mut skipped_args: Vec<u32> = Vec::new();
    for (i, fa) in typ.args.iter().enumerate() {
        let name = match &fa.kind {
            FnArgKind::Positional { name: Some(n) } => n.clone(),
            FnArgKind::Labeled { name, .. } => name.clone(),
            _ => return None,
        };
        let arg_typ = expand_refs(&fa.typ, &ec.env);
        // An invariant fn-typed formal drops out of the signature: its
        // uses are statically-resolved calls, and a body that needs it
        // as a value fails to emit. A rebind slot cannot carry a lambda.
        if is_fn_shaped(&arg_typ, &ec.env) {
            if inv[i] && matches!(fa.kind, FnArgKind::Positional { .. }) {
                skipped_args.push(i as u32);
                continue;
            }
            return None;
        }
        let kt = kernel_abi::freeze_for_abi_normalized(&arg_typ)?;
        let kind = type_to_region_input_kind(kt.clone())?;
        let id = g.args().get(i).and_then(|p| p.single_bind_id());
        if id.is_none() {
            // A destructured formal refuses: a name-only slot would let
            // a same-named body leaf resolve to the whole composite.
            let mut has_ids = false;
            if let Some(p) = g.args().get(i) {
                p.ids(&mut |_| has_ids = true);
            }
            if has_ids {
                return None;
            }
        }
        inputs.push((name, kind, id));
        formal_slot_types_by_position.push((i, kt));
    }
    let tail_invariant: Vec<u32> = inv
        .iter()
        .enumerate()
        .filter(|(i, v)| **v && !skipped_args.contains(&(*i as u32)))
        .map(|(i, _)| i as u32)
        .collect();
    // Formal patterns bind outside the body, so `refs` reports them as
    // external; they are excluded from the captures.
    let mut arg_ids: LPooled<nohash::IntSet<BindId>> = LPooled::take();
    for pat in g.args() {
        pat.ids(&mut |id| {
            arg_ids.insert(id);
        });
    }
    let mut refs = Refs::default();
    g.body().refs(&mut refs);
    let mut external: LPooled<Vec<BindId>> = LPooled::take();
    refs.with_external_refs(|id| {
        if !arg_ids.contains(&id) {
            external.push(id);
        }
    });
    external.sort_by_key(|id| id.inner());
    let mut captures: Vec<CaptureSlot> = Vec::new();
    for bind_id in external.iter().copied() {
        // A self-reference lowers as a call, never as a value.
        if Some(bind_id) == self_bind {
            continue;
        }
        let b = ec.env.by_id.get(&bind_id)?;
        let cap_typ = b.typ.clone();
        // A statically-resolved fn capture is emitted as a call, not a
        // value slot; a body that needs it as a value fails to emit.
        if matches!(&cap_typ, Type::Fn(_)) {
            continue;
        }
        let kt = match kernel_abi::freeze_for_abi_normalized(&expand_refs(
            &cap_typ, &ec.env,
        )) {
            Some(t) => t,
            None => return None,
        };
        let kind = match type_to_region_input_kind(kt.clone()) {
            Some(k) => k,
            None => return None,
        };
        let name = ArcStr::from(b.name.as_str());
        inputs.push((name.clone(), kind, Some(bind_id)));
        captures.push(CaptureSlot { bind_id, name, typ: kt });
    }
    let return_typ =
        kernel_abi::freeze_for_abi_normalized(&expand_refs(&typ.rtype, &ec.env))?;
    // A unit-typed call has no value to return; bare Null should have
    // widened.
    if matches!(
        kernel_abi::abi_kind(&return_typ),
        Some(kernel_abi::AbiKind::Unit | kernel_abi::AbiKind::Null)
    ) {
        return None;
    }
    let is_rec = g.self_recursive();
    // Defense in depth behind static instance checking: a self-call
    // feeding a formal a differently-shaped value would marshal it under
    // the wrong ABI.
    if is_rec
        && let Some(sb) = self_bind
        && !self_calls_abi_consistent(g.body(), sb, &formal_slot_types_by_position, ec)
    {
        return None;
    }
    let has_tail = g.tail_loop();
    let (mut sig, arg_types) = match fusion::sig_from_inputs(
        kernel_name.clone(),
        inputs.iter().map(|(name, kind, bind_id)| (name.clone(), kind, *bind_id)),
        return_typ.clone(),
    ) {
        Ok(v) => v,
        Err(e) => {
            log::trace!("build_lambda_kernel: sig_from_inputs failed: {e:#}");
            return None;
        }
    };
    sig.has_tail_loop = has_tail;
    sig.skipped_args = skipped_args;
    sig.tail_invariant = tail_invariant;
    if crate::dbgenv::graphix_dbg_kernels() {
        crate::format_with_flags(crate::PrintFlag::DerefTVars, || {
            eprintln!(
                "KERNEL BUILT {kernel_name}: ret={return_typ} kind={:?}",
                kernel_abi::abi_kind(&return_typ)
            );
            Ok::<_, anyhow::Error>(())
        })
        .unwrap();
    }
    let signature = KnownFusedFn { arg_types, return_type: return_typ, self_bind };
    let cached = CachedKernel {
        fn_name: kernel_name.clone(),
        kernel: std::sync::Arc::new(sig),
        signature,
        captures,
        is_rec,
        self_bind,
        apply_sites: discovery.apply_sites,
    };
    ec.fusion.kernels.lock().insert(key, cached.clone());
    Some(cached)
}

/// The structural tail-loop predicate shared by the JIT's native-loop
/// gate and the interpreter's `tail_loop` gate, so both backends loop
/// the same lambdas: `g` is self-recursive through `self_bind`, every
/// formal is positional, every loop-carried formal is a kernel-encodable
/// kind, and a self-call sits in tail position. Not sync-gated; the
/// interpreter's `analyze` adds that gate.
pub(crate) fn structural_tail_loop<R: Rt, E: UserEvent>(
    g: &GXLambda<R, E>,
    self_bind: BindId,
    ec: &ExecCtx<R, E>,
) -> bool {
    let mut refs = Refs::default();
    g.body().refs(&mut refs);
    let mut is_rec = false;
    refs.with_external_refs(|id| {
        if id == self_bind {
            is_rec = true;
        }
    });
    if !is_rec {
        return false;
    }
    // The rebind maps the self-call's positional args 1:1 onto the formals.
    if g.typ().vargs.is_some() {
        return false;
    }
    let inv = invariant_formals(g, Some(self_bind));
    for (i, fa) in g.typ().args.iter().enumerate() {
        if !matches!(fa.kind, FnArgKind::Positional { .. }) {
            return false;
        }
        if inv[i] {
            continue;
        }
        let arg_typ = expand_refs(&fa.typ, &ec.env);
        let kt = match kernel_abi::freeze_for_abi_normalized(&arg_typ) {
            Some(t) => t,
            None => return false,
        };
        if type_to_region_input_kind(kt).is_none() {
            return false;
        }
    }
    body_has_self_tail_call(g.body(), self_bind)
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

/// Does every self-call in the body feed each positional formal a value
/// whose frozen type fits that formal's slot type? Conservative: an
/// unfreezable arg or a self-call that does not map 1:1 onto the
/// formals counts as inconsistent. Nested lambda bodies are not walked.
fn self_calls_abi_consistent<R: Rt, E: UserEvent>(
    body: &Node<R, E>,
    self_bind: BindId,
    formal_slot_types_by_position: &[(usize, Type)],
    ec: &ExecCtx<R, E>,
) -> bool {
    let mut ok = true;
    fusion::for_each_node(body, &mut |n| {
        if !ok {
            return;
        }
        let NodeView::CallSite(cs) = n.view() else { return };
        if !matches!(cs.fnode().view(), NodeView::Ref(r) if r.id == self_bind) {
            return;
        }
        for (i, formal_kt) in formal_slot_types_by_position.iter() {
            let Some(arg) = cs.arg_positional(*i) else {
                ok = false;
                return;
            };
            let Some(arg_kt) =
                kernel_abi::freeze_for_abi_normalized(&expand_refs(arg.typ(), &ec.env))
            else {
                ok = false;
                return;
            };
            if formal_kt.check_contains(&ec.env, &arg_kt).is_err() {
                ok = false;
                return;
            }
        }
    });
    ok
}

/// Kernel input slot classification, the source of a
/// [`kernel_abi::KernelParam`]'s `ParamKind`. Function-typed inputs are
/// not value slots. Each carried `Type` is frozen.
#[derive(Debug, Clone)]
pub enum RegionInputKind {
    Prim(PrimType),
    /// Carries the element type.
    Array(Type),
    /// Carries the full tuple type.
    Tuple(Type),
    /// Carries the full struct type.
    Struct(Type),
    /// Carries the full variant type (a `Variant` or a `Set` of them).
    Variant(Type),
    /// `[T, null]`; carries `T`.
    Nullable(Type),
    String,
    /// Any other value shape; carries the full type.
    Value(Type),
}

/// Classify a value [`Type`] as a kernel input. `None` for `Unit`, bare
/// `Null` (widened to `Nullable<T>` at construction) and fn types.
pub(crate) fn type_to_region_input_kind(t: Type) -> Option<RegionInputKind> {
    use AbiKind;
    // The stored type must be frozen: the non-derefing accessors
    // (`tuple_slots`, `struct_fields`, `array_elem`) run on it later.
    let t = kernel_abi::freeze_for_abi(&t)?;
    match abi_kind(&t)? {
        AbiKind::Scalar(p) => Some(RegionInputKind::Prim(p)),
        AbiKind::Array => {
            kernel_abi::array_elem(&t).map(|e| RegionInputKind::Array(e.clone()))
        }
        AbiKind::Tuple => Some(RegionInputKind::Tuple(t)),
        AbiKind::Struct => Some(RegionInputKind::Struct(t)),
        AbiKind::Variant => Some(RegionInputKind::Variant(t)),
        AbiKind::Nullable => {
            kernel_abi::nullable_inner(&t).map(RegionInputKind::Nullable)
        }
        AbiKind::String => Some(RegionInputKind::String),
        AbiKind::Value => Some(RegionInputKind::Value(t)),
        AbiKind::Unit | AbiKind::Null => None,
    }
}

/// A marshallable call argument shape: every fusable shape but `Unit`.
fn is_call_arg_supported(t: &Type) -> bool {
    use AbiKind;
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

/// True if `t` derefs to one of the single-bit `datetime`/`duration`
/// value-shape primitives.
pub(crate) fn is_datetime_or_duration(t: &Type) -> bool {
    t.with_deref(|r| match r {
        Some(Type::Primitive(p)) if p.iter().count() == 1 => {
            p.contains(netidx_value::Typ::DateTime)
                || p.contains(netidx_value::Typ::Duration)
        }
        _ => false,
    })
}

/// A marshallable call return shape: every fusable shape but bare `Null`.
fn is_call_return_supported(t: &Type) -> bool {
    use AbiKind;
    match abi_kind(t) {
        Some(
            AbiKind::Scalar(_)
            | AbiKind::Array
            | AbiKind::Tuple
            | AbiKind::Struct
            | AbiKind::Variant
            | AbiKind::Nullable
            | AbiKind::Value
            | AbiKind::Unit
            | AbiKind::String,
        ) => true,
        Some(AbiKind::Null) | None => false,
    }
}
