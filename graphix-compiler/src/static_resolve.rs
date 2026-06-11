//! Static call resolution.
//!
//! Post-typecheck compiler pass that pre-binds CallSites whose
//! function expression can be proven to always resolve to a single
//! known LambdaDef. For each such CallSite we call `LambdaDef::init`
//! at compile time (instead of lazily on first runtime update),
//! storing the resulting `Apply` on `CallSite::function`.
//!
//! Two wins:
//!
//! 1. **Interpreter**: every dynamic call site pays `fnode.update`
//!    + Value equality + optional downcast on every invocation, and
//!    a one-shot `bind()` call on first invocation. Pre-resolving
//!    eliminates the first two for the rest of the program's life
//!    and runs `bind()` once at compile time.
//!
//! 2. **Fusion (future)**: the pre-bound `Box<dyn Apply>` is a
//!    `GXLambda<R, E>` for user lambdas — and `GXLambda` exposes
//!    the compiled body `Node<R, E>` as a struct field. Fusion's
//!    NodeView walker can then descend through a resolved CallSite
//!    straight into the body Node, eliminating today's
//!    `LambdaBind`-as-fusion-candidate special case.
//!
//! ## Resolvability
//!
//! A CallSite is statically resolvable when its function expression
//! is either:
//!
//! - A `Ref` to a `BindId` whose value Node is a `Lambda` AND the
//!   BindId is NOT in `ctx.unstable_bindings` (no `Connect` (`<-`)
//!   targeting it). This is the common case for stdlib calls
//!   (`array::map`, `str::trim`, etc.) and user `let f = |…| …; f(…)`.
//!
//! - A direct lambda literal: `(|x| x + 1)(42)`. The CallSite's
//!   `fnode` is a `Lambda` Node directly, with no intervening Ref.
//!
//! Anything else — function-typed parameters (HOF callbacks),
//! conditional function expressions, etc. — stays dynamic and goes
//! through the existing bind-on-first-call path.

use crate::{
    node::{bind::Bind, callsite::CallSite, lambda::LambdaDef},
    typ::Type,
    BindId, ExecCtx, Node, NodeView, Rt, StaticFnArg, Update, UserEvent,
};
use anyhow::Result;
use netidx::subscriber::Value;
use nohash::IntMap;
use std::any::Any;

/// Run the static call resolution pass.
///
/// Called by [`crate::compile`] between the deferred-checks fixpoint
/// and `fusion::fuse`. Walks the typed Node tree, pre-binds every
/// statically resolvable CallSite, and leaves the rest untouched.
///
/// Returns the first error encountered (e.g. an InitFn failure for
/// a lambda whose body has a static error). Caller is expected to
/// have already typechecked successfully, so errors here are
/// unusual.
pub fn resolve_static_calls<R: Rt, E: UserEvent>(
    ctx: &mut ExecCtx<R, E>,
    root: &mut Node<R, E>,
) -> Result<()> {
    // Pass 1: build the BindId → LambdaDef Value map by walking the
    // tree immutably via NodeView. A Bind whose value Node is a
    // Lambda contributes one entry per bind_id its pattern
    // introduces (almost always one — you can't usefully
    // destructure a lambda value).
    let mut bind_to_lambda: IntMap<BindId, Value> = IntMap::default();
    collect_lambda_binds::<R, E>(root, &mut bind_to_lambda);

    // Pass 2: walk mutably, identifying resolvable CallSites and
    // calling `resolve_static` on each. Uses `Any` downcasting at
    // each step — Update's supertrait list includes `Any` so trait
    // upcasting to `&mut dyn Any` is stable.
    visit_mut::<R, E>(root, &mut |node| {
        try_resolve_callsite::<R, E>(node, &bind_to_lambda, ctx)
    })
}

/// Walk the tree, populate `out` with every (bind_id → Lambda's
/// stored Value) pair we find. Bindings whose value Node isn't a
/// Lambda are skipped.
///
/// The match is EXHAUSTIVE over [`NodeView`] on purpose — this is the
/// canonical child-enumeration for static resolution (#204): a new
/// node variant fails to compile here instead of silently not being
/// traversed. [`visit_mut`] must descend exactly the same children;
/// when the compiler sends you here for a new variant, extend both.
fn collect_lambda_binds<R: Rt, E: UserEvent>(
    node: &Node<R, E>,
    out: &mut IntMap<BindId, Value>,
) {
    macro_rules! rec {
        ($($n:expr),*) => {{ $(collect_lambda_binds::<R, E>($n, out);)* }};
    }
    match node.view() {
        NodeView::Bind(b) => {
            if let NodeView::Lambda(l) = b.node.view() {
                let fv = l.def_value().clone();
                b.pattern.ids(&mut |id| {
                    out.insert(id, fv.clone());
                });
            }
            // The Bind's value Node could itself contain Lambdas
            // (e.g. `let _ = { let f = |x| x; ... };`) — descend.
            rec!(&b.node)
        }
        NodeView::Module(m) => {
            for child in m.nodes.iter() {
                rec!(child)
            }
            // Interface re-exports: a signed module proxies each impl
            // binding's value to its public `val` signature binding.
            // Callers reference the *signature* binding's BindId, so
            // map it to the same LambdaDef the impl binding has. The
            // impl Bind→Lambda entries were just collected above (the
            // impl bindings are this module's children), so the
            // `out.get(impl_id)` lookup sees them. `proxy()` maps
            // impl_id → sig_id.
            for (impl_id, sig_id) in m.proxy() {
                if let Some(fv) = out.get(impl_id).cloned() {
                    out.insert(*sig_id, fv);
                }
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
        // A lambda's body compiles per call site — a Bind inside it
        // belongs to that body's own resolution pass (#203).
        NodeView::Lambda(_) => {}
        // Leaves.
        NodeView::Ref(_)
        | NodeView::Constant(_)
        | NodeView::Use(_)
        | NodeView::TypeDef(_)
        | NodeView::Nop(_) => {}
        // Synthetic, produced by fusion — can't exist during this
        // pre-fusion pass.
        NodeView::FusedKernel(_) => {}
    }
}

/// Recursively visit every Node via direct mutable access,
/// dispatching `f` on each. Container Nodes recurse into their
/// children after `f` sees them.
///
/// Descends every child-bearing node EXCEPT `Lambda` (a body compiles
/// per call site — #203 territory) and `FusedKernel` (synthetic,
/// post-fusion). The covered set must stay identical to
/// [`collect_lambda_binds`]'s exhaustive `NodeView` match — that match
/// is the compiler-checked enumeration; when a new variant lands
/// there, add its downcast arm here.
fn visit_mut<R: Rt, E: UserEvent>(
    node: &mut Node<R, E>,
    f: &mut dyn FnMut(&mut Node<R, E>) -> Result<()>,
) -> Result<()> {
    f(node)?;
    // Dispatch on concrete type via Any-downcast. Each arm gives
    // mutable access to the container's children.
    let upd: &mut dyn Update<R, E> = &mut **node;
    let any: &mut dyn Any = upd;
    // Binary ops: lhs/rhs Cached children.
    macro_rules! binop {
        ($($t:ty),* $(,)?) => {
            $(if let Some(n) = any.downcast_mut::<$t>() {
                visit_mut::<R, E>(&mut n.lhs.node, f)?;
                return visit_mut::<R, E>(&mut n.rhs.node, f);
            })*
        };
    }
    // Single `n: Node` child.
    macro_rules! single_n {
        ($($t:ty),* $(,)?) => {
            $(if let Some(x) = any.downcast_mut::<$t>() {
                return visit_mut::<R, E>(&mut x.n, f);
            })*
        };
    }
    // A `n: Box<[Cached]>` child list.
    macro_rules! cached_list {
        ($($t:ty),* $(,)?) => {
            $(if let Some(x) = any.downcast_mut::<$t>() {
                for c in x.n.iter_mut() {
                    visit_mut::<R, E>(&mut c.node, f)?;
                }
                return Ok(());
            })*
        };
    }
    if let Some(m) = any.downcast_mut::<crate::node::module::Module<R, E>>() {
        for child in m.nodes.iter_mut() {
            visit_mut::<R, E>(child, f)?;
        }
        return Ok(());
    }
    if let Some(b) = any.downcast_mut::<crate::node::Block<R, E>>() {
        for child in b.children.iter_mut() {
            visit_mut::<R, E>(child, f)?;
        }
        return Ok(());
    }
    if let Some(b) = any.downcast_mut::<Bind<R, E>>() {
        visit_mut::<R, E>(&mut b.node, f)?;
        return Ok(());
    }
    if let Some(cs) = any.downcast_mut::<CallSite<R, E>>() {
        for arg in cs.args.values_mut() {
            if let Some(ref mut n) = arg.node {
                visit_mut::<R, E>(n, f)?;
            }
        }
        visit_mut::<R, E>(&mut cs.fnode, f)?;
        return Ok(());
    }
    if let Some(s) = any.downcast_mut::<crate::node::select::Select<R, E>>() {
        visit_mut::<R, E>(&mut s.arg.node, f)?;
        for (pat, body) in s.arms.iter_mut() {
            if let Some(g) = &mut pat.guard {
                visit_mut::<R, E>(&mut g.node, f)?;
            }
            visit_mut::<R, E>(&mut body.node, f)?;
        }
        return Ok(());
    }
    if let Some(t) = any.downcast_mut::<crate::node::error::TryCatch<R, E>>() {
        for n in t.nodes.iter_mut() {
            visit_mut::<R, E>(n, f)?;
        }
        return visit_mut::<R, E>(&mut t.handler, f);
    }
    single_n!(
        crate::node::error::Qop<R, E>,
        crate::node::error::OrNever<R, E>,
        crate::node::ExplicitParens<R, E>,
        crate::node::TypeCast<R, E>,
        crate::node::op::Not<R, E>,
    );
    if let Some(c) = any.downcast_mut::<crate::node::Connect<R, E>>() {
        return visit_mut::<R, E>(&mut c.node, f);
    }
    if let Some(c) = any.downcast_mut::<crate::node::ConnectDeref<R, E>>() {
        return visit_mut::<R, E>(&mut c.rhs.node, f);
    }
    if let Some(s) = any.downcast_mut::<crate::node::StringInterpolate<R, E>>()
    {
        for a in s.args.iter_mut() {
            visit_mut::<R, E>(&mut a.node, f)?;
        }
        return Ok(());
    }
    if let Some(a) = any.downcast_mut::<crate::node::Any<R, E>>() {
        for n in a.n.iter_mut() {
            visit_mut::<R, E>(n, f)?;
        }
        return Ok(());
    }
    if let Some(s) = any.downcast_mut::<crate::node::Sample<R, E>>() {
        visit_mut::<R, E>(&mut s.trigger, f)?;
        return visit_mut::<R, E>(&mut s.arg.node, f);
    }
    cached_list!(
        crate::node::data::Struct<R, E>,
        crate::node::data::Tuple<R, E>,
        crate::node::data::Variant<R, E>,
        crate::node::array::Array<R, E>,
    );
    if let Some(s) = any.downcast_mut::<crate::node::data::StructWith<R, E>>()
    {
        visit_mut::<R, E>(&mut s.source, f)?;
        for r in s.replace.iter_mut() {
            visit_mut::<R, E>(&mut r.n.node, f)?;
        }
        return Ok(());
    }
    if let Some(m) = any.downcast_mut::<crate::node::map::Map<R, E>>() {
        for c in m.keys.iter_mut() {
            visit_mut::<R, E>(&mut c.node, f)?;
        }
        for c in m.vals.iter_mut() {
            visit_mut::<R, E>(&mut c.node, f)?;
        }
        return Ok(());
    }
    if let Some(s) = any.downcast_mut::<crate::node::data::StructRef<R, E>>() {
        return visit_mut::<R, E>(&mut s.source, f);
    }
    if let Some(t) = any.downcast_mut::<crate::node::data::TupleRef<R, E>>() {
        return visit_mut::<R, E>(&mut t.source, f);
    }
    if let Some(a) = any.downcast_mut::<crate::node::array::ArrayRef<R, E>>() {
        visit_mut::<R, E>(&mut a.source.node, f)?;
        return visit_mut::<R, E>(&mut a.i.node, f);
    }
    if let Some(a) = any.downcast_mut::<crate::node::array::ArraySlice<R, E>>()
    {
        visit_mut::<R, E>(&mut a.source.node, f)?;
        if let Some(s) = &mut a.start {
            visit_mut::<R, E>(&mut s.node, f)?;
        }
        if let Some(e) = &mut a.end {
            visit_mut::<R, E>(&mut e.node, f)?;
        }
        return Ok(());
    }
    if let Some(m) = any.downcast_mut::<crate::node::map::MapRef<R, E>>() {
        visit_mut::<R, E>(&mut m.source.node, f)?;
        return visit_mut::<R, E>(&mut m.key.node, f);
    }
    if let Some(b) = any.downcast_mut::<crate::node::bind::ByRef<R, E>>() {
        return visit_mut::<R, E>(&mut b.child, f);
    }
    if let Some(d) = any.downcast_mut::<crate::node::bind::Deref<R, E>>() {
        return visit_mut::<R, E>(&mut d.child, f);
    }
    binop!(
        crate::node::op::Add<R, E>,
        crate::node::op::Sub<R, E>,
        crate::node::op::Mul<R, E>,
        crate::node::op::Div<R, E>,
        crate::node::op::Mod<R, E>,
        crate::node::op::CheckedAdd<R, E>,
        crate::node::op::CheckedSub<R, E>,
        crate::node::op::CheckedMul<R, E>,
        crate::node::op::CheckedDiv<R, E>,
        crate::node::op::CheckedMod<R, E>,
        crate::node::op::Eq<R, E>,
        crate::node::op::Ne<R, E>,
        crate::node::op::Lt<R, E>,
        crate::node::op::Gt<R, E>,
        crate::node::op::Lte<R, E>,
        crate::node::op::Gte<R, E>,
        crate::node::op::And<R, E>,
        crate::node::op::Or<R, E>,
    );
    Ok(())
}

/// Inspect `node`; if it's a CallSite whose function resolves to a
/// known static lambda, pre-bind via `CallSite::resolve_static`.
///
/// We do the inspection in two steps to satisfy the borrow checker:
/// 1. Read-only downcast to capture the bind_id & fv we'd resolve.
/// 2. Mutable downcast to call `resolve_static` with `ctx`.
fn try_resolve_callsite<R: Rt, E: UserEvent>(
    node: &mut Node<R, E>,
    bind_to_lambda: &IntMap<BindId, Value>,
    ctx: &mut ExecCtx<R, E>,
) -> Result<()> {
    // Step 1: probe for a resolvable target without holding a
    // mutable borrow on the node.
    let target: Option<Value> = {
        let upd: &dyn Update<R, E> = &**node;
        let any: &dyn Any = upd;
        let Some(cs) = any.downcast_ref::<CallSite<R, E>>() else {
            return Ok(());
        };
        if cs.statically_resolved {
            return Ok(());
        }
        // The function expression must be a Ref whose bind_id is
        // bound to a Lambda, AND the bind_id must NOT be a Connect
        // target. Direct lambda-literal calls also count — the
        // fnode is a Lambda Node.
        match cs.fnode.view() {
            NodeView::Ref(r) => {
                if ctx.unstable_bindings.contains(&r.id) {
                    return Ok(());
                }
                // bind_to_lambda is built from the *fixture's* Bind
                // nodes. Stdlib packages are compiled separately, so a
                // call to e.g. `array::map` has no fixture-tree Bind —
                // fall back to the binding's cached value (the
                // LambdaDef persists in ctx.cached). The step-2
                // downcast filters out non-lambda cached values.
                bind_to_lambda
                    .get(&r.id)
                    .cloned()
                    .or_else(|| ctx.cached.get(&r.id).cloned())
            }
            NodeView::Lambda(l) => Some(l.def_value().clone()),
            _ => None,
        }
    };
    let Some(fv) = target else {
        return Ok(());
    };
    // Step 2: take a mutable borrow and resolve. The LambdaDef has
    // to be re-extracted from `fv` because we dropped the prior
    // borrow.
    let upd: &mut dyn Update<R, E> = &mut **node;
    let any: &mut dyn Any = upd;
    let Some(cs) = any.downcast_mut::<CallSite<R, E>>() else {
        return Ok(());
    };
    let Some(def) = fv.downcast_ref::<crate::node::lambda::LambdaDef<R, E>>() else {
        // Shouldn't happen — `bind_to_lambda` only stores Values we
        // pulled from `Lambda::def_value`, which always wrap a
        // LambdaDef. Belt-and-suspenders.
        return Ok(());
    };
    // Safety: lifetime juggling — `def` borrows `fv`, which we
    // own here. Call resolve_static while still holding fv.
    cs.resolve_static(ctx, def, fv.clone())?;
    // After resolve_static populates `cs.function`, walk the call
    // site's positional args: any whose formal type is `Type::Fn`
    // AND whose Node is statically resolvable to a single known
    // `LambdaDef` becomes a `StaticFnArg` entry. The just-resolved
    // Apply gets a chance to react via the new
    // `Apply::static_resolve_fn_args` hook — HOF builtins (MapQ,
    // FoldQ) use it to pre-materialize their callback Apply Nodes
    // so fusion's walker can later descend without LambdaDef
    // analysis.
    invoke_apply_fn_arg_hook::<R, E>(node, bind_to_lambda, ctx)?;
    Ok(())
}

/// Discovers the call site's statically-resolvable fn-typed args
/// and invokes `Apply::static_resolve_fn_args` on the just-bound
/// Apply. Called immediately after `cs.resolve_static` so the
/// builtin sees the same Apply instance that runtime will use.
fn invoke_apply_fn_arg_hook<R: Rt, E: UserEvent>(
    node: &mut Node<R, E>,
    bind_to_lambda: &IntMap<BindId, Value>,
    ctx: &mut ExecCtx<R, E>,
) -> Result<()> {
    // Step 1: read-only borrow to collect (arg_idx, fv) tuples.
    // We can't carry `&LambdaDef` borrows out of this scope because
    // `fv: &Value` aliases the call site's stored Value via the
    // Apply box; we re-extract LambdaDef inside step 2.
    let fn_arg_targets: Vec<(usize, Value)> = {
        let upd: &dyn Update<R, E> = &**node;
        let any: &dyn Any = upd;
        let Some(cs) = any.downcast_ref::<CallSite<R, E>>() else {
            return Ok(());
        };
        if !cs.statically_resolved {
            return Ok(());
        }
        let Some(ftype) = cs.resolved_ftype().or_else(|| cs.ftype()) else {
            return Ok(());
        };
        let mut out: Vec<(usize, Value)> = Vec::new();
        for (i, farg) in ftype.args.iter().enumerate() {
            // Only positional fn-typed formals. Labeled fn-typed
            // args (rare, but possible) are skipped for now — the
            // existing HOF builtins all take callbacks positionally.
            //
            // The formal may be a TVar bound to a Fn (the common case
            // at a monomorphized HOF call site), not a bare `Type::Fn`
            // — deref before checking.
            if !farg.typ.with_deref(|t| matches!(t, Some(Type::Fn(_)))) {
                continue;
            }
            let Some(arg_node) = cs.arg_positional(i) else {
                continue;
            };
            match arg_node.view() {
                NodeView::Lambda(l) => {
                    out.push((i, l.def_value().clone()));
                }
                NodeView::Ref(r) => {
                    if ctx.unstable_bindings.contains(&r.id) {
                        continue;
                    }
                    if let Some(fv) = bind_to_lambda.get(&r.id) {
                        out.push((i, fv.clone()));
                    }
                }
                _ => {}
            }
        }
        if out.is_empty() {
            return Ok(());
        }
        out
    };
    // Step 2: take a mutable borrow on the Apply via cs.function,
    // re-extract LambdaDef refs from the cloned Values, build the
    // borrowed StaticFnArg slice, invoke the hook.
    //
    // The Value -> LambdaDef downcast borrow chain requires the
    // Values to outlive the borrow of the slice we pass to the
    // hook; we keep them in `fn_arg_targets` for the duration.
    let upd: &mut dyn Update<R, E> = &mut **node;
    let any: &mut dyn Any = upd;
    let Some(cs) = any.downcast_mut::<CallSite<R, E>>() else {
        return Ok(());
    };
    // Extract the Apply from cs.function. We need exclusive access
    // for the hook; cs.function is `Option<(Value, Box<dyn Apply>)>`.
    let Some((_, apply)) = cs.function.as_mut() else {
        return Ok(());
    };
    let mut fn_args: Vec<StaticFnArg<'_, R, E>> = Vec::with_capacity(fn_arg_targets.len());
    for (idx, fv) in &fn_arg_targets {
        if let Some(def) = fv.downcast_ref::<LambdaDef<R, E>>() {
            fn_args.push(StaticFnArg { arg_idx: *idx, lambda: def });
        }
    }
    if fn_args.is_empty() {
        return Ok(());
    }
    apply.static_resolve_fn_args(ctx, &fn_args)?;
    Ok(())
}

