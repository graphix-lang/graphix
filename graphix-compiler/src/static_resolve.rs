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
fn collect_lambda_binds<R: Rt, E: UserEvent>(
    node: &Node<R, E>,
    out: &mut IntMap<BindId, Value>,
) {
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
            collect_lambda_binds::<R, E>(&b.node, out);
        }
        NodeView::Module(m) => {
            for child in m.nodes.iter() {
                collect_lambda_binds::<R, E>(child, out);
            }
        }
        NodeView::Block(blk) => {
            for child in blk.children.iter() {
                collect_lambda_binds::<R, E>(child, out);
            }
        }
        _ => {
            // Lambdas can only be bound at let-binding sites, which
            // are always `Bind` Nodes. So we only need to descend
            // through containers that can host Binds (Module,
            // Block). Other containers (CallSite args, Select arms,
            // arithmetic operands, etc.) can contain *expressions*
            // that produce lambda values, but those aren't visible
            // to static resolution — a lambda value flowing
            // through a non-Bind path is the dynamic case.
        }
    }
}

/// Recursively visit every Node via direct mutable access,
/// dispatching `f` on each. Container Nodes recurse into their
/// children after `f` sees them.
///
/// We descend Module, Block, Bind (value), and CallSite (args +
/// fnode). Other container types are not yet traversed — extending
/// to those is a follow-up as we extend coverage to nested
/// expression positions.
fn visit_mut<R: Rt, E: UserEvent>(
    node: &mut Node<R, E>,
    f: &mut dyn FnMut(&mut Node<R, E>) -> Result<()>,
) -> Result<()> {
    f(node)?;
    // Dispatch on concrete type via Any-downcast. Each arm gives
    // mutable access to the container's children.
    let upd: &mut dyn Update<R, E> = &mut **node;
    let any: &mut dyn Any = upd;
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
                bind_to_lambda.get(&r.id).cloned()
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
            if !matches!(&farg.typ, Type::Fn(_)) {
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

