//! `FusedRegion` — the runtime node M8.4 splices into place of a
//! maximal sync region's existing compiled subtree.
//!
//! Structurally this mirrors what `CallSite` does for a lambda call
//! ([`crate::node::callsite::CallSite`]): the region has *inputs*
//! (one per free-variable `Ref` discovered by
//! [`crate::fusion::discover_region_inputs`]) which need to be fed
//! to the kernel each cycle, and a *kernel* (a [`KirNode`]) that
//! consumes those inputs and produces the region's value. The
//! difference: a `CallSite` wraps a runtime lambda *call*, while a
//! `FusedRegion` wraps the region's expression tree — there's no
//! lambda, no rebind, no resolved FnType.
//!
//! Feeder nodes are fresh `Ref` nodes (one per input), compiled via
//! [`crate::node::bind::Ref::compile`]. Each registers a
//! subscription on the binding's `BindId`; the runtime fires the
//! `FusedRegion` whenever any input updates.

use crate::{
    expr::{Expr, ExprKind, ModPath},
    fusion::{FusedSubgraph, ModuleKernel},
    kir_interp::{kir_type_to_graphix_type, total_kernel_arity, KirNode},
    node::bind::Ref,
    typ::Type,
    Apply, BindId, Event, ExecCtx, Node, Refs, Rt, Scope, TypecheckPhase,
    Update, UserEvent,
};
use anyhow::{anyhow, Result};
use netidx_value::Value;
use triomphe::Arc;

pub struct FusedRegion<R: Rt, E: UserEvent> {
    spec: Arc<Expr>,
    typ: Type,
    /// The kernel runner. [`KirNode`] is an `Apply<R,E>` whose
    /// `update` drives the `from` slice (i.e. `arg_nodes`) and runs
    /// the kernel.
    inner: KirNode<R, E>,
    /// Feeder nodes — one per region input, in input declaration
    /// order. `KirNode::update` reads each via `from[i].update(...)`.
    arg_nodes: Vec<Node<R, E>>,
    /// Module-kernel exports — `BindId`s the kernel's tuple result
    /// publishes to via the runtime variable system. Empty for a
    /// value-region kernel (which just returns its tail value).
    /// Populated by [`Self::from_module_kernel`] for module batches
    /// (M8.4 step f); the i-th slot of the kernel's
    /// `KirType::Tuple(…)` return becomes `set_var(exports[i], …)`.
    exports: Vec<BindId>,
}

impl<R: Rt, E: UserEvent> std::fmt::Debug for FusedRegion<R, E> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("FusedRegion")
            .field("root_id", &self.spec.id)
            .field("n_args", &self.arg_nodes.len())
            .finish()
    }
}

impl<R: Rt, E: UserEvent> FusedRegion<R, E> {
    /// Build a `FusedRegion` for `subgraph`. Compiles a fresh `Ref`
    /// feeder for each input (subscribing to its `BindId`), then
    /// constructs the `KirNode` with the subgraph's kernel +
    /// registry. Returns `Err` if the subgraph has no kernel — the
    /// caller should keep the region as the existing node graph.
    ///
    /// `spec` is the region root's `Expr` (cloned into an `Arc`);
    /// it's used as `spec()` for diagnostics. `top_id` for the
    /// freshly-compiled `Ref` feeders is the region root's
    /// `ExprId`, so `ref_var` subscriptions live at the region
    /// granularity.
    pub fn from_subgraph(
        ctx: &mut ExecCtx<R, E>,
        scope: &Scope,
        spec: Expr,
        subgraph: &FusedSubgraph,
    ) -> Result<Node<R, E>> {
        let kernel = subgraph
            .kernel
            .clone()
            .ok_or_else(|| anyhow!("FusedRegion: subgraph has no kernel"))?;
        let top_id = subgraph.root_id;
        // Compile a Ref feeder per input. Each Ref::compile call
        // looks up the binding in `ctx.env`, calls
        // `ctx.rt.ref_var(bind_id, top_id)`, and returns a
        // ready-to-run `Node`.
        let mut arg_nodes: Vec<Node<R, E>> =
            Vec::with_capacity(subgraph.inputs.len());
        for input in &subgraph.inputs {
            let name = ModPath::from_iter([input.name.as_str()]);
            let ref_spec = ExprKind::Ref { name: name.clone() }.to_expr_nopos();
            let feeder = Ref::compile(ctx, ref_spec, scope, top_id, &name)?;
            arg_nodes.push(feeder);
        }
        // `total_kernel_arity` for a region kernel equals the input
        // count: `build_kir_kernel_from_region` populates
        // `tail_call_slots` from inputs in declaration order, and
        // regions have no fn_params in the M8.4 initial model.
        let n_args = total_kernel_arity(&kernel);
        debug_assert_eq!(n_args, subgraph.inputs.len(), "FusedRegion arity");
        let typ = kir_type_to_graphix_type(&kernel.return_type);
        // `KirNode::new` runs both pre_init helpers internally via
        // its `build` chokepoint — no manual `pre_init_*` needed.
        let inner = KirNode::new(
            ctx,
            kernel,
            n_args,
            subgraph.registry.clone(),
            scope.clone(),
            top_id,
        )?;
        Ok(Box::new(Self {
            spec: Arc::new(spec),
            typ,
            inner,
            arg_nodes,
            exports: Vec::new(),
        }))
    }

    /// Build a `FusedRegion` for a module batch — a kernel that
    /// computes every sync top-level `let` in the batch and returns
    /// a tuple of those values. Each tuple slot publishes to the
    /// corresponding `BindId` via the runtime variable system on
    /// every update, mirroring what the original `Bind` nodes did
    /// statement-by-statement.
    pub fn from_module_kernel(
        ctx: &mut ExecCtx<R, E>,
        scope: &Scope,
        spec: Expr,
        top_id: crate::expr::ExprId,
        mk: &ModuleKernel,
    ) -> Result<Node<R, E>> {
        // Compile one Ref feeder per free-var input (same as
        // `from_subgraph`). Module kernels for pure-constant chains
        // have no inputs; the kernel still fires on init via the
        // `from.is_empty() && event.init` path in
        // `KirNode::update`.
        let mut arg_nodes: Vec<Node<R, E>> = Vec::with_capacity(mk.inputs.len());
        for input in &mk.inputs {
            let name = ModPath::from_iter([input.name.as_str()]);
            let ref_spec = ExprKind::Ref { name: name.clone() }.to_expr_nopos();
            let feeder = Ref::compile(ctx, ref_spec, scope, top_id, &name)?;
            arg_nodes.push(feeder);
        }
        let n_args = total_kernel_arity(&mk.kernel);
        debug_assert_eq!(n_args, mk.inputs.len(), "module-kernel arity");
        let typ = kir_type_to_graphix_type(&mk.kernel.return_type);
        // `KirNode::new` runs both pre_init helpers internally.
        let inner = KirNode::new(
            ctx,
            mk.kernel.clone(),
            n_args,
            mk.registry.clone(),
            scope.clone(),
            top_id,
        )?;
        let exports: Vec<BindId> =
            mk.exports.iter().map(|e| e.bind_id).collect();
        Ok(Box::new(Self {
            spec: Arc::new(spec),
            typ,
            inner,
            arg_nodes,
            exports,
        }))
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for FusedRegion<R, E> {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        event: &mut Event<E>,
    ) -> Option<Value> {
        let result = self.inner.update(ctx, &mut self.arg_nodes, event)?;
        // Module batch: the kernel returned a tuple whose slots
        // correspond to `self.exports`. Publish each slot via the
        // same mechanism `Bind` would have used — write to
        // `event.variables` so this cycle's downstream readers see
        // the new value, update `ctx.cached`, and `notify_set` so
        // subscribers are scheduled. Mirrors `Bind::update`.
        if !self.exports.is_empty() {
            if let Value::Array(arr) = &result {
                for (i, bind_id) in self.exports.iter().enumerate() {
                    let Some(v) = arr.get(i) else { break };
                    event.variables.insert(*bind_id, v.clone());
                    ctx.cached.insert(*bind_id, v.clone());
                    ctx.rt.notify_set(*bind_id);
                }
            }
        }
        Some(result)
    }

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        for n in &mut self.arg_nodes {
            n.delete(ctx);
        }
        Apply::delete(&mut self.inner, ctx);
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        for n in &mut self.arg_nodes {
            n.sleep(ctx);
        }
        Apply::sleep(&mut self.inner, ctx);
    }

    fn typecheck_inner(&mut self, ctx: &mut ExecCtx<R, E>) -> Result<()> {
        for n in &mut self.arg_nodes {
            n.typecheck(ctx)?;
        }
        // The kernel was already typechecked by fusion at build
        // time; the runtime `Apply::typecheck` is a no-op for
        // `KirNode`. Pass `TypecheckPhase::Lambda` for parity with
        // how lambda Applies are checked.
        Apply::typecheck(
            &mut self.inner,
            ctx,
            &mut self.arg_nodes,
            TypecheckPhase::Lambda,
        )?;
        Ok(())
    }

    fn typ(&self) -> &Type {
        &self.typ
    }

    fn refs(&self, refs: &mut Refs) {
        for n in &self.arg_nodes {
            n.refs(refs);
        }
        Apply::refs(&self.inner, refs);
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }
}
