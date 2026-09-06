//! Runtime wrapper around a JIT-compiled kernel.
//!
//! Fusion has exactly two evaluators: the node-walk (`Box<dyn Update>`
//! graph, the canonical model) and the cranelift JIT. Fusion builds a
//! [`KernelSig`], JIT-compiles it, and:
//!
//! - JIT success → splice the native kernel + delete the original
//!   nodes. [`Kernel`] is the [`Apply<R, E>`] wrapper that drives the
//!   feeders, packs args across the JIT ABI boundary, and unpacks
//!   the result.
//! - JIT failure → DON'T splice. The original nodes
//!   stay in the graph and run through the node-walk, the universal
//!   fallback. A [`Kernel`] cannot be constructed without a JIT
//!   wrapper, so this is structural.
//!
//! This file keeps [`Kernel`] and its arg-packing — everything the
//! JIT boundary needs.

#[cfg(debug_assertions)]
use crate::fusion::emit_helpers::record_fusion_invocation;
use crate::{
    Apply, Event, ExecCtx, Node, Refs, Rt, Tag, UserEvent,
    fusion::{
        emit::{STALE, TAINT, WrappedKernel, pack_value_to_u64, prim_to_value_disc},
        emit_helpers::{KERNEL_ABORT, TagValue},
        kernel_abi::{self, KernelSig},
    },
};
use netidx_value::{ValArray, Value};
use std::sync::Arc;

// ─── Kernel: the Apply<R, E> wrapper ────────────────────────────

/// Wraps a [`KernelSig`] as an [`Apply<R, E>`] so the runtime can call
/// into a compiled kernel through the same dispatch path it uses for
/// every other function. On each `update` cycle we drive the input
/// nodes, cache their values, decide whether anything fired, and
/// dispatch into native code via the wrapper.
pub struct Kernel {
    /// wake catch-up: set by `sleep()`, taken by the next update — the
    /// kernel is a node like any other and tracks its own sleep. Feeds
    /// wire slot 0 bit 2.
    slept: bool,
    /// The kernel's ABI contract; the `Arc` is also its identity (the
    /// JIT's `by_kernel` cache keys on the pointer).
    kernel: Arc<KernelSig>,
    /// The compiled JIT wrapper this node dispatches into. Required:
    /// a fused node without a JIT cannot exist — JIT failure means
    /// the region was never spliced and the original nodes node-walk.
    jit: Arc<WrappedKernel>,
    /// Per-INSTANCE cross-invocation state, `jit.state_words` zeroed
    /// `u64`s (empty for the common stateless kernel). Passed by
    /// pointer in wire slot 1 each invocation; emission sites claim a
    /// word each for firing bookkeeping (exact HOF resize detection,
    /// first-call words — `design/kernel_instance_state.md`).
    /// Zero = "no previous observation": consumers store `value + 1`,
    /// so a fresh instance's init semantics fall out of the zeroing.
    state: Box<[u64]>,
    /// This instance's PER-CALL-SITE block (wire slot 2) when the
    /// compiled body claimed site words — the storage a kernel CALLER
    /// would normally supply. A region parent has no kernel caller, so
    /// it supplies its own here and writes the honor header, which is
    /// what makes its interior taint caches (scrutinee rides, call-
    /// result caches) live rather than inert.
    site: Box<[u64]>,
    /// Reusable only outside frames and wakes, with valid quiet feeders
    /// and a valid cached result. Bottom feeders may belong to untaken
    /// branches, so only executing the kernel determines output validity.
    resident: TagValue,
    /// Recursion-shrink reclaim (the JIT twin of the interp's activation
    /// delete). `self_gen` is bumped each invocation and stamped into
    /// every activation block reached (via `SELF_BLOCK_GEN`); after the
    /// run, any per-activation `SelfBlock` NOT stamped with it is freed
    /// (`reclaim_self_block_tree`). `tree_size` is the count of live
    /// activation blocks — the reclaim walk is GATED on the reach count
    /// falling below it (no shrink → no walk), so a stable or growing
    /// recursion pays only the counter, never the O(depth) walk.
    self_gen: u64,
    tree_size: u64,
}

impl Drop for Kernel {
    fn drop(&mut self) {
        // Free the per-slot state-table chains the JIT'd code boxed
        // behind their claimed anchor words (`graphix_slot_state_table`
        // — nested loops' prev-length words and in-loop call-site
        // blocks; an anchor owns `own_levels` directory levels, one per
        // enclosing loop). Semantic state: neither `sleep` nor
        // `reset_replay` touches these words, only instance death does.
        for a in self.jit.slot_table_words.iter() {
            let p = std::mem::replace(&mut self.state[a.rel as usize], 0);
            super::emit_helpers::free_slot_chain(
                p,
                a.own_levels as u64,
                a.leaf.as_deref(),
            );
        }
        // PER-ACTIVATION block trees (`SelfBlock`): a recursive call's
        // activations allocate their own blocks lazily, and instance
        // death is the only thing that reclaims them — the node-walk's
        // retained instance tree, freed when its owner dies.
        for b in self.jit.state_self_blocks.iter() {
            let p = std::mem::replace(&mut self.state[b.rel as usize], 0);
            super::emit_helpers::free_self_block_tree(p, &b.slots);
        }
        if let Some(l) = self.jit.own_site.as_ref() {
            for b in l.self_blocks.iter() {
                let p = std::mem::replace(&mut self.site[b.rel as usize], 0);
                super::emit_helpers::free_self_block_tree(p, &b.slots);
            }
        }
        // Anchors inside the block we supplied ourselves: a cross-kernel
        // caller frees the chains in the blocks it hands out, so a
        // region parent must free its own.
        if let Some(l) = self.jit.own_site.as_ref() {
            for a in l.anchors.iter() {
                let p = std::mem::replace(&mut self.site[a.rel as usize], 0);
                super::emit_helpers::free_slot_chain(
                    p,
                    a.own_levels as u64,
                    a.leaf.as_deref(),
                );
            }
        }
    }
}

impl std::fmt::Debug for Kernel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Kernel")
            .field("fn_name", &self.kernel.fn_name)
            .field("params", &self.kernel.params.len())
            .finish()
    }
}

impl Kernel {
    /// The compiled kernel IR this node executes. Used by graph
    /// introspection (`node_shape`) to assert on what a region
    /// actually fused into.
    pub fn kernel(&self) -> &Arc<KernelSig> {
        &self.kernel
    }

    /// Single construction chokepoint: a Kernel dispatches into
    /// `wrapped`, the JIT artifact — there is no other way to make
    /// one (JIT failure means the region is never spliced and the
    /// original nodes node-walk).
    pub fn new(
        kernel: Arc<KernelSig>,
        n_args: usize,
        wrapped: Arc<WrappedKernel>,
    ) -> ::anyhow::Result<Self> {
        debug_assert_eq!(n_args, kernel.params.len(), "Kernel arity = param count");
        let state = vec![0u64; wrapped.state_words].into_boxed_slice();
        // The parent's OWN call-site block, when its body was compiled
        // to the callee ABI.
        let site =
            vec![0u64; wrapped.own_site.as_ref().map(|l| l.words as usize).unwrap_or(0)]
                .into_boxed_slice();
        Ok(Self {
            slept: false,
            kernel,
            jit: wrapped,
            state,
            site,
            resident: TagValue::phantom(),
            self_gen: 0,
            tree_size: 0,
        })
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for Kernel {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> &TagValue {
        let woke = std::mem::take(&mut self.slept) && ctx.frame_depth == 0;
        let mut any_updated = false;
        let mut any_bottom = false;
        let mut polled: smallvec::SmallVec<[(Tag, Option<Value>); 16]> =
            smallvec::SmallVec::with_capacity(from.len());
        for src in from.iter_mut() {
            let tv = src.update(ctx, event);
            let tag = tv.tag();
            if tag.triggers() {
                any_updated = true;
            }
            any_bottom |= tag.is_bottom();
            let v = if tag.is_bottom() { None } else { Some(tv.value_cloned()) };
            polled.push((tag, v));
        }
        if crate::dbgenv::gxdbg_kpoll() {
            eprintln!(
                "KPOLL {} init={} any_updated={any_updated} tags={:?} present={:?} fd={}",
                self.kernel.fn_name,
                event.init,
                polled.iter().map(|(t, _)| t.bits()).collect::<Vec<_>>(),
                polled.iter().map(|(_, v)| v.is_some()).collect::<Vec<_>>(),
                ctx.frame_depth
            );
        }
        if !(any_updated
            || any_bottom
            || event.init
            || woke
            || ctx.frame_depth > 0
            || self.resident.tag().is_bottom())
        {
            return self.resident.ride();
        }
        if crate::dbgenv::graphix_dbg_invoke() {
            eprintln!(
                "KERNEL INVOKE {} init={} fired={:?} present={:?}",
                self.kernel.fn_name,
                event.init,
                polled.iter().map(|(t, _)| t.is_fired()).collect::<Vec<_>>(),
                polled.iter().map(|(_, v)| v.is_some()).collect::<Vec<_>>()
            );
        }
        #[cfg(debug_assertions)]
        record_fusion_invocation();
        let k = &self.kernel;
        let n_params = k.params.len();
        let mut param_opts: smallvec::SmallVec<[Option<Value>; 16]> =
            smallvec::smallvec![None; n_params];
        let mut param_tags: smallvec::SmallVec<[Tag; 16]> =
            smallvec::smallvec![Tag::STALE_BOTTOM; n_params];
        for (i, (tag, v)) in polled.drain(..).enumerate() {
            param_opts[i] = v;
            param_tags[i] = tag;
        }
        let wrapped = &self.jit;
        let taint = TAINT as u64;
        let stale = STALE as u64;
        let bits = |v: &Value| -> (u64, u64) {
            let [d, p] = crate::tval::value_words(v);
            (d, p)
        };
        // STAGE `(disc, payload word, keepalive Value)` per param, in
        // params (= ABI) order, in ONE pass: the present value
        // validated against the declared shape, or the kind's tainted
        // placeholder. The payload word is read while the staged Value
        // is on this stack and stays VALID across its move into the
        // smallvec (it's the Arc/inline payload word, unaffected by
        // the move); the keepalive Value holds the refcount across the
        // wrapper call (the kernel refcount-bumps what it keeps at
        // entry). Scalars keep nothing alive — their payload comes
        // from `pack_value_to_u64` (exact sign/zero extension: a
        // narrow Value's upper payload bytes are padding, so `bits`
        // alone would read uninitialized memory).
        use kernel_abi::ParamKind;
        let staged: smallvec::SmallVec<[(u64, u64, Value); 16]> = k
            .params
            .iter()
            .enumerate()
            .map(|(i, p)| {
                let ptag = param_tags[i];
                let flag = if ptag.is_fired() { 0 } else { stale };
                // The bottom flags: TAINT plus STALE for a standing
                // bottom (a triggering one stages bare TAINT).
                let bflag = taint | if ptag.triggers() { 0 } else { stale };
                let mismatch = |v: &Value| {
                    log::error!(
                        "kernel param `{}`: runtime {v:?} doesn't match the \
                         compiled {:?} slot (typechecker static/dynamic \
                         mismatch) — treating as bottom",
                        p.name,
                        p.kind,
                    );
                };
                match (&p.kind, param_opts[i].as_ref()) {
                    (ParamKind::Scalar(prim), Some(v)) => {
                        match pack_value_to_u64(v, *prim) {
                            Some(payload) => {
                                let disc = prim_to_value_disc(*prim) as u64 | flag;
                                (disc, payload, Value::Null)
                            }
                            None => {
                                mismatch(v);
                                let disc = prim_to_value_disc(*prim) as u64 | taint;
                                (disc, 0, Value::Null)
                            }
                        }
                    }
                    (ParamKind::Scalar(prim), None) => {
                        let disc = prim_to_value_disc(*prim) as u64 | bflag;
                        (disc, 0, Value::Null)
                    }
                    (
                        ParamKind::Array { .. }
                        | ParamKind::Tuple { .. }
                        | ParamKind::Struct { .. },
                        v,
                    ) => {
                        let staged = match v {
                            Some(v @ Value::Array(_)) => Some(v.clone()),
                            Some(v) => {
                                mismatch(v);
                                None
                            }
                            None => None,
                        };
                        match staged {
                            Some(v) => {
                                let (disc, payload) = bits(&v);
                                (disc | flag, payload, v)
                            }
                            None => {
                                let v = Value::Array(ValArray::from([]));
                                let (disc, payload) = bits(&v);
                                (disc | bflag, payload, v)
                            }
                        }
                    }
                    (ParamKind::String, v) => {
                        let staged = match v {
                            Some(v @ Value::String(_)) => Some(v.clone()),
                            Some(v) => {
                                mismatch(v);
                                None
                            }
                            None => None,
                        };
                        match staged {
                            Some(v) => {
                                let (disc, payload) = bits(&v);
                                (disc | flag, payload, v)
                            }
                            None => {
                                let v = Value::String(arcstr::ArcStr::new());
                                let (disc, payload) = bits(&v);
                                (disc | bflag, payload, v)
                            }
                        }
                    }
                    // Variant / Nullable / bare value shapes carry any
                    // Value with its real disc — no shape validation
                    // (a union's member set is the type system's
                    // concern; the kernel's consumers dispatch on the
                    // disc).
                    (
                        ParamKind::Variant { .. }
                        | ParamKind::Nullable { .. }
                        | ParamKind::Value { .. },
                        v,
                    ) => match v {
                        Some(v) => {
                            let v = v.clone();
                            let (disc, payload) = bits(&v);
                            (disc | flag, payload, v)
                        }
                        None => {
                            let v = Value::Null;
                            let (disc, payload) = bits(&v);
                            (disc | bflag, payload, v)
                        }
                    },
                }
            })
            .collect();
        // Pack the wire slots: context words then (disc, payload) per
        // param in ABI (= params) order.
        let mut slots: smallvec::SmallVec<[u64; 16]> =
            smallvec::SmallVec::with_capacity(self.kernel.abi_wire_slots_total());
        // Slot 0 carries the invocation's forced init view (bit 0),
        // quiet frame view (bit 1), and wake view (bit 2). Genuine init
        // is bit0 & !bit2; a wake cannot freshen standing deliveries.
        let init = if ctx.frame_depth > 0 { ctx.frame_init } else { event.init };
        let quiet = ctx.frame_depth > 0 && !ctx.frame_init;
        let wake = (ctx.frame_depth == 0 && event.wake_init) || woke;
        slots.push(init as u64 | (quiet as u64) << 1 | (wake as u64) << 2);
        slots.push(if self.state.is_empty() {
            0
        } else {
            self.state.as_mut_ptr() as u64
        });
        // Slot 2: the per-call-site state block. Supplied by the CALLER
        // for a cross-kernel call; a region parent supplies its own
        // (empty unless its body was compiled to the callee ABI).
        slots.push(if self.site.is_empty() { 0 } else { self.site.as_mut_ptr() as u64 });
        for (disc, payload, _keepalive) in staged.iter() {
            slots.push(*disc);
            slots.push(*payload);
        }
        // Drift guard: the packed slot count must equal the kernel's
        // declared ABI footprint.
        debug_assert_eq!(
            slots.len(),
            self.kernel.abi_wire_slots_total(),
            "packed slot count must match the kernel ABI layout"
        );
        let mut out: [u64; 2] = [0, 0];
        let f = unsafe { wrapped.fn_ptr() };
        // Always reset the abort flag before the call so we can
        // distinguish "this kernel aborted" from "some earlier kernel
        // left the flag set."
        KERNEL_ABORT.with(|c| c.set(false));
        // Recursion-shrink reclaim setup (the JIT twin of the interp's
        // activation delete): only for kernels that carry a self-block
        // tree. Bump this instance's generation and stamp it into every
        // activation reached during the call (via `SELF_BLOCK_GEN` in
        // `graphix_site_child_block`), resetting the reach counter; save
        // the enclosing thread-local values so a NESTED kernel restores
        // ours (its reaches must not count toward this tree).
        let has_self_blocks = !self.jit.state_self_blocks.is_empty()
            || self.jit.own_site.as_ref().is_some_and(|l| !l.self_blocks.is_empty());
        let (shrink_gen, saved_gen, saved_reached) = if has_self_blocks {
            self.self_gen = self.self_gen.wrapping_add(1);
            let sg =
                super::emit_helpers::SELF_BLOCK_GEN.with(|c| c.replace(self.self_gen));
            let sr = super::emit_helpers::SELF_BLOCK_REACHED.with(|c| c.replace(0));
            (Some(self.self_gen), sg, sr)
        } else {
            (None, 0, 0)
        };
        // Value-hook loan (the core-trait seam): `graphix_value_eq`
        // and every other helper comparing or printing Values inside
        // this invocation honors core Eq/Ord/Display implementations,
        // exactly as the interp's armed operators do. The env loan is
        // what a typed fastcall site (`graphix_typedcall`: a cast, a
        // typed parser) resolves type names through — the same
        // `cast_value` call the node-walk makes.
        crate::node::coretraits::with_value_hooks(ctx, event, |ctx, event| {
            let ((), raises) = super::emit_helpers::with_qop_raises(|| unsafe {
                super::emit_helpers::with_kernel_env(&ctx.env, || {
                    f(slots.as_ptr(), out.as_mut_ptr());
                })
            });
            // The run's `?` deliveries, in execution order — the
            // kernel is a pure function of its inputs to (result,
            // deliveries), and the deliveries happen HERE, at the node
            // boundary, through the interp's own handler path.
            for (site, v) in raises {
                // SAFETY: `site` is the interned `QopSite` the emitted
                // code baked; the kernel's `KernelValues` keeps it alive.
                let site = unsafe { &*site };
                if let Value::Error(e) = v {
                    crate::node::error::deliver_error(
                        ctx,
                        event,
                        &site.handler,
                        site.own_top,
                        &site.spec,
                        (*e).clone(),
                    );
                }
            }
        });
        let pending = KERNEL_ABORT.with(|c| c.replace(false));
        // Recursion-shrink reclaim + restore the reach thread-locals.
        // MUST run on every exit path (the pending branch below returns
        // early), so it sits before that branch. On a clean run: if the
        // reach count fell below the live tree size, some depth was not
        // re-reached — free the unreached activation subtrees and null
        // their words (`Kernel::drop`/`reset_replay` then see 0 and
        // never double-free); update the tree size. A PENDING (aborted)
        // run reached only a prefix, so its reach count is not a
        // shrink signal — skip the reclaim, just restore.
        if let Some(generation) = shrink_gen {
            use super::emit_helpers::{
                SELF_BLOCK_GEN, SELF_BLOCK_REACHED, reclaim_self_block_tree,
            };
            let reached = SELF_BLOCK_REACHED.with(|c| c.get());
            if !pending {
                if reached < self.tree_size {
                    let jit = self.jit.clone();
                    for b in jit.state_self_blocks.iter() {
                        reclaim_self_block_tree(
                            (&mut self.state[b.rel as usize]) as *mut u64,
                            b.words as usize,
                            &b.slots,
                            generation,
                        );
                    }
                    if let Some(l) = jit.own_site.as_ref() {
                        for b in l.self_blocks.iter() {
                            reclaim_self_block_tree(
                                (&mut self.site[b.rel as usize]) as *mut u64,
                                b.words as usize,
                                &b.slots,
                                generation,
                            );
                        }
                    }
                }
                self.tree_size = reached;
            }
            SELF_BLOCK_GEN.with(|c| c.set(saved_gen));
            SELF_BLOCK_REACHED.with(|c| c.set(saved_reached));
        }
        if pending {
            // Abort paths have dropped their owned values. The out
            // slot is a sentinel, not a Value, and cannot be decoded.
            if crate::dbgenv::graphix_dbg_invoke() {
                eprintln!(
                    "KERNEL RESULT {} PENDING fd={}",
                    self.kernel.fn_name, ctx.frame_depth
                );
            }
            if ctx.frame_depth > 0 {
                return TagValue::bottom_null(true);
            }
            return self.resident.ride();
        }
        // SAFETY: the kernel's return path wrote a real Value's words
        // into the out slot (the pending path returned before the
        // decode). TagValue masks TAINT/STALE before materializing it.
        let tv = unsafe { TagValue::from_raw(out[0], out[1]) };
        let tag = tv.tag();
        if crate::dbgenv::graphix_dbg_invoke() {
            eprintln!("KERNEL RESULT {} tag={tag:?} pending=false", self.kernel.fn_name);
        }
        if tag.is_bottom() {
            drop(tv.value());
            return self.resident.set(TagValue::tagged(Value::Null, tag));
        }
        // Fill the RESULT slot (the value channel — see `resident`)
        // and lend it: Fired for a fresh result, Stale for a quiet
        // one (the output chain didn't fire this invocation).
        let v = tv.value();
        self.resident.set(TagValue::tagged(v, tag))
    }

    fn delete(&mut self, _ctx: &mut ExecCtx<R, E>) {}

    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {
        if crate::dbgenv::gxdbg_kernel_sleep() {
            eprintln!("KERNEL-APPLY-SLEEP {}", self.kernel.fn_name);
        }
        // SLEEP IS PAUSE (Eric's ruling 2026-07-31): the kernel's
        // interior memory (prev-length words, first-call words, the
        // activation trees) survives an arm's sleep; only a frame
        // reset (`reset_replay`) and instance death (`Drop`) clear
        // anything. The next update is the wake (wire slot 0 bit 2).
        self.slept = true;
    }

    fn reset_replay(&mut self, _ctx: &mut ExecCtx<R, E>) {
        // A kernel holds no replay caches: its interior memory is all
        // semantic (prev-length words, first-call words, the slot
        // chains and activation trees), which the node-walk's
        // `FoldQ::reset_replay` keeps too (quiet-frame-init-view-
        // aug2026/08).
    }

    fn refs(&self, _refs: &mut Refs) {}
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::fusion::emit::unpack_u64_to_value;
    use kernel_abi::PrimType;

    #[test]
    fn value_boundary_bits_round_trip() {
        // Value → u64 bits (pack) → Value (unpack) should be lossless
        // for the scalar primitives that cross the JIT boundary.
        let cases: &[(Value, PrimType)] = &[
            (Value::I64(42), PrimType::I64),
            (Value::I64(i64::MIN), PrimType::I64),
            (Value::F64(3.14), PrimType::F64),
            (Value::F32(2.5), PrimType::F32),
            (Value::Bool(true), PrimType::Bool),
            (Value::Bool(false), PrimType::Bool),
            (Value::U32(7), PrimType::U32),
            (Value::U64(u64::MAX), PrimType::U64),
            (Value::I8(-1), PrimType::I8),
        ];
        for (v, p) in cases {
            let bits = pack_value_to_u64(v, *p).expect("matching prim");
            assert_eq!(unpack_u64_to_value(bits, *p), *v);
        }
    }
}
