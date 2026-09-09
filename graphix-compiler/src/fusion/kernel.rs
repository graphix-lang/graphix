//! [`Kernel`]: the [`Apply<R, E>`] wrapper around a JIT-compiled
//! kernel. It drives the feeders, packs their values across the JIT
//! ABI boundary, and unpacks the result. A `Kernel` cannot exist
//! without a compiled wrapper; a region whose JIT fails is never
//! spliced and its nodes keep node-walking.

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

/// Wraps a [`KernelSig`] as an [`Apply<R, E>`]: each `update` drives
/// the input nodes, decides whether anything fired, and dispatches
/// into the compiled wrapper.
pub struct Kernel {
    /// Set by `sleep()`, taken by the next update; feeds wire slot 0 bit 2.
    slept: bool,
    /// The ABI contract; the `Arc` pointer is also the kernel's identity
    /// in the JIT's `by_kernel` cache.
    kernel: Arc<KernelSig>,
    jit: Arc<WrappedKernel>,
    /// Per-instance state words (wire slot 1): prev-length and first-call
    /// words. Zero means "no previous observation"; consumers store
    /// `value + 1`.
    state: Box<[u64]>,
    /// This instance's own call-site block (wire slot 2), the storage a
    /// kernel caller would otherwise supply.
    site: Box<[u64]>,
    /// The last result; ridden when no feeder fired. Bottom feeders may
    /// belong to untaken branches, so only running the kernel decides
    /// output validity.
    resident: TagValue,
    /// `self_gen` is stamped into every activation block reached by an
    /// invocation; blocks left unstamped are freed afterwards. The walk
    /// runs only when the reach count falls below `tree_size`.
    self_gen: u64,
    tree_size: u64,
}

impl Drop for Kernel {
    fn drop(&mut self) {
        // Only instance death frees the slot chains and activation trees;
        // neither `sleep` nor `reset_replay` touches them.
        for a in self.jit.slot_table_words.iter() {
            let p = std::mem::replace(&mut self.state[a.rel as usize], 0);
            super::emit_helpers::free_slot_chain(
                p,
                a.own_levels as u64,
                a.leaf.as_deref(),
            );
        }
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
    /// The ABI contract this node executes.
    pub fn kernel(&self) -> &Arc<KernelSig> {
        &self.kernel
    }

    pub fn new(
        kernel: Arc<KernelSig>,
        n_args: usize,
        wrapped: Arc<WrappedKernel>,
    ) -> ::anyhow::Result<Self> {
        debug_assert_eq!(n_args, kernel.params.len(), "Kernel arity = param count");
        let state = vec![0u64; wrapped.state_words].into_boxed_slice();
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
        // Each staged `(disc, payload, keepalive)`: the keepalive Value
        // holds the payload's refcount across the wrapper call. Scalars
        // go through `pack_value_to_u64` because a narrow Value's upper
        // payload bytes are padding.
        use kernel_abi::ParamKind;
        let staged: smallvec::SmallVec<[(u64, u64, Value); 16]> = k
            .params
            .iter()
            .enumerate()
            .map(|(i, p)| {
                let ptag = param_tags[i];
                let flag = if ptag.is_fired() { 0 } else { stale };
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
        let mut slots: smallvec::SmallVec<[u64; 16]> =
            smallvec::SmallVec::with_capacity(self.kernel.abi_wire_slots_total());
        // Slot 0: bit 0 init view, bit 1 quiet frame, bit 2 wake.
        let init = if ctx.frame_depth > 0 { ctx.frame_init } else { event.init };
        let quiet = ctx.frame_depth > 0 && !ctx.frame_init;
        let wake = (ctx.frame_depth == 0 && event.wake_init) || woke;
        slots.push(init as u64 | (quiet as u64) << 1 | (wake as u64) << 2);
        slots.push(if self.state.is_empty() {
            0
        } else {
            self.state.as_mut_ptr() as u64
        });
        slots.push(if self.site.is_empty() { 0 } else { self.site.as_mut_ptr() as u64 });
        for (disc, payload, _keepalive) in staged.iter() {
            slots.push(*disc);
            slots.push(*payload);
        }
        debug_assert_eq!(
            slots.len(),
            self.kernel.abi_wire_slots_total(),
            "packed slot count must match the kernel ABI layout"
        );
        let mut out: [u64; 2] = [0, 0];
        let f = unsafe { wrapped.fn_ptr() };
        KERNEL_ABORT.with(|c| c.set(false));
        // A nested kernel's reaches must not count toward this tree, so
        // the enclosing thread-local values are saved and restored.
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
        crate::node::coretraits::with_value_hooks(ctx, event, |ctx, event| {
            let ((), raises) = super::emit_helpers::with_qop_raises(|| unsafe {
                super::emit_helpers::with_kernel_env(&ctx.env, || {
                    f(slots.as_ptr(), out.as_mut_ptr());
                })
            });
            for (site, v) in raises {
                // SAFETY: `site` is an interned `QopSite` kept alive by the
                // kernel's `KernelValues`.
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
        // Must run before the pending early return. An aborted run
        // reached only a prefix, so its reach count is not a shrink signal.
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
            // The out slot is a sentinel, not a Value.
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
        // SAFETY: a non-pending run wrote a real Value's words into `out`.
        let tv = unsafe { TagValue::from_raw(out[0], out[1]) };
        let tag = tv.tag();
        if crate::dbgenv::graphix_dbg_invoke() {
            eprintln!("KERNEL RESULT {} tag={tag:?} pending=false", self.kernel.fn_name);
        }
        if tag.is_bottom() {
            drop(tv.value());
            return self.resident.set(TagValue::tagged(Value::Null, tag));
        }
        let v = tv.value();
        self.resident.set(TagValue::tagged(v, tag))
    }

    fn delete(&mut self, _ctx: &mut ExecCtx<R, E>) {}

    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {
        if crate::dbgenv::gxdbg_kernel_sleep() {
            eprintln!("KERNEL-APPLY-SLEEP {}", self.kernel.fn_name);
        }
        // Sleep is pause: interior memory survives it.
        self.slept = true;
    }

    fn reset_replay(&mut self, _ctx: &mut ExecCtx<R, E>) {
        // A kernel holds no replay caches; its interior memory is semantic.
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
