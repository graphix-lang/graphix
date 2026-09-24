//! [`FusedKernel`]: the `Update` node over a JIT-compiled region. It
//! drives the region's input feeders, packs their values across the
//! JIT ABI boundary, and unpacks the result. It exists only over a
//! compiled wrapper: a region whose JIT fails is never spliced and its
//! nodes keep node-walking.

#[cfg(debug_assertions)]
use crate::fusion::emit_helpers::record_fusion_invocation;
use crate::{
    Event, ExecCtx, Node, NodeView, Refs, Rt, Update, UserEvent,
    expr::Expr,
    fusion::{
        emit::{
            STALE, TAINT, WrappedKernel, pack_value_to_u64, prim_to_value_disc,
            record_decode, record_encode, record_len,
        },
        emit_helpers::{
            self, EMPTY_ARR, KERNEL_ABORT, SELF_BLOCK_GEN, SELF_BLOCK_REACHED, TagValue,
            free_self_block_tree, free_slot_chain, reclaim_self_block_tree,
        },
        kernel_abi::{KernelSig, ParamKind},
    },
    image::{
        self, ImageBuf,
        nodes::{NodeTag, decode_nodes, encode_nodes, nodes_len, put_tag, tag_len},
    },
    node::WakeBit,
    tval::{Tag, value_words},
    typ::Type,
};
use anyhow::Result;
use netidx_core::pack::{Pack, PackError, decode_varint, encode_varint, varint_len};
use netidx_value::Value;
use poolshark::local::LPooled;
use smallvec::SmallVec;
use std::sync::LazyLock;
use triomphe::Arc;

/// The placeholder words of an absent composite input.
static EMPTY_ARRAY: LazyLock<Value> = LazyLock::new(|| Value::Array(EMPTY_ARR.clone()));

/// An `Update` node over a compiled kernel and its input feeders.
pub struct FusedKernel<R: Rt, E: UserEvent> {
    spec: Expr,
    typ: Type,
    /// One feeder Node per kernel input slot.
    feeders: Box<[Node<R, E>]>,
    /// Set by `sleep()`, taken by the next update; feeds wire slot 0 bit 2.
    slept: WakeBit,
    /// The ABI contract; the `Arc` pointer is also the kernel's identity
    /// in the JIT's `by_kernel` cache.
    kernel: Arc<KernelSig>,
    jit: WrappedKernel,
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

impl<R: Rt, E: UserEvent> Drop for FusedKernel<R, E> {
    fn drop(&mut self) {
        // Only instance death frees the slot chains and activation trees;
        // neither `sleep` nor `reset_replay` touches them.
        // SAFETY: the words are taken out of the state, so each chain and
        // tree is freed once, by the layout the wrapper describes.
        for a in self.jit.slot_table_words.iter() {
            let p = std::mem::replace(&mut self.state[a.rel as usize], 0);
            unsafe { free_slot_chain(p, a.own_levels as u64, a.leaf.as_deref()) };
        }
        for b in self.jit.state_self_blocks.iter() {
            let p = std::mem::replace(&mut self.state[b.rel as usize], 0);
            unsafe { free_self_block_tree(p, &b.layout) };
        }
        if let Some(l) = self.jit.own_site.as_ref() {
            for b in l.self_blocks.iter() {
                let p = std::mem::replace(&mut self.site[b.rel as usize], 0);
                unsafe { free_self_block_tree(p, &b.layout) };
            }
            for a in l.anchors.iter() {
                let p = std::mem::replace(&mut self.site[a.rel as usize], 0);
                unsafe { free_slot_chain(p, a.own_levels as u64, a.leaf.as_deref()) };
            }
        }
    }
}

impl<R: Rt, E: UserEvent> std::fmt::Debug for FusedKernel<R, E> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("FusedKernel")
            .field("fn_name", &self.kernel.fn_name)
            .field("inputs", &self.feeders.len())
            .finish()
    }
}

impl<R: Rt, E: UserEvent> FusedKernel<R, E> {
    pub(crate) fn new(
        spec: Expr,
        typ: Type,
        kernel: Arc<KernelSig>,
        jit: WrappedKernel,
        feeders: Box<[Node<R, E>]>,
    ) -> Node<R, E> {
        debug_assert_eq!(feeders.len(), kernel.params.len(), "one feeder per param");
        let state = vec![0u64; jit.state_words].into_boxed_slice();
        let site = vec![0u64; jit.own_site.as_ref().map_or(0, |l| l.words as usize)]
            .into_boxed_slice();
        Node::new(Self {
            spec,
            typ,
            feeders,
            slept: WakeBit::default(),
            kernel,
            jit,
            state,
            site,
            resident: TagValue::phantom(),
            self_gen: 0,
            tree_size: 0,
        })
    }

    /// The kernel signature this region fused into.
    pub fn kernel(&self) -> &Arc<KernelSig> {
        &self.kernel
    }

    /// The feeder nodes, one per kernel input slot.
    pub fn feeders(&self) -> &[Node<R, E>] {
        &self.feeders
    }

    /// Rebuild a region from an image: the wrapper record installs into
    /// the context's module and the node allocates fresh state.
    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<R, E>,
        buf: &mut &[u8],
    ) -> Result<Node<R, E>, PackError> {
        let spec = Expr::decode(buf)?;
        let typ = Type::decode(buf)?;
        let feeders = decode_nodes(ctx, buf)?.into_boxed_slice();
        let state_words = decode_varint(buf)? as usize;
        let slot_table_words = Pack::decode(buf)?;
        let own_site = Pack::decode(buf)?;
        let state_self_blocks = Pack::decode(buf)?;
        let wrapper = record_decode(buf)?;
        let jit = ctx
            .fusion
            .jit()
            .and_then(|mut jit| {
                jit.load_wrapped(
                    &wrapper,
                    state_words,
                    slot_table_words,
                    own_site,
                    state_self_blocks,
                )
            })
            .map_err(|e| {
                log::warn!(
                    "loading the kernel `{}` from the image: {e:#}",
                    wrapper.label
                );
                PackError::InvalidFormat
            })?;
        let kernel = wrapper.kernel.clone();
        if feeders.len() != kernel.params.len() {
            return Err(PackError::InvalidFormat);
        }
        Ok(Self::new(spec, typ, kernel, jit, feeders))
    }

    /// Nothing ran yet: every word the image does not carry is initial.
    fn quiescent(&self) -> bool {
        let mut slept = self.slept;
        self.state.iter().chain(self.site.iter()).all(|w| *w == 0)
            && !slept.take()
            && self.self_gen == 0
            && self.tree_size == 0
            && self.resident.tag() == Tag::STALE_BOTTOM
            && self.resident.with_value(|v| matches!(v, Value::Null))
    }

    /// The `(disc, payload)` words of one input: the Value encoding of
    /// its production with the tag folded on, or a placeholder under
    /// TAINT for an absent one. The words borrow the feeder's resident
    /// (or a static): the kernel clones what it keeps on entry.
    fn stage(p: &ParamKind, name: &str, tv: &TagValue) -> (u64, u64) {
        let tag = tv.tag();
        // The typechecker and the runtime disagree about this slot: a
        // compiler bug, not a program's, and nothing downstream could be
        // trusted past it.
        let mismatch = |v: &Value| -> ! {
            panic!(
                "kernel param `{name}`: runtime {v:?} does not match the compiled {p:?} slot"
            )
        };
        if tag.is_bottom() {
            let flag = TAINT as u64 | if tag.triggers() { 0 } else { STALE as u64 };
            let [disc, payload] = match p {
                ParamKind::Scalar(prim) => [prim_to_value_disc(*prim) as u64, 0],
                ParamKind::Array { .. }
                | ParamKind::Tuple { .. }
                | ParamKind::Struct { .. } => value_words(&EMPTY_ARRAY),
                ParamKind::String => value_words(&Value::String(arcstr::ArcStr::new())),
                ParamKind::Variant { .. }
                | ParamKind::Nullable { .. }
                | ParamKind::Value { .. } => value_words(&Value::Null),
            };
            return (disc | flag, payload);
        }
        let flag = if tag.is_fired() { 0 } else { STALE as u64 };
        tv.with_value(|v| {
            let [disc, payload] = match (p, v) {
                // A narrow Value's upper payload bytes are padding.
                (ParamKind::Scalar(prim), v) => match pack_value_to_u64(v, *prim) {
                    Some(payload) => [prim_to_value_disc(*prim) as u64, payload],
                    None => mismatch(v),
                },
                (
                    ParamKind::Array { .. }
                    | ParamKind::Tuple { .. }
                    | ParamKind::Struct { .. },
                    Value::Array(_),
                )
                | (ParamKind::String, Value::String(_))
                | (
                    ParamKind::Variant { .. }
                    | ParamKind::Nullable { .. }
                    | ParamKind::Value { .. },
                    _,
                ) => value_words(v),
                (_, v) => mismatch(v),
            };
            (disc | flag, payload)
        })
    }
}

impl<R: Rt, E: UserEvent> Update<R, E> for FusedKernel<R, E> {
    fn image_len(&self) -> usize {
        let w = &self.jit;
        tag_len()
            + self.spec.encoded_len()
            + self.typ.encoded_len()
            + nodes_len(&self.feeders)
            + varint_len(w.state_words as u64)
            + w.slot_table_words.encoded_len()
            + w.own_site.encoded_len()
            + w.state_self_blocks.encoded_len()
            + record_len(&w.wrapper)
    }

    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        if !self.quiescent() {
            return Err(PackError::Application(image::NOT_QUIESCENT));
        }
        let w = &self.jit;
        put_tag(NodeTag::Fused, buf);
        self.spec.encode(buf)?;
        self.typ.encode(buf)?;
        encode_nodes(&self.feeders, buf)?;
        encode_varint(w.state_words as u64, buf);
        w.slot_table_words.encode(buf)?;
        w.own_site.encode(buf)?;
        w.state_self_blocks.encode(buf)?;
        record_encode(&w.wrapper, buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<R, E>, event: &mut Event<E>) -> &TagValue {
        let woke = self.slept.take() && ctx.frame_depth == 0;
        let mut any_updated = false;
        let mut any_bottom = false;
        let mut polled: SmallVec<[&TagValue; 8]> = SmallVec::new();
        for src in self.feeders.iter_mut() {
            let tv = src.update(ctx, event);
            let tag = tv.tag();
            any_updated |= tag.triggers();
            any_bottom |= tag.is_bottom();
            polled.push(tv);
        }
        if crate::dbgenv::gxdbg_kpoll() {
            eprintln!(
                "KPOLL {} init={} any_updated={any_updated} tags={:?} present={:?} fd={}",
                self.kernel.fn_name,
                event.init,
                polled.iter().map(|tv| tv.tag().bits()).collect::<Vec<_>>(),
                polled.iter().map(|tv| !tv.is_bottom()).collect::<Vec<_>>(),
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
                polled.iter().map(|tv| tv.is_fired()).collect::<Vec<_>>(),
                polled.iter().map(|tv| !tv.is_bottom()).collect::<Vec<_>>()
            );
        }
        #[cfg(debug_assertions)]
        record_fusion_invocation();
        let mut slots: LPooled<Vec<u64>> = LPooled::take();
        // Slot 0: bit 0 init view, bit 1 quiet frame, bit 2 wake.
        let init = if ctx.frame_depth > 0 { ctx.dispatch_init } else { event.init };
        let quiet = ctx.frame_depth > 0 && !ctx.dispatch_init;
        let wake = (ctx.frame_depth == 0 && event.wake_init) || woke;
        slots.push(init as u64 | (quiet as u64) << 1 | (wake as u64) << 2);
        slots.push(if self.state.is_empty() {
            0
        } else {
            self.state.as_mut_ptr() as u64
        });
        slots.push(if self.site.is_empty() { 0 } else { self.site.as_mut_ptr() as u64 });
        for (p, tv) in self.kernel.params.iter().zip(polled.drain(..)) {
            let (disc, payload) = Self::stage(&p.kind, &p.name, tv);
            slots.push(disc);
            slots.push(payload);
        }
        debug_assert_eq!(
            slots.len(),
            self.kernel.abi_wire_slots_total(),
            "packed slot count must match the kernel ABI layout"
        );
        let mut out: [u64; 2] = [0, 0];
        let f = unsafe { self.jit.fn_ptr() };
        KERNEL_ABORT.with(|c| c.set(false));
        // A nested kernel's reaches must not count toward this tree, so
        // the enclosing thread-local values are saved and restored.
        let has_self_blocks = !self.jit.state_self_blocks.is_empty()
            || self.jit.own_site.as_ref().is_some_and(|l| !l.self_blocks.is_empty());
        let (shrink_gen, saved_gen, saved_reached) = if has_self_blocks {
            self.self_gen = self.self_gen.wrapping_add(1);
            let sg = SELF_BLOCK_GEN.with(|c| c.replace(self.self_gen));
            let sr = SELF_BLOCK_REACHED.with(|c| c.replace(0));
            (Some(self.self_gen), sg, sr)
        } else {
            (None, 0, 0)
        };
        // The run reads its env loan under the value-hook loan (a
        // snapshot when a hook can fire) and delivers its raises after.
        // SAFETY: `slots` is laid out by the kernel's ABI (asserted above)
        // and `out` is two words the wrapper fills.
        let ((), mut raises) =
            crate::node::coretraits::with_display_hooks(ctx, event, |env| {
                emit_helpers::with_qop_raises(|| {
                    emit_helpers::with_kernel_env(env, || unsafe {
                        f(slots.as_ptr(), out.as_mut_ptr());
                    })
                })
            });
        for (site, v) in raises.drain(..) {
            // SAFETY: `site` is a `QopSite` constant of the kernel's
            // record, which outlives its code.
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
        let pending = KERNEL_ABORT.with(|c| c.replace(false));
        // Must run before the pending early return. An aborted run
        // reached only a prefix, so its reach count is not a shrink signal.
        if let Some(generation) = shrink_gen {
            let reached = SELF_BLOCK_REACHED.with(|c| c.get());
            if !pending {
                if reached < self.tree_size {
                    // SAFETY: the root words are this kernel's own state,
                    // laid out as the wrapper describes.
                    for b in self.jit.state_self_blocks.iter() {
                        unsafe {
                            reclaim_self_block_tree(
                                (&mut self.state[b.rel as usize]) as *mut u64,
                                &b.layout,
                                generation,
                            )
                        };
                    }
                    if let Some(l) = self.jit.own_site.as_ref() {
                        for b in l.self_blocks.iter() {
                            unsafe {
                                reclaim_self_block_tree(
                                    (&mut self.site[b.rel as usize]) as *mut u64,
                                    &b.layout,
                                    generation,
                                )
                            };
                        }
                    }
                }
                self.tree_size = reached;
            }
            SELF_BLOCK_GEN.with(|c| c.set(saved_gen));
            SELF_BLOCK_REACHED.with(|c| c.set(saved_reached));
        }
        super::emit_helpers::resume_kernel_panic();
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

    fn delete(&mut self, ctx: &mut ExecCtx<R, E>) {
        for feeder in self.feeders.iter_mut() {
            feeder.delete(ctx);
        }
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<R, E>) {
        if crate::dbgenv::gxdbg_kernel_sleep() {
            eprintln!("FUSED-KERNEL-SLEEP {:?}", self.spec.id);
        }
        // Sleep is pause: interior memory survives it.
        self.slept.set();
        for feeder in self.feeders.iter_mut() {
            feeder.sleep(ctx);
        }
    }

    fn reset_replay(&mut self, ctx: &mut ExecCtx<R, E>) {
        // A kernel holds no replay caches; its interior memory is semantic.
        for feeder in self.feeders.iter_mut() {
            feeder.reset_replay(ctx);
        }
    }

    fn typecheck0(&mut self, _ctx: &mut ExecCtx<R, E>) -> Result<()> {
        Ok(())
    }

    fn typecheck1(&mut self, _ctx: &mut ExecCtx<R, E>) -> Result<()> {
        Ok(())
    }

    fn typ(&self) -> &Type {
        &self.typ
    }

    fn refs(&self, refs: &mut Refs) {
        for feeder in self.feeders.iter() {
            feeder.refs(refs);
        }
    }

    fn spec(&self) -> &Expr {
        &self.spec
    }

    fn view(&self) -> NodeView<'_, R, E> {
        NodeView::FusedKernel(self)
    }
}

#[cfg(test)]
mod tests {
    use crate::fusion::{emit::pack_value_to_u64, kernel_abi::PrimType};
    use netidx_value::Value;

    fn unpack_u64_to_value(bits: u64, prim: PrimType) -> Value {
        match prim {
            PrimType::I8 => Value::I8(bits as i8),
            PrimType::I16 => Value::I16(bits as i16),
            PrimType::I32 => Value::I32(bits as i32),
            PrimType::I64 => Value::I64(bits as i64),
            PrimType::U8 => Value::U8(bits as u8),
            PrimType::U16 => Value::U16(bits as u16),
            PrimType::U32 => Value::U32(bits as u32),
            PrimType::U64 => Value::U64(bits),
            PrimType::F32 => Value::F32(f32::from_bits(bits as u32)),
            PrimType::F64 => Value::F64(f64::from_bits(bits)),
            PrimType::Bool => Value::Bool(bits != 0),
        }
    }

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
