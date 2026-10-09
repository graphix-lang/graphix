//! [`FusedKernel`]: the `Update` node over a JIT-compiled region. It
//! drives the region's input feeders, packs their values across the
//! JIT ABI boundary, and unpacks the result. It is spliced when its
//! region emits and gets its entry when the pass links; a region that
//! fails to emit is never spliced and its nodes keep node-walking.

#[cfg(debug_assertions)]
use crate::fusion::emit_helpers::record_fusion_invocation;
use crate::{
    BindId, CompileCtx, ExecCtx, Node, NodeView, Refs, Rt, Update, UserEvent,
    analysis::RegionFacts,
    cost::ForkSite,
    expr::Expr,
    fusion::{
        emit::{KernelType, WrappedKernel, pack_value_to_u64, prim_to_value_disc},
        emit_helpers::{
            self, EMPTY_ARR, KERNEL_ABORT, SELF_BLOCK_GEN, SELF_BLOCK_MADE,
            SELF_BLOCK_REACHED, TagValue, free_self_block_tree, free_slot_chain,
            reclaim_self_block_tree, reclaim_slot_chain,
        },
        kernel_abi::{self, KernelSig, ParamKind},
        share::Redirects,
    },
    image::{
        self, ImageBuf,
        nodes::{NodeTag, decode_nodes, encode_nodes, put_tag},
    },
    node::WakeBit,
    tval::{Tag, value_words},
    typ::Type,
};
use anyhow::Result;
use netidx_core::pack::{Pack, PackError};
use netidx_value::Value;
use poolshark::local::LPooled;
use std::sync::LazyLock;
use triomphe::Arc;

/// The placeholder words of an absent composite input.
static EMPTY_ARRAY: LazyLock<Value> = LazyLock::new(|| Value::Array(EMPTY_ARR.clone()));

/// An `Update` node over a compiled kernel and its input feeders.
pub struct FusedKernel<R: Rt, E: UserEvent> {
    spec: Expr,
    /// Owns the typedefs it names: one the replaced region declared is
    /// deleted with it.
    typ: KernelType,
    /// What the region it replaced did (`analysis::region_facts`).
    facts: RegionFacts,
    /// One feeder Node per kernel input slot.
    feeders: Box<[Node<R, E>]>,
    fork: ForkSite,
    /// Set by `sleep()`, taken by the next update; feeds wire slot 0 bit 1.
    slept: WakeBit,
    /// The ABI contract; the `Arc` pointer is also the kernel's identity
    /// in the JIT's `by_kernel` cache.
    kernel: Arc<KernelSig>,
    jit: WrappedKernel,
    /// Per-instance state words (wire slot 1): prev-length and first-call
    /// words. Zero means "no previous observation"; consumers store
    /// `value + 1`.
    state: Box<[u64]>,
    /// The last result; ridden when no feeder fired. Bottom feeders may
    /// belong to untaken branches, so only running the kernel decides
    /// output validity.
    resident: TagValue,
    /// `self_gen` is stamped into every activation block reached by an
    /// invocation; blocks left unstamped are freed afterwards. The walk
    /// runs only when the reach count falls below the live blocks:
    /// `tree_size` plus those the invocation made.
    self_gen: u64,
    /// The activation blocks live after the last invocation.
    tree_size: u64,
    /// A slot's kernel shared with its prototype delivers these raises
    /// to the slot's handlers (`fusion::share`).
    redirects: Redirects,
}

impl<R: Rt, E: UserEvent> Drop for FusedKernel<R, E> {
    fn drop(&mut self) {
        // Only instance death frees the slot chains and activation trees;
        // `sleep` does not touch them.
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
        facts: RegionFacts,
        kernel: Arc<KernelSig>,
        jit: WrappedKernel,
        feeders: Box<[Node<R, E>]>,
    ) -> Node<R, E> {
        debug_assert_eq!(feeders.len(), kernel.params.len(), "one feeder per param");
        let state = vec![0u64; jit.state_words].into_boxed_slice();
        Node::new(Self {
            spec,
            typ: KernelType::new(typ),
            facts,
            feeders,
            fork: ForkSite::default(),
            slept: WakeBit::default(),
            kernel,
            jit,
            state,
            resident: TagValue::phantom(),
            self_gen: 0,
            tree_size: 0,
            redirects: Box::default(),
        })
    }

    /// What the region it replaced did.
    pub(crate) fn facts(&self) -> RegionFacts {
        self.facts
    }

    /// The kernel signature this region fused into.
    pub fn kernel(&self) -> &Arc<KernelSig> {
        &self.kernel
    }

    pub(crate) fn jit(&self) -> &WrappedKernel {
        &self.jit
    }

    pub(crate) fn redirect(&mut self, redirects: Redirects) {
        self.redirects = redirects
    }

    /// The input a read of `id` feeds.
    fn input(&self, id: BindId) -> Option<usize> {
        self.feeders
            .iter()
            .position(|f| matches!(f.view(), NodeView::Ref(r) if r.id == id))
    }

    /// Whether a read of `id` feeds an input.
    pub(crate) fn has_input(&self, id: BindId) -> bool {
        self.input(id).is_some()
    }

    /// Feed the input a read of `id` feeds ([`Self::has_input`]) from
    /// `node` instead.
    pub(crate) fn feed(
        &mut self,
        ctx: &mut CompileCtx<R, E>,
        id: BindId,
        node: Node<R, E>,
    ) {
        let i = self.input(id).expect("an input read from the id");
        ctx.discard(std::mem::replace(&mut self.feeders[i], node));
    }

    /// The feeder nodes, one per kernel input slot.
    pub fn feeders(&self) -> &[Node<R, E>] {
        &self.feeders
    }

    /// Rebuild a region from an image: the wrapper record installs into
    /// the context's module and the node allocates fresh state.
    pub(crate) fn image_decode(
        ctx: &mut ExecCtx<'_, R, E>,
        buf: &mut &[u8],
    ) -> Result<Node<R, E>, PackError> {
        let spec = Expr::decode(buf)?;
        let typ = Type::decode(buf)?;
        let facts = RegionFacts::decode(buf)?;
        let feeders = decode_nodes(ctx, buf)?.into_boxed_slice();
        let (jit, kernel) = WrappedKernel::image_decode(&ctx.fusion, buf)?;
        if feeders.len() != kernel.params.len() {
            return Err(PackError::InvalidFormat);
        }
        Ok(Self::new(spec, typ, facts, kernel, jit, feeders))
    }

    /// Nothing ran yet: every word the image does not carry is initial.
    fn quiescent(&self) -> bool {
        let mut slept = self.slept;
        self.state.iter().all(|w| *w == 0)
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
        let flag = (tag.bits() as u64) << 56;
        if tag.is_bottom() {
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
    fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        if !self.quiescent() || !self.redirects.is_empty() {
            return Err(PackError::Application(image::NOT_QUIESCENT));
        }
        let w = &self.jit;
        put_tag(NodeTag::Fused, buf);
        self.spec.encode(buf)?;
        self.typ.typ.encode(buf)?;
        self.facts.encode(buf)?;
        encode_nodes(&self.feeders, buf)?;
        w.image_encode(buf)
    }

    fn update(&mut self, ctx: &mut ExecCtx<'_, R, E>) -> &TagValue {
        let woke = self.slept.take();
        // the feeders are a fork point, as a call's arguments are
        let (_, mut polled) = crate::node::gather(ctx, &mut self.feeders, &mut self.fork);
        let any_updated = polled.iter().any(|tv| tv.tag().triggers());
        let any_bottom = polled.iter().any(|tv| tv.tag().is_bottom());
        if crate::dbgenv::gxdbg_kpoll() {
            eprintln!(
                "KPOLL {} init={} any_updated={any_updated} tags={:?} present={:?}",
                self.kernel.fn_name,
                ctx.event.init(),
                polled.iter().map(|tv| tv.tag().bits()).collect::<Vec<_>>(),
                polled.iter().map(|tv| !tv.is_bottom()).collect::<Vec<_>>(),
            );
        }
        if !(any_updated
            || any_bottom
            || ctx.event.init()
            || woke
            || self.resident.tag().is_bottom())
        {
            return self.resident.ride();
        }
        if crate::dbgenv::graphix_dbg_invoke() {
            eprintln!(
                "KERNEL INVOKE {} init={} fired={:?} present={:?}",
                self.kernel.fn_name,
                ctx.event.init(),
                polled.iter().map(|tv| tv.is_fired()).collect::<Vec<_>>(),
                polled.iter().map(|tv| !tv.is_bottom()).collect::<Vec<_>>()
            );
        }
        #[cfg(debug_assertions)]
        record_fusion_invocation();
        let mut slots: LPooled<Vec<u64>> = LPooled::take();
        // slot 0: the context word
        let wake = ctx.event.wake() || woke;
        slots.push(kernel_abi::ctx_word(ctx.event.init(), wake));
        slots.push(if self.state.is_empty() {
            0
        } else {
            self.state.as_mut_ptr() as u64
        });
        // a region parent claims no site words
        slots.push(0);
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
        // A run nested in another on this thread (a stolen pool job, a
        // value hook) must neither see nor clobber the enclosing run's
        // abort, panic or reach count: each is saved and restored.
        let outer_abort = KERNEL_ABORT.with(|c| c.replace(false));
        let outer_panic = emit_helpers::take_kernel_panic();
        let has_self_blocks = !self.jit.state_self_blocks.is_empty()
            || self.jit.slot_table_words.iter().any(|a| a.roots_trees());
        let shrink_gen = has_self_blocks.then(|| {
            self.self_gen = self.self_gen.wrapping_add(1);
            self.self_gen
        });
        let saved_gen = SELF_BLOCK_GEN.with(|c| c.replace(shrink_gen.unwrap_or(0)));
        let saved_reached = SELF_BLOCK_REACHED.with(|c| c.replace(0));
        let saved_made = SELF_BLOCK_MADE.with(|c| c.replace(0));
        // The run reads its env loan under the value-hook loan (a
        // snapshot when a hook can fire) and delivers its raises after.
        // SAFETY: `slots` is laid out by the kernel's ABI (asserted above)
        // and `out` is two words the wrapper fills.
        let body = ctx.fork.body();
        let loan = super::par_loop::ParLoan {
            mode: ctx.fork_mode(),
            body_mode: ctx.with_fork_flags(body, |ctx| ctx.fork_mode()),
            forced: ctx.fork.forced,
            control: &**ctx.control,
        };
        let mut run = |env: &crate::env::Env| {
            emit_helpers::with_qop_raises(|| {
                emit_helpers::with_kernel_env(env, || {
                    super::par_loop::with_par_loan(Some(loan), || unsafe {
                        f(slots.as_ptr(), out.as_mut_ptr());
                    })
                })
            })
        };
        // only a kernel that meets an abstract value takes the value-hook
        // loan, under which its loops never fork
        let ((), mut raises) = match self.jit.meets_abstract {
            true => crate::node::coretraits::with_display_hooks(ctx, run),
            false => run(&ctx.env),
        };
        for (site, v) in raises.drain(..) {
            // SAFETY: `site` is a `QopSite` constant of the kernel's
            // record, which outlives its code.
            let site = unsafe { &*site };
            let (handler, top) = self
                .redirects
                .iter()
                .find(|((h, t), _)| h.same(&site.handler) && *t == site.own_top)
                .map_or((&site.handler, site.own_top), |(_, (h, t))| (h, *t));
            if let Value::Error(e) = v {
                handler.raise();
                crate::node::error::deliver_error(
                    ctx,
                    handler,
                    top,
                    &site.spec,
                    (*e).clone(),
                );
            }
        }
        let pending = KERNEL_ABORT.with(|c| c.replace(outer_abort));
        let panic = emit_helpers::take_kernel_panic();
        if let Some(p) = outer_panic {
            emit_helpers::set_kernel_panic(p)
        }
        // Must run before the pending early return. An aborted run
        // reached only a prefix, so its reach count is not a shrink signal.
        let made = SELF_BLOCK_MADE.with(|c| c.get());
        if let Some(generation) = shrink_gen {
            let reached = SELF_BLOCK_REACHED.with(|c| c.get());
            // what was live plus what this run made; an aborted run only adds
            let live = self.tree_size + made;
            if pending {
                self.tree_size = live;
            } else {
                if reached < live {
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
                    for a in self.jit.slot_table_words.iter() {
                        unsafe {
                            reclaim_slot_chain(
                                (&mut self.state[a.rel as usize]) as *mut u64,
                                a.own_levels as u64,
                                a.leaf.as_deref(),
                                generation,
                            )
                        };
                    }
                }
                self.tree_size = reached;
            }
        }
        SELF_BLOCK_GEN.with(|c| c.set(saved_gen));
        SELF_BLOCK_REACHED.with(|c| c.set(saved_reached));
        SELF_BLOCK_MADE.with(|c| c.set(saved_made));
        if let Some(p) = panic {
            std::panic::resume_unwind(p)
        }
        if pending {
            // The out slot is a sentinel, not a Value.
            if crate::dbgenv::graphix_dbg_invoke() {
                eprintln!("KERNEL RESULT {} PENDING", self.kernel.fn_name);
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

    fn delete(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        for feeder in self.feeders.iter_mut() {
            feeder.delete(ctx);
        }
    }

    fn sleep(&mut self, ctx: &mut ExecCtx<'_, R, E>) {
        if crate::dbgenv::gxdbg_kernel_sleep() {
            eprintln!("FUSED-KERNEL-SLEEP {:?}", self.spec.id);
        }
        // Sleep is pause: interior memory survives it.
        self.slept.set();
        for feeder in self.feeders.iter_mut() {
            feeder.sleep(ctx);
        }
    }

    fn typecheck0(&mut self, _ctx: &mut CompileCtx<R, E>) -> Result<()> {
        Ok(())
    }

    fn typecheck1(&mut self, _ctx: &mut CompileCtx<R, E>) -> Result<()> {
        Ok(())
    }

    fn typ(&self) -> &Type {
        &self.typ.typ
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
