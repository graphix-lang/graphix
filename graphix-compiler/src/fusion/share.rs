//! The region kernels a collection's slots share with its prototype.
//! A slot binds its callback at run time, after fusion; the prototype
//! is an instance of the same definition at the same type, fused at
//! compile time. The slot's instance mirrors the prototype's fusion walk
//! and takes each kernel the walk's attempt at the same ordinal built,
//! with fresh state, where the region bakes nothing that differs.

use crate::{
    ApplyView, CompileCtx, ErrorHandler, ExecCtx, Node, NodeView, Rt, UserEvent,
    expr::ExprId,
    fusion::{
        FusedKernel, collect_region_inputs,
        emit::{WrappedKernel, record_decode, record_encode},
        for_each_reachable_node,
        kernel_abi::{KernelSig, SelfBlock, SiteAnchor},
    },
    image::{self, ImageBuf},
    node::genn,
    typ::Type,
};
use anyhow::Result;
use bytes::BufMut;
use netidx_core::pack::{Pack, PackError, decode_varint, encode_varint};
use triomphe::Arc;

/// What a region's code bakes beyond its inputs: the callee bodies it
/// calls statically and, per `?` that raises to a handler, the handler,
/// the raise's top and its expression, in walk order.
struct RegionPrint {
    callees: Vec<ExprId>,
    raises: Vec<(ErrorHandler, ExprId, ExprId)>,
}

impl RegionPrint {
    fn of<R: Rt, E: UserEvent>(node: &Node<R, E>) -> Self {
        let mut print = Self { callees: Vec::new(), raises: Vec::new() };
        for_each_reachable_node(node, &mut |n| match n.view() {
            NodeView::CallSite(cs) => {
                if let Some(ApplyView::Lambda(g)) = cs.resolved_apply() {
                    print.callees.push(g.body().spec().id)
                }
            }
            NodeView::Qop(q) => {
                if let Some(h) = &q.handler {
                    print.raises.push((h.clone(), q.top_id, q.spec.id))
                }
            }
            _ => (),
        });
        print
    }

    /// Where a slot's region, printed `slot`, delivers the raises this
    /// print's code queues: the handlers that differ, or `None` when the
    /// code is not the slot's (another callee, another raise, or one
    /// prototype handler standing for two of the slot's).
    fn redirects(&self, slot: &Self) -> Option<Redirects> {
        if self.callees != slot.callees || self.raises.len() != slot.raises.len() {
            return None;
        }
        let mut all: Vec<Redirect> = Vec::new();
        for ((h0, t0, s0), (h1, t1, s1)) in self.raises.iter().zip(slot.raises.iter()) {
            if s0 != s1 {
                return None;
            }
            match all.iter().find(|((h, t), _)| h.same(h0) && t == t0) {
                Some((_, (h, t))) if h.same(h1) && t == t1 => (),
                Some(_) => return None,
                None => all.push(((h0.clone(), *t0), (h1.clone(), *t1))),
            }
        }
        all.retain(|((h0, t0), (h1, t1))| !(h0.same(h1) && t0 == t1));
        Some(all.into_boxed_slice())
    }
}

/// A prototype handler and the top its raise delivers under, and the
/// slot's in its place.
pub(crate) type Redirect = ((ErrorHandler, ExprId), (ErrorHandler, ExprId));
pub(crate) type Redirects = Box<[Redirect]>;

pub(crate) struct SharedRegion {
    root: ExprId,
    kernel: Arc<KernelSig>,
    jit: WrappedKernel,
    print: RegionPrint,
}

/// The fusion attempts of one prototype walk, by ordinal; `Some` where
/// the attempt built a kernel.
type Table = Arc<Vec<Option<SharedRegion>>>;

/// What the fusion walk does with a region attempt beside building it.
pub(crate) enum Share {
    /// A prototype walk: record every attempt.
    Collect(Vec<Option<SharedRegion>>),
    /// A slot walk: answer attempt `next` from the table, never build.
    Reuse { table: Table, next: usize },
}

/// A collection's part of a table: its prototype's attempts start at
/// `base`.
#[derive(Clone)]
pub(crate) struct SlotShare {
    table: Table,
    base: usize,
}

impl std::fmt::Debug for SlotShare {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SlotShare")
            .field("base", &self.base)
            .field("attempts", &self.table.len())
            .finish()
    }
}

/// Record a prototype walk's attempt at `node`, which built `fused`.
pub(crate) fn record<R: Rt, E: UserEvent>(
    ctx: &mut CompileCtx<R, E>,
    node: &Node<R, E>,
    fused: Option<&mut Node<R, E>>,
) {
    let Some(Share::Collect(attempts)) = &mut ctx.fusion.share else { return };
    let entry =
        fused.and_then(|n| n.downcast_mut::<FusedKernel<R, E>>()).map(|k| SharedRegion {
            root: node.spec().id,
            kernel: k.kernel().clone(),
            jit: k.jit().clone(),
            print: RegionPrint::of(node),
        });
    attempts.push(entry)
}

/// A slot walk's attempt at `node`: the prototype's kernel for it over
/// the slot's own inputs, when the region is the one the prototype
/// fused: the same root, inputs of the same names and kinds in the same
/// order, the same return and the same print.
pub(crate) fn reuse<R: Rt, E: UserEvent>(
    ctx: &mut CompileCtx<R, E>,
    node: &Node<R, E>,
    return_type: &Type,
) -> Option<Node<R, E>> {
    let Some(Share::Reuse { table, next }) = &mut ctx.fusion.share else { return None };
    let (table, at) = (table.clone(), *next);
    *next += 1;
    let entry = table.get(at)?.as_ref()?;
    if entry.root != node.spec().id {
        log::debug!(
            "fusion::share: attempt {at} is {:?}, not {:?}",
            entry.root,
            node.spec().id
        );
        return None;
    }
    if entry.kernel.return_type != *return_type {
        log::debug!("fusion::share: slot region {:?} returns another type", entry.root);
        return None;
    }
    let inputs = collect_region_inputs(&**node, ctx);
    let params = &entry.kernel.params;
    let refused = |why: &str| {
        log::debug!(
            "fusion::share: slot region {:?} keeps its node-walk: {why}",
            entry.root
        );
        None
    };
    if inputs.len() != params.len()
        || !inputs
            .iter()
            .zip(params.iter())
            .all(|(i, p)| i.name == p.name && i.kind == p.kind)
    {
        return refused("its inputs differ");
    }
    let Some(redirects) = entry.print.redirects(&RegionPrint::of(node)) else {
        return refused("its callees or raises differ");
    };
    let top = ctx.fusion.top_id.unwrap_or(node.spec().id);
    let feeders: Box<[Node<R, E>]> = inputs
        .iter()
        .map(|fv| genn::reference::<R, E>(ctx, fv.bind_id, fv.typ.clone(), top))
        .collect();
    let mut fused = FusedKernel::new(
        node.spec().clone(),
        node.typ().clone(),
        entry.kernel.clone(),
        entry.jit.clone(),
        feeders,
    );
    if !redirects.is_empty() {
        let k = fused.downcast_mut::<FusedKernel<R, E>>().expect("a kernel");
        k.redirect(redirects);
    }
    Some(fused)
}

/// Fuse a collection's prototype through `f`: the table its slots
/// share, or `None` when no slot would find a kernel in it.
pub(crate) fn fuse_prototype<R: Rt, E: UserEvent>(
    ctx: &mut CompileCtx<R, E>,
    f: impl FnOnce(&mut CompileCtx<R, E>) -> Result<()>,
) -> Result<Option<SlotShare>> {
    match ctx.fusion.share.take() {
        None => {
            ctx.fusion.share = Some(Share::Collect(Vec::new()));
            let r = f(ctx);
            let Some(Share::Collect(attempts)) = ctx.fusion.share.take() else {
                unreachable!("the walk restores its share")
            };
            r?;
            let any = attempts.iter().any(Option::is_some);
            Ok(any.then(|| SlotShare { table: Arc::new(attempts), base: 0 }))
        }
        Some(Share::Collect(attempts)) => {
            ctx.fusion.share = Some(Share::Collect(attempts));
            f(ctx)?;
            Ok(None)
        }
        Some(Share::Reuse { table, next: base }) => {
            ctx.fusion.share = Some(Share::Reuse { table: table.clone(), next: base });
            f(ctx)?;
            let end = match &ctx.fusion.share {
                Some(Share::Reuse { next, .. }) => (*next).min(table.len()),
                _ => base,
            };
            let any = table[base.min(end)..end].iter().any(Option::is_some);
            Ok(any.then(|| SlotShare { table, base }))
        }
    }
}

/// Fuse a slot's freshly bound instance through `f` from its
/// collection's table. A region the table does not answer node-walks.
pub(crate) fn fuse_slot<R: Rt, E: UserEvent>(
    ctx: &mut CompileCtx<R, E>,
    share: &SlotShare,
    top_id: ExprId,
    f: impl FnOnce(&mut CompileCtx<R, E>) -> Result<()>,
) {
    let reuse = Share::Reuse { table: share.table.clone(), next: share.base };
    let saved = ctx.fusion.share.replace(reuse);
    let saved_top = ctx.fusion.top_id.replace(top_id);
    if let Err(e) = f(ctx) {
        log::debug!("fusion::share: a slot instance kept its node-walk: {e:#}");
    }
    ctx.fusion.share = saved;
    ctx.fusion.top_id = saved_top;
}

impl SlotShare {
    pub(crate) fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        encode_varint(self.base as u64, buf);
        encode_varint(self.table.len() as u64, buf);
        for e in self.table.iter() {
            let Some(e) = e else {
                buf.put_u8(0);
                continue;
            };
            buf.put_u8(1);
            let w = &e.jit;
            e.root.encode(buf)?;
            encode_varint(w.state_words as u64, buf);
            w.slot_table_words.encode(buf)?;
            w.own_site.encode(buf)?;
            w.state_self_blocks.encode(buf)?;
            record_encode(w.wrapper(), buf)?;
            e.print.callees.encode(buf)?;
            encode_varint(e.print.raises.len() as u64, buf);
            for (h, t, s) in e.print.raises.iter() {
                image::handler_encode(h, buf)?;
                t.encode(buf)?;
                s.encode(buf)?;
            }
        }
        Ok(())
    }

    /// Each kernel installs into the context's module from its record.
    pub(crate) fn image_decode<R: Rt, E: UserEvent>(
        ctx: &mut ExecCtx<R, E>,
        buf: &mut &[u8],
    ) -> Result<Self, PackError> {
        let base = decode_varint(buf)? as usize;
        let n = decode_varint(buf)? as usize;
        let mut table = Vec::with_capacity(n.min(1024));
        for _ in 0..n {
            if u8::decode(buf)? == 0 {
                table.push(None);
                continue;
            }
            let root = ExprId::decode(buf)?;
            let state_words = decode_varint(buf)? as usize;
            let slot_table_words: Vec<SiteAnchor> = Pack::decode(buf)?;
            let own_site = Pack::decode(buf)?;
            let state_self_blocks: Vec<SelfBlock> = Pack::decode(buf)?;
            let wrapper = record_decode(buf)?;
            let callees = Pack::decode(buf)?;
            let nraises = decode_varint(buf)? as usize;
            let mut raises = Vec::with_capacity(nraises.min(1024));
            for _ in 0..nraises {
                let h = image::handler_decode(buf)?;
                raises.push((h, ExprId::decode(buf)?, ExprId::decode(buf)?));
            }
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
                        "loading the shared kernel `{}` from the image: {e:#}",
                        wrapper.label
                    );
                    PackError::InvalidFormat
                })?;
            let kernel = wrapper.kernel.clone();
            let print = RegionPrint { callees, raises };
            table.push(Some(SharedRegion { root, kernel, jit, print }));
        }
        Ok(Self { table: Arc::new(table), base })
    }
}
