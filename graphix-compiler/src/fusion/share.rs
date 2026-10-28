//! The region kernels a collection's slots share with its prototype.
//! A slot binds its callback at run time, after fusion; the prototype
//! is an instance of the same definition at the same type, fused at
//! compile time. The slot's instance mirrors the prototype's fusion walk
//! and takes each kernel the walk's attempt at the same ordinal built,
//! with fresh state, where the region bakes nothing that differs.

use crate::{
    ApplyView, CompileCtx, ErrorHandler, ExecCtx, Node, NodeView, Rt,
    analysis::RegionFacts,
    expr::ExprId,
    fusion::{
        FusedKernel, FusionAttempt, collect_region_inputs, emit::WrappedKernel,
        for_each_reachable_node, kernel_abi::KernelSig,
    },
    image::{self, ImageBuf},
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
    fn of<R: Rt>(node: &Node<R>) -> Self {
        let mut print = Self { callees: Vec::new(), raises: Vec::new() };
        for_each_reachable_node(node, &mut |n| match n.view() {
            NodeView::CallSite(cs) => {
                if let Some(ApplyView::Lambda(g)) = cs.resolved_apply() {
                    print.callees.push(g.body().spec().id)
                }
            }
            NodeView::Qop(q) => {
                if let Some((h, top_id)) = q.raise_site() {
                    print.raises.push((h.clone(), top_id, q.spec.id))
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
    facts: RegionFacts,
    print: RegionPrint,
}

impl SharedRegion {
    fn with(
        root: ExprId,
        kernel: Arc<KernelSig>,
        jit: WrappedKernel,
        facts: RegionFacts,
        print: RegionPrint,
    ) -> Self {
        SharedRegion { root, kernel, jit, facts, print }
    }
}

pub(crate) enum SharedAttempt {
    Fused(SharedRegion),
    Refused { root: ExprId, blocker: Option<ExprId> },
}

impl SharedAttempt {
    fn fused(&self) -> bool {
        matches!(self, Self::Fused(_))
    }

    fn refused(root: ExprId, blocker: Option<ExprId>) -> Self {
        SharedAttempt::Refused { root, blocker }
    }
}

type Table = Arc<Vec<SharedAttempt>>;

/// What the fusion walk does with a region attempt beside building it.
pub(crate) enum Share {
    /// A prototype walk: record every attempt.
    Collect(Vec<SharedAttempt>),
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

/// Record a prototype walk's outcome at `node`.
pub(crate) fn record<R: Rt>(
    ctx: &mut CompileCtx<R>,
    node: &Node<R>,
    attempt: &mut FusionAttempt<R>,
) {
    let Some(Share::Collect(attempts)) = &mut ctx.fusion.share else { return };
    let entry = match attempt {
        FusionAttempt::Fused(n) => {
            let k = n
                .downcast_mut::<FusedKernel<R>>()
                .expect("a fused attempt builds a kernel");
            SharedAttempt::Fused(SharedRegion::with(
                node.spec().id,
                k.kernel().clone(),
                k.jit().clone(),
                k.facts(),
                RegionPrint::of(node),
            ))
        }
        FusionAttempt::Refused(blocker) => {
            SharedAttempt::refused(node.spec().id, *blocker)
        }
    };
    attempts.push(entry)
}

/// A slot walk's attempt at `node`: the prototype's kernel for it over
/// the slot's own inputs, when the region is the one the prototype
/// fused: the same root, inputs of the same names and kinds in the same
/// order, the same return and the same print.
pub(crate) fn reuse<R: Rt>(
    ctx: &mut CompileCtx<R>,
    node: &Node<R>,
    return_type: &Type,
) -> FusionAttempt<R> {
    let Some(Share::Reuse { table, next }) = &mut ctx.fusion.share else {
        return FusionAttempt::Refused(None);
    };
    let (table, at) = (table.clone(), *next);
    *next += 1;
    let entry = match table.get(at) {
        Some(SharedAttempt::Fused(entry)) => entry,
        Some(SharedAttempt::Refused { root, blocker }) if *root == node.spec().id => {
            return FusionAttempt::Refused(*blocker);
        }
        _ => return FusionAttempt::Refused(None),
    };
    if entry.root != node.spec().id {
        log::debug!(
            "fusion::share: attempt {at} is {:?}, not {:?}",
            entry.root,
            node.spec().id
        );
        return FusionAttempt::Refused(None);
    }
    if entry.kernel.return_type != *return_type {
        log::debug!("fusion::share: slot region {:?} returns another type", entry.root);
        return FusionAttempt::Refused(None);
    }
    let inputs = collect_region_inputs(&**node, ctx);
    let params = &entry.kernel.params;
    let refused = |why: &str| {
        log::debug!(
            "fusion::share: slot region {:?} keeps its node-walk: {why}",
            entry.root
        );
        FusionAttempt::Refused(None)
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
    let mut fused = super::kernel_node(
        ctx,
        node,
        entry.facts,
        &inputs,
        entry.kernel.clone(),
        entry.jit.clone(),
    );
    if !redirects.is_empty() {
        let k = fused.downcast_mut::<FusedKernel<R>>().expect("a kernel");
        k.redirect(redirects);
    }
    if !ctx.attr_census.lock().is_empty() {
        let decorated = super::discover_lambda_calls(node, ctx);
        ctx.attr_absorbed.lock().extend(decorated.decorated.iter().map(|n| n.spec().id));
    }
    FusionAttempt::Fused(fused)
}

/// Fuse a collection's prototype through `f`: the table its slots
/// share, or `None` when no slot would find a kernel in it.
pub(crate) fn fuse_prototype<R: Rt>(
    ctx: &mut CompileCtx<R>,
    f: impl FnOnce(&mut CompileCtx<R>) -> Result<()>,
) -> Result<Option<SlotShare>> {
    match ctx.fusion.share.take() {
        None => {
            ctx.fusion.share = Some(Share::Collect(Vec::new()));
            let r = f(ctx);
            let Some(Share::Collect(attempts)) = ctx.fusion.share.take() else {
                unreachable!("the walk restores its share")
            };
            r?;
            let any = attempts.iter().any(SharedAttempt::fused);
            Ok(any.then(|| SlotShare::with(Arc::new(attempts), 0)))
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
            let any = table[base.min(end)..end].iter().any(SharedAttempt::fused);
            Ok(any.then(|| SlotShare::with(table, base)))
        }
    }
}

/// Fuse a slot's freshly bound instance through `f` from its
/// collection's table. A region the table does not answer node-walks.
pub(crate) fn fuse_slot<R: Rt>(
    ctx: &mut CompileCtx<R>,
    share: &SlotShare,
    top_id: ExprId,
    f: impl FnOnce(&mut CompileCtx<R>) -> Result<()>,
) -> Result<()> {
    let reuse = Share::Reuse { table: share.table.clone(), next: share.base };
    let saved = ctx.fusion.share.replace(reuse);
    let saved_top = ctx.fusion.top_id.replace(top_id);
    let result = f(ctx);
    ctx.fusion.share = saved;
    ctx.fusion.top_id = saved_top;
    result
}

impl SlotShare {
    fn with(table: Table, base: usize) -> Self {
        SlotShare { table, base }
    }

    pub(crate) fn image_encode(&self, buf: &mut ImageBuf) -> Result<(), PackError> {
        encode_varint(self.base as u64, buf);
        encode_varint(self.table.len() as u64, buf);
        for e in self.table.iter() {
            let e = match e {
                SharedAttempt::Fused(e) => e,
                SharedAttempt::Refused { root, blocker } => {
                    buf.put_u8(0);
                    root.encode(buf)?;
                    blocker.encode(buf)?;
                    continue;
                }
            };
            buf.put_u8(1);
            e.root.encode(buf)?;
            e.facts.encode(buf)?;
            e.jit.image_encode(buf)?;
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
    pub(crate) fn image_decode<R: Rt>(
        ctx: &mut ExecCtx<'_, R>,
        buf: &mut &[u8],
    ) -> Result<Self, PackError> {
        let base = decode_varint(buf)? as usize;
        let n = decode_varint(buf)? as usize;
        let mut table = Vec::with_capacity(n.min(1024));
        for _ in 0..n {
            match u8::decode(buf)? {
                0 => {
                    table.push(SharedAttempt::refused(
                        ExprId::decode(buf)?,
                        Option::<ExprId>::decode(buf)?,
                    ));
                    continue;
                }
                1 => (),
                _ => return Err(PackError::InvalidFormat),
            }
            let root = ExprId::decode(buf)?;
            let facts = RegionFacts::decode(buf)?;
            let (jit, kernel) = WrappedKernel::image_decode(&ctx.fusion, buf)?;
            let callees = Pack::decode(buf)?;
            let nraises = decode_varint(buf)? as usize;
            let mut raises = Vec::with_capacity(nraises.min(1024));
            for _ in 0..nraises {
                let h = image::handler_decode(buf)?;
                raises.push((h, ExprId::decode(buf)?, ExprId::decode(buf)?));
            }
            let print = RegionPrint { callees, raises };
            table.push(SharedAttempt::Fused(SharedRegion::with(
                root, kernel, jit, facts, print,
            )));
        }
        Ok(Self::with(Arc::new(table), base))
    }
}
