//! A loop at a kernel body's top level, outlined into a chunk: a
//! function of its own that runs a range of the loop's slots, which
//! `graphix_par_loop` runs in order or forked (`fusion::par_loop`,
//! `design/parallel_eval.md` §10).

use super::{
    abi::{JitEnv, Local, STALE, ValueVar, local_payload_ty},
    body::{BodyCx, emit_kernel_abort},
    call::{BufKind, close_buf, open_buf},
    jit::{ChunkFn, chunk_signature},
    lower::{
        Channel, ClosedFrame, CtxWord, HelperRefs, KernelFns, LowerCtx, SlotTable,
        SlotTableFrame, StateChannel, TailCtx, TruncRec,
    },
    record::KernelConst,
    scaffold::LoopBody,
    scaffold::{Accs, Iteration, LoopFrame, Sink, SinkKind, Slots, Sunk, open_sink},
};
use crate::{
    cost::LoopSite,
    fusion::{
        kernel_abi::KernelKey,
        par_loop::{FIND, OUT_WORDS, ROOT},
    },
};
use anyhow::Result;
use cranelift_codegen::ir::{
    FuncRef, Function, InstBuilder, MemFlags, StackSlotData, StackSlotKind, UserFuncName,
    Value as ClifValue, types,
};
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};
use cranelift_module::FuncId;
use smallvec::SmallVec;
use std::{
    cell::{Cell, RefCell},
    collections::BTreeMap,
};
use triomphe::Arc;

/// The loop to outline: its sink, its source and disc, its slot count
/// and the sites in its body that keep per-slot state.
pub(super) struct Loop<'s> {
    pub(super) kind: SinkKind,
    pub(super) src: ClifValue,
    pub(super) src_disc: ClifValue,
    pub(super) len: ClifValue,
    /// The slots the loop instance entered before this run.
    pub(super) entered: ClifValue,
    pub(super) facts: &'s LoopBody,
}

/// The frame a chunk reads: the context word, the state and site
/// pointers, the source, its disc, the slot count and the slots entered
/// before, then the loop's slot-table bases, then a `(disc, payload)`
/// pair per local in scope that the iteration reads ([`handed`]).
const CTX: usize = 0;
const STATE: usize = 1;
const SITE: usize = 2;
const SRC: usize = 3;
const SRC_DISC: usize = 4;
const LEN: usize = 5;
const ENTERED: usize = 6;
const HEADER: usize = 7;

fn word(i: usize) -> i32 {
    (8 * i) as i32
}

/// Emit `iteration` as the chunk of a loop at the body's top level and
/// the call that runs its slots, folding them into `accs`.
pub(super) fn emit_outlined(
    cx: &mut BodyCx,
    lp: Loop,
    accs: Accs,
    iteration: impl Iteration,
) -> Result<Sunk> {
    // The loop's slot tables are made here, where every chunk finds them.
    let tables = cx.claim_slot_tables(&lp.facts.sites, lp.len, lp.src_disc)?;
    let site = cx.const_ptr(KernelConst::LoopSite(Arc::new(LoopSite::default())))?;
    let (id, pending) = emit_chunk(cx, &lp, &tables, iteration)?;
    // The chain levels at the loop's own depth are sized before any
    // chunk runs, so a chunk's ensure of one only reads it.
    *cx.ctx.closed_frame.borrow_mut() =
        Some(ClosedFrame { depth: 1, len: lp.len, src_disc: lp.src_disc, pending });
    cx.emit_slot_truncates()?;
    let locals: SmallVec<[ValueVar; 16]> =
        handed(cx.env, lp.facts).map(|l| l.words).collect();
    let (fbase, obase) = stack_record(cx, HEADER + tables.len() + 2 * locals.len());
    let ctx_word =
        CtxWord { born: cx.ctx.born_flag, wake: cx.ctx.wake_flag }.encode(cx.b);
    let header = [
        ctx_word,
        cx.state_ptr(),
        cx.site_ptr(),
        lp.src,
        lp.src_disc,
        lp.len,
        lp.entered,
    ];
    let bases = tables.iter().map(|t| t.base);
    for (i, v) in header.into_iter().chain(bases).enumerate() {
        cx.b.ins().store(MemFlags::trusted(), v, fbase, word(i));
    }
    let base = HEADER + tables.len();
    for (i, vv) in locals.iter().enumerate() {
        let disc = cx.b.use_var(vv.disc);
        let payload = cx.b.use_var(vv.payload);
        cx.b.ins().store(MemFlags::trusted(), disc, fbase, word(base + 2 * i));
        cx.b.ins().store(MemFlags::trusted(), payload, fbase, word(base + 2 * i + 1));
    }
    let chunk_ref = cx.ctx.names.borrow().import_func(id, cx.b.func);
    let chunk = cx.b.ins().func_addr(types::I64, chunk_ref);
    let mut kind = 0;
    if lp.kind == SinkKind::Find {
        kind |= FIND;
    }
    if cx.ctx.claims == Channel::State {
        kind |= ROOT;
    }
    let kind = cx.b.ins().iconst(types::I64, kind as i64);
    let run = super::scaffold::slots_run(cx, lp.len, lp.src_disc);
    let call =
        cx.call_helper("graphix_par_loop", &[chunk, fbase, run, site, kind, obase])?;
    let aborted = call[0];
    let abort_bl = cx.b.create_block();
    let cont_bl = cx.b.create_block();
    cx.b.ins().brif(aborted, abort_bl, &[], cont_bl, &[]);
    cx.b.switch_to_block(abort_bl);
    cx.b.seal_block(abort_bl);
    emit_kernel_abort(cx)?;
    cx.b.switch_to_block(cont_bl);
    cx.b.seal_block(cont_bl);
    let out = |cx: &mut BodyCx, i: usize| {
        cx.b.ins().load(types::I64, MemFlags::trusted(), obase, word(i))
    };
    let taint = out(cx, 0);
    let cur = cx.b.use_var(accs.taint);
    let taint = cx.b.ins().bor(cur, taint);
    cx.b.def_var(accs.taint, taint);
    let stale = out(cx, 1);
    let cur = cx.b.use_var(accs.stale);
    let stale = cx.b.ins().band(cur, stale);
    cx.b.def_var(accs.stale, stale);
    Ok(match lp.kind {
        SinkKind::Buf => Sunk::Buf(out(cx, 2)),
        SinkKind::Find => Sunk::Find(out(cx, 3), out(cx, 4)),
    })
}

// CR claude for claude: [perf/medium] [perf/medium] Every outlined loop stores
// every local in scope into its frame record, and its chunk loads and binds all of
// them back, though an iteration reads only its callback's free locals. A kernel
// with M loops and L locals therefore emits M x L stores, executed at every kernel
// run, and L loads per chunk. Separately, `call::emit_pending_abort`'s rungs
// `use_var` every owned local, so the SSA builder passes all of them as block
// arguments from every abort exit (M x L again). For a chain of n `array::map`
// lets: 10900 stores at n=100, 41800 at 200, 163600 at 400, and first value at 0.64
// s / 2.09 s / 8.09 s for n=400 / 800 / 1600 (debug, mostly phase Link). Fix: store
// and bind only the locals the iteration reads (the outer reads
// `lowering::ReadsMemo` already computes for captures). Spill each owned local once
// where it is bound and have the rungs load it. probe:
// design/review-2026-10-19/repro/x-perf-compile-outline-frame-stores.sh
// (x-perf-compile-01)
// 2026-10-20 claude: The frame half is done (see x-rerun-6-05 beside this: only the
// locals the iteration reads are stored and bound). Still open:
// call::emit_pending_abort rungs use_var every owned local, so every abort exit
// passes all of them as block arguments.
// 2026-10-20 claude: Measured the abort-rung half after the frame fix: in the chain
// probe block params grow as n^2 (41k/162k/645k at n=200/400/800) while
// instructions grow linearly (82 per loop), and the Cranelift backend on the one
// large kernel takes 0.41/1.96/7.0 s at n=800/1600/3200 (1.25/4.7 s at 1600/3200
// with the verifier off; opt_level none does not help), the end-to-end run
// 1.0/3.5/11.2 s. The fix stands as written, with one hazard: a tail loop redefines
// its formals variables every iteration (body.rs, the staged def_vars before the
// jump to the head), so a spill slot must be written at every def_var of an owned
// local, not only at bind_local, or a rung drops a stale value.
// XCR claude for claude: [perf/low] Every outlined loop stores every local in scope
// into the frame, and emit_chunk loads every one back, whether the loop's body
// reads it or not. A chain of n array::map lets in one function emits about n^2/2
// store pairs: 6478 stores at n=80 and 25758 at n=160, while calls and slot bytes
// grow linearly. A fused build takes 2.1 s at n=800 against 0.36 s node-walked. Lay
// the frame out over the locals the iteration reads (its free local names), or
// store each local once when it is bound. probe:
// design/review-2026-10-19/repro/x-rerun-6-outline-stores-quadratic.sh
// (x-rerun-6-05)
// 2026-10-20 claude: An outlined loop hands its chunk only the locals the iteration
// reads (its body and callees, Refs external reads: scaffold::loop_body) plus every
// id-less synthetic local; the probe x-rerun-6-outline-stores-quadratic.sh now
// prints 78/158/318 stores at n=40/80/160 (was 6478 at 80), and a chain of 800 maps
// runs in 0.77 s (was 2.1 s). A read the filter missed would refuse the region,
// which regress fusion manifest catches (552, 0 mismatches).
/// The locals a chunk is handed: those the iteration reads, and every
/// synthetic one, which no read names by id.
fn handed<'e>(env: &'e JitEnv, facts: &'e LoopBody) -> impl Iterator<Item = &'e Local> {
    env.locals.iter().filter(|l| l.bind_id.is_none_or(|id| facts.reads.contains(&id)))
}

fn stack_record(cx: &mut BodyCx, words: usize) -> (ClifValue, ClifValue) {
    let size = (8 * (words + OUT_WORDS)) as u32;
    let slot = match cx.ctx.outline_frame.get() {
        Some(slot) => {
            let record = &mut cx.b.func.sized_stack_slots[slot];
            record.size = record.size.max(size);
            slot
        }
        None => {
            let data = StackSlotData::new(StackSlotKind::ExplicitSlot, size, 3);
            let slot = cx.b.create_sized_stack_slot(data);
            cx.ctx.outline_frame.set(Some(slot));
            slot
        }
    };
    let out = cx.b.ins().stack_addr(types::I64, slot, 0);
    let frame = cx.b.ins().stack_addr(types::I64, slot, word(OUT_WORDS));
    (frame, out)
}

/// A state channel of the body continued in its chunk: the chunk claims
/// where the body left off, and [`give_back`] returns what it claimed.
fn continued(ch: &StateChannel, ptr: ClifValue) -> StateChannel {
    StateChannel {
        ptr,
        next: Cell::new(ch.next.get()),
        anchors: RefCell::new(ch.anchors.take()),
        self_blocks: RefCell::new(ch.self_blocks.take()),
    }
}

fn give_back(ch: &StateChannel, from: StateChannel) {
    ch.next.set(from.next.get());
    ch.anchors.replace(from.anchors.into_inner());
    ch.self_blocks.replace(from.self_blocks.into_inner());
}

/// Build the chunk `(frame, lo, hi, out)` that runs slots `lo..hi` of
/// `lp`, over the slot tables `tables` and the body's locals, and add
/// it to the body's chunks. Returns its id and its nested sites' chain
/// records, for the body to size.
fn emit_chunk(
    cx: &mut BodyCx,
    lp: &Loop,
    tables: &[SlotTable],
    iteration: impl Iteration,
) -> Result<(FuncId, Vec<TruncRec>)> {
    let p = cx.ctx;
    let sig = chunk_signature(cx.b.func.signature.call_conv);
    let mut names = p.names.borrow_mut();
    let id = names.local(&sig);
    let names = RefCell::new(&mut **names);
    let mut func = Function::with_name_signature(UserFuncName::user(0, id.as_u32()), sig);
    let mut fctx = FunctionBuilderContext::new();
    let mut b = FunctionBuilder::new(&mut func, &mut fctx);
    let entry = b.create_block();
    b.append_block_params_for_function_params(entry);
    b.switch_to_block(entry);
    b.seal_block(entry);
    let params = b.block_params(entry);
    let (frame, lo, hi, out) = (params[0], params[1], params[2], params[3]);
    let load = |b: &mut FunctionBuilder, ty, i| {
        b.ins().load(ty, MemFlags::trusted(), frame, word(i))
    };
    let ctx_word = load(&mut b, types::I64, CTX);
    let state = load(&mut b, types::I64, STATE);
    let site = load(&mut b, types::I64, SITE);
    let src = load(&mut b, types::I64, SRC);
    let src_disc = load(&mut b, types::I64, SRC_DISC);
    let len = load(&mut b, types::I64, LEN);
    let entered = load(&mut b, types::I64, ENTERED);
    let tables: Vec<SlotTable> = tables
        .iter()
        .enumerate()
        .map(|(j, t)| SlotTable {
            site: t.site,
            base: load(&mut b, types::I64, HEADER + j),
            guarded: t.guarded,
            blocks: t.blocks,
        })
        .collect();
    // The body's locals, borrowed: the body drops them.
    let mut env = JitEnv::new();
    let base = HEADER + tables.len();
    for (i, l) in handed(cx.env, lp.facts).enumerate() {
        let disc = load(&mut b, types::I64, base + 2 * i);
        let payload = load(&mut b, local_payload_ty(l.kind), base + 2 * i + 1);
        let words = ValueVar {
            disc: b.declare_var(types::I64),
            payload: b.declare_var(local_payload_ty(l.kind)),
        };
        b.def_var(words.disc, disc);
        b.def_var(words.payload, payload);
        env.bind(l.name.clone(), words, l.kind, l.bind_id);
    }
    let CtxWord { born: born_flag, wake: wake_flag } = CtxWord::decode(&mut b, ctx_word);
    let tail_scrut_stale_acc = b.declare_var(types::I64);
    let stale = b.ins().iconst(types::I64, STALE);
    b.def_var(tail_scrut_stale_acc, stale);
    let callee_refs: BTreeMap<KernelKey, KernelFns<FuncRef>> = p
        .callee_ids
        .iter()
        .map(|(k, ids)| {
            let names = names.borrow();
            let body = names.import_func(ids.body, b.func);
            let thunk = names.import_func(ids.thunk, b.func);
            (*k, KernelFns { body, thunk, checked: ids.checked })
        })
        .collect();
    let consts = RefCell::new(Vec::new());
    let ctx = LowerCtx {
        tail: TailCtx {
            loop_head: None,
            param_mark: env.mark(),
            call_slots: &[],
            params: &[],
            tail_scrut_stale_acc,
        },
        born_flag,
        wake_flag,
        claims: p.claims,
        state: continued(&p.state, state),
        site: continued(&p.site, site),
        callee_layouts: p.callee_layouts,
        slot_tables: RefCell::new(Vec::new()),
        closed_frame: RefCell::new(None),
        callee_refs: &callee_refs,
        callee_ids: p.callee_ids,
        helper_ids: p.helper_ids,
        helper_refs: HelperRefs::new(p.helper_ids, &names),
        chunks: p.chunks,
        outline_frame: Cell::new(None),
        chunk: true,
        owned_floor: env.mark(),
        in_flight_bufs: RefCell::new(Vec::new()),
        owned_input_stack: RefCell::new(Vec::new()),
        collection_site: Cell::new(p.collection_site.get()),
        self_call_roots: RefCell::new(Vec::new()),
        consts: &consts,
        names: &names,
        kernel: p.kernel,
        pending_exit: RefCell::new(None),
        ladder: RefCell::default(),
        builtin_apply_sites: p.builtin_apply_sites,
        lambda_call_sites: p.lambda_call_sites,
        self_call: p.self_call,
        type_env: p.type_env,
    };
    let mut ccx = BodyCx { b: &mut b, env: &mut env, ctx: &ctx };
    let slots = Range { lo, hi, out, src, src_disc, len, entered };
    let r = emit_range(&mut ccx, lp.kind, slots, tables, iteration);
    let LowerCtx { state, site, self_call_roots, pending_exit, .. } = ctx;
    give_back(&p.state, state);
    give_back(&p.site, site);
    debug_assert!(self_call_roots.into_inner().is_empty(), "a self-call root in a loop");
    let pending = r?;
    if let Some(exit) = pending_exit.into_inner() {
        b.switch_to_block(exit);
        b.ins().return_(&[]);
    }
    b.seal_all_blocks();
    b.finalize();
    p.chunks.borrow_mut().push(ChunkFn { id, func, consts: consts.into_inner() });
    Ok((id, pending))
}

/// The chunk's own values: its range of slots, its out record and the
/// loop's source and slot count, loaded from the frame.
struct Range {
    lo: ClifValue,
    hi: ClifValue,
    out: ClifValue,
    src: ClifValue,
    src_disc: ClifValue,
    len: ClifValue,
    entered: ClifValue,
}

/// The chunk's loop over its range and its out record; the frame's
/// chain records once the loop has closed.
fn emit_range(
    cx: &mut BodyCx,
    kind: SinkKind,
    r: Range,
    tables: Vec<SlotTable>,
    iteration: impl Iteration,
) -> Result<Vec<TruncRec>> {
    let i_var = cx.b.declare_var(types::I64);
    cx.b.def_var(i_var, r.lo);
    let accs = Accs::new(cx);
    let sink = match kind {
        // the invoker took the range's buf, on its own thread
        SinkKind::Buf => {
            let buf = cx.b.ins().load(types::I64, MemFlags::trusted(), r.out, word(2));
            open_buf(cx, BufKind::Value, buf);
            Sink::Buf(buf)
        }
        SinkKind::Find => {
            let cap = cx.b.ins().isub(r.hi, r.lo);
            open_sink(cx, kind, cap)?
        }
    };
    cx.ctx.slot_tables.borrow_mut().push(SlotTableFrame {
        depth: 1,
        idx_var: i_var,
        len: r.len,
        src_disc: r.src_disc,
        tables,
        pending: Vec::new(),
    });
    let frame = LoopFrame::blocks(cx, i_var, r.hi, r.entered);
    iteration(cx, &frame, &Slots { src: r.src, src_disc: r.src_disc, sink, accs })?;
    frame.end(cx);
    let closed =
        cx.ctx.closed_frame.borrow_mut().take().expect("the loop closed its frame");
    let store = |cx: &mut BodyCx, v, i| {
        cx.b.ins().store(MemFlags::trusted(), v, r.out, word(i));
    };
    let taint = cx.b.use_var(accs.taint);
    store(cx, taint, 0);
    let stale = cx.b.use_var(accs.stale);
    store(cx, stale, 1);
    match sink {
        Sink::Buf(buf) => {
            close_buf(cx);
            store(cx, buf, 2);
        }
        Sink::Find { found, disc, payload, mark } => {
            cx.env.truncate(mark);
            let found = cx.b.use_var(found);
            let found = cx.b.ins().uextend(types::I64, found);
            store(cx, found, 2);
            let disc = cx.b.use_var(disc);
            store(cx, disc, 3);
            let payload = cx.b.use_var(payload);
            store(cx, payload, 4);
        }
    }
    cx.b.ins().return_(&[]);
    Ok(closed.pending)
}
