//! A loop at a kernel body's top level, outlined into a chunk: a
//! function of its own that runs a range of the loop's slots, which
//! `graphix_par_loop` runs in order or forked (`fusion::par_loop`,
//! `design/parallel_eval.md` §10).

use super::{
    abi::{JitEnv, STALE, ValueVar, local_payload_ty},
    body::{BodyCx, emit_kernel_bottom},
    call::close_buf,
    jit::{ChunkFn, chunk_signature},
    lower::{
        Channel, ClosedFrame, HelperRefs, LowerCtx, SlotTable, SlotTableFrame,
        StateChannel, TailCtx, TruncRec,
    },
    record::KernelConst,
    scaffold::{Accs, Iteration, LoopFrame, Sink, SinkKind, Slots, Sunk, open_sink},
};
use crate::{
    cost::LoopSite,
    expr::ExprId,
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
    pub(super) sel_sites: &'s [ExprId],
}

/// The frame a chunk reads: the context word, the state and site
/// pointers, the source, its disc, the slot count and the slots entered
/// before, then the loop's slot-table bases, then a `(disc, payload)`
/// pair per local in scope.
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
    // CR claude for claude: [structure] open_slot_tables is called here only for its
    // claims. It pushes a frame whose index variable is this never-defined `unused`,
    // the next line pops that frame, and emit_range pushes the real frame by hand
    // (outline.rs:330-337). Nothing reads `unused` today; if something did, cranelift
    // would silently give it 0, so every slot would read as slot 0. Split the claiming
    // (sites, len, src_disc -> tables, pending) out of open_slot_tables, call only that
    // here, and push each frame where its index variable exists (open_loop,
    // emit_range). (f-call-flow-11)
    let unused = cx.b.declare_var(types::I64);
    cx.open_slot_tables(lp.sel_sites, lp.len, lp.src_disc, unused)?;
    let frame = cx.ctx.slot_tables.borrow_mut().pop().expect("opened above");
    let site = cx.const_ptr(KernelConst::LoopSite(Arc::new(LoopSite::default())))?;
    let (id, pending) = emit_chunk(cx, &lp, &frame.tables, frame.pending, iteration)?;
    // The chain levels at the loop's own depth are sized before any
    // chunk runs, so a chunk's ensure of one only reads it.
    *cx.ctx.closed_frame.borrow_mut() =
        Some(ClosedFrame { depth: 1, len: lp.len, src_disc: lp.src_disc, pending });
    cx.emit_slot_truncates()?;
    let locals: SmallVec<[ValueVar; 16]> =
        cx.env.locals.iter().map(|l| l.words).collect();
    let fbase = stack_record(cx, HEADER + frame.tables.len() + 2 * locals.len());
    let wake = cx.b.ins().ishl_imm(cx.ctx.wake_flag, 1);
    let ctx_word = cx.b.ins().bor(cx.ctx.init_flag, wake);
    let header = [
        ctx_word,
        cx.state_ptr(),
        cx.site_ptr(),
        lp.src,
        lp.src_disc,
        lp.len,
        lp.entered,
    ];
    let tables = frame.tables.iter().map(|t| t.base);
    for (i, v) in header.into_iter().chain(tables).enumerate() {
        cx.b.ins().store(MemFlags::trusted(), v, fbase, word(i));
    }
    let base = HEADER + frame.tables.len();
    for (i, vv) in locals.iter().enumerate() {
        let disc = cx.b.use_var(vv.disc);
        let payload = cx.b.use_var(vv.payload);
        cx.b.ins().store(MemFlags::trusted(), disc, fbase, word(base + 2 * i));
        cx.b.ins().store(MemFlags::trusted(), payload, fbase, word(base + 2 * i + 1));
    }
    let obase = stack_record(cx, OUT_WORDS);
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
    let call =
        cx.call_helper("graphix_par_loop", &[chunk, fbase, lp.len, site, kind, obase])?;
    let aborted = cx.b.inst_results(call)[0];
    let abort_bl = cx.b.create_block();
    let cont_bl = cx.b.create_block();
    cx.b.ins().brif(aborted, abort_bl, &[], cont_bl, &[]);
    cx.b.switch_to_block(abort_bl);
    cx.b.seal_block(abort_bl);
    emit_kernel_bottom(cx)?;
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

/// The address of a fresh `words`-word record on the stack.
fn stack_record(cx: &mut BodyCx, words: usize) -> ClifValue {
    let data = StackSlotData::new(StackSlotKind::ExplicitSlot, (8 * words) as u32, 3);
    let slot = cx.b.create_sized_stack_slot(data);
    cx.b.ins().stack_addr(types::I64, slot, 0)
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
/// it to the body's chunks. Returns its id and the frame's chain
/// records, `pending` and its nested sites', for the body to size.
fn emit_chunk(
    cx: &mut BodyCx,
    lp: &Loop,
    tables: &[SlotTable],
    pending: Vec<TruncRec>,
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
        })
        .collect();
    // The body's locals, borrowed: the body drops them.
    let mut env = JitEnv::new();
    let base = HEADER + tables.len();
    for (i, l) in cx.env.locals.iter().enumerate() {
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
    // CR claude for claude: [structure] The context word's layout (bit 0 init, bit 1
    // wake) is written out by hand at every site. It is decoded here and at
    // lower.rs:72-79, encoded at outline.rs:91-92 and kernel.rs:349, and built at
    // call.rs:338-347 from the init view alone, which is how callees lost the wake bit
    // (probe: design/review-2026-10-05/repro/f-kernel-01.gx). Two bit constants beside
    // CTX_WIRE_SLOTS (which kernel.rs:349 would also use) and a `CtxWord { init, wake
    // }` in lower.rs with `decode(b, word)` and `encode(b)`, used at the four emit
    // sites, would make the layout one definition. (f-call-flow-07)
    let init_flag = b.ins().band_imm(ctx_word, 1);
    let wake_flag = {
        let w = b.ins().band_imm(ctx_word, 2);
        b.ins().ushr_imm(w, 1)
    };
    let tail_scrut_stale_acc = b.declare_var(types::I64);
    let stale = b.ins().iconst(types::I64, STALE);
    b.def_var(tail_scrut_stale_acc, stale);
    let callee_refs: BTreeMap<KernelKey, FuncRef> = p
        .callee_ids
        .iter()
        .map(|(k, fid)| (*k, names.borrow().import_func(*fid, b.func)))
        .collect();
    let self_thunk = p.self_thunk_id.map(|t| names.borrow().import_func(t, b.func));
    let consts = RefCell::new(Vec::new());
    let ctx = LowerCtx {
        tail: TailCtx {
            loop_head: None,
            param_mark: env.mark(),
            call_slots: &[],
            tail_scrut_stale_acc,
        },
        init_flag,
        wake_flag,
        claims: p.claims,
        state: continued(&p.state, state),
        site: continued(&p.site, site),
        callee_layouts: p.callee_layouts,
        slot_tables: RefCell::new(Vec::new()),
        closed_frame: RefCell::new(None),
        callee_refs: &callee_refs,
        callee_ids: p.callee_ids,
        self_thunk,
        self_thunk_id: p.self_thunk_id,
        helper_ids: p.helper_ids,
        helper_refs: HelperRefs::new(p.helper_ids, &names),
        chunks: p.chunks,
        owned_floor: env.mark(),
        in_flight_bufs: RefCell::new(Vec::new()),
        owned_input_stack: RefCell::new(Vec::new()),
        collection_site: Cell::new(p.collection_site.get()),
        self_call_roots: RefCell::new(Vec::new()),
        consts: &consts,
        names: &names,
        kernel: p.kernel,
        pending_exit: RefCell::new(None),
        builtin_apply_sites: p.builtin_apply_sites,
        lambda_call_sites: p.lambda_call_sites,
        self_call: p.self_call,
        type_env: p.type_env,
    };
    let mut ccx = BodyCx { b: &mut b, env: &mut env, ctx: &ctx };
    let slots = Range { lo, hi, out, src, src_disc, len, entered };
    let r = emit_range(&mut ccx, lp.kind, slots, tables, pending, iteration);
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
    pending: Vec<TruncRec>,
    iteration: impl Iteration,
) -> Result<Vec<TruncRec>> {
    let i_var = cx.b.declare_var(types::I64);
    cx.b.def_var(i_var, r.lo);
    let accs = Accs::new(cx);
    let cap = cx.b.ins().isub(r.hi, r.lo);
    let sink = open_sink(cx, kind, cap)?;
    cx.ctx.slot_tables.borrow_mut().push(SlotTableFrame {
        depth: 1,
        idx_var: i_var,
        len: r.len,
        src_disc: r.src_disc,
        tables,
        pending,
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
