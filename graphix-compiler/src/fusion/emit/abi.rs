//! The value-shape ABI inside a kernel: [`CompiledExpr`], the
//! TAINT/STALE disc-tag algebra, and [`JitEnv`] (name → local
//! binding, ownership kinds, scope truncation).

use crate::{BindId, Node, Rt, UserEvent, fusion::kernel_abi::PrimType};
use anyhow::Result;
use arcstr::ArcStr;
use cranelift_codegen::ir::{
    BlockArg, InstBuilder, MemFlags, Type as ClifType, Value as ClifValue,
    condcodes::IntCC, types,
};
use cranelift_frontend::{FunctionBuilder, Variable};

use super::{
    body::{BodyCx, emit_bottom_abort},
    scalar::{cast_u64_to_prim, prim_to_clif, scalar_to_payload_i64},
};

// ─── Value-shape ABI ─────────────────────────────────────────────
//
// `Value` is `#[repr(u64)]` with explicit discriminant values
// (`Value::Null = 0x0000_8000`, `Value::I64(_) = 0x0000_0400`, etc.)
// and a fixed 16-byte layout — `(u64 disc, u64 payload)`. The JIT
// represents Value-shaped expressions as a pair of CLIF `I64` SSA
// values; the SysV AMD64 ABI passes them in two integer registers
// when crossing the helper boundary. Discriminant constants below
// mirror the values in `netidx_value::Value`'s definition; the
// const block at the bottom of `emit_helpers.rs` keeps Value's
// layout pinned at 16 bytes, so these stay coherent.
pub(super) mod value_disc {
    pub const U8: i64 = 0x0000_0001;
    pub const I8: i64 = 0x0000_0002;
    pub const U16: i64 = 0x0000_0004;
    pub const I16: i64 = 0x0000_0008;
    pub const U32: i64 = 0x0000_0010;
    pub const I32: i64 = 0x0000_0040;
    pub const U64: i64 = 0x0000_0100;
    pub const I64: i64 = 0x0000_0400;
    pub const F32: i64 = 0x0000_1000;
    pub const F64: i64 = 0x0000_2000;
    pub const BOOL: i64 = 0x0000_4000;
    pub const NULL: i64 = 0x0000_8000;
    pub const STRING: i64 = 0x8000_0000;
    pub const ARRAY: i64 = 0x1000_0000;
}

/// Map a [`PrimType`] to the `Value` discriminant for boxing a
/// scalar into a `Value::T(x)`. Used by `IfChain` widening to
/// `Nullable<T>` — the arm packs `(prim_to_value_disc(T), scalar)`
/// inline rather than calling a helper.
pub(crate) fn prim_to_value_disc(p: PrimType) -> i64 {
    match p {
        PrimType::I8 => value_disc::I8,
        PrimType::I16 => value_disc::I16,
        PrimType::I32 => value_disc::I32,
        PrimType::I64 => value_disc::I64,
        PrimType::U8 => value_disc::U8,
        PrimType::U16 => value_disc::U16,
        PrimType::U32 => value_disc::U32,
        PrimType::U64 => value_disc::U64,
        PrimType::F32 => value_disc::F32,
        PrimType::F64 => value_disc::F64,
        PrimType::Bool => value_disc::BOOL,
    }
}

/// A Variant or Nullable local in the JIT: two `Variable`s holding
/// the (disc, payload) words of a `repr(u64)` Value. Reads return
/// both via `b.use_var`; writes (tail-call rebind, IfChain merge
/// via phi) update both.
#[derive(Debug, Clone, Copy)]
pub(super) struct ValueVar {
    pub(super) disc: Variable,
    pub(super) payload: Variable,
}

/// Result of emitting one expression node: a `Value`-shaped
/// `(disc, payload)` register pair, mirroring netidx's `#[repr(u64)]`
/// `Value` layout. `disc` is always an `I64` discriminant word;
/// `payload` keeps its NATURAL cranelift type within a kernel (`F64`
/// for floats, `I8` for bools, `I64` for ints / pointers / the value
/// word of a two-word Value).
///
/// `disc` doubles as the **taint channel** (#219). A real discriminant
/// only occupies bits 0..31 (a bitmask — see [`value_disc`]), so bit 62
/// ([`TAINT`]) is reserved to mark a value that MAY be a bottom (a div0,
/// a `?`-error, a missing region input the taken path never consumes).
/// Pure ops OR their operands' taint into the result disc
/// ([`propagate_taint`]); the kernel FORCES at the output (and at
/// destructuring consumers) via [`is_tainted`] — a tainted output makes
/// `Kernel::update` return `None`, moving the abort from the producing
/// site to the output so an intermediate bottom an un-taken arm never
/// consumes can't abort the whole kernel (`design/representable_bottom.md`).
///
/// For a non-tainted value the disc is a compile-time constant cranelift
/// folds out, so the uniform two-register shape costs nothing on the hot
/// path while letting every consumer treat every value identically.
#[derive(Debug, Clone, Copy)]
pub struct CompiledExpr {
    pub disc: ClifValue,
    pub payload: ClifValue,
}

/// The reserved taint bit of a [`CompiledExpr`]'s `disc`. Outside the
/// [`value_disc`] bitmask range (bits 0..31) so it never collides with a
/// real discriminant; set = "this value MAY be a bottom" (#219). Shared
/// with the runtime dispatch (`kernel.rs`), which sets it for a missing
/// input's disc word.
pub(crate) const TAINT: i64 = (crate::tval::Tag::TAINT_BIT as i64) << 56;

/// The reserved "did not fire this cycle" bit of a [`CompiledExpr`]'s
/// `disc` (bit 61, inside the JIT tag region [`emit_helpers::TagValue`]
/// reserves, below [`TAINT`]). Set = "this value carries a CACHED value
/// from a prior cycle — it did not update this cycle." It is the JIT
/// twin of the node-walk's `Cached` reporting `false` from `update`
/// (`node/mod.rs`): a node fires (publishes / writes a variable) only
/// when at least one of its inputs fired this cycle. Leaves set it
/// (a non-firing feeder, a constant after init); ops AND-reduce it
/// ([`propagate_stale`] — a result fires iff ANY operand fired); and the
/// kernel FORCES freshness at exactly two consumers — the output
/// ([`emit_kernel_return`], via [`is_not_fresh`]) and a `connect`/`?`
/// variable write (`set_var_typed`, the runtime twin). Mid-expression a stale
/// value is USED (its cached payload), never aborted — that is what
/// makes combineLatest agree with the node-walk. Invariant `TAINT ⟹
/// STALE` (a value that bottoms this cycle reads as not-fired
/// downstream) is maintained by [`propagate_flags`].
pub(crate) const STALE: i64 = 0x2000_0000_0000_0000;

impl CompiledExpr {
    pub fn new(disc: ClifValue, payload: ClifValue) -> Self {
        Self { disc, payload }
    }
}

/// An `I64` discriminant constant for a scalar of `prim` (taint clear).
pub(super) fn scalar_disc(b: &mut FunctionBuilder, prim: PrimType) -> ClifValue {
    b.ins().iconst(types::I64, prim_to_value_disc(prim))
}

/// OR the [`TAINT`] bit of each operand disc into `base`, yielding the
/// result disc. The fast path (every operand a non-tainted const disc)
/// folds back to `base`. Mirrors the interp's `?`-absorb: any consumed
/// bottom taints the result.
pub(super) fn propagate_taint(
    b: &mut FunctionBuilder,
    base: ClifValue,
    operands: &[ClifValue],
) -> ClifValue {
    let mut disc = base;
    for op in operands {
        let t = b.ins().band_imm(*op, TAINT);
        disc = b.ins().bor(disc, t);
    }
    disc
}

/// AND-reduce the [`STALE`] bit of `operands` into `base`. The dual of
/// [`propagate_taint`]: a node FIRES this cycle iff at least one of its
/// inputs fired, so its result is stale (not-fired) ONLY when EVERY
/// operand is stale. Folds to `base` for a single operand and, when all
/// operands carry compile-time-fresh discs, cranelift constant-folds the
/// whole chain away.
pub(super) fn propagate_stale(
    b: &mut FunctionBuilder,
    base: ClifValue,
    operands: &[ClifValue],
) -> ClifValue {
    let Some((first, rest)) = operands.split_first() else { return base };
    let mut all = b.ins().band_imm(*first, STALE);
    for op in rest {
        let s = b.ins().band_imm(*op, STALE);
        all = b.ins().band(all, s);
    }
    b.ins().bor(base, all)
}

/// The result-disc flag combinator for an op that consumes `operands`:
/// [`TAINT`] is OR-reduced (any consumed bottom taints the result) and
/// [`STALE`] is AND-reduced (the result fired iff any operand fired).
/// The single call every binary/unary op uses to build its result disc.
pub(super) fn propagate_flags(
    b: &mut FunctionBuilder,
    base: ClifValue,
    operands: &[ClifValue],
) -> ClifValue {
    let d = propagate_taint(b, base, operands);
    propagate_stale(b, d, operands)
}

/// Fold `base` with a conditional taint: when `cond` (an I8 0/1) is
/// true, OR [`TAINT`] into the disc. Used where a computed condition
/// signals bottom (a div0, a `?`-error).
pub(super) fn taint_if(
    b: &mut FunctionBuilder,
    base: ClifValue,
    cond: ClifValue,
) -> ClifValue {
    let tainted = b.ins().bor_imm(base, TAINT);
    b.ins().select(cond, tainted, base)
}

/// The interior-bottom cache-substitute (Eric-approved 2026-07-04):
/// [`TAINT`] must mean "no value EVER seen at this point" — but in the
/// node-walk every node caches its last value, so a taint ORIGIN (a
/// div0, a `$`-dropped error) with a PRIOR success doesn't poison its
/// consumers: they fire with the origin's cached value (node/op.rs
/// `Cached` operands; the interior-bottom soak family). Kernel INPUTS
/// already honor this (the runtime hands STALE + the last value after
/// the first fire); this wraps interior scalar origins the same way.
///
/// Wraps an already-computed scalar result: on every UNTAINTED compute
/// the value + a valid flag are stored to two claimed state words; on a
/// TAINTED disc with stored history the result becomes (cached value,
/// scalar base | [`STALE`]) — didn't-fire-this-cycle, exactly the
/// node-walk — while a genuine no-history taint passes through and
/// gates at the output as before. CALLEE bodies claim from the site
/// channel instead (jul16a 000000: the callee refusal made a callee's
/// `0 % 0` taint the whole result where the interp's operand cache
/// rides the previous value): the two words live in the caller's
/// per-call-site block, gated by the block's HONOR header —
/// substitution only ever activates under a caller that registered
/// the words for `reset_replay` zeroing (a region root call site).
/// Stateless fallback (scaffold loops, unhonored callers): the
/// unwrapped result — the pre-existing conflation, a documented
/// residual.
pub(super) fn emit_scalar_taint_cache(
    cx: &mut BodyCx,
    prim: PrimType,
    cv: CompiledExpr,
) -> CompiledExpr {
    // IN-LOOP sites: per-slot (value, ok) pairs via a reset-registered
    // slot chain ([`BodyCx::claim_slot_cache_words`]) — one word pair
    // per slot ordinal, so slot i−1's success can't bridge slot i's
    // bottom (the jul10h aliasing rule that used to force the
    // stateless degrade here). The interp twin is each slot's retained
    // node caches. Callee-body loops still degrade (no reset
    // authority over caller-hosted storage).
    if cx.ctx.loop_depth.get() > 0 {
        if let Some(addr) = cx.claim_slot_cache_words() {
            let one = cx.b.ins().iconst(types::I64, 1);
            return emit_taint_cache_at(cx, prim, cv, addr, 0, 8, one);
        }
        return cv;
    }
    // REPLAY words: `Kernel::reset_replay` zeroes them (ok = 0 = "no
    // history"), so iteration i−1's success can't bridge iteration
    // i's bottom inside an evaluation frame — the bridge stays a
    // strictly REACTIVE (cycle-to-cycle) behavior, like the node-walk
    // caches it mirrors.
    if let (Some(off_val), Some(off_ok)) =
        (cx.claim_state_word_replay(), cx.claim_state_word_replay())
    {
        let sp = cx.state_ptr();
        let one = cx.b.ins().iconst(types::I64, 1);
        return emit_taint_cache_at(cx, prim, cv, sp, off_val, off_ok, one);
    }
    let (Some((off_val, hdr)), Some((off_ok, _))) =
        (cx.claim_site_word_replay(), cx.claim_site_word_replay())
    else {
        return cv;
    };
    // The site block base is 0 on a recursive back-edge — branch
    // around the cache (the fresh-transient no-memory semantics).
    let base = cx.site_ptr();
    let has = cx.b.ins().icmp_imm(IntCC::NotEqual, base, 0);
    let mem_bl = cx.b.create_block();
    let merge = cx.b.create_block();
    cx.b.append_block_param(merge, types::I64);
    // The payload rides at the prim's native CLIF type (jul16e flood:
    // an I64 param panicked cranelift-frontend on float payloads —
    // "declared type of variable doesn't match", killing the runtime).
    cx.b.append_block_param(merge, prim_to_clif(prim));
    cx.b.ins().brif(
        has,
        mem_bl,
        &[],
        merge,
        &[BlockArg::Value(cv.disc), BlockArg::Value(cv.payload)],
    );
    cx.b.seal_block(mem_bl);
    cx.b.switch_to_block(mem_bl);
    // `ok` is set from the HONOR header instead of the constant 1: a
    // caller that didn't store 1 there (it can't register the words
    // for reset) keeps `ok` 0 forever and the cache never substitutes.
    let honor = cx.b.ins().load(types::I64, MemFlags::trusted(), base, hdr);
    let wrapped = emit_taint_cache_at(cx, prim, cv, base, off_val, off_ok, honor);
    cx.b.ins()
        .jump(merge, &[BlockArg::Value(wrapped.disc), BlockArg::Value(wrapped.payload)]);
    cx.b.seal_block(merge);
    cx.b.switch_to_block(merge);
    let disc = cx.b.block_params(merge)[0];
    let value = cx.b.block_params(merge)[1];
    CompiledExpr::new(disc, value)
}

/// The taint-cache load/store/substitute body against two words at
/// `base + off_val` / `base + off_ok`, with `ok_set` stored as the
/// valid flag on an untainted compute (1 for state words; the honor
/// header's value for site words).
fn emit_taint_cache_at(
    cx: &mut BodyCx,
    prim: PrimType,
    cv: CompiledExpr,
    base: ClifValue,
    off_val: i32,
    off_ok: i32,
    ok_set: ClifValue,
) -> CompiledExpr {
    let tainted = is_tainted(cx.b, cv.disc);
    // Loads FIRST: `have` must reflect history from BEFORE this fire.
    let cur_val = cx.b.ins().load(types::I64, MemFlags::trusted(), base, off_val);
    let cur_ok = cx.b.ins().load(types::I64, MemFlags::trusted(), base, off_ok);
    // Branchless store: keep the old contents when tainted, else the
    // fresh value + the valid flag.
    let widened = scalar_to_payload_i64(cx.b, prim, cv.payload);
    let new_val = cx.b.ins().select(tainted, cur_val, widened);
    let new_ok = cx.b.ins().select(tainted, cur_ok, ok_set);
    cx.b.ins().store(MemFlags::trusted(), new_val, base, off_val);
    cx.b.ins().store(MemFlags::trusted(), new_ok, base, off_ok);
    // Substitute on tainted-with-history.
    let have = cx.b.ins().icmp_imm(IntCC::NotEqual, cur_ok, 0);
    let use_cache = cx.b.ins().band(tainted, have);
    let cached = cast_u64_to_prim(cx.b, cur_val, prim);
    let value = cx.b.ins().select(use_cache, cached, cv.payload);
    let disc_base = scalar_disc(cx.b, prim);
    let stale = cx.b.ins().bor_imm(disc_base, STALE);
    let disc = cx.b.ins().select(use_cache, stale, cv.disc);
    CompiledExpr::new(disc, value)
}

/// True (I8 bool) iff the disc's [`TAINT`] bit is set.
pub(super) fn is_tainted(b: &mut FunctionBuilder, disc: ClifValue) -> ClifValue {
    let t = b.ins().band_imm(disc, TAINT);
    b.ins().icmp_imm(IntCC::NotEqual, t, 0)
}

/// True (I8 bool) iff the disc's [`TAINT`] bit is CLEAR — the
/// "continue / has-a-value" bit consumed by [`emit_bottom_abort`].
pub(super) fn is_untainted(b: &mut FunctionBuilder, disc: ClifValue) -> ClifValue {
    let t = b.ins().band_imm(disc, TAINT);
    b.ins().icmp_imm(IntCC::Equal, t, 0)
}

/// [`is_untainted`] widened to `I64` — the helper-argument form
/// ([`BodyCx::open_slot_tables`]'s resize gate).
pub(super) fn emit_untainted_i64(b: &mut FunctionBuilder, disc: ClifValue) -> ClifValue {
    let v = is_untainted(b, disc);
    b.ins().uextend(types::I64, v)
}

/// True (I8 bool) iff the disc is NOT fresh — [`TAINT`] (bottom) OR
/// [`STALE`] (did not fire this cycle) set, i.e. this value did NOT fire
/// with a value. The firing gate consumed at the kernel-return seam
/// (`emit_kernel_return` / `emit_force`): the node-walk publishes only
/// when its output node returned `Some`, so a not-fresh root routes to
/// the pending path → `Kernel::update` returns `None`. (The `set_var`
/// write gate is the runtime twin, `set_var_typed` in `kernel.rs`.)
/// Folds to const-false for a value the emitter proved fresh (an
/// untainted, non-stale const disc) — no branch on the hot path.
pub(super) fn is_not_fresh(b: &mut FunctionBuilder, disc: ClifValue) -> ClifValue {
    let m = b.ins().band_imm(disc, TAINT | STALE);
    b.ins().icmp_imm(IntCC::NotEqual, m, 0)
}

/// OR [`STALE`] into a constant's `disc` on every NON-init cycle: a
/// constant node fires only at init (`event.init`, wire slot 0), then
/// reports not-fired (a cached value) — the node-walk's `Constant`
/// `update` returning `Some` once then `None`. `init_flag` is the
/// kernel's [`LowerCtx::init_flag`] (1 at init).
pub(super) fn const_stale_gate(
    b: &mut FunctionBuilder,
    init_flag: ClifValue,
    disc: ClifValue,
) -> ClifValue {
    let not_init = b.ins().icmp_imm(IntCC::Equal, init_flag, 0);
    let staled = b.ins().bor_imm(disc, STALE);
    b.ins().select(not_init, staled, disc)
}

/// Strip the [`TAINT`] and [`STALE`] flag bits, yielding the real netidx
/// discriminant. Used before a disc crosses into a netidx-`Value` helper
/// (a flagged disc is an invalid tag) and before a structural disc
/// compare (we compare on the underlying tag, stale or not).
pub(super) fn clean_disc(b: &mut FunctionBuilder, disc: ClifValue) -> ClifValue {
    b.ins().band_imm(disc, !(TAINT | STALE))
}

// ─── Env: name → Variable lookup ─────────────────────────────────

/// What a [`Local`]'s `payload` word means and how it is owned/dropped
/// at scope exit. The `disc` word always carries the netidx
/// discriminant (and #219 taint); `kind` says how to read and free the
/// `payload`.
#[derive(Debug, Clone, Copy)]
pub(super) enum LocalKind {
    /// `payload` is the scalar value at its natural CLIF type; nothing
    /// to drop.
    Scalar(PrimType),
    /// `payload` is owned ValArray bits (array / tuple / struct);
    /// dropped via `graphix_valarray_drop`. Reads are BORROWED (the env
    /// keeps owning); consumers clone when they need ownership.
    Composite,
    /// `payload` is an owned `ArcStr` thin pointer; dropped via
    /// `graphix_arcstr_drop`. Reads CLONE (refcount bump) so each
    /// consumer gets an independently-owned ArcStr.
    String,
    /// `payload` is the value word of a two-word `repr(u64)` Value;
    /// dropped via `graphix_value_drop(disc, payload)`; reads BORROWED.
    /// Variant / Nullable / bare value-shape stay distinct for
    /// consumer-op well-typedness (`IsNull` vs `VariantTagEq`).
    Variant,
    Nullable,
    Value,
}

/// One in-scope kernel local. Every local is a two-register Value
/// (`vv` = disc + payload Variables) tagged by `kind`; the disc carries
/// #219 taint uniformly, so there is no separate validity slot. The
/// `payload` Variable's CLIF type follows `kind` (a scalar's natural
/// type, else `I64` for a pointer / value word).
pub(super) struct Local {
    pub(super) name: ArcStr,
    pub(super) vv: ValueVar,
    pub(super) kind: LocalKind,
    /// `Some` for params and lets carrying a source BindId, so a `Ref`
    /// resolves BindId-first (exact under shadowing — the #162/#167 bug
    /// class). `None` for synthetic locals (e.g. an HOF loop element
    /// bound only by name).
    pub(super) bind_id: Option<BindId>,
    /// Scaffold-loop nesting depth at bind time. 0 = bound outside
    /// every loop (a kernel param or a region-level let) — such a
    /// binding is LOOP-INVARIANT: identical on every iteration of
    /// every open loop. Loop elements/indices/accs bind at depth ≥ 1
    /// (the scaffolds bracket their binds inside `enter_loop`).
    pub(super) depth: u32,
}

pub(crate) struct JitEnv {
    /// All in-scope locals in binding order. Lookups walk back-to-front
    /// so an inner binding shadows an outer one. `mark`/`truncate` track
    /// the single Vec length — a block / select-arm / loop-body scope
    /// pops back to its entry length on exit.
    pub(super) locals: Vec<Local>,
    /// Current scaffold-loop nesting depth (mirrors the EmitCtx
    /// counter; bumped by `BodyCx::enter_loop`/`exit_loop`). Stamped
    /// on each `Local` at bind time — see [`Local::depth`].
    pub(super) loop_depth: u32,
}

impl JitEnv {
    pub(super) fn new() -> Self {
        Self { locals: Vec::with_capacity(8), loop_depth: 0 }
    }

    pub(super) fn bind(
        &mut self,
        name: ArcStr,
        vv: ValueVar,
        kind: LocalKind,
        bind_id: Option<BindId>,
    ) {
        let depth = self.loop_depth;
        self.locals.push(Local { name, vv, kind, bind_id, depth });
    }

    /// Resolve a local BindId-first (exact under shadowing), then by
    /// name — but ONLY to id-LESS (synthetic) locals. A same-named
    /// local carrying a DIFFERENT real id is a distinct binding: a
    /// capture whose id missed must fail here (the site Errs and the
    /// region de-fuses — a perf loss, never a wrong answer), not
    /// silently read an unrelated variable. The unrestricted fallback
    /// let a fold callback's captured `x` resolve to the fold ELEMENT
    /// (which FoldQ name-binds as "x") whenever the capture's
    /// instantiation id drifted — a nondeterministic wrong VALUE
    /// (interp 19 / jit 17, soak jul07h fuzz/divergence_000001).
    /// Walks back-to-front.
    pub(super) fn lookup(&self, id: BindId, name: &str) -> Option<&Local> {
        if let Some(l) = self.locals.iter().rev().find(|l| l.bind_id == Some(id)) {
            return Some(l);
        }
        self.locals.iter().rev().find(|l| l.bind_id.is_none() && l.name.as_str() == name)
    }

    /// Resolve a local by name only — for sites with no BindId (string /
    /// variant / nullable / value reads via `ref_local_name`).
    pub(super) fn lookup_name(&self, name: &str) -> Option<&Local> {
        self.locals.iter().rev().find(|l| l.name.as_str() == name)
    }

    /// Resolve a local by BindId only (no name fallback).
    pub(super) fn lookup_by_id(&self, id: BindId) -> Option<&Local> {
        self.locals.iter().rev().find(|l| l.bind_id == Some(id))
    }

    /// Snapshot the binding list length. Pair with [`Self::truncate`] to
    /// pop every binding introduced since the mark, so a block /
    /// select-arm / loop-body scope doesn't leak names into its
    /// enclosing scope.
    ///
    /// `truncate` does NOT emit any runtime drops. Dropping owned
    /// composite / string / value locals is the scope-exit code's job
    /// (`emit_scope_drops`) or the terminating return
    /// (`drop_owned_composites`); `truncate` is purely compile-time env
    /// hygiene.
    pub(super) fn mark(&self) -> usize {
        self.locals.len()
    }

    pub(super) fn truncate(&mut self, mark: usize) {
        self.locals.truncate(mark);
    }
}

/// The CLIF payload type for a local of `kind` — a scalar's natural
/// type, else `I64` (pointer / value word).
pub(super) fn local_payload_ty(kind: LocalKind) -> ClifType {
    match kind {
        LocalKind::Scalar(p) => prim_to_clif(p),
        _ => types::I64,
    }
}

/// Declare fresh disc + payload Variables holding `disc`/`payload`, then
/// bind them as a [`Local`] of `kind`. The single end-state bind path —
/// every local is a two-register Value carrying its own taint in the
/// disc.
pub(super) fn bind_local(
    cx: &mut BodyCx,
    name: ArcStr,
    disc: ClifValue,
    payload: ClifValue,
    kind: LocalKind,
    bind_id: Option<BindId>,
) -> ValueVar {
    let dv = cx.b.declare_var(types::I64);
    cx.b.def_var(dv, disc);
    let pv = cx.b.declare_var(local_payload_ty(kind));
    cx.b.def_var(pv, payload);
    let vv = ValueVar { disc: dv, payload: pv };
    cx.env.bind(name, vv, kind, bind_id);
    vv
}

pub(crate) fn bind_scalar_var_with_disc(
    cx: &mut BodyCx,
    name: ArcStr,
    prim: PrimType,
    payload: Variable,
    disc: Variable,
    bind_id: Option<BindId>,
) {
    cx.env.bind(name, ValueVar { disc, payload }, LocalKind::Scalar(prim), bind_id);
}

/// Emit an HOF operand and FORCE its #219 taint (a tainted operand
/// bottoms the whole HOF), returning the payload word. Used by the
/// stdlib array HOF `emit_clif` impls for operands whose scaffold has no
/// per-value taint channel — the source array, a scalar predicate /
/// body, a composite flat_map body, the `init` count. Folds to no branch
/// for an untainted operand.
pub fn emit_forced<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    node: &Node<R, E>,
) -> Result<ClifValue> {
    let cv = node.emit_clif(cx)?;
    let valid = is_untainted(cx.b, cv.disc);
    emit_bottom_abort(cx.b, cx.env, cx.ctx, valid)?;
    Ok(cv.payload)
}

/// Wrap owned ValArray bits as a composite [`CompiledExpr`]
/// (const `ARRAY` disc). The result of an HOF that produces an array.
pub fn array_result(cx: &mut BodyCx, ptr: ClifValue) -> CompiledExpr {
    let disc = cx.b.ins().iconst(types::I64, value_disc::ARRAY);
    CompiledExpr::new(disc, ptr)
}

/// Wrap a register-scalar payload as a [`CompiledExpr`] with the prim's
/// natural (taint-clear) disc. The result of an HOF that produces a
/// scalar (`array::fold` over a scalar accumulator). Unlike
/// [`array_result`], the disc's tag matches the payload's shape, so the
/// value survives a `connect`'s `set_var` (which reconstructs a `Value`
/// from the disc) — an ARRAY disc over a scalar payload would deref the
/// scalar as a `*ValArray`.
pub fn scalar_result(
    cx: &mut BodyCx,
    prim: PrimType,
    payload: ClifValue,
) -> CompiledExpr {
    CompiledExpr::new(scalar_disc(cx.b, prim), payload)
}

/// Like [`emit_forced`] but returns the whole [`CompiledExpr`] (disc +
/// payload) rather than just the payload. The abort guarantees the
/// continue path is reached only when [`TAINT`] is clear, so the returned
/// disc carries just the operand's [`STALE`] bit.
pub fn emit_forced_keep<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    node: &Node<R, E>,
) -> Result<CompiledExpr> {
    let cv = node.emit_clif(cx)?;
    let valid = is_untainted(cx.b, cv.disc);
    emit_bottom_abort(cx.b, cx.env, cx.ctx, valid)?;
    Ok(cv)
}
