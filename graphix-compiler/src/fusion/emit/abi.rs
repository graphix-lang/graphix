//! The value-shape ABI inside a kernel: [`CompiledExpr`], the
//! TAINT/STALE disc-tag algebra, and [`JitEnv`] (name → local
//! binding, ownership kinds, scope truncation).

use crate::{BindId, Node, Rt, UserEvent, fusion::kernel_abi::PrimType};
use anyhow::Result;
use arcstr::ArcStr;
use cranelift_codegen::ir::{
    InstBuilder, Type as ClifType, Value as ClifValue, condcodes::IntCC, types,
};
use cranelift_frontend::{FunctionBuilder, Variable};

use super::{
    body::{BodyCx, emit_bottom_abort},
    scalar::prim_to_clif,
};

/// `Value` discriminants, mirroring `netidx_value::Value`'s
/// `#[repr(u64)]` tags. A Value-shaped expression is a
/// `(disc, payload)` pair of `I64`s; `emit_helpers.rs` pins the
/// 16-byte layout.
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

/// The `Value` discriminant for a scalar of `p`.
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

/// The two `Variable`s holding a local's `(disc, payload)` words.
#[derive(Debug, Clone, Copy)]
pub(super) struct ValueVar {
    pub(super) disc: Variable,
    pub(super) payload: Variable,
}

/// One emitted expression: a `(disc, payload)` register pair. `disc`
/// is the `I64` discriminant and carries the [`TAINT`] and [`STALE`]
/// bits; `payload` keeps its natural CLIF type. Taint is forced only
/// at the kernel output and at destructuring consumers, so a bottom an
/// untaken arm never consumes cannot abort the kernel.
#[derive(Debug, Clone, Copy)]
pub struct CompiledExpr {
    pub disc: ClifValue,
    pub payload: ClifValue,
}

/// Disc bit 62: this value may be a bottom. The runtime dispatch also
/// sets it for a missing input.
pub(crate) const TAINT: i64 = (crate::tval::Tag::TAINT_BIT as i64) << 56;

/// Disc bit 61: the value did not fire this cycle and carries a cached
/// payload. Leaves set it, ops AND-reduce it ([`propagate_stale`]), and
/// only the kernel output forces freshness. Invariant: `TAINT ⟹ STALE`.
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

/// OR each operand's [`TAINT`] into `base`: any consumed bottom taints
/// the result.
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

/// AND-reduce the operands' [`STALE`] into `base`: the result is stale
/// only when every operand is.
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

/// [`propagate_taint`] then [`propagate_stale`]: the result disc of an
/// op consuming `operands`.
pub(super) fn propagate_flags(
    b: &mut FunctionBuilder,
    base: ClifValue,
    operands: &[ClifValue],
) -> ClifValue {
    let d = propagate_taint(b, base, operands);
    propagate_stale(b, d, operands)
}

/// OR [`TAINT`] into `base` when `cond` (I8 0/1) is true.
pub(super) fn taint_if(
    b: &mut FunctionBuilder,
    base: ClifValue,
    cond: ClifValue,
) -> ClifValue {
    let tainted = b.ins().bor_imm(base, TAINT);
    b.ins().select(cond, tainted, base)
}

/// True (I8 bool) iff the disc's [`TAINT`] bit is set.
pub(super) fn is_tainted(b: &mut FunctionBuilder, disc: ClifValue) -> ClifValue {
    let t = b.ins().band_imm(disc, TAINT);
    b.ins().icmp_imm(IntCC::NotEqual, t, 0)
}

/// True (I8 bool) iff the disc's [`TAINT`] bit is clear.
pub(super) fn is_untainted(b: &mut FunctionBuilder, disc: ClifValue) -> ClifValue {
    let t = b.ins().band_imm(disc, TAINT);
    b.ins().icmp_imm(IntCC::Equal, t, 0)
}

/// [`is_untainted`] widened to `I64` for a helper argument.
pub(super) fn emit_untainted_i64(b: &mut FunctionBuilder, disc: ClifValue) -> ClifValue {
    let v = is_untainted(b, disc);
    b.ins().uextend(types::I64, v)
}

/// True (I8 bool) iff neither [`TAINT`] nor [`STALE`] is set: the value
/// fired this cycle.
pub(super) fn is_fresh(b: &mut FunctionBuilder, disc: ClifValue) -> ClifValue {
    let m = b.ins().band_imm(disc, TAINT | STALE);
    b.ins().icmp_imm(IntCC::Equal, m, 0)
}

/// OR [`STALE`] into a constant's `disc` when `init_flag` is 0: a
/// constant fires only at init.
pub(super) fn const_stale_gate(
    b: &mut FunctionBuilder,
    init_flag: ClifValue,
    disc: ClifValue,
) -> ClifValue {
    let not_init = b.ins().icmp_imm(IntCC::Equal, init_flag, 0);
    let staled = b.ins().bor_imm(disc, STALE);
    b.ins().select(not_init, staled, disc)
}

/// Strip [`TAINT`] and [`STALE`], leaving the netidx discriminant: a
/// flagged disc is an invalid tag to a `Value` helper or a tag compare.
pub(super) fn clean_disc(b: &mut FunctionBuilder, disc: ClifValue) -> ClifValue {
    b.ins().band_imm(disc, !(TAINT | STALE))
}

/// What a [`Local`]'s `payload` word holds and how it is dropped at
/// scope exit.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum LocalKind {
    /// The scalar at its natural CLIF type; nothing to drop.
    Scalar(PrimType),
    /// Owned ValArray bits, dropped via `graphix_valarray_drop`; reads
    /// borrow.
    Composite,
    /// An owned `ArcStr` pointer, dropped via `graphix_arcstr_drop`;
    /// reads clone.
    String,
    /// The value word of a two-word Value, dropped via
    /// `graphix_value_drop`; reads borrow. The three stay distinct so
    /// consumer ops stay well-typed (`IsNull` vs `VariantTagEq`).
    Variant,
    Nullable,
    Value,
}

/// One in-scope kernel local: a two-register Value tagged by `kind`.
/// The payload Variable's CLIF type follows `kind`.
pub(super) struct Local {
    pub(super) name: ArcStr,
    pub(super) words: ValueVar,
    pub(super) kind: LocalKind,
    /// `Some` for params and lets; a `Ref` resolves BindId-first, which
    /// is exact under shadowing. `None` for synthetic locals.
    pub(super) bind_id: Option<BindId>,
    /// Scaffold-loop depth at bind time; 0 means loop-invariant.
    pub(super) loop_depth: u32,
}

pub(crate) struct JitEnv {
    /// In binding order; lookups walk back to front so an inner binding
    /// shadows an outer one.
    pub(super) locals: Vec<Local>,
    /// Current scaffold-loop depth, stamped on each `Local` at bind time.
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
        let loop_depth = self.loop_depth;
        self.locals.push(Local { name, words: vv, kind, bind_id, loop_depth });
    }

    /// The kind and words of every local bound above `mark`, for dropping.
    pub(super) fn locals_above(
        &self,
        mark: usize,
    ) -> impl Iterator<Item = (LocalKind, ValueVar)> + '_ {
        self.locals[mark..].iter().map(|l| (l.kind, l.words))
    }

    /// BindId first, then by name but only to id-less synthetic locals:
    /// a same-named local with a different id is a distinct binding and
    /// must miss here (the region de-fuses) rather than be read.
    pub(super) fn lookup(&self, id: BindId, name: &str) -> Option<&Local> {
        if let Some(l) = self.locals.iter().rev().find(|l| l.bind_id == Some(id)) {
            return Some(l);
        }
        self.locals.iter().rev().find(|l| l.bind_id.is_none() && l.name.as_str() == name)
    }

    /// By name only, for sites with no BindId.
    pub(super) fn lookup_name(&self, name: &str) -> Option<&Local> {
        self.locals.iter().rev().find(|l| l.name.as_str() == name)
    }

    /// By BindId alone, for synthetic `Ref`s that name nothing.
    pub(super) fn lookup_id(&self, id: BindId) -> Option<&Local> {
        self.locals.iter().rev().find(|l| l.bind_id == Some(id))
    }

    /// Pair with [`Self::truncate`] to pop the bindings introduced since
    /// the mark. Compile-time only: runtime drops are `emit_scope_drops`'
    /// and the return path's job.
    pub(super) fn mark(&self) -> usize {
        self.locals.len()
    }

    pub(super) fn truncate(&mut self, mark: usize) {
        self.locals.truncate(mark);
    }
}

/// The CLIF payload type for a local of `kind`.
pub(super) fn local_payload_ty(kind: LocalKind) -> ClifType {
    match kind {
        LocalKind::Scalar(p) => prim_to_clif(p),
        _ => types::I64,
    }
}

/// Declare disc and payload Variables holding `disc`/`payload` and bind
/// them as a [`Local`] of `kind`.
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

/// Emit an operand and abort the kernel if it is tainted, returning the
/// payload word. For HOF operands that have no per-value taint channel.
pub fn emit_or_abort_on_taint<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    node: &Node<R, E>,
) -> Result<ClifValue> {
    let cv = node.emit_clif(cx)?;
    let valid = is_untainted(cx.b, cv.disc);
    emit_bottom_abort(cx.b, cx.env, cx.ctx, valid)?;
    Ok(cv.payload)
}

/// Wrap owned ValArray bits as a composite [`CompiledExpr`].
pub fn array_result(cx: &mut BodyCx, ptr: ClifValue) -> CompiledExpr {
    let disc = cx.b.ins().iconst(types::I64, value_disc::ARRAY);
    CompiledExpr::new(disc, ptr)
}

/// Wrap a scalar payload as a [`CompiledExpr`] with the prim's disc.
/// The disc must match the payload's shape: `set_var` rebuilds a
/// `Value` from it.
pub fn scalar_result(
    cx: &mut BodyCx,
    prim: PrimType,
    payload: ClifValue,
) -> CompiledExpr {
    CompiledExpr::new(scalar_disc(cx.b, prim), payload)
}

/// [`emit_or_abort_on_taint`] returning the whole [`CompiledExpr`]; on the continue
/// path the disc carries only the operand's [`STALE`] bit.
pub fn emit_or_abort_on_taint_keep<R: Rt, E: UserEvent>(
    cx: &mut BodyCx,
    node: &Node<R, E>,
) -> Result<CompiledExpr> {
    let cv = node.emit_clif(cx)?;
    let valid = is_untainted(cx.b, cv.disc);
    emit_bottom_abort(cx.b, cx.env, cx.ctx, valid)?;
    Ok(cv)
}
