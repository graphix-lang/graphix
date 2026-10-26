//! The value-shape ABI inside a kernel: [`CompiledExpr`], the
//! TAINT/STALE disc-tag algebra, and [`JitEnv`] (name → local
//! binding, ownership kinds, scope truncation).

use crate::{
    BindId,
    fusion::kernel_abi::{self, AbiKind, PrimType},
};
use arcstr::ArcStr;
use cranelift_codegen::ir::{
    InstBuilder, StackSlot, StackSlotData, StackSlotKind, Type as ClifType,
    Value as ClifValue, condcodes::IntCC, types,
};
use cranelift_frontend::{FunctionBuilder, Variable};
use smallvec::SmallVec;

use super::{body::BodyCx, scalar::prim_to_clif};

/// `Value` discriminants: netidx gives each `Typ` the bits of the
/// matching `Value` tag (pinned by `value_disc_matches_value_layout`).
/// A Value-shaped expression is a `(disc, payload)` pair of `I64`s.
pub(super) mod value_disc {
    use netidx_value::Typ;

    pub const U8: i64 = Typ::U8 as i64;
    pub const I8: i64 = Typ::I8 as i64;
    pub const U16: i64 = Typ::U16 as i64;
    pub const I16: i64 = Typ::I16 as i64;
    pub const U32: i64 = Typ::U32 as i64;
    pub const I32: i64 = Typ::I32 as i64;
    pub const U64: i64 = Typ::U64 as i64;
    pub const I64: i64 = Typ::I64 as i64;
    pub const F32: i64 = Typ::F32 as i64;
    pub const F64: i64 = Typ::F64 as i64;
    pub const BOOL: i64 = Typ::Bool as i64;
    pub const NULL: i64 = Typ::Null as i64;
    pub const STRING: i64 = Typ::String as i64;
    pub const ERROR: i64 = Typ::Error as i64;
    pub const ARRAY: i64 = Typ::Array as i64;
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

/// The two `Variable`s holding a local's `(disc, payload)` words. An
/// owned value's are also kept in a stack slot every definition writes,
/// which its drops read: a read of the variables at a drop would carry the
/// local as a block parameter through every join since its definition,
/// and every abort exit's ladder of drops through all of them.
#[derive(Debug, Clone, Copy)]
pub(super) struct ValueVar {
    pub(super) disc: Variable,
    pub(super) payload: Variable,
    spill: Option<StackSlot>,
}

impl ValueVar {
    /// The variables of a value of `kind` this body owns.
    pub(super) fn owned(b: &mut FunctionBuilder, kind: LocalKind) -> Self {
        let spill = (!matches!(kind, LocalKind::Scalar(_))).then(|| {
            b.create_sized_stack_slot(StackSlotData::new(
                StackSlotKind::ExplicitSlot,
                16,
                3,
            ))
        });
        Self::with_spill(b, kind, spill)
    }

    /// The variables of a value of `kind` another body owns and drops.
    pub(super) fn borrowed(b: &mut FunctionBuilder, kind: LocalKind) -> Self {
        Self::with_spill(b, kind, None)
    }

    /// Scalar words: nothing to drop.
    pub(super) fn scalar(disc: Variable, payload: Variable) -> Self {
        Self { disc, payload, spill: None }
    }

    fn with_spill(
        b: &mut FunctionBuilder,
        kind: LocalKind,
        spill: Option<StackSlot>,
    ) -> Self {
        let disc = b.declare_var(types::I64);
        let payload = b.declare_var(local_payload_ty(kind));
        Self { disc, payload, spill }
    }

    /// Define both words.
    pub(super) fn define(
        self,
        b: &mut FunctionBuilder,
        disc: ClifValue,
        payload: ClifValue,
    ) {
        self.define_disc(b, disc);
        b.def_var(self.payload, payload);
        if let Some(s) = self.spill {
            b.ins().stack_store(types::I64, payload, s, 8);
        }
    }

    /// Define the disc alone.
    pub(super) fn define_disc(self, b: &mut FunctionBuilder, disc: ClifValue) {
        b.def_var(self.disc, disc);
        if let Some(s) = self.spill {
            b.ins().stack_store(types::I64, disc, s, 0);
        }
    }

    pub(super) fn read(self, b: &mut FunctionBuilder) -> CompiledExpr {
        CompiledExpr::new(b.use_var(self.disc), b.use_var(self.payload))
    }

    /// The words as a drop reads them.
    pub(super) fn read_spilled(self, b: &mut FunctionBuilder) -> CompiledExpr {
        match self.spill {
            Some(s) => CompiledExpr::new(
                b.ins().stack_load(types::I64, types::I64, s, 0),
                b.ins().stack_load(types::I64, types::I64, s, 8),
            ),
            None => self.read(b),
        }
    }
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

/// Disc bit 62: the value is a bottom; the payload under it is a
/// placeholder (an owned empty one for a composite kind), never read.
/// The runtime dispatch also sets it for a missing input. Independent
/// of [`STALE`]: a fresh bottom is an event.
pub(crate) const TAINT: i64 = (crate::tval::Tag::TAINT_BIT as i64) << 56;

/// Disc bit 61: the value did not fire this cycle. When it is not
/// tainted the payload is the standing value. Ops AND-reduce it
/// ([`propagate_stale`]) while [`TAINT`] ORs.
pub(crate) const STALE: i64 = (crate::tval::Tag::STALE_BIT as i64) << 56;

/// Whether a value fired: [`STALE`], read as a test.
pub(crate) const FIRE_TEST: i64 = STALE;

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
        let t = b.ins().band_imm_u(*op, TAINT);
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
    let mut all = b.ins().band_imm_u(*first, STALE);
    for op in rest {
        let s = b.ins().band_imm_u(*op, STALE);
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
    let tainted = b.ins().bor_imm_u(base, TAINT);
    b.ins().select(cond, tainted, base)
}

/// True (I8 bool) iff the disc's [`TAINT`] bit is set.
pub(super) fn is_tainted(b: &mut FunctionBuilder, disc: ClifValue) -> ClifValue {
    let t = b.ins().band_imm_u(disc, TAINT);
    b.ins().icmp_imm_s(IntCC::NotEqual, t, 0)
}

/// True (I8 bool) iff the disc's [`TAINT`] bit is clear.
pub(super) fn is_untainted(b: &mut FunctionBuilder, disc: ClifValue) -> ClifValue {
    let t = b.ins().band_imm_u(disc, TAINT);
    b.ins().icmp_imm_s(IntCC::Equal, t, 0)
}

/// [`is_untainted`] widened to `I64` for a helper argument.
pub(super) fn emit_untainted_i64(b: &mut FunctionBuilder, disc: ClifValue) -> ClifValue {
    let v = is_untainted(b, disc);
    b.ins().uextend(types::I64, v)
}

/// True (I8 bool) iff neither [`TAINT`] nor [`FIRE_TEST`] is set: the
/// value fired this cycle.
pub(super) fn is_fresh(b: &mut FunctionBuilder, disc: ClifValue) -> ClifValue {
    let m = b.ins().band_imm_u(disc, TAINT | FIRE_TEST);
    b.ins().icmp_imm_s(IntCC::Equal, m, 0)
}

/// OR [`STALE`] into a constant's `disc` when `born_flag` is 0: a
/// constant fires only where it is born.
pub(super) fn const_stale_gate(
    b: &mut FunctionBuilder,
    born_flag: ClifValue,
    disc: ClifValue,
) -> ClifValue {
    let not_init = b.ins().icmp_imm_s(IntCC::Equal, born_flag, 0);
    let staled = b.ins().bor_imm_u(disc, STALE);
    b.ins().select(not_init, staled, disc)
}

/// Strip [`TAINT`] and [`STALE`], leaving the netidx discriminant: a
/// flagged disc is an invalid tag to a `Value` helper or a tag compare.
pub(super) fn clean_disc(b: &mut FunctionBuilder, disc: ClifValue) -> ClifValue {
    b.ins().band_imm_u(disc, !(TAINT | STALE))
}

/// The kind of an owned non-scalar local.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum OwnedKind {
    Composite,
    String,
    Value,
}

impl From<OwnedKind> for LocalKind {
    fn from(k: OwnedKind) -> Self {
        match k {
            OwnedKind::Composite => LocalKind::Composite,
            OwnedKind::String => LocalKind::String,
            OwnedKind::Value => LocalKind::Value,
        }
    }
}

/// The results `rs` of a read helper as the (disc, payload) of a local
/// of `kind`: a composite or string read returns its bits alone, a value
/// read its pair, a scalar read the scalar.
pub(super) fn owned_words(
    b: &mut FunctionBuilder,
    kind: LocalKind,
    rs: &[ClifValue],
) -> (ClifValue, ClifValue) {
    let (r0, r1) = (rs[0], rs.get(1).copied());
    match (kind, r1) {
        (LocalKind::Scalar(p), _) => (scalar_disc(b, p), r0),
        (LocalKind::Composite, _) => (b.ins().iconst(types::I64, value_disc::ARRAY), r0),
        (LocalKind::String, _) => (b.ins().iconst(types::I64, value_disc::STRING), r0),
        (LocalKind::Value, Some(payload)) => (r0, payload),
        (LocalKind::Value, None) => unreachable!("a value read returns a pair"),
    }
}

/// What a [`Local`]'s `payload` word holds and how it is dropped at
/// scope exit.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum LocalKind {
    /// The scalar at its natural CLIF type; nothing to drop.
    Scalar(PrimType),
    /// Owned ValArray bits, dropped via `graphix_valarray_drop`; reads
    /// borrow.
    Composite,
    /// An owned `ArcStr` pointer, dropped via `graphix_arcstr_drop`;
    /// reads clone.
    String,
    /// The value word of a two-word Value (a variant, a nullable or any
    /// other value shape), dropped via `graphix_value_drop`; reads borrow.
    Value,
}

impl LocalKind {
    /// The kind of a local holding a value of ABI shape `k`; `None` for
    /// `Unit` and bare `Null`, which bind no local.
    pub(super) fn of(k: AbiKind) -> Option<Self> {
        Some(match k {
            AbiKind::Scalar(p) => Self::Scalar(p),
            AbiKind::Array | AbiKind::Tuple | AbiKind::Struct => Self::Composite,
            AbiKind::String => Self::String,
            AbiKind::Variant | AbiKind::Nullable | AbiKind::Value => Self::Value,
            AbiKind::Unit | AbiKind::Null => return None,
        })
    }

    /// The local a kernel parameter of kind `k` binds as. Every kind is a
    /// two-word `(disc, payload)` pair on the wire; they differ in entry
    /// binding and body emission.
    pub(super) fn of_param(k: &kernel_abi::ParamKind) -> Self {
        use kernel_abi::ParamKind as P;
        match k {
            P::Scalar(p) => LocalKind::Scalar(*p),
            P::Array { .. } | P::Tuple { .. } | P::Struct { .. } => LocalKind::Composite,
            P::String => LocalKind::String,
            P::Variant { .. } | P::Nullable { .. } | P::Value { .. } => LocalKind::Value,
        }
    }

    /// The owned kind of a non-scalar local.
    pub(crate) fn owned(self) -> anyhow::Result<OwnedKind> {
        Ok(match self {
            LocalKind::Scalar(p) => {
                anyhow::bail!("emit_clif: a scalar {p:?} is no owned local")
            }
            LocalKind::Composite => OwnedKind::Composite,
            LocalKind::String => OwnedKind::String,
            LocalKind::Value => OwnedKind::Value,
        })
    }
}

/// One in-scope kernel local: a two-register Value tagged by `kind`.
/// The payload Variable's CLIF type follows `kind`.
pub(super) struct Local {
    pub(super) name: ArcStr,
    pub(super) words: ValueVar,
    pub(super) kind: LocalKind,
    /// `Some` for params, lets and every bind a pattern, element or leaf
    /// names; a `Ref` resolves BindId-first, which is exact under
    /// shadowing. `None` for a local no BindId names (an adopted select
    /// scrutinee, a find loop's result): a read finds it by `name`.
    pub(super) bind_id: Option<BindId>,
}

pub(crate) struct JitEnv {
    /// In binding order; lookups walk back to front so an inner binding
    /// shadows an outer one.
    pub(super) locals: Vec<Local>,
    /// Current scaffold-loop depth. State claims are refused inside
    /// loops: one static word cannot hold per-slot memory.
    pub(super) loop_depth: u32,
    /// In a loop body, the innermost slot's views.
    pub(super) slot: Option<SlotView>,
    /// A tuple bound as one local per field: its BindId, its fields'
    /// BindIds, and the locals' length when it was bound.
    pub(super) unpacked: SmallVec<[(BindId, SmallVec<[BindId; 4]>, usize); 1]>,
}

/// How a loop slot's body reads what stands.
#[derive(Clone, Copy)]
pub(super) struct SlotView {
    /// Whether the slot is born (`BodyCx::born_flag`): the enclosing
    /// view's, or a new slot's.
    pub(super) born: ClifValue,
    /// 1 in a new slot, else 0.
    pub(super) new: ClifValue,
    /// ANDed into the disc of a read of a local bound before the loop:
    /// clears STALE in a genuine birth (an init view, no wake), where a
    /// standing value reads as delivered.
    pub(super) standing: ClifValue,
    /// The locals below this index were bound before the loop.
    pub(super) mark: usize,
}

impl JitEnv {
    pub(super) fn new() -> Self {
        Self {
            locals: Vec::with_capacity(8),
            loop_depth: 0,
            slot: None,
            unpacked: SmallVec::new(),
        }
    }

    pub(super) fn bind(
        &mut self,
        name: ArcStr,
        vv: ValueVar,
        kind: LocalKind,
        bind_id: Option<BindId>,
    ) {
        self.locals.push(Local { name, words: vv, kind, bind_id });
    }

    /// The kind and words of every local bound above `mark`, for dropping.
    pub(super) fn locals_above(
        &self,
        mark: usize,
    ) -> impl Iterator<Item = (LocalKind, ValueVar)> + '_ {
        self.locals[mark..].iter().map(|l| (l.kind, l.words))
    }

    /// The index of a read's local: BindId first, then by `name` but only
    /// to id-less synthetic locals (a same-named local with a different id
    /// is a distinct binding and must miss here, so the region de-fuses,
    /// rather than be read); by BindId alone for a synthetic `Ref` that
    /// names nothing.
    pub(super) fn position(&self, id: BindId, name: Option<&str>) -> Option<usize> {
        let by_id = self.locals.iter().rposition(|l| l.bind_id == Some(id));
        by_id.or_else(|| {
            let name = name?;
            self.locals
                .iter()
                .rposition(|l| l.bind_id.is_none() && l.name.as_str() == name)
        })
    }

    /// By BindId alone.
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
        self.unpacked.retain(|(_, _, at)| *at <= mark);
    }

    /// Bind the tuple `id` as its fields, already bound as `fields`.
    pub(super) fn unpack(&mut self, id: BindId, fields: SmallVec<[BindId; 4]>) {
        self.unpacked.push((id, fields, self.locals.len()));
    }

    /// The local standing for field `idx` of the tuple `id`, when the
    /// tuple is bound as its fields.
    pub(super) fn unpacked_field(&self, id: BindId, idx: usize) -> Option<BindId> {
        let (_, fields, _) = self.unpacked.iter().rev().find(|(t, _, _)| *t == id)?;
        fields.get(idx).copied()
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
    let vv = ValueVar::owned(cx.b, kind);
    vv.define(cx.b, disc, payload);
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
    cx.env.bind(name, ValueVar::scalar(disc, payload), LocalKind::Scalar(prim), bind_id);
}

/// Wrap owned ValArray bits as a composite [`CompiledExpr`].
pub fn array_result(cx: &mut BodyCx, ptr: ClifValue) -> CompiledExpr {
    let disc = cx.b.ins().iconst(types::I64, value_disc::ARRAY);
    CompiledExpr::new(disc, ptr)
}

#[cfg(test)]
mod tests {
    use super::value_disc;
    use crate::tval::value_words;
    use netidx_value::{ValArray, Value};

    #[test]
    fn value_disc_matches_value_layout() {
        let cases = [
            (Value::U8(0), value_disc::U8),
            (Value::I8(0), value_disc::I8),
            (Value::U16(0), value_disc::U16),
            (Value::I16(0), value_disc::I16),
            (Value::U32(0), value_disc::U32),
            (Value::I32(0), value_disc::I32),
            (Value::U64(0), value_disc::U64),
            (Value::I64(0), value_disc::I64),
            (Value::F32(0.), value_disc::F32),
            (Value::F64(0.), value_disc::F64),
            (Value::Bool(false), value_disc::BOOL),
            (Value::Null, value_disc::NULL),
            (Value::String(arcstr::ArcStr::new()), value_disc::STRING),
            (Value::error(arcstr::ArcStr::new()), value_disc::ERROR),
            (Value::Array(ValArray::from([])), value_disc::ARRAY),
        ];
        for (v, disc) in cases {
            assert_eq!(value_words(&v)[0], disc as u64, "{v:?}");
        }
    }
}
