//! Typed kernel intermediate representation.
//!
//! This is the shared form between the fusion analysis (which decides
//! what's fusable and produces a GIR tree) and the JIT backend that
//! lowers GIR to executable code:
//!
//! - `gir_to_clif` (in `crate::gir_jit`) — produces Cranelift IR
//!   for the JIT path. The compiled function pointer lives inside a
//!   `CraneliftNode<R, E>` wrapped around the would-be interpreter
//!   apply, with lazy compile + atomic swap.
//!
//! The JIT shares the fusion-side analysis. Anything GIR can't
//! represent is by definition not fusable; the fusion pass falls back
//! to the interpreter for that subtree.
//!
//! ## Shape
//!
//! - [`GirExpr`] is a typed expression node — every expression carries
//!   its [`PrimType`]. The `op` field is a [`GirOp`] enum spanning all
//!   supported expression forms.
//!
//! - [`GirStmt`] is a statement that appears in a function body —
//!   `let`-bindings, `return`, statement-form `select` chains, and
//!   self-tail-calls. Function bodies are `Vec<GirStmt>`.
//!
//! - [`GirKernel`] is a complete function: name, params, return type,
//!   `has_tail_loop` flag (whether the body ends with a self-recursive
//!   tail call and thus needs a surrounding `loop {}`), and body.
//!
//! Expression-form blocks (`{ let a = ..; let b = ..; tail }`) live
//! inside [`GirOp::Block`] with a flat list of [`Let`]s and a tail
//! [`GirExpr`]. Expression-form `select` lowers to [`GirOp::IfChain`]
//! — a sequence of (cond, value) pairs where the last entry's `cond`
//! may be `None` for an unconditional `else`.

use crate::typ::Type;
use arcstr::ArcStr;
use netidx_value::Value;

pub use crate::kernel_abi::*;


// ─── Operators ───────────────────────────────────────────────────

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CmpOp {
    Eq,
    Ne,
    Lt,
    Gt,
    Lte,
    Gte,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BoolOp {
    And,
    Or,
}

// ─── Inputs and known fns/consts ─────────────────────────────────


/// Signature of a function the emitter has already seen fuse
/// successfully. Used when one fused kernel calls another — the call
/// site lowers to a direct call against the target's `body_fn_name`.
#[derive(Debug, Clone)]
pub struct KnownFusedFn {
    /// Rust name of the free fn (e.g. "fused_iterate_body"). Must be
    /// visible from the emitted kernel (same crate).
    pub body_fn_name: String,
    /// Positional argument types in declaration order. Either scalar
    /// primitives or flat arrays of primitives.
    pub arg_types: Vec<Type>,
    /// Return type — scalar or array.
    pub return_type: Type,
    /// The `let` binding this kernel was built from, when known.
    /// `emit_known_fused_call` requires an unresolved call site's
    /// fnode `Ref` to carry this id before resolving by name — names
    /// shadow, ids don't. Without the check, a body call to a
    /// shadowed same-name outer lambda (`let f = …; let f = |n|
    /// f(n) * 2`) resolves against the kernel ITSELF (#206: infinite
    /// native self-call, stack overflow). `None` (region/module
    /// kernels — not name-callable from bodies anyway) keeps
    /// name-only resolution.
    pub self_bind: Option<crate::BindId>,
}

/// A compile-time-known primitive expression bound to a Graphix-level
/// name. Used to inline references to outer-scope bindings whose value
/// is itself a constant or an expression over already-known constants.
/// Stored as a [`GirExpr`] (rather than a precomputed string) so each
/// inline pays its own way through both backends; rustc constant-folds
/// the inlined Rust source, and the CLIF backend evaluates the same
/// SSA tree.
#[derive(Debug, Clone)]
pub struct KnownConst {
    pub expr: GirExpr,
}

impl KnownConst {
    pub fn typ(&self) -> Type {
        self.expr.typ.clone()
    }
}

// ─── GIR core ────────────────────────────────────────────────────

/// A typed expression node. `typ` is the Graphix type the expression
/// evaluates to (scalar primitive or flat array of primitives); `op`
/// is the actual operation.
#[derive(Debug, Clone)]
pub struct GirExpr {
    pub op: GirOp,
    pub typ: Type,
}

#[derive(Debug, Clone)]
pub enum GirOp {
    /// A compile-time constant carrying a netidx [`Value`]. The
    /// interpreter clones the `Value`. The JIT dispatches on the
    /// value's shape: a `Value::is_copy()` scalar (`i64`/`f64`/`bool`/
    /// …) lowers inline as an `iconst`/`f64const` (extracting the
    /// scalar for the `PrimType` from the expression's `typ`), while a
    /// non-copy value-shape constant (datetime/duration/bytes/map) is
    /// interned in a per-kernel value-constants table (like
    /// `ConstStr`'s string table) and emitted as the `(disc, payload)`
    /// words, cloning the `Arc` on use. Result type matches the
    /// value's shape via the expression's `typ` ([`GirType::Prim`] for
    /// scalars, [`GirType::DateTime`]/`Duration`/`Bytes`/`Map` for
    /// value-shape).
    Const(Value),
    /// Arithmetic on Value-shape operands — `datetime ± duration`,
    /// `duration ± duration`, `duration {*,/} <number>`,
    /// `<number> * duration`. Either operand may be scalar (promoted
    /// to its `Value` form) or already Value-shape; the result is
    /// always Value-shape ([`GirType::DateTime`]/[`GirType::Duration`]).
    /// Both backends compute via netidx's `impl {Add,Sub,Mul,Div} for
    /// Value`, so the result is byte-identical to the non-fused arith
    /// node. Distinct from [`GirOp::Bin`] (scalar, register math).
    ValueArith {
        op: BinOp,
        lhs: Box<GirExpr>,
        rhs: Box<GirExpr>,
    },
    /// Equality (`==` / `!=`) on two same-typed Value-shape operands
    /// (Map/Variant/Nullable/Bytes/DateTime/Duration), where neither is
    /// a primitive (those use the register-`Cmp` path). Both backends
    /// compare via netidx's `impl PartialEq for Value`. Result `Bool`.
    ValueEq {
        /// `true` for `!=`, `false` for `==`.
        ne: bool,
        lhs: Box<GirExpr>,
        rhs: Box<GirExpr>,
    },
    /// A string-typed constant. Used by `emit_expr`'s
    /// `ExprKind::Constant(Value::String)` arm and by `Concat`'s
    /// literal segments. Result type is [`GirType::String`]. A distinct
    /// op from [`GirOp::Const`] because String SSA is a single-register
    /// thin pointer (`ArcStr`), not the two-register `Value` shape the
    /// JIT uses for [`GirOp::Const`]'s value-shape constants.
    ConstStr(ArcStr),
    /// Concatenation of string-renderable parts — the GIR form of
    /// `ExprKind::StringInterpolate` (`"x=[x]"`). Each child must
    /// be [`GirType::String`] or [`GirType::Prim`]; prim children
    /// are formatted via the netidx `Value` `Display` (the same
    /// renderer `StringInterpolate` uses at non-fusion time).
    /// Result type is [`GirType::String`].
    Concat(Vec<GirExpr>),
    /// The literal `null`. Result type is [`GirType::Null`]. Runtime
    /// representation is `Value::Null`. The JIT can't lower this op
    /// today — kernels containing it route to the interpreter via
    /// `kernel_contains_null` (same fallback shape as `String`).
    ConstNull,
    /// Test whether a nullable value is `null`. The operand must be
    /// [`GirType::Null`] or [`GirType::Nullable`]; result is
    /// `Bool`. Emitted as the condition for `select` arms whose
    /// type-predicate is `null`. JIT-gated by `kernel_contains_null`.
    IsNull(Box<GirExpr>),
    /// `?` (Qop) applied to a Nullable value. The operand must be
    /// [`GirType::Nullable<T>`]. At runtime:
    ///   * If the inner Value is `Value::Error(...)` (Result error
    ///     case), signal pending — the wrapper's pending check
    ///     short-circuits the kernel and returns `None`, mirroring
    ///     the non-fused `Qop::update`'s "return None on Error" path.
    ///     A future iteration will additionally route the error to
    ///     the nearest catch handler's BindId; today it just drops.
    ///   * Otherwise extract `T` from the Value.
    ///
    /// Result type is the inner `T` (whatever `Nullable<T>`'s inner
    /// is). The JIT codegen reads the operand's `(disc, payload)`
    /// pair, branches on the error-disc check, and extracts `T` via
    /// the same payload-to-prim/string/composite path the kernel
    /// boundary uses.
    QopUnwrap {
        inner: Box<GirExpr>,
        success_typ: Type,
    },
    /// Reference to a function arg or let-bound local. Identified by
    /// name; the Rust backend uses the name directly, the CLIF backend
    /// looks it up in its `Variable` table.
    Local(ArcStr),
    /// Binary arithmetic. Constructor enforces `lhs.typ == rhs.typ`
    /// and `lhs.typ.is_numeric()`; result type is the operand type.
    Bin {
        op: BinOp,
        lhs: Box<GirExpr>,
        rhs: Box<GirExpr>,
    },
    /// Comparison. `lhs.typ == rhs.typ`; result type is `Bool`.
    Cmp {
        op: CmpOp,
        lhs: Box<GirExpr>,
        rhs: Box<GirExpr>,
    },
    /// Boolean and/or. Both operands and the result are `Bool`.
    BoolBin {
        op: BoolOp,
        lhs: Box<GirExpr>,
        rhs: Box<GirExpr>,
    },
    /// Boolean negation. Operand and result are `Bool`.
    Not(Box<GirExpr>),
    /// Primitive cast (Rust `as`). Excludes bool↔integer (not
    /// supported yet — Rust uses match for those, not `as`).
    Cast {
        inner: Box<GirExpr>,
        target: PrimType,
    },
    /// Direct call to an already-fused function (looked up by name).
    /// All-positional arguments, types must match the function's
    /// declared signature.
    Call {
        fn_name: ArcStr,
        args: Vec<GirExpr>,
    },
    /// Late-bound call into a function value the kernel doesn't know
    /// statically. Resolved at runtime by the interpreter, which
    /// reads the `fn_index`-th slot of the kernel's fn-args table
    /// (populated by [`crate::gir_interp::GirNode`] from its incoming
    /// args), invokes the resulting `LambdaDef`'s `Apply`, and
    /// returns the resulting `Value`.
    ///
    /// The cranelift JIT supports `DynCall` end-to-end (scalar /
    /// composite args, scalar / composite / Value-shape returns,
    /// pending-path correctness). See the JIT codegen notes in
    /// `CLAUDE.md` for the dispatch ABI and pending-path discipline.
    ///
    /// Type info is carried on the op rather than re-derived at
    /// runtime so the interpreter doesn't have to look up
    /// `kernel.fn_params[fn_index]` on every call.
    DynCall {
        fn_index: u32,
        args: Vec<GirExpr>,
        /// Argument types parallel to `args`. Used by the dispatch
        /// machinery to encode each arg into a `netidx::Value` and
        /// (in the JIT path) to pick the right buf-push helper —
        /// scalars marshal as primitives, composites pass as
        /// refcount-bumped clones of the caller's owned pointer.
        arg_types: Vec<Type>,
        /// Return type. Equals the wrapping `GirExpr.typ`; carried
        /// here for symmetry with `arg_types` and so the JIT can
        /// pick the right `cast_u64_to_*` / `Box::from_raw` decode
        /// path without re-walking `GirExpr.typ`.
        return_type: Type,
    },
    /// Expression-form block: `{ let a = ..; let b = ..; tail }`.
    /// All non-tail items are let-bindings; the tail provides the
    /// block's value.
    Block {
        lets: Vec<Let>,
        tail: Box<GirExpr>,
    },
    /// Expression-form if-chain. Each entry is `(condition, value)`;
    /// the last entry's condition may be `None` for an unconditional
    /// `else`. If no entry is unconditional, lowering inserts an
    /// `unreachable!()` tail (typecheck should make this unreachable
    /// in practice).
    ///
    /// `scrut` is the select's scrutinee (the dispatch value). It is
    /// evaluated ONCE up front and bottom-checked: a bottom (`None`)
    /// scrutinee poisons the whole chain (it produces bottom), matching
    /// the node-walk where a `select` node with no scrutinee value
    /// never fires. This is distinct from a bottom *arm condition*
    /// (e.g. a guard), which just makes that arm fail to match and
    /// falls through. `scrut` is `None` only for selects with no
    /// representable scrutinee gate (none today — lowering always sets
    /// it); a `None` here means "no up-front bottom-check".
    IfChain {
        scrut: Option<Box<GirExpr>>,
        arms: Vec<(Option<GirExpr>, GirExpr)>,
    },
    /// Length of a flat array parameter, as `u64`. `name` must
    /// resolve in `GirKernel.array_params`. Lowers to `arr.len() as
    /// u64` in the Rust backend; the GIR interpreter reads
    /// `array_inputs[i].len()`.
    ArrayLen {
        name: ArcStr,
    },
    /// Indexed read of a flat array parameter. `name` resolves in
    /// `array_params`; `idx` evaluates to an integer index. Result
    /// type is the wrapping `GirExpr.typ`, which must equal the
    /// referenced array's `elem` type. Lowers to
    /// `unsafe { arr.get_unchecked::<T>(idx as usize) }` in the Rust
    /// backend; the interpreter does the same call.
    ///
    /// No bounds check at runtime — the constructor is unsafe in the
    /// effective sense that it relies on the caller (fusion's lowering
    /// pass) to only emit indices the typechecker has proven in
    /// range. Out-of-range graphix-level array reads bail to
    /// non-fused execution before we ever build a kernel that would
    /// hit this op (see the Qop / `$` handling in fusion).
    ArrayGet {
        name: ArcStr,
        idx: Box<GirExpr>,
    },
    /// `bytes[i]` — index a `bytes` value, bounds-checked + negative-from-
    /// end (shared `node::array::bytes_index`). Operand is value-shape
    /// (Bytes); result is `Nullable<u8>` (`[u8, Error<…>]`).
    BytesIndex {
        bytes: Box<GirExpr>,
        idx: Box<GirExpr>,
    },
    /// Map access `m{key}`. `map` is a `GirType::Map` value-shape
    /// operand, `key` any value-bearing operand; the result is
    /// `Nullable<V>` carrying the looked-up value or a
    /// `map key not found` error (the same `[V, Error]` shape the
    /// node-walk `MapRef` produces). Both operands are consumed by
    /// the lookup.
    MapRef {
        map: Box<GirExpr>,
        key: Box<GirExpr>,
    },
    /// Array/bytes slice `a[i..j]` / `a[i..]` / `a[..j]` / `a[..]`.
    /// `source` is a `GirType::Array`/`Bytes` value-shape operand;
    /// `start`/`end` are optional scalar (integer) bounds. The result
    /// is `Nullable<source>` — the sub-array/sub-bytes or an
    /// out-of-bounds / negative-bound error (the `[T, Error]` shape the
    /// node-walk `ArraySlice` produces).
    ArraySlice {
        source: Box<GirExpr>,
        start: Option<Box<GirExpr>>,
        end: Option<Box<GirExpr>>,
    },
    /// Reduce a flat array to a scalar via a same-cycle fold. Lowers
    /// `array::fold(arr, init, |acc, x| body)` when the callback
    /// fuses to scalar GIR. `array` resolves in `array_params`;
    /// `acc_local` and `elem_local` are introduced as locals visible
    /// only inside `body`. The result type equals `body.typ` (which
    /// the constructor has pinned to `init.typ`). Lowering emits a
    /// Rust `for i in 0..arr.len()` loop with `acc` mutated in
    /// place — should auto-vectorize when the body is pure
    /// arithmetic over a primitive accumulator.
    ///
    /// Errors inside the body propagate the same way the surrounding
    /// kernel handles them (currently: bail to non-fused if the body
    /// can't be lowered cleanly). Per-element error semantics match
    /// the runtime's existing `array::fold` behavior — see the
    /// callback-error notes in `design/whole_graph_fusion.md`.
    ArrayFold {
        array: ArcStr,
        /// Element type of `array` — `Prim` for scalar elements, or a
        /// composite (`Tuple`/…) for `Array<(k,v)>`. Copied here at
        /// construction time so the backend doesn't need to walk the
        /// body looking for an `ArrayGet` against the same name (the
        /// body usually reads the bound `elem_local`). The loop binds
        /// `elem_local` as a scalar local for `Prim`, a composite
        /// otherwise (same split as `ArrayMap`).
        elem_typ: Type,
        init: Box<GirExpr>,
        acc_local: ArcStr,
        elem_local: ArcStr,
        body: Box<GirExpr>,
    },
    /// Build a flat array of `n` elements by applying a fused body to
    /// each index. Lowers `array::init(n, |idx| body)`. The body sees
    /// `idx_local: i64` as a local; its result type becomes the
    /// array's element type. Result `GirExpr.typ` is
    /// `GirType::Array(elem_typ)`.
    ///
    /// Rust emit:
    /// ```ignore
    /// {
    ///     let __n = n as usize;
    ///     ::netidx_value::ValArray::from_iter_exact(
    ///         (0..__n).map(|__i| {
    ///             let idx: i64 = __i as i64;
    ///             ::netidx_value::Value::F64(body)
    ///         })
    ///     )
    /// }
    /// ```
    /// One pooled allocation per invocation (the result array
    /// itself); no per-element Vec.
    ArrayInit {
        n: Box<GirExpr>,
        idx_local: ArcStr,
        // The output element type is `body.typ` (any GirType — prim or
        // composite); no separate field needed.
        body: Box<GirExpr>,
    },
    /// Build a flat array by applying a fused body to each element of
    /// an input array. Lowers `array::map(arr, |x| body)`. Result
    /// `GirExpr.typ` is `GirType::Array(result_elem)` where
    /// `result_elem` is `body.typ`'s primitive variant.
    ///
    /// Same one-allocation cost profile as `ArrayInit`; element loads
    /// go through the unsafe `ValArray::get_unchecked` fast path.
    ArrayMap {
        array: ArcStr,
        /// Element type of the *input* `array` — `Prim` for scalar
        /// elements, or a composite (`Tuple`/`Struct`/…) for
        /// `Array<(k, v)>`-style elements. The loop binds `elem_local`
        /// as a scalar local for `Prim` and a composite local
        /// otherwise.
        in_elem: Type,
        elem_local: ArcStr,
        // Output element type is `body.typ` (any GirType — prim or
        // composite); no separate field needed.
        body: Box<GirExpr>,
    },
    /// Build a flat array by retaining each element of an input array
    /// for which a fused predicate body returns true. Lowers
    /// `array::filter(arr, |x| pred)`. Result `GirExpr.typ` is
    /// `GirType::Array(elem)` — same element type as the input
    /// (filter never changes the element shape).
    ///
    /// Output length is dynamic, so the lowering uses
    /// `iter.collect::<ValArray>()` which goes through the
    /// `FromIterator` impl that internally takes an `LPooled<Vec>`
    /// as scratch and feeds `from_iter_exact`. After warmup the
    /// pool returns the same buffer each call — no per-invocation
    /// allocation in the hot path.
    ArrayFilter {
        array: ArcStr,
        /// Element type — `Prim` for scalar elements, composite for
        /// `Array<(k,v)>`. The loop binds `elem_local` scalar/composite
        /// (same split as `ArrayMap`); on a `keep`, the *original*
        /// element is pushed to the output.
        elem: Type,
        elem_local: ArcStr,
        /// Predicate body — must emit as `GirType::Prim(Bool)`.
        predicate: Box<GirExpr>,
    },
    /// Return the first element of `array` for which a fused predicate
    /// body returns true, as an option (`null` when none match). Lowers
    /// `array::find(arr, |x| pred)`. Result `GirExpr.typ` is
    /// `GirType::Nullable(elem)` — the matched element or `null`.
    ArrayFind {
        array: ArcStr,
        /// `Prim` for scalar elements, composite for `Array<(k,v)>`;
        /// bound the same way as `ArrayMap`. The result is
        /// `Nullable<elem>` — the matched element (scalar or composite)
        /// or `null`.
        elem: Type,
        elem_local: ArcStr,
        /// Predicate body — must emit as `GirType::Prim(Bool)`.
        predicate: Box<GirExpr>,
    },
    /// Build a flat array from the non-`null` results of a fused body
    /// returning `[out_elem, null]` per element. Lowers
    /// `array::filter_map(arr, |x| body)`. Result `GirExpr.typ` is
    /// `GirType::Array(out_elem)`; like `ArrayFilter`, output length is
    /// dynamic so the lowering collects into a `ValArray`.
    ArrayFilterMap {
        array: ArcStr,
        in_elem: PrimType,
        elem_local: ArcStr,
        /// Element type of the *output* — the inner type of the body's
        /// `Nullable` result.
        out_elem: PrimType,
        /// Body — must emit as `GirType::Nullable(out_elem)`.
        body: Box<GirExpr>,
    },
    /// `array::find_map` — like `ArrayFilterMap` (body yields
    /// `Nullable<out>`) but **early-exits** on the first non-null body
    /// result and returns it (the whole op's result is that
    /// `Nullable<out>`, or `null` if no element mapped non-null).
    /// `in_elem` may be composite (`Array<(k,v)>`), bound the same way
    /// as `ArrayMap` (scalar local vs `arrays` slot).
    ArrayFindMap {
        array: ArcStr,
        in_elem: Type,
        elem_local: ArcStr,
        /// Body — must emit as `GirType::Nullable(out)`; the op's
        /// result type is the same `Nullable<out>`.
        body: Box<GirExpr>,
    },
    /// Build a flat array by concatenating the per-element `Array<out_elem>`
    /// results of a fused body. Lowers `array::flat_map(arr, |x| body)`
    /// where the body produces an array. Result `GirExpr.typ` is
    /// `GirType::Array(out_elem)`; output length is dynamic.
    ArrayFlatMap {
        array: ArcStr,
        /// Element type — `Prim` for scalar elements, composite for
        /// `Array<(k,v)>`; bound the same way as `ArrayMap`.
        in_elem: Type,
        elem_local: ArcStr,
        /// Element type of the *output* — the element type of the body's
        /// `Array` result.
        out_elem: PrimType,
        /// Body — must emit as `GirType::Array(out_elem)`.
        body: Box<GirExpr>,
    },
    /// Read tuple slot `idx` of `name` (a tuple kernel parameter).
    /// Result `GirExpr.typ` matches `elem_typ`. For `Prim` slots the
    /// runtime extracts a scalar via `ValArray::get_unchecked`; for
    /// composite slots (nested array/tuple/struct/variant) the runtime
    /// returns the slot's `Value` wrapped in an `EvalResult::ValArray`
    /// or `Variant`. Kernels containing composite-slot accesses route
    /// to the interpreter via `kernel_contains_composite_element_op`
    /// (the JIT's primitive-only extraction path can't represent the
    /// composite return).
    TupleGet {
        name: ArcStr,
        idx: usize,
        elem_typ: Type,
    },
    /// Build a tuple from per-slot expressions. Each `fields[i]`
    /// emits to a GIR expression of type `elem_types[i]`; the
    /// interpreter / JIT wrap primitives in `Value::<variant>(x)`
    /// and feed composite values (tuples, structs, variants, arrays,
    /// nullables, strings) through the appropriate boundary helper.
    /// Result type is `GirType::Tuple(elem_types)`.
    TupleNew {
        fields: Vec<GirExpr>,
        elem_types: Vec<Type>,
    },
    /// Read struct field `field` from `name` (a struct kernel
    /// parameter), at the sorted index `sorted_idx`. Result type
    /// matches `elem_typ` — composite-typed fields produce
    /// composite `EvalResult`s in the interpreter; the JIT routes
    /// kernels with composite-field access to the interpreter via
    /// `kernel_contains_composite_element_op`.
    StructGet {
        name: ArcStr,
        field: ArcStr,
        sorted_idx: usize,
        elem_typ: Type,
    },
    /// Build a struct from per-field expressions. `sorted_fields` is
    /// the canonical alphabetical order (graphix's runtime layout).
    /// Lowers identically to `TupleNew` — the runtime doesn't
    /// distinguish tuples from structs at the ValArray level. Field
    /// types in `sorted_types` may be composite (mirroring TupleNew's
    /// generalisation).
    StructNew {
        sorted_fields: Vec<(ArcStr, GirExpr)>,
        sorted_types: Vec<(ArcStr, Type)>,
    },
    /// Test whether a variant param's runtime tag matches a
    /// compile-time-known tag string. Reads `name`'s slot 0 (the
    /// interned tag `ArcStr`), compares against `expected_tag` via
    /// byte equality. Result type is `GirType::Prim(Bool)`. Used
    /// as the condition for select arms matching a variant pattern.
    VariantTagEq {
        name: ArcStr,
        expected_tag: ArcStr,
    },
    /// Read a payload slot from a variant param. `payload_idx` is
    /// the 0-based payload position (slot index = payload_idx + 1
    /// at runtime — the tag occupies slot 0). `elem_typ` is the
    /// payload's primitive type. Only safe inside a select arm
    /// gated by a matching `VariantTagEq`.
    VariantPayload {
        name: ArcStr,
        payload_idx: usize,
        elem_typ: PrimType,
    },
    /// Build a variant value: `` `Tag(p0, p1, ...) ``. Lowers to
    /// `ValArray::from_iter_exact([Value::String("Tag"), Value::T0(p0), ...])`.
    /// Result `GirExpr.typ` is `GirType::Variant([(tag, payload_types)])`
    /// — exactly one case (the one being constructed). Payload types
    /// may be composite (mirroring TupleNew/StructNew's
    /// generalisation).
    VariantNew {
        tag: ArcStr,
        payloads: Vec<GirExpr>,
        payload_types: Vec<Type>,
    },
}

/// A single let-binding. Used in both [`GirOp::Block`] (expression-form
/// blocks) and [`GirStmt::Let`] (function bodies).
#[derive(Debug, Clone)]
pub struct Let {
    pub local: ArcStr,
    pub value: GirExpr,
}

// ─── Statement-form (function bodies) ────────────────────────────

#[derive(Debug, Clone)]
pub enum GirStmt {
    /// `let mut <local> = <value>;` — introduces a new local visible
    /// to subsequent statements.
    Let(Let),
    /// Function exit with a value.
    Return(GirExpr),
    /// Self-tail-call: assign new arg values to the loop variables and
    /// continue the surrounding `loop {}`. Only legal when the kernel
    /// has `has_tail_loop = true`. The backend chooses how to spell
    /// the temp-then-assign sequence (Rust uses temps; CLIF uses SSA).
    /// Args are stored in declaration order.
    TailCall { args: Vec<GirExpr> },
    /// Statement-form select chain. Each arm is conditional or
    /// unconditional; arms after an unconditional arm are dead. If
    /// no arm is unconditional, lowering inserts a fallthrough
    /// `unreachable!()` — typecheck should forbid this in practice.
    ///
    /// `scrut` is the scrutinee, evaluated once up front and
    /// bottom-checked: a bottom scrutinee poisons the select (the
    /// kernel returns bottom), mirroring the node-walk's "Select fires
    /// iff the scrutinee has a value." See [`GirOp::IfChain`] for the
    /// scrutinee-vs-guard distinction. `None` means no up-front
    /// bottom-check (none today).
    Select { scrut: Option<GirExpr>, arms: Vec<SelectArm> },
    /// Evaluate `expr` for its side effect, discard the result.
    /// Used for `GirType::Unit` calls (`println`, `dbg`, `log`, …)
    /// and any other expression whose value the program doesn't
    /// consume. The interpreter / JIT evaluates `expr` (firing any
    /// `GirOp::DynCall`s or producer-op effects inside it) and
    /// throws away the `EvalResult`.
    Discard(GirExpr),
}

#[derive(Debug, Clone)]
pub struct SelectArm {
    /// `None` means unconditional (final `else`).
    pub cond: Option<GirExpr>,
    pub body: Vec<GirStmt>,
}

/// A complete kernel — one function's worth: the ABI contract
/// ([`KernelSig`] — shared by `Arc` with the runtime dispatch node
/// and the JIT cache) plus the GIR op-list body.
#[derive(Debug, Clone)]
pub struct GirKernel {
    pub sig: std::sync::Arc<KernelSig>,
    pub body: Vec<GirStmt>,
}

/// Transitional: the GIR op-list (and `GirKernel` with it) is being
/// deleted in favor of emitting CLIF straight from the node graph;
/// until that flip, deref'ing to the sig keeps the many read sites
/// (`kernel.params`, `kernel.abi_params()`, …) identical on both
/// paths instead of churning code that is about to be removed.
impl std::ops::Deref for GirKernel {
    type Target = KernelSig;

    fn deref(&self) -> &KernelSig {
        &self.sig
    }
}

impl GirKernel {
    /// True if this kernel does no computation: its body is a single
    /// `Return` of a bare `Local` read. Such a kernel just forwards one
    /// of its inputs (a region binding / lifted async value) unchanged,
    /// so fusing it wraps a zero-compute kernel in dispatch overhead —
    /// the runtime `Ref` feeder already produces that value. `fuse()`
    /// skips the splice for these (the original nodes stay live and
    /// carry the value). A body with any actual op — even one extra
    /// `Let` before the `Return` — is not trivial and fuses normally.
    pub fn is_identity_passthrough(&self) -> bool {
        self.body.len() == 1
            && matches!(
                &self.body[0],
                GirStmt::Return(GirExpr { op: GirOp::Local(_), .. })
            )
    }
}


// ─── Constructors that enforce GIR invariants ────────────────────
//
// These are the canonical way to build typed nodes — they enforce the
// type invariants every backend assumes (matching operand types for
// arithmetic, both operands bool for boolean ops, etc.) so a malformed
// node can't sneak in. Returning `Option<GirExpr>` mirrors the existing
// fusion-pass shape: a None constructor result aborts the whole parent
// fusion attempt.

pub fn arith(lhs: GirExpr, rhs: GirExpr, op: BinOp) -> Option<GirExpr> {
    let lp = scalar_prim(&lhs.typ)?;
    let rp = scalar_prim(&rhs.typ)?;
    if lp != rp || !lp.is_numeric() {
        return None;
    }
    Some(GirExpr {
        op: GirOp::Bin { op, lhs: Box::new(lhs), rhs: Box::new(rhs) },
        typ: prim_type(lp),
    })
}

pub fn cmp(lhs: GirExpr, rhs: GirExpr, op: CmpOp) -> Option<GirExpr> {
    if lhs.typ != rhs.typ {
        return None;
    }
    if scalar_prim(&lhs.typ).is_some() {
        return Some(GirExpr {
            op: GirOp::Cmp { op, lhs: Box::new(lhs), rhs: Box::new(rhs) },
            typ: prim_type(PrimType::Bool),
        });
    }
    // Non-primitive `==` / `!=` (String, composite Array/Tuple/Struct, or
    // value-shape Map/Variant/Nullable/Bytes/DateTime/Duration) compares
    // via `Value`'s PartialEq. Ordering operators on non-prim types
    // aren't lowered, and Unit/Null have no comparable runtime form.
    if matches!(op, CmpOp::Eq | CmpOp::Ne)
        && !matches!(abi_kind(&lhs.typ), Some(AbiKind::Unit | AbiKind::Null))
    {
        return Some(GirExpr {
            op: GirOp::ValueEq {
                ne: matches!(op, CmpOp::Ne),
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            },
            typ: prim_type(PrimType::Bool),
        });
    }
    None
}

pub fn bool_op(lhs: GirExpr, rhs: GirExpr, op: BoolOp) -> Option<GirExpr> {
    let bool_t = prim_type(PrimType::Bool);
    if lhs.typ != bool_t || rhs.typ != bool_t {
        return None;
    }
    Some(GirExpr {
        op: GirOp::BoolBin { op, lhs: Box::new(lhs), rhs: Box::new(rhs) },
        typ: bool_t,
    })
}

pub fn not(inner: GirExpr) -> Option<GirExpr> {
    let bool_t = prim_type(PrimType::Bool);
    if inner.typ != bool_t {
        return None;
    }
    Some(GirExpr { op: GirOp::Not(Box::new(inner)), typ: bool_t })
}

pub fn cast(inner: GirExpr, target: PrimType) -> Option<GirExpr> {
    // Rust's `as` doesn't accept bool↔integer in either direction;
    // those need a `match`. Until the emitter grows that, refuse.
    let inner_p = scalar_prim(&inner.typ)?;
    if target == PrimType::Bool || inner_p == PrimType::Bool {
        return None;
    }
    Some(GirExpr {
        op: GirOp::Cast { inner: Box::new(inner), target },
        typ: prim_type(target),
    })
}

/// Build a primitive-typed [`GirOp::Const`] from a scalar [`Value`].
/// The lowered [`PrimType`] is derived from the value's variant via
/// [`scalar_prim_of_value`] (collapsing `Z32`/`V64`/… to their fixed-
/// width form). Panics on a non-scalar `Value` — a GIR-malformed
/// condition; value-shape constants (datetime/duration/bytes/map) are
/// emitted as `GirExpr { op: GirOp::Const(v), typ: <value-shape> }`
/// directly by the lowering code, which knows the [`GirType`].
pub fn const_expr(value: Value) -> GirExpr {
    let prim = scalar_prim_of_value(&value).unwrap_or_else(|| {
        panic!("const_expr: non-scalar Value {value:?} — GIR malformed")
    });
    GirExpr { op: GirOp::Const(value), typ: prim_type(prim) }
}


pub fn local(name: ArcStr, prim: PrimType) -> GirExpr {
    GirExpr { op: GirOp::Local(name), typ: prim_type(prim) }
}

/// Construct a [`GirExpr`] referring to an array-typed local. The
/// element type goes on `Type::Array(elem)` so downstream ops
/// (ArrayMap, ArrayFold) can read it back without a separate sidecar.
pub fn local_array(name: ArcStr, elem: PrimType) -> GirExpr {
    GirExpr {
        op: GirOp::Local(name),
        typ: array_type(prim_type(elem)),
    }
}

/// True if the kernel contains a [`GirOp::Call`] anywhere — i.e.
/// non-tail self-recursion or cross-kernel calls. The interpreter and
/// JIT v1 do not handle these. Callers wiring GIR through the runtime
/// path use this to refuse to instantiate a `GirNode` that would panic
/// on first call. Lifts in M4-followups when the kernel registry is in
/// place.
pub fn kernel_contains_call(kernel: &GirKernel) -> bool {
    kernel.body.iter().any(stmt_has_call)
}

fn stmt_has_call(stmt: &GirStmt) -> bool {
    match stmt {
        GirStmt::Let(l) => expr_has_call(&l.value),
        GirStmt::Return(e) => expr_has_call(e),
        GirStmt::Discard(e) => expr_has_call(e),
        GirStmt::TailCall { args } => args.iter().any(expr_has_call),
        GirStmt::Select { scrut, arms } => {
            scrut.as_ref().is_some_and(expr_has_call)
                || arms.iter().any(|a| {
                    a.cond.as_ref().is_some_and(expr_has_call)
                        || a.body.iter().any(stmt_has_call)
                })
        }
    }
}

/// Extract the integer value of a `GirOp::Const(Value::<int>)` as an
/// `i128` (covers every signed/unsigned width). `None` for non-constant
/// or non-integer exprs.
pub(crate) fn const_int(e: &GirExpr) -> Option<i128> {
    match &e.op {
        GirOp::Const(v) => match v {
            Value::I8(n) => Some(*n as i128),
            Value::I16(n) => Some(*n as i128),
            Value::I32(n) => Some(*n as i128),
            Value::I64(n) | Value::Z64(n) => Some(*n as i128),
            Value::U8(n) => Some(*n as i128),
            Value::U16(n) => Some(*n as i128),
            Value::U32(n) | Value::V32(n) => Some(*n as i128),
            Value::U64(n) | Value::V64(n) => Some(*n as i128),
            _ => None,
        },
        _ => None,
    }
}

/// Whether an integer div/mod (`lhs op rhs`) could be a value-bottom
/// (div/mod-by-zero, or signed MIN/-1 overflow). Returns `false` only
/// when both the divisor is a known-nonzero constant AND the overflow
/// case is provably absent — i.e. the op provably can't bottom (so it
/// stays a non-tainted `Single`, the fast path). A non-constant divisor
/// conservatively returns `true`.
pub(crate) fn int_div_may_bottom(lhs: &GirExpr, rhs: &GirExpr) -> bool {
    match const_int(rhs) {
        // Non-constant divisor: could be zero (or -1 with a MIN
        // dividend) at runtime.
        None => true,
        Some(0) => true,
        // Divisor -1 can overflow only with a signed MIN dividend.
        // Stay conservative unless the dividend is a non-MIN constant.
        Some(-1) => {
            let signed = scalar_prim(&lhs.typ)
                .is_some_and(|p| p.is_signed());
            if !signed {
                false
            } else {
                match (const_int(lhs), scalar_prim(&lhs.typ)) {
                    (Some(l), Some(p)) => l == signed_min_i128(p),
                    _ => true,
                }
            }
        }
        // Any other nonzero constant divisor can't bottom.
        Some(_) => false,
    }
}

fn signed_min_i128(p: PrimType) -> i128 {
    match p {
        PrimType::I8 => i8::MIN as i128,
        PrimType::I16 => i16::MIN as i128,
        PrimType::I32 => i32::MIN as i128,
        _ => i64::MIN as i128,
    }
}

/// Whether a SCALAR `GirExpr` could compile to a value-bottom
/// (`CompiledExpr::Scalar2` in the JIT) — i.e. produce or propagate an
/// integer div/mod-by-zero, signed MIN/-1, or scalar `?`-on-error.
/// Used by the JIT's `compile_ifchain` to decide whether the scalar
/// merge block needs a validity-bit phi. MUST over-approximate (never
/// under-report) — a missed taint would silently drop a bottom.
///
/// Returns `true` for any op that produces taint (int div/mod, scalar
/// QopUnwrap) or propagates it from a sub-expression (the pure scalar
/// consumers Bin/Cmp/BoolBin/Not/Cast/IfChain), AND for a bare scalar
/// `Local` (which may read a tainted let-bound local — invisible from
/// the expression alone, so conservatively assumed taintable). Non-
/// taintable: composite/value-shape producers, DynCalls/Calls,
/// accessors, constants.
pub(crate) fn expr_may_value_bottom(e: &GirExpr) -> bool {
    match &e.op {
        // Producers.
        GirOp::Bin { op, lhs, rhs } => {
            (matches!(op, BinOp::Div | BinOp::Mod)
                && scalar_prim(&lhs.typ).is_some_and(|p| p.is_integer())
                && int_div_may_bottom(lhs, rhs))
                || expr_may_value_bottom(lhs)
                || expr_may_value_bottom(rhs)
        }
        GirOp::QopUnwrap { .. } => true,
        // Propagating scalar consumers.
        GirOp::Cmp { lhs, rhs, .. } | GirOp::BoolBin { lhs, rhs, .. } => {
            expr_may_value_bottom(lhs) || expr_may_value_bottom(rhs)
        }
        GirOp::Not(inner) | GirOp::Cast { inner, .. } => {
            expr_may_value_bottom(inner)
        }
        GirOp::IfChain { scrut, arms } => {
            scrut.as_ref().is_some_and(|s| expr_may_value_bottom(s))
                || arms
                    .iter()
                    .any(|(_, body)| expr_may_value_bottom(body))
        }
        // A scalar Local may read a tainted let-bound local.
        GirOp::Local(_) => {
            matches!(abi_kind(&e.typ), Some(AbiKind::Scalar(_)))
        }
        // Non-taintable in scalar position.
        _ => false,
    }
}

pub(crate) fn expr_has_call(e: &GirExpr) -> bool {
    match &e.op {
        GirOp::Call { .. } | GirOp::DynCall { .. } => true,
        GirOp::Const(_)
        | GirOp::ConstStr(_)
        | GirOp::ConstNull
        | GirOp::Local(_)
        | GirOp::ArrayLen { .. } => false,
        GirOp::Concat(parts) => parts.iter().any(expr_has_call),
        GirOp::IsNull(inner) => expr_has_call(inner),
        GirOp::QopUnwrap { inner, .. } => expr_has_call(inner),
        GirOp::ValueArith { lhs, rhs, .. }
        | GirOp::ValueEq { lhs, rhs, .. } => {
            expr_has_call(lhs) || expr_has_call(rhs)
        }
        GirOp::Bin { lhs, rhs, .. }
        | GirOp::Cmp { lhs, rhs, .. }
        | GirOp::BoolBin { lhs, rhs, .. } => {
            expr_has_call(lhs) || expr_has_call(rhs)
        }
        GirOp::Not(inner) | GirOp::Cast { inner, .. } => expr_has_call(inner),
        GirOp::ArrayGet { idx, .. } => expr_has_call(idx),
        GirOp::BytesIndex { bytes, idx } => {
            expr_has_call(bytes) || expr_has_call(idx)
        }
        GirOp::MapRef { map, key } => {
            expr_has_call(map) || expr_has_call(key)
        }
        GirOp::ArraySlice { source, start, end } => {
            expr_has_call(source)
                || start.as_ref().is_some_and(|e| expr_has_call(e))
                || end.as_ref().is_some_and(|e| expr_has_call(e))
        }
        GirOp::ArrayFold { init, body, .. } => {
            expr_has_call(init) || expr_has_call(body)
        }
        GirOp::ArrayInit { n, body, .. } => expr_has_call(n) || expr_has_call(body),
        GirOp::ArrayMap { body, .. } => expr_has_call(body),
        GirOp::ArrayFilterMap { body, .. } => expr_has_call(body),
        GirOp::ArrayFindMap { body, .. } => expr_has_call(body),
        GirOp::ArrayFlatMap { body, .. } => expr_has_call(body),
        GirOp::ArrayFilter { predicate, .. } => expr_has_call(predicate),
        GirOp::ArrayFind { predicate, .. } => expr_has_call(predicate),
        GirOp::TupleGet { .. } | GirOp::StructGet { .. } => false,
        GirOp::TupleNew { fields, .. } => fields.iter().any(expr_has_call),
        GirOp::StructNew { sorted_fields, .. } => {
            sorted_fields.iter().any(|(_, e)| expr_has_call(e))
        }
        GirOp::VariantTagEq { .. } | GirOp::VariantPayload { .. } => false,
        GirOp::VariantNew { payloads, .. } => payloads.iter().any(expr_has_call),
        GirOp::Block { lets, tail } => {
            lets.iter().any(|l| expr_has_call(&l.value)) || expr_has_call(tail)
        }
        GirOp::IfChain { scrut, arms } => {
            scrut.as_ref().is_some_and(|s| expr_has_call(s))
                || arms.iter().any(|(c, v)| {
                    c.as_ref().is_some_and(expr_has_call) || expr_has_call(v)
                })
        }
    }
}

/// True if the kernel contains a `TupleGet` / `StructGet` (and
/// eventually array / variant accessors) whose `elem_typ` is a
/// composite (non-`Prim`) `GirType`. The JIT's primitive-only
/// scalar extraction can't handle these — callers gate on this
/// helper to route the whole kernel to the interpreter (where
/// `extract_composite_or_scalar` handles both cases).
pub fn kernel_contains_composite_element_op(kernel: &GirKernel) -> bool {
    kernel.body.iter().any(stmt_has_composite_element_op)
}

/// True if the kernel references [`GirType::Null`] or
/// [`GirType::Nullable`] anywhere — via [`GirOp::ConstNull`],
/// [`GirOp::IsNull`], a Null/Nullable arg/return on a `DynCall`,
/// or any `GirExpr.typ` of that shape. The JIT can't lower these
/// today, so such kernels route through the interpreter — same
/// fallback shape as `kernel_contains_string`.
pub fn kernel_contains_null(kernel: &GirKernel) -> bool {
    if is_null_or_nullable(&kernel.return_type) {
        return true;
    }
    kernel.body.iter().any(stmt_has_null)
}

/// True if `t`'s top-level shape is the `null` type or a
/// `Nullable`/option/result shape.
fn is_null_or_nullable(t: &Type) -> bool {
    matches!(abi_kind(t), Some(AbiKind::Null | AbiKind::Nullable))
}

fn stmt_has_null(s: &GirStmt) -> bool {
    match s {
        GirStmt::Let(l) => expr_has_null(&l.value),
        GirStmt::Return(e) => expr_has_null(e),
        GirStmt::Discard(e) => expr_has_null(e),
        GirStmt::TailCall { args } => args.iter().any(expr_has_null),
        GirStmt::Select { scrut, arms } => {
            scrut.as_ref().is_some_and(expr_has_null)
                || arms.iter().any(|a| {
                    a.cond.as_ref().is_some_and(expr_has_null)
                        || a.body.iter().any(stmt_has_null)
                })
        }
    }
}

fn expr_has_null(e: &GirExpr) -> bool {
    if is_null_or_nullable(&e.typ) {
        return true;
    }
    match &e.op {
        GirOp::ConstNull | GirOp::IsNull(_) => true,
        // QopUnwrap operates on a Nullable inner — counts as touching null.
        GirOp::QopUnwrap { .. } => true,
        GirOp::Const(_) | GirOp::ConstStr(_)
        | GirOp::Local(_)
        | GirOp::ArrayLen { .. } | GirOp::TupleGet { .. }
        | GirOp::StructGet { .. } | GirOp::VariantTagEq { .. }
        | GirOp::VariantPayload { .. } => false,
        GirOp::Concat(parts) => parts.iter().any(expr_has_null),
        GirOp::Bin { lhs, rhs, .. }
        | GirOp::Cmp { lhs, rhs, .. }
        | GirOp::BoolBin { lhs, rhs, .. }
        | GirOp::ValueArith { lhs, rhs, .. }
        | GirOp::ValueEq { lhs, rhs, .. } => {
            expr_has_null(lhs) || expr_has_null(rhs)
        }
        GirOp::Not(inner) | GirOp::Cast { inner, .. } => expr_has_null(inner),
        GirOp::ArrayGet { idx, .. } => expr_has_null(idx),
        GirOp::BytesIndex { bytes, idx } => {
            expr_has_null(bytes) || expr_has_null(idx)
        }
        GirOp::MapRef { map, key } => {
            expr_has_null(map) || expr_has_null(key)
        }
        GirOp::ArraySlice { source, start, end } => {
            expr_has_null(source)
                || start.as_ref().is_some_and(|e| expr_has_null(e))
                || end.as_ref().is_some_and(|e| expr_has_null(e))
        }
        GirOp::ArrayFold { init, body, .. } => {
            expr_has_null(init) || expr_has_null(body)
        }
        GirOp::ArrayInit { n, body, .. } => expr_has_null(n) || expr_has_null(body),
        GirOp::ArrayMap { body, .. } => expr_has_null(body),
        GirOp::ArrayFilterMap { body, .. } => expr_has_null(body),
        GirOp::ArrayFindMap { body, .. } => expr_has_null(body),
        GirOp::ArrayFlatMap { body, .. } => expr_has_null(body),
        GirOp::ArrayFilter { predicate, .. } => expr_has_null(predicate),
        // ArrayFind *produces* a Nullable result, so it always counts as
        // null-touching regardless of the predicate.
        GirOp::ArrayFind { .. } => true,
        GirOp::TupleNew { fields, .. } => fields.iter().any(expr_has_null),
        GirOp::StructNew { sorted_fields, .. } => {
            sorted_fields.iter().any(|(_, e)| expr_has_null(e))
        }
        GirOp::VariantNew { payloads, .. } => payloads.iter().any(expr_has_null),
        GirOp::Block { lets, tail } => {
            lets.iter().any(|l| expr_has_null(&l.value)) || expr_has_null(tail)
        }
        GirOp::IfChain { scrut, arms } => {
            scrut.as_ref().is_some_and(|s| expr_has_null(s))
                || arms.iter().any(|(c, v)| {
                    c.as_ref().is_some_and(expr_has_null) || expr_has_null(v)
                })
        }
        GirOp::Call { args, .. } => args.iter().any(expr_has_null),
        GirOp::DynCall { args, arg_types, return_type, .. } => {
            is_null_or_nullable(return_type)
                || arg_types.iter().any(is_null_or_nullable)
                || args.iter().any(expr_has_null)
        }
    }
}

fn stmt_has_composite_element_op(s: &GirStmt) -> bool {
    match s {
        GirStmt::Let(l) => expr_has_composite_element_op(&l.value),
        GirStmt::Return(e) => expr_has_composite_element_op(e),
        GirStmt::Discard(e) => expr_has_composite_element_op(e),
        GirStmt::TailCall { args } => {
            args.iter().any(expr_has_composite_element_op)
        }
        GirStmt::Select { scrut, arms } => {
            scrut.as_ref().is_some_and(expr_has_composite_element_op)
                || arms.iter().any(|a| {
                    a.cond.as_ref().is_some_and(expr_has_composite_element_op)
                        || a.body.iter().any(stmt_has_composite_element_op)
                })
        }
    }
}

fn expr_has_composite_element_op(e: &GirExpr) -> bool {
    match &e.op {
        GirOp::TupleGet { elem_typ, .. } | GirOp::StructGet { elem_typ, .. } => {
            scalar_prim(elem_typ).is_none()
        }
        // ArrayGet's element type lives on `e.typ` (no separate
        // elem_typ field). Composite-result ArrayGet routes to interp.
        GirOp::ArrayGet { idx, .. } => {
            scalar_prim(&e.typ).is_none()
                || expr_has_composite_element_op(idx)
        }
        GirOp::BytesIndex { bytes, idx } => {
            expr_has_composite_element_op(bytes)
                || expr_has_composite_element_op(idx)
        }
        GirOp::MapRef { map, key } => {
            expr_has_composite_element_op(map)
                || expr_has_composite_element_op(key)
        }
        GirOp::ArraySlice { source, start, end } => {
            expr_has_composite_element_op(source)
                || start.as_ref().is_some_and(|e| expr_has_composite_element_op(e))
                || end.as_ref().is_some_and(|e| expr_has_composite_element_op(e))
        }
        GirOp::Const(_) | GirOp::ConstStr(_)
        | GirOp::ConstNull
        | GirOp::Local(_) | GirOp::ArrayLen { .. }
        | GirOp::VariantTagEq { .. } | GirOp::VariantPayload { .. } => false,
        GirOp::Concat(parts) => parts.iter().any(expr_has_composite_element_op),
        GirOp::IsNull(inner) => expr_has_composite_element_op(inner),
        GirOp::QopUnwrap { inner, .. } => expr_has_composite_element_op(inner),
        GirOp::Bin { lhs, rhs, .. }
        | GirOp::Cmp { lhs, rhs, .. }
        | GirOp::BoolBin { lhs, rhs, .. }
        | GirOp::ValueArith { lhs, rhs, .. }
        | GirOp::ValueEq { lhs, rhs, .. } => {
            expr_has_composite_element_op(lhs)
                || expr_has_composite_element_op(rhs)
        }
        GirOp::Not(inner) | GirOp::Cast { inner, .. } => {
            expr_has_composite_element_op(inner)
        }
        GirOp::ArrayFold { init, body, .. } => {
            expr_has_composite_element_op(init)
                || expr_has_composite_element_op(body)
        }
        GirOp::ArrayInit { n, body, .. } => {
            expr_has_composite_element_op(n)
                || expr_has_composite_element_op(body)
        }
        GirOp::ArrayMap { body, .. } => expr_has_composite_element_op(body),
        GirOp::ArrayFilterMap { body, .. } => expr_has_composite_element_op(body),
        GirOp::ArrayFindMap { body, .. } => expr_has_composite_element_op(body),
        GirOp::ArrayFlatMap { body, .. } => expr_has_composite_element_op(body),
        GirOp::ArrayFilter { predicate, .. } => {
            expr_has_composite_element_op(predicate)
        }
        GirOp::ArrayFind { predicate, .. } => {
            expr_has_composite_element_op(predicate)
        }
        GirOp::TupleNew { fields, .. } => {
            fields.iter().any(expr_has_composite_element_op)
        }
        GirOp::StructNew { sorted_fields, .. } => sorted_fields
            .iter()
            .any(|(_, e)| expr_has_composite_element_op(e)),
        GirOp::VariantNew { payloads, .. } => {
            payloads.iter().any(expr_has_composite_element_op)
        }
        GirOp::Block { lets, tail } => {
            lets.iter().any(|l| expr_has_composite_element_op(&l.value))
                || expr_has_composite_element_op(tail)
        }
        GirOp::IfChain { scrut, arms } => {
            scrut.as_ref().is_some_and(|s| expr_has_composite_element_op(s))
                || arms.iter().any(|(c, v)| {
                    c.as_ref().is_some_and(expr_has_composite_element_op)
                        || expr_has_composite_element_op(v)
                })
        }
        GirOp::Call { args, .. } | GirOp::DynCall { args, .. } => {
            args.iter().any(expr_has_composite_element_op)
        }
    }
}

/// Walk a kernel body collecting the names of every `GirOp::Call` it
/// contains. Used by the JIT path (to declare callee `FuncRef`s
/// before lowering) and by the lazy-fusion path (to discover
/// transitive callees of an already-built kernel).
pub fn collect_call_sites(kernel: &GirKernel) -> std::collections::BTreeSet<ArcStr> {
    let mut out = std::collections::BTreeSet::new();
    for s in &kernel.body {
        walk_call_sites_stmt(s, &mut out);
    }
    out
}

fn walk_call_sites_stmt(s: &GirStmt, out: &mut std::collections::BTreeSet<ArcStr>) {
    match s {
        GirStmt::Let(l) => walk_call_sites_expr(&l.value, out),
        GirStmt::Return(e) => walk_call_sites_expr(e, out),
        GirStmt::Discard(e) => walk_call_sites_expr(e, out),
        GirStmt::TailCall { args } => {
            for a in args {
                walk_call_sites_expr(a, out);
            }
        }
        GirStmt::Select { scrut, arms } => {
            if let Some(s) = scrut {
                walk_call_sites_expr(s, out);
            }
            for a in arms {
                if let Some(c) = &a.cond {
                    walk_call_sites_expr(c, out);
                }
                for s in &a.body {
                    walk_call_sites_stmt(s, out);
                }
            }
        }
    }
}

fn walk_call_sites_expr(e: &GirExpr, out: &mut std::collections::BTreeSet<ArcStr>) {
    match &e.op {
        GirOp::Call { fn_name, args } => {
            out.insert(fn_name.clone());
            for a in args {
                walk_call_sites_expr(a, out);
            }
        }
        GirOp::DynCall { args, .. } => {
            // No static name to record — DynCall resolves through the
            // kernel's fn-args table at runtime, not the static-call
            // FuncRef table. Just recurse into the arg expressions.
            for a in args {
                walk_call_sites_expr(a, out);
            }
        }
        GirOp::Concat(parts) => {
            for p in parts {
                walk_call_sites_expr(p, out);
            }
        }
        GirOp::Const(_)
        | GirOp::ConstStr(_)
        | GirOp::ConstNull
        | GirOp::Local(_)
        | GirOp::ArrayLen { .. } => {}
        GirOp::Bin { lhs, rhs, .. }
        | GirOp::Cmp { lhs, rhs, .. }
        | GirOp::BoolBin { lhs, rhs, .. }
        | GirOp::ValueArith { lhs, rhs, .. }
        | GirOp::ValueEq { lhs, rhs, .. } => {
            walk_call_sites_expr(lhs, out);
            walk_call_sites_expr(rhs, out);
        }
        GirOp::Not(inner) | GirOp::Cast { inner, .. } => walk_call_sites_expr(inner, out),
        GirOp::IsNull(inner) => walk_call_sites_expr(inner, out),
        GirOp::QopUnwrap { inner, .. } => walk_call_sites_expr(inner, out),
        GirOp::ArrayGet { idx, .. } => walk_call_sites_expr(idx, out),
        GirOp::BytesIndex { bytes, idx } => {
            walk_call_sites_expr(bytes, out);
            walk_call_sites_expr(idx, out);
        }
        GirOp::MapRef { map, key } => {
            walk_call_sites_expr(map, out);
            walk_call_sites_expr(key, out);
        }
        GirOp::ArraySlice { source, start, end } => {
            walk_call_sites_expr(source, out);
            if let Some(e) = start {
                walk_call_sites_expr(e, out);
            }
            if let Some(e) = end {
                walk_call_sites_expr(e, out);
            }
        }
        GirOp::ArrayFold { init, body, .. } => {
            walk_call_sites_expr(init, out);
            walk_call_sites_expr(body, out);
        }
        GirOp::ArrayInit { n, body, .. } => {
            walk_call_sites_expr(n, out);
            walk_call_sites_expr(body, out);
        }
        GirOp::ArrayMap { body, .. } => walk_call_sites_expr(body, out),
        GirOp::ArrayFilterMap { body, .. } => walk_call_sites_expr(body, out),
        GirOp::ArrayFindMap { body, .. } => walk_call_sites_expr(body, out),
        GirOp::ArrayFlatMap { body, .. } => walk_call_sites_expr(body, out),
        GirOp::ArrayFilter { predicate, .. } => walk_call_sites_expr(predicate, out),
        GirOp::ArrayFind { predicate, .. } => walk_call_sites_expr(predicate, out),
        GirOp::TupleGet { .. } | GirOp::StructGet { .. } => {}
        GirOp::TupleNew { fields, .. } => {
            for f in fields {
                walk_call_sites_expr(f, out);
            }
        }
        GirOp::StructNew { sorted_fields, .. } => {
            for (_, f) in sorted_fields {
                walk_call_sites_expr(f, out);
            }
        }
        GirOp::VariantTagEq { .. } | GirOp::VariantPayload { .. } => {}
        GirOp::VariantNew { payloads, .. } => {
            for p in payloads {
                walk_call_sites_expr(p, out);
            }
        }
        GirOp::Block { lets, tail } => {
            for l in lets {
                walk_call_sites_expr(&l.value, out);
            }
            walk_call_sites_expr(tail, out);
        }
        GirOp::IfChain { scrut, arms } => {
            if let Some(s) = scrut {
                walk_call_sites_expr(s, out);
            }
            for (c, v) in arms {
                if let Some(c) = c {
                    walk_call_sites_expr(c, out);
                }
                walk_call_sites_expr(v, out);
            }
        }
    }
}

// (Helper `find_array_elem_type_in_body` removed — the element type
// is now baked into `GirOp::ArrayFold` directly via `elem_typ`, so
// neither backend needs to walk the body to recover it.)


#[cfg(test)]
mod tests {
    use super::*;

    fn i64c(x: i64) -> GirExpr {
        const_expr(Value::I64(x))
    }

    fn f64c(x: f64) -> GirExpr {
        const_expr(Value::F64(x))
    }

    fn loc(name: &str, prim: PrimType) -> GirExpr {
        local(ArcStr::from(name), prim)
    }

    #[test]
    fn arith_constructors_enforce_types() {
        // Same numeric type: ok.
        assert!(arith(i64c(1), i64c(2), BinOp::Add).is_some());
        assert!(arith(f64c(1.0), f64c(2.0), BinOp::Mul).is_some());
        // Mismatched types: rejected.
        assert!(arith(i64c(1), f64c(2.0), BinOp::Add).is_none());
        // Bool isn't numeric.
        let t = const_expr(Value::Bool(true));
        let f = const_expr(Value::Bool(false));
        assert!(arith(t, f, BinOp::Add).is_none());
    }

    #[test]
    fn cast_bool_rejected() {
        let x = loc("x", PrimType::Bool);
        assert!(cast(x, PrimType::I64).is_none());
        let y = loc("y", PrimType::I64);
        assert!(cast(y, PrimType::Bool).is_none());
    }

    #[test]
    fn abi_kind_to_param_kind_round_trips() {
        // Unit / Null aren't valid params; everything else maps.
        assert!(AbiKind::Unit.to_abi_param_kind().is_none());
        assert!(AbiKind::Null.to_abi_param_kind().is_none());
        assert!(matches!(
            AbiKind::Scalar(PrimType::I64).to_abi_param_kind(),
            Some(AbiParamKind::Scalar(PrimType::I64))
        ));
        for k in [
            AbiKind::Array,
            AbiKind::Tuple,
            AbiKind::Struct,
            AbiKind::String,
            AbiKind::Variant,
            AbiKind::Nullable,
            AbiKind::Value,
        ] {
            assert!(k.to_abi_param_kind().is_some(), "{k:?} should be a param");
        }
    }
}
