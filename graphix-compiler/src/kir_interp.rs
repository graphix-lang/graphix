//! Tree-walking interpreter for the kernel IR.
//!
//! This is the universal "fusable lambda" execution path: every lambda
//! whose body fuses into [`KirKernel`] runs through this interpreter
//! instead of being compiled into a tree of `Box<dyn Update>` nodes.
//! For pure-arithmetic kernels (mandelbrot, fib, anything with a hot
//! inner loop on primitives) the interpreter is strictly faster than
//! the regular node graph because it:
//!
//! - Has no `Box<dyn Update>` indirection per arithmetic op (the per-
//!   op cost is one `match` on a small enum, not a vtable dispatch).
//! - Keeps internal values as raw primitives in a small register file
//!   ([`RegValue`]); no `Value` packing/unpacking on intermediate
//!   results.
//! - Doesn't carry per-node reactive machinery (`sleep`, `refs`,
//!   per-op caching) for nodes whose only trigger is "an arg changed".
//!
//! Inputs cross the boundary as `Value` once at the [`KirNode::update`]
//! boundary, get unpacked into [`RegValue`]s, and the interpreter runs
//! over typed primitives until the kernel returns.
//!
//! When the JIT (M3+M4) is wired up, the kernel field gets a JIT slot
//! alongside; on first hit of native code, the wrapper dispatches to
//! the function pointer instead of running the interpreter. The
//! interpreter remains as the always-available fallback.

use crate::{
    kernel_ir::{
        BinOp, BoolOp, CmpOp, ConstVal, KirExpr, KirKernel, KirOp, KirStmt, PrimType,
    },
    Apply, Event, ExecCtx, Node, Rt, UserEvent,
};
use arcstr::ArcStr;
use netidx::subscriber::Value;
use std::{collections::BTreeMap, sync::Arc};

// ─── Runtime register value ──────────────────────────────────────

/// A primitive value at runtime. Mirrors [`ConstVal`] in shape but is
/// the type the interpreter actually operates on. `Copy` so we can
/// freely move values through registers without smart-pointer
/// machinery.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum RegValue {
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    F32(f32),
    F64(f64),
    Bool(bool),
}

impl RegValue {
    pub fn typ(&self) -> PrimType {
        match self {
            RegValue::I8(_) => PrimType::I8,
            RegValue::I16(_) => PrimType::I16,
            RegValue::I32(_) => PrimType::I32,
            RegValue::I64(_) => PrimType::I64,
            RegValue::U8(_) => PrimType::U8,
            RegValue::U16(_) => PrimType::U16,
            RegValue::U32(_) => PrimType::U32,
            RegValue::U64(_) => PrimType::U64,
            RegValue::F32(_) => PrimType::F32,
            RegValue::F64(_) => PrimType::F64,
            RegValue::Bool(_) => PrimType::Bool,
        }
    }

    pub fn from_const(c: ConstVal) -> RegValue {
        match c {
            ConstVal::I8(x) => RegValue::I8(x),
            ConstVal::I16(x) => RegValue::I16(x),
            ConstVal::I32(x) => RegValue::I32(x),
            ConstVal::I64(x) => RegValue::I64(x),
            ConstVal::U8(x) => RegValue::U8(x),
            ConstVal::U16(x) => RegValue::U16(x),
            ConstVal::U32(x) => RegValue::U32(x),
            ConstVal::U64(x) => RegValue::U64(x),
            ConstVal::F32(x) => RegValue::F32(x),
            ConstVal::F64(x) => RegValue::F64(x),
            ConstVal::Bool(b) => RegValue::Bool(b),
        }
    }

    /// Lift a netidx [`Value`] to a [`RegValue`], coercing to the
    /// `expected` primitive type. Returns `None` if the Value isn't a
    /// primitive of the expected variant — that means the caller built
    /// a kernel against a typechecker decision the runtime is now
    /// disagreeing with, which is a bug.
    pub fn from_value(v: &Value, expected: PrimType) -> Option<RegValue> {
        let got = match v {
            Value::I8(x) => RegValue::I8(*x),
            Value::I16(x) => RegValue::I16(*x),
            Value::I32(x) | Value::Z32(x) => RegValue::I32(*x),
            Value::I64(x) | Value::Z64(x) => RegValue::I64(*x),
            Value::U8(x) => RegValue::U8(*x),
            Value::U16(x) => RegValue::U16(*x),
            Value::U32(x) | Value::V32(x) => RegValue::U32(*x),
            Value::U64(x) | Value::V64(x) => RegValue::U64(*x),
            Value::F32(x) => RegValue::F32(*x),
            Value::F64(x) => RegValue::F64(*x),
            Value::Bool(b) => RegValue::Bool(*b),
            _ => return None,
        };
        if got.typ() == expected {
            Some(got)
        } else {
            None
        }
    }

    pub fn to_value(self) -> Value {
        match self {
            RegValue::I8(x) => Value::I8(x),
            RegValue::I16(x) => Value::I16(x),
            RegValue::I32(x) => Value::I32(x),
            RegValue::I64(x) => Value::I64(x),
            RegValue::U8(x) => Value::U8(x),
            RegValue::U16(x) => Value::U16(x),
            RegValue::U32(x) => Value::U32(x),
            RegValue::U64(x) => Value::U64(x),
            RegValue::F32(x) => Value::F32(x),
            RegValue::F64(x) => Value::F64(x),
            RegValue::Bool(b) => Value::Bool(b),
        }
    }

    fn as_bool(self) -> bool {
        match self {
            RegValue::Bool(b) => b,
            _ => panic!("as_bool: not a Bool — KIR is malformed"),
        }
    }
}

// ─── Interpreter env ─────────────────────────────────────────────

/// Scope-stack environment. Locals are pushed in lexical order; lookup
/// walks back-to-front so shadowing works correctly. At scope exit
/// (e.g. when a select arm's body completes), [`Self::truncate`] pops
/// everything pushed within that scope.
///
/// Why a Vec rather than a HashMap: typical fused kernels have under
/// ten locals, and linear scan over a small hot Vec is faster than
/// hash + branch + dereference. If we ever fuse kernels with dozens
/// of locals we can revisit.
struct InterpEnv {
    /// Each entry is `(name, value)`. The first `param_count` entries
    /// are the function's parameters and stay alive for the whole call;
    /// later entries are let-bindings or pattern-bindings that get
    /// popped on scope exit. Tail calls update parameter slots in
    /// place and truncate everything beyond `param_count`.
    locals: Vec<(arcstr::ArcStr, RegValue)>,
    param_count: usize,
}

impl InterpEnv {
    fn new(params: usize) -> Self {
        Self { locals: Vec::with_capacity(params + 4), param_count: params }
    }

    fn lookup(&self, name: &str) -> Option<RegValue> {
        for (n, v) in self.locals.iter().rev() {
            if n.as_str() == name {
                return Some(*v);
            }
        }
        None
    }

    fn push(&mut self, name: arcstr::ArcStr, value: RegValue) {
        self.locals.push((name, value));
    }

    fn mark(&self) -> usize {
        self.locals.len()
    }

    fn truncate(&mut self, n: usize) {
        self.locals.truncate(n);
    }

    /// Replace the value of the parameter at `idx`. Used by tail-call
    /// dispatch.
    fn set_param(&mut self, idx: usize, value: RegValue) {
        self.locals[idx].1 = value;
    }
}

/// Body evaluation control flow.
enum BodyResult {
    Return(RegValue),
    /// Tail call — params already updated in env, locals beyond params
    /// already truncated. Caller re-enters the body from the top.
    TailCall,
    /// A `KirOp::DynCall` returned `None` this cycle (the late-bound
    /// callee had no value yet). Kernel as a whole returns `None`,
    /// so the caller can re-fire when the callee produces.
    Pending,
}

/// Type-erased late-bound dispatcher passed through the interpreter
/// for `KirOp::DynCall`. Takes the kernel's `fn_index` plus the args
/// already converted to [`Value`]s, and returns `Some(value)` if the
/// callee produced a value this cycle or `None` if it didn't. The
/// closure body is created in [`KirNode`]'s `Apply<R, E>` impl and
/// captures the per-slot Apply state, so this trait-object is the
/// only place R/E generics surface in eval_kernel.
pub type DynDispatch<'a> = &'a mut dyn FnMut(u32, &[Value]) -> Option<Value>;

/// Dispatch closure that panics on any DynCall — used by callers
/// (and tests) that build kernels without HOF args.
fn no_dyn_dispatch(_idx: u32, _args: &[Value]) -> Option<Value> {
    panic!("KirOp::DynCall encountered but no dispatcher supplied")
}

// ─── Public entry points ─────────────────────────────────────────

/// Evaluate a [`KirKernel`] given a slice of typed argument values.
/// Returns the kernel's return value as a [`RegValue`]. Uses an
/// empty registry and panics on any `KirOp::DynCall` — for kernels
/// that contain DynCall sites, use [`eval_kernel_with_dispatch`].
pub fn eval_kernel(kernel: &KirKernel, args: &[RegValue]) -> RegValue {
    let empty = KernelRegistry::default();
    eval_kernel_with_registry(kernel, args, &empty)
}

/// Evaluate a [`KirKernel`], resolving any `KirOp::Call` against
/// `registry`. Panics on `KirOp::DynCall` — use
/// [`eval_kernel_with_dispatch`] for those.
pub fn eval_kernel_with_registry(
    kernel: &KirKernel,
    args: &[RegValue],
    registry: &KernelRegistry,
) -> RegValue {
    let mut dispatch = no_dyn_dispatch;
    eval_kernel_with_dispatch(kernel, args, registry, &mut dispatch)
        .expect("kernel without DynCall must produce a value")
}

/// Evaluate a [`KirKernel`] resolving `KirOp::Call` via `registry`
/// and `KirOp::DynCall` via `dispatch`. Returns `None` if a DynCall
/// produced no value this cycle (synchronous-only v1: the kernel
/// short-circuits and the caller must re-fire when the callee
/// catches up).
pub fn eval_kernel_with_dispatch(
    kernel: &KirKernel,
    args: &[RegValue],
    registry: &KernelRegistry,
    dispatch: DynDispatch<'_>,
) -> Option<RegValue> {
    debug_assert_eq!(
        args.len(),
        kernel.params.len(),
        "eval_kernel: arity mismatch"
    );
    let mut env = InterpEnv::new(kernel.params.len());
    for (p, v) in kernel.params.iter().zip(args) {
        debug_assert_eq!(
            v.typ(),
            p.prim,
            "eval_kernel: arg {} type mismatch",
            p.name
        );
        env.push(p.name.clone(), *v);
    }
    loop {
        match eval_body(&mut env, &kernel.body, registry, dispatch) {
            BodyResult::Return(v) => return Some(v),
            BodyResult::Pending => return None,
            BodyResult::TailCall => {
                // Loop back to the top of the body. params have been
                // updated and locals beyond params have been popped.
            }
        }
    }
}

// ─── Body / expression evaluation ────────────────────────────────

fn eval_body(
    env: &mut InterpEnv,
    stmts: &[KirStmt],
    registry: &KernelRegistry,
    dispatch: DynDispatch<'_>,
) -> BodyResult {
    for stmt in stmts {
        match stmt {
            KirStmt::Let(l) => {
                let v = match eval_expr(env, &l.value, registry, dispatch) {
                    Some(v) => v,
                    None => return BodyResult::Pending,
                };
                env.push(l.local.clone(), v);
            }
            KirStmt::Return(e) => {
                return match eval_expr(env, e, registry, dispatch) {
                    Some(v) => BodyResult::Return(v),
                    None => BodyResult::Pending,
                };
            }
            KirStmt::TailCall { args } => {
                // Evaluate all args first so an arg expression that
                // reads an old param sees the old value, not one we
                // already overwrote.
                let mut new_vals: smallvec::SmallVec<[RegValue; 8]> =
                    smallvec::SmallVec::with_capacity(args.len());
                for a in args {
                    match eval_expr(env, a, registry, dispatch) {
                        Some(v) => new_vals.push(v),
                        None => return BodyResult::Pending,
                    }
                }
                debug_assert_eq!(new_vals.len(), env.param_count);
                for (i, v) in new_vals.iter().enumerate() {
                    env.set_param(i, *v);
                }
                env.truncate(env.param_count);
                return BodyResult::TailCall;
            }
            KirStmt::Select { arms } => {
                for arm in arms {
                    let matches = match &arm.cond {
                        None => true,
                        Some(cond) => {
                            match eval_expr(env, cond, registry, dispatch) {
                                Some(v) => v.as_bool(),
                                None => return BodyResult::Pending,
                            }
                        }
                    };
                    if matches {
                        let mark = env.mark();
                        let r = eval_body(env, &arm.body, registry, dispatch);
                        if matches!(r, BodyResult::Return(_) | BodyResult::Pending) {
                            env.truncate(mark);
                        }
                        return r;
                    }
                }
                panic!(
                    "select fell through — typecheck should forbid; \
                     KIR is malformed"
                );
            }
        }
    }
    panic!(
        "body fell through without a return or tail call — KIR is \
         malformed"
    );
}

fn eval_expr(
    env: &mut InterpEnv,
    e: &KirExpr,
    registry: &KernelRegistry,
    dispatch: DynDispatch<'_>,
) -> Option<RegValue> {
    Some(match &e.op {
        KirOp::Const(c) => RegValue::from_const(*c),
        KirOp::Local(name) => env
            .lookup(name)
            .unwrap_or_else(|| panic!("undefined local `{name}` — KIR is malformed")),
        KirOp::Bin { op, lhs, rhs } => {
            let l = eval_expr(env, lhs, registry, dispatch)?;
            let r = eval_expr(env, rhs, registry, dispatch)?;
            eval_bin(*op, l, r)
        }
        KirOp::Cmp { op, lhs, rhs } => {
            let l = eval_expr(env, lhs, registry, dispatch)?;
            let r = eval_expr(env, rhs, registry, dispatch)?;
            eval_cmp(*op, l, r)
        }
        KirOp::BoolBin { op, lhs, rhs } => {
            // Short-circuit to match Rust && / ||.
            let l = eval_expr(env, lhs, registry, dispatch)?.as_bool();
            let result = match op {
                BoolOp::And => {
                    l && eval_expr(env, rhs, registry, dispatch)?.as_bool()
                }
                BoolOp::Or => {
                    l || eval_expr(env, rhs, registry, dispatch)?.as_bool()
                }
            };
            RegValue::Bool(result)
        }
        KirOp::Not(inner) => {
            RegValue::Bool(!eval_expr(env, inner, registry, dispatch)?.as_bool())
        }
        KirOp::Cast { inner, target } => {
            let v = eval_expr(env, inner, registry, dispatch)?;
            eval_cast(v, *target)
        }
        KirOp::Call { fn_name, args } => {
            // Look up the callee in the registry, evaluate args (in
            // the caller's env), then recursively eval the callee
            // with its own fresh env. The callee shares the same
            // registry and dispatch — its body might call further
            // kernels or have its own DynCall sites.
            let callee = registry.kernels.get(fn_name).unwrap_or_else(|| {
                panic!(
                    "KirOp::Call to `{fn_name}` not in kernel registry \
                     — Lambda::compile should have populated this"
                )
            });
            let mut call_args: smallvec::SmallVec<[RegValue; 8]> =
                smallvec::SmallVec::with_capacity(args.len());
            for a in args {
                call_args.push(eval_expr(env, a, registry, dispatch)?);
            }
            eval_kernel_with_dispatch(callee, &call_args, registry, dispatch)?
        }
        KirOp::DynCall {
            fn_index,
            args,
            arg_types,
            return_type,
        } => {
            // Evaluate args, convert to Values (the dispatcher's
            // contract is in netidx Value), call dispatcher, convert
            // result back. None propagates up through the kernel.
            debug_assert_eq!(args.len(), arg_types.len(), "DynCall arity mismatch");
            let mut value_args: smallvec::SmallVec<[Value; 8]> =
                smallvec::SmallVec::with_capacity(args.len());
            for (a, t) in args.iter().zip(arg_types.iter()) {
                let r = eval_expr(env, a, registry, dispatch)?;
                debug_assert_eq!(r.typ(), *t, "DynCall arg type mismatch");
                value_args.push(r.to_value());
            }
            let result = dispatch(*fn_index, &value_args)?;
            RegValue::from_value(&result, *return_type).unwrap_or_else(|| {
                panic!(
                    "DynCall returned a Value not matching the declared \
                     return type {:?} — typechecker should have caught \
                     this",
                    return_type
                )
            })
        }
        KirOp::Block { lets, tail } => {
            let mark = env.mark();
            for l in lets {
                let v = eval_expr(env, &l.value, registry, dispatch)?;
                env.push(l.local.clone(), v);
            }
            let result = eval_expr(env, tail, registry, dispatch)?;
            env.truncate(mark);
            result
        }
        KirOp::IfChain { arms } => {
            for (cond, body) in arms {
                let matches = match cond {
                    None => true,
                    Some(c) => eval_expr(env, c, registry, dispatch)?.as_bool(),
                };
                if matches {
                    return eval_expr(env, body, registry, dispatch);
                }
            }
            panic!("if-chain fell through without a match — KIR is malformed");
        }
    })
}

fn eval_bin(op: BinOp, lhs: RegValue, rhs: RegValue) -> RegValue {
    macro_rules! int_arith {
        ($a:expr, $b:expr, $variant:ident) => {
            RegValue::$variant(match op {
                BinOp::Add => $a.wrapping_add($b),
                BinOp::Sub => $a.wrapping_sub($b),
                BinOp::Mul => $a.wrapping_mul($b),
                BinOp::Div => $a.wrapping_div($b),
                BinOp::Mod => $a.wrapping_rem($b),
            })
        };
    }
    macro_rules! float_arith {
        ($a:expr, $b:expr, $variant:ident) => {
            RegValue::$variant(match op {
                BinOp::Add => $a + $b,
                BinOp::Sub => $a - $b,
                BinOp::Mul => $a * $b,
                BinOp::Div => $a / $b,
                BinOp::Mod => $a % $b,
            })
        };
    }
    match (lhs, rhs) {
        (RegValue::I8(a), RegValue::I8(b)) => int_arith!(a, b, I8),
        (RegValue::I16(a), RegValue::I16(b)) => int_arith!(a, b, I16),
        (RegValue::I32(a), RegValue::I32(b)) => int_arith!(a, b, I32),
        (RegValue::I64(a), RegValue::I64(b)) => int_arith!(a, b, I64),
        (RegValue::U8(a), RegValue::U8(b)) => int_arith!(a, b, U8),
        (RegValue::U16(a), RegValue::U16(b)) => int_arith!(a, b, U16),
        (RegValue::U32(a), RegValue::U32(b)) => int_arith!(a, b, U32),
        (RegValue::U64(a), RegValue::U64(b)) => int_arith!(a, b, U64),
        (RegValue::F32(a), RegValue::F32(b)) => float_arith!(a, b, F32),
        (RegValue::F64(a), RegValue::F64(b)) => float_arith!(a, b, F64),
        _ => panic!(
            "eval_bin: type mismatch ({:?} vs {:?}) — KIR is malformed",
            lhs.typ(),
            rhs.typ()
        ),
    }
}

fn eval_cmp(op: CmpOp, lhs: RegValue, rhs: RegValue) -> RegValue {
    macro_rules! cmp_dispatch {
        ($a:expr, $b:expr) => {
            RegValue::Bool(match op {
                CmpOp::Eq => $a == $b,
                CmpOp::Ne => $a != $b,
                CmpOp::Lt => $a < $b,
                CmpOp::Gt => $a > $b,
                CmpOp::Lte => $a <= $b,
                CmpOp::Gte => $a >= $b,
            })
        };
    }
    match (lhs, rhs) {
        (RegValue::I8(a), RegValue::I8(b)) => cmp_dispatch!(a, b),
        (RegValue::I16(a), RegValue::I16(b)) => cmp_dispatch!(a, b),
        (RegValue::I32(a), RegValue::I32(b)) => cmp_dispatch!(a, b),
        (RegValue::I64(a), RegValue::I64(b)) => cmp_dispatch!(a, b),
        (RegValue::U8(a), RegValue::U8(b)) => cmp_dispatch!(a, b),
        (RegValue::U16(a), RegValue::U16(b)) => cmp_dispatch!(a, b),
        (RegValue::U32(a), RegValue::U32(b)) => cmp_dispatch!(a, b),
        (RegValue::U64(a), RegValue::U64(b)) => cmp_dispatch!(a, b),
        (RegValue::F32(a), RegValue::F32(b)) => cmp_dispatch!(a, b),
        (RegValue::F64(a), RegValue::F64(b)) => cmp_dispatch!(a, b),
        (RegValue::Bool(a), RegValue::Bool(b)) => cmp_dispatch!(a, b),
        _ => panic!(
            "eval_cmp: type mismatch ({:?} vs {:?}) — KIR is malformed",
            lhs.typ(),
            rhs.typ()
        ),
    }
}

fn eval_cast(v: RegValue, target: PrimType) -> RegValue {
    macro_rules! cast_from {
        ($x:expr) => {
            match target {
                PrimType::I8 => RegValue::I8($x as i8),
                PrimType::I16 => RegValue::I16($x as i16),
                PrimType::I32 => RegValue::I32($x as i32),
                PrimType::I64 => RegValue::I64($x as i64),
                PrimType::U8 => RegValue::U8($x as u8),
                PrimType::U16 => RegValue::U16($x as u16),
                PrimType::U32 => RegValue::U32($x as u32),
                PrimType::U64 => RegValue::U64($x as u64),
                PrimType::F32 => RegValue::F32($x as f32),
                PrimType::F64 => RegValue::F64($x as f64),
                PrimType::Bool => panic!(
                    "cast to Bool not supported (kernel_ir::cast \
                     refuses bool↔int)"
                ),
            }
        };
    }
    match v {
        RegValue::I8(x) => cast_from!(x),
        RegValue::I16(x) => cast_from!(x),
        RegValue::I32(x) => cast_from!(x),
        RegValue::I64(x) => cast_from!(x),
        RegValue::U8(x) => cast_from!(x),
        RegValue::U16(x) => cast_from!(x),
        RegValue::U32(x) => cast_from!(x),
        RegValue::U64(x) => cast_from!(x),
        RegValue::F32(x) => cast_from!(x),
        RegValue::F64(x) => cast_from!(x),
        RegValue::Bool(_) => panic!(
            "cast from Bool not supported (kernel_ir::cast refuses \
             bool↔int)"
        ),
    }
}

// ─── Cross-kernel call dispatch ──────────────────────────────────

/// Runtime lookup table for `KirOp::Call`. Each kernel that fuses at
/// `Lambda::compile` time gets registered here under its source-level
/// binding name; an inner lambda whose body calls one of those names
/// produces a `KirOp::Call`, and the interpreter walks this map to
/// dispatch.
///
/// The registry is built once per Lambda::compile (snapshot of the
/// fusion-visibility scope at that point) and shared via `Arc` into
/// every `KirNode` the lambda's init produces.
#[derive(Debug, Default)]
pub struct KernelRegistry {
    pub kernels: BTreeMap<ArcStr, Arc<KirKernel>>,
}

// ─── KirNode: the Apply<R, E> wrapper ────────────────────────────

/// Per-DynCall-site state. For each fn-typed param of the kernel we
/// pre-allocate a slot containing the BindIds the side-channel uses,
/// the [`Ref`]-style nodes that read from those BindIds (passed as
/// `from` to the inner Apply's `update`), and a cache of the most
/// recently constructed inner Apply (invalidated when the LambdaDef
/// pointer changes).
///
/// Generic over `R, E` because the cached `Box<dyn Apply<R, E>>` and
/// the arg-ref nodes are.
pub struct DynCallSlot<R: Rt, E: UserEvent> {
    /// One BindId per callee argument. Pre-allocated at KirNode
    /// construction. The DynCall-time dispatcher writes the converted
    /// arg `Value` into `event.variables[bind_ids[i]]`; the matching
    /// `Ref` node in `arg_refs` reads it back inside the inner
    /// Apply's `update`.
    bind_ids: Vec<crate::BindId>,
    /// Per-arg `Ref` nodes that read from `bind_ids`. Passed as the
    /// `from: &mut [Node<R, E>]` slice to the inner Apply's `update`.
    arg_refs: Vec<Node<R, E>>,
    /// Cached `(LambdaDef pointer, Apply instance)`. Invalidated when
    /// a new LambdaDef arrives (different raw pointer) — typical case
    /// is the hot loop where the same callback is reused.
    current: Option<(*const u8, Box<dyn Apply<R, E>>)>,
    /// Lexical scope at the kernel's definition site. Re-passed to
    /// the inner Apply's `init` so it sees the right environment.
    scope: crate::Scope,
    /// Top-level expression id for the inner Apply's diagnostics.
    top_id: crate::expr::ExprId,
}

unsafe impl<R: Rt, E: UserEvent> Send for DynCallSlot<R, E> {}
unsafe impl<R: Rt, E: UserEvent> Sync for DynCallSlot<R, E> {}

impl<R: Rt, E: UserEvent> std::fmt::Debug for DynCallSlot<R, E> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("DynCallSlot")
            .field("bind_ids", &self.bind_ids.len())
            .field("cached", &self.current.is_some())
            .finish()
    }
}

impl<R: Rt, E: UserEvent> DynCallSlot<R, E> {
    /// Allocate a fresh slot for a kernel `FnParam`. `arg_types` gives
    /// the expected types of the callee's args; we allocate one
    /// BindId + one [`crate::node::bind::Ref`] node per arg.
    pub fn new(
        fn_param: &crate::kernel_ir::FnParam,
        scope: crate::Scope,
        top_id: crate::expr::ExprId,
    ) -> Self {
        let mut bind_ids = Vec::with_capacity(fn_param.arg_types.len());
        let mut arg_refs: Vec<Node<R, E>> =
            Vec::with_capacity(fn_param.arg_types.len());
        for prim in &fn_param.arg_types {
            let id = crate::BindId::new();
            bind_ids.push(id);
            // Ref reads `event.variables[id]` (or falls back to
            // `ctx.cached[id]`) on each `update`. `typ` is the
            // primitive Graphix type matching `prim`.
            let typ = prim_type_to_graphix_type(*prim);
            let node = crate::node::bind::Ref::new::<R, E>(
                id,
                typ,
                top_id,
                crate::expr::Expr::default(),
            );
            arg_refs.push(node);
        }
        Self { bind_ids, arg_refs, current: None, scope, top_id }
    }

    /// Dispatch the DynCall: look up (or initialize) the inner Apply,
    /// side-channel each arg through its BindId, run `apply.update`,
    /// clean up the BindIds, and return whatever Value the Apply
    /// produced this cycle (or `None` for synchronous-only-v1
    /// "no value yet").
    pub fn dispatch(
        &mut self,
        lambda_value: &Value,
        ctx: &mut crate::ExecCtx<R, E>,
        event: &mut crate::Event<E>,
        args: &[Value],
    ) -> Option<Value> {
        debug_assert_eq!(args.len(), self.bind_ids.len(), "DynCall arity");
        // Resolve the callee's LambdaDef out of the Value via
        // `downcast_ref`. We key the cache by the LambdaDef's address
        // (stable for the lifetime of the inner Arc<AbstractInner>),
        // so the hot path of "same callback re-invoked" reuses the
        // existing Apply without re-init'ing.
        let lambda_def = lambda_value
            .downcast_ref::<crate::node::lambda::LambdaDef<R, E>>()
            .unwrap_or_else(|| {
                panic!(
                    "DynCall: fn-arg value isn't a LambdaDef — \
                     typecheck should have rejected this"
                )
            });
        let lambda_ptr = lambda_def as *const _ as *const u8;
        let needs_init = match &self.current {
            Some((p, _)) if *p == lambda_ptr => false,
            _ => true,
        };
        if needs_init {
            // Drop the old Apply (if any) so it releases resources
            // before we initialize a new one.
            if let Some((_, mut prev)) = self.current.take() {
                prev.delete(ctx);
            }
            let new_apply = (lambda_def.init)(
                &self.scope,
                ctx,
                &mut self.arg_refs,
                None, // no resolved FnType; inner Apply uses its own typ
                self.top_id,
            )
            .ok()?;
            self.current = Some((lambda_ptr, new_apply));
        }
        // Side-channel: stash each arg Value at its BindId so the
        // arg_refs `Ref` nodes read it inside `apply.update`.
        let mut set: poolshark::local::LPooled<Vec<crate::BindId>> =
            poolshark::local::LPooled::take();
        for (i, v) in args.iter().enumerate() {
            let id = self.bind_ids[i];
            event.variables.insert(id, v.clone());
            set.push(id);
        }
        let result = {
            let apply = &mut self.current.as_mut().unwrap().1;
            apply.update(ctx, &mut self.arg_refs, event)
        };
        // Cleanup: remove the side-channel entries so a downstream
        // dispatcher (or the outer event loop) doesn't see them.
        for id in set.drain(..) {
            event.variables.remove(&id);
        }
        result
    }
}

/// Map a [`PrimType`] back to a Graphix [`crate::typ::Type`] so we can
/// build [`crate::node::bind::Ref`] nodes for DynCall arg-binders.
fn prim_type_to_graphix_type(p: PrimType) -> crate::typ::Type {
    let flag = match p {
        PrimType::I8 => netidx_value::Typ::I8,
        PrimType::I16 => netidx_value::Typ::I16,
        PrimType::I32 => netidx_value::Typ::I32,
        PrimType::I64 => netidx_value::Typ::I64,
        PrimType::U8 => netidx_value::Typ::U8,
        PrimType::U16 => netidx_value::Typ::U16,
        PrimType::U32 => netidx_value::Typ::U32,
        PrimType::U64 => netidx_value::Typ::U64,
        PrimType::F32 => netidx_value::Typ::F32,
        PrimType::F64 => netidx_value::Typ::F64,
        PrimType::Bool => netidx_value::Typ::Bool,
    };
    let mut flags = enumflags2::BitFlags::<netidx_value::Typ>::empty();
    flags.insert(flag);
    crate::typ::Type::Primitive(flags)
}

/// Wraps a [`KirKernel`] as an [`Apply<R, E>`] so the runtime can call
/// into the interpreter through the same dispatch path it uses for
/// every other function. On each `update` cycle we drive the input
/// nodes, cache their values, and (once every input slot is populated)
/// unpack into [`RegValue`]s and run [`eval_kernel`] — or, when the
/// JIT slot is filled, dispatch into native code via the wrapper.
///
/// Generic over `R, E` because the per-DynCall-slot state holds
/// `Box<dyn Apply<R, E>>` and `Node<R, E>`. (Pre-DynCall versions
/// were intentionally non-generic; the tradeoff is now in DynCall's
/// favor.)
pub struct KirNode<R: Rt, E: UserEvent> {
    /// The IR. `Arc` so structurally-identical kernels can share
    /// state (and, in M4e, share the JIT-compiled function pointer
    /// via an IR-hash cache).
    kernel: Arc<KirKernel>,
    /// Per-cycle input cache, parallel to the `from` slice the runtime
    /// passes into `update`. `None` means "haven't seen a value yet";
    /// the kernel runs once every slot is `Some`.
    args: Box<[Option<Value>]>,
    /// Synchronously-compiled JIT wrapper, when available. Filled at
    /// `Lambda::compile` time when `GRAPHIX_JIT=1` is set. Update
    /// dispatch checks this first.
    jit: Option<Arc<crate::kir_jit::WrappedKernel>>,
    /// Async JIT slot. Filled by the global background worker once
    /// it finishes compiling. `update` polls the slot on each call;
    /// before it's filled the interpreter runs, after it the JIT'd
    /// wrapper does. Set when `GRAPHIX_JIT_ASYNC=1` is set.
    async_jit: Option<Arc<crate::kir_jit::AsyncJitSlot>>,
    /// Fused kernels visible at this lambda's compile site, shared
    /// across all instantiations of the lambda. The interpreter uses
    /// this to dispatch `KirOp::Call`. Empty registry is fine —
    /// kernels that don't reference any cross-kernel calls just
    /// never look up.
    registry: Arc<KernelRegistry>,
    /// One slot per `kernel.fn_params` entry. Empty for kernels with
    /// no HOF args. The DynCall dispatcher closure (assembled inside
    /// `Apply<R, E>::update`) borrows this slice mutably to invoke
    /// inner Applies.
    dyn_slots: Vec<DynCallSlot<R, E>>,
    /// Pre-computed mapping from call-site arg position (index into
    /// `args`) to whether that position is a primitive param (with
    /// its index in `kernel.params`) or a function param (with its
    /// index in `kernel.fn_params`). Computed once at construction
    /// to avoid scanning `fn_params` per cycle.
    arg_layout: Vec<ArgKind>,
}

/// Tag for each call-site arg position: primitive param `i` (extract
/// to `RegValue` and pass to `eval_kernel_with_dispatch`) or fn
/// param `i` (use as the LambdaDef value for DynCall slot `i`).
#[derive(Debug, Clone, Copy)]
enum ArgKind {
    Prim(u32),
    Fn(u32),
}

fn build_arg_layout(kernel: &KirKernel) -> Vec<ArgKind> {
    use crate::kernel_ir::FnSource;
    // Only HOF-arg (`Param`-source) fn_params occupy slots in the
    // incoming `from` slice. `Binding`-source slots resolve through
    // ctx.cached at dispatch time and don't take a positional input.
    let param_source_count = kernel
        .fn_params
        .iter()
        .filter(|fp| matches!(fp.source, FnSource::Param { .. }))
        .count();
    let total = kernel.params.len() + param_source_count;
    let mut out = Vec::with_capacity(total);
    let mut prim_idx: u32 = 0;
    for i in 0..total {
        // Find any HOF-arg fn_param whose arg_pos == i.
        let fn_match = kernel.fn_params.iter().position(|fp| {
            matches!(fp.source, FnSource::Param { arg_pos } if arg_pos as usize == i)
        });
        match fn_match {
            Some(fn_idx) => out.push(ArgKind::Fn(fn_idx as u32)),
            None => {
                out.push(ArgKind::Prim(prim_idx));
                prim_idx += 1;
            }
        }
    }
    out
}

impl<R: Rt, E: UserEvent> std::fmt::Debug for KirNode<R, E> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("KirNode")
            .field("fn_name", &self.kernel.fn_name)
            .field("params", &self.kernel.params.len())
            .field("fn_params", &self.kernel.fn_params.len())
            .field("jit", &self.jit.is_some())
            .finish()
    }
}

impl<R: Rt, E: UserEvent> KirNode<R, E> {
    /// Construct a fresh KirNode for `kernel`, sized to match `n_args`
    /// input slots (which must equal `kernel.params.len() +
    /// kernel.fn_params.len()` — the input slice the runtime passes
    /// into `update` mixes prim and fn args). Runs through the
    /// interpreter; use [`Self::with_jit`] for synchronous JIT or
    /// [`Self::with_async_jit`] for background-compiled JIT.
    ///
    /// `scope` and `top_id` are used to initialize per-DynCall-slot
    /// state (the inner Applies that DynCall dispatches into).
    pub fn new(
        kernel: Arc<KirKernel>,
        n_args: usize,
        registry: Arc<KernelRegistry>,
        scope: crate::Scope,
        top_id: crate::expr::ExprId,
    ) -> Self {
        debug_assert_eq!(
            n_args,
            kernel.params.len() + kernel.fn_params.len(),
            "KirNode arity = prim params + fn params"
        );
        let dyn_slots = kernel
            .fn_params
            .iter()
            .map(|fp| DynCallSlot::new(fp, scope.clone(), top_id))
            .collect();
        let arg_layout = build_arg_layout(&kernel);
        Self {
            kernel,
            args: vec![None; n_args].into_boxed_slice(),
            jit: None,
            async_jit: None,
            registry,
            dyn_slots,
            arg_layout,
        }
    }

    /// Construct a KirNode whose `update` dispatches into a JIT'd
    /// kernel via `wrapped`. JIT path is only used for kernels that
    /// don't contain `KirOp::Call` or `KirOp::DynCall`.
    pub fn with_jit(
        kernel: Arc<KirKernel>,
        n_args: usize,
        wrapped: Arc<crate::kir_jit::WrappedKernel>,
        registry: Arc<KernelRegistry>,
        scope: crate::Scope,
        top_id: crate::expr::ExprId,
    ) -> Self {
        debug_assert_eq!(
            n_args,
            kernel.params.len() + kernel.fn_params.len()
        );
        let dyn_slots = kernel
            .fn_params
            .iter()
            .map(|fp| DynCallSlot::new(fp, scope.clone(), top_id))
            .collect();
        let arg_layout = build_arg_layout(&kernel);
        Self {
            kernel,
            args: vec![None; n_args].into_boxed_slice(),
            jit: Some(wrapped),
            async_jit: None,
            registry,
            dyn_slots,
            arg_layout,
        }
    }

    /// Construct a KirNode that interprets initially and atomic-
    /// swaps to a JIT'd wrapper when the async worker finishes
    /// compiling.
    pub fn with_async_jit(
        kernel: Arc<KirKernel>,
        n_args: usize,
        slot: Arc<crate::kir_jit::AsyncJitSlot>,
        registry: Arc<KernelRegistry>,
        scope: crate::Scope,
        top_id: crate::expr::ExprId,
    ) -> Self {
        debug_assert_eq!(
            n_args,
            kernel.params.len() + kernel.fn_params.len()
        );
        let dyn_slots = kernel
            .fn_params
            .iter()
            .map(|fp| DynCallSlot::new(fp, scope.clone(), top_id))
            .collect();
        let arg_layout = build_arg_layout(&kernel);
        Self {
            kernel,
            args: vec![None; n_args].into_boxed_slice(),
            jit: None,
            async_jit: Some(slot),
            registry,
            dyn_slots,
            arg_layout,
        }
    }
}

impl<R: Rt, E: UserEvent> Apply<R, E> for KirNode<R, E> {
    fn update(
        &mut self,
        ctx: &mut ExecCtx<R, E>,
        from: &mut [Node<R, E>],
        event: &mut Event<E>,
    ) -> Option<Value> {
        // Drive each child and cache its update. Mirrors what
        // `CachedVals::update` does: we want to fire the kernel only
        // when at least one input has produced this cycle, but use
        // cached values for inputs that didn't.
        let mut any_updated = false;
        for (i, src) in from.iter_mut().enumerate() {
            if let Some(v) = src.update(ctx, event) {
                self.args[i] = Some(v);
                any_updated = true;
            }
        }
        if !any_updated {
            return None;
        }
        // Classify into prim args (RegValue) and fn arg values
        // (kept as Value, used by the DynCall dispatcher). Both must
        // have a value before we can run; bail with None otherwise.
        let mut reg_args: smallvec::SmallVec<[RegValue; 8]> =
            smallvec::SmallVec::with_capacity(self.kernel.params.len());
        let mut fn_arg_values: smallvec::SmallVec<[Value; 4]> =
            smallvec::SmallVec::with_capacity(self.kernel.fn_params.len());
        // Pre-fill fn_arg_values with placeholders; we'll overwrite by
        // index. Use a Default value just to size the SmallVec.
        for _ in 0..self.kernel.fn_params.len() {
            fn_arg_values.push(Value::Null);
        }
        // Walk incoming args. Prim slots fill reg_args; Param-source
        // fn slots fill fn_arg_values at the matching fn_idx. Binding-
        // source fn slots aren't represented in arg_layout — we'll
        // resolve them from ctx.cached below.
        for (i, kind) in self.arg_layout.iter().enumerate() {
            let v = self.args[i].as_ref()?;
            match *kind {
                ArgKind::Prim(prim_idx) => {
                    let prim = self.kernel.params[prim_idx as usize].prim;
                    let r = RegValue::from_value(v, prim).unwrap_or_else(|| {
                        panic!(
                            "KirNode: arg {i} ({}) has wrong runtime type \
                             for declared {prim:?}",
                            self.kernel.params[prim_idx as usize].name
                        )
                    });
                    reg_args.push(r);
                }
                ArgKind::Fn(fn_idx) => {
                    fn_arg_values[fn_idx as usize] = v.clone();
                }
            }
        }
        // Resolve Binding-source fn slots by reading the BindId out
        // of `event.variables` first (current-cycle update) or
        // falling back to `ctx.cached` (prior cycle's value). If
        // neither has a value yet, the kernel can't run — return
        // None and try again next cycle.
        for (fn_idx, fp) in self.kernel.fn_params.iter().enumerate() {
            if let crate::kernel_ir::FnSource::Binding { bind_id } = &fp.source
            {
                let v = event
                    .variables
                    .get(bind_id)
                    .cloned()
                    .or_else(|| ctx.cached.get(bind_id).cloned())?;
                fn_arg_values[fn_idx] = v;
            }
        }
        // Pick a JIT wrapper if available. Sync JIT wins (already
        // compiled at Lambda::compile); else check the async slot
        // which atomic-fills when the worker thread finishes.
        let wrapper = match &self.jit {
            Some(w) => Some(w.clone()),
            None => self.async_jit.as_ref().and_then(|s| s.fetch()),
        };
        let result = match wrapper.as_ref() {
            None => {
                // Interpreter path. Build a dispatcher closure that
                // captures the fn-arg values and the per-slot Apply
                // state, so DynCall sites in the kernel can invoke
                // their callees through `apply.update`.
                let dyn_slots = &mut self.dyn_slots[..];
                let fn_arg_values_ref = &fn_arg_values;
                let mut dispatch = move |fn_index: u32, args: &[Value]| {
                    let slot = &mut dyn_slots[fn_index as usize];
                    let lambda_v = &fn_arg_values_ref[fn_index as usize];
                    slot.dispatch(lambda_v, ctx, event, args)
                };
                let r = eval_kernel_with_dispatch(
                    &self.kernel,
                    &reg_args,
                    &self.registry,
                    &mut dispatch,
                )?;
                r
            }
            Some(wrapped) => {
                // Pack RegValues into raw u64 slots, call through the
                // wrapper, unpack the result. `slots` is on the stack
                // — we cap at 16 args, which covers every plausible
                // fused kernel; over that we'd allocate a Vec. The
                // kir_jit pack helpers handle the bit munging.
                //
                // Note: JIT path is only entered when the kernel has
                // no DynCall sites, so fn_arg_values isn't touched.
                let mut slots: smallvec::SmallVec<[u64; 16]> =
                    smallvec::SmallVec::with_capacity(reg_args.len());
                for r in &reg_args {
                    slots.push(crate::kir_jit::pack_reg_to_u64(r));
                }
                let mut out: u64 = 0;
                let f = unsafe { wrapped.fn_ptr() };
                unsafe {
                    f(slots.as_ptr(), &mut out);
                }
                crate::kir_jit::unpack_u64_to_reg(out, self.kernel.return_type)
            }
        };
        Some(result.to_value())
    }

    fn sleep(&mut self, _ctx: &mut ExecCtx<R, E>) {
        for slot in self.args.iter_mut() {
            *slot = None;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::kernel_ir::{
        arith, bool_op, cast, cmp, const_expr, local, BinOp, BoolOp, CmpOp,
        ConstVal, Input, KirExpr, KirKernel, KirOp, KirStmt, Let, PrimType,
        SelectArm,
    };
    use arcstr::ArcStr;

    fn input(name: &str, prim: PrimType) -> Input {
        Input {
            name: ArcStr::from(name),
            prim,
            bind_id: None,
            rust_name: name.to_string(),
        }
    }

    fn loc(name: &str, prim: PrimType) -> KirExpr {
        local(ArcStr::from(name), prim)
    }

    #[test]
    fn arith_i64() {
        // Body: `a + b * c`
        let body = arith(
            loc("a", PrimType::I64),
            arith(
                loc("b", PrimType::I64),
                loc("c", PrimType::I64),
                BinOp::Mul,
            )
            .unwrap(),
            BinOp::Add,
        )
        .unwrap();
        let kernel = KirKernel {
            fn_name: ArcStr::from("ax_plus_bc"),
            params: vec![
                input("a", PrimType::I64),
                input("b", PrimType::I64),
                input("c", PrimType::I64),
            ],
            fn_params: vec![],
            return_type: PrimType::I64,
            has_tail_loop: false,
            body: vec![KirStmt::Return(body)],
        };
        let r = eval_kernel(
            &kernel,
            &[RegValue::I64(2), RegValue::I64(3), RegValue::I64(4)],
        );
        assert_eq!(r, RegValue::I64(2 + 3 * 4));
    }

    #[test]
    fn arith_f64_with_block() {
        // |a, b| -> f64 { let c = a + b; c * 2.0 }
        let lets = vec![Let {
            local: ArcStr::from("c"),
            value: arith(
                loc("a", PrimType::F64),
                loc("b", PrimType::F64),
                BinOp::Add,
            )
            .unwrap(),
        }];
        let tail = arith(
            loc("c", PrimType::F64),
            const_expr(ConstVal::F64(2.0)),
            BinOp::Mul,
        )
        .unwrap();
        let body = KirExpr {
            op: KirOp::Block { lets, tail: Box::new(tail) },
            typ: PrimType::F64,
        };
        let kernel = KirKernel {
            fn_name: ArcStr::from("scaled"),
            params: vec![input("a", PrimType::F64), input("b", PrimType::F64)],
            fn_params: vec![],
            return_type: PrimType::F64,
            has_tail_loop: false,
            body: vec![KirStmt::Return(body)],
        };
        let r = eval_kernel(&kernel, &[RegValue::F64(3.0), RegValue::F64(5.0)]);
        assert_eq!(r, RegValue::F64(16.0));
    }

    #[test]
    fn cmp_and_bool_ops() {
        // (x > 0.0) && (y < 10.0)
        let body = bool_op(
            cmp(
                loc("x", PrimType::F64),
                const_expr(ConstVal::F64(0.0)),
                CmpOp::Gt,
            )
            .unwrap(),
            cmp(
                loc("y", PrimType::F64),
                const_expr(ConstVal::F64(10.0)),
                CmpOp::Lt,
            )
            .unwrap(),
            BoolOp::And,
        )
        .unwrap();
        let kernel = KirKernel {
            fn_name: ArcStr::from("range_check"),
            params: vec![input("x", PrimType::F64), input("y", PrimType::F64)],
            fn_params: vec![],
            return_type: PrimType::Bool,
            has_tail_loop: false,
            body: vec![KirStmt::Return(body)],
        };
        let r = eval_kernel(&kernel, &[RegValue::F64(5.0), RegValue::F64(7.0)]);
        assert_eq!(r, RegValue::Bool(true));
        let r = eval_kernel(&kernel, &[RegValue::F64(-1.0), RegValue::F64(7.0)]);
        assert_eq!(r, RegValue::Bool(false));
        let r = eval_kernel(&kernel, &[RegValue::F64(5.0), RegValue::F64(20.0)]);
        assert_eq!(r, RegValue::Bool(false));
    }

    #[test]
    fn cast_int_to_float() {
        // |i| cast<f64>(i)
        let body = cast(loc("i", PrimType::I64), PrimType::F64).unwrap();
        let kernel = KirKernel {
            fn_name: ArcStr::from("itof"),
            params: vec![input("i", PrimType::I64)],
            fn_params: vec![],
            return_type: PrimType::F64,
            has_tail_loop: false,
            body: vec![KirStmt::Return(body)],
        };
        let r = eval_kernel(&kernel, &[RegValue::I64(42)]);
        assert_eq!(r, RegValue::F64(42.0));
    }

    /// Build mandelbrot's iterate KIR by hand and check known-output
    /// cases. The shape mirrors what `fusion::emit_function_kernel`
    /// produces — this test is the M5 differential's spiritual ancestor.
    fn mandelbrot_iterate_kernel() -> KirKernel {
        // body of select arm 1: return 0 (when i == 0)
        let arm0 = SelectArm {
            cond: Some(
                cmp(
                    loc("i", PrimType::I64),
                    const_expr(ConstVal::I64(0)),
                    CmpOp::Eq,
                )
                .unwrap(),
            ),
            body: vec![KirStmt::Return(const_expr(ConstVal::I64(0)))],
        };
        // body of arm 2: return i (when escaped)
        let escaped_cond = cmp(
            arith(
                arith(
                    loc("zr", PrimType::F64),
                    loc("zr", PrimType::F64),
                    BinOp::Mul,
                )
                .unwrap(),
                arith(
                    loc("zi", PrimType::F64),
                    loc("zi", PrimType::F64),
                    BinOp::Mul,
                )
                .unwrap(),
                BinOp::Add,
            )
            .unwrap(),
            const_expr(ConstVal::F64(4.0)),
            CmpOp::Gt,
        )
        .unwrap();
        let arm1 = SelectArm {
            cond: Some(escaped_cond),
            body: vec![KirStmt::Return(loc("i", PrimType::I64))],
        };
        // body of arm 3: tail-call iterate(zr*zr - zi*zi + cr, 2*zr*zi + ci, cr, ci, i-1)
        let new_zr = arith(
            arith(
                arith(
                    loc("zr", PrimType::F64),
                    loc("zr", PrimType::F64),
                    BinOp::Mul,
                )
                .unwrap(),
                arith(
                    loc("zi", PrimType::F64),
                    loc("zi", PrimType::F64),
                    BinOp::Mul,
                )
                .unwrap(),
                BinOp::Sub,
            )
            .unwrap(),
            loc("cr", PrimType::F64),
            BinOp::Add,
        )
        .unwrap();
        let new_zi = arith(
            arith(
                arith(
                    const_expr(ConstVal::F64(2.0)),
                    loc("zr", PrimType::F64),
                    BinOp::Mul,
                )
                .unwrap(),
                loc("zi", PrimType::F64),
                BinOp::Mul,
            )
            .unwrap(),
            loc("ci", PrimType::F64),
            BinOp::Add,
        )
        .unwrap();
        let new_i = arith(
            loc("i", PrimType::I64),
            const_expr(ConstVal::I64(1)),
            BinOp::Sub,
        )
        .unwrap();
        let arm2 = SelectArm {
            cond: None,
            body: vec![KirStmt::TailCall {
                args: vec![
                    new_zr,
                    new_zi,
                    loc("cr", PrimType::F64),
                    loc("ci", PrimType::F64),
                    new_i,
                ],
            }],
        };
        KirKernel {
            fn_name: ArcStr::from("iterate"),
            params: vec![
                input("zr", PrimType::F64),
                input("zi", PrimType::F64),
                input("cr", PrimType::F64),
                input("ci", PrimType::F64),
                input("i", PrimType::I64),
            ],
            fn_params: vec![],
            return_type: PrimType::I64,
            has_tail_loop: true,
            body: vec![KirStmt::Select { arms: vec![arm0, arm1, arm2] }],
        }
    }

    #[test]
    fn mandelbrot_iterate_inside_set_returns_zero() {
        // c = 0+0i: z stays at 0 forever, |z|² never exceeds 4, so we
        // exhaust i and return 0.
        let kernel = mandelbrot_iterate_kernel();
        let r = eval_kernel(
            &kernel,
            &[
                RegValue::F64(0.0),
                RegValue::F64(0.0),
                RegValue::F64(0.0),
                RegValue::F64(0.0),
                RegValue::I64(20),
            ],
        );
        assert_eq!(r, RegValue::I64(0));
    }

    #[test]
    fn mandelbrot_iterate_outside_set_escapes() {
        // c = 1+0i: z evolves 0 → 1 → 2 → 5 → ...; |2|² = 4 (NOT > 4),
        // |5|² = 25 > 4 so escape on the next iteration.
        // Trace from i=10:
        //   i=10 (zr=0): not zero, |0|²=0 not > 4, recurse with zr=1, i=9
        //   i=9  (zr=1): |1|²=1 not > 4, recurse with zr=2, i=8
        //   i=8  (zr=2): |2|²=4 not > 4 (strict), recurse with zr=5, i=7
        //   i=7  (zr=5): |5|²=25 > 4 → return i = 7
        let kernel = mandelbrot_iterate_kernel();
        let r = eval_kernel(
            &kernel,
            &[
                RegValue::F64(0.0),
                RegValue::F64(0.0),
                RegValue::F64(1.0),
                RegValue::F64(0.0),
                RegValue::I64(10),
            ],
        );
        assert_eq!(r, RegValue::I64(7));
    }

    #[test]
    fn naive_fib_via_recursion() {
        // We can't yet do cross-kernel calls in the interpreter, so
        // simulate naive fib via tail-recursion by hand: it's actually
        // not naturally tail-recursive, so this test instead exercises
        // a self-recursive Tower-of-power: countdown(n) = if n == 0 { 0 }
        // else { countdown(n - 1) }  → always returns 0, but exercises
        // the tail-call+loop path.
        let arm0 = SelectArm {
            cond: Some(
                cmp(
                    loc("n", PrimType::I64),
                    const_expr(ConstVal::I64(0)),
                    CmpOp::Eq,
                )
                .unwrap(),
            ),
            body: vec![KirStmt::Return(const_expr(ConstVal::I64(0)))],
        };
        let arm1 = SelectArm {
            cond: None,
            body: vec![KirStmt::TailCall {
                args: vec![arith(
                    loc("n", PrimType::I64),
                    const_expr(ConstVal::I64(1)),
                    BinOp::Sub,
                )
                .unwrap()],
            }],
        };
        let kernel = KirKernel {
            fn_name: ArcStr::from("countdown"),
            params: vec![input("n", PrimType::I64)],
            fn_params: vec![],
            return_type: PrimType::I64,
            has_tail_loop: true,
            body: vec![KirStmt::Select { arms: vec![arm0, arm1] }],
        };
        let r = eval_kernel(&kernel, &[RegValue::I64(1_000_000)]);
        assert_eq!(r, RegValue::I64(0));
    }

    #[test]
    fn block_lets_pop_on_scope_exit() {
        // |x: i64| -> i64 { let y = x + 1; y } + (let-shadow doesn't
        // happen here but we verify push/pop hygiene: outer x must
        // remain visible after the block).
        // We'll fold a block that pushes y, then add to x outside.
        // Body: { let y = x + 1; y } + x
        let inner_block = KirExpr {
            op: KirOp::Block {
                lets: vec![Let {
                    local: ArcStr::from("y"),
                    value: arith(
                        loc("x", PrimType::I64),
                        const_expr(ConstVal::I64(1)),
                        BinOp::Add,
                    )
                    .unwrap(),
                }],
                tail: Box::new(loc("y", PrimType::I64)),
            },
            typ: PrimType::I64,
        };
        let body = arith(inner_block, loc("x", PrimType::I64), BinOp::Add).unwrap();
        let kernel = KirKernel {
            fn_name: ArcStr::from("inner_y_plus_x"),
            params: vec![input("x", PrimType::I64)],
            fn_params: vec![],
            return_type: PrimType::I64,
            has_tail_loop: false,
            body: vec![KirStmt::Return(body)],
        };
        let r = eval_kernel(&kernel, &[RegValue::I64(10)]);
        // y = 10 + 1 = 11; result = 11 + 10 = 21
        assert_eq!(r, RegValue::I64(21));
    }

    #[test]
    fn ifchain_expression() {
        // |x: i64| if x > 0 { 1 } else if x < 0 { -1 } else { 0 }
        let chain = KirExpr {
            op: KirOp::IfChain {
                arms: vec![
                    (
                        Some(
                            cmp(
                                loc("x", PrimType::I64),
                                const_expr(ConstVal::I64(0)),
                                CmpOp::Gt,
                            )
                            .unwrap(),
                        ),
                        const_expr(ConstVal::I64(1)),
                    ),
                    (
                        Some(
                            cmp(
                                loc("x", PrimType::I64),
                                const_expr(ConstVal::I64(0)),
                                CmpOp::Lt,
                            )
                            .unwrap(),
                        ),
                        const_expr(ConstVal::I64(-1)),
                    ),
                    (None, const_expr(ConstVal::I64(0))),
                ],
            },
            typ: PrimType::I64,
        };
        let kernel = KirKernel {
            fn_name: ArcStr::from("sign"),
            params: vec![input("x", PrimType::I64)],
            fn_params: vec![],
            return_type: PrimType::I64,
            has_tail_loop: false,
            body: vec![KirStmt::Return(chain)],
        };
        assert_eq!(
            eval_kernel(&kernel, &[RegValue::I64(5)]),
            RegValue::I64(1)
        );
        assert_eq!(
            eval_kernel(&kernel, &[RegValue::I64(-3)]),
            RegValue::I64(-1)
        );
        assert_eq!(
            eval_kernel(&kernel, &[RegValue::I64(0)]),
            RegValue::I64(0)
        );
    }

    #[test]
    fn integer_overflow_wraps() {
        // i64::MAX + 1 wraps to i64::MIN, matching Rust release-mode
        // arithmetic and what the AOT-emitted Rust does.
        let body = arith(
            loc("x", PrimType::I64),
            const_expr(ConstVal::I64(1)),
            BinOp::Add,
        )
        .unwrap();
        let kernel = KirKernel {
            fn_name: ArcStr::from("inc"),
            params: vec![input("x", PrimType::I64)],
            fn_params: vec![],
            return_type: PrimType::I64,
            has_tail_loop: false,
            body: vec![KirStmt::Return(body)],
        };
        let r = eval_kernel(&kernel, &[RegValue::I64(i64::MAX)]);
        assert_eq!(r, RegValue::I64(i64::MIN));
    }

    #[test]
    fn value_to_reg_round_trip() {
        // Value → RegValue → Value should be lossless for primitives.
        let cases: &[(Value, PrimType, RegValue)] = &[
            (Value::I64(42), PrimType::I64, RegValue::I64(42)),
            (Value::F64(3.14), PrimType::F64, RegValue::F64(3.14)),
            (Value::Bool(true), PrimType::Bool, RegValue::Bool(true)),
            (Value::U32(7), PrimType::U32, RegValue::U32(7)),
        ];
        for (v, p, expected) in cases {
            let r = RegValue::from_value(v, *p).expect("conversion should succeed");
            assert_eq!(&r, expected);
            assert_eq!(r.to_value(), *v);
        }
        // Type mismatch returns None.
        assert!(RegValue::from_value(&Value::I64(42), PrimType::F64).is_none());
        assert!(RegValue::from_value(&Value::Bool(true), PrimType::I64).is_none());
    }
}
