// Primitive arithmetic operator edge cases.
//
// These pin operator semantics that the `math::*` builtin tests don't
// cover: IEEE float edges (inexact rounding, div-by-zero → inf/NaN,
// signed zero, subnormals, no fused-multiply-add contraction) and
// checked-arithmetic overflow detection. Every fixture runs through
// `run!`, which asserts the node-walk, interp-fusion, and JIT modes all
// agree — so these double as a cross-mode float-determinism guard (a
// last-bit divergence between cranelift and the node-walk's scalar Rust
// would fail the cross-mode check).
//
// The checked-overflow fixtures were prompted by a real bug: `+?`/`-?`/
// `*?` used the wrapping `+`/`-`/`*` operators and never detected
// overflow (only `/?`/`%?` worked). Fixed in node/op.rs by routing the
// checked variants through `Value::checked_*`.

use anyhow::Result;
use graphix_package_core::run;
use netidx::subscriber::Value;

// ── IEEE float edges (these fuse + JIT) ──

// Inexact: 0.1 + 0.2 is not 0.3.
const FLOAT_ADD_INEXACT: &str = "f64:0.1 + f64:0.2";
run!(float_add_inexact, FLOAT_ADD_INEXACT, |v: Result<&Value>| {
    matches!(v, Ok(Value::F64(f)) if *f == 0.1 + 0.2)
});

// No fused-multiply-add contraction: a*b+c must round a*b first, then
// add c (two roundings), matching the node-walk's scalar Rust. If
// cranelift ever contracted this to a single-rounding `fma`, the value
// would differ from the separate-rounding Rust computation AND the
// cross-mode check would fail.
const FLOAT_FMA_NO_CONTRACT: &str = "f64:0.1 * f64:0.2 + f64:0.3";
run!(float_fma_no_contract, FLOAT_FMA_NO_CONTRACT, |v: Result<&Value>| {
    matches!(v, Ok(Value::F64(f)) if *f == 0.1_f64 * 0.2 + 0.3)
});

// Float division by zero is IEEE inf, not a graphix error (that's
// integer-only).
const FLOAT_DIV_ZERO_INF: &str = "f64:1.0 / f64:0.0";
run!(float_div_zero_inf, FLOAT_DIV_ZERO_INF, |v: Result<&Value>| {
    matches!(v, Ok(Value::F64(f)) if f.is_infinite() && *f > 0.0)
});

// Sign of zero propagates: 1.0 / -0.0 is -inf.
const FLOAT_NEG_DIV_ZERO: &str = "f64:1.0 / f64:-0.0";
run!(float_neg_div_zero, FLOAT_NEG_DIV_ZERO, |v: Result<&Value>| {
    matches!(v, Ok(Value::F64(f)) if f.is_infinite() && *f < 0.0)
});

// 0.0 / 0.0 is NaN.
const FLOAT_ZERO_DIV_ZERO_NAN: &str = "f64:0.0 / f64:0.0";
run!(float_zero_div_zero_nan, FLOAT_ZERO_DIV_ZERO_NAN, |v: Result<&Value>| {
    matches!(v, Ok(Value::F64(f)) if f.is_nan())
});

// graphix float comparison uses a TOTAL order (NaN == NaN, and NaN sorts
// below every non-NaN value) so `Value` is usable as a map key — NOT
// IEEE. The node-walk uses `Value::partial_cmp`; the fused interp + JIT
// paths were fixed to match (task #174). These pin that all three modes
// agree on the total order.

// NaN equals itself (the deliberate non-IEEE choice).
const NAN_EQ_SELF: &str = "{ let x = f64:0.0 / f64:0.0; x == x }";
run!(nan_eq_self, NAN_EQ_SELF, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
});

// ...so `!=` on NaN is false.
const NAN_NE_SELF: &str = "{ let x = f64:0.0 / f64:0.0; x != x }";
run!(nan_ne_self, NAN_NE_SELF, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(false)))
});

// NaN sorts below every non-NaN value: NaN < 1.0 is true.
const NAN_LT_VALUE: &str = "{ let x = f64:0.0 / f64:0.0; x < f64:1.0 }";
run!(nan_lt_value, NAN_LT_VALUE, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
});

// ...and a non-NaN value is greater than NaN: 1.0 < NaN is false.
const VALUE_LT_NAN: &str = "{ let x = f64:0.0 / f64:0.0; f64:1.0 < x }";
run!(value_lt_nan, VALUE_LT_NAN, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(false)))
});

// Signed zeros compare equal.
const FLOAT_NEG_ZERO_EQ_ZERO: &str = "f64:-0.0 == f64:0.0";
run!(float_neg_zero_eq_zero, FLOAT_NEG_ZERO_EQ_ZERO, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
});

// Subnormal arithmetic: doubling the smallest positive subnormal.
const FLOAT_SUBNORMAL: &str = "f64:5e-324 + f64:5e-324";
run!(float_subnormal, FLOAT_SUBNORMAL, |v: Result<&Value>| {
    matches!(v, Ok(Value::F64(f)) if *f == 5e-324_f64 + 5e-324)
});

// Float modulo. Cranelift has no `frem`, so the JIT bails — a kernel that
// can't JIT isn't spliced at all, so the program node-walks
// (FuseExpect::None). It must still produce the
// right value. Regression for a fuzzer-found bug (#175): the JIT's
// float-`%` arm emitted a runtime trap that crashed the whole runtime
// instead of bailing. Found by graphix-fuzz on `f64:7.0 % f64:3.0`.
const FLOAT_MOD: &str = "f64:7.0 % f64:3.0";
run!(float_mod, FLOAT_MOD, |v: Result<&Value>| {
    matches!(v, Ok(Value::F64(f)) if *f == 1.0)
}; graphix_package_core::testing::FuseExpect::None);

// f32 inexact add.
const F32_ADD_INEXACT: &str = "f32:0.1 + f32:0.2";
run!(f32_add_inexact, F32_ADD_INEXACT, |v: Result<&Value>| {
    matches!(v, Ok(Value::F32(f)) if *f == 0.1_f32 + 0.2)
});

// ── Checked-arithmetic overflow detection (regression for the
//    +?/-?/*? wrapping bug). These don't fuse (`+?` has no fusion
//    lowering), so they run the node-walk. ──

const CHECKED_ADD_OVERFLOW: &str = "is_err(i64:9223372036854775807 +? i64:1)";
run!(checked_add_overflow_errs, CHECKED_ADD_OVERFLOW, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
}; graphix_package_core::testing::FuseExpect::Jit);

const CHECKED_SUB_UNDERFLOW: &str = "is_err(i64:-9223372036854775808 -? i64:1)";
run!(checked_sub_underflow_errs, CHECKED_SUB_UNDERFLOW, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
}; graphix_package_core::testing::FuseExpect::Jit);

const CHECKED_MUL_OVERFLOW: &str = "is_err(i64:9223372036854775807 *? i64:2)";
run!(checked_mul_overflow_errs, CHECKED_MUL_OVERFLOW, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
}; graphix_package_core::testing::FuseExpect::Jit);

const CHECKED_U8_OVERFLOW: &str = "is_err(u8:200 +? u8:100)";
run!(checked_u8_overflow_errs, CHECKED_U8_OVERFLOW, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
}; graphix_package_core::testing::FuseExpect::Jit);

const CHECKED_U8_UNDERFLOW: &str = "is_err(u8:0 -? u8:1)";
run!(checked_u8_underflow_errs, CHECKED_U8_UNDERFLOW, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
}; graphix_package_core::testing::FuseExpect::Jit);

// Checked div-by-zero already worked before the fix — guard against
// regression.
const CHECKED_DIV_ZERO: &str = "is_err(i64:10 /? i64:0)";
run!(checked_div_zero_errs, CHECKED_DIV_ZERO, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
}; graphix_package_core::testing::FuseExpect::Jit);

// No overflow: the checked op returns the bare value, not an error.
const CHECKED_NO_OVERFLOW: &str = "i64:5 +? i64:3";
run!(checked_no_overflow_ok, CHECKED_NO_OVERFLOW, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(8)))
}; graphix_package_core::testing::FuseExpect::Jit);

// Unchecked overflow deliberately wraps (the fast path); pin that so a
// future change to unchecked semantics is a conscious decision. This one
// fuses + JITs.
const UNCHECKED_OVERFLOW_WRAPS: &str = "i64:9223372036854775807 + i64:1";
run!(unchecked_overflow_wraps, UNCHECKED_OVERFLOW_WRAPS, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(i)) if *i == i64::MIN)
});

// ── Source E (adversarial fuzzer, #176) regressions ──

// Cluster A: integer div/rem by zero / signed MIN-/-1 overflow must NOT
// crash the runtime — the node-walk drops to bottom (arith error), so the
// fused backends must too: a divisor guard → pending_exit in the JIT, and
// checked_div → None (bottom) in the interp. We can't assert "bottom" via
// run! (it would time out), so pin that VALID division still works AND
// JITs — i.e. the guard didn't break the common path.
const DIV_VALID: &str = "i64:10 / i64:2";
run!(div_valid, DIV_VALID, |v: Result<&Value>| matches!(v, Ok(Value::I64(5))));

const MOD_VALID: &str = "i64:10 % i64:3";
run!(mod_valid, MOD_VALID, |v: Result<&Value>| matches!(v, Ok(Value::I64(1))));

// A div-by-zero in an UN-taken select arm: the guard codegen is exercised
// (the arm fuses) and the program still produces a value.
const DIV_IN_ARM: &str = "select i64:1 { 1 => i64:42, n => n / i64:0 }";
run!(div_in_untaken_arm, DIV_IN_ARM, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(42)))
});

// Cluster B: an error() value passed to a builtin (a value-shape DynCall
// arg) must not crash the fused interp — the marshalling match was missing
// the value-shape error arm.
const ERR_DYNCALL_ARG: &str = "is_err(error(1))";
run!(err_as_dyncall_arg, ERR_DYNCALL_ARG, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
});

// Cluster D: interpolating a non-scalar part (Nullable<string> from an
// array index) must bail the StringInterpolate to the node-walk, not crash
// the fused Concat (which only handles String/scalar parts).
const INTERP_NONSCALAR: &str =
    "{ let words = [\"alpha\", \"beta\"]; \"first=[words[0]]\" }";
run!(interp_nonscalar_part, INTERP_NONSCALAR, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if s == "first=alpha")
}; graphix_package_core::testing::FuseExpect::Jit);

// ── Dead-statement elimination (fuzzer-found, block bottom-poison) ──
//
// A fused block evaluated EVERY statement eagerly, so a bottom in a non-
// tail statement (div/mod-by-zero → DYNCALL_PENDING) poisoned the whole
// kernel — while the node-walk treats each statement as an independent
// node: an unreferenced bottom binding simply never fires and the tail
// still produces. `prune_dead_stmts`/`prune_dead_lets` drop dead, pure
// non-tail statements so the bottom never executes. All three modes must
// now agree on the tail value (and the pruned block still fuses).

// A dead (unreferenced) bottom `let` is dropped; the tail is produced.
const DEAD_LET_BOTTOM: &str = "{ let v = i64:1 / i64:0; i64:42 }";
run!(dead_let_bottom, DEAD_LET_BOTTOM, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(42)))
});

// A bare (value-discarded) bottom statement is dropped.
const BARE_BOTTOM_STMT: &str = "{ i64:1 / i64:0; i64:42 }";
run!(bare_bottom_stmt, BARE_BOTTOM_STMT, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(42)))
});

// The backward-fixpoint case: `v` is referenced only by a bare statement
// that is itself dropped, so `v`'s bottom binding is dropped too.
const DEAD_LET_FIXPOINT: &str = "{ let v = i64:1 / i64:0; v; i64:42 }";
run!(dead_let_fixpoint, DEAD_LET_FIXPOINT, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(42)))
});

// Pruning must NOT touch a live binding: `a` flows to the tail, only the
// dead `b` (a bottom) is dropped.
const DEAD_LET_KEEPS_LIVE: &str = "{ let a = i64:7; let b = i64:1 / i64:0; a + i64:1 }";
run!(dead_let_keeps_live, DEAD_LET_KEEPS_LIVE, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(8)))
});

// ── Select-arm sinking (fuzzer-found, per-arm laziness) ──
//
// The node-walk evaluates select arms lazily (an unselected arm's body
// never fires), so a bottom (div0/mod0) feeding ONLY an un-taken arm
// yields the taken arm's value. Dead-statement elimination can't fix this
// (the binding IS referenced — by an un-taken arm). `sink_into_select_stmts`
// moves the arm-exclusive bottom-capable let INTO the consuming arm body,
// where it becomes lazy for free (arm bodies are already lazy in both
// fused backends). All three modes must agree, and the pruned/sunk block
// still fuses.

// v feeds only the un-taken arm (scrutinee 5, arm key 2) -> v sinks into
// arm 2 -> never runs -> 99.
const SINK_ONE_ARM: &str =
    "{ let v = i64:1 / i64:0; select i64:5 { 2 => v, _ => i64:99 } }";
run!(sink_one_arm, SINK_ONE_ARM, |v: Result<&Value>| matches!(v, Ok(Value::I64(99))));

// mod-by-zero — operator-agnostic, same mechanism.
const SINK_MOD: &str = "{ let v = i64:1 % i64:0; select i64:5 { 2 => v, _ => i64:99 } }";
run!(sink_mod, SINK_MOD, |v: Result<&Value>| matches!(v, Ok(Value::I64(99))));

// v feeds TWO un-taken arms -> duplicated into each (pure ⇒ idempotent).
const SINK_MULTI_ARM: &str =
    "{ let v = i64:1 / i64:0; select i64:5 { 1 => v, 2 => v, _ => i64:99 } }";
run!(sink_multi_arm, SINK_MULTI_ARM, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(99)))
});

// Dependency chain v -> w, w feeds only an un-taken arm: the fixpoint sinks
// w first, then v follows into the same arm.
const SINK_CHAIN: &str =
    "{ let v = i64:1 / i64:0; let w = v + i64:1; select i64:5 { 2 => w, _ => i64:99 } }";
run!(sink_chain, SINK_CHAIN, |v: Result<&Value>| matches!(v, Ok(Value::I64(99))));

// v is referenced only inside a nested select living in the outer un-taken
// arm -> v sinks into the outer arm body.
const SINK_NESTED: &str = "{ let v = i64:1 / i64:0; \
    select i64:5 { 2 => select i64:1 { 1 => v, _ => i64:0 }, _ => i64:99 } }";
run!(sink_nested, SINK_NESTED, |v: Result<&Value>| matches!(v, Ok(Value::I64(99))));

// A bottom in a let consumed by the TAKEN arm must STILL bottom (matches
// node-walk): v sinks into arm 2 but arm 2 IS taken -> all three Timeout.
// Can't assert Timeout via run!; instead pin the sibling where the scrutinee
// picks the literal arm whose body does NOT use v -> 99 even though v is
// referenced by the (un-taken) catch-all.
const SINK_LITERAL_ARM_TAKEN: &str =
    "{ let v = i64:1 / i64:0; select i64:7 { 7 => i64:42, _ => v } }";
run!(sink_literal_arm_taken, SINK_LITERAL_ARM_TAKEN, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(42)))
});

// Expression form: a block-with-select used as a sub-expression lowers to
// a block of lets with an if-chain tail; the sink pass must reach the
// IfChain arms there too (here the block is an arithmetic operand).
const SINK_EXPR_OPERAND: &str =
    "({ let v = i64:1 / i64:0; select i64:5 { 2 => v, _ => i64:99 } }) + i64:1";
run!(sink_expr_operand, SINK_EXPR_OPERAND, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(100)))
});

// A bottom-capable let that ALSO contains a (pure) call: the `expr_has_call`
// gate must not exclude it — `1/0` is bottom-capable, so it sinks even
// though `str::len` is a call. (Caught by adversarial review.)
const SINK_CALL_BEARING: &str =
    "{ let v = i64:1 / i64:0 + str::len(\"x\"); select i64:5 { 2 => v, _ => i64:99 } }";
run!(sink_call_bearing, SINK_CALL_BEARING, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(99)))
});

// A `?`-on-parse-error bottom feeding an un-taken arm: the QopUnwrap makes
// the let bottom-capable, so it sinks (the str::parse call is value-
// returning/pure). Sunk -> the parse never runs on the taken arm -> 99.
const SINK_QOP_PARSE: &str =
    "{ let v: i64 = str::parse(\"notanumber\")?; select i64:5 { 2 => v, _ => i64:99 } }";
run!(sink_qop_parse, SINK_QOP_PARSE, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(99)))
});

// Dependency chain THROUGH a multi-arm let: v (pure) feeds two un-taken
// arms, w (bottom) feeds v. Both must sink (v duplicated, w follows) — the
// multi-arm gate had to be dropped for this. (Caught by adversarial review.)
const SINK_MULTI_ARM_CHAIN: &str = "{ let x = str::len(\"hello\"); let w = x / i64:0; \
    let v = w + i64:1; select x { 5 => i64:99, 6 => v, _ => v } }";
run!(sink_multi_arm_chain, SINK_MULTI_ARM_CHAIN, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(99)))
});

// A bottom whose only consumer is an arm of a NESTED select inside an outer
// un-taken arm: the sink must recurse into nested selects. s=7 -> outer arm
// 7 -> inner select t=2 -> `_ => 50` (v's `3 =>` arm un-taken). (Caught by
// adversarial review.)
const SINK_NESTED_STMT: &str = "{ let s = i64:7; let t = i64:2; let v = i64:10 / i64:0; \
    select s { 7 => select t { 3 => v + i64:1, _ => i64:50 }, _ => i64:99 } }";
run!(sink_nested_stmt, SINK_NESTED_STMT, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(50)))
});

// An impure NON-bottom let (a bare `rand`) must stay EAGER so its effect
// can't become arm-conditional — and since rand doesn't bottom, eager eval
// is harmless and all modes agree at the taken arm's 99.
const SINK_RAND_STAYS_EAGER: &str =
    "{ let v = rand::rand(#start: 0, #end: 9, #clock: 1); select i64:5 { 2 => v, _ => i64:99 } }";
run!(sink_rand_stays_eager, SINK_RAND_STAYS_EAGER, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(99)))
}; graphix_package_core::testing::FuseExpect::Jit);

// ── Bottom scrutinee vs bottom guard (differential-found) ──
//
// A bottom (None) SCRUTINEE must poison the whole select — the node-walk's
// Select node only fires when the scrutinee has a value — NOT fall through
// to the catch-all. The fused IfChain/Select folds the scrutinee into the
// per-arm conds, so a bottom scrutinee previously made the conditional arm
// fail to match and the `_` catch-all (cond None) win, returning its value
// instead of bottom. Fixed via a `scrut` gate evaluated up front and
// bottom-checked.
//
// The poison case produces *bottom* (all modes Timeout) and so can't be a
// `run!` fixture — it's pinned in the findings/bottom-scrutinee-jun2026
// oracle corpus instead. The COMPLEMENT — the cases that must STILL produce
// a value, proving the fix doesn't over-poison — is the existing `sink_*`
// family above (`sink_one_arm` etc.: a fine scrutinee with a bottom value
// in an un-taken arm still yields the taken arm's value, all three modes).
// The bottom-GUARD distinction (`select 5 { n if v>0 => …, _ => 99 }` with
// v = 1/0 → 99, scrutinee fine + guard bottom → fall through) is verified
// in the same oracle corpus; it can't be a `run!` fixture because the guard
// is EAGER, so the JIT aborts the kernel at the `let v = 1/0` producer
// (Timeout) while interp/fused recover 99 — an expected `interp == fused !=
// jit` shape, not a value `run!` can assert across all three arms.
