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

// Float modulo. Cranelift has no `frem`, so the JIT bails and the kernel
// runs on the interpreter (FuseExpect::Interp) — it must still produce
// the right value. Regression for a fuzzer-found bug (#175): the JIT's
// float-`%` arm emitted a runtime trap that crashed the whole runtime
// instead of bailing. Found by graphix-fuzz on `f64:7.0 % f64:3.0`.
const FLOAT_MOD: &str = "f64:7.0 % f64:3.0";
run!(float_mod, FLOAT_MOD, |v: Result<&Value>| {
    matches!(v, Ok(Value::F64(f)) if *f == 1.0)
}; graphix_package_core::testing::FuseExpect::Interp);

// f32 inexact add.
const F32_ADD_INEXACT: &str = "f32:0.1 + f32:0.2";
run!(f32_add_inexact, F32_ADD_INEXACT, |v: Result<&Value>| {
    matches!(v, Ok(Value::F32(f)) if *f == 0.1_f32 + 0.2)
});

// ── Checked-arithmetic overflow detection (regression for the
//    +?/-?/*? wrapping bug). These don't fuse (`+?` has no GIR
//    lowering), so they run the node-walk. ──

const CHECKED_ADD_OVERFLOW: &str = "is_err(i64:9223372036854775807 +? i64:1)";
run!(checked_add_overflow_errs, CHECKED_ADD_OVERFLOW, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
}; graphix_package_core::testing::FuseExpect::None);

const CHECKED_SUB_UNDERFLOW: &str = "is_err(i64:-9223372036854775808 -? i64:1)";
run!(checked_sub_underflow_errs, CHECKED_SUB_UNDERFLOW, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
}; graphix_package_core::testing::FuseExpect::None);

const CHECKED_MUL_OVERFLOW: &str = "is_err(i64:9223372036854775807 *? i64:2)";
run!(checked_mul_overflow_errs, CHECKED_MUL_OVERFLOW, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
}; graphix_package_core::testing::FuseExpect::None);

const CHECKED_U8_OVERFLOW: &str = "is_err(u8:200 +? u8:100)";
run!(checked_u8_overflow_errs, CHECKED_U8_OVERFLOW, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
}; graphix_package_core::testing::FuseExpect::None);

const CHECKED_U8_UNDERFLOW: &str = "is_err(u8:0 -? u8:1)";
run!(checked_u8_underflow_errs, CHECKED_U8_UNDERFLOW, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
}; graphix_package_core::testing::FuseExpect::None);

// Checked div-by-zero already worked before the fix — guard against
// regression.
const CHECKED_DIV_ZERO: &str = "is_err(i64:10 /? i64:0)";
run!(checked_div_zero_errs, CHECKED_DIV_ZERO, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
}; graphix_package_core::testing::FuseExpect::None);

// No overflow: the checked op returns the bare value, not an error.
const CHECKED_NO_OVERFLOW: &str = "i64:5 +? i64:3";
run!(checked_no_overflow_ok, CHECKED_NO_OVERFLOW, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(8)))
}; graphix_package_core::testing::FuseExpect::None);

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
// the GirType::Error arm.
const ERR_DYNCALL_ARG: &str = "is_err(error(1))";
run!(err_as_dyncall_arg, ERR_DYNCALL_ARG, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
});

// Cluster D: interpolating a non-scalar part (Nullable<string> from an
// array index) must bail the StringInterpolate to the node-walk, not crash
// the fused Concat (which only handles String/scalar parts).
const INTERP_NONSCALAR: &str = "{ let words = [\"alpha\", \"beta\"]; \"first=[words[0]]\" }";
run!(interp_nonscalar_part, INTERP_NONSCALAR, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if s == "first=alpha")
}; graphix_package_core::testing::FuseExpect::None);
