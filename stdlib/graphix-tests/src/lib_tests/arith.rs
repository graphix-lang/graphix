// Primitive arithmetic operator edge cases: IEEE float edges, the
// total-order float comparison, checked-arithmetic overflow, and bottom
// locality in fused blocks. `run!` makes them a cross-mode
// determinism guard.

use anyhow::Result;
use graphix_package_core::run;
use netidx::subscriber::Value;

// Inexact: 0.1 + 0.2 is not 0.3.
const FLOAT_ADD_INEXACT: &str = "f64:0.1 + f64:0.2";
run!(float_add_inexact, FLOAT_ADD_INEXACT, |v: Result<&Value>| {
    matches!(v, Ok(Value::F64(f)) if *f == 0.1 + 0.2)
});

// No fused-multiply-add contraction: a*b+c rounds twice.
const FLOAT_FMA_NO_CONTRACT: &str = "f64:0.1 * f64:0.2 + f64:0.3";
run!(float_fma_no_contract, FLOAT_FMA_NO_CONTRACT, |v: Result<&Value>| {
    matches!(v, Ok(Value::F64(f)) if *f == 0.1_f64 * 0.2 + 0.3)
});

// Float division by zero is IEEE inf, not an error.
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

// Float comparison is a total order (NaN == NaN, NaN below every
// non-NaN), not IEEE.

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

// Float modulo has no cranelift lowering, so the program node-walks;
// the value must still be right.
const FLOAT_MOD: &str = "f64:7.0 % f64:3.0";
run!(float_mod, FLOAT_MOD, |v: Result<&Value>| {
    matches!(v, Ok(Value::F64(f)) if *f == 1.0)
}; graphix_package_core::testing::FuseExpect::None);

// f32 inexact add.
const F32_ADD_INEXACT: &str = "f32:0.1 + f32:0.2";
run!(f32_add_inexact, F32_ADD_INEXACT, |v: Result<&Value>| {
    matches!(v, Ok(Value::F32(f)) if *f == 0.1_f32 + 0.2)
});

// Checked-arithmetic overflow detection; these node-walk.

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

const CHECKED_DIV_ZERO: &str = "is_err(i64:10 /? i64:0)";
run!(checked_div_zero_errs, CHECKED_DIV_ZERO, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
}; graphix_package_core::testing::FuseExpect::Jit);

// No overflow: the checked op returns the bare value, not an error.
const CHECKED_NO_OVERFLOW: &str = "i64:5 +? i64:3";
run!(checked_no_overflow_ok, CHECKED_NO_OVERFLOW, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(8)))
}; graphix_package_core::testing::FuseExpect::Jit);

// Unchecked overflow wraps.
const UNCHECKED_OVERFLOW_WRAPS: &str = "i64:9223372036854775807 + i64:1";
run!(unchecked_overflow_wraps, UNCHECKED_OVERFLOW_WRAPS, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(i)) if *i == i64::MIN)
});

// Integer div/rem by zero and MIN/-1 bottom without crashing; a valid
// division still fuses.
const DIV_VALID: &str = "i64:10 / i64:2";
run!(div_valid, DIV_VALID, |v: Result<&Value>| matches!(v, Ok(Value::I64(5))));

const MOD_VALID: &str = "i64:10 % i64:3";
run!(mod_valid, MOD_VALID, |v: Result<&Value>| matches!(v, Ok(Value::I64(1))));

// A div-by-zero in an un-taken select arm still produces a value.
const DIV_IN_ARM: &str = "select i64:1 { 1 => i64:42, n => n / i64:0 }";
run!(div_in_untaken_arm, DIV_IN_ARM, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(42)))
});

// An `error()` value passed to a fast fn marshals as a value-shape arg.
const ERR_DYNCALL_ARG: &str = "is_err(error(1))";
run!(err_as_dyncall_arg, ERR_DYNCALL_ARG, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
}; graphix_package_core::testing::FuseExpect::Jit);

// Interpolating a non-scalar part (`Nullable<string>` from an index)
// node-walks the interpolation instead of crashing.
const INTERP_NONSCALAR: &str =
    "{ let words = [\"alpha\", \"beta\"]; \"first=[words[0]]\" }";
run!(interp_nonscalar_part, INTERP_NONSCALAR, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if s == "first=alpha")
}; graphix_package_core::testing::FuseExpect::Jit);

// A bottom in a dead non-tail statement does not poison the tail.

// A dead bottom `let` is dropped; the tail is produced.
const DEAD_LET_BOTTOM: &str = "{ let v = i64:1 / i64:0; i64:42 }";
run!(dead_let_bottom, DEAD_LET_BOTTOM, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(42)))
});

// A bare (value-discarded) bottom statement is dropped.
const BARE_BOTTOM_STMT: &str = "{ i64:1 / i64:0; i64:42 }";
run!(bare_bottom_stmt, BARE_BOTTOM_STMT, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(42)))
});

// `v` is referenced only by a bare statement that is itself dropped.
const DEAD_LET_FIXPOINT: &str = "{ let v = i64:1 / i64:0; v; i64:42 }";
run!(dead_let_fixpoint, DEAD_LET_FIXPOINT, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(42)))
});

// A live binding survives pruning; only the dead bottom `b` is dropped.
const DEAD_LET_KEEPS_LIVE: &str = "{ let a = i64:7; let b = i64:1 / i64:0; a + i64:1 }";
run!(dead_let_keeps_live, DEAD_LET_KEEPS_LIVE, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(8)))
});

// A bottom feeding only an un-taken select arm yields the taken arm's
// value.

// v feeds only the un-taken arm: 99.
const SINK_ONE_ARM: &str =
    "{ let v = i64:1 / i64:0; select i64:5 { 2 => v, _ => i64:99 } }";
run!(sink_one_arm, SINK_ONE_ARM, |v: Result<&Value>| matches!(v, Ok(Value::I64(99))));

// mod-by-zero, same mechanism.
const SINK_MOD: &str = "{ let v = i64:1 % i64:0; select i64:5 { 2 => v, _ => i64:99 } }";
run!(sink_mod, SINK_MOD, |v: Result<&Value>| matches!(v, Ok(Value::I64(99))));

// v feeds two un-taken arms.
const SINK_MULTI_ARM: &str =
    "{ let v = i64:1 / i64:0; select i64:5 { 1 => v, 2 => v, _ => i64:99 } }";
run!(sink_multi_arm, SINK_MULTI_ARM, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(99)))
});

// A dependency chain v -> w where w feeds only an un-taken arm.
const SINK_CHAIN: &str =
    "{ let v = i64:1 / i64:0; let w = v + i64:1; select i64:5 { 2 => w, _ => i64:99 } }";
run!(sink_chain, SINK_CHAIN, |v: Result<&Value>| matches!(v, Ok(Value::I64(99))));

// v is referenced only inside a nested select in the un-taken arm.
const SINK_NESTED: &str = "{ let v = i64:1 / i64:0; \
    select i64:5 { 2 => select i64:1 { 1 => v, _ => i64:0 }, _ => i64:99 } }";
run!(sink_nested, SINK_NESTED, |v: Result<&Value>| matches!(v, Ok(Value::I64(99))));

// The scrutinee picks the literal arm that does not use v: 99, even
// though the un-taken catch-all references v.
const SINK_LITERAL_ARM_TAKEN: &str =
    "{ let v = i64:1 / i64:0; select i64:7 { 7 => i64:42, _ => v } }";
run!(sink_literal_arm_taken, SINK_LITERAL_ARM_TAKEN, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(42)))
});

// A block-with-select as an arithmetic operand.
const SINK_EXPR_OPERAND: &str =
    "({ let v = i64:1 / i64:0; select i64:5 { 2 => v, _ => i64:99 } }) + i64:1";
run!(sink_expr_operand, SINK_EXPR_OPERAND, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(100)))
});

// A bottom-capable let that also contains a pure call.
const SINK_CALL_BEARING: &str =
    "{ let v = i64:1 / i64:0 + str::len(\"x\"); select i64:5 { 2 => v, _ => i64:99 } }";
run!(sink_call_bearing, SINK_CALL_BEARING, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(99)))
});

// A `?`-on-parse-error bottom feeding an un-taken arm: 99.
const SINK_QOP_PARSE: &str =
    "{ let v: i64 = str::parse(\"notanumber\")?; select i64:5 { 2 => v, _ => i64:99 } }";
run!(sink_qop_parse, SINK_QOP_PARSE, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(99)))
});

// A dependency chain through a multi-arm let: v feeds two un-taken
// arms, w (bottom) feeds v.
const SINK_MULTI_ARM_CHAIN: &str = "{ let x = str::len(\"hello\"); let w = x / i64:0; \
    let v = w + i64:1; select x { 5 => i64:99, 6 => v, _ => v } }";
run!(sink_multi_arm_chain, SINK_MULTI_ARM_CHAIN, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(99)))
});

// A bottom whose only consumer is an arm of a nested select inside an
// outer un-taken arm: 50.
const SINK_NESTED_STMT: &str = "{ let s = i64:7; let t = i64:2; let v = i64:10 / i64:0; \
    select s { 7 => select t { 3 => v + i64:1, _ => i64:50 }, _ => i64:99 } }";
run!(sink_nested_stmt, SINK_NESTED_STMT, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(50)))
});

// An impure non-bottom let (a bare `rand`) evaluates eagerly; all modes
// agree at 99.
const SINK_RAND_STAYS_EAGER: &str = "{ let v = rand::rand(#start: 0, #end: 9, #clock: 1); select i64:5 { 2 => v, _ => i64:99 } }";
run!(sink_rand_stays_eager, SINK_RAND_STAYS_EAGER, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(99)))
}; graphix_package_core::testing::FuseExpect::Jit);

// A bottom scrutinee bottoms the whole select (pinned in
// findings/bottom-scrutinee-jun2026, since `run!` cannot assert bottom);
// the `sink_*` family above is the complement that must still produce.
