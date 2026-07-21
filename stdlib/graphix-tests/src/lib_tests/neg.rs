//! Unary negation (`-x`). A real `Neg` node, not a `0 - x` desugar: the
//! signed/float/decimal type constraint makes `-x` on an unsigned operand
//! a COMPILE error (rather than a silent runtime underflow), and the
//! node-walk (`wrapping_neg` / `-`) agrees with the JIT (`ineg` / `fneg`)
//! — `run!` asserts both modes produce the same value, and that the pure
//! arithmetic region fuses.

use anyhow::Result;
use graphix_package_core::{run, testing::eval};
use netidx::subscriber::Value;

// `-x` on a signed integer fuses (ineg).
const NEG_I64: &str = "{ let x = i64:5; -x }";
run!(neg_i64, NEG_I64, |v: Result<&Value>| matches!(v, Ok(Value::I64(-5)));
    graphix_package_core::testing::FuseExpect::Jit);

// `-x` on a float fuses (fneg).
const NEG_F64: &str = "{ let x = f64:2.5; -x }";
run!(neg_f64, NEG_F64, |v: Result<&Value>| matches!(v, Ok(Value::F64(f)) if *f == -2.5);
    graphix_package_core::testing::FuseExpect::Jit);

// A parenthesized arithmetic operand: `-(a + b)` fuses as one region and
// must bind tighter than nothing here — it's the whole expression.
const NEG_SUM: &str = "{ let a = i64:3; let b = i64:4; -(a + b) }";
run!(neg_sum, NEG_SUM, |v: Result<&Value>| matches!(v, Ok(Value::I64(-7)));
    graphix_package_core::testing::FuseExpect::Jit);

// `-x` on an UNSIGNED operand is a compile error — the entire reason `Neg`
// is a real node with a signed constraint instead of a `0 - x` desugar
// that would silently underflow to bottom at runtime.
#[tokio::test]
async fn neg_unsigned_is_compile_error() {
    let r = eval("{ let x = u64:5; -x }", crate::TEST_REGISTER).await;
    assert!(
        r.is_err(),
        "negating an unsigned value must be a compile error, got {:?}",
        r.map(|(v, _)| v)
    );
}
