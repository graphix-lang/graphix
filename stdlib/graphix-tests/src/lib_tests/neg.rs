//! Unary negation (`-x`) is a real `Neg` node: `-x` on an unsigned
//! operand is a compile error, and both engines agree on the value.

use anyhow::Result;
use graphix_package_core::{run, testing::eval};
use netidx::subscriber::Value;

const NEG_I64: &str = "{ let x = i64:5; -x }";
run!(neg_i64, NEG_I64, |v: Result<&Value>| matches!(v, Ok(Value::I64(-5)));
    graphix_package_core::testing::FuseExpect::Jit);

const NEG_F64: &str = "{ let x = f64:2.5; -x }";
run!(neg_f64, NEG_F64, |v: Result<&Value>| matches!(v, Ok(Value::F64(f)) if *f == -2.5);
    graphix_package_core::testing::FuseExpect::Jit);

// `-(a + b)` fuses as one region.
const NEG_SUM: &str = "{ let a = i64:3; let b = i64:4; -(a + b) }";
run!(neg_sum, NEG_SUM, |v: Result<&Value>| matches!(v, Ok(Value::I64(-7)));
    graphix_package_core::testing::FuseExpect::Jit);

// `-x` on an unsigned operand is a compile error.
#[tokio::test]
async fn neg_unsigned_is_compile_error() {
    let r = eval("{ let x = u64:5; -x }", crate::TEST_REGISTER).await;
    assert!(
        r.is_err(),
        "negating an unsigned value must be a compile error, got {:?}",
        r.map(|(v, _)| v)
    );
}
