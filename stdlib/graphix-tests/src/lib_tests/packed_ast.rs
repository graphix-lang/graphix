//! The resolver's packed-AST path: `eval_packed` ships the test module
//! as a `serialize::pack_module` blob through `ModuleResolver::VFS`, and
//! the result must equal `eval` over the same source.

use graphix_package_core::testing::{eval, eval_packed};

async fn assert_packed_matches(code: &str) {
    let parsed = eval(code, crate::TEST_REGISTER).await.expect("parse-load").0;
    let packed = eval_packed(code, crate::TEST_REGISTER).await.expect("packed-load").0;
    assert_eq!(parsed, packed, "packed-load != parse-load for `{code}`");
}

#[tokio::test]
async fn packed_load_matches_parsed() {
    assert_packed_matches("i64:1 + i64:2").await;
    assert_packed_matches("{ let x = i64:3; x * x + i64:1 }").await;
    assert_packed_matches("select i64:1 { 1 => \"one\", _ => \"other\" }").await;
    assert_packed_matches("array::map([1, 2, 3], |x| x + 1)").await;
}
