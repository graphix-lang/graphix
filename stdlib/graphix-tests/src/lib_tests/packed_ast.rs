//! Part D3 — the resolver's packed-AST path. `eval_packed` ships the test
//! module as a `serialize::pack_module` blob through `ModuleResolver::VFS`, so
//! `resolve` takes its `unpack_module` branch instead of parsing; the result
//! must equal `eval` (which parses the same source). This exercises the whole
//! splice end to end: `VfsEntry.packed` → `resolve` → `serialize::unpack_module`
//! → compile → run.

use graphix_package_core::testing::{eval, eval_packed};

async fn assert_packed_matches(code: &str) {
    let parsed = eval(code, crate::TEST_REGISTER).await.expect("parse-load").0;
    let packed = eval_packed(code, crate::TEST_REGISTER).await.expect("packed-load").0;
    assert_eq!(parsed, packed, "packed-load != parse-load for `{code}`");
}

#[tokio::test]
async fn packed_load_matches_parsed() {
    // Pure arithmetic, a block with a binding, a select, and a stdlib HOF call
    // (the packed test module importing a normally-parsed stdlib module).
    assert_packed_matches("i64:1 + i64:2").await;
    assert_packed_matches("{ let x = i64:3; x * x + i64:1 }").await;
    assert_packed_matches("select i64:1 { 1 => \"one\", _ => \"other\" }").await;
    assert_packed_matches("array::map([1, 2, 3], |x| x + 1)").await;
}
