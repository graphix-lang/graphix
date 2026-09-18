// `seq let pat = e { .. }`: the trigger's value named for the body.

use anyhow::Result;
use graphix_package_core::testing::eval;
use netidx::publisher::Value;

const BIND_NAME: &str = r#"{
  let go = 1;
  seq let c = go + 41 { c }
}"#;

const BIND_PATTERN: &str = r#"{
  let pt = { x: 40, y: 2 };
  seq let {x, y} = pt { x + y }
}"#;

const BIND_TUPLE_FROM_CALL: &str = r#"{
  let f = |x| (x, x + 1);
  seq let (a, b) = f(20) { a + b }
}"#;

const BIND_TYPED: &str = r#"{
  let go = 1;
  seq let c: i64 = go + 41 { c }
}"#;

// The name is the body's: the outer scope does not see it.
const BIND_IS_SCOPED: &str = r#"{
  let go = 1;
  let r = seq let c = go { c };
  c
}"#;

// Refused by the parser: a trigger has no self to recurse on.
const BIND_REC_REFUSED: &str = r#"{
  let go = 1;
  seq let rec c = go { c }
}"#;

#[tokio::test(flavor = "current_thread")]
async fn seq_let_binds_the_trigger() -> Result<()> {
    for (src, expected) in [
        (BIND_NAME, 42),
        (BIND_PATTERN, 42),
        (BIND_TUPLE_FROM_CALL, 41),
        (BIND_TYPED, 42),
    ] {
        let (v, ctx) = eval(src, crate::TEST_REGISTER).await?;
        assert_eq!(v, Value::I64(expected), "{src}");
        ctx.shutdown().await;
    }
    for (src, refusal) in [(BIND_IS_SCOPED, "c"), (BIND_REC_REFUSED, "parse error")] {
        let msg = match eval(src, crate::TEST_REGISTER).await {
            Err(e) => format!("{e:#}"),
            Ok((v, _)) => panic!("must be refused: {src} => {v:?}"),
        };
        assert!(msg.contains(refusal), "wrong refusal for {src}: {msg}");
    }
    Ok(())
}
