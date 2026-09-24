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
    for (src, refusal) in [(BIND_IS_SCOPED, "c"), (BIND_REC_REFUSED, "cannot be rec")] {
        let msg = match eval(src, crate::TEST_REGISTER).await {
            Err(e) => format!("{e:#}"),
            Ok((v, _)) => panic!("must be refused: {src} => {v:?}"),
        };
        assert!(msg.contains(refusal), "wrong refusal for {src}: {msg}");
    }
    Ok(())
}

// A destructured trigger is the run's snapshot, like a named one: the
// body reads the value the run started with, not a busy-dropped one.
async fn destructured_snapshot(fusion_disabled: bool) -> Result<()> {
    for (trigger, body) in [("let (a, b) = t", "a + b"), ("let p = t", "p.0 + p.1")] {
        let code = format!(
            r#"{{
                let t = (0, 0);
                t <- sys::time::after_idle(duration:50.ms, (1, 1));
                seq {trigger} {{
                    sys::time::after_idle(duration:120.ms, 0);
                    {body}
                }}
            }}"#
        );
        let (values, _) = super::dense_deltas::run_delta(&code, fusion_disabled).await?;
        assert_eq!(super::dense_deltas::as_i64s(&values), [0], "{trigger}");
    }
    Ok(())
}

// The trigger's name reads the snapshot; a write or a reference reaches
// the variable itself. Each run outlasts its write, which would otherwise
// land on an idle machine and start the next run.
async fn trigger_writes_reach_the_variable(fusion_disabled: bool) -> Result<()> {
    let hold = "sys::time::after_idle(duration:20.ms, 1)";
    for body in [format!("t <- 10; {hold}; t"), format!("let q = &t; *q <- 10; {hold}")] {
        let code = format!(
            r#"{{
                let t = 0;
                let r = seq t {{ {body} }};
                select t {{ 0 => never(), n => n }}
            }}"#
        );
        let (values, _) = super::dense_deltas::run_delta(&code, fusion_disabled).await?;
        assert_eq!(super::dense_deltas::as_i64s(&values), [10], "{body}");
    }
    let code = r#"{
        let t = 0;
        seq t { t <- 10; sys::time::after_idle(duration:20.ms, 1); t + 1 }
    }"#;
    let (values, _) = super::dense_deltas::run_delta(code, fusion_disabled).await?;
    assert_eq!(super::dense_deltas::as_i64s(&values), [1], "the body reads the snapshot");
    Ok(())
}

macro_rules! modes {
    ($($test:ident),+ $(,)?) => {$ (
        mod $test {
            use super::*;

            #[tokio::test(flavor = "current_thread")]
            async fn interp() -> Result<()> { super::$test(true).await }

            #[tokio::test(flavor = "current_thread")]
            async fn jit() -> Result<()> { super::$test(false).await }
        }
    )+};
}

modes!(destructured_snapshot, trigger_writes_reach_the_variable);
