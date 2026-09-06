use super::dense_deltas::{as_i64s, run_delta};
use anyhow::Result;
use netidx_value::{ValArray, Value};

async fn carried_arguments(fusion_disabled: bool) -> Result<()> {
    for form in ["seq", "seqq"] {
        for body in [
            "let x = request; let f = |v| v ~ x; f(request)",
            "let x = request; let f = |v| v ~ x; let y = f(request); y",
            "let x = request; let f = |v| v ~ x; do { let y = f(request); y }",
            "do { let x = request; let f = |v| v ~ x; f(request) }",
            "let x = request; let f = |v| v ~ x; f(f(request))",
            "let x = request; let f = |v| v ~ x; f(request) + 0",
            "let x = request; let f = |v| v ~ x; select request { r => f(r) }",
            "let x = request; let f = |#value, v| v ~ value; f(#value: x, request)",
            "let x = request; let f = |#extra = 0, v| v ~ (v + extra); f(request)",
            "let x = request; let f = 'a: Number |v: 'a| v ~ v; f(request)",
            "let x = request; let f = |v| v ~ x; f(array::iter([request]))",
            "let x = request; array::fold([request], 0, |a, v| a + v)",
            "let x = request; array::fold([{v: request}], 0, |a, r| a + r.v)",
            "let x = request; let invoke = |f: fn(v: i64) -> i64| f ~ f(x); \
             invoke(|v| v + 0)",
            "let x = request; let s = Display::fmt(request); request",
            "let x = request; let f = select request { \
             1 => |v| v ~ v, _ => |v| v ~ v }; f(request)",
        ] {
            let code = format!(
                r#"{{
                    let step = 0;
                    step <- select step {{ n if n < 90 => n + 1, _ => never() }};
                    let request = select step {{ 1 => 1, 30 => 2, 60 => 3, _ => never() }};
                    {form} request {{ {body} }}
                }}"#
            );
            let (values, _) = run_delta(&code, fusion_disabled).await?;
            assert_eq!(as_i64s(&values), [1, 2, 3], "{form}: {body}");
        }
    }
    Ok(())
}

async fn inputs_ready_together(fusion_disabled: bool) -> Result<()> {
    let code = r#"{
        let step = 0;
        step <- select step { n if n < 30 => n + 1, _ => never() };
        let request = select step { 1 => step, _ => never() };
        let a = select step { n if n < 5 => 10, _ => 20 };
        let b = select step { n if n < 10 => never(), _ => 30 };
        let calls = 0;
        let f = |a, b| {
            calls <- a ~ calls + 1;
            sys::time::after_idle(duration:10.ms, a ~ (a + b))
        };
        let result = seq request { f(a, b) };
        result ~ (result, calls)
    }"#;
    let (values, _) = run_delta(code, fusion_disabled).await?;
    assert_eq!(values, [Value::Array(ValArray::from([Value::I64(50), Value::I64(1)]))]);
    Ok(())
}

async fn bottom_at_reentry(fusion_disabled: bool) -> Result<()> {
    let code = r#"{
        let step = 0;
        step <- select step { n if n < 50 => n + 1, _ => never() };
        let request = select step { 1 | 15 | 35 => step, _ => never() };
        let input = select step {
            n if n < 10 => 1,
            n if n < 22 => never(),
            n if n < 30 => 2,
            n if n < 42 => never(),
            _ => 3
        };
        let f = |v| v ~ v;
        seq request { f(input) }
    }"#;
    let (values, _) = run_delta(code, fusion_disabled).await?;
    assert_eq!(as_i64s(&values), [1, 2, 3]);
    Ok(())
}

async fn until_stays_live(fusion_disabled: bool) -> Result<()> {
    let code = r#"{
        let step = 0;
        step <- select step { n if n < 20 => n + 1, _ => never() };
        let ready = |n| n >= 10;
        seq { until ready(step); step }
    }"#;
    let (values, _) = run_delta(code, fusion_disabled).await?;
    assert_eq!(as_i64s(&values), [11]);
    Ok(())
}

async fn pending_call_keeps_inputs(fusion_disabled: bool) -> Result<()> {
    let code = r#"{
        let step = 0;
        step <- select step { n if n < 30 => n + 1, _ => never() };
        let request = select step { 1 => step, _ => never() };
        let input = select step { n if n < 5 => 7, _ => never() };
        let f = |v| sys::time::after_idle(duration:10.ms, v);
        seq request { f(input) }
    }"#;
    let (values, _) = run_delta(code, fusion_disabled).await?;
    assert_eq!(as_i64s(&values), [7]);
    Ok(())
}

async fn native_call(fusion_disabled: bool) -> Result<()> {
    let code = r#"{
        let step = 0;
        step <- select step { n if n < 90 => n + 1, _ => never() };
        let request = select step { 1 => 1, 30 => 2, 60 => 3, _ => never() };
        let f = |v| v + 0;
        seq request { let x = request; #[native] f(x) }
    }"#;
    let (values, _) = run_delta(code, fusion_disabled).await?;
    assert_eq!(as_i64s(&values), [1, 2, 3]);
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

modes!(
    carried_arguments,
    inputs_ready_together,
    bottom_at_reentry,
    until_stays_live,
    pending_call_keeps_inputs,
    native_call,
);
