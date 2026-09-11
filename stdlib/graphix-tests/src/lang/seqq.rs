use super::dense_deltas::{as_i64s, run_delta};
use anyhow::Result;
use arcstr::ArcStr;
use graphix_compiler::CFlag;
use graphix_package_core::testing::init_with_flags_and_setup;
use netidx_value::{ValArray, Value};
use tokio::sync::mpsc;

const BURST: &str = r#"
    let step = 0;
    step <- select step { n if n < 12 => n + 1, _ => never() };
    let request = select step { 1 | 2 | 3 => step, _ => never() };
"#;

async fn burst_captures(fusion_disabled: bool) -> Result<()> {
    let code = format!(
        r#"{{
        {BURST}
        let x = step * 10;
        seqq request {{ sys::time::after_idle(duration:20.ms, (request, x)) }}
    }}"#
    );
    let (values, _) = run_delta(&code, fusion_disabled).await?;
    let expected: Vec<_> = (1..=3)
        .map(|n| Value::Array(ValArray::from([Value::I64(n), Value::I64(n * 10)])))
        .collect();
    assert_eq!(values, expected);
    Ok(())
}

async fn repeated_outputs(fusion_disabled: bool) -> Result<()> {
    let code = format!(
        r#"{{
        {BURST}
        let same = request ~ 7;
        seqq same {{ sys::time::after_idle(duration:10.ms, same) }}
    }}"#
    );
    let (values, _) = run_delta(&code, fusion_disabled).await?;
    assert_eq!(as_i64s(&values), [7, 7, 7]);
    Ok(())
}

async fn delayed_capture(fusion_disabled: bool) -> Result<()> {
    let code = format!(
        r#"{{
        {BURST}
        let late = sys::time::after_idle(duration:30.ms, 22);
        seqq request {{ late }}
    }}"#
    );
    let (values, _) = run_delta(&code, fusion_disabled).await?;
    assert_eq!(as_i64s(&values), [22, 22, 22]);
    Ok(())
}

async fn stale_capture(fusion_disabled: bool) -> Result<()> {
    let code = format!(
        r#"{{
        {BURST}
        let x = select step {{ 0 => 10, 2 => 20, _ => never() }};
        seqq request {{ x }}
    }}"#
    );
    let (values, _) = run_delta(&code, fusion_disabled).await?;
    assert_eq!(as_i64s(&values), [10, 20, 20]);
    Ok(())
}

async fn live_until(fusion_disabled: bool) -> Result<()> {
    let code = r#"{
        let ready = false;
        ready <- sys::time::after_idle(duration:30.ms, true);
        seqq { until ready; 42 }
    }"#;
    let (values, _) = run_delta(code, fusion_disabled).await?;
    assert_eq!(as_i64s(&values), [42]);
    Ok(())
}

async fn live_writes(fusion_disabled: bool) -> Result<()> {
    for body in ["n <- n + 1; n", "let target = &n; *target <- *target + 1; *target"] {
        let code = format!(r#"{{ {BURST} let n = 0; seqq request {{ {body} }} }}"#);
        let (values, _) = run_delta(&code, fusion_disabled).await?;
        assert_eq!(as_i64s(&values), [1, 2, 3], "{body}");
    }
    Ok(())
}

async fn capture_scopes(fusion_disabled: bool) -> Result<()> {
    let code = format!(
        r#"{{
        {BURST}
        let x = step * 10;
        seqq request {{
            let y = {{ let x = request; x + 1 }};
            let values = array::map([y], |x| x + 1);
            (request, x, values[0]?)
        }}
    }}"#
    );
    let (values, _) = run_delta(&code, fusion_disabled).await?;
    let expected: Vec<_> = (1..=3)
        .map(|n| {
            Value::Array(ValArray::from([
                Value::I64(n),
                Value::I64(n * 10),
                Value::I64(n + 2),
            ]))
        })
        .collect();
    assert_eq!(values, expected);
    Ok(())
}

async fn abort_releases(fusion_disabled: bool) -> Result<()> {
    let code = format!(
        r#"{{
        {BURST}
        let f = |n: i64| select n {{ 2 => error(`Oops)?, n => n }};
        catch(e) println(e);
        seqq request {{ f(request) }}
    }}"#
    );
    let (values, out) = run_delta(&code, fusion_disabled).await?;
    assert_eq!(as_i64s(&values), [1, 3]);
    assert_eq!(out.lines().count(), 1);
    Ok(())
}

async fn block_lets_are_local(fusion_disabled: bool) -> Result<()> {
    let code = format!(
        r#"{{
        {BURST}
        let x = 100;
        seqq request {{
            {{ let x = request; x }};
            x
        }}
    }}"#
    );
    let (values, _) = run_delta(&code, fusion_disabled).await?;
    assert_eq!(as_i64s(&values), [100, 100, 100]);
    Ok(())
}

async fn sleep_restarts(fusion_disabled: bool) -> Result<()> {
    for (trigger, expr) in [
        ("", "tick"),
        ("tick", "tick"),
        ("", "sys::time::after_idle(duration:20.ms, tick)"),
    ] {
        let code = format!(
            r#"{{
        let tick = count(sys::time::timer(duration:80.ms, 3)?);
        let issued = never<i64>();
        select tick {{
            1 | 3 => seqq {trigger} {{ let value = {expr}; issued <- value; value }},
            _ => never()
        }};
        issued
    }}"#
        );
        let (values, _) = run_delta(&code, fusion_disabled).await?;
        assert_eq!(as_i64s(&values), [1, 3], "seqq {trigger}: {expr}");
    }
    Ok(())
}

async fn reference_inputs(fusion_disabled: bool) -> Result<()> {
    let code = format!(
        r#"{{
        {BURST}
        let a = 0;
        let b = 0;
        let target = select step {{ 1 => &a, _ => &b }};
        let done = seqq request {{ *target <- request; request }};
        done ~ (a, b)
    }}"#
    );
    let (values, _) = run_delta(&code, fusion_disabled).await?;
    assert_eq!(
        values,
        [
            Value::Array(ValArray::from([Value::I64(1), Value::I64(0)])),
            Value::Array(ValArray::from([Value::I64(1), Value::I64(2)])),
            Value::Array(ValArray::from([Value::I64(1), Value::I64(3)])),
        ]
    );
    Ok(())
}

async fn function_captures_stay_static(fusion_disabled: bool) -> Result<()> {
    let code = format!(
        r#"{{
        {BURST}
        let k = 5;
        let h = |x: i64| -> i64 x * 2 + k;
        seqq request {{ let a = #[native] h(request + k); a }}
    }}"#
    );
    let (values, _) = run_delta(&code, fusion_disabled).await?;
    assert_eq!(as_i64s(&values), [17, 19, 21]);
    Ok(())
}

async fn unhandled_warnings(fusion_disabled: bool) -> Result<()> {
    let (tx, _rx) = mpsc::channel(100);
    let mut flags = CFlag::WarnUnhandled | CFlag::WarningsAreErrors;
    if fusion_disabled {
        flags.insert(CFlag::FusionDisabled);
    }
    let ctx = init_with_flags_and_setup(tx, &crate::TEST_REGISTER, vec![], flags, |_| {})
        .await?;
    for code in [
        "seq { 42 }",
        "{ let f = || seq { 42 }; f() }",
        "seqq { 42 }",
        "{ let f = || seqq { 42 }; f() }",
        "seqq { sys::time::after_idle(duration:1.ms, 42) }",
        "{ catch(e) null; let v: [i64, Error<`Expected>] = 42; seqq { v? } }",
    ] {
        ctx.rt.compile(ArcStr::from(code)).await?;
    }
    for code in [
        "{ let v: [i64, Error<`Expected>] = 42; seqq { v? } }",
        "{ let f = |n| select n { 0 => error(`Expected)?, n => n }; seqq { f(0) } }",
    ] {
        let error = ctx.rt.compile(ArcStr::from(code)).await.unwrap_err();
        assert!(format!("{error:#}").contains("will not be caught"), "{error:#}");
    }
    ctx.shutdown().await;
    Ok(())
}

macro_rules! modes {
    ($test:ident, $interp:ident, $jit:ident) => {
        #[tokio::test(flavor = "current_thread")]
        async fn $interp() -> Result<()> {
            $test(true).await
        }
        #[tokio::test(flavor = "current_thread")]
        async fn $jit() -> Result<()> {
            $test(false).await
        }
    };
}

modes!(burst_captures, burst_captures_interp, burst_captures_jit);
modes!(repeated_outputs, repeated_outputs_interp, repeated_outputs_jit);
modes!(delayed_capture, delayed_capture_interp, delayed_capture_jit);
modes!(stale_capture, stale_capture_interp, stale_capture_jit);
modes!(live_until, live_until_interp, live_until_jit);
modes!(live_writes, live_writes_interp, live_writes_jit);
modes!(capture_scopes, capture_scopes_interp, capture_scopes_jit);
modes!(abort_releases, abort_releases_interp, abort_releases_jit);
modes!(block_lets_are_local, block_lets_are_local_interp, block_lets_are_local_jit);
modes!(sleep_restarts, sleep_restarts_interp, sleep_restarts_jit);
modes!(reference_inputs, reference_inputs_interp, reference_inputs_jit);
modes!(unhandled_warnings, unhandled_warnings_interp, unhandled_warnings_jit);
modes!(
    function_captures_stay_static,
    function_captures_stay_static_interp,
    function_captures_stay_static_jit
);
