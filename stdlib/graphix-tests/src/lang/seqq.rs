use super::dense_deltas::{as_i64s, run_delta};
use anyhow::Result;
use arcstr::ArcStr;
use graphix_compiler::CFlag;
use graphix_package_core::testing::Mode;
use graphix_package_core::testing::init_with_flags_and_setup;
use netidx_value::{ValArray, Value};
use tokio::sync::mpsc;

const BURST: &str = r#"
    let step = 0;
    step <- select step { n if n < 12 => n + 1, _ => never() };
    let request = select step { 1 | 2 | 3 => step, _ => never() };
"#;

async fn burst_captures(mode: Mode) -> Result<()> {
    let code = format!(
        r#"{{
        {BURST}
        let x = step * 10;
        seqq request {{ sys::time::after_idle(duration:20.ms, (request, x)) }}
    }}"#
    );
    let (values, _) = run_delta(&code, mode).await?;
    let expected: Vec<_> = (1..=3)
        .map(|n| Value::Array(ValArray::from([Value::I64(n), Value::I64(n * 10)])))
        .collect();
    assert_eq!(values, expected);
    Ok(())
}

async fn repeated_outputs(mode: Mode) -> Result<()> {
    let code = format!(
        r#"{{
        {BURST}
        let same = request ~ 7;
        seqq same {{ sys::time::after_idle(duration:10.ms, same) }}
    }}"#
    );
    let (values, _) = run_delta(&code, mode).await?;
    assert_eq!(as_i64s(&values), [7, 7, 7]);
    Ok(())
}

async fn delayed_capture(mode: Mode) -> Result<()> {
    let code = format!(
        r#"{{
        {BURST}
        let late = sys::time::after_idle(duration:30.ms, 22);
        seqq request {{ late }}
    }}"#
    );
    let (values, _) = run_delta(&code, mode).await?;
    assert_eq!(as_i64s(&values), [22, 22, 22]);
    Ok(())
}

async fn stale_capture(mode: Mode) -> Result<()> {
    let code = format!(
        r#"{{
        {BURST}
        let x = select step {{ 0 => 10, 2 => 20, _ => never() }};
        seqq request {{ x }}
    }}"#
    );
    let (values, _) = run_delta(&code, mode).await?;
    assert_eq!(as_i64s(&values), [10, 20, 20]);
    Ok(())
}

async fn call_trigger_debounces(mode: Mode) -> Result<()> {
    let code = format!(
        r#"{{
        {BURST}
        seqq sys::time::after_idle(duration:20.ms, request) {{ request }}
    }}"#
    );
    let (values, _) = run_delta(&code, mode).await?;
    assert_eq!(as_i64s(&values), [3]);
    Ok(())
}

// A bound trigger under seqq is the value that queued the run.
async fn let_binds_each_request(mode: Mode) -> Result<()> {
    let code = format!(
        r#"{{
        {BURST}
        seqq let v = request * 10 {{ sys::time::after_idle(duration:20.ms, v) }}
    }}"#
    );
    let (values, _) = run_delta(&code, mode).await?;
    assert_eq!(as_i64s(&values), [10, 20, 30]);
    Ok(())
}

async fn live_until(mode: Mode) -> Result<()> {
    let code = r#"{
        let ready = false;
        ready <- sys::time::after_idle(duration:30.ms, true);
        seqq { until ready; 42 }
    }"#;
    let (values, _) = run_delta(code, mode).await?;
    assert_eq!(as_i64s(&values), [42]);
    Ok(())
}

// An `until` reads everything live but the trigger's name, which is the
// request this run serves, as it is under `seq`.
async fn until_waits_on_its_own_request(mode: Mode) -> Result<()> {
    for kw in ["seq", "seqq"] {
        let code = format!(
            r#"{{
            let step = 0;
            step <- select step {{ n if n < 60 => n + 1, _ => never() }};
            let request = select step {{ 1 | 30 => step, _ => never() }};
            let done = {kw} request {{ until step > request + 10; request }};
            done ~ step - done
        }}"#
        );
        let (values, _) = run_delta(&code, mode).await?;
        let waits = as_i64s(&values);
        assert_eq!(waits.len(), 2, "{kw}: {waits:?}");
        assert!(waits.iter().all(|w| (11..16).contains(w)), "{kw}: {waits:?}");
    }
    Ok(())
}

async fn live_writes(mode: Mode) -> Result<()> {
    for body in ["n <- n + 1; n", "let target = &mut n; *target <- *target + 1; *target"]
    {
        let code = format!(r#"{{ {BURST} let n = 0; seqq request {{ {body} }} }}"#);
        let (values, _) = run_delta(&code, mode).await?;
        assert_eq!(as_i64s(&values), [1, 2, 3], "{body}");
    }
    Ok(())
}

async fn capture_scopes(mode: Mode) -> Result<()> {
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
    let (values, _) = run_delta(&code, mode).await?;
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

async fn abort_releases(mode: Mode) -> Result<()> {
    let code = format!(
        r#"{{
        {BURST}
        let f = |n: i64| select n {{ 2 => error(`Oops)?, n => n }};
        catch(e) println(e);
        seqq request {{ f(request) }}
    }}"#
    );
    let (values, out) = run_delta(&code, mode).await?;
    assert_eq!(as_i64s(&values), [1, 3]);
    assert_eq!(out.lines().count(), 1);
    Ok(())
}

async fn block_lets_are_local(mode: Mode) -> Result<()> {
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
    let (values, _) = run_delta(&code, mode).await?;
    assert_eq!(as_i64s(&values), [100, 100, 100]);
    Ok(())
}

async fn sleep_restarts(mode: Mode) -> Result<()> {
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
        let (values, _) = run_delta(&code, mode).await?;
        assert_eq!(as_i64s(&values), [1, 3], "seqq {trigger}: {expr}");
    }
    Ok(())
}

async fn reference_inputs(mode: Mode) -> Result<()> {
    let code = format!(
        r#"{{
        {BURST}
        let a = 0;
        let b = 0;
        let target = select step {{ 1 => &mut a, _ => &mut b }};
        let done = seqq request {{ *target <- request; request }};
        done ~ (a, b)
    }}"#
    );
    let (values, _) = run_delta(&code, mode).await?;
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

async fn function_captures_stay_static(mode: Mode) -> Result<()> {
    let code = format!(
        r#"{{
        {BURST}
        let k = 5;
        let h = |x: i64| -> i64 x * 2 + k;
        seqq request {{ let a = #[native] h(request + k); a }}
    }}"#
    );
    let (values, _) = run_delta(&code, mode).await?;
    assert_eq!(as_i64s(&values), [17, 19, 21]);
    Ok(())
}

async fn unhandled_warnings(mode: Mode) -> Result<()> {
    let (tx, _rx) = mpsc::channel(100);
    let mut flags = CFlag::WarnUnhandled | CFlag::WarningsAreErrors;
    if mode.node_walk() {
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

modes!(burst_captures);
modes!(repeated_outputs);
modes!(delayed_capture);
modes!(stale_capture);
modes!(call_trigger_debounces);
modes!(let_binds_each_request);
modes!(live_until);
modes!(until_waits_on_its_own_request);
modes!(live_writes);
modes!(capture_scopes);
modes!(abort_releases);
modes!(block_lets_are_local);
modes!(sleep_restarts);
modes!(reference_inputs);
modes!(unhandled_warnings);
modes!(function_captures_stay_static);
