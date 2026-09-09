//! `try … with`: an error-triggered branch inside a seq. The outer
//! `catch` around each program is what an aborted run's rethrow reaches.

use super::dense_deltas::{as_i64s, run_delta};
use anyhow::Result;
use graphix_package_core::testing::eval;
use netidx_value::Value;

const CLOCK: &str = r#"
    let step = 0;
    step <- select step { n if n < 60 => n + 1, _ => never() };
"#;

/// Requests 1, 2 and 3, spaced so a `seq` run completes between them.
const SPACED: &str =
    "let request = select step { 1 => 1, 20 => 2, 40 => 3, _ => never() };";
/// Requests 1, 2 and 3 on consecutive cycles: `seqq` queues them.
const BURST: &str = "let request = select step { 1 | 2 | 3 => step, _ => never() };";
/// Returns an error VALUE for 1; the caller's `?` raises it.
const BAD_RESULT: &str =
    "let bad = |v| -> [i64, Error<`Oops>] select v { 1 => error(`Oops), _ => v };";
/// THROWS for 1 through its own `?`: the caller has no `?` to match.
const BAD_THROWS: &str =
    "let bad = |v| { select v { 1 => error(`Oops)?, _ => null }; v };";

fn requests(form: &str) -> &'static str {
    if form == "seq" { SPACED } else { BURST }
}

async fn recovers_value(fusion_disabled: bool) -> Result<()> {
    for form in ["seq", "seqq"] {
        let code = format!(
            r#"{{
                {CLOCK}
                {requests}
                {BAD_RESULT}
                catch(e) println(e ~ "outer");
                {form} request {{
                    let x = try {{ bad(request)? }} with(e) {{ println("toast [request]"); 0 }};
                    x * 10
                }}
            }}"#,
            requests = requests(form)
        );
        let (values, out) = run_delta(&code, fusion_disabled).await?;
        assert_eq!(as_i64s(&values), [0, 20, 30], "{form}\n{out}");
        assert_eq!(out.trim(), "toast 1", "{form}");
    }
    Ok(())
}

// The try body has no `?`: the error arrives through the callee's
// dynamic scope, which is why the lowering installs a handler.
async fn callee_throw(fusion_disabled: bool) -> Result<()> {
    for form in ["seq", "seqq"] {
        let code = format!(
            r#"{{
                {CLOCK}
                {requests}
                {BAD_THROWS}
                catch(e) println(e ~ "outer");
                {form} request {{
                    let x = try {{ bad(request) }} with(e) {{ println("toast"); -1 }};
                    x
                }}
            }}"#,
            requests = requests(form)
        );
        let (values, out) = run_delta(&code, fusion_disabled).await?;
        assert_eq!(as_i64s(&values), [-1, 2, 3], "{form}\n{out}");
        assert_eq!(out.trim(), "toast", "{form}");
    }
    Ok(())
}

// Cleanup then abort: the with body rethrows once, the machine resets,
// the next request runs.
async fn cleanup_rethrow(fusion_disabled: bool) -> Result<()> {
    for form in ["seq", "seqq"] {
        for cleanup in [
            "println(\"cleanup [request]\")",
            "println(\"cleanup [request] [(e.0).error]\")",
        ] {
            let code = format!(
                r#"{{
                    {CLOCK}
                    {requests}
                    {BAD_RESULT}
                    catch(e) println("caught [(e.0).error]");
                    {form} request {{ try {{ bad(request)? }} with(e) {{ {cleanup}; e? }} }}
                }}"#,
                requests = requests(form)
            );
            let (values, out) = run_delta(&code, fusion_disabled).await?;
            assert_eq!(as_i64s(&values), [2, 3], "{form}: {cleanup}\n{out}");
            let expected = if cleanup.contains("error") {
                "cleanup 1 Oops\ncaught Oops"
            } else {
                "cleanup 1\ncaught Oops"
            };
            assert_eq!(out.trim(), expected, "{form}: {cleanup}");
        }
    }
    Ok(())
}

async fn nested(fusion_disabled: bool) -> Result<()> {
    for form in ["seq", "seqq"] {
        let code = format!(
            r#"{{
                {CLOCK}
                {requests}
                {BAD_RESULT}
                catch(e) println(e ~ "caught");
                {form} request {{
                    let x = try {{
                        try {{ bad(request)? }} with(e) {{ println("inner"); e? }}
                    }} with(e) {{ println("outer"); -1 }};
                    x
                }}
            }}"#,
            requests = requests(form)
        );
        let (values, out) = run_delta(&code, fusion_disabled).await?;
        assert_eq!(as_i64s(&values), [-1, 2, 3], "{form}\n{out}");
        assert_eq!(out.trim(), "inner\nouter", "{form}");
    }
    Ok(())
}

// Two `?` in one step both raise in the same cycle: the with body runs
// once, `e` is the first, the rest are consumed.
async fn multiple_errors(fusion_disabled: bool) -> Result<()> {
    for form in ["seq", "seqq"] {
        let code = format!(
            r#"{{
                {CLOCK}
                {requests}
                catch(e) println(e ~ "caught");
                {form} request {{
                    try {{
                        select request {{
                            1 => {{ error(`First)?; error(`Second)?; error(`Third)?; request }},
                            _ => request
                        }}
                    }} with(e) {{ println("with [request] [(e.0).error]"); -1 }}
                }}
            }}"#,
            requests = requests(form)
        );
        let (values, out) = run_delta(&code, fusion_disabled).await?;
        assert_eq!(as_i64s(&values), [-1, 2, 3], "{form}\n{out}");
        assert_eq!(out.trim(), "with 1 First", "{form}");
    }
    Ok(())
}

// The failing step follows an async one. `seqq` only: the wait is an
// idle wait, which the clock defers past every spaced `seq` request.
async fn async_step(fusion_disabled: bool) -> Result<()> {
    let code = format!(
        r#"{{
            {CLOCK}
            {BURST}
            {BAD_RESULT}
            catch(e) println(e ~ "caught");
            seqq request {{
                let x = try {{
                    let d = sys::time::after_idle(duration:2.ms, request);
                    bad(d)?
                }} with(e) {{ println("toast"); -1 }};
                x
            }}
        }}"#
    );
    let (values, out) = run_delta(&code, fusion_disabled).await?;
    assert_eq!(as_i64s(&values), [-1, 2, 3], "{out}");
    assert_eq!(out.trim(), "toast");
    Ok(())
}

// A let before the try is visible in both bodies; the with body's
// `e` and a try-body let are scoped to their bodies (see `refusals`).
async fn value_forms(fusion_disabled: bool) -> Result<()> {
    for (body, expected) in [
        (
            "let before = request * 2; let x = try { bad(request)? } with(e) { before }; x",
            vec![2, 2, 3],
        ),
        ("let out = 0; out <- try { bad(request)? } with(_) { -1 }; out", vec![-1, 2, 3]),
        (
            "println(\"start [request]\"); try { bad(request)? } with(e) { println(\"toast\") }; request * 3",
            vec![3, 6, 9],
        ),
        ("try { bad(request)? } with(e) { -1 }", vec![-1, 2, 3]),
    ] {
        let code = format!(
            r#"{{
                {CLOCK}
                {SPACED}
                {BAD_RESULT}
                catch(e) println(e ~ "caught");
                seq request {{ {body} }}
            }}"#
        );
        let (values, out) = run_delta(&code, fusion_disabled).await?;
        assert_eq!(as_i64s(&values), expected, "{body}\n{out}");
        assert!(!out.contains("caught"), "{body}\n{out}");
    }
    Ok(())
}

// A recovered request returns its credit at completion; a with body
// that rethrows returns it at the reset.
async fn seqq_credit(fusion_disabled: bool) -> Result<()> {
    for (handler, values, expected_out) in [
        ("println(\"toast\"); 0", vec![0, 2, 3], "toast"),
        ("println(\"cleanup\"); e?", vec![2, 3], "cleanup\ncaught"),
    ] {
        let code = format!(
            r#"{{
                {CLOCK}
                {BURST}
                {BAD_RESULT}
                catch(e) println(e ~ "caught");
                seqq request {{
                    let x = try {{ bad(request)? }} with(e) {{ {handler} }};
                    sys::time::after_idle(duration:2.ms, x)
                }}
            }}"#
        );
        let (got, out) = run_delta(&code, fusion_disabled).await?;
        assert_eq!(as_i64s(&got), values, "{handler}\n{out}");
        assert_eq!(out.trim(), expected_out, "{handler}");
    }
    Ok(())
}

async fn refusals() -> Result<()> {
    for (src, needle) in [
        (
            "seq { do { try { 1 } with(e) { 2 } } }",
            "try is a seq statement, not a do statement",
        ),
        (
            "seq { do { let x = try { 1 } with(e) { 2 }; x } }",
            "try is a seq statement, not a do statement",
        ),
        ("seq { do { catch(e) e; 1 } }", "catch is not allowed inside a seq"),
        ("seq { catch(e) e; 1 }", "catch is not allowed inside a seq"),
        (
            "{ let bad = |v| -> [i64, Error<`Oops>] error(`Oops); seq { do { catch(e) println(e); bad(1)? } } }",
            "catch is not allowed inside a seq",
        ),
        ("try { 1 } with(e) { 2 }", "`try … with` is only legal as a seq statement"),
        (
            "{ let f = |x| x; seq { f(try { 1 } with(e) { 2 }) } }",
            "`try … with` is only legal as a seq statement",
        ),
        ("seq { try { let y = 1; y } with(e) { 0 }; y }", "y not defined"),
        ("seq { try { 1 } with(e) { 0 }; e }", "e not defined"),
        ("seq { let x: string = try { 1 } with(e) { 2 }; x }", "string does not contain"),
        (
            "{ let bad = |v| -> [i64, Error<`Oops>] error(`Oops); seq { try { bad(1)? } with(e: Error<`Wrong>) { 0 } } }",
            "Error<`Wrong> does not contain",
        ),
        ("seq { { let x = 1; x } }", "a block is not a seq statement"),
    ] {
        let r = eval(src, crate::TEST_REGISTER).await;
        let msg = match &r {
            Err(e) => format!("{e:#}"),
            Ok((v, _)) => panic!("must be refused: {src} => {v:?}"),
        };
        assert!(msg.contains(needle), "wrong refusal for {src}: {msg}");
    }
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn refusals_interp() -> Result<()> {
    refusals().await
}

// A lambda literal is its own dynamic scope: a catch inside it is
// ordinary Graphix, not a seq statement, wherever the lambda sits.
#[tokio::test(flavor = "current_thread")]
async fn lambda_catch_is_ordinary() -> Result<()> {
    for src in [
        "seq { let f = |x| { catch(e) null; x }; f(1) }",
        "seq { let f = |x| { let g = |y| { catch(e) null; y }; g(x) }; f(1) }",
    ] {
        let (v, _) = eval(src, crate::TEST_REGISTER).await?;
        assert_eq!(v, Value::I64(1), "{src}");
    }
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
    recovers_value,
    callee_throw,
    cleanup_rethrow,
    nested,
    multiple_errors,
    async_step,
    value_forms,
    seqq_credit,
);
