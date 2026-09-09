use super::dense_deltas::{as_i64s, run_delta};
use anyhow::Result;
use graphix_package_core::testing::eval;
use netidx_value::{ValArray, Value};

const CLOCK: &str = r#"
    let step = 0;
    step <- select step { n if n < 60 => n + 1, _ => never() };
"#;

async fn continuations(fusion_disabled: bool) -> Result<()> {
    for form in ["seq", "seqq"] {
        for body in [
            "do { { error(`Oops)?; 1 } }; n <- n + 1",
            "let x = { error(`Oops)?; 1 }; n <- x",
            "n <- { error(`Oops)?; 1 }",
            "until { error(`Oops)?; true }; n <- n + 1",
            "do { { error(`Oops)?; 1 }; n <- n + 1 }",
            "do { let x = { error(`Oops)?; 1 }; n <- x }",
            "do { let x = 1; n <- { error(`Oops)?; x } }",
            "bad(1); n <- n + 1",
            "do { bad(1); n <- n + 1 }",
            "delayed(1); n <- n + 1",
        ] {
            let code = format!(
                r#"{{
                    {CLOCK}
                    let n = 0;
                    let errors = 0;
                    let bad = |v| {{ error(`Oops)?; v }};
                    let delayed = |v| {{
                        let t = sys::time::after_idle(duration:5.ms, v);
                        error(t ~ `Oops)?;
                        t
                    }};
                    catch(e) errors <- e ~ errors + 1;
                    {form} {{ {body} }};
                    sys::time::after_idle(duration:20.ms,
                        select step {{ 60 => (n, errors), _ => never() }})
                }}"#
            );
            let (values, _) = run_delta(&code, fusion_disabled).await?;
            assert_eq!(
                values,
                [Value::Array(ValArray::from([Value::I64(0), Value::I64(1)]))],
                "{form} {{ {body} }}"
            );
        }
    }
    Ok(())
}

async fn final_output(fusion_disabled: bool) -> Result<()> {
    for form in ["seq", "seqq"] {
        for body in [
            "do { { error(`Oops)?; 42 } }",
            "let x = { error(`Oops)?; 42 }",
            "n <- { error(`Oops)?; 42 }",
            "do { let x = 42; { error(`Oops)?; x } }",
            "bad(42)",
        ] {
            let code = format!(
                r#"{{
                    let n = 0;
                    let bad = |v| {{ error(`Oops)?; v }};
                    catch(e) println(e ~ "caught");
                    {form} {{ {body} }}
                }}"#
            );
            let (values, out) = run_delta(&code, fusion_disabled).await?;
            assert!(values.is_empty(), "{form} {{ {body} }}: {values:?}");
            assert_eq!(out.trim(), "caught", "{form} {{ {body} }}");
        }
    }
    Ok(())
}

async fn queue_credit(fusion_disabled: bool) -> Result<()> {
    for body in [
        "do { { select request { 1 | 3 => error(`Oops)?, _ => never() }; request } }",
        "bad(request)",
        "let r = bad(request); sys::time::after_idle(duration:5.ms, r)",
        "do { let r = bad(request); r }",
    ] {
        let code = format!(
            r#"{{
                {CLOCK}
                let request = select step {{ 1 | 2 | 3 | 4 | 5 => step, _ => never() }};
                let bad = |v| {{
                    select v {{ 1 | 3 => error(`Oops)?, _ => never() }};
                    v
                }};
                catch(e) println(e ~ "caught");
                seqq request {{ {body} }}
            }}"#
        );
        let (values, out) = run_delta(&code, fusion_disabled).await?;
        assert_eq!(as_i64s(&values), [2, 4, 5], "{body}");
        assert_eq!(out.lines().collect::<Vec<_>>(), ["caught", "caught"], "{body}");
    }
    Ok(())
}

async fn restart(fusion_disabled: bool) -> Result<()> {
    for form in ["seq", "seqq"] {
        for (body, expected) in [
            ("bad(request)", "caught"),
            (
                r#"try { bad(request) } with(e) { println("cleanup"); e? }"#,
                "cleanup\ncaught",
            ),
        ] {
            let code = format!(
                r#"{{
                    {CLOCK}
                    let request = select step {{
                        1 => 1, 20 => 2, 40 => 3, _ => never()
                    }};
                    let bad = |v| {{
                        select v {{ 1 => error(`Oops)?, _ => never() }};
                        v
                    }};
                    catch(e) println(e ~ "caught");
                    {form} request {{ {body} }}
                }}"#
            );
            let (values, out) = run_delta(&code, fusion_disabled).await?;
            assert_eq!(as_i64s(&values), [2, 3], "{form}, {body}");
            assert_eq!(out.trim(), expected, "{form}, {body}");
        }
    }
    Ok(())
}

async fn scoped_errors(fusion_disabled: bool) -> Result<()> {
    for code in [
        r#"{ catch(e) println(e ~ "caught"); error(`Oops)?; 42 }"#,
        r#"seq { try { error(`Oops)?; 42 } with(e) { println("caught"); 42 } }"#,
        r#"seqq { try { error(`Oops)?; 42 } with(e) { println("caught"); 42 } }"#,
        r#"{
            catch(e) println(e ~ "caught");
            seq { do { { error(`Oops)?; 0 } } };
            seq { 42 }
        }"#,
        r#"{
            let f = |v| { select v { 0 => error(`Oops)?, _ => never() }; v };
            catch(e) println(e ~ "caught");
            seq { f(0) };
            seq { f(42) }
        }"#,
    ] {
        let (values, out) = run_delta(code, fusion_disabled).await?;
        assert_eq!(as_i64s(&values), [42], "{code}");
        assert_eq!(out.trim(), "caught", "{code}");
    }
    Ok(())
}

async fn recursive_errors(fusion_disabled: bool) -> Result<()> {
    for form in ["seq", "seqq"] {
        let code = format!(
            r#"{{
                #[sync]
                let rec bad = |n: i64| select n {{
                    0 => {{ error(`Oops)?; 42 }}, n => bad(n - 1)
                }};
                catch(e) println(e ~ "caught");
                {form} {{ bad(1000) }}
            }}"#
        );
        let (values, out) = run_delta(&code, fusion_disabled).await?;
        assert!(values.is_empty(), "{form}: {values:?}");
        assert_eq!(out.trim(), "caught", "{form}");
    }
    Ok(())
}

async fn nested_sequences(fusion_disabled: bool) -> Result<()> {
    for inner in ["seq", "seqq"] {
        let code = format!(
            r#"{{
                catch(e) println(e ~ "caught");
                seq {{ {inner} {{ do {{ {{ error(`Oops)?; 1 }} }} }}; 42 }}
            }}"#
        );
        let (values, out) = run_delta(&code, fusion_disabled).await?;
        assert!(values.is_empty(), "{inner}: {values:?}");
        assert_eq!(out.trim(), "caught", "{inner}");
    }
    Ok(())
}

async fn multiple_errors(fusion_disabled: bool) -> Result<()> {
    for form in ["seq", "seqq"] {
        for body in ["bad(request)", "{ let r = bad(request); r }"] {
            let requests = if form == "seq" {
                "1 => 1, 15 => 2, 30 => 3, 45 => 4, 55 => 5"
            } else {
                "1 | 2 | 3 | 4 | 5 => step"
            };
            let success = if form == "seq" {
                "v"
            } else {
                "sys::time::after_idle(duration:5.ms, v)"
            };
            let code = format!(
                r#"{{
                    {CLOCK}
                    let request = select step {{ {requests}, _ => never() }};
                    let bad = |v| select v {{
                        1 | 3 => {{
                            error(`First)?;
                            error(`Second)?;
                            error(`Third)?;
                            v
                        }},
                        _ => {success}
                    }};
                    catch(e) println("caught [(e.0).error]");
                    {form} request {{
                        try {{ {body} }} with(e) {{
                            println("cleanup [request] [(e.0).error]");
                            e?
                        }}
                    }}
                }}"#
            );
            let (values, out) = run_delta(&code, fusion_disabled).await?;
            assert_eq!(as_i64s(&values), [2, 4, 5], "{form}: {body}\n{out}");
            // three `?` raise in one cycle; the with body sees the first
            // and rethrows it once
            let expected = [1, 3]
                .into_iter()
                .map(|v| format!("cleanup {v} First\ncaught First"))
                .collect::<Vec<_>>()
                .join("\n");
            assert_eq!(out.trim(), expected, "{form}: {body}");
        }
    }
    Ok(())
}

async fn nested_multiple_errors(fusion_disabled: bool) -> Result<()> {
    for inner in ["seq", "seqq"] {
        let code = format!(
            r#"{{
                {CLOCK}
                let request = select step {{ 1 | 2 | 3 => step, _ => never() }};
                catch(e) println("caught [(e.0).error]");
                seqq request {{
                    try {{
                        {inner} {{
                            try {{
                                select request {{
                                    1 => {{ error(`First)?; error(`Second)?; 1 }},
                                    _ => request
                                }}
                            }} with(e) {{ println("inner [request] [(e.0).error]"); e? }}
                        }}
                    }} with(e) {{ println("outer [request] [(e.0).error]"); e? }};
                    request
                }}
            }}"#
        );
        let (values, out) = run_delta(&code, fusion_disabled).await?;
        assert_eq!(as_i64s(&values), [2, 3], "{inner}\n{out}");
        assert_eq!(out.trim(), "inner 1 First\nouter 1 First\ncaught First", "{inner}");
    }
    Ok(())
}

async fn recursive_multiple_errors(fusion_disabled: bool) -> Result<()> {
    for (body, expected) in [
        ("select request { 1 => bad(1000), _ => request }", "caught Same\ncaught Same"),
        (
            r#"try {
                    select request { 1 => bad(1000), _ => request }
                } with(e) { println("cleanup [request]"); e? }"#,
            "cleanup 1\ncaught Same",
        ),
    ] {
        let code = format!(
            r#"{{
                {CLOCK}
                let request = select step {{ 1 | 2 | 3 => step, _ => never() }};
                #[sync]
                let rec bad = |n: i64| select n {{
                    0 => {{ error(`Same)?; error(`Same)?; 1 }},
                    n => bad(n - 1)
                }};
                catch(e) println("caught [(e.0).error]");
                seqq request {{ {body} }}
            }}"#
        );
        let (values, out) = run_delta(&code, fusion_disabled).await?;
        assert_eq!(as_i64s(&values), [2, 3], "{body}\n{out}");
        assert_eq!(out.trim(), expected, "{body}");
    }
    Ok(())
}

async fn multiple_errors_after_sleep(fusion_disabled: bool) -> Result<()> {
    for form in ["seq", "seqq"] {
        let code = format!(
            r#"{{
                let request = count(sys::time::timer(duration:80.ms, 5)?);
                let done = never<i64>();
                catch(e) println("caught [(e.0).error]");
                select request {{
                    1 | 3 | 5 => {{
                        let r = {form} request {{
                            try {{
                                select request {{
                                    1 | 3 => {{ error(`First)?; error(`Second)?; request }},
                                    _ => request
                                }}
                            }} with(e) {{ println("cleanup [request]"); e? }}
                        }};
                        done <- r
                    }},
                    _ => never()
                }};
                done
            }}"#
        );
        let (values, out) = run_delta(&code, fusion_disabled).await?;
        assert_eq!(as_i64s(&values), [5], "{form}\n{out}");
        assert_eq!(
            out.trim(),
            "cleanup 1\ncaught First\ncleanup 3\ncaught First",
            "{form}"
        );
    }
    Ok(())
}

async fn error_payloads(fusion_disabled: bool) -> Result<()> {
    let code = format!(
        r#"{{
            {CLOCK}
            let request = select step {{ 1 | 2 | 3 => step, _ => never() }};
            let bad = |v: i64| select v {{
                1 => {{ error(`Oops(v))?; error(`Oops(v + 10))?; v }},
                _ => v
            }};
            catch(e) select (e.0).error {{ `Oops(n) => println("caught [n]") }};
            seqq request {{
                try {{ bad(request) }} with(e) {{
                    select (e.0).error {{ `Oops(n) => println("cleanup [request] [n]") }};
                    e?
                }}
            }}
        }}"#
    );
    let (values, out) = run_delta(&code, fusion_disabled).await?;
    assert_eq!(as_i64s(&values), [2, 3], "{out}");
    assert_eq!(out.trim(), "cleanup 1 1\ncaught 1");
    Ok(())
}

async fn nested_handler_choices(fusion_disabled: bool) -> Result<()> {
    for rethrow in [false, true] {
        let handler = if rethrow {
            "select (e.0).error { `First => e?, `Second => request }"
        } else {
            "request"
        };
        let outer =
            if rethrow { "catch(e) println(\"caught [(e.0).error]\");" } else { "" };
        let code = format!(
            r#"{{
                {CLOCK}
                let request = select step {{ 1 | 2 | 3 => step, _ => never() }};
                {outer}
                seqq request {{
                    try {{
                        select request {{
                            1 => {{ error(`First)?; error(`Second)?; request }},
                            _ => request
                        }}
                    }} with(e) {{ println("local [(e.0).error]"); {handler} }}
                }}
            }}"#
        );
        let (values, out) = run_delta(&code, fusion_disabled).await?;
        let expected_values: &[i64] = if rethrow { &[2, 3] } else { &[1, 2, 3] };
        assert_eq!(as_i64s(&values), expected_values, "{handler}\n{out}");
        let expected = if rethrow { "local First\ncaught First" } else { "local First" };
        assert_eq!(out.trim(), expected, "{handler}");
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

async fn seq_statement_refusals() -> Result<()> {
    for (src, needle) in [
        ("seq { catch(e) e; 1 }", "catch is not allowed inside a seq"),
        ("seqq { catch(e) e; 1 }", "catch is not allowed inside a seq"),
        ("seq { 1; catch(e) e }", "catch is not allowed inside a seq"),
        ("seq { { let x = 1; x } }", "a block is not a seq statement"),
        ("seqq { { let x = 1; x }; 2 }", "a block is not a seq statement"),
        ("seq { do { catch(e) e } }", "catch is not allowed inside a seq"),
        ("seq { do { 1; catch(e) e } }", "catch is not allowed inside a seq"),
        ("seq { let x = { catch(e) null; 1 }; x }", "catch is not allowed inside a seq"),
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
async fn seq_statement_refusals_interp() -> Result<()> {
    seq_statement_refusals().await
}

modes!(
    continuations,
    final_output,
    queue_credit,
    restart,
    scoped_errors,
    recursive_errors,
    nested_sequences,
    multiple_errors,
    nested_multiple_errors,
    recursive_multiple_errors,
    multiple_errors_after_sleep,
    error_payloads,
    nested_handler_choices
);
