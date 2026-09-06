use super::dense_deltas::{as_i64s, run_delta};
use anyhow::Result;
use netidx_value::Value;

async fn seq_timers(fusion_disabled: bool) -> Result<()> {
    for (expr, check) in [
        (
            "sys::time::after_idle(duration:40.ms, go)",
            "reply == go && sys::time::diff(sys::time::now(reply), go) >= duration:40.ms",
        ),
        (
            "sys::time::timer(duration:40.ms, false)?",
            "sys::time::diff(reply, go) >= duration:40.ms",
        ),
        (
            "sys::time::after_idle(duration:40.ms, 7)",
            "reply == 7 && sys::time::diff(sys::time::now(reply), go) >= duration:40.ms",
        ),
    ] {
        let code = format!(
            r#"{{
                let go = sys::time::timer(duration:150.ms, 2)?;
                seq go {{
                    let reply = {expr};
                    {check}
                }}
            }}"#
        );
        let (values, _) = run_delta(&code, fusion_disabled).await?;
        assert_eq!(values, [Value::Bool(true), Value::Bool(true)], "{expr}");
    }
    Ok(())
}

async fn seq_iterators(fusion_disabled: bool) -> Result<()> {
    for (expr, result) in [
        ("range(go, go + 1)?", "reply"),
        ("array::iter([go])", "reply"),
        ("array::iterq(#clock: go, [go])", "reply"),
        ("map::iter({go => go})", "reply.1"),
        ("map::iterq(#clock: go, {go => go})", "reply.1"),
        ("list::iter(list::from_array([go]))", "reply"),
        ("list::iterq(#clock: go, list::from_array([go]))", "reply"),
        ("queue(#clock: go, go)", "reply"),
    ] {
        let code = format!(
            r#"{{
                let step = 0;
                step <- select step {{ s if s < 20 => s + 1, _ => never() }};
                let go = select step {{ 1 | 10 => step, _ => never() }};
                seq go {{ let reply = {expr}; {result} }}
            }}"#
        );
        let (values, _) = run_delta(&code, fusion_disabled).await?;
        assert_eq!(as_i64s(&values), [1, 10], "{expr}");
    }
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn seq_projected_iterator() -> Result<()> {
    for fusion_disabled in [true, false] {
        let code = r#"{
            let step = 0;
            step <- select step { s if s < 20 => s + 1, _ => never() };
            let go = select step { 1 | 10 => step, _ => never() };
            seq go { let reply = (map::iter({go => go})).1; reply }
        }"#;
        let (values, _) = run_delta(code, fusion_disabled).await?;
        assert_eq!(as_i64s(&values), [1, 10]);
    }
    Ok(())
}

async fn seq_composed_iterators(fusion_disabled: bool) -> Result<()> {
    for expr in [
        "(map::iter({go => go})).1 + 0",
        "(array::iter([go]), 0).0",
        "({x: array::iter([go])}).x",
        "(array::iter([{x: go}])).x",
        "array::iter([[go]])[0]?",
        "array::iter([{0 => go}]){0}?",
        "cast<i64>(array::iter([go]))?",
        "-(-array::iter([go]))",
    ] {
        let code = format!(
            r#"{{
                let step = 0;
                step <- select step {{ s if s < 35 => s + 1, _ => never() }};
                let go = select step {{ 1 | 15 | 30 => step, _ => never() }};
                seq go {{ let reply = {expr}; reply }}
            }}"#
        );
        let (values, _) = run_delta(&code, fusion_disabled).await?;
        assert_eq!(as_i64s(&values), [1, 15, 30], "{expr}");
    }
    Ok(())
}

#[tokio::test]
async fn seq_composed_iterators_interp() -> Result<()> {
    seq_composed_iterators(true).await
}

#[tokio::test]
async fn seq_composed_iterators_jit() -> Result<()> {
    seq_composed_iterators(false).await
}

async fn seq_io(fusion_disabled: bool) -> Result<()> {
    let dir = tempfile::tempdir()?;
    std::fs::write(dir.path().join("1"), "first")?;
    std::fs::write(dir.path().join("2"), "second")?;
    let path = dir.path().to_string_lossy().replace('\\', "/");
    let code = format!(
        r#"{{
            let go = count(sys::time::timer(duration:150.ms, 2)?);
            seq go {{
                let reply = sys::fs::read_all("{path}/[go]")?;
                reply
            }}
        }}"#
    );
    let (values, _) = run_delta(&code, fusion_disabled).await?;
    assert_eq!(values, [Value::from("first"), Value::from("second")]);
    Ok(())
}

async fn select_restarts_timer(fusion_disabled: bool) -> Result<()> {
    let code = r#"{
        let tick = count(sys::time::timer(duration:100.ms, 4)?);
        let issued = never<i64>();
        select tick {
            1 | 3 => select sys::time::after_idle(duration:30.ms, tick) {
                reply => issued <- reply
            },
            _ => never()
        };
        issued
    }"#;
    let (values, _) = run_delta(code, fusion_disabled).await?;
    assert_eq!(as_i64s(&values), [1, 3]);
    Ok(())
}

async fn seq_network(fusion_disabled: bool) -> Result<()> {
    for expr in [
        r#"sys::net::subscribe("/restart/[go]")?"#,
        r#"sys::net::call("/restart/echo", {n: go})?"#,
    ] {
        let code = format!(
            r#"{{
                sys::net::publish("/restart/1", 1);
                sys::net::publish("/restart/2", 2);
                sys::net::rpc(
                    #path: "/restart/echo",
                    #doc: "echo",
                    #spec: {{n: {{default: 0, doc: "value"}}}},
                    #f: |args: {{n: i64}}| args.n
                );
                let go = count(sys::time::timer(duration:250.ms, 2)?);
                seq go {{ let reply: i64 = {expr}; reply }}
            }}"#
        );
        let (values, _) = run_delta(&code, fusion_disabled).await?;
        assert_eq!(as_i64s(&values), [1, 2], "{expr}");
    }
    Ok(())
}

async fn live_timer_keeps_value(fusion_disabled: bool) -> Result<()> {
    let code = r#"{
        let tick = count(sys::time::timer(duration:100.ms, 2)?);
        let ready = sys::time::after_idle(duration:30.ms, tick);
        let probe = sys::time::after_idle(duration:10.ms, tick);
        probe ~ ready
    }"#;
    let (values, _) = run_delta(code, fusion_disabled).await?;
    assert_eq!(as_i64s(&values), [1, 1]);
    Ok(())
}

#[cfg(unix)]
async fn line_reader_rewake(fusion_disabled: bool) -> Result<()> {
    let code = r#"{
        use sys::io::Lines;
        let child = sys::process::spawn(sys::process::options(
            #args: ["-c", "printf 'first\\n'; sleep 0.3; printf 'second\\n'"],
            #stdio: sys::process::stdio(#stdout: `Pipe),
            #kill_on_drop: true,
            "/bin/sh"
        ))?;
        let stream = opt::ok_or(child.stdout, `NoStdout)?;
        let tick = count(sys::time::timer(child ~ duration:100.ms, 3)?);
        let active = true;
        active <- select tick { 1 => false, 2 => true, _ => never() };
        select active {
            true => Lines::lines(stream)?,
            false => never()
        }
    }"#;
    let (values, _) = run_delta(code, fusion_disabled).await?;
    assert_eq!(values, [Value::from("first"), Value::from("second")]);
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

modes!(seq_timers, seq_timers_interp, seq_timers_jit);
modes!(seq_iterators, seq_iterators_interp, seq_iterators_jit);
modes!(seq_io, seq_io_interp, seq_io_jit);
modes!(select_restarts_timer, select_restarts_timer_interp, select_restarts_timer_jit);
modes!(seq_network, seq_network_interp, seq_network_jit);
modes!(live_timer_keeps_value, live_timer_keeps_value_interp, live_timer_keeps_value_jit);
#[cfg(unix)]
modes!(line_reader_rewake, line_reader_rewake_interp, line_reader_rewake_jit);
