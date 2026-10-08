use super::dense_deltas::{as_i64s, run_delta};
use anyhow::Result;
use graphix_package_core::testing::Mode;
use netidx_value::Value;

async fn seq_timers(mode: Mode) -> Result<()> {
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
        let (values, _) = run_delta(&code, mode).await?;
        assert_eq!(values, [Value::Bool(true), Value::Bool(true)], "{expr}");
    }
    Ok(())
}

async fn seq_iterators(mode: Mode) -> Result<()> {
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
        let (values, _) = run_delta(&code, mode).await?;
        assert_eq!(as_i64s(&values), [1, 10], "{expr}");
    }
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn seq_projected_iterator() -> Result<()> {
    for mode in Mode::ALL {
        let code = r#"{
            let step = 0;
            step <- select step { s if s < 20 => s + 1, _ => never() };
            let go = select step { 1 | 10 => step, _ => never() };
            seq go { let reply = (map::iter({go => go})).1; reply }
        }"#;
        let (values, _) = run_delta(code, mode).await?;
        assert_eq!(as_i64s(&values), [1, 10]);
    }
    Ok(())
}

async fn seq_composed_iterators(mode: Mode) -> Result<()> {
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
        let (values, _) = run_delta(&code, mode).await?;
        assert_eq!(as_i64s(&values), [1, 15, 30], "{expr}");
    }
    Ok(())
}

modes!(seq_composed_iterators);

async fn seq_io(mode: Mode) -> Result<()> {
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
    let (values, _) = run_delta(&code, mode).await?;
    assert_eq!(values, [Value::from("first"), Value::from("second")]);
    Ok(())
}

async fn select_restarts_timer(mode: Mode) -> Result<()> {
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
    let (values, _) = run_delta(code, mode).await?;
    assert_eq!(as_i64s(&values), [1, 3]);
    Ok(())
}

async fn seq_network(mode: Mode) -> Result<()> {
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
        let (values, _) = run_delta(&code, mode).await?;
        assert_eq!(as_i64s(&values), [1, 2], "{expr}");
    }
    Ok(())
}

async fn live_timer_keeps_value(mode: Mode) -> Result<()> {
    let code = r#"{
        let tick = count(sys::time::timer(duration:100.ms, 2)?);
        let ready = sys::time::after_idle(duration:30.ms, tick);
        let probe = sys::time::after_idle(duration:10.ms, tick);
        probe ~ ready
    }"#;
    let (values, _) = run_delta(code, mode).await?;
    assert_eq!(as_i64s(&values), [1, 1]);
    Ok(())
}

#[cfg(unix)]
async fn line_reader_rewake(mode: Mode) -> Result<()> {
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
    let (values, _) = run_delta(code, mode).await?;
    assert_eq!(values, [Value::from("first"), Value::from("second")]);
    Ok(())
}

modes!(seq_timers);
modes!(seq_iterators);
modes!(seq_io);
modes!(select_restarts_timer);
modes!(seq_network);
modes!(live_timer_keeps_value);
#[cfg(unix)]
modes!(line_reader_rewake);

// A woken arm restarts each builtin over its present arguments, every one
// of them standing at the wake.

// An async builtin is issued again, so it answers again.
async fn wake_reissues_standing_async(mode: Mode) -> Result<()> {
    let dir = tempfile::tempdir()?;
    std::fs::write(dir.path().join("f"), "")?;
    let path = dir.path().join("f").to_string_lossy().replace('\\', "/");
    let code = format!(
        r#"{{
            let path = "{path}";
            let on = true;
            let r = select on {{ true => sys::fs::is_file(path)$, false => never() }};
            let n = 0;
            n <- r ~ n + 1;
            let t = 0;
            t <- select on {{
                false => select t {{ x if x < 3 => x + 1, _ => never() }},
                true => never()
            }};
            on <- select n {{ 1 => false, _ => never() }};
            on <- select t {{ 3 => true, _ => never() }};
            n
        }}"#
    );
    let (values, _) = run_delta(&code, mode).await?;
    assert_eq!(as_i64s(&values), [0, 1, 2]);
    Ok(())
}

// take keeps a let-bound #n across the sleep and restarts its count.
async fn wake_keeps_a_standing_count(mode: Mode) -> Result<()> {
    let code = r#"{
        let n = 0;
        n <- select n { k if k < 7 => k + 1, _ => never() };
        let in0 = select n { 3 | 4 => true, _ => false };
        let k = 1;
        select in0 { true => -1, false => take(#n: k, n) }
    }"#;
    let (values, _) = run_delta(code, mode).await?;
    assert_eq!(as_i64s(&values), [0, 0, 0, -1, -1, 5, 5, 5]);
    Ok(())
}

// A woken count is a fresh one: it does not deliver its pre-sleep total.
async fn wake_restarts_a_count(mode: Mode) -> Result<()> {
    let code = r#"{
        let n = 0;
        n <- select n { k if k < 5 => k + 1, _ => never() };
        let in0 = select n { 2 | 3 => 0, _ => 1 };
        select in0 { 0 => -1, _ => count(n) }
    }"#;
    let (values, _) = run_delta(code, mode).await?;
    assert_eq!(as_i64s(&values), [1, 2, -1, -1, 1, 2]);
    Ok(())
}

// A range waits for a late bound instead of refusing it.
async fn range_over_a_late_bound(mode: Mode) -> Result<()> {
    let code = r#"{
        let late = never();
        late <- 3;
        range(0, late)
    }"#;
    let (values, _) = run_delta(code, mode).await?;
    assert_eq!(as_i64s(&values), [0, 1, 2]);
    Ok(())
}

modes!(
    wake_reissues_standing_async,
    wake_keeps_a_standing_count,
    wake_restarts_a_count,
    range_over_a_late_bound
);
