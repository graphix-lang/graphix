use super::dense_deltas::run_delta;
use anyhow::Result;
use graphix_package_core::testing::{Mode, as_i64s};
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

async fn seq_composed_iterators(mode: Mode) -> Result<()> {
    for expr in [
        "(map::iter({go => go})).1",
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

// An idle wait's argument firing at the wake restarts the wait: tick 4
// replaces the value tick 1 left pending, so only 4 lands.
async fn select_aborts_slept_call(mode: Mode) -> Result<()> {
    let code = r#"{
        let tick = count(sys::time::timer(duration:100.ms, 4)?);
        let issued = never<i64>();
        select tick {
            1 | 4 => select sys::time::after_idle(duration:250.ms, tick) {
                reply => issued <- reply
            },
            _ => never()
        };
        issued
    }"#;
    let (values, _) = run_delta(code, mode).await?;
    assert_eq!(as_i64s(&values), [4]);
    Ok(())
}

// A sleep pauses an idle wait and the wake resumes it: the arm wakes at
// tick 4 with no fire of the wait's argument, and the wait tick 1 began
// still answers.
async fn wake_resumes_an_idle_wait(mode: Mode) -> Result<()> {
    let code = r#"{
        let tick = count(sys::time::timer(duration:100.ms, 4)?);
        let v = 7;
        let on = select tick { 1 | 4 => true, _ => false };
        let issued = never<i64>();
        select on {
            true => select sys::time::after_idle(duration:250.ms, v) {
                reply => issued <- reply
            },
            false => never()
        };
        issued
    }"#;
    let (values, _) = run_delta(code, mode).await?;
    assert_eq!(as_i64s(&values), [7]);
    Ok(())
}

// A sleep aborts a call in flight and the wake issues nothing: the child
// exits while the arm sleeps, the arm wakes with no argument firing, and
// the wait never answers. A child that exits before the sleep answers.
#[cfg(unix)]
async fn sleep_aborts_a_call_in_flight(mode: Mode) -> Result<()> {
    for (secs, expect) in [("0.05", &[Value::Bool(true)][..]), ("0.25", &[])] {
        let code = format!(
            r#"{{
                let child = sys::process::spawn(
                    sys::process::options(#args: ["-c", "sleep {secs}"], "/bin/sh")
                )$;
                let tick = count(sys::time::timer(child ~ duration:100.ms, 4)?);
                let on = true;
                on <- select tick {{ 2 => false, 4 => true, _ => never() }};
                let issued = never<bool>();
                select on {{
                    true => select sys::process::wait(child.proc) {{
                        error as _ => never(),
                        st => issued <- st.success
                    }},
                    false => never()
                }};
                issued
            }}"#
        );
        let (values, _) = run_delta(&code, mode).await?;
        assert_eq!(values, expect, "sleep {secs}");
    }
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
modes!(select_aborts_slept_call);
modes!(wake_resumes_an_idle_wait);
#[cfg(unix)]
modes!(sleep_aborts_a_call_in_flight);
modes!(seq_network);
modes!(live_timer_keeps_value);
#[cfg(unix)]
modes!(line_reader_rewake);

// A wake is a resume: it issues no call, and a call's result survives the
// sleep.

// The woken arm's count reads is_file's standing answer and counts no
// second one: r stays 1, and n reaches 2 only because the re-entered select
// delivers the arm's standing value.
async fn wake_keeps_a_calls_result(mode: Mode) -> Result<()> {
    let dir = tempfile::tempdir()?;
    std::fs::write(dir.path().join("f"), "")?;
    let path = dir.path().join("f").to_string_lossy().replace('\\', "/");
    let code = format!(
        r#"{{
            let path = "{path}";
            let on = true;
            let r = select on {{ true => count(sys::fs::is_file(path)$), false => never() }};
            let n = 0;
            n <- r ~ n + 1;
            let t = 0;
            t <- select on {{
                false => select t {{ x if x < 3 => x + 1, _ => never() }},
                true => never()
            }};
            on <- select n {{ 1 => false, _ => never() }};
            on <- select t {{ 3 => true, _ => never() }};
            (n, r)
        }}"#
    );
    let (values, _) = run_delta(&code, mode).await?;
    let printed: Vec<String> = values.iter().map(|v| v.to_string()).collect();
    assert_eq!(printed.last().map(String::as_str), Some("[i64:2, i64:1]"), "{printed:?}");
    Ok(())
}

// take keeps its count across the sleep: it took its one value before.
async fn wake_keeps_a_standing_count(mode: Mode) -> Result<()> {
    let code = r#"{
        let n = 0;
        n <- select n { k if k < 7 => k + 1, _ => never() };
        let in0 = select n { 3 | 4 => true, _ => false };
        let k = 1;
        select in0 { true => -1, false => take(#n: k, n) }
    }"#;
    let (values, _) = run_delta(code, mode).await?;
    assert_eq!(as_i64s(&values), [0, 0, 0, -1, -1, 0, 0, 0]);
    Ok(())
}

// A woken count resumes: it counts on from its pre-sleep total.
async fn wake_resumes_a_count(mode: Mode) -> Result<()> {
    let code = r#"{
        let n = 0;
        n <- select n { k if k < 5 => k + 1, _ => never() };
        let in0 = select n { 2 | 3 => 0, _ => 1 };
        select in0 { 0 => -1, _ => count(n) }
    }"#;
    let (values, _) = run_delta(code, mode).await?;
    assert_eq!(as_i64s(&values), [1, 2, -1, -1, 3, 4]);
    Ok(())
}

// A sleep pauses a timer's run and the wake resumes it: a one-shot or an
// idle wait that already fired stays done, and a run of three fires its
// last two after the wake.
async fn wake_resumes_standing_timers(mode: Mode) -> Result<()> {
    for (expr, expect) in [
        ("count(sys::time::timer(interval, once)?)", &[0, 1][..]),
        ("count(sys::time::after_idle(interval, seven))", &[0, 1]),
        ("count(sys::time::timer(interval, 3)?)", &[0, 1, 2, 3]),
    ] {
        let code = format!(
            r#"{{
                let interval = duration:10.ms;
                let once = false;
                let seven = 7;
                let on = true;
                let r = select on {{ true => {expr}, false => never() }};
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
        assert_eq!(as_i64s(&values), expect, "{expr}");
    }
    Ok(())
}

// A restart builtin keeps its state across a sleep: woken while its input
// stands it shows what it showed, and goes on when the input fires.
async fn wake_resumes_quietly(mode: Mode) -> Result<()> {
    for (expr, expect) in [
        ("count(x)", &[1, 2, -1, -1, 2, 3, 3][..]),
        ("once(x)", &[5, 5, -1, -1, 5, 5, 5]),
        ("uniq(x)", &[5, 6, -1, -1, 6, 7, 7]),
        ("take(#n: 1, x)", &[5, 5, -1, -1, 5, 5, 5]),
        ("skip(#n: 1, x)", &[6, -1, -1, 6, 7, 7]),
        ("hold(#clock: x, x)", &[5, 6, -1, -1, 6, 7, 7]),
    ] {
        let code = format!(
            r#"{{
                let n = 0;
                n <- select n {{ k if k < 6 => k + 1, _ => never() }};
                let x = 5;
                x <- select n {{ 0 => 6, 4 => 7, _ => never() }};
                let in0 = select n {{ 2 | 3 => 0, _ => 1 }};
                select in0 {{ 0 => -1, _ => {expr} }}
            }}"#
        );
        let (values, _) = run_delta(&code, mode).await?;
        assert_eq!(as_i64s(&values), expect, "{expr}");
    }
    Ok(())
}

// An arm holding a raise to a handler sleeps and wakes like an arm with a
// write: re-entered over a standing error, it reads it stale and raises
// nothing new.
async fn reentry_raises_once(mode: Mode) -> Result<()> {
    let code = r#"{
        let s = array::iter([1, 2, 1, 2]);
        let t = "x";
        let raised = 0;
        let out = {
            catch(x) raised <- x ~ raised + 1;
            select s { 1 => cast<i64>(t)? + 1, _ => 0 }
        };
        raised
    }"#;
    let (values, _) = run_delta(code, mode).await?;
    assert_eq!(values.last(), Some(&Value::I64(1)), "{values:?}");
    Ok(())
}

// The same through a called helper: an arm whose raise or outer write
// happens in a callee sleeps too, and its re-entries raise and write
// nothing new.
async fn reentry_through_a_callee_once(mode: Mode) -> Result<()> {
    let code = r#"{
        let s = array::iter([1, 2, 1, 2]);
        let t = "x";
        let n = 7;
        let raised = 0;
        let log: Array<i64> = [];
        let g = |v: string| cast<i64>(v)? + 1;
        let bump = |e: i64| { log <- [e]; 1 };
        let out = {
            catch(x) raised <- x ~ raised + 1;
            select s { 1 => (g(t), bump(n)), _ => (0, 0) }
        };
        (raised, count(log))
    }"#;
    let (values, _) = run_delta(code, mode).await?;
    let last = matches!(values.last(), Some(Value::Array(a))
        if a[..] == [Value::I64(1), Value::I64(2)]);
    assert!(last, "{values:?}");
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
    wake_keeps_a_calls_result,
    wake_keeps_a_standing_count,
    wake_resumes_a_count,
    wake_resumes_quietly,
    wake_resumes_standing_timers,
    reentry_raises_once,
    reentry_through_a_callee_once,
    range_over_a_late_bound
);
