use super::dense_deltas::run_delta;
use anyhow::Result;
use graphix_package_core::{
    run,
    testing::{Mode, as_i64s, escape_path},
};
use netidx_value::{ValArray, Value};

async fn carried_arguments(mode: Mode) -> Result<()> {
    for form in ["seq", "seqq"] {
        for body in [
            "let x = request; let f = |v| v ~ x; f(request)",
            "let x = request; let f = |v| v ~ x; let y = f(request); y",
            "let x = request; let f = |v| v ~ x; { let y = f(request); y }",
            "{ let x = request; let f = |v| v ~ x; f(request) }",
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
            let (values, _) = run_delta(&code, mode).await?;
            assert_eq!(as_i64s(&values), [1, 2, 3], "{form}: {body}");
        }
    }
    Ok(())
}

async fn inputs_ready_together(mode: Mode) -> Result<()> {
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
    let (values, _) = run_delta(code, mode).await?;
    assert_eq!(values, [Value::Array(ValArray::from([Value::I64(50), Value::I64(1)]))]);
    Ok(())
}

async fn bottom_at_reentry(mode: Mode) -> Result<()> {
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
    let (values, _) = run_delta(code, mode).await?;
    assert_eq!(as_i64s(&values), [1, 2, 3]);
    Ok(())
}

async fn until_stays_live(mode: Mode) -> Result<()> {
    let code = r#"{
        let step = 0;
        step <- select step { n if n < 20 => n + 1, _ => never() };
        let ready = |n| n >= 10;
        seq { until live(ready(step)); step }
    }"#;
    let (values, _) = run_delta(code, mode).await?;
    assert_eq!(as_i64s(&values), [10]);
    Ok(())
}

async fn pending_call_keeps_inputs(mode: Mode) -> Result<()> {
    let code = r#"{
        let step = 0;
        step <- select step { n if n < 30 => n + 1, _ => never() };
        let request = select step { 1 => step, _ => never() };
        let input = select step { n if n < 5 => 7, _ => never() };
        let f = |v| sys::time::after_idle(duration:10.ms, v);
        seq request { f(input) }
    }"#;
    let (values, _) = run_delta(code, mode).await?;
    assert_eq!(as_i64s(&values), [7]);
    Ok(())
}

async fn native_call(mode: Mode) -> Result<()> {
    let code = r#"{
        let step = 0;
        step <- select step { n if n < 90 => n + 1, _ => never() };
        let request = select step { 1 => 1, 30 => 2, 60 => 3, _ => never() };
        let f = |v| v + 0;
        seq request { let x = request; #[native] f(x) }
    }"#;
    let (values, _) = run_delta(code, mode).await?;
    assert_eq!(as_i64s(&values), [1, 2, 3]);
    Ok(())
}

// A call handed a reference may write through it: the statement after
// it that reads the variable starts the next cycle and sees the write.
async fn reference_argument_writes(mode: Mode) -> Result<()> {
    for form in ["seq", "seqq"] {
        for body in ["set(&mut b, 5); let s = b; s", "let r = &mut b; set(r, 5); b"] {
            let code = format!(
                r#"{{
                    let set = |r: &mut i64, v: i64| {{ *r <- v; null }};
                    let b = 0;
                    {form} {{ {body} }}
                }}"#
            );
            let (values, _) = run_delta(&code, mode).await?;
            assert_eq!(as_i64s(&values), [5], "{form}: {body}");
        }
    }
    Ok(())
}

modes!(
    carried_arguments,
    inputs_ready_together,
    bottom_at_reentry,
    until_stays_live,
    pending_call_keeps_inputs,
    native_call,
    reference_argument_writes,
);

// A builtin given references inside a structure writes through them: the
// next step waits for the writes.
async fn builtin_writes_through_nested_references(mode: Mode) -> Result<()> {
    let (values, _) = run_delta(
        r#"{
            let x: u8 = u8:0;
            let b = buffer::encode([`U8(u8:7)]);
            seq { buffer::decode(b, [`U8(&mut x)])$; cast<i64>(x)$ }
        }"#,
        mode,
    )
    .await?;
    assert_eq!(as_i64s(&values), vec![7]);
    Ok(())
}

// Each instance's seqq captures are its own: a second call's plan does not
// decide the first's.
async fn seqq_captures_per_instance(mode: Mode) -> Result<()> {
    let (values, _) = run_delta(
        r#"{
            let mk = |go: i64| {
                let n = 0;
                let put = |v| { n <- v; v };
                seqq go { put(5); n }
            };
            mk(1) * 10 + mk(1)
        }"#,
        mode,
    )
    .await?;
    assert_eq!(as_i64s(&values), vec![55]);
    Ok(())
}

// One definition whose instances need different verdicts: the instance
// whose callback writes `x` reads it live (100), the other snapshots it
// (0), whichever is bound first.
async fn seqq_verdict_per_instance(mode: Mode) -> Result<()> {
    let a = "let a = run(1, |v| { x <- v * 100; v });";
    let b = "let b = run(1, |v| sys::time::after_idle(duration:50.ms, v));";
    for lets in [[a, b], [b, a]] {
        let [first, second] = lets;
        let code = format!(
            r#"{{
            let x = 0;
            let run = |go: i64, f: fn(v: i64) -> i64| seqq go {{ f(1); x }};
            {first}
            {second}
            (a, b)
        }}"#
        );
        let (values, _) = run_delta(&code, mode).await?;
        let values: Vec<String> = values.iter().map(|v| v.to_string()).collect();
        assert_eq!(values, ["[i64:100, i64:0]"], "{first} first");
    }
    Ok(())
}

modes!(
    builtin_writes_through_nested_references,
    seqq_captures_per_instance,
    seqq_verdict_per_instance
);

// An unannotated parameter passed where the callee declares a function
// stays bare in the issue, so the call resolves its callback and fuses.
run!(
    unannotated_fn_param_issued_bare,
    r#"{
  let apply = |f, t| seq t {
    let y = #[native] array::map([1, 2, 3], f);
    y
  };
  apply(|x: i64| x + 1, 1)
}"#,
    |v: Result<&Value>| matches!(v, Ok(v) if v.to_string() == "[i64:2, i64:3, i64:4]")
);

// The same under seqq: the parameter is not captured into the request.
run!(
    unannotated_fn_param_not_captured_by_seqq,
    r#"{
  let apply = |f, t| seqq t {
    let y = #[native] array::map([1, 2, 3], f);
    y
  };
  apply(|x: i64| x + 1, 1)
}"#,
    |v: Result<&Value>| matches!(v, Ok(v) if v.to_string() == "[i64:2, i64:3, i64:4]")
);

// A call whose argument reads the world live is issued again at each fire
// of that argument, a pattern bind of a live scrutinee included, and the
// step completes on the call's first value after entry (x-rerun-1-01).
run!(
    a_call_over_a_live_argument_is_issued_at_each_fire,
    r#"{
  let x = 0;
  select x { k if k < 5 => x <- x + 1, _ => never() };
  let big = |v| v > 2;
  let guarded = seq { let v = select live(x) { v if big(v) => v, _ => never() }; v };
  let scrutinee = seq { let v = select big(live(x)) { true => live(x), false => never() }; v };
  (guarded, scrutinee)
}"#,
    |v: Result<&Value>| {
        matches!(v, Ok(Value::Array(a)) if a[..] == [Value::I64(3), Value::I64(3)])
    }
);

// A nullary call in a step is issued on the entry clock, as any call is,
// so each run's answer is that run's (t-seq-03). Each run after the first
// starts from the answer before it.
run!(
    a_nullary_call_answers_each_run,
    r#"{
  let next = never<i64>();
  let t = any(0, next);
  let runs = 0;
  let f = || sys::time::timer(duration:5.ms, false) ~ runs;
  let r = seq t { runs <- runs + 1; let a = f(); a };
  next <- select r { n if n < 3 => n, _ => never() };
  array::group(r, |n, _| n == 3)
}"#,
    |v: Result<&Value>| {
        matches!(v, Ok(Value::Array(a)) if a[..] == [Value::I64(1), Value::I64(2), Value::I64(3)])
    }
);

// A call a step issues is a fresh frame at every run (t-seq-03): the
// callee reads what stands, its captures included, as a fresh call would,
// so each run reads back the file it wrote, through a function, a function
// it calls, a function value (bound at its first call or before) and a
// callback alike.
async fn a_callee_reads_its_world_fresh_each_run(mode: Mode) -> Result<()> {
    let dir = tempfile::tempdir()?;
    let path = escape_path(dir.path().join("f.txt").display());
    for call in [
        "sys::fs::read_all(path)",
        "load()",
        "load1(n)",
        "nested(n)",
        "pick(path)",
        "bound()",
        "apply(|p| sys::fs::read_all(p))",
    ] {
        let code = r#"{
            let path = "PATH";
            let next = never<i64>();
            let runs = 0;
            let t = select (any(0, next) ~ runs) { k if k < 3 => k, _ => never() };
            let load = || sys::fs::read_all(path);
            let load1 = |u| sys::fs::read_all(path);
            let nested = |u| u ~ load();
            let pick = select runs {
                100 => |p| sys::fs::read_all(p),
                _ => |p| sys::fs::read_all(p)
            };
            let bound = select t {
                100 => || sys::fs::read_all(path),
                _ => || sys::fs::read_all(path)
            };
            let apply = |f: fn(p: string) -> Result<string, `IOError(string)>| f(path);
            let r = seq t {
                let n = runs + 1;
                runs <- n;
                sys::fs::write_all(#path: path, "[n]")$;
                let a = cast<i64>(CALL$)$;
                a
            };
            next <- r ~ 1;
            r
        }"#
        .replace("PATH", &path)
        .replace("CALL", call);
        let (values, _) = run_delta(&code, mode).await?;
        assert_eq!(as_i64s(&values), [1, 2, 3], "{call}");
    }
    Ok(())
}

// An rpc call a step issues answers its own run: the server answers the
// run it serves, and a run never completes on the reply that answered the
// run before it, through a function value or a sample. `RPC` is the call
// builtin `name`, `RET` its result type, and `ANSWER` takes the answer
// from that.
async fn steps_rpc_answer_their_own_runs(
    mode: Mode,
    name: &str,
    ret: &str,
    answer: &str,
) -> Result<()> {
    for call in [
        "RPC(p, null)",
        "load()",
        "load1(n)",
        "nested(n)",
        "pick(p)",
        "bound()",
        "apply(|q| RPC(q, null))",
    ] {
        let code = r#"{
            let p = "/local/seq/NAME";
            let next = never<i64>();
            let runs = 0;
            sys::net::rpc(#path: p, #doc: "runs", #spec: null, #f: |a: null| a ~ runs);
            let t = select (any(0, next) ~ runs) { k if k < 3 => k, _ => never() };
            let load = || RPC(p, null);
            let load1 = |u| RPC(p, null);
            let nested = |u| u ~ load();
            let pick = select runs {
                100 => |q| RPC(q, null),
                _ => |q| RPC(q, null)
            };
            let bound = select t {
                100 => || RPC(p, null),
                _ => || RPC(p, null)
            };
            let apply = |f: fn(q: string) -> RET| f(p);
            let r = seq t {
                let n = runs + 1;
                runs <- n;
                let a: i64 = (CALL)ANSWER$;
                a
            };
            next <- r ~ 1;
            r
        }"#
        .replace("CALL", call)
        .replace("RPC", &format!("sys::net::{name}"))
        .replace("NAME", name)
        .replace("RET", ret)
        .replace("ANSWER", answer);
        let (values, _) = run_delta(&code, mode).await?;
        assert_eq!(as_i64s(&values), [1, 2, 3], "{name}: {call}");
    }
    Ok(())
}

async fn a_steps_rpc_call_answers_its_own_run(mode: Mode) -> Result<()> {
    let ret = "(sys::net::RpcId, Result<i64, \
               [`RpcError(string), `InvalidCast(string), `Aborted]>)";
    steps_rpc_answer_their_own_runs(mode, "call", ret, ".1").await
}

async fn a_steps_callq_answers_its_own_run(mode: Mode) -> Result<()> {
    let ret = "Result<i64, [`RpcError(string), `InvalidCast(string)]>";
    steps_rpc_answer_their_own_runs(mode, "callq", ret, "").await
}

// The state a callee keeps starts afresh at each run, `count`'s and a
// written `let`'s alike, while what it captures from outside keeps its
// value across runs (t-seq-03).
async fn a_callees_own_state_starts_fresh_each_run(mode: Mode) -> Result<()> {
    let code = r#"{
        let next = never<i64>();
        let runs = 0;
        let t = select (any(0, next) ~ runs) { k if k < 3 => k, _ => never() };
        let base = 0;
        let bump = |u| { let n = base; n <- u ~ n + 1; u ~ n * 10 + count(u) };
        let total = 0;
        let add = |u| { total <- u ~ total + 1; u ~ total };
        let r = seq t {
            let n = runs + 1;
            runs <- n;
            let a = bump(n);
            let b = add(n);
            n * 1000 + a * 10 + b
        };
        next <- r ~ 1;
        r
    }"#;
    let (values, _) = run_delta(code, mode).await?;
    assert_eq!(as_i64s(&values), [1010, 2011, 3012]);
    Ok(())
}

// Outside a seq a call is not born again: the arm holding it resumes, and
// the state its callee keeps goes on from where it paused.
async fn a_call_outside_a_seq_resumes(mode: Mode) -> Result<()> {
    let code = r#"{
        let step = 0;
        step <- select step { n if n < 40 => n + 1, _ => never() };
        let phase = select step {
            k if k < 10 => `A,
            k if k < 20 => `B,
            k if k < 30 => `A,
            _ => `B
        };
        let base = 0;
        let bump = |u| { let n = base; n <- u ~ n + 1; n };
        select phase { `A => bump(step), `B => never() }
    }"#;
    let (values, _) = run_delta(code, mode).await?;
    assert_eq!(as_i64s(&values), (0..20).collect::<Vec<i64>>());
    Ok(())
}

// A call issued through a function value that is bottom at the issue is
// the run's fresh frame when the function returns, a cycle later: the
// callee reads the file this run wrote and its own `count` starts afresh,
// a call inside it through such a function included (t-seq-03).
async fn a_callee_bottom_at_the_issue_is_born_when_it_returns(mode: Mode) -> Result<()> {
    let dir = tempfile::tempdir()?;
    let path = escape_path(dir.path().join("f.txt").display());
    for (call, expect) in [
        ("g(n)", [11, 21, 31]),
        ("h(n)", [11, 21, 31]),
        ("cast<i64>(load()$)$", [11, 22, 33]),
    ] {
        let code = r#"{
            let path = "PATH";
            let next = never<i64>();
            let runs = 0;
            let t = select (any(0, next) ~ runs) { k if k < 3 => k, _ => never() };
            let on = true;
            on <- select on { false => true, true => never() };
            let g = select on { true => |u: i64| u ~ count(u), false => never() };
            let h = |u: i64| g(u);
            let load = select on { true => || sys::fs::read_all(path), false => never() };
            let r = seq t {
                let n = runs + 1;
                runs <- n;
                sys::fs::write_all(#path: path, "[n]")$;
                on <- false;
                until live(!on);
                let c = CALL;
                n * 10 + c
            };
            next <- r ~ 1;
            r
        }"#
        .replace("PATH", &path)
        .replace("CALL", call);
        let (values, _) = run_delta(&code, mode).await?;
        assert_eq!(as_i64s(&values), expect, "{call}");
    }
    Ok(())
}

// A callee that returns an event it captures answers the event as it
// stands at each run, its last fire, as a fresh call reads it; it does not
// wait for the next fire (t-seq-03).
async fn a_callee_answers_a_captured_event_as_it_stands(mode: Mode) -> Result<()> {
    let code = r#"{
        let step = 0;
        step <- select step { n if n < 40 => n + 1, _ => never() };
        let ev = never<i64>();
        ev <- select step { 5 => 50, 15 => 150, _ => never() };
        let go = select step { 10 => 1, 20 => 2, 30 => 3, _ => never() };
        let wait = |u| ev;
        seq let m = go { let a = wait(m); m * 1000 + a }
    }"#;
    let (values, _) = run_delta(code, mode).await?;
    assert_eq!(as_i64s(&values), [1050, 2150, 3150]);
    Ok(())
}

modes!(
    a_callee_reads_its_world_fresh_each_run,
    a_steps_rpc_call_answers_its_own_run,
    a_steps_callq_answers_its_own_run,
    a_callees_own_state_starts_fresh_each_run,
    a_call_outside_a_seq_resumes,
    a_callee_bottom_at_the_issue_is_born_when_it_returns,
    a_callee_answers_a_captured_event_as_it_stands,
);
