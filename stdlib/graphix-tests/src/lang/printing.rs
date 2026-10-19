use super::dense_deltas::run_delta;
use anyhow::Result;
use graphix_package_core::testing::{Mode, as_i64s};
use netidx_value::{ValArray, Value};

async fn acknowledgements(mode: Mode) -> Result<()> {
    for builtin in ["print", "println", "log"] {
        let code = format!(
            r#"{{
                let step = 0;
                step <- select step {{ s if s < 10 => s + 1, _ => never() }};
                let msg = never<string>();
                msg <- select step {{ 2 | 5 => step ~ "message", _ => never() }};
                let dest = select step {{ s if s < 4 => `Stdout, _ => `Stderr }};
                let done: null = {builtin}(#dest: dest, msg);
                done ~ step
            }}"#
        );
        let (values, out) = run_delta(&code, mode).await?;
        assert_eq!(as_i64s(&values), [3, 6], "{builtin}: {out}");
        match builtin {
            "print" => assert_eq!(out, "messagemessage"),
            "println" => assert_eq!(out, "message\nmessage\n"),
            "log" => {
                let lines: Vec<_> = out.lines().collect();
                assert_eq!(lines.len(), 2, "{out}");
                assert!(lines.iter().all(|s| s.ends_with(r#": "message""#)), "{out}");
            }
            _ => unreachable!(),
        }
    }
    Ok(())
}

modes!(acknowledgements);

/// Steps print in order; a block's statements are issued together, so
/// within one run its lines are unordered.
async fn seq_printing(mode: Mode) -> Result<()> {
    for (body, ordered) in [
        (r#"print("a"); println("b"); log("c"); go"#, true),
        (r#"{ println("a"); println("b"); log("c"); go }"#, false),
    ] {
        let code = format!(
            r#"{{
                let step = 0;
                step <- select step {{ s if s < 20 => s + 1, _ => never() }};
                let go = select step {{ 1 | 10 => step, _ => never() }};
                seq go {{ {body} }}
            }}"#
        );
        let (values, out) = run_delta(&code, mode).await?;
        assert_eq!(as_i64s(&values), [1, 10], "{body}: {out}");
        let lines: Vec<_> = out.lines().collect();
        let per_run = if ordered { 2 } else { 3 };
        assert_eq!(lines.len(), 2 * per_run, "{out}");
        for mut run in lines.chunks(per_run).map(|r| r.to_vec()) {
            if !ordered {
                run.sort();
            }
            let n = run.len();
            assert_eq!(
                &run[..n - 1],
                if ordered { &["ab"][..] } else { &["a", "b"] },
                "{out}"
            );
            assert!(run[n - 1].ends_with(r#": "c""#), "{out}");
        }
    }
    Ok(())
}

modes!(seq_printing);

async fn seq_printing_waits_for_message(mode: Mode) -> Result<()> {
    let code = r#"{
        let step = 0;
        step <- select step { s if s < 12 => s + 1, _ => never() };
        let msg = never<string>();
        msg <- select step { 5 => "ready", _ => never() };
        let say = |s: string| -> null println(s);
        seq {
            {
                say(msg);
                step
            }
        }
    }"#;
    let (values, out) = run_delta(code, mode).await?;
    assert_eq!(values, [Value::I64(6)]);
    assert_eq!(out, "ready\n");
    Ok(())
}

modes!(seq_printing_waits_for_message);

/// The lines `out` holds, sorted: forked slots print unordered.
fn sorted_lines(out: &str) -> Vec<&str> {
    let mut lines: Vec<_> = out.lines().collect();
    lines.sort();
    lines
}

/// A printer acts at each fire of its message, never again when its arm
/// is re-entered with the message standing.
async fn printers_act_once_per_fire(mode: Mode) -> Result<()> {
    let code = r#"{
        let step = 0;
        step <- select step { s if s < 8 => s + 1, _ => never() };
        let c = select step { 1 | 3 | 5 => true, _ => false };
        let x = 5;
        let y = "P5";
        let a = select c { true => dbg(#dest: `Stdout, x), false => 0 };
        let b = select c { true => { log(#dest: `Stdout, x); 1 }, false => 0 };
        let d = select c { true => { println(y); 2 }, false => 0 };
        (a, b, d) ~ step
    }"#;
    let (_, out) = run_delta(code, mode).await?;
    let lines = sorted_lines(&out);
    assert_eq!(lines.len(), 3, "{out}");
    assert_eq!(lines.iter().filter(|l| l.ends_with("dbg(x): 5")).count(), 1, "{out}");
    assert_eq!(
        lines.iter().filter(|l| !l.contains("dbg") && l.ends_with(": 5")).count(),
        1,
        "{out}"
    );
    assert!(lines.contains(&"P5"), "{out}");
    Ok(())
}

modes!(printers_act_once_per_fire);

/// `now` reads the clock at each fire of its trigger, never at an arm's
/// re-entry with the trigger standing.
async fn now_samples_once_per_trigger(mode: Mode) -> Result<()> {
    let code = r#"{
        let step = 0;
        step <- select step { s if s < 8 => s + 1, _ => never() };
        let c = select step { 1 | 3 | 5 => true, _ => false };
        let x = 5;
        select c {
            true => sys::time::now(x),
            false => datetime:"2000-01-01T00:00:00Z"
        }
    }"#;
    let (values, _) = run_delta(code, mode).await?;
    let unset = values[0].clone();
    let sampled: Vec<&Value> = values.iter().filter(|v| **v != unset).collect();
    assert_eq!(sampled.len(), 3, "{values:?}");
    assert!(sampled.iter().all(|v| *v == sampled[0]), "{values:?}");
    Ok(())
}

modes!(now_samples_once_per_trigger);

/// A tail recursion that prints is one native loop.
async fn printing_tail_loop_is_native(mode: Mode) -> Result<()> {
    let code = r#"{
        #[tail_recursive]
        let rec f = |n: i64, acc: i64| -> i64 #[native] select n {
            0 => acc,
            _ => {
                println(n);
                f(n - 1, acc + n)
            }
        };
        f(3, 0)
    }"#;
    let (values, out) = run_delta(code, mode).await?;
    assert_eq!(values, [Value::I64(6)]);
    assert_eq!(out, "3\n2\n1\n");
    Ok(())
}

modes!(printing_tail_loop_is_native);

async fn dbg_fuses_under_native(mode: Mode) -> Result<()> {
    let code = r#"{
        let xs = [1, 2, 3];
        #[native] array::map(xs, |x| dbg(#dest: `Stdout, x * 2) + 1)
    }"#;
    let (values, out) = run_delta(code, mode).await?;
    let mapped = Value::Array(ValArray::from_iter([3, 5, 7].map(Value::I64)));
    assert_eq!(values, [mapped]);
    let lines = sorted_lines(&out);
    assert_eq!(lines.len(), 3, "{out}");
    for (line, v) in lines.iter().zip(["2", "4", "6"]) {
        assert!(line.ends_with(&format!("dbg(x * 2): {v}")), "{out}");
    }
    Ok(())
}

modes!(dbg_fuses_under_native);

/// A change of `#dest` alone neither prints nor fires, fused or not.
async fn dest_alone_neither_prints_nor_fires(mode: Mode) -> Result<()> {
    let code = r#"{
        let step = 0;
        step <- select step { s if s < 6 => s + 1, _ => never() };
        let d = select step { s if s < 3 => `Stdout, _ => `Stderr };
        let m = 7;
        #[native] dbg(#dest: d, m)
    }"#;
    let (values, out) = run_delta(code, mode).await?;
    assert_eq!(values, [Value::I64(7)]);
    assert_eq!(out.lines().count(), 1, "{out}");
    assert!(out.ends_with("dbg(m): 7\n"), "{out}");
    Ok(())
}

modes!(dest_alone_neither_prints_nor_fires);

/// A constant message prints at its arm's birth only, fused or not; one
/// sampled on the scrutinee's delivery prints at every entry.
async fn arm_entry_prints_a_constant(mode: Mode) -> Result<()> {
    for (message, expect) in [(r#""E""#, "E\n"), (r#"c ~ "E""#, "E\nE\nE\n")] {
        let code = format!(
            r#"{{
            let step = 0;
            step <- select step {{ s if s < 8 => s + 1, _ => never() }};
            let c = select step {{ 1 | 3 | 5 => true, _ => false }};
            let r = select c {{ true => {{ println({message}); 1 }}, false => 0 }};
            r ~ step
        }}"#
        );
        let (_, out) = run_delta(&code, mode).await?;
        assert_eq!(out, expect, "{message}");
    }
    Ok(())
}

modes!(arm_entry_prints_a_constant);

// A print under a select arm whose message is a constant prints at the
// arm's birth only, fused or not: a later entry fires its trigger only
// through the scrutinee or the arm's binds, never through an untaken
// nested arm, an argument a function ignores, or a callee's own constant.
async fn arm_entry_prints_every_time(mode: Mode) -> Result<()> {
    for (code, prints) in [
        (
            r#"{
                let step = 0;
                step <- select step { s if s < 8 => s + 1, _ => never() };
                let f = |x: i64, y: i64| select x {
                    0 => dbg(#dest: `Stdout, select y { 0 => "zero", _ => "[x]" }),
                    _ => "other"
                };
                f(step % 2, 0)
            }"#,
            1,
        ),
        (
            r#"{
                let step = 0;
                step <- select step { s if s < 8 => s + 1, _ => never() };
                let p = |x: i64| { println("hi"); x + 1 };
                let f = |s: i64| select s { 0 => p(s), _ => -1 };
                f(step % 2)
            }"#,
            1,
        ),
    ] {
        let (_, out) = run_delta(code, mode).await?;
        let (_, walked) = run_delta(code, Mode::Interp).await?;
        assert_eq!(out, walked, "{code}");
        assert_eq!(out.lines().count(), prints, "{code}\n{out}");
    }
    Ok(())
}

modes!(arm_entry_prints_every_time);
