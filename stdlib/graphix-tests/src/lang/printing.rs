use super::dense_deltas::{as_i64s, run_delta};
use anyhow::Result;
use netidx_value::Value;

async fn acknowledgements(fusion_disabled: bool) -> Result<()> {
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
        let (values, out) = run_delta(&code, fusion_disabled).await?;
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

#[tokio::test(flavor = "current_thread")]
async fn acknowledgements_interp() -> Result<()> {
    acknowledgements(true).await
}

#[tokio::test(flavor = "current_thread")]
async fn acknowledgements_jit() -> Result<()> {
    acknowledgements(false).await
}

/// Steps print in order; a block's statements are issued together, so
/// within one run its lines are unordered.
async fn seq_printing(fusion_disabled: bool) -> Result<()> {
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
        let (values, out) = run_delta(&code, fusion_disabled).await?;
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

#[tokio::test(flavor = "current_thread")]
async fn seq_printing_interp() -> Result<()> {
    seq_printing(true).await
}

#[tokio::test(flavor = "current_thread")]
async fn seq_printing_jit() -> Result<()> {
    seq_printing(false).await
}

async fn seq_printing_waits_for_message(fusion_disabled: bool) -> Result<()> {
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
    let (values, out) = run_delta(code, fusion_disabled).await?;
    assert_eq!(values, [Value::I64(6)]);
    assert_eq!(out, "ready\n");
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn seq_printing_waits_for_message_interp() -> Result<()> {
    seq_printing_waits_for_message(true).await
}

#[tokio::test(flavor = "current_thread")]
async fn seq_printing_waits_for_message_jit() -> Result<()> {
    seq_printing_waits_for_message(false).await
}
