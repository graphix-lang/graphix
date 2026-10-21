// Fixtures over builtins only a test registers: Rust that panics, a
// function value retyped past the checker, and a builtin's quantifier no
// argument can carry a value of.

use super::dense_deltas::run_delta_with;
use anyhow::Result;
use graphix_package_core::{
    fast_builtin,
    testing::{Mode, TestState},
};
use netidx_value::Value;
use std::time::Duration;

fn fc_panic_at_13(args: &[Value]) -> Option<Value> {
    match &args[0] {
        Value::I64(13) => panic!("thirteen"),
        v => Some(v.clone()),
    }
}

fast_builtin!(PanicAt13, PanicAt13Ev, "test_panic_at_13", fc_panic_at_13);

fn fc_identity(args: &[Value]) -> Option<Value> {
    Some(args[0].clone())
}

fast_builtin!(Identity, IdentityEv, "test_identity", fc_identity);

fn fc_last(args: &[Value]) -> Option<Value> {
    args.last().cloned()
}

fast_builtin!(Last, LastEv, "test_last", fc_last);

async fn run(code: &str, mode: Mode) -> Result<Vec<Value>> {
    let setup = |ctx: &mut TestState| {
        ctx.register_builtin::<PanicAt13>().unwrap();
        ctx.register_builtin::<Identity>().unwrap();
        ctx.register_builtin::<Last>().unwrap();
    };
    Ok(run_delta_with(code, mode, setup, Duration::from_millis(700)).await?.values)
}

/// Each program panics at n = 13 inside `#[native]`: the runtime dies
/// as the node-walk's unwind kills it, and the process lives.
async fn rust_panics_in_a_kernel(mode: Mode) -> Result<()> {
    let head = r#"
        let boom = |x: i64| -> i64 'test_panic_at_13;
        type K = Abstract<i64>;
        impl Eq for K { let eq = |a, b| boom(a.0) == boom(b.0) };
        impl Ord for K {
            let cmp = |a, b| select boom(a.0) < boom(b.0) { true => `Less, false => `Greater }
        };
        impl Display for K { let fmt = |k| "[boom(k.0)]" };
        let n = 10;
        n <- select n { k if k < 20 => k + 1, _ => never() };"#;
    for body in [
        "#[native] (boom(n) + 1)",
        "#[native] ([K(n)] == [K(1)])",
        "#[native] map::len({K(n) => 1, K(1) => 2})",
        "#[native] str::len(\"[K(n)]\")",
    ] {
        let r = run(&format!("{{ {head} {body} }}"), mode).await;
        match r {
            Err(e) if e.to_string().contains("the runtime died") => (),
            r => panic!("{body}: {r:?}"),
        }
    }
    Ok(())
}

/// A function value whose type the checker was told wrong reaches a
/// dynamic call site: the site refuses the instance and never runs it.
async fn mistyped_function_value_is_refused(mode: Mode) -> Result<()> {
    let code = |f: &str| {
        format!(
            r#"{{
                let launder = |f: Any| -> fn(x: f64) -> f64 'test_identity;
                let g = launder({f});
                g(1.5)
            }}"#
        )
    };
    let good = run(&code("|x: f64| x + 1.0"), mode).await?;
    assert_eq!(good, [Value::F64(2.5)]);
    let bad = run(&code("|x: i64| x + 1"), mode).await?;
    assert_eq!(bad, []);
    Ok(())
}

/// A builtin's `'a: Concrete` that only a handler the call does not pass
/// holds is never read: the call is accepted with the handler omitted or
/// null, and refused where a value of `'a` can arrive and nothing settles
/// it (a handler whose argument is left open) or the result holds it.
async fn unread_quantifier_settles(mode: Mode) -> Result<()> {
    let code = |ret: &str, call: &str| {
        format!(
            r#"{{
                let f = 'a: Concrete |#h: [fn(v: 'a) -> i64, null] = null, x: i64|
                    -> {ret} 'test_last;
                {call}
            }}"#
        )
    };
    assert_eq!(run(&code("i64", "f(3)"), mode).await?, [Value::I64(3)]);
    assert_eq!(run(&code("i64", "f(#h: null, 3)"), mode).await?, [Value::I64(3)]);
    for (ret, call) in [
        ("i64", "f(#h: |v| 0, 3)"),
        ("['a, i64]", "f(3)"),
        (
            "i64",
            "{ let g = 'b: Concrete |#h: [fn(v: 'b) -> i64, null] = null, x: i64| x; g(3) }",
        ),
    ] {
        match run(&code(ret, call), mode).await {
            Err(e) if format!("{e:#}").contains("must be fully known") => (),
            r => panic!("{ret} {call}: {r:?}"),
        }
    }
    Ok(())
}

modes!(
    rust_panics_in_a_kernel,
    mistyped_function_value_is_refused,
    unread_quantifier_settles
);
