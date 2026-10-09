// Fixtures over builtins only a test registers: Rust that panics, and a
// function value retyped past the checker.

use anyhow::Result;
use graphix_package_core::{
    fast_builtin,
    testing::{
        Mode, compile_result, fixture_runtime, result_source, updates_until_quiet,
    },
};
use netidx_value::Value;
use std::time::Duration;
use tokio::time::Instant;

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

async fn run(code: &str, mode: Mode) -> Result<Vec<Value>> {
    let (ctx, mut rx) = fixture_runtime(
        [("/test.gx", result_source(code))],
        &crate::TEST_REGISTER,
        mode,
        |ctx| {
            ctx.register_builtin::<PanicAt13>().unwrap();
            ctx.register_builtin::<Identity>().unwrap();
        },
    )
    .await?;
    let res = compile_result(&ctx).await?;
    let deadline = Instant::now() + Duration::from_secs(20);
    let values = updates_until_quiet(
        &mut rx,
        res.exprs[0].id,
        Duration::from_millis(700),
        deadline,
    )
    .await?;
    ctx.shutdown().await;
    Ok(values)
}

/// Each program panics at n = 13 inside `#[native]`: the runtime dies
/// as the node-walk's unwind kills it, and the process lives.
async fn rust_panics_in_a_kernel(mode: Mode) -> Result<()> {
    let head = r#"
        let boom = |x: i64| -> i64 'test_panic_at_13;
        type K = Abstract<i64>;
        impl Eq for K { let eq = |a, b| boom(a.0) == boom(b.0) };
        let n = 10;
        n <- select n { k if k < 20 => k + 1, _ => never() };"#;
    for body in ["#[native] (boom(n) + 1)", "#[native] ([K(n)] == [K(1)])"] {
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

modes!(rust_panics_in_a_kernel, mistyped_function_value_is_refused);
