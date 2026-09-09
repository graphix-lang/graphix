use anyhow::Result;
use arcstr::ArcStr;
use graphix_package_core::{ProgramArgs, run, testing};
use netidx::publisher::Value;

const ARGS_EMPTY: &str = r#"
    sys::args()
"#;

run!(args_empty, ARGS_EMPTY, |v: Result<&Value>| match v {
    Ok(Value::Array(a)) => a.is_empty(),
    _ => false,
}; graphix_package_core::testing::FuseExpect::None);
// None: sys::args is once-latched, so Async.

#[tokio::test(flavor = "current_thread")]
async fn args_injected() -> Result<()> {
    let code = r#"sys::args()"#;
    let (v, ctx) = testing::eval_with_setup(code, &crate::TEST_REGISTER, |ctx| {
        ctx.libstate.set(ProgramArgs(vec![
            ArcStr::from("script.gx"),
            ArcStr::from("--port"),
            ArcStr::from("8080"),
        ]));
    })
    .await?;
    match &v {
        Value::Array(a) => {
            assert_eq!(a.len(), 3);
            assert_eq!(a[0], Value::String(ArcStr::from("script.gx")));
            assert_eq!(a[1], Value::String(ArcStr::from("--port")));
            assert_eq!(a[2], Value::String(ArcStr::from("8080")));
        }
        other => panic!("expected Array, got {other:?}"),
    }
    ctx.shutdown().await;
    Ok(())
}

const STDOUT_WRITE: &str = r#"
{
    use sys::io::Write;
    let out = sys::io::stdout(null);
    let written = Write::write_exact(out, buffer::from_string("hello stdout\n"));
    let flushed = Write::flush(written? ~ out);
    !is_err(flushed)
}
"#;

run!(stdout_write, STDOUT_WRITE, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
}; graphix_package_core::testing::FuseExpect::Jit);

const STDERR_WRITE: &str = r#"
{
    use sys::io::Write;
    let err = sys::io::stderr(null);
    let written = Write::write_exact(err, buffer::from_string("hello stderr\n"));
    let flushed = Write::flush(written? ~ err);
    !is_err(flushed)
}
"#;

run!(stderr_write, STDERR_WRITE, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
}; graphix_package_core::testing::FuseExpect::Jit);

// stdin is a valid stream (no data can be fed in a test).
const STDIN_CREATE: &str = r#"
{
    let inp = sys::io::stdin(null);
    !is_err(inp)
}
"#;

run!(stdin_create, STDIN_CREATE, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
}; graphix_package_core::testing::FuseExpect::Jit);

// Writing to stdin is an error.
const STDIN_WRITE_ERR: &str = r#"
{
    use sys::io::Write;
    let inp = sys::io::stdin(null);
    Write::write_exact(inp, buffer::from_string("nope"))
}
"#;

run!(stdin_write_err, STDIN_WRITE_ERR, |v: Result<&Value>| {
    matches!(v, Ok(Value::Error(_)))
}; graphix_package_core::testing::FuseExpect::None);
