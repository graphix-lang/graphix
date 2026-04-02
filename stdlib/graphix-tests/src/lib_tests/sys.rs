use anyhow::Result;
use arcstr::ArcStr;
use graphix_package_core::{run, testing, ProgramArgs};
use netidx::publisher::Value;

const ARGS_EMPTY: &str = r#"
    sys::args()
"#;

run!(args_empty, ARGS_EMPTY, |v: Result<&Value>| match v {
    Ok(Value::Array(a)) => a.is_empty(),
    _ => false,
});

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

// stdout: write and flush succeed
const STDOUT_WRITE: &str = r#"
{
    let out = sys::io::stdout(null);
    let written = sys::io::write_exact(out, buffer::from_string("hello stdout\n"));
    let flushed = sys::io::flush(written? ~ out);
    !is_err(flushed)
}
"#;

run!(stdout_write, STDOUT_WRITE, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
});

// stderr: write and flush succeed
const STDERR_WRITE: &str = r#"
{
    let err = sys::io::stderr(null);
    let written = sys::io::write_exact(err, buffer::from_string("hello stderr\n"));
    let flushed = sys::io::flush(written? ~ err);
    !is_err(flushed)
}
"#;

run!(stderr_write, STDERR_WRITE, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
});

// stdin: can be created (we can't feed data in a test, but verify it's a valid stream)
const STDIN_CREATE: &str = r#"
{
    let inp = sys::io::stdin(null);
    !is_err(inp)
}
"#;

run!(stdin_create, STDIN_CREATE, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
});

// writing to stdin returns an error
const STDIN_WRITE_ERR: &str = r#"
{
    let inp = sys::io::stdin(null);
    sys::io::write_exact(inp, buffer::from_string("nope"))
}
"#;

run!(stdin_write_err, STDIN_WRITE_ERR, |v: Result<&Value>| {
    matches!(v, Ok(Value::Error(_)))
});
