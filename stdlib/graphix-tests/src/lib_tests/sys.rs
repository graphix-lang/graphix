use anyhow::Result;
use arcstr::ArcStr;
use graphix_package_core::{ProgramArgs, run, testing, testing::FuseExpect};
use netidx::publisher::Value;

const ARGS_EMPTY: &str = r#"
    sys::args()
"#;

run!(args_empty, ARGS_EMPTY, |v: Result<&Value>| match v {
    Ok(Value::Array(a)) => a.is_empty(),
    _ => false,
}; FuseExpect::None);
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
}; FuseExpect::Jit);

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
}; FuseExpect::Jit);

// stdin produces a stream (no data can be fed in a test).
const STDIN_CREATE: &str = r#"
  sys::io::stdin(null) ~ true
"#;

run!(stdin_create, STDIN_CREATE, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
}; FuseExpect::None);

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
}; FuseExpect::None);

const WRITE_EXACT_ZERO: &str = r#"
{
    use sys::io::Write;
    type Sink = Abstract<null>;
    impl Write for Sink {
        let write = |s, data| {
            let result: Result<u64, `IOError(string)> = select count((s, data)) {
                1 => u64:0,
                _ => error(`IOError("write retried after returning zero"))
            };
            result
        };
        let flush = |s| {
            let result: Result<null, `IOError(string)> = s ~ null;
            result
        }
    };
    let sink = Sink(null);
    let empty = Write::write_exact(sink, buffer::from_string(""));
    let full = Write::write_exact(sink, buffer::from_string("hello"));
    let result = never();
    catch(e) select (e.0).error {
        `IOError(message) => result <- empty ~ (message == "write returned 0 bytes")
    };
    full?;
    result
}
"#;

run!(write_exact_zero, WRITE_EXACT_ZERO, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
}; FuseExpect::Jit);

const AFTER_IDLE_INVALID_TIMEOUT: &str = r#"
{
    let timeout = 0.05;
    timeout <- sys::time::after_idle(duration:10.ms, -1.0);
    let result = sys::time::after_idle(timeout, 7);
    result ~ (result == 7 && timeout == -1.0)
}
"#;

run!(after_idle_invalid_timeout, AFTER_IDLE_INVALID_TIMEOUT, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
}, timeout: 2; FuseExpect::None);

run!(handle_accessors_are_sync, r#"{
    #[sync]
    let peer = |s: sys::tcp::TcpStream| sys::tcp::Socket::peer_addr(s);
    #[sync]
    let local = |s: sys::tcp::TcpStream| sys::tcp::Socket::local_addr(s);
    #[sync]
    let listener = |l: sys::tcp::TcpListener| sys::tcp::listener_addr(l);
    #[sync]
    let pid = |p: sys::process::Proc| sys::process::pid(p);
    (peer(never()), local(never()), listener(never()), pid(never()));
    true
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Bool(true))); FuseExpect::Jit; build_only);

run!(stdio_constructors_are_sync, r#"{
    #[sync]
    let input = |x| sys::io::stdin(x);
    #[sync]
    let output = |x| sys::io::stdout(x);
    #[sync]
    let error = |x| sys::io::stderr(x);
    input(null) != input(null)
        && output(null) != output(null)
        && error(null) != error(null)
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Bool(true))); FuseExpect::None);
