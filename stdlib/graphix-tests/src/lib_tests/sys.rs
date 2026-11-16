use anyhow::Result;
use arcstr::{ArcStr, literal};
use graphix_package_core::{
    ExitRequest, PrintSink, ProgramArgs, run,
    testing::{self, FuseExpect, HANG_GUARD, Mode, is},
};
use graphix_rt::GXEvent;
use netidx::publisher::Value;
use tokio::time::timeout;

const ARGS_EMPTY: &str = r#"
    sys::args()
"#;

run!(args_empty, ARGS_EMPTY, |v: Result<&Value>| match v {
    Ok(Value::Array(a)) => a.is_empty(),
    _ => false,
}; FuseExpect::None);
// None: sys::args is once-latched state.

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

// Stdin does not write: a write to it is a type error.
const STDIN_WRITE_ERR: &str = r#"
{
    use sys::io::Write;
    let inp = sys::io::stdin(null);
    Write::write_exact(inp, buffer::from_string("nope"))
}
"#;

run!(
    stdin_write_err,
    STDIN_WRITE_ERR,
    graphix_package_core::testing::refused("Stdin does not implement Write");
    FuseExpect::None
);

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
    timeout <- once(timeout) ~ -1.0;
    let result = sys::time::after_idle(timeout, 7);
    result ~ (result == 7 && timeout == -1.0)
}
"#;

run!(after_idle_invalid_timeout, AFTER_IDLE_INVALID_TIMEOUT, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
}; FuseExpect::None);

// CR claude for claude: [test-gap/low] handle_accessors_are_sync and
// born_builtins_and_parse_are_sync carry `; build_only`, the `run!` form for a refusal
// only a build makes. But the check and the build both accept them, so the exemption
// only drops the interp run's `testing::verdicts_agree` assertion. A check that began
// refusing a `#[sync]` lambda, `sys::dirs::config_dir()` or `args::parse(never())`
// would pass here. Remove `; build_only` from both. probe:
// design/review-2026-11-16/repro/tests-lib-b-c-build-only-accepted.sh
// (tests-lib-b-c-06)
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
}"#, is(Value::Bool(true)); FuseExpect::Jit; build_only);

run!(born_builtins_and_parse_are_sync, r#"{
    #[sync]
    let cfg = |name: string| "[sys::dirs::config_dir()]/[name]";
    #[sync]
    let argv = |n: i64| (n, sys::args());
    #[sync]
    let parse = |c: args::Command| args::parse(c);
    (cfg("x.toml"), argv(1), parse(never()));
    true
}"#, is(Value::Bool(true)); FuseExpect::None; build_only);

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
}"#, is(Value::Bool(true)); FuseExpect::None);

// sys::exit under an embedder that seeded an ExitRequest and stopped
// listening ends the program and not the process, which is the
// embedder's. The program runs in a child of this binary, which a process
// exit would end with the program's code (fuzz-lib-b-01).
#[tokio::test(flavor = "current_thread")]
async fn exit_without_a_listener_keeps_the_process() -> Result<()> {
    const CHILD: &str = "GRAPHIX_TEST_EXIT_CHILD";
    const NAME: &str = "lib_tests::sys::exit_without_a_listener_keeps_the_process";
    if std::env::var_os(CHILD).is_some() {
        let (v, ctx) = testing::eval_with_setup(
            "{ let x = 3; sys::exit(x); x }",
            &crate::TEST_REGISTER,
            |ctx| {
                ctx.libstate.set(ExitRequest::new().0);
            },
        )
        .await?;
        assert_eq!(v, Value::I64(3));
        ctx.shutdown().await;
        return Ok(());
    }
    let out = std::process::Command::new(std::env::current_exe()?)
        .args([NAME, "--exact", "--include-ignored"])
        .env(CHILD, "1")
        .output()?;
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert!(
        out.status.success() && stdout.contains("1 passed"),
        "{:?}\n{stdout}",
        out.status
    );
    Ok(())
}

// sys::exit under an embedder that seeded no ExitRequest ends the
// process after the cycle that asked, which runs whole: what its walk
// reaches after the exit prints. The program runs in a child of this
// binary, which the process exit ends with the program's code.
#[tokio::test(flavor = "current_thread")]
async fn exit_without_a_request_ends_the_process_after_its_cycle() -> Result<()> {
    const CHILD: &str = "GRAPHIX_TEST_EXIT_PROCESS_CHILD";
    const NAME: &str =
        "lib_tests::sys::exit_without_a_request_ends_the_process_after_its_cycle";
    if std::env::var_os(CHILD).is_some() {
        let _ctx = testing::eval(
            r#"{
                let n = 0;
                n <- n + 1;
                sys::exit(select n { m if m == 20 => 7, _ => never() });
                println("n=[n]");
                n
            }"#,
            &crate::TEST_REGISTER,
        )
        .await?;
        tokio::time::sleep(HANG_GUARD).await;
        std::process::exit(99)
    }
    let out = std::process::Command::new(std::env::current_exe()?)
        .args([NAME, "--exact", "--include-ignored", "--nocapture"])
        .env(CHILD, "1")
        .output()?;
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert_eq!(out.status.code(), Some(7), "{:?}\n{stdout}", out.status);
    assert_eq!(
        stdout.lines().filter(|l| l.starts_with("n=")).last(),
        Some("n=20"),
        "{stdout}"
    );
    Ok(())
}

// sys::exit ends the program in the cycle that asks. That cycle runs
// whole, what its walk reaches after the exit included, and its batch is
// delivered; no cycle runs after it, so a counter that runs flat out stops
// there. The runtime then answers its embedder as an idle one, and what the
// embedder compiles never runs.
#[tokio::test(flavor = "current_thread")]
async fn exit_is_the_programs_last_cycle() -> Result<()> {
    const LAST: i64 = 20;
    const CODE: &str = r#"{
        let n = 0;
        n <- n + 1;
        sys::exit(select n { m if m == 20 => m - 20, _ => never() });
        let twice = array::map([n, n], |x| x * 2);
        println("[n] [twice]");
        n
    }"#;
    for mode in Mode::ALL {
        let sink = PrintSink::default();
        let (exit, asked) = ExitRequest::new();
        let seeded = sink.clone();
        let (ctx, mut rx) = testing::fixture_runtime(
            [("/test.gx", testing::result_source(CODE))],
            &crate::TEST_REGISTER,
            mode,
            move |ctx| {
                ctx.libstate.set(seeded);
                ctx.libstate.set(exit);
            },
        )
        .await?;
        let res = testing::compile_result(&ctx).await?;
        assert_eq!(timeout(HANG_GUARD, asked).await??, 0, "{mode:?}");
        // answered once the cycle that asked has run
        ctx.rt.compile(literal!("println(\"late\")")).await?;
        ctx.rt.get_env().await?;
        let printed: String =
            (0..=LAST).map(|n| format!("{n} [{}, {}]\n", n * 2, n * 2)).collect();
        assert_eq!(sink.take(), printed, "{mode:?}");
        timeout(HANG_GUARD, ctx.rt.wait_idle()).await??;
        let mut values = vec![];
        while let Ok(mut batch) = rx.try_recv() {
            values.extend(batch.drain(..).filter_map(|e| match e {
                GXEvent::Updated(id, v) if id == res.exprs[0].id => Some(v),
                _ => None,
            }))
        }
        let counted: Vec<Value> = (0..=LAST).map(Value::I64).collect();
        assert_eq!(values, counted, "{mode:?}");
        assert_eq!(sink.take(), "", "{mode:?}");
        ctx.shutdown().await;
    }
    Ok(())
}
