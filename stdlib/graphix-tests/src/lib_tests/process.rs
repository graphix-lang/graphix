use anyhow::Result;
use graphix_package_core::{run, testing::FuseExpect};
use netidx::subscriber::Value;

#[cfg(unix)]
const PROCESS_STDOUT_PIPE: &str = r#"
{
  use sys::io::Read;
  use opt;
  let options = sys::process::options(
    #args: ["-c", "printf hello"],
    #stdio: sys::process::stdio(#stdout: `Pipe, #stderr: `Inherit),
    #kill_on_drop: true,
    "/bin/sh"
  );
  let child = sys::process::spawn(options)?;
  let stdout = opt::ok_or(child.stdout, `Null("stdout"))?;
  let out = buffer::to_string(Read::read(stdout, u64:1024)?)?;
  out
}
"#;

#[cfg(windows)]
const PROCESS_STDOUT_PIPE: &str = r#"
{
  use sys::io::Read;
  use opt;
  let options = sys::process::options(
    #args: ["/C", "<nul set /p =hello"],
    #stdio: sys::process::stdio(#stdout: `Pipe, #stderr: `Inherit),
    #kill_on_drop: true,
    "cmd.exe"
  );
  let child = sys::process::spawn(options)?;
  let stdout = opt::ok_or(child.stdout, `Null("stdout"))?;
  buffer::to_string(Read::read(stdout, u64:1024)?)?
}
"#;

run!(process_stdout_pipe, PROCESS_STDOUT_PIPE, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "hello")
}; FuseExpect::Jit);

#[cfg(unix)]
const PROCESS_STDIN_PIPE: &str = r#"
{
  use sys::io::{Read, Write};
  use opt;
  let options = sys::process::options(
    #args: ["-c", "cat"],
    #stdio: sys::process::stdio(#stdin: `Pipe, #stdout: `Pipe, #stderr: `Inherit),
    #kill_on_drop: true,
    "/bin/sh"
  );
  let child = sys::process::spawn(options)?;
  let stdin = opt::ok_or(child.stdin, `Null("stdin"))?;
  let wrote = Write::write_exact(stdin, buffer::from_string("ping"))?;
  let flushed = Write::flush(wrote ~ stdin)?;
  let stdout = opt::ok_or(child.stdout, `Null("stdout"))?;
  let out = buffer::to_string(Read::read(flushed ~ stdout, u64:4)?)?;
  sys::process::kill(out ~ child.proc);
  out
}
"#;

#[cfg(unix)]
run!(process_stdin_pipe, PROCESS_STDIN_PIPE, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "ping")
}; FuseExpect::Jit);

#[cfg(unix)]
const PROCESS_WAIT_STATUS: &str = r#"
{
  let options = sys::process::options(
    #args: ["-c", "exit 7"],
    #kill_on_drop: true,
    "/bin/sh"
  );
  let child = sys::process::spawn(options)?;
  sys::process::wait(child.proc)?
}
"#;

#[cfg(windows)]
const PROCESS_WAIT_STATUS: &str = r#"
{
  let options = sys::process::options(
    #args: ["/C", "exit /B 7"],
    #kill_on_drop: true,
    "cmd.exe"
  );
  let child = sys::process::spawn(options)?;
  sys::process::wait(child.proc)?
}
"#;

run!(process_wait_status, PROCESS_WAIT_STATUS, |v: Result<&Value>| match v {
    Ok(Value::Array(a)) => {
        a.len() == 2
            && matches!(&a[0], Value::Array(pair) if pair.len() == 2 && matches!((&pair[0], &pair[1]), (Value::String(k), Value::I64(7)) if &**k == "code"))
            && matches!(&a[1], Value::Array(pair) if pair.len() == 2 && matches!((&pair[0], &pair[1]), (Value::String(k), Value::Bool(false)) if &**k == "success"))
    }
    _ => false,
}; FuseExpect::Jit);

#[cfg(unix)]
const PROCESS_CONCURRENT_WAIT: &str = r#"
{
  let options = sys::process::options(
    #args: ["-c", "sleep 0.2; exit 3"],
    #kill_on_drop: true,
    "/bin/sh"
  );
  let child = sys::process::spawn(options)?;
  let first = sys::process::wait(child.proc)?;
  let second = sys::process::wait(child.proc)?;
  first.code$ == 3 && second.code$ == 3
}
"#;

#[cfg(unix)]
run!(process_concurrent_wait, PROCESS_CONCURRENT_WAIT, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
}; FuseExpect::Jit);

#[cfg(unix)]
const PROCESS_KILL_DURING_WAIT: &str = r#"
{
  let options = sys::process::options(
    #args: ["-c", "sleep 10"],
    #kill_on_drop: true,
    "/bin/sh"
  );
  let child = sys::process::spawn(options)?;
  let status = sys::process::wait(child.proc)?;
  let killed = sys::process::kill(sys::time::timer(duration:100.ms, false) ~ child.proc);
  !status.success && killed == null
}
"#;

#[cfg(unix)]
run!(process_kill_during_wait, PROCESS_KILL_DURING_WAIT, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
}; FuseExpect::Jit);

#[cfg(unix)]
const PROCESS_STDIN_EOF: &str = r#"
{
  use sys::io::{Read, Write, Close};
  use opt;
  let options = sys::process::options(
    #args: ["-c", "cat"],
    #stdio: sys::process::stdio(#stdin: `Pipe, #stdout: `Pipe, #stderr: `Inherit),
    #kill_on_drop: true,
    "/bin/sh"
  );
  let child = sys::process::spawn(options)?;
  let stdin = opt::ok_or(child.stdin, `Null("stdin"))?;
  let wrote = Write::write_exact(stdin, buffer::from_string("eof-test"))?;
  let closed = Close::close(wrote ~ stdin)?;
  let stdout = opt::ok_or(child.stdout, `Null("stdout"))?;
  let out = buffer::to_string(Read::read(closed ~ stdout, u64:1024)?)?;
  let status = sys::process::wait(out ~ child.proc)?;
  status.success && out == "eof-test"
}
"#;

#[cfg(unix)]
run!(process_stdin_eof, PROCESS_STDIN_EOF, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
}; FuseExpect::Jit);

#[cfg(unix)]
const PROCESS_ENV: &str = r#"
{
  use sys::io::Read;
  use opt;
  let options = sys::process::options(
    #args: ["-c", "printf \"$FOO\""],
    #env: {"FOO" => "bar"},
    #stdio: sys::process::stdio(#stdout: `Pipe, #stderr: `Inherit),
    #kill_on_drop: true,
    "/bin/sh"
  );
  let child = sys::process::spawn(options)?;
  let stdout = opt::ok_or(child.stdout, `Null("stdout"))?;
  buffer::to_string(Read::read(stdout, u64:1024)?)?
}
"#;

#[cfg(unix)]
run!(process_env, PROCESS_ENV, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "bar")
}; FuseExpect::Jit);

#[cfg(unix)]
const PROCESS_GRACEFUL_KILL: &str = r#"
{
  use sys::io::Lines;
  use opt;
  let options = sys::process::options(
    #args: ["-c", "trap 'exit 0' TERM; echo ready; sleep 10 & wait"],
    #stdio: sys::process::stdio(#stdout: `Pipe, #stderr: `Inherit),
    #kill_on_drop: true,
    "/bin/sh"
  );
  let child = sys::process::spawn(options)?;
  let status = sys::process::wait(child.proc)?;
  let out = opt::ok_or(child.stdout, `Null("stdout"))?;
  let ready = Lines::lines(out)?;
  sys::process::kill(#grace: duration:5.s, ready ~ child.proc);
  status.success
}
"#;

#[cfg(unix)]
run!(process_graceful_kill, PROCESS_GRACEFUL_KILL, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
}; FuseExpect::Jit);

const PROCESS_SPAWN_FAIL: &str = r#"
{
  let options = sys::process::options(
    #kill_on_drop: true,
    "/definitely/not/a/real/command"
  );
  is_err(sys::process::spawn(options))
}
"#;

run!(process_spawn_fail, PROCESS_SPAWN_FAIL, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
}; FuseExpect::Jit);

// `Lines::lines` frames at the byte level: a line split across two
// reads, a CRLF line, and a trailing fragment with no newline.
#[cfg(unix)]
const IO_LINES: &str = r#"
{
  use sys::io::Lines;
  use opt;
  let child = sys::process::spawn(sys::process::options(
    #args: ["-c", "printf 'alpha\nbeta\r\ngamma\n'; sleep 0.2; printf 'del'; sleep 0.1; printf 'ta\nlast-no-newline'"],
    #stdio: sys::process::stdio(#stdout: `Pipe, #stderr: `Inherit),
    #kill_on_drop: true,
    "/bin/sh"
  ))?;
  let out = opt::ok_or(child.stdout, `Null("stdout"))?;
  let line = Lines::lines(out)?;
  let seen = [];
  seen <- array::push(line ~ seen, line);
  // Produce once, after the stream ended: a line delivered at EOF
  // lands in seen first.
  sys::time::after_idle(duration:200.ms, sys::process::wait(child.proc)?) ~ seen
}
"#;

#[cfg(unix)]
run!(io_lines, IO_LINES, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => {
            let got: Vec<&str> = a
                .iter()
                .filter_map(|v| match v {
                    Value::String(s) => Some(&**s),
                    _ => None,
                })
                .collect();
            a.len() == 4 && got == ["alpha", "beta", "gamma", "delta"]
        }
        _ => false,
    }
}; FuseExpect::Jit);

// The batched form delivers one event per read carrying every line that
// read made available: two writes arrive as two arrays.
#[cfg(unix)]
const IO_LINES_BATCHED: &str = r#"
{
  use sys::io::Lines;
  use opt;
  let child = sys::process::spawn(sys::process::options(
    #args: ["-c", "printf 'a1\na2\na3\n'; sleep 0.2; printf 'b1\nb2\n'"],
    #stdio: sys::process::stdio(#stdout: `Pipe, #stderr: `Inherit),
    #kill_on_drop: true,
    "/bin/sh"
  ))?;
  let out = opt::ok_or(child.stdout, `Null("stdout"))?;
  let batch = Lines::lines_batched(out)?;
  let seen = [];
  // Assert the CONTRACT (every line, in order, never split across
  // events), not the read boundaries: a pipe may split one write across
  // reads, so how many lines ride in a given batch is not ours to pin.
  seen <- array::concat(batch ~ seen, batch);
  select array::len(seen) {
    i64:5 => str::join(#sep: ",", seen),
    _ => never()
  }
}
"#;

#[cfg(unix)]
run!(io_lines_batched, IO_LINES_BATCHED, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "a1,a2,a3,b1,b2")
}; FuseExpect::Jit);

/// Whether `pid` names a live process (a zombie is dead).
#[cfg(target_os = "linux")]
fn alive(pid: i64) -> bool {
    std::fs::read_to_string(format!("/proc/{pid}/stat")).is_ok_and(|stat| {
        stat.rsplit(')').next().is_some_and(|rest| !rest.trim_start().starts_with('Z'))
    })
}

// Dropping the expression that holds the last handle to a child spawned
// with #kill_on_drop kills the child (a module's bindings would outlive
// the expression that loaded it, so the program is compiled bare).
#[cfg(target_os = "linux")]
#[tokio::test(flavor = "current_thread")]
async fn kill_on_drop_kills_the_child() -> Result<()> {
    use graphix_package_core::testing::{Mode, fixture_runtime, next_update};
    use std::time::Duration;
    use tokio::time::Instant;
    let src = r#"{
  let child = sys::process::spawn(sys::process::options(
    #args: ["30"],
    #kill_on_drop: true,
    "/bin/sleep"
  ))?;
  sys::process::pid(child.proc)
}"#;
    let (ctx, mut rx) =
        fixture_runtime([], crate::TEST_REGISTER, Mode::Jit, |_| {}).await?;
    let res = ctx.rt.compile(arcstr::ArcStr::from(src)).await?;
    let deadline = Instant::now() + Duration::from_secs(5);
    let pid = match next_update(&mut rx, res.exprs[0].id, deadline).await? {
        Value::I64(pid) => pid,
        v => anyhow::bail!("expected a pid, got {v:?}"),
    };
    assert!(alive(pid), "the child is not running");
    drop(res);
    ctx.rt.wait_idle().await?;
    let deadline = Instant::now() + Duration::from_secs(5);
    while alive(pid) {
        if Instant::now() >= deadline {
            anyhow::bail!("child {pid} outlived the expression that held it");
        }
        tokio::time::sleep(Duration::from_millis(20)).await;
    }
    ctx.shutdown().await;
    Ok(())
}
