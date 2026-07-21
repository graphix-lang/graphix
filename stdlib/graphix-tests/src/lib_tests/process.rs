use anyhow::Result;
use graphix_package_core::run;
use netidx::subscriber::Value;

#[cfg(unix)]
const PROCESS_STDOUT_PIPE: &str = r#"
{
  use opt;
  let options = sys::process::options(
    #args: ["-c", "printf hello"],
    #stdio: sys::process::stdio(#stdout: `Pipe, #stderr: `Inherit),
    #kill_on_drop: true,
    "/bin/sh"
  );
  let child = sys::process::spawn(options)?;
  let stdout = opt::ok_or(child.stdout, `Null("stdout"))?;
  let out = buffer::to_string(sys::io::read(stdout, u64:1024)?)?;
  out
}
"#;

#[cfg(windows)]
const PROCESS_STDOUT_PIPE: &str = r#"
{
  use opt;
  let options = sys::process::options(
    #args: ["/C", "<nul set /p =hello"],
    #stdio: sys::process::stdio(#stdout: `Pipe, #stderr: `Inherit),
    #kill_on_drop: true,
    "cmd.exe"
  );
  let child = sys::process::spawn(options)?;
  let stdout = opt::ok_or(child.stdout, `Null("stdout"))?;
  buffer::to_string(sys::io::read(stdout, u64:1024)?)?
}
"#;

run!(process_stdout_pipe, PROCESS_STDOUT_PIPE, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "hello")
}; graphix_package_core::testing::FuseExpect::Jit);

#[cfg(unix)]
const PROCESS_STDIN_PIPE: &str = r#"
{
  use opt;
  let options = sys::process::options(
    #args: ["-c", "cat"],
    #stdio: sys::process::stdio(#stdin: `Pipe, #stdout: `Pipe, #stderr: `Inherit),
    #kill_on_drop: true,
    "/bin/sh"
  );
  let child = sys::process::spawn(options)?;
  let stdin = opt::ok_or(child.stdin, `Null("stdin"))?;
  let wrote = sys::io::write_exact(stdin, buffer::from_string("ping"))?;
  let flushed = sys::io::flush(wrote ~ stdin)?;
  let stdout = opt::ok_or(child.stdout, `Null("stdout"))?;
  let out = buffer::to_string(sys::io::read(flushed ~ stdout, u64:4)?)?;
  sys::process::kill(out ~ child.proc);
  out
}
"#;

#[cfg(unix)]
run!(process_stdin_pipe, PROCESS_STDIN_PIPE, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "ping")
}; graphix_package_core::testing::FuseExpect::Jit);

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
}; graphix_package_core::testing::FuseExpect::Jit);

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
  first.code == 3 && second.code == 3
}
"#;

#[cfg(unix)]
run!(process_concurrent_wait, PROCESS_CONCURRENT_WAIT, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
}; graphix_package_core::testing::FuseExpect::Jit);

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
}; graphix_package_core::testing::FuseExpect::Jit);

#[cfg(unix)]
const PROCESS_ENV: &str = r#"
{
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
  buffer::to_string(sys::io::read(stdout, u64:1024)?)?
}
"#;

#[cfg(unix)]
run!(process_env, PROCESS_ENV, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "bar")
}; graphix_package_core::testing::FuseExpect::Jit);

#[cfg(unix)]
const PROCESS_GRACEFUL_KILL: &str = r#"
{
  let options = sys::process::options(
    #args: ["-c", "trap 'exit 0' TERM; sleep 10 & wait"],
    #kill_on_drop: true,
    "/bin/sh"
  );
  let child = sys::process::spawn(options)?;
  let status = sys::process::wait(child.proc)?;
  sys::process::kill(#grace: duration:5.s, sys::time::timer(duration:100.ms, false) ~ child.proc);
  status.success
}
"#;

#[cfg(unix)]
run!(process_graceful_kill, PROCESS_GRACEFUL_KILL, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
}; graphix_package_core::testing::FuseExpect::Jit);

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
}; graphix_package_core::testing::FuseExpect::Jit);
