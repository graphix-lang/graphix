# sys::process

The `sys::process` module starts and manages local child processes. Stdio
handles configured as `Pipe` are exposed as `sys::io::Stream<\`Pipe>`.

```graphix
use sys::io;

type Proc;

type Stdio = [
  `Pipe,
  `Inherit,
  `Null,
];

type StdioConfig = {
  stdin: Stdio,
  stdout: Stdio,
  stderr: Stdio,
};

type SpawnOptions = {
  command: string,
  args: Array<string>,
  cwd: [string, null],
  clear_env: bool,
  env: Map<string, [string, null]>,
  stdio: StdioConfig,
  kill_on_drop: bool,
};

type Child = {
  proc: Proc,
  pid: i64,
  stdin: [io::Stream<`Pipe>, null],
  stdout: [io::Stream<`Pipe>, null],
  stderr: [io::Stream<`Pipe>, null],
};

type ExitStatus = {
  code: [i64, null],
  success: bool,
};

val stdio: fn(
  ?#stdin: Stdio,
  ?#stdout: Stdio,
  ?#stderr: Stdio
) -> StdioConfig;

val options: fn(
  ?#args: Array<string>,
  ?#cwd: [string, null],
  ?#clear_env: bool,
  ?#env: Map<string, [string, null]>,
  ?#stdio: StdioConfig,
  ?#kill_on_drop: bool,
  command: string
) -> SpawnOptions;

val spawn: fn(options: SpawnOptions) -> Result<Child, `ProcessError(string)>;

val wait: fn(proc: Proc) -> Result<ExitStatus, `ProcessError(string)>;

val pid: fn(proc: Proc) -> i64;

val kill: fn(?#grace: duration, proc: Proc) -> null;
```

`kill` stops a running process gracefully: it signals shutdown (SIGTERM
on unix, a shutdown event on Windows), gives the process `#grace` to
comply, then hard-kills as the backstop. The default grace of zero is
an immediate hard kill. It resolves after the process is dead, and is
a no-op for a process that already exited. On Windows a `kill_on_drop`
child is also assigned to a job object, so it is terminated even if
the graphix process crashes; a child spawned with `kill_on_drop:
false` is detached and may outlive the graphix process on every
platform.

There is no separate status-probe function: `wait` is the reactive
status source. To track running state, give it an initial value:

```graphix
let status = `Running;
status <- `Exited(sys::process::wait(child.proc)?)
```

`status` holds `` `Running `` until the process exits, then fires
exactly once with the exit status — no polling, no race. A `wait`
called after the process has already exited fires immediately, and
multiple concurrent waiters are fine.

Example:

```graphix
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
  let status = sys::process::wait(child.proc)?;
  status.success ~ out
}
```
