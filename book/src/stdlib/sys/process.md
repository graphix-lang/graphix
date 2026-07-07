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

val exit_status: fn(proc: Proc) -> Result<[ExitStatus, null], `ProcessError(string)>;

val wait: fn(proc: Proc) -> Result<ExitStatus, `ProcessError(string)>;

val pid: fn(proc: Proc) -> i64;

val kill: fn(proc: Proc) -> Result<null, `ProcessError(string)>;
```

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
