use anyhow::{bail, Result};
use arcstr::ArcStr;
use compact_str::format_compact;
use graphix_compiler::errf;
use graphix_package_core::{CachedArgsAsync, CachedVals, EvalCachedAsync};
use immutable_chunkmap::map::Map as CMap;
use netidx_activation::process::{spawn as os_spawn, stop_proc, Job, Spawned};
use netidx_derive::{FromValue, IntoValue};
use netidx_value::{abstract_type::AbstractWrapper, Abstract, FromValue as _, Value};
use std::{
    cmp::Ordering,
    hash::{Hash, Hasher},
    process::{ExitStatus as StdExitStatus, Stdio as ProcessStdio},
    sync::{LazyLock, OnceLock},
    time::Duration,
};
use tokio::{
    process::Command,
    sync::{mpsc, oneshot, watch},
};
use triomphe::Arc;

use crate::{wrap_stream, StreamKind};

// -- Abstract ProcValue -------------------------------------------------------

type Status = Option<Result<ExitStatusValue, ArcStr>>;

struct KillReq {
    grace: Duration,
    done: oneshot::Sender<()>,
}

/// Handle to a supervised child. The child itself is owned by a spawned
/// task ([`own_child`]) — kill requests travel over `ctl`, the exit
/// status arrives on the watch. Dropping the last handle closes `ctl`,
/// which is the kill-on-drop signal.
struct ManagedProc {
    pid: i64,
    ctl: mpsc::UnboundedSender<KillReq>,
    status_rx: watch::Receiver<Status>,
}

impl std::fmt::Debug for ManagedProc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ManagedProc").field("pid", &self.pid).finish()
    }
}

impl ManagedProc {
    fn status(&self) -> Status {
        self.status_rx.borrow().clone()
    }

    /// Stop the child: graceful signal, `grace` to comply, hard kill as
    /// the backstop (`stop_proc`). Resolves after the child is dead. A
    /// closed channel or dropped ack means the child already exited —
    /// a no-op, matching "kill if still running".
    async fn kill(&self, grace: Duration) {
        let (done, ack) = oneshot::channel();
        if self.ctl.send(KillReq { grace, done }).is_ok() {
            let _ = ack.await;
        }
    }
}

/// The task that owns the child. All mutable access lives here: `wait`
/// and kill requests are serialized by the select loop, so there is no
/// lock and no polling. Biased: an exited child wins a race with a kill
/// request (the kill acks as a no-op from the drain loop).
async fn own_child(
    mut spawned: Spawned,
    kill_on_drop: bool,
    status_tx: watch::Sender<Status>,
    mut ctl: mpsc::UnboundedReceiver<KillReq>,
) {
    loop {
        tokio::select! {
            biased;
            status = spawned.wait() => {
                let status = match status {
                    Ok(s) => Ok(s.into()),
                    Err(e) => Err(ArcStr::from(format_compact!("{e}").as_str())),
                };
                let _ = status_tx.send(Some(status));
                break;
            }
            req = ctl.recv() => match req {
                Some(KillReq { grace, done }) => {
                    stop_proc(&mut spawned, grace).await;
                    let _ = done.send(());
                }
                None => {
                    // Every handle dropped — nobody can observe the
                    // status anymore.
                    if kill_on_drop {
                        stop_proc(&mut spawned, Duration::ZERO).await;
                    }
                    break;
                }
            },
        }
    }
    while let Some(KillReq { done, .. }) = ctl.recv().await {
        let _ = done.send(());
    }
}

#[derive(Debug, Clone)]
pub(crate) struct ProcValue {
    inner: Arc<ManagedProc>,
}

impl PartialEq for ProcValue {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.inner, &other.inner)
    }
}

impl Eq for ProcValue {}

impl PartialOrd for ProcValue {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for ProcValue {
    fn cmp(&self, other: &Self) -> Ordering {
        (&*self.inner as *const ManagedProc).cmp(&(&*other.inner as *const ManagedProc))
    }
}

impl Hash for ProcValue {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (&*self.inner as *const ManagedProc).hash(state)
    }
}

graphix_package_core::impl_no_pack!(ProcValue);

static PROC_WRAPPER: LazyLock<AbstractWrapper<ProcValue>> = LazyLock::new(|| {
    let id = uuid::Uuid::from_bytes([
        0xd4, 0xe5, 0xf6, 0xa1, 0xb2, 0xc3, 0x47, 0x89, 0x9a, 0xbc, 0xde, 0xf0, 0x12,
        0x34, 0x56, 0x7b,
    ]);
    Abstract::register::<ProcValue>(id).expect("failed to register ProcValue")
});

fn get_proc(cached: &CachedVals, idx: usize) -> Option<ProcValue> {
    match cached.0.get(idx)?.as_ref()? {
        Value::Abstract(a) => a.downcast_ref::<ProcValue>().cloned(),
        _ => None,
    }
}

// -- Process status ----------------------------------------------------------

#[derive(Debug, Clone, IntoValue)]
struct ExitStatusValue {
    code: Option<i64>,
    success: bool,
}

impl From<StdExitStatus> for ExitStatusValue {
    fn from(status: StdExitStatus) -> Self {
        Self { code: status.code().map(i64::from), success: status.success() }
    }
}

// -- Spawn options -----------------------------------------------------------

#[derive(Debug, Clone, Copy, FromValue)]
enum Stdio {
    Pipe,
    Inherit,
    Null,
}

#[derive(Debug, FromValue)]
struct StdioConfig {
    stdin: Stdio,
    stdout: Stdio,
    stderr: Stdio,
}

// CR claude for eric: SpawnOptionsValue/SpawnOptions are 7-field twins
// existing only so TryFrom can narrow `env: Value` to the Map rep. If
// netidx's FromValue derives through CMap<Value, Value, 32> (Value::Map
// holds exactly that), `env` can be typed directly and the twin struct
// + TryFrom collapse. Worth a try; if FromValue can't, leave it.
#[derive(Debug, FromValue)]
struct SpawnOptionsValue {
    command: ArcStr,
    args: Vec<ArcStr>,
    cwd: Option<ArcStr>,
    clear_env: bool,
    env: Value,
    stdio: StdioConfig,
    kill_on_drop: bool,
}

#[derive(Debug)]
pub(crate) struct SpawnOptions {
    command: ArcStr,
    args: Vec<ArcStr>,
    cwd: Option<ArcStr>,
    clear_env: bool,
    env: CMap<Value, Value, 32>,
    stdio: StdioConfig,
    kill_on_drop: bool,
}

impl TryFrom<SpawnOptionsValue> for SpawnOptions {
    type Error = anyhow::Error;

    fn try_from(options: SpawnOptionsValue) -> Result<Self> {
        let Value::Map(env) = options.env else {
            bail!("field `env` must be Map<string, [string, null]>");
        };
        Ok(Self {
            command: options.command,
            args: options.args,
            cwd: options.cwd,
            clear_env: options.clear_env,
            env,
            stdio: options.stdio,
            kill_on_drop: options.kill_on_drop,
        })
    }
}

#[derive(Debug, IntoValue)]
struct ChildBundle {
    proc: ProcValue,
    pid: i64,
    stdin: Option<Value>,
    stdout: Option<Value>,
    stderr: Option<Value>,
}

fn stdio_to_process(stdio: Stdio) -> ProcessStdio {
    match stdio {
        Stdio::Pipe => ProcessStdio::piped(),
        Stdio::Inherit => ProcessStdio::inherit(),
        Stdio::Null => ProcessStdio::null(),
    }
}

fn apply_env(cmd: &mut Command, env: CMap<Value, Value, 32>) -> Result<()> {
    for (key, val) in env.into_iter() {
        let Value::String(key) = key else {
            bail!("env keys must be strings");
        };
        match val {
            Value::String(val) => {
                cmd.env(&**key, &**val);
            }
            Value::Null => {
                cmd.env_remove(&**key);
            }
            _ => bail!("env values must be string or null"),
        }
    }
    Ok(())
}

impl From<ProcValue> for Value {
    fn from(proc: ProcValue) -> Self {
        PROC_WRAPPER.wrap(proc)
    }
}

/// The one [`Job`] `kill_on_drop` children are assigned to. On Windows
/// this is a `KILL_ON_JOB_CLOSE` job object, so those children die with
/// the graphix process even if it crashes; a no-op handle on unix.
/// `kill_on_drop: false` children are spawned OUTSIDE the job so they
/// may outlive the graphix process on every platform. A raced double
/// init drops the loser's empty job harmlessly.
static JOB: OnceLock<Job> = OnceLock::new();

fn job() -> Result<&'static Job> {
    if let Some(job) = JOB.get() {
        return Ok(job);
    }
    let job = Job::new()?;
    Ok(JOB.get_or_init(|| job))
}

fn spawn_child(opts: SpawnOptions) -> Result<ChildBundle> {
    let mut cmd = Command::new(&*opts.command);
    cmd.args(opts.args.iter().map(|s| &**s));
    if let Some(cwd) = opts.cwd {
        cmd.current_dir(&*cwd);
    }
    if opts.clear_env {
        cmd.env_clear();
    }
    apply_env(&mut cmd, opts.env)?;
    cmd.stdin(stdio_to_process(opts.stdio.stdin));
    cmd.stdout(stdio_to_process(opts.stdio.stdout));
    cmd.stderr(stdio_to_process(opts.stdio.stderr));

    let job = if opts.kill_on_drop { Some(job()?) } else { None };
    let mut spawned = os_spawn(cmd, job)?;
    let pid = spawned
        .id()
        .ok_or_else(|| anyhow::anyhow!("child process has no OS pid"))?
        as i64;
    let stdin = spawned.take_stdin().map(|s| wrap_stream(StreamKind::ChildStdin(s)));
    let stdout = spawned.take_stdout().map(|s| wrap_stream(StreamKind::ChildStdout(s)));
    let stderr = spawned.take_stderr().map(|s| wrap_stream(StreamKind::ChildStderr(s)));
    let (status_tx, status_rx) = watch::channel(None);
    let (ctl, ctl_rx) = mpsc::unbounded_channel();
    tokio::spawn(own_child(spawned, opts.kill_on_drop, status_tx, ctl_rx));
    let proc = ProcValue { inner: Arc::new(ManagedProc { pid, ctl, status_rx }) };

    Ok(ChildBundle { proc, pid, stdin, stdout, stderr })
}

// -- ProcessSpawn ------------------------------------------------------------

#[derive(Debug, Default)]
pub(crate) struct ProcessSpawnEv;

impl EvalCachedAsync for ProcessSpawnEv {
    const NAME: &str = "sys_process_spawn";
    type Args = Result<SpawnOptions>;

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        let options = cached.0.get(0)?.as_ref()?;
        Some(SpawnOptionsValue::from_value(options.clone()).and_then(TryInto::try_into))
    }

    fn eval(args: Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            let opts = match args {
                Ok(opts) => opts,
                Err(e) => return errf!("ProcessError", "{e}"),
            };
            match spawn_child(opts) {
                Ok(child) => child.into(),
                Err(e) => errf!("ProcessError", "spawn failed: {e}"),
            }
        }
    }
}

pub(crate) type ProcessSpawn = CachedArgsAsync<ProcessSpawnEv>;

// -- ProcessExitStatus -------------------------------------------------------

// CR claude for eric: design question — `exit_status` samples the watch
// once per delivery of its `proc` argument and never re-fires, so the
// natural `spawn → exit_status` reads null essentially always and the
// caller must re-trigger it manually (`exit_status(timer ~ proc)`).
// The watch channel is already there; subscribing it would make this a
// reactive status source (null while running, then the ExitStatus) —
// which is the graphix-native shape, and distinct from `wait` (one-shot
// final). If sample-only is the intent, the .gxi doc should warn about
// the retrigger idiom. Relatedly the exit_status fixture races under
// load: a >100ms dispatch delay lets "exit 0" land before the sample
// and the test yields false.
#[derive(Debug, Default)]
pub(crate) struct ProcessExitStatusEv;

impl EvalCachedAsync for ProcessExitStatusEv {
    const NAME: &str = "sys_process_exit_status";
    type Args = ProcValue;

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        get_proc(cached, 0)
    }

    fn eval(proc: Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            match proc.inner.status() {
                Some(Ok(status)) => status.into(),
                Some(Err(e)) => errf!("ProcessError", "{e}"),
                None => Value::Null,
            }
        }
    }
}

pub(crate) type ProcessExitStatus = CachedArgsAsync<ProcessExitStatusEv>;

// -- ProcessWait -------------------------------------------------------------

#[derive(Debug, Default)]
pub(crate) struct ProcessWaitEv;

impl EvalCachedAsync for ProcessWaitEv {
    const NAME: &str = "sys_process_wait";
    type Args = ProcValue;

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        get_proc(cached, 0)
    }

    fn eval(proc: Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            let mut status = proc.inner.status_rx.clone();
            let result = match status.wait_for(|status| status.is_some()).await {
                Ok(status) => status.clone().expect("wait_for predicate guarantees status"),
                Err(e) => return errf!("ProcessError", "status channel closed: {e}"),
            };
            match result {
                Ok(status) => status.into(),
                Err(e) => errf!("ProcessError", "{e}"),
            }
        }
    }
}

pub(crate) type ProcessWait = CachedArgsAsync<ProcessWaitEv>;

// -- ProcessKill -------------------------------------------------------------

#[derive(Debug, Default)]
pub(crate) struct ProcessKillEv;

impl EvalCachedAsync for ProcessKillEv {
    const NAME: &str = "sys_process_kill";
    type Args = (ProcValue, Duration);

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        let grace = cached.0.get(0)?.as_ref()?.clone().cast_to::<Duration>().ok()?;
        let proc = get_proc(cached, 1)?;
        Some((proc, grace))
    }

    fn eval((proc, grace): Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            proc.inner.kill(grace).await;
            Value::Null
        }
    }
}

pub(crate) type ProcessKill = CachedArgsAsync<ProcessKillEv>;

// -- ProcessPid ------------------------------------------------------------

#[derive(Debug, Default)]
pub(crate) struct ProcessPidEv;

impl EvalCachedAsync for ProcessPidEv {
    const NAME: &str = "sys_process_pid";
    type Args = ProcValue;

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        get_proc(cached, 0)
    }

    fn eval(proc: Self::Args) -> impl Future<Output = Value> + Send {
        async move { Value::I64(proc.inner.pid) }
    }
}

pub(crate) type ProcessPid = CachedArgsAsync<ProcessPidEv>;
