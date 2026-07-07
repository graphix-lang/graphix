use anyhow::{bail, Result};
use arcstr::ArcStr;
use graphix_compiler::errf;
use graphix_package_core::{CachedArgsAsync, CachedVals, EvalCachedAsync};
use immutable_chunkmap::map::Map as CMap;
use netidx_derive::{FromValue, IntoValue};
use netidx_value::{abstract_type::AbstractWrapper, Abstract, FromValue as _, Value};
use std::{
    cmp::Ordering,
    hash::{Hash, Hasher},
    process::{ExitStatus as StdExitStatus, Stdio as ProcessStdio},
    sync::{Arc, LazyLock, Weak},
};
use tokio::{
    process::{Child as TokioChild, Command},
    sync::{watch, Mutex},
    time::{self, Duration},
};

use crate::{wrap_stream, StreamKind};

// -- Abstract ProcValue -------------------------------------------------------

#[derive(Debug)]
struct ManagedProc {
    child: Mutex<Option<TokioChild>>,
    pid: i64,
    kill_on_drop: bool,
    status_tx: watch::Sender<Option<Result<ExitStatusValue, ArcStr>>>,
    status_rx: watch::Receiver<Option<Result<ExitStatusValue, ArcStr>>>,
}

impl ManagedProc {
    fn new(child: TokioChild, pid: i64, kill_on_drop: bool) -> Self {
        let (status_tx, status_rx) = watch::channel(None);
        Self { child: Mutex::new(Some(child)), pid, kill_on_drop, status_tx, status_rx }
    }

    fn set_status(&self, status: Result<ExitStatusValue, ArcStr>) {
        let _ = self.status_tx.send(Some(status));
    }

    fn status(&self) -> Option<Result<ExitStatusValue, ArcStr>> {
        self.status_rx.borrow().clone()
    }
}

async fn poll_child(proc: Weak<ManagedProc>) {
    let mut interval = time::interval(Duration::from_millis(100));
    loop {
        interval.tick().await;
        let Some(proc) = proc.upgrade() else {
            return;
        };
        if proc.status().is_some() {
            return;
        }
        let status = {
            let mut guard = proc.child.lock().await;
            let Some(child) = guard.as_mut() else {
                return;
            };
            match child.try_wait() {
                Ok(Some(status)) => {
                    let _ = guard.take();
                    Some(Ok(status.into()))
                }
                Ok(None) => None,
                Err(e) => Some(Err(ArcStr::from(e.to_string().as_str()))),
            }
        };
        if let Some(status) = status {
            proc.set_status(status);
            return;
        }
    }
}

impl Drop for ManagedProc {
    fn drop(&mut self) {
        if !self.kill_on_drop || self.status_rx.borrow().is_some() {
            return;
        }
        if let Ok(mut child) = self.child.try_lock()
            && let Some(child) = child.as_mut()
        {
            let _ = child.start_kill();
        }
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
        Arc::as_ptr(&self.inner).cmp(&Arc::as_ptr(&other.inner))
    }
}

impl Hash for ProcValue {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Arc::as_ptr(&self.inner).hash(state)
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

fn proc_value(proc: ProcValue) -> Value {
    PROC_WRAPPER.wrap(proc)
}

impl From<ProcValue> for Value {
    fn from(proc: ProcValue) -> Self {
        proc_value(proc)
    }
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

    let mut child = cmd.spawn()?;
    let pid =
        child.id().ok_or_else(|| anyhow::anyhow!("child process has no OS pid"))?;
    let stdin = child.stdin.take().map(|s| wrap_stream(StreamKind::ChildStdin(s)));
    let stdout = child.stdout.take().map(|s| wrap_stream(StreamKind::ChildStdout(s)));
    let stderr = child.stderr.take().map(|s| wrap_stream(StreamKind::ChildStderr(s)));
    let inner = Arc::new(ManagedProc::new(child, pid as i64, opts.kill_on_drop));
    tokio::spawn(poll_child(Arc::downgrade(&inner)));
    let proc = ProcValue { inner };

    Ok(ChildBundle { proc, pid: pid as i64, stdin, stdout, stderr })
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
    type Args = ProcValue;

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        get_proc(cached, 0)
    }

    fn eval(proc: Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            if proc.inner.status().is_some() {
                return Value::Null;
            }
            let mut child = proc.inner.child.lock().await;
            let Some(child) = child.as_mut() else {
                return Value::Null;
            };
            match child.start_kill() {
                Ok(()) => Value::Null,
                Err(e) => errf!("ProcessError", "kill failed: {e}"),
            }
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
