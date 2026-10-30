//! Running the `graphix` binary from a test.

#![allow(dead_code)]

use std::{
    fs,
    io::{Read, Write},
    path::Path,
    process::{Child, Command, ExitStatus, Stdio},
    thread,
    time::{Duration, Instant},
};

/// How long a run may take before it is taken for a hang.
const HANG: Duration = Duration::from_secs(120);

/// A finished run of `graphix`.
pub struct Ran {
    pub status: ExitStatus,
    pub stdout: String,
    pub stderr: String,
    /// Every log file it wrote, in one string.
    pub log: String,
}

/// Run `program` as the script `dir/main.gx`, with `args` before the
/// file and `env` added: no netidx, stdin closed, the log in `dir/log`
/// (emptied first) and the image cache in `dir/cache`, never the user's.
/// `dir` is the caller's, typically a `tempfile::TempDir`, so a failed
/// test leaves nothing behind.
pub fn graphix(dir: &Path, program: &str, args: &[&str], env: &[(&str, &str)]) -> Ran {
    let path = dir.join("main.gx");
    fs::write(&path, program).expect("write the program");
    let child = command(dir)
        .args(args)
        .arg(&path)
        .envs(env.iter().copied())
        .stdin(Stdio::null())
        .spawn()
        .expect("run graphix");
    finish(dir, child)
}

/// Run the REPL with `args`, its stdin a pipe that holds `input` and then
/// ends; the rest as [`graphix`] runs a script.
pub fn repl(dir: &Path, args: &[&str], input: &str) -> Ran {
    let mut child =
        command(dir).args(args).stdin(Stdio::piped()).spawn().expect("run graphix");
    let mut stdin = child.stdin.take().expect("the REPL's stdin");
    stdin.write_all(input.as_bytes()).expect("write the REPL's input");
    drop(stdin);
    finish(dir, child)
}

/// `graphix` with no netidx, its log in `dir/log` (emptied first) and
/// its image cache in `dir/cache`, its output piped.
fn command(dir: &Path) -> Command {
    let logs = dir.join("log");
    let _ = fs::remove_dir_all(&logs);
    fs::create_dir_all(&logs).expect("create the log dir");
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_graphix"));
    cmd.arg("--no-netidx")
        .arg("--log-dir")
        .arg(&logs)
        .env("XDG_CACHE_HOME", dir.join("cache"))
        .stdout(Stdio::piped())
        .stderr(Stdio::piped());
    cmd
}

/// The run of `child`, started by [`command`] over `dir`, once it exits.
fn finish(dir: &Path, child: Child) -> Ran {
    let out = within_deadline(child);
    let log = fs::read_dir(dir.join("log"))
        .expect("read the log dir")
        .filter_map(|e| e.ok())
        .filter(|e| e.path().extension().is_some_and(|x| x == "log"))
        .map(|e| fs::read_to_string(e.path()).expect("read a log"))
        .collect::<String>();
    Ran {
        status: out.status,
        stdout: String::from_utf8_lossy(&out.stdout).into_owned(),
        stderr: String::from_utf8_lossy(&out.stderr).into_owned(),
        log,
    }
}

/// The child's output once it exits. One still running after [`HANG`] is
/// sent SIGABRT, so systemd-coredump keeps its threads' stacks, and the
/// test fails naming the pid to read them by.
fn within_deadline(mut child: Child) -> std::process::Output {
    let drain = |r: Option<Box<dyn Read + Send>>| {
        thread::spawn(move || {
            let mut buf = vec![];
            if let Some(mut r) = r {
                let _ = r.read_to_end(&mut buf);
            }
            buf
        })
    };
    let stdout = drain(child.stdout.take().map(|r| Box::new(r) as Box<dyn Read + Send>));
    let stderr = drain(child.stderr.take().map(|r| Box::new(r) as Box<dyn Read + Send>));
    let start = Instant::now();
    let status = loop {
        if let Some(status) = child.try_wait().expect("wait for graphix") {
            break status;
        }
        if start.elapsed() > HANG {
            let pid = child.id();
            #[cfg(unix)]
            unsafe {
                libc::kill(pid as libc::pid_t, libc::SIGABRT);
            }
            #[cfg(not(unix))]
            let _ = child.kill();
            let _ = child.wait();
            let stderr =
                String::from_utf8_lossy(&stderr.join().unwrap_or_default()).into_owned();
            panic!(
                "graphix (pid {pid}) ran past {HANG:?}: aborted it for a core, read its \
                 threads with `coredumpctl debug {pid}` then `thread apply all bt`\n\
                 stderr:\n{stderr}"
            )
        }
        thread::sleep(Duration::from_millis(20));
    };
    std::process::Output {
        status,
        stdout: stdout.join().unwrap_or_default(),
        stderr: stderr.join().unwrap_or_default(),
    }
}

/// The instructions retired in user space, from when it is opened, by
/// the thread that opened it and every thread and process that thread
/// starts afterwards: the work a run does, the same under any load. One
/// kernel counter for each kind of core (a hybrid CPU has two), summed.
#[cfg(target_os = "linux")]
pub struct Instructions(Vec<std::os::fd::OwnedFd>);

#[cfg(target_os = "linux")]
impl Instructions {
    /// Start counting. The kernel refuses while
    /// `kernel.perf_event_paranoid` is above 2.
    pub fn count() -> std::io::Result<Self> {
        use std::os::fd::{FromRawFd, OwnedFd};
        /// `struct perf_event_attr` as its first version laid it out.
        #[repr(C)]
        struct Attr {
            kind: u32,
            size: u32,
            config: u64,
            sample_period: u64,
            sample_type: u64,
            read_format: u64,
            flags: u64,
            wakeup_events: u32,
            bp_type: u32,
            config1: u64,
        }
        const HARDWARE: u32 = 0;
        const INSTRUCTIONS: u64 = 1;
        const INHERIT: u64 = 1 << 1;
        const EXCLUDE_KERNEL: u64 = 1 << 5;
        const EXCLUDE_HV: u64 = 1 << 6;
        const FD_CLOEXEC: libc::c_ulong = 8;
        let open = |pmu: u64| {
            let attr = Attr {
                kind: HARDWARE,
                size: size_of::<Attr>() as u32,
                config: pmu << 32 | INSTRUCTIONS,
                sample_period: 0,
                sample_type: 0,
                read_format: 0,
                flags: INHERIT | EXCLUDE_KERNEL | EXCLUDE_HV,
                wakeup_events: 0,
                bp_type: 0,
                config1: 0,
            };
            // SAFETY: attr outlives the call; pid 0 and cpu -1 count this
            // thread on any core.
            let fd = unsafe {
                libc::syscall(libc::SYS_perf_event_open, &attr, 0, -1, -1, FD_CLOEXEC)
            };
            match fd {
                -1 => Err(std::io::Error::last_os_error()),
                // SAFETY: the kernel returned a descriptor no one else owns.
                fd => Ok(unsafe { OwnedFd::from_raw_fd(fd as i32) }),
            }
        };
        // each core PMU of a hybrid CPU lists its cores, and a counter
        // naming no PMU counts on one kind of core only
        let pmus: Vec<u64> = fs::read_dir("/sys/bus/event_source/devices")
            .into_iter()
            .flatten()
            .flatten()
            .filter(|d| d.path().join("cpus").exists())
            .filter_map(|d| fs::read_to_string(d.path().join("type")).ok())
            .filter_map(|t| t.trim().parse().ok())
            .collect();
        match pmus.iter().map(|p| open(*p)).collect::<std::io::Result<Vec<_>>>() {
            Ok(fds) if !fds.is_empty() => Ok(Self(fds)),
            _ => Ok(Self(vec![open(0)?])),
        }
    }

    /// The count so far, running threads' included.
    pub fn read(&self) -> u64 {
        use std::os::fd::AsRawFd;
        self.0
            .iter()
            .map(|fd| {
                let mut n = 0u64;
                // SAFETY: reads one u64 into n.
                let r = unsafe { libc::read(fd.as_raw_fd(), (&raw mut n).cast(), 8) };
                assert_eq!(r, 8, "read the instruction count");
                n
            })
            .sum()
    }
}

/// The env that logs what the image cache does.
pub const CACHE_LOG: (&str, &str) = ("RUST_LOG", "warn,graphix_shell::cache=info");

/// Whether a run logged under [`CACHE_LOG`] that it wrote the program's
/// entry, and whether it found the entry fresh and loaded it.
pub fn program_image(ran: &Ran) -> (bool, bool) {
    let lines = || ran.log.lines().filter(|l| l.contains("Program image "));
    let written = lines().any(|l| l.contains("written to"));
    let loaded = lines().any(|l| {
        l.contains("INFO") && !l.contains("written to") && !l.ends_with("is stale")
    });
    (written, loaded)
}
