//! Running the `graphix` binary from a test.

#![allow(dead_code)]

use std::{
    fs,
    io::Read,
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
    let logs = dir.join("log");
    let _ = fs::remove_dir_all(&logs);
    fs::create_dir_all(&logs).expect("create the log dir");
    let child = Command::new(env!("CARGO_BIN_EXE_graphix"))
        .arg("--no-netidx")
        .arg("--log-dir")
        .arg(&logs)
        .args(args)
        .arg(&path)
        .env("XDG_CACHE_HOME", dir.join("cache"))
        .envs(env.iter().copied())
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("run graphix");
    let out = within_deadline(child);
    let log = fs::read_dir(&logs)
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
