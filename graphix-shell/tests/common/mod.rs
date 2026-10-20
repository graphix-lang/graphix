//! Running the `graphix` binary from a test.

#![allow(dead_code)]

use std::{
    fs,
    path::Path,
    process::{Command, ExitStatus, Stdio},
};

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
    let out = Command::new(env!("CARGO_BIN_EXE_graphix"))
        .arg("--no-netidx")
        .arg("--log-dir")
        .arg(&logs)
        .args(args)
        .arg(&path)
        .env("XDG_CACHE_HOME", dir.join("cache"))
        .envs(env.iter().copied())
        .stdin(Stdio::null())
        .output()
        .expect("run graphix");
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

/// The env that logs what the image cache does.
pub const CACHE_LOG: (&str, &str) = ("RUST_LOG", "warn,graphix_shell::cache=info");

/// Whether a run logged under [`CACHE_LOG`] that it wrote the program's
/// entry, and whether it found the entry fresh and loaded it.
pub fn program_image(ran: &Ran) -> (bool, bool) {
    let lines = || ran.log.lines().filter(|l| l.contains("Program image "));
    let written = lines().any(|l| l.contains("written to"));
    let loaded = lines().any(|l| !l.contains("written to") && !l.ends_with("is stale"));
    (written, loaded)
}
