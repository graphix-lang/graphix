//! `cargo xtest [cargo test options] [-- test binary options]`: the workspace's
//! tests, as `cargo test` runs them, but every test binary side by side. It
//! builds with `cargo test --no-run`, runs each binary as cargo would (its
//! package root the working directory, cargo's environment), the slowest of
//! the last run first, and the doc tests with `cargo test --doc` beside them;
//! it prints each binary's result as it ends, then the whole output of every
//! one that failed, and exits 1 when any did. `XTEST_JOBS` sets how many run
//! at once (a quarter of the cores by default, at least two).

use anyhow::{Context, Result, bail};
use serde_json::Value as Json;
use std::{
    collections::{HashMap, VecDeque},
    env, fs,
    io::{BufRead, BufReader},
    path::{Path, PathBuf},
    process::{Command, ExitCode, Output, Stdio},
    sync::{Arc, Mutex, mpsc},
    thread,
    time::{Duration, Instant},
};

/// One test binary, or the doc tests, and how to run it.
struct Job {
    label: String,
    command: Command,
}

/// What a job did.
struct Done {
    label: String,
    took: Duration,
    output: std::io::Result<Output>,
}

/// The counts in libtest's `test result:` lines, summed.
#[derive(Default)]
struct Counts {
    passed: u64,
    failed: u64,
    ignored: u64,
}

impl Counts {
    fn read(stdout: &str) -> Option<Counts> {
        let mut counts = None;
        for line in stdout.lines().filter(|l| l.starts_with("test result:")) {
            let c: &mut Counts = counts.get_or_insert_with(Counts::default);
            let field = |what: &str| {
                line.split(';')
                    .find_map(|part| part.trim().strip_suffix(what))
                    .and_then(|n| n.trim().rsplit(' ').next()?.parse::<u64>().ok())
                    .unwrap_or(0)
            };
            c.passed += field(" passed");
            c.failed += field(" failed");
            c.ignored += field(" ignored");
        }
        counts
    }
}

fn main() -> Result<ExitCode> {
    let args: Vec<String> = env::args().skip(1).collect();
    let usage = "usage: xtask test [cargo test options] [-- test binary options]";
    let Some((cmd, rest)) = args.split_first() else { bail!(usage) };
    if cmd != "test" {
        bail!(usage)
    }
    let (cargo_args, bin_args) = match rest.iter().position(|a| a == "--") {
        Some(i) => (&rest[..i], &rest[i + 1..]),
        None => (rest, &[][..]),
    };
    let cargo = env::var_os("CARGO").unwrap_or_else(|| "cargo".into());
    let Some(binaries) = build(&cargo, cargo_args)? else {
        return Ok(ExitCode::FAILURE);
    };
    let times_path = binaries
        .first()
        .and_then(|b| Some(b.exe.parent()?.parent()?.parent()?.join("xtest-times.json")));
    let last = times_path.as_deref().map(read_times).unwrap_or_default();
    let library_path = library_path(binaries.first().and_then(|b| b.exe.parent()))?;
    let mut jobs: Vec<(Option<f64>, Job)> = binaries
        .iter()
        .map(|b| (last.get(&b.label).copied(), b.job(bin_args, &library_path)))
        .collect();
    let mut doc = Command::new(&cargo);
    doc.arg("test").arg("--doc").args(cargo_args).arg("--").args(bin_args);
    jobs.push((
        last.get("doc tests").copied(),
        Job { label: "doc tests".into(), command: doc },
    ));
    // a binary the last run did not time may be the longest: it goes first
    jobs.sort_by(|(a, _), (b, _)| {
        b.unwrap_or(f64::MAX).total_cmp(&a.unwrap_or(f64::MAX))
    });
    let started = Instant::now();
    let done = run(jobs.into_iter().map(|(_, j)| j).collect(), job_limit());
    let mut times = last;
    let (mut total, mut failures) = (Counts::default(), Vec::new());
    for d in &done {
        times.insert(d.label.clone(), d.took.as_secs_f64());
        let ok = match &d.output {
            Ok(o) => {
                let counts = Counts::read(&String::from_utf8_lossy(&o.stdout));
                if let Some(c) = &counts {
                    total.passed += c.passed;
                    total.failed += c.failed;
                    total.ignored += c.ignored;
                }
                o.status.success() && counts.is_some()
            }
            Err(_) => false,
        };
        if !ok {
            failures.push(d);
        }
    }
    if let Some(path) = &times_path {
        let map: serde_json::Map<String, Json> =
            times.into_iter().map(|(k, v)| (k, Json::from(v))).collect();
        let _ = fs::write(path, Json::Object(map).to_string());
    }
    for d in &failures {
        eprintln!("\n==== {} failed ====", d.label);
        match &d.output {
            Ok(o) => {
                eprintln!("{}", String::from_utf8_lossy(&o.stdout));
                eprintln!("{}", String::from_utf8_lossy(&o.stderr));
                eprintln!("exit: {}", o.status);
            }
            Err(e) => eprintln!("could not run: {e}"),
        }
    }
    eprintln!(
        "\nxtest: {} binaries in {:.1}s: {} passed, {} failed, {} ignored; {} binaries failed",
        done.len(),
        started.elapsed().as_secs_f64(),
        total.passed,
        total.failed,
        total.ignored,
        failures.len()
    );
    for d in &failures {
        eprintln!("  FAILED {}", d.label);
    }
    Ok(if failures.is_empty() { ExitCode::SUCCESS } else { ExitCode::FAILURE })
}

/// A test binary `cargo test --no-run` built.
struct Binary {
    label: String,
    exe: PathBuf,
    manifest: PathBuf,
    package: String,
    version: String,
    crate_name: String,
}

impl Binary {
    /// Its run as `cargo test` makes it.
    fn job(&self, args: &[String], library_path: &(String, String)) -> Job {
        let dir = self.manifest.parent().unwrap_or(Path::new("."));
        let mut command = Command::new(&self.exe);
        command
            .args(args)
            .current_dir(dir)
            .env("CARGO_MANIFEST_DIR", dir)
            .env("CARGO_MANIFEST_PATH", &self.manifest)
            .env("CARGO_PKG_NAME", &self.package)
            .env("CARGO_PKG_VERSION", &self.version)
            .env("CARGO_CRATE_NAME", &self.crate_name)
            .env(&library_path.0, &library_path.1);
        Job { label: self.label.clone(), command }
    }
}

/// Build the test binaries, cargo's diagnostics on stderr; `None` when the
/// build failed.
fn build(cargo: &std::ffi::OsStr, cargo_args: &[String]) -> Result<Option<Vec<Binary>>> {
    let mut child = Command::new(cargo)
        .args(["test", "--no-run", "--message-format=json-render-diagnostics"])
        .args(cargo_args)
        .stdout(Stdio::piped())
        .spawn()
        .context("cargo")?;
    let mut binaries = Vec::new();
    let stdout = child.stdout.take().context("cargo's stdout")?;
    for line in BufReader::new(stdout).lines() {
        let line = line?;
        let Ok(m) = serde_json::from_str::<Json>(&line) else { continue };
        match m["reason"].as_str() {
            Some("compiler-artifact") if m["profile"]["test"] == true => {
                let Some(exe) = m["executable"].as_str() else { continue };
                let target = m["target"]["name"].as_str().unwrap_or("?");
                let kind = m["target"]["kind"][0].as_str().unwrap_or("?");
                let manifest = PathBuf::from(m["manifest_path"].as_str().unwrap_or(""));
                let (package, version) =
                    package_of(m["package_id"].as_str().unwrap_or(""), &manifest);
                binaries.push(Binary {
                    label: format!("{package} {kind} {target}"),
                    exe: exe.into(),
                    manifest,
                    package,
                    version,
                    crate_name: target.replace('-', "_"),
                });
            }
            Some("compiler-message") => {
                if let Some(r) = m["message"]["rendered"].as_str() {
                    eprint!("{r}")
                }
            }
            _ => (),
        }
    }
    Ok(child.wait()?.success().then_some(binaries))
}

/// A package's name and version from its id: `path+file:///…/dir#name@1.0`, or
/// `…/dir#1.0` where the name is the directory's.
fn package_of(id: &str, manifest: &Path) -> (String, String) {
    let tail = id.rsplit('#').next().unwrap_or("");
    match tail.split_once('@') {
        Some((name, version)) => (name.into(), version.into()),
        None => {
            let dir = manifest.parent().and_then(|d| d.file_name());
            (
                dir.map(|d| d.to_string_lossy().into_owned()).unwrap_or_default(),
                tail.into(),
            )
        }
    }
}

/// The dynamic library path cargo gives a test: the build's deps and the
/// toolchain's libraries (a proc macro's tests link the standard library).
fn library_path(deps: Option<&Path>) -> Result<(String, String)> {
    let var = if cfg!(windows) {
        "PATH"
    } else if cfg!(target_os = "macos") {
        "DYLD_FALLBACK_LIBRARY_PATH"
    } else {
        "LD_LIBRARY_PATH"
    };
    let rustc = env::var_os("RUSTC").unwrap_or_else(|| "rustc".into());
    let sysroot = Command::new(rustc).args(["--print", "sysroot"]).output()?;
    let sysroot = PathBuf::from(String::from_utf8_lossy(&sysroot.stdout).trim());
    let mut dirs: Vec<PathBuf> = deps.into_iter().map(Path::to_path_buf).collect();
    dirs.push(sysroot.join(if cfg!(windows) { "bin" } else { "lib" }));
    dirs.extend(
        env::var_os(var)
            .map(|p| env::split_paths(&p).collect::<Vec<_>>())
            .unwrap_or_default(),
    );
    Ok((var.into(), env::join_paths(dirs)?.to_string_lossy().into_owned()))
}

fn read_times(path: &Path) -> HashMap<String, f64> {
    let Ok(text) = fs::read_to_string(path) else { return HashMap::new() };
    let Ok(Json::Object(m)) = serde_json::from_str(&text) else { return HashMap::new() };
    m.into_iter().filter_map(|(k, v)| Some((k, v.as_f64()?))).collect()
}

fn job_limit() -> usize {
    match env::var("XTEST_JOBS").ok().and_then(|n| n.parse().ok()) {
        Some(n) if n > 0 => n,
        _ => {
            let cores = thread::available_parallelism().map(|n| n.get()).unwrap_or(4);
            (cores / 4).max(2)
        }
    }
}

/// Run `jobs` in order, `limit` at a time, printing each as it ends.
fn run(jobs: Vec<Job>, limit: usize) -> Vec<Done> {
    let n = jobs.len();
    let queue = Arc::new(Mutex::new(VecDeque::from(jobs)));
    let (tx, rx) = mpsc::channel();
    for _ in 0..limit.min(n) {
        let (queue, tx) = (queue.clone(), tx.clone());
        thread::spawn(move || {
            loop {
                let Some(mut job) = queue.lock().unwrap().pop_front() else { break };
                let start = Instant::now();
                let output = job.command.stdin(Stdio::null()).output();
                let done = Done { label: job.label, took: start.elapsed(), output };
                if tx.send(done).is_err() {
                    break;
                }
            }
        });
    }
    drop(tx);
    let mut done = Vec::with_capacity(n);
    for d in rx {
        let verdict = match &d.output {
            Ok(o) if o.status.success() => {
                match Counts::read(&String::from_utf8_lossy(&o.stdout)) {
                    Some(c) => {
                        format!("ok      {} passed, {} ignored", c.passed, c.ignored)
                    }
                    None => "FAILED  no test result".into(),
                }
            }
            Ok(o) => format!("FAILED  {}", o.status),
            Err(e) => format!("FAILED  {e}"),
        };
        eprintln!("{:7.1}s  {verdict:<34} {}", d.took.as_secs_f64(), d.label);
        done.push(d);
    }
    done
}
