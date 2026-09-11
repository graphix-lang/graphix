//! `graphix-fuzz`: the differential fuzzer CLI. `check <file>` runs
//! interp vs jit and reports a divergence; `run <file>` prints every
//! mode's outcome; the rest are campaigns, gates and hidden workers
//! (see the usage string). See design/graphix_fuzz.md.

use anyhow::{Result, bail};
use graphix_fuzz::{
    CAMPAIGN_MINIMIZE_BUDGET, Corpus, Mode, Outcome, check, fuzz, generate_campaign,
    minimize, regression_corpus_len, run_regression,
};
use std::{
    sync::{Arc, LazyLock},
    time::Duration,
};

// Harness-only: subjects are re-execs of this binary and allocate like
// the compiler they drive.
#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

/// Default soak mix, as CPU shares `fuzz:generate:reactive`, weighted
/// by measured findings per CPU-second.
const DEFAULT_MIX: &str = "50:25:25";

/// Parse a `fuzz:generate:reactive` CPU-share mix. Shares are relative;
/// the pool normalizes them.
fn parse_mix(spec: &str) -> Result<[f64; 3]> {
    let parts: Vec<&str> = spec.split(':').collect();
    if parts.len() != 3 {
        bail!("mix must be fuzz:generate:reactive, got {spec:?}");
    }
    let mut out = [0.0; 3];
    for (i, p) in parts.iter().enumerate() {
        out[i] = p
            .parse::<f64>()
            .map_err(|_| anyhow::anyhow!("mix component {p:?} is not a number"))?;
        if out[i] < 0.0 || !out[i].is_finite() {
            bail!("mix component {p:?} must be finite and non-negative");
        }
    }
    if out.iter().sum::<f64>() <= 0.0 {
        bail!("mix must have at least one positive share");
    }
    Ok(out)
}

/// Parse an iteration count. `forever`/`inf`/`0` → run forever (`None`);
/// a number → that many; absent/garbage → a sane default.
fn parse_iters(arg: Option<&String>, default: usize) -> Option<usize> {
    match arg.map(String::as_str) {
        None => Some(default),
        Some("forever") | Some("inf") | Some("0") => None,
        Some(s) => Some(s.parse().unwrap_or(default)),
    }
}

fn fmt_iters(iters: Option<usize>) -> String {
    iters.map_or_else(|| "forever".to_string(), |n| n.to_string())
}

// Every budget scales by GRAPHIX_FUZZ_TIMEOUT_SCALE (integer, default 1,
// set per machine) so every derived margin stays coherent.
static TIMEOUT_SCALE: LazyLock<u32> = LazyLock::new(|| {
    std::env::var("GRAPHIX_FUZZ_TIMEOUT_SCALE")
        .ok()
        .and_then(|s| s.parse().ok())
        .map(|n: u32| n.clamp(1, 100))
        .unwrap_or(1)
});

fn scaled(secs: u64) -> Duration {
    Duration::from_secs(secs) * *TIMEOUT_SCALE
}

fn timeout() -> Duration {
    scaled(10)
}

// A regression surfaces fast; a legitimately-bottom program only has to
// confirm "still all-Timeout".
fn regress_timeout() -> Duration {
    scaled(3)
}

// Generated programs terminate in milliseconds or produce bottom; a
// real divergence surfaces well within 3s.
fn campaign_timeout() -> Duration {
    scaled(3)
}

async fn print_regression() -> usize {
    let regr = run_regression(regress_timeout()).await;
    println!(
        "regression corpus: {} programs, {} regressions",
        regression_corpus_len(),
        regr.len()
    );
    for (name, d) in &regr {
        println!("  REGRESSION {name} — {}", d.bisect());
        println!("    interp={}", render(&d.interp));
        println!("    jit=  {}", render(&d.jit));
    }
    regr.len()
}

fn render(o: &Outcome) -> String {
    match o {
        Outcome::Trace(t) => {
            let epochs: Vec<String> = t
                .epochs
                .iter()
                .map(|e| {
                    let evs: Vec<String> =
                        e.events.iter().map(|(o, v)| format!("{o}:{v}")).collect();
                    let evs = evs.join(" ");
                    if e.capped {
                        format!("[{evs} …capped]")
                    } else {
                        format!("[{evs}]")
                    }
                })
                .collect();
            if t.stdout.is_empty() {
                format!("Trace({})", epochs.join("; "))
            } else {
                format!("Trace({}; stdout=[{}])", epochs.join("; "), t.stdout.join(" | "))
            }
        }
        Outcome::CompileErr(e) => format!("CompileErr({})", e.replace('\n', " | ")),
        Outcome::RuntimeErr(e) => format!("RuntimeErr({})", first_line(e)),
        Outcome::Timeout => "Timeout".to_string(),
    }
}

fn first_line(s: &str) -> String {
    s.lines().next().unwrap_or("").to_string()
}

/// Per-feature compile rates for gen-check/reactive-check, bucketed by
/// source substrings. A 0% row is a dead generator arm; an "absent" row
/// is an arm that stopped firing.
fn feature_report(progs: &[String], ok: &[bool]) {
    const FEATURES: &[(&str, &str)] = &[
        ("catch", "catch("),
        ("qop-catch", ")?"),
        ("qop-dollar", "$"),
        ("select", "select "),
        ("guard", " if "),
        ("rec", "let rec"),
        ("array-hof", "array::"),
        ("map-hof", "map::"),
        ("list-hof", "list::"),
        ("list-lit", "[<"),
        ("collection-trait", "Collection::"),
        ("trait-union-call", "::both("),
        ("bounded-hof-call", "::tsum("),
        ("collection-generic-call", "::csize("),
        ("str", "str::"),
        ("re", "re::"),
        ("variant", "`"),
        ("connect", "<-"),
        ("cast", "cast<"),
        ("refs", "&"),
        ("modules", "mod "),
        ("files", "file-v1"),
        ("reactive", "schedule-v1"),
        ("use-super", "use super::"),
        ("use-main", "use m"),
        ("path-super", "super::m"),
        ("path-package", "package::m"),
    ];
    println!("  per-feature compile rates:");
    for (name, pat) in FEATURES {
        let idx: Vec<usize> = progs
            .iter()
            .enumerate()
            .filter(|(_, p)| p.contains(pat))
            .map(|(i, _)| i)
            .collect();
        if idx.is_empty() {
            println!("    {name:>12}: absent from sample  <-- arm not firing?");
            continue;
        }
        let c = idx.iter().filter(|&&i| ok[i]).count();
        let pct = c as f64 * 100.0 / idx.len() as f64;
        let mark = if c == 0 { "  <-- DEAD ARM" } else { "" };
        println!("    {name:>12}: {c}/{} ({pct:.1}%){mark}", idx.len());
    }
}

/// Parse `check-batch`'s stdin framing: `{n}\n` then, per subject,
/// `{byte_len}\n{bytes}`. Length-prefixed because programs are
/// arbitrary text.
fn parse_batch_frames(input: &str) -> Result<Vec<String>> {
    let mut progs = Vec::new();
    let (head, mut rest) = input
        .split_once('\n')
        .ok_or_else(|| anyhow::anyhow!("check-batch: empty stdin"))?;
    let n: usize = head.trim().parse()?;
    for _ in 0..n {
        let (len, tail) = rest
            .split_once('\n')
            .ok_or_else(|| anyhow::anyhow!("check-batch: truncated frame header"))?;
        let len: usize = len.trim().parse()?;
        if tail.len() < len {
            anyhow::bail!("check-batch: truncated frame body");
        }
        progs.push(tail[..len].to_string());
        rest = &tail[len..];
    }
    Ok(progs)
}

/// Read a whole program from stdin (the `check-one` / `minimize-one`
/// isolated-worker input channel).
fn read_stdin() -> Result<String> {
    use std::io::Read;
    let mut buf = String::new();
    std::io::stdin().read_to_string(&mut buf)?;
    Ok(buf)
}

// Two worker threads on purpose: the work is in the children, each
// with its own runtime, and every parent thread is one stolen from them
// (measured: per-core sizing was a 7.5x throughput regression).
#[tokio::main(flavor = "multi_thread", worker_threads = 2)]
async fn main() -> Result<()> {
    // Before anything allocates: the child half of the sandbox's
    // address-space limit.
    graphix_fuzz::apply_mem_limit();
    // Cap this process like the children so an in-process runaway
    // recursion aborts to Timeout instead of growing stack segments.
    if std::env::var_os("GRAPHIX_STACK_BUDGET").is_none() {
        graphix_compiler::set_stack_budget(1 << 30);
    }
    let mut args: Vec<String> = std::env::args().collect();
    let reactive = args.iter().any(|a| a == "--reactive");
    args.retain(|a| a != "--reactive");
    // Generated programs call `sys::fs::write_all` & co. with arbitrary
    // paths. Worker processes are sandboxed by the spawning campaign
    // (GRAPHIX_FUZZ_SANDBOXED); this covers manual invocations.
    let sandbox_cwd = std::env::var_os("GRAPHIX_FUZZ_SANDBOXED").is_none()
        && match args.get(1).map(String::as_str) {
            Some(
                "check-one" | "check-batch" | "gen-batch" | "detcheck-one"
                | "selfcheck-one" | "minimize-one" | "typemorph-one" | "gen-check"
                | "regress" | "fusecheck" | "leakcheck",
            ) => true,
            Some("check" | "run") => {
                if let Some(f) = args.get_mut(2) {
                    if let Ok(abs) = std::fs::canonicalize(&*f) {
                        *f = abs.to_string_lossy().into_owned();
                    }
                }
                true
            }
            _ => false,
        };
    // captured before the sandbox chdir: `fusecheck --bless` writes here
    let orig_cwd = std::env::current_dir()?;
    let cwd_guard = if sandbox_cwd {
        let d = tempfile::tempdir()?;
        std::env::set_current_dir(d.path())?;
        Some(d)
    } else {
        None
    };
    let gen_one = move |rng: &mut graphix_fuzz::mutate::Rng| {
        if reactive {
            graphix_fuzz::generate::reactive::gen_reactive_program(rng)
        } else {
            graphix_fuzz::generate::gen_program(rng)
        }
    };
    match args.get(1).map(String::as_str) {
        Some("gen") => {
            // print N generated programs, no oracle
            let n: usize = args.get(2).and_then(|s| s.parse().ok()).unwrap_or(10);
            let seed: u64 = args.get(3).and_then(|s| s.parse().ok()).unwrap_or(1);
            let mut rng = graphix_fuzz::mutate::Rng::new(seed);
            for _ in 0..n {
                println!("{}\n", gen_one(&mut rng));
            }
        }
        Some("gen-check") => {
            // generator health: compile rate + reject reasons; the
            // generator is type-correct by construction
            let n: usize = args.get(2).and_then(|s| s.parse().ok()).unwrap_or(500);
            let seed: u64 = args.get(3).and_then(|s| s.parse().ok()).unwrap_or(1);
            let progs: Vec<String> = {
                let mut rng = graphix_fuzz::mutate::Rng::new(seed);
                (0..n).map(|_| gen_one(&mut rng)).collect()
            };
            let par =
                std::thread::available_parallelism().map(|n| n.get() * 2).unwrap_or(8);
            let mut set: tokio::task::JoinSet<(usize, Option<String>)> =
                tokio::task::JoinSet::new();
            let mut next = 0usize;
            let mut compiled = 0usize;
            // bucket by the innermost anyhow cause (the last line), with
            // one example program per bucket
            let mut rejects: std::collections::BTreeMap<String, (usize, String)> =
                std::collections::BTreeMap::new();
            let spawn = |set: &mut tokio::task::JoinSet<_>, i: usize, p: String| {
                set.spawn(async move {
                    (
                        i,
                        graphix_fuzz::compile_program(&p, graphix_fuzz::Mode::Interp)
                            .await,
                    )
                });
            };
            while next < progs.len() && set.len() < par {
                spawn(&mut set, next, progs[next].clone());
                next += 1;
            }
            // `GRAPHIX_FUZZ_DUMP_REJECTS=<dir>`: write each rejected
            // program with its full error to `<dir>/reject_<i>.gx`
            let dump_dir = std::env::var_os("GRAPHIX_FUZZ_DUMP_REJECTS");
            let mut ok = vec![false; progs.len()];
            while let Some(res) = set.join_next().await {
                match res {
                    Ok((i, None)) => {
                        compiled += 1;
                        ok[i] = true;
                    }
                    Ok((i, Some(err))) => {
                        if let Some(dir) = &dump_dir {
                            let p = std::path::Path::new(dir)
                                .join(format!("reject_{i:06}.gx"));
                            let body = format!(
                                "{}\n// gen-check reject:\n// {}\n",
                                progs[i],
                                err.replace('\n', "\n// ")
                            );
                            let _ = std::fs::write(p, body);
                        }
                        let mut key = err
                            .lines()
                            .rev()
                            .find(|l| !l.trim().is_empty())
                            .unwrap_or("")
                            .trim()
                            .to_string();
                        key.truncate(120);
                        let entry =
                            rejects.entry(key).or_insert_with(|| (0, progs[i].clone()));
                        entry.0 += 1;
                    }
                    Err(_) => {
                        rejects
                            .entry("worker panicked".into())
                            .or_insert_with(|| (0, String::new()))
                            .0 += 1;
                    }
                }
                if next < progs.len() {
                    spawn(&mut set, next, progs[next].clone());
                    next += 1;
                }
            }
            println!(
                "gen-check: seed={seed}: {compiled}/{n} compiled ({:.1}%)",
                compiled as f64 * 100.0 / n as f64
            );
            feature_report(&progs, &ok);
            let mut buckets: Vec<(usize, String, String)> =
                rejects.into_iter().map(|(k, (c, ex))| (c, k, ex)).collect();
            buckets.sort_by(|a, b| b.0.cmp(&a.0));
            for (count, msg, example) in buckets.iter().take(15) {
                println!("  {count:>4}  {msg}");
                println!("        e.g. {example}");
            }
            if buckets.len() > 15 {
                println!("  … {} more reject buckets", buckets.len() - 15);
            }
        }
        Some("regress") => {
            let n = print_regression().await;
            if n > 0 {
                drop(cwd_guard);
                std::process::exit(1);
            }
        }
        Some("leakcheck") => {
            // RSS leak lane: run each witness under both modes on the
            // given shell binary and require the fused RSS slope within
            // headroom of the interp slope. Linux only (/proc).
            let bin = args.get(2).cloned().unwrap_or_else(|| {
                eprintln!("usage: graphix-fuzz leakcheck <graphix-bin> [secs]");
                std::process::exit(2)
            });
            let secs: u64 = args.get(3).and_then(|s| s.parse().ok()).unwrap_or(60);
            let mut bad = 0usize;
            for (name, prog) in LEAK_WITNESSES {
                let dir = tempfile::tempdir()?;
                let f = dir.path().join("w.gx");
                std::fs::write(&f, prog)?;
                let mut slopes = [0f64; 2];
                for (i, mode_args) in [&["--no-fusion"][..], &[][..]].iter().enumerate() {
                    let mut child = std::process::Command::new(&bin)
                        .args(*mode_args)
                        .arg(&f)
                        .stdout(std::process::Stdio::null())
                        .stderr(std::process::Stdio::null())
                        .spawn()?;
                    let pid = child.id();
                    std::thread::sleep(Duration::from_secs(5));
                    let a = vm_rss_kb(pid);
                    std::thread::sleep(Duration::from_secs(secs));
                    let b = vm_rss_kb(pid);
                    let _ = child.kill();
                    let _ = child.wait();
                    let (Some(a), Some(b)) = (a, b) else {
                        eprintln!("  {name}: child died early — skipping");
                        continue;
                    };
                    slopes[i] = (b as f64 - a as f64) / secs as f64;
                }
                let [interp, jit] = slopes;
                // 50 kB/s of slack rides load noise without hiding a
                // real leak at 60s
                let ok = jit <= interp + 50.0;
                if !ok {
                    bad += 1;
                }
                println!(
                    "  {name}: interp {interp:.1} kB/s, jit {jit:.1} kB/s{}",
                    if ok { "" } else { "  <-- LEAK" }
                );
            }
            println!("leakcheck: {} witnesses, {bad} leaks", LEAK_WITNESSES.len());
            if bad > 0 {
                drop(cwd_guard);
                std::process::exit(1);
            }
        }
        Some("fusecheck") => {
            // fused-region counts per corpus program vs the checked-in
            // manifest. `--bless` rewrites the manifest (rebuild afterward:
            // the compare reads the embedded copy). An unmeasurable
            // count is a failure, never a 0.
            let bless = args.iter().any(|a| a == "--bless");
            let timeout = scaled(60);
            let counts = graphix_fuzz::run_fusecheck(timeout).await;
            let mut unreadable = 0usize;
            for (n, c) in &counts {
                if let Err(e) = c {
                    unreadable += 1;
                    let last =
                        e.lines().rev().find(|l| !l.trim().is_empty()).unwrap_or(e);
                    println!("  unreadable: {n}: {last}");
                }
            }
            if bless {
                if unreadable > 0 {
                    eprintln!(
                        "fusecheck: refusing to bless — {unreadable} unreadable counts"
                    );
                    drop(cwd_guard);
                    std::process::exit(1);
                }
                let mut out = String::new();
                for (n, c) in &counts {
                    let c = c.as_ref().expect("unreadable counts checked above");
                    out.push_str(&format!("{c}\t{n}\n"));
                }
                let path = orig_cwd.join("graphix-fuzz/fusecheck.manifest");
                std::fs::write(&path, out).unwrap_or_else(|e| {
                    panic!("writing {} (run from the repo root): {e}", path.display())
                });
                println!(
                    "fusecheck: blessed {} entries — rebuild to embed",
                    counts.len()
                );
            } else {
                let mut recorded: std::collections::BTreeMap<&str, u64> =
                    std::collections::BTreeMap::new();
                for l in graphix_fuzz::FUSECHECK_MANIFEST.lines() {
                    if let Some((c, n)) = l.split_once('\t') {
                        if let Ok(c) = c.parse() {
                            recorded.insert(n, c);
                        }
                    }
                }
                let mut bad = unreadable;
                for (n, c) in &counts {
                    let rec = recorded.remove(n.as_str());
                    let Ok(c) = c else { continue };
                    match rec {
                        Some(r) if r == *c => {}
                        Some(r) => {
                            bad += 1;
                            let dir = if *c < r { "LOST" } else { "gained" };
                            println!("  {dir} fusion: {n}: {r} -> {c}");
                        }
                        None => {
                            bad += 1;
                            println!("  unrecorded: {n} ({c} fused) — bless to record");
                        }
                    }
                }
                for (n, r) in recorded {
                    bad += 1;
                    println!("  stale manifest row: {n} ({r}) — bless to drop");
                }
                println!("fusecheck: {} programs, {bad} mismatches", counts.len());
                if bad > 0 {
                    drop(cwd_guard);
                    std::process::exit(1);
                }
            }
        }
        Some("reactive-check") => {
            // reactive-generator health: programs must quiesce within
            // their trace budget and injection epochs must advance the
            // trace. Interp only.
            let n: usize = args.get(2).and_then(|s| s.parse().ok()).unwrap_or(200);
            let seed: u64 = args.get(3).and_then(|s| s.parse().ok()).unwrap_or(1);
            let progs: Vec<String> = {
                let mut rng = graphix_fuzz::mutate::Rng::new(seed);
                (0..n)
                    .map(|_| {
                        graphix_fuzz::generate::reactive::gen_reactive_program(&mut rng)
                    })
                    .collect()
            };
            let par =
                std::thread::available_parallelism().map(|n| n.get() * 2).unwrap_or(8);
            let mut set: tokio::task::JoinSet<(usize, Outcome)> =
                tokio::task::JoinSet::new();
            let mut next = 0usize;
            let spawn = |set: &mut tokio::task::JoinSet<_>, i: usize, p: String| {
                set.spawn(async move {
                    (i, graphix_fuzz::run_program(&p, Mode::Interp, timeout()).await)
                });
            };
            while next < progs.len() && set.len() < par {
                spawn(&mut set, next, progs[next].clone());
                next += 1;
            }
            let (mut compiled, mut quiesced, mut advanced, mut wedged) = (0, 0, 0, 0);
            let mut ok = vec![false; progs.len()];
            let mut rejects: std::collections::BTreeMap<String, usize> =
                std::collections::BTreeMap::new();
            while let Some(res) = set.join_next().await {
                if let Ok((i, out)) = res {
                    if !matches!(out, Outcome::CompileErr(_) | Outcome::RuntimeErr(_)) {
                        ok[i] = true;
                    }
                    match out {
                        Outcome::CompileErr(e) => {
                            let mut key = e
                                .lines()
                                .rev()
                                .find(|l| !l.trim().is_empty())
                                .unwrap_or("")
                                .trim()
                                .to_string();
                            key.truncate(120);
                            *rejects.entry(key).or_default() += 1;
                        }
                        Outcome::RuntimeErr(e) => {
                            let mut key = format!("RUNTIME: {}", first_line(&e));
                            key.truncate(120);
                            *rejects.entry(key).or_default() += 1;
                        }
                        Outcome::Timeout => {
                            compiled += 1;
                            wedged += 1;
                        }
                        Outcome::Trace(t) => {
                            compiled += 1;
                            if !t.epochs.iter().any(|e| e.capped) {
                                quiesced += 1;
                            }
                            // advanced iff any epoch past the compile
                            // burst produced events
                            let has_inj = t.epochs.len() > 1;
                            if !has_inj
                                || t.epochs[1..].iter().any(|e| !e.events.is_empty())
                            {
                                advanced += 1;
                            }
                        }
                    }
                }
                if next < progs.len() {
                    spawn(&mut set, next, progs[next].clone());
                    next += 1;
                }
            }
            let pct = |x: usize| x as f64 * 100.0 / n as f64;
            println!(
                "reactive-check: seed={seed}: {compiled}/{n} compiled ({:.1}%), \
                 quiesced {quiesced} ({:.1}%), epochs-advanced {advanced} ({:.1}%), \
                 wedged {wedged}",
                pct(compiled),
                pct(quiesced),
                pct(advanced),
            );
            feature_report(&progs, &ok);
            let mut buckets: Vec<(usize, String)> =
                rejects.into_iter().map(|(k, c)| (c, k)).collect();
            buckets.sort_by(|a, b| b.0.cmp(&a.0));
            for (count, msg) in buckets.iter().take(15) {
                println!("  {count:>4}  {msg}");
            }
        }
        Some("selfcheck") => {
            // oracle-soundness gate: per-mode trace determinism; must be
            // 100% before any interp-vs-jit finding is trusted
            let iters = args.get(2).and_then(|s| s.parse().ok()).unwrap_or(1000);
            let seed: u64 = args.get(3).and_then(|s| s.parse().ok()).unwrap_or(1);
            let total = regression_corpus_len();
            println!(
                "selfcheck: {iters} generated (seed={seed}) + corpus \
                 (≥{total} seeds), twice per mode"
            );
            let flaky = graphix_fuzz::selfcheck(iters, seed, timeout()).await;
            if flaky.is_empty() {
                println!("selfcheck OK — every trace deterministic in both modes");
            } else {
                for (prog, mode) in &flaky {
                    println!("FLAKY under {mode}: {}", prog.replace('\n', "\\n"));
                }
                println!("selfcheck FAILED — {} flaky traces", flaky.len());
                std::process::exit(1);
            }
        }
        // Hidden workers. The verdict rides the exit code or a named
        // file, never stdout: the program under test can write to
        // stdout itself. detcheck-one drives the program to quiescence
        // so the lazily compiled per-slot kernels all appear in the
        // CLIF dump; exit 0 = ran, 3 = compile reject, 4 = timeout.
        Some("detcheck-one") => {
            let code = read_stdin()?;
            match graphix_fuzz::run_program(code.trim(), Mode::Jit, timeout()).await {
                graphix_fuzz::Outcome::CompileErr(e) => {
                    eprintln!("COMPILE REJECT: {e}");
                    std::process::exit(3);
                }
                graphix_fuzz::Outcome::Timeout => std::process::exit(4),
                graphix_fuzz::Outcome::Trace(_)
                | graphix_fuzz::Outcome::RuntimeErr(_) => std::process::exit(0),
            }
        }
        // typemorph child: base program on stdin, verdict lines to the
        // file named by argv[2]
        Some("typemorph-one") => {
            let out = args
                .get(2)
                .cloned()
                .ok_or_else(|| anyhow::anyhow!("typemorph-one <outfile>"))?;
            let code = read_stdin()?;
            let text =
                match graphix_fuzz::typemorph_subject(code.trim(), timeout(), 3).await {
                    Ok(rep) => rep.render(),
                    Err(e) => format!("HARNESS {e}\n"),
                };
            std::fs::write(&out, text)?;
        }
        // one-shot triage: every applicable transform on <file>, flips
        // confirmed in a fresh child
        Some("typemorph") => {
            let f = args
                .get(2)
                .cloned()
                .ok_or_else(|| anyhow::anyhow!("typemorph <file>"))?;
            let code = std::fs::read_to_string(&f)?;
            let flips =
                graphix_fuzz::typemorph_scan(vec![(f.clone(), code)], timeout()).await;
            for (name, detail) in &flips {
                println!("TYPEFLIP {name} {detail}");
            }
            if flips.is_empty() {
                println!("typemorph: no flips");
            } else {
                std::process::exit(1);
            }
        }
        // the acceptance-plane gate: corpus + n generated subjects
        Some("typemorph-scan") => {
            let n: usize = args.get(2).and_then(|s| s.parse().ok()).unwrap_or(100);
            let seed: u64 = args.get(3).and_then(|s| s.parse().ok()).unwrap_or(1);
            let mut programs: Vec<(String, String)> =
                graphix_fuzz::corpus::REGRESSION_CORPUS
                    .iter()
                    .map(|(name, prog)| (name.to_string(), prog.to_string()))
                    .collect();
            let corpus_n = programs.len();
            let mut rng = graphix_fuzz::mutate::Rng::new(seed);
            for i in 0..n {
                programs.push((format!("gen#{i}"), gen_one(&mut rng)));
            }
            let total = programs.len();
            let flips = graphix_fuzz::typemorph_scan(programs, timeout()).await;
            for (name, detail) in &flips {
                println!("TYPEFLIP {name}: {detail}");
            }
            println!(
                "typemorph-scan: {total} programs ({corpus_n} corpus + {n} generated), {} flips",
                flips.len()
            );
            if !flips.is_empty() {
                std::process::exit(1);
            }
        }
        // the determinism gate: every Exact-tier program in two fresh
        // child processes, normalized CLIF dumps compared. Non-Exact
        // tiers are skipped: IO pacing varies which slots populate.
        Some("detcheck") => {
            let n: usize = args.get(2).and_then(|s| s.parse().ok()).unwrap_or(200);
            let seed: u64 = args.get(3).and_then(|s| s.parse().ok()).unwrap_or(1);
            let mut programs: Vec<(String, String)> =
                graphix_fuzz::corpus::REGRESSION_CORPUS
                    .iter()
                    .map(|(name, prog)| (name.to_string(), prog.to_string()))
                    .collect();
            {
                let mut rng = graphix_fuzz::mutate::Rng::new(seed);
                for i in 0..n {
                    programs.push((format!("gen#{i}"), gen_one(&mut rng)));
                }
            }
            let total = programs.len();
            programs.retain(|(_, prog)| {
                graphix_fuzz::oracle_tier(prog) == graphix_fuzz::OracleTier::Exact
            });
            let skipped = total - programs.len();
            let flaps = graphix_fuzz::detcheck(programs, timeout()).await;
            for (name, detail) in &flaps {
                println!("FLAP {name}: {detail}");
            }
            println!(
                "detcheck: {total} programs ({} corpus + {n} generated, \
                 {skipped} non-Exact skipped), {} flaps",
                total - n,
                flaps.len()
            );
            if !flaps.is_empty() {
                std::process::exit(1);
            }
        }
        // batch worker: length-prefixed programs on stdin, verdicts
        // appended and flushed to the file named by argv[2] so a
        // mid-batch death leaves the completed prefix on record
        Some("check-batch") => {
            use std::io::Write;
            let verdict_path = args
                .get(2)
                .cloned()
                .ok_or_else(|| anyhow::anyhow!("check-batch requires a verdict path"))?;
            let input = read_stdin()?;
            let progs = parse_batch_frames(&input)?;
            let mut out = std::fs::File::create(&verdict_path)?;
            graphix_fuzz::run_batch(&progs, campaign_timeout(), |i, v| {
                let tag = match v {
                    graphix_fuzz::BatchVerdict::Agree { ran: true } => "R",
                    graphix_fuzz::BatchVerdict::Agree { ran: false } => "A",
                    graphix_fuzz::BatchVerdict::Other => "O",
                };
                let _ = writeln!(out, "{i} {tag}");
                let _ = out.flush();
            })
            .await;
            graphix_fuzz::report_self_cpu();
        }
        // the aggregator's worker: told what to make, not what to run
        Some("gen-batch") => {
            let out_path = args
                .get(2)
                .cloned()
                .ok_or_else(|| anyhow::anyhow!("gen-batch requires an output path"))?;
            let order = graphix_fuzz::WorkOrder::decode(&read_stdin()?)?;
            let mut out = std::fs::File::create(&out_path)?;
            graphix_fuzz::run_work_order(&order, campaign_timeout(), &mut out).await;
        }
        Some("check-one") => {
            let code = read_stdin()?;
            // 0 = agree; 7 = agree and both modes produced runtime
            // traces; 10 = diverge
            let status =
                match graphix_fuzz::check_classified(code.trim(), campaign_timeout())
                    .await
                {
                    (Some(_), _) => 10,
                    (None, true) => 7,
                    (None, false) => 0,
                };
            graphix_fuzz::report_self_cpu();
            std::process::exit(status);
        }
        // isolated selfcheck worker: 0 = clean, 40+mask (bit 1 interp
        // flaky, bit 2 jit flaky), 50 = inconclusive
        Some("selfcheck-one") => {
            let code = read_stdin()?;
            let mut mask = 0;
            let mut inconclusive = false;
            for mode in graphix_fuzz::selfcheck_one(code.trim(), timeout()).await {
                match mode {
                    "interp" => mask |= 1,
                    "jit" => mask |= 2,
                    // timed out on the confirm pair: the budget decided
                    "inconclusive" => inconclusive = true,
                    _ => mask |= 3,
                }
            }
            std::process::exit(if mask != 0 {
                40 + mask
            } else if inconclusive {
                50
            } else {
                0
            });
        }
        // isolated minimizer: program on stdin, reduced program written
        // to the file named by argv[2]
        Some("minimize-one") => {
            let out_path = args
                .get(2)
                .cloned()
                .ok_or_else(|| anyhow::anyhow!("minimize-one requires an output path"))?;
            let code = read_stdin()?;
            let (min, _) =
                minimize(code.trim(), campaign_timeout(), CAMPAIGN_MINIMIZE_BUDGET).await;
            std::fs::write(&out_path, min)?;
        }
        Some(cmd @ ("generate" | "fuzz")) => {
            // `forever`/`0` runs until killed; the corpus is loaded up
            // front so a finding is never re-reported
            let iters = parse_iters(args.get(2), if cmd == "fuzz" { 50 } else { 100 });
            let seed: u64 = args.get(3).and_then(|s| s.parse().ok()).unwrap_or(1);
            // `GRAPHIX_FUZZ_CORPUS` overrides the corpus dir. Concurrent
            // campaigns must not share one (colliding finding indices),
            // and the default lives outside the synced repo.
            let out = match std::env::var_os("GRAPHIX_FUZZ_CORPUS") {
                Some(p) => std::path::PathBuf::from(p),
                None => std::env::home_dir()
                    .map(|h| h.join("tmp/target/fuzz/crashes"))
                    .unwrap_or_else(|| "fuzz/crashes".into()),
            };
            let corpus = Arc::new(Corpus::load(&out));
            println!(
                "corpus: {} existing divergences loaded from {}/",
                corpus.len(),
                out.display()
            );
            // regression gate first
            let regressions = print_regression().await;
            let before = corpus.len();
            println!(
                "{cmd}: iters={} seed={seed} → {}/",
                fmt_iters(iters),
                out.display()
            );
            let stats = if cmd == "fuzz" {
                fuzz(iters, seed, campaign_timeout(), &corpus).await
            } else {
                generate_campaign(iters, seed, campaign_timeout(), &corpus, reactive)
                    .await
            };
            let new = corpus.len() - before;
            println!(
                "done: {} programs, {} divergences, {} crashes \
                 ({new} new, {} total in corpus), {} novel shapes",
                stats.run,
                stats.divergences,
                stats.crashes,
                corpus.len(),
                stats.novel
            );
            if new > 0 || regressions > 0 {
                std::process::exit(1);
            }
        }
        // a whole campaign in one process: all three sources through one
        // pool, divided by measured CPU
        Some("soak") => {
            let iters = parse_iters(args.get(2), 100);
            let seed: u64 = args.get(3).and_then(|s| s.parse().ok()).unwrap_or(1);
            let mix = args.get(4).map(String::as_str).unwrap_or(DEFAULT_MIX).to_string();
            let w = parse_mix(&mix)?;
            let out = match std::env::var_os("GRAPHIX_FUZZ_CORPUS") {
                Some(p) => std::path::PathBuf::from(p),
                None => std::env::home_dir()
                    .map(|h| h.join("tmp/target/fuzz/crashes"))
                    .unwrap_or_else(|| "fuzz/crashes".into()),
            };
            let corpus = Arc::new(Corpus::load(&out));
            println!(
                "corpus: {} existing divergences loaded from {}/",
                corpus.len(),
                out.display()
            );
            let regressions = print_regression().await;
            let before = corpus.len();
            println!(
                "soak: iters={} seed={seed} mix={mix} → {}/",
                fmt_iters(iters),
                out.display()
            );
            // per-source seed streams stay separate so a subject is
            // reproducible from its source and seed
            let per_source =
                graphix_fuzz::run_aggregator(&corpus, iters, campaign_timeout(), w).await;
            let new = corpus.len() - before;
            let total: f64 = per_source.iter().map(|(_, _, c)| c.as_secs_f64()).sum();
            for (name, stats, cpu) in &per_source {
                let pct =
                    if total > 0.0 { cpu.as_secs_f64() * 100.0 / total } else { 0.0 };
                println!(
                    "done {name}: {} programs, {} divergences, {} crashes, \
                     {} novel shapes, {:.0}% cpu",
                    stats.run, stats.divergences, stats.crashes, stats.novel, pct
                );
            }
            println!("{new} new, {} total in corpus", corpus.len());
            if new > 0 || regressions > 0 {
                std::process::exit(1);
            }
        }
        Some("minimize") => {
            let path = match args.get(2) {
                Some(p) => p,
                None => bail!("usage: graphix-fuzz minimize <file> [budget]"),
            };
            // the budget is oracle checks; interactive, so generous
            let budget = args.get(3).and_then(|s| s.parse().ok()).unwrap_or(4000);
            let code = std::fs::read_to_string(path)?;
            let (min, calls) = minimize(code.trim(), timeout(), budget).await;
            match check(&min, timeout()).await {
                None => println!("no divergence to minimize (program agrees)"),
                Some(d) => {
                    println!("minimized ({calls} checks) — {}", d.bisect());
                    println!("{min}");
                    println!("  interp={} jit={}", render(&d.interp), render(&d.jit));
                }
            }
        }
        Some(cmd @ ("check" | "run")) => {
            let path = match args.get(2) {
                Some(p) => p,
                None => bail!("usage: graphix-fuzz {cmd} <file>"),
            };
            let code = std::fs::read_to_string(path)?;
            let code = code.trim();
            match cmd {
                "run" => {
                    let routes: &[graphix_fuzz::Route] =
                        if graphix_fuzz::callable::has_header(code) {
                            &[
                                graphix_fuzz::Route::InLanguage,
                                graphix_fuzz::Route::Dispatch,
                            ]
                        } else {
                            &[graphix_fuzz::Route::InLanguage]
                        };
                    for (mode, &route) in [Mode::Interp, Mode::Jit]
                        .into_iter()
                        .flat_map(|m| routes.iter().map(move |r| (m, r)))
                    {
                        let (o, stats) = graphix_fuzz::run_program_with_stats_routed(
                            code,
                            mode,
                            route,
                            timeout(),
                        )
                        .await;
                        println!("{mode:?}/{route:?}: {}", render(&o));
                        if !matches!(mode, Mode::Interp) {
                            println!(
                                "  fusion: attempted={} fused={} jit_generations={}",
                                stats.attempted, stats.fused, stats.jit_generations
                            );
                            for failure in &stats.failed {
                                println!("  failed {:?}: {}", failure.id, failure.reason);
                            }
                        }
                    }
                    for (mode, &route) in [Mode::Interp, Mode::Jit]
                        .into_iter()
                        .flat_map(|m| routes.iter().map(move |r| (m, r)))
                    {
                        let s = graphix_fuzz::run_sessions(code, mode, route, timeout())
                            .await;
                        for (name, o) in [
                            ("nocache", &s.nocache),
                            ("cold", &s.cold),
                            ("warm", &s.warm),
                        ] {
                            println!("{mode:?}/{route:?}/{name}: {}", render(o));
                        }
                    }
                }
                "check" => match check(code, timeout()).await {
                    None => println!(
                        "AGREE — interp and jit, no cache, cold and warm produce the same result"
                    ),
                    Some(d) => {
                        let (la, lb) = d.labels();
                        println!("DIVERGENCE — {}", d.bisect());
                        println!("  {la}: {}", render(&d.interp));
                        println!("  {lb}: {}", render(&d.jit));
                        drop(cwd_guard);
                        std::process::exit(1);
                    }
                },
                _ => unreachable!(),
            }
        }
        _ => bail!(
            "usage: graphix-fuzz <check|run|minimize|typemorph> <file>  |  \
             graphix-fuzz soak [iters] [seed] [fuzz:generate:reactive]  |  \
             graphix-fuzz <fuzz|generate> [iters] [seed] [--reactive]  |  \
             graphix-fuzz <gen|gen-check> [n] [seed] [--reactive]  |  \
             graphix-fuzz reactive-check [n] [seed]  |  \
             graphix-fuzz typemorph-scan [n] [seed]  |  \
             graphix-fuzz selfcheck [iters] [seed]  |  graphix-fuzz regress"
        ),
    }
    Ok(())
}

/// `VmRSS` of `pid` in kB (Linux `/proc`).
fn vm_rss_kb(pid: u32) -> Option<u64> {
    let status = std::fs::read_to_string(format!("/proc/{pid}/status")).ok()?;
    status
        .lines()
        .find(|l| l.starts_with("VmRSS:"))?
        .split_whitespace()
        .nth(1)?
        .parse()
        .ok()
}

/// Long-running leak witnesses for `leakcheck`. The control rows keep
/// the gate honest: a shared baseline drift fails nothing.
const LEAK_WITNESSES: &[(&str, &str)] = &[
    (
        // fused handler-less `$` minting an owned error every tick
        "qop-scalar-error",
        "let clk = sys::time::timer(duration:0.001s, true);\n\
         let x = i64:0;\n\
         x <- clk ~ (x + i64:1);\n\
         let d = (i64:10 /? (x - x))$;\n\
         let s = d + i64:1;\n\
         s\n",
    ),
    (
        // control: same shape, divisor never 0
        "qop-scalar-control",
        "let clk = sys::time::timer(duration:0.001s, true);\n\
         let x = i64:0;\n\
         x <- clk ~ (x + i64:1);\n\
         let d = (i64:10 /? (x - x + i64:1))$;\n\
         let s = d + i64:1;\n\
         s\n",
    ),
    (
        // a fresh owned string per tick through a fused result
        "string-churn",
        "let clk = sys::time::timer(duration:0.001s, true);\n\
         let x = i64:0;\n\
         x <- clk ~ (x + i64:1);\n\
         str::to_upper(\"[x]abc\")\n",
    ),
    (
        // a fused select arm's owned variant-payload bind
        "select-payload-bind",
        "let clk = sys::time::timer(duration:0.001s, true);\n\
         let x = i64:0;\n\
         x <- clk ~ (x + i64:1);\n\
         select `A([x, i64:2, i64:3]) { `A(xs) => array::len(xs) }\n",
    ),
    (
        // the list face: `ListHead`/`ListTail` clones in a list-pattern arm
        "select-list-binds",
        "let clk = sys::time::timer(duration:0.001s, true);\n\
         let x = i64:0;\n\
         x <- clk ~ (x + i64:1);\n\
         select [<[x, i64:2], [i64:3, i64:4]>] {\n\
             [<h, t..>] => array::len(h),\n\
             [<>] => i64:0\n\
         }\n",
    ),
];
