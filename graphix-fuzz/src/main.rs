//! `graphix-fuzz` — differential model-checking fuzzer CLI.
//!
//! V1 subcommands:
//!   graphix-fuzz check <file>   run interp vs jit, report any divergence
//!   graphix-fuzz run   <file>   run all three modes, print each outcome
//!
//! `check` is the primitive both the mechanical fuzzer (forthcoming) and
//! the adversarial agent sources depend on. See design/graphix_fuzz.md.

use anyhow::{Result, bail};
use graphix_fuzz::{
    CAMPAIGN_MINIMIZE_BUDGET, Corpus, Mode, Outcome, check, fuzz, generate_campaign,
    minimize, regression_corpus_len, run_regression,
};
use std::{
    sync::{Arc, LazyLock},
    time::Duration,
};

// The harness allocates like the compiler it drives, and subjects are
// re-execs of this binary — mimalloc cuts the glibc malloc tail (~12%
// of subject CPU in the prof22 profiling round; the cold-start alloc
// storm is exactly what a modern allocator absorbs). Harness-only:
// the shell/compiler crates are untouched.
#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

/// Default soak mix, as CPU shares `fuzz:generate:reactive`.
///
/// Weighted by measured yield per CPU-second, not by taste. Over the
/// five days to 2026-08-17 the three sources produced 14/7/14 findings
/// while drawing 14%/19%/67% of the box — so per unit of CPU, mutation
/// is ~4.8x the reactive generator and ~2.7x the plain one. The mix
/// still funds both generators well past their share of findings,
/// because they explore shapes mutation cannot reach and the yield
/// estimate is noisy (14 vs 7 is only ~1.7 sigma).
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

// The base budgets below are tuned for ryouko; the soak fleet's other
// machines are slower and non-uniform, so every budget scales by
// GRAPHIX_FUZZ_TIMEOUT_SCALE (an integer multiplier, default 1, set
// per machine at campaign launch like GRAPHIX_FUZZ_PAR). Scaling at
// the source keeps every derived margin — the isolated child's outer
// deadline, the check() escalation floor's *relative* part — coherent.
static TIMEOUT_SCALE: LazyLock<u32> = LazyLock::new(|| {
    std::env::var("GRAPHIX_FUZZ_TIMEOUT_SCALE")
        .ok()
        .and_then(|s| s.parse().ok())
        .map(|n: u32| n.clamp(1, 100))
        .unwrap_or(1)
});

fn timeout() -> Duration {
    Duration::from_secs(10) * *TIMEOUT_SCALE
}

// A regression surfaces fast (crash / value mismatch); a legitimately-
// bottom program just needs to confirm "still all-Timeout", so a short
// per-program timeout keeps the gate quick even as the corpus grows.
fn regress_timeout() -> Duration {
    Duration::from_secs(3) * *TIMEOUT_SCALE
}

// Campaign timeout: generated/mutated programs terminate in milliseconds
// or produce bottom — a short timeout means a bottom program doesn't sleep
// 30s (3 modes × 10s), so the worker pool refills fast and the cores stay
// busy. A real divergence (value mismatch / crash) surfaces well within 3s.
fn campaign_timeout() -> Duration {
    Duration::from_secs(3) * *TIMEOUT_SCALE
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
        Outcome::CompileErr(e) => format!("CompileErr({})", first_line(e)),
        Outcome::RuntimeErr(e) => format!("RuntimeErr({})", first_line(e)),
        Outcome::Timeout => "Timeout".to_string(),
    }
}

fn first_line(s: &str) -> String {
    s.lines().next().unwrap_or("").to_string()
}

/// Per-FEATURE compile rates for gen-check/reactive-check: bucket the
/// sample by source markers (v1 — substring buckets, not generator-rule
/// attribution) and report each bucket's compile rate. The aggregate
/// rate hides a dead arm when the arm is a few percent of output — the
/// try/catch migration left the error-lambda arm emitting deleted
/// syntax for WEEKS behind a healthy-looking 99% (fuzzer gap 2,
/// 2026-08-07). A 0%% row is a dead arm; an "absent" row is an arm
/// that stopped firing at all.
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
        ("str", "str::"),
        ("re", "re::"),
        ("variant", "`"),
        ("connect", "<-"),
        ("cast", "cast<"),
        ("refs", "&"),
        ("modules", "mod "),
        ("files", "file-v1"),
        ("reactive", "schedule-v1"),
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
/// arbitrary text (delimiters are corruptible).
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

// The driver stays at TWO worker threads, deliberately.
//
// It looks wrong — one process coordinating up to `par` children on a
// 20-core box — and sizing it per-core is a 7.5x THROUGHPUT REGRESSION,
// measured on hz0 at par=160: 2 threads gave 70% box utilization and
// 2.59M subjects/120s, 6 threads 56% and 586k, 20 threads 37% and 347k.
// The work is in the CHILDREN, each with its own runtime; every thread
// the parent takes is one it steals from them, on a box already
// oversubscribed 8x. The parent's own job is coordination, which is
// I/O-bound and fits in two.
#[tokio::main(flavor = "multi_thread", worker_threads = 2)]
async fn main() -> Result<()> {
    // `--reactive` selects the reactive (scheduled) generator wherever
    // a generator is used; it's stripped before positional parsing so
    // `generate --reactive 500 42` and `generate 500 42 --reactive`
    // both work.
    let mut args: Vec<String> = std::env::args().collect();
    let reactive = args.iter().any(|a| a == "--reactive");
    args.retain(|a| a != "--reactive");
    // Generated/mutated programs freely call `sys::fs::write_all` &
    // co. with arbitrary short strings as paths — executed with an
    // inherited cwd they litter the campaign launch directory (the
    // repo root filled with files named `bar`, `hello world`, `,` …).
    // The per-subject WORKER processes (program on stdin, no path
    // args) are sandboxed by the SPAWNING campaign (lib.rs
    // `sandbox_cwd`: parent-owned tempdir as the child's cwd, removed
    // after the child exits, signalled via GRAPHIX_FUZZ_SANDBOXED). A
    // child-owned tempdir here leaked on `process::exit` (worker arms
    // skip drops) and a soak's millions of subjects exhausted /tmp's
    // INODES (jul10d). The self-sandbox below covers MANUAL
    // invocations only; for the single-file commands (`check`/`run`)
    // the file argument is made absolute first.
    let sandbox_cwd = std::env::var_os("GRAPHIX_FUZZ_SANDBOXED").is_none()
        && match args.get(1).map(String::as_str) {
            Some(
                "check-one" | "check-batch" | "gen-batch" | "detcheck-one"
                | "selfcheck-one" | "minimize-one" | "gen-check" | "regress"
                | "fusecheck" | "leakcheck",
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
    // Captured BEFORE the sandbox chdir: file outputs that belong in
    // the invoking directory (fusecheck --bless) resolve against it.
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
            // Debug: print N generated programs (no oracle) to eyeball the
            // generator's output.
            let n: usize = args.get(2).and_then(|s| s.parse().ok()).unwrap_or(10);
            let seed: u64 = args.get(3).and_then(|s| s.parse().ok()).unwrap_or(1);
            let mut rng = graphix_fuzz::mutate::Rng::new(seed);
            for _ in 0..n {
                println!("{}\n", gen_one(&mut rng));
            }
        }
        Some("gen-check") => {
            // Generator health: compile rate + reject reasons. The
            // generator is type-correct by construction, so every
            // reject is a generator bug or a rule to tune — this is
            // the instrument for every vocabulary stage.
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
            // Bucket by the innermost anyhow cause (the last line) — the
            // outer layers are per-program position context. Keep one
            // example program per bucket: the reject is only actionable
            // next to the text that provoked it.
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
            // program (with its full error as a trailing comment) to
            // `<dir>/reject_<i>.gx` — the inline one-example-per-bucket
            // print mangles multi-line programs (dynmod raw strings),
            // so byte-exact repro needs the file.
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
            // RSS leak lane (fuzzer gap 5): run each embedded
            // long-running witness under BOTH modes on the given
            // graphix shell binary, sample VmRSS at 5s and 5+secs,
            // and require the fused slope within headroom of the
            // interp slope. qop-scalar-error-leak (+11MB/90s) is the
            // class this exists for — invisible to every value
            // oracle. Manual/CI gate; Linux only (/proc).
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
                // Headroom: the pin's leak was ~+120 kB/s over a
                // ~30 kB/s shared timer baseline; 50 kB/s of slack
                // rides load noise without hiding a real leak at
                // 60s (3MB vs 7MB delta).
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
            // Fusion-coverage manifest gate (fuzzer gap 6): per-corpus
            // fused-region counts vs the checked-in manifest — a
            // silent de-fusion regression fails LOUD. Manual/CI gate;
            // run from the repo root. `--bless` rewrites the manifest
            // (rebuild afterward — the compare reads the EMBEDDED
            // copy). Counts are measured COMPILE-only; an unmeasurable
            // count is a gate failure, never a 0 — bless refuses to
            // write anything rather than bake one in.
            let bless = args.iter().any(|a| a == "--bless");
            let timeout = Duration::from_secs(60) * *TIMEOUT_SCALE;
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
            // Reactive-generator health beyond compile rate: programs
            // must QUIESCE within their trace budget (runaways are the
            // deliberate few percent) and injection epochs must
            // actually ADVANCE the trace (an all-quiet epoch tail is
            // this stage's silent-loss mode — a generator that stopped
            // wiring inputs into observable results would still
            // compile fine). Runs each program under interp only (the
            // health of the GENERATOR, not the differential).
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
                            // Injection epochs advanced iff any epoch
                            // past the compile burst produced events.
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
            // Oracle-soundness gate: per-mode trace determinism over the
            // corpus + generated programs. Must be 100% before any
            // interp-vs-jit trace finding is trusted.
            let iters = args.get(2).and_then(|s| s.parse().ok()).unwrap_or(1000);
            let seed: u64 = args.get(3).and_then(|s| s.parse().ok()).unwrap_or(1);
            let total = regression_corpus_len(); // corpus part; rest generated
            println!(
                "selfcheck: {iters} generated (seed={seed}) + corpus \
                 (≥{total} seeds), twice per mode"
            );
            // The generous per-run timeout keeps loaded-gate JIT runs
            // (which pay compile cost under heavy parallelism) from
            // breaching the backstop and reading as flakes.
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
        // Hidden: the isolated-check worker the campaign pool spawns
        // (program on stdin). The verdict rides the EXIT CODE (0 =
        // agree, 10 = diverge), NOT stdout: the program under test can
        // write to stdout itself (`sys::io::stdout`) and corrupt any
        // in-band line protocol — a write_exact mutant read as "no
        // VERDICT line" and recorded a false crash (soak jul06g). A
        // program that kills the evaluator kills only this process
        // (any other status) — the parent records a crash finding.
        // See lib.rs `check_isolated`.
        // Hidden: the detcheck child (program on stdin; the parent sets
        // GRAPHIX_DUMP_CLIF=1 and reads the dump from OUR stderr). The
        // program is DRIVEN to quiescence, not just compiled: per-slot
        // HOF kernels compile lazily as slots populate at runtime, and
        // a compile-only child raced the runtime's first cycles against
        // shutdown — whether those kernels appeared in the dump was a
        // scheduling coin flip. Driving every epoch to quiescence
        // saturates the lazy-compile set, which for an Exact-tier
        // program is a pure function of the text (the parent only feeds
        // Exact tier). Exit 0 = ran, 3 = compile reject (the reject
        // message prints to stderr and is part of the compared output —
        // rejection must be deterministic too), 4 = wall-clock timeout
        // (the cut is inherently racy; the parent skips the pair).
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
        // The determinism gate: run every Exact-tier corpus finding
        // (plus N generated programs) in TWO fresh child processes each
        // — each child gets its own ASLR — and compare normalized CLIF
        // dumps. Fusion shape must be a pure function of the program
        // text; a flap here is an allocation-order dependence in
        // typing/resolution/fusion (the #19 class). Non-Exact tiers are
        // skipped: IO pacing legitimately varies which slots ever
        // populate, so their lazy-compile set is not a function of the
        // text alone.
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
        // Hidden: the batch worker (length-prefixed programs on stdin;
        // per-subject verdicts appended + flushed to the FILE named by
        // the extra argument — stdout is corruptible by the programs
        // under test, and incremental flushing means a mid-batch death
        // leaves the completed prefix on record for the parent's
        // individual-re-run fallback). One warmed runtime pair serves
        // the whole batch — the per-subject stdlib-compile constant is
        // the fleet throughput bound (design/interp_lazy_bind_cost.md,
        // the actual-soak profile).
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
            // What this batch cost, for the source that asked for it.
            graphix_fuzz::report_self_cpu();
        }
        // The aggregator's worker: it is told WHAT to make, not what to
        // run, so the parent never generates or ships program text.
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
            // 0 = agree; 7 = agree AND both modes produced runtime
            // traces (the parent's ring-admission bar); 10 = diverge.
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
        // Hidden: the isolated selfcheck worker (program on stdin;
        // verdict in the EXIT CODE for the same stdout-pollution
        // reason: 0 = clean, 40+mask with bit 1 = interp flaky, bit 2 =
        // jit flaky). Child-per-subject keeps the deliberate JIT leak
        // from accumulating in the gate process — see lib.rs
        // `selfcheck_isolated`.
        Some("selfcheck-one") => {
            let code = read_stdin()?;
            let mut mask = 0;
            let mut inconclusive = false;
            for mode in graphix_fuzz::selfcheck_one(code.trim(), timeout()).await {
                match mode {
                    "interp" => mask |= 1,
                    "jit" => mask |= 2,
                    // Timed out at 4x on the confirm pair: the budget,
                    // not the engine, decided. Reported separately so a
                    // gate that stops covering subjects says so.
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
        // Hidden: the isolated minimizer (program on stdin, the reduced
        // program written to the FILE named by the extra argument —
        // stdout can be polluted by the programs the minimizer runs). A
        // reduction that crashes kills only this process — the parent
        // falls back to recording the unminimized mutant.
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
            // `iters` may be `forever`/`0` to run until killed, surfacing new
            // divergences live. The corpus is loaded up front so a campaign
            // never re-reports a finding it (or a prior run) already saved.
            let iters = parse_iters(args.get(2), if cmd == "fuzz" { 50 } else { 100 });
            let seed: u64 = args.get(3).and_then(|s| s.parse().ok()).unwrap_or(1);
            // `GRAPHIX_FUZZ_CORPUS` overrides the corpus dir — concurrent
            // soak campaigns must NOT share one: each process loads the
            // max index at startup and writes findings with its own
            // counter, so two campaigns on one dir silently clobber
            // each other's findings at colliding indices.
            // The DEFAULT lives OUTSIDE the repo: the repo's fuzz/ dir
            // is synced across machines (syncthing), and a campaign is
            // an artifact firehose — the jul10d environment breakage
            // wrote garbage findings into the repo at 300MB/s.
            // ~/tmp/target is build scratch (never synced, cleaned
            // freely) — durable triage summaries still belong in the
            // repo, written by hand.
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
            // Regression gate first: re-check every saved finding so a fixed
            // bug coming back is caught loudly before we hunt for new ones.
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
            // (Only reached in finite mode; `forever` runs until killed.)
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
        // A whole campaign in ONE process: all three sources through one
        // pool, divided by MEASURED CPU. Three lane processes could only
        // divide a box through the OS scheduler, which arbitrates
        // between runnable processes, so equal worker counts bought
        // wildly unequal CPU (13/19/66 measured, reactive taking two
        // thirds while looking evenly provisioned). Weights are CPU
        // shares, not slot counts.
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
            // Per-source seed streams are kept SEPARATE (the same
            // +1000/+2000 offsets the three lanes used), so a subject
            // stays reproducible from its source and seed even though
            // the interleaving is now resource-dependent.
            // The parent aggregates; the children generate. Its cost is
            // per batch and per finding, never per subject.
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
            // The budget is oracle CHECKS, and a big finding wants a big
            // one: reduction is greedy per round, so the last bytes cost
            // the most checks. Interactive, so the default is generous
            // (the campaign's `minimize-one` keeps its own tight budget).
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
                    for mode in [Mode::Interp, Mode::Jit] {
                        let (o, stats) =
                            graphix_fuzz::run_program_with_stats(code, mode, timeout())
                                .await;
                        println!("{mode:?}: {}", render(&o));
                        // Stats are compile-time fusion counters; only the
                        // fusing modes have anything to say.
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
                }
                "check" => match check(code, timeout()).await {
                    None => println!("AGREE — interp and jit produce the same result"),
                    Some(d) => {
                        println!("DIVERGENCE — {}", d.bisect());
                        println!("  interp: {}", render(&d.interp));
                        println!("  jit:    {}", render(&d.jit));
                        drop(cwd_guard);
                        std::process::exit(1);
                    }
                },
                _ => unreachable!(),
            }
        }
        _ => bail!(
            "usage: graphix-fuzz <check|run|minimize> <file>  |  \
             graphix-fuzz soak [iters] [seed] [fuzz:generate:reactive]  |  \
             graphix-fuzz <fuzz|generate> [iters] [seed] [--reactive]  |  \
             graphix-fuzz <gen|gen-check> [n] [seed] [--reactive]  |  \
             graphix-fuzz reactive-check [n] [seed]  |  \
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

/// Long-running leak witnesses for `leakcheck` — reactive programs a
/// value oracle can never leak-check. Each pairs with the finding
/// that motivated it; the control rows keep the gate honest (a shared
/// baseline drift fails nothing).
const LEAK_WITNESSES: &[(&str, &str)] = &[
    (
        // qop-scalar-error-leak-aug2026: the fused handler-less `$`
        // minted an owned ArithError every tick and never dropped it.
        "qop-scalar-error",
        "let clk = sys::time::timer(duration:0.001s, true);\n\
         let x = i64:0;\n\
         x <- clk ~ (x + i64:1);\n\
         let d = (i64:10 /? (x - x))$;\n\
         let s = d + i64:1;\n\
         s\n",
    ),
    (
        // Control: same shape, divisor never 0 — no error is minted.
        "qop-scalar-control",
        "let clk = sys::time::timer(duration:0.001s, true);\n\
         let x = i64:0;\n\
         x <- clk ~ (x + i64:1);\n\
         let d = (i64:10 /? (x - x + i64:1))$;\n\
         let s = d + i64:1;\n\
         s\n",
    ),
    (
        // String-churn steady state: a fresh owned ArcStr per tick
        // through a fused DynCall result — the owned-result drop
        // discipline under sustained fire.
        "string-churn",
        "let clk = sys::time::timer(duration:0.001s, true);\n\
         let x = i64:0;\n\
         x <- clk ~ (x + i64:1);\n\
         str::to_upper(\"[x]abc\")\n",
    ),
];
