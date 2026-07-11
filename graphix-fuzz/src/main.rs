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
    Corpus, Mode, Outcome, check, fuzz, generate_campaign, minimize,
    regression_corpus_len, run_regression,
};
use std::{sync::Arc, time::Duration};

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

const TIMEOUT: Duration = Duration::from_secs(10);
// A regression surfaces fast (crash / value mismatch); a legitimately-
// bottom program just needs to confirm "still all-Timeout", so a short
// per-program timeout keeps the gate quick even as the corpus grows.
const REGRESS_TIMEOUT: Duration = Duration::from_secs(3);
// Campaign timeout: generated/mutated programs terminate in milliseconds
// or produce bottom — a short timeout means a bottom program doesn't sleep
// 30s (3 modes × 10s), so the worker pool refills fast and the cores stay
// busy. A real divergence (value mismatch / crash) surfaces well within 3s.
const CAMPAIGN_TIMEOUT: Duration = Duration::from_secs(3);

async fn print_regression() -> usize {
    let regr = run_regression(REGRESS_TIMEOUT).await;
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
            format!("Trace({})", epochs.join("; "))
        }
        Outcome::CompileErr(e) => format!("CompileErr({})", first_line(e)),
        Outcome::RuntimeErr(e) => format!("RuntimeErr({})", first_line(e)),
        Outcome::Timeout => "Timeout".to_string(),
    }
}

fn first_line(s: &str) -> String {
    s.lines().next().unwrap_or("").to_string()
}

/// Read a whole program from stdin (the `check-one` / `minimize-one`
/// isolated-worker input channel).
fn read_stdin() -> Result<String> {
    use std::io::Read;
    let mut buf = String::new();
    std::io::stdin().read_to_string(&mut buf)?;
    Ok(buf)
}

#[tokio::main]
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
                "check-one" | "detcheck-one" | "selfcheck-one" | "minimize-one"
                | "gen-check" | "regress",
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
            while let Some(res) = set.join_next().await {
                match res {
                    Ok((_, None)) => compiled += 1,
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
                    (i, graphix_fuzz::run_program(&p, Mode::Interp, TIMEOUT).await)
                });
            };
            while next < progs.len() && set.len() < par {
                spawn(&mut set, next, progs[next].clone());
                next += 1;
            }
            let (mut compiled, mut quiesced, mut advanced, mut wedged) = (0, 0, 0, 0);
            let mut rejects: std::collections::BTreeMap<String, usize> =
                std::collections::BTreeMap::new();
            while let Some(res) = set.join_next().await {
                if let Ok((_, out)) = res {
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
            let flaky = graphix_fuzz::selfcheck(iters, seed, TIMEOUT).await;
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
            match graphix_fuzz::run_program(code.trim(), Mode::Jit, TIMEOUT).await {
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
            let mut programs: Vec<(String, String)> = graphix_fuzz::corpus::REGRESSION_CORPUS
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
            let flaps = graphix_fuzz::detcheck(programs, TIMEOUT).await;
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
        Some("check-one") => {
            let code = read_stdin()?;
            let status = match check(code.trim(), CAMPAIGN_TIMEOUT).await {
                None => 0,
                Some(_) => 10,
            };
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
            for mode in graphix_fuzz::selfcheck_one(code.trim(), TIMEOUT).await {
                match mode {
                    "interp" => mask |= 1,
                    "jit" => mask |= 2,
                    _ => mask |= 3,
                }
            }
            std::process::exit(if mask == 0 { 0 } else { 40 + mask });
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
            let (min, _) = minimize(code.trim(), CAMPAIGN_TIMEOUT, 80).await;
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
                fuzz(iters, seed, CAMPAIGN_TIMEOUT, &corpus).await
            } else {
                generate_campaign(iters, seed, CAMPAIGN_TIMEOUT, &corpus, reactive).await
            };
            // (Only reached in finite mode; `forever` runs until killed.)
            let new = corpus.len() - before;
            println!(
                "done: {} programs, {} divergences, {} crashes \
                 ({new} new, {} total in corpus)",
                stats.run,
                stats.divergences,
                stats.crashes,
                corpus.len()
            );
            if new > 0 || regressions > 0 {
                std::process::exit(1);
            }
        }
        Some("minimize") => {
            let path = match args.get(2) {
                Some(p) => p,
                None => bail!("usage: graphix-fuzz minimize <file>"),
            };
            let code = std::fs::read_to_string(path)?;
            let (min, calls) = minimize(code.trim(), TIMEOUT, 200).await;
            match check(&min, TIMEOUT).await {
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
                            graphix_fuzz::run_program_with_stats(code, mode, TIMEOUT)
                                .await;
                        println!("{mode:?}: {}", render(&o));
                        // Stats are compile-time fusion counters; only the
                        // fusing modes have anything to say.
                        if !matches!(mode, Mode::Interp) {
                            println!(
                                "  fusion: attempted={} fused={}",
                                stats.attempted, stats.fused
                            );
                            for (id, why) in &stats.failed {
                                println!("  failed {id:?}: {why}");
                            }
                        }
                    }
                }
                "check" => match check(code, TIMEOUT).await {
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
             graphix-fuzz <fuzz|generate> [iters] [seed] [--reactive]  |  \
             graphix-fuzz <gen|gen-check> [n] [seed] [--reactive]  |  \
             graphix-fuzz reactive-check [n] [seed]  |  \
             graphix-fuzz selfcheck [iters] [seed]  |  graphix-fuzz regress"
        ),
    }
    Ok(())
}
