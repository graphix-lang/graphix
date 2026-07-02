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
        Outcome::Value(v) => format!("Value({v})"),
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
    let args: Vec<String> = std::env::args().collect();
    match args.get(1).map(String::as_str) {
        Some("gen") => {
            // Debug: print N generated programs (no oracle) to eyeball the
            // generator's output.
            let n: usize = args.get(2).and_then(|s| s.parse().ok()).unwrap_or(10);
            let seed: u64 = args.get(3).and_then(|s| s.parse().ok()).unwrap_or(1);
            let mut rng = graphix_fuzz::mutate::Rng::new(seed);
            for _ in 0..n {
                println!("{}", graphix_fuzz::generate::gen_program(&mut rng));
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
                (0..n).map(|_| graphix_fuzz::generate::gen_program(&mut rng)).collect()
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
            while let Some(res) = set.join_next().await {
                match res {
                    Ok((_, None)) => compiled += 1,
                    Ok((i, Some(err))) => {
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
                std::process::exit(1);
            }
        }
        // Hidden: the isolated-check worker the campaign pool spawns
        // (program on stdin, one VERDICT line on stdout). A program that
        // kills the evaluator kills only this process — the parent
        // records a crash finding. See lib.rs `check_isolated`.
        Some("check-one") => {
            let code = read_stdin()?;
            let verdict = match check(code.trim(), CAMPAIGN_TIMEOUT).await {
                None => "AGREE",
                Some(_) => "DIVERGE",
            };
            println!("VERDICT\t{verdict}");
        }
        // Hidden: the isolated minimizer (program on stdin, the reduced
        // program after a MINIMIZED marker on stdout). A reduction that
        // crashes kills only this process — the parent falls back to
        // recording the unminimized mutant.
        Some("minimize-one") => {
            let code = read_stdin()?;
            let (min, _) = minimize(code.trim(), CAMPAIGN_TIMEOUT, 80).await;
            println!("MINIMIZED");
            println!("{min}");
        }
        Some(cmd @ ("generate" | "fuzz")) => {
            // `iters` may be `forever`/`0` to run until killed, surfacing new
            // divergences live. The corpus is loaded up front so a campaign
            // never re-reports a finding it (or a prior run) already saved.
            let iters = parse_iters(args.get(2), if cmd == "fuzz" { 50 } else { 100 });
            let seed: u64 = args.get(3).and_then(|s| s.parse().ok()).unwrap_or(1);
            let out = std::path::PathBuf::from("fuzz/crashes");
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
                generate_campaign(iters, seed, CAMPAIGN_TIMEOUT, &corpus).await
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
                        std::process::exit(1);
                    }
                },
                _ => unreachable!(),
            }
        }
        _ => bail!(
            "usage: graphix-fuzz <check|run|minimize> <file>  |  \
             graphix-fuzz <fuzz|generate> [iters] [seed]  |  \
             graphix-fuzz gen-check [n] [seed]  |  graphix-fuzz regress"
        ),
    }
    Ok(())
}
