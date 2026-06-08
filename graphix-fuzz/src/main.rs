//! `graphix-fuzz` — differential model-checking fuzzer CLI.
//!
//! V1 subcommands:
//!   graphix-fuzz check <file>   run interp vs jit, report any divergence
//!   graphix-fuzz run   <file>   run all three modes, print each outcome
//!
//! `check` is the primitive both the mechanical fuzzer (forthcoming) and
//! the adversarial agent sources depend on. See design/graphix_fuzz.md.

use anyhow::{bail, Result};
use graphix_fuzz::{check, fuzz, minimize, run_program, Mode, Outcome};
use std::time::Duration;

const TIMEOUT: Duration = Duration::from_secs(10);

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

#[tokio::main]
async fn main() -> Result<()> {
    let args: Vec<String> = std::env::args().collect();
    match args.get(1).map(String::as_str) {
        Some("fuzz") => {
            let iters: usize = args.get(2).and_then(|s| s.parse().ok()).unwrap_or(50);
            let seed: u64 = args.get(3).and_then(|s| s.parse().ok()).unwrap_or(1);
            let out = std::path::PathBuf::from("fuzz/crashes");
            println!("fuzzing: iters={iters} seed={seed} → {}/", out.display());
            let stats = fuzz(iters, seed, TIMEOUT, &out).await;
            println!(
                "done: {} mutants run, {} divergences",
                stats.run, stats.divergences
            );
            if stats.divergences > 0 {
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
                    for mode in [Mode::Interp, Mode::Fused, Mode::Jit] {
                        let o = run_program(code, mode, TIMEOUT).await;
                        println!("{mode:?}: {}", render(&o));
                    }
                }
                "check" => match check(code, TIMEOUT).await {
                    None => println!("AGREE — interp and jit produce the same result"),
                    Some(d) => {
                        println!("DIVERGENCE — {}", d.bisect());
                        println!("  interp: {}", render(&d.interp));
                        println!("  fused:  {}", render(&d.fused));
                        println!("  jit:    {}", render(&d.jit));
                        std::process::exit(1);
                    }
                },
                _ => unreachable!(),
            }
        }
        _ => bail!(
            "usage: graphix-fuzz <check|run|minimize> <file>  |  \
             graphix-fuzz fuzz [iters] [seed]"
        ),
    }
    Ok(())
}
