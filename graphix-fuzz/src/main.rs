//! `graphix-fuzz` — differential model-checking fuzzer CLI.
//!
//! V1 subcommands:
//!   graphix-fuzz check <file>   run interp vs jit, report any divergence
//!   graphix-fuzz run   <file>   run all three modes, print each outcome
//!
//! `check` is the primitive both the mechanical fuzzer (forthcoming) and
//! the adversarial agent sources depend on. See design/graphix_fuzz.md.

use anyhow::{bail, Result};
use graphix_fuzz::{check, run_program, Mode, Outcome};
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
    let (cmd, path) = match args.get(1).map(String::as_str) {
        Some("check") | Some("run") => match args.get(2) {
            Some(p) => (args[1].as_str(), p.clone()),
            None => bail!("usage: graphix-fuzz {} <file>", &args[1]),
        },
        _ => bail!("usage: graphix-fuzz <check|run> <file>"),
    };
    let code = std::fs::read_to_string(&path)?;
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
    Ok(())
}
