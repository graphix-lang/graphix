//! fuzz-main-aux-07: `Corpus::record` (lib.rs:3622) and the campaigns'
//! `println!` (lib.rs:4797, 5136) format a divergence's outcomes with
//! the derived, recursive `Debug` of `Value`, in the soak parent's derive
//! task, on a tokio worker (2 MiB stack).
//!
//! `record_deep_divergence` re-runs this binary as a child per case. The
//! child is the soak's derive task minus the minimizer child: an
//! in-process `check`, then `Corpus::record`, in a task on a runtime
//! built like `main`'s (`multi_thread`, 2 workers, default stack).
//! `"TwinDiverged"` makes `check` return a real `Divergence`
//! (`Pair::Twin`) without an engine bug.
//!   record:   ("TwinDiverged", <list of N>): `value_has_tag` stops at
//!             the tag, only `record`'s Debug walks the list.
//!   nonfinal: the list in the epoch's first event, the tag in its final
//!             one: `value_has_tag` reads finals only, Debug every event.
//!   check:    the untagged list: `check` itself recurses over it in
//!             `value_has_tag` (lib.rs:734) on the same worker.
//!
//! Command (copy to graphix-fuzz/tests/review_fuzz_main_aux_07.rs first):
//!   timeout -s KILL 2400 cargo test -p graphix-fuzz \
//!     --test review_fuzz_main_aux_07 -- --nocapture
//! Expected: every child checks and records. `graphix-fuzz check` on the
//! same programs at N = 20000 reports the divergence fine (its `render`
//! uses the iterative Display).
//! Observed (dev profile, HEAD c722befe): for record and nonfinal the
//! in-process check returns the twin divergence at every N; at N = 1000
//! the finding is written (Debug 172317 bytes, clipped to 2 KB after;
//! Display 10910); at N = 2000 and 4000 the child dies inside
//! `Corpus::record` -> `<Outcome as Debug>::fmt`: "thread
//! 'tokio-rt-worker' has overflowed its stack", SIGABRT, no finding file
//! (22648 Debug frames, ~18 per list level, ~1250 levels reached). check
//! passes at N = 500 and dies in value_has_tag (2240-byte frames) at
//! N = 1000.
use graphix_fuzz::{Corpus, Outcome, check};
use std::{path::Path, process::Command, time::Duration};

const DEPTH_VAR: &str = "REVIEW_AUX07_DEPTH";
const KIND_VAR: &str = "REVIEW_AUX07_KIND";
const DIR_VAR: &str = "REVIEW_AUX07_DIR";

fn prog(kind: &str, n: usize) -> String {
    match kind {
        "record" => {
            format!("(\"TwinDiverged\", list::from_array(array::init({n}, |i| i)))")
        }
        "nonfinal" => format!(
            "let l = list::from_array(array::init({n}, |i| i));\n\
             let x = (l, \"a\");\n\
             x <- l ~ ([<>], \"TwinDiverged\");\n\
             x"
        ),
        _ => format!("list::from_array(array::init({n}, |i| i))"),
    }
}

#[test]
#[ignore = "child of record_deep_divergence"]
fn aux07_child() {
    let n: usize = std::env::var(DEPTH_VAR).unwrap().parse().unwrap();
    let kind = std::env::var(KIND_VAR).unwrap();
    let dir = std::env::var(DIR_VAR).unwrap();
    let rt = tokio::runtime::Builder::new_multi_thread()
        .worker_threads(2)
        .enable_all()
        .build()
        .unwrap();
    rt.block_on(async move {
        tokio::spawn(async move {
            let p = prog(&kind, n);
            let d = check(&p, Duration::from_secs(10)).await;
            eprintln!(
                "CHECKED depth={n} divergence={} thread={:?}",
                d.as_ref().map(|d| d.bisect()).unwrap_or("none"),
                std::thread::current().name()
            );
            if let Some(d) = d {
                let corpus = Corpus::load(Path::new(&dir));
                let new = corpus.record(&d, &p, &p);
                eprintln!("RECORDED depth={n} new={new}");
                let dbg = format!("{:?}", d.interp);
                let disp = match &d.interp {
                    Outcome::Trace(t) => t
                        .epochs
                        .iter()
                        .flat_map(|e| e.events.iter())
                        .map(|(_, v)| format!("{v}").len())
                        .sum::<usize>(),
                    _ => 0,
                };
                eprintln!(
                    "SIZES depth={n} debug_len={} display_len={disp} head={:?}",
                    dbg.len(),
                    &dbg[..dbg.len().min(260)]
                );
            }
        })
        .await
        .unwrap();
    });
}

#[test]
fn record_deep_divergence() {
    let exe = std::env::current_exe().unwrap();
    let tmp = tempfile::tempdir().unwrap();
    let mut failures = Vec::new();
    for (kind, depths) in [
        ("record", &[1000usize, 2000, 4000][..]),
        ("nonfinal", &[1000usize, 2000, 4000][..]),
        ("check", &[500usize, 1000, 2000][..]),
    ] {
        for &n in depths {
            let dir = tmp.path().join(format!("{kind}_{n}"));
            let out = Command::new(&exe)
                .args(["--ignored", "--exact", "aux07_child", "--nocapture"])
                .env(DEPTH_VAR, n.to_string())
                .env(KIND_VAR, kind)
                .env(DIR_VAR, &dir)
                .env("XDG_CACHE_HOME", tmp.path().join("cache"))
                .env_remove("RUST_MIN_STACK")
                .output()
                .unwrap();
            let stderr = String::from_utf8_lossy(&out.stderr);
            let find = |p: &str| {
                stderr.lines().find(|l| l.contains(p)).map(|l| {
                    let l = l.trim();
                    l[..l.len().min(400)].to_string()
                })
            };
            let files = std::fs::read_dir(&dir).map(|r| r.count()).unwrap_or(0);
            println!(
                "{kind:8} depth={n:5}: {}\n    checked={:?}\n    recorded={:?}\n    \
                 sizes={:?}\n    overflow={:?}\n    corpus_files={files}",
                out.status,
                find("CHECKED"),
                find("RECORDED"),
                find("SIZES"),
                find("overflowed its stack"),
            );
            if !out.status.success() {
                failures.push(format!("{kind} depth={n}: {}", out.status));
            }
        }
    }
    assert!(failures.is_empty(), "children died: {failures:#?}");
}
