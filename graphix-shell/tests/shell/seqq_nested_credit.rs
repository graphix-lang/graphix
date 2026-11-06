//! A seqq nested in an outer seq's step runs every request in order on
//! each outer run: the step's re-entry is the queue's birth, which
//! credits it once.

use super::common;
use common::graphix;

const PROGRAM: &str = r#"
let n = 0;
let oclock = sys::time::timer(duration:400.ms, 1);
n <- oclock ~ n + 1;
let f = 0;
let fclock = sys::time::timer(duration:20.ms, 30);
f <- fclock ~ f + 1;
let res = seq let k = n {
    {
        let v = seqq let j = f {
            let w = sys::time::timer(duration:50.ms, false);
            println("inner [k] [j]");
            j
        };
        let w = sys::time::timer(duration:120.ms, false);
        v
    };
    k
};
sys::exit(sys::time::after_idle(duration:700.ms, 0));
res
"#;

// CR claude for claude: [bug/medium] A nested seqq drops a request about 1% of
// runs under load: the second outer run prints `inner 1 16` then `inner 1 18` (or
// 17 then 19), where a seqq keeps every request. It failed the gate once beside a
// soak (load 350). PROGRAM at 16 side by side at that load: main 3/300, base-r18
// 1/300, base-r17 1/300, GRAPHIX_PAR=off 1/300, always around the second run's
// requests 16-19, none at 8 side by side (0/60); so not parallel evaluation and not
// recent. Not traced: the drop is mid-run, after the step's birth. Find the cause;
// a longer wait here would hide it. probe:
// ~/ptmp/cr29/scratch/skillcheck/seqqn/{p.gx,run.py} (run.py BIN 300 16)
// (seqq-nested-drop-01)
#[test]
fn nested_seqq_runs_consecutive_requests_on_each_outer_run() {
    let dir = tempfile::tempdir().expect("tempdir");
    let stdout = graphix(dir.path(), PROGRAM, &["--no-cache"], &[]).stdout;
    let inner: Vec<(i64, i64)> = stdout
        .lines()
        .filter_map(|l| l.strip_prefix("inner "))
        .filter_map(|l| l.split_once(' '))
        .map(|(k, j)| (k.parse().unwrap(), j.parse().unwrap()))
        .collect();
    assert!(inner.iter().any(|(k, _)| *k == 1), "no second outer run: {stdout}");
    for w in inner.windows(2) {
        if w[0].0 == w[1].0 {
            assert_eq!(w[0].1 + 1, w[1].1, "a request was dropped: {stdout}");
        }
    }
}
