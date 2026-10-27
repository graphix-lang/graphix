//! A seqq nested in an outer seq's step runs every request in order on
//! each outer run: the step's re-entry is the queue's birth, which
//! credits it once.

mod common;
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
