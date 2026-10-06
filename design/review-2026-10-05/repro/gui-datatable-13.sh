#!/usr/bin/env bash
# gui-datatable-13: decimate_sparkline drops peaks and valleys. It keeps,
# of each adjacent pair, the point farther from the pair's mean
# (stdlib/graphix-package-gui/src/widgets/data_table/types.rs:488-512), but
# both points of a pair are |a - b| / 2 from their mean, so `da >= db` ties
# and keeps the first point except where rounding breaks the tie. The rule
# is 2:1 subsampling; an extreme at the second position of its pair is
# dropped. The sparkline draws, and auto-scales its column's shared y-axis
# from, exactly this history (render.rs:565-580, 795-805).
#
# The script compiles decimate_sparkline exactly as it stands in types.rs
# (extracted by awk, not a hand copy) into a standalone harness with
# rustc, and runs it. The call-site simulation repeats
# subscriptions.rs:206-217 (push, drop points older than history_seconds,
# decimate when len > MAX_SPARKLINE_POINTS = 512) for a 10 Hz feed and the
# default 60 s window; the last part repeats the loop of the pin
# data_table_test.rs:1440-1465 (sparkline_decimation_preserves_extremes).
#
# command (from the repository root):
#   bash design/review-2026-10-05/repro/gui-datatable-13.sh
#
# expected (doc of decimate_sparkline: "keep the point farther from the
# pair's mean, preserving peaks and valleys"): a one-sample spike or valley
# survives decimation wherever it sits; in the 10 Hz simulation (one 100.0
# spike among 1.0 samples, 180 s feed, a run per spike position) only the
# 60 s cutoff removes a spike, never decimate_sparkline.
#
# observed (HEAD c722befe, rustc 1.98.1):
#   pair (1, 100): mean 50.5, |a - mean| = 49.5, |b - mean| = 49.5, kept: 1
#   spike 100 at index 1 of 513 ones: len 513 -> 257, max after = 1
#   spike 100 at index 301 of 513 ones: len 513 -> 257, max after = 1
#   spike 100 at index 300 of 513 ones: len 513 -> 257, max after = 100
#   valley 0 at index 123 of 513 fifties: min after = 50
#   valley 0 at index 122 of 513 fifties: min after = 0
#   random pairs (uniform in [0, 1)): second point kept in 12.5% of 1000000
#   10 Hz, 60 s window, one spike per run at each sample 0..1200: 692 of
#     1200 spikes are removed by decimate_sparkline, at ages 0.1 s to
#     59.8 s (median 28.5 s) of the 60 s window
#     the spike at sample 301 (t = 30.1 s) is gone at t = 51.2 s, age 21.1 s
#   pin loop (0..1024 ramp): decimate min 0 max 1023 len 512; keep-first
#     min 0 max 1023 len 512; identical values: true
#   (the pin passes for plain keep-first subsampling: it cannot catch this)
set -euo pipefail

rel=stdlib/graphix-package-gui/src/widgets/data_table/types.rs
if [ -f "$PWD/$rel" ]; then
    src="$PWD/$rel"
else
    src="$(cd "$(dirname "$0")/../../.." && pwd)/$rel"
fi
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT

fn_text=$(awk '/^pub\(crate\) fn decimate_sparkline/{p=1} p{print} p&&/^}$/{exit}' "$src")
case "$fn_text" in
    *"fn decimate_sparkline"*"}") ;;
    *) echo "could not extract decimate_sparkline from $src" >&2; exit 2 ;;
esac

{
cat <<'EOF'
#![allow(dead_code)]
use std::{
    collections::VecDeque,
    time::{Duration, Instant},
};

const MAX_SPARKLINE_POINTS: usize = 512;
const SPIKE: f64 = 100.0;

EOF
printf '%s\n' "$fn_text"
cat <<'EOF'

type History = VecDeque<(Instant, f64)>;

fn series(base: Instant, n: usize, f: impl Fn(usize) -> f64) -> History {
    (0..n).map(|i| (base + Duration::from_millis(100 * i as u64), f(i))).collect()
}

fn max_of(h: &History) -> f64 {
    h.iter().map(|p| p.1).fold(f64::NEG_INFINITY, f64::max)
}

fn min_of(h: &History) -> f64 {
    h.iter().map(|p| p.1).fold(f64::INFINITY, f64::min)
}

fn keep_first(h: &mut History) {
    let points: Vec<(Instant, f64)> = h.drain(..).collect();
    for c in points.chunks(2) {
        h.push_back(c[0]);
    }
}

fn main() {
    let base = Instant::now() + Duration::from_secs(3600);

    let (a, b) = (1.0f64, SPIKE);
    let mean = (a + b) / 2.0;
    let (da, db) = ((a - mean).abs(), (b - mean).abs());
    println!(
        "pair (1, 100): mean {mean}, |a - mean| = {da}, |b - mean| = {db}, kept: {}",
        if da >= db { a } else { b }
    );

    for k in [1usize, 301, 300] {
        let mut h = series(base, 513, |i| if i == k { SPIKE } else { 1.0 });
        decimate_sparkline(&mut h);
        println!(
            "spike 100 at index {k} of 513 ones: len 513 -> {}, max after = {}",
            h.len(),
            max_of(&h)
        );
    }
    for k in [123usize, 122] {
        let mut h = series(base, 513, |i| if i == k { 0.0 } else { 50.0 });
        decimate_sparkline(&mut h);
        println!("valley 0 at index {k} of 513 fifties: min after = {}", min_of(&h));
    }

    let mut s: u64 = 0x9E37_79B9_7F4A_7C15;
    let mut rnd = move || {
        s ^= s << 13;
        s ^= s >> 7;
        s ^= s << 17;
        (s >> 11) as f64 / (1u64 << 53) as f64
    };
    let pairs = 1_000_000usize;
    let vals: Vec<f64> = (0..2 * pairs).map(|_| rnd()).collect();
    let mut h = series(base, 2 * pairs, |i| vals[i]);
    decimate_sparkline(&mut h);
    let second = h
        .iter()
        .enumerate()
        .filter(|(j, p)| p.1 == vals[2 * j + 1] && p.1 != vals[2 * j])
        .count();
    println!(
        "random pairs (uniform in [0, 1)): second point kept in {:.1}% of {pairs}",
        100.0 * second as f64 / pairs as f64
    );

    let window = 60.0f64;
    let samples = 1800usize;
    let mut ages: Vec<f64> = Vec::new();
    let mut example = None;
    for k in 0..1200usize {
        let mut history: History = VecDeque::new();
        let t_k = base + Duration::from_millis(100 * k as u64);
        for i in 0..samples {
            let now = base + Duration::from_millis(100 * i as u64);
            let f = if i == k { SPIKE } else { 1.0 };
            history.push_back((now, f));
            let cutoff = now - Duration::from_secs_f64(window);
            while history.front().map(|(t, _)| *t < cutoff).unwrap_or(false) {
                history.pop_front();
            }
            if history.len() > MAX_SPARKLINE_POINTS {
                let before = max_of(&history) == SPIKE;
                decimate_sparkline(&mut history);
                if before && max_of(&history) < SPIKE {
                    let age = (now - t_k).as_secs_f64();
                    ages.push(age);
                    if k == 301 {
                        example = Some(((now - base).as_secs_f64(), age));
                    }
                    break;
                }
            }
        }
    }
    ages.sort_by(f64::total_cmp);
    println!(
        "10 Hz, 60 s window, one spike per run at each sample 0..1200: {} of 1200 spikes are removed by decimate_sparkline, at ages {:.1} s to {:.1} s (median {:.1} s) of the 60 s window",
        ages.len(),
        ages.first().copied().unwrap_or(f64::NAN),
        ages.last().copied().unwrap_or(f64::NAN),
        ages.get(ages.len() / 2).copied().unwrap_or(f64::NAN)
    );
    if let Some((t, age)) = example {
        println!("  the spike at sample 301 (t = 30.1 s) is gone at t = {t:.1} s, age {age:.1} s");
    }

    let pin = |decimate: fn(&mut History)| {
        let mut h: History = VecDeque::new();
        for i in 0..1024u64 {
            h.push_back((base + Duration::from_micros(i), i as f64));
            if h.len() > MAX_SPARKLINE_POINTS {
                decimate(&mut h);
            }
        }
        h
    };
    let d = pin(decimate_sparkline);
    let n = pin(keep_first);
    let same = d.iter().map(|p| p.1).eq(n.iter().map(|p| p.1));
    println!(
        "pin loop (0..1024 ramp): decimate min {} max {} len {}; keep-first min {} max {} len {}; identical values: {same}",
        min_of(&d),
        max_of(&d),
        d.len(),
        min_of(&n),
        max_of(&n),
        n.len()
    );
}
EOF
} > "$work/main.rs"

rustc --edition 2024 -O -o "$work/probe" "$work/main.rs"
"$work/probe"
