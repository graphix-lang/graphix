// gui-chart: an axis with one label over a range spanning >= 1e308 hangs plotters
//
// The chart hands plotters any range `checked_range` accepts (finite ends,
// min < max, finite span): a program's x/y/z range, and every zoomed or
// panned view. `pad_range_by` keeps auto ranges inside +-f64::MAX/4 (span
// <= MAX/2 < 1e308), but nothing bounds the others. With `mesh_style(#y_labels:
// 1)` (or 0, or a negative count: `ticks(n, 1)` makes them 1) plotters'
// `compute_f64_key_points(range, 1)` multiplies its 1e308 scale by 10, gets
// inf, and its 'outer loop never breaks (every npoints is NaN, NaN as usize
// is 0). draw runs on the GUI thread: the window freezes for good.
//
// This file holds VERBATIM copies of plotters-0.3.7's compute_f64_key_points
// (src/coord/ranged1d/types/numeric.rs, macro gen_key_points_comp! float arm),
// of ranges.rs's pad_range_by and checked_range, and of handle_zoom's `zoom`
// closure, and runs the two paths that reach plotters: a program range and
// two wheel notches out from an auto range.
//
// Run: rustc -O -o hang gui-chart-one-label-hang.rs && timeout -s KILL 30 ./hang
// Expected: every case "returned"
// Observed:
//   user y_range (-6e307, 6e307): checked_range=Some((-6e307, 6e307))
//   user y_range (-6e307, 6e307), y_labels 1: HANG (no return after 2 s)
//   auto (-4.4e307, 4.4e307) -> two zoom-outs (-5.324e307, 5.324e307) checked=true
//   auto range of data +-4e307 after two zoom-outs, y_labels 1: HANG (no return after 2 s)
//   auto range itself, y_labels 1: returned 1 points
//   user y_range (-6e307, 6e307), y_labels 2: returned 1 points
//   two zoom-outs, y_labels 2: returned 1 points
use std::{sync::mpsc, thread, time::Duration};

fn compute_f64_key_points(range: (f64, f64), max_points: usize) -> Vec<f64> {
    if max_points == 0 {
        return vec![];
    }

    let range = (range.0.min(range.1) as f64, range.1.max(range.0) as f64);

    assert!(!(range.0.is_nan() || range.1.is_nan()));

    if (range.0 - range.1).abs() < f64::EPSILON {
        return vec![range.0 as f64];
    }

    let mut scale = (10f64).powf((range.1 - range.0).log(10.0).floor());
    let mut value_granularity = scale / 10.0;
    fn rem_euclid(a: f64, b: f64) -> f64 {
        let ret = if b > 0.0 { a - (a / b).floor() * b } else { a - (a / b).ceil() * b };
        if (ret - b).abs() < f64::EPSILON { 0.0 } else { ret }
    }

    if 1 + ((range.1 - range.0) / scale).floor() as usize > max_points {
        scale *= 10.0;
        value_granularity *= 10.0;
    }

    'outer: loop {
        let old_scale = scale;
        for nxt in [2.0, 5.0, 10.0].iter() {
            let mut new_left = range.0 - rem_euclid(range.0, old_scale / nxt);
            if new_left < range.0 {
                new_left += old_scale / nxt;
            }
            let new_right = range.1 - rem_euclid(range.1, old_scale / nxt);

            let npoints = 1.0 + ((new_right - new_left) / old_scale * nxt);

            if npoints.round() as usize > max_points {
                break 'outer;
            }

            scale = old_scale / nxt;
        }
        scale = old_scale / 10.0;
        value_granularity /= 10.0;
    }

    let mut ret = vec![];
    let left = {
        let mut value = range.0 - rem_euclid(range.0, scale);
        if value < range.0 {
            value += scale;
        }
        value
    };
    let left_base = (left / value_granularity).floor() * value_granularity;
    let mut left_relative = left - left_base;
    let right = range.1 - rem_euclid(range.1, scale);
    while (right - left_relative - left_base) >= -f64::EPSILON {
        let new_left_relative = (left_relative / value_granularity).round() * value_granularity;
        if new_left_relative < 0.0 {
            left_relative += value_granularity;
        }
        ret.push((left_relative + left_base) as f64);
        left_relative += scale;
    }
    ret
}

// stdlib/graphix-package-gui/src/widgets/chart/ranges.rs
fn pad_range_by(min: f64, max: f64, unit: f64) -> (f64, f64) {
    const LIMIT: f64 = f64::MAX / 4.0;
    if !(min <= max) || !min.is_finite() || !max.is_finite() {
        return (-unit, unit);
    }
    let (min, max) = (min.clamp(-LIMIT, LIMIT), max.clamp(-LIMIT, LIMIT));
    let (lo, hi) = if min == max { (min - unit, max + unit) } else { (min, max) };
    let pad = (hi / 2.0 - lo / 2.0) / 10.0;
    let (lo, hi) = ((lo - pad).max(-LIMIT), (hi + pad).min(LIMIT));
    if lo < hi {
        (lo, hi)
    } else {
        (
            (lo - unit.max(lo.abs() * 1e-9)).max(-LIMIT),
            (hi + unit.max(hi.abs() * 1e-9)).min(LIMIT),
        )
    }
}

fn checked_range(r: (f64, f64)) -> Option<(f64, f64)> {
    (r.0.is_finite() && r.1.is_finite() && r.0 < r.1 && (r.1 - r.0).is_finite()).then_some(r)
}

// interact.rs handle_zoom's closure, at the plot's centre, zooming out one line
const ZOOM_ULPS: f64 = 4096.0 * f64::EPSILON;
fn zoom_out(r: (f64, f64)) -> Option<(f64, f64)> {
    let (t, least, factor) = (0.5, 0.0f64, 1.1f64.powf(1.0));
    let at = r.0 + t * (r.1 - r.0);
    let span = (r.1 - r.0) * factor;
    checked_range((at - t * span, at + (1.0 - t) * span))
        .filter(|z| z.1 - z.0 >= least.max(ZOOM_ULPS * z.0.abs().max(z.1.abs())))
}

fn run(what: &str, range: (f64, f64), labels: usize) {
    let (tx, rx) = mpsc::channel();
    thread::spawn(move || {
        let _ = tx.send(compute_f64_key_points(range, labels).len());
    });
    match rx.recv_timeout(Duration::from_secs(2)) {
        Ok(n) => println!("{what}, y_labels {labels}: returned {n} points"),
        Err(_) => println!("{what}, y_labels {labels}: HANG (no return after 2 s)"),
    }
}

fn main() {
    let user = (-6e307, 6e307);
    println!("user y_range {user:?}: checked_range={:?}", checked_range(user));
    run("user y_range (-6e307, 6e307)", user, 1);
    let auto = pad_range_by(-4e307, 4e307, 1.0);
    let zoomed = zoom_out(zoom_out(auto).unwrap()).unwrap();
    println!("auto {auto:?} -> two zoom-outs {zoomed:?} checked={}", checked_range(zoomed).is_some());
    run("auto range of data +-4e307 after two zoom-outs", zoomed, 1);
    run("auto range itself", auto, 1);
    run("user y_range (-6e307, 6e307)", user, 2);
    run("two zoom-outs", zoomed, 2);
    std::process::exit(0);
}
