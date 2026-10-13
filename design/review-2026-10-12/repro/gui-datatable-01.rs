// gui-datatable-01: NaN/inf sparkline sample panics the GUI (lyon debug_assert)
// when the cell is drawn (test 1 below; test 2 is gui-datatable-02).
// Run: copy to stdlib/graphix-package-gui/tests/review_gui_datatable_01.rs, then
//   timeout -s KILL 2400 cargo test -p graphix-package-gui --test review_gui_datatable_01 -- --nocapture
// Expected: sparkline_with_a_nan_sample_builds_a_path passes (a NaN point skipped).
// Observed: panicked at lyon_path-1.0.19/src/path.rs:812:5:
//   assertion failed: p.y.is_finite()
//
// Two data table defects shown through the very code paths the widget
// uses, without a display.
//
// 1. render.rs:91-99 maps every sparkline point to `y = h - ((v - min) /
//    (max - min)) * h` and feeds it to iced's canvas Path builder. A NaN
//    sample (value_to_f64 passes NaN and inf through, types.rs:503) makes
//    `y` NaN, and lyon_path's builder debug_asserts finite points: a dev
//    build's GUI thread panics in the canvas draw.
// 2. subscriptions.rs:249 sorts by the FORMATTED text: a duration formats
//    as "2.s", "500.ms", "1.m" (netidx fmt_naked), which does not parse as
//    an f64, so durations sort by their bytes.
use iced_core::Point;
use iced_widget::canvas::Path;
use netidx::publisher::Value;
use std::{
    panic::{AssertUnwindSafe, catch_unwind},
    time::Duration,
};

fn sparkline_path(points: &[(f64, f64)], min_v: f64, max_v: f64) {
    let (w, h) = (100.0f32, 20.0f32);
    let (min_t, max_t) = (points[0].0, points[points.len() - 1].0);
    let t_range = (max_t - min_t).max(1e-12);
    let v_range = max_v - min_v;
    let _ = Path::new(|b| {
        let mut first = true;
        for &(t, v) in points {
            let x = ((t - min_t) / t_range) as f32 * w;
            let y = h - ((v - min_v) / v_range) as f32 * h;
            if first {
                b.move_to(Point::new(x, y));
                first = false;
            } else {
                b.line_to(Point::new(x, y));
            }
        }
    });
}

#[test]
fn sparkline_with_a_nan_sample_builds_a_path() {
    // bounds as compute_sparkline_bounds makes them: NaN never wins a
    // `<`/`>` so the column range is the finite samples'
    let pts = [(0.0, 1.0), (1.0, f64::NAN), (2.0, 3.0)];
    let ok = catch_unwind(AssertUnwindSafe(|| sparkline_path(&pts, 1.0, 3.0))).is_ok();
    assert!(ok, "a NaN sample panics the sparkline path build");
}

fn naked(v: &Value) -> String {
    v.to_string_naked()
}

#[test]
fn durations_sort_by_value() {
    let vals = [
        Value::from(Duration::from_secs(60)),
        Value::from(Duration::from_secs(2)),
        Value::from(Duration::from_millis(500)),
    ];
    let mut texts: Vec<String> = vals.iter().map(naked).collect();
    eprintln!("formatted: {texts:?}");
    assert!(texts.iter().all(|t| t.parse::<f64>().is_err()));
    // the widget's order for text keys: by bytes
    texts.sort();
    assert_eq!(texts, vec!["500.ms", "2.s", "1.m"], "ascending by duration");
}
