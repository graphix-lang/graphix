// Title: clip_segment returns NaN points and drops the visible part when b - a overflows
// Run: rustc -O -o clip_probe gui-chart-clip-overflow.rs && ./clip_probe
// (clip_segment, lerp and clip_polyline are verbatim copies of
//  stdlib/graphix-package-gui/src/widgets/chart/clip.rs with LPooled -> Vec)
// Expected: the 2nd and 3rd inputs give one run inside the view, like the control
//   (crossing x = 1 / spanning y = 0.5 across x 0..2).
// Observed: runs [[(0.0, NaN), (0.0, NaN)]] and [[(NaN, 0.5), (NaN, 0.5)]]: nothing of the
//   visible segment is drawn, and NaN points reach plotters' pixel map.
// verbatim copy of stdlib/graphix-package-gui/src/widgets/chart/clip.rs (LPooled -> Vec)
#[derive(Clone, Copy, Debug)]
pub struct View { pub x: (f64, f64), pub y: (f64, f64) }
fn clip_segment(a: (f64, f64), b: (f64, f64), view: View) -> Option<(f64, f64)> {
    if ![a.0, a.1, b.0, b.1].iter().all(|c| c.is_finite()) { return None; }
    let (dx, dy) = (b.0 - a.0, b.1 - a.1);
    let (mut t0, mut t1) = (0.0f64, 1.0f64);
    for (p, q) in [(-dx, a.0 - view.x.0),(dx, view.x.1 - a.0),(-dy, a.1 - view.y.0),(dy, view.y.1 - a.1)] {
        if p == 0.0 { if q < 0.0 { return None; } } else {
            let r = q / p;
            if p < 0.0 { t0 = t0.max(r); } else { t1 = t1.min(r); }
        }
    }
    (t0 <= t1).then_some((t0, t1))
}
fn lerp(a: (f64, f64), b: (f64, f64), t: f64) -> (f64, f64) { (a.0 + (b.0 - a.0) * t, a.1 + (b.1 - a.1) * t) }
pub fn clip_polyline(pts: &[(f64, f64)], view: View) -> Vec<Vec<(f64, f64)>> {
    let mut runs: Vec<Vec<(f64, f64)>> = vec![];
    let mut open = false;
    for w in pts.windows(2) {
        match clip_segment(w[0], w[1], view) {
            None => open = false,
            Some((t0, t1)) => {
                if !open || t0 > 0.0 { runs.push(vec![lerp(w[0], w[1], t0)]); }
                if let Some(run) = runs.last_mut() { run.push(lerp(w[0], w[1], t1)); }
                open = t1 >= 1.0;
            }
        }
    }
    runs
}
// plotters-0.3.7 RangedCoordf64::map (ranged1d/types/numeric.rs), wrapping add as in release
fn map(r: (f64, f64), v: f64, limit: (i32, i32)) -> Option<i32> {
    let logic_length = (v - r.0) / (r.1 - r.0);
    let actual_length = limit.1 - limit.0;
    if logic_length.is_infinite() { return Some(if logic_length.is_sign_positive() { limit.1 } else { limit.0 }); }
    if actual_length > 0 { limit.0.checked_add((actual_length as f64 * logic_length + 1e-3).floor() as i32) }
    else { limit.0.checked_add((actual_length as f64 * logic_length - 1e-3).ceil() as i32) }
}
fn main() {
    let view = View { x: (0.0, 2.0), y: (0.0, 1.0) };
    for pts in [
        [(0.0, -1e10), (2.0, 1e10)],       // control: crosses the view at x = 1
        [(0.0, -1e308), (2.0, 1e308)],     // same line, dy overflows to inf
        [(-1e308, 0.5), (1e308, 0.5)],     // horizontal through the view, dx overflows
    ] {
        let runs = clip_polyline(&pts, view);
        println!("{pts:?} -> runs {runs:?}");
        for p in runs.iter().flatten() {
            println!("   in view: {}  pixel y (plot 10..260): {:?}", view.x.0 <= p.0 && p.0 <= view.x.1 && view.y.0 <= p.1 && p.1 <= view.y.1, map(view.y, p.1, (260, 10)));
        }
    }
}
