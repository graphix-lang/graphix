// Title: clip_area flattens the area's edge where a segment crosses the view's
// top or bottom: the fill leaves the visible line.
// Command: rustc --edition 2024 -O -o /tmp/area design/review-2026-10-19/repro/gui-chart-area-clamp.rs && /tmp/area
// The functions below are copied verbatim from
// stdlib/graphix-package-gui/src/widgets/chart/clip.rs (LPooled -> Vec).
// Expected: the area outline follows the line: at x = 9 the line (clip_polyline)
//   is at y = 10 (the view's top), so the fill's top edge is at 10 there too.
// Observed: the outline is (0,10) (5,10) (10,7.5); at x = 9 its edge is at 8.0,
//   two units under the line drawn over it.
// Second case: a NaN sample breaks the line into two runs, but the area's
//   outline joins (1,1) to (3,1) and fills across the gap.
#![allow(dead_code)]
//! Clipping series to the visible part of a 2D plot, in data space, so
//! nothing far outside the view is mapped to pixels: plotters clamps an
//! outside point onto the plot's edge, and one far enough out overflows
//! its i32 pixel arithmetic.


/// The visible x and y intervals of a plot.
#[derive(Clone, Copy, Debug)]
pub struct View {
    pub x: (f64, f64),
    pub y: (f64, f64),
}

impl View {
    pub fn contains(&self, (x, y): (f64, f64)) -> bool {
        self.contains_x(x) && self.y.0 <= y && y <= self.y.1
    }

    pub fn contains_x(&self, x: f64) -> bool {
        self.x.0 <= x && x <= self.x.1
    }

    /// `y` moved into the view; NaN stays NaN.
    pub fn clamp_y(&self, y: f64) -> f64 {
        y.clamp(self.y.0, self.y.1)
    }
}

/// The parameter interval `[t0, t1]` of the segment `a..b` inside `view`
/// (Liang–Barsky); `None` when none of it is, or a coordinate is not
/// finite.
fn clip_segment(a: (f64, f64), b: (f64, f64), view: View) -> Option<(f64, f64)> {
    if ![a.0, a.1, b.0, b.1].iter().all(|c| c.is_finite()) {
        return None;
    }
    let (dx, dy) = (b.0 - a.0, b.1 - a.1);
    let (mut t0, mut t1) = (0.0f64, 1.0f64);
    for (p, q) in [
        (-dx, a.0 - view.x.0),
        (dx, view.x.1 - a.0),
        (-dy, a.1 - view.y.0),
        (dy, view.y.1 - a.1),
    ] {
        if p == 0.0 {
            if q < 0.0 {
                return None;
            }
        } else {
            let r = q / p;
            if p < 0.0 {
                t0 = t0.max(r);
            } else {
                t1 = t1.min(r);
            }
        }
    }
    (t0 <= t1).then_some((t0, t1))
}

fn lerp(a: (f64, f64), b: (f64, f64), t: f64) -> (f64, f64) {
    (a.0 + (b.0 - a.0) * t, a.1 + (b.1 - a.1) * t)
}

/// The visible pieces of the polyline `pts`, each a polyline inside
/// `view`, cut where the line crosses an edge.
pub fn clip_polyline(
    pts: &[(f64, f64)],
    view: View,
) -> Vec<Vec<(f64, f64)>> {
    let mut runs: Vec<Vec<(f64, f64)>> = Vec::new();
    let mut open = false;
    for w in pts.windows(2) {
        match clip_segment(w[0], w[1], view) {
            None => open = false,
            Some((t0, t1)) => {
                if !open || t0 > 0.0 {
                    let mut run: Vec<(f64, f64)> = Vec::new();
                    run.push(lerp(w[0], w[1], t0));
                    runs.push(run);
                }
                if let Some(run) = runs.last_mut() {
                    run.push(lerp(w[0], w[1], t1));
                }
                open = t1 >= 1.0;
            }
        }
    }
    runs
}

/// The outline of the area under `pts` inside `view`: the line cut to
/// the view's x interval, its y moved into the view. Filled down to the
/// baseline clamped the same way, it is exactly the area's visible part.
pub fn clip_area(pts: &[(f64, f64)], view: View) -> Vec<(f64, f64)> {
    let in_x = View { x: view.x, y: (f64::NEG_INFINITY, f64::INFINITY) };
    let mut out: Vec<(f64, f64)> = Vec::new();
    for w in pts.windows(2) {
        if let Some((t0, t1)) = clip_segment(w[0], w[1], in_x) {
            let (a, b) = (lerp(w[0], w[1], t0), lerp(w[0], w[1], t1));
            if out.last() != Some(&a) {
                out.push(a);
            }
            out.push(b);
        }
    }
    for p in out.iter_mut() {
        p.1 = view.clamp_y(p.1);
    }
    out
}


fn edge_at(poly: &[(f64, f64)], x: f64) -> f64 {
    for w in poly.windows(2) {
        if w[0].0 <= x && x <= w[1].0 {
            let t = (x - w[0].0) / (w[1].0 - w[0].0);
            return w[0].1 + t * (w[1].1 - w[0].1);
        }
    }
    f64::NAN
}

fn main() {
    let v = View { x: (0.0, 10.0), y: (0.0, 10.0) };
    let pts = [(-5.0, 20.0), (5.0, 20.0), (15.0, -5.0)];
    let area = clip_area(&pts, v);
    let line: Vec<(f64, f64)> = clip_polyline(&pts, v).iter().flat_map(|r| r.iter().copied()).collect();
    println!("area outline: {:?}", area);
    println!("line runs:    {:?}", line);
    println!("at x=9: line y = {}, area edge y = {}", edge_at(&line, 9.0), edge_at(&area, 9.0));
    let big = View { x: (-10.0, 10.0), y: (-10.0, 10.0) };
    let gap = [(0.0, 1.0), (1.0, 1.0), (2.0, f64::NAN), (3.0, 1.0), (4.0, 1.0)];
    println!("gap: line runs {:?}", clip_polyline(&gap, big).iter().map(|r| r.to_vec()).collect::<Vec<_>>());
    println!("gap: area outline {:?}", clip_area(&gap, big));
}
