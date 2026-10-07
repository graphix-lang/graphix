//! Clipping series to the visible part of a 2D plot, in data space, so
//! nothing far outside the view is mapped to pixels: plotters clamps an
//! outside point onto the plot's edge, and one far enough out overflows
//! its i32 pixel arithmetic.

use poolshark::local::LPooled;

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
) -> LPooled<Vec<LPooled<Vec<(f64, f64)>>>> {
    let mut runs: LPooled<Vec<LPooled<Vec<(f64, f64)>>>> = LPooled::take();
    let mut open = false;
    for w in pts.windows(2) {
        match clip_segment(w[0], w[1], view) {
            None => open = false,
            Some((t0, t1)) => {
                if !open || t0 > 0.0 {
                    let mut run: LPooled<Vec<(f64, f64)>> = LPooled::take();
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
pub fn clip_area(pts: &[(f64, f64)], view: View) -> LPooled<Vec<(f64, f64)>> {
    let in_x = View { x: view.x, y: (f64::NEG_INFINITY, f64::INFINITY) };
    let mut out: LPooled<Vec<(f64, f64)>> = LPooled::take();
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

#[cfg(test)]
mod test {
    use super::*;

    const V: View = View { x: (0.0, 10.0), y: (0.0, 10.0) };

    #[test]
    fn a_line_through_the_view_is_cut_at_its_edges() {
        let runs = clip_polyline(&[(-10.0, 5.0), (20.0, 5.0)], V);
        assert_eq!(runs.len(), 1);
        assert_eq!(&runs[0][..], &[(0.0, 5.0), (10.0, 5.0)]);
    }

    #[test]
    fn a_line_leaving_and_coming_back_is_two_runs() {
        let pts = [(1.0, 1.0), (5.0, 1e12), (9.0, 1.0)];
        let runs = clip_polyline(&pts, V);
        assert_eq!(runs.len(), 2);
        assert!(runs.iter().flat_map(|r| r.iter()).all(|p| V.contains(*p)));
    }

    #[test]
    fn non_finite_points_break_the_line() {
        let pts = [(1.0, 1.0), (2.0, f64::NAN), (3.0, 1.0), (4.0, 2.0)];
        let runs = clip_polyline(&pts, V);
        assert_eq!(runs.len(), 1);
        assert_eq!(&runs[0][..], &[(3.0, 1.0), (4.0, 2.0)]);
    }

    #[test]
    fn an_area_is_cut_to_x_and_clamped_in_y() {
        let out = clip_area(&[(-5.0, 20.0), (5.0, 20.0), (15.0, -5.0)], V);
        assert_eq!(out.first(), Some(&(0.0, 10.0)));
        assert_eq!(out.last(), Some(&(10.0, 7.5)));
        assert!(out.iter().all(|p| V.contains(*p)));
    }
}
