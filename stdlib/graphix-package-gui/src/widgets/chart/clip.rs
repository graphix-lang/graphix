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

/// A view edge: the line `x = v` or `y = v`.
#[derive(Clone, Copy)]
enum Edge {
    X(f64),
    Y(f64),
}

/// The ends of the part of the segment `a..b` inside `view`
/// (Liang–Barsky), each its parameter along the segment and its point,
/// which lies exactly on the edge that cut it; `None` when none of it
/// is, or a coordinate is not finite.
// XCR claude for claude: [bug/low] b - a overflows to infinity for finite points of
// opposite sign beyond about 9e307. Then q / p is 0, the interval collapses to t = 0,
// and lerp returns NaN (a + inf * 0). The visible part of the segment is dropped and
// NaN points reach plotters, which breaks the module's promise that runs lie inside the
// view. Fix: when dx or dy is not finite, clip a/2 and b/2 against the halved view
// (Liang–Barsky does not change under uniform scaling), and compute lerp as a*(1-t) +
// b*t. probe: design/review-2026-10-26/repro/gui-chart-clip-overflow.rs (verbatim copy
// of these functions, compiled standalone) (gui-chart-04)
// 2026-10-26 claude: clip_segment now takes every difference as half_diff (b/2 - a/2,
// finite for finite inputs, bit-identical ratios above the subnormals), lerp goes
// through it, and each clipped end is snapped onto the edge that cut it, so a visible
// piece of a segment between far points is kept in the view even where its parameter
// rounds (the halving alone would leave the horizontal case at x = 0..0); clip_area's y
// cuts use half_diff too. Pins:
// clip::test::a_line_between_far_points_is_cut_at_its_edges,
// an_area_between_far_points_is_cut_where_it_crosses (both fail on the old code).
fn clip_segment(
    a: (f64, f64),
    b: (f64, f64),
    view: View,
) -> Option<[(f64, (f64, f64)); 2]> {
    if ![a.0, a.1, b.0, b.1].iter().all(|c| c.is_finite()) {
        return None;
    }
    let (dx, dy) = (half_diff(a.0, b.0), half_diff(a.1, b.1));
    let mut ends = [(0.0f64, None), (1.0f64, None)];
    for (p, q, edge) in [
        (-dx, half_diff(view.x.0, a.0), Edge::X(view.x.0)),
        (dx, half_diff(a.0, view.x.1), Edge::X(view.x.1)),
        (-dy, half_diff(view.y.0, a.1), Edge::Y(view.y.0)),
        (dy, half_diff(a.1, view.y.1), Edge::Y(view.y.1)),
    ] {
        if p == 0.0 {
            if q < 0.0 {
                return None;
            }
        } else {
            let r = q / p;
            if p < 0.0 && r > ends[0].0 {
                ends[0] = (r, Some(edge));
            } else if p > 0.0 && r < ends[1].0 {
                ends[1] = (r, Some(edge));
            }
        }
    }
    (ends[0].0 <= ends[1].0).then(|| {
        ends.map(|(t, edge)| {
            let mut p = lerp(a, b, t);
            match edge {
                Some(Edge::X(x)) => p.0 = x,
                Some(Edge::Y(y)) => p.1 = y,
                None => (),
            }
            (t, p)
        })
    })
}

/// Half of `b - a`, finite for any two finite values; the ratios and
/// points computed from it are those of `b - a` (halving is exact above
/// the subnormals).
fn half_diff(a: f64, b: f64) -> f64 {
    b / 2.0 - a / 2.0
}

fn lerp(a: (f64, f64), b: (f64, f64), t: f64) -> (f64, f64) {
    let at = |a: f64, b: f64| 2.0 * (a / 2.0 + half_diff(a, b) * t);
    (at(a.0, b.0), at(a.1, b.1))
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
            Some([(t0, a), (t1, b)]) => {
                if !open || t0 > 0.0 {
                    let mut run: LPooled<Vec<(f64, f64)>> = LPooled::take();
                    run.push(a);
                    runs.push(run);
                }
                if let Some(run) = runs.last_mut() {
                    run.push(b);
                }
                open = t1 >= 1.0;
            }
        }
    }
    runs
}

/// The outlines of the area under `pts` inside `view`, one for each run
/// clip_polyline would draw: the line cut to the view's x interval and
/// at its crossings of the view's bottom and top, its y then moved into
/// the view. Each, filled down to the baseline clamped the same way, is
/// exactly the visible part of the area under its run.
pub fn clip_area(
    pts: &[(f64, f64)],
    view: View,
) -> LPooled<Vec<LPooled<Vec<(f64, f64)>>>> {
    let in_x = View { x: view.x, y: (f64::NEG_INFINITY, f64::INFINITY) };
    let mut runs: LPooled<Vec<LPooled<Vec<(f64, f64)>>>> = LPooled::take();
    let mut open = false;
    for w in pts.windows(2) {
        let Some([(t0, a), (t1, b)]) = clip_segment(w[0], w[1], in_x) else {
            open = false;
            continue;
        };
        if !open || t0 > 0.0 {
            let mut run: LPooled<Vec<(f64, f64)>> = LPooled::take();
            run.push(a);
            runs.push(run);
        }
        let Some(run) = runs.last_mut() else { continue };
        let mut cuts =
            [view.y.0, view.y.1].map(|y| half_diff(a.1, y) / half_diff(a.1, b.1));
        if cuts[1] < cuts[0] {
            cuts.swap(0, 1);
        }
        for t in cuts {
            if 0.0 < t && t < 1.0 {
                run.push(lerp(a, b, t));
            }
        }
        run.push(b);
        open = t1 >= 1.0;
    }
    for p in runs.iter_mut().flat_map(|r| r.iter_mut()) {
        p.1 = view.clamp_y(p.1);
    }
    runs
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
        let runs = clip_area(&[(-5.0, 20.0), (5.0, 20.0), (15.0, -5.0)], V);
        assert_eq!(runs.len(), 1);
        let out = &runs[0];
        assert_eq!(out.first(), Some(&(0.0, 10.0)));
        assert_eq!(out.last(), Some(&(10.0, 7.5)));
        assert!(out.iter().all(|p| V.contains(*p)));
        // the outline follows the line down from where it crosses the top
        assert!(out.contains(&(9.0, 10.0)), "{out:?}");
    }

    /// Finite points so far apart that their difference overflows.
    const FAR: [[(f64, f64); 2]; 2] =
        [[(0.0, -1e308), (2.0, 1e308)], [(-1e308, 0.5), (1e308, 0.5)]];
    const NEAR: View = View { x: (0.0, 2.0), y: (0.0, 1.0) };

    #[test]
    fn a_line_between_far_points_is_cut_at_its_edges() {
        let runs = clip_polyline(&FAR[0], NEAR);
        assert_eq!(runs.len(), 1);
        assert!(runs[0].iter().all(|p| NEAR.contains(*p)), "{:?}", &runs[0][..]);
        assert!(runs[0].iter().all(|p| (p.0 - 1.0).abs() < 1e-9), "{:?}", &runs[0][..]);
        let runs = clip_polyline(&FAR[1], NEAR);
        assert_eq!(runs.len(), 1);
        assert_eq!(&runs[0][..], &[(0.0, 0.5), (2.0, 0.5)]);
    }

    #[test]
    fn an_area_between_far_points_is_cut_where_it_crosses() {
        let runs = clip_area(&FAR[0], NEAR);
        assert_eq!(runs.len(), 1);
        let out = &runs[0];
        assert_eq!(out.len(), 4, "{out:?}");
        assert_eq!((out[0], out[3]), ((0.0, 0.0), (2.0, 1.0)));
        assert!(out[1..3].iter().all(|p| (p.0 - 1.0).abs() < 1e-9), "{out:?}");
    }

    #[test]
    fn non_finite_points_break_the_area() {
        let pts = [(1.0, 1.0), (2.0, 2.0), (3.0, f64::NAN), (4.0, 1.0), (5.0, 2.0)];
        let runs = clip_area(&pts, V);
        assert_eq!(runs.len(), 2);
        assert_eq!(&runs[0][..], &[(1.0, 1.0), (2.0, 2.0)]);
        assert_eq!(&runs[1][..], &[(4.0, 1.0), (5.0, 2.0)]);
    }
}
