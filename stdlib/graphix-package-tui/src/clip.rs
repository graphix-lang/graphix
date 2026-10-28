type Point = (f64, f64);

fn midpoint(a: f64, b: f64) -> f64 {
    let d = b - a;
    if d.is_finite() { a + d / 2.0 } else { a / 2.0 + b / 2.0 }
}

fn intersection(mut a: Point, mut b: Point, x: f64) -> Point {
    if x == a.0 {
        return a;
    }
    if x == b.0 {
        return b;
    }
    let mid = (midpoint(a.0, b.0), midpoint(a.1, b.1));
    if x == mid.0 {
        return (x, mid.1);
    }
    if (a.0 < x && x < mid.0) || (mid.0 < x && x < a.0) {
        b = mid;
    } else {
        a = mid;
    }
    if (x - a.0).abs() > (x - b.0).abs() {
        std::mem::swap(&mut a, &mut b);
    }
    let dx = b.0 - a.0;
    let dy = b.1 - a.1;
    let slope = dy / dx;
    let y = if slope.is_finite() {
        (x - a.0).mul_add(slope, a.1)
    } else {
        let t = (x - a.0) / dx;
        (1.0 - t) * a.1 + t * b.1
    };
    (x, y.clamp(a.1.min(b.1), a.1.max(b.1)))
}

fn clip_axis(a: &mut Point, b: &mut Point, bounds: [f64; 2]) -> bool {
    let [min, max] = bounds;
    if (a.0 < min && b.0 < min) || (a.0 > max && b.0 > max) {
        return false;
    }
    if a.0 < min {
        *a = intersection(*a, *b, min);
    }
    if a.0 > max {
        *a = intersection(*a, *b, max);
    }
    if b.0 < min {
        *b = intersection(*a, *b, min);
    }
    if b.0 > max {
        *b = intersection(*a, *b, max);
    }
    true
}

pub(super) fn clip_line(
    mut a: Point,
    mut b: Point,
    x: [f64; 2],
    y: [f64; 2],
) -> Option<[Point; 2]> {
    if ![a.0, a.1, b.0, b.1, x[0], x[1], y[0], y[1]].iter().all(|v| v.is_finite())
        || x[0] >= x[1]
        || y[0] >= y[1]
    {
        return None;
    }
    if !clip_axis(&mut a, &mut b, x) {
        return None;
    }
    let (mut a, mut b) = ((a.1, a.0), (b.1, b.0));
    if !clip_axis(&mut a, &mut b, y) {
        return None;
    }
    Some([(a.1, a.0), (b.1, b.0)])
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn huge_segments_clip_to_their_visible_part() {
        for (a, b, expected) in [
            ((-1e300, 1e300), (5.0, 5.0), [(0.0, 10.0), (5.0, 5.0)]),
            ((-1e308, -1e308), (1e308, 1e308), [(0.0, 0.0), (10.0, 10.0)]),
            ((-1e308, 5.0), (1e308, 5.0), [(0.0, 5.0), (10.0, 5.0)]),
            ((5.0, -1e308), (5.0, 1e308), [(5.0, 0.0), (5.0, 10.0)]),
            ((-1e308, -3e307), (1e308, 3e307), [(0.0, 0.0), (10.0, 3.0)]),
            ((-20.0, 8.0), (20.0, 8.0), [(0.0, 8.0), (10.0, 8.0)]),
        ] {
            let forward = clip_line(a, b, [0.0, 10.0], [0.0, 10.0]).unwrap();
            let backward = clip_line(b, a, [0.0, 10.0], [0.0, 10.0]).unwrap();
            for (actual, expected) in forward
                .into_iter()
                .zip(expected)
                .chain(backward.into_iter().zip(expected.into_iter().rev()))
            {
                assert!(
                    (actual.0 - expected.0).abs() < 1e-12
                        && (actual.1 - expected.1).abs() < 1e-12,
                    "{a:?} {b:?}: {actual:?} != {expected:?}"
                );
            }
        }
    }

    #[test]
    fn outside_and_nonfinite_segments_are_empty() {
        for (a, b) in [
            ((-10.0, 1.0), (1.0, -10.0)),
            ((0.0, 11.0), (10.0, 11.0)),
            ((f64::NAN, 0.0), (5.0, 5.0)),
            ((0.0, 0.0), (f64::INFINITY, 5.0)),
        ] {
            assert_eq!(clip_line(a, b, [0.0, 10.0], [0.0, 10.0]), None);
        }
        assert_eq!(
            clip_line((5.0, 5.0), (5.0, 5.0), [0.0, 10.0], [0.0, 10.0]),
            Some([(5.0, 5.0); 2])
        );
    }
}
