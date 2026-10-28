type Point = (f64, f64);

fn scale(a: f64, b: f64) -> f64 {
    let exponent = a.abs().max(b.abs()).to_bits() & (0x7ff << 52);
    f64::from_bits(exponent.max(1 << 52))
}

fn intersection(a: Point, b: Point, x: f64) -> Point {
    if x == a.0 {
        return a;
    }
    if x == b.0 {
        return b;
    }
    let sx = scale(a.0, b.0);
    let sy = scale(a.1, b.1);
    let (ax, ay, bx, by) = (a.0 / sx, a.1 / sy, b.0 / sx, b.1 / sy);
    let product = ax * by;
    let error = (-ax).mul_add(by, product);
    let determinant = ay.mul_add(bx, -product) + error;
    let y = (x / sx).mul_add(by - ay, determinant) / (bx - ax) * sy;
    (x, y)
}

fn clip_axis(
    a: &mut Point,
    b: &mut Point,
    original: [Point; 2],
    bounds: [f64; 2],
) -> bool {
    let [min, max] = bounds;
    if (a.0 < min && b.0 < min) || (a.0 > max && b.0 > max) {
        return false;
    }
    for p in [a, b] {
        if p.0 < min || p.0 > max {
            *p = intersection(original[0], original[1], p.0.clamp(min, max));
        }
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
    let original = [(a.1, a.0), (b.1, b.0)];
    let endpoints = [a, b];
    if !clip_axis(&mut a, &mut b, endpoints, x) {
        return None;
    }
    let (mut a, mut b) = ((a.1, a.0), (b.1, b.0));
    if !clip_axis(&mut a, &mut b, original, y) {
        return None;
    }
    Some([(a.1.clamp(x[0], x[1]), a.0), (b.1.clamp(x[0], x[1]), b.0)])
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
    fn asymmetric_segments_keep_their_slope() {
        for scale in [1e16, 1e300] {
            let a = (-scale, -0.7 * scale);
            let b = (3.0 * scale, 2.1 * scale);
            for (a, b, expected) in
                [(a, b, [(0.0, 0.0), (10.0, 7.0)]), (b, a, [(10.0, 7.0), (0.0, 0.0)])]
            {
                let got = clip_line(a, b, [0.0, 10.0], [0.0, 10.0]);
                let got = got.unwrap_or_else(|| panic!("{a:?} -> {b:?} disappeared"));
                for (got, expected) in got.into_iter().zip(expected) {
                    assert!(
                        (got.0 - expected.0).abs() < 1e-12
                            && (got.1 - expected.1).abs() < 1e-12,
                        "{got:?} != {expected:?}"
                    );
                }
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
