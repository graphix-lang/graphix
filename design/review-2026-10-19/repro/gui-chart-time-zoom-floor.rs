// Title: a time series cannot be zoomed narrower than ~3.6 s: handle_zoom's
// "past what f64 resolves" guard is relative to the axis' magnitude, 1e-9.
// Command: rustc --edition 2024 -O -o /tmp/tz design/review-2026-10-19/repro/gui-chart-time-zoom-floor.rs && /tmp/tz
// checked_range and the `zoom` closure are copied verbatim from
// stdlib/graphix-package-gui/src/widgets/chart/{ranges.rs,interact.rs}.
// Expected: zooming in on one second of 2026 data (x in ms since the epoch)
//   keeps narrowing the view; f64 resolves ~0.0002 ms at 1.8e12 ms and the
//   draw keeps microseconds (ms_datetime).
// Observed: the first zoom of a 1.1 s view is refused (prints None), and a
//   60 s view stops narrowing at 3782 ms.
pub fn checked_range(r: (f64, f64)) -> Option<(f64, f64)> {
    (r.0.is_finite() && r.1.is_finite() && r.0 < r.1 && (r.1 - r.0).is_finite())
        .then_some(r)
}

fn main() {
    let factor = 1.1f64.powf(-1.0);
    let zoom = |r: (f64, f64), t: f64| {
        let at = r.0 + t * (r.1 - r.0);
        let span = (r.1 - r.0) * factor;
        checked_range((at - t * span, at + (1.0 - t) * span))
            .filter(|z| z.1 - z.0 > 1e-9 * (z.0.abs() + z.1.abs()))
    };
    let t0 = 1_790_000_000_000.0f64; // 2026-09 in ms
    println!("1.1 s view, one wheel line in: {:?}", zoom((t0, t0 + 1100.0), 0.5));
    let mut r = (t0, t0 + 60_000.0);
    for _ in 0..200 {
        match zoom(r, 0.5) {
            Some(z) => r = z,
            None => break,
        }
    }
    println!("narrowest view reachable from 60 s: {:.1} ms", r.1 - r.0);
}
