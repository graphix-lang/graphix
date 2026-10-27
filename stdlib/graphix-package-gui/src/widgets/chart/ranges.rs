use super::{
    dataset::{ChartMode, DatasetEntry},
    types::*,
};
use chrono::{DateTime, Utc};
use graphix_rt::GXExt;

/// An hour in ms: how far a single time-series point's range reaches.
const HOUR_MS: f64 = 3_600_000.0;

/// How far inside chrono's ends a drawn time range stays: plotters steps
/// its key points one period past a range's end, and below 292 years a
/// period is at most 2.5 spans.
const TIME_MARGIN_MS: f64 = 1000.0 * 365.25 * 24.0 * HOUR_MS;

/// The extent of the finite values added to it.
struct Extent {
    lo: f64,
    hi: f64,
}

impl Extent {
    fn new() -> Self {
        Self { lo: f64::INFINITY, hi: f64::NEG_INFINITY }
    }

    fn add(&mut self, v: f64) {
        if v.is_finite() {
            self.lo = self.lo.min(v);
            self.hi = self.hi.max(v);
        }
    }
}

/// The x (in ms for a time series) and y ranges of the 2D datasets of the
/// one kind `time` says, padded.
pub fn compute_ranges<X: GXExt>(
    datasets: &[DatasetEntry<X>],
    time: bool,
) -> ((f64, f64), (f64, f64)) {
    let (mut x, mut y) = (Extent::new(), Extent::new());
    for ds in datasets {
        match ds {
            DatasetEntry::XY { data, .. } => {
                for &(px, py) in
                    data.t.iter().filter(|d| d.time == time).flat_map(|d| d.pts.iter())
                {
                    x.add(px);
                    y.add(py);
                }
            }
            DatasetEntry::Candlestick { data, .. } => {
                for p in
                    data.t.iter().filter(|d| d.time == time).flat_map(|d| d.pts.iter())
                {
                    x.add(p.x);
                    y.add(p.low);
                    y.add(p.high);
                }
            }
            DatasetEntry::ErrorBar { data, .. } => {
                for p in
                    data.t.iter().filter(|d| d.time == time).flat_map(|d| d.pts.iter())
                {
                    x.add(p.x);
                    y.add(p.min);
                    y.add(p.max);
                }
            }
            DatasetEntry::Bar { .. }
            | DatasetEntry::Pie { .. }
            | DatasetEntry::Scatter3D { .. }
            | DatasetEntry::Line3D { .. }
            | DatasetEntry::Surface { .. } => {}
        }
    }
    let x = match time {
        true => time_range(pad(x, HOUR_MS)),
        false => pad(x, 1.0),
    };
    (x, pad(y, 1.0))
}

/// The x, y and z ranges of the 3D datasets, padded.
pub fn compute_3d_ranges<X: GXExt>(
    datasets: &[DatasetEntry<X>],
) -> ((f64, f64), (f64, f64), (f64, f64)) {
    let (mut x, mut y, mut z) = (Extent::new(), Extent::new(), Extent::new());
    let mut add = |&(px, py, pz): &(f64, f64, f64)| {
        x.add(px);
        y.add(py);
        z.add(pz);
    };
    for ds in datasets {
        match ds {
            DatasetEntry::Scatter3D { data, .. } | DatasetEntry::Line3D { data, .. } => {
                data.t.iter().flat_map(|d| d.0.iter()).for_each(&mut add)
            }
            DatasetEntry::Surface { data, .. } => data
                .t
                .iter()
                .flat_map(|g| g.0.iter().flat_map(|r| r.iter()))
                .for_each(&mut add),
            _ => {}
        }
    }
    (pad(x, 1.0), pad(y, 1.0), pad(z, 1.0))
}

fn pad(e: Extent, unit: f64) -> (f64, f64) {
    pad_range_by(e.lo, e.hi, unit)
}

/// `min..max` with 5% on each side; a single value reaches `unit` each
/// way, no value is `-unit..unit`. Always a `checked_range`.
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

/// A numeric axis' padded range; see `pad_range_by`.
pub fn pad_range(min: f64, max: f64) -> (f64, f64) {
    pad_range_by(min, max, 1.0)
}

/// `r` when plotters can draw over it: finite ends, `min < max`, a
/// finite span.
pub fn checked_range(r: (f64, f64)) -> Option<(f64, f64)> {
    (r.0.is_finite() && r.1.is_finite() && r.0 < r.1 && (r.1 - r.0).is_finite())
        .then_some(r)
}

/// A drawable time range in ms: `r` slid inside chrono's range less
/// `TIME_MARGIN_MS` at each end, its span kept, or all of that when `r`
/// is wider. `r` is ordered.
pub fn time_range(r: (f64, f64)) -> (f64, f64) {
    let lo = datetime_ms(&DateTime::<Utc>::MIN_UTC) + TIME_MARGIN_MS;
    let hi = datetime_ms(&DateTime::<Utc>::MAX_UTC) - TIME_MARGIN_MS;
    let span = r.1 - r.0;
    if !(span < hi - lo) {
        (lo, hi)
    } else if r.0 < lo {
        (lo, lo + span.max(1.0))
    } else if r.1 > hi {
        (hi - span.max(1.0), hi)
    } else {
        r
    }
}

/// The x range `mode` draws for the view `x`.
pub fn drawable_x(mode: ChartMode, x: (f64, f64)) -> (f64, f64) {
    match mode {
        ChartMode::TimeSeries => time_range(x),
        _ => x,
    }
}

/// How a value axis writes its ticks: enough digits to tell neighbours
/// apart, and scientific notation for magnitudes fixed notation writes
/// long.
#[derive(Clone, Copy, Debug)]
pub struct TickFormat {
    decimals: usize,
    scientific: bool,
}

impl TickFormat {
    /// For `labels` ticks over `range`.
    pub fn new(range: (f64, f64), labels: usize) -> Self {
        let step = (range.1 - range.0) / labels.max(1) as f64;
        let magnitude = range.0.abs().max(range.1.abs());
        let decimals = |x: f64| match x.is_finite() && x > 0.0 {
            true => ((-x.log10()).ceil().max(0.0) as usize).min(15),
            false => 0,
        };
        if magnitude >= 1e7 || (0.0 < magnitude && magnitude < 1e-3) {
            Self { decimals: decimals(step / magnitude), scientific: true }
        } else {
            Self { decimals: decimals(step).max(1), scientific: false }
        }
    }

    pub fn format(self, v: f64) -> String {
        let d = self.decimals;
        if self.scientific { format!("{v:.d$e}") } else { format!("{v:.d$}") }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn ticks_tell_their_values_apart() {
        for (range, n) in
            [((0.0, 2e-6), 10), ((1000.0, 1000.00001), 10), ((0.0, 10.0), 5)]
        {
            let fmt = TickFormat::new(range, n);
            let step = (range.1 - range.0) / n as f64;
            let labels: Vec<String> =
                (0..=n).map(|k| fmt.format(range.0 + k as f64 * step)).collect();
            let mut unique = labels.clone();
            unique.dedup();
            assert_eq!(unique, labels, "{range:?}");
        }
        let fmt = TickFormat::new((0.0, 5e20), 10);
        assert!(fmt.format(4.5e20).len() <= 8, "{}", fmt.format(4.5e20));
        assert_eq!(TickFormat::new((0.0, 10.0), 10).format(2.0), "2.0");
    }

    #[test]
    fn padding_extreme_values_keeps_an_ordered_bounded_range() {
        let limit = f64::MAX / 4.0;
        for (min, max) in [
            (1e308, 1e308),
            (1e308, f64::MAX),
            (-f64::MAX, -1e308),
            (-1e308, -1e308),
            (-f64::MAX, f64::MAX),
            (0.0, 0.0),
            (f64::NAN, f64::NAN),
            (f64::NEG_INFINITY, f64::INFINITY),
        ] {
            let r = pad_range(min, max);
            assert_eq!(checked_range(r), Some(r), "{min}..{max}: {r:?}");
            assert!(r.0 >= -limit && r.1 <= limit, "{r:?}");
            if min > limit {
                assert!(r.0 > limit / 2.0);
            }
            if max < -limit {
                assert!(r.1 < -limit / 2.0);
            }
        }
    }
}
