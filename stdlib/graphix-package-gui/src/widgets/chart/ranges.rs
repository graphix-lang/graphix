use super::{dataset::DatasetEntry, types::*};
use chrono::{DateTime, Utc};
use graphix_rt::GXExt;

/// An hour in ms: how far a single time-series point's range reaches.
const HOUR_MS: f64 = 3_600_000.0;

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
            DatasetEntry::Surface { data, .. } => {
                data.t.iter().flat_map(|g| g.0.iter().flatten()).for_each(&mut add)
            }
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
    let (lo, hi) = if min == max { (min - unit, max + unit) } else { (min, max) };
    let pad = (hi / 2.0 - lo / 2.0) / 10.0;
    let (lo, hi) = ((lo - pad).max(-LIMIT), (hi + pad).min(LIMIT));
    if lo < hi {
        (lo, hi)
    } else {
        (lo - unit.max(lo.abs() * 1e-9), hi + unit.max(hi.abs() * 1e-9))
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

/// A time range in ms, inside what a datetime can hold.
pub fn time_range(r: (f64, f64)) -> (f64, f64) {
    let lo = datetime_ms(&DateTime::<Utc>::MIN_UTC);
    let hi = datetime_ms(&DateTime::<Utc>::MAX_UTC);
    let (a, b) = (r.0.clamp(lo, hi), r.1.clamp(lo, hi));
    if a < b {
        (a, b)
    } else if b < hi {
        (b, (b + HOUR_MS).min(hi))
    } else {
        (hi - HOUR_MS, hi)
    }
}

/// Decimal places needed to distinguish ~10 plotters ticks over the range.
pub fn tick_precision(range: f64) -> usize {
    let step = range / 10.0;
    if step >= 1.0 {
        1
    } else if step >= 0.1 {
        2
    } else if step >= 0.01 {
        3
    } else {
        4
    }
}
