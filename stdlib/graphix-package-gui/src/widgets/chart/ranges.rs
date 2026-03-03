use super::dataset::DatasetEntry;
use super::types::*;
use chrono::{DateTime, TimeDelta, Utc};
use graphix_rt::GXExt;

/// Compute numeric axis ranges across all dataset entries.
pub(super) fn compute_ranges<X: GXExt>(
    datasets: &[DatasetEntry<X>],
) -> ((f64, f64), (f64, f64)) {
    let mut x_min = f64::INFINITY;
    let mut x_max = f64::NEG_INFINITY;
    let mut y_min = f64::INFINITY;
    let mut y_max = f64::NEG_INFINITY;

    macro_rules! extend {
        ($x:expr, $ylo:expr, $yhi:expr) => {
            if $x < x_min {
                x_min = $x;
            }
            if $x > x_max {
                x_max = $x;
            }
            if $ylo < y_min {
                y_min = $ylo;
            }
            if $yhi > y_max {
                y_max = $yhi;
            }
        };
    }

    for ds in datasets {
        match ds {
            DatasetEntry::XY { data, .. } | DatasetEntry::DashedLine { data, .. } => {
                if let Some(XYData::Numeric(pts)) = data.t.as_ref() {
                    for &(x, y) in pts.iter() {
                        extend!(x, y, y);
                    }
                }
            }
            DatasetEntry::Bar { .. }
            | DatasetEntry::Pie { .. }
            | DatasetEntry::Scatter3D { .. }
            | DatasetEntry::Line3D { .. }
            | DatasetEntry::Surface { .. } => {}
            DatasetEntry::Candlestick { data, .. } => {
                if let Some(OHLCData::Numeric(pts)) = data.t.as_ref() {
                    for pt in pts.iter() {
                        extend!(pt.x, pt.low, pt.high);
                    }
                }
            }
            DatasetEntry::ErrorBar { data, .. } => {
                if let Some(EBData::Numeric(pts)) = data.t.as_ref() {
                    for pt in pts.iter() {
                        extend!(pt.x, pt.min, pt.max);
                    }
                }
            }
        }
    }

    (pad_range(x_min, x_max), pad_range(y_min, y_max))
}

/// Compute datetime x-axis and numeric y-axis ranges across all dataset entries.
pub(super) fn compute_time_ranges<X: GXExt>(
    datasets: &[DatasetEntry<X>],
) -> ((DateTime<Utc>, DateTime<Utc>), (f64, f64)) {
    let mut x_min = DateTime::<Utc>::MAX_UTC;
    let mut x_max = DateTime::<Utc>::MIN_UTC;
    let mut y_min = f64::INFINITY;
    let mut y_max = f64::NEG_INFINITY;

    macro_rules! extend_y {
        ($ylo:expr, $yhi:expr) => {
            if $ylo < y_min {
                y_min = $ylo;
            }
            if $yhi > y_max {
                y_max = $yhi;
            }
        };
    }

    macro_rules! extend_x {
        ($x:expr) => {
            if $x < x_min {
                x_min = $x;
            }
            if $x > x_max {
                x_max = $x;
            }
        };
    }

    for ds in datasets {
        match ds {
            DatasetEntry::XY { data, .. } | DatasetEntry::DashedLine { data, .. } => {
                if let Some(XYData::DateTime(pts)) = data.t.as_ref() {
                    for &(x, y) in pts.iter() {
                        extend_x!(x);
                        extend_y!(y, y);
                    }
                }
            }
            DatasetEntry::Bar { .. }
            | DatasetEntry::Pie { .. }
            | DatasetEntry::Scatter3D { .. }
            | DatasetEntry::Line3D { .. }
            | DatasetEntry::Surface { .. } => {}
            DatasetEntry::Candlestick { data, .. } => {
                if let Some(OHLCData::DateTime(pts)) = data.t.as_ref() {
                    for pt in pts.iter() {
                        extend_x!(pt.x);
                        extend_y!(pt.low, pt.high);
                    }
                }
            }
            DatasetEntry::ErrorBar { data, .. } => {
                if let Some(EBData::DateTime(pts)) = data.t.as_ref() {
                    for pt in pts.iter() {
                        extend_x!(pt.x);
                        extend_y!(pt.min, pt.max);
                    }
                }
            }
        }
    }

    (pad_time_range(x_min, x_max), pad_range(y_min, y_max))
}

/// Compute 3D axis ranges across all 3D dataset entries.
pub(super) fn compute_3d_ranges<X: GXExt>(
    datasets: &[DatasetEntry<X>],
) -> ((f64, f64), (f64, f64), (f64, f64)) {
    let mut x_min = f64::INFINITY;
    let mut x_max = f64::NEG_INFINITY;
    let mut y_min = f64::INFINITY;
    let mut y_max = f64::NEG_INFINITY;
    let mut z_min = f64::INFINITY;
    let mut z_max = f64::NEG_INFINITY;

    macro_rules! extend3 {
        ($x:expr, $y:expr, $z:expr) => {
            if $x < x_min {
                x_min = $x;
            }
            if $x > x_max {
                x_max = $x;
            }
            if $y < y_min {
                y_min = $y;
            }
            if $y > y_max {
                y_max = $y;
            }
            if $z < z_min {
                z_min = $z;
            }
            if $z > z_max {
                z_max = $z;
            }
        };
    }

    for ds in datasets {
        match ds {
            DatasetEntry::Scatter3D { data, .. } | DatasetEntry::Line3D { data, .. } => {
                if let Some(pts) = data.t.as_ref() {
                    for &(x, y, z) in pts.0.iter() {
                        extend3!(x, y, z);
                    }
                }
            }
            DatasetEntry::Surface { data, .. } => {
                if let Some(grid) = data.t.as_ref() {
                    for row in grid.0.iter() {
                        for &(x, y, z) in row.iter() {
                            extend3!(x, y, z);
                        }
                    }
                }
            }
            _ => {}
        }
    }

    (pad_range(x_min, x_max), pad_range(y_min, y_max), pad_range(z_min, z_max))
}

pub(crate) fn pad_range(min: f64, max: f64) -> (f64, f64) {
    if min > max {
        return (-1.0, 1.0);
    }
    let (min, max) = if min == max { (min - 1.0, max + 1.0) } else { (min, max) };
    let pad = (max - min) * 0.05;
    (min - pad, max + pad)
}

/// Estimate the number of decimal places needed for tick labels
/// given the axis range. Plotters generates ~10 ticks, so the step
/// is roughly range/10. We need enough precision to distinguish ticks.
pub(super) fn tick_precision(range: f64) -> usize {
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

pub(super) fn pad_time_range(
    min: DateTime<Utc>,
    max: DateTime<Utc>,
) -> (DateTime<Utc>, DateTime<Utc>) {
    if min > max {
        let now = Utc::now();
        return (now - TimeDelta::hours(1), now + TimeDelta::hours(1));
    }
    if min == max {
        return (min - TimeDelta::hours(1), max + TimeDelta::hours(1));
    }
    let span = max - min;
    let pad = span * 5 / 100;
    (min - pad, max + pad)
}
