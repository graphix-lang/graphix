//! Numbers a widget takes clamped into the range ratatui accepts. Each
//! type's `FromValue` clamps a delivered value once and warns, so a
//! widget holds only valid values and draws without checking them.

use anyhow::Result;
use netidx::publisher::{FromValue, Value};

/// Upper bound for per-element visual sizes (bar widths, gaps, margins):
/// well below `u16::MAX` so ratatui's internal sums cannot overflow u16,
/// yet larger than any terminal dimension.
pub(crate) const VISUAL_DIMENSION_CAP: i64 = 1024;

/// `raw` clamped into `lo..=hi`, warning when it was outside.
fn clamped(what: &str, raw: i64, lo: i64, hi: i64) -> i64 {
    let c = raw.clamp(lo, hi);
    if c != raw {
        log::warn!("{what} {raw} outside [{lo}, {hi}]; clamping to {c}");
    }
    c
}

/// A visual size (a bar width, a gap, a margin, a spacing):
/// `0..=VISUAL_DIMENSION_CAP`.
#[derive(Clone, Copy, Debug, PartialEq)]
pub(crate) struct Dim(pub u16);

impl FromValue for Dim {
    fn from_value(v: Value) -> Result<Self> {
        Ok(Self(clamped("size", v.cast_to::<i64>()?, 0, VISUAL_DIMENSION_CAP) as u16))
    }
}

/// A content offset (lines or chars to scroll past): `0..=u16::MAX`.
#[derive(Clone, Copy, Debug, PartialEq)]
pub(crate) struct Offset(pub u16);

impl FromValue for Offset {
    fn from_value(v: Value) -> Result<Self> {
        Ok(Self(clamped("offset", v.cast_to::<i64>()?, 0, u16::MAX as i64) as u16))
    }
}

/// An index or a count: not negative.
#[derive(Clone, Copy, Debug, PartialEq)]
pub(crate) struct Index(pub usize);

impl FromValue for Index {
    fn from_value(v: Value) -> Result<Self> {
        Ok(Self(clamped("index", v.cast_to::<i64>()?, 0, i64::MAX) as usize))
    }
}

/// A percentage: `0..=100`.
#[derive(Clone, Copy, Debug, PartialEq)]
pub(crate) struct Percent(pub u16);

impl FromValue for Percent {
    fn from_value(v: Value) -> Result<Self> {
        Ok(Self(clamped("percentage", v.cast_to::<i64>()?, 0, 100) as u16))
    }
}

/// A color channel or palette index: `0..=255`.
#[derive(Clone, Copy, Debug, PartialEq)]
pub(crate) struct Byte(pub u8);

impl FromValue for Byte {
    fn from_value(v: Value) -> Result<Self> {
        Ok(Self(clamped("color component", v.cast_to::<i64>()?, 0, 255) as u8))
    }
}

/// A fraction: `0.0..=1.0`, NaN read as 0.
#[derive(Clone, Copy, Debug, PartialEq)]
pub(crate) struct Ratio(pub f64);

impl FromValue for Ratio {
    fn from_value(v: Value) -> Result<Self> {
        let raw = v.cast_to::<f64>()?;
        let c = if raw.is_nan() { 0.0 } else { raw.clamp(0.0, 1.0) };
        if c != raw {
            log::warn!("ratio {raw} outside [0, 1]; clamping to {c}");
        }
        Ok(Self(c))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn of<T: FromValue>(v: Value) -> T {
        T::from_value(v).unwrap()
    }

    #[test]
    fn sizes_clamp_to_the_cap() {
        assert_eq!(of::<Dim>(Value::I64(100)), Dim(100));
        assert_eq!(of::<Dim>(Value::I64(-1)), Dim(0));
        assert_eq!(of::<Dim>(Value::I64(1_000_000)), Dim(VISUAL_DIMENSION_CAP as u16));
    }

    #[test]
    fn offsets_reach_u16_max() {
        assert_eq!(of::<Offset>(Value::I64(1500)), Offset(1500));
        assert_eq!(of::<Offset>(Value::I64(-3)), Offset(0));
        assert_eq!(of::<Offset>(Value::I64(1 << 40)), Offset(u16::MAX));
    }

    #[test]
    fn indexes_are_not_negative() {
        assert_eq!(of::<Index>(Value::I64(-1)), Index(0));
        assert_eq!(of::<Index>(Value::I64(7)), Index(7));
    }

    #[test]
    fn percentages_and_bytes_clamp() {
        assert_eq!(of::<Percent>(Value::I64(101)), Percent(100));
        assert_eq!(of::<Percent>(Value::I64(-1)), Percent(0));
        assert_eq!(of::<Byte>(Value::I64(300)), Byte(255));
    }

    #[test]
    fn ratios_clamp_and_nan_is_zero() {
        for (raw, want) in [(0.5, 0.5), (1.5, 1.0), (-0.3, 0.0), (f64::INFINITY, 1.0)] {
            assert_eq!(of::<Ratio>(Value::F64(raw)), Ratio(want));
        }
        assert_eq!(of::<Ratio>(Value::F64(f64::NAN)), Ratio(0.0));
    }
}
