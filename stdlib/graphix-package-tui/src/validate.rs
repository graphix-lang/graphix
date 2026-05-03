//! Helpers for clamping user-supplied numeric values into the ranges
//! ratatui (or downstream type casts) require, with deduplicated
//! `log::warn!` so a stuck-bad value doesn't spam the log.
//!
//! All helpers follow the same shape as `clamp_ratio` in `gauge.rs`:
//! the caller threads through an `Option<u64>` field on the widget
//! struct that records the bit pattern of the last-warned value;
//! returning to the valid range resets it.

/// Visual upper bound for "size of a single visual element in a row /
/// column" parameters (bar widths, gaps, scroll offsets). Picked
/// well below `u16::MAX` so ratatui's internal sums (e.g.
/// `n_bars * bar_width + (n_bars - 1) * bar_gap + group_gap`) can't
/// overflow u16 for any realistic group size, while still being
/// larger than any conceivable terminal dimension.
pub(crate) const VISUAL_DIMENSION_CAP: i64 = 1024;

/// Clamp an `i64` (graphix's native integer type) into the
/// `[0, VISUAL_DIMENSION_CAP]` range. ratatui's u16 parameters wouldn't
/// strictly fail until `u16::MAX`, but its internal layout arithmetic
/// (multiplying group sizes, summing widths and gaps) overflows well
/// before that. The visual cap keeps the math safe; values larger
/// than any real terminal width get clamped + warned.
pub(crate) fn clamp_u16(widget: &str, label: &str, last: &mut Option<i64>, raw: i64) -> u16 {
    if (0..=VISUAL_DIMENSION_CAP).contains(&raw) {
        *last = None;
        return raw as u16;
    }
    let clamped = raw.clamp(0, VISUAL_DIMENSION_CAP) as u16;
    if *last != Some(raw) {
        log::warn!(
            "{widget} {label} {raw} outside [0, {VISUAL_DIMENSION_CAP}]; \
             clamping to {clamped}"
        );
        *last = Some(raw);
    }
    clamped
}

/// Clamp an `i64` into the `[0, u64::MAX]` range. Negative values
/// become 0; unsigned overflow isn't reachable from i64.
pub(crate) fn clamp_u64(widget: &str, label: &str, last: &mut Option<i64>, raw: i64) -> u64 {
    if raw >= 0 {
        *last = None;
        return raw as u64;
    }
    if *last != Some(raw) {
        log::warn!("{widget} {label} {raw} negative; clamping to 0");
        *last = Some(raw);
    }
    0
}

/// Clamp an `i64` into the `[0, usize::MAX]` range. Negative values
/// become 0. On 64-bit targets `usize::MAX == u64::MAX` so the
/// positive side is also safe; on 32-bit targets we additionally
/// truncate to `usize::MAX`.
pub(crate) fn clamp_usize(widget: &str, label: &str, last: &mut Option<i64>, raw: i64) -> usize {
    if raw < 0 {
        if *last != Some(raw) {
            log::warn!("{widget} {label} {raw} negative; clamping to 0");
            *last = Some(raw);
        }
        return 0;
    }
    let max = usize::MAX as u128;
    if raw as u128 > max {
        if *last != Some(raw) {
            log::warn!("{widget} {label} {raw} exceeds usize::MAX; clamping");
            *last = Some(raw);
        }
        return usize::MAX;
    }
    *last = None;
    raw as usize
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn u16_in_range() {
        let mut last = None;
        assert_eq!(clamp_u16("w", "v", &mut last, 100), 100);
        assert_eq!(clamp_u16("w", "v", &mut last, 0), 0);
        assert_eq!(
            clamp_u16("w", "v", &mut last, VISUAL_DIMENSION_CAP),
            VISUAL_DIMENSION_CAP as u16
        );
        assert_eq!(last, None);
    }

    #[test]
    fn u16_clamps_negative() {
        let mut last = None;
        assert_eq!(clamp_u16("w", "v", &mut last, -1), 0);
        assert!(last.is_some());
    }

    #[test]
    fn u16_clamps_above_visual_cap() {
        let mut last = None;
        assert_eq!(
            clamp_u16("w", "v", &mut last, 1_000_000),
            VISUAL_DIMENSION_CAP as u16
        );
        assert!(last.is_some());
    }

    #[test]
    fn u16_dedupes() {
        let mut last = None;
        clamp_u16("w", "v", &mut last, -5);
        let after_first = last;
        clamp_u16("w", "v", &mut last, -5);
        assert_eq!(last, after_first);
    }

    #[test]
    fn u64_clamps_negative() {
        let mut last = None;
        assert_eq!(clamp_u64("w", "v", &mut last, -10), 0);
        assert!(last.is_some());
    }

    #[test]
    fn u64_passes_zero_and_positive() {
        let mut last = None;
        assert_eq!(clamp_u64("w", "v", &mut last, 0), 0);
        assert_eq!(clamp_u64("w", "v", &mut last, i64::MAX), i64::MAX as u64);
        assert_eq!(last, None);
    }

    #[test]
    fn usize_clamps_negative() {
        let mut last = None;
        assert_eq!(clamp_usize("w", "v", &mut last, -1), 0);
        assert!(last.is_some());
    }
}
