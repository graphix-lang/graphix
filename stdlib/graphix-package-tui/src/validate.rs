//! Clamp user-supplied numeric values into the ranges ratatui (or a
//! downstream cast) requires, warning once per distinct bad value: the
//! caller threads an `Option<u64>` holding the last-warned bit pattern.

/// Upper bound for per-element visual sizes (bar widths, gaps, scroll
/// offsets): well below `u16::MAX` so ratatui's internal sums cannot
/// overflow u16, yet larger than any terminal dimension.
pub(crate) const VISUAL_DIMENSION_CAP: i64 = 1024;

/// Clamp an `i64` into `[0, VISUAL_DIMENSION_CAP]`. ratatui's layout
/// arithmetic overflows u16 well before `u16::MAX`.
pub(crate) fn clamp_u16(
    widget: &str,
    label: &str,
    last: &mut Option<i64>,
    raw: i64,
) -> u16 {
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
pub(crate) fn clamp_u64(
    widget: &str,
    label: &str,
    last: &mut Option<i64>,
    raw: i64,
) -> u64 {
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

/// Clamp an `i64` into `[0, usize::MAX]`. Negative values become 0;
/// 32-bit targets additionally truncate to `usize::MAX`.
pub(crate) fn clamp_usize(
    widget: &str,
    label: &str,
    last: &mut Option<i64>,
    raw: i64,
) -> usize {
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
