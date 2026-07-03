//! The per-cycle trace outcome (oracle V2).
//!
//! V1 compared the FIRST value the watched expr emitted — blind to
//! everything multi-cycle: extra fires, missing fires, wrong pacing,
//! and every value after the first. V2 compares the whole observable
//! history: for each epoch (one injection burst; epoch 0 is the
//! compile), the sequence of `(cycle offset, value)` the watched expr
//! produced, plus whether the runtime-side trace budget was hit.
//!
//! Offsets are relative to each epoch's ANCHOR — the first event of
//! its [`TraceSegment`] (the `Compiled` marker for epoch 0, an input
//! ref's own echo for injection epochs) — because absolute runtime
//! cycle numbers are not comparable across runs (startup and control
//! traffic shift them; mid-burst RELATIVE pacing is deterministic).
//!
//! Node-walk is canonical: in a divergence, `first_difference(interp,
//! jit)` classifies what the JIT did wrong, and that classification is
//! part of the bug bucket so the minimizer can't morph a missing-fire
//! bug into a value bug while reducing.

use graphix_compiler::expr::ExprId;
use graphix_rt::{TraceEvent, TraceSegment};
use netidx::publisher::Value;

/// Runtime-side trace budgets (see [`graphix_rt::GXHandle::trace_start`]):
/// total recorded events per trace, and active cycles per segment. Both
/// are DATA — identical in every mode by construction — so hitting a
/// budget is deterministic and a cap mismatch is a real divergence.
/// With schedules (Phase 3) these become per-program schedule data.
pub const MAX_EVENTS: usize = 512;
pub const MAX_CYCLES: u64 = 64;

/// The observable history of one epoch: every value the watched expr
/// emitted, at its cycle offset from the epoch anchor.
#[derive(Debug, Clone, PartialEq)]
pub struct Epoch {
    pub events: Vec<(u32, Value)>,
    /// The trace budget was hit during (or before) this epoch. Sticky
    /// runtime-side: every later epoch of a capped trace is empty.
    pub capped: bool,
}

impl Epoch {
    /// Project a runtime segment onto the watched expr: anchor at the
    /// segment's first event, keep `Updated(eid)` values.
    pub fn from_segment(seg: &TraceSegment, eid: ExprId) -> Self {
        let anchor = match seg.events.first() {
            None => 0,
            Some(
                TraceEvent::Compiled { cycle, .. } | TraceEvent::Updated { cycle, .. },
            ) => *cycle,
        };
        let events = seg
            .events
            .iter()
            .filter_map(|e| match e {
                TraceEvent::Updated { cycle, id, value } if *id == eid => {
                    Some(((*cycle - anchor) as u32, value.clone()))
                }
                TraceEvent::Updated { .. } | TraceEvent::Compiled { .. } => None,
            })
            .collect();
        Epoch { events, capped: seg.capped_cycles || seg.capped_events }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Trace {
    pub epochs: Vec<Epoch>,
}

impl Trace {
    pub fn from_segments(segs: &[TraceSegment], eid: ExprId) -> Self {
        Trace { epochs: segs.iter().map(|s| Epoch::from_segment(s, eid)).collect() }
    }

    /// Structural equality. `Value`'s own equality is graphix's total
    /// order (`NaN == NaN`, `-0.0 == 0.0`), so no float special-casing.
    pub fn agrees_with(&self, other: &Trace) -> bool {
        self == other
    }

    /// Classify the first difference, `self` = the node-walk reference,
    /// `other` = the JIT under test. `None` when the traces agree.
    pub fn first_difference(&self, other: &Trace) -> Option<TraceDiff> {
        for (a, b) in self.epochs.iter().zip(other.epochs.iter()) {
            if a.capped != b.capped {
                return Some(TraceDiff::CapMismatch);
            }
            for ((ao, av), (bo, bv)) in a.events.iter().zip(b.events.iter()) {
                if av != bv {
                    return Some(TraceDiff::ValueMismatch);
                }
                if ao != bo {
                    return Some(TraceDiff::Pacing);
                }
            }
            if a.events.len() > b.events.len() {
                return Some(TraceDiff::MissingFire);
            }
            if a.events.len() < b.events.len() {
                return Some(TraceDiff::ExtraFire);
            }
        }
        if self.epochs.len() != other.epochs.len() {
            return Some(TraceDiff::EpochCount);
        }
        None
    }
}

/// What the JIT did wrong, relative to the canonical node-walk. Part
/// of the divergence bucket key.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TraceDiff {
    /// Different number of epochs — a driver-level failure (an
    /// injection wait died in one mode), not a program semantics one.
    EpochCount,
    /// Same position in the history, different value.
    ValueMismatch,
    /// The JIT never produced a fire the node-walk did.
    MissingFire,
    /// The JIT produced a fire the node-walk never did.
    ExtraFire,
    /// Same values in the same order, at different cycle offsets.
    Pacing,
    /// One mode hit the trace budget where the other quiesced — a
    /// deterministic budget makes this a real firing divergence.
    CapMismatch,
}

#[cfg(test)]
mod tests {
    use super::*;

    fn tr(epochs: Vec<Vec<(u32, i64)>>) -> Trace {
        Trace {
            epochs: epochs
                .into_iter()
                .map(|e| Epoch {
                    events: e.into_iter().map(|(o, v)| (o, Value::I64(v))).collect(),
                    capped: false,
                })
                .collect(),
        }
    }

    #[test]
    fn first_difference_classification() {
        let a = tr(vec![vec![(0, 1), (1, 2)]]);
        assert_eq!(a.first_difference(&a), None);
        // Value beats pacing when both differ at the same index.
        let b = tr(vec![vec![(0, 1), (2, 3)]]);
        assert_eq!(a.first_difference(&b), Some(TraceDiff::ValueMismatch));
        let c = tr(vec![vec![(0, 1), (2, 2)]]);
        assert_eq!(a.first_difference(&c), Some(TraceDiff::Pacing));
        let d = tr(vec![vec![(0, 1)]]);
        assert_eq!(a.first_difference(&d), Some(TraceDiff::MissingFire));
        assert_eq!(d.first_difference(&a), Some(TraceDiff::ExtraFire));
        let e = tr(vec![vec![(0, 1), (1, 2)], vec![]]);
        assert_eq!(a.first_difference(&e), Some(TraceDiff::EpochCount));
        let mut f = a.clone();
        f.epochs[0].capped = true;
        assert_eq!(a.first_difference(&f), Some(TraceDiff::CapMismatch));
        // A difference in a common epoch wins over the epoch count.
        let g = tr(vec![vec![(0, 9), (1, 2)], vec![]]);
        assert_eq!(a.first_difference(&g), Some(TraceDiff::ValueMismatch));
    }
}
