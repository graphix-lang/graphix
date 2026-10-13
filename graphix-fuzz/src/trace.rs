//! The per-cycle trace outcome the oracle compares: for each epoch
//! (one injection burst; epoch 0 is the compile), the `(cycle offset,
//! value)` sequence the watched expr produced, plus whether the trace
//! budget was hit.
//!
//! Offsets are relative to each epoch's anchor, the first event of its
//! [`TraceSegment`], because absolute cycle numbers are not comparable
//! across runs. Node-walk is canonical: `first_difference(interp, jit)`
//! classifies what the JIT did wrong and is part of the bug bucket.

use arcstr::{ArcStr, literal};
use graphix_compiler::{
    NoUserEvent, abstract_value,
    env::Env,
    expr::ExprId,
    node::lambda::{DefOrigin, LambdaDef},
};
use graphix_rt::{GXRt, NoExt, TraceEvent, TraceSegment};
use netidx::publisher::Value;
use std::fmt;

/// Default trace budgets (see [`graphix_rt::GXHandle::trace_start`]):
/// total recorded events per trace, and active cycles per segment. A
/// schedule header overrides them per program.
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

/// Fn values normalize to the lambda's printed source (`LambdaDef::src`)
/// so traces of two compiles of one program compare equal; a
/// `LambdaDef`'s own equality is its minted id. An abstract value's
/// payload normalizes too. Iterative because value nesting depth is
/// program-controlled.
// CR claude for claude: [perf/low] normalize rebuilds every Array, Map and Error it
// visits with new allocations, plus a temporary `pairs` Vec per map and unpooled
// `work`/`out` stacks. It does this even when the value holds no `LambdaDef`, which is
// the only thing it rewrites. It runs on every traced event of every session of every
// subject, so a large collection result is copied element by element up to MAX_EVENTS
// times per session. Fix: first scan read-only for an `Abstract` downcasting to
// `LambdaDef` and return `v.clone()` (a refcount bump) when there is none, and rebuild
// only the spine leading to one. Use pooled stacks for the walk. (fuzz-main-aux-12)
fn normalize(v: &Value) -> Value {
    enum Task<'a> {
        Visit(&'a Value),
        BuildArray(usize),
        BuildError,
        BuildMap(usize),
        BuildAbstract(&'a abstract_value::GxAbstract),
    }
    let mut work: Vec<Task> = vec![Task::Visit(v)];
    let mut out: Vec<Value> = Vec::new();
    while let Some(t) = work.pop() {
        match t {
            Task::Visit(Value::Array(a)) => {
                work.push(Task::BuildArray(a.len()));
                for e in a.iter().rev() {
                    work.push(Task::Visit(e));
                }
            }
            Task::Visit(Value::Error(e)) => {
                work.push(Task::BuildError);
                work.push(Task::Visit(e));
            }
            Task::Visit(v) if let Some(a) = abstract_value::get(v) => {
                work.push(Task::BuildAbstract(a));
                work.push(Task::Visit(&a.payload));
            }
            Task::Visit(Value::Map(m)) => {
                work.push(Task::BuildMap(m.len()));
                let pairs: Vec<(&Value, &Value)> = m.into_iter().collect();
                for (k, v) in pairs.into_iter().rev() {
                    work.push(Task::Visit(v));
                    work.push(Task::Visit(k));
                }
            }
            Task::Visit(v) => {
                out.push(match v.downcast_ref::<LambdaDef<GXRt<NoExt>, NoUserEvent>>() {
                    Some(def) => match &def.origin {
                        DefOrigin::Source { spec, .. } => {
                            Value::String(ArcStr::from(spec.to_string()))
                        }
                        DefOrigin::Runtime => Value::String(literal!("<runtime>")),
                    },
                    None => v.clone(),
                })
            }
            Task::BuildArray(n) => {
                let arr: Value = Value::Array(out.drain(out.len() - n..).collect());
                out.push(arr);
            }
            Task::BuildError => {
                let e = out.pop().expect("normalize: error payload missing");
                out.push(Value::Error(e.into()));
            }
            Task::BuildAbstract(a) => {
                let p = out.pop().expect("normalize: abstract payload missing");
                out.push(abstract_value::wrap(a.id, a.name.clone(), a.params.clone(), p));
            }
            Task::BuildMap(n) => {
                let mut pairs: Vec<(Value, Value)> = Vec::with_capacity(n);
                let mut it = out.drain(out.len() - n * 2..);
                while let (Some(k), Some(v)) = (it.next(), it.next()) {
                    pairs.push((k, v));
                }
                drop(it);
                out.push(Value::Map(pairs.into_iter().collect()));
            }
        }
    }
    out.pop().expect("normalize: empty result stack")
}

/// Equality as a program can observe it: `Value`'s own, except that a
/// float compares by its bits (NaN canonical), so `-0.0` is not `0.0`,
/// and a decimal by mantissa and scale. Iterative: value nesting depth is
/// program-controlled.
pub fn same(a: &Value, b: &Value) -> bool {
    let mut work = vec![(a, b)];
    while let Some((a, b)) = work.pop() {
        let equal = match (a, b) {
            (Value::F64(x), Value::F64(y)) => {
                x.is_nan() && y.is_nan() || x.to_bits() == y.to_bits()
            }
            (Value::F32(x), Value::F32(y)) => {
                x.is_nan() && y.is_nan() || x.to_bits() == y.to_bits()
            }
            (Value::Decimal(x), Value::Decimal(y)) => {
                x.mantissa() == y.mantissa() && x.scale() == y.scale()
            }
            (Value::Array(x), Value::Array(y)) => {
                work.extend(x.iter().zip(y.iter()));
                x.len() == y.len()
            }
            (Value::Map(x), Value::Map(y)) => {
                for ((xk, xv), (yk, yv)) in x.into_iter().zip(y.into_iter()) {
                    work.push((xk, yk));
                    work.push((xv, yv));
                }
                x.len() == y.len()
            }
            (Value::Error(x), Value::Error(y)) => {
                work.push((&**x, &**y));
                true
            }
            (a, b)
                if let (Some(x), Some(y)) =
                    (abstract_value::get(a), abstract_value::get(b)) =>
            {
                work.push((&x.payload, &y.payload));
                x.id == y.id && x.params == y.params
            }
            (a, b) => a == b,
        };
        if !equal {
            return false;
        }
    }
    true
}

impl Epoch {
    /// The cycle a segment's offsets count from: its first event's.
    pub fn anchor(seg: &TraceSegment) -> Option<u64> {
        match seg.events.first()? {
            TraceEvent::Compiled { cycle, .. } | TraceEvent::Updated { cycle, .. } => {
                Some(*cycle)
            }
        }
    }

    /// Project a runtime segment onto the watched expr: anchor at the
    /// segment's first event, keep `Updated(eid)` values (fn values
    /// normalized to their source — see [`normalize`]).
    pub fn from_segment(seg: &TraceSegment, eid: ExprId) -> Self {
        let anchor = Self::anchor(seg).unwrap_or(0);
        let events = seg
            .events
            .iter()
            .filter_map(|e| match e {
                TraceEvent::Updated { cycle, id, value } if *id == eid => {
                    Some(((*cycle - anchor) as u32, normalize(value)))
                }
                TraceEvent::Updated { .. } | TraceEvent::Compiled { .. } => None,
            })
            .collect();
        Epoch { events, capped: seg.capped_cycles || seg.capped_events }
    }
}

/// A captured print-family call: its epoch, its cycle's offset from the
/// epoch's anchor (the first event, or in an epoch without one its
/// first printing cycle) and the text it wrote.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Printed {
    pub epoch: u32,
    pub offset: i64,
    pub text: String,
}

impl fmt::Display for Printed {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}+{}:{}", self.epoch, self.offset, self.text.escape_debug())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Trace {
    pub epochs: Vec<Epoch>,
    /// The run's captured print-family calls, Exact tier only, sorted:
    /// within one cycle the order is the evaluation's (forked branches
    /// print as they run), so a cycle's calls are a multiset.
    pub stdout: Vec<Printed>,
    /// The code the program asked `sys::exit` to end with.
    pub exit: Option<i32>,
}

impl Trace {
    pub fn from_segments(segs: &[TraceSegment], eid: ExprId) -> Self {
        Trace {
            epochs: segs.iter().map(|s| Epoch::from_segment(s, eid)).collect(),
            stdout: Vec::new(),
            exit: None,
        }
    }

    /// Each reference a value of `typ` holds, as the order the trace
    /// first shows it in (`&ref0`, `&ref1`, ..): a reference's value is
    /// its session's bind id, which differs between runs, while which
    /// values alias does not.
    pub fn canonicalize_refs(&mut self, env: &Env, typ: &graphix_compiler::typ::Type) {
        if !typ.holds_ref(env) {
            return;
        }
        let mut seen: Vec<Value> = Vec::new();
        for (_, v) in self.epochs.iter_mut().flat_map(|e| e.events.iter_mut()) {
            *v = typ.map_refs(env, v, &mut |r| {
                let i = seen.iter().position(|s| s == r).unwrap_or_else(|| {
                    seen.push(r.clone());
                    seen.len() - 1
                });
                Value::String(ArcStr::from(format!("&ref{i}")))
            });
        }
    }

    /// Equality as a program could tell: every epoch's cap, offsets and
    /// values ([`same`]), and the output.
    pub fn agrees_with(&self, other: &Trace) -> bool {
        self.epochs.len() == other.epochs.len()
            && self.epochs.iter().zip(&other.epochs).all(|(a, b)| {
                a.capped == b.capped
                    && a.events.len() == b.events.len()
                    && a.events
                        .iter()
                        .zip(&b.events)
                        .all(|((ao, av), (bo, bv))| ao == bo && same(av, bv))
            })
            && self.stdout == other.stdout
            && self.exit == other.exit
    }

    /// Per-epoch FINAL value: the last event's value, `None` for a
    /// quiet epoch.
    pub fn final_values(&self) -> Vec<Option<&Value>> {
        self.epochs.iter().map(|e| e.events.last().map(|(_, v)| v)).collect()
    }

    /// Agreement for value-deterministic async programs
    /// (`OracleTier::FinalValues`): per-epoch settled values must agree;
    /// intra-epoch pacing may differ. Any cap on either side falls back
    /// to exact agreement, since a capped epoch never settled.
    pub fn agrees_final(&self, other: &Trace) -> bool {
        if self.epochs.iter().chain(other.epochs.iter()).any(|e| e.capped) {
            return self.agrees_with(other);
        }
        self.epochs.len() == other.epochs.len()
            && self.final_values().iter().zip(other.final_values()).all(|(a, b)| {
                match (a, b) {
                    (Some(a), Some(b)) => same(a, b),
                    (a, b) => a.is_none() && b.is_none(),
                }
            })
            && self.exit == other.exit
            && self.prints() == other.prints()
    }

    /// Each epoch's printed texts, as a multiset: offsets are not
    /// contractual where values compare final.
    fn prints(&self) -> Vec<(u32, &str)> {
        let mut p: Vec<(u32, &str)> =
            self.stdout.iter().map(|l| (l.epoch, l.text.as_str())).collect();
        p.sort();
        p
    }

    /// [`Self::first_difference`] at final strength: the bucket key for
    /// final-tier divergences is the epoch index of the first final
    /// mismatch, since the exact `TraceDiff` is pacing-sensitive.
    pub fn first_final_difference(&self, other: &Trace) -> Option<TraceDiff> {
        if self.epochs.iter().chain(other.epochs.iter()).any(|e| e.capped) {
            return self.first_difference(other);
        }
        if self.epochs.len() != other.epochs.len() {
            return Some(TraceDiff::EpochCount);
        }
        self.final_values()
            .iter()
            .zip(other.final_values().iter())
            .position(|(a, b)| match (a, b) {
                (Some(a), Some(b)) => !same(a, b),
                (a, b) => a.is_some() || b.is_some(),
            })
            .map(TraceDiff::FinalValue)
            .or((self.prints() != other.prints()).then_some(TraceDiff::Stdout))
            .or((self.exit != other.exit).then_some(TraceDiff::Exit))
    }

    /// Classify the first difference, `self` = the node-walk reference,
    /// `other` = the JIT under test. `None` when the traces agree.
    pub fn first_difference(&self, other: &Trace) -> Option<TraceDiff> {
        for (a, b) in self.epochs.iter().zip(other.epochs.iter()) {
            if a.capped != b.capped {
                return Some(TraceDiff::CapMismatch);
            }
            for ((ao, av), (bo, bv)) in a.events.iter().zip(b.events.iter()) {
                if !same(av, bv) {
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
        if self.stdout != other.stdout {
            return Some(TraceDiff::Stdout);
        }
        if self.exit != other.exit {
            return Some(TraceDiff::Exit);
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
    /// Final-tier only: the epoch whose SETTLED value differs (see
    /// [`Trace::first_final_difference`]).
    FinalValue(usize),
    /// Values and pacing agree but the captured print-family output
    /// differs.
    Stdout,
    /// Everything else agrees but the `sys::exit` codes differ.
    Exit,
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
            stdout: Vec::new(),
            exit: None,
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

    #[test]
    fn final_strength() {
        // Same settled value per epoch — pacing and intermediates
        // differ, finals agree.
        let a = tr(vec![vec![(0, 1), (1, 2), (2, 7)], vec![(0, 9)]]);
        let b = tr(vec![vec![(0, 2), (3, 7)], vec![(1, 9)]]);
        assert!(a.agrees_final(&b));
        assert!(!a.agrees_with(&b));
        assert_eq!(a.first_final_difference(&b), None);
        // A different settled value in epoch 1.
        let c = tr(vec![vec![(0, 7)], vec![(0, 8)]]);
        assert!(!a.agrees_final(&c));
        assert_eq!(a.first_final_difference(&c), Some(TraceDiff::FinalValue(1)));
        // Quiet-vs-fired epochs disagree at final strength.
        let d = tr(vec![vec![(0, 7)], vec![]]);
        assert!(!a.agrees_final(&d));
        assert_eq!(a.first_final_difference(&d), Some(TraceDiff::FinalValue(1)));
        // Epoch count still matters.
        let e = tr(vec![vec![(0, 7)]]);
        assert_eq!(a.first_final_difference(&e), Some(TraceDiff::EpochCount));
        // Any cap on either side falls back to EXACT agreement.
        let mut f = b.clone();
        f.epochs[0].capped = true;
        assert!(!a.agrees_final(&f));
        assert_eq!(a.first_final_difference(&f), a.first_difference(&f));
    }
}
