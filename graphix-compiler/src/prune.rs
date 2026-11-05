//! Walk pruning (`design/walk_pruning.md`): the boundaries a cycle can
//! skip, and the readers a delivery marks.
use crate::expr::ExprId;
use std::{
    fmt,
    hash::{Hash, Hasher},
    sync::atomic::{AtomicU64, Ordering::Relaxed},
};
use triomphe::Arc;

/// A subtree a cycle may skip: a statement, an instance body or a select
/// arm. Marked in a cycle when something its subtree reads is delivered;
/// a mark reaches every enclosing boundary, so a clean boundary has no
/// dirty boundary inside it.
pub struct Boundary {
    parent: Option<BoundaryRef>,
    /// How many boundaries are around this one.
    depth: u32,
    /// The last cycle the boundary was marked in.
    stamp: AtomicU64,
    /// The last cycle a walk reached the boundary, skipped or not.
    visited: AtomicU64,
    /// The last fire that judged the boundary's readers ([`Passed`]), shifted
    /// left one, or'd with whether that fire's walk had passed them.
    judged: AtomicU64,
}

/// A chain of parents is as long as a recursion is deep, so the last owner
/// unlinks it in a loop rather than dropping it recursively.
impl Drop for Boundary {
    fn drop(&mut self) {
        let mut next = self.parent.take();
        while let Some(b) = next {
            next = match Arc::try_unwrap(b.0) {
                Ok(mut inner) => inner.parent.take(),
                Err(_) => None,
            };
        }
    }
}

#[derive(Clone)]
pub struct BoundaryRef(Arc<Boundary>);

impl BoundaryRef {
    /// A boundary inside `parent`, never marked yet.
    pub fn new(parent: Option<BoundaryRef>) -> Self {
        let depth = parent.as_ref().map_or(0, |p| p.0.depth + 1);
        Self(Arc::new(Boundary {
            parent,
            depth,
            stamp: AtomicU64::new(u64::MAX),
            visited: AtomicU64::new(u64::MAX),
            judged: AtomicU64::new(0),
        }))
    }

    /// Mark this boundary and its ancestors in `cycle`, up to the first one
    /// already marked.
    pub fn mark(&self, cycle: u64) {
        let mut b = self;
        loop {
            if b.0.stamp.load(Relaxed) == cycle {
                break;
            }
            b.0.stamp.store(cycle, Relaxed);
            match &b.0.parent {
                Some(p) => b = p,
                None => break,
            }
        }
    }

    /// Whether the boundary was marked in `cycle`.
    pub fn marked(&self, cycle: u64) -> bool {
        self.0.stamp.load(Relaxed) == cycle
    }

    /// Record that a walk reached the boundary in `cycle`.
    pub fn visit(&self, cycle: u64) {
        self.0.visited.store(cycle, Relaxed)
    }

    /// Whether a walk reached the boundary in `cycle`.
    pub fn visited(&self, cycle: u64) -> bool {
        self.0.visited.load(Relaxed) == cycle
    }

    fn parent(&self) -> Option<&BoundaryRef> {
        self.0.parent.as_ref()
    }

    /// The verdict `fire` gave this boundary's readers, if it judged them.
    fn judged(&self, fire: u64) -> Option<bool> {
        let j = self.0.judged.load(Relaxed);
        (j >> 1 == fire).then_some(j & 1 == 1)
    }

    fn judge(&self, fire: u64, passed: bool) {
        self.0.judged.store(fire << 1 | passed as u64, Relaxed)
    }
}

impl PartialEq for BoundaryRef {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.0, &other.0)
    }
}

impl Eq for BoundaryRef {}

impl Hash for BoundaryRef {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Arc::as_ptr(&self.0).hash(state)
    }
}

impl fmt::Debug for BoundaryRef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Boundary({:p})", Arc::as_ptr(&self.0))
    }
}

/// Who reads a variable: the top-level expression the runtime schedules,
/// and the innermost boundary around the reader, if any.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Reader {
    pub root: ExprId,
    pub boundary: Option<BoundaryRef>,
}

impl Reader {
    /// Mark the reader's boundary in `cycle`.
    pub fn mark(&self, cycle: u64) {
        if let Some(b) = &self.boundary {
            b.mark(cycle)
        }
    }
}

static FIRES: AtomicU64 = AtomicU64::new(1);

// CR claude for claude: [bug/medium] Order is judged by boundary only: a reader in a
// boundary the walk is still inside counts as ahead, though the walk may have passed it
// there, so it never gets the fire. A Deref directly in a statement, before a nested block
// statement that fires its target: `{let rr: [&i64, null] = null; let p = (*(rr$), { let
// inner = array::iter([i64:1, i64:2, i64:3, i64:4]); rr <- &inner; inner }); p}` ends (3,
// 4), the same program as two statements (4, 4); a callee's let in the same statement
// alike (s3_same_stmt_callee 40, 50 against the split's 50, 50). Main behaves the same (it
// has no late path); the docs state the limit. Record the read order inside a boundary (a
// Deref's read stamp compared at the fire) or keep the limit. probe:
// ~/ptmp/cr29/scratch/review-r16/W-r1-1/{fz_s1,fz_s4,s3_same_stmt_callee,s4_split_stmt_callee}.gx
// (late-same-boundary-01)

/// One fire's judgment of whether its walk, now inside `at`, has passed
/// each reader: reached the reader's boundary, or one around it, and left
/// it. A reader's verdict is that of its innermost boundary the walk
/// reached this cycle, and each boundary is judged once per fire.
pub struct Passed<'a> {
    /// The fire's token, drawn at its second reader: a lone reader's
    /// judgment is not kept. Zero until then.
    fire: u64,
    /// Whether a reader was judged.
    met: bool,
    cycle: u64,
    at: Option<&'a BoundaryRef>,
    /// The innermost boundary around the walk not yet judged; every one
    /// inside it is. Only a walk no other branch runs beside trusts it:
    /// a concurrent branch's fire may overwrite a judgment.
    walk: Option<&'a BoundaryRef>,
    concurrent: bool,
    #[cfg(test)]
    climbed: usize,
}

impl<'a> Passed<'a> {
    pub fn new(at: Option<&'a BoundaryRef>, cycle: u64, concurrent: bool) -> Self {
        Self {
            fire: 0,
            met: false,
            cycle,
            at,
            walk: at,
            concurrent,
            #[cfg(test)]
            climbed: 0,
        }
    }

    pub fn passed(&mut self, r: &Reader) -> bool {
        if !self.met {
            self.met = true
        } else if self.fire == 0 {
            self.fire = FIRES.fetch_add(1, Relaxed)
        }
        let keep = self.fire != 0;
        let mut b = r.boundary.as_ref();
        let (passed, end) = loop {
            let Some(x) = b else { break (false, None) };
            #[cfg(test)]
            {
                self.climbed += 1;
            }
            if keep && let Some(passed) = x.judged(self.fire) {
                break (passed, Some(x));
            }
            if x.visited(self.cycle) {
                break (!self.on_walk(x), x.parent());
            }
            b = x.parent();
        };
        let mut b = r.boundary.as_ref().filter(|_| keep);
        while let Some(x) = b
            && Some(x) != end
        {
            x.judge(self.fire, passed);
            b = x.parent();
        }
        passed
    }

    /// Whether the walk is inside `x`. A serial walk that keeps its
    /// judgments judges its boundaries on the way, so the next call starts
    /// where this one stopped.
    fn on_walk(&mut self, x: &BoundaryRef) -> bool {
        let serial = !self.concurrent && self.fire != 0;
        let mut c = if serial { self.walk } else { self.at };
        while let Some(w) = c
            && w.0.depth >= x.0.depth
        {
            #[cfg(test)]
            {
                self.climbed += 1;
            }
            c = w.parent();
            if serial {
                w.judge(self.fire, false);
                self.walk = c;
            }
            if w == x {
                return true;
            }
        }
        false
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn reader(boundary: &BoundaryRef) -> Reader {
        Reader { root: ExprId::new(), boundary: Some(boundary.clone()) }
    }

    /// Whether a walk inside `at` passed `r`, each boundary judged anew.
    fn passed_by(r: &Reader, at: Option<&BoundaryRef>, cycle: u64) -> bool {
        let mut b = r.boundary.as_ref();
        while let Some(x) = b {
            if x.visited(cycle) {
                let mut w = at;
                while let Some(y) = w {
                    if y == x {
                        return false;
                    }
                    w = y.parent();
                }
                return true;
            }
            b = x.parent();
        }
        false
    }

    /// A chain of `n` boundaries below `top`, innermost last.
    fn chain(top: Option<BoundaryRef>, n: usize) -> Vec<BoundaryRef> {
        let mut v: Vec<BoundaryRef> = Vec::with_capacity(n);
        for _ in 0..n {
            let parent = v.last().cloned().or_else(|| top.clone());
            v.push(BoundaryRef::new(parent))
        }
        v
    }

    #[test]
    fn readers_deep_in_a_chain_ahead_of_the_walk_cost_its_depth() {
        let cycle = 7;
        let at = BoundaryRef::new(None);
        at.visit(cycle);
        let deep = chain(None, 2000);
        let mut judge = Passed::new(Some(&at), cycle, false);
        for b in deep.iter().rev() {
            assert!(!judge.passed(&reader(b)));
        }
        assert!(judge.climbed <= 3 * deep.len(), "climbed {}", judge.climbed);
    }

    #[test]
    fn readers_along_a_deep_walk_cost_its_depth() {
        let cycle = 7;
        let walk = chain(None, 2000);
        for b in &walk {
            b.visit(cycle)
        }
        let at = walk.last();
        let mut judge = Passed::new(at, cycle, false);
        for b in &walk {
            assert!(!judge.passed(&reader(b)));
        }
        assert!(judge.climbed <= 3 * walk.len(), "climbed {}", judge.climbed);
    }

    /// Every verdict, serial or concurrent, is the one judging each
    /// boundary anew gives, over trees of random shape and visits.
    #[test]
    fn verdicts_are_those_of_judging_each_boundary_anew() {
        let mut seed: u64 = 0x2545_f491_4f6c_dd1d;
        let mut rand = move |n: usize| {
            seed =
                seed.wrapping_mul(6364136223846793005).wrapping_add(1442695040888963407);
            (seed >> 33) as usize % n
        };
        let cycle = 3;
        for _ in 0..200 {
            let mut all: Vec<BoundaryRef> = Vec::new();
            for _ in 0..1 + rand(60) {
                let parent = match rand(all.len() + 1) {
                    0 => None,
                    i => Some(all[i - 1].clone()),
                };
                let b = BoundaryRef::new(parent);
                if rand(3) == 0 {
                    b.visit(cycle)
                }
                all.push(b)
            }
            let at = match rand(all.len() + 1) {
                0 => None,
                i => Some(all[i - 1].clone()),
            };
            let mut w = at.as_ref();
            while let Some(b) = w {
                b.visit(cycle);
                w = b.parent();
            }
            for concurrent in [false, true] {
                let mut judge = Passed::new(at.as_ref(), cycle, concurrent);
                for _ in 0..all.len() {
                    let r = reader(&all[rand(all.len())]);
                    assert_eq!(judge.passed(&r), passed_by(&r, at.as_ref(), cycle));
                }
            }
        }
    }
}
