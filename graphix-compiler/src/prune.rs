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
    /// An ancestor further up: the skew-binary jump pointers of Myers'
    /// random-access lists, so a climb to any ancestor takes logarithmic
    /// steps.
    jump: Option<BoundaryRef>,
    /// How many boundaries are around this one.
    depth: u32,
    /// The last cycle the boundary was marked in.
    stamp: AtomicU64,
    /// The last cycle a walk reached the boundary, skipped or not. A walk
    /// reaches a boundary only inside its parent, so the boundaries a
    /// cycle visited are closed upward.
    visited: AtomicU64,
}

/// A chain of parents is as long as a recursion is deep, so the last owner
/// unlinks it in a loop rather than dropping it recursively. A jump names
/// an ancestor its parent chain holds too, so dropping it first never drops
/// the ancestor.
impl Drop for Boundary {
    fn drop(&mut self) {
        drop(self.jump.take());
        let mut next = self.parent.take();
        while let Some(b) = next {
            next = match Arc::try_unwrap(b.0) {
                Ok(mut inner) => {
                    drop(inner.jump.take());
                    inner.parent.take()
                }
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
        let (depth, jump) = match &parent {
            None => (0, None),
            Some(p) => {
                let jump = match &p.0.jump {
                    Some(j)
                        if let Some(jj) = &j.0.jump
                            && p.0.depth - j.0.depth == j.0.depth - jj.0.depth =>
                    {
                        jj.clone()
                    }
                    _ => p.clone(),
                };
                (p.0.depth + 1, Some(jump))
            }
        };
        Self(Arc::new(Boundary {
            parent,
            jump,
            depth,
            stamp: AtomicU64::new(u64::MAX),
            visited: AtomicU64::new(u64::MAX),
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

    /// Record that a walk reached the boundary in `cycle`. A walk reaches a
    /// boundary only inside its parent, so a cycle's visits are closed
    /// upward, which `Reader::passed_by`'s jumps rely on.
    pub fn visit(&self, cycle: u64) {
        debug_assert!(
            self.0.parent.as_ref().is_none_or(|p| p.visited(cycle)),
            "a boundary visited outside its parent"
        );
        self.0.visited.store(cycle, Relaxed)
    }

    /// Whether a walk reached the boundary in `cycle`.
    pub fn visited(&self, cycle: u64) -> bool {
        self.0.visited.load(Relaxed) == cycle
    }

    /// The innermost of this boundary and its ancestors a walk reached in
    /// `cycle`.
    fn innermost_visited(&self, cycle: u64) -> Option<&BoundaryRef> {
        let mut b = self;
        loop {
            step();
            if b.visited(cycle) {
                return Some(b);
            }
            b = match &b.0.jump {
                Some(j) if !j.visited(cycle) => j,
                _ => b.0.parent.as_ref()?,
            };
        }
    }

    /// Whether this boundary is `inner` or one of its ancestors.
    pub(crate) fn holds(&self, inner: Option<&BoundaryRef>) -> bool {
        let mut b = inner;
        while let Some(x) = b
            && x.0.depth > self.0.depth
        {
            step();
            b = match &x.0.jump {
                Some(j) if j.0.depth >= self.0.depth => Some(j),
                _ => x.0.parent.as_ref(),
            };
        }
        b == Some(self)
    }
}

#[cfg(test)]
thread_local! {
    static STEPS: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
}

#[inline(always)]
fn step() {
    #[cfg(test)]
    STEPS.with(|s| s.set(s.get() + 1))
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

/// The last cycle a reader read its binding: a reader whose binding may
/// fire later in the walk of its own boundary keeps one, which orders it
/// against that fire where the boundaries cannot.
#[derive(Clone)]
pub struct ReadStamp(Arc<AtomicU64>);

impl Default for ReadStamp {
    fn default() -> Self {
        Self(Arc::new(AtomicU64::new(u64::MAX)))
    }
}

impl ReadStamp {
    /// Record a read in `cycle`.
    pub fn read(&self, cycle: u64) {
        self.0.store(cycle, Relaxed)
    }

    /// Whether the reader read in `cycle`.
    pub fn read_in(&self, cycle: u64) -> bool {
        self.0.load(Relaxed) == cycle
    }
}

impl PartialEq for ReadStamp {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.0, &other.0)
    }
}

impl Eq for ReadStamp {}

impl Hash for ReadStamp {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Arc::as_ptr(&self.0).hash(state)
    }
}

impl fmt::Debug for ReadStamp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "ReadStamp({:p})", Arc::as_ptr(&self.0))
    }
}

/// Who reads a variable: the top-level expression the runtime schedules,
/// the innermost boundary around the reader, if any, and the reader's own
/// read stamp, if it keeps one.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Reader {
    pub root: ExprId,
    pub boundary: Option<BoundaryRef>,
    pub read: Option<ReadStamp>,
}

impl Reader {
    /// Mark the reader's boundary in `cycle`.
    pub fn mark(&self, cycle: u64) {
        if let Some(b) = &self.boundary {
            b.mark(cycle)
        }
    }

    // XCR claude for claude: [bug/medium] Order is judged by boundary only: a reader in a
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
    // 2026-11-05 claude: A Deref keeps a read stamp (prune::ReadStamp, carried in its Reader) set as
    // it reads; Reader::passed_by counts a reader that read this cycle as passed, so a *r before a
    // nested block or call of its own statement that fires its target reads the fire next cycle, as
    // the split statement does, and late_fire matches the stamp so a Deref after the producer takes
    // no late copy. Pins that fail without it:
    // lang::delivery::a_fire_reaches_a_passed_dereference_in_its_statement (all four modes); unit
    // prune::tests::a_reader_in_the_walks_boundary_is_passed_once_it_read. Docs: CLAUDE.md,
    // design/dense_delivery.md, book core/connect.md, graphix-lang skill. Needs a soak.
    /// Whether a walk now inside `at` in `cycle` has passed the reader: it
    /// read in `cycle`, or the walk reached its boundary, or one around it,
    /// and left it, the verdict that of the innermost of them the walk
    /// reached.
    pub fn passed_by(&self, at: Option<&BoundaryRef>, cycle: u64) -> bool {
        self.read.as_ref().is_some_and(|r| r.read_in(cycle))
            || self
                .boundary
                .as_ref()
                .and_then(|b| b.innermost_visited(cycle))
                .is_some_and(|x| !x.holds(at))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn reader(boundary: &BoundaryRef) -> Reader {
        Reader { root: ExprId::new(), boundary: Some(boundary.clone()), read: None }
    }

    /// Whether a walk inside `at` passed `r`, climbing one boundary a step.
    fn passed_linearly(r: &Reader, at: Option<&BoundaryRef>, cycle: u64) -> bool {
        let mut b = r.boundary.as_ref();
        while let Some(x) = b {
            if x.visited(cycle) {
                let mut w = at;
                while let Some(y) = w {
                    if y == x {
                        return false;
                    }
                    w = y.0.parent.as_ref();
                }
                return true;
            }
            b = x.0.parent.as_ref();
        }
        false
    }

    /// The steps `f` climbs.
    fn steps(f: impl FnOnce()) -> usize {
        let before = STEPS.with(|s| s.get());
        f();
        STEPS.with(|s| s.get()) - before
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

    /// The steps one judgment may climb at depth `n`.
    fn bound(n: usize) -> usize {
        8 * (usize::BITS - n.leading_zeros()) as usize + 8
    }

    /// A reader in the boundary the walk is inside is passed once it read
    /// in the cycle, and ahead before.
    #[test]
    fn a_reader_in_the_walks_boundary_is_passed_once_it_read() {
        let cycle = 7;
        let stmt = BoundaryRef::new(None);
        stmt.visit(cycle);
        let block = BoundaryRef::new(Some(stmt.clone()));
        block.visit(cycle);
        let read = ReadStamp::default();
        let r = Reader {
            root: ExprId::new(),
            boundary: Some(stmt),
            read: Some(read.clone()),
        };
        assert!(!r.passed_by(Some(&block), cycle));
        read.read(cycle - 1);
        assert!(!r.passed_by(Some(&block), cycle));
        read.read(cycle);
        assert!(r.passed_by(Some(&block), cycle));
    }

    /// A recursion `n` deep whose level `k` fires once while the walk is
    /// inside it, read at the far end of a chain hanging below the last
    /// level, which the walk has not reached.
    #[test]
    fn a_fire_a_level_read_deep_ahead_costs_the_log_of_the_depth() {
        let (cycle, n) = (7, 2000);
        let levels = chain(None, n);
        let below = chain(levels.last().cloned(), n);
        let mut climbed = 0;
        for k in 0..n {
            levels[k].visit(cycle);
            let r = reader(&below[n - 1 - k]);
            climbed += steps(|| assert!(!r.passed_by(Some(&levels[k]), cycle)));
        }
        assert!(climbed <= n * bound(2 * n), "climbed {climbed}");
    }

    /// A recursion `n` deep whose level `k` fires once while the walk is
    /// inside it, read in a statement of the outermost body: one the walk
    /// has not reached (ahead), and one it reached and left (passed).
    #[test]
    fn a_fire_a_level_read_at_the_top_costs_the_log_of_the_depth() {
        let (cycle, n) = (7, 2000);
        let body = BoundaryRef::new(None);
        body.visit(cycle);
        let before = BoundaryRef::new(Some(body.clone()));
        before.visit(cycle);
        let after = BoundaryRef::new(Some(body.clone()));
        let levels = chain(Some(body.clone()), n);
        let mut climbed = 0;
        for k in 0..n {
            levels[k].visit(cycle);
            let at = Some(&levels[k]);
            climbed += steps(|| {
                assert!(!reader(&after).passed_by(at, cycle));
                assert!(reader(&before).passed_by(at, cycle));
            });
        }
        assert!(climbed <= 2 * n * bound(n + 1), "climbed {climbed}");
    }

    /// Every verdict is the one climbing a boundary a step gives, over
    /// trees of random shape, each cycle's visits closed upward as a
    /// walk's are.
    #[test]
    fn verdicts_are_those_of_climbing_a_step_at_a_time() {
        let mut seed: u64 = 0x2545_f491_4f6c_dd1d;
        let mut rand = move |n: usize| {
            seed =
                seed.wrapping_mul(6364136223846793005).wrapping_add(1442695040888963407);
            (seed >> 33) as usize % n
        };
        for cycle in 1..300 {
            let mut all: Vec<BoundaryRef> = Vec::new();
            for _ in 0..1 + rand(80) {
                let parent = match rand(all.len() + 1) {
                    0 => None,
                    i => Some(all[i - 1].clone()),
                };
                let b = BoundaryRef::new(parent);
                if b.0.parent.as_ref().is_none_or(|p| p.visited(cycle)) && rand(3) != 0 {
                    b.visit(cycle)
                }
                all.push(b)
            }
            let visited: Vec<&BoundaryRef> =
                all.iter().filter(|b| b.visited(cycle)).collect();
            let at = match rand(visited.len() + 1) {
                0 => None,
                i => Some(visited[i - 1].clone()),
            };
            for _ in 0..all.len() {
                let r = reader(&all[rand(all.len())]);
                assert_eq!(
                    r.passed_by(at.as_ref(), cycle),
                    passed_linearly(&r, at.as_ref(), cycle)
                );
            }
        }
    }
}
