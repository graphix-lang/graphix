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
    /// The last cycle the boundary was marked in.
    stamp: AtomicU64,
    /// The last cycle a walk reached the boundary, skipped or not.
    visited: AtomicU64,
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
        Self(Arc::new(Boundary {
            parent,
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

    /// Record that a walk reached the boundary in `cycle`.
    pub fn visit(&self, cycle: u64) {
        self.0.visited.store(cycle, Relaxed)
    }

    /// Whether a walk reached the boundary in `cycle`.
    pub fn visited(&self, cycle: u64) -> bool {
        self.0.visited.load(Relaxed) == cycle
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
