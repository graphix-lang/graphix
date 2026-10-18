//! Walk pruning (`design/walk_pruning.md`): the boundaries a cycle can
//! skip, and the readers a delivery marks.
use crate::expr::ExprId;
use std::{
    fmt,
    hash::{Hash, Hasher},
    sync::atomic::{AtomicU32, AtomicU64, Ordering::Relaxed},
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
    /// Holds the boundary dirty in every cycle while above zero: a node
    /// inside it that must run again although nothing it reads arrives.
    sticky: AtomicU32,
}

#[derive(Clone)]
pub struct BoundaryRef(Arc<Boundary>);

impl BoundaryRef {
    /// A boundary inside `parent`, never marked yet.
    pub fn new(parent: Option<BoundaryRef>) -> Self {
        Self(Arc::new(Boundary {
            parent,
            stamp: AtomicU64::new(u64::MAX),
            sticky: AtomicU32::new(0),
        }))
    }

    pub fn parent(&self) -> Option<&BoundaryRef> {
        self.0.parent.as_ref()
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

    /// Whether the boundary was marked in `cycle` or is held sticky.
    pub fn marked(&self, cycle: u64) -> bool {
        self.0.stamp.load(Relaxed) == cycle || self.0.sticky.load(Relaxed) > 0
    }

    /// Hold the boundary dirty until a matching [`Self::release`].
    pub fn hold(&self) {
        self.0.sticky.fetch_add(1, Relaxed);
    }

    pub fn release(&self) {
        self.0.sticky.fetch_sub(1, Relaxed);
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
