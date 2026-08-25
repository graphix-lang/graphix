use std::{
    cell::Cell,
    sync::{
        LazyLock,
        atomic::{AtomicUsize, Ordering},
    },
};

/// Stack headroom that must remain before [`ensure_sufficient`]
/// switches to a fresh segment. It has to exceed what ONE recursion
/// level can consume between two checks — an unoptimized `expr` parse
/// cycle burns ~420KB (the `choice` tuples in `expr` and `arith_term`
/// are ~112KB and ~125KB of stack frame each), a node-walk lambda
/// dispatch ~10KB.
pub(crate) const RED_ZONE: usize = 1024 * 1024;

/// Size of each fresh segment. Segments are mmap'd on entry and
/// released on exit, so this bounds how often a deep recursion pays
/// for one, not how much memory it holds.
pub(crate) const SEGMENT: usize = 32 * 1024 * 1024;

/// The stack a thread may hold on grown segments before the running
/// derivation is ABORTED (`Control::abort`, the sticky shutdown Ctrl-C
/// arms). Depth is bounded by memory (`design/recursive_activations.md`
/// §4b) and this is the embedder's word on how much of it: containment
/// outside the language, like the interrupt — no program can observe
/// it short of being stopped. Unlimited by default; `GRAPHIX_STACK_BUDGET`
/// (bytes) or [`set_stack_budget`] set it — the fuzz pool gives every
/// child one so a runaway recursion cannot take a soak box down.
static STACK_BUDGET: LazyLock<AtomicUsize> = LazyLock::new(|| {
    AtomicUsize::new(match std::env::var("GRAPHIX_STACK_BUDGET") {
        Ok(s) => s.trim().parse().unwrap_or(usize::MAX),
        Err(_) => usize::MAX,
    })
});

pub fn set_stack_budget(bytes: usize) {
    STACK_BUDGET.store(bytes, Ordering::Relaxed);
}

/// Abort the running runtime because a recursion exceeded the budget —
/// the one exit for both engines (the node-walk's [`grow`], the
/// kernel's `graphix_stack_check`), so the log line and the
/// `CtlFlag::Budget` mark are the same whichever descended.
pub(crate) fn budget_abort() {
    log::error!(
        "stack budget ({} bytes) exceeded by a recursion — aborting the runtime \
         (raise via GRAPHIX_STACK_BUDGET or graphix_compiler::set_stack_budget)",
        STACK_BUDGET.load(Ordering::Relaxed)
    );
    crate::fusion::emit_helpers::abort_current_control_budget();
}

thread_local! {
    /// Bytes of grown segments currently live on this thread.
    static GROWN: Cell<usize> = const { Cell::new(0) };
}

/// Run `f` with a guarantee of [`RED_ZONE`] stack, moving onto a fresh
/// heap segment when the current stack is nearly exhausted.
///
/// Wrap the recursion knots a user program can drive arbitrarily deep:
/// how deeply a program may nest is then bounded by memory rather than
/// by whichever thread the work lands on (libtest gives 2MB, tokio
/// workers 2MB, the main thread 8MB), and it stops depending on the
/// build profile — unoptimized frames are ~6x the optimized ones, so a
/// 2MB thread parses 5 levels of nesting at opt-level 0 against 26 at
/// opt-level "z".
#[inline(always)]
pub(crate) fn ensure_sufficient<R>(f: impl FnOnce() -> R) -> R {
    if stacker::remaining_stack().unwrap_or(0) >= RED_ZONE { f() } else { grow(f) }
}

/// Whether one more segment would put this thread over the budget.
pub(crate) fn grow_exceeds_budget() -> bool {
    GROWN.with(|g| g.get() + SEGMENT > STACK_BUDGET.load(Ordering::Relaxed))
}

/// Run `f` on a fresh segment. Over budget, the current runtime is
/// aborted first; the segment is still granted so the node-walk can
/// unwind at its next interrupt poll instead of overflowing here.
pub(crate) fn grow<R>(f: impl FnOnce() -> R) -> R {
    if grow_exceeds_budget() {
        budget_abort();
    }
    GROWN.with(|g| g.set(g.get() + SEGMENT));
    let r = stacker::grow(SEGMENT, f);
    GROWN.with(|g| g.set(g.get() - SEGMENT));
    r
}
