use std::{
    cell::Cell,
    sync::{
        LazyLock,
        atomic::{AtomicUsize, Ordering},
    },
};

/// Stack headroom that must remain before [`ensure_sufficient`]
/// switches to a fresh segment. It must exceed what one recursion
/// level consumes between two checks (~420KB for an unoptimized
/// `expr` parse level).
pub(crate) const RED_ZONE: usize = 1024 * 1024;

/// Size of each fresh segment. Segments are mmap'd on entry and
/// released on exit, so this bounds how often a deep recursion pays
/// for one, not how much memory it holds.
pub(crate) const SEGMENT: usize = 32 * 1024 * 1024;

/// The budget a new runtime's [`crate::Control`] starts with: unlimited,
/// `GRAPHIX_STACK_BUDGET`, or the last [`set_stack_budget`].
static DEFAULT_BUDGET: LazyLock<AtomicUsize> = LazyLock::new(|| {
    AtomicUsize::new(match std::env::var("GRAPHIX_STACK_BUDGET") {
        Err(_) => usize::MAX,
        Ok(s) => parse_budget(&s).unwrap_or_else(|| {
            let msg = compact_str::format_compact!(
                "GRAPHIX_STACK_BUDGET={s:?} is not a byte count (1073741824, \
                 64M, 1G); running without a stack budget"
            );
            log::error!("{msg}");
            eprintln!("{msg}");
            usize::MAX
        }),
    })
});

/// Bytes, optionally scaled by a binary `K`, `M` or `G` (`64M`, `1GiB`).
fn parse_budget(s: &str) -> Option<usize> {
    let s = s.trim();
    let digits = s.find(|c: char| !c.is_ascii_digit()).unwrap_or(s.len());
    let n: usize = s[..digits].parse().ok()?;
    const UNITS: [(&str, u32); 11] = [
        ("", 0),
        ("B", 0),
        ("K", 10),
        ("KB", 10),
        ("KiB", 10),
        ("M", 20),
        ("MB", 20),
        ("MiB", 20),
        ("G", 30),
        ("GB", 30),
        ("GiB", 30),
    ];
    let unit = s[digits..].trim_start();
    let (_, scale) = UNITS.iter().find(|(u, _)| u.eq_ignore_ascii_case(unit))?;
    n.checked_mul(1 << scale)
}

pub(crate) fn default_budget() -> usize {
    DEFAULT_BUDGET.load(Ordering::Relaxed)
}

/// Set the stack budget runtimes created from now on start with; a
/// running one keeps its own ([`crate::Control::set_stack_budget`]).
pub fn set_stack_budget(bytes: usize) {
    DEFAULT_BUDGET.store(bytes, Ordering::Relaxed);
}

/// Abort the running runtime because a recursion exceeded its budget;
/// the one exit for both the node-walk and the kernel stack check.
pub(crate) fn budget_abort() {
    log::error!(
        "stack budget ({} bytes) exceeded by a recursion — aborting the runtime \
         (raise via GRAPHIX_STACK_BUDGET or graphix_compiler::set_stack_budget)",
        crate::fusion::emit_helpers::current_stack_budget()
    );
    crate::fusion::emit_helpers::abort_current_control_budget();
}

thread_local! {
    /// Bytes of grown segments currently live on this thread.
    static GROWN: Cell<usize> = const { Cell::new(0) };
}

/// Run `f` with a guarantee of [`RED_ZONE`] stack, moving onto a fresh
/// heap segment when the current stack is nearly exhausted. Wrap every
/// recursion knot a user program can drive arbitrarily deep.
#[inline(always)]
pub(crate) fn ensure_sufficient<R>(f: impl FnOnce() -> R) -> R {
    if stacker::remaining_stack().unwrap_or(0) >= RED_ZONE { f() } else { grow(f) }
}

/// Whether one more segment would put this thread over the budget of
/// the runtime running on it.
pub(crate) fn grow_exceeds_budget() -> bool {
    let budget = crate::fusion::emit_helpers::current_stack_budget();
    GROWN.with(|g| g.get() + SEGMENT > budget)
}

/// One segment's share of [`GROWN`], returned when the segment is left,
/// by an unwind too.
struct Grown;

impl Grown {
    fn enter() -> Self {
        GROWN.with(|g| g.set(g.get() + SEGMENT));
        Grown
    }
}

impl Drop for Grown {
    fn drop(&mut self) {
        GROWN.with(|g| g.set(g.get() - SEGMENT));
    }
}

/// Run `f` on a fresh segment. Over budget, the current runtime is
/// aborted first; the segment is still granted so the node-walk can
/// unwind at its next interrupt poll instead of overflowing here.
pub(crate) fn grow<R>(f: impl FnOnce() -> R) -> R {
    if grow_exceeds_budget() {
        budget_abort();
    }
    let _grown = Grown::enter();
    stacker::grow(SEGMENT, f)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn budget_units() {
        assert_eq!(parse_budget("1073741824"), Some(1 << 30));
        assert_eq!(parse_budget(" 64M "), Some(64 << 20));
        assert_eq!(parse_budget("64 MB"), Some(64 << 20));
        assert_eq!(parse_budget("1GiB"), Some(1 << 30));
        assert_eq!(parse_budget("512k"), Some(512 << 10));
        assert_eq!(parse_budget("1e8"), None);
        assert_eq!(parse_budget("64 MBs"), None);
        assert_eq!(parse_budget("M"), None);
        assert_eq!(parse_budget("-1"), None);
        assert_eq!(parse_budget(&format!("{}G", usize::MAX)), None);
    }
}
