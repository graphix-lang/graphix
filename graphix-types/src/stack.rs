use enumflags2::bitflags;
use std::{
    cell::Cell,
    sync::{
        LazyLock,
        atomic::{AtomicU8, AtomicU32, AtomicUsize, Ordering},
    },
};

/// Stack headroom that must remain before [`ensure_sufficient`]
/// switches to a fresh segment. It must exceed what one recursion
/// level consumes between two checks (~420KB for an unoptimized
/// `expr` parse level).
#[doc(hidden)]
pub const RED_ZONE: usize = 1024 * 1024;

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
/// running one keeps its own ([`Control::set_stack_budget`]).
pub fn set_stack_budget(bytes: usize) {
    DEFAULT_BUDGET.store(bytes, Ordering::Relaxed);
}

/// Abort the running runtime because a recursion exceeded its budget;
/// the one exit for both the node-walk and the kernel stack check.
#[doc(hidden)]
pub fn budget_abort() {
    log::error!(
        "stack budget ({} bytes) exceeded by a recursion — aborting the runtime \
         (raise via GRAPHIX_STACK_BUDGET or graphix_compiler::set_stack_budget)",
        current_stack_budget()
    );
    abort_current_control_budget();
}

thread_local! {
    /// Bytes of grown segments currently live on this thread.
    static GROWN: Cell<usize> = const { Cell::new(0) };
}

/// Run `f` with a guarantee of [`RED_ZONE`] stack, moving onto a fresh
/// heap segment when the current stack is nearly exhausted. Wrap every
/// recursion knot a user program can drive arbitrarily deep.
#[inline(always)]
#[doc(hidden)]
pub fn ensure_sufficient<R>(f: impl FnOnce() -> R) -> R {
    if stacker::remaining_stack().unwrap_or(0) >= RED_ZONE { f() } else { grow(f) }
}

/// Whether one more segment would put this thread over the budget of
/// the runtime running on it.
#[doc(hidden)]
pub fn grow_exceeds_budget() -> bool {
    let budget = current_stack_budget();
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
#[doc(hidden)]
pub fn grow<R>(f: impl FnOnce() -> R) -> R {
    if grow_exceeds_budget() {
        budget_abort();
    }
    let _grown = Grown::enter();
    stacker::grow(SEGMENT, f)
}

/// Runtime control signals shared between a runtime handle and the
/// running `ExecCtx`. `Interrupt` makes in-flight loops abort to bottom
/// while the runtime keeps going; `Abort` also shuts the runtime down.
/// Polled lock-free via [`Control::interrupted`] and `graphix_interrupted`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[bitflags]
#[repr(u32)]
pub enum CtlFlag {
    Interrupt = 1,
    Abort = 2,
    /// Set beside `Abort` when the stack budget stopped the runtime.
    Budget = 4,
}

/// Lock-free [`CtlFlag`] set. A loop polls [`Control::interrupted`];
/// the run loop polls [`Control::aborted`]. Also this runtime's stack
/// budget: the grown stack a recursion may hold before
/// [`Control::abort_budget`].
#[derive(Debug)]
pub struct Control {
    flags: AtomicU32,
    stack_budget: AtomicUsize,
    par: AtomicU8,
}

/// How a runtime forks the independent subtrees of a cycle
/// (`design/parallel_eval.md`).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum ParMode {
    /// Never.
    Off,
    /// Where the cost model says a fork pays.
    Auto,
    /// At every legal fork point.
    Force,
}

impl ParMode {
    /// `GRAPHIX_PAR` (`off`, `auto` or `force`), else `Off`.
    pub fn from_env() -> Self {
        static MODE: LazyLock<ParMode> =
            LazyLock::new(|| match std::env::var("GRAPHIX_PAR").as_deref() {
                Ok("force") => ParMode::Force,
                Ok("auto") => ParMode::Auto,
                _ => ParMode::Off,
            });
        *MODE
    }
}

impl Default for Control {
    fn default() -> Self {
        Self::new()
    }
}

impl Control {
    pub fn new() -> Self {
        Control {
            flags: AtomicU32::new(0),
            stack_budget: AtomicUsize::new(default_budget()),
            par: AtomicU8::new(ParMode::from_env() as u8),
        }
    }

    pub fn par_mode(&self) -> ParMode {
        match self.par.load(Ordering::Relaxed) {
            1 => ParMode::Auto,
            2 => ParMode::Force,
            _ => ParMode::Off,
        }
    }

    pub fn set_par_mode(&self, mode: ParMode) {
        self.par.store(mode as u8, Ordering::Relaxed)
    }

    /// The bytes of grown stack segments a thread running this runtime
    /// may hold; `usize::MAX` is unlimited.
    pub fn stack_budget(&self) -> usize {
        self.stack_budget.load(Ordering::Relaxed)
    }

    pub fn set_stack_budget(&self, bytes: usize) {
        self.stack_budget.store(bytes, Ordering::Relaxed)
    }

    /// Request that in-flight loops abort this cycle; cleared at the
    /// end of the cycle.
    pub fn interrupt(&self) {
        self.flags.fetch_or(CtlFlag::Interrupt as u32, Ordering::Release);
    }

    /// Request shutdown: in-flight loops abort and the run loop returns
    /// before the next cycle. Sticky.
    pub fn abort(&self) {
        self.flags.fetch_or(CtlFlag::Abort as u32, Ordering::Release);
    }

    /// [`Self::abort`], marked as the stack budget's doing.
    pub fn abort_budget(&self) {
        self.flags
            .fetch_or(CtlFlag::Abort as u32 | CtlFlag::Budget as u32, Ordering::Release);
    }

    /// True if the stack budget aborted this runtime.
    pub fn budget_aborted(&self) -> bool {
        self.flags.load(Ordering::Acquire) & (CtlFlag::Budget as u32) != 0
    }

    /// True if any control flag is set: a loop should abort.
    pub fn interrupted(&self) -> bool {
        self.flags.load(Ordering::Acquire) != 0
    }

    /// True if `Abort` is set.
    pub fn aborted(&self) -> bool {
        self.flags.load(Ordering::Acquire) & (CtlFlag::Abort as u32) != 0
    }

    /// Clear the `Interrupt` bit, leaving `Abort` sticky.
    pub fn clear_interrupt(&self) {
        self.flags.fetch_and(!(CtlFlag::Interrupt as u32), Ordering::Release);
    }
}

thread_local! {
    /// The [`Control`] of the runtime whose cycle is running on this
    /// thread ([`InterruptScope`]); null outside a cycle.
    static CURRENT: Cell<*const Control> = const { Cell::new(std::ptr::null()) };
}

/// Whether the runtime whose cycle this thread is running has an
/// interrupt or an abort pending.
pub fn interrupted() -> bool {
    CURRENT.with(|c| {
        let p = c.get();
        // SAFETY: `p` is the running runtime's `Control`, which outlives
        // the cycle; null when no cycle is running.
        !p.is_null() && unsafe { (*p).interrupted() }
    })
}

/// Points `graphix_interrupted` at a runtime's [`Control`] on
/// this thread while its cycle's nodes run; dropping it restores the
/// enclosing runtime's. Create it on the thread that runs the nodes and
/// drop it there, before the task can migrate, while `control` lives.
pub struct InterruptScope {
    prev: *const Control,
}

impl InterruptScope {
    pub fn new(control: &Control) -> Self {
        let prev = CURRENT.with(|c| c.replace(control as *const Control));
        Self { prev }
    }
}

impl Drop for InterruptScope {
    fn drop(&mut self) {
        CURRENT.with(|c| c.set(self.prev));
    }
}

/// The stack budget of the runtime whose cycle this thread is running;
/// the default budget outside a cycle.
pub(crate) fn current_stack_budget() -> usize {
    CURRENT.with(|c| {
        let p = c.get();
        // SAFETY: see `graphix_interrupted`.
        if p.is_null() { default_budget() } else { unsafe { (*p).stack_budget() } }
    })
}

/// Abort the runtime this thread is running under (the stack budget's
/// containment); a no-op with no runtime on this thread.
pub(crate) fn abort_current_control_budget() {
    CURRENT.with(|c| {
        let p = c.get();
        if !p.is_null() {
            // SAFETY: see `graphix_interrupted`.
            unsafe { (*p).abort_budget() }
        }
    });
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
