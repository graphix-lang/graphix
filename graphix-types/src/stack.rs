use enumflags2::bitflags;
use std::{
    cell::Cell,
    sync::{
        LazyLock, OnceLock,
        atomic::{AtomicI64, AtomicU8, AtomicU32, AtomicU64, AtomicUsize, Ordering},
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

/// The budget a new runtime's [`Control`] starts with: unlimited,
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

/// The mode the variable `var` names, any case, else `default`; a value
/// that names none is reported, so a mistyped knob does not run quietly
/// in the default mode.
fn env_mode<T: Copy>(var: &str, modes: &[(&str, T)], default: T) -> T {
    let Ok(s) = std::env::var(var) else { return default };
    match modes.iter().find(|(n, _)| n.eq_ignore_ascii_case(s.trim())) {
        Some((_, m)) => *m,
        None => {
            let names = modes.iter().map(|(n, _)| *n).collect::<Vec<_>>().join(", ");
            let msg = compact_str::format_compact!(
                "{var}={s:?} is not one of {names}; using the default"
            );
            log::error!("{msg}");
            eprintln!("{msg}");
            default
        }
    }
}

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

/// The activation budget a new runtime's [`Control`] starts with:
/// unlimited, `GRAPHIX_ACTIVATION_BUDGET`, or the last
/// [`set_activation_budget`].
static DEFAULT_ACTIVATIONS: LazyLock<AtomicU64> = LazyLock::new(|| {
    AtomicU64::new(match std::env::var("GRAPHIX_ACTIVATION_BUDGET") {
        Err(_) => u64::MAX,
        Ok(s) => s.trim().parse().unwrap_or_else(|_| {
            let msg = compact_str::format_compact!(
                "GRAPHIX_ACTIVATION_BUDGET={s:?} is not a count; running without an \
                 activation budget"
            );
            log::error!("{msg}");
            eprintln!("{msg}");
            u64::MAX
        }),
    })
});

/// Set the activation budget runtimes created from now on start with; a
/// running one keeps its own ([`Control::set_activation_budget`]).
pub fn set_activation_budget(activations: u64) {
    DEFAULT_ACTIVATIONS.store(activations, Ordering::Relaxed);
}

/// Count an activation born in the runtime whose cycle runs on this
/// thread, aborting it past its budget: the containment of a runaway
/// recursion, which a count follows whatever thread or fork it runs on.
/// The address of the runtime charged, which [`Control::activation_died`]
/// releases; `None` outside a cycle, charging nothing.
#[doc(hidden)]
pub fn activation_born() -> Option<usize> {
    let p = current_control()?;
    // SAFETY: see `current_control`.
    let c = unsafe { &*p };
    let live = c.activations.fetch_add(1, Ordering::Relaxed) + 1;
    if live > c.activation_budget() && c.abort_budget(Budget::Activations) {
        log::error!(
            "activation budget ({}) exceeded by a recursion — aborting the runtime \
             (raise via GRAPHIX_ACTIVATION_BUDGET or Control::set_activation_budget)",
            c.activation_budget()
        );
    }
    Some(p as usize)
}

/// Abort the running runtime because a recursion exceeded its budget;
/// the one exit for both the node-walk and the kernel stack check.
#[doc(hidden)]
pub fn budget_abort() {
    if abort_current_control_budget() {
        log::error!(
            "stack budget ({} bytes) exceeded by a recursion — aborting the runtime \
             (raise via GRAPHIX_STACK_BUDGET or graphix_compiler::set_stack_budget)",
            current_stack_budget()
        );
    }
}

/// Run `f` with a guarantee of [`RED_ZONE`] stack, moving onto a fresh
/// heap segment when the current stack is nearly exhausted. Wrap every
/// recursion knot a user program can drive arbitrarily deep.
#[inline(always)]
#[doc(hidden)]
pub fn ensure_sufficient<R>(f: impl FnOnce() -> R) -> R {
    if stacker::remaining_stack().unwrap_or(0) >= RED_ZONE { f() } else { grow(f) }
}

/// Whether one more segment would put the runtime running on this
/// thread over its budget; outside a cycle there is none to exceed. The
/// budget is counted in whole segments.
#[doc(hidden)]
pub fn grow_exceeds_budget() -> bool {
    match current_control() {
        // SAFETY: see `current_control`.
        Some(c) => unsafe {
            (*c).grown.load(Ordering::Relaxed) + SEGMENT > (*c).stack_budget()
        },
        None => false,
    }
}

/// Count a fused kernel run (`jit` false) or a JIT wrapper entry
/// against the runtime whose cycle runs on this thread.
#[doc(hidden)]
pub fn record_invocation(jit: bool) {
    if let Some(c) = current_control() {
        // SAFETY: see `current_control`.
        let c = unsafe { &*c };
        let n = if jit { &c.jit_runs } else { &c.fused_runs };
        n.fetch_add(1, Ordering::Relaxed);
    }
}

/// Count `n` per-activation kernel blocks allocated (or freed, negative)
/// against the runtime whose cycle runs on this thread. A block is born
/// and freed in one runtime's cycles, so it is released where it was
/// charged.
#[doc(hidden)]
pub fn record_self_blocks(n: i64) {
    let Some(c) = current_control() else { return };
    // SAFETY: see `current_control`.
    let c = unsafe { &*c };
    c.self_blocks.fetch_add(n, Ordering::Relaxed);
    // a kernel's per-activation block is an activation
    match n {
        1 => {
            activation_born();
        }
        -1 => c.activation_died(),
        _ => (),
    }
}

/// `GRAPHIX_EVAL_THREADS`, else one worker a core: the workers an
/// evaluation pool is built with.
pub fn eval_threads() -> usize {
    static THREADS: LazyLock<usize> = LazyLock::new(|| {
        std::env::var("GRAPHIX_EVAL_THREADS")
            .ok()
            .and_then(|v| v.parse().ok())
            .filter(|n| *n > 0)
            .unwrap_or_else(|| {
                std::thread::available_parallelism().map_or(1, |n| n.get())
            })
    });
    *THREADS
}

/// `f` of the [`Control`] of the cycle running on this thread, `None`
/// outside one.
pub fn with_current_control<T>(f: impl FnOnce(Option<&Control>) -> T) -> T {
    // SAFETY: see `current_control`.
    f(current_control().map(|c| unsafe { &*c }))
}

/// The running cycle's [`Control`] on this thread; it outlives every
/// segment the cycle grows.
fn current_control() -> Option<*const Control> {
    CURRENT.with(|c| Some(c.get()).filter(|p| !p.is_null()))
}

/// One segment's share of the count it was charged to, returned when
/// the segment is left, by an unwind too.
struct Grown(Option<*const Control>);

impl Grown {
    fn enter() -> Self {
        let control = current_control();
        if let Some(c) = control {
            // SAFETY: see `current_control`.
            unsafe { (*c).grown.fetch_add(SEGMENT, Ordering::Relaxed) };
        }
        Grown(control)
    }
}

impl Drop for Grown {
    fn drop(&mut self) {
        if let Some(c) = self.0 {
            // SAFETY: see `current_control`.
            unsafe { (*c).grown.fetch_sub(SEGMENT, Ordering::Relaxed) };
        }
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
    StackBudget = 4,
    /// Set beside `Abort` when the activation budget stopped the runtime.
    ActivationBudget = 8,
}

/// Which budget stopped a runtime.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Budget {
    Stack,
    Activations,
}

/// Lock-free [`CtlFlag`] set. A loop polls [`Control::interrupted`];
/// the run loop polls [`Control::aborted`]. Also this runtime's stack
/// budget: the grown stack a recursion may hold before
/// [`Control::abort_budget`].
#[derive(Debug)]
pub struct Control {
    flags: AtomicU32,
    stack_budget: AtomicUsize,
    /// Bytes of grown stack segments live in this runtime's cycle, on
    /// every thread of its evaluation.
    grown: AtomicUsize,
    par: AtomicU8,
    /// Fused kernel runs and JIT wrapper entries in this runtime's
    /// cycles, for the test harness's fusion expectations.
    fused_runs: AtomicU64,
    jit_runs: AtomicU64,
    /// Live per-activation kernel blocks, a test instrument for their
    /// reclaim.
    self_blocks: AtomicI64,
    /// Forks made by this runtime's cycles.
    forks: AtomicU64,
    /// Runs of compile tasks its cycles forked to build instances.
    build_forks: AtomicU64,
    /// Batches of run-time binds and compiles its cycles applied.
    rebuilds: AtomicU64,
    /// Fork-point verdicts its cycles worked out from the graph.
    verdicts: AtomicU64,
    /// Live activations its cycles bound, and how many it may hold.
    activations: AtomicU64,
    activation_budget: AtomicU64,
    prune: AtomicU8,
    /// Boundaries its cycles walked and skipped.
    walked: AtomicU64,
    skipped: AtomicU64,
    /// The pool its cycles fork onto, built at the first fork: no other
    /// runtime's parts run there, so none waits on another's.
    pool: OnceLock<rayon::ThreadPool>,
    /// Parts forked onto the pool and not yet finished.
    live_parts: AtomicUsize,
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
    /// `GRAPHIX_PAR` (`off`, `auto` or `force`), else `Auto`.
    pub fn from_env() -> Self {
        static MODE: LazyLock<ParMode> = LazyLock::new(|| {
            env_mode(
                "GRAPHIX_PAR",
                &[
                    ("off", ParMode::Off),
                    ("auto", ParMode::Auto),
                    ("force", ParMode::Force),
                ],
                ParMode::Auto,
            )
        });
        *MODE
    }
}

/// Whether a cycle skips the boundaries nothing they read reached
/// (`design/walk_pruning.md`).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum PruneMode {
    /// Every boundary is walked.
    Off,
    /// A clean boundary rides its resident.
    On,
    /// Every boundary is walked, and one pruning would have skipped must
    /// change nothing.
    Audit,
}

impl PruneMode {
    /// `GRAPHIX_PRUNE` (`off`, `on` or `audit`), else `On`.
    pub fn from_env() -> Self {
        static MODE: LazyLock<PruneMode> = LazyLock::new(|| {
            env_mode(
                "GRAPHIX_PRUNE",
                &[
                    ("off", PruneMode::Off),
                    ("on", PruneMode::On),
                    ("audit", PruneMode::Audit),
                ],
                PruneMode::On,
            )
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
            grown: AtomicUsize::new(0),
            par: AtomicU8::new(ParMode::from_env() as u8),
            fused_runs: AtomicU64::new(0),
            jit_runs: AtomicU64::new(0),
            self_blocks: AtomicI64::new(0),
            forks: AtomicU64::new(0),
            build_forks: AtomicU64::new(0),
            rebuilds: AtomicU64::new(0),
            verdicts: AtomicU64::new(0),
            activations: AtomicU64::new(0),
            activation_budget: AtomicU64::new(
                DEFAULT_ACTIVATIONS.load(Ordering::Relaxed),
            ),
            prune: AtomicU8::new(PruneMode::from_env() as u8),
            walked: AtomicU64::new(0),
            skipped: AtomicU64::new(0),
            pool: OnceLock::new(),
            live_parts: AtomicUsize::new(0),
        }
    }

    /// This runtime's evaluation pool, built at the first call.
    pub fn eval_pool(&self) -> &rayon::ThreadPool {
        self.pool.get_or_init(|| {
            let mut threads = eval_threads();
            // a process short of address space or threads gets fewer workers
            loop {
                let built = rayon::ThreadPoolBuilder::new()
                    .num_threads(threads)
                    .stack_size(16 << 20)
                    .thread_name(|i| format!("graphix-eval-{i}"))
                    .build();
                match built {
                    Ok(pool) => break pool,
                    Err(e) if threads > 1 => {
                        log::warn!("the evaluation pool: {threads} workers: {e}");
                        threads /= 2
                    }
                    Err(e) => panic!("the evaluation pool: {e}"),
                }
            }
        })
    }

    /// The workers of this runtime's evaluation pool: those it has, else
    /// those it will be built with.
    pub fn eval_workers(&self) -> usize {
        self.pool.get().map_or_else(eval_threads, |p| p.current_num_threads())
    }

    /// `n` parts forked onto the pool, or finished when negative.
    pub fn add_live_parts(&self, n: isize) {
        match n {
            0.. => self.live_parts.fetch_add(n as usize, Ordering::Relaxed),
            _ => self.live_parts.fetch_sub(n.unsigned_abs(), Ordering::Relaxed),
        };
    }

    /// Whether every worker of the pool has a part to run.
    pub fn eval_saturated(&self) -> bool {
        self.live_parts.load(Ordering::Relaxed) >= self.eval_workers()
    }

    pub fn prune_mode(&self) -> PruneMode {
        match self.prune.load(Ordering::Relaxed) {
            0 => PruneMode::Off,
            2 => PruneMode::Audit,
            _ => PruneMode::On,
        }
    }

    pub fn set_prune_mode(&self, mode: PruneMode) {
        self.prune.store(mode as u8, Ordering::Relaxed)
    }

    /// Count a boundary walked, or skipped.
    pub fn note_boundary(&self, walked: bool) {
        match walked {
            true => self.walked.fetch_add(1, Ordering::Relaxed),
            false => self.skipped.fetch_add(1, Ordering::Relaxed),
        };
    }

    /// The boundaries this runtime's cycles walked and skipped.
    pub fn boundaries(&self) -> (u64, u64) {
        (self.walked.load(Ordering::Relaxed), self.skipped.load(Ordering::Relaxed))
    }

    /// Fused kernel runs and JIT wrapper entries since the last reset.
    pub fn invocations(&self) -> (u64, u64) {
        (self.fused_runs.load(Ordering::Relaxed), self.jit_runs.load(Ordering::Relaxed))
    }

    /// Forks made by this runtime's cycles.
    pub fn forks(&self) -> u64 {
        self.forks.load(Ordering::Relaxed)
    }

    #[doc(hidden)]
    pub fn forked(&self) {
        self.forks.fetch_add(1, Ordering::Relaxed);
    }

    /// Runs of compile tasks this runtime's cycles forked to build
    /// instances.
    pub fn build_forks(&self) -> u64 {
        self.build_forks.load(Ordering::Relaxed)
    }

    #[doc(hidden)]
    pub fn build_forked(&self) {
        self.build_forks.fetch_add(1, Ordering::Relaxed);
    }

    /// Batches of run-time binds and compiles applied so far: a count
    /// that changes whenever a call may have been bound since it was
    /// last read.
    pub fn rebuilds(&self) -> u64 {
        self.rebuilds.load(Ordering::Relaxed)
    }

    #[doc(hidden)]
    pub fn rebuilt(&self) {
        self.rebuilds.fetch_add(1, Ordering::Relaxed);
    }

    /// Fork-point verdicts this runtime's cycles worked out from the
    /// graph below the fork point, where no analysis had decided one.
    pub fn verdicts(&self) -> u64 {
        self.verdicts.load(Ordering::Relaxed)
    }

    #[doc(hidden)]
    pub fn verdict_asked(&self) {
        self.verdicts.fetch_add(1, Ordering::Relaxed);
    }

    /// Live per-activation kernel blocks.
    pub fn live_self_blocks(&self) -> i64 {
        self.self_blocks.load(Ordering::Relaxed)
    }

    pub fn reset_invocations(&self) {
        self.fused_runs.store(0, Ordering::Relaxed);
        self.jit_runs.store(0, Ordering::Relaxed);
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

    /// The bytes of grown stack segments a cycle of this runtime may
    /// hold, summed over every thread it runs on; `usize::MAX` is
    /// unlimited.
    pub fn stack_budget(&self) -> usize {
        self.stack_budget.load(Ordering::Relaxed)
    }

    pub fn set_stack_budget(&self, bytes: usize) {
        self.stack_budget.store(bytes, Ordering::Relaxed)
    }

    /// The activations this runtime may hold at once, over every call,
    /// slot and kernel block its cycles bind; `u64::MAX` is unlimited.
    /// Past it the runtime aborts as past its stack budget.
    pub fn activation_budget(&self) -> u64 {
        self.activation_budget.load(Ordering::Relaxed)
    }

    pub fn set_activation_budget(&self, activations: u64) {
        self.activation_budget.store(activations, Ordering::Relaxed)
    }

    /// The activations this runtime's cycles hold now.
    pub fn live_activations(&self) -> u64 {
        self.activations.load(Ordering::Relaxed)
    }

    /// Release an activation [`activation_born`] charged here.
    pub fn activation_died(&self) {
        let _ =
            self.activations.fetch_update(Ordering::Relaxed, Ordering::Relaxed, |n| {
                Some(n.saturating_sub(1))
            });
    }

    /// Request that in-flight loops abort this cycle; cleared when the
    /// next cycle starts, so one sent while the runtime is idle is dropped.
    pub fn interrupt(&self) {
        self.flags.fetch_or(CtlFlag::Interrupt as u32, Ordering::Release);
    }

    /// Request shutdown: in-flight loops abort and the run loop returns
    /// before the next cycle. Sticky.
    pub fn abort(&self) {
        self.flags.fetch_or(CtlFlag::Abort as u32, Ordering::Release);
    }

    /// [`Self::abort`], marked as `budget`'s doing unless a budget already
    /// stopped the runtime; whether this call was the first budget to.
    pub fn abort_budget(&self, budget: Budget) -> bool {
        const CAUSES: u32 =
            CtlFlag::StackBudget as u32 | CtlFlag::ActivationBudget as u32;
        let cause = match budget {
            Budget::Stack => CtlFlag::StackBudget as u32,
            Budget::Activations => CtlFlag::ActivationBudget as u32,
        };
        let prev = self.flags.fetch_or(CtlFlag::Abort as u32, Ordering::AcqRel);
        prev & CAUSES == 0
            && self
                .flags
                .fetch_update(Ordering::Release, Ordering::Acquire, |f| {
                    (f & CAUSES == 0).then_some(f | cause)
                })
                .is_ok()
    }

    /// The budget that aborted this runtime, if one did.
    pub fn aborted_by(&self) -> Option<Budget> {
        let f = self.flags.load(Ordering::Acquire);
        if f & CtlFlag::StackBudget as u32 != 0 {
            Some(Budget::Stack)
        } else if f & CtlFlag::ActivationBudget as u32 != 0 {
            Some(Budget::Activations)
        } else {
            None
        }
    }

    /// True if a budget, stack or activations, aborted this runtime.
    pub fn budget_aborted(&self) -> bool {
        self.aborted_by().is_some()
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
    /// thread ([`with_control`]); null outside a cycle.
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

/// Runs `f` with `graphix_interrupted` pointed at a runtime's
/// [`Control`] on this thread, as while its cycle's nodes run; the
/// enclosing runtime's is back when `f` returns or unwinds. Run it on
/// the thread that runs the nodes, before the task can migrate.
pub fn with_control<T>(control: &Control, f: impl FnOnce() -> T) -> T {
    let _restore = crate::Restore::replace(&CURRENT, control as *const Control);
    f()
}

/// The stack budget of the runtime whose cycle this thread is running;
/// the default budget outside a cycle.
pub(crate) fn current_stack_budget() -> usize {
    CURRENT.with(|c| {
        let p = c.get();
        // SAFETY: see `interrupted`.
        if p.is_null() { default_budget() } else { unsafe { (*p).stack_budget() } }
    })
}

/// Abort the running cycle's runtime by its stack budget; whether this
/// call stopped it (false with no runtime on this thread).
pub(crate) fn abort_current_control_budget() -> bool {
    CURRENT.with(|c| {
        let p = c.get();
        // SAFETY: see `interrupted`.
        !p.is_null() && unsafe { (*p).abort_budget(Budget::Stack) }
    })
}

#[cfg(test)]
mod test {
    use super::*;

    // each runtime forks onto a pool of its own: no worker of one runs
    // another's parts
    #[test]
    fn a_pool_per_runtime() {
        let (a, b) = (Control::new(), Control::new());
        assert!(!std::ptr::eq(a.eval_pool(), b.eval_pool()));
        a.eval_pool().install(|| {
            assert!(a.eval_pool().current_thread_index().is_some());
            assert!(b.eval_pool().current_thread_index().is_none());
        });
    }

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
