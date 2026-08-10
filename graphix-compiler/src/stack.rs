/// Stack headroom that must remain before [`ensure_sufficient`]
/// switches to a fresh segment. It has to exceed what ONE recursion
/// level can consume between two checks — an unoptimized `expr` parse
/// cycle burns ~420KB (the `choice` tuples in `expr` and `arith_term`
/// are ~112KB and ~125KB of stack frame each), a node-walk lambda
/// dispatch ~10KB.
const RED_ZONE: usize = 1024 * 1024;

/// Size of each fresh segment. Segments are mmap'd on entry and
/// released on exit, so this bounds how often a deep recursion pays
/// for one, not how much memory it holds.
const SEGMENT: usize = 32 * 1024 * 1024;

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
    stacker::maybe_grow(RED_ZONE, SEGMENT, f)
}
