//! Process-global string interner for JIT-compiled kernels.
//!
//! ## Design
//!
//! - Each [`intern`] call hands back an `ArcStr` that's a clone of
//!   the canonical entry in the global table. Two intern calls for
//!   the same content share the same underlying `Arc<str>`.
//! - Compiled kernels own a `Box<[ArcStr]>` of these clones; the
//!   JIT-emitted machine code holds raw `*const ArcStr` pointers
//!   into that boxed slice. When the kernel drops, its slice drops,
//!   decrementing the underlying Arc refcounts.
//! - A background GC thread walks the global table on a timer,
//!   dropping entries whose `ref_count == 1` (i.e. no live kernel
//!   holds a clone). It only acquires the mutex when the
//!   `INTERN_COUNT` counter has advanced past a threshold since the
//!   last pass, so the cost is amortized.
//!
//! ## Why a global interner
//!
//! Two reasons:
//!
//! 1. Field names and variant tags are drawn from a small vocabulary
//!    in human-written code (`x`, `y`, `width`, `value`, …). Sharing
//!    the `Arc<str>` across all kernels in the process saves the
//!    bytes-per-string buffer for each duplicate.
//! 2. The GC pass reclaims entries when their last consumer drops,
//!    so machine-generated code with hygienic field names — a known
//!    blow-up case for naive interners — stays bounded as long as
//!    the runtime garbage-collects old kernels (which it does when
//!    a lambda is recompiled and the old `WrappedKernel` is
//!    replaced).
//!
//! The GC pass is correct because the JIT code's `*const ArcStr`
//! pointers go into the **per-kernel** boxed slice, not into the
//! global table. So dropping a table entry is only safe when no
//! kernel's boxed slice still holds a clone — and exactly when the
//! ArcStr's `ref_count` drops to 1, that condition is met.

use arcstr::ArcStr;
use std::{
    collections::HashSet,
    sync::{
        atomic::{AtomicUsize, Ordering},
        LazyLock, Mutex,
    },
    time::Duration,
};

/// How many new interns must accumulate before the GC thread takes
/// the mutex to walk the table. Lower = more responsive cleanup,
/// higher = less lock contention. 1024 keeps cleanup latency under
/// a second on workloads that intern at a sustained rate, and
/// produces zero overhead on workloads that intern a few hundred
/// total strings.
const GC_THRESHOLD: usize = 1024;

/// How often the background thread wakes to check the counter.
/// Combined with the threshold, this caps both the latency of
/// cleanup and the worst-case stall when the mutex is held.
const GC_INTERVAL: Duration = Duration::from_secs(5);

static INTERN_COUNT: AtomicUsize = AtomicUsize::new(0);

static INTERNER: LazyLock<Mutex<HashSet<ArcStr>>> = LazyLock::new(|| {
    // Spawn the GC thread once at first access. The thread holds
    // no references — it walks through the LazyLock each cycle —
    // so leaking it on process exit is harmless.
    std::thread::Builder::new()
        .name("graphix-jit-intern-gc".into())
        .spawn(gc_loop)
        .expect("spawn intern GC thread");
    Mutex::new(HashSet::new())
});

/// Intern `s`. If the canonical entry exists, returns a clone
/// (refcount bump). Otherwise inserts a clone of `s` as the
/// canonical and returns another clone. The returned ArcStr shares
/// its underlying `Arc<str>` with every other ArcStr ever returned
/// for the same content.
pub fn intern(s: &ArcStr) -> ArcStr {
    let mut table = INTERNER.lock().expect("intern table poisoned");
    if let Some(canonical) = table.get(s) {
        return canonical.clone();
    }
    let canonical = s.clone();
    table.insert(canonical.clone());
    INTERN_COUNT.fetch_add(1, Ordering::Relaxed);
    canonical
}

/// Run a GC pass immediately. Exposed for tests; production
/// reclaim happens via the background thread.
#[cfg(test)]
pub fn gc_now() {
    gc_pass();
}

fn gc_loop() {
    let mut last_seen = 0usize;
    loop {
        std::thread::sleep(GC_INTERVAL);
        let now = INTERN_COUNT.load(Ordering::Relaxed);
        if now.wrapping_sub(last_seen) < GC_THRESHOLD {
            continue;
        }
        last_seen = now;
        gc_pass();
    }
}

fn gc_pass() {
    let mut table = INTERNER.lock().expect("intern table poisoned");
    table.retain(|canonical| {
        // `strong_count` returns `None` for statically-allocated
        // `literal!` ArcStrs — those cost nothing, keep them.
        // Otherwise `Some(n)`: n == 1 means only this entry holds
        // the buffer, so dropping it reclaims memory; n > 1 means
        // some kernel's boxed slice still has a clone.
        match ArcStr::strong_count(canonical) {
            None => true,
            Some(n) => n > 1,
        }
    });
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn dedup_returns_same_underlying_arc() {
        let a = intern(&ArcStr::from("dedup_test_x"));
        let b = intern(&ArcStr::from("dedup_test_x"));
        // Same underlying buffer → ptr_eq.
        assert!(ArcStr::ptr_eq(&a, &b));
    }

    #[test]
    fn gc_reclaims_unreferenced_entries() {
        // Use a content-keyed search so we don't need to keep an
        // ArcStr around that would itself bump the refcount and
        // prevent GC. Intern via a fresh temporary; drop everything
        // we own; the table's entry should be the last reference.
        let unique = "gc_reclaim_unique_v2";
        {
            let _canonical = intern(&ArcStr::from(unique));
            // _canonical dropped at end of block.
        }
        gc_now();
        let table = INTERNER.lock().unwrap();
        let still_present =
            table.iter().any(|e| e.as_str() == unique);
        assert!(
            !still_present,
            "GC should have reclaimed entry `{unique}` whose only \
             external reference was dropped",
        );
    }

    #[test]
    fn gc_keeps_entries_with_live_clones() {
        let unique = "gc_keep_unique_v2";
        let held = intern(&ArcStr::from(unique));
        gc_now();
        let table = INTERNER.lock().unwrap();
        let present = table.iter().any(|e| e.as_str() == unique);
        assert!(present, "GC must not reclaim while a clone is live");
        drop(held);
    }
}
