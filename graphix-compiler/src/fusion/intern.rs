//! Process-global string interner for JIT-compiled kernels.
//!
//! [`intern`] returns a clone of the canonical `ArcStr` for its content,
//! so every kernel holding the same string shares one `Arc<str>`.
//! Kernels own a `Box<[ArcStr]>` of these clones and emitted code holds
//! raw pointers into that slice, never into the table, so a background
//! thread may drop any table entry it alone references.

use arcstr::ArcStr;
use std::{
    collections::HashSet,
    sync::{LazyLock, Mutex},
    time::Duration,
};

/// How often the GC thread sweeps the table.
const GC_INTERVAL: Duration = Duration::from_secs(5);

static INTERNER: LazyLock<Mutex<HashSet<ArcStr>>> = LazyLock::new(|| {
    std::thread::Builder::new()
        .name("graphix-jit-intern-gc".into())
        .spawn(gc_loop)
        .expect("spawn intern GC thread");
    Mutex::new(HashSet::new())
});

/// Returns the canonical `ArcStr` for `s`'s content, inserting `s` if absent.
pub fn intern(s: &ArcStr) -> ArcStr {
    let mut table = INTERNER.lock().expect("intern table poisoned");
    if let Some(canonical) = table.get(s) {
        return canonical.clone();
    }
    let canonical = s.clone();
    table.insert(canonical.clone());
    canonical
}

/// Run a GC pass immediately.
#[cfg(test)]
pub fn gc_now() {
    gc_pass();
}

// XCR codex for eric: CR23 — done: every interval sweeps a non-empty table;
// a string's owners can drop without a new intern, so no counter decides.
fn gc_loop() {
    loop {
        std::thread::sleep(GC_INTERVAL);
        gc_pass();
    }
}

fn gc_pass() {
    let mut table = INTERNER.lock().expect("intern table poisoned");
    table.retain(|canonical| {
        // `None` is a static `literal!`; `Some(1)` means only the table holds it.
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
        assert!(ArcStr::ptr_eq(&a, &b));
    }

    #[test]
    fn gc_reclaims_unreferenced_entries() {
        let unique = "gc_reclaim_unique_v2";
        {
            let _canonical = intern(&ArcStr::from(unique));
        }
        gc_now();
        let table = INTERNER.lock().unwrap();
        let still_present = table.iter().any(|e| e.as_str() == unique);
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
