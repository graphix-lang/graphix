//! t-misc-03: `InterruptScope::new` is a safe API that can leave the
//! thread's CURRENT control pointer dangling (use-after-free from safe
//! code).
//!
//! Command (with this file copied to
//! graphix-types/tests/review_t_misc_03.rs):
//!   timeout -s KILL 2400 cargo test -p graphix-types \
//!     --test review_t_misc_03 -- --nocapture
//!
//! `graphix_compiler::InterruptScope`/`Control` are re-exports of the
//! types used here. Each test reaches, in safe code only, a state where
//! CURRENT points at a freed `Control`: a scope that outlives its
//! control (no lifetime on the type), a forgotten scope, and two scopes
//! dropped out of order (which a `PhantomData<&'a Control>` would still
//! accept). The freed chunk is then handed back by the allocator as an
//! unrelated `Vec<u64>`, and three safe calls run on the same thread:
//! `interrupted()`, `record_self_blocks` and an ordinary recursion
//! through `ensure_sufficient` (the stack guard every parse, compile,
//! print and drop of a deep tree runs).
//!
//! Expected: the buffer stays all zeros and `interrupted()` is false (no
//! control was ever interrupted). Observed at c722befe, the same in all
//! three tests (dev profile):
//!   scope outlives its control: freed Control at 0x7f2244000cc0,
//!     new Vec<u64> buffer at 0x7f2244000cc0 (same chunk: true)
//!   interrupted() over a buffer of ones: true
//!   buffer after record_self_blocks(0x5eed): [0, 0, 0, 0, 0x5eed, 0, 0, 0]
//!   buffer after a deep ensure_sufficient recursion: [0, 0, 0, 0, 0, 0, 0, 0x6]
//!   interrupted() after it: true
//!   test result: FAILED. 0 passed; 3 failed
//! The stack guard found the "budget" (zero bytes of the Vec) exceeded,
//! ORed Abort|Budget (6) into the dead control's flags word, and added
//! then removed a 32 MiB segment from its `grown` word, all inside the
//! live Vec.

use graphix_types::stack::{
    Control, InterruptScope, ensure_sufficient, interrupted, record_self_blocks,
};
use std::{
    hint::black_box,
    mem::{forget, size_of},
};

const WORDS: usize = size_of::<Control>() / 8;

fn words(buf: *const u64) -> [u64; WORDS] {
    std::array::from_fn(|i| unsafe { std::ptr::read_volatile(buf.add(i)) })
}

fn fill(buf: *mut u64, w: u64) {
    for i in 0..WORDS {
        unsafe { std::ptr::write_volatile(buf.add(i), w) }
    }
}

fn deep(depth: usize) -> u8 {
    ensure_sufficient(|| {
        let frame = black_box([depth as u8; 64 * 1024]);
        if depth == 40 { frame[0] } else { deep(depth + 1).wrapping_add(frame[1]) }
    })
}

/// Called right after the `Control` at `dead` was freed while CURRENT
/// still points at it.
fn probe(case: &str, dead: *const Control) {
    assert_eq!(size_of::<Control>() % 8, 0);
    let mut held: Vec<Vec<u64>> = Vec::new();
    let mut buf: Vec<u64> = Vec::with_capacity(WORDS);
    while buf.as_ptr() as usize != dead as usize && held.len() < 8 {
        held.push(std::mem::replace(&mut buf, Vec::with_capacity(WORDS)));
    }
    buf.resize(WORDS, 0);
    let p = black_box(buf.as_mut_ptr());
    let reused = p as usize == dead as usize;
    if !reused {
        // writing through CURRENT would corrupt the allocator's free list
        panic!("{case}: inconclusive, the allocator did not reuse {dead:p}");
    }
    fill(p, u64::MAX);
    let spurious = interrupted();
    fill(p, 0);
    record_self_blocks(0x5eed);
    let after_record = words(p);
    fill(p, 0);
    black_box(deep(0));
    let after_guard = words(p);
    let interrupted_after_guard = interrupted();
    println!(
        "{case}: freed Control at {dead:p}, new Vec<u64> buffer at {p:p} (same chunk: {reused})\n  \
         interrupted() over a buffer of ones: {spurious}\n  \
         buffer after record_self_blocks(0x5eed): {after_record:#x?}\n  \
         buffer after a deep ensure_sufficient recursion: {after_guard:#x?}\n  \
         interrupted() after it: {interrupted_after_guard}"
    );
    drop(held);
    assert!(
        !spurious && after_record == [0; WORDS] && after_guard == [0; WORDS],
        "{case}: safe calls read and wrote a freed Control's memory, now owned by an \
         unrelated Vec"
    );
}

fn scope_over_a_local_control() -> (InterruptScope, *const Control) {
    let control = Box::new(Control::new());
    (InterruptScope::new(&control), &*control as *const Control)
}

#[test]
fn a_scope_outlives_its_control() {
    let (scope, dead) = scope_over_a_local_control();
    probe("scope outlives its control", dead);
    drop(scope);
}

#[test]
fn a_forgotten_scope_leaves_its_control_current() {
    let control = Box::new(Control::new());
    let dead = &*control as *const Control;
    forget(InterruptScope::new(&control));
    drop(control);
    probe("forgotten scope", dead);
}

#[test]
fn scopes_dropped_out_of_order_restore_a_dead_control() {
    let first = Box::new(Control::new());
    let dead = &*first as *const Control;
    let outer = InterruptScope::new(&first);
    let second = Control::new();
    let inner = InterruptScope::new(&second);
    drop(outer);
    drop(inner);
    drop(first);
    probe("scopes dropped out of order", dead);
}
