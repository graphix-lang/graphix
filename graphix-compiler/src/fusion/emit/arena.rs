//! The memory a JIT generation installs into: one reservation of address
//! space, committed a page run at a time as the module allocates.

use anyhow::anyhow;
use cranelift_jit::{BranchProtection, JITMemoryKind, JITMemoryProvider};
use cranelift_module::{ModuleError, ModuleResult};
use region::Protection;
use std::{
    io,
    sync::{
        Arc,
        atomic::{AtomicUsize, Ordering},
    },
};
use wasmtime_internal_jit_icache_coherence::{clear_cache, pipeline_flush_mt};

fn icache(e: impl std::fmt::Display) -> ModuleError {
    ModuleError::Backend(anyhow!("jit icache: {e}"))
}

fn align_up(n: usize, align: usize) -> usize {
    (n + align - 1) & !(align - 1)
}

/// Pages that take one protection at finalization, `start` and `len` as
/// offsets into the arena, `used` bytes of them allocated.
struct Segment {
    start: usize,
    len: usize,
    used: usize,
    prot: Protection,
}

impl Segment {
    fn fits(&self, size: usize, align: usize) -> bool {
        align_up(self.start + self.used, align) + size <= self.start + self.len
    }

    fn bump(&mut self, size: usize, align: usize) -> usize {
        let at = align_up(self.start + self.used, align);
        self.used = at + size - self.start;
        at
    }
}

/// What an arena has committed, counted where its owners read it: its
/// generation's count, and its JIT's across every live generation.
#[derive(Clone)]
pub(super) struct Committed {
    pub(super) generation: Arc<AtomicUsize>,
    pub(super) jit: Arc<AtomicUsize>,
}

impl Committed {
    fn add(&self, n: usize) {
        self.generation.fetch_add(n, Ordering::Relaxed);
        self.jit.fetch_add(n, Ordering::Relaxed);
    }

    fn sub(&self, n: usize) {
        self.generation.fetch_sub(n, Ordering::Relaxed);
        self.jit.fetch_sub(n, Ordering::Relaxed);
    }
}

/// The arena has no room left for an allocation: the generation retires
/// and the install retries in a fresh one.
#[derive(Debug)]
pub(crate) struct ArenaExhausted;

impl std::fmt::Display for ArenaExhausted {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("the JIT code arena is exhausted")
    }
}

impl std::error::Error for ArenaExhausted {}

pub(super) struct Arena {
    /// None once freed.
    reservation: Option<os::Reservation>,
    size: usize,
    committed: usize,
    counted: Committed,
    /// The segments not finalized yet, in address order; the last one
    /// ends at `committed`. A finalized segment is never written again.
    open: Vec<Segment>,
}

impl Arena {
    pub(super) fn new(size: usize, counted: Committed) -> io::Result<Self> {
        let size = align_up(size, region::page::size());
        let reservation = Some(os::Reservation::new(size)?);
        Ok(Self { reservation, size, committed: 0, counted, open: Vec::new() })
    }

    fn release(&mut self) {
        self.open.clear();
        if self.reservation.take().is_some() {
            self.counted.sub(self.committed);
        }
    }
}

impl Drop for Arena {
    fn drop(&mut self) {
        self.release()
    }
}

impl JITMemoryProvider for Arena {
    fn allocate(
        &mut self,
        size: usize,
        align: u64,
        kind: JITMemoryKind,
    ) -> io::Result<*mut u8> {
        let page = region::page::size();
        let align = usize::try_from(align)
            .ok()
            .filter(|a| a.is_power_of_two() && *a <= page)
            .ok_or_else(|| {
                io::Error::other(format!("unsupported jit alignment {align}"))
            })?;
        let prot = match kind {
            JITMemoryKind::Executable => Protection::READ_EXECUTE,
            JITMemoryKind::Writable => Protection::READ_WRITE,
            JITMemoryKind::ReadOnly => Protection::READ,
        };
        let res = self
            .reservation
            .as_ref()
            .ok_or_else(|| io::Error::other("jit arena freed"))?;
        let i = match self.open.iter().position(|s| s.prot == prot && s.fits(size, align))
        {
            Some(i) => i,
            None => {
                let grow = align_up(size, page);
                if grow > self.size - self.committed {
                    return Err(io::Error::other(ArenaExhausted));
                }
                // SAFETY: the run lies inside the reservation.
                unsafe { res.commit(res.base().add(self.committed), grow)? };
                match self.open.last_mut() {
                    Some(s) if s.prot == prot => s.len += grow,
                    _ => self.open.push(Segment {
                        start: self.committed,
                        len: grow,
                        used: 0,
                        prot,
                    }),
                }
                self.committed += grow;
                self.counted.add(grow);
                self.open.len() - 1
            }
        };
        let at = self.open[i].bump(size, align);
        // SAFETY: `at + size` lies inside a committed segment.
        Ok(unsafe { res.base().add(at) })
    }

    unsafe fn free_memory(&mut self) {
        self.release()
    }

    fn finalize(&mut self, branch_protection: BranchProtection) -> ModuleResult<()> {
        let backend = |e: io::Error| ModuleError::Backend(e.into());
        if let Some(res) = &self.reservation {
            for s in self.open.drain(..) {
                // SAFETY: the segment is committed and inside the reservation.
                unsafe {
                    let ptr = res.base().add(s.start);
                    if s.prot == Protection::READ_EXECUTE {
                        clear_cache(ptr.cast(), s.len).map_err(icache)?;
                    }
                    region::protect(ptr, s.len, s.prot)
                        .map_err(|e| backend(io::Error::other(e)))?;
                    if s.prot == Protection::READ_EXECUTE {
                        mark_bti(ptr, s.len, branch_protection).map_err(backend)?;
                    }
                }
            }
        }
        pipeline_flush_mt().map_err(icache)
    }
}

/// SAFETY: `[ptr, ptr + len)` is committed executable memory.
#[cfg(all(target_arch = "aarch64", target_os = "linux"))]
unsafe fn mark_bti(ptr: *mut u8, len: usize, bp: BranchProtection) -> io::Result<()> {
    const PROT_BTI: libc::c_int = 0x10;
    if bp == BranchProtection::BTI && std::arch::is_aarch64_feature_detected!("bti") {
        let prot = libc::PROT_EXEC | libc::PROT_READ | PROT_BTI;
        if unsafe { libc::mprotect(ptr.cast(), len, prot) } < 0 {
            return Err(io::Error::last_os_error());
        }
    }
    Ok(())
}

#[cfg(not(all(target_arch = "aarch64", target_os = "linux")))]
unsafe fn mark_bti(_: *mut u8, _: usize, _: BranchProtection) -> io::Result<()> {
    Ok(())
}

#[cfg(not(windows))]
mod os {
    use region::Protection;
    use std::io;

    pub(super) struct Reservation(region::Allocation);

    // SAFETY: the mapping belongs to no thread.
    unsafe impl Send for Reservation {}

    impl Reservation {
        pub(super) fn new(len: usize) -> io::Result<Self> {
            region::alloc(len, Protection::NONE).map(Self).map_err(io::Error::other)
        }

        pub(super) fn base(&self) -> *mut u8 {
            self.0.as_ptr::<u8>().cast_mut()
        }

        /// SAFETY: `[at, at + len)` lies inside the reservation.
        pub(super) unsafe fn commit(&self, at: *mut u8, len: usize) -> io::Result<()> {
            unsafe { region::protect(at, len, Protection::READ_WRITE) }
                .map_err(io::Error::other)
        }
    }
}

#[cfg(windows)]
mod os {
    use std::{io, ptr};
    use windows_sys::Win32::System::Memory::{
        MEM_COMMIT, MEM_RELEASE, MEM_RESERVE, PAGE_NOACCESS, PAGE_READWRITE,
        VirtualAlloc, VirtualFree,
    };

    pub(super) struct Reservation(*mut u8);

    // SAFETY: the mapping belongs to no thread.
    unsafe impl Send for Reservation {}

    impl Reservation {
        pub(super) fn new(len: usize) -> io::Result<Self> {
            let p = unsafe { VirtualAlloc(ptr::null(), len, MEM_RESERVE, PAGE_NOACCESS) };
            match p.is_null() {
                true => Err(io::Error::last_os_error()),
                false => Ok(Self(p.cast())),
            }
        }

        pub(super) fn base(&self) -> *mut u8 {
            self.0
        }

        /// SAFETY: `[at, at + len)` lies inside the reservation.
        pub(super) unsafe fn commit(&self, at: *mut u8, len: usize) -> io::Result<()> {
            let p = unsafe { VirtualAlloc(at.cast(), len, MEM_COMMIT, PAGE_READWRITE) };
            match p.is_null() {
                true => Err(io::Error::last_os_error()),
                false => Ok(()),
            }
        }
    }

    impl Drop for Reservation {
        fn drop(&mut self) {
            unsafe { VirtualFree(self.0.cast(), 0, MEM_RELEASE) };
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn counted() -> Committed {
        Committed { generation: Arc::default(), jit: Arc::default() }
    }

    #[test]
    fn released_memory_is_uncounted() {
        let c = counted();
        let mut arena = Arena::new(1 << 20, c.clone()).unwrap();
        arena.allocate(5000, 8, JITMemoryKind::Writable).unwrap();
        let page = region::page::size();
        assert_eq!(c.jit.load(Ordering::Relaxed), align_up(5000, page));
        drop(arena);
        assert_eq!(c.jit.load(Ordering::Relaxed), 0);
        assert_eq!(c.generation.load(Ordering::Relaxed), 0);
    }

    #[test]
    fn allocations_are_aligned_and_disjoint() {
        let mut arena = Arena::new(1 << 20, counted()).unwrap();
        let mut last: Option<(usize, usize)> = None;
        for read_only in [false, true, false] {
            for log2 in 0..8 {
                let align = 1usize << log2;
                let kind = match read_only {
                    true => JITMemoryKind::ReadOnly,
                    false => JITMemoryKind::Writable,
                };
                let p = arena.allocate(37, align as u64, kind).unwrap() as usize;
                assert_eq!(p % align, 0);
                if let Some((q, n)) = last {
                    assert!(p >= q + n || p + 37 <= q, "allocations overlap");
                }
                // SAFETY: freshly allocated, writable until finalized.
                unsafe { std::ptr::write_bytes(p as *mut u8, 0xa5, 37) };
                last = Some((p, 37));
            }
        }
        arena.finalize(BranchProtection::None).unwrap();
    }

    #[test]
    fn a_finalized_segment_is_never_written_again() {
        let mut arena = Arena::new(1 << 20, counted()).unwrap();
        let a = arena.allocate(8, 8, JITMemoryKind::ReadOnly).unwrap();
        arena.finalize(BranchProtection::None).unwrap();
        let b = arena.allocate(8, 8, JITMemoryKind::ReadOnly).unwrap();
        assert!(b as usize >= a as usize + region::page::size());
        // SAFETY: `b` is writable until the next finalize; `a` is readable.
        unsafe {
            b.write(1);
            assert_eq!(a.read(), 0);
        }
    }

    #[test]
    fn an_allocation_past_the_reservation_fails() {
        let mut arena = Arena::new(1 << 20, counted()).unwrap();
        arena.allocate(900_000, 1, JITMemoryKind::Writable).unwrap();
        arena.allocate(200_000, 1, JITMemoryKind::Writable).unwrap_err();
        unsafe { arena.free_memory() };
        arena.allocate(1, 1, JITMemoryKind::Writable).unwrap_err();
    }

    /// Reserving commits nothing: sixty-four 256MB arenas charge the
    /// process for the pages they allocate only.
    #[cfg(windows)]
    #[test]
    fn a_reservation_commits_only_what_is_allocated() {
        use windows_sys::Win32::System::{
            ProcessStatus::{K32GetProcessMemoryInfo, PROCESS_MEMORY_COUNTERS_EX},
            Threading::GetCurrentProcess,
        };
        fn private_bytes() -> usize {
            let mut c: PROCESS_MEMORY_COUNTERS_EX = unsafe { std::mem::zeroed() };
            let size = std::mem::size_of_val(&c) as u32;
            let ok = unsafe {
                K32GetProcessMemoryInfo(GetCurrentProcess(), (&raw mut c).cast(), size)
            };
            assert_ne!(ok, 0, "{}", io::Error::last_os_error());
            c.PrivateUsage
        }
        let before = private_bytes();
        let mut arenas: Vec<Arena> =
            (0..64).map(|_| Arena::new(256 << 20, counted()).unwrap()).collect();
        for a in &mut arenas {
            a.allocate(4096, 16, JITMemoryKind::Executable).unwrap();
        }
        let grew = private_bytes().saturating_sub(before);
        assert!(grew < 16 << 20, "64 arenas committed {grew} bytes");
    }
}
