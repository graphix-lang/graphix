//! The evaluation pool: work-stealing workers, each parking the moment it
//! finds no job, so a fork costs its woken workers no more CPU than the
//! jobs they run (`design/parallel_eval.md` §9).

use crossbeam_deque::{Injector, Steal, Stealer, Worker};
use parking_lot::{Condvar, Mutex};
use std::{
    any::Any,
    cell::{Cell, UnsafeCell},
    fmt, io, mem,
    panic::{self, AssertUnwindSafe},
    ptr,
    sync::atomic::{AtomicBool, AtomicU64, AtomicUsize, Ordering},
    thread::{self, Thread},
};
use triomphe::Arc;

type Panic = Box<dyn Any + Send>;

/// A job some thread waits on: `run(data, index)`.
#[derive(Clone, Copy)]
struct JobRef {
    data: *const (),
    index: usize,
    run: unsafe fn(*const (), usize),
}

// SAFETY: `data` is `Sync` and outlives the job: the thread that pushed it
// waits for the job to finish before the frame holding `data` returns.
unsafe impl Send for JobRef {}

impl JobRef {
    fn run(self) {
        // SAFETY: see `JobRef`'s `Send`.
        unsafe { (self.run)(self.data, self.index) }
    }
}

/// Where one worker blocks.
#[derive(Default)]
struct Bed {
    asleep: Mutex<bool>,
    wake: Condvar,
}

/// Workers with no job to run block here, each in its own bed, until a
/// job is announced or, for one waiting on its fork, the fork is done.
struct Sleep {
    beds: Box<[Bed]>,
    sleepers: AtomicUsize,
    /// Bumped by every announcement: a worker that read it before a
    /// search that found nothing sleeps only if it has not moved since.
    epoch: AtomicU64,
    terminated: AtomicBool,
    /// Searches that found no job, and parks: one a search.
    #[cfg(test)]
    empty_searches: AtomicU64,
    #[cfg(test)]
    parks: AtomicU64,
}

impl Sleep {
    /// `jobs` new jobs are visible: wake that many sleepers.
    fn announce(&self, mut jobs: usize) {
        self.epoch.fetch_add(1, Ordering::SeqCst);
        if jobs == 0 || self.sleepers.load(Ordering::SeqCst) == 0 {
            return;
        }
        for worker in 0..self.beds.len() {
            if self.wake(worker) {
                jobs -= 1;
                if jobs == 0 {
                    break;
                }
            }
        }
    }

    /// Wake `worker` if it is asleep; whether it was.
    fn wake(&self, worker: usize) -> bool {
        let bed = &self.beds[worker];
        let mut asleep = bed.asleep.lock();
        let was = mem::replace(&mut *asleep, false);
        if was {
            bed.wake.notify_one();
        }
        was
    }

    /// Block `worker` until it is woken, unless a job was announced since
    /// `seen` was read or it is `done` waiting.
    fn park(&self, worker: usize, seen: u64, done: impl Fn() -> bool) {
        #[cfg(test)]
        self.parks.fetch_add(1, Ordering::Relaxed);
        let bed = &self.beds[worker];
        let mut asleep = bed.asleep.lock();
        self.sleepers.fetch_add(1, Ordering::SeqCst);
        if self.epoch.load(Ordering::SeqCst) == seen && !done() {
            *asleep = true;
            while *asleep {
                bed.wake.wait(&mut asleep)
            }
        }
        self.sleepers.fetch_sub(1, Ordering::SeqCst);
    }

    fn terminate(&self) {
        self.terminated.store(true, Ordering::SeqCst);
        self.epoch.fetch_add(1, Ordering::SeqCst);
        for worker in 0..self.beds.len() {
            self.wake(worker);
        }
    }
}

struct Registry {
    injector: Injector<JobRef>,
    stealers: Box<[Stealer<JobRef>]>,
    sleep: Sleep,
}

/// A worker's own state, on its stack while it runs.
struct WorkerThread {
    registry: Arc<Registry>,
    index: usize,
    local: Worker<JobRef>,
}

thread_local! {
    static WORKER: Cell<*const WorkerThread> = const { Cell::new(ptr::null()) };
}

impl WorkerThread {
    /// This thread's worker state, if it is a worker of `registry`.
    fn current(registry: &Registry) -> Option<&WorkerThread> {
        let w = WORKER.with(Cell::get);
        // SAFETY: a worker clears `WORKER` before its state is dropped.
        let w = unsafe { w.as_ref() }?;
        ptr::eq(&*w.registry, registry).then_some(w)
    }

    /// A job: this worker's newest, else the injector's oldest, else
    /// another worker's oldest.
    fn find(&self) -> Option<JobRef> {
        if let Some(job) = self.local.pop() {
            return Some(job);
        }
        let stealers = &self.registry.stealers;
        loop {
            let mut retry = false;
            let others =
                (1..stealers.len()).map(|k| &stealers[(self.index + k) % stealers.len()]);
            for steal in std::iter::once(self.registry.injector.steal())
                .chain(others.map(Stealer::steal))
            {
                match steal {
                    Steal::Success(job) => return Some(job),
                    Steal::Retry => retry = true,
                    Steal::Empty => (),
                }
            }
            if !retry {
                #[cfg(test)]
                self.registry.sleep.empty_searches.fetch_add(1, Ordering::Relaxed);
                return None;
            }
        }
    }

    /// Run jobs until `done`, parking whenever there is none to run.
    fn run_until(&self, done: impl Fn() -> bool) {
        let sleep = &self.registry.sleep;
        loop {
            let seen = sleep.epoch.load(Ordering::SeqCst);
            if done() {
                break;
            }
            match self.find() {
                Some(job) => job.run(),
                None => sleep.park(self.index, seen, &done),
            }
        }
    }
}

/// One worker, not yet running: a spawned thread calls [`WorkerStart::run`].
pub struct WorkerStart {
    registry: Arc<Registry>,
    index: usize,
    local: Worker<JobRef>,
}

impl WorkerStart {
    pub fn index(&self) -> usize {
        self.index
    }

    /// Run jobs until the pool is dropped.
    pub fn run(self) {
        let WorkerStart { registry, index, local } = self;
        let me = WorkerThread { registry, index, local };
        WORKER.with(|w| w.set(&me));
        let sleep = &me.registry.sleep;
        me.run_until(|| sleep.terminated.load(Ordering::SeqCst));
        WORKER.with(|w| w.set(ptr::null()));
    }
}

/// A fork's shared part: the jobs `1..n` of `f` run on the pool's
/// workers, each counting `pending` down; the worker that forked, `owner`,
/// waits for zero.
struct Fork<'a> {
    f: &'a (dyn Fn(usize) + Sync),
    pending: AtomicUsize,
    sleep: &'a Sleep,
    owner: usize,
    panic: Mutex<Option<Panic>>,
}

unsafe fn run_part(data: *const (), index: usize) {
    // SAFETY: the forking thread waits for `pending` to reach zero.
    let fork = unsafe { &*(data as *const Fork) };
    if let Err(p) = panic::catch_unwind(AssertUnwindSafe(|| (fork.f)(index))) {
        fork.panic.lock().get_or_insert(p);
    }
    // the fork may be gone once `pending` is zero; its pool, which this
    // worker belongs to, is not
    let (sleep, owner) = (fork.sleep, fork.owner);
    if fork.pending.fetch_sub(1, Ordering::Release) == 1 {
        sleep.wake(owner);
    }
}

/// A job handed to the pool from a thread outside it.
struct Install<F, T> {
    f: UnsafeCell<Option<F>>,
    out: UnsafeCell<Option<thread::Result<T>>>,
    done: AtomicBool,
    owner: Thread,
}

unsafe fn run_install<F: FnOnce() -> T, T>(data: *const (), _: usize) {
    // SAFETY: the installing thread waits for `done`, and only this job
    // touches `f` and `out` until then.
    let job = unsafe { &*(data as *const Install<F, T>) };
    let f = unsafe { (*job.f.get()).take() }.expect("an install runs once");
    let out = panic::catch_unwind(AssertUnwindSafe(f));
    unsafe { *job.out.get() = Some(out) };
    let owner = job.owner.clone();
    job.done.store(true, Ordering::Release);
    owner.unpark()
}

/// `n` items reached from many threads, each index by one only.
struct Items<T>(*mut T);

// SAFETY: every index is handed to one job, which alone touches it.
unsafe impl<T: Send> Sync for Items<T> {}

impl<T> Items<T> {
    /// SAFETY: `i` is in bounds and no other live reference reaches it.
    #[allow(clippy::mut_from_ref)]
    unsafe fn get(&self, i: usize) -> &mut T {
        unsafe { &mut *self.0.add(i) }
    }
}

/// A pool of evaluation workers. Dropping it ends them once they are
/// idle.
pub struct EvalPool {
    registry: Arc<Registry>,
}

impl EvalPool {
    /// A pool of `threads` workers, each started by `spawn`; the first
    /// error `spawn` returns ends the workers it started and is returned.
    pub fn build(
        threads: usize,
        mut spawn: impl FnMut(WorkerStart) -> io::Result<()>,
    ) -> io::Result<EvalPool> {
        let threads = threads.max(1);
        let mut locals: Vec<Worker<JobRef>> =
            (0..threads).map(|_| Worker::new_lifo()).collect();
        let registry = Arc::new(Registry {
            injector: Injector::new(),
            stealers: locals.iter().map(Worker::stealer).collect(),
            sleep: Sleep {
                beds: (0..threads).map(|_| Bed::default()).collect(),
                sleepers: AtomicUsize::new(0),
                epoch: AtomicU64::new(0),
                terminated: AtomicBool::new(false),
                #[cfg(test)]
                empty_searches: AtomicU64::new(0),
                #[cfg(test)]
                parks: AtomicU64::new(0),
            },
        });
        let pool = EvalPool { registry };
        for (index, local) in locals.drain(..).enumerate() {
            spawn(WorkerStart { registry: pool.registry.clone(), index, local })?
        }
        Ok(pool)
    }

    pub fn current_num_threads(&self) -> usize {
        self.registry.stealers.len()
    }

    /// Whether this thread is one of the pool's workers.
    pub fn on_worker(&self) -> bool {
        WorkerThread::current(&self.registry).is_some()
    }

    /// `f` on one of the pool's workers, or here when this thread is one.
    /// A panic in `f` resumes here.
    pub fn install<T: Send>(&self, f: impl FnOnce() -> T + Send) -> T {
        fn install<F: FnOnce() -> T + Send, T: Send>(pool: &EvalPool, f: F) -> T {
            let job = Install {
                f: UnsafeCell::new(Some(f)),
                out: UnsafeCell::new(None),
                done: AtomicBool::new(false),
                owner: thread::current(),
            };
            let data = &job as *const Install<F, T> as *const ();
            pool.registry.injector.push(JobRef {
                data,
                index: 0,
                run: run_install::<F, T>,
            });
            pool.registry.sleep.announce(1);
            while !job.done.load(Ordering::Acquire) {
                thread::park()
            }
            match job.out.into_inner().expect("the install ran") {
                Ok(t) => t,
                Err(p) => panic::resume_unwind(p),
            }
        }
        if self.on_worker() { f() } else { install(self, f) }
    }

    /// `f(0)` .. `f(n - 1)`, each a job of its own, `f(0)` on this thread
    /// (after moving onto a worker). This thread runs jobs, its own and
    /// others', until all `n` are done, then resumes the first panic.
    fn fork(&self, n: usize, f: &(dyn Fn(usize) + Sync)) {
        let Some(me) = WorkerThread::current(&self.registry) else {
            return self.install(|| self.fork(n, f));
        };
        if n == 0 {
            return;
        }
        let fork = Fork {
            f,
            pending: AtomicUsize::new(n - 1),
            sleep: &self.registry.sleep,
            owner: me.index,
            panic: Mutex::new(None),
        };
        let data = &fork as *const Fork as *const ();
        for index in (1..n).rev() {
            me.local.push(JobRef { data, index, run: run_part })
        }
        self.registry.sleep.announce(n - 1);
        let first = panic::catch_unwind(AssertUnwindSafe(|| f(0)));
        me.run_until(|| fork.pending.load(Ordering::Acquire) == 0);
        if let Err(p) = first {
            panic::resume_unwind(p)
        }
        if let Some(p) = fork.panic.into_inner() {
            panic::resume_unwind(p)
        }
    }

    /// `f` over each item, every item a job of its own.
    pub fn for_each<T: Send>(&self, items: &mut [T], f: impl Fn(&mut T) + Sync) {
        let base = Items(items.as_mut_ptr());
        // SAFETY: `fork` hands each index below the length to one job.
        self.fork(items.len(), &|i| f(unsafe { base.get(i) }))
    }

    /// `out` cleared and filled with `f` of each item in order, every item
    /// a job of its own.
    pub fn map<T: Send, U: Send>(
        &self,
        items: &mut [T],
        out: &mut Vec<U>,
        f: impl Fn(&mut T) -> U + Sync,
    ) {
        let n = items.len();
        out.clear();
        out.reserve(n);
        let base = Items(items.as_mut_ptr());
        let slots = Items(out.spare_capacity_mut().as_mut_ptr());
        // SAFETY: as in `for_each`; a slot is written once, by its item's
        // job, and the length is set only after every job wrote its slot.
        self.fork(n, &|i| unsafe {
            slots.get(i).write(f(base.get(i)));
        });
        unsafe { out.set_len(n) }
    }

    /// `a` and `b`, `b` a job of its own, `a` on this thread.
    pub fn join<A, B, RA, RB>(&self, a: A, b: B) -> (RA, RB)
    where
        A: FnOnce() -> RA + Send,
        B: FnOnce() -> RB + Send,
        RA: Send,
        RB: Send,
    {
        let (mut a, mut b) = (Some(a), Some(b));
        let (mut ra, mut rb) = (None, None);
        {
            let mut left = || ra = a.take().map(|a| a());
            let mut right = || rb = b.take().map(|b| b());
            let mut parts: [&mut (dyn FnMut() + Send); 2] = [&mut left, &mut right];
            self.for_each(&mut parts, |p| p());
        }
        (ra.expect("join ran its left side"), rb.expect("join ran its right side"))
    }
}

impl fmt::Debug for EvalPool {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("EvalPool").field("workers", &self.current_num_threads()).finish()
    }
}

impl Drop for EvalPool {
    fn drop(&mut self) {
        self.registry.sleep.terminate()
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::{sync::atomic::AtomicU32, time::Duration};

    fn pool(threads: usize) -> EvalPool {
        EvalPool::build(threads, |t| {
            thread::Builder::new().spawn(move || t.run()).map(mem::drop)
        })
        .unwrap()
    }

    fn fib(pool: &EvalPool, n: u64) -> u64 {
        if n < 2 {
            return n;
        }
        let (a, b) = pool.join(|| fib(pool, n - 1), || fib(pool, n - 2));
        a + b
    }

    #[test]
    fn nested_joins() {
        let pool = pool(4);
        assert_eq!(pool.install(|| fib(&pool, 20)), 6765);
        assert_eq!(fib(&pool, 15), 610);
    }

    #[test]
    fn each_item_once() {
        let pool = pool(3);
        let mut items: Vec<u32> = (0..1000).collect();
        pool.for_each(&mut items, |i| *i += 1);
        assert!(items.iter().enumerate().all(|(k, i)| *i == k as u32 + 1));
        let mut out = vec![7u64];
        pool.map(&mut items, &mut out, |i| *i as u64 * 2);
        assert_eq!(out.len(), 1000);
        assert!(out.iter().enumerate().all(|(k, o)| *o == 2 * (k as u64 + 1)));
        let mut none: [u32; 0] = [];
        pool.for_each(&mut none, |_| unreachable!());
    }

    #[test]
    fn parts_run_side_by_side() {
        let pool = pool(4);
        let arrived = AtomicU32::new(0);
        let mut parts = [(); 4];
        // each part waits for every other: only four threads at once finish
        pool.for_each(&mut parts, |_| {
            arrived.fetch_add(1, Ordering::SeqCst);
            while arrived.load(Ordering::SeqCst) < 4 {
                thread::yield_now()
            }
        });
    }

    #[test]
    fn a_panic_resumes_after_every_part() {
        let pool = pool(2);
        let ran = AtomicU32::new(0);
        let mut parts = [0u32, 1, 2, 3];
        let r = panic::catch_unwind(AssertUnwindSafe(|| {
            pool.for_each(&mut parts, |p| {
                thread::sleep(Duration::from_millis(5));
                ran.fetch_add(1, Ordering::SeqCst);
                if *p == 2 {
                    panic!("part 2")
                }
            })
        }));
        assert_eq!(ran.load(Ordering::SeqCst), 4);
        let p = r.unwrap_err();
        assert_eq!(p.downcast_ref::<&str>(), Some(&"part 2"));
        assert_eq!(pool.install(|| 5), 5, "the pool outlives a panic");
    }

    // a thread that finds no job parks at once: every empty search is
    // followed by a park, where a spinning pool searches dozens of times
    // before each
    #[test]
    fn idle_workers_park_at_once() {
        const WORKERS: usize = 8;
        let pool = pool(WORKERS);
        for _ in 0..200 {
            let mut parts = [0u8; 4];
            pool.for_each(&mut parts, |p| *p += 1);
        }
        let sleep = &pool.registry.sleep;
        while !sleep.beds.iter().all(|b| *b.asleep.lock()) {
            thread::sleep(Duration::from_millis(1));
        }
        let empty = sleep.empty_searches.load(Ordering::Relaxed);
        let parks = sleep.parks.load(Ordering::Relaxed);
        assert!(empty > 0);
        assert_eq!(empty, parks, "{empty} empty searches, {parks} parks");
    }

    #[test]
    fn a_failed_spawn_ends_the_started_workers() {
        let mut started = Vec::new();
        let r = EvalPool::build(4, |t| {
            if t.index() == 2 {
                return Err(io::ErrorKind::WouldBlock.into());
            }
            started.push(thread::Builder::new().spawn(move || t.run())?);
            Ok(())
        });
        assert!(r.is_err());
        assert_eq!(started.len(), 2);
        for t in started {
            t.join().unwrap()
        }
    }
}
