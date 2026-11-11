//! The evaluation pool: work-stealing workers, each parking the moment it
//! finds no job, so beyond the jobs they run a fork costs its workers a
//! wake and a park for each one called to search (`design/parallel_eval.md`
//! §9).

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

/// A worker's state in its bed.
#[derive(Clone, Copy, PartialEq, Eq, Default)]
enum Rest {
    #[default]
    Awake,
    Asleep,
    /// Woken by an announcement to search, and counted in
    /// [`Sleep::searching`] until that search ends.
    Called,
}

/// Where one worker blocks.
#[derive(Default)]
struct Bed {
    rest: Mutex<Rest>,
    wake: Condvar,
}

/// Workers with no job to run block here, each in its own bed, until a
/// job is announced or, for one waiting on its fork, the fork is done.
struct Sleep {
    beds: Box<[Bed]>,
    sleepers: AtomicUsize,
    /// Workers called whose search has not ended: an announcement calls
    /// sleepers only for the jobs beyond them. A job announced while one
    /// is counted is found by its search, by the search the epoch sends it
    /// on again, or by the worker it calls if it ends busy and the last.
    searching: AtomicUsize,
    /// Bumped by every announcement: a worker that read it before a
    /// search that found nothing sleeps only if it has not moved since.
    epoch: AtomicU64,
    terminated: AtomicBool,
    /// Searches that found no job, parks (one a search), and workers
    /// called.
    #[cfg(test)]
    empty_searches: AtomicU64,
    #[cfg(test)]
    parks: AtomicU64,
    #[cfg(test)]
    calls: AtomicU64,
}

impl Sleep {
    fn new(workers: usize) -> Self {
        Sleep {
            beds: (0..workers).map(|_| Bed::default()).collect(),
            sleepers: AtomicUsize::new(0),
            searching: AtomicUsize::new(0),
            epoch: AtomicU64::new(0),
            terminated: AtomicBool::new(false),
            #[cfg(test)]
            empty_searches: AtomicU64::new(0),
            #[cfg(test)]
            parks: AtomicU64::new(0),
            #[cfg(test)]
            calls: AtomicU64::new(0),
        }
    }

    // XCR claude for claude: [perf/low] Every fork wakes as many parked workers as it pushed jobs,
    // and in a 2-part join the owner usually pops the one job itself first, so the woken worker
    // searches once, finds nothing and parks again: a futex wake, a context switch and a park per
    // join. Under GRAPHIX_PAR=force with many tiny forks (fh300.gx, --no-fusion) wall time is about
    // 1.5x rayon's at 16 and 32 workers (3.3 s vs 2.2 s at 32), equal at 2-8 and under Auto; announce
    // also locks each bed in turn. A fix: wake one sleeper per announce and let a worker that finds a
    // job wake the next while more remain, or keep an atomic mask of asleep beds; then re-measure and
    // correct parallel_eval.md section 9's fork-cost sentence. probe:
    // ~/ptmp/cr29/scratch/r19/coord/runtime/fh300.gx (eval-pool-wake-batch-01)
    // 2026-11-10 claude: An announcement now calls sleepers only for the jobs beyond the workers
    // already called whose search has not ended (Sleep::searching, Rest::Called); a called worker
    // that finds a job, or returns to its own work, calls another unless one still searches
    // (Sleep::searched), so tiny joins call one worker at a time. fh300.gx force --no-fusion against
    // the lane before the fix: 32 workers 3.7-3.9M context switches, 30.5-31.3 s task-clock, 65-67G
    // instr -> 0.37-0.41M, 14.0-14.9 s, 50G; 16 workers 2.0-2.5M -> 0.40-0.50M; 2 workers equal
    // within noise. The bed scan was left: Sleep::wake is 0.33% of samples after (3.97% before).
    // Pins: eval_pool::test::a_searching_worker_answers_for_one_job and
    // tiny_joins_call_one_searcher_at_a_time fail with an announcement calling one per job (4000
    // joins: 4001 calls, 4484 parks; fixed 164-460 calls, 302-801 parks over 16 runs);
    // joins_announced_while_others_search_run_side_by_side hangs with the hand-off in Sleep::searched
    // removed. parallel_eval.md sections 5 and 9 restated.
    /// `jobs` new jobs are visible.
    fn announce(&self, jobs: usize) {
        self.epoch.fetch_add(1, Ordering::SeqCst);
        self.call(jobs)
    }

    /// Call a sleeper to search for each of `jobs` beyond those the
    /// workers searching cover.
    fn call(&self, jobs: usize) {
        if self.sleepers.load(Ordering::SeqCst) == 0 {
            return;
        }
        let mut jobs = jobs.saturating_sub(self.searching.load(Ordering::SeqCst));
        for worker in 0..self.beds.len() {
            if jobs == 0 {
                break;
            }
            if self.wake(worker, true) {
                jobs -= 1;
            }
        }
    }

    /// A called worker's search ended. One `busy` with what it found, or
    /// with its own work, calls another in its place unless one still
    /// searches.
    fn searched(&self, busy: bool) {
        self.searching.fetch_sub(1, Ordering::SeqCst);
        if busy {
            self.call(1)
        }
    }

    /// Wake `worker` if it is asleep, `called` to search; whether it was.
    fn wake(&self, worker: usize, called: bool) -> bool {
        let bed = &self.beds[worker];
        let mut rest = bed.rest.lock();
        let was = *rest == Rest::Asleep;
        if was {
            *rest = if called {
                #[cfg(test)]
                self.calls.fetch_add(1, Ordering::Relaxed);
                self.searching.fetch_add(1, Ordering::SeqCst);
                Rest::Called
            } else {
                Rest::Awake
            };
            bed.wake.notify_one();
        }
        was
    }

    /// Block `worker` until it is woken, unless a job was announced since
    /// `seen` was read or it is `done` waiting; whether it was called.
    fn park(&self, worker: usize, seen: u64, done: impl Fn() -> bool) -> bool {
        #[cfg(test)]
        self.parks.fetch_add(1, Ordering::Relaxed);
        let bed = &self.beds[worker];
        let mut rest = bed.rest.lock();
        self.sleepers.fetch_add(1, Ordering::SeqCst);
        if self.epoch.load(Ordering::SeqCst) == seen && !done() {
            *rest = Rest::Asleep;
            while *rest == Rest::Asleep {
                bed.wake.wait(&mut rest)
            }
        }
        self.sleepers.fetch_sub(1, Ordering::SeqCst);
        mem::take(&mut *rest) == Rest::Called
    }

    fn terminate(&self) {
        self.terminated.store(true, Ordering::SeqCst);
        self.epoch.fetch_add(1, Ordering::SeqCst);
        for worker in 0..self.beds.len() {
            self.wake(worker, false);
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
        let mut called = false;
        loop {
            let seen = sleep.epoch.load(Ordering::SeqCst);
            if done() {
                break;
            }
            let job = self.find();
            if mem::take(&mut called) {
                sleep.searched(job.is_some())
            }
            match job {
                Some(job) => job.run(),
                None => called = sleep.park(self.index, seen, &done),
            }
        }
        if called {
            sleep.searched(true)
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
        sleep.wake(owner, false);
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
            sleep: Sleep::new(threads),
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
        let sleep = all_asleep(&pool);
        let empty = sleep.empty_searches.load(Ordering::Relaxed);
        let parks = sleep.parks.load(Ordering::Relaxed);
        assert!(empty > 0);
        assert_eq!(empty, parks, "{empty} empty searches, {parks} parks");
    }

    fn all_asleep(pool: &EvalPool) -> &Sleep {
        let sleep = &pool.registry.sleep;
        while !sleep.beds.iter().all(|b| *b.rest.lock() == Rest::Asleep) {
            thread::sleep(Duration::from_millis(1));
        }
        sleep
    }

    #[test]
    fn a_searching_worker_answers_for_one_job() {
        let sleep = Sleep::new(4);
        for bed in sleep.beds.iter() {
            *bed.rest.lock() = Rest::Asleep
        }
        sleep.sleepers.store(4, Ordering::SeqCst);
        let calls = || sleep.calls.load(Ordering::Relaxed);
        sleep.announce(1);
        assert_eq!(calls(), 1);
        sleep.announce(1);
        assert_eq!(calls(), 1, "the worker called searches for the second job too");
        sleep.announce(3);
        assert_eq!(calls(), 3);
        sleep.searched(false);
        sleep.searched(true);
        assert_eq!(calls(), 3, "a searcher is left");
        sleep.searched(true);
        assert_eq!(calls(), 4, "the last searcher, busy, calls one in its place");
    }

    // each owner waits in its left side for every right side to run beside
    // it, so a right side announced while the workers called were searching
    // must still be searched for
    #[test]
    fn joins_announced_while_others_search_run_side_by_side() {
        const PARTS: usize = 4;
        let pool = pool(2 * PARTS);
        let arrived = AtomicU32::new(0);
        let meet = || {
            arrived.fetch_add(1, Ordering::SeqCst);
            while arrived.load(Ordering::SeqCst) < 2 * PARTS as u32 {
                thread::yield_now()
            }
        };
        for _ in 0..20 {
            all_asleep(&pool);
            arrived.store(0, Ordering::SeqCst);
            let mut parts = [(); PARTS];
            pool.for_each(&mut parts, |_| {
                pool.join(meet, meet);
            });
        }
    }

    // a busy owner's tiny joins, each of whose one job it runs itself,
    // call one searcher at a time, not a sleeper a join
    #[test]
    fn tiny_joins_call_one_searcher_at_a_time() {
        const JOINS: u64 = 4000;
        let pool = pool(16);
        let parks = all_asleep(&pool).parks.load(Ordering::Relaxed);
        pool.install(|| {
            for _ in 0..JOINS {
                pool.join(|| (), || ());
            }
        });
        let sleep = all_asleep(&pool);
        let parks = sleep.parks.load(Ordering::Relaxed) - parks;
        let calls = sleep.calls.load(Ordering::Relaxed);
        assert!(parks < JOINS / 2, "{JOINS} joins: {parks} parks, {calls} calls");
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
