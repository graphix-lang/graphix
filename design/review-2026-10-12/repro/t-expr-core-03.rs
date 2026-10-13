// t-expr-core-03: a nested GxAbstract decode re-takes netidx's REGISTRY
// read lock while holding it; a concurrent Abstract::register (a writer)
// queued between the two reads deadlocks the decoding thread.
// Run: timeout -s KILL 2400 cargo test -p graphix-tests --test review_t_expr_core_03 -- --nocapture
// (copy to stdlib/graphix-tests/tests/ first)
// Expected: both runs keep making progress.
// Observed: "no deadlock in 10s (depth 1): 4417211 decodes, 38776936 register calls"
// then "DEADLOCK (depth 64): decodes stuck at 0->0, register calls 1024->1024":
// the decoder and the registering thread are both blocked for good.
use arcstr::literal;
use graphix_compiler::{
    abstract_value::{self, GxAbstract},
    expr::ModPath,
    typ::AbstractId,
};
use netidx_core::pack::Pack;
use netidx_value::{Abstract, Value};
use std::{
    sync::{
        Arc,
        atomic::{AtomicBool, AtomicUsize, Ordering},
    },
    thread,
    time::{Duration, Instant},
};

#[test]
fn nested_abstract_decode_vs_register() {
    // control: one level, no read lock taken twice, must not stall
    run(1);
    thread::sleep(Duration::from_millis(500));
    run(64)
}

fn run(depth: usize) {
    let id = AbstractId::of(&ModPath::root(), "Nest");
    let mut v = Value::I64(0);
    for _ in 0..depth {
        v = abstract_value::wrap(id, literal!("Nest"), Vec::<graphix_compiler::typ::Type>::new().into(), v);
    }
    let uuid = match &v {
        Value::Abstract(a) => a.id(),
        _ => unreachable!(),
    };
    let mut buf = bytes::BytesMut::new();
    v.encode(&mut buf).unwrap();
    let bytes = buf.freeze();
    let done = Arc::new(AtomicBool::new(false));
    let progress = Arc::new(AtomicUsize::new(0));
    {
        let (done, progress, bytes) = (done.clone(), progress.clone(), bytes.clone());
        thread::spawn(move || {
            while !done.load(Ordering::Relaxed) {
                let mut b = bytes.clone();
                let d = Value::decode(&mut b).unwrap();
                assert!(matches!(d, Value::Abstract(_)));
                progress.fetch_add(1, Ordering::Relaxed);
            }
        });
    }
    let writes = Arc::new(AtomicUsize::new(0));
    {
        let (done, writes) = (done.clone(), writes.clone());
        thread::spawn(move || {
            while !done.load(Ordering::Relaxed) {
                Abstract::register::<GxAbstract>(uuid).unwrap();
                writes.fetch_add(1, Ordering::Relaxed);
            }
        });
    }
    let start = Instant::now();
    let limit = Duration::from_secs(10);
    let mut last = 0;
    let mut stuck_since = Instant::now();
    while start.elapsed() < limit {
        thread::sleep(Duration::from_millis(250));
        let p = progress.load(Ordering::Relaxed);
        if p != last {
            last = p;
            stuck_since = Instant::now();
        } else if stuck_since.elapsed() > Duration::from_secs(3) {
            let w0 = writes.load(Ordering::Relaxed);
            thread::sleep(Duration::from_secs(1));
            let w1 = writes.load(Ordering::Relaxed);
            let p1 = progress.load(Ordering::Relaxed);
            eprintln!(
                "DEADLOCK (depth {depth}): decodes stuck at {p}->{p1}, register calls {w0}->{w1}"
            );
            panic!("DEADLOCK: decode thread stuck after {p} decodes");
        }
    }
    done.store(true, Ordering::Relaxed);
    eprintln!("no deadlock in 10s (depth {depth}): {last} decodes, {} register calls", writes.load(Ordering::Relaxed));
}
