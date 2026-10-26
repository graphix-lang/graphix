// Title: pointer-keyed nohash maps share x-perf-runtime-02's dead control tag (x-perf-runtime-02 variant)
// Command: rustc -O -o <scratch>/ptrhash design/review-2026-10-26/repro/x-rerun-5-ptr-idhash-bench.rs && <scratch>/ptrhash
// Expected: identity within ~1.5x of mixed, as image_id! keys now are.
// Observed: identity 2.6-5.7 ns/op vs mixed 0.8-1.1 ns/op (ratio 2.3-6.2) for real heap
//   pointers (32..200-byte allocations, n = 300..30000). A user-space pointer is below
//   2^47, so the 7 top bits hashbrown takes as the control tag are 0 for every key, and
//   its alignment zeroes the low bits the bucket index takes.
// x-perf-runtime: identity-hashed ids under std HashMap (nohash::IntMap) are 3-68x slower
// than the same keys under a multiplicative mix. Every minted id is 1<<62 + n, so the
// top 7 bits, which hashbrown uses as the control-byte tag, are the same for every key:
// the SIMD tag match filters nothing and each probe compares keys one by one.
// Command: rustc -O -o /tmp/idhash design/review-2026-10-19/repro/x-perf-runtime-idhash-bench.rs && /tmp/idhash
// Expected: identity within ~1.5x of mixed. Observed: ratio 3-11x at strides 3..16,
// up to 68x at stride 1 n=3000/10000 (identity 68-71 ns/op, mixed 1.0-1.1 ns/op).
// Identity-hashed u64 ids above 1<<62 (graphix's minted BindIds under
// nohash::IntMap) vs the same keys under a multiplicative mix.
use std::collections::HashMap;
use std::hash::{BuildHasherDefault, Hasher};
use std::time::Instant;

#[derive(Default)]
struct Id(u64);
impl Hasher for Id {
    fn write(&mut self, _: &[u8]) { unreachable!() }
    fn write_u64(&mut self, n: u64) { self.0 = n }
    fn finish(&self) -> u64 { self.0 }
}
#[derive(Default)]
struct Mix(u64);
impl Hasher for Mix {
    fn write(&mut self, _: &[u8]) { unreachable!() }
    fn write_u64(&mut self, n: u64) { self.0 = n.wrapping_mul(0x9E3779B97F4A7C15) }
    fn finish(&self) -> u64 { self.0 }
}

fn bench<H: Hasher + Default>(keys: &[u64], absent: &[u64]) -> f64 {
    let mut m: HashMap<u64, u64, BuildHasherDefault<H>> = HashMap::default();
    for k in keys { m.insert(*k, *k); }
    let rounds = 200;
    let t = Instant::now();
    let mut acc = 0u64;
    for _ in 0..rounds {
        for k in keys { acc = acc.wrapping_add(*m.get(k).unwrap()); }
        for k in absent { if m.contains_key(k) { acc += 1 } }
    }
    std::hint::black_box(acc);
    t.elapsed().as_nanos() as f64 / (rounds * (keys.len() + absent.len())) as f64
}

fn main() {
    for size in [32usize, 64, 200] {
        for n in [300usize, 1000, 3000, 10000, 30000] {
            let boxes: Vec<Box<[u8]>> = (0..2 * n).map(|_| vec![0u8; size].into_boxed_slice()).collect();
            let ptrs: Vec<u64> = boxes.iter().map(|b| b.as_ptr() as u64).collect();
            let keys: Vec<u64> = ptrs.iter().step_by(2).copied().collect();
            let absent: Vec<u64> = ptrs.iter().skip(1).step_by(2).copied().collect();
            let a = bench::<Id>(&keys, &absent);
            let b = bench::<Mix>(&keys, &absent);
            println!("ptr alloc={size:3} n={n:6} identity={a:7.1} ns/op  mixed={b:5.1} ns/op  ratio={:5.1}", a / b);
            std::hint::black_box(&boxes);
        }
    }
}
