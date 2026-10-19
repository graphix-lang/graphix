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
    const BASE: u64 = 1 << 62;
    for stride in [1u64, 3, 4, 8, 16] {
        for n in [300usize, 1000, 3000, 10000, 30000] {
            // present keys: every stride-th id; absent: the ids between
            let keys: Vec<u64> = (0..n as u64).map(|i| BASE + i * stride).collect();
            let absent: Vec<u64> = (0..n as u64).map(|i| BASE + i * stride + stride / 2 + n as u64 * stride).collect();
            let a = bench::<Id>(&keys, &absent);
            let b = bench::<Mix>(&keys, &absent);
            println!("stride={stride:2} n={n:6} identity={a:7.1} ns/op  mixed={b:5.1} ns/op  ratio={:5.1}", a / b);
        }
    }
}
