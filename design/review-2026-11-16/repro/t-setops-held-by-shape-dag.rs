// Review 2026-11-16, t-setops: `setops::held_by_shape` (`Held::of`) walks a
// pair of types as trees: where the subtrahend holds the minuend by shape
// without being identical to it, each shared node of a DAG is walked once
// per path, so `Type::diff` of two tuple types of depth d that share their
// parts costs 2^d.
//
// Command (from the repo root, with this file copied to
// graphix-types/tests/review_t_setops_2.rs):
//   cargo test -p graphix-types --test review_t_setops_2 -- --nocapture
//
// Expected: the time of one difference grows with d, the number of nodes.
// Observed (HEAD e1be2fbf2, debug profile): it doubles with each level:
//   depth 12: held 624.695µs, apart at the last position 614.755µs
//   depth 14: held 2.503352ms, apart at the last position 2.402947ms
//   depth 16: held 9.747074ms, apart at the last position 10.450423ms
//   depth 18: held 38.917714ms, apart at the last position 39.187365ms
//   depth 20: held 181.781364ms, apart at the last position 164.959173ms
//   depth 22: held 635.196087ms, apart at the last position 622.944734ms
// and the test's last assertion fails.
use graphix_types::{env::Env, expr::parser::parse_type, typ::Type};
use std::time::Instant;
use triomphe::Arc;

/// `leaf` paired with itself `depth` times, each level one shared node.
fn dag(leaf: &str, depth: usize) -> Type {
    let mut t = parse_type(leaf).unwrap();
    for _ in 0..depth {
        t = Type::Tuple(Arc::from_iter([t.clone(), t.clone()]));
    }
    t
}

#[test]
fn diff_of_two_tuple_dags_is_linear_in_their_nodes() {
    let env = Env::default();
    let tail = |t: Type, last: &str| {
        Type::Tuple(Arc::from_iter([t, parse_type(last).unwrap()]))
    };
    let mut times = vec![];
    for depth in [12, 14, 16, 18, 20, 22] {
        let narrow = dag("(i64, i64)", depth);
        let wide = dag("([i64, null], i64)", depth);
        let start = Instant::now();
        let held = narrow.diff(&env, &wide).unwrap();
        let whole = start.elapsed();
        assert!(held.is_uninhabited());
        let (m, t) = (tail(narrow, "i64"), tail(wide, "null"));
        let start = Instant::now();
        let d = m.diff(&env, &t).unwrap();
        let part = start.elapsed();
        assert!(!d.is_uninhabited());
        println!("depth {depth}: held {whole:?}, apart at the last position {part:?}");
        times.push((depth, whole));
    }
    let (first, last) = (times[0].1.as_secs_f64(), times[times.len() - 1].1.as_secs_f64());
    assert!(last < first * 64.0, "depth 12 took {first}s, depth 22 took {last}s");
}
