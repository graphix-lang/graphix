// Walk pruning (design/walk_pruning.md): a boundary nothing it reads
// delivered to is not walked, and the program computes what the unpruned
// node-walk does.

use anyhow::Result;
use graphix_package_core::testing::{
    Mode, compile_result, data, fixture_runtime, result_source, updates_until_quiet,
};
use netidx::publisher::Value;
use std::time::Duration;
use tokio::time::Instant;

/// Every update of `code`'s result and the boundaries walked, in `mode`.
async fn walks(code: &str, mode: Mode) -> Result<(Vec<Value>, u64)> {
    let (ctx, mut rx) = fixture_runtime(
        [("/test.gx", result_source(code))],
        &crate::TEST_REGISTER,
        mode,
        |_| (),
    )
    .await?;
    let res = compile_result(&ctx).await?;
    let deadline = Instant::now() + Duration::from_secs(20);
    let values = updates_until_quiet(
        &ctx.rt,
        &mut rx,
        res.exprs[0].id,
        Duration::from_millis(300),
        deadline,
    )
    .await?;
    let (walked, _) = ctx.rt.control().boundaries();
    ctx.shutdown().await;
    Ok((values, walked))
}

/// An idle map of `size` slots beside a counter ticking 100 times.
fn idle_map(size: usize) -> String {
    format!(
        "{{
  let n = 0;
  select n {{ x if x < 100 => n <- (x ~ n) + 1, _ => never() }};
  let m = array::map(array::init({size}, |i| i), |x| once(x) * 2);
  (n, array::len(m))
}}"
    )
}

// The counter's ticks walk none of the idle map's slots: the walks a
// thousand more slots cost are their first ones, where unpruned every
// tick walks them all again.
#[tokio::test(flavor = "multi_thread")]
async fn idle_map_beside_a_ticking_counter() -> Result<()> {
    let (small, small_walks) = walks(&idle_map(10), Mode::Interp).await?;
    let (big, big_walks) = walks(&idle_map(1010), Mode::Interp).await?;
    let (unpruned, unpruned_walks) = walks(&idle_map(1010), Mode::Unpruned).await?;
    let expected: Vec<Value> =
        (0..=100).map(|n| Value::from(vec![Value::I64(n), Value::I64(1010)])).collect();
    assert_eq!(big.into_iter().map(data).collect::<Vec<_>>(), expected);
    assert_eq!(unpruned.into_iter().map(data).collect::<Vec<_>>(), expected);
    assert_eq!(small.len(), expected.len());
    assert!(
        big_walks - small_walks <= 3 * 1000,
        "pruned: {small_walks} walks at 10 slots, {big_walks} at 1010"
    );
    assert!(
        unpruned_walks - small_walks >= 100 * 1000,
        "unpruned: {unpruned_walks} walks at 1010 slots"
    );
    Ok(())
}
