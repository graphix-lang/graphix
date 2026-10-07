// Fused loops whose slots run as chunks of their kernel
// (design/parallel_eval.md §10): every fixture also runs `jit_par`,
// where each slot of a loop of two or more is a chunk of its own.

use anyhow::Result;
use graphix_package_core::{run, testing::FuseExpect};
use netidx::publisher::Value;

// Chunks queue their raises apart; the kernel delivers them in slot
// order, as the serial loop raises them.
const CHUNK_RAISES_IN_SLOT_ORDER: &str = r#"
{
  let log: Array<i64> = [];
  let out = {
    catch(e) log <- e ~ array::push(log, select (e.0).error { `Bad(n) => n });
    array::map([1, 2, 3, 4, 5, 6, 7, 8, 9], |x| {
      let r: [i64, Error<`Bad(i64)>] = select x % 3 { 0 => error(`Bad(x)), _ => x };
      r? * 10
    })
  };
  select array::len(log) { 3 => log, _ => never() }
}
"#;

run!(chunk_raises_in_slot_order, CHUNK_RAISES_IN_SLOT_ORDER, |v: Result<&Value>| {
    matches!(v, Ok(Value::Array(a))
        if a.iter().cloned().collect::<Vec<_>>()
            == [Value::I64(3), Value::I64(6), Value::I64(9)])
}; FuseExpect::Jit);

// A find takes the lowest slot's match whichever chunk finds one first;
// the filter family concatenates its chunks in slot order.
const CHUNK_FIND_TAKES_LOWEST: &str = r#"
{
  let xs = [1, 2, 8, 3, 9, 7];
  let f = array::find(xs, |x| x > 5)$;
  let g = array::find_map(xs, |x| select x > 6 { true => x * 100, false => null })$;
  let h = array::filter_map(xs, |x| select x > 2 { true => x, false => null });
  let k = array::flat_map(xs, |x| [x, -x]);
  "[f] [g] [h] [k]"
}
"#;

run!(chunk_find_takes_lowest, CHUNK_FIND_TAKES_LOWEST, |v: Result<&Value>| {
    let want = "8 800 [8, 3, 9, 7] [1, -1, 2, -2, 8, -8, 3, -3, 9, -9, 7, -7]";
    matches!(v, Ok(Value::String(s)) if s == want)
}; FuseExpect::Jit);
