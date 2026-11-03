//! A collection callback that calls the recursive function whose body
//! holds it is that function's self-call, made from a loop slot: each
//! slot roots an activation of its own (`design/kernel_instance_state.md`,
//! slot-root chains).

use super::lift::{assert_stream, assert_values, collect_n_blocks};
use anyhow::{Result, bail};
use graphix_package_core::{
    run,
    testing::{FuseExpect, is, prints},
};
use netidx_value::Value;

run!(callback_self_call, r#"
{
    let rec k = |n: i64| -> i64 select n { 0 => 0, _ => n + array::fold([n - 1], 0, |a, x| a + k(x)) };
    #[native] k(4)
}
"#, is(Value::I64(10)); FuseExpect::Jit);

run!(callback_self_call_from_a_collection_callback, r#"
{
    let rec k = |n: i64| -> i64 select n { 0 => 0, _ => n + array::fold([n - 1], 0, |a, x| a + k(x)) };
    #[native] array::map([1, 2, 4], k)
}
"#, prints("[i64:1, i64:3, i64:10]"); FuseExpect::Jit);

// depth 20, breadth 3: one child goes on down, two are leaves
run!(tree_recursion_through_map, r#"
{
    let rec t = |d: i64| -> i64 select d {
        0 => 1,
        _ => 1 + array::fold(array::map([d - 1, 0, 0], |c| t(c)), 0, |a, x| a + x)
    };
    #[native] t(20)
}
"#, is(Value::I64(61)); FuseExpect::Jit);

run!(tree_recursion_through_fold, r#"
{
    let rec t = |d: i64| -> i64 select d {
        0 => 1,
        _ => array::fold([d - 1, 0, 0], 1, |a, c| a + t(c))
    };
    #[native] t(20)
}
"#, is(Value::I64(61)); FuseExpect::Jit);

// every node of a ternary tree six deep, through two loops
run!(full_tree_recursion, r#"
{
    let rec t = |d: i64| -> i64 select d {
        0 => 1,
        _ => 1 + array::fold(array::map([d - 1, d - 1, d - 1], |c| t(c)), 0, |a, x| a + x)
    };
    #[native] t(6)
}
"#, is(Value::I64(1093)); FuseExpect::Jit);

// The recursion reads a binding from outside it, which every self-call
// in a loop's slot forwards: through each loop that runs as chunks.
run!(capturing_tree_recursion_through_map, r#"
{
    let y = 2;
    let rec t = |d: i64| -> i64 select d {
        0 => y,
        _ => array::fold(array::map([d - 1, d - 1], |c| t(c)), 0, |a, x| a + x)
    };
    #[native] t(3)
}
"#, is(Value::I64(16)); FuseExpect::Jit);

run!(capturing_tree_recursion_through_filter, r#"
{
    let y = 2;
    let rec t = |d: i64| -> i64 select d {
        0 => y,
        _ => d * y + array::len(array::filter([d - 1, d - 1], |c| t(c) > d))
    };
    #[native] t(3)
}
"#, is(Value::I64(8)); FuseExpect::Jit);

run!(capturing_tree_recursion_through_filter_map, r#"
{
    let y = 2;
    let rec t = |d: i64| -> i64 select d {
        0 => y,
        _ => array::fold(
            array::filter_map([d - 1, d - 1, 0 - 1], |c| select c >= 0 {
                true => `Keep(t(c)),
                false => null
            }),
            0,
            |a, x| a + x
        )
    };
    #[native] t(3)
}
"#, is(Value::I64(16)); FuseExpect::Jit);

run!(capturing_tree_recursion_through_flat_map, r#"
{
    let y = 2;
    let rec t = |d: i64| -> i64 select d {
        0 => y,
        _ => array::fold(array::flat_map([d - 1, d - 1], |c| [t(c), 1]), 0, |a, x| a + x)
    };
    #[native] t(3)
}
"#, is(Value::I64(30)); FuseExpect::Jit);

// the self-call is in a loop the chunk's own callback runs
run!(capturing_tree_recursion_through_nested_maps, r#"
{
    let y = 2;
    let rec t = |d: i64| -> i64 select d {
        0 => y,
        _ => array::fold(
            array::map([d - 1, d - 1], |c| array::fold(array::map([c], |e| t(e)), 0, |a, x| a + x)),
            0,
            |a, x| a + x
        )
    };
    #[native] t(3)
}
"#, is(Value::I64(16)); FuseExpect::Jit);

/// The captured binding fires: every activation reads its new value.
#[tokio::test(flavor = "current_thread")]
async fn capturing_tree_recursion_follows_its_capture() -> Result<()> {
    let code = "{ let y = array::iter([1, 2, 3]); \
                let rec t = |d: i64| -> i64 select d { 0 => y, _ => \
                  array::fold(array::map([d - 1, d - 1], |c| t(c)), 0, |a, x| a + x) }; \
                #[native] t(3) }";
    assert_stream(code, &[8, 16, 24]).await
}

// the recursion's tail is a call of its own; the callback's is not
run!(callback_self_call_beside_a_tail_call, r#"
{
    let rec k = |n: i64, acc: i64| -> i64 select n {
        0 => acc,
        _ => k(n - 1, acc + n + array::fold([n - 1], 0, |a, x| a + k(x, 0)))
    };
    #[native] k(5, 0)
}
"#, is(Value::I64(57)); FuseExpect::Jit);

/// The fused recursion is under an arm that sleeps every other value.
#[tokio::test(flavor = "current_thread")]
async fn callback_recursion_sleeps_and_wakes() -> Result<()> {
    let code = "{ let rec k = |n: i64| -> i64 select n { 0 => 0, _ => \
                  n + array::fold([n - 1], 0, |a, x| a + k(x)) }; \
                let ticks = array::iter([0, 1, 2, 3, 4]); \
                select ticks % 2 { 0 => #[native] k(ticks + 2), _ => 0 - 1 } }";
    assert_stream(code, &[3, -1, 10, -1, 21]).await
}

/// Slots of the loop that calls the recursion are deleted and made again.
#[tokio::test(flavor = "current_thread")]
async fn callback_recursion_slots_deleted() -> Result<()> {
    let code = "{ let rec k = |n: i64| -> i64 select n { 0 => 0, _ => \
                  n + array::fold([n - 1], 0, |a, x| a + k(x)) }; \
                let src = array::iter([[3, 4, 5], [3], [2, 6]]); \
                #[native] array::map(src, k) }";
    assert_values(code, &["[i64:6, i64:10, i64:15]", "[i64:6]", "[i64:3, i64:21]"]).await
}

/// The slots that root child activations are deleted and made again,
/// at every depth.
const SHRINKING_CHILDREN: &str = "{ let rec f = |n: i64| -> i64 select n { 0 => 1, _ => \
      1 + array::fold(array::filter([n - 1, n - 2], |c| c >= 0), 0, |a, c| a + f(c)) }; \
    let n = array::iter([NS]); \
    #[native] f(n) }";

#[tokio::test(flavor = "current_thread")]
async fn callback_recursion_children_deleted() -> Result<()> {
    let code = SHRINKING_CHILDREN.replace("NS", "5, 1, 5, 7, 2");
    assert_stream(&code, &[20, 2, 20, 54, 4]).await
}

/// A recursion whose children shrink frees the activations of the slots
/// it deleted: 231 live below the root of `f(10)`, one once `f(1)`
/// follows it.
#[tokio::test(flavor = "current_thread")]
async fn callback_recursion_frees_deleted_slots() -> Result<()> {
    let deep = collect_n_blocks(&SHRINKING_CHILDREN.replace("NS", "10"), 1).await?;
    let shallow = collect_n_blocks(&SHRINKING_CHILDREN.replace("NS", "10, 1"), 2).await?;
    if deep[..] != [231] || shallow[1] != 1 {
        bail!("live SelfBlocks {deep:?} then {shallow:?}, expected [231] then [_, 1]")
    }
    Ok(())
}
