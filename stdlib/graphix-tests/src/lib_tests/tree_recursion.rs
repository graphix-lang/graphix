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

// A collection whose callback is the recursion's own name: each slot
// makes the recursion's call over its element, as `|c| t(c)` does.
run!(bare_tree_recursion_through_map, r#"
{
    let rec t = |d: i64| -> i64 select d {
        0 => 1,
        _ => 1 + array::fold(array::map([d - 1, 0, 0], t), 0, |a, x| a + x)
    };
    #[native] t(20)
}
"#, is(Value::I64(61)); FuseExpect::Jit);

run!(bare_tree_recursion_through_fold, r#"
{
    let rec t = |a: i64, d: i64| -> i64 select d {
        0 => a + 1,
        _ => array::fold([d - 1, 0, 0], a + 1, t)
    };
    #[native] t(0, 20)
}
"#, is(Value::I64(61)); FuseExpect::Jit);

run!(bare_tree_recursion_through_filter, r#"
{
    let rec t = |d: i64| -> bool select d {
        0 => true,
        _ => array::len(array::filter([d - 1, d - 1, 0], t)) != 3
    };
    #[native] [t(9), t(10)]
}
"#, prints("[false, true]"); FuseExpect::Jit);

run!(bare_tree_recursion_through_flat_map, r#"
{
    let rec t = |d: i64| -> Array<i64> select d {
        0 => [0],
        1 => [1],
        _ => array::flat_map([d - 1, d - 2], t)
    };
    #[native] t(5)
}
"#, prints("[i64:1, i64:0, i64:1, i64:1, i64:0, i64:1, i64:0, i64:1]"); FuseExpect::Jit);

run!(bare_capturing_tree_recursion, r#"
{
    let y = 2;
    let rec t = |d: i64| -> i64 select d {
        0 => y,
        _ => array::fold(array::map([d - 1, d - 1], t), 0, |a, x| a + x)
    };
    #[native] t(3)
}
"#, is(Value::I64(16)); FuseExpect::Jit);

// the bare callback's loop is in the slots of a loop that runs as chunks,
// which hands each the recursion's capture
run!(bare_capturing_tree_recursion_through_nested_maps, r#"
{
    let y = 2;
    let rec t = |d: i64| -> i64 select d {
        0 => y,
        _ => array::fold(
            array::map([d - 1, d - 1], |c| array::fold(array::map([c], t), 0, |a, x| a + x)),
            0,
            |a, x| a + x
        )
    };
    #[native] t(3)
}
"#, is(Value::I64(16)); FuseExpect::Jit);

// the recursion is the callback of a loop the region runs inline: the
// slots of the loop in its body call its kernel
run!(bare_recursion_from_a_collection_callback, r#"
{
    let rec k = |n: i64| -> i64 select n {
        0 => 0,
        _ => n + array::fold(array::map([n - 1], k), 0, |a, x| a + x)
    };
    #[native] list::map([<1, 2, 4>], k)
}
"#, prints("[i64:1, i64:3, i64:10]"); FuseExpect::Jit);

// the recursion is the callback of the collection its body calls: the
// call in its body shares the outer call's instance, whose loop it runs
run!(bare_recursion_from_its_own_collection, r#"
{
    let rec k = |n: i64| -> i64 select n {
        0 => 0,
        _ => n + array::fold(array::map([n - 1], k), 0, |a, x| a + x)
    };
    #[native] array::map([1, 2, 4], k)
}
"#, prints("[i64:1, i64:3, i64:10]"); FuseExpect::Jit);

run!(bare_recursion_from_its_own_fold, r#"
{
    let rec f = |a: i64, x: i64| -> i64 select x <= 0 {
        true => a + 1,
        false => a + array::fold([x - 1, x - 2], x, f)
    };
    #[native] array::fold([1, 2, 3], 0, f)
}
"#, is(Value::I64(21)); FuseExpect::Jit);

run!(bare_capturing_recursion_from_its_own_collection, r#"
{
    let y = 2;
    let rec t = |d: i64| -> i64 select d {
        0 => y,
        _ => array::fold(array::map([d - 1, d - 1], t), 0, |a, x| a + x)
    };
    #[native] array::map([3, 1], t)
}
"#, prints("[i64:16, i64:4]"); FuseExpect::Jit);

// two recursions, each the callback of a collection in its own body,
// in one region: every instance of a collection shares its prototype's
// id, and the slots of each call their own recursion
run!(bare_recursions_from_their_own_collections, r#"
{
    let rec k = |n: i64| -> i64 select n {
        0 => 0,
        _ => n + array::fold(array::map([n - 1], k), 0, |a, x| a + x)
    };
    let rec j = |n: i64| -> i64 select n {
        0 => 1,
        _ => n * array::fold(array::map([n - 1, n - 1], j), 0, |a, x| a + x)
    };
    #[native] (array::map([1, 2, 4], k), array::map([1, 2, 3], j))
}
"#, prints("[[i64:1, i64:3, i64:10], [i64:2, i64:8, i64:48]]"); FuseExpect::Jit);

run!(bare_recursions_from_collection_callbacks, r#"
{
    let rec k = |n: i64| -> i64 select n {
        0 => 0,
        _ => n + array::fold(array::map([n - 1], k), 0, |a, x| a + x)
    };
    let rec j = |n: i64| -> i64 select n {
        0 => 1,
        _ => n * array::fold(array::map([n - 1, n - 1], j), 0, |a, x| a + x)
    };
    #[native] (list::map([<1, 2, 4>], k), list::map([<1, 2, 3>], j))
}
"#, prints("[[i64:1, i64:3, i64:10], [i64:2, i64:8, i64:48]]"); FuseExpect::Jit);

// a bare callback that names another function: an instance of its own
run!(bare_callback_naming_another_function, r#"
{
    let g = |c: i64| -> i64 c * 2;
    let rec t = |d: i64| -> i64 select d {
        0 => 1,
        _ => t(d - 1) + array::fold(array::map([d, 1], g), 0, |a, x| a + x)
    };
    #[native] t(4)
}
"#, is(Value::I64(29)); FuseExpect::Jit);

/// The bare-callback recursion is under an arm that sleeps every other
/// value.
#[tokio::test(flavor = "current_thread")]
async fn bare_recursion_sleeps_and_wakes() -> Result<()> {
    let code = "{ let rec k = |n: i64| -> i64 select n { 0 => 0, _ => \
                  n + array::fold(array::map([n - 1], k), 0, |a, x| a + x) }; \
                let ticks = array::iter([0, 1, 2, 3, 4]); \
                select ticks % 2 { 0 => #[native] k(ticks + 2), _ => 0 - 1 } }";
    assert_stream(code, &[3, -1, 10, -1, 21]).await
}

/// The bare callback's slots root child activations, deleted and made
/// again at every depth.
const BARE_SHRINKING_CHILDREN: &str = "{ let rec f = |n: i64| -> i64 select n { 0 => 1, _ => \
      1 + array::fold(array::map(array::filter([n - 1, n - 2], |c| c >= 0), f), 0, \
      |a, c| a + c) }; \
    let n = array::iter([NS]); \
    #[native] f(n) }";

/// [`BARE_SHRINKING_CHILDREN`] where the recursion is also the callback
/// of the collection around it, whose instance its body's call shares.
const SHARED_SHRINKING_CHILDREN: &str = "{ let rec f = |n: i64| -> i64 select n { 0 => 1, _ => \
      1 + array::fold(array::map(array::filter([n - 1, n - 2], |c| c >= 0), f), 0, \
      |a, c| a + c) }; \
    let n = array::iter([NS]); \
    #[native] array::fold(array::map([n], f), 0, |a, c| a + c) }";

#[tokio::test(flavor = "current_thread")]
async fn shared_recursion_children_deleted() -> Result<()> {
    let code = SHARED_SHRINKING_CHILDREN.replace("NS", "5, 1, 5, 7, 2");
    assert_stream(&code, &[20, 2, 20, 54, 4]).await
}

/// The region runs the outer loop and the root's body inline and calls
/// the recursion's kernel for the root's children, so every activation
/// but those three is a self-call's: 229 of the 232 under 10, 4 of the 7
/// under 3.
#[tokio::test(flavor = "current_thread")]
async fn shared_recursion_frees_deleted_slots() -> Result<()> {
    let deep =
        collect_n_blocks(&SHARED_SHRINKING_CHILDREN.replace("NS", "10"), 1).await?;
    let shallow =
        collect_n_blocks(&SHARED_SHRINKING_CHILDREN.replace("NS", "10, 3"), 2).await?;
    if deep[..] != [229] || shallow[1] != 4 {
        bail!("live SelfBlocks {deep:?} then {shallow:?}, expected [229] then [_, 4]")
    }
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn shared_recursion_sleeps_and_wakes() -> Result<()> {
    let code = "{ let rec k = |n: i64| -> i64 select n { 0 => 0, _ => \
                  n + array::fold(array::map([n - 1], k), 0, |a, x| a + x) }; \
                let ticks = array::iter([0, 1, 2, 3, 4]); \
                select ticks % 2 { 0 => #[native] array::fold(array::map([ticks + 2], k), \
                  0, |a, x| a + x), _ => 0 - 1 } }";
    assert_stream(code, &[3, -1, 10, -1, 21]).await
}

#[tokio::test(flavor = "current_thread")]
async fn bare_recursion_children_deleted() -> Result<()> {
    let code = BARE_SHRINKING_CHILDREN.replace("NS", "5, 1, 5, 7, 2");
    assert_stream(&code, &[20, 2, 20, 54, 4]).await
}

#[tokio::test(flavor = "current_thread")]
async fn bare_recursion_frees_deleted_slots() -> Result<()> {
    let deep = collect_n_blocks(&BARE_SHRINKING_CHILDREN.replace("NS", "10"), 1).await?;
    let shallow =
        collect_n_blocks(&BARE_SHRINKING_CHILDREN.replace("NS", "10, 1"), 2).await?;
    if deep[..] != [231] || shallow[1] != 1 {
        bail!("live SelfBlocks {deep:?} then {shallow:?}, expected [231] then [_, 1]")
    }
    Ok(())
}
