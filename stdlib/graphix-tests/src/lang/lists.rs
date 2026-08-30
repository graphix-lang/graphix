// Native List literals and list-slice patterns
// (`design/list_native.md`, phase B).

use anyhow::Result;
use graphix_package_core::run;
use netidx::publisher::Value;

// ── Literals ────────────────────────────────────────────────────────

const LIST_LIT_BASIC: &str = r#"
  list::to_array([<1, 2, 3>])
"#;

run!(list_lit_basic, LIST_LIT_BASIC, |v: Result<&Value>| {
    match v {
        Ok(v) => matches!(v.clone().cast_to::<[i64; 3]>(), Ok([1, 2, 3])),
        _ => false,
    }
}; graphix_package_core::testing::FuseExpect::Jit);

const LIST_LIT_EMPTY: &str = r#"
  list::len([<>])
"#;

// The empty literal is a CONSTANT; the whole call folds and the
// identity kernel is suppressed (#139) — no kernel runs.
run!(list_lit_empty, LIST_LIT_EMPTY, |v: Result<&Value>| matches!(
    v,
    Ok(Value::I64(0))
); graphix_package_core::testing::FuseExpect::None);

const LIST_LIT_NESTED: &str = r#"
{
  let nested = [<[<1>], [<2, 3>]>];
  list::fold(nested, 0, |a, x| a + list::fold(x, 0, |a, y| a + y))
}
"#;

run!(list_lit_nested, LIST_LIT_NESTED, |v: Result<&Value>| matches!(
    v,
    Ok(Value::I64(6))
); graphix_package_core::testing::FuseExpect::Jit);

// ── Patterns ────────────────────────────────────────────────────────

// The canonical ladder: `[<>]` + `[<h, t..>]` is exhaustive (length
// coverage 0..∞), and the tail bind is the k-th tail — O(1), shared.
const LIST_PAT_SUM: &str = r#"
{
  let rec sum = |l: List<i64>, acc: i64| -> i64
    select l { [<>] => acc, [<h, t..>] => sum(t, acc + h) };
  sum([<1, 2, 3>], 0)
}
"#;

run!(list_pat_sum, LIST_PAT_SUM, |v: Result<&Value>| matches!(
    v,
    Ok(Value::I64(6))
); graphix_package_core::testing::FuseExpect::None);

// Exact-length arms miss on other lengths; anonymous rest `..`.
// (These count `Jit`: the select de-fuses in phase B, but the literal
// binds fuse as their own kernels.)
const LIST_PAT_SHAPES: &str = r#"
{
  let l = [<1, 2, 3>];
  let two = select l { [<a, b>] => a + b, [<>] => -1, [<_, ..>] => -2 };
  let first = select l { [<>] => -1, [<h, ..>] => h };
  two * 100 + first
}
"#;

run!(list_pat_shapes, LIST_PAT_SHAPES, |v: Result<&Value>| matches!(
    v,
    Ok(Value::I64(-199))
); graphix_package_core::testing::FuseExpect::Jit);

// Guards consult after structure; the @-bind captures the whole list.
const LIST_PAT_GUARD_AT: &str = r#"
{
  let l = [<10, 20>];
  select l {
    [<>] => -1,
    w@ [<h, ..>] if h > 5 => h + list::len(w),
    [<_, ..>] => -2
  }
}
"#;

run!(list_pat_guard_at, LIST_PAT_GUARD_AT, |v: Result<&Value>| matches!(
    v,
    Ok(Value::I64(12))
); graphix_package_core::testing::FuseExpect::Jit);

// The tail bind SHARES the spine (semantics: it IS the k-th tail).
const LIST_PAT_TAIL: &str = r#"
{
  let l = [<1, 2, 3>];
  select l { [<>] => -1, [<_, t..>] => list::len(t) }
}
"#;

run!(list_pat_tail, LIST_PAT_TAIL, |v: Result<&Value>| matches!(
    v,
    Ok(Value::I64(2))
); graphix_package_core::testing::FuseExpect::Jit);

// ── Coverage diagnostics ────────────────────────────────────────────

const LIST_PAT_NONEXHAUSTIVE: &str = r#"
{
  let l = [<1, 2>];
  select l { [<>] => 0 }
}
"#;

run!(list_pat_nonexhaustive, LIST_PAT_NONEXHAUSTIVE, |v: Result<&Value>| v.is_err();
    graphix_package_core::testing::FuseExpect::None);

const LIST_PAT_DEAD_WILDCARD: &str = r#"
{
  let l = [<1, 2>];
  select l { [<>] => 0, [<h, t..>] => h, _ => -1 }
}
"#;

run!(list_pat_dead_wildcard, LIST_PAT_DEAD_WILDCARD, |v: Result<&Value>| v.is_err();
    graphix_package_core::testing::FuseExpect::None);

// The suffix form is refused for lists: the tail is O(1), the front
// is an O(n) walk (`design/list_native.md`).
const LIST_PAT_SUFFIX_REFUSED: &str = r#"
{
  let l = [<1, 2>];
  select l { [<init.., last>] => last, _ => -1 }
}
"#;

run!(list_pat_suffix_refused, LIST_PAT_SUFFIX_REFUSED, |v: Result<&Value>| v.is_err();
    graphix_package_core::testing::FuseExpect::None);
