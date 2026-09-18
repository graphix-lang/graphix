// What the checker infers on its own and where it needs an annotation;
// each refusal is paired with the annotation that lifts it (the rules
// in ide/skills/graphix-lang/SKILL.md, Annotations and Gotchas).

use anyhow::Result;
use graphix_package_core::testing::eval;
use netidx::publisher::Value;

// A never() arm adds nothing to a select's type: the reader that
// matches the absent member hits a dead arm until the let says the
// union it will match.
const NEVER_ARM_UNANNOTATED: &str = r#"{
  let b = false;
  let r = select b { true => never(), false => error(`E("x")) };
  select r { error as _ => 0, _ => 1 }
}"#;
const NEVER_ARM_ANNOTATED: &str = r#"{
  let b = false;
  let r: [i64, Error<`E(string)>] = select b { true => never(), false => error(`E("x")) };
  select r { error as _ => 0, _ => 1 }
}"#;

// A type test over an untyped parameter binds it.
const TYPE_TEST_BINDS_PARAM: &str = r#"{
  let f = |x| select x { null as _ => 0, v => v };
  f(null) + f(2)
}"#;
const TYPE_TEST_PARAM_ANNOTATED: &str = r#"{
  let f = |x: [i64, null]| select x { null as _ => 0, v => v };
  f(null) + f(2)
}"#;

// A fold's accumulator is the type of its init; the callback cannot
// widen it, the init can.
const FOLD_NULL_INIT: &str = r#"{
  let m = array::fold([1, 2, 3], null, |acc, x| select acc { null as _ => x, found => found });
  select m { null as _ => 0, n => n }
}"#;
const FOLD_CALLBACK_ANNOTATED: &str = r#"{
  let m = array::fold([1, 2, 3], null, |acc: [i64, null], x| select acc { null as _ => x, found => found });
  select m { null as _ => 0, n => n }
}"#;
const FOLD_INIT_ANNOTATED: &str = r#"{
  let init: [i64, null] = null;
  let m = array::fold([1, 2, 3], init, |acc, x| select acc { null as _ => x, found => found });
  select m { null as _ => 0, n => n }
}"#;

// Arms of different types are not a problem: an inferred or declared
// type variable against i64, and [] against [1].
const MIXED_ARMS_INFERRED: &str = r#"{
  let h = |x, b: bool| select b { true => x, false => 1 };
  let w = h("s", false);
  h(2, true) + 1
}"#;
const MIXED_ARMS_DECLARED: &str = r#"{
  let h = 'a: Number |x: 'a, b: bool| select b { true => x, false => 1 };
  h(2, true) + 1
}"#;
const EMPTY_ARRAY_ARM: &str = r#"{
  let b = true;
  let x = select b { true => [], false => [1] };
  array::len(x)
}"#;

// A declared type variable stays rigid through a cell merge: an arm's
// alias of `'a` over a union scrutinee is still `'a` at the return
// check, so the union does not bind it to a sibling member (u2), a
// foreign member is refused rather than absorbed (u5), and the empty
// slice arm over `[Array<'a>, null]` is ordinary (s1).
const RIGID_ALIAS_UNION_RETURN: &str = r#"{
  let first = |rows: [Array<'a>, null]| -> ['a, null] select rows {
    null as _ => null,
    [a, ..] => a,
    _ => null
  };
  select first([1, 2]) { null as _ => 0, v => v }
}"#;
const RIGID_ALIAS_FOREIGN_MEMBER: &str = r#"{
  let first = |rows: [Array<'a>, null]| -> ['a, null] select rows {
    null as _ => null,
    [a, ..] => a,
    _ => 0
  };
  select first(["s"]) { null as _ => "", v => v }
}"#;
const RIGID_ALIAS_EMPTY_SLICE: &str = r#"{
  let selected = |rows: [Array<'a>, null], i: i64| -> ['a, null] select rows {
    null as _ => null,
    [] => null,
    rows => rows[min(i, array::len(rows) - 1)]$
  };
  let q: [Array<{code: string}>, null] = [{code: "x"}];
  select selected(q, 0) { null as _ => "", c => c.code }
}"#;

#[tokio::test(flavor = "current_thread")]
async fn rigid_type_variables_survive_a_cell_merge() -> Result<()> {
    for (src, expected) in [
        (RIGID_ALIAS_UNION_RETURN, Value::I64(1)),
        (RIGID_ALIAS_EMPTY_SLICE, Value::from("x")),
    ] {
        let (v, ctx) = eval(src, crate::TEST_REGISTER).await?;
        assert_eq!(v, expected, "{src}");
        ctx.shutdown().await;
    }
    let msg = match eval(RIGID_ALIAS_FOREIGN_MEMBER, crate::TEST_REGISTER).await {
        Err(e) => format!("{e:#}"),
        Ok((v, _)) => panic!("must be refused: {v:?}"),
    };
    assert!(msg.contains("does not contain"), "{msg}");
    Ok(())
}

#[tokio::test(flavor = "current_thread")]
async fn annotations_the_checker_asks_for() -> Result<()> {
    for (src, refusal) in [
        (NEVER_ARM_UNANNOTATED, "unreachable arm"),
        (TYPE_TEST_BINDS_PARAM, "unreachable arm"),
        (FOLD_NULL_INIT, "unreachable arm"),
        (FOLD_CALLBACK_ANNOTATED, "does not contain"),
    ] {
        let msg = match eval(src, crate::TEST_REGISTER).await {
            Err(e) => format!("{e:#}"),
            Ok((v, _)) => panic!("must be refused: {src} => {v:?}"),
        };
        assert!(msg.contains(refusal), "wrong refusal for {src}: {msg}");
    }
    for (src, expected) in [
        (NEVER_ARM_ANNOTATED, 0),
        (TYPE_TEST_PARAM_ANNOTATED, 2),
        (FOLD_INIT_ANNOTATED, 1),
        (MIXED_ARMS_INFERRED, 3),
        (MIXED_ARMS_DECLARED, 3),
        (EMPTY_ARRAY_ARM, 0),
    ] {
        let (v, ctx) = eval(src, crate::TEST_REGISTER).await?;
        assert_eq!(v, Value::I64(expected), "{src}");
        ctx.shutdown().await;
    }
    Ok(())
}
