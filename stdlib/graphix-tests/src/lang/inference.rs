// What the checker infers on its own and where it needs an annotation;
// a refusal is paired with the annotation that lifts it where one does
// (the rules in ide/skills/graphix-lang/SKILL.md, Annotations and
// Gotchas).

use anyhow::Result;
use graphix_package_core::testing::eval_modes;
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

// A type test over an untyped parameter tests each call's value: the
// parameter takes null and i64, so an annotation changes nothing.
const TYPE_TEST_BINDS_PARAM: &str = r#"{
  let f = |x| select x { null as _ => 0, v => v };
  f(null) + f(2)
}"#;
const TYPE_TEST_PARAM_ANNOTATED: &str = r#"{
  let f = |x: [i64, null]| select x { null as _ => 0, v => v };
  f(null) + f(2)
}"#;

// A fold's accumulator is the union of its init and the callback's
// results, so a null init needs no annotation.
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

// A let over ⊥ takes every value written to it; declaring the union
// says the same.
const WRITER_WIDER_THAN_FIRST: &str = r#"{
  let n: [i64, null] = null;
  let x = never();
  x <- 1;
  x <- n;
  select x { null as _ => 0, v => v }
}"#;
const WRITER_TARGET_DECLARED: &str = r#"{
  let n: [i64, null] = null;
  let x: [i64, null] = never();
  x <- 1;
  x <- n;
  select x { null as _ => 0, v => v }
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
// check, so the union does not bind it to a sibling member, a foreign
// member is refused rather than absorbed, and the empty slice arm over
// `[Array<'a>, null]` is ordinary.
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
        let v =
            eval_modes(src, crate::TEST_REGISTER).await?.map_err(anyhow::Error::msg)?;
        assert_eq!(v, expected, "{src}");
    }
    let msg = match eval_modes(RIGID_ALIAS_FOREIGN_MEMBER, crate::TEST_REGISTER).await? {
        Err(msg) => msg,
        Ok(v) => panic!("must be refused: {v:?}"),
    };
    // CR claude for claude: [test-gap/low] [test-gap/low] Any type mismatch satisfies
    // this assertion. The rule's refusal is at the definition ("a call picks 'a ...
    // drop `-> [null, 'a]`"). An 'a that absorbed the foreign `0` would instead be
    // refused at the call `first(["s"])` with "Array<i64> does not contain
    // Array<string>", which this assertion also accepts. Assert "a call picks 'a". In
    // annotations_the_checker_asks_for, FOLD_CALLBACK_ANNOTATED is held to "does not
    // contain" too (pin "type mismatch fn(acc", as the skill's case does).
    // WRITER_WIDER_THAN_TVAR's comment says no declaration is offered, but nothing
    // asserts the message lacks "declare". WRITER_WIDER_THAN_INITIALIZER has no lift,
    // though the header pairs every refusal with one (`let x: [i64, string] = y + 1; x
    // <- "s"` checks). probe:
    // design/review-2026-11-16/repro/tests-types-d-rigid-alias-loose-pin.gx
    // (tests-types-d-05)
    // 2026-11-21 claude: annotations_the_checker_asks_for no longer holds
    // FOLD_CALLBACK_ANNOTATED to a refusal: the program is accepted and pinned at its
    // value 1.
    assert!(msg.contains("does not contain"), "{msg}");
    Ok(())
}

/// `?` and `$` over an operand whose type is open: its type is asked for.
const STRIP_OVER_AN_OPEN_OPERAND: &str = r#"{
    let f = |x| x$;
    f(1)
}"#;

const RAISE_OVER_AN_OPEN_OPERAND: &str = r#"{
    let f = |x| x?;
    f(1)
}"#;

/// A writer wider than a `let`'s initializer widens the let: it holds both.
const WRITER_WIDER_THAN_INITIALIZER: &str = r#"{
    let y = 1;
    let x = y + 1;
    x <- "s";
    x
}"#;

/// A writer wider than a declared type variable: no declaration widens
/// it, so none is offered.
const WRITER_WIDER_THAN_TVAR: &str = r#"{
    let f = 'a: Number |x: 'a| -> 'a { x <- "s"; x };
    f(1)
}"#;

/// A name a destructuring let binds takes every write: arithmetic over the
/// widened name is refused.
const WRITER_WIDER_THAN_A_PATTERN_NAME: &str = r#"{
    let (a, b) = (1, 2);
    a <- "s";
    a + b
}"#;

/// A pattern type that holds the write checks.
const WRITER_PATTERN_DECLARED: &str = r#"{
    let (a, b): ([i64, string], i64) = (1, 2);
    a <- "s";
    b
}"#;

#[tokio::test(flavor = "current_thread")]
async fn annotations_the_checker_asks_for() -> Result<()> {
    for (src, refusal) in [
        (NEVER_ARM_UNANNOTATED, "will never match: nothing reaches it"),
        (WRITER_WIDER_THAN_TVAR, "x is 'a: unbound within Number and cannot hold string"),
        (WRITER_WIDER_THAN_A_PATTERN_NAME, "both operands must be one numeric type"),
        (
            STRIP_OVER_AN_OPEN_OPERAND,
            "the type of the operand of ? or $ is not known here",
        ),
        (
            RAISE_OVER_AN_OPEN_OPERAND,
            "the type of the operand of ? or $ is not known here",
        ),
    ] {
        let msg = match eval_modes(src, crate::TEST_REGISTER).await? {
            Err(msg) => msg,
            Ok(v) => panic!("must be refused: {src} => {v:?}"),
        };
        assert!(msg.contains(refusal), "wrong refusal for {src}: {msg}");
    }
    for (src, expected) in [
        (NEVER_ARM_ANNOTATED, 0),
        (TYPE_TEST_BINDS_PARAM, 2),
        (TYPE_TEST_PARAM_ANNOTATED, 2),
        (FOLD_NULL_INIT, 1),
        (FOLD_CALLBACK_ANNOTATED, 1),
        (FOLD_INIT_ANNOTATED, 1),
        (WRITER_WIDER_THAN_FIRST, 1),
        (WRITER_WIDER_THAN_INITIALIZER, 2),
        (MIXED_ARMS_INFERRED, 3),
        (MIXED_ARMS_DECLARED, 3),
        (EMPTY_ARRAY_ARM, 0),
        (WRITER_TARGET_DECLARED, 1),
        (WRITER_PATTERN_DECLARED, 2),
    ] {
        let v =
            eval_modes(src, crate::TEST_REGISTER).await?.map_err(anyhow::Error::msg)?;
        assert_eq!(v, Value::I64(expected), "{src}");
    }
    Ok(())
}
