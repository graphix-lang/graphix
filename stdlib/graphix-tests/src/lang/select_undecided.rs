// A type not decided at a select is one unknown type there

use anyhow::Result;
// CR claude for claude: [structure/low] The predicate `|v: Result<&Value>| matches!(v,
// Ok(Value::Array(a)) if as_i64s(a) == [..])` is written out 134 times in this file (82
// more in select.rs, 30 in fusion_review.rs), and `matches!(v, Ok(Value::I64(n)))` 51
// times, though testing::is already covers it. Add one predicate beside testing::is and
// testing::prints, say `i64s(&'static [i64])`, and use `is(Value::I64(n))` for a single
// value. Found by reading; not run. (tests-lang-rest-a-08)
use graphix_package_core::{
    run,
    testing::{FuseExpect, as_i64s, prints, refused, settles},
};
use netidx::publisher::Value;

run!(undecided_member_tested, r#"{
    let b = true;
    let z = never();
    let v = select b { true => {a: [1], c: 1}, false => z };
    let r = select v { {a, ..} => "[a]" };
    z <- {a: ["s"], c: "w"};
    r
}"#, prints(r#""[1]""#); FuseExpect::Jit);

run!(undecided_member_tested_whole, r#"{
    let b = true;
    let z = never();
    let v = select b { true => {a: [1], c: 1}, false => z };
    let r = select v { {a, c} => "[a]" };
    z <- {a: ["s"], c: "w"};
    r
}"#, prints(r#""[1]""#); FuseExpect::Jit);

run!(undecided_member_annotated, r#"{
    let b = false;
    let z: {a: Array<string>, c: string} = never();
    let v = select b { true => {a: [1], c: 1}, false => z };
    let r = select v { {a, c} => "[a] [c]" };
    z <- {a: ["s"], c: "w"};
    r
}"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == r#"["s"] w"#); FuseExpect::Jit);

run!(undecided_member_beside_a_literal, r#"{
    let b = true;
    let z = never();
    let v = select b { true => 1, false => z };
    let r = select v { 1 => "one", _ => "other" };
    z <- 2;
    r
}"#, prints(r#""one""#); FuseExpect::Jit);

run!(undecided_parameter_tested, r#"{
    let f = |z, b: bool| {
        let v = select b { true => {a: [1], c: 1}, false => z };
        select v { {a, ..} => "[a]" }
    };
    f({a: [2], c: 2}, false)
}"#, prints(r#""[2]""#); FuseExpect::Jit);

run!(undecided_parameter_annotated, r#"{
    let f = |z: {a: Array<i64>, c: i64}, b: bool| {
        let v = select b { true => {a: [1], c: 1}, false => z };
        select v { {a, ..} => "[a]" }
    };
    f({a: [2], c: 2}, false)
}"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "[2]"); FuseExpect::Jit);

// An arm is judged by what reaches it: a member an earlier arm took whole
// meets no later arm.
run!(undecided_member_taken_by_an_earlier_tag, r#"{
    let b = false;
    let z = never();
    let v = select b { true => (`K, 1), false => (`J, z) };
    let r = select v { (`J, _) => "j", (_, 1) => "one", _ => "o" };
    z <- "q";
    r
}"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "j"); FuseExpect::Jit);

run!(undecided_member_taken_by_an_earlier_field_tag, r#"{
    let b = false;
    let z = never();
    let v = select b { true => {k: `K, p: 1}, false => {k: `J, p: z} };
    let r = select v { {k: `J, ..} => "j", {p: 1, ..} => "one", _ => "o" };
    z <- "q";
    r
}"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "j"); FuseExpect::Jit);

run!(undecided_parameter_taken_by_an_earlier_tag, r#"{
    let f = |b: bool, z| {
        let v = select b { true => (`K, 1), false => (`J, z) };
        select v { (`J, _) => "j", (_, 1) => "one", _ => "o" }
    };
    f(false, "q")
}"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "j"); FuseExpect::Jit);

// A guarded arm takes nothing: the arm after it meets the member, which its
// partial struct pattern would decide.
// CR claude for claude: [test-gap/low] No fixture pins what makes this program pass.
// The same is true of undecided_member_beside_a_literal_refused,
// element_tag_deciding_an_undecided_part_refused,
// written_collection_test_with_an_open_variable_refused and
// written_list_element_test_with_an_open_variable_refused (CLAUDE.md: a refusing rule
// pins the refusal and the annotation that passes). Here the passing form is not
// obvious. The refusal says to annotate `z`, but `z: {a: string, c: string}` is refused
// again ("matches more than one member"), while naming every field, `(_, {a, c: _})`,
// passes with `z` unannotated. Add the passing fixture for each. probe:
// design/review-2026-11-16/repro/tests-lang-rest-a-guarded-tag-twin.sh
// (tests-lang-rest-a-07)
run!(undecided_member_beside_a_guarded_tag, r#"{
    let b = false;
    let g = true;
    let z = never();
    let v = select b { true => (`K, {a: 1, c: 1}), false => (`J, z) };
    let r = select v { (`J, _) if g => "j", (_, {a, ..}) => "[a]" };
    z <- {a: "s", c: "w"};
    r
}"#, prints(r#""j""#); FuseExpect::Jit);

// The arms' own cells, a bind's or a `_`'s, are what the arms decided.
run!(arm_cells_are_decided, r#"{
    let f = |x| select x { `A(0) => 1, `A(n) => n, `B(_) => 2 };
    [f(`A(3)), f(`B("s"))]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [3, 2]); FuseExpect::Jit);

run!(undecided_payload_members_reach_the_arm, r#"{
    let b = true;
    let x = never();
    let y = never();
    let v = select b { true => `P({a: [1], c: x}), false => `P({a: ["s"], c: y}) };
    let r = select v { `P({a, ..}) => "[a]" };
    x <- 1;
    y <- 2;
    r
}"#, prints(r#""[1]""#); FuseExpect::Jit);

run!(annotated_payload_members_reach_the_arm, r#"{
    let b = false;
    let x: i64 = never();
    let y: i64 = never();
    let v = select b { true => `P({a: [1], c: x}), false => `P({a: ["s"], c: y}) };
    let r = select v { `P({a, ..}) => "[a]" };
    x <- 1;
    y <- 2;
    r
}"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == r#"["s"]"#); FuseExpect::Jit);

run!(annotated_payload_members_leave_no_other_arm, r#"{
    let b = false;
    let x: i64 = never();
    let y: i64 = never();
    let v = select b { true => `P({a: [1], c: x}), false => `P({a: ["s"], c: y}) };
    let r = select v { `P({a, ..}) => "[a]", _ => "other" };
    x <- 1;
    y <- 2;
    r
}"#, refused("pattern _ will never match: nothing reaches it (6:20; the value is `P({ a: Array<i64>, c: i64 }) | `P({ a: Array<string>, c: i64 }))"); FuseExpect::None);

// A name over a union position holds an undecided member's part beside
// the others, so what its body decides of it decides that part.
run!(name_over_an_undecided_part_decides_it, r#"{
    let b = true;
    let z = never();
    let v = select b { true => (`A, z), false => (`B, "s") };
    let r = select v { (_, x) => str::len(x) };
    z <- "ab";
    r
}"#, |v: Result<&Value>| matches!(v, Ok(Value::I64(2))); FuseExpect::Jit);

run!(name_over_an_undecided_part_written_apart_refused, r#"{
    let b = true;
    let z = never();
    let v = select b { true => (`A, z), false => (`B, "s") };
    let r = select v { (_, x) => str::len(x) };
    z <- 5;
    r
}"#, refused("type mismatch string does not contain i64"); FuseExpect::None);

run!(name_over_an_undecided_field_written_apart_refused, r#"{
    let b = true;
    let z = never();
    let v = select b { true => {a: z, k: 1}, false => {a: "s", k: 2} };
    let r = select v { {a: x, ..} => str::len(x) };
    z <- 5;
    r
}"#, refused("type mismatch string does not contain i64"); FuseExpect::None);

run!(name_over_an_undecided_guarded_part_written_apart_refused, r#"{
    let b = true;
    let z = never();
    let v = select b { true => (`A, z), false => (`B, "s") };
    let r = select v { (_, x) if true => str::len(x), _ => 0 };
    z <- 5;
    r
}"#, refused("type mismatch string does not contain i64"); FuseExpect::None);

// A slice with a rest decides the shape of the undecided part it stands
// over, as an annotation would.
run!(slice_over_an_undecided_part_decides_its_shape, r#"{
    let b = true;
    let z = never();
    let v = select b { true => (`A, [1]), false => (`B, z) };
    let r = select v { (_, [x, ..]) => x + 1, (_, _) => 0 };
    z <- ["s"];
    r
}"#, refused("type mismatch i64 does not contain string"); FuseExpect::None);

run!(name_over_an_undecided_parameter_part_called_apart_refused, r#"{
    let f = |z, b| {
        let v = select b { true => {a: z, k: 1}, false => {a: "s", k: 2} };
        select v { {a: x, ..} => str::len(x) }
    };
    f(5, true)
}"#, refused("string does not contain i64"); FuseExpect::None);

// A written test is a closed type: a variable nothing declares in it is
// undecided.
run!(written_test_with_an_open_variable, r#"{
    let f = |x: [i64, `A(i64)]| -> i64 select x { ['b, `A(i64)] as `A(n) => n, _ => 0 };
    let w: [i64, `A(i64)] = 4;
    f(w)
}"#, prints("i64:0"); FuseExpect::Jit);

run!(written_test_closed, r#"{
    let f = |x: [i64, `A(i64)]| -> i64 select x { [i64, `A(i64)] as `A(n) => n, _ => 0 };
    let w: [i64, `A(i64)] = 4;
    let a: [i64, `A(i64)] = `A(7);
    [f(w), f(a)]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [0, 7]); FuseExpect::Jit);

run!(written_collection_test_with_an_open_variable, r#"{
    let f = 'c: Collection |xs: 'c<[i64, `Keep(i64)]>| -> i64 select xs {
        Array<['q, `Keep(i64)]> as [`Keep(r)] => r,
        _ => 0
    };
    [f([`Keep(3)]), f([1])]
}"#, prints("[i64:3, i64:0]"); FuseExpect::Jit);

// The narrowing over what reaches the arm decides a written test's own
// variable: one its structure tests is refused all the same.
run!(written_array_element_test_with_an_open_variable, r#"{
    let f = |xs: Array<[i64, `Keep(i64)]>| -> i64 select xs {
        Array<['q, `Keep(i64)]> as [`Keep(r)] => r,
        _ => 0
    };
    [f([`Keep(3)]), f([1])]
}"#, prints("[i64:3, i64:0]"); FuseExpect::Jit);

run!(written_list_element_test_with_an_open_variable, r#"{
    let f = |xs: List<[i64, `Keep(i64)]>| -> i64 select xs {
        List<['q, `Keep(i64)]> as [<`Keep(r), ..>] => r,
        _ => 0
    };
    [f([<`Keep(3)>]), f([<1>])]
}"#, prints("[i64:3, i64:0]"); FuseExpect::Jit);

run!(written_test_with_an_open_variable_over_any_refused, r#"{
    let f = |x: Any| -> i64 select x { ['q, `A(i64)] as `A(n) => n, _ => 0 };
    let s: Any = `A("s");
    [f(`A(3)), f(s)]
}"#, refused("type mismatch i64 does not contain Any"); FuseExpect::None);

run!(written_array_element_test_with_an_open_variable_over_any_refused, r#"{
    let f = |xs: Array<Any>| -> i64 select xs {
        Array<['q, `Keep(i64)]> as [`Keep(r)] => r,
        _ => 0
    };
    let s: Any = `Keep("s");
    [f([`Keep(3)]), f([s])]
}"#, refused("type mismatch i64 does not contain Any"); FuseExpect::None);

// Inside a tuple the narrowing reaches `Any` at the variable's position;
// an or-pattern, a literal the test's other member holds and an abstract's
// payload test the variable as a tag does.
run!(written_tuple_part_test_with_an_open_variable_over_any_refused, r#"{
    let f = |x: (Any, i64)| -> i64 select x {
        (['q, `B(i64)], i64) as (`B(n), m) => n + m,
        _ => 0
    };
    let s: Any = `B("s");
    [f((`B(1), 1)), f((s, 1))]
}"#, refused("type mismatch Number does not contain Any"); FuseExpect::None);

run!(written_tuple_part_test_over_any_closed, r#"{
    let f = |x: (Any, i64)| -> i64 select x {
        (`B(i64), i64) as (`B(n), m) => n + m,
        _ => 0
    };
    let s: Any = `B("s");
    [f((`B(1), 1)), f((s, 1))]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [2, 0]); FuseExpect::Jit);

run!(written_or_test_with_an_open_variable_over_any_refused, r#"{
    let f = |x: Any| -> i64 select x {
        ['q, `A(i64), `B(i64)] as `A(n) | `B(n) => n,
        _ => 0
    };
    let s: Any = `A("s");
    [f(`A(3)), f(s)]
}"#, refused("type mismatch i64 does not contain Any"); FuseExpect::None);

run!(written_or_test_over_any_closed, r#"{
    let f = |x: Any| -> i64 select x {
        [`A(i64), `B(i64)] as `A(n) | `B(n) => n,
        _ => 0
    };
    let s: Any = `A("s");
    [f(`A(3)), f(`B(4)), f(s)]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [3, 4, 0]); FuseExpect::Jit);

run!(written_null_test_with_an_open_variable, r#"{
    let f = |x: [i64, null]| -> i64 select x { ['q, null] as null => 0, _ => 1 };
    [f(null), f(1)]
}"#, prints("[i64:0, i64:1]"); FuseExpect::Jit);

run!(written_null_test_closed, r#"{
    let f = |x: [i64, null]| -> i64 select x { [i64, null] as null => 0, _ => 1 };
    [f(null), f(1)]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [0, 1]); FuseExpect::Jit);

run!(written_abstract_payload_test_with_an_open_variable_refused, r#"{
    type W<'a> = Abstract<'a>;
    let f = |x: W<[i64, `A(i64)]>| -> i64 select x {
        W<['q, `A(i64)]> as W(`A(n)) => n,
        _ => 0
    };
    [f(W(`A(3))), f(W(1))]
}"#, refused("pattern W<['q, `A(i64)]> will never match: nothing reaches it (3:50; the value is W<[i64, `A(i64)]>)"); FuseExpect::None);

run!(written_abstract_payload_test_closed, r#"{
    type W<'a> = Abstract<'a>;
    let f = |x: W<[i64, `A(i64)]>| -> i64 select x {
        W<[i64, `A(i64)]> as W(`A(n)) => n,
        _ => 0
    };
    [f(W(`A(3))), f(W(1))]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [3, 0]); FuseExpect::None);

run!(written_array_element_test_closed, r#"{
    let f = |xs: Array<[i64, `Keep(i64)]>| -> i64 select xs {
        Array<[i64, `Keep(i64)]> as [`Keep(r)] => r,
        _ => 0
    };
    [f([`Keep(3)]), f([1])]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [3, 0]); FuseExpect::Jit);

// A name over a written test's own variable tests nothing, and a
// destructure that takes apart every value the narrowing gives it decides
// its shape as an annotation would.
run!(written_array_test_binding_an_open_variable, r#"{
    let f = |xs: Array<[i64, `Keep(i64)]>| -> i64 select xs {
        Array<'q> as [a, ..] => 1,
        _ => 0
    };
    [f([`Keep(3)]), f([1]), f([])]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [1, 1, 0]); FuseExpect::Jit);

run!(written_test_destructuring_an_open_variable, r#"{
    let f = |xs: Array<(i64, i64)>, x: ((i64, i64), i64)| -> i64 {
        let a = select xs { Array<'q> as [(a, b), ..] => a + b, _ => 0 };
        let c = select x { ('p, i64) as ((a, b), c) => a + b + c };
        a + c
    };
    f([(1, 2)], ((3, 4), 5))
}"#, |v: Result<&Value>| matches!(v, Ok(Value::I64(15))); FuseExpect::Jit);

// A written test tests what it reads as any other pattern does: one
// that takes a member only through an undecided part is refused.
run!(written_union_test_of_an_undecided_part_refused, r#"{
    let b = false;
    let z = never();
    let v = select b { true => (`K, 1), false => (`J, z) };
    let r = select v { (`J, [i64, string]) as (`J, s) => "js", (_, n) => "n" };
    z <- true;
    r
}"#, refused("pattern (`J, [i64, string]) will never match: nothing reaches it (5:20; the value is (`K, i64) | (`J, bool))"); FuseExpect::None);

run!(written_union_test_of_an_undecided_part_before_a_literal_refused, r#"{
    let b = false;
    let z = never();
    let v = select b { true => (`K, 1), false => (`J, z) };
    let r = select v { (`J, [i64, string]) as (`J, s) => "js", (_, 1) => "one", (_, n) => "n" };
    z <- true;
    r
}"#, refused("pattern (`J, [i64, string]) will never match: nothing reaches it (5:20; the value is (`K, i64) | (`J, bool))"); FuseExpect::None);

run!(written_union_test_of_an_undecided_parameter, r#"{
    let f = |b: bool, z| {
        let v = select b { true => (`K, 1), false => (`J, z) };
        select v { (`J, [i64, string]) as (`J, s) => "js", (_, n) => "n" }
    };
    f(false, true)
}"#, prints(r#""n""#); FuseExpect::Jit);

// Two undecided cells are one type each together: each may pass the test
// held alone, the other free to become what the test needs.
run!(written_union_test_of_two_undecided_parts_refused, r#"{
    let b = false;
    let w = never();
    let z = never();
    let v = select b { true => (`K, 1, 1), false => (`J, w, z) };
    let r = select v { [(`J, Any, i64), (`J, i64, Any)] as (`J, x, y) => "j", (_, a, c) => "n" };
    w <- "s";
    z <- "t";
    r
}"#, refused("pattern [(`J, Any, i64), (`J, i64, Any)] will never match: nothing reaches it (6:20; the value is (`K, i64, i64) | (`J, string, string))"); FuseExpect::None);

run!(written_union_test_of_two_undecided_parameters, r#"{
    let f = |b: bool, w, z| {
        let v = select b { true => (`K, 1, 1), false => (`J, w, z) };
        select v { [(`J, Any, i64), (`J, i64, Any)] as (`J, x, y) => "j", (_, a, c) => "n" }
    };
    f(false, "s", "t")
}"#, prints(r#""n""#); FuseExpect::Jit);

run!(
    written_union_test_of_two_annotated_parts,
    r#"{
    let b = false;
    let w: [i64, string] = never();
    let z: [i64, string] = never();
    let v = select b { true => (`K, 1, 1), false => (`J, w, z) };
    let r = select v { [(`J, Any, i64), (`J, i64, Any)] as (`J, x, y) => "j", (_, a, c) => "n" };
    w <- "s";
    z <- "t";
    r
}"#,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "n")
);

run!(written_union_test_of_an_annotated_part, r#"{
    let b = false;
    let z: [i64, string, bool] = never();
    let v = select b { true => (`K, 1), false => (`J, z) };
    let r = select v { (`J, [i64, string]) as (`J, s) => "js", (_, n) => "n" };
    z <- true;
    r
}"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "n"); FuseExpect::Jit);

// A written test over an undecided part it does not test (Any there) passes.
run!(written_test_any_over_an_undecided_part, r#"{
    let b = false;
    let z = never();
    let v = select b { true => (`K, 1), false => (`J, z) };
    let r = select v { (`J, Any) as (`J, _) => "j", (_, n) => "n" };
    z <- true;
    r
}"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "j"); FuseExpect::Jit);

// Exhaustiveness holds an undecided part only `_` meets as one type,
// whatever it becomes: the product distributes over it.
run!(undecided_part_only_wildcards_meet_distributes, r#"{
    let b = true;
    let z = never();
    let v = select b { true => (`A, 5, z), false => (`B, "s", z) };
    let w = (1, v);
    let r = select w { (_, (`A, _, _)) => "a", (_, (`B, _, _)) => "b" };
    z <- 0;
    r
}"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "a"); FuseExpect::Jit);

run!(undecided_part_beside_a_bool_head_distributes, r#"{
    let b = false;
    let z = never();
    let v = select b { true => (`A, true, 5, z), false => (`B, true, "s", z) };
    let w = (1, v);
    let r = select w {
        (_, (_, true, x, _)) => "x[x]",
        (_, (`A, _, _, _)) => "a",
        (_, (`B, _, _, _)) => "b"
    };
    z <- 0;
    r
}"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "xs"); FuseExpect::Jit);

// A body decides the element its arm binds: a later arm's literal there
// is that body's type mismatch.
run!(widened_open_element_refused, r#"{
    let f = |v| select (v, 0) { ([x, ..], _) => str::len(x), ([1, ..], _) => 5, (_, _) => 0 };
    [f(["ab"]), f([1])]
}"#, refused("type mismatch string does not contain i64"); FuseExpect::None);

run!(widened_open_element_annotated, r#"{
    let f = |v: Array<string>| select (v, 0) { ([x, ..], _) => str::len(x), (_, _) => 0 };
    [f(["ab"]), f([])]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [2, 0]); FuseExpect::Jit);

// A slice arm claims its lengths of each member whose element type its
// elements cover.
run!(slice_arm_claims_the_member_it_covers, r#"{
    let f = |v: [Array<(`A, i64)>, Array<(`B, string)>]| select v {
        [(`A, x), ..] => "a[x]", [(`B, s), ..] => "b[s]", [] => "e"
    };
    let a: [Array<(`A, i64)>, Array<(`B, string)>] = [(`A, 5)];
    let b: [Array<(`A, i64)>, Array<(`B, string)>] = [(`B, "s")];
    let e: Array<(`B, string)> = [];
    "[f(a)] [f(b)] [f(e)]"
}"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "a5 bs e"); FuseExpect::Jit);

run!(written_slice_arm_claims_the_member_it_covers, r#"{
    let f = |v: [Array<(`A, i64)>, Array<(`B, string)>]| select v {
        Array<(`A, i64)> as [(`A, x), ..] => "a[x]",
        Array<(`B, string)> as [(`B, s), ..] => "b[s]",
        [] => "e"
    };
    let a: [Array<(`A, i64)>, Array<(`B, string)>] = [(`A, 5)];
    let b: [Array<(`A, i64)>, Array<(`B, string)>] = [(`B, "s")];
    "[f(a)] [f(b)]"
}"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "a5 bs"); FuseExpect::Jit);

// The pool reads a primitive member beside `bool` and `null` as a head
// only a name or `_` matches.
run!(bool_heads_beside_a_null_arm_cover, r#"{
    let u = |x: (bool, [i64, null])| select x {
        (_, null) => 1, (true, _) => 0, (false, _) => 2
    };
    [u((true, null)), u((false, 7)), u((false, null)), u((true, 3))]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [1, 2, 1, 0]); FuseExpect::Jit);

run!(bool_heads_after_a_null_head_cover, r#"{
    let u = |x: (bool, [i64, null])| select x {
        (true, null) => 0, (true, _) => 2, (false, _) => 1
    };
    [u((true, null)), u((false, 7)), u((false, null)), u((true, 3))]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [0, 1, 1, 2]); FuseExpect::Jit);

run!(rest_head_left_uncovered_refused, r#"{
    let u = |x: (bool, [i64, null])| select x { (_, null) => 1, (true, _) => 0 };
    u((false, 7))
}"#, refused("missing match cases"); FuseExpect::None);

// A written test that holds part of a member covers that part with the
// bool heads beside it: the member is cut, at each position, into the
// position's members.
run!(written_test_beside_bool_head_covers, r#"{
    let u = |x: (bool, [i64, null])| select x {
        (true, _) => 0,
        (bool, i64) as (false, n) => n,
        (bool, null) as (false, _) => 1
    };
    [u((true, null)), u((false, 7)), u((false, null)), u((true, 3))]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [0, 7, 1, 0]); FuseExpect::Jit);

run!(written_tests_split_a_primitive_member, r#"{
    let u = |x: (bool, [i64, string, null])| select x {
        (true, _) => 0,
        (bool, i64) as (false, n) => n,
        (bool, string) as (false, s) => str::len(s),
        (false, null) => 3
    };
    [u((true, null)), u((false, 7)), u((false, "abcd")), u((false, null))]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [0, 7, 4, 3]); FuseExpect::Jit);

run!(written_test_leaves_a_member_part_refused, r#"{
    let u = |x: (bool, [i64, string, null])| select x {
        (true, _) => 0,
        (bool, i64) as (false, n) => n,
        (false, null) => 3
    };
    u((false, "abcd"))
}"#, refused("missing match cases: nothing matches (bool, string) (2:53)"); FuseExpect::None);

run!(written_tests_split_a_struct_member, r#"{
    type N = [i64, null];
    let u = |x: {a: bool, b: [N, string]}| select x {
        {a: true, b: _} => 0,
        {a: bool, b: i64} as {a: false, b: n} => n,
        {a: bool, b: [string, null]} as {a: false, b: _} => 1
    };
    [u({a: true, b: null}), u({a: false, b: 7}), u({a: false, b: "s"}), u({a: false, b: null})]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [0, 7, 1, 1]); FuseExpect::Jit);

// Two positions, each cut by the tests.
run!(written_tests_narrow_two_positions, r#"{
    let u = |x: ([bool, null], [i64, null])| select x {
        ([bool, null], null) as (_, null) => 0,
        (bool, i64) as (true, n) => n,
        (bool, i64) as (false, n) => n + 100,
        (null, i64) as (_, n) => n + 1000
    };
    [u((null, null)), u((true, 1)), u((false, 2)), u((null, 3))]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [0, 1, 102, 1003]); FuseExpect::Jit);

run!(written_tests_narrow_two_positions_leaving_one_refused, r#"{
    let u = |x: ([bool, null], [i64, null])| select x {
        ([bool, null], null) as (false, null) => 0,
        ([bool, null], null) as (null, null) => 5,
        (bool, i64) as (true, n) => n,
        (bool, i64) as (false, n) => n + 100,
        (null, i64) as (_, n) => n + 1000
    };
    u((true, null))
}"#, refused("missing match cases: nothing matches (bool, null) (2:53)"); FuseExpect::None);

// A test holding part of two members of one shape covers its part of each.
run!(written_test_over_two_members_covers_each_part, r#"{
    let u = |x: [(bool, [i64, null]), (bool, [string, null])]| select x {
        (true, _) => 0,
        (bool, [i64, string]) as (false, n) => 1,
        (bool, null) as (false, _) => 2
    };
    [u((false, 1)), u((false, "s")), u((false, null)), u((true, 1))]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [1, 1, 2, 0]); FuseExpect::Jit);

// A declared variable at the position is cut into its bound's members,
// each also held by a test naming the variable.
run!(written_tests_split_a_declared_variable_beside_bool_head, r#"{
    let u = 'b: [i64, string] |x: (bool, ['b, null])| select x {
        (true, _) => 0,
        (bool, i64) as (false, n) => 1,
        (bool, string) as (false, s) => 2,
        (false, null) => 3
    };
    let (w0, w1): ([i64, string], [i64, string]) = (7, "four");
    [u((true, null)), u((false, w0)), u((false, w1)), u((false, null))]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [0, 1, 2, 3]); FuseExpect::Jit);

run!(written_test_leaves_a_declared_variable_part_refused, r#"{
    let u = 'b: Number |x: (bool, ['b, null])| select x {
        (true, _) => 0,
        (bool, i64) as (false, n) => 1,
        (bool, f64) as (false, n) => 2,
        (false, null) => 3
    };
    u((true, null))
}"#, refused("missing match cases: nothing matches (bool, [Uint, i8, i16, i32, z32, z64, f32, decimal]) (2:55)"); FuseExpect::None);

run!(written_test_naming_the_declared_variable_covers_its_part, r#"{
    let u = 'b: [i64, string] |x: (bool, ['b, null])| select x {
        (true, _) => 0,
        (bool, 'b) as (false, v) => 1,
        (false, null) => 3
    };
    let (w0, w1): ([i64, string], [i64, string]) = (7, "four");
    [u((true, null)), u((false, w0)), u((false, w1)), u((false, null))]
}"#, refused("a test cannot name a declared type variable ('b): write `_` there, and the name takes the value's own type"); FuseExpect::Jit);

// A test naming a declared variable holds only the variable, never its
// bound: an instance may take the variable narrower.
run!(written_test_naming_another_declared_variable_covers_nothing_refused, r#"{
    let f = 'b: [i64, u8] |x: (bool, [i64, u8]), y: 'b| select x {
        (bool, 'b) as (true, a) => 1,
        (false, _) => 2
    };
    f((true, u8:1), i64:3)
}"#, refused("a test cannot name a declared type variable ('b): write `_` there, and the name takes the value's own type"); FuseExpect::None);

run!(written_test_naming_another_declared_variable_beside_a_catch_all, r#"{
    let f = 'b: [i64, u8] |x: (bool, [i64, u8]), y: 'b| select x {
        (bool, 'b) as (true, a) => 1,
        _ => 2
    };
    [f((true, u8:1), i64:3), f((true, i64:1), i64:3), f((false, i64:1), i64:3)]
}"#, refused("a test cannot name a declared type variable ('b): write `_` there, and the name takes the value's own type"); FuseExpect::Jit);

// Every combination a written test matches in each part it reaches,
// earlier arms match: dead.
run!(written_test_after_bool_head_is_dead, r#"{
    let u = |x: (bool, [i64, null])| select x {
        (true, _) => 0,
        (bool, i64) as (true, n) => n,
        _ => 1
    };
    u((false, 7))
}"#, refused("pattern (bool, i64) will never match: nothing reaches it (2:45; the value is (bool, [i64, null]))"); FuseExpect::None);

// The tests' sets cut the positions alike: a test holding i64 at one
// position and u8 at another covers neither alone.
run!(written_test_sets_cut_each_position_refused, r#"{
    let u = |x: (bool, [i64, u8, null], [i64, u8, null])| select x {
        (bool, i64, u8) as (true, a, b) => 1,
        (_, null, _) => 2,
        (_, _, null) => 3,
        (false, _, _) => 4
    };
    u((true, u8:1, i64:2))
}"#, refused("missing match cases: nothing matches [(bool, u8, [u8, i64]), (bool, i64, i64)] (2:66)"); FuseExpect::None);

run!(written_test_sets_cut_each_position_covered, r#"{
    let u = |x: (bool, [i64, u8, null], [i64, u8, null])| select x {
        (bool, i64, u8) as (true, a, b) => 1,
        (bool, [i64, u8], [i64, u8]) as (true, a, b) => 5,
        (_, null, _) => 2,
        (_, _, null) => 3,
        (false, _, _) => 4
    };
    [u((true, u8:1, i64:2)), u((true, i64:1, u8:2)), u((false, null, null))]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [5, 1, 2]); FuseExpect::Jit);

// A typedef applied twice in one test holds what each application holds.
run!(each_typedef_application_holds_its_own_part_refused, r#"{
    type O<'a> = ['a, null];
    let u = |x: (bool, [i64, u8, null], [i64, u8, null])| select x {
        (bool, O<[i64, u8]>, O<i64>) as (true, a, b) => 1,
        (false, _, _) => 2
    };
    u((true, null, u8:1))
}"#, refused("missing match cases: nothing matches (bool, [u8, i64, null], u8) (3:66)"); FuseExpect::None);

run!(each_typedef_application_holds_its_own_part_covered, r#"{
    type O<'a> = ['a, i64, null];
    let u = |x: (bool, [i64, u8, null], [i64, u8, string, null])| select x {
        (bool, O<[i64, u8]>, O<string>) as (true, a, b) => 1,
        (bool, [i64, u8, null], u8) as (true, a, b) => 3,
        (false, _, _) => 2
    };
    [u((false, null, null)), u((true, null, "s")), u((true, null, i64:1)), u((true, null, u8:1))]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [2, 1, 1, 3]); FuseExpect::Jit);

// A declared variable beside the member the pools count holds values
// the pools do not see: an arm whose test can match it is not dead by
// pooling.
run!(written_test_reaching_a_declared_variable_member_is_live_by_parts, r#"{
    let u = 'a: ([bool, i64], string) |x: ['a, (bool, [i64, null])]| select x {
        (bool, i64) as (true, _) => 1,
        (bool, null) as (true, _) => 2,
        [(bool, [i64, null]), ([bool, i64], string)] as (true, _) => 3,
        _ => 4
    };
    let s: [([bool, i64], string), (bool, [i64, null])] = (true, "s");
    [u(s), u((true, 4)), u((false, 4))]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [3, 1, 4]); FuseExpect::Jit);

run!(written_test_reaching_a_declared_variable_member_is_live_by_pool, r#"{
    let u = 'a: ([bool, i64], string) |x: ['a, (bool, [i64, null])]| select x {
        (bool, [i64, null]) as (true, _) => 1,
        [(bool, [i64, null]), ([bool, i64], string)] as (true, _) => 3,
        _ => 4
    };
    let s: [([bool, i64], string), (bool, [i64, null])] = (true, "s");
    [u(s), u((true, 4)), u((false, 4))]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [3, 1, 4]); FuseExpect::Jit);

run!(written_test_missing_a_declared_variable_member_is_dead_by_parts, r#"{
    let u = 'a: ([bool, i64], string) |x: ['a, (bool, [i64, null])]| select x {
        (bool, i64) as (true, _) => 1,
        (bool, null) as (true, _) => 2,
        (bool, [i64, null]) as (true, _) => 3,
        _ => 4
    };
    u((true, 4))
}"#, refused("pattern (bool, [i64, null]) will never match: nothing reaches it (2:77; the value is ['a, (bool, [i64, null])])"); FuseExpect::None);

// Cutting a member leaves each part's combinations under the pool's
// limit: seven bool positions beside a test holding eight primitives.
run!(cut_member_keeps_bool_combinations_in_reach, r#"{
    let u = |x: (bool, bool, bool, bool, bool, bool, bool, [i64, u8, i32, u32, string, f64, f32, bytes, null])| select x {
        (bool, bool, bool, bool, bool, bool, bool, [i64, u8, i32, u32, string, f64, f32, bytes]) as (true, true, true, true, true, true, true, v) => 50,
        (_, _, _, _, _, _, _, null) => 0,
        (true, _, _, _, _, _, _, _) => 1,
        (false, true, _, _, _, _, _, _) => 2,
        (false, false, true, _, _, _, _, _) => 3,
        (false, false, false, true, _, _, _, _) => 4,
        (false, false, false, false, true, _, _, _) => 5,
        (false, false, false, false, false, true, _, _) => 6,
        (false, false, false, false, false, false, true, _) => 7,
        (false, false, false, false, false, false, false, _) => 99
    };
    [u((false, false, false, false, false, false, false, 3)), u((true, true, true, true, true, true, true, "s")), u((true, false, false, false, false, false, false, null))]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [99, 50, 0]); FuseExpect::Jit);

// A written test at a nested union position covers its member alone.
run!(written_nested_constructor_covers_its_member_alone, r#"{
    let g = 'b: Number |x: {f: ['b, `Keep('b)]}| -> ['b, null] select x {
        {f: ['b, `Keep('b)]} as {f: `Keep(r)} => r
    };
    let w: {f: [i64, `Keep(i64)]} = {f: 5};
    g(w)
}"#, refused("a test cannot name a declared type variable ('b): write `_` there, and the name takes the value's own type"); FuseExpect::None);

run!(written_nested_constructor_beside_a_catch_all, r#"{
    let g = 'b: Number |x: {f: ['b, `Keep('b)]}| -> ['b, null] select x {
        {f: ['b, `Keep('b)]} as {f: `Keep(r)} => r,
        _ => null
    };
    let w: {f: [i64, `Keep(i64)]} = {f: 5};
    let k: {f: [i64, `Keep(i64)]} = {f: `Keep(4)};
    "[g(w)] [g(k)]"
}"#, refused("a test cannot name a declared type variable ('b): write `_` there, and the name takes the value's own type"); FuseExpect::Jit);

// An abstract pattern whose payload tests one member of the representation
// covers that member alone.
run!(abstract_payload_variant_covers_its_member_alone, r#"{
    type R<'a> = Abstract<['a, `Err(string)]>;
    let v: [i64, `Err(string)] = 5;
    let x: R<i64> = R(v);
    select x { R(`Err(m)) => str::len(m) }
}"#, refused("missing match cases"); FuseExpect::None);

run!(abstract_payload_parameter_undecided_beside_a_catch_all, r#"{
    type R<'a> = Abstract<['a, `Err(string)]>;
    let v: [i64, `Err(string)] = 5;
    select R(v) { R(`Err(m)) => str::len(m), _ => 0 }
}"#, prints("i64:0"); FuseExpect::Jit);

run!(abstract_payload_variant_beside_a_catch_all, r#"{
    type R<'a> = Abstract<['a, `Err(string)]>;
    let f = 'a: Number |x: R<'a>| -> i64 select x { R(`Err(m)) => str::len(m), _ => 0 };
    let v: [i64, `Err(string)] = `Err("abc");
    let w: [f64, `Err(string)] = 1.5;
    [f(R(v)), f(R(w))]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [3, 0]); FuseExpect::Jit);

// An arm whose pattern would decide a part the scrutinee held undecided
// before the arms tests it: refused, naming the binding to annotate.
run!(tag_over_an_unannotated_part, r#"{
    let f = |x| select (x, 0) { (`J, _) => 1, (`M, _) => 2 };
    f(`M)
}"#, prints("i64:2"); FuseExpect::Jit);

run!(tag_over_an_unannotated_part_beside_a_wildcard, r#"{
    let f = |x| select (x, 0) { (`J, _) => 1, _ => 2 };
    f(`M)
}"#, prints("i64:2"); FuseExpect::Jit);

run!(tag_over_an_annotated_part, r#"{
    let f = |x: [`J, `M]| select (x, 0) { (`J, _) => 1, (`M, _) => 2 };
    [f(`M), f(`J)]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [2, 1]); FuseExpect::Jit);

run!(tag_deciding_a_later_written_part_refused, r#"{
    let w = never();
    let r = select (w, 0) { (`J, _) => 1, (`M, _) => 2, _ => 3 };
    w <- `M;
    r
}"#, refused("pattern (`J, _) will never match: nothing reaches it (3:20; the value is (`M, i64))"); FuseExpect::None);

run!(tag_over_a_later_written_annotated_part, r#"{
    let w: [`J, `M, `L] = never();
    let r = select (w, 0) { (`J, _) => 1, (`M, _) => 2, _ => 3 };
    w <- `M;
    r
}"#, |v: Result<&Value>| matches!(v, Ok(Value::I64(2))); FuseExpect::Jit);

run!(element_tag_deciding_an_undecided_part_refused, r#"{
    let w = never();
    let r = select [w] { [`J] => 1, _ => 2 };
    w <- `M;
    r
}"#, refused("pattern [`J] will never match: nothing reaches it (3:20; the value is Array<`M>)"); FuseExpect::None);

run!(payload_tag_deciding_an_undecided_part_refused, r#"{
    let w = never();
    let r = select `A(w) { `A(`J) => 1, _ => 2 };
    w <- `M;
    r
}"#, refused("pattern `A(`J) will never match: nothing reaches it (3:20; the value is `A(`M))"); FuseExpect::None);

run!(payload_tag_over_an_annotated_part, r#"{
    let w: [`J, `M] = never();
    let r = select `A(w) { `A(`J) => 1, _ => 2 };
    w <- `M;
    r
}"#, |v: Result<&Value>| matches!(v, Ok(Value::I64(2))); FuseExpect::Jit);

run!(written_test_deciding_an_undecided_part_refused, r#"{
    let w = never();
    let r = select (w, 0) { (`J, i64) as (j, _) => 1, _ => 2 };
    w <- `M;
    r
}"#, refused("pattern (`J, i64) will never match: nothing reaches it (3:20; the value is (`M, i64))"); FuseExpect::None);

run!(written_test_over_an_annotated_part, r#"{
    let w: [`J, `M] = never();
    let r = select (w, 0) { (`J, i64) as (j, _) => 1, _ => 2 };
    w <- `M;
    r
}"#, |v: Result<&Value>| matches!(v, Ok(Value::I64(2))); FuseExpect::Jit);

run!(partial_struct_completing_an_undecided_member, r#"{
    let b = false;
    let z = never();
    let v = select b { true => (`K, {a: 1, c: 1}), false => (`J, z) };
    let r = select v { (`K, _) => "k", (_, {a, ..}) => "[a]" };
    z <- {a: "s", c: "w"};
    r
}"#, prints(r#""s""#); FuseExpect::Jit);

run!(partial_struct_over_an_annotated_member, r#"{
    let b = false;
    let z: {a: string, c: string} = never();
    let v = select b { true => (`K, {a: 1, c: 1}), false => (`J, z) };
    let r = select v { (`K, _) => "k", (_, {a, ..}) => "[a]" };
    z <- {a: "s", c: "w"};
    r
}"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "s"); FuseExpect::Jit);

run!(partial_struct_over_a_lone_undecided_cell, r#"{
    let z = never();
    let r = select z { {a, ..} => "[a]" };
    z <- {a: ["s"], c: "w"};
    r
}"#, prints(r#""[\"s\"]""#); FuseExpect::Jit);

run!(partial_struct_over_an_undecided_parameter, r#"{
    let f = |x| select x { {a, ..} => "[a]" };
    f({a: ["s"], c: "w"})
}"#, prints(r#""[\"s\"]""#); FuseExpect::Jit);

run!(partial_struct_over_an_annotated_parameter, r#"{
    let f = |x: {a: Array<string>, c: string}| select x { {a, ..} => "[a]" };
    f({a: ["s"], c: "w"})
}"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == r#"["s"]"#); FuseExpect::Jit);

run!(or_alternative_testing_an_undecided_part, r#"{
    let b = false;
    let z = never();
    let v = select b { true => (`K, 1), false => (`J, z) };
    let r = select v { (`J, _) | (_, 1) => "j1", _ => "o" };
    z <- "q";
    r
}"#, prints(r#""j1""#); FuseExpect::Jit);

run!(or_alternative_over_an_annotated_part, r#"{
    let b = false;
    let z: [i64, string] = never();
    let v = select b { true => (`K, 1), false => (`J, z) };
    let r = select v { (`J, _) | (_, 1) => "j1", _ => "o" };
    z <- "q";
    r
}"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "j1"); FuseExpect::Jit);

// A union of an undecided part and a bind: deciding the part leaves the
// union's holder standing for the bind's cell, which is still judged as
// the union it was.
run!(tag_over_an_unannotated_part_beside_a_bind, r#"{
    let z = never();
    let k: [i64, null] = 1;
    let u = select k { null as _ => z, n => n };
    let r = select (u, 0) { (42, v) => v, (a, b) => a };
    z <- 2;
    r
}"#, prints("i64:1"); FuseExpect::Jit);

run!(tag_over_an_annotated_part_beside_a_bind, r#"{
    let z: i64 = never();
    let k: [i64, null] = 1;
    let u = select k { null as _ => z, n => n };
    let r = select (u, 0) { (42, v) => v, (a, b) => a };
    z <- 2;
    r
}"#, |v: Result<&Value>| matches!(v, Ok(Value::I64(1))); FuseExpect::Jit);

run!(written_test_over_an_unannotated_part_beside_a_bind, r#"{
    let z = never();
    let k: [i64, null] = 1;
    let u = select k { null as _ => z, i64 as n => n };
    let r = select (u, 0) { (i64, i64) as (a, b) => a };
    z <- 2;
    r
}"#, prints("i64:1"); FuseExpect::Jit);

run!(written_test_over_an_annotated_part_beside_a_bind, r#"{
    let z: i64 = never();
    let k: [i64, null] = 1;
    let u = select k { null as _ => z, i64 as n => n };
    let r = select (u, 0) { (i64, i64) as (a, b) => a };
    z <- 2;
    r
}"#, |v: Result<&Value>| matches!(v, Ok(Value::I64(1))); FuseExpect::Jit);

run!(tag_over_an_unannotated_parameter_beside_a_bind, r#"{
    let f = |z, k: [i64, null]| {
        let u = select k { null as _ => z, i64 as n => n };
        select (u, 0) { (42, v) => v, (a, b) => a }
    };
    f(3, 1)
}"#, prints("i64:1"); FuseExpect::Jit);

// A written test over the whole union decides the undecided member beside
// the bind, as it decides one beside a plain type.
run!(written_test_over_a_whole_union_beside_a_bind_decides_the_member, r#"{
    let z = never();
    let k: [i64, null] = null;
    let u = select k { null as _ => z, i64 as n => n };
    let r = select u { i64 as a => a };
    z <- 2;
    r
}"#, |v: Result<&Value>| matches!(v, Ok(Value::I64(2))); FuseExpect::Jit);

run!(written_test_over_a_whole_union_beside_a_bind_then_written_apart_refused, r#"{
    let z = never();
    let k: [i64, null] = null;
    let u = select k { null as _ => z, i64 as n => n };
    let r = select u { i64 as a => a };
    z <- "s";
    r
}"#, refused("missing match cases: nothing matches string (5:20)"); FuseExpect::None);

// A pattern that takes apart two constructions of one nominal type makes
// their undecided payloads one, and the union's holder then stands for one
// of its members, which is still judged as the union it was.
run!(payload_pattern_over_two_constructions_a_call_returns, r#"{
    type B<'p> = Abstract<'p>;
    let f = |a, c| select true { true => a, false => c };
    select f(B([1]), B([])) { B(v) => v }
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [1]); FuseExpect::Jit);

run!(payload_pattern_over_two_constructions_bound_by_a_let, r#"{
    type B<'p> = Abstract<'p>;
    let f = |a, c| select true { true => a, false => c };
    let x = f(B([1]), B([]));
    select x { B(v) => v }
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [1]); FuseExpect::Jit);

run!(payload_pattern_over_two_undecided_constructions, r#"{
    type B<'p> = Abstract<'p>;
    let f = |a, c| select true { true => a, false => c };
    select f(B([]), B([])) { B(v) => 0 }
}"#, |v: Result<&Value>| matches!(v, Ok(Value::I64(0))); FuseExpect::Jit);

// Arms that each bind one part of a payload built at two shapes, one
// holding the other's cell, give the select the union of the two over that
// one cell: a call that takes it builds as it checked, and what an arm
// decides of its bind decides the cell for a later write.
run!(payload_part_bound_by_literal_arms_over_two_shapes_builds_at_a_call, r#"{
    type B<'p> = Abstract<'p>;
    let x = never();
    let b = B((1, [x]));
    b <- B((2, x));
    let v = select b.0 { (1, h) => h, (2, h) => h, (3, h) => h, (_, h) => h };
    x <- 1;
    let show = |a| a;
    show(v)
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [1]); FuseExpect::Jit);

run!(payload_part_an_arm_decides_over_two_shapes_then_written_apart_refused, r#"{
    type B<'p> = Abstract<'p>;
    let x = never();
    let b = B((3, [x]));
    b <- B((2, x));
    let v = select b.0 {
        (1, h) => 0,
        (2, h) => 0,
        (3, h) => { let q: [i64, Array<i64>] = h; select q { Array<i64> as a => array::len(a), n => n + 1 } },
        (_, h) => 0
    };
    x <- "s";
    v
}"#, refused("type mismatch i64 does not contain string"); FuseExpect::None);

// A later arm's pattern decides the part before an earlier arm's body is
// checked: the body's error is the select's.
// CR claude for eric: [bug/medium] This pins a refusal that names a consequence, not
// the cause. `x` is open before the arms, so `(`M, _)` deciding it is what the select
// rule refuses ("(`M, _) tests a value of 'x, a type not decided at this select ...
// annotate it"), and the check says exactly that when the guarded arm's body does not
// read `a`. Here select.rs::typecheck0_with checks the first arm's body first and
// returns its error at once. So the user is told "pattern `J will never match `M" about
// a type they never wrote, and only reaches the real refusal after deleting the `J arm
// as told. The dead-arm path already runs check_undecided before it returns. Doing the
// same for a body or guard error (finish the arm walk, return check_undecided's refusal
// if there is one, else the first body error) leaves the a_body_error_* fixtures'
// refusals as they are, as far as I traced, and this fixture would then pin "(`M, _)
// tests a value of 'x". probe:
// design/review-2026-11-16/repro/tests-lang-rest-a-body-error-masks-undecided.sh
// (tests-lang-rest-a-01)
run!(an_earlier_body_over_a_part_a_later_arm_decides_is_its_error, r#"{
    let f = |b: bool, x| select (x, 0) {
        (a, _) if b => select a { `J => 1, `M => 2 },
        (`M, _) => 3,
        _ => 4
    };
    f(true, `M)
}"#, refused("pattern `J will never match: nothing reaches it (3:31; the value is `M)"); FuseExpect::None);

run!(later_arm_over_an_annotated_part_an_earlier_body_tests, r#"{
    let f = |b: bool, x: [`J, `M]| select (x, 0) {
        (a, _) if b => select a { `J => 1, `M => 2 },
        (`M, _) => 3,
        _ => 4
    };
    [f(true, `M), f(false, `M), f(false, `J)]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [2, 3, 4]); FuseExpect::Jit);

// A whole scrutinee open before the arms is theirs to decide together, and
// a name or `_` over an undecided part decides nothing.
run!(whole_open_scrutinee_tag_no_write_brings_is_dead, r#"{
    let w = never();
    let r = select w { `J => 1, `M => 2 };
    w <- `M;
    r
}"#, refused("pattern `J will never match: nothing reaches it"); FuseExpect::Jit);

run!(wildcard_and_name_over_an_undecided_part, r#"{
    let w = never();
    let r = select (w, 0) { (_, 1) => `Q, (x, 0) => x, _ => `Q };
    w <- `M;
    r
}"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "M"); FuseExpect::Jit);

// An arm that only takes an undecided part apart decides its shape, as an
// annotation would.
run!(destructuring_tuple_decides_an_undecided_part, r#"{
    let f = |x| select (x, 0) { ((a, b), _) => a + b };
    f((1, 2))
}"#, |v: Result<&Value>| matches!(v, Ok(Value::I64(3))); FuseExpect::Jit);

run!(destructuring_struct_decides_an_undecided_part, r#"{
    let f = |x| select (x, 0) { ({a, b}, _) => a + b };
    f({a: 1, b: 2})
}"#, |v: Result<&Value>| matches!(v, Ok(Value::I64(3))); FuseExpect::Jit);

run!(destructuring_slice_with_a_rest_decides_an_undecided_part, r#"{
    let f = |x| select (x, 0) { ([a, ..], _) => a, (_, _) => 0 };
    [f([4, 5]), f([])]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [4, 0]); FuseExpect::Jit);

run!(destructuring_slice_with_a_rest_over_a_declared_element, r#"{
    let f = |x: Array<'e>| select (x, 0) { ([a, ..], _) => a, (_, _) => 0 };
    [f([4, 5]), f([])]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [4, 0]); FuseExpect::Jit);

run!(destructuring_slice_suffix_decides_an_undecided_part, r#"{
    let f = |x| select (x, 0) { ([init.., l], _) => l + array::len(init), (_, _) => 0 };
    [f([4, 5]), f([])]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [6, 0]); FuseExpect::Jit);

run!(destructuring_list_decides_an_undecided_part, r#"{
    let f = |x| select (x, 0) { ([<a, t..>], _) => a + list::len(t), (_, _) => 0 };
    [f([<4, 5, 6>]), f([<>])]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [6, 0]); FuseExpect::Jit);

run!(destructuring_abstract_payload_decides_an_undecided_part, r#"{
    type W = Abstract<(i64, i64)>;
    let f = |x| select (x, 0) { (W((p, q)), _) => p + q };
    f(W((1, 2)))
}"#, |v: Result<&Value>| matches!(v, Ok(Value::I64(3))); FuseExpect::Jit);

run!(destructuring_nested_capture_decides_an_undecided_part, r#"{
    let f = |x| select (x, 0) { (v@ (a, {b, c}), _) => a + b + c + v.0 };
    f((1, {b: 2, c: 3}))
}"#, |v: Result<&Value>| matches!(v, Ok(Value::I64(7))); FuseExpect::Jit);

// A part the scrutinee holds only through a lower bound (a reference's
// referent, an abstract constructor's parameter) is judged as the bound
// holds it.
run!(tag_testing_an_undecided_part_below_a_reference, r#"{
    let f = |x| { let r = &mut (x, 0); select *r { (`J, _) => 1, (`M, _) => 3 } };
    f(`M)
}"#, prints("i64:3"); FuseExpect::None);

run!(tag_testing_an_undecided_part_below_a_reference_beside_a_wildcard, r#"{
    let f = |x| { let r = &mut (x, 0); select *r { (`J, _) => 1, (_, _) => 3 } };
    f(`M)
}"#, prints("i64:3"); FuseExpect::None);

run!(field_tag_testing_an_undecided_part_below_a_reference, r#"{
    let f = |x| { let r = &mut {a: x, b: 0}; select *r { {a: `J, b} => b, {a, b} => 3 } };
    f(`M)
}"#, prints("i64:3"); FuseExpect::None);

run!(written_test_over_an_unannotated_part_below_a_reference, r#"{
    let f = |x| { let r = &mut (x, 0); select *r { ([`J, `M], i64) as (`J, _) => 1, _ => 3 } };
    f(`M)
}"#, prints("i64:3"); FuseExpect::None);

run!(tag_testing_an_undecided_part_below_an_abstract_parameter, r#"{
    type W<'a> = Abstract<'a>;
    let f = |x| select W((x, 0)).0 { (`J, _) => 1, (`M, _) => 3, _ => 2 };
    f(`M)
}"#, prints("i64:3"); FuseExpect::Jit);

run!(tag_over_an_annotated_part_below_a_reference, r#"{
    let f = |x: [`J, `M]| { let r = &mut (x, 0); select *r { (`J, _) => 1, (`M, _) => 3 } };
    [f(`M), f(`J)]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [3, 1]); FuseExpect::None);

run!(destructuring_a_part_below_a_reference, r#"{
    let f = |x| { let r = &mut (x, 0); select *r { ((a, b), c) => a + b + c } };
    f((1, 2))
}"#, |v: Result<&Value>| matches!(v, Ok(Value::I64(3))); FuseExpect::Jit);

// A part below a bound that the arms leave open is judged too: the
// referent they decide (`Array<Any>`) holds its values without it.
run!(tag_over_an_unannotated_open_part_below_a_reference, r#"{
    let f = |x| { let r = &mut [x, 0]; select *r { [`J, _] => 1, _ => 3 } };
    f(`M)
}"#, prints("i64:3"); FuseExpect::None);

run!(tag_over_an_annotated_open_part_below_a_reference, r#"{
    let f = |x: [`J, `M]| { let r = &mut [x, 0]; select *r { [`J, _] => 1, _ => 3 } };
    [f(`M), f(`J)]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [3, 1]); FuseExpect::None);

run!(tag_over_an_annotated_reference_to_an_open_part, r#"{
    let f = |x| {
        let r: &mut Array<[`J, `M, i64]> = &mut [x, 0];
        select *r { [`J, _] => 1, _ => 3 }
    };
    [f(`M), f(`J)]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [3, 1]); FuseExpect::None);

run!(tag_over_an_unannotated_open_part_the_referent_keeps, r#"{
    let f = |x| { let r = &mut [x, 0]; select *r { [`J, ..] => 1, _ => 3 } };
    f(`M)
}"#, prints("i64:3"); FuseExpect::None);

run!(tag_over_an_unannotated_open_part_below_two_references, r#"{
    let f = |x| {
        let r = &mut [x, 0];
        let s = &mut r;
        select **s { [`J, _] => 1, _ => 3 }
    };
    f(`M)
}"#, prints("i64:3"); FuseExpect::None);

run!(tag_over_an_unannotated_open_part_below_an_abstract_parameter, r#"{
    type W<'a> = Abstract<'a>;
    let f = |x| select W([x, 0]).0 { [`J, _] => 1, _ => 3 };
    f(`M)
}"#, prints("i64:3"); FuseExpect::Jit);

run!(tag_over_an_annotated_open_part_below_an_abstract_parameter, r#"{
    type W<'a> = Abstract<'a>;
    let f = |x: [`J, `M]| select W([x, 0]).0 { [`J, _] => 1, _ => 3 };
    [f(`M), f(`J)]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [3, 1]); FuseExpect::Jit);

run!(tag_over_an_unannotated_never_let_below_a_reference, r#"{
    let z = never();
    let r = &mut [z, 0];
    let o = select *r { [`J, _] => 1, _ => 3 };
    z <- `J;
    o
}"#, prints("i64:1"); FuseExpect::None);

run!(tag_over_an_annotated_never_let_below_a_reference, r#"{
    let z: [`J, `M] = never();
    let r = &mut [z, 0];
    let o = select *r { [`J, _] => 1, _ => 3 };
    z <- `J;
    o
}"#, |v: Result<&Value>| matches!(v, Ok(Value::I64(1))); FuseExpect::None);

run!(dead_arm_after_a_tested_open_part_below_a_reference_refused, r#"{
    let f = |x| { let r = &mut [x, 0]; select *r { [`J, _] => 1, _ => 3, [_, _] => 4 } };
    f(`M)
}"#, refused("nothing reaches it"); FuseExpect::None);

run!(dead_arm_after_a_tested_annotated_part_below_a_reference_is_unreachable, r#"{
    let f = |x: [`J, `M]| { let r = &mut [x, 0]; select *r { [`J, _] => 1, _ => 3, [_, _] => 4 } };
    f(`M)
}"#, refused("pattern [_, _] will never match: nothing reaches it"); FuseExpect::None);

run!(destructuring_an_open_part_below_a_reference, r#"{
    let f = |x| { let r = &mut [x, 0]; select *r { [(a, b), _] => a, _ => 2 } };
    [f((1, 2)), f((7, 2))]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [1, 7]); FuseExpect::None);

run!(literal_testing_an_open_part_below_a_reference, r#"{
    let f = |x| { let r = &mut [x, 0]; select *r { [1, _] => 1, _ => 3 } };
    f(2)
}"#, prints("i64:3"); FuseExpect::None);

run!(literal_over_an_annotated_open_part_below_a_reference, r#"{
    let f = |x: i64| { let r = &mut [x, 0]; select *r { [1, _] => 1, _ => 3 } };
    [f(2), f(1)]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [3, 1]); FuseExpect::None);

run!(or_alternative_testing_an_open_part_below_a_reference, r#"{
    let f = |x| { let r = &mut [x, 0]; select *r { [`J | `K, _] => 1, _ => 3 } };
    f(`M)
}"#, prints("i64:3"); FuseExpect::None);

run!(list_tag_testing_an_open_part_below_a_reference, r#"{
    let f = |x| { let r = &mut [<x, 0>]; select *r { [<`J, t..>] => 1, _ => 3 } };
    f(`M)
}"#, prints("i64:3"); FuseExpect::None);

run!(payload_tag_testing_an_open_part_below_a_reference, r#"{
    let f = |x| { let r = &mut `P(x); select *r { `P(`J) => 1, `P(_) => 3 } };
    f(`M)
}"#, prints("i64:3"); FuseExpect::None);

run!(payload_tag_over_an_annotated_open_part_below_a_reference, r#"{
    let f = |x: [`J, `M]| { let r = &mut `P(x); select *r { `P(`J) => 1, `P(_) => 3 } };
    [f(`M), f(`J)]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [3, 1]); FuseExpect::None);

run!(tag_over_an_unannotated_open_part_below_a_captured_reference, r#"{
    let f = |x| {
        let r = &mut [x, 0];
        let g = || select *r { [`J, _] => 1, _ => 3 };
        g()
    };
    f(`M)
}"#, prints("i64:3"); FuseExpect::None);

run!(naming_an_open_part_below_a_reference, r#"{
    let f = |x| { let r = &mut [x, 0]; select *r { [a, _] => 1, _ => 3 } };
    [f(`M), f("s")]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [1, 1]); FuseExpect::None);

// A member of the bound that the unguarded arms before an arm cover whole
// does not reach it: the arm tests no value of that member's cells.
run!(arm_behind_a_cover_of_a_member_below_a_reference, r#"{
    let f = |x, b: bool| {
        let r = &mut (select b { true => (`K, x), false => (`L, 0) });
        select *r { (`K, _) => 1, (_, 0) => 2, _ => 3 }
    };
    [f(1, true), f(1, false)]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [1, 2]); FuseExpect::Jit);

run!(arm_behind_a_cover_leaves_the_covered_part_open, r#"{
    let f = |x, b: bool| {
        let r = &mut (select b { true => (x, `K), false => (0, `L) });
        select *r { (_, `K) => 1, (0, _) => 2, _ => 3 }
    };
    [f("s", true), f(0, true), f("s", false), f(0, false)]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [1, 1, 2, 2]); FuseExpect::Jit);

// The cover holds the member whatever its cell becomes, so the cell stays
// open wherever the cover sorts among the arms' tests: no test that would
// decide it takes the member first.
run!(arm_behind_a_cover_leaves_the_covered_part_open_tag_first, r#"{
    let f = |x, b: bool| {
        let r = &mut (select b { true => (`K, x), false => (`L, 0) });
        select *r { (`K, _) => 1, (_, 0) => 2, _ => 3 }
    };
    [f("s", true), f(0, true), f("s", false), f(0, false)]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [1, 1, 2, 2]); FuseExpect::Jit);

run!(arm_behind_a_cover_leaves_the_covered_field_open, r#"{
    let f = |x, b: bool| {
        let r = &mut (select b { true => {k: `K, v: x}, false => {k: `L, v: 0} });
        select *r { {k: `K, v: _} => 1, {k: _, v: 0} => 2, _ => 3 }
    };
    let g = |x, b: bool| {
        let r = &mut (select b { true => {a: x, k: `K}, false => {a: 0, k: `L} });
        select *r { {a: _, k: `K} => 1, {a: 0, k: _} => 2, _ => 3 }
    };
    [f("s", true), f(0, false), g("s", true), g(0, false)]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [1, 2, 1, 2]); FuseExpect::Jit);

run!(arm_behind_a_cover_leaves_the_covered_payload_open, r#"{
    let f = |x, b: bool| {
        let r = &mut (select b { true => `P(`K, x), false => `P(`L, 0) });
        select *r { `P(`K, _) => 1, `P(_, 0) => 2, _ => 3 }
    };
    let g = |x, b: bool| {
        let r = &mut (select b { true => `P(x, `K), false => `P(0, `L) });
        select *r { `P(_, `K) => 1, `P(0, _) => 2, _ => 3 }
    };
    [f("s", true), f(0, false), g("s", true), g(0, false)]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [1, 2, 1, 2]); FuseExpect::Jit);

run!(arm_behind_a_cover_leaves_a_cell_inside_the_covered_part_open, r#"{
    let f = |x, b: bool| {
        let r = &mut (select b { true => (`K, (x, 1)), false => (`L, (0, 1)) });
        select *r { (`K, _) => 1, (_, (0, _)) => 2, _ => 3 }
    };
    [f("s", true), f(0, true), f("s", false), f(0, false)]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [1, 1, 2, 2]); FuseExpect::Jit);

run!(arm_behind_a_cover_leaves_both_covered_parts_open, r#"{
    let f = |x, y, b: bool| {
        let r = &mut (select b { true => (`K, x, y), false => (`L, 0, "a") });
        select *r { (`K, _, _) => 1, (_, 0, _) => 2, (_, _, "a") => 3, _ => 4 }
    };
    [f("s", 5, true), f(5, "s", true), f("s", 5, false)]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [1, 1, 2]); FuseExpect::Jit);

run!(arm_behind_a_cover_leaves_an_abstract_parameter_part_open_tag_first, r#"{
    type W<'p> = Abstract<'p>;
    let f = |x, b: bool| {
        let w = W(select b { true => (`K, x), false => (`L, 0) });
        select w.0 { (`K, _) => 1, (_, 0) => 2, _ => 3 }
    };
    [f("s", true), f(0, true), f("s", false), f(0, false)]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [1, 1, 2, 2]); FuseExpect::Jit);

run!(arm_behind_an_or_cover_leaves_both_covered_parts_open, r#"{
    let f = |x, y, b: i64| {
        let r = &mut (select b { 0 => (`K, x), 1 => (`M, y), _ => (`L, 0) });
        select *r { (`K, _) | (`M, _) => 1, (_, 0) => 3, _ => 4 }
    };
    [f("s", 2.5, 0), f("s", 2.5, 1), f("s", 2.5, 2)]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [1, 1, 3]); FuseExpect::Jit);

// A cell the covered part holds through a select of its own, or twice, stays
// open the same way.
run!(arm_behind_a_cover_leaves_a_selected_cell_of_the_covered_part_open, r#"{
    let f = |x, b: bool, c: bool| {
        let r = &mut (select b {
            true => (`K, select c { true => x, false => 0 }),
            false => (`L, 0)
        });
        select *r { (`K, _) => 1, (_, 0) => 2, _ => 3 }
    };
    [f("s", true, true), f("s", true, false), f("s", false, true), f(0, true, true)]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [1, 1, 2, 1]); FuseExpect::Jit);

run!(arm_behind_a_cover_leaves_a_cell_built_in_twice_open, r#"{
    let f = |x, b: bool| {
        let r = &mut (select b { true => (`K, x, x), false => (`L, 0, "a") });
        select *r { (`K, _, _) => 1, (_, 0, _) => 2, (_, _, "a") => 3, _ => 4 }
    };
    [f("s", true), f(1, true), f("s", false), f(1, false)]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [1, 1, 2, 2]); FuseExpect::Jit);

// The referent the arms decided holds the covered part as `Any`: a second
// select over it tests the value there at run time.
run!(second_select_tests_the_part_a_cover_left_open, r#"{
    let f = |x, b: bool| {
        let r = &mut (select b { true => (`K, x), false => (`L, 0) });
        let s = select *r { (`K, _) => 1, (_, 0) => 2, _ => 3 };
        let t = select *r { (_, 0) => 5, _ => 6 };
        s * 10 + t
    };
    [f(0, true), f("s", true), f("s", false)]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [15, 16, 25]); FuseExpect::Jit);

// A test that holds the member with one part annotated holds its other
// cell, behind a cover that binds a part too.
run!(test_holding_an_annotated_part_leaves_the_other_part_open, r#"{
    let f = |x: i64, y, b: bool| {
        let r = &mut (select b { true => (`K, x, y), false => (`L, 0, "a") });
        select *r { (`K, n, _) => n, (_, 0, _) => 2, (_, _, "a") => 3, _ => 4 }
    };
    let g = |x: i64, y, b: bool| {
        let r = &mut (select b { true => (y, x, `K), false => ("a", 0, `L) });
        select *r { (_, n, `K) => n, (_, 0, _) => 2, ("a", _, _) => 3, _ => 4 }
    };
    [f(5, "s", true), f(5, 7, true), f(5, 7, false), g(5, "s", true), g(5, 7, true), g(5, 7, false)]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [5, 5, 2, 5, 5, 2]); FuseExpect::Jit);

// A cover that names a part holds the member as it stands, as a `_` does:
// the tests behind it that could each take it decide none of its cells.
run!(built_part_two_tests_behind_a_named_cover_decide_nothing, r#"{
    let f = |x, y, b: bool| {
        let r = &mut (select b { true => (`K, x, y), false => (`L, 0, "a") });
        select *r { (`K, n, _) => n, (_, 0, _) => 2, (_, _, "a") => 3, _ => 4 }
    };
    [f(5, "s", true), f(5, "s", false)]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [5, 2]); FuseExpect::Jit);

run!(built_part_two_tests_behind_a_named_cover_in_another_order, r#"{
    let f = |x, y, b: bool| {
        let r = &mut (select b { true => (y, x, `K), false => ("a", 0, `L) });
        select *r { (_, n, `K) => n, (_, 0, _) => 2, ("a", _, _) => 3, _ => 4 }
    };
    [f(5, "s", true), f(5, "s", false)]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [5, 2]); FuseExpect::Jit);

run!(built_part_two_tests_could_take_annotated, r#"{
    let f = |x: i64, y: string, b: bool| {
        let r = &mut (select b { true => (`K, x, y), false => (`L, 0, "a") });
        select *r { (`K, n, _) => n, (_, 0, _) => 2, (_, _, "a") => 3, _ => 4 }
    };
    let g = |x: i64, y: string, b: bool| {
        let r = &mut (select b { true => (y, x, `K), false => ("a", 0, `L) });
        select *r { (_, n, `K) => n, (_, 0, _) => 2, ("a", _, _) => 3, _ => 4 }
    };
    [f(5, "s", true), f(5, "s", false), g(5, "s", true), g(5, "s", false)]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [5, 2, 5, 2]); FuseExpect::Jit);

// Tests that cannot hold every part built into the referent beside a
// catch-all leave the referent what was built: each bind takes its part of
// the members its arm matches, as over the select written in place.
run!(referent_bind_takes_its_part_of_what_was_built_refused, r#"{
    let b = true;
    let r = &mut (select b { true => (`K, "s", "t"), false => (`L, 0, "a") });
    select *r { (`K, n, _) => n + 1, (_, _, m) => str::len(m) }
}"#, refused("type mismatch i64 does not contain string"); FuseExpect::None);

run!(referent_bind_takes_its_part_of_what_was_built, r#"{
    let f = |b: bool| {
        let r = &mut (select b { true => (`K, "ss", "t"), false => (`L, 0, "aaa") });
        select *r { (`K, n, _) => str::len(n), (_, _, m) => str::len(m) }
    };
    [f(true), f(false)]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [2, 3]); FuseExpect::Jit);

run!(referent_bind_beside_a_bind_at_its_part_refused, r#"{
    let b = true;
    let r = &mut (select b { true => (`K, "s", 1), false => (`L, "a", 1) });
    select *r { (`K, n, _) => n + 1, (_, m, _) => str::len(m) }
}"#, refused("type mismatch i64 does not contain string"); FuseExpect::None);

run!(referent_field_bind_takes_its_part_of_what_was_built_refused, r#"{
    let b = true;
    let r = &mut (select b { true => {k: `K, v: "s", w: "t"}, false => {k: `L, v: 0, w: "a"} });
    select *r { {k: `K, v: n, w: _} => n + 1, {k: _, v: _, w: m} => str::len(m) }
}"#, refused("type mismatch i64 does not contain string"); FuseExpect::None);

run!(abstract_parameter_bind_takes_its_part_of_what_was_built_refused, r#"{
    type W<'p> = Abstract<'p>;
    let f = |x, y, b: bool| {
        let w = W(select b { true => (`K, x, y), false => (`L, 0, "a") });
        select w.0 { (`K, n, _) => n + 1, (_, _, m) => str::len(m) }
    };
    [f("s", "s", true), f("s", "s", false)]
}"#, refused("i64 does not contain string"); FuseExpect::None);

run!(referent_bind_over_a_parameter_takes_the_parameter, r#"{
    let f = |x, y, b: bool| {
        let r = &mut (select b { true => (`K, x, y), false => (`L, 0, "a") });
        select *r { (`K, n, _) => n + 1, (_, _, m) => str::len(m) }
    };
    [f(5, 7, true), f(5, 7, false)]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [6, 1]); FuseExpect::Jit);

run!(referent_bind_over_a_parameter_takes_the_parameter_refused, r#"{
    let f = |x, y, b: bool| {
        let r = &mut (select b { true => (`K, x, y), false => (`L, 0, "a") });
        select *r { (`K, n, _) => n + 1, (_, _, m) => str::len(m) }
    };
    [f("s", "s", true), f("s", "s", false)]
}"#, refused("i64 does not contain string"); FuseExpect::None);

// The referent is what was built, so a later write of a member of it fits.
run!(referent_written_after_the_select_takes_a_built_member, r#"{
    let f = |b: bool| {
        let r = &mut (select b { true => (`K, "ss", "t"), false => (`L, 0, "aaa") });
        let s = select *r { (`K, n, _) => str::len(n), (_, _, m) => str::len(m) };
        *r <- (`L, 1, "bbbb");
        s
    };
    [f(true), f(false)]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [2, 3]); FuseExpect::Jit);

run!(arm_behind_a_cover_of_a_member_below_an_abstract_parameter, r#"{
    type W<'p> = Abstract<'p>;
    let f = |x, b: bool| {
        let w = W(select b { true => (x, `K), false => (0, `L) });
        select w.0 { (_, `K) => 1, (0, _) => 2, _ => 3 }
    };
    [f("s", true), f(0, true), f("s", false), f(0, false)]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [1, 1, 2, 2]); FuseExpect::Jit);

run!(arm_behind_an_or_cover_of_two_members_below_a_reference, r#"{
    let f = |x, y, b: i64| {
        let r = &mut (select b { 0 => (`K, x), 1 => (`M, y), _ => (`L, 0) });
        select *r { (`K, _) | (`M, _) => 1, (_, 0) => 3, _ => 4 }
    };
    [f(1, 2, 0), f(1, 2, 1), f(1, 2, 2)]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [1, 1, 3]); FuseExpect::Jit);

run!(arm_behind_a_null_cover_of_a_member_below_a_reference, r#"{
    let f = |x, b: bool| {
        let r = &mut (select b { true => (null, x), false => (1, 0) });
        select *r { (null, _) => 1, (_, 0) => 2, _ => 3 }
    };
    [f(1, true), f(1, false)]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [1, 2]); FuseExpect::Jit);

run!(written_arm_behind_a_cover_of_a_member_below_a_reference, r#"{
    let f = |x, b: bool| {
        let r = &mut (select b { true => (`K, x), false => (`L, 0) });
        select *r { (`K, _) => 1, (Any, i64) as (_, 0) => 2, _ => 3 }
    };
    [f(1, true), f(1, false)]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [1, 2]); FuseExpect::Jit);

// A member of the bound that unguarded arms before an arm take together, a
// literal pool over its heads, does not reach it either: each of those arms'
// tests holds the member where its pattern takes it, whatever the member's
// cells become.
run!(arm_behind_a_pool_of_a_member_below_a_reference, r#"{
    let f = |x, b: bool, c: bool| {
        let r = &mut (select c { true => (b, x), false => (`L, 0) });
        select *r { (true, _) => 1, (false, _) => 2, (_, 0) => 3, _ => 4 }
    };
    let g = |x, b: bool, c: bool| {
        let r = &mut (select c { true => (x, b), false => (0, `L) });
        select *r { (_, true) => 1, (_, false) => 2, (0, _) => 3, _ => 4 }
    };
    [
        f("s", true, true), f("s", false, true), f("s", true, false),
        f(0, true, true), f(0, false, true), f(0, false, false),
        g("s", true, true), g("s", false, true), g("s", true, false),
        g(0, true, true), g(0, false, true), g(0, false, false)
    ]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3]); FuseExpect::Jit);

run!(arm_behind_a_pool_of_a_struct_and_a_payload_below_a_reference, r#"{
    let f = |x, b: bool, c: bool| {
        let r = &mut (select c { true => {k: b, v: x}, false => {k: `L, v: 0} });
        select *r { {k: true, v: _} => 1, {k: false, v: _} => 2, {k: _, v: 0} => 3, _ => 4 }
    };
    let g = |x, b: bool, c: bool| {
        let r = &mut (select c { true => `P(b, x), false => `P(`L, 0) });
        select *r { `P(true, _) => 1, `P(false, _) => 2, `P(_, 0) => 3, _ => 4 }
    };
    [
        f("s", true, true), f("s", false, true), f("s", true, false), f(0, false, true),
        g("s", true, true), g("s", false, true), g("s", true, false), g(0, false, true)
    ]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [1, 2, 3, 2, 1, 2, 3, 2]); FuseExpect::Jit);

run!(arm_behind_a_pool_of_a_member_below_an_abstract_parameter, r#"{
    type W<'p> = Abstract<'p>;
    let f = |x, b: bool, c: bool| {
        let w = W(select c { true => (b, x), false => (`L, 0) });
        select w.0 { (true, _) => 1, (false, _) => 2, (_, 0) => 3, _ => 4 }
    };
    [f("s", true, true), f("s", false, true), f("s", true, false), f(0, true, true)]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [1, 2, 3, 1]); FuseExpect::Jit);

run!(arm_behind_a_pool_over_two_parts_of_a_member_below_a_reference, r#"{
    let f = |x, a: bool, b: bool, c: bool| {
        let r = &mut (select c { true => (a, b, x), false => (`L, `L, 0) });
        select *r {
            (true, true, _) => 1,
            (true, false, _) => 2,
            (false, _, _) => 3,
            (_, _, 0) => 4,
            _ => 5
        }
    };
    let g = |x, b: bool, c: bool| {
        let r = &mut (select c { true => ((b, 1), x), false => (`L, 0) });
        select *r { ((true, _), _) => 1, ((false, _), _) => 2, (_, 0) => 4, _ => 5 }
    };
    [
        f("s", true, true, true), f("s", true, false, true), f("s", false, true, true),
        f("s", true, true, false), f(0, true, true, true),
        g("s", true, true), g("s", false, true), g("s", true, false), g(0, true, true)
    ]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [1, 2, 3, 4, 1, 1, 2, 4, 1]); FuseExpect::Jit);

run!(arm_behind_an_or_pool_of_a_member_below_a_reference, r#"{
    let f = |x, b: bool, c: bool| {
        let r = &mut (select c { true => (b, x), false => (`L, 0) });
        select *r { (true | false, _) => 1, (_, 0) => 3, _ => 4 }
    };
    let g = |x, b: bool, c: bool| {
        let r = &mut (select c { true => (b, x), false => (`L, 0) });
        select *r { (true, _) | (false, _) => 1, (_, 0) => 3, _ => 4 }
    };
    let h = |x, t: [`A, `B, `C], c: bool| {
        let r = &mut (select c { true => (t, x), false => (`L, 0) });
        select *r { (`A | `B, _) => 1, (`C, _) => 2, (_, 0) => 3, _ => 4 }
    };
    [
        f("s", true, true), f("s", false, true), f("s", true, false), f(0, true, true),
        g("s", true, true), g("s", false, true), g("s", true, false), g(0, true, true),
        h("s", `A, true), h("s", `C, true), h("s", `A, false), h(0, `B, true)
    ]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [1, 1, 3, 1, 1, 1, 3, 1, 1, 2, 3, 1]); FuseExpect::Jit);

// Arms that hold a member only together hold its cells together: each tag
// arm takes half of the tags the member's part has, and the two take all of
// it.
run!(arm_behind_the_tag_pool_of_a_member_below_a_reference, r#"{
    let f = |x, t: [`A, `B], c: bool| {
        let r = &mut (select c { true => (t, x), false => (`L, 0) });
        select *r { (`A, _) => 1, (`B, _) => 2, (_, 0) => 3, _ => 4 }
    };
    let g = |x, t: [`A, `B], b: bool, c: bool| {
        let r = &mut (select c { true => (t, b, x), false => (`L, `L, 0) });
        select *r {
            (`A, true, _) => 1,
            (`A, false, _) => 2,
            (`B, _, _) => 3,
            (_, _, 0) => 4,
            _ => 5
        }
    };
    let h = |x, t: [`A, `B], c: bool| select (select c { true => (t, x), false => (`L, 0) }) {
        (`A, _) => 1,
        (`B, _) => 2,
        (_, 0) => 3,
        _ => 4
    };
    [
        f("s", `A, true), f("s", `B, true), f("s", `A, false), f(0, `A, true), f(0, `B, true),
        g("s", `A, true, true), g("s", `A, false, true), g("s", `B, true, true),
        g("s", `A, true, false), g(0, `A, true, true),
        h("s", `A, true), h("s", `B, true), h("s", `A, false), h(0, `B, true)
    ]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [1, 2, 3, 1, 2, 1, 2, 3, 4, 1, 1, 2, 3, 2]); FuseExpect::Jit);

run!(arm_behind_the_tag_pool_of_a_member_beside_other_members, r#"{
    let f = |x, t: [`A, `B], c: i64| {
        let r = &mut (select c { 0 => (t, x), 1 => (`L, 0), _ => (`M, x) });
        select *r { (`A, _) => 1, (`B, _) => 2, (`M, _) => 5, (_, 0) => 3, _ => 4 }
    };
    let g = |x, t: [`A, `B], c: bool| {
        let r = &mut (select c { true => (t, x), false => (`L, "z") });
        select *r { (`A, _) => 1, (`B, _) => 2, (_, "z") => 3, _ => 4 }
    };
    [f(0, `A, 0), f(0, `B, 1), f(7, `A, 2), g("s", `A, true), g("z", `B, true), g("q", `A, false)]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [1, 3, 5, 1, 2, 3]); FuseExpect::Jit);

run!(later_use_decides_a_cell_of_a_tag_pooled_member, r#"{
    let f = |x, t: [`A, `B], c: bool| {
        let r = &mut (select c { true => (t, x), false => (`L, 0) });
        let s = select *r { (`A, _) => 1, (`B, _) => 2, (_, 0) => 3, _ => 4 };
        s * 100 + x + 1
    };
    [f(1, `A, true), f(1, `B, false)]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [102, 302]); FuseExpect::Jit);

run!(arm_behind_half_a_tag_pool_of_a_member_below_a_reference, r#"{
    let f = |x, t: [`A, `B], c: bool| {
        let r = &mut (select c { true => (t, x), false => (`L, 0) });
        select *r { (`A, _) => 1, (_, 0) => 3, _ => 4 }
    };
    f("s", `A, true)
}"#, prints("i64:1"); FuseExpect::Jit);

run!(arm_behind_a_tag_pool_missing_a_tag_of_a_member, r#"{
    let f = |x, t: [`A, `B, `C], c: bool| {
        let r = &mut (select c { true => (t, x), false => (`L, 0) });
        select *r { (`A, _) => 1, (`B, _) => 2, (_, 0) => 3, _ => 4 }
    };
    f("s", `A, true)
}"#, prints("i64:1"); FuseExpect::Jit);

run!(arm_behind_a_guarded_tag_pool_of_a_member, r#"{
    let f = |x, t: [`A, `B], c: bool| {
        let r = &mut (select c { true => (t, x), false => (`L, 0) });
        select *r { (`A, _) => 1, (`B, _) if c => 2, (_, 0) => 3, _ => 4 }
    };
    f("s", `A, true)
}"#, prints("i64:1"); FuseExpect::Jit);

run!(arm_after_a_tag_pool_that_covers_it_is_unreachable, r#"{
    let f = |x, t: [`A, `B], c: bool| {
        let r = &mut (select c { true => (t, x), false => (`L, 0) });
        select *r { (`A, _) => 1, (`B, _) => 2, (`B, 0) => 5, (_, 0) => 3, _ => 4 }
    };
    f(0, `B, true)
}"#, refused("nothing reaches it"); FuseExpect::None);

run!(arm_after_a_tag_pool_that_covers_an_annotated_part_is_unreachable, r#"{
    let f = |x: i64, t: [`A, `B], c: bool| {
        let r = &mut (select c { true => (t, x), false => (`L, 0) });
        select *r { (`A, _) => 1, (`B, _) => 2, (`B, 0) => 5, (_, 0) => 3, _ => 4 }
    };
    f(0, `B, true)
}"#, refused("pattern (`B, 0) will never match: nothing reaches it"); FuseExpect::None);

// A name in a pooled arm holds the member's part as it stands, so the
// arm's test still holds the member.
run!(arm_behind_a_pool_that_binds_the_open_part, r#"{
    let f = |x, b: bool, c: bool| {
        let r = &mut (select c { true => (b, x), false => (`L, 0) });
        select *r { (true, n) => str::len("[n]"), (false, _) => 20, (_, 0) => 30, _ => 40 }
    };
    [f("s", true, true), f(100, true, true), f("s", false, true), f("s", true, false)]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [1, 3, 20, 30]); FuseExpect::Jit);

run!(later_use_decides_a_cell_of_a_pooled_member_below_a_reference, r#"{
    let x = never();
    let b = false;
    let c = true;
    let r = &mut (select c { true => (b, x), false => (`L, 0) });
    let s = select *r { (true, _) => 1, (false, _) => 2, (_, 0) => 3, _ => 4 };
    x <- "s";
    s
}"#, |v: Result<&Value>| matches!(v, Ok(Value::I64(2))); FuseExpect::Jit);

// The pool is what frees the arm behind: half of it, a guarded half, an
// arm between its halves, or a member it does not take, and the arm is
// refused.
run!(arm_behind_half_a_pool_of_a_member_below_a_reference, r#"{
    let f = |x, b: bool, c: bool| {
        let r = &mut (select c { true => (b, x), false => (`L, 0) });
        select *r { (true, _) => 1, (_, 0) => 3, _ => 4 }
    };
    f("s", true, true)
}"#, prints("i64:1"); FuseExpect::Jit);

run!(arm_behind_a_guarded_pool_of_a_member_below_a_reference, r#"{
    let f = |x, b: bool, c: bool| {
        let r = &mut (select c { true => (b, x), false => (`L, 0) });
        select *r { (true, _) if c => 1, (false, _) => 2, (_, 0) => 3, _ => 4 }
    };
    f("s", true, true)
}"#, prints("i64:1"); FuseExpect::Jit);

run!(arm_between_the_halves_of_a_pool_below_a_reference, r#"{
    let f = |x, b: bool, c: bool| {
        let r = &mut (select c { true => (b, x), false => (`L, 0) });
        select *r { (true, _) => 1, (_, 0) => 3, (false, _) => 2, _ => 4 }
    };
    f("s", true, true)
}"#, prints("i64:1"); FuseExpect::Jit);

run!(arm_behind_a_pool_tests_the_member_that_reaches_it, r#"{
    let f = |x, y, b: bool, c: bool| {
        let r = &mut (select c { true => (b, x), false => (`L, y) });
        select *r { (true, _) => 1, (false, _) => 2, (_, 0) => 3, _ => 4 }
    };
    f(1, 0, true, true)
}"#, prints("i64:1"); FuseExpect::Jit);

// The pool of names over a member the arms decided covers it in the walk of
// the scrutinee's own members, so the test behind it meets none of its values
// and decides nothing of its cell beyond the type the test gives it.
run!(arm_behind_a_pool_over_a_cell_the_arms_decided_covers_it, r#"{
    let f = |x, b: bool, c: bool| {
        let r = &mut (select c { true => (b, x), false => (`L, 0) });
        select *r { (true, n) => 1, (false, m) => 2, (_, 0) => 3, _ => 4 }
    };
    [f(0, true, true), f(0, false, true), f(0, true, false)]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [1, 2, 3]); FuseExpect::Jit);

run!(arm_behind_a_pool_over_a_cell_types_nothing_by_its_test, r#"{
    let f = |x, b: bool, c: bool| {
        let r = &mut (select c { true => (b, x), false => (`L, 0) });
        select *r { (true, n) => 1, (false, m) => 2, (_, 0) => 3, _ => 4 }
    };
    f("s", true, true)
}"#, prints("i64:1"); FuseExpect::Jit);

// A use of the pool's bind types the cell its test left open.
run!(arm_behind_a_pool_over_a_cell_a_use_types_refused, r#"{
    let f = |x, b: bool, c: bool| {
        let r = &mut (select c { true => (b, x), false => (`L, 0) });
        select *r { (true, n) => n + 1, (false, m) => 2, (_, 0) => 3, _ => 4 }
    };
    f("s", true, true)
}"#, refused("i64 does not contain string"); FuseExpect::None);

// The arms up to a dead one are judged behind the pool too, so the dead
// arm is what is refused.
run!(dead_arm_behind_a_pool_of_a_member_below_a_reference_refused, r#"{
    let f = |x, b: bool, c: bool| {
        let r = &mut (select c { true => (b, x), false => (`L, 0) });
        select *r { (true, _) => 1, (false, _) => 2, (_, 0) => 3, (true, _) => 5, _ => 4 }
    };
    f(1, true, true)
}"#, refused("nothing reaches it"); FuseExpect::None);

run!(dead_arm_behind_a_pool_of_an_annotated_member_below_a_reference_is_unreachable, r#"{
    let f = |x: i64, b: bool, c: bool| {
        let r = &mut (select c { true => (b, x), false => (`L, 0) });
        select *r { (true, _) => 1, (false, _) => 2, (_, 0) => 3, (true, _) => 5, _ => 4 }
    };
    f(1, true, true)
}"#, refused("pattern (true, _) will never match: nothing reaches it"); FuseExpect::None);

// A pool counts every head the member's part admits: three for a nullable
// bool, and two of the three free no arm behind them.
run!(arm_behind_a_pool_of_three_heads_below_a_reference, r#"{
    let f = |x, b: [bool, null], c: bool| {
        let r = &mut (select c { true => (b, x), false => (`L, 0) });
        select *r { (true, _) => 1, (false, _) => 2, (null, _) => 5, (_, 0) => 3, _ => 4 }
    };
    [f("s", true, true), f("s", null, true), f(0, null, true), f(0, false, true), f(0, true, false)]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [1, 5, 5, 2, 3]); FuseExpect::Jit);

run!(arm_behind_two_heads_of_three_below_a_reference, r#"{
    let f = |x, b: [bool, null], c: bool| {
        let r = &mut (select c { true => (b, x), false => (`L, 0) });
        select *r { (true, _) => 1, (false, _) => 2, (_, 0) => 3, _ => 4 }
    };
    f(0, null, true)
}"#, prints("i64:3"); FuseExpect::Jit);

// Each member of the bound has a pool of its own, and an or-arm's
// alternatives pool each against the member it takes.
run!(arm_behind_the_pools_of_two_members_below_a_reference, r#"{
    let f = |x, y, a: bool, b: bool, c: bool| {
        let r = &mut (select c { true => (a, x), false => (`L, b, y) });
        select *r {
            (true, _) => 1,
            (`L, true, _) => 3,
            (_, 0) => 5,
            (_, _, 0) => 6,
            _ => 7
        }
    };
    let g = |x, b: bool, c: bool| {
        let r = &mut (select c { true => (b, x), false => (`L, 0) });
        select *r { (true, _) | (`L, 1) => 1, (false, _) => 2, (_, 0) => 3, _ => 4 }
    };
    [
        f("s", "t", true, true, true), f("s", "t", false, true, true),
        f("s", "t", true, true, false), f(0, 0, false, false, false),
        g("s", true, true), g("s", false, true), g("s", true, false), g(0, true, true)
    ]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [1, 7, 3, 6, 1, 2, 3, 1]); FuseExpect::Jit);

// The arms that take a member's two pools take the member: an arm behind
// them is dead, whatever the part nothing decided turns out to be.
run!(arm_behind_both_pools_of_each_member_below_a_reference_is_dead, r#"{
    let f = |x, y, a: bool, b: bool, c: bool| {
        let r = &mut (select c { true => (a, x), false => (`L, b, y) });
        select *r {
            (true, _) => 1,
            (false, _) => 2,
            (`L, true, _) => 3,
            (`L, false, _) => 4,
            (_, 0) => 5,
            _ => 7
        }
    };
    f("s", "t", true, true, true)
}"#, refused("pattern (_, 0) will never match: nothing reaches it"); FuseExpect::None);

// The member is the one the bound holds, however it was built: a binding,
// a call's result, the open cell inside a part of it.
run!(arm_behind_a_pool_of_a_member_built_elsewhere_below_a_reference, r#"{
    let mk = |b: bool, x| (b, x);
    let f = |x, b: bool, c: bool| {
        let p = (b, x);
        let r = &mut (select c { true => p, false => (`L, 0) });
        select *r { (true, _) => 1, (false, _) => 2, (_, 0) => 3, _ => 4 }
    };
    let g = |x, b: bool, c: bool| {
        let r = &mut (select c { true => mk(b, x), false => (`L, 0) });
        select *r { (true, _) => 1, (false, _) => 2, (_, 0) => 3, _ => 4 }
    };
    let h = |x, b: bool, c: bool| {
        let r = &mut (select c { true => (b, {v: [x]}), false => (`L, {v: [0]}) });
        select *r { (true, _) => 1, (false, _) => 2, (_, {v: [0]}) => 3, _ => 4 }
    };
    [
        f("s", true, true), f("s", false, true), f("s", true, false), f(0, true, true),
        g("s", true, true), g("s", false, true), g("s", true, false), g(0, true, true),
        h("s", true, true), h("s", false, true), h("s", true, false), h(0, true, true)
    ]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [1, 2, 3, 1, 1, 2, 3, 1, 1, 2, 3, 1]); FuseExpect::Jit);

// The select stands where the reference is read: a closure, a seq step,
// an arm entered later.
run!(arm_behind_a_pool_in_a_closure_a_step_and_an_arm, r#"{
    let f = |x, b: bool, c: bool| {
        let r = &mut (select c { true => (b, x), false => (`L, 0) });
        let g = |u: null| select *r { (true, _) => 1, (false, _) => 2, (_, 0) => 3, _ => 4 };
        g(null)
    };
    let g = |x, b: bool, c: bool| {
        let r = &mut (select c { true => (b, x), false => (`L, 0) });
        seq c {
            let s = select *r { (true, _) => 1, (false, _) => 2, (_, 0) => 3, _ => 4 };
            s + 10
        }
    };
    let h = |x, b: bool, c: bool, k: i64| {
        let r = &mut (select c { true => (b, x), false => (`L, 0) });
        select k {
            0 => 0,
            n => n ~ select *r { (true, _) => 1, (false, _) => 2, (_, 0) => 3, _ => 4 }
        }
    };
    [
        f("s", true, true), f("s", false, true), f("s", true, false), f(0, true, true),
        g("s", true, true), g("s", false, true), g("s", true, false), g(0, true, true),
        h("s", true, true, 1), h("s", false, true, 1), h("s", true, false, 1), h(0, true, true, 0)
    ]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [1, 2, 3, 1, 11, 12, 13, 11, 1, 2, 3, 0]); FuseExpect::Jit);

run!(arm_behind_a_cover_tests_the_member_that_reaches_it, r#"{
    let f = |x, y, b: bool| {
        let r = &mut (select b { true => (`K, x), false => (`L, y) });
        select *r { (`K, _) => 1, (_, 0) => 2, _ => 3 }
    };
    [f(1, 0, true), f(1, 0, false)]
}"#, prints("[i64:1, i64:2]"); FuseExpect::Jit);

run!(arm_behind_a_cover_of_another_member_of_its_cell, r#"{
    let f = |x, b: bool| {
        let r = &mut (select b { true => (`K, x), false => (`L, x) });
        select *r { (`K, _) => 1, (_, 0) => 2, _ => 3 }
    };
    [f(1, true), f(1, false)]
}"#, prints("[i64:1, i64:3]"); FuseExpect::Jit);

run!(arm_behind_a_guarded_arm_over_a_member_below_a_reference, r#"{
    let f = |x, b: bool| {
        let r = &mut (select b { true => (`K, x), false => (`L, 0) });
        select *r { (`K, _) if b => 1, (_, 0) => 2, _ => 3 }
    };
    [f(1, true), f(1, false)]
}"#, prints("[i64:1, i64:2]"); FuseExpect::Jit);

run!(arm_before_the_cover_of_a_member_below_a_reference, r#"{
    let f = |x, b: bool| {
        let r = &mut (select b { true => (`K, x), false => (`L, 0) });
        select *r { (_, 0) => 2, (`K, _) => 1, _ => 3 }
    };
    [f(1, true), f(1, false)]
}"#, prints("[i64:1, i64:2]"); FuseExpect::Jit);

run!(covering_arm_testing_its_member_below_a_reference, r#"{
    let f = |x, b: bool| {
        let r = &mut (select b { true => (`K, x), false => (`L, 0) });
        select *r { (`K, i64) as (_, n) => n, (_, 0) => 2, _ => 3 }
    };
    [f(1, true), f(1, false)]
}"#, prints("[i64:1, i64:2]"); FuseExpect::Jit);

// A later use decides the covered member's cell apart from what the arm
// behind the cover tests: that arm meets none of its values.
run!(later_use_decides_a_cell_of_a_covered_member_below_a_reference, r#"{
    let x = never();
    let b = false;
    let r = &mut (select b { true => (x, `K), false => (0, `L) });
    let s = select *r { (_, `K) => 1, (0, _) => 2, _ => 3 };
    x <- "s";
    s
}"#, |v: Result<&Value>| matches!(v, Ok(Value::I64(2))); FuseExpect::Jit);

run!(later_use_decides_a_cell_of_a_covered_member_tag_first, r#"{
    let x = never();
    let b = true;
    let r = &mut (select b { true => (`K, x), false => (`L, 0) });
    let s = select *r { (`K, _) => 1, (_, 0) => 2, _ => 3 };
    x <- "s";
    s
}"#, |v: Result<&Value>| matches!(v, Ok(Value::I64(1))); FuseExpect::Jit);

// The arms up to a dead one are judged behind their covers too, so the
// dead arm is what is refused.
run!(dead_arm_behind_a_cover_of_a_member_below_a_reference_refused, r#"{
    let f = |x, b: bool| {
        let r = &mut (select b { true => (x, null), false => (`L, 0) });
        select *r { (_, null) => 1, (`L, _) => 2, _ => 3 }
    };
    [f("s", true), f("s", false)]
}"#, refused("nothing reaches it"); FuseExpect::None);

run!(dead_arm_behind_a_cover_of_an_annotated_member_below_a_reference_is_unreachable, r#"{
    let f = |x: string, b: bool| {
        let r = &mut (select b { true => (x, null), false => (`L, 0) });
        select *r { (_, null) => 1, (`L, _) => 2, _ => 3 }
    };
    [f("s", true), f("s", false)]
}"#, refused("pattern _ will never match: nothing reaches it"); FuseExpect::None);

run!(tag_testing_a_declared_variable_below_a_reference, r#"{
    let f = |x: 'a| { let r = &mut [x, 0]; select *r { [`J, _] => 1, _ => 3 } };
    [f(`M), f(`J)]
}"#, prints("[i64:3, i64:1]"); FuseExpect::None);

run!(tag_testing_a_declared_variable_the_referent_keeps, r#"{
    let f = |x: 'a| { let r = &mut [x, 0]; select *r { [`J, ..] => 1, _ => 3 } };
    [f(`M), f(`J)]
}"#, prints("[i64:3, i64:1]"); FuseExpect::None);

run!(tag_testing_a_bounded_variable_below_a_reference, r#"{
    let f = 'a: [`J, `M] |x: 'a| { let r = &mut [x, 0]; select *r { [`J, _] => 1, _ => 3 } };
    [f(`M), f(`J)]
}"#, prints("[i64:3, i64:1]"); FuseExpect::None);

run!(written_test_over_a_bounded_variable_below_a_reference, r#"{
    let f = 'a: [`J, `M] |x: 'a| {
        let r = &mut [x, 0];
        select *r { Array<[i64, `J, `M]> as [`J, _] => 1, _ => 3 }
    };
    [f(`M), f(`J)]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [3, 1]); FuseExpect::None);

run!(naming_a_declared_variable_below_a_reference, r#"{
    let f = |x: 'a| { let r = &mut [x, 0]; select *r { [a, _] => 1, _ => 3 } };
    [f(`M), f("s")]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [1, 1]); FuseExpect::None);

run!(tag_testing_a_declared_variable_in_a_tuple_below_a_reference, r#"{
    let f = |x: 'a| { let r = &mut (x, 0); select *r { (`J, _) => 1, _ => 3 } };
    [f(`M), f(`J)]
}"#, prints("[i64:3, i64:1]"); FuseExpect::None);

run!(tag_testing_a_declared_variable_below_a_read_only_reference, r#"{
    let f = |x: 'a| { let r = &[x, 0]; select *r { [`J, _] => 1, _ => 3 } };
    [f(`M), f(`J)]
}"#, prints("[i64:3, i64:1]"); FuseExpect::None);

run!(literal_testing_a_declared_variable_below_a_reference, r#"{
    let f = |x: 'a| { let r = &mut [x, 0]; select *r { [0, _] => 1, _ => 3 } };
    [f(1), f(0)]
}"#, prints("[i64:3, i64:1]"); FuseExpect::None);

run!(tag_testing_a_declared_variable_below_an_abstract_parameter, r#"{
    type W<'a> = Abstract<'a>;
    let f = |x: 'a| select W([x, 0]).0 { [`J, _] => 1, _ => 3 };
    [f(`M), f(`J)]
}"#, prints("[i64:3, i64:1]"); FuseExpect::Jit);

run!(written_test_over_a_bounded_variable_below_an_abstract_parameter, r#"{
    type W<'a> = Abstract<'a>;
    let f = 'a: [`J, `M] |x: 'a| select W([x, 0]).0 {
        Array<[i64, `J, `M]> as [`J, _] => 1,
        _ => 3
    };
    [f(`M), f(`J)]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [3, 1]); FuseExpect::Jit);

run!(tag_testing_a_declared_variable_a_write_brings, r#"{
    let f = |x: 'a| { let v = never(); v <- [x, 0]; select v { [`J, _] => 1, _ => 3 } };
    [f(`M), f(`J)]
}"#, prints("[i64:3, i64:1]"); FuseExpect::Jit);

run!(written_test_holding_a_wildcard_over_a_variable_below_a_reference, r#"{
    let f = 'b: [`A(i64), `B] |x: 'b| {
        let r = &mut (x, 0);
        select *r { (`A(_), i64) as (`A(z), _) => 1, _ => 2 }
    };
    [f(`B), f(`A(7))]
}"#, prints("[i64:2, i64:1]"); FuseExpect::None);

run!(written_test_holding_its_own_variable_below_a_reference, r#"{
    let f = 'b: [`A(i64), `B] |x: 'b| {
        let r = &mut (x, 0);
        select *r { ('c, i64) as (`A(z), _) => 1, _ => 2 }
    };
    [f(`B), f(`A(7))]
}"#, prints("[i64:2, i64:1]"); FuseExpect::None);

run!(closed_written_test_over_a_variable_below_a_reference, r#"{
    let f = 'b: [`A(i64), `B] |x: 'b| {
        let r = &mut (x, 0);
        select *r { ([`A(i64), `B], i64) as (`A(z), _) => z, _ => 2 }
    };
    [f(`B), f(`A(7))]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [2, 7]); FuseExpect::None);

// A destructuring `let` meets the variable below a bound as a select does.
run!(let_binding_inside_a_declared_variable_below_a_reference_takes_its_bound, r#"{
    let f = 'a: (i64, i64) |x: 'a| { let r = &mut (x, 0); let ((a, b), c) = *r; a + b };
    f((1, 2))
}"#, prints("i64:3"); FuseExpect::Jit);

run!(annotated_let_over_a_declared_variable_below_a_reference, r#"{
    let f = 'a: (i64, i64) |x: 'a| {
        let r = &mut (x, 0);
        let ((a, b), c): ((i64, i64), i64) = *r;
        a + b
    };
    f((1, 2))
}"#, |v: Result<&Value>| matches!(v, Ok(Value::I64(3))); FuseExpect::Jit);

run!(let_binding_inside_an_unbounded_variable_below_a_reference_refused, r#"{
    let f = |x: 'a| { let r = &mut (x, 0); let ((a, b), c) = *r; c };
    f((1, 2))
}"#, refused("type mismatch (Any, Any) does not contain 'a"); FuseExpect::None);

run!(let_binding_inside_a_declared_variable_a_read_passes_on_takes_its_bound, r#"{
    let f = 'a: (i64, i64) |x: 'a| {
        let r = &mut (x, 0);
        let y = *r;
        let ((a, b), c) = y;
        a + b
    };
    f((1, 2))
}"#, prints("i64:3"); FuseExpect::Jit);

run!(let_binding_inside_a_declared_variable_in_a_struct_below_a_reference_takes_its_bound, r#"{
    let f = 'a: {p: i64} |x: 'a| { let r = &mut {q: x}; let {q: {p}} = *r; p };
    f({p: 1})
}"#, prints("i64:1"); FuseExpect::None);

run!(let_binding_inside_a_declared_variable_below_an_abstract_parameter_takes_its_bound, r#"{
    type W<'a> = Abstract<'a>;
    let f = 'a: (i64, i64) |x: 'a| { let ((a, b), c) = W((x, 0)).0; a + b };
    f((1, 2))
}"#, prints("i64:3"); FuseExpect::Jit);

run!(annotated_let_over_a_declared_variable_below_an_abstract_parameter, r#"{
    type W<'a> = Abstract<'a>;
    let f = 'a: (i64, i64) |x: 'a| {
        let ((a, b), c): ((i64, i64), i64) = W((x, 0)).0;
        a + b
    };
    f((1, 2))
}"#, |v: Result<&Value>| matches!(v, Ok(Value::I64(3))); FuseExpect::Jit);

run!(let_naming_a_declared_variable_below_a_reference, r#"{
    let f = |x: 'a| { let r = &mut (x, (1, 2)); let (p, (c, d)) = *r; c + d };
    [f(`M), f("s")]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [3, 3]); FuseExpect::Jit);

// A dead arm is refused as dead: nothing reaches it, and the bounds hold
// what the arms before it took.
run!(dead_arm_testing_an_open_part_below_a_reference_is_unreachable, r#"{
    let f = |x| { let r = &mut [x, 0]; select *r { _ => 3, [`J, _] => 1 } };
    f(`M)
}"#, refused("nothing reaches it"); FuseExpect::None);

// Over a decided part an arm behind a whole type or tag is dead (3a).
run!(dead_arm_testing_an_annotated_open_part_below_a_reference_is_unreachable, r#"{
    let f = |x: [`J, `M]| { let r = &mut [x, 0]; select *r { _ => 3, [`J, _] => 1 } };
    f(`M)
}"#, refused("pattern [`J, _] will never match: nothing reaches it"); FuseExpect::None);

run!(dead_arm_deciding_a_part_below_a_reference_is_unreachable, r#"{
    let f = |x| { let r = &mut (x, 0); select *r { _ => 3, (`J, _) => 1 } };
    f(`M)
}"#, refused("nothing reaches it"); FuseExpect::None);

run!(dead_arm_testing_an_annotated_part_below_a_reference_is_unreachable, r#"{
    let f = |x: [`J, `M]| { let r = &mut (x, 0); select *r { _ => 3, (`J, _) => 1 } };
    f(`M)
}"#, refused("pattern (`J, _) will never match: nothing reaches it"); FuseExpect::None);

// The referent of a reference to an expression no binding names is a cell
// no source can write: a hint names a binding holding the reference.
run!(tag_testing_a_referent_of_an_undecided_part, r#"{
    let f = |x| { let r = &mut (x, 0); select (*r, 1) { ((`J, _), _) => 1, ((`M, _), _) => 3 } };
    f(`M)
}"#, prints("i64:3"); FuseExpect::None);

run!(tag_over_a_referent_of_an_annotated_part, r#"{
    let f = |x: [`J, `M]| { let r = &mut (x, 0); select (*r, 1) { ((`J, _), _) => 1, ((`M, _), _) => 3 } };
    f(`M)
}"#, prints("i64:3"); FuseExpect::None);

run!(tag_over_an_unannotated_referent_through_a_copied_reference, r#"{
    let f = |x| {
        let r = &mut (x, 0);
        let s = r;
        select (*s, 1) { ((`J, _), _) => 1, ((`M, _), _) => 3 }
    };
    f(`M)
}"#, prints("i64:3"); FuseExpect::None);

run!(tag_testing_a_referent_in_a_struct, r#"{
    let f = |x| {
        let st = {r: &mut (x, 0), n: 1};
        select (*st.r, 1) { ((`J, _), _) => 1, ((`M, _), _) => 3 }
    };
    f(`M)
}"#, prints("i64:3"); FuseExpect::None);

run!(tag_over_an_unannotated_referent_nothing_holds, r#"{
    let f = |x| select (*(&mut (x, 0)), 1) { ((`J, _), _) => 1, ((`M, _), _) => 3 };
    f(`M)
}"#, prints("i64:3"); FuseExpect::None);

run!(tag_over_a_referent_read_into_an_annotated_binding, r#"{
    let f = |x| {
        let r = &mut (x, 0);
        let y: ([`J, `M], i64) = *r;
        select (y, 1) { ((`J, _), _) => 1, ((`M, _), _) => 3 }
    };
    [f(`M), f(`J)]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [3, 1]); FuseExpect::Jit);

run!(tag_over_a_referent_read_into_an_unannotated_binding, r#"{
    let f = |x| {
        let r = &mut (x, 0);
        let y = *r;
        select (y, 1) { ((`J, _), _) => 1, ((`M, _), _) => 3 }
    };
    f(`M)
}"#, prints("i64:3"); FuseExpect::Jit);

run!(tag_over_an_unannotated_referent_through_a_call, r#"{
    let f = |x| {
        let r = &mut (x, 0);
        let g = |q: &mut ('a, i64)| *q;
        select (g(r), 1) { ((`J, _), _) => 1, ((`M, _), _) => 3 }
    };
    f(`M)
}"#, prints("i64:3"); FuseExpect::None);

run!(tag_over_an_annotated_reference, r#"{
    let f = |x| {
        let r: &mut ([`J, `M], i64) = &mut (x, 0);
        select (*r, 1) { ((`J, _), _) => 1, ((`M, _), _) => 3 }
    };
    [f(`M), f(`J)]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [3, 1]); FuseExpect::None);

// A cell the bounds hold whole is the arms' to decide, as a whole open
// scrutinee is.
run!(tags_deciding_a_whole_abstract_parameter, r#"{
    type W<'a> = Abstract<'a>;
    let f = |x| select W(x).0 { `J => 1, `M => 3 };
    [f(`M), f(`J)]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [3, 1]); FuseExpect::Jit);

run!(tuple_tags_deciding_a_whole_abstract_parameter, r#"{
    type W<'a> = Abstract<'a>;
    let f = |x| select W(x).0 { (`J, _) => 1, (`M, _) => 3 };
    [f((`M, 1)), f((`J, 1))]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [3, 1]); FuseExpect::Jit);

run!(partial_struct_over_a_whole_abstract_parameter, r#"{
    type W<'a> = Abstract<'a>;
    let f = |x| select W(x).0 { {a: `J, ..} => 1, _ => 3 };
    f({a: `M, b: 1})
}"#, prints("i64:3"); FuseExpect::Jit);

run!(destructuring_a_part_below_an_abstract_parameter, r#"{
    type W<'a> = Abstract<'a>;
    let f = |x| select W((x, 0)).0 { ((a, b), _) => a + b };
    f((1, 2))
}"#, |v: Result<&Value>| matches!(v, Ok(Value::I64(3))); FuseExpect::Jit);

// A pattern that chooses a value, a tag or a length at an undecided part
// decides it to that choice: refused as testing it.
run!(bool_literals_testing_an_undecided_part, r#"{
    let f = |x| select (x, 0) { (true, _) => 1, (false, _) => 2 };
    f(true)
}"#, prints("i64:1"); FuseExpect::Jit);

run!(bool_literals_over_an_annotated_part, r#"{
    let f = |x: bool| select (x, 0) { (true, _) => 1, (false, _) => 2 };
    [f(true), f(false)]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [1, 2]); FuseExpect::Jit);

run!(literal_testing_an_undecided_part, r#"{
    let f = |x| select (x, 0) { (1, _) => 1, _ => 2 };
    f(2)
}"#, prints("i64:2"); FuseExpect::Jit);

run!(empty_slice_testing_an_undecided_part, r#"{
    let f = |x| select (x, 0) { ([], _) => 1, _ => 2 };
    f([1])
}"#, prints("i64:2"); FuseExpect::Jit);

run!(fixed_length_slice_testing_an_undecided_part, r#"{
    let f = |x| select (x, 0) { ([a, b], _) => a + b, (_, _) => 0 };
    f([1, 2])
}"#, prints("i64:3"); FuseExpect::Jit);

run!(fixed_length_slice_over_an_annotated_part, r#"{
    let f = |x: Array<i64>| select (x, 0) { ([a, b], _) => a + b, (_, _) => 0 };
    [f([1, 2]), f([1])]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [3, 0]); FuseExpect::Jit);

run!(list_head_testing_an_undecided_part, r#"{
    let f = |x| select (x, 0) { ([<`J, ..>], _) => 1, _ => 2 };
    f([<`M>])
}"#, prints("i64:2"); FuseExpect::Jit);

run!(or_pattern_testing_an_undecided_part, r#"{
    let f = |x| select (x, 0) { ((a, b) | (a, b, _), _) => a + b };
    f((1, 2))
}"#, prints("i64:3"); FuseExpect::Jit);

run!(partial_struct_testing_an_undecided_part, r#"{
    let f = |x| select (x, 0) { ({a, ..}, _) => a };
    f({a: 1, b: 2})
}"#, prints("i64:1"); FuseExpect::Jit);

// A destructure decides the part's shape as an annotation would: an arm of
// another shape after it is dead.
run!(other_shape_than_an_earlier_destructure_is_live, r#"{
    let f = |x| select (x, 0) { ((a, b), _) => a + b, ((a, b, c), _) => a };
    f((1, 2))
}"#, prints("i64:3"); FuseExpect::Jit);

run!(other_shape_than_an_annotated_part_is_dead, r#"{
    let f = |x: (i64, i64)| select (x, 0) { ((a, b), _) => a + b, ((a, b, c), _) => a };
    f((1, 2))
}"#, refused("pattern ((a, b, c), _) will never match: nothing reaches it"); FuseExpect::None);

// A literal at a union position decides the undecided member beside the
// one that holds its type.
run!(tag_at_a_union_position_testing_an_undecided_member, r#"{
    let f = |b: bool, x| {
        let y = select b { true => x, false => `J };
        select (y, 0) { (`J, _) => 0, (v, _) => 1 }
    };
    f(true, `M)
}"#, prints("i64:1"); FuseExpect::Jit);

run!(tag_at_a_union_position_over_an_annotated_member, r#"{
    let f = |b: bool, x| {
        let y: [`J, `M] = select b { true => x, false => `J };
        select (y, 0) { (`J, _) => 0, (v, _) => 1 }
    };
    [f(true, `M), f(false, `M)]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [1, 0]); FuseExpect::Jit);

run!(null_at_a_union_position_testing_an_undecided_member, r#"{
    let f = |b: bool, x| {
        let y = select b { true => x, false => null };
        select (y, 0) { (null, _) => 0, (v, _) => 1 }
    };
    f(true, `M)
}"#, prints("i64:1"); FuseExpect::Jit);

// A destructure that takes the member beside an undecided one leaves it
// undecided: refused for it, also where a later arm is refused first.
run!(destructure_beside_an_undecided_member_before_a_catch_all, r#"{
    let f = |x, c| {
        let y = select c { true => x, false => (1, 2) };
        select (y, 0) { ((a, b), _) => a + b, _ => 0 }
    };
    f((5, 1), true)
}"#, prints("i64:6"); FuseExpect::Jit);

run!(struct_beside_an_undecided_member_before_a_catch_all, r#"{
    let f = |x, c| {
        let y = select c { true => x, false => {a: 1, b: 2} };
        select (y, 0) { ({a, b}, _) => a + b, _ => 0 }
    };
    f({a: 5, b: 1}, true)
}"#, prints("i64:6"); FuseExpect::Jit);

run!(whole_holder_destructured_before_a_dead_arm, r#"{
    let f = |x, c| {
        let y = select c { true => x, false => (1, 2) };
        select y { (a, b) => a + b, _ => 0 }
    };
    f((5, 1), true)
}"#, prints("i64:6"); FuseExpect::Jit);

run!(destructure_beside_an_annotated_member, r#"{
    let f = |x: [(i64, i64), null], c| {
        let y = select c { true => x, false => (1, 2) };
        select (y, 0) { ((a, b), _) => a + b, _ => 0 }
    };
    [f((5, 1), true), f(null, true), f(null, false)]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [6, 0, 3]); FuseExpect::Jit);

// A body's error in one arm is that error, not a refusal of a later arm
// judged as if no arm before it took anything.
run!(a_body_error_before_a_later_arm_is_the_error, r#"{
    let f = |z, c| {
        let y = select c { true => (`J, z), false => (`K, 1) };
        select y { (`J, _) => 0, (`K, n) => str::len(n), (_, 3) => 2, _ => 3 }
    };
    f(4, true)
}"#, refused("type mismatch string does not contain i64"); FuseExpect::None);

run!(a_later_arm_after_a_whole_member_taken, r#"{
    let f = |z, c| {
        let y = select c { true => (`J, z), false => (`K, 1) };
        select y { (`J, _) => 0, (`K, 7) => 1, (_, 3) => 2, _ => 3 }
    };
    [f(4, true), f(4, false)]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [0, 3]); FuseExpect::Jit);

// An arm's own error is the select's, never a refusal of another arm for
// what the arms decided: a body's, a guard's, after a literal or a
// destructure.
run!(a_body_error_after_a_literal_arm_is_the_error, r#"{
    let f = |z, c| {
        let y = select c { true => z, false => 1 };
        select y { 3 => 0, i64 as n => n + "x", s => str::len(s) }
    };
    f("ab", true)
}"#, refused("type mismatch i64 does not contain string"); FuseExpect::None);

run!(a_guard_error_after_a_literal_arm_is_the_error, r#"{
    let f = |z, c| {
        let y = select c { true => z, false => 1 };
        select y { 3 => 0, i64 as n if n + "x" == 2 => n, i64 as n => n, s => str::len(s) }
    };
    f("ab", true)
}"#, refused("type mismatch i64 does not contain string"); FuseExpect::None);

run!(a_body_error_after_a_destructuring_arm_is_the_error, r#"{
    let f = |z, c| {
        let y = select c { true => (`J, z), false => (`K, 1) };
        select y { (`J, (a, b)) => 0, (`K, n) => str::len(n), (_, 3) => 2, _ => 3 }
    };
    f((4, 5), true)
}"#, refused("type mismatch string does not contain i64"); FuseExpect::None);

run!(arms_after_a_literal_arm_run, r#"{
    let f = |z, c| {
        let y = select c { true => z, false => 1 };
        select y { 3 => 0, i64 as n => n + 1, s => str::len(s) }
    };
    [f("ab", true), f("ab", false)]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [2, 2]); FuseExpect::Jit);

run!(arms_after_a_destructuring_arm_run, r#"{
    let f = |z, c| {
        let y = select c { true => (`J, z), false => (`K, 1) };
        select y { (`J, (a, b)) => 0, (`K, 7) => 1, (_, 3) => 2, _ => 3 }
    };
    [f((4, 5), true), f((4, 5), false)]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [0, 3]); FuseExpect::Jit);

// Beside a member of its shape a destructure chooses between that member
// and the undecided part, so it tests the part, whether the part stays
// open or takes the member's type: an abstract payload, a ladder of
// arms, a slice with a rest.
run!(abstract_payload_beside_an_undecided_member, r#"{
    type W = Abstract<i64>;
    let f = |x, c| {
        let y = select c { true => x, false => W(1) };
        select (y, 0) { (W(a), _) => a, _ => 0 }
    };
    f(W(5), true)
}"#, prints("i64:5"); FuseExpect::Jit);

run!(abstract_payload_beside_an_annotated_member, r#"{
    type W = Abstract<i64>;
    let f = |x: [W, null], c| {
        let y = select c { true => x, false => W(1) };
        select (y, 0) { (W(a), _) => a, _ => 0 }
    };
    [f(W(5), true), f(null, true), f(null, false)]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [5, 0, 1]); FuseExpect::Jit);

run!(destructure_ladder_beside_an_undecided_member, r#"{
    let f = |x, c| {
        let y = select c { true => x, false => (1, 2) };
        select (y, 0) { ((a, b), 0) => a + b, ((a, b), _) => a - b, _ => 0 }
    };
    f((5, 1), true)
}"#, prints("i64:6"); FuseExpect::Jit);

run!(destructure_ladder_beside_an_annotated_member, r#"{
    let f = |x: [(i64, i64), null], c| {
        let y = select c { true => x, false => (1, 2) };
        select (y, 0) { ((a, b), 0) => a + b, ((a, b), _) => a - b, _ => 0 }
    };
    [f((5, 1), true), f(null, true), f(null, false)]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [6, 0, 3]); FuseExpect::Jit);

run!(slice_with_a_rest_beside_an_undecided_member, r#"{
    let f = |x, c| {
        let y = select c { true => x, false => [1, 2] };
        select (y, 0) { ([a, ..], _) => a, _ => 0 }
    };
    f([5], true)
}"#, prints("i64:5"); FuseExpect::Jit);

run!(slice_with_a_rest_beside_an_annotated_member, r#"{
    let f = |x: [Array<i64>, null], c| {
        let y = select c { true => x, false => [1, 2] };
        select (y, 0) { ([a, ..], _) => a, _ => 0 }
    };
    [f([5], true), f(null, true), f(null, false)]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [5, 0, 1]); FuseExpect::Jit);

// A structure under a slice's element only takes apart the element the
// slice decides: no test of the part, which the element's runtime form
// refuses instead.
run!(destructure_under_a_slice_element_is_no_undecided_test, r#"{
    let f = |x| select (x, 0) { ([(a, b), ..], _) => a, _ => 0 };
    f([(1, 2)])
}"#, prints("i64:1"); FuseExpect::Jit);

run!(destructure_under_an_annotated_slice_element, r#"{
    let f = |x: Array<(i64, i64)>| select (x, 0) { ([(a, b), ..], _) => a, _ => 0 };
    [f([(1, 2)]), f([])]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [1, 0]); FuseExpect::Jit);

run!(undecided_index_part_unannotated, r#"{
    let f = |x| select (x[0]$, 0) { (`J, _) => 1, _ => 2 };
    f([`M])
}"#, prints("i64:2"); FuseExpect::Jit);

run!(undecided_index_part_annotated, r#"{
    let f = |x: Array<[`J, `M]>| select (x[0]$, 0) { (`J, _) => 1, _ => 2 };
    [f([`M]), f([`J])]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [2, 1]); FuseExpect::Jit);

run!(undecided_nested_index_part_unannotated, r#"{
    let f = |x| select ((x[0]$)[0]$, 0) { (`J, _) => 1, _ => 2 };
    f([[`M]])
}"#, prints("i64:2"); FuseExpect::Jit);

run!(undecided_part_of_a_function_a_struct_holds, r#"{
    let s = {g: |x| select (x[0]$, 0) { (`J, _) => 1, _ => 2 }};
    s.g([`M])
}"#, prints("i64:2"); FuseExpect::None);

run!(undecided_abstract_payload_over_an_unannotated_argument, r#"{
    type W<'a> = Abstract<'a>;
    let f = |x| select (W(x), 0) { (W(`J), _) => 1, _ => 2 };
    f(`M)
}"#, prints("i64:2"); FuseExpect::Jit);

run!(undecided_abstract_payload_over_an_annotated_argument, r#"{
    type W<'a> = Abstract<'a>;
    let f = |x: [`J, `M]| select (W(x), 0) { (W(`J), _) => 1, _ => 2 };
    f(`M)
}"#, prints("i64:2"); FuseExpect::Jit);

run!(
    undecided_abstract_payload_annotated_where_built,
    r#"{
    type W<'a> = Abstract<'a>;
    let f = |x| { let w: W<[`J, `M]> = W(x); select (w, 0) { (W(`J), _) => 1, _ => 2 } };
    [f(`M), f(`J)]
}"#,
    |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [2, 1])
);

run!(undecided_part_of_a_destructured_binding, r#"{
    let f = |x| select (x, 0) { ((a, b), _) => select (a, 0) { (`J, _) => 1, _ => 0 }, _ => 0 };
    f((`J, 1))
}"#, prints("i64:1"); FuseExpect::Jit);

run!(undecided_part_of_a_destructured_binding_annotated, r#"{
    let f = |x: ([`J, `M], i64)| select (x, 0) { ((a, b), _) => select (a, 0) { (`J, _) => 1, _ => 0 } };
    [f((`J, 1)), f((`M, 1))]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [1, 0]); FuseExpect::Jit);

run!(undecided_part_reached_through_a_pattern_alias, r#"{
    let f = |x| select (x, 0) { (c@ (a, b), _) => select c { (1, d) => d, _ => 0 } };
    f((5, 1))
}"#, prints("i64:0"); FuseExpect::Jit);

run!(undecided_part_through_an_intermediate_binding, r#"{
    let f = |x, c| {
        let y = select c { true => x[0]$, false => (1, 2) };
        select (y, 0) { ((a, b), 0) => a + b, ((a, b), _) => a - b, _ => 0 }
    };
    f([(5, 1)], true)
}"#, prints("i64:6"); FuseExpect::Jit);

run!(slice_element_binds_a_member_written_later_refused, r#"{
    let v = never();
    let z = select v { [`J(n), ..] => n + 1, _ => 3 };
    v <- [`J("s"), `K];
    z
}"#, refused("type mismatch i64 does not contain string"); FuseExpect::None);

run!(slice_element_beside_a_member_written_later_it_does_not_take, r#"{
    let v = never();
    let z = select v { [`J(n), ..] => n + 1, _ => 3 };
    v <- [`J(1), `K];
    z
}"#, |v: Result<&Value>| matches!(v, Ok(Value::I64(2))); FuseExpect::Jit);

run!(slice_element_binds_a_member_of_an_annotated_scrutinee, r#"{
    let v: Array<[`J(i64), `J(string), `K]> = never();
    let z = select v {
        [`J(n), ..] => select n { i64 as i => i + 1, string as s => str::len(s) },
        _ => 3
    };
    v <- [`J("abc"), `K];
    z
}"#, |v: Result<&Value>| matches!(v, Ok(Value::I64(3))); FuseExpect::Jit);

run!(slice_element_binds_a_member_written_later_inside_a_union_refused, r#"{
    let v = never();
    let z = select v { [`J(n), ..] => n + 1, _ => 3 };
    v <- [`J(1), `J("s")];
    z
}"#, refused("type mismatch i64 does not contain [i64, string]"); FuseExpect::None);

run!(slice_element_binds_a_tuple_written_later_refused, r#"{
    let v = never();
    let z = select v { [(n, m), ..] => n + 1, _ => 3 };
    v <- [("s", 1), (1, 1)];
    z
}"#, refused("type mismatch i64 does not contain [i64, string]"); FuseExpect::None);

run!(slice_element_binds_a_struct_written_later_refused, r#"{
    let v = never();
    let z = select v { [{a}, ..] => a + 1, _ => 3 };
    v <- [{a: "s"}, {a: 1}];
    z
}"#, refused("type mismatch i64 does not contain [i64, string]"); FuseExpect::None);

run!(list_element_binds_a_member_written_later_refused, r#"{
    let v = never();
    let z = select v { [<`J(n), ..>] => n + 1, _ => 3 };
    v <- [<`J("s"), 0>];
    z
}"#, refused("type mismatch i64 does not contain string"); FuseExpect::None);

run!(slice_suffix_element_binds_a_member_written_later_refused, r#"{
    let v = never();
    let z = select v { [.., `J(n)] => n + 1, _ => 3 };
    v <- [`K, `J("s")];
    z
}"#, refused("type mismatch i64 does not contain string"); FuseExpect::None);

run!(slice_element_beside_a_named_rest_binds_a_member_written_later_refused, r#"{
    let v = never();
    let z = select v { [`J(n), t..] => n + 1, _ => 3 };
    v <- [`J("s"), `K];
    z
}"#, refused("type mismatch i64 does not contain string"); FuseExpect::None);

run!(slice_element_capture_binds_a_member_written_later_refused, r#"{
    let v = never();
    let z = select v { [x@ `J(n), ..] => n + 1, _ => 3 };
    v <- [`J("s"), `K];
    z
}"#, refused("type mismatch i64 does not contain string"); FuseExpect::None);

run!(slice_element_under_a_tuple_binds_a_member_written_later_refused, r#"{
    let v = never();
    let z = select v { (1, [`J(n), ..]) => n + 1, _ => 3 };
    v <- (1, [`J("s"), `K]);
    z
}"#, refused("type mismatch i64 does not contain string"); FuseExpect::None);

// A literal the element tests takes only the members that may hold it.
run!(slice_element_beside_a_literal_binds_a_member_written_later_refused, r#"{
    let v = never();
    let z = select v { [(n, 1), ..] => n + 1, _ => 3 };
    v <- [("s", 1), (1, 1)];
    z
}"#, refused("type mismatch i64 does not contain [i64, string]"); FuseExpect::None);

run!(slice_element_beside_a_literal_a_member_written_later_cannot_hold, r#"{
    let v = never();
    let z = select v { [(n, 1), ..] => n + 1, _ => 3 };
    v <- [(1, 1), ("s", `K)];
    z
}"#, |v: Result<&Value>| matches!(v, Ok(Value::I64(2))); FuseExpect::Jit);

run!(slice_element_binds_what_an_any_written_later_holds_refused, r#"{
    let v = never();
    let z = select v { [`J(n), ..] => n + 1, _ => 3 };
    let a: Array<Any> = [`J("s")];
    v <- a;
    z
}"#, refused("type mismatch i64 does not contain Any"); FuseExpect::None);

// An abstract pattern takes every instance of its type: only the slice's
// type test reads the parameters.
run!(slice_element_binds_an_abstract_instance_written_later_refused, r#"{
    type W<'a> = Abstract<'a>;
    let v = never();
    let z = select v { [W(p), ..] => p + 1, _ => 3 };
    v <- [W("s"), W(1)];
    z
}"#, refused("type mismatch i64 does not contain [i64, string]"); FuseExpect::None);

// CR claude for claude: [structure/low] This fixture and
// abstract_element_beside_a_rest_over_a_parameter_refused run the same program and pin
// two parts of one refusal. slice_element_binds_an_unnamed_open_member_annotated is
// also the same program as abstract_element_beside_a_rest_over_an_annotated_parameter.
// Each copy costs four more runs and pins nothing new. Delete this pair, and move this
// fixture's phrase into the other's predicate if both phrases matter. Found by reading;
// not run. (tests-lang-rest-a-05)
run!(slice_element_binds_an_unnamed_open_member_refused, r#"{
    type W<'a> = Abstract<'a>;
    let f = |x| select x { [W(p), ..] => p + 100, _ => 0 };
    f([W("s")])
}"#, refused("type mismatch i64 does not contain string"); FuseExpect::None);

run!(slice_element_binds_an_unnamed_open_member_annotated, r#"{
    type W<'a> = Abstract<'a>;
    let f = |x: Array<W<i64>>| select x { [W(p), ..] => p + 100, _ => 0 };
    f([W(1)])
}"#, |v: Result<&Value>| matches!(v, Ok(Value::I64(101))); FuseExpect::Jit);

run!(slice_element_binds_the_abstract_instance_it_typed, r#"{
    type W<'a> = Abstract<'a>;
    let v = never();
    let z = select v { [W(p), ..] => p + 1, _ => 3 };
    v <- [W(1), `K];
    z
}"#, |v: Result<&Value>| matches!(v, Ok(Value::I64(2))); FuseExpect::Jit);

run!(slice_element_binds_a_member_written_later_in_a_function_refused, r#"{
    let f = |u: i64| {
        let v = never();
        let z = select v { [`J(n), ..] => n + u, _ => 3 };
        v <- [`J("s"), `K];
        z
    };
    f(1)
}"#, refused("type mismatch i64 does not contain string"); FuseExpect::None);

// A bind the arm's body leaves undecided takes the later member's part.
run!(slice_element_bind_undecided_at_the_write_takes_its_part, r#"{
    let v = never();
    let z = select v { [`J(n), ..] => "[n]", _ => "" };
    v <- [`J("s"), `J(1), `K];
    z
}"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "s"); FuseExpect::Jit);

run!(slice_element_binds_a_declared_variable_written_later_refused, r#"{
    let f = |x: 'a| {
        let v = never();
        let z = select v { [`J(n), ..] => n + 1, _ => 3 };
        v <- [`J(x), `K];
        z
    };
    f("s")
}"#, refused("type mismatch i64 does not contain 'a"); FuseExpect::None);

// An element that only takes apart a type written later is typed by its
// body, and judged the same way.
run!(slice_element_takes_apart_an_element_written_later_refused, r#"{
    let e = never();
    let v = [e];
    let z = select v { [(a, b), ..] => a + 1, _ => 0 };
    e <- ("s", 1);
    z
}"#, refused("type mismatch i64 does not contain string"); FuseExpect::None);

// A member still open at the settle is any value of a bound of its that
// is a type: a declared variable's bound holds what a call may bring.
run!(slice_element_binds_a_bounded_variable_written_later_refused, r#"{
    let f = 'a: [`J(string), `K] |x: Array<'a>| {
        let v = never();
        let z = select v { [`J(n), ..] => n + 1, _ => 3 };
        v <- x;
        z
    };
    f([`J("s")])
}"#, refused("type mismatch i64 does not contain string"); FuseExpect::None);

run!(slice_element_binds_a_bounded_variable_beside_a_member_refused, r#"{
    let f = 'a: [(string, i64), `K] |x: 'a| {
        let v = never();
        let z = select v { [(n, m), ..] => n + 1, _ => 3 };
        v <- [x, (1, 1)];
        z
    };
    f(("s", 1))
}"#, refused("type mismatch i64 does not contain [i64, string]"); FuseExpect::None);

run!(slice_element_testing_a_tag_its_bounded_variable_lacks_is_dead, r#"{
    let f = 'a: [`L(string), `K] |x: Array<'a>| {
        let v = never();
        let z = select v { [`J(n), ..] => n + 1, _ => 3 };
        v <- x;
        z
    };
    f([`L("s")])
}"#, refused("pattern [`J(n), ..] will never match: nothing reaches it"); FuseExpect::Jit);

run!(slice_element_binds_the_bounded_variable_it_typed, r#"{
    let f = 'a: [`J(i64), `K] |x: Array<'a>| {
        let v = never();
        let z = select v { [`J(n), ..] => n + 1, _ => 3 };
        v <- x;
        z
    };
    f([`J(5)])
}"#, |v: Result<&Value>| matches!(v, Ok(Value::I64(6))); FuseExpect::Jit);

// A bind its arm leaves open would be the function's own variable, which
// a call decides apart from the value the element takes.
run!(slice_element_bind_left_open_beside_a_bounded_variable_refused, r#"{
    let f = 'a: [`J(string), `K] |x: Array<'a>| {
        let v = never();
        let z = select v { [`J(n), ..] => n, _ => never() };
        v <- x;
        z
    };
    let q: i64 = f([`J("s")]);
    q + 1
}"#, refused("type mismatch i64 does not contain string"); FuseExpect::None);

// An abstract type's test shares no other type's runtime form, so a
// member left open beside it is this judgment's alone: it may become
// any instance of the type.
run!(slice_element_binds_an_abstract_beside_an_open_member_refused, r#"{
    type W<'a> = Abstract<'a>;
    let f = |x| {
        let v = never();
        let z = select v { [W(p), ..] => p + 1, _ => 3 };
        v <- [x, W(1)];
        z
    };
    f(W("s"))
}"#, refused("type mismatch i64 does not contain [i64, string]"); FuseExpect::None);

run!(slice_element_binds_an_abstract_of_one_instance_beside_an_open_member, r#"{
    type W = Abstract<i64>;
    let f = |x| {
        let v = never();
        let z = select v { [W(p), ..] => p + 1, _ => 3 };
        v <- [x, W(1)];
        z
    };
    f(W(5))
}"#, |v: Result<&Value>| matches!(v, Ok(Value::I64(6))); FuseExpect::Jit);

run!(abstract_element_beside_a_rest_over_a_parameter_refused, r#"{
    type W<'a> = Abstract<'a>;
    let f = |x| select x { [W(p), ..] => p + 100, _ => 0 };
    f([W("s")])
}"#, refused("type mismatch i64 does not contain string"); FuseExpect::None);

run!(abstract_element_beside_a_rest_called_with_its_own_instance, r#"{
    type W<'a> = Abstract<'a>;
    let f = |x| select x { [W(p), ..] => p + 100, _ => 0 };
    f([W(1)])
}"#, prints("i64:101"); FuseExpect::Jit);

run!(abstract_tuple_element_beside_a_rest_over_a_parameter_refused, r#"{
    type W<'a> = Abstract<'a>;
    let f = |x| select x { [W((a, b)), ..] => a + 100, _ => 0 };
    f([W(("s", 2))])
}"#, refused("type mismatch i64 does not contain string"); FuseExpect::None);

run!(abstract_element_beside_a_named_rest_over_a_parameter_refused, r#"{
    type W<'a> = Abstract<'a>;
    let f = |x| select x { [W(p), r..] => p + 100, _ => 0 };
    f([W("s")])
}"#, refused("type mismatch i64 does not contain string"); FuseExpect::None);

run!(abstract_element_after_a_named_rest_over_a_parameter_refused, r#"{
    type W<'a> = Abstract<'a>;
    let f = |x| select x { [r.., W(p)] => p + 100, _ => 0 };
    f([W("s")])
}"#, refused("type mismatch i64 does not contain string"); FuseExpect::None);

run!(abstract_list_element_beside_a_rest_over_a_parameter_refused, r#"{
    type W<'a> = Abstract<'a>;
    let f = |x| select x { [<W(p), r..>] => p + 100, _ => 0 };
    f([<W("s")>])
}"#, refused("type mismatch i64 does not contain string"); FuseExpect::None);

run!(abstract_element_beside_a_rest_of_a_let_written_later_refused, r#"{
    type W<'a> = Abstract<'a>;
    let x = never();
    let r = select x { [W(p), ..] => p + 100, _ => 0 };
    x <- [W("s")];
    r
}"#, refused("type mismatch i64 does not contain string"); FuseExpect::None);

run!(abstract_element_beside_a_rest_over_an_annotated_parameter, r#"{
    type W<'a> = Abstract<'a>;
    let f = |x: Array<W<i64>>| select x { [W(p), ..] => p + 100, _ => 0 };
    f([W(1)])
}"#, |v: Result<&Value>| matches!(v, Ok(Value::I64(101))); FuseExpect::Jit);

run!(abstract_of_one_instance_beside_a_rest_over_a_parameter, r#"{
    type W = Abstract<i64>;
    let f = |x| select x { [r.., W(p)] => p + 100, _ => 0 };
    f([W(1), W(2)])
}"#, |v: Result<&Value>| matches!(v, Ok(Value::I64(102))); FuseExpect::Jit);

// With a `_` among its elements the slice tests none of them: the bind
// is `Any` where it is typed, whatever is written later.
run!(slice_element_beside_a_wildcard_over_a_let_the_arms_decide_refused, r#"{
    let v = never();
    let z = select v { [`J(n), _] => n + 1, _ => 3 };
    v <- [`J("s"), 0];
    z
}"#, refused("type mismatch i64 does not contain string"); FuseExpect::None);

// A `_` at a part of a tuple, struct or variant reads `Any` in a scrutinee
// the arms decide: it takes the values the other arms' tests of that part
// leave, of whatever type.
run!(wildcard_beside_a_shape_under_another_tag, r#"{
    let f = |s| select s { (`Key, `Char(c)) => c + 1, (`Mouse, _) => 0 };
    [f((`Key, `Char(7))), f((`Mouse, "x"))]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [8, 0]); FuseExpect::Jit);

run!(shape_under_a_tag_binds_any_payload_refused, r#"{
    let f = |s| select s { (`Key, `Char(c)) => c + 1, (`Key, _) => 0 };
    [f((`Key, `Char(7))), f((`Key, `Enter)), f((`Key, `Char("s")))]
}"#, refused("i64 does not contain string"); FuseExpect::Jit);

run!(wildcard_beside_a_shape_in_a_struct, r#"{
    let f = |s| select s { {k: `Key, v: `Char(c)} => c + 1, {k: `Mouse, v: _} => 0 };
    [f({k: `Key, v: `Char(7)}), f({k: `Mouse, v: "x"})]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [8, 0]); FuseExpect::Jit);

run!(wildcard_beside_a_shape_in_a_variants_payload, r#"{
    let f = |s| select s { `S(`C(1)) => 1, `S((2, _)) => 2, `S(_) => 0 };
    [f(`S(`C(1))), f(`S((2, "x"))), f(`S(7)), f(`S(`C("s")))]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [1, 2, 0, 0]); FuseExpect::Jit);

run!(wildcard_beside_a_literal_of_another_type, r#"{
    let f = |s| select s { (1, _) => 0, (_, "a") => 1, (_, _) => 2 };
    [f((2, 5)), f((2, "a")), f((1, null))]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [2, 1, 0]); FuseExpect::Jit);

run!(wildcard_arm_behind_a_null_test_is_live, r#"{
    let f = |s| select s { (1, _) => 0, (_, null) => 1, (_, _) => 2 };
    [f((1, null)), f((2, null)), f((2, 5))]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [0, 1, 2]); FuseExpect::Jit);

run!(wildcard_part_of_one_instance_called_at_two_types, r#"{
    let mk = |z| |s| select s { (1, _) => 0, (_, _) => 1 };
    let h = mk(0);
    [h((1, "a")), h((2, 3))]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [0, 1]); FuseExpect::None);

run!(wildcard_part_of_a_let_written_at_two_types, r#"{
    let s = never();
    let v = select s { (1, _) => 0, (_, _) => 1 };
    s <- (1, "a");
    s <- (2, 3);
    v
}"#, |v: Result<&Value>| matches!(v, Ok(Value::I64(0))); FuseExpect::Jit);

run!(wildcard_part_of_a_parsed_value, r#"{
    let p = str::parse("(2, \"s\")")$;
    select p { (1, _) => 0, (_, _) => 1 }
}"#, |v: Result<&Value>| matches!(v, Ok(Value::I64(1))); FuseExpect::None);

// An arm that binds a part whole beside arms that take that part apart
// types it as the union of their shapes, and its member holds each of
// theirs: an argument goes to it where that decides every cell the
// shape's own member would.
run!(bind_beside_arms_that_take_the_part_apart, r#"{
    let f = |s| select s { (1, b) => 10, (2, `A(x)) => x, (3, `B(y)) => str::len(y), (_, b) => 99 };
    [f((1, `A(5))), f((1, `B("x"))), f((3, `B("x"))), f((2, `A(7))), f((9, `B("q"))), f((9, `A(1)))]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [10, 10, 1, 7, 99, 99]); FuseExpect::Jit);

run!(bind_beside_arms_that_take_the_part_apart_as_tuples, r#"{
    let f = |s| select s { (1, b) => 10, (2, (x, y)) => x, (3, (x, y, z)) => str::len(z), (_, b) => 99 };
    [f((1, (5, 6))), f((1, (5, 6, "z"))), f((2, (5, 6))), f((3, (6, 7, "zz"))), f((3, (6, 7)))]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [10, 10, 5, 2, 99]); FuseExpect::Jit);

run!(bind_beside_arms_that_take_the_part_apart_as_a_slice_and_a_struct, r#"{
    let f = |s| select s { (1, b) => 10, (2, [x, ..]) => x, (3, {a, c}) => a, (_, b) => 99 };
    [f((1, [5])), f((1, {a: 4, c: "s"})), f((2, [6, 7])), f((3, {a: 4, c: "s"})), f((3, [6]))]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [10, 10, 6, 4, 99]); FuseExpect::Jit);

run!(bind_beside_arms_that_take_the_part_apart_in_a_struct, r#"{
    let f = |s| select s {
        {k: 1, v} => 10,
        {k: 2, v: `A(x)} => x,
        {k: 3, v: `B(y)} => str::len(y),
        {k: _, v} => 99
    };
    [f({k: 1, v: `A(5)}), f({k: 1, v: `B("x")}), f({k: 3, v: `B("x")}), f({k: 2, v: `A(9)}), f({k: 2, v: `B("x")})]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [10, 10, 1, 9, 99]); FuseExpect::Jit);

run!(bind_beside_arms_that_take_the_part_apart_in_a_variants_payload, r#"{
    let f = |s| select s {
        `S(1, b) => 10,
        `S(2, `A(x)) => x,
        `S(3, `B(y)) => str::len(y),
        `S(_, b) => 99
    };
    [f(`S(1, `A(5))), f(`S(1, `B("x"))), f(`S(3, `B("x"))), f(`S(2, `A(9))), f(`S(2, `B("x")))]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [10, 10, 1, 9, 99]); FuseExpect::Jit);

run!(bind_beside_arms_that_take_the_part_apart_whose_value_is_the_bind, r#"{
    let f = |s| select s { (1, b) => b, (2, `A(x)) => `A(x + 100), (3, `B(y)) => `B(y), (_, b) => b };
    let n = |v| select v { `A(n) => n + 1, `B(s) => str::len(s) };
    [n(f((1, `A(5)))), n(f((2, `A(5)))), n(f((3, `B("xy")))), n(f((7, `B("xyz"))))]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [6, 106, 2, 3]); FuseExpect::Jit);

run!(bind_beside_arms_that_take_the_part_apart_in_one_instance, r#"{
    let mk = |z| |s| select s { (1, b) => 10, (2, `A(x)) => x, (3, `B(y)) => str::len(y), (_, b) => 99 };
    let h = mk(0);
    [h((1, `A(5))), h((2, `A(6))), h((3, `B("x"))), h((1, `B("x"))), h((4, `A(1)))]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [10, 6, 1, 10, 99]); FuseExpect::None);

run!(bind_beside_arms_that_take_the_part_apart_in_one_instance_at_two_types, r#"{
    let mk = |z| |s| select s { (1, b) => 10, (2, `A(x)) => x, (3, `B(y)) => str::len(y), (_, b) => 99 };
    let h = mk(0);
    (h((1, `A(5))), h((2, `A("s"))))
}"#, prints(r#"[i64:10, "s"]"#); FuseExpect::None);

run!(bind_beside_arms_over_a_let_written_later_leaves_an_unwritten_tag_dead, r#"{
    let v = never();
    let r = select v { (1, b) => 10, (2, `A(x)) => x, (3, `B(y)) => str::len(y), (_, b) => 99 };
    v <- (1, `A(5));
    r
}"#, refused("pattern (3, `B(y)) will never match: nothing reaches it"); FuseExpect::Jit);

run!(bind_beside_arms_that_take_the_part_apart_used_as_another_type_refused, r#"{
    let f = |s| select s { (1, b) => 10, (2, `A(x)) => x, (3, `B(y)) => str::len(y), (_, b) => 99 };
    f((3, `B(4)))
}"#, refused("type mismatch string does not contain i64"); FuseExpect::None);

// The member that holds the others takes an argument only where that
// leaves nothing for another to decide. A `_` holds any value, and a
// name another arm binds inside that part is fixed at its arm's end
// (`Any`, or what the arm decided), so the `_`'s member takes an argument
// whose shape is decided; one still undecided goes to neither.
run!(
    arm_holding_a_bind_inside_takes_any_payload_refused,
    r#"{
    let f = |x| select x { (1, _, z) => z, (2, (c, _), z) => c, (_, _, z) => z };
    let g = |x| select x { (1, _, z) => z, (2, (c, _), z) => c + 1, (_, _, z) => z };
    (f((2, ("s", 4), 0)), f((1, 7, 3)), f((5, "q", "zz")), g((2, (5, 4), 0)), g((2, ("s", 4), 7)))
}"#,
    refused("i64 does not contain string")
);

run!(wildcard_member_holding_a_bind_inside_is_not_decided_by_a_caller_refused, r#"{
    let f = |x| select x { (1, _, z) => z, (2, (c, _), z) => c, (_, _, z) => z };
    f((2, ("s", 4), 0)) + 1
}"#, refused("type mismatch i64 does not contain string"); FuseExpect::None);

run!(wildcard_member_holding_a_bind_inside_takes_a_passed_on_argument, r#"{
    let f = |x| select x { (1, _, z) => z, (2, (c, _), z) => c, (_, _, z) => z };
    let g = |u| f((2, u, 0));
    g(("s", 4))
}"#, prints(r#""s""#); FuseExpect::Jit);

run!(wildcard_member_holding_a_bind_inside_annotated, r#"{
    let f = |x: (i64, (string, i64), i64)| select x { (1, _, z) => "[z]", (2, (c, _), z) => c, (_, _, z) => "[z]" };
    f((2, ("s", 4), 0))
}"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "s"); FuseExpect::Jit);

// An argument that holds a cell at the shape goes to the bind's member
// too: the shape's own member holds it as it stands once that cell is
// its cell.
run!(bind_beside_arms_that_take_the_part_apart_takes_an_argument_holding_a_cell, r#"{
    let f = |s| select s { (1, b) => 10, (2, `A(x)) => x, (3, `B(y)) => str::len(y), (_, b) => 99 };
    let h = |a| f((2, `A(a)));
    let n = |v| select v { i64 as i => i, string as s => str::len(s) };
    [h(5), h(6), n(h("xyz")), f((3, `B("q")))]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [5, 6, 3, 1]); FuseExpect::Jit);

run!(bind_beside_arms_that_take_the_part_apart_takes_an_empty_array, r#"{
    let f = |s| select s { (1, b) => 10, (2, [x, ..]) => x, (3, {a, c}) => a, (_, b) => 99 };
    [f((2, [])), f((2, [4])), f((1, []))]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [99, 4, 10]); FuseExpect::Jit);

run!(bind_beside_arms_that_take_the_part_apart_takes_each_member_of_a_union, r#"{
    let f = |s| select s { (1, b) => 10, (2, `A(x)) => x, (3, `B(y)) => str::len(y), (_, b) => 99 };
    let v: [(i64, `A(i64)), (i64, `B(string))] = (3, `B("xy"));
    let w: [(i64, `A(i64)), (i64, `B(string))] = (2, `A(8));
    [f(v), f(w)]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [2, 8]); FuseExpect::Jit);

// Three members, each holding the next: the widest takes the argument.
run!(bind_beside_arms_that_take_the_part_apart_at_two_depths, r#"{
    let f = |s| select s {
        (1, b) => 10,
        (2, `A(c)) => 20,
        (3, `A((x, y))) => x,
        (4, `A(`K(z))) => str::len(z),
        (_, b) => 99
    };
    [f((3, `A((5, 6)))), f((4, `A(`K("xy")))), f((2, `A(`K("q")))), f((1, `A((1, 2)))), f((4, `A((1, 2))))]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [5, 2, 20, 10, 99]); FuseExpect::Jit);

// An argument whose shape nothing has decided could become either
// shape's: no member takes it, and the wrapper's parameter is what to
// annotate.
run!(bind_beside_arms_that_take_the_part_apart_takes_an_unannotated_argument, r#"{
    let f = |s| select s { (1, b) => 10, (2, (x, y)) => x, (3, (x, y, z)) => x, (_, b) => 99 };
    let g = |u| f((2, u));
    g((5, 6))
}"#, prints("i64:5"); FuseExpect::Jit);

run!(bind_beside_arms_that_take_the_part_apart_takes_an_annotated_argument, r#"{
    let f = |s| select s { (1, b) => 10, (2, (x, y)) => x, (3, (x, y, z)) => x, (_, b) => 99 };
    let g = |u: (i64, i64)| f((2, u));
    g((5, 6))
}"#, |v: Result<&Value>| matches!(v, Ok(Value::I64(5))); FuseExpect::Jit);

// A union built after an arm's pattern decided a cell (the select's own
// result, an arm's body, a guard) may drop the member that held the cell
// from the scrutinee's union: the arms are still judged over the union as
// it stood before them.
run!(test_beside_a_never_member_the_result_union_drops, r#"{
    let z = never();
    let k: [i64, null] = 1;
    let u = select k { i64 as n => {a: n}, _ => {a: z} };
    let r = select u { {a: 7} => u, v => u };
    r.a
}"#, prints("i64:1"); FuseExpect::Jit);

run!(test_beside_a_never_member_the_result_union_keeps, r#"{
    let z = never();
    let k: [i64, null] = 1;
    let u = select k { i64 as n => {a: n}, _ => {a: z} };
    let r = select u { {a: 7} => 1, v => u.a };
    r
}"#, prints("i64:1"); FuseExpect::Jit);

run!(test_over_an_annotated_member_the_result_union_drops, r#"{
    let z: i64 = never();
    let k: [i64, null] = 1;
    let u = select k { i64 as n => {a: n}, _ => {a: z} };
    let r = select u { {a: 7} => u, v => u };
    r.a
}"#, |v: Result<&Value>| matches!(v, Ok(Value::I64(1))); FuseExpect::Jit);

run!(tuple_test_beside_a_never_member_the_result_union_drops, r#"{
    let z = never();
    let k: [i64, null] = 1;
    let u = select k { i64 as n => (n, 0), _ => (z, 0) };
    let r = select u { (7, _) => u, v => u };
    r.0
}"#, prints("i64:1"); FuseExpect::Jit);

run!(test_over_an_unannotated_parameter_the_result_union_drops, r#"{
    let f = |z| {
        let k: [i64, null] = null;
        let u = select k { i64 as n => {a: n}, _ => {a: z} };
        select u { {a: 7} => u, v => u }
    };
    f(3).a
}"#, prints("i64:3"); FuseExpect::Jit);

run!(test_over_an_annotated_parameter_the_result_union_drops, r#"{
    let f = |z: i64| {
        let k: [i64, null] = null;
        let u = select k { i64 as n => {a: n}, _ => {a: z} };
        select u { {a: 7} => u, v => u }
    };
    f(3).a
}"#, |v: Result<&Value>| matches!(v, Ok(Value::I64(3))); FuseExpect::Jit);

run!(test_beside_a_never_member_an_arm_body_drops, r#"{
    let z = never();
    let k: [i64, null] = 1;
    let u = select k { i64 as n => {a: n}, _ => {a: z} };
    select u {
        {a: 7} => { let w = select k { null as _ => u, _ => u }; w.a },
        {a: 8} => 2,
        v => 0
    }
}"#, prints("i64:0"); FuseExpect::Jit);

run!(test_over_an_annotated_member_an_arm_body_drops, r#"{
    let z: i64 = never();
    let k: [i64, null] = 1;
    let u = select k { i64 as n => {a: n}, _ => {a: z} };
    select u {
        {a: 7} => { let w = select k { null as _ => u, _ => u }; w.a },
        {a: 8} => 2,
        v => 0
    }
}"#, |v: Result<&Value>| matches!(v, Ok(Value::I64(0))); FuseExpect::Jit);

run!(test_beside_a_never_member_a_guard_drops, r#"{
    let z = never();
    let k: [i64, null] = 1;
    let u = select k { i64 as n => {a: n}, _ => {a: z} };
    select u { {a: 7} if (select k { null as _ => u, _ => u }).a == 7 => 1, v => 0 }
}"#, prints("i64:0"); FuseExpect::Jit);

// An arm dead for the decision is refused for the decision, the member
// dropped or not.
run!(test_deciding_a_member_an_arm_body_drops_before_a_dead_arm_refused, r#"{
    let z = never();
    let k: [i64, null] = 1;
    let u = select k { i64 as n => {a: n}, _ => {a: z} };
    select u {
        {a: 7} => select k { null as _ => u, _ => u },
        {a: i64} as v => {a: 0},
        w => {a: 1}
    }
}"#, refused("pattern w will never match: nothing reaches it (5:12; the value is { a: i64 })"); FuseExpect::None);

// What an arm's body decides is no pattern's decision: the union it drops
// a member from is judged as before.
run!(body_deciding_a_member_a_later_union_drops, r#"{
    let z = never();
    let k: [i64, null] = 1;
    let u = select k { i64 as n => (n, 0), _ => (z, 0) };
    select u {
        (a, 1) => { z <- k ~ 1; let w = select k { null as _ => u, _ => u }; a + w.0 },
        (a, _) => a
    }
}"#, |v: Result<&Value>| matches!(v, Ok(Value::I64(1))); FuseExpect::Jit);

// The same under a written test, in an or-pattern, and where the union is
// built in a later arm's body or in a function an arm builds.
run!(written_test_beside_a_never_member_the_result_union_drops, r#"{
    let z = never();
    let k: [i64, null] = 1;
    let u = select k { i64 as n => {a: n}, _ => {a: z} };
    let r = select u { {a: i64} as {a: 7} => u, v => u };
    r.a
}"#, prints("i64:1"); FuseExpect::Jit);

run!(written_test_over_an_annotated_member_the_result_union_drops, r#"{
    let z: i64 = never();
    let k: [i64, null] = 1;
    let u = select k { i64 as n => {a: n}, _ => {a: z} };
    let r = select u { {a: i64} as {a: 7} => u, v => u };
    r.a
}"#, |v: Result<&Value>| matches!(v, Ok(Value::I64(1))); FuseExpect::Jit);

run!(or_test_over_a_member_the_result_union_drops, r#"{
    let z = never();
    let k: [i64, null] = 1;
    let u = select k { i64 as n => {a: n}, _ => {a: z} };
    let r = select u { {a: 7} | {a: 8} => u, v => u };
    r.a
}"#, prints("i64:1"); FuseExpect::Jit);

run!(test_beside_a_never_member_a_later_arm_body_drops, r#"{
    let z = never();
    let k: [i64, null] = 1;
    let u = select k { i64 as n => {a: n}, _ => {a: z} };
    select u {
        {a: 7} => 0,
        v => { let w = select k { null as _ => u, _ => u }; w.a }
    }
}"#, prints("i64:1"); FuseExpect::Jit);

run!(test_beside_a_never_member_a_function_in_its_arm_drops, r#"{
    let z = never();
    let k: [i64, null] = 1;
    let u = select k { i64 as n => {a: n}, _ => {a: z} };
    select u {
        {a: 7} => { let g = || select k { null as _ => u, _ => u }; g().a },
        v => 0
    }
}"#, prints("i64:0"); FuseExpect::Jit);

// A pattern that only takes a part apart decides its shape: the union a
// body then drops a member from is judged with that decision, as before.
run!(take_apart_deciding_a_part_a_body_union_drops, r#"{
    let f = |z, y| {
        let k: [i64, null] = 1;
        let u = select k { i64 as n => (z, 0), _ => (y, 0) };
        select u {
            ((p, q), _) => {
                let s: i64 = p + q;
                let w = select k { null as _ => u, _ => u };
                s + w.1
            }
        }
    };
    f((1, 2), (3, 4))
}"#, |v: Result<&Value>| matches!(v, Ok(Value::I64(3))); FuseExpect::Jit);

// A written test that two undecided members of the scrutinee could each
// take goes to neither: which took it would follow their order in the
// union.
run!(written_test_two_undecided_members_could_take, r#"{
    let x = never();
    let y = never();
    let u = select true { true => x, false => y };
    let r = select u { i64 as n => n, _ => 0 };
    x <- 1;
    y <- "s";
    r
}"#, prints("i64:1"); FuseExpect::Jit);

run!(written_test_two_undecided_members_written_the_other_way, r#"{
    let x = never();
    let y = never();
    let u = select true { true => x, false => y };
    let r = select u { i64 as n => n, _ => 0 };
    x <- "s";
    y <- 1;
    r
}"#, prints("i64:0"); FuseExpect::Jit);

run!(written_test_two_undecided_members_a_call_returns, r#"{
    let mk = |c, a, b| select c { true => a, false => b };
    let x = never();
    let y = never();
    let u = mk(true, x, y);
    let r = select u { i64 as n => n, _ => 0 };
    x <- 1;
    y <- "s";
    r
}"#, prints("i64:1"); FuseExpect::Jit);

run!(written_test_two_undecided_parameters_could_take, r#"{
    let g = |p, q| {
        let u = select true { true => p, false => q };
        select u { null as _ => 0, v => 1 }
    };
    g(null, 5)
}"#, prints("i64:0"); FuseExpect::Jit);

run!(written_test_three_undecided_members_could_take, r#"{
    let x = never();
    let y = never();
    let z = never();
    let u = select 1 { 1 => x, 2 => y, _ => z };
    let r = select u { i64 as n => n, string as s => 1, _ => 0 };
    x <- 1;
    y <- "s";
    z <- null;
    r
}"#, prints("i64:1"); FuseExpect::Jit);

run!(written_tuple_test_two_undecided_members_could_take, r#"{
    let x = never();
    let y = never();
    let u = select true { true => x, false => y };
    let r = select u { (i64, string) as (a, b) => a, _ => 0 };
    x <- (1, "t");
    y <- "s";
    r
}"#, prints("i64:1"); FuseExpect::Jit);

run!(guarded_written_test_over_two_members_written_later, r#"{
    let x = never();
    let y = never();
    let u = select true { true => x, false => y };
    let r = select u { i64 as n if n > 0 => n, _ => 0 };
    x <- 1;
    y <- "s";
    r
}"#, prints("i64:1"); FuseExpect::Jit);

// One annotation leaves one member to take the test, or a decided member
// that holds it.
run!(written_test_beside_one_annotated_member_decides_the_other, r#"{
    let x: string = never();
    let y = never();
    let u = select false { true => x, false => y };
    let r = select u { i64 as n => n, _ => 0 };
    x <- "s";
    y <- 7;
    r
}"#, |v: Result<&Value>| matches!(v, Ok(Value::I64(7))); FuseExpect::Jit);

run!(written_test_a_decided_member_holds_leaves_the_other_open, r#"{
    let x = never();
    let y: i64 = never();
    let u = select true { true => x, false => y };
    let r = select u { i64 as n => n, _ => 0 };
    x <- "s";
    y <- 7;
    r
}"#, |v: Result<&Value>| matches!(v, Ok(Value::I64(0))); FuseExpect::Jit);

run!(written_test_two_annotated_members, r#"{
    let x: [i64, string] = never();
    let y: [i64, string] = never();
    let u = select false { true => x, false => y };
    let r = select u { i64 as n => n, _ => 0 };
    x <- "s";
    y <- 7;
    r
}"#, |v: Result<&Value>| matches!(v, Ok(Value::I64(7))); FuseExpect::Jit);

// A decided member that holds the test takes it, however many undecided
// members stand beside it.
run!(written_test_a_decided_member_holds_beside_two_undecided, r#"{
    let x = never();
    let y = never();
    let u = select 3 { 1 => x, 2 => y, _ => 5 };
    let r = select u { i64 as n => n, _ => 0 };
    x <- "s";
    y <- null;
    r
}"#, |v: Result<&Value>| matches!(v, Ok(Value::I64(5))); FuseExpect::Jit);

// Arms that cover the scrutinee between them decide every undecided
// member together, as the union of what they test.
run!(written_tests_covering_two_undecided_members_decide_both, r#"{
    let x = never();
    let y = never();
    let u = select false { true => x, false => y };
    let r = select u { i64 as n => n + 1, string as s => 0 };
    x <- "s";
    y <- 1;
    r
}"#, |v: Result<&Value>| matches!(v, Ok(Value::I64(2))); FuseExpect::Jit);

// A member something already decided is no taker.
run!(written_test_one_undecided_member_beside_one_a_use_decided, r#"{
    let x = never();
    let y = never();
    let k = x + 1;
    let u = select false { true => x, false => y };
    let r = select u { string as s => 1, _ => 0 };
    x <- 1;
    y <- "s";
    r
}"#, |v: Result<&Value>| matches!(v, Ok(Value::I64(1))); FuseExpect::Jit);

// With no catch-all a case the arms leave out is the refusal: with it
// written, the arms decide both members and nothing is picked.
run!(written_tests_short_of_a_decided_member_name_the_missing_case, r#"{
    let g = |x, y, k: i64| {
        let u = select k { 0 => y, 1 => x, _ => null };
        select u { i64 as n => n, string as s => str::len(s) }
    };
    g(1, "ab", 0)
}"#, refused("type mismatch [i64, string] does not contain null"); FuseExpect::None);

run!(written_tests_covering_a_decided_member_and_two_undecided, r#"{
    let g = |x, y, k: i64| {
        let u = select k { 0 => y, 1 => x, _ => null };
        select u { i64 as n => n, string as s => str::len(s), null as _ => -1 }
    };
    [g(1, "ab", 0), g("ab", 1, 0), g(1, 1, 2)]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [2, 1, -1]); FuseExpect::Jit);

// Two undecided members no binding of their own names go by the binding
// that holds their union, named once: it is the one to annotate.
run!(written_test_over_two_empty_array_reads_settles, r#"{
    let a = [];
    let b = [];
    let u = select true { true => a[0]$, false => b[0]$ };
    select u { i64 as n => n, _ => 0 }
}"#, settles(); FuseExpect::Jit);

run!(written_test_two_parsed_members_name_their_holder_once, r#"{
    let u = select true { true => str::parse("1")$, false => str::parse("s")$ };
    select u { i64 as n => n, _ => 0 }
}"#, refused("the type 'b must be known here, an operation reads it to know what to make: annotate where its result goes"); FuseExpect::None);

run!(written_test_two_parsed_members_their_holder_annotated, r#"{
    let u: [i64, string] = select true { true => str::parse("1")$, false => str::parse("s")$ };
    select u { i64 as n => n, _ => 0 }
}"#, |v: Result<&Value>| matches!(v, Ok(Value::I64(1))); FuseExpect::Jit);

// The members reach the select through a callback's element, a field and
// an earlier arm that takes a decided member.
run!(written_test_two_undecided_members_a_callback_element, r#"{
    let f = |a, b| array::map([a, b], |e| select e { i64 as n => n, _ => 0 });
    f(1, "s")
}"#, prints("[i64:1, i64:0]"); FuseExpect::Jit);

run!(written_test_a_callback_element_annotated, r#"{
    let f = |a, b| array::map([a, b], |e: [i64, string]| select e { i64 as n => n, _ => 0 });
    [f("s", 1), f(1, "s")]
}"#, |v: Result<&Value>| match v {
    Ok(Value::Array(t)) => match &t[..] {
        [Value::Array(p), Value::Array(q)] => as_i64s(p) == [0, 1] && as_i64s(q) == [1, 0],
        _ => false,
    },
    _ => false,
}; FuseExpect::Jit);

run!(written_test_two_undecided_members_a_field_holds, r#"{
    let x = never();
    let y = never();
    let t = {f: select true { true => x, false => y }};
    let r = select t.f { i64 as n => n, _ => 0 };
    x <- 1;
    y <- "s";
    r
}"#, prints("i64:1"); FuseExpect::Jit);

run!(written_error_test_two_undecided_members_could_take, r#"{
    let x = never();
    let y = never();
    let u = select true { true => x, false => y };
    let r = select u { error as e => 1, v => 0 };
    x <- error(`E);
    y <- "s";
    r
}"#, prints("i64:1"); FuseExpect::Jit);

run!(written_test_behind_an_arm_a_decided_member_takes, r#"{
    let f = |a, b| {
        let u = select 0 { 0 => a, 1 => b, _ => null };
        select u { null as _ => -1, i64 as n => n, _ => 0 }
    };
    f(1, "s")
}"#, prints("i64:1"); FuseExpect::Jit);

run!(written_test_behind_an_arm_one_member_annotated, r#"{
    let f = |a: i64, b| {
        let u = select 0 { 0 => a, 1 => b, _ => null };
        select u { null as _ => -1, i64 as n => n, _ => 0 }
    };
    [f(1, "s"), f(1, null), f(1, 4)]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [1, 1, 1]); FuseExpect::Jit);

// A bind behind arms that test its part, over a scrutinee the arms decide,
// holds every member's part that reaches its arm: the earlier arms' tests
// decide nothing of it.
run!(bind_behind_literals_of_two_types_is_typed_by_the_call, r#"{
    let f = |s| select s { (1, 1) => 0, (3, "x") => 2, (4, v) => v + 1, (_, _) => 9 };
    f((4, 7))
}"#, prints("i64:8"); FuseExpect::Jit);

// The call types the bind; arithmetic still needs one numeric type.
run!(bind_behind_literals_of_two_types_called_with_a_string_refused, r#"{
    let f = |s| select s { (1, 1) => 0, (3, "x") => 2, (4, v) => v + 1, (_, _) => 9 };
    f((4, "s"))
}"#, refused("i64 does not contain string"); FuseExpect::None);

run!(bind_behind_literals_of_two_types_in_a_struct_is_typed_by_the_call, r#"{
    let f = |s| select s { {k: 1, v: 1} => 0, {k: 3, v: "x"} => 2, {k: 4, v} => v + 1, {k: _, v: _} => 9 };
    f({k: 4, v: 7})
}"#, prints("i64:8"); FuseExpect::Jit);

run!(bind_behind_literals_of_two_types_in_a_struct_called_with_a_string_refused, r#"{
    let f = |s| select s { {k: 1, v: 1} => 0, {k: 3, v: "x"} => 2, {k: 4, v} => v + 1, {k: _, v: _} => 9 };
    f({k: 4, v: "s"})
}"#, refused("i64 does not contain string"); FuseExpect::None);

run!(bind_behind_a_literal_and_a_null_is_typed_by_the_call, r#"{
    let f = |s| select s { (1, 1) => 0, (3, null) => 2, (4, v) => v + 1, (_, _) => 9 };
    f((4, 5))
}"#, prints("i64:6"); FuseExpect::Jit);

run!(bind_behind_a_literal_and_a_null_called_with_a_null_refused, r#"{
    let f = |s| select s { (1, 1) => 0, (3, null) => 2, (4, v) => v + 1, (_, _) => 9 };
    f((4, null))
}"#, refused("i64 does not contain null"); FuseExpect::None);

run!(bind_behind_literals_of_two_types_holds_both, r#"{
    let f = |s| select s { (1, 1) => "k0", (3, "x") => "k2", (4, v) => "[v]", (_, _) => "w" };
    [f((4, "y")), f((4, 7)), f((1, 1)), f((3, "x")), f((5, 5))]
}"#, prints(r#"["y", "7", "k0", "k2", "w"]"#); FuseExpect::Jit);

run!(bind_behind_literals_of_two_types_tested_in_its_arm, r#"{
    let f = |s| select s {
        (1, 1) => 0,
        (3, "x") => 2,
        (4, v) => select v { i64 as n => n + 1, s => str::len(s) },
        (_, _) => 9
    };
    [f((4, "yy")), f((4, 7)), f((5, 5))]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [2, 8, 9]); FuseExpect::Jit);

run!(bind_behind_arms_that_take_its_part_apart, r#"{
    let f = |s| select s { (1, `B(v1)) => 100, (2, (v1, v2)) => v1 + 200, (3, v1) => 300, (_, v1) => 9 };
    [f((3, (1, 7))), f((3, `B(1))), f((2, (1, 7))), f((1, `B(1))), f((5, `B(1)))]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [300, 300, 201, 100, 9]); FuseExpect::Jit);

run!(bind_behind_a_slice_and_a_literal, r#"{
    let f = |s| select s { (1, [v1]) => v1 + 100, (2, 2) => 200, (3, v1) => 300, (_, v1) => 9 };
    [f((3, 3)), f((3, [1])), f((1, [4])), f((2, 2)), f((1, 1))]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [300, 300, 104, 200, 9]); FuseExpect::Jit);

run!(bind_behind_payload_tags_in_a_variant, r#"{
    let f = |s| select s { `S(2, `B(v0)) => str::len(v0), `S(4, `A(v1)) => 400, `S(v1, v2) => v1 + 500 };
    [f(`S(2, `A(1))), f(`S(2, `B("xy"))), f(`S(4, `A(1))), f(`S(4, `B("x")))]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [502, 2, 400, 504]); FuseExpect::Jit);

run!(bind_behind_payload_tags_in_a_variant_beside_a_catch_all, r#"{
    let f = |s| select s { `S(2, `B(v0)) => 200, `S(4, `A(v1)) => 400, `S(v1, v2) => v1 + 500, _ => 9 };
    f(`S(2, `A(1)))
}"#, prints("i64:502"); FuseExpect::Jit);

run!(bind_behind_payload_tags_in_an_annotated_variant_leaves_a_catch_all_dead, r#"{
    let f = |s: `S(i64, [`A(i64), `B(i64)])| select s { `S(2, `B(v0)) => 200, `S(4, `A(v1)) => 400, `S(v1, v2) => v1 + 500, _ => 9 };
    f(`S(2, `A(1)))
}"#, refused("pattern _ will never match: nothing reaches it"); FuseExpect::None);

// A bind at a part of a variant's payload beside another arm's test of that
// part holds the test's values wherever they reach its arm, and does not where
// an earlier unguarded arm takes them whole.
run!(payload_bind_beside_a_later_literal_refused, r#"{
    let f = |s| select s { `A(1, a) => a + 1, `A(2, "x") => 0, _ => 0 };
    f(`A(1, "x"))
}"#, refused("type mismatch i64 does not contain string"); FuseExpect::None);

run!(payload_bind_beside_an_earlier_literal_refused, r#"{
    let f = |s| select s { `A(2, "x") => 0, `A(1, a) => a + 1, _ => 0 };
    f(`A(1, "x"))
}"#, refused("type mismatch i64 does not contain string"); FuseExpect::None);

run!(payload_bind_beside_a_null_refused, r#"{
    let f = |s| select s { `A(1, a) => a, `A(2, null) => 0, _ => 1 };
    f(`A(1, null)) + 1
}"#, refused("type mismatch i64 does not contain null"); FuseExpect::None);

run!(payload_bind_beside_a_literal_in_a_nested_payload_refused, r#"{
    let f = |s| select s { `B(`A(1, a)) => a + 1, `B(`A(2, "x")) => 0, _ => 0 };
    f(`B(`A(1, "x")))
}"#, refused("type mismatch i64 does not contain string"); FuseExpect::None);

run!(payload_bind_beside_a_guarded_null_refused, r#"{
    let f = |s, b: bool| select s { `A(null) if b => 0, `A(a) => a + 1 };
    f(`A(null), false)
}"#, refused("type mismatch i64 does not contain null"); FuseExpect::None);

run!(payload_bind_beside_a_literal_holds_it, r#"{
    let f = |s| select s { `A(1, a) => "[a]", `A(2, "x") => "k", _ => "w" };
    [f(`A(1, "x")), f(`A(2, "x")), f(`A(3, "y"))]
}"#, prints(r#"["x", "k", "w"]"#); FuseExpect::Jit);

run!(payload_bind_beside_a_bool_holds_it, r#"{
    let f = |s| select s { `A(1, a) => a, `A(2, true) => 0, _ => 1 };
    [f(`A(1, false)), f(`A(2, true)), f(`A(3, true))]
}"#, prints("[false, i64:0, i64:1]"); FuseExpect::Jit);

run!(payload_bind_behind_arms_that_take_its_members, r#"{
    let f = |s| select s { `A(null) => 0, `A(a) => a + 1 };
    let g = |s| select s { `A(`X) => 0, `A(`Y) => 1, `A(a) => a };
    let h = |s| select s { `Ok(null) => "none", `Ok(v) => v, `Err(e) => e };
    let k = |s| select s { `B(`A(null)) => 0, `B(`A(a)) => a + 1 };
    ([f(`A(null)), f(`A(3)), k(`B(`A(null))), k(`B(`A(5)))], [g(`A(`X)), g(`A(`Z))], [h(`Ok(null)), h(`Ok("x")), h(`Err("e"))])
}"#, prints(r#"[[i64:0, i64:4, i64:0, i64:6], [i64:0, "Z"], ["none", "x", "e"]]"#); FuseExpect::Jit);

// A bind before an arm testing null at its part takes the null: its arm
// matches every payload, so the null arm after it is dead.
run!(payload_bind_before_a_null_arm_refused, r#"{
    let f = |s| select s { `A(a) => a + 1, `A(null) => 0 };
    f(`A(null))
}"#, refused("type mismatch i64 does not contain null"); FuseExpect::None);

run!(payload_bind_before_a_null_arm_leaves_it_dead, r#"{
    let f = |s| select s { `A(a) => "[a]", `A(null) => "n" };
    f(`A(null))
}"#, refused("nothing reaches it"); FuseExpect::None);

run!(payload_bind_before_a_null_arm_over_an_annotated_parameter_leaves_it_dead, r#"{
    let f = |s: `A([i64, null])| select s { `A(a) => "[a]", `A(null) => "n" };
    f(`A(null))
}"#, refused("pattern `A(null) will never match: nothing reaches it"); FuseExpect::None);

run!(payload_bind_behind_covers_beside_other_parts_and_tags, r#"{
    let f = |s| select s { `B(null) => 0, `A(null) => 1, `A(a) => a + 1, `B(b) => b };
    let g = |s| select s { `A(null) | `B(null) => 0, `B(b) => b, `A(a) => a + 1 };
    let h = |s| select s { `A(1, x) => 7, `A(n, null) => n, `A(m, b) => m + b };
    let k = |s| select s { `A(_, null) => 0, `A(m, b) => b + 1 };
    ([f(`A(null)), f(`B(2)), f(`A(3))], [g(`A(null)), g(`B(null)), g(`A(1)), g(`B(4))], [h(`A(1, null)), h(`A(2, null)), h(`A(2, 3))], [k(`A("q", null)), k(`A("q", 2))])
}"#, prints("[[i64:1, i64:2, i64:4], [i64:0, i64:0, i64:2, i64:4], [i64:7, i64:2, i64:5], [i64:0, i64:3]]"); FuseExpect::Jit);

// A `_` at a part of a tuple, struct or variant stands beside another arm's
// name at that part as an unused name does: the part is what the name's uses
// make it, over a scrutinee the arms decide.
run!(wildcard_beside_a_bind_over_an_unannotated_parameter, r#"{
    let f = |s| select s { (1, _) => 0, (_, b) => b + 1 };
    [f((1, 5)), f((2, 5))]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [0, 6]); FuseExpect::Jit);

run!(wildcard_beside_a_bind_behind_a_slice_ladder, r#"{
    let f = |s| select s { ([], _) => 0, ([x, ..], b) => str::len(b) };
    [f(([], "q")), f(([1], "abc"))]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [0, 3]); FuseExpect::Jit);

run!(wildcard_beside_a_bind_returned_keeps_the_binds_type, r#"{
    let f = |s| select s { ([], _) => 0, ([x, ..], b) => b };
    let r = f(([1], "abc"));
    select r { i64 as i => i, string as z => str::len(z) }
}"#, |v: Result<&Value>| matches!(v, Ok(Value::I64(3))); FuseExpect::Jit);

run!(wildcards_beside_binds_at_two_parts, r#"{
    let f = |s| select s { ([x], b, c) => x + b, ([], _, _) => 1, ([x, y, t..], _, _) => 2 };
    [f(([], 5, "r")), f(([3], 5, "r")), f(([1, 2], 5, "r"))]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [1, 8, 2]); FuseExpect::Jit);

run!(wildcard_beside_a_bind_in_a_variant_and_a_struct, r#"{
    let f = |s| select s { `A(1, _) => 0, `A(_, b) => b + 1 };
    let g = |s| select s { {a: 1, b: _} => 0, {a: _, b} => b + 1 };
    [f(`A(2, 5)), g({a: 2, b: 7})]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [6, 8]); FuseExpect::Jit);

run!(wildcard_inside_a_part_a_bind_sits_beside, r#"{
    let f = |s| select s { (1, v) => 0, (2, (_, "a")) => 1, (_, (_, "a")) => 2, (4, v) => v.0 + 1, _ => 9 };
    let g = |s| select s { `S(1, (1, "b"), _) => 0, `S(3, v, _) => v.0 + 1, _ => 9 };
    let h = |s| select s { {a: 1, b} => 0, {a: 2, b: {c: _, d: "a"}} => 1, {a: _, b: {c: _, d: "a"}} => 2, {a: 4, b} => b.c + 1, _ => 9 };
    [f((4, (1, "a"))), f((5, (1, "a"))), g(`S(3, (2, "b"), (1, "a"))), h({a: 4, b: {c: 6, d: "x"}})]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [2, 2, 3, 7]); FuseExpect::Jit);

run!(wildcard_beside_a_bind_over_a_let_written_later, r#"{
    let s = never();
    let v = select s { (1, _) => 0, (_, b) => b + 1 };
    s <- (2, 5);
    v
}"#, |v: Result<&Value>| matches!(v, Ok(Value::I64(6))); FuseExpect::Jit);

// CR claude for claude: [test-gap/medium] This refusal does not depend on its `(1, _)`
// arm. With that arm deleted the program is refused in the same words, because `b` is
// bound at one part and used as i64 in one arm and as string in another, which is a
// rule of its own. The `_` rule this test is named for (select.md: a `_` beside a name
// at that part stands as the unused name, so a call whose part is wider than the name's
// type, `f((2, "a"))`, is refused) has no refusal pin. select.md's `no "type mismatch"`
// case for this program has the same flaw. A change that let the `_` widen that part to
// `Any` would pass both. Pin `let f = |s| select s { (1, _) => 0, (_, b) => b + 1 };
// f((2, "a"))` refused with "(i64, i64) does not contain (i64, string)", here and as
// the select.md case. probe:
// design/review-2026-11-16/repro/tests-lang-rest-a-wildcard-pin-vacuous.sh
// (tests-lang-rest-a-02)
run!(wildcard_beside_a_bind_used_as_two_types_refused, r#"{
    let f = |s| select s { (1, _) => 0, (2, b) => b + 1, (_, b) => str::len(b) };
    f((2, 5))
}"#, refused("type mismatch"); FuseExpect::None);

// A part an earlier arm takes whole, binding only another cell, holds the
// member as it stands, so a later test decides none of its cells, in
// whichever order the parts stand.
run!(cover_bind_part_after_a_taken_member_leaves_the_cells_open, r#"{
    let f = |x, y, b: bool| {
        let r = &mut (select b { true => (`K, x, y), false => (`L, 0, "a") });
        select *r { (`K, n, _) => "0:[n]", (_, _, "a") => "1", _ => "w" }
    };
    [f(5, 7, true), f(5, 7, false)]
}"#, prints(r#"["0:5", "1"]"#); FuseExpect::Jit);

run!(cover_bind_part_after_a_taken_member_annotated, r#"{
    let f = |x, y: i64, b: bool| {
        let r = &mut (select b { true => (`K, x, y), false => (`L, 0, "a") });
        select *r { (`K, n, _) => "0:[n]", (_, _, "a") => "1", _ => "w" }
    };
    [f(5, 7, true), f(5, 7, false)]
}"#, prints(r#"["0:5", "1"]"#); FuseExpect::Jit);

run!(cover_bind_part_after_a_taken_member_part_first, r#"{
    let f = |x, y, b: bool| {
        let r = &mut (select b { true => (y, `K, x), false => ("a", `L, 0) });
        select *r { (_, `K, n) => "0:[n]", ("a", _, _) => "1", _ => "w" }
    };
    [f(5, 7, true), f(5, 7, false)]
}"#, prints(r#"["0:5", "1"]"#); FuseExpect::Jit);

run!(standing_tests_name_their_parts, r#"{
    let f = |x, t: [`A, `B], c: bool| {
        let r = &mut (select c { true => (t, x), false => (`L, 0) });
        select *r { (`A, n) => 1, (`B, m) => 2, (_, 0) => 3, _ => 4 }
    };
    [f(0, `A, true), f(0, `B, true), f(0, `A, false)]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [1, 2, 3]); FuseExpect::Jit);

run!(standing_tests_name_one_part_and_ignore_another, r#"{
    let f = |x, t: [`A, `B], c: bool| {
        let r = &mut (select c { true => (t, x), false => (`L, 0) });
        select *r { (`A, n) => 1, (`B, _) => 2, (_, 0) => 3, _ => 4 }
    };
    [f(0, `A, true), f(0, `B, true), f(0, `A, false)]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [1, 2, 3]); FuseExpect::Jit);

run!(standing_tests_that_leave_a_member_uncovered, r#"{
    let f = |x, t: [`A, `B], c: bool| {
        let r = &mut (select c { true => (t, x), false => (`L, 0) });
        select *r { (`A, n) => 1, (_, 0) => 3, _ => 4 }
    };
    [f(0, `A, true), f(0, `B, true)]
}"#, prints("[i64:1, i64:3]"); FuseExpect::Jit);

run!(standing_tests_that_leave_a_member_uncovered_annotated, r#"{
    let f = |x: i64, t: [`A, `B], c: bool| {
        let r = &mut (select c { true => (t, x), false => (`L, 0) });
        select *r { (`A, n) => 1, (_, 0) => 3, _ => 4 }
    };
    [f(0, `A, true), f(0, `B, true)]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [1, 3]); FuseExpect::Jit);

// A value a generic definition's select waits on is routed by each call,
// one whose type is a cell of the definition made of its variables too
// (the element of `[[x]]`).
run!(waiting_value_inside_a_definitions_cell_is_routed_by_each_call, r#"{
    let f = |x, c: bool| {
        let b = select c { true => [x], false => [[x]] };
        select b { [h, ..] => h, _ => never() }
    };
    f(1, true) + 1
}"#, refused("i64 does not contain Array<i64>"); FuseExpect::None);

run!(waiting_value_inside_a_definitions_cell_written_later, r#"{
    let f = |x| {
        let b = [x];
        b <- [[x]];
        select b { [h, ..] => h, _ => never() }
    };
    f(1) + 1
}"#, refused("i64 does not contain Array<i64>"); FuseExpect::None);

run!(waiting_value_inside_a_definitions_tuple_cell, r#"{
    let f = |x, c: bool| {
        let b = select c { true => (x, 1), false => ((x, x), 1) };
        select b { (0, _) => 0, (h, _) => h }
    };
    f(1, true) + 1
}"#, refused("i64 does not contain (i64, i64)"); FuseExpect::None);

run!(waiting_value_inside_a_definitions_cell_routed, r#"{
    let f = |x, c: bool| {
        let b = select c { true => [x], false => [[x]] };
        select b { [h, ..] => h, _ => never() }
    };
    let r = f(1, false);
    select r { i64 as n => "n [n]", Array<i64> as a => "a [a]" }
}"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if s == "a [1]"); FuseExpect::Jit);

// An arm an earlier arm takes whole by its structure is dead over a value
// nothing has decided too, a literal behind a wildcard included.
run!(
    tag_behind_a_wildcard_over_an_undecided_parameter_is_dead,
    "{ let f = |x| select x { _ => 3, `J => 1 }; f(`J) }",
    refused("pattern `J will never match: nothing reaches it");
    FuseExpect::None
);

run!(
    payload_behind_a_whole_tag_over_an_undecided_parameter_is_dead,
    "{ let f = |x| select x { `A(a) => 1, `A(null) => 2, _ => 3 }; f(`A(null)) }",
    refused("pattern `A(null) will never match: nothing reaches it");
    FuseExpect::None
);

run!(
    literal_behind_a_wildcard_over_an_undecided_parameter_is_dead,
    "{ let f = |x| select x { _ => 3, 5 => 1 }; f(5) }",
    refused("pattern 5 will never match: nothing reaches it");
    FuseExpect::None
);
