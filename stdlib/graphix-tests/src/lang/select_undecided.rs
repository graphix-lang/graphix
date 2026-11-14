// A type not decided at a select is one unknown type there

use anyhow::Result;
use graphix_package_core::{
    run,
    testing::{FuseExpect, as_i64s, refused},
};
use netidx::publisher::Value;

const NOT_DECIDED: &str = "a type not decided at this select";
const TOLD_APART: &str = "told apart by types not decided at this select";

run!(undecided_member_tested_refused, r#"{
    let b = true;
    let z = never();
    let v = select b { true => {a: [1], c: 1}, false => z };
    let r = select v { {a, ..} => "[a]" };
    z <- {a: ["s"], c: "w"};
    r
}"#, refused("{ a, .. } tests a value of 'z, a type not decided at this select"); FuseExpect::None);

run!(undecided_member_tested_whole_refused, r#"{
    let b = true;
    let z = never();
    let v = select b { true => {a: [1], c: 1}, false => z };
    let r = select v { {a, c} => "[a]" };
    z <- {a: ["s"], c: "w"};
    r
}"#, refused(NOT_DECIDED); FuseExpect::None);

run!(undecided_member_annotated, r#"{
    let b = false;
    let z: {a: Array<string>, c: string} = never();
    let v = select b { true => {a: [1], c: 1}, false => z };
    let r = select v { {a, c} => "[a] [c]" };
    z <- {a: ["s"], c: "w"};
    r
}"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == r#"["s"] w"#); FuseExpect::Jit);

run!(undecided_member_beside_a_literal_refused, r#"{
    let b = true;
    let z = never();
    let v = select b { true => 1, false => z };
    let r = select v { 1 => "one", _ => "other" };
    z <- 2;
    r
}"#, refused("1 tests a value of 'z, a type not decided at this select"); FuseExpect::None);

run!(undecided_parameter_tested_refused, r#"{
    let f = |z, b: bool| {
        let v = select b { true => {a: [1], c: 1}, false => z };
        select v { {a, ..} => "[a]" }
    };
    f({a: [2], c: 2}, false)
}"#, refused("{ a, .. } tests a value of 'z, a type not decided at this select"); FuseExpect::None);

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
run!(undecided_member_beside_a_guarded_tag_refused, r#"{
    let b = false;
    let g = true;
    let z = never();
    let v = select b { true => (`K, {a: 1, c: 1}), false => (`J, z) };
    let r = select v { (`J, _) if g => "j", (_, {a, ..}) => "[a]" };
    z <- {a: "s", c: "w"};
    r
}"#, refused("(_, { a, .. }) tests a value of 'z, a type not decided at this select"); FuseExpect::None);

// The arms' own cells, a bind's or a `_`'s, are what the arms decided.
run!(arm_cells_are_decided, r#"{
    let f = |x| select x { `A(0) => 1, `A(n) => n, `B(_) => 2 };
    [f(`A(3)), f(`B("s"))]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [3, 2]); FuseExpect::None);

run!(undecided_payload_members_refused, r#"{
    let b = true;
    let x = never();
    let y = never();
    let v = select b { true => `P({a: [1], c: x}), false => `P({a: ["s"], c: y}) };
    let r = select v { `P({a, ..}) => "[a]" };
    x <- 1;
    y <- 2;
    r
}"#, refused(TOLD_APART); FuseExpect::None);

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
}"#, refused("unreachable arm: the earlier arms already cover the whole scrutinee"); FuseExpect::None);

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
}"#, refused("z is string, inferred from an earlier use, and cannot hold i64"); FuseExpect::None);

run!(name_over_an_undecided_field_written_apart_refused, r#"{
    let b = true;
    let z = never();
    let v = select b { true => {a: z, k: 1}, false => {a: "s", k: 2} };
    let r = select v { {a: x, ..} => str::len(x) };
    z <- 5;
    r
}"#, refused("z is string, inferred from an earlier use, and cannot hold i64"); FuseExpect::None);

run!(name_over_an_undecided_guarded_part_written_apart_refused, r#"{
    let b = true;
    let z = never();
    let v = select b { true => (`A, z), false => (`B, "s") };
    let r = select v { (_, x) if true => str::len(x), _ => 0 };
    z <- 5;
    r
}"#, refused("z is string, inferred from an earlier use, and cannot hold i64"); FuseExpect::None);

// A slice with a rest decides the shape of the undecided part it stands
// over, as an annotation would.
run!(slice_over_an_undecided_part_decides_its_shape, r#"{
    let b = true;
    let z = never();
    let v = select b { true => (`A, [1]), false => (`B, z) };
    let r = select v { (_, [x, ..]) => x + 1, (_, _) => 0 };
    z <- ["s"];
    r
}"#, refused("z is Array<i64>, inferred from an earlier use, and cannot hold Array<string>"); FuseExpect::None);

run!(name_over_an_undecided_parameter_part_called_apart_refused, r#"{
    let f = |z, b| {
        let v = select b { true => {a: z, k: 1}, false => {a: "s", k: 2} };
        select v { {a: x, ..} => str::len(x) }
    };
    f(5, true)
}"#, refused("string does not contain i64"); FuseExpect::None);

// A written test is a closed type: a variable nothing declares in it is
// undecided.
run!(written_test_with_an_open_variable_refused, r#"{
    let f = |x: [i64, `A(i64)]| -> i64 select x { ['b, `A(i64)] as `A(n) => n, _ => 0 };
    let w: [i64, `A(i64)] = 4;
    f(w)
}"#, refused("`A(n) tests a value of 'b, a type variable its type test"); FuseExpect::None);

run!(written_test_closed, r#"{
    let f = |x: [i64, `A(i64)]| -> i64 select x { [i64, `A(i64)] as `A(n) => n, _ => 0 };
    let w: [i64, `A(i64)] = 4;
    let a: [i64, `A(i64)] = `A(7);
    [f(w), f(a)]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [0, 7]); FuseExpect::Jit);

run!(written_collection_test_with_an_open_variable_refused, r#"{
    let f = 'c: Collection |xs: 'c<[i64, `Keep(i64)]>| -> i64 select xs {
        Array<['q, `Keep(i64)]> as [`Keep(r)] => r,
        _ => 0
    };
    [f([`Keep(3)]), f([1])]
}"#, refused("[`Keep(r)] tests a value of 'q, a type variable its type test"); FuseExpect::None);

// The narrowing over what reaches the arm decides a written test's own
// variable: one its structure tests is refused all the same.
run!(written_array_element_test_with_an_open_variable_refused, r#"{
    let f = |xs: Array<[i64, `Keep(i64)]>| -> i64 select xs {
        Array<['q, `Keep(i64)]> as [`Keep(r)] => r,
        _ => 0
    };
    [f([`Keep(3)]), f([1])]
}"#, refused("[`Keep(r)] tests a value of 'q, a type variable its type test Array<['q, `Keep(i64)]> leaves open"); FuseExpect::None);

run!(written_list_element_test_with_an_open_variable_refused, r#"{
    let f = |xs: List<[i64, `Keep(i64)]>| -> i64 select xs {
        List<['q, `Keep(i64)]> as [<`Keep(r), ..>] => r,
        _ => 0
    };
    [f([<`Keep(3)>]), f([<1>])]
}"#, refused("[<`Keep(r), ..>] tests a value of 'q, a type variable its type test"); FuseExpect::None);

run!(written_test_with_an_open_variable_over_any_refused, r#"{
    let f = |x: Any| -> i64 select x { ['q, `A(i64)] as `A(n) => n, _ => 0 };
    let s: Any = `A("s");
    [f(`A(3)), f(s)]
}"#, refused("`A(n) tests a value of 'q, a type variable its type test"); FuseExpect::None);

run!(written_array_element_test_with_an_open_variable_over_any_refused, r#"{
    let f = |xs: Array<Any>| -> i64 select xs {
        Array<['q, `Keep(i64)]> as [`Keep(r)] => r,
        _ => 0
    };
    let s: Any = `Keep("s");
    [f([`Keep(3)]), f([s])]
}"#, refused("[`Keep(r)] tests a value of 'q, a type variable its type test"); FuseExpect::None);

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
}"#, refused("(`B(n), m) tests a value of 'q, a type variable its type test"); FuseExpect::None);

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
}"#, refused("`A(n) | `B(n) tests a value of 'q, a type variable its type test"); FuseExpect::None);

run!(written_or_test_over_any_closed, r#"{
    let f = |x: Any| -> i64 select x {
        [`A(i64), `B(i64)] as `A(n) | `B(n) => n,
        _ => 0
    };
    let s: Any = `A("s");
    [f(`A(3)), f(`B(4)), f(s)]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [3, 4, 0]); FuseExpect::Jit);

run!(written_null_test_with_an_open_variable_refused, r#"{
    let f = |x: [i64, null]| -> i64 select x { ['q, null] as null => 0, _ => 1 };
    [f(null), f(1)]
}"#, refused("null tests a value of 'q, a type variable its type test"); FuseExpect::None);

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
}"#, refused("W(`A(n)) tests a value of 'q, a type variable its type test"); FuseExpect::None);

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
}"#, refused("(`J, [i64, string]) as (`J, s) tests a value of 'z, a type not decided at this select"); FuseExpect::None);

run!(written_union_test_of_an_undecided_part_before_a_literal_refused, r#"{
    let b = false;
    let z = never();
    let v = select b { true => (`K, 1), false => (`J, z) };
    let r = select v { (`J, [i64, string]) as (`J, s) => "js", (_, 1) => "one", (_, n) => "n" };
    z <- true;
    r
}"#, refused("(`J, [i64, string]) as (`J, s) tests a value of 'z, a type not decided at this select"); FuseExpect::None);

run!(written_union_test_of_an_undecided_parameter_refused, r#"{
    let f = |b: bool, z| {
        let v = select b { true => (`K, 1), false => (`J, z) };
        select v { (`J, [i64, string]) as (`J, s) => "js", (_, n) => "n" }
    };
    f(false, true)
}"#, refused("(`J, [i64, string]) as (`J, s) tests a value of 'z, a type not decided at this select"); FuseExpect::None);

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
}"#, refused("tests a value of 'w, a type not decided at this select"); FuseExpect::None);

run!(written_union_test_of_two_undecided_parameters_refused, r#"{
    let f = |b: bool, w, z| {
        let v = select b { true => (`K, 1, 1), false => (`J, w, z) };
        select v { [(`J, Any, i64), (`J, i64, Any)] as (`J, x, y) => "j", (_, a, c) => "n" }
    };
    f(false, "s", "t")
}"#, refused("a type not decided at this select"); FuseExpect::None);

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
}"#, refused("type mismatch string does not contain [i64, "); FuseExpect::None);

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
    let e: [Array<(`A, i64)>, Array<(`B, string)>] = [];
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
}"#, refused("missing match cases: no arm covers"); FuseExpect::None);

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
}"#, refused("missing match cases: no arm covers"); FuseExpect::None);

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
}"#, refused("missing match cases: no arm covers"); FuseExpect::None);

run!(written_test_naming_the_declared_variable_covers_its_part, r#"{
    let u = 'b: [i64, string] |x: (bool, ['b, null])| select x {
        (true, _) => 0,
        (bool, 'b) as (false, v) => 1,
        (false, null) => 3
    };
    let (w0, w1): ([i64, string], [i64, string]) = (7, "four");
    [u((true, null)), u((false, w0)), u((false, w1)), u((false, null))]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [0, 1, 1, 3]); FuseExpect::Jit);

// A test naming a declared variable holds only the variable, never its
// bound: an instance may take the variable narrower.
run!(written_test_naming_another_declared_variable_covers_nothing_refused, r#"{
    let f = 'b: [i64, u8] |x: (bool, [i64, u8]), y: 'b| select x {
        (bool, 'b) as (true, a) => 1,
        (false, _) => 2
    };
    f((true, u8:1), i64:3)
}"#, refused("missing match cases: no arm covers"); FuseExpect::None);

run!(written_test_naming_another_declared_variable_beside_a_catch_all, r#"{
    let f = 'b: [i64, u8] |x: (bool, [i64, u8]), y: 'b| select x {
        (bool, 'b) as (true, a) => 1,
        _ => 2
    };
    [f((true, u8:1), i64:3), f((true, i64:1), i64:3), f((false, i64:1), i64:3)]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [2, 1, 2]); FuseExpect::Jit);

// Every combination a written test matches in each part it reaches,
// earlier arms match: dead.
run!(written_test_after_bool_head_is_dead, r#"{
    let u = |x: (bool, [i64, null])| select x {
        (true, _) => 0,
        (bool, i64) as (true, n) => n,
        _ => 1
    };
    u((false, 7))
}"#, refused("every combination of heads it matches is covered by earlier arms"); FuseExpect::None);

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
}"#, refused("missing match cases: no arm covers"); FuseExpect::None);

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
}"#, refused("missing match cases: no arm covers"); FuseExpect::None);

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
}"#, refused("every combination of heads it matches is covered by earlier arms"); FuseExpect::None);

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
}"#, refused("missing match cases"); FuseExpect::None);

run!(written_nested_constructor_beside_a_catch_all, r#"{
    let g = 'b: Number |x: {f: ['b, `Keep('b)]}| -> ['b, null] select x {
        {f: ['b, `Keep('b)]} as {f: `Keep(r)} => r,
        _ => null
    };
    let w: {f: [i64, `Keep(i64)]} = {f: 5};
    let k: {f: [i64, `Keep(i64)]} = {f: `Keep(4)};
    "[g(w)] [g(k)]"
}"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "null 4"); FuseExpect::Jit);

// An abstract pattern whose payload tests one member of the representation
// covers that member alone.
run!(abstract_payload_variant_covers_its_member_alone, r#"{
    type R<'a> = Abstract<['a, `Err(string)]>;
    let v: [i64, `Err(string)] = 5;
    let x: R<i64> = R(v);
    select x { R(`Err(m)) => str::len(m) }
}"#, refused("missing match cases"); FuseExpect::None);

run!(abstract_payload_parameter_undecided_refused, r#"{
    type R<'a> = Abstract<['a, `Err(string)]>;
    let v: [i64, `Err(string)] = 5;
    select R(v) { R(`Err(m)) => str::len(m), _ => 0 }
}"#, refused("R(`Err(m)) tests a value whose type is not decided at this select"); FuseExpect::None);

run!(abstract_payload_variant_beside_a_catch_all, r#"{
    type R<'a> = Abstract<['a, `Err(string)]>;
    let f = 'a: Number |x: R<'a>| -> i64 select x { R(`Err(m)) => str::len(m), _ => 0 };
    let v: [i64, `Err(string)] = `Err("abc");
    let w: [f64, `Err(string)] = 1.5;
    [f(R(v)), f(R(w))]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [3, 0]); FuseExpect::Jit);

// An arm whose pattern would decide a part the scrutinee held undecided
// before the arms tests it: refused, naming the binding to annotate.
run!(tag_deciding_an_undecided_part_refused, r#"{
    let f = |x| select (x, 0) { (`J, _) => 1, (`M, _) => 2 };
    f(`M)
}"#, refused("(`J, _) tests a value of 'x, a type not decided at this select"); FuseExpect::None);

run!(tag_deciding_an_undecided_part_beside_a_wildcard_refused, r#"{
    let f = |x| select (x, 0) { (`J, _) => 1, _ => 2 };
    f(`M)
}"#, refused("(`J, _) tests a value of 'x, a type not decided at this select"); FuseExpect::None);

run!(tag_over_an_annotated_part, r#"{
    let f = |x: [`J, `M]| select (x, 0) { (`J, _) => 1, (`M, _) => 2 };
    [f(`M), f(`J)]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [2, 1]); FuseExpect::Jit);

run!(tag_deciding_a_later_written_part_refused, r#"{
    let w = never();
    let r = select (w, 0) { (`J, _) => 1, (`M, _) => 2, _ => 3 };
    w <- `M;
    r
}"#, refused("(`J, _) tests a value of 'w, a type not decided at this select"); FuseExpect::None);

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
}"#, refused("[`J] tests a value of 'w, a type not decided at this select"); FuseExpect::None);

run!(payload_tag_deciding_an_undecided_part_refused, r#"{
    let w = never();
    let r = select `A(w) { `A(`J) => 1, _ => 2 };
    w <- `M;
    r
}"#, refused("`A(`J) tests a value of 'w, a type not decided at this select"); FuseExpect::None);

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
}"#, refused("(`J, i64) as (j, _) tests a value of 'w, a type not decided at this select"); FuseExpect::None);

run!(written_test_over_an_annotated_part, r#"{
    let w: [`J, `M] = never();
    let r = select (w, 0) { (`J, i64) as (j, _) => 1, _ => 2 };
    w <- `M;
    r
}"#, |v: Result<&Value>| matches!(v, Ok(Value::I64(2))); FuseExpect::Jit);

run!(partial_struct_completing_an_undecided_member_refused, r#"{
    let b = false;
    let z = never();
    let v = select b { true => (`K, {a: 1, c: 1}), false => (`J, z) };
    let r = select v { (`K, _) => "k", (_, {a, ..}) => "[a]" };
    z <- {a: "s", c: "w"};
    r
}"#, refused("(_, { a, .. }) tests a value of 'z, a type not decided at this select"); FuseExpect::None);

run!(partial_struct_over_an_annotated_member, r#"{
    let b = false;
    let z: {a: string, c: string} = never();
    let v = select b { true => (`K, {a: 1, c: 1}), false => (`J, z) };
    let r = select v { (`K, _) => "k", (_, {a, ..}) => "[a]" };
    z <- {a: "s", c: "w"};
    r
}"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "s"); FuseExpect::Jit);

run!(partial_struct_over_a_lone_undecided_cell_refused, r#"{
    let z = never();
    let r = select z { {a, ..} => "[a]" };
    z <- {a: ["s"], c: "w"};
    r
}"#, refused("{ a, .. } tests a value of 'z, a type not decided at this select"); FuseExpect::None);

run!(partial_struct_over_an_undecided_parameter_refused, r#"{
    let f = |x| select x { {a, ..} => "[a]" };
    f({a: ["s"], c: "w"})
}"#, refused("{ a, .. } tests a value of 'x, a type not decided at this select"); FuseExpect::None);

run!(partial_struct_over_an_annotated_parameter, r#"{
    let f = |x: {a: Array<string>, c: string}| select x { {a, ..} => "[a]" };
    f({a: ["s"], c: "w"})
}"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == r#"["s"]"#); FuseExpect::Jit);

run!(or_alternative_deciding_an_undecided_part_refused, r#"{
    let b = false;
    let z = never();
    let v = select b { true => (`K, 1), false => (`J, z) };
    let r = select v { (`J, _) | (_, 1) => "j1", _ => "o" };
    z <- "q";
    r
}"#, refused("(`J, _) | (_, 1) tests a value of 'z, a type not decided at this select"); FuseExpect::None);

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
run!(tag_deciding_an_undecided_part_beside_a_bind_refused, r#"{
    let z = never();
    let k: [i64, null] = 1;
    let u = select k { null as _ => z, n => n };
    let r = select (u, 0) { (42, v) => v, (a, b) => a };
    z <- 2;
    r
}"#, refused("(42, v) tests a value of 'z, a type not decided at this select"); FuseExpect::None);

run!(tag_over_an_annotated_part_beside_a_bind, r#"{
    let z: i64 = never();
    let k: [i64, null] = 1;
    let u = select k { null as _ => z, n => n };
    let r = select (u, 0) { (42, v) => v, (a, b) => a };
    z <- 2;
    r
}"#, |v: Result<&Value>| matches!(v, Ok(Value::I64(1))); FuseExpect::Jit);

run!(written_test_deciding_an_undecided_part_beside_a_bind_refused, r#"{
    let z = never();
    let k: [i64, null] = 1;
    let u = select k { null as _ => z, i64 as n => n };
    let r = select (u, 0) { (i64, i64) as (a, b) => a };
    z <- 2;
    r
}"#, refused("(i64, i64) as (a, b) tests a value of 'z, a type not decided at this select"); FuseExpect::None);

run!(written_test_over_an_annotated_part_beside_a_bind, r#"{
    let z: i64 = never();
    let k: [i64, null] = 1;
    let u = select k { null as _ => z, i64 as n => n };
    let r = select (u, 0) { (i64, i64) as (a, b) => a };
    z <- 2;
    r
}"#, |v: Result<&Value>| matches!(v, Ok(Value::I64(1))); FuseExpect::Jit);

run!(tag_deciding_an_undecided_parameter_beside_a_bind_refused, r#"{
    let f = |z, k: [i64, null]| {
        let u = select k { null as _ => z, i64 as n => n };
        select (u, 0) { (42, v) => v, (a, b) => a }
    };
    f(3, 1)
}"#, refused("(42, v) tests a value of 'z, a type not decided at this select"); FuseExpect::None);

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
}"#, refused("z is i64, inferred from an earlier use, and cannot hold string"); FuseExpect::None);

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
}"#, refused("x is i64, inferred from an earlier use, and cannot hold string"); FuseExpect::None);

// A later arm's pattern decides the part before an earlier arm's body is
// checked: the body's error is the select's.
run!(an_earlier_body_over_a_part_a_later_arm_decides_is_its_error, r#"{
    let f = |b: bool, x| select (x, 0) {
        (a, _) if b => select a { `J => 1, `M => 2 },
        (`M, _) => 3,
        _ => 4
    };
    f(true, `M)
}"#, refused("pattern `J will never match `M"); FuseExpect::None);

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
run!(whole_open_scrutinee_decided_by_the_arms, r#"{
    let w = never();
    let r = select w { `J => 1, `M => 2 };
    w <- `M;
    r
}"#, |v: Result<&Value>| matches!(v, Ok(Value::I64(2))); FuseExpect::Jit);

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
run!(tag_deciding_a_part_below_a_reference_refused, r#"{
    let f = |x| { let r = &mut (x, 0); select *r { (`J, _) => 1, (`M, _) => 3 } };
    f(`M)
}"#, refused("(`J, _) tests a value of 'x, a type not decided at this select"); FuseExpect::None);

run!(tag_deciding_a_part_below_a_reference_beside_a_wildcard_refused, r#"{
    let f = |x| { let r = &mut (x, 0); select *r { (`J, _) => 1, (_, _) => 3 } };
    f(`M)
}"#, refused("(`J, _) tests a value of 'x, a type not decided at this select"); FuseExpect::None);

run!(field_tag_deciding_a_part_below_a_reference_refused, r#"{
    let f = |x| { let r = &mut {a: x, b: 0}; select *r { {a: `J, b} => b, {a, b} => 3 } };
    f(`M)
}"#, refused("{ a: `J, b } tests a value of 'x, a type not decided at this select"); FuseExpect::None);

run!(written_test_deciding_a_part_below_a_reference_refused, r#"{
    let f = |x| { let r = &mut (x, 0); select *r { ([`J, `M], i64) as (`J, _) => 1, _ => 3 } };
    f(`M)
}"#, refused("([`J, `M], i64) as (`J, _) tests a value of 'x, a type not decided at this select"); FuseExpect::None);

run!(tag_deciding_a_part_below_an_abstract_parameter_refused, r#"{
    type W<'a> = Abstract<'a>;
    let f = |x| select W((x, 0)).0 { (`J, _) => 1, (`M, _) => 3, _ => 2 };
    f(`M)
}"#, refused("(`J, _) tests a value of 'x, a type not decided at this select"); FuseExpect::None);

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
run!(tag_testing_an_open_part_below_a_reference_refused, r#"{
    let f = |x| { let r = &mut [x, 0]; select *r { [`J, _] => 1, _ => 3 } };
    f(`M)
}"#, refused("[`J, _] tests a value of 'x, a type not decided at this select, which a later use could decide apart from what the arms test: annotate it, writing for 'x the type of `x` (line 2"); FuseExpect::None);

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

run!(tag_testing_an_open_part_the_referent_keeps_refused, r#"{
    let f = |x| { let r = &mut [x, 0]; select *r { [`J, ..] => 1, _ => 3 } };
    f(`M)
}"#, refused("[`J, ..] tests a value of 'x, a type not decided at this select"); FuseExpect::None);

run!(tag_testing_an_open_part_below_two_references_refused, r#"{
    let f = |x| {
        let r = &mut [x, 0];
        let s = &mut r;
        select **s { [`J, _] => 1, _ => 3 }
    };
    f(`M)
}"#, refused("[`J, _] tests a value of 'x, a type not decided at this select"); FuseExpect::None);

run!(tag_testing_an_open_part_below_an_abstract_parameter_refused, r#"{
    type W<'a> = Abstract<'a>;
    let f = |x| select W([x, 0]).0 { [`J, _] => 1, _ => 3 };
    f(`M)
}"#, refused("[`J, _] tests a value of 'x, a type not decided at this select"); FuseExpect::None);

run!(tag_over_an_annotated_open_part_below_an_abstract_parameter, r#"{
    type W<'a> = Abstract<'a>;
    let f = |x: [`J, `M]| select W([x, 0]).0 { [`J, _] => 1, _ => 3 };
    [f(`M), f(`J)]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [3, 1]); FuseExpect::None);

run!(tag_testing_a_never_let_below_a_reference_refused, r#"{
    let z = never();
    let r = &mut [z, 0];
    let o = select *r { [`J, _] => 1, _ => 3 };
    z <- `J;
    o
}"#, refused("[`J, _] tests a value of 'z, a type not decided at this select"); FuseExpect::None);

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
}"#, refused("[`J, _] tests a value of 'x, a type not decided at this select"); FuseExpect::None);

run!(destructuring_an_open_part_below_a_reference, r#"{
    let f = |x| { let r = &mut [x, 0]; select *r { [(a, b), _] => a, _ => 2 } };
    [f((1, 2)), f((7, 2))]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [1, 7]); FuseExpect::None);

run!(literal_testing_an_open_part_below_a_reference_refused, r#"{
    let f = |x| { let r = &mut [x, 0]; select *r { [1, _] => 1, _ => 3 } };
    f(2)
}"#, refused("[1, _] tests a value of 'x, a type not decided at this select"); FuseExpect::None);

run!(literal_over_an_annotated_open_part_below_a_reference, r#"{
    let f = |x: i64| { let r = &mut [x, 0]; select *r { [1, _] => 1, _ => 3 } };
    [f(2), f(1)]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [3, 1]); FuseExpect::None);

run!(or_alternative_testing_an_open_part_below_a_reference_refused, r#"{
    let f = |x| { let r = &mut [x, 0]; select *r { [`J | `K, _] => 1, _ => 3 } };
    f(`M)
}"#, refused("[`J | `K, _] tests a value of 'x, a type not decided at this select"); FuseExpect::None);

run!(list_tag_testing_an_open_part_below_a_reference_refused, r#"{
    let f = |x| { let r = &mut [<x, 0>]; select *r { [<`J, t..>] => 1, _ => 3 } };
    f(`M)
}"#, refused("[<`J, t..>] tests a value of 'x, a type not decided at this select"); FuseExpect::None);

run!(payload_tag_testing_an_open_part_below_a_reference_refused, r#"{
    let f = |x| { let r = &mut `P(x); select *r { `P(`J) => 1, `P(_) => 3 } };
    f(`M)
}"#, refused("`P(`J) tests a value of 'x, a type not decided at this select"); FuseExpect::None);

run!(payload_tag_over_an_annotated_open_part_below_a_reference, r#"{
    let f = |x: [`J, `M]| { let r = &mut `P(x); select *r { `P(`J) => 1, `P(_) => 3 } };
    [f(`M), f(`J)]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [3, 1]); FuseExpect::None);

run!(tag_testing_an_open_part_below_a_captured_reference_refused, r#"{
    let f = |x| {
        let r = &mut [x, 0];
        let g = || select *r { [`J, _] => 1, _ => 3 };
        g()
    };
    f(`M)
}"#, refused("[`J, _] tests a value of 'x, a type not decided at this select"); FuseExpect::None);

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

// No test holds the member as it stands (the cover binds a part), and two
// tests could take it, each deciding another of its cells: which did would
// follow their place in the arms' union.
run!(built_part_two_tests_could_take_refused, r#"{
    let f = |x, y, b: bool| {
        let r = &mut (select b { true => (`K, x, y), false => (`L, 0, "a") });
        select *r { (`K, n, _) => n, (_, 0, _) => 2, (_, _, "a") => 3, _ => 4 }
    };
    [f(5, "s", true), f(5, "s", false)]
}"#, refused("(`K, 'x, 'y), built into it, could go to (Any, Any, string) or (Any, i64, Any) of the types they test, neither of which holds it as it stands: annotate it, writing for 'x the type of `x`"); FuseExpect::None);

run!(built_part_two_tests_could_take_in_another_order_refused, r#"{
    let f = |x, y, b: bool| {
        let r = &mut (select b { true => (y, x, `K), false => ("a", 0, `L) });
        select *r { (_, n, `K) => n, (_, 0, _) => 2, ("a", _, _) => 3, _ => 4 }
    };
    [f(5, "s", true), f(5, "s", false)]
}"#, refused("('y, 'x, `K), built into it, could go to (Any, i64, Any) or (string, Any, Any) of the types they test, neither of which holds it as it stands: annotate it, writing for 'y the type of `y`"); FuseExpect::None);

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
run!(arm_behind_half_a_pool_of_a_member_below_a_reference_refused, r#"{
    let f = |x, b: bool, c: bool| {
        let r = &mut (select c { true => (b, x), false => (`L, 0) });
        select *r { (true, _) => 1, (_, 0) => 3, _ => 4 }
    };
    f("s", true, true)
}"#, refused("(_, 0) tests a value of 'x, a type not decided at this select"); FuseExpect::None);

run!(arm_behind_a_guarded_pool_of_a_member_below_a_reference_refused, r#"{
    let f = |x, b: bool, c: bool| {
        let r = &mut (select c { true => (b, x), false => (`L, 0) });
        select *r { (true, _) if c => 1, (false, _) => 2, (_, 0) => 3, _ => 4 }
    };
    f("s", true, true)
}"#, refused("(_, 0) tests a value of 'x, a type not decided at this select"); FuseExpect::None);

run!(arm_between_the_halves_of_a_pool_below_a_reference_refused, r#"{
    let f = |x, b: bool, c: bool| {
        let r = &mut (select c { true => (b, x), false => (`L, 0) });
        select *r { (true, _) => 1, (_, 0) => 3, (false, _) => 2, _ => 4 }
    };
    f("s", true, true)
}"#, refused("(_, 0) tests a value of 'x, a type not decided at this select"); FuseExpect::None);

run!(arm_behind_a_pool_tests_the_member_that_reaches_it_refused, r#"{
    let f = |x, y, b: bool, c: bool| {
        let r = &mut (select c { true => (b, x), false => (`L, y) });
        select *r { (true, _) => 1, (false, _) => 2, (_, 0) => 3, _ => 4 }
    };
    f(1, 0, true, true)
}"#, refused("(_, 0) tests a value of 'y, a type not decided at this select"); FuseExpect::None);

// A cell of the member that the arms decided is not one the pool's arms
// hold whatever it becomes: the test behind them is what decided it.
run!(arm_behind_a_pool_over_a_cell_the_arms_decided_refused, r#"{
    let f = |x, b: bool, c: bool| {
        let r = &mut (select c { true => (b, x), false => (`L, 0) });
        select *r { (true, n) => 1, (false, m) => 2, (_, 0) => 3, _ => 4 }
    };
    f(0, true, true)
}"#, refused("(_, 0) tests a value of 'x, a type not decided at this select"); FuseExpect::None);

// The arms up to a dead one are judged behind the pool too, so the dead
// arm is what is refused.
run!(dead_arm_behind_a_pool_of_a_member_below_a_reference_refused, r#"{
    let f = |x, b: bool, c: bool| {
        let r = &mut (select c { true => (b, x), false => (`L, 0) });
        select *r { (true, _) => 1, (false, _) => 2, (_, 0) => 3, (true, _) => 5, _ => 4 }
    };
    f(1, true, true)
}"#, refused("unreachable arm"); FuseExpect::None);

// A pool counts every head the member's part admits: three for a nullable
// bool, and two of the three free no arm behind them.
run!(arm_behind_a_pool_of_three_heads_below_a_reference, r#"{
    let f = |x, b: [bool, null], c: bool| {
        let r = &mut (select c { true => (b, x), false => (`L, 0) });
        select *r { (true, _) => 1, (false, _) => 2, (null, _) => 5, (_, 0) => 3, _ => 4 }
    };
    [f("s", true, true), f("s", null, true), f(0, null, true), f(0, false, true), f(0, true, false)]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [1, 5, 5, 2, 3]); FuseExpect::Jit);

run!(arm_behind_two_heads_of_three_below_a_reference_refused, r#"{
    let f = |x, b: [bool, null], c: bool| {
        let r = &mut (select c { true => (b, x), false => (`L, 0) });
        select *r { (true, _) => 1, (false, _) => 2, (_, 0) => 3, _ => 4 }
    };
    f(0, null, true)
}"#, refused("(_, 0) tests a value of 'x, a type not decided at this select"); FuseExpect::None);

// Each member of the bound has a pool of its own, and an or-arm's
// alternatives pool each against the member it takes.
run!(arm_behind_the_pools_of_two_members_below_a_reference, r#"{
    let f = |x, y, a: bool, b: bool, c: bool| {
        let r = &mut (select c { true => (a, x), false => (`L, b, y) });
        select *r {
            (true, _) => 1,
            (false, _) => 2,
            (`L, true, _) => 3,
            (`L, false, _) => 4,
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
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [1, 2, 3, 4, 1, 2, 3, 1]); FuseExpect::Jit);

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

run!(arm_behind_a_cover_tests_the_member_that_reaches_it_refused, r#"{
    let f = |x, y, b: bool| {
        let r = &mut (select b { true => (`K, x), false => (`L, y) });
        select *r { (`K, _) => 1, (_, 0) => 2, _ => 3 }
    };
    [f(1, 0, true), f(1, 0, false)]
}"#, refused("(_, 0) tests a value of 'y, a type not decided at this select"); FuseExpect::None);

run!(arm_behind_a_cover_of_another_member_of_its_cell_refused, r#"{
    let f = |x, b: bool| {
        let r = &mut (select b { true => (`K, x), false => (`L, x) });
        select *r { (`K, _) => 1, (_, 0) => 2, _ => 3 }
    };
    [f(1, true), f(1, false)]
}"#, refused("(_, 0) tests a value of 'x, a type not decided at this select"); FuseExpect::None);

run!(arm_behind_a_guarded_arm_over_a_member_below_a_reference_refused, r#"{
    let f = |x, b: bool| {
        let r = &mut (select b { true => (`K, x), false => (`L, 0) });
        select *r { (`K, _) if b => 1, (_, 0) => 2, _ => 3 }
    };
    [f(1, true), f(1, false)]
}"#, refused("(_, 0) tests a value of 'x, a type not decided at this select"); FuseExpect::None);

run!(arm_before_the_cover_of_a_member_below_a_reference_refused, r#"{
    let f = |x, b: bool| {
        let r = &mut (select b { true => (`K, x), false => (`L, 0) });
        select *r { (_, 0) => 2, (`K, _) => 1, _ => 3 }
    };
    [f(1, true), f(1, false)]
}"#, refused("(_, 0) tests a value of 'x, a type not decided at this select"); FuseExpect::None);

run!(covering_arm_testing_its_member_below_a_reference_refused, r#"{
    let f = |x, b: bool| {
        let r = &mut (select b { true => (`K, x), false => (`L, 0) });
        select *r { (`K, i64) as (_, n) => n, (_, 0) => 2, _ => 3 }
    };
    [f(1, true), f(1, false)]
}"#, refused("(`K, i64) as (_, n) tests a value of 'x, a type not decided at this select"); FuseExpect::None);

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
}"#, refused("unreachable arm: the earlier arms already cover the whole scrutinee"); FuseExpect::None);

run!(tag_testing_a_declared_variable_below_a_reference_refused, r#"{
    let f = |x: 'a| { let r = &mut [x, 0]; select *r { [`J, _] => 1, _ => 3 } };
    [f(`M), f(`J)]
}"#, refused("[`J, _] tests inside a value of 'a, a type variable of the definition, which types none of its parts: write the type it tests, `T as [`J, _]`, or bound 'a"); FuseExpect::None);

run!(tag_testing_a_declared_variable_the_referent_keeps_refused, r#"{
    let f = |x: 'a| { let r = &mut [x, 0]; select *r { [`J, ..] => 1, _ => 3 } };
    [f(`M), f(`J)]
}"#, refused("[`J, ..] tests inside a value of 'a"); FuseExpect::None);

run!(tag_testing_a_bounded_variable_below_a_reference_refused, r#"{
    let f = 'a: [`J, `M] |x: 'a| { let r = &mut [x, 0]; select *r { [`J, _] => 1, _ => 3 } };
    [f(`M), f(`J)]
}"#, refused("[`J, _] tests inside a value of 'a, a type variable of the definition, which types none of its parts: write the type it tests, `Array<[i64, `J, `M]> as [`J, _]`"); FuseExpect::None);

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

run!(tag_testing_a_declared_variable_in_a_tuple_below_a_reference_refused, r#"{
    let f = |x: 'a| { let r = &mut (x, 0); select *r { (`J, _) => 1, _ => 3 } };
    [f(`M), f(`J)]
}"#, refused("(`J, _) tests inside a value of 'a, a type variable of the definition"); FuseExpect::None);

run!(tag_testing_a_declared_variable_below_a_read_only_reference_refused, r#"{
    let f = |x: 'a| { let r = &[x, 0]; select *r { [`J, _] => 1, _ => 3 } };
    [f(`M), f(`J)]
}"#, refused("[`J, _] tests inside a value of 'a"); FuseExpect::None);

run!(literal_testing_a_declared_variable_below_a_reference_refused, r#"{
    let f = |x: 'a| { let r = &mut [x, 0]; select *r { [0, _] => 1, _ => 3 } };
    [f(1), f(0)]
}"#, refused("[0, _] tests inside a value of 'a"); FuseExpect::None);

run!(tag_testing_a_declared_variable_below_an_abstract_parameter_refused, r#"{
    type W<'a> = Abstract<'a>;
    let f = |x: 'a| select W([x, 0]).0 { [`J, _] => 1, _ => 3 };
    [f(`M), f(`J)]
}"#, refused("[`J, _] tests inside a value of 'a"); FuseExpect::None);

run!(written_test_over_a_bounded_variable_below_an_abstract_parameter, r#"{
    type W<'a> = Abstract<'a>;
    let f = 'a: [`J, `M] |x: 'a| select W([x, 0]).0 {
        Array<[i64, `J, `M]> as [`J, _] => 1,
        _ => 3
    };
    [f(`M), f(`J)]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [3, 1]); FuseExpect::Jit);

run!(tag_testing_a_declared_variable_a_write_brings_refused, r#"{
    let f = |x: 'a| { let v = never(); v <- [x, 0]; select v { [`J, _] => 1, _ => 3 } };
    [f(`M), f(`J)]
}"#, refused("[`J, _] tests inside a value of 'a"); FuseExpect::None);

run!(written_test_holding_bottom_over_a_variable_below_a_reference_refused, r#"{
    let f = 'b: [`A(i64), `B] |x: 'b| {
        let r = &mut (x, 0);
        select *r { (`A(_), i64) as (`A(z), _) => 1, _ => 2 }
    };
    [f(`B), f(`A(7))]
}"#, refused("(`A(z), _) tests inside a value of 'b, a type variable of the definition, and its type test types none of it: `A(_) holds `_`, which is bottom; write the type it tests, `([`A(i64), `B], i64) as (`A(z), _)`"); FuseExpect::None);

run!(written_test_holding_its_own_variable_below_a_reference_refused, r#"{
    let f = 'b: [`A(i64), `B] |x: 'b| {
        let r = &mut (x, 0);
        select *r { ('c, i64) as (`A(z), _) => 1, _ => 2 }
    };
    [f(`B), f(`A(7))]
}"#, refused("(`A(z), _) tests inside a value of 'b, a type variable of the definition, and its type test types none of it: 'c is a type variable nothing declares"); FuseExpect::None);

run!(closed_written_test_over_a_variable_below_a_reference, r#"{
    let f = 'b: [`A(i64), `B] |x: 'b| {
        let r = &mut (x, 0);
        select *r { ([`A(i64), `B], i64) as (`A(z), _) => z, _ => 2 }
    };
    [f(`B), f(`A(7))]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [2, 7]); FuseExpect::None);

// A destructuring `let` meets the variable below a bound as a select does.
run!(let_binding_inside_a_declared_variable_below_a_reference_refused, r#"{
    let f = 'a: (i64, i64) |x: 'a| { let r = &mut (x, 0); let ((a, b), c) = *r; a + b };
    f((1, 2))
}"#, refused("the pattern ((a, b), c) binds inside a value of 'a, a type variable of the definition, which types none of its parts: annotate the binding, `let ((a, b), c): ((i64, i64), i64) = ..`"); FuseExpect::None);

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
}"#, refused("the pattern ((a, b), c) binds inside a value of 'a, a type variable of the definition, which types none of its parts: annotate the binding with the type it binds, `let ((a, b), c): T = ..`, or bound 'a"); FuseExpect::None);

run!(let_binding_inside_a_declared_variable_a_read_passes_on_refused, r#"{
    let f = 'a: (i64, i64) |x: 'a| {
        let r = &mut (x, 0);
        let y = *r;
        let ((a, b), c) = y;
        a + b
    };
    f((1, 2))
}"#, refused("the pattern ((a, b), c) binds inside a value of 'a"); FuseExpect::None);

run!(let_binding_inside_a_declared_variable_in_a_struct_below_a_reference_refused, r#"{
    let f = 'a: {p: i64} |x: 'a| { let r = &mut {q: x}; let {q: {p}} = *r; p };
    f({p: 1})
}"#, refused("the pattern { q: { p } } binds inside a value of 'a, a type variable of the definition, which types none of its parts: annotate the binding, `let { q: { p } }: { q: { p: i64 } } = ..`"); FuseExpect::None);

run!(let_binding_inside_a_declared_variable_below_an_abstract_parameter_refused, r#"{
    type W<'a> = Abstract<'a>;
    let f = 'a: (i64, i64) |x: 'a| { let ((a, b), c) = W((x, 0)).0; a + b };
    f((1, 2))
}"#, refused("the pattern ((a, b), c) binds inside a value of 'a"); FuseExpect::None);

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
}"#, refused("unreachable arm: the earlier arms already cover the whole scrutinee"); FuseExpect::None);

run!(dead_arm_deciding_a_part_below_a_reference_is_unreachable, r#"{
    let f = |x| { let r = &mut (x, 0); select *r { _ => 3, (`J, _) => 1 } };
    f(`M)
}"#, refused("unreachable arm: the earlier arms already cover the whole scrutinee"); FuseExpect::None);

// The referent of a reference to an expression no binding names is a cell
// no source can write: a hint names a binding holding the reference.
run!(tag_deciding_a_referent_names_the_reference_refused, r#"{
    let f = |x| { let r = &mut (x, 0); select (*r, 1) { ((`J, _), _) => 1, ((`M, _), _) => 3 } };
    f(`M)
}"#, refused("((`J, _), _) tests a value of 'r, a type not decided at this select, which a later use could decide apart from what the arms test: annotate it, writing for 'r the type `r` refers to (line 2"); FuseExpect::None);

run!(tag_deciding_a_referent_of_an_annotated_part_names_the_reference_refused, r#"{
    let f = |x: [`J, `M]| { let r = &mut (x, 0); select (*r, 1) { ((`J, _), _) => 1, ((`M, _), _) => 3 } };
    f(`M)
}"#, refused("annotate it, writing for 'r the type `r` refers to (line 2"); FuseExpect::None);

run!(tag_deciding_a_referent_names_the_first_reference_refused, r#"{
    let f = |x| {
        let r = &mut (x, 0);
        let s = r;
        select (*s, 1) { ((`J, _), _) => 1, ((`M, _), _) => 3 }
    };
    f(`M)
}"#, refused("tests a value of 'r, a type not decided at this select, which a later use could decide apart from what the arms test: annotate it, writing for 'r the type `r` refers to (line 3"); FuseExpect::None);

run!(tag_deciding_a_referent_in_a_struct_names_the_struct_refused, r#"{
    let f = |x| {
        let st = {r: &mut (x, 0), n: 1};
        select (*st.r, 1) { ((`J, _), _) => 1, ((`M, _), _) => 3 }
    };
    f(`M)
}"#, refused("tests a value whose type is a part of 'st, a type not decided at this select, which a later use could decide apart from what the arms test: annotate it, writing for 'st the type of `st` (line 3"); FuseExpect::None);

run!(tag_deciding_a_referent_nothing_holds_refused, r#"{
    let f = |x| select (*(&mut (x, 0)), 1) { ((`J, _), _) => 1, ((`M, _), _) => 3 };
    f(`M)
}"#, refused("((`J, _), _) tests a value whose type is not decided at this select, which a later use could decide apart from what the arms test: annotate the scrutinee's type where it is built"); FuseExpect::None);

run!(tag_over_a_referent_read_into_an_annotated_binding, r#"{
    let f = |x| {
        let r = &mut (x, 0);
        let y: ([`J, `M], i64) = *r;
        select (y, 1) { ((`J, _), _) => 1, ((`M, _), _) => 3 }
    };
    [f(`M), f(`J)]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [3, 1]); FuseExpect::Jit);

run!(tag_deciding_a_referent_read_into_a_binding_names_it_refused, r#"{
    let f = |x| {
        let r = &mut (x, 0);
        let y = *r;
        select (y, 1) { ((`J, _), _) => 1, ((`M, _), _) => 3 }
    };
    f(`M)
}"#, refused("tests a value of 'y, a type not decided at this select, which a later use could decide apart from what the arms test: annotate it, writing for 'y the type of `y` (line 4"); FuseExpect::None);

run!(tag_deciding_a_referent_through_a_call_names_the_reference_refused, r#"{
    let f = |x| {
        let r = &mut (x, 0);
        let g = |q: &mut ('a, i64)| *q;
        select (g(r), 1) { ((`J, _), _) => 1, ((`M, _), _) => 3 }
    };
    f(`M)
}"#, refused("tests a value whose type is a part of 'r, a type not decided at this select, which a later use could decide apart from what the arms test: annotate it, writing for 'r the type of `r` (line 3"); FuseExpect::None);

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
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [3, 1]); FuseExpect::None);

run!(partial_struct_over_a_whole_abstract_parameter_refused, r#"{
    type W<'a> = Abstract<'a>;
    let f = |x| select W(x).0 { {a: `J, ..} => 1, _ => 3 };
    f({a: `M, b: 1})
}"#, refused("{ a: `J, .. } tests a value whose type is not decided at this select"); FuseExpect::None);

run!(destructuring_a_part_below_an_abstract_parameter, r#"{
    type W<'a> = Abstract<'a>;
    let f = |x| select W((x, 0)).0 { ((a, b), _) => a + b };
    f((1, 2))
}"#, |v: Result<&Value>| matches!(v, Ok(Value::I64(3))); FuseExpect::Jit);

// A pattern that chooses a value, a tag or a length at an undecided part
// decides it to that choice: refused as testing it.
run!(bool_literals_deciding_an_undecided_part_refused, r#"{
    let f = |x| select (x, 0) { (true, _) => 1, (false, _) => 2 };
    f(true)
}"#, refused("(true, _) tests a value of 'x, a type not decided at this select"); FuseExpect::None);

run!(bool_literals_over_an_annotated_part, r#"{
    let f = |x: bool| select (x, 0) { (true, _) => 1, (false, _) => 2 };
    [f(true), f(false)]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [1, 2]); FuseExpect::Jit);

run!(literal_deciding_an_undecided_part_refused, r#"{
    let f = |x| select (x, 0) { (1, _) => 1, _ => 2 };
    f(2)
}"#, refused("(1, _) tests a value of 'x, a type not decided at this select"); FuseExpect::None);

run!(empty_slice_deciding_an_undecided_part_refused, r#"{
    let f = |x| select (x, 0) { ([], _) => 1, _ => 2 };
    f([1])
}"#, refused("([], _) tests a value of 'x, a type not decided at this select"); FuseExpect::None);

run!(fixed_length_slice_deciding_an_undecided_part_refused, r#"{
    let f = |x| select (x, 0) { ([a, b], _) => a + b, (_, _) => 0 };
    f([1, 2])
}"#, refused("([a, b], _) tests a value of 'x, a type not decided at this select"); FuseExpect::None);

run!(fixed_length_slice_over_an_annotated_part, r#"{
    let f = |x: Array<i64>| select (x, 0) { ([a, b], _) => a + b, (_, _) => 0 };
    [f([1, 2]), f([1])]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [3, 0]); FuseExpect::Jit);

run!(list_head_deciding_an_undecided_part_refused, r#"{
    let f = |x| select (x, 0) { ([<`J, ..>], _) => 1, _ => 2 };
    f([<`M>])
}"#, refused("([<`J, ..>], _) tests a value of 'x, a type not decided at this select"); FuseExpect::None);

run!(or_pattern_deciding_an_undecided_part_refused, r#"{
    let f = |x| select (x, 0) { ((a, b) | (a, b, _), _) => a + b };
    f((1, 2))
}"#, refused("((a, b) | (a, b, _), _) tests a value of 'x, a type not decided at this select"); FuseExpect::None);

run!(partial_struct_deciding_an_undecided_part_refused, r#"{
    let f = |x| select (x, 0) { ({a, ..}, _) => a };
    f({a: 1, b: 2})
}"#, refused("({ a, .. }, _) tests a value of 'x, a type not decided at this select"); FuseExpect::None);

// A destructure decides the part's shape as an annotation would: an arm of
// another shape after it is dead.
run!(other_shape_than_an_earlier_arm_decided_is_dead, r#"{
    let f = |x| select (x, 0) { ((a, b), _) => a + b, ((a, b, c), _) => a };
    f((1, 2))
}"#, refused("unreachable arm: the earlier arms already cover the whole scrutinee"); FuseExpect::None);

// A literal at a union position decides the undecided member beside the
// one that holds its type.
run!(tag_at_a_union_position_deciding_an_undecided_member_refused, r#"{
    let f = |b: bool, x| {
        let y = select b { true => x, false => `J };
        select (y, 0) { (`J, _) => 0, (v, _) => 1 }
    };
    f(true, `M)
}"#, refused("(`J, _) tests a value of 'x, a type not decided at this select"); FuseExpect::None);

run!(tag_at_a_union_position_over_an_annotated_member, r#"{
    let f = |b: bool, x| {
        let y: [`J, `M] = select b { true => x, false => `J };
        select (y, 0) { (`J, _) => 0, (v, _) => 1 }
    };
    [f(true, `M), f(false, `M)]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [1, 0]); FuseExpect::Jit);

run!(null_at_a_union_position_deciding_an_undecided_member_refused, r#"{
    let f = |b: bool, x| {
        let y = select b { true => x, false => null };
        select (y, 0) { (null, _) => 0, (v, _) => 1 }
    };
    f(true, `M)
}"#, refused("(null, _) tests a value of 'x, a type not decided at this select"); FuseExpect::None);

// A destructure that takes the member beside an undecided one leaves it
// undecided: refused for it, also where a later arm is refused first.
run!(destructure_beside_an_undecided_member_before_a_dead_arm_refused, r#"{
    let f = |x, c| {
        let y = select c { true => x, false => (1, 2) };
        select (y, 0) { ((a, b), _) => a + b, _ => 0 }
    };
    f((5, 1), true)
}"#, refused("((a, b), _) tests a value of 'x, a type not decided at this select"); FuseExpect::None);

run!(struct_beside_an_undecided_member_before_a_dead_arm_refused, r#"{
    let f = |x, c| {
        let y = select c { true => x, false => {a: 1, b: 2} };
        select (y, 0) { ({a, b}, _) => a + b, _ => 0 }
    };
    f({a: 5, b: 1}, true)
}"#, refused("({ a, b }, _) tests a value of 'x, a type not decided at this select"); FuseExpect::None);

run!(whole_holder_destructured_before_a_dead_arm_refused, r#"{
    let f = |x, c| {
        let y = select c { true => x, false => (1, 2) };
        select y { (a, b) => a + b, _ => 0 }
    };
    f((5, 1), true)
}"#, refused("(a, b) tests a value of 'x, a type not decided at this select"); FuseExpect::None);

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
}"#, refused("cannot compute i64 + string"); FuseExpect::None);

run!(a_guard_error_after_a_literal_arm_is_the_error, r#"{
    let f = |z, c| {
        let y = select c { true => z, false => 1 };
        select y { 3 => 0, i64 as n if n + "x" == 2 => n, i64 as n => n, s => str::len(s) }
    };
    f("ab", true)
}"#, refused("cannot compute i64 + string"); FuseExpect::None);

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
run!(abstract_payload_beside_an_undecided_member_refused, r#"{
    type W = Abstract<i64>;
    let f = |x, c| {
        let y = select c { true => x, false => W(1) };
        select (y, 0) { (W(a), _) => a, _ => 0 }
    };
    f(W(5), true)
}"#, refused("(W(a), _) tests a value of 'x, a type not decided at this select"); FuseExpect::None);

run!(abstract_payload_beside_an_annotated_member, r#"{
    type W = Abstract<i64>;
    let f = |x: [W, null], c| {
        let y = select c { true => x, false => W(1) };
        select (y, 0) { (W(a), _) => a, _ => 0 }
    };
    [f(W(5), true), f(null, true), f(null, false)]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [5, 0, 1]); FuseExpect::Jit);

run!(destructure_ladder_beside_an_undecided_member_refused, r#"{
    let f = |x, c| {
        let y = select c { true => x, false => (1, 2) };
        select (y, 0) { ((a, b), 0) => a + b, ((a, b), _) => a - b, _ => 0 }
    };
    f((5, 1), true)
}"#, refused("((a, b), 0) tests a value of 'x, a type not decided at this select"); FuseExpect::None);

run!(destructure_ladder_beside_an_annotated_member, r#"{
    let f = |x: [(i64, i64), null], c| {
        let y = select c { true => x, false => (1, 2) };
        select (y, 0) { ((a, b), 0) => a + b, ((a, b), _) => a - b, _ => 0 }
    };
    [f((5, 1), true), f(null, true), f(null, false)]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [6, 0, 3]); FuseExpect::Jit);

run!(slice_with_a_rest_beside_an_undecided_member_refused, r#"{
    let f = |x, c| {
        let y = select c { true => x, false => [1, 2] };
        select (y, 0) { ([a, ..], _) => a, _ => 0 }
    };
    f([5], true)
}"#, refused("([a, ..], _) tests a value of 'x, a type not decided at this select"); FuseExpect::None);

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
}"#, refused("this pattern tests for ("); FuseExpect::None);

run!(destructure_under_an_annotated_slice_element, r#"{
    let f = |x: Array<(i64, i64)>| select (x, 0) { ([(a, b), ..], _) => a, _ => 0 };
    [f([(1, 2)]), f([])]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [1, 0]); FuseExpect::Jit);

const PART_OF_X: &str =
    "tests a value whose type is a part of 'x, a type not decided at this select";

run!(undecided_index_part_names_its_binding, r#"{
    let f = |x| select (x[0]$, 0) { (`J, _) => 1, _ => 2 };
    f([`M])
}"#, refused(PART_OF_X); FuseExpect::None);

run!(undecided_index_part_annotated, r#"{
    let f = |x: Array<[`J, `M]>| select (x[0]$, 0) { (`J, _) => 1, _ => 2 };
    [f([`M]), f([`J])]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [2, 1]); FuseExpect::Jit);

run!(undecided_nested_index_part_names_its_binding, r#"{
    let f = |x| select ((x[0]$)[0]$, 0) { (`J, _) => 1, _ => 2 };
    f([[`M]])
}"#, refused(PART_OF_X); FuseExpect::None);

run!(undecided_part_names_no_function_holding_it, r#"{
    let s = {g: |x| select (x[0]$, 0) { (`J, _) => 1, _ => 2 }};
    s.g([`M])
}"#, refused(PART_OF_X); FuseExpect::None);

const BUILT_HERE: &str = "tests a value whose type is not decided at this select, which a \
     later use could decide apart from what the arms test: annotate the scrutinee's type \
     where it is built";

run!(undecided_abstract_payload_names_no_argument, r#"{
    type W<'a> = Abstract<'a>;
    let f = |x| select (W(x), 0) { (W(`J), _) => 1, _ => 2 };
    f(`M)
}"#, refused(BUILT_HERE); FuseExpect::None);

run!(undecided_abstract_payload_over_an_annotated_argument, r#"{
    type W<'a> = Abstract<'a>;
    let f = |x: [`J, `M]| select (W(x), 0) { (W(`J), _) => 1, _ => 2 };
    f(`M)
}"#, refused(BUILT_HERE); FuseExpect::None);

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
}"#, refused(PART_OF_X); FuseExpect::None);

run!(undecided_part_of_a_destructured_binding_annotated, r#"{
    let f = |x: ([`J, `M], i64)| select (x, 0) { ((a, b), _) => select (a, 0) { (`J, _) => 1, _ => 0 } };
    [f((`J, 1)), f((`M, 1))]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [1, 0]); FuseExpect::Jit);

run!(undecided_part_reached_through_a_pattern_alias, r#"{
    let f = |x| select (x, 0) { (c@ (a, b), _) => select c { (1, d) => d, _ => 0 } };
    f((5, 1))
}"#, refused(PART_OF_X); FuseExpect::None);

run!(undecided_part_through_an_intermediate_binding, r#"{
    let f = |x, c| {
        let y = select c { true => x[0]$, false => (1, 2) };
        select (y, 0) { ((a, b), 0) => a + b, ((a, b), _) => a - b, _ => 0 }
    };
    f([(5, 1)], true)
}"#, refused(PART_OF_X); FuseExpect::None);

// A slice over a scrutinee the arms decide leaves its rest open, and its
// run-time test passes whatever the rest becomes at every position: a
// member written after the select that an element's structure takes is
// one the element's binds were never typed over.
const WRITTEN_LATER: &str = "too, a type its scrutinee was given after this select";

run!(slice_element_binds_a_member_written_later_refused, r#"{
    let v = never();
    let z = select v { [`J(n), ..] => n + 1, _ => 3 };
    v <- [`J("s"), `K];
    z
}"#, refused("this pattern takes a value of `J(string) too, a type its scrutinee was given after this select typed the pattern's binds for `J(i64): annotate the scrutinee's type where it is built"); FuseExpect::None);

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
}"#, refused("this pattern takes a value of `J([i64, string]) too"); FuseExpect::None);

run!(slice_element_binds_a_tuple_written_later_refused, r#"{
    let v = never();
    let z = select v { [(n, m), ..] => n + 1, _ => 3 };
    v <- [("s", 1), (1, 1)];
    z
}"#, refused(WRITTEN_LATER); FuseExpect::None);

run!(slice_element_binds_a_struct_written_later_refused, r#"{
    let v = never();
    let z = select v { [{a}, ..] => a + 1, _ => 3 };
    v <- [{a: "s"}, {a: 1}];
    z
}"#, refused(WRITTEN_LATER); FuseExpect::None);

run!(list_element_binds_a_member_written_later_refused, r#"{
    let v = never();
    let z = select v { [<`J(n), ..>] => n + 1, _ => 3 };
    v <- [<`J("s"), 0>];
    z
}"#, refused(WRITTEN_LATER); FuseExpect::None);

run!(slice_suffix_element_binds_a_member_written_later_refused, r#"{
    let v = never();
    let z = select v { [.., `J(n)] => n + 1, _ => 3 };
    v <- [`K, `J("s")];
    z
}"#, refused(WRITTEN_LATER); FuseExpect::None);

run!(slice_element_beside_a_named_rest_binds_a_member_written_later_refused, r#"{
    let v = never();
    let z = select v { [`J(n), t..] => n + 1, _ => 3 };
    v <- [`J("s"), `K];
    z
}"#, refused(WRITTEN_LATER); FuseExpect::None);

run!(slice_element_capture_binds_a_member_written_later_refused, r#"{
    let v = never();
    let z = select v { [x@ `J(n), ..] => n + 1, _ => 3 };
    v <- [`J("s"), `K];
    z
}"#, refused(WRITTEN_LATER); FuseExpect::None);

run!(slice_element_under_a_tuple_binds_a_member_written_later_refused, r#"{
    let v = never();
    let z = select v { (1, [`J(n), ..]) => n + 1, _ => 3 };
    v <- (1, [`J("s"), `K]);
    z
}"#, refused(WRITTEN_LATER); FuseExpect::None);

// A literal the element tests takes only the members that may hold it.
run!(slice_element_beside_a_literal_binds_a_member_written_later_refused, r#"{
    let v = never();
    let z = select v { [(n, 1), ..] => n + 1, _ => 3 };
    v <- [("s", 1), (1, 1)];
    z
}"#, refused("this pattern takes a value of ([i64, string], i64) too"); FuseExpect::None);

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
}"#, refused("this pattern takes a value of Any too"); FuseExpect::None);

// An abstract pattern takes every instance of its type: only the slice's
// type test reads the parameters.
run!(slice_element_binds_an_abstract_instance_written_later_refused, r#"{
    type W<'a> = Abstract<'a>;
    let v = never();
    let z = select v { [W(p), ..] => p + 1, _ => 3 };
    v <- [W("s"), W(1)];
    z
}"#, refused("this pattern takes a value of W<string> too"); FuseExpect::None);

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
}"#, refused(WRITTEN_LATER); FuseExpect::None);

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
}"#, refused(WRITTEN_LATER); FuseExpect::None);

// An element that only takes apart a type written later is typed by its
// body, and judged the same way.
run!(slice_element_takes_apart_an_element_written_later_refused, r#"{
    let e = never();
    let v = [e];
    let z = select v { [(a, b), ..] => a + 1, _ => 0 };
    e <- ("s", 1);
    z
}"#, refused("this pattern takes a value of (string, i64) too"); FuseExpect::None);

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
}"#, refused("this pattern takes a value of `J(string) too"); FuseExpect::None);

run!(slice_element_binds_a_bounded_variable_beside_a_member_refused, r#"{
    let f = 'a: [(string, i64), `K] |x: 'a| {
        let v = never();
        let z = select v { [(n, m), ..] => n + 1, _ => 3 };
        v <- [x, (1, 1)];
        z
    };
    f(("s", 1))
}"#, refused("this pattern takes a value of (string, i64) too"); FuseExpect::None);

run!(slice_element_beside_a_bounded_variable_it_does_not_take, r#"{
    let f = 'a: [`L(string), `K] |x: Array<'a>| {
        let v = never();
        let z = select v { [`J(n), ..] => n + 1, _ => 3 };
        v <- x;
        z
    };
    f([`L("s")])
}"#, |v: Result<&Value>| matches!(v, Ok(Value::I64(3))); FuseExpect::Jit);

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
}"#, refused("this pattern takes a value of `J(string) too"); FuseExpect::None);

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
}"#, refused("becomes that has its structure, and this select typed the pattern's binds for W<i64> alone"); FuseExpect::None);

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

// With a `_` among its elements the slice tests none of them: the bind
// is `Any` where it is typed, whatever is written later.
run!(slice_element_beside_a_wildcard_over_a_let_the_arms_decide_refused, r#"{
    let v = never();
    let z = select v { [`J(n), _] => n + 1, _ => 3 };
    v <- [`J("s"), 0];
    z
}"#, refused("cannot compute Any + i64"); FuseExpect::None);

// A `_` at a part of a tuple, struct or variant reads `Any` in a scrutinee
// the arms decide: it takes the values the other arms' tests of that part
// leave, of whatever type.
run!(wildcard_beside_a_shape_under_another_tag, r#"{
    let f = |s| select s { (`Key, `Char(c)) => c + 1, (`Mouse, _) => 0 };
    [f((`Key, `Char(7))), f((`Mouse, "x"))]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [8, 0]); FuseExpect::Jit);

run!(wildcard_beside_a_shape_under_the_same_tag, r#"{
    let f = |s| select s { (`Key, `Char(c)) => c + 1, (`Key, _) => 0 };
    [f((`Key, `Char(7))), f((`Key, `Enter)), f((`Key, `Char("s")))]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [8, 0, 0]); FuseExpect::Jit);

run!(wildcard_beside_a_shape_in_a_struct, r#"{
    let f = |s| select s { {k: `Key, v: `Char(c)} => c + 1, {k: `Mouse, v: _} => 0 };
    [f({k: `Key, v: `Char(7)}), f({k: `Mouse, v: "x"})]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [8, 0]); FuseExpect::Jit);

run!(wildcard_beside_a_shape_in_a_variants_payload, r#"{
    let f = |s| select s { `S(`C(1)) => 1, `S((2, _)) => 2, `S(_) => 0 };
    [f(`S(`C(1))), f(`S((2, "x"))), f(`S(7)), f(`S(`C("s")))]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [1, 2, 0, 0]); FuseExpect::None);

run!(wildcard_beside_a_literal_of_another_type, r#"{
    let f = |s| select s { (1, _) => 0, (_, "a") => 1, (_, _) => 2 };
    [f((2, 5)), f((2, "a")), f((1, null))]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [2, 1, 0]); FuseExpect::None);

run!(wildcard_arm_behind_a_null_test_is_live, r#"{
    let f = |s| select s { (1, _) => 0, (_, null) => 1, (_, _) => 2 };
    [f((1, null)), f((2, null)), f((2, 5))]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [0, 1, 2]); FuseExpect::None);

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
}"#, |v: Result<&Value>| matches!(v, Ok(Value::I64(0))); FuseExpect::None);

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
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [10, 10, 6, 4, 99]); FuseExpect::None);

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

run!(bind_beside_arms_that_take_the_part_apart_in_one_instance_at_two_types_refused, r#"{
    let mk = |z| |s| select s { (1, b) => 10, (2, `A(x)) => x, (3, `B(y)) => str::len(y), (_, b) => 99 };
    let h = mk(0);
    (h((1, `A(5))), h((2, `A("s"))))
}"#, refused("does not contain (i64, `A(string))"); FuseExpect::None);

run!(bind_beside_arms_that_take_the_part_apart_over_a_let_written_later, r#"{
    let v = never();
    let r = select v { (1, b) => 10, (2, `A(x)) => x, (3, `B(y)) => str::len(y), (_, b) => 99 };
    v <- (1, `A(5));
    r
}"#, |v: Result<&Value>| matches!(v, Ok(Value::I64(10))); FuseExpect::Jit);

run!(bind_beside_arms_that_take_the_part_apart_used_as_another_type_refused, r#"{
    let f = |s| select s { (1, b) => 10, (2, `A(x)) => x, (3, `B(y)) => str::len(y), (_, b) => 99 };
    f((3, `B(4)))
}"#, refused("does not contain (i64, `B(i64))"); FuseExpect::None);

// The member that holds the others takes an argument only where that
// leaves nothing for another to decide: a `_` holds any value, and what
// `c` reads would stay open.
run!(wildcard_member_holding_a_bind_inside_takes_no_argument, r#"{
    let f = |x| select x { (1, _, z) => z, (2, (c, _), z) => c, (_, _, z) => z };
    f((2, ("s", 4), 0))
}"#, refused("could go to (i64, Any, '_"); FuseExpect::None);

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
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [99, 4, 10]); FuseExpect::None);

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
run!(bind_beside_arms_that_take_the_part_apart_takes_no_undecided_argument, r#"{
    let f = |s| select s { (1, b) => 10, (2, (x, y)) => x, (3, (x, y, z)) => x, (_, b) => 99 };
    let g = |u| f((2, u));
    g((5, 6))
}"#, refused("could go to (i64, [("); FuseExpect::None);

run!(bind_beside_arms_that_take_the_part_apart_takes_an_annotated_argument, r#"{
    let f = |s| select s { (1, b) => 10, (2, (x, y)) => x, (3, (x, y, z)) => x, (_, b) => 99 };
    let g = |u: (i64, i64)| f((2, u));
    g((5, 6))
}"#, |v: Result<&Value>| matches!(v, Ok(Value::I64(5))); FuseExpect::None);

// A union built after an arm's pattern decided a cell (the select's own
// result, an arm's body, a guard) may drop the member that held the cell
// from the scrutinee's union: the arms are still judged over the union as
// it stood before them.
run!(test_deciding_a_member_the_result_union_drops_refused, r#"{
    let z = never();
    let k: [i64, null] = 1;
    let u = select k { i64 as n => {a: n}, _ => {a: z} };
    let r = select u { {a: 7} => u, v => u };
    r.a
}"#, refused("{ a: 7 } tests a value of 'z, a type not decided at this select"); FuseExpect::None);

run!(test_deciding_a_member_the_result_union_keeps_refused, r#"{
    let z = never();
    let k: [i64, null] = 1;
    let u = select k { i64 as n => {a: n}, _ => {a: z} };
    let r = select u { {a: 7} => 1, v => u.a };
    r
}"#, refused("{ a: 7 } tests a value of 'z, a type not decided at this select"); FuseExpect::None);

run!(test_over_an_annotated_member_the_result_union_drops, r#"{
    let z: i64 = never();
    let k: [i64, null] = 1;
    let u = select k { i64 as n => {a: n}, _ => {a: z} };
    let r = select u { {a: 7} => u, v => u };
    r.a
}"#, |v: Result<&Value>| matches!(v, Ok(Value::I64(1))); FuseExpect::Jit);

run!(tuple_test_deciding_a_member_the_result_union_drops_refused, r#"{
    let z = never();
    let k: [i64, null] = 1;
    let u = select k { i64 as n => (n, 0), _ => (z, 0) };
    let r = select u { (7, _) => u, v => u };
    r.0
}"#, refused("(7, _) tests a value of 'z, a type not decided at this select"); FuseExpect::None);

run!(test_deciding_a_parameter_the_result_union_drops_refused, r#"{
    let f = |z| {
        let k: [i64, null] = null;
        let u = select k { i64 as n => {a: n}, _ => {a: z} };
        select u { {a: 7} => u, v => u }
    };
    f(3).a
}"#, refused("{ a: 7 } tests a value of 'z, a type not decided at this select"); FuseExpect::None);

run!(test_over_an_annotated_parameter_the_result_union_drops, r#"{
    let f = |z: i64| {
        let k: [i64, null] = null;
        let u = select k { i64 as n => {a: n}, _ => {a: z} };
        select u { {a: 7} => u, v => u }
    };
    f(3).a
}"#, |v: Result<&Value>| matches!(v, Ok(Value::I64(3))); FuseExpect::Jit);

run!(test_deciding_a_member_an_arm_body_drops_refused, r#"{
    let z = never();
    let k: [i64, null] = 1;
    let u = select k { i64 as n => {a: n}, _ => {a: z} };
    select u {
        {a: 7} => { let w = select k { null as _ => u, _ => u }; w.a },
        {a: 8} => 2,
        v => 0
    }
}"#, refused("{ a: 7 } tests a value of 'z, a type not decided at this select"); FuseExpect::None);

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

run!(test_deciding_a_member_a_guard_drops_refused, r#"{
    let z = never();
    let k: [i64, null] = 1;
    let u = select k { i64 as n => {a: n}, _ => {a: z} };
    select u { {a: 7} if (select k { null as _ => u, _ => u }).a == 7 => 1, v => 0 }
}"#, refused("{ a: 7 } tests a value of 'z, a type not decided at this select"); FuseExpect::None);

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
}"#, refused("{ a: 7 } tests a value of 'z, a type not decided at this select"); FuseExpect::None);

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
run!(written_test_deciding_a_member_the_result_union_drops_refused, r#"{
    let z = never();
    let k: [i64, null] = 1;
    let u = select k { i64 as n => {a: n}, _ => {a: z} };
    let r = select u { {a: i64} as {a: 7} => u, v => u };
    r.a
}"#, refused("{ a: i64 } as { a: 7 } tests a value of 'z, a type not decided at this select"); FuseExpect::None);

run!(written_test_over_an_annotated_member_the_result_union_drops, r#"{
    let z: i64 = never();
    let k: [i64, null] = 1;
    let u = select k { i64 as n => {a: n}, _ => {a: z} };
    let r = select u { {a: i64} as {a: 7} => u, v => u };
    r.a
}"#, |v: Result<&Value>| matches!(v, Ok(Value::I64(1))); FuseExpect::Jit);

run!(or_test_deciding_a_member_the_result_union_drops_refused, r#"{
    let z = never();
    let k: [i64, null] = 1;
    let u = select k { i64 as n => {a: n}, _ => {a: z} };
    let r = select u { {a: 7} | {a: 8} => u, v => u };
    r.a
}"#, refused("{ a: 7 } | { a: 8 } tests a value of 'z, a type not decided at this select"); FuseExpect::None);

run!(test_deciding_a_member_a_later_arm_body_drops_refused, r#"{
    let z = never();
    let k: [i64, null] = 1;
    let u = select k { i64 as n => {a: n}, _ => {a: z} };
    select u {
        {a: 7} => 0,
        v => { let w = select k { null as _ => u, _ => u }; w.a }
    }
}"#, refused("{ a: 7 } tests a value of 'z, a type not decided at this select"); FuseExpect::None);

run!(test_deciding_a_member_a_function_in_its_arm_drops_refused, r#"{
    let z = never();
    let k: [i64, null] = 1;
    let u = select k { i64 as n => {a: n}, _ => {a: z} };
    select u {
        {a: 7} => { let g = || select k { null as _ => u, _ => u }; g().a },
        v => 0
    }
}"#, refused("{ a: 7 } tests a value of 'z, a type not decided at this select"); FuseExpect::None);

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
