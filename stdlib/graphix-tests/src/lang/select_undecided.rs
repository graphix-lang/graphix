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

// A guarded arm takes nothing: the arm after it meets the member, and decides it.
run!(undecided_member_beside_a_guarded_tag_decided, r#"{
    let b = false;
    let g = true;
    let z = never();
    let v = select b { true => (`K, {a: 1, c: 1}), false => (`J, z) };
    let r = select v { (`J, _) if g => "j", (_, {a, ..}) => "[a]" };
    z <- {a: "s", c: "w"};
    r
}"#, refused("z is { a: string, c: i64 }, inferred from an earlier use"); FuseExpect::None);

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
    let r = select v { `P({a, ..}) => "[a]", _ => "other" };
    x <- 1;
    y <- 2;
    r
}"#, refused(TOLD_APART); FuseExpect::None);

run!(annotated_payload_members_reach_the_arm, r#"{
    let b = false;
    let x: i64 = never();
    let y: i64 = never();
    let v = select b { true => `P({a: [1], c: x}), false => `P({a: ["s"], c: y}) };
    let r = select v { `P({a, ..}) => "[a]", _ => "other" };
    x <- 1;
    y <- 2;
    r
}"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == r#"["s"]"#); FuseExpect::Jit);

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

run!(name_over_an_undecided_slice_part_written_apart_refused, r#"{
    let b = true;
    let z = never();
    let v = select b { true => (`A, [1]), false => (`B, z) };
    let r = select v { (_, [x, ..]) => x + 1, (_, _) => 0 };
    z <- ["s"];
    r
}"#, refused("z is Array<i64>, inferred from an earlier use"); FuseExpect::None);

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

run!(written_union_test_of_two_annotated_parts, r#"{
    let b = false;
    let w: [i64, string] = never();
    let z: [i64, string] = never();
    let v = select b { true => (`K, 1, 1), false => (`J, w, z) };
    let r = select v { [(`J, Any, i64), (`J, i64, Any)] as (`J, x, y) => "j", (_, a, c) => "n" };
    w <- "s";
    z <- "t";
    r
}"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "n"));

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

run!(widened_open_element_refused, r#"{
    let f = |v| select (v, 0) { ([x, ..], _) => str::len(x), ([1, ..], _) => 5, (_, _) => 0 };
    [f(["ab"]), f([1])]
}"#, refused("string does not contain [i64"); FuseExpect::None);

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
