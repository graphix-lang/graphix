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

// A written test that holds part of a member pools as that part: its
// type's primitives at a position are heads of their own.
run!(written_test_beside_bool_head_covers, r#"{
    let u = |x: (bool, [i64, null])| select x {
        (true, _) => 0,
        (bool, i64) as (false, n) => n,
        (bool, null) as (false, _) => 1
    };
    [u((true, null)), u((false, 7)), u((false, null)), u((true, 3))]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [0, 7, 1, 0]); FuseExpect::None);

run!(written_tests_split_a_primitive_member, r#"{
    let u = |x: (bool, [i64, string, null])| select x {
        (true, _) => 0,
        (bool, i64) as (false, n) => n,
        (bool, string) as (false, s) => str::len(s),
        (false, null) => 3
    };
    [u((true, null)), u((false, 7)), u((false, "abcd")), u((false, null))]
}"#, |v: Result<&Value>| matches!(v, Ok(Value::Array(a)) if as_i64s(a) == [0, 7, 4, 3]); FuseExpect::Jit);

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
    let u = 'b: [i64, string] |x: (bool, ['b, null])| select x {
        (true, _) => 0,
        (bool, i64) as (false, n) => 1,
        (false, null) => 3
    };
    u((true, null))
}"#, refused("missing match cases"); FuseExpect::None);

run!(written_test_leaves_a_member_part_refused, r#"{
    let u = |x: (bool, [i64, string, null])| select x {
        (true, _) => 0,
        (bool, i64) as (false, n) => n,
        (false, null) => 3
    };
    u((false, "abcd"))
}"#, refused("missing match cases"); FuseExpect::None);

run!(written_test_after_bool_head_is_dead, r#"{
    let u = |x: (bool, [i64, null])| select x {
        (true, _) => 0,
        (bool, i64) as (true, n) => n,
        _ => 1
    };
    u((false, 7))
}"#, refused("every combination of heads it matches is covered by earlier arms"); FuseExpect::None);

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
