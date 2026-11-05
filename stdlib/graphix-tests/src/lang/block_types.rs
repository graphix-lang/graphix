// Type names a block declares, read after the block: an instance's body,
// a labeled default compiled at a call, a value's type at run time.

use graphix_package_core::{
    run,
    testing::{FuseExpect, prints, refused},
};

// An instance's body names its block's type over an outer one of the
// same name, wherever the outer one is declared.
run!(
    block_type_over_an_outer_one_in_an_instance,
    r#"{
    type L = string;
    let g = |z: i64| {
        type L = [i64, null];
        let f = |x: i64| { let b: L = 3; (b, x) };
        f(z)
    };
    g(1)
}"#,
    prints("[i64:3, i64:1]")
);

run!(
    block_type_over_an_outer_one_declared_later,
    r#"{
    let g = |z: i64| {
        type L = [i64, null];
        let f = |x: i64| { let b: L = 3; (b, x) };
        f(z)
    };
    type L = string;
    g(1)
}"#,
    prints("[i64:3, i64:1]")
);

run!(
    body_type_over_an_outer_one_its_parameter_names,
    r#"{
    type M = string;
    let f = |x: M| { type M = [i64, null]; let q: M = 3; (q, x) };
    f("a")
}"#,
    prints(r#"[i64:3, "a"]"#)
);

// The block's type reaches the instance only through a declared
// variable's bound and the argument's type.
run!(
    block_type_reaches_an_instance_through_a_bound,
    r#"{
    type L = string;
    let top = |w: i64| {
        type L = [i64, null];
        let outer = 'a: L |x: 'a| -> i64 select x { i64 as n => n + 1, L as null => 0 };
        let v: L = w;
        (outer(w), outer(v), outer(null))
    };
    top(5)
}"#,
    prints("[i64:6, i64:6, i64:0]")
);

run!(
    block_type_over_an_imported_one,
    prints("[i64:3, i64:1]"),
    "/test.gx" => r#"
mod m;
use m::L;
let g = |z: i64| {
    type L = [i64, null];
    let f = |x: i64| { let b: L = 3; (b, x) };
    f(z)
};
let result = g(1)
"#,
    "/test/m.gx" => "type L = string"
);

// A name a block declares means its type throughout the block, in a
// function written above the declaration as in a later instance of it.
run!(
    block_type_named_above_its_declaration_in_a_function,
    r#"{
    type L = i64;
    let g = |z: i64| {
        let f = |x: i64| { let b: L = 3; (b, x) };
        type L = string;
        f(z)
    };
    g(1)
}"#,
    refused("type mismatch L does not contain i64");
    FuseExpect::None
);

run!(
    block_type_named_above_its_declaration_in_a_function_fits,
    r#"{
    type L = i64;
    let g = |z: i64| {
        let f = |x: i64| { let b: L = "s"; (b, x) };
        type L = string;
        f(z)
    };
    g(1)
}"#,
    prints(r#"["s", i64:1]"#)
);

run!(
    block_type_named_above_its_declaration_in_a_block,
    r#"{
    type L = i64;
    let r = {
        let f = |x: i64| { let b: L = 3; (b, x) };
        type L = string;
        f(1)
    };
    r
}"#,
    refused("type mismatch L does not contain i64");
    FuseExpect::None
);

run!(
    block_type_named_above_its_declaration_over_an_outer_one,
    r#"{
    type Foo = string;
    let r = { let f = |x: i64| { let a: Foo = 42; a + x }; type Foo = i64; f(1) };
    r + 1
}"#,
    prints("i64:44")
);

run!(
    block_type_named_above_its_declaration_alone,
    r#"{
    let r = { let f = |x: i64| { let a: Foo = 42; a + x }; type Foo = i64; f(1) };
    r + 1
}"#,
    prints("i64:44")
);

// An abstract constructor, pattern and payload read above a block's
// typedef name the block's type, as an annotation there does, never an
// outer type of the name.
run!(
    block_abstract_constructed_above_its_declaration,
    r#"{
    type W = Abstract<string>;
    let g = |z: i64| {
        let mk = |v: i64| W(v);
        let isw = |x: Any| select x { W as _ => true, _ => false };
        type W = Abstract<i64>;
        let a: W = mk(z);
        (a.0, isw(mk(z)), isw(W(z)))
    };
    g(1)
}"#,
    prints("[i64:1, true, true]")
);

run!(
    block_abstract_constructed_above_its_declaration_over_an_outer_one,
    r#"{
    type W = Abstract<string>;
    let g = |z: i64| {
        let mk = |v: string| W(v);
        type W = Abstract<i64>;
        mk("a")
    };
    g(1)
}"#,
    refused("type mismatch i64 does not contain string");
    FuseExpect::None
);

run!(
    block_abstract_pattern_above_its_declaration,
    r#"{
    type W = Abstract<i64>;
    let g = |z: i64| {
        let un = |w: W| select w { W(n) => n };
        let rd = |w: W| w.0 + z;
        type W = Abstract<i64>;
        (un(W(z)), rd(W(5)))
    };
    g(1)
}"#,
    prints("[i64:1, i64:6]")
);

// A default's block type reaches a call through a field, which binds the
// callee at run time.
run!(
    default_block_type_through_a_field,
    r#"{
    let f = |#d = {
        type L = [`Nil, `Cons(i64, L)];
        let l: L = `Cons(1, `Nil);
        l
    }, x: i64| (d, x);
    let s = {g: f};
    let t: {g: fn(x: i64) -> 'r} = s;
    let p = t.g(1);
    (p, p == p)
}"#,
    prints(r#"[[["Cons", i64:1, "Nil"], i64:1], true]"#)
);

// Data reads into the type a default's block declares, never into an
// outer type of that name.
run!(
    json_reads_a_defaults_block_type,
    r##"{
    type L = [`X(i64)];
    let out = never();
    let g = |z: i64| {
        type L = [`Nil, `Cons(i64, L)];
        let f = |#d = { let b: [`Y, `Z(L)] = never(); out <- b; 0 }, x: i64| d + x;
        f(z)
    };
    let r1 = json::read(r#"["Z",["Cons",1,"Nil"]]"#);
    let r2 = json::read(r#"["Z",["X",1]]"#);
    out <- r1$;
    out <- r2$;
    (g(1), r1, r2)
}"##,
    prints(
        r#"[i64:1, ["Z", ["Cons", i64:1, "Nil"]], error:["InvalidCast", "can't cast [\"X\", 1] to [`Cons(i64, L), `Nil]: no member admits it"]]"#
    )
);

// A default naming a type, its block's or a top-level one, takes its
// definition's types at a bind: at `` `B `` the tuple arm reaches nothing
// and never runs.
run!(
    default_naming_a_block_type_takes_its_definitions_types,
    r#"{
    type P = (i64, i64);
    let ot = 'a: [(i64, i64), `B] |x: 'a| -> i64 {
        type Q = (i64, i64);
        let f = |#d = select x {
            (i64, i64) as t => { let k: P = (1, 2); let j: Q = (3, 4); t.0 + k.0 + j.0 },
            `B as _ => 0
        }, y: i64| d + y;
        f(1)
    };
    (ot((4, 0)), ot(`B))
}"#,
    prints("[i64:9, i64:1]")
);

// A recursive typedef applied inside a parameter written inline compares
// as the regular tree it names, as it does written through an alias, and
// keys a map once.
run!(
    abstract_inline_recursive_parameters_compare,
    r#"{
    type B<'x> = Abstract<i64>;
    type R<'a> = [`Nil, `Cons('a, R<'a>)];
    let c: B<R<i64>> = B(1);
    let e: B<[`Nil, `Cons(i64, R<i64>)]> = B(1);
    let xs: Array<Any> = [c, e];
    let m: Map<Any, i64> = {};
    let n = map::insert(map::insert(m, xs[0]$, 1), xs[1]$, 2);
    (c == e, xs[0]$ == xs[1]$, map::len(n))
}"#,
    prints("[true, true, i64:1]")
);

// A parameter a type test or a write decides, written inline, names the
// typedef its scope means there, as an annotation does: equal types
// compare equal, and a block's own `R` is another type.
run!(
    abstract_parameters_a_type_test_or_a_write_decides_compare,
    r#"{
    type B<'x> = Abstract<i64>;
    type R<'a> = [`Nil, `Cons('a, R<'a>)];
    let c: B<R<i64>> = B(1);
    let t = B(1);
    let q = select t { B<[`Nil, `Cons(i64, R<i64>)]> as _ => 1 };
    let v = B(1);
    v <- never<B<[`Nil, `Cons(i64, R<i64>)]>>();
    let o = {
        type R<'a> = [`X('a)];
        let b = B(1);
        let p = select b { B<[`Nil, `Cons(i64, R<i64>)]> as _ => 2 };
        b
    };
    let xs: Array<Any> = [c, t, v, o];
    let m: Map<Any, i64> = {};
    let n = array::fold(xs, m, |acc, x| map::insert(acc, x, 1));
    (array::map(xs, |x| x == xs[0]$), xs[1]$ == xs[3]$, map::len(n), q)
}"#,
    prints("[[true, true, true, false], false, i64:2, i64:1]")
);

// The same in a module's body, whose private typedef only its own env
// resolves.
run!(
    abstract_parameter_a_module_type_test_decides_compares,
    prints("[true, i64:1]"),
    "/test.gx" => r#"
mod m;
let result = (m::xs[0]$ == m::xs[1]$, m::q)
"#,
    "/test/m.gxi" => r#"
type B<'x>;
val q: i64;
val xs: Array<Any>;
"#,
    "/test/m.gx" => r#"
type B<'x> = Abstract<i64>;
type P<'a> = [`Nil, `Cons('a, P<'a>)];
let c: B<P<i64>> = B(1);
let t = B(1);
let q = select t { B<[`Nil, `Cons(i64, P<i64>)]> as _ => 1 };
let xs: Array<Any> = [c, t]
"#
);

// The same in a labeled default's block: `t` names the default's `R`,
// never the outer one, and `t2` the default's `Q`, whether a call omits
// the default or none does.
run!(
    abstract_parameter_a_default_type_test_decides_compares,
    r#"{
    type B<'x> = Abstract<i64>;
    type R<'a> = [`X('a)];
    type R2<'a> = [`Nil, `Cons('a, R2<'a>)];
    let t = B(1);
    let g = |#d = {
        type R<'a> = [`Nil, `Cons('a, R<'a>)];
        select t { B<[`Nil, `Cons(i64, R<i64>)]> as _ => 0 }
    }, x: i64| d + x;
    let t2 = B(1);
    let h = |#d = {
        type Q<'a> = [`Nil, `Cons('a, Q<'a>)];
        select t2 { B<[`Nil, `Cons(i64, Q<i64>)]> as _ => 0 }
    }, x: i64| d + x;
    let good: B<R2<i64>> = B(1);
    let wrong: B<[`Nil, `Cons(i64, [`X(i64)])]> = B(1);
    let xs: Array<Any> = [good, t, t2, wrong];
    let m: Map<Any, i64> = {};
    let n = array::fold(xs, m, |acc, x| map::insert(acc, x, 1));
    (array::map(xs, |x| x == xs[0]$), map::len(n), g(1))
}"#,
    prints("[[true, true, true, false], i64:2, i64:1]")
);

// A dynamic module's new source is a new source: a block it declares at
// the place the old source declared another is its own definition, even
// while the old body still runs.
run!(
    a_reload_declares_its_own_block_type,
    r#"{
    let k = 0;
    k <- select k { 0 => 1, _ => never() };
    let status = mod foo dynamic {
        sandbox whitelist [core];
        sig { val y: string };
        source select k {
            0 => "let y = { type L = i64; let v: L = 1; \"a\" }",
            _ => "let y = { type L = string; let v: L = \"b\"; v }"
        }
    };
    let r = select status { error as e => "[e]", null as _ => foo::y };
    filter(r, |s| s != "a")
}"#,
    prints(r#""b""#)
);
