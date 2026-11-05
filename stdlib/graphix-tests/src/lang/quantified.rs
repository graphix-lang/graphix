// A function type with type variables of its own stands only as a
// definition's signature, and a function stays generic only where a
// binding names it (design/tvar_constraints.md, Generalization).

use anyhow::Result;
use graphix_package_core::{
    run,
    testing::{FuseExpect, is, prints, refused},
};
use netidx::publisher::Value;

const QUANTIFIED: &str = "stands only as a definition's signature";

macro_rules! refused_positions {
    ($($name:ident => $src:expr,)*) => {$(
        #[tokio::test(flavor = "current_thread")]
        async fn $name() -> Result<()> {
            super::refused_everywhere($src, QUANTIFIED).await
        }
    )*};
}

refused_positions! {
    quantified_formal_is_refused =>
        "{ let apply = |f: fn<'b: Number>(x: 'b) -> 'b| f(1); apply(|x| x) }",
    trait_parameter_in_a_formal_is_refused =>
        "{ let apply = |f: fn(x: Display) -> string| f(1); apply(|x| \"[x]\") }",
    ctor_quantifier_in_a_formal_is_refused =>
        "{ let apply = |f: fn<'c: Collection>(c: 'c) -> i64| 0; apply(|c| 0) }",
    quantified_field_is_refused =>
        "{ let s: {f: fn<'a: Any>(x: 'a) -> 'a} = {f: |x| x}; s.f(1) }",
    quantified_element_is_refused =>
        "{ let a: Array<fn<'a: Any>(x: 'a) -> 'a> = [|x| x]; 0 }",
    quantified_union_member_is_refused =>
        "{ let u: [fn<'a: Any>(x: 'a) -> 'a, null] = null; 0 }",
    quantified_let_annotation_is_refused =>
        "{ let f: fn<'a: Any>(x: 'a) -> 'a = |x| x; f(1) }",
    quantified_let_rec_annotation_is_refused =>
        "{ let rec f: fn<'a: Any>(x: 'a) -> 'a = |x| x; f(1) }",
    quantified_typedef_body_is_refused =>
        "{ type F = fn<'a: Any>(x: 'a) -> 'a; 0 }",
    trait_parameter_in_a_typedef_body_is_refused =>
        "{ type F = fn(x: Display) -> string; 0 }",
    quantified_argument_of_a_function_type_is_refused =>
        "{ let g = |h: fn(f: fn<'a: Any>(x: 'a) -> 'a) -> i64| 0; 0 }",
    quantified_result_of_a_function_type_is_refused =>
        "{ let g = |h: fn(z: i64) -> fn<'a: Any>(x: 'a) -> 'a| 0; 0 }",
    quantified_return_type_is_refused =>
        "{ let mk = |z: i64| -> fn<'a: Any>(x: 'a) -> 'a |x| x; 0 }",
    quantified_referent_is_refused =>
        "{ let g = |r: &fn<'a: Any>(x: 'a) -> 'a| 0; 0 }",
    quantified_abstract_representation_is_refused =>
        "{ type W = Abstract<fn<'a: Any>(x: 'a) -> 'a>; 0 }",
    quantified_abstract_parameter_is_refused =>
        "{ type W<'t> = Abstract<'t>; let g = |w: W<fn<'a: Any>(x: 'a) -> 'a>| 0; 0 }",
    quantified_variant_payload_is_refused =>
        "{ let g = |v: `F(fn<'a: Any>(x: 'a) -> 'a)| 0; 0 }",
    quantified_never_type_is_refused =>
        "{ let f = never<fn<'a: Any>(x: 'a) -> 'a>(); 0 }",
    quantified_bound_is_refused =>
        "{ let g = 'b: fn<'a: Any>(x: 'a) -> 'a |f: 'b| 0; 0 }",
    quantified_cast_target_is_refused =>
        "{ let v: Any = 1; cast<Array<fn<'a: Any>(x: 'a) -> 'a>>(v) }",
    quantified_type_test_is_refused =>
        "{ let v: [i64, string] = 1; select v { Array<fn<'a: Any>(x: 'a) -> 'a> as a => 0, _ => 1 } }",
    quantified_catch_annotation_is_refused =>
        "{ let r = never(); catch(e: Error<ErrChain<`F(fn<'a: Any>(x: 'a) -> 'a)>>) r <- e; 0 }",
}

// The kept forms: a definition declares the variables its parameters use,
// and a generic function is bound by a `let` and called by name.
run!(
    a_definitions_variable_types_a_function_parameter,
    "{ let apply = 'b: Number |f: fn(x: 'b) -> 'b, x: 'b| f(x); (apply(|x| x * 2, 3), apply(|x| x * 2.0, 1.5)) }",
    prints("[i64:6, f64:3.]");
    FuseExpect::Jit
);

run!(
    a_let_bound_generic_function_is_called_at_two_types,
    "{ let f = 'a: Any |x: 'a| -> 'a x; (f(1), f(\"s\")) }",
    prints(r#"[i64:1, "s"]"#);
    FuseExpect::Jit
);

run!(
    a_holder_of_lambdas_is_generic,
    "{ let s = {f: |x| x}; (s.f(1), s.f(\"s\")) }",
    prints(r#"[i64:1, "s"]"#);
    FuseExpect::None
);

run!(
    a_parameterized_typedef_of_a_function_type,
    "{ type F<'a> = fn(x: 'a) -> 'a; let apply = 'a: Any |f: F<'a>, x: 'a| f(x); (apply(|x| x, 1), apply(|x| x, \"s\")) }",
    prints(r#"[i64:1, "s"]"#);
    FuseExpect::None
);

run!(
    a_ctor_bound_on_a_definition,
    is(Value::I64(21)),
    "/test.gx" => r#"
        use Collection::*;
        let n = 'c: Collection |c: 'c| fold(c, 0, |acc, x| acc + 1);
        let result = n([1, 2]) * 10 + n([<"x">])
    "#
);

run!(
    an_interface_val_with_a_trait_parameter,
    is(Value::I64(21)),
    "/test.gx" => r#"
        mod m;
        let result = m::csize([1, 2]) * 10 + m::csize([<"x">])
    "#,
    "/test/m.gxi" => "val csize: fn(c: Collection) -> i64",
    "/test/m.gx" => r#"
        use Collection::*;
        let csize = |c: Collection| fold(c, 0, |acc, x| acc + 1)
    "#
);

run!(
    a_trait_method_with_a_trait_parameter,
    prints(r#"["1/s", "1/2"]"#),
    "/test.gx" => r#"
        trait Sh { val sh: fn(self, c: Display) -> string };
        impl Sh for i64 { let sh = |s, c| "[s]/[c]" };
        let result = (Sh::sh(1, "s"), Sh::sh(1, 2))
    "#
);

run!(
    a_trait_method_passed_as_a_value_is_one_instance_per_use,
    prints(r#"["1/s", "1/2"]"#),
    "/test.gx" => r#"
        trait Sh { val sh: fn(self, c: Display) -> string };
        impl Sh for i64 { let sh = |s, c| "[s]/[c]" };
        let ap = |f: fn(s: i64, c: string) -> string| f(1, "s");
        let bp = |f: fn(s: i64, c: i64) -> string| f(1, 2);
        let result = (ap(Sh::sh), bp(Sh::sh))
    "#
);

run!(
    a_nested_quantified_signature_in_an_interface_val_is_refused,
    refused(QUANTIFIED),
    "/test.gx" => "mod m; let result = m::c.f(1)",
    "/test/m.gxi" => "val c: {f: fn<'b: Any>(x: 'b) -> 'b}",
    "/test/m.gx" => "let c = {f: |x| x}"
);

run!(
    a_quantified_parameter_of_a_trait_method_is_refused,
    refused(QUANTIFIED),
    "/test.gx" => r#"
        trait T { val m: fn(self, f: fn<'a: Any>(x: 'a) -> 'a) -> i64 };
        let result = 0
    "#
);

run!(
    an_interface_val_quantified_at_its_top,
    prints(r#"[i64:1, "s"]"#),
    "/test.gx" => "mod m; let result = (m::g(1), m::g(\"s\"))",
    "/test/m.gxi" => "val g: fn<'a: Any>(x: 'a) -> 'a",
    "/test/m.gx" => "let g = |x| x"
);

run!(
    a_parenthesized_lambda_in_a_holder_is_generic,
    "{ let k = {p: (|x| x)}; (k.p(1), k.p(true)) }",
    prints("[i64:1, true]");
    FuseExpect::None
);

run!(
    a_parenthesized_lambda_in_a_tuple_holder_is_generic,
    "{ let k = ((|x| x), 1); (k.0(1), k.0(true)) }",
    prints("[i64:1, true]");
    FuseExpect::None
);

run!(
    a_parenthesized_generic_name_in_a_holder_is_generic,
    "{ let f = |x| x; let k = {p: (f)}; (k.p(1), k.p(true)) }",
    prints("[i64:1, true]");
    FuseExpect::None
);

// The ML rule: a function value no binding names is one instance.
run!(
    a_callbacks_result_is_one_instance,
    "{ let fs = array::map([1], |r| |x| x); let g = fs[0]$; (g(1), g(\"s\")) }",
    refused("does not contain string");
    FuseExpect::None
);

run!(
    a_callbacks_result_takes_its_one_type,
    "{ let fs = array::map([1], |r| |x| x); let g = fs[0]$; (g(1), g(2)) }",
    prints("[i64:1, i64:2]");
    FuseExpect::None
);

run!(
    a_generic_binding_read_into_a_callbacks_result_is_one_instance,
    "{ let id = |x| x; let fs = array::map([1], |r| id); let g = fs[0]$; (g(1), g(\"s\")) }",
    refused("does not contain string");
    FuseExpect::None
);

run!(
    a_functions_result_is_one_instance,
    "{ let mk = |z| 'a: Number |x: 'a| x; let h = mk(0); (h(1), h(1.5)) }",
    refused("does not contain f64");
    FuseExpect::None
);

run!(
    a_field_of_a_calls_result_is_one_instance,
    "{ let mk = |z| {f: |x| x}; let h = mk(0).f; (h(1), h(\"s\")) }",
    refused("does not contain string");
    FuseExpect::None
);

run!(
    each_call_of_a_maker_is_an_instance_of_its_own,
    "{ let mk = |z| |x| x; let a = mk(0); let b = mk(0); (a(1), b(\"s\")) }",
    prints(r#"[i64:1, "s"]"#);
    FuseExpect::None
);

run!(
    a_projection_of_a_generic_binding_is_one_instance,
    "{ let id = |x| x; let k = (id, 1).0; (k(1), k(\"s\")) }",
    refused("does not contain string");
    FuseExpect::None
);

// A let in a seq step names what it holds as a let outside one does.
run!(
    a_holder_bound_in_a_seq_step_is_generic,
    "seq { let s = {g: |x| x}; let a = s.g(1); let b = s.g(\"s\"); (a, b) }",
    prints(r#"[i64:1, "s"]"#);
    FuseExpect::Jit
);

run!(
    a_holder_bound_in_a_seqq_step_is_generic,
    "seqq { let s = {g: |x| x}; (s.g(1), s.g(\"s\")) }",
    prints(r#"[i64:1, "s"]"#);
    FuseExpect::Jit
);

run!(
    a_destructured_holder_in_a_seq_statement_block_is_generic,
    "seq { { let (g, n) = (|x| x, 1); (g(n), g(\"s\")) } }",
    prints(r#"[i64:1, "s"]"#);
    FuseExpect::Jit
);

run!(
    a_holder_of_a_steps_lambda_in_a_seq_step_is_generic,
    "seq { let f = |x| x; let s = {g: f}; (s.g(1), s.g(\"s\")) }",
    prints(r#"[i64:1, "s"]"#);
    FuseExpect::Jit
);

run!(
    a_destructured_holder_in_a_seq_step_written_by_a_later_step_is_one_instance,
    "seq { let (g, k) = (|x| x, |y| y); let rk = &mut k; *rk <- |y: i64| y + 1; let a = 0; (k(\"a\"), g(1)) }",
    refused("type mismatch i64 does not contain string");
    FuseExpect::None
);

run!(
    a_destructured_holder_in_a_seq_step_written_by_a_later_step_at_its_type,
    "seq { let (g, k) = (|x| x, |y| y); let rk = &mut k; *rk <- |y: i64| y + 1; let a = 0; (k(2), g(\"s\")) }",
    prints(r#"[i64:3, "s"]"#);
    FuseExpect::Jit
);

// A step's destructured holder is evaluated once: its `?` raises once and
// its connect writes once. Each fixture reads its counts eight cycles in.
run!(
    a_destructured_holder_in_a_seq_step_raises_once,
    r#"{
        let n = 0;
        let r: Result<i64, `E> = error(`E);
        let out = { catch(e) n <- e ~ n + 1; seq { let (g, m) = (|x| x, r?); g(m) } };
        let k = 0;
        k <- select k { 8 => never(), _ => k + 1 };
        select k { 8 => n, _ => never() }
    }"#,
    is(Value::I64(1));
    FuseExpect::Jit
);

run!(
    a_destructured_holder_in_a_seq_step_connects_once,
    r#"{
        let c = 0;
        let fires = 0;
        fires <- c ~ fires + 1;
        let out = seq { let (g, m) = (|x| x, { c <- 10 + c; 2 }); g(m) };
        let k = 0;
        k <- select k { 8 => never(), _ => k + 1 };
        select k { 8 => (out, c, fires), _ => never() }
    }"#,
    prints("[i64:2, i64:10, i64:2]");
    FuseExpect::Jit
);

run!(
    a_destructured_holder_in_a_seq_statement_block_raises_once,
    r#"{
        let n = 0;
        let r: Result<i64, `E> = error(`E);
        let out = { catch(e) n <- e ~ n + 1; seq { { let (g, m) = (|x| x, r?); g(m) }; 1 } };
        let k = 0;
        k <- select k { 8 => never(), _ => k + 1 };
        select k { 8 => n, _ => never() }
    }"#,
    is(Value::I64(1));
    FuseExpect::Jit
);

// A method's annotation writes no scheme of its own: the trait's
// declaration is the scheme, which the method is held to.
const OUTSIDE_V: &str =
    "that of v, a binding outside its let: give v a type that holds every 'a";

run!(
    a_quantified_trait_default_writing_an_outer_binding_is_refused,
    refused(OUTSIDE_V),
    "/test.gx" => r#"
        let v = never();
        trait T { val m: fn<'a: Any>(self, x: 'a) -> 'a = |s, x| { v <- x; x } };
        impl T for i64 {};
        let a: i64 = v;
        let result = T::m(1, "s")
    "#
);

run!(
    a_quantified_trait_default_writing_a_binding_of_any_type,
    prints(r#"["s", i64:2]"#),
    "/test.gx" => r#"
        let v: Any = never();
        trait T { val m: fn<'a: Any>(self, x: 'a) -> 'a = |s, x| { v <- x; x } };
        impl T for i64 {};
        let result = (T::m(1, "s"), T::m(1, 2))
    "#
    ; FuseExpect::None
);

run!(
    a_quantified_trait_default_called_at_two_types,
    prints("[i64:4, f64:3.]"),
    "/test.gx" => r#"
        trait T { val m: fn<'a: Number + Singleton>(self, x: 'a) -> 'a = |s, x| x + x };
        impl T for i64 {};
        let result = (T::m(1, 2), T::m(1, 1.5))
    "#
);

run!(
    an_impl_methods_quantified_annotation_over_an_outer_writer_is_refused,
    refused(OUTSIDE_V),
    "/test.gx" => r#"
        trait T { val m: fn(self, x: 'a) -> 'a };
        let v = never();
        impl T for i64 { let m: fn<'a: Any>(s: i64, x: 'a) -> 'a = |s, x| { v <- x; x } };
        let a: i64 = v;
        let result = T::m(1, "s")
    "#
);

run!(
    an_impl_methods_quantified_annotation_over_a_makers_result_is_refused,
    refused("its value is no lambda, so no call copies its types"),
    "/test.gx" => r#"
        trait T { val m: fn(self, x: 'a) -> 'a };
        let mk = |z| { let v = never(); |s: i64, x| { v <- x; v } };
        impl T for i64 { let m: fn<'a: Any>(s: i64, x: 'a) -> 'a = mk(0) };
        let result = (T::m(1, 2), T::m(1, "s"))
    "#
);

run!(
    an_impl_methods_quantified_annotation_over_a_lambda_at_two_types,
    prints("[i64:4, f64:3.]"),
    "/test.gx" => r#"
        trait T { val m: fn<'a: Number + Singleton>(self, x: 'a) -> 'a };
        impl T for i64 { let m: fn<'a: Number + Singleton>(s: i64, x: 'a) -> 'a = |s, x| x + x };
        let result = (T::m(1, 2), T::m(1, 1.5))
    "#
);

run!(
    an_unannotated_impl_method_under_a_quantified_val_writing_an_outer_binding_is_refused,
    refused(OUTSIDE_V),
    "/test.gx" => r#"
        let v = never();
        trait T { val m: fn<'a: Any>(self, x: 'a) -> 'a };
        impl T for i64 { let m = |s, x| { v <- x; x } };
        let result = (T::m(1, "s"), T::m(1, 2))
    "#
);

run!(
    an_unannotated_impl_method_under_a_quantified_val_writing_a_binding_of_any_type,
    prints(r#"["s", i64:2]"#),
    "/test.gx" => r#"
        let v: Any = never();
        trait T { val m: fn<'a: Any>(self, x: 'a) -> 'a };
        impl T for i64 { let m = |s, x| { v <- x; x } };
        let result = (T::m(1, "s"), T::m(1, 2))
    "#
    ; FuseExpect::None
);

run!(
    a_makers_result_as_an_impl_method_under_a_quantified_val_is_refused,
    refused("its value is no lambda, so no call copies its types"),
    "/test.gx" => r#"
        let mk = |z| |s: i64, x| { z <- x; z };
        trait T { val m: fn<'a: Any>(self, x: 'a) -> 'a };
        impl T for i64 { let m = mk(never()) };
        let result = (T::m(1, "s"), T::m(1, 2))
    "#
);
