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
    a_projection_of_a_tuple_literal_is_one_instance,
    "{ let id = |x| x; let k = (id, 1).0; (k(1), k(\"s\")) }",
    refused("does not contain string");
    FuseExpect::None
);

// A let names what its value reads: past parentheses and a block's final
// expression, a lambda, a generalized binding, or a field or tuple of one.
run!(
    a_field_of_a_generic_binding_is_generic,
    "{ let s = {g: |x| x}; let j = s.g; (j(1), j(\"s\")) }",
    prints(r#"[i64:1, "s"]"#);
    FuseExpect::None
);

run!(
    a_tuple_field_of_a_generic_binding_is_generic,
    "{ let t = (|x| x, 1); let j = t.0; (j(1), j(\"s\")) }",
    prints(r#"[i64:1, "s"]"#);
    FuseExpect::None
);

run!(
    a_nested_field_of_a_generic_binding_is_generic,
    "{ let u = {a: {g: |x| x}}; let j = u.a.g; (j(1), j(\"s\")) }",
    prints(r#"[i64:1, "s"]"#);
    FuseExpect::None
);

run!(
    a_block_whose_value_is_a_lambda_is_generic,
    "{ let f = { let n = 1; |x| x }; (f(1), f(\"s\")) }",
    prints(r#"[i64:1, "s"]"#);
    FuseExpect::None
);

run!(
    a_block_whose_value_is_a_generic_name_is_generic,
    "{ let g = |x| x; let f = { let n = 1; g }; (f(1), f(\"s\")) }",
    prints(r#"[i64:1, "s"]"#);
    FuseExpect::None
);

run!(
    a_parenthesized_block_whose_value_is_a_field_read_is_generic,
    "{ let s = {g: |x| x}; let j = ({ let n = 1; (s).g }); (j(1), j(\"s\")) }",
    prints(r#"[i64:1, "s"]"#);
    FuseExpect::None
);

run!(
    a_holder_of_a_field_read_is_generic,
    "{ let s = {g: |x| x}; let u = {h: { let n = 1; s.g }}; (u.h(1), u.h(\"s\")) }",
    prints(r#"[i64:1, "s"]"#);
    FuseExpect::None
);

run!(
    a_reference_to_a_generic_function_is_one_instance,
    "{ let f = |x| x; let r = &f; ((*r)(1), (*r)(\"s\")) }",
    refused("does not contain string");
    FuseExpect::None
);

run!(
    a_reference_to_a_generic_function_takes_its_one_type,
    "{ let f = |x| x; let r = &f; ((*r)(1), (*r)(2)) }",
    prints("[i64:1, i64:2]");
    FuseExpect::None
);

run!(
    a_written_field_alias_is_one_instance,
    "{ let s = {g: |x| x}; let j = s.g; j <- |x| x; (j(1), j(\"s\")) }",
    refused("does not contain string");
    FuseExpect::None
);

run!(
    a_written_field_alias_takes_its_one_type,
    "{ let s = {g: |x| x}; let j = s.g; j <- |x| x; (j(1), j(2)) }",
    prints("[i64:1, i64:2]");
    FuseExpect::None
);

run!(
    an_interface_val_implemented_by_a_field_read,
    prints(r#"[i64:1, "s"]"#),
    "/test.gx" => r#"
        mod m;
        let result = (m::f(1), m::f("s"))
    "#,
    "/test/m.gxi" => r#"
        val f: fn(x: 'a) -> 'a
    "#,
    "/test/m.gx" => r#"
        let s = {g: |x| x};
        let f = s.g
    "#; FuseExpect::None);

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

// A step reads an outer variable through its entry snapshot, which
// carries a generalized binding's scheme: nothing writes a snapshot.
run!(
    an_outer_holder_read_in_a_seq_step_is_generic,
    "{ let s = {g: |x| x}; seq { (s.g(1), s.g(\"s\")) } }",
    prints(r#"[i64:1, "s"]"#);
    FuseExpect::Jit
);

run!(
    an_outer_holder_bound_in_a_seq_step_is_generic,
    "{ let s = {g: |x| x}; seq { let u = s; (u.g(1), u.g(\"s\")) } }",
    prints(r#"[i64:1, "s"]"#);
    FuseExpect::Jit
);

run!(
    a_holder_of_an_outer_holder_in_a_seq_step_is_generic,
    "{ let s = {g: |x| x}; seq { let u = {a: s}; let a = u.a.g(1); let b = u.a.g(\"s\"); (a, b) } }",
    prints(r#"[i64:1, "s"]"#);
    FuseExpect::Jit
);

run!(
    a_holder_of_an_outer_holder_in_a_seq_statement_block_is_generic,
    "{ let s = {g: |x| x}; seq { { let u = {a: s}; (u.a.g(1), u.a.g(\"s\")) } } }",
    prints(r#"[i64:1, "s"]"#);
    FuseExpect::Jit
);

run!(
    a_holder_with_a_live_part_in_a_seq_step_is_generic,
    "{ let v = 1; seq { let s = {g: |x| x, n: live(v)}; (s.g(1), s.g(\"s\"), s.n) } }",
    prints(r#"[i64:1, "s", i64:1]"#);
    FuseExpect::Jit
);

run!(
    a_holder_with_a_call_part_in_a_seq_step_is_one_instance,
    "seq { let s = {g: |x| x, n: str::len(\"ab\")}; (s.g(1), s.g(\"s\"), s.n) }",
    refused("does not contain string");
    FuseExpect::None
);

run!(
    a_holder_with_a_call_part_in_a_seq_step_takes_its_one_type,
    "seq { let s = {g: |x| x, n: str::len(\"ab\")}; (s.g(1), s.g(2), s.n) }",
    prints("[i64:1, i64:2, i64:2]");
    FuseExpect::Jit
);

// The snapshot reads a holder's part that tracks a variable at the step's
// entry: `k` counts on to 3 while the step waits, and `s.n` is what it was.
run!(
    a_holder_part_a_seq_step_reads_is_its_entry_value,
    "{ let k = 0; k <- select k { 8 => never(), _ => k + 1 }; let s = {g: |x| x, n: k}; \
     seq { select live(k) { 3 => (s.g(1), s.g(\"s\"), s.n), _ => never() } } }",
    prints(r#"[i64:1, "s", i64:1]"#);
    FuseExpect::Jit
);

run!(
    a_holder_a_later_step_writes_is_one_instance,
    "{ let s = {g: |x| x}; seq { let u = s; let w = &mut u; *w <- {g: |x: i64| x + 1}; (u.g(1), u.g(\"s\")) } }",
    refused("does not contain string");
    FuseExpect::None
);

run!(
    a_holder_a_later_step_writes_takes_its_one_type,
    "{ let s = {g: |x| x}; seq { let u = s; let w = &mut u; *w <- {g: |x: i64| x + 1}; (u.g(1), u.g(2)) } }",
    prints("[i64:2, i64:3]");
    FuseExpect::Jit
);

// A later step reads a step's holder through its own entry snapshot: `h`,
// bound before the write, calls the holder as it stood at `h`'s step.
run!(
    a_lambda_over_a_step_s_holder_reads_it_at_its_step_s_entry,
    "seq { let u = (|x| x + 1, 0); let h = |z| u.0(z); let ru = &mut u; \
     *ru <- (|x| x + 100, 0); let b = 1; h(1) }",
    is(Value::I64(2));
    FuseExpect::Jit
);

run!(
    a_lambda_over_a_step_s_written_capture_reads_it_at_its_step_s_entry,
    "seq { let u@ (g, k) = (|x| x, |y| y); let h = |z| u.0(z); let ru = &mut u; \
     *ru <- (|x: i64| x + 100, |y: i64| y); let b = 1; let c = 2; h(\"t\") }",
    is(Value::from("t"));
    FuseExpect::Jit
);

run!(
    a_step_s_written_capture_is_its_write_after_it,
    "seq { let u@ (g, k) = (|x| x, |y| y); let h = |z| u.0(z); let a = h(\"s\"); \
     let ru = &mut u; *ru <- (|x: i64| x + 100, |y: i64| y); let b = 1; let c = 2; \
     (a, h(\"t\"), u.0(1)) }",
    prints(r#"["s", "t", i64:101]"#);
    FuseExpect::Jit
);

// A step's `let` of a generic function, a read of it or of a field of one,
// is generic as it is outside a seq: the step's value carries the scheme.
run!(
    a_field_alias_in_a_seq_step_is_generic,
    "{ let s = {g: |x| x}; seq { let j = s.g; (j(1), j(\"s\")) } }",
    prints(r#"[i64:1, "s"]"#);
    FuseExpect::Jit
);

run!(
    an_outer_function_alias_in_a_seq_step_is_generic,
    "{ let f = |x| x; seq { let g = f; (g(1), g(\"s\")) } }",
    prints(r#"[i64:1, "s"]"#);
    FuseExpect::Jit
);

run!(
    a_step_function_alias_in_a_seq_step_is_generic,
    "seq { let f = |x| x; let g = f; (g(1), g(\"s\")) }",
    prints(r#"[i64:1, "s"]"#);
    FuseExpect::Jit
);

run!(
    a_field_alias_in_a_seq_statement_block_is_generic,
    "{ let s = {g: |x| x}; seq { { let j = s.g; (j(1), j(\"s\")) } } }",
    prints(r#"[i64:1, "s"]"#);
    FuseExpect::Jit
);

// A `{ .. }` item's value is its last item's, which carries a generic
// name's scheme as a block's final expression does.
run!(
    a_seq_statement_block_ending_in_a_holder_is_generic,
    "{ let s = {g: |x| x}; seq { let u = s; let v = { let w = u; w }; (v.g(2), v.g(\"z\")) } }",
    prints(r#"[i64:2, "z"]"#);
    FuseExpect::Jit
);

run!(
    a_seq_statement_block_ending_in_a_field_read_is_generic,
    "{ let s = {g: |x| x}; seq { let j = { let n = 1; s.g }; (j(1), j(\"s\")) } }",
    prints(r#"[i64:1, "s"]"#);
    FuseExpect::Jit
);

run!(
    a_written_function_alias_in_a_seq_step_is_one_instance,
    "{ let f = |x| x; seq { let g = f; g <- |x: i64| x + 1; (g(1), g(\"s\")) } }",
    refused("does not contain string");
    FuseExpect::None
);

run!(
    a_written_function_alias_in_a_seq_step_takes_its_one_type,
    "{ let f = |x| x; seq { let g = f; g <- |x: i64| x + 1; (g(1), g(2)) } }",
    prints("[i64:2, i64:3]");
    FuseExpect::Jit
);

run!(
    a_written_name_a_seq_statement_block_ends_in_is_one_instance,
    "{ let s = {g: |x| x}; seq { let v = { let w = s; w <- {g: |x: i64| x + 1}; w }; (v.g(1), v.g(\"s\")) } }",
    refused("does not contain string");
    FuseExpect::None
);

run!(
    a_written_name_a_seq_statement_block_ends_in_takes_its_one_type,
    "{ let s = {g: |x| x}; seq { let v = { let w = s; w <- {g: |x: i64| x + 1}; w }; (v.g(1), v.g(2)) } }",
    prints("[i64:1, i64:2]");
    FuseExpect::Jit
);

// Two reads of one holder's field in an array meet as two names' reads do:
// an element of data is one instance.
run!(
    two_reads_of_a_holder_field_in_an_array_are_one_instance,
    "{ let s = {g: |x| x}; let a = [s.g, s.g]; (a[0]$(1), a[1]$(\"s\")) }",
    refused("does not contain string");
    FuseExpect::None
);

run!(
    two_reads_of_a_holder_field_in_an_array_take_their_one_type,
    "{ let s = {g: |x| x}; let a = [s.g, s.g]; (a[0]$(1), a[1]$(2)) }",
    prints("[i64:1, i64:2]");
    FuseExpect::None
);

// A value a program writes in a seq step is judged as the same `let`
// outside a seq: only the lowering's own forms pass a scheme on.
run!(
    a_program_s_tuple_read_in_a_seq_step_is_one_instance,
    "{ let s = {g: |x| x}; seq { let j = (1, s.g).1; (j(1), j(\"s\")) } }",
    refused("does not contain string");
    FuseExpect::None
);

run!(
    a_program_s_tuple_read_in_a_seq_step_takes_its_one_type,
    "{ let s = {g: |x| x}; seq { let j = (1, s.g).1; (j(1), j(2)) } }",
    prints("[i64:1, i64:2]");
    FuseExpect::Jit
);

run!(
    a_program_s_sample_in_a_seq_step_is_one_instance,
    "{ let s = {g: |x| x}; let k = 1; seq { let h = k ~! s.g; (h(1), h(\"s\")) } }",
    refused("does not contain string");
    FuseExpect::None
);

run!(
    a_program_s_sample_in_a_seq_step_takes_its_one_type,
    "{ let s = {g: |x| x}; let k = 1; seq { let h = k ~! s.g; (h(1), h(2)) } }",
    prints("[i64:1, i64:2]");
    FuseExpect::Jit
);

run!(
    a_program_s_any_in_a_seq_step_is_one_instance,
    "{ let s = {g: |x| x}; seq { let h = any(s.g, s.g); (h(1), h(\"s\")) } }",
    refused("does not contain string");
    FuseExpect::None
);

run!(
    a_program_s_any_in_a_seq_step_takes_its_one_type,
    "{ let s = {g: |x| x}; seq { let h = any(s.g, s.g); (h(1), h(2)) } }",
    prints("[i64:1, i64:2]");
    FuseExpect::Jit
);

// A step's destructured holder is refused at its statement, inside the seq,
// once the holder is checked: the pattern takes the holder's own binding.
run!(
    a_destructured_holder_in_a_seq_step_is_refused_at_its_let,
    "seq { let (g, m, z) = (|x| x, 1); g(m) }",
    |v: anyhow::Result<&Value>| matches!(v, Err(e) if {
        let e = format!("{e:#}");
        let identity = e
            .split_once("match error (fn(x: ")
            .and_then(|(_, r)| r.split_once(": unbound) -> "))
            .is_some_and(|(x, r)| r.starts_with(x));
        e.contains("in: let result = seq { let (g, m, z) = ")
            && e.contains("column: 20 in module /test: match error")
            && identity
    });
    FuseExpect::None
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
