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

// A let of a module without an interface that a write from outside names
// (`&mut m::j`, `m::j <-`, through an import) is written, so one instance,
// whatever generic form its value takes.
macro_rules! written_from_outside {
    ($($name:ident => $body:expr, $test:expr,)*) => {$(
        run!(
            $name,
            refused("does not contain string"),
            "/test.gx" => $test,
            "/test/m.gx" => $body
        );
    )*};
}

const WRITTEN_J: &str = "mod m; let r = &mut m::j; *r <- |x: i64| x + 1; \
     let result = (m::j(1), m::j(\"s\"))";

written_from_outside! {
    a_module_s_field_alias_a_parent_writes_is_one_instance =>
        "let s = {g: |x| x}; let j = s.g", WRITTEN_J,
    a_module_s_function_alias_a_parent_writes_is_one_instance =>
        "let f = |x| x; let j = f", WRITTEN_J,
    a_module_s_block_alias_a_parent_writes_is_one_instance =>
        "let s = {g: |x| x}; let j = { let n = 1; s.g }", WRITTEN_J,
    a_module_s_holder_a_parent_writes_is_one_instance =>
        "let s = {g: |x| x}; let j = s",
        "mod m; let r = &mut m::j; *r <- {g: |x: i64| x + 1}; \
         let result = (m::j.g(1), m::j.g(\"s\"))",
    a_module_s_alias_a_parent_imports_and_writes_is_one_instance =>
        "let s = {g: |x| x}; let j = s.g",
        "mod m; use m::j; let r = &mut j; *r <- |x: i64| x + 1; \
         let result = (j(1), j(\"s\"))",
    a_module_a_parent_aliases_and_writes_is_one_instance =>
        "let s = {g: |x| x}; let j = s.g",
        "mod m; use m as q; let r = &mut q::j; *r <- |x: i64| x + 1; \
         let result = (m::j(1), m::j(\"s\"))",
    a_module_a_parent_aliases_through_self_and_writes_is_one_instance =>
        "let f = |x| x; let j = f",
        "mod m; use self::m as q; let r = &mut q::j; *r <- |x: i64| x + 1; \
         let result = (m::j(1), m::j(\"s\"))",
    a_module_s_holder_a_parent_aliases_and_writes_is_one_instance =>
        "let s = {g: |x| x}; let j = s",
        "mod m; use m as q; let r = &mut q::j; *r <- {g: |x: i64| x + 1}; \
         let result = (m::j.g(1), m::j.g(\"s\"))",
    a_module_a_parent_s_block_aliases_and_writes_is_one_instance =>
        "let s = {g: |x| x}; let j = s.g",
        "mod m; let z = { use m as q; let r = &mut q::j; *r <- |x: i64| x + 1; 0 }; \
         let result = (m::j(1), m::j(\"s\"))",
    a_module_a_parent_s_seq_step_aliases_and_writes_is_one_instance =>
        "let s = {g: |x| x}; let j = s.g",
        "mod m; use m as q; let result = seq { let rj = &mut q::j; \
         *rj <- |x: i64| x + 1; let a = 0; (m::j(1), m::j(\"s\")) }",
    a_module_a_parent_s_block_globs_over_a_let_and_writes_is_one_instance =>
        "let s = {g: |x| x}; let j = s.g",
        "mod m; let j = |x: i64| x; \
         let z = { use m::*; let r = &mut j; *r <- |x: i64| x + 1; 0 }; \
         let result = (m::j(1), m::j(\"s\"))",
    a_module_s_function_alias_a_parent_s_block_globs_over_a_let_and_writes_is_one_instance =>
        "let f = |x| x; let j = f",
        "mod m; let j = |x: i64| x; \
         let z = { use m::*; let r = &mut j; *r <- |x: i64| x + 1; 0 }; \
         let result = (m::j(1), m::j(\"s\"))",
    a_module_a_parent_s_seq_statement_globs_over_a_let_and_writes_is_one_instance =>
        "let s = {g: |x| x}; let j = s.g",
        "mod m; let j = |x: i64| x; let result = seq { \
         { use m::*; let r = &mut j; *r <- |x: i64| x + 1 }; (m::j(1), m::j(\"s\")) }",
}

// A name a pattern takes out of a generalized binding, which something
// writes, is one instance its uses and writers share; the pattern's other
// names keep the scheme.
macro_rules! destructured_and_written {
    ($($name:ident => $test:expr, $says:expr,)*) => {$(
        run!($name, $test, refused($says); FuseExpect::None);
    )*};
}

destructured_and_written! {
    a_struct_name_taken_from_a_holder_and_written_is_one_instance =>
        "{ let s = {f: |x| x, n: 1}; let {f, n} = s; let a = f(\"a\"); \
           f <- |x: i64| x + 1; a }",
        "f is fn(x: string) -> string and cannot hold fn(x: i64) -> i64",
    a_name_taken_from_a_holder_s_field_and_written_is_one_instance =>
        "{ let s = {a: {f: |x| x, n: 1}}; let {f, n} = s.a; let a = f(\"a\"); \
           f <- |x: i64| x + 1; a }",
        "f is fn(x: string) -> string and cannot hold fn(x: i64) -> i64",
    a_name_taken_from_a_holder_s_element_and_written_is_one_instance =>
        "{ let s = ((|x| x, 1), 2); let (f, n) = s.0; let a = f(\"a\"); \
           f <- |x: i64| x + 1; a }",
        "f is fn(x: string) -> string and cannot hold fn(x: i64) -> i64",
    a_capture_of_a_holder_written_is_one_instance =>
        "{ let s = {f: |x| x, n: 1}; let u@ {f, n} = s; let a = u.f(\"a\"); \
           u <- {f: |x: i64| x + 1, n: 2}; a }",
        "u is { f: fn(x: string) -> string, n: i64 } and cannot hold",
    a_seq_step_s_name_taken_from_a_literal_and_written_is_one_instance =>
        "seq { let (g, k) = (|x| x, |y| y); let h = |z| g(z); let a = h(\"s\"); \
           let rg = &mut g; *rg <- |x: i64| x + 100; let b = 1; let c = 2; (a, h(\"t\")) }",
        "does not contain fn(x: i64) -> i64",
}

run!(
    a_name_taken_from_a_holder_beside_a_written_one_is_generic,
    "{ let s = {f: |x| x, g: |y| y}; let {f, g} = s; f <- |x: i64| x + 1; \
       (f(1), g(\"s\"), g(2)) }",
    prints(r#"[i64:1, "s", i64:2]"#);
    FuseExpect::None
);

run!(
    a_struct_name_taken_from_a_holder_and_written_takes_its_one_type,
    "{ let s = {f: |x| x, n: 1}; let {f, n} = s; f <- |x: i64| x + 1; (f(1), n) }",
    prints("[i64:1, i64:1]");
    FuseExpect::None
);

// A function body compiles after the definitions it reads are checked: a
// write in it the block's walk does not see finds the binding generic, so
// what it writes must be as general.
macro_rules! written_after_the_check {
    ($($name:ident => $test:expr,)*) => {$(
        run!(
            $name,
            refused("it holds a type variable of the code around the write; to write \
                     it at one type, write that type on its let (on its val, where an \
                     interface declares it)"),
            "/test.gx" => $test,
            "/test/m.gx" => "let s = {g: |x| x}; let j = s.g"
        );
    )*};
}

// A refused write names the binding it reached by its module path, and a
// package's binding, which the program cannot annotate, gets no hint.
run!(
    a_write_a_glob_sends_to_a_package_s_binding_names_it,
    r#"{
        let s = {f: |x| x};
        let len = s.f;
        let r = { use array::*; let g = |v: i64| { len <- |x: i64| x + 1; v }; g(1) };
        (len("a"), r)
    }"#,
    |v: Result<&Value>| matches!(v, Err(e) if {
        let e = format!("{e:#}");
        e.contains("array::len is fn(a: Array<'a: unbound>) -> i64, a generic binding, \
                    so what is written to it must be as general, and fn(x: i64) -> i64 \
                    is not")
            && !e.contains("to write it at one type")
    })
);

written_after_the_check! {
    a_module_s_alias_a_parent_s_lambda_writes_is_held_to_its_scheme =>
        "mod m; let set = |v| { m::j <- v; null }; let z = set(|x: i64| x + 1); \
         let result = (m::j(1), m::j(\"s\"))",
    a_module_s_alias_a_parent_s_lambda_writes_through_an_alias_is_held_to_its_scheme =>
        "mod m; use m as q; let set = |v| { q::j <- v; null }; \
         let z = set(|x: i64| x + 1); let result = (m::j(1), m::j(\"s\"))",
}

run!(
    a_module_s_annotated_alias_a_parent_s_lambda_writes_takes_its_one_type,
    prints("[i64:1, i64:0]"),
    "/test.gx" => "mod m; let set = |v| { m::j <- v; 0 }; let z = set(|x: i64| x + 1); \
                   let result = (m::j(1), z)",
    "/test/m.gx" => "let s = {g: |x| x}; let j: fn(x: i64) -> i64 = s.g"; FuseExpect::None
);

// A glob in a nested block names what it provides before a let outside
// the block: the module's let, not the parent's, is written.
run!(
    a_parent_s_let_a_nested_glob_shadows_is_generic,
    prints(r#"[i64:1, "s", i64:5]"#),
    "/test.gx" => "mod m; let s = {g: |x| x}; let j = s.g; \
                   let z = { use m::*; let r = &mut j; *r <- |x: i64| x; 0 }; \
                   let result = (j(1), j(\"s\"), m::j(5))",
    "/test/m.gx" => "let j = |x: i64| x"; FuseExpect::None
);

run!(
    a_parent_s_let_a_nested_glob_of_an_interface_shadows_is_generic,
    prints(r#"[i64:1, "s", i64:0]"#),
    "/test.gx" => "mod m; let s = {g: |x| x}; let j = s.g; \
                   let z = { use m::*; let r = &mut j; *r <- |x: i64| x + 1; 0 }; \
                   let result = (j(1), j(\"s\"), z)",
    "/test/m.gxi" => "val j: fn(x: i64) -> i64",
    "/test/m.gx" => "let j = |x: i64| x"; FuseExpect::None
);

// A write in a module's function body compiles at its definition's check,
// after the parent's lets: a glob in the block around the `mod` hides
// nothing from it, and it is held to one instance.
run!(
    a_module_s_function_body_writes_past_a_nested_glob_is_one_instance,
    prints("[i64:1, i64:1]"),
    "/test.gx" => "let s = {f: |x| x}; let j = s.f; \
                   let r = { use array::*; mod m; m::n }; \
                   let result = (j(1), r)",
    "/test/m.gx" => "let g = |v: i64| { super::j <- |x: i64| x + 1; v }; let n = g(1)";
    FuseExpect::None
);

run!(
    a_module_s_function_body_writes_through_an_import_past_a_nested_glob,
    prints("[i64:1, i64:1]"),
    "/test.gx" => "let s = {f: |x| x}; let j = s.f; \
                   let r = { use array::*; mod m; m::n }; \
                   let result = (j(1), r)",
    "/test/m.gx" => "use super::j; \
                     let g = |v: i64| { let w = &mut j; *w <- |x: i64| x + 1; v }; \
                     let n = g(1)";
    FuseExpect::None
);

// A `use` of a value named like a module leaves the module's name alone.
run!(
    a_module_a_value_import_shares_the_name_of_is_written_through_it,
    refused("does not contain string"),
    "/test.gx" => "mod m; mod n; use n::m; let r = &mut m::j; *r <- |x: i64| x + 1; \
                   let result = (m::j(1), m::j(\"s\"), m)",
    "/test/m.gx" => "let s = {g: |x| x}; let j = s.g",
    "/test/n.gx" => "let m = 3"
);

run!(
    a_module_a_value_import_shares_the_name_of_takes_its_one_type,
    prints("[i64:1, i64:2, i64:3]"),
    "/test.gx" => "mod m; mod n; use n::m; let r = &mut m::j; *r <- |x: i64| x; \
                   let result = (m::j(1), m::j(2), m)",
    "/test/m.gx" => "let s = {g: |x| x}; let j = s.g",
    "/test/n.gx" => "let m = 3"; FuseExpect::None
);

// A nested block's `self::` import names the module's own let.
macro_rules! written_through_self {
    ($($name:ident => $test:expr,)*) => {$(
        run!($name, refused("does not contain string"), "/test.gx" => $test);
    )*};
}

written_through_self! {
    a_let_a_nested_block_imports_through_self_and_writes_is_one_instance =>
        "let s = {g: |x| x}; let j = s.g; \
         let z = { use self::j as k; let r = &mut k; *r <- |x: i64| x + 1; 0 }; \
         let result = (j(1), j(\"s\"), z)",
    a_let_a_nested_block_globs_through_self_and_writes_is_one_instance =>
        "let s = {g: |x| x}; let j = s.g; \
         let z = { let j = 2; { use self::*; let r = &mut j; *r <- |x: i64| x + 1; 0 } }; \
         let result = (j(1), j(\"s\"), z)",
}

// A write in a `seqq`'s flush is a writer as any other.
run!(
    a_holder_a_seqq_flush_writes_is_one_instance,
    "{ let s = {g: |x| x}; let j = s.g; \
       let v = seqq 1; flush({ let r = &mut j; *r <- |x: i64| x + 1; never<bool>() }) { 1 }; \
       (j(1), j(\"s\"), v) }",
    refused("does not contain string");
    FuseExpect::None
);

run!(
    a_holder_a_seqq_flush_writes_takes_its_one_type,
    "{ let s = {g: |x| x}; let j = s.g; \
       let v = seqq 1; flush({ let r = &mut j; *r <- |x: i64| x; never<bool>() }) { 1 }; \
       (j(1), j(2), v) }",
    prints("[i64:1, i64:2, i64:1]");
    FuseExpect::Jit
);

// The same through a submodule a parent imports, or a glob that brings
// it in.
macro_rules! written_into_submodule {
    ($($name:ident => $test:expr,)*) => {$(
        run!(
            $name,
            refused("does not contain string"),
            "/test.gx" => $test,
            "/test/m.gx" => "mod k",
            "/test/m/k.gx" => "let s = {g: |x| x}; let j = s.g"
        );
    )*};
}

written_into_submodule! {
    a_submodule_a_parent_imports_and_writes_is_one_instance =>
        "mod m; use m::k; let r = &mut k::j; *r <- |x: i64| x + 1; \
         let result = (m::k::j(1), m::k::j(\"s\"))",
    a_submodule_a_parent_globs_and_writes_is_one_instance =>
        "mod m; use m::*; let r = &mut k::j; *r <- |x: i64| x + 1; \
         let result = (m::k::j(1), m::k::j(\"s\"))",
}

// A module without an interface inside one with an interface.
run!(
    a_submodule_of_an_interface_module_a_parent_writes_is_one_instance,
    refused("does not contain string"),
    "/test.gx" => "mod m; let r = &mut m::k::j; *r <- |x: i64| x + 1; \
                   let result = (m::k::j(1), m::k::j(\"s\"))",
    "/test/m.gxi" => "mod k",
    "/test/m.gx" => "mod k",
    "/test/m/k.gx" => "let f = |x| x; let j = f"
);

// And from a module beside it, by any import of it.
macro_rules! written_by_a_sibling {
    ($($name:ident => $sibling:expr,)*) => {$(
        run!(
            $name,
            refused("does not contain string"),
            "/test.gx" => "mod m; mod n; let result = (m::j(1), m::j(\"s\"))",
            "/test/m.gx" => "let s = {g: |x| x}; let j = s.g",
            "/test/n.gx" => $sibling
        );
    )*};
}

written_by_a_sibling! {
    a_module_a_sibling_globs_and_writes_is_one_instance =>
        "use super::m::*; let r = &mut j; *r <- |x: i64| x + 1",
    a_module_a_sibling_imports_and_writes_is_one_instance =>
        "use super::m; let r = &mut m::j; *r <- |x: i64| x + 1",
    a_module_a_sibling_aliases_and_writes_is_one_instance =>
        "use super::m as q; let r = &mut q::j; *r <- |x: i64| x + 1",
    a_module_a_sibling_globs_its_parent_and_writes_is_one_instance =>
        "use super::*; let r = &mut m::j; *r <- |x: i64| x + 1",
    a_module_a_sibling_writes_through_chained_globs_is_one_instance =>
        "use super::*; use array::*; use list::*; use map::*; use str::*; \
         use re::*; use rand::*; let r = &mut m::j; *r <- |x: i64| x + 1",
    a_module_a_sibling_s_block_globs_over_a_let_and_writes_is_one_instance =>
        "let j = |x: i64| x; \
         let go = { use super::m::*; let r = &mut j; *r <- |x: i64| x + 1; 1 }",
}

run!(
    a_module_s_alias_beside_one_a_sibling_writes_through_an_alias_is_generic,
    prints(r#"[i64:1, "s", i64:2]"#),
    "/test.gx" => "mod m; mod n; let result = (m::j(1), m::j(\"s\"), m::w(2))",
    "/test/m.gx" => "let s = {g: |x| x}; let j = s.g; let w = s.g",
    "/test/n.gx" => "use super::m as q; let r = &mut q::w; *r <- |x: i64| x"; FuseExpect::None
);

run!(
    a_module_s_alias_a_parent_writes_takes_its_one_type,
    prints("[i64:1, i64:2]"),
    "/test.gx" => "mod m; let r = &mut m::j; *r <- |x: i64| x; \
                   let result = (m::j(1), m::j(2))",
    "/test/m.gx" => "let s = {g: |x| x}; let j = s.g"; FuseExpect::None
);

run!(
    a_module_s_alias_beside_one_a_parent_writes_is_generic,
    prints(r#"[i64:1, "s", i64:2]"#),
    "/test.gx" => "mod m; let r = &mut m::w; *r <- |x: i64| x; \
                   let result = (m::j(1), m::j(\"s\"), m::w(2))",
    "/test/m.gx" => "let s = {g: |x| x}; let j = s.g; let w = s.g"; FuseExpect::None
);

run!(
    a_module_s_alias_beside_one_a_parent_writes_through_an_alias_is_generic,
    prints(r#"[i64:1, "s", i64:2]"#),
    "/test.gx" => "mod m; use m as q; let r = &mut q::w; *r <- |x: i64| x; \
                   let result = (m::j(1), m::j(\"s\"), m::w(2))",
    "/test/m.gx" => "let s = {g: |x| x}; let j = s.g; let w = s.g"; FuseExpect::None
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

// A written capture is one instance, which its readers and writers share,
// a lambda's read through its step's entry snapshot included.
run!(
    a_lambda_over_a_step_s_written_capture_is_its_one_instance,
    "seq { let u@ (g, k) = (|x| x, |y| y); let h = |z| u.0(z); let ru = &mut u; \
     *ru <- (|x: i64| x + 100, |y: i64| y); let b = 1; let c = 2; h(\"t\") }",
    refused("does not contain");
    FuseExpect::None
);

run!(
    a_lambda_over_a_step_s_written_capture_reads_it_at_its_step_s_entry,
    "seq { let u@ (g, k) = (|x| x, |y| y); let h = |z| u.0(z); let ru = &mut u; \
     *ru <- (|x: i64| x + 100, |y: i64| y); let b = 1; let c = 2; h(7) }",
    is(Value::I64(7));
    FuseExpect::Jit
);

run!(
    a_step_s_written_capture_is_one_instance,
    "seq { let u@ (g, k) = (|x| x, |y| y); let h = |z| u.0(z); let a = h(\"s\"); \
     let ru = &mut u; *ru <- (|x: i64| x + 100, |y: i64| y); let b = 1; let c = 2; \
     (a, h(\"t\"), u.0(1)) }",
    refused("does not contain");
    FuseExpect::None
);

run!(
    a_step_s_written_capture_is_its_write_after_it,
    "seq { let u@ (g, k) = (|x| x, |y| y); let h = |z| u.0(z); let a = h(5); \
     let ru = &mut u; *ru <- (|x: i64| x + 100, |y: i64| y); let b = 1; let c = 2; \
     (a, h(6), u.0(1)) }",
    prints("[i64:5, i64:6, i64:101]");
    FuseExpect::Jit
);

// A `seqq` request's capture of a generalized binding carries its scheme,
// as a step's entry snapshot does: nothing writes a queued capture.
run!(
    an_outer_holder_read_in_a_seqq_step_is_generic,
    "{ let s = {g: |x| x}; seqq 1 { (s.g(1), s.g(\"s\")) } }",
    prints(r#"[i64:1, "s"]"#);
    FuseExpect::Jit
);

run!(
    an_outer_holder_read_in_a_later_seqq_step_is_generic,
    "{ let s = {g: |x| x}; seqq 1 { let a = s.g(1); let b = s.g(\"s\"); (a, b) } }",
    prints(r#"[i64:1, "s"]"#);
    FuseExpect::Jit
);

run!(
    each_queued_seqq_request_reads_an_outer_holder_generically,
    "{ let s = {g: |x| x}; \
       let r = seqq let n = array::iter([1, 2]) { let a = s.g(n); let b = s.g(\"s\"); (a, b, s.g(n * 10)) }; \
       let acc: Array<(i64, string, i64)> = []; acc <- r ~ array::push(acc, r); \
       select array::len(acc) { 2 => acc, _ => never() } }",
    prints(r#"[[i64:1, "s", i64:10], [i64:2, "s", i64:20]]"#);
    FuseExpect::Jit
);

run!(
    an_outer_holder_read_in_a_flushed_seqq_is_generic,
    "{ let s = {g: |x| x}; seqq 1; flush(never(s.g(false))) { let a = s.g(1); (a, s.g(\"s\")) } }",
    prints(r#"[i64:1, "s"]"#);
    FuseExpect::Jit
);

// A capture a step writes, through a callee here, is read live: the binding
// is written, so one instance.
run!(
    a_holder_a_seqq_step_writes_is_one_instance,
    "{ let s = {g: |x| x}; let put = |v| { s <- v; 0 }; \
       seqq 1 { put({g: |x| x}); (s.g(1), s.g(\"s\")) } }",
    refused("does not contain string");
    FuseExpect::None
);

run!(
    a_holder_a_seqq_step_writes_takes_its_one_type,
    "{ let s: {g: fn(x: i64) -> i64} = {g: |x| x}; let put = |v| { s <- v; 0 }; \
       seqq 1 { put({g: |x| x + 1}); (s.g(1), s.g(2)) } }",
    prints("[i64:2, i64:3]");
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
