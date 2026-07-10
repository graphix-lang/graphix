//! The `#[native]` attribute (Part C3). The decorated expression must compile
//! to native code — one fused JIT kernel with zero node-walk residue — else it
//! is a compile error. It may only decorate a value-producing computation or a
//! call; a function definition (or any function-typed target) is rejected,
//! because a `native` requirement on a function value would be infectious and
//! brittle (it could never be stored, dynamically dispatched, or passed to a
//! non-fusing HOF) — a performance requirement belongs at the use site.
//!
//! These are compile-time assertions, so they are tested directly via `eval`
//! (which runs with fusion on) rather than the `run!` differential harness:
//! `#[native]` is deliberately mode-dependent (it cannot be verified under
//! `--no-fusion`), so the node-walk-vs-jit value-agreement harness doesn't
//! apply.

use graphix_package_core::testing::eval;
use netidx::subscriber::Value;

// A pure computation that fully fuses → `#[native]` is satisfied, and the
// program still produces its value.
#[tokio::test]
async fn native_fusable_ok() {
    let r =
        eval("#[native]\n{ let x = i64:3; x * x + i64:1 }", crate::TEST_REGISTER).await;
    assert!(
        matches!(r.as_ref().map(|(v, _)| v), Ok(Value::I64(10))),
        "expected a fused native computation to compile and yield 10, got {:?}",
        r.map(|(v, _)| v)
    );
}

// `#[native]` on a bare lambda literal — a function definition — is an error.
#[tokio::test]
async fn native_on_lambda_literal_is_error() {
    let r = eval("#[native]\n|x: i64| x + i64:1", crate::TEST_REGISTER).await;
    assert!(
        r.is_err(),
        "#[native] on a function literal must be a compile error, got {:?}",
        r.map(|(v, _)| v)
    );
}

// `#[native]` on a function binding — also a function definition — is an error.
#[tokio::test]
async fn native_on_lambda_binding_is_error() {
    let r =
        eval("{ #[native]\nlet f = |x: i64| x + i64:1; f(i64:2) }", crate::TEST_REGISTER)
            .await;
    assert!(
        r.is_err(),
        "#[native] on a function binding must be a compile error, got {:?}",
        r.map(|(v, _)| v)
    );
}

// `#[native]` on an async computation that cannot fuse is an error (this is
// the teeth of the attribute — it forbids exactly the constructs that
// de-fuse). `once` is classified async, so it node-walks.
#[tokio::test]
async fn native_on_unfusable_is_error() {
    let r = eval("#[native]\nonce(i64:5)", crate::TEST_REGISTER).await;
    assert!(
        r.is_err(),
        "#[native] on an async (non-fusing) expr must be a compile error, got {:?}",
        r.map(|(v, _)| v)
    );
}

// An unregistered attribute name is a compile error, independent of fusion.
#[tokio::test]
async fn unknown_attribute_is_error() {
    let r = eval("#[bogus]\ni64:1", crate::TEST_REGISTER).await;
    assert!(
        r.is_err(),
        "an unknown attribute must be a compile error, got {:?}",
        r.map(|(v, _)| v)
    );
}

// `#[native]` INSIDE an HOF callback body — the case that used to pass
// vacuously (the attribute checker stopped at lambda bodies). `list::map`
// over a recursive List doesn't batch-loop inline, so its callback fuses
// per-element; a wholly-sync callback fully fuses, so `#[native]` on its
// body is satisfied and the program produces its value.
#[tokio::test]
async fn native_hof_callback_fusable_ok() {
    let prog = "list::to_array(list::map(list::from_array([1, 2, 3]), \
                |x| #[native] { let y = x * 2; y + 1 }))";
    let r = eval(prog, crate::TEST_REGISTER).await;
    assert!(
        r.is_ok(),
        "#[native] on a fully-fusing HOF callback body must compile, got {:?}",
        r.map(|(v, _)| v)
    );
}

// #203 (now FIXED): an `array::init` callback that calls a recursive
// lambda defined in an EARLIER top-level statement fully fuses. The callee
// resolves across statements (batch-scoped `bind_to_lambda`), its kernel is
// discovered and built (recursive tail-loop), and the callback→callee call
// lowers to a cross-kernel call — so `#[native]` on the callback body is
// satisfied. (Before #203 the call node-walked and this was a compile
// error; this is the miniature of `bench/mandelbrot.gx`.)
#[tokio::test]
async fn native_hof_callback_recursive_call_fuses_ok() {
    let prog = "{ \
                let rec f = |n: i64| -> i64 select n { 0 => 0, _ => f(n - 1) }; \
                array::init(4, |idx| #[native] { let a = idx * 2; f(a) }) \
                }";
    let r = eval(prog, crate::TEST_REGISTER).await;
    assert!(
        r.is_ok(),
        "#[native] on a callback that calls a recursive lambda must compile now \
         that nested cross-statement calls fuse (#203), got {:?}",
        r.map(|(v, _)| v)
    );
}

// P4: an in-language HOF call site DISPATCHES by node-walk (fn-typed
// args have no ABI) while the loop fuses inside the per-site instance,
// so `#[native]` on the call is now a compile error. ASPIRE: inline
// the site-monomorphic instance body into the enclosing region and
// flip this back to is_ok.
#[tokio::test]
async fn native_inlining_hof_callsite_ok() {
    let r =
        eval("#[native]\narray::init(3, |i| i * i + i64:1)", crate::TEST_REGISTER).await;
    assert!(
        r.is_err(),
        "#[native] on an in-language HOF call must report the node-walk \
         dispatch, got {:?}",
        r.map(|(v, _)| v)
    );
}

// `#[native]` inside a `list::fold` callback — FoldQ fuses its `(acc, elem)`
// callback per-element through the per-slot template, so a wholly-sync
// callback body is native.
#[tokio::test]
async fn native_fold_callback_fusable_ok() {
    let prog = "list::fold(list::from_array([1, 2, 3]), 0, \
                |acc, x| #[native] { let s = x + acc; s + i64:0 })";
    let r = eval(prog, crate::TEST_REGISTER).await;
    assert!(
        r.is_ok(),
        "#[native] on a fully-fusing fold callback body must compile, got {:?}",
        r.map(|(v, _)| v)
    );
}

// Stage 2 — `#[native]` on a computation that calls a transitively-defined
// callee whose BODY contains a sync DynCall (`cast<i64>` lowers to the cast
// machinery). The callee `g` fuses as a cross-kernel FuncId and its cast
// dispatches through the region-wide combined `dyn_slots` table — so the whole
// decorated computation is native (the `let g` binding itself node-walks, but
// it isn't part of the decorated expr). Before Stage 2 the callee's cast
// de-fused and this was a compile error.
#[tokio::test]
async fn native_transitive_callee_dyncall_ok() {
    let prog = "{ \
                let g = |b: bool| cast<i64>(b)$; \
                #[native] (g(true) + g(false)) \
                }";
    let r = eval(prog, crate::TEST_REGISTER).await;
    assert!(
        r.is_ok(),
        "#[native] on a computation calling a callee with a body DynCall must \
         compile now that Stage 2 delivers transitive-callee DynCalls, got {:?}",
        r.map(|(v, _)| v)
    );
}

// `#[native]` on a `{ s with f: v }` struct update — Phase 1 gave StructWith
// an `emit_clif` (build a new struct, copying unchanged fields from the source
// via `compile_element_read`, overriding the replaced ones). An all-scalar
// struct-with over a fused source struct fully fuses into one kernel.
#[tokio::test]
async fn native_structwith_ok() {
    let prog =
        "#[native]\n{ let s = { x: i64:1, y: i64:2, z: i64:3 }; { s with y: i64:10 } }";
    let r = eval(prog, crate::TEST_REGISTER).await;
    assert!(
        r.is_ok(),
        "#[native] on a scalar struct-with must compile now that StructWith has \
         an emit_clif, got {:?}",
        r.map(|(v, _)| v)
    );
}

// A struct-with whose source struct has a STRING field copied UNCHANGED — the
// old "composite-with-string cliff". Phase 1 copies it via
// `compile_element_read` + `push_field` (`graphix_struct_get_arcstr`), so the
// whole update fuses.
#[tokio::test]
async fn native_structwith_string_field_ok() {
    let prog = "#[native]\n{ let s = { name: \"x\", n: i64:1 }; { s with n: i64:2 } }";
    let r = eval(prog, crate::TEST_REGISTER).await;
    assert!(
        r.is_ok(),
        "#[native] on a struct-with copying an unchanged string field must \
         compile, got {:?}",
        r.map(|(v, _)| v)
    );
}

// A non-scalar `connect` (Phase 2): a composite RHS marshaled to an owned
// Value and handed to `set_var`. The target `last` is an external capture
// WRITTEN (not read) inside the map callback, so the connect fuses (the
// read-after-write guard doesn't fire) and the struct literal is marshaled via
// `emit_owned_value_operand_node`. Before Phase 2 the non-scalar RHS de-fused.
#[tokio::test]
async fn native_connect_composite_rhs_ok() {
    let prog = "{ let last = { v: i64:0 }; \
                array::map([1, 2, 3], |x| #[native] { last <- { v: x }; x }); \
                last.v }";
    let r = eval(prog, crate::TEST_REGISTER).await;
    assert!(
        r.is_ok(),
        "a non-scalar connect in a fused callback must compile now that \
         emit_connect_node marshals any shape, got {:?}",
        r.map(|(v, _)| v)
    );
}

// String elements in an array HOF: under P4 the map call site
// dispatches by node-walk AND the instance's For de-fuses (the
// cross-kernel call can't marshal the string formal yet), so
// `#[native]` errors. ASPIRE: instance-body inlining (or string arg
// marshaling) restores this to is_ok.
#[tokio::test]
async fn native_hof_string_element_ok() {
    let prog = "#[native]\narray::map([\"a\", \"bb\", \"ccc\"], |s| str::len(s))";
    let r = eval(prog, crate::TEST_REGISTER).await;
    assert!(
        r.is_err(),
        "a string-element map's call site node-walks under P4, got {:?}",
        r.map(|(v, _)| v)
    );
}

// `select` structural destructuring (Phase 4): a tuple pattern over a
// borrowed scrutinee fuses — the arm's length test + scalar leaf reads
// compile into the kernel.
#[tokio::test]
async fn native_select_destructure_ok() {
    let prog = "#[native]\n{ let t = (3, 4); select t { (0, y) => y, (x, y) => x + y } }";
    let r = eval(prog, crate::TEST_REGISTER).await;
    assert!(
        r.is_ok(),
        "a tuple-destructuring select must fully fuse now, got {:?}",
        r.map(|(v, _)| v)
    );
}

// NESTED structural select patterns (Phase 5): the intermediate composite
// reads are borrowed interior pointers staged behind each level's length
// test — a nested slice inside a TUPLE fully fuses.
#[tokio::test]
async fn native_select_nested_tuple_ok() {
    let prog = "{ let t = ([1.0, 2.0], 42); \
                #[native] select t { ([a, b], c) => a + b, _ => 0.0 } }";
    let r = eval(prog, crate::TEST_REGISTER).await;
    assert!(
        r.is_ok(),
        "a nested-slice-in-tuple select must fully fuse now, got {:?}",
        r.map(|(v, _)| v)
    );
}

// The STRUCT-parent nested case (the nestedmatch3 shape) — flipped from a
// de-fuse pin: the blocker was `_` inferring as `Type::Any`
// (`infer_type_predicate`), which made select's unification-by-contains
// walk short-circuit at the `_` slot (`T.contains(Any)` = false), leaving
// every LATER slot's bind TVars un-narrowed. `_` now infers a fresh TVar
// like an anonymous bind, so the whole select fuses.
#[tokio::test]
async fn native_select_nested_struct_ok() {
    let prog = "{ let x = { foo: [1.0, 2.0, 4.5], bar: 42, baz: 8.0 }; \
                #[native] select x { \
                { foo: [a, b, ..], bar: _, baz: _ } => a + b, _ => 0.0 } }";
    let r = eval(prog, crate::TEST_REGISTER).await;
    assert!(
        r.is_ok(),
        "the struct-nested select must fully fuse now that `_` infers a \
         fresh TVar, got {:?}",
        r.map(|(v, _)| v)
    );
}

// A FLOAT-result select with a CONDITIONAL final arm (a composite pattern's
// length test makes final arms conditional) exercises the miss trap's
// zero constant — which must be `f64const`, not `iconst.f64` (invalid CLIF:
// a cranelift verifier PANIC that killed the check runtime).
#[tokio::test]
async fn native_select_float_conditional_final_ok() {
    let prog = "{ let x = { bar: 42, baz: 8.0 }; \
                #[native] select x { \
                { bar: 0, baz } => baz, { bar: _, baz: _ } => 0.0 } }";
    let r = eval(prog, crate::TEST_REGISTER).await;
    assert!(
        r.is_ok(),
        "a float-result select with a conditional final arm must fuse \
         (and not panic the verifier), got {:?}",
        r.map(|(v, _)| v)
    );
}

// HOF destructure leaves: under P4 the `|(pt, n)|` destructured
// formal has no single BindId, so the callee kernel can't bind its
// leaves and the call site node-walks — `#[native]` errors. ASPIRE:
// instance-body inlining (leaf binds off the composite arg, the old
// Phase 5 machinery's shape) restores is_ok.
#[tokio::test]
async fn native_hof_composite_leaf_ok() {
    let prog = "#[native]\narray::map([((1, 2), 10), ((3, 4), 20)], |(pt, n)| pt.0 + n)";
    let r = eval(prog, crate::TEST_REGISTER).await;
    assert!(
        r.is_err(),
        "a destructured-formal map's call site node-walks under P4, got {:?}",
        r.map(|(v, _)| v)
    );
}

// ...and the teeth for the DEFERRED case: a NAMED rest binding
// (`[x, rest..]`) allocates an owned subslice arm local (JitEnv::truncate
// emits no drops), so that select still de-fuses — `#[native]` on it must
// be a compile error until that lands.
#[tokio::test]
async fn native_select_named_rest_defuses() {
    let prog = "{ let a = [1, 2, 3]; \
                #[native] select a { [x, rest..] => x + array::len(rest), _ => 0 } }";
    let r = eval(prog, crate::TEST_REGISTER).await;
    assert!(
        r.is_err(),
        "a named-rest select must still de-fuse (owned subslice arm local), got {:?}",
        r.map(|(v, _)| v)
    );
}

// The blocker LIST must be clean: a callback whose arithmetic fuses but
// whose call node-walks should report the CALL ("builtin call site not
// discovered"), NOT the structural `let`s ("node does not emit CLIF")
// whose values fused — the `fused_ids` filter suppresses those. `once` is
// classified async (a permanent fusion boundary), so its call is the
// stable non-fuser here (a recursive lambda call now fuses, #203, so it
// can no longer play this role); the `let a` value still fuses, exercising
// the filter.
#[tokio::test]
async fn native_blocker_list_is_filtered() {
    let prog = "array::init(4, |idx| #[native] { let a = idx * 2; once(a) })";
    let e =
        eval(prog, crate::TEST_REGISTER).await.err().expect("must be a compile error");
    // `{:#}` includes anyhow's full cause chain (the `#[native]` blocker
    // detail is a CAUSE, not the top-level context).
    let err = format!("{e:#}");
    assert!(
        err.contains("builtin call site") && err.contains("not discovered"),
        "should report the real call blocker, got: {err}"
    );
    assert!(
        !err.contains("does not emit CLIF"),
        "structural `let` noise (whose values fused) must be filtered out, got: {err}"
    );
}
