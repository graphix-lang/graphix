// Tests for tuples and structs

use anyhow::Result;
use arcstr::ArcStr;
use graphix_compiler::node_shape::{KernelMatcher, NodeShape};
use graphix_package_core::run;
use netidx::publisher::Value;

const TUPLES0: &str = r#"
{
  let t: (string, Number, Number) = ("foo", 42, 23.5);
  t
}
"#;

run!(tuples0, TUPLES0, |v: Result<&Value>| match v {
    Ok(Value::Array(a)) => match &a[..] {
        [Value::String(s), Value::I64(42), Value::F64(23.5)] => &*s == "foo",
        _ => false,
    },
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit;
   shape: NodeShape::contains_fused(KernelMatcher::new()));

// A composite literal with a value-shape (Duration) field. The
// `compile_and_push_field` helper-selection already routed all six
// value-shapes to the 2-register `push_value`, but the compile-dispatch
// only handled Variant|Nullable — so a Duration/DateTime/Bytes/Map field
// fell to the scalar arm, `.single()` Err'd, and the whole tuple silently
// de-fused. Now both dispatches key on `is_value_shape()`, so this JITs.
const TUPLE_DURATION_FIELD: &str = r#"
{
  let t = (duration:1.s, 2);
  t
}
"#;

run!(tuple_duration_field, TUPLE_DURATION_FIELD, |v: Result<&Value>| match v {
    Ok(Value::Array(a)) => {
        matches!(&a[..], [Value::Duration(_), Value::I64(2)])
    }
    _ => false,
});

// A COMPUTED bool as a composite field. A total-order float comparison
// lowers to `setcc`, which leaves the upper register bits dirty; the
// result was pushed straight into the struct via
// `graphix_value_buf_push_bool`, whose `i8` AbiParam carried no
// ArgumentExtension. The `extern "C" fn(v: u8)` helper is compiled under
// the C ABI's zeroext contract (it may read the full register), so
// `v != 0` saw the garbage and returned `true` for a `false` comparison —
// {x: false} node-walk vs {x: true} jit. A CONST bool folds to a clean
// `iconst`, so only computed comparisons in composites diverged. Fixed by
// uext/sext on the narrow-int helper params (helper_signature, soak jul06).
const STRUCT_COMPUTED_BOOL_FIELD: &str = r#"
{ x: f64:0.1 < f64:0.1 }
"#;

run!(struct_computed_bool_field, STRUCT_COMPUTED_BOOL_FIELD, |v: Result<&Value>| match v {
    Ok(Value::Array(a)) => matches!(
        &a[..],
        [Value::Array(f)]
            if matches!(&f[..], [Value::String(_), Value::Bool(false)])
    ),
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit;
   shape: NodeShape::contains_fused(KernelMatcher::new()));

const TUPLES1: &str = r#"
{
  let t: (string, Number, Number) = ("foo", 42, 23.5);
  let (_, y, z) = t;
  y + z
}
"#;

// ASPIRE: Jit (currently None) — blocked on: composite/value cross-kernel call args
run!(tuples1, TUPLES1, |v: Result<&Value>| match v {
    Ok(Value::F64(65.5)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

const TUPLES2: &str = r#"
{
  let t = ("foo", 42, 23.5);
  select t {
    ("foo", x, y) => x + y,
    _ => never()
  }
}
"#;

// ASPIRE: Jit (currently None) — blocked on: composite/value cross-kernel call args
run!(tuples2, TUPLES2, |v: Result<&Value>| match v {
    Ok(Value::F64(65.5)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

const TUPLEACCESSOR: &str = r#"
{
  let x = ( "bar", 42, 84.0 );
  x.1
}
"#;

run!(tupleaccessor, TUPLEACCESSOR, |v: Result<&Value>| match v {
    Ok(Value::I64(42)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit;
   shape: NodeShape::contains_fused(KernelMatcher::new()));

const STRUCTS0: &str = r#"
{
  let x = { foo: "bar", bar: 42, baz: 84.0 };
  x
}
"#;

run!(structs0, STRUCTS0, |v: Result<&Value>| match v {
    Ok(Value::Array(a)) if a.len() == 3 => match &a[..] {
        [Value::Array(f0), Value::Array(f1), Value::Array(f2)]
            if f0.len() == 2 && f1.len() == 2 && f2.len() == 2 =>
        {
            let f0 = match &f0[..] {
                [Value::String(n), Value::I64(42)] if n == "bar" => true,
                _ => false,
            };
            let f1 = match &f1[..] {
                [Value::String(n), Value::F64(84.0)] if n == "baz" => true,
                _ => false,
            };
            let f2 = match &f2[..] {
                [Value::String(n), Value::String(s)] if n == "foo" && s == "bar" => true,
                _ => false,
            };
            f0 && f1 && f2
        }
        _ => false,
    },
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit;
   shape: NodeShape::contains_fused(KernelMatcher::new()));

const BINDSTRUCT: &str = r#"
{
  let x = { foo: "bar", bar: 42, baz: 84.0 };
  let { foo: _, bar, baz } = x;
  bar + baz
}
"#;

// ASPIRE: Jit (currently None) — blocked on: composite/value cross-kernel call args
run!(bindstruct, BINDSTRUCT, |v: Result<&Value>| match v {
    Ok(Value::F64(126.0)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

const STRUCTACCESSOR: &str = r#"
{
  let x = { foo: "bar", bar: 42, baz: 84.0 };
  x.foo
}
"#;

run!(structaccessor, STRUCTACCESSOR, |v: Result<&Value>| match v {
    Ok(Value::String(s)) => s == "bar",
    _ => false,
});

const STRUCTWITH0: &str = r#"
{
  let x = { foo: "bar", bar: 42, baz: 84.0 };
  let x = { x with foo: 1 };
  x.foo
}
"#;

run!(structwith0, STRUCTWITH0, |v: Result<&Value>| match v {
    Err(_) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::None);

const STRUCTWITH1: &str = r#"
{
  let x = { foo: "bar", bar: 42, baz: 84.0 };
  let x = { x with bar: 1 };
  x.bar + x.baz
}
"#;

// `emit_struct_with_node` fuses the whole update, including copying the
// unchanged `string` field `foo` via `compile_element_read` +
// `push_field` (`graphix_struct_get_arcstr`) — the old composite-with-
// string cliff is gone.
run!(structwith1, STRUCTWITH1, |v: Result<&Value>| match v {
    Ok(Value::F64(85.0)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

const STRUCTWITH2: &str = r#"
{
  let selected = { x: 0, y: 0 };
  let y = 1;
  { selected with y }
}
"#;

// `{ selected with y }` (field shorthand) — `emit_struct_with` expands
// to a StructNew copying unchanged fields via StructGet.
run!(structwith2, STRUCTWITH2, |v: Result<&Value>| match v {
    Ok(v) => match v.clone().cast_to::<[(ArcStr, i64); 2]>() {
        Ok([(s0, 0), (s1, 1)]) if &*s0 == "x" && &*s1 == "y" => true,
        _ => false,
    },
    _ => false,
});

const STRUCTWITH3: &str = r#"
{
  let selected = { x: 0, y: 0 };
  { selected with y: selected.y + 1 }
}
"#;

// `{ selected with y: selected.y + 1 }` — the replacement reads the
// source struct (StructGet), the unchanged `x` is also copied via
// StructGet.
run!(structwith3, STRUCTWITH3, |v: Result<&Value>| match v {
    Ok(v) => match v.clone().cast_to::<[(ArcStr, i64); 2]>() {
        Ok([(s0, 0), (s1, 1)]) if &*s0 == "x" && &*s1 == "y" => true,
        _ => false,
    },
    _ => false,
});

const STRUCTWITH4: &str = r#"
{
    let selected = { x: 0, y: 0 };
    let handle = |e: [`Up, `Down, `Left, `Right]| -> `Stop select e {
        e@ `Left => {
            selected <- e ~ { selected with x: selected.x - 1 };
            `Stop
        },
        e@ `Right => {
            selected <- e ~ { selected with x: selected.x + 1 };
            `Stop
        },
        e@ `Down => {
            selected <- e ~ { selected with y: selected.y + 1 };
            `Stop
        },
        e@ `Up => {
            selected <- e ~ { selected with y: selected.y - 1 };
            `Stop
        }
    };
    handle(array::iter([`Up, `Down, `Left, `Right]));
    (array::group(selected, |n, _| n == 5))[1..]
}
"#;

// ASPIRE: Jit (currently None) — doesn't fuse its body into a
// kernel yet; the prior "fused" status was the hollow
// `result`-wrapper identity kernel (#139 identity suppression).
run!(structwith4, STRUCTWITH4, |v: Result<&Value>| match v {
    Ok(v) => match v.clone().cast_to::<[[(ArcStr, i64); 2]; 4]>() {
        Ok(
            [[(f00, 0), (f01, -1)], [(f10, 0), (f11, 0)], [(f20, -1), (f21, 0)], [(f30, 0), (f31, 0)]],
        ) if f00 == "x"
            && f01 == "y"
            && f10 == f00
            && f20 == f00
            && f30 == f00
            && f11 == f01
            && f21 == f01
            && f31 == f01 =>
            true,
        _ => false,
    },
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

const STRUCTWITH5: &str = r#"
{
    let selected = { x: 0, y: 0 };
    let handle = |e: [`Up]| -> `Stop select e {
        e@ `Up => {
            selected <- e ~ { selected with y: selected.y - 1 };
            `Stop
        }
    };
    handle(array::iter([`Up]));
    (array::group(selected, |n, _| n == 2))[1..]
}
"#;

// ASPIRE: Jit (currently None) — doesn't fuse its body into a
// kernel yet; the prior "fused" status was the hollow
// `result`-wrapper identity kernel (#139 identity suppression).
run!(structwith5, STRUCTWITH5, |v: Result<&Value>| match v {
    Ok(v) => match v.clone().cast_to::<[[(ArcStr, i64); 2]; 1]>() {
        Ok([[(f00, 0), (f01, -1)]]) if f00 == "x" && f01 == "y" => true,
        _ => false,
    },
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

// A struct-with that copies an UNCHANGED composite field (`pt`, a tuple)
// while replacing a scalar (`n`). Reads a field back so interp==jit
// agreement proves the composite copy (and its drop) is correct.
const STRUCTWITH_COMPOSITE: &str = r#"
{
  let s = { pt: (i64:1, i64:2), n: i64:0 };
  let s2 = { s with n: i64:5 };
  s2.pt.1 + s2.n
}
"#;

run!(structwith_composite, STRUCTWITH_COMPOSITE, |v: Result<&Value>| matches!(
    v,
    Ok(Value::I64(7))
));

// A `#[native]` struct-with in the differential `run!` harness: the interp
// mode (fusion off) exercises the `#[native]` `--no-fusion` no-op, the jit mode
// verifies the struct-with fuses to native — both must yield 9.
const STRUCTWITH_NATIVE: &str = r#"
#[native]
{
  let s = { x: i64:1, y: i64:2 };
  ({ s with y: i64:9 }).y
}
"#;

run!(structwith_native, STRUCTWITH_NATIVE, |v: Result<&Value>| matches!(
    v,
    Ok(Value::I64(9))
));

// A may-bottom REPLACEMENT field (`i64:10 / d`, a division) that is
// runtime-clean (`d = 2`). Exercises `emit_push_field_node`'s bottom-abort
// branch + the outer/inner buf registration on the struct-with build path,
// while still yielding a real value both modes agree on.
const STRUCTWITH_MAYBOTTOM: &str = r#"
{
  let s = { x: i64:0, y: i64:0 };
  let d = i64:2;
  ({ s with x: i64:10 / d }).x
}
"#;

run!(structwith_maybottom, STRUCTWITH_MAYBOTTOM, |v: Result<&Value>| matches!(
    v,
    Ok(Value::I64(5))
));

// ─── Composite / value-shape cross-kernel calls (#131) ───────────
//
// A top-level `let f = <lambda>` bails the enclosing Do (a lambda
// binding can't be a kernel value), so a top-level `f(args)` never
// becomes a cross-kernel call — `f` just runs as its own kernel with
// composite *params*. To exercise a real cross-kernel call
// with a non-scalar arg/return, the call must sit inside ANOTHER
// lambda's body: `g`'s kernel then contains the call to `h`.
//
// `g`'s body calls `h` with a composite (tuple) arg. #203 Phase C
// (transitive cross-kernel discovery) builds `h`'s kernel and lowers
// `g`'s `h((a,b),c)` call to a CLIF cross-kernel call, so the whole
// thing JITs — the realized #131-JIT follow-up the prior annotation
// aspired to.

const CALL_TUPLE_ARG: &str = r#"
{
  let h = |p: (i64, i64), n: i64| p.0 + p.1 + n;
  let g = |a: i64, b: i64, c: i64| h((a, b), c);
  g(10, 20, 5)
}
"#;

run!(call_tuple_arg, CALL_TUPLE_ARG, |v: Result<&Value>| match v {
    Ok(Value::I64(35)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

const CALL_STRUCT_ARG: &str = r#"
{
  let h = |s: {a: i64, b: i64}| s.a + s.b;
  let g = |x: i64, y: i64| h({a: x, b: y});
  g(3, 4)
}
"#;

// `g` calls `h` with a struct arg; #203 Phase C builds `h`'s kernel and
// lowers the cross-kernel call, so the whole body JITs.
run!(call_struct_arg, CALL_STRUCT_ARG, |v: Result<&Value>| match v {
    Ok(Value::I64(7)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

// A value-shape (nullable) RETURN from a lambda, end-to-end: `f`
// returns `[i64, null]` via a `select` with a `null` arm, and is
// called inside the result block. The block's tail type is the
// collapsed `i64 | null` primitive form; `abi_kind` now
// recognises it (kernel_abi.rs), so `infer_body_rtype`'s fast path keeps
// the region from de-fusing. Exercises #131's value-shape Call
// return through the full fusion pipeline.
const CALL_NULLABLE_RETURN: &str = r#"
{
  let f = |x: i64| select x {
    0 => null,
    n => n
  };
  f(5)
}
"#;

// ASPIRE: Jit (currently None) — doesn't fuse its body into a
// kernel yet; the prior "fused" status was the hollow
// `result`-wrapper identity kernel (#139 identity suppression).
run!(call_nullable_return, CALL_NULLABLE_RETURN, |v: Result<&Value>| match v {
    Ok(Value::I64(5)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

// ── Value-shape `==` / `!=` (the ValueEq op) ──────────────────────

// String equality — exercises the String operand of `ValueEq`
// (wrapped into `Value::String` for the comparison).
const VALUE_EQ_STRING: &str = r#"
{
  let s = "hello";
  s == "hello"
}
"#;

run!(value_eq_string, VALUE_EQ_STRING, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
});

const VALUE_EQ_STRING_NE: &str = r#"
{
  let s = "hello";
  s != "world"
}
"#;

run!(value_eq_string_ne, VALUE_EQ_STRING_NE, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
});

// Composite (tuple) equality — exercises the composite operand of
// `ValueEq` (wrapped into `Value::Array`); lhs is a Borrowed local
// read (clone), rhs an owned `TupleNew`.
const VALUE_EQ_TUPLE: &str = r#"
{
  let t = (1, 2);
  t == (1, 2)
}
"#;

run!(value_eq_tuple, VALUE_EQ_TUPLE, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
});

// Out-of-range tuple index must be a TYPE ERROR, not a compiler panic
// (TupleRef::typecheck0 indexed the field list unchecked — the panic
// killed the runtime worker, and the LSP with it).
const TUPLE_INDEX_OOB: &str = r#"
{
  let t = (1, 2);
  t.5
}
"#;

run!(tuple_index_oob, TUPLE_INDEX_OOB, |v: Result<&Value>| matches!(v, Err(_));
    graphix_package_core::testing::FuseExpect::None);

// `{src with f}` where the source type sits behind TVars and the
// replacement recursively typechecks a select over the SAME struct —
// StructWith::typecheck0 used to run the recursion inside with_deref's
// read guards, deadlocking the compiler on a single thread.
const STRUCT_WITH_SELECT_OVER_SOURCE: &str = r#"
{
  let g = |v: {x: i64, y: i64}| v;
  let t = g({x: 1, y: 2});
  {t with x: select t { {x, y} => x + y }}
}
"#;

run!(struct_with_select_over_source, STRUCT_WITH_SELECT_OVER_SOURCE, |v: Result<
    &Value,
>| match v {
    Ok(Value::Array(flds)) => match &flds[..] {
        [Value::Array(x), Value::Array(y)] => {
            matches!(&x[..], [Value::String(n), Value::I64(3)] if &**n == "x")
                && matches!(&y[..], [Value::String(n), Value::I64(2)] if &**n == "y")
        }
        _ => false,
    },
    _ => false,
});
