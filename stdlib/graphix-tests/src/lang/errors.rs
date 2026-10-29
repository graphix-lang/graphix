// Tests for try/catch and error handling

use anyhow::Result;
use graphix_package_core::{
    run,
    testing::{FuseExpect, Mode, is, prints, refused},
};
use netidx::publisher::Value;

const UNCHECKED0: &str = r#"
2 + 2
"#;

run!(unchecked0, UNCHECKED0, is(Value::I64(4)); FuseExpect::Jit);

const CHECKED0: &str = r#"
2 +? 2
"#;

run!(checked0, CHECKED0, is(Value::I64(4)); FuseExpect::Jit);

const CHECKED_DIV0: &str = r#"
{
    let res = never();
    catch(e) select (e.0).error {
        `ArithError(s) => res <- s
    };
    (0 /? 0)?;
    res
}
"#;

run!(checked_div0, CHECKED_DIV0, |v: Result<&Value>| match v {
    Ok(Value::String(s)) => s == "attempt to divide by zero",
    _ => false,
});

// Checked decimal arithmetic names each cause, as an integer's does.
run!(
    checked_decimal_names_its_cause,
    r#"{
    let f = |n: i64| {
        let d = cast<decimal>(n)$;
        let max = decimal:79228162514264337593543950335.0;
        (max +? d, d /? decimal:0.0, d %? decimal:0.0, max *? d)
    };
    f(3)
}"#,
    prints(
        r#"[error:["ArithError", "attempt to add with overflow"], error:["ArithError", "attempt to divide by zero"], error:["ArithError", "attempt to calculate the remainder with a divisor of zero"], error:["ArithError", "attempt to multiply with overflow"]]"#
    )
);

// `x % -1` is 0 for every x, MIN included, checked or not; `%` fails
// only on a zero divisor. The divisor is a parameter, so the kernel
// guards it at run time.
const REM_BY_NEG_ONE_IS_ZERO: &str = r#"
{
    let rem = |a: i64, b: i64| -> i64 a % b;
    let rem8 = |a: i8, b: i8| -> i8 a % b;
    let mn = -9223372036854775807 - 1;
    let n1 = -1;
    (#[native] rem(mn, n1), #[native] rem(7, n1), #[native] rem8(i8:-128, i8:-1), (mn %? n1)$)
}
"#;

run!(rem_by_neg_one_is_zero, REM_BY_NEG_ONE_IS_ZERO, |v: Result<&Value>| match v {
    Ok(Value::Array(a)) => {
        matches!(&a[..], [Value::I64(0), Value::I64(0), Value::I8(0), Value::I64(0)])
    }
    _ => false,
}; FuseExpect::Jit);

// Checked float arithmetic follows IEEE and never fails: an infinity,
// a NaN, never an error.
run!(
    checked_float_follows_ieee,
    r#"{
    let f = |x: f64, y: f64| (x +? y, x -? y, x *? y, x /? y, x %? y);
    let g = |x: f32, y: f32| (x +? y, x *? y, x /? y, x %? y);
    (#[native] f(1.0, 0.0), #[native] f(1e308, 1e308), #[native] f(7.5, -2.0), #[native] g(f32:1.0, f32:0.0))
}"#,
    prints(
        "[[f64:1., f64:1., f64:0., f64:inf, f64:NaN], [f64:inf, f64:0., f64:inf, f64:1., f64:0.], \
         [f64:5.5, f64:9.5, f64:-15., f64:-3.75, f64:1.5], [f32:1., f32:0., f32:inf, f32:NaN]]"
    );
    FuseExpect::Jit
);

// Each checked failure names its cause.
const CHECKED_FAILURES_NAME_THEIR_CAUSE: &str = r#"
{
    let mx = 9223372036854775807;
    let mn = -9223372036854775807 - 1;
    let z = 0;
    [mx +? 1, mn -? 1, mx *? 2, 1 /? z, mn /? -1, 1 %? z]
}
"#;

run!(
    checked_failures_name_their_cause,
    CHECKED_FAILURES_NAME_THEIR_CAUSE,
    |v: Result<&Value>| {
        let cause = |c: &str| format!(r#"error:["ArithError", "attempt to {c}"]"#);
        let want = [
            "add with overflow",
            "subtract with overflow",
            "multiply with overflow",
            "divide by zero",
            "divide with overflow",
            "calculate the remainder with a divisor of zero",
        ]
        .map(cause)
        .join(", ");
        format!("{}", v.unwrap()) == format!("[{want}]")
    }
);

// A handler-ful `?` in a fused region raises its error onto the
// invocation's queue and the handler counts it.
const CATCH_ARRAY_INDEX_FUSED: &str = r#"
{
    let caught = never();
    catch(e) caught <- e ~ 1;
    let a = [10, 20, 30];
    let v = a[5]? * 2;
    caught
}
"#;

run!(catch_array_index_fused, CATCH_ARRAY_INDEX_FUSED, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(1)))
});

const CATCH1: &str = r#"
{
    catch(e) select (e.0).error {
        `ArrayIndexError(s) => { println("array index error [s]"); -1 }
    };
    let a = [1, 2, 3];
    a[0]? + a[1]?
}
"#;

run!(catch1, CATCH1, is(Value::I64(3)); FuseExpect::Jit);

// Nested catches in one block: the second covers the ?s below it; its
// handler's rethrow resolves to the first, seen the same cycle.
const CATCH4: &str = r#"
{
    let a = [0, 1, 2, 3, 4, 5];
    let err0: Error<ErrChain<[`ArithError(string), `ArrayIndexError(string)]>> = never();
    let err1: Error<ErrChain<[`ArithError(string), `ArrayIndexError(string)]>> = never();
    catch(e) err0 <- e;
    catch(e) select (e.0).error {
       `ArithError(_) => err1 <- e,
       _ => e?
    };
    (a[5]? /? a[0]?)?;
    a[6]?;
    ((err0.0).error, (err1.0).error, count(err0), count(err1))
}
"#;

run!(catch4, CATCH4, prints(r#"[["ArrayIndexError", "array index out of bounds"], ["ArithError", "attempt to divide by zero"], i64:1, i64:1]"#); FuseExpect::Jit);

const CHECKED_DOLLAR: &str = r#"
{
    let x = (0 /? 0)$;
    any(x, 2 + 2)
}
"#;

run!(checked_dollar, CHECKED_DOLLAR, is(Value::I64(4)); FuseExpect::Jit);

// Composite-success `$`.
const COMPOSITE_DOLLAR: &str = r#"
{
    let a = [1, 2, 3];
    a[1..]$
}
"#;

run!(composite_dollar, COMPOSITE_DOLLAR, |v: Result<&Value>| match v {
    Ok(Value::Array(a)) => &**a == &[Value::I64(2), Value::I64(3)],
    _ => false,
}; FuseExpect::Jit);

// The borrowed-inner variant: the qop inner is a read of a nullable
// local.
const COMPOSITE_DOLLAR_BORROWED: &str = r#"
{
    let a = [1, 2, 3];
    let x = a[1..];
    x$
}
"#;

run!(
    composite_dollar_borrowed,
    COMPOSITE_DOLLAR_BORROWED,
    |v: Result<&Value>| match v {
        Ok(Value::Array(a)) => &**a == &[Value::I64(2), Value::I64(3)],
        _ => false,
    };
    FuseExpect::Jit
);

// A `$` over a nested fallible union strips every error member; the
// timer delays the element so the init cycle sees no value.
const QOP_NESTED_UNION_STRING: &str = r#"
{
  let s: [string, Error<`E(string)>] = {
    let tm = sys::time::timer(duration:0.05s, false);
    tm ~ "hello"
  };
  let t = [s];
  t[i64:0]$
}
"#;

run!(qop_nested_union_string, QOP_NESTED_UNION_STRING, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "hello")
}; FuseExpect::Jit);

// A Sync builtin that produces no value bound to an unconsumed local:
// the block's unrelated result still emits. (The logged error each fire
// is expected.)
const DYNCALL_PENDING_UNCONSUMED: &str = r#"
{
  let b = buffer::encode([`Pad(u64:18446744073709551615)]);
  f64:0.
}
"#;

run!(dyncall_pending_unconsumed, DYNCALL_PENDING_UNCONSUMED, |v: Result<&Value>| {
    matches!(v, Ok(Value::F64(f)) if *f == 0.)
}; FuseExpect::Jit);

// The same in statement position: the statement's bottom is discarded.
const DYNCALL_PENDING_STATEMENT: &str = r#"
{
  buffer::encode([`Pad(u64:18446744073709551615)]);
  7
}
"#;

run!(dyncall_pending_statement, DYNCALL_PENDING_STATEMENT, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(7)))
}; FuseExpect::Jit);

// The bottom value consumed by `buffer::len`, whose result is itself
// unconsumed: the tuple still emits.
const DYNCALL_PENDING_CONSUMED_LOCAL: &str = r#"
{
  let b = buffer::encode([`Pad(u64:18446744073709551615)]);
  let n = buffer::len(b);
  (1, 2).0
}
"#;

run!(
    dyncall_pending_consumed_local,
    DYNCALL_PENDING_CONSUMED_LOCAL,
    is(Value::I64(1));
    FuseExpect::Jit
);

// The bottom inside a cross-kernel callee: the caller still emits 5.
const DYNCALL_PENDING_CALLEE: &str = r#"
{
  let f = |n: u64| buffer::len(buffer::encode([`Pad(n)]));
  let x = f(u64:18446744073709551615);
  5
}
"#;

run!(dyncall_pending_callee, DYNCALL_PENDING_CALLEE, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(5)))
}; FuseExpect::Jit);

// The bottom inside a HOF callback slot: the map local bottoms while the
// block's unrelated output still emits.
const DYNCALL_PENDING_HOF_SLOT: &str = r#"
{
  let m = array::map([u64:18446744073709551615], |n| buffer::encode([`Pad(n)]));
  9
}
"#;

run!(dyncall_pending_hof_slot, DYNCALL_PENDING_HOF_SLOT, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(9)))
}; FuseExpect::Jit);

// A handler-ful `?` that succeeds inside a fusable region fuses.
const QOP_HANDLER_DELIVER: &str = r#"
{
  let caught = i64:0;
  catch(e) caught <- e ~ i64:1;
  let a = [i64:10, i64:20];
  a[i64:0]? + i64:100
}
"#;

run!(qop_handler_deliver, QOP_HANDLER_DELIVER, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(110)))
}; FuseExpect::Jit);

// A lambda whose select merges a scalar arm with an error arm, called so
// the error arm is taken: the return is the union [i64, Error<f64>].
const ERROR_ARM_LAMBDA_RETURN: &str = r#"
{
  let f = |n: i64| select n {i64:0 => i64:0, _ => error(f64:0.)};
  f(i64:1)
}
"#;

run!(error_arm_lambda_return, ERROR_ARM_LAMBDA_RETURN, |v: Result<&Value>| {
    matches!(v, Ok(Value::Error(e)) if matches!(&**e, Value::F64(f) if *f == 0.0))
}; FuseExpect::Jit);

// A catch covers only the statements below it in its block. The
// handler's index-only select proves it by exhaustiveness.
const CATCH_POSITIONAL: &str = r#"
{
    let a = [1, 2, 3];
    let tag = "";
    let above = (i64:0 /? i64:0)?;
    catch(e) tag <- select (e.0).error { `ArrayIndexError(_) => "index" };
    let below = a[10]?;
    select tag { "" => never(""), t => t }
}
"#;

run!(catch_positional, CATCH_POSITIONAL, |v: Result<&Value>| matches!(
    v,
    Ok(Value::String(s)) if &**s == "index"
); FuseExpect::Jit);

// A catch inside a nested block covers only that block.
const CATCH_BLOCK_SCOPE: &str = r#"
{
    let a = [1];
    let tag = "";
    let inner = {
        catch(e) tag <- select (e.0).error { `ArrayIndexError(_) => "index" };
        any(a[5]?, 0)
    };
    let outer = (i64:0 /? i64:0)?;
    select tag { "" => never(""), t => t }
}
"#;

run!(catch_block_scope, CATCH_BLOCK_SCOPE, |v: Result<&Value>| matches!(
    v,
    Ok(Value::String(s)) if &**s == "index"
); FuseExpect::Jit);

// A catch covering a lambda body's `?` keeps those errors out of the
// lambda's inferred throws.
const CATCH_IN_LAMBDA_THROWS: &str = r#"
{
    let err0: Error<Any> = never();
    let f = |i: i64| {
        catch(e) err0 <- e;
        let a = [1, 2, 3];
        any(a[i]?, 0 - 1)
    };
    let g: fn(i: i64) -> i64 = f;
    (g(0), g(9)).0
}
"#;

run!(catch_in_lambda_throws, CATCH_IN_LAMBDA_THROWS, is(Value::I64(1)); FuseExpect::Jit);

// Without the catch the lambda throws, and the ascription refuses it.
const UNCAUGHT_IN_LAMBDA_THROWS: &str = r#"
{
    let f = |i: i64| {
        let a = [1, 2, 3];
        any(a[i]?, 0 - 1)
    };
    let g: fn(i: i64) -> i64 = f;
    (g(0), g(9)).0
}
"#;

run!(
    uncaught_in_lambda_throws,
    UNCAUGHT_IN_LAMBDA_THROWS,
    graphix_package_core::testing::refused("throws Error<ErrChain<`ArrayIndexError(string)>>");
    FuseExpect::None
);

/// A catch installed by one `GXHandle::compile` input covers later
/// inputs; the `?`'s delivery crosses tops via `set_var`.
#[tokio::test(flavor = "current_thread")]
async fn catch_repl_cross_input() -> anyhow::Result<()> {
    use arcstr::ArcStr;
    use graphix_package_core::testing::init;
    use graphix_rt::GXEvent;
    use tokio::sync::mpsc;
    let (tx, mut rx) = mpsc::channel(1000);
    let ctx = init(tx, crate::TEST_REGISTER).await?;
    // CompRes deletes its graph on Drop; hold every input's handle.
    let _i1 = ctx.rt.compile(ArcStr::from("let tag = \"\"")).await?;
    let _i2 = ctx
        .rt
        .compile(ArcStr::from(
            "catch(e: Error<ErrChain<`ArrayIndexError(string)>>) tag <- e ~ \"caught\"",
        ))
        .await?;
    let _i3 = ctx.rt.compile(ArcStr::from("let a = [1]")).await?;
    let _i4 = ctx.rt.compile(ArcStr::from("a[9]?")).await?;
    let probe = ctx
        .rt
        .compile(ArcStr::from("select tag { \"\" => never(\"\"), t => t }"))
        .await?;
    let id = probe.exprs[0].id;
    // The probe emits on its init read or on tag's flip.
    let deadline = tokio::time::Instant::now() + std::time::Duration::from_secs(10);
    loop {
        let batch = tokio::time::timeout_at(deadline, rx.recv()).await;
        let Ok(Some(batch)) = batch else { anyhow::bail!("tag never became \"caught\"") };
        for ev in batch.iter() {
            if let GXEvent::Updated(eid, netidx_value::Value::String(s)) = ev {
                if *eid == id && &**s == "caught" {
                    return Ok(());
                }
            }
        }
    }
}

// A handler installed inside a recursive body belongs to its
// activation.
const CATCH_PER_ACTIVATION: &str = r#"
{
    let rec f = |n: i64| -> i64 select n {
        0 => 0,
        _ => {
            let hit = never();
            catch(e) hit <- e ~ n;
            error(`Boom)?;
            hit + f(n - 1)
        }
    };
    f(3)
}
"#;

run!(catch_per_activation, CATCH_PER_ACTIVATION, is(Value::I64(6)); FuseExpect::None);

// A callee's handler covers the callee's body only.
const CATCH_IN_CALLEE_STAYS_IN_CALLEE: &str = r#"
{
    let outer = never();
    catch(e) outer <- e ~ 1;
    let g = || {
        let inner = never();
        catch(e) inner <- e ~ 10;
        error(`A)?;
        inner
    };
    let v = g();
    error(`B)?;
    v + outer
}
"#;

run!(catch_in_callee_stays_in_callee, CATCH_IN_CALLEE_STAYS_IN_CALLEE, is(Value::I64(11)); FuseExpect::Jit);

// A `?` in a body that installs no handler reaches the caller's.
const CATCH_THROUGH_CALL: &str = r#"
{
    let caught = never();
    catch(e) caught <- e ~ 1;
    let f = |x: i64| -> i64 {
        error(`A)?;
        x
    };
    let v = f(5);
    v + caught
}
"#;

run!(catch_through_call, CATCH_THROUGH_CALL, is(Value::I64(6)); FuseExpect::Jit);

// A `?` in an inline collection callback is baked into the calling
// kernel, so two instances of `g` under different catches are two kernels.
const CATCH_IN_INLINE_CALLBACK_PER_INSTANCE: &str = r#"
{
    let h = |x: [i64, Error<`E(string)>]| -> i64 x?;
    let g = |a: Array<[i64, Error<`E(string)>]>| -> Array<i64> array::map(a, h);
    let a1: Array<[i64, Error<`E(string)>]> = [1, error(`E("one"))];
    let a2: Array<[i64, Error<`E(string)>]> = [2, error(`E("two"))];
    let got1 = never();
    let got2 = never();
    { catch(e) got1 <- e ~ "h1"; g(a1) };
    { catch(e) got2 <- e ~ "h2"; g(a2) };
    (got1, got2)
}
"#;

run!(
    catch_in_inline_callback_per_instance,
    CATCH_IN_INLINE_CALLBACK_PER_INSTANCE,
    prints(r#"["h1", "h2"]"#)
);

// A connect of the catch variable into a binding over a different
// error union is a type mismatch; the timeout turns a hang into a
// failure.
#[tokio::test]
async fn catch_connect_union_mismatch_is_an_error() -> Result<()> {
    let src = "{let err1: Error<ErrChain<[`ArithError(string)]>> = never(); \
               catch(e) err1 <- e; error(`B)?; 1}";
    let r = tokio::time::timeout(
        std::time::Duration::from_secs(120),
        graphix_package_core::testing::eval(src, crate::TEST_REGISTER),
    )
    .await
    .map_err(|_| anyhow::anyhow!("the mismatch check did not terminate"))?;
    let e = match r {
        Ok((v, _)) => anyhow::bail!("expected a type mismatch, got {v}"),
        Err(e) => format!("{e:#}"),
    };
    assert!(
        e.contains(
            "err1 is Error<ErrChain<`ArithError(string)>> and cannot hold \
             Error<ErrChain<`B>>; declare err1: [Error<ErrChain<`ArithError(string)>>, \
             Error<ErrChain<`B>>] where it is bound"
        ),
        "{e}"
    );
    Ok(())
}

// `catch(e: T)` ascribes `T` to `e`; a `?`-rethrowing handler typechecks
// even when the covered region throws nothing.
const CATCH_ASCRIPTION_RETHROW_NO_THROW: &str = r#"
{
  catch(e: Error<Any>) e?;
  7
}
"#;

run!(catch_ascription_rethrow_no_throw, CATCH_ASCRIPTION_RETHROW_NO_THROW, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(7)))
}; FuseExpect::None);

// An ascribed rethrow still delivers to the enclosing catch.
const CATCH_ASCRIPTION_RETHROW: &str = r#"
{
  let caught = never();
  catch(e) caught <- e ~ 1;
  {
    catch(e: Error<Any>) e?;
    [0][1]?;
    99
  };
  caught
}
"#;

run!(catch_ascription_rethrow, CATCH_ASCRIPTION_RETHROW, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(1)))
}; FuseExpect::Jit);

// T must still contain every thrown error.
#[tokio::test]
async fn catch_ascription_too_narrow_is_an_error() -> Result<()> {
    let src = r#"{
        let a = [1, 2, 3];
        catch(e: Error<ErrChain<`ArithError(string)>>) never(e);
        a[10]?
    }"#;
    let e = graphix_package_core::testing::refusal(src, crate::TEST_REGISTER).await?;
    assert!(e.contains("does not contain Error<ErrChain<`ArrayIndexError"), "{e}");
    Ok(())
}

// A bottom-typed arm's handler-ful `?` over an input delivers each
// raise once. Every handler-ful `?` under a select arm node-walks
// (`lowering::arm_raise_blocker`), which the #[native] twin pins.
const ARM_INPUT_RAISE_DELIVERS: &str = r#"
{
  let x = array::iter([1, 2, 3, 4]);
  let n = 0;
  let v = { catch(e) n <- e ~ n + 1; select x { 2 => error(`E(x))?, _ => x } };
  select count(x) { 4 => n, _ => never() }
}
"#;

run!(arm_input_raise_delivers, ARM_INPUT_RAISE_DELIVERS, is(Value::I64(1)); FuseExpect::None);

const ARM_INPUT_RAISE_NODE_WALKS: &str = r#"
{
  let x = array::iter([1, 2, 3, 4]);
  let n = 0;
  let v: i64 = { catch(e) n <- e ~ n + 1; #[native] select x { 2 => error(`E(x))?, _ => x } };
  select count(x) { 4 => n, _ => never() }
}
"#;

run!(arm_input_raise_node_walks, ARM_INPUT_RAISE_NODE_WALKS,
    refused("fire tracker — arm entry is the node-walk's"); FuseExpect::None; jit_only);

// In a function body the select is tried twice, at the definition's open
// type and at the instance's i64: #[native] names every failure of the
// source, the arm-raise blocker among them.
const ARM_INPUT_RAISE_IN_A_BODY: &str = r#"
{
  let n = 0;
  let f = |x: i64| { catch(e) n <- e ~ n + 1; #[native] select x { 2 => error(`E(x))?, _ => x } };
  let x = array::iter([1, 2, 3, 4]);
  let v = f(x);
  select count(x) { 4 => n, _ => never() }
}
"#;

run!(arm_input_raise_in_a_body, ARM_INPUT_RAISE_IN_A_BODY,
    refused("fire tracker — arm entry is the node-walk's"); FuseExpect::None; jit_only);

// `$` over a nullable waits out the null, and says nothing about it.
const OR_NEVER_NULL: &str = r#"
{
    let a: Array<[i64, null]> = [null, 42];
    array::iter(a)$
}
"#;

run!(or_never_null, OR_NEVER_NULL, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(42)))
}; FuseExpect::Jit);

// `?` over a nullable raises `NullError` naming its operand; the
// handler's one arm select proves that is the whole error type.
const QOP_NULL_RAISES: &str = r#"
{
    let res = never();
    catch(e) select (e.0).error {
        `NullError(s) => res <- s
    };
    let a: Array<[i64, null]> = [null];
    let x = array::iter(a);
    x?;
    res
}
"#;

run!(qop_null_raises, QOP_NULL_RAISES, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "x")
});

const QOP_NULL_PASSES: &str = r#"
{
    let caught = never();
    catch(e) caught <- e ~ 1;
    let x: [i64, null] = 41;
    x? + 1
}
"#;

run!(qop_null_passes, QOP_NULL_PASSES, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(42)))
}; FuseExpect::Jit);

// Errors come off first: one `$` leaves a Result's null success in
// place, a second takes it.
const QOP_ERROR_BEFORE_NULL: &str = r#"
{
    let a: Array<[i64, null, Error<`E>]> = [error(`E), null, 7];
    array::iter(a)$$
}
"#;

run!(qop_error_before_null, QOP_ERROR_BEFORE_NULL, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(7)))
});

const QOP_NULL_SUCCESS_SURVIVES: &str = r#"
{
    let r: [null, Error<`E>] = null;
    select r$ {
        null as _ => 1
    }
}
"#;

run!(qop_null_success_survives, QOP_NULL_SUCCESS_SURVIVES, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(1)))
}; FuseExpect::None);

// `?`/`$` sit inside a postfix chain, and chain with each other.
const QOP_IN_POSTFIX_CHAIN: &str = r#"
{
    let s: [{cfg: [i64, null]}, null] = {cfg: 42};
    let t = sys::time::timer(duration:0.05s, false);
    (t ~ s)$.cfg$
}
"#;

run!(qop_in_postfix_chain, QOP_IN_POSTFIX_CHAIN, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(42)))
});

// A builtin's callback raises to the caller's handler, as a Graphix
// HOF's does.
const BUILTIN_CALLBACK_RAISES_TO_CALLER: &str = r#"
{
  let c = array::iter([1, 2, 3]);
  let r: [`Caught, null] = null;
  { catch(e) r <- e ~ `Caught; filter(c, |v| select v { 2 => error(`Boom)?, _ => true }) };
  any(r$, sys::time::timer(duration:0.2s, false) ~ `Timeout)
}
"#;

run!(builtin_callback_raises_to_caller, BUILTIN_CALLBACK_RAISES_TO_CALLER, prints("\"Caught\""); FuseExpect::None);

// Two raises to one handler in one cycle: the first is delivered that
// cycle, the second the next, from forked siblings as from serial ones.
const TWO_RAISES_ONE_HANDLER: &str = r#"
{
  let n = 0;
  let r = { catch(e) n <- e ~ n + 1; (error(`A)?, error(`B)?) };
  select n { 2 => true, _ => never() }
}
"#;

run!(two_raises_one_handler, TWO_RAISES_ONE_HANDLER, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
});

// A `?` extends a chain only where its type says it raises chains: a
// struct with a chain's field names and other types is wrapped, as is an
// array of a chain's runtime form, and a generic raise of either; a
// struct that fits `ErrChain`, and a union of chains, are extended.
run!(raise_wraps_a_chain_lookalike, r#"
{
    let f = |x: i64| x * 2 + 1;
    let r = never();
    {
        catch(e) r <- (f((e.0).error.cause) + f((e.0).error.pos), (e.0).cause);
        error({cause: 1, error: 20, ori: 3, pos: 4})?
    };
    r
}
"#, |v: Result<&Value>| {
    matches!(v, Ok(v) if v.to_string() == "[i64:12, null]")
}; FuseExpect::Jit);

run!(
    raise_extends_a_struct_that_fits_a_chain,
    r#"
{
    let o = never();
    let ori = {parent: null, source: `Vfs("x"), text: "t"};
    let pos = {line: i32:7, column: i32:9};
    {
        catch(e) o <- (
            (e.0).error,
            select (e.0).cause { null => i32:0, c => c.pos.line },
            (e.0).pos.line != i32:7
        );
        error({cause: null, error: 20, ori, pos})?
    };
    o
}
"#,
    |v: Result<&Value>| {
        matches!(v, Ok(v) if v.to_string() == "[i64:20, i32:7, true]")
    }
);

run!(
    generic_raise_wraps_a_chain_lookalike,
    r#"
{
    let h = |r: Result<i64, 'e>| r?;
    let g = never();
    let x: Result<i64, {cause: i64, error: string, ori: bool, pos: null}> =
        error({cause: 5, error: "e", ori: true, pos: null});
    {
        catch(e) g <- (e.0).error.cause + 1;
        h(x)
    };
    g
}
"#,
    is(Value::I64(6))
);

run!(
    raise_extends_a_union_of_chains,
    r#"
{
    let m = never();
    let a: [ErrChain<`A>, null] = never();
    let b: [ErrChain<`B>, null] = never();
    { catch(e) a <- e.0; error(`A)? };
    { catch(e) b <- e.0; error(`B)? };
    let n = 0;
    n <- a ~ 1;
    {
        catch(e) m <- select (e.0).error { `A => "a", `B => "b" };
        error(select n { 1 => a$, 2 => b$, _ => never() })?
    };
    m
}
"#,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "a")
);

run!(
    raise_wraps_an_array_of_a_chains_form,
    r#"
{
    let out = never();
    {
        catch(e) out <- select (e.0).error {
            [] => "empty",
            [a, ..] => "first [a]"
        };
        error([["cause", "w"], ["error", "x"], ["ori", "y"], ["pos", "z"]])?
    };
    out
}
"#,
    |v: Result<&Value>| {
        matches!(v, Ok(Value::String(s)) if &**s == r#"first ["cause", "w"]"#)
    }
);

// An `error:` literal is an error of its payload's type.
run!(error_literal_has_its_payload_type, r#"{
    let f = |e: Error<string>| e.0;
    f(error:"boom")
}"#, |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if &**s == "boom"); FuseExpect::Jit);

// A call through a `throws 'e` parameter raises 'e: the definition
// infers `throws 'e`, so the caller's catch is checked against what the
// callback throws.
const CALLBACK_THROWS_REACH_THE_CALLERS_CATCH: &str = r#"
{
    let h = |f: fn(x: i64) -> i64 throws 'e, x: i64| f(x);
    let g = |x: i64| -> i64 select x { 0 => error(`Boom)?, x => x };
    let c: i64 = never();
    catch(e) c <- e;
    h(g, 0);
    c
}
"#;

run!(
    callback_throws_reach_the_callers_catch,
    CALLBACK_THROWS_REACH_THE_CALLERS_CATCH,
    graphix_package_core::testing::refused("c is i64 and cannot hold Error<ErrChain<`Boom>>");
    FuseExpect::None
);

// A catch inside the definition takes the callback's 'e.
const CALLBACK_THROWS_REACH_AN_INNER_CATCH: &str = r#"
{
    let h = |f: fn(x: i64) -> i64 throws 'e, x: i64| {
        let caught: bool = never();
        catch(e) caught <- e;
        f(x);
        caught
    };
    h(|x| x, 0)
}
"#;

run!(
    callback_throws_reach_an_inner_catch,
    CALLBACK_THROWS_REACH_AN_INNER_CATCH,
    graphix_package_core::testing::refused("caught is bool and cannot hold 'e");
    FuseExpect::None
);

// A `?` over a generic error raises it wrapped in a chain.
const GENERIC_ERROR_RAISES: &str = r#"
{
    let must = |r: Result<i64, 'e>| -> i64 r?;
    let caught: Error<ErrChain<`Neg>> = never();
    catch(e) caught <- e;
    must(error(`Neg));
    caught
}
"#;

run!(generic_error_raises, GENERIC_ERROR_RAISES, |v: Result<&Value>| {
    format!("{}", v.unwrap()).contains(r#"["error", "Neg"]"#)
});

// A chain raised again through a generic `?` keeps its type, as the run
// time extends it rather than nesting it, and its history: the first
// raise is the new link's cause.
const GENERIC_RERAISE_KEEPS_THE_CHAIN: &str = r#"
{
    let must = |r: Result<i64, 'e>| -> i64 r?;
    let inner = |x: i64| -> i64 select x { 0 => error(`Zero)?, x => x };
    let relay = |x: i64| -> [i64, Error<ErrChain<`Zero>>] {
        let r: [i64, Error<ErrChain<`Zero>>] = never();
        catch(e) r <- e;
        r <- inner(x);
        r
    };
    let caught: Error<ErrChain<`Zero>> = never();
    catch(e) caught <- e;
    must(relay(0));
    caught
}
"#;

run!(generic_reraise_keeps_the_chain, GENERIC_RERAISE_KEEPS_THE_CHAIN, |v: Result<
    &Value,
>| {
    let s = format!("{}", v.unwrap());
    s.starts_with(r#"error:[["cause", [["cause", null], ["error", "Zero"]"#)
});

// A chain written in a block joins the chain a fresh `?` raises: two
// references to ErrChain are one definition whatever scope wrote them.
run!(
    block_local_chain_joins_a_fresh_raise,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if s == "B"),
    "/test.gx" => r#"
        let result = {
          let out = never();
          let saved: Error<ErrChain<`A>> = never();
          let r = { catch(e) out <- select (e.0).error { `A => "A", `B => "B" }; saved?; error(`B)? };
          out
        }
    "#
; FuseExpect::Jit);

run!(
    module_returned_chain_joins_a_fresh_raise,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if s == "B"),
    "/test.gx" => r#"
        mod m;
        let result = {
          let out = never();
          let saved = m::g(1);
          let r = { catch(e) out <- select (e.0).error { `A => "A", `B => "B" }; saved?; error(`B)? };
          out
        }
    "#,
    "/test/m.gx" => "let g = |x: i64| -> Error<ErrChain<`A>> { catch(e) e; error(`A)? }"
; FuseExpect::Jit);

// An open member beside a null cannot be split from the errors it may
// hold: `?` and `$` take the null, and every instance strips what its
// definition does, an error passing through.
run!(
    open_member_beside_null_strips_the_null,
    prints(r#"[[i64:3, error:"E", i64:4], ["NullError", "x"]]"#),
    "/test.gx" => r#"
        let f = |x: ['a, null]| x$;
        let g = |x: ['a, null]| x?;
        let out = never();
        let r = { catch(e) out <- e ~ ((f(3), f(error(`E)), g(4)), (e.0).error); g(null) };
        let result = out
    "#
; FuseExpect::Jit);

// A raise whose payload may be a chain or a value is not chains: it
// starts a new chain over both, and its handler reads the payload.
run!(
    mixed_chain_payload_is_one_error,
    |v: Result<&Value>| matches!(v, Ok(Value::String(s)) if s == "`A null"),
    "/test.gx" => r#"
        type MyChain = ErrChain<`A>;
        let saved: Error<MyChain> = never();
        let r = { catch(e) saved <- e; error(`A)? };
        let p: [`B, MyChain] = saved.0;
        let out = never();
        let u = { catch(e) out <- e.0; error(p)? };
        let result = select out.error {
            `B => "B",
            MyChain as c => "[c.error] [select out.cause { null => "null", _ => "chain" }]"
        }
    "#
; FuseExpect::Jit);

// A comment is not syntax: the text of a `?`'s NullError is the same with
// one above the operand.
run!(
    a_comment_does_not_reach_an_error_value,
    is(Value::Bool(true)),
    "/test.gx" => r#"
        let x: [i64, null] = null;
        let s = { a: 1, b: x };
        let out = never();
        let r = {
          catch(e) out <- (e.0).error == `NullError("(s.b)");
          let v = (
            // why
            s.b
          )?;
          v
        };
        let result = out
    "#
; FuseExpect::Jit);

// A raise is typed with core's ErrChain whatever a program names
// ErrChain: `r.ori` is core's origin struct.
run!(err_chain_is_cores_under_a_shadowing_type, r#"{
  type ErrChain<'a> = {cause: i64, error: 'a, ori: i64, pos: string};
  let r = never();
  { catch(e) r <- e.0; error(`A(1))? };
  r.ori + 1
}"#, refused("cannot compute { parent: [null, Ori], source: Source, text: string } + i64"); FuseExpect::None);

// `?` raises its operand's error part, which an Any operand does not say.
run!(
    qop_on_any_refused,
    refused("the operand of ? has type Any, so the error it raises is not known"),
    "/test.gx" => r#"
        let x: Any = 3;
        let result = { catch(e) println("[e]"); x? }
    "#
);

// A union member that names Any through a typedef is Any as written out.
run!(
    qop_on_a_typedef_of_any_refused,
    refused("so the error it raises is not known"),
    "/test.gx" => r#"
        type A = Any;
        let a: Any = error(`Other);
        let x: [A, Error<`E>] = a;
        let result = { catch(e) println("[e]"); x? }
    "#
);

async fn catch_without_binder_warns(mode: Mode) -> Result<()> {
    use arcstr::literal;
    use graphix_compiler::{CFlag, expr::Load};
    use graphix_package_core::testing::{TestCtx, config};
    use poolshark::local::LPooled;
    use std::fmt::Write;
    let (tx, _rx) = tokio::sync::mpsc::channel(100);
    let ctx = TestCtx::start(
        config(tx, &crate::TEST_REGISTER, vec![], |ctx| {
            ctx.control.set_par_mode(mode.par())
        })?
        .flags(mode.flags() | CFlag::WarningsAreErrors),
    )
    .await?;
    let mut message: LPooled<String> = LPooled::take();
    for source in [
        literal!("{ let failed = false; catch(e) failed <- true; 1 }"),
        literal!(
            "{ let failed = false; catch(e) { let f = |e| e; failed <- f(true) }; 1 }"
        ),
    ] {
        for result in [
            ctx.rt.check(Load::Text(source.clone()), None).await.map(|_| ()),
            ctx.rt.compile(source.clone()).await.map(|_| ()),
        ] {
            let error = match result {
                Ok(()) => anyhow::bail!("a handler ignoring its binder must warn"),
                Err(error) => error,
            };
            message.clear();
            write!(message, "{error:#}")?;
            assert!(message.contains("catch handler never reads its error"), "{message}");
        }
    }
    for source in [
        literal!("{ let failed = false; catch(e) failed <- e ~ true; 1 }"),
        literal!(
            "{ let failed = false; catch(e) { let f = |v| e ~ v; failed <- f(true) }; 1 }"
        ),
    ] {
        ctx.rt.check(Load::Text(source.clone()), None).await?;
        ctx.rt.compile(source).await?;
    }
    ctx.shutdown().await;
    Ok(())
}

modes!(catch_without_binder_warns);

run!(
    catch_ascription_refuses_open_raise,
    r#"{
        let run = |r: Result<i64, 'e>| {
            catch(e: Error<ErrChain<`A>>) never(e);
            r? + 1
        };
        run(error(`B))
    }"#,
    refused("does not contain")
);

run!(
    catch_ascription_refuses_open_callback,
    r#"{
        let run = |g: fn() -> i64 throws 'e| {
            catch(e: Error<ErrChain<`A>>) never(e);
            g() + 1
        };
        run(|| error(`B)?)
    }"#,
    refused("does not contain")
);

run!(
    catch_ascription_refuses_rigid_union_raise,
    r#"{
        let run = 'e: [`A, `B] |r: Result<i64, 'e>| {
            catch(e: Error<ErrChain<`A>>) never(e);
            r? + 1
        };
        run(error(`A))
    }"#,
    refused(
        "this error reaches a catch whose handler was checked to take Error<ErrChain<`A>>"
    )
);

run!(
    catch_ascription_accepts_bounded_raise,
    r#"{
        let run = 'e: `A |r: Result<i64, 'e>| {
            catch(e: Error<ErrChain<`A>>) never(e);
            r? + 1
        };
        run(2)
    }"#,
    is(Value::I64(3))
);

// A function handed to a collection runs at the slot's element type, which
// holds no error: its `?` raises nothing there.
run!(
    callback_narrower_than_its_raise,
    r#"{
        let h = |x: [i64, Error<`E>]| -> i64 x?;
        catch(e) never(e);
        array::fold(array::map([1, 2], h), 0, |acc, v| acc + v)
    }"#,
    is(Value::I64(3))
);

// A catch that sleeps with raises in flight gives them up, and at its wake
// counts as arrived those whose deliveries landed while it slept: the seq
// step that called into it passes once its later raises are handled, and
// the handler hears no given-up error: six of the eight raises before the
// step passes, the two given up at the sleeps passed over.
run!(
    a_catch_asleep_with_raises_in_flight_lets_its_step_pass,
    r#"{
        let t = 0;
        let n = 0;
        t <- select t { t if t < 14 => t + 1, _ => never() };
        let g = |k: i64| -> i64 select t {
            t if (t % 3 == 1) || (t >= 7) => {
                catch(e) n <- e ~ n + 1;
                let a = select t % 3 { 1 => error(`A(t))?, _ => 0 };
                let b = select t % 3 { 1 => error(`B(t))?, _ => 0 };
                select t { 12 => t, _ => never() }
            },
            _ => never()
        };
        let done = 0;
        let s = seq { let v = g(0); done <- v };
        select done { 12 => n, _ => never() }
    }"#,
    is(Value::I64(6))
);

// A write a declaration would admit names the type to declare as one the
// program can write: a constructed value's open parameter shows as what
// its payload put below it.
run!(
    a_write_hint_names_a_writable_type,
    r#"{
    type Box<'a> = Abstract<'a>;
    let b: Box<i64> = Box(1);
    b <- Box("s");
    0
}"#,
    refused(
        "b is Box<i64> and cannot hold Box<string>; declare b: [Box<i64>, Box<string>]"
    )
);
