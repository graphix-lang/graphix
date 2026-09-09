// Tests for try/catch and error handling

use anyhow::Result;
use graphix_package_core::run;
use netidx::publisher::Value;

const UNCHECKED0: &str = r#"
2 + 2
"#;

run!(unchecked0, UNCHECKED0, |v: Result<&Value>| match v {
    Ok(Value::I64(4)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

const CHECKED0: &str = r#"
2 +? 2
"#;

// ASPIRE: Jit — the body does not fuse into a kernel yet.
run!(checked0, CHECKED0, |v: Result<&Value>| match v {
    Ok(Value::I64(4)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

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
    Ok(Value::String(_)) => true,
    _ => false,
});

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

run!(catch1, CATCH1, |v: Result<&Value>| match v {
    Ok(Value::I64(3)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

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
    [err0, err1]
}
"#;

run!(catch4, CATCH4, |v: Result<&Value>| match v
    .and_then(|v| v.clone().cast_to::<[Value; 2]>())
{
    Ok([Value::Error(_), Value::Error(_)]) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

const CHECKED_DOLLAR: &str = r#"
{
    let x = (0 /? 0)$;
    any(x, 2 + 2)
}
"#;

// ASPIRE: Jit — the body does not fuse into a kernel yet.
run!(checked_dollar, CHECKED_DOLLAR, |v: Result<&Value>| match v {
    Ok(Value::I64(4)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

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
}; graphix_package_core::testing::FuseExpect::Jit);

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
    graphix_package_core::testing::FuseExpect::Jit
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
}; graphix_package_core::testing::FuseExpect::Jit);

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
}; graphix_package_core::testing::FuseExpect::Jit);

// The same in statement position: the statement's bottom is discarded.
const DYNCALL_PENDING_STATEMENT: &str = r#"
{
  buffer::encode([`Pad(u64:18446744073709551615)]);
  7
}
"#;

run!(dyncall_pending_statement, DYNCALL_PENDING_STATEMENT, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(7)))
}; graphix_package_core::testing::FuseExpect::Jit);

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
    |v: Result<&Value>| matches!(v, Ok(Value::I64(1)));
    graphix_package_core::testing::FuseExpect::Jit
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
}; graphix_package_core::testing::FuseExpect::Jit);

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
}; graphix_package_core::testing::FuseExpect::Jit);

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
}; graphix_package_core::testing::FuseExpect::Jit);

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
}; graphix_package_core::testing::FuseExpect::Jit);

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
); graphix_package_core::testing::FuseExpect::Jit);

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
); graphix_package_core::testing::FuseExpect::Jit);

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
    (f(0), f(9)).0
}
"#;

// None: the catch is a fusion boundary at the lambda body's root.
run!(catch_in_lambda_throws, CATCH_IN_LAMBDA_THROWS, |v: Result<&Value>| matches!(
    v,
    Ok(Value::I64(1))
); graphix_package_core::testing::FuseExpect::None);

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
    let _i2 = ctx.rt.compile(ArcStr::from("catch(e) tag <- \"caught\"")).await?;
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

run!(catch_per_activation, CATCH_PER_ACTIVATION, |v: Result<&Value>| match v {
    Ok(Value::I64(6)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::None);

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

run!(catch_in_callee_stays_in_callee, CATCH_IN_CALLEE_STAYS_IN_CALLEE, |v: Result<&Value>| match v {
    Ok(Value::I64(11)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

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

run!(catch_through_call, CATCH_THROUGH_CALL, |v: Result<&Value>| match v {
    Ok(Value::I64(6)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

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
    assert!(e.contains("does not contain"), "{e}");
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
}; graphix_package_core::testing::FuseExpect::Jit);

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
}; graphix_package_core::testing::FuseExpect::Jit);

// T must still contain every thrown error.
#[tokio::test]
async fn catch_ascription_too_narrow_is_an_error() -> Result<()> {
    let src = r#"{
        let a = [1, 2, 3];
        catch(e: Error<ErrChain<`ArithError(string)>>) never(e);
        a[10]?
    }"#;
    let r = graphix_package_core::testing::eval(src, crate::TEST_REGISTER).await;
    assert!(
        r.is_err(),
        "too-narrow catch(e: T) must be rejected, got {:?}",
        r.map(|(v, _)| v)
    );
    Ok(())
}
