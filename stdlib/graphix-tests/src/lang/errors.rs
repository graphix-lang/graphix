// Tests for try/catch and error handling

use anyhow::Result;
use graphix_package_core::run;
use netidx::publisher::Value;

// unchecked arithmetic: 2 + 2 works normally
const UNCHECKED0: &str = r#"
2 + 2
"#;

run!(unchecked0, UNCHECKED0, |v: Result<&Value>| match v {
    Ok(Value::I64(4)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

// checked arithmetic: 2 +? 2 returns a union value (still i64 when no error)
const CHECKED0: &str = r#"
2 +? 2
"#;

// ASPIRE: Jit (currently None) — doesn't fuse its body into a
// kernel yet; the prior "fused" status was the hollow
// `result`-wrapper identity kernel (#139 identity suppression).
run!(checked0, CHECKED0, |v: Result<&Value>| match v {
    Ok(Value::I64(4)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

// checked div by zero returns an error value that can be caught
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

// Fuses again. It stopped on 2026-08-07 and the loss was misattributed
// here to the value-taint-cache storage law
// (callee-value-taint-passthrough-aug2026) — plausible, since that
// landed the same day, but wrong. The real cause was an arity defect in
// `emit_qop_deliver`: this is a handler-ful `?`, so it emits the
// error-delivery DynCall, and 9d042cbc had grown `graphix_dyncall` to
// five parameters without updating that call site. Cranelift's verifier
// rejected the function and the region node-walked. See
// findings/qop-deliver-dyncall-arity-aug2026.
run!(checked_div0, CHECKED_DIV0, |v: Result<&Value>| match v {
    Ok(Value::String(_)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

// catch with array index errors still works
const CATCH1: &str = r#"
{
    catch(e) select (e.0).error {
        `ArrayIndexError(s) => { println("array index error [s]"); -1 }
    };
    let a = [1, 2, 3];
    a[0]? + a[1]?
}
"#;

// Fuses now: the handler-ful `?` (a[0]?, a[1]?) delivers its error to
// the catch handler's variable in-kernel (variable-write-in-kernel), and
// `Catch::fuse` descends into the handler. The catch handler that
// reads the error variable is the separate kernel (next cycle).
run!(catch1, CATCH1, |v: Result<&Value>| match v {
    Ok(Value::I64(3)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

// Handler-ful `?` with a NON-SCALAR (composite) success type fuses: the
// success path unwraps the owned Array, and the error path delivers the
// error to the catch handler in-kernel (the deliver CONSUMES the error,
// replacing the drop). Success path here (`m{"a"}` exists).
// Handler-ful `?` with a NON-SCALAR (composite/string) success type
// fuses + delivers correctly — validated by graphix-fuzz (a `m{key}?`
// returning an Array success path, a missing-key error delivering to the
// catch, and a string-success `?`, all interp==jit + fused). Not pinned
// as a `run!` fixture: the test harness reads `test::result` by
// subscription and currently times out on composite-`?` / connect-result
// programs the shell and the fuzzer both run correctly — a harness
// limitation, not a fusion bug.

// nested catches (same-block shadowing) with checked arith and array
// index errors: the second catch covers the ?s below it; its handler's
// rethrow (`e?`) resolves to the FIRST catch — the predecessor — and
// must be seen the same cycle (catches update innermost-first).
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

// checked arithmetic with $ (swallow error)
const CHECKED_DOLLAR: &str = r#"
{
    let x = (0 /? 0)$;
    any(x, 2 + 2)
}
"#;

// ASPIRE: Jit (currently None) — doesn't fuse its body into a
// kernel yet; the prior "fused" status was the hollow
// `result`-wrapper identity kernel (#139 identity suppression).
run!(checked_dollar, CHECKED_DOLLAR, |v: Result<&Value>| match v {
    Ok(Value::I64(4)) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

// composite-success `$` (#199): the fused unwrap must re-box the
// Value's inline ValArray bits into the composite ABI's
// `*mut ValArray` — passing the raw payload word through SIGSEGV'd.
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

// borrowed-inner variant of the same class: the qop inner is a Local
// read of a Nullable local, exercising the clone-not-consume re-box.
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

// A `$` over a NESTED fallible union: t[0]'s inner type is
// [[string, Error<E>], Error<ArrayIndexError>], and the qop node's
// static type strips EVERY error member of the flattened union
// (string) — matching the node-walk, which drops ANY error value.
// The emit used to arm-select on the inner's ONE-LAYER option
// success ([string, Error<E>] — value-shape) and minted the
// value-shape (Null, 0) placeholder on the not-fired init cycle;
// the String-conventioned kernel return force-dropped payload 0 —
// graphix_arcstr_drop(NULL) SIGABRT (soak jul05 item 15,
// crash_000014). emit_qop_node now arm-selects on the node's own
// frozen static type. The timer delays the element one epoch so the
// init cycle exercises the placeholder path before the value lands.
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

// A Sync builtin that returns no value (`buffer::encode`'s 64MB Pad
// guard logs + bottoms) bound to an UNCONSUMED local. The node-walk
// keeps that bottom local — the block's unrelated result still emits.
// The fused kernel used to whole-kernel pending-abort, killing the
// f64:0. output (soak jul05 item 28, divergence_000025); the pend now
// converts to a #219 tainted placeholder at the DynCall site and only
// consumers bottom. (The error log line each fire is the guard's —
// expected noise.)
const DYNCALL_PENDING_UNCONSUMED: &str = r#"
{
  let b = buffer::encode([`Pad(u64:18446744073709551615)]);
  f64:0.
}
"#;

run!(dyncall_pending_unconsumed, DYNCALL_PENDING_UNCONSUMED, |v: Result<&Value>| {
    matches!(v, Ok(Value::F64(f)) if *f == 0.)
}; graphix_package_core::testing::FuseExpect::Jit);

// The same pend in STATEMENT position. A call statement used to
// de-fuse the whole block ("a no-value fire would pending-abort the
// kernel"); with the pend riding as discardable taint the block fuses
// and the statement's bottom is discarded exactly like the node-walk.
const DYNCALL_PENDING_STATEMENT: &str = r#"
{
  buffer::encode([`Pad(u64:18446744073709551615)]);
  7
}
"#;

run!(dyncall_pending_statement, DYNCALL_PENDING_STATEMENT, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(7)))
}; graphix_package_core::testing::FuseExpect::Jit);

// The pended value CONSUMED by another DynCall (`buffer::len`), whose
// tainted-arg skip path produces a tainted scalar — but the local `n`
// is itself unconsumed, so the tuple result still emits. Chains
// pend → placeholder → arg-taint skip → locality.
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

// The pend INSIDE a cross-kernel callee (scalar return): the callee's
// DynCall site converts it to taint, and the not-fresh bits ride back
// through CALLEE_RESULT_FLAGS as data — the callee must NOT abort to
// pending_exit (the caller would read the null sentinel; pre-fix the
// wrapper check silenced the 5 too).
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

// The pend INSIDE a HOF callback slot: the tainted element rides the
// loop's SlotFlags accumulator into the map result's disc — the map
// local bottoms (node-walk: the aggregate never fires) while the
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

// A handler-ful `?` — one covered by an enclosing `catch` — in a
// fusable region. The `?` succeeds here; what matters is that the
// error-delivery DynCall (`FnSource::QopDeliver`) is EMITTED, which it
// is unconditionally whenever a handler is in scope.
//
// This shape had no coverage, and it silently stopped fusing on
// 2026-08-07: 9d042cbc grew `graphix_dyncall` to five parameters
// (adding `stale_mask`) and updated `emit/call.rs` but not
// `emit_qop_deliver`, so cranelift's verifier rejected the whole
// function — "mismatched argument count: got 4, expected 5" — and the
// region node-walked. A verifier rejection is invisible without
// `--fusion-stats`, which is why a month passed; `BodyCx::call_helper`
// now debug-asserts the arity so the class panics a test instead.
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

// A lambda whose select merges a scalar arm with an error arm, called
// so the ERROR arm is taken. The kernel return must freeze as the
// union [i64, Error<f64>] (Nullable), not Scalar(I64): the def-time
// binding 'a := f64 inside the rtype's Error<'a> constraint has to
// survive call-site instantiation (reset_tvars preserves bound cells)
// or the union member terminal-settles to ⊥, [i64, ⊥] normalizes to
// i64, and the JIT marshals the error value's payload pointer as a
// scalar (soak-jul06c B5, findings/lambda-instantiation-binding-jul2026).
const ERROR_ARM_LAMBDA_RETURN: &str = r#"
{
  let f = |n: i64| select n {i64:0 => i64:0, _ => error(f64:0.)};
  f(i64:1)
}
"#;

run!(error_arm_lambda_return, ERROR_ARM_LAMBDA_RETURN, |v: Result<&Value>| {
    matches!(v, Ok(Value::Error(e)) if matches!(&**e, Value::F64(f) if *f == 0.0))
}; graphix_package_core::testing::FuseExpect::Jit);

// THE CATCH STATEMENT (2026-08-06, design/catch.md): positional
// coverage — a catch covers only the statements BELOW it in its
// block. The arith `?` above the catch has no handler (warns, error
// dropped); the index `?` below delivers. The handler's select is
// deliberately index-ONLY: the accumulated error union proves the
// coverage — if the arith `?` ever leaked in, the select would fail
// exhaustiveness (missing ArithError arm) and this test would not
// compile.
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

// A catch inside a nested do-block covers only that block (e1 in
// design/catch.md): the inner index error arrives; the outer arith
// `?` is uncovered (warns, drops). The index-only select proves it
// by exhaustiveness, as in CATCH_POSITIONAL.
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

// A catch covering a lambda BODY's interior `?` keeps those errors
// out of the lambda's inferred throws: the call site needs no outer
// handler (the def-gate faux only accumulates UNCOVERED ?s), and a
// failing call rides the handler instead.
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

// FuseExpect::None: the catch is a fusion boundary and it sits at the
// lambda-body block's root, so the body node-walks (the same loss
// profile as an in-block try/catch had); the surrounding pieces are
// too small to leave a fused kernel.
run!(catch_in_lambda_throws, CATCH_IN_LAMBDA_THROWS, |v: Result<&Value>| matches!(
    v,
    Ok(Value::I64(1))
); graphix_package_core::testing::FuseExpect::None);

/// REPL cross-input catch (the catch redesign, 2026-08-06): a catch
/// installed by ONE `GXHandle::compile` input covers LATER inputs —
/// the shell threads a session scope that the catch advances, and the
/// `?`'s delivery crosses TOPS, taking the `set_var` next-cycle path
/// (a same-cycle event-map insert only reaches nodes that update
/// later in the same cycle, and cross-top ordering is a coin flip —
/// the arg-order class).
#[tokio::test(flavor = "current_thread")]
async fn catch_repl_cross_input() -> anyhow::Result<()> {
    use arcstr::ArcStr;
    use graphix_package_core::testing::init;
    use graphix_rt::GXEvent;
    use tokio::sync::mpsc;
    let (tx, mut rx) = mpsc::channel(1000);
    let ctx = init(tx, crate::TEST_REGISTER).await?;
    // CompRes deletes its graph (and unbinds its lets) on Drop — hold
    // every input's handle for the session's lifetime, like the shell.
    let _i1 = ctx.rt.compile(ArcStr::from("let tag = \"\"")).await?;
    let _i2 = ctx.rt.compile(ArcStr::from("catch(e) tag <- \"caught\"")).await?;
    let _i3 = ctx.rt.compile(ArcStr::from("let a = [1]")).await?;
    let _i4 = ctx.rt.compile(ArcStr::from("a[9]?")).await?;
    let probe = ctx
        .rt
        .compile(ArcStr::from("select tag { \"\" => never(\"\"), t => t }"))
        .await?;
    let id = probe.exprs[0].id;
    // Watch the shell's event channel: the probe emits on its init
    // read (if the handler already ran) or on tag's flip.
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
