// Tests for try/catch and error handling

use anyhow::Result;
use graphix_package_core::run_no_jit;
use netidx::publisher::Value;

// unchecked arithmetic: 2 + 2 works normally
const UNCHECKED0: &str = r#"
2 + 2
"#;

run_no_jit!(unchecked0, UNCHECKED0, |v: Result<&Value>| match v {
    Ok(Value::I64(4)) => true,
    _ => false,
});

// checked arithmetic: 2 +? 2 returns a union value (still i64 when no error)
const CHECKED0: &str = r#"
2 +? 2
"#;

run_no_jit!(checked0, CHECKED0, |v: Result<&Value>| match v {
    Ok(Value::I64(4)) => true,
    _ => false,
});

// checked div by zero returns an error value that can be caught
const CHECKED_DIV0: &str = r#"
{
    let res = never();
    try (0 /? 0)?
    catch(e) => select (e.0).error {
        `ArithError(s) => res <- s
    };
    res
}
"#;

run_no_jit!(checked_div0, CHECKED_DIV0, |v: Result<&Value>| match v {
    Ok(Value::String(_)) => true,
    _ => false,
});

// try/catch with array index errors still works
const CATCH1: &str = r#"
try
    let a = [1, 2, 3];
    a[0]? + a[1]?
catch(e) => select (e.0).error {
    `ArrayIndexError(s) => { println("array index error [s]"); -1 }
}
"#;

run_no_jit!(catch1, CATCH1, |v: Result<&Value>| match v {
    Ok(Value::I64(3)) => true,
    _ => false,
});

// nested try/catch with checked arith and array index errors
const CATCH4: &str = r#"
{
    let a = [0, 1, 2, 3, 4, 5];
    let err0: Error<ErrChain<[`ArithError(string), `ArrayIndexError(string)]>> = never();
    let err1: Error<ErrChain<[`ArithError(string), `ArrayIndexError(string)]>> = never();
    try
       try
           (a[5]? /? a[0]?)?;
           a[6]?
       catch(e) => select (e.0).error {
          `ArithError(_) => err1 <- e,
          _ => e?
       }
    catch(e) => err0 <- e;
    [err0, err1]
}
"#;

run_no_jit!(catch4, CATCH4, |v: Result<&Value>| match v
    .and_then(|v| v.clone().cast_to::<[Value; 2]>())
{
    Ok([Value::Error(_), Value::Error(_)]) => true,
    _ => false,
});

// checked arithmetic with $ (swallow error)
const CHECKED_DOLLAR: &str = r#"
{
    let x = (0 /? 0)$;
    any(x, 2 + 2)
}
"#;

run_no_jit!(checked_dollar, CHECKED_DOLLAR, |v: Result<&Value>| match v {
    Ok(Value::I64(4)) => true,
    _ => false,
});
