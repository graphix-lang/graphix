use crate::test::{REGISTER, ROOT};
// Tests for try/catch and error handling

use crate::run;
use anyhow::Result;
use netidx::publisher::Value;

const CATCH0: &str = r#"
try 2 + 2
catch(e) => select (e.0).error {
    `ArithError(s) => println("arithmetic operation error [s]")
}
"#;

run!(catch0, CATCH0, |v: Result<&Value>| match v {
    Ok(Value::I64(4)) => true,
    _ => false,
});

const CATCH1: &str = r#"
try
    let a = [1, 2, 3];
    a[0]? + a[1]?
catch(e) => select (e.0).error {
    `ArithError(s) => println("arithmetic operation error [s]")
}
"#;

run!(catch1, CATCH1, |v: Result<&Value>| match v {
    Err(_) => true,
    _ => false,
});

const CATCH2: &str = r#"
try 2 + 2
catch(e) => select (e.0).error {
    `ArithError(s) => println("arithmetic operation error [s]"),
    `ArrayIndexError(s) => println("array index error [s]")
}
"#;

run!(catch2, CATCH2, |v: Result<&Value>| match v {
    Err(_) => true,
    _ => false,
});

const CATCH3: &str = r#"
{
    let f = |x| x / x;
    let res = never();
    try any(f(0), res)
    catch(e) => select (e.0).error {
        `ArithError(s) => res <- s
    }
}
"#;

run!(catch3, CATCH3, |v: Result<&Value>| match v {
    Ok(Value::String(_)) => true,
    _ => false,
});

const CATCH4: &str = r#"
{
    let a = [0, 1, 2, 3, 4, 5];
    let err0: Error<ErrChain<`ArrayIndexError(string)>> = never();
    let err1: Error<ErrChain<`ArithError(string)>> = never();
    try
       try
           a[5]? / a[0]?;
           a[6]?
       catch(e) => select (e.0).error {
          `ArithError(_) => err1 <- e,
          _ => e?
       }
    catch(e) => err0 <- e;
    [err0, err1]
}
"#;

run!(catch4, CATCH4, |v: Result<&Value>| match v
    .and_then(|v| v.clone().cast_to::<[Value; 2]>())
{
    Ok([Value::Error(_), Value::Error(_)]) => true,
    _ => false,
});

const CATCH5: &str = r#"
{
    let f = |x| x / x;
    let res = never();
    try any(f(0), res)
    catch(e) => select (e.0).error {
        `ArithError(s) => res <- s,
        `ArrayIndexError(s) => res <- s
    }
}
"#;

run!(catch5, CATCH5, |v: Result<&Value>| match v {
    Err(_) => true,
    _ => false,
});
