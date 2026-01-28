// Tests for datetime arithmetic

use crate::run;
use anyhow::{bail, Result};
use chrono::prelude::*;
use netidx::publisher::Value;
use std::time::Duration;

const DATETIME_ARITH00: &str = r#"
{
    let x: datetime = datetime:"2024-11-05T00:00:00Z" + duration:3600.s;
    x
}
"#;

run!(datetime_arith00, DATETIME_ARITH00, |v: Result<&Value>| match v {
    Ok(Value::DateTime(dt))
        if **dt == "2024-11-05T01:00:00Z".parse::<DateTime<Utc>>().unwrap() =>
        true,
    _ => false,
});

const DATETIME_ARITH01: &str = r#"
{
    let x: datetime = datetime:"2024-11-05T00:00:00Z" - duration:3600.s;
    x
}
"#;

run!(datetime_arith01, DATETIME_ARITH01, |v: Result<&Value>| match v {
    Ok(Value::DateTime(dt))
        if **dt == "2024-11-04T23:00:00Z".parse::<DateTime<Utc>>().unwrap() =>
        true,
    _ => false,
});

const DATETIME_ARITH02: &str = r#"
{
    let x: duration = u32:2 * duration:3600.s;
    x
}
"#;

run!(datetime_arith02, DATETIME_ARITH02, |v: Result<&Value>| match v {
    Ok(Value::Duration(dt)) if **dt == Duration::from_secs(7200) => true,
    _ => false,
});

const DATETIME_ARITH03: &str = r#"
{
    let x: duration = duration:3600.s * u32:2;
    x
}
"#;

run!(datetime_arith03, DATETIME_ARITH03, |v: Result<&Value>| match v {
    Ok(Value::Duration(dt)) if **dt == Duration::from_secs(7200) => true,
    _ => false,
});

const DATETIME_ARITH04: &str = r#"
{
    let x: duration = duration:3600.s / u32:2;
    x
}
"#;

run!(datetime_arith04, DATETIME_ARITH04, |v: Result<&Value>| match v {
    Ok(Value::Duration(dt)) if **dt == Duration::from_secs(1800) => true,
    _ => false,
});

const DATETIME_ARITH05: &str = r#"
{
    let x: duration = duration:3600.s - duration:1800.s;
    x
}
"#;

run!(datetime_arith05, DATETIME_ARITH05, |v: Result<&Value>| match v {
    Ok(Value::Duration(dt)) if **dt == Duration::from_secs(1800) => true,
    _ => false,
});

const DATETIME_ARITH06: &str = r#"
{
    let x: duration = duration:0.s + duration:1800.s;
    x
}
"#;

run!(datetime_arith06, DATETIME_ARITH06, |v: Result<&Value>| match v {
    Ok(Value::Duration(dt)) if **dt == Duration::from_secs(1800) => true,
    _ => false,
});

const DATETIME_ARITH07: &str = r#"
{
    let x: duration = duration:2.s * duration:1800.s;
    x
}
"#;

run!(datetime_arith07, DATETIME_ARITH07, |v: Result<&Value>| match v {
    Err(_) => true,
    _ => false,
});

const DATETIME_ARITH08: &str = r#"
{
    let x: duration = duration:2.s / duration:1800.s;
    x
}
"#;

run!(datetime_arith08, DATETIME_ARITH08, |v: Result<&Value>| match v {
    Err(_) => true,
    _ => false,
});

const DATETIME_ARITH09: &str = r#"
{
    let x: duration = duration:2.s % duration:1800.s;
    x
}
"#;

run!(datetime_arith09, DATETIME_ARITH09, |v: Result<&Value>| match v {
    Err(_) => true,
    _ => false,
});

const DATETIME_ARITH10: &str = r#"
{
    let x: duration = duration:2.s + u32:1;
    x
}
"#;

run!(datetime_arith10, DATETIME_ARITH10, |v: Result<&Value>| match v {
    Err(_) => true,
    _ => false,
});

const DATETIME_ARITH11: &str = r#"
{
    let x: duration = duration:2.s - u32:1;
    x
}
"#;

run!(datetime_arith11, DATETIME_ARITH11, |v: Result<&Value>| match v {
    Err(_) => true,
    _ => false,
});

const DATETIME_ARITH12: &str = r#"
{
    let x: duration = datetime:"2024-11-05T00:00:00Z" - 1;
    x
}
"#;

run!(datetime_arith12, DATETIME_ARITH12, |v: Result<&Value>| match v {
    Err(_) => true,
    _ => false,
});

const DATETIME_ARITH13: &str = r#"
{
    let x: duration = datetime:"2024-11-05T00:00:00Z" + 1;
    x
}
"#;

run!(datetime_arith13, DATETIME_ARITH13, |v: Result<&Value>| match v {
    Err(_) => true,
    _ => false,
});

const DATETIME_ARITH14: &str = r#"
{
    let x: duration = datetime:"2024-11-05T00:00:00Z" * 2;
    x
}
"#;

run!(datetime_arith14, DATETIME_ARITH14, |v: Result<&Value>| match v {
    Err(_) => true,
    _ => false,
});

const DATETIME_ARITH15: &str = r#"
{
    let x: duration = datetime:"2024-11-05T00:00:00Z" / 2;
    x
}
"#;

run!(datetime_arith15, DATETIME_ARITH15, |v: Result<&Value>| match v {
    Err(_) => true,
    _ => false,
});

const DATETIME_ARITH16: &str = r#"
{
    let x: duration = datetime:"2024-11-05T00:00:00Z" % 2;
    x
}
"#;

run!(datetime_arith16, DATETIME_ARITH16, |v: Result<&Value>| match v {
    Err(_) => true,
    _ => false,
});

const DATETIME_ARITH17: &str = r#"
{
    let x: datetime = duration:1.0s - datetime:"2024-11-05T00:00:00Z";
    x
}
"#;

run!(datetime_arith17, DATETIME_ARITH17, |v: Result<&Value>| match v {
    Err(_) => true,
    _ => false,
});

const DATETIME_ARITH18: &str = r#"
{
    let errors = never();
    try
        let you_have_been_in_suspention_for = duration:9999999999999.s * 99999999999999;
        any(you_have_been_in_suspention_for, errors)
    catch(e: `ArithError(string)) => errors <- e
}
"#;

run!(datetime_arith18, DATETIME_ARITH18, |v: Result<&Value>| match v {
    Ok(Value::Error(_)) => true,
    _ => false,
});
