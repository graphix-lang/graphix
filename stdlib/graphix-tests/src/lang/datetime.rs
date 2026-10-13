// Tests for datetime arithmetic

use anyhow::Result;
use chrono::prelude::*;
use graphix_package_core::{
    run,
    testing::{FuseExpect, refused},
};
use netidx::publisher::Value;
use std::time::Duration;

const DATETIME_ARITH00: &str = r#"
{
    let x: datetime = sys::time::add(datetime:"2024-11-05T00:00:00Z", duration:3600.s);
    x
}
"#;

run!(datetime_arith00, DATETIME_ARITH00, |v: Result<&Value>| match v {
    Ok(Value::DateTime(dt))
        if **dt == "2024-11-05T01:00:00Z".parse::<DateTime<Utc>>().unwrap() =>
        true,
    _ => false,
}; FuseExpect::Jit);

const DATETIME_ARITH01: &str = r#"
{
    let x: datetime = sys::time::sub(datetime:"2024-11-05T00:00:00Z", duration:3600.s);
    x
}
"#;

run!(datetime_arith01, DATETIME_ARITH01, |v: Result<&Value>| match v {
    Ok(Value::DateTime(dt))
        if **dt == "2024-11-04T23:00:00Z".parse::<DateTime<Utc>>().unwrap() =>
        true,
    _ => false,
}; FuseExpect::Jit);

const DATETIME_ARITH02: &str = r#"
{
    let x: duration = sys::time::scale(duration:3600.s, 2.0)$;
    x
}
"#;

run!(datetime_arith02, DATETIME_ARITH02, |v: Result<&Value>| match v {
    Ok(Value::Duration(dt)) if **dt == Duration::from_secs(7200) => true,
    _ => false,
}; FuseExpect::Jit);

const DATETIME_ARITH03: &str = r#"
{
    let x: duration = sys::time::scale(duration:1800.s, 4.0)$;
    x
}
"#;

run!(datetime_arith03, DATETIME_ARITH03, |v: Result<&Value>| match v {
    Ok(Value::Duration(dt)) if **dt == Duration::from_secs(7200) => true,
    _ => false,
}; FuseExpect::Jit);

const DATETIME_ARITH04: &str = r#"
{
    let x: duration = sys::time::scale(duration:3600.s, 0.5)$;
    x
}
"#;

run!(datetime_arith04, DATETIME_ARITH04, |v: Result<&Value>| match v {
    Ok(Value::Duration(dt)) if **dt == Duration::from_secs(1800) => true,
    _ => false,
}; FuseExpect::Jit);

const DATETIME_ARITH05: &str = r#"
{
    let x: duration = sys::time::sub_dur(duration:3600.s, duration:1800.s);
    x
}
"#;

run!(datetime_arith05, DATETIME_ARITH05, |v: Result<&Value>| match v {
    Ok(Value::Duration(dt)) if **dt == Duration::from_secs(1800) => true,
    _ => false,
}; FuseExpect::Jit);

const DATETIME_ARITH06: &str = r#"
{
    let x: duration = sys::time::add_dur(duration:0.s, duration:1800.s)$;
    x
}
"#;

run!(datetime_arith06, DATETIME_ARITH06, |v: Result<&Value>| match v {
    Ok(Value::Duration(dt)) if **dt == Duration::from_secs(1800) => true,
    _ => false,
}; FuseExpect::Jit);

const DATETIME_ARITH07: &str = r#"
{
    let x: duration = duration:2.s * duration:1800.s;
    x
}
"#;

run!(datetime_arith07, DATETIME_ARITH07, refused("Number does not contain duration"); FuseExpect::None);

const DATETIME_ARITH08: &str = r#"
{
    let x: duration = duration:2.s / duration:1800.s;
    x
}
"#;

run!(datetime_arith08, DATETIME_ARITH08, refused("Number does not contain duration"); FuseExpect::None);

const DATETIME_ARITH09: &str = r#"
{
    let x: duration = duration:2.s % duration:1800.s;
    x
}
"#;

run!(datetime_arith09, DATETIME_ARITH09, refused("Number does not contain duration"); FuseExpect::None);

const DATETIME_ARITH10: &str = r#"
{
    let x: duration = duration:2.s + u32:1;
    x
}
"#;

run!(datetime_arith10, DATETIME_ARITH10, refused("cannot compute duration + u32"); FuseExpect::None);

const DATETIME_ARITH11: &str = r#"
{
    let x: duration = duration:2.s - u32:1;
    x
}
"#;

run!(datetime_arith11, DATETIME_ARITH11, refused("cannot compute duration - u32"); FuseExpect::None);

const DATETIME_ARITH12: &str = r#"
{
    let x = datetime:"2024-11-05T00:00:00Z" - 1;
    x
}
"#;

run!(datetime_arith12, DATETIME_ARITH12, refused("cannot compute datetime - i64"); FuseExpect::None);

const DATETIME_ARITH13: &str = r#"
{
    let x = datetime:"2024-11-05T00:00:00Z" + 1;
    x
}
"#;

run!(datetime_arith13, DATETIME_ARITH13, refused("cannot compute datetime + i64"); FuseExpect::None);

const DATETIME_ARITH14: &str = r#"
{
    let x = datetime:"2024-11-05T00:00:00Z" * 2;
    x
}
"#;

run!(datetime_arith14, DATETIME_ARITH14, refused("cannot compute datetime * i64"); FuseExpect::None);

const DATETIME_ARITH15: &str = r#"
{
    let x = datetime:"2024-11-05T00:00:00Z" / 2;
    x
}
"#;

run!(datetime_arith15, DATETIME_ARITH15, refused("cannot compute datetime / i64"); FuseExpect::None);

const DATETIME_ARITH16: &str = r#"
{
    let x = datetime:"2024-11-05T00:00:00Z" % 2;
    x
}
"#;

run!(datetime_arith16, DATETIME_ARITH16, refused("cannot compute datetime % i64"); FuseExpect::None);

const DATETIME_ARITH17: &str = r#"
{
    let x: datetime = duration:1.0s - datetime:"2024-11-05T00:00:00Z";
    x
}
"#;

run!(datetime_arith17, DATETIME_ARITH17, refused("cannot compute duration - datetime"); FuseExpect::None);

const DATETIME_ARITH18: &str = r#"
    duration:1.s *? duration:2.s
"#;

// duration is not a Number: the operator rejects at compile time
// (scaling is sys::time::scale).
run!(datetime_arith18, DATETIME_ARITH18, refused("Number does not contain duration");
     FuseExpect::None);

const DATETIME_MINUS_DATETIME: &str = r#"
    datetime:"2024-11-05T01:00:00Z" - datetime:"2024-11-05T00:00:00Z"
"#;

// datetime is not a Number either: the difference is sys::time::diff.
run!(datetime_minus_datetime, DATETIME_MINUS_DATETIME, refused("Number does not contain datetime");
     FuseExpect::None);

const DATETIME_ARITH19: &str = r#"
{
    let x: duration = sys::time::diff(datetime:"2024-11-05T01:00:00Z", datetime:"2024-11-05T00:00:00Z");
    x
}
"#;

run!(datetime_arith19, DATETIME_ARITH19, |v: Result<&Value>| match v {
    Ok(Value::Duration(dt)) if **dt == Duration::from_secs(3600) => true,
    _ => false,
}; FuseExpect::Jit);

const DATETIME_ARITH20: &str = r#"
{
    let x: duration = sys::time::diff(datetime:"2024-11-05T00:00:00Z", datetime:"2024-11-05T01:00:00Z");
    x
}
"#;

// later before earlier saturates at zero: durations are unsigned.
run!(datetime_arith20, DATETIME_ARITH20, |v: Result<&Value>| match v {
    Ok(Value::Duration(dt)) if **dt == Duration::ZERO => true,
    _ => false,
}; FuseExpect::Jit);

// The range edges time.gxi documents, fused: add and sub saturate at
// the datetime limits, sub_dur at zero, and a negative or NaN scale or
// an add_dur overflow is a DurationError.
const DATETIME_EDGES: &str = r#"
{
    let f = |t: datetime, d: duration, d2: duration, big: duration, k: f64| (
        sys::time::add(t, big),
        sys::time::sub(t, big),
        sys::time::sub_dur(d, d2),
        sys::time::scale(d, -k),
        sys::time::scale(d, 0.0 / (k - k)),
        sys::time::add_dur(big, big)
    );
    #[native] f(
        datetime:"2024-11-05T00:00:00Z",
        duration:1.s,
        duration:2.s,
        duration:10000000000000000000.s,
        1.0
    )
}
"#;

run!(datetime_edges, DATETIME_EDGES, |v: Result<&Value>| match v {
    Ok(Value::Array(a)) => match &a[..] {
        [Value::DateTime(hi), Value::DateTime(lo), Value::Duration(z), neg, nan, over] => {
            let scale = r#"error:["DurationError", "invalid duration scale (negative, NaN, or overflow)"]"#;
            **hi == DateTime::<Utc>::MAX_UTC
                && **lo == DateTime::<Utc>::MIN_UTC
                && **z == Duration::ZERO
                && format!("{neg}") == scale
                && format!("{nan}") == scale
                && format!("{over}") == r#"error:["DurationError", "duration overflow"]"#
        }
        _ => false,
    },
    _ => false,
}; FuseExpect::Jit);
