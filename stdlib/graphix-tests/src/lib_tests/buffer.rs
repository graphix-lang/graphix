use anyhow::Result;
use graphix_package_core::run;
use netidx::subscriber::Value;

// from_string + to_string round-trip
const BYTES_ROUND_TRIP: &str = r#"
  buffer::to_string(buffer::from_string("hello"))
"#;

run!(bytes_round_trip, BYTES_ROUND_TRIP, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "hello")
});

// to_string_lossy with valid UTF-8
const BYTES_TO_STRING_LOSSY: &str = r#"
  buffer::to_string_lossy(buffer::from_string("hello"))
"#;

run!(bytes_to_string_lossy, BYTES_TO_STRING_LOSSY, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "hello")
});

// to_string with invalid UTF-8 returns error
const BYTES_TO_STRING_INVALID: &str = r#"{
  let b = buffer::from_array([u8:0, u8:159, u8:146, u8:150]);
  is_err(buffer::to_string(b))
}"#;

run!(bytes_to_string_invalid, BYTES_TO_STRING_INVALID, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
});

// to_string_lossy with invalid UTF-8 replaces with replacement char
const BYTES_TO_STRING_LOSSY_INVALID: &str = r#"{
  let b = buffer::from_array([u8:104, u8:101, u8:255, u8:108, u8:111]);
  str::len(buffer::to_string_lossy(b)) > 0
}"#;

run!(bytes_to_string_lossy_invalid, BYTES_TO_STRING_LOSSY_INVALID, |v: Result<
    &Value,
>| {
    matches!(v, Ok(Value::Bool(true)))
});

// concat
const BYTES_CONCAT: &str = r#"{
  let a = buffer::from_string("hello");
  let b = buffer::from_string(" world");
  buffer::to_string(buffer::concat(a, b))
}"#;

run!(bytes_concat, BYTES_CONCAT, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "hello world")
});

// to_array + from_array round-trip
const BYTES_ARRAY_ROUND_TRIP: &str = r#"{
  let b = buffer::from_string("abc");
  let arr = buffer::to_array(b);
  buffer::to_string(buffer::from_array(arr))
}"#;

run!(bytes_array_round_trip, BYTES_ARRAY_ROUND_TRIP, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "abc")
});

// len
const BYTES_LEN: &str = r#"
  buffer::len(buffer::from_string("hello"))
"#;

run!(bytes_len, BYTES_LEN, |v: Result<&Value>| { matches!(v, Ok(Value::U64(5))) });

// bytes indexing
const BYTES_INDEX: &str = r#"{
  let b = buffer::from_string("hello");
  b[0]
}"#;

run!(bytes_index, BYTES_INDEX, |v: Result<&Value>| {
    // 'h' is ASCII 104
    matches!(v, Ok(Value::U8(104)))
});

// bytes negative indexing
const BYTES_NEG_INDEX: &str = r#"{
  let b = buffer::from_string("hello");
  b[-1]
}"#;

run!(bytes_neg_index, BYTES_NEG_INDEX, |v: Result<&Value>| {
    // 'o' is ASCII 111
    matches!(v, Ok(Value::U8(111)))
});

// bytes slicing
const BYTES_SLICE: &str = r#"{
  let b = buffer::from_string("hello");
  buffer::to_string(b[1..4]?)
}"#;

run!(bytes_slice, BYTES_SLICE, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "ell")
});

// bytes slice from start
const BYTES_SLICE_FROM: &str = r#"{
  let b = buffer::from_string("hello");
  buffer::to_string(b[2..]?)
}"#;

run!(bytes_slice_from, BYTES_SLICE_FROM, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "llo")
});

// bytes slice to end
const BYTES_SLICE_TO: &str = r#"{
  let b = buffer::from_string("hello");
  buffer::to_string(b[..3]?)
}"#;

run!(bytes_slice_to, BYTES_SLICE_TO, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "hel")
});
