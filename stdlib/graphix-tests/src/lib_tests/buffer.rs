use anyhow::Result;
use graphix_package_core::run;
use netidx::subscriber::Value;

const BYTES_ROUND_TRIP: &str = r#"
  buffer::to_string(buffer::from_string("hello"))
"#;

run!(bytes_round_trip, BYTES_ROUND_TRIP, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "hello")
});

const BYTES_TO_STRING_LOSSY: &str = r#"
  buffer::to_string_lossy(buffer::from_string("hello"))
"#;

run!(bytes_to_string_lossy, BYTES_TO_STRING_LOSSY, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "hello")
});

const BYTES_TO_STRING_INVALID: &str = r#"{
  let b = buffer::from_array([u8:0, u8:159, u8:146, u8:150]);
  is_err(buffer::to_string(b))
}"#;

run!(bytes_to_string_invalid, BYTES_TO_STRING_INVALID, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
});

const BYTES_TO_STRING_LOSSY_INVALID: &str = r#"{
  let b = buffer::from_array([u8:104, u8:101, u8:255, u8:108, u8:111]);
  str::len(buffer::to_string_lossy(b)) > 0
}"#;

run!(bytes_to_string_lossy_invalid, BYTES_TO_STRING_LOSSY_INVALID, |v: Result<
    &Value,
>| {
    matches!(v, Ok(Value::Bool(true)))
});

const BYTES_CONCAT: &str = r#"{
  let a = buffer::from_string("hello");
  let b = buffer::from_string(" world");
  buffer::to_string(buffer::concat(a, b))
}"#;

run!(bytes_concat, BYTES_CONCAT, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "hello world")
});

const BYTES_ARRAY_ROUND_TRIP: &str = r#"{
  let b = buffer::from_string("abc");
  let arr = buffer::to_array(b);
  buffer::to_string(buffer::from_array(arr))
}"#;

run!(bytes_array_round_trip, BYTES_ARRAY_ROUND_TRIP, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "abc")
});

const BYTES_LEN: &str = r#"
  buffer::len(buffer::from_string("hello"))
"#;

run!(bytes_len, BYTES_LEN, |v: Result<&Value>| { matches!(v, Ok(Value::U64(5))) });

const BYTES_INDEX: &str = r#"{
  let b = buffer::from_string("hello");
  b[0]
}"#;

run!(bytes_index, BYTES_INDEX, |v: Result<&Value>| { matches!(v, Ok(Value::U8(104))) });

const BYTES_NEG_INDEX: &str = r#"{
  let b = buffer::from_string("hello");
  b[-1]
}"#;

run!(bytes_neg_index, BYTES_NEG_INDEX, |v: Result<&Value>| {
    matches!(v, Ok(Value::U8(111)))
});

const BYTES_SLICE: &str = r#"{
  let b = buffer::from_string("hello");
  buffer::to_string(b[1..4]?)
}"#;

run!(bytes_slice, BYTES_SLICE, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "ell")
});

const BYTES_SLICE_FROM: &str = r#"{
  let b = buffer::from_string("hello");
  buffer::to_string(b[2..]?)
}"#;

run!(bytes_slice_from, BYTES_SLICE_FROM, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "llo")
});

const BYTES_SLICE_TO: &str = r#"{
  let b = buffer::from_string("hello");
  buffer::to_string(b[..3]?)
}"#;

run!(bytes_slice_to, BYTES_SLICE_TO, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "hel")
});

const ENCODE_FIXED_SIZES: &str = r#"{
  let b1 = buffer::encode([`I8(i8:1), `U8(u8:2)]);
  let b2 = buffer::encode([`I16(i16:1), `U16(u16:2)]);
  let b4 = buffer::encode([`I32(i32:1), `U32(u32:2)]);
  let b8 = buffer::encode([`I64(i64:1), `U64(u64:2)]);
  let bf = buffer::encode([`F32(f32:1.0), `F64(f64:2.0)]);
  (buffer::len(b1), buffer::len(b2), buffer::len(b4), buffer::len(b8), buffer::len(bf))
}"#;

// ASPIRE: Jit — the body does not fuse into a kernel yet.
run!(encode_fixed_sizes, ENCODE_FIXED_SIZES, |v: Result<&Value>| match v {
    Ok(Value::Array(a)) => matches!(
        &a[..],
        [Value::U64(2), Value::U64(4), Value::U64(8), Value::U64(16), Value::U64(12)]
    ),
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

const ENCODE_BYTES_PAD: &str = r#"{
  let b = buffer::encode([`Bytes(buffer::from_string("hi")), `Pad(u64:3)]);
  buffer::len(b)
}"#;

// ASPIRE: Jit — the body does not fuse into a kernel yet.
run!(encode_bytes_pad, ENCODE_BYTES_PAD, |v: Result<&Value>| {
    matches!(v, Ok(Value::U64(5)))
}; graphix_package_core::testing::FuseExpect::Jit);

const ENCODE_PAD_ZEROS: &str = r#"{
  let b = buffer::encode([`Pad(u64:4)]);
  buffer::to_array(b)
}"#;

// ASPIRE: Jit — the body does not fuse into a kernel yet.
run!(encode_pad_zeros, ENCODE_PAD_ZEROS, |v: Result<&Value>| match v {
    Ok(Value::Array(a)) =>
        matches!(&a[..], [Value::U8(0), Value::U8(0), Value::U8(0), Value::U8(0)]),
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

const ENCODE_ENDIANNESS: &str = r#"{
  let le = buffer::to_array(buffer::encode([`U32LE(u32:1)]));
  let be = buffer::to_array(buffer::encode([`U32(u32:1)]));
  (le, be)
}"#;

// ASPIRE: Jit — the body does not fuse into a kernel yet.
run!(encode_endianness, ENCODE_ENDIANNESS, |v: Result<&Value>| match v {
    Ok(Value::Array(a)) if a.len() == 2 => {
        let le = match &a[0] {
            Value::Array(a) => a,
            _ => return false,
        };
        let be = match &a[1] {
            Value::Array(a) => a,
            _ => return false,
        };
        matches!(&le[..], [Value::U8(1), Value::U8(0), Value::U8(0), Value::U8(0)])
            && matches!(&be[..], [Value::U8(0), Value::U8(0), Value::U8(0), Value::U8(1)])
    }
    _ => false,
}; graphix_package_core::testing::FuseExpect::Jit);

const DECODE_I64_ROUND_TRIP: &str = r#"{
  let x: i64 = never();
  let encoded = buffer::encode([`I64(i64:42)]);
  buffer::decode(encoded, [`I64(&x)])?;
  x
}"#;

run!(decode_i64_round_trip, DECODE_I64_ROUND_TRIP, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(42)))
}; graphix_package_core::testing::FuseExpect::Jit);

const DECODE_U32_ROUND_TRIP: &str = r#"{
  let x: u32 = never();
  let encoded = buffer::encode([`U32(u32:12345)]);
  buffer::decode(encoded, [`U32(&x)])?;
  x
}"#;

run!(decode_u32_round_trip, DECODE_U32_ROUND_TRIP, |v: Result<&Value>| {
    matches!(v, Ok(Value::U32(12345)))
}; graphix_package_core::testing::FuseExpect::Jit);

// A length-prefixed protocol: U64 length then UTF8.
const DECODE_LENGTH_PREFIXED: &str = r#"{
  let name = "hello world";
  let name_bytes = buffer::from_string(name);
  let encoded = buffer::encode([
    `U64(buffer::len(name_bytes)),
    `Bytes(name_bytes)
  ]);
  let name_len: u64 = never();
  let decoded_name: string = never();
  buffer::decode(encoded, [`U64(&name_len), `UTF8(&name_len, &decoded_name)])?;
  decoded_name
}"#;

// ASPIRE: Jit — buffer::decode with mutable ref arguments.
run!(decode_length_prefixed, DECODE_LENGTH_PREFIXED, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if s.as_str() == "hello world")
}; graphix_package_core::testing::FuseExpect::Jit);

const DECODE_BYTES_ROUND_TRIP: &str = r#"{
  let data = buffer::from_string("abc");
  let encoded = buffer::encode([
    `U64(buffer::len(data)),
    `Bytes(data)
  ]);
  let data_len: u64 = never();
  let decoded_data: bytes = never();
  buffer::decode(encoded, [`U64(&data_len), `Bytes(&data_len, &decoded_data)])?;
  buffer::to_string(decoded_data)?
}"#;

// ASPIRE: Jit — buffer::decode with mutable ref arguments.
run!(decode_bytes_round_trip, DECODE_BYTES_ROUND_TRIP, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if s.as_str() == "abc")
}; graphix_package_core::testing::FuseExpect::Jit);

const DECODE_INSUFFICIENT: &str = r#"{
  let x: i64 = never();
  let short = buffer::encode([`U8(u8:1)]);
  is_err(buffer::decode(short, [`I64(&x)]))
}"#;

run!(decode_insufficient, DECODE_INSUFFICIENT, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
}; graphix_package_core::testing::FuseExpect::Jit);

const DECODE_INVALID_UTF8: &str = r#"{
  let bad_bytes = buffer::from_array([u8:0, u8:159, u8:146, u8:150]);
  let bad = buffer::encode([`U64(buffer::len(bad_bytes)), `Bytes(bad_bytes)]);
  let slen: u64 = never();
  let s: string = never();
  is_err(buffer::decode(bad, [`U64(&slen), `UTF8(&slen, &s)]))
}"#;

// ASPIRE: Jit — the body does not fuse into a kernel yet.
run!(decode_invalid_utf8, DECODE_INVALID_UTF8, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
}; graphix_package_core::testing::FuseExpect::Jit);

const DECODE_SKIP: &str = r#"{
  let encoded = buffer::encode([`U8(u8:1), `U8(u8:2), `U8(u8:3)]);
  let skip_len = u64:1;
  let x: u8 = never();
  buffer::decode(encoded, [`Skip(&skip_len), `U8(&x)])?;
  x
}"#;

// ASPIRE: Jit — the body does not fuse into a kernel yet.
run!(decode_skip, DECODE_SKIP, |v: Result<&Value>| {
    matches!(v, Ok(Value::U8(2)))
}; graphix_package_core::testing::FuseExpect::Jit);

const DECODE_REMAINING: &str = r#"{
  let encoded = buffer::encode([`U8(u8:1), `U8(u8:2), `U8(u8:3)]);
  let x: u8 = never();
  let rest = buffer::decode(encoded, [`U8(&x)])?;
  buffer::len(rest)
}"#;

// ASPIRE: Jit — the body does not fuse into a kernel yet.
run!(decode_remaining, DECODE_REMAINING, |v: Result<&Value>| {
    matches!(v, Ok(Value::U64(2)))
}; graphix_package_core::testing::FuseExpect::Jit);

const VARINT_ROUND_TRIP: &str = r#"{
  let x: u64 = never();
  let encoded = buffer::encode([`Varint(u64:300)]);
  buffer::decode(encoded, [`Varint(&x)])?;
  x
}"#;

run!(varint_round_trip, VARINT_ROUND_TRIP, |v: Result<&Value>| {
    matches!(v, Ok(Value::U64(300)))
}; graphix_package_core::testing::FuseExpect::Jit);

const VARINT_SMALL: &str = r#"{
  let b = buffer::encode([`Varint(u64:127)]);
  buffer::len(b)
}"#;

// ASPIRE: Jit — the body does not fuse into a kernel yet.
run!(varint_small, VARINT_SMALL, |v: Result<&Value>| {
    matches!(v, Ok(Value::U64(1)))
}; graphix_package_core::testing::FuseExpect::Jit);

const VARINT_LARGE: &str = r#"{
  let b = buffer::encode([`Varint(u64:128)]);
  buffer::len(b)
}"#;

// ASPIRE: Jit — the body does not fuse into a kernel yet.
run!(varint_large, VARINT_LARGE, |v: Result<&Value>| {
    matches!(v, Ok(Value::U64(2)))
}; graphix_package_core::testing::FuseExpect::Jit);

const ZIGZAG_NEGATIVE: &str = r#"{
  let x: i64 = never();
  let encoded = buffer::encode([`Zigzag(i64:-42)]);
  buffer::decode(encoded, [`Zigzag(&x)])?;
  x
}"#;

run!(zigzag_negative, ZIGZAG_NEGATIVE, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(-42)))
}; graphix_package_core::testing::FuseExpect::Jit);

const ZIGZAG_POSITIVE: &str = r#"{
  let x: i64 = never();
  let encoded = buffer::encode([`Zigzag(i64:42)]);
  buffer::decode(encoded, [`Zigzag(&x)])?;
  x
}"#;

run!(zigzag_positive, ZIGZAG_POSITIVE, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(42)))
}; graphix_package_core::testing::FuseExpect::Jit);

// zigzag maps -1 to 1, one byte.
const ZIGZAG_SMALL: &str = r#"{
  let b = buffer::encode([`Zigzag(i64:-1)]);
  buffer::len(b)
}"#;

// ASPIRE: Jit — the body does not fuse into a kernel yet.
run!(zigzag_small, ZIGZAG_SMALL, |v: Result<&Value>| {
    matches!(v, Ok(Value::U64(1)))
}; graphix_package_core::testing::FuseExpect::Jit);

const VARINT_LENGTH_PREFIXED: &str = r#"{
  let data = buffer::from_string("hello");
  let encoded = buffer::encode([`Varint(buffer::len(data)), `Bytes(data)]);
  let data_len: u64 = never();
  let decoded: bytes = never();
  buffer::decode(encoded, [`Varint(&data_len), `Bytes(&data_len, &decoded)])?;
  buffer::to_string(decoded)?
}"#;

// ASPIRE: Jit — buffer::decode with mutable ref arguments.
run!(varint_length_prefixed, VARINT_LENGTH_PREFIXED, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if s.as_str() == "hello")
}; graphix_package_core::testing::FuseExpect::Jit);

// A ref to a literal in a decode spec is a runtime decode error.
const DECODE_REF_TO_LITERAL: &str = r#"{
  let encoded = buffer::encode([`U8(u8:1)]);
  is_err(buffer::decode(encoded, [`U8(&u8:0)]))
}"#;

run!(decode_ref_to_literal, DECODE_REF_TO_LITERAL, |v: Result<&Value>| {
    matches!(v, Ok(Value::Bool(true)))
}; graphix_package_core::testing::FuseExpect::Jit);

// A decoded field is discarded by writing to an unused let binding.
const DECODE_SKIP_UNRESOLVED: &str = r#"{
  let encoded = buffer::encode([`U8(u8:1), `U8(u8:2), `U8(u8:3)]);
  let discard: u8 = never();
  let x: u8 = never();
  buffer::decode(encoded, [`U8(&discard), `U8(&x)])?;
  x
}"#;

run!(decode_skip_unresolved, DECODE_SKIP_UNRESOLVED, |v: Result<&Value>| {
    matches!(v, Ok(Value::U8(2)))
}; graphix_package_core::testing::FuseExpect::Jit);
