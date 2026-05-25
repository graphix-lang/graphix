use anyhow::Result;
use graphix_package_core::run;
use netidx::subscriber::Value;

// ── bit_and ──────────────────────────────────────────────────────

const BIT_AND_BASIC: &str = r#"
  bit_and(u8:0b1100, u8:0b1010)
"#;

run!(bit_and_basic, BIT_AND_BASIC, |v: Result<&Value>| {
    matches!(v, Ok(Value::U8(0b1000)))
});

const BIT_AND_ZERO_MASK: &str = r#"
  bit_and(u8:0xFF, u8:0)
"#;

run!(bit_and_zero_mask, BIT_AND_ZERO_MASK, |v: Result<&Value>| {
    matches!(v, Ok(Value::U8(0)))
});

// ── bit_or ───────────────────────────────────────────────────────

const BIT_OR_BASIC: &str = r#"
  bit_or(u8:0b1100, u8:0b1010)
"#;

run!(bit_or_basic, BIT_OR_BASIC, |v: Result<&Value>| {
    matches!(v, Ok(Value::U8(0b1110)))
});

// ── bit_xor ──────────────────────────────────────────────────────

const BIT_XOR_BASIC: &str = r#"
  bit_xor(u8:0b1100, u8:0b1010)
"#;

run!(bit_xor_basic, BIT_XOR_BASIC, |v: Result<&Value>| {
    matches!(v, Ok(Value::U8(0b0110)))
});

const BIT_XOR_SELF: &str = r#"
  bit_xor(u8:0x2A, u8:0x2A)
"#;

run!(bit_xor_self, BIT_XOR_SELF, |v: Result<&Value>| {
    matches!(v, Ok(Value::U8(0)))
});

// ── bit_not ──────────────────────────────────────────────────────

const BIT_NOT_BASIC: &str = r#"
  bit_not(u8:0)
"#;

run!(bit_not_basic, BIT_NOT_BASIC, |v: Result<&Value>| {
    matches!(v, Ok(Value::U8(0xFF)))
});

const BIT_NOT_ROUNDTRIP: &str = r#"
  bit_not(bit_not(u8:0x2A))
"#;

run!(bit_not_roundtrip, BIT_NOT_ROUNDTRIP, |v: Result<&Value>| {
    matches!(v, Ok(Value::U8(0x2A)))
});

// ── shl ──────────────────────────────────────────────────────────

const SHL_BASIC: &str = r#"
  shl(u8:1, u8:4)
"#;

run!(shl_basic, SHL_BASIC, |v: Result<&Value>| {
    matches!(v, Ok(Value::U8(0x10)))
});

const SHL_ZERO: &str = r#"
  shl(u8:0x2A, u8:0)
"#;

run!(shl_zero, SHL_ZERO, |v: Result<&Value>| {
    matches!(v, Ok(Value::U8(0x2A)))
});

// ── shr ──────────────────────────────────────────────────────────

const SHR_BASIC: &str = r#"
  shr(u8:0x10, u8:4)
"#;

run!(shr_basic, SHR_BASIC, |v: Result<&Value>| {
    matches!(v, Ok(Value::U8(1)))
});

// ── polymorphism (non-u8 types) ──────────────────────────────────

const BIT_AND_U32: &str = r#"
  bit_and(u32:0xFF00, u32:0x0FF0)
"#;

run!(bit_and_u32, BIT_AND_U32, |v: Result<&Value>| {
    matches!(v, Ok(Value::U32(0x0F00)))
});

const SHL_I64: &str = r#"
  shl(i64:1, i64:32)
"#;

run!(shl_i64, SHL_I64, |v: Result<&Value>| {
    matches!(v, Ok(Value::I64(0x100000000)))
});
