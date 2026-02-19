use anyhow::Result;
use graphix_package_core::run;
use netidx::subscriber::Value;

const RAND_FLOAT_DEFAULT: &str = r#"
  rand::rand(#clock:1)
"#;

run!(rand_float_default, RAND_FLOAT_DEFAULT, |v: Result<&Value>| {
    match v {
        Ok(Value::F64(f)) => *f >= 0.0 && *f < 1.0,
        _ => false,
    }
});

const RAND_FLOAT_RANGE: &str = r#"
  rand::rand(#start:10.0, #end:20.0, #clock:1)
"#;

run!(rand_float_range, RAND_FLOAT_RANGE, |v: Result<&Value>| {
    match v {
        Ok(Value::F64(f)) => *f >= 10.0 && *f < 20.0,
        _ => false,
    }
});

const RAND_INT_RANGE: &str = r#"
  rand::rand(#start:0, #end:100, #clock:1)
"#;

run!(rand_int_range, RAND_INT_RANGE, |v: Result<&Value>| {
    match v {
        Ok(Value::I64(i)) => *i >= 0 && *i < 100,
        _ => false,
    }
});

const PICK_FROM_ARRAY: &str = r#"
  rand::pick([10, 20, 30])
"#;

run!(pick_from_array, PICK_FROM_ARRAY, |v: Result<&Value>| {
    match v {
        Ok(Value::I64(i)) => *i == 10 || *i == 20 || *i == 30,
        _ => false,
    }
});

const PICK_SINGLE: &str = r#"
  rand::pick([42])
"#;

run!(pick_single, PICK_SINGLE, |v: Result<&Value>| {
    match v {
        Ok(Value::I64(42)) => true,
        _ => false,
    }
});

const SHUFFLE_ARRAY: &str = r#"
  rand::shuffle([1, 2, 3])
"#;

run!(shuffle_array, SHUFFLE_ARRAY, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => {
            a.len() == 3
                && a.iter().any(|v| *v == Value::I64(1))
                && a.iter().any(|v| *v == Value::I64(2))
                && a.iter().any(|v| *v == Value::I64(3))
        }
        _ => false,
    }
});

const SHUFFLE_EMPTY: &str = r#"
  rand::shuffle([])
"#;

run!(shuffle_empty, SHUFFLE_EMPTY, |v: Result<&Value>| {
    match v {
        Ok(Value::Array(a)) => a.is_empty(),
        _ => false,
    }
});
