use anyhow::Result;
use graphix_package_core::run;
use netidx::subscriber::Value;

const HOME_DIR: &str = r#"
    sys::dirs::home_dir()
"#;

// ASPIRE: Jit (currently None) — blocked on: sys::dirs::home_dir builtin not fused yet
run!(home_dir, HOME_DIR, |v: Result<&Value>| match v {
    Ok(Value::String(s)) => !s.is_empty(),
    Ok(Value::Null) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::None);

const CONFIG_DIR: &str = r#"
    sys::dirs::config_dir()
"#;

// ASPIRE: Jit (currently None) — blocked on: sys::dirs::config_dir builtin not fused yet
run!(config_dir, CONFIG_DIR, |v: Result<&Value>| match v {
    Ok(Value::String(s)) => !s.is_empty(),
    Ok(Value::Null) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::None);

const DATA_DIR: &str = r#"
    sys::dirs::data_dir()
"#;

// ASPIRE: Jit (currently None) — blocked on: sys::dirs::data_dir builtin not fused yet
run!(data_dir, DATA_DIR, |v: Result<&Value>| match v {
    Ok(Value::String(s)) => !s.is_empty(),
    Ok(Value::Null) => true,
    _ => false,
}; graphix_package_core::testing::FuseExpect::None);
