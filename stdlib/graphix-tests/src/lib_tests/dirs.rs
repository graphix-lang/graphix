use anyhow::Result;
use graphix_package_core::run_no_jit;
use netidx::subscriber::Value;

const HOME_DIR: &str = r#"
    sys::dirs::home_dir()
"#;

run_no_jit!(home_dir, HOME_DIR, |v: Result<&Value>| match v {
    Ok(Value::String(s)) => !s.is_empty(),
    Ok(Value::Null) => true,
    _ => false,
});

const CONFIG_DIR: &str = r#"
    sys::dirs::config_dir()
"#;

run_no_jit!(config_dir, CONFIG_DIR, |v: Result<&Value>| match v {
    Ok(Value::String(s)) => !s.is_empty(),
    Ok(Value::Null) => true,
    _ => false,
});

const DATA_DIR: &str = r#"
    sys::dirs::data_dir()
"#;

run_no_jit!(data_dir, DATA_DIR, |v: Result<&Value>| match v {
    Ok(Value::String(s)) => !s.is_empty(),
    Ok(Value::Null) => true,
    _ => false,
});
