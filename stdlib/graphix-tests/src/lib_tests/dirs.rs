use anyhow::Result;
use graphix_package_core::run;
use netidx::subscriber::Value;

const HOME_DIR: &str = r#"
    sys::dirs::home_dir()
"#;

run!(home_dir, HOME_DIR, |v: Result<&Value>| match v {
    Ok(Value::String(s)) => !s.is_empty(),
    Ok(Value::Null) => true,
    _ => false,
});

const CONFIG_DIR: &str = r#"
    sys::dirs::config_dir()
"#;

run!(config_dir, CONFIG_DIR, |v: Result<&Value>| match v {
    Ok(Value::String(s)) => !s.is_empty(),
    Ok(Value::Null) => true,
    _ => false,
});

const DATA_DIR: &str = r#"
    sys::dirs::data_dir()
"#;

run!(data_dir, DATA_DIR, |v: Result<&Value>| match v {
    Ok(Value::String(s)) => !s.is_empty(),
    Ok(Value::Null) => true,
    _ => false,
});
