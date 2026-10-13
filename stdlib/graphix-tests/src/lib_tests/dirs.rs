use anyhow::Result;
use graphix_package_core::{run, testing::FuseExpect};
use netidx::subscriber::Value;

const HOME_DIR: &str = r#"
    sys::dirs::home_dir()
"#;

// CR claude for claude: [test-gap/low] home_dir, config_dir and data_dir all accept
// `Ok(Value::Null)`, so a builtin that always returned null passes on any machine.
// Compare against the `dirs` crate the builtin wraps: the expected value is
// `dirs::home_dir()` etc. (add `dirs` as a dev-dependency), and null is right only when
// that is None. The comment explaining FuseExpect::None sits below the first item; move
// it above. json_f64 in json.rs has the same weakness: a json round trip of an f64 is
// exact, so assert `*f == 3.14` instead of a 1e-10 tolerance, which would hide a writer
// that loses precision. (tests-lib-b1-07)
run!(home_dir, HOME_DIR, |v: Result<&Value>| match v {
    Ok(Value::String(s)) => !s.is_empty(),
    Ok(Value::Null) => true,
    _ => false,
}; FuseExpect::None);
// None: the dirs builtins are once-latched, so Async.

const CONFIG_DIR: &str = r#"
    sys::dirs::config_dir()
"#;

run!(config_dir, CONFIG_DIR, |v: Result<&Value>| match v {
    Ok(Value::String(s)) => !s.is_empty(),
    Ok(Value::Null) => true,
    _ => false,
}; FuseExpect::None);

const DATA_DIR: &str = r#"
    sys::dirs::data_dir()
"#;

run!(data_dir, DATA_DIR, |v: Result<&Value>| match v {
    Ok(Value::String(s)) => !s.is_empty(),
    Ok(Value::Null) => true,
    _ => false,
}; FuseExpect::None);
