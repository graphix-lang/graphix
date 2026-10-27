use anyhow::Result;
use graphix_package_core::{run, testing::FuseExpect};
use netidx::subscriber::Value;
use std::path::PathBuf;

/// The `dirs` crate's answer, which the builtin renders: its path, or
/// null where there is none.
fn dir_is(expected: Option<PathBuf>) -> impl Fn(Result<&Value>) -> bool {
    move |v| match (v, &expected) {
        (Ok(Value::String(s)), Some(p)) => s.as_str() == p.to_string_lossy(),
        (Ok(Value::Null), None) => true,
        _ => false,
    }
}

// FuseExpect::None: the dirs builtins are once-latched state.

const HOME_DIR: &str = r#"
    sys::dirs::home_dir()
"#;

run!(home_dir, HOME_DIR, dir_is(dirs::home_dir()); FuseExpect::None);

const CONFIG_DIR: &str = r#"
    sys::dirs::config_dir()
"#;

run!(config_dir, CONFIG_DIR, dir_is(dirs::config_dir()); FuseExpect::None);

const DATA_DIR: &str = r#"
    sys::dirs::data_dir()
"#;

run!(data_dir, DATA_DIR, dir_is(dirs::data_dir()); FuseExpect::None);
