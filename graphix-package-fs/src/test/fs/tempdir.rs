use crate::test::{REGISTER, ROOT};
use anyhow::Result;
use graphix_package_core::run;
use netidx::subscriber::Value;
use std::path::Path;

// Test basic tempdir creation with null trigger
// Use fs::is_dir to verify the directory was actually created
const TEMPDIR_BASIC: &str = r#"{
  let temp = fs::tempdir(null)?;
  fs::is_dir(temp)
}"#;

run!(test_tempdir_basic, TEMPDIR_BASIC, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(_)))
});

// Test tempdir creation with explicit parent directory
// Verify both parent and child are directories using fs::is_dir
const TEMPDIR_WITH_IN: &str = r#"{
  let parent = fs::tempdir(null)?;
  let child = fs::tempdir(#in: parent, null)?;
  fs::is_dir(child)
}"#;

run!(test_tempdir_with_in, TEMPDIR_WITH_IN, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(_)))
});

// Test tempdir with prefix
// Verify it's a directory using fs::is_dir and check the prefix format
const TEMPDIR_WITH_PREFIX: &str = r#"{
  let temp = fs::tempdir(#name: `Prefix("myprefix_"), null)?;
  fs::is_dir(temp)
}"#;

run!(test_tempdir_with_prefix, TEMPDIR_WITH_PREFIX, |v: Result<&Value>| {
    match v {
        Ok(Value::String(path)) => {
            // Verify the directory name has the expected prefix
            let p = Path::new(&**path);
            if let Some(name) = p.file_name().and_then(|n| n.to_str()) {
                name.starts_with("myprefix_")
            } else {
                false
            }
        }
        _ => false,
    }
});

// Test tempdir with suffix
// Verify it's a directory using fs::is_dir and check the suffix format
const TEMPDIR_WITH_SUFFIX: &str = r#"{
  let temp = fs::tempdir(#name: `Suffix("_mysuffix"), null)?;
  fs::is_dir(temp)
}"#;

run!(test_tempdir_with_suffix, TEMPDIR_WITH_SUFFIX, |v: Result<&Value>| {
    match v {
        Ok(Value::String(path)) => {
            // Verify the directory name has the expected suffix
            let p = Path::new(&**path);
            if let Some(name) = p.file_name().and_then(|n| n.to_str()) {
                name.ends_with("_mysuffix")
            } else {
                false
            }
        }
        _ => false,
    }
});

// Test tempdir with both parent dir and prefix
// Verify it's a directory using fs::is_dir and check the prefix format
const TEMPDIR_WITH_IN_AND_PREFIX: &str = r#"{
  let parent = fs::tempdir(null)?;
  let child = fs::tempdir(#in: parent, #name: `Prefix("test_"), null)?;
  fs::is_dir(child)
}"#;

run!(test_tempdir_with_in_and_prefix, TEMPDIR_WITH_IN_AND_PREFIX, |v: Result<&Value>| {
    match v {
        Ok(Value::String(path)) => {
            // Verify the directory name has the expected prefix
            let p = Path::new(&**path);
            if let Some(name) = p.file_name().and_then(|n| n.to_str()) {
                name.starts_with("test_")
            } else {
                false
            }
        }
        _ => false,
    }
});

// Test tempdir with both parent dir and suffix
// Verify it's a directory using fs::is_dir and check the suffix format
const TEMPDIR_WITH_IN_AND_SUFFIX: &str = r#"{
  let parent = fs::tempdir(null)?;
  let child = fs::tempdir(#in: parent, #name: `Suffix("_test"), null)?;
  fs::is_dir(child)
}"#;

run!(test_tempdir_with_in_and_suffix, TEMPDIR_WITH_IN_AND_SUFFIX, |v: Result<&Value>| {
    match v {
        Ok(Value::String(path)) => {
            // Verify the directory name has the expected suffix
            let p = Path::new(&**path);
            if let Some(name) = p.file_name().and_then(|n| n.to_str()) {
                name.ends_with("_test")
            } else {
                false
            }
        }
        _ => false,
    }
});

// Note: Tests for tempdir trigger updates are not included here because
// they would require mutable state management which is not easily testable
// in the current test framework. The trigger functionality is tested
// indirectly through the integration tests.

// Test tempdir error handling with invalid parent directory
const TEMPDIR_INVALID_PARENT: &str =
    r#"fs::tempdir(#in: "/this/path/should/not/exist/anywhere", null)"#;

run!(test_tempdir_invalid_parent, TEMPDIR_INVALID_PARENT, |v: Result<&Value>| {
    matches!(v, Ok(Value::Error(_)))
});

// Test using tempdir for write/read cycle
// Verify directory, write, read, and file existence using fs functions
const TEMPDIR_WRITE_READ_CYCLE: &str = r#"{
  let temp = fs::tempdir(null)?;
  let verified_temp = fs::is_dir(temp)?;
  let file_path = fs::join_path(verified_temp, "cycle_test.txt");
  let write_result = fs::write_all(#path: file_path, "Hello from tempdir!");
  let verified_file = fs::is_file(write_result ~ file_path)?;
  fs::read_all(verified_file)
}"#;

run!(test_tempdir_write_read_cycle, TEMPDIR_WRITE_READ_CYCLE, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "Hello from tempdir!")
});
