use anyhow::Result;
use graphix_package_core::{run, testing::FuseExpect};
use netidx::subscriber::Value;
use std::path::Path;

fn named(path: &str, ok: impl Fn(&str) -> bool) -> bool {
    Path::new(path).file_name().and_then(|n| n.to_str()).is_some_and(ok)
}

/// A fixture's `(parent, child)`: the child is directly under the parent,
/// and its name is one `ok` accepts.
fn child_named(v: Result<&Value>, ok: impl Fn(&str) -> bool) -> bool {
    let Ok(Value::Array(a)) = v else { return false };
    match &a[..] {
        [Value::String(parent), Value::String(child)] => {
            Path::new(&**child).parent() == Some(Path::new(&**parent)) && named(child, ok)
        }
        _ => false,
    }
}

const TEMPDIR_BASIC: &str = r#"{
  use sys::fs::tempdir;
  let temp = tempdir::create(null)?;
  sys::fs::is_dir(tempdir::path(temp))
}"#;

run!(test_tempdir_basic, TEMPDIR_BASIC, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(_)))
}; FuseExpect::None);

const TEMPDIR_WITH_IN: &str = r#"{
  use sys::fs::tempdir;
  let parent = tempdir::create(null)?;
  let child = tempdir::create(#in: tempdir::path(parent), null)?;
  (tempdir::path(parent), sys::fs::is_dir(tempdir::path(child))?)
}"#;

run!(test_tempdir_with_in, TEMPDIR_WITH_IN, |v: Result<&Value>| {
    child_named(v, |_| true)
}; FuseExpect::None);

const TEMPDIR_WITH_PREFIX: &str = r#"{
  use sys::fs::{is_dir, tempdir};
  let temp = tempdir::create(#name: `Prefix("myprefix_"), null)?;
  is_dir(tempdir::path(temp))
}"#;

run!(test_tempdir_with_prefix, TEMPDIR_WITH_PREFIX, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(p)) if named(p, |n| n.starts_with("myprefix_")))
}; FuseExpect::None);

const TEMPDIR_WITH_SUFFIX: &str = r#"{
  use sys::fs::{is_dir, tempdir};
  let temp = tempdir::create(#name: `Suffix("_mysuffix"), null)?;
  is_dir(tempdir::path(temp))
}"#;

run!(test_tempdir_with_suffix, TEMPDIR_WITH_SUFFIX, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(p)) if named(p, |n| n.ends_with("_mysuffix")))
}; FuseExpect::None);

const TEMPDIR_WITH_IN_AND_PREFIX: &str = r#"{
  use sys::fs::{is_dir, tempdir};
  let parent = tempdir::create(null)?;
  let child = tempdir::create(#in: tempdir::path(parent), #name: `Prefix("test_"), null)?;
  (tempdir::path(parent), is_dir(tempdir::path(child))?)
}"#;

run!(test_tempdir_with_in_and_prefix, TEMPDIR_WITH_IN_AND_PREFIX, |v: Result<&Value>| {
    child_named(v, |n| n.starts_with("test_"))
}; FuseExpect::None);

const TEMPDIR_WITH_IN_AND_SUFFIX: &str = r#"{
  use sys::fs::{is_dir, tempdir};
  let parent = tempdir::create(null)?;
  let child = tempdir::create(#in: tempdir::path(parent), #name: `Suffix("_test"), null)?;
  (tempdir::path(parent), is_dir(tempdir::path(child))?)
}"#;

run!(test_tempdir_with_in_and_suffix, TEMPDIR_WITH_IN_AND_SUFFIX, |v: Result<&Value>| {
    child_named(v, |n| n.ends_with("_test"))
}; FuseExpect::None);

const TEMPDIR_INVALID_PARENT: &str =
    r#"sys::fs::tempdir::create(#in: "/this/path/should/not/exist/anywhere", null)"#;

run!(test_tempdir_invalid_parent, TEMPDIR_INVALID_PARENT, |v: Result<&Value>| {
    matches!(v, Ok(Value::Error(_)))
}; FuseExpect::None);

const TEMPDIR_WRITE_READ_CYCLE: &str = r#"{
  use sys::fs::{is_dir, is_file, read_all, tempdir, write_all};
  let temp = tempdir::create(null)?;
  let temp_path = tempdir::path(temp);
  let verified_temp = is_dir(temp_path)?;
  let file_path = sys::join_path(verified_temp, "cycle_test.txt");
  let write_result = write_all(#path: file_path, "Hello from tempdir!");
  let verified_file = is_file(write_result ~ file_path)?;
  read_all(verified_file)
}"#;

run!(test_tempdir_write_read_cycle, TEMPDIR_WRITE_READ_CYCLE, |v: Result<&Value>| {
    matches!(v, Ok(Value::String(s)) if &**s == "Hello from tempdir!")
}; FuseExpect::Jit);
